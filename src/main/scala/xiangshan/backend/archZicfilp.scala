package xiangshan.backend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._

class ArchZicfilpIO(implicit p: Parameters) extends XSBundle {
  // commitValid is per-slot; isCommit excludes ROB walk and blocked cycles.
  val isCommit = Input(Bool())
  val commitValid = Input(Vec(CommitWidth, Bool()))
  val commitJalr = Input(Vec(CommitWidth, Bool()))
  val commitLPAD = Input(Vec(CommitWidth, Bool()))
  val enable = Input(Bool())
  val trap = Input(Bool())
  val xret = Flipped(ValidIO(Bool()))
  val archELP = Output(Bool())
}

class ArchZicfilp(implicit p: Parameters) extends XSModule {
  val io = IO(new ArchZicfilpIO)

  val archELP = RegInit(false.B)
  val commitEventValid = io.commitValid.map(_ && io.isCommit)
  val commitELP = commitEventValid.zip(io.commitJalr).zip(io.commitLPAD).foldLeft(archELP) {
    case (elp, ((valid, jalr), lpad)) =>
      Mux(io.enable && valid && jalr, true.B, Mux(io.enable && valid && lpad, false.B, elp))
  }

  when(io.trap) {
    archELP := false.B
  }.elsewhen(io.xret.valid) {
    archELP := io.xret.bits
  }.otherwise {
    archELP := commitELP
  }

  // Keep the unmasked architectural state visible to CSR trap entry logic.
  io.archELP := archELP
}
