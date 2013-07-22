;; Faraday FA726TE Pipeline Description
;; Copyright (C) 2010-2013 Free Software Foundation, Inc.
;; Written by I-Jui Sung, based on ARM926EJ-S Pipeline Description.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; GCC is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.  */

;; These descriptions are based on the information contained in the
;; FA726TE Core Design Note, Copyright (c) 2010 Faraday Technology Corp.

;; This automaton provides a pipeline description for the Faraday
;; FA726TE core.
;;
;; The model given here assumes that the condition for all conditional
;; instructions is "true", i.e., that all of the instructions are
;; actually executed.

(define_automaton "fa726te")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pipelines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;   The ALU pipeline has fetch, decode, execute, memory, and
;;   write stages.  We only need to model the execute, memory and write
;;   stages.

;;	E1	E2	E3	E4	E5	WB
;;______________________________________________________
;;
;;      <-------------- LD/ST ----------->
;;    shifter + LU      <-- AU -->
;;      <-- AU -->     shifter + LU    CPSR     (Pipe 0)
;;______________________________________________________
;;
;;      <---------- MUL --------->
;;    shifter + LU      <-- AU -->
;;      <-- AU -->     shifter + LU    CPSR     (Pipe 1)


(define_cpu_unit "fa726te_alu0_pipe,fa726te_alu1_pipe" "fa726te")
(define_cpu_unit "fa726te_mac_pipe" "fa726te")
(define_cpu_unit "fa726te_lsu_pipe_e,fa726te_lsu_pipe_w" "fa726te")

;; Pretend we have 2 LSUs (the second is ONLY for LDR), which can possibly
;; improve code quality.
(define_query_cpu_unit "fa726te_lsu1_pipe_e,fa726te_lsu1_pipe_w" "fa726te")
(define_cpu_unit "fa726te_is0,fa726te_is1" "fa726te")

(define_reservation "fa726te_issue" "(fa726te_is0|fa726te_is1)")
;; Reservation to restrict issue to 1.
(define_reservation "fa726te_blockage" "(fa726te_is0+fa726te_is1)")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ALU Instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ALU instructions require three cycles to execute, and use the ALU
;; pipeline in each of the three stages.  The results are available
;; after the execute stage stage has finished.
;;
;; If the destination register is the PC, the pipelines are stalled
;; for several cycles.  That case is not modeled here.

;; Move instructions.
(define_insn_reservation "726te_shift_op" 1
  (and (eq_attr "tune" "fa726te")
       (eq_attr "type" "mov_imm,mov_reg,mov_shift,mov_shift_reg,\
                        mvn_imm,mvn_reg,mvn_shift,mvn_shift_reg"))
  "fa726te_issue+(fa726te_alu0_pipe|fa726te_alu1_pipe)")

;; ALU operations with no shifted operand will finished in 1 cycle
;; Other ALU instructions 2 cycles.
(define_insn_reservation "726te_alu_op" 1
 (and (eq_attr "tune" "fa726te")
      (eq_attr "type" "arlo_imm,arlo_reg,shift,shift_reg"))
  "fa726te_issue+(fa726te_alu0_pipe|fa726te_alu1_pipe)")

;; ALU operations with a shift-by-register operand.
;; These really stall in the decoder, in order to read the shift value
;; in the first cycle.  If the instruction uses both shifter and AU,
;; it takes 3 cycles.
(define_insn_reservation "726te_alu_shift_op" 3
 (and (eq_attr "tune" "fa726te")
      (eq_attr "type" "extend,arlo_shift"))
  "fa726te_issue+(fa726te_alu0_pipe|fa726te_alu1_pipe)")

(define_insn_reservation "726te_alu_shift_reg_op" 3
 (and (eq_attr "tune" "fa726te")
      (eq_attr "type" "arlo_shift_reg"))
  "fa726te_issue+(fa726te_alu0_pipe|fa726te_alu1_pipe)")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiplication Instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Multiplication instructions loop in the execute stage until the
;; instruction has been passed through the multiplier array enough
;; times.  Multiply operations occur in both the execute and memory
;; stages of the pipeline

(define_insn_reservation "726te_mult_op" 3
 (and (eq_attr "tune" "fa726te")
      (eq_attr "type" "smlalxy,mul,mla,muls,mlas,umull,umlal,smull,smlal,\
                       umulls,umlals,smulls,smlals,smlawx,smulxy,smlaxy"))
 "fa726te_issue+fa726te_mac_pipe")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load/Store Instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The models for load/store instructions do not accurately describe
;; the difference between operations with a base register writeback
;; (such as "ldm!").  These models assume that all memory references
;; hit in dcache.

;; Loads with a shifted offset take 3 cycles, and are (a) probably the
;; most common and (b) the pessimistic assumption will lead to fewer stalls.

;; Scalar loads are pipelined in FA726TE LSU pipe.
;; Here we model the resource conflict between Load@E3-stage & Store@W-stage.
;; The 2nd LSU (lsu1) is to model the fact that if 2 loads are scheduled in the
;; same "bundle", and the 2nd load will introudce another ISSUE stall but is
;; still ok to execute (and may be benefical sometimes).

(define_insn_reservation "726te_load1_op" 3
 (and (eq_attr "tune" "fa726te")
      (eq_attr "type" "load1,load_byte"))
 "(fa726te_issue+fa726te_lsu_pipe_e+fa726te_lsu_pipe_w)\
  | (fa726te_issue+fa726te_lsu1_pipe_e+fa726te_lsu1_pipe_w,fa726te_blockage)")

(define_insn_reservation "726te_store1_op" 1
 (and (eq_attr "tune" "fa726te")
      (eq_attr "type" "store1"))
 "fa726te_blockage*2")

;; Load/Store Multiple blocks all pipelines in EX stages until WB.
;; No other instructions can be issued together.  Since they essentially
;; prevent all scheduling opportunities, we model them together here.

;; The LDM is breaking into multiple load instructions, later instruction in
;; the pipe 1 is stalled.
(define_insn_reservation "726te_ldm2_op" 4
 (and (eq_attr "tune" "fa726te")
      (eq_attr "type" "load2,load3"))
 "fa726te_blockage*4")

(define_insn_reservation "726te_ldm3_op" 5
 (and (eq_attr "tune" "fa726te")
      (eq_attr "type" "load4"))
 "fa726te_blockage*5")

(define_insn_reservation "726te_stm2_op" 2
 (and (eq_attr "tune" "fa726te")
      (eq_attr "type" "store2,store3"))
 "fa726te_blockage*3")

(define_insn_reservation "726te_stm3_op" 3
 (and (eq_attr "tune" "fa726te")
      (eq_attr "type" "store4"))
 "fa726te_blockage*4")

(define_bypass 1 "726te_load1_op,726te_ldm2_op,726te_ldm3_op" "726te_store1_op,\
                  726te_stm2_op,726te_stm3_op" "arm_no_early_store_addr_dep")
(define_bypass 0 "726te_shift_op,726te_alu_op,726te_alu_shift_op,\
                 726te_alu_shift_reg_op,726te_mult_op" "726te_store1_op"
                 "arm_no_early_store_addr_dep")
(define_bypass 0 "726te_shift_op,726te_alu_op" "726te_shift_op,726te_alu_op")
(define_bypass 1 "726te_alu_shift_op,726te_alu_shift_reg_op"
                 "726te_shift_op,726te_alu_op")
(define_bypass 1 "726te_alu_shift_op,726te_alu_shift_reg_op,726te_mult_op"
                 "726te_alu_shift_op" "arm_no_early_alu_shift_dep")
(define_bypass 1 "726te_alu_shift_op,726te_alu_shift_reg_op,726te_mult_op"
                 "726te_alu_shift_reg_op" "arm_no_early_alu_shift_value_dep")
(define_bypass 1 "726te_mult_op" "726te_shift_op,726te_alu_op")

(define_bypass 4 "726te_load1_op" "726te_mult_op")
(define_bypass 5 "726te_ldm2_op" "726te_mult_op")
(define_bypass 6 "726te_ldm3_op" "726te_mult_op")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Branch and Call Instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Branch instructions are difficult to model accurately.  The FA726TE
;; core can predict most branches.  If the branch is predicted
;; correctly, and predicted early enough, the branch can be completely
;; eliminated from the instruction stream.  Some branches can
;; therefore appear to require zero cycle to execute.  We assume that
;; all branches are predicted correctly, and that the latency is
;; therefore the minimum value.

(define_insn_reservation "726te_branch_op" 0
 (and (eq_attr "tune" "fa726te")
      (eq_attr "type" "branch"))
 "fa726te_blockage")

;; The latency for a call is actually the latency when the result is available.
;; i.e. R0 is ready for int return value.
(define_insn_reservation "726te_call_op" 1
 (and (eq_attr "tune" "fa726te")
      (eq_attr "type" "call"))
 "fa726te_blockage")

