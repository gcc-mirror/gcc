;; Faraday FA606TE Pipeline Description
;; Copyright (C) 2010-2013 Free Software Foundation, Inc.
;; Written by Mingfeng Wu, based on ARM926EJ-S Pipeline Description.
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
;; FA606TE Core Design Note, Copyright (c) 2010 Faraday Technology Corp.

;; Modeled pipeline characteristics:
;; LD -> any use: latency = 2 (1 cycle penalty).
;; ALU -> any use: latency = 1 (0 cycle penalty).

;; This automaton provides a pipeline description for the Faraday
;; FA606TE core.
;;
;; The model given here assumes that the condition for all conditional
;; instructions is "true", i.e., that all of the instructions are
;; actually executed.

(define_automaton "fa606te")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pipelines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; There is a single pipeline
;;
;;   The ALU pipeline has fetch, decode, execute, memory, and
;;   write stages.  We only need to model the execute, memory and write
;;   stages.

;;      E      M      W

(define_cpu_unit "fa606te_core" "fa606te")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ALU Instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ALU instructions require two cycles to execute, and use the ALU
;; pipeline in each of the three stages.  The results are available
;; after the execute stage stage has finished.
;;
;; If the destination register is the PC, the pipelines are stalled
;; for several cycles.  That case is not modeled here.

;; ALU operations
(define_insn_reservation "606te_alu_op" 1
 (and (eq_attr "tune" "fa606te")
      (eq_attr "type" "alu_reg,simple_alu_imm,simple_alu_shift,alu_shift,alu_shift_reg"))
 "fa606te_core")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiplication Instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn_reservation "606te_mult1" 2
 (and (eq_attr "tune" "fa606te")
      (eq_attr "type" "smlalxy"))
 "fa606te_core")

(define_insn_reservation "606te_mult2" 3
 (and (eq_attr "tune" "fa606te")
      (eq_attr "type" "smlaxy,smulxy,smulwy,smlawy"))
 "fa606te_core*2")

(define_insn_reservation "606te_mult3" 4
 (and (eq_attr "tune" "fa606te")
      (eq_attr "type" "mul,mla,muls,mlas"))
 "fa606te_core*3")

(define_insn_reservation "606te_mult4" 5
 (and (eq_attr "tune" "fa606te")
      (eq_attr "type" "umull,umlal,smull,smlal,umulls,umlals,smulls,smlals"))
 "fa606te_core*4")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load/Store Instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The models for load/store instructions do not accurately describe
;; the difference between operations with a base register writeback
;; (such as "ldm!").  These models assume that all memory references
;; hit in dcache.

(define_insn_reservation "606te_load1_op" 2
 (and (eq_attr "tune" "fa606te")
      (eq_attr "type" "load1,load_byte"))
 "fa606te_core")

(define_insn_reservation "606te_load2_op" 3
 (and (eq_attr "tune" "fa606te")
      (eq_attr "type" "load2"))
 "fa606te_core*2")

(define_insn_reservation "606te_load3_op" 4
 (and (eq_attr "tune" "fa606te")
      (eq_attr "type" "load3"))
 "fa606te_core*3")

(define_insn_reservation "606te_load4_op" 5
 (and (eq_attr "tune" "fa606te")
      (eq_attr "type" "load4"))
 "fa606te_core*4")

(define_insn_reservation "606te_store1_op" 0
 (and (eq_attr "tune" "fa606te")
      (eq_attr "type" "store1"))
 "fa606te_core")

(define_insn_reservation "606te_store2_op" 1
 (and (eq_attr "tune" "fa606te")
      (eq_attr "type" "store2"))
 "fa606te_core*2")

(define_insn_reservation "606te_store3_op" 2
 (and (eq_attr "tune" "fa606te")
      (eq_attr "type" "store3"))
 "fa606te_core*3")

(define_insn_reservation "606te_store4_op" 3
 (and (eq_attr "tune" "fa606te")
      (eq_attr "type" "store4"))
 "fa606te_core*4")


;;(define_insn_reservation "606te_ldm_op" 9
;; (and (eq_attr "tune" "fa606te")
;;      (eq_attr "type" "load2,load3,load4,store2,store3,store4"))
;; "fa606te_core*7")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Branch and Call Instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Branch instructions are difficult to model accurately.  The FA606TE
;; core can predict most branches.  If the branch is predicted
;; correctly, and predicted early enough, the branch can be completely
;; eliminated from the instruction stream.  Some branches can
;; therefore appear to require zero cycles to execute.  We assume that
;; all branches are predicted correctly, and that the latency is
;; therefore the minimum value.

(define_insn_reservation "606te_branch_op" 0
 (and (eq_attr "tune" "fa606te")
      (eq_attr "type" "branch"))
 "fa606te_core")

;; The latency for a call is actually the latency when the result is available.
;; i.e. R0 ready for int return value.  For most cases, the return value is set
;; by a mov instruction, which has 1 cycle latency.
(define_insn_reservation "606te_call_op" 1
 (and (eq_attr "tune" "fa606te")
      (eq_attr "type" "call"))
 "fa606te_core")

