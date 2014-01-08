;; Faraday FA626TE Pipeline Description
;; Copyright (C) 2010-2014 Free Software Foundation, Inc.
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
;; FA626TE Core Design Note, Copyright (c) 2010 Faraday Technology Corp.

;; Modeled pipeline characteristics:
;; ALU -> simple address LDR/STR: latency = 2 (available after 2 cycles).
;; ALU -> shifted address LDR/STR: latency = 3.
;;		( extra 1 cycle unavoidable stall).
;; ALU -> other use: latency = 2 (available after 2 cycles).
;; LD  -> simple address LDR/STR: latency = 3 (available after 3 cycles).
;; LD  -> shifted address LDR/STR: latency = 4
;;		( extra 1 cycle unavoidable stall).
;; LD  -> any other use: latency = 3 (available after 3 cycles).

;; This automaton provides a pipeline description for the Faraday
;; FA626TE core.
;;
;; The model given here assumes that the condition for all conditional
;; instructions is "true", i.e., that all of the instructions are
;; actually executed.

(define_automaton "fa626te")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pipelines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; There is a single pipeline
;;
;;   The ALU pipeline has fetch, decode, execute, memory, and
;;   write stages.  We only need to model the execute, memory and write
;;   stages.

;;      S      E      M      W

(define_cpu_unit "fa626te_core" "fa626te")

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
(define_insn_reservation "626te_alu_op" 1
 (and (eq_attr "tune" "fa626,fa626te")
      (eq_attr "type" "alu_imm,alus_imm,logic_imm,logics_imm,\
                       alu_reg,alus_reg,logic_reg,logics_reg,\
                       adc_imm,adcs_imm,adc_reg,adcs_reg,\
                       adr,bfm,rev,\
                       shift_imm,shift_reg,\
                       mov_imm,mov_reg,mvn_imm,mvn_reg,\
                       mrs,multiple,no_insn"))
 "fa626te_core")

(define_insn_reservation "626te_alu_shift_op" 2
 (and (eq_attr "tune" "fa626,fa626te")
      (eq_attr "type" "extend,\
                       alu_shift_imm,alus_shift_imm,\
                       logic_shift_imm,logics_shift_imm,\
                       alu_shift_reg,alus_shift_reg,\
                       logic_shift_reg,logics_shift_reg,\
                       mov_shift,mov_shift_reg,\
                       mvn_shift,mvn_shift_reg"))
 "fa626te_core")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiplication Instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define_insn_reservation "626te_mult1" 2
 (and (eq_attr "tune" "fa626,fa626te")
      (eq_attr "type" "smulwy,smlawy,smulxy,smlaxy"))
 "fa626te_core")

(define_insn_reservation "626te_mult2" 2
 (and (eq_attr "tune" "fa626,fa626te")
      (eq_attr "type" "mul,mla"))
 "fa626te_core")

(define_insn_reservation "626te_mult3" 3
 (and (eq_attr "tune" "fa626,fa626te")
      (eq_attr "type" "muls,mlas,smull,smlal,umull,umlal,smlalxy,smlawx"))
 "fa626te_core*2")

(define_insn_reservation "626te_mult4" 4
 (and (eq_attr "tune" "fa626,fa626te")
      (eq_attr "type" "smulls,smlals,umulls,umlals"))
 "fa626te_core*3")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load/Store Instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The models for load/store instructions do not accurately describe
;; the difference between operations with a base register writeback
;; (such as "ldm!").  These models assume that all memory references
;; hit in dcache.

(define_insn_reservation "626te_load1_op" 3
 (and (eq_attr "tune" "fa626,fa626te")
      (eq_attr "type" "load1,load_byte"))
 "fa626te_core")

(define_insn_reservation "626te_load2_op" 4
 (and (eq_attr "tune" "fa626,fa626te")
      (eq_attr "type" "load2,load3"))
 "fa626te_core*2")

(define_insn_reservation "626te_load3_op" 5
 (and (eq_attr "tune" "fa626,fa626te")
      (eq_attr "type" "load4"))
 "fa626te_core*3")

(define_insn_reservation "626te_store1_op" 0
 (and (eq_attr "tune" "fa626,fa626te")
      (eq_attr "type" "store1"))
 "fa626te_core")

(define_insn_reservation "626te_store2_op" 1
 (and (eq_attr "tune" "fa626,fa626te")
      (eq_attr "type" "store2,store3"))
 "fa626te_core*2")

(define_insn_reservation "626te_store3_op" 2
 (and (eq_attr "tune" "fa626,fa626te")
      (eq_attr "type" "store4"))
 "fa626te_core*3")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Branch and Call Instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Branch instructions are difficult to model accurately.  The FA626TE
;; core can predict most branches.  If the branch is predicted
;; correctly, and predicted early enough, the branch can be completely
;; eliminated from the instruction stream.  Some branches can
;; therefore appear to require zero cycle to execute.  We assume that
;; all branches are predicted correctly, and that the latency is
;; therefore the minimum value.

(define_insn_reservation "626te_branch_op" 0
 (and (eq_attr "tune" "fa626,fa626te")
      (eq_attr "type" "branch"))
 "fa626te_core")

;; The latency for a call is actually the latency when the result is available.
;; i.e. R0 ready for int return value. 
(define_insn_reservation "626te_call_op" 1
 (and (eq_attr "tune" "fa626,fa626te")
      (eq_attr "type" "call"))
 "fa626te_core")

