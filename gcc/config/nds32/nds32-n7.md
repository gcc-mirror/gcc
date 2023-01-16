;; Pipeline descriptions of Andes NDS32 cpu for GNU compiler
;; Copyright (C) 2012-2023 Free Software Foundation, Inc.
;; Contributed by Andes Technology Corporation.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.


;; ------------------------------------------------------------------------
;; Define N8 pipeline settings.
;; ------------------------------------------------------------------------

(define_automaton "nds32_n7_machine")

;; ------------------------------------------------------------------------
;; Pipeline Stages
;; ------------------------------------------------------------------------
;; IF - Instruction Fetch
;;   Instruction Alignment
;;   Instruction Pre-decode
;; II - Instruction Issue
;;   Instruction Decode
;;   Register File Access
;;   Instruction Execution
;;   Interrupt Handling
;; EXD - Psuedo Stage
;;   Load Data Completion

(define_cpu_unit "n7_ii" "nds32_n7_machine")

(define_insn_reservation "nds_n7_unknown" 1
  (and (eq_attr "type" "unknown")
       (eq_attr "pipeline_model" "n7"))
  "n7_ii")

(define_insn_reservation "nds_n7_misc" 1
  (and (eq_attr "type" "misc")
       (eq_attr "pipeline_model" "n7"))
  "n7_ii")

(define_insn_reservation "nds_n7_alu" 1
  (and (eq_attr "type" "alu")
       (eq_attr "pipeline_model" "n7"))
  "n7_ii")

(define_insn_reservation "nds_n7_load" 1
  (and (match_test "nds32::load_single_p (insn)")
       (eq_attr "pipeline_model" "n7"))
  "n7_ii")

(define_insn_reservation "nds_n7_store" 1
  (and (match_test "nds32::store_single_p (insn)")
       (eq_attr "pipeline_model" "n7"))
  "n7_ii")

(define_insn_reservation "nds_n7_load_multiple_1" 1
  (and (and (eq_attr "type" "load_multiple")
	    (eq_attr "combo" "1"))
       (eq_attr "pipeline_model" "n7"))
  "n7_ii")

(define_insn_reservation "nds_n7_load_multiple_2" 1
  (and (ior (and (eq_attr "type" "load_multiple")
		 (eq_attr "combo" "2"))
	    (match_test "nds32::load_double_p (insn)"))
       (eq_attr "pipeline_model" "n7"))
  "n7_ii*2")

(define_insn_reservation "nds_n7_load_multiple_3" 1
  (and (and (eq_attr "type" "load_multiple")
	    (eq_attr "combo" "3"))
       (eq_attr "pipeline_model" "n7"))
  "n7_ii*3")

(define_insn_reservation "nds_n7_load_multiple_4" 1
  (and (and (eq_attr "type" "load_multiple")
	    (eq_attr "combo" "4"))
       (eq_attr "pipeline_model" "n7"))
  "n7_ii*4")

(define_insn_reservation "nds_n7_load_multiple_5" 1
  (and (and (eq_attr "type" "load_multiple")
	    (eq_attr "combo" "5"))
       (eq_attr "pipeline_model" "n7"))
  "n7_ii*5")

(define_insn_reservation "nds_n7_load_multiple_6" 1
  (and (and (eq_attr "type" "load_multiple")
	    (eq_attr "combo" "6"))
       (eq_attr "pipeline_model" "n7"))
  "n7_ii*6")

(define_insn_reservation "nds_n7_load_multiple_7" 1
  (and (and (eq_attr "type" "load_multiple")
	    (eq_attr "combo" "7"))
       (eq_attr "pipeline_model" "n7"))
  "n7_ii*7")

(define_insn_reservation "nds_n7_load_multiple_8" 1
  (and (and (eq_attr "type" "load_multiple")
	    (eq_attr "combo" "8"))
       (eq_attr "pipeline_model" "n7"))
  "n7_ii*8")

(define_insn_reservation "nds_n7_load_multiple_12" 1
  (and (and (eq_attr "type" "load_multiple")
	    (eq_attr "combo" "12"))
       (eq_attr "pipeline_model" "n7"))
  "n7_ii*12")

(define_insn_reservation "nds_n7_store_multiple_1" 1
  (and (and (eq_attr "type" "store_multiple")
	    (eq_attr "combo" "1"))
       (eq_attr "pipeline_model" "n7"))
  "n7_ii")

(define_insn_reservation "nds_n7_store_multiple_2" 1
  (and (ior (and (eq_attr "type" "store_multiple")
		 (eq_attr "combo" "2"))
	    (match_test "nds32::store_double_p (insn)"))
       (eq_attr "pipeline_model" "n7"))
  "n7_ii*2")

(define_insn_reservation "nds_n7_store_multiple_3" 1
  (and (and (eq_attr "type" "store_multiple")
	    (eq_attr "combo" "3"))
       (eq_attr "pipeline_model" "n7"))
  "n7_ii*3")

(define_insn_reservation "nds_n7_store_multiple_4" 1
  (and (and (eq_attr "type" "store_multiple")
	    (eq_attr "combo" "4"))
       (eq_attr "pipeline_model" "n7"))
  "n7_ii*4")

(define_insn_reservation "nds_n7_store_multiple_5" 1
  (and (and (eq_attr "type" "store_multiple")
	    (eq_attr "combo" "5"))
       (eq_attr "pipeline_model" "n7"))
  "n7_ii*5")

(define_insn_reservation "nds_n7_store_multiple_6" 1
  (and (and (eq_attr "type" "store_multiple")
	    (eq_attr "combo" "6"))
       (eq_attr "pipeline_model" "n7"))
  "n7_ii*6")

(define_insn_reservation "nds_n7_store_multiple_7" 1
  (and (and (eq_attr "type" "store_multiple")
	    (eq_attr "combo" "7"))
       (eq_attr "pipeline_model" "n7"))
  "n7_ii*7")

(define_insn_reservation "nds_n7_store_multiple_8" 1
  (and (and (eq_attr "type" "store_multiple")
	    (eq_attr "combo" "8"))
       (eq_attr "pipeline_model" "n7"))
  "n7_ii*8")

(define_insn_reservation "nds_n7_store_multiple_12" 1
  (and (and (eq_attr "type" "store_multiple")
	    (eq_attr "combo" "12"))
       (eq_attr "pipeline_model" "n7"))
  "n7_ii*12")

(define_insn_reservation "nds_n7_mul_fast" 1
  (and (match_test "nds32_mul_config != MUL_TYPE_SLOW")
       (and (eq_attr "type" "mul")
	    (eq_attr "pipeline_model" "n7")))
  "n7_ii")

(define_insn_reservation "nds_n7_mul_slow" 1
  (and (match_test "nds32_mul_config == MUL_TYPE_SLOW")
       (and (eq_attr "type" "mul")
	    (eq_attr "pipeline_model" "n7")))
  "n7_ii*17")

(define_insn_reservation "nds_n7_mac_fast" 1
  (and (match_test "nds32_mul_config != MUL_TYPE_SLOW")
       (and (eq_attr "type" "mac")
	    (eq_attr "pipeline_model" "n7")))
  "n7_ii*2")

(define_insn_reservation "nds_n7_mac_slow" 1
  (and (match_test "nds32_mul_config == MUL_TYPE_SLOW")
       (and (eq_attr "type" "mac")
	    (eq_attr "pipeline_model" "n7")))
  "n7_ii*18")

(define_insn_reservation "nds_n7_div" 1
  (and (eq_attr "type" "div")
       (eq_attr "pipeline_model" "n7"))
  "n7_ii*37")

(define_insn_reservation "nds_n7_branch" 1
  (and (eq_attr "type" "branch")
       (eq_attr "pipeline_model" "n7"))
  "n7_ii")

;; ------------------------------------------------------------------------
;; Comment Notations and Bypass Rules
;; ------------------------------------------------------------------------
;; Producers (LHS)
;;   LD_!bi
;;     Load data from the memory (without updating the base register) and
;;     produce the loaded data. The result is ready at EXD.
;;   LMW(N, M)
;;     There are N micro-operations within an instruction that loads multiple
;;     words. The result produced by the M-th micro-operation is sent to
;;     consumers. The result is ready at EXD. If the base register should be
;;     updated, an extra micro-operation is inserted to the sequence, and the
;;     result is ready at II.
;;
;; Consumers (RHS)
;;   ALU, MUL, DIV
;;     Require operands at II.
;;   MOVD44_E
;;     A double-word move instruction needs two micro-operations because the
;;     reigster ports is 2R1W. The first micro-operation writes an even number
;;     register, and the second micro-operation writes an odd number register.
;;     Each input operand is required at II for each micro-operation. The letter
;;     'E' stands for even.
;;   MAC_RaRb
;;     A MAC instruction is separated into two micro-operations. The first
;;     micro-operation does the multiplication, which requires operands Ra
;;     and Rb at II. The second micro-options does the accumulation, which
;;     requires the operand Rt at II.
;;   ADDR_IN_MOP(N)
;;     Because the reigster port is 2R1W, some load/store instructions are
;;     separated into many micro-operations. N denotes the address input is
;;     required by the N-th micro-operation. Such operand is required at II.
;;   ST_bi
;;     A post-increment store instruction requires its data at II.
;;   ST_!bi_RI
;;     A store instruction with an immediate offset requires its data at II.
;;     If the offset field is a register (ST_!bi_RR), the instruction will be
;;     separated into two micro-operations, and the second one requires the
;;     input operand at II in order to store it to the memory.
;;   SMW(N, M)
;;     There are N micro-operations within an instruction that stores multiple
;;     words. Each M-th micro-operation requires its data at II. If the base
;;     register should be updated, an extra micro-operation is inserted to the
;;     sequence.
;;   BR_COND
;;     If a branch instruction is conditional, its input data is required at II.

;; LD_!bi
;;   -> ALU, MOVD44_E, MUL, MAC_RaRb, DIV, BR, ADDR_IN_MOP(1), ST_bi, ST_!bi_RI, SMW(N, 1)
(define_bypass 2
  "nds_n7_load"
  "nds_n7_alu,\
   nds_n7_mul_fast, nds_n7_mul_slow,\
   nds_n7_mac_fast, nds_n7_mac_slow,\
   nds_n7_div,\
   nds_n7_branch,\
   nds_n7_load, nds_n7_store,\
   nds_n7_load_multiple_1,nds_n7_load_multiple_2, nds_n7_load_multiple_3,\
   nds_n7_load_multiple_4,nds_n7_load_multiple_5, nds_n7_load_multiple_6,\
   nds_n7_load_multiple_7,nds_n7_load_multiple_8, nds_n7_load_multiple_12,\
   nds_n7_store_multiple_1,nds_n7_store_multiple_2, nds_n7_store_multiple_3,\
   nds_n7_store_multiple_4,nds_n7_store_multiple_5, nds_n7_store_multiple_6,\
   nds_n7_store_multiple_7,nds_n7_store_multiple_8, nds_n7_store_multiple_12"
  "nds32_n7_load_to_ii_p"
)

;; LMW(N, N)
;;   -> ALU, MOVD44_E, MUL, MAC_RaRb, DIV, BR, AADR_IN_MOP(1), ST_bi, ST_!bi_RI, SMW(N, 1)
(define_bypass 2
  "nds_n7_load_multiple_1,nds_n7_load_multiple_2, nds_n7_load_multiple_3,\
   nds_n7_load_multiple_4,nds_n7_load_multiple_5, nds_n7_load_multiple_6,\
   nds_n7_load_multiple_7,nds_n7_load_multiple_8, nds_n7_load_multiple_12"
  "nds_n7_alu,\
   nds_n7_mul_fast, nds_n7_mul_slow,\
   nds_n7_mac_fast, nds_n7_mac_slow,\
   nds_n7_div,\
   nds_n7_branch,\
   nds_n7_load, nds_n7_store,\
   nds_n7_load_multiple_1,nds_n7_load_multiple_2, nds_n7_load_multiple_3,\
   nds_n7_load_multiple_4,nds_n7_load_multiple_5, nds_n7_load_multiple_6,\
   nds_n7_load_multiple_7,nds_n7_load_multiple_8, nds_n7_load_multiple_12,\
   nds_n7_store_multiple_1,nds_n7_store_multiple_2, nds_n7_store_multiple_3,\
   nds_n7_store_multiple_4,nds_n7_store_multiple_5, nds_n7_store_multiple_6,\
   nds_n7_store_multiple_7,nds_n7_store_multiple_8, nds_n7_store_multiple_12"
  "nds32_n7_last_load_to_ii_p"
)
