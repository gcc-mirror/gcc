;; Pipeline descriptions of Andes NDS32 cpu for GNU compiler
;; Copyright (C) 2012-2019 Free Software Foundation, Inc.
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
;; Define E8 pipeline settings.
;; ------------------------------------------------------------------------

(define_automaton "nds32_e8_machine")

;; ------------------------------------------------------------------------
;; Pipeline Stages
;; ------------------------------------------------------------------------
;; IF - Instruction Fetch
;; II - Instruction Issue / Address Generation
;; EX - Instruction Execution
;; EXD - Psuedo Stage / Load Data Completion

(define_cpu_unit "e8_ii" "nds32_e8_machine")
(define_cpu_unit "e8_ex" "nds32_e8_machine")

(define_insn_reservation "nds_e8_unknown" 1
  (and (eq_attr "type" "unknown")
       (eq_attr "pipeline_model" "e8"))
  "e8_ii, e8_ex")

(define_insn_reservation "nds_e8_misc" 1
  (and (eq_attr "type" "misc")
       (eq_attr "pipeline_model" "e8"))
  "e8_ii, e8_ex")

(define_insn_reservation "nds_e8_alu" 1
  (and (eq_attr "type" "alu")
       (eq_attr "pipeline_model" "e8"))
  "e8_ii, e8_ex")

(define_insn_reservation "nds_e8_load" 1
  (and (match_test "nds32::load_single_p (insn)")
       (eq_attr "pipeline_model" "e8"))
  "e8_ii, e8_ex")

(define_insn_reservation "nds_e8_store" 1
  (and (match_test "nds32::store_single_p (insn)")
       (eq_attr "pipeline_model" "e8"))
  "e8_ii, e8_ex")

(define_insn_reservation "nds_e8_load_multiple_1" 1
  (and (and (eq_attr "type" "load_multiple")
	    (eq_attr "combo" "1"))
       (eq_attr "pipeline_model" "e8"))
  "e8_ii, e8_ex")

(define_insn_reservation "nds_e8_load_multiple_2" 1
  (and (ior (and (eq_attr "type" "load_multiple")
		 (eq_attr "combo" "2"))
	    (match_test "nds32::load_double_p (insn)"))
       (eq_attr "pipeline_model" "e8"))
  "e8_ii, e8_ii+e8_ex, e8_ex")

(define_insn_reservation "nds_e8_load_multiple_3" 1
  (and (and (eq_attr "type" "load_multiple")
	    (eq_attr "combo" "3"))
       (eq_attr "pipeline_model" "e8"))
  "e8_ii, (e8_ii+e8_ex)*2, e8_ex")

(define_insn_reservation "nds_e8_load_multiple_4" 1
  (and (and (eq_attr "type" "load_multiple")
	    (eq_attr "combo" "4"))
       (eq_attr "pipeline_model" "e8"))
  "e8_ii, (e8_ii+e8_ex)*3, e8_ex")

(define_insn_reservation "nds_e8_load_multiple_5" 1
  (and (and (eq_attr "type" "load_multiple")
	    (eq_attr "combo" "5"))
       (eq_attr "pipeline_model" "e8"))
  "e8_ii, (e8_ii+e8_ex)*4, e8_ex")

(define_insn_reservation "nds_e8_load_multiple_6" 1
  (and (and (eq_attr "type" "load_multiple")
	    (eq_attr "combo" "6"))
       (eq_attr "pipeline_model" "e8"))
  "e8_ii, (e8_ii+e8_ex)*5, e8_ex")

(define_insn_reservation "nds_e8_load_multiple_7" 1
  (and (and (eq_attr "type" "load_multiple")
	    (eq_attr "combo" "7"))
       (eq_attr "pipeline_model" "e8"))
  "e8_ii, (e8_ii+e8_ex)*6, e8_ex")

(define_insn_reservation "nds_e8_load_multiple_8" 1
  (and (and (eq_attr "type" "load_multiple")
	    (eq_attr "combo" "8"))
       (eq_attr "pipeline_model" "e8"))
  "e8_ii, (e8_ii+e8_ex)*7, e8_ex")

(define_insn_reservation "nds_e8_load_multiple_12" 1
  (and (and (eq_attr "type" "load_multiple")
	    (eq_attr "combo" "12"))
       (eq_attr "pipeline_model" "e8"))
  "e8_ii, (e8_ii+e8_ex)*11, e8_ex")

(define_insn_reservation "nds_e8_store_multiple_1" 1
  (and (and (eq_attr "type" "store_multiple")
	    (eq_attr "combo" "1"))
       (eq_attr "pipeline_model" "e8"))
  "e8_ii, e8_ex")

(define_insn_reservation "nds_e8_store_multiple_2" 1
  (and (ior (and (eq_attr "type" "store_multiple")
		 (eq_attr "combo" "2"))
	    (match_test "nds32::store_double_p (insn)"))
       (eq_attr "pipeline_model" "e8"))
  "e8_ii, e8_ii+e8_ex, e8_ex")

(define_insn_reservation "nds_e8_store_multiple_3" 1
  (and (and (eq_attr "type" "store_multiple")
	    (eq_attr "combo" "3"))
       (eq_attr "pipeline_model" "e8"))
  "e8_ii, (e8_ii+e8_ex)*2, e8_ex")

(define_insn_reservation "nds_e8_store_multiple_4" 1
  (and (and (eq_attr "type" "store_multiple")
	    (eq_attr "combo" "4"))
       (eq_attr "pipeline_model" "e8"))
  "e8_ii, (e8_ii+e8_ex)*3, e8_ex")

(define_insn_reservation "nds_e8_store_multiple_5" 1
  (and (and (eq_attr "type" "store_multiple")
	    (eq_attr "combo" "5"))
       (eq_attr "pipeline_model" "e8"))
  "e8_ii, (e8_ii+e8_ex)*4, e8_ex")

(define_insn_reservation "nds_e8_store_multiple_6" 1
  (and (and (eq_attr "type" "store_multiple")
	    (eq_attr "combo" "6"))
       (eq_attr "pipeline_model" "e8"))
  "e8_ii, (e8_ii+e8_ex)*5, e8_ex")

(define_insn_reservation "nds_e8_store_multiple_7" 1
  (and (and (eq_attr "type" "store_multiple")
	    (eq_attr "combo" "7"))
       (eq_attr "pipeline_model" "e8"))
  "e8_ii, (e8_ii+e8_ex)*6, e8_ex")

(define_insn_reservation "nds_e8_store_multiple_8" 1
  (and (and (eq_attr "type" "store_multiple")
	    (eq_attr "combo" "8"))
       (eq_attr "pipeline_model" "e8"))
  "e8_ii, (e8_ii+e8_ex)*7, e8_ex")

(define_insn_reservation "nds_e8_store_multiple_12" 1
  (and (and (eq_attr "type" "store_multiple")
	    (eq_attr "combo" "12"))
       (eq_attr "pipeline_model" "e8"))
  "e8_ii, (e8_ii+e8_ex)*11, e8_ex")

(define_insn_reservation "nds_e8_mul_fast" 1
  (and (match_test "nds32_mul_config != MUL_TYPE_SLOW")
       (and (eq_attr "type" "mul")
	    (eq_attr "pipeline_model" "e8")))
  "e8_ii, e8_ex")

(define_insn_reservation "nds_e8_mul_slow" 1
  (and (match_test "nds32_mul_config == MUL_TYPE_SLOW")
       (and (eq_attr "type" "mul")
	    (eq_attr "pipeline_model" "e8")))
  "e8_ii, e8_ex*16")

(define_insn_reservation "nds_e8_mac_fast" 1
  (and (match_test "nds32_mul_config != MUL_TYPE_SLOW")
       (and (eq_attr "type" "mac")
	    (eq_attr "pipeline_model" "e8")))
  "e8_ii, e8_ii+e8_ex, e8_ex")

(define_insn_reservation "nds_e8_mac_slow" 1
  (and (match_test "nds32_mul_config == MUL_TYPE_SLOW")
       (and (eq_attr "type" "mac")
	    (eq_attr "pipeline_model" "e8")))
  "e8_ii, (e8_ii+e8_ex)*16, e8_ex")

(define_insn_reservation "nds_e8_div" 1
  (and (eq_attr "type" "div")
       (eq_attr "pipeline_model" "e8"))
  "e8_ii, (e8_ii+e8_ex)*36, e8_ex")

(define_insn_reservation "nds_e8_branch" 1
  (and (eq_attr "type" "branch")
       (eq_attr "pipeline_model" "e8"))
  "e8_ii, e8_ex")

;; ------------------------------------------------------------------------
;; Comment Notations and Bypass Rules
;; ------------------------------------------------------------------------
;; Producers (LHS)
;;   LD
;;     Load data from the memory and produce the loaded data. The result is
;;     ready at EXD.
;;   LMW(N, M)
;;     There are N micro-operations within an instruction that loads multiple
;;     words. The result produced by the M-th micro-operation is sent to
;;     consumers. The result is ready at EXD.
;;   ADDR_OUT
;;     Most load/store instructions can produce an address output if updating
;;     the base register is required. The result is ready at EX, which is
;;     produced by ALU.
;;   ALU, MOVD44, MUL, MAC
;;     The result is ready at EX.
;;   DIV_Rs
;;     A division instruction saves the quotient result to Rt and saves the
;;     remainder result to Rs. The instruction is separated into two micro-
;;     operations. The first micro-operation writes to Rt, and the seconde
;;     one writes to Rs. Each of the results is ready at EX.
;;
;; Consumers (RHS)
;;   ALU, MUL, DIV
;;     Require operands at EX.
;;   ADDR_IN_MOP(N)
;;      N denotes the address input is required by the N-th micro-operation.
;;      Such operand is required at II.
;;   ST
;;     A store instruction requires its data at EX.
;;   SMW(N, M)
;;     There are N micro-operations within an instruction that stores multiple
;;     words. Each M-th micro-operation requires its data at EX.
;;   BR_COND
;;     If a branch instruction is conditional, its input data is required at EX.

;; LD -> ADDR_IN_MOP(1)
(define_bypass 2
  "nds_e8_load"
  "nds_e8_branch,\
   nds_e8_load, nds_e8_store,\
   nds_e8_load_multiple_1,nds_e8_load_multiple_2, nds_e8_load_multiple_3,\
   nds_e8_load_multiple_4,nds_e8_load_multiple_5, nds_e8_load_multiple_6,\
   nds_e8_load_multiple_7,nds_e8_load_multiple_8, nds_e8_load_multiple_12,\
   nds_e8_store_multiple_1,nds_e8_store_multiple_2, nds_e8_store_multiple_3,\
   nds_e8_store_multiple_4,nds_e8_store_multiple_5, nds_e8_store_multiple_6,\
   nds_e8_store_multiple_7,nds_e8_store_multiple_8, nds_e8_store_multiple_12"
  "nds32_e8_load_to_ii_p"
)

;; LD -> ALU, MUL, MAC, DIV, BR_COND, ST, SMW(N, 1)
(define_bypass 2
  "nds_e8_load"
  "nds_e8_alu,
   nds_e8_mul_fast, nds_e8_mul_slow,\
   nds_e8_mac_fast, nds_e8_mac_slow,\
   nds_e8_div,\
   nds_e8_branch,\
   nds_e8_store,\
   nds_e8_store_multiple_1,nds_e8_store_multiple_2, nds_e8_store_multiple_3,\
   nds_e8_store_multiple_4,nds_e8_store_multiple_5, nds_e8_store_multiple_6,\
   nds_e8_store_multiple_7,nds_e8_store_multiple_8, nds_e8_store_multiple_12"
  "nds32_e8_load_to_ex_p"
)

;; ALU, MOVD44, MUL, MAC, DIV_Rs, LD_bi, ADDR_OUT -> ADDR_IN_MOP(1)
(define_bypass 2
  "nds_e8_alu,
   nds_e8_mul_fast, nds_e8_mul_slow,\
   nds_e8_mac_fast, nds_e8_mac_slow,\
   nds_e8_div,\
   nds_e8_load, nds_e8_store,\
   nds_e8_load_multiple_1,nds_e8_load_multiple_2, nds_e8_load_multiple_3,\
   nds_e8_load_multiple_4,nds_e8_load_multiple_5, nds_e8_load_multiple_6,\
   nds_e8_load_multiple_7,nds_e8_load_multiple_8, nds_e8_load_multiple_12,\
   nds_e8_store_multiple_1,nds_e8_store_multiple_2, nds_e8_store_multiple_3,\
   nds_e8_store_multiple_4,nds_e8_store_multiple_5, nds_e8_store_multiple_6,\
   nds_e8_store_multiple_7,nds_e8_store_multiple_8, nds_e8_store_multiple_12"
  "nds_e8_branch,\
   nds_e8_load, nds_e8_store,\
   nds_e8_load_multiple_1,nds_e8_load_multiple_2, nds_e8_load_multiple_3,\
   nds_e8_load_multiple_4,nds_e8_load_multiple_5, nds_e8_load_multiple_6,\
   nds_e8_load_multiple_7,nds_e8_load_multiple_8, nds_e8_load_multiple_12,\
   nds_e8_store_multiple_1,nds_e8_store_multiple_2, nds_e8_store_multiple_3,\
   nds_e8_store_multiple_4,nds_e8_store_multiple_5, nds_e8_store_multiple_6,\
   nds_e8_store_multiple_7,nds_e8_store_multiple_8, nds_e8_store_multiple_12"
  "nds32_e8_ex_to_ii_p"
)

;; LMW(N, N) -> ADDR_IN_MOP(1)
(define_bypass 2
  "nds_e8_load_multiple_1,nds_e8_load_multiple_2, nds_e8_load_multiple_3,\
   nds_e8_load_multiple_4,nds_e8_load_multiple_5, nds_e8_load_multiple_6,\
   nds_e8_load_multiple_7,nds_e8_load_multiple_8, nds_e8_load_multiple_12"
  "nds_e8_branch,\
   nds_e8_load, nds_e8_store,\
   nds_e8_load_multiple_1,nds_e8_load_multiple_2, nds_e8_load_multiple_3,\
   nds_e8_load_multiple_4,nds_e8_load_multiple_5, nds_e8_load_multiple_6,\
   nds_e8_load_multiple_7,nds_e8_load_multiple_8, nds_e8_load_multiple_12,\
   nds_e8_store_multiple_1,nds_e8_store_multiple_2, nds_e8_store_multiple_3,\
   nds_e8_store_multiple_4,nds_e8_store_multiple_5, nds_e8_store_multiple_6,\
   nds_e8_store_multiple_7,nds_e8_store_multiple_8, nds_e8_store_multiple_12"
  "nds32_e8_last_load_to_ii_p"
)

;; LMW(N, N) -> ALU, MUL, MAC, DIV, BR_COND, ST, SMW(N, 1)
(define_bypass 2
  "nds_e8_load_multiple_1,nds_e8_load_multiple_2, nds_e8_load_multiple_3,\
   nds_e8_load_multiple_4,nds_e8_load_multiple_5, nds_e8_load_multiple_6,\
   nds_e8_load_multiple_7,nds_e8_load_multiple_8, nds_e8_load_multiple_12"
  "nds_e8_alu,
   nds_e8_mul_fast, nds_e8_mul_slow,\
   nds_e8_mac_fast, nds_e8_mac_slow,\
   nds_e8_div,\
   nds_e8_branch,\
   nds_e8_store,\
   nds_e8_store_multiple_1,nds_e8_store_multiple_2, nds_e8_store_multiple_3,\
   nds_e8_store_multiple_4,nds_e8_store_multiple_5, nds_e8_store_multiple_6,\
   nds_e8_store_multiple_7,nds_e8_store_multiple_8, nds_e8_store_multiple_12"
  "nds32_e8_last_load_to_ex_p"
)
