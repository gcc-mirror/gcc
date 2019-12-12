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
;; Define N8 pipeline settings.
;; ------------------------------------------------------------------------

(define_automaton "nds32_n8_machine")

;; ------------------------------------------------------------------------
;; Pipeline Stages
;; ------------------------------------------------------------------------
;; IF - Instruction Fetch
;; II - Instruction Issue / Address Generation
;; EX - Instruction Execution
;; EXD - Psuedo Stage / Load Data Completion

(define_cpu_unit "n8_ii" "nds32_n8_machine")
(define_cpu_unit "n8_ex" "nds32_n8_machine")

(define_insn_reservation "nds_n8_unknown" 1
  (and (eq_attr "type" "unknown")
       (eq_attr "pipeline_model" "n8"))
  "n8_ii, n8_ex")

(define_insn_reservation "nds_n8_misc" 1
  (and (eq_attr "type" "misc")
       (eq_attr "pipeline_model" "n8"))
  "n8_ii, n8_ex")

(define_insn_reservation "nds_n8_alu" 1
  (and (eq_attr "type" "alu")
       (eq_attr "pipeline_model" "n8"))
  "n8_ii, n8_ex")

(define_insn_reservation "nds_n8_load" 1
  (and (match_test "nds32::load_single_p (insn)")
       (eq_attr "pipeline_model" "n8"))
  "n8_ii, n8_ex")

(define_insn_reservation "nds_n8_store" 1
  (and (match_test "nds32::store_single_p (insn)")
       (eq_attr "pipeline_model" "n8"))
  "n8_ii, n8_ex")

(define_insn_reservation "nds_n8_load_multiple_1" 1
  (and (and (eq_attr "type" "load_multiple")
	    (eq_attr "combo" "1"))
       (eq_attr "pipeline_model" "n8"))
  "n8_ii, n8_ex")

(define_insn_reservation "nds_n8_load_multiple_2" 1
  (and (ior (and (eq_attr "type" "load_multiple")
		 (eq_attr "combo" "2"))
	    (match_test "nds32::load_double_p (insn)"))
       (eq_attr "pipeline_model" "n8"))
  "n8_ii, n8_ii+n8_ex, n8_ex")

(define_insn_reservation "nds_n8_load_multiple_3" 1
  (and (and (eq_attr "type" "load_multiple")
	    (eq_attr "combo" "3"))
       (eq_attr "pipeline_model" "n8"))
  "n8_ii, (n8_ii+n8_ex)*2, n8_ex")

(define_insn_reservation "nds_n8_load_multiple_4" 1
  (and (and (eq_attr "type" "load_multiple")
	    (eq_attr "combo" "4"))
       (eq_attr "pipeline_model" "n8"))
  "n8_ii, (n8_ii+n8_ex)*3, n8_ex")

(define_insn_reservation "nds_n8_load_multiple_5" 1
  (and (and (eq_attr "type" "load_multiple")
	    (eq_attr "combo" "5"))
       (eq_attr "pipeline_model" "n8"))
  "n8_ii, (n8_ii+n8_ex)*4, n8_ex")

(define_insn_reservation "nds_n8_load_multiple_6" 1
  (and (and (eq_attr "type" "load_multiple")
	    (eq_attr "combo" "6"))
       (eq_attr "pipeline_model" "n8"))
  "n8_ii, (n8_ii+n8_ex)*5, n8_ex")

(define_insn_reservation "nds_n8_load_multiple_7" 1
  (and (and (eq_attr "type" "load_multiple")
	    (eq_attr "combo" "7"))
       (eq_attr "pipeline_model" "n8"))
  "n8_ii, (n8_ii+n8_ex)*6, n8_ex")

(define_insn_reservation "nds_n8_load_multiple_8" 1
  (and (and (eq_attr "type" "load_multiple")
	    (eq_attr "combo" "8"))
       (eq_attr "pipeline_model" "n8"))
  "n8_ii, (n8_ii+n8_ex)*7, n8_ex")

(define_insn_reservation "nds_n8_load_multiple_12" 1
  (and (and (eq_attr "type" "load_multiple")
	    (eq_attr "combo" "12"))
       (eq_attr "pipeline_model" "n8"))
  "n8_ii, (n8_ii+n8_ex)*11, n8_ex")

(define_insn_reservation "nds_n8_store_multiple_1" 1
  (and (and (eq_attr "type" "store_multiple")
	    (eq_attr "combo" "1"))
       (eq_attr "pipeline_model" "n8"))
  "n8_ii, n8_ex")

(define_insn_reservation "nds_n8_store_multiple_2" 1
  (and (ior (and (eq_attr "type" "store_multiple")
		 (eq_attr "combo" "2"))
	    (match_test "nds32::store_double_p (insn)"))
       (eq_attr "pipeline_model" "n8"))
  "n8_ii, n8_ii+n8_ex, n8_ex")

(define_insn_reservation "nds_n8_store_multiple_3" 1
  (and (and (eq_attr "type" "store_multiple")
	    (eq_attr "combo" "3"))
       (eq_attr "pipeline_model" "n8"))
  "n8_ii, (n8_ii+n8_ex)*2, n8_ex")

(define_insn_reservation "nds_n8_store_multiple_4" 1
  (and (and (eq_attr "type" "store_multiple")
	    (eq_attr "combo" "4"))
       (eq_attr "pipeline_model" "n8"))
  "n8_ii, (n8_ii+n8_ex)*3, n8_ex")

(define_insn_reservation "nds_n8_store_multiple_5" 1
  (and (and (eq_attr "type" "store_multiple")
	    (eq_attr "combo" "5"))
       (eq_attr "pipeline_model" "n8"))
  "n8_ii, (n8_ii+n8_ex)*4, n8_ex")

(define_insn_reservation "nds_n8_store_multiple_6" 1
  (and (and (eq_attr "type" "store_multiple")
	    (eq_attr "combo" "6"))
       (eq_attr "pipeline_model" "n8"))
  "n8_ii, (n8_ii+n8_ex)*5, n8_ex")

(define_insn_reservation "nds_n8_store_multiple_7" 1
  (and (and (eq_attr "type" "store_multiple")
	    (eq_attr "combo" "7"))
       (eq_attr "pipeline_model" "n8"))
  "n8_ii, (n8_ii+n8_ex)*6, n8_ex")

(define_insn_reservation "nds_n8_store_multiple_8" 1
  (and (and (eq_attr "type" "store_multiple")
	    (eq_attr "combo" "8"))
       (eq_attr "pipeline_model" "n8"))
  "n8_ii, (n8_ii+n8_ex)*7, n8_ex")

(define_insn_reservation "nds_n8_store_multiple_12" 1
  (and (and (eq_attr "type" "store_multiple")
	    (eq_attr "combo" "12"))
       (eq_attr "pipeline_model" "n8"))
  "n8_ii, (n8_ii+n8_ex)*11, n8_ex")

(define_insn_reservation "nds_n8_mul_fast" 1
  (and (match_test "nds32_mul_config != MUL_TYPE_SLOW")
       (and (eq_attr "type" "mul")
	    (eq_attr "pipeline_model" "n8")))
  "n8_ii, n8_ex")

(define_insn_reservation "nds_n8_mul_slow" 1
  (and (match_test "nds32_mul_config == MUL_TYPE_SLOW")
       (and (eq_attr "type" "mul")
	    (eq_attr "pipeline_model" "n8")))
  "n8_ii, n8_ex*16")

(define_insn_reservation "nds_n8_mac_fast" 1
  (and (match_test "nds32_mul_config != MUL_TYPE_SLOW")
       (and (eq_attr "type" "mac")
	    (eq_attr "pipeline_model" "n8")))
  "n8_ii, n8_ii+n8_ex, n8_ex")

(define_insn_reservation "nds_n8_mac_slow" 1
  (and (match_test "nds32_mul_config == MUL_TYPE_SLOW")
       (and (eq_attr "type" "mac")
	    (eq_attr "pipeline_model" "n8")))
  "n8_ii, (n8_ii+n8_ex)*16, n8_ex")

(define_insn_reservation "nds_n8_div" 1
  (and (eq_attr "type" "div")
       (eq_attr "pipeline_model" "n8"))
  "n8_ii, (n8_ii+n8_ex)*36, n8_ex")

(define_insn_reservation "nds_n8_branch" 1
  (and (eq_attr "type" "branch")
       (eq_attr "pipeline_model" "n8"))
  "n8_ii, n8_ex")

;; ------------------------------------------------------------------------
;; Comment Notations and Bypass Rules
;; ------------------------------------------------------------------------
;; Producers (LHS)
;;   LD_!bi
;;     Load data from the memory (without updating the base register) and
;;     produce the loaded data. The result is ready at EXD.
;;   LD_bi
;;     Load data from the memory (with updating the base register) and
;;     produce the loaded data. The result is ready at EXD. Because the
;;     register port is 2R1W, two micro-operations are required in order
;;     to write two registers. The base register is updated by the second
;;     micro-operation and the result is ready at EX.
;;   LMW(N, M)
;;     There are N micro-operations within an instruction that loads multiple
;;     words. The result produced by the M-th micro-operation is sent to
;;     consumers. The result is ready at EXD. If the base register should be
;;     updated, an extra micro-operation is inserted to the sequence, and the
;;     result is ready at EX.
;;   ADDR_OUT
;;     Most load/store instructions can produce an address output if updating
;;     the base register is required. The result is ready at EX, which is
;;     produced by ALU.
;;   ALU, MUL, MAC
;;     The result is ready at EX.
;;   MOVD44_O
;;     A double-word move instruction needs to write registers twice. Because
;;     the register port is 2R1W, two micro-operations are required. The even
;;     number reigster is updated by the first one, and the odd number register
;;     is updated by the second one. Each of the results is ready at EX.
;;     The letter 'O' stands for odd.
;;   DIV_Rs
;;     A division instruction saves the quotient result to Rt and saves the
;;     remainder result to Rs. It requires two micro-operations because the
;;     register port is 2R1W. The first micro-operation writes to Rt, and
;;     the seconde one writes to Rs. Each of the results is ready at EX.
;;
;; Consumers (RHS)
;;   ALU, MUL, DIV
;;     Require operands at EX.
;;   MOVD44_E
;;     The letter 'E' stands for even, which is accessed by the first micro-
;;     operation and a movd44 instruction. The operand is required at EX.
;;   MAC_RaRb
;;     A MAC instruction is separated into two micro-operations. The first
;;     micro-operation does the multiplication, which requires operands Ra
;;     and Rb at EX. The second micro-options does the accumulation, which
;;     requires the operand Rt at EX.
;;   ADDR_IN_MOP(N)
;;     Because the reigster port is 2R1W, some load/store instructions are
;;     separated into many micro-operations. N denotes the address input is
;;     required by the N-th micro-operation. Such operand is required at II.
;;   ST_bi
;;     A post-increment store instruction requires its data at EX.
;;   ST_!bi_RI
;;     A store instruction with an immediate offset requires its data at EX.
;;     If the offset field is a register (ST_!bi_RR), the instruction will be
;;     separated into two micro-operations, and the second one requires the
;;     input operand at EX in order to store it to the memory.
;;   SMW(N, M)
;;     There are N micro-operations within an instruction that stores multiple
;;     words. Each M-th micro-operation requires its data at EX. If the base
;;     register should be updated, an extra micro-operation is inserted to the
;;     sequence.
;;   BR_COND
;;     If a branch instruction is conditional, its input data is required at EX.

;; LD_!bi -> ADDR_IN_MOP(1)
(define_bypass 3
  "nds_n8_load"
  "nds_n8_branch,\
   nds_n8_load, nds_n8_store,\
   nds_n8_load_multiple_1,nds_n8_load_multiple_2, nds_n8_load_multiple_3,\
   nds_n8_load_multiple_4,nds_n8_load_multiple_5, nds_n8_load_multiple_6,\
   nds_n8_load_multiple_7,nds_n8_load_multiple_8, nds_n8_load_multiple_12,\
   nds_n8_store_multiple_1,nds_n8_store_multiple_2, nds_n8_store_multiple_3,\
   nds_n8_store_multiple_4,nds_n8_store_multiple_5, nds_n8_store_multiple_6,\
   nds_n8_store_multiple_7,nds_n8_store_multiple_8, nds_n8_store_multiple_12"
  "nds32_n8_load_to_ii_p"
)

;; LMW(N, N) -> ADDR_IN_MOP(1)
(define_bypass 3
  "nds_n8_load_multiple_1,nds_n8_load_multiple_2, nds_n8_load_multiple_3,\
   nds_n8_load_multiple_4,nds_n8_load_multiple_5, nds_n8_load_multiple_6,\
   nds_n8_load_multiple_7,nds_n8_load_multiple_8, nds_n8_load_multiple_12"
  "nds_n8_branch,\
   nds_n8_load, nds_n8_store,\
   nds_n8_load_multiple_1,nds_n8_load_multiple_2, nds_n8_load_multiple_3,\
   nds_n8_load_multiple_4,nds_n8_load_multiple_5, nds_n8_load_multiple_6,\
   nds_n8_load_multiple_7,nds_n8_load_multiple_8, nds_n8_load_multiple_12,\
   nds_n8_store_multiple_1,nds_n8_store_multiple_2, nds_n8_store_multiple_3,\
   nds_n8_store_multiple_4,nds_n8_store_multiple_5, nds_n8_store_multiple_6,\
   nds_n8_store_multiple_7,nds_n8_store_multiple_8, nds_n8_store_multiple_12"
  "nds32_n8_last_load_to_ii_p"
)

;; LMW(N, N - 1) -> ADDR_IN_MOP(1)
(define_bypass 2
  "nds_n8_load_multiple_1,nds_n8_load_multiple_2, nds_n8_load_multiple_3,\
   nds_n8_load_multiple_4,nds_n8_load_multiple_5, nds_n8_load_multiple_6,\
   nds_n8_load_multiple_7,nds_n8_load_multiple_8, nds_n8_load_multiple_12"
  "nds_n8_branch,\
   nds_n8_load, nds_n8_store,\
   nds_n8_load_multiple_1,nds_n8_load_multiple_2, nds_n8_load_multiple_3,\
   nds_n8_load_multiple_4,nds_n8_load_multiple_5, nds_n8_load_multiple_6,\
   nds_n8_load_multiple_7,nds_n8_load_multiple_8, nds_n8_load_multiple_12,\
   nds_n8_store_multiple_1,nds_n8_store_multiple_2, nds_n8_store_multiple_3,\
   nds_n8_store_multiple_4,nds_n8_store_multiple_5, nds_n8_store_multiple_6,\
   nds_n8_store_multiple_7,nds_n8_store_multiple_8, nds_n8_store_multiple_12"
  "nds32_n8_last_load_two_to_ii_p"
)

;; LD_bi -> ADDR_IN_MOP(1)
(define_bypass 2
  "nds_n8_load"
  "nds_n8_branch,\
   nds_n8_load, nds_n8_store,\
   nds_n8_load_multiple_1,nds_n8_load_multiple_2, nds_n8_load_multiple_3,\
   nds_n8_load_multiple_4,nds_n8_load_multiple_5, nds_n8_load_multiple_6,\
   nds_n8_load_multiple_7,nds_n8_load_multiple_8, nds_n8_load_multiple_12,\
   nds_n8_store_multiple_1,nds_n8_store_multiple_2, nds_n8_store_multiple_3,\
   nds_n8_store_multiple_4,nds_n8_store_multiple_5, nds_n8_store_multiple_6,\
   nds_n8_store_multiple_7,nds_n8_store_multiple_8, nds_n8_store_multiple_12"
  "nds32_n8_load_bi_to_ii_p"
)

;; LD_!bi -> ALU, MOVD44_E, MUL, MAC_RaRb, DIV, BR_COND, ST_bi, ST_!bi_RI, SMW(N, 1)
(define_bypass 2
  "nds_n8_load"
  "nds_n8_alu,
   nds_n8_mul_fast, nds_n8_mul_slow,\
   nds_n8_mac_fast, nds_n8_mac_slow,\
   nds_n8_div,\
   nds_n8_branch,\
   nds_n8_store,\
   nds_n8_store_multiple_1,nds_n8_store_multiple_2, nds_n8_store_multiple_3,\
   nds_n8_store_multiple_4,nds_n8_store_multiple_5, nds_n8_store_multiple_6,\
   nds_n8_store_multiple_7,nds_n8_store_multiple_8, nds_n8_store_multiple_12"
  "nds32_n8_load_to_ex_p"
)

;; ALU, MOVD44_O, MUL, MAC, DIV_Rs, LD_bi, ADDR_OUT -> ADDR_IN_MOP(1)
(define_bypass 2
  "nds_n8_alu,
   nds_n8_mul_fast, nds_n8_mul_slow,\
   nds_n8_mac_fast, nds_n8_mac_slow,\
   nds_n8_div,\
   nds_n8_load, nds_n8_store,\
   nds_n8_load_multiple_1,nds_n8_load_multiple_2, nds_n8_load_multiple_3,\
   nds_n8_load_multiple_4,nds_n8_load_multiple_5, nds_n8_load_multiple_6,\
   nds_n8_load_multiple_7,nds_n8_load_multiple_8, nds_n8_load_multiple_12,\
   nds_n8_store_multiple_1,nds_n8_store_multiple_2, nds_n8_store_multiple_3,\
   nds_n8_store_multiple_4,nds_n8_store_multiple_5, nds_n8_store_multiple_6,\
   nds_n8_store_multiple_7,nds_n8_store_multiple_8, nds_n8_store_multiple_12"
  "nds_n8_branch,\
   nds_n8_load, nds_n8_store,\
   nds_n8_load_multiple_1,nds_n8_load_multiple_2, nds_n8_load_multiple_3,\
   nds_n8_load_multiple_4,nds_n8_load_multiple_5, nds_n8_load_multiple_6,\
   nds_n8_load_multiple_7,nds_n8_load_multiple_8, nds_n8_load_multiple_12,\
   nds_n8_store_multiple_1,nds_n8_store_multiple_2, nds_n8_store_multiple_3,\
   nds_n8_store_multiple_4,nds_n8_store_multiple_5, nds_n8_store_multiple_6,\
   nds_n8_store_multiple_7,nds_n8_store_multiple_8, nds_n8_store_multiple_12"
  "nds32_n8_ex_to_ii_p"
)

;; LMW(N, N) -> ALU, MOVD44_E, MUL, MAC_RaRb, DIV, BR_COND, ST_bi, ST_!bi_RI, SMW(N, 1)
(define_bypass 2
  "nds_n8_load_multiple_1,nds_n8_load_multiple_2, nds_n8_load_multiple_3,\
   nds_n8_load_multiple_4,nds_n8_load_multiple_5, nds_n8_load_multiple_6,\
   nds_n8_load_multiple_7,nds_n8_load_multiple_8, nds_n8_load_multiple_12"
  "nds_n8_alu,
   nds_n8_mul_fast, nds_n8_mul_slow,\
   nds_n8_mac_fast, nds_n8_mac_slow,\
   nds_n8_div,\
   nds_n8_branch,\
   nds_n8_store,\
   nds_n8_store_multiple_1,nds_n8_store_multiple_2, nds_n8_store_multiple_3,\
   nds_n8_store_multiple_4,nds_n8_store_multiple_5, nds_n8_store_multiple_6,\
   nds_n8_store_multiple_7,nds_n8_store_multiple_8, nds_n8_store_multiple_12"
  "nds32_n8_last_load_to_ex_p"
)
