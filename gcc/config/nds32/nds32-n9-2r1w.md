;; Pipeline descriptions of Andes NDS32 cpu for GNU compiler
;; Copyright (C) 2012-2021 Free Software Foundation, Inc.
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
;; Define N9 2R1W pipeline settings.
;; ------------------------------------------------------------------------

(define_automaton "nds32_n9_2r1w_machine")

;; ------------------------------------------------------------------------
;; Pipeline Stages
;; ------------------------------------------------------------------------
;; IF - Instruction Fetch
;; II - Instruction Issue / Instruction Decode
;; EX - Instruction Execution
;; MM - Memory Execution
;; WB - Instruction Retire / Result Write-Back

(define_cpu_unit "n9_2r1w_ii" "nds32_n9_2r1w_machine")
(define_cpu_unit "n9_2r1w_ex" "nds32_n9_2r1w_machine")
(define_cpu_unit "n9_2r1w_mm" "nds32_n9_2r1w_machine")
(define_cpu_unit "n9_2r1w_wb" "nds32_n9_2r1w_machine")

(define_insn_reservation "nds_n9_2r1w_unknown" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_2R1W")
       (and (eq_attr "type" "unknown")
	    (eq_attr "pipeline_model" "n9")))
  "n9_2r1w_ii, n9_2r1w_ex, n9_2r1w_mm, n9_2r1w_wb")

(define_insn_reservation "nds_n9_2r1w_misc" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_2R1W")
       (and (eq_attr "type" "misc")
	    (eq_attr "pipeline_model" "n9")))
  "n9_2r1w_ii, n9_2r1w_ex, n9_2r1w_mm, n9_2r1w_wb")

(define_insn_reservation "nds_n9_2r1w_mmu" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_2R1W")
       (and (eq_attr "type" "mmu")
	    (eq_attr "pipeline_model" "n9")))
  "n9_2r1w_ii, n9_2r1w_ex, n9_2r1w_mm, n9_2r1w_wb")

(define_insn_reservation "nds_n9_2r1w_alu" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_2R1W")
       (and (eq_attr "type" "alu")
	    (eq_attr "pipeline_model" "n9")))
  "n9_2r1w_ii, n9_2r1w_ex, n9_2r1w_mm, n9_2r1w_wb")

(define_insn_reservation "nds_n9_2r1w_alu_shift" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_2R1W")
       (and (eq_attr "type" "alu_shift")
	    (eq_attr "pipeline_model" "n9")))
  "n9_2r1w_ii, n9_2r1w_ii+n9_2r1w_ex, n9_2r1w_ex+n9_2r1w_mm, n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_wb")

(define_insn_reservation "nds_n9_2r1w_pbsad" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_2R1W")
       (and (eq_attr "type" "pbsad")
	    (eq_attr "pipeline_model" "n9")))
  "n9_2r1w_ii, n9_2r1w_ex*3, n9_2r1w_mm, n9_2r1w_wb")

(define_insn_reservation "nds_n9_2r1w_pbsada" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_2R1W")
       (and (eq_attr "type" "pbsada")
	    (eq_attr "pipeline_model" "n9")))
  "n9_2r1w_ii, n9_2r1w_ex*3, n9_2r1w_mm, n9_2r1w_wb")

(define_insn_reservation "nds_n9_2r1w_load" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_2R1W")
       (and (match_test "nds32::load_single_p (insn)")
	    (eq_attr "pipeline_model" "n9")))
  "n9_2r1w_ii, n9_2r1w_ex, n9_2r1w_mm, n9_2r1w_wb")

(define_insn_reservation "nds_n9_2r1w_store" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_2R1W")
       (and (match_test "nds32::store_single_p (insn)")
	    (eq_attr "pipeline_model" "n9")))
  "n9_2r1w_ii, n9_2r1w_ex, n9_2r1w_mm, n9_2r1w_wb")

(define_insn_reservation "nds_n9_2r1w_load_multiple_1" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_2R1W")
       (and (eq_attr "pipeline_model" "n9")
	    (and (eq_attr "type" "load_multiple")
		 (eq_attr "combo" "1"))))
  "n9_2r1w_ii, n9_2r1w_ex, n9_2r1w_mm, n9_2r1w_wb")

(define_insn_reservation "nds_n9_2r1w_load_multiple_2" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_2R1W")
       (and (eq_attr "pipeline_model" "n9")
	    (ior (and (eq_attr "type" "load_multiple")
		      (eq_attr "combo" "2"))
		 (match_test "nds32::load_double_p (insn)"))))
  "n9_2r1w_ii, n9_2r1w_ii+n9_2r1w_ex, n9_2r1w_ex+n9_2r1w_mm, n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_wb")

(define_insn_reservation "nds_n9_2r1w_load_multiple_3" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_2R1W")
       (and (eq_attr "pipeline_model" "n9")
	    (and (eq_attr "type" "load_multiple")
		 (eq_attr "combo" "3"))))
  "n9_2r1w_ii, n9_2r1w_ii+n9_2r1w_ex, n9_2r1w_ii+n9_2r1w_ex+n9_2r1w_mm, n9_2r1w_ex+n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_wb")

(define_insn_reservation "nds_n9_2r1w_load_multiple_4" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_2R1W")
       (and (eq_attr "pipeline_model" "n9")
	    (and (eq_attr "type" "load_multiple")
		 (eq_attr "combo" "4"))))
  "n9_2r1w_ii, n9_2r1w_ii+n9_2r1w_ex, n9_2r1w_ii+n9_2r1w_ex+n9_2r1w_mm, n9_2r1w_ii+n9_2r1w_ex+n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_ex+n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_wb")

(define_insn_reservation "nds_n9_2r1w_load_multiple_5" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_2R1W")
       (and (eq_attr "pipeline_model" "n9")
	    (and (eq_attr "type" "load_multiple")
		 (eq_attr "combo" "5"))))
  "n9_2r1w_ii, n9_2r1w_ii+n9_2r1w_ex, n9_2r1w_ii+n9_2r1w_ex+n9_2r1w_mm, (n9_2r1w_ii+n9_2r1w_ex+n9_2r1w_mm+n9_2r1w_wb)*2, n9_2r1w_ex+n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_wb")

(define_insn_reservation "nds_n9_2r1w_load_multiple_6" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_2R1W")
       (and (eq_attr "pipeline_model" "n9")
	    (and (eq_attr "type" "load_multiple")
		 (eq_attr "combo" "6"))))
  "n9_2r1w_ii, n9_2r1w_ii+n9_2r1w_ex, n9_2r1w_ii+n9_2r1w_ex+n9_2r1w_mm, (n9_2r1w_ii+n9_2r1w_ex+n9_2r1w_mm+n9_2r1w_wb)*3, n9_2r1w_ex+n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_wb")

(define_insn_reservation "nds_n9_2r1w_load_multiple_7" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_2R1W")
       (and (eq_attr "pipeline_model" "n9")
	    (and (eq_attr "type" "load_multiple")
		 (eq_attr "combo" "7"))))
  "n9_2r1w_ii, n9_2r1w_ii+n9_2r1w_ex, n9_2r1w_ii+n9_2r1w_ex+n9_2r1w_mm, (n9_2r1w_ii+n9_2r1w_ex+n9_2r1w_mm+n9_2r1w_wb)*4, n9_2r1w_ex+n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_wb")

(define_insn_reservation "nds_n9_2r1w_load_multiple_8" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_2R1W")
       (and (eq_attr "pipeline_model" "n9")
	    (and (eq_attr "type" "load_multiple")
		 (eq_attr "combo" "8"))))
  "n9_2r1w_ii, n9_2r1w_ii+n9_2r1w_ex, n9_2r1w_ii+n9_2r1w_ex+n9_2r1w_mm, (n9_2r1w_ii+n9_2r1w_ex+n9_2r1w_mm+n9_2r1w_wb)*5, n9_2r1w_ex+n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_wb")

(define_insn_reservation "nds_n9_2r1w_load_multiple_12" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_2R1W")
       (and (eq_attr "pipeline_model" "n9")
	    (and (eq_attr "type" "load_multiple")
		 (eq_attr "combo" "12"))))
  "n9_2r1w_ii, n9_2r1w_ii+n9_2r1w_ex, n9_2r1w_ii+n9_2r1w_ex+n9_2r1w_mm, (n9_2r1w_ii+n9_2r1w_ex+n9_2r1w_mm+n9_2r1w_wb)*9, n9_2r1w_ex+n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_wb")

(define_insn_reservation "nds_n9_2r1w_store_multiple_1" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_2R1W")
       (and (eq_attr "pipeline_model" "n9")
	    (and (eq_attr "type" "store_multiple")
		 (eq_attr "combo" "1"))))
  "n9_2r1w_ii, n9_2r1w_ex, n9_2r1w_mm, n9_2r1w_wb")

(define_insn_reservation "nds_n9_2r1w_store_multiple_2" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_2R1W")
       (and (eq_attr "pipeline_model" "n9")
	    (ior (and (eq_attr "type" "store_multiple")
		      (eq_attr "combo" "2"))
		 (match_test "nds32::store_double_p (insn)"))))
  "n9_2r1w_ii, n9_2r1w_ii+n9_2r1w_ex, n9_2r1w_ex+n9_2r1w_mm, n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_wb")

(define_insn_reservation "nds_n9_2r1w_store_multiple_3" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_2R1W")
       (and (eq_attr "pipeline_model" "n9")
	    (and (eq_attr "type" "store_multiple")
		 (eq_attr "combo" "3"))))
  "n9_2r1w_ii, n9_2r1w_ii+n9_2r1w_ex, n9_2r1w_ii+n9_2r1w_ex+n9_2r1w_mm, n9_2r1w_ex+n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_wb")

(define_insn_reservation "nds_n9_2r1w_store_multiple_4" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_2R1W")
       (and (eq_attr "pipeline_model" "n9")
	    (and (eq_attr "type" "store_multiple")
		 (eq_attr "combo" "4"))))
  "n9_2r1w_ii, n9_2r1w_ii+n9_2r1w_ex, n9_2r1w_ii+n9_2r1w_ex+n9_2r1w_mm, n9_2r1w_ii+n9_2r1w_ex+n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_ex+n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_wb")

(define_insn_reservation "nds_n9_2r1w_store_multiple_5" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_2R1W")
       (and (eq_attr "pipeline_model" "n9")
	    (and (eq_attr "type" "store_multiple")
		 (eq_attr "combo" "5"))))
  "n9_2r1w_ii, n9_2r1w_ii+n9_2r1w_ex, n9_2r1w_ii+n9_2r1w_ex+n9_2r1w_mm, (n9_2r1w_ii+n9_2r1w_ex+n9_2r1w_mm+n9_2r1w_wb)*2, n9_2r1w_ex+n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_wb")

(define_insn_reservation "nds_n9_2r1w_store_multiple_6" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_2R1W")
       (and (eq_attr "pipeline_model" "n9")
	    (and (eq_attr "type" "store_multiple")
		 (eq_attr "combo" "6"))))
  "n9_2r1w_ii, n9_2r1w_ii+n9_2r1w_ex, n9_2r1w_ii+n9_2r1w_ex+n9_2r1w_mm, (n9_2r1w_ii+n9_2r1w_ex+n9_2r1w_mm+n9_2r1w_wb)*3, n9_2r1w_ex+n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_wb")

(define_insn_reservation "nds_n9_2r1w_store_multiple_7" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_2R1W")
       (and (eq_attr "pipeline_model" "n9")
	    (and (eq_attr "type" "store_multiple")
		 (eq_attr "combo" "7"))))
  "n9_2r1w_ii, n9_2r1w_ii+n9_2r1w_ex, n9_2r1w_ii+n9_2r1w_ex+n9_2r1w_mm, (n9_2r1w_ii+n9_2r1w_ex+n9_2r1w_mm+n9_2r1w_wb)*4, n9_2r1w_ex+n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_wb")

(define_insn_reservation "nds_n9_2r1w_store_multiple_8" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_2R1W")
       (and (eq_attr "pipeline_model" "n9")
	    (and (eq_attr "type" "store_multiple")
		 (eq_attr "combo" "8"))))
  "n9_2r1w_ii, n9_2r1w_ii+n9_2r1w_ex, n9_2r1w_ii+n9_2r1w_ex+n9_2r1w_mm, (n9_2r1w_ii+n9_2r1w_ex+n9_2r1w_mm+n9_2r1w_wb)*5, n9_2r1w_ex+n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_wb")

(define_insn_reservation "nds_n9_2r1w_store_multiple_12" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_2R1W")
       (and (eq_attr "pipeline_model" "n9")
	    (and (eq_attr "type" "store_multiple")
		 (eq_attr "combo" "12"))))
  "n9_2r1w_ii, n9_2r1w_ii+n9_2r1w_ex, n9_2r1w_ii+n9_2r1w_ex+n9_2r1w_mm, (n9_2r1w_ii+n9_2r1w_ex+n9_2r1w_mm+n9_2r1w_wb)*9, n9_2r1w_ex+n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_wb")

(define_insn_reservation "nds_n9_2r1w_mul_fast" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_2R1W && nds32_mul_config != MUL_TYPE_SLOW")
       (and (eq_attr "type" "mul")
	    (eq_attr "pipeline_model" "n9")))
  "n9_2r1w_ii, n9_2r1w_ex, n9_2r1w_mm, n9_2r1w_wb")

(define_insn_reservation "nds_n9_2r1w_mul_slow" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_2R1W && nds32_mul_config == MUL_TYPE_SLOW")
       (and (eq_attr "type" "mul")
	    (eq_attr "pipeline_model" "n9")))
  "n9_2r1w_ii, n9_2r1w_ex*17, n9_2r1w_mm, n9_2r1w_wb")

(define_insn_reservation "nds_n9_2r1w_mac_fast" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_2R1W && nds32_mul_config != MUL_TYPE_SLOW")
       (and (eq_attr "type" "mac")
	    (eq_attr "pipeline_model" "n9")))
  "n9_2r1w_ii, n9_2r1w_ii+n9_2r1w_ex, n9_2r1w_ex+n9_2r1w_mm, n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_wb")

(define_insn_reservation "nds_n9_2r1w_mac_slow" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_2R1W && nds32_mul_config == MUL_TYPE_SLOW")
       (and (eq_attr "type" "mac")
	    (eq_attr "pipeline_model" "n9")))
  "n9_2r1w_ii, (n9_2r1w_ii+n9_2r1w_ex)*17, n9_2r1w_ex+n9_2r1w_mm, n9_2r1w_ex+n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_wb")

(define_insn_reservation "nds_n9_2r1w_div" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_2R1W")
       (and (eq_attr "type" "div")
	    (eq_attr "pipeline_model" "n9")))
  "n9_2r1w_ii, (n9_2r1w_ii+n9_2r1w_ex)*34, n9_2r1w_ex+n9_2r1w_mm, n9_2r1w_mm+n9_2r1w_wb, n9_2r1w_wb")

(define_insn_reservation "nds_n9_2r1w_branch" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_2R1W")
       (and (eq_attr "type" "branch")
	    (eq_attr "pipeline_model" "n9")))
  "n9_2r1w_ii, n9_2r1w_ex, n9_2r1w_mm, n9_2r1w_wb")

;; ------------------------------------------------------------------------
;; Comment Notations and Bypass Rules
;; ------------------------------------------------------------------------
;; Producers (LHS)
;;   LD_!bi
;;     Load data from the memory (without updating the base register) and
;;     produce the loaded data. The result is ready at MM. Because the register
;;     port is 2R1W, two micro-operations are required if the base register
;;     should be updated. In this case, the base register is updated by the
;;     second micro-operation, and the updated result is ready at EX.
;;   LMW(N, M)
;;     There are N micro-operations within an instruction that loads multiple
;;     words. The result produced by the M-th micro-operation is sent to
;;     consumers. The result is ready at MM.  If the base register should be
;;     updated, an extra micro-operation is apppended to the end of the
;;     sequence, and the result is ready at EX.
;;   MUL, MAC
;;     Compute data in the multiply-adder and produce the data. The result
;;     is ready at MM.
;;   DIV
;;     Compute data in the divider and produce the data. The result is ready
;;     at MM.
;;
;; Consumers (RHS)
;;   ALU, PBSAD, PBSADA_RaRb, MUL, MAC, DIV, MMU
;;     Require operands at EX.
;;   ALU_SHIFT_Rb
;;     An ALU-SHIFT instruction consists of a shift micro-operation followed
;;     by an arithmetic micro-operation. The operand Rb is used by the first
;;     micro-operation, and there are some latencies if data dependency occurs.
;;   MOVD44_E
;;     A double-word move instruction needs two micro-operations because the
;;     reigster ports is 2R1W. The first micro-operation writes an even number
;;     register, and the second micro-operation writes an odd number register.
;;     Each input operand is required at EX for each micro-operation. MOVD44_E
;;     stands for the first micro-operation.
;;   MAC_RaRb, M2R
;;     MAC instructions do multiplication at EX and do accumulation at MM, but
;;     MAC instructions which operate on general purpose registers always
;;     require operands at EX because MM stage cannot be forwarded in 2R1W mode.
;;   ADDR_IN
;;     If an instruction requires an address as its input operand, the address
;;     is required at EX.
;;   ST_bi
;;     A post-increment store instruction requires its data at EX because MM
;;     cannot be forwarded in 2R1W mode.
;;   ST_!bi_RI
;;     A store instruction with an immediate offset requires its data at EX
;;     because MM cannot be forwarded in 2R1W mode. If the offset field is a
;;     register (ST_!bi_RR), the instruction will be separated into two micro-
;;     operations, and the second one requires the input operand at EX in order
;;     to store it to the memory.
;;   SMW(N, M)
;;     There are N micro-operations within an instruction that stores multiple
;;     words. Each M-th micro-operation requires its data at MM.
;;   BR
;;     If a branch instruction is conditional, its input data is required at EX.

;; LD_!bi, MUL, MAC
;;   -> ALU, ALU_SHIFT_Rb, PBSAD, PBSADA_RaRb, MOVD44_E, MUL, MAC_RaRb, M2R, DIV, ADDR_IN_!bi, ADDR_IN_bi_Ra, ST_bi, ST_!bi_RI, BR, MMU
(define_bypass 2
  "nds_n9_2r1w_load,\
   nds_n9_2r1w_mul_fast, nds_n9_2r1w_mul_slow,\
   nds_n9_2r1w_mac_fast, nds_n9_2r1w_mac_slow"
  "nds_n9_2r1w_alu, nds_n9_2r1w_alu_shift,\
   nds_n9_2r1w_pbsad, nds_n9_2r1w_pbsada,\
   nds_n9_2r1w_mul_fast, nds_n9_2r1w_mul_slow,\
   nds_n9_2r1w_mac_fast, nds_n9_2r1w_mac_slow,\
   nds_n9_2r1w_branch,\
   nds_n9_2r1w_div,\
   nds_n9_2r1w_load,nds_n9_2r1w_store,\
   nds_n9_2r1w_load_multiple_1,nds_n9_2r1w_load_multiple_2, nds_n9_2r1w_load_multiple_3,\
   nds_n9_2r1w_load_multiple_4,nds_n9_2r1w_load_multiple_5, nds_n9_2r1w_load_multiple_6,\
   nds_n9_2r1w_load_multiple_7,nds_n9_2r1w_load_multiple_8, nds_n9_2r1w_load_multiple_12,\
   nds_n9_2r1w_store_multiple_1,nds_n9_2r1w_store_multiple_2, nds_n9_2r1w_store_multiple_3,\
   nds_n9_2r1w_store_multiple_4,nds_n9_2r1w_store_multiple_5, nds_n9_2r1w_store_multiple_6,\
   nds_n9_2r1w_store_multiple_7,nds_n9_2r1w_store_multiple_8, nds_n9_2r1w_store_multiple_12,\
   nds_n9_2r1w_mmu"
  "nds32_n9_2r1w_mm_to_ex_p"
)

;; LMW(N, N)
;;   -> ALU, ALU_SHIFT_Rb, PBSAD, PBSADA_RaRb, MOVD44_E, MUL, MAC_RaRb, M2R, DIV, ADDR_IN_!bi, ADDR_IN_bi_Ra, ST_bi, ST_!bi_RI, BR, MMU
(define_bypass 2
  "nds_n9_2r1w_load_multiple_1,nds_n9_2r1w_load_multiple_2, nds_n9_2r1w_load_multiple_3,\
   nds_n9_2r1w_load_multiple_4,nds_n9_2r1w_load_multiple_5, nds_n9_2r1w_load_multiple_6,\
   nds_n9_2r1w_load_multiple_7,nds_n9_2r1w_load_multiple_8, nds_n9_2r1w_load_multiple_12"
  "nds_n9_2r1w_alu, nds_n9_2r1w_alu_shift,\
   nds_n9_2r1w_pbsad, nds_n9_2r1w_pbsada,\
   nds_n9_2r1w_mul_fast, nds_n9_2r1w_mul_slow,\
   nds_n9_2r1w_mac_fast, nds_n9_2r1w_mac_slow,\
   nds_n9_2r1w_branch,\
   nds_n9_2r1w_div,\
   nds_n9_2r1w_load,nds_n9_2r1w_store,\
   nds_n9_2r1w_load_multiple_1,nds_n9_2r1w_load_multiple_2, nds_n9_2r1w_load_multiple_3,\
   nds_n9_2r1w_load_multiple_4,nds_n9_2r1w_load_multiple_5, nds_n9_2r1w_load_multiple_6,\
   nds_n9_2r1w_load_multiple_7,nds_n9_2r1w_load_multiple_8, nds_n9_2r1w_load_multiple_12,\
   nds_n9_2r1w_store_multiple_1,nds_n9_2r1w_store_multiple_2, nds_n9_2r1w_store_multiple_3,\
   nds_n9_2r1w_store_multiple_4,nds_n9_2r1w_store_multiple_5, nds_n9_2r1w_store_multiple_6,\
   nds_n9_2r1w_store_multiple_7,nds_n9_2r1w_store_multiple_8, nds_n9_2r1w_store_multiple_12,\
   nds_n9_2r1w_mmu"
  "nds32_n9_last_load_to_ex_p"
)
