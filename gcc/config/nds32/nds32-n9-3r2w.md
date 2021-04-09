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
;; Define N9 3R2W pipeline settings.
;; ------------------------------------------------------------------------

(define_automaton "nds32_n9_3r2w_machine")

;; ------------------------------------------------------------------------
;; Pipeline Stages
;; ------------------------------------------------------------------------
;; IF - Instruction Fetch
;; II - Instruction Issue / Instruction Decode
;; EX - Instruction Execution
;; MM - Memory Execution
;; WB - Instruction Retire / Result Write-Back

(define_cpu_unit "n9_3r2w_ii" "nds32_n9_3r2w_machine")
(define_cpu_unit "n9_3r2w_ex" "nds32_n9_3r2w_machine")
(define_cpu_unit "n9_3r2w_mm" "nds32_n9_3r2w_machine")
(define_cpu_unit "n9_3r2w_wb" "nds32_n9_3r2w_machine")

(define_insn_reservation "nds_n9_3r2w_unknown" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W")
       (and (eq_attr "type" "unknown")
	    (eq_attr "pipeline_model" "n9")))
  "n9_3r2w_ii, n9_3r2w_ex, n9_3r2w_mm, n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_misc" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W")
       (and (eq_attr "type" "misc")
	    (eq_attr "pipeline_model" "n9")))
  "n9_3r2w_ii, n9_3r2w_ex, n9_3r2w_mm, n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_mmu" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W")
       (and (eq_attr "type" "mmu")
	    (eq_attr "pipeline_model" "n9")))
  "n9_3r2w_ii, n9_3r2w_ex, n9_3r2w_mm, n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_alu" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W")
       (and (eq_attr "type" "alu")
	    (eq_attr "pipeline_model" "n9")))
  "n9_3r2w_ii, n9_3r2w_ex, n9_3r2w_mm, n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_alu_shift" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W")
       (and (eq_attr "type" "alu_shift")
	    (eq_attr "pipeline_model" "n9")))
  "n9_3r2w_ii, n9_3r2w_ii+n9_3r2w_ex, n9_3r2w_ex+n9_3r2w_mm, n9_3r2w_mm+n9_3r2w_wb, n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_pbsad" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W")
       (and (eq_attr "type" "pbsad")
	    (eq_attr "pipeline_model" "n9")))
  "n9_3r2w_ii, n9_3r2w_ex*3, n9_3r2w_mm, n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_pbsada" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W")
       (and (eq_attr "type" "pbsada")
	    (eq_attr "pipeline_model" "n9")))
  "n9_3r2w_ii, n9_3r2w_ex*3, n9_3r2w_mm, n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_load" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W")
       (and (match_test "nds32::load_single_p (insn)")
	    (eq_attr "pipeline_model" "n9")))
  "n9_3r2w_ii, n9_3r2w_ex, n9_3r2w_mm, n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_store" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W")
       (and (match_test "nds32::store_single_p (insn)")
	    (eq_attr "pipeline_model" "n9")))
  "n9_3r2w_ii, n9_3r2w_ex, n9_3r2w_mm, n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_load_multiple_1" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W")
       (and (eq_attr "pipeline_model" "n9")
	    (and (eq_attr "type" "load_multiple")
		 (eq_attr "combo" "1"))))
  "n9_3r2w_ii, n9_3r2w_ex, n9_3r2w_mm, n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_load_multiple_2" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W")
       (and (eq_attr "pipeline_model" "n9")
	    (ior (and (eq_attr "type" "load_multiple")
		      (eq_attr "combo" "2"))
		 (match_test "nds32::load_double_p (insn)"))))
  "n9_3r2w_ii, n9_3r2w_ii+n9_3r2w_ex, n9_3r2w_ex+n9_3r2w_mm, n9_3r2w_mm+n9_3r2w_wb, n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_load_multiple_3" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W")
       (and (eq_attr "pipeline_model" "n9")
	    (and (eq_attr "type" "load_multiple")
		 (eq_attr "combo" "3"))))
  "n9_3r2w_ii, n9_3r2w_ii+n9_3r2w_ex, n9_3r2w_ii+n9_3r2w_ex+n9_3r2w_mm, n9_3r2w_ex+n9_3r2w_mm+n9_3r2w_wb, n9_3r2w_mm+n9_3r2w_wb, n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_load_multiple_4" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W")
       (and (eq_attr "pipeline_model" "n9")
	    (and (eq_attr "type" "load_multiple")
		 (eq_attr "combo" "4"))))
  "n9_3r2w_ii, n9_3r2w_ii+n9_3r2w_ex, n9_3r2w_ii+n9_3r2w_ex+n9_3r2w_mm, n9_3r2w_ii+n9_3r2w_ex+n9_3r2w_mm+n9_3r2w_wb, n9_3r2w_ex+n9_3r2w_mm+n9_3r2w_wb, n9_3r2w_mm+n9_3r2w_wb, n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_load_multiple_5" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W")
       (and (eq_attr "pipeline_model" "n9")
	    (and (eq_attr "type" "load_multiple")
		 (eq_attr "combo" "5"))))
  "n9_3r2w_ii, n9_3r2w_ii+n9_3r2w_ex, n9_3r2w_ii+n9_3r2w_ex+n9_3r2w_mm, (n9_3r2w_ii+n9_3r2w_ex+n9_3r2w_mm+n9_3r2w_wb)*2, n9_3r2w_ex+n9_3r2w_mm+n9_3r2w_wb, n9_3r2w_mm+n9_3r2w_wb, n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_load_multiple_6" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W")
       (and (eq_attr "pipeline_model" "n9")
	    (and (eq_attr "type" "load_multiple")
		 (eq_attr "combo" "6"))))
  "n9_3r2w_ii, n9_3r2w_ii+n9_3r2w_ex, n9_3r2w_ii+n9_3r2w_ex+n9_3r2w_mm, (n9_3r2w_ii+n9_3r2w_ex+n9_3r2w_mm+n9_3r2w_wb)*3, n9_3r2w_ex+n9_3r2w_mm+n9_3r2w_wb, n9_3r2w_mm+n9_3r2w_wb, n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_load_multiple_7" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W")
       (and (eq_attr "pipeline_model" "n9")
	    (and (eq_attr "type" "load_multiple")
		 (eq_attr "combo" "7"))))
  "n9_3r2w_ii, n9_3r2w_ii+n9_3r2w_ex, n9_3r2w_ii+n9_3r2w_ex+n9_3r2w_mm, (n9_3r2w_ii+n9_3r2w_ex+n9_3r2w_mm+n9_3r2w_wb)*4, n9_3r2w_ex+n9_3r2w_mm+n9_3r2w_wb, n9_3r2w_mm+n9_3r2w_wb, n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_load_multiple_8" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W")
       (and (eq_attr "pipeline_model" "n9")
	    (and (eq_attr "type" "load_multiple")
		 (eq_attr "combo" "8"))))
  "n9_3r2w_ii, n9_3r2w_ii+n9_3r2w_ex, n9_3r2w_ii+n9_3r2w_ex+n9_3r2w_mm, (n9_3r2w_ii+n9_3r2w_ex+n9_3r2w_mm+n9_3r2w_wb)*5, n9_3r2w_ex+n9_3r2w_mm+n9_3r2w_wb, n9_3r2w_mm+n9_3r2w_wb, n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_load_multiple_12" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W")
       (and (eq_attr "pipeline_model" "n9")
	    (and (eq_attr "type" "load_multiple")
		 (eq_attr "combo" "12"))))
  "n9_3r2w_ii, n9_3r2w_ii+n9_3r2w_ex, n9_3r2w_ii+n9_3r2w_ex+n9_3r2w_mm, (n9_3r2w_ii+n9_3r2w_ex+n9_3r2w_mm+n9_3r2w_wb)*9, n9_3r2w_ex+n9_3r2w_mm+n9_3r2w_wb, n9_3r2w_mm+n9_3r2w_wb, n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_store_multiple_1" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W")
       (and (eq_attr "pipeline_model" "n9")
	    (and (eq_attr "type" "store_multiple")
		 (eq_attr "combo" "1"))))
  "n9_3r2w_ii, n9_3r2w_ex, n9_3r2w_mm, n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_store_multiple_2" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W")
       (and (eq_attr "pipeline_model" "n9")
	    (ior (and (eq_attr "type" "store_multiple")
		      (eq_attr "combo" "2"))
		 (match_test "nds32::store_double_p (insn)"))))
  "n9_3r2w_ii, n9_3r2w_ii+n9_3r2w_ex, n9_3r2w_ex+n9_3r2w_mm, n9_3r2w_mm+n9_3r2w_wb, n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_store_multiple_3" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W")
       (and (eq_attr "pipeline_model" "n9")
	    (and (eq_attr "type" "store_multiple")
		 (eq_attr "combo" "3"))))
  "n9_3r2w_ii, n9_3r2w_ii+n9_3r2w_ex, n9_3r2w_ii+n9_3r2w_ex+n9_3r2w_mm, n9_3r2w_ex+n9_3r2w_mm+n9_3r2w_wb, n9_3r2w_mm+n9_3r2w_wb, n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_store_multiple_4" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W")
       (and (eq_attr "pipeline_model" "n9")
	    (and (eq_attr "type" "store_multiple")
		 (eq_attr "combo" "4"))))
  "n9_3r2w_ii, n9_3r2w_ii+n9_3r2w_ex, n9_3r2w_ii+n9_3r2w_ex+n9_3r2w_mm, n9_3r2w_ii+n9_3r2w_ex+n9_3r2w_mm+n9_3r2w_wb, n9_3r2w_ex+n9_3r2w_mm+n9_3r2w_wb, n9_3r2w_mm+n9_3r2w_wb, n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_store_multiple_5" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W")
       (and (eq_attr "pipeline_model" "n9")
	    (and (eq_attr "type" "store_multiple")
		 (eq_attr "combo" "5"))))
  "n9_3r2w_ii, n9_3r2w_ii+n9_3r2w_ex, n9_3r2w_ii+n9_3r2w_ex+n9_3r2w_mm, (n9_3r2w_ii+n9_3r2w_ex+n9_3r2w_mm+n9_3r2w_wb)*2, n9_3r2w_ex+n9_3r2w_mm+n9_3r2w_wb, n9_3r2w_mm+n9_3r2w_wb, n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_store_multiple_6" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W")
       (and (eq_attr "pipeline_model" "n9")
	    (and (eq_attr "type" "store_multiple")
		 (eq_attr "combo" "6"))))
  "n9_3r2w_ii, n9_3r2w_ii+n9_3r2w_ex, n9_3r2w_ii+n9_3r2w_ex+n9_3r2w_mm, (n9_3r2w_ii+n9_3r2w_ex+n9_3r2w_mm+n9_3r2w_wb)*3, n9_3r2w_ex+n9_3r2w_mm+n9_3r2w_wb, n9_3r2w_mm+n9_3r2w_wb, n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_store_multiple_7" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W")
       (and (eq_attr "pipeline_model" "n9")
	    (and (eq_attr "type" "store_multiple")
		 (eq_attr "combo" "7"))))
  "n9_3r2w_ii, n9_3r2w_ii+n9_3r2w_ex, n9_3r2w_ii+n9_3r2w_ex+n9_3r2w_mm, (n9_3r2w_ii+n9_3r2w_ex+n9_3r2w_mm+n9_3r2w_wb)*4, n9_3r2w_ex+n9_3r2w_mm+n9_3r2w_wb, n9_3r2w_mm+n9_3r2w_wb, n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_store_multiple_8" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W")
       (and (eq_attr "pipeline_model" "n9")
	    (and (eq_attr "type" "store_multiple")
		 (eq_attr "combo" "8"))))
  "n9_3r2w_ii, n9_3r2w_ii+n9_3r2w_ex, n9_3r2w_ii+n9_3r2w_ex+n9_3r2w_mm, (n9_3r2w_ii+n9_3r2w_ex+n9_3r2w_mm+n9_3r2w_wb)*5, n9_3r2w_ex+n9_3r2w_mm+n9_3r2w_wb, n9_3r2w_mm+n9_3r2w_wb, n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_store_multiple_12" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W")
       (and (eq_attr "pipeline_model" "n9")
	    (and (eq_attr "type" "store_multiple")
		 (eq_attr "combo" "12"))))
  "n9_3r2w_ii, n9_3r2w_ii+n9_3r2w_ex, n9_3r2w_ii+n9_3r2w_ex+n9_3r2w_mm, (n9_3r2w_ii+n9_3r2w_ex+n9_3r2w_mm+n9_3r2w_wb)*9, n9_3r2w_ex+n9_3r2w_mm+n9_3r2w_wb, n9_3r2w_mm+n9_3r2w_wb, n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_mul_fast1" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W && nds32_mul_config == MUL_TYPE_FAST_1")
       (and (eq_attr "type" "mul")
	    (eq_attr "pipeline_model" "n9")))
  "n9_3r2w_ii, n9_3r2w_ex, n9_3r2w_mm, n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_mul_fast2" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W && nds32_mul_config == MUL_TYPE_FAST_2")
       (and (eq_attr "type" "mul")
	    (eq_attr "pipeline_model" "n9")))
  "n9_3r2w_ii, n9_3r2w_ex*2, n9_3r2w_mm, n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_mul_slow" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W && nds32_mul_config == MUL_TYPE_SLOW")
       (and (eq_attr "type" "mul")
	    (eq_attr "pipeline_model" "n9")))
  "n9_3r2w_ii, n9_3r2w_ex*17, n9_3r2w_mm, n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_mac_fast1" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W && nds32_mul_config == MUL_TYPE_FAST_1")
       (and (eq_attr "type" "mac")
	    (eq_attr "pipeline_model" "n9")))
  "n9_3r2w_ii, n9_3r2w_ex, n9_3r2w_mm, n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_mac_fast2" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W && nds32_mul_config == MUL_TYPE_FAST_2")
       (and (eq_attr "type" "mac")
	    (eq_attr "pipeline_model" "n9")))
  "n9_3r2w_ii, n9_3r2w_ex*2, n9_3r2w_mm, n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_mac_slow" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W && nds32_mul_config == MUL_TYPE_SLOW")
       (and (eq_attr "type" "mac")
	    (eq_attr "pipeline_model" "n9")))
  "n9_3r2w_ii, n9_3r2w_ex*17, n9_3r2w_ex+n9_3r2w_mm, n9_3r2w_ex+n9_3r2w_mm+n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_div" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W")
       (and (eq_attr "type" "div")
	    (eq_attr "pipeline_model" "n9")))
  "n9_3r2w_ii, n9_3r2w_ex*34, n9_3r2w_mm, n9_3r2w_wb")

(define_insn_reservation "nds_n9_3r2w_branch" 1
  (and (match_test "nds32_register_ports_config == REG_PORT_3R2W")
       (and (eq_attr "type" "branch")
	    (eq_attr "pipeline_model" "n9")))
  "n9_3r2w_ii, n9_3r2w_ex, n9_3r2w_mm, n9_3r2w_wb")

;; ------------------------------------------------------------------------
;; Comment Notations and Bypass Rules
;; ------------------------------------------------------------------------
;; Producers (LHS)
;;   LD
;;     Load data from the memory and produce the loaded data. The result is
;;     ready at MM.
;;   LMW(N, M)
;;     There are N micro-operations within an instruction that loads multiple
;;     words. The result produced by the M-th micro-operation is sent to
;;     consumers. The result is ready at MM.
;;   MUL, MAC
;;     Compute data in the multiply-adder and produce the data. The result
;;     is ready at MM.
;;   DIV
;;     Compute data in the divider and produce the data. The result is ready
;;     at MM.
;;
;; Consumers (RHS)
;;   ALU, MOVD44, PBSAD, PBSADA_RaRb, MUL, MAC, DIV, MMU
;;     Require operands at EX.
;;   ALU_SHIFT_Rb
;;     An ALU-SHIFT instruction consists of a shift micro-operation followed
;;     by an arithmetic micro-operation. The operand Rb is used by the first
;;     micro-operation, and there are some latencies if data dependency occurs.
;;   MAC_RaRb
;;     A MAC instruction does multiplication at EX and does accumulation at MM,
;;     so the operand Rt is required at MM, and operands Ra and Rb are required
;;     at EX.
;;   ADDR_IN
;;     If an instruction requires an address as its input operand, the address
;;     is required at EX.
;;   ST
;;     A store instruction requires its data at MM.
;;   SMW(N, M)
;;     There are N micro-operations within an instruction that stores multiple
;;     words. Each M-th micro-operation requires its data at MM.
;;   BR
;;     If a branch instruction is conditional, its input data is required at EX.

;; LD, MUL, MAC, DIV
;;   -> ALU, ALU_SHIFT_Rb, PBSAD, PBSADA_RaRb, MOVD44, MUL, MAC_RaRb, DIV, ADDR_IN, BR, MMU
(define_bypass 2
  "nds_n9_3r2w_load,\
   nds_n9_3r2w_mul_fast1, nds_n9_3r2w_mul_fast2, nds_n9_3r2w_mul_slow,\
   nds_n9_3r2w_mac_fast1, nds_n9_3r2w_mac_fast2, nds_n9_3r2w_mac_slow,\
   nds_n9_3r2w_div"
  "nds_n9_3r2w_alu, nds_n9_3r2w_alu_shift,\
   nds_n9_3r2w_pbsad, nds_n9_3r2w_pbsada,\
   nds_n9_3r2w_mul_fast1, nds_n9_3r2w_mul_fast2, nds_n9_3r2w_mul_slow,\
   nds_n9_3r2w_mac_fast1, nds_n9_3r2w_mac_fast2, nds_n9_3r2w_mac_slow,\
   nds_n9_3r2w_branch,\
   nds_n9_3r2w_div,\
   nds_n9_3r2w_load,nds_n9_3r2w_store,\
   nds_n9_3r2w_load_multiple_1,nds_n9_3r2w_load_multiple_2, nds_n9_3r2w_load_multiple_3,\
   nds_n9_3r2w_load_multiple_4,nds_n9_3r2w_load_multiple_5, nds_n9_3r2w_load_multiple_6,\
   nds_n9_3r2w_load_multiple_7,nds_n9_3r2w_load_multiple_8, nds_n9_3r2w_load_multiple_12,\
   nds_n9_3r2w_store_multiple_1,nds_n9_3r2w_store_multiple_2, nds_n9_3r2w_store_multiple_3,\
   nds_n9_3r2w_store_multiple_4,nds_n9_3r2w_store_multiple_5, nds_n9_3r2w_store_multiple_6,\
   nds_n9_3r2w_store_multiple_7,nds_n9_3r2w_store_multiple_8, nds_n9_3r2w_store_multiple_12,\
   nds_n9_3r2w_mmu"
  "nds32_n9_3r2w_mm_to_ex_p"
)

;; LMW(N, N)
;;   -> ALU, ALU_SHIFT_Rb, PBSAD, PBSADA_RaRb, MOVD44, MUL, MAC_RaRb, DIV, ADDR_IN, BR, MMU
(define_bypass 2
  "nds_n9_3r2w_load_multiple_1,nds_n9_3r2w_load_multiple_2, nds_n9_3r2w_load_multiple_3,\
   nds_n9_3r2w_load_multiple_4,nds_n9_3r2w_load_multiple_5, nds_n9_3r2w_load_multiple_6,\
   nds_n9_3r2w_load_multiple_7,nds_n9_3r2w_load_multiple_8, nds_n9_3r2w_load_multiple_12"
  "nds_n9_3r2w_alu, nds_n9_3r2w_alu_shift,\
   nds_n9_3r2w_pbsad, nds_n9_3r2w_pbsada,\
   nds_n9_3r2w_mul_fast1, nds_n9_3r2w_mul_fast2, nds_n9_3r2w_mul_slow,\
   nds_n9_3r2w_mac_fast1, nds_n9_3r2w_mac_fast2, nds_n9_3r2w_mac_slow,\
   nds_n9_3r2w_branch,\
   nds_n9_3r2w_div,\
   nds_n9_3r2w_load,nds_n9_3r2w_store,\
   nds_n9_3r2w_load_multiple_1,nds_n9_3r2w_load_multiple_2, nds_n9_3r2w_load_multiple_3,\
   nds_n9_3r2w_load_multiple_4,nds_n9_3r2w_load_multiple_5, nds_n9_3r2w_load_multiple_6,\
   nds_n9_3r2w_load_multiple_7,nds_n9_3r2w_load_multiple_8, nds_n9_3r2w_load_multiple_12,\
   nds_n9_3r2w_store_multiple_1,nds_n9_3r2w_store_multiple_2, nds_n9_3r2w_store_multiple_3,\
   nds_n9_3r2w_store_multiple_4,nds_n9_3r2w_store_multiple_5, nds_n9_3r2w_store_multiple_6,\
   nds_n9_3r2w_store_multiple_7,nds_n9_3r2w_store_multiple_8, nds_n9_3r2w_store_multiple_12,\
   nds_n9_3r2w_mmu"
  "nds32_n9_last_load_to_ex_p"
)
