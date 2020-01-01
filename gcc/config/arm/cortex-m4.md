;; ARM Cortex-M4 pipeline description
;; Copyright (C) 2010-2020 Free Software Foundation, Inc.
;; Contributed by CodeSourcery.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_automaton "cortex_m4")

;; We model the pipelining of LDR instructions by using two artificial units.

(define_cpu_unit "cortex_m4_a" "cortex_m4")

(define_cpu_unit "cortex_m4_b" "cortex_m4")

(define_reservation "cortex_m4_ex" "cortex_m4_a+cortex_m4_b")

;; ALU and multiply is one cycle.
(define_insn_reservation "cortex_m4_alu" 1
  (and (eq_attr "tune" "cortexm4")
       (ior (eq_attr "type" "alu_imm,alus_imm,logic_imm,logics_imm,\
                             alu_sreg,alus_sreg,logic_reg,logics_reg,\
                             adc_imm,adcs_imm,adc_reg,adcs_reg,\
                             adr,bfm,clz,rbit,rev,alu_dsp_reg,\
                             shift_imm,shift_reg,extend,\
                             alu_shift_imm,alus_shift_imm,\
                             logic_shift_imm,logics_shift_imm,\
                             alu_shift_reg,alus_shift_reg,\
                             logic_shift_reg,logics_shift_reg,\
                             mov_imm,mov_reg,mov_shift,mov_shift_reg,\
                             mvn_imm,mvn_reg,mvn_shift,mvn_shift_reg,\
                             mrs,multiple")
	    (ior (eq_attr "mul32" "yes")
		 (eq_attr "widen_mul64" "yes"))))
  "cortex_m4_ex")

;; Byte, half-word and word load is two cycles.
(define_insn_reservation "cortex_m4_load1" 2
  (and (eq_attr "tune" "cortexm4")
       (eq_attr "type" "load_byte,load_4"))
  "cortex_m4_a, cortex_m4_b")

;; str rx, [ry, #imm] is always one cycle.
(define_insn_reservation "cortex_m4_store1_1" 1
  (and (and (eq_attr "tune" "cortexm4")
	    (eq_attr "type" "store_4"))
       (match_test "arm_address_offset_is_imm (insn)"))
  "cortex_m4_a")

;; Other byte, half-word and word load is two cycles.
(define_insn_reservation "cortex_m4_store1_2" 2
  (and (and (eq_attr "tune" "cortexm4")
	    (eq_attr "type" "store_4"))
       (not (match_test "arm_address_offset_is_imm (insn)")))
  "cortex_m4_a*2")

(define_insn_reservation "cortex_m4_load2" 3
  (and (eq_attr "tune" "cortexm4")
       (eq_attr "type" "load_8"))
  "cortex_m4_ex*3")

(define_insn_reservation "cortex_m4_store2" 3
  (and (eq_attr "tune" "cortexm4")
       (eq_attr "type" "store_8"))
  "cortex_m4_ex*3")

(define_insn_reservation "cortex_m4_load3" 4
  (and (eq_attr "tune" "cortexm4")
       (eq_attr "type" "load_12"))
  "cortex_m4_ex*4")

(define_insn_reservation "cortex_m4_store3" 4
  (and (eq_attr "tune" "cortexm4")
       (eq_attr "type" "store_12"))
  "cortex_m4_ex*4")

(define_insn_reservation "cortex_m4_load4" 5
  (and (eq_attr "tune" "cortexm4")
       (eq_attr "type" "load_16"))
  "cortex_m4_ex*5")

(define_insn_reservation "cortex_m4_store4" 5
  (and (eq_attr "tune" "cortexm4")
       (eq_attr "type" "store_16"))
  "cortex_m4_ex*5")

(define_bypass 1 "cortex_m4_load1"
                 "cortex_m4_store1_1,cortex_m4_store1_2"
                 "arm_no_early_store_addr_dep")

;; If the address of load or store depends on the result of the preceding
;; instruction, the latency is increased by one.

(define_bypass 2 "cortex_m4_alu"
		 "cortex_m4_load1"
		 "arm_early_load_addr_dep")

(define_bypass 2 "cortex_m4_alu"
		 "cortex_m4_store1_1,cortex_m4_store1_2"
		 "arm_early_store_addr_dep")

(define_insn_reservation "cortex_m4_branch" 3
  (and (eq_attr "tune" "cortexm4")
       (eq_attr "type" "branch"))
  "cortex_m4_ex*3")

(define_insn_reservation "cortex_m4_call" 3
  (and (eq_attr "tune" "cortexm4")
       (eq_attr "type" "call"))
  "cortex_m4_ex*3")

(define_insn_reservation "cortex_m4_block" 1
  (and (eq_attr "tune" "cortexm4")
       (eq_attr "type" "block"))
  "cortex_m4_ex")
