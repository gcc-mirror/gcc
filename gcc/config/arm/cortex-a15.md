;; ARM Cortex-A15 pipeline description
;; Copyright (C) 2011-2013 Free Software Foundation, Inc.
;;
;; Written by Matthew Gretton-Dann <matthew.gretton-dann@arm.com>

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

(define_automaton "cortex_a15")

;; The Cortex-A15 core is modelled as a triple issue pipeline that has
;; the following dispatch units.
;; 1. Two pipelines for simple integer operations: SX1, SX2
;; 2. Individual units for Neon and FP operations as in cortex-a15-neon.md
;; 3. One pipeline for branch operations: BX
;; 4. One pipeline for integer multiply and divide operations: MX
;; 5. Two pipelines for load and store operations: LS1, LS2
;;
;; We can issue into three pipelines per-cycle.
;;
;; We assume that where we have unit pairs xx1 is always filled before xx2.

;; The three issue units
(define_cpu_unit "ca15_i0, ca15_i1, ca15_i2" "cortex_a15")

(define_reservation "ca15_issue1" "(ca15_i0|ca15_i1|ca15_i2)")
(define_reservation "ca15_issue2" "((ca15_i0+ca15_i1)|(ca15_i1+ca15_i2))")
(define_reservation "ca15_issue3" "(ca15_i0+ca15_i1+ca15_i2)")
(final_presence_set "ca15_i1" "ca15_i0")
(final_presence_set "ca15_i2" "ca15_i1")

;; The main dispatch units
(define_cpu_unit "ca15_sx1, ca15_sx2" "cortex_a15")
(define_cpu_unit "ca15_ls1, ca15_ls2" "cortex_a15")
(define_cpu_unit "ca15_bx, ca15_mx" "cortex_a15")

(define_reservation "ca15_ls" "(ca15_ls1|ca15_ls2)")

;; The extended load-store pipeline
(define_cpu_unit "ca15_ldr, ca15_str" "cortex_a15")

;; The extended ALU pipeline
(define_cpu_unit "ca15_sx1_alu, ca15_sx1_shf, ca15_sx1_sat" "cortex_a15")
(define_cpu_unit "ca15_sx2_alu, ca15_sx2_shf, ca15_sx2_sat" "cortex_a15")

;; Simple Execution Unit:
;;
;; Simple ALU without shift
(define_insn_reservation "cortex_a15_alu" 2
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "alu_imm,alus_imm,logic_imm,logics_imm,\
                        alu_reg,alus_reg,logic_reg,logics_reg,\
                        adc_imm,adcs_imm,adc_reg,adcs_reg,\
                        adr,bfm,rev,\
                        shift_imm,shift_reg,\
                        mov_imm,mov_reg,\
                        mvn_imm,mvn_reg,\
                        mrs,multiple,no_insn"))
  "ca15_issue1,(ca15_sx1,ca15_sx1_alu)|(ca15_sx2,ca15_sx2_alu)")

;; ALU ops with immediate shift
(define_insn_reservation "cortex_a15_alu_shift" 3
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "extend,\
                        alu_shift_imm,alus_shift_imm,\
                        logic_shift_imm,logics_shift_imm,\
                        mov_shift,mvn_shift"))
  "ca15_issue1,(ca15_sx1,ca15_sx1+ca15_sx1_shf,ca15_sx1_alu)\
	       |(ca15_sx2,ca15_sx2+ca15_sx2_shf,ca15_sx2_alu)")

;; ALU ops with register controlled shift
(define_insn_reservation "cortex_a15_alu_shift_reg" 3
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "alu_shift_reg,alus_shift_reg,\
                        logic_shift_reg,logics_shift_reg,\
                        mov_shift_reg,mvn_shift_reg"))
  "(ca15_issue2,ca15_sx1+ca15_sx2,ca15_sx1_shf,ca15_sx2_alu)\
   |(ca15_issue1,(ca15_issue1+ca15_sx2,ca15_sx1+ca15_sx2_shf)\
   |(ca15_issue1+ca15_sx1,ca15_sx1+ca15_sx1_shf),ca15_sx1_alu)")

;; Multiply Execution Unit:
;;
;; 32-bit multiplies
(define_insn_reservation "cortex_a15_mult32" 3
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "mul32" "yes"))
  "ca15_issue1,ca15_mx")

;; 64-bit multiplies
(define_insn_reservation "cortex_a15_mult64" 4
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "mul64" "yes"))
  "ca15_issue1,ca15_mx*2")

;; Integer divide
(define_insn_reservation "cortex_a15_udiv" 9
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "udiv"))
  "ca15_issue1,ca15_mx")

(define_insn_reservation "cortex_a15_sdiv" 10
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "sdiv"))
  "ca15_issue1,ca15_mx")

;; Block all issue pipes for a cycle
(define_insn_reservation "cortex_a15_block" 1
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "block"))
  "ca15_issue3")

;; Branch execution Unit
;;
;; Branches take one issue slot.
;; No latency as there is no result
(define_insn_reservation "cortex_a15_branch" 0
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "branch"))
  "ca15_issue1,ca15_bx")

;; Load-store execution Unit
;;
;; Loads of up to two words.
(define_insn_reservation "cortex_a15_load1" 4
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "load_byte,load1,load2"))
  "ca15_issue1,ca15_ls,ca15_ldr,nothing")

;; Loads of three or four words.
(define_insn_reservation "cortex_a15_load3" 5
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "load3,load4"))
  "ca15_issue2,ca15_ls1+ca15_ls2,ca15_ldr,ca15_ldr,nothing")

;; Stores of up to two words.
(define_insn_reservation "cortex_a15_store1" 0
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "store1,store2"))
  "ca15_issue1,ca15_ls,ca15_str")

;; Stores of three or four words.
(define_insn_reservation "cortex_a15_store3" 0
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "store3,store4"))
  "ca15_issue2,ca15_ls1+ca15_ls2,ca15_str,ca15_str")

;; We include Neon.md here to ensure that the branch can block the Neon units.
(include "../arm/cortex-a15-neon.md")

;; We lie with calls.  They take up all issue slots, and form a block in the
;; pipeline.  The result however is available the next cycle.
(define_insn_reservation "cortex_a15_call" 1
  (and (eq_attr "tune" "cortexa15")
       (eq_attr "type" "call"))
  "ca15_issue3,\
   ca15_sx1+ca15_sx2+ca15_bx+ca15_mx+ca15_cx_ij+ca15_cx_ik+ca15_ls1+ca15_ls2+\
   ca15_cx_imac1+ca15_cx_ialu1+ca15_cx_ialu2+ca15_cx_ishf+\
   ca15_cx_acc+ca15_cx_fmul1+ca15_cx_fmul2+ca15_cx_fmul3+ca15_cx_fmul4+\
   ca15_cx_falu1+ca15_cx_falu2+ca15_cx_falu3+ca15_cx_falu4+ca15_cx_vfp_i,\
   ca15_sx1_alu+ca15_sx1_shf+ca15_sx1_sat+ca15_sx2_alu+\
   ca15_sx2_shf+ca15_sx2_sat+ca15_ldr+ca15_str")

;; Simple execution unit bypasses
(define_bypass 1 "cortex_a15_alu"
	       "cortex_a15_alu,cortex_a15_alu_shift,cortex_a15_alu_shift_reg")
(define_bypass 2 "cortex_a15_alu_shift"
	       "cortex_a15_alu,cortex_a15_alu_shift,cortex_a15_alu_shift_reg")
(define_bypass 2 "cortex_a15_alu_shift_reg"
	       "cortex_a15_alu,cortex_a15_alu_shift,cortex_a15_alu_shift_reg")
(define_bypass 1 "cortex_a15_alu" "cortex_a15_load1,cortex_a15_load3")
(define_bypass 2 "cortex_a15_alu_shift" "cortex_a15_load1,cortex_a15_load3")
(define_bypass 2 "cortex_a15_alu_shift_reg"
	       "cortex_a15_load1,cortex_a15_load3")
