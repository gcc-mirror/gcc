;; Copyright (C) 2012-2013 Free Software Foundation, Inc.
;;
;; Contributed by ARM Ltd.
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

;; In the absence of any ARMv8-A implementations, two examples derived
;; from ARM's most recent ARMv7-A cores (Cortex-A7 and Cortex-A15) are
;; included by way of example.  This is a temporary measure.

;; Example pipeline description for an example 'large' core
;; implementing AArch64

;;-------------------------------------------------------
;; General Description
;;-------------------------------------------------------

(define_automaton "large_cpu")

;; The core is modelled as a triple issue pipeline that has
;; the following dispatch units.
;; 1. Two pipelines for simple integer operations: int1, int2
;; 2. Two pipelines for SIMD and FP data-processing operations: fpsimd1, fpsimd2
;; 3. One pipeline for branch operations: br
;; 4. One pipeline for integer multiply and divide operations: multdiv
;; 5. Two pipelines for load and store operations: ls1, ls2
;;
;; We can issue into three pipelines per-cycle.
;;
;; We assume that where we have unit pairs xxx1 is always filled before xxx2.

;;-------------------------------------------------------
;; CPU Units and Reservations
;;-------------------------------------------------------

;; The three issue units
(define_cpu_unit "large_cpu_unit_i1, large_cpu_unit_i2, large_cpu_unit_i3" "large_cpu")

(define_reservation "large_cpu_resv_i1"
		    "(large_cpu_unit_i1 | large_cpu_unit_i2 | large_cpu_unit_i3)")

(define_reservation "large_cpu_resv_i2"
		    "((large_cpu_unit_i1 + large_cpu_unit_i2) | (large_cpu_unit_i2 + large_cpu_unit_i3))")

(define_reservation "large_cpu_resv_i3"
		    "(large_cpu_unit_i1 + large_cpu_unit_i2 + large_cpu_unit_i3)")

(final_presence_set "large_cpu_unit_i2" "large_cpu_unit_i1")
(final_presence_set "large_cpu_unit_i3" "large_cpu_unit_i2")

;; The main dispatch units
(define_cpu_unit "large_cpu_unit_int1, large_cpu_unit_int2" "large_cpu")
(define_cpu_unit "large_cpu_unit_fpsimd1, large_cpu_unit_fpsimd2" "large_cpu")
(define_cpu_unit "large_cpu_unit_ls1, large_cpu_unit_ls2" "large_cpu")
(define_cpu_unit "large_cpu_unit_br" "large_cpu")
(define_cpu_unit "large_cpu_unit_multdiv" "large_cpu")

(define_reservation "large_cpu_resv_ls" "(large_cpu_unit_ls1 | large_cpu_unit_ls2)")

;; The extended load-store pipeline
(define_cpu_unit "large_cpu_unit_load, large_cpu_unit_store" "large_cpu")

;; The extended ALU pipeline
(define_cpu_unit "large_cpu_unit_int1_alu, large_cpu_unit_int2_alu" "large_cpu")
(define_cpu_unit "large_cpu_unit_int1_shf, large_cpu_unit_int2_shf" "large_cpu")
(define_cpu_unit "large_cpu_unit_int1_sat, large_cpu_unit_int2_sat" "large_cpu")


;;-------------------------------------------------------
;; Simple ALU Instructions
;;-------------------------------------------------------

;; Simple ALU operations without shift
(define_insn_reservation "large_cpu_alu" 2
  (and (eq_attr "tune" "large") (eq_attr "v8type" "adc,alu,alu_ext"))
  "large_cpu_resv_i1, \
   (large_cpu_unit_int1, large_cpu_unit_int1_alu) |\
     (large_cpu_unit_int2, large_cpu_unit_int2_alu)")

(define_insn_reservation "large_cpu_logic" 2
  (and (eq_attr "tune" "large") (eq_attr "v8type" "logic,logic_imm"))
  "large_cpu_resv_i1, \
   (large_cpu_unit_int1, large_cpu_unit_int1_alu) |\
     (large_cpu_unit_int2, large_cpu_unit_int2_alu)")

(define_insn_reservation "large_cpu_shift" 2
  (and (eq_attr "tune" "large") (eq_attr "v8type" "shift,shift_imm"))
  "large_cpu_resv_i1, \
   (large_cpu_unit_int1, large_cpu_unit_int1_shf) |\
     (large_cpu_unit_int2, large_cpu_unit_int2_shf)")

;; Simple ALU operations with immediate shift
(define_insn_reservation "large_cpu_alu_shift" 3
  (and (eq_attr "tune" "large") (eq_attr "v8type" "alu_shift"))
  "large_cpu_resv_i1, \
   (large_cpu_unit_int1,
     large_cpu_unit_int1 + large_cpu_unit_int1_shf, large_cpu_unit_int1_alu) | \
   (large_cpu_unit_int2,
     large_cpu_unit_int2 + large_cpu_unit_int2_shf, large_cpu_unit_int2_alu)")

(define_insn_reservation "large_cpu_logic_shift" 3
  (and (eq_attr "tune" "large") (eq_attr "v8type" "logic_shift"))
  "large_cpu_resv_i1, \
   (large_cpu_unit_int1, large_cpu_unit_int1_alu) |\
     (large_cpu_unit_int2, large_cpu_unit_int2_alu)")


;;-------------------------------------------------------
;; Multiplication/Division
;;-------------------------------------------------------

;; Simple multiplication
(define_insn_reservation "large_cpu_mult_single" 3
  (and (eq_attr "tune" "large")
       (and (eq_attr "v8type" "mult,madd") (eq_attr "mode" "SI")))
  "large_cpu_resv_i1, large_cpu_unit_multdiv")

(define_insn_reservation "large_cpu_mult_double" 4
  (and (eq_attr "tune" "large")
       (and (eq_attr "v8type" "mult,madd") (eq_attr "mode" "DI")))
  "large_cpu_resv_i1, large_cpu_unit_multdiv")

;; 64-bit multiplication
(define_insn_reservation "large_cpu_mull" 4
  (and (eq_attr "tune" "large") (eq_attr "v8type" "mull,mulh,maddl"))
  "large_cpu_resv_i1, large_cpu_unit_multdiv * 2")

;; Division
(define_insn_reservation "large_cpu_udiv_single" 9
  (and (eq_attr "tune" "large")
       (and (eq_attr "v8type" "udiv") (eq_attr "mode" "SI")))
  "large_cpu_resv_i1, large_cpu_unit_multdiv")

(define_insn_reservation "large_cpu_udiv_double" 18
  (and (eq_attr "tune" "large")
       (and (eq_attr "v8type" "udiv") (eq_attr "mode" "DI")))
  "large_cpu_resv_i1, large_cpu_unit_multdiv")

(define_insn_reservation "large_cpu_sdiv_single" 10
  (and (eq_attr "tune" "large")
       (and (eq_attr "v8type" "sdiv") (eq_attr "mode" "SI")))
  "large_cpu_resv_i1, large_cpu_unit_multdiv")

(define_insn_reservation "large_cpu_sdiv_double" 20
  (and (eq_attr "tune" "large")
       (and (eq_attr "v8type" "sdiv") (eq_attr "mode" "DI")))
  "large_cpu_resv_i1, large_cpu_unit_multdiv")


;;-------------------------------------------------------
;; Branches
;;-------------------------------------------------------

;; Branches take one issue slot.
;; No latency as there is no result
(define_insn_reservation "large_cpu_branch" 0
  (and (eq_attr "tune" "large") (eq_attr "v8type" "branch"))
  "large_cpu_resv_i1, large_cpu_unit_br")


;; Calls take up all issue slots, and form a block in the
;; pipeline.  The result however is available the next cycle.
;; Addition of new units requires this to be updated.
(define_insn_reservation "large_cpu_call" 1
  (and (eq_attr "tune" "large") (eq_attr "v8type" "call"))
  "large_cpu_resv_i3 | large_cpu_resv_i2, \
   large_cpu_unit_int1 + large_cpu_unit_int2 + large_cpu_unit_br + \
     large_cpu_unit_multdiv + large_cpu_unit_fpsimd1 + large_cpu_unit_fpsimd2 + \
     large_cpu_unit_ls1 + large_cpu_unit_ls2,\
   large_cpu_unit_int1_alu + large_cpu_unit_int1_shf + large_cpu_unit_int1_sat + \
     large_cpu_unit_int2_alu + large_cpu_unit_int2_shf + \
     large_cpu_unit_int2_sat + large_cpu_unit_load + large_cpu_unit_store")


;;-------------------------------------------------------
;; Load/Store Instructions
;;-------------------------------------------------------

;; Loads of up to two words.
(define_insn_reservation "large_cpu_load1" 4
  (and (eq_attr "tune" "large") (eq_attr "v8type" "load_acq,load1,load2"))
  "large_cpu_resv_i1, large_cpu_resv_ls, large_cpu_unit_load, nothing")

;; Stores of up to two words.
(define_insn_reservation "large_cpu_store1" 0
  (and (eq_attr "tune" "large") (eq_attr "v8type" "store_rel,store1,store2"))
  "large_cpu_resv_i1, large_cpu_resv_ls, large_cpu_unit_store")


;;-------------------------------------------------------
;; Floating-point arithmetic.
;;-------------------------------------------------------

(define_insn_reservation "large_cpu_fpalu" 4
  (and (eq_attr "tune" "large")
       (eq_attr "v8type" "ffarith,fadd,fccmp,fcvt,fcmp"))
  "large_cpu_resv_i1 + large_cpu_unit_fpsimd1")

(define_insn_reservation "large_cpu_fconst" 3
  (and (eq_attr "tune" "large")
       (eq_attr "v8type" "fconst"))
  "large_cpu_resv_i1 + large_cpu_unit_fpsimd1")

(define_insn_reservation "large_cpu_fpmuls" 4
  (and (eq_attr "tune" "large")
       (and (eq_attr "v8type" "fmul,fmadd") (eq_attr "mode" "SF")))
  "large_cpu_resv_i1 + large_cpu_unit_fpsimd1")

(define_insn_reservation "large_cpu_fpmuld" 7
  (and (eq_attr "tune" "large")
       (and (eq_attr "v8type" "fmul,fmadd") (eq_attr "mode" "DF")))
  "large_cpu_resv_i1 + large_cpu_unit_fpsimd1, large_cpu_unit_fpsimd1 * 2,\
   large_cpu_resv_i1 + large_cpu_unit_fpsimd1")


;;-------------------------------------------------------
;; Floating-point Division
;;-------------------------------------------------------

;; Single-precision divide takes 14 cycles to complete, and this
;; includes the time taken for the special instruction used to collect the
;; result to travel down the multiply pipeline.

(define_insn_reservation "large_cpu_fdivs" 14
  (and (eq_attr "tune" "large")
       (and (eq_attr "v8type" "fdiv,fsqrt") (eq_attr "mode" "SF")))
  "large_cpu_resv_i1, large_cpu_unit_fpsimd1 * 13")

(define_insn_reservation "large_cpu_fdivd" 29
  (and (eq_attr "tune" "large")
       (and (eq_attr "v8type" "fdiv,fsqrt") (eq_attr "mode" "DF")))
  "large_cpu_resv_i1, large_cpu_unit_fpsimd1 * 28")



;;-------------------------------------------------------
;; Floating-point Transfers
;;-------------------------------------------------------

(define_insn_reservation "large_cpu_i2f" 4
  (and (eq_attr "tune" "large")
       (eq_attr "v8type" "fmovi2f"))
  "large_cpu_resv_i1")

(define_insn_reservation "large_cpu_f2i" 2
  (and (eq_attr "tune" "large")
       (eq_attr "v8type" "fmovf2i"))
  "large_cpu_resv_i1")


;;-------------------------------------------------------
;; Floating-point Load/Store
;;-------------------------------------------------------

(define_insn_reservation "large_cpu_floads" 4
  (and (eq_attr "tune" "large")
       (and (eq_attr "v8type" "fpsimd_load,fpsimd_load2") (eq_attr "mode" "SF")))
  "large_cpu_resv_i1")

(define_insn_reservation "large_cpu_floadd" 5
  (and (eq_attr "tune" "large")
       (and (eq_attr "v8type" "fpsimd_load,fpsimd_load2") (eq_attr "mode" "DF")))
  "large_cpu_resv_i1 + large_cpu_unit_br, large_cpu_resv_i1")

(define_insn_reservation "large_cpu_fstores" 0
  (and (eq_attr "tune" "large")
       (and (eq_attr "v8type" "fpsimd_store,fpsimd_store2") (eq_attr "mode" "SF")))
  "large_cpu_resv_i1")

(define_insn_reservation "large_cpu_fstored" 0
  (and (eq_attr "tune" "large")
       (and (eq_attr "v8type" "fpsimd_store,fpsimd_store2") (eq_attr "mode" "DF")))
  "large_cpu_resv_i1 + large_cpu_unit_br, large_cpu_resv_i1")


;;-------------------------------------------------------
;; Bypasses
;;-------------------------------------------------------

(define_bypass 1 "large_cpu_alu, large_cpu_logic, large_cpu_shift"
  "large_cpu_alu, large_cpu_alu_shift, large_cpu_logic, large_cpu_logic_shift, large_cpu_shift")

(define_bypass 2 "large_cpu_alu_shift, large_cpu_logic_shift"
  "large_cpu_alu, large_cpu_alu_shift, large_cpu_logic, large_cpu_logic_shift, large_cpu_shift")

(define_bypass 1 "large_cpu_alu, large_cpu_logic, large_cpu_shift" "large_cpu_load1")

(define_bypass 2 "large_cpu_alu_shift, large_cpu_logic_shift" "large_cpu_load1")

(define_bypass 2 "large_cpu_floads"
                 "large_cpu_fpalu, large_cpu_fpmuld,\
		  large_cpu_fdivs, large_cpu_fdivd,\
		  large_cpu_f2i")

(define_bypass 3 "large_cpu_floadd"
                 "large_cpu_fpalu, large_cpu_fpmuld,\
		  large_cpu_fdivs, large_cpu_fdivd,\
		  large_cpu_f2i")
