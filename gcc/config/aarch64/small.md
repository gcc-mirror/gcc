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

;; Example pipeline description for an example 'small' core
;; implementing AArch64

;;-------------------------------------------------------
;; General Description
;;-------------------------------------------------------

(define_automaton "small_cpu")

;; The core is modelled as a single issue pipeline with the following
;; dispatch units.
;; 1. One pipeline for simple intructions.
;; 2. One pipeline for branch intructions.
;;
;; There are five pipeline stages.
;; The decode/issue stages operate the same for all instructions.
;; Instructions always advance one stage per cycle in order.
;; Only branch instructions may dual-issue with other instructions, except
;; when those instructions take multiple cycles to issue.


;;-------------------------------------------------------
;; CPU Units and Reservations
;;-------------------------------------------------------

(define_cpu_unit "small_cpu_unit_i" "small_cpu")
(define_cpu_unit "small_cpu_unit_br" "small_cpu")

;; Pseudo-unit for blocking the multiply pipeline when a double-precision
;; multiply is in progress.
(define_cpu_unit "small_cpu_unit_fpmul_pipe" "small_cpu")

;; The floating-point add pipeline, used to model the usage
;; of the add pipeline by fp alu instructions.
(define_cpu_unit "small_cpu_unit_fpadd_pipe" "small_cpu")

;; Floating-point division pipeline (long latency, out-of-order completion).
(define_cpu_unit "small_cpu_unit_fpdiv" "small_cpu")


;;-------------------------------------------------------
;; Simple ALU Instructions
;;-------------------------------------------------------

;; Simple ALU operations without shift
(define_insn_reservation "small_cpu_alu" 2
  (and (eq_attr "tune" "small")
       (eq_attr "v8type" "adc,alu,alu_ext"))
  "small_cpu_unit_i")

(define_insn_reservation "small_cpu_logic" 2
  (and (eq_attr "tune" "small")
       (eq_attr "v8type" "logic,logic_imm"))
  "small_cpu_unit_i")

(define_insn_reservation "small_cpu_shift" 2
  (and (eq_attr "tune" "small")
       (eq_attr "v8type" "shift,shift_imm"))
  "small_cpu_unit_i")

;; Simple ALU operations with immediate shift
(define_insn_reservation "small_cpu_alu_shift" 2
  (and (eq_attr "tune" "small")
       (eq_attr "v8type" "alu_shift"))
  "small_cpu_unit_i")

(define_insn_reservation "small_cpu_logic_shift" 2
  (and (eq_attr "tune" "small")
       (eq_attr "v8type" "logic_shift"))
  "small_cpu_unit_i")


;;-------------------------------------------------------
;; Multiplication/Division
;;-------------------------------------------------------

;; Simple multiplication
(define_insn_reservation "small_cpu_mult_single" 2
  (and (eq_attr "tune" "small")
       (and (eq_attr "v8type" "mult,madd") (eq_attr "mode" "SI")))
  "small_cpu_unit_i")

(define_insn_reservation "small_cpu_mult_double" 3
  (and (eq_attr "tune" "small")
       (and (eq_attr "v8type" "mult,madd") (eq_attr "mode" "DI")))
  "small_cpu_unit_i")

;; 64-bit multiplication
(define_insn_reservation "small_cpu_mull" 3
  (and (eq_attr "tune" "small") (eq_attr "v8type" "mull,mulh,maddl"))
  "small_cpu_unit_i * 2")

;; Division
(define_insn_reservation "small_cpu_udiv_single" 5
  (and (eq_attr "tune" "small")
       (and (eq_attr "v8type" "udiv") (eq_attr "mode" "SI")))
  "small_cpu_unit_i")

(define_insn_reservation "small_cpu_udiv_double" 10
  (and (eq_attr "tune" "small")
       (and (eq_attr "v8type" "udiv") (eq_attr "mode" "DI")))
  "small_cpu_unit_i")

(define_insn_reservation "small_cpu_sdiv_single" 6
  (and (eq_attr "tune" "small")
       (and (eq_attr "v8type" "sdiv") (eq_attr "mode" "SI")))
  "small_cpu_unit_i")

(define_insn_reservation "small_cpu_sdiv_double" 12
  (and (eq_attr "tune" "small")
       (and (eq_attr "v8type" "sdiv") (eq_attr "mode" "DI")))
  "small_cpu_unit_i")


;;-------------------------------------------------------
;; Load/Store Instructions
;;-------------------------------------------------------

(define_insn_reservation "small_cpu_load1" 2
  (and (eq_attr "tune" "small")
       (eq_attr "v8type" "load_acq,load1"))
  "small_cpu_unit_i")

(define_insn_reservation "small_cpu_store1" 0
  (and (eq_attr "tune" "small")
       (eq_attr "v8type" "store_rel,store1"))
  "small_cpu_unit_i")

(define_insn_reservation "small_cpu_load2" 3
  (and (eq_attr "tune" "small")
       (eq_attr "v8type" "load2"))
  "small_cpu_unit_i + small_cpu_unit_br, small_cpu_unit_i")

(define_insn_reservation "small_cpu_store2" 0
  (and (eq_attr "tune" "small")
       (eq_attr "v8type" "store2"))
  "small_cpu_unit_i + small_cpu_unit_br, small_cpu_unit_i")


;;-------------------------------------------------------
;; Branches
;;-------------------------------------------------------

;; Direct branches are the only instructions that can dual-issue.
;; The latency here represents when the branch actually takes place.

(define_insn_reservation "small_cpu_unit_br" 3
  (and (eq_attr "tune" "small")
       (eq_attr "v8type" "branch,call"))
  "small_cpu_unit_br")


;;-------------------------------------------------------
;; Floating-point arithmetic.
;;-------------------------------------------------------

(define_insn_reservation "small_cpu_fpalu" 4
  (and (eq_attr "tune" "small")
       (eq_attr "v8type" "ffarith,fadd,fccmp,fcvt,fcmp"))
  "small_cpu_unit_i + small_cpu_unit_fpadd_pipe")

(define_insn_reservation "small_cpu_fconst" 3
  (and (eq_attr "tune" "small")
       (eq_attr "v8type" "fconst"))
  "small_cpu_unit_i + small_cpu_unit_fpadd_pipe")

(define_insn_reservation "small_cpu_fpmuls" 4
  (and (eq_attr "tune" "small")
       (and (eq_attr "v8type" "fmul") (eq_attr "mode" "SF")))
  "small_cpu_unit_i + small_cpu_unit_fpmul_pipe")

(define_insn_reservation "small_cpu_fpmuld" 7
  (and (eq_attr "tune" "small")
       (and (eq_attr "v8type" "fmul") (eq_attr "mode" "DF")))
  "small_cpu_unit_i + small_cpu_unit_fpmul_pipe, small_cpu_unit_fpmul_pipe * 2,\
   small_cpu_unit_i + small_cpu_unit_fpmul_pipe")


;;-------------------------------------------------------
;; Floating-point Division
;;-------------------------------------------------------

;; Single-precision divide takes 14 cycles to complete, and this
;; includes the time taken for the special instruction used to collect the
;; result to travel down the multiply pipeline.

(define_insn_reservation "small_cpu_fdivs" 14
  (and (eq_attr "tune" "small")
       (and (eq_attr "v8type" "fdiv,fsqrt") (eq_attr "mode" "SF")))
  "small_cpu_unit_i, small_cpu_unit_fpdiv * 13")

(define_insn_reservation "small_cpu_fdivd" 29
  (and (eq_attr "tune" "small")
       (and (eq_attr "v8type" "fdiv,fsqrt") (eq_attr "mode" "DF")))
  "small_cpu_unit_i, small_cpu_unit_fpdiv * 28")


;;-------------------------------------------------------
;; Floating-point Transfers
;;-------------------------------------------------------

(define_insn_reservation "small_cpu_i2f" 4
  (and (eq_attr "tune" "small")
       (eq_attr "v8type" "fmovi2f"))
  "small_cpu_unit_i")

(define_insn_reservation "small_cpu_f2i" 2
  (and (eq_attr "tune" "small")
       (eq_attr "v8type" "fmovf2i"))
  "small_cpu_unit_i")


;;-------------------------------------------------------
;; Floating-point Load/Store
;;-------------------------------------------------------

(define_insn_reservation "small_cpu_floads" 4
  (and (eq_attr "tune" "small")
       (and (eq_attr "v8type" "fpsimd_load") (eq_attr "mode" "SF")))
  "small_cpu_unit_i")

(define_insn_reservation "small_cpu_floadd" 5
  (and (eq_attr "tune" "small")
       (and (eq_attr "v8type" "fpsimd_load") (eq_attr "mode" "DF")))
  "small_cpu_unit_i + small_cpu_unit_br, small_cpu_unit_i")

(define_insn_reservation "small_cpu_fstores" 0
  (and (eq_attr "tune" "small")
       (and (eq_attr "v8type" "fpsimd_store") (eq_attr "mode" "SF")))
  "small_cpu_unit_i")

(define_insn_reservation "small_cpu_fstored" 0
  (and (eq_attr "tune" "small")
       (and (eq_attr "v8type" "fpsimd_store") (eq_attr "mode" "DF")))
  "small_cpu_unit_i + small_cpu_unit_br, small_cpu_unit_i")


;;-------------------------------------------------------
;; Bypasses
;;-------------------------------------------------------

;; Forwarding path for unshifted operands.

(define_bypass 1 "small_cpu_alu, small_cpu_alu_shift" 
  "small_cpu_alu, small_cpu_alu_shift, small_cpu_logic, small_cpu_logic_shift, small_cpu_shift")

(define_bypass 1 "small_cpu_logic, small_cpu_logic_shift" 
  "small_cpu_alu, small_cpu_alu_shift, small_cpu_logic, small_cpu_logic_shift, small_cpu_shift")

(define_bypass 1 "small_cpu_shift" 
  "small_cpu_alu, small_cpu_alu_shift, small_cpu_logic, small_cpu_logic_shift, small_cpu_shift")

;; Load-to-use for floating-point values has a penalty of one cycle.

(define_bypass 2 "small_cpu_floads"
                 "small_cpu_fpalu, small_cpu_fpmuld,\
		  small_cpu_fdivs, small_cpu_fdivd,\
		  small_cpu_f2i")

(define_bypass 3 "small_cpu_floadd"
                 "small_cpu_fpalu, small_cpu_fpmuld,\
		  small_cpu_fdivs, small_cpu_fdivd,\
		  small_cpu_f2i")
