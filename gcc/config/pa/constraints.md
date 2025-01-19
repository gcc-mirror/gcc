;; Constraint definitions for pa
;; Copyright (C) 2007-2025 Free Software Foundation, Inc.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;;; Unused letters:
;;;    ABCD   H                Y 
;;;     bcde  h jkl       tuvw  z

;; Register constraints.
(define_register_constraint "a" "R1_REGS"
  "General register 1.")

(define_register_constraint "f" "FP_REGS"
  "Floating-point register.")

(define_register_constraint "q" "SHIFT_REGS"
  "Shift amount register.")

;; Keep 'x' for backward compatibility with user asm.
(define_register_constraint "x" "FP_REGS"
  "Floating-point register.")

(define_register_constraint "y" "TARGET_64BIT ? FP_REGS : FPUPPER_REGS"
  "Upper floating-point register.")

(define_register_constraint "Z" "ALL_REGS"
  "Any register.")

;; Integer constant constraints.
(define_constraint "I"
  "Signed 11-bit integer constant."
  (and (match_code "const_int")
       (match_test "VAL_11_BITS_P (ival)")))

(define_constraint "J"
  "Signed 14-bit integer constant."
  (and (match_code "const_int")
       (match_test "VAL_14_BITS_P (ival)")))

(define_constraint "K"
  "Integer constant that can be deposited with a zdepi instruction."
  (and (match_code "const_int")
       (match_test "pa_zdepi_cint_p (ival)")))

(define_constraint "L"
  "Signed 5-bit integer constant."
  (and (match_code "const_int")
       (match_test "VAL_5_BITS_P (ival)")))

(define_constraint "M"
  "Integer constant 0."
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "N"
  "Integer constant that can be loaded with a ldil instruction."
  (and (match_code "const_int")
       (match_test "pa_ldil_cint_p (ival)")))

(define_constraint "O"
  "Integer constant such that ival+1 is a power of 2."
  (and (match_code "const_int")
       (match_test "(ival & (ival + 1)) == 0")))

(define_constraint "P"
  "Integer constant that can be used as an and mask in depi and
   extru instructions."
  (and (match_code "const_int")
       (match_test "pa_and_mask_p (ival)")))

(define_constraint "S"
  "Integer constant 31."
  (and (match_code "const_int")
       (match_test "ival == 31")))

(define_constraint "U"
  "Integer constant 63."
  (and (match_code "const_int")
       (match_test "ival == 63")))

;; Floating-point constant constraints.
(define_constraint "G"
  "Floating-point constant 0."
  (and (match_code "const_double")
       (match_test "GET_MODE_CLASS (mode) == MODE_FLOAT
		    && op == CONST0_RTX (mode)")))

;; Extra constraints.
(define_constraint "A"
  "A LO_SUM DLT memory operand."
  (and (match_code "mem")
       (match_test "IS_LO_SUM_DLT_ADDR_P (XEXP (op, 0))")))

(define_constraint "Q"
  "A memory operand that can be used as the destination operand of an
   integer store, or the source operand of an integer load.  That is
   any memory operand that isn't a symbolic, indexed or lo_sum memory
   operand.  Note that an unassigned pseudo register is such a memory
   operand.  We accept unassigned pseudo registers because reload
   generates them and then doesn't re-recognize the insn, causing
   constrain_operands to fail."
  (match_test "integer_store_memory_operand (op, mode)"))

(define_constraint "R"
  "A scaled or unscaled indexed memory operand that can be used as the
   source address in integer and floating-point loads."
  (and (match_code "mem")
       (match_test "IS_INDEX_ADDR_P (XEXP (op, 0))")))

(define_constraint "T"
  "A memory operand for floating-point loads and stores."
  (match_test "floating_point_store_memory_operand (op, mode)"))

;; We could allow short displacements but TARGET_LEGITIMATE_ADDRESS_P
;; can't tell when a long displacement is valid.
(define_constraint "W"
  "A register indirect memory operand."
  (and (match_code "mem")
       (match_test "REG_P (XEXP (op, 0))
		    && REG_OK_FOR_BASE_P (XEXP (op, 0))")))
