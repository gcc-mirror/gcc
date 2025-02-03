;; Constraint definitions for Renesas H8/300.
;; Copyright (C) 2011-2025 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Register constraints.
(define_register_constraint "a" "MAC_REGS"
  "@internal")

(define_register_constraint "c" "COUNTER_REGS"
  "@internal")

;; Some patterns need to use er6 as a scratch register.  This is
;; difficult to arrange since er6 is the frame pointer and usually can't
;; be spilled.

;; Such patterns should define two alternatives, one which allows only
;; er6 and one which allows any general register.  The former
;; alternative should have a 'd' constraint while the latter should be
;; disparaged and use 'D'.

;; Normally, 'd' maps to DESTINATION_REGS and 'D' maps to GENERAL_REGS.
;; However, there are cases where they should be NO_REGS:

;;   - 'd' should be NO_REGS when reloading a function that uses the
;;     frame pointer.  In this case, DESTINATION_REGS won't contain any
;;     spillable registers, so the first alternative can't be used.

;;   - -fno-omit-frame-pointer means that the frame pointer will
;;     always be in use.  It's therefore better to map 'd' to NO_REGS
;;     before reload so that register allocator will pick the second
;;     alternative.

;;   - we would like 'D' to be NO_REGS when the frame pointer isn't
;;     live, but we the frame pointer may turn out to be needed after
;;     we start reload, and then we may have already decided we don't
;;     have a choice, so we can't do that.  Forcing the register
;;     allocator to use er6 if possible might produce better code for
;;     small functions: it's more efficient to save and restore er6 in
;;     the prologue & epilogue than to do it in a define_split.
;;     Hopefully disparaging 'D' will have a similar effect, without
;;     forcing a reload failure if the frame pointer is found to be
;;     needed too late.

(define_register_constraint "d"
  "(!flag_omit_frame_pointer && !reload_completed
    ? NO_REGS
    : (frame_pointer_needed && reload_in_progress
       ? NO_REGS
       : DESTINATION_REGS))"
  "@internal")

(define_register_constraint "D" "GENERAL_REGS"
  "@internal")

(define_register_constraint "f" "SOURCE_REGS"
  "@internal")

;; Integer constraints.
(define_constraint "I"
  "Integer zero."
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "J"
  "An integer with its low byte clear."
  (and (match_code "const_int")
       (match_test "(ival & 0xff) == 0")))

(define_constraint "L"
  "Integer 1, 2 or 4"
  (and (match_code "const_int")
       (match_test "ival == 1 || ival == 2 || ival == 4")))

(define_constraint "M"
  "Integer 1 or 2."
  (and (match_code "const_int")
       (match_test "ival == 1 || ival == 2")))

(define_constraint "N"
  "Integer -1, -2 or -4"
  (and (match_code "const_int")
       (match_test "ival == -1 || ival == -2 || ival == -4")))

(define_constraint "O"
  "Integer -1 or -2."
  (and (match_code "const_int")
       (match_test "ival == -1 || ival == -2")))

(define_constraint "P1>X"
  "A positive, non-zero integer that fits in 1 bits."
  (and (match_code "const_int")
       (match_test "TARGET_H8300SX")
       (match_test "IN_RANGE (ival, 1, (1 << 1) - 1)")))

(define_constraint "P3>X"
  "A positive, non-zero integer that fits in 3 bits."
  (and (match_code "const_int")
       (match_test "TARGET_H8300SX")
       (match_test "IN_RANGE (ival, 1, (1 << 3) - 1)")))

(define_constraint "P4>X"
  "A positive, non-zero integer that fits in 4 bits."
  (and (match_code "const_int")
       (match_test "TARGET_H8300SX")
       (match_test "IN_RANGE (ival, 1, (1 << 4) - 1)")))

(define_constraint "P5>X"
  "A positive, non-zero integer that fits in 5 bits."
  (and (match_code "const_int")
       (match_test "TARGET_H8300SX")
       (match_test "IN_RANGE (ival, 1, (1 << 5) - 1)")))

(define_constraint "P8>X"
  "A positive, non-zero integer that fits in 8 bits."
  (and (match_code "const_int")
       (match_test "TARGET_H8300SX")
       (match_test "IN_RANGE (ival, 1, (1 << 8) - 1)")))

(define_constraint "P3<X"
  "A negative, non-zero integer that fits in 3 bits."
  (and (match_code "const_int")
       (match_test "TARGET_H8300SX")
       (match_test "IN_RANGE (ival, (-(1 << 3)) + 1, -1)")))

;; Floating-point constraints.
(define_constraint "G"
  "Single-float zero."
  (and (match_code "const_double")
       (match_test "op == CONST0_RTX (SFmode)")))

;; Extra constraints.
(define_special_memory_constraint "Q"
  "@internal"
  (and (match_test "TARGET_H8300SX")
       (match_operand 0 "memory_operand")))

(define_constraint "R"
  "@internal"
  (and (match_code "const_int")
       (match_test "!h8300_shift_needs_scratch_p (ival, QImode, CLOBBER)")))

(define_constraint "C"
  "@internal"
  (match_code "symbol_ref"))

(define_constraint "S"
  "@internal"
  (and (match_code "const_int")
       (match_test "!h8300_shift_needs_scratch_p (ival, HImode, CLOBBER)")))

(define_constraint "T"
  "@internal"
  (and (match_code "const_int")
       (match_test "!h8300_shift_needs_scratch_p (ival, SImode, CLOBBER)")))

(define_memory_constraint "U"
  "An operand valid for a bset destination."
  (ior (and (match_code "mem")
	    (match_code "reg" "0")
	    (match_test "(reload_in_progress || reload_completed)
			 ? REG_OK_FOR_BASE_STRICT_P (XEXP (op, 0))
			 : REG_OK_FOR_BASE_P (XEXP (op, 0))"))
       (and (match_code "mem")
	    (match_code "symbol_ref" "0")
	    (match_test "TARGET_H8300S"))
       (and (match_code "mem")
	    (match_code "const" "0")
	    (match_code "plus" "00")
	    (match_code "symbol_ref" "000")
	    (match_code "const_int" "001")
	    (ior (match_test "TARGET_H8300S")
		 (match_test "(SYMBOL_REF_FLAGS (XEXP (XEXP (XEXP (op, 0), 0), 0)) & SYMBOL_FLAG_EIGHTBIT_DATA) != 0")))
       (and (match_code "mem")
	    (match_test "h8300_eightbit_constant_address_p (XEXP (op, 0))"))
       (and (match_code "mem")
	    (ior (match_test "TARGET_H8300S")
		 (match_test "TARGET_H8300SX"))
	    (match_code "const_int" "0"))))

(define_memory_constraint "WU"
  "@internal"
  (and (match_code "mem")
       (match_test "satisfies_constraint_U (op)")))

(define_constraint "Y0"
  "@internal"
  (and (match_code "const_int")
       (match_test "exact_log2 (~ival & 0xff) != -1")))

(define_constraint "Y2"
  "@internal"
  (and (match_code "const_int")
       (match_test "exact_log2 (ival & 0xff) != -1")))

(define_constraint "Zz"
  "@internal"
  (and (match_test "TARGET_H8300SX")
       (match_code "mem")
       (match_test "CONSTANT_P (XEXP (op, 0))")))

(define_register_constraint "Z0" "NOT_R0_REGS"
  "@internal")

(define_register_constraint "Z1" "NOT_R1_REGS"
  "@internal")

(define_register_constraint "Z2" "NOT_R2_REGS"
  "@internal")

(define_register_constraint "Z3" "NOT_R3_REGS"
  "@internal")

(define_register_constraint "Z4" "NOT_R4_REGS"
  "@internal")

(define_register_constraint "Z5" "NOT_R5_REGS"
  "@internal")

(define_register_constraint "Z6" "NOT_R6_REGS"
  "@internal")

(define_register_constraint "Z7" "NOT_SP_REGS"
  "@internal")

(define_constraint "Za" "@internal" (match_test "pre_incdec_with_reg (op, 0)"))
(define_constraint "Zb" "@internal" (match_test "pre_incdec_with_reg (op, 1)"))
(define_constraint "Zc" "@internal" (match_test "pre_incdec_with_reg (op, 2)"))
(define_constraint "Zd" "@internal" (match_test "pre_incdec_with_reg (op, 3)"))
(define_constraint "Ze" "@internal" (match_test "pre_incdec_with_reg (op, 4)"))
(define_constraint "Zf" "@internal" (match_test "pre_incdec_with_reg (op, 5)"))
(define_constraint "Zg" "@internal" (match_test "pre_incdec_with_reg (op, 6)"))
(define_constraint "Zh" "@internal" (match_test "pre_incdec_with_reg (op, 7)"))
