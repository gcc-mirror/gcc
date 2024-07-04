;; Constraint definitions for IA-64
;; Copyright (C) 2006-2024 Free Software Foundation, Inc.
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

;; Register constraints

(define_register_constraint "a" "ADDL_REGS"
  "addl register")

(define_register_constraint "b" "BR_REGS"
  "branch register")

(define_register_constraint "c" "PR_REGS"
  "predicate register")

(define_register_constraint "d" "AR_M_REGS"
  "memory pipeline application register")

(define_register_constraint "e" "AR_I_REGS"
  "integer pipeline application register")

(define_register_constraint "f" "FR_REGS"
  "floating-point register")

(define_register_constraint "x" "FP_REGS"
  "floating-point register, excluding f31 and f127, used for fldp")

;; Integer constraints

(define_constraint "I"
  "14 bit signed immediate for arithmetic instructions"
  (and (match_code "const_int")
       (match_test "(unsigned HOST_WIDE_INT)ival + 0x2000 < 0x4000")))

(define_constraint "J"
  "22 bit signed immediate for arith instructions with r0/r1/r2/r3 source"
  (and (match_code "const_int")
       (match_test "(unsigned HOST_WIDE_INT)ival + 0x200000 < 0x400000")))

(define_constraint "j"
  "(2**32-2**13)..(2**32-1) for addp4 instructions"
  (and (match_code "const_int")
       (match_test "(unsigned HOST_WIDE_INT)ival >= 0xffffe000
		    && (unsigned HOST_WIDE_INT)ival <= 0xffffffff")))

(define_constraint "K"
  "8 bit signed immediate for logical instructions"
  (and (match_code "const_int")
       (match_test "(unsigned HOST_WIDE_INT)ival + 0x80 < 0x100")))

(define_constraint "L"
  "8 bit adjusted signed immediate for compare pseudo-ops"
  (and (match_code "const_int")
       (match_test "(unsigned HOST_WIDE_INT)ival + 0x7F < 0x100")))

(define_constraint "M"
  "6 bit unsigned immediate for shift counts"
  (and (match_code "const_int")
       (match_test "(unsigned HOST_WIDE_INT)ival < 0x40")))

(define_constraint "N"
  "9 bit signed immediate for load/store post-increments"
  (and (match_code "const_int")
       (match_test "(unsigned HOST_WIDE_INT)ival + 0x100 < 0x200")))

(define_constraint "O"
  "constant zero"
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "P"
  "0 or -1 for dep instruction"
  (and (match_code "const_int")
       (match_test "ival == 0 || ival == -1")))

;; Floating-point constraints

(define_constraint "G"
  "0.0 and 1.0 for fr0 and fr1"
  (and (match_code "const_double")
       (match_test "op == CONST0_RTX (mode) || op == CONST1_RTX (mode)")))

(define_constraint "Z"
  "1.0 or (0.0 and !flag_signed_zeros)"
  (and (match_code "const_double")
       (ior (match_test "op == CONST1_RTX (mode)")
	    (and (match_test "op == CONST0_RTX (mode)")
		 (match_test "!flag_signed_zeros")))))

(define_constraint "H"
  "0.0"
  (and (match_code "const_double")
       (match_test "op == CONST0_RTX (mode)")))

;; Extra constraints

;; Note that while this accepts mem, it only accepts non-volatile mem,
;; and so cannot be "fixed" by adjusting the address.  Thus it cannot
;; and does not use define_memory_constraint.
(define_constraint "Q"
  "Non-volatile memory for FP_REG loads/stores"
  (and (match_operand 0 "memory_operand")
       (match_test "!MEM_VOLATILE_P (op)")))

(define_constraint "R"
  "1..4 for shladd arguments"
  (and (match_code "const_int")
       (match_test "ival >= 1 && ival <= 4")))

(define_constraint "T"
  "Symbol ref to small-address-area"
  (match_operand 0 "small_addr_symbolic_operand"))

(define_constraint "U"
  "vector zero constant"
  (and (match_code "const_vector")
       (match_test "op == CONST0_RTX (mode)")))

(define_constraint "W"
  "An integer vector, such that conversion to an integer yields a
   value appropriate for an integer 'J' constraint."
  (and (match_code "const_vector")
       (match_test "GET_MODE_CLASS (mode) == MODE_VECTOR_INT")
       (match_test
	"satisfies_constraint_J (simplify_subreg (DImode, op, mode, 0))")))

(define_constraint "Y"
  "A V2SF vector containing elements that satisfy 'G'"
  (and (match_code "const_vector")
       (match_test "mode == V2SFmode")
       (match_test "satisfies_constraint_G (XVECEXP (op, 0, 0))")
       (match_test "satisfies_constraint_G (XVECEXP (op, 0, 1))")))

;; Memory constraints

(define_memory_constraint "S"
  "Non-post-inc memory for asms and other unsavory creatures"
  (and (match_code "mem")
       (match_test "GET_RTX_CLASS (GET_CODE (XEXP (op, 0))) != RTX_AUTOINC")))
