;; Constraints for C-SKY.
;; Copyright (C) 2018-2024 Free Software Foundation, Inc.
;; Contributed by C-SKY Microsystems and Mentor Graphics.
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
;; <http://www.gnu.org/licenses/>.  */

;; Register constraints.

(define_register_constraint "a" "MINI_REGS" "r0 - r7")
(define_register_constraint "b" "LOW_REGS"  "r0 - r15")
(define_register_constraint "c" "C_REGS" "C register")
(define_register_constraint "y" "HILO_REGS" "HI and LO registers")
(define_register_constraint "v" "V_REGS" "vector registers")
(define_register_constraint "z" "SP_REGS" "SP register")


;; Memory and misc constraints.

(define_memory_constraint "Q"
  "Memory operands with base register, index register and short displacement for FPUV2"
  (match_test "csky_valid_mem_constraint_operand (op, \"Q\")"))

(define_memory_constraint "W"
  "Memory operands with base register, index register"
  (match_test "csky_valid_mem_constraint_operand (op, \"W\")"))

(define_memory_constraint "Y"
  "Memory operands without index register"
  (not (match_test "csky_valid_mem_constraint_operand (op, \"W\")")))

(define_constraint "R"
  "Memory operands whose address is a label_ref"
  (and (match_code "mem")
       (match_test "GET_CODE (XEXP (op, 0)) == LABEL_REF")))

(define_constraint "S"
  "Symbol reference with optional offset"
  (match_test "csky_symbolic_address_p (op)"))


;; Constant integer constraints.

(define_constraint "I"
  "Constant in range [0, 65535]"
  (and (match_code "const_int")
       (match_test "CSKY_CONST_OK_FOR_I (ival)")))

(define_constraint "J"
  "Constant in range [1, 32]"
  (and (match_code "const_int")
       (match_test "CSKY_CONST_OK_FOR_J (ival)")))

(define_constraint "K"
  "Constant in range [0, 31]"
  (and (match_code "const_int")
       (match_test "CSKY_CONST_OK_FOR_K (ival)")))

(define_constraint "L"
  "Constant in range [1, 8]"
  (and (match_code "const_int")
       (match_test "CSKY_CONST_OK_FOR_L (ival)")))

(define_constraint "M"
  "Constant in range [1, 4096]"
  (and (match_code "const_int")
       (match_test "CSKY_CONST_OK_FOR_M (ival)")))

(define_constraint "N"
  "Constant in range [1, 256]"
  (and (match_code "const_int")
       (match_test "CSKY_CONST_OK_FOR_N (ival)")))

(define_constraint "O"
  "Constant in range [0, 4095]"
  (and (match_code "const_int")
       (match_test "CSKY_CONST_OK_FOR_O (ival)")))

(define_constraint "P"
  "Constant in range [4, 508] that is divisible by 4"
  (and (match_code "const_int")
       (match_test "CSKY_CONST_OK_FOR_P (ival)")))

(define_constraint "T"
  "Constant in range [-256, -1]"
  (and (match_code "const_int")
       (match_test "CSKY_CONST_OK_FOR_T (ival)")))

(define_constraint "Ua"
  "Constant 0"
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "Ub"
  "Unsigned int that is an exact power of 2"
  (and (match_code "const_int")
       (match_test "CSKY_CONST_OK_FOR_Ub (ival)")))

(define_constraint "Uc"
  "Unsigned int X such that X+1 is an exact power of 2"
  (and (match_code "const_int")
       (match_test "CSKY_CONST_OK_FOR_Uc (ival)")))

(define_constraint "Ud"
  "64-bit int whose high/low words separately satisfy I, Ub, or Uc"
  (and (match_code "const_int")
       (match_test "CSKY_CONST_OK_FOR_Ud (ival)")))

(define_constraint "Ug"
  "Constant in range [-508, -4] that is divisible by 4"
  (and (match_code "const_int")
       (match_test "CSKY_CONST_OK_FOR_Ug (ival)")))

(define_constraint "Uh"
  "Constant in range [-31, 0]"
  (and (match_code "const_int")
       (match_test "CSKY_CONST_OK_FOR_Uh (ival)")))

(define_constraint "Uj"
  "Constant in range [4, 1024] that is divisible by 4"
  (and (match_code "const_int")
       (match_test "CSKY_CONST_OK_FOR_Uj (ival)")))

(define_constraint "Uk"
  "Constant in range [1, 65536]"
  (and (match_code "const_int")
       (match_test "CSKY_CONST_OK_FOR_Uk (ival)")))

(define_constraint "Ul"
  "Constant in range [-1024, -4] that is divisible by 4"
  (and (match_code "const_int")
       (match_test "CSKY_CONST_OK_FOR_Ul (ival)")))

(define_constraint "Um"
  "Constant in range [-4096, -1]"
  (and (match_code "const_int")
       (match_test "CSKY_CONST_OK_FOR_Um (ival)")))

(define_constraint "Un"
  "Constant whose low 16 bits are all zeros"
  (and (match_code "const_int")
       (match_test "CSKY_CONST_OK_FOR_MOVIH (ival)")))

(define_constraint "Uo"
  "Constant that can be synthesized with an extra instruction"
  (and (match_code "const_int")
       (match_test "csky_inlinable_constant (ival)")))

(define_constraint "Up"
  "Constant in range [0, 255]"
  (and (match_code "const_int")
       (match_test "CSKY_CONST_OK_FOR_N (ival + 1)")))

(define_constraint "Uq"
  "Constant in range [0, 1020] that is divisible by 4"
  (and (match_code "const_int")
       (match_test "CSKY_CONST_OK_FOR_Uj (ival + 4)")))

(define_constraint "Ur"
  "Constant in range [-1020, -4] that is divisible by 4"
  (and (match_code "const_int")
       (match_test "CSKY_CONST_OK_FOR_Uj (-ival + 4)")))

(define_constraint "Us"
  "Constant in range [-8, -1]"
  (and (match_code "const_int")
       (match_test "CSKY_CONST_OK_FOR_US (ival)")))

(define_constraint "Dv"
 "@VFPv3
  A const_double which can be used with a VFP fmovi
  instruction."
  (and (match_code "const_double")
       (match_test "fpuv3_const_double_rtx (op)")))
