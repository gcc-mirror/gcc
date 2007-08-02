;; Constraint definitions for DEC Alpha.
;; Copyright (C) 2007 Free Software Foundation, Inc.
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

;;; Unused letters:
;;;    ABCDEF               V  YZ
;;;       de ghijklmnopq stu wxyz

;; Integer register constraints.

(define_register_constraint "a" "R24_REG"
 "General register 24, input to division routine")

(define_register_constraint "b" "R25_REG"
 "General register 24, input to division routine")

(define_register_constraint "c" "R27_REG"
 "General register 27, function call address")

(define_register_constraint "f" "FLOAT_REGS"
 "Any floating-point register")

(define_register_constraint "v" "R0_REG"
 "General register 0, function value return address")

;; Integer constant constraints.
(define_constraint "I"
  "An unsigned 8 bit constant"
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival <= 255")))

(define_constraint "J"
  "The constant zero"
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "K"
  "Signed 16-bit integer constant"
  (and (match_code "const_int")
       (match_test "ival >= -32768 && ival < 32768 ")))

(define_constraint "L"
  "A shifted signed 16-bit constant appropriate for LDAH"
  (and (match_code "const_int")
       (match_test "(ival & 0xffff) == 0
		    && (ival >> 31 == -1 || ival >> 31 == 0)")))

(define_constraint "M"
  "A valid operand of a ZAP insn"
  (and (match_code "const_int")
       (match_test "zap_mask (ival) != 0")))

(define_constraint "N"
  "A complemented unsigned 8-bit constant"
  (and (match_code "const_int")
       (match_test "~ival >= 0 && ~ival <= 255")))

(define_constraint "O"
  "A negated unsigned 8-bit constant"
  (and (match_code "const_int")
       (match_test "-ival >= 0 && -ival <= 255")))

(define_constraint "P"
  "The constant 1, 2 or 3"
  (and (match_code "const_int")
       (match_test "ival == 1 || ival == 2 || ival == 3")))

(define_constraint "H"
  "A valid operand of a ZAP insn, when building with 32-bit HOST_WIDE_INT"
  (and (match_code "const_double")
       (match_test "mode == VOIDmode && zap_mask (hval) && zap_mask (lval)")))

;; Floating-point constant constraints.
(define_constraint "G"
  "The floating point zero constant"
  (and (match_code "const_double")
       (match_test "GET_MODE_CLASS (mode) == MODE_FLOAT
		    && op == CONST0_RTX (mode)")))

;; "Extra" constraints.
(define_constraint "Q"
  "@internal A normal_memory_operand"
  (match_operand 0 "normal_memory_operand"))

(define_constraint "R"
  "@internal A direct_call_operand"
  (match_operand:DI 0 "direct_call_operand"))

(define_constraint "S"
  "An unsigned 6-bit constant"
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival <= 63")))

(define_constraint "T"
  "@internal A high-part symbol"
  (match_code "high"))

(define_constraint "U"
  "@internal A UNICOSMK symbol"
  (and (match_test "TARGET_ABI_UNICOSMK")
       (match_operand 0 "symbolic_operand")))

(define_constraint "W"
  "A vector zero constant"
  (and (match_code "const_vector")
       (match_test "op == CONST0_RTX (mode)")))
