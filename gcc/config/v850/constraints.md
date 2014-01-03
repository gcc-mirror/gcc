;; Constraint definitions for V850.
;; Copyright (C) 2011-2014 Free Software Foundation, Inc.
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

(define_register_constraint "e" "EVEN_REGS"
  "@internal")

;; Integer constraints.
(define_constraint "I"
  "Integer constant 0."
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "J"
  "A signed 5-bit immediate."
  (and (match_code "const_int")
       (match_test "ival >= -16 && ival <= 15")))

(define_constraint "K"
  "A signed 16-bit immediate."
  (and (match_code "const_int")
       (match_test "ival >= -32768 && ival <= 32767")))

(define_constraint "L"
  "A valid constant for a movhi instruction."
  (and (match_code "const_int")
       (ior (match_test "(ival | 0x7fff0000) == 0x7fff0000")
	    (match_test "(ival | 0x7fff0000) + 0x10000 == 0"))))

(define_constraint "M"
  "An unsigned 16-bit immediate."
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival <= 65535")))

(define_constraint "N"
  "An unsigned 5-bit immediate in shift instructions."
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival <= 31")))

(define_constraint "O"
  "A signed 9-bit immediate for word multiply instructions."
  (and (match_code "const_int")
       (match_test "ival >= -255 && ival <= 255")))

(define_constraint "P"
  "@internal"
  (and (match_code "const_int")
       (match_test "0")))

;; Floating-point constraints.
(define_constraint "G"
  "A zero of some form."
  (and (match_code "const_double")
       (ior (match_test "GET_MODE_CLASS (mode) == MODE_FLOAT")
	    (match_test "GET_MODE_CLASS (mode) == MODE_INT"))
       (match_test "op == CONST0_RTX (mode)")))

(define_constraint "H"
  "@internal"
  (and (match_code "const_double")
       (match_test "0")))

;;; Extra constraints.
(define_memory_constraint "Q"
  "A memory address that does not contain a symbol address."
  (and (match_code "mem")
       (match_test "ep_memory_operand (op, mode, FALSE)")))

(define_constraint "R"
  "@internal"
  (match_test "special_symbolref_operand (op, VOIDmode)"))

(define_constraint "S"
  "@internal"
  (and (match_code "symbol_ref")
       (match_test "!SYMBOL_REF_ZDA_P (op)")))

(define_constraint "T"
  "@internal"
  (match_test "ep_memory_operand (op, mode, TRUE)"))

(define_constraint "U"
  "@internal"
  (ior (and (match_code "symbol_ref")
	    (match_test "SYMBOL_REF_ZDA_P (op)"))
       (and (match_code "const")
	    (match_test "GET_CODE (XEXP (op, 0)) == PLUS")
	    (match_test "GET_CODE (XEXP (XEXP (op, 0), 0)) == SYMBOL_REF")
	    (match_test "SYMBOL_REF_ZDA_P (XEXP (XEXP (op, 0), 0))"))))

(define_constraint "W"
  "@internal"
  (match_test "disp23_operand (op, VOIDmode)"))
