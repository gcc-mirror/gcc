;; Constraint definitions for FT32
;; Copyright (C) 2015-2024 Free Software Foundation, Inc.
;; Contributed by FTDI <support@ftdi.com>

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

;; -------------------------------------------------------------------------
;; Constraints
;; -------------------------------------------------------------------------

(define_memory_constraint "A"
  "An absolute address."
  (and (match_code "mem")
       (match_test "(!ft32_is_mem_pm(op))")
       (ior (match_test "GET_CODE (XEXP (op, 0)) == SYMBOL_REF")
            (match_test "GET_CODE (XEXP (op, 0)) == LABEL_REF")
            (match_test "GET_CODE (XEXP (op, 0)) == CONST_INT")
            (and (match_test "(GET_CODE (XEXP (op, 0)) == PLUS)")
                 (ior (match_test "GET_CODE (XEXP (XEXP (op, 0), 0)) == SYMBOL_REF")
                      (match_test "GET_CODE (XEXP (XEXP (op, 0), 0)) == LABEL_REF")
                      (match_test "GET_CODE (XEXP (XEXP (op, 0), 0)) == CONST_INT"))
                 (ior (match_test "GET_CODE (XEXP (XEXP (op, 0), 1)) == SYMBOL_REF")
                      (match_test "GET_CODE (XEXP (XEXP (op, 0), 1)) == LABEL_REF")
                      (match_test "GET_CODE (XEXP (XEXP (op, 0), 1)) == CONST_INT"))))))

(define_memory_constraint "B"
  "An offset address."
  (and (match_code "mem")
       (match_test "(!ft32_is_mem_pm(op))")
       (match_test "(GET_CODE (XEXP (op, 0)) == PLUS)")))

(define_memory_constraint "W"
  "A register indirect memory operand."
  (and (match_code "mem")
       (match_test "!ft32_is_mem_pm(op)
        && REG_P (XEXP (op, 0))
		    && REGNO_OK_FOR_BASE_P (REGNO (XEXP (op, 0)))")))

(define_memory_constraint "e"
  "An offset address."
  (and (match_code "mem")
       (match_test "ft32_is_mem_pm(op) && (
          (GET_CODE (XEXP (op, 0)) == SYMBOL_REF) ||
          (GET_CODE (XEXP (op, 0)) == LABEL_REF) ||
          (GET_CODE (XEXP (op, 0)) == CONST_INT) ||
          (GET_CODE (XEXP (op, 0)) == CONST))"
       )))

(define_memory_constraint "f"
  "An offset address."
  (and (match_code "mem")
       (match_test "ft32_is_mem_pm(op) && (
          ((GET_CODE (XEXP (op, 0)) == PLUS)) ||
          (GET_CODE (XEXP (op, 0)) == REG))"
       )))

(define_constraint "O"
  "The constant zero or one"
  (and (match_code "const_int")
       (match_test "((ival == 0) || (ival == 1))")))

(define_constraint "I"
  "A 16-bit signed constant (-32768..32767)"
  (and (match_code "const_int")
       (match_test "ival >= -32768 && ival <= 32767")))

(define_constraint "w"
  "A bitfield mask suitable for bext or bins"
  (and (match_code "const_int")
       (match_test "ft32_as_bitfield(ival) != -1")))

(define_constraint "x"
  "An inverted bitfield mask suitable for bext or bins"
  (and (match_code "const_int")
       (match_test "ft32_as_bitfield(0xffffffff ^ ival) != -1")))

(define_constraint "L"
  "A 16-bit unsigned constant, multiple of 4 (-65532..0)"
  (and (match_code "const_int")
       (match_test "ival >= -65532 && ival <= 0 && (ival & 3) == 0")))

(define_constraint "S"
  "A 20-bit signed constant (-524288..524287)"
  (ior
    (and (match_code "const_int")
         (match_test "ival >= -524288 && ival <= 524287"))
    (match_test "GET_CODE (op) == LABEL_REF")
    (match_test "GET_CODE (op) == SYMBOL_REF")
    (match_test "GET_CODE (op) == CONST")))

(define_constraint "b"
  "A constant for a bitfield width (1..16)"
  (and (match_code "const_int")
       (match_test "ival >= 1 && ival <= 16")))

(define_constraint "KA"
  "A 10-bit signed constant (-512..511)"
  (and (match_code "const_int")
       (match_test "ival >= -512 && ival <= 511")))
