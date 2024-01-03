;; Constraint definitions for XSTORMY16.
;; Copyright (C) 2011-2024 Free Software Foundation, Inc.
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
(define_register_constraint "a" "R0_REGS"
  "@internal")

(define_register_constraint "b" "R1_REGS"
  "@internal")

(define_register_constraint "c" "R2_REGS"
  "@internal")

(define_register_constraint "d" "R8_REGS"
  "@internal")

(define_register_constraint "e" "EIGHT_REGS"
  "@internal")

(define_register_constraint "t" "TWO_REGS"
  "@internal")

(define_register_constraint "z" "ICALL_REGS"
  "@internal")

;; Integer constraints.
(define_constraint "I"
  "An integer between 0 and 3."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 3)")))

(define_constraint "J"
  "A power of two."
  (and (match_code "const_int")
       (match_test "exact_log2 (ival) != -1")))

(define_constraint "K"
  "A power of two when inverted."
  (and (match_code "const_int")
       (match_test "exact_log2 (~ival) != -1")))

(define_constraint "L"
  "An 8-bit unsigned integer."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 255)")))

(define_constraint "M"
  "An integer between -255 and 0."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -255, 0)")))

(define_constraint "N"
  "An integer between -3 and 0."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -3, 0)")))

(define_constraint "O"
  "An integer between 1 and 4."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 1, 4)")))

(define_constraint "P"
  "An integer between -4 and -1."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -4, -1)")))

;; Extra constraints.
(define_constraint "Q"
  "A register push operation."
  (and (match_code "mem")
       (match_code "post_inc" "0")
       (match_test "XEXP (XEXP (op, 0), 0) == stack_pointer_rtx")))

(define_constraint "R"
  "A register pop operation."
  (and (match_code "mem")
       (match_code "pre_dec" "0")
       (match_test "XEXP (XEXP (op, 0), 0) == stack_pointer_rtx")))

(define_constraint "S"
  "An immediate memory address."
  (and (match_code "mem")
       (match_code "const_int" "0")
       (match_test "xstormy16_legitimate_address_p (VOIDmode, XEXP (op, 0), false)")))

(define_constraint "T"
  "@internal"
  ;; For Rx; not implemented yet.
  (match_test "0"))

(define_constraint "U"
  "An integer not between 2 and 15."
  (and (match_code "const_int")
       (match_test "!IN_RANGE (ival, 2, 15)")))

(define_constraint "W"
  "@internal"
  (match_operand 0 "xstormy16_below100_operand"))

(define_constraint "Z"
  "Zero."
  (and (match_code "const_int")
       (match_test "ival == 0")))