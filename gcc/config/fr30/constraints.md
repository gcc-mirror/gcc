;; Constraint definitions for the FR30.
;; Copyright (C) 2011-2021 Free Software Foundation, Inc.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Register constraints.
(define_register_constraint "d" "MULTIPLY_64_REG"
  "The MDH,MDL register pair as used by MUL and MULU.")

(define_register_constraint "e" "MULTIPLY_32_REG"
  "The MDL register as used by MULH and MULUH.")

(define_register_constraint "h" "HIGH_REGS"
  "Registers 8 through 15.")

(define_register_constraint "l" "LOW_REGS"
  "Registers 0 through 7.")

(define_register_constraint "a" "ALL_REGS"
  "@internal")

;; Integer constraints.
(define_constraint "I"
  "An integer in the range 0 to 15."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 15)")))

(define_constraint "J"
  "An integer in the range -16 to -1."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -16, -1)")))

(define_constraint "K"
  "An integer in the range 16 to 31."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 16, 31)")))

(define_constraint "L"
  "An integer in the range 0 to 255."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 255)")))

(define_constraint "M"
  "An integer in the range 0 to 1048575."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 1048575)")))

(define_constraint "P"
  "An integer in the range -256 to 255."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -256, 255)")))

;; Extra constraints.
(define_constraint "Q"
  "@internal"
  (and (match_code "mem")
       (match_code "symbol_ref" "0")))
