;; Constraint definitions for S+CORE
;; Copyright (C) 2005, 2007, 2008, 2009, 2010 Free Software Foundation, Inc.
;; Contributed by Sunnorth.

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
;; <http://www.gnu.org/licenses/>.  */

;; -------------------------------------------------------------------------
;; Constraints
;; -------------------------------------------------------------------------

;; Register constraints.
(define_register_constraint "d" "G32_REGS"
  "r0 to r31")

(define_register_constraint "e" "G16_REGS"
  "r0 to r15")

(define_register_constraint "t" "T32_REGS"
  "r8 to r11 | r22 to r27")

(define_register_constraint "h" "HI_REG"
  "hi")

(define_register_constraint "l" "LO_REG"
  "lo")

(define_register_constraint "x" "CE_REGS"
  "hi + lo")

(define_register_constraint "q" "CN_REG"
  "cnt")

(define_register_constraint "y" "LC_REG"
  "lcb")

(define_register_constraint "z" "SC_REG"
  "scb")

(define_register_constraint "a" "SP_REGS"
  "cnt + lcb + scb")

(define_register_constraint "c" "CR_REGS"
  "cr0 to cr15")

;; Integer constant constraints.
(define_constraint "I"
  "High 16-bit constant (32-bit constant with 16 LSBs zero)."
  (and (match_code "const_int")
       (match_test "(ival & 0xffff) == 0")))

(define_constraint "J"
  "Unsigned 5 bit integer (in the range 0 to 31)."
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival <= 31")))

(define_constraint "K"
  "Unsigned 16 bit integer (in the range 0 to 65535)."
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival <= 65535")))

(define_constraint "L"
  "Signed 16 bit integer (in the range −32768 to 32767)."
  (and (match_code "const_int")
       (match_test "ival >= -32768 && ival <= 32767")))

(define_constraint "M"
  "Unsigned 14 bit integer (in the range 0 to 16383)."
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival <= 16383")))

(define_constraint "N"
  "Signed 14 bit integer (in the range −8192 to 8191)."
  (and (match_code "const_int")
       (match_test "ival >= -8192 && ival <= 8191")))

(define_constraint "Z"
  "Any SYMBOL_REF."
  (and (match_code "symbol_ref")
       (match_test "GET_CODE (op) == SYMBOL_REF")))
