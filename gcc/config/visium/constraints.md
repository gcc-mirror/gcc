;; Constraint definitions for Visium.
;; Copyright (C) 2006-2023 Free Software Foundation, Inc.
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

(define_register_constraint "b" "MDB"
  "EAM register mdb")

(define_register_constraint "c" "MDC"
  "EAM register mdc")

(define_register_constraint "f" "TARGET_FPU ? FP_REGS : NO_REGS"
  "Floating point register")

(define_register_constraint "k" "SIBCALL_REGS"
  "Register for sibcall optimization")

(define_register_constraint "l" "LOW_REGS"
  "General register, but not r29, r30 and r31")

(define_register_constraint "t" "R1"
  "Register r1")

(define_register_constraint "u" "R2"
  "Register r2")

(define_register_constraint "v" "R3"
  "Register r3")

;; Immediate integer operand constraints

(define_constraint "J"
  "Integer constant in the range 0 .. 65535 (16-bit immediate)"
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 65535)")))

(define_constraint "K"
  "Integer constant in the range 1 .. 31 (5-bit immediate)"
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 1, 31)")))

(define_constraint "L"
  "Integer constant in the range -65535 .. -1 (16-bit negative immediate)"
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -65535, -1)")))

(define_constraint "M"
  "Integer constant -1"
  (and (match_code "const_int")
       (match_test "ival == -1")))

(define_constraint "O"
  "Integer constant 0"
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "P"
  "Integer constant 32"
  (and (match_code "const_int")
       (match_test "ival == 32")))

;; Immediate FP operand constraints

(define_constraint "G"
  "Floating-point constant 0.0"
  (and (match_code "const_double")
       (match_test "op == CONST0_RTX (mode)")))
