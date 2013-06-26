;; Constraint definitions for Xilinx MicroBlaze processors.
;; Copyright (C) 2010-2013 Free Software Foundation, Inc.

;; Contributed by Michael Eager <eager@eagercon.com>.

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

(define_register_constraint "d" "GR_REGS"
  "A general register.")

(define_register_constraint "z" "ST_REGS"
  "A status register.")

;; Define integer constraints

(define_constraint "I"
  "A signed 16-bit constant."
  (and (match_code "const_int")
       (match_test "SMALL_OPERAND (ival)")))

(define_constraint "J"
  "Integer zero."
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "M"
  "A constant which needs two instructions to load."
  (and (match_code "const_int")
       (match_test "LARGE_OPERAND (ival)")))

(define_constraint "N"
  "A constant in the range -65535 to -1 (inclusive)."
  (and (match_code "const_int")
       (match_test "(unsigned HOST_WIDE_INT) (ival + 0xffff) < 0xffff")))

(define_constraint "P"
  "A constant in the range 1 to 65535 (inclusive)."
  (and (match_code "const_int")
       (match_test "ival > 0 && ival < 0x10000")))

;; Define floating point constraints

(define_constraint "G"
  "Floating-point zero."
  (and (match_code "const_double")
       (match_test "op == CONST0_RTX (mode)")))

;; Define memory constraints

(define_memory_constraint "R"
  "Memory operand which fits in single instruction."
  (and (match_code "mem")
       (match_test "simple_memory_operand (op, GET_MODE (op))")))

(define_memory_constraint "T"
  "Double word operand."
  (and (match_code "mem")
       (match_test "double_memory_operand (op, GET_MODE (op))")))

(define_memory_constraint "Q"
  "Memory operand which is a single register."
  (and (match_code "mem")
       (match_test "GET_CODE ( XEXP (op, 0)) == REG")))
