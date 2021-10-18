;; Constraint definitions for TI PRU.
;; Copyright (C) 2014-2021 Free Software Foundation, Inc.
;; Contributed by Dimitar Dimitrov <dimitar@dinux.eu>
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

;; We use the following constraint letters for constants:
;;
;;  I: 0 to 255.
;;  J: 0 to 65535.
;;  L: 0 to 31 (for shift counts).
;;  T: Text segment label.  Needed to know when to select %pmem relocation.
;;  Z: Constant integer zero.
;;
;; We use the following built-in register classes:
;;
;;  r: General purpose register (r0..r31).
;;  m: Memory operand.
;;
;; The following constraints are intended for internal use only:
;;  Rmd0, Rms0, Rms1: Registers for MUL instruction operands.
;;  Rsib: Jump address register suitable for sibling calls.
;;  Rrio: The R30 and R31 I/O registers.
;;  M: -255 to 0 (for converting ADD to SUB with suitable UBYTE OP2).
;;  N: -32768 to 32767 (16-bit signed integer).
;;  O: -128 to 127 (8-bit signed integer).
;;  P: 1

;; Register constraints.

(define_register_constraint "Rsib" "SIB_REGS"
  "@internal
  A register suitable for an indirect sibcall.")

(define_register_constraint "Rmd0" "MULDST_REGS"
  "@internal
  The multiply destination register.")

(define_register_constraint "Rms0" "MULSRC0_REGS"
  "@internal
  The multiply source 0 register.")

(define_register_constraint "Rms1" "MULSRC1_REGS"
  "@internal
  The multiply source 1 register.")

(define_register_constraint "Rrio" "REGIO_REGS"
  "@internal
  The R30 and R31 I/O registers.")

;; Integer constraints.

(define_constraint "I"
  "An unsigned 8-bit constant."
  (and (match_code "const_int")
       (match_test "UBYTE_INT (ival)")))

(define_constraint "J"
  "An unsigned 16-bit constant."
  (and (match_code "const_int")
       (match_test "UHWORD_INT (ival)")))

(define_constraint "L"
  "An unsigned 5-bit constant (for shift counts)."
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival <= 31")))

(define_constraint "M"
  "@internal
  A constant in the range [-255, 0]."
  (and (match_code "const_int")
       (match_test "UBYTE_INT (-ival)")))

(define_constraint "N"
  "@internal
  A constant in the range [-32768, 32767]."
  (and (match_code "const_int")
       (match_test "SHWORD_INT (ival)")))

(define_constraint "O"
  "@internal
  A constant in the range [-128, 127]."
  (and (match_code "const_int")
       (match_test "SBYTE_INT (ival)")))

(define_constraint "P"
  "@internal
  A constant 1."
  (and (match_code "const_int")
       (match_test "ival == 1")))

(define_constraint "T"
  "A text segment (program memory) constant label."
  (match_test "text_segment_operand (op, VOIDmode)"))

(define_constraint "Z"
  "An integer constant zero."
  (and (match_code "const_int")
       (match_test "ival == 0")))
