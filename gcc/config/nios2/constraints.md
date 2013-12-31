;; Constraint definitions for Altera Nios II.
;; Copyright (C) 2012-2013 Free Software Foundation, Inc.
;; Contributed by Chung-Lin Tang <cltang@codesourcery.com>
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

;; We use the following constraint letters for constants
;;
;;  I: -32768 to -32767
;;  J: 0 to 65535
;;  K: $nnnn0000 for some nnnn
;;  L: 0 to 31 (for shift counts)
;;  M: 0
;;  N: 0 to 255 (for custom instruction numbers)
;;  O: 0 to 31 (for control register numbers)
;;
;; We use the following built-in register classes:
;;
;;  r: general purpose register (r0..r31)
;;  m: memory operand
;;
;; Plus, we define the following constraint strings:
;;
;;  S: symbol that is in the "small data" area

;; Register constraints

(define_register_constraint "j" "SIB_REGS"
  "A register suitable for an indirect sibcall.")

;; Integer constraints

(define_constraint "I"
  "A signed 16-bit constant (for arithmetic instructions)."
  (and (match_code "const_int")
       (match_test "SMALL_INT (ival)")))

(define_constraint "J"
  "An unsigned 16-bit constant (for logical instructions)."
  (and (match_code "const_int")
       (match_test "SMALL_INT_UNSIGNED (ival)")))

(define_constraint "K"
  "An unsigned 16-bit high constant (for logical instructions)."
  (and (match_code "const_int")
       (match_test "UPPER16_INT (ival)")))

(define_constraint "L"
  "An unsigned 5-bit constant (for shift counts)."
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival <= 31")))

(define_constraint "M"
  "Integer zero."
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "N"
  "An unsigned 8-bit constant (for custom instruction codes)."
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival <= 255")))

(define_constraint "O"
  "An unsigned 5-bit constant (for control register numbers)."
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival <= 31")))

(define_constraint "S"
  "An immediate stored in small data, accessible by GP."
  (and (match_code "symbol_ref")
       (match_test "nios2_symbol_ref_in_small_data_p (op)")))

(define_constraint "T"
  "A constant unspec offset representing a relocation."
  (match_test "nios2_unspec_reloc_p (op)"))
