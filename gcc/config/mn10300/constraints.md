;; Constraint definitions for the MN10300.
;; Copyright (C) 2007-2014 Free Software Foundation, Inc.
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

(define_register_constraint "d" "DATA_REGS"
  "A data register.")

(define_register_constraint "a" "ADDRESS_REGS"
  "An address register.")

;; This can be used for QI/HImode memory operations, and most arithmetic.
;; AM33 supports these on all registers, where MN103 needs DATA_REGS.
(define_register_constraint "D" "TARGET_AM33 ? GENERAL_REGS : DATA_REGS"
  "A general register for AM33, and a data register otherwise.")

;; Similarly for ADDRESS_REGS vs GENERAL_REGS.
(define_register_constraint "A" "TARGET_AM33 ? GENERAL_REGS : ADDRESS_REGS"
  "A general register for AM33, and an address register otherwise.")

(define_register_constraint "y" "SP_REGS"
  "An SP register (if available).")

(define_register_constraint "z" "MDR_REGS"
  "The MDR register.")

(define_register_constraint "x" "TARGET_AM33 ? EXTENDED_REGS : NO_REGS"
  "An extended register.")

(define_register_constraint "f" "TARGET_AM33_2 ? FP_REGS : NO_REGS"
  "A floating point register.")

(define_register_constraint "c" "TARGET_AM33_2 ? FP_ACC_REGS : NO_REGS"
  "A floating point accumulator register.")

(define_memory_constraint "Q"
  "@internal"
  (and (match_code "mem")
       (match_test "!CONSTANT_ADDRESS_P (XEXP (op, 0))")))

(define_constraint "S"
  "@internal"
  (if_then_else (match_test "flag_pic")
	(and (match_test "GET_CODE (op) == UNSPEC")
	     (ior (match_test "XINT (op, 1) == UNSPEC_PLT")
		  (match_test "XINT (op, 1) == UNSPEC_PIC")
		  (match_test "XINT (op, 1) == UNSPEC_GOTSYM_OFF")))
	(match_test "GET_CODE (op) == SYMBOL_REF")))

;; Integer constraints

(define_constraint "I"
  "An integer zero."
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "J"
  "An integer one."
  (and (match_code "const_int")
       (match_test "ival == 1")))

(define_constraint "K"
  "An integer two."
  (and (match_code "const_int")
       (match_test "ival == 2")))

(define_constraint "L"
  "An integer four."
  (and (match_code "const_int")
       (match_test "ival == 4")))

(define_constraint "M"
  "An integer three."
  (and (match_code "const_int")
       (match_test "ival == 3")))

(define_constraint "N"
  "An integer of either 255 or 65535."
  (and (match_code "const_int")
       (ior (match_test "ival == 255")
	    (match_test "ival == 65535"))))

(define_constraint "O"
  "An integer between -8 and +7 inclusive."
  (and (match_code "const_int")
       (and (match_test "ival >= -8")
	    (match_test "ival <=  7"))))

;; Floating-point constraints
(define_constraint "G"
  "Floating-point zero."
  (and (match_code "const_double")
       (match_test "op == CONST0_RTX (mode)")))
