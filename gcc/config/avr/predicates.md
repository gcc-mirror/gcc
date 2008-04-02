;; Predicate definitions for ATMEL AVR micro controllers.
;; Copyright (C) 2006, 2007, 2008 Free Software Foundation, Inc.
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

;; Registers from r0 to r15.
(define_predicate "l_register_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) <= 15")))

;; Registers from r16 to r31.
(define_predicate "d_register_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) >= 16 && REGNO (op) <= 31")))

(define_predicate "even_register_operand"
  (and (match_code "reg")
       (and (match_test "REGNO (op) <= 31")
            (match_test "(REGNO (op) & 1) == 0"))))

(define_predicate "odd_register_operand"
  (and (match_code "reg")
       (and (match_test "REGNO (op) <= 31")
            (match_test "(REGNO (op) & 1) != 0"))))

;; SP register.
(define_predicate "stack_register_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) == REG_SP")))

;; Return true if OP is a valid address for lower half of I/O space.
(define_predicate "low_io_address_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE((INTVAL (op)), 0x20, 0x3F)")))

;; Return true if OP is a valid address for high half of I/O space.
(define_predicate "high_io_address_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE((INTVAL (op)), 0x40, 0x5F)")))

;; Return true if OP is a valid address of I/O space.
(define_predicate "io_address_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE((INTVAL (op)), 0x20, (0x60 - GET_MODE_SIZE(mode)))")))

;; Return 1 if OP is the zero constant for MODE.
(define_predicate "const0_operand"
  (and (match_code "const_int,const_double")
       (match_test "op == CONST0_RTX (mode)")))

;; Returns true if OP is either the constant zero or a register.
(define_predicate "reg_or_0_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "const0_operand")))

;; Returns 1 if OP is a SYMBOL_REF.
(define_predicate "symbol_ref_operand"
  (match_code "symbol_ref"))

;; Return true if OP is a constant that contains only one 1 in its
;; binary representation.
(define_predicate "single_one_operand"
  (and (match_code "const_int")
       (match_test "exact_log2(INTVAL (op) & GET_MODE_MASK (mode)) >= 0")))

;; Return true if OP is a constant that contains only one 0 in its
;; binary representation.
(define_predicate "single_zero_operand"
  (and (match_code "const_int")
       (match_test "exact_log2(~INTVAL (op) & GET_MODE_MASK (mode)) >= 0")))

;;
(define_predicate "avr_sp_immediate_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) >= -6 && INTVAL (op) <= 5")))

;; True for EQ & NE
(define_predicate "eqne_operator"
  (match_code "eq,ne"))
       
;; True for GE & LT
(define_predicate "gelt_operator"
  (match_code "ge,lt"))
       
;; True for GT, GTU, LE & LEU
(define_predicate "difficult_comparison_operator"
  (match_code "gt,gtu,le,leu"))

;; False for GT, GTU, LE & LEU
(define_predicate "simple_comparison_operator"
  (and (match_operand 0 "comparison_operator")
       (not (match_code "gt,gtu,le,leu"))))

;; Return true if OP is a valid call operand.
(define_predicate "call_insn_operand"
  (and (match_code "mem")
       (ior (match_test "register_operand (XEXP (op, 0), mode)")
            (match_test "CONSTANT_ADDRESS_P (XEXP (op, 0))"))))
