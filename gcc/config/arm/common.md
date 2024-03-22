;; Common predicate definitions for ARM, Thumb and AArch64
;; Copyright (C) 2020-2024 Free Software Foundation, Inc.
;; Contributed by Fujitsu Ltd.

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

;; Return true if constant is CONST_INT >= 1 and <= 4
(define_predicate "const_1_to_4_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 1, 4)")))

;; Return true if constant is 2 or 4 or 8 or 16
(define_predicate "const_2_4_8_16_operand"
  (and (match_code "const_int")
       (match_test ("   INTVAL (op) == 2
                     || INTVAL (op) == 4
                     || INTVAL (op) == 8
                     || INTVAL (op) == 16 "))))

;; Return true if shift type is lsl and amount is in[1,4].
(define_predicate "alu_shift_operator_lsl_1_to_4"
  (and (match_code "ashift")
       (match_test "const_1_to_4_operand (XEXP (op, 1), mode)")))

;; Return true if the operand is register.
(define_predicate "alu_shift_reg_p"
  (match_test "register_operand (XEXP (op, 1), mode)"))
