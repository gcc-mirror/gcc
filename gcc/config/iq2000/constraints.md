;; Constraints for Vitesse IQ2000 processors
;; Copyright (C) 2011-2019 Free Software Foundation, Inc.

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
(define_register_constraint "b" "ALL_REGS"
  "@internal")

(define_register_constraint "d" "GR_REGS"
  "@internal")

(define_register_constraint "y" "GR_REGS"
  "@internal")

;; Integer constraints.
(define_constraint "I"
  "A 16-bit signed integer."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -32768, 32767)")))

(define_constraint "J"
  "Zero."
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "K"
  "A 16-bit unsigned integer"
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 65535)")))

(define_constraint "L"
  "A 32-bit constant whose bottom 16 bits are zero."
  (and (match_code "const_int")
      (ior (match_test "(ival | 0x7fff0000) == 0x7fff0000")
	   (match_test "(ival | 0x7fff0000) + 0x10000 == 0"))))

(define_constraint "M"
  "Any constant not matched by 'I', 'K', or 'L'."
  (and (match_code "const_int")
       (match_test "!insn_const_int_ok_for_constraint (ival, CONSTRAINT_I)")
       (match_test "!insn_const_int_ok_for_constraint (ival, CONSTRAINT_K)")
       (match_test "!insn_const_int_ok_for_constraint (ival, CONSTRAINT_L)")))

(define_constraint "N"
  "Any constant whose lower or upper 16 bits are 0xffff."
  (and (match_code "const_int")
       (ior (match_test "(ival & 0xffff) == 0xffff")
	    (match_test "(ival & 0xffff0000) == 0xffff0000"))))

(define_constraint "O"
  "A 5-bit signed integer."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -31, 31)")))

;; Floating-point constraints.
(define_constraint "G"
  "Floating-point zero."
  (and (match_code "const_double")
       (match_test "op == CONST0_RTX (mode)")))

;; Extra constraints.
(define_constraint "R"
  "A memory reference which takes one word for the instruction."
  (match_test "simple_memory_operand (op, mode)"))
