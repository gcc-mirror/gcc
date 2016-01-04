;; Constraint definitions for FRV.
;; Copyright (C) 2001-2016 Free Software Foundation, Inc.
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

;; Register constraints.
(define_register_constraint "a" "ACC_REGS"
  "@internal")

(define_register_constraint "b" "EVEN_ACC_REGS"
  "@internal")

(define_register_constraint "c" "CC_REGS"
  "@internal")

(define_register_constraint "d" "GPR_REGS"
  "@internal")

(define_register_constraint "e" "EVEN_REGS"
  "@internal")

(define_register_constraint "f" "FPR_REGS"
  "@internal")

(define_register_constraint "h" "FEVEN_REGS"
  "@internal")

(define_register_constraint "l" "LR_REG"
  "@internal")

(define_register_constraint "q" "QUAD_REGS"
  "@internal")

(define_register_constraint "t" "ICC_REGS"
  "@internal")

(define_register_constraint "u" "FCC_REGS"
  "@internal")

(define_register_constraint "v" "ICR_REGS"
  "@internal")

(define_register_constraint "w" "FCR_REGS"
  "@internal")

(define_register_constraint "x" "QUAD_FPR_REGS"
  "@internal")

(define_register_constraint "y" "LCR_REG"
  "@internal")

(define_register_constraint "z" "SPR_REGS"
  "@internal")

(define_register_constraint "A" "QUAD_ACC_REGS"
  "@internal")

(define_register_constraint "B" "ACCG_REGS"
  "@internal")

(define_register_constraint "C" "CR_REGS"
  "@internal")

(define_register_constraint "D89" "GR89_REGS"
  "@internal")

(define_register_constraint "D09" "GR9_REGS"
  "@internal")

(define_register_constraint "D08" "GR8_REGS"
  "@internal")

(define_register_constraint "D14" "FDPIC_FPTR_REGS"
  "@internal")

(define_register_constraint "D15" "FDPIC_REGS"
  "@internal")

(define_register_constraint "W" "FDPIC_CALL_REGS"
  "@internal")

(define_register_constraint "Z" "FDPIC_REGS"
  "@internal")

;; Integer constraints.
(define_constraint "I"
  "A signed 6-bit immediate."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -32, 31)")))

(define_constraint "J"
  "A signed 10-bit immediate."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -512, 511)")))

(define_constraint "K"
  "@internal"
  ;; Unused.
  (and (match_code "const_int")
       (match_test "0")))

(define_constraint "L"
  "A signed 16-bit immediate."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -32768, 32767)")))

(define_constraint "M"
  "An unsigned 16-bit immediate."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 0, 65535)")))

(define_constraint "N"
  "A signed 12-bit immediate that is negative."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -2048, -1)")))

(define_constraint "O"
  "Zero."
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "P"
  "A signed 12-bit immediate that is positive."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 1, 2047)")))

;; Floating-point constraints.
(define_constraint "G"
  "Floating-point zero."
  (and (match_code "const_double")
       (ior (and (match_test "mode == VOIDmode")
		 (match_test "hval == 0 && lval == 0"))
	    (and (match_test "mode == SFmode || mode == DFmode")
		 (match_test "op == CONST0_RTX (mode)")))))

(define_constraint "H"
  "@internal"
  ;; Unused.
  (and (match_code "const_double")
       (match_test "0")))

(define_constraint "Q"
  "12-bit relocations."
  (match_test "got12_operand (op, mode)"))

(define_memory_constraint "R"
  "Double word memory ops that take one instruction."
  (match_test "dbl_memory_one_insn_operand (op, mode)"))

(define_constraint "S"
  "SYMBOL_REF."
  (match_test "CONSTANT_P (op) && call_operand (op, VOIDmode)"))

(define_memory_constraint "T"
  "Double word memory ops that take two instructions."
  (match_test "dbl_memory_two_insn_operand (op, mode)"))

(define_memory_constraint "U"
  "Memory operand for conditional execution."
  (match_test "condexec_memory_operand (op, mode)"))
