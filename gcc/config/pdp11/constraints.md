;;- Constraint definitions for the pdp11 for GNU C compiler
;; Copyright (C) 2010-2020 Free Software Foundation, Inc.
;; Contributed by Michael K. Gschwind (mike@vlsivie.tuwien.ac.at).

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

(define_register_constraint "a" "LOAD_FPU_REGS"
  "FPU register that can be directly loaded from memory")

(define_register_constraint "f" "FPU_REGS"
  "Any FPU register")

(define_register_constraint "h" "NO_LOAD_FPU_REGS"
  "FPU register that cannot be directly loaded from memory")

(define_register_constraint "d" "MUL_REGS"
  "General register that can be used for 16-bit multiply (odd numbered)")

(define_constraint "I"
  "Integer constant that fits in 16 bits unsigned"
  (and (match_code "const_int")
       (match_test "(ival & 0xffff0000) == 0")))

(define_constraint "J"
  "Integer constant whose low 16 bits are zero"
  (and (match_code "const_int")
       (match_test "(ival & 0xffff) == 0")))

(define_constraint "K"
  "Integer constant whose lower and upper 16 bit half are both non-zero"
  (and (match_code "const_int")
       (match_test "(ival & 0xffff) != 0 && (ival & 0xffff0000) != 0")))

(define_constraint "L"
  "Integer constant 1"
  (and (match_code "const_int")
       (match_test "ival == 1")))

(define_constraint "M"
  "Integer constant -1"
  (and (match_code "const_int")
       (match_test "ival == -1")))

(define_constraint "N"
  "Integer constant 0"
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "O"
  "Integer constant for which several individual shifts are better than one big one"
  (and (match_code "const_int")
       (match_test "pdp11_small_shift (ival)")))

(define_constraint "G"
  "Defines a real zero constant."
  (and (match_code "const_double")
       (match_test "op == CONST0_RTX (GET_MODE (op))")))

(define_memory_constraint "Q"
  "Memory reference that requires an additional word after the opcode"
  (and (match_code "mem")
       (match_test "memory_address_p (GET_MODE (op), XEXP (op, 0))
                    && !simple_memory_operand (op, GET_MODE (op))")))

(define_memory_constraint "R"
  "Memory reference that is encoded within the opcode"
  (and (match_code "mem")
       (match_test "memory_address_p (GET_MODE (op), XEXP (op, 0))
                    && simple_memory_operand (op, GET_MODE (op))")))

(define_memory_constraint "D"
  "Memory reference that is encoded within the opcode, and not push or pop"
  (and (match_code "mem")
       (match_test "memory_address_p (GET_MODE (op), XEXP (op, 0))
                    && no_side_effect_operand (op, GET_MODE (op))")))

;; What follows is a set of constraints used to prevent the generation
;; of insns that have a register as source, and an auto-increment or
;; auto-decrement memory reference as the destination where the register
;; is the same as the source.  On the PDP11, such instructions are not
;; implemented consistently across the models and often do something
;; different from what the RTL intends.
(define_register_constraint "Z0" "NOTR0_REG" "Register other than 0")
(define_register_constraint "Z1" "NOTR1_REG" "Register other than 1")
(define_register_constraint "Z2" "NOTR2_REG" "Register other than 2")
(define_register_constraint "Z3" "NOTR3_REG" "Register other than 3")
(define_register_constraint "Z4" "NOTR4_REG" "Register other than 4")
(define_register_constraint "Z5" "NOTR5_REG" "Register other than 5")
(define_register_constraint "Z6" "NOTSP_REG"
  "Register other than stack pointer (register 6)")
(define_memory_constraint "Za" "R0 push/pop"
  (match_test "pushpop_regeq (op, 0)"))
(define_memory_constraint "Zb" "R1 push/pop"
  (match_test "pushpop_regeq (op, 1)"))
(define_memory_constraint "Zc" "R2 push/pop"
  (match_test "pushpop_regeq (op, 2)"))
(define_memory_constraint "Zd" "R3 push/pop"
  (match_test "pushpop_regeq (op, 3)"))
(define_memory_constraint "Ze" "R4 push/pop"
  (match_test "pushpop_regeq (op, 4)"))
(define_memory_constraint "Zf" "R5 push/pop"
  (match_test "pushpop_regeq (op, 5)"))
(define_memory_constraint "Zg" "SP push/pop"
  (match_test "pushpop_regeq (op, 6)"))
  
