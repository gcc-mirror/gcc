;; Constraint definitions for Xtensa.
;; Copyright (C) 2006-2014 Free Software Foundation, Inc.
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

(define_register_constraint "a" "GR_REGS"
 "General-purpose AR registers @code{a0}-@code{a15},
  except @code{a1} (@code{sp}).")

(define_register_constraint "b" "TARGET_BOOLEANS ? BR_REGS : NO_REGS"
 "Boolean registers @code{b0}-@code{b15}; only available if the Xtensa
  Boolean Option is configured.")

(define_register_constraint "d" "TARGET_DENSITY ? AR_REGS: NO_REGS"
 "@internal
  All AR registers, including sp, but only if the Xtensa Code Density
  Option is configured.")

(define_register_constraint "f" "TARGET_HARD_FLOAT ? FP_REGS : NO_REGS"
 "Floating-point registers @code{f0}-@code{f15}; only available if the
  Xtensa Floating-Pointer Coprocessor is configured.")

(define_register_constraint "q" "SP_REG"
 "@internal
  The stack pointer (register @code{a1}).")

(define_register_constraint "A" "TARGET_MAC16 ? ACC_REG : NO_REGS"
 "The low 32 bits of the accumulator from the Xtensa MAC16 Option.")

(define_register_constraint "B" "TARGET_SEXT ? GR_REGS : NO_REGS"
 "@internal
  General-purpose AR registers, but only if the Xtensa Sign Extend
  Option is configured.")

(define_register_constraint "C" "TARGET_MUL16 ? GR_REGS: NO_REGS"
 "@internal
  General-purpose AR registers, but only if the Xtensa 16-Bit Integer
  Multiply Option is configured.")

(define_register_constraint "D" "TARGET_DENSITY ? GR_REGS: NO_REGS"
 "@internal
  General-purpose AR registers, but only if the Xtensa Code Density
  Option is configured.")

(define_register_constraint "W" "TARGET_CONST16 ? GR_REGS: NO_REGS"
 "@internal
  General-purpose AR registers, but only if the Xtensa Const16
  Option is configured.")

;; Integer constant constraints.

(define_constraint "I"
 "A signed 12-bit integer constant for use with MOVI instructions."
 (and (match_code "const_int")
      (match_test "xtensa_simm12b (ival)")))

(define_constraint "J"
 "A signed 8-bit integer constant for use with ADDI instructions."
 (and (match_code "const_int")
      (match_test "xtensa_simm8 (ival)")))

(define_constraint "K"
 "A constant integer that can be an immediate operand of an Xtensa
  conditional branch instruction that performs a signed comparison or
  a comparison against zero."
 (and (match_code "const_int")
      (match_test "xtensa_b4const_or_zero (ival)")))

(define_constraint "L"
 "A constant integer that can be an immediate operand of an Xtensa
  conditional branch instruction that performs an unsigned comparison."
 (and (match_code "const_int")
      (match_test "xtensa_b4constu (ival)")))

(define_constraint "M"
 "An integer constant in the range @minus{}32-95 for use with MOVI.N
  instructions."
 (and (match_code "const_int")
      (match_test "ival >= -32 && ival <= 95")))

(define_constraint "N"
 "An unsigned 8-bit integer constant shifted left by 8 bits for use
  with ADDMI instructions."
 (and (match_code "const_int")
      (match_test "xtensa_simm8x256 (ival)")))

(define_constraint "O"
 "An integer constant that can be used in ADDI.N instructions."
 (and (match_code "const_int")
      (match_test "ival == -1 || (ival >= 1 && ival <= 15)")))

(define_constraint "P"
 "An integer constant that can be used as a mask value in an EXTUI
  instruction."
 (and (match_code "const_int")
      (match_test "xtensa_mask_immediate (ival)")))

;; Memory constraints.  Do not use define_memory_constraint here.  Doing so
;; causes reload to force some constants into the constant pool, but since
;; the Xtensa constant pool can only be accessed with L32R instructions, it
;; is always better to just copy a constant into a register.  Instead, use
;; regular constraints but add a check to allow pseudos during reload.

(define_constraint "R"
 "Memory that can be accessed with a 4-bit unsigned offset from a register."
 (ior (and (match_code "mem")
	   (match_test "smalloffset_mem_p (op)"))
      (and (match_code "reg")
	   (match_test "reload_in_progress
			&& REGNO (op) >= FIRST_PSEUDO_REGISTER"))))

(define_constraint "T"
 "Memory in a literal pool (addressable with an L32R instruction)."
 (and (match_code "mem")
      (match_test "!TARGET_CONST16 && constantpool_mem_p (op)")))

(define_constraint "U"
 "Memory that is not in a literal pool."
 (ior (and (match_code "mem")
	   (match_test "! constantpool_mem_p (op)"))
      (and (match_code "reg")
	   (match_test "reload_in_progress
			&& REGNO (op) >= FIRST_PSEUDO_REGISTER"))))
