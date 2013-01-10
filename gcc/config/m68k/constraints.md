;; Constraint definitions for m68k
;; Copyright (C) 2007-2013 Free Software Foundation, Inc.

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

(define_register_constraint "a" "ADDR_REGS"
  "Address register.")

(define_register_constraint "d" "DATA_REGS"
  "Data register.")

(define_register_constraint "f" "TARGET_HARD_FLOAT ? FP_REGS : NO_REGS"
  "Floating point register.")

(define_constraint "I"
  "Integer constant in the range 1 @dots 8, for immediate shift counts and addq."
  (and (match_code "const_int")
       (match_test "ival > 0 && ival <= 8")))

(define_constraint "J"
  "Signed 16-bit integer constant."
  (and (match_code "const_int")
       (match_test "ival >= -0x8000 && ival <= 0x7fff")))

(define_constraint "K"
  "Integer constant that moveq can't handle."
  (and (match_code "const_int")
       (match_test "ival < -0x80 || ival >= 0x80")))

(define_constraint "L"
  "Integer constant in the range -8 @dots -1, for subq."
  (and (match_code "const_int")
       (match_test "ival < 0 && ival >= -8")))

(define_constraint "M"
  "Integer constant that moveq+notb can't handle."
  (and (match_code "const_int")
       (match_test "ival < -0x100 || ival >= 0x100")))

(define_constraint "N"
  "Integer constant in the range 24 @dots 31, for rotatert:SI 8 to 1 expressed as rotate."
  (and (match_code "const_int")
       (match_test "ival >= 24 && ival <= 31")))

(define_constraint "O"
  "Integer constant 16, for rotate using swap."
  (and (match_code "const_int")
       (match_test "ival == 16")))

(define_constraint "P"
  "Integer constant in the range 8 @dots 15, for rotatert:HI 8 to 1 expressed as rotate."
  (and (match_code "const_int")
       (match_test "ival >= 8 && ival <= 15")))

(define_constraint "R"
  "Integer constant that mov3q can handle."
  (and (match_code "const_int")
       (match_test "valid_mov3q_const (ival)")))

(define_constraint "G"
  "Defines all of the floating constants that are *NOT* 68881
   constants.  This is so 68881 constants get reloaded and the fpmovecr
   is used."
  (and (match_code "const_double")
       (match_test "!(TARGET_68881 && standard_68881_constant_p (op))")))

(define_constraint "H"
  "Defines a real zero constant."
  (and (match_code "const_double")
       (match_test "op == CONST0_RTX (GET_MODE (op))")))

(define_constraint "S"
  "Used for operands that satisfy 'm' when -mpcrel is in effect."
  (and (match_code "mem")
       (match_test "TARGET_PCREL
		    && (GET_CODE (XEXP (op, 0)) == SYMBOL_REF
			|| GET_CODE (XEXP (op, 0)) == LABEL_REF
			|| GET_CODE (XEXP (op, 0)) == CONST)")))

(define_constraint "T"
  "Used for operands that satisfy 's' when -mpcrel is not in effect."
  (and (match_code "symbol_ref,label_ref,const")
       (match_test "!TARGET_PCREL")
       (match_test "!flag_pic || LEGITIMATE_PIC_OPERAND_P (op)")))

(define_memory_constraint "Q"
  "Means address register indirect addressing mode."
  (and (match_code "mem")
       (match_test "m68k_matches_q_p (op)")))

(define_constraint "U"
  "Used for register offset addressing."
  (and (match_code "mem")
       (match_test "m68k_matches_u_p (op)")))

(define_constraint "W"
  "Used for const_call_operands."
  (match_operand 0 "const_call_operand"))

(define_constraint "Cs"
  "symbol_ref or const."
  (match_code "symbol_ref,const"))

(define_constraint "Ci"
  "const_int."
  (and (match_code "const_int")
       (match_test "true")))

(define_constraint "C0"
  "const_int 0."
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "Cj"
  "Range of signed numbers that don't fit in 16 bits."
  (and (match_code "const_int")
       (match_test "ival < -0x8000 || ival > 0x7FFF")))

(define_constraint "Cu"
  "16-bit offset for wrapped symbols"
  (and (match_code "const")
       (match_test "m68k_unwrap_symbol (op, false) != op")))

(define_constraint "CQ"
  "Integers valid for mvq."
  (and (match_code "const_int")
       (match_test "m68k_const_method (ival) == MOVQ")))

(define_constraint "CW"
  "Integers valid for a moveq followed by a swap."
  (and (match_code "const_int")
       (match_test "m68k_const_method (ival) == SWAP")))

(define_constraint "CZ"
  "Integers valid for mvz."
  (and (match_code "const_int")
       (match_test "m68k_const_method (ival) == MVZ")))

(define_constraint "CS"
  "Integers valid for mvs."
  (and (match_code "const_int")
       (match_test "m68k_const_method (ival) == MVS")))

(define_constraint "Ap"
  "push_operand."
  (match_operand 0 "push_operand"))

(define_constraint "Ac"
  "Non-register operands allowed in clr."
  (and (match_operand 0 "movsi_const0_operand")
       (match_test "!REG_P (op)")))
