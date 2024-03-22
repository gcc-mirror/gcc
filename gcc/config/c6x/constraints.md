;; Constraint definitions for TI C6X.
;; Copyright (C) 2010-2024 Free Software Foundation, Inc.
;; Contributed by Andrew Jenner <andrew@codesourcery.com>
;; Contributed by Bernd Schmidt <bernds@codesourcery.com>
;; Contributed by CodeSourcery.
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

(define_register_constraint "a" "A_REGS"
  "Register file A (A0--A31).")

(define_register_constraint "b" "B_REGS"
  "Register file B (B0--B31).")

(define_register_constraint "A" "PREDICATE_A_REGS"
  "Predicate registers in register file A (A0--A2 on C64X and higher,
   A1 and A2 otherwise).")

(define_register_constraint "B" "PREDICATE_B_REGS"
  "Predicate registers in register file B (B0--B2).")

(define_register_constraint "C" "CALL_USED_B_REGS"
  "A call-used register in register file B (B0--B9, B16--B31).")

(define_register_constraint "Da" "NONPREDICATE_A_REGS"
  "Register file A, excluding predicate registers (A3--A31, plus A0 if
not C64X or higher).")

(define_register_constraint "Db" "NONPREDICATE_B_REGS"
  "Register file B, excluding predicate registers (B3--B31).")

(define_register_constraint "Z" "PICREG"
  "Register B14 (aka DP).")

(define_register_constraint "z" "SPREG"
  "Register B15 (aka SP).")

(define_constraint "Iu4"
  "Integer constant in the range 0 @dots{} 15, aka ucst4."
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival <= 15")))

(define_constraint "Iu5"
  "Integer constant in the range 0 @dots{} 31, aka ucst5."
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival <= 31")))

(define_constraint "In5"
  "Integer constant in the range @minus{}31 @dots{} 0, negation of ucst5."
  (and (match_code "const_int")
       (match_test "ival >= -31 && ival <= 0")))

(define_constraint "Is5"
  "Integer constant in the range @minus{}16 @dots{} 15, aka scst5."
  (and (match_code "const_int")
       (match_test "ival >= -16 && ival <= 15")))

(define_constraint "I5x"
  "Integer constant that can be the operand of an ADDA or a SUBA insn."
  (and (match_code "const_int")
       (match_test "(ival >= -31 && ival <= 31)
		    || ((ival & 1) == 0 && ival >= -62 && ival <= 62)
		    || ((ival & 3) == 0 && ival >= -124 && ival <= 124)
		    || ((TARGET_INSNS_64 || TARGET_INSNS_67)
 			&& (ival & 7) == 0 && ival > 0 && ival <= 248)")))

(define_constraint "Iux"
  "Integer constant that can be the operand of a long ADDA or a SUBA insn,
   i.e. one involving B14 or B15 as source operand."
  (and (match_code "const_int")
       (and (match_test "TARGET_INSNS_64PLUS")
	    (match_test "ival >= 0
 			 && (ival < 32768
		     	     || ((ival & 1) == 0 && ival < 65536)
			     || ((ival & 3) == 0 && ival < 131072))"))))

(define_constraint "IuB"
  "Integer constant in the range 0 @dots{} 65535, aka ucst16."
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival <= 65535")))

(define_constraint "IsB"
  "Integer constant in the range @minus{}32768 @dots{} 32767."
  (and (match_code "const_int")
       (match_test "ival >= -32768 && ival <= 32767")))

(define_constraint "IsC"
  "Integer constant in the range @math{-2^{20}} @dots{} @math{2^{20} - 1}."
  (and (match_code "const_int")
       (match_test "ival >= -0x100000 && ival <= 0xfffff")))

(define_constraint "JA"
  "@internal
   Integer constant in the range 0 @dots{} 31, corresponding to an A register
   number."
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival < 32")))

(define_constraint "JB"
  "@internal
   Integer constant in the range 32 @dots{} 63, corresponding to a B register
   number."
  (and (match_code "const_int")
       (match_test "ival >= 32 && ival < 64")))

(define_constraint "Jc"
  "Integer constant that is a valid mask for the clr instruction"
  (and (match_code "const_int")
       (match_test "c6x_valid_mask_p (ival)")))

(define_constraint "Js"
  "Integer constant that is a valid mask for the set instruction"
  (and (match_code "const_int")
       (match_test "c6x_valid_mask_p (~ival)")))

(define_memory_constraint "Q"
  "Memory location with A base register."
  (and (match_code "mem")
       (match_test "c6x_mem_operand (op, A_REGS, false)")))

(define_memory_constraint "R"
  "Memory location with B base register."
  (and (match_code "mem")
       (match_test "c6x_mem_operand (op, B_REGS, false)")))

(define_memory_constraint "T"
  "@internal
   Memory location with B base register, but not using a long offset."
  (and (match_code "mem")
       (match_test "c6x_mem_operand (op, B_REGS, true)")))

(define_constraint "S0"
  "@internal
   On C64x+ targets, a GP-relative small data reference"
  (and (match_test "TARGET_INSNS_64PLUS")
       (match_operand 0 "sdata_symbolic_operand")))

(define_constraint "S1"
  "@internal
   Any kind of @code{SYMBOL_REF}, for use in a call address."
  (and (match_code "symbol_ref")
       (match_operand 0 "c6x_call_operand")))

(define_constraint "S2"
  "@internal
   Any SYMBOL_REF or LABEL_REF."
  (ior (match_code "symbol_ref") (match_code "label_ref")))

(define_constraint "S3"
  "Matches a symbolic integer constant, even if invalid for PIC."
  (and (match_test "CONSTANT_P (op)")
       (match_test "!CONST_SCALAR_INT_P (op)")))

(define_constraint "Si"
  "@internal
   Any immediate value, unless it matches the S0 constraint."
  (and (match_operand 0 "immediate_operand")
       (match_test "!satisfies_constraint_S0 (op)")))

(define_memory_constraint "W"
  "@internal
   A memory operand with an address that can't be used in an unaligned access."
  (and (match_code "mem")
       (match_test "!c6x_legitimate_address_p_1 (GET_MODE (op), XEXP (op, 0),
 						 reload_completed, true)")))
