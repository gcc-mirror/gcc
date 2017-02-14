;; Machine description for AArch64 architecture.
;; Copyright (C) 2009-2017 Free Software Foundation, Inc.
;; Contributed by ARM Ltd.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_register_constraint "k" "STACK_REG"
  "@internal The stack register.")

(define_register_constraint "Ucs" "CALLER_SAVE_REGS"
  "@internal The caller save registers.")

(define_register_constraint "w" "FP_REGS"
  "Floating point and SIMD vector registers.")

(define_register_constraint "x" "FP_LO_REGS"
  "Floating point and SIMD vector registers V0 - V15.")

(define_constraint "I"
 "A constant that can be used with an ADD operation."
 (and (match_code "const_int")
      (match_test "aarch64_uimm12_shift (ival)")))

(define_constraint "Upl"
  "@internal A constant that matches two uses of add instructions."
  (and (match_code "const_int")
       (match_test "aarch64_pluslong_strict_immedate (op, VOIDmode)")))

(define_constraint "J"
 "A constant that can be used with a SUB operation (once negated)."
 (and (match_code "const_int")
      (match_test "aarch64_uimm12_shift (-ival)")))

;; We can't use the mode of a CONST_INT to determine the context in
;; which it is being used, so we must have a separate constraint for
;; each context.

(define_constraint "K"
 "A constant that can be used with a 32-bit logical operation."
 (and (match_code "const_int")
      (match_test "aarch64_bitmask_imm (ival, SImode)")))

(define_constraint "L"
 "A constant that can be used with a 64-bit logical operation."
 (and (match_code "const_int")
      (match_test "aarch64_bitmask_imm (ival, DImode)")))

(define_constraint "M"
 "A constant that can be used with a 32-bit MOV immediate operation."
 (and (match_code "const_int")
      (match_test "aarch64_move_imm (ival, SImode)")))

(define_constraint "N"
 "A constant that can be used with a 64-bit MOV immediate operation."
 (and (match_code "const_int")
      (match_test "aarch64_move_imm (ival, DImode)")))

(define_constraint "UsO"
 "A constant that can be used with a 32-bit and operation."
 (and (match_code "const_int")
      (match_test "aarch64_and_bitmask_imm (ival, SImode)")))

(define_constraint "UsP"
 "A constant that can be used with a 64-bit and operation."
 (and (match_code "const_int")
      (match_test "aarch64_and_bitmask_imm (ival, DImode)")))

(define_constraint "S"
  "A constraint that matches an absolute symbolic address."
  (and (match_code "const,symbol_ref,label_ref")
       (match_test "aarch64_symbolic_address_p (op)")))

(define_constraint "Y"
  "Floating point constant zero."
  (and (match_code "const_double")
       (match_test "aarch64_float_const_zero_rtx_p (op)")))

(define_constraint "Z"
  "Integer constant zero."
  (match_test "op == const0_rtx"))

(define_constraint "Ush"
  "A constraint that matches an absolute symbolic address high part."
  (and (match_code "high")
       (match_test "aarch64_valid_symref (XEXP (op, 0), GET_MODE (XEXP (op, 0)))")))

(define_constraint "Uss"
  "@internal
  A constraint that matches an immediate shift constant in SImode."
  (and (match_code "const_int")
       (match_test "(unsigned HOST_WIDE_INT) ival < 32")))

(define_constraint "Usn"
 "A constant that can be used with a CCMN operation (once negated)."
 (and (match_code "const_int")
      (match_test "IN_RANGE (ival, -31, 0)")))

(define_constraint "Usd"
  "@internal
  A constraint that matches an immediate shift constant in DImode."
  (and (match_code "const_int")
       (match_test "(unsigned HOST_WIDE_INT) ival < 64")))

(define_constraint "Usf"
  "@internal Usf is a symbol reference under the context where plt stub allowed."
  (and (match_code "symbol_ref")
       (match_test "!aarch64_is_noplt_call_p (op)")))

(define_constraint "UsM"
  "@internal
  A constraint that matches the immediate constant -1."
  (match_test "op == constm1_rtx"))

(define_constraint "Ui1"
  "@internal
  A constraint that matches the immediate constant +1."
  (match_test "op == const1_rtx"))

(define_constraint "Ui3"
  "@internal
  A constraint that matches the integers 0...4."
  (and (match_code "const_int")
       (match_test "(unsigned HOST_WIDE_INT) ival <= 4")))

(define_constraint "Up3"
  "@internal
  A constraint that matches the integers 2^(0...4)."
  (and (match_code "const_int")
       (match_test "(unsigned) exact_log2 (ival) <= 4")))

(define_memory_constraint "Q"
 "A memory address which uses a single base register with no offset."
 (and (match_code "mem")
      (match_test "REG_P (XEXP (op, 0))")))

(define_memory_constraint "Ump"
  "@internal
  A memory address suitable for a load/store pair operation."
  (and (match_code "mem")
       (match_test "aarch64_legitimate_address_p (GET_MODE (op), XEXP (op, 0),
						  PARALLEL, 1)")))

(define_memory_constraint "Utv"
  "@internal
   An address valid for loading/storing opaque structure
   types wider than TImode."
  (and (match_code "mem")
       (match_test "aarch64_simd_mem_operand_p (op)")))

(define_constraint "Ufc"
  "A floating point constant which can be used with an\
   FMOV immediate operation."
  (and (match_code "const_double")
       (match_test "aarch64_float_const_representable_p (op)")))

(define_constraint "Dn"
  "@internal
 A constraint that matches vector of immediates."
 (and (match_code "const_vector")
      (match_test "aarch64_simd_valid_immediate (op, GET_MODE (op),
						 false, NULL)")))

(define_constraint "Dh"
  "@internal
 A constraint that matches an immediate operand valid for\
 AdvSIMD scalar move in HImode."
 (and (match_code "const_int")
      (match_test "aarch64_simd_scalar_immediate_valid_for_move (op,
						 HImode)")))

(define_constraint "Dq"
  "@internal
 A constraint that matches an immediate operand valid for\
 AdvSIMD scalar move in QImode."
 (and (match_code "const_int")
      (match_test "aarch64_simd_scalar_immediate_valid_for_move (op,
						 QImode)")))

(define_constraint "Dl"
  "@internal
 A constraint that matches vector of immediates for left shifts."
 (and (match_code "const_vector")
      (match_test "aarch64_simd_shift_imm_p (op, GET_MODE (op),
						 true)")))

(define_constraint "Dr"
  "@internal
 A constraint that matches vector of immediates for right shifts."
 (and (match_code "const_vector")
      (match_test "aarch64_simd_shift_imm_p (op, GET_MODE (op),
						 false)")))
(define_constraint "Dz"
  "@internal
 A constraint that matches vector of immediate zero."
 (and (match_code "const_vector")
      (match_test "aarch64_simd_imm_zero_p (op, GET_MODE (op))")))

(define_constraint "Dd"
  "@internal
 A constraint that matches an immediate operand valid for AdvSIMD scalar."
 (and (match_code "const_int")
      (match_test "aarch64_simd_imm_scalar_p (op, GET_MODE (op))")))
