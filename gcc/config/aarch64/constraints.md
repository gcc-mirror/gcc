;; Machine description for AArch64 architecture.
;; Copyright (C) 2009-2018 Free Software Foundation, Inc.
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

(define_register_constraint "Ucs" "TAILCALL_ADDR_REGS"
  "@internal Registers suitable for an indirect tail call")

(define_register_constraint "w" "FP_REGS"
  "Floating point and SIMD vector registers.")

(define_register_constraint "Upa" "PR_REGS"
  "SVE predicate registers p0 - p15.")

(define_register_constraint "Upl" "PR_LO_REGS"
  "SVE predicate registers p0 - p7.")

(define_register_constraint "x" "FP_LO_REGS"
  "Floating point and SIMD vector registers V0 - V15.")

(define_constraint "I"
 "A constant that can be used with an ADD operation."
 (and (match_code "const_int")
      (match_test "aarch64_uimm12_shift (ival)")))

(define_constraint "Uaa"
  "@internal A constant that matches two uses of add instructions."
  (and (match_code "const_int")
       (match_test "aarch64_pluslong_strict_immedate (op, VOIDmode)")))

(define_constraint "Uav"
  "@internal
   A constraint that matches a VG-based constant that can be added by
   a single ADDVL or ADDPL."
 (match_operand 0 "aarch64_sve_addvl_addpl_immediate"))

(define_constraint "Uat"
  "@internal
   A constraint that matches a VG-based constant that can be added by
   using multiple instructions, with one temporary register."
 (match_operand 0 "aarch64_split_add_offset_immediate"))

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

(define_constraint "Uti"
 "A constant that can be used with a 128-bit MOV immediate operation."
 (and (ior (match_code "const_int")
	   (match_code "const_wide_int"))
      (match_test "aarch64_mov128_immediate (op)")))

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

(define_constraint "Usa"
  "@internal
   A constraint that matches an absolute symbolic address that can be
   loaded by a single ADR."
  (and (match_code "const,symbol_ref,label_ref")
       (match_test "aarch64_symbolic_address_p (op)")
       (match_test "aarch64_mov_operand_p (op, GET_MODE (op))")))

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
       (match_test "!(aarch64_is_noplt_call_p (op)
		      || aarch64_is_long_call_p (op))")))

(define_constraint "Usg"
  "@internal
  A constraint that matches an immediate right shift constant in SImode
  suitable for a SISD instruction."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 1, 31)")))

(define_constraint "Usj"
  "@internal
  A constraint that matches an immediate right shift constant in DImode
  suitable for a SISD instruction."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, 1, 63)")))

(define_constraint "UsM"
  "@internal
  A constraint that matches the immediate constant -1."
  (match_test "op == constm1_rtx"))

(define_constraint "Usv"
  "@internal
   A constraint that matches a VG-based constant that can be loaded by
   a single CNT[BHWD]."
 (match_operand 0 "aarch64_sve_cnt_immediate"))

(define_constraint "Usi"
  "@internal
 A constraint that matches an immediate operand valid for
 the SVE INDEX instruction."
 (match_operand 0 "aarch64_sve_index_immediate"))

(define_constraint "Ui1"
  "@internal
  A constraint that matches the immediate constant +1."
  (match_test "op == const1_rtx"))

(define_constraint "Ui2"
  "@internal
  A constraint that matches the integers 0...3."
  (and (match_code "const_int")
       (match_test "(unsigned HOST_WIDE_INT) ival <= 3")))

(define_constraint "Ui3"
  "@internal
  A constraint that matches the integers 0...4."
  (and (match_code "const_int")
       (match_test "(unsigned HOST_WIDE_INT) ival <= 4")))

(define_constraint "Ui7"
  "@internal
  A constraint that matches the integers 0...7."
  (and (match_code "const_int")
       (match_test "(unsigned HOST_WIDE_INT) ival <= 7")))

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
						  true, ADDR_QUERY_LDP_STP)")))

;; Used for storing or loading pairs in an AdvSIMD register using an STP/LDP
;; as a vector-concat.  The address mode uses the same constraints as if it
;; were for a single value.
(define_memory_constraint "Umn"
  "@internal
  A memory address suitable for a load/store pair operation."
  (and (match_code "mem")
       (match_test "aarch64_legitimate_address_p (GET_MODE (op), XEXP (op, 0),
						  true,
						  ADDR_QUERY_LDP_STP_N)")))

(define_memory_constraint "Utr"
  "@internal
   An address valid for SVE LDR and STR instructions (as distinct from
   LD[1234] and ST[1234] patterns)."
  (and (match_code "mem")
       (match_test "aarch64_sve_ldr_operand_p (op)")))

(define_memory_constraint "Utv"
  "@internal
   An address valid for loading/storing opaque structure
   types wider than TImode."
  (and (match_code "mem")
       (match_test "aarch64_simd_mem_operand_p (op)")))

(define_memory_constraint "Utq"
  "@internal
   An address valid for loading or storing a 128-bit AdvSIMD register"
  (and (match_code "mem")
       (match_test "aarch64_legitimate_address_p (V2DImode,
						  XEXP (op, 0), 1)")))

(define_memory_constraint "Uty"
  "@internal
   An address valid for SVE LD1Rs."
  (and (match_code "mem")
       (match_test "aarch64_sve_ld1r_operand_p (op)")))

(define_memory_constraint "Utx"
  "@internal
   An address valid for SVE structure mov patterns (as distinct from
   LD[234] and ST[234] patterns)."
  (match_operand 0 "aarch64_sve_struct_memory_operand"))

(define_constraint "Ufc"
  "A floating point constant which can be used with an\
   FMOV immediate operation."
  (and (match_code "const_double")
       (match_test "aarch64_float_const_representable_p (op)")))

(define_constraint "Uvi"
  "A floating point constant which can be used with a\
   MOVI immediate operation."
  (and (match_code "const_double")
       (match_test "aarch64_can_const_movi_rtx_p (op, GET_MODE (op))")))

(define_constraint "Do"
  "@internal
   A constraint that matches vector of immediates for orr."
 (and (match_code "const_vector")
      (match_test "aarch64_simd_valid_immediate (op, NULL,
						 AARCH64_CHECK_ORR)")))

(define_constraint "Db"
  "@internal
   A constraint that matches vector of immediates for bic."
 (and (match_code "const_vector")
      (match_test "aarch64_simd_valid_immediate (op, NULL,
						 AARCH64_CHECK_BIC)")))

(define_constraint "Dn"
  "@internal
 A constraint that matches vector of immediates."
 (and (match_code "const,const_vector")
      (match_test "aarch64_simd_valid_immediate (op, NULL)")))

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
 (and (match_code "const,const_vector")
      (match_test "aarch64_simd_shift_imm_p (op, GET_MODE (op),
						 true)")))

(define_constraint "Dr"
  "@internal
 A constraint that matches vector of immediates for right shifts."
 (and (match_code "const,const_vector")
      (match_test "aarch64_simd_shift_imm_p (op, GET_MODE (op),
						 false)")))
(define_constraint "Dz"
  "@internal
 A constraint that matches a vector of immediate zero."
 (and (match_code "const,const_vector")
      (match_test "op == CONST0_RTX (GET_MODE (op))")))

(define_constraint "Dm"
  "@internal
 A constraint that matches a vector of immediate minus one."
 (and (match_code "const,const_vector")
      (match_test "op == CONST1_RTX (GET_MODE (op))")))

(define_constraint "Dd"
  "@internal
 A constraint that matches an integer immediate operand valid\
 for AdvSIMD scalar operations in DImode."
 (and (match_code "const_int")
      (match_test "aarch64_can_const_movi_rtx_p (op, DImode)")))

(define_constraint "Ds"
  "@internal
 A constraint that matches an integer immediate operand valid\
 for AdvSIMD scalar operations in SImode."
 (and (match_code "const_int")
      (match_test "aarch64_can_const_movi_rtx_p (op, SImode)")))

(define_address_constraint "Dp"
  "@internal
 An address valid for a prefetch instruction."
 (match_test "aarch64_address_valid_for_prefetch_p (op, true)"))

(define_constraint "vsa"
  "@internal
   A constraint that matches an immediate operand valid for SVE
   arithmetic instructions."
 (match_operand 0 "aarch64_sve_arith_immediate"))

(define_constraint "vsc"
  "@internal
   A constraint that matches a signed immediate operand valid for SVE
   CMP instructions."
 (match_operand 0 "aarch64_sve_cmp_vsc_immediate"))

(define_constraint "vsd"
  "@internal
   A constraint that matches an unsigned immediate operand valid for SVE
   CMP instructions."
 (match_operand 0 "aarch64_sve_cmp_vsd_immediate"))

(define_constraint "vsi"
  "@internal
   A constraint that matches a vector count operand valid for SVE INC and
   DEC instructions."
 (match_operand 0 "aarch64_sve_inc_dec_immediate"))

(define_constraint "vsn"
  "@internal
   A constraint that matches an immediate operand whose negative
   is valid for SVE SUB instructions."
 (match_operand 0 "aarch64_sve_sub_arith_immediate"))

(define_constraint "vsl"
  "@internal
   A constraint that matches an immediate operand valid for SVE logical
   operations."
 (match_operand 0 "aarch64_sve_logical_immediate"))

(define_constraint "vsm"
  "@internal
   A constraint that matches an immediate operand valid for SVE MUL
   operations."
 (match_operand 0 "aarch64_sve_mul_immediate"))

(define_constraint "vsA"
  "@internal
   A constraint that matches an immediate operand valid for SVE FADD
   and FSUB operations."
 (match_operand 0 "aarch64_sve_float_arith_immediate"))

(define_constraint "vsM"
  "@internal
   A constraint that matches an imediate operand valid for SVE FMUL
   operations."
 (match_operand 0 "aarch64_sve_float_mul_immediate"))

(define_constraint "vsN"
  "@internal
   A constraint that matches the negative of vsA"
 (match_operand 0 "aarch64_sve_float_arith_with_sub_immediate"))
