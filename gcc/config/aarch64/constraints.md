;; Machine description for AArch64 architecture.
;; Copyright (C) 2009-2024 Free Software Foundation, Inc.
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

(define_register_constraint "Uci" "W8_W11_REGS"
  "@internal r8-r11, which can be used to index ZA.")

(define_register_constraint "Ucj" "W12_W15_REGS"
  "@internal r12-r15, which can be used to index ZA.")

(define_register_constraint "Ucs" "TAILCALL_ADDR_REGS"
  "@internal Registers suitable for an indirect tail call")

(define_register_constraint "Ucr"
    "aarch64_harden_sls_blr_p () ? STUB_REGS : GENERAL_REGS"
  "@internal Registers to be used for an indirect call.
   This is usually the general registers, but when we are hardening against
   Straight Line Speculation we disallow x16, x17, and x30 so we can use
   indirection stubs.  These indirection stubs cannot use the above registers
   since they will be reached by a BL that may have to go through a linker
   veneer.")

(define_register_constraint "w" "FP_REGS"
  "Floating point and SIMD vector registers.")

(define_register_constraint "x" "FP_LO_REGS"
  "Floating point and SIMD vector registers V0 - V15.")

(define_register_constraint "y" "FP_LO8_REGS"
  "Floating point and SIMD vector registers V0 - V7.")

(define_register_constraint "Uw2" "FP_REGS"
  "Even floating point and SIMD vector registers."
  "regno % 2 == 0")

(define_register_constraint "Uw4" "FP_REGS"
  "4-tuple-aligned floating point and SIMD vector registers."
  "regno % 4 == 0")

(define_register_constraint "Uwd" "FP_REGS"
  "@internal The first register in a tuple of 2 strided FPRs."
  "(regno & 0x8) == 0")

(define_register_constraint "Uwt" "FP_REGS"
  "@internal The first register in a tuple of 4 strided FPRs."
  "(regno & 0xc) == 0")

(define_register_constraint "Upa" "PR_REGS"
  "SVE predicate registers p0 - p15.")

(define_register_constraint "Up2" "PR_REGS"
  "An even SVE predicate register, p0 - p14."
  "regno % 2 == 0")

(define_register_constraint "Upl" "PR_LO_REGS"
  "SVE predicate registers p0 - p7.")

(define_register_constraint "Uph" "PR_HI_REGS"
  "SVE predicate registers p8 - p15.")

(define_register_constraint "Umv" "MOVEABLE_SYSREGS"
  "@internal System Registers suitable for moving rather than requiring an unspec msr")

(define_constraint "c"
 "@internal The condition code register."
  (match_operand 0 "cc_register"))

(define_constraint "I"
 "A constant that can be used with an ADD operation."
 (and (match_code "const_int")
      (match_test "aarch64_uimm12_shift (ival)")))

(define_constraint "Uaa"
  "@internal A constant that matches two uses of add instructions."
  (and (match_code "const_int")
       (match_test "aarch64_pluslong_strict_immedate (op, VOIDmode)")))

(define_constraint "Uai"
  "@internal
   A constraint that matches a VG-based constant that can be added by
   a single INC or DEC."
  (match_operand 0 "aarch64_sve_scalar_inc_dec_immediate"))

(define_constraint "Uav"
  "@internal
   A constraint that matches a VG-based constant that can be added by
   a single ADDVL or ADDPL."
 (match_operand 0 "aarch64_sve_addvl_addpl_immediate"))

(define_constraint "UaV"
  "@internal
   A constraint that matches a VG-based constant that can be added by
   a single ADDSVL or ADDSPL."
 (match_operand 0 "aarch64_addsvl_addspl_immediate"))

(define_constraint "Uat"
  "@internal
   A constraint that matches a VG-based constant that can be added by
   using multiple instructions, with one temporary register."
 (match_operand 0 "aarch64_split_add_offset_immediate"))

(define_constraint "J"
 "A constant that can be used with a SUB operation (once negated)."
 (and (match_code "const_int")
      (match_test "aarch64_uimm12_shift (- (unsigned HOST_WIDE_INT) ival)")))

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
      (match_test "aarch64_is_mov_xn_imm (ival)")))

(define_constraint "O"
 "A constant that can be used with a 32 or 64-bit MOV immediate operation."
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
  "Integer or floating-point constant zero."
  (match_test "op == CONST0_RTX (GET_MODE (op))"))

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

(define_constraint "Usm"
 "A constant that can be used with the S[MIN/MAX] CSSC instructions."
 (and (match_code "const_int")
      (match_test "aarch64_sminmax_immediate (op, VOIDmode)")))

;; const is needed here to support UNSPEC_SALT_ADDR.
(define_constraint "Usw"
  "@internal
   A constraint that matches a small GOT access."
  (and (match_code "const,symbol_ref")
       (match_test "aarch64_classify_symbolic_expression (op)
		     == SYMBOL_SMALL_GOT_4G")))

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

(define_constraint "Ulc"
 "@internal
 A constraint that matches a constant integer whose bits are consecutive ones
 from the MSB."
 (and (match_code "const_int")
      (match_test "aarch64_high_bits_all_ones_p (ival)")))

(define_constraint "Usr"
  "@internal
   A constraint that matches a value produced by RDVL."
 (and (match_code "const_poly_int")
      (match_test "aarch64_sve_rdvl_immediate_p (op)")))

(define_constraint "UsR"
  "@internal
   A constraint that matches a value produced by RDSVL."
 (and (match_code "const")
      (match_test "aarch64_rdsvl_immediate_p (op)")))

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

(define_constraint "Uih"
  "@internal
  A constraint that matches HImode integers zero extendable to
  SImode plus_operand."
  (and (match_code "const_int")
       (match_test "aarch64_plushi_immediate (op, VOIDmode)")))

(define_memory_constraint "Q"
 "A memory address which uses a single base register with no offset."
 (and (match_code "mem")
      (match_test "REG_P (XEXP (op, 0))")))

(define_memory_constraint "Ust"
  "@internal
  A memory address with 9bit unscaled offset."
  (match_operand 0 "aarch64_9bit_offset_memory_operand"))

(define_memory_constraint "Ump"
  "@internal
  A memory address suitable for a load/store pair operation."
  (and (match_code "mem")
       (match_test "aarch64_legitimate_address_p (GET_MODE (op), XEXP (op, 0),
						  true, ADDR_QUERY_LDP_STP)")))

;; Used for storing or loading pairs in an AdvSIMD register using an STP/LDP
;; as a vector-concat.  The address mode uses the same constraints as if it
;; were for a single value.
(define_relaxed_memory_constraint "Umn"
  "@internal
  A memory address suitable for a load/store pair operation."
  (and (match_code "mem")
       (match_test "aarch64_legitimate_address_p (GET_MODE (op), XEXP (op, 0),
						  true,
						  ADDR_QUERY_LDP_STP_N)")))

(define_address_constraint "UPb"
  "@internal
   An address valid for SVE PRFB instructions."
  (match_test "aarch64_sve_prefetch_operand_p (op, VNx16QImode)"))

(define_address_constraint "UPd"
  "@internal
   An address valid for SVE PRFD instructions."
  (match_test "aarch64_sve_prefetch_operand_p (op, VNx2DImode)"))

(define_address_constraint "UPh"
  "@internal
   An address valid for SVE PRFH instructions."
  (match_test "aarch64_sve_prefetch_operand_p (op, VNx8HImode)"))

(define_address_constraint "UPw"
  "@internal
   An address valid for SVE PRFW instructions."
  (match_test "aarch64_sve_prefetch_operand_p (op, VNx4SImode)"))

(define_memory_constraint "Utf"
  "@internal
   An address valid for SVE LDFF1 instructions."
  (and (match_code "mem")
       (match_test "aarch64_sve_ldff1_operand_p (op)")))

(define_memory_constraint "Utn"
  "@internal
   An address valid for SVE LDNF1 instructions."
  (and (match_code "mem")
       (match_test "aarch64_sve_ldnf1_operand_p (op)")))

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

(define_relaxed_memory_constraint "Utq"
  "@internal
   An address valid for loading or storing a 128-bit AdvSIMD register"
  (and (match_code "mem")
       (match_test "aarch64_legitimate_address_p (GET_MODE (op),
						  XEXP (op, 0), 1)")
       (match_test "aarch64_legitimate_address_p (V2DImode,
						  XEXP (op, 0), 1)")))

(define_relaxed_memory_constraint "UtQ"
  "@internal
   An address valid for SVE LD1RQs."
  (and (match_code "mem")
       (match_test "aarch64_sve_ld1rq_operand_p (op)")))

(define_relaxed_memory_constraint "UOb"
  "@internal
   An address valid for SVE LD1ROH."
  (and (match_code "mem")
       (match_test "aarch64_sve_ld1ro_operand_p (op, QImode)")))

(define_relaxed_memory_constraint "UOh"
  "@internal
   An address valid for SVE LD1ROH."
  (and (match_code "mem")
       (match_test "aarch64_sve_ld1ro_operand_p (op, HImode)")))


(define_relaxed_memory_constraint "UOw"
  "@internal
   An address valid for SVE LD1ROW."
  (and (match_code "mem")
       (match_test "aarch64_sve_ld1ro_operand_p (op, SImode)")))

(define_relaxed_memory_constraint "UOd"
  "@internal
   An address valid for SVE LD1ROD."
  (and (match_code "mem")
       (match_test "aarch64_sve_ld1ro_operand_p (op, DImode)")))

(define_relaxed_memory_constraint "Uty"
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
  (and (match_code "const_double,const_vector")
       (match_test "aarch64_float_const_representable_p (op)")))

(define_constraint "Uum"
 "A constant that can be used with the U[MIN/MAX] CSSC instructions."
 (and (match_code "const_int")
      (match_test "aarch64_uminmax_immediate (op, VOIDmode)")))

(define_constraint "Uvi"
  "A floating point constant which can be used with a\
   MOVI immediate operation."
  (and (match_code "const_double")
       (match_test "aarch64_can_const_movi_rtx_p (op, GET_MODE (op))")))

(define_constraint "Do"
  "@internal
   A constraint that matches vector of immediates for orr."
 (and (match_code "const_vector")
      (match_test "aarch64_simd_valid_orr_imm (op)")))

(define_constraint "Db"
  "@internal
   A constraint that matches vector of immediates for and/bic."
 (and (match_code "const_vector")
      (match_test "aarch64_simd_valid_and_imm (op)")))

(define_constraint "Dn"
  "@internal
 A constraint that matches vector of immediates."
 (and (match_code "const,const_vector")
      (match_test "aarch64_simd_valid_mov_imm (op)")))

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

(define_constraint "Dt"
  "@internal
 A const_double which is the reciprocal of an exact power of two, can be
 used in an scvtf with fract bits operation"
 (and (match_code "const_double")
      (match_test "aarch64_fpconst_pow2_recip (op) > 0")))

(define_constraint "Dl"
  "@internal
 A constraint that matches vector of immediates for left shifts."
 (and (match_code "const,const_vector")
      (match_test "aarch64_simd_shift_imm_p (op, GET_MODE (op),
						 true)")))

(define_constraint "D1"
  "@internal
 A constraint that matches vector of immediates that is bits(mode)-1."
 (and (match_code "const,const_vector")
      (match_test "aarch64_const_vec_all_same_in_range_p (op,
			GET_MODE_UNIT_BITSIZE (mode) - 1,
			GET_MODE_UNIT_BITSIZE (mode) - 1)")))

(define_constraint "D2"
  "@internal
 A constraint that matches vector of immediates that is bits(mode)/2."
 (and (match_code "const,const_vector")
      (match_test "aarch64_simd_shift_imm_vec_exact_top (op, mode)")))

(define_constraint "DL"
  "@internal
 A constraint that matches vector of immediates for left shift long.
 That is immediates between 0 to (bits(mode)/2)-1."
 (and (match_code "const,const_vector")
      (match_test "aarch64_const_vec_all_same_in_range_p (op, 0,
			(GET_MODE_UNIT_BITSIZE (mode) / 2) - 1)")))

(define_constraint "Dr"
  "@internal
 A constraint that matches vector of immediates for right shifts."
 (and (match_code "const,const_vector")
      (match_test "aarch64_simd_shift_imm_p (op, GET_MODE (op),
						 false)")))

(define_constraint "Dx"
  "@internal
 A constraint that matches a vector of 64-bit immediates which we don't have a
 single instruction to create but that we can create in creative ways."
 (and (match_code "const_int,const,const_vector")
      (match_test "aarch64_simd_special_constant_p (op, DImode)")))

(define_constraint "Dz"
  "@internal
 A constraint that matches a vector of immediate zero."
 (and (match_code "const,const_vector")
      (match_test "op == CONST0_RTX (GET_MODE (op))")))

(define_constraint "Dm"
  "@internal
 A constraint that matches a vector of immediate minus one."
 (and (match_code "const,const_vector")
      (match_test "op == CONSTM1_RTX (GET_MODE (op))")))

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

(define_constraint "vgb"
  "@internal
   A constraint that matches an immediate offset valid for SVE LD1B
   gather instructions."
 (match_operand 0 "aarch64_sve_gather_immediate_b"))

(define_constraint "vgd"
  "@internal
   A constraint that matches an immediate offset valid for SVE LD1D
   gather instructions."
 (match_operand 0 "aarch64_sve_gather_immediate_d"))

(define_constraint "vgh"
  "@internal
   A constraint that matches an immediate offset valid for SVE LD1H
   gather instructions."
 (match_operand 0 "aarch64_sve_gather_immediate_h"))

(define_constraint "vgw"
  "@internal
   A constraint that matches an immediate offset valid for SVE LD1W
   gather instructions."
 (match_operand 0 "aarch64_sve_gather_immediate_w"))

(define_constraint "vsa"
  "@internal
   A constraint that matches an immediate operand valid for SVE
   arithmetic instructions."
 (match_operand 0 "aarch64_sve_arith_immediate"))

(define_constraint "vsb"
  "@internal
   A constraint that matches an immediate operand valid for SVE UMAX
   and UMIN operations."
 (match_operand 0 "aarch64_sve_vsb_immediate"))

(define_constraint "vsc"
  "@internal
   A constraint that matches a signed immediate operand valid for SVE
   CMP instructions."
 (match_operand 0 "aarch64_sve_cmp_vsc_immediate"))

(define_constraint "vss"
  "@internal
   A constraint that matches a signed immediate operand valid for SVE
   DUP instructions."
 (match_test "aarch64_sve_dup_immediate_p (op)"))

(define_constraint "vsd"
  "@internal
   A constraint that matches an unsigned immediate operand valid for SVE
   CMP instructions."
 (match_operand 0 "aarch64_sve_cmp_vsd_immediate"))

(define_constraint "vsi"
  "@internal
   A constraint that matches a vector count operand valid for SVE INC and
   DEC instructions."
 (match_operand 0 "aarch64_sve_vector_inc_dec_immediate"))

(define_constraint "vsn"
  "@internal
   A constraint that matches an immediate operand whose negative
   is valid for SVE SUB instructions."
 (match_operand 0 "aarch64_sve_sub_arith_immediate"))

(define_constraint "vsQ"
  "@internal
   Like vsa, but additionally check that the immediate is nonnegative
   when interpreted as a signed value."
 (match_operand 0 "aarch64_sve_qadd_immediate"))

(define_constraint "vsS"
  "@internal
   Like vsn, but additionally check that the immediate is negative
   when interpreted as a signed value."
 (match_operand 0 "aarch64_sve_qsub_immediate"))

(define_constraint "vsl"
  "@internal
   A constraint that matches an immediate operand valid for SVE logical
   operations."
 (match_operand 0 "aarch64_sve_logical_immediate"))

(define_constraint "vsm"
  "@internal
   A constraint that matches an immediate operand valid for SVE MUL,
   SMAX and SMIN operations."
 (match_operand 0 "aarch64_sve_vsm_immediate"))

(define_constraint "vs1"
  "@internal
 A constraint that matches a vector of immediate one."
 (and (match_code "const,const_vector")
      (match_test "op == CONST1_RTX (GET_MODE (op))")))

(define_constraint "vsA"
  "@internal
   A constraint that matches an immediate operand valid for SVE FADD
   and FSUB operations."
 (match_operand 0 "aarch64_sve_float_arith_immediate"))

;; "B" for "bound".
(define_constraint "vsB"
  "@internal
   A constraint that matches an immediate operand valid for SVE FMAX
   and FMIN operations."
 (match_operand 0 "aarch64_sve_float_maxmin_immediate"))

(define_constraint "vsM"
  "@internal
   A constraint that matches an immediate operand valid for SVE FMUL
   operations."
 (match_operand 0 "aarch64_sve_float_mul_immediate"))

(define_constraint "vsN"
  "@internal
   A constraint that matches the negative of vsA"
 (match_operand 0 "aarch64_sve_float_negated_arith_immediate"))
