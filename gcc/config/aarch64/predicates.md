;; Machine description for AArch64 architecture.
;; Copyright (C) 2009-2019 Free Software Foundation, Inc.
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

(define_special_predicate "cc_register"
  (and (match_code "reg")
       (and (match_test "REGNO (op) == CC_REGNUM")
	    (ior (match_test "mode == GET_MODE (op)")
		 (match_test "mode == VOIDmode
			      && GET_MODE_CLASS (GET_MODE (op)) == MODE_CC"))))
)

(define_predicate "aarch64_call_insn_operand"
  (ior (match_code "symbol_ref")
       (match_operand 0 "register_operand")))

(define_predicate "aarch64_general_reg"
  (and (match_operand 0 "register_operand")
       (match_test "REGNO_REG_CLASS (REGNO (op)) == GENERAL_REGS")))

;; Return true if OP a (const_int 0) operand.
(define_predicate "const0_operand"
  (and (match_code "const_int")
       (match_test "op == CONST0_RTX (mode)")))

(define_predicate "const_1_to_3_operand"
  (match_code "const_int,const_vector")
{
  op = unwrap_const_vec_duplicate (op);
  return CONST_INT_P (op) && IN_RANGE (INTVAL (op), 1, 3);
})

(define_predicate "subreg_lowpart_operator"
  (ior (match_code "truncate")
       (and (match_code "subreg")
	    (match_test "subreg_lowpart_p (op)"))))

(define_predicate "aarch64_ccmp_immediate"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), -31, 31)")))

(define_predicate "aarch64_ccmp_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_ccmp_immediate")))

(define_predicate "aarch64_simd_register"
  (and (match_code "reg")
       (match_test "FP_REGNUM_P (REGNO (op))")))

(define_predicate "aarch64_reg_or_zero"
  (and (match_code "reg,subreg,const_int,const_double")
       (ior (match_operand 0 "register_operand")
	    (match_test "op == CONST0_RTX (GET_MODE (op))"))))

(define_predicate "aarch64_reg_or_fp_zero"
  (ior (match_operand 0 "register_operand")
	(and (match_code "const_double")
	     (match_test "aarch64_float_const_zero_rtx_p (op)"))))

(define_predicate "aarch64_reg_zero_or_fp_zero"
  (ior (match_operand 0 "aarch64_reg_or_fp_zero")
       (match_operand 0 "aarch64_reg_or_zero")))

(define_predicate "aarch64_reg_zero_or_m1_or_1"
  (and (match_code "reg,subreg,const_int")
       (ior (match_operand 0 "register_operand")
	    (ior (match_test "op == const0_rtx")
		 (ior (match_test "op == constm1_rtx")
		      (match_test "op == const1_rtx"))))))

(define_predicate "aarch64_reg_or_orr_imm"
   (ior (match_operand 0 "register_operand")
	(and (match_code "const_vector")
	     (match_test "aarch64_simd_valid_immediate (op, NULL,
							AARCH64_CHECK_ORR)"))))

(define_predicate "aarch64_reg_or_bic_imm"
   (ior (match_operand 0 "register_operand")
	(and (match_code "const_vector")
	     (match_test "aarch64_simd_valid_immediate (op, NULL,
							AARCH64_CHECK_BIC)"))))

(define_predicate "aarch64_fp_compare_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_code "const_double")
	    (match_test "aarch64_float_const_zero_rtx_p (op)"))))

(define_predicate "aarch64_fp_pow2"
  (and (match_code "const_double")
	(match_test "aarch64_fpconst_pow_of_2 (op) > 0")))

(define_predicate "aarch64_fp_pow2_recip"
  (and (match_code "const_double")
       (match_test "aarch64_fpconst_pow2_recip (op) > 0")))

(define_predicate "aarch64_fp_vec_pow2"
  (match_test "aarch64_vec_fpconst_pow_of_2 (op) > 0"))

(define_predicate "aarch64_sve_cnt_immediate"
  (and (match_code "const_poly_int")
       (match_test "aarch64_sve_cnt_immediate_p (op)")))

(define_predicate "aarch64_sub_immediate"
  (and (match_code "const_int")
       (match_test "aarch64_uimm12_shift (-INTVAL (op))")))

(define_predicate "aarch64_plus_immediate"
  (and (match_code "const_int")
       (ior (match_test "aarch64_uimm12_shift (INTVAL (op))")
	    (match_test "aarch64_uimm12_shift (-INTVAL (op))"))))

(define_predicate "aarch64_plus_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_plus_immediate")))

(define_predicate "aarch64_plushi_immediate"
  (match_code "const_int")
{
  HOST_WIDE_INT val = INTVAL (op);
  /* The HImode value must be zero-extendable to an SImode plus_operand.  */
  return ((val & 0xfff) == val || sext_hwi (val & 0xf000, 16) == val);
})

(define_predicate "aarch64_plushi_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_plushi_immediate")))

(define_predicate "aarch64_pluslong_immediate"
  (and (match_code "const_int")
       (match_test "(INTVAL (op) < 0xffffff && INTVAL (op) > -0xffffff)")))

(define_predicate "aarch64_pluslong_strict_immedate"
  (and (match_operand 0 "aarch64_pluslong_immediate")
       (not (match_operand 0 "aarch64_plus_immediate"))))

(define_predicate "aarch64_sve_scalar_inc_dec_immediate"
  (and (match_code "const_poly_int")
       (match_test "aarch64_sve_scalar_inc_dec_immediate_p (op)")))

(define_predicate "aarch64_sve_addvl_addpl_immediate"
  (and (match_code "const_poly_int")
       (match_test "aarch64_sve_addvl_addpl_immediate_p (op)")))

(define_predicate "aarch64_sve_plus_immediate"
  (ior (match_operand 0 "aarch64_sve_scalar_inc_dec_immediate")
       (match_operand 0 "aarch64_sve_addvl_addpl_immediate")))

(define_predicate "aarch64_split_add_offset_immediate"
  (and (match_code "const_poly_int")
       (match_test "aarch64_add_offset_temporaries (op) == 1")))

(define_predicate "aarch64_pluslong_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_pluslong_immediate")
       (and (match_test "TARGET_SVE")
	    (match_operand 0 "aarch64_sve_plus_immediate"))))

(define_predicate "aarch64_pluslong_or_poly_operand"
  (ior (match_operand 0 "aarch64_pluslong_operand")
       (match_operand 0 "aarch64_split_add_offset_immediate")))

(define_predicate "aarch64_logical_immediate"
  (and (match_code "const_int")
       (match_test "aarch64_bitmask_imm (INTVAL (op), mode)")))

(define_predicate "aarch64_logical_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_logical_immediate")))

(define_predicate "aarch64_mov_imm_operand"
  (and (match_code "const_int")
       (match_test "aarch64_move_imm (INTVAL (op), mode)")))

(define_predicate "aarch64_logical_and_immediate"
  (and (match_code "const_int")
       (match_test "aarch64_and_bitmask_imm (INTVAL (op), mode)")))

(define_predicate "aarch64_shift_imm_si"
  (and (match_code "const_int")
       (match_test "(unsigned HOST_WIDE_INT) INTVAL (op) < 32")))

(define_predicate "aarch64_shift_imm_di"
  (and (match_code "const_int")
       (match_test "(unsigned HOST_WIDE_INT) INTVAL (op) < 64")))

(define_predicate "aarch64_shift_imm64_di"
  (and (match_code "const_int")
       (match_test "(unsigned HOST_WIDE_INT) INTVAL (op) <= 64")))

(define_predicate "aarch64_reg_or_shift_imm_si"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_shift_imm_si")))

(define_predicate "aarch64_reg_or_shift_imm_di"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_shift_imm_di")))

;; The imm3 field is a 3-bit field that only accepts immediates in the
;; range 0..4.
(define_predicate "aarch64_imm3"
  (and (match_code "const_int")
       (match_test "(unsigned HOST_WIDE_INT) INTVAL (op) <= 4")))

;; The imm2 field is a 2-bit field that only accepts immediates in the
;; range 0..3.
(define_predicate "aarch64_imm2"
  (and (match_code "const_int")
       (match_test "UINTVAL (op) <= 3")))

;; The imm3 field is a 3-bit field that only accepts immediates in the
;; range 0..7.
(define_predicate "aarch64_lane_imm3"
  (and (match_code "const_int")
       (match_test "UINTVAL (op) <= 7")))

;; An immediate that fits into 24 bits.
(define_predicate "aarch64_imm24"
  (and (match_code "const_int")
       (match_test "IN_RANGE (UINTVAL (op), 0, 0xffffff)")))

(define_predicate "aarch64_pwr_imm3"
  (and (match_code "const_int")
       (match_test "INTVAL (op) != 0
		    && (unsigned) exact_log2 (INTVAL (op)) <= 4")))

(define_predicate "aarch64_pwr_2_si"
  (and (match_code "const_int")
       (match_test "INTVAL (op) != 0
		    && (unsigned) exact_log2 (INTVAL (op)) < 32")))

(define_predicate "aarch64_pwr_2_di"
  (and (match_code "const_int")
       (match_test "INTVAL (op) != 0
		    && (unsigned) exact_log2 (INTVAL (op)) < 64")))

(define_predicate "aarch64_mem_pair_offset"
  (and (match_code "const_int")
       (match_test "aarch64_offset_7bit_signed_scaled_p (mode, INTVAL (op))")))

(define_predicate "aarch64_mem_pair_operand"
  (and (match_code "mem")
       (match_test "aarch64_legitimate_address_p (mode, XEXP (op, 0), false,
						  ADDR_QUERY_LDP_STP)")))

;; Used for storing two 64-bit values in an AdvSIMD register using an STP
;; as a 128-bit vec_concat.
(define_predicate "aarch64_mem_pair_lanes_operand"
  (and (match_code "mem")
       (match_test "aarch64_legitimate_address_p (GET_MODE (op), XEXP (op, 0),
						  false,
						  ADDR_QUERY_LDP_STP_N)")))

(define_predicate "aarch64_prefetch_operand"
  (match_test "aarch64_address_valid_for_prefetch_p (op, false)"))

(define_predicate "aarch64_valid_symref"
  (match_code "const, symbol_ref, label_ref")
{
  return (aarch64_classify_symbolic_expression (op)
	  != SYMBOL_FORCE_TO_MEM);
})

(define_predicate "aarch64_tls_ie_symref"
  (match_code "const, symbol_ref, label_ref")
{
  switch (GET_CODE (op))
    {
    case CONST:
      op = XEXP (op, 0);
      if (GET_CODE (op) != PLUS
	  || GET_CODE (XEXP (op, 0)) != SYMBOL_REF
	  || GET_CODE (XEXP (op, 1)) != CONST_INT)
	return false;
      op = XEXP (op, 0);
      /* FALLTHRU */

    case SYMBOL_REF:
      return SYMBOL_REF_TLS_MODEL (op) == TLS_MODEL_INITIAL_EXEC;

    default:
      gcc_unreachable ();
    }
})

(define_predicate "aarch64_tls_le_symref"
  (match_code "const, symbol_ref, label_ref")
{
  switch (GET_CODE (op))
    {
    case CONST:
      op = XEXP (op, 0);
      if (GET_CODE (op) != PLUS
	  || GET_CODE (XEXP (op, 0)) != SYMBOL_REF
	  || GET_CODE (XEXP (op, 1)) != CONST_INT)
	return false;
      op = XEXP (op, 0);
      /* FALLTHRU */

    case SYMBOL_REF:
      return SYMBOL_REF_TLS_MODEL (op) == TLS_MODEL_LOCAL_EXEC;

    default:
      gcc_unreachable ();
    }
})

(define_predicate "aarch64_mov_operand"
  (and (match_code "reg,subreg,mem,const,const_int,symbol_ref,label_ref,high,
		    const_poly_int,const_vector")
       (ior (match_operand 0 "register_operand")
	    (ior (match_operand 0 "memory_operand")
		 (match_test "aarch64_mov_operand_p (op, mode)")))))

(define_predicate "aarch64_nonmemory_operand"
  (and (match_code "reg,subreg,const,const_int,symbol_ref,label_ref,high,
		    const_poly_int,const_vector")
       (ior (match_operand 0 "register_operand")
	    (match_test "aarch64_mov_operand_p (op, mode)"))))

(define_predicate "aarch64_movti_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "memory_operand")
       (and (match_operand 0 "const_scalar_int_operand")
	    (match_test "aarch64_mov128_immediate (op)"))))

(define_predicate "aarch64_reg_or_imm"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "const_scalar_int_operand")))

;; True for integer comparisons and for FP comparisons other than LTGT or UNEQ.
(define_special_predicate "aarch64_comparison_operator"
  (match_code "eq,ne,le,lt,ge,gt,geu,gtu,leu,ltu,unordered,
	       ordered,unlt,unle,unge,ungt"))

;; Same as aarch64_comparison_operator but don't ignore the mode.
;; RTL SET operations require their operands source and destination have
;; the same modes, so we can't ignore the modes there.  See PR target/69161.
(define_predicate "aarch64_comparison_operator_mode"
  (match_code "eq,ne,le,lt,ge,gt,geu,gtu,leu,ltu,unordered,
	       ordered,unlt,unle,unge,ungt"))

(define_special_predicate "aarch64_comparison_operation"
  (match_code "eq,ne,le,lt,ge,gt,geu,gtu,leu,ltu,unordered,
	       ordered,unlt,unle,unge,ungt")
{
  if (XEXP (op, 1) != const0_rtx)
    return false;
  rtx op0 = XEXP (op, 0);
  if (!REG_P (op0) || REGNO (op0) != CC_REGNUM)
    return false;
  return aarch64_get_condition_code (op) >= 0;
})

(define_special_predicate "aarch64_equality_operator"
  (match_code "eq,ne"))

(define_special_predicate "aarch64_carry_operation"
  (match_code "ltu,geu")
{
  if (XEXP (op, 1) != const0_rtx)
    return false;
  rtx op0 = XEXP (op, 0);
  if (!REG_P (op0) || REGNO (op0) != CC_REGNUM)
    return false;
  machine_mode ccmode = GET_MODE (op0);
  if (ccmode == CC_Cmode)
    return GET_CODE (op) == LTU;
  if (ccmode == CC_ADCmode || ccmode == CCmode)
    return GET_CODE (op) == GEU;
  return false;
})

; borrow is essentially the inverse of carry since the sense of the C flag
; is inverted during subtraction.  See the note in aarch64-modes.def.
(define_special_predicate "aarch64_borrow_operation"
  (match_code "geu,ltu")
{
  if (XEXP (op, 1) != const0_rtx)
    return false;
  rtx op0 = XEXP (op, 0);
  if (!REG_P (op0) || REGNO (op0) != CC_REGNUM)
    return false;
  machine_mode ccmode = GET_MODE (op0);
  if (ccmode == CC_Cmode)
    return GET_CODE (op) == GEU;
  if (ccmode == CC_ADCmode || ccmode == CCmode)
    return GET_CODE (op) == LTU;
  return false;
})

;; True if the operand is memory reference suitable for a load/store exclusive.
(define_predicate "aarch64_sync_memory_operand"
  (and (match_operand 0 "memory_operand")
       (match_code "reg" "0")))

(define_predicate "aarch64_9bit_offset_memory_operand"
  (and (match_operand 0 "memory_operand")
       (ior (match_code "reg" "0")
	    (and (match_code "plus" "0")
		 (match_code "reg"  "00")
		 (match_code "const_int" "01"))))
{
  rtx mem_op = XEXP (op, 0);

  if (REG_P (mem_op))
    return GET_MODE (mem_op) == DImode;

  rtx plus_op0 = XEXP (mem_op, 0);
  rtx plus_op1 = XEXP (mem_op, 1);

  if (GET_MODE (plus_op0) != DImode)
    return false;

  poly_int64 offset;
  if (!poly_int_rtx_p (plus_op1, &offset))
    gcc_unreachable ();

  return aarch64_offset_9bit_signed_unscaled_p (mode, offset);
})

(define_predicate "aarch64_rcpc_memory_operand"
  (if_then_else (match_test "AARCH64_ISA_RCPC8_4")
    (match_operand 0 "aarch64_9bit_offset_memory_operand")
    (match_operand 0 "aarch64_sync_memory_operand")))

;; Predicates for parallel expanders based on mode.
(define_special_predicate "vect_par_cnst_hi_half"
  (match_code "parallel")
{
  return aarch64_simd_check_vect_par_cnst_half (op, mode, true);
})

(define_special_predicate "vect_par_cnst_lo_half"
  (match_code "parallel")
{
  return aarch64_simd_check_vect_par_cnst_half (op, mode, false);
})

(define_predicate "descending_int_parallel"
  (match_code "parallel")
{
  return aarch64_stepped_int_parallel_p (op, -1);
})

(define_predicate "ascending_int_parallel"
  (match_code "parallel")
{
  return aarch64_stepped_int_parallel_p (op, 1);
})

(define_special_predicate "aarch64_simd_lshift_imm"
  (match_code "const,const_vector")
{
  return aarch64_simd_shift_imm_p (op, mode, true);
})

(define_special_predicate "aarch64_simd_rshift_imm"
  (match_code "const,const_vector")
{
  return aarch64_simd_shift_imm_p (op, mode, false);
})

(define_predicate "aarch64_simd_imm_zero"
  (and (match_code "const,const_vector")
       (match_test "op == CONST0_RTX (GET_MODE (op))")))

(define_predicate "aarch64_simd_imm_one"
  (and (match_code "const_vector")
       (match_test "op == CONST1_RTX (GET_MODE (op))")))

(define_predicate "aarch64_simd_or_scalar_imm_zero"
  (and (match_code "const_int,const_double,const,const_vector")
       (match_test "op == CONST0_RTX (GET_MODE (op))")))

(define_predicate "aarch64_simd_imm_minus_one"
  (and (match_code "const,const_vector")
       (match_test "op == CONSTM1_RTX (GET_MODE (op))")))

(define_predicate "aarch64_simd_reg_or_zero"
  (and (match_code "reg,subreg,const_int,const_double,const,const_vector")
       (ior (match_operand 0 "register_operand")
	    (match_test "op == const0_rtx")
	    (match_operand 0 "aarch64_simd_or_scalar_imm_zero"))))

(define_predicate "aarch64_simd_reg_or_minus_one"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_simd_imm_minus_one")))

(define_predicate "aarch64_simd_struct_operand"
  (and (match_code "mem")
       (match_test "TARGET_SIMD && aarch64_simd_mem_operand_p (op)")))

;; Like general_operand but allow only valid SIMD addressing modes.
(define_predicate "aarch64_simd_general_operand"
  (and (match_operand 0 "general_operand")
       (match_test "!MEM_P (op)
		    || GET_CODE (XEXP (op, 0)) == POST_INC
		    || GET_CODE (XEXP (op, 0)) == REG")))

;; Like nonimmediate_operand but allow only valid SIMD addressing modes.
(define_predicate "aarch64_simd_nonimmediate_operand"
  (and (match_operand 0 "nonimmediate_operand")
       (match_test "!MEM_P (op)
		    || GET_CODE (XEXP (op, 0)) == POST_INC
		    || GET_CODE (XEXP (op, 0)) == REG")))

;; Predicates used by the various SIMD shift operations.  These
;; fall in to 3 categories.
;;   Shifts with a range 0-(bit_size - 1) (aarch64_simd_shift_imm)
;;   Shifts with a range 1-bit_size (aarch64_simd_shift_imm_offset)
;;   Shifts with a range 0-bit_size (aarch64_simd_shift_imm_bitsize)
(define_predicate "aarch64_simd_shift_imm_qi"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 7)")))

(define_predicate "aarch64_simd_shift_imm_hi"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 15)")))

(define_predicate "aarch64_simd_shift_imm_si"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 31)")))

(define_predicate "aarch64_simd_shift_imm_di"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 63)")))

(define_predicate "aarch64_simd_shift_imm_offset_qi"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 1, 8)")))

(define_predicate "aarch64_simd_shift_imm_offset_hi"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 1, 16)")))

(define_predicate "aarch64_simd_shift_imm_offset_si"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 1, 32)")))

(define_predicate "aarch64_simd_shift_imm_offset_di"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 1, 64)")))

(define_predicate "aarch64_simd_shift_imm_bitsize_qi"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 8)")))

(define_predicate "aarch64_simd_shift_imm_bitsize_hi"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 16)")))

(define_predicate "aarch64_simd_shift_imm_bitsize_si"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 32)")))

(define_predicate "aarch64_simd_shift_imm_bitsize_di"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 64)")))

(define_predicate "aarch64_constant_pool_symref"
   (and (match_code "symbol_ref")
	(match_test "CONSTANT_POOL_ADDRESS_P (op)")))

(define_predicate "aarch64_constant_vector_operand"
  (match_code "const,const_vector"))

(define_predicate "aarch64_sve_ld1r_operand"
  (and (match_operand 0 "memory_operand")
       (match_test "aarch64_sve_ld1r_operand_p (op)")))

(define_predicate "aarch64_sve_ld1rq_operand"
  (and (match_code "mem")
       (match_test "aarch64_sve_ld1rq_operand_p (op)")))

(define_predicate "aarch64_sve_ldff1_operand"
  (and (match_code "mem")
       (match_test "aarch64_sve_ldff1_operand_p (op)")))

(define_predicate "aarch64_sve_ldnf1_operand"
  (and (match_code "mem")
       (match_test "aarch64_sve_ldnf1_operand_p (op)")))

;; Like memory_operand, but restricted to addresses that are valid for
;; SVE LDR and STR instructions.
(define_predicate "aarch64_sve_ldr_operand"
  (and (match_code "mem")
       (match_test "aarch64_sve_ldr_operand_p (op)")))

(define_special_predicate "aarch64_sve_prefetch_operand"
  (and (match_code "reg, plus")
       (match_test "aarch64_sve_prefetch_operand_p (op, mode)")))

(define_predicate "aarch64_sve_nonimmediate_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_sve_ldr_operand")))

(define_predicate "aarch64_sve_general_operand"
  (and (match_code "reg,subreg,mem,const,const_vector")
       (ior (match_operand 0 "register_operand")
	    (match_operand 0 "aarch64_sve_ldr_operand")
	    (match_test "aarch64_mov_operand_p (op, mode)"))))

(define_predicate "aarch64_sve_struct_memory_operand"
  (and (match_code "mem")
       (match_test "aarch64_sve_struct_memory_operand_p (op)")))

(define_predicate "aarch64_sve_struct_nonimmediate_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_sve_struct_memory_operand")))

;; Doesn't include immediates, since those are handled by the move
;; patterns instead.
(define_predicate "aarch64_sve_dup_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_sve_ld1r_operand")))

(define_predicate "aarch64_sve_ptrue_svpattern_immediate"
  (and (match_code "const")
       (match_test "aarch64_sve_ptrue_svpattern_p (op, NULL)")))

(define_predicate "aarch64_sve_arith_immediate"
  (and (match_code "const,const_vector")
       (match_test "aarch64_sve_arith_immediate_p (op, false)")))

(define_predicate "aarch64_sve_sub_arith_immediate"
  (and (match_code "const,const_vector")
       (match_test "aarch64_sve_arith_immediate_p (op, true)")))

(define_predicate "aarch64_sve_qadd_immediate"
  (and (match_code "const,const_vector")
       (match_test "aarch64_sve_sqadd_sqsub_immediate_p (op, false)")))

(define_predicate "aarch64_sve_qsub_immediate"
  (and (match_code "const,const_vector")
       (match_test "aarch64_sve_sqadd_sqsub_immediate_p (op, true)")))

(define_predicate "aarch64_sve_vector_inc_dec_immediate"
  (and (match_code "const,const_vector")
       (match_test "aarch64_sve_vector_inc_dec_immediate_p (op)")))

(define_predicate "aarch64_sve_gather_immediate_b"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 31)")))

(define_predicate "aarch64_sve_gather_immediate_h"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 62)")
       (match_test "(INTVAL (op) & 1) == 0")))

(define_predicate "aarch64_sve_gather_immediate_w"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 124)")
       (match_test "(INTVAL (op) & 3) == 0")))

(define_predicate "aarch64_sve_gather_immediate_d"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 248)")
       (match_test "(INTVAL (op) & 7) == 0")))

(define_predicate "aarch64_sve_uxtb_immediate"
  (and (match_code "const_vector")
       (match_test "GET_MODE_UNIT_BITSIZE (GET_MODE (op)) > 8")
       (match_test "aarch64_const_vec_all_same_int_p (op, 0xff)")))

(define_predicate "aarch64_sve_uxth_immediate"
  (and (match_code "const_vector")
       (match_test "GET_MODE_UNIT_BITSIZE (GET_MODE (op)) > 16")
       (match_test "aarch64_const_vec_all_same_int_p (op, 0xffff)")))

(define_predicate "aarch64_sve_uxtw_immediate"
  (and (match_code "const_vector")
       (match_test "GET_MODE_UNIT_BITSIZE (GET_MODE (op)) > 32")
       (match_test "aarch64_const_vec_all_same_int_p (op, 0xffffffff)")))

(define_predicate "aarch64_sve_uxt_immediate"
  (ior (match_operand 0 "aarch64_sve_uxtb_immediate")
       (match_operand 0 "aarch64_sve_uxth_immediate")
       (match_operand 0 "aarch64_sve_uxtw_immediate")))

(define_predicate "aarch64_sve_logical_immediate"
  (and (match_code "const,const_vector")
       (match_test "aarch64_sve_bitmask_immediate_p (op)")))

;; Used for SVE UMAX and UMIN.
(define_predicate "aarch64_sve_vsb_immediate"
  (and (match_code "const_vector")
       (match_test "GET_MODE_INNER (GET_MODE (op)) == QImode
		    ? aarch64_const_vec_all_same_in_range_p (op, -128, 127)
		    : aarch64_const_vec_all_same_in_range_p (op, 0, 255)")))

;; Used for SVE MUL, SMAX and SMIN.
(define_predicate "aarch64_sve_vsm_immediate"
  (and (match_code "const,const_vector")
       (match_test "aarch64_const_vec_all_same_in_range_p (op, -128, 127)")))

(define_predicate "aarch64_sve_dup_immediate"
  (and (match_code "const,const_vector")
       (ior (match_test "aarch64_sve_dup_immediate_p (op)")
	    (match_test "aarch64_float_const_representable_p (op)"))))

(define_predicate "aarch64_sve_cmp_vsc_immediate"
  (and (match_code "const_int,const_vector")
       (match_test "aarch64_sve_cmp_immediate_p (op, true)")))

(define_predicate "aarch64_sve_cmp_vsd_immediate"
  (and (match_code "const_int,const_vector")
       (match_test "aarch64_sve_cmp_immediate_p (op, false)")))

(define_predicate "aarch64_sve_index_immediate"
  (and (match_code "const_int")
       (match_test "aarch64_sve_index_immediate_p (op)")))

(define_predicate "aarch64_sve_float_arith_immediate"
  (and (match_code "const,const_vector")
       (match_test "aarch64_sve_float_arith_immediate_p (op, false)")))

(define_predicate "aarch64_sve_float_negated_arith_immediate"
  (and (match_code "const,const_vector")
       (match_test "aarch64_sve_float_arith_immediate_p (op, true)")))

(define_predicate "aarch64_sve_float_arith_with_sub_immediate"
  (ior (match_operand 0 "aarch64_sve_float_arith_immediate")
       (match_operand 0 "aarch64_sve_float_negated_arith_immediate")))

(define_predicate "aarch64_sve_float_mul_immediate"
  (and (match_code "const,const_vector")
       (match_test "aarch64_sve_float_mul_immediate_p (op)")))

(define_predicate "aarch64_sve_float_maxmin_immediate"
  (and (match_code "const_vector")
       (ior (match_test "op == CONST0_RTX (GET_MODE (op))")
	    (match_test "op == CONST1_RTX (GET_MODE (op))"))))

(define_predicate "aarch64_sve_arith_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_sve_arith_immediate")))

(define_predicate "aarch64_sve_add_operand"
  (ior (match_operand 0 "aarch64_sve_arith_operand")
       (match_operand 0 "aarch64_sve_sub_arith_immediate")
       (match_operand 0 "aarch64_sve_vector_inc_dec_immediate")))

(define_predicate "aarch64_sve_sqadd_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_sve_qadd_immediate")
       (match_operand 0 "aarch64_sve_qsub_immediate")))

(define_predicate "aarch64_sve_pred_and_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_sve_uxt_immediate")))

(define_predicate "aarch64_sve_logical_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_sve_logical_immediate")))

(define_predicate "aarch64_sve_gather_offset_b"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_sve_gather_immediate_b")))

(define_predicate "aarch64_sve_gather_offset_h"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_sve_gather_immediate_h")))

(define_predicate "aarch64_sve_gather_offset_w"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_sve_gather_immediate_w")))

(define_predicate "aarch64_sve_gather_offset_d"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_sve_gather_immediate_d")))

(define_predicate "aarch64_sve_lshift_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_simd_lshift_imm")))

(define_predicate "aarch64_sve_rshift_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_simd_rshift_imm")))

(define_predicate "aarch64_sve_vsb_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_sve_vsb_immediate")))

(define_predicate "aarch64_sve_vsm_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_sve_vsm_immediate")))

(define_predicate "aarch64_sve_reg_or_dup_imm"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_sve_dup_immediate")))

(define_predicate "aarch64_sve_cmp_vsc_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_sve_cmp_vsc_immediate")))

(define_predicate "aarch64_sve_cmp_vsd_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_sve_cmp_vsd_immediate")))

(define_predicate "aarch64_sve_index_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_sve_index_immediate")))

(define_predicate "aarch64_sve_float_arith_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_sve_float_arith_immediate")))

(define_predicate "aarch64_sve_float_arith_with_sub_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_sve_float_arith_with_sub_immediate")))

(define_predicate "aarch64_sve_float_mul_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_sve_float_mul_immediate")))

(define_predicate "aarch64_sve_float_maxmin_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_sve_float_maxmin_immediate")))

(define_predicate "aarch64_sve_vec_perm_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_constant_vector_operand")))

(define_predicate "aarch64_sve_ptrue_flag"
  (and (match_code "const_int")
       (ior (match_test "INTVAL (op) == SVE_MAYBE_NOT_PTRUE")
	    (match_test "INTVAL (op) == SVE_KNOWN_PTRUE"))))

(define_predicate "aarch64_sve_gp_strictness"
  (and (match_code "const_int")
       (ior (match_test "INTVAL (op) == SVE_RELAXED_GP")
	    (match_test "INTVAL (op) == SVE_STRICT_GP"))))

(define_predicate "aarch64_gather_scale_operand_b"
  (and (match_code "const_int")
       (match_test "INTVAL (op) == 1")))

(define_predicate "aarch64_gather_scale_operand_h"
  (and (match_code "const_int")
       (match_test "INTVAL (op) == 1 || INTVAL (op) == 2")))

(define_predicate "aarch64_gather_scale_operand_w"
  (and (match_code "const_int")
       (match_test "INTVAL (op) == 1 || INTVAL (op) == 4")))

(define_predicate "aarch64_gather_scale_operand_d"
  (and (match_code "const_int")
       (match_test "INTVAL (op) == 1 || INTVAL (op) == 8")))

;; A special predicate that doesn't match a particular mode.
(define_special_predicate "aarch64_any_register_operand"
  (match_code "reg"))

(define_predicate "aarch64_sve_any_binary_operator"
  (match_code "plus,minus,mult,div,udiv,smax,umax,smin,umin,and,ior,xor"))

(define_predicate "aarch64_bytes_per_sve_vector_operand"
  (and (match_code "const_int,const_poly_int")
       (match_test "known_eq (wi::to_poly_wide (op, mode),
			      BYTES_PER_SVE_VECTOR)")))

(define_predicate "aarch64_memtag_tag_offset"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 15)")))

(define_predicate "aarch64_granule16_uimm6"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 1008)
		    && !(INTVAL (op) & 0xf)")))

(define_predicate "aarch64_granule16_simm9"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op),  -4096, 4080)
		    && !(INTVAL (op) & 0xf)")))
