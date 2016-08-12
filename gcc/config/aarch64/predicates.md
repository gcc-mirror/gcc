;; Machine description for AArch64 architecture.
;; Copyright (C) 2009-2016 Free Software Foundation, Inc.
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

;; Return true if OP a (const_int 0) operand.
(define_predicate "const0_operand"
  (and (match_code "const_int")
       (match_test "op == CONST0_RTX (mode)")))

(define_predicate "aarch64_ccmp_immediate"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), -31, 31)")))

(define_predicate "aarch64_ccmp_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_ccmp_immediate")))

(define_predicate "aarch64_simd_register"
  (and (match_code "reg")
       (ior (match_test "REGNO_REG_CLASS (REGNO (op)) == FP_LO_REGS")
            (match_test "REGNO_REG_CLASS (REGNO (op)) == FP_REGS"))))

(define_predicate "aarch64_reg_or_zero"
  (and (match_code "reg,subreg,const_int")
       (ior (match_operand 0 "register_operand")
	    (match_test "op == const0_rtx"))))

(define_predicate "aarch64_reg_or_fp_zero"
  (and (match_code "reg,subreg,const_double")
       (ior (match_operand 0 "register_operand")
	    (match_test "aarch64_float_const_zero_rtx_p (op)"))))

(define_predicate "aarch64_reg_zero_or_m1_or_1"
  (and (match_code "reg,subreg,const_int")
       (ior (match_operand 0 "register_operand")
	    (ior (match_test "op == const0_rtx")
		 (ior (match_test "op == constm1_rtx")
		      (match_test "op == const1_rtx"))))))

(define_predicate "aarch64_fp_compare_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_code "const_double")
	    (match_test "aarch64_float_const_zero_rtx_p (op)"))))

(define_predicate "aarch64_fp_pow2"
  (and (match_code "const_double")
	(match_test "aarch64_fpconst_pow_of_2 (op) > 0")))

(define_predicate "aarch64_fp_vec_pow2"
  (match_test "aarch64_vec_fpconst_pow_of_2 (op) > 0"))

(define_predicate "aarch64_plus_immediate"
  (and (match_code "const_int")
       (ior (match_test "aarch64_uimm12_shift (INTVAL (op))")
	    (match_test "aarch64_uimm12_shift (-INTVAL (op))"))))

(define_predicate "aarch64_plus_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_plus_immediate")))

(define_predicate "aarch64_pluslong_immediate"
  (and (match_code "const_int")
       (match_test "(INTVAL (op) < 0xffffff && INTVAL (op) > -0xffffff)")))

(define_predicate "aarch64_pluslong_strict_immedate"
  (and (match_operand 0 "aarch64_pluslong_immediate")
       (not (match_operand 0 "aarch64_plus_immediate"))))

(define_predicate "aarch64_pluslong_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_pluslong_immediate")))

(define_predicate "aarch64_logical_immediate"
  (and (match_code "const_int")
       (match_test "aarch64_bitmask_imm (INTVAL (op), mode)")))

(define_predicate "aarch64_logical_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "aarch64_logical_immediate")))

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
       (match_test "aarch64_legitimate_address_p (mode, XEXP (op, 0), PARALLEL,
					       0)")))

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
  (and (match_code "reg,subreg,mem,const,const_int,symbol_ref,label_ref,high")
       (ior (match_operand 0 "register_operand")
	    (ior (match_operand 0 "memory_operand")
		 (match_test "aarch64_mov_operand_p (op, mode)")))))

(define_predicate "aarch64_movti_operand"
  (and (match_code "reg,subreg,mem,const_int")
       (ior (match_operand 0 "register_operand")
	    (ior (match_operand 0 "memory_operand")
		 (match_operand 0 "const_int_operand")))))

(define_predicate "aarch64_reg_or_imm"
  (and (match_code "reg,subreg,const_int")
       (ior (match_operand 0 "register_operand")
	    (match_operand 0 "const_int_operand"))))

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

(define_special_predicate "aarch64_carry_operation"
  (match_code "ne,geu")
{
  if (XEXP (op, 1) != const0_rtx)
    return false;
  machine_mode ccmode = (GET_CODE (op) == NE ? CC_Cmode : CCmode);
  rtx op0 = XEXP (op, 0);
  return REG_P (op0) && REGNO (op0) == CC_REGNUM && GET_MODE (op0) == ccmode;
})

(define_special_predicate "aarch64_borrow_operation"
  (match_code "eq,ltu")
{
  if (XEXP (op, 1) != const0_rtx)
    return false;
  machine_mode ccmode = (GET_CODE (op) == EQ ? CC_Cmode : CCmode);
  rtx op0 = XEXP (op, 0);
  return REG_P (op0) && REGNO (op0) == CC_REGNUM && GET_MODE (op0) == ccmode;
})

;; True if the operand is memory reference suitable for a load/store exclusive.
(define_predicate "aarch64_sync_memory_operand"
  (and (match_operand 0 "memory_operand")
       (match_code "reg" "0")))

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

(define_special_predicate "aarch64_simd_lshift_imm"
  (match_code "const_vector")
{
  return aarch64_simd_shift_imm_p (op, mode, true);
})

(define_special_predicate "aarch64_simd_rshift_imm"
  (match_code "const_vector")
{
  return aarch64_simd_shift_imm_p (op, mode, false);
})

(define_predicate "aarch64_simd_reg_or_zero"
  (and (match_code "reg,subreg,const_int,const_double,const_vector")
       (ior (match_operand 0 "register_operand")
           (ior (match_test "op == const0_rtx")
                (match_test "aarch64_simd_imm_zero_p (op, mode)")))))

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

(define_special_predicate "aarch64_simd_imm_zero"
  (match_code "const_vector")
{
  return aarch64_simd_imm_zero_p (op, mode);
})

(define_special_predicate "aarch64_simd_imm_minus_one"
  (match_code "const_vector")
{
  return aarch64_const_vec_all_same_int_p (op, -1);
})

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
