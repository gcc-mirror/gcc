;; Predicate definitions for ARM and Thumb
;; Copyright (C) 2004-2017 Free Software Foundation, Inc.
;; Contributed by ARM Ltd.

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

(define_predicate "s_register_operand"
  (match_code "reg,subreg")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  /* We don't consider registers whose class is NO_REGS
     to be a register operand.  */
  /* XXX might have to check for lo regs only for thumb ??? */
  return (REG_P (op)
	  && (REGNO (op) >= FIRST_PSEUDO_REGISTER
	      || REGNO_REG_CLASS (REGNO (op)) != NO_REGS));
})

(define_predicate "imm_for_neon_inv_logic_operand"
  (match_code "const_vector")
{
  return (TARGET_NEON
          && neon_immediate_valid_for_logic (op, mode, 1, NULL, NULL));
})

(define_predicate "neon_inv_logic_op2"
  (ior (match_operand 0 "imm_for_neon_inv_logic_operand")
       (match_operand 0 "s_register_operand")))

(define_predicate "imm_for_neon_logic_operand"
  (match_code "const_vector")
{
  return (TARGET_NEON
          && neon_immediate_valid_for_logic (op, mode, 0, NULL, NULL));
})

(define_predicate "neon_logic_op2"
  (ior (match_operand 0 "imm_for_neon_logic_operand")
       (match_operand 0 "s_register_operand")))

;; Any general register.
(define_predicate "arm_hard_general_register_operand"
  (match_code "reg")
{
  return REGNO (op) <= LAST_ARM_REGNUM;
})

;; A low register.
(define_predicate "low_register_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) <= LAST_LO_REGNUM")))

;; A low register or const_int.
(define_predicate "low_reg_or_int_operand"
  (ior (match_code "const_int")
       (match_operand 0 "low_register_operand")))

;; Any core register, or any pseudo.  */ 
(define_predicate "arm_general_register_operand"
  (match_code "reg,subreg")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  return (REG_P (op)
	  && (REGNO (op) <= LAST_ARM_REGNUM
	      || REGNO (op) >= FIRST_PSEUDO_REGISTER));
})

(define_predicate "arm_general_adddi_operand"
  (ior (match_operand 0 "arm_general_register_operand")
       (and (match_code "const_int")
	    (match_test "const_ok_for_dimode_op (INTVAL (op), PLUS)"))))

(define_predicate "vfp_register_operand"
  (match_code "reg,subreg")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  /* We don't consider registers whose class is NO_REGS
     to be a register operand.  */
  return (REG_P (op)
	  && (REGNO (op) >= FIRST_PSEUDO_REGISTER
	      || REGNO_REG_CLASS (REGNO (op)) == VFP_D0_D7_REGS
	      || REGNO_REG_CLASS (REGNO (op)) == VFP_LO_REGS
	      || (TARGET_VFPD32
		  && REGNO_REG_CLASS (REGNO (op)) == VFP_REGS)));
})

(define_predicate "vfp_hard_register_operand"
  (match_code "reg")
{
  return (IS_VFP_REGNUM (REGNO (op)));
})

(define_predicate "zero_operand"
  (and (match_code "const_int,const_double,const_vector")
       (match_test "op == CONST0_RTX (mode)")))

;; Match a register, or zero in the appropriate mode.
(define_predicate "reg_or_zero_operand"
  (ior (match_operand 0 "s_register_operand")
       (match_operand 0 "zero_operand")))

(define_special_predicate "subreg_lowpart_operator"
  (and (match_code "subreg")
       (match_test "subreg_lowpart_p (op)")))

;; Reg, subreg(reg) or const_int.
(define_predicate "reg_or_int_operand"
  (ior (match_code "const_int")
       (match_operand 0 "s_register_operand")))

(define_predicate "arm_immediate_operand"
  (and (match_code "const_int")
       (match_test "const_ok_for_arm (INTVAL (op))")))

;; A constant value which fits into two instructions, each taking
;; an arithmetic constant operand for one of the words.
(define_predicate "arm_immediate_di_operand"
  (and (match_code "const_int,const_double")
       (match_test "arm_const_double_by_immediates (op)")))

(define_predicate "arm_neg_immediate_operand"
  (and (match_code "const_int")
       (match_test "const_ok_for_arm (-INTVAL (op))")))

(define_predicate "arm_not_immediate_operand"
  (and (match_code "const_int")
       (match_test "const_ok_for_arm (~INTVAL (op))")))

(define_predicate "const0_operand"
  (match_test "op == CONST0_RTX (mode)"))

;; Something valid on the RHS of an ARM data-processing instruction
(define_predicate "arm_rhs_operand"
  (ior (match_operand 0 "s_register_operand")
       (match_operand 0 "arm_immediate_operand")))

(define_predicate "arm_rhsm_operand"
  (ior (match_operand 0 "arm_rhs_operand")
       (match_operand 0 "memory_operand")))

(define_predicate "const_int_I_operand"
  (and (match_operand 0 "const_int_operand")
       (match_test "satisfies_constraint_I (op)")))

(define_predicate "const_int_M_operand"
  (and (match_operand 0 "const_int_operand")
       (match_test "satisfies_constraint_M (op)")))

;; This doesn't have to do much because the constant is already checked
;; in the shift_operator predicate.
(define_predicate "shift_amount_operand"
  (ior (and (match_test "TARGET_ARM")
	    (match_operand 0 "s_register_operand"))
       (match_operand 0 "const_int_operand")))

(define_predicate "const_neon_scalar_shift_amount_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (UINTVAL (op), 1, GET_MODE_BITSIZE (mode))")))

(define_predicate "ldrd_strd_offset_operand"
  (and (match_operand 0 "const_int_operand")
       (match_test "TARGET_LDRD && offset_ok_for_ldrd_strd (INTVAL (op))")))

(define_predicate "arm_add_operand"
  (ior (match_operand 0 "arm_rhs_operand")
       (match_operand 0 "arm_neg_immediate_operand")))

(define_predicate "arm_anddi_operand_neon"
  (ior (match_operand 0 "s_register_operand")
       (and (match_code "const_int")
	    (match_test "const_ok_for_dimode_op (INTVAL (op), AND)"))
       (match_operand 0 "neon_inv_logic_op2")))

(define_predicate "arm_iordi_operand_neon"
  (ior (match_operand 0 "s_register_operand")
       (and (match_code "const_int")
	    (match_test "const_ok_for_dimode_op (INTVAL (op), IOR)"))
       (match_operand 0 "neon_logic_op2")))

(define_predicate "arm_xordi_operand"
  (ior (match_operand 0 "s_register_operand")
       (and (match_code "const_int")
	    (match_test "const_ok_for_dimode_op (INTVAL (op), XOR)"))))

(define_predicate "arm_adddi_operand"
  (ior (match_operand 0 "s_register_operand")
       (and (match_code "const_int")
	    (match_test "const_ok_for_dimode_op (INTVAL (op), PLUS)"))))

(define_predicate "arm_addimm_operand"
  (ior (match_operand 0 "arm_immediate_operand")
       (match_operand 0 "arm_neg_immediate_operand")))

(define_predicate "arm_not_operand"
  (ior (match_operand 0 "arm_rhs_operand")
       (match_operand 0 "arm_not_immediate_operand")))

(define_predicate "arm_di_operand"
  (ior (match_operand 0 "s_register_operand")
       (match_operand 0 "arm_immediate_di_operand")))

;; True if the operand is a memory reference which contains an
;; offsettable address.
(define_predicate "offsettable_memory_operand"
  (and (match_code "mem")
       (match_test
        "offsettable_address_p (reload_completed | reload_in_progress,
				mode, XEXP (op, 0))")))

;; True if the operand is a memory operand that does not have an
;; automodified base register (and thus will not generate output reloads).
(define_predicate "call_memory_operand"
  (and (match_code "mem")
       (and (match_test "GET_RTX_CLASS (GET_CODE (XEXP (op, 0)))
			 != RTX_AUTOINC")
	    (match_operand 0 "memory_operand"))))

(define_predicate "arm_reload_memory_operand"
  (and (match_code "mem,reg,subreg")
       (match_test "(!CONSTANT_P (op)
		     && (true_regnum(op) == -1
			 || (REG_P (op)
			     && REGNO (op) >= FIRST_PSEUDO_REGISTER)))")))

(define_predicate "vfp_compare_operand"
  (ior (match_operand 0 "s_register_operand")
       (and (match_code "const_double")
	    (match_test "arm_const_double_rtx (op)"))))

;; True for valid index operands.
(define_predicate "index_operand"
  (ior (match_operand 0 "s_register_operand")
       (and (match_operand 0 "immediate_operand")
	    (match_test "(!CONST_INT_P (op)
			  || (INTVAL (op) < 4096 && INTVAL (op) > -4096))"))))

;; True for operators that can be combined with a shift in ARM state.
(define_special_predicate "shiftable_operator"
  (and (match_code "plus,minus,ior,xor,and")
       (match_test "mode == GET_MODE (op)")))

(define_special_predicate "shiftable_operator_strict_it"
  (and (match_code "plus,and")
       (match_test "mode == GET_MODE (op)")))

;; True for logical binary operators.
(define_special_predicate "logical_binary_operator"
  (and (match_code "ior,xor,and")
       (match_test "mode == GET_MODE (op)")))

;; True for commutative operators
(define_special_predicate "commutative_binary_operator"
  (and (match_code "ior,xor,and,plus")
       (match_test "mode == GET_MODE (op)")))

;; True for shift operators.
;; Notes:
;;  * mult is only permitted with a constant shift amount
;;  * patterns that permit register shift amounts only in ARM mode use
;;    shift_amount_operand, patterns that always allow registers do not,
;;    so we don't have to worry about that sort of thing here.
(define_special_predicate "shift_operator"
  (and (ior (ior (and (match_code "mult")
		      (match_test "power_of_two_operand (XEXP (op, 1), mode)"))
		 (and (match_code "rotate")
		      (match_test "CONST_INT_P (XEXP (op, 1))
				   && (UINTVAL (XEXP (op, 1))) < 32")))
	    (and (match_code "ashift,ashiftrt,lshiftrt,rotatert")
		 (match_test "!CONST_INT_P (XEXP (op, 1))
			      || (UINTVAL (XEXP (op, 1))) < 32")))
       (match_test "mode == GET_MODE (op)")))

(define_special_predicate "shift_nomul_operator"
  (and (ior (and (match_code "rotate")
		 (match_test "CONST_INT_P (XEXP (op, 1))
			      && (UINTVAL (XEXP (op, 1))) < 32"))
	    (and (match_code "ashift,ashiftrt,lshiftrt,rotatert")
		 (match_test "!CONST_INT_P (XEXP (op, 1))
			      || (UINTVAL (XEXP (op, 1))) < 32")))
       (match_test "mode == GET_MODE (op)")))

;; True for shift operators which can be used with saturation instructions.
(define_special_predicate "sat_shift_operator"
  (and (ior (and (match_code "mult")
                 (match_test "power_of_two_operand (XEXP (op, 1), mode)"))
            (and (match_code "ashift,ashiftrt")
                 (match_test "CONST_INT_P (XEXP (op, 1))
		              && (UINTVAL (XEXP (op, 1)) < 32)")))
       (match_test "mode == GET_MODE (op)")))

;; True for MULT, to identify which variant of shift_operator is in use.
(define_special_predicate "mult_operator"
  (match_code "mult"))

;; True for operators that have 16-bit thumb variants.  */
(define_special_predicate "thumb_16bit_operator"
  (match_code "plus,minus,and,ior,xor"))

;; True for EQ & NE
(define_special_predicate "equality_operator"
  (match_code "eq,ne"))

;; True for integer comparisons and, if FP is active, for comparisons
;; other than LTGT or UNEQ.
(define_special_predicate "expandable_comparison_operator"
  (match_code "eq,ne,le,lt,ge,gt,geu,gtu,leu,ltu,
	       unordered,ordered,unlt,unle,unge,ungt"))

;; Likewise, but only accept comparisons that are directly supported
;; by ARM condition codes.
(define_special_predicate "arm_comparison_operator"
  (and (match_operand 0 "expandable_comparison_operator")
       (match_test "maybe_get_arm_condition_code (op) != ARM_NV")))

;; Likewise, but don't ignore the mode.
;; RTL SET operations require their operands source and destination have
;; the same modes, so we can't ignore the modes there.  See PR target/69161.
(define_predicate "arm_comparison_operator_mode"
  (and (match_operand 0 "expandable_comparison_operator")
       (match_test "maybe_get_arm_condition_code (op) != ARM_NV")))

(define_special_predicate "lt_ge_comparison_operator"
  (match_code "lt,ge"))

;; The vsel instruction only accepts the ARM condition codes listed below.
(define_special_predicate "arm_vsel_comparison_operator"
  (and (match_operand 0 "expandable_comparison_operator")
       (match_test "maybe_get_arm_condition_code (op) == ARM_GE
                    || maybe_get_arm_condition_code (op) == ARM_GT
                    || maybe_get_arm_condition_code (op) == ARM_EQ
                    || maybe_get_arm_condition_code (op) == ARM_VS
                    || maybe_get_arm_condition_code (op) == ARM_LT
                    || maybe_get_arm_condition_code (op) == ARM_LE
                    || maybe_get_arm_condition_code (op) == ARM_NE
                    || maybe_get_arm_condition_code (op) == ARM_VC")))

(define_special_predicate "arm_cond_move_operator"
  (if_then_else (match_test "arm_restrict_it")
		(and (match_test "TARGET_VFP5")
		     (match_operand 0 "arm_vsel_comparison_operator"))
		(match_operand 0 "expandable_comparison_operator")))

(define_special_predicate "noov_comparison_operator"
  (match_code "lt,ge,eq,ne"))

(define_special_predicate "minmax_operator"
  (and (match_code "smin,smax,umin,umax")
       (match_test "mode == GET_MODE (op)")))

(define_special_predicate "cc_register"
  (and (match_code "reg")
       (and (match_test "REGNO (op) == CC_REGNUM")
	    (ior (match_test "mode == GET_MODE (op)")
		 (match_test "mode == VOIDmode && GET_MODE_CLASS (GET_MODE (op)) == MODE_CC")))))

(define_special_predicate "dominant_cc_register"
  (match_code "reg")
{
  if (mode == VOIDmode)
    {
      mode = GET_MODE (op);
      
      if (GET_MODE_CLASS (mode) != MODE_CC)
	return false;
    }

  return (cc_register (op, mode)
	  && (mode == CC_DNEmode
	     || mode == CC_DEQmode
	     || mode == CC_DLEmode
	     || mode == CC_DLTmode
	     || mode == CC_DGEmode
	     || mode == CC_DGTmode
	     || mode == CC_DLEUmode
	     || mode == CC_DLTUmode
	     || mode == CC_DGEUmode
	     || mode == CC_DGTUmode));
})

;; Any register, including CC
(define_predicate "cc_register_operand"
  (and (match_code "reg")
       (ior (match_operand 0 "s_register_operand")
	    (match_operand 0 "cc_register"))))

(define_special_predicate "arm_extendqisi_mem_op"
  (and (match_operand 0 "memory_operand")
       (match_test "TARGET_ARM ? arm_legitimate_address_outer_p (mode,
                                                                 XEXP (op, 0),
						                 SIGN_EXTEND,
								 0)
                               : memory_address_p (QImode, XEXP (op, 0))")))

(define_special_predicate "arm_reg_or_extendqisi_mem_op"
  (ior (match_operand 0 "arm_extendqisi_mem_op")
       (match_operand 0 "s_register_operand")))

(define_predicate "power_of_two_operand"
  (match_code "const_int")
{
  unsigned HOST_WIDE_INT value = INTVAL (op) & 0xffffffff;

  return value != 0 && (value & (value - 1)) == 0;
})

(define_predicate "nonimmediate_di_operand"
  (match_code "reg,subreg,mem")
{
   if (s_register_operand (op, mode))
     return true;

   if (GET_CODE (op) == SUBREG)
     op = SUBREG_REG (op);

   return MEM_P (op) && memory_address_p (DImode, XEXP (op, 0));
})

(define_predicate "di_operand"
  (ior (match_code "const_int,const_double")
       (and (match_code "reg,subreg,mem")
	    (match_operand 0 "nonimmediate_di_operand"))))

(define_predicate "nonimmediate_soft_df_operand"
  (match_code "reg,subreg,mem")
{
  if (s_register_operand (op, mode))
    return true;

  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  return MEM_P (op) && memory_address_p (DFmode, XEXP (op, 0));
})

(define_predicate "soft_df_operand"
  (ior (match_code "const_double")
       (and (match_code "reg,subreg,mem")
	    (match_operand 0 "nonimmediate_soft_df_operand"))))

(define_special_predicate "load_multiple_operation"
  (match_code "parallel")
{
 return ldm_stm_operation_p (op, /*load=*/true, SImode,
                                 /*consecutive=*/false,
                                 /*return_pc=*/false);
})

(define_special_predicate "store_multiple_operation"
  (match_code "parallel")
{
 return ldm_stm_operation_p (op, /*load=*/false, SImode,
                                 /*consecutive=*/false,
                                 /*return_pc=*/false);
})

(define_special_predicate "pop_multiple_return"
  (match_code "parallel")
{
 return ldm_stm_operation_p (op, /*load=*/true, SImode,
                                 /*consecutive=*/false,
                                 /*return_pc=*/true);
})

(define_special_predicate "pop_multiple_fp"
  (match_code "parallel")
{
 return ldm_stm_operation_p (op, /*load=*/true, DFmode,
                                 /*consecutive=*/true,
                                 /*return_pc=*/false);
})

(define_special_predicate "multi_register_push"
  (match_code "parallel")
{
  if ((GET_CODE (XVECEXP (op, 0, 0)) != SET)
      || (GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != UNSPEC)
      || (XINT (SET_SRC (XVECEXP (op, 0, 0)), 1) != UNSPEC_PUSH_MULT))
    return false;

  return true;
})

(define_predicate "push_mult_memory_operand"
  (match_code "mem")
{
  /* ??? Given how PUSH_MULT is generated in the prologues, is there
     any point in testing for thumb1 specially?  All of the variants
     use the same form.  */
  if (TARGET_THUMB1)
    {
      /* ??? No attempt is made to represent STMIA, or validate that
	 the stack adjustment matches the register count.  This is
	 true of the ARM/Thumb2 path as well.  */
      rtx x = XEXP (op, 0);
      if (GET_CODE (x) != PRE_MODIFY)
	return false;
      if (XEXP (x, 0) != stack_pointer_rtx)
	return false;
      x = XEXP (x, 1);
      if (GET_CODE (x) != PLUS)
	return false;
      if (XEXP (x, 0) != stack_pointer_rtx)
	return false;
      return CONST_INT_P (XEXP (x, 1));
    }

  /* ARM and Thumb2 handle pre-modify in their legitimate_address.  */
  return memory_operand (op, mode);
})

;;-------------------------------------------------------------------------
;;
;; Thumb predicates
;;

(define_predicate "thumb1_cmp_operand"
  (ior (and (match_code "reg,subreg")
	    (match_operand 0 "s_register_operand"))
       (and (match_code "const_int")
	    (match_test "(UINTVAL (op)) < 256"))))

(define_predicate "thumb1_cmpneg_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) < 0 && INTVAL (op) > -256")))

;; Return TRUE if a result can be stored in OP without clobbering the
;; condition code register.  Prior to reload we only accept a
;; register.  After reload we have to be able to handle memory as
;; well, since a pseudo may not get a hard reg and reload cannot
;; handle output-reloads on jump insns.

;; We could possibly handle mem before reload as well, but that might
;; complicate things with the need to handle increment
;; side-effects.
(define_predicate "thumb_cbrch_target_operand"
  (and (match_code "reg,subreg,mem")
       (ior (match_operand 0 "s_register_operand")
	    (and (match_test "reload_in_progress || reload_completed")
		 (match_operand 0 "memory_operand")))))

;;-------------------------------------------------------------------------
;;
;; iWMMXt predicates
;;

(define_predicate "imm_or_reg_operand"
  (ior (match_operand 0 "immediate_operand")
       (match_operand 0 "register_operand")))

;; Neon predicates

(define_predicate "const_multiple_of_8_operand"
  (match_code "const_int")
{
  unsigned HOST_WIDE_INT val = INTVAL (op);
  return (val & 7) == 0;
})

(define_predicate "imm_for_neon_mov_operand"
  (match_code "const_vector,const_int")
{
  return neon_immediate_valid_for_move (op, mode, NULL, NULL);
})

(define_predicate "imm_for_neon_lshift_operand"
  (match_code "const_vector")
{
  return neon_immediate_valid_for_shift (op, mode, NULL, NULL, true);
})

(define_predicate "imm_for_neon_rshift_operand"
  (match_code "const_vector")
{
  return neon_immediate_valid_for_shift (op, mode, NULL, NULL, false);
})

(define_predicate "imm_lshift_or_reg_neon"
  (ior (match_operand 0 "s_register_operand")
       (match_operand 0 "imm_for_neon_lshift_operand")))

(define_predicate "imm_rshift_or_reg_neon"
  (ior (match_operand 0 "s_register_operand")
       (match_operand 0 "imm_for_neon_rshift_operand")))

;; Predicates for named expanders that overlap multiple ISAs.

(define_predicate "cmpdi_operand"
  (and (match_test "TARGET_32BIT")
       (match_operand 0 "arm_di_operand")))

;; True if the operand is memory reference suitable for a ldrex/strex.
(define_predicate "arm_sync_memory_operand"
  (and (match_operand 0 "memory_operand")
       (match_code "reg" "0")))

;; Predicates for parallel expanders based on mode.
(define_special_predicate "vect_par_constant_high" 
  (match_code "parallel")
{
  return arm_simd_check_vect_par_cnst_half_p (op, mode, true);
})

(define_special_predicate "vect_par_constant_low"
  (match_code "parallel")
{
  return arm_simd_check_vect_par_cnst_half_p (op, mode, false);
})

(define_predicate "const_double_vcvt_power_of_two_reciprocal"
  (and (match_code "const_double")
       (match_test "TARGET_32BIT
		    && vfp3_const_double_for_fract_bits (op)")))

(define_predicate "const_double_vcvt_power_of_two"
  (and (match_code "const_double")
       (match_test "TARGET_32BIT
		    && vfp3_const_double_for_bits (op) > 0")))

(define_predicate "neon_struct_operand"
  (and (match_code "mem")
       (match_test "TARGET_32BIT && neon_vector_mem_operand (op, 2, true)")))

(define_predicate "neon_permissive_struct_operand"
  (and (match_code "mem")
       (match_test "TARGET_32BIT && neon_vector_mem_operand (op, 2, false)")))

(define_predicate "neon_perm_struct_or_reg_operand"
  (ior (match_operand 0 "neon_permissive_struct_operand")
       (match_operand 0 "s_register_operand")))

(define_special_predicate "add_operator"
  (match_code "plus"))

(define_predicate "mem_noofs_operand"
  (and (match_code "mem")
       (match_code "reg" "0")))

(define_predicate "call_insn_operand"
  (ior (and (match_code "symbol_ref")
	    (match_test "!arm_is_long_call_p (SYMBOL_REF_DECL (op))"))
       (match_operand 0 "s_register_operand")))
