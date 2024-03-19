;; Predicate description for RISC-V target.
;; Copyright (C) 2011-2024 Free Software Foundation, Inc.
;; Contributed by Andrew Waterman (andrew@sifive.com).
;; Based on MIPS target for GNU compiler.
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

(define_predicate "const_arith_operand"
  (and (match_code "const_int")
       (match_test "SMALL_OPERAND (INTVAL (op))")))

(define_predicate "arith_operand"
  (ior (match_operand 0 "const_arith_operand")
       (match_operand 0 "register_operand")))

(define_predicate "arith_operand_or_mode_mask"
  (ior (match_operand 0 "arith_operand")
       (and (match_code "const_int")
            (match_test "UINTVAL (op) == GET_MODE_MASK (HImode)
			 || UINTVAL (op) == GET_MODE_MASK (SImode)"))))

(define_predicate "lui_operand"
  (and (match_code "const_int")
       (match_test "LUI_OPERAND (INTVAL (op))")))

(define_predicate "sfb_alu_operand"
  (ior (match_operand 0 "arith_operand")
       (match_operand 0 "lui_operand")))

(define_predicate "movcc_operand"
  (if_then_else (match_test "TARGET_SFB_ALU || TARGET_XTHEADCONDMOV
			     || TARGET_ZICOND_LIKE")
		(match_operand 0 "sfb_alu_operand")
		(match_operand 0 "arith_operand")))

(define_predicate "const_csr_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 31)")))

(define_predicate "const_0_3_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 3)")))

(define_predicate "const_0_10_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 10)")))

(define_predicate "csr_operand"
  (ior (match_operand 0 "const_csr_operand")
       (match_operand 0 "register_operand")))

;; V has 32-bit unsigned immediates.  This happens to be the same constraint as
;; the csr_operand, but it's not CSR related.
(define_predicate "vector_scalar_shift_operand"
  (match_operand 0 "csr_operand"))

(define_predicate "sle_operand"
  (and (match_code "const_int")
       (match_test "SMALL_OPERAND (INTVAL (op) + 1)")))

(define_predicate "sleu_operand"
  (and (match_operand 0 "sle_operand")
       (match_test "INTVAL (op) + 1 != 0")))

(define_predicate "const_0_operand"
  (and (match_code "const_int,const_wide_int,const_double,const_vector")
       (match_test "op == CONST0_RTX (GET_MODE (op))")))

(define_predicate "const_1_operand"
  (and (match_code "const_int,const_wide_int,const_vector")
       (match_test "op == CONST1_RTX (GET_MODE (op))")))

(define_predicate "const_1_or_2_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) == 1 || INTVAL (op) == 2")))

(define_predicate "const_1_or_4_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) == 1 || INTVAL (op) == 4")))

(define_predicate "const_1_or_8_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) == 1 || INTVAL (op) == 8")))

(define_predicate "reg_or_0_operand"
  (ior (match_operand 0 "const_0_operand")
       (match_operand 0 "register_operand")))

;; ZCMP predicates
(define_predicate "stack_push_up_to_ra_operand"
  (and (match_code "const_int")
       (match_test "riscv_zcmp_valid_stack_adj_bytes_p (INTVAL (op) * -1, 1)")))

(define_predicate "stack_push_up_to_s0_operand"
  (and (match_code "const_int")
       (match_test "riscv_zcmp_valid_stack_adj_bytes_p (INTVAL (op) * -1, 2)")))

(define_predicate "stack_push_up_to_s1_operand"
  (and (match_code "const_int")
       (match_test "riscv_zcmp_valid_stack_adj_bytes_p (INTVAL (op) * -1, 3)")))

(define_predicate "stack_push_up_to_s2_operand"
  (and (match_code "const_int")
       (match_test "riscv_zcmp_valid_stack_adj_bytes_p (INTVAL (op) * -1, 4)")))

(define_predicate "stack_push_up_to_s3_operand"
  (and (match_code "const_int")
       (match_test "riscv_zcmp_valid_stack_adj_bytes_p (INTVAL (op) * -1, 5)")))

(define_predicate "stack_push_up_to_s4_operand"
  (and (match_code "const_int")
       (match_test "riscv_zcmp_valid_stack_adj_bytes_p (INTVAL (op) * -1, 6)")))

(define_predicate "stack_push_up_to_s5_operand"
  (and (match_code "const_int")
       (match_test "riscv_zcmp_valid_stack_adj_bytes_p (INTVAL (op) * -1, 7)")))

(define_predicate "stack_push_up_to_s6_operand"
  (and (match_code "const_int")
       (match_test "riscv_zcmp_valid_stack_adj_bytes_p (INTVAL (op) * -1, 8)")))

(define_predicate "stack_push_up_to_s7_operand"
  (and (match_code "const_int")
       (match_test "riscv_zcmp_valid_stack_adj_bytes_p (INTVAL (op) * -1, 9)")))

(define_predicate "stack_push_up_to_s8_operand"
  (and (match_code "const_int")
       (match_test "riscv_zcmp_valid_stack_adj_bytes_p (INTVAL (op) * -1, 10)")))

(define_predicate "stack_push_up_to_s9_operand"
  (and (match_code "const_int")
       (match_test "riscv_zcmp_valid_stack_adj_bytes_p (INTVAL (op) * -1, 11)")))

(define_predicate "stack_push_up_to_s11_operand"
  (and (match_code "const_int")
       (match_test "riscv_zcmp_valid_stack_adj_bytes_p (INTVAL (op) * -1, 13)")))

(define_predicate "stack_pop_up_to_ra_operand"
  (and (match_code "const_int")
       (match_test "riscv_zcmp_valid_stack_adj_bytes_p (INTVAL (op), 1)")))

(define_predicate "stack_pop_up_to_s0_operand"
  (and (match_code "const_int")
       (match_test "riscv_zcmp_valid_stack_adj_bytes_p (INTVAL (op), 2)")))

(define_predicate "stack_pop_up_to_s1_operand"
  (and (match_code "const_int")
       (match_test "riscv_zcmp_valid_stack_adj_bytes_p (INTVAL (op), 3)")))

(define_predicate "stack_pop_up_to_s2_operand"
  (and (match_code "const_int")
       (match_test "riscv_zcmp_valid_stack_adj_bytes_p (INTVAL (op), 4)")))

(define_predicate "stack_pop_up_to_s3_operand"
  (and (match_code "const_int")
       (match_test "riscv_zcmp_valid_stack_adj_bytes_p (INTVAL (op), 5)")))

(define_predicate "stack_pop_up_to_s4_operand"
  (and (match_code "const_int")
       (match_test "riscv_zcmp_valid_stack_adj_bytes_p (INTVAL (op), 6)")))

(define_predicate "stack_pop_up_to_s5_operand"
  (and (match_code "const_int")
       (match_test "riscv_zcmp_valid_stack_adj_bytes_p (INTVAL (op), 7)")))

(define_predicate "stack_pop_up_to_s6_operand"
  (and (match_code "const_int")
       (match_test "riscv_zcmp_valid_stack_adj_bytes_p (INTVAL (op), 8)")))

(define_predicate "stack_pop_up_to_s7_operand"
  (and (match_code "const_int")
       (match_test "riscv_zcmp_valid_stack_adj_bytes_p (INTVAL (op), 9)")))

(define_predicate "stack_pop_up_to_s8_operand"
  (and (match_code "const_int")
       (match_test "riscv_zcmp_valid_stack_adj_bytes_p (INTVAL (op), 10)")))

(define_predicate "stack_pop_up_to_s9_operand"
  (and (match_code "const_int")
       (match_test "riscv_zcmp_valid_stack_adj_bytes_p (INTVAL (op), 11)")))

(define_predicate "stack_pop_up_to_s11_operand"
  (and (match_code "const_int")
       (match_test "riscv_zcmp_valid_stack_adj_bytes_p (INTVAL (op), 13)")))

(define_predicate "a0a1_reg_operand"
  (and (match_code "reg")
       (match_test "IN_RANGE (REGNO (op), A0_REGNUM, A1_REGNUM)")))

(define_predicate "zcmp_mv_sreg_operand"
  (and (match_code "reg")
       (match_test "TARGET_RVE ? IN_RANGE (REGNO (op), S0_REGNUM, S1_REGNUM)
                    : IN_RANGE (REGNO (op), S0_REGNUM, S1_REGNUM)
                    || IN_RANGE (REGNO (op), S2_REGNUM, S7_REGNUM)")))

;; Only use branch-on-bit sequences when the mask is not an ANDI immediate.
(define_predicate "branch_on_bit_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) >= IMM_BITS - 1")))

;; A legitimate CONST_INT operand that takes more than one instruction
;; to load.
(define_predicate "splittable_const_int_operand"
  (match_code "const_int")
{
  /* Don't handle multi-word moves this way; we don't want to introduce
     the individual word-mode moves until after reload.  */
  if (GET_MODE_SIZE (mode).to_constant () > UNITS_PER_WORD)
    return false;

  /* Check whether the constant can be loaded in a single
     instruction with zbs extensions.  */
  if (TARGET_ZBS && SINGLE_BIT_MASK_OPERAND (INTVAL (op)))
    return false;

  /* Otherwise check whether the constant can be loaded in a single
     instruction.  */
  return !LUI_OPERAND (INTVAL (op)) && !SMALL_OPERAND (INTVAL (op));
})

(define_predicate "p2m1_shift_operand"
  (match_code "const_int")
{
  int val = exact_log2 (INTVAL (op) + 1);
  if (val < 12)
    return false;
  return true;
 })

(define_predicate "high_mask_shift_operand"
  (match_code "const_int")
{
  int val1 = clz_hwi (~ INTVAL (op));
  int val0 = ctz_hwi (INTVAL (op));
  if ((val0 + val1 == BITS_PER_WORD)
      && val0 > 31 && val0 < 64)
    return true;
  return false;
})

(define_predicate "move_operand"
  (match_operand 0 "general_operand")
{
  enum riscv_symbol_type symbol_type;

  /* The thinking here is as follows:

     (1) The move expanders should split complex load sequences into
	 individual instructions.  Those individual instructions can
	 then be optimized by all rtl passes.

     (2) The target of pre-reload load sequences should not be used
	 to store temporary results.  If the target register is only
	 assigned one value, reload can rematerialize that value
	 on demand, rather than spill it to the stack.

     (3) If we allowed pre-reload passes like combine and cse to recreate
	 complex load sequences, we would want to be able to split the
	 sequences before reload as well, so that the pre-reload scheduler
	 can see the individual instructions.  This falls foul of (2);
	 the splitter would be forced to reuse the target register for
	 intermediate results.

     (4) We want to define complex load splitters for combine.  These
	 splitters can request a temporary scratch register, which avoids
	 the problem in (2).  They allow things like:

	      (set (reg T1) (high SYM))
	      (set (reg T2) (low (reg T1) SYM))
	      (set (reg X) (plus (reg T2) (const_int OFFSET)))

	 to be combined into:

	      (set (reg T3) (high SYM+OFFSET))
	      (set (reg X) (lo_sum (reg T3) SYM+OFFSET))

	 if T2 is only used this once.  */
  switch (GET_CODE (op))
    {
    case CONST_INT:
      return !splittable_const_int_operand (op, mode);

    case CONST_POLY_INT:
      return known_eq (rtx_to_poly_int64 (op), BYTES_PER_RISCV_VECTOR);

    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      return riscv_symbolic_constant_p (op, &symbol_type)
	     && !riscv_split_symbol_type (symbol_type)
	     && symbol_type != SYMBOL_FORCE_TO_MEM;

    case HIGH:
      op = XEXP (op, 0);
      return riscv_symbolic_constant_p (op, &symbol_type)
	      && riscv_split_symbol_type (symbol_type)
	      && symbol_type != SYMBOL_PCREL;

    default:
      return true;
    }
})

(define_predicate "symbolic_operand"
  (match_code "const,symbol_ref,label_ref")
{
  enum riscv_symbol_type type;
  return riscv_symbolic_constant_p (op, &type);
})

(define_predicate "absolute_symbolic_operand"
  (match_code "const,symbol_ref,label_ref")
{
  enum riscv_symbol_type type;
  return (riscv_symbolic_constant_p (op, &type)
	  && (type == SYMBOL_ABSOLUTE || type == SYMBOL_PCREL));
})

(define_predicate "plt_symbolic_operand"
  (match_code "const,symbol_ref,label_ref")
{
  enum riscv_symbol_type type;
  return (riscv_symbolic_constant_p (op, &type)
	  && type == SYMBOL_GOT_DISP && !SYMBOL_REF_WEAK (op) && TARGET_PLT);
})

(define_predicate "call_insn_operand"
  (match_operand 0 "general_operand")
{
  if (riscv_cmodel == CM_LARGE)
    return register_operand (op, mode);
  else
    return (absolute_symbolic_operand (op, mode)
	    || plt_symbolic_operand (op, mode)
	    || register_operand (op, mode));
})

(define_predicate "modular_operator"
  (match_code "plus,minus,mult,ashift"))

(define_predicate "ne_operator"
  (match_code "ne"))

(define_predicate "equality_operator"
  (match_code "eq,ne"))

(define_predicate "signed_order_operator"
  (match_code "eq,ne,lt,le,ge,gt"))

(define_predicate "subreg_lowpart_operator"
  (ior (match_code "truncate")
       (and (match_code "subreg")
            (match_test "subreg_lowpart_p (op)"))))

(define_predicate "fp_native_comparison"
  (match_code "eq,lt,le,gt,ge"))

(define_predicate "fp_scc_comparison"
  (match_code "unordered,ordered,unlt,unge,unle,ungt,ltgt,ne,eq,lt,le,gt,ge"))

(define_predicate "fp_branch_comparison"
  (match_code "unordered,ordered,unlt,unge,unle,ungt,uneq,ltgt,ne,eq,lt,le,gt,ge"))

(define_special_predicate "gpr_save_operation"
  (match_code "parallel")
{
  return riscv_gpr_save_operation_p (op);
})

;; Predicates for the ZBS extension.
(define_predicate "single_bit_mask_operand"
  (and (match_code "const_int")
       (match_test "SINGLE_BIT_MASK_OPERAND (UINTVAL (op))")))

(define_predicate "not_single_bit_mask_operand"
  (and (match_code "const_int")
       (match_test "SINGLE_BIT_MASK_OPERAND (~UINTVAL (op))")))

(define_predicate "const_si_mask_operand"
  (and (match_code "const_int")
       (match_test "(INTVAL (op) & (GET_MODE_BITSIZE (SImode) - 1))
                    == GET_MODE_BITSIZE (SImode) - 1")))

(define_predicate "const_di_mask_operand"
  (and (match_code "const_int")
       (match_test "(INTVAL (op) & (GET_MODE_BITSIZE (DImode) - 1))
                    == GET_MODE_BITSIZE (DImode) - 1")))

(define_predicate "imm5_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) < 5")))

;; A const_int for sh1add/sh2add/sh3add
(define_predicate "imm123_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 1, 3)")))

;; A CONST_INT operand that consists of a single run of consecutive set bits.
(define_predicate "consecutive_bits_operand"
  (match_code "const_int")
{
	unsigned HOST_WIDE_INT val = UINTVAL (op);
	if (exact_log2 ((val >> ctz_hwi (val)) + 1) < 0)
	        return false;

	return true;
})

;; CORE-V Predicates:
(define_predicate "immediate_register_operand"
  (ior (match_operand 0 "register_operand")
       (match_code "const_int")))

(define_predicate "const_int6s_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), -32, 31)")))

(define_predicate "int6s_operand"
  (ior (match_operand 0 "const_int6s_operand")
       (match_operand 0 "register_operand")))

(define_predicate "const_int2_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 3)")))

(define_predicate "const_int6_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 0, 63)")))

(define_predicate "int6_operand"
  (ior (match_operand 0 "const_int6_operand")
       (match_operand 0 "register_operand")))

(define_predicate "const_int5s_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), -16, 15)")))

;; Predicates for the V extension.
(define_special_predicate "vector_length_operand"
  (ior (match_operand 0 "pmode_register_operand")
       (and (ior (match_test "TARGET_XTHEADVECTOR && rtx_equal_p (op, const0_rtx)")
		 (match_test "!TARGET_XTHEADVECTOR"))
    (match_operand 0 "const_csr_operand"))))

(define_special_predicate "autovec_length_operand"
  (ior (match_operand 0 "pmode_register_operand")
       (match_code "const_int,const_poly_int")))

(define_predicate "reg_or_mem_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "memory_operand")))

(define_predicate "reg_or_int_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "const_int_operand")))

(define_predicate "vector_const_0_operand"
  (and (match_code "const_vector")
       (match_test "satisfies_constraint_Wc0 (op)")))

(define_predicate "vector_const_int_or_double_0_operand"
  (and (match_code "const_vector")
       (match_test "satisfies_constraint_vi (op)
                    || satisfies_constraint_Wc0 (op)")))

(define_predicate "vector_move_operand"
  (ior (match_operand 0 "nonimmediate_operand")
       (and (match_code "const_vector")
            (match_test "reload_completed
		|| satisfies_constraint_vi (op)
		|| satisfies_constraint_Wc0 (op)"))))

(define_predicate "vector_all_trues_mask_operand"
  (and (match_code "const_vector")
       (match_test "op == CONSTM1_RTX (GET_MODE (op))")))

(define_predicate "vector_least_significant_set_mask_operand"
  (and (match_code "const_vector")
       (match_test "rtx_equal_p (op, riscv_vector::gen_scalar_move_mask (GET_MODE (op)))")))

(define_predicate "vector_mask_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "vector_all_trues_mask_operand")))

(define_predicate "vector_broadcast_mask_operand"
  (ior (match_operand 0 "vector_least_significant_set_mask_operand")
    (ior (match_operand 0 "register_operand")
         (match_operand 0 "vector_all_trues_mask_operand"))))

(define_predicate "vector_undef_operand"
  (match_test "rtx_equal_p (op, RVV_VUNDEF (GET_MODE (op)))"))

(define_predicate "vector_merge_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "vector_undef_operand")))

(define_predicate "autovec_else_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "scratch_operand")))

(define_predicate "vector_arith_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_code "const_vector")
            (match_test "riscv_vector::const_vec_all_same_in_range_p (op, -16, 15)"))))

(define_predicate "vector_neg_arith_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_code "const_vector")
            (match_test "riscv_vector::const_vec_all_same_in_range_p (op, -15, 16)"))))

(define_predicate "vector_shift_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_code "const_vector")
            (match_test "riscv_vector::const_vec_all_same_in_range_p (op, 0, 31)"))))

(define_predicate "vector_perm_operand"
  (ior (match_operand 0 "register_operand")
       (match_code "const_vector")))

(define_predicate "vector_gs_extension_operand"
  (ior (match_operand 0 "const_1_operand")
       (and (match_operand 0 "const_0_operand")
            (match_test "Pmode == SImode"))))

(define_predicate "vector_gs_scale_operand_16_rv32"
  (and (match_code "const_int")
       (match_test "INTVAL (op) == 1
		    || (INTVAL (op) == 2 && Pmode == SImode)")))

(define_predicate "vector_gs_scale_operand_32_rv32"
  (and (match_code "const_int")
       (match_test "INTVAL (op) == 1
		    || (INTVAL (op) == 4 && Pmode == SImode)")))

(define_predicate "ltge_operator"
  (match_code "lt,ltu,ge,geu"))

(define_predicate "comparison_except_ltge_operator"
  (match_code "eq,ne,le,leu,gt,gtu"))

(define_predicate "comparison_except_eqge_operator"
  (match_code "le,leu,gt,gtu,lt,ltu"))

(define_predicate "ge_operator"
  (match_code "ge,geu"))

;; pmode_reg_or_uimm5_operand can be used by vsll.vx/vsrl.vx/vsra.vx instructions.
;; Since it has the same predicate with vector_length_operand which allows register
;; or immediate (0 ~ 31), we define this predicate same as vector_length_operand here.
;; We don't use vector_length_operand directly to predicate vsll.vx/vsrl.vx/vsra.vx
;; since it may be confusing.
(define_special_predicate "pmode_reg_or_uimm5_operand"
  (match_operand 0 "vector_length_operand"))

(define_special_predicate "pmode_reg_or_0_operand"
  (ior (match_operand 0 "const_0_operand")
       (match_operand 0 "pmode_register_operand")))

;; [1, 2, 4, 8] means strided load/store with stride == element width
(define_special_predicate "vector_eew8_stride_operand"
  (ior (match_operand 0 "pmode_register_operand")
       (and (match_code "const_int")
            (match_test "INTVAL (op) == 1 || INTVAL (op) == 0"))))
(define_special_predicate "vector_eew16_stride_operand"
  (ior (match_operand 0 "pmode_register_operand")
       (and (match_code "const_int")
            (match_test "INTVAL (op) == 2 || INTVAL (op) == 0"))))
(define_special_predicate "vector_eew32_stride_operand"
  (ior (match_operand 0 "pmode_register_operand")
       (and (match_code "const_int")
            (match_test "INTVAL (op) == 4 || INTVAL (op) == 0"))))
(define_special_predicate "vector_eew64_stride_operand"
  (ior (match_operand 0 "pmode_register_operand")
       (and (match_code "const_int")
            (match_test "INTVAL (op) == 8 || INTVAL (op) == 0"))))

;; A special predicate that doesn't match a particular mode.
(define_special_predicate "vector_any_register_operand"
  (match_code "reg"))

;; The scalar operand can be directly broadcast by RVV instructions.
(define_predicate "direct_broadcast_operand"
  (match_test "riscv_vector::can_be_broadcasted_p (op)"))

;; A CONST_INT operand that has exactly two bits cleared.
(define_predicate "const_nottwobits_operand"
  (and (match_code "const_int")
       (match_test "popcount_hwi (~UINTVAL (op)) == 2")))

(define_predicate "const_nottwobits_not_arith_operand"
  (and (match_code "const_int")
       (and (not (match_operand 0 "arith_operand"))
	    (match_operand 0 "const_nottwobits_operand"))))

;; A CONST_INT operand that consists of a single run of 32 consecutive
;; set bits.
(define_predicate "consecutive_bits32_operand"
  (and (match_operand 0 "consecutive_bits_operand")
       (match_test "popcount_hwi (UINTVAL (op)) == 32")))

;; A CONST_INT operand that, if shifted down to start with its least
;; significant non-zero bit, is a SMALL_OPERAND (suitable as an
;; immediate to logical and arithmetic instructions).
(define_predicate "shifted_const_arith_operand"
  (and (match_code "const_int")
       (match_test "ctz_hwi (INTVAL (op)) > 0")
       (match_test "SMALL_OPERAND (INTVAL (op) >> ctz_hwi (INTVAL (op)))")))

;; A CONST_INT operand that has exactly two bits set.
(define_predicate "const_twobits_operand"
  (and (match_code "const_int")
       (match_test "popcount_hwi (UINTVAL (op)) == 2")))

(define_predicate "const_twobits_not_arith_operand"
  (and (match_code "const_int")
       (and (not (match_operand 0 "arith_operand"))
	    (match_operand 0 "const_twobits_operand"))))

;; A CONST_INT operand that fits into the unsigned half of a
;; signed-immediate after the top bit has been cleared
(define_predicate "uimm_extra_bit_operand"
  (and (match_code "const_int")
       (match_test "UIMM_EXTRA_BIT_OPERAND (UINTVAL (op))")))

(define_predicate "uimm_extra_bit_or_twobits"
  (and (match_code "const_int")
       (ior (match_operand 0 "uimm_extra_bit_operand")
	    (match_operand 0 "const_twobits_not_arith_operand"))))

;; A CONST_INT operand that fits into the negative half of a
;; signed-immediate after a single cleared top bit has been
;; set: i.e., a bitwise-negated uimm_extra_bit_operand
(define_predicate "not_uimm_extra_bit_operand"
  (and (match_code "const_int")
       (match_test "UIMM_EXTRA_BIT_OPERAND (~UINTVAL (op))")))

(define_predicate "not_uimm_extra_bit_or_nottwobits"
  (and (match_code "const_int")
       (ior (match_operand 0 "not_uimm_extra_bit_operand")
	    (match_operand 0 "const_nottwobits_not_arith_operand"))))

(define_predicate "pcrel_symbol_operand"
  (match_code "symbol_ref")
{
  enum riscv_symbol_type type;
  return (riscv_symbolic_constant_p (op, &type)
         && type == SYMBOL_PCREL);
})
