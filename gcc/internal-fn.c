/* Internal functions.
   Copyright (C) 2011-2014 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "internal-fn.h"
#include "tree.h"
#include "stor-layout.h"
#include "expr.h"
#include "optabs.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "ubsan.h"
#include "target.h"
#include "predict.h"
#include "stringpool.h"
#include "tree-ssanames.h"

/* The names of each internal function, indexed by function number.  */
const char *const internal_fn_name_array[] = {
#define DEF_INTERNAL_FN(CODE, FLAGS) #CODE,
#include "internal-fn.def"
#undef DEF_INTERNAL_FN
  "<invalid-fn>"
};

/* The ECF_* flags of each internal function, indexed by function number.  */
const int internal_fn_flags_array[] = {
#define DEF_INTERNAL_FN(CODE, FLAGS) FLAGS,
#include "internal-fn.def"
#undef DEF_INTERNAL_FN
  0
};

/* ARRAY_TYPE is an array of vector modes.  Return the associated insn
   for load-lanes-style optab OPTAB.  The insn must exist.  */

static enum insn_code
get_multi_vector_move (tree array_type, convert_optab optab)
{
  enum insn_code icode;
  enum machine_mode imode;
  enum machine_mode vmode;

  gcc_assert (TREE_CODE (array_type) == ARRAY_TYPE);
  imode = TYPE_MODE (array_type);
  vmode = TYPE_MODE (TREE_TYPE (array_type));

  icode = convert_optab_handler (optab, imode, vmode);
  gcc_assert (icode != CODE_FOR_nothing);
  return icode;
}

/* Expand LOAD_LANES call STMT.  */

static void
expand_LOAD_LANES (gimple stmt)
{
  struct expand_operand ops[2];
  tree type, lhs, rhs;
  rtx target, mem;

  lhs = gimple_call_lhs (stmt);
  rhs = gimple_call_arg (stmt, 0);
  type = TREE_TYPE (lhs);

  target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  mem = expand_normal (rhs);

  gcc_assert (MEM_P (mem));
  PUT_MODE (mem, TYPE_MODE (type));

  create_output_operand (&ops[0], target, TYPE_MODE (type));
  create_fixed_operand (&ops[1], mem);
  expand_insn (get_multi_vector_move (type, vec_load_lanes_optab), 2, ops);
}

/* Expand STORE_LANES call STMT.  */

static void
expand_STORE_LANES (gimple stmt)
{
  struct expand_operand ops[2];
  tree type, lhs, rhs;
  rtx target, reg;

  lhs = gimple_call_lhs (stmt);
  rhs = gimple_call_arg (stmt, 0);
  type = TREE_TYPE (rhs);

  target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  reg = expand_normal (rhs);

  gcc_assert (MEM_P (target));
  PUT_MODE (target, TYPE_MODE (type));

  create_fixed_operand (&ops[0], target);
  create_input_operand (&ops[1], reg, TYPE_MODE (type));
  expand_insn (get_multi_vector_move (type, vec_store_lanes_optab), 2, ops);
}

static void
expand_ANNOTATE (gimple stmt ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

/* This should get expanded in adjust_simduid_builtins.  */

static void
expand_GOMP_SIMD_LANE (gimple stmt ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

/* This should get expanded in adjust_simduid_builtins.  */

static void
expand_GOMP_SIMD_VF (gimple stmt ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

/* This should get expanded in adjust_simduid_builtins.  */

static void
expand_GOMP_SIMD_LAST_LANE (gimple stmt ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

/* This should get expanded in the sanopt pass.  */

static void
expand_UBSAN_NULL (gimple stmt ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

/* Add sub/add overflow checking to the statement STMT.
   CODE says whether the operation is +, or -.  */

void
ubsan_expand_si_overflow_addsub_check (tree_code code, gimple stmt)
{
  rtx res, op0, op1;
  tree lhs, fn, arg0, arg1;
  rtx done_label, do_error, target = NULL_RTX;

  lhs = gimple_call_lhs (stmt);
  arg0 = gimple_call_arg (stmt, 0);
  arg1 = gimple_call_arg (stmt, 1);
  done_label = gen_label_rtx ();
  do_error = gen_label_rtx ();
  do_pending_stack_adjust ();
  op0 = expand_normal (arg0);
  op1 = expand_normal (arg1);

  enum machine_mode mode = TYPE_MODE (TREE_TYPE (arg0));
  if (lhs)
    target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);

  enum insn_code icode
    = optab_handler (code == PLUS_EXPR ? addv4_optab : subv4_optab, mode);
  if (icode != CODE_FOR_nothing)
    {
      struct expand_operand ops[4];
      rtx last = get_last_insn ();

      res = gen_reg_rtx (mode);
      create_output_operand (&ops[0], res, mode);
      create_input_operand (&ops[1], op0, mode);
      create_input_operand (&ops[2], op1, mode);
      create_fixed_operand (&ops[3], do_error);
      if (maybe_expand_insn (icode, 4, ops))
	{
	  last = get_last_insn ();
	  if (profile_status_for_fn (cfun) != PROFILE_ABSENT
	      && JUMP_P (last)
	      && any_condjump_p (last)
	      && !find_reg_note (last, REG_BR_PROB, 0))
	    add_int_reg_note (last, REG_BR_PROB, PROB_VERY_UNLIKELY);
	  emit_jump (done_label);
        }
      else
	{
	  delete_insns_since (last);
	  icode = CODE_FOR_nothing;
	}
    }

  if (icode == CODE_FOR_nothing)
    {
      rtx sub_check = gen_label_rtx ();
      int pos_neg = 3;

      /* Compute the operation.  On RTL level, the addition is always
	 unsigned.  */
      res = expand_binop (mode, code == PLUS_EXPR ? add_optab : sub_optab,
			  op0, op1, NULL_RTX, false, OPTAB_LIB_WIDEN);

      /* If we can prove one of the arguments is always non-negative
	 or always negative, we can do just one comparison and
	 conditional jump instead of 2 at runtime, 3 present in the
	 emitted code.  If one of the arguments is CONST_INT, all we
	 need is to make sure it is op1, then the first
	 emit_cmp_and_jump_insns will be just folded.  Otherwise try
	 to use range info if available.  */
      if (CONST_INT_P (op0))
	{
	  rtx tem = op0;
	  op0 = op1;
	  op1 = tem;
	}
      else if (CONST_INT_P (op1))
	;
      else if (TREE_CODE (arg0) == SSA_NAME)
	{
	  double_int arg0_min, arg0_max;
	  if (get_range_info (arg0, &arg0_min, &arg0_max) == VR_RANGE)
	    {
	      if (!arg0_min.is_negative ())
		pos_neg = 1;
	      else if (arg0_max.is_negative ())
		pos_neg = 2;
	    }
	  if (pos_neg != 3)
	    {
	      rtx tem = op0;
	      op0 = op1;
	      op1 = tem;
	    }
	}
      if (pos_neg == 3 && !CONST_INT_P (op1) && TREE_CODE (arg1) == SSA_NAME)
	{
	  double_int arg1_min, arg1_max;
	  if (get_range_info (arg1, &arg1_min, &arg1_max) == VR_RANGE)
	    {
	      if (!arg1_min.is_negative ())
		pos_neg = 1;
	      else if (arg1_max.is_negative ())
		pos_neg = 2;
	    }
	}

      /* If the op1 is negative, we have to use a different check.  */
      if (pos_neg == 3)
	emit_cmp_and_jump_insns (op1, const0_rtx, LT, NULL_RTX, mode,
				 false, sub_check, PROB_EVEN);

      /* Compare the result of the operation with one of the operands.  */
      if (pos_neg & 1)
	emit_cmp_and_jump_insns (res, op0, code == PLUS_EXPR ? GE : LE,
				 NULL_RTX, mode, false, done_label,
				 PROB_VERY_LIKELY);

      /* If we get here, we have to print the error.  */
      if (pos_neg == 3)
	{
	  emit_jump (do_error);

	  emit_label (sub_check);
	}

      /* We have k = a + b for b < 0 here.  k <= a must hold.  */
      if (pos_neg & 2)
	emit_cmp_and_jump_insns (res, op0, code == PLUS_EXPR ? LE : GE,
				 NULL_RTX, mode, false, done_label,
				 PROB_VERY_LIKELY);
    }

  emit_label (do_error);
  /* Expand the ubsan builtin call.  */
  push_temp_slots ();
  fn = ubsan_build_overflow_builtin (code, gimple_location (stmt),
				     TREE_TYPE (arg0), arg0, arg1);
  expand_normal (fn);
  pop_temp_slots ();
  do_pending_stack_adjust ();

  /* We're done.  */
  emit_label (done_label);

  if (lhs)
    emit_move_insn (target, res);
}

/* Add negate overflow checking to the statement STMT.  */

void
ubsan_expand_si_overflow_neg_check (gimple stmt)
{
  rtx res, op1;
  tree lhs, fn, arg1;
  rtx done_label, do_error, target = NULL_RTX;

  lhs = gimple_call_lhs (stmt);
  arg1 = gimple_call_arg (stmt, 1);
  done_label = gen_label_rtx ();
  do_error = gen_label_rtx ();

  do_pending_stack_adjust ();
  op1 = expand_normal (arg1);

  enum machine_mode mode = TYPE_MODE (TREE_TYPE (arg1));
  if (lhs)
    target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);

  enum insn_code icode = optab_handler (negv3_optab, mode);
  if (icode != CODE_FOR_nothing)
    {
      struct expand_operand ops[3];
      rtx last = get_last_insn ();

      res = gen_reg_rtx (mode);
      create_output_operand (&ops[0], res, mode);
      create_input_operand (&ops[1], op1, mode);
      create_fixed_operand (&ops[2], do_error);
      if (maybe_expand_insn (icode, 3, ops))
	{
	  last = get_last_insn ();
	  if (profile_status_for_fn (cfun) != PROFILE_ABSENT
	      && JUMP_P (last)
	      && any_condjump_p (last)
	      && !find_reg_note (last, REG_BR_PROB, 0))
	    add_int_reg_note (last, REG_BR_PROB, PROB_VERY_UNLIKELY);
	  emit_jump (done_label);
        }
      else
	{
	  delete_insns_since (last);
	  icode = CODE_FOR_nothing;
	}
    }

  if (icode == CODE_FOR_nothing)
    {
      /* Compute the operation.  On RTL level, the addition is always
	 unsigned.  */
      res = expand_unop (mode, neg_optab, op1, NULL_RTX, false);

      /* Compare the operand with the most negative value.  */
      rtx minv = expand_normal (TYPE_MIN_VALUE (TREE_TYPE (arg1)));
      emit_cmp_and_jump_insns (op1, minv, NE, NULL_RTX, mode, false,
			       done_label, PROB_VERY_LIKELY);
    }

  emit_label (do_error);
  /* Expand the ubsan builtin call.  */
  push_temp_slots ();
  fn = ubsan_build_overflow_builtin (NEGATE_EXPR, gimple_location (stmt),
				     TREE_TYPE (arg1), arg1, NULL_TREE);
  expand_normal (fn);
  pop_temp_slots ();
  do_pending_stack_adjust ();

  /* We're done.  */
  emit_label (done_label);

  if (lhs)
    emit_move_insn (target, res);
}

/* Add mul overflow checking to the statement STMT.  */

void
ubsan_expand_si_overflow_mul_check (gimple stmt)
{
  rtx res, op0, op1;
  tree lhs, fn, arg0, arg1;
  rtx done_label, do_error, target = NULL_RTX;

  lhs = gimple_call_lhs (stmt);
  arg0 = gimple_call_arg (stmt, 0);
  arg1 = gimple_call_arg (stmt, 1);
  done_label = gen_label_rtx ();
  do_error = gen_label_rtx ();

  do_pending_stack_adjust ();
  op0 = expand_normal (arg0);
  op1 = expand_normal (arg1);

  enum machine_mode mode = TYPE_MODE (TREE_TYPE (arg0));
  if (lhs)
    target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);

  enum insn_code icode = optab_handler (mulv4_optab, mode);
  if (icode != CODE_FOR_nothing)
    {
      struct expand_operand ops[4];
      rtx last = get_last_insn ();

      res = gen_reg_rtx (mode);
      create_output_operand (&ops[0], res, mode);
      create_input_operand (&ops[1], op0, mode);
      create_input_operand (&ops[2], op1, mode);
      create_fixed_operand (&ops[3], do_error);
      if (maybe_expand_insn (icode, 4, ops))
	{
	  last = get_last_insn ();
	  if (profile_status_for_fn (cfun) != PROFILE_ABSENT
	      && JUMP_P (last)
	      && any_condjump_p (last)
	      && !find_reg_note (last, REG_BR_PROB, 0))
	    add_int_reg_note (last, REG_BR_PROB, PROB_VERY_UNLIKELY);
	  emit_jump (done_label);
        }
      else
	{
	  delete_insns_since (last);
	  icode = CODE_FOR_nothing;
	}
    }

  if (icode == CODE_FOR_nothing)
    {
      struct separate_ops ops;
      enum machine_mode hmode
	= mode_for_size (GET_MODE_PRECISION (mode) / 2, MODE_INT, 1);
      ops.op0 = arg0;
      ops.op1 = arg1;
      ops.op2 = NULL_TREE;
      ops.location = gimple_location (stmt);
      if (GET_MODE_2XWIDER_MODE (mode) != VOIDmode
	  && targetm.scalar_mode_supported_p (GET_MODE_2XWIDER_MODE (mode)))
	{
	  enum machine_mode wmode = GET_MODE_2XWIDER_MODE (mode);
	  ops.code = WIDEN_MULT_EXPR;
	  ops.type
	    = build_nonstandard_integer_type (GET_MODE_PRECISION (wmode), 0);

	  res = expand_expr_real_2 (&ops, NULL_RTX, wmode, EXPAND_NORMAL);
	  rtx hipart = expand_shift (RSHIFT_EXPR, wmode, res,
				     GET_MODE_PRECISION (mode), NULL_RTX, 0);
	  hipart = gen_lowpart (mode, hipart);
	  res = gen_lowpart (mode, res);
	  rtx signbit = expand_shift (RSHIFT_EXPR, mode, res,
				      GET_MODE_PRECISION (mode) - 1,
				      NULL_RTX, 0);
	  /* RES is low half of the double width result, HIPART
	     the high half.  There was overflow if
	     HIPART is different from RES < 0 ? -1 : 0.  */
	  emit_cmp_and_jump_insns (signbit, hipart, EQ, NULL_RTX, mode,
				   false, done_label, PROB_VERY_LIKELY);
	}
      else if (hmode != BLKmode
	       && 2 * GET_MODE_PRECISION (hmode) == GET_MODE_PRECISION (mode))
	{
	  rtx large_op0 = gen_label_rtx ();
	  rtx small_op0_large_op1 = gen_label_rtx ();
	  rtx one_small_one_large = gen_label_rtx ();
	  rtx both_ops_large = gen_label_rtx ();
	  rtx after_hipart_neg = gen_label_rtx ();
	  rtx after_lopart_neg = gen_label_rtx ();
	  rtx do_overflow = gen_label_rtx ();
	  rtx hipart_different = gen_label_rtx ();

	  int hprec = GET_MODE_PRECISION (hmode);
	  rtx hipart0 = expand_shift (RSHIFT_EXPR, mode, op0, hprec,
				      NULL_RTX, 0);
	  hipart0 = gen_lowpart (hmode, hipart0);
	  rtx lopart0 = gen_lowpart (hmode, op0);
	  rtx signbit0 = expand_shift (RSHIFT_EXPR, hmode, lopart0, hprec - 1,
				       NULL_RTX, 0);
	  rtx hipart1 = expand_shift (RSHIFT_EXPR, mode, op1, hprec,
				      NULL_RTX, 0);
	  hipart1 = gen_lowpart (hmode, hipart1);
	  rtx lopart1 = gen_lowpart (hmode, op1);
	  rtx signbit1 = expand_shift (RSHIFT_EXPR, hmode, lopart1, hprec - 1,
				       NULL_RTX, 0);

	  res = gen_reg_rtx (mode);

	  /* True if op0 resp. op1 are known to be in the range of
	     halfstype.  */
	  bool op0_small_p = false;
	  bool op1_small_p = false;
	  /* True if op0 resp. op1 are known to have all zeros or all ones
	     in the upper half of bits, but are not known to be
	     op{0,1}_small_p.  */
	  bool op0_medium_p = false;
	  bool op1_medium_p = false;
	  /* -1 if op{0,1} is known to be negative, 0 if it is known to be
	     nonnegative, 1 if unknown.  */
	  int op0_sign = 1;
	  int op1_sign = 1;

	  if (TREE_CODE (arg0) == SSA_NAME)
	    {
	      double_int arg0_min, arg0_max;
	      if (get_range_info (arg0, &arg0_min, &arg0_max) == VR_RANGE)
		{
		  if (arg0_max.sle (double_int::max_value (hprec, false))
		      && double_int::min_value (hprec, false).sle (arg0_min))
		    op0_small_p = true;
		  else if (arg0_max.sle (double_int::max_value (hprec, true))
			   && (~double_int::max_value (hprec,
						       true)).sle (arg0_min))
		    op0_medium_p = true;
		  if (!arg0_min.is_negative ())
		    op0_sign = 0;
		  else if (arg0_max.is_negative ())
		    op0_sign = -1;
		}
	    }
	  if (TREE_CODE (arg1) == SSA_NAME)
	    {
	      double_int arg1_min, arg1_max;
	      if (get_range_info (arg1, &arg1_min, &arg1_max) == VR_RANGE)
		{
		  if (arg1_max.sle (double_int::max_value (hprec, false))
		      && double_int::min_value (hprec, false).sle (arg1_min))
		    op1_small_p = true;
		  else if (arg1_max.sle (double_int::max_value (hprec, true))
			   && (~double_int::max_value (hprec,
						       true)).sle (arg1_min))
		    op1_medium_p = true;
		  if (!arg1_min.is_negative ())
		    op1_sign = 0;
		  else if (arg1_max.is_negative ())
		    op1_sign = -1;
		}
	    }

	  int smaller_sign = 1;
	  int larger_sign = 1;
	  if (op0_small_p)
	    {
	      smaller_sign = op0_sign;
	      larger_sign = op1_sign;
	    }
	  else if (op1_small_p)
	    {
	      smaller_sign = op1_sign;
	      larger_sign = op0_sign;
	    }
	  else if (op0_sign == op1_sign)
	    {
	      smaller_sign = op0_sign;
	      larger_sign = op0_sign;
	    }

	  if (!op0_small_p)
	    emit_cmp_and_jump_insns (signbit0, hipart0, NE, NULL_RTX, hmode,
				     false, large_op0, PROB_UNLIKELY);

	  if (!op1_small_p)
	    emit_cmp_and_jump_insns (signbit1, hipart1, NE, NULL_RTX, hmode,
				     false, small_op0_large_op1,
				     PROB_UNLIKELY);

	  /* If both op0 and op1 are sign extended from hmode to mode,
	     the multiplication will never overflow.  We can do just one
	     hmode x hmode => mode widening multiplication.  */
	  if (GET_CODE (lopart0) == SUBREG)
	    {
	      SUBREG_PROMOTED_VAR_P (lopart0) = 1;
	      SUBREG_PROMOTED_UNSIGNED_SET (lopart0, 0);
	    }
	  if (GET_CODE (lopart1) == SUBREG)
	    {
	      SUBREG_PROMOTED_VAR_P (lopart1) = 1;
	      SUBREG_PROMOTED_UNSIGNED_SET (lopart1, 0);
	    }
	  tree halfstype = build_nonstandard_integer_type (hprec, 0);
	  ops.op0 = make_tree (halfstype, lopart0);
	  ops.op1 = make_tree (halfstype, lopart1);
	  ops.code = WIDEN_MULT_EXPR;
	  ops.type = TREE_TYPE (arg0);
	  rtx thisres
	    = expand_expr_real_2 (&ops, NULL_RTX, mode, EXPAND_NORMAL);
	  emit_move_insn (res, thisres);
	  emit_jump (done_label);

	  emit_label (small_op0_large_op1);

	  /* If op0 is sign extended from hmode to mode, but op1 is not,
	     just swap the arguments and handle it as op1 sign extended,
	     op0 not.  */
	  rtx larger = gen_reg_rtx (mode);
	  rtx hipart = gen_reg_rtx (hmode);
	  rtx lopart = gen_reg_rtx (hmode);
	  emit_move_insn (larger, op1);
	  emit_move_insn (hipart, hipart1);
	  emit_move_insn (lopart, lopart0);
	  emit_jump (one_small_one_large);

	  emit_label (large_op0);

	  if (!op1_small_p)
	    emit_cmp_and_jump_insns (signbit1, hipart1, NE, NULL_RTX, hmode,
				     false, both_ops_large, PROB_UNLIKELY);

	  /* If op1 is sign extended from hmode to mode, but op0 is not,
	     prepare larger, hipart and lopart pseudos and handle it together
	     with small_op0_large_op1.  */
	  emit_move_insn (larger, op0);
	  emit_move_insn (hipart, hipart0);
	  emit_move_insn (lopart, lopart1);

	  emit_label (one_small_one_large);

	  /* lopart is the low part of the operand that is sign extended
	     to mode, larger is the the other operand, hipart is the
	     high part of larger and lopart0 and lopart1 are the low parts
	     of both operands.
	     We perform lopart0 * lopart1 and lopart * hipart widening
	     multiplications.  */
	  tree halfutype = build_nonstandard_integer_type (hprec, 1);
	  ops.op0 = make_tree (halfutype, lopart0);
	  ops.op1 = make_tree (halfutype, lopart1);
	  rtx lo0xlo1
	    = expand_expr_real_2 (&ops, NULL_RTX, mode, EXPAND_NORMAL);

	  ops.op0 = make_tree (halfutype, lopart);
	  ops.op1 = make_tree (halfutype, hipart);
	  rtx loxhi = gen_reg_rtx (mode);
	  rtx tem = expand_expr_real_2 (&ops, NULL_RTX, mode, EXPAND_NORMAL);
	  emit_move_insn (loxhi, tem);

	  /* if (hipart < 0) loxhi -= lopart << (bitsize / 2);  */
	  if (larger_sign == 0)
	    emit_jump (after_hipart_neg);
	  else if (larger_sign != -1)
	    emit_cmp_and_jump_insns (hipart, const0_rtx, GE, NULL_RTX, hmode,
				     false, after_hipart_neg, PROB_EVEN);

	  tem = convert_modes (mode, hmode, lopart, 1);
	  tem = expand_shift (LSHIFT_EXPR, mode, tem, hprec, NULL_RTX, 1);
	  tem = expand_simple_binop (mode, MINUS, loxhi, tem, NULL_RTX,
				     1, OPTAB_DIRECT);
	  emit_move_insn (loxhi, tem);

	  emit_label (after_hipart_neg);

	  /* if (lopart < 0) loxhi -= larger;  */
	  if (smaller_sign == 0)
	    emit_jump (after_lopart_neg);
	  else if (smaller_sign != -1)
	    emit_cmp_and_jump_insns (lopart, const0_rtx, GE, NULL_RTX, hmode,
				     false, after_lopart_neg, PROB_EVEN);

	  tem = expand_simple_binop (mode, MINUS, loxhi, larger, NULL_RTX,
				     1, OPTAB_DIRECT);
	  emit_move_insn (loxhi, tem);

	  emit_label (after_lopart_neg);

	  /* loxhi += (uns) lo0xlo1 >> (bitsize / 2);  */
	  tem = expand_shift (RSHIFT_EXPR, mode, lo0xlo1, hprec, NULL_RTX, 1);
	  tem = expand_simple_binop (mode, PLUS, loxhi, tem, NULL_RTX,
				     1, OPTAB_DIRECT);
	  emit_move_insn (loxhi, tem);

	  /* if (loxhi >> (bitsize / 2)
		 == (hmode) loxhi >> (bitsize / 2 - 1))  */
	  rtx hipartloxhi = expand_shift (RSHIFT_EXPR, mode, loxhi, hprec,
					  NULL_RTX, 0);
	  hipartloxhi = gen_lowpart (hmode, hipartloxhi);
	  rtx lopartloxhi = gen_lowpart (hmode, loxhi);
	  rtx signbitloxhi = expand_shift (RSHIFT_EXPR, hmode, lopartloxhi,
					   hprec - 1, NULL_RTX, 0);

	  emit_cmp_and_jump_insns (signbitloxhi, hipartloxhi, NE, NULL_RTX,
				   hmode, false, do_overflow,
				   PROB_VERY_UNLIKELY);

	  /* res = (loxhi << (bitsize / 2)) | (hmode) lo0xlo1;  */
	  rtx loxhishifted = expand_shift (LSHIFT_EXPR, mode, loxhi, hprec,
					   NULL_RTX, 1);
	  tem = convert_modes (mode, hmode, gen_lowpart (hmode, lo0xlo1), 1);

	  tem = expand_simple_binop (mode, IOR, loxhishifted, tem, res,
				     1, OPTAB_DIRECT);
	  if (tem != res)
	    emit_move_insn (res, tem);
	  emit_jump (done_label);

	  emit_label (both_ops_large);

	  /* If both operands are large (not sign extended from hmode),
	     then perform the full multiplication which will be the result
	     of the operation.  The only cases which don't overflow are
	     some cases where both hipart0 and highpart1 are 0 or -1.  */
	  ops.code = MULT_EXPR;
	  ops.op0 = make_tree (TREE_TYPE (arg0), op0);
	  ops.op1 = make_tree (TREE_TYPE (arg0), op1);
	  tem = expand_expr_real_2 (&ops, NULL_RTX, mode, EXPAND_NORMAL);
	  emit_move_insn (res, tem);

	  if (!op0_medium_p)
	    {
	      tem = expand_simple_binop (hmode, PLUS, hipart0, const1_rtx,
					 NULL_RTX, 1, OPTAB_DIRECT);
	      emit_cmp_and_jump_insns (tem, const1_rtx, GTU, NULL_RTX, hmode,
				       true, do_error, PROB_VERY_UNLIKELY);
	    }

	  if (!op1_medium_p)
	    {
	      tem = expand_simple_binop (hmode, PLUS, hipart1, const1_rtx,
					 NULL_RTX, 1, OPTAB_DIRECT);
	      emit_cmp_and_jump_insns (tem, const1_rtx, GTU, NULL_RTX, hmode,
				       true, do_error, PROB_VERY_UNLIKELY);
	    }

	  /* At this point hipart{0,1} are both in [-1, 0].  If they are the
	     same, overflow happened if res is negative, if they are different,
	     overflow happened if res is positive.  */
	  if (op0_sign != 1 && op1_sign != 1 && op0_sign != op1_sign)
	    emit_jump (hipart_different);
	  else if (op0_sign == 1 || op1_sign == 1)
	    emit_cmp_and_jump_insns (hipart0, hipart1, NE, NULL_RTX, hmode,
				     true, hipart_different, PROB_EVEN);

	  emit_cmp_and_jump_insns (res, const0_rtx, LT, NULL_RTX, mode, false,
				   do_error, PROB_VERY_UNLIKELY);
	  emit_jump (done_label);

	  emit_label (hipart_different);

	  emit_cmp_and_jump_insns (res, const0_rtx, GE, NULL_RTX, mode, false,
				   do_error, PROB_VERY_UNLIKELY);
	  emit_jump (done_label);

	  emit_label (do_overflow);

	  /* Overflow, do full multiplication and fallthru into do_error.  */
	  ops.op0 = make_tree (TREE_TYPE (arg0), op0);
	  ops.op1 = make_tree (TREE_TYPE (arg0), op1);
	  tem = expand_expr_real_2 (&ops, NULL_RTX, mode, EXPAND_NORMAL);
	  emit_move_insn (res, tem);
	}
      else
	{
	  ops.code = MULT_EXPR;
	  ops.type = TREE_TYPE (arg0);
	  res = expand_expr_real_2 (&ops, NULL_RTX, mode, EXPAND_NORMAL);
	  emit_jump (done_label);
	}
    }

  emit_label (do_error);
  /* Expand the ubsan builtin call.  */
  push_temp_slots ();
  fn = ubsan_build_overflow_builtin (MULT_EXPR, gimple_location (stmt),
				     TREE_TYPE (arg0), arg0, arg1);
  expand_normal (fn);
  pop_temp_slots ();
  do_pending_stack_adjust ();

  /* We're done.  */
  emit_label (done_label);

  if (lhs)
    emit_move_insn (target, res);
}

/* Expand UBSAN_CHECK_ADD call STMT.  */

static void
expand_UBSAN_CHECK_ADD (gimple stmt)
{
  ubsan_expand_si_overflow_addsub_check (PLUS_EXPR, stmt);
}

/* Expand UBSAN_CHECK_SUB call STMT.  */

static void
expand_UBSAN_CHECK_SUB (gimple stmt)
{
  if (integer_zerop (gimple_call_arg (stmt, 0)))
    ubsan_expand_si_overflow_neg_check (stmt);
  else
    ubsan_expand_si_overflow_addsub_check (MINUS_EXPR, stmt);
}

/* Expand UBSAN_CHECK_MUL call STMT.  */

static void
expand_UBSAN_CHECK_MUL (gimple stmt)
{
  ubsan_expand_si_overflow_mul_check (stmt);
}

/* This should get folded in tree-vectorizer.c.  */

static void
expand_LOOP_VECTORIZED (gimple stmt ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

static void
expand_MASK_LOAD (gimple stmt)
{
  struct expand_operand ops[3];
  tree type, lhs, rhs, maskt;
  rtx mem, target, mask;

  maskt = gimple_call_arg (stmt, 2);
  lhs = gimple_call_lhs (stmt);
  if (lhs == NULL_TREE)
    return;
  type = TREE_TYPE (lhs);
  rhs = fold_build2 (MEM_REF, type, gimple_call_arg (stmt, 0),
		     gimple_call_arg (stmt, 1));

  mem = expand_expr (rhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  gcc_assert (MEM_P (mem));
  mask = expand_normal (maskt);
  target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  create_output_operand (&ops[0], target, TYPE_MODE (type));
  create_fixed_operand (&ops[1], mem);
  create_input_operand (&ops[2], mask, TYPE_MODE (TREE_TYPE (maskt)));
  expand_insn (optab_handler (maskload_optab, TYPE_MODE (type)), 3, ops);
}

static void
expand_MASK_STORE (gimple stmt)
{
  struct expand_operand ops[3];
  tree type, lhs, rhs, maskt;
  rtx mem, reg, mask;

  maskt = gimple_call_arg (stmt, 2);
  rhs = gimple_call_arg (stmt, 3);
  type = TREE_TYPE (rhs);
  lhs = fold_build2 (MEM_REF, type, gimple_call_arg (stmt, 0),
		     gimple_call_arg (stmt, 1));

  mem = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  gcc_assert (MEM_P (mem));
  mask = expand_normal (maskt);
  reg = expand_normal (rhs);
  create_fixed_operand (&ops[0], mem);
  create_input_operand (&ops[1], reg, TYPE_MODE (type));
  create_input_operand (&ops[2], mask, TYPE_MODE (TREE_TYPE (maskt)));
  expand_insn (optab_handler (maskstore_optab, TYPE_MODE (type)), 3, ops);
}

static void
expand_ABNORMAL_DISPATCHER (gimple)
{
}

/* Routines to expand each internal function, indexed by function number.
   Each routine has the prototype:

       expand_<NAME> (gimple stmt)

   where STMT is the statement that performs the call. */
static void (*const internal_fn_expanders[]) (gimple) = {
#define DEF_INTERNAL_FN(CODE, FLAGS) expand_##CODE,
#include "internal-fn.def"
#undef DEF_INTERNAL_FN
  0
};

/* Expand STMT, which is a call to internal function FN.  */

void
expand_internal_call (gimple stmt)
{
  internal_fn_expanders[(int) gimple_call_internal_fn (stmt)] (stmt);
}
