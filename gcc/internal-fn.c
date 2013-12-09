/* Internal functions.
   Copyright (C) 2011-2013 Free Software Foundation, Inc.

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

      /* Compute the operation.  On RTL level, the addition is always
	 unsigned.  */
      res = expand_binop (mode, add_optab, op0, op1,
			  NULL_RTX, false, OPTAB_LIB_WIDEN);

      /* If the op1 is negative, we have to use a different check.  */
      emit_cmp_and_jump_insns (op1, const0_rtx, LT, NULL_RTX, mode,
			       false, sub_check, PROB_EVEN);

      /* Compare the result of the addition with one of the operands.  */
      emit_cmp_and_jump_insns (res, op0, code == PLUS_EXPR ? GE : LE,
			       NULL_RTX, mode, false, done_label,
			       PROB_VERY_LIKELY);
      /* If we get here, we have to print the error.  */
      emit_jump (do_error);

      emit_label (sub_check);
      /* We have k = a + b for b < 0 here.  k <= a must hold.  */
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
      else
	{
	  /* For now we don't instrument this.  See __mulvDI3 in libgcc2.c
	     for what could be done.  */
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
