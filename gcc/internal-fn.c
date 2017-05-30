/* Internal functions.
   Copyright (C) 2011-2015 Free Software Foundation, Inc.

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
#include "hash-set.h"
#include "machmode.h"
#include "vec.h"
#include "double-int.h"
#include "input.h"
#include "alias.h"
#include "symtab.h"
#include "options.h"
#include "wide-int.h"
#include "inchash.h"
#include "tree.h"
#include "fold-const.h"
#include "internal-fn.h"
#include "stor-layout.h"
#include "hashtab.h"
#include "tm.h"
#include "hard-reg-set.h"
#include "function.h"
#include "rtl.h"
#include "flags.h"
#include "statistics.h"
#include "real.h"
#include "fixed-value.h"
#include "insn-config.h"
#include "expmed.h"
#include "dojump.h"
#include "explow.h"
#include "calls.h"
#include "emit-rtl.h"
#include "varasm.h"
#include "stmt.h"
#include "expr.h"
#include "insn-codes.h"
#include "optabs.h"
#include "predict.h"
#include "dominance.h"
#include "cfg.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "ubsan.h"
#include "target.h"
#include "stringpool.h"
#include "tree-ssanames.h"
#include "diagnostic-core.h"

/* The names of each internal function, indexed by function number.  */
const char *const internal_fn_name_array[] = {
#define DEF_INTERNAL_FN(CODE, FLAGS, FNSPEC) #CODE,
#include "internal-fn.def"
#undef DEF_INTERNAL_FN
  "<invalid-fn>"
};

/* The ECF_* flags of each internal function, indexed by function number.  */
const int internal_fn_flags_array[] = {
#define DEF_INTERNAL_FN(CODE, FLAGS, FNSPEC) FLAGS,
#include "internal-fn.def"
#undef DEF_INTERNAL_FN
  0
};

/* Fnspec of each internal function, indexed by function number.  */
const_tree internal_fn_fnspec_array[IFN_LAST + 1];

void
init_internal_fns ()
{
#define DEF_INTERNAL_FN(CODE, FLAGS, FNSPEC) \
  if (FNSPEC) internal_fn_fnspec_array[IFN_##CODE] = \
    build_string ((int) sizeof (FNSPEC), FNSPEC ? FNSPEC : "");
#include "internal-fn.def"
#undef DEF_INTERNAL_FN
  internal_fn_fnspec_array[IFN_LAST] = 0;
}

/* ARRAY_TYPE is an array of vector modes.  Return the associated insn
   for load-lanes-style optab OPTAB.  The insn must exist.  */

static enum insn_code
get_multi_vector_move (tree array_type, convert_optab optab)
{
  enum insn_code icode;
  machine_mode imode;
  machine_mode vmode;

  gcc_assert (TREE_CODE (array_type) == ARRAY_TYPE);
  imode = TYPE_MODE (array_type);
  vmode = TYPE_MODE (TREE_TYPE (array_type));

  icode = convert_optab_handler (optab, imode, vmode);
  gcc_assert (icode != CODE_FOR_nothing);
  return icode;
}

/* Expand LOAD_LANES call STMT.  */

static void
expand_LOAD_LANES (gcall *stmt)
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
expand_STORE_LANES (gcall *stmt)
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
expand_ANNOTATE (gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in adjust_simduid_builtins.  */

static void
expand_GOMP_SIMD_LANE (gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in adjust_simduid_builtins.  */

static void
expand_GOMP_SIMD_VF (gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in adjust_simduid_builtins.  */

static void
expand_GOMP_SIMD_LAST_LANE (gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in the sanopt pass.  */

static void
expand_UBSAN_NULL (gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in the sanopt pass.  */

static void
expand_UBSAN_BOUNDS (gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in the sanopt pass.  */

static void
expand_UBSAN_VPTR (gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in the sanopt pass.  */

static void
expand_UBSAN_OBJECT_SIZE (gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in the sanopt pass.  */

static void
expand_ASAN_CHECK (gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in the tsan pass.  */

static void
expand_TSAN_FUNC_EXIT (gcall *)
{
  gcc_unreachable ();
}

/* Helper function for expand_addsub_overflow.  Return 1
   if ARG interpreted as signed in its precision is known to be always
   positive or 2 if ARG is known to be always negative, or 3 if ARG may
   be positive or negative.  */

static int
get_range_pos_neg (tree arg)
{
  if (arg == error_mark_node)
    return 3;

  int prec = TYPE_PRECISION (TREE_TYPE (arg));
  int cnt = 0;
  if (TREE_CODE (arg) == INTEGER_CST)
    {
      wide_int w = wi::sext (arg, prec);
      if (wi::neg_p (w))
	return 2;
      else
	return 1;
    }
  while (CONVERT_EXPR_P (arg)
	 && INTEGRAL_TYPE_P (TREE_TYPE (TREE_OPERAND (arg, 0)))
	 && TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (arg, 0))) <= prec)
    {
      arg = TREE_OPERAND (arg, 0);
      /* Narrower value zero extended into wider type
	 will always result in positive values.  */
      if (TYPE_UNSIGNED (TREE_TYPE (arg))
	  && TYPE_PRECISION (TREE_TYPE (arg)) < prec)
	return 1;
      prec = TYPE_PRECISION (TREE_TYPE (arg));
      if (++cnt > 30)
	return 3;
    }

  if (TREE_CODE (arg) != SSA_NAME)
    return 3;
  wide_int arg_min, arg_max;
  while (get_range_info (arg, &arg_min, &arg_max) != VR_RANGE)
    {
      gimple g = SSA_NAME_DEF_STMT (arg);
      if (is_gimple_assign (g)
	  && CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (g)))
	{
	  tree t = gimple_assign_rhs1 (g);
	  if (INTEGRAL_TYPE_P (TREE_TYPE (t))
	      && TYPE_PRECISION (TREE_TYPE (t)) <= prec)
	    {
	      if (TYPE_UNSIGNED (TREE_TYPE (t))
		  && TYPE_PRECISION (TREE_TYPE (t)) < prec)
		return 1;
	      prec = TYPE_PRECISION (TREE_TYPE (t));
	      arg = t;
	      if (++cnt > 30)
		return 3;
	      continue;
	    }
	}
      return 3;
    }
  if (TYPE_UNSIGNED (TREE_TYPE (arg)))
    {
      /* For unsigned values, the "positive" range comes
	 below the "negative" range.  */
      if (!wi::neg_p (wi::sext (arg_max, prec), SIGNED))
	return 1;
      if (wi::neg_p (wi::sext (arg_min, prec), SIGNED))
	return 2;
    }
  else
    {
      if (!wi::neg_p (wi::sext (arg_min, prec), SIGNED))
	return 1;
      if (wi::neg_p (wi::sext (arg_max, prec), SIGNED))
	return 2;
    }
  return 3;
}

/* Return minimum precision needed to represent all values
   of ARG in SIGNed integral type.  */

static int
get_min_precision (tree arg, signop sign)
{
  int prec = TYPE_PRECISION (TREE_TYPE (arg));
  int cnt = 0;
  signop orig_sign = sign;
  if (TREE_CODE (arg) == INTEGER_CST)
    {
      int p;
      if (TYPE_SIGN (TREE_TYPE (arg)) != sign)
	{
	  widest_int w = wi::to_widest (arg);
	  w = wi::ext (w, prec, sign);
	  p = wi::min_precision (w, sign);
	}
      else
	p = wi::min_precision (arg, sign);
      return MIN (p, prec);
    }
  while (CONVERT_EXPR_P (arg)
	 && INTEGRAL_TYPE_P (TREE_TYPE (TREE_OPERAND (arg, 0)))
	 && TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (arg, 0))) <= prec)
    {
      arg = TREE_OPERAND (arg, 0);
      if (TYPE_PRECISION (TREE_TYPE (arg)) < prec)
	{
	  if (TYPE_UNSIGNED (TREE_TYPE (arg)))
	    sign = UNSIGNED;
	  else if (sign == UNSIGNED && get_range_pos_neg (arg) != 1)
	    return prec + (orig_sign != sign);
	  prec = TYPE_PRECISION (TREE_TYPE (arg));
	}
      if (++cnt > 30)
	return prec + (orig_sign != sign);
    }
  if (TREE_CODE (arg) != SSA_NAME)
    return prec + (orig_sign != sign);
  wide_int arg_min, arg_max;
  while (get_range_info (arg, &arg_min, &arg_max) != VR_RANGE)
    {
      gimple g = SSA_NAME_DEF_STMT (arg);
      if (is_gimple_assign (g)
	  && CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (g)))
	{
	  tree t = gimple_assign_rhs1 (g);
	  if (INTEGRAL_TYPE_P (TREE_TYPE (t))
	      && TYPE_PRECISION (TREE_TYPE (t)) <= prec)
	    {
	      arg = t;
	      if (TYPE_PRECISION (TREE_TYPE (arg)) < prec)
		{
		  if (TYPE_UNSIGNED (TREE_TYPE (arg)))
		    sign = UNSIGNED;
		  else if (sign == UNSIGNED && get_range_pos_neg (arg) != 1)
		    return prec + (orig_sign != sign);
		  prec = TYPE_PRECISION (TREE_TYPE (arg));
		}
	      if (++cnt > 30)
		return prec + (orig_sign != sign);
	      continue;
	    }
	}
      return prec + (orig_sign != sign);
    }
  if (sign == TYPE_SIGN (TREE_TYPE (arg)))
    {
      int p1 = wi::min_precision (arg_min, sign);
      int p2 = wi::min_precision (arg_max, sign);
      p1 = MAX (p1, p2);
      prec = MIN (prec, p1);
    }
  else if (sign == UNSIGNED && !wi::neg_p (arg_min, SIGNED))
    {
      int p = wi::min_precision (arg_max, SIGNED);
      prec = MIN (prec, p);
    }
  return prec + (orig_sign != sign);
}

/* Helper for expand_*_overflow.  Store RES into the __real__ part
   of TARGET.  If RES has larger MODE than __real__ part of TARGET,
   set the __imag__ part to 1 if RES doesn't fit into it.  */

static void
expand_arith_overflow_result_store (tree lhs, rtx target,
				    machine_mode mode, rtx res)
{
  machine_mode tgtmode = GET_MODE_INNER (GET_MODE (target));
  rtx lres = res;
  if (tgtmode != mode)
    {
      rtx_code_label *done_label = gen_label_rtx ();
      int uns = TYPE_UNSIGNED (TREE_TYPE (TREE_TYPE (lhs)));
      lres = convert_modes (tgtmode, mode, res, uns);
      gcc_assert (GET_MODE_PRECISION (tgtmode) < GET_MODE_PRECISION (mode));
      do_compare_rtx_and_jump (res, convert_modes (mode, tgtmode, lres, uns),
			       EQ, true, mode, NULL_RTX, NULL_RTX, done_label,
			       PROB_VERY_LIKELY);
      write_complex_part (target, const1_rtx, true);
      emit_label (done_label);
    }
  write_complex_part (target, lres, false);
}

/* Helper for expand_*_overflow.  Store RES into TARGET.  */

static void
expand_ubsan_result_store (rtx target, rtx res)
{
  if (GET_CODE (target) == SUBREG && SUBREG_PROMOTED_VAR_P (target))
    /* If this is a scalar in a register that is stored in a wider mode   
       than the declared mode, compute the result into its declared mode
       and then convert to the wider mode.  Our value is the computed
       expression.  */
    convert_move (SUBREG_REG (target), res, SUBREG_PROMOTED_SIGN (target));
  else
    emit_move_insn (target, res);
}

/* Add sub/add overflow checking to the statement STMT.
   CODE says whether the operation is +, or -.  */

static void
expand_addsub_overflow (location_t loc, tree_code code, tree lhs,
			tree arg0, tree arg1, bool unsr_p, bool uns0_p,
			bool uns1_p, bool is_ubsan)
{
  rtx res, target = NULL_RTX;
  tree fn;
  rtx_code_label *done_label = gen_label_rtx ();
  rtx_code_label *do_error = gen_label_rtx ();
  do_pending_stack_adjust ();
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  machine_mode mode = TYPE_MODE (TREE_TYPE (arg0));
  int prec = GET_MODE_PRECISION (mode);
  rtx sgn = immed_wide_int_const (wi::min_value (prec, SIGNED), mode);
  bool do_xor = false;

  if (is_ubsan)
    gcc_assert (!unsr_p && !uns0_p && !uns1_p);

  if (lhs)
    {
      target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
      if (!is_ubsan)
	write_complex_part (target, const0_rtx, true);
    }

  /* We assume both operands and result have the same precision
     here (GET_MODE_BITSIZE (mode)), S stands for signed type
     with that precision, U for unsigned type with that precision,
     sgn for unsigned most significant bit in that precision.
     s1 is signed first operand, u1 is unsigned first operand,
     s2 is signed second operand, u2 is unsigned second operand,
     sr is signed result, ur is unsigned result and the following
     rules say how to compute result (which is always result of
     the operands as if both were unsigned, cast to the right
     signedness) and how to compute whether operation overflowed.

     s1 + s2 -> sr
	res = (S) ((U) s1 + (U) s2)
	ovf = s2 < 0 ? res > s1 : res < s1 (or jump on overflow)
     s1 - s2 -> sr
	res = (S) ((U) s1 - (U) s2)
	ovf = s2 < 0 ? res < s1 : res > s2 (or jump on overflow)
     u1 + u2 -> ur
	res = u1 + u2
	ovf = res < u1 (or jump on carry, but RTL opts will handle it)
     u1 - u2 -> ur
	res = u1 - u2
	ovf = res > u1 (or jump on carry, but RTL opts will handle it)
     s1 + u2 -> sr
	res = (S) ((U) s1 + u2)
	ovf = ((U) res ^ sgn) < u2
     s1 + u2 -> ur
	t1 = (S) (u2 ^ sgn)
	t2 = s1 + t1
	res = (U) t2 ^ sgn
	ovf = t1 < 0 ? t2 > s1 : t2 < s1 (or jump on overflow)
     s1 - u2 -> sr
	res = (S) ((U) s1 - u2)
	ovf = u2 > ((U) s1 ^ sgn)
     s1 - u2 -> ur
	res = (U) s1 - u2
	ovf = s1 < 0 || u2 > (U) s1
     u1 - s2 -> sr
	res = u1 - (U) s2
 	ovf = u1 >= ((U) s2 ^ sgn)
     u1 - s2 -> ur
	t1 = u1 ^ sgn
	t2 = t1 - (U) s2
	res = t2 ^ sgn
	ovf = s2 < 0 ? (S) t2 < (S) t1 : (S) t2 > (S) t1 (or jump on overflow)
     s1 + s2 -> ur
	res = (U) s1 + (U) s2
	ovf = s2 < 0 ? (s1 | (S) res) < 0) : (s1 & (S) res) < 0)
     u1 + u2 -> sr
	res = (S) (u1 + u2)
	ovf = (U) res < u2 || res < 0
     u1 - u2 -> sr
	res = (S) (u1 - u2)
	ovf = u1 >= u2 ? res < 0 : res >= 0
     s1 - s2 -> ur
	res = (U) s1 - (U) s2
	ovf = s2 >= 0 ? ((s1 | (S) res) < 0) : ((s1 & (S) res) < 0)  */

  if (code == PLUS_EXPR && uns0_p && !uns1_p)
    {
      /* PLUS_EXPR is commutative, if operand signedness differs,
	 canonicalize to the first operand being signed and second
	 unsigned to simplify following code.  */
      rtx tem = op1;
      op1 = op0;
      op0 = tem;
      tree t = arg1;
      arg1 = arg0;
      arg0 = t;
      uns0_p = 0;
      uns1_p = 1;
    }

  /* u1 +- u2 -> ur  */
  if (uns0_p && uns1_p && unsr_p)
    {
      /* Compute the operation.  On RTL level, the addition is always
	 unsigned.  */
      res = expand_binop (mode, code == PLUS_EXPR ? add_optab : sub_optab,
			  op0, op1, NULL_RTX, false, OPTAB_LIB_WIDEN);
      rtx tem = op0;
      /* For PLUS_EXPR, the operation is commutative, so we can pick
	 operand to compare against.  For prec <= BITS_PER_WORD, I think
	 preferring REG operand is better over CONST_INT, because
	 the CONST_INT might enlarge the instruction or CSE would need
	 to figure out we'd already loaded it into a register before.
	 For prec > BITS_PER_WORD, I think CONST_INT might be more beneficial,
	 as then the multi-word comparison can be perhaps simplified.  */
      if (code == PLUS_EXPR
	  && (prec <= BITS_PER_WORD
	      ? (CONST_SCALAR_INT_P (op0) && REG_P (op1))
	      : CONST_SCALAR_INT_P (op1)))
	tem = op1;
      do_compare_rtx_and_jump (res, tem, code == PLUS_EXPR ? GEU : LEU,
			       true, mode, NULL_RTX, NULL_RTX, done_label,
			       PROB_VERY_LIKELY);
      goto do_error_label;
    }

  /* s1 +- u2 -> sr  */
  if (!uns0_p && uns1_p && !unsr_p)
    {
      /* Compute the operation.  On RTL level, the addition is always
	 unsigned.  */
      res = expand_binop (mode, code == PLUS_EXPR ? add_optab : sub_optab,
			  op0, op1, NULL_RTX, false, OPTAB_LIB_WIDEN);
      rtx tem = expand_binop (mode, add_optab,
			      code == PLUS_EXPR ? res : op0, sgn,
			      NULL_RTX, false, OPTAB_LIB_WIDEN);
      do_compare_rtx_and_jump (tem, op1, GEU, true, mode, NULL_RTX, NULL_RTX,
			       done_label, PROB_VERY_LIKELY);
      goto do_error_label;
    }

  /* s1 + u2 -> ur  */
  if (code == PLUS_EXPR && !uns0_p && uns1_p && unsr_p)
    {
      op1 = expand_binop (mode, add_optab, op1, sgn, NULL_RTX, false,
			  OPTAB_LIB_WIDEN);
      /* As we've changed op1, we have to avoid using the value range
	 for the original argument.  */
      arg1 = error_mark_node;
      do_xor = true;
      goto do_signed;
    }

  /* u1 - s2 -> ur  */
  if (code == MINUS_EXPR && uns0_p && !uns1_p && unsr_p)
    {
      op0 = expand_binop (mode, add_optab, op0, sgn, NULL_RTX, false,
			  OPTAB_LIB_WIDEN);
      /* As we've changed op0, we have to avoid using the value range
	 for the original argument.  */
      arg0 = error_mark_node;
      do_xor = true;
      goto do_signed;
    }

  /* s1 - u2 -> ur  */
  if (code == MINUS_EXPR && !uns0_p && uns1_p && unsr_p)
    {
      /* Compute the operation.  On RTL level, the addition is always
	 unsigned.  */
      res = expand_binop (mode, sub_optab, op0, op1, NULL_RTX, false,
			  OPTAB_LIB_WIDEN);
      int pos_neg = get_range_pos_neg (arg0);
      if (pos_neg == 2)
	/* If ARG0 is known to be always negative, this is always overflow.  */
	emit_jump (do_error);
      else if (pos_neg == 3)
	/* If ARG0 is not known to be always positive, check at runtime.  */
	do_compare_rtx_and_jump (op0, const0_rtx, LT, false, mode, NULL_RTX,
				 NULL_RTX, do_error, PROB_VERY_UNLIKELY);
      do_compare_rtx_and_jump (op1, op0, LEU, true, mode, NULL_RTX, NULL_RTX,
			       done_label, PROB_VERY_LIKELY);
      goto do_error_label;
    }

  /* u1 - s2 -> sr  */
  if (code == MINUS_EXPR && uns0_p && !uns1_p && !unsr_p)
    {
      /* Compute the operation.  On RTL level, the addition is always
	 unsigned.  */
      res = expand_binop (mode, sub_optab, op0, op1, NULL_RTX, false,
			  OPTAB_LIB_WIDEN);
      rtx tem = expand_binop (mode, add_optab, op1, sgn, NULL_RTX, false,
			      OPTAB_LIB_WIDEN);
      do_compare_rtx_and_jump (op0, tem, LTU, true, mode, NULL_RTX, NULL_RTX,
			       done_label, PROB_VERY_LIKELY);
      goto do_error_label;
    }

  /* u1 + u2 -> sr  */
  if (code == PLUS_EXPR && uns0_p && uns1_p && !unsr_p)
    {
      /* Compute the operation.  On RTL level, the addition is always
	 unsigned.  */
      res = expand_binop (mode, add_optab, op0, op1, NULL_RTX, false,
			  OPTAB_LIB_WIDEN);
      do_compare_rtx_and_jump (res, const0_rtx, LT, false, mode, NULL_RTX,
			       NULL_RTX, do_error, PROB_VERY_UNLIKELY);
      rtx tem = op1;
      /* The operation is commutative, so we can pick operand to compare
	 against.  For prec <= BITS_PER_WORD, I think preferring REG operand
	 is better over CONST_INT, because the CONST_INT might enlarge the
	 instruction or CSE would need to figure out we'd already loaded it
	 into a register before.  For prec > BITS_PER_WORD, I think CONST_INT
	 might be more beneficial, as then the multi-word comparison can be
	 perhaps simplified.  */
      if (prec <= BITS_PER_WORD
	  ? (CONST_SCALAR_INT_P (op1) && REG_P (op0))
	  : CONST_SCALAR_INT_P (op0))
	tem = op0;
      do_compare_rtx_and_jump (res, tem, GEU, true, mode, NULL_RTX, NULL_RTX,
			       done_label, PROB_VERY_LIKELY);
      goto do_error_label;
    }

  /* s1 +- s2 -> ur  */
  if (!uns0_p && !uns1_p && unsr_p)
    {
      /* Compute the operation.  On RTL level, the addition is always
	 unsigned.  */
      res = expand_binop (mode, code == PLUS_EXPR ? add_optab : sub_optab,
			  op0, op1, NULL_RTX, false, OPTAB_LIB_WIDEN);
      int pos_neg = get_range_pos_neg (arg1);
      if (code == PLUS_EXPR)
	{
	  int pos_neg0 = get_range_pos_neg (arg0);
	  if (pos_neg0 != 3 && pos_neg == 3)
	    {
	      rtx tem = op1;
	      op1 = op0;
	      op0 = tem;
	      pos_neg = pos_neg0;
	    }
	}
      rtx tem;
      if (pos_neg != 3)
	{
	  tem = expand_binop (mode, ((pos_neg == 1) ^ (code == MINUS_EXPR))
				    ? and_optab : ior_optab,
			      op0, res, NULL_RTX, false, OPTAB_LIB_WIDEN);
	  do_compare_rtx_and_jump (tem, const0_rtx, GE, false, mode, NULL_RTX,
				   NULL_RTX, done_label, PROB_VERY_LIKELY);
	}
      else
	{
	  rtx_code_label *do_ior_label = gen_label_rtx ();
	  do_compare_rtx_and_jump (op1, const0_rtx,
				   code == MINUS_EXPR ? GE : LT, false, mode,
				   NULL_RTX, NULL_RTX, do_ior_label,
				   PROB_EVEN);
	  tem = expand_binop (mode, and_optab, op0, res, NULL_RTX, false,
			      OPTAB_LIB_WIDEN);
	  do_compare_rtx_and_jump (tem, const0_rtx, GE, false, mode, NULL_RTX,
				   NULL_RTX, done_label, PROB_VERY_LIKELY);
	  emit_jump (do_error);
	  emit_label (do_ior_label);
	  tem = expand_binop (mode, ior_optab, op0, res, NULL_RTX, false,
			      OPTAB_LIB_WIDEN);
	  do_compare_rtx_and_jump (tem, const0_rtx, GE, false, mode, NULL_RTX,
				   NULL_RTX, done_label, PROB_VERY_LIKELY);
	}
      goto do_error_label;
    }

  /* u1 - u2 -> sr  */
  if (code == MINUS_EXPR && uns0_p && uns1_p && !unsr_p)
    {
      /* Compute the operation.  On RTL level, the addition is always
	 unsigned.  */
      res = expand_binop (mode, sub_optab, op0, op1, NULL_RTX, false,
			  OPTAB_LIB_WIDEN);
      rtx_code_label *op0_geu_op1 = gen_label_rtx ();
      do_compare_rtx_and_jump (op0, op1, GEU, true, mode, NULL_RTX, NULL_RTX,
			       op0_geu_op1, PROB_EVEN);
      do_compare_rtx_and_jump (res, const0_rtx, LT, false, mode, NULL_RTX,
			       NULL_RTX, done_label, PROB_VERY_LIKELY);
      emit_jump (do_error);
      emit_label (op0_geu_op1);
      do_compare_rtx_and_jump (res, const0_rtx, GE, false, mode, NULL_RTX,
			       NULL_RTX, done_label, PROB_VERY_LIKELY);
      goto do_error_label;
    }

  gcc_assert (!uns0_p && !uns1_p && !unsr_p);

  /* s1 +- s2 -> sr  */
 do_signed: ;
  enum insn_code icode;
  icode = optab_handler (code == PLUS_EXPR ? addv4_optab : subv4_optab, mode);
  if (icode != CODE_FOR_nothing)
    {
      struct expand_operand ops[4];
      rtx_insn *last = get_last_insn ();

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
      rtx_code_label *sub_check = gen_label_rtx ();
      int pos_neg = 3;

      /* Compute the operation.  On RTL level, the addition is always
	 unsigned.  */
      res = expand_binop (mode, code == PLUS_EXPR ? add_optab : sub_optab,
			  op0, op1, NULL_RTX, false, OPTAB_LIB_WIDEN);

      /* If we can prove one of the arguments (for MINUS_EXPR only
	 the second operand, as subtraction is not commutative) is always
	 non-negative or always negative, we can do just one comparison
	 and conditional jump instead of 2 at runtime, 3 present in the
	 emitted code.  If one of the arguments is CONST_INT, all we
	 need is to make sure it is op1, then the first
	 do_compare_rtx_and_jump will be just folded.  Otherwise try
	 to use range info if available.  */
      if (code == PLUS_EXPR && CONST_INT_P (op0))
	{
	  rtx tem = op0;
	  op0 = op1;
	  op1 = tem;
	}
      else if (CONST_INT_P (op1))
	;
      else if (code == PLUS_EXPR && TREE_CODE (arg0) == SSA_NAME)
	{
	  pos_neg = get_range_pos_neg (arg0);
	  if (pos_neg != 3)
	    {
	      rtx tem = op0;
	      op0 = op1;
	      op1 = tem;
	    }
	}
      if (pos_neg == 3 && !CONST_INT_P (op1) && TREE_CODE (arg1) == SSA_NAME)
	pos_neg = get_range_pos_neg (arg1);

      /* If the op1 is negative, we have to use a different check.  */
      if (pos_neg == 3)
	do_compare_rtx_and_jump (op1, const0_rtx, LT, false, mode, NULL_RTX,
				 NULL_RTX, sub_check, PROB_EVEN);

      /* Compare the result of the operation with one of the operands.  */
      if (pos_neg & 1)
	do_compare_rtx_and_jump (res, op0, code == PLUS_EXPR ? GE : LE,
				 false, mode, NULL_RTX, NULL_RTX, done_label,
				 PROB_VERY_LIKELY);

      /* If we get here, we have to print the error.  */
      if (pos_neg == 3)
	{
	  emit_jump (do_error);

	  emit_label (sub_check);
	}

      /* We have k = a + b for b < 0 here.  k <= a must hold.  */
      if (pos_neg & 2)
	do_compare_rtx_and_jump (res, op0, code == PLUS_EXPR ? LE : GE,
				 false, mode, NULL_RTX, NULL_RTX, done_label,
				 PROB_VERY_LIKELY);
    }

 do_error_label:
  emit_label (do_error);
  if (is_ubsan)
    {
      /* Expand the ubsan builtin call.  */
      push_temp_slots ();
      fn = ubsan_build_overflow_builtin (code, loc, TREE_TYPE (arg0),
					 arg0, arg1);
      expand_normal (fn);
      pop_temp_slots ();
      do_pending_stack_adjust ();
    }
  else if (lhs)
    write_complex_part (target, const1_rtx, true);

  /* We're done.  */
  emit_label (done_label);

  if (lhs)
    {
      if (is_ubsan)
	expand_ubsan_result_store (target, res);
      else
	{
	  if (do_xor)
	    res = expand_binop (mode, add_optab, res, sgn, NULL_RTX, false,
				OPTAB_LIB_WIDEN);

	  expand_arith_overflow_result_store (lhs, target, mode, res);
	}
    }
}

/* Add negate overflow checking to the statement STMT.  */

static void
expand_neg_overflow (location_t loc, tree lhs, tree arg1, bool is_ubsan)
{
  rtx res, op1;
  tree fn;
  rtx_code_label *done_label, *do_error;
  rtx target = NULL_RTX;

  done_label = gen_label_rtx ();
  do_error = gen_label_rtx ();

  do_pending_stack_adjust ();
  op1 = expand_normal (arg1);

  machine_mode mode = TYPE_MODE (TREE_TYPE (arg1));
  if (lhs)
    {
      target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
      if (!is_ubsan)
	write_complex_part (target, const0_rtx, true);
    }

  enum insn_code icode = optab_handler (negv3_optab, mode);
  if (icode != CODE_FOR_nothing)
    {
      struct expand_operand ops[3];
      rtx_insn *last = get_last_insn ();

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
      do_compare_rtx_and_jump (op1, minv, NE, true, mode, NULL_RTX, NULL_RTX,
			       done_label, PROB_VERY_LIKELY);
    }

  emit_label (do_error);
  if (is_ubsan)
    {
      /* Expand the ubsan builtin call.  */
      push_temp_slots ();
      fn = ubsan_build_overflow_builtin (NEGATE_EXPR, loc, TREE_TYPE (arg1),
					 arg1, NULL_TREE);
      expand_normal (fn);
      pop_temp_slots ();
      do_pending_stack_adjust ();
    }
  else if (lhs)
    write_complex_part (target, const1_rtx, true);

  /* We're done.  */
  emit_label (done_label);

  if (lhs)
    {
      if (is_ubsan)
	expand_ubsan_result_store (target, res);
      else
	expand_arith_overflow_result_store (lhs, target, mode, res);
    }
}

/* Add mul overflow checking to the statement STMT.  */

static void
expand_mul_overflow (location_t loc, tree lhs, tree arg0, tree arg1,
		     bool unsr_p, bool uns0_p, bool uns1_p, bool is_ubsan)
{
  rtx res, op0, op1;
  tree fn, type;
  rtx_code_label *done_label, *do_error;
  rtx target = NULL_RTX;
  signop sign;
  enum insn_code icode;

  done_label = gen_label_rtx ();
  do_error = gen_label_rtx ();

  do_pending_stack_adjust ();
  op0 = expand_normal (arg0);
  op1 = expand_normal (arg1);

  machine_mode mode = TYPE_MODE (TREE_TYPE (arg0));
  bool uns = unsr_p;
  if (lhs)
    {
      target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
      if (!is_ubsan)
	write_complex_part (target, const0_rtx, true);
    }

  if (is_ubsan)
    gcc_assert (!unsr_p && !uns0_p && !uns1_p);

  /* We assume both operands and result have the same precision
     here (GET_MODE_BITSIZE (mode)), S stands for signed type
     with that precision, U for unsigned type with that precision,
     sgn for unsigned most significant bit in that precision.
     s1 is signed first operand, u1 is unsigned first operand,
     s2 is signed second operand, u2 is unsigned second operand,
     sr is signed result, ur is unsigned result and the following
     rules say how to compute result (which is always result of
     the operands as if both were unsigned, cast to the right
     signedness) and how to compute whether operation overflowed.
     main_ovf (false) stands for jump on signed multiplication
     overflow or the main algorithm with uns == false.
     main_ovf (true) stands for jump on unsigned multiplication
     overflow or the main algorithm with uns == true.

     s1 * s2 -> sr
	res = (S) ((U) s1 * (U) s2)
	ovf = main_ovf (false)
     u1 * u2 -> ur
	res = u1 * u2
	ovf = main_ovf (true)
     s1 * u2 -> ur
	res = (U) s1 * u2
	ovf = (s1 < 0 && u2) || main_ovf (true)
     u1 * u2 -> sr
	res = (S) (u1 * u2)
	ovf = res < 0 || main_ovf (true)
     s1 * u2 -> sr
	res = (S) ((U) s1 * u2)
	ovf = (S) u2 >= 0 ? main_ovf (false)
			  : (s1 != 0 && (s1 != -1 || u2 != (U) res))
     s1 * s2 -> ur
	t1 = (s1 & s2) < 0 ? (-(U) s1) : ((U) s1)
	t2 = (s1 & s2) < 0 ? (-(U) s2) : ((U) s2)
	res = t1 * t2
	ovf = (s1 ^ s2) < 0 ? (s1 && s2) : main_ovf (true)  */

  if (uns0_p && !uns1_p)
    {
      /* Multiplication is commutative, if operand signedness differs,
	 canonicalize to the first operand being signed and second
	 unsigned to simplify following code.  */
      rtx tem = op1;
      op1 = op0;
      op0 = tem;
      tree t = arg1;
      arg1 = arg0;
      arg0 = t;
      uns0_p = 0;
      uns1_p = 1;
    }

  int pos_neg0 = get_range_pos_neg (arg0);
  int pos_neg1 = get_range_pos_neg (arg1);

  /* s1 * u2 -> ur  */
  if (!uns0_p && uns1_p && unsr_p)
    {
      switch (pos_neg0)
	{
	case 1:
	  /* If s1 is non-negative, just perform normal u1 * u2 -> ur.  */
	  goto do_main;
	case 2:
	  /* If s1 is negative, avoid the main code, just multiply and
	     signal overflow if op1 is not 0.  */
	  struct separate_ops ops;
	  ops.code = MULT_EXPR;
	  ops.type = TREE_TYPE (arg1);
	  ops.op0 = make_tree (ops.type, op0);
	  ops.op1 = make_tree (ops.type, op1);
	  ops.op2 = NULL_TREE;
	  ops.location = loc;
	  res = expand_expr_real_2 (&ops, NULL_RTX, mode, EXPAND_NORMAL);
	  do_compare_rtx_and_jump (op1, const0_rtx, EQ, true, mode, NULL_RTX,
				   NULL_RTX, done_label, PROB_VERY_LIKELY);
	  goto do_error_label;
	case 3:
	  rtx_code_label *do_main_label;
	  do_main_label = gen_label_rtx ();
	  do_compare_rtx_and_jump (op0, const0_rtx, GE, false, mode, NULL_RTX,
				   NULL_RTX, do_main_label, PROB_VERY_LIKELY);
	  do_compare_rtx_and_jump (op1, const0_rtx, EQ, true, mode, NULL_RTX,
				   NULL_RTX, do_main_label, PROB_VERY_LIKELY);
	  write_complex_part (target, const1_rtx, true);
	  emit_label (do_main_label);
	  goto do_main;
	default:
	  gcc_unreachable ();
	}
    }

  /* u1 * u2 -> sr  */
  if (uns0_p && uns1_p && !unsr_p)
    {
      uns = true;
      /* Rest of handling of this case after res is computed.  */
      goto do_main;
    }

  /* s1 * u2 -> sr  */
  if (!uns0_p && uns1_p && !unsr_p)
    {
      switch (pos_neg1)
	{
	case 1:
	  goto do_main;
	case 2:
	  /* If (S) u2 is negative (i.e. u2 is larger than maximum of S,
	     avoid the main code, just multiply and signal overflow
	     unless 0 * u2 or -1 * ((U) Smin).  */
	  struct separate_ops ops;
	  ops.code = MULT_EXPR;
	  ops.type = TREE_TYPE (arg1);
	  ops.op0 = make_tree (ops.type, op0);
	  ops.op1 = make_tree (ops.type, op1);
	  ops.op2 = NULL_TREE;
	  ops.location = loc;
	  res = expand_expr_real_2 (&ops, NULL_RTX, mode, EXPAND_NORMAL);
	  do_compare_rtx_and_jump (op0, const0_rtx, EQ, true, mode, NULL_RTX,
				   NULL_RTX, done_label, PROB_VERY_LIKELY);
	  do_compare_rtx_and_jump (op0, constm1_rtx, NE, true, mode, NULL_RTX,
				   NULL_RTX, do_error, PROB_VERY_UNLIKELY);
	  int prec;
	  prec = GET_MODE_PRECISION (mode);
	  rtx sgn;
	  sgn = immed_wide_int_const (wi::min_value (prec, SIGNED), mode);
	  do_compare_rtx_and_jump (op1, sgn, EQ, true, mode, NULL_RTX,
				   NULL_RTX, done_label, PROB_VERY_LIKELY);
	  goto do_error_label;
	case 3:
	  /* Rest of handling of this case after res is computed.  */
	  goto do_main;
	default:
	  gcc_unreachable ();
	}
    }

  /* s1 * s2 -> ur  */
  if (!uns0_p && !uns1_p && unsr_p)
    {
      rtx tem, tem2;
      switch (pos_neg0 | pos_neg1)
	{
	case 1: /* Both operands known to be non-negative.  */
	  goto do_main;
	case 2: /* Both operands known to be negative.  */
	  op0 = expand_unop (mode, neg_optab, op0, NULL_RTX, false);
	  op1 = expand_unop (mode, neg_optab, op1, NULL_RTX, false);
	  /* Avoid looking at arg0/arg1 ranges, as we've changed
	     the arguments.  */
	  arg0 = error_mark_node;
	  arg1 = error_mark_node;
	  goto do_main;
	case 3:
	  if ((pos_neg0 ^ pos_neg1) == 3)
	    {
	      /* If one operand is known to be negative and the other
		 non-negative, this overflows always, unless the non-negative
		 one is 0.  Just do normal multiply and set overflow
		 unless one of the operands is 0.  */
	      struct separate_ops ops;
	      ops.code = MULT_EXPR;
	      ops.type
		= build_nonstandard_integer_type (GET_MODE_PRECISION (mode),
						  1);
	      ops.op0 = make_tree (ops.type, op0);
	      ops.op1 = make_tree (ops.type, op1);
	      ops.op2 = NULL_TREE;
	      ops.location = loc;
	      res = expand_expr_real_2 (&ops, NULL_RTX, mode, EXPAND_NORMAL);
	      tem = expand_binop (mode, and_optab, op0, op1, NULL_RTX, false,
				  OPTAB_LIB_WIDEN);
	      do_compare_rtx_and_jump (tem, const0_rtx, EQ, true, mode,
				       NULL_RTX, NULL_RTX, done_label,
				       PROB_VERY_LIKELY);
	      goto do_error_label;
	    }
	  /* The general case, do all the needed comparisons at runtime.  */
	  rtx_code_label *do_main_label, *after_negate_label;
	  rtx rop0, rop1;
	  rop0 = gen_reg_rtx (mode);
	  rop1 = gen_reg_rtx (mode);
	  emit_move_insn (rop0, op0);
	  emit_move_insn (rop1, op1);
	  op0 = rop0;
	  op1 = rop1;
	  do_main_label = gen_label_rtx ();
	  after_negate_label = gen_label_rtx ();
	  tem = expand_binop (mode, and_optab, op0, op1, NULL_RTX, false,
			      OPTAB_LIB_WIDEN);
	  do_compare_rtx_and_jump (tem, const0_rtx, GE, false, mode, NULL_RTX,
				   NULL_RTX, after_negate_label,
				   PROB_VERY_LIKELY);
	  /* Both arguments negative here, negate them and continue with
	     normal unsigned overflow checking multiplication.  */
	  emit_move_insn (op0, expand_unop (mode, neg_optab, op0,
					    NULL_RTX, false));
	  emit_move_insn (op1, expand_unop (mode, neg_optab, op1,
					    NULL_RTX, false));
	  /* Avoid looking at arg0/arg1 ranges, as we might have changed
	     the arguments.  */
	  arg0 = error_mark_node;
	  arg1 = error_mark_node;
	  emit_jump (do_main_label);
	  emit_label (after_negate_label);
	  tem2 = expand_binop (mode, xor_optab, op0, op1, NULL_RTX, false,
			       OPTAB_LIB_WIDEN);
	  do_compare_rtx_and_jump (tem2, const0_rtx, GE, false, mode, NULL_RTX,
				   NULL_RTX, do_main_label, PROB_VERY_LIKELY);
	  /* One argument is negative here, the other positive.  This
	     overflows always, unless one of the arguments is 0.  But
	     if e.g. s2 is 0, (U) s1 * 0 doesn't overflow, whatever s1
	     is, thus we can keep do_main code oring in overflow as is.  */
	  do_compare_rtx_and_jump (tem, const0_rtx, EQ, true, mode, NULL_RTX,
				   NULL_RTX, do_main_label, PROB_VERY_LIKELY);
	  write_complex_part (target, const1_rtx, true);
	  emit_label (do_main_label);
	  goto do_main;
	default:
	  gcc_unreachable ();
	}
    }

 do_main:
  type = build_nonstandard_integer_type (GET_MODE_PRECISION (mode), uns);
  sign = uns ? UNSIGNED : SIGNED;
  icode = optab_handler (uns ? umulv4_optab : mulv4_optab, mode);
  if (icode != CODE_FOR_nothing)
    {
      struct expand_operand ops[4];
      rtx_insn *last = get_last_insn ();

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
      int prec = GET_MODE_PRECISION (mode);
      machine_mode hmode = mode_for_size (prec / 2, MODE_INT, 1);
      ops.op0 = make_tree (type, op0);
      ops.op1 = make_tree (type, op1);
      ops.op2 = NULL_TREE;
      ops.location = loc;
      if (GET_MODE_2XWIDER_MODE (mode) != VOIDmode
	  && targetm.scalar_mode_supported_p (GET_MODE_2XWIDER_MODE (mode)))
	{
	  machine_mode wmode = GET_MODE_2XWIDER_MODE (mode);
	  ops.code = WIDEN_MULT_EXPR;
	  ops.type
	    = build_nonstandard_integer_type (GET_MODE_PRECISION (wmode), uns);

	  res = expand_expr_real_2 (&ops, NULL_RTX, wmode, EXPAND_NORMAL);
	  rtx hipart = expand_shift (RSHIFT_EXPR, wmode, res, prec,
				     NULL_RTX, uns);
	  hipart = gen_lowpart (mode, hipart);
	  res = gen_lowpart (mode, res);
	  if (uns)
	    /* For the unsigned multiplication, there was overflow if
	       HIPART is non-zero.  */
	    do_compare_rtx_and_jump (hipart, const0_rtx, EQ, true, mode,
				     NULL_RTX, NULL_RTX, done_label,
				     PROB_VERY_LIKELY);
	  else
	    {
	      rtx signbit = expand_shift (RSHIFT_EXPR, mode, res, prec - 1,
					  NULL_RTX, 0);
	      /* RES is low half of the double width result, HIPART
		 the high half.  There was overflow if
		 HIPART is different from RES < 0 ? -1 : 0.  */
	      do_compare_rtx_and_jump (signbit, hipart, EQ, true, mode,
				       NULL_RTX, NULL_RTX, done_label,
				       PROB_VERY_LIKELY);
	    }
	}
      else if (hmode != BLKmode && 2 * GET_MODE_PRECISION (hmode) == prec)
	{
	  rtx_code_label *large_op0 = gen_label_rtx ();
	  rtx_code_label *small_op0_large_op1 = gen_label_rtx ();
	  rtx_code_label *one_small_one_large = gen_label_rtx ();
	  rtx_code_label *both_ops_large = gen_label_rtx ();
	  rtx_code_label *after_hipart_neg = uns ? NULL : gen_label_rtx ();
	  rtx_code_label *after_lopart_neg = uns ? NULL : gen_label_rtx ();
	  rtx_code_label *do_overflow = gen_label_rtx ();
	  rtx_code_label *hipart_different = uns ? NULL : gen_label_rtx ();

	  unsigned int hprec = GET_MODE_PRECISION (hmode);
	  rtx hipart0 = expand_shift (RSHIFT_EXPR, mode, op0, hprec,
				      NULL_RTX, uns);
	  hipart0 = gen_lowpart (hmode, hipart0);
	  rtx lopart0 = gen_lowpart (hmode, op0);
	  rtx signbit0 = const0_rtx;
	  if (!uns)
	    signbit0 = expand_shift (RSHIFT_EXPR, hmode, lopart0, hprec - 1,
				     NULL_RTX, 0);
	  rtx hipart1 = expand_shift (RSHIFT_EXPR, mode, op1, hprec,
				      NULL_RTX, uns);
	  hipart1 = gen_lowpart (hmode, hipart1);
	  rtx lopart1 = gen_lowpart (hmode, op1);
	  rtx signbit1 = const0_rtx;
	  if (!uns)
	    signbit1 = expand_shift (RSHIFT_EXPR, hmode, lopart1, hprec - 1,
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

	  if (pos_neg0 == 1)
	    op0_sign = 0;
	  else if (pos_neg0 == 2)
	    op0_sign = -1;
	  if (pos_neg1 == 1)
	    op1_sign = 0;
	  else if (pos_neg1 == 2)
	    op1_sign = -1;

	  unsigned int mprec0 = prec;
	  if (arg0 != error_mark_node)
	    mprec0 = get_min_precision (arg0, sign);
	  if (mprec0 <= hprec)
	    op0_small_p = true;
	  else if (!uns && mprec0 <= hprec + 1)
	    op0_medium_p = true;
	  unsigned int mprec1 = prec;
	  if (arg1 != error_mark_node)
	    mprec1 = get_min_precision (arg1, sign);
	  if (mprec1 <= hprec)
	    op1_small_p = true;
	  else if (!uns && mprec1 <= hprec + 1)
	    op1_medium_p = true;

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
	    do_compare_rtx_and_jump (signbit0, hipart0, NE, true, hmode,
				     NULL_RTX, NULL_RTX, large_op0,
				     PROB_UNLIKELY);

	  if (!op1_small_p)
	    do_compare_rtx_and_jump (signbit1, hipart1, NE, true, hmode,
				     NULL_RTX, NULL_RTX, small_op0_large_op1,
				     PROB_UNLIKELY);

	  /* If both op0 and op1 are sign (!uns) or zero (uns) extended from
	     hmode to mode, the multiplication will never overflow.  We can
	     do just one hmode x hmode => mode widening multiplication.  */
	  rtx lopart0s = lopart0, lopart1s = lopart1;
	  if (GET_CODE (lopart0) == SUBREG)
	    {
	      lopart0s = shallow_copy_rtx (lopart0);
	      SUBREG_PROMOTED_VAR_P (lopart0s) = 1;
	      SUBREG_PROMOTED_SET (lopart0s, uns ? SRP_UNSIGNED : SRP_SIGNED);
	    }
	  if (GET_CODE (lopart1) == SUBREG)
	    {
	      lopart1s = shallow_copy_rtx (lopart1);
	      SUBREG_PROMOTED_VAR_P (lopart1s) = 1;
	      SUBREG_PROMOTED_SET (lopart1s, uns ? SRP_UNSIGNED : SRP_SIGNED);
	    }
	  tree halfstype = build_nonstandard_integer_type (hprec, uns);
	  ops.op0 = make_tree (halfstype, lopart0s);
	  ops.op1 = make_tree (halfstype, lopart1s);
	  ops.code = WIDEN_MULT_EXPR;
	  ops.type = type;
	  rtx thisres
	    = expand_expr_real_2 (&ops, NULL_RTX, mode, EXPAND_NORMAL);
	  emit_move_insn (res, thisres);
	  emit_jump (done_label);

	  emit_label (small_op0_large_op1);

	  /* If op0 is sign (!uns) or zero (uns) extended from hmode to mode,
	     but op1 is not, just swap the arguments and handle it as op1
	     sign/zero extended, op0 not.  */
	  rtx larger = gen_reg_rtx (mode);
	  rtx hipart = gen_reg_rtx (hmode);
	  rtx lopart = gen_reg_rtx (hmode);
	  emit_move_insn (larger, op1);
	  emit_move_insn (hipart, hipart1);
	  emit_move_insn (lopart, lopart0);
	  emit_jump (one_small_one_large);

	  emit_label (large_op0);

	  if (!op1_small_p)
	    do_compare_rtx_and_jump (signbit1, hipart1, NE, true, hmode,
				     NULL_RTX, NULL_RTX, both_ops_large,
				     PROB_UNLIKELY);

	  /* If op1 is sign (!uns) or zero (uns) extended from hmode to mode,
	     but op0 is not, prepare larger, hipart and lopart pseudos and
	     handle it together with small_op0_large_op1.  */
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

	  if (!uns)
	    {
	      /* if (hipart < 0) loxhi -= lopart << (bitsize / 2);  */
	      if (larger_sign == 0)
		emit_jump (after_hipart_neg);
	      else if (larger_sign != -1)
		do_compare_rtx_and_jump (hipart, const0_rtx, GE, false, hmode,
					 NULL_RTX, NULL_RTX, after_hipart_neg,
					 PROB_EVEN);

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
		do_compare_rtx_and_jump (lopart, const0_rtx, GE, false, hmode,
					 NULL_RTX, NULL_RTX, after_lopart_neg,
					 PROB_EVEN);

	      tem = expand_simple_binop (mode, MINUS, loxhi, larger, NULL_RTX,
					 1, OPTAB_DIRECT);
	      emit_move_insn (loxhi, tem);

	      emit_label (after_lopart_neg);
	    }

	  /* loxhi += (uns) lo0xlo1 >> (bitsize / 2);  */
	  tem = expand_shift (RSHIFT_EXPR, mode, lo0xlo1, hprec, NULL_RTX, 1);
	  tem = expand_simple_binop (mode, PLUS, loxhi, tem, NULL_RTX,
				     1, OPTAB_DIRECT);
	  emit_move_insn (loxhi, tem);

	  /* if (loxhi >> (bitsize / 2)
		 == (hmode) loxhi >> (bitsize / 2 - 1))  (if !uns)
	     if (loxhi >> (bitsize / 2) == 0		 (if uns).  */
	  rtx hipartloxhi = expand_shift (RSHIFT_EXPR, mode, loxhi, hprec,
					  NULL_RTX, 0);
	  hipartloxhi = gen_lowpart (hmode, hipartloxhi);
	  rtx signbitloxhi = const0_rtx;
	  if (!uns)
	    signbitloxhi = expand_shift (RSHIFT_EXPR, hmode,
					 gen_lowpart (hmode, loxhi),
					 hprec - 1, NULL_RTX, 0);

	  do_compare_rtx_and_jump (signbitloxhi, hipartloxhi, NE, true, hmode,
				   NULL_RTX, NULL_RTX, do_overflow,
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

	  /* If both operands are large (not sign (!uns) or zero (uns)
	     extended from hmode), then perform the full multiplication
	     which will be the result of the operation.
	     The only cases which don't overflow are for signed multiplication
	     some cases where both hipart0 and highpart1 are 0 or -1.
	     For unsigned multiplication when high parts are both non-zero
	     this overflows always.  */
	  ops.code = MULT_EXPR;
	  ops.op0 = make_tree (type, op0);
	  ops.op1 = make_tree (type, op1);
	  tem = expand_expr_real_2 (&ops, NULL_RTX, mode, EXPAND_NORMAL);
	  emit_move_insn (res, tem);

	  if (!uns)
	    {
	      if (!op0_medium_p)
		{
		  tem = expand_simple_binop (hmode, PLUS, hipart0, const1_rtx,
					     NULL_RTX, 1, OPTAB_DIRECT);
		  do_compare_rtx_and_jump (tem, const1_rtx, GTU, true, hmode,
					   NULL_RTX, NULL_RTX, do_error,
					   PROB_VERY_UNLIKELY);
		}

	      if (!op1_medium_p)
		{
		  tem = expand_simple_binop (hmode, PLUS, hipart1, const1_rtx,
					     NULL_RTX, 1, OPTAB_DIRECT);
		  do_compare_rtx_and_jump (tem, const1_rtx, GTU, true, hmode,
					   NULL_RTX, NULL_RTX, do_error,
					   PROB_VERY_UNLIKELY);
		}

	      /* At this point hipart{0,1} are both in [-1, 0].  If they are
		 the same, overflow happened if res is negative, if they are
		 different, overflow happened if res is positive.  */
	      if (op0_sign != 1 && op1_sign != 1 && op0_sign != op1_sign)
		emit_jump (hipart_different);
	      else if (op0_sign == 1 || op1_sign == 1)
		do_compare_rtx_and_jump (hipart0, hipart1, NE, true, hmode,
					 NULL_RTX, NULL_RTX, hipart_different,
					 PROB_EVEN);

	      do_compare_rtx_and_jump (res, const0_rtx, LT, false, mode,
				       NULL_RTX, NULL_RTX, do_error,
				       PROB_VERY_UNLIKELY);
	      emit_jump (done_label);

	      emit_label (hipart_different);

	      do_compare_rtx_and_jump (res, const0_rtx, GE, false, mode,
				       NULL_RTX, NULL_RTX, do_error,
				       PROB_VERY_UNLIKELY);
	      emit_jump (done_label);
	    }

	  emit_label (do_overflow);

	  /* Overflow, do full multiplication and fallthru into do_error.  */
	  ops.op0 = make_tree (type, op0);
	  ops.op1 = make_tree (type, op1);
	  tem = expand_expr_real_2 (&ops, NULL_RTX, mode, EXPAND_NORMAL);
	  emit_move_insn (res, tem);
	}
      else
	{
	  gcc_assert (!is_ubsan);
	  ops.code = MULT_EXPR;
	  ops.type = type;
	  res = expand_expr_real_2 (&ops, NULL_RTX, mode, EXPAND_NORMAL);
	  emit_jump (done_label);
	}
    }

 do_error_label:
  emit_label (do_error);
  if (is_ubsan)
    {
      /* Expand the ubsan builtin call.  */
      push_temp_slots ();
      fn = ubsan_build_overflow_builtin (MULT_EXPR, loc, TREE_TYPE (arg0),
					 arg0, arg1);
      expand_normal (fn);
      pop_temp_slots ();
      do_pending_stack_adjust ();
    }
  else if (lhs)
    write_complex_part (target, const1_rtx, true);

  /* We're done.  */
  emit_label (done_label);

  /* u1 * u2 -> sr  */
  if (uns0_p && uns1_p && !unsr_p)
    {
      rtx_code_label *all_done_label = gen_label_rtx ();
      do_compare_rtx_and_jump (res, const0_rtx, GE, false, mode, NULL_RTX,
			       NULL_RTX, all_done_label, PROB_VERY_LIKELY);
      write_complex_part (target, const1_rtx, true);
      emit_label (all_done_label);
    }

  /* s1 * u2 -> sr  */
  if (!uns0_p && uns1_p && !unsr_p && pos_neg1 == 3)
    {
      rtx_code_label *all_done_label = gen_label_rtx ();
      rtx_code_label *set_noovf = gen_label_rtx ();
      do_compare_rtx_and_jump (op1, const0_rtx, GE, false, mode, NULL_RTX,
			       NULL_RTX, all_done_label, PROB_VERY_LIKELY);
      write_complex_part (target, const1_rtx, true);
      do_compare_rtx_and_jump (op0, const0_rtx, EQ, true, mode, NULL_RTX,
			       NULL_RTX, set_noovf, PROB_VERY_LIKELY);
      do_compare_rtx_and_jump (op0, constm1_rtx, NE, true, mode, NULL_RTX,
			       NULL_RTX, all_done_label, PROB_VERY_UNLIKELY);
      do_compare_rtx_and_jump (op1, res, NE, true, mode, NULL_RTX, NULL_RTX,
			       all_done_label, PROB_VERY_UNLIKELY);
      emit_label (set_noovf);
      write_complex_part (target, const0_rtx, true);
      emit_label (all_done_label);
    }

  if (lhs)
    {
      if (is_ubsan)
	expand_ubsan_result_store (target, res);
      else
	expand_arith_overflow_result_store (lhs, target, mode, res);
    }
}

/* Expand UBSAN_CHECK_ADD call STMT.  */

static void
expand_UBSAN_CHECK_ADD (gcall *stmt)
{
  location_t loc = gimple_location (stmt);
  tree lhs = gimple_call_lhs (stmt);
  tree arg0 = gimple_call_arg (stmt, 0);
  tree arg1 = gimple_call_arg (stmt, 1);
  expand_addsub_overflow (loc, PLUS_EXPR, lhs, arg0, arg1,
			  false, false, false, true);
}

/* Expand UBSAN_CHECK_SUB call STMT.  */

static void
expand_UBSAN_CHECK_SUB (gcall *stmt)
{
  location_t loc = gimple_location (stmt);
  tree lhs = gimple_call_lhs (stmt);
  tree arg0 = gimple_call_arg (stmt, 0);
  tree arg1 = gimple_call_arg (stmt, 1);
  if (integer_zerop (arg0))
    expand_neg_overflow (loc, lhs, arg1, true);
  else
    expand_addsub_overflow (loc, MINUS_EXPR, lhs, arg0, arg1,
			    false, false, false, true);
}

/* Expand UBSAN_CHECK_MUL call STMT.  */

static void
expand_UBSAN_CHECK_MUL (gcall *stmt)
{
  location_t loc = gimple_location (stmt);
  tree lhs = gimple_call_lhs (stmt);
  tree arg0 = gimple_call_arg (stmt, 0);
  tree arg1 = gimple_call_arg (stmt, 1);
  expand_mul_overflow (loc, lhs, arg0, arg1, false, false, false, true);
}

/* Helper function for {ADD,SUB,MUL}_OVERFLOW call stmt expansion.  */

static void
expand_arith_overflow (enum tree_code code, gimple stmt)
{
  tree lhs = gimple_call_lhs (stmt);
  if (lhs == NULL_TREE)
    return;
  tree arg0 = gimple_call_arg (stmt, 0);
  tree arg1 = gimple_call_arg (stmt, 1);
  tree type = TREE_TYPE (TREE_TYPE (lhs));
  int uns0_p = TYPE_UNSIGNED (TREE_TYPE (arg0));
  int uns1_p = TYPE_UNSIGNED (TREE_TYPE (arg1));
  int unsr_p = TYPE_UNSIGNED (type);
  int prec0 = TYPE_PRECISION (TREE_TYPE (arg0));
  int prec1 = TYPE_PRECISION (TREE_TYPE (arg1));
  int precres = TYPE_PRECISION (type);
  location_t loc = gimple_location (stmt);
  if (!uns0_p && get_range_pos_neg (arg0) == 1)
    uns0_p = true;
  if (!uns1_p && get_range_pos_neg (arg1) == 1)
    uns1_p = true;
  int pr = get_min_precision (arg0, uns0_p ? UNSIGNED : SIGNED);
  prec0 = MIN (prec0, pr);
  pr = get_min_precision (arg1, uns1_p ? UNSIGNED : SIGNED);
  prec1 = MIN (prec1, pr);

  /* If uns0_p && uns1_p, precop is minimum needed precision
     of unsigned type to hold the exact result, otherwise
     precop is minimum needed precision of signed type to
     hold the exact result.  */
  int precop;
  if (code == MULT_EXPR)
    precop = prec0 + prec1 + (uns0_p != uns1_p);
  else
    {
      if (uns0_p == uns1_p)
	precop = MAX (prec0, prec1) + 1;
      else if (uns0_p)
	precop = MAX (prec0 + 1, prec1) + 1;
      else
	precop = MAX (prec0, prec1 + 1) + 1;
    }
  int orig_precres = precres;

  do
    {
      if ((uns0_p && uns1_p)
	  ? ((precop + !unsr_p) <= precres
	     /* u1 - u2 -> ur can overflow, no matter what precision
		the result has.  */
	     && (code != MINUS_EXPR || !unsr_p))
	  : (!unsr_p && precop <= precres))
	{
	  /* The infinity precision result will always fit into result.  */
	  rtx target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
	  write_complex_part (target, const0_rtx, true);
	  enum machine_mode mode = TYPE_MODE (type);
	  struct separate_ops ops;
	  ops.code = code;
	  ops.type = type;
	  ops.op0 = fold_convert_loc (loc, type, arg0);
	  ops.op1 = fold_convert_loc (loc, type, arg1);
	  ops.op2 = NULL_TREE;
	  ops.location = loc;
	  rtx tem = expand_expr_real_2 (&ops, NULL_RTX, mode, EXPAND_NORMAL);
	  expand_arith_overflow_result_store (lhs, target, mode, tem);
	  return;
	}

#ifdef WORD_REGISTER_OPERATIONS
      /* For sub-word operations, if target doesn't have them, start
	 with precres widening right away, otherwise do it only
	 if the most simple cases can't be used.  */
      if (orig_precres == precres && precres < BITS_PER_WORD)
	;
      else
#endif
      if ((uns0_p && uns1_p && unsr_p && prec0 <= precres && prec1 <= precres)
	  || ((!uns0_p || !uns1_p) && !unsr_p
	      && prec0 + uns0_p <= precres
	      && prec1 + uns1_p <= precres))
	{
	  arg0 = fold_convert_loc (loc, type, arg0);
	  arg1 = fold_convert_loc (loc, type, arg1);
	  switch (code)
	    {
	    case MINUS_EXPR:
	      if (integer_zerop (arg0) && !unsr_p)
		{
		  expand_neg_overflow (loc, lhs, arg1, false);
		  return;
		}
	      /* FALLTHRU */
	    case PLUS_EXPR:
	      expand_addsub_overflow (loc, code, lhs, arg0, arg1,
				      unsr_p, unsr_p, unsr_p, false);
	      return;
	    case MULT_EXPR:
	      expand_mul_overflow (loc, lhs, arg0, arg1,
				   unsr_p, unsr_p, unsr_p, false);
	      return;
	    default:
	      gcc_unreachable ();
	    }
	}

      /* For sub-word operations, retry with a wider type first.  */
      if (orig_precres == precres && precop <= BITS_PER_WORD)
	{
#ifdef WORD_REGISTER_OPERATIONS
	  int p = BITS_PER_WORD;
#else
	  int p = precop;
#endif
	  enum machine_mode m = smallest_mode_for_size (p, MODE_INT);
	  tree optype = build_nonstandard_integer_type (GET_MODE_PRECISION (m),
							uns0_p && uns1_p
							&& unsr_p);
	  p = TYPE_PRECISION (optype);
	  if (p > precres)
	    {
	      precres = p;
	      unsr_p = TYPE_UNSIGNED (optype);
	      type = optype;
	      continue;
	    }
	}

      if (prec0 <= precres && prec1 <= precres)
	{
	  tree types[2];
	  if (unsr_p)
	    {
	      types[0] = build_nonstandard_integer_type (precres, 0);
	      types[1] = type;
	    }
	  else
	    {
	      types[0] = type;
	      types[1] = build_nonstandard_integer_type (precres, 1);
	    }
	  arg0 = fold_convert_loc (loc, types[uns0_p], arg0);
	  arg1 = fold_convert_loc (loc, types[uns1_p], arg1);
	  if (code != MULT_EXPR)
	    expand_addsub_overflow (loc, code, lhs, arg0, arg1, unsr_p,
				    uns0_p, uns1_p, false);
	  else
	    expand_mul_overflow (loc, lhs, arg0, arg1, unsr_p,
				 uns0_p, uns1_p, false);
	  return;
	}

      /* Retry with a wider type.  */
      if (orig_precres == precres)
	{
	  int p = MAX (prec0, prec1);
	  enum machine_mode m = smallest_mode_for_size (p, MODE_INT);
	  tree optype = build_nonstandard_integer_type (GET_MODE_PRECISION (m),
							uns0_p && uns1_p
							&& unsr_p);
	  p = TYPE_PRECISION (optype);
	  if (p > precres)
	    {
	      precres = p;
	      unsr_p = TYPE_UNSIGNED (optype);
	      type = optype;
	      continue;
	    }
	}

      gcc_unreachable ();
    }
  while (1);
}

/* Expand ADD_OVERFLOW STMT.  */

static void
expand_ADD_OVERFLOW (gcall *stmt)
{
  expand_arith_overflow (PLUS_EXPR, stmt);
}

/* Expand SUB_OVERFLOW STMT.  */

static void
expand_SUB_OVERFLOW (gcall *stmt)
{
  expand_arith_overflow (MINUS_EXPR, stmt);
}

/* Expand MUL_OVERFLOW STMT.  */

static void
expand_MUL_OVERFLOW (gcall *stmt)
{
  expand_arith_overflow (MULT_EXPR, stmt);
}

/* This should get folded in tree-vectorizer.c.  */

static void
expand_LOOP_VECTORIZED (gcall *)
{
  gcc_unreachable ();
}

static void
expand_MASK_LOAD (gcall *stmt)
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
expand_MASK_STORE (gcall *stmt)
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
expand_ABNORMAL_DISPATCHER (gcall *)
{
}

static void
expand_BUILTIN_EXPECT (gcall *stmt)
{
  /* When guessing was done, the hints should be already stripped away.  */
  gcc_assert (!flag_guess_branch_prob || optimize == 0 || seen_error ());

  rtx target;
  tree lhs = gimple_call_lhs (stmt);
  if (lhs)
    target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  else
    target = const0_rtx;
  rtx val = expand_expr (gimple_call_arg (stmt, 0), target, VOIDmode, EXPAND_NORMAL);
  if (lhs && val != target)
    emit_move_insn (target, val);
}

/* Routines to expand each internal function, indexed by function number.
   Each routine has the prototype:

       expand_<NAME> (gcall *stmt)

   where STMT is the statement that performs the call. */
static void (*const internal_fn_expanders[]) (gcall *) = {
#define DEF_INTERNAL_FN(CODE, FLAGS, FNSPEC) expand_##CODE,
#include "internal-fn.def"
#undef DEF_INTERNAL_FN
  0
};

/* Expand STMT, which is a call to internal function FN.  */

void
expand_internal_call (gcall *stmt)
{
  internal_fn_expanders[(int) gimple_call_internal_fn (stmt)] (stmt);
}
