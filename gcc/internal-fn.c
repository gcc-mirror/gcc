/* Internal functions.
   Copyright (C) 2011-2019 Free Software Foundation, Inc.

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
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "predict.h"
#include "stringpool.h"
#include "tree-vrp.h"
#include "tree-ssanames.h"
#include "expmed.h"
#include "memmodel.h"
#include "optabs.h"
#include "emit-rtl.h"
#include "diagnostic-core.h"
#include "fold-const.h"
#include "internal-fn.h"
#include "stor-layout.h"
#include "dojump.h"
#include "expr.h"
#include "stringpool.h"
#include "attribs.h"
#include "asan.h"
#include "ubsan.h"
#include "recog.h"
#include "builtins.h"
#include "optabs-tree.h"
#include "gimple-ssa.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"

/* The names of each internal function, indexed by function number.  */
const char *const internal_fn_name_array[] = {
#define DEF_INTERNAL_FN(CODE, FLAGS, FNSPEC) #CODE,
#include "internal-fn.def"
  "<invalid-fn>"
};

/* The ECF_* flags of each internal function, indexed by function number.  */
const int internal_fn_flags_array[] = {
#define DEF_INTERNAL_FN(CODE, FLAGS, FNSPEC) FLAGS,
#include "internal-fn.def"
  0
};

/* Return the internal function called NAME, or IFN_LAST if there's
   no such function.  */

internal_fn
lookup_internal_fn (const char *name)
{
  typedef hash_map<nofree_string_hash, internal_fn> name_to_fn_map_type;
  static name_to_fn_map_type *name_to_fn_map;

  if (!name_to_fn_map)
    {
      name_to_fn_map = new name_to_fn_map_type (IFN_LAST);
      for (unsigned int i = 0; i < IFN_LAST; ++i)
	name_to_fn_map->put (internal_fn_name (internal_fn (i)),
			     internal_fn (i));
    }
  internal_fn *entry = name_to_fn_map->get (name);
  return entry ? *entry : IFN_LAST;
}

/* Fnspec of each internal function, indexed by function number.  */
const_tree internal_fn_fnspec_array[IFN_LAST + 1];

void
init_internal_fns ()
{
#define DEF_INTERNAL_FN(CODE, FLAGS, FNSPEC) \
  if (FNSPEC) internal_fn_fnspec_array[IFN_##CODE] = \
    build_string ((int) sizeof (FNSPEC), FNSPEC ? FNSPEC : "");
#include "internal-fn.def"
  internal_fn_fnspec_array[IFN_LAST] = 0;
}

/* Create static initializers for the information returned by
   direct_internal_fn.  */
#define not_direct { -2, -2, false }
#define mask_load_direct { -1, 2, false }
#define load_lanes_direct { -1, -1, false }
#define mask_load_lanes_direct { -1, -1, false }
#define gather_load_direct { 3, 1, false }
#define mask_store_direct { 3, 2, false }
#define store_lanes_direct { 0, 0, false }
#define mask_store_lanes_direct { 0, 0, false }
#define scatter_store_direct { 3, 1, false }
#define unary_direct { 0, 0, true }
#define binary_direct { 0, 0, true }
#define ternary_direct { 0, 0, true }
#define cond_unary_direct { 1, 1, true }
#define cond_binary_direct { 1, 1, true }
#define cond_ternary_direct { 1, 1, true }
#define while_direct { 0, 2, false }
#define fold_extract_direct { 2, 2, false }
#define fold_left_direct { 1, 1, false }
#define mask_fold_left_direct { 1, 1, false }
#define check_ptrs_direct { 0, 0, false }

const direct_internal_fn_info direct_internal_fn_array[IFN_LAST + 1] = {
#define DEF_INTERNAL_FN(CODE, FLAGS, FNSPEC) not_direct,
#define DEF_INTERNAL_OPTAB_FN(CODE, FLAGS, OPTAB, TYPE) TYPE##_direct,
#define DEF_INTERNAL_SIGNED_OPTAB_FN(CODE, FLAGS, SELECTOR, SIGNED_OPTAB, \
				     UNSIGNED_OPTAB, TYPE) TYPE##_direct,
#include "internal-fn.def"
  not_direct
};

/* ARRAY_TYPE is an array of vector modes.  Return the associated insn
   for load-lanes-style optab OPTAB, or CODE_FOR_nothing if none.  */

static enum insn_code
get_multi_vector_move (tree array_type, convert_optab optab)
{
  machine_mode imode;
  machine_mode vmode;

  gcc_assert (TREE_CODE (array_type) == ARRAY_TYPE);
  imode = TYPE_MODE (array_type);
  vmode = TYPE_MODE (TREE_TYPE (array_type));

  return convert_optab_handler (optab, imode, vmode);
}

/* Expand LOAD_LANES call STMT using optab OPTAB.  */

static void
expand_load_lanes_optab_fn (internal_fn, gcall *stmt, convert_optab optab)
{
  class expand_operand ops[2];
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
  expand_insn (get_multi_vector_move (type, optab), 2, ops);
}

/* Expand STORE_LANES call STMT using optab OPTAB.  */

static void
expand_store_lanes_optab_fn (internal_fn, gcall *stmt, convert_optab optab)
{
  class expand_operand ops[2];
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
  expand_insn (get_multi_vector_move (type, optab), 2, ops);
}

static void
expand_ANNOTATE (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in omp_device_lower pass.  */

static void
expand_GOMP_USE_SIMT (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in omp_device_lower pass.  */

static void
expand_GOMP_SIMT_ENTER (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* Allocate per-lane storage and begin non-uniform execution region.  */

static void
expand_GOMP_SIMT_ENTER_ALLOC (internal_fn, gcall *stmt)
{
  rtx target;
  tree lhs = gimple_call_lhs (stmt);
  if (lhs)
    target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  else
    target = gen_reg_rtx (Pmode);
  rtx size = expand_normal (gimple_call_arg (stmt, 0));
  rtx align = expand_normal (gimple_call_arg (stmt, 1));
  class expand_operand ops[3];
  create_output_operand (&ops[0], target, Pmode);
  create_input_operand (&ops[1], size, Pmode);
  create_input_operand (&ops[2], align, Pmode);
  gcc_assert (targetm.have_omp_simt_enter ());
  expand_insn (targetm.code_for_omp_simt_enter, 3, ops);
}

/* Deallocate per-lane storage and leave non-uniform execution region.  */

static void
expand_GOMP_SIMT_EXIT (internal_fn, gcall *stmt)
{
  gcc_checking_assert (!gimple_call_lhs (stmt));
  rtx arg = expand_normal (gimple_call_arg (stmt, 0));
  class expand_operand ops[1];
  create_input_operand (&ops[0], arg, Pmode);
  gcc_assert (targetm.have_omp_simt_exit ());
  expand_insn (targetm.code_for_omp_simt_exit, 1, ops);
}

/* Lane index on SIMT targets: thread index in the warp on NVPTX.  On targets
   without SIMT execution this should be expanded in omp_device_lower pass.  */

static void
expand_GOMP_SIMT_LANE (internal_fn, gcall *stmt)
{
  tree lhs = gimple_call_lhs (stmt);
  if (!lhs)
    return;

  rtx target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  gcc_assert (targetm.have_omp_simt_lane ());
  emit_insn (targetm.gen_omp_simt_lane (target));
}

/* This should get expanded in omp_device_lower pass.  */

static void
expand_GOMP_SIMT_VF (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* Lane index of the first SIMT lane that supplies a non-zero argument.
   This is a SIMT counterpart to GOMP_SIMD_LAST_LANE, used to represent the
   lane that executed the last iteration for handling OpenMP lastprivate.  */

static void
expand_GOMP_SIMT_LAST_LANE (internal_fn, gcall *stmt)
{
  tree lhs = gimple_call_lhs (stmt);
  if (!lhs)
    return;

  rtx target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  rtx cond = expand_normal (gimple_call_arg (stmt, 0));
  machine_mode mode = TYPE_MODE (TREE_TYPE (lhs));
  class expand_operand ops[2];
  create_output_operand (&ops[0], target, mode);
  create_input_operand (&ops[1], cond, mode);
  gcc_assert (targetm.have_omp_simt_last_lane ());
  expand_insn (targetm.code_for_omp_simt_last_lane, 2, ops);
}

/* Non-transparent predicate used in SIMT lowering of OpenMP "ordered".  */

static void
expand_GOMP_SIMT_ORDERED_PRED (internal_fn, gcall *stmt)
{
  tree lhs = gimple_call_lhs (stmt);
  if (!lhs)
    return;

  rtx target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  rtx ctr = expand_normal (gimple_call_arg (stmt, 0));
  machine_mode mode = TYPE_MODE (TREE_TYPE (lhs));
  class expand_operand ops[2];
  create_output_operand (&ops[0], target, mode);
  create_input_operand (&ops[1], ctr, mode);
  gcc_assert (targetm.have_omp_simt_ordered ());
  expand_insn (targetm.code_for_omp_simt_ordered, 2, ops);
}

/* "Or" boolean reduction across SIMT lanes: return non-zero in all lanes if
   any lane supplies a non-zero argument.  */

static void
expand_GOMP_SIMT_VOTE_ANY (internal_fn, gcall *stmt)
{
  tree lhs = gimple_call_lhs (stmt);
  if (!lhs)
    return;

  rtx target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  rtx cond = expand_normal (gimple_call_arg (stmt, 0));
  machine_mode mode = TYPE_MODE (TREE_TYPE (lhs));
  class expand_operand ops[2];
  create_output_operand (&ops[0], target, mode);
  create_input_operand (&ops[1], cond, mode);
  gcc_assert (targetm.have_omp_simt_vote_any ());
  expand_insn (targetm.code_for_omp_simt_vote_any, 2, ops);
}

/* Exchange between SIMT lanes with a "butterfly" pattern: source lane index
   is destination lane index XOR given offset.  */

static void
expand_GOMP_SIMT_XCHG_BFLY (internal_fn, gcall *stmt)
{
  tree lhs = gimple_call_lhs (stmt);
  if (!lhs)
    return;

  rtx target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  rtx src = expand_normal (gimple_call_arg (stmt, 0));
  rtx idx = expand_normal (gimple_call_arg (stmt, 1));
  machine_mode mode = TYPE_MODE (TREE_TYPE (lhs));
  class expand_operand ops[3];
  create_output_operand (&ops[0], target, mode);
  create_input_operand (&ops[1], src, mode);
  create_input_operand (&ops[2], idx, SImode);
  gcc_assert (targetm.have_omp_simt_xchg_bfly ());
  expand_insn (targetm.code_for_omp_simt_xchg_bfly, 3, ops);
}

/* Exchange between SIMT lanes according to given source lane index.  */

static void
expand_GOMP_SIMT_XCHG_IDX (internal_fn, gcall *stmt)
{
  tree lhs = gimple_call_lhs (stmt);
  if (!lhs)
    return;

  rtx target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  rtx src = expand_normal (gimple_call_arg (stmt, 0));
  rtx idx = expand_normal (gimple_call_arg (stmt, 1));
  machine_mode mode = TYPE_MODE (TREE_TYPE (lhs));
  class expand_operand ops[3];
  create_output_operand (&ops[0], target, mode);
  create_input_operand (&ops[1], src, mode);
  create_input_operand (&ops[2], idx, SImode);
  gcc_assert (targetm.have_omp_simt_xchg_idx ());
  expand_insn (targetm.code_for_omp_simt_xchg_idx, 3, ops);
}

/* This should get expanded in adjust_simduid_builtins.  */

static void
expand_GOMP_SIMD_LANE (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in adjust_simduid_builtins.  */

static void
expand_GOMP_SIMD_VF (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in adjust_simduid_builtins.  */

static void
expand_GOMP_SIMD_LAST_LANE (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in adjust_simduid_builtins.  */

static void
expand_GOMP_SIMD_ORDERED_START (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in adjust_simduid_builtins.  */

static void
expand_GOMP_SIMD_ORDERED_END (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in the sanopt pass.  */

static void
expand_UBSAN_NULL (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in the sanopt pass.  */

static void
expand_UBSAN_BOUNDS (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in the sanopt pass.  */

static void
expand_UBSAN_VPTR (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in the sanopt pass.  */

static void
expand_UBSAN_PTR (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in the sanopt pass.  */

static void
expand_UBSAN_OBJECT_SIZE (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in the sanopt pass.  */

static void
expand_ASAN_CHECK (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in the sanopt pass.  */

static void
expand_ASAN_MARK (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in the sanopt pass.  */

static void
expand_ASAN_POISON (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in the sanopt pass.  */

static void
expand_ASAN_POISON_USE (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in the tsan pass.  */

static void
expand_TSAN_FUNC_EXIT (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in the lower pass.  */

static void
expand_FALLTHROUGH (internal_fn, gcall *call)
{
  error_at (gimple_location (call),
	    "invalid use of attribute %<fallthrough%>");
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
	p = wi::min_precision (wi::to_wide (arg), sign);
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
      gimple *g = SSA_NAME_DEF_STMT (arg);
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
      int p = wi::min_precision (arg_max, UNSIGNED);
      prec = MIN (prec, p);
    }
  return prec + (orig_sign != sign);
}

/* Helper for expand_*_overflow.  Set the __imag__ part to true
   (1 except for signed:1 type, in which case store -1).  */

static void
expand_arith_set_overflow (tree lhs, rtx target)
{
  if (TYPE_PRECISION (TREE_TYPE (TREE_TYPE (lhs))) == 1
      && !TYPE_UNSIGNED (TREE_TYPE (TREE_TYPE (lhs))))
    write_complex_part (target, constm1_rtx, true);
  else
    write_complex_part (target, const1_rtx, true);
}

/* Helper for expand_*_overflow.  Store RES into the __real__ part
   of TARGET.  If RES has larger MODE than __real__ part of TARGET,
   set the __imag__ part to 1 if RES doesn't fit into it.  Similarly
   if LHS has smaller precision than its mode.  */

static void
expand_arith_overflow_result_store (tree lhs, rtx target,
				    scalar_int_mode mode, rtx res)
{
  scalar_int_mode tgtmode
    = as_a <scalar_int_mode> (GET_MODE_INNER (GET_MODE (target)));
  rtx lres = res;
  if (tgtmode != mode)
    {
      rtx_code_label *done_label = gen_label_rtx ();
      int uns = TYPE_UNSIGNED (TREE_TYPE (TREE_TYPE (lhs)));
      lres = convert_modes (tgtmode, mode, res, uns);
      gcc_assert (GET_MODE_PRECISION (tgtmode) < GET_MODE_PRECISION (mode));
      do_compare_rtx_and_jump (res, convert_modes (mode, tgtmode, lres, uns),
			       EQ, true, mode, NULL_RTX, NULL, done_label,
			       profile_probability::very_likely ());
      expand_arith_set_overflow (lhs, target);
      emit_label (done_label);
    }
  int prec = TYPE_PRECISION (TREE_TYPE (TREE_TYPE (lhs)));
  int tgtprec = GET_MODE_PRECISION (tgtmode);
  if (prec < tgtprec)
    {
      rtx_code_label *done_label = gen_label_rtx ();
      int uns = TYPE_UNSIGNED (TREE_TYPE (TREE_TYPE (lhs)));
      res = lres;
      if (uns)
	{
	  rtx mask
	    = immed_wide_int_const (wi::shifted_mask (0, prec, false, tgtprec),
				    tgtmode);
	  lres = expand_simple_binop (tgtmode, AND, res, mask, NULL_RTX,
				      true, OPTAB_LIB_WIDEN);
	}
      else
	{
	  lres = expand_shift (LSHIFT_EXPR, tgtmode, res, tgtprec - prec,
			       NULL_RTX, 1);
	  lres = expand_shift (RSHIFT_EXPR, tgtmode, lres, tgtprec - prec,
			       NULL_RTX, 0);
	}
      do_compare_rtx_and_jump (res, lres,
			       EQ, true, tgtmode, NULL_RTX, NULL, done_label,
			       profile_probability::very_likely ());
      expand_arith_set_overflow (lhs, target);
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
			bool uns1_p, bool is_ubsan, tree *datap)
{
  rtx res, target = NULL_RTX;
  tree fn;
  rtx_code_label *done_label = gen_label_rtx ();
  rtx_code_label *do_error = gen_label_rtx ();
  do_pending_stack_adjust ();
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  scalar_int_mode mode = SCALAR_INT_TYPE_MODE (TREE_TYPE (arg0));
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
      std::swap (op0, op1);
      std::swap (arg0, arg1);
      uns0_p = false;
      uns1_p = true;
    }

  /* u1 +- u2 -> ur  */
  if (uns0_p && uns1_p && unsr_p)
    {
      insn_code icode = optab_handler (code == PLUS_EXPR ? uaddv4_optab
                                       : usubv4_optab, mode);
      if (icode != CODE_FOR_nothing)
	{
	  class expand_operand ops[4];
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
		add_reg_br_prob_note (last,
				      profile_probability::very_unlikely ());
	      emit_jump (done_label);
	      goto do_error_label;
	    }

	  delete_insns_since (last);
	}

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
			       true, mode, NULL_RTX, NULL, done_label,
			       profile_probability::very_likely ());
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
      do_compare_rtx_and_jump (tem, op1, GEU, true, mode, NULL_RTX, NULL,
			       done_label, profile_probability::very_likely ());
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
				 NULL, do_error, profile_probability::very_unlikely ());
      do_compare_rtx_and_jump (op1, op0, LEU, true, mode, NULL_RTX, NULL,
			       done_label, profile_probability::very_likely ());
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
      do_compare_rtx_and_jump (op0, tem, LTU, true, mode, NULL_RTX, NULL,
			       done_label, profile_probability::very_likely ());
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
			       NULL, do_error, profile_probability::very_unlikely ());
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
      do_compare_rtx_and_jump (res, tem, GEU, true, mode, NULL_RTX, NULL,
			       done_label, profile_probability::very_likely ());
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
	      std::swap (op0, op1);
	      pos_neg = pos_neg0;
	    }
	}
      rtx tem;
      if (pos_neg != 3)
	{
	  tem = expand_binop (mode, ((pos_neg == 1) ^ (code == MINUS_EXPR))
				    ? and_optab : ior_optab,
			      op0, res, NULL_RTX, false, OPTAB_LIB_WIDEN);
	  do_compare_rtx_and_jump (tem, const0_rtx, GE, false, mode, NULL,
				   NULL, done_label, profile_probability::very_likely ());
	}
      else
	{
	  rtx_code_label *do_ior_label = gen_label_rtx ();
	  do_compare_rtx_and_jump (op1, const0_rtx,
				   code == MINUS_EXPR ? GE : LT, false, mode,
				   NULL_RTX, NULL, do_ior_label,
				   profile_probability::even ());
	  tem = expand_binop (mode, and_optab, op0, res, NULL_RTX, false,
			      OPTAB_LIB_WIDEN);
	  do_compare_rtx_and_jump (tem, const0_rtx, GE, false, mode, NULL_RTX,
				   NULL, done_label, profile_probability::very_likely ());
	  emit_jump (do_error);
	  emit_label (do_ior_label);
	  tem = expand_binop (mode, ior_optab, op0, res, NULL_RTX, false,
			      OPTAB_LIB_WIDEN);
	  do_compare_rtx_and_jump (tem, const0_rtx, GE, false, mode, NULL_RTX,
				   NULL, done_label, profile_probability::very_likely ());
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
      do_compare_rtx_and_jump (op0, op1, GEU, true, mode, NULL_RTX, NULL,
			       op0_geu_op1, profile_probability::even ());
      do_compare_rtx_and_jump (res, const0_rtx, LT, false, mode, NULL_RTX,
			       NULL, done_label, profile_probability::very_likely ());
      emit_jump (do_error);
      emit_label (op0_geu_op1);
      do_compare_rtx_and_jump (res, const0_rtx, GE, false, mode, NULL_RTX,
			       NULL, done_label, profile_probability::very_likely ());
      goto do_error_label;
    }

  gcc_assert (!uns0_p && !uns1_p && !unsr_p);

  /* s1 +- s2 -> sr  */
 do_signed:
  {
    insn_code icode = optab_handler (code == PLUS_EXPR ? addv4_optab
				     : subv4_optab, mode);
    if (icode != CODE_FOR_nothing)
      {
	class expand_operand ops[4];
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
	      add_reg_br_prob_note (last, 
				    profile_probability::very_unlikely ());
	    emit_jump (done_label);
	    goto do_error_label;
	  }

	delete_insns_since (last);
      }

    /* Compute the operation.  On RTL level, the addition is always
       unsigned.  */
    res = expand_binop (mode, code == PLUS_EXPR ? add_optab : sub_optab,
			op0, op1, NULL_RTX, false, OPTAB_LIB_WIDEN);

    /* If we can prove that one of the arguments (for MINUS_EXPR only
       the second operand, as subtraction is not commutative) is always
       non-negative or always negative, we can do just one comparison
       and conditional jump.  */
    int pos_neg = get_range_pos_neg (arg1);
    if (code == PLUS_EXPR)
      {
	int pos_neg0 = get_range_pos_neg (arg0);
	if (pos_neg0 != 3 && pos_neg == 3)
	  {
	    std::swap (op0, op1);
	    pos_neg = pos_neg0;
	  }
      }

    /* Addition overflows if and only if the two operands have the same sign,
       and the result has the opposite sign.  Subtraction overflows if and
       only if the two operands have opposite sign, and the subtrahend has
       the same sign as the result.  Here 0 is counted as positive.  */
    if (pos_neg == 3)
      {
	/* Compute op0 ^ op1 (operands have opposite sign).  */
        rtx op_xor = expand_binop (mode, xor_optab, op0, op1, NULL_RTX, false,
				   OPTAB_LIB_WIDEN);

	/* Compute res ^ op1 (result and 2nd operand have opposite sign).  */
	rtx res_xor = expand_binop (mode, xor_optab, res, op1, NULL_RTX, false,
				    OPTAB_LIB_WIDEN);

	rtx tem;
	if (code == PLUS_EXPR)
	  {
	    /* Compute (res ^ op1) & ~(op0 ^ op1).  */
	    tem = expand_unop (mode, one_cmpl_optab, op_xor, NULL_RTX, false);
	    tem = expand_binop (mode, and_optab, res_xor, tem, NULL_RTX, false,
				OPTAB_LIB_WIDEN);
	  }
	else
	  {
	    /* Compute (op0 ^ op1) & ~(res ^ op1).  */
	    tem = expand_unop (mode, one_cmpl_optab, res_xor, NULL_RTX, false);
	    tem = expand_binop (mode, and_optab, op_xor, tem, NULL_RTX, false,
				OPTAB_LIB_WIDEN);
	  }

	/* No overflow if the result has bit sign cleared.  */
	do_compare_rtx_and_jump (tem, const0_rtx, GE, false, mode, NULL_RTX,
				 NULL, done_label, profile_probability::very_likely ());
      }

    /* Compare the result of the operation with the first operand.
       No overflow for addition if second operand is positive and result
       is larger or second operand is negative and result is smaller.
       Likewise for subtraction with sign of second operand flipped.  */
    else
      do_compare_rtx_and_jump (res, op0,
			       (pos_neg == 1) ^ (code == MINUS_EXPR) ? GE : LE,
			       false, mode, NULL_RTX, NULL, done_label,
			       profile_probability::very_likely ());
  }

 do_error_label:
  emit_label (do_error);
  if (is_ubsan)
    {
      /* Expand the ubsan builtin call.  */
      push_temp_slots ();
      fn = ubsan_build_overflow_builtin (code, loc, TREE_TYPE (arg0),
					 arg0, arg1, datap);
      expand_normal (fn);
      pop_temp_slots ();
      do_pending_stack_adjust ();
    }
  else if (lhs)
    expand_arith_set_overflow (lhs, target);

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
expand_neg_overflow (location_t loc, tree lhs, tree arg1, bool is_ubsan,
		     tree *datap)
{
  rtx res, op1;
  tree fn;
  rtx_code_label *done_label, *do_error;
  rtx target = NULL_RTX;

  done_label = gen_label_rtx ();
  do_error = gen_label_rtx ();

  do_pending_stack_adjust ();
  op1 = expand_normal (arg1);

  scalar_int_mode mode = SCALAR_INT_TYPE_MODE (TREE_TYPE (arg1));
  if (lhs)
    {
      target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
      if (!is_ubsan)
	write_complex_part (target, const0_rtx, true);
    }

  enum insn_code icode = optab_handler (negv3_optab, mode);
  if (icode != CODE_FOR_nothing)
    {
      class expand_operand ops[3];
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
	    add_reg_br_prob_note (last, 
				  profile_probability::very_unlikely ());
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
      do_compare_rtx_and_jump (op1, minv, NE, true, mode, NULL_RTX, NULL,
			       done_label, profile_probability::very_likely ());
    }

  emit_label (do_error);
  if (is_ubsan)
    {
      /* Expand the ubsan builtin call.  */
      push_temp_slots ();
      fn = ubsan_build_overflow_builtin (NEGATE_EXPR, loc, TREE_TYPE (arg1),
					 arg1, NULL_TREE, datap);
      expand_normal (fn);
      pop_temp_slots ();
      do_pending_stack_adjust ();
    }
  else if (lhs)
    expand_arith_set_overflow (lhs, target);

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

/* Return true if UNS WIDEN_MULT_EXPR with result mode WMODE and operand
   mode MODE can be expanded without using a libcall.  */

static bool
can_widen_mult_without_libcall (scalar_int_mode wmode, scalar_int_mode mode,
				rtx op0, rtx op1, bool uns)
{
  if (find_widening_optab_handler (umul_widen_optab, wmode, mode)
      != CODE_FOR_nothing)
    return true;
    
  if (find_widening_optab_handler (smul_widen_optab, wmode, mode)
      != CODE_FOR_nothing)
    return true;

  rtx_insn *last = get_last_insn ();
  if (CONSTANT_P (op0))
    op0 = convert_modes (wmode, mode, op0, uns);
  else
    op0 = gen_raw_REG (wmode, LAST_VIRTUAL_REGISTER + 1);
  if (CONSTANT_P (op1))
    op1 = convert_modes (wmode, mode, op1, uns);
  else
    op1 = gen_raw_REG (wmode, LAST_VIRTUAL_REGISTER + 2);
  rtx ret = expand_mult (wmode, op0, op1, NULL_RTX, uns, true);
  delete_insns_since (last);
  return ret != NULL_RTX;
} 

/* Add mul overflow checking to the statement STMT.  */

static void
expand_mul_overflow (location_t loc, tree lhs, tree arg0, tree arg1,
		     bool unsr_p, bool uns0_p, bool uns1_p, bool is_ubsan,
		     tree *datap)
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

  scalar_int_mode mode = SCALAR_INT_TYPE_MODE (TREE_TYPE (arg0));
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
      std::swap (op0, op1);
      std::swap (arg0, arg1);
      uns0_p = false;
      uns1_p = true;
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
				   NULL, done_label, profile_probability::very_likely ());
	  goto do_error_label;
	case 3:
	  rtx_code_label *do_main_label;
	  do_main_label = gen_label_rtx ();
	  do_compare_rtx_and_jump (op0, const0_rtx, GE, false, mode, NULL_RTX,
				   NULL, do_main_label, profile_probability::very_likely ());
	  do_compare_rtx_and_jump (op1, const0_rtx, EQ, true, mode, NULL_RTX,
				   NULL, do_main_label, profile_probability::very_likely ());
	  expand_arith_set_overflow (lhs, target);
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
				   NULL, done_label, profile_probability::very_likely ());
	  do_compare_rtx_and_jump (op0, constm1_rtx, NE, true, mode, NULL_RTX,
				   NULL, do_error, profile_probability::very_unlikely ());
	  int prec;
	  prec = GET_MODE_PRECISION (mode);
	  rtx sgn;
	  sgn = immed_wide_int_const (wi::min_value (prec, SIGNED), mode);
	  do_compare_rtx_and_jump (op1, sgn, EQ, true, mode, NULL_RTX,
				   NULL, done_label, profile_probability::very_likely ());
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
      rtx tem;
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
	      do_compare_rtx_and_jump (pos_neg0 == 1 ? op0 : op1, const0_rtx, EQ,
				       true, mode, NULL_RTX, NULL, done_label,
				       profile_probability::very_likely ());
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
				   NULL, after_negate_label, profile_probability::very_likely ());
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
	  tem = expand_binop (mode, xor_optab, op0, op1, NULL_RTX, false,
			      OPTAB_LIB_WIDEN);
	  do_compare_rtx_and_jump (tem, const0_rtx, GE, false, mode, NULL_RTX,
				   NULL, do_main_label,
				   profile_probability::very_likely ());
	  /* One argument is negative here, the other positive.  This
	     overflows always, unless one of the arguments is 0.  But
	     if e.g. s2 is 0, (U) s1 * 0 doesn't overflow, whatever s1
	     is, thus we can keep do_main code oring in overflow as is.  */
	  if (pos_neg0 != 2)
	    do_compare_rtx_and_jump (op0, const0_rtx, EQ, true, mode, NULL_RTX,
				     NULL, do_main_label,
				     profile_probability::very_unlikely ());
	  if (pos_neg1 != 2)
	    do_compare_rtx_and_jump (op1, const0_rtx, EQ, true, mode, NULL_RTX,
				     NULL, do_main_label,
				     profile_probability::very_unlikely ());
	  expand_arith_set_overflow (lhs, target);
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
  if (uns
      && (integer_pow2p (arg0) || integer_pow2p (arg1))
      && (optimize_insn_for_speed_p () || icode == CODE_FOR_nothing))
    {
      /* Optimize unsigned multiplication by power of 2 constant
	 using 2 shifts, one for result, one to extract the shifted
	 out bits to see if they are all zero.
	 Don't do this if optimizing for size and we have umulv4_optab,
	 in that case assume multiplication will be shorter.
	 This is heuristics based on the single target that provides
	 umulv4 right now (i?86/x86_64), if further targets add it, this
	 might need to be revisited.
	 Cases where both operands are constant should be folded already
	 during GIMPLE, and cases where one operand is constant but not
	 power of 2 are questionable, either the WIDEN_MULT_EXPR case
	 below can be done without multiplication, just by shifts and adds,
	 or we'd need to divide the result (and hope it actually doesn't
	 really divide nor multiply) and compare the result of the division
	 with the original operand.  */
      rtx opn0 = op0;
      rtx opn1 = op1;
      tree argn0 = arg0;
      tree argn1 = arg1;
      if (integer_pow2p (arg0))
	{
	  std::swap (opn0, opn1);
	  std::swap (argn0, argn1);
	}
      int cnt = tree_log2 (argn1);
      if (cnt >= 0 && cnt < GET_MODE_PRECISION (mode))
	{
	  rtx upper = const0_rtx;
	  res = expand_shift (LSHIFT_EXPR, mode, opn0, cnt, NULL_RTX, uns);
	  if (cnt != 0)
	    upper = expand_shift (RSHIFT_EXPR, mode, opn0,
				  GET_MODE_PRECISION (mode) - cnt,
				  NULL_RTX, uns);
	  do_compare_rtx_and_jump (upper, const0_rtx, EQ, true, mode,
				   NULL_RTX, NULL, done_label,
				   profile_probability::very_likely ());
	  goto do_error_label;
	}
    }
  if (icode != CODE_FOR_nothing)
    {
      class expand_operand ops[4];
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
	    add_reg_br_prob_note (last, 
				  profile_probability::very_unlikely ());
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
      scalar_int_mode hmode, wmode;
      ops.op0 = make_tree (type, op0);
      ops.op1 = make_tree (type, op1);
      ops.op2 = NULL_TREE;
      ops.location = loc;

      /* Optimize unsigned overflow check where we don't use the
	 multiplication result, just whether overflow happened.
	 If we can do MULT_HIGHPART_EXPR, that followed by
	 comparison of the result against zero is cheapest.
	 We'll still compute res, but it should be DCEd later.  */
      use_operand_p use;
      gimple *use_stmt;
      if (!is_ubsan
	  && lhs
	  && uns
	  && !(uns0_p && uns1_p && !unsr_p)
	  && can_mult_highpart_p (mode, uns) == 1
	  && single_imm_use (lhs, &use, &use_stmt)
	  && is_gimple_assign (use_stmt)
	  && gimple_assign_rhs_code (use_stmt) == IMAGPART_EXPR)
	goto highpart;

      if (GET_MODE_2XWIDER_MODE (mode).exists (&wmode)
	  && targetm.scalar_mode_supported_p (wmode)
	  && can_widen_mult_without_libcall (wmode, mode, op0, op1, uns))
	{
	twoxwider:
	  ops.code = WIDEN_MULT_EXPR;
	  ops.type
	    = build_nonstandard_integer_type (GET_MODE_PRECISION (wmode), uns);

	  res = expand_expr_real_2 (&ops, NULL_RTX, wmode, EXPAND_NORMAL);
	  rtx hipart = expand_shift (RSHIFT_EXPR, wmode, res, prec,
				     NULL_RTX, uns);
	  hipart = convert_modes (mode, wmode, hipart, uns);
	  res = convert_modes (mode, wmode, res, uns);
	  if (uns)
	    /* For the unsigned multiplication, there was overflow if
	       HIPART is non-zero.  */
	    do_compare_rtx_and_jump (hipart, const0_rtx, EQ, true, mode,
				     NULL_RTX, NULL, done_label,
				     profile_probability::very_likely ());
	  else
	    {
	      rtx signbit = expand_shift (RSHIFT_EXPR, mode, res, prec - 1,
					  NULL_RTX, 0);
	      /* RES is low half of the double width result, HIPART
		 the high half.  There was overflow if
		 HIPART is different from RES < 0 ? -1 : 0.  */
	      do_compare_rtx_and_jump (signbit, hipart, EQ, true, mode,
				       NULL_RTX, NULL, done_label,
				       profile_probability::very_likely ());
	    }
	}
      else if (can_mult_highpart_p (mode, uns) == 1)
	{
	highpart:
	  ops.code = MULT_HIGHPART_EXPR;
	  ops.type = type;

	  rtx hipart = expand_expr_real_2 (&ops, NULL_RTX, mode,
					   EXPAND_NORMAL);
	  ops.code = MULT_EXPR;
	  res = expand_expr_real_2 (&ops, NULL_RTX, mode, EXPAND_NORMAL);
	  if (uns)
	    /* For the unsigned multiplication, there was overflow if
	       HIPART is non-zero.  */
	    do_compare_rtx_and_jump (hipart, const0_rtx, EQ, true, mode,
				     NULL_RTX, NULL, done_label,
				     profile_probability::very_likely ());
	  else
	    {
	      rtx signbit = expand_shift (RSHIFT_EXPR, mode, res, prec - 1,
					  NULL_RTX, 0);
	      /* RES is low half of the double width result, HIPART
		 the high half.  There was overflow if
		 HIPART is different from RES < 0 ? -1 : 0.  */
	      do_compare_rtx_and_jump (signbit, hipart, EQ, true, mode,
				       NULL_RTX, NULL, done_label,
				       profile_probability::very_likely ());
	    }
	  
	}
      else if (int_mode_for_size (prec / 2, 1).exists (&hmode)
	       && 2 * GET_MODE_PRECISION (hmode) == prec)
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
	  hipart0 = convert_modes (hmode, mode, hipart0, uns);
	  rtx lopart0 = convert_modes (hmode, mode, op0, uns);
	  rtx signbit0 = const0_rtx;
	  if (!uns)
	    signbit0 = expand_shift (RSHIFT_EXPR, hmode, lopart0, hprec - 1,
				     NULL_RTX, 0);
	  rtx hipart1 = expand_shift (RSHIFT_EXPR, mode, op1, hprec,
				      NULL_RTX, uns);
	  hipart1 = convert_modes (hmode, mode, hipart1, uns);
	  rtx lopart1 = convert_modes (hmode, mode, op1, uns);
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
				     NULL_RTX, NULL, large_op0,
				     profile_probability::unlikely ());

	  if (!op1_small_p)
	    do_compare_rtx_and_jump (signbit1, hipart1, NE, true, hmode,
				     NULL_RTX, NULL, small_op0_large_op1,
				     profile_probability::unlikely ());

	  /* If both op0 and op1 are sign (!uns) or zero (uns) extended from
	     hmode to mode, the multiplication will never overflow.  We can
	     do just one hmode x hmode => mode widening multiplication.  */
	  tree halfstype = build_nonstandard_integer_type (hprec, uns);
	  ops.op0 = make_tree (halfstype, lopart0);
	  ops.op1 = make_tree (halfstype, lopart1);
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
				     NULL_RTX, NULL, both_ops_large,
				     profile_probability::unlikely ());

	  /* If op1 is sign (!uns) or zero (uns) extended from hmode to mode,
	     but op0 is not, prepare larger, hipart and lopart pseudos and
	     handle it together with small_op0_large_op1.  */
	  emit_move_insn (larger, op0);
	  emit_move_insn (hipart, hipart0);
	  emit_move_insn (lopart, lopart1);

	  emit_label (one_small_one_large);

	  /* lopart is the low part of the operand that is sign extended
	     to mode, larger is the other operand, hipart is the
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
					 NULL_RTX, NULL, after_hipart_neg,
					 profile_probability::even ());

	      tem = convert_modes (mode, hmode, lopart, 1);
	      tem = expand_shift (LSHIFT_EXPR, mode, tem, hprec, NULL_RTX, 1);
	      tem = expand_simple_binop (mode, MINUS, loxhi, tem, NULL_RTX,
					 1, OPTAB_WIDEN);
	      emit_move_insn (loxhi, tem);

	      emit_label (after_hipart_neg);

	      /* if (lopart < 0) loxhi -= larger;  */
	      if (smaller_sign == 0)
		emit_jump (after_lopart_neg);
	      else if (smaller_sign != -1)
		do_compare_rtx_and_jump (lopart, const0_rtx, GE, false, hmode,
					 NULL_RTX, NULL, after_lopart_neg,
					 profile_probability::even ());

	      tem = expand_simple_binop (mode, MINUS, loxhi, larger, NULL_RTX,
					 1, OPTAB_WIDEN);
	      emit_move_insn (loxhi, tem);

	      emit_label (after_lopart_neg);
	    }

	  /* loxhi += (uns) lo0xlo1 >> (bitsize / 2);  */
	  tem = expand_shift (RSHIFT_EXPR, mode, lo0xlo1, hprec, NULL_RTX, 1);
	  tem = expand_simple_binop (mode, PLUS, loxhi, tem, NULL_RTX,
				     1, OPTAB_WIDEN);
	  emit_move_insn (loxhi, tem);

	  /* if (loxhi >> (bitsize / 2)
		 == (hmode) loxhi >> (bitsize / 2 - 1))  (if !uns)
	     if (loxhi >> (bitsize / 2) == 0		 (if uns).  */
	  rtx hipartloxhi = expand_shift (RSHIFT_EXPR, mode, loxhi, hprec,
					  NULL_RTX, 0);
	  hipartloxhi = convert_modes (hmode, mode, hipartloxhi, 0);
	  rtx signbitloxhi = const0_rtx;
	  if (!uns)
	    signbitloxhi = expand_shift (RSHIFT_EXPR, hmode,
					 convert_modes (hmode, mode,
							loxhi, 0),
					 hprec - 1, NULL_RTX, 0);

	  do_compare_rtx_and_jump (signbitloxhi, hipartloxhi, NE, true, hmode,
				   NULL_RTX, NULL, do_overflow,
				   profile_probability::very_unlikely ());

	  /* res = (loxhi << (bitsize / 2)) | (hmode) lo0xlo1;  */
	  rtx loxhishifted = expand_shift (LSHIFT_EXPR, mode, loxhi, hprec,
					   NULL_RTX, 1);
	  tem = convert_modes (mode, hmode,
			       convert_modes (hmode, mode, lo0xlo1, 1), 1);

	  tem = expand_simple_binop (mode, IOR, loxhishifted, tem, res,
				     1, OPTAB_WIDEN);
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
					     NULL_RTX, 1, OPTAB_WIDEN);
		  do_compare_rtx_and_jump (tem, const1_rtx, GTU, true, hmode,
					   NULL_RTX, NULL, do_error,
					   profile_probability::very_unlikely ());
		}

	      if (!op1_medium_p)
		{
		  tem = expand_simple_binop (hmode, PLUS, hipart1, const1_rtx,
					     NULL_RTX, 1, OPTAB_WIDEN);
		  do_compare_rtx_and_jump (tem, const1_rtx, GTU, true, hmode,
					   NULL_RTX, NULL, do_error,
					   profile_probability::very_unlikely ());
		}

	      /* At this point hipart{0,1} are both in [-1, 0].  If they are
		 the same, overflow happened if res is non-positive, if they
		 are different, overflow happened if res is positive.  */
	      if (op0_sign != 1 && op1_sign != 1 && op0_sign != op1_sign)
		emit_jump (hipart_different);
	      else if (op0_sign == 1 || op1_sign == 1)
		do_compare_rtx_and_jump (hipart0, hipart1, NE, true, hmode,
					 NULL_RTX, NULL, hipart_different,
					 profile_probability::even ());

	      do_compare_rtx_and_jump (res, const0_rtx, LE, false, mode,
				       NULL_RTX, NULL, do_error,
				       profile_probability::very_unlikely ());
	      emit_jump (done_label);

	      emit_label (hipart_different);

	      do_compare_rtx_and_jump (res, const0_rtx, GE, false, mode,
				       NULL_RTX, NULL, do_error,
				       profile_probability::very_unlikely ());
	      emit_jump (done_label);
	    }

	  emit_label (do_overflow);

	  /* Overflow, do full multiplication and fallthru into do_error.  */
	  ops.op0 = make_tree (type, op0);
	  ops.op1 = make_tree (type, op1);
	  tem = expand_expr_real_2 (&ops, NULL_RTX, mode, EXPAND_NORMAL);
	  emit_move_insn (res, tem);
	}
      else if (GET_MODE_2XWIDER_MODE (mode).exists (&wmode)
	       && targetm.scalar_mode_supported_p (wmode))
	/* Even emitting a libcall is better than not detecting overflow
	   at all.  */
	goto twoxwider;
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
					 arg0, arg1, datap);
      expand_normal (fn);
      pop_temp_slots ();
      do_pending_stack_adjust ();
    }
  else if (lhs)
    expand_arith_set_overflow (lhs, target);

  /* We're done.  */
  emit_label (done_label);

  /* u1 * u2 -> sr  */
  if (uns0_p && uns1_p && !unsr_p)
    {
      rtx_code_label *all_done_label = gen_label_rtx ();
      do_compare_rtx_and_jump (res, const0_rtx, GE, false, mode, NULL_RTX,
			       NULL, all_done_label, profile_probability::very_likely ());
      expand_arith_set_overflow (lhs, target);
      emit_label (all_done_label);
    }

  /* s1 * u2 -> sr  */
  if (!uns0_p && uns1_p && !unsr_p && pos_neg1 == 3)
    {
      rtx_code_label *all_done_label = gen_label_rtx ();
      rtx_code_label *set_noovf = gen_label_rtx ();
      do_compare_rtx_and_jump (op1, const0_rtx, GE, false, mode, NULL_RTX,
			       NULL, all_done_label, profile_probability::very_likely ());
      expand_arith_set_overflow (lhs, target);
      do_compare_rtx_and_jump (op0, const0_rtx, EQ, true, mode, NULL_RTX,
			       NULL, set_noovf, profile_probability::very_likely ());
      do_compare_rtx_and_jump (op0, constm1_rtx, NE, true, mode, NULL_RTX,
			       NULL, all_done_label, profile_probability::very_unlikely ());
      do_compare_rtx_and_jump (op1, res, NE, true, mode, NULL_RTX, NULL,
			       all_done_label, profile_probability::very_unlikely ());
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

/* Expand UBSAN_CHECK_* internal function if it has vector operands.  */

static void
expand_vector_ubsan_overflow (location_t loc, enum tree_code code, tree lhs,
			      tree arg0, tree arg1)
{
  poly_uint64 cnt = TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0));
  rtx_code_label *loop_lab = NULL;
  rtx cntvar = NULL_RTX;
  tree cntv = NULL_TREE;
  tree eltype = TREE_TYPE (TREE_TYPE (arg0));
  tree sz = TYPE_SIZE (eltype);
  tree data = NULL_TREE;
  tree resv = NULL_TREE;
  rtx lhsr = NULL_RTX;
  rtx resvr = NULL_RTX;
  unsigned HOST_WIDE_INT const_cnt = 0;
  bool use_loop_p = (!cnt.is_constant (&const_cnt) || const_cnt > 4);

  if (lhs)
    {
      optab op;
      lhsr = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
      if (!VECTOR_MODE_P (GET_MODE (lhsr))
	  || (op = optab_for_tree_code (code, TREE_TYPE (arg0),
					optab_default)) == unknown_optab
	  || (optab_handler (op, TYPE_MODE (TREE_TYPE (arg0)))
	      == CODE_FOR_nothing))
	{
	  if (MEM_P (lhsr))
	    resv = make_tree (TREE_TYPE (lhs), lhsr);
	  else
	    {
	      resvr = assign_temp (TREE_TYPE (lhs), 1, 1);
	      resv = make_tree (TREE_TYPE (lhs), resvr);
	    }
	}
    }
  if (use_loop_p)
    {
      do_pending_stack_adjust ();
      loop_lab = gen_label_rtx ();
      cntvar = gen_reg_rtx (TYPE_MODE (sizetype));
      cntv = make_tree (sizetype, cntvar);
      emit_move_insn (cntvar, const0_rtx);
      emit_label (loop_lab);
    }
  if (TREE_CODE (arg0) != VECTOR_CST)
    {
      rtx arg0r = expand_normal (arg0);
      arg0 = make_tree (TREE_TYPE (arg0), arg0r);
    }
  if (TREE_CODE (arg1) != VECTOR_CST)
    {
      rtx arg1r = expand_normal (arg1);
      arg1 = make_tree (TREE_TYPE (arg1), arg1r);
    }
  for (unsigned int i = 0; i < (use_loop_p ? 1 : const_cnt); i++)
    {
      tree op0, op1, res = NULL_TREE;
      if (use_loop_p)
	{
	  tree atype = build_array_type_nelts (eltype, cnt);
	  op0 = uniform_vector_p (arg0);
	  if (op0 == NULL_TREE)
	    {
	      op0 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, atype, arg0);
	      op0 = build4_loc (loc, ARRAY_REF, eltype, op0, cntv,
				NULL_TREE, NULL_TREE);
	    }
	  op1 = uniform_vector_p (arg1);
	  if (op1 == NULL_TREE)
	    {
	      op1 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, atype, arg1);
	      op1 = build4_loc (loc, ARRAY_REF, eltype, op1, cntv,
				NULL_TREE, NULL_TREE);
	    }
	  if (resv)
	    {
	      res = fold_build1_loc (loc, VIEW_CONVERT_EXPR, atype, resv);
	      res = build4_loc (loc, ARRAY_REF, eltype, res, cntv,
				NULL_TREE, NULL_TREE);
	    }
	}
      else
	{
	  tree bitpos = bitsize_int (tree_to_uhwi (sz) * i);
	  op0 = fold_build3_loc (loc, BIT_FIELD_REF, eltype, arg0, sz, bitpos);
	  op1 = fold_build3_loc (loc, BIT_FIELD_REF, eltype, arg1, sz, bitpos);
	  if (resv)
	    res = fold_build3_loc (loc, BIT_FIELD_REF, eltype, resv, sz,
				   bitpos);
	}
      switch (code)
	{
	case PLUS_EXPR:
	  expand_addsub_overflow (loc, PLUS_EXPR, res, op0, op1,
				  false, false, false, true, &data);
	  break;
	case MINUS_EXPR:
	  if (use_loop_p ? integer_zerop (arg0) : integer_zerop (op0))
	    expand_neg_overflow (loc, res, op1, true, &data);
	  else
	    expand_addsub_overflow (loc, MINUS_EXPR, res, op0, op1,
				    false, false, false, true, &data);
	  break;
	case MULT_EXPR:
	  expand_mul_overflow (loc, res, op0, op1, false, false, false,
			       true, &data);
	  break;
	default:
	  gcc_unreachable ();
	}
    }
  if (use_loop_p)
    {
      struct separate_ops ops;
      ops.code = PLUS_EXPR;
      ops.type = TREE_TYPE (cntv);
      ops.op0 = cntv;
      ops.op1 = build_int_cst (TREE_TYPE (cntv), 1);
      ops.op2 = NULL_TREE;
      ops.location = loc;
      rtx ret = expand_expr_real_2 (&ops, cntvar, TYPE_MODE (sizetype),
				    EXPAND_NORMAL);
      if (ret != cntvar)
	emit_move_insn (cntvar, ret);
      rtx cntrtx = gen_int_mode (cnt, TYPE_MODE (sizetype));
      do_compare_rtx_and_jump (cntvar, cntrtx, NE, false,
			       TYPE_MODE (sizetype), NULL_RTX, NULL, loop_lab,
			       profile_probability::very_likely ());
    }
  if (lhs && resv == NULL_TREE)
    {
      struct separate_ops ops;
      ops.code = code;
      ops.type = TREE_TYPE (arg0);
      ops.op0 = arg0;
      ops.op1 = arg1;
      ops.op2 = NULL_TREE;
      ops.location = loc;
      rtx ret = expand_expr_real_2 (&ops, lhsr, TYPE_MODE (TREE_TYPE (arg0)),
				    EXPAND_NORMAL);
      if (ret != lhsr)
	emit_move_insn (lhsr, ret);
    }
  else if (resvr)
    emit_move_insn (lhsr, resvr);
}

/* Expand UBSAN_CHECK_ADD call STMT.  */

static void
expand_UBSAN_CHECK_ADD (internal_fn, gcall *stmt)
{
  location_t loc = gimple_location (stmt);
  tree lhs = gimple_call_lhs (stmt);
  tree arg0 = gimple_call_arg (stmt, 0);
  tree arg1 = gimple_call_arg (stmt, 1);
  if (VECTOR_TYPE_P (TREE_TYPE (arg0)))
    expand_vector_ubsan_overflow (loc, PLUS_EXPR, lhs, arg0, arg1);
  else
    expand_addsub_overflow (loc, PLUS_EXPR, lhs, arg0, arg1,
			    false, false, false, true, NULL);
}

/* Expand UBSAN_CHECK_SUB call STMT.  */

static void
expand_UBSAN_CHECK_SUB (internal_fn, gcall *stmt)
{
  location_t loc = gimple_location (stmt);
  tree lhs = gimple_call_lhs (stmt);
  tree arg0 = gimple_call_arg (stmt, 0);
  tree arg1 = gimple_call_arg (stmt, 1);
  if (VECTOR_TYPE_P (TREE_TYPE (arg0)))
    expand_vector_ubsan_overflow (loc, MINUS_EXPR, lhs, arg0, arg1);
  else if (integer_zerop (arg0))
    expand_neg_overflow (loc, lhs, arg1, true, NULL);
  else
    expand_addsub_overflow (loc, MINUS_EXPR, lhs, arg0, arg1,
			    false, false, false, true, NULL);
}

/* Expand UBSAN_CHECK_MUL call STMT.  */

static void
expand_UBSAN_CHECK_MUL (internal_fn, gcall *stmt)
{
  location_t loc = gimple_location (stmt);
  tree lhs = gimple_call_lhs (stmt);
  tree arg0 = gimple_call_arg (stmt, 0);
  tree arg1 = gimple_call_arg (stmt, 1);
  if (VECTOR_TYPE_P (TREE_TYPE (arg0)))
    expand_vector_ubsan_overflow (loc, MULT_EXPR, lhs, arg0, arg1);
  else
    expand_mul_overflow (loc, lhs, arg0, arg1, false, false, false, true,
			 NULL);
}

/* Helper function for {ADD,SUB,MUL}_OVERFLOW call stmt expansion.  */

static void
expand_arith_overflow (enum tree_code code, gimple *stmt)
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
	  scalar_int_mode mode = SCALAR_INT_TYPE_MODE (type);
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

      /* For operations with low precision, if target doesn't have them, start
	 with precres widening right away, otherwise do it only if the most
	 simple cases can't be used.  */
      const int min_precision = targetm.min_arithmetic_precision ();
      if (orig_precres == precres && precres < min_precision)
	;
      else if ((uns0_p && uns1_p && unsr_p && prec0 <= precres
		&& prec1 <= precres)
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
		  expand_neg_overflow (loc, lhs, arg1, false, NULL);
		  return;
		}
	      /* FALLTHRU */
	    case PLUS_EXPR:
	      expand_addsub_overflow (loc, code, lhs, arg0, arg1, unsr_p,
				      unsr_p, unsr_p, false, NULL);
	      return;
	    case MULT_EXPR:
	      expand_mul_overflow (loc, lhs, arg0, arg1, unsr_p,
				   unsr_p, unsr_p, false, NULL);
	      return;
	    default:
	      gcc_unreachable ();
	    }
	}

      /* For sub-word operations, retry with a wider type first.  */
      if (orig_precres == precres && precop <= BITS_PER_WORD)
	{
	  int p = MAX (min_precision, precop);
	  scalar_int_mode m = smallest_int_mode_for_size (p);
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
				    uns0_p, uns1_p, false, NULL);
	  else
	    expand_mul_overflow (loc, lhs, arg0, arg1, unsr_p,
				 uns0_p, uns1_p, false, NULL);
	  return;
	}

      /* Retry with a wider type.  */
      if (orig_precres == precres)
	{
	  int p = MAX (prec0, prec1);
	  scalar_int_mode m = smallest_int_mode_for_size (p);
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
expand_ADD_OVERFLOW (internal_fn, gcall *stmt)
{
  expand_arith_overflow (PLUS_EXPR, stmt);
}

/* Expand SUB_OVERFLOW STMT.  */

static void
expand_SUB_OVERFLOW (internal_fn, gcall *stmt)
{
  expand_arith_overflow (MINUS_EXPR, stmt);
}

/* Expand MUL_OVERFLOW STMT.  */

static void
expand_MUL_OVERFLOW (internal_fn, gcall *stmt)
{
  expand_arith_overflow (MULT_EXPR, stmt);
}

/* This should get folded in tree-vectorizer.c.  */

static void
expand_LOOP_VECTORIZED (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get folded in tree-vectorizer.c.  */

static void
expand_LOOP_DIST_ALIAS (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* Return a memory reference of type TYPE for argument INDEX of STMT.
   Use argument INDEX + 1 to derive the second (TBAA) operand.  */

static tree
expand_call_mem_ref (tree type, gcall *stmt, int index)
{
  tree addr = gimple_call_arg (stmt, index);
  tree alias_ptr_type = TREE_TYPE (gimple_call_arg (stmt, index + 1));
  unsigned int align = tree_to_shwi (gimple_call_arg (stmt, index + 1));
  if (TYPE_ALIGN (type) != align)
    type = build_aligned_type (type, align);

  tree tmp = addr;
  if (TREE_CODE (tmp) == SSA_NAME)
    {
      gimple *def = SSA_NAME_DEF_STMT (tmp);
      if (gimple_assign_single_p (def))
	tmp = gimple_assign_rhs1 (def);
    }

  if (TREE_CODE (tmp) == ADDR_EXPR)
    {
      tree mem = TREE_OPERAND (tmp, 0);
      if (TREE_CODE (mem) == TARGET_MEM_REF
	  && types_compatible_p (TREE_TYPE (mem), type))
	{
	  tree offset = TMR_OFFSET (mem);
	  if (type != TREE_TYPE (mem)
	      || alias_ptr_type != TREE_TYPE (offset)
	      || !integer_zerop (offset))
	    {
	      mem = copy_node (mem);
	      TMR_OFFSET (mem) = wide_int_to_tree (alias_ptr_type,
						   wi::to_poly_wide (offset));
	      TREE_TYPE (mem) = type;
	    }
	  return mem;
	}
    }

  return fold_build2 (MEM_REF, type, addr, build_int_cst (alias_ptr_type, 0));
}

/* Expand MASK_LOAD{,_LANES} call STMT using optab OPTAB.  */

static void
expand_mask_load_optab_fn (internal_fn, gcall *stmt, convert_optab optab)
{
  class expand_operand ops[3];
  tree type, lhs, rhs, maskt;
  rtx mem, target, mask;
  insn_code icode;

  maskt = gimple_call_arg (stmt, 2);
  lhs = gimple_call_lhs (stmt);
  if (lhs == NULL_TREE)
    return;
  type = TREE_TYPE (lhs);
  rhs = expand_call_mem_ref (type, stmt, 0);

  if (optab == vec_mask_load_lanes_optab)
    icode = get_multi_vector_move (type, optab);
  else
    icode = convert_optab_handler (optab, TYPE_MODE (type),
				   TYPE_MODE (TREE_TYPE (maskt)));

  mem = expand_expr (rhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  gcc_assert (MEM_P (mem));
  mask = expand_normal (maskt);
  target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  create_output_operand (&ops[0], target, TYPE_MODE (type));
  create_fixed_operand (&ops[1], mem);
  create_input_operand (&ops[2], mask, TYPE_MODE (TREE_TYPE (maskt)));
  expand_insn (icode, 3, ops);
}

#define expand_mask_load_lanes_optab_fn expand_mask_load_optab_fn

/* Expand MASK_STORE{,_LANES} call STMT using optab OPTAB.  */

static void
expand_mask_store_optab_fn (internal_fn, gcall *stmt, convert_optab optab)
{
  class expand_operand ops[3];
  tree type, lhs, rhs, maskt;
  rtx mem, reg, mask;
  insn_code icode;

  maskt = gimple_call_arg (stmt, 2);
  rhs = gimple_call_arg (stmt, 3);
  type = TREE_TYPE (rhs);
  lhs = expand_call_mem_ref (type, stmt, 0);

  if (optab == vec_mask_store_lanes_optab)
    icode = get_multi_vector_move (type, optab);
  else
    icode = convert_optab_handler (optab, TYPE_MODE (type),
				   TYPE_MODE (TREE_TYPE (maskt)));

  mem = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  gcc_assert (MEM_P (mem));
  mask = expand_normal (maskt);
  reg = expand_normal (rhs);
  create_fixed_operand (&ops[0], mem);
  create_input_operand (&ops[1], reg, TYPE_MODE (type));
  create_input_operand (&ops[2], mask, TYPE_MODE (TREE_TYPE (maskt)));
  expand_insn (icode, 3, ops);
}

#define expand_mask_store_lanes_optab_fn expand_mask_store_optab_fn

static void
expand_ABNORMAL_DISPATCHER (internal_fn, gcall *)
{
}

static void
expand_BUILTIN_EXPECT (internal_fn, gcall *stmt)
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

/* IFN_VA_ARG is supposed to be expanded at pass_stdarg.  So this dummy function
   should never be called.  */

static void
expand_VA_ARG (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* IFN_VEC_CONVERT is supposed to be expanded at pass_lower_vector.  So this
   dummy function should never be called.  */

static void
expand_VEC_CONVERT (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* Expand the IFN_UNIQUE function according to its first argument.  */

static void
expand_UNIQUE (internal_fn, gcall *stmt)
{
  rtx pattern = NULL_RTX;
  enum ifn_unique_kind kind
    = (enum ifn_unique_kind) TREE_INT_CST_LOW (gimple_call_arg (stmt, 0));

  switch (kind)
    {
    default:
      gcc_unreachable ();

    case IFN_UNIQUE_UNSPEC:
      if (targetm.have_unique ())
	pattern = targetm.gen_unique ();
      break;

    case IFN_UNIQUE_OACC_FORK:
    case IFN_UNIQUE_OACC_JOIN:
      if (targetm.have_oacc_fork () && targetm.have_oacc_join ())
	{
	  tree lhs = gimple_call_lhs (stmt);
	  rtx target = const0_rtx;

	  if (lhs)
	    target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);

	  rtx data_dep = expand_normal (gimple_call_arg (stmt, 1));
	  rtx axis = expand_normal (gimple_call_arg (stmt, 2));

	  if (kind == IFN_UNIQUE_OACC_FORK)
	    pattern = targetm.gen_oacc_fork (target, data_dep, axis);
	  else
	    pattern = targetm.gen_oacc_join (target, data_dep, axis);
	}
      else
	gcc_unreachable ();
      break;
    }

  if (pattern)
    emit_insn (pattern);
}

/* The size of an OpenACC compute dimension.  */

static void
expand_GOACC_DIM_SIZE (internal_fn, gcall *stmt)
{
  tree lhs = gimple_call_lhs (stmt);

  if (!lhs)
    return;

  rtx target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  if (targetm.have_oacc_dim_size ())
    {
      rtx dim = expand_expr (gimple_call_arg (stmt, 0), NULL_RTX,
			     VOIDmode, EXPAND_NORMAL);
      emit_insn (targetm.gen_oacc_dim_size (target, dim));
    }
  else
    emit_move_insn (target, GEN_INT (1));
}

/* The position of an OpenACC execution engine along one compute axis.  */

static void
expand_GOACC_DIM_POS (internal_fn, gcall *stmt)
{
  tree lhs = gimple_call_lhs (stmt);

  if (!lhs)
    return;

  rtx target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  if (targetm.have_oacc_dim_pos ())
    {
      rtx dim = expand_expr (gimple_call_arg (stmt, 0), NULL_RTX,
			     VOIDmode, EXPAND_NORMAL);
      emit_insn (targetm.gen_oacc_dim_pos (target, dim));
    }
  else
    emit_move_insn (target, const0_rtx);
}

/* This is expanded by oacc_device_lower pass.  */

static void
expand_GOACC_LOOP (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This is expanded by oacc_device_lower pass.  */

static void
expand_GOACC_REDUCTION (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This is expanded by oacc_device_lower pass.  */

static void
expand_GOACC_TILE (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* Set errno to EDOM.  */

static void
expand_SET_EDOM (internal_fn, gcall *)
{
#ifdef TARGET_EDOM
#ifdef GEN_ERRNO_RTX
  rtx errno_rtx = GEN_ERRNO_RTX;
#else
  rtx errno_rtx = gen_rtx_MEM (word_mode, gen_rtx_SYMBOL_REF (Pmode, "errno"));
#endif
  emit_move_insn (errno_rtx,
		  gen_int_mode (TARGET_EDOM, GET_MODE (errno_rtx)));
#else
  gcc_unreachable ();
#endif
}

/* Expand atomic bit test and set.  */

static void
expand_ATOMIC_BIT_TEST_AND_SET (internal_fn, gcall *call)
{
  expand_ifn_atomic_bit_test_and (call);
}

/* Expand atomic bit test and complement.  */

static void
expand_ATOMIC_BIT_TEST_AND_COMPLEMENT (internal_fn, gcall *call)
{
  expand_ifn_atomic_bit_test_and (call);
}

/* Expand atomic bit test and reset.  */

static void
expand_ATOMIC_BIT_TEST_AND_RESET (internal_fn, gcall *call)
{
  expand_ifn_atomic_bit_test_and (call);
}

/* Expand atomic bit test and set.  */

static void
expand_ATOMIC_COMPARE_EXCHANGE (internal_fn, gcall *call)
{
  expand_ifn_atomic_compare_exchange (call);
}

/* Expand LAUNDER to assignment, lhs = arg0.  */

static void
expand_LAUNDER (internal_fn, gcall *call)
{
  tree lhs = gimple_call_lhs (call);

  if (!lhs)
    return;

  expand_assignment (lhs, gimple_call_arg (call, 0), false);
}

/* Expand {MASK_,}SCATTER_STORE{S,U} call CALL using optab OPTAB.  */

static void
expand_scatter_store_optab_fn (internal_fn, gcall *stmt, direct_optab optab)
{
  internal_fn ifn = gimple_call_internal_fn (stmt);
  int rhs_index = internal_fn_stored_value_index (ifn);
  int mask_index = internal_fn_mask_index (ifn);
  tree base = gimple_call_arg (stmt, 0);
  tree offset = gimple_call_arg (stmt, 1);
  tree scale = gimple_call_arg (stmt, 2);
  tree rhs = gimple_call_arg (stmt, rhs_index);

  rtx base_rtx = expand_normal (base);
  rtx offset_rtx = expand_normal (offset);
  HOST_WIDE_INT scale_int = tree_to_shwi (scale);
  rtx rhs_rtx = expand_normal (rhs);

  class expand_operand ops[6];
  int i = 0;
  create_address_operand (&ops[i++], base_rtx);
  create_input_operand (&ops[i++], offset_rtx, TYPE_MODE (TREE_TYPE (offset)));
  create_integer_operand (&ops[i++], TYPE_UNSIGNED (TREE_TYPE (offset)));
  create_integer_operand (&ops[i++], scale_int);
  create_input_operand (&ops[i++], rhs_rtx, TYPE_MODE (TREE_TYPE (rhs)));
  if (mask_index >= 0)
    {
      tree mask = gimple_call_arg (stmt, mask_index);
      rtx mask_rtx = expand_normal (mask);
      create_input_operand (&ops[i++], mask_rtx, TYPE_MODE (TREE_TYPE (mask)));
    }

  insn_code icode = convert_optab_handler (optab, TYPE_MODE (TREE_TYPE (rhs)),
					   TYPE_MODE (TREE_TYPE (offset)));
  expand_insn (icode, i, ops);
}

/* Expand {MASK_,}GATHER_LOAD call CALL using optab OPTAB.  */

static void
expand_gather_load_optab_fn (internal_fn, gcall *stmt, direct_optab optab)
{
  tree lhs = gimple_call_lhs (stmt);
  tree base = gimple_call_arg (stmt, 0);
  tree offset = gimple_call_arg (stmt, 1);
  tree scale = gimple_call_arg (stmt, 2);

  rtx lhs_rtx = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  rtx base_rtx = expand_normal (base);
  rtx offset_rtx = expand_normal (offset);
  HOST_WIDE_INT scale_int = tree_to_shwi (scale);

  int i = 0;
  class expand_operand ops[6];
  create_output_operand (&ops[i++], lhs_rtx, TYPE_MODE (TREE_TYPE (lhs)));
  create_address_operand (&ops[i++], base_rtx);
  create_input_operand (&ops[i++], offset_rtx, TYPE_MODE (TREE_TYPE (offset)));
  create_integer_operand (&ops[i++], TYPE_UNSIGNED (TREE_TYPE (offset)));
  create_integer_operand (&ops[i++], scale_int);
  if (optab == mask_gather_load_optab)
    {
      tree mask = gimple_call_arg (stmt, 4);
      rtx mask_rtx = expand_normal (mask);
      create_input_operand (&ops[i++], mask_rtx, TYPE_MODE (TREE_TYPE (mask)));
    }
  insn_code icode = convert_optab_handler (optab, TYPE_MODE (TREE_TYPE (lhs)),
					   TYPE_MODE (TREE_TYPE (offset)));
  expand_insn (icode, i, ops);
}

/* Expand DIVMOD() using:
 a) optab handler for udivmod/sdivmod if it is available.
 b) If optab_handler doesn't exist, generate call to
    target-specific divmod libfunc.  */

static void
expand_DIVMOD (internal_fn, gcall *call_stmt)
{
  tree lhs = gimple_call_lhs (call_stmt);
  tree arg0 = gimple_call_arg (call_stmt, 0);
  tree arg1 = gimple_call_arg (call_stmt, 1);

  gcc_assert (TREE_CODE (TREE_TYPE (lhs)) == COMPLEX_TYPE);
  tree type = TREE_TYPE (TREE_TYPE (lhs));
  machine_mode mode = TYPE_MODE (type);
  bool unsignedp = TYPE_UNSIGNED (type);
  optab tab = (unsignedp) ? udivmod_optab : sdivmod_optab;

  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  rtx target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);

  rtx quotient, remainder, libfunc;

  /* Check if optab_handler exists for divmod_optab for given mode.  */
  if (optab_handler (tab, mode) != CODE_FOR_nothing)
    {
      quotient = gen_reg_rtx (mode);
      remainder = gen_reg_rtx (mode);
      expand_twoval_binop (tab, op0, op1, quotient, remainder, unsignedp);
    }

  /* Generate call to divmod libfunc if it exists.  */
  else if ((libfunc = optab_libfunc (tab, mode)) != NULL_RTX)
    targetm.expand_divmod_libfunc (libfunc, mode, op0, op1,
				   &quotient, &remainder);

  else
    gcc_unreachable ();

  /* Wrap the return value (quotient, remainder) within COMPLEX_EXPR.  */
  expand_expr (build2 (COMPLEX_EXPR, TREE_TYPE (lhs),
		       make_tree (TREE_TYPE (arg0), quotient),
		       make_tree (TREE_TYPE (arg1), remainder)),
	       target, VOIDmode, EXPAND_NORMAL);
}

/* Expand a NOP.  */

static void
expand_NOP (internal_fn, gcall *)
{
  /* Nothing.  But it shouldn't really prevail.  */
}

/* Expand a call to FN using the operands in STMT.  FN has a single
   output operand and NARGS input operands.  */

static void
expand_direct_optab_fn (internal_fn fn, gcall *stmt, direct_optab optab,
			unsigned int nargs)
{
  expand_operand *ops = XALLOCAVEC (expand_operand, nargs + 1);

  tree_pair types = direct_internal_fn_types (fn, stmt);
  insn_code icode = direct_optab_handler (optab, TYPE_MODE (types.first));
  gcc_assert (icode != CODE_FOR_nothing);

  tree lhs = gimple_call_lhs (stmt);
  rtx lhs_rtx = NULL_RTX;
  if (lhs)
    lhs_rtx = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);

  /* Do not assign directly to a promoted subreg, since there is no
     guarantee that the instruction will leave the upper bits of the
     register in the state required by SUBREG_PROMOTED_SIGN.  */
  rtx dest = lhs_rtx;
  if (dest && GET_CODE (dest) == SUBREG && SUBREG_PROMOTED_VAR_P (dest))
    dest = NULL_RTX;

  create_output_operand (&ops[0], dest, insn_data[icode].operand[0].mode);

  for (unsigned int i = 0; i < nargs; ++i)
    {
      tree rhs = gimple_call_arg (stmt, i);
      tree rhs_type = TREE_TYPE (rhs);
      rtx rhs_rtx = expand_normal (rhs);
      if (INTEGRAL_TYPE_P (rhs_type))
	create_convert_operand_from (&ops[i + 1], rhs_rtx,
				     TYPE_MODE (rhs_type),
				     TYPE_UNSIGNED (rhs_type));
      else
	create_input_operand (&ops[i + 1], rhs_rtx, TYPE_MODE (rhs_type));
    }

  expand_insn (icode, nargs + 1, ops);
  if (lhs_rtx && !rtx_equal_p (lhs_rtx, ops[0].value))
    {
      /* If the return value has an integral type, convert the instruction
	 result to that type.  This is useful for things that return an
	 int regardless of the size of the input.  If the instruction result
	 is smaller than required, assume that it is signed.

	 If the return value has a nonintegral type, its mode must match
	 the instruction result.  */
      if (GET_CODE (lhs_rtx) == SUBREG && SUBREG_PROMOTED_VAR_P (lhs_rtx))
	{
	  /* If this is a scalar in a register that is stored in a wider
	     mode than the declared mode, compute the result into its
	     declared mode and then convert to the wider mode.  */
	  gcc_checking_assert (INTEGRAL_TYPE_P (TREE_TYPE (lhs)));
	  rtx tmp = convert_to_mode (GET_MODE (lhs_rtx), ops[0].value, 0);
	  convert_move (SUBREG_REG (lhs_rtx), tmp,
			SUBREG_PROMOTED_SIGN (lhs_rtx));
	}
      else if (GET_MODE (lhs_rtx) == GET_MODE (ops[0].value))
	emit_move_insn (lhs_rtx, ops[0].value);
      else
	{
	  gcc_checking_assert (INTEGRAL_TYPE_P (TREE_TYPE (lhs)));
	  convert_move (lhs_rtx, ops[0].value, 0);
	}
    }
}

/* Expand WHILE_ULT call STMT using optab OPTAB.  */

static void
expand_while_optab_fn (internal_fn, gcall *stmt, convert_optab optab)
{
  expand_operand ops[3];
  tree rhs_type[2];

  tree lhs = gimple_call_lhs (stmt);
  tree lhs_type = TREE_TYPE (lhs);
  rtx lhs_rtx = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  create_output_operand (&ops[0], lhs_rtx, TYPE_MODE (lhs_type));

  for (unsigned int i = 0; i < 2; ++i)
    {
      tree rhs = gimple_call_arg (stmt, i);
      rhs_type[i] = TREE_TYPE (rhs);
      rtx rhs_rtx = expand_normal (rhs);
      create_input_operand (&ops[i + 1], rhs_rtx, TYPE_MODE (rhs_type[i]));
    }

  insn_code icode = convert_optab_handler (optab, TYPE_MODE (rhs_type[0]),
					   TYPE_MODE (lhs_type));

  expand_insn (icode, 3, ops);
  if (!rtx_equal_p (lhs_rtx, ops[0].value))
    emit_move_insn (lhs_rtx, ops[0].value);
}

/* Expanders for optabs that can use expand_direct_optab_fn.  */

#define expand_unary_optab_fn(FN, STMT, OPTAB) \
  expand_direct_optab_fn (FN, STMT, OPTAB, 1)

#define expand_binary_optab_fn(FN, STMT, OPTAB) \
  expand_direct_optab_fn (FN, STMT, OPTAB, 2)

#define expand_ternary_optab_fn(FN, STMT, OPTAB) \
  expand_direct_optab_fn (FN, STMT, OPTAB, 3)

#define expand_cond_unary_optab_fn(FN, STMT, OPTAB) \
  expand_direct_optab_fn (FN, STMT, OPTAB, 3)

#define expand_cond_binary_optab_fn(FN, STMT, OPTAB) \
  expand_direct_optab_fn (FN, STMT, OPTAB, 4)

#define expand_cond_ternary_optab_fn(FN, STMT, OPTAB) \
  expand_direct_optab_fn (FN, STMT, OPTAB, 5)

#define expand_fold_extract_optab_fn(FN, STMT, OPTAB) \
  expand_direct_optab_fn (FN, STMT, OPTAB, 3)

#define expand_fold_left_optab_fn(FN, STMT, OPTAB) \
  expand_direct_optab_fn (FN, STMT, OPTAB, 2)

#define expand_mask_fold_left_optab_fn(FN, STMT, OPTAB) \
  expand_direct_optab_fn (FN, STMT, OPTAB, 3)

#define expand_check_ptrs_optab_fn(FN, STMT, OPTAB) \
  expand_direct_optab_fn (FN, STMT, OPTAB, 4)

/* RETURN_TYPE and ARGS are a return type and argument list that are
   in principle compatible with FN (which satisfies direct_internal_fn_p).
   Return the types that should be used to determine whether the
   target supports FN.  */

tree_pair
direct_internal_fn_types (internal_fn fn, tree return_type, tree *args)
{
  const direct_internal_fn_info &info = direct_internal_fn (fn);
  tree type0 = (info.type0 < 0 ? return_type : TREE_TYPE (args[info.type0]));
  tree type1 = (info.type1 < 0 ? return_type : TREE_TYPE (args[info.type1]));
  return tree_pair (type0, type1);
}

/* CALL is a call whose return type and arguments are in principle
   compatible with FN (which satisfies direct_internal_fn_p).  Return the
   types that should be used to determine whether the target supports FN.  */

tree_pair
direct_internal_fn_types (internal_fn fn, gcall *call)
{
  const direct_internal_fn_info &info = direct_internal_fn (fn);
  tree op0 = (info.type0 < 0
	      ? gimple_call_lhs (call)
	      : gimple_call_arg (call, info.type0));
  tree op1 = (info.type1 < 0
	      ? gimple_call_lhs (call)
	      : gimple_call_arg (call, info.type1));
  return tree_pair (TREE_TYPE (op0), TREE_TYPE (op1));
}

/* Return true if OPTAB is supported for TYPES (whose modes should be
   the same) when the optimization type is OPT_TYPE.  Used for simple
   direct optabs.  */

static bool
direct_optab_supported_p (direct_optab optab, tree_pair types,
			  optimization_type opt_type)
{
  machine_mode mode = TYPE_MODE (types.first);
  gcc_checking_assert (mode == TYPE_MODE (types.second));
  return direct_optab_handler (optab, mode, opt_type) != CODE_FOR_nothing;
}

/* Return true if OPTAB is supported for TYPES, where the first type
   is the destination and the second type is the source.  Used for
   convert optabs.  */

static bool
convert_optab_supported_p (convert_optab optab, tree_pair types,
			   optimization_type opt_type)
{
  return (convert_optab_handler (optab, TYPE_MODE (types.first),
				 TYPE_MODE (types.second), opt_type)
	  != CODE_FOR_nothing);
}

/* Return true if load/store lanes optab OPTAB is supported for
   array type TYPES.first when the optimization type is OPT_TYPE.  */

static bool
multi_vector_optab_supported_p (convert_optab optab, tree_pair types,
				optimization_type opt_type)
{
  gcc_assert (TREE_CODE (types.first) == ARRAY_TYPE);
  machine_mode imode = TYPE_MODE (types.first);
  machine_mode vmode = TYPE_MODE (TREE_TYPE (types.first));
  return (convert_optab_handler (optab, imode, vmode, opt_type)
	  != CODE_FOR_nothing);
}

#define direct_unary_optab_supported_p direct_optab_supported_p
#define direct_binary_optab_supported_p direct_optab_supported_p
#define direct_ternary_optab_supported_p direct_optab_supported_p
#define direct_cond_unary_optab_supported_p direct_optab_supported_p
#define direct_cond_binary_optab_supported_p direct_optab_supported_p
#define direct_cond_ternary_optab_supported_p direct_optab_supported_p
#define direct_mask_load_optab_supported_p direct_optab_supported_p
#define direct_load_lanes_optab_supported_p multi_vector_optab_supported_p
#define direct_mask_load_lanes_optab_supported_p multi_vector_optab_supported_p
#define direct_gather_load_optab_supported_p convert_optab_supported_p
#define direct_mask_store_optab_supported_p direct_optab_supported_p
#define direct_store_lanes_optab_supported_p multi_vector_optab_supported_p
#define direct_mask_store_lanes_optab_supported_p multi_vector_optab_supported_p
#define direct_scatter_store_optab_supported_p convert_optab_supported_p
#define direct_while_optab_supported_p convert_optab_supported_p
#define direct_fold_extract_optab_supported_p direct_optab_supported_p
#define direct_fold_left_optab_supported_p direct_optab_supported_p
#define direct_mask_fold_left_optab_supported_p direct_optab_supported_p
#define direct_check_ptrs_optab_supported_p direct_optab_supported_p

/* Return the optab used by internal function FN.  */

static optab
direct_internal_fn_optab (internal_fn fn, tree_pair types)
{
  switch (fn)
    {
#define DEF_INTERNAL_FN(CODE, FLAGS, FNSPEC) \
    case IFN_##CODE: break;
#define DEF_INTERNAL_OPTAB_FN(CODE, FLAGS, OPTAB, TYPE) \
    case IFN_##CODE: return OPTAB##_optab;
#define DEF_INTERNAL_SIGNED_OPTAB_FN(CODE, FLAGS, SELECTOR, SIGNED_OPTAB, \
				     UNSIGNED_OPTAB, TYPE)		\
    case IFN_##CODE: return (TYPE_UNSIGNED (types.SELECTOR)		\
			     ? UNSIGNED_OPTAB ## _optab			\
			     : SIGNED_OPTAB ## _optab);
#include "internal-fn.def"

    case IFN_LAST:
      break;
    }
  gcc_unreachable ();
}

/* Return the optab used by internal function FN.  */

static optab
direct_internal_fn_optab (internal_fn fn)
{
  switch (fn)
    {
#define DEF_INTERNAL_FN(CODE, FLAGS, FNSPEC) \
    case IFN_##CODE: break;
#define DEF_INTERNAL_OPTAB_FN(CODE, FLAGS, OPTAB, TYPE) \
    case IFN_##CODE: return OPTAB##_optab;
#include "internal-fn.def"

    case IFN_LAST:
      break;
    }
  gcc_unreachable ();
}

/* Return true if FN is supported for the types in TYPES when the
   optimization type is OPT_TYPE.  The types are those associated with
   the "type0" and "type1" fields of FN's direct_internal_fn_info
   structure.  */

bool
direct_internal_fn_supported_p (internal_fn fn, tree_pair types,
				optimization_type opt_type)
{
  switch (fn)
    {
#define DEF_INTERNAL_FN(CODE, FLAGS, FNSPEC) \
    case IFN_##CODE: break;
#define DEF_INTERNAL_OPTAB_FN(CODE, FLAGS, OPTAB, TYPE) \
    case IFN_##CODE: \
      return direct_##TYPE##_optab_supported_p (OPTAB##_optab, types, \
						opt_type);
#define DEF_INTERNAL_SIGNED_OPTAB_FN(CODE, FLAGS, SELECTOR, SIGNED_OPTAB, \
				     UNSIGNED_OPTAB, TYPE)		\
    case IFN_##CODE:							\
      {									\
	optab which_optab = (TYPE_UNSIGNED (types.SELECTOR)		\
			     ? UNSIGNED_OPTAB ## _optab			\
			     : SIGNED_OPTAB ## _optab);			\
	return direct_##TYPE##_optab_supported_p (which_optab, types,	\
						  opt_type);		\
      }
#include "internal-fn.def"

    case IFN_LAST:
      break;
    }
  gcc_unreachable ();
}

/* Return true if FN is supported for type TYPE when the optimization
   type is OPT_TYPE.  The caller knows that the "type0" and "type1"
   fields of FN's direct_internal_fn_info structure are the same.  */

bool
direct_internal_fn_supported_p (internal_fn fn, tree type,
				optimization_type opt_type)
{
  const direct_internal_fn_info &info = direct_internal_fn (fn);
  gcc_checking_assert (info.type0 == info.type1);
  return direct_internal_fn_supported_p (fn, tree_pair (type, type), opt_type);
}

/* Return true if the STMT is supported when the optimization type is OPT_TYPE,
   given that STMT is a call to a direct internal function.  */

bool
direct_internal_fn_supported_p (gcall *stmt, optimization_type opt_type)
{
  internal_fn fn = gimple_call_internal_fn (stmt);
  tree_pair types = direct_internal_fn_types (fn, stmt);
  return direct_internal_fn_supported_p (fn, types, opt_type);
}

/* If FN is commutative in two consecutive arguments, return the
   index of the first, otherwise return -1.  */

int
first_commutative_argument (internal_fn fn)
{
  switch (fn)
    {
    case IFN_FMA:
    case IFN_FMS:
    case IFN_FNMA:
    case IFN_FNMS:
    case IFN_AVG_FLOOR:
    case IFN_AVG_CEIL:
    case IFN_MULHS:
    case IFN_MULHRS:
    case IFN_FMIN:
    case IFN_FMAX:
      return 0;

    case IFN_COND_ADD:
    case IFN_COND_MUL:
    case IFN_COND_MIN:
    case IFN_COND_MAX:
    case IFN_COND_AND:
    case IFN_COND_IOR:
    case IFN_COND_XOR:
    case IFN_COND_FMA:
    case IFN_COND_FMS:
    case IFN_COND_FNMA:
    case IFN_COND_FNMS:
      return 1;

    default:
      return -1;
    }
}

/* Return true if IFN_SET_EDOM is supported.  */

bool
set_edom_supported_p (void)
{
#ifdef TARGET_EDOM
  return true;
#else
  return false;
#endif
}

#define DEF_INTERNAL_OPTAB_FN(CODE, FLAGS, OPTAB, TYPE) \
  static void						\
  expand_##CODE (internal_fn fn, gcall *stmt)		\
  {							\
    expand_##TYPE##_optab_fn (fn, stmt, OPTAB##_optab);	\
  }
#define DEF_INTERNAL_SIGNED_OPTAB_FN(CODE, FLAGS, SELECTOR, SIGNED_OPTAB, \
				     UNSIGNED_OPTAB, TYPE)		\
  static void								\
  expand_##CODE (internal_fn fn, gcall *stmt)				\
  {									\
    tree_pair types = direct_internal_fn_types (fn, stmt);		\
    optab which_optab = direct_internal_fn_optab (fn, types);		\
    expand_##TYPE##_optab_fn (fn, stmt, which_optab);			\
  }
#include "internal-fn.def"

/* Routines to expand each internal function, indexed by function number.
   Each routine has the prototype:

       expand_<NAME> (gcall *stmt)

   where STMT is the statement that performs the call. */
static void (*const internal_fn_expanders[]) (internal_fn, gcall *) = {
#define DEF_INTERNAL_FN(CODE, FLAGS, FNSPEC) expand_##CODE,
#include "internal-fn.def"
  0
};

/* Invoke T(CODE, IFN) for each conditional function IFN that maps to a
   tree code CODE.  */
#define FOR_EACH_CODE_MAPPING(T) \
  T (PLUS_EXPR, IFN_COND_ADD) \
  T (MINUS_EXPR, IFN_COND_SUB) \
  T (MULT_EXPR, IFN_COND_MUL) \
  T (TRUNC_DIV_EXPR, IFN_COND_DIV) \
  T (TRUNC_MOD_EXPR, IFN_COND_MOD) \
  T (RDIV_EXPR, IFN_COND_RDIV) \
  T (MIN_EXPR, IFN_COND_MIN) \
  T (MAX_EXPR, IFN_COND_MAX) \
  T (BIT_AND_EXPR, IFN_COND_AND) \
  T (BIT_IOR_EXPR, IFN_COND_IOR) \
  T (BIT_XOR_EXPR, IFN_COND_XOR) \
  T (LSHIFT_EXPR, IFN_COND_SHL) \
  T (RSHIFT_EXPR, IFN_COND_SHR)

/* Return a function that only performs CODE when a certain condition is met
   and that uses a given fallback value otherwise.  For example, if CODE is
   a binary operation associated with conditional function FN:

     LHS = FN (COND, A, B, ELSE)

   is equivalent to the C expression:

     LHS = COND ? A CODE B : ELSE;

   operating elementwise if the operands are vectors.

   Return IFN_LAST if no such function exists.  */

internal_fn
get_conditional_internal_fn (tree_code code)
{
  switch (code)
    {
#define CASE(CODE, IFN) case CODE: return IFN;
      FOR_EACH_CODE_MAPPING(CASE)
#undef CASE
    default:
      return IFN_LAST;
    }
}

/* If IFN implements the conditional form of a tree code, return that
   tree code, otherwise return ERROR_MARK.  */

tree_code
conditional_internal_fn_code (internal_fn ifn)
{
  switch (ifn)
    {
#define CASE(CODE, IFN) case IFN: return CODE;
      FOR_EACH_CODE_MAPPING(CASE)
#undef CASE
    default:
      return ERROR_MARK;
    }
}

/* Invoke T(IFN) for each internal function IFN that also has an
   IFN_COND_* form.  */
#define FOR_EACH_COND_FN_PAIR(T) \
  T (FMA) \
  T (FMS) \
  T (FNMA) \
  T (FNMS)

/* Return a function that only performs internal function FN when a
   certain condition is met and that uses a given fallback value otherwise.
   In other words, the returned function FN' is such that:

     LHS = FN' (COND, A1, ... An, ELSE)

   is equivalent to the C expression:

     LHS = COND ? FN (A1, ..., An) : ELSE;

   operating elementwise if the operands are vectors.

   Return IFN_LAST if no such function exists.  */

internal_fn
get_conditional_internal_fn (internal_fn fn)
{
  switch (fn)
    {
#define CASE(NAME) case IFN_##NAME: return IFN_COND_##NAME;
      FOR_EACH_COND_FN_PAIR(CASE)
#undef CASE
    default:
      return IFN_LAST;
    }
}

/* If IFN implements the conditional form of an unconditional internal
   function, return that unconditional function, otherwise return IFN_LAST.  */

internal_fn
get_unconditional_internal_fn (internal_fn ifn)
{
  switch (ifn)
    {
#define CASE(NAME) case IFN_COND_##NAME: return IFN_##NAME;
      FOR_EACH_COND_FN_PAIR(CASE)
#undef CASE
    default:
      return IFN_LAST;
    }
}

/* Return true if STMT can be interpreted as a conditional tree code
   operation of the form:

     LHS = COND ? OP (RHS1, ...) : ELSE;

   operating elementwise if the operands are vectors.  This includes
   the case of an all-true COND, so that the operation always happens.

   When returning true, set:

   - *COND_OUT to the condition COND, or to NULL_TREE if the condition
     is known to be all-true
   - *CODE_OUT to the tree code
   - OPS[I] to operand I of *CODE_OUT
   - *ELSE_OUT to the fallback value ELSE, or to NULL_TREE if the
     condition is known to be all true.  */

bool
can_interpret_as_conditional_op_p (gimple *stmt, tree *cond_out,
				   tree_code *code_out,
				   tree (&ops)[3], tree *else_out)
{
  if (gassign *assign = dyn_cast <gassign *> (stmt))
    {
      *cond_out = NULL_TREE;
      *code_out = gimple_assign_rhs_code (assign);
      ops[0] = gimple_assign_rhs1 (assign);
      ops[1] = gimple_assign_rhs2 (assign);
      ops[2] = gimple_assign_rhs3 (assign);
      *else_out = NULL_TREE;
      return true;
    }
  if (gcall *call = dyn_cast <gcall *> (stmt))
    if (gimple_call_internal_p (call))
      {
	internal_fn ifn = gimple_call_internal_fn (call);
	tree_code code = conditional_internal_fn_code (ifn);
	if (code != ERROR_MARK)
	  {
	    *cond_out = gimple_call_arg (call, 0);
	    *code_out = code;
	    unsigned int nops = gimple_call_num_args (call) - 2;
	    for (unsigned int i = 0; i < 3; ++i)
	      ops[i] = i < nops ? gimple_call_arg (call, i + 1) : NULL_TREE;
	    *else_out = gimple_call_arg (call, nops + 1);
	    if (integer_truep (*cond_out))
	      {
		*cond_out = NULL_TREE;
		*else_out = NULL_TREE;
	      }
	    return true;
	  }
      }
  return false;
}

/* Return true if IFN is some form of load from memory.  */

bool
internal_load_fn_p (internal_fn fn)
{
  switch (fn)
    {
    case IFN_MASK_LOAD:
    case IFN_LOAD_LANES:
    case IFN_MASK_LOAD_LANES:
    case IFN_GATHER_LOAD:
    case IFN_MASK_GATHER_LOAD:
      return true;

    default:
      return false;
    }
}

/* Return true if IFN is some form of store to memory.  */

bool
internal_store_fn_p (internal_fn fn)
{
  switch (fn)
    {
    case IFN_MASK_STORE:
    case IFN_STORE_LANES:
    case IFN_MASK_STORE_LANES:
    case IFN_SCATTER_STORE:
    case IFN_MASK_SCATTER_STORE:
      return true;

    default:
      return false;
    }
}

/* Return true if IFN is some form of gather load or scatter store.  */

bool
internal_gather_scatter_fn_p (internal_fn fn)
{
  switch (fn)
    {
    case IFN_GATHER_LOAD:
    case IFN_MASK_GATHER_LOAD:
    case IFN_SCATTER_STORE:
    case IFN_MASK_SCATTER_STORE:
      return true;

    default:
      return false;
    }
}

/* If FN takes a vector mask argument, return the index of that argument,
   otherwise return -1.  */

int
internal_fn_mask_index (internal_fn fn)
{
  switch (fn)
    {
    case IFN_MASK_LOAD:
    case IFN_MASK_LOAD_LANES:
    case IFN_MASK_STORE:
    case IFN_MASK_STORE_LANES:
      return 2;

    case IFN_MASK_GATHER_LOAD:
    case IFN_MASK_SCATTER_STORE:
      return 4;

    default:
      return (conditional_internal_fn_code (fn) != ERROR_MARK
	      || get_unconditional_internal_fn (fn) != IFN_LAST ? 0 : -1);
    }
}

/* If FN takes a value that should be stored to memory, return the index
   of that argument, otherwise return -1.  */

int
internal_fn_stored_value_index (internal_fn fn)
{
  switch (fn)
    {
    case IFN_MASK_STORE:
    case IFN_SCATTER_STORE:
    case IFN_MASK_SCATTER_STORE:
      return 3;

    default:
      return -1;
    }
}

/* Return true if the target supports gather load or scatter store function
   IFN.  For loads, VECTOR_TYPE is the vector type of the load result,
   while for stores it is the vector type of the stored data argument.
   MEMORY_ELEMENT_TYPE is the type of the memory elements being loaded
   or stored.  OFFSET_VECTOR_TYPE is the vector type that holds the
   offset from the shared base address of each loaded or stored element.
   SCALE is the amount by which these offsets should be multiplied
   *after* they have been extended to address width.  */

bool
internal_gather_scatter_fn_supported_p (internal_fn ifn, tree vector_type,
					tree memory_element_type,
					tree offset_vector_type, int scale)
{
  if (!tree_int_cst_equal (TYPE_SIZE (TREE_TYPE (vector_type)),
			   TYPE_SIZE (memory_element_type)))
    return false;
  if (maybe_ne (TYPE_VECTOR_SUBPARTS (vector_type),
		TYPE_VECTOR_SUBPARTS (offset_vector_type)))
    return false;
  optab optab = direct_internal_fn_optab (ifn);
  insn_code icode = convert_optab_handler (optab, TYPE_MODE (vector_type),
					   TYPE_MODE (offset_vector_type));
  int output_ops = internal_load_fn_p (ifn) ? 1 : 0;
  bool unsigned_p = TYPE_UNSIGNED (TREE_TYPE (offset_vector_type));
  return (icode != CODE_FOR_nothing
	  && insn_operand_matches (icode, 2 + output_ops, GEN_INT (unsigned_p))
	  && insn_operand_matches (icode, 3 + output_ops, GEN_INT (scale)));
}

/* Return true if the target supports IFN_CHECK_{RAW,WAR}_PTRS function IFN
   for pointers of type TYPE when the accesses have LENGTH bytes and their
   common byte alignment is ALIGN.  */

bool
internal_check_ptrs_fn_supported_p (internal_fn ifn, tree type,
				    poly_uint64 length, unsigned int align)
{
  machine_mode mode = TYPE_MODE (type);
  optab optab = direct_internal_fn_optab (ifn);
  insn_code icode = direct_optab_handler (optab, mode);
  if (icode == CODE_FOR_nothing)
    return false;
  rtx length_rtx = immed_wide_int_const (length, mode);
  return (insn_operand_matches (icode, 3, length_rtx)
	  && insn_operand_matches (icode, 4, GEN_INT (align)));
}

/* Expand STMT as though it were a call to internal function FN.  */

void
expand_internal_call (internal_fn fn, gcall *stmt)
{
  internal_fn_expanders[fn] (fn, stmt);
}

/* Expand STMT, which is a call to internal function FN.  */

void
expand_internal_call (gcall *stmt)
{
  expand_internal_call (gimple_call_internal_fn (stmt), stmt);
}

/* If TYPE is a vector type, return true if IFN is a direct internal
   function that is supported for that type.  If TYPE is a scalar type,
   return true if IFN is a direct internal function that is supported for
   the target's preferred vector version of TYPE.  */

bool
vectorized_internal_fn_supported_p (internal_fn ifn, tree type)
{
  scalar_mode smode;
  if (!VECTOR_TYPE_P (type) && is_a <scalar_mode> (TYPE_MODE (type), &smode))
    {
      machine_mode vmode = targetm.vectorize.preferred_simd_mode (smode);
      if (VECTOR_MODE_P (vmode))
	type = build_vector_type_for_mode (type, vmode);
    }

  return (VECTOR_MODE_P (TYPE_MODE (type))
	  && direct_internal_fn_supported_p (ifn, type, OPTIMIZE_FOR_SPEED));
}

void
expand_PHI (internal_fn, gcall *)
{
    gcc_unreachable ();
}
