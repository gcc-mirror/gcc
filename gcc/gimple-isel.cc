/* Schedule GIMPLE vector statements.
   Copyright (C) 2020-2025 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "expmed.h"
#include "optabs-tree.h"
#include "tree-eh.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "gimplify.h"
#include "tree-cfg.h"
#include "bitmap.h"
#include "tree-ssa-dce.h"
#include "memmodel.h"
#include "optabs.h"
#include "gimple-fold.h"
#include "internal-fn.h"
#include "fold-const.h"

/* Expand all ARRAY_REF(VIEW_CONVERT_EXPR) gimple assignments into calls to
   internal function based on vector type of selected expansion.

   For vec_set:

     VIEW_CONVERT_EXPR<int[4]>(u)[_1] = i_4(D);
   =>
     _7 = u;
     _8 = .VEC_SET (_7, i_4(D), _1);
     u = _8;

   For vec_extract:

      _3 = VIEW_CONVERT_EXPR<intD.1[4]>(vD.2208)[idx_2(D)];
   =>
      _4 = vD.2208;
      _3 = .VEC_EXTRACT (_4, idx_2(D));  */

static bool
gimple_expand_vec_set_extract_expr (struct function *fun,
				    gimple_stmt_iterator *gsi)
{
  gcall *new_stmt = NULL;
  gassign *ass_stmt = NULL;
  bool cfg_changed = false;

  /* Only consider code == GIMPLE_ASSIGN.  */
  gassign *stmt = dyn_cast<gassign *> (gsi_stmt (*gsi));
  if (!stmt)
    return false;

  bool is_extract = false;

  tree lhs = gimple_assign_lhs (stmt);
  tree rhs = gimple_assign_rhs1 (stmt);
  tree val, ref;
  if (TREE_CODE (lhs) == ARRAY_REF)
    {
      /* Assume it is a vec_set.  */
      val = rhs;
      ref = lhs;
    }
  else if (TREE_CODE (rhs) == ARRAY_REF)
    {
      /* vec_extract.  */
      is_extract = true;
      val = lhs;
      ref = rhs;
    }
  else
    return false;

  tree op0 = TREE_OPERAND (ref, 0);
  if (TREE_CODE (op0) == VIEW_CONVERT_EXPR && DECL_P (TREE_OPERAND (op0, 0))
      && VECTOR_TYPE_P (TREE_TYPE (TREE_OPERAND (op0, 0)))
      && TYPE_MODE (TREE_TYPE (ref))
	   == TYPE_MODE (TREE_TYPE (TREE_TYPE (TREE_OPERAND (op0, 0)))))
    {
      tree pos = TREE_OPERAND (ref, 1);

      tree view_op0 = TREE_OPERAND (op0, 0);

      tree idx = TREE_OPERAND (ref, 1);
      // if index is a constant, then check the bounds
      poly_uint64 idx_poly;
      if (poly_int_tree_p (idx, &idx_poly))
	{
	  poly_uint64 nelts = TYPE_VECTOR_SUBPARTS (TREE_TYPE (view_op0));
	  if (known_gt (idx_poly, nelts))
	    return false;
	}
      machine_mode outermode = TYPE_MODE (TREE_TYPE (view_op0));
      machine_mode extract_mode = TYPE_MODE (TREE_TYPE (ref));

      if ((auto_var_in_fn_p (view_op0, fun->decl)
	   || (VAR_P (view_op0) && DECL_HARD_REGISTER (view_op0)))
	  && !TREE_ADDRESSABLE (view_op0)
	  && ((!is_extract && can_vec_set_var_idx_p (outermode))
	      || (is_extract
		  && can_vec_extract_var_idx_p (outermode, extract_mode))))
	{
	  location_t loc = gimple_location (stmt);
	  tree var_src = make_ssa_name (TREE_TYPE (view_op0));

	  ass_stmt = gimple_build_assign (var_src, view_op0);
	  gimple_set_vuse (ass_stmt, gimple_vuse (stmt));
	  gimple_set_location (ass_stmt, loc);
	  gsi_insert_before (gsi, ass_stmt, GSI_SAME_STMT);

	  if (!is_extract)
	    {
	      tree var_dst = make_ssa_name (TREE_TYPE (view_op0));

	      new_stmt = gimple_build_call_internal (IFN_VEC_SET, 3, var_src,
						     val, pos);

	      gimple_call_set_lhs (new_stmt, var_dst);
	      gimple_set_location (new_stmt, loc);
	      gsi_insert_before (gsi, new_stmt, GSI_SAME_STMT);

	      ass_stmt = gimple_build_assign (view_op0, var_dst);
	      gimple_set_location (ass_stmt, loc);
	      gimple_move_vops (ass_stmt, stmt);
	      gsi_insert_before (gsi, ass_stmt, GSI_SAME_STMT);

	      basic_block bb = gimple_bb (stmt);
	      if (gsi_remove (gsi, true)
		  && gimple_purge_dead_eh_edges (bb))
		cfg_changed = true;
	      *gsi = gsi_for_stmt (ass_stmt);
	    }
	  else
	    {
	      new_stmt
		= gimple_build_call_internal (IFN_VEC_EXTRACT, 2, var_src, pos);
	      gimple_call_set_lhs (new_stmt, lhs);

	      gsi_replace (gsi, new_stmt, true);
	      cfg_changed = true;
	    }
	}
    }

  return cfg_changed;
}

/* Expand all VEC_COND_EXPR gimple assignments into calls to internal
   function based on type of selected expansion.  */

static gimple *
gimple_expand_vec_cond_expr (gimple_stmt_iterator *gsi)
{
  tree lhs, op0a = NULL_TREE;
  enum tree_code code;
  enum tree_code tcode;

  /* Only consider code == GIMPLE_ASSIGN.  */
  gassign *stmt = dyn_cast<gassign *> (gsi_stmt (*gsi));
  if (!stmt)
    return NULL;

  code = gimple_assign_rhs_code (stmt);
  if (code != VEC_COND_EXPR)
    return NULL;

  tree op0 = gimple_assign_rhs1 (stmt);
  tree op1 = gimple_assign_rhs2 (stmt);
  tree op2 = gimple_assign_rhs3 (stmt);
  lhs = gimple_assign_lhs (stmt);
  machine_mode mode = TYPE_MODE (TREE_TYPE (lhs));

  /* Lower mask typed, non-vector mode VEC_COND_EXPRs to bitwise operations.
     Those can end up generated by folding and at least for integer mode masks
     we cannot expect vcond expanders to exist.  We lower a ? b : c
     to (b & a) | (c & ~a).  */
  if (VECTOR_BOOLEAN_TYPE_P (TREE_TYPE (lhs))
      && !VECTOR_MODE_P (mode))
    {
      gcc_assert (types_compatible_p (TREE_TYPE (op0), TREE_TYPE (op1)));
      gimple_seq stmts = NULL;
      tree type = TREE_TYPE (lhs);
      location_t loc = gimple_location (stmt);
      tree tem0 = gimple_build (&stmts, loc, BIT_AND_EXPR, type, op1, op0);
      tree tem1 = gimple_build (&stmts, loc, BIT_NOT_EXPR, type, op0);
      tree tem2 = gimple_build (&stmts, loc, BIT_AND_EXPR, type, op2, tem1);
      tree tem3 = gimple_build (&stmts, loc, BIT_IOR_EXPR, type, tem0, tem2);
      gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
      return gimple_build_assign (lhs, tem3);
    }

  bool can_compute_op0 = true;
  gcc_assert (!COMPARISON_CLASS_P (op0));
  if (TREE_CODE (op0) == SSA_NAME)
    {
      gassign *def_stmt = dyn_cast<gassign *> (SSA_NAME_DEF_STMT (op0));
      if (def_stmt)
	{
	  tcode = gimple_assign_rhs_code (def_stmt);
	  op0a = gimple_assign_rhs1 (def_stmt);

	  tree op0_type = TREE_TYPE (op0);
	  tree op0a_type = TREE_TYPE (op0a);
	  if (TREE_CODE_CLASS (tcode) == tcc_comparison)
	    can_compute_op0 = expand_vec_cmp_expr_p (op0a_type, op0_type,
						     tcode);
	  gcc_assert (can_compute_op0);

	  if (can_compute_op0
	      && TYPE_MODE (TREE_TYPE (lhs)) == TYPE_MODE (TREE_TYPE (op0)))
	    {
	      /* Assuming c = x CMP y.  */
	      bool op1_minus_onep = integer_minus_onep (op1);
	      bool op2_zerop = integer_zerop (op2);
	      tree vtype = TREE_TYPE (lhs);
	      machine_mode vmode = TYPE_MODE (vtype);
	      /* Try to fold r = c ? -1 : 0 to r = c.  */
	      if (op1_minus_onep && op2_zerop)
		{
		  tree conv_op = build1 (VIEW_CONVERT_EXPR, vtype, op0);
		  return gimple_build_assign (lhs, conv_op);
		}
	      /* Try to fold r = c ? -1 : z to r = c | z, or
		 r = c ? c : z.  */
	      if (op1_minus_onep)
		{
		  tree conv_op = build1 (VIEW_CONVERT_EXPR, vtype, op0);
		  tree new_op1 = make_ssa_name (vtype);
		  gassign *new_stmt = gimple_build_assign (new_op1, conv_op);
		  gsi_insert_seq_before (gsi, new_stmt, GSI_SAME_STMT);
		  if (optab_handler (ior_optab, vmode) != CODE_FOR_nothing)
		    /* r = c | z */
		    return gimple_build_assign (lhs, BIT_IOR_EXPR, new_op1,
						op2);
		  /* r = c ? c : z */
		  op1 = new_op1;
		}
	      /* Try to fold r = c ? z : 0 to r = c & z, or
		 r = c ? z : c.  */
	      else if (op2_zerop)
		{
		  tree conv_op = build1 (VIEW_CONVERT_EXPR, vtype, op0);
		  tree new_op2 = make_ssa_name (vtype);
		  gassign *new_stmt = gimple_build_assign (new_op2, conv_op);
		  gsi_insert_seq_before (gsi, new_stmt, GSI_SAME_STMT);
		  if (optab_handler (and_optab, vmode) != CODE_FOR_nothing)
		    /* r = c | z */
		    return gimple_build_assign (lhs, BIT_AND_EXPR, new_op2,
						op1);
		  /* r = c ? z : c */
		  op2 = new_op2;
		}
	      bool op1_zerop = integer_zerop (op1);
	      bool op2_minus_onep = integer_minus_onep (op2);
	      /* Try to fold r = c ? 0 : z to r = .BIT_ANDN (z, c).  */
	      if (op1_zerop
		  && (direct_internal_fn_supported_p (IFN_BIT_ANDN, vtype,
						      OPTIMIZE_FOR_BOTH)))
		{
		  tree conv_op = build1 (VIEW_CONVERT_EXPR, vtype, op0);
		  tree new_op = make_ssa_name (vtype);
		  gassign *new_stmt = gimple_build_assign (new_op, conv_op);
		  gsi_insert_seq_before (gsi, new_stmt, GSI_SAME_STMT);
		  return gimple_build_call_internal (IFN_BIT_ANDN, 2, op2,
						     new_op);
		}
	      /* Try to fold r = c ? z : -1 to r = .BIT_IORN (z, c).  */
	      else if (op2_minus_onep
		       && (direct_internal_fn_supported_p (IFN_BIT_IORN, vtype,
							   OPTIMIZE_FOR_BOTH)))
		{
		  tree conv_op = build1 (VIEW_CONVERT_EXPR, vtype, op0);
		  tree new_op = make_ssa_name (vtype);
		  gassign *new_stmt = gimple_build_assign (new_op, conv_op);
		  gsi_insert_seq_before (gsi, new_stmt, GSI_SAME_STMT);
		  return gimple_build_call_internal (IFN_BIT_IORN, 2, op1,
						     new_op);
		}
	    }
	}
    }

  gcc_assert (VECTOR_BOOLEAN_TYPE_P (TREE_TYPE (op0)));
  gcc_assert (get_vcond_mask_icode (mode, TYPE_MODE (TREE_TYPE (op0)))
	      != CODE_FOR_nothing);
  return gimple_build_call_internal (IFN_VCOND_MASK, 3, op0, op1, op2);
}

/* Duplicate COND_EXPR condition defs of STMT located in BB when they are
   comparisons so RTL expansion with the help of TER
   can perform better if conversion.  */
static void
maybe_duplicate_comparison (gassign *stmt, basic_block bb)
{
  imm_use_iterator imm_iter;
  use_operand_p use_p;
  auto_vec<gassign *, 4> cond_exprs;
  tree lhs = gimple_assign_lhs (stmt);
  unsigned cnt = 0;

  /* This is should not be used for -O0 nor it is not useful
     when ter is turned off. */
  if (!optimize || !flag_tree_ter)
    return;

  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, lhs)
    {
      if (is_gimple_debug (USE_STMT (use_p)))
	continue;
      cnt++;
      /* Add the use statement if it was a cond_expr.  */
      if (gimple_bb (USE_STMT (use_p)) == bb
	  && is_gimple_assign (USE_STMT (use_p))
	  && gimple_assign_rhs_code (USE_STMT (use_p)) == COND_EXPR
	  && gimple_assign_rhs1_ptr (USE_STMT (use_p)) == use_p->use)
	cond_exprs.safe_push (as_a <gassign *> (USE_STMT (use_p)));
    }

  /* If the comparison has 0 or 1 uses, no reason to do anything. */
  if (cnt <= 1)
    return;

  /* If we only use the expression inside cond_exprs in that BB, we don't
     need to duplicate for one of them so pop the top. */
  if (cond_exprs.length () == cnt)
    cond_exprs.pop();

  while (!cond_exprs.is_empty())
    {
      auto old_top = cond_exprs.pop();
      gassign *copy = as_a <gassign *> (gimple_copy (stmt));
      tree new_def = duplicate_ssa_name (lhs, copy);
      gimple_assign_set_lhs (copy, new_def);
      auto gsi2 = gsi_for_stmt (old_top);
      gsi_insert_before (&gsi2, copy, GSI_SAME_STMT);
      gimple_assign_set_rhs1 (old_top, new_def);
      update_stmt (old_top);
    }
}

/* match.pd function to match atomic_bit_test_and pattern which
   has nop_convert:
     _1 = __atomic_fetch_or_4 (&v, 1, 0);
     _2 = (int) _1;
     _5 = _2 & 1;
 */
extern bool gimple_nop_atomic_bit_test_and_p (tree, tree *,
					      tree (*) (tree));
extern bool gimple_nop_convert (tree, tree*, tree (*) (tree));

namespace {

const pass_data pass_data_gimple_isel =
{
  GIMPLE_PASS, /* type */
  "isel", /* name */
  OPTGROUP_VEC, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa, /* todo_flags_finish */
};

class pass_gimple_isel : public gimple_opt_pass
{
public:
  pass_gimple_isel (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_gimple_isel, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override
    {
      return true;
    }

  unsigned int execute (function *fun) final override;
}; // class pass_gimple_isel



/* Convert
   _1 = __atomic_fetch_or_* (ptr_6, 1, _3);
   _7 = ~_1;
   _5 = (_Bool) _7;
   to
   _1 = __atomic_fetch_or_* (ptr_6, 1, _3);
   _8 = _1 & 1;
   _5 = _8 == 0;
   and convert
   _1 = __atomic_fetch_and_* (ptr_6, ~1, _3);
   _7 = ~_1;
   _4 = (_Bool) _7;
   to
   _1 = __atomic_fetch_and_* (ptr_6, ~1, _3);
   _8 = _1 & 1;
   _4 = (_Bool) _8;

   USE_STMT is the gimplt statement which uses the return value of
   __atomic_fetch_or_*.  LHS is the return value of __atomic_fetch_or_*.
   MASK is the mask passed to __atomic_fetch_or_*.
 */

static gimple *
convert_atomic_bit_not (enum internal_fn fn, gimple *use_stmt,
			tree lhs, tree mask)
{
  tree and_mask;
  if (fn == IFN_ATOMIC_BIT_TEST_AND_RESET)
    {
      /* MASK must be ~1.  */
      if (!operand_equal_p (build_int_cst (TREE_TYPE (lhs),
					   ~HOST_WIDE_INT_1), mask, 0))
	return nullptr;
      and_mask = build_int_cst (TREE_TYPE (lhs), 1);
    }
  else
    {
      /* MASK must be 1.  */
      if (!operand_equal_p (build_int_cst (TREE_TYPE (lhs), 1), mask, 0))
	return nullptr;
      and_mask = mask;
    }

  tree use_lhs = gimple_assign_lhs (use_stmt);

  use_operand_p use_p;
  gimple *use_not_stmt;

  if (!single_imm_use (use_lhs, &use_p, &use_not_stmt)
      || !is_gimple_assign (use_not_stmt))
    return nullptr;

  if (!CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (use_not_stmt)))
    return nullptr;

  tree use_not_lhs = gimple_assign_lhs (use_not_stmt);
  if (TREE_CODE (TREE_TYPE (use_not_lhs)) != BOOLEAN_TYPE)
    return nullptr;

  gimple_stmt_iterator gsi;
  tree var = make_ssa_name (TREE_TYPE (lhs));
  /* use_stmt need to be removed after use_nop_stmt,
     so use_lhs can be released.  */
  gimple *use_stmt_removal = use_stmt;
  use_stmt = gimple_build_assign (var, BIT_AND_EXPR, lhs, and_mask);
  gsi = gsi_for_stmt (use_not_stmt);
  gsi_insert_before (&gsi, use_stmt, GSI_NEW_STMT);
  lhs = gimple_assign_lhs (use_not_stmt);
  gimple *g = gimple_build_assign (lhs, EQ_EXPR, var,
				   build_zero_cst (TREE_TYPE (mask)));
  gsi_insert_after (&gsi, g, GSI_NEW_STMT);
  gsi = gsi_for_stmt (use_not_stmt);
  gsi_remove (&gsi, true);
  gsi = gsi_for_stmt (use_stmt_removal);
  gsi_remove (&gsi, true);
  return use_stmt;
}

/* Optimize
     mask_2 = 1 << cnt_1;
     _4 = __atomic_fetch_or_* (ptr_6, mask_2, _3);
     _5 = _4 & mask_2;
   to
     _4 = .ATOMIC_BIT_TEST_AND_SET (ptr_6, cnt_1, 0, _3);
     _5 = _4;
   If _5 is only used in _5 != 0 or _5 == 0 comparisons, 1
   is passed instead of 0, and the builtin just returns a zero
   or 1 value instead of the actual bit.
   Similarly for __sync_fetch_and_or_* (without the ", _3" part
   in there), and/or if mask_2 is a power of 2 constant.
   Similarly for xor instead of or, use ATOMIC_BIT_TEST_AND_COMPLEMENT
   in that case.  And similarly for and instead of or, except that
   the second argument to the builtin needs to be one's complement
   of the mask instead of mask.  */

static bool
optimize_atomic_bit_test_and (gimple_stmt_iterator *gsip,
			      enum internal_fn fn, bool has_model_arg,
			      bool after)
{
  gimple *call = gsi_stmt (*gsip);
  tree lhs = gimple_call_lhs (call);
  use_operand_p use_p;
  gimple *use_stmt;
  tree mask;
  optab optab;

  if (!flag_inline_atomics
      || optimize_debug
      || !gimple_call_builtin_p (call, BUILT_IN_NORMAL)
      || !lhs
      || SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs)
      || !single_imm_use (lhs, &use_p, &use_stmt)
      || !is_gimple_assign (use_stmt)
      || !gimple_vdef (call))
    return false;

  switch (fn)
    {
    case IFN_ATOMIC_BIT_TEST_AND_SET:
      optab = atomic_bit_test_and_set_optab;
      break;
    case IFN_ATOMIC_BIT_TEST_AND_COMPLEMENT:
      optab = atomic_bit_test_and_complement_optab;
      break;
    case IFN_ATOMIC_BIT_TEST_AND_RESET:
      optab = atomic_bit_test_and_reset_optab;
      break;
    default:
      return false;
    }

  tree bit = nullptr;

  mask = gimple_call_arg (call, 1);
  tree_code rhs_code = gimple_assign_rhs_code (use_stmt);
  if (rhs_code != BIT_AND_EXPR)
    {
      if (rhs_code != NOP_EXPR && rhs_code != BIT_NOT_EXPR)
	return false;

      tree use_lhs = gimple_assign_lhs (use_stmt);
      if (TREE_CODE (use_lhs) == SSA_NAME
	  && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (use_lhs))
	return false;

      tree use_rhs = gimple_assign_rhs1 (use_stmt);
      if (lhs != use_rhs)
	return false;

      if (optab_handler (optab, TYPE_MODE (TREE_TYPE (lhs)))
	  == CODE_FOR_nothing)
	return false;

      gimple *g;
      gimple_stmt_iterator gsi;
      tree var;
      int ibit = -1;

      if (rhs_code == BIT_NOT_EXPR)
	{
	  g = convert_atomic_bit_not (fn, use_stmt, lhs, mask);
	  if (!g)
	    return false;
	  use_stmt = g;
	  ibit = 0;
	}
      else if (TREE_CODE (TREE_TYPE (use_lhs)) == BOOLEAN_TYPE)
	{
	  tree and_mask;
	  if (fn == IFN_ATOMIC_BIT_TEST_AND_RESET)
	    {
	      /* MASK must be ~1.  */
	      if (!operand_equal_p (build_int_cst (TREE_TYPE (lhs),
						   ~HOST_WIDE_INT_1),
				    mask, 0))
		return false;

	      /* Convert
		 _1 = __atomic_fetch_and_* (ptr_6, ~1, _3);
		 _4 = (_Bool) _1;
		 to
		 _1 = __atomic_fetch_and_* (ptr_6, ~1, _3);
		 _5 = _1 & 1;
		 _4 = (_Bool) _5;
	       */
	      and_mask = build_int_cst (TREE_TYPE (lhs), 1);
	    }
	  else
	    {
	      and_mask = build_int_cst (TREE_TYPE (lhs), 1);
	      if (!operand_equal_p (and_mask, mask, 0))
		return false;

	      /* Convert
		 _1 = __atomic_fetch_or_* (ptr_6, 1, _3);
		 _4 = (_Bool) _1;
		 to
		 _1 = __atomic_fetch_or_* (ptr_6, 1, _3);
		 _5 = _1 & 1;
		 _4 = (_Bool) _5;
	       */
	    }
	  var = make_ssa_name (TREE_TYPE (use_rhs));
	  replace_uses_by (use_rhs, var);
	  g = gimple_build_assign (var, BIT_AND_EXPR, use_rhs,
				   and_mask);
	  gsi = gsi_for_stmt (use_stmt);
	  gsi_insert_before (&gsi, g, GSI_NEW_STMT);
	  use_stmt = g;
	  ibit = 0;
	}
      else if (TYPE_PRECISION (TREE_TYPE (use_lhs))
	       <= TYPE_PRECISION (TREE_TYPE (use_rhs)))
	{
	  gimple *use_nop_stmt;
	  if (!single_imm_use (use_lhs, &use_p, &use_nop_stmt)
	      || (!is_gimple_assign (use_nop_stmt)
		  && gimple_code (use_nop_stmt) != GIMPLE_COND))
	    return false;
	  /* Handle both
	     _4 = _5 < 0;
	     and
	     if (_5 < 0)
	   */
	  tree use_nop_lhs = nullptr;
	  rhs_code = ERROR_MARK;
	  if (is_gimple_assign (use_nop_stmt))
	    {
	      use_nop_lhs = gimple_assign_lhs (use_nop_stmt);
	      rhs_code = gimple_assign_rhs_code (use_nop_stmt);
	    }
	  if (!use_nop_lhs || rhs_code != BIT_AND_EXPR)
	    {
	      /* Also handle
		 if (_5 < 0)
	       */
	      if (use_nop_lhs
		  && TREE_CODE (use_nop_lhs) == SSA_NAME
		  && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (use_nop_lhs))
		return false;
	      if (use_nop_lhs && rhs_code == BIT_NOT_EXPR)
		{
		  /* Handle
		     _7 = ~_2;
		   */
		  g = convert_atomic_bit_not (fn, use_nop_stmt, lhs,
					      mask);
		  if (!g)
		    return false;
		  /* Convert
		     _1 = __atomic_fetch_or_4 (ptr_6, 1, _3);
		     _2 = (int) _1;
		     _7 = ~_2;
		     _5 = (_Bool) _7;
		     to
		     _1 = __atomic_fetch_or_4 (ptr_6, ~1, _3);
		     _8 = _1 & 1;
		     _5 = _8 == 0;
		     and convert
		     _1 = __atomic_fetch_and_4 (ptr_6, ~1, _3);
		     _2 = (int) _1;
		     _7 = ~_2;
		     _5 = (_Bool) _7;
		     to
		     _1 = __atomic_fetch_and_4 (ptr_6, 1, _3);
		     _8 = _1 & 1;
		     _5 = _8 == 0;
		   */
		  gsi = gsi_for_stmt (use_stmt);
		  gsi_remove (&gsi, true);
		  use_stmt = g;
		  ibit = 0;
		}
	      else
		{
		  tree cmp_rhs1, cmp_rhs2;
		  if (use_nop_lhs)
		    {
		      /* Handle
			 _4 = _5 < 0;
		       */
		      if (TREE_CODE (TREE_TYPE (use_nop_lhs))
			  != BOOLEAN_TYPE)
			return false;
		      cmp_rhs1 = gimple_assign_rhs1 (use_nop_stmt);
		      cmp_rhs2 = gimple_assign_rhs2 (use_nop_stmt);
		    }
		  else
		    {
		      /* Handle
			 if (_5 < 0)
		       */
		      rhs_code = gimple_cond_code (use_nop_stmt);
		      cmp_rhs1 = gimple_cond_lhs (use_nop_stmt);
		      cmp_rhs2 = gimple_cond_rhs (use_nop_stmt);
		    }
		  if (rhs_code != GE_EXPR && rhs_code != LT_EXPR)
		    return false;
		  if (use_lhs != cmp_rhs1)
		    return false;
		  if (!integer_zerop (cmp_rhs2))
		    return false;

		  tree and_mask;

		  unsigned HOST_WIDE_INT bytes
		    = tree_to_uhwi (TYPE_SIZE_UNIT (TREE_TYPE (use_rhs)));
		  ibit = bytes * BITS_PER_UNIT - 1;
		  unsigned HOST_WIDE_INT highest
		    = HOST_WIDE_INT_1U << ibit;

		  if (fn == IFN_ATOMIC_BIT_TEST_AND_RESET)
		    {
		      /* Get the signed maximum of the USE_RHS type.  */
		      and_mask = build_int_cst (TREE_TYPE (use_rhs),
						highest - 1);
		      if (!operand_equal_p (and_mask, mask, 0))
			return false;

		      /* Convert
			 _1 = __atomic_fetch_and_4 (ptr_6, 0x7fffffff, _3);
			 _5 = (signed int) _1;
			 _4 = _5 < 0 or _5 >= 0;
			 to
			 _1 = __atomic_fetch_and_4 (ptr_6, 0x7fffffff, _3);
			 _6 = _1 & 0x80000000;
			 _4 = _6 != 0 or _6 == 0;
			 and convert
			 _1 = __atomic_fetch_and_4 (ptr_6, 0x7fffffff, _3);
			 _5 = (signed int) _1;
			 if (_5 < 0 or _5 >= 0)
			 to
			 _1 = __atomic_fetch_and_4 (ptr_6, 0x7fffffff, _3);
			 _6 = _1 & 0x80000000;
			 if (_6 != 0 or _6 == 0)
		       */
		      and_mask = build_int_cst (TREE_TYPE (use_rhs),
						highest);
		    }
		  else
		    {
		      /* Get the signed minimum of the USE_RHS type.  */
		      and_mask = build_int_cst (TREE_TYPE (use_rhs),
						highest);
		      if (!operand_equal_p (and_mask, mask, 0))
			return false;

		      /* Convert
			 _1 = __atomic_fetch_or_4 (ptr_6, 0x80000000, _3);
			 _5 = (signed int) _1;
			 _4 = _5 < 0 or _5 >= 0;
			 to
			 _1 = __atomic_fetch_or_4 (ptr_6, 0x80000000, _3);
			 _6 = _1 & 0x80000000;
			 _4 = _6 != 0 or _6 == 0;
			 and convert
			 _1 = __atomic_fetch_or_4 (ptr_6, 0x80000000, _3);
			 _5 = (signed int) _1;
			 if (_5 < 0 or _5 >= 0)
			 to
			 _1 = __atomic_fetch_or_4 (ptr_6, 0x80000000, _3);
			 _6 = _1 & 0x80000000;
			 if (_6 != 0 or _6 == 0)
		       */
		    }
		  var = make_ssa_name (TREE_TYPE (use_rhs));
		  gimple* use_stmt_removal = use_stmt;
		  g = gimple_build_assign (var, BIT_AND_EXPR, use_rhs,
					   and_mask);
		  gsi = gsi_for_stmt (use_nop_stmt);
		  gsi_insert_before (&gsi, g, GSI_NEW_STMT);
		  use_stmt = g;
		  rhs_code = rhs_code == GE_EXPR ? EQ_EXPR : NE_EXPR;
		  tree const_zero = build_zero_cst (TREE_TYPE (use_rhs));
		  if (use_nop_lhs)
		    g = gimple_build_assign (use_nop_lhs, rhs_code,
					     var, const_zero);
		  else
		    g = gimple_build_cond (rhs_code, var, const_zero,
					   nullptr, nullptr);
		  gsi_insert_after (&gsi, g, GSI_NEW_STMT);
		  gsi = gsi_for_stmt (use_nop_stmt);
		  gsi_remove (&gsi, true);
		  gsi = gsi_for_stmt (use_stmt_removal);
		  gsi_remove (&gsi, true);
		}
	    }
	  else
	    {
	      tree match_op[3];
	      gimple *g;
	      if (!gimple_nop_atomic_bit_test_and_p (use_nop_lhs,
						     &match_op[0], NULL)
		  || SSA_NAME_OCCURS_IN_ABNORMAL_PHI (match_op[2])
		  || !single_imm_use (match_op[2], &use_p, &g)
		  || !is_gimple_assign (g))
		return false;
	      mask = match_op[0];
	      if (TREE_CODE (match_op[1]) == INTEGER_CST)
		{
		  ibit = tree_log2 (match_op[1]);
		  gcc_assert (ibit >= 0);
		}
	      else
		{
		  g = SSA_NAME_DEF_STMT (match_op[1]);
		  gcc_assert (is_gimple_assign (g));
		  bit = gimple_assign_rhs2 (g);
		}
	      /* Convert
		 _1 = __atomic_fetch_or_4 (ptr_6, mask, _3);
		 _2 = (int) _1;
		 _5 = _2 & mask;
		 to
		 _1 = __atomic_fetch_or_4 (ptr_6, mask, _3);
		 _6 = _1 & mask;
		 _5 = (int) _6;
		 and convert
		 _1 = ~mask_7;
		 _2 = (unsigned int) _1;
		 _3 = __atomic_fetch_and_4 (ptr_6, _2, 0);
		 _4 = (int) _3;
		 _5 = _4 & mask_7;
		 to
		 _1 = __atomic_fetch_and_* (ptr_6, ~mask_7, _3);
		 _12 = _3 & mask_7;
		 _5 = (int) _12;

		 and Convert
		 _1 = __atomic_fetch_and_4 (ptr_6, ~mask, _3);
		 _2 = (short int) _1;
		 _5 = _2 & mask;
		 to
		 _1 = __atomic_fetch_and_4 (ptr_6, ~mask, _3);
		 _8 = _1 & mask;
		 _5 = (short int) _8;
	      */
	      gimple_seq stmts = NULL;
	      match_op[1] = gimple_convert (&stmts,
					    TREE_TYPE (use_rhs),
					    match_op[1]);
	      var = gimple_build (&stmts, BIT_AND_EXPR,
				  TREE_TYPE (use_rhs), use_rhs, match_op[1]);
	      gsi = gsi_for_stmt (use_stmt);
	      gsi_remove (&gsi, true);
	      release_defs (use_stmt);
	      use_stmt = gimple_seq_last_stmt (stmts);
	      gsi = gsi_for_stmt (use_nop_stmt);
	      gsi_insert_seq_before (&gsi, stmts, GSI_SAME_STMT);
	      gimple_assign_set_rhs_with_ops (&gsi, CONVERT_EXPR, var);
	      update_stmt (use_nop_stmt);
	    }
	}
      else
	return false;

      if (!bit)
	{
	  if (ibit < 0)
	    gcc_unreachable ();
	  bit = build_int_cst (TREE_TYPE (lhs), ibit);
	}
    }
  else if (optab_handler (optab, TYPE_MODE (TREE_TYPE (lhs)))
	   == CODE_FOR_nothing)
    return false;

  tree use_lhs = gimple_assign_lhs (use_stmt);
  if (!use_lhs)
    return false;

  if (!bit)
    {
      if (TREE_CODE (mask) == INTEGER_CST)
	{
	  if (fn == IFN_ATOMIC_BIT_TEST_AND_RESET)
	    mask = const_unop (BIT_NOT_EXPR, TREE_TYPE (mask), mask);
	  mask = fold_convert (TREE_TYPE (lhs), mask);
	  int ibit = tree_log2 (mask);
	  if (ibit < 0)
	    return false;
	  bit = build_int_cst (TREE_TYPE (lhs), ibit);
	}
      else if (TREE_CODE (mask) == SSA_NAME)
	{
	  gimple *g = SSA_NAME_DEF_STMT (mask);
	  tree match_op;
	  if (gimple_nop_convert (mask, &match_op, NULL))
	    {
	      mask = match_op;
	      if (TREE_CODE (mask) != SSA_NAME)
		return false;
	      g = SSA_NAME_DEF_STMT (mask);
	    }
	  if (!is_gimple_assign (g))
	    return false;

	  if (fn == IFN_ATOMIC_BIT_TEST_AND_RESET)
	    {
	      if (gimple_assign_rhs_code (g) != BIT_NOT_EXPR)
		return false;
	      mask = gimple_assign_rhs1 (g);
	      if (TREE_CODE (mask) != SSA_NAME)
		return false;
	      g = SSA_NAME_DEF_STMT (mask);
	    }

	  if (!is_gimple_assign (g)
	      || gimple_assign_rhs_code (g) != LSHIFT_EXPR
	      || !integer_onep (gimple_assign_rhs1 (g)))
	    return false;
	  bit = gimple_assign_rhs2 (g);
	}
      else
	return false;

      tree cmp_mask;
      if (gimple_assign_rhs1 (use_stmt) == lhs)
	cmp_mask = gimple_assign_rhs2 (use_stmt);
      else
	cmp_mask = gimple_assign_rhs1 (use_stmt);

      tree match_op;
      if (gimple_nop_convert (cmp_mask, &match_op, NULL))
	cmp_mask = match_op;

      if (!operand_equal_p (cmp_mask, mask, 0))
	return false;
    }

  bool use_bool = true;
  bool has_debug_uses = false;
  imm_use_iterator iter;
  gimple *g;

  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (use_lhs))
    use_bool = false;
  FOR_EACH_IMM_USE_STMT (g, iter, use_lhs)
    {
      enum tree_code code = ERROR_MARK;
      tree op0 = NULL_TREE, op1 = NULL_TREE;
      if (is_gimple_debug (g))
	{
	  has_debug_uses = true;
	  continue;
	}
      else if (is_gimple_assign (g))
	switch (gimple_assign_rhs_code (g))
	  {
	  case COND_EXPR:
	    op1 = gimple_assign_rhs1 (g);
	    code = TREE_CODE (op1);
	    if (TREE_CODE_CLASS (code) != tcc_comparison)
	      break;
	    op0 = TREE_OPERAND (op1, 0);
	    op1 = TREE_OPERAND (op1, 1);
	    break;
	  case EQ_EXPR:
	  case NE_EXPR:
	    code = gimple_assign_rhs_code (g);
	    op0 = gimple_assign_rhs1 (g);
	    op1 = gimple_assign_rhs2 (g);
	    break;
	  default:
	    break;
	  }
      else if (gimple_code (g) == GIMPLE_COND)
	{
	  code = gimple_cond_code (g);
	  op0 = gimple_cond_lhs (g);
	  op1 = gimple_cond_rhs (g);
	}

      if ((code == EQ_EXPR || code == NE_EXPR)
	  && op0 == use_lhs
	  && integer_zerop (op1))
	{
	  use_operand_p use_p;
	  int n = 0;
	  FOR_EACH_IMM_USE_ON_STMT (use_p, iter)
	    n++;
	  if (n == 1)
	    continue;
	}

      use_bool = false;
      break;
    }

  tree new_lhs = make_ssa_name (TREE_TYPE (lhs));
  tree flag = build_int_cst (TREE_TYPE (lhs), use_bool);
  if (has_model_arg)
    g = gimple_build_call_internal (fn, 5, gimple_call_arg (call, 0),
				    bit, flag, gimple_call_arg (call, 2),
				    gimple_call_fn (call));
  else
    g = gimple_build_call_internal (fn, 4, gimple_call_arg (call, 0),
				    bit, flag, gimple_call_fn (call));
  gimple_call_set_lhs (g, new_lhs);
  gimple_set_location (g, gimple_location (call));
  gimple_move_vops (g, call);
  bool throws = stmt_can_throw_internal (cfun, call);
  gimple_call_set_nothrow (as_a <gcall *> (g),
			   gimple_call_nothrow_p (as_a <gcall *> (call)));
  gimple_stmt_iterator gsi = *gsip;
  gsi_insert_after (&gsi, g, GSI_NEW_STMT);
  edge e = NULL;
  if (throws)
    {
      maybe_clean_or_replace_eh_stmt (call, g);
      if (after || (use_bool && has_debug_uses))
	e = find_fallthru_edge (gsi_bb (gsi)->succs);
    }
  if (after)
    {
      /* The internal function returns the value of the specified bit
	 before the atomic operation.  If we are interested in the value
	 of the specified bit after the atomic operation (makes only sense
	 for xor, otherwise the bit content is compile time known),
	 we need to invert the bit.  */
      tree mask_convert = mask;
      gimple_seq stmts = NULL;
      if (!use_bool)
	mask_convert = gimple_convert (&stmts, TREE_TYPE (lhs), mask);
      new_lhs = gimple_build (&stmts, BIT_XOR_EXPR, TREE_TYPE (lhs), new_lhs,
			      use_bool ? build_int_cst (TREE_TYPE (lhs), 1)
				       : mask_convert);
      if (throws)
	{
	  gsi_insert_seq_on_edge_immediate (e, stmts);
	  gsi = gsi_for_stmt (gimple_seq_last (stmts));
	}
      else
	gsi_insert_seq_after (&gsi, stmts, GSI_NEW_STMT);
    }
  if (use_bool && has_debug_uses)
    {
      tree temp = NULL_TREE;
      if (!throws || after || single_pred_p (e->dest))
	{
	  temp = build_debug_expr_decl (TREE_TYPE (lhs));
	  tree t = build2 (LSHIFT_EXPR, TREE_TYPE (lhs), new_lhs, bit);
	  g = gimple_build_debug_bind (temp, t, g);
	  if (throws && !after)
	    {
	      gsi = gsi_after_labels (e->dest);
	      gsi_insert_before (&gsi, g, GSI_SAME_STMT);
	    }
	  else
	    gsi_insert_after (&gsi, g, GSI_NEW_STMT);
	}
      FOR_EACH_IMM_USE_STMT (g, iter, use_lhs)
	if (is_gimple_debug (g))
	  {
	    use_operand_p use_p;
	    if (temp == NULL_TREE)
	      gimple_debug_bind_reset_value (g);
	    else
	      FOR_EACH_IMM_USE_ON_STMT (use_p, iter)
		SET_USE (use_p, temp);
	    update_stmt (g);
	  }
    }
  SSA_NAME_OCCURS_IN_ABNORMAL_PHI (new_lhs)
    = SSA_NAME_OCCURS_IN_ABNORMAL_PHI (use_lhs);
  replace_uses_by (use_lhs, new_lhs);
  gsi = gsi_for_stmt (use_stmt);
  gsi_remove (&gsi, true);
  release_defs (use_stmt);
  gsi_remove (gsip, true);
  release_ssa_name (lhs);
  return true;
}

/* Optimize
     _4 = __atomic_add_fetch_* (ptr_6, arg_2, _3);
     _5 = _4 == 0;
   to
     _4 = .ATOMIC_ADD_FETCH_CMP_0 (EQ_EXPR, ptr_6, arg_2, _3);
     _5 = _4;
   Similarly for __sync_add_and_fetch_* (without the ", _3" part
   in there).  */

static bool
optimize_atomic_op_fetch_cmp_0 (gimple_stmt_iterator *gsip,
				enum internal_fn fn, bool has_model_arg)
{
  gimple *call = gsi_stmt (*gsip);
  tree lhs = gimple_call_lhs (call);
  use_operand_p use_p;
  gimple *use_stmt;

  if (!flag_inline_atomics
      || !gimple_call_builtin_p (call, BUILT_IN_NORMAL)
      || !lhs
      || SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs)
      || !single_imm_use (lhs, &use_p, &use_stmt)
      || !gimple_vdef (call))
    return false;

  optab optab;
  switch (fn)
    {
    case IFN_ATOMIC_ADD_FETCH_CMP_0:
      optab = atomic_add_fetch_cmp_0_optab;
      break;
    case IFN_ATOMIC_SUB_FETCH_CMP_0:
      optab = atomic_sub_fetch_cmp_0_optab;
      break;
    case IFN_ATOMIC_AND_FETCH_CMP_0:
      optab = atomic_and_fetch_cmp_0_optab;
      break;
    case IFN_ATOMIC_OR_FETCH_CMP_0:
      optab = atomic_or_fetch_cmp_0_optab;
      break;
    case IFN_ATOMIC_XOR_FETCH_CMP_0:
      optab = atomic_xor_fetch_cmp_0_optab;
      break;
    default:
      return false;
    }

  if (optab_handler (optab, TYPE_MODE (TREE_TYPE (lhs)))
      == CODE_FOR_nothing)
    return false;

  tree use_lhs = lhs;
  if (gimple_assign_cast_p (use_stmt))
    {
      use_lhs = gimple_assign_lhs (use_stmt);
      if (!tree_nop_conversion_p (TREE_TYPE (use_lhs), TREE_TYPE (lhs))
	  || (!INTEGRAL_TYPE_P (TREE_TYPE (use_lhs))
	      && !POINTER_TYPE_P (TREE_TYPE (use_lhs)))
	  || SSA_NAME_OCCURS_IN_ABNORMAL_PHI (use_lhs)
	  || !single_imm_use (use_lhs, &use_p, &use_stmt))
	return false;
    }
  enum tree_code code = ERROR_MARK;
  tree op0 = NULL_TREE, op1 = NULL_TREE;
  if (is_gimple_assign (use_stmt))
    switch (gimple_assign_rhs_code (use_stmt))
      {
      case COND_EXPR:
	op1 = gimple_assign_rhs1 (use_stmt);
	code = TREE_CODE (op1);
	if (TREE_CODE_CLASS (code) == tcc_comparison)
	  {
	    op0 = TREE_OPERAND (op1, 0);
	    op1 = TREE_OPERAND (op1, 1);
	  }
	break;
      default:
	code = gimple_assign_rhs_code (use_stmt);
	if (TREE_CODE_CLASS (code) == tcc_comparison)
	  {
	    op0 = gimple_assign_rhs1 (use_stmt);
	    op1 = gimple_assign_rhs2 (use_stmt);
	  }
	break;
      }
  else if (gimple_code (use_stmt) == GIMPLE_COND)
    {
      code = gimple_cond_code (use_stmt);
      op0 = gimple_cond_lhs (use_stmt);
      op1 = gimple_cond_rhs (use_stmt);
    }

  switch (code)
    {
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
      if (!INTEGRAL_TYPE_P (TREE_TYPE (use_lhs))
	  || TREE_CODE (TREE_TYPE (use_lhs)) == BOOLEAN_TYPE
	  || TYPE_UNSIGNED (TREE_TYPE (use_lhs)))
	return false;
      /* FALLTHRU */
    case EQ_EXPR:
    case NE_EXPR:
      if (op0 == use_lhs && integer_zerop (op1))
	break;
      return false;
    default:
      return false;
    }

  int encoded;
  switch (code)
    {
    /* Use special encoding of the operation.  We want to also
       encode the mode in the first argument and for neither EQ_EXPR
       etc. nor EQ etc. we can rely it will fit into QImode.  */
    case EQ_EXPR: encoded = ATOMIC_OP_FETCH_CMP_0_EQ; break;
    case NE_EXPR: encoded = ATOMIC_OP_FETCH_CMP_0_NE; break;
    case LT_EXPR: encoded = ATOMIC_OP_FETCH_CMP_0_LT; break;
    case LE_EXPR: encoded = ATOMIC_OP_FETCH_CMP_0_LE; break;
    case GT_EXPR: encoded = ATOMIC_OP_FETCH_CMP_0_GT; break;
    case GE_EXPR: encoded = ATOMIC_OP_FETCH_CMP_0_GE; break;
    default: gcc_unreachable ();
    }

  tree new_lhs = make_ssa_name (boolean_type_node);
  gimple *g;
  tree flag = build_int_cst (TREE_TYPE (lhs), encoded);
  if (has_model_arg)
    g = gimple_build_call_internal (fn, 5, flag,
				    gimple_call_arg (call, 0),
				    gimple_call_arg (call, 1),
				    gimple_call_arg (call, 2),
				    gimple_call_fn (call));
  else
    g = gimple_build_call_internal (fn, 4, flag,
				    gimple_call_arg (call, 0),
				    gimple_call_arg (call, 1),
				    gimple_call_fn (call));
  gimple_call_set_lhs (g, new_lhs);
  gimple_set_location (g, gimple_location (call));
  gimple_move_vops (g, call);
  bool throws = stmt_can_throw_internal (cfun, call);
  gimple_call_set_nothrow (as_a <gcall *> (g),
			   gimple_call_nothrow_p (as_a <gcall *> (call)));
  gimple_stmt_iterator gsi = *gsip;
  gsi_insert_after (&gsi, g, GSI_SAME_STMT);
  if (throws)
    maybe_clean_or_replace_eh_stmt (call, g);
  if (is_gimple_assign (use_stmt))
    switch (gimple_assign_rhs_code (use_stmt))
      {
      case COND_EXPR:
	gimple_assign_set_rhs1 (use_stmt, new_lhs);
	break;
      default:
	gsi = gsi_for_stmt (use_stmt);
	if (tree ulhs = gimple_assign_lhs (use_stmt))
	  if (useless_type_conversion_p (TREE_TYPE (ulhs),
					 boolean_type_node))
	    {
	      gimple_assign_set_rhs_with_ops (&gsi, SSA_NAME, new_lhs);
	      break;
	    }
	gimple_assign_set_rhs_with_ops (&gsi, NOP_EXPR, new_lhs);
	break;
      }
  else if (gimple_code (use_stmt) == GIMPLE_COND)
    {
      gcond *use_cond = as_a <gcond *> (use_stmt);
      gimple_cond_set_code (use_cond, NE_EXPR);
      gimple_cond_set_lhs (use_cond, new_lhs);
      gimple_cond_set_rhs (use_cond, boolean_false_node);
    }

  update_stmt (use_stmt);
  if (use_lhs != lhs)
    {
      gsi = gsi_for_stmt (SSA_NAME_DEF_STMT (use_lhs));
      gsi_remove (&gsi, true);
      release_ssa_name (use_lhs);
    }
  gsi_remove (gsip, true);
  release_ssa_name (lhs);
  return true;
}

/* Process builtin CALL located at GSI.
   Currently it is only fgr atomic functions optimizations from above. */
static void
gimple_isel_builtin_call (gcall *call, gimple_stmt_iterator *gsi)
{
  /* Don't handle these in non optimization mode or optimize debug mode.  */
  if (!optimize || optimize_debug)
    return;

  if (!gimple_call_builtin_p (call, BUILT_IN_NORMAL))
    return;

  tree callee = gimple_call_fndecl (call);
  
  switch (DECL_FUNCTION_CODE (callee))
    {
#define CASE_ATOMIC(NAME) 			\
      case BUILT_IN_##NAME##_1:	\
      case BUILT_IN_##NAME##_2:	\
      case BUILT_IN_##NAME##_4:	\
      case BUILT_IN_##NAME##_8:	\
      case BUILT_IN_##NAME##_16
#define CASE_ATOMIC_CMP0(ATOMIC, SYNC) 					\
      CASE_ATOMIC(ATOMIC_##ATOMIC):					\
	optimize_atomic_op_fetch_cmp_0 (gsi,				\
					IFN_ATOMIC_##ATOMIC##_CMP_0,	\
					true);				\
	break;								\
      CASE_ATOMIC(SYNC_##SYNC):						\
	optimize_atomic_op_fetch_cmp_0 (gsi,				\
					IFN_ATOMIC_##ATOMIC##_CMP_0, 	\
					false);				\
      break;


      CASE_ATOMIC_CMP0(ADD_FETCH, ADD_AND_FETCH)
      CASE_ATOMIC_CMP0(SUB_FETCH, SUB_AND_FETCH)
      CASE_ATOMIC_CMP0(AND_FETCH, AND_AND_FETCH)
      CASE_ATOMIC_CMP0(OR_FETCH, OR_AND_FETCH)
#define CASE_ATOMIC_BIT_TEST_AND(ATOMIC, SYNC, FN, AFTER) 		\
      CASE_ATOMIC(ATOMIC_##ATOMIC):					\
	optimize_atomic_bit_test_and (gsi,				\
				      IFN_ATOMIC_BIT_TEST_AND_##FN,	\
				      true, AFTER);			\
	break;								\
      CASE_ATOMIC(SYNC_##SYNC):						\
	optimize_atomic_bit_test_and (gsi,				\
				      IFN_ATOMIC_BIT_TEST_AND_##FN, 	\
				      false, AFTER);			\
        break;
      CASE_ATOMIC_BIT_TEST_AND(FETCH_OR,  FETCH_AND_OR,  SET, false)
      CASE_ATOMIC_BIT_TEST_AND(FETCH_XOR, FETCH_AND_XOR, COMPLEMENT, false)
      CASE_ATOMIC_BIT_TEST_AND(FETCH_AND, FETCH_AND_AND, RESET, false)

      CASE_ATOMIC(ATOMIC_XOR_FETCH):
	if (optimize_atomic_bit_test_and
	     (gsi, IFN_ATOMIC_BIT_TEST_AND_COMPLEMENT, true, true))
	  break;
	optimize_atomic_op_fetch_cmp_0 (gsi,
					IFN_ATOMIC_XOR_FETCH_CMP_0,
					true);
	break;
      CASE_ATOMIC(SYNC_XOR_AND_FETCH):
	if (optimize_atomic_bit_test_and
	      (gsi, IFN_ATOMIC_BIT_TEST_AND_COMPLEMENT, false, true))
	  break;
	optimize_atomic_op_fetch_cmp_0 (gsi,
					IFN_ATOMIC_XOR_FETCH_CMP_0,
					false);
        break;

      default:;
    }
}

/* Iterate all gimple statements and perform pre RTL expansion
   GIMPLE massaging to improve instruction selection.  */

unsigned int
pass_gimple_isel::execute (struct function *fun)
{
  gimple_stmt_iterator gsi;
  basic_block bb;
  bool cfg_changed = false;

  FOR_EACH_BB_FN (bb, fun)
    {
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  /* Give the target first try at replacing the instruction.  */
	  cfg_changed |= targetm.instruction_selection (fun, &gsi);

	  /* Pre-expand VEC_COND_EXPRs to .VCOND* internal function
	     calls mapping to supported optabs.  */
	  gimple *g = gimple_expand_vec_cond_expr (&gsi);
	  if (g != NULL)
	    {
	      tree lhs = gimple_assign_lhs (gsi_stmt (gsi));
	      gimple_set_lhs (g, lhs);
	      gsi_replace (&gsi, g, false);
	    }

	  /* Recognize .VEC_SET and .VEC_EXTRACT patterns.  */
	  cfg_changed |= gimple_expand_vec_set_extract_expr (fun, &gsi);
	  if (gsi_end_p (gsi))
	    break;

	  if (gcall *call = dyn_cast <gcall*>(*gsi))
	    {
	      gimple_isel_builtin_call (call, &gsi);
	      continue;
	    }
	  gassign *stmt = dyn_cast <gassign *> (*gsi);
	  if (!stmt)
	    continue;

	  tree_code code = gimple_assign_rhs_code (stmt);
	  if (TREE_CODE_CLASS (code) == tcc_comparison)
	    maybe_duplicate_comparison (stmt, bb);
	}
    }

  return cfg_changed ? TODO_cleanup_cfg : 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_gimple_isel (gcc::context *ctxt)
{
  return new pass_gimple_isel (ctxt);
}

