/* Schedule GIMPLE vector statements.
   Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

