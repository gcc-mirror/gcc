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
gimple_expand_vec_cond_expr (struct function *fun, gimple_stmt_iterator *gsi,
			     hash_map<tree, unsigned int> *vec_cond_ssa_name_uses)
{
  tree lhs, op0a = NULL_TREE, op0b = NULL_TREE;
  enum tree_code code;
  enum tree_code tcode;
  machine_mode cmp_op_mode;
  bool unsignedp;
  enum insn_code icode;
  imm_use_iterator imm_iter;

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
      unsigned int used_vec_cond_exprs = 0;
      unsigned int *slot = vec_cond_ssa_name_uses->get (op0);
      if (slot)
	used_vec_cond_exprs = *slot;
      else
	{
	  gimple *use_stmt;
	  FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, op0)
	    {
	      gassign *assign = dyn_cast<gassign *> (use_stmt);
	      if (assign != NULL
		  && gimple_assign_rhs_code (assign) == VEC_COND_EXPR
		  && gimple_assign_rhs1 (assign) == op0)
		used_vec_cond_exprs++;
	    }
	  vec_cond_ssa_name_uses->put (op0, used_vec_cond_exprs);
	}

      gassign *def_stmt = dyn_cast<gassign *> (SSA_NAME_DEF_STMT (op0));
      if (def_stmt)
	{
	  tcode = gimple_assign_rhs_code (def_stmt);
	  op0a = gimple_assign_rhs1 (def_stmt);
	  op0b = gimple_assign_rhs2 (def_stmt);

	  tree op0_type = TREE_TYPE (op0);
	  tree op0a_type = TREE_TYPE (op0a);
	  if (TREE_CODE_CLASS (tcode) == tcc_comparison)
	    can_compute_op0 = expand_vec_cmp_expr_p (op0a_type, op0_type,
						     tcode);

	  /* Try to fold x CMP y ? -1 : 0 to x CMP y.  */
	  if (can_compute_op0
	      && integer_minus_onep (op1)
	      && integer_zerop (op2)
	      && TYPE_MODE (TREE_TYPE (lhs)) == TYPE_MODE (TREE_TYPE (op0)))
	    {
	      tree conv_op = build1 (VIEW_CONVERT_EXPR, TREE_TYPE (lhs), op0);
	      gassign *new_stmt = gimple_build_assign (lhs, conv_op);
	      gsi_replace (gsi, new_stmt, true);
	      return new_stmt;
	    }

	  /* When the compare has EH we do not want to forward it when
	     it has multiple uses and in general because of the complication
	     with EH redirection.  */
	  if (stmt_can_throw_internal (fun, def_stmt))
	    tcode = TREE_CODE (op0);

	  /* If we can compute op0 and have multiple uses, keep the SSA
	     name and use vcond_mask.  */
	  else if (can_compute_op0
		   && used_vec_cond_exprs >= 2
		   && (get_vcond_mask_icode (mode, TYPE_MODE (op0_type))
		       != CODE_FOR_nothing))
	    tcode = TREE_CODE (op0);
	}
      else
	tcode = TREE_CODE (op0);
    }
  else
    tcode = TREE_CODE (op0);

  if (TREE_CODE_CLASS (tcode) != tcc_comparison)
    {
      gcc_assert (VECTOR_BOOLEAN_TYPE_P (TREE_TYPE (op0)));
      if (get_vcond_mask_icode (mode, TYPE_MODE (TREE_TYPE (op0)))
	  != CODE_FOR_nothing)
	return gimple_build_call_internal (IFN_VCOND_MASK, 3, op0, op1, op2);
      /* Fake op0 < 0.  */
      else
	{
	  gcc_assert (GET_MODE_CLASS (TYPE_MODE (TREE_TYPE (op0)))
		      == MODE_VECTOR_INT);
	  op0a = op0;
	  op0b = build_zero_cst (TREE_TYPE (op0));
	  tcode = LT_EXPR;
	}
    }
  cmp_op_mode = TYPE_MODE (TREE_TYPE (op0a));
  unsignedp = TYPE_UNSIGNED (TREE_TYPE (op0a));

  gcc_assert (known_eq (GET_MODE_NUNITS (mode),
			GET_MODE_NUNITS (cmp_op_mode)));

  icode = get_vcond_icode (mode, cmp_op_mode, unsignedp);
  /* Some targets do not have vcondeq and only vcond with NE/EQ
     but not vcondu, so make sure to also try vcond here as
     vcond_icode_p would canonicalize the optab query to.  */
  if (icode == CODE_FOR_nothing
      && (tcode == NE_EXPR || tcode == EQ_EXPR)
      && ((icode = get_vcond_icode (mode, cmp_op_mode, !unsignedp))
	  != CODE_FOR_nothing))
    unsignedp = !unsignedp;
  if (icode == CODE_FOR_nothing)
    {
      if (tcode == LT_EXPR
	  && op0a == op0)
	{
	  /* A VEC_COND_EXPR condition could be folded from EQ_EXPR/NE_EXPR
	     into a constant when only get_vcond_eq_icode is supported.
	     Try changing it to NE_EXPR.  */
	  tcode = NE_EXPR;
	}
      if ((tcode == EQ_EXPR || tcode == NE_EXPR)
	  && direct_internal_fn_supported_p (IFN_VCONDEQ, TREE_TYPE (lhs),
					     TREE_TYPE (op0a),
					     OPTIMIZE_FOR_BOTH))
	{
	  tree tcode_tree = build_int_cst (integer_type_node, tcode);
	  return gimple_build_call_internal (IFN_VCONDEQ, 5, op0a, op0b, op1,
					     op2, tcode_tree);
	}

      gcc_assert (VECTOR_BOOLEAN_TYPE_P (TREE_TYPE (op0))
		  && can_compute_op0
		  && (get_vcond_mask_icode (mode, TYPE_MODE (TREE_TYPE (op0)))
		      != CODE_FOR_nothing));
      return gimple_build_call_internal (IFN_VCOND_MASK, 3, op0, op1, op2);
    }

  tree tcode_tree = build_int_cst (integer_type_node, tcode);
  return gimple_build_call_internal (unsignedp ? IFN_VCONDU : IFN_VCOND,
				     5, op0a, op0b, op1, op2, tcode_tree);
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
  hash_map<tree, unsigned int> vec_cond_ssa_name_uses;
  auto_bitmap dce_ssa_names;
  bool cfg_changed = false;

  FOR_EACH_BB_FN (bb, fun)
    {
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  /* Pre-expand VEC_COND_EXPRs to .VCOND* internal function
	     calls mapping to supported optabs.  */
	  gimple *g = gimple_expand_vec_cond_expr (fun, &gsi,
						   &vec_cond_ssa_name_uses);
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
	  tree lhs = gimple_assign_lhs (stmt);
	  if (TREE_CODE_CLASS (code) == tcc_comparison
	      && !has_single_use (lhs))
	    {
	      /* Duplicate COND_EXPR condition defs when they are
		 comparisons so RTL expansion with the help of TER
		 can perform better if conversion.  */
	      imm_use_iterator imm_iter;
	      use_operand_p use_p;
	      auto_vec<gassign *, 4> cond_exprs;
	      unsigned cnt = 0;
	      FOR_EACH_IMM_USE_FAST (use_p, imm_iter, lhs)
		{
		  if (is_gimple_debug (USE_STMT (use_p)))
		    continue;
		  cnt++;
		  if (gimple_bb (USE_STMT (use_p)) == bb
		      && is_gimple_assign (USE_STMT (use_p))
		      && gimple_assign_rhs1_ptr (USE_STMT (use_p)) == use_p->use
		      && gimple_assign_rhs_code (USE_STMT (use_p)) == COND_EXPR)
		    cond_exprs.safe_push (as_a <gassign *> (USE_STMT (use_p)));
		}
	      for (unsigned i = cond_exprs.length () == cnt ? 1 : 0;
		   i < cond_exprs.length (); ++i)
		{
		  gassign *copy = as_a <gassign *> (gimple_copy (stmt));
		  tree new_def = duplicate_ssa_name (lhs, copy);
		  gimple_assign_set_lhs (copy, new_def);
		  auto gsi2 = gsi_for_stmt (cond_exprs[i]);
		  gsi_insert_before (&gsi2, copy, GSI_SAME_STMT);
		  gimple_assign_set_rhs1 (cond_exprs[i], new_def);
		  update_stmt (cond_exprs[i]);
		}
	    }
	}
    }

  for (auto it = vec_cond_ssa_name_uses.begin ();
       it != vec_cond_ssa_name_uses.end (); ++it)
    bitmap_set_bit (dce_ssa_names, SSA_NAME_VERSION ((*it).first));

  simple_dce_from_worklist (dce_ssa_names);

  return cfg_changed ? TODO_cleanup_cfg : 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_gimple_isel (gcc::context *ctxt)
{
  return new pass_gimple_isel (ctxt);
}

