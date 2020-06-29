/* Schedule GIMPLE vector statements.
   Copyright (C) 2020 Free Software Foundation, Inc.

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

/* Expand all VEC_COND_EXPR gimple assignments into calls to internal
   function based on type of selected expansion.  */

static gimple *
gimple_expand_vec_cond_expr (gimple_stmt_iterator *gsi,
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

	  tree op0a_type = TREE_TYPE (op0a);
	  if (used_vec_cond_exprs >= 2
	      && (get_vcond_mask_icode (mode, TYPE_MODE (op0a_type))
		  != CODE_FOR_nothing)
	      && expand_vec_cmp_expr_p (op0a_type, TREE_TYPE (lhs), tcode))
	    {
	      /* Keep the SSA name and use vcond_mask.  */
	      tcode = TREE_CODE (op0);
	    }
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


  gcc_assert (known_eq (GET_MODE_SIZE (mode), GET_MODE_SIZE (cmp_op_mode))
	      && known_eq (GET_MODE_NUNITS (mode),
			   GET_MODE_NUNITS (cmp_op_mode)));

  icode = get_vcond_icode (mode, cmp_op_mode, unsignedp);
  if (icode == CODE_FOR_nothing)
    {
      if (tcode == LT_EXPR
	  && op0a == op0
	  && TREE_CODE (op0) == VECTOR_CST)
	{
	  /* A VEC_COND_EXPR condition could be folded from EQ_EXPR/NE_EXPR
	     into a constant when only get_vcond_eq_icode is supported.
	     Verify < 0 and != 0 behave the same and change it to NE_EXPR.  */
	  unsigned HOST_WIDE_INT nelts;
	  if (!VECTOR_CST_NELTS (op0).is_constant (&nelts))
	    {
	      if (VECTOR_CST_STEPPED_P (op0))
		gcc_unreachable ();
	      nelts = vector_cst_encoded_nelts (op0);
	    }
	  for (unsigned int i = 0; i < nelts; ++i)
	    if (tree_int_cst_sgn (vector_cst_elt (op0, i)) == 1)
	      gcc_unreachable ();
	  tcode = NE_EXPR;
	}
      if (tcode == EQ_EXPR || tcode == NE_EXPR)
	{
	  tree tcode_tree = build_int_cst (integer_type_node, tcode);
	  return gimple_build_call_internal (IFN_VCONDEQ, 5, op0a, op0b, op1,
					     op2, tcode_tree);
	}
    }

  gcc_assert (icode != CODE_FOR_nothing);
  tree tcode_tree = build_int_cst (integer_type_node, tcode);
  return gimple_build_call_internal (unsignedp ? IFN_VCONDU : IFN_VCOND,
				     5, op0a, op0b, op1, op2, tcode_tree);
}



/* Iterate all gimple statements and try to expand
   VEC_COND_EXPR assignments.  */

static unsigned int
gimple_expand_vec_cond_exprs (void)
{
  gimple_stmt_iterator gsi;
  basic_block bb;
  hash_map<tree, unsigned int> vec_cond_ssa_name_uses;
  auto_bitmap dce_ssa_names;

  FOR_EACH_BB_FN (bb, cfun)
    {
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *g = gimple_expand_vec_cond_expr (&gsi,
						   &vec_cond_ssa_name_uses);
	  if (g != NULL)
	    {
	      tree lhs = gimple_assign_lhs (gsi_stmt (gsi));
	      gimple_set_lhs (g, lhs);
	      gsi_replace (&gsi, g, false);
	    }
	}
    }

  for (hash_map<tree, unsigned int>::iterator it = vec_cond_ssa_name_uses.begin ();
       it != vec_cond_ssa_name_uses.end (); ++it)
    bitmap_set_bit (dce_ssa_names, SSA_NAME_VERSION ((*it).first));

  simple_dce_from_worklist (dce_ssa_names);

  return 0;
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
  virtual bool gate (function *)
    {
      return true;
    }

  virtual unsigned int execute (function *)
    {
      return gimple_expand_vec_cond_exprs ();
    }

}; // class pass_gimple_isel

} // anon namespace

gimple_opt_pass *
make_pass_gimple_isel (gcc::context *ctxt)
{
  return new pass_gimple_isel (ctxt);
}

