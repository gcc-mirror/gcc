/* -Wnonnull-compare warning support.
   Copyright (C) 2016-2025 Free Software Foundation, Inc.
   Contributed by Jakub Jelinek <jakub@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "diagnostic-core.h"
#include "tree-dfa.h"

/* Warn about comparison of nonnull_arg_p argument initial values
   with NULL.  */

static void
do_warn_nonnull_compare (function *fun, tree arg)
{
  if (!POINTER_TYPE_P (TREE_TYPE (arg))
      && TREE_CODE (TREE_TYPE (arg)) != OFFSET_TYPE)
    return;

  if (!nonnull_arg_p (arg))
    return;

  tree d = ssa_default_def (fun, arg);
  if (d == NULL_TREE)
    return;

  use_operand_p use_p;
  imm_use_iterator iter;

  FOR_EACH_IMM_USE_FAST (use_p, iter, d)
    {
      gimple *stmt = USE_STMT (use_p);
      tree op = NULL_TREE;
      location_t loc = gimple_location (stmt);
      if (gimple_code (stmt) == GIMPLE_COND)
	switch (gimple_cond_code (stmt))
	  {
	  case EQ_EXPR:
	  case NE_EXPR:
	    if (gimple_cond_lhs (stmt) == d)
	      op = gimple_cond_rhs (stmt);
	    break;
	  default:
	    break;
	  }
      else if (is_gimple_assign (stmt))
	switch (gimple_assign_rhs_code (stmt))
	  {
	  case EQ_EXPR:
	  case NE_EXPR:
	    if (gimple_assign_rhs1 (stmt) == d)
	      op = gimple_assign_rhs2 (stmt);
	    break;
	  case COND_EXPR:
	    switch (TREE_CODE (gimple_assign_rhs1 (stmt)))
	      {
	      case EQ_EXPR:
	      case NE_EXPR:
		op = gimple_assign_rhs1 (stmt);
		if (TREE_OPERAND (op, 0) != d)
		  {
		    op = NULL_TREE;
		    break;
		  }
		loc = EXPR_LOC_OR_LOC (op, loc);
		op = TREE_OPERAND (op, 1);
		break;
	      default:
		break;
	      }
	    break;
	  default:
	    break;
	  }
      if (op
	  && (POINTER_TYPE_P (TREE_TYPE (arg))
	      ? integer_zerop (op) : integer_minus_onep (op))
	  && !warning_suppressed_p (stmt, OPT_Wnonnull_compare))
	warning_at (loc, OPT_Wnonnull_compare,
		    "%<nonnull%> argument %qD compared to NULL", arg);
    }
}

namespace {

const pass_data pass_data_warn_nonnull_compare =
{
  GIMPLE_PASS, /* type */
  "*nonnullcmp", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_warn_nonnull_compare : public gimple_opt_pass
{
public:
  pass_warn_nonnull_compare (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_warn_nonnull_compare, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override { return warn_nonnull_compare; }

  unsigned int execute (function *) final override;

}; // class pass_warn_nonnull_compare

unsigned int
pass_warn_nonnull_compare::execute (function *fun)
{
  if (fun->static_chain_decl)
    do_warn_nonnull_compare (fun, fun->static_chain_decl);

  for (tree arg = DECL_ARGUMENTS (cfun->decl); arg; arg = DECL_CHAIN (arg))
    do_warn_nonnull_compare (fun, arg);
  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_warn_nonnull_compare (gcc::context *ctxt)
{
  return new pass_warn_nonnull_compare (ctxt);
}
