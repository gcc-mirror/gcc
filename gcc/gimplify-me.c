/* Tree lowering to gimple for middle end use only.  
   This converts the GENERIC functions-as-trees tree representation into
   the GIMPLE form.
   Copyright (C) 2013-2014 Free Software Foundation, Inc.
   Major work done by Sebastian Pop <s.pop@laposte.net>,
   Diego Novillo <dnovillo@redhat.com> and Jason Merrill <jason@redhat.com>.

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
#include "tree.h"
#include "stmt.h"
#include "stor-layout.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "tree-eh.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimplify.h"
#include "gimplify-me.h"
#include "gimple-ssa.h"
#include "stringpool.h"
#include "tree-ssanames.h"


/* Expand EXPR to list of gimple statements STMTS.  GIMPLE_TEST_F specifies
   the predicate that will hold for the result.  If VAR is not NULL, make the
   base variable of the final destination be VAR if suitable.  */

tree
force_gimple_operand_1 (tree expr, gimple_seq *stmts,
			gimple_predicate gimple_test_f, tree var)
{
  enum gimplify_status ret;
  location_t saved_location;

  *stmts = NULL;

  /* gimple_test_f might be more strict than is_gimple_val, make
     sure we pass both.  Just checking gimple_test_f doesn't work
     because most gimple predicates do not work recursively.  */
  if (is_gimple_val (expr)
      && (*gimple_test_f) (expr))
    return expr;

  push_gimplify_context (gimple_in_ssa_p (cfun), true);
  saved_location = input_location;
  input_location = UNKNOWN_LOCATION;

  if (var)
    {
      if (gimple_in_ssa_p (cfun) && is_gimple_reg (var))
	var = make_ssa_name (var, NULL);
      expr = build2 (MODIFY_EXPR, TREE_TYPE (var), var, expr);
    }

  if (TREE_CODE (expr) != MODIFY_EXPR
      && TREE_TYPE (expr) == void_type_node)
    {
      gimplify_and_add (expr, stmts);
      expr = NULL_TREE;
    }
  else
    {
      ret = gimplify_expr (&expr, stmts, NULL, gimple_test_f, fb_rvalue);
      gcc_assert (ret != GS_ERROR);
    }

  input_location = saved_location;
  pop_gimplify_context (NULL);

  return expr;
}

/* Expand EXPR to list of gimple statements STMTS.  If SIMPLE is true,
   force the result to be either ssa_name or an invariant, otherwise
   just force it to be a rhs expression.  If VAR is not NULL, make the
   base variable of the final destination be VAR if suitable.  */

tree
force_gimple_operand (tree expr, gimple_seq *stmts, bool simple, tree var)
{
  return force_gimple_operand_1 (expr, stmts,
				 simple ? is_gimple_val : is_gimple_reg_rhs,
				 var);
}

/* Invoke force_gimple_operand_1 for EXPR with parameters GIMPLE_TEST_F
   and VAR.  If some statements are produced, emits them at GSI.
   If BEFORE is true.  the statements are appended before GSI, otherwise
   they are appended after it.  M specifies the way GSI moves after
   insertion (GSI_SAME_STMT or GSI_CONTINUE_LINKING are the usual values).  */

tree
force_gimple_operand_gsi_1 (gimple_stmt_iterator *gsi, tree expr,
			    gimple_predicate gimple_test_f,
			    tree var, bool before,
			    enum gsi_iterator_update m)
{
  gimple_seq stmts;

  expr = force_gimple_operand_1 (expr, &stmts, gimple_test_f, var);

  if (!gimple_seq_empty_p (stmts))
    {
      if (before)
	gsi_insert_seq_before (gsi, stmts, m);
      else
	gsi_insert_seq_after (gsi, stmts, m);
    }

  return expr;
}

/* Invoke force_gimple_operand_1 for EXPR with parameter VAR.
   If SIMPLE is true, force the result to be either ssa_name or an invariant,
   otherwise just force it to be a rhs expression.  If some statements are
   produced, emits them at GSI.  If BEFORE is true, the statements are
   appended before GSI, otherwise they are appended after it.  M specifies
   the way GSI moves after insertion (GSI_SAME_STMT or GSI_CONTINUE_LINKING
   are the usual values).  */

tree
force_gimple_operand_gsi (gimple_stmt_iterator *gsi, tree expr,
			  bool simple_p, tree var, bool before,
			  enum gsi_iterator_update m)
{
  return force_gimple_operand_gsi_1 (gsi, expr,
				     simple_p
				     ? is_gimple_val : is_gimple_reg_rhs,
				     var, before, m);
}

/* Some transformations like inlining may invalidate the GIMPLE form
   for operands.  This function traverses all the operands in STMT and
   gimplifies anything that is not a valid gimple operand.  Any new
   GIMPLE statements are inserted before *GSI_P.  */

void
gimple_regimplify_operands (gimple stmt, gimple_stmt_iterator *gsi_p)
{
  size_t i, num_ops;
  tree lhs;
  gimple_seq pre = NULL;
  gimple post_stmt = NULL;

  push_gimplify_context (gimple_in_ssa_p (cfun));

  switch (gimple_code (stmt))
    {
    case GIMPLE_COND:
      gimplify_expr (gimple_cond_lhs_ptr (stmt), &pre, NULL,
		     is_gimple_val, fb_rvalue);
      gimplify_expr (gimple_cond_rhs_ptr (stmt), &pre, NULL,
		     is_gimple_val, fb_rvalue);
      break;
    case GIMPLE_SWITCH:
      gimplify_expr (gimple_switch_index_ptr (stmt), &pre, NULL,
		     is_gimple_val, fb_rvalue);
      break;
    case GIMPLE_OMP_ATOMIC_LOAD:
      gimplify_expr (gimple_omp_atomic_load_rhs_ptr (stmt), &pre, NULL,
		     is_gimple_val, fb_rvalue);
      break;
    case GIMPLE_ASM:
      {
	size_t i, noutputs = gimple_asm_noutputs (stmt);
	const char *constraint, **oconstraints;
	bool allows_mem, allows_reg, is_inout;

	oconstraints
	  = (const char **) alloca ((noutputs) * sizeof (const char *));
	for (i = 0; i < noutputs; i++)
	  {
	    tree op = gimple_asm_output_op (stmt, i);
	    constraint = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (op)));
	    oconstraints[i] = constraint;
	    parse_output_constraint (&constraint, i, 0, 0, &allows_mem,
				     &allows_reg, &is_inout);
	    gimplify_expr (&TREE_VALUE (op), &pre, NULL,
			   is_inout ? is_gimple_min_lval : is_gimple_lvalue,
			   fb_lvalue | fb_mayfail);
	  }
	for (i = 0; i < gimple_asm_ninputs (stmt); i++)
	  {
	    tree op = gimple_asm_input_op (stmt, i);
	    constraint = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (op)));
	    parse_input_constraint (&constraint, 0, 0, noutputs, 0,
				    oconstraints, &allows_mem, &allows_reg);
	    if (TREE_ADDRESSABLE (TREE_TYPE (TREE_VALUE (op))) && allows_mem)
	      allows_reg = 0;
	    if (!allows_reg && allows_mem)
	      gimplify_expr (&TREE_VALUE (op), &pre, NULL,
			     is_gimple_lvalue, fb_lvalue | fb_mayfail);
	    else
	      gimplify_expr (&TREE_VALUE (op), &pre, NULL,
			     is_gimple_asm_val, fb_rvalue);
	  }
      }
      break;
    default:
      /* NOTE: We start gimplifying operands from last to first to
	 make sure that side-effects on the RHS of calls, assignments
	 and ASMs are executed before the LHS.  The ordering is not
	 important for other statements.  */
      num_ops = gimple_num_ops (stmt);
      for (i = num_ops; i > 0; i--)
	{
	  tree op = gimple_op (stmt, i - 1);
	  if (op == NULL_TREE)
	    continue;
	  if (i == 1 && (is_gimple_call (stmt) || is_gimple_assign (stmt)))
	    gimplify_expr (&op, &pre, NULL, is_gimple_lvalue, fb_lvalue);
	  else if (i == 2
		   && is_gimple_assign (stmt)
		   && num_ops == 2
		   && get_gimple_rhs_class (gimple_expr_code (stmt))
		      == GIMPLE_SINGLE_RHS)
	    gimplify_expr (&op, &pre, NULL,
			   rhs_predicate_for (gimple_assign_lhs (stmt)),
			   fb_rvalue);
	  else if (i == 2 && is_gimple_call (stmt))
	    {
	      if (TREE_CODE (op) == FUNCTION_DECL)
		continue;
	      gimplify_expr (&op, &pre, NULL, is_gimple_call_addr, fb_rvalue);
	    }
	  else
	    gimplify_expr (&op, &pre, NULL, is_gimple_val, fb_rvalue);
	  gimple_set_op (stmt, i - 1, op);
	}

      lhs = gimple_get_lhs (stmt);
      /* If the LHS changed it in a way that requires a simple RHS,
	 create temporary.  */
      if (lhs && !is_gimple_reg (lhs))
	{
	  bool need_temp = false;

	  if (is_gimple_assign (stmt)
	      && num_ops == 2
	      && get_gimple_rhs_class (gimple_expr_code (stmt))
		 == GIMPLE_SINGLE_RHS)
	    gimplify_expr (gimple_assign_rhs1_ptr (stmt), &pre, NULL,
			   rhs_predicate_for (gimple_assign_lhs (stmt)),
			   fb_rvalue);
	  else if (is_gimple_reg (lhs))
	    {
	      if (is_gimple_reg_type (TREE_TYPE (lhs)))
		{
		  if (is_gimple_call (stmt))
		    {
		      i = gimple_call_flags (stmt);
		      if ((i & ECF_LOOPING_CONST_OR_PURE)
			  || !(i & (ECF_CONST | ECF_PURE)))
			need_temp = true;
		    }
		  if (stmt_can_throw_internal (stmt))
		    need_temp = true;
		}
	    }
	  else
	    {
	      if (is_gimple_reg_type (TREE_TYPE (lhs)))
		need_temp = true;
	      else if (TYPE_MODE (TREE_TYPE (lhs)) != BLKmode)
		{
		  if (is_gimple_call (stmt))
		    {
		      tree fndecl = gimple_call_fndecl (stmt);

		      if (!aggregate_value_p (TREE_TYPE (lhs), fndecl)
			  && !(fndecl && DECL_RESULT (fndecl)
			       && DECL_BY_REFERENCE (DECL_RESULT (fndecl))))
			need_temp = true;
		    }
		  else
		    need_temp = true;
		}
	    }
	  if (need_temp)
	    {
	      tree temp = create_tmp_reg (TREE_TYPE (lhs), NULL);
	      if (gimple_in_ssa_p (cfun))
		temp = make_ssa_name (temp, NULL);
	      gimple_set_lhs (stmt, temp);
	      post_stmt = gimple_build_assign (lhs, temp);
	    }
	}
      break;
    }

  if (!gimple_seq_empty_p (pre))
    gsi_insert_seq_before (gsi_p, pre, GSI_SAME_STMT);
  if (post_stmt)
    gsi_insert_after (gsi_p, post_stmt, GSI_NEW_STMT);

  pop_gimplify_context (NULL);
}


