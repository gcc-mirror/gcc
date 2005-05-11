/* Forward propagation of single use variables.
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "errors.h"
#include "ggc.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "basic-block.h"
#include "timevar.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "tree-dump.h"

/* This pass performs simple forward propagation of single use variables
   from their definition site into their single use site.

   Right now we only bother forward propagating into COND_EXPRs since those
   are relatively common cases where forward propagation creates valid
   gimple code without the expression needing to fold.  i.e.

     bb0:
       x = a COND b;
       if (x) goto ... else goto ...

   Will be transformed into:

     bb0:
       if (a COND b) goto ... else goto ...
 
   Similarly for the tests (x == 0), (x != 0), (x == 1) and (x != 1).

   Or (assuming c1 and c2 are constants):

     bb0:
       x = a + c1;  
       if (x EQ/NEQ c2) goto ... else goto ...

   Will be transformed into:

     bb0:
        if (a EQ/NEQ (c2 - c1)) goto ... else goto ...

   Similarly for x = a - c1.
    
   Or

     bb0:
       x = !a
       if (x) goto ... else goto ...

   Will be transformed into:

     bb0:
        if (a == 0) goto ... else goto ...

   Similarly for the tests (x == 0), (x != 0), (x == 1) and (x != 1).
   For these cases, we propagate A into all, possibly more than one,
   COND_EXPRs that use X.

   Or

     bb0:
       x = (typecast) a
       if (x) goto ... else goto ...

   Will be transformed into:

     bb0:
        if (a != 0) goto ... else goto ...

   (Assuming a is an integral type and x is a boolean or x is an
    integral and a is a boolean.)

   Similarly for the tests (x == 0), (x != 0), (x == 1) and (x != 1).
   For these cases, we propagate A into all, possibly more than one,
   COND_EXPRs that use X.

   In addition to eliminating the variable and the statement which assigns
   a value to the variable, we may be able to later thread the jump without
   adding insane complexity in the dominator optimizer.

   Also note these transformations can cascade.  We handle this by having
   a worklist of COND_EXPR statements to examine.  As we make a change to
   a statement, we put it back on the worklist to examine on the next
   iteration of the main loop.

   This will (of course) be extended as other needs arise.  */

/* Given an SSA_NAME VAR, return true if and only if VAR is defined by
   a comparison.  */

static bool
ssa_name_defined_by_comparison_p (tree var)
{
  tree def = SSA_NAME_DEF_STMT (var);

  if (TREE_CODE (def) == MODIFY_EXPR)
    {
      tree rhs = TREE_OPERAND (def, 1);
      return COMPARISON_CLASS_P (rhs);
    }

  return 0;
}

/* Forward propagate a single-use variable into COND once.  Return a
   new condition if successful.  Return NULL_TREE otherwise.  */

static tree
forward_propagate_into_cond_1 (tree cond, tree *test_var_p)
{
  tree new_cond = NULL_TREE;
  enum tree_code cond_code = TREE_CODE (cond);
  tree test_var = NULL_TREE;
  tree def;
  tree def_rhs;

  /* If the condition is not a lone variable or an equality test of an
     SSA_NAME against an integral constant, then we do not have an
     optimizable case.

     Note these conditions also ensure the COND_EXPR has no
     virtual operands or other side effects.  */
  if (cond_code != SSA_NAME
      && !((cond_code == EQ_EXPR || cond_code == NE_EXPR)
	   && TREE_CODE (TREE_OPERAND (cond, 0)) == SSA_NAME
	   && CONSTANT_CLASS_P (TREE_OPERAND (cond, 1))
	   && INTEGRAL_TYPE_P (TREE_TYPE (TREE_OPERAND (cond, 1)))))
    return NULL_TREE;

  /* Extract the single variable used in the test into TEST_VAR.  */
  if (cond_code == SSA_NAME)
    test_var = cond;
  else
    test_var = TREE_OPERAND (cond, 0);

  /* Now get the defining statement for TEST_VAR.  Skip this case if
     it's not defined by some MODIFY_EXPR.  */
  def = SSA_NAME_DEF_STMT (test_var);
  if (TREE_CODE (def) != MODIFY_EXPR)
    return NULL_TREE;

  def_rhs = TREE_OPERAND (def, 1);

  /* If TEST_VAR is set by adding or subtracting a constant
     from an SSA_NAME, then it is interesting to us as we
     can adjust the constant in the conditional and thus
     eliminate the arithmetic operation.  */
  if (TREE_CODE (def_rhs) == PLUS_EXPR
      || TREE_CODE (def_rhs) == MINUS_EXPR)
    {
      tree op0 = TREE_OPERAND (def_rhs, 0);
      tree op1 = TREE_OPERAND (def_rhs, 1);

      /* The first operand must be an SSA_NAME and the second
	 operand must be a constant.  */
      if (TREE_CODE (op0) != SSA_NAME
	  || !CONSTANT_CLASS_P (op1)
	  || !INTEGRAL_TYPE_P (TREE_TYPE (op1)))
	return NULL_TREE;

      /* Don't propagate if the first operand occurs in
	 an abnormal PHI.  */
      if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (op0))
	return NULL_TREE;

      if (has_single_use (test_var))
	{
	  enum tree_code new_code;
	  tree t;

	  /* If the variable was defined via X + C, then we must
	     subtract C from the constant in the conditional.
	     Otherwise we add C to the constant in the
	     conditional.  The result must fold into a valid
	     gimple operand to be optimizable.  */
	  new_code = (TREE_CODE (def_rhs) == PLUS_EXPR
		      ? MINUS_EXPR : PLUS_EXPR);
	  t = int_const_binop (new_code, TREE_OPERAND (cond, 1), op1, 0);
	  if (!is_gimple_val (t))
	    return NULL_TREE;

	  new_cond = build (cond_code, boolean_type_node, op0, t);
	}
    }

  /* These cases require comparisons of a naked SSA_NAME or
     comparison of an SSA_NAME against zero or one.  */
  else if (TREE_CODE (cond) == SSA_NAME
	   || integer_zerop (TREE_OPERAND (cond, 1))
	   || integer_onep (TREE_OPERAND (cond, 1)))
    {
      /* If TEST_VAR is set from a relational operation
	 between two SSA_NAMEs or a combination of an SSA_NAME
	 and a constant, then it is interesting.  */
      if (COMPARISON_CLASS_P (def_rhs))
	{
	  tree op0 = TREE_OPERAND (def_rhs, 0);
	  tree op1 = TREE_OPERAND (def_rhs, 1);

	  /* Both operands of DEF_RHS must be SSA_NAMEs or
	     constants.  */
	  if ((TREE_CODE (op0) != SSA_NAME
	       && !is_gimple_min_invariant (op0))
	      || (TREE_CODE (op1) != SSA_NAME
		  && !is_gimple_min_invariant (op1)))
	    return NULL_TREE;

	  /* Don't propagate if the first operand occurs in
	     an abnormal PHI.  */
	  if (TREE_CODE (op0) == SSA_NAME
	      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (op0))
	    return NULL_TREE;

	  /* Don't propagate if the second operand occurs in
	     an abnormal PHI.  */
	  if (TREE_CODE (op1) == SSA_NAME
	      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (op1))
	    return NULL_TREE;

	  if (has_single_use (test_var))
	    {
	      /* TEST_VAR was set from a relational operator.  */
	      new_cond = build (TREE_CODE (def_rhs),
				boolean_type_node, op0, op1);

	      /* Invert the conditional if necessary.  */
	      if ((cond_code == EQ_EXPR
		   && integer_zerop (TREE_OPERAND (cond, 1)))
		  || (cond_code == NE_EXPR
		      && integer_onep (TREE_OPERAND (cond, 1))))
		{
		  new_cond = invert_truthvalue (new_cond);

		  /* If we did not get a simple relational
		     expression or bare SSA_NAME, then we can
		     not optimize this case.  */
		  if (!COMPARISON_CLASS_P (new_cond)
		      && TREE_CODE (new_cond) != SSA_NAME)
		    new_cond = NULL_TREE;
		}
	    }
	}

      /* If TEST_VAR is set from a TRUTH_NOT_EXPR, then it
	 is interesting.  */
      else if (TREE_CODE (def_rhs) == TRUTH_NOT_EXPR)
	{
	  enum tree_code new_code;

	  def_rhs = TREE_OPERAND (def_rhs, 0);

	  /* DEF_RHS must be an SSA_NAME or constant.  */
	  if (TREE_CODE (def_rhs) != SSA_NAME
	      && !is_gimple_min_invariant (def_rhs))
	    return NULL_TREE;

	  /* Don't propagate if the operand occurs in
	     an abnormal PHI.  */
	  if (TREE_CODE (def_rhs) == SSA_NAME
	      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (def_rhs))
	    return NULL_TREE;

	  if (cond_code == SSA_NAME
	      || (cond_code == NE_EXPR
		  && integer_zerop (TREE_OPERAND (cond, 1)))
	      || (cond_code == EQ_EXPR
		  && integer_onep (TREE_OPERAND (cond, 1))))
	    new_code = EQ_EXPR;
	  else
	    new_code = NE_EXPR;

	  new_cond = build2 (new_code, boolean_type_node, def_rhs,
			     fold_convert (TREE_TYPE (def_rhs),
					   integer_zero_node));
	}

      /* If TEST_VAR was set from a cast of an integer type
	 to a boolean type or a cast of a boolean to an
	 integral, then it is interesting.  */
      else if (TREE_CODE (def_rhs) == NOP_EXPR
	       || TREE_CODE (def_rhs) == CONVERT_EXPR)
	{
	  tree outer_type;
	  tree inner_type;

	  outer_type = TREE_TYPE (def_rhs);
	  inner_type = TREE_TYPE (TREE_OPERAND (def_rhs, 0));

	  if ((TREE_CODE (outer_type) == BOOLEAN_TYPE
	       && INTEGRAL_TYPE_P (inner_type))
	      || (TREE_CODE (inner_type) == BOOLEAN_TYPE
		  && INTEGRAL_TYPE_P (outer_type)))
	    ;
	  else if (INTEGRAL_TYPE_P (outer_type)
		   && INTEGRAL_TYPE_P (inner_type)
		   && TREE_CODE (TREE_OPERAND (def_rhs, 0)) == SSA_NAME
		   && ssa_name_defined_by_comparison_p (TREE_OPERAND (def_rhs,
								      0)))
	    ;
	  else
	    return NULL_TREE;

	  /* Don't propagate if the operand occurs in
	     an abnormal PHI.  */
	  if (TREE_CODE (TREE_OPERAND (def_rhs, 0)) == SSA_NAME
	      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (TREE_OPERAND
						  (def_rhs, 0)))
	    return NULL_TREE;

	  if (has_single_use (test_var))
	    {
	      enum tree_code new_code;
	      tree new_arg;

	      if (cond_code == SSA_NAME
		  || (cond_code == NE_EXPR
		      && integer_zerop (TREE_OPERAND (cond, 1)))
		  || (cond_code == EQ_EXPR
		      && integer_onep (TREE_OPERAND (cond, 1))))
		new_code = NE_EXPR;
	      else
		new_code = EQ_EXPR;

	      new_arg = TREE_OPERAND (def_rhs, 0);
	      new_cond = build2 (new_code, boolean_type_node, new_arg,
				 fold_convert (TREE_TYPE (new_arg),
					       integer_zero_node));
	    }
	}
    }

  *test_var_p = test_var;
  return new_cond;
}

/* Forward propagate a single-use variable into COND_EXPR as many
   times as possible.  */

static void
forward_propagate_into_cond (tree cond_expr)
{
  gcc_assert (TREE_CODE (cond_expr) == COND_EXPR);

  while (1)
    {
      tree test_var = NULL_TREE;
      tree cond = COND_EXPR_COND (cond_expr);
      tree new_cond = forward_propagate_into_cond_1 (cond, &test_var);

      /* Return if unsuccessful.  */
      if (new_cond == NULL_TREE)
	break;

      /* Dump details.  */
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "  Replaced '");
	  print_generic_expr (dump_file, cond, dump_flags);
	  fprintf (dump_file, "' with '");
	  print_generic_expr (dump_file, new_cond, dump_flags);
	  fprintf (dump_file, "'\n");
	}

      COND_EXPR_COND (cond_expr) = new_cond;
      update_stmt (cond_expr);

      if (has_zero_uses (test_var))
	{
	  tree def = SSA_NAME_DEF_STMT (test_var);
	  block_stmt_iterator bsi = bsi_for_stmt (def);
	  bsi_remove (&bsi);
	}
    }
}

/* Main entry point for the forward propagation optimizer.  */

static void
tree_ssa_forward_propagate_single_use_vars (void)
{
  basic_block bb;

  FOR_EACH_BB (bb)
    {
      tree last = last_stmt (bb);
      if (last && TREE_CODE (last) == COND_EXPR)
	forward_propagate_into_cond (last);
    }
}


static bool
gate_forwprop (void)
{
  return 1;
}

struct tree_opt_pass pass_forwprop = {
  "forwprop",			/* name */
  gate_forwprop,		/* gate */
  tree_ssa_forward_propagate_single_use_vars,	/* execute */
  NULL,				/* sub */
  NULL,				/* next */
  0,				/* static_pass_number */
  TV_TREE_FORWPROP,		/* tv_id */
  PROP_cfg | PROP_ssa
    | PROP_alias,		/* properties_required */
  0,				/* properties_provided */
  0,				/* properties_destroyed */
  0,				/* todo_flags_start */
  TODO_dump_func | TODO_ggc_collect	/* todo_flags_finish */
  | TODO_verify_ssa,
  0					/* letter */
};
