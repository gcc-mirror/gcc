/* Forward propagation of single use variables.
   Copyright (C) 2004 Free Software Foundation, Inc.

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
   gimple code without the expression needing to fold.  ie

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

   In addition to eliminating the variable and the statement which assigns
   a value to the variable, we may be able to later thread the jump without
   adding insane complexity in the dominator optimizer. 

   This will (of course) be extended as other needs arise.  */

/* Bitmap of variables for which we want immediate uses.  This is set
   by record_single_argument_cond_exprs and tested in need_imm_uses_for.  */
static bitmap vars;

static bool need_imm_uses_for (tree);
static void tree_ssa_forward_propagate_single_use_vars (void);
static varray_type record_single_argument_cond_exprs (void);
static void substitute_single_use_vars (varray_type);

/* Function indicating whether we ought to include information for 'var'
   when calculating immediate uses.  */

static bool
need_imm_uses_for (tree var)
{
  return bitmap_bit_p (vars, SSA_NAME_VERSION (var));
}

/* Find all COND_EXPRs with a condition that is a naked SSA_NAME or
   an equality comparison against a constant.

   Record the identified COND_EXPRs and the SSA_NAME used in the COND_EXPR
   into a virtual array, which is returned to the caller.  Also record
   into VARS that we will need immediate uses for the identified SSA_NAME.

   The more uninteresting COND_EXPRs and associated SSA_NAMEs we can
   filter out here, the faster this pass will run since its runtime is
   dominated by the time to build immediate uses.  */

static varray_type
record_single_argument_cond_exprs (void)
{
  basic_block bb;
  varray_type retval;

  vars = BITMAP_XMALLOC ();

  VARRAY_TREE_INIT (retval, 10, "forward propagation vars");

  /* The first pass over the blocks gathers the set of variables we need
     immediate uses for as well as the set of interesting COND_EXPRs.

     A simpler implementation may be appropriate if/when we have a lower
     overhead means of getting immediate use information.  */
  FOR_EACH_BB (bb)
    {
      tree last = last_stmt (bb);

      /* See if this block ends in a COND_EXPR.  */
      if (last && TREE_CODE (last) == COND_EXPR)
	{
	  tree cond = COND_EXPR_COND (last);
	  enum tree_code cond_code = TREE_CODE (cond);

	  /* If the condition is a lone variable or an equality test of
	     an SSA_NAME against an integral constant, then we may have an 
	     optimizable case.

	     Note these conditions also ensure the COND_EXPR has no
	     virtual operands or other side effects.  */
	  if (cond_code == SSA_NAME
	      || ((cond_code == EQ_EXPR || cond_code == NE_EXPR)
		  && TREE_CODE (TREE_OPERAND (cond, 0)) == SSA_NAME
		  && TREE_CODE_CLASS (TREE_CODE (TREE_OPERAND (cond, 1))) == 'c'
		  && INTEGRAL_TYPE_P (TREE_TYPE (TREE_OPERAND (cond, 1)))))
	    {
	      tree def;
	      tree test_var;

	      /* Extract the single variable used in the test into TEST_VAR.  */
	      if (cond_code == SSA_NAME)
		test_var = cond;
	      else
		test_var = TREE_OPERAND (cond, 0);

	      /* If we have already recorded this SSA_NAME as interesting,
		 do not do so again.  */
	      if (bitmap_bit_p (vars, SSA_NAME_VERSION (test_var)))
		continue;

	      /* Now get the defining statement for TEST_VAR and see if it
		 something we are interested in.  */
	      def = SSA_NAME_DEF_STMT (test_var);
	      if (TREE_CODE (def) == MODIFY_EXPR)
		{
		  tree def_rhs = TREE_OPERAND (def, 1);

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
			  || TREE_CODE_CLASS (TREE_CODE (op1)) != 'c'
			  || !INTEGRAL_TYPE_P (TREE_TYPE (op1)))
			continue;
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
		      if (TREE_CODE_CLASS (TREE_CODE (def_rhs)) == '<')
			{
			  tree op0 = TREE_OPERAND (def_rhs, 0);
			  tree op1 = TREE_OPERAND (def_rhs, 1);

			  /* Both operands of DEF_RHS must be SSA_NAMEs or
			     constants.  */
			  if ((TREE_CODE (op0) != SSA_NAME
			       && !is_gimple_min_invariant (op0))
			      || (TREE_CODE (op1) != SSA_NAME
				  && !is_gimple_min_invariant (op1)))
			    continue;
		        }

		      /* If TEST_VAR is set from a TRUTH_NOT_EXPR, then it
			 is interesting.  */
		      else if (TREE_CODE (def_rhs) == TRUTH_NOT_EXPR)
			{
			  def_rhs = TREE_OPERAND (def_rhs, 0);

			  /* DEF_RHS must be an SSA_NAME or constant.  */
			  if (TREE_CODE (def_rhs) != SSA_NAME
			      && !is_gimple_min_invariant (def_rhs))
			    continue;
			}
		      else
			continue;
		    }
		  else
		    continue;

		  /* All the tests passed, record TEST_VAR as interesting.  */
		  VARRAY_PUSH_TREE (retval, test_var);
		  bitmap_set_bit (vars, SSA_NAME_VERSION (test_var));
		}
	    }
	}
    }
  return retval;
}

/* Given FORWPROP_DATA containing SSA_NAMEs which are used in COND_EXPRs
   that we may be able to optimize, attempt to rewrite the condition
   in each COND_EXPR to use the RHS of the statement which defines the
   SSA_NAME used in the COND_EXPR.  */
  
static void
substitute_single_use_vars (varray_type forwprop_data)
{
  unsigned int i;

  for (i = 0; i < VARRAY_ACTIVE_SIZE (forwprop_data); i++)
    {
      tree test_var = VARRAY_TREE (forwprop_data, i);
      tree def = SSA_NAME_DEF_STMT (test_var);
      dataflow_t df;
      int j, num_uses, propagated_uses;
      block_stmt_iterator bsi;

      /* Now compute the immediate uses of TEST_VAR.  */
      df = get_immediate_uses (def);
      num_uses = num_immediate_uses (df);
      propagated_uses = 0;

      /* If TEST_VAR is used more than once and is not a boolean set
	 via TRUTH_NOT_EXPR with another SSA_NAME as its argument, then
	 we can not optimize.  */
      if (num_uses == 1
	  || (TREE_CODE (TREE_TYPE (test_var)) == BOOLEAN_TYPE
	      && TREE_CODE (TREE_OPERAND (def, 1)) == TRUTH_NOT_EXPR
	      && (TREE_CODE (TREE_OPERAND (TREE_OPERAND (def, 1), 0))
		  == SSA_NAME)))
	;
      else
	continue;

      /* Walk over each use and try to forward propagate the RHS of
	 DEF into the use.  */
      for (j = 0; j < num_uses; j++)
	{
	  tree cond_stmt;
	  tree cond;
	  enum tree_code cond_code;
	  tree def_rhs;
	  enum tree_code def_rhs_code;
	  tree new_cond;

	  cond_stmt = immediate_use (df, j);

	  /* For now we can only propagate into COND_EXPRs.  */
	  if (TREE_CODE (cond_stmt) != COND_EXPR) 
	    continue;

	  cond = COND_EXPR_COND (cond_stmt);
	  cond_code = TREE_CODE (cond);
	  def_rhs = TREE_OPERAND (def, 1);
	  def_rhs_code = TREE_CODE (def_rhs);

	  /* If the definition of the single use variable was from an
	     arithmetic operation, then we just need to adjust the
	     constant in the COND_EXPR_COND and update the variable tested.  */
	  if (def_rhs_code == PLUS_EXPR || def_rhs_code == MINUS_EXPR)
	    {
	      tree op0 = TREE_OPERAND (def_rhs, 0);
	      tree op1 = TREE_OPERAND (def_rhs, 1);
	      enum tree_code new_code;
	      tree t;

	      /* If the variable was defined via X + C, then we must subtract
		 C from the constant in the conditional.  Otherwise we add
		 C to the constant in the conditional.  The result must fold
		 into a valid gimple operand to be optimizable.  */
	      new_code = def_rhs_code == PLUS_EXPR ? MINUS_EXPR : PLUS_EXPR;
	      t = int_const_binop (new_code, TREE_OPERAND (cond, 1), op1, 0);
	      if (!is_gimple_val (t))
		continue;

	      new_cond = build (cond_code, boolean_type_node, op0, t);
	    }
	  /* If the variable is defined by a conditional expression... */
	  else if (TREE_CODE_CLASS (def_rhs_code) == '<')
	    {
	      /* TEST_VAR was set from a relational operator.  */
	      tree op0 = TREE_OPERAND (def_rhs, 0);
	      tree op1 = TREE_OPERAND (def_rhs, 1);

	      new_cond = build (def_rhs_code, boolean_type_node, op0, op1);

	      /* Invert the conditional if necessary.  */
	      if ((cond_code == EQ_EXPR
		   && integer_zerop (TREE_OPERAND (cond, 1)))
		  || (cond_code == NE_EXPR
		      && integer_onep (TREE_OPERAND (cond, 1))))
		{
		  new_cond = invert_truthvalue (new_cond);

		  /* If we did not get a simple relational expression or
		     bare SSA_NAME, then we can not optimize this case.  */
		  if (TREE_CODE_CLASS (TREE_CODE (new_cond)) != '<'
		      && TREE_CODE (new_cond) != SSA_NAME)
		    continue;
		}
	    }
	  else
	    {
	      /* TEST_VAR was set from a TRUTH_NOT_EXPR.  */
	      if (cond_code == SSA_NAME
		  || (cond_code == NE_EXPR
		      && integer_zerop (TREE_OPERAND (cond, 1)))
		  || (cond_code == EQ_EXPR
		      && integer_onep (TREE_OPERAND (cond, 1))))
		new_cond = build (EQ_EXPR, boolean_type_node,
				  TREE_OPERAND (def_rhs, 0),
				  convert (TREE_TYPE (def_rhs),
					   integer_zero_node));
	      else
		new_cond = build (NE_EXPR, boolean_type_node,
				  TREE_OPERAND (def_rhs, 0),
				  convert (TREE_TYPE (def_rhs),
					   integer_zero_node));
	    }

	  /* Dump details.  */
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "  Replaced '");
	      print_generic_expr (dump_file, cond, dump_flags);
	      fprintf (dump_file, "' with '");
	      print_generic_expr (dump_file, new_cond, dump_flags);
	      fprintf (dump_file, "'\n");
	    }

	  /* Replace the condition.  */
	  COND_EXPR_COND (cond_stmt) = new_cond;
	  modify_stmt (cond_stmt);
	  propagated_uses++;
	}

      /* If we propagated into all the uses, then we can delete DEF.
	 Unfortunately, we have to find the defining statement in
	 whatever block it might be in.  */
      if (num_uses && num_uses == propagated_uses)
	for (bsi = bsi_start (bb_for_stmt (def));
	     !bsi_end_p (bsi);
	     bsi_next (&bsi))
	  {
	    if (def == bsi_stmt (bsi))
	      {
		bsi_remove (&bsi);
		break;
	      }
	  }
    }
}

/* Main entry point for the forward propagation optimizer.  */

static void
tree_ssa_forward_propagate_single_use_vars (void)
{
  varray_type forwprop_data;

  /* First get a list of all the interesting COND_EXPRs and potential single
     use variables which feed those COND_EXPRs.  */
  forwprop_data = record_single_argument_cond_exprs ();

  /* Now compute immediate uses for all the variables we care about.  */
  compute_immediate_uses (TDFA_USE_OPS, need_imm_uses_for);

  /* We are done with the VARS bitmap and other dataflow information.  */
  BITMAP_XFREE (vars);
  vars = NULL;

  /* And optimize.  */
  substitute_single_use_vars (forwprop_data);

  /* All done.  Clean up.  */
  free_df ();
  VARRAY_CLEAR (forwprop_data);
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
  PROP_cfg | PROP_ssa,		/* properties_required */
  0,				/* properties_provided */
  0,				/* properties_destroyed */
  0,				/* todo_flags_start */
  TODO_dump_func | TODO_ggc_collect	/* todo_flags_finish */
  | TODO_verify_ssa
};
