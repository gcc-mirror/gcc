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

/* Bitmap of variables for which we want immediate uses.  This is set
   by record_single_argument_cond_exprs and tested in need_imm_uses_for.  */
static bitmap vars;

static bool need_imm_uses_for (tree);
static void tree_ssa_forward_propagate_single_use_vars (void);
static void record_single_argument_cond_exprs (varray_type,
					       varray_type *,
					       bitmap);
static void substitute_single_use_vars (varray_type *, varray_type);

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

static void
record_single_argument_cond_exprs (varray_type cond_worklist,
				   varray_type *vars_worklist,
				   bitmap vars)

{
  /* The first pass over the blocks gathers the set of variables we need
     immediate uses for as well as the set of interesting COND_EXPRs.

     A simpler implementation may be appropriate if/when we have a lower
     overhead means of getting immediate use information.  */
  while (VARRAY_ACTIVE_SIZE (cond_worklist) > 0)
    {
      tree last = VARRAY_TOP_TREE (cond_worklist);

      VARRAY_POP (cond_worklist);

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
		  && CONSTANT_CLASS_P (TREE_OPERAND (cond, 1))
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
			  || !CONSTANT_CLASS_P (op1)
			  || !INTEGRAL_TYPE_P (TREE_TYPE (op1)))
			continue;
		      
		      /* Don't propagate if the first operand occurs in
		         an abnormal PHI.  */
		      if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (op0))
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
			    continue;
		      
			  /* Don't propagate if the first operand occurs in
			     an abnormal PHI.  */
			  if (TREE_CODE (op0) == SSA_NAME
			      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (op0))
			    continue;
		      
			  /* Don't propagate if the second operand occurs in
			     an abnormal PHI.  */
			  if (TREE_CODE (op1) == SSA_NAME
			      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (op1))
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
		      
			  /* Don't propagate if the operand occurs in
			     an abnormal PHI.  */
			  if (TREE_CODE (def_rhs) == SSA_NAME
			      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (def_rhs))
			    continue;
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
			  else
			    continue;
		      
			  /* Don't propagate if the operand occurs in
			     an abnormal PHI.  */
			  if (TREE_CODE (TREE_OPERAND (def_rhs, 0)) == SSA_NAME
			      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (TREE_OPERAND
					                          (def_rhs, 0)))
			    continue;
			}
		      else
			continue;
		    }
		  else
		    continue;

		  /* All the tests passed, record TEST_VAR as interesting.  */
		  VARRAY_PUSH_TREE (*vars_worklist, test_var);
		  bitmap_set_bit (vars, SSA_NAME_VERSION (test_var));
		}
	    }
	}
    }
}

/* Given FORWPROP_DATA containing SSA_NAMEs which are used in COND_EXPRs
   that we may be able to optimize, attempt to rewrite the condition
   in each COND_EXPR to use the RHS of the statement which defines the
   SSA_NAME used in the COND_EXPR.  */
  
static void
substitute_single_use_vars (varray_type *cond_worklist,
			    varray_type vars_worklist)
{
  while (VARRAY_ACTIVE_SIZE (vars_worklist) > 0)
    {
      tree test_var = VARRAY_TOP_TREE (vars_worklist);
      tree def = SSA_NAME_DEF_STMT (test_var);
      dataflow_t df;
      int j, num_uses, propagated_uses;

      VARRAY_POP (vars_worklist);

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
	  else if (TREE_CODE_CLASS (def_rhs_code) == tcc_comparison)
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
		  if (!COMPARISON_CLASS_P (new_cond)
		      && TREE_CODE (new_cond) != SSA_NAME)
		    continue;
		}
	    }
	  else
	    {
	      bool invert = false;
	      enum tree_code new_code;
	      tree new_arg;

	      /* TEST_VAR was set from a TRUTH_NOT_EXPR or a NOP_EXPR.  */
	      if (def_rhs_code == TRUTH_NOT_EXPR)
		invert = true;
      
	      /* If we don't have <NE_EXPR/EQ_EXPR x INT_CST>, then we cannot
	         optimize this case.  */
	      if ((cond_code == NE_EXPR || cond_code == EQ_EXPR)
	          && TREE_CODE (TREE_OPERAND (cond, 1)) != INTEGER_CST)
		continue;
		
	      if (cond_code == SSA_NAME
		  || (cond_code == NE_EXPR
		      && integer_zerop (TREE_OPERAND (cond, 1)))
		  || (cond_code == EQ_EXPR
		      && integer_onep (TREE_OPERAND (cond, 1))))
		new_code = NE_EXPR;
	      else
		new_code = EQ_EXPR;

	      if (invert)
		new_code = (new_code == EQ_EXPR ? NE_EXPR  : EQ_EXPR);

	      new_arg = TREE_OPERAND (def_rhs, 0);
	      new_cond = build2 (new_code, boolean_type_node, new_arg,
				 fold_convert (TREE_TYPE (new_arg),
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
	  VARRAY_PUSH_TREE (*cond_worklist, cond_stmt);
	}

      /* If we propagated into all the uses, then we can delete DEF.
	 Unfortunately, we have to find the defining statement in
	 whatever block it might be in.  */
      if (num_uses && num_uses == propagated_uses)
	{
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
  varray_type vars_worklist, cond_worklist;

  vars = BITMAP_ALLOC (NULL);
  VARRAY_TREE_INIT (vars_worklist, 10, "VARS worklist");
  VARRAY_TREE_INIT (cond_worklist, 10, "COND worklist");

  /* Prime the COND_EXPR worklist by placing all the COND_EXPRs on the
     worklist.  */
  FOR_EACH_BB (bb)
    {
      tree last = last_stmt (bb);
      if (last && TREE_CODE (last) == COND_EXPR)
	VARRAY_PUSH_TREE (cond_worklist, last);
    }

  while (VARRAY_ACTIVE_SIZE (cond_worklist) > 0)
    {
      /* First get a list of all the interesting COND_EXPRs and potential
	 single use variables which feed those COND_EXPRs.  This will drain
	 COND_WORKLIST and initialize VARS_WORKLIST.  */
      record_single_argument_cond_exprs (cond_worklist, &vars_worklist, vars);

      if (VARRAY_ACTIVE_SIZE (vars_worklist) > 0)
	{
	  /* Now compute immediate uses for all the variables we care about.  */
	  compute_immediate_uses (TDFA_USE_OPS, need_imm_uses_for);

	  /* We've computed immediate uses, so we can/must clear the VARS
	     bitmap for the next iteration.  */
	  bitmap_clear (vars);

	  /* And optimize.  This will drain VARS_WORKLIST and initialize
	     COND_WORKLIST for the next iteration.  */
	  substitute_single_use_vars (&cond_worklist, vars_worklist);

	  /* We do not incrementally update the dataflow information
	     so we must free it here and recompute the necessary bits
	     on the next iteration.  If this turns out to be expensive,
	     methods for incrementally updating the dataflow are known.  */
	  free_df ();
	}
    }

  /* All done.  Clean up.  */
  BITMAP_FREE (vars);
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
