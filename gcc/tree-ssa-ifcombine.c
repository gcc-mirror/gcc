/* Combining of if-expressions on trees.
   Copyright (C) 2007, 2008, 2009, 2010, 2011 Free Software Foundation, Inc.
   Contributed by Richard Guenther <rguenther@suse.de>

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
#include "tm.h"
#include "tree.h"
#include "basic-block.h"
#include "timevar.h"
#include "tree-pretty-print.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "tree-dump.h"

/* This pass combines COND_EXPRs to simplify control flow.  It
   currently recognizes bit tests and comparisons in chains that
   represent logical and or logical or of two COND_EXPRs.

   It does so by walking basic blocks in a approximate reverse
   post-dominator order and trying to match CFG patterns that
   represent logical and or logical or of two COND_EXPRs.
   Transformations are done if the COND_EXPR conditions match
   either

     1. two single bit tests X & (1 << Yn) (for logical and)

     2. two bit tests X & Yn (for logical or)

     3. two comparisons X OPn Y (for logical or)

   To simplify this pass, removing basic blocks and dead code
   is left to CFG cleanup and DCE.  */


/* Recognize a if-then-else CFG pattern starting to match with the
   COND_BB basic-block containing the COND_EXPR.  The recognized
   then end else blocks are stored to *THEN_BB and *ELSE_BB.  If
   *THEN_BB and/or *ELSE_BB are already set, they are required to
   match the then and else basic-blocks to make the pattern match.
   Returns true if the pattern matched, false otherwise.  */

static bool
recognize_if_then_else (basic_block cond_bb,
			basic_block *then_bb, basic_block *else_bb)
{
  edge t, e;

  if (EDGE_COUNT (cond_bb->succs) != 2)
    return false;

  /* Find the then/else edges.  */
  t = EDGE_SUCC (cond_bb, 0);
  e = EDGE_SUCC (cond_bb, 1);
  if (!(t->flags & EDGE_TRUE_VALUE))
    {
      edge tmp = t;
      t = e;
      e = tmp;
    }
  if (!(t->flags & EDGE_TRUE_VALUE)
      || !(e->flags & EDGE_FALSE_VALUE))
    return false;

  /* Check if the edge destinations point to the required block.  */
  if (*then_bb
      && t->dest != *then_bb)
    return false;
  if (*else_bb
      && e->dest != *else_bb)
    return false;

  if (!*then_bb)
    *then_bb = t->dest;
  if (!*else_bb)
    *else_bb = e->dest;

  return true;
}

/* Verify if the basic block BB does not have side-effects.  Return
   true in this case, else false.  */

static bool
bb_no_side_effects_p (basic_block bb)
{
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple stmt = gsi_stmt (gsi);

      if (gimple_has_side_effects (stmt)
	  || gimple_vuse (stmt))
	return false;
    }

  return true;
}

/* Verify if all PHI node arguments in DEST for edges from BB1 or
   BB2 to DEST are the same.  This makes the CFG merge point
   free from side-effects.  Return true in this case, else false.  */

static bool
same_phi_args_p (basic_block bb1, basic_block bb2, basic_block dest)
{
  edge e1 = find_edge (bb1, dest);
  edge e2 = find_edge (bb2, dest);
  gimple_stmt_iterator gsi;
  gimple phi;

  for (gsi = gsi_start_phis (dest); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      phi = gsi_stmt (gsi);
      if (!operand_equal_p (PHI_ARG_DEF_FROM_EDGE (phi, e1),
			    PHI_ARG_DEF_FROM_EDGE (phi, e2), 0))
        return false;
    }

  return true;
}

/* Return the best representative SSA name for CANDIDATE which is used
   in a bit test.  */

static tree
get_name_for_bit_test (tree candidate)
{
  /* Skip single-use names in favor of using the name from a
     non-widening conversion definition.  */
  if (TREE_CODE (candidate) == SSA_NAME
      && has_single_use (candidate))
    {
      gimple def_stmt = SSA_NAME_DEF_STMT (candidate);
      if (is_gimple_assign (def_stmt)
	  && CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (def_stmt)))
	{
	  if (TYPE_PRECISION (TREE_TYPE (candidate))
	      <= TYPE_PRECISION (TREE_TYPE (gimple_assign_rhs1 (def_stmt))))
	    return gimple_assign_rhs1 (def_stmt);
	}
    }

  return candidate;
}

/* Recognize a single bit test pattern in GIMPLE_COND and its defining
   statements.  Store the name being tested in *NAME and the bit
   in *BIT.  The GIMPLE_COND computes *NAME & (1 << *BIT).
   Returns true if the pattern matched, false otherwise.  */

static bool
recognize_single_bit_test (gimple cond, tree *name, tree *bit)
{
  gimple stmt;

  /* Get at the definition of the result of the bit test.  */
  if (gimple_cond_code (cond) != NE_EXPR
      || TREE_CODE (gimple_cond_lhs (cond)) != SSA_NAME
      || !integer_zerop (gimple_cond_rhs (cond)))
    return false;
  stmt = SSA_NAME_DEF_STMT (gimple_cond_lhs (cond));
  if (!is_gimple_assign (stmt))
    return false;

  /* Look at which bit is tested.  One form to recognize is
     D.1985_5 = state_3(D) >> control1_4(D);
     D.1986_6 = (int) D.1985_5;
     D.1987_7 = op0 & 1;
     if (D.1987_7 != 0)  */
  if (gimple_assign_rhs_code (stmt) == BIT_AND_EXPR
      && integer_onep (gimple_assign_rhs2 (stmt))
      && TREE_CODE (gimple_assign_rhs1 (stmt)) == SSA_NAME)
    {
      tree orig_name = gimple_assign_rhs1 (stmt);

      /* Look through copies and conversions to eventually
	 find the stmt that computes the shift.  */
      stmt = SSA_NAME_DEF_STMT (orig_name);

      while (is_gimple_assign (stmt)
	     && ((CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (stmt))
		  && (TYPE_PRECISION (TREE_TYPE (gimple_assign_lhs (stmt)))
		      <= TYPE_PRECISION (TREE_TYPE (gimple_assign_rhs1 (stmt)))))
		 || gimple_assign_ssa_name_copy_p (stmt)))
	stmt = SSA_NAME_DEF_STMT (gimple_assign_rhs1 (stmt));

      /* If we found such, decompose it.  */
      if (is_gimple_assign (stmt)
	  && gimple_assign_rhs_code (stmt) == RSHIFT_EXPR)
	{
	  /* op0 & (1 << op1) */
	  *bit = gimple_assign_rhs2 (stmt);
	  *name = gimple_assign_rhs1 (stmt);
	}
      else
	{
	  /* t & 1 */
	  *bit = integer_zero_node;
	  *name = get_name_for_bit_test (orig_name);
	}

      return true;
    }

  /* Another form is
     D.1987_7 = op0 & (1 << CST)
     if (D.1987_7 != 0)  */
  if (gimple_assign_rhs_code (stmt) == BIT_AND_EXPR
      && TREE_CODE (gimple_assign_rhs1 (stmt)) == SSA_NAME
      && integer_pow2p (gimple_assign_rhs2 (stmt)))
    {
      *name = gimple_assign_rhs1 (stmt);
      *bit = build_int_cst (integer_type_node,
			    tree_log2 (gimple_assign_rhs2 (stmt)));
      return true;
    }

  /* Another form is
     D.1986_6 = 1 << control1_4(D)
     D.1987_7 = op0 & D.1986_6
     if (D.1987_7 != 0)  */
  if (gimple_assign_rhs_code (stmt) == BIT_AND_EXPR
      && TREE_CODE (gimple_assign_rhs1 (stmt)) == SSA_NAME
      && TREE_CODE (gimple_assign_rhs2 (stmt)) == SSA_NAME)
    {
      gimple tmp;

      /* Both arguments of the BIT_AND_EXPR can be the single-bit
	 specifying expression.  */
      tmp = SSA_NAME_DEF_STMT (gimple_assign_rhs1 (stmt));
      if (is_gimple_assign (tmp)
	  && gimple_assign_rhs_code (tmp) == LSHIFT_EXPR
	  && integer_onep (gimple_assign_rhs1 (tmp)))
	{
	  *name = gimple_assign_rhs2 (stmt);
	  *bit = gimple_assign_rhs2 (tmp);
	  return true;
	}

      tmp = SSA_NAME_DEF_STMT (gimple_assign_rhs2 (stmt));
      if (is_gimple_assign (tmp)
	  && gimple_assign_rhs_code (tmp) == LSHIFT_EXPR
	  && integer_onep (gimple_assign_rhs1 (tmp)))
	{
	  *name = gimple_assign_rhs1 (stmt);
	  *bit = gimple_assign_rhs2 (tmp);
	  return true;
	}
    }

  return false;
}

/* Recognize a bit test pattern in a GIMPLE_COND and its defining
   statements.  Store the name being tested in *NAME and the bits
   in *BITS.  The COND_EXPR computes *NAME & *BITS.
   Returns true if the pattern matched, false otherwise.  */

static bool
recognize_bits_test (gimple cond, tree *name, tree *bits)
{
  gimple stmt;

  /* Get at the definition of the result of the bit test.  */
  if (gimple_cond_code (cond) != NE_EXPR
      || TREE_CODE (gimple_cond_lhs (cond)) != SSA_NAME
      || !integer_zerop (gimple_cond_rhs (cond)))
    return false;
  stmt = SSA_NAME_DEF_STMT (gimple_cond_lhs (cond));
  if (!is_gimple_assign (stmt)
      || gimple_assign_rhs_code (stmt) != BIT_AND_EXPR)
    return false;

  *name = get_name_for_bit_test (gimple_assign_rhs1 (stmt));
  *bits = gimple_assign_rhs2 (stmt);

  return true;
}

/* If-convert on a and pattern with a common else block.  The inner
   if is specified by its INNER_COND_BB, the outer by OUTER_COND_BB.
   Returns true if the edges to the common else basic-block were merged.  */

static bool
ifcombine_ifandif (basic_block inner_cond_bb, basic_block outer_cond_bb)
{
  gimple_stmt_iterator gsi;
  gimple inner_cond, outer_cond;
  tree name1, name2, bit1, bit2;

  inner_cond = last_stmt (inner_cond_bb);
  if (!inner_cond
      || gimple_code (inner_cond) != GIMPLE_COND)
    return false;

  outer_cond = last_stmt (outer_cond_bb);
  if (!outer_cond
      || gimple_code (outer_cond) != GIMPLE_COND)
    return false;

  /* See if we test a single bit of the same name in both tests.  In
     that case remove the outer test, merging both else edges,
     and change the inner one to test for
     name & (bit1 | bit2) == (bit1 | bit2).  */
  if (recognize_single_bit_test (inner_cond, &name1, &bit1)
      && recognize_single_bit_test (outer_cond, &name2, &bit2)
      && name1 == name2)
    {
      tree t, t2;

      /* Do it.  */
      gsi = gsi_for_stmt (inner_cond);
      t = fold_build2 (LSHIFT_EXPR, TREE_TYPE (name1),
		       build_int_cst (TREE_TYPE (name1), 1), bit1);
      t2 = fold_build2 (LSHIFT_EXPR, TREE_TYPE (name1),
		        build_int_cst (TREE_TYPE (name1), 1), bit2);
      t = fold_build2 (BIT_IOR_EXPR, TREE_TYPE (name1), t, t2);
      t = force_gimple_operand_gsi (&gsi, t, true, NULL_TREE,
				    true, GSI_SAME_STMT);
      t2 = fold_build2 (BIT_AND_EXPR, TREE_TYPE (name1), name1, t);
      t2 = force_gimple_operand_gsi (&gsi, t2, true, NULL_TREE,
				     true, GSI_SAME_STMT);
      t = fold_build2 (EQ_EXPR, boolean_type_node, t2, t);
      t = canonicalize_cond_expr_cond (t);
      if (!t)
	return false;
      gimple_cond_set_condition_from_tree (inner_cond, t);
      update_stmt (inner_cond);

      /* Leave CFG optimization to cfg_cleanup.  */
      gimple_cond_set_condition_from_tree (outer_cond, boolean_true_node);
      update_stmt (outer_cond);

      if (dump_file)
	{
	  fprintf (dump_file, "optimizing double bit test to ");
	  print_generic_expr (dump_file, name1, 0);
	  fprintf (dump_file, " & T == T\nwith temporary T = (1 << ");
	  print_generic_expr (dump_file, bit1, 0);
	  fprintf (dump_file, ") | (1 << ");
	  print_generic_expr (dump_file, bit2, 0);
	  fprintf (dump_file, ")\n");
	}

      return true;
    }

  /* See if we have two comparisons that we can merge into one.  */
  else if (TREE_CODE_CLASS (gimple_cond_code (inner_cond)) == tcc_comparison
	   && TREE_CODE_CLASS (gimple_cond_code (outer_cond)) == tcc_comparison)
    {
      tree t;

      if (!(t = maybe_fold_and_comparisons (gimple_cond_code (inner_cond),
					    gimple_cond_lhs (inner_cond),
					    gimple_cond_rhs (inner_cond),
					    gimple_cond_code (outer_cond),
					    gimple_cond_lhs (outer_cond),
					    gimple_cond_rhs (outer_cond))))
	return false;
      t = canonicalize_cond_expr_cond (t);
      if (!t)
	return false;
      gimple_cond_set_condition_from_tree (inner_cond, t);
      update_stmt (inner_cond);

      /* Leave CFG optimization to cfg_cleanup.  */
      gimple_cond_set_condition_from_tree (outer_cond, boolean_true_node);
      update_stmt (outer_cond);

      if (dump_file)
	{
	  fprintf (dump_file, "optimizing two comparisons to ");
	  print_generic_expr (dump_file, t, 0);
	  fprintf (dump_file, "\n");
	}

      return true;
    }

  return false;
}

/* If-convert on a or pattern with a common then block.  The inner
   if is specified by its INNER_COND_BB, the outer by OUTER_COND_BB.
   Returns true, if the edges leading to the common then basic-block
   were merged.  */

static bool
ifcombine_iforif (basic_block inner_cond_bb, basic_block outer_cond_bb)
{
  gimple inner_cond, outer_cond;
  tree name1, name2, bits1, bits2;

  inner_cond = last_stmt (inner_cond_bb);
  if (!inner_cond
      || gimple_code (inner_cond) != GIMPLE_COND)
    return false;

  outer_cond = last_stmt (outer_cond_bb);
  if (!outer_cond
      || gimple_code (outer_cond) != GIMPLE_COND)
    return false;

  /* See if we have two bit tests of the same name in both tests.
     In that case remove the outer test and change the inner one to
     test for name & (bits1 | bits2) != 0.  */
  if (recognize_bits_test (inner_cond, &name1, &bits1)
      && recognize_bits_test (outer_cond, &name2, &bits2))
    {
      gimple_stmt_iterator gsi;
      tree t;

      /* Find the common name which is bit-tested.  */
      if (name1 == name2)
	;
      else if (bits1 == bits2)
	{
	  t = name2;
	  name2 = bits2;
	  bits2 = t;
	  t = name1;
	  name1 = bits1;
	  bits1 = t;
	}
      else if (name1 == bits2)
	{
	  t = name2;
	  name2 = bits2;
	  bits2 = t;
	}
      else if (bits1 == name2)
	{
	  t = name1;
	  name1 = bits1;
	  bits1 = t;
	}
      else
	return false;

      /* As we strip non-widening conversions in finding a common
         name that is tested make sure to end up with an integral
	 type for building the bit operations.  */
      if (TYPE_PRECISION (TREE_TYPE (bits1))
	  >= TYPE_PRECISION (TREE_TYPE (bits2)))
	{
	  bits1 = fold_convert (unsigned_type_for (TREE_TYPE (bits1)), bits1);
	  name1 = fold_convert (TREE_TYPE (bits1), name1);
	  bits2 = fold_convert (unsigned_type_for (TREE_TYPE (bits2)), bits2);
	  bits2 = fold_convert (TREE_TYPE (bits1), bits2);
	}
      else
	{
	  bits2 = fold_convert (unsigned_type_for (TREE_TYPE (bits2)), bits2);
	  name1 = fold_convert (TREE_TYPE (bits2), name1);
	  bits1 = fold_convert (unsigned_type_for (TREE_TYPE (bits1)), bits1);
	  bits1 = fold_convert (TREE_TYPE (bits2), bits1);
	}

      /* Do it.  */
      gsi = gsi_for_stmt (inner_cond);
      t = fold_build2 (BIT_IOR_EXPR, TREE_TYPE (name1), bits1, bits2);
      t = force_gimple_operand_gsi (&gsi, t, true, NULL_TREE,
				    true, GSI_SAME_STMT);
      t = fold_build2 (BIT_AND_EXPR, TREE_TYPE (name1), name1, t);
      t = force_gimple_operand_gsi (&gsi, t, true, NULL_TREE,
				    true, GSI_SAME_STMT);
      t = fold_build2 (NE_EXPR, boolean_type_node, t,
		       build_int_cst (TREE_TYPE (t), 0));
      t = canonicalize_cond_expr_cond (t);
      if (!t)
	return false;
      gimple_cond_set_condition_from_tree (inner_cond, t);
      update_stmt (inner_cond);

      /* Leave CFG optimization to cfg_cleanup.  */
      gimple_cond_set_condition_from_tree (outer_cond, boolean_false_node);
      update_stmt (outer_cond);

      if (dump_file)
	{
	  fprintf (dump_file, "optimizing bits or bits test to ");
	  print_generic_expr (dump_file, name1, 0);
	  fprintf (dump_file, " & T != 0\nwith temporary T = ");
	  print_generic_expr (dump_file, bits1, 0);
	  fprintf (dump_file, " | ");
	  print_generic_expr (dump_file, bits2, 0);
	  fprintf (dump_file, "\n");
	}

      return true;
    }

  /* See if we have two comparisons that we can merge into one.
     This happens for C++ operator overloading where for example
     GE_EXPR is implemented as GT_EXPR || EQ_EXPR.  */
    else if (TREE_CODE_CLASS (gimple_cond_code (inner_cond)) == tcc_comparison
	   && TREE_CODE_CLASS (gimple_cond_code (outer_cond)) == tcc_comparison)
    {
      tree t;

      if (!(t = maybe_fold_or_comparisons (gimple_cond_code (inner_cond),
					   gimple_cond_lhs (inner_cond),
					   gimple_cond_rhs (inner_cond),
					   gimple_cond_code (outer_cond),
					   gimple_cond_lhs (outer_cond),
					   gimple_cond_rhs (outer_cond))))
	return false;
      t = canonicalize_cond_expr_cond (t);
      if (!t)
	return false;
      gimple_cond_set_condition_from_tree (inner_cond, t);
      update_stmt (inner_cond);

      /* Leave CFG optimization to cfg_cleanup.  */
      gimple_cond_set_condition_from_tree (outer_cond, boolean_false_node);
      update_stmt (outer_cond);

      if (dump_file)
	{
	  fprintf (dump_file, "optimizing two comparisons to ");
	  print_generic_expr (dump_file, t, 0);
	  fprintf (dump_file, "\n");
	}

      return true;
    }

  return false;
}

/* Recognize a CFG pattern and dispatch to the appropriate
   if-conversion helper.  We start with BB as the innermost
   worker basic-block.  Returns true if a transformation was done.  */

static bool
tree_ssa_ifcombine_bb (basic_block inner_cond_bb)
{
  basic_block then_bb = NULL, else_bb = NULL;

  if (!recognize_if_then_else (inner_cond_bb, &then_bb, &else_bb))
    return false;

  /* Recognize && and || of two conditions with a common
     then/else block which entry edges we can merge.  That is:
       if (a || b)
	 ;
     and
       if (a && b)
	 ;
     This requires a single predecessor of the inner cond_bb.  */
  if (single_pred_p (inner_cond_bb))
    {
      basic_block outer_cond_bb = single_pred (inner_cond_bb);

      /* The && form is characterized by a common else_bb with
	 the two edges leading to it mergable.  The latter is
	 guaranteed by matching PHI arguments in the else_bb and
	 the inner cond_bb having no side-effects.  */
      if (recognize_if_then_else (outer_cond_bb, &inner_cond_bb, &else_bb)
	  && same_phi_args_p (outer_cond_bb, inner_cond_bb, else_bb)
	  && bb_no_side_effects_p (inner_cond_bb))
	{
	  /* We have
	       <outer_cond_bb>
		 if (q) goto inner_cond_bb; else goto else_bb;
	       <inner_cond_bb>
		 if (p) goto ...; else goto else_bb;
		 ...
	       <else_bb>
		 ...
	   */
	  return ifcombine_ifandif (inner_cond_bb, outer_cond_bb);
	}

      /* The || form is characterized by a common then_bb with the
	 two edges leading to it mergable.  The latter is guaranteed
         by matching PHI arguments in the then_bb and the inner cond_bb
	 having no side-effects.  */
      if (recognize_if_then_else (outer_cond_bb, &then_bb, &inner_cond_bb)
	  && same_phi_args_p (outer_cond_bb, inner_cond_bb, then_bb)
	  && bb_no_side_effects_p (inner_cond_bb))
	{
	  /* We have
	       <outer_cond_bb>
		 if (q) goto then_bb; else goto inner_cond_bb;
	       <inner_cond_bb>
		 if (q) goto then_bb; else goto ...;
	       <then_bb>
		 ...
	   */
	  return ifcombine_iforif (inner_cond_bb, outer_cond_bb);
	}
    }

  return false;
}

/* Main entry for the tree if-conversion pass.  */

static unsigned int
tree_ssa_ifcombine (void)
{
  basic_block *bbs;
  bool cfg_changed = false;
  int i;

  bbs = blocks_in_phiopt_order ();
  calculate_dominance_info (CDI_DOMINATORS);

  for (i = 0; i < n_basic_blocks - NUM_FIXED_BLOCKS; ++i)
    {
      basic_block bb = bbs[i];
      gimple stmt = last_stmt (bb);

      if (stmt
	  && gimple_code (stmt) == GIMPLE_COND)
	cfg_changed |= tree_ssa_ifcombine_bb (bb);
    }

  free (bbs);

  return cfg_changed ? TODO_cleanup_cfg : 0;
}

static bool
gate_ifcombine (void)
{
  return 1;
}

struct gimple_opt_pass pass_tree_ifcombine =
{
 {
  GIMPLE_PASS,
  "ifcombine",			/* name */
  gate_ifcombine,		/* gate */
  tree_ssa_ifcombine,		/* execute */
  NULL,				/* sub */
  NULL,				/* next */
  0,				/* static_pass_number */
  TV_TREE_IFCOMBINE,		/* tv_id */
  PROP_cfg | PROP_ssa,		/* properties_required */
  0,				/* properties_provided */
  0,				/* properties_destroyed */
  0,				/* todo_flags_start */
  TODO_dump_func
  | TODO_ggc_collect
  | TODO_update_ssa
  | TODO_verify_ssa		/* todo_flags_finish */
 }
};
