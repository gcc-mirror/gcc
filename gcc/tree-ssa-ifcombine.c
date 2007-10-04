/* Combining of if-expressions on trees.
   Copyright (C) 2007 Free Software Foundation, Inc.
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
#include "diagnostic.h"
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
  block_stmt_iterator bsi;

  for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
    {
      tree stmt = bsi_stmt (bsi);
      stmt_ann_t ann = stmt_ann (stmt);

      if (ann->has_volatile_ops
	  || !ZERO_SSA_OPERANDS (stmt, SSA_OP_ALL_VIRTUALS))
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
  tree phi;

  for (phi = phi_nodes (dest); phi; phi = PHI_CHAIN (phi))
    if (!operand_equal_p (PHI_ARG_DEF_FROM_EDGE (phi, e1),
			  PHI_ARG_DEF_FROM_EDGE (phi, e2), 0))
      return false;

  return true;
}

/* Recognize a single bit test pattern in COND_EXPR and its defining
   statements.  Store the name being tested in *NAME and the bit
   in *BIT.  The COND_EXPR computes *NAME & (1 << *BIT).
   Returns true if the pattern matched, false otherwise.  */

static bool
recognize_single_bit_test (tree cond_expr, tree *name, tree *bit)
{
  tree t;

  /* Get at the definition of the result of the bit test.  */
  t = TREE_OPERAND (cond_expr, 0);
  if (TREE_CODE (t) == NE_EXPR
      && integer_zerop (TREE_OPERAND (t, 1)))
    t = TREE_OPERAND (t, 0);
  if (TREE_CODE (t) != SSA_NAME)
    return false;
  t = SSA_NAME_DEF_STMT (t);
  if (TREE_CODE (t) != GIMPLE_MODIFY_STMT)
    return false;
  t = GIMPLE_STMT_OPERAND (t, 1);

  /* Look at which bit is tested.  One form to recognize is
     D.1985_5 = state_3(D) >> control1_4(D);
     D.1986_6 = (int) D.1985_5;
     D.1987_7 = op0 & 1;
     if (D.1987_7 != 0)  */
  if (TREE_CODE (t) == BIT_AND_EXPR
      && integer_onep (TREE_OPERAND (t, 1))
      && TREE_CODE (TREE_OPERAND (t, 0)) == SSA_NAME)
    {
      tree orig_name = TREE_OPERAND (t, 0);

      /* Look through copies and conversions to eventually
	 find the stmt that computes the shift.  */
      t = orig_name;
      do {
	t = SSA_NAME_DEF_STMT (t);
	if (TREE_CODE (t) != GIMPLE_MODIFY_STMT)
	  break;
	t = GIMPLE_STMT_OPERAND (t, 1);
	if (TREE_CODE (t) == NOP_EXPR
	    || TREE_CODE (t) == CONVERT_EXPR)
	  t = TREE_OPERAND (t, 0);
      } while (TREE_CODE (t) == SSA_NAME);

      /* If we found such, decompose it.  */
      if (TREE_CODE (t) == RSHIFT_EXPR)
	{
	  /* op0 & (1 << op1) */
	  *bit = TREE_OPERAND (t, 1);
	  *name = TREE_OPERAND (t, 0);
	}
      else
	{
	  /* t & 1 */
	  *bit = integer_zero_node;
	  *name = orig_name;
	}

      return true;
    }

  /* Another form is
     D.1987_7 = op0 & (1 << CST)
     if (D.1987_7 != 0)  */
  if (TREE_CODE (t) == BIT_AND_EXPR
      && TREE_CODE (TREE_OPERAND (t, 0)) == SSA_NAME
      && integer_pow2p (TREE_OPERAND (t, 1)))
    {
      *name = TREE_OPERAND (t, 0);
      *bit = build_int_cst (integer_type_node,
			    tree_log2 (TREE_OPERAND (t, 1)));
      return true;
    }

  /* Another form is
     D.1986_6 = 1 << control1_4(D)
     D.1987_7 = op0 & D.1986_6
     if (D.1987_7 != 0)  */
  if (TREE_CODE (t) == BIT_AND_EXPR
      && TREE_CODE (TREE_OPERAND (t, 0)) == SSA_NAME
      && TREE_CODE (TREE_OPERAND (t, 1)) == SSA_NAME)
    {
      tree tmp;

      /* Both arguments of the BIT_AND_EXPR can be the single-bit
	 specifying expression.  */
      tmp = SSA_NAME_DEF_STMT (TREE_OPERAND (t, 0));
      if (TREE_CODE (tmp) == GIMPLE_MODIFY_STMT
	  && TREE_CODE (GIMPLE_STMT_OPERAND (tmp, 1)) == LSHIFT_EXPR
	  && integer_onep (TREE_OPERAND (GIMPLE_STMT_OPERAND (tmp, 1), 0)))
	{
	  *name = TREE_OPERAND (t, 1);
	  *bit = TREE_OPERAND (GIMPLE_STMT_OPERAND (tmp, 1), 1);
	  return true;
	}

      tmp = SSA_NAME_DEF_STMT (TREE_OPERAND (t, 1));
      if (TREE_CODE (tmp) == GIMPLE_MODIFY_STMT
	  && TREE_CODE (GIMPLE_STMT_OPERAND (tmp, 1)) == LSHIFT_EXPR
	  && integer_onep (TREE_OPERAND (GIMPLE_STMT_OPERAND (tmp, 1), 0)))
	{
	  *name = TREE_OPERAND (t, 0);
	  *bit = TREE_OPERAND (GIMPLE_STMT_OPERAND (tmp, 1), 1);
	  return true;
	}
    }

  return false;
}

/* Recognize a bit test pattern in COND_EXPR and its defining
   statements.  Store the name being tested in *NAME and the bits
   in *BITS.  The COND_EXPR computes *NAME & *BITS.
   Returns true if the pattern matched, false otherwise.  */

static bool
recognize_bits_test (tree cond_expr, tree *name, tree *bits)
{
  tree t;

  /* Get at the definition of the result of the bit test.  */
  t = TREE_OPERAND (cond_expr, 0);
  if (TREE_CODE (t) == NE_EXPR
      && integer_zerop (TREE_OPERAND (t, 1)))
    t = TREE_OPERAND (t, 0);
  if (TREE_CODE (t) != SSA_NAME)
    return false;
  t = SSA_NAME_DEF_STMT (t);
  if (TREE_CODE (t) != GIMPLE_MODIFY_STMT)
    return false;
  t = GIMPLE_STMT_OPERAND (t, 1);

  if (TREE_CODE (t) != BIT_AND_EXPR)
    return false;

  *name = TREE_OPERAND (t, 0);
  *bits = TREE_OPERAND (t, 1);

  return true;
}

/* If-convert on a and pattern with a common else block.  The inner
   if is specified by its INNER_COND_BB, the outer by OUTER_COND_BB.
   Returns true if the edges to the common else basic-block were merged.  */

static bool
ifcombine_ifandif (basic_block inner_cond_bb, basic_block outer_cond_bb)
{
  block_stmt_iterator bsi;
  tree inner_cond, outer_cond;
  tree name1, name2, bit1, bit2;

  inner_cond = last_stmt (inner_cond_bb);
  if (!inner_cond
      || TREE_CODE (inner_cond) != COND_EXPR)
    return false;

  outer_cond = last_stmt (outer_cond_bb);
  if (!outer_cond
      || TREE_CODE (outer_cond) != COND_EXPR)
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
      bsi = bsi_for_stmt (inner_cond);
      t = fold_build2 (LSHIFT_EXPR, TREE_TYPE (name1),
		       build_int_cst (TREE_TYPE (name1), 1), bit1);
      t2 = fold_build2 (LSHIFT_EXPR, TREE_TYPE (name1),
		        build_int_cst (TREE_TYPE (name1), 1), bit2);
      t = fold_build2 (BIT_IOR_EXPR, TREE_TYPE (name1), t, t2);
      t = force_gimple_operand_bsi (&bsi, t, true, NULL_TREE,
				    true, BSI_SAME_STMT);
      t2 = fold_build2 (BIT_AND_EXPR, TREE_TYPE (name1), name1, t);
      t2 = force_gimple_operand_bsi (&bsi, t2, true, NULL_TREE,
				     true, BSI_SAME_STMT);
      COND_EXPR_COND (inner_cond) = fold_build2 (EQ_EXPR, boolean_type_node,
						 t2, t);
      update_stmt (inner_cond);

      /* Leave CFG optimization to cfg_cleanup.  */
      COND_EXPR_COND (outer_cond) = boolean_true_node;
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

  return false;
}

/* If-convert on a or pattern with a common then block.  The inner
   if is specified by its INNER_COND_BB, the outer by OUTER_COND_BB.
   Returns true, if the edges leading to the common then basic-block
   were merged.  */

static bool
ifcombine_iforif (basic_block inner_cond_bb, basic_block outer_cond_bb)
{
  tree inner_cond, outer_cond;
  tree name1, name2, bits1, bits2;

  inner_cond = last_stmt (inner_cond_bb);
  if (!inner_cond
      || TREE_CODE (inner_cond) != COND_EXPR)
    return false;

  outer_cond = last_stmt (outer_cond_bb);
  if (!outer_cond
      || TREE_CODE (outer_cond) != COND_EXPR)
    return false;

  /* See if we have two bit tests of the same name in both tests.
     In that case remove the outer test and change the inner one to
     test for name & (bits1 | bits2) != 0.  */
  if (recognize_bits_test (inner_cond, &name1, &bits1)
      && recognize_bits_test (outer_cond, &name2, &bits2))
    {
      block_stmt_iterator bsi;
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

      /* Do it.  */
      bsi = bsi_for_stmt (inner_cond);
      t = fold_build2 (BIT_IOR_EXPR, TREE_TYPE (name1), bits1, bits2);
      t = force_gimple_operand_bsi (&bsi, t, true, NULL_TREE,
				    true, BSI_SAME_STMT);
      t = fold_build2 (BIT_AND_EXPR, TREE_TYPE (name1), name1, t);
      t = force_gimple_operand_bsi (&bsi, t, true, NULL_TREE,
				    true, BSI_SAME_STMT);
      COND_EXPR_COND (inner_cond) = fold_build2 (NE_EXPR, boolean_type_node, t,
						 build_int_cst (TREE_TYPE (t), 0));
      update_stmt (inner_cond);

      /* Leave CFG optimization to cfg_cleanup.  */
      COND_EXPR_COND (outer_cond) = boolean_false_node;
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
  else if (COMPARISON_CLASS_P (COND_EXPR_COND (inner_cond))
	   && COMPARISON_CLASS_P (COND_EXPR_COND (outer_cond))
	   && operand_equal_p (TREE_OPERAND (COND_EXPR_COND (inner_cond), 0),
			       TREE_OPERAND (COND_EXPR_COND (outer_cond), 0), 0)
	   && operand_equal_p (TREE_OPERAND (COND_EXPR_COND (inner_cond), 1),
			       TREE_OPERAND (COND_EXPR_COND (outer_cond), 1), 0))
    {
      tree ccond1 = COND_EXPR_COND (inner_cond);
      tree ccond2 = COND_EXPR_COND (outer_cond);
      enum tree_code code1 = TREE_CODE (ccond1);
      enum tree_code code2 = TREE_CODE (ccond2);
      enum tree_code code;
      tree t;

#define CHK(a,b) ((code1 == a ## _EXPR && code2 == b ## _EXPR) \
		  || (code2 == a ## _EXPR && code1 == b ## _EXPR))
      /* Merge the two condition codes if possible.  */
      if (code1 == code2)
	code = code1;
      else if (CHK (EQ, LT))
	code = LE_EXPR;
      else if (CHK (EQ, GT))
	code = GE_EXPR;
      else if (CHK (LT, LE))
	code = LE_EXPR;
      else if (CHK (GT, GE))
	code = GE_EXPR;
      else if (INTEGRAL_TYPE_P (TREE_TYPE (TREE_OPERAND (ccond1, 0)))
	       || flag_unsafe_math_optimizations)
	{
	  if (CHK (LT, GT))
	    code = NE_EXPR;
	  else if (CHK (LT, NE))
	    code = NE_EXPR;
	  else if (CHK (GT, NE))
	    code = NE_EXPR;
	  else
	    return false;
	}
      /* We could check for combinations leading to trivial true/false.  */
      else
	return false;
#undef CHK

      /* Do it.  */
      t = fold_build2 (code, boolean_type_node,
		       TREE_OPERAND (ccond2, 0), TREE_OPERAND (ccond2, 1));
      t = canonicalize_cond_expr_cond (t);
      if (!t)
	return false;
      COND_EXPR_COND (inner_cond) = t;
      update_stmt (inner_cond);

      /* Leave CFG optimization to cfg_cleanup.  */
      COND_EXPR_COND (outer_cond) = boolean_false_node;
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

  for (i = 0; i < n_basic_blocks - NUM_FIXED_BLOCKS; ++i)
    {
      basic_block bb = bbs[i];
      tree stmt = last_stmt (bb);

      if (stmt
	  && TREE_CODE (stmt) == COND_EXPR)
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

struct tree_opt_pass pass_tree_ifcombine = {
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
  | TODO_verify_ssa,		/* todo_flags_finish */
  0				/* letter */
};
