/* Loop unswitching.
   Copyright (C) 2004, 2005, 2007, 2008 Free Software Foundation, Inc.
   
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
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "output.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "timevar.h"
#include "cfgloop.h"
#include "params.h"
#include "tree-pass.h"
#include "tree-inline.h"

/* This file implements the loop unswitching, i.e. transformation of loops like

   while (A)
     {
       if (inv)
         B;

       X;

       if (!inv)
	 C;
     }

   where inv is the loop invariant, into

   if (inv)
     {
       while (A)
	 {
           B;
	   X;
	 }
     }
   else
     {
       while (A)
	 {
	   X;
	   C;
	 }
     }

   Inv is considered invariant iff the values it compares are both invariant;
   tree-ssa-loop-im.c ensures that all the suitable conditions are in this
   shape.  */

static struct loop *tree_unswitch_loop (struct loop *, basic_block, tree);
static bool tree_unswitch_single_loop (struct loop *, int);
static tree tree_may_unswitch_on (basic_block, struct loop *);

/* Main entry point.  Perform loop unswitching on all suitable loops.  */

unsigned int
tree_ssa_unswitch_loops (void)
{
  loop_iterator li;
  struct loop *loop;
  bool changed = false;

  /* Go through inner loops (only original ones).  */
  FOR_EACH_LOOP (li, loop, LI_ONLY_INNERMOST)
    {
      changed |= tree_unswitch_single_loop (loop, 0);
    }

  if (changed)
    return TODO_cleanup_cfg;
  return 0;
}

/* Checks whether we can unswitch LOOP on condition at end of BB -- one of its
   basic blocks (for what it means see comments below).  */

static tree
tree_may_unswitch_on (basic_block bb, struct loop *loop)
{
  gimple stmt, def;
  tree cond, use;
  basic_block def_bb;
  ssa_op_iter iter;

  /* BB must end in a simple conditional jump.  */
  stmt = last_stmt (bb);
  if (!stmt || gimple_code (stmt) != GIMPLE_COND)
    return NULL_TREE;

  /* Condition must be invariant.  */
  FOR_EACH_SSA_TREE_OPERAND (use, stmt, iter, SSA_OP_USE)
    {
      def = SSA_NAME_DEF_STMT (use);
      def_bb = gimple_bb (def);
      if (def_bb
	  && flow_bb_inside_loop_p (loop, def_bb))
	return NULL_TREE;
    }

  cond = build2 (gimple_cond_code (stmt), boolean_type_node,
		 gimple_cond_lhs (stmt), gimple_cond_rhs (stmt));

  /* To keep the things simple, we do not directly remove the conditions,
     but just replace tests with 0/1.  Prevent the infinite loop where we
     would unswitch again on such a condition.  */
  if (integer_zerop (cond) || integer_nonzerop (cond))
    return NULL_TREE;

  return cond;
}

/* Simplifies COND using checks in front of the entry of the LOOP.  Just very
   simplish (sufficient to prevent us from duplicating loop in unswitching
   unnecessarily).  */

static tree
simplify_using_entry_checks (struct loop *loop, tree cond)
{
  edge e = loop_preheader_edge (loop);
  gimple stmt;

  while (1)
    {
      stmt = last_stmt (e->src);
      if (stmt
	  && gimple_code (stmt) == GIMPLE_COND
	  && gimple_cond_code (stmt) == TREE_CODE (cond)
	  && operand_equal_p (gimple_cond_lhs (stmt),
			      TREE_OPERAND (cond, 0), 0)
	  && operand_equal_p (gimple_cond_rhs (stmt),
			      TREE_OPERAND (cond, 1), 0))
	return (e->flags & EDGE_TRUE_VALUE
		? boolean_true_node
		: boolean_false_node);

      if (!single_pred_p (e->src))
	return cond;

      e = single_pred_edge (e->src);
      if (e->src == ENTRY_BLOCK_PTR)
	return cond;
    }
}

/* Unswitch single LOOP.  NUM is number of unswitchings done; we do not allow
   it to grow too much, it is too easy to create example on that the code would
   grow exponentially.  */

static bool
tree_unswitch_single_loop (struct loop *loop, int num)
{
  basic_block *bbs;
  struct loop *nloop;
  unsigned i;
  tree cond = NULL_TREE;
  gimple stmt;
  bool changed = false;

  /* Do not unswitch too much.  */
  if (num > PARAM_VALUE (PARAM_MAX_UNSWITCH_LEVEL))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, ";; Not unswitching anymore, hit max level\n");
      return false;
    }

  /* Only unswitch innermost loops.  */
  if (loop->inner)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, ";; Not unswitching, not innermost loop\n");
      return false;
    }

  /* Do not unswitch in cold regions.  */
  if (optimize_loop_for_size_p (loop))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, ";; Not unswitching cold loops\n");
      return false;
    }

  /* The loop should not be too large, to limit code growth.  */
  if (tree_num_loop_insns (loop, &eni_size_weights)
      > (unsigned) PARAM_VALUE (PARAM_MAX_UNSWITCH_INSNS))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, ";; Not unswitching, loop too big\n");
      return false;
    }

  i = 0;
  bbs = get_loop_body (loop);
  
  while (1)
    {
      /* Find a bb to unswitch on.  */
      for (; i < loop->num_nodes; i++)
	if ((cond = tree_may_unswitch_on (bbs[i], loop)))
	  break;

      if (i == loop->num_nodes)
	{
	  free (bbs);
	  return changed;
	}

      cond = simplify_using_entry_checks (loop, cond);
      stmt = last_stmt (bbs[i]);
      if (integer_nonzerop (cond))
	{
	  /* Remove false path.  */
	  gimple_cond_set_condition_from_tree (stmt, boolean_true_node);
	  changed = true;
	}
      else if (integer_zerop (cond))
	{
	  /* Remove true path.  */
	  gimple_cond_set_condition_from_tree (stmt, boolean_false_node);
	  changed = true;
	}
      else
	break;

      update_stmt (stmt);
      i++;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, ";; Unswitching loop\n");

  initialize_original_copy_tables ();
  /* Unswitch the loop on this condition.  */
  nloop = tree_unswitch_loop (loop, bbs[i], cond);
  if (!nloop)
    {
      free_original_copy_tables ();
      free (bbs);
      return changed;
    }

  /* Update the SSA form after unswitching.  */
  update_ssa (TODO_update_ssa);
  free_original_copy_tables ();

  /* Invoke itself on modified loops.  */
  tree_unswitch_single_loop (nloop, num + 1);
  tree_unswitch_single_loop (loop, num + 1);
  free (bbs);
  return true;
}

/* Unswitch a LOOP w.r. to given basic block UNSWITCH_ON.  We only support
   unswitching of innermost loops.  COND is the condition determining which
   loop is entered -- the new loop is entered if COND is true.  Returns NULL
   if impossible, new loop otherwise.  */

static struct loop *
tree_unswitch_loop (struct loop *loop,
		    basic_block unswitch_on, tree cond)
{
  unsigned prob_true;
  edge edge_true, edge_false;

  /* Some sanity checking.  */
  gcc_assert (flow_bb_inside_loop_p (loop, unswitch_on));
  gcc_assert (EDGE_COUNT (unswitch_on->succs) == 2);
  gcc_assert (loop->inner == NULL);

  extract_true_false_edges_from_block (unswitch_on, &edge_true, &edge_false);
  prob_true = edge_true->probability;
  return loop_version (loop, unshare_expr (cond), 
		       NULL, prob_true, prob_true,
		       REG_BR_PROB_BASE - prob_true, false);
}
