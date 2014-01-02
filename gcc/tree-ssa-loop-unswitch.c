/* Loop unswitching.
   Copyright (C) 2004-2014 Free Software Foundation, Inc.

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
#include "tm_p.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimplify.h"
#include "gimple-ssa.h"
#include "tree-cfg.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "tree-ssa-loop-niter.h"
#include "tree-ssa-loop.h"
#include "tree-into-ssa.h"
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
  struct loop *loop;
  bool changed = false;
  HOST_WIDE_INT iterations;

  /* Go through inner loops (only original ones).  */
  FOR_EACH_LOOP (loop, LI_ONLY_INNERMOST)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
        fprintf (dump_file, ";; Considering loop %d\n", loop->num);

      /* Do not unswitch in cold regions. */
      if (optimize_loop_for_size_p (loop))
        {
          if (dump_file && (dump_flags & TDF_DETAILS))
            fprintf (dump_file, ";; Not unswitching cold loops\n");
          continue;
        }

      /* The loop should not be too large, to limit code growth. */
      if (tree_num_loop_insns (loop, &eni_size_weights)
          > (unsigned) PARAM_VALUE (PARAM_MAX_UNSWITCH_INSNS))
        {
          if (dump_file && (dump_flags & TDF_DETAILS))
            fprintf (dump_file, ";; Not unswitching, loop too big\n");
          continue;
        }

      /* If the loop is not expected to iterate, there is no need
	 for unswitching.  */
      iterations = estimated_loop_iterations_int (loop);
      if (iterations >= 0 && iterations <= 1)
	{
          if (dump_file && (dump_flags & TDF_DETAILS))
            fprintf (dump_file, ";; Not unswitching, loop is not expected to iterate\n");
          continue;
	}

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

  /* To keep the things simple, we do not directly remove the conditions,
     but just replace tests with 0 != 0 resp. 1 != 0.  Prevent the infinite
     loop where we would unswitch again on such a condition.  */
  if (gimple_cond_true_p (stmt) || gimple_cond_false_p (stmt))
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
      if (e->src == ENTRY_BLOCK_PTR_FOR_FN (cfun))
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
  unsigned i, found;
  tree cond = NULL_TREE;
  gimple stmt;
  bool changed = false;

  i = 0;
  bbs = get_loop_body (loop);
  found = loop->num_nodes;

  while (1)
    {
      /* Find a bb to unswitch on.  */
      for (; i < loop->num_nodes; i++)
	if ((cond = tree_may_unswitch_on (bbs[i], loop)))
	  break;

      if (i == loop->num_nodes)
	{
	  if (dump_file
	      && num > PARAM_VALUE (PARAM_MAX_UNSWITCH_LEVEL)
	      && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, ";; Not unswitching anymore, hit max level\n");

	  if (found == loop->num_nodes)
	    {
	      free (bbs);
	      return changed;
	    }
	  break;
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
      /* Do not unswitch too much.  */
      else if (num > PARAM_VALUE (PARAM_MAX_UNSWITCH_LEVEL))
	{
	  i++;
	  continue;
	}
      /* In nested tree_unswitch_single_loop first optimize all conditions
	 using entry checks, then discover still reachable blocks in the
	 loop and find the condition only among those still reachable bbs.  */
      else if (num != 0)
	{
	  if (found == loop->num_nodes)
	    found = i;
	  i++;
	  continue;
	}
      else
	{
	  found = i;
	  break;
	}

      update_stmt (stmt);
      i++;
    }

  if (num != 0)
    {
      basic_block *tos, *worklist;

      /* When called recursively, first do a quick discovery
	 of reachable bbs after the above changes and only
	 consider conditions in still reachable bbs.  */
      tos = worklist = XNEWVEC (basic_block, loop->num_nodes);

      for (i = 0; i < loop->num_nodes; i++)
	bbs[i]->flags &= ~BB_REACHABLE;

      /* Start with marking header.  */
      *tos++ = bbs[0];
      bbs[0]->flags |= BB_REACHABLE;

      /* Iterate: find everything reachable from what we've already seen
	 within the same innermost loop.  Don't look through false edges
	 if condition is always true or true edges if condition is
	 always false.  */
      while (tos != worklist)
	{
	  basic_block b = *--tos;
	  edge e;
	  edge_iterator ei;
	  int flags = 0;

	  if (EDGE_COUNT (b->succs) == 2)
	    {
	      gimple stmt = last_stmt (b);
	      if (stmt
		  && gimple_code (stmt) == GIMPLE_COND)
		{
		  if (gimple_cond_true_p (stmt))
		    flags = EDGE_FALSE_VALUE;
		  else if (gimple_cond_false_p (stmt))
		    flags = EDGE_TRUE_VALUE;
		}
	    }

	  FOR_EACH_EDGE (e, ei, b->succs)
	    {
	      basic_block dest = e->dest;

	      if (dest->loop_father == loop
		  && !(dest->flags & BB_REACHABLE)
		  && !(e->flags & flags))
		{
		  *tos++ = dest;
		  dest->flags |= BB_REACHABLE;
		}
	    }
	}

      free (worklist);

      /* Find a bb to unswitch on.  */
      for (; found < loop->num_nodes; found++)
	if ((bbs[found]->flags & BB_REACHABLE)
	    && (cond = tree_may_unswitch_on (bbs[found], loop)))
	  break;

      if (found == loop->num_nodes)
	{
	  free (bbs);
	  return changed;
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, ";; Unswitching loop\n");

  initialize_original_copy_tables ();
  /* Unswitch the loop on this condition.  */
  nloop = tree_unswitch_loop (loop, bbs[found], cond);
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

/* Loop unswitching pass.  */

static unsigned int
tree_ssa_loop_unswitch (void)
{
  if (number_of_loops (cfun) <= 1)
    return 0;

  return tree_ssa_unswitch_loops ();
}

static bool
gate_tree_ssa_loop_unswitch (void)
{
  return flag_unswitch_loops != 0;
}

namespace {

const pass_data pass_data_tree_unswitch =
{
  GIMPLE_PASS, /* type */
  "unswitch", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_TREE_LOOP_UNSWITCH, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_tree_unswitch : public gimple_opt_pass
{
public:
  pass_tree_unswitch (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_tree_unswitch, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_tree_ssa_loop_unswitch (); }
  unsigned int execute () { return tree_ssa_loop_unswitch (); }

}; // class pass_tree_unswitch

} // anon namespace

gimple_opt_pass *
make_pass_tree_unswitch (gcc::context *ctxt)
{
  return new pass_tree_unswitch (ctxt);
}


