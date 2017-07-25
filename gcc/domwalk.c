/* Generic dominator tree walker
   Copyright (C) 2003-2017 Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@redhat.com>

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
#include "cfganal.h"
#include "domwalk.h"
#include "dumpfile.h"

/* This file implements a generic walker for dominator trees.

  To understand the dominator walker one must first have a grasp of dominators,
  immediate dominators and the dominator tree.

  Dominators
    A block B1 is said to dominate B2 if every path from the entry to B2 must
    pass through B1.  Given the dominance relationship, we can proceed to
    compute immediate dominators.  Note it is not important whether or not
    our definition allows a block to dominate itself.

  Immediate Dominators:
    Every block in the CFG has no more than one immediate dominator.  The
    immediate dominator of block BB must dominate BB and must not dominate
    any other dominator of BB and must not be BB itself.

  Dominator tree:
    If we then construct a tree where each node is a basic block and there
    is an edge from each block's immediate dominator to the block itself, then
    we have a dominator tree.


  [ Note this walker can also walk the post-dominator tree, which is
    defined in a similar manner.  i.e., block B1 is said to post-dominate
    block B2 if all paths from B2 to the exit block must pass through
    B1.  ]

  For example, given the CFG

                   1
                   |
                   2
                  / \
                 3   4
                    / \
       +---------->5   6
       |          / \ /
       |    +--->8   7
       |    |   /    |
       |    +--9    11
       |      /      |
       +--- 10 ---> 12


  We have a dominator tree which looks like

                   1
                   |
                   2
                  / \
                 /   \
                3     4
                   / / \ \
                   | | | |
                   5 6 7 12
                   |   |
                   8   11
                   |
                   9
                   |
                  10



  The dominator tree is the basis for a number of analysis, transformation
  and optimization algorithms that operate on a semi-global basis.

  The dominator walker is a generic routine which visits blocks in the CFG
  via a depth first search of the dominator tree.  In the example above
  the dominator walker might visit blocks in the following order
  1, 2, 3, 4, 5, 8, 9, 10, 6, 7, 11, 12.

  The dominator walker has a number of callbacks to perform actions
  during the walk of the dominator tree.  There are two callbacks
  which walk statements, one before visiting the dominator children,
  one after visiting the dominator children.  There is a callback
  before and after each statement walk callback.  In addition, the
  dominator walker manages allocation/deallocation of data structures
  which are local to each block visited.

  The dominator walker is meant to provide a generic means to build a pass
  which can analyze or transform/optimize a function based on walking
  the dominator tree.  One simply fills in the dominator walker data
  structure with the appropriate callbacks and calls the walker.

  We currently use the dominator walker to prune the set of variables
  which might need PHI nodes (which can greatly improve compile-time
  performance in some cases).

  We also use the dominator walker to rewrite the function into SSA form
  which reduces code duplication since the rewriting phase is inherently
  a walk of the dominator tree.

  And (of course), we use the dominator walker to drive our dominator
  optimizer, which is a semi-global optimizer.

  TODO:

    Walking statements is based on the block statement iterator abstraction,
    which is currently an abstraction over walking tree statements.  Thus
    the dominator walker is currently only useful for trees.  */

/* Reverse postorder index of each basic block.  */
static int *bb_postorder;

static int
cmp_bb_postorder (const void *a, const void *b)
{
  basic_block bb1 = *(const basic_block *)(a);
  basic_block bb2 = *(const basic_block *)(b);
  /* Place higher completion number first (pop off lower number first).  */
  return bb_postorder[bb2->index] - bb_postorder[bb1->index];
}

/* Permute array BBS of N basic blocks in postorder,
   i.e. by descending number in BB_POSTORDER array.  */

static void
sort_bbs_postorder (basic_block *bbs, int n)
{
  if (__builtin_expect (n == 2, true))
    {
      basic_block bb0 = bbs[0], bb1 = bbs[1];
      if (bb_postorder[bb0->index] < bb_postorder[bb1->index])
	bbs[0] = bb1, bbs[1] = bb0;
    }
  else if (__builtin_expect (n == 3, true))
    {
      basic_block bb0 = bbs[0], bb1 = bbs[1], bb2 = bbs[2];
      if (bb_postorder[bb0->index] < bb_postorder[bb1->index])
	std::swap (bb0, bb1);
      if (bb_postorder[bb1->index] < bb_postorder[bb2->index])
	{
	  std::swap (bb1, bb2);
	  if (bb_postorder[bb0->index] < bb_postorder[bb1->index])
	    std::swap (bb0, bb1);
	}
      bbs[0] = bb0, bbs[1] = bb1, bbs[2] = bb2;
    }
  else
    qsort (bbs, n, sizeof *bbs, cmp_bb_postorder);
}

/* Constructor for a dom walker.

   If SKIP_UNREACHBLE_BLOCKS is true, then we need to set
   EDGE_EXECUTABLE on every edge in the CFG. */
dom_walker::dom_walker (cdi_direction direction,
			bool skip_unreachable_blocks)
  : m_dom_direction (direction),
    m_skip_unreachable_blocks (skip_unreachable_blocks),
    m_unreachable_dom (NULL)
{
  /* If we are not skipping unreachable blocks, then there is nothing
     to do.  */
  if (!m_skip_unreachable_blocks)
    return;

  basic_block bb;
  FOR_ALL_BB_FN (bb, cfun)
    {
      edge_iterator ei;
      edge e;
      FOR_EACH_EDGE (e, ei, bb->succs)
	e->flags |= EDGE_EXECUTABLE;
    }
}

/* Return TRUE if BB is reachable, false otherwise.  */

bool
dom_walker::bb_reachable (struct function *fun, basic_block bb)
{
  /* If we're not skipping unreachable blocks, then assume everything
     is reachable.  */
  if (!m_skip_unreachable_blocks)
    return true;

  /* If any of the predecessor edges that do not come from blocks dominated
     by us are still marked as possibly executable consider this block
     reachable.  */
  bool reachable = false;
  if (!m_unreachable_dom)
    {
      reachable = bb == ENTRY_BLOCK_PTR_FOR_FN (fun);
      edge_iterator ei;
      edge e;
      FOR_EACH_EDGE (e, ei, bb->preds)
	if (!dominated_by_p (CDI_DOMINATORS, e->src, bb))
	  reachable |= (e->flags & EDGE_EXECUTABLE);
    }

  return reachable;
}

/* BB has been determined to be unreachable.  Propagate that property
   to incoming and outgoing edges of BB as appropriate.  */

void
dom_walker::propagate_unreachable_to_edges (basic_block bb,
					    FILE *dump_file,
					    dump_flags_t dump_flags)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Marking all outgoing edges of unreachable "
	     "BB %d as not executable\n", bb->index);

  edge_iterator ei;
  edge e;
  FOR_EACH_EDGE (e, ei, bb->succs)
    e->flags &= ~EDGE_EXECUTABLE;

  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      if (dominated_by_p (CDI_DOMINATORS, e->src, bb))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Marking backedge from BB %d into "
		     "unreachable BB %d as not executable\n",
		     e->src->index, bb->index);
	  e->flags &= ~EDGE_EXECUTABLE;
	}
    }

  if (!m_unreachable_dom)
    m_unreachable_dom = bb;
}

/* Recursively walk the dominator tree.
   BB is the basic block we are currently visiting.  */

void
dom_walker::walk (basic_block bb)
{
  basic_block dest;
  basic_block *worklist = XNEWVEC (basic_block,
				   n_basic_blocks_for_fn (cfun) * 2);
  int sp = 0;
  int *postorder, postorder_num;

  if (m_dom_direction == CDI_DOMINATORS)
    {
      postorder = XNEWVEC (int, n_basic_blocks_for_fn (cfun));
      postorder_num = pre_and_rev_post_order_compute (NULL, postorder, true);
      bb_postorder = XNEWVEC (int, last_basic_block_for_fn (cfun));
      for (int i = 0; i < postorder_num; ++i)
	bb_postorder[postorder[i]] = i;
      free (postorder);
    }

  while (true)
    {
      /* Don't worry about unreachable blocks.  */
      if (EDGE_COUNT (bb->preds) > 0
	  || bb == ENTRY_BLOCK_PTR_FOR_FN (cfun)
	  || bb == EXIT_BLOCK_PTR_FOR_FN (cfun))
	{

	  /* Callback for subclasses to do custom things before we have walked
	     the dominator children, but before we walk statements.  */
	  if (this->bb_reachable (cfun, bb))
	    {
	      edge taken_edge = before_dom_children (bb);
	      if (taken_edge)
		{
		  edge_iterator ei;
		  edge e;
		  FOR_EACH_EDGE (e, ei, bb->succs)
		    if (e != taken_edge)
		      e->flags &= ~EDGE_EXECUTABLE;
		}
	    }
	  else
	    propagate_unreachable_to_edges (bb, dump_file, dump_flags);

	  /* Mark the current BB to be popped out of the recursion stack
	     once children are processed.  */
	  worklist[sp++] = bb;
	  worklist[sp++] = NULL;

	  int saved_sp = sp;
	  for (dest = first_dom_son (m_dom_direction, bb);
	       dest; dest = next_dom_son (m_dom_direction, dest))
	    worklist[sp++] = dest;
	  if (sp - saved_sp > 1 && m_dom_direction == CDI_DOMINATORS)
	    sort_bbs_postorder (&worklist[saved_sp], sp - saved_sp);
	}
      /* NULL is used to mark pop operations in the recursion stack.  */
      while (sp > 0 && !worklist[sp - 1])
	{
	  --sp;
	  bb = worklist[--sp];

	  /* Callback allowing subclasses to do custom things after we have
	     walked dominator children, but before we walk statements.  */
	  if (bb_reachable (cfun, bb))
	    after_dom_children (bb);
	  else if (m_unreachable_dom == bb)
	    m_unreachable_dom = NULL;
	}
      if (sp)
	bb = worklist[--sp];
      else
	break;
    }
  if (m_dom_direction == CDI_DOMINATORS)
    {
      free (bb_postorder);
      bb_postorder = NULL;
    }
  free (worklist);
}
