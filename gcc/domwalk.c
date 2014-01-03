/* Generic dominator tree walker
   Copyright (C) 2003-2014 Free Software Foundation, Inc.
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
#include "tm.h"
#include "basic-block.h"
#include "domwalk.h"
#include "sbitmap.h"

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

static int *bb_postorder;

static int
cmp_bb_postorder (const void *a, const void *b)
{
  basic_block bb1 = *(basic_block *)const_cast<void *>(a);
  basic_block bb2 = *(basic_block *)const_cast<void *>(b);
  if (bb1->index == bb2->index)
    return 0;
  /* Place higher completion number first (pop off lower number first).  */
  if (bb_postorder[bb1->index] > bb_postorder[bb2->index])
    return -1;
  return 1;
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
      postorder_num = inverted_post_order_compute (postorder);
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
	  before_dom_children (bb);

	  /* Mark the current BB to be popped out of the recursion stack
	     once children are processed.  */
	  worklist[sp++] = bb;
	  worklist[sp++] = NULL;

	  int saved_sp = sp;
	  for (dest = first_dom_son (m_dom_direction, bb);
	       dest; dest = next_dom_son (m_dom_direction, dest))
	    worklist[sp++] = dest;
	  if (m_dom_direction == CDI_DOMINATORS)
	    switch (sp - saved_sp)
	      {
	      case 0:
	      case 1:
		break;
	      default:
		qsort (&worklist[saved_sp], sp - saved_sp,
		       sizeof (basic_block), cmp_bb_postorder);
	      }
	}
      /* NULL is used to mark pop operations in the recursion stack.  */
      while (sp > 0 && !worklist[sp - 1])
	{
	  --sp;
	  bb = worklist[--sp];

	  /* Callback allowing subclasses to do custom things after we have
	     walked dominator children, but before we walk statements.  */
	  after_dom_children (bb);
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
