/* Basic block reordering routines for the GNU compiler.
   Copyright (C) 2000, 2002 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to the Free
   Software Foundation, 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.  */

/* References:

   "Profile Guided Code Positioning"
   Pettis and Hanson; PLDI '90.

   TODO:

   (1) Consider:

		if (p) goto A;		// predict taken
		foo ();
	      A:
		if (q) goto B;		// predict taken
		bar ();
	      B:
		baz ();
		return;

       We'll currently reorder this as

		if (!p) goto C;
	      A:
		if (!q) goto D;
	      B:
		baz ();
		return;
	      D:
		bar ();
		goto B;
	      C:
		foo ();
		goto A;

       A better ordering is

		if (!p) goto C;
		if (!q) goto D;
	      B:
		baz ();
		return;
	      C:
		foo ();
		if (q) goto B;
	      D:
		bar ();
		goto B;

       This requires that we be able to duplicate the jump at A, and
       adjust the graph traversal such that greedy placement doesn't
       fix D before C is considered.

   (2) Coordinate with shorten_branches to minimize the number of
       long branches.

   (3) Invent a method by which sufficiently non-predicted code can
       be moved to either the end of the section or another section
       entirely.  Some sort of NOTE_INSN note would work fine.

       This completely scroggs all debugging formats, so the user
       would have to explicitly ask for it.
*/

#include "config.h"
#include "system.h"
#include "tree.h"
#include "rtl.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "flags.h"
#include "output.h"
#include "cfglayout.h"
#include "function.h"
#include "target.h"

/* Local function prototypes.  */
static void make_reorder_chain		PARAMS ((void));
static basic_block make_reorder_chain_1	PARAMS ((basic_block, basic_block));
static basic_block maybe_duplicate_computed_goto_succ PARAMS ((basic_block));

/* Compute an ordering for a subgraph beginning with block BB.  Record the
   ordering in RBI()->index and chained through RBI()->next.  */

static void
make_reorder_chain ()
{
  basic_block prev = NULL;
  basic_block next, bb;

  /* Loop until we've placed every block.  */
  do
    {
      next = NULL;

      /* Find the next unplaced block.  */
      /* ??? Get rid of this loop, and track which blocks are not yet
	 placed more directly, so as to avoid the O(N^2) worst case.
	 Perhaps keep a doubly-linked list of all to-be-placed blocks;
	 remove from the list as we place.  The head of that list is
	 what we're looking for here.  */

      FOR_EACH_BB (bb)
	if (! RBI (bb)->visited)
	  {
	    next = bb;
	    break;
	  }

      if (next)
	prev = make_reorder_chain_1 (next, prev);
    }
  while (next);
  RBI (prev)->next = NULL;
}

/* If the successor is our artificial computed_jump block, duplicate it.  */

static inline basic_block
maybe_duplicate_computed_goto_succ (bb)
     basic_block bb;
{
  edge e;
  basic_block next;

  /* Note that we can't rely on computed_goto_common_label still being in
     the instruction stream -- cfgloop.c likes to munge things about.  But
     we can still use it's non-null-ness to avoid a fruitless search.  */
  if (!cfun->computed_goto_common_label)
    return NULL;

  /* Only want to duplicate when coming from a simple branch.  */
  e = bb->succ;
  if (!e || e->succ_next)
    return NULL;

  /* Only duplicate if we've already layed out this block once.  */
  next = e->dest;
  if (!RBI (next)->visited)
    return NULL;

  /* See if the block contains only a computed branch.  */
  if ((next->head == next->end
       || next_active_insn (next->head) == next->end)
      && computed_jump_p (next->end))
    {
      if (rtl_dump_file)
	fprintf (rtl_dump_file, "Duplicating block %d after %d\n",
		 next->index, bb->index);
      return cfg_layout_duplicate_bb (next, e);
    }

  return NULL;
}

/* A helper function for make_reorder_chain.

   We do not follow EH edges, or non-fallthru edges to noreturn blocks.
   These are assumed to be the error condition and we wish to cluster
   all of them at the very end of the function for the benefit of cache
   locality for the rest of the function.

   ??? We could do slightly better by noticing earlier that some subgraph
   has all paths leading to noreturn functions, but for there to be more
   than one block in such a subgraph is rare.  */

static basic_block
make_reorder_chain_1 (bb, prev)
     basic_block bb;
     basic_block prev;
{
  edge e;
  basic_block next;
  rtx note;

  /* Mark this block visited.  */
  if (prev)
    {
 restart:
      RBI (prev)->next = bb;

      if (rtl_dump_file && prev->next_bb != bb)
	fprintf (rtl_dump_file, "Reordering block %d after %d\n",
		 bb->index, prev->index);
    }
  else
    {
      if (bb->prev_bb != ENTRY_BLOCK_PTR)
	abort ();
    }
  RBI (bb)->visited = 1;
  prev = bb;

  if (bb->succ == NULL)
    return prev;

  /* Find the most probable block.  */

  next = NULL;
  if (any_condjump_p (bb->end)
      && (note = find_reg_note (bb->end, REG_BR_PROB, 0)) != NULL)
    {
      int taken, probability;
      edge e_taken, e_fall;

      probability = INTVAL (XEXP (note, 0));
      taken = probability > REG_BR_PROB_BASE / 2;

      /* Find the normal taken edge and the normal fallthru edge.

	 Note, conditional jumps with other side effects may not
	 be fully optimized.  In this case it is possible for
	 the conditional jump to branch to the same location as
	 the fallthru path.

	 We should probably work to improve optimization of that
	 case; however, it seems silly not to also deal with such
	 problems here if they happen to occur.  */

      e_taken = e_fall = NULL;
      for (e = bb->succ; e ; e = e->succ_next)
	{
	  if (e->flags & EDGE_FALLTHRU)
	    e_fall = e;
	  else if (! (e->flags & EDGE_EH))
	    e_taken = e;
	}

      next = ((taken && e_taken) ? e_taken : e_fall)->dest;
    }

  /* If the successor is our artificial computed_jump block, duplicate it.  */
  else
    next = maybe_duplicate_computed_goto_succ (bb);

  /* In the absence of a prediction, disturb things as little as possible
     by selecting the old "next" block from the list of successors.  If
     there had been a fallthru edge, that will be the one.  */
  /* Note that the fallthru block may not be next any time we eliminate
     forwarder blocks.  */
  if (! next)
    {
      for (e = bb->succ; e ; e = e->succ_next)
	if (e->flags & EDGE_FALLTHRU)
	  {
	    next = e->dest;
	    break;
	  }
	else if (e->dest == bb->next_bb)
	  {
	    if (! (e->flags & (EDGE_ABNORMAL_CALL | EDGE_EH)))
	      next = e->dest;
	  }
    }

  /* Make sure we didn't select a silly next block.  */
  if (! next || next == EXIT_BLOCK_PTR || RBI (next)->visited)
    next = NULL;

  /* Recurse on the successors.  Unroll the last call, as the normal
     case is exactly one or two edges, and we can tail recurse.  */
  for (e = bb->succ; e; e = e->succ_next)
    if (e->dest != EXIT_BLOCK_PTR
	&& ! RBI (e->dest)->visited
	&& e->dest->succ
	&& ! (e->flags & (EDGE_ABNORMAL_CALL | EDGE_EH)))
      {
	if (next)
	  {
	    prev = make_reorder_chain_1 (next, prev);
	    next = RBI (e->dest)->visited ? NULL : e->dest;
	  }
	else
	  next = e->dest;
      }
  if (next)
    {
      bb = next;
      goto restart;
    }

  return prev;
}

/* Reorder basic blocks.  The main entry point to this file.  */

void
reorder_basic_blocks ()
{
  if (n_basic_blocks <= 1)
    return;

  if ((* targetm.cannot_modify_jumps_p) ())
    return;

  cfg_layout_initialize ();

  make_reorder_chain ();

  if (rtl_dump_file)
    dump_flow_info (rtl_dump_file);

  cfg_layout_finalize ();
}
