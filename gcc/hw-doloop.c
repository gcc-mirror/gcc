/* Code to analyze doloop loops in order for targets to perform late
   optimizations converting doloops to other forms of hardware loops.
   Copyright (C) 2011-2016 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "df.h"
#include "insn-config.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "cfgrtl.h"
#include "hw-doloop.h"
#include "dumpfile.h"

/* Dump information collected in LOOPS.  */
static void
dump_hwloops (hwloop_info loops)
{
  hwloop_info loop;

  for (loop = loops; loop; loop = loop->next)
    {
      hwloop_info i;
      basic_block b;
      unsigned ix;

      fprintf (dump_file, ";; loop %d: ", loop->loop_no);
      if (loop->bad)
	fprintf (dump_file, "(bad) ");
      fprintf (dump_file, "{head:%d, depth:%d, reg:%u}",
	       loop->head == NULL ? -1 : loop->head->index,
	       loop->depth, REGNO (loop->iter_reg));

      fprintf (dump_file, " blocks: [ ");
      for (ix = 0; loop->blocks.iterate (ix, &b); ix++)
	fprintf (dump_file, "%d ", b->index);
      fprintf (dump_file, "] ");

      fprintf (dump_file, " inner loops: [ ");
      for (ix = 0; loop->loops.iterate (ix, &i); ix++)
	fprintf (dump_file, "%d ", i->loop_no);
      fprintf (dump_file, "]\n");
    }
  fprintf (dump_file, "\n");
}

/* Return true if BB is part of LOOP.  */
static bool
bb_in_loop_p (hwloop_info loop, basic_block bb)
{
  return bitmap_bit_p (loop->block_bitmap, bb->index);
}

/* Scan the blocks of LOOP (and its inferiors), and record things such as
   hard registers set, jumps out of and within the loop.  */
static void
scan_loop (hwloop_info loop)
{
  unsigned ix;
  basic_block bb;

  if (loop->bad)
    return;

  if (REGNO_REG_SET_P (df_get_live_in (loop->successor),
		       REGNO (loop->iter_reg)))
    loop->iter_reg_used_outside = true;

  for (ix = 0; loop->blocks.iterate (ix, &bb); ix++)
    {
      rtx_insn *insn;
      edge e;
      edge_iterator ei;

      if (bb != loop->tail)
	FOR_EACH_EDGE (e, ei, bb->succs)
	  {
	    if (bb_in_loop_p (loop, e->dest))
	      {
		if (!(e->flags & EDGE_FALLTHRU))
		  loop->jumps_within = true;
	      }
	    else
	      {
		loop->jumps_outof = true;
		if (!loop->bad)
		  gcc_assert (!REGNO_REG_SET_P (df_get_live_in (e->dest),
						REGNO (loop->iter_reg)));
	      }
	  }

      for (insn = BB_HEAD (bb);
	   insn != NEXT_INSN (BB_END (bb));
	   insn = NEXT_INSN (insn))
	{
	  df_ref def;
	  HARD_REG_SET set_this_insn;

	  if (!NONDEBUG_INSN_P (insn))
	    continue;

	  if (recog_memoized (insn) < 0
	      && (GET_CODE (PATTERN (insn)) == ASM_INPUT
		  || asm_noperands (PATTERN (insn)) >= 0))
	    loop->has_asm = true;

	  CLEAR_HARD_REG_SET (set_this_insn);
	  FOR_EACH_INSN_DEF (def, insn)
	    {
	      rtx dreg = DF_REF_REG (def);

	      if (!REG_P (dreg))
		continue;

	      add_to_hard_reg_set (&set_this_insn, GET_MODE (dreg),
				   REGNO (dreg));
	    }

	  if (insn == loop->loop_end)
	    CLEAR_HARD_REG_BIT (set_this_insn, REGNO (loop->iter_reg));
	  else if (reg_mentioned_p (loop->iter_reg, PATTERN (insn)))
	    loop->iter_reg_used = true;
	  IOR_HARD_REG_SET (loop->regs_set_in_loop, set_this_insn);
	}
    }
}

/* Compute the incoming_dest and incoming_src members of LOOP by
   identifying the edges that jump into the loop.  If there is more
   than one block that jumps into the loop, incoming_src will be set
   to NULL; likewise, if there is more than one block in the loop that
   is the destination of an incoming edge, incoming_dest will be NULL.

   Return true if either of these two fields is nonnull, false
   otherwise.  */
static bool
process_incoming_edges (hwloop_info loop)
{
  edge e;
  edge_iterator ei;
  bool first = true;

  FOR_EACH_EDGE (e, ei, loop->incoming)
    {
      if (first)
	{
	  loop->incoming_src = e->src;
	  loop->incoming_dest = e->dest;
	  first = false;
	}
      else
	{
	  if (e->dest != loop->incoming_dest)
	    loop->incoming_dest = NULL;
	  if (e->src != loop->incoming_src)
	    loop->incoming_src = NULL;
	}
    }
  if (loop->incoming_src == NULL && loop->incoming_dest == NULL)
    return false;

  return true;
}

/* Try to identify a forwarder block that jump into LOOP, and add it to
   the set of blocks in the loop, updating the vector of incoming blocks as
   well.  This transformation gives a second chance to loops we couldn't
   otherwise handle by increasing the chance that we'll end up with one
   incoming_src block.
   Return true if we made a change, false otherwise.  */
static bool
add_forwarder_blocks (hwloop_info loop)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, loop->incoming)
    {
      if (forwarder_block_p (e->src))
	{
	  edge e2;
	  edge_iterator ei2;

	  if (dump_file)
	    fprintf (dump_file,
		     ";; Adding forwarder block %d to loop %d and retrying\n",
		     e->src->index, loop->loop_no);
	  loop->blocks.safe_push (e->src);
	  bitmap_set_bit (loop->block_bitmap, e->src->index);
	  FOR_EACH_EDGE (e2, ei2, e->src->preds)
	    vec_safe_push (loop->incoming, e2);
	  loop->incoming->unordered_remove (ei.index);
	  return true;
	}
    }
  return false;
}

/* Called from reorg_loops when a potential loop end is found.  LOOP is
   a newly set up structure describing the loop, it is this function's
   responsibility to fill most of it.  TAIL_BB and TAIL_INSN point to the
   loop_end insn and its enclosing basic block.  REG is the loop counter
   register.
   For our purposes, a loop is defined by the set of blocks reachable from
   the loop head in which the loop counter register is live.  This matches
   the expected use; targets that call into this code usually replace the
   loop counter with a different special register.  */
static void
discover_loop (hwloop_info loop, basic_block tail_bb, rtx_insn *tail_insn, rtx reg)
{
  bool found_tail;
  unsigned dwork = 0;
  basic_block bb;

  loop->tail = tail_bb;
  loop->loop_end = tail_insn;
  loop->iter_reg = reg;
  vec_alloc (loop->incoming, 2);
  loop->start_label = as_a <rtx_insn *> (JUMP_LABEL (tail_insn));

  if (EDGE_COUNT (tail_bb->succs) != 2)
    {
      loop->bad = true;
      return;
    }
  loop->head = BRANCH_EDGE (tail_bb)->dest;
  loop->successor = FALLTHRU_EDGE (tail_bb)->dest;

  auto_vec<basic_block, 20> works;
  works.safe_push (loop->head);

  found_tail = false;
  for (dwork = 0; works.iterate (dwork, &bb); dwork++)
    {
      edge e;
      edge_iterator ei;
      if (bb == EXIT_BLOCK_PTR_FOR_FN (cfun))
	{
	  /* We've reached the exit block.  The loop must be bad. */
	  if (dump_file)
	    fprintf (dump_file,
		     ";; Loop is bad - reached exit block while scanning\n");
	  loop->bad = true;
	  break;
	}

      if (bitmap_bit_p (loop->block_bitmap, bb->index))
	continue;

      /* We've not seen this block before.  Add it to the loop's
	 list and then add each successor to the work list.  */

      loop->blocks.safe_push (bb);
      bitmap_set_bit (loop->block_bitmap, bb->index);

      if (bb == tail_bb)
	found_tail = true;
      else
	{
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    {
	      basic_block succ = EDGE_SUCC (bb, ei.index)->dest;
	      if (REGNO_REG_SET_P (df_get_live_in (succ),
				   REGNO (loop->iter_reg)))
		works.safe_push (succ);
	    }
	}
    }

  if (!found_tail)
    loop->bad = true;
  
  /* Find the predecessor, and make sure nothing else jumps into this loop.  */
  if (!loop->bad)
    {
      FOR_EACH_VEC_ELT (loop->blocks, dwork, bb)
	{
	  edge e;
	  edge_iterator ei;
	  FOR_EACH_EDGE (e, ei, bb->preds)
	    {
	      basic_block pred = e->src;

	      if (!bb_in_loop_p (loop, pred))
		{
		  if (dump_file)
		    fprintf (dump_file, ";; Loop %d: incoming edge %d -> %d\n",
			     loop->loop_no, pred->index,
			     e->dest->index);
		  vec_safe_push (loop->incoming, e);
		}
	    }
	}

      if (!process_incoming_edges (loop))
	{
	  if (dump_file)
	    fprintf (dump_file,
		     ";; retrying loop %d with forwarder blocks\n",
		     loop->loop_no);
	  if (!add_forwarder_blocks (loop))
	    {
	      if (dump_file)
		fprintf (dump_file, ";; No forwarder blocks found\n");
	      loop->bad = true;
	    }
	  else if (!process_incoming_edges (loop))
	    {
	      if (dump_file)
		fprintf (dump_file,
			 ";; can't find suitable entry for loop %d\n",
			 loop->loop_no);
	    }
	}
    }
}

/* Analyze the structure of the loops in the current function.  Use
   LOOP_STACK for bitmap allocations.  Returns all the valid candidates for
   hardware loops found in this function.  HOOKS is the argument
   passed to reorg_loops, used here to find the iteration registers
   from a loop_end pattern.  */
static hwloop_info
discover_loops (bitmap_obstack *loop_stack, struct hw_doloop_hooks *hooks)
{
  hwloop_info loops = NULL;
  hwloop_info loop;
  basic_block bb;
  int nloops = 0;

  /* Find all the possible loop tails.  This means searching for every
     loop_end instruction.  For each one found, create a hwloop_info
     structure and add the head block to the work list. */
  FOR_EACH_BB_FN (bb, cfun)
    {
      rtx_insn *tail = BB_END (bb);
      rtx_insn *insn;
      rtx reg;

      while (tail && NOTE_P (tail) && tail != BB_HEAD (bb))
	tail = PREV_INSN (tail);

      if (tail == NULL_RTX)
	continue;

      if (!JUMP_P (tail))
	continue;
      reg = hooks->end_pattern_reg (tail);
      if (reg == NULL_RTX)
	continue;

      /* A possible loop end */

      /* There's a degenerate case we can handle - an empty loop consisting
	 of only a back branch.  Handle that by deleting the branch.  */
      insn = JUMP_LABEL_AS_INSN (tail);
      while (insn && !NONDEBUG_INSN_P (insn))
	insn = NEXT_INSN (insn);
      if (insn == tail)
	{
	  basic_block succ = FALLTHRU_EDGE (bb)->dest;
	  if (dump_file)
	    {
	      fprintf (dump_file, ";; degenerate loop ending at\n");
	      print_rtl_single (dump_file, tail);
	    }
	  if (!REGNO_REG_SET_P (df_get_live_in (succ), REGNO (reg)))
	    {
	      if (dump_file)
		fprintf (dump_file, ";; deleting it\n");
	      delete_insn_and_edges (tail);
	      continue;
	    }
	}

      loop = XCNEW (struct hwloop_info_d);
      loop->next = loops;
      loops = loop;
      loop->loop_no = nloops++;
      loop->blocks.create (20);
      loop->block_bitmap = BITMAP_ALLOC (loop_stack);

      if (dump_file)
	{
	  fprintf (dump_file, ";; potential loop %d ending at\n",
		   loop->loop_no);
	  print_rtl_single (dump_file, tail);
	}

      discover_loop (loop, bb, tail, reg);
    }

  /* Compute loop nestings.  Given two loops A and B, either the sets
     of their blocks don't intersect at all, or one is the subset of
     the other, or the blocks don't form a good nesting structure.  */
  for (loop = loops; loop; loop = loop->next)
    {
      hwloop_info other;

      if (loop->bad)
	continue;

      for (other = loops; other; other = other->next)
	{
	  if (other->bad)
	    continue;

	  if (!bitmap_intersect_p (other->block_bitmap, loop->block_bitmap))
	    continue;
	  if (!bitmap_intersect_compl_p (other->block_bitmap,
					 loop->block_bitmap))
	    loop->loops.safe_push (other);
	  else if (!bitmap_intersect_compl_p (loop->block_bitmap,
					      other->block_bitmap))
	    other->loops.safe_push (loop);
	  else
	    {
	      if (dump_file)
		fprintf (dump_file,
			 ";; can't find suitable nesting for loops %d and %d\n",
			 loop->loop_no, other->loop_no);
	      loop->bad = other->bad = true;
	    }
	}
    }

  if (dump_file)
    dump_hwloops (loops);

  return loops;
}

/* Free up the loop structures in LOOPS.  */
static void
free_loops (hwloop_info loops)
{
  while (loops)
    {
      hwloop_info loop = loops;
      loops = loop->next;
      loop->loops.release ();
      loop->blocks.release ();
      BITMAP_FREE (loop->block_bitmap);
      XDELETE (loop);
    }
}

#define BB_AUX_INDEX(BB) ((intptr_t) (BB)->aux)

/* Initialize the aux fields to give ascending indices to basic blocks.  */
static void
set_bb_indices (void)
{
  basic_block bb;
  intptr_t index;

  index = 0;
  FOR_EACH_BB_FN (bb, cfun)
    bb->aux = (void *) index++;
}

/* The taken-branch edge from the loop end can actually go forward.
   If the target's hardware loop support requires that the loop end be
   after the loop start, try to reorder a loop's basic blocks when we
   find such a case.
   This is not very aggressive; it only moves at most one block.  It
   does not introduce new branches into loops; it may remove them, or
   it may switch fallthru/jump edges.  */
static void
reorder_loops (hwloop_info loops)
{
  basic_block bb;
  hwloop_info loop;

  cfg_layout_initialize (0);

  set_bb_indices ();

  for (loop = loops; loop; loop = loop->next)
    {
      edge e;
      edge_iterator ei;

      if (loop->bad)
	continue;

      if (BB_AUX_INDEX (loop->head) <= BB_AUX_INDEX (loop->tail))
	continue;

      FOR_EACH_EDGE (e, ei, loop->head->succs)
	{
	  if (bitmap_bit_p (loop->block_bitmap, e->dest->index)
	      && BB_AUX_INDEX (e->dest) < BB_AUX_INDEX (loop->tail))
	    {
	      basic_block start_bb = e->dest;
	      basic_block start_prev_bb = start_bb->prev_bb;

	      if (dump_file)
		fprintf (dump_file, ";; Moving block %d before block %d\n",
			 loop->head->index, start_bb->index);
	      loop->head->prev_bb->next_bb = loop->head->next_bb;
	      loop->head->next_bb->prev_bb = loop->head->prev_bb;

	      loop->head->prev_bb = start_prev_bb;
	      loop->head->next_bb = start_bb;
	      start_prev_bb->next_bb = start_bb->prev_bb = loop->head;

	      set_bb_indices ();
	      break;
	    }
	}
      loops = loops->next;
    }
  
  FOR_EACH_BB_FN (bb, cfun)
    {
      if (bb->next_bb != EXIT_BLOCK_PTR_FOR_FN (cfun))
	bb->aux = bb->next_bb;
      else
	bb->aux = NULL;
    }
  cfg_layout_finalize ();
  clear_aux_for_blocks ();
  df_analyze ();
}

/* Call the OPT function for LOOP and all of its sub-loops.  This is
   done in a depth-first search; innermost loops are visited first.
   OPTIMIZE and FAIL are the functions passed to reorg_loops by the
   target's reorg pass.  */
static void
optimize_loop (hwloop_info loop, struct hw_doloop_hooks *hooks)
{
  int ix;
  hwloop_info inner;
  int inner_depth = 0;

  if (loop->visited)
    return;

  loop->visited = 1;

  if (loop->bad)
    {
      if (dump_file)
	fprintf (dump_file, ";; loop %d bad when found\n", loop->loop_no);
      goto bad_loop;
    }

  /* Every loop contains in its list of inner loops every loop nested inside
     it, even if there are intermediate loops.  This works because we're doing
     a depth-first search here and never visit a loop more than once.
     Recursion depth is effectively limited by the number of available
     hardware registers.  */
  for (ix = 0; loop->loops.iterate (ix, &inner); ix++)
    {
      optimize_loop (inner, hooks);

      if (!inner->bad && inner_depth < inner->depth)
	inner_depth = inner->depth;
      /* The set of registers may be changed while optimizing the inner
	 loop.  */
      IOR_HARD_REG_SET (loop->regs_set_in_loop, inner->regs_set_in_loop);
    }

  loop->depth = inner_depth + 1;

  if (hooks->opt (loop))
    return;

 bad_loop:
  if (dump_file)
    fprintf (dump_file, ";; loop %d is bad\n", loop->loop_no);

  loop->bad = true;
  hooks->fail (loop);
}

/* This function can be used from a port's machine_dependent_reorg to
   find and analyze loops that end in loop_end instructions.  It uses
   a set of function pointers in HOOKS to call back into the
   target-specific functions to perform the actual machine-specific
   transformations.

   Such transformations typically involve additional set-up
   instructions before the loop, to define loop bounds or set up a
   special loop counter register.

   DO_REORDER should be set to true if we should try to use the
   reorder_loops function to ensure the loop end occurs after the loop
   start.  This is for use by targets where the loop hardware requires
   this condition.

   HOOKS is used to pass in target specific hooks; see
   hw-doloop.h.  */
void
reorg_loops (bool do_reorder, struct hw_doloop_hooks *hooks)
{
  hwloop_info loops = NULL;
  hwloop_info loop;
  bitmap_obstack loop_stack;

  df_live_add_problem ();
  df_live_set_all_dirty ();
  df_analyze ();

  bitmap_obstack_initialize (&loop_stack);

  if (dump_file)
    fprintf (dump_file, ";; Find loops, first pass\n\n");

  loops = discover_loops (&loop_stack, hooks);

  /* We can't enter cfglayout mode anymore if basic block partitioning
     already happened.  */
  if (do_reorder && !flag_reorder_blocks_and_partition)
    {
      reorder_loops (loops);
      free_loops (loops);

      if (dump_file)
	fprintf (dump_file, ";; Find loops, second pass\n\n");

      loops = discover_loops (&loop_stack, hooks);
    }

  for (loop = loops; loop; loop = loop->next)
    scan_loop (loop);

  /* Now apply the optimizations.  */
  for (loop = loops; loop; loop = loop->next)
    optimize_loop (loop, hooks);

  if (dump_file)
    {
      fprintf (dump_file, ";; After hardware loops optimization:\n\n");
      dump_hwloops (loops);
    }

  free_loops (loops);
  bitmap_obstack_release (&loop_stack);

  if (dump_file)
    print_rtl (dump_file, get_insns ());
}
