/* Instruction scheduling pass.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com) Enhanced by,
   and currently maintained by, Jim Wilson (wilson@cygnus.com)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "toplev.h"
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "regs.h"
#include "function.h"
#include "flags.h"
#include "insn-config.h"
#include "insn-attr.h"
#include "except.h"
#include "toplev.h"
#include "recog.h"
#include "cfglayout.h"
#include "params.h"
#include "sched-int.h"
#include "target.h"

/* The number of insns to be scheduled in total.  */
static int target_n_insns;
/* The number of insns scheduled so far.  */
static int sched_n_insns;

/* Implementations of the sched_info functions for region scheduling.  */
static void init_ready_list (struct ready_list *);
static int can_schedule_ready_p (rtx);
static int new_ready (rtx);
static int schedule_more_p (void);
static const char *ebb_print_insn (rtx, int);
static int rank (rtx, rtx);
static int contributes_to_priority (rtx, rtx);
static void compute_jump_reg_dependencies (rtx, regset, regset, regset);
static basic_block earliest_block_with_similiar_load (basic_block, rtx);
static void add_deps_for_risky_insns (rtx, rtx);
static basic_block schedule_ebb (rtx, rtx);
static basic_block fix_basic_block_boundaries (basic_block, basic_block, rtx,
					       rtx);
static void add_missing_bbs (rtx, basic_block, basic_block);

/* Return nonzero if there are more insns that should be scheduled.  */

static int
schedule_more_p (void)
{
  return sched_n_insns < target_n_insns;
}

/* Add all insns that are initially ready to the ready list READY.  Called
   once before scheduling a set of insns.  */

static void
init_ready_list (struct ready_list *ready)
{
  rtx prev_head = current_sched_info->prev_head;
  rtx next_tail = current_sched_info->next_tail;
  rtx insn;

  target_n_insns = 0;
  sched_n_insns = 0;

#if 0
  /* Print debugging information.  */
  if (sched_verbose >= 5)
    debug_dependencies ();
#endif

  /* Initialize ready list with all 'ready' insns in target block.
     Count number of insns in the target block being scheduled.  */
  for (insn = NEXT_INSN (prev_head); insn != next_tail; insn = NEXT_INSN (insn))
    {
      if (INSN_DEP_COUNT (insn) == 0)
	ready_add (ready, insn);
      target_n_insns++;
    }
}

/* Called after taking INSN from the ready list.  Returns nonzero if this
   insn can be scheduled, nonzero if we should silently discard it.  */

static int
can_schedule_ready_p (rtx insn ATTRIBUTE_UNUSED)
{
  sched_n_insns++;
  return 1;
}

/* Called after INSN has all its dependencies resolved.  Return nonzero
   if it should be moved to the ready list or the queue, or zero if we
   should silently discard it.  */
static int
new_ready (rtx next ATTRIBUTE_UNUSED)
{
  return 1;
}

/* Return a string that contains the insn uid and optionally anything else
   necessary to identify this insn in an output.  It's valid to use a
   static buffer for this.  The ALIGNED parameter should cause the string
   to be formatted so that multiple output lines will line up nicely.  */

static const char *
ebb_print_insn (rtx insn, int aligned ATTRIBUTE_UNUSED)
{
  static char tmp[80];

  sprintf (tmp, "%4d", INSN_UID (insn));
  return tmp;
}

/* Compare priority of two insns.  Return a positive number if the second
   insn is to be preferred for scheduling, and a negative one if the first
   is to be preferred.  Zero if they are equally good.  */

static int
rank (rtx insn1, rtx insn2)
{
  basic_block bb1 = BLOCK_FOR_INSN (insn1);
  basic_block bb2 = BLOCK_FOR_INSN (insn2);

  if (bb1->count > bb2->count
      || bb1->frequency > bb2->frequency)
    return -1;
  if (bb1->count < bb2->count
      || bb1->frequency < bb2->frequency)
    return 1;
  return 0;
}

/* NEXT is an instruction that depends on INSN (a backward dependence);
   return nonzero if we should include this dependence in priority
   calculations.  */

static int
contributes_to_priority (rtx next ATTRIBUTE_UNUSED,
			 rtx insn ATTRIBUTE_UNUSED)
{
  return 1;
}

 /* INSN is a JUMP_INSN, COND_SET is the set of registers that are
    conditionally set before INSN.  Store the set of registers that
    must be considered as used by this jump in USED and that of
    registers that must be considered as set in SET.  */

static void
compute_jump_reg_dependencies (rtx insn, regset cond_set, regset used,
			       regset set)
{
  basic_block b = BLOCK_FOR_INSN (insn);
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, b->succs)
    if (e->flags & EDGE_FALLTHRU)
      /* The jump may be a by-product of a branch that has been merged
	 in the main codepath after being conditionalized.  Therefore
	 it may guard the fallthrough block from using a value that has
	 conditionally overwritten that of the main codepath.  So we
	 consider that it restores the value of the main codepath.  */
      bitmap_and (set, e->dest->il.rtl->global_live_at_start, cond_set);
    else
      bitmap_ior_into (used, e->dest->il.rtl->global_live_at_start);
}

/* Used in schedule_insns to initialize current_sched_info for scheduling
   regions (or single basic blocks).  */

static struct sched_info ebb_sched_info =
{
  init_ready_list,
  can_schedule_ready_p,
  schedule_more_p,
  new_ready,
  rank,
  ebb_print_insn,
  contributes_to_priority,
  compute_jump_reg_dependencies,

  NULL, NULL,
  NULL, NULL,
  0, 1, 0
};

/* It is possible that ebb scheduling eliminated some blocks.
   Place blocks from FIRST to LAST before BEFORE.  */

static void
add_missing_bbs (rtx before, basic_block first, basic_block last)
{
  for (; last != first->prev_bb; last = last->prev_bb)
    {
      before = emit_note_before (NOTE_INSN_BASIC_BLOCK, before);
      NOTE_BASIC_BLOCK (before) = last;
      BB_HEAD (last) = before;
      BB_END (last) = before;
      update_bb_for_insn (last);
    }
}

/* Fixup the CFG after EBB scheduling.  Re-recognize the basic
   block boundaries in between HEAD and TAIL and update basic block
   structures between BB and LAST.  */

static basic_block
fix_basic_block_boundaries (basic_block bb, basic_block last, rtx head,
			    rtx tail)
{
  rtx insn = head;
  rtx last_inside = BB_HEAD (bb);
  rtx aftertail = NEXT_INSN (tail);

  head = BB_HEAD (bb);

  for (; insn != aftertail; insn = NEXT_INSN (insn))
    {
      gcc_assert (!LABEL_P (insn));
      /* Create new basic blocks just before first insn.  */
      if (inside_basic_block_p (insn))
	{
	  if (!last_inside)
	    {
	      rtx note;

	      /* Re-emit the basic block note for newly found BB header.  */
	      if (LABEL_P (insn))
		{
		  note = emit_note_after (NOTE_INSN_BASIC_BLOCK, insn);
		  head = insn;
		  last_inside = note;
		}
	      else
		{
		  note = emit_note_before (NOTE_INSN_BASIC_BLOCK, insn);
		  head = note;
		  last_inside = insn;
		}
	    }
	  else
	    last_inside = insn;
	}
      /* Control flow instruction terminate basic block.  It is possible
	 that we've eliminated some basic blocks (made them empty).
	 Find the proper basic block using BLOCK_FOR_INSN and arrange things in
	 a sensible way by inserting empty basic blocks as needed.  */
      if (control_flow_insn_p (insn) || (insn == tail && last_inside))
	{
	  basic_block curr_bb = BLOCK_FOR_INSN (insn);
	  rtx note;

	  if (!control_flow_insn_p (insn))
	    curr_bb = last;
	  if (bb == last->next_bb)
	    {
	      edge f;
	      rtx h;
	      edge_iterator ei;

	      /* An obscure special case, where we do have partially dead
	         instruction scheduled after last control flow instruction.
	         In this case we can create new basic block.  It is
	         always exactly one basic block last in the sequence.  Handle
	         it by splitting the edge and repositioning the block.
	         This is somewhat hackish, but at least avoid cut&paste

	         A safer solution can be to bring the code into sequence,
	         do the split and re-emit it back in case this will ever
	         trigger problem.  */

	      FOR_EACH_EDGE (f, ei, bb->prev_bb->succs)
		if (f->flags & EDGE_FALLTHRU)
		  break;

	      if (f)
		{
		  last = curr_bb = split_edge (f);
		  h = BB_HEAD (curr_bb);
		  BB_HEAD (curr_bb) = head;
		  BB_END (curr_bb) = insn;
		  /* Edge splitting created misplaced BASIC_BLOCK note, kill
		     it.  */
		  delete_insn (h);
		}
	      /* It may happen that code got moved past unconditional jump in
	         case the code is completely dead.  Kill it.  */
	      else
		{
		  rtx next = next_nonnote_insn (insn);
		  delete_insn_chain (head, insn);
		  /* We keep some notes in the way that may split barrier from the
		     jump.  */
		  if (BARRIER_P (next))
		     {
		       emit_barrier_after (prev_nonnote_insn (head));
		       delete_insn (next);
		     }
		  insn = NULL;
		}
	    }
	  else
	    {
	      BB_HEAD (curr_bb) = head;
	      BB_END (curr_bb) = insn;
	      add_missing_bbs (BB_HEAD (curr_bb), bb, curr_bb->prev_bb);
	    }
	  note = LABEL_P (head) ? NEXT_INSN (head) : head;
	  NOTE_BASIC_BLOCK (note) = curr_bb;
	  update_bb_for_insn (curr_bb);
	  bb = curr_bb->next_bb;
	  last_inside = NULL;
	  if (!insn)
	     break;
	}
    }
  add_missing_bbs (BB_HEAD (last->next_bb), bb, last);
  return bb->prev_bb;
}

/* Returns the earliest block in EBB currently being processed where a
   "similar load" 'insn2' is found, and hence LOAD_INSN can move
   speculatively into the found block.  All the following must hold:

   (1) both loads have 1 base register (PFREE_CANDIDATEs).
   (2) load_insn and load2 have a def-use dependence upon
   the same insn 'insn1'.

   From all these we can conclude that the two loads access memory
   addresses that differ at most by a constant, and hence if moving
   load_insn would cause an exception, it would have been caused by
   load2 anyhow.

   The function uses list (given by LAST_BLOCK) of already processed
   blocks in EBB.  The list is formed in `add_deps_for_risky_insns'.  */

static basic_block
earliest_block_with_similiar_load (basic_block last_block, rtx load_insn)
{
  rtx back_link;
  basic_block bb, earliest_block = NULL;

  for (back_link = LOG_LINKS (load_insn);
       back_link;
       back_link = XEXP (back_link, 1))
    {
      rtx insn1 = XEXP (back_link, 0);

      if (GET_MODE (back_link) == VOIDmode)
	{
	  /* Found a DEF-USE dependence (insn1, load_insn).  */
	  rtx fore_link;

	  for (fore_link = INSN_DEPEND (insn1);
	       fore_link;
	       fore_link = XEXP (fore_link, 1))
	    {
	      rtx insn2 = XEXP (fore_link, 0);
	      basic_block insn2_block = BLOCK_FOR_INSN (insn2);

	      if (GET_MODE (fore_link) == VOIDmode)
		{
		  if (earliest_block != NULL
		      && earliest_block->index < insn2_block->index)
		    continue;

		  /* Found a DEF-USE dependence (insn1, insn2).  */
		  if (haifa_classify_insn (insn2) != PFREE_CANDIDATE)
		    /* insn2 not guaranteed to be a 1 base reg load.  */
		    continue;

		  for (bb = last_block; bb; bb = bb->aux)
		    if (insn2_block == bb)
		      break;

		  if (!bb)
		    /* insn2 is the similar load.  */
		    earliest_block = insn2_block;
		}
	    }
	}
    }

  return earliest_block;
}

/* The following function adds dependencies between jumps and risky
   insns in given ebb.  */

static void
add_deps_for_risky_insns (rtx head, rtx tail)
{
  rtx insn, prev;
  int class;
  rtx last_jump = NULL_RTX;
  rtx next_tail = NEXT_INSN (tail);
  basic_block last_block = NULL, bb;

  for (insn = head; insn != next_tail; insn = NEXT_INSN (insn))
    if (JUMP_P (insn))
      {
	bb = BLOCK_FOR_INSN (insn);
	bb->aux = last_block;
	last_block = bb;
	last_jump = insn;
      }
    else if (INSN_P (insn) && last_jump != NULL_RTX)
      {
	class = haifa_classify_insn (insn);
	prev = last_jump;
	switch (class)
	  {
	  case PFREE_CANDIDATE:
	    if (flag_schedule_speculative_load)
	      {
		bb = earliest_block_with_similiar_load (last_block, insn);
		if (bb)
		  {
		    bb = bb->aux;
		    if (!bb)
		      break;
		    prev = BB_END (bb);
		  }
	      }
	    /* Fall through.  */
	  case TRAP_RISKY:
	  case IRISKY:
	  case PRISKY_CANDIDATE:
	    /* ??? We could implement better checking PRISKY_CANDIDATEs
	       analogous to sched-rgn.c.  */
	    /* We can not change the mode of the backward
	       dependency because REG_DEP_ANTI has the lowest
	       rank.  */
	    if (! sched_insns_conditions_mutex_p (insn, prev)
		&& add_dependence (insn, prev, REG_DEP_ANTI))
	      add_forward_dependence (prev, insn, REG_DEP_ANTI);
            break;

          default:
            break;
	  }
      }
  /* Maintain the invariant that bb->aux is clear after use.  */
  while (last_block)
    {
      bb = last_block->aux;
      last_block->aux = NULL;
      last_block = bb;
    }
}

/* Schedule a single extended basic block, defined by the boundaries HEAD
   and TAIL.  */

static basic_block
schedule_ebb (rtx head, rtx tail)
{
  int n_insns;
  basic_block b;
  struct deps tmp_deps;
  basic_block first_bb = BLOCK_FOR_INSN (head);
  basic_block last_bb = BLOCK_FOR_INSN (tail);

  if (no_real_insns_p (head, tail))
    return BLOCK_FOR_INSN (tail);

  init_deps_global ();

  /* Compute LOG_LINKS.  */
  init_deps (&tmp_deps);
  sched_analyze (&tmp_deps, head, tail);
  free_deps (&tmp_deps);

  /* Compute INSN_DEPEND.  */
  compute_forward_dependences (head, tail);

  add_deps_for_risky_insns (head, tail);

  if (targetm.sched.dependencies_evaluation_hook)
    targetm.sched.dependencies_evaluation_hook (head, tail);

  /* Set priorities.  */
  n_insns = set_priorities (head, tail);

  current_sched_info->prev_head = PREV_INSN (head);
  current_sched_info->next_tail = NEXT_INSN (tail);

  if (write_symbols != NO_DEBUG)
    {
      save_line_notes (first_bb->index, head, tail);
      rm_line_notes (head, tail);
    }

  /* rm_other_notes only removes notes which are _inside_ the
     block---that is, it won't remove notes before the first real insn
     or after the last real insn of the block.  So if the first insn
     has a REG_SAVE_NOTE which would otherwise be emitted before the
     insn, it is redundant with the note before the start of the
     block, and so we have to take it out.  */
  if (INSN_P (head))
    {
      rtx note;

      for (note = REG_NOTES (head); note; note = XEXP (note, 1))
	if (REG_NOTE_KIND (note) == REG_SAVE_NOTE)
	  remove_note (head, note);
    }

  /* Remove remaining note insns from the block, save them in
     note_list.  These notes are restored at the end of
     schedule_block ().  */
  rm_other_notes (head, tail);

  current_sched_info->queue_must_finish_empty = 1;

  schedule_block (-1, n_insns);

  /* Sanity check: verify that all region insns were scheduled.  */
  gcc_assert (sched_n_insns == n_insns);
  head = current_sched_info->head;
  tail = current_sched_info->tail;

  if (write_symbols != NO_DEBUG)
    restore_line_notes (head, tail);
  b = fix_basic_block_boundaries (first_bb, last_bb, head, tail);

  finish_deps_global ();
  return b;
}

/* The one entry point in this file.  DUMP_FILE is the dump file for
   this pass.  */

void
schedule_ebbs (FILE *dump_file)
{
  basic_block bb;
  int probability_cutoff;

  if (profile_info && flag_branch_probabilities)
    probability_cutoff = PARAM_VALUE (TRACER_MIN_BRANCH_PROBABILITY_FEEDBACK);
  else
    probability_cutoff = PARAM_VALUE (TRACER_MIN_BRANCH_PROBABILITY);
  probability_cutoff = REG_BR_PROB_BASE / 100 * probability_cutoff;

  /* Taking care of this degenerate case makes the rest of
     this code simpler.  */
  if (n_basic_blocks == 0)
    return;

  sched_init (dump_file);

  current_sched_info = &ebb_sched_info;

  compute_bb_for_insn ();

  /* Schedule every region in the subroutine.  */
  FOR_EACH_BB (bb)
    {
      rtx head = BB_HEAD (bb);
      rtx tail;

      for (;;)
	{
	  edge e;
	  edge_iterator ei;
	  tail = BB_END (bb);
	  if (bb->next_bb == EXIT_BLOCK_PTR
	      || LABEL_P (BB_HEAD (bb->next_bb)))
	    break;
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    if ((e->flags & EDGE_FALLTHRU) != 0)
	      break;
	  if (! e)
	    break;
	  if (e->probability <= probability_cutoff)
	    break;
	  bb = bb->next_bb;
	}

      /* Blah.  We should fix the rest of the code not to get confused by
	 a note or two.  */
      while (head != tail)
	{
	  if (NOTE_P (head))
	    head = NEXT_INSN (head);
	  else if (NOTE_P (tail))
	    tail = PREV_INSN (tail);
	  else if (LABEL_P (head))
	    head = NEXT_INSN (head);
	  else
	    break;
	}

      bb = schedule_ebb (head, tail);
    }

  /* Updating life info can be done by local propagation over the modified
     superblocks.  */

  /* Reposition the prologue and epilogue notes in case we moved the
     prologue/epilogue insns.  */
  if (reload_completed)
    reposition_prologue_and_epilogue_notes (get_insns ());

  if (write_symbols != NO_DEBUG)
    rm_redundant_line_notes ();

  sched_finish ();
}
