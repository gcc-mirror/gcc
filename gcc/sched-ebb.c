/* Instruction scheduling pass.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
   Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com) Enhanced by,
   and currently maintained by, Jim Wilson (wilson@cygnus.com)

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
#include "output.h"


#ifdef INSN_SCHEDULING

/* The number of insns to be scheduled in total.  */
static int rgn_n_insns;

/* The number of insns scheduled so far.  */
static int sched_rgn_n_insns;

/* Set of blocks, that already have their dependencies calculated.  */
static bitmap_head dont_calc_deps;

/* Last basic block in current ebb.  */
static basic_block last_bb;

/* Implementations of the sched_info functions for region scheduling.  */
static void init_ready_list (void);
static void begin_schedule_ready (rtx, rtx);
static int schedule_more_p (void);
static const char *ebb_print_insn (const_rtx, int);
static int rank (rtx, rtx);
static int ebb_contributes_to_priority (rtx, rtx);
static basic_block earliest_block_with_similiar_load (basic_block, rtx);
static void add_deps_for_risky_insns (rtx, rtx);
static basic_block schedule_ebb (rtx, rtx);
static void debug_ebb_dependencies (rtx, rtx);

static void ebb_add_remove_insn (rtx, int);
static void ebb_add_block (basic_block, basic_block);
static basic_block advance_target_bb (basic_block, rtx);
static void ebb_fix_recovery_cfg (int, int, int);

/* Return nonzero if there are more insns that should be scheduled.  */

static int
schedule_more_p (void)
{
  return sched_rgn_n_insns < rgn_n_insns;
}

/* Print dependency information about ebb between HEAD and TAIL.  */
static void
debug_ebb_dependencies (rtx head, rtx tail)
{
  fprintf (sched_dump,
	   ";;   --------------- forward dependences: ------------ \n");

  fprintf (sched_dump, "\n;;   --- EBB Dependences --- from bb%d to bb%d \n",
	   BLOCK_NUM (head), BLOCK_NUM (tail));

  debug_dependencies (head, tail);
}

/* Add all insns that are initially ready to the ready list READY.  Called
   once before scheduling a set of insns.  */

static void
init_ready_list (void)
{
  int n = 0;
  rtx prev_head = current_sched_info->prev_head;
  rtx next_tail = current_sched_info->next_tail;
  rtx insn;

  sched_rgn_n_insns = 0;

  /* Print debugging information.  */
  if (sched_verbose >= 5)
    debug_ebb_dependencies (NEXT_INSN (prev_head), PREV_INSN (next_tail));

  /* Initialize ready list with all 'ready' insns in target block.
     Count number of insns in the target block being scheduled.  */
  for (insn = NEXT_INSN (prev_head); insn != next_tail; insn = NEXT_INSN (insn))
    {
      try_ready (insn);
      n++;
    }

  gcc_assert (n == rgn_n_insns);
}

/* INSN is being scheduled after LAST.  Update counters.  */
static void
begin_schedule_ready (rtx insn, rtx last)
{
  sched_rgn_n_insns++;

  if (BLOCK_FOR_INSN (insn) == last_bb
      /* INSN is a jump in the last block, ...  */
      && control_flow_insn_p (insn)
      /* that is going to be moved over some instructions.  */
      && last != PREV_INSN (insn))
    {
      edge e;
      edge_iterator ei;
      basic_block bb;

      /* An obscure special case, where we do have partially dead
	 instruction scheduled after last control flow instruction.
	 In this case we can create new basic block.  It is
	 always exactly one basic block last in the sequence.  */
      
      FOR_EACH_EDGE (e, ei, last_bb->succs)
	if (e->flags & EDGE_FALLTHRU)
	  break;

#ifdef ENABLE_CHECKING
      gcc_assert (!e || !(e->flags & EDGE_COMPLEX));	    

      gcc_assert (BLOCK_FOR_INSN (insn) == last_bb
		  && !IS_SPECULATION_CHECK_P (insn)
		  && BB_HEAD (last_bb) != insn
		  && BB_END (last_bb) == insn);

      {
	rtx x;

	x = NEXT_INSN (insn);
	if (e)
	  gcc_assert (NOTE_P (x) || LABEL_P (x));
	else
	  gcc_assert (BARRIER_P (x));
      }
#endif

      if (e)
	{
	  bb = split_edge (e);
	  gcc_assert (NOTE_INSN_BASIC_BLOCK_P (BB_END (bb)));
	}
      else
	/* Create an empty unreachable block after the INSN.  */
	bb = create_basic_block (NEXT_INSN (insn), NULL_RTX, last_bb);
      
      /* split_edge () creates BB before E->DEST.  Keep in mind, that
	 this operation extends scheduling region till the end of BB.
	 Hence, we need to shift NEXT_TAIL, so haifa-sched.c won't go out
	 of the scheduling region.  */
      current_sched_info->next_tail = NEXT_INSN (BB_END (bb));
      gcc_assert (current_sched_info->next_tail);

      /* Append new basic block to the end of the ebb.  */
      sched_init_only_bb (bb, last_bb);
      gcc_assert (last_bb == bb);
    }
}

/* Return a string that contains the insn uid and optionally anything else
   necessary to identify this insn in an output.  It's valid to use a
   static buffer for this.  The ALIGNED parameter should cause the string
   to be formatted so that multiple output lines will line up nicely.  */

static const char *
ebb_print_insn (const_rtx insn, int aligned ATTRIBUTE_UNUSED)
{
  static char tmp[80];

  /* '+' before insn means it is a new cycle start.  */
  if (GET_MODE (insn) == TImode)
    sprintf (tmp, "+ %4d", INSN_UID (insn));
  else
    sprintf (tmp, "  %4d", INSN_UID (insn));

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
ebb_contributes_to_priority (rtx next ATTRIBUTE_UNUSED,
                             rtx insn ATTRIBUTE_UNUSED)
{
  return 1;
}

 /* INSN is a JUMP_INSN, COND_SET is the set of registers that are
    conditionally set before INSN.  Store the set of registers that
    must be considered as used by this jump in USED and that of
    registers that must be considered as set in SET.  */

void
ebb_compute_jump_reg_dependencies (rtx insn, regset cond_set, regset used,
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
      bitmap_and (set, df_get_live_in (e->dest), cond_set);
    else
      bitmap_ior_into (used, df_get_live_in (e->dest));
}

/* Used in schedule_insns to initialize current_sched_info for scheduling
   regions (or single basic blocks).  */

static struct common_sched_info_def ebb_common_sched_info;

static struct sched_deps_info_def ebb_sched_deps_info =
  {
    ebb_compute_jump_reg_dependencies,
    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
    NULL,
    1, 0, 0
  };

static struct haifa_sched_info ebb_sched_info =
{
  init_ready_list,
  NULL,
  schedule_more_p,
  NULL,
  rank,
  ebb_print_insn,
  ebb_contributes_to_priority,

  NULL, NULL,
  NULL, NULL,
  1, 0,

  ebb_add_remove_insn,
  begin_schedule_ready,
  advance_target_bb,
  SCHED_EBB
  /* We can create new blocks in begin_schedule_ready ().  */
  | NEW_BBS
};

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
  sd_iterator_def back_sd_it;
  dep_t back_dep;
  basic_block bb, earliest_block = NULL;

  FOR_EACH_DEP (load_insn, SD_LIST_BACK, back_sd_it, back_dep)
    {
      rtx insn1 = DEP_PRO (back_dep);

      if (DEP_TYPE (back_dep) == REG_DEP_TRUE)	
	/* Found a DEF-USE dependence (insn1, load_insn).  */
	{
	  sd_iterator_def fore_sd_it;
	  dep_t fore_dep;

	  FOR_EACH_DEP (insn1, SD_LIST_FORW, fore_sd_it, fore_dep)
	    {
	      rtx insn2 = DEP_CON (fore_dep);
	      basic_block insn2_block = BLOCK_FOR_INSN (insn2);

	      if (DEP_TYPE (fore_dep) == REG_DEP_TRUE)
		{
		  if (earliest_block != NULL
		      && earliest_block->index < insn2_block->index)
		    continue;

		  /* Found a DEF-USE dependence (insn1, insn2).  */
		  if (haifa_classify_insn (insn2) != PFREE_CANDIDATE)
		    /* insn2 not guaranteed to be a 1 base reg load.  */
		    continue;

		  for (bb = last_block; bb; bb = (basic_block) bb->aux)
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
  int classification;
  rtx last_jump = NULL_RTX;
  rtx next_tail = NEXT_INSN (tail);
  basic_block last_block = NULL, bb;

  for (insn = head; insn != next_tail; insn = NEXT_INSN (insn))
    if (control_flow_insn_p (insn))
      {
	bb = BLOCK_FOR_INSN (insn);
	bb->aux = last_block;
	last_block = bb;
	last_jump = insn;
      }
    else if (INSN_P (insn) && last_jump != NULL_RTX)
      {
	classification = haifa_classify_insn (insn);
	prev = last_jump;
	switch (classification)
	  {
	  case PFREE_CANDIDATE:
	    if (flag_schedule_speculative_load)
	      {
		bb = earliest_block_with_similiar_load (last_block, insn);
		if (bb)
		  {
		    bb = (basic_block) bb->aux;
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
	    if (! sched_insns_conditions_mutex_p (insn, prev))
	      {
		dep_def _dep, *dep = &_dep;

		init_dep (dep, prev, insn, REG_DEP_ANTI);

		if (!(current_sched_info->flags & USE_DEPS_LIST))
		  {
		    enum DEPS_ADJUST_RESULT res;

		    res = sd_add_or_update_dep (dep, false);

		    /* We can't change an existing dependency with
		       DEP_ANTI.  */
		    gcc_assert (res != DEP_CHANGED);
		  }
		else
		  {
		    if ((current_sched_info->flags & DO_SPECULATION)
			&& (spec_info->mask & BEGIN_CONTROL))
		      DEP_STATUS (dep) = set_dep_weak (DEP_ANTI, BEGIN_CONTROL,
						       MAX_DEP_WEAK);

		    sd_add_or_update_dep (dep, false);

		    /* Dep_status could have been changed.
		       No assertion here.  */
		  }
	      }

            break;

          default:
            break;
	  }
      }
  /* Maintain the invariant that bb->aux is clear after use.  */
  while (last_block)
    {
      bb = (basic_block) last_block->aux;
      last_block->aux = NULL;
      last_block = bb;
    }
}

/* Schedule a single extended basic block, defined by the boundaries HEAD
   and TAIL.  */

static basic_block
schedule_ebb (rtx head, rtx tail)
{
  basic_block first_bb, target_bb;
  struct deps tmp_deps;
  
  first_bb = BLOCK_FOR_INSN (head);
  last_bb = BLOCK_FOR_INSN (tail);

  if (no_real_insns_p (head, tail))
    return BLOCK_FOR_INSN (tail);

  gcc_assert (INSN_P (head) && INSN_P (tail));

  if (!bitmap_bit_p (&dont_calc_deps, first_bb->index))
    {
      init_deps_global ();

      /* Compute dependencies.  */
      init_deps (&tmp_deps, false);
      sched_analyze (&tmp_deps, head, tail);
      free_deps (&tmp_deps);

      add_deps_for_risky_insns (head, tail);

      if (targetm.sched.dependencies_evaluation_hook)
        targetm.sched.dependencies_evaluation_hook (head, tail);

      finish_deps_global ();
    }
  else
    /* Only recovery blocks can have their dependencies already calculated,
       and they always are single block ebbs.  */       
    gcc_assert (first_bb == last_bb);

  /* Set priorities.  */
  current_sched_info->sched_max_insns_priority = 0;
  rgn_n_insns = set_priorities (head, tail);
  current_sched_info->sched_max_insns_priority++;

  current_sched_info->prev_head = PREV_INSN (head);
  current_sched_info->next_tail = NEXT_INSN (tail);

  remove_notes (head, tail);

  unlink_bb_notes (first_bb, last_bb);

  target_bb = first_bb;

  /* Make ready list big enough to hold all the instructions from the ebb.  */
  sched_extend_ready_list (rgn_n_insns);
  schedule_block (&target_bb);
  /* Free ready list.  */
  sched_finish_ready_list ();

  /* We might pack all instructions into fewer blocks,
     so we may made some of them empty.  Can't assert (b == last_bb).  */
  
  /* Sanity check: verify that all region insns were scheduled.  */
  gcc_assert (sched_rgn_n_insns == rgn_n_insns);

  /* Free dependencies.  */
  sched_free_deps (current_sched_info->head, current_sched_info->tail, true);

  gcc_assert (haifa_recovery_bb_ever_added_p
	      || deps_pools_are_empty_p ());

  if (EDGE_COUNT (last_bb->preds) == 0)
    /* LAST_BB is unreachable.  */
    {
      gcc_assert (first_bb != last_bb
		  && EDGE_COUNT (last_bb->succs) == 0);
      last_bb = last_bb->prev_bb;
      delete_basic_block (last_bb->next_bb);
    }

  return last_bb;
}

/* The one entry point in this file.  */

void
schedule_ebbs (void)
{
  basic_block bb;
  int probability_cutoff;
  rtx tail;

  if (profile_info && flag_branch_probabilities)
    probability_cutoff = PARAM_VALUE (TRACER_MIN_BRANCH_PROBABILITY_FEEDBACK);
  else
    probability_cutoff = PARAM_VALUE (TRACER_MIN_BRANCH_PROBABILITY);
  probability_cutoff = REG_BR_PROB_BASE / 100 * probability_cutoff;

  /* Taking care of this degenerate case makes the rest of
     this code simpler.  */
  if (n_basic_blocks == NUM_FIXED_BLOCKS)
    return;

  /* Setup infos.  */
  {
    memcpy (&ebb_common_sched_info, &haifa_common_sched_info,
	    sizeof (ebb_common_sched_info));

    ebb_common_sched_info.fix_recovery_cfg = ebb_fix_recovery_cfg;
    ebb_common_sched_info.add_block = ebb_add_block;
    ebb_common_sched_info.sched_pass_id = SCHED_EBB_PASS;

    common_sched_info = &ebb_common_sched_info;
    sched_deps_info = &ebb_sched_deps_info;
    current_sched_info = &ebb_sched_info;
  }

  haifa_sched_init ();

  compute_bb_for_insn ();

  /* Initialize DONT_CALC_DEPS and ebb-{start, end} markers.  */
  bitmap_initialize (&dont_calc_deps, 0);
  bitmap_clear (&dont_calc_deps);

  /* Schedule every region in the subroutine.  */
  FOR_EACH_BB (bb)
    {
      rtx head = BB_HEAD (bb);

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
  bitmap_clear (&dont_calc_deps);

  /* Reposition the prologue and epilogue notes in case we moved the
     prologue/epilogue insns.  */
  if (reload_completed)
    reposition_prologue_and_epilogue_notes ();

  haifa_sched_finish ();
}

/* INSN has been added to/removed from current ebb.  */
static void
ebb_add_remove_insn (rtx insn ATTRIBUTE_UNUSED, int remove_p)
{
  if (!remove_p)
    rgn_n_insns++;
  else
    rgn_n_insns--;
}

/* BB was added to ebb after AFTER.  */
static void
ebb_add_block (basic_block bb, basic_block after)
{
  /* Recovery blocks are always bounded by BARRIERS, 
     therefore, they always form single block EBB,
     therefore, we can use rec->index to identify such EBBs.  */
  if (after == EXIT_BLOCK_PTR)
    bitmap_set_bit (&dont_calc_deps, bb->index);
  else if (after == last_bb)
    last_bb = bb;
}

/* Return next block in ebb chain.  For parameter meaning please refer to
   sched-int.h: struct sched_info: advance_target_bb.  */
static basic_block
advance_target_bb (basic_block bb, rtx insn)
{
  if (insn)
    {
      if (BLOCK_FOR_INSN (insn) != bb
	  && control_flow_insn_p (insn)
	  /* We handle interblock movement of the speculation check
	     or over a speculation check in
	     haifa-sched.c: move_block_after_check ().  */
	  && !IS_SPECULATION_BRANCHY_CHECK_P (insn)
	  && !IS_SPECULATION_BRANCHY_CHECK_P (BB_END (bb)))
	{
	  /* Assert that we don't move jumps across blocks.  */
	  gcc_assert (!control_flow_insn_p (BB_END (bb))
		      && NOTE_INSN_BASIC_BLOCK_P (BB_HEAD (bb->next_bb)));
	  return bb;
	}
      else
	return 0;
    }
  else
    /* Return next non empty block.  */
    {
      do
	{
	  gcc_assert (bb != last_bb);

	  bb = bb->next_bb;
	}
      while (bb_note (bb) == BB_END (bb));

      return bb;
    }
}

/* Fix internal data after interblock movement of jump instruction.
   For parameter meaning please refer to
   sched-int.h: struct sched_info: fix_recovery_cfg.  */
static void
ebb_fix_recovery_cfg (int bbi ATTRIBUTE_UNUSED, int jump_bbi,
		      int jump_bb_nexti)
{
  gcc_assert (last_bb->index != bbi);

  if (jump_bb_nexti == last_bb->index)
    last_bb = BASIC_BLOCK (jump_bbi);
}

#endif /* INSN_SCHEDULING */
