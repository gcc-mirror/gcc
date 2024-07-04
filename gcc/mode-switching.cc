/* CPU mode switching
   Copyright (C) 1998-2024 Free Software Foundation, Inc.

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
#include "target.h"
#include "rtl.h"
#include "cfghooks.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "regs.h"
#include "emit-rtl.h"
#include "cfgrtl.h"
#include "cfganal.h"
#include "lcm.h"
#include "cfgcleanup.h"
#include "tree-pass.h"
#include "cfgbuild.h"

/* We want target macros for the mode switching code to be able to refer
   to instruction attribute values.  */
#include "insn-attr.h"

#ifdef OPTIMIZE_MODE_SWITCHING

/* The algorithm for setting the modes consists of scanning the insn list
   and finding all the insns which require a specific mode.  Each insn gets
   a unique struct seginfo element.  These structures are inserted into a list
   for each basic block.  For each entity, there is an array of bb_info over
   the flow graph basic blocks (local var 'bb_info'), which contains a list
   of all insns within that basic block, in the order they are encountered.

   For each entity, any basic block WITHOUT any insns requiring a specific
   mode are given a single entry without a mode (each basic block in the
   flow graph must have at least one entry in the segment table).

   The LCM algorithm is then run over the flow graph to determine where to
   place the sets to the highest-priority mode with respect to the first
   insn in any one block.  Any adjustments required to the transparency
   vectors are made, then the next iteration starts for the next-lower
   priority mode, till for each entity all modes are exhausted.

   More details can be found in the code of optimize_mode_switching.  */

/* This structure contains the information for each insn which requires
   either single or double mode to be set.
   MODE is the mode this insn must be executed in.
   INSN_PTR is the insn to be executed (may be the note that marks the
   beginning of a basic block).
   NEXT is the next insn in the same basic block.  */
struct seginfo
{
  int prev_mode;
  int mode;
  rtx_insn *insn_ptr;
  struct seginfo *next;
  HARD_REG_SET regs_live;
};

struct bb_info
{
  struct seginfo *seginfo;
  int computing;
  int mode_out;
  int mode_in;
  int single_succ;
};

/* Clear ode I from entity J in bitmap B.  */
#define clear_mode_bit(b, j, i) \
       bitmap_clear_bit (b, (j * max_num_modes) + i)

/* Test mode I from entity J in bitmap B.  */
#define mode_bit_p(b, j, i) \
       bitmap_bit_p (b, (j * max_num_modes) + i)

/* Set mode I from entity J in bitmal B.  */
#define set_mode_bit(b, j, i) \
       bitmap_set_bit (b, (j * max_num_modes) + i)

/* Emit modes segments from EDGE_LIST associated with entity E.
   INFO gives mode availability for each mode.  */

static bool
commit_mode_sets (struct edge_list *edge_list, int e, struct bb_info *info)
{
  bool need_commit = false;

  for (int ed = NUM_EDGES (edge_list) - 1; ed >= 0; ed--)
    {
      edge eg = INDEX_EDGE (edge_list, ed);

      if (eg->aux)
	{
	  int mode = (int) (intptr_t) eg->aux - 1;
	  HARD_REG_SET live_at_edge;
	  basic_block src_bb = eg->src;
	  int cur_mode = info[src_bb->index].mode_out;
	  rtx_insn *mode_set;

	  REG_SET_TO_HARD_REG_SET (live_at_edge, df_get_live_out (src_bb));

	  rtl_profile_for_edge (eg);
	  start_sequence ();

	  targetm.mode_switching.emit (e, mode, cur_mode, live_at_edge);

	  mode_set = get_insns ();
	  end_sequence ();
	  default_rtl_profile ();

	  /* Do not bother to insert empty sequence.  */
	  if (mode_set == NULL)
	    continue;

	  /* We should not get an abnormal edge here.  */
	  gcc_assert (! (eg->flags & EDGE_ABNORMAL));

	  need_commit = true;
	  insert_insn_on_edge (mode_set, eg);
	}
    }

  return need_commit;
}

/* Allocate a new BBINFO structure, initialized with the PREV_MODE, MODE,
   INSN, and REGS_LIVE parameters.
   INSN may not be a NOTE_INSN_BASIC_BLOCK, unless it is an empty
   basic block; that allows us later to insert instructions in a FIFO-like
   manner.  */

static struct seginfo *
new_seginfo (int prev_mode, int mode, rtx_insn *insn,
	     const HARD_REG_SET &regs_live)
{
  struct seginfo *ptr;

  gcc_assert (!NOTE_INSN_BASIC_BLOCK_P (insn)
	      || insn == BB_END (NOTE_BASIC_BLOCK (insn)));
  ptr = XNEW (struct seginfo);
  ptr->prev_mode = prev_mode;
  ptr->mode = mode;
  ptr->insn_ptr = insn;
  ptr->next = NULL;
  ptr->regs_live = regs_live;
  return ptr;
}

/* Add a seginfo element to the end of a list.
   TAIL is a pointer to the list's null terminator.
   INFO is the structure to be linked in.  */

static void
add_seginfo (struct seginfo ***tail_ptr, struct seginfo *info)
{
  **tail_ptr = info;
  *tail_ptr = &info->next;
}

/* Record in LIVE that register REG died.  */

static void
reg_dies (rtx reg, HARD_REG_SET *live)
{
  int regno;

  if (!REG_P (reg))
    return;

  regno = REGNO (reg);
  if (regno < FIRST_PSEUDO_REGISTER)
    remove_from_hard_reg_set (live, GET_MODE (reg), regno);
}

/* Record in LIVE that register REG became live.
   This is called via note_stores.  */

static void
reg_becomes_live (rtx reg, const_rtx setter ATTRIBUTE_UNUSED, void *live)
{
  int regno;

  if (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);

  if (!REG_P (reg))
    return;

  regno = REGNO (reg);
  if (regno < FIRST_PSEUDO_REGISTER)
    add_to_hard_reg_set ((HARD_REG_SET *) live, GET_MODE (reg), regno);
}

/* Split the fallthrough edge to the exit block, so that we can note
   that there NORMAL_MODE is required.  Return the new block if it's
   inserted before the exit block.  Otherwise return null.  */

static basic_block
create_pre_exit (int n_entities, int *entity_map, const int *num_modes)
{
  edge eg;
  edge_iterator ei;
  basic_block pre_exit;

  /* The only non-call predecessor at this stage is a block with a
     fallthrough edge; there can be at most one, but there could be
     none at all, e.g. when exit is called.  */
  pre_exit = 0;
  FOR_EACH_EDGE (eg, ei, EXIT_BLOCK_PTR_FOR_FN (cfun)->preds)
    if (eg->flags & EDGE_FALLTHRU)
      {
	basic_block src_bb = eg->src;
	rtx_insn *last_insn;
	rtx ret_reg;

	gcc_assert (!pre_exit);
	/* If this function returns a value at the end, we have to
	   insert the final mode switch before the return value copy
	   to its hard register.

	   x86 targets use mode-switching infrastructure to
	   conditionally insert vzeroupper instruction at the exit
	   from the function where there is no need to switch the
	   mode before the return value copy.  The vzeroupper insertion
	   pass runs after reload, so use !reload_completed as a stand-in
	   for x86 to skip the search for the return value copy insn.

	   N.b.: the code below assumes that the return copy insn
	   immediately precedes its corresponding use insn.  This
	   assumption does not hold after reload, since sched1 pass
	   can schedule the return copy insn away from its
	   corresponding use insn.  */
	if (!reload_completed
	    && EDGE_COUNT (EXIT_BLOCK_PTR_FOR_FN (cfun)->preds) == 1
	    && NONJUMP_INSN_P ((last_insn = BB_END (src_bb)))
	    && GET_CODE (PATTERN (last_insn)) == USE
	    && GET_CODE ((ret_reg = XEXP (PATTERN (last_insn), 0))) == REG)
	  {
	    auto_bitmap live;
	    df_simulate_initialize_backwards (src_bb, live);

	    int ret_start = REGNO (ret_reg);
	    int nregs = REG_NREGS (ret_reg);
	    int ret_end = ret_start + nregs;
	    bool short_block = false;
	    bool multi_reg_return = false;
	    bool forced_late_switch = false;
	    rtx_insn *before_return_copy;

	    df_simulate_one_insn_backwards (src_bb, last_insn, live);

	    do
	      {
		rtx_insn *return_copy = PREV_INSN (last_insn);
		rtx return_copy_pat, copy_reg;
		int copy_start, copy_num;
		int j;

		df_simulate_one_insn_backwards (src_bb, return_copy, live);

		if (NONDEBUG_INSN_P (return_copy))
		  {
		    /* When using SJLJ exceptions, the call to the
		       unregister function is inserted between the
		       clobber of the return value and the copy.
		       We do not want to split the block before this
		       or any other call; if we have not found the
		       copy yet, the copy must have been deleted.  */
		    if (CALL_P (return_copy))
		      {
			short_block = true;
			break;
		      }
		    return_copy_pat = PATTERN (return_copy);
		    switch (GET_CODE (return_copy_pat))
		      {
		      case USE:
			/* Skip USEs of multiple return registers.
			   __builtin_apply pattern is also handled here.  */
			if (GET_CODE (XEXP (return_copy_pat, 0)) == REG
			    && (targetm.calls.function_value_regno_p
				(REGNO (XEXP (return_copy_pat, 0)))))
			  {
			    multi_reg_return = true;
			    last_insn = return_copy;
			    continue;
			  }
			break;

		      case ASM_OPERANDS:
			/* Skip barrier insns.  */
			if (!MEM_VOLATILE_P (return_copy_pat))
			  break;

			/* Fall through.  */

		      case ASM_INPUT:
		      case UNSPEC_VOLATILE:
			last_insn = return_copy;
			continue;

		      default:
			break;
		      }

		    /* If the return register is not (in its entirety)
		       likely spilled, the return copy might be
		       partially or completely optimized away.  */
		    return_copy_pat = single_set (return_copy);
		    if (!return_copy_pat)
		      {
			return_copy_pat = PATTERN (return_copy);
			if (GET_CODE (return_copy_pat) != CLOBBER)
			  break;
			else if (!optimize)
			  {
			    /* This might be (clobber (reg [<result>]))
			       when not optimizing.  Then check if
			       the previous insn is the clobber for
			       the return register.  */
			    copy_reg = SET_DEST (return_copy_pat);
			    if (GET_CODE (copy_reg) == REG
				&& !HARD_REGISTER_NUM_P (REGNO (copy_reg)))
			      {
				if (INSN_P (PREV_INSN (return_copy)))
				  {
				    return_copy = PREV_INSN (return_copy);
				    return_copy_pat = PATTERN (return_copy);
				    if (GET_CODE (return_copy_pat) != CLOBBER)
				      break;
				  }
			      }
			  }
		      }
		    copy_reg = SET_DEST (return_copy_pat);
		    if (GET_CODE (copy_reg) == REG)
		      copy_start = REGNO (copy_reg);
		    else if (GET_CODE (copy_reg) == SUBREG
			     && GET_CODE (SUBREG_REG (copy_reg)) == REG)
		      copy_start = REGNO (SUBREG_REG (copy_reg));
		    else
		      {
			/* When control reaches end of non-void function,
			   there are no return copy insns at all.  This
			   avoids an ice on that invalid function.  */
			if (ret_start + nregs == ret_end)
			  short_block = true;
			break;
		      }
		    if (!targetm.calls.function_value_regno_p (copy_start))
		      copy_num = 0;
		    else
		      copy_num = hard_regno_nregs (copy_start,
						   GET_MODE (copy_reg));

		    /* If the return register is not likely spilled, - as is
		       the case for floating point on SH4 - then it might
		       be set by an arithmetic operation that needs a
		       different mode than the exit block.  */
		    HARD_REG_SET hard_regs_live;
		    REG_SET_TO_HARD_REG_SET (hard_regs_live, live);
		    for (j = n_entities - 1; j >= 0; j--)
		      {
			int e = entity_map[j];
			int mode =
			  targetm.mode_switching.needed (e, return_copy,
							 hard_regs_live);

			if (mode != num_modes[e]
			    && mode != targetm.mode_switching.exit (e))
			  break;
		      }
		    if (j >= 0)
		      {
			/* __builtin_return emits a sequence of loads to all
			   return registers.  One of them might require
			   another mode than MODE_EXIT, even if it is
			   unrelated to the return value, so we want to put
			   the final mode switch after it.  */
			if (multi_reg_return
			    && targetm.calls.function_value_regno_p
			        (copy_start))
			  forced_late_switch = true;

			/* For the SH4, floating point loads depend on fpscr,
			   thus we might need to put the final mode switch
			   after the return value copy.  That is still OK,
			   because a floating point return value does not
			   conflict with address reloads.  */
			if (copy_start >= ret_start
			    && copy_start + copy_num <= ret_end
			    && GET_CODE (return_copy_pat) == SET
			    && OBJECT_P (SET_SRC (return_copy_pat)))
			  forced_late_switch = true;
			break;
		      }
		    if (copy_num == 0)
		      {
			last_insn = return_copy;
			continue;
		      }

		    if (copy_start >= ret_start
			&& copy_start + copy_num <= ret_end)
		      nregs -= copy_num;
		    else if (!multi_reg_return
			     || !targetm.calls.function_value_regno_p
				 (copy_start))
		      break;
		    last_insn = return_copy;
		  }
		/* ??? Exception handling can lead to the return value
		   copy being already separated from the return value use,
		   as in  unwind-dw2.c .
		   Similarly, conditionally returning without a value,
		   and conditionally using builtin_return can lead to an
		   isolated use.  */
		if (return_copy == BB_HEAD (src_bb))
		  {
		    short_block = true;
		    break;
		  }
		last_insn = return_copy;
	      }
	    while (nregs);

	    /* If we didn't see a full return value copy, verify that there
	       is a plausible reason for this.  If some, but not all of the
	       return register is likely spilled, we can expect that there
	       is a copy for the likely spilled part.  */
	    gcc_assert (!nregs
			|| forced_late_switch
			|| short_block
			|| !(targetm.class_likely_spilled_p
			     (REGNO_REG_CLASS (ret_start)))
			|| nregs != REG_NREGS (ret_reg)
			/* For multi-hard-register floating point
		   	   values, sometimes the likely-spilled part
		   	   is ordinarily copied first, then the other
		   	   part is set with an arithmetic operation.
		   	   This doesn't actually cause reload
		   	   failures, so let it pass.  */
			|| (GET_MODE_CLASS (GET_MODE (ret_reg)) != MODE_INT
			    && nregs != 1));

	    if (!NOTE_INSN_BASIC_BLOCK_P (last_insn))
	      {
		before_return_copy
		  = emit_note_before (NOTE_INSN_DELETED, last_insn);
		/* Instructions preceding LAST_INSN in the same block might
		   require a different mode than MODE_EXIT, so if we might
		   have such instructions, keep them in a separate block
		   from pre_exit.  */
		src_bb = split_block (src_bb,
				      PREV_INSN (before_return_copy))->dest;
	      }
	    else
	      before_return_copy = last_insn;
	    pre_exit = split_block (src_bb, before_return_copy)->src;
	  }
	else
	  {
	    pre_exit = split_edge (eg);
	  }
      }

  return pre_exit;
}

/* Return the confluence of modes MODE1 and MODE2 for entity ENTITY,
   using NO_MODE to represent an unknown mode if nothing more precise
   is available.  */

int
mode_confluence (int entity, int mode1, int mode2, int no_mode)
{
  if (mode1 == mode2)
    return mode1;

  if (mode1 != no_mode
      && mode2 != no_mode
      && targetm.mode_switching.confluence)
    return targetm.mode_switching.confluence (entity, mode1, mode2);

  return no_mode;
}

/* Information for the dataflow problems below.  */
struct
{
  /* Information about each basic block, indexed by block id.  */
  struct bb_info *bb_info;

  /* A bitmap of blocks for which the current entity is transparent.  */
  sbitmap transp;

  /* The entity that we're processing.  */
  int entity;

  /* The number of modes defined for the entity, and thus the identifier
     of the "don't know" mode.  */
  int no_mode;
} confluence_info;

/* Propagate information about any mode change on edge E to the
   destination block's mode_in.  Return true if something changed.

   The mode_in and mode_out fields use no_mode + 1 to mean "not yet set".  */

static bool
forward_confluence_n (edge e)
{
  /* The entry and exit blocks have no useful mode information.  */
  if (e->src->index == ENTRY_BLOCK || e->dest->index == EXIT_BLOCK)
    return false;

  /* We don't control mode changes across abnormal edges.  */
  if (e->flags & EDGE_ABNORMAL)
    return false;

  /* E->aux is nonzero if we have computed the LCM problem and scheduled
     E to change the mode to E->aux - 1.  Otherwise model the change
     from the source to the destination.  */
  struct bb_info *bb_info = confluence_info.bb_info;
  int no_mode = confluence_info.no_mode;
  int src_mode = bb_info[e->src->index].mode_out;
  if (e->aux)
    src_mode = (int) (intptr_t) e->aux - 1;
  if (src_mode == no_mode + 1)
    return false;

  int dest_mode = bb_info[e->dest->index].mode_in;
  if (dest_mode == no_mode + 1)
    {
      bb_info[e->dest->index].mode_in = src_mode;
      return true;
    }

  int entity = confluence_info.entity;
  int new_mode = mode_confluence (entity, src_mode, dest_mode, no_mode);
  if (dest_mode == new_mode)
    return false;

  bb_info[e->dest->index].mode_in = new_mode;
  return true;
}

/* Update block BB_INDEX's mode_out based on its mode_in.  Return true if
   something changed.  */

static bool
forward_transfer (int bb_index)
{
  /* The entry and exit blocks have no useful mode information.  */
  if (bb_index == ENTRY_BLOCK || bb_index == EXIT_BLOCK)
    return false;

  /* Only propagate through a block if the entity is transparent.  */
  struct bb_info *bb_info = confluence_info.bb_info;
  if (bb_info[bb_index].computing != confluence_info.no_mode
      || bb_info[bb_index].mode_out == bb_info[bb_index].mode_in)
    return false;

  bb_info[bb_index].mode_out = bb_info[bb_index].mode_in;
  return true;
}

/* A backwards confluence function.  Update the bb_info single_succ
   field for E's source block, based on changes to E's destination block.
   At the end of the dataflow problem, single_succ is the single mode
   that all successors require (directly or indirectly), or no_mode
   if there are conflicting requirements.

   Initially, a value of no_mode + 1 means "don't know".  */

static bool
single_succ_confluence_n (edge e)
{
  /* The entry block has no associated mode information.  */
  if (e->src->index == ENTRY_BLOCK)
    return false;

  /* We don't control mode changes across abnormal edges.  */
  if (e->flags & EDGE_ABNORMAL)
    return false;

  /* Do nothing if we've already found a conflict.  */
  struct bb_info *bb_info = confluence_info.bb_info;
  int no_mode = confluence_info.no_mode;
  int src_mode = bb_info[e->src->index].single_succ;
  if (src_mode == no_mode)
    return false;

  /* Work out what mode the destination block (or its successors) require.  */
  int dest_mode;
  if (e->dest->index == EXIT_BLOCK)
    dest_mode = no_mode;
  else if (bitmap_bit_p (confluence_info.transp, e->dest->index))
    dest_mode = bb_info[e->dest->index].single_succ;
  else
    dest_mode = bb_info[e->dest->index].seginfo->mode;

  /* Do nothing if the destination block has no new information.  */
  if (dest_mode == no_mode + 1 || dest_mode == src_mode)
    return false;

  /* Detect conflicting modes.  */
  if (src_mode != no_mode + 1)
    dest_mode = no_mode;

  bb_info[e->src->index].single_succ = dest_mode;
  return true;
}

/* A backward transfer function for computing the bb_info single_succ
   fields, as described above single_succ_confluence.  */

static bool
single_succ_transfer (int bb_index)
{
  /* We don't have any field to transfer to.  Assume that, after the
     first iteration, we are only called if single_succ has changed.
     We should then process incoming edges if the entity is transparent.  */
  return bitmap_bit_p (confluence_info.transp, bb_index);
}

/* Check whether the target wants to back-propagate a mode change across
   edge E, and update the source block's computed mode if so.  Return true
   if something changed.  */

static bool
backprop_confluence_n (edge e)
{
  /* The entry and exit blocks have no useful mode information.  */
  if (e->src->index == ENTRY_BLOCK || e->dest->index == EXIT_BLOCK)
    return false;

  /* We don't control mode changes across abnormal edges.  */
  if (e->flags & EDGE_ABNORMAL)
    return false;

  /* We can only require a new mode in the source block if the entity
     was originally transparent there.  */
  if (!bitmap_bit_p (confluence_info.transp, e->src->index))
    return false;

  /* Exit now if there is no required mode, or if all paths into the
     source block leave the entity in the required mode.  */
  struct bb_info *bb_info = confluence_info.bb_info;
  int no_mode = confluence_info.no_mode;
  int src_mode = bb_info[e->src->index].mode_out;
  int dest_mode = bb_info[e->dest->index].mode_in;
  if (dest_mode == no_mode || src_mode == dest_mode)
    return false;

  /* See what the target thinks about this transition.  */
  int entity = confluence_info.entity;
  int new_mode = targetm.mode_switching.backprop (entity, src_mode,
						  dest_mode);
  if (new_mode == no_mode)
    return false;

  /* The target doesn't like the current transition, but would be happy
     with a transition from NEW_MODE.

     If we force the source block to use NEW_MODE, we might introduce a
     double transition on at least one path through the function (one to
     NEW_MODE and then one to DEST_MODE).  Therefore, if all destination
     blocks require the same mode, it is usually better to bring that
     mode requirement forward.

     If that isn't possible, merge the preference for this edge with
     the preferences for other edges.  no_mode + 1 indicates that there
     was no previous preference.  */
  int old_mode = bb_info[e->src->index].computing;
  if (bb_info[e->src->index].single_succ != no_mode)
    new_mode = bb_info[e->src->index].single_succ;
  else if (old_mode != no_mode + 1)
    new_mode = mode_confluence (entity, old_mode, new_mode, no_mode);

  if (old_mode == new_mode)
    return false;

  bb_info[e->src->index].computing = new_mode;
  return true;
}

/* If the current entity was originally transparent in block BB_INDEX,
   update the incoming mode to match the outgoing mode.  Register a mode
   change if the entity is no longer transparent.

   Also, as an on-the-fly optimization, check whether the entity was
   originally transparent in BB_INDEX and if all successor blocks require
   the same mode.  If so, anticipate the mode change in BB_INDEX if
   doing it on the incoming edges would require no more mode changes than
   doing it on the outgoing edges.  The aim is to reduce the total number
   of mode changes emitted for the function (and thus reduce code size and
   cfg complexity) without increasing the number of mode changes on any
   given path through the function.  A typical case where it helps is:

	  T
	 / \
	T   M
	 \ /
	  M

   where the entity is transparent in the T blocks and is required to have
   mode M in the M blocks.  If there are no redundancies leading up to this,
   there will be two mutually-exclusive changes to mode M, one on each of
   the T->M edges.  The optimization instead converts it to:

	  T            T            M
	 / \          / \          / \
	T   M   ->   M   M   ->   M   M
	 \ /          \ /          \ /
	  M            M            M

   which creates a single transition to M for both paths through the diamond.

   Return true if something changed.  */

static bool
backprop_transfer (int bb_index)
{
  /* The entry and exit blocks have no useful mode information.  */
  if (bb_index == ENTRY_BLOCK || bb_index == EXIT_BLOCK)
    return false;

  /* We can only require a new mode if the entity was previously
     transparent.  */
  if (!bitmap_bit_p (confluence_info.transp, bb_index))
    return false;

  struct bb_info *bb_info = confluence_info.bb_info;
  basic_block bb = BASIC_BLOCK_FOR_FN (cfun, bb_index);
  int no_mode = confluence_info.no_mode;
  int mode_in = bb_info[bb_index].mode_in;
  int mode_out = bb_info[bb_index].computing;
  if (mode_out == no_mode + 1)
    {
      /* The entity is still transparent for this block.  See whether
	 all successor blocks need the same mode, either directly or
	 indirectly.  */
      mode_out = bb_info[bb_index].single_succ;
      if (mode_out == no_mode)
	return false;

      /* Get a minimum bound on the number of transitions that would be
	 removed if BB itself required MODE_OUT.  */
      unsigned int moved = 0;
      for (edge e : bb->succs)
	if (e->dest->index != EXIT_BLOCK
	    && mode_out == bb_info[e->dest->index].seginfo->mode)
	  moved += 1;

      /* See whether making the mode change on all incoming edges would
	 be no worse than making it on MOVED outgoing edges.  */
      if (moved < EDGE_COUNT (bb->preds))
	return false;

      bb_info[bb_index].mode_out = mode_out;
      bb_info[bb_index].computing = mode_out;
    }
  else if (mode_out == mode_in)
    return false;

  bb_info[bb_index].mode_in = mode_out;
  bb_info[bb_index].seginfo->mode = mode_out;
  return true;
}

/* Find all insns that need a particular mode setting, and insert the
   necessary mode switches.  Return true if we did work.  */

static int
optimize_mode_switching (void)
{
  int e;
  basic_block bb;
  bool need_commit = false;
  static const int num_modes[] = NUM_MODES_FOR_MODE_SWITCHING;
#define N_ENTITIES ARRAY_SIZE (num_modes)
  int entity_map[N_ENTITIES] = {};
  struct bb_info *bb_info[N_ENTITIES] = {};
  int i, j;
  int n_entities = 0;
  int max_num_modes = 0;
  bool emitted ATTRIBUTE_UNUSED = false;
  basic_block post_entry = 0;
  basic_block pre_exit = 0;
  struct edge_list *edge_list = 0;

  /* These bitmaps are used for the LCM algorithm.  */
  sbitmap *kill, *del, *insert, *antic, *transp, *comp;
  sbitmap *avin, *avout;

  for (e = N_ENTITIES - 1; e >= 0; e--)
    if (OPTIMIZE_MODE_SWITCHING (e))
      {
	int entry_exit_extra = 0;

	/* Create the list of segments within each basic block.
	   If NORMAL_MODE is defined, allow for two extra
	   blocks split from the entry and exit block.  */
	if (targetm.mode_switching.entry && targetm.mode_switching.exit)
	  entry_exit_extra = 3;

	bb_info[n_entities]
	  = XCNEWVEC (struct bb_info,
		      last_basic_block_for_fn (cfun) + entry_exit_extra);
	entity_map[n_entities++] = e;
	if (num_modes[e] > max_num_modes)
	  max_num_modes = num_modes[e];
      }

  if (! n_entities)
    return 0;

  /* Make sure if MODE_ENTRY is defined MODE_EXIT is defined.  */
  gcc_assert ((targetm.mode_switching.entry && targetm.mode_switching.exit)
	      || (!targetm.mode_switching.entry
		  && !targetm.mode_switching.exit));

  if (targetm.mode_switching.entry && targetm.mode_switching.exit)
    {
      /* Split the edge from the entry block, so that we can note that
	 there NORMAL_MODE is supplied.  */
      post_entry = split_edge (single_succ_edge (ENTRY_BLOCK_PTR_FOR_FN (cfun)));
      pre_exit = create_pre_exit (n_entities, entity_map, num_modes);
    }

  df_note_add_problem ();
  df_analyze ();

  /* Create the bitmap vectors.  */
  antic = sbitmap_vector_alloc (last_basic_block_for_fn (cfun),
				n_entities * max_num_modes);
  transp = sbitmap_vector_alloc (last_basic_block_for_fn (cfun),
				 n_entities * max_num_modes);
  comp = sbitmap_vector_alloc (last_basic_block_for_fn (cfun),
			       n_entities * max_num_modes);
  avin = sbitmap_vector_alloc (last_basic_block_for_fn (cfun),
			       n_entities * max_num_modes);
  avout = sbitmap_vector_alloc (last_basic_block_for_fn (cfun),
				n_entities * max_num_modes);
  kill = sbitmap_vector_alloc (last_basic_block_for_fn (cfun),
			       n_entities * max_num_modes);

  bitmap_vector_ones (transp, last_basic_block_for_fn (cfun));
  bitmap_vector_clear (antic, last_basic_block_for_fn (cfun));
  bitmap_vector_clear (comp, last_basic_block_for_fn (cfun));

  auto_sbitmap transp_all (last_basic_block_for_fn (cfun));

  auto_bitmap blocks;

  /* Forward-propagate mode information through blocks where the entity
     is transparent, so that mode_in describes the mode on entry to each
     block and mode_out describes the mode on exit from each block.  */
  auto forwprop_mode_info = [&](struct bb_info *info,
				int entity, int no_mode)
    {
      /* Use no_mode + 1 to mean "not yet set".  */
      FOR_EACH_BB_FN (bb, cfun)
	{
	  if (bb_has_abnormal_pred (bb))
	    info[bb->index].mode_in = info[bb->index].seginfo->mode;
	  else
	    info[bb->index].mode_in = no_mode + 1;
	  if (info[bb->index].computing != no_mode)
	    info[bb->index].mode_out = info[bb->index].computing;
	  else
	    info[bb->index].mode_out = no_mode + 1;
	}

      confluence_info.bb_info = info;
      confluence_info.transp = nullptr;
      confluence_info.entity = entity;
      confluence_info.no_mode = no_mode;

      bitmap_set_range (blocks, 0, last_basic_block_for_fn (cfun));
      df_simple_dataflow (DF_FORWARD, NULL, NULL, forward_confluence_n,
			  forward_transfer, blocks,
			  df_get_postorder (DF_FORWARD),
			  df_get_n_blocks (DF_FORWARD));

    };

  if (targetm.mode_switching.backprop)
    clear_aux_for_edges ();

  for (j = n_entities - 1; j >= 0; j--)
    {
      int e = entity_map[j];
      int no_mode = num_modes[e];
      struct bb_info *info = bb_info[j];
      rtx_insn *insn;

      bitmap_ones (transp_all);

      /* Determine what the first use (if any) need for a mode of entity E is.
	 This will be the mode that is anticipatable for this block.
	 Also compute the initial transparency settings.  */
      FOR_EACH_BB_FN (bb, cfun)
	{
	  struct seginfo **tail_ptr = &info[bb->index].seginfo;
	  struct seginfo *ptr;
	  int last_mode = no_mode;
	  bool any_set_required = false;
	  HARD_REG_SET live_now;

	  info[bb->index].mode_out = info[bb->index].mode_in = no_mode;

	  REG_SET_TO_HARD_REG_SET (live_now, df_get_live_in (bb));

	  /* Pretend the mode is clobbered across abnormal edges.  */
	  {
	    edge_iterator ei;
	    edge eg;
	    FOR_EACH_EDGE (eg, ei, bb->preds)
	      if (eg->flags & EDGE_COMPLEX)
		break;
	    if (eg)
	      {
		rtx_insn *ins_pos = BB_HEAD (bb);
		if (LABEL_P (ins_pos))
		  ins_pos = NEXT_INSN (ins_pos);
		gcc_assert (NOTE_INSN_BASIC_BLOCK_P (ins_pos));
		if (ins_pos != BB_END (bb))
		  ins_pos = NEXT_INSN (ins_pos);
		if (bb_has_eh_pred (bb)
		    && targetm.mode_switching.eh_handler)
		  last_mode = targetm.mode_switching.eh_handler (e);
		ptr = new_seginfo (no_mode, last_mode, ins_pos, live_now);
		add_seginfo (&tail_ptr, ptr);
		bitmap_clear_bit (transp_all, bb->index);
	      }
	  }

	  FOR_BB_INSNS (bb, insn)
	    {
	      if (NONDEBUG_INSN_P (insn))
		{
		  int mode = targetm.mode_switching.needed (e, insn, live_now);
		  rtx link;

		  if (mode != no_mode && mode != last_mode)
		    {
		      ptr = new_seginfo (last_mode, mode, insn, live_now);
		      add_seginfo (&tail_ptr, ptr);
		      bitmap_clear_bit (transp_all, bb->index);
		      any_set_required = true;
		      last_mode = mode;
		    }

		  /* Update LIVE_NOW.  */
		  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
		    if (REG_NOTE_KIND (link) == REG_DEAD)
		      reg_dies (XEXP (link, 0), &live_now);

		  note_stores (insn, reg_becomes_live, &live_now);
		  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
		    if (REG_NOTE_KIND (link) == REG_UNUSED)
		      reg_dies (XEXP (link, 0), &live_now);

		  if (targetm.mode_switching.after)
		    last_mode = targetm.mode_switching.after (e, last_mode,
							      insn, live_now);
		}
	    }

	  info[bb->index].computing = last_mode;
	  /* Check for blocks without ANY mode requirements.
	     N.B. because of MODE_AFTER, last_mode might still
	     be different from no_mode, in which case we need to
	     mark the block as nontransparent.  */
	  if (!any_set_required)
	    {
	      ptr = new_seginfo (last_mode, no_mode, BB_END (bb), live_now);
	      add_seginfo (&tail_ptr, ptr);
	      if (last_mode != no_mode)
		bitmap_clear_bit (transp_all, bb->index);
	    }
	}
      if (targetm.mode_switching.entry && targetm.mode_switching.exit)
	{
	  info[post_entry->index].mode_out =
	    info[post_entry->index].mode_in = no_mode;

	  int mode = targetm.mode_switching.entry (e);
	  if (mode != no_mode)
	    {
	      /* Insert a fake computing definition of MODE into entry
		 blocks which compute no mode. This represents the mode on
		 entry.  */
	      info[post_entry->index].computing = mode;
	      bitmap_clear_bit (transp_all, post_entry->index);
	    }

	  if (pre_exit)
	    {
	      info[pre_exit->index].mode_out =
		info[pre_exit->index].mode_in = no_mode;

	      int mode = targetm.mode_switching.exit (e);
	      if (mode != no_mode)
		{
		  info[pre_exit->index].seginfo->mode = mode;
		  bitmap_clear_bit (transp_all, pre_exit->index);
		}
	    }
	}

      /* If the target requests it, back-propagate selected mode requirements
	 through transparent blocks.  */
      if (targetm.mode_switching.backprop)
	{
	  /* First work out the mode on entry to and exit from each block.  */
	  forwprop_mode_info (info, e, no_mode);

	  /* Compute the single_succ fields, as described above
	     single_succ_confluence.  */
	  FOR_EACH_BB_FN (bb, cfun)
	    info[bb->index].single_succ = no_mode + 1;

	  confluence_info.transp = transp_all;
	  bitmap_set_range (blocks, 0, last_basic_block_for_fn (cfun));
	  df_simple_dataflow (DF_BACKWARD, NULL, NULL,
			      single_succ_confluence_n,
			      single_succ_transfer, blocks,
			      df_get_postorder (DF_BACKWARD),
			      df_get_n_blocks (DF_BACKWARD));

	  FOR_EACH_BB_FN (bb, cfun)
	    {
	      /* Repurpose mode_in as the first mode required by the block,
		 or the output mode if none.  */
	      if (info[bb->index].seginfo->mode != no_mode)
		info[bb->index].mode_in = info[bb->index].seginfo->mode;

	      /* In transparent blocks, use computing == no_mode + 1
		 to indicate that no propagation has taken place.  */
	      if (info[bb->index].computing == no_mode)
		info[bb->index].computing = no_mode + 1;
	    }

	  bitmap_set_range (blocks, 0, last_basic_block_for_fn (cfun));
	  df_simple_dataflow (DF_BACKWARD, NULL, NULL, backprop_confluence_n,
			      backprop_transfer, blocks,
			      df_get_postorder (DF_BACKWARD),
			      df_get_n_blocks (DF_BACKWARD));

	  /* Any block that now computes a mode is no longer transparent.  */
	  FOR_EACH_BB_FN (bb, cfun)
	    if (info[bb->index].computing == no_mode + 1)
	      info[bb->index].computing = no_mode;
	    else if (info[bb->index].computing != no_mode)
	      bitmap_clear_bit (transp_all, bb->index);
	}

      /* Set the anticipatable and computing arrays.  */
      for (i = 0; i < no_mode; i++)
	{
	  int m = targetm.mode_switching.priority (entity_map[j], i);

	  FOR_EACH_BB_FN (bb, cfun)
	    {
	      if (!bitmap_bit_p (transp_all, bb->index))
		clear_mode_bit (transp[bb->index], j, m);

	      if (info[bb->index].seginfo->mode == m)
		set_mode_bit (antic[bb->index], j, m);

	      if (info[bb->index].computing == m)
		set_mode_bit (comp[bb->index], j, m);
	    }
	}
    }

  /* Calculate the optimal locations for the
     placement mode switches to modes with priority I.  */

  FOR_EACH_BB_FN (bb, cfun)
    bitmap_not (kill[bb->index], transp[bb->index]);

  edge_list = pre_edge_lcm_avs (n_entities * max_num_modes, transp, comp, antic,
				kill, avin, avout, &insert, &del);

  auto_sbitmap jumping_blocks (last_basic_block_for_fn (cfun));
  bitmap_clear (jumping_blocks);
  for (j = n_entities - 1; j >= 0; j--)
    {
      int no_mode = num_modes[entity_map[j]];
      struct bb_info *info = bb_info[j];

      /* Insert all mode sets that have been inserted by lcm.  */

      for (int ed = NUM_EDGES (edge_list) - 1; ed >= 0; ed--)
	{
	  edge eg = INDEX_EDGE (edge_list, ed);

	  eg->aux = (void *) (intptr_t) 0;

	  for (i = 0; i < no_mode; i++)
	    {
	      int m = targetm.mode_switching.priority (entity_map[j], i);
	      if (mode_bit_p (insert[ed], j, m))
		{
		  eg->aux = (void *) (intptr_t) (m + 1);
		  break;
		}
	    }
	}

      /* mode_in and mode_out can be calculated directly from avin and
	 avout if all the modes are mutually exclusive.  Use the target-
	 provided confluence function otherwise.  */
      if (targetm.mode_switching.confluence)
	forwprop_mode_info (info, entity_map[j], no_mode);

      FOR_EACH_BB_FN (bb, cfun)
	{
	  auto modes_confluence = [&](sbitmap *av)
	    {
	      for (int i = 0; i < no_mode; ++i)
		if (mode_bit_p (av[bb->index], j, i))
		  {
		    for (int i2 = i + 1; i2 < no_mode; ++i2)
		      if (mode_bit_p (av[bb->index], j, i2))
			return no_mode;
		    return i;
		  }
	      return no_mode;
	    };

	  /* intialize mode in/out availability for bb.  */
	  if (!targetm.mode_switching.confluence)
	    {
	      info[bb->index].mode_out = modes_confluence (avout);
	      info[bb->index].mode_in = modes_confluence (avin);
	    }

	  for (i = 0; i < no_mode; i++)
	    if (mode_bit_p (del[bb->index], j, i))
	      info[bb->index].seginfo->mode = no_mode;

	  /* See whether the target can perform the first transition.
	     If not, push it onto the incoming edges.  The earlier backprop
	     pass should ensure that the resulting transitions are valid.  */
	  if (targetm.mode_switching.backprop)
	    {
	      int from_mode = info[bb->index].mode_in;
	      int to_mode = info[bb->index].seginfo->mode;
	      if (targetm.mode_switching.backprop (entity_map[j], from_mode,
						   to_mode) != no_mode)
		{
		  for (edge e : bb->preds)
		    e->aux = (void *) (intptr_t) (to_mode + 1);
		  info[bb->index].mode_in = to_mode;
		}
	    }
	}

      /* Now output the remaining mode sets in all the segments.  */

      /* In case there was no mode inserted. the mode information on the edge
	 might not be complete.
	 Update mode info on edges and commit pending mode sets.  */
      need_commit |= commit_mode_sets (edge_list, entity_map[j], bb_info[j]);

      /* Reset modes for next entity.  */
      clear_aux_for_edges ();

      FOR_EACH_BB_FN (bb, cfun)
	{
	  struct seginfo *ptr, *next;
	  struct seginfo *first = bb_info[j][bb->index].seginfo;

	  for (ptr = first; ptr; ptr = next)
	    {
	      next = ptr->next;
	      if (ptr->mode != no_mode)
		{
		  rtx_insn *mode_set;

		  rtl_profile_for_bb (bb);
		  start_sequence ();

		  int cur_mode = (ptr == first && ptr->prev_mode == no_mode
				  ? bb_info[j][bb->index].mode_in
				  : ptr->prev_mode);

		  targetm.mode_switching.emit (entity_map[j], ptr->mode,
					       cur_mode, ptr->regs_live);
		  mode_set = get_insns ();
		  end_sequence ();

		  /* Insert MODE_SET only if it is nonempty.  */
		  if (mode_set != NULL_RTX)
		    {
		      for (auto insn = mode_set; insn; insn = NEXT_INSN (insn))
			if (JUMP_P (insn))
			  {
			    rebuild_jump_labels_chain (mode_set);
			    bitmap_set_bit (jumping_blocks, bb->index);
			    break;
			  }
		      emitted = true;
		      if (NOTE_INSN_BASIC_BLOCK_P (ptr->insn_ptr))
			/* We need to emit the insns in a FIFO-like manner,
			   i.e. the first to be emitted at our insertion
			   point ends up first in the instruction steam.
			   Because we made sure that NOTE_INSN_BASIC_BLOCK is
			   only used for initially empty basic blocks, we
			   can achieve this by appending at the end of
			   the block.  */
			emit_insn_after
			  (mode_set, BB_END (NOTE_BASIC_BLOCK (ptr->insn_ptr)));
		      else
			emit_insn_before (mode_set, ptr->insn_ptr);
		    }

		  default_rtl_profile ();
		}

	      free (ptr);
	    }
	}

      free (bb_info[j]);
    }

  free_edge_list (edge_list);

  /* Finished. Free up all the things we've allocated.  */
  sbitmap_vector_free (del);
  sbitmap_vector_free (insert);
  sbitmap_vector_free (kill);
  sbitmap_vector_free (antic);
  sbitmap_vector_free (transp);
  sbitmap_vector_free (comp);
  sbitmap_vector_free (avin);
  sbitmap_vector_free (avout);

  gcc_assert (SBITMAP_SIZE ((sbitmap) jumping_blocks)
	      == (unsigned int) last_basic_block_for_fn (cfun));
  if (!bitmap_empty_p (jumping_blocks))
    find_many_sub_basic_blocks (jumping_blocks);

  if (need_commit)
    commit_edge_insertions ();

  if (targetm.mode_switching.entry && targetm.mode_switching.exit)
    {
      free_dominance_info (CDI_DOMINATORS);
      cleanup_cfg (CLEANUP_NO_INSN_DEL);
    }
  else if (!need_commit && !emitted)
    return 0;

  return 1;
}

#endif /* OPTIMIZE_MODE_SWITCHING */

namespace {

const pass_data pass_data_mode_switching =
{
  RTL_PASS, /* type */
  "mode_sw", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_MODE_SWITCH, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_df_finish, /* todo_flags_finish */
};

class pass_mode_switching : public rtl_opt_pass
{
public:
  pass_mode_switching (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_mode_switching, ctxt)
  {}

  /* opt_pass methods: */
  /* The epiphany backend creates a second instance of this pass, so we need
     a clone method.  */
  opt_pass * clone () final override { return new pass_mode_switching (m_ctxt); }
  bool gate (function *) final override
    {
#ifdef OPTIMIZE_MODE_SWITCHING
      return true;
#else
      return false;
#endif
    }

  unsigned int execute (function *) final override
    {
#ifdef OPTIMIZE_MODE_SWITCHING
      optimize_mode_switching ();
#endif /* OPTIMIZE_MODE_SWITCHING */
      return 0;
    }

}; // class pass_mode_switching

} // anon namespace

rtl_opt_pass *
make_pass_mode_switching (gcc::context *ctxt)
{
  return new pass_mode_switching (ctxt);
}
