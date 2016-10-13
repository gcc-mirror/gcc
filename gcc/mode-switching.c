/* CPU mode switching
   Copyright (C) 1998-2016 Free Software Foundation, Inc.

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
   BBNUM is the flow graph basic block this insn occurs in.
   NEXT is the next insn in the same basic block.  */
struct seginfo
{
  int mode;
  rtx_insn *insn_ptr;
  int bbnum;
  struct seginfo *next;
  HARD_REG_SET regs_live;
};

struct bb_info
{
  struct seginfo *seginfo;
  int computing;
  int mode_out;
  int mode_in;
};

static struct seginfo * new_seginfo (int, rtx_insn *, int, HARD_REG_SET);
static void add_seginfo (struct bb_info *, struct seginfo *);
static void reg_dies (rtx, HARD_REG_SET *);
static void reg_becomes_live (rtx, const_rtx, void *);

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
      int mode;

      if ((mode = (int)(intptr_t)(eg->aux)) != -1)
	{
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

/* Allocate a new BBINFO structure, initialized with the MODE, INSN,
   and basic block BB parameters.
   INSN may not be a NOTE_INSN_BASIC_BLOCK, unless it is an empty
   basic block; that allows us later to insert instructions in a FIFO-like
   manner.  */

static struct seginfo *
new_seginfo (int mode, rtx_insn *insn, int bb, HARD_REG_SET regs_live)
{
  struct seginfo *ptr;

  gcc_assert (!NOTE_INSN_BASIC_BLOCK_P (insn)
	      || insn == BB_END (NOTE_BASIC_BLOCK (insn)));
  ptr = XNEW (struct seginfo);
  ptr->mode = mode;
  ptr->insn_ptr = insn;
  ptr->bbnum = bb;
  ptr->next = NULL;
  COPY_HARD_REG_SET (ptr->regs_live, regs_live);
  return ptr;
}

/* Add a seginfo element to the end of a list.
   HEAD is a pointer to the list beginning.
   INFO is the structure to be linked in.  */

static void
add_seginfo (struct bb_info *head, struct seginfo *info)
{
  struct seginfo *ptr;

  if (head->seginfo == NULL)
    head->seginfo = info;
  else
    {
      ptr = head->seginfo;
      while (ptr->next != NULL)
	ptr = ptr->next;
      ptr->next = info;
    }
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
	   to its hard register.  */
	if (EDGE_COUNT (EXIT_BLOCK_PTR_FOR_FN (cfun)->preds) == 1
	    && NONJUMP_INSN_P ((last_insn = BB_END (src_bb)))
	    && GET_CODE (PATTERN (last_insn)) == USE
	    && GET_CODE ((ret_reg = XEXP (PATTERN (last_insn), 0))) == REG)
	  {
	    int ret_start = REGNO (ret_reg);
	    int nregs = REG_NREGS (ret_reg);
	    int ret_end = ret_start + nregs;
	    bool short_block = false;
	    bool multi_reg_return = false;
	    bool forced_late_switch = false;
	    rtx_insn *before_return_copy;

	    do
	      {
		rtx_insn *return_copy = PREV_INSN (last_insn);
		rtx return_copy_pat, copy_reg;
		int copy_start, copy_num;
		int j;

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
		      copy_num
			= hard_regno_nregs[copy_start][GET_MODE (copy_reg)];

		    /* If the return register is not likely spilled, - as is
		       the case for floating point on SH4 - then it might
		       be set by an arithmetic operation that needs a
		       different mode than the exit block.  */
		    for (j = n_entities - 1; j >= 0; j--)
		      {
			int e = entity_map[j];
			int mode =
			  targetm.mode_switching.needed (e, return_copy);

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
			|| (nregs
			    != hard_regno_nregs[ret_start][GET_MODE (ret_reg)])
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
  int entity_map[N_ENTITIES];
  struct bb_info *bb_info[N_ENTITIES];
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

  for (j = n_entities - 1; j >= 0; j--)
    {
      int e = entity_map[j];
      int no_mode = num_modes[e];
      struct bb_info *info = bb_info[j];
      rtx_insn *insn;

      /* Determine what the first use (if any) need for a mode of entity E is.
	 This will be the mode that is anticipatable for this block.
	 Also compute the initial transparency settings.  */
      FOR_EACH_BB_FN (bb, cfun)
	{
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
		ptr = new_seginfo (no_mode, ins_pos, bb->index, live_now);
		add_seginfo (info + bb->index, ptr);
		for (i = 0; i < no_mode; i++)
		  clear_mode_bit (transp[bb->index], j, i);
	      }
	  }

	  FOR_BB_INSNS (bb, insn)
	    {
	      if (INSN_P (insn))
		{
		  int mode = targetm.mode_switching.needed (e, insn);
		  rtx link;

		  if (mode != no_mode && mode != last_mode)
		    {
		      any_set_required = true;
		      last_mode = mode;
		      ptr = new_seginfo (mode, insn, bb->index, live_now);
		      add_seginfo (info + bb->index, ptr);
		      for (i = 0; i < no_mode; i++)
			clear_mode_bit (transp[bb->index], j, i);
		    }

		  if (targetm.mode_switching.after)
		    last_mode = targetm.mode_switching.after (e, last_mode,
							      insn);

		  /* Update LIVE_NOW.  */
		  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
		    if (REG_NOTE_KIND (link) == REG_DEAD)
		      reg_dies (XEXP (link, 0), &live_now);

		  note_stores (PATTERN (insn), reg_becomes_live, &live_now);
		  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
		    if (REG_NOTE_KIND (link) == REG_UNUSED)
		      reg_dies (XEXP (link, 0), &live_now);
		}
	    }

	  info[bb->index].computing = last_mode;
	  /* Check for blocks without ANY mode requirements.
	     N.B. because of MODE_AFTER, last_mode might still
	     be different from no_mode, in which case we need to
	     mark the block as nontransparent.  */
	  if (!any_set_required)
	    {
	      ptr = new_seginfo (no_mode, BB_END (bb), bb->index, live_now);
	      add_seginfo (info + bb->index, ptr);
	      if (last_mode != no_mode)
		for (i = 0; i < no_mode; i++)
		  clear_mode_bit (transp[bb->index], j, i);
	    }
	}
      if (targetm.mode_switching.entry && targetm.mode_switching.exit)
	{
	  int mode = targetm.mode_switching.entry (e);

	  info[post_entry->index].mode_out =
	    info[post_entry->index].mode_in = no_mode;
	  if (pre_exit)
	    {
	      info[pre_exit->index].mode_out =
		info[pre_exit->index].mode_in = no_mode;
	    }

	  if (mode != no_mode)
	    {
	      bb = post_entry;

	      /* By always making this nontransparent, we save
		 an extra check in make_preds_opaque.  We also
		 need this to avoid confusing pre_edge_lcm when
		 antic is cleared but transp and comp are set.  */
	      for (i = 0; i < no_mode; i++)
		clear_mode_bit (transp[bb->index], j, i);

	      /* Insert a fake computing definition of MODE into entry
		 blocks which compute no mode. This represents the mode on
		 entry.  */
	      info[bb->index].computing = mode;

	      if (pre_exit)
		info[pre_exit->index].seginfo->mode =
		  targetm.mode_switching.exit (e);
	    }
	}

      /* Set the anticipatable and computing arrays.  */
      for (i = 0; i < no_mode; i++)
	{
	  int m = targetm.mode_switching.priority (entity_map[j], i);

	  FOR_EACH_BB_FN (bb, cfun)
	    {
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

  for (j = n_entities - 1; j >= 0; j--)
    {
      int no_mode = num_modes[entity_map[j]];

      /* Insert all mode sets that have been inserted by lcm.  */

      for (int ed = NUM_EDGES (edge_list) - 1; ed >= 0; ed--)
	{
	  edge eg = INDEX_EDGE (edge_list, ed);

	  eg->aux = (void *)(intptr_t)-1;

	  for (i = 0; i < no_mode; i++)
	    {
	      int m = targetm.mode_switching.priority (entity_map[j], i);
	      if (mode_bit_p (insert[ed], j, m))
		{
		  eg->aux = (void *)(intptr_t)m;
		  break;
		}
	    }
	}

      FOR_EACH_BB_FN (bb, cfun)
	{
	  struct bb_info *info = bb_info[j];
	  int last_mode = no_mode;

	  /* intialize mode in availability for bb.  */
	  for (i = 0; i < no_mode; i++)
	    if (mode_bit_p (avout[bb->index], j, i))
	      {
		if (last_mode == no_mode)
		  last_mode = i;
		if (last_mode != i)
		  {
		    last_mode = no_mode;
		    break;
		  }
	      }
	  info[bb->index].mode_out = last_mode;

	  /* intialize mode out availability for bb.  */
	  last_mode = no_mode;
	  for (i = 0; i < no_mode; i++)
	    if (mode_bit_p (avin[bb->index], j, i))
	      {
		if (last_mode == no_mode)
		  last_mode = i;
		if (last_mode != i)
		  {
		    last_mode = no_mode;
		    break;
		  }
	      }
	  info[bb->index].mode_in = last_mode;

	  for (i = 0; i < no_mode; i++)
	    if (mode_bit_p (del[bb->index], j, i))
	      info[bb->index].seginfo->mode = no_mode;
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
	  int cur_mode = bb_info[j][bb->index].mode_in;

	  for (ptr = bb_info[j][bb->index].seginfo; ptr; ptr = next)
	    {
	      next = ptr->next;
	      if (ptr->mode != no_mode)
		{
		  rtx_insn *mode_set;

		  rtl_profile_for_bb (bb);
		  start_sequence ();

		  targetm.mode_switching.emit (entity_map[j], ptr->mode,
					       cur_mode, ptr->regs_live);
		  mode_set = get_insns ();
		  end_sequence ();

		  /* modes kill each other inside a basic block.  */
		  cur_mode = ptr->mode;

		  /* Insert MODE_SET only if it is nonempty.  */
		  if (mode_set != NULL_RTX)
		    {
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

  if (need_commit)
    commit_edge_insertions ();

  if (targetm.mode_switching.entry && targetm.mode_switching.exit)
    cleanup_cfg (CLEANUP_NO_INSN_DEL);
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
  opt_pass * clone () { return new pass_mode_switching (m_ctxt); }
  virtual bool gate (function *)
    {
#ifdef OPTIMIZE_MODE_SWITCHING
      return true;
#else
      return false;
#endif
    }

  virtual unsigned int execute (function *)
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
