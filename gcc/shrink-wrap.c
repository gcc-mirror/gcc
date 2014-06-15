/* Shrink-wrapping related optimizations.
   Copyright (C) 1987-2014 Free Software Foundation, Inc.

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

/* This file handles shrink-wrapping related optimizations.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl-error.h"
#include "tree.h"
#include "stor-layout.h"
#include "varasm.h"
#include "stringpool.h"
#include "flags.h"
#include "except.h"
#include "function.h"
#include "expr.h"
#include "optabs.h"
#include "libfuncs.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "insn-config.h"
#include "recog.h"
#include "output.h"
#include "hashtab.h"
#include "tm_p.h"
#include "langhooks.h"
#include "target.h"
#include "common/common-target.h"
#include "gimple-expr.h"
#include "gimplify.h"
#include "tree-pass.h"
#include "predict.h"
#include "df.h"
#include "params.h"
#include "bb-reorder.h"
#include "shrink-wrap.h"
#include "regcprop.h"

#ifdef HAVE_simple_return

/* Return true if INSN requires the stack frame to be set up.
   PROLOGUE_USED contains the hard registers used in the function
   prologue.  SET_UP_BY_PROLOGUE is the set of registers we expect the
   prologue to set up for the function.  */
bool
requires_stack_frame_p (rtx insn, HARD_REG_SET prologue_used,
			HARD_REG_SET set_up_by_prologue)
{
  df_ref def, use;
  HARD_REG_SET hardregs;
  unsigned regno;

  if (CALL_P (insn))
    return !SIBLING_CALL_P (insn);

  /* We need a frame to get the unique CFA expected by the unwinder.  */
  if (cfun->can_throw_non_call_exceptions && can_throw_internal (insn))
    return true;

  CLEAR_HARD_REG_SET (hardregs);
  FOR_EACH_INSN_DEF (def, insn)
    {
      rtx dreg = DF_REF_REG (def);

      if (!REG_P (dreg))
	continue;

      add_to_hard_reg_set (&hardregs, GET_MODE (dreg),
			   REGNO (dreg));
    }
  if (hard_reg_set_intersect_p (hardregs, prologue_used))
    return true;
  AND_COMPL_HARD_REG_SET (hardregs, call_used_reg_set);
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (TEST_HARD_REG_BIT (hardregs, regno)
	&& df_regs_ever_live_p (regno))
      return true;

  FOR_EACH_INSN_USE (use, insn)
    {
      rtx reg = DF_REF_REG (use);

      if (!REG_P (reg))
	continue;

      add_to_hard_reg_set (&hardregs, GET_MODE (reg),
			   REGNO (reg));
    }
  if (hard_reg_set_intersect_p (hardregs, set_up_by_prologue))
    return true;

  return false;
}

/* See whether there has a single live edge from BB, which dest uses
   [REGNO, END_REGNO).  Return the live edge if its dest bb has
   one or two predecessors.  Otherwise return NULL.  */

static edge
live_edge_for_reg (basic_block bb, int regno, int end_regno)
{
  edge e, live_edge;
  edge_iterator ei;
  bitmap live;
  int i;

  live_edge = NULL;
  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      live = df_get_live_in (e->dest);
      for (i = regno; i < end_regno; i++)
	if (REGNO_REG_SET_P (live, i))
	  {
	    if (live_edge && live_edge != e)
	      return NULL;
	    live_edge = e;
	  }
    }

  /* We can sometimes encounter dead code.  Don't try to move it
     into the exit block.  */
  if (!live_edge || live_edge->dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
    return NULL;

  /* Reject targets of abnormal edges.  This is needed for correctness
     on ports like Alpha and MIPS, whose pic_offset_table_rtx can die on
     exception edges even though it is generally treated as call-saved
     for the majority of the compilation.  Moving across abnormal edges
     isn't going to be interesting for shrink-wrap usage anyway.  */
  if (live_edge->flags & EDGE_ABNORMAL)
    return NULL;

  /* When live_edge->dest->preds == 2, we can create a new block on
     the edge to make it meet the requirement.  */
  if (EDGE_COUNT (live_edge->dest->preds) > 2)
    return NULL;

  return live_edge;
}

/* Try to move INSN from BB to a successor.  Return true on success.
   USES and DEFS are the set of registers that are used and defined
   after INSN in BB.  SPLIT_P indicates whether a live edge from BB
   is splitted or not.  */

static bool
move_insn_for_shrink_wrap (basic_block bb, rtx insn,
			   const HARD_REG_SET uses,
			   const HARD_REG_SET defs,
			   bool *split_p)
{
  rtx set, src, dest;
  bitmap live_out, live_in, bb_uses, bb_defs;
  unsigned int i, dregno, end_dregno, sregno, end_sregno;
  basic_block next_block;
  edge live_edge;

  /* Look for a simple register copy.  */
  set = single_set (insn);
  if (!set)
    return false;
  src = SET_SRC (set);
  dest = SET_DEST (set);
  if (!REG_P (dest) || !REG_P (src)
      /* STACK or FRAME related adjustment might be part of prologue.
	 So keep them in the entry block.  */
      || dest == stack_pointer_rtx
      || dest == frame_pointer_rtx
      || dest == hard_frame_pointer_rtx)
    return false;

  /* Make sure that the source register isn't defined later in BB.  */
  sregno = REGNO (src);
  end_sregno = END_REGNO (src);
  if (overlaps_hard_reg_set_p (defs, GET_MODE (src), sregno))
    return false;

  /* Make sure that the destination register isn't referenced later in BB.  */
  dregno = REGNO (dest);
  end_dregno = END_REGNO (dest);
  if (overlaps_hard_reg_set_p (uses, GET_MODE (dest), dregno)
      || overlaps_hard_reg_set_p (defs, GET_MODE (dest), dregno))
    return false;

  /* See whether there is a successor block to which we could move INSN.  */
  live_edge = live_edge_for_reg (bb, dregno, end_dregno);
  if (!live_edge)
    return false;

  next_block = live_edge->dest;
  /* Create a new basic block on the edge.  */
  if (EDGE_COUNT (next_block->preds) == 2)
    {
      /* split_edge for a block with only one successor is meaningless.  */
      if (EDGE_COUNT (bb->succs) == 1)
	return false;

      /* If DF_LIVE doesn't exist, i.e. at -O1, just give up.  */
      if (!df_live)
	return false;

      next_block = split_edge (live_edge);

      /* We create a new basic block.  Call df_grow_bb_info to make sure
	 all data structures are allocated.  */
      df_grow_bb_info (df_live);
      bitmap_copy (df_get_live_in (next_block), df_get_live_out (bb));
      df_set_bb_dirty (next_block);

      /* We should not split more than once for a function.  */
      gcc_assert (!(*split_p));
      *split_p = true;
    }

  /* At this point we are committed to moving INSN, but let's try to
     move it as far as we can.  */
  do
    {
      live_out = df_get_live_out (bb);
      live_in = df_get_live_in (next_block);
      bb = next_block;

      /* Check whether BB uses DEST or clobbers DEST.  We need to add
	 INSN to BB if so.  Either way, DEST is no longer live on entry,
	 except for any part that overlaps SRC (next loop).  */
      bb_uses = &DF_LR_BB_INFO (bb)->use;
      bb_defs = &DF_LR_BB_INFO (bb)->def;
      if (df_live)
	{
	  for (i = dregno; i < end_dregno; i++)
	    {
	      if (*split_p
		  || REGNO_REG_SET_P (bb_uses, i)
		  || REGNO_REG_SET_P (bb_defs, i)
		  || REGNO_REG_SET_P (&DF_LIVE_BB_INFO (bb)->gen, i))
		next_block = NULL;
	      CLEAR_REGNO_REG_SET (live_out, i);
	      CLEAR_REGNO_REG_SET (live_in, i);
	    }

	  /* Check whether BB clobbers SRC.  We need to add INSN to BB if so.
	     Either way, SRC is now live on entry.  */
	  for (i = sregno; i < end_sregno; i++)
	    {
	      if (*split_p
		  || REGNO_REG_SET_P (bb_defs, i)
		  || REGNO_REG_SET_P (&DF_LIVE_BB_INFO (bb)->gen, i))
		next_block = NULL;
	      SET_REGNO_REG_SET (live_out, i);
	      SET_REGNO_REG_SET (live_in, i);
	    }
	}
      else
	{
	  /* DF_LR_BB_INFO (bb)->def does not comprise the DF_REF_PARTIAL and
	     DF_REF_CONDITIONAL defs.  So if DF_LIVE doesn't exist, i.e.
	     at -O1, just give up searching NEXT_BLOCK.  */
	  next_block = NULL;
	  for (i = dregno; i < end_dregno; i++)
	    {
	      CLEAR_REGNO_REG_SET (live_out, i);
	      CLEAR_REGNO_REG_SET (live_in, i);
	    }

	  for (i = sregno; i < end_sregno; i++)
	    {
	      SET_REGNO_REG_SET (live_out, i);
	      SET_REGNO_REG_SET (live_in, i);
	    }
	}

      /* If we don't need to add the move to BB, look for a single
	 successor block.  */
      if (next_block)
	{
	  live_edge = live_edge_for_reg (next_block, dregno, end_dregno);
	  if (!live_edge || EDGE_COUNT (live_edge->dest->preds) > 1)
	    break;
	  next_block = live_edge->dest;
	}
    }
  while (next_block);

  /* For the new created basic block, there is no dataflow info at all.
     So skip the following dataflow update and check.  */
  if (!(*split_p))
    {
      /* BB now defines DEST.  It only uses the parts of DEST that overlap SRC
	 (next loop).  */
      for (i = dregno; i < end_dregno; i++)
	{
	  CLEAR_REGNO_REG_SET (bb_uses, i);
	  SET_REGNO_REG_SET (bb_defs, i);
	}

      /* BB now uses SRC.  */
      for (i = sregno; i < end_sregno; i++)
	SET_REGNO_REG_SET (bb_uses, i);
    }

  emit_insn_after (PATTERN (insn), bb_note (bb));
  delete_insn (insn);
  return true;
}

/* Look for register copies in the first block of the function, and move
   them down into successor blocks if the register is used only on one
   path.  This exposes more opportunities for shrink-wrapping.  These
   kinds of sets often occur when incoming argument registers are moved
   to call-saved registers because their values are live across one or
   more calls during the function.  */

void
prepare_shrink_wrap (basic_block entry_block)
{
  rtx insn, curr, x;
  HARD_REG_SET uses, defs;
  df_ref def, use;
  bool split_p = false;

  if (JUMP_P (BB_END (entry_block)))
    {
      /* To have more shrink-wrapping opportunities, prepare_shrink_wrap tries
	 to sink the copies from parameter to callee saved register out of
	 entry block.  copyprop_hardreg_forward_bb_without_debug_insn is called
	 to release some dependences.  */
      copyprop_hardreg_forward_bb_without_debug_insn (entry_block);
    }

  CLEAR_HARD_REG_SET (uses);
  CLEAR_HARD_REG_SET (defs);
  FOR_BB_INSNS_REVERSE_SAFE (entry_block, insn, curr)
    if (NONDEBUG_INSN_P (insn)
	&& !move_insn_for_shrink_wrap (entry_block, insn, uses, defs,
				       &split_p))
      {
	/* Add all defined registers to DEFs.  */
	FOR_EACH_INSN_DEF (def, insn)
	  {
	    x = DF_REF_REG (def);
	    if (REG_P (x) && HARD_REGISTER_P (x))
	      SET_HARD_REG_BIT (defs, REGNO (x));
	  }

	/* Add all used registers to USESs.  */
	FOR_EACH_INSN_USE (use, insn)
	  {
	    x = DF_REF_REG (use);
	    if (REG_P (x) && HARD_REGISTER_P (x))
	      SET_HARD_REG_BIT (uses, REGNO (x));
	  }
      }
}

/* Create a copy of BB instructions and insert at BEFORE.  Redirect
   preds of BB to COPY_BB if they don't appear in NEED_PROLOGUE.  */
void
dup_block_and_redirect (basic_block bb, basic_block copy_bb, rtx before,
			bitmap_head *need_prologue)
{
  edge_iterator ei;
  edge e;
  rtx insn = BB_END (bb);

  /* We know BB has a single successor, so there is no need to copy a
     simple jump at the end of BB.  */
  if (simplejump_p (insn))
    insn = PREV_INSN (insn);

  start_sequence ();
  duplicate_insn_chain (BB_HEAD (bb), insn);
  if (dump_file)
    {
      unsigned count = 0;
      for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
	if (active_insn_p (insn))
	  ++count;
      fprintf (dump_file, "Duplicating bb %d to bb %d, %u active insns.\n",
	       bb->index, copy_bb->index, count);
    }
  insn = get_insns ();
  end_sequence ();
  emit_insn_before (insn, before);

  /* Redirect all the paths that need no prologue into copy_bb.  */
  for (ei = ei_start (bb->preds); (e = ei_safe_edge (ei));)
    if (!bitmap_bit_p (need_prologue, e->src->index))
      {
	int freq = EDGE_FREQUENCY (e);
	copy_bb->count += e->count;
	copy_bb->frequency += EDGE_FREQUENCY (e);
	e->dest->count -= e->count;
	if (e->dest->count < 0)
	  e->dest->count = 0;
	e->dest->frequency -= freq;
	if (e->dest->frequency < 0)
	  e->dest->frequency = 0;
	redirect_edge_and_branch_force (e, copy_bb);
	continue;
      }
    else
      ei_next (&ei);
}


/* Try to perform a kind of shrink-wrapping, making sure the
   prologue/epilogue is emitted only around those parts of the
   function that require it.  */

void
try_shrink_wrapping (edge *entry_edge, edge orig_entry_edge,
		     bitmap_head *bb_flags, rtx prologue_seq)
{
  edge e;
  edge_iterator ei;
  bool nonempty_prologue = false;
  unsigned max_grow_size;
  rtx seq;

  for (seq = prologue_seq; seq; seq = NEXT_INSN (seq))
    if (!NOTE_P (seq) || NOTE_KIND (seq) != NOTE_INSN_PROLOGUE_END)
      {
	nonempty_prologue = true;
	break;
      }

  if (flag_shrink_wrap && HAVE_simple_return
      && (targetm.profile_before_prologue () || !crtl->profile)
      && nonempty_prologue && !crtl->calls_eh_return)
    {
      HARD_REG_SET prologue_clobbered, prologue_used, live_on_edge;
      struct hard_reg_set_container set_up_by_prologue;
      rtx p_insn;
      vec<basic_block> vec;
      basic_block bb;
      bitmap_head bb_antic_flags;
      bitmap_head bb_on_list;
      bitmap_head bb_tail;

      if (dump_file)
	fprintf (dump_file, "Attempting shrink-wrapping optimization.\n");

      /* Compute the registers set and used in the prologue.  */
      CLEAR_HARD_REG_SET (prologue_clobbered);
      CLEAR_HARD_REG_SET (prologue_used);
      for (p_insn = prologue_seq; p_insn; p_insn = NEXT_INSN (p_insn))
	{
	  HARD_REG_SET this_used;
	  if (!NONDEBUG_INSN_P (p_insn))
	    continue;

	  CLEAR_HARD_REG_SET (this_used);
	  note_uses (&PATTERN (p_insn), record_hard_reg_uses,
		     &this_used);
	  AND_COMPL_HARD_REG_SET (this_used, prologue_clobbered);
	  IOR_HARD_REG_SET (prologue_used, this_used);
	  note_stores (PATTERN (p_insn), record_hard_reg_sets,
		       &prologue_clobbered);
	}

      prepare_shrink_wrap ((*entry_edge)->dest);

      bitmap_initialize (&bb_antic_flags, &bitmap_default_obstack);
      bitmap_initialize (&bb_on_list, &bitmap_default_obstack);
      bitmap_initialize (&bb_tail, &bitmap_default_obstack);

      /* Find the set of basic blocks that require a stack frame,
	 and blocks that are too big to be duplicated.  */

      vec.create (n_basic_blocks_for_fn (cfun));

      CLEAR_HARD_REG_SET (set_up_by_prologue.set);
      add_to_hard_reg_set (&set_up_by_prologue.set, Pmode,
			   STACK_POINTER_REGNUM);
      add_to_hard_reg_set (&set_up_by_prologue.set, Pmode, ARG_POINTER_REGNUM);
      if (frame_pointer_needed)
	add_to_hard_reg_set (&set_up_by_prologue.set, Pmode,
			     HARD_FRAME_POINTER_REGNUM);
      if (pic_offset_table_rtx)
	add_to_hard_reg_set (&set_up_by_prologue.set, Pmode,
			     PIC_OFFSET_TABLE_REGNUM);
      if (crtl->drap_reg)
	add_to_hard_reg_set (&set_up_by_prologue.set,
			     GET_MODE (crtl->drap_reg),
			     REGNO (crtl->drap_reg));
      if (targetm.set_up_by_prologue)
	targetm.set_up_by_prologue (&set_up_by_prologue);

      /* We don't use a different max size depending on
	 optimize_bb_for_speed_p because increasing shrink-wrapping
	 opportunities by duplicating tail blocks can actually result
	 in an overall decrease in code size.  */
      max_grow_size = get_uncond_jump_length ();
      max_grow_size *= PARAM_VALUE (PARAM_MAX_GROW_COPY_BB_INSNS);

      FOR_EACH_BB_FN (bb, cfun)
	{
	  rtx insn;
	  unsigned size = 0;

	  FOR_BB_INSNS (bb, insn)
	    if (NONDEBUG_INSN_P (insn))
	      {
		if (requires_stack_frame_p (insn, prologue_used,
					    set_up_by_prologue.set))
		  {
		    if (bb == (*entry_edge)->dest)
		      goto fail_shrinkwrap;
		    bitmap_set_bit (bb_flags, bb->index);
		    vec.quick_push (bb);
		    break;
		  }
		else if (size <= max_grow_size)
		  {
		    size += get_attr_min_length (insn);
		    if (size > max_grow_size)
		      bitmap_set_bit (&bb_on_list, bb->index);
		  }
	      }
	}

      /* Blocks that really need a prologue, or are too big for tails.  */
      bitmap_ior_into (&bb_on_list, bb_flags);

      /* For every basic block that needs a prologue, mark all blocks
	 reachable from it, so as to ensure they are also seen as
	 requiring a prologue.  */
      while (!vec.is_empty ())
	{
	  basic_block tmp_bb = vec.pop ();

	  FOR_EACH_EDGE (e, ei, tmp_bb->succs)
	    if (e->dest != EXIT_BLOCK_PTR_FOR_FN (cfun)
		&& bitmap_set_bit (bb_flags, e->dest->index))
	      vec.quick_push (e->dest);
	}

      /* Find the set of basic blocks that need no prologue, have a
	 single successor, can be duplicated, meet a max size
	 requirement, and go to the exit via like blocks.  */
      vec.quick_push (EXIT_BLOCK_PTR_FOR_FN (cfun));
      while (!vec.is_empty ())
	{
	  basic_block tmp_bb = vec.pop ();

	  FOR_EACH_EDGE (e, ei, tmp_bb->preds)
	    if (single_succ_p (e->src)
		&& !bitmap_bit_p (&bb_on_list, e->src->index)
		&& can_duplicate_block_p (e->src))
	      {
		edge pe;
		edge_iterator pei;

		/* If there is predecessor of e->src which doesn't
		   need prologue and the edge is complex,
		   we might not be able to redirect the branch
		   to a copy of e->src.  */
		FOR_EACH_EDGE (pe, pei, e->src->preds)
		  if ((pe->flags & EDGE_COMPLEX) != 0
		      && !bitmap_bit_p (bb_flags, pe->src->index))
		    break;
		if (pe == NULL && bitmap_set_bit (&bb_tail, e->src->index))
		  vec.quick_push (e->src);
	      }
	}

      /* Now walk backwards from every block that is marked as needing
	 a prologue to compute the bb_antic_flags bitmap.  Exclude
	 tail blocks; They can be duplicated to be used on paths not
	 needing a prologue.  */
      bitmap_clear (&bb_on_list);
      bitmap_and_compl (&bb_antic_flags, bb_flags, &bb_tail);
      FOR_EACH_BB_FN (bb, cfun)
	{
	  if (!bitmap_bit_p (&bb_antic_flags, bb->index))
	    continue;
	  FOR_EACH_EDGE (e, ei, bb->preds)
	    if (!bitmap_bit_p (&bb_antic_flags, e->src->index)
		&& bitmap_set_bit (&bb_on_list, e->src->index))
	      vec.quick_push (e->src);
	}
      while (!vec.is_empty ())
	{
	  basic_block tmp_bb = vec.pop ();
	  bool all_set = true;

	  bitmap_clear_bit (&bb_on_list, tmp_bb->index);
	  FOR_EACH_EDGE (e, ei, tmp_bb->succs)
	    if (!bitmap_bit_p (&bb_antic_flags, e->dest->index))
	      {
		all_set = false;
		break;
	      }

	  if (all_set)
	    {
	      bitmap_set_bit (&bb_antic_flags, tmp_bb->index);
	      FOR_EACH_EDGE (e, ei, tmp_bb->preds)
		if (!bitmap_bit_p (&bb_antic_flags, e->src->index)
		    && bitmap_set_bit (&bb_on_list, e->src->index))
		  vec.quick_push (e->src);
	    }
	}
      /* Find exactly one edge that leads to a block in ANTIC from
	 a block that isn't.  */
      if (!bitmap_bit_p (&bb_antic_flags, (*entry_edge)->dest->index))
	FOR_EACH_BB_FN (bb, cfun)
	  {
	    if (!bitmap_bit_p (&bb_antic_flags, bb->index))
	      continue;
	    FOR_EACH_EDGE (e, ei, bb->preds)
	      if (!bitmap_bit_p (&bb_antic_flags, e->src->index))
		{
		  if (*entry_edge != orig_entry_edge)
		    {
		      *entry_edge = orig_entry_edge;
		      if (dump_file)
			fprintf (dump_file, "More than one candidate edge.\n");
		      goto fail_shrinkwrap;
		    }
		  if (dump_file)
		    fprintf (dump_file, "Found candidate edge for "
			     "shrink-wrapping, %d->%d.\n", e->src->index,
			     e->dest->index);
		  *entry_edge = e;
		}
	  }

      if (*entry_edge != orig_entry_edge)
	{
	  /* Test whether the prologue is known to clobber any register
	     (other than FP or SP) which are live on the edge.  */
	  CLEAR_HARD_REG_BIT (prologue_clobbered, STACK_POINTER_REGNUM);
	  if (frame_pointer_needed)
	    CLEAR_HARD_REG_BIT (prologue_clobbered, HARD_FRAME_POINTER_REGNUM);
	  REG_SET_TO_HARD_REG_SET (live_on_edge,
				   df_get_live_in ((*entry_edge)->dest));
	  if (hard_reg_set_intersect_p (live_on_edge, prologue_clobbered))
	    {
	      *entry_edge = orig_entry_edge;
	      if (dump_file)
		fprintf (dump_file,
			 "Shrink-wrapping aborted due to clobber.\n");
	    }
	}
      if (*entry_edge != orig_entry_edge)
	{
	  crtl->shrink_wrapped = true;
	  if (dump_file)
	    fprintf (dump_file, "Performing shrink-wrapping.\n");

	  /* Find tail blocks reachable from both blocks needing a
	     prologue and blocks not needing a prologue.  */
	  if (!bitmap_empty_p (&bb_tail))
	    FOR_EACH_BB_FN (bb, cfun)
	      {
		bool some_pro, some_no_pro;
		if (!bitmap_bit_p (&bb_tail, bb->index))
		  continue;
		some_pro = some_no_pro = false;
		FOR_EACH_EDGE (e, ei, bb->preds)
		  {
		    if (bitmap_bit_p (bb_flags, e->src->index))
		      some_pro = true;
		    else
		      some_no_pro = true;
		  }
		if (some_pro && some_no_pro)
		  vec.quick_push (bb);
		else
		  bitmap_clear_bit (&bb_tail, bb->index);
	      }
	  /* Find the head of each tail.  */
	  while (!vec.is_empty ())
	    {
	      basic_block tbb = vec.pop ();

	      if (!bitmap_bit_p (&bb_tail, tbb->index))
		continue;

	      while (single_succ_p (tbb))
		{
		  tbb = single_succ (tbb);
		  bitmap_clear_bit (&bb_tail, tbb->index);
		}
	    }
	  /* Now duplicate the tails.  */
	  if (!bitmap_empty_p (&bb_tail))
	    FOR_EACH_BB_REVERSE_FN (bb, cfun)
	      {
		basic_block copy_bb, tbb;
		rtx insert_point;
		int eflags;

		if (!bitmap_clear_bit (&bb_tail, bb->index))
		  continue;

		/* Create a copy of BB, instructions and all, for
		   use on paths that don't need a prologue.
		   Ideal placement of the copy is on a fall-thru edge
		   or after a block that would jump to the copy.  */
		FOR_EACH_EDGE (e, ei, bb->preds)
		  if (!bitmap_bit_p (bb_flags, e->src->index)
		      && single_succ_p (e->src))
		    break;
		if (e)
		  {
                    /* Make sure we insert after any barriers.  */
                    rtx end = get_last_bb_insn (e->src);
                    copy_bb = create_basic_block (NEXT_INSN (end),
                                                  NULL_RTX, e->src);
		    BB_COPY_PARTITION (copy_bb, e->src);
		  }
		else
		  {
		    /* Otherwise put the copy at the end of the function.  */
		    copy_bb = create_basic_block (NULL_RTX, NULL_RTX,
						  EXIT_BLOCK_PTR_FOR_FN (cfun)->prev_bb);
		    BB_COPY_PARTITION (copy_bb, bb);
		  }

		insert_point = emit_note_after (NOTE_INSN_DELETED,
						BB_END (copy_bb));
		emit_barrier_after (BB_END (copy_bb));

		tbb = bb;
		while (1)
		  {
		    dup_block_and_redirect (tbb, copy_bb, insert_point,
					    bb_flags);
		    tbb = single_succ (tbb);
		    if (tbb == EXIT_BLOCK_PTR_FOR_FN (cfun))
		      break;
		    e = split_block (copy_bb, PREV_INSN (insert_point));
		    copy_bb = e->dest;
		  }

		/* Quiet verify_flow_info by (ab)using EDGE_FAKE.
		   We have yet to add a simple_return to the tails,
		   as we'd like to first convert_jumps_to_returns in
		   case the block is no longer used after that.  */
		eflags = EDGE_FAKE;
		if (CALL_P (PREV_INSN (insert_point))
		    && SIBLING_CALL_P (PREV_INSN (insert_point)))
		  eflags = EDGE_SIBCALL | EDGE_ABNORMAL;
		make_single_succ_edge (copy_bb, EXIT_BLOCK_PTR_FOR_FN (cfun),
				       eflags);

		/* verify_flow_info doesn't like a note after a
		   sibling call.  */
		delete_insn (insert_point);
		if (bitmap_empty_p (&bb_tail))
		  break;
	      }
	}

    fail_shrinkwrap:
      bitmap_clear (&bb_tail);
      bitmap_clear (&bb_antic_flags);
      bitmap_clear (&bb_on_list);
      vec.release ();
    }
}

/* If we're allowed to generate a simple return instruction, then by
   definition we don't need a full epilogue.  If the last basic
   block before the exit block does not contain active instructions,
   examine its predecessors and try to emit (conditional) return
   instructions.  */

edge
get_unconverted_simple_return (edge exit_fallthru_edge, bitmap_head bb_flags,
			       vec<edge> *unconverted_simple_returns,
			       rtx *returnjump)
{
  if (optimize)
    {
      unsigned i, last;

      /* convert_jumps_to_returns may add to preds of the exit block
         (but won't remove).  Stop at end of current preds.  */
      last = EDGE_COUNT (EXIT_BLOCK_PTR_FOR_FN (cfun)->preds);
      for (i = 0; i < last; i++)
	{
	  edge e = EDGE_I (EXIT_BLOCK_PTR_FOR_FN (cfun)->preds, i);
	  if (LABEL_P (BB_HEAD (e->src))
	      && !bitmap_bit_p (&bb_flags, e->src->index)
	      && !active_insn_between (BB_HEAD (e->src), BB_END (e->src)))
	    *unconverted_simple_returns
		  = convert_jumps_to_returns (e->src, true,
					      *unconverted_simple_returns);
	}
    }

  if (exit_fallthru_edge != NULL
      && EDGE_COUNT (exit_fallthru_edge->src->preds) != 0
      && !bitmap_bit_p (&bb_flags, exit_fallthru_edge->src->index))
    {
      basic_block last_bb;

      last_bb = emit_return_for_exit (exit_fallthru_edge, true);
      *returnjump = BB_END (last_bb);
      exit_fallthru_edge = NULL;
    }
  return exit_fallthru_edge;
}

/* If there were branches to an empty LAST_BB which we tried to
   convert to conditional simple_returns, but couldn't for some
   reason, create a block to hold a simple_return insn and redirect
   those remaining edges.  */

void
convert_to_simple_return (edge entry_edge, edge orig_entry_edge,
			  bitmap_head bb_flags, rtx returnjump,
			  vec<edge> unconverted_simple_returns)
{
  edge e;
  edge_iterator ei;

  if (!unconverted_simple_returns.is_empty ())
    {
      basic_block simple_return_block_hot = NULL;
      basic_block simple_return_block_cold = NULL;
      edge pending_edge_hot = NULL;
      edge pending_edge_cold = NULL;
      basic_block exit_pred;
      int i;

      gcc_assert (entry_edge != orig_entry_edge);

      /* See if we can reuse the last insn that was emitted for the
	 epilogue.  */
      if (returnjump != NULL_RTX
	  && JUMP_LABEL (returnjump) == simple_return_rtx)
	{
	  e = split_block (BLOCK_FOR_INSN (returnjump), PREV_INSN (returnjump));
	  if (BB_PARTITION (e->src) == BB_HOT_PARTITION)
	    simple_return_block_hot = e->dest;
	  else
	    simple_return_block_cold = e->dest;
	}

      /* Also check returns we might need to add to tail blocks.  */
      FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR_FOR_FN (cfun)->preds)
	if (EDGE_COUNT (e->src->preds) != 0
	    && (e->flags & EDGE_FAKE) != 0
	    && !bitmap_bit_p (&bb_flags, e->src->index))
	  {
	    if (BB_PARTITION (e->src) == BB_HOT_PARTITION)
	      pending_edge_hot = e;
	    else
	      pending_edge_cold = e;
	  }

      /* Save a pointer to the exit's predecessor BB for use in
         inserting new BBs at the end of the function.  Do this
         after the call to split_block above which may split
         the original exit pred.  */
      exit_pred = EXIT_BLOCK_PTR_FOR_FN (cfun)->prev_bb;

      FOR_EACH_VEC_ELT (unconverted_simple_returns, i, e)
	{
	  basic_block *pdest_bb;
	  edge pending;

	  if (BB_PARTITION (e->src) == BB_HOT_PARTITION)
	    {
	      pdest_bb = &simple_return_block_hot;
	      pending = pending_edge_hot;
	    }
	  else
	    {
	      pdest_bb = &simple_return_block_cold;
	      pending = pending_edge_cold;
	    }

	  if (*pdest_bb == NULL && pending != NULL)
	    {
	      emit_return_into_block (true, pending->src);
	      pending->flags &= ~(EDGE_FALLTHRU | EDGE_FAKE);
	      *pdest_bb = pending->src;
	    }
	  else if (*pdest_bb == NULL)
	    {
	      basic_block bb;
	      rtx start;

	      bb = create_basic_block (NULL, NULL, exit_pred);
	      BB_COPY_PARTITION (bb, e->src);
	      start = emit_jump_insn_after (gen_simple_return (),
					    BB_END (bb));
	      JUMP_LABEL (start) = simple_return_rtx;
	      emit_barrier_after (start);

	      *pdest_bb = bb;
	      make_edge (bb, EXIT_BLOCK_PTR_FOR_FN (cfun), 0);
	    }
	  redirect_edge_and_branch_force (e, *pdest_bb);
	}
      unconverted_simple_returns.release ();
    }

  if (entry_edge != orig_entry_edge)
    {
      FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR_FOR_FN (cfun)->preds)
	if (EDGE_COUNT (e->src->preds) != 0
	    && (e->flags & EDGE_FAKE) != 0
	    && !bitmap_bit_p (&bb_flags, e->src->index))
	  {
	    emit_return_into_block (true, e->src);
	    e->flags &= ~(EDGE_FALLTHRU | EDGE_FAKE);
	  }
    }
}

#endif
