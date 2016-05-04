/* Shrink-wrapping related optimizations.
   Copyright (C) 1987-2016 Free Software Foundation, Inc.

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
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "cfghooks.h"
#include "df.h"
#include "tm_p.h"
#include "regs.h"
#include "emit-rtl.h"
#include "output.h"
#include "tree-pass.h"
#include "cfgrtl.h"
#include "params.h"
#include "bb-reorder.h"
#include "shrink-wrap.h"
#include "regcprop.h"
#include "rtl-iter.h"
#include "valtrack.h"


/* Return true if INSN requires the stack frame to be set up.
   PROLOGUE_USED contains the hard registers used in the function
   prologue.  SET_UP_BY_PROLOGUE is the set of registers we expect the
   prologue to set up for the function.  */
bool
requires_stack_frame_p (rtx_insn *insn, HARD_REG_SET prologue_used,
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

      add_to_hard_reg_set (&hardregs, GET_MODE (dreg), REGNO (dreg));
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
move_insn_for_shrink_wrap (basic_block bb, rtx_insn *insn,
			   const HARD_REG_SET uses,
			   const HARD_REG_SET defs,
			   bool *split_p,
			   struct dead_debug_local *debug)
{
  rtx set, src, dest;
  bitmap live_out, live_in, bb_uses, bb_defs;
  unsigned int i, dregno, end_dregno;
  unsigned int sregno = FIRST_PSEUDO_REGISTER;
  unsigned int end_sregno = FIRST_PSEUDO_REGISTER;
  basic_block next_block;
  edge live_edge;
  rtx_insn *dinsn;
  df_ref def;

  /* Look for a simple register assignment.  We don't use single_set here
     because we can't deal with any CLOBBERs, USEs, or REG_UNUSED secondary
     destinations.  */
  if (!INSN_P (insn))
    return false;
  set = PATTERN (insn);
  if (GET_CODE (set) != SET)
    return false;
  src = SET_SRC (set);
  dest = SET_DEST (set);

  /* For the destination, we want only a register.  Also disallow STACK
     or FRAME related adjustments.  They are likely part of the prologue,
     so keep them in the entry block.  */
  if (!REG_P (dest)
      || dest == stack_pointer_rtx
      || dest == frame_pointer_rtx
      || dest == hard_frame_pointer_rtx)
    return false;

  /* For the source, we want one of:
      (1) A (non-overlapping) register
      (2) A constant,
      (3) An expression involving no more than one register.

     That last point comes from the code following, which was originally
     written to handle only register move operations, and still only handles
     a single source register when checking for overlaps.  Happily, the
     same checks can be applied to expressions like (plus reg const).  */

  if (CONSTANT_P (src))
    ;
  else if (!REG_P (src))
    {
      rtx src_inner = NULL_RTX;

      if (can_throw_internal (insn))
	return false;

      subrtx_var_iterator::array_type array;
      FOR_EACH_SUBRTX_VAR (iter, array, src, ALL)
	{
	  rtx x = *iter;
	  switch (GET_RTX_CLASS (GET_CODE (x)))
	    {
	    case RTX_CONST_OBJ:
	    case RTX_COMPARE:
	    case RTX_COMM_COMPARE:
	    case RTX_BIN_ARITH:
	    case RTX_COMM_ARITH:
	    case RTX_UNARY:
	    case RTX_TERNARY:
	      /* Constant or expression.  Continue.  */
	      break;

	    case RTX_OBJ:
	    case RTX_EXTRA:
	      switch (GET_CODE (x))
		{
		case UNSPEC:
		case SUBREG:
		case STRICT_LOW_PART:
		case PC:
		case LO_SUM:
		  /* Ok.  Continue.  */
		  break;

		case REG:
		  /* Fail if we see a second inner register.  */
		  if (src_inner != NULL)
		    return false;
		  src_inner = x;
		  break;

		default:
		  return false;
		}
	      break;

	    default:
	      return false;
	    }
	}

      if (src_inner != NULL)
	src = src_inner;
    }

  /* Make sure that the source register isn't defined later in BB.  */
  if (REG_P (src))
    {
      sregno = REGNO (src);
      end_sregno = END_REGNO (src);
      if (overlaps_hard_reg_set_p (defs, GET_MODE (src), sregno))
	return false;
    }

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

      basic_block old_dest = live_edge->dest;
      next_block = split_edge (live_edge);

      /* We create a new basic block.  Call df_grow_bb_info to make sure
	 all data structures are allocated.  */
      df_grow_bb_info (df_live);

      bitmap_and (df_get_live_in (next_block), df_get_live_out (bb),
		  df_get_live_in (old_dest));
      df_set_bb_dirty (next_block);

      /* We should not split more than once for a function.  */
      if (*split_p)
	return false;

      *split_p = true;
    }

  /* At this point we are committed to moving INSN, but let's try to
     move it as far as we can.  */
  do
    {
      if (MAY_HAVE_DEBUG_INSNS)
	{
	  FOR_BB_INSNS_REVERSE (bb, dinsn)
	    if (DEBUG_INSN_P (dinsn))
	      {
		df_ref use;
		FOR_EACH_INSN_USE (use, dinsn)
		  if (refers_to_regno_p (dregno, end_dregno,
					 DF_REF_REG (use), (rtx *) NULL))
		    dead_debug_add (debug, use, DF_REF_REGNO (use));
	      }
	    else if (dinsn == insn)
	      break;
	}
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

  /* Insert debug temps for dead REGs used in subsequent debug insns.  */
  if (debug->used && !bitmap_empty_p (debug->used))
    FOR_EACH_INSN_DEF (def, insn)
      dead_debug_insert_temp (debug, DF_REF_REGNO (def), insn,
			      DEBUG_TEMP_BEFORE_WITH_VALUE);

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

static void
prepare_shrink_wrap (basic_block entry_block)
{
  rtx_insn *insn, *curr;
  rtx x;
  HARD_REG_SET uses, defs;
  df_ref def, use;
  bool split_p = false;
  unsigned int i;
  struct dead_debug_local debug;

  if (JUMP_P (BB_END (entry_block)))
    {
      /* To have more shrink-wrapping opportunities, prepare_shrink_wrap tries
	 to sink the copies from parameter to callee saved register out of
	 entry block.  copyprop_hardreg_forward_bb_without_debug_insn is called
	 to release some dependences.  */
      copyprop_hardreg_forward_bb_without_debug_insn (entry_block);
    }

  dead_debug_local_init (&debug, NULL, NULL);
  CLEAR_HARD_REG_SET (uses);
  CLEAR_HARD_REG_SET (defs);

  FOR_BB_INSNS_REVERSE_SAFE (entry_block, insn, curr)
    if (NONDEBUG_INSN_P (insn)
	&& !move_insn_for_shrink_wrap (entry_block, insn, uses, defs,
				       &split_p, &debug))
      {
	/* Add all defined registers to DEFs.  */
	FOR_EACH_INSN_DEF (def, insn)
	  {
	    x = DF_REF_REG (def);
	    if (REG_P (x) && HARD_REGISTER_P (x))
	      for (i = REGNO (x); i < END_REGNO (x); i++)
		SET_HARD_REG_BIT (defs, i);
	  }

	/* Add all used registers to USESs.  */
	FOR_EACH_INSN_USE (use, insn)
	  {
	    x = DF_REF_REG (use);
	    if (REG_P (x) && HARD_REGISTER_P (x))
	      for (i = REGNO (x); i < END_REGNO (x); i++)
		SET_HARD_REG_BIT (uses, i);
	  }
      }

  dead_debug_local_finish (&debug, NULL);
}

/* Return whether basic block PRO can get the prologue.  It can not if it
   has incoming complex edges that need a prologue inserted (we make a new
   block for the prologue, so those edges would need to be redirected, which
   does not work).  It also can not if there exist registers live on entry
   to PRO that are clobbered by the prologue.  */

static bool
can_get_prologue (basic_block pro, HARD_REG_SET prologue_clobbered)
{
  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, pro->preds)
    if (e->flags & (EDGE_COMPLEX | EDGE_CROSSING)
	&& !dominated_by_p (CDI_DOMINATORS, e->src, pro))
      return false;

  HARD_REG_SET live;
  REG_SET_TO_HARD_REG_SET (live, df_get_live_in (pro));
  if (hard_reg_set_intersect_p (live, prologue_clobbered))
    return false;

  return true;
}

/* Return whether we can duplicate basic block BB for shrink wrapping.  We
   cannot if the block cannot be duplicated at all, or if any of its incoming
   edges are complex and come from a block that does not require a prologue
   (we cannot redirect such edges), or if the block is too big to copy.
   PRO is the basic block before which we would put the prologue, MAX_SIZE is
   the maximum size block we allow to be copied.  */

static bool
can_dup_for_shrink_wrapping (basic_block bb, basic_block pro, unsigned max_size)
{
  if (!can_duplicate_block_p (bb))
    return false;

  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, bb->preds)
    if (e->flags & (EDGE_COMPLEX | EDGE_CROSSING)
	&& !dominated_by_p (CDI_DOMINATORS, e->src, pro))
      return false;

  unsigned size = 0;

  rtx_insn *insn;
  FOR_BB_INSNS (bb, insn)
    if (NONDEBUG_INSN_P (insn))
      {
	size += get_attr_min_length (insn);
	if (size > max_size)
	  return false;
      }

  return true;
}

/* If the source of edge E has more than one successor, the verifier for
   branch probabilities gets confused by the fake edges we make where
   simple_return statements will be inserted later (because those are not
   marked as fallthrough edges).  Fix this by creating an extra block just
   for that fallthrough.  */

static edge
fix_fake_fallthrough_edge (edge e)
{
  if (EDGE_COUNT (e->src->succs) <= 1)
    return e;

  basic_block old_bb = e->src;
  rtx_insn *end = BB_END (old_bb);
  rtx_note *note = emit_note_after (NOTE_INSN_DELETED, end);
  basic_block new_bb = create_basic_block (note, note, old_bb);
  BB_COPY_PARTITION (new_bb, old_bb);
  BB_END (old_bb) = end;

  redirect_edge_succ (e, new_bb);
  e->flags |= EDGE_FALLTHRU;
  e->flags &= ~EDGE_FAKE;

  return make_edge (new_bb, EXIT_BLOCK_PTR_FOR_FN (cfun), EDGE_FAKE);
}

/* Try to perform a kind of shrink-wrapping, making sure the
   prologue/epilogue is emitted only around those parts of the
   function that require it.

   There will be exactly one prologue, and it will be executed either
   zero or one time, on any path.  Depending on where the prologue is
   placed, some of the basic blocks can be reached via both paths with
   and without a prologue.  Such blocks will be duplicated here, and the
   edges changed to match.

   Paths that go to the exit without going through the prologue will use
   a simple_return instead of the epilogue.  We maximize the number of
   those, making sure to only duplicate blocks that can be duplicated.
   If the prologue can then still be placed in multiple locations, we
   place it as early as possible.

   An example, where we duplicate blocks with control flow (legend:
   _B_egin, _R_eturn and _S_imple_return; edges without arrowhead should
   be taken to point down or to the right, to simplify the diagram; here,
   block 3 needs a prologue, the rest does not):


       B                 B
       |                 |
       2                 2
       |\                |\
       | 3    becomes    | 3
       |/                |  \
       4                 7   4
       |\                |\  |\
       | 5               | 8 | 5
       |/                |/  |/
       6                 9   6
       |                 |   |
       R                 S   R


   (bb 4 is duplicated to 7, and so on; the prologue is inserted on the
   edge 2->3).

   Another example, where part of a loop is duplicated (again, bb 3 is
   the only block that needs a prologue):


       B   3<--              B       ->3<--
       |   |   |             |      |  |   |
       |   v   |   becomes   |      |  v   |
       2---4---              2---5--   4---
           |                     |     |
           R                     S     R


   (bb 4 is duplicated to 5; the prologue is inserted on the edge 5->3).

   ENTRY_EDGE is the edge where the prologue will be placed, possibly
   changed by this function.  BB_WITH is a bitmap that, if we do shrink-
   wrap, will on return contain the interesting blocks that run with
   prologue.  PROLOGUE_SEQ is the prologue we will insert.  */

void
try_shrink_wrapping (edge *entry_edge, bitmap_head *bb_with,
		     rtx_insn *prologue_seq)
{
  /* If we cannot shrink-wrap, are told not to shrink-wrap, or it makes
     no sense to shrink-wrap: then do not shrink-wrap!  */

  if (!SHRINK_WRAPPING_ENABLED)
    return;

  if (crtl->profile && !targetm.profile_before_prologue ())
    return;

  if (crtl->calls_eh_return)
    return;

  bool empty_prologue = true;
  for (rtx_insn *insn = prologue_seq; insn; insn = NEXT_INSN (insn))
    if (!(NOTE_P (insn) && NOTE_KIND (insn) == NOTE_INSN_PROLOGUE_END))
      {
	empty_prologue = false;
	break;
      }
  if (empty_prologue)
    return;

  /* Move some code down to expose more shrink-wrapping opportunities.  */

  basic_block entry = (*entry_edge)->dest;
  prepare_shrink_wrap (entry);

  if (dump_file)
    fprintf (dump_file, "Attempting shrink-wrapping optimization.\n");

  /* Compute the registers set and used in the prologue.  */

  HARD_REG_SET prologue_clobbered, prologue_used;
  CLEAR_HARD_REG_SET (prologue_clobbered);
  CLEAR_HARD_REG_SET (prologue_used);
  for (rtx_insn *insn = prologue_seq; insn; insn = NEXT_INSN (insn))
    if (NONDEBUG_INSN_P (insn))
      {
	HARD_REG_SET this_used;
	CLEAR_HARD_REG_SET (this_used);
	note_uses (&PATTERN (insn), record_hard_reg_uses, &this_used);
	AND_COMPL_HARD_REG_SET (this_used, prologue_clobbered);
	IOR_HARD_REG_SET (prologue_used, this_used);
	note_stores (PATTERN (insn), record_hard_reg_sets, &prologue_clobbered);
      }
  CLEAR_HARD_REG_BIT (prologue_clobbered, STACK_POINTER_REGNUM);
  if (frame_pointer_needed)
    CLEAR_HARD_REG_BIT (prologue_clobbered, HARD_FRAME_POINTER_REGNUM);

  /* Find out what registers are set up by the prologue; any use of these
     cannot happen before the prologue.  */

  struct hard_reg_set_container set_up_by_prologue;
  CLEAR_HARD_REG_SET (set_up_by_prologue.set);
  add_to_hard_reg_set (&set_up_by_prologue.set, Pmode, STACK_POINTER_REGNUM);
  add_to_hard_reg_set (&set_up_by_prologue.set, Pmode, ARG_POINTER_REGNUM);
  if (frame_pointer_needed)
    add_to_hard_reg_set (&set_up_by_prologue.set, Pmode,
			 HARD_FRAME_POINTER_REGNUM);
  if (pic_offset_table_rtx 
      && (unsigned) PIC_OFFSET_TABLE_REGNUM != INVALID_REGNUM)
    add_to_hard_reg_set (&set_up_by_prologue.set, Pmode,
			 PIC_OFFSET_TABLE_REGNUM);
  if (crtl->drap_reg)
    add_to_hard_reg_set (&set_up_by_prologue.set,
			 GET_MODE (crtl->drap_reg),
			 REGNO (crtl->drap_reg));
  if (targetm.set_up_by_prologue)
    targetm.set_up_by_prologue (&set_up_by_prologue);

  /* We will insert the prologue before the basic block PRO.  PRO should
     dominate all basic blocks that need the prologue to be executed
     before them.  First, make PRO the "tightest wrap" possible.  */

  calculate_dominance_info (CDI_DOMINATORS);

  basic_block pro = 0;

  basic_block bb;
  edge e;
  edge_iterator ei;
  FOR_EACH_BB_FN (bb, cfun)
    {
      rtx_insn *insn;
      FOR_BB_INSNS (bb, insn)
	if (NONDEBUG_INSN_P (insn)
	    && requires_stack_frame_p (insn, prologue_used,
				       set_up_by_prologue.set))
	  {
	    if (dump_file)
	      fprintf (dump_file, "Block %d needs the prologue.\n", bb->index);
	    pro = nearest_common_dominator (CDI_DOMINATORS, pro, bb);
	    break;
	  }
    }

  /* If nothing needs a prologue, just put it at the start.  This really
     shouldn't happen, but we cannot fix it here.  */

  if (pro == 0)
    {
      if (dump_file)
	fprintf(dump_file, "Nothing needs a prologue, but it isn't empty; "
			   "putting it at the start.\n");
      pro = entry;
    }

  if (dump_file)
    fprintf (dump_file, "After wrapping required blocks, PRO is now %d\n",
	     pro->index);

  /* Now see if we can put the prologue at the start of PRO.  Putting it
     there might require duplicating a block that cannot be duplicated,
     or in some cases we cannot insert the prologue there at all.  If PRO
     wont't do, try again with the immediate dominator of PRO, and so on.

     The blocks that need duplicating are those reachable from PRO but
     not dominated by it.  We keep in BB_WITH a bitmap of the blocks
     reachable from PRO that we already found, and in VEC a stack of
     those we still need to consider (to find successors).  */

  bitmap_set_bit (bb_with, pro->index);

  vec<basic_block> vec;
  vec.create (n_basic_blocks_for_fn (cfun));
  vec.quick_push (pro);

  unsigned max_grow_size = get_uncond_jump_length ();
  max_grow_size *= PARAM_VALUE (PARAM_MAX_GROW_COPY_BB_INSNS);

  while (!vec.is_empty () && pro != entry)
    {
      while (pro != entry && !can_get_prologue (pro, prologue_clobbered))
	{
	  pro = get_immediate_dominator (CDI_DOMINATORS, pro);

	  if (bitmap_set_bit (bb_with, pro->index))
	    vec.quick_push (pro);
	}

      basic_block bb = vec.pop ();
      if (!can_dup_for_shrink_wrapping (bb, pro, max_grow_size))
	while (!dominated_by_p (CDI_DOMINATORS, bb, pro))
	  {
	    gcc_assert (pro != entry);

	    pro = get_immediate_dominator (CDI_DOMINATORS, pro);

	    if (bitmap_set_bit (bb_with, pro->index))
	      vec.quick_push (pro);
	  }

      FOR_EACH_EDGE (e, ei, bb->succs)
	if (e->dest != EXIT_BLOCK_PTR_FOR_FN (cfun)
	    && bitmap_set_bit (bb_with, e->dest->index))
	  vec.quick_push (e->dest);
    }

  if (dump_file)
    fprintf (dump_file, "Avoiding non-duplicatable blocks, PRO is now %d\n",
	     pro->index);

  /* If we can move PRO back without having to duplicate more blocks, do so.
     We do this because putting the prologue earlier is better for scheduling.

     We can move back to a block PRE if every path from PRE will eventually
     need a prologue, that is, PRO is a post-dominator of PRE.  PRE needs
     to dominate every block reachable from itself.  We keep in BB_TMP a
     bitmap of the blocks reachable from PRE that we already found, and in
     VEC a stack of those we still need to consider.

     Any block reachable from PRE is also reachable from all predecessors
     of PRE, so if we find we need to move PRE back further we can leave
     everything not considered so far on the stack.  Any block dominated
     by PRE is also dominated by all other dominators of PRE, so anything
     found good for some PRE does not need to be reconsidered later.

     We don't need to update BB_WITH because none of the new blocks found
     can jump to a block that does not need the prologue.  */

  if (pro != entry)
    {
      calculate_dominance_info (CDI_POST_DOMINATORS);

      bitmap bb_tmp = BITMAP_ALLOC (NULL);
      bitmap_copy (bb_tmp, bb_with);
      basic_block last_ok = pro;
      vec.truncate (0);

      while (pro != entry)
	{
	  basic_block pre = get_immediate_dominator (CDI_DOMINATORS, pro);
	  if (!dominated_by_p (CDI_POST_DOMINATORS, pre, pro))
	    break;

	  if (bitmap_set_bit (bb_tmp, pre->index))
	    vec.quick_push (pre);

	  bool ok = true;
	  while (!vec.is_empty ())
	    {
	      if (!dominated_by_p (CDI_DOMINATORS, vec.last (), pre))
		{
		  ok = false;
		  break;
		}

	      basic_block bb = vec.pop ();
	      FOR_EACH_EDGE (e, ei, bb->succs)
		if (bitmap_set_bit (bb_tmp, e->dest->index))
		  vec.quick_push (e->dest);
	    }

	  if (ok && can_get_prologue (pre, prologue_clobbered))
	    last_ok = pre;

	  pro = pre;
	}

      pro = last_ok;

      BITMAP_FREE (bb_tmp);
      free_dominance_info (CDI_POST_DOMINATORS);
    }

  vec.release ();

  if (dump_file)
    fprintf (dump_file, "Bumping back to anticipatable blocks, PRO is now %d\n",
	     pro->index);

  if (pro == entry)
    {
      free_dominance_info (CDI_DOMINATORS);
      return;
    }

  /* Compute what fraction of the frequency and count of the blocks that run
     both with and without prologue are for running with prologue.  This gives
     the correct answer for reducible flow graphs; for irreducible flow graphs
     our profile is messed up beyond repair anyway.  */

  gcov_type num = 0;
  gcov_type den = 0;

  FOR_EACH_EDGE (e, ei, pro->preds)
    if (!dominated_by_p (CDI_DOMINATORS, e->src, pro))
      {
	num += EDGE_FREQUENCY (e);
	den += e->src->frequency;
      }

  if (den == 0)
    den = 1;

  /* All is okay, so do it.  */

  crtl->shrink_wrapped = true;
  if (dump_file)
    fprintf (dump_file, "Performing shrink-wrapping.\n");

  /* Copy the blocks that can run both with and without prologue.  The
     originals run with prologue, the copies without.  Store a pointer to
     the copy in the ->aux field of the original.  */

  FOR_EACH_BB_FN (bb, cfun)
    if (bitmap_bit_p (bb_with, bb->index)
	&& !dominated_by_p (CDI_DOMINATORS, bb, pro))
      {
	basic_block dup = duplicate_block (bb, 0, 0);

	bb->aux = dup;

	if (JUMP_P (BB_END (dup)) && !any_condjump_p (BB_END (dup)))
	  emit_barrier_after_bb (dup);

	if (EDGE_COUNT (dup->succs) == 0)
	  emit_barrier_after_bb (dup);

	if (dump_file)
	  fprintf (dump_file, "Duplicated %d to %d\n", bb->index, dup->index);

	bb->frequency = RDIV (num * bb->frequency, den);
	dup->frequency -= bb->frequency;
	bb->count = RDIV (num * bb->count, den);
	dup->count -= bb->count;
      }

  /* Now change the edges to point to the copies, where appropriate.  */

  FOR_EACH_BB_FN (bb, cfun)
    if (!dominated_by_p (CDI_DOMINATORS, bb, pro))
      {
	basic_block src = bb;
	if (bitmap_bit_p (bb_with, bb->index))
	  src = (basic_block) bb->aux;

	FOR_EACH_EDGE (e, ei, src->succs)
	  {
	    if (e->dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
	      continue;

	    if (bitmap_bit_p (bb_with, e->dest->index)
		&& !dominated_by_p (CDI_DOMINATORS, e->dest, pro))
	      {
		if (dump_file)
		  fprintf (dump_file, "Redirecting edge %d->%d to %d\n",
			   e->src->index, e->dest->index,
			   ((basic_block) e->dest->aux)->index);
		redirect_edge_and_branch_force (e, (basic_block) e->dest->aux);
	      }
	    else if (e->flags & EDGE_FALLTHRU
		     && bitmap_bit_p (bb_with, bb->index))
	      force_nonfallthru (e);
	  }
      }

  /* Also redirect the function entry edge if necessary.  */

  FOR_EACH_EDGE (e, ei, ENTRY_BLOCK_PTR_FOR_FN (cfun)->succs)
    if (bitmap_bit_p (bb_with, e->dest->index)
	&& !dominated_by_p (CDI_DOMINATORS, e->dest, pro))
      {
	basic_block split_bb = split_edge (e);
	e = single_succ_edge (split_bb);
	redirect_edge_and_branch_force (e, (basic_block) e->dest->aux);
      }

  /* Make a simple_return for those exits that run without prologue.  */

  FOR_EACH_BB_REVERSE_FN (bb, cfun)
    if (!bitmap_bit_p (bb_with, bb->index))
      FOR_EACH_EDGE (e, ei, bb->succs)
	if (e->dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
	  {
	    e = fix_fake_fallthrough_edge (e);

	    e->flags &= ~EDGE_FALLTHRU;
	    if (!(e->flags & EDGE_SIBCALL))
	      {
		rtx_insn *ret = targetm.gen_simple_return ();
		rtx_insn *end = BB_END (e->src);
		rtx_jump_insn *start = emit_jump_insn_after (ret, end);
		JUMP_LABEL (start) = simple_return_rtx;
		e->flags &= ~EDGE_FAKE;

		if (dump_file)
		  fprintf (dump_file,
			   "Made simple_return with UID %d in bb %d\n",
			   INSN_UID (start), e->src->index);
	      }

	    emit_barrier_after_bb (e->src);
	  }

  /* Finally, we want a single edge to put the prologue on.  Make a new
     block before the PRO block; the edge beteen them is the edge we want.
     Then redirect those edges into PRO that come from blocks without the
     prologue, to point to the new block instead.  The new prologue block
     is put at the end of the insn chain.  */

  basic_block new_bb = create_empty_bb (EXIT_BLOCK_PTR_FOR_FN (cfun)->prev_bb);
  BB_COPY_PARTITION (new_bb, pro);
  if (dump_file)
    fprintf (dump_file, "Made prologue block %d\n", new_bb->index);

  for (ei = ei_start (pro->preds); (e = ei_safe_edge (ei)); )
    {
      if (bitmap_bit_p (bb_with, e->src->index)
	  || dominated_by_p (CDI_DOMINATORS, e->src, pro))
	{
	  ei_next (&ei);
	  continue;
	}

      new_bb->count += RDIV (e->src->count * e->probability, REG_BR_PROB_BASE);
      new_bb->frequency += EDGE_FREQUENCY (e);

      redirect_edge_and_branch_force (e, new_bb);
      if (dump_file)
	fprintf (dump_file, "Redirected edge from %d\n", e->src->index);
    }

  *entry_edge = make_single_succ_edge (new_bb, pro, EDGE_FALLTHRU);
  force_nonfallthru (*entry_edge);

  free_dominance_info (CDI_DOMINATORS);
}
