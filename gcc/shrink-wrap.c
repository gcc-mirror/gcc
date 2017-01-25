/* Shrink-wrapping related optimizations.
   Copyright (C) 1987-2017 Free Software Foundation, Inc.

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
#include "memmodel.h"
#include "tm_p.h"
#include "regs.h"
#include "insn-config.h"
#include "emit-rtl.h"
#include "output.h"
#include "tree-pass.h"
#include "cfgrtl.h"
#include "cfgbuild.h"
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

/* Do whatever needs to be done for exits that run without prologue.
   Sibcalls need nothing done.  Normal exits get a simple_return inserted.  */

static void
handle_simple_exit (edge e)
{

  if (e->flags & EDGE_SIBCALL)
    {
      /* Tell function.c to take no further action on this edge.  */
      e->flags |= EDGE_IGNORE;

      e->flags &= ~EDGE_FALLTHRU;
      emit_barrier_after_bb (e->src);
      return;
    }

  /* If the basic block the edge comes from has multiple successors,
     split the edge.  */
  if (EDGE_COUNT (e->src->succs) > 1)
    {
      basic_block old_bb = e->src;
      rtx_insn *end = BB_END (old_bb);
      rtx_note *note = emit_note_after (NOTE_INSN_DELETED, end);
      basic_block new_bb = create_basic_block (note, note, old_bb);
      BB_COPY_PARTITION (new_bb, old_bb);
      BB_END (old_bb) = end;

      redirect_edge_succ (e, new_bb);
      e->flags |= EDGE_FALLTHRU;

      e = make_edge (new_bb, EXIT_BLOCK_PTR_FOR_FN (cfun), 0);
    }

  e->flags &= ~EDGE_FALLTHRU;
  rtx_jump_insn *ret = emit_jump_insn_after (targetm.gen_simple_return (),
					     BB_END (e->src));
  JUMP_LABEL (ret) = simple_return_rtx;
  emit_barrier_after_bb (e->src);

  if (dump_file)
    fprintf (dump_file, "Made simple_return with UID %d in bb %d\n",
	     INSN_UID (ret), e->src->index);
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
   changed by this function.  PROLOGUE_SEQ is the prologue we will insert.  */

void
try_shrink_wrapping (edge *entry_edge, rtx_insn *prologue_seq)
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

  bitmap bb_with = BITMAP_ALLOC (NULL);
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
      BITMAP_FREE (bb_with);
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
	  handle_simple_exit (e);

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

  BITMAP_FREE (bb_with);
  free_dominance_info (CDI_DOMINATORS);
}

/* Separate shrink-wrapping

   Instead of putting all of the prologue and epilogue in one spot, we
   can put parts of it in places where those components are executed less
   frequently.  The following code does this, for prologue and epilogue
   components that can be put in more than one location, and where those
   components can be executed more than once (the epilogue component will
   always be executed before the prologue component is executed a second
   time).

   What exactly is a component is target-dependent.  The more usual
   components are simple saves/restores to/from the frame of callee-saved
   registers.  This code treats components abstractly (as an sbitmap),
   letting the target handle all details.

   Prologue components are placed in such a way that for every component
   the prologue is executed as infrequently as possible.  We do this by
   walking the dominator tree, comparing the cost of placing a prologue
   component before a block to the sum of costs determined for all subtrees
   of that block.

   From this placement, we then determine for each component all blocks
   where at least one of this block's dominators (including itself) will
   get a prologue inserted.  That then is how the components are placed.
   We could place the epilogue components a bit smarter (we can save a
   bit of code size sometimes); this is a possible future improvement.

   Prologues and epilogues are preferably placed into a block, either at
   the beginning or end of it, if it is needed for all predecessor resp.
   successor edges; or placed on the edge otherwise.

   If the placement of any prologue/epilogue leads to a situation we cannot
   handle (for example, an abnormal edge would need to be split, or some
   targets want to use some specific registers that may not be available
   where we want to put them), separate shrink-wrapping for the components
   in that prologue/epilogue is aborted.  */


/* Print the sbitmap COMPONENTS to the DUMP_FILE if not empty, with the
   label LABEL.  */
static void
dump_components (const char *label, sbitmap components)
{
  if (bitmap_empty_p (components))
    return;

  fprintf (dump_file, " [%s", label);

  for (unsigned int j = 0; j < components->n_bits; j++)
    if (bitmap_bit_p (components, j))
      fprintf (dump_file, " %u", j);

  fprintf (dump_file, "]");
}

/* The data we collect for each bb.  */
struct sw {
  /* What components does this BB need?  */
  sbitmap needs_components;

  /* What components does this BB have?  This is the main decision this
     pass makes.  */
  sbitmap has_components;

  /* The components for which we placed code at the start of the BB (instead
     of on all incoming edges).  */
  sbitmap head_components;

  /* The components for which we placed code at the end of the BB (instead
     of on all outgoing edges).  */
  sbitmap tail_components;

  /* The frequency of executing the prologue for this BB, if a prologue is
     placed on this BB.  This is a pessimistic estimate (no prologue is
     needed for edges from blocks that have the component under consideration
     active already).  */
  gcov_type own_cost;

  /* The frequency of executing the prologue for this BB and all BBs
     dominated by it.  */
  gcov_type total_cost;
};

/* A helper function for accessing the pass-specific info.  */
static inline struct sw *
SW (basic_block bb)
{
  gcc_assert (bb->aux);
  return (struct sw *) bb->aux;
}

/* Create the pass-specific data structures for separately shrink-wrapping
   with components COMPONENTS.  */
static void
init_separate_shrink_wrap (sbitmap components)
{
  basic_block bb;
  FOR_ALL_BB_FN (bb, cfun)
    {
      bb->aux = xcalloc (1, sizeof (struct sw));

      SW (bb)->needs_components = targetm.shrink_wrap.components_for_bb (bb);

      /* Mark all basic blocks without successor as needing all components.
	 This avoids problems in at least cfgcleanup, sel-sched, and
	 regrename (largely to do with all paths to such a block still
	 needing the same dwarf CFI info).  */
      if (EDGE_COUNT (bb->succs) == 0)
	bitmap_copy (SW (bb)->needs_components, components);

      if (dump_file)
	{
	  fprintf (dump_file, "bb %d components:", bb->index);
	  dump_components ("has", SW (bb)->needs_components);
	  fprintf (dump_file, "\n");
	}

      SW (bb)->has_components = sbitmap_alloc (SBITMAP_SIZE (components));
      SW (bb)->head_components = sbitmap_alloc (SBITMAP_SIZE (components));
      SW (bb)->tail_components = sbitmap_alloc (SBITMAP_SIZE (components));
      bitmap_clear (SW (bb)->has_components);
    }
}

/* Destroy the pass-specific data.  */
static void
fini_separate_shrink_wrap (void)
{
  basic_block bb;
  FOR_ALL_BB_FN (bb, cfun)
    if (bb->aux)
      {
	sbitmap_free (SW (bb)->needs_components);
	sbitmap_free (SW (bb)->has_components);
	sbitmap_free (SW (bb)->head_components);
	sbitmap_free (SW (bb)->tail_components);
	free (bb->aux);
	bb->aux = 0;
      }
}

/* Place the prologue for component WHICH, in the basic blocks dominated
   by HEAD.  Do a DFS over the dominator tree, and set bit WHICH in the
   HAS_COMPONENTS of a block if either the block has that bit set in
   NEEDS_COMPONENTS, or it is cheaper to place the prologue here than in all
   dominator subtrees separately.  */
static void
place_prologue_for_one_component (unsigned int which, basic_block head)
{
  /* The block we are currently dealing with.  */
  basic_block bb = head;
  /* Is this the first time we visit this block, i.e. have we just gone
     down the tree.  */
  bool first_visit = true;

  /* Walk the dominator tree, visit one block per iteration of this loop.
     Each basic block is visited twice: once before visiting any children
     of the block, and once after visiting all of them (leaf nodes are
     visited only once).  As an optimization, we do not visit subtrees
     that can no longer influence the prologue placement.  */
  for (;;)
    {
      /* First visit of a block: set the (children) cost accumulator to zero;
	 if the block does not have the component itself, walk down.  */
      if (first_visit)
	{
	  /* Initialize the cost.  The cost is the block execution frequency
	     that does not come from backedges.  Calculating this by simply
	     adding the cost of all edges that aren't backedges does not
	     work: this does not always add up to the block frequency at
	     all, and even if it does, rounding error makes for bad
	     decisions.  */
	  SW (bb)->own_cost = bb->frequency;

	  edge e;
	  edge_iterator ei;
	  FOR_EACH_EDGE (e, ei, bb->preds)
	    if (dominated_by_p (CDI_DOMINATORS, e->src, bb))
	      {
		if (SW (bb)->own_cost > EDGE_FREQUENCY (e))
		  SW (bb)->own_cost -= EDGE_FREQUENCY (e);
		else
		  SW (bb)->own_cost = 0;
	      }

	  SW (bb)->total_cost = 0;

	  if (!bitmap_bit_p (SW (bb)->needs_components, which)
	      && first_dom_son (CDI_DOMINATORS, bb))
	    {
	      bb = first_dom_son (CDI_DOMINATORS, bb);
	      continue;
	    }
	}

      /* If this block does need the component itself, or it is cheaper to
	 put the prologue here than in all the descendants that need it,
	 mark it so.  If this block's immediate post-dominator is dominated
	 by this block, and that needs the prologue, we can put it on this
	 block as well (earlier is better).  */
      if (bitmap_bit_p (SW (bb)->needs_components, which)
	  || SW (bb)->total_cost > SW (bb)->own_cost)
	{
	  SW (bb)->total_cost = SW (bb)->own_cost;
	  bitmap_set_bit (SW (bb)->has_components, which);
	}
      else
	{
	  basic_block kid = get_immediate_dominator (CDI_POST_DOMINATORS, bb);
	  if (dominated_by_p (CDI_DOMINATORS, kid, bb)
	      && bitmap_bit_p (SW (kid)->has_components, which))
	    {
	      SW (bb)->total_cost = SW (bb)->own_cost;
	      bitmap_set_bit (SW (bb)->has_components, which);
	    }
	}

      /* We are back where we started, so we are done now.  */
      if (bb == head)
	return;

      /* We now know the cost of the subtree rooted at the current block.
	 Accumulate this cost in the parent.  */
      basic_block parent = get_immediate_dominator (CDI_DOMINATORS, bb);
      SW (parent)->total_cost += SW (bb)->total_cost;

      /* Don't walk the tree down unless necessary.  */
      if (next_dom_son (CDI_DOMINATORS, bb)
          && SW (parent)->total_cost <= SW (parent)->own_cost)
	{
	  bb = next_dom_son (CDI_DOMINATORS, bb);
	  first_visit = true;
	}
      else
	{
	  bb = parent;
	  first_visit = false;
	}
    }
}

/* Set HAS_COMPONENTS in every block to the maximum it can be set to without
   setting it on any path from entry to exit where it was not already set
   somewhere (or, for blocks that have no path to the exit, consider only
   paths from the entry to the block itself).  */
static void
spread_components (sbitmap components)
{
  basic_block entry_block = ENTRY_BLOCK_PTR_FOR_FN (cfun);
  basic_block exit_block = EXIT_BLOCK_PTR_FOR_FN (cfun);

  /* A stack of all blocks left to consider, and a bitmap of all blocks
     on that stack.  */
  vec<basic_block> todo;
  todo.create (n_basic_blocks_for_fn (cfun));
  bitmap seen = BITMAP_ALLOC (NULL);

  sbitmap old = sbitmap_alloc (SBITMAP_SIZE (components));

  /* Find for every block the components that are *not* needed on some path
     from the entry to that block.  Do this with a flood fill from the entry
     block.  Every block can be visited at most as often as the number of
     components (plus one), and usually much less often.  */

  if (dump_file)
    fprintf (dump_file, "Spreading down...\n");

  basic_block bb;
  FOR_ALL_BB_FN (bb, cfun)
    bitmap_clear (SW (bb)->head_components);

  bitmap_copy (SW (entry_block)->head_components, components);

  edge e;
  edge_iterator ei;

  todo.quick_push (single_succ (entry_block));
  bitmap_set_bit (seen, single_succ (entry_block)->index);
  while (!todo.is_empty ())
    {
      bb = todo.pop ();

      bitmap_copy (old, SW (bb)->head_components);

      FOR_EACH_EDGE (e, ei, bb->preds)
	bitmap_ior (SW (bb)->head_components, SW (bb)->head_components,
		    SW (e->src)->head_components);

      bitmap_and_compl (SW (bb)->head_components, SW (bb)->head_components,
			SW (bb)->has_components);

      if (!bitmap_equal_p (old, SW (bb)->head_components))
	FOR_EACH_EDGE (e, ei, bb->succs)
	  if (bitmap_set_bit (seen, e->dest->index))
	    todo.quick_push (e->dest);

      bitmap_clear_bit (seen, bb->index);
    }

  /* Find for every block the components that are *not* needed on some reverse
     path from the exit to that block.  */

  if (dump_file)
    fprintf (dump_file, "Spreading up...\n");

  /* First, mark all blocks not reachable from the exit block as not needing
     any component on any path to the exit.  Mark everything, and then clear
     again by a flood fill.  */

  FOR_ALL_BB_FN (bb, cfun)
    bitmap_copy (SW (bb)->tail_components, components);

  FOR_EACH_EDGE (e, ei, exit_block->preds)
    {
      todo.quick_push (e->src);
      bitmap_set_bit (seen, e->src->index);
    }

  while (!todo.is_empty ())
    {
      bb = todo.pop ();

      if (!bitmap_empty_p (SW (bb)->tail_components))
	FOR_EACH_EDGE (e, ei, bb->preds)
	  if (bitmap_set_bit (seen, e->src->index))
	    todo.quick_push (e->src);

      bitmap_clear (SW (bb)->tail_components);

      bitmap_clear_bit (seen, bb->index);
    }

  /* And then, flood fill backwards to find for every block the components
     not needed on some path to the exit.  */

  bitmap_copy (SW (exit_block)->tail_components, components);

  FOR_EACH_EDGE (e, ei, exit_block->preds)
    {
      todo.quick_push (e->src);
      bitmap_set_bit (seen, e->src->index);
    }

  while (!todo.is_empty ())
    {
      bb = todo.pop ();

      bitmap_copy (old, SW (bb)->tail_components);

      FOR_EACH_EDGE (e, ei, bb->succs)
	bitmap_ior (SW (bb)->tail_components, SW (bb)->tail_components,
		    SW (e->dest)->tail_components);

      bitmap_and_compl (SW (bb)->tail_components, SW (bb)->tail_components,
			SW (bb)->has_components);

      if (!bitmap_equal_p (old, SW (bb)->tail_components))
	FOR_EACH_EDGE (e, ei, bb->preds)
	  if (bitmap_set_bit (seen, e->src->index))
	    todo.quick_push (e->src);

      bitmap_clear_bit (seen, bb->index);
    }

  /* Finally, mark everything not not needed both forwards and backwards.  */

  FOR_EACH_BB_FN (bb, cfun)
    {
      bitmap_and (SW (bb)->head_components, SW (bb)->head_components,
		  SW (bb)->tail_components);
      bitmap_and_compl (SW (bb)->has_components, components,
			SW (bb)->head_components);
    }

  FOR_ALL_BB_FN (bb, cfun)
    {
      if (dump_file)
	{
	  fprintf (dump_file, "bb %d components:", bb->index);
	  dump_components ("has", SW (bb)->has_components);
	  fprintf (dump_file, "\n");
	}
    }

  sbitmap_free (old);
  BITMAP_FREE (seen);
}

/* If we cannot handle placing some component's prologues or epilogues where
   we decided we should place them, unmark that component in COMPONENTS so
   that it is not wrapped separately.  */
static void
disqualify_problematic_components (sbitmap components)
{
  sbitmap pro = sbitmap_alloc (SBITMAP_SIZE (components));
  sbitmap epi = sbitmap_alloc (SBITMAP_SIZE (components));

  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      edge e;
      edge_iterator ei;
      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  /* Find which components we want pro/epilogues for here.  */
	  bitmap_and_compl (epi, SW (e->src)->has_components,
			    SW (e->dest)->has_components);
	  bitmap_and_compl (pro, SW (e->dest)->has_components,
			    SW (e->src)->has_components);

	  /* Ask the target what it thinks about things.  */
	  if (!bitmap_empty_p (epi))
	    targetm.shrink_wrap.disqualify_components (components, e, epi,
						       false);
	  if (!bitmap_empty_p (pro))
	    targetm.shrink_wrap.disqualify_components (components, e, pro,
						       true);

	  /* If this edge doesn't need splitting, we're fine.  */
	  if (single_pred_p (e->dest)
	      && e->dest != EXIT_BLOCK_PTR_FOR_FN (cfun))
	    continue;

	  /* If the edge can be split, that is fine too.  */
	  if ((e->flags & EDGE_ABNORMAL) == 0)
	    continue;

	  /* We also can handle sibcalls.  */
	  if (e->dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
	    {
	      gcc_assert (e->flags & EDGE_SIBCALL);
	      continue;
	    }

	  /* Remove from consideration those components we would need
	     pro/epilogues for on edges where we cannot insert them.  */
	  bitmap_and_compl (components, components, epi);
	  bitmap_and_compl (components, components, pro);

	  if (dump_file && !bitmap_subset_p (epi, components))
	    {
	      fprintf (dump_file, "  BAD epi %d->%d", e->src->index,
		       e->dest->index);
	      if (e->flags & EDGE_EH)
		fprintf (dump_file, " for EH");
	      dump_components ("epi", epi);
	      fprintf (dump_file, "\n");
	    }

	  if (dump_file && !bitmap_subset_p (pro, components))
	    {
	      fprintf (dump_file, "  BAD pro %d->%d", e->src->index,
		       e->dest->index);
	      if (e->flags & EDGE_EH)
		fprintf (dump_file, " for EH");
	      dump_components ("pro", pro);
	      fprintf (dump_file, "\n");
	    }
	}
    }

  sbitmap_free (pro);
  sbitmap_free (epi);
}

/* Place code for prologues and epilogues for COMPONENTS where we can put
   that code at the start of basic blocks.  */
static void
emit_common_heads_for_components (sbitmap components)
{
  sbitmap pro = sbitmap_alloc (SBITMAP_SIZE (components));
  sbitmap epi = sbitmap_alloc (SBITMAP_SIZE (components));
  sbitmap tmp = sbitmap_alloc (SBITMAP_SIZE (components));

  basic_block bb;
  FOR_ALL_BB_FN (bb, cfun)
    bitmap_clear (SW (bb)->head_components);

  FOR_EACH_BB_FN (bb, cfun)
    {
      /* Find which prologue resp. epilogue components are needed for all
	 predecessor edges to this block.  */

      /* First, select all possible components.  */
      bitmap_copy (epi, components);
      bitmap_copy (pro, components);

      edge e;
      edge_iterator ei;
      FOR_EACH_EDGE (e, ei, bb->preds)
	{
	  if (e->flags & EDGE_ABNORMAL)
	    {
	      bitmap_clear (epi);
	      bitmap_clear (pro);
	      break;
	    }

	  /* Deselect those epilogue components that should not be inserted
	     for this edge.  */
	  bitmap_and_compl (tmp, SW (e->src)->has_components,
			    SW (e->dest)->has_components);
	  bitmap_and (epi, epi, tmp);

	  /* Similar, for the prologue.  */
	  bitmap_and_compl (tmp, SW (e->dest)->has_components,
			    SW (e->src)->has_components);
	  bitmap_and (pro, pro, tmp);
	}

      if (dump_file && !(bitmap_empty_p (epi) && bitmap_empty_p (pro)))
	fprintf (dump_file, "  bb %d", bb->index);

      if (dump_file && !bitmap_empty_p (epi))
	dump_components ("epi", epi);
      if (dump_file && !bitmap_empty_p (pro))
	dump_components ("pro", pro);

      if (dump_file && !(bitmap_empty_p (epi) && bitmap_empty_p (pro)))
	fprintf (dump_file, "\n");

      /* Place code after the BB note.  */
      if (!bitmap_empty_p (pro))
	{
	  start_sequence ();
	  targetm.shrink_wrap.emit_prologue_components (pro);
	  rtx_insn *seq = get_insns ();
	  end_sequence ();
	  record_prologue_seq (seq);

	  emit_insn_after (seq, bb_note (bb));

	  bitmap_ior (SW (bb)->head_components, SW (bb)->head_components, pro);
	}

      if (!bitmap_empty_p (epi))
	{
	  start_sequence ();
	  targetm.shrink_wrap.emit_epilogue_components (epi);
	  rtx_insn *seq = get_insns ();
	  end_sequence ();
	  record_epilogue_seq (seq);

	  emit_insn_after (seq, bb_note (bb));

	  bitmap_ior (SW (bb)->head_components, SW (bb)->head_components, epi);
	}
    }

  sbitmap_free (pro);
  sbitmap_free (epi);
  sbitmap_free (tmp);
}

/* Place code for prologues and epilogues for COMPONENTS where we can put
   that code at the end of basic blocks.  */
static void
emit_common_tails_for_components (sbitmap components)
{
  sbitmap pro = sbitmap_alloc (SBITMAP_SIZE (components));
  sbitmap epi = sbitmap_alloc (SBITMAP_SIZE (components));
  sbitmap tmp = sbitmap_alloc (SBITMAP_SIZE (components));

  basic_block bb;
  FOR_ALL_BB_FN (bb, cfun)
    bitmap_clear (SW (bb)->tail_components);

  FOR_EACH_BB_FN (bb, cfun)
    {
      /* Find which prologue resp. epilogue components are needed for all
	 successor edges from this block.  */
      if (EDGE_COUNT (bb->succs) == 0)
	continue;

      /* First, select all possible components.  */
      bitmap_copy (epi, components);
      bitmap_copy (pro, components);

      edge e;
      edge_iterator ei;
      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  if (e->flags & EDGE_ABNORMAL)
	    {
	      bitmap_clear (epi);
	      bitmap_clear (pro);
	      break;
	    }

	  /* Deselect those epilogue components that should not be inserted
	     for this edge, and also those that are already put at the head
	     of the successor block.  */
	  bitmap_and_compl (tmp, SW (e->src)->has_components,
			    SW (e->dest)->has_components);
	  bitmap_and_compl (tmp, tmp, SW (e->dest)->head_components);
	  bitmap_and (epi, epi, tmp);

	  /* Similarly, for the prologue.  */
	  bitmap_and_compl (tmp, SW (e->dest)->has_components,
			    SW (e->src)->has_components);
	  bitmap_and_compl (tmp, tmp, SW (e->dest)->head_components);
	  bitmap_and (pro, pro, tmp);
	}

      /* If the last insn of this block is a control flow insn we cannot
	 put anything after it.  We can put our code before it instead,
	 but only if that jump insn is a simple jump.  */
      rtx_insn *last_insn = BB_END (bb);
      if (control_flow_insn_p (last_insn) && !simplejump_p (last_insn))
	{
	  bitmap_clear (epi);
	  bitmap_clear (pro);
	}

      if (dump_file && !(bitmap_empty_p (epi) && bitmap_empty_p (pro)))
	fprintf (dump_file, "  bb %d", bb->index);

      if (dump_file && !bitmap_empty_p (epi))
	dump_components ("epi", epi);
      if (dump_file && !bitmap_empty_p (pro))
	dump_components ("pro", pro);

      if (dump_file && !(bitmap_empty_p (epi) && bitmap_empty_p (pro)))
	fprintf (dump_file, "\n");

      /* Put the code at the end of the BB, but before any final jump.  */
      if (!bitmap_empty_p (epi))
	{
	  start_sequence ();
	  targetm.shrink_wrap.emit_epilogue_components (epi);
	  rtx_insn *seq = get_insns ();
	  end_sequence ();
	  record_epilogue_seq (seq);

	  if (control_flow_insn_p (last_insn))
	    emit_insn_before (seq, last_insn);
	  else
	    emit_insn_after (seq, last_insn);

	  bitmap_ior (SW (bb)->tail_components, SW (bb)->tail_components, epi);
	}

      if (!bitmap_empty_p (pro))
	{
	  start_sequence ();
	  targetm.shrink_wrap.emit_prologue_components (pro);
	  rtx_insn *seq = get_insns ();
	  end_sequence ();
	  record_prologue_seq (seq);

	  if (control_flow_insn_p (last_insn))
	    emit_insn_before (seq, last_insn);
	  else
	    emit_insn_after (seq, last_insn);

	  bitmap_ior (SW (bb)->tail_components, SW (bb)->tail_components, pro);
	}
    }

  sbitmap_free (pro);
  sbitmap_free (epi);
  sbitmap_free (tmp);
}

/* Place prologues and epilogues for COMPONENTS on edges, if we haven't already
   placed them inside blocks directly.  */
static void
insert_prologue_epilogue_for_components (sbitmap components)
{
  sbitmap pro = sbitmap_alloc (SBITMAP_SIZE (components));
  sbitmap epi = sbitmap_alloc (SBITMAP_SIZE (components));

  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      if (!bb->aux)
	continue;

      edge e;
      edge_iterator ei;
      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  /* Find which pro/epilogue components are needed on this edge.  */
	  bitmap_and_compl (epi, SW (e->src)->has_components,
			    SW (e->dest)->has_components);
	  bitmap_and_compl (pro, SW (e->dest)->has_components,
			    SW (e->src)->has_components);
	  bitmap_and (epi, epi, components);
	  bitmap_and (pro, pro, components);

	  /* Deselect those we already have put at the head or tail of the
	     edge's dest resp. src.  */
	  bitmap_and_compl (epi, epi, SW (e->dest)->head_components);
	  bitmap_and_compl (pro, pro, SW (e->dest)->head_components);
	  bitmap_and_compl (epi, epi, SW (e->src)->tail_components);
	  bitmap_and_compl (pro, pro, SW (e->src)->tail_components);

	  if (!bitmap_empty_p (epi) || !bitmap_empty_p (pro))
	    {
	      if (dump_file)
		{
		  fprintf (dump_file, "  %d->%d", e->src->index,
			   e->dest->index);
		  dump_components ("epi", epi);
		  dump_components ("pro", pro);
		  if (e->flags & EDGE_SIBCALL)
		    fprintf (dump_file, "  (SIBCALL)");
		  else if (e->flags & EDGE_ABNORMAL)
		    fprintf (dump_file, "  (ABNORMAL)");
		  fprintf (dump_file, "\n");
		}

	      /* Put the epilogue components in place.  */
	      start_sequence ();
	      targetm.shrink_wrap.emit_epilogue_components (epi);
	      rtx_insn *seq = get_insns ();
	      end_sequence ();
	      record_epilogue_seq (seq);

	      if (e->flags & EDGE_SIBCALL)
		{
		  gcc_assert (e->dest == EXIT_BLOCK_PTR_FOR_FN (cfun));

		  rtx_insn *insn = BB_END (e->src);
		  gcc_assert (CALL_P (insn) && SIBLING_CALL_P (insn));
		  emit_insn_before (seq, insn);
		}
	      else if (e->dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
		{
		  gcc_assert (e->flags & EDGE_FALLTHRU);
		  basic_block new_bb = split_edge (e);
		  emit_insn_after (seq, BB_END (new_bb));
		}
	      else
		insert_insn_on_edge (seq, e);

	      /* Put the prologue components in place.  */
	      start_sequence ();
	      targetm.shrink_wrap.emit_prologue_components (pro);
	      seq = get_insns ();
	      end_sequence ();
	      record_prologue_seq (seq);

	      insert_insn_on_edge (seq, e);
	    }
	}
    }

  sbitmap_free (pro);
  sbitmap_free (epi);

  commit_edge_insertions ();
}

/* The main entry point to this subpass.  FIRST_BB is where the prologue
   would be normally put.  */
void
try_shrink_wrapping_separate (basic_block first_bb)
{
  if (HAVE_cc0)
    return;

  if (!(SHRINK_WRAPPING_ENABLED
	&& flag_shrink_wrap_separate
	&& optimize_function_for_speed_p (cfun)
	&& targetm.shrink_wrap.get_separate_components))
    return;

  /* We don't handle "strange" functions.  */
  if (cfun->calls_alloca
      || cfun->calls_setjmp
      || cfun->can_throw_non_call_exceptions
      || crtl->calls_eh_return
      || crtl->has_nonlocal_goto
      || crtl->saves_all_registers)
    return;

  /* Ask the target what components there are.  If it returns NULL, don't
     do anything.  */
  sbitmap components = targetm.shrink_wrap.get_separate_components ();
  if (!components)
    return;

  /* We need LIVE info, not defining anything in the entry block and not
     using anything in the exit block.  A block then needs a component if
     the register for that component is in the IN or GEN or KILL set for
     that block.  */
  df_scan->local_flags |= DF_SCAN_EMPTY_ENTRY_EXIT;
  df_update_entry_exit_and_calls ();
  df_live_add_problem ();
  df_live_set_all_dirty ();
  df_analyze ();

  calculate_dominance_info (CDI_DOMINATORS);
  calculate_dominance_info (CDI_POST_DOMINATORS);

  init_separate_shrink_wrap (components);

  sbitmap_iterator sbi;
  unsigned int j;
  EXECUTE_IF_SET_IN_BITMAP (components, 0, j, sbi)
    place_prologue_for_one_component (j, first_bb);

  spread_components (components);

  disqualify_problematic_components (components);

  /* Don't separately shrink-wrap anything where the "main" prologue will
     go; the target code can often optimize things if it is presented with
     all components together (say, if it generates store-multiple insns).  */
  bitmap_and_compl (components, components, SW (first_bb)->has_components);

  if (bitmap_empty_p (components))
    {
      if (dump_file)
	fprintf (dump_file, "Not wrapping anything separately.\n");
    }
  else
    {
      if (dump_file)
	{
	  fprintf (dump_file, "The components we wrap separately are");
	  dump_components ("sep", components);
	  fprintf (dump_file, "\n");

	  fprintf (dump_file, "... Inserting common heads...\n");
	}

      emit_common_heads_for_components (components);

      if (dump_file)
	fprintf (dump_file, "... Inserting common tails...\n");

      emit_common_tails_for_components (components);

      if (dump_file)
	fprintf (dump_file, "... Inserting the more difficult ones...\n");

      insert_prologue_epilogue_for_components (components);

      if (dump_file)
	fprintf (dump_file, "... Done.\n");

      targetm.shrink_wrap.set_handled_components (components);

      crtl->shrink_wrapped_separate = true;
    }

  fini_separate_shrink_wrap ();

  sbitmap_free (components);
  free_dominance_info (CDI_DOMINATORS);
  free_dominance_info (CDI_POST_DOMINATORS);

  /* All done.  */
  df_scan->local_flags &= ~DF_SCAN_EMPTY_ENTRY_EXIT;
  df_update_entry_exit_and_calls ();
  df_live_set_all_dirty ();
  df_analyze ();
}
