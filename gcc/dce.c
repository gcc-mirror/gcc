/* RTL dead code elimination.
   Copyright (C) 2005-2021 Free Software Foundation, Inc.

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
#include "tree.h"
#include "predict.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "emit-rtl.h"  /* FIXME: Can go away once crtl is moved to rtl.h.  */
#include "cfgrtl.h"
#include "cfgbuild.h"
#include "cfgcleanup.h"
#include "dce.h"
#include "valtrack.h"
#include "tree-pass.h"
#include "dbgcnt.h"
#include "rtl-iter.h"


/* -------------------------------------------------------------------------
   Core mark/delete routines
   ------------------------------------------------------------------------- */

/* True if we are invoked while the df engine is running; in this case,
   we don't want to reenter it.  */
static bool df_in_progress = false;

/* True if we are allowed to alter the CFG in this pass.  */
static bool can_alter_cfg = false;

/* Instructions that have been marked but whose dependencies have not
   yet been processed.  */
static vec<rtx_insn *> worklist;

/* Bitmap of instructions marked as needed indexed by INSN_UID.  */
static sbitmap marked;

/* Bitmap obstacks used for block processing by the fast algorithm.  */
static bitmap_obstack dce_blocks_bitmap_obstack;
static bitmap_obstack dce_tmp_bitmap_obstack;

static bool find_call_stack_args (rtx_call_insn *, bool, bool, bitmap);

/* A subroutine for which BODY is part of the instruction being tested;
   either the top-level pattern, or an element of a PARALLEL.  The
   instruction is known not to be a bare USE or CLOBBER.  */

static bool
deletable_insn_p_1 (rtx body)
{
  switch (GET_CODE (body))
    {
    case PREFETCH:
    case TRAP_IF:
      /* The UNSPEC case was added here because the ia-64 claims that
	 USEs do not work after reload and generates UNSPECS rather
	 than USEs.  Since dce is run after reload we need to avoid
	 deleting these even if they are dead.  If it turns out that
	 USEs really do work after reload, the ia-64 should be
	 changed, and the UNSPEC case can be removed.  */
    case UNSPEC:
      return false;

    default:
      return !volatile_refs_p (body);
    }
}

/* Don't delete calls that may throw if we cannot do so.  */

static bool
can_delete_call (rtx_insn *insn)
{
  if (cfun->can_delete_dead_exceptions && can_alter_cfg)
    return true;
  if (!insn_nothrow_p (insn))
    return false;
  if (can_alter_cfg)
    return true;
  /* If we can't alter cfg, even when the call can't throw exceptions, it
     might have EDGE_ABNORMAL_CALL edges and so we shouldn't delete such
     calls.  */
  gcc_assert (CALL_P (insn));
  if (BLOCK_FOR_INSN (insn) && BB_END (BLOCK_FOR_INSN (insn)) == insn)
    {
      edge e;
      edge_iterator ei;

      FOR_EACH_EDGE (e, ei, BLOCK_FOR_INSN (insn)->succs)
	if ((e->flags & EDGE_ABNORMAL_CALL) != 0)
	  return false;
    }
  return true;
}

/* Return true if INSN is a normal instruction that can be deleted by
   the DCE pass.  */

static bool
deletable_insn_p (rtx_insn *insn, bool fast, bitmap arg_stores)
{
  rtx body, x;
  int i;
  df_ref def;

  if (CALL_P (insn)
      /* We cannot delete calls inside of the recursive dce because
	 this may cause basic blocks to be deleted and this messes up
	 the rest of the stack of optimization passes.  */
      && (!df_in_progress)
      /* We cannot delete pure or const sibling calls because it is
	 hard to see the result.  */
      && (!SIBLING_CALL_P (insn))
      /* We can delete dead const or pure calls as long as they do not
         infinite loop.  */
      && (RTL_CONST_OR_PURE_CALL_P (insn)
	  && !RTL_LOOPING_CONST_OR_PURE_CALL_P (insn))
      /* Don't delete calls that may throw if we cannot do so.  */
      && can_delete_call (insn))
    return find_call_stack_args (as_a <rtx_call_insn *> (insn), false,
				 fast, arg_stores);

  /* Don't delete jumps, notes and the like.  */
  if (!NONJUMP_INSN_P (insn))
    return false;

  /* Don't delete insns that may throw if we cannot do so.  */
  if (!(cfun->can_delete_dead_exceptions && can_alter_cfg)
      && !insn_nothrow_p (insn))
    return false;

  /* If INSN sets a global_reg, leave it untouched.  */
  FOR_EACH_INSN_DEF (def, insn)
    if (HARD_REGISTER_NUM_P (DF_REF_REGNO (def))
	&& global_regs[DF_REF_REGNO (def)])
      return false;
    /* Initialization of pseudo PIC register should never be removed.  */
    else if (DF_REF_REG (def) == pic_offset_table_rtx
	     && REGNO (pic_offset_table_rtx) >= FIRST_PSEUDO_REGISTER)
      return false;

  /* Callee-save restores are needed.  */
  if (RTX_FRAME_RELATED_P (insn)
      && crtl->shrink_wrapped_separate
      && find_reg_note (insn, REG_CFA_RESTORE, NULL))
    return false;

  body = PATTERN (insn);
  switch (GET_CODE (body))
    {
    case USE:
    case VAR_LOCATION:
      return false;

    case CLOBBER:
      if (fast)
	{
	  /* A CLOBBER of a dead pseudo register serves no purpose.
	     That is not necessarily true for hard registers until
	     after reload.  */
	  x = XEXP (body, 0);
	  return REG_P (x) && (!HARD_REGISTER_P (x) || reload_completed);
	}
      else
	/* Because of the way that use-def chains are built, it is not
	   possible to tell if the clobber is dead because it can
	   never be the target of a use-def chain.  */
	return false;

    case PARALLEL:
      for (i = XVECLEN (body, 0) - 1; i >= 0; i--)
	if (!deletable_insn_p_1 (XVECEXP (body, 0, i)))
	  return false;
      return true;

    default:
      return deletable_insn_p_1 (body);
    }
}


/* Return true if INSN has been marked as needed.  */

static inline int
marked_insn_p (rtx_insn *insn)
{
  /* Artificial defs are always needed and they do not have an insn.
     We should never see them here.  */
  gcc_assert (insn);
  return bitmap_bit_p (marked, INSN_UID (insn));
}


/* If INSN has not yet been marked as needed, mark it now, and add it to
   the worklist.  */

static void
mark_insn (rtx_insn *insn, bool fast)
{
  if (!marked_insn_p (insn))
    {
      if (!fast)
	worklist.safe_push (insn);
      bitmap_set_bit (marked, INSN_UID (insn));
      if (dump_file)
	fprintf (dump_file, "  Adding insn %d to worklist\n", INSN_UID (insn));
      if (CALL_P (insn)
	  && !df_in_progress
	  && !SIBLING_CALL_P (insn)
	  && (RTL_CONST_OR_PURE_CALL_P (insn)
	      && !RTL_LOOPING_CONST_OR_PURE_CALL_P (insn))
	  && can_delete_call (insn))
	find_call_stack_args (as_a <rtx_call_insn *> (insn), true, fast, NULL);
    }
}


/* A note_stores callback used by mark_nonreg_stores.  DATA is the
   instruction containing DEST.  */

static void
mark_nonreg_stores_1 (rtx dest, const_rtx pattern, void *data)
{
  if (GET_CODE (pattern) != CLOBBER && !REG_P (dest))
    mark_insn ((rtx_insn *) data, true);
}


/* A note_stores callback used by mark_nonreg_stores.  DATA is the
   instruction containing DEST.  */

static void
mark_nonreg_stores_2 (rtx dest, const_rtx pattern, void *data)
{
  if (GET_CODE (pattern) != CLOBBER && !REG_P (dest))
    mark_insn ((rtx_insn *) data, false);
}


/* Mark INSN if it stores to a non-register destination.  */

static void
mark_nonreg_stores (rtx_insn *insn, bool fast)
{
  if (fast)
    note_stores (insn, mark_nonreg_stores_1, insn);
  else
    note_stores (insn, mark_nonreg_stores_2, insn);
}


/* Return true if a store to SIZE bytes, starting OFF bytes from stack pointer,
   is a call argument store, and clear corresponding bits from SP_BYTES
   bitmap if it is.  */

static bool
check_argument_store (HOST_WIDE_INT size, HOST_WIDE_INT off,
		      HOST_WIDE_INT min_sp_off, HOST_WIDE_INT max_sp_off,
		      bitmap sp_bytes)
{
  HOST_WIDE_INT byte;
  for (byte = off; byte < off + size; byte++)
    {
      if (byte < min_sp_off
	  || byte >= max_sp_off
	  || !bitmap_clear_bit (sp_bytes, byte - min_sp_off))
	return false;
    }
  return true;
}

/* If MEM has sp address, return 0, if it has sp + const address,
   return that const, if it has reg address where reg is set to sp + const
   and FAST is false, return const, otherwise return
   INTTYPE_MINUMUM (HOST_WIDE_INT).  */

static HOST_WIDE_INT
sp_based_mem_offset (rtx_call_insn *call_insn, const_rtx mem, bool fast)
{
  HOST_WIDE_INT off = 0;
  rtx addr = XEXP (mem, 0);
  if (GET_CODE (addr) == PLUS
      && REG_P (XEXP (addr, 0))
      && CONST_INT_P (XEXP (addr, 1)))
    {
      off = INTVAL (XEXP (addr, 1));
      addr = XEXP (addr, 0);
    }
  if (addr == stack_pointer_rtx)
    return off;

  if (!REG_P (addr) || fast)
    return INTTYPE_MINIMUM (HOST_WIDE_INT);

  /* If not fast, use chains to see if addr wasn't set to sp + offset.  */
  df_ref use;
  FOR_EACH_INSN_USE (use, call_insn)
  if (rtx_equal_p (addr, DF_REF_REG (use)))
    break;

  if (use == NULL)
    return INTTYPE_MINIMUM (HOST_WIDE_INT);

  struct df_link *defs;
  for (defs = DF_REF_CHAIN (use); defs; defs = defs->next)
    if (! DF_REF_IS_ARTIFICIAL (defs->ref))
      break;

  if (defs == NULL)
    return INTTYPE_MINIMUM (HOST_WIDE_INT);

  rtx set = single_set (DF_REF_INSN (defs->ref));
  if (!set)
    return INTTYPE_MINIMUM (HOST_WIDE_INT);

  if (GET_CODE (SET_SRC (set)) != PLUS
      || XEXP (SET_SRC (set), 0) != stack_pointer_rtx
      || !CONST_INT_P (XEXP (SET_SRC (set), 1)))
    return INTTYPE_MINIMUM (HOST_WIDE_INT);

  off += INTVAL (XEXP (SET_SRC (set), 1));
  return off;
}

/* Data for check_argument_load called via note_uses.  */
struct check_argument_load_data {
  bitmap sp_bytes;
  HOST_WIDE_INT min_sp_off, max_sp_off;
  rtx_call_insn *call_insn;
  bool fast;
  bool load_found;
};

/* Helper function for find_call_stack_args.  Check if there are
   any loads from the argument slots in between the const/pure call
   and store to the argument slot, set LOAD_FOUND if any is found.  */

static void
check_argument_load (rtx *loc, void *data)
{
  struct check_argument_load_data *d
    = (struct check_argument_load_data *) data;
  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, *loc, NONCONST)
    {
      const_rtx mem = *iter;
      HOST_WIDE_INT size;
      if (MEM_P (mem)
	  && MEM_SIZE_KNOWN_P (mem)
	  && MEM_SIZE (mem).is_constant (&size))
	{
	  HOST_WIDE_INT off = sp_based_mem_offset (d->call_insn, mem, d->fast);
	  if (off != INTTYPE_MINIMUM (HOST_WIDE_INT)
	      && off < d->max_sp_off
	      && off + size > d->min_sp_off)
	    for (HOST_WIDE_INT byte = MAX (off, d->min_sp_off);
		 byte < MIN (off + size, d->max_sp_off); byte++)
	      if (bitmap_bit_p (d->sp_bytes, byte - d->min_sp_off))
		{
		  d->load_found = true;
		  return;
		}
	}
    }
}

/* Try to find all stack stores of CALL_INSN arguments if
   ACCUMULATE_OUTGOING_ARGS.  If all stack stores have been found
   and it is therefore safe to eliminate the call, return true,
   otherwise return false.  This function should be first called
   with DO_MARK false, and only when the CALL_INSN is actually
   going to be marked called again with DO_MARK true.  */

static bool
find_call_stack_args (rtx_call_insn *call_insn, bool do_mark, bool fast,
		      bitmap arg_stores)
{
  rtx p;
  rtx_insn *insn, *prev_insn;
  bool ret;
  HOST_WIDE_INT min_sp_off, max_sp_off;
  bitmap sp_bytes;

  gcc_assert (CALL_P (call_insn));
  if (!ACCUMULATE_OUTGOING_ARGS)
    return true;

  if (!do_mark)
    {
      gcc_assert (arg_stores);
      bitmap_clear (arg_stores);
    }

  min_sp_off = INTTYPE_MAXIMUM (HOST_WIDE_INT);
  max_sp_off = 0;

  /* First determine the minimum and maximum offset from sp for
     stored arguments.  */
  for (p = CALL_INSN_FUNCTION_USAGE (call_insn); p; p = XEXP (p, 1))
    if (GET_CODE (XEXP (p, 0)) == USE
	&& MEM_P (XEXP (XEXP (p, 0), 0)))
      {
	rtx mem = XEXP (XEXP (p, 0), 0);
	HOST_WIDE_INT size;
	if (!MEM_SIZE_KNOWN_P (mem) || !MEM_SIZE (mem).is_constant (&size))
	  return false;
	HOST_WIDE_INT off = sp_based_mem_offset (call_insn, mem, fast);
	if (off == INTTYPE_MINIMUM (HOST_WIDE_INT))
	  return false;
	min_sp_off = MIN (min_sp_off, off);
	max_sp_off = MAX (max_sp_off, off + size);
      }

  if (min_sp_off >= max_sp_off)
    return true;
  sp_bytes = BITMAP_ALLOC (NULL);

  /* Set bits in SP_BYTES bitmap for bytes relative to sp + min_sp_off
     which contain arguments.  Checking has been done in the previous
     loop.  */
  for (p = CALL_INSN_FUNCTION_USAGE (call_insn); p; p = XEXP (p, 1))
    if (GET_CODE (XEXP (p, 0)) == USE
	&& MEM_P (XEXP (XEXP (p, 0), 0)))
      {
	rtx mem = XEXP (XEXP (p, 0), 0);
	/* Checked in the previous iteration.  */
	HOST_WIDE_INT size = MEM_SIZE (mem).to_constant ();
	HOST_WIDE_INT off = sp_based_mem_offset (call_insn, mem, fast);
	gcc_checking_assert (off != INTTYPE_MINIMUM (HOST_WIDE_INT));
	for (HOST_WIDE_INT byte = off; byte < off + size; byte++)
	  if (!bitmap_set_bit (sp_bytes, byte - min_sp_off))
	    gcc_unreachable ();
      }

  /* Walk backwards, looking for argument stores.  The search stops
     when seeing another call, sp adjustment, memory store other than
     argument store or a read from an argument stack slot.  */
  struct check_argument_load_data data
    = { sp_bytes, min_sp_off, max_sp_off, call_insn, fast, false };
  ret = false;
  for (insn = PREV_INSN (call_insn); insn; insn = prev_insn)
    {
      if (insn == BB_HEAD (BLOCK_FOR_INSN (call_insn)))
	prev_insn = NULL;
      else
	prev_insn = PREV_INSN (insn);

      if (CALL_P (insn))
	break;

      if (!NONDEBUG_INSN_P (insn))
	continue;

      rtx set = single_set (insn);
      if (!set || SET_DEST (set) == stack_pointer_rtx)
	break;

      note_uses (&PATTERN (insn), check_argument_load, &data);
      if (data.load_found)
	break;

      if (!MEM_P (SET_DEST (set)))
	continue;

      rtx mem = SET_DEST (set);
      HOST_WIDE_INT off = sp_based_mem_offset (call_insn, mem, fast);
      if (off == INTTYPE_MINIMUM (HOST_WIDE_INT))
	break;

      HOST_WIDE_INT size;
      if (!MEM_SIZE_KNOWN_P (mem)
	  || !MEM_SIZE (mem).is_constant (&size)
	  || !check_argument_store (size, off, min_sp_off,
				    max_sp_off, sp_bytes))
	break;

      if (!deletable_insn_p (insn, fast, NULL))
	break;

      if (do_mark)
	mark_insn (insn, fast);
      else
	bitmap_set_bit (arg_stores, INSN_UID (insn));

      if (bitmap_empty_p (sp_bytes))
	{
	  ret = true;
	  break;
	}
    }

  BITMAP_FREE (sp_bytes);
  if (!ret && arg_stores)
    bitmap_clear (arg_stores);

  return ret;
}


/* Remove all REG_EQUAL and REG_EQUIV notes referring to the registers INSN
   writes to.  */

static void
remove_reg_equal_equiv_notes_for_defs (rtx_insn *insn)
{
  df_ref def;

  FOR_EACH_INSN_DEF (def, insn)
    remove_reg_equal_equiv_notes_for_regno (DF_REF_REGNO (def));
}

/* Scan all BBs for debug insns and reset those that reference values
   defined in unmarked insns.  */

static void
reset_unmarked_insns_debug_uses (void)
{
  basic_block bb;
  rtx_insn *insn, *next;

  FOR_EACH_BB_REVERSE_FN (bb, cfun)
    FOR_BB_INSNS_REVERSE_SAFE (bb, insn, next)
      if (DEBUG_INSN_P (insn))
	{
	  df_ref use;

	  FOR_EACH_INSN_USE (use, insn)
	    {
	      struct df_link *defs;
	      for (defs = DF_REF_CHAIN (use); defs; defs = defs->next)
		{
		  rtx_insn *ref_insn;
		  if (DF_REF_IS_ARTIFICIAL (defs->ref))
		    continue;
		  ref_insn = DF_REF_INSN (defs->ref);
		  if (!marked_insn_p (ref_insn))
		    break;
		}
	      if (!defs)
		continue;
	      /* ??? FIXME could we propagate the values assigned to
		 each of the DEFs?  */
	      INSN_VAR_LOCATION_LOC (insn) = gen_rtx_UNKNOWN_VAR_LOC ();
	      df_insn_rescan_debug_internal (insn);
	      break;
	    }
	}
}

/* Delete every instruction that hasn't been marked.  */

static void
delete_unmarked_insns (void)
{
  basic_block bb;
  rtx_insn *insn, *next;
  bool must_clean = false;

  FOR_EACH_BB_REVERSE_FN (bb, cfun)
    FOR_BB_INSNS_REVERSE_SAFE (bb, insn, next)
      if (NONDEBUG_INSN_P (insn))
	{
	  rtx turn_into_use = NULL_RTX;

	  /* Always delete no-op moves.  */
	  if (noop_move_p (insn)
	      /* Unless the no-op move can throw and we are not allowed
		 to alter cfg.  */
	      && (!cfun->can_throw_non_call_exceptions
		  || (cfun->can_delete_dead_exceptions && can_alter_cfg)
		  || insn_nothrow_p (insn)))
	    {
	      if (RTX_FRAME_RELATED_P (insn))
		turn_into_use
		  = find_reg_note (insn, REG_CFA_RESTORE, NULL);
	      if (turn_into_use && REG_P (XEXP (turn_into_use, 0)))
		turn_into_use = XEXP (turn_into_use, 0);
	      else
		turn_into_use = NULL_RTX;
	    }

	  /* Otherwise rely only on the DCE algorithm.  */
	  else if (marked_insn_p (insn))
	    continue;

	  /* Beware that reaching a dbg counter limit here can result
	     in miscompiled file.  This occurs when a group of insns
	     must be deleted together, typically because the kept insn
	     depends on the output from the deleted insn.  Deleting
	     this insns in reverse order (both at the bb level and
	     when looking at the blocks) minimizes this, but does not
	     eliminate it, since it is possible for the using insn to
	     be top of a block and the producer to be at the bottom of
	     the block.  However, in most cases this will only result
	     in an uninitialized use of an insn that is dead anyway.

	     However, there is one rare case that will cause a
	     miscompile: deletion of non-looping pure and constant
	     calls on a machine where ACCUMULATE_OUTGOING_ARGS is true.
	     In this case it is possible to remove the call, but leave
	     the argument pushes to the stack.  Because of the changes
	     to the stack pointer, this will almost always lead to a
	     miscompile.  */
	  if (!dbg_cnt (dce))
	    continue;

	  if (dump_file)
	    fprintf (dump_file, "DCE: Deleting insn %d\n", INSN_UID (insn));

	  /* Before we delete the insn we have to remove the REG_EQUAL notes
	     for the destination regs in order to avoid dangling notes.  */
	  remove_reg_equal_equiv_notes_for_defs (insn);

	  if (turn_into_use)
	    {
	      /* Don't remove frame related noop moves if they cary
		 REG_CFA_RESTORE note, while we don't need to emit any code,
		 we need it to emit the CFI restore note.  */
	      PATTERN (insn)
		= gen_rtx_USE (GET_MODE (turn_into_use), turn_into_use);
	      INSN_CODE (insn) = -1;
	      df_insn_rescan (insn);
	    }
	  else
	    /* Now delete the insn.  */
	    must_clean |= delete_insn_and_edges (insn);
	}

  /* Deleted a pure or const call.  */
  if (must_clean)
    {
      gcc_assert (can_alter_cfg);
      delete_unreachable_blocks ();
      free_dominance_info (CDI_DOMINATORS);
    }
}


/* Go through the instructions and mark those whose necessity is not
   dependent on inter-instruction information.  Make sure all other
   instructions are not marked.  */

static void
prescan_insns_for_dce (bool fast)
{
  basic_block bb;
  rtx_insn *insn, *prev;
  bitmap arg_stores = NULL;

  if (dump_file)
    fprintf (dump_file, "Finding needed instructions:\n");

  if (!df_in_progress && ACCUMULATE_OUTGOING_ARGS)
    arg_stores = BITMAP_ALLOC (NULL);

  FOR_EACH_BB_FN (bb, cfun)
    {
      FOR_BB_INSNS_REVERSE_SAFE (bb, insn, prev)
	if (NONDEBUG_INSN_P (insn))
	  {
	    /* Don't mark argument stores now.  They will be marked
	       if needed when the associated CALL is marked.  */
	    if (arg_stores && bitmap_bit_p (arg_stores, INSN_UID (insn)))
	      continue;
	    if (deletable_insn_p (insn, fast, arg_stores))
	      mark_nonreg_stores (insn, fast);
	    else
	      mark_insn (insn, fast);
	  }
      /* find_call_stack_args only looks at argument stores in the
	 same bb.  */
      if (arg_stores)
	bitmap_clear (arg_stores);
    }

  if (arg_stores)
    BITMAP_FREE (arg_stores);

  if (dump_file)
    fprintf (dump_file, "Finished finding needed instructions:\n");
}


/* UD-based DSE routines. */

/* Mark instructions that define artificially-used registers, such as
   the frame pointer and the stack pointer.  */

static void
mark_artificial_uses (void)
{
  basic_block bb;
  struct df_link *defs;
  df_ref use;

  FOR_ALL_BB_FN (bb, cfun)
    FOR_EACH_ARTIFICIAL_USE (use, bb->index)
      for (defs = DF_REF_CHAIN (use); defs; defs = defs->next)
	if (!DF_REF_IS_ARTIFICIAL (defs->ref))
	  mark_insn (DF_REF_INSN (defs->ref), false);
}


/* Mark every instruction that defines a register value that INSN uses.  */

static void
mark_reg_dependencies (rtx_insn *insn)
{
  struct df_link *defs;
  df_ref use;

  if (DEBUG_INSN_P (insn))
    return;

  FOR_EACH_INSN_USE (use, insn)
    {
      if (dump_file)
	{
	  fprintf (dump_file, "Processing use of ");
	  print_simple_rtl (dump_file, DF_REF_REG (use));
	  fprintf (dump_file, " in insn %d:\n", INSN_UID (insn));
	}
      for (defs = DF_REF_CHAIN (use); defs; defs = defs->next)
	if (! DF_REF_IS_ARTIFICIAL (defs->ref))
	  mark_insn (DF_REF_INSN (defs->ref), false);
    }
}


/* Initialize global variables for a new DCE pass.  */

static void
init_dce (bool fast)
{
  if (!df_in_progress)
    {
      if (!fast)
	{
	  df_set_flags (DF_RD_PRUNE_DEAD_DEFS);
	  df_chain_add_problem (DF_UD_CHAIN);
	}
      df_analyze ();
    }

  if (dump_file)
    df_dump (dump_file);

  if (fast)
    {
      bitmap_obstack_initialize (&dce_blocks_bitmap_obstack);
      bitmap_obstack_initialize (&dce_tmp_bitmap_obstack);
      can_alter_cfg = false;
    }
  else
    can_alter_cfg = true;

  marked = sbitmap_alloc (get_max_uid () + 1);
  bitmap_clear (marked);
}


/* Free the data allocated by init_dce.  */

static void
fini_dce (bool fast)
{
  sbitmap_free (marked);

  if (fast)
    {
      bitmap_obstack_release (&dce_blocks_bitmap_obstack);
      bitmap_obstack_release (&dce_tmp_bitmap_obstack);
    }
}


/* UD-chain based DCE.  */

static unsigned int
rest_of_handle_ud_dce (void)
{
  rtx_insn *insn;

  init_dce (false);

  prescan_insns_for_dce (false);
  mark_artificial_uses ();
  while (worklist.length () > 0)
    {
      insn = worklist.pop ();
      mark_reg_dependencies (insn);
    }
  worklist.release ();

  if (MAY_HAVE_DEBUG_BIND_INSNS)
    reset_unmarked_insns_debug_uses ();

  /* Before any insns are deleted, we must remove the chains since
     they are not bidirectional.  */
  df_remove_problem (df_chain);
  delete_unmarked_insns ();

  fini_dce (false);
  return 0;
}


namespace {

const pass_data pass_data_ud_rtl_dce =
{
  RTL_PASS, /* type */
  "ud_dce", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_DCE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_df_finish, /* todo_flags_finish */
};

class pass_ud_rtl_dce : public rtl_opt_pass
{
public:
  pass_ud_rtl_dce (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_ud_rtl_dce, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      return optimize > 1 && flag_dce && dbg_cnt (dce_ud);
    }

  virtual unsigned int execute (function *)
    {
      return rest_of_handle_ud_dce ();
    }

}; // class pass_ud_rtl_dce

} // anon namespace

rtl_opt_pass *
make_pass_ud_rtl_dce (gcc::context *ctxt)
{
  return new pass_ud_rtl_dce (ctxt);
}


/* -------------------------------------------------------------------------
   Fast DCE functions
   ------------------------------------------------------------------------- */

/* Process basic block BB.  Return true if the live_in set has
   changed. REDO_OUT is true if the info at the bottom of the block
   needs to be recalculated before starting.  AU is the proper set of
   artificial uses.  Track global substitution of uses of dead pseudos
   in debug insns using GLOBAL_DEBUG.  */

static bool
word_dce_process_block (basic_block bb, bool redo_out,
			struct dead_debug_global *global_debug)
{
  bitmap local_live = BITMAP_ALLOC (&dce_tmp_bitmap_obstack);
  rtx_insn *insn;
  bool block_changed;
  struct dead_debug_local debug;

  if (redo_out)
    {
      /* Need to redo the live_out set of this block if when one of
	 the succs of this block has had a change in it live in
	 set.  */
      edge e;
      edge_iterator ei;
      df_confluence_function_n con_fun_n = df_word_lr->problem->con_fun_n;
      bitmap_clear (DF_WORD_LR_OUT (bb));
      FOR_EACH_EDGE (e, ei, bb->succs)
	(*con_fun_n) (e);
    }

  if (dump_file)
    {
      fprintf (dump_file, "processing block %d live out = ", bb->index);
      df_print_word_regset (dump_file, DF_WORD_LR_OUT (bb));
    }

  bitmap_copy (local_live, DF_WORD_LR_OUT (bb));
  dead_debug_local_init (&debug, NULL, global_debug);

  FOR_BB_INSNS_REVERSE (bb, insn)
    if (DEBUG_INSN_P (insn))
      {
	df_ref use;
	FOR_EACH_INSN_USE (use, insn)
	  if (DF_REF_REGNO (use) >= FIRST_PSEUDO_REGISTER
	      && known_eq (GET_MODE_SIZE (GET_MODE (DF_REF_REAL_REG (use))),
			   2 * UNITS_PER_WORD)
	      && !bitmap_bit_p (local_live, 2 * DF_REF_REGNO (use))
	      && !bitmap_bit_p (local_live, 2 * DF_REF_REGNO (use) + 1))
	    dead_debug_add (&debug, use, DF_REF_REGNO (use));
      }
    else if (INSN_P (insn))
      {
	bool any_changed;

	/* No matter if the instruction is needed or not, we remove
	   any regno in the defs from the live set.  */
	any_changed = df_word_lr_simulate_defs (insn, local_live);
	if (any_changed)
	  mark_insn (insn, true);

	/* On the other hand, we do not allow the dead uses to set
	   anything in local_live.  */
	if (marked_insn_p (insn))
	  df_word_lr_simulate_uses (insn, local_live);

	/* Insert debug temps for dead REGs used in subsequent debug
	   insns.  We may have to emit a debug temp even if the insn
	   was marked, in case the debug use was after the point of
	   death.  */
	if (debug.used && !bitmap_empty_p (debug.used))
	  {
	    df_ref def;

	    FOR_EACH_INSN_DEF (def, insn)
	      dead_debug_insert_temp (&debug, DF_REF_REGNO (def), insn,
				      marked_insn_p (insn)
				      && !control_flow_insn_p (insn)
				      ? DEBUG_TEMP_AFTER_WITH_REG_FORCE
				      : DEBUG_TEMP_BEFORE_WITH_VALUE);
	  }

	if (dump_file)
	  {
	    fprintf (dump_file, "finished processing insn %d live out = ",
		     INSN_UID (insn));
	    df_print_word_regset (dump_file, local_live);
	  }
      }

  block_changed = !bitmap_equal_p (local_live, DF_WORD_LR_IN (bb));
  if (block_changed)
    bitmap_copy (DF_WORD_LR_IN (bb), local_live);

  dead_debug_local_finish (&debug, NULL);
  BITMAP_FREE (local_live);
  return block_changed;
}


/* Process basic block BB.  Return true if the live_in set has
   changed. REDO_OUT is true if the info at the bottom of the block
   needs to be recalculated before starting.  AU is the proper set of
   artificial uses.  Track global substitution of uses of dead pseudos
   in debug insns using GLOBAL_DEBUG.  */

static bool
dce_process_block (basic_block bb, bool redo_out, bitmap au,
		   struct dead_debug_global *global_debug)
{
  bitmap local_live = BITMAP_ALLOC (&dce_tmp_bitmap_obstack);
  rtx_insn *insn;
  bool block_changed;
  df_ref def;
  struct dead_debug_local debug;

  if (redo_out)
    {
      /* Need to redo the live_out set of this block if when one of
	 the succs of this block has had a change in it live in
	 set.  */
      edge e;
      edge_iterator ei;
      df_confluence_function_n con_fun_n = df_lr->problem->con_fun_n;
      bitmap_clear (DF_LR_OUT (bb));
      FOR_EACH_EDGE (e, ei, bb->succs)
	(*con_fun_n) (e);
    }

  if (dump_file)
    {
      fprintf (dump_file, "processing block %d lr out = ", bb->index);
      df_print_regset (dump_file, DF_LR_OUT (bb));
    }

  bitmap_copy (local_live, DF_LR_OUT (bb));

  df_simulate_initialize_backwards (bb, local_live);
  dead_debug_local_init (&debug, NULL, global_debug);

  FOR_BB_INSNS_REVERSE (bb, insn)
    if (DEBUG_INSN_P (insn))
      {
	df_ref use;
	FOR_EACH_INSN_USE (use, insn)
	  if (!bitmap_bit_p (local_live, DF_REF_REGNO (use))
	      && !bitmap_bit_p (au, DF_REF_REGNO (use)))
	    dead_debug_add (&debug, use, DF_REF_REGNO (use));
      }
    else if (INSN_P (insn))
      {
	bool needed = marked_insn_p (insn);

	/* The insn is needed if there is someone who uses the output.  */
	if (!needed)
	  FOR_EACH_INSN_DEF (def, insn)
	    if (bitmap_bit_p (local_live, DF_REF_REGNO (def))
		|| bitmap_bit_p (au, DF_REF_REGNO (def)))
	      {
		needed = true;
		mark_insn (insn, true);
		break;
	      }

	/* No matter if the instruction is needed or not, we remove
	   any regno in the defs from the live set.  */
	df_simulate_defs (insn, local_live);

	/* On the other hand, we do not allow the dead uses to set
	   anything in local_live.  */
	if (needed)
	  df_simulate_uses (insn, local_live);

	/* Insert debug temps for dead REGs used in subsequent debug
	   insns.  We may have to emit a debug temp even if the insn
	   was marked, in case the debug use was after the point of
	   death.  */
	if (debug.used && !bitmap_empty_p (debug.used))
	  FOR_EACH_INSN_DEF (def, insn)
	    dead_debug_insert_temp (&debug, DF_REF_REGNO (def), insn,
				    needed && !control_flow_insn_p (insn)
				    ? DEBUG_TEMP_AFTER_WITH_REG_FORCE
				    : DEBUG_TEMP_BEFORE_WITH_VALUE);
      }

  dead_debug_local_finish (&debug, NULL);
  df_simulate_finalize_backwards (bb, local_live);

  block_changed = !bitmap_equal_p (local_live, DF_LR_IN (bb));
  if (block_changed)
    bitmap_copy (DF_LR_IN (bb), local_live);

  BITMAP_FREE (local_live);
  return block_changed;
}


/* Perform fast DCE once initialization is done.  If WORD_LEVEL is
   true, use the word level dce, otherwise do it at the pseudo
   level.  */

static void
fast_dce (bool word_level)
{
  int *postorder = df_get_postorder (DF_BACKWARD);
  int n_blocks = df_get_n_blocks (DF_BACKWARD);
  /* The set of blocks that have been seen on this iteration.  */
  bitmap processed = BITMAP_ALLOC (&dce_blocks_bitmap_obstack);
  /* The set of blocks that need to have the out vectors reset because
     the in of one of their successors has changed.  */
  bitmap redo_out = BITMAP_ALLOC (&dce_blocks_bitmap_obstack);
  bitmap all_blocks = BITMAP_ALLOC (&dce_blocks_bitmap_obstack);
  bool global_changed = true;

  /* These regs are considered always live so if they end up dying
     because of some def, we need to bring the back again.  Calling
     df_simulate_fixup_sets has the disadvantage of calling
     bb_has_eh_pred once per insn, so we cache the information
     here.  */
  bitmap au = &df->regular_block_artificial_uses;
  bitmap au_eh = &df->eh_block_artificial_uses;
  int i;
  struct dead_debug_global global_debug;

  prescan_insns_for_dce (true);

  for (i = 0; i < n_blocks; i++)
    bitmap_set_bit (all_blocks, postorder[i]);

  dead_debug_global_init (&global_debug, NULL);

  while (global_changed)
    {
      global_changed = false;

      for (i = 0; i < n_blocks; i++)
	{
	  int index = postorder[i];
	  basic_block bb = BASIC_BLOCK_FOR_FN (cfun, index);
	  bool local_changed;

	  if (index < NUM_FIXED_BLOCKS)
	    {
	      bitmap_set_bit (processed, index);
	      continue;
	    }

	  if (word_level)
	    local_changed
	      = word_dce_process_block (bb, bitmap_bit_p (redo_out, index),
					&global_debug);
	  else
	    local_changed
	      = dce_process_block (bb, bitmap_bit_p (redo_out, index),
				   bb_has_eh_pred (bb) ? au_eh : au,
				   &global_debug);
	  bitmap_set_bit (processed, index);

	  if (local_changed)
	    {
	      edge e;
	      edge_iterator ei;
	      FOR_EACH_EDGE (e, ei, bb->preds)
		if (bitmap_bit_p (processed, e->src->index))
		  /* Be tricky about when we need to iterate the
		     analysis.  We only have redo the analysis if the
		     bitmaps change at the top of a block that is the
		     entry to a loop.  */
		  global_changed = true;
		else
		  bitmap_set_bit (redo_out, e->src->index);
	    }
	}

      if (global_changed)
	{
	  /* Turn off the RUN_DCE flag to prevent recursive calls to
	     dce.  */
	  int old_flag = df_clear_flags (DF_LR_RUN_DCE);

	  /* So something was deleted that requires a redo.  Do it on
	     the cheap.  */
	  delete_unmarked_insns ();
	  bitmap_clear (marked);
	  bitmap_clear (processed);
	  bitmap_clear (redo_out);

	  /* We do not need to rescan any instructions.  We only need
	     to redo the dataflow equations for the blocks that had a
	     change at the top of the block.  Then we need to redo the
	     iteration.  */
	  if (word_level)
	    df_analyze_problem (df_word_lr, all_blocks, postorder, n_blocks);
	  else
	    df_analyze_problem (df_lr, all_blocks, postorder, n_blocks);

	  if (old_flag & DF_LR_RUN_DCE)
	    df_set_flags (DF_LR_RUN_DCE);

	  prescan_insns_for_dce (true);
	}
    }

  dead_debug_global_finish (&global_debug, NULL);

  delete_unmarked_insns ();

  BITMAP_FREE (processed);
  BITMAP_FREE (redo_out);
  BITMAP_FREE (all_blocks);
}


/* Fast register level DCE.  */

static unsigned int
rest_of_handle_fast_dce (void)
{
  init_dce (true);
  fast_dce (false);
  fini_dce (true);
  return 0;
}


/* Fast byte level DCE.  */

void
run_word_dce (void)
{
  int old_flags;

  if (!flag_dce)
    return;

  timevar_push (TV_DCE);
  old_flags = df_clear_flags (DF_DEFER_INSN_RESCAN + DF_NO_INSN_RESCAN);
  df_word_lr_add_problem ();
  init_dce (true);
  fast_dce (true);
  fini_dce (true);
  df_set_flags (old_flags);
  timevar_pop (TV_DCE);
}


/* This is an internal call that is used by the df live register
   problem to run fast dce as a side effect of creating the live
   information.  The stack is organized so that the lr problem is run,
   this pass is run, which updates the live info and the df scanning
   info, and then returns to allow the rest of the problems to be run.

   This can be called by elsewhere but it will not update the bit
   vectors for any other problems than LR.  */

void
run_fast_df_dce (void)
{
  if (flag_dce)
    {
      /* If dce is able to delete something, it has to happen
	 immediately.  Otherwise there will be problems handling the
	 eq_notes.  */
      int old_flags =
	df_clear_flags (DF_DEFER_INSN_RESCAN + DF_NO_INSN_RESCAN);

      df_in_progress = true;
      rest_of_handle_fast_dce ();
      df_in_progress = false;

      df_set_flags (old_flags);
    }
}


/* Run a fast DCE pass.  */

void
run_fast_dce (void)
{
  if (flag_dce)
    rest_of_handle_fast_dce ();
}


namespace {

const pass_data pass_data_fast_rtl_dce =
{
  RTL_PASS, /* type */
  "rtl_dce", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_DCE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_df_finish, /* todo_flags_finish */
};

class pass_fast_rtl_dce : public rtl_opt_pass
{
public:
  pass_fast_rtl_dce (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_fast_rtl_dce, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      return optimize > 0 && flag_dce && dbg_cnt (dce_fast);
    }

  virtual unsigned int execute (function *)
    {
      return rest_of_handle_fast_dce ();
    }

}; // class pass_fast_rtl_dce

} // anon namespace

rtl_opt_pass *
make_pass_fast_rtl_dce (gcc::context *ctxt)
{
  return new pass_fast_rtl_dce (ctxt);
}
