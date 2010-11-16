/* RTL dead code elimination.
   Copyright (C) 2005, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.

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
#include "hashtab.h"
#include "tm.h"
#include "rtl.h"
#include "tree.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "flags.h"
#include "except.h"
#include "df.h"
#include "cselib.h"
#include "dce.h"
#include "timevar.h"
#include "tree-pass.h"
#include "dbgcnt.h"
#include "tm_p.h"


/* -------------------------------------------------------------------------
   Core mark/delete routines
   ------------------------------------------------------------------------- */

/* True if we are invoked while the df engine is running; in this case,
   we don't want to reenter it.  */
static bool df_in_progress = false;

/* Instructions that have been marked but whose dependencies have not
   yet been processed.  */
static VEC(rtx,heap) *worklist;

/* Bitmap of instructions marked as needed indexed by INSN_UID.  */
static sbitmap marked;

/* Bitmap obstacks used for block processing by the fast algorithm.  */
static bitmap_obstack dce_blocks_bitmap_obstack;
static bitmap_obstack dce_tmp_bitmap_obstack;

static bool find_call_stack_args (rtx, bool, bool, bitmap);

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


/* Return true if INSN is a normal instruction that can be deleted by
   the DCE pass.  */

static bool
deletable_insn_p (rtx insn, bool fast, bitmap arg_stores)
{
  rtx body, x;
  int i;

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
	  && !RTL_LOOPING_CONST_OR_PURE_CALL_P (insn)))
    return find_call_stack_args (insn, false, fast, arg_stores);

  /* Don't delete jumps, notes and the like.  */
  if (!NONJUMP_INSN_P (insn))
    return false;

  /* Don't delete insns that can throw.  */
  if (!insn_nothrow_p (insn))
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
marked_insn_p (rtx insn)
{
  /* Artificial defs are always needed and they do not have an insn.
     We should never see them here.  */
  gcc_assert (insn);
  return TEST_BIT (marked, INSN_UID (insn));
}


/* If INSN has not yet been marked as needed, mark it now, and add it to
   the worklist.  */

static void
mark_insn (rtx insn, bool fast)
{
  if (!marked_insn_p (insn))
    {
      if (!fast)
	VEC_safe_push (rtx, heap, worklist, insn);
      SET_BIT (marked, INSN_UID (insn));
      if (dump_file)
	fprintf (dump_file, "  Adding insn %d to worklist\n", INSN_UID (insn));
      if (CALL_P (insn)
	  && !df_in_progress
	  && !SIBLING_CALL_P (insn)
	  && (RTL_CONST_OR_PURE_CALL_P (insn)
	      && !RTL_LOOPING_CONST_OR_PURE_CALL_P (insn)))
	find_call_stack_args (insn, true, fast, NULL);
    }
}


/* A note_stores callback used by mark_nonreg_stores.  DATA is the
   instruction containing DEST.  */

static void
mark_nonreg_stores_1 (rtx dest, const_rtx pattern, void *data)
{
  if (GET_CODE (pattern) != CLOBBER && !REG_P (dest))
    mark_insn ((rtx) data, true);
}


/* A note_stores callback used by mark_nonreg_stores.  DATA is the
   instruction containing DEST.  */

static void
mark_nonreg_stores_2 (rtx dest, const_rtx pattern, void *data)
{
  if (GET_CODE (pattern) != CLOBBER && !REG_P (dest))
    mark_insn ((rtx) data, false);
}


/* Mark INSN if BODY stores to a non-register destination.  */

static void
mark_nonreg_stores (rtx body, rtx insn, bool fast)
{
  if (fast)
    note_stores (body, mark_nonreg_stores_1, insn);
  else
    note_stores (body, mark_nonreg_stores_2, insn);
}


/* Try to find all stack stores of CALL_INSN arguments if
   ACCUMULATE_OUTGOING_ARGS.  If all stack stores have been found
   and it is therefore safe to eliminate the call, return true,
   otherwise return false.  This function should be first called
   with DO_MARK false, and only when the CALL_INSN is actually
   going to be marked called again with DO_MARK true.  */

static bool
find_call_stack_args (rtx call_insn, bool do_mark, bool fast,
		      bitmap arg_stores)
{
  rtx p, insn, prev_insn;
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
	rtx mem = XEXP (XEXP (p, 0), 0), addr, size;
	HOST_WIDE_INT off = 0;
	size = MEM_SIZE (mem);
	if (size == NULL_RTX)
	  return false;
	addr = XEXP (mem, 0);
	if (GET_CODE (addr) == PLUS
	    && REG_P (XEXP (addr, 0))
	    && CONST_INT_P (XEXP (addr, 1)))
	  {
	    off = INTVAL (XEXP (addr, 1));
	    addr = XEXP (addr, 0);
	  }
	if (addr != stack_pointer_rtx)
	  {
	    if (!REG_P (addr))
	      return false;
	    /* If not fast, use chains to see if addr wasn't set to
	       sp + offset.  */
	    if (!fast)
	      {
		df_ref *use_rec;
		struct df_link *defs;
		rtx set;

		for (use_rec = DF_INSN_USES (call_insn); *use_rec; use_rec++)
		  if (rtx_equal_p (addr, DF_REF_REG (*use_rec)))
		    break;

		if (*use_rec == NULL)
		  return false;

		for (defs = DF_REF_CHAIN (*use_rec); defs; defs = defs->next)
		  if (! DF_REF_IS_ARTIFICIAL (defs->ref))
		    break;

		if (defs == NULL)
		  return false;

		set = single_set (DF_REF_INSN (defs->ref));
		if (!set)
		  return false;

		if (GET_CODE (SET_SRC (set)) != PLUS
		    || XEXP (SET_SRC (set), 0) != stack_pointer_rtx
		    || !CONST_INT_P (XEXP (SET_SRC (set), 1)))
		  return false;

		off += INTVAL (XEXP (SET_SRC (set), 1));
	      }
	    else
	      return false;
	  }
	min_sp_off = MIN (min_sp_off, off);
	max_sp_off = MAX (max_sp_off, off + INTVAL (size));
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
	rtx mem = XEXP (XEXP (p, 0), 0), addr;
	HOST_WIDE_INT off = 0, byte;
	addr = XEXP (mem, 0);
	if (GET_CODE (addr) == PLUS
	    && REG_P (XEXP (addr, 0))
	    && CONST_INT_P (XEXP (addr, 1)))
	  {
	    off = INTVAL (XEXP (addr, 1));
	    addr = XEXP (addr, 0);
	  }
	if (addr != stack_pointer_rtx)
	  {
	    df_ref *use_rec;
	    struct df_link *defs;
	    rtx set;

	    for (use_rec = DF_INSN_USES (call_insn); *use_rec; use_rec++)
	      if (rtx_equal_p (addr, DF_REF_REG (*use_rec)))
		break;

	    for (defs = DF_REF_CHAIN (*use_rec); defs; defs = defs->next)
	      if (! DF_REF_IS_ARTIFICIAL (defs->ref))
		break;

	    set = single_set (DF_REF_INSN (defs->ref));
	    off += INTVAL (XEXP (SET_SRC (set), 1));
	  }
	for (byte = off; byte < off + INTVAL (MEM_SIZE (mem)); byte++)
	  {
	    if (!bitmap_set_bit (sp_bytes, byte - min_sp_off))
	      gcc_unreachable ();
	  }
      }

  /* Walk backwards, looking for argument stores.  The search stops
     when seeing another call, sp adjustment or memory store other than
     argument store.  */
  ret = false;
  for (insn = PREV_INSN (call_insn); insn; insn = prev_insn)
    {
      rtx set, mem, addr;
      HOST_WIDE_INT off, byte;

      if (insn == BB_HEAD (BLOCK_FOR_INSN (call_insn)))
	prev_insn = NULL_RTX;
      else
	prev_insn = PREV_INSN (insn);

      if (CALL_P (insn))
	break;

      if (!INSN_P (insn))
	continue;

      set = single_set (insn);
      if (!set || SET_DEST (set) == stack_pointer_rtx)
	break;

      if (!MEM_P (SET_DEST (set)))
	continue;

      mem = SET_DEST (set);
      addr = XEXP (mem, 0);
      off = 0;
      if (GET_CODE (addr) == PLUS
	  && REG_P (XEXP (addr, 0))
	  && CONST_INT_P (XEXP (addr, 1)))
	{
	  off = INTVAL (XEXP (addr, 1));
	  addr = XEXP (addr, 0);
	}
      if (addr != stack_pointer_rtx)
	{
	  if (!REG_P (addr))
	    break;
	  if (!fast)
	    {
	      df_ref *use_rec;
	      struct df_link *defs;
	      rtx set;

	      for (use_rec = DF_INSN_USES (insn); *use_rec; use_rec++)
		if (rtx_equal_p (addr, DF_REF_REG (*use_rec)))
		  break;

	      if (*use_rec == NULL)
		break;

	      for (defs = DF_REF_CHAIN (*use_rec); defs; defs = defs->next)
		if (! DF_REF_IS_ARTIFICIAL (defs->ref))
		  break;

	      if (defs == NULL)
		break;

	      set = single_set (DF_REF_INSN (defs->ref));
	      if (!set)
		break;

	      if (GET_CODE (SET_SRC (set)) != PLUS
		  || XEXP (SET_SRC (set), 0) != stack_pointer_rtx
		  || !CONST_INT_P (XEXP (SET_SRC (set), 1)))
		break;

	      off += INTVAL (XEXP (SET_SRC (set), 1));
	    }
	  else
	    break;
	}

      if (GET_MODE_SIZE (GET_MODE (mem)) == 0)
	break;

      for (byte = off; byte < off + GET_MODE_SIZE (GET_MODE (mem)); byte++)
	{
	  if (byte < min_sp_off
	      || byte >= max_sp_off
	      || !bitmap_clear_bit (sp_bytes, byte - min_sp_off))
	    break;
	}

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
remove_reg_equal_equiv_notes_for_defs (rtx insn)
{
  df_ref *def_rec;

  for (def_rec = DF_INSN_DEFS (insn); *def_rec; def_rec++)
    remove_reg_equal_equiv_notes_for_regno (DF_REF_REGNO (*def_rec));
}


/* Delete every instruction that hasn't been marked.  */

static void
delete_unmarked_insns (void)
{
  basic_block bb;
  rtx insn, next;
  bool must_clean = false;

  FOR_EACH_BB_REVERSE (bb)
    FOR_BB_INSNS_REVERSE_SAFE (bb, insn, next)
      if (INSN_P (insn))
	{
	  /* Always delete no-op moves.  */
	  if (noop_move_p (insn))
	    ;

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

	  /* If a pure or const call is deleted, this may make the cfg
	     have unreachable blocks.  We rememeber this and call
	     delete_unreachable_blocks at the end.  */
	  if (CALL_P (insn))
	    must_clean = true;

	  /* Now delete the insn.  */
	  delete_insn_and_edges (insn);
	}

  /* Deleted a pure or const call.  */
  if (must_clean)
    delete_unreachable_blocks ();
}


/* Go through the instructions and mark those whose necessity is not
   dependent on inter-instruction information.  Make sure all other
   instructions are not marked.  */

static void
prescan_insns_for_dce (bool fast)
{
  basic_block bb;
  rtx insn, prev;
  bitmap arg_stores = NULL;

  if (dump_file)
    fprintf (dump_file, "Finding needed instructions:\n");

  if (!df_in_progress && ACCUMULATE_OUTGOING_ARGS)
    arg_stores = BITMAP_ALLOC (NULL);

  FOR_EACH_BB (bb)
    {
      FOR_BB_INSNS_REVERSE_SAFE (bb, insn, prev)
	if (INSN_P (insn))
	  {
	    /* Don't mark argument stores now.  They will be marked
	       if needed when the associated CALL is marked.  */
	    if (arg_stores && bitmap_bit_p (arg_stores, INSN_UID (insn)))
	      continue;
	    if (deletable_insn_p (insn, fast, arg_stores))
	      mark_nonreg_stores (PATTERN (insn), insn, fast);
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
  df_ref *use_rec;

  FOR_ALL_BB (bb)
    {
      for (use_rec = df_get_artificial_uses (bb->index);
	   *use_rec; use_rec++)
	for (defs = DF_REF_CHAIN (*use_rec); defs; defs = defs->next)
	  if (! DF_REF_IS_ARTIFICIAL (defs->ref))
	    mark_insn (DF_REF_INSN (defs->ref), false);
    }
}


/* Mark every instruction that defines a register value that INSN uses.  */

static void
mark_reg_dependencies (rtx insn)
{
  struct df_link *defs;
  df_ref *use_rec;

  if (DEBUG_INSN_P (insn))
    return;

  for (use_rec = DF_INSN_USES (insn); *use_rec; use_rec++)
    {
      df_ref use = *use_rec;
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
	df_chain_add_problem (DF_UD_CHAIN);
      df_analyze ();
    }

  if (dump_file)
    df_dump (dump_file);

  if (fast)
    {
      bitmap_obstack_initialize (&dce_blocks_bitmap_obstack);
      bitmap_obstack_initialize (&dce_tmp_bitmap_obstack);
    }

  marked = sbitmap_alloc (get_max_uid () + 1);
  sbitmap_zero (marked);
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
  rtx insn;

  init_dce (false);

  prescan_insns_for_dce (false);
  mark_artificial_uses ();
  while (VEC_length (rtx, worklist) > 0)
    {
      insn = VEC_pop (rtx, worklist);
      mark_reg_dependencies (insn);
    }
  VEC_free (rtx, heap, worklist);

  /* Before any insns are deleted, we must remove the chains since
     they are not bidirectional.  */
  df_remove_problem (df_chain);
  delete_unmarked_insns ();

  fini_dce (false);
  return 0;
}


static bool
gate_ud_dce (void)
{
  return optimize > 1 && flag_dce
    && dbg_cnt (dce_ud);
}

struct rtl_opt_pass pass_ud_rtl_dce =
{
 {
  RTL_PASS,
  "ud dce",                             /* name */
  gate_ud_dce,                          /* gate */
  rest_of_handle_ud_dce,                /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_DCE,                               /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_dump_func |
  TODO_df_finish | TODO_verify_rtl_sharing |
  TODO_ggc_collect                     /* todo_flags_finish */
 }
};


/* -------------------------------------------------------------------------
   Fast DCE functions
   ------------------------------------------------------------------------- */

/* Process basic block BB.  Return true if the live_in set has
   changed. REDO_OUT is true if the info at the bottom of the block
   needs to be recalculated before starting.  AU is the proper set of
   artificial uses. */

static bool
byte_dce_process_block (basic_block bb, bool redo_out, bitmap au)
{
  bitmap local_live = BITMAP_ALLOC (&dce_tmp_bitmap_obstack);
  rtx insn;
  bool block_changed;
  df_ref *def_rec;

  if (redo_out)
    {
      /* Need to redo the live_out set of this block if when one of
	 the succs of this block has had a change in it live in
	 set.  */
      edge e;
      edge_iterator ei;
      df_confluence_function_n con_fun_n = df_byte_lr->problem->con_fun_n;
      bitmap_clear (DF_BYTE_LR_OUT (bb));
      FOR_EACH_EDGE (e, ei, bb->succs)
	(*con_fun_n) (e);
    }

  if (dump_file)
    {
      fprintf (dump_file, "processing block %d live out = ", bb->index);
      df_print_byte_regset (dump_file, DF_BYTE_LR_OUT (bb));
    }

  bitmap_copy (local_live, DF_BYTE_LR_OUT (bb));

  df_byte_lr_simulate_artificial_refs_at_end (bb, local_live);

  FOR_BB_INSNS_REVERSE (bb, insn)
    if (INSN_P (insn))
      {
	/* The insn is needed if there is someone who uses the output.  */
	for (def_rec = DF_INSN_DEFS (insn); *def_rec; def_rec++)
	  {
	    df_ref def = *def_rec;
	    unsigned int last;
	    unsigned int dregno = DF_REF_REGNO (def);
	    unsigned int start = df_byte_lr_get_regno_start (dregno);
	    unsigned int len = df_byte_lr_get_regno_len (dregno);

	    unsigned int sb;
	    unsigned int lb;
	    /* This is one of the only places where DF_MM_MAY should
	       be used for defs.  Need to make sure that we are
	       checking for all of the bits that may be used.  */

	    if (!df_compute_accessed_bytes (def, DF_MM_MAY, &sb, &lb))
	      {
		start += sb;
		len = lb - sb;
	      }

	    if (bitmap_bit_p (au, dregno))
	      {
		mark_insn (insn, true);
		goto quickexit;
	      }

	    last = start + len;
	    while (start < last)
	      if (bitmap_bit_p (local_live, start++))
		{
		  mark_insn (insn, true);
		  goto quickexit;
		}
	  }

      quickexit:

	/* No matter if the instruction is needed or not, we remove
	   any regno in the defs from the live set.  */
	df_byte_lr_simulate_defs (insn, local_live);

	/* On the other hand, we do not allow the dead uses to set
	   anything in local_live.  */
	if (marked_insn_p (insn))
	  df_byte_lr_simulate_uses (insn, local_live);

	if (dump_file)
	  {
	    fprintf (dump_file, "finished processing insn %d live out = ",
		     INSN_UID (insn));
	    df_print_byte_regset (dump_file, local_live);
	  }
      }

  df_byte_lr_simulate_artificial_refs_at_top (bb, local_live);

  block_changed = !bitmap_equal_p (local_live, DF_BYTE_LR_IN (bb));
  if (block_changed)
    bitmap_copy (DF_BYTE_LR_IN (bb), local_live);
  BITMAP_FREE (local_live);
  return block_changed;
}


/* Process basic block BB.  Return true if the live_in set has
   changed. REDO_OUT is true if the info at the bottom of the block
   needs to be recalculated before starting.  AU is the proper set of
   artificial uses. */

static bool
dce_process_block (basic_block bb, bool redo_out, bitmap au)
{
  bitmap local_live = BITMAP_ALLOC (&dce_tmp_bitmap_obstack);
  rtx insn;
  bool block_changed;
  df_ref *def_rec;

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

  FOR_BB_INSNS_REVERSE (bb, insn)
    if (INSN_P (insn))
      {
	bool needed = false;

	/* The insn is needed if there is someone who uses the output.  */
	for (def_rec = DF_INSN_DEFS (insn); *def_rec; def_rec++)
	  if (bitmap_bit_p (local_live, DF_REF_REGNO (*def_rec))
	      || bitmap_bit_p (au, DF_REF_REGNO (*def_rec)))
	    {
	      needed = true;
	      break;
	    }

	if (needed)
	  mark_insn (insn, true);

	/* No matter if the instruction is needed or not, we remove
	   any regno in the defs from the live set.  */
	df_simulate_defs (insn, local_live);

	/* On the other hand, we do not allow the dead uses to set
	   anything in local_live.  */
	if (marked_insn_p (insn))
	  df_simulate_uses (insn, local_live);
      }

  df_simulate_finalize_backwards (bb, local_live);

  block_changed = !bitmap_equal_p (local_live, DF_LR_IN (bb));
  if (block_changed)
    bitmap_copy (DF_LR_IN (bb), local_live);

  BITMAP_FREE (local_live);
  return block_changed;
}


/* Perform fast DCE once initialization is done.  If BYTE_LEVEL is
   true, use the byte level dce, otherwise do it at the pseudo
   level.  */

static void
fast_dce (bool byte_level)
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
  bitmap au = df->regular_block_artificial_uses;
  bitmap au_eh = df->eh_block_artificial_uses;
  int i;

  prescan_insns_for_dce (true);

  for (i = 0; i < n_blocks; i++)
    bitmap_set_bit (all_blocks, postorder[i]);

  while (global_changed)
    {
      global_changed = false;

      for (i = 0; i < n_blocks; i++)
	{
	  int index = postorder[i];
	  basic_block bb = BASIC_BLOCK (index);
	  bool local_changed;

	  if (index < NUM_FIXED_BLOCKS)
	    {
	      bitmap_set_bit (processed, index);
	      continue;
	    }

	  if (byte_level)
	    local_changed
	      = byte_dce_process_block (bb, bitmap_bit_p (redo_out, index),
					  bb_has_eh_pred (bb) ? au_eh : au);
	  else
	    local_changed
	      = dce_process_block (bb, bitmap_bit_p (redo_out, index),
				   bb_has_eh_pred (bb) ? au_eh : au);
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
	  sbitmap_zero (marked);
	  bitmap_clear (processed);
	  bitmap_clear (redo_out);

	  /* We do not need to rescan any instructions.  We only need
	     to redo the dataflow equations for the blocks that had a
	     change at the top of the block.  Then we need to redo the
	     iteration.  */
	  if (byte_level)
	    df_analyze_problem (df_byte_lr, all_blocks, postorder, n_blocks);
	  else
	    df_analyze_problem (df_lr, all_blocks, postorder, n_blocks);

	  if (old_flag & DF_LR_RUN_DCE)
	    df_set_flags (DF_LR_RUN_DCE);

	  prescan_insns_for_dce (true);
	}
    }

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

static unsigned int
rest_of_handle_fast_byte_dce (void)
{
  df_byte_lr_add_problem ();
  init_dce (true);
  fast_dce (true);
  fini_dce (true);
  return 0;
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


static bool
gate_fast_dce (void)
{
  return optimize > 0 && flag_dce
    && dbg_cnt (dce_fast);
}

struct rtl_opt_pass pass_fast_rtl_dce =
{
 {
  RTL_PASS,
  "rtl dce",                            /* name */
  gate_fast_dce,                        /* gate */
  rest_of_handle_fast_dce,              /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_DCE,                               /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_dump_func |
  TODO_df_finish | TODO_verify_rtl_sharing |
  TODO_ggc_collect                      /* todo_flags_finish */
 }
};

struct rtl_opt_pass pass_fast_rtl_byte_dce =
{
 {
  RTL_PASS,
  "byte-dce",                           /* name */
  gate_fast_dce,                        /* gate */
  rest_of_handle_fast_byte_dce,         /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_DCE,                               /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_dump_func |
  TODO_df_finish | TODO_verify_rtl_sharing |
  TODO_ggc_collect                      /* todo_flags_finish */
 }
};
