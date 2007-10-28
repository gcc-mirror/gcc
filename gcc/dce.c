/* RTL dead code elimination.
   Copyright (C) 2005, 2006, 2007 Free Software Foundation, Inc.

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
#include "df.h"
#include "cselib.h"
#include "dce.h"
#include "timevar.h"
#include "tree-pass.h"
#include "dbgcnt.h"

DEF_VEC_I(int);
DEF_VEC_ALLOC_I(int,heap);


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
      if (volatile_refs_p (body))
	return false;

      if (flag_non_call_exceptions && may_trap_p (body))
	return false;

      return true;
    }
}


/* Return true if INSN is a normal instruction that can be deleted by
   the DCE pass.  */

static bool
deletable_insn_p (rtx insn, bool fast)
{
  rtx body, x;
  int i;

  if (!NONJUMP_INSN_P (insn))
    return false;

  body = PATTERN (insn);
  switch (GET_CODE (body))
    {
    case USE:
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
  if (insn)
    return TEST_BIT (marked, INSN_UID (insn));
  else 
    /* Artificial defs are always needed and they do not have an
       insn.  */
    return true;
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


/* Return true if the entire libcall sequence starting at INSN is dead.
   NOTE is the REG_LIBCALL note attached to INSN.

   A libcall sequence is a block of insns with no side-effects, i.e.
   that is only used for its return value.  The terminology derives
   from that of a call, but a libcall sequence need not contain one.
   It is only defined by a pair of REG_LIBCALL/REG_RETVAL notes.

   From a dataflow viewpoint, a libcall sequence has the property that
   no UD chain can enter it from the outside.  As a consequence, if a
   libcall sequence has a dead return value, it is effectively dead.
   This is both enforced by CSE (cse_extended_basic_block) and relied
   upon by delete_trivially_dead_insns.

   However, in practice, the return value business is a tricky one and
   only checking the liveness of the last insn is not sufficient to
   decide whether the whole sequence is dead (e.g. PR middle-end/19551)
   so we check the liveness of every insn starting from the call.  */

static bool
libcall_dead_p (rtx insn, rtx note)
{
  rtx last = XEXP (note, 0);

  /* Find the call insn.  */
  while (insn != last && !CALL_P (insn))
    insn = NEXT_INSN (insn);

  /* If there is none, do nothing special, since ordinary death handling
     can understand these insns.  */
  if (!CALL_P (insn))
    return false;

  /* If this is a call that returns a value via an invisible pointer, the
     dataflow engine cannot see it so it has been marked unconditionally.
     Skip it unless it has been made the last insn in the libcall, for
     example by the combiner, in which case we're left with no easy way
     of asserting its liveness.  */
  if (!single_set (insn))
    {
      if (insn == last)
	return false;
      insn = NEXT_INSN (insn);
    }

  while (insn != NEXT_INSN (last))
    {
      if (INSN_P (insn) && marked_insn_p (insn))
	return false;
      insn = NEXT_INSN (insn);
    }

  return true;
}


/* Delete all REG_EQUAL notes of the registers INSN writes, to prevent
   bad dangling REG_EQUAL notes. */

static void
delete_corresponding_reg_eq_notes (rtx insn)
{
  struct df_ref **def_rec;
  for (def_rec = DF_INSN_DEFS (insn); *def_rec; def_rec++)
    {
      struct df_ref *def = *def_rec;
      unsigned int regno = DF_REF_REGNO (def);
      /* This loop is a little tricky.  We cannot just go down the
	 chain because it is being modified by the actions in the
	 loop.  So we just get the head.  We plan to drain the list
	 anyway.  */
      while (DF_REG_EQ_USE_CHAIN (regno))
	{
	  struct df_ref *eq_use = DF_REG_EQ_USE_CHAIN (regno);
	  rtx noted_insn = DF_REF_INSN (eq_use);
	  rtx note = find_reg_note (noted_insn, REG_EQUAL, NULL_RTX);
	  if (!note)
	    note = find_reg_note (noted_insn, REG_EQUIV, NULL_RTX);

	  /* This assert is generally triggered when someone deletes a
	     REG_EQUAL or REG_EQUIV note by hacking the list manually
	     rather than calling remove_note.  */
	  gcc_assert (note);
	  remove_note (noted_insn, note);
	}
    }
}


/* Delete every instruction that hasn't been marked.  */

static void
delete_unmarked_insns (void)
{
  basic_block bb;
  rtx insn, next;

  FOR_EACH_BB (bb)
    FOR_BB_INSNS_SAFE (bb, insn, next)
      if (INSN_P (insn))
	{
	  rtx note = find_reg_note (insn, REG_LIBCALL, NULL_RTX);

	  /* Always delete no-op moves.  */
	  if (noop_move_p (insn))
	    ;

	  /* Try to delete libcall sequences as a whole.  */
	  else if (note && libcall_dead_p (insn, note))
	    {
	      rtx last = XEXP (note, 0);

	      if (!dbg_cnt (dce))
		continue;

	      if (dump_file)
	        fprintf (dump_file, "DCE: Deleting libcall %d-%d\n",
			 INSN_UID (insn), INSN_UID (last));

	      next = NEXT_INSN (last);
	      delete_insn_chain_and_edges (insn, last);
	      continue;
	    }

	  /* Otherwise rely only on the DCE algorithm.  */
	  else if (marked_insn_p (insn))
	    continue;

	  if (!dbg_cnt (dce))
	    continue;

	  if (dump_file)
	    fprintf (dump_file, "DCE: Deleting insn %d\n", INSN_UID (insn));

	  /* Before we delete the insn we have to delete REG_EQUAL notes
	     for the destination regs in order to avoid dangling notes.  */
	  delete_corresponding_reg_eq_notes (insn);

	  /* If we're about to delete the first insn of a libcall, then
	     move the REG_LIBCALL note to the next real insn and update
	     the REG_RETVAL note.  */
	  if (note && (XEXP (note, 0) != insn))
	    {
	      rtx new_libcall_insn = next_real_insn (insn);
	      rtx retval_note = find_reg_note (XEXP (note, 0),
					       REG_RETVAL, NULL_RTX);
	      /* If the RETVAL and LIBCALL notes would land on the same
		 insn just remove them.  */
	      if (XEXP (note, 0) == new_libcall_insn)
		remove_note (new_libcall_insn, retval_note);
	      else
		{
		  REG_NOTES (new_libcall_insn)
		    = gen_rtx_INSN_LIST (REG_LIBCALL, XEXP (note, 0),
					 REG_NOTES (new_libcall_insn));
		  XEXP (retval_note, 0) = new_libcall_insn;
		}
	    }

	  /* If the insn contains a REG_RETVAL note and is dead, but the
	     libcall as a whole is not dead, then we want to remove the
	     insn, but not the whole libcall sequence.  However, we also
	     need to remove the dangling REG_LIBCALL note in order to
	     avoid mismatched notes.  We could find a new location for
	     the REG_RETVAL note, but it hardly seems worth the effort.  */
	  note = find_reg_note (insn, REG_RETVAL, NULL_RTX);
	  if (note && (XEXP (note, 0) != insn))
	    {
	      rtx libcall_note
		= find_reg_note (XEXP (note, 0), REG_LIBCALL, NULL_RTX);
	      remove_note (XEXP (note, 0), libcall_note);
	    }

	  /* Now delete the insn.  */
	  delete_insn_and_edges (insn);
	}
}


/* Helper function for prescan_insns_for_dce: prescan the entire libcall
   sequence starting at INSN and return the insn following the libcall.
   NOTE is the REG_LIBCALL note attached to INSN.  */

static rtx
prescan_libcall_for_dce (rtx insn, rtx note, bool fast)
{
  rtx last = XEXP (note, 0);

  /* A libcall is never necessary on its own but we need to mark the stores
     to a non-register destination.  */
  while (insn != last && !CALL_P (insn))
    {
      if (INSN_P (insn))
	mark_nonreg_stores (PATTERN (insn), insn, fast);
      insn = NEXT_INSN (insn);
    }

  /* If this is a call that returns a value via an invisible pointer, the
     dataflow engine cannot see it so it has to be marked unconditionally.  */
  if (CALL_P (insn) && !single_set (insn))
    {
      mark_insn (insn, fast);
      insn = NEXT_INSN (insn);
    }

  while (insn != NEXT_INSN (last))
    {
      if (INSN_P (insn))
	mark_nonreg_stores (PATTERN (insn), insn, fast);
      insn = NEXT_INSN (insn);
    }

  return insn;
}


/* Go through the instructions and mark those whose necessity is not
   dependent on inter-instruction information.  Make sure all other
   instructions are not marked.  */

static void
prescan_insns_for_dce (bool fast)
{
  basic_block bb;
  rtx insn, next;
  
  if (dump_file)
    fprintf (dump_file, "Finding needed instructions:\n");
  
  FOR_EACH_BB (bb)
    FOR_BB_INSNS_SAFE (bb, insn, next)
      if (INSN_P (insn))
	{
	  rtx note = find_reg_note (insn, REG_LIBCALL, NULL_RTX);
	  if (note)
	    next = prescan_libcall_for_dce (insn, note, fast);
	  else if (deletable_insn_p (insn, fast))
	    mark_nonreg_stores (PATTERN (insn), insn, fast);
	  else
	    mark_insn (insn, fast);
	}

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
  struct df_ref **use_rec;

  FOR_ALL_BB (bb)
    {
      for (use_rec = df_get_artificial_uses (bb->index); 
	   *use_rec; use_rec++)
	for (defs = DF_REF_CHAIN (*use_rec); defs; defs = defs->next)
	  mark_insn (DF_REF_INSN (defs->ref), false);
    }
}


/* Mark every instruction that defines a register value that INSN uses.  */

static void
mark_reg_dependencies (rtx insn)
{
  struct df_link *defs;
  struct df_ref **use_rec;

  for (use_rec = DF_INSN_USES (insn); *use_rec; use_rec++)
    {
      struct df_ref *use = *use_rec;
      if (dump_file)
	{
	  fprintf (dump_file, "Processing use of ");
	  print_simple_rtl (dump_file, DF_REF_REG (use));
	  fprintf (dump_file, " in insn %d:\n", INSN_UID (insn));
	}
      for (defs = DF_REF_CHAIN (use); defs; defs = defs->next)
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
  return optimize > 1 && flag_dce;
}

struct tree_opt_pass pass_ud_rtl_dce =
{
  "dce",                                /* name */
  gate_ud_dce,                        /* gate */
  rest_of_handle_ud_dce,              /* execute */
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
  TODO_ggc_collect,                     /* todo_flags_finish */
  'w'                                   /* letter */
};


/* -------------------------------------------------------------------------
   Fast DCE functions
   ------------------------------------------------------------------------- */

/* Process basic block BB.  Return true if the live_in set has changed.  */

static bool
dce_process_block (basic_block bb, bool redo_out)
{
  bitmap local_live = BITMAP_ALLOC (&dce_tmp_bitmap_obstack);
  bitmap au;
  rtx insn;
  bool block_changed;
  struct df_ref **def_rec, **use_rec;
  unsigned int bb_index = bb->index;

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
      fprintf (dump_file, "processing block %d live out = ", bb->index);
      df_print_regset (dump_file, DF_LR_OUT (bb));
    }

  bitmap_copy (local_live, DF_LR_OUT (bb));

  /* Process the artificial defs and uses at the bottom of the block.  */
  for (def_rec = df_get_artificial_defs (bb_index); *def_rec; def_rec++)
    {
      struct df_ref *def = *def_rec;
      if (((DF_REF_FLAGS (def) & DF_REF_AT_TOP) == 0)
	  && (!(DF_REF_FLAGS (def) & (DF_REF_PARTIAL | DF_REF_CONDITIONAL))))
	bitmap_clear_bit (local_live, DF_REF_REGNO (def));
    }

  for (use_rec = df_get_artificial_uses (bb_index); *use_rec; use_rec++)
    {
      struct df_ref *use = *use_rec;
      if ((DF_REF_FLAGS (use) & DF_REF_AT_TOP) == 0)
	bitmap_set_bit (local_live, DF_REF_REGNO (use));
    }

  /* These regs are considered always live so if they end up dying
     because of some def, we need to bring the back again.
     Calling df_simulate_fixup_sets has the disadvantage of calling
     bb_has_eh_pred once per insn, so we cache the information here.  */
  if (bb_has_eh_pred (bb))
    au = df->eh_block_artificial_uses;
  else
    au = df->regular_block_artificial_uses;

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
  
  for (def_rec = df_get_artificial_defs (bb_index); *def_rec; def_rec++)
    {
      struct df_ref *def = *def_rec;
      if ((DF_REF_FLAGS (def) & DF_REF_AT_TOP)
	  && (!(DF_REF_FLAGS (def) & (DF_REF_PARTIAL | DF_REF_CONDITIONAL))))
	bitmap_clear_bit (local_live, DF_REF_REGNO (def));
    }

#ifdef EH_USES
  /* Process the uses that are live into an exception handler.  */
  for (use_rec = df_get_artificial_uses (bb_index); *use_rec; use_rec++)
    {
      /* Add use to set of uses in this BB.  */
      struct df_ref *use = *use_rec;
      if (DF_REF_FLAGS (use) & DF_REF_AT_TOP)
	bitmap_set_bit (local_live, DF_REF_REGNO (use));
    }
#endif

  block_changed = !bitmap_equal_p (local_live, DF_LR_IN (bb));
  if (block_changed)
    bitmap_copy (DF_LR_IN (bb), local_live);

  BITMAP_FREE (local_live);
  return block_changed;
}


/* Perform fast DCE once initialization is done.  */

static void
fast_dce (void)
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

	  local_changed 
	    = dce_process_block (bb, bitmap_bit_p (redo_out, index));
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


/* Fast DCE.  */

static unsigned int
rest_of_handle_fast_dce (void)
{
  init_dce (true);
  fast_dce ();
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
      enum df_changeable_flags old_flags 
	= df_clear_flags (DF_DEFER_INSN_RESCAN + DF_NO_INSN_RESCAN);
      
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
  return optimize > 0 && flag_dce;
}

struct tree_opt_pass pass_fast_rtl_dce =
{
  "dce",                                /* name */
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
  TODO_ggc_collect,                     /* todo_flags_finish */
  'w'                                   /* letter */
};
