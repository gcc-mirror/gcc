/* Control flow graph manipulation code for GNU compiler.
   Copyright (C) 1987-2018 Free Software Foundation, Inc.

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

/* This file contains low level functions to manipulate the CFG and analyze it
   that are aware of the RTL intermediate language.

   Available functionality:
     - Basic CFG/RTL manipulation API documented in cfghooks.h
     - CFG-aware instruction chain manipulation
	 delete_insn, delete_insn_chain
     - Edge splitting and committing to edges
	 insert_insn_on_edge, commit_edge_insertions
     - CFG updating after insn simplification
	 purge_dead_edges, purge_all_dead_edges
     - CFG fixing after coarse manipulation
	fixup_abnormal_edges

   Functions not supposed for generic use:
     - Infrastructure to determine quickly basic block for insn
	 compute_bb_for_insn, update_bb_for_insn, set_block_for_insn,
     - Edge redirection with updating and optimizing of insn chain
	 block_label, tidy_fallthru_edge, force_nonfallthru  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "cfghooks.h"
#include "df.h"
#include "insn-config.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "cfgrtl.h"
#include "cfganal.h"
#include "cfgbuild.h"
#include "cfgcleanup.h"
#include "bb-reorder.h"
#include "rtl-error.h"
#include "insn-attr.h"
#include "dojump.h"
#include "expr.h"
#include "cfgloop.h"
#include "tree-pass.h"
#include "print-rtl.h"

/* Holds the interesting leading and trailing notes for the function.
   Only applicable if the CFG is in cfglayout mode.  */
static GTY(()) rtx_insn *cfg_layout_function_footer;
static GTY(()) rtx_insn *cfg_layout_function_header;

static rtx_insn *skip_insns_after_block (basic_block);
static void record_effective_endpoints (void);
static void fixup_reorder_chain (void);

void verify_insn_chain (void);
static void fixup_fallthru_exit_predecessor (void);
static int can_delete_note_p (const rtx_note *);
static int can_delete_label_p (const rtx_code_label *);
static basic_block rtl_split_edge (edge);
static bool rtl_move_block_after (basic_block, basic_block);
static int rtl_verify_flow_info (void);
static basic_block cfg_layout_split_block (basic_block, void *);
static edge cfg_layout_redirect_edge_and_branch (edge, basic_block);
static basic_block cfg_layout_redirect_edge_and_branch_force (edge, basic_block);
static void cfg_layout_delete_block (basic_block);
static void rtl_delete_block (basic_block);
static basic_block rtl_redirect_edge_and_branch_force (edge, basic_block);
static edge rtl_redirect_edge_and_branch (edge, basic_block);
static basic_block rtl_split_block (basic_block, void *);
static void rtl_dump_bb (FILE *, basic_block, int, dump_flags_t);
static int rtl_verify_flow_info_1 (void);
static void rtl_make_forwarder_block (edge);

/* Return true if NOTE is not one of the ones that must be kept paired,
   so that we may simply delete it.  */

static int
can_delete_note_p (const rtx_note *note)
{
  switch (NOTE_KIND (note))
    {
    case NOTE_INSN_DELETED:
    case NOTE_INSN_BASIC_BLOCK:
    case NOTE_INSN_EPILOGUE_BEG:
      return true;

    default:
      return false;
    }
}

/* True if a given label can be deleted.  */

static int
can_delete_label_p (const rtx_code_label *label)
{
  return (!LABEL_PRESERVE_P (label)
	  /* User declared labels must be preserved.  */
	  && LABEL_NAME (label) == 0
	  && !vec_safe_contains<rtx_insn *> (forced_labels,
					     const_cast<rtx_code_label *> (label)));
}

/* Delete INSN by patching it out.  */

void
delete_insn (rtx_insn *insn)
{
  rtx note;
  bool really_delete = true;

  if (LABEL_P (insn))
    {
      /* Some labels can't be directly removed from the INSN chain, as they
	 might be references via variables, constant pool etc.
	 Convert them to the special NOTE_INSN_DELETED_LABEL note.  */
      if (! can_delete_label_p (as_a <rtx_code_label *> (insn)))
	{
	  const char *name = LABEL_NAME (insn);
	  basic_block bb = BLOCK_FOR_INSN (insn);
	  rtx_insn *bb_note = NEXT_INSN (insn);

	  really_delete = false;
	  PUT_CODE (insn, NOTE);
	  NOTE_KIND (insn) = NOTE_INSN_DELETED_LABEL;
	  NOTE_DELETED_LABEL_NAME (insn) = name;

	  /* If the note following the label starts a basic block, and the
	     label is a member of the same basic block, interchange the two.  */
	  if (bb_note != NULL_RTX
	      && NOTE_INSN_BASIC_BLOCK_P (bb_note)
	      && bb != NULL
	      && bb == BLOCK_FOR_INSN (bb_note))
	    {
	      reorder_insns_nobb (insn, insn, bb_note);
	      BB_HEAD (bb) = bb_note;
	      if (BB_END (bb) == bb_note)
		BB_END (bb) = insn;
	    }
	}

      remove_node_from_insn_list (insn, &nonlocal_goto_handler_labels);
    }

  if (really_delete)
    {
      /* If this insn has already been deleted, something is very wrong.  */
      gcc_assert (!insn->deleted ());
      if (INSN_P (insn))
	df_insn_delete (insn);
      remove_insn (insn);
      insn->set_deleted ();
    }

  /* If deleting a jump, decrement the use count of the label.  Deleting
     the label itself should happen in the normal course of block merging.  */
  if (JUMP_P (insn))
    {
      if (JUMP_LABEL (insn)
	  && LABEL_P (JUMP_LABEL (insn)))
	LABEL_NUSES (JUMP_LABEL (insn))--;

      /* If there are more targets, remove them too.  */
      while ((note
	      = find_reg_note (insn, REG_LABEL_TARGET, NULL_RTX)) != NULL_RTX
	     && LABEL_P (XEXP (note, 0)))
	{
	  LABEL_NUSES (XEXP (note, 0))--;
	  remove_note (insn, note);
	}
    }

  /* Also if deleting any insn that references a label as an operand.  */
  while ((note = find_reg_note (insn, REG_LABEL_OPERAND, NULL_RTX)) != NULL_RTX
	 && LABEL_P (XEXP (note, 0)))
    {
      LABEL_NUSES (XEXP (note, 0))--;
      remove_note (insn, note);
    }

  if (rtx_jump_table_data *table = dyn_cast <rtx_jump_table_data *> (insn))
    {
      rtvec vec = table->get_labels ();
      int len = GET_NUM_ELEM (vec);
      int i;

      for (i = 0; i < len; i++)
	{
	  rtx label = XEXP (RTVEC_ELT (vec, i), 0);

	  /* When deleting code in bulk (e.g. removing many unreachable
	     blocks) we can delete a label that's a target of the vector
	     before deleting the vector itself.  */
	  if (!NOTE_P (label))
	    LABEL_NUSES (label)--;
	}
    }
}

/* Like delete_insn but also purge dead edges from BB.
   Return true if any edges are eliminated.  */

bool
delete_insn_and_edges (rtx_insn *insn)
{
  bool purge = false;

  if (INSN_P (insn)
      && BLOCK_FOR_INSN (insn)
      && BB_END (BLOCK_FOR_INSN (insn)) == insn)
    purge = true;
  delete_insn (insn);
  if (purge)
    return purge_dead_edges (BLOCK_FOR_INSN (insn));
  return false;
}

/* Unlink a chain of insns between START and FINISH, leaving notes
   that must be paired.  If CLEAR_BB is true, we set bb field for
   insns that cannot be removed to NULL.  */

void
delete_insn_chain (rtx start, rtx_insn *finish, bool clear_bb)
{
  /* Unchain the insns one by one.  It would be quicker to delete all of these
     with a single unchaining, rather than one at a time, but we need to keep
     the NOTE's.  */
  rtx_insn *current = finish;
  while (1)
    {
      rtx_insn *prev = PREV_INSN (current);
      if (NOTE_P (current) && !can_delete_note_p (as_a <rtx_note *> (current)))
	;
      else
	delete_insn (current);

      if (clear_bb && !current->deleted ())
	set_block_for_insn (current, NULL);

      if (current == start)
	break;
      current = prev;
    }
}

/* Create a new basic block consisting of the instructions between HEAD and END
   inclusive.  This function is designed to allow fast BB construction - reuses
   the note and basic block struct in BB_NOTE, if any and do not grow
   BASIC_BLOCK chain and should be used directly only by CFG construction code.
   END can be NULL in to create new empty basic block before HEAD.  Both END
   and HEAD can be NULL to create basic block at the end of INSN chain.
   AFTER is the basic block we should be put after.  */

basic_block
create_basic_block_structure (rtx_insn *head, rtx_insn *end, rtx_note *bb_note,
			      basic_block after)
{
  basic_block bb;

  if (bb_note
      && (bb = NOTE_BASIC_BLOCK (bb_note)) != NULL
      && bb->aux == NULL)
    {
      /* If we found an existing note, thread it back onto the chain.  */

      rtx_insn *after;

      if (LABEL_P (head))
	after = head;
      else
	{
	  after = PREV_INSN (head);
	  head = bb_note;
	}

      if (after != bb_note && NEXT_INSN (after) != bb_note)
	reorder_insns_nobb (bb_note, bb_note, after);
    }
  else
    {
      /* Otherwise we must create a note and a basic block structure.  */

      bb = alloc_block ();

      init_rtl_bb_info (bb);
      if (!head && !end)
	head = end = bb_note
	  = emit_note_after (NOTE_INSN_BASIC_BLOCK, get_last_insn ());
      else if (LABEL_P (head) && end)
	{
	  bb_note = emit_note_after (NOTE_INSN_BASIC_BLOCK, head);
	  if (head == end)
	    end = bb_note;
	}
      else
	{
	  bb_note = emit_note_before (NOTE_INSN_BASIC_BLOCK, head);
	  head = bb_note;
	  if (!end)
	    end = head;
	}

      NOTE_BASIC_BLOCK (bb_note) = bb;
    }

  /* Always include the bb note in the block.  */
  if (NEXT_INSN (end) == bb_note)
    end = bb_note;

  BB_HEAD (bb) = head;
  BB_END (bb) = end;
  bb->index = last_basic_block_for_fn (cfun)++;
  bb->flags = BB_NEW | BB_RTL;
  link_block (bb, after);
  SET_BASIC_BLOCK_FOR_FN (cfun, bb->index, bb);
  df_bb_refs_record (bb->index, false);
  update_bb_for_insn (bb);
  BB_SET_PARTITION (bb, BB_UNPARTITIONED);

  /* Tag the block so that we know it has been used when considering
     other basic block notes.  */
  bb->aux = bb;

  return bb;
}

/* Create new basic block consisting of instructions in between HEAD and END
   and place it to the BB chain after block AFTER.  END can be NULL to
   create a new empty basic block before HEAD.  Both END and HEAD can be
   NULL to create basic block at the end of INSN chain.  */

static basic_block
rtl_create_basic_block (void *headp, void *endp, basic_block after)
{
  rtx_insn *head = (rtx_insn *) headp;
  rtx_insn *end = (rtx_insn *) endp;
  basic_block bb;

  /* Grow the basic block array if needed.  */
  if ((size_t) last_basic_block_for_fn (cfun)
      >= basic_block_info_for_fn (cfun)->length ())
    {
      size_t new_size =
	(last_basic_block_for_fn (cfun)
	 + (last_basic_block_for_fn (cfun) + 3) / 4);
      vec_safe_grow_cleared (basic_block_info_for_fn (cfun), new_size);
    }

  n_basic_blocks_for_fn (cfun)++;

  bb = create_basic_block_structure (head, end, NULL, after);
  bb->aux = NULL;
  return bb;
}

static basic_block
cfg_layout_create_basic_block (void *head, void *end, basic_block after)
{
  basic_block newbb = rtl_create_basic_block (head, end, after);

  return newbb;
}

/* Delete the insns in a (non-live) block.  We physically delete every
   non-deleted-note insn, and update the flow graph appropriately.

   Return nonzero if we deleted an exception handler.  */

/* ??? Preserving all such notes strikes me as wrong.  It would be nice
   to post-process the stream to remove empty blocks, loops, ranges, etc.  */

static void
rtl_delete_block (basic_block b)
{
  rtx_insn *insn, *end;

  /* If the head of this block is a CODE_LABEL, then it might be the
     label for an exception handler which can't be reached.  We need
     to remove the label from the exception_handler_label list.  */
  insn = BB_HEAD (b);

  end = get_last_bb_insn (b);

  /* Selectively delete the entire chain.  */
  BB_HEAD (b) = NULL;
  delete_insn_chain (insn, end, true);


  if (dump_file)
    fprintf (dump_file, "deleting block %d\n", b->index);
  df_bb_delete (b->index);
}

/* Records the basic block struct in BLOCK_FOR_INSN for every insn.  */

void
compute_bb_for_insn (void)
{
  basic_block bb;

  FOR_EACH_BB_FN (bb, cfun)
    {
      rtx_insn *end = BB_END (bb);
      rtx_insn *insn;

      for (insn = BB_HEAD (bb); ; insn = NEXT_INSN (insn))
	{
	  BLOCK_FOR_INSN (insn) = bb;
	  if (insn == end)
	    break;
	}
    }
}

/* Release the basic_block_for_insn array.  */

unsigned int
free_bb_for_insn (void)
{
  rtx_insn *insn;
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    if (!BARRIER_P (insn))
      BLOCK_FOR_INSN (insn) = NULL;
  return 0;
}

namespace {

const pass_data pass_data_free_cfg =
{
  RTL_PASS, /* type */
  "*free_cfg", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  PROP_cfg, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_free_cfg : public rtl_opt_pass
{
public:
  pass_free_cfg (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_free_cfg, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *);

}; // class pass_free_cfg

unsigned int
pass_free_cfg::execute (function *)
{
  /* The resource.c machinery uses DF but the CFG isn't guaranteed to be
     valid at that point so it would be too late to call df_analyze.  */
  if (DELAY_SLOTS && optimize > 0 && flag_delayed_branch)
    {
      df_note_add_problem ();
      df_analyze ();
    }

  if (crtl->has_bb_partition)
    insert_section_boundary_note ();

  free_bb_for_insn ();
  return 0;
}

} // anon namespace

rtl_opt_pass *
make_pass_free_cfg (gcc::context *ctxt)
{
  return new pass_free_cfg (ctxt);
}

/* Return RTX to emit after when we want to emit code on the entry of function.  */
rtx_insn *
entry_of_function (void)
{
  return (n_basic_blocks_for_fn (cfun) > NUM_FIXED_BLOCKS ?
	  BB_HEAD (ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb) : get_insns ());
}

/* Emit INSN at the entry point of the function, ensuring that it is only
   executed once per function.  */
void
emit_insn_at_entry (rtx insn)
{
  edge_iterator ei = ei_start (ENTRY_BLOCK_PTR_FOR_FN (cfun)->succs);
  edge e = ei_safe_edge (ei);
  gcc_assert (e->flags & EDGE_FALLTHRU);

  insert_insn_on_edge (insn, e);
  commit_edge_insertions ();
}

/* Update BLOCK_FOR_INSN of insns between BEGIN and END
   (or BARRIER if found) and notify df of the bb change.
   The insn chain range is inclusive
   (i.e. both BEGIN and END will be updated. */

static void
update_bb_for_insn_chain (rtx_insn *begin, rtx_insn *end, basic_block bb)
{
  rtx_insn *insn;

  end = NEXT_INSN (end);
  for (insn = begin; insn != end; insn = NEXT_INSN (insn))
    if (!BARRIER_P (insn))
      df_insn_change_bb (insn, bb);
}

/* Update BLOCK_FOR_INSN of insns in BB to BB,
   and notify df of the change.  */

void
update_bb_for_insn (basic_block bb)
{
  update_bb_for_insn_chain (BB_HEAD (bb), BB_END (bb), bb);
}


/* Like active_insn_p, except keep the return value clobber around
   even after reload.  */

static bool
flow_active_insn_p (const rtx_insn *insn)
{
  if (active_insn_p (insn))
    return true;

  /* A clobber of the function return value exists for buggy
     programs that fail to return a value.  Its effect is to
     keep the return value from being live across the entire
     function.  If we allow it to be skipped, we introduce the
     possibility for register lifetime confusion.  */
  if (GET_CODE (PATTERN (insn)) == CLOBBER
      && REG_P (XEXP (PATTERN (insn), 0))
      && REG_FUNCTION_VALUE_P (XEXP (PATTERN (insn), 0)))
    return true;

  return false;
}

/* Return true if the block has no effect and only forwards control flow to
   its single destination.  */

bool
contains_no_active_insn_p (const_basic_block bb)
{
  rtx_insn *insn;

  if (bb == EXIT_BLOCK_PTR_FOR_FN (cfun)
      || bb == ENTRY_BLOCK_PTR_FOR_FN (cfun)
      || !single_succ_p (bb)
      || (single_succ_edge (bb)->flags & EDGE_FAKE) != 0)
    return false;

  for (insn = BB_HEAD (bb); insn != BB_END (bb); insn = NEXT_INSN (insn))
    if (INSN_P (insn) && flow_active_insn_p (insn))
      return false;

  return (!INSN_P (insn)
	  || (JUMP_P (insn) && simplejump_p (insn))
	  || !flow_active_insn_p (insn));
}

/* Likewise, but protect loop latches, headers and preheaders.  */
/* FIXME: Make this a cfg hook.  */

bool
forwarder_block_p (const_basic_block bb)
{
  if (!contains_no_active_insn_p (bb))
    return false;

  /* Protect loop latches, headers and preheaders.  */
  if (current_loops)
    {
      basic_block dest;
      if (bb->loop_father->header == bb)
	return false;
      dest = EDGE_SUCC (bb, 0)->dest;
      if (dest->loop_father->header == dest)
	return false;
    }

  return true;
}

/* Return nonzero if we can reach target from src by falling through.  */
/* FIXME: Make this a cfg hook, the result is only valid in cfgrtl mode.  */

bool
can_fallthru (basic_block src, basic_block target)
{
  rtx_insn *insn = BB_END (src);
  rtx_insn *insn2;
  edge e;
  edge_iterator ei;

  if (target == EXIT_BLOCK_PTR_FOR_FN (cfun))
    return true;
  if (src->next_bb != target)
    return false;

  /* ??? Later we may add code to move jump tables offline.  */
  if (tablejump_p (insn, NULL, NULL))
    return false;

  FOR_EACH_EDGE (e, ei, src->succs)
    if (e->dest == EXIT_BLOCK_PTR_FOR_FN (cfun)
	&& e->flags & EDGE_FALLTHRU)
      return false;

  insn2 = BB_HEAD (target);
  if (!active_insn_p (insn2))
    insn2 = next_active_insn (insn2);

  return next_active_insn (insn) == insn2;
}

/* Return nonzero if we could reach target from src by falling through,
   if the target was made adjacent.  If we already have a fall-through
   edge to the exit block, we can't do that.  */
static bool
could_fall_through (basic_block src, basic_block target)
{
  edge e;
  edge_iterator ei;

  if (target == EXIT_BLOCK_PTR_FOR_FN (cfun))
    return true;
  FOR_EACH_EDGE (e, ei, src->succs)
    if (e->dest == EXIT_BLOCK_PTR_FOR_FN (cfun)
	&& e->flags & EDGE_FALLTHRU)
      return 0;
  return true;
}

/* Return the NOTE_INSN_BASIC_BLOCK of BB.  */
rtx_note *
bb_note (basic_block bb)
{
  rtx_insn *note;

  note = BB_HEAD (bb);
  if (LABEL_P (note))
    note = NEXT_INSN (note);

  gcc_assert (NOTE_INSN_BASIC_BLOCK_P (note));
  return as_a <rtx_note *> (note);
}

/* Return the INSN immediately following the NOTE_INSN_BASIC_BLOCK
   note associated with the BLOCK.  */

static rtx_insn *
first_insn_after_basic_block_note (basic_block block)
{
  rtx_insn *insn;

  /* Get the first instruction in the block.  */
  insn = BB_HEAD (block);

  if (insn == NULL_RTX)
    return NULL;
  if (LABEL_P (insn))
    insn = NEXT_INSN (insn);
  gcc_assert (NOTE_INSN_BASIC_BLOCK_P (insn));

  return NEXT_INSN (insn);
}

/* Creates a new basic block just after basic block BB by splitting
   everything after specified instruction INSNP.  */

static basic_block
rtl_split_block (basic_block bb, void *insnp)
{
  basic_block new_bb;
  rtx_insn *insn = (rtx_insn *) insnp;
  edge e;
  edge_iterator ei;

  if (!insn)
    {
      insn = first_insn_after_basic_block_note (bb);

      if (insn)
	{
	  rtx_insn *next = insn;

	  insn = PREV_INSN (insn);

	  /* If the block contains only debug insns, insn would have
	     been NULL in a non-debug compilation, and then we'd end
	     up emitting a DELETED note.  For -fcompare-debug
	     stability, emit the note too.  */
	  if (insn != BB_END (bb)
	      && DEBUG_INSN_P (next)
	      && DEBUG_INSN_P (BB_END (bb)))
	    {
	      while (next != BB_END (bb) && DEBUG_INSN_P (next))
		next = NEXT_INSN (next);

	      if (next == BB_END (bb))
		emit_note_after (NOTE_INSN_DELETED, next);
	    }
	}
      else
	insn = get_last_insn ();
    }

  /* We probably should check type of the insn so that we do not create
     inconsistent cfg.  It is checked in verify_flow_info anyway, so do not
     bother.  */
  if (insn == BB_END (bb))
    emit_note_after (NOTE_INSN_DELETED, insn);

  /* Create the new basic block.  */
  new_bb = create_basic_block (NEXT_INSN (insn), BB_END (bb), bb);
  BB_COPY_PARTITION (new_bb, bb);
  BB_END (bb) = insn;

  /* Redirect the outgoing edges.  */
  new_bb->succs = bb->succs;
  bb->succs = NULL;
  FOR_EACH_EDGE (e, ei, new_bb->succs)
    e->src = new_bb;

  /* The new block starts off being dirty.  */
  df_set_bb_dirty (bb);
  return new_bb;
}

/* Return true if the single edge between blocks A and B is the only place
   in RTL which holds some unique locus.  */

static bool
unique_locus_on_edge_between_p (basic_block a, basic_block b)
{
  const location_t goto_locus = EDGE_SUCC (a, 0)->goto_locus;
  rtx_insn *insn, *end;

  if (LOCATION_LOCUS (goto_locus) == UNKNOWN_LOCATION)
    return false;

  /* First scan block A backward.  */
  insn = BB_END (a);
  end = PREV_INSN (BB_HEAD (a));
  while (insn != end && (!NONDEBUG_INSN_P (insn) || !INSN_HAS_LOCATION (insn)))
    insn = PREV_INSN (insn);

  if (insn != end && INSN_LOCATION (insn) == goto_locus)
    return false;

  /* Then scan block B forward.  */
  insn = BB_HEAD (b);
  if (insn)
    {
      end = NEXT_INSN (BB_END (b));
      while (insn != end && !NONDEBUG_INSN_P (insn))
	insn = NEXT_INSN (insn);

      if (insn != end && INSN_HAS_LOCATION (insn)
	  && INSN_LOCATION (insn) == goto_locus)
	return false;
    }

  return true;
}

/* If the single edge between blocks A and B is the only place in RTL which
   holds some unique locus, emit a nop with that locus between the blocks.  */

static void
emit_nop_for_unique_locus_between (basic_block a, basic_block b)
{
  if (!unique_locus_on_edge_between_p (a, b))
    return;

  BB_END (a) = emit_insn_after_noloc (gen_nop (), BB_END (a), a);
  INSN_LOCATION (BB_END (a)) = EDGE_SUCC (a, 0)->goto_locus;
}

/* Blocks A and B are to be merged into a single block A.  The insns
   are already contiguous.  */

static void
rtl_merge_blocks (basic_block a, basic_block b)
{
  /* If B is a forwarder block whose outgoing edge has no location, we'll
     propagate the locus of the edge between A and B onto it.  */
  const bool forward_edge_locus
    = (b->flags & BB_FORWARDER_BLOCK) != 0
      && LOCATION_LOCUS (EDGE_SUCC (b, 0)->goto_locus) == UNKNOWN_LOCATION;
  rtx_insn *b_head = BB_HEAD (b), *b_end = BB_END (b), *a_end = BB_END (a);
  rtx_insn *del_first = NULL, *del_last = NULL;
  rtx_insn *b_debug_start = b_end, *b_debug_end = b_end;
  int b_empty = 0;

  if (dump_file)
    fprintf (dump_file, "Merging block %d into block %d...\n", b->index,
	     a->index);

  while (DEBUG_INSN_P (b_end))
    b_end = PREV_INSN (b_debug_start = b_end);

  /* If there was a CODE_LABEL beginning B, delete it.  */
  if (LABEL_P (b_head))
    {
      /* Detect basic blocks with nothing but a label.  This can happen
	 in particular at the end of a function.  */
      if (b_head == b_end)
	b_empty = 1;

      del_first = del_last = b_head;
      b_head = NEXT_INSN (b_head);
    }

  /* Delete the basic block note and handle blocks containing just that
     note.  */
  if (NOTE_INSN_BASIC_BLOCK_P (b_head))
    {
      if (b_head == b_end)
	b_empty = 1;
      if (! del_last)
	del_first = b_head;

      del_last = b_head;
      b_head = NEXT_INSN (b_head);
    }

  /* If there was a jump out of A, delete it.  */
  if (JUMP_P (a_end))
    {
      rtx_insn *prev;

      for (prev = PREV_INSN (a_end); ; prev = PREV_INSN (prev))
	if (!NOTE_P (prev)
	    || NOTE_INSN_BASIC_BLOCK_P (prev)
	    || prev == BB_HEAD (a))
	  break;

      del_first = a_end;

      /* If this was a conditional jump, we need to also delete
	 the insn that set cc0.  */
      if (HAVE_cc0 && only_sets_cc0_p (prev))
	{
	  rtx_insn *tmp = prev;

	  prev = prev_nonnote_insn (prev);
	  if (!prev)
	    prev = BB_HEAD (a);
	  del_first = tmp;
	}

      a_end = PREV_INSN (del_first);
    }
  else if (BARRIER_P (NEXT_INSN (a_end)))
    del_first = NEXT_INSN (a_end);

  /* Delete everything marked above as well as crap that might be
     hanging out between the two blocks.  */
  BB_END (a) = a_end;
  BB_HEAD (b) = b_empty ? NULL : b_head;
  delete_insn_chain (del_first, del_last, true);

  /* If not optimizing, preserve the locus of the single edge between
     blocks A and B if necessary by emitting a nop.  */
  if (!optimize
      && !forward_edge_locus
      && !DECL_IGNORED_P (current_function_decl))
    {
      emit_nop_for_unique_locus_between (a, b);
      a_end = BB_END (a);
    }

  /* Reassociate the insns of B with A.  */
  if (!b_empty)
    {
      update_bb_for_insn_chain (a_end, b_debug_end, a);

      BB_END (a) = b_debug_end;
      BB_HEAD (b) = NULL;
    }
  else if (b_end != b_debug_end)
    {
      /* Move any deleted labels and other notes between the end of A
	 and the debug insns that make up B after the debug insns,
	 bringing the debug insns into A while keeping the notes after
	 the end of A.  */
      if (NEXT_INSN (a_end) != b_debug_start)
	reorder_insns_nobb (NEXT_INSN (a_end), PREV_INSN (b_debug_start),
			    b_debug_end);
      update_bb_for_insn_chain (b_debug_start, b_debug_end, a);
      BB_END (a) = b_debug_end;
    }

  df_bb_delete (b->index);

  if (forward_edge_locus)
    EDGE_SUCC (b, 0)->goto_locus = EDGE_SUCC (a, 0)->goto_locus;

  if (dump_file)
    fprintf (dump_file, "Merged blocks %d and %d.\n", a->index, b->index);
}


/* Return true when block A and B can be merged.  */

static bool
rtl_can_merge_blocks (basic_block a, basic_block b)
{
  /* If we are partitioning hot/cold basic blocks, we don't want to
     mess up unconditional or indirect jumps that cross between hot
     and cold sections.

     Basic block partitioning may result in some jumps that appear to
     be optimizable (or blocks that appear to be mergeable), but which really
     must be left untouched (they are required to make it safely across
     partition boundaries).  See  the comments at the top of
     bb-reorder.c:partition_hot_cold_basic_blocks for complete details.  */

  if (BB_PARTITION (a) != BB_PARTITION (b))
    return false;

  /* Protect the loop latches.  */
  if (current_loops && b->loop_father->latch == b)
    return false;

  /* There must be exactly one edge in between the blocks.  */
  return (single_succ_p (a)
	  && single_succ (a) == b
	  && single_pred_p (b)
	  && a != b
	  /* Must be simple edge.  */
	  && !(single_succ_edge (a)->flags & EDGE_COMPLEX)
	  && a->next_bb == b
	  && a != ENTRY_BLOCK_PTR_FOR_FN (cfun)
	  && b != EXIT_BLOCK_PTR_FOR_FN (cfun)
	  /* If the jump insn has side effects,
	     we can't kill the edge.  */
	  && (!JUMP_P (BB_END (a))
	      || (reload_completed
		  ? simplejump_p (BB_END (a)) : onlyjump_p (BB_END (a)))));
}

/* Return the label in the head of basic block BLOCK.  Create one if it doesn't
   exist.  */

rtx_code_label *
block_label (basic_block block)
{
  if (block == EXIT_BLOCK_PTR_FOR_FN (cfun))
    return NULL;

  if (!LABEL_P (BB_HEAD (block)))
    {
      BB_HEAD (block) = emit_label_before (gen_label_rtx (), BB_HEAD (block));
    }

  return as_a <rtx_code_label *> (BB_HEAD (block));
}

/* Attempt to perform edge redirection by replacing possibly complex jump
   instruction by unconditional jump or removing jump completely.  This can
   apply only if all edges now point to the same block.  The parameters and
   return values are equivalent to redirect_edge_and_branch.  */

edge
try_redirect_by_replacing_jump (edge e, basic_block target, bool in_cfglayout)
{
  basic_block src = e->src;
  rtx_insn *insn = BB_END (src), *kill_from;
  rtx set;
  int fallthru = 0;

  /* If we are partitioning hot/cold basic blocks, we don't want to
     mess up unconditional or indirect jumps that cross between hot
     and cold sections.

     Basic block partitioning may result in some jumps that appear to
     be optimizable (or blocks that appear to be mergeable), but which really
     must be left untouched (they are required to make it safely across
     partition boundaries).  See  the comments at the top of
     bb-reorder.c:partition_hot_cold_basic_blocks for complete details.  */

  if (BB_PARTITION (src) != BB_PARTITION (target))
    return NULL;

  /* We can replace or remove a complex jump only when we have exactly
     two edges.  Also, if we have exactly one outgoing edge, we can
     redirect that.  */
  if (EDGE_COUNT (src->succs) >= 3
      /* Verify that all targets will be TARGET.  Specifically, the
	 edge that is not E must also go to TARGET.  */
      || (EDGE_COUNT (src->succs) == 2
	  && EDGE_SUCC (src, EDGE_SUCC (src, 0) == e)->dest != target))
    return NULL;

  if (!onlyjump_p (insn))
    return NULL;
  if ((!optimize || reload_completed) && tablejump_p (insn, NULL, NULL))
    return NULL;

  /* Avoid removing branch with side effects.  */
  set = single_set (insn);
  if (!set || side_effects_p (set))
    return NULL;

  /* In case we zap a conditional jump, we'll need to kill
     the cc0 setter too.  */
  kill_from = insn;
  if (HAVE_cc0 && reg_mentioned_p (cc0_rtx, PATTERN (insn))
      && only_sets_cc0_p (PREV_INSN (insn)))
    kill_from = PREV_INSN (insn);

  /* See if we can create the fallthru edge.  */
  if (in_cfglayout || can_fallthru (src, target))
    {
      if (dump_file)
	fprintf (dump_file, "Removing jump %i.\n", INSN_UID (insn));
      fallthru = 1;

      /* Selectively unlink whole insn chain.  */
      if (in_cfglayout)
	{
	  rtx_insn *insn = BB_FOOTER (src);

	  delete_insn_chain (kill_from, BB_END (src), false);

	  /* Remove barriers but keep jumptables.  */
	  while (insn)
	    {
	      if (BARRIER_P (insn))
		{
		  if (PREV_INSN (insn))
		    SET_NEXT_INSN (PREV_INSN (insn)) = NEXT_INSN (insn);
		  else
		    BB_FOOTER (src) = NEXT_INSN (insn);
		  if (NEXT_INSN (insn))
		    SET_PREV_INSN (NEXT_INSN (insn)) = PREV_INSN (insn);
		}
	      if (LABEL_P (insn))
		break;
	      insn = NEXT_INSN (insn);
	    }
	}
      else
	delete_insn_chain (kill_from, PREV_INSN (BB_HEAD (target)),
			   false);
    }

  /* If this already is simplejump, redirect it.  */
  else if (simplejump_p (insn))
    {
      if (e->dest == target)
	return NULL;
      if (dump_file)
	fprintf (dump_file, "Redirecting jump %i from %i to %i.\n",
		 INSN_UID (insn), e->dest->index, target->index);
      if (!redirect_jump (as_a <rtx_jump_insn *> (insn),
			  block_label (target), 0))
	{
	  gcc_assert (target == EXIT_BLOCK_PTR_FOR_FN (cfun));
	  return NULL;
	}
    }

  /* Cannot do anything for target exit block.  */
  else if (target == EXIT_BLOCK_PTR_FOR_FN (cfun))
    return NULL;

  /* Or replace possibly complicated jump insn by simple jump insn.  */
  else
    {
      rtx_code_label *target_label = block_label (target);
      rtx_insn *barrier;
      rtx_insn *label;
      rtx_jump_table_data *table;

      emit_jump_insn_after_noloc (targetm.gen_jump (target_label), insn);
      JUMP_LABEL (BB_END (src)) = target_label;
      LABEL_NUSES (target_label)++;
      if (dump_file)
	fprintf (dump_file, "Replacing insn %i by jump %i\n",
		 INSN_UID (insn), INSN_UID (BB_END (src)));


      delete_insn_chain (kill_from, insn, false);

      /* Recognize a tablejump that we are converting to a
	 simple jump and remove its associated CODE_LABEL
	 and ADDR_VEC or ADDR_DIFF_VEC.  */
      if (tablejump_p (insn, &label, &table))
	delete_insn_chain (label, table, false);

      barrier = next_nonnote_nondebug_insn (BB_END (src));
      if (!barrier || !BARRIER_P (barrier))
	emit_barrier_after (BB_END (src));
      else
	{
	  if (barrier != NEXT_INSN (BB_END (src)))
	    {
	      /* Move the jump before barrier so that the notes
		 which originally were or were created before jump table are
		 inside the basic block.  */
	      rtx_insn *new_insn = BB_END (src);

	      update_bb_for_insn_chain (NEXT_INSN (BB_END (src)),
				        PREV_INSN (barrier), src);

	      SET_NEXT_INSN (PREV_INSN (new_insn)) = NEXT_INSN (new_insn);
	      SET_PREV_INSN (NEXT_INSN (new_insn)) = PREV_INSN (new_insn);

	      SET_NEXT_INSN (new_insn) = barrier;
	      SET_NEXT_INSN (PREV_INSN (barrier)) = new_insn;

	      SET_PREV_INSN (new_insn) = PREV_INSN (barrier);
	      SET_PREV_INSN (barrier) = new_insn;
	    }
	}
    }

  /* Keep only one edge out and set proper flags.  */
  if (!single_succ_p (src))
    remove_edge (e);
  gcc_assert (single_succ_p (src));

  e = single_succ_edge (src);
  if (fallthru)
    e->flags = EDGE_FALLTHRU;
  else
    e->flags = 0;

  e->probability = profile_probability::always ();

  if (e->dest != target)
    redirect_edge_succ (e, target);
  return e;
}

/* Subroutine of redirect_branch_edge that tries to patch the jump
   instruction INSN so that it reaches block NEW.  Do this
   only when it originally reached block OLD.  Return true if this
   worked or the original target wasn't OLD, return false if redirection
   doesn't work.  */

static bool
patch_jump_insn (rtx_insn *insn, rtx_insn *old_label, basic_block new_bb)
{
  rtx_jump_table_data *table;
  rtx tmp;
  /* Recognize a tablejump and adjust all matching cases.  */
  if (tablejump_p (insn, NULL, &table))
    {
      rtvec vec;
      int j;
      rtx_code_label *new_label = block_label (new_bb);

      if (new_bb == EXIT_BLOCK_PTR_FOR_FN (cfun))
	return false;
      vec = table->get_labels ();

      for (j = GET_NUM_ELEM (vec) - 1; j >= 0; --j)
	if (XEXP (RTVEC_ELT (vec, j), 0) == old_label)
	  {
	    RTVEC_ELT (vec, j) = gen_rtx_LABEL_REF (Pmode, new_label);
	    --LABEL_NUSES (old_label);
	    ++LABEL_NUSES (new_label);
	  }

      /* Handle casesi dispatch insns.  */
      if ((tmp = single_set (insn)) != NULL
	  && SET_DEST (tmp) == pc_rtx
	  && GET_CODE (SET_SRC (tmp)) == IF_THEN_ELSE
	  && GET_CODE (XEXP (SET_SRC (tmp), 2)) == LABEL_REF
	  && label_ref_label (XEXP (SET_SRC (tmp), 2)) == old_label)
	{
	  XEXP (SET_SRC (tmp), 2) = gen_rtx_LABEL_REF (Pmode,
						       new_label);
	  --LABEL_NUSES (old_label);
	  ++LABEL_NUSES (new_label);
	}
    }
  else if ((tmp = extract_asm_operands (PATTERN (insn))) != NULL)
    {
      int i, n = ASM_OPERANDS_LABEL_LENGTH (tmp);
      rtx note;

      if (new_bb == EXIT_BLOCK_PTR_FOR_FN (cfun))
	return false;
      rtx_code_label *new_label = block_label (new_bb);

      for (i = 0; i < n; ++i)
	{
	  rtx old_ref = ASM_OPERANDS_LABEL (tmp, i);
	  gcc_assert (GET_CODE (old_ref) == LABEL_REF);
	  if (XEXP (old_ref, 0) == old_label)
	    {
	      ASM_OPERANDS_LABEL (tmp, i)
		= gen_rtx_LABEL_REF (Pmode, new_label);
	      --LABEL_NUSES (old_label);
	      ++LABEL_NUSES (new_label);
	    }
	}

      if (JUMP_LABEL (insn) == old_label)
	{
	  JUMP_LABEL (insn) = new_label;
	  note = find_reg_note (insn, REG_LABEL_TARGET, new_label);
	  if (note)
	    remove_note (insn, note);
	}
      else
	{
	  note = find_reg_note (insn, REG_LABEL_TARGET, old_label);
	  if (note)
	    remove_note (insn, note);
	  if (JUMP_LABEL (insn) != new_label
	      && !find_reg_note (insn, REG_LABEL_TARGET, new_label))
	    add_reg_note (insn, REG_LABEL_TARGET, new_label);
	}
      while ((note = find_reg_note (insn, REG_LABEL_OPERAND, old_label))
	     != NULL_RTX)
	XEXP (note, 0) = new_label;
    }
  else
    {
      /* ?? We may play the games with moving the named labels from
	 one basic block to the other in case only one computed_jump is
	 available.  */
      if (computed_jump_p (insn)
	  /* A return instruction can't be redirected.  */
	  || returnjump_p (insn))
	return false;

      if (!currently_expanding_to_rtl || JUMP_LABEL (insn) == old_label)
	{
	  /* If the insn doesn't go where we think, we're confused.  */
	  gcc_assert (JUMP_LABEL (insn) == old_label);

	  /* If the substitution doesn't succeed, die.  This can happen
	     if the back end emitted unrecognizable instructions or if
	     target is exit block on some arches.  */
	  if (!redirect_jump (as_a <rtx_jump_insn *> (insn),
			      block_label (new_bb), 0))
	    {
	      gcc_assert (new_bb == EXIT_BLOCK_PTR_FOR_FN (cfun));
	      return false;
	    }
	}
    }
  return true;
}


/* Redirect edge representing branch of (un)conditional jump or tablejump,
   NULL on failure  */
static edge
redirect_branch_edge (edge e, basic_block target)
{
  rtx_insn *old_label = BB_HEAD (e->dest);
  basic_block src = e->src;
  rtx_insn *insn = BB_END (src);

  /* We can only redirect non-fallthru edges of jump insn.  */
  if (e->flags & EDGE_FALLTHRU)
    return NULL;
  else if (!JUMP_P (insn) && !currently_expanding_to_rtl)
    return NULL;

  if (!currently_expanding_to_rtl)
    {
      if (!patch_jump_insn (as_a <rtx_jump_insn *> (insn), old_label, target))
	return NULL;
    }
  else
    /* When expanding this BB might actually contain multiple
       jumps (i.e. not yet split by find_many_sub_basic_blocks).
       Redirect all of those that match our label.  */
    FOR_BB_INSNS (src, insn)
      if (JUMP_P (insn) && !patch_jump_insn (as_a <rtx_jump_insn *> (insn),
					     old_label, target))
	return NULL;

  if (dump_file)
    fprintf (dump_file, "Edge %i->%i redirected to %i\n",
	     e->src->index, e->dest->index, target->index);

  if (e->dest != target)
    e = redirect_edge_succ_nodup (e, target);

  return e;
}

/* Called when edge E has been redirected to a new destination,
   in order to update the region crossing flag on the edge and
   jump.  */

static void
fixup_partition_crossing (edge e)
{
  if (e->src == ENTRY_BLOCK_PTR_FOR_FN (cfun) || e->dest
      == EXIT_BLOCK_PTR_FOR_FN (cfun))
    return;
  /* If we redirected an existing edge, it may already be marked
     crossing, even though the new src is missing a reg crossing note.
     But make sure reg crossing note doesn't already exist before
     inserting.  */
  if (BB_PARTITION (e->src) != BB_PARTITION (e->dest))
    {
      e->flags |= EDGE_CROSSING;
      if (JUMP_P (BB_END (e->src)))
	CROSSING_JUMP_P (BB_END (e->src)) = 1;
    }
  else if (BB_PARTITION (e->src) == BB_PARTITION (e->dest))
    {
      e->flags &= ~EDGE_CROSSING;
      /* Remove the section crossing note from jump at end of
         src if it exists, and if no other successors are
         still crossing.  */
      if (JUMP_P (BB_END (e->src)) && CROSSING_JUMP_P (BB_END (e->src)))
        {
          bool has_crossing_succ = false;
          edge e2;
          edge_iterator ei;
          FOR_EACH_EDGE (e2, ei, e->src->succs)
            {
              has_crossing_succ |= (e2->flags & EDGE_CROSSING);
              if (has_crossing_succ)
                break;
            }
          if (!has_crossing_succ)
	    CROSSING_JUMP_P (BB_END (e->src)) = 0;
        }
    }
}

/* Called when block BB has been reassigned to the cold partition,
   because it is now dominated by another cold block,
   to ensure that the region crossing attributes are updated.  */

static void
fixup_new_cold_bb (basic_block bb)
{
  edge e;
  edge_iterator ei;

  /* This is called when a hot bb is found to now be dominated
     by a cold bb and therefore needs to become cold. Therefore,
     its preds will no longer be region crossing. Any non-dominating
     preds that were previously hot would also have become cold
     in the caller for the same region. Any preds that were previously
     region-crossing will be adjusted in fixup_partition_crossing.  */
  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      fixup_partition_crossing (e);
    }

  /* Possibly need to make bb's successor edges region crossing,
     or remove stale region crossing.  */
  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      /* We can't have fall-through edges across partition boundaries.
         Note that force_nonfallthru will do any necessary partition
         boundary fixup by calling fixup_partition_crossing itself.  */
      if ((e->flags & EDGE_FALLTHRU)
          && BB_PARTITION (bb) != BB_PARTITION (e->dest)
	  && e->dest != EXIT_BLOCK_PTR_FOR_FN (cfun))
        force_nonfallthru (e);
      else
        fixup_partition_crossing (e);
    }
}

/* Attempt to change code to redirect edge E to TARGET.  Don't do that on
   expense of adding new instructions or reordering basic blocks.

   Function can be also called with edge destination equivalent to the TARGET.
   Then it should try the simplifications and do nothing if none is possible.

   Return edge representing the branch if transformation succeeded.  Return NULL
   on failure.
   We still return NULL in case E already destinated TARGET and we didn't
   managed to simplify instruction stream.  */

static edge
rtl_redirect_edge_and_branch (edge e, basic_block target)
{
  edge ret;
  basic_block src = e->src;
  basic_block dest = e->dest;

  if (e->flags & (EDGE_ABNORMAL_CALL | EDGE_EH))
    return NULL;

  if (dest == target)
    return e;

  if ((ret = try_redirect_by_replacing_jump (e, target, false)) != NULL)
    {
      df_set_bb_dirty (src);
      fixup_partition_crossing (ret);
      return ret;
    }

  ret = redirect_branch_edge (e, target);
  if (!ret)
    return NULL;

  df_set_bb_dirty (src);
  fixup_partition_crossing (ret);
  return ret;
}

/* Emit a barrier after BB, into the footer if we are in CFGLAYOUT mode.  */

void
emit_barrier_after_bb (basic_block bb)
{
  rtx_barrier *barrier = emit_barrier_after (BB_END (bb));
  gcc_assert (current_ir_type () == IR_RTL_CFGRTL
              || current_ir_type () == IR_RTL_CFGLAYOUT);
  if (current_ir_type () == IR_RTL_CFGLAYOUT)
    {
      rtx_insn *insn = unlink_insn_chain (barrier, barrier);

      if (BB_FOOTER (bb))
	{
          rtx_insn *footer_tail = BB_FOOTER (bb);

          while (NEXT_INSN (footer_tail))
            footer_tail = NEXT_INSN (footer_tail);
          if (!BARRIER_P (footer_tail))
            {
              SET_NEXT_INSN (footer_tail) = insn;
              SET_PREV_INSN (insn) = footer_tail;
            }
	}
      else
        BB_FOOTER (bb) = insn;
    }
}

/* Like force_nonfallthru below, but additionally performs redirection
   Used by redirect_edge_and_branch_force.  JUMP_LABEL is used only
   when redirecting to the EXIT_BLOCK, it is either ret_rtx or
   simple_return_rtx, indicating which kind of returnjump to create.
   It should be NULL otherwise.  */

basic_block
force_nonfallthru_and_redirect (edge e, basic_block target, rtx jump_label)
{
  basic_block jump_block, new_bb = NULL, src = e->src;
  rtx note;
  edge new_edge;
  int abnormal_edge_flags = 0;
  bool asm_goto_edge = false;
  int loc;

  /* In the case the last instruction is conditional jump to the next
     instruction, first redirect the jump itself and then continue
     by creating a basic block afterwards to redirect fallthru edge.  */
  if (e->src != ENTRY_BLOCK_PTR_FOR_FN (cfun)
      && e->dest != EXIT_BLOCK_PTR_FOR_FN (cfun)
      && any_condjump_p (BB_END (e->src))
      && JUMP_LABEL (BB_END (e->src)) == BB_HEAD (e->dest))
    {
      rtx note;
      edge b = unchecked_make_edge (e->src, target, 0);
      bool redirected;

      redirected = redirect_jump (as_a <rtx_jump_insn *> (BB_END (e->src)),
				  block_label (target), 0);
      gcc_assert (redirected);

      note = find_reg_note (BB_END (e->src), REG_BR_PROB, NULL_RTX);
      if (note)
	{
	  int prob = XINT (note, 0);

	  b->probability = profile_probability::from_reg_br_prob_note (prob);
	  e->probability -= e->probability;
	}
    }

  if (e->flags & EDGE_ABNORMAL)
    {
      /* Irritating special case - fallthru edge to the same block as abnormal
	 edge.
	 We can't redirect abnormal edge, but we still can split the fallthru
	 one and create separate abnormal edge to original destination.
	 This allows bb-reorder to make such edge non-fallthru.  */
      gcc_assert (e->dest == target);
      abnormal_edge_flags = e->flags & ~EDGE_FALLTHRU;
      e->flags &= EDGE_FALLTHRU;
    }
  else
    {
      gcc_assert (e->flags & EDGE_FALLTHRU);
      if (e->src == ENTRY_BLOCK_PTR_FOR_FN (cfun))
	{
	  /* We can't redirect the entry block.  Create an empty block
	     at the start of the function which we use to add the new
	     jump.  */
	  edge tmp;
	  edge_iterator ei;
	  bool found = false;

	  basic_block bb = create_basic_block (BB_HEAD (e->dest), NULL,
					       ENTRY_BLOCK_PTR_FOR_FN (cfun));
	  bb->count = ENTRY_BLOCK_PTR_FOR_FN (cfun)->count;

	  /* Make sure new block ends up in correct hot/cold section.  */
	  BB_COPY_PARTITION (bb, e->dest);

	  /* Change the existing edge's source to be the new block, and add
	     a new edge from the entry block to the new block.  */
	  e->src = bb;
	  for (ei = ei_start (ENTRY_BLOCK_PTR_FOR_FN (cfun)->succs);
	       (tmp = ei_safe_edge (ei)); )
	    {
	      if (tmp == e)
		{
		  ENTRY_BLOCK_PTR_FOR_FN (cfun)->succs->unordered_remove (ei.index);
		  found = true;
		  break;
		}
	      else
		ei_next (&ei);
	    }

	  gcc_assert (found);

	  vec_safe_push (bb->succs, e);
	  make_single_succ_edge (ENTRY_BLOCK_PTR_FOR_FN (cfun), bb,
				 EDGE_FALLTHRU);
	}
    }

  /* If e->src ends with asm goto, see if any of the ASM_OPERANDS_LABELs
     don't point to the target or fallthru label.  */
  if (JUMP_P (BB_END (e->src))
      && target != EXIT_BLOCK_PTR_FOR_FN (cfun)
      && (e->flags & EDGE_FALLTHRU)
      && (note = extract_asm_operands (PATTERN (BB_END (e->src)))))
    {
      int i, n = ASM_OPERANDS_LABEL_LENGTH (note);
      bool adjust_jump_target = false;

      for (i = 0; i < n; ++i)
	{
	  if (XEXP (ASM_OPERANDS_LABEL (note, i), 0) == BB_HEAD (e->dest))
	    {
	      LABEL_NUSES (XEXP (ASM_OPERANDS_LABEL (note, i), 0))--;
	      XEXP (ASM_OPERANDS_LABEL (note, i), 0) = block_label (target);
	      LABEL_NUSES (XEXP (ASM_OPERANDS_LABEL (note, i), 0))++;
	      adjust_jump_target = true;
	    }
	  if (XEXP (ASM_OPERANDS_LABEL (note, i), 0) == BB_HEAD (target))
	    asm_goto_edge = true;
	}
      if (adjust_jump_target)
	{
	  rtx_insn *insn = BB_END (e->src);
	  rtx note;
	  rtx_insn *old_label = BB_HEAD (e->dest);
	  rtx_insn *new_label = BB_HEAD (target);

	  if (JUMP_LABEL (insn) == old_label)
	    {
	      JUMP_LABEL (insn) = new_label;
	      note = find_reg_note (insn, REG_LABEL_TARGET, new_label);
	      if (note)
		remove_note (insn, note);
	    }
	  else
	    {
	      note = find_reg_note (insn, REG_LABEL_TARGET, old_label);
	      if (note)
		remove_note (insn, note);
	      if (JUMP_LABEL (insn) != new_label
		  && !find_reg_note (insn, REG_LABEL_TARGET, new_label))
		add_reg_note (insn, REG_LABEL_TARGET, new_label);
	    }
	  while ((note = find_reg_note (insn, REG_LABEL_OPERAND, old_label))
		 != NULL_RTX)
	    XEXP (note, 0) = new_label;
	}
    }

  if (EDGE_COUNT (e->src->succs) >= 2 || abnormal_edge_flags || asm_goto_edge)
    {
      rtx_insn *new_head;
      profile_count count = e->count ();
      profile_probability probability = e->probability;
      /* Create the new structures.  */

      /* If the old block ended with a tablejump, skip its table
	 by searching forward from there.  Otherwise start searching
	 forward from the last instruction of the old block.  */
      rtx_jump_table_data *table;
      if (tablejump_p (BB_END (e->src), NULL, &table))
	new_head = table;
      else
	new_head = BB_END (e->src);
      new_head = NEXT_INSN (new_head);

      jump_block = create_basic_block (new_head, NULL, e->src);
      jump_block->count = count;

      /* Make sure new block ends up in correct hot/cold section.  */

      BB_COPY_PARTITION (jump_block, e->src);

      /* Wire edge in.  */
      new_edge = make_edge (e->src, jump_block, EDGE_FALLTHRU);
      new_edge->probability = probability;

      /* Redirect old edge.  */
      redirect_edge_pred (e, jump_block);
      e->probability = profile_probability::always ();

      /* If e->src was previously region crossing, it no longer is
         and the reg crossing note should be removed.  */
      fixup_partition_crossing (new_edge);

      /* If asm goto has any label refs to target's label,
	 add also edge from asm goto bb to target.  */
      if (asm_goto_edge)
	{
	  new_edge->probability = new_edge->probability.apply_scale (1, 2);
	  jump_block->count = jump_block->count.apply_scale (1, 2);
	  edge new_edge2 = make_edge (new_edge->src, target,
				      e->flags & ~EDGE_FALLTHRU);
	  new_edge2->probability = probability - new_edge->probability;
	}

      new_bb = jump_block;
    }
  else
    jump_block = e->src;

  loc = e->goto_locus;
  e->flags &= ~EDGE_FALLTHRU;
  if (target == EXIT_BLOCK_PTR_FOR_FN (cfun))
    {
      if (jump_label == ret_rtx)
	emit_jump_insn_after_setloc (targetm.gen_return (),
				     BB_END (jump_block), loc);
      else
	{
	  gcc_assert (jump_label == simple_return_rtx);
	  emit_jump_insn_after_setloc (targetm.gen_simple_return (),
				       BB_END (jump_block), loc);
	}
      set_return_jump_label (BB_END (jump_block));
    }
  else
    {
      rtx_code_label *label = block_label (target);
      emit_jump_insn_after_setloc (targetm.gen_jump (label),
				   BB_END (jump_block), loc);
      JUMP_LABEL (BB_END (jump_block)) = label;
      LABEL_NUSES (label)++;
    }

  /* We might be in cfg layout mode, and if so, the following routine will
     insert the barrier correctly.  */
  emit_barrier_after_bb (jump_block);
  redirect_edge_succ_nodup (e, target);

  if (abnormal_edge_flags)
    make_edge (src, target, abnormal_edge_flags);

  df_mark_solutions_dirty ();
  fixup_partition_crossing (e);
  return new_bb;
}

/* Edge E is assumed to be fallthru edge.  Emit needed jump instruction
   (and possibly create new basic block) to make edge non-fallthru.
   Return newly created BB or NULL if none.  */

static basic_block
rtl_force_nonfallthru (edge e)
{
  return force_nonfallthru_and_redirect (e, e->dest, NULL_RTX);
}

/* Redirect edge even at the expense of creating new jump insn or
   basic block.  Return new basic block if created, NULL otherwise.
   Conversion must be possible.  */

static basic_block
rtl_redirect_edge_and_branch_force (edge e, basic_block target)
{
  if (redirect_edge_and_branch (e, target)
      || e->dest == target)
    return NULL;

  /* In case the edge redirection failed, try to force it to be non-fallthru
     and redirect newly created simplejump.  */
  df_set_bb_dirty (e->src);
  return force_nonfallthru_and_redirect (e, target, NULL_RTX);
}

/* The given edge should potentially be a fallthru edge.  If that is in
   fact true, delete the jump and barriers that are in the way.  */

static void
rtl_tidy_fallthru_edge (edge e)
{
  rtx_insn *q;
  basic_block b = e->src, c = b->next_bb;

  /* ??? In a late-running flow pass, other folks may have deleted basic
     blocks by nopping out blocks, leaving multiple BARRIERs between here
     and the target label. They ought to be chastised and fixed.

     We can also wind up with a sequence of undeletable labels between
     one block and the next.

     So search through a sequence of barriers, labels, and notes for
     the head of block C and assert that we really do fall through.  */

  for (q = NEXT_INSN (BB_END (b)); q != BB_HEAD (c); q = NEXT_INSN (q))
    if (NONDEBUG_INSN_P (q))
      return;

  /* Remove what will soon cease being the jump insn from the source block.
     If block B consisted only of this single jump, turn it into a deleted
     note.  */
  q = BB_END (b);
  if (JUMP_P (q)
      && onlyjump_p (q)
      && (any_uncondjump_p (q)
	  || single_succ_p (b)))
    {
      rtx_insn *label;
      rtx_jump_table_data *table;

      if (tablejump_p (q, &label, &table))
	{
	  /* The label is likely mentioned in some instruction before
	     the tablejump and might not be DCEd, so turn it into
	     a note instead and move before the tablejump that is going to
	     be deleted.  */
	  const char *name = LABEL_NAME (label);
	  PUT_CODE (label, NOTE);
	  NOTE_KIND (label) = NOTE_INSN_DELETED_LABEL;
	  NOTE_DELETED_LABEL_NAME (label) = name;
	  reorder_insns (label, label, PREV_INSN (q));
	  delete_insn (table);
	}

      /* If this was a conditional jump, we need to also delete
	 the insn that set cc0.  */
      if (HAVE_cc0 && any_condjump_p (q) && only_sets_cc0_p (PREV_INSN (q)))
	q = PREV_INSN (q);

      q = PREV_INSN (q);
    }
  /* Unconditional jumps with side-effects (i.e. which we can't just delete
     together with the barrier) should never have a fallthru edge.  */
  else if (JUMP_P (q) && any_uncondjump_p (q))
    return;

  /* Selectively unlink the sequence.  */
  if (q != PREV_INSN (BB_HEAD (c)))
    delete_insn_chain (NEXT_INSN (q), PREV_INSN (BB_HEAD (c)), false);

  e->flags |= EDGE_FALLTHRU;
}

/* Should move basic block BB after basic block AFTER.  NIY.  */

static bool
rtl_move_block_after (basic_block bb ATTRIBUTE_UNUSED,
		      basic_block after ATTRIBUTE_UNUSED)
{
  return false;
}

/* Locate the last bb in the same partition as START_BB.  */

static basic_block
last_bb_in_partition (basic_block start_bb)
{
  basic_block bb;
  FOR_BB_BETWEEN (bb, start_bb, EXIT_BLOCK_PTR_FOR_FN (cfun), next_bb)
    {
      if (BB_PARTITION (start_bb) != BB_PARTITION (bb->next_bb))
        return bb;
    }
  /* Return bb before the exit block.  */
  return bb->prev_bb;
}

/* Split a (typically critical) edge.  Return the new block.
   The edge must not be abnormal.

   ??? The code generally expects to be called on critical edges.
   The case of a block ending in an unconditional jump to a
   block with multiple predecessors is not handled optimally.  */

static basic_block
rtl_split_edge (edge edge_in)
{
  basic_block bb, new_bb;
  rtx_insn *before;

  /* Abnormal edges cannot be split.  */
  gcc_assert (!(edge_in->flags & EDGE_ABNORMAL));

  /* We are going to place the new block in front of edge destination.
     Avoid existence of fallthru predecessors.  */
  if ((edge_in->flags & EDGE_FALLTHRU) == 0)
    {
      edge e = find_fallthru_edge (edge_in->dest->preds);

      if (e)
	force_nonfallthru (e);
    }

  /* Create the basic block note.  */
  if (edge_in->dest != EXIT_BLOCK_PTR_FOR_FN (cfun))
    before = BB_HEAD (edge_in->dest);
  else
    before = NULL;

  /* If this is a fall through edge to the exit block, the blocks might be
     not adjacent, and the right place is after the source.  */
  if ((edge_in->flags & EDGE_FALLTHRU)
      && edge_in->dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
    {
      before = NEXT_INSN (BB_END (edge_in->src));
      bb = create_basic_block (before, NULL, edge_in->src);
      BB_COPY_PARTITION (bb, edge_in->src);
    }
  else
    {
      if (edge_in->src == ENTRY_BLOCK_PTR_FOR_FN (cfun))
        {
          bb = create_basic_block (before, NULL, edge_in->dest->prev_bb);
          BB_COPY_PARTITION (bb, edge_in->dest);
        }
      else
        {
          basic_block after = edge_in->dest->prev_bb;
          /* If this is post-bb reordering, and the edge crosses a partition
             boundary, the new block needs to be inserted in the bb chain
             at the end of the src partition (since we put the new bb into
             that partition, see below). Otherwise we may end up creating
             an extra partition crossing in the chain, which is illegal.
             It can't go after the src, because src may have a fall-through
             to a different block.  */
          if (crtl->bb_reorder_complete
              && (edge_in->flags & EDGE_CROSSING))
            {
              after = last_bb_in_partition (edge_in->src);
              before = get_last_bb_insn (after);
              /* The instruction following the last bb in partition should
                 be a barrier, since it cannot end in a fall-through.  */
              gcc_checking_assert (BARRIER_P (before));
              before = NEXT_INSN (before);
            }
          bb = create_basic_block (before, NULL, after);
          /* Put the split bb into the src partition, to avoid creating
             a situation where a cold bb dominates a hot bb, in the case
             where src is cold and dest is hot. The src will dominate
             the new bb (whereas it might not have dominated dest).  */
          BB_COPY_PARTITION (bb, edge_in->src);
        }
    }

  make_single_succ_edge (bb, edge_in->dest, EDGE_FALLTHRU);

  /* Can't allow a region crossing edge to be fallthrough.  */
  if (BB_PARTITION (bb) != BB_PARTITION (edge_in->dest)
      && edge_in->dest != EXIT_BLOCK_PTR_FOR_FN (cfun))
    {
      new_bb = force_nonfallthru (single_succ_edge (bb));
      gcc_assert (!new_bb);
    }

  /* For non-fallthru edges, we must adjust the predecessor's
     jump instruction to target our new block.  */
  if ((edge_in->flags & EDGE_FALLTHRU) == 0)
    {
      edge redirected = redirect_edge_and_branch (edge_in, bb);
      gcc_assert (redirected);
    }
  else
    {
      if (edge_in->src != ENTRY_BLOCK_PTR_FOR_FN (cfun))
	{
	  /* For asm goto even splitting of fallthru edge might
	     need insn patching, as other labels might point to the
	     old label.  */
	  rtx_insn *last = BB_END (edge_in->src);
	  if (last
	      && JUMP_P (last)
	      && edge_in->dest != EXIT_BLOCK_PTR_FOR_FN (cfun)
	      && (extract_asm_operands (PATTERN (last))
		  || JUMP_LABEL (last) == before)
	      && patch_jump_insn (last, before, bb))
	    df_set_bb_dirty (edge_in->src);
	}
      redirect_edge_succ (edge_in, bb);
    }

  return bb;
}

/* Queue instructions for insertion on an edge between two basic blocks.
   The new instructions and basic blocks (if any) will not appear in the
   CFG until commit_edge_insertions is called.  */

void
insert_insn_on_edge (rtx pattern, edge e)
{
  /* We cannot insert instructions on an abnormal critical edge.
     It will be easier to find the culprit if we die now.  */
  gcc_assert (!((e->flags & EDGE_ABNORMAL) && EDGE_CRITICAL_P (e)));

  if (e->insns.r == NULL_RTX)
    start_sequence ();
  else
    push_to_sequence (e->insns.r);

  emit_insn (pattern);

  e->insns.r = get_insns ();
  end_sequence ();
}

/* Update the CFG for the instructions queued on edge E.  */

void
commit_one_edge_insertion (edge e)
{
  rtx_insn *before = NULL, *after = NULL, *insns, *tmp, *last;
  basic_block bb;

  /* Pull the insns off the edge now since the edge might go away.  */
  insns = e->insns.r;
  e->insns.r = NULL;

  /* Figure out where to put these insns.  If the destination has
     one predecessor, insert there.  Except for the exit block.  */
  if (single_pred_p (e->dest) && e->dest != EXIT_BLOCK_PTR_FOR_FN (cfun))
    {
      bb = e->dest;

      /* Get the location correct wrt a code label, and "nice" wrt
	 a basic block note, and before everything else.  */
      tmp = BB_HEAD (bb);
      if (LABEL_P (tmp))
	tmp = NEXT_INSN (tmp);
      if (NOTE_INSN_BASIC_BLOCK_P (tmp))
	tmp = NEXT_INSN (tmp);
      if (tmp == BB_HEAD (bb))
	before = tmp;
      else if (tmp)
	after = PREV_INSN (tmp);
      else
	after = get_last_insn ();
    }

  /* If the source has one successor and the edge is not abnormal,
     insert there.  Except for the entry block.
     Don't do this if the predecessor ends in a jump other than
     unconditional simple jump.  E.g. for asm goto that points all
     its labels at the fallthru basic block, we can't insert instructions
     before the asm goto, as the asm goto can have various of side effects,
     and can't emit instructions after the asm goto, as it must end
     the basic block.  */
  else if ((e->flags & EDGE_ABNORMAL) == 0
	   && single_succ_p (e->src)
	   && e->src != ENTRY_BLOCK_PTR_FOR_FN (cfun)
	   && (!JUMP_P (BB_END (e->src))
	       || simplejump_p (BB_END (e->src))))
    {
      bb = e->src;

      /* It is possible to have a non-simple jump here.  Consider a target
	 where some forms of unconditional jumps clobber a register.  This
	 happens on the fr30 for example.

	 We know this block has a single successor, so we can just emit
	 the queued insns before the jump.  */
      if (JUMP_P (BB_END (bb)))
	before = BB_END (bb);
      else
	{
	  /* We'd better be fallthru, or we've lost track of what's what.  */
	  gcc_assert (e->flags & EDGE_FALLTHRU);

	  after = BB_END (bb);
	}
    }

  /* Otherwise we must split the edge.  */
  else
    {
      bb = split_edge (e);

      /* If E crossed a partition boundary, we needed to make bb end in
         a region-crossing jump, even though it was originally fallthru.  */
      if (JUMP_P (BB_END (bb)))
	before = BB_END (bb);
      else
        after = BB_END (bb);
    }

  /* Now that we've found the spot, do the insertion.  */
  if (before)
    {
      emit_insn_before_noloc (insns, before, bb);
      last = prev_nonnote_insn (before);
    }
  else
    last = emit_insn_after_noloc (insns, after, bb);

  if (returnjump_p (last))
    {
      /* ??? Remove all outgoing edges from BB and add one for EXIT.
	 This is not currently a problem because this only happens
	 for the (single) epilogue, which already has a fallthru edge
	 to EXIT.  */

      e = single_succ_edge (bb);
      gcc_assert (e->dest == EXIT_BLOCK_PTR_FOR_FN (cfun)
		  && single_succ_p (bb) && (e->flags & EDGE_FALLTHRU));

      e->flags &= ~EDGE_FALLTHRU;
      emit_barrier_after (last);

      if (before)
	delete_insn (before);
    }
  else
    gcc_assert (!JUMP_P (last));
}

/* Update the CFG for all queued instructions.  */

void
commit_edge_insertions (void)
{
  basic_block bb;

  /* Optimization passes that invoke this routine can cause hot blocks
     previously reached by both hot and cold blocks to become dominated only
     by cold blocks. This will cause the verification below to fail,
     and lead to now cold code in the hot section. In some cases this
     may only be visible after newly unreachable blocks are deleted,
     which will be done by fixup_partitions.  */
  fixup_partitions ();

  checking_verify_flow_info ();

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (cfun),
		  EXIT_BLOCK_PTR_FOR_FN (cfun), next_bb)
    {
      edge e;
      edge_iterator ei;

      FOR_EACH_EDGE (e, ei, bb->succs)
	if (e->insns.r)
	  commit_one_edge_insertion (e);
    }
}


/* Print out RTL-specific basic block information (live information
   at start and end with TDF_DETAILS).  FLAGS are the TDF_* masks
   documented in dumpfile.h.  */

static void
rtl_dump_bb (FILE *outf, basic_block bb, int indent, dump_flags_t flags)
{
  char *s_indent;

  s_indent = (char *) alloca ((size_t) indent + 1);
  memset (s_indent, ' ', (size_t) indent);
  s_indent[indent] = '\0';

  if (df && (flags & TDF_DETAILS))
    {
      df_dump_top (bb, outf);
      putc ('\n', outf);
    }

  if (bb->index != ENTRY_BLOCK && bb->index != EXIT_BLOCK)
    {
      rtx_insn *last = BB_END (bb);
      if (last)
	last = NEXT_INSN (last);
      for (rtx_insn *insn = BB_HEAD (bb); insn != last; insn = NEXT_INSN (insn))
	{
	  if (flags & TDF_DETAILS)
	    df_dump_insn_top (insn, outf);
	  if (! (flags & TDF_SLIM))
	    print_rtl_single (outf, insn);
	  else
	    dump_insn_slim (outf, insn);
	  if (flags & TDF_DETAILS)
	    df_dump_insn_bottom (insn, outf);
	}
    }

  if (df && (flags & TDF_DETAILS))
    {
      df_dump_bottom (bb, outf);
      putc ('\n', outf);
    }

}

/* Like dump_function_to_file, but for RTL.  Print out dataflow information
   for the start of each basic block.  FLAGS are the TDF_* masks documented
   in dumpfile.h.  */

void
print_rtl_with_bb (FILE *outf, const rtx_insn *rtx_first, dump_flags_t flags)
{
  const rtx_insn *tmp_rtx;
  if (rtx_first == 0)
    fprintf (outf, "(nil)\n");
  else
    {
      enum bb_state { NOT_IN_BB, IN_ONE_BB, IN_MULTIPLE_BB };
      int max_uid = get_max_uid ();
      basic_block *start = XCNEWVEC (basic_block, max_uid);
      basic_block *end = XCNEWVEC (basic_block, max_uid);
      enum bb_state *in_bb_p = XCNEWVEC (enum bb_state, max_uid);
      basic_block bb;

      /* After freeing the CFG, we still have BLOCK_FOR_INSN set on most
	 insns, but the CFG is not maintained so the basic block info
	 is not reliable.  Therefore it's omitted from the dumps.  */
      if (! (cfun->curr_properties & PROP_cfg))
        flags &= ~TDF_BLOCKS;

      if (df)
	df_dump_start (outf);

      if (flags & TDF_BLOCKS)
	{
	  FOR_EACH_BB_REVERSE_FN (bb, cfun)
	    {
	      rtx_insn *x;

	      start[INSN_UID (BB_HEAD (bb))] = bb;
	      end[INSN_UID (BB_END (bb))] = bb;
	      for (x = BB_HEAD (bb); x != NULL_RTX; x = NEXT_INSN (x))
		{
		  enum bb_state state = IN_MULTIPLE_BB;

		  if (in_bb_p[INSN_UID (x)] == NOT_IN_BB)
		    state = IN_ONE_BB;
		  in_bb_p[INSN_UID (x)] = state;

		  if (x == BB_END (bb))
		    break;
		}
	    }
	}

      for (tmp_rtx = rtx_first; tmp_rtx != NULL; tmp_rtx = NEXT_INSN (tmp_rtx))
	{
	  if (flags & TDF_BLOCKS)
	    {
	      bb = start[INSN_UID (tmp_rtx)];
	      if (bb != NULL)
		{
		  dump_bb_info (outf, bb, 0, dump_flags, true, false);
		  if (df && (flags & TDF_DETAILS))
		    df_dump_top (bb, outf);
		}

	      if (in_bb_p[INSN_UID (tmp_rtx)] == NOT_IN_BB
		  && !NOTE_P (tmp_rtx)
		  && !BARRIER_P (tmp_rtx))
		fprintf (outf, ";; Insn is not within a basic block\n");
	      else if (in_bb_p[INSN_UID (tmp_rtx)] == IN_MULTIPLE_BB)
		fprintf (outf, ";; Insn is in multiple basic blocks\n");
	    }

	  if (flags & TDF_DETAILS)
	    df_dump_insn_top (tmp_rtx, outf);
	  if (! (flags & TDF_SLIM))
	    print_rtl_single (outf, tmp_rtx);
	  else
	    dump_insn_slim (outf, tmp_rtx);
	  if (flags & TDF_DETAILS)
	    df_dump_insn_bottom (tmp_rtx, outf);

	  if (flags & TDF_BLOCKS)
	    {
	      bb = end[INSN_UID (tmp_rtx)];
	      if (bb != NULL)
		{
		  dump_bb_info (outf, bb, 0, dump_flags, false, true);
		  if (df && (flags & TDF_DETAILS))
		    df_dump_bottom (bb, outf);
		  putc ('\n', outf);
		}
	    }
	}

      free (start);
      free (end);
      free (in_bb_p);
    }
}

/* Update the branch probability of BB if a REG_BR_PROB is present.  */

void
update_br_prob_note (basic_block bb)
{
  rtx note;
  note = find_reg_note (BB_END (bb), REG_BR_PROB, NULL_RTX);
  if (!JUMP_P (BB_END (bb)) || !BRANCH_EDGE (bb)->probability.initialized_p ())
    {
      if (note)
	{
	  rtx *note_link, this_rtx;

	  note_link = &REG_NOTES (BB_END (bb));
	  for (this_rtx = *note_link; this_rtx; this_rtx = XEXP (this_rtx, 1))
	    if (this_rtx == note)
	      {
		*note_link = XEXP (this_rtx, 1);
		break;
	      }
	}
      return;
    }
  if (!note
      || XINT (note, 0) == BRANCH_EDGE (bb)->probability.to_reg_br_prob_note ())
    return;
  XINT (note, 0) = BRANCH_EDGE (bb)->probability.to_reg_br_prob_note ();
}

/* Get the last insn associated with block BB (that includes barriers and
   tablejumps after BB).  */
rtx_insn *
get_last_bb_insn (basic_block bb)
{
  rtx_jump_table_data *table;
  rtx_insn *tmp;
  rtx_insn *end = BB_END (bb);

  /* Include any jump table following the basic block.  */
  if (tablejump_p (end, NULL, &table))
    end = table;

  /* Include any barriers that may follow the basic block.  */
  tmp = next_nonnote_nondebug_insn_bb (end);
  while (tmp && BARRIER_P (tmp))
    {
      end = tmp;
      tmp = next_nonnote_nondebug_insn_bb (end);
    }

  return end;
}

/* Add all BBs reachable from entry via hot paths into the SET.  */

void
find_bbs_reachable_by_hot_paths (hash_set<basic_block> *set)
{
  auto_vec<basic_block, 64> worklist;

  set->add (ENTRY_BLOCK_PTR_FOR_FN (cfun));
  worklist.safe_push (ENTRY_BLOCK_PTR_FOR_FN (cfun));

  while (worklist.length () > 0)
    {
      basic_block bb = worklist.pop ();
      edge_iterator ei;
      edge e;

      FOR_EACH_EDGE (e, ei, bb->succs)
	if (BB_PARTITION (e->dest) != BB_COLD_PARTITION
	    && !set->add (e->dest))
	  worklist.safe_push (e->dest);
    }
}

/* Sanity check partition hotness to ensure that basic blocks in
   the cold partition don't dominate basic blocks in the hot partition.
   If FLAG_ONLY is true, report violations as errors. Otherwise
   re-mark the dominated blocks as cold, since this is run after
   cfg optimizations that may make hot blocks previously reached
   by both hot and cold blocks now only reachable along cold paths.  */

static vec<basic_block>
find_partition_fixes (bool flag_only)
{
  basic_block bb;
  vec<basic_block> bbs_in_cold_partition = vNULL;
  vec<basic_block> bbs_to_fix = vNULL;
  hash_set<basic_block> set;

  /* Callers check this.  */
  gcc_checking_assert (crtl->has_bb_partition);

  find_bbs_reachable_by_hot_paths (&set);

  FOR_EACH_BB_FN (bb, cfun)
    if (!set.contains (bb)
	&& BB_PARTITION (bb) != BB_COLD_PARTITION)
      {
	if (flag_only)
	  error ("non-cold basic block %d reachable only "
		 "by paths crossing the cold partition", bb->index);
	else
	  BB_SET_PARTITION (bb, BB_COLD_PARTITION);
	bbs_to_fix.safe_push (bb);
	bbs_in_cold_partition.safe_push (bb);
      }

  return bbs_to_fix;
}

/* Perform cleanup on the hot/cold bb partitioning after optimization
   passes that modify the cfg.  */

void
fixup_partitions (void)
{
  basic_block bb;

  if (!crtl->has_bb_partition)
    return;

  /* Delete any blocks that became unreachable and weren't
     already cleaned up, for example during edge forwarding
     and convert_jumps_to_returns. This will expose more
     opportunities for fixing the partition boundaries here.
     Also, the calculation of the dominance graph during verification
     will assert if there are unreachable nodes.  */
  delete_unreachable_blocks ();

  /* If there are partitions, do a sanity check on them: A basic block in
     a cold partition cannot dominate a basic block in a hot partition.
     Fixup any that now violate this requirement, as a result of edge
     forwarding and unreachable block deletion.  */
  vec<basic_block> bbs_to_fix = find_partition_fixes (false);

  /* Do the partition fixup after all necessary blocks have been converted to
     cold, so that we only update the region crossings the minimum number of
     places, which can require forcing edges to be non fallthru.  */
  while (! bbs_to_fix.is_empty ())
    {
      bb = bbs_to_fix.pop ();
      fixup_new_cold_bb (bb);
    }
}

/* Verify, in the basic block chain, that there is at most one switch
   between hot/cold partitions. This condition will not be true until
   after reorder_basic_blocks is called.  */

static int
verify_hot_cold_block_grouping (void)
{
  basic_block bb;
  int err = 0;
  bool switched_sections = false;
  int current_partition = BB_UNPARTITIONED;

  /* Even after bb reordering is complete, we go into cfglayout mode
     again (in compgoto). Ensure we don't call this before going back
     into linearized RTL when any layout fixes would have been committed.  */
  if (!crtl->bb_reorder_complete
      || current_ir_type () != IR_RTL_CFGRTL)
    return err;

  FOR_EACH_BB_FN (bb, cfun)
    {
      if (current_partition != BB_UNPARTITIONED
          && BB_PARTITION (bb) != current_partition)
	{
	  if (switched_sections)
	    {
	      error ("multiple hot/cold transitions found (bb %i)",
		     bb->index);
	      err = 1;
	    }
	  else
            switched_sections = true;

          if (!crtl->has_bb_partition)
            error ("partition found but function partition flag not set");
	}
      current_partition = BB_PARTITION (bb);
    }

  return err;
}


/* Perform several checks on the edges out of each block, such as
   the consistency of the branch probabilities, the correctness
   of hot/cold partition crossing edges, and the number of expected
   successor edges.  Also verify that the dominance relationship
   between hot/cold blocks is sane.  */

static int
rtl_verify_edges (void)
{
  int err = 0;
  basic_block bb;

  FOR_EACH_BB_REVERSE_FN (bb, cfun)
    {
      int n_fallthru = 0, n_branch = 0, n_abnormal_call = 0, n_sibcall = 0;
      int n_eh = 0, n_abnormal = 0;
      edge e, fallthru = NULL;
      edge_iterator ei;
      rtx note;
      bool has_crossing_edge = false;

      if (JUMP_P (BB_END (bb))
	  && (note = find_reg_note (BB_END (bb), REG_BR_PROB, NULL_RTX))
	  && EDGE_COUNT (bb->succs) >= 2
	  && any_condjump_p (BB_END (bb)))
	{
	  if (!BRANCH_EDGE (bb)->probability.initialized_p ())
	    {
	      if (profile_status_for_fn (cfun) != PROFILE_ABSENT)
		{
		  error ("verify_flow_info: "
			 "REG_BR_PROB is set but cfg probability is not");
		  err = 1;
		}
	    }
	  else if (XINT (note, 0)
	           != BRANCH_EDGE (bb)->probability.to_reg_br_prob_note ()
	           && profile_status_for_fn (cfun) != PROFILE_ABSENT)
	    {
	      error ("verify_flow_info: REG_BR_PROB does not match cfg %i %i",
		     XINT (note, 0),
		     BRANCH_EDGE (bb)->probability.to_reg_br_prob_note ());
	      err = 1;
	    }
	}

      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  bool is_crossing;

	  if (e->flags & EDGE_FALLTHRU)
	    n_fallthru++, fallthru = e;

	  is_crossing = (BB_PARTITION (e->src) != BB_PARTITION (e->dest)
			 && e->src != ENTRY_BLOCK_PTR_FOR_FN (cfun)
			 && e->dest != EXIT_BLOCK_PTR_FOR_FN (cfun));
          has_crossing_edge |= is_crossing;
	  if (e->flags & EDGE_CROSSING)
	    {
	      if (!is_crossing)
		{
		  error ("EDGE_CROSSING incorrectly set across same section");
		  err = 1;
		}
	      if (e->flags & EDGE_FALLTHRU)
		{
		  error ("fallthru edge crosses section boundary in bb %i",
			 e->src->index);
		  err = 1;
		}
	      if (e->flags & EDGE_EH)
		{
		  error ("EH edge crosses section boundary in bb %i",
			 e->src->index);
		  err = 1;
		}
              if (JUMP_P (BB_END (bb)) && !CROSSING_JUMP_P (BB_END (bb)))
		{
		  error ("No region crossing jump at section boundary in bb %i",
			 bb->index);
		  err = 1;
		}
	    }
	  else if (is_crossing)
	    {
	      error ("EDGE_CROSSING missing across section boundary");
	      err = 1;
	    }

	  if ((e->flags & ~(EDGE_DFS_BACK
			    | EDGE_CAN_FALLTHRU
			    | EDGE_IRREDUCIBLE_LOOP
			    | EDGE_LOOP_EXIT
			    | EDGE_CROSSING
			    | EDGE_PRESERVE)) == 0)
	    n_branch++;

	  if (e->flags & EDGE_ABNORMAL_CALL)
	    n_abnormal_call++;

	  if (e->flags & EDGE_SIBCALL)
	    n_sibcall++;

	  if (e->flags & EDGE_EH)
	    n_eh++;

	  if (e->flags & EDGE_ABNORMAL)
	    n_abnormal++;
	}

      if (!has_crossing_edge
	  && JUMP_P (BB_END (bb))
	  && CROSSING_JUMP_P (BB_END (bb)))
	{
	  print_rtl_with_bb (stderr, get_insns (), TDF_BLOCKS | TDF_DETAILS);
	  error ("Region crossing jump across same section in bb %i",
		 bb->index);
	  err = 1;
	}

      if (n_eh && !find_reg_note (BB_END (bb), REG_EH_REGION, NULL_RTX))
	{
	  error ("missing REG_EH_REGION note at the end of bb %i", bb->index);
	  err = 1;
	}
      if (n_eh > 1)
	{
	  error ("too many exception handling edges in bb %i", bb->index);
	  err = 1;
	}
      if (n_branch
	  && (!JUMP_P (BB_END (bb))
	      || (n_branch > 1 && (any_uncondjump_p (BB_END (bb))
				   || any_condjump_p (BB_END (bb))))))
	{
	  error ("too many outgoing branch edges from bb %i", bb->index);
	  err = 1;
	}
      if (n_fallthru && any_uncondjump_p (BB_END (bb)))
	{
	  error ("fallthru edge after unconditional jump in bb %i", bb->index);
	  err = 1;
	}
      if (n_branch != 1 && any_uncondjump_p (BB_END (bb)))
	{
	  error ("wrong number of branch edges after unconditional jump"
		 " in bb %i", bb->index);
	  err = 1;
	}
      if (n_branch != 1 && any_condjump_p (BB_END (bb))
	  && JUMP_LABEL (BB_END (bb)) != BB_HEAD (fallthru->dest))
	{
	  error ("wrong amount of branch edges after conditional jump"
		 " in bb %i", bb->index);
	  err = 1;
	}
      if (n_abnormal_call && !CALL_P (BB_END (bb)))
	{
	  error ("abnormal call edges for non-call insn in bb %i", bb->index);
	  err = 1;
	}
      if (n_sibcall && !CALL_P (BB_END (bb)))
	{
	  error ("sibcall edges for non-call insn in bb %i", bb->index);
	  err = 1;
	}
      if (n_abnormal > n_eh
	  && !(CALL_P (BB_END (bb))
	       && n_abnormal == n_abnormal_call + n_sibcall)
	  && (!JUMP_P (BB_END (bb))
	      || any_condjump_p (BB_END (bb))
	      || any_uncondjump_p (BB_END (bb))))
	{
	  error ("abnormal edges for no purpose in bb %i", bb->index);
	  err = 1;
	}

      int has_eh = -1;
      FOR_EACH_EDGE (e, ei, bb->preds)
	{
	  if (has_eh == -1)
	    has_eh = (e->flags & EDGE_EH);
	  if ((e->flags & EDGE_EH) == has_eh)
	    continue;
	  error ("EH incoming edge mixed with non-EH incoming edges "
		 "in bb %i", bb->index);
	  err = 1;
	  break;
	}
    }

  /* If there are partitions, do a sanity check on them: A basic block in
     a cold partition cannot dominate a basic block in a hot partition.  */
  if (crtl->has_bb_partition && !err
      && current_ir_type () == IR_RTL_CFGLAYOUT)
    {
      vec<basic_block> bbs_to_fix = find_partition_fixes (true);
      err = !bbs_to_fix.is_empty ();
    }

  /* Clean up.  */
  return err;
}

/* Checks on the instructions within blocks. Currently checks that each
   block starts with a basic block note, and that basic block notes and
   control flow jumps are not found in the middle of the block.  */

static int
rtl_verify_bb_insns (void)
{
  rtx_insn *x;
  int err = 0;
  basic_block bb;

  FOR_EACH_BB_REVERSE_FN (bb, cfun)
    {
      /* Now check the header of basic
	 block.  It ought to contain optional CODE_LABEL followed
	 by NOTE_BASIC_BLOCK.  */
      x = BB_HEAD (bb);
      if (LABEL_P (x))
	{
	  if (BB_END (bb) == x)
	    {
	      error ("NOTE_INSN_BASIC_BLOCK is missing for block %d",
		     bb->index);
	      err = 1;
	    }

	  x = NEXT_INSN (x);
	}

      if (!NOTE_INSN_BASIC_BLOCK_P (x) || NOTE_BASIC_BLOCK (x) != bb)
	{
	  error ("NOTE_INSN_BASIC_BLOCK is missing for block %d",
		 bb->index);
	  err = 1;
	}

      if (BB_END (bb) == x)
	/* Do checks for empty blocks here.  */
	;
      else
	for (x = NEXT_INSN (x); x; x = NEXT_INSN (x))
	  {
	    if (NOTE_INSN_BASIC_BLOCK_P (x))
	      {
		error ("NOTE_INSN_BASIC_BLOCK %d in middle of basic block %d",
		       INSN_UID (x), bb->index);
		err = 1;
	      }

	    if (x == BB_END (bb))
	      break;

	    if (control_flow_insn_p (x))
	      {
		error ("in basic block %d:", bb->index);
		fatal_insn ("flow control insn inside a basic block", x);
	      }
	  }
    }

  /* Clean up.  */
  return err;
}

/* Verify that block pointers for instructions in basic blocks, headers and
   footers are set appropriately.  */

static int
rtl_verify_bb_pointers (void)
{
  int err = 0;
  basic_block bb;

  /* Check the general integrity of the basic blocks.  */
  FOR_EACH_BB_REVERSE_FN (bb, cfun)
    {
      rtx_insn *insn;

      if (!(bb->flags & BB_RTL))
	{
	  error ("BB_RTL flag not set for block %d", bb->index);
	  err = 1;
	}

      FOR_BB_INSNS (bb, insn)
	if (BLOCK_FOR_INSN (insn) != bb)
	  {
	    error ("insn %d basic block pointer is %d, should be %d",
		   INSN_UID (insn),
		   BLOCK_FOR_INSN (insn) ? BLOCK_FOR_INSN (insn)->index : 0,
		   bb->index);
	    err = 1;
	  }

      for (insn = BB_HEADER (bb); insn; insn = NEXT_INSN (insn))
	if (!BARRIER_P (insn)
	    && BLOCK_FOR_INSN (insn) != NULL)
	  {
	    error ("insn %d in header of bb %d has non-NULL basic block",
		   INSN_UID (insn), bb->index);
	    err = 1;
	  }
      for (insn = BB_FOOTER (bb); insn; insn = NEXT_INSN (insn))
	if (!BARRIER_P (insn)
	    && BLOCK_FOR_INSN (insn) != NULL)
	  {
	    error ("insn %d in footer of bb %d has non-NULL basic block",
		   INSN_UID (insn), bb->index);
	    err = 1;
	  }
    }

  /* Clean up.  */
  return err;
}

/* Verify the CFG and RTL consistency common for both underlying RTL and
   cfglayout RTL.

   Currently it does following checks:

   - overlapping of basic blocks
   - insns with wrong BLOCK_FOR_INSN pointers
   - headers of basic blocks (the NOTE_INSN_BASIC_BLOCK note)
   - tails of basic blocks (ensure that boundary is necessary)
   - scans body of the basic block for JUMP_INSN, CODE_LABEL
     and NOTE_INSN_BASIC_BLOCK
   - verify that no fall_thru edge crosses hot/cold partition boundaries
   - verify that there are no pending RTL branch predictions
   - verify that hot blocks are not dominated by cold blocks

   In future it can be extended check a lot of other stuff as well
   (reachability of basic blocks, life information, etc. etc.).  */

static int
rtl_verify_flow_info_1 (void)
{
  int err = 0;

  err |= rtl_verify_bb_pointers ();

  err |= rtl_verify_bb_insns ();

  err |= rtl_verify_edges ();

  return err;
}

/* Walk the instruction chain and verify that bb head/end pointers
  are correct, and that instructions are in exactly one bb and have
  correct block pointers.  */

static int
rtl_verify_bb_insn_chain (void)
{
  basic_block bb;
  int err = 0;
  rtx_insn *x;
  rtx_insn *last_head = get_last_insn ();
  basic_block *bb_info;
  const int max_uid = get_max_uid ();

  bb_info = XCNEWVEC (basic_block, max_uid);

  FOR_EACH_BB_REVERSE_FN (bb, cfun)
    {
      rtx_insn *head = BB_HEAD (bb);
      rtx_insn *end = BB_END (bb);

      for (x = last_head; x != NULL_RTX; x = PREV_INSN (x))
	{
	  /* Verify the end of the basic block is in the INSN chain.  */
	  if (x == end)
	    break;

            /* And that the code outside of basic blocks has NULL bb field.  */
          if (!BARRIER_P (x)
              && BLOCK_FOR_INSN (x) != NULL)
            {
              error ("insn %d outside of basic blocks has non-NULL bb field",
                     INSN_UID (x));
              err = 1;
            }
	}

      if (!x)
	{
	  error ("end insn %d for block %d not found in the insn stream",
		 INSN_UID (end), bb->index);
	  err = 1;
	}

      /* Work backwards from the end to the head of the basic block
	 to verify the head is in the RTL chain.  */
      for (; x != NULL_RTX; x = PREV_INSN (x))
	{
	  /* While walking over the insn chain, verify insns appear
	     in only one basic block.  */
	  if (bb_info[INSN_UID (x)] != NULL)
	    {
	      error ("insn %d is in multiple basic blocks (%d and %d)",
		     INSN_UID (x), bb->index, bb_info[INSN_UID (x)]->index);
	      err = 1;
	    }

	  bb_info[INSN_UID (x)] = bb;

	  if (x == head)
	    break;
	}
      if (!x)
	{
	  error ("head insn %d for block %d not found in the insn stream",
		 INSN_UID (head), bb->index);
	  err = 1;
	}

      last_head = PREV_INSN (x);
    }

  for (x = last_head; x != NULL_RTX; x = PREV_INSN (x))
    {
      /* Check that the code before the first basic block has NULL
	 bb field.  */
      if (!BARRIER_P (x)
	  && BLOCK_FOR_INSN (x) != NULL)
	{
	  error ("insn %d outside of basic blocks has non-NULL bb field",
		 INSN_UID (x));
	  err = 1;
	}
    }
  free (bb_info);

  return err;
}

/* Verify that fallthru edges point to adjacent blocks in layout order and
   that barriers exist after non-fallthru blocks.  */

static int
rtl_verify_fallthru (void)
{
  basic_block bb;
  int err = 0;

  FOR_EACH_BB_REVERSE_FN (bb, cfun)
    {
      edge e;

      e = find_fallthru_edge (bb->succs);
      if (!e)
	{
	  rtx_insn *insn;

	  /* Ensure existence of barrier in BB with no fallthru edges.  */
	  for (insn = NEXT_INSN (BB_END (bb)); ; insn = NEXT_INSN (insn))
	    {
	      if (!insn || NOTE_INSN_BASIC_BLOCK_P (insn))
		{
		  error ("missing barrier after block %i", bb->index);
		  err = 1;
		  break;
		}
	      if (BARRIER_P (insn))
		break;
	    }
	}
      else if (e->src != ENTRY_BLOCK_PTR_FOR_FN (cfun)
	       && e->dest != EXIT_BLOCK_PTR_FOR_FN (cfun))
	{
	  rtx_insn *insn;

	  if (e->src->next_bb != e->dest)
	    {
	      error
		("verify_flow_info: Incorrect blocks for fallthru %i->%i",
		 e->src->index, e->dest->index);
	      err = 1;
	    }
	  else
	    for (insn = NEXT_INSN (BB_END (e->src)); insn != BB_HEAD (e->dest);
		 insn = NEXT_INSN (insn))
	      if (BARRIER_P (insn) || NONDEBUG_INSN_P (insn))
		{
		  error ("verify_flow_info: Incorrect fallthru %i->%i",
			 e->src->index, e->dest->index);
		  fatal_insn ("wrong insn in the fallthru edge", insn);
		  err = 1;
		}
	}
    }

   return err;
}

/* Verify that blocks are laid out in consecutive order. While walking the
   instructions, verify that all expected instructions are inside the basic
   blocks, and that all returns are followed by barriers.  */

static int
rtl_verify_bb_layout (void)
{
  basic_block bb;
  int err = 0;
  rtx_insn *x, *y;
  int num_bb_notes;
  rtx_insn * const rtx_first = get_insns ();
  basic_block last_bb_seen = ENTRY_BLOCK_PTR_FOR_FN (cfun), curr_bb = NULL;

  num_bb_notes = 0;
  last_bb_seen = ENTRY_BLOCK_PTR_FOR_FN (cfun);

  for (x = rtx_first; x; x = NEXT_INSN (x))
    {
      if (NOTE_INSN_BASIC_BLOCK_P (x))
	{
	  bb = NOTE_BASIC_BLOCK (x);

	  num_bb_notes++;
	  if (bb != last_bb_seen->next_bb)
	    internal_error ("basic blocks not laid down consecutively");

	  curr_bb = last_bb_seen = bb;
	}

      if (!curr_bb)
	{
	  switch (GET_CODE (x))
	    {
	    case BARRIER:
	    case NOTE:
	      break;

	    case CODE_LABEL:
	      /* An ADDR_VEC is placed outside any basic block.  */
	      if (NEXT_INSN (x)
		  && JUMP_TABLE_DATA_P (NEXT_INSN (x)))
		x = NEXT_INSN (x);

	      /* But in any case, non-deletable labels can appear anywhere.  */
	      break;

	    default:
	      fatal_insn ("insn outside basic block", x);
	    }
	}

      if (JUMP_P (x)
	  && returnjump_p (x) && ! condjump_p (x)
	  && ! ((y = next_nonnote_nondebug_insn (x))
		&& BARRIER_P (y)))
	    fatal_insn ("return not followed by barrier", x);

      if (curr_bb && x == BB_END (curr_bb))
	curr_bb = NULL;
    }

  if (num_bb_notes != n_basic_blocks_for_fn (cfun) - NUM_FIXED_BLOCKS)
    internal_error
      ("number of bb notes in insn chain (%d) != n_basic_blocks (%d)",
       num_bb_notes, n_basic_blocks_for_fn (cfun));

   return err;
}

/* Verify the CFG and RTL consistency common for both underlying RTL and
   cfglayout RTL, plus consistency checks specific to linearized RTL mode.

   Currently it does following checks:
   - all checks of rtl_verify_flow_info_1
   - test head/end pointers
   - check that blocks are laid out in consecutive order
   - check that all insns are in the basic blocks
     (except the switch handling code, barriers and notes)
   - check that all returns are followed by barriers
   - check that all fallthru edge points to the adjacent blocks
   - verify that there is a single hot/cold partition boundary after bbro  */

static int
rtl_verify_flow_info (void)
{
  int err = 0;

  err |= rtl_verify_flow_info_1 ();

  err |= rtl_verify_bb_insn_chain ();

  err |= rtl_verify_fallthru ();

  err |= rtl_verify_bb_layout ();

  err |= verify_hot_cold_block_grouping ();

  return err;
}

/* Assume that the preceding pass has possibly eliminated jump instructions
   or converted the unconditional jumps.  Eliminate the edges from CFG.
   Return true if any edges are eliminated.  */

bool
purge_dead_edges (basic_block bb)
{
  edge e;
  rtx_insn *insn = BB_END (bb);
  rtx note;
  bool purged = false;
  bool found;
  edge_iterator ei;

  if (DEBUG_INSN_P (insn) && insn != BB_HEAD (bb))
    do
      insn = PREV_INSN (insn);
    while ((DEBUG_INSN_P (insn) || NOTE_P (insn)) && insn != BB_HEAD (bb));

  /* If this instruction cannot trap, remove REG_EH_REGION notes.  */
  if (NONJUMP_INSN_P (insn)
      && (note = find_reg_note (insn, REG_EH_REGION, NULL)))
    {
      rtx eqnote;

      if (! may_trap_p (PATTERN (insn))
	  || ((eqnote = find_reg_equal_equiv_note (insn))
	      && ! may_trap_p (XEXP (eqnote, 0))))
	remove_note (insn, note);
    }

  /* Cleanup abnormal edges caused by exceptions or non-local gotos.  */
  for (ei = ei_start (bb->succs); (e = ei_safe_edge (ei)); )
    {
      bool remove = false;

      /* There are three types of edges we need to handle correctly here: EH
	 edges, abnormal call EH edges, and abnormal call non-EH edges.  The
	 latter can appear when nonlocal gotos are used.  */
      if (e->flags & EDGE_ABNORMAL_CALL)
	{
	  if (!CALL_P (insn))
	    remove = true;
	  else if (can_nonlocal_goto (insn))
	    ;
	  else if ((e->flags & EDGE_EH) && can_throw_internal (insn))
	    ;
	  else if (flag_tm && find_reg_note (insn, REG_TM, NULL))
	    ;
	  else
	    remove = true;
	}
      else if (e->flags & EDGE_EH)
	remove = !can_throw_internal (insn);

      if (remove)
	{
	  remove_edge (e);
	  df_set_bb_dirty (bb);
	  purged = true;
	}
      else
	ei_next (&ei);
    }

  if (JUMP_P (insn))
    {
      rtx note;
      edge b,f;
      edge_iterator ei;

      /* We do care only about conditional jumps and simplejumps.  */
      if (!any_condjump_p (insn)
	  && !returnjump_p (insn)
	  && !simplejump_p (insn))
	return purged;

      /* Branch probability/prediction notes are defined only for
	 condjumps.  We've possibly turned condjump into simplejump.  */
      if (simplejump_p (insn))
	{
	  note = find_reg_note (insn, REG_BR_PROB, NULL);
	  if (note)
	    remove_note (insn, note);
	  while ((note = find_reg_note (insn, REG_BR_PRED, NULL)))
	    remove_note (insn, note);
	}

      for (ei = ei_start (bb->succs); (e = ei_safe_edge (ei)); )
	{
	  /* Avoid abnormal flags to leak from computed jumps turned
	     into simplejumps.  */

	  e->flags &= ~EDGE_ABNORMAL;

	  /* See if this edge is one we should keep.  */
	  if ((e->flags & EDGE_FALLTHRU) && any_condjump_p (insn))
	    /* A conditional jump can fall through into the next
	       block, so we should keep the edge.  */
	    {
	      ei_next (&ei);
	      continue;
	    }
	  else if (e->dest != EXIT_BLOCK_PTR_FOR_FN (cfun)
		   && BB_HEAD (e->dest) == JUMP_LABEL (insn))
	    /* If the destination block is the target of the jump,
	       keep the edge.  */
	    {
	      ei_next (&ei);
	      continue;
	    }
	  else if (e->dest == EXIT_BLOCK_PTR_FOR_FN (cfun)
		   && returnjump_p (insn))
	    /* If the destination block is the exit block, and this
	       instruction is a return, then keep the edge.  */
	    {
	      ei_next (&ei);
	      continue;
	    }
	  else if ((e->flags & EDGE_EH) && can_throw_internal (insn))
	    /* Keep the edges that correspond to exceptions thrown by
	       this instruction and rematerialize the EDGE_ABNORMAL
	       flag we just cleared above.  */
	    {
	      e->flags |= EDGE_ABNORMAL;
	      ei_next (&ei);
	      continue;
	    }

	  /* We do not need this edge.  */
	  df_set_bb_dirty (bb);
	  purged = true;
	  remove_edge (e);
	}

      if (EDGE_COUNT (bb->succs) == 0 || !purged)
	return purged;

      if (dump_file)
	fprintf (dump_file, "Purged edges from bb %i\n", bb->index);

      if (!optimize)
	return purged;

      /* Redistribute probabilities.  */
      if (single_succ_p (bb))
	{
	  single_succ_edge (bb)->probability = profile_probability::always ();
	}
      else
	{
	  note = find_reg_note (insn, REG_BR_PROB, NULL);
	  if (!note)
	    return purged;

	  b = BRANCH_EDGE (bb);
	  f = FALLTHRU_EDGE (bb);
	  b->probability = profile_probability::from_reg_br_prob_note
					 (XINT (note, 0));
	  f->probability = b->probability.invert ();
	}

      return purged;
    }
  else if (CALL_P (insn) && SIBLING_CALL_P (insn))
    {
      /* First, there should not be any EH or ABCALL edges resulting
	 from non-local gotos and the like.  If there were, we shouldn't
	 have created the sibcall in the first place.  Second, there
	 should of course never have been a fallthru edge.  */
      gcc_assert (single_succ_p (bb));
      gcc_assert (single_succ_edge (bb)->flags
		  == (EDGE_SIBCALL | EDGE_ABNORMAL));

      return 0;
    }

  /* If we don't see a jump insn, we don't know exactly why the block would
     have been broken at this point.  Look for a simple, non-fallthru edge,
     as these are only created by conditional branches.  If we find such an
     edge we know that there used to be a jump here and can then safely
     remove all non-fallthru edges.  */
  found = false;
  FOR_EACH_EDGE (e, ei, bb->succs)
    if (! (e->flags & (EDGE_COMPLEX | EDGE_FALLTHRU)))
      {
	found = true;
	break;
      }

  if (!found)
    return purged;

  /* Remove all but the fake and fallthru edges.  The fake edge may be
     the only successor for this block in the case of noreturn
     calls.  */
  for (ei = ei_start (bb->succs); (e = ei_safe_edge (ei)); )
    {
      if (!(e->flags & (EDGE_FALLTHRU | EDGE_FAKE)))
	{
	  df_set_bb_dirty (bb);
	  remove_edge (e);
	  purged = true;
	}
      else
	ei_next (&ei);
    }

  gcc_assert (single_succ_p (bb));

  single_succ_edge (bb)->probability = profile_probability::always ();

  if (dump_file)
    fprintf (dump_file, "Purged non-fallthru edges from bb %i\n",
	     bb->index);
  return purged;
}

/* Search all basic blocks for potentially dead edges and purge them.  Return
   true if some edge has been eliminated.  */

bool
purge_all_dead_edges (void)
{
  int purged = false;
  basic_block bb;

  FOR_EACH_BB_FN (bb, cfun)
    {
      bool purged_here = purge_dead_edges (bb);

      purged |= purged_here;
    }

  return purged;
}

/* This is used by a few passes that emit some instructions after abnormal
   calls, moving the basic block's end, while they in fact do want to emit
   them on the fallthru edge.  Look for abnormal call edges, find backward
   the call in the block and insert the instructions on the edge instead.

   Similarly, handle instructions throwing exceptions internally.

   Return true when instructions have been found and inserted on edges.  */

bool
fixup_abnormal_edges (void)
{
  bool inserted = false;
  basic_block bb;

  FOR_EACH_BB_FN (bb, cfun)
    {
      edge e;
      edge_iterator ei;

      /* Look for cases we are interested in - calls or instructions causing
         exceptions.  */
      FOR_EACH_EDGE (e, ei, bb->succs)
	if ((e->flags & EDGE_ABNORMAL_CALL)
	    || ((e->flags & (EDGE_ABNORMAL | EDGE_EH))
		== (EDGE_ABNORMAL | EDGE_EH)))
	  break;

      if (e && !CALL_P (BB_END (bb)) && !can_throw_internal (BB_END (bb)))
	{
	  rtx_insn *insn;

	  /* Get past the new insns generated.  Allow notes, as the insns
	     may be already deleted.  */
	  insn = BB_END (bb);
	  while ((NONJUMP_INSN_P (insn) || NOTE_P (insn))
		 && !can_throw_internal (insn)
		 && insn != BB_HEAD (bb))
	    insn = PREV_INSN (insn);

	  if (CALL_P (insn) || can_throw_internal (insn))
	    {
	      rtx_insn *stop, *next;

	      e = find_fallthru_edge (bb->succs);

	      stop = NEXT_INSN (BB_END (bb));
	      BB_END (bb) = insn;

	      for (insn = NEXT_INSN (insn); insn != stop; insn = next)
		{
		  next = NEXT_INSN (insn);
		  if (INSN_P (insn))
		    {
		      delete_insn (insn);

		      /* Sometimes there's still the return value USE.
			 If it's placed after a trapping call (i.e. that
			 call is the last insn anyway), we have no fallthru
			 edge.  Simply delete this use and don't try to insert
			 on the non-existent edge.  */
		      if (GET_CODE (PATTERN (insn)) != USE)
			{
			  /* We're not deleting it, we're moving it.  */
			  insn->set_undeleted ();
			  SET_PREV_INSN (insn) = NULL_RTX;
			  SET_NEXT_INSN (insn) = NULL_RTX;

			  insert_insn_on_edge (insn, e);
			  inserted = true;
			}
		    }
		  else if (!BARRIER_P (insn))
		    set_block_for_insn (insn, NULL);
		}
	    }

	  /* It may be that we don't find any trapping insn.  In this
	     case we discovered quite late that the insn that had been
	     marked as can_throw_internal in fact couldn't trap at all.
	     So we should in fact delete the EH edges out of the block.  */
	  else
	    purge_dead_edges (bb);
	}
    }

  return inserted;
}

/* Cut the insns from FIRST to LAST out of the insns stream.  */

rtx_insn *
unlink_insn_chain (rtx_insn *first, rtx_insn *last)
{
  rtx_insn *prevfirst = PREV_INSN (first);
  rtx_insn *nextlast = NEXT_INSN (last);

  SET_PREV_INSN (first) = NULL;
  SET_NEXT_INSN (last) = NULL;
  if (prevfirst)
    SET_NEXT_INSN (prevfirst) = nextlast;
  if (nextlast)
    SET_PREV_INSN (nextlast) = prevfirst;
  else
    set_last_insn (prevfirst);
  if (!prevfirst)
    set_first_insn (nextlast);
  return first;
}

/* Skip over inter-block insns occurring after BB which are typically
   associated with BB (e.g., barriers). If there are any such insns,
   we return the last one. Otherwise, we return the end of BB.  */

static rtx_insn *
skip_insns_after_block (basic_block bb)
{
  rtx_insn *insn, *last_insn, *next_head, *prev;

  next_head = NULL;
  if (bb->next_bb != EXIT_BLOCK_PTR_FOR_FN (cfun))
    next_head = BB_HEAD (bb->next_bb);

  for (last_insn = insn = BB_END (bb); (insn = NEXT_INSN (insn)) != 0; )
    {
      if (insn == next_head)
	break;

      switch (GET_CODE (insn))
	{
	case BARRIER:
	  last_insn = insn;
	  continue;

	case NOTE:
	  switch (NOTE_KIND (insn))
	    {
	    case NOTE_INSN_BLOCK_END:
	      gcc_unreachable ();
	      continue;
	    default:
	      continue;
	      break;
	    }
	  break;

	case CODE_LABEL:
	  if (NEXT_INSN (insn)
	      && JUMP_TABLE_DATA_P (NEXT_INSN (insn)))
	    {
	      insn = NEXT_INSN (insn);
	      last_insn = insn;
	      continue;
	    }
	  break;

	default:
	  break;
	}

      break;
    }

  /* It is possible to hit contradictory sequence.  For instance:

     jump_insn
     NOTE_INSN_BLOCK_BEG
     barrier

     Where barrier belongs to jump_insn, but the note does not.  This can be
     created by removing the basic block originally following
     NOTE_INSN_BLOCK_BEG.  In such case reorder the notes.  */

  for (insn = last_insn; insn != BB_END (bb); insn = prev)
    {
      prev = PREV_INSN (insn);
      if (NOTE_P (insn))
	switch (NOTE_KIND (insn))
	  {
	  case NOTE_INSN_BLOCK_END:
	    gcc_unreachable ();
	    break;
	  case NOTE_INSN_DELETED:
	  case NOTE_INSN_DELETED_LABEL:
	  case NOTE_INSN_DELETED_DEBUG_LABEL:
	    continue;
	  default:
	    reorder_insns (insn, insn, last_insn);
	  }
    }

  return last_insn;
}

/* Locate or create a label for a given basic block.  */

static rtx_insn *
label_for_bb (basic_block bb)
{
  rtx_insn *label = BB_HEAD (bb);

  if (!LABEL_P (label))
    {
      if (dump_file)
	fprintf (dump_file, "Emitting label for block %d\n", bb->index);

      label = block_label (bb);
    }

  return label;
}

/* Locate the effective beginning and end of the insn chain for each
   block, as defined by skip_insns_after_block above.  */

static void
record_effective_endpoints (void)
{
  rtx_insn *next_insn;
  basic_block bb;
  rtx_insn *insn;

  for (insn = get_insns ();
       insn
       && NOTE_P (insn)
       && NOTE_KIND (insn) != NOTE_INSN_BASIC_BLOCK;
       insn = NEXT_INSN (insn))
    continue;
  /* No basic blocks at all?  */
  gcc_assert (insn);

  if (PREV_INSN (insn))
    cfg_layout_function_header =
	    unlink_insn_chain (get_insns (), PREV_INSN (insn));
  else
    cfg_layout_function_header = NULL;

  next_insn = get_insns ();
  FOR_EACH_BB_FN (bb, cfun)
    {
      rtx_insn *end;

      if (PREV_INSN (BB_HEAD (bb)) && next_insn != BB_HEAD (bb))
	BB_HEADER (bb) = unlink_insn_chain (next_insn,
						PREV_INSN (BB_HEAD (bb)));
      end = skip_insns_after_block (bb);
      if (NEXT_INSN (BB_END (bb)) && BB_END (bb) != end)
	BB_FOOTER (bb) = unlink_insn_chain (NEXT_INSN (BB_END (bb)), end);
      next_insn = NEXT_INSN (BB_END (bb));
    }

  cfg_layout_function_footer = next_insn;
  if (cfg_layout_function_footer)
    cfg_layout_function_footer = unlink_insn_chain (cfg_layout_function_footer, get_last_insn ());
}

namespace {

const pass_data pass_data_into_cfg_layout_mode =
{
  RTL_PASS, /* type */
  "into_cfglayout", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_CFG, /* tv_id */
  0, /* properties_required */
  PROP_cfglayout, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_into_cfg_layout_mode : public rtl_opt_pass
{
public:
  pass_into_cfg_layout_mode (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_into_cfg_layout_mode, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *)
    {
      cfg_layout_initialize (0);
      return 0;
    }

}; // class pass_into_cfg_layout_mode

} // anon namespace

rtl_opt_pass *
make_pass_into_cfg_layout_mode (gcc::context *ctxt)
{
  return new pass_into_cfg_layout_mode (ctxt);
}

namespace {

const pass_data pass_data_outof_cfg_layout_mode =
{
  RTL_PASS, /* type */
  "outof_cfglayout", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_CFG, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  PROP_cfglayout, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_outof_cfg_layout_mode : public rtl_opt_pass
{
public:
  pass_outof_cfg_layout_mode (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_outof_cfg_layout_mode, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *);

}; // class pass_outof_cfg_layout_mode

unsigned int
pass_outof_cfg_layout_mode::execute (function *fun)
{
  basic_block bb;

  FOR_EACH_BB_FN (bb, fun)
    if (bb->next_bb != EXIT_BLOCK_PTR_FOR_FN (fun))
      bb->aux = bb->next_bb;

  cfg_layout_finalize ();

  return 0;
}

} // anon namespace

rtl_opt_pass *
make_pass_outof_cfg_layout_mode (gcc::context *ctxt)
{
  return new pass_outof_cfg_layout_mode (ctxt);
}


/* Link the basic blocks in the correct order, compacting the basic
   block queue while at it.  If STAY_IN_CFGLAYOUT_MODE is false, this
   function also clears the basic block header and footer fields.

   This function is usually called after a pass (e.g. tracer) finishes
   some transformations while in cfglayout mode.  The required sequence
   of the basic blocks is in a linked list along the bb->aux field.
   This functions re-links the basic block prev_bb and next_bb pointers
   accordingly, and it compacts and renumbers the blocks.

   FIXME: This currently works only for RTL, but the only RTL-specific
   bits are the STAY_IN_CFGLAYOUT_MODE bits.  The tracer pass was moved
   to GIMPLE a long time ago, but it doesn't relink the basic block
   chain.  It could do that (to give better initial RTL) if this function
   is made IR-agnostic (and moved to cfganal.c or cfg.c while at it).  */

void
relink_block_chain (bool stay_in_cfglayout_mode)
{
  basic_block bb, prev_bb;
  int index;

  /* Maybe dump the re-ordered sequence.  */
  if (dump_file)
    {
      fprintf (dump_file, "Reordered sequence:\n");
      for (bb = ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb, index =
	   NUM_FIXED_BLOCKS;
	   bb;
	   bb = (basic_block) bb->aux, index++)
	{
	  fprintf (dump_file, " %i ", index);
	  if (get_bb_original (bb))
	    fprintf (dump_file, "duplicate of %i ",
		     get_bb_original (bb)->index);
	  else if (forwarder_block_p (bb)
		   && !LABEL_P (BB_HEAD (bb)))
	    fprintf (dump_file, "compensation ");
	  else
	    fprintf (dump_file, "bb %i ", bb->index);
	}
    }

  /* Now reorder the blocks.  */
  prev_bb = ENTRY_BLOCK_PTR_FOR_FN (cfun);
  bb = ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb;
  for (; bb; prev_bb = bb, bb = (basic_block) bb->aux)
    {
      bb->prev_bb = prev_bb;
      prev_bb->next_bb = bb;
    }
  prev_bb->next_bb = EXIT_BLOCK_PTR_FOR_FN (cfun);
  EXIT_BLOCK_PTR_FOR_FN (cfun)->prev_bb = prev_bb;

  /* Then, clean up the aux fields.  */
  FOR_ALL_BB_FN (bb, cfun)
    {
      bb->aux = NULL;
      if (!stay_in_cfglayout_mode)
	BB_HEADER (bb) = BB_FOOTER (bb) = NULL;
    }

  /* Maybe reset the original copy tables, they are not valid anymore
     when we renumber the basic blocks in compact_blocks.  If we are
     are going out of cfglayout mode, don't re-allocate the tables.  */
  if (original_copy_tables_initialized_p ())
    free_original_copy_tables ();
  if (stay_in_cfglayout_mode)
    initialize_original_copy_tables ();

  /* Finally, put basic_block_info in the new order.  */
  compact_blocks ();
}


/* Given a reorder chain, rearrange the code to match.  */

static void
fixup_reorder_chain (void)
{
  basic_block bb;
  rtx_insn *insn = NULL;

  if (cfg_layout_function_header)
    {
      set_first_insn (cfg_layout_function_header);
      insn = cfg_layout_function_header;
      while (NEXT_INSN (insn))
	insn = NEXT_INSN (insn);
    }

  /* First do the bulk reordering -- rechain the blocks without regard to
     the needed changes to jumps and labels.  */

  for (bb = ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb; bb; bb = (basic_block)
       bb->aux)
    {
      if (BB_HEADER (bb))
	{
	  if (insn)
	    SET_NEXT_INSN (insn) = BB_HEADER (bb);
	  else
	    set_first_insn (BB_HEADER (bb));
	  SET_PREV_INSN (BB_HEADER (bb)) = insn;
	  insn = BB_HEADER (bb);
	  while (NEXT_INSN (insn))
	    insn = NEXT_INSN (insn);
	}
      if (insn)
	SET_NEXT_INSN (insn) = BB_HEAD (bb);
      else
	set_first_insn (BB_HEAD (bb));
      SET_PREV_INSN (BB_HEAD (bb)) = insn;
      insn = BB_END (bb);
      if (BB_FOOTER (bb))
	{
	  SET_NEXT_INSN (insn) = BB_FOOTER (bb);
	  SET_PREV_INSN (BB_FOOTER (bb)) = insn;
	  while (NEXT_INSN (insn))
	    insn = NEXT_INSN (insn);
	}
    }

  SET_NEXT_INSN (insn) = cfg_layout_function_footer;
  if (cfg_layout_function_footer)
    SET_PREV_INSN (cfg_layout_function_footer) = insn;

  while (NEXT_INSN (insn))
    insn = NEXT_INSN (insn);

  set_last_insn (insn);
  if (flag_checking)
    verify_insn_chain ();

  /* Now add jumps and labels as needed to match the blocks new
     outgoing edges.  */

  for (bb = ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb; bb ; bb = (basic_block)
       bb->aux)
    {
      edge e_fall, e_taken, e;
      rtx_insn *bb_end_insn;
      rtx ret_label = NULL_RTX;
      basic_block nb;
      edge_iterator ei;

      if (EDGE_COUNT (bb->succs) == 0)
	continue;

      /* Find the old fallthru edge, and another non-EH edge for
	 a taken jump.  */
      e_taken = e_fall = NULL;

      FOR_EACH_EDGE (e, ei, bb->succs)
	if (e->flags & EDGE_FALLTHRU)
	  e_fall = e;
	else if (! (e->flags & EDGE_EH))
	  e_taken = e;

      bb_end_insn = BB_END (bb);
      if (rtx_jump_insn *bb_end_jump = dyn_cast <rtx_jump_insn *> (bb_end_insn))
	{
	  ret_label = JUMP_LABEL (bb_end_jump);
	  if (any_condjump_p (bb_end_jump))
	    {
	      /* This might happen if the conditional jump has side
		 effects and could therefore not be optimized away.
		 Make the basic block to end with a barrier in order
		 to prevent rtl_verify_flow_info from complaining.  */
	      if (!e_fall)
		{
		  gcc_assert (!onlyjump_p (bb_end_jump)
			      || returnjump_p (bb_end_jump)
                              || (e_taken->flags & EDGE_CROSSING));
		  emit_barrier_after (bb_end_jump);
		  continue;
		}

	      /* If the old fallthru is still next, nothing to do.  */
	      if (bb->aux == e_fall->dest
		  || e_fall->dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
		continue;

	      /* The degenerated case of conditional jump jumping to the next
		 instruction can happen for jumps with side effects.  We need
		 to construct a forwarder block and this will be done just
		 fine by force_nonfallthru below.  */
	      if (!e_taken)
		;

	      /* There is another special case: if *neither* block is next,
		 such as happens at the very end of a function, then we'll
		 need to add a new unconditional jump.  Choose the taken
		 edge based on known or assumed probability.  */
	      else if (bb->aux != e_taken->dest)
		{
		  rtx note = find_reg_note (bb_end_jump, REG_BR_PROB, 0);

		  if (note
		      && profile_probability::from_reg_br_prob_note
				 (XINT (note, 0)) < profile_probability::even ()
		      && invert_jump (bb_end_jump,
				      (e_fall->dest
				       == EXIT_BLOCK_PTR_FOR_FN (cfun)
				       ? NULL_RTX
				       : label_for_bb (e_fall->dest)), 0))
		    {
		      e_fall->flags &= ~EDGE_FALLTHRU;
		      gcc_checking_assert (could_fall_through
					   (e_taken->src, e_taken->dest));
		      e_taken->flags |= EDGE_FALLTHRU;
		      update_br_prob_note (bb);
		      e = e_fall, e_fall = e_taken, e_taken = e;
		    }
		}

	      /* If the "jumping" edge is a crossing edge, and the fall
		 through edge is non-crossing, leave things as they are.  */
	      else if ((e_taken->flags & EDGE_CROSSING)
		       && !(e_fall->flags & EDGE_CROSSING))
		continue;

	      /* Otherwise we can try to invert the jump.  This will
		 basically never fail, however, keep up the pretense.  */
	      else if (invert_jump (bb_end_jump,
				    (e_fall->dest
				     == EXIT_BLOCK_PTR_FOR_FN (cfun)
				     ? NULL_RTX
				     : label_for_bb (e_fall->dest)), 0))
		{
		  e_fall->flags &= ~EDGE_FALLTHRU;
		  gcc_checking_assert (could_fall_through
				       (e_taken->src, e_taken->dest));
		  e_taken->flags |= EDGE_FALLTHRU;
		  update_br_prob_note (bb);
		  if (LABEL_NUSES (ret_label) == 0
		      && single_pred_p (e_taken->dest))
		    delete_insn (as_a<rtx_insn *> (ret_label));
		  continue;
		}
	    }
	  else if (extract_asm_operands (PATTERN (bb_end_insn)) != NULL)
	    {
	      /* If the old fallthru is still next or if
		 asm goto doesn't have a fallthru (e.g. when followed by
		 __builtin_unreachable ()), nothing to do.  */
	      if (! e_fall
		  || bb->aux == e_fall->dest
		  || e_fall->dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
		continue;

	      /* Otherwise we'll have to use the fallthru fixup below.  */
	    }
	  else
	    {
	      /* Otherwise we have some return, switch or computed
		 jump.  In the 99% case, there should not have been a
		 fallthru edge.  */
	      gcc_assert (returnjump_p (bb_end_insn) || !e_fall);
	      continue;
	    }
	}
      else
	{
	  /* No fallthru implies a noreturn function with EH edges, or
	     something similarly bizarre.  In any case, we don't need to
	     do anything.  */
	  if (! e_fall)
	    continue;

	  /* If the fallthru block is still next, nothing to do.  */
	  if (bb->aux == e_fall->dest)
	    continue;

	  /* A fallthru to exit block.  */
	  if (e_fall->dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
	    continue;
	}

      /* We got here if we need to add a new jump insn. 
	 Note force_nonfallthru can delete E_FALL and thus we have to
	 save E_FALL->src prior to the call to force_nonfallthru.  */
      nb = force_nonfallthru_and_redirect (e_fall, e_fall->dest, ret_label);
      if (nb)
	{
	  nb->aux = bb->aux;
	  bb->aux = nb;
	  /* Don't process this new block.  */
	  bb = nb;
	}
    }

  relink_block_chain (/*stay_in_cfglayout_mode=*/false);

  /* Annoying special case - jump around dead jumptables left in the code.  */
  FOR_EACH_BB_FN (bb, cfun)
    {
      edge e = find_fallthru_edge (bb->succs);

      if (e && !can_fallthru (e->src, e->dest))
	force_nonfallthru (e);
    }

  /* Ensure goto_locus from edges has some instructions with that locus in RTL
     when not optimizing.  */
  if (!optimize && !DECL_IGNORED_P (current_function_decl))
    FOR_EACH_BB_FN (bb, cfun)
      {
        edge e;
        edge_iterator ei;

        FOR_EACH_EDGE (e, ei, bb->succs)
	  if (LOCATION_LOCUS (e->goto_locus) != UNKNOWN_LOCATION
	      && !(e->flags & EDGE_ABNORMAL))
	    {
	      edge e2;
	      edge_iterator ei2;
	      basic_block dest, nb;
	      rtx_insn *end;

	      insn = BB_END (e->src);
	      end = PREV_INSN (BB_HEAD (e->src));
	      while (insn != end
		     && (!NONDEBUG_INSN_P (insn) || !INSN_HAS_LOCATION (insn)))
		insn = PREV_INSN (insn);
	      if (insn != end
		  && INSN_LOCATION (insn) == e->goto_locus)
		continue;
	      if (simplejump_p (BB_END (e->src))
		  && !INSN_HAS_LOCATION (BB_END (e->src)))
		{
		  INSN_LOCATION (BB_END (e->src)) = e->goto_locus;
		  continue;
		}
	      dest = e->dest;
	      if (dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
		{
		  /* Non-fallthru edges to the exit block cannot be split.  */
		  if (!(e->flags & EDGE_FALLTHRU))
		    continue;
		}
	      else
		{
		  insn = BB_HEAD (dest);
		  end = NEXT_INSN (BB_END (dest));
		  while (insn != end && !NONDEBUG_INSN_P (insn))
		    insn = NEXT_INSN (insn);
		  if (insn != end && INSN_HAS_LOCATION (insn)
		      && INSN_LOCATION (insn) == e->goto_locus)
		    continue;
		}
	      nb = split_edge (e);
	      if (!INSN_P (BB_END (nb)))
		BB_END (nb) = emit_insn_after_noloc (gen_nop (), BB_END (nb),
							 nb);
	      INSN_LOCATION (BB_END (nb)) = e->goto_locus;

	      /* If there are other incoming edges to the destination block
		 with the same goto locus, redirect them to the new block as
		 well, this can prevent other such blocks from being created
		 in subsequent iterations of the loop.  */
	      for (ei2 = ei_start (dest->preds); (e2 = ei_safe_edge (ei2)); )
		if (LOCATION_LOCUS (e2->goto_locus) != UNKNOWN_LOCATION
		    && !(e2->flags & (EDGE_ABNORMAL | EDGE_FALLTHRU))
		    && e->goto_locus == e2->goto_locus)
		  redirect_edge_and_branch (e2, nb);
		else
		  ei_next (&ei2);
	    }
      }
}

/* Perform sanity checks on the insn chain.
   1. Check that next/prev pointers are consistent in both the forward and
      reverse direction.
   2. Count insns in chain, going both directions, and check if equal.
   3. Check that get_last_insn () returns the actual end of chain.  */

DEBUG_FUNCTION void
verify_insn_chain (void)
{
  rtx_insn *x, *prevx, *nextx;
  int insn_cnt1, insn_cnt2;

  for (prevx = NULL, insn_cnt1 = 1, x = get_insns ();
       x != 0;
       prevx = x, insn_cnt1++, x = NEXT_INSN (x))
    gcc_assert (PREV_INSN (x) == prevx);

  gcc_assert (prevx == get_last_insn ());

  for (nextx = NULL, insn_cnt2 = 1, x = get_last_insn ();
       x != 0;
       nextx = x, insn_cnt2++, x = PREV_INSN (x))
    gcc_assert (NEXT_INSN (x) == nextx);

  gcc_assert (insn_cnt1 == insn_cnt2);
}

/* If we have assembler epilogues, the block falling through to exit must
   be the last one in the reordered chain when we reach final.  Ensure
   that this condition is met.  */
static void
fixup_fallthru_exit_predecessor (void)
{
  edge e;
  basic_block bb = NULL;

  /* This transformation is not valid before reload, because we might
     separate a call from the instruction that copies the return
     value.  */
  gcc_assert (reload_completed);

  e = find_fallthru_edge (EXIT_BLOCK_PTR_FOR_FN (cfun)->preds);
  if (e)
    bb = e->src;

  if (bb && bb->aux)
    {
      basic_block c = ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb;

      /* If the very first block is the one with the fall-through exit
	 edge, we have to split that block.  */
      if (c == bb)
	{
	  bb = split_block_after_labels (bb)->dest;
	  bb->aux = c->aux;
	  c->aux = bb;
	  BB_FOOTER (bb) = BB_FOOTER (c);
	  BB_FOOTER (c) = NULL;
	}

      while (c->aux != bb)
	c = (basic_block) c->aux;

      c->aux = bb->aux;
      while (c->aux)
	c = (basic_block) c->aux;

      c->aux = bb;
      bb->aux = NULL;
    }
}

/* In case there are more than one fallthru predecessors of exit, force that
   there is only one.  */

static void
force_one_exit_fallthru (void)
{
  edge e, predecessor = NULL;
  bool more = false;
  edge_iterator ei;
  basic_block forwarder, bb;

  FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR_FOR_FN (cfun)->preds)
    if (e->flags & EDGE_FALLTHRU)
      {
	if (predecessor == NULL)
	  predecessor = e;
	else
	  {
	    more = true;
	    break;
	  }
      }

  if (!more)
    return;

  /* Exit has several fallthru predecessors.  Create a forwarder block for
     them.  */
  forwarder = split_edge (predecessor);
  for (ei = ei_start (EXIT_BLOCK_PTR_FOR_FN (cfun)->preds);
       (e = ei_safe_edge (ei)); )
    {
      if (e->src == forwarder
	  || !(e->flags & EDGE_FALLTHRU))
	ei_next (&ei);
      else
	redirect_edge_and_branch_force (e, forwarder);
    }

  /* Fix up the chain of blocks -- make FORWARDER immediately precede the
     exit block.  */
  FOR_EACH_BB_FN (bb, cfun)
    {
      if (bb->aux == NULL && bb != forwarder)
	{
	  bb->aux = forwarder;
	  break;
	}
    }
}

/* Return true in case it is possible to duplicate the basic block BB.  */

static bool
cfg_layout_can_duplicate_bb_p (const_basic_block bb)
{
  /* Do not attempt to duplicate tablejumps, as we need to unshare
     the dispatch table.  This is difficult to do, as the instructions
     computing jump destination may be hoisted outside the basic block.  */
  if (tablejump_p (BB_END (bb), NULL, NULL))
    return false;

  /* Do not duplicate blocks containing insns that can't be copied.  */
  if (targetm.cannot_copy_insn_p)
    {
      rtx_insn *insn = BB_HEAD (bb);
      while (1)
	{
	  if (INSN_P (insn) && targetm.cannot_copy_insn_p (insn))
	    return false;
	  if (insn == BB_END (bb))
	    break;
	  insn = NEXT_INSN (insn);
	}
    }

  return true;
}

rtx_insn *
duplicate_insn_chain (rtx_insn *from, rtx_insn *to)
{
  rtx_insn *insn, *next, *copy;
  rtx_note *last;

  /* Avoid updating of boundaries of previous basic block.  The
     note will get removed from insn stream in fixup.  */
  last = emit_note (NOTE_INSN_DELETED);

  /* Create copy at the end of INSN chain.  The chain will
     be reordered later.  */
  for (insn = from; insn != NEXT_INSN (to); insn = NEXT_INSN (insn))
    {
      switch (GET_CODE (insn))
	{
	case DEBUG_INSN:
	  /* Don't duplicate label debug insns.  */
	  if (DEBUG_BIND_INSN_P (insn)
	      && TREE_CODE (INSN_VAR_LOCATION_DECL (insn)) == LABEL_DECL)
	    break;
	  /* FALLTHRU */
	case INSN:
	case CALL_INSN:
	case JUMP_INSN:
	  copy = emit_copy_of_insn_after (insn, get_last_insn ());
	  if (JUMP_P (insn) && JUMP_LABEL (insn) != NULL_RTX
	      && ANY_RETURN_P (JUMP_LABEL (insn)))
	    JUMP_LABEL (copy) = JUMP_LABEL (insn);
          maybe_copy_prologue_epilogue_insn (insn, copy);
	  break;

	case JUMP_TABLE_DATA:
	  /* Avoid copying of dispatch tables.  We never duplicate
	     tablejumps, so this can hit only in case the table got
	     moved far from original jump.
	     Avoid copying following barrier as well if any
	     (and debug insns in between).  */
	  for (next = NEXT_INSN (insn);
	       next != NEXT_INSN (to);
	       next = NEXT_INSN (next))
	    if (!DEBUG_INSN_P (next))
	      break;
	  if (next != NEXT_INSN (to) && BARRIER_P (next))
	    insn = next;
	  break;

	case CODE_LABEL:
	  break;

	case BARRIER:
	  emit_barrier ();
	  break;

	case NOTE:
	  switch (NOTE_KIND (insn))
	    {
	      /* In case prologue is empty and function contain label
		 in first BB, we may want to copy the block.  */
	    case NOTE_INSN_PROLOGUE_END:

	    case NOTE_INSN_DELETED:
	    case NOTE_INSN_DELETED_LABEL:
	    case NOTE_INSN_DELETED_DEBUG_LABEL:
	      /* No problem to strip these.  */
	    case NOTE_INSN_FUNCTION_BEG:
	      /* There is always just single entry to function.  */
	    case NOTE_INSN_BASIC_BLOCK:
              /* We should only switch text sections once.  */
	    case NOTE_INSN_SWITCH_TEXT_SECTIONS:
	      break;

	    case NOTE_INSN_EPILOGUE_BEG:
	    case NOTE_INSN_UPDATE_SJLJ_CONTEXT:
	      emit_note_copy (as_a <rtx_note *> (insn));
	      break;

	    default:
	      /* All other notes should have already been eliminated.  */
	      gcc_unreachable ();
	    }
	  break;
	default:
	  gcc_unreachable ();
	}
    }
  insn = NEXT_INSN (last);
  delete_insn (last);
  return insn;
}

/* Create a duplicate of the basic block BB.  */

static basic_block
cfg_layout_duplicate_bb (basic_block bb)
{
  rtx_insn *insn;
  basic_block new_bb;

  insn = duplicate_insn_chain (BB_HEAD (bb), BB_END (bb));
  new_bb = create_basic_block (insn,
			       insn ? get_last_insn () : NULL,
			       EXIT_BLOCK_PTR_FOR_FN (cfun)->prev_bb);

  BB_COPY_PARTITION (new_bb, bb);
  if (BB_HEADER (bb))
    {
      insn = BB_HEADER (bb);
      while (NEXT_INSN (insn))
	insn = NEXT_INSN (insn);
      insn = duplicate_insn_chain (BB_HEADER (bb), insn);
      if (insn)
	BB_HEADER (new_bb) = unlink_insn_chain (insn, get_last_insn ());
    }

  if (BB_FOOTER (bb))
    {
      insn = BB_FOOTER (bb);
      while (NEXT_INSN (insn))
	insn = NEXT_INSN (insn);
      insn = duplicate_insn_chain (BB_FOOTER (bb), insn);
      if (insn)
	BB_FOOTER (new_bb) = unlink_insn_chain (insn, get_last_insn ());
    }

  return new_bb;
}


/* Main entry point to this module - initialize the datastructures for
   CFG layout changes.  It keeps LOOPS up-to-date if not null.

   FLAGS is a set of additional flags to pass to cleanup_cfg().  */

void
cfg_layout_initialize (int flags)
{
  rtx_insn_list *x;
  basic_block bb;

  /* Once bb partitioning is complete, cfg layout mode should not be
     re-entered.  Entering cfg layout mode may require fixups.  As an
     example, if edge forwarding performed when optimizing the cfg
     layout required moving a block from the hot to the cold
     section. This would create an illegal partitioning unless some
     manual fixup was performed.  */
  gcc_assert (!crtl->bb_reorder_complete || !crtl->has_bb_partition);

  initialize_original_copy_tables ();

  cfg_layout_rtl_register_cfg_hooks ();

  record_effective_endpoints ();

  /* Make sure that the targets of non local gotos are marked.  */
  for (x = nonlocal_goto_handler_labels; x; x = x->next ())
    {
      bb = BLOCK_FOR_INSN (x->insn ());
      bb->flags |= BB_NON_LOCAL_GOTO_TARGET;
    }

  cleanup_cfg (CLEANUP_CFGLAYOUT | flags);
}

/* Splits superblocks.  */
void
break_superblocks (void)
{
  bool need = false;
  basic_block bb;

  auto_sbitmap superblocks (last_basic_block_for_fn (cfun));
  bitmap_clear (superblocks);

  FOR_EACH_BB_FN (bb, cfun)
    if (bb->flags & BB_SUPERBLOCK)
      {
	bb->flags &= ~BB_SUPERBLOCK;
	bitmap_set_bit (superblocks, bb->index);
	need = true;
      }

  if (need)
    {
      rebuild_jump_labels (get_insns ());
      find_many_sub_basic_blocks (superblocks);
    }
}

/* Finalize the changes: reorder insn list according to the sequence specified
   by aux pointers, enter compensation code, rebuild scope forest.  */

void
cfg_layout_finalize (void)
{
  free_dominance_info (CDI_DOMINATORS);
  force_one_exit_fallthru ();
  rtl_register_cfg_hooks ();
  if (reload_completed && !targetm.have_epilogue ())
    fixup_fallthru_exit_predecessor ();
  fixup_reorder_chain ();

  rebuild_jump_labels (get_insns ());
  delete_dead_jumptables ();

  if (flag_checking)
    verify_insn_chain ();
  checking_verify_flow_info ();
}


/* Same as split_block but update cfg_layout structures.  */

static basic_block
cfg_layout_split_block (basic_block bb, void *insnp)
{
  rtx insn = (rtx) insnp;
  basic_block new_bb = rtl_split_block (bb, insn);

  BB_FOOTER (new_bb) = BB_FOOTER (bb);
  BB_FOOTER (bb) = NULL;

  return new_bb;
}

/* Redirect Edge to DEST.  */
static edge
cfg_layout_redirect_edge_and_branch (edge e, basic_block dest)
{
  basic_block src = e->src;
  edge ret;

  if (e->flags & (EDGE_ABNORMAL_CALL | EDGE_EH))
    return NULL;

  if (e->dest == dest)
    return e;

  if (e->flags & EDGE_CROSSING
      && BB_PARTITION (e->src) == BB_PARTITION (dest)
      && simplejump_p (BB_END (src)))
    {
      if (dump_file)
	fprintf (dump_file,
	  	 "Removing crossing jump while redirecting edge form %i to %i\n",
		 e->src->index, dest->index);
      delete_insn (BB_END (src));
      e->flags |= EDGE_FALLTHRU;
    }

  if (e->src != ENTRY_BLOCK_PTR_FOR_FN (cfun)
      && (ret = try_redirect_by_replacing_jump (e, dest, true)))
    {
      df_set_bb_dirty (src);
      return ret;
    }

  if (e->src == ENTRY_BLOCK_PTR_FOR_FN (cfun)
      && (e->flags & EDGE_FALLTHRU) && !(e->flags & EDGE_COMPLEX))
    {
      if (dump_file)
	fprintf (dump_file, "Redirecting entry edge from bb %i to %i\n",
		 e->src->index, dest->index);

      df_set_bb_dirty (e->src);
      redirect_edge_succ (e, dest);
      return e;
    }

  /* Redirect_edge_and_branch may decide to turn branch into fallthru edge
     in the case the basic block appears to be in sequence.  Avoid this
     transformation.  */

  if (e->flags & EDGE_FALLTHRU)
    {
      /* Redirect any branch edges unified with the fallthru one.  */
      if (JUMP_P (BB_END (src))
	  && label_is_jump_target_p (BB_HEAD (e->dest),
				     BB_END (src)))
	{
	  edge redirected;

	  if (dump_file)
	    fprintf (dump_file, "Fallthru edge unified with branch "
		     "%i->%i redirected to %i\n",
		     e->src->index, e->dest->index, dest->index);
	  e->flags &= ~EDGE_FALLTHRU;
	  redirected = redirect_branch_edge (e, dest);
	  gcc_assert (redirected);
	  redirected->flags |= EDGE_FALLTHRU;
	  df_set_bb_dirty (redirected->src);
	  return redirected;
	}
      /* In case we are redirecting fallthru edge to the branch edge
	 of conditional jump, remove it.  */
      if (EDGE_COUNT (src->succs) == 2)
	{
	  /* Find the edge that is different from E.  */
	  edge s = EDGE_SUCC (src, EDGE_SUCC (src, 0) == e);

	  if (s->dest == dest
	      && any_condjump_p (BB_END (src))
	      && onlyjump_p (BB_END (src)))
	    delete_insn (BB_END (src));
	}
      if (dump_file)
	fprintf (dump_file, "Redirecting fallthru edge %i->%i to %i\n",
		 e->src->index, e->dest->index, dest->index);
      ret = redirect_edge_succ_nodup (e, dest);
    }
  else
    ret = redirect_branch_edge (e, dest);

  fixup_partition_crossing (ret);
  /* We don't want simplejumps in the insn stream during cfglayout.  */
  gcc_assert (!simplejump_p (BB_END (src)) || CROSSING_JUMP_P (BB_END (src)));

  df_set_bb_dirty (src);
  return ret;
}

/* Simple wrapper as we always can redirect fallthru edges.  */
static basic_block
cfg_layout_redirect_edge_and_branch_force (edge e, basic_block dest)
{
  edge redirected = cfg_layout_redirect_edge_and_branch (e, dest);

  gcc_assert (redirected);
  return NULL;
}

/* Same as delete_basic_block but update cfg_layout structures.  */

static void
cfg_layout_delete_block (basic_block bb)
{
  rtx_insn *insn, *next, *prev = PREV_INSN (BB_HEAD (bb)), *remaints;
  rtx_insn **to;

  if (BB_HEADER (bb))
    {
      next = BB_HEAD (bb);
      if (prev)
	SET_NEXT_INSN (prev) = BB_HEADER (bb);
      else
	set_first_insn (BB_HEADER (bb));
      SET_PREV_INSN (BB_HEADER (bb)) = prev;
      insn = BB_HEADER (bb);
      while (NEXT_INSN (insn))
	insn = NEXT_INSN (insn);
      SET_NEXT_INSN (insn) = next;
      SET_PREV_INSN (next) = insn;
    }
  next = NEXT_INSN (BB_END (bb));
  if (BB_FOOTER (bb))
    {
      insn = BB_FOOTER (bb);
      while (insn)
	{
	  if (BARRIER_P (insn))
	    {
	      if (PREV_INSN (insn))
		SET_NEXT_INSN (PREV_INSN (insn)) = NEXT_INSN (insn);
	      else
		BB_FOOTER (bb) = NEXT_INSN (insn);
	      if (NEXT_INSN (insn))
		SET_PREV_INSN (NEXT_INSN (insn)) = PREV_INSN (insn);
	    }
	  if (LABEL_P (insn))
	    break;
	  insn = NEXT_INSN (insn);
	}
      if (BB_FOOTER (bb))
	{
	  insn = BB_END (bb);
	  SET_NEXT_INSN (insn) = BB_FOOTER (bb);
	  SET_PREV_INSN (BB_FOOTER (bb)) = insn;
	  while (NEXT_INSN (insn))
	    insn = NEXT_INSN (insn);
	  SET_NEXT_INSN (insn) = next;
	  if (next)
	    SET_PREV_INSN (next) = insn;
	  else
	    set_last_insn (insn);
	}
    }
  if (bb->next_bb != EXIT_BLOCK_PTR_FOR_FN (cfun))
    to = &BB_HEADER (bb->next_bb);
  else
    to = &cfg_layout_function_footer;

  rtl_delete_block (bb);

  if (prev)
    prev = NEXT_INSN (prev);
  else
    prev = get_insns ();
  if (next)
    next = PREV_INSN (next);
  else
    next = get_last_insn ();

  if (next && NEXT_INSN (next) != prev)
    {
      remaints = unlink_insn_chain (prev, next);
      insn = remaints;
      while (NEXT_INSN (insn))
	insn = NEXT_INSN (insn);
      SET_NEXT_INSN (insn) = *to;
      if (*to)
	SET_PREV_INSN (*to) = insn;
      *to = remaints;
    }
}

/* Return true when blocks A and B can be safely merged.  */

static bool
cfg_layout_can_merge_blocks_p (basic_block a, basic_block b)
{
  /* If we are partitioning hot/cold basic blocks, we don't want to
     mess up unconditional or indirect jumps that cross between hot
     and cold sections.

     Basic block partitioning may result in some jumps that appear to
     be optimizable (or blocks that appear to be mergeable), but which really
     must be left untouched (they are required to make it safely across
     partition boundaries).  See  the comments at the top of
     bb-reorder.c:partition_hot_cold_basic_blocks for complete details.  */

  if (BB_PARTITION (a) != BB_PARTITION (b))
    return false;

  /* Protect the loop latches.  */
  if (current_loops && b->loop_father->latch == b)
    return false;

  /* If we would end up moving B's instructions, make sure it doesn't fall
     through into the exit block, since we cannot recover from a fallthrough
     edge into the exit block occurring in the middle of a function.  */
  if (NEXT_INSN (BB_END (a)) != BB_HEAD (b))
    {
      edge e = find_fallthru_edge (b->succs);
      if (e && e->dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
	return false;
    }

  /* There must be exactly one edge in between the blocks.  */
  return (single_succ_p (a)
	  && single_succ (a) == b
	  && single_pred_p (b) == 1
	  && a != b
	  /* Must be simple edge.  */
	  && !(single_succ_edge (a)->flags & EDGE_COMPLEX)
	  && a != ENTRY_BLOCK_PTR_FOR_FN (cfun)
	  && b != EXIT_BLOCK_PTR_FOR_FN (cfun)
	  /* If the jump insn has side effects, we can't kill the edge.
	     When not optimizing, try_redirect_by_replacing_jump will
	     not allow us to redirect an edge by replacing a table jump.  */
	  && (!JUMP_P (BB_END (a))
	      || ((!optimize || reload_completed)
		  ? simplejump_p (BB_END (a)) : onlyjump_p (BB_END (a)))));
}

/* Merge block A and B.  The blocks must be mergeable.  */

static void
cfg_layout_merge_blocks (basic_block a, basic_block b)
{
  /* If B is a forwarder block whose outgoing edge has no location, we'll
     propagate the locus of the edge between A and B onto it.  */
  const bool forward_edge_locus
    = (b->flags & BB_FORWARDER_BLOCK) != 0
      && LOCATION_LOCUS (EDGE_SUCC (b, 0)->goto_locus) == UNKNOWN_LOCATION;
  rtx_insn *insn;

  gcc_checking_assert (cfg_layout_can_merge_blocks_p (a, b));

  if (dump_file)
    fprintf (dump_file, "Merging block %d into block %d...\n", b->index,
			 a->index);

  /* If there was a CODE_LABEL beginning B, delete it.  */
  if (LABEL_P (BB_HEAD (b)))
    {
      delete_insn (BB_HEAD (b));
    }

  /* We should have fallthru edge in a, or we can do dummy redirection to get
     it cleaned up.  */
  if (JUMP_P (BB_END (a)))
    try_redirect_by_replacing_jump (EDGE_SUCC (a, 0), b, true);
  gcc_assert (!JUMP_P (BB_END (a)));

  /* If not optimizing, preserve the locus of the single edge between
     blocks A and B if necessary by emitting a nop.  */
  if (!optimize
      && !forward_edge_locus
      && !DECL_IGNORED_P (current_function_decl))
    emit_nop_for_unique_locus_between (a, b);

  /* Move things from b->footer after a->footer.  */
  if (BB_FOOTER (b))
    {
      if (!BB_FOOTER (a))
	BB_FOOTER (a) = BB_FOOTER (b);
      else
	{
	  rtx_insn *last = BB_FOOTER (a);

	  while (NEXT_INSN (last))
	    last = NEXT_INSN (last);
	  SET_NEXT_INSN (last) = BB_FOOTER (b);
	  SET_PREV_INSN (BB_FOOTER (b)) = last;
	}
      BB_FOOTER (b) = NULL;
    }

  /* Move things from b->header before a->footer.
     Note that this may include dead tablejump data, but we don't clean
     those up until we go out of cfglayout mode.  */
   if (BB_HEADER (b))
     {
      if (! BB_FOOTER (a))
	BB_FOOTER (a) = BB_HEADER (b);
      else
	{
	  rtx_insn *last = BB_HEADER (b);
 
	  while (NEXT_INSN (last))
	    last = NEXT_INSN (last);
	  SET_NEXT_INSN (last) = BB_FOOTER (a);
	  SET_PREV_INSN (BB_FOOTER (a)) = last;
	  BB_FOOTER (a) = BB_HEADER (b);
	}
      BB_HEADER (b) = NULL;
    }

  /* In the case basic blocks are not adjacent, move them around.  */
  if (NEXT_INSN (BB_END (a)) != BB_HEAD (b))
    {
      insn = unlink_insn_chain (BB_HEAD (b), BB_END (b));

      emit_insn_after_noloc (insn, BB_END (a), a);
    }
  /* Otherwise just re-associate the instructions.  */
  else
    {
      insn = BB_HEAD (b);
      BB_END (a) = BB_END (b);
    }

  /* emit_insn_after_noloc doesn't call df_insn_change_bb.
     We need to explicitly call. */
  update_bb_for_insn_chain (insn, BB_END (b), a);

  /* Skip possible DELETED_LABEL insn.  */
  if (!NOTE_INSN_BASIC_BLOCK_P (insn))
    insn = NEXT_INSN (insn);
  gcc_assert (NOTE_INSN_BASIC_BLOCK_P (insn));
  BB_HEAD (b) = BB_END (b) = NULL;
  delete_insn (insn);

  df_bb_delete (b->index);

  if (forward_edge_locus)
    EDGE_SUCC (b, 0)->goto_locus = EDGE_SUCC (a, 0)->goto_locus;

  if (dump_file)
    fprintf (dump_file, "Merged blocks %d and %d.\n", a->index, b->index);
}

/* Split edge E.  */

static basic_block
cfg_layout_split_edge (edge e)
{
  basic_block new_bb =
    create_basic_block (e->src != ENTRY_BLOCK_PTR_FOR_FN (cfun)
			? NEXT_INSN (BB_END (e->src)) : get_insns (),
			NULL_RTX, e->src);

  if (e->dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
    BB_COPY_PARTITION (new_bb, e->src);
  else
    BB_COPY_PARTITION (new_bb, e->dest);
  make_edge (new_bb, e->dest, EDGE_FALLTHRU);
  redirect_edge_and_branch_force (e, new_bb);

  return new_bb;
}

/* Do postprocessing after making a forwarder block joined by edge FALLTHRU.  */

static void
rtl_make_forwarder_block (edge fallthru ATTRIBUTE_UNUSED)
{
}

/* Return true if BB contains only labels or non-executable
   instructions.  */

static bool
rtl_block_empty_p (basic_block bb)
{
  rtx_insn *insn;

  if (bb == ENTRY_BLOCK_PTR_FOR_FN (cfun)
      || bb == EXIT_BLOCK_PTR_FOR_FN (cfun))
    return true;

  FOR_BB_INSNS (bb, insn)
    if (NONDEBUG_INSN_P (insn) && !any_uncondjump_p (insn))
      return false;

  return true;
}

/* Split a basic block if it ends with a conditional branch and if
   the other part of the block is not empty.  */

static basic_block
rtl_split_block_before_cond_jump (basic_block bb)
{
  rtx_insn *insn;
  rtx_insn *split_point = NULL;
  rtx_insn *last = NULL;
  bool found_code = false;

  FOR_BB_INSNS (bb, insn)
    {
      if (any_condjump_p (insn))
	split_point = last;
      else if (NONDEBUG_INSN_P (insn))
	found_code = true;
      last = insn;
    }

  /* Did not find everything.  */ 
  if (found_code && split_point)
    return split_block (bb, split_point)->dest;
  else 
    return NULL;
}

/* Return 1 if BB ends with a call, possibly followed by some
   instructions that must stay with the call, 0 otherwise.  */

static bool
rtl_block_ends_with_call_p (basic_block bb)
{
  rtx_insn *insn = BB_END (bb);

  while (!CALL_P (insn)
	 && insn != BB_HEAD (bb)
	 && (keep_with_call_p (insn)
	     || NOTE_P (insn)
	     || DEBUG_INSN_P (insn)))
    insn = PREV_INSN (insn);
  return (CALL_P (insn));
}

/* Return 1 if BB ends with a conditional branch, 0 otherwise.  */

static bool
rtl_block_ends_with_condjump_p (const_basic_block bb)
{
  return any_condjump_p (BB_END (bb));
}

/* Return true if we need to add fake edge to exit.
   Helper function for rtl_flow_call_edges_add.  */

static bool
need_fake_edge_p (const rtx_insn *insn)
{
  if (!INSN_P (insn))
    return false;

  if ((CALL_P (insn)
       && !SIBLING_CALL_P (insn)
       && !find_reg_note (insn, REG_NORETURN, NULL)
       && !(RTL_CONST_OR_PURE_CALL_P (insn))))
    return true;

  return ((GET_CODE (PATTERN (insn)) == ASM_OPERANDS
	   && MEM_VOLATILE_P (PATTERN (insn)))
	  || (GET_CODE (PATTERN (insn)) == PARALLEL
	      && asm_noperands (insn) != -1
	      && MEM_VOLATILE_P (XVECEXP (PATTERN (insn), 0, 0)))
	  || GET_CODE (PATTERN (insn)) == ASM_INPUT);
}

/* Add fake edges to the function exit for any non constant and non noreturn
   calls, volatile inline assembly in the bitmap of blocks specified by
   BLOCKS or to the whole CFG if BLOCKS is zero.  Return the number of blocks
   that were split.

   The goal is to expose cases in which entering a basic block does not imply
   that all subsequent instructions must be executed.  */

static int
rtl_flow_call_edges_add (sbitmap blocks)
{
  int i;
  int blocks_split = 0;
  int last_bb = last_basic_block_for_fn (cfun);
  bool check_last_block = false;

  if (n_basic_blocks_for_fn (cfun) == NUM_FIXED_BLOCKS)
    return 0;

  if (! blocks)
    check_last_block = true;
  else
    check_last_block = bitmap_bit_p (blocks,
				     EXIT_BLOCK_PTR_FOR_FN (cfun)->prev_bb->index);

  /* In the last basic block, before epilogue generation, there will be
     a fallthru edge to EXIT.  Special care is required if the last insn
     of the last basic block is a call because make_edge folds duplicate
     edges, which would result in the fallthru edge also being marked
     fake, which would result in the fallthru edge being removed by
     remove_fake_edges, which would result in an invalid CFG.

     Moreover, we can't elide the outgoing fake edge, since the block
     profiler needs to take this into account in order to solve the minimal
     spanning tree in the case that the call doesn't return.

     Handle this by adding a dummy instruction in a new last basic block.  */
  if (check_last_block)
    {
      basic_block bb = EXIT_BLOCK_PTR_FOR_FN (cfun)->prev_bb;
      rtx_insn *insn = BB_END (bb);

      /* Back up past insns that must be kept in the same block as a call.  */
      while (insn != BB_HEAD (bb)
	     && keep_with_call_p (insn))
	insn = PREV_INSN (insn);

      if (need_fake_edge_p (insn))
	{
	  edge e;

	  e = find_edge (bb, EXIT_BLOCK_PTR_FOR_FN (cfun));
	  if (e)
	    {
	      insert_insn_on_edge (gen_use (const0_rtx), e);
	      commit_edge_insertions ();
	    }
	}
    }

  /* Now add fake edges to the function exit for any non constant
     calls since there is no way that we can determine if they will
     return or not...  */

  for (i = NUM_FIXED_BLOCKS; i < last_bb; i++)
    {
      basic_block bb = BASIC_BLOCK_FOR_FN (cfun, i);
      rtx_insn *insn;
      rtx_insn *prev_insn;

      if (!bb)
	continue;

      if (blocks && !bitmap_bit_p (blocks, i))
	continue;

      for (insn = BB_END (bb); ; insn = prev_insn)
	{
	  prev_insn = PREV_INSN (insn);
	  if (need_fake_edge_p (insn))
	    {
	      edge e;
	      rtx_insn *split_at_insn = insn;

	      /* Don't split the block between a call and an insn that should
		 remain in the same block as the call.  */
	      if (CALL_P (insn))
		while (split_at_insn != BB_END (bb)
		       && keep_with_call_p (NEXT_INSN (split_at_insn)))
		  split_at_insn = NEXT_INSN (split_at_insn);

	      /* The handling above of the final block before the epilogue
		 should be enough to verify that there is no edge to the exit
		 block in CFG already.  Calling make_edge in such case would
		 cause us to mark that edge as fake and remove it later.  */

	      if (flag_checking && split_at_insn == BB_END (bb))
		{
		  e = find_edge (bb, EXIT_BLOCK_PTR_FOR_FN (cfun));
		  gcc_assert (e == NULL);
		}

	      /* Note that the following may create a new basic block
		 and renumber the existing basic blocks.  */
	      if (split_at_insn != BB_END (bb))
		{
		  e = split_block (bb, split_at_insn);
		  if (e)
		    blocks_split++;
		}

	      edge ne = make_edge (bb, EXIT_BLOCK_PTR_FOR_FN (cfun), EDGE_FAKE);
	      ne->probability = profile_probability::guessed_never ();
	    }

	  if (insn == BB_HEAD (bb))
	    break;
	}
    }

  if (blocks_split)
    verify_flow_info ();

  return blocks_split;
}

/* Add COMP_RTX as a condition at end of COND_BB.  FIRST_HEAD is
   the conditional branch target, SECOND_HEAD should be the fall-thru
   there is no need to handle this here the loop versioning code handles
   this.  the reason for SECON_HEAD is that it is needed for condition
   in trees, and this should be of the same type since it is a hook.  */
static void
rtl_lv_add_condition_to_bb (basic_block first_head ,
			    basic_block second_head ATTRIBUTE_UNUSED,
			    basic_block cond_bb, void *comp_rtx)
{
  rtx_code_label *label;
  rtx_insn *seq, *jump;
  rtx op0 = XEXP ((rtx)comp_rtx, 0);
  rtx op1 = XEXP ((rtx)comp_rtx, 1);
  enum rtx_code comp = GET_CODE ((rtx)comp_rtx);
  machine_mode mode;


  label = block_label (first_head);
  mode = GET_MODE (op0);
  if (mode == VOIDmode)
    mode = GET_MODE (op1);

  start_sequence ();
  op0 = force_operand (op0, NULL_RTX);
  op1 = force_operand (op1, NULL_RTX);
  do_compare_rtx_and_jump (op0, op1, comp, 0, mode, NULL_RTX, NULL, label,
			   profile_probability::uninitialized ());
  jump = get_last_insn ();
  JUMP_LABEL (jump) = label;
  LABEL_NUSES (label)++;
  seq = get_insns ();
  end_sequence ();

  /* Add the new cond, in the new head.  */
  emit_insn_after (seq, BB_END (cond_bb));
}


/* Given a block B with unconditional branch at its end, get the
   store the return the branch edge and the fall-thru edge in
   BRANCH_EDGE and FALLTHRU_EDGE respectively.  */
static void
rtl_extract_cond_bb_edges (basic_block b, edge *branch_edge,
			   edge *fallthru_edge)
{
  edge e = EDGE_SUCC (b, 0);

  if (e->flags & EDGE_FALLTHRU)
    {
      *fallthru_edge = e;
      *branch_edge = EDGE_SUCC (b, 1);
    }
  else
    {
      *branch_edge = e;
      *fallthru_edge = EDGE_SUCC (b, 1);
    }
}

void
init_rtl_bb_info (basic_block bb)
{
  gcc_assert (!bb->il.x.rtl);
  bb->il.x.head_ = NULL;
  bb->il.x.rtl = ggc_cleared_alloc<rtl_bb_info> ();
}

/* Returns true if it is possible to remove edge E by redirecting
   it to the destination of the other edge from E->src.  */

static bool
rtl_can_remove_branch_p (const_edge e)
{
  const_basic_block src = e->src;
  const_basic_block target = EDGE_SUCC (src, EDGE_SUCC (src, 0) == e)->dest;
  const rtx_insn *insn = BB_END (src);
  rtx set;

  /* The conditions are taken from try_redirect_by_replacing_jump.  */
  if (target == EXIT_BLOCK_PTR_FOR_FN (cfun))
    return false;

  if (e->flags & (EDGE_ABNORMAL_CALL | EDGE_EH))
    return false;

  if (BB_PARTITION (src) != BB_PARTITION (target))
    return false;

  if (!onlyjump_p (insn)
      || tablejump_p (insn, NULL, NULL))
    return false;

  set = single_set (insn);
  if (!set || side_effects_p (set))
    return false;

  return true;
}

static basic_block
rtl_duplicate_bb (basic_block bb)
{
  bb = cfg_layout_duplicate_bb (bb);
  bb->aux = NULL;
  return bb;
}

/* Do book-keeping of basic block BB for the profile consistency checker.
   If AFTER_PASS is 0, do pre-pass accounting, or if AFTER_PASS is 1
   then do post-pass accounting.  Store the counting in RECORD.  */
static void
rtl_account_profile_record (basic_block bb, int after_pass,
			    struct profile_record *record)
{
  rtx_insn *insn;
  FOR_BB_INSNS (bb, insn)
    if (INSN_P (insn))
      {
	record->size[after_pass] += insn_cost (insn, false);
	if (bb->count.initialized_p ())
	  record->time[after_pass]
	    += insn_cost (insn, true) * bb->count.to_gcov_type ();
	else if (profile_status_for_fn (cfun) == PROFILE_GUESSED)
	  record->time[after_pass]
	    += insn_cost (insn, true) * bb->count.to_frequency (cfun);
      }
}

/* Implementation of CFG manipulation for linearized RTL.  */
struct cfg_hooks rtl_cfg_hooks = {
  "rtl",
  rtl_verify_flow_info,
  rtl_dump_bb,
  rtl_dump_bb_for_graph,
  rtl_create_basic_block,
  rtl_redirect_edge_and_branch,
  rtl_redirect_edge_and_branch_force,
  rtl_can_remove_branch_p,
  rtl_delete_block,
  rtl_split_block,
  rtl_move_block_after,
  rtl_can_merge_blocks,  /* can_merge_blocks_p */
  rtl_merge_blocks,
  rtl_predict_edge,
  rtl_predicted_by_p,
  cfg_layout_can_duplicate_bb_p,
  rtl_duplicate_bb,
  rtl_split_edge,
  rtl_make_forwarder_block,
  rtl_tidy_fallthru_edge,
  rtl_force_nonfallthru,
  rtl_block_ends_with_call_p,
  rtl_block_ends_with_condjump_p,
  rtl_flow_call_edges_add,
  NULL, /* execute_on_growing_pred */
  NULL, /* execute_on_shrinking_pred */
  NULL, /* duplicate loop for trees */
  NULL, /* lv_add_condition_to_bb */
  NULL, /* lv_adjust_loop_header_phi*/
  NULL, /* extract_cond_bb_edges */
  NULL, /* flush_pending_stmts */
  rtl_block_empty_p, /* block_empty_p */
  rtl_split_block_before_cond_jump, /* split_block_before_cond_jump */
  rtl_account_profile_record,
};

/* Implementation of CFG manipulation for cfg layout RTL, where
   basic block connected via fallthru edges does not have to be adjacent.
   This representation will hopefully become the default one in future
   version of the compiler.  */

struct cfg_hooks cfg_layout_rtl_cfg_hooks = {
  "cfglayout mode",
  rtl_verify_flow_info_1,
  rtl_dump_bb,
  rtl_dump_bb_for_graph,
  cfg_layout_create_basic_block,
  cfg_layout_redirect_edge_and_branch,
  cfg_layout_redirect_edge_and_branch_force,
  rtl_can_remove_branch_p,
  cfg_layout_delete_block,
  cfg_layout_split_block,
  rtl_move_block_after,
  cfg_layout_can_merge_blocks_p,
  cfg_layout_merge_blocks,
  rtl_predict_edge,
  rtl_predicted_by_p,
  cfg_layout_can_duplicate_bb_p,
  cfg_layout_duplicate_bb,
  cfg_layout_split_edge,
  rtl_make_forwarder_block,
  NULL, /* tidy_fallthru_edge */
  rtl_force_nonfallthru,
  rtl_block_ends_with_call_p,
  rtl_block_ends_with_condjump_p,
  rtl_flow_call_edges_add,
  NULL, /* execute_on_growing_pred */
  NULL, /* execute_on_shrinking_pred */
  duplicate_loop_to_header_edge, /* duplicate loop for trees */
  rtl_lv_add_condition_to_bb, /* lv_add_condition_to_bb */
  NULL, /* lv_adjust_loop_header_phi*/
  rtl_extract_cond_bb_edges, /* extract_cond_bb_edges */
  NULL, /* flush_pending_stmts */  
  rtl_block_empty_p, /* block_empty_p */
  rtl_split_block_before_cond_jump, /* split_block_before_cond_jump */
  rtl_account_profile_record,
};

#include "gt-cfgrtl.h"
