/* Control flow graph manipulation code for GNU compiler.
   Copyright (C) 1987, 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* This file contains low level functions to manipulate the CFG and analyze it
   that are aware of the RTL intermediate language.

   Available functionality:
     - CFG-aware instruction chain manipulation
	 delete_insn, delete_insn_chain
     - Basic block manipulation
	 create_basic_block, flow_delete_block, split_block,
	 merge_blocks_nomove
     - Infrastructure to determine quickly basic block for insn
	 compute_bb_for_insn, update_bb_for_insn, set_block_for_insn,
     - Edge redirection with updating and optimizing of insn chain
	 block_label, redirect_edge_and_branch,
	 redirect_edge_and_branch_force, tidy_fallthru_edge, force_nonfallthru
     - Edge splitting and commiting to edges
	 split_edge, insert_insn_on_edge, commit_edge_insertions
     - Dumping and debugging
	 print_rtl_with_bb, dump_bb, debug_bb, debug_bb_n
     - Consistency checking
	 verify_flow_info
     - CFG updating after constant propagation
	 purge_dead_edges, purge_all_dead_edges   */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "rtl.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "regs.h"
#include "flags.h"
#include "output.h"
#include "function.h"
#include "except.h"
#include "toplev.h"
#include "tm_p.h"
#include "obstack.h"
#include "insn-config.h"

/* Stubs in case we don't have a return insn.  */
#ifndef HAVE_return
#define HAVE_return 0
#define gen_return() NULL_RTX
#endif

/* The labels mentioned in non-jump rtl.  Valid during find_basic_blocks.  */
/* ??? Should probably be using LABEL_NUSES instead.  It would take a
   bit of surgery to be able to use or co-opt the routines in jump.  */
rtx label_value_list;
rtx tail_recursion_label_list;

static int can_delete_note_p		PARAMS ((rtx));
static int can_delete_label_p		PARAMS ((rtx));
static void commit_one_edge_insertion	PARAMS ((edge, int));
static bool try_redirect_by_replacing_jump PARAMS ((edge, basic_block));
static rtx last_loop_beg_note		PARAMS ((rtx));
static bool back_edge_of_syntactic_loop_p PARAMS ((basic_block, basic_block));
static basic_block force_nonfallthru_and_redirect PARAMS ((edge, basic_block));

/* Return true if NOTE is not one of the ones that must be kept paired,
   so that we may simply delete it.  */

static int
can_delete_note_p (note)
     rtx note;
{
  return (NOTE_LINE_NUMBER (note) == NOTE_INSN_DELETED
	  || NOTE_LINE_NUMBER (note) == NOTE_INSN_BASIC_BLOCK
	  || NOTE_LINE_NUMBER (note) == NOTE_INSN_PREDICTION);
}

/* True if a given label can be deleted.  */

static int
can_delete_label_p (label)
     rtx label;
{
  return (!LABEL_PRESERVE_P (label)
	  /* User declared labels must be preserved.  */
	  && LABEL_NAME (label) == 0
	  && !in_expr_list_p (forced_labels, label)
	  && !in_expr_list_p (label_value_list, label));
}

/* Delete INSN by patching it out.  Return the next insn.  */

rtx
delete_insn (insn)
     rtx insn;
{
  rtx next = NEXT_INSN (insn);
  rtx note;
  bool really_delete = true;

  if (GET_CODE (insn) == CODE_LABEL)
    {
      /* Some labels can't be directly removed from the INSN chain, as they
         might be references via variables, constant pool etc.
         Convert them to the special NOTE_INSN_DELETED_LABEL note.  */
      if (! can_delete_label_p (insn))
	{
	  const char *name = LABEL_NAME (insn);

	  really_delete = false;
	  PUT_CODE (insn, NOTE);
	  NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED_LABEL;
	  NOTE_SOURCE_FILE (insn) = name;
	}

      remove_node_from_expr_list (insn, &nonlocal_goto_handler_labels);
    }

  if (really_delete)
    {
      /* If this insn has already been deleted, something is very wrong.  */
      if (INSN_DELETED_P (insn))
	abort ();
      remove_insn (insn);
      INSN_DELETED_P (insn) = 1;
    }

  /* If deleting a jump, decrement the use count of the label.  Deleting
     the label itself should happen in the normal course of block merging.  */
  if (GET_CODE (insn) == JUMP_INSN
      && JUMP_LABEL (insn)
      && GET_CODE (JUMP_LABEL (insn)) == CODE_LABEL)
    LABEL_NUSES (JUMP_LABEL (insn))--;

  /* Also if deleting an insn that references a label.  */
  else if ((note = find_reg_note (insn, REG_LABEL, NULL_RTX)) != NULL_RTX
	   && GET_CODE (XEXP (note, 0)) == CODE_LABEL)
    LABEL_NUSES (XEXP (note, 0))--;

  if (GET_CODE (insn) == JUMP_INSN
      && (GET_CODE (PATTERN (insn)) == ADDR_VEC
	  || GET_CODE (PATTERN (insn)) == ADDR_DIFF_VEC))
    {
      rtx pat = PATTERN (insn);
      int diff_vec_p = GET_CODE (PATTERN (insn)) == ADDR_DIFF_VEC;
      int len = XVECLEN (pat, diff_vec_p);
      int i;

      for (i = 0; i < len; i++)
	{
	  rtx label = XEXP (XVECEXP (pat, diff_vec_p, i), 0);

	  /* When deleting code in bulk (e.g. removing many unreachable
	     blocks) we can delete a label that's a target of the vector
	     before deleting the vector itself.  */
	  if (GET_CODE (label) != NOTE)
	    LABEL_NUSES (label)--;
	}
    }

  return next;
}

/* Like delete_insn but also purge dead edges from BB.  */
rtx
delete_insn_and_edges (insn)
     rtx insn;
{
  rtx x;
  bool purge = false;

  if (INSN_P (insn)
      && BLOCK_FOR_INSN (insn)
      && BLOCK_FOR_INSN (insn)->end == insn)
    purge = true;
  x = delete_insn (insn);
  if (purge)
    purge_dead_edges (BLOCK_FOR_INSN (insn));
  return x;
}

/* Unlink a chain of insns between START and FINISH, leaving notes
   that must be paired.  */

void
delete_insn_chain (start, finish)
     rtx start, finish;
{
  rtx next;

  /* Unchain the insns one by one.  It would be quicker to delete all of these
     with a single unchaining, rather than one at a time, but we need to keep
     the NOTE's.  */
  while (1)
    {
      next = NEXT_INSN (start);
      if (GET_CODE (start) == NOTE && !can_delete_note_p (start))
	;
      else
	next = delete_insn (start);

      if (start == finish)
	break;
      start = next;
    }
}

/* Like delete_insn but also purge dead edges from BB.  */
void
delete_insn_chain_and_edges (first, last)
     rtx first, last;
{
  bool purge = false;

  if (INSN_P (last)
      && BLOCK_FOR_INSN (last)
      && BLOCK_FOR_INSN (last)->end == last)
    purge = true;
  delete_insn_chain (first, last);
  if (purge)
    purge_dead_edges (BLOCK_FOR_INSN (last));
}

/* Create a new basic block consisting of the instructions between HEAD and END
   inclusive.  This function is designed to allow fast BB construction - reuses
   the note and basic block struct in BB_NOTE, if any and do not grow
   BASIC_BLOCK chain and should be used directly only by CFG construction code.
   END can be NULL in to create new empty basic block before HEAD.  Both END
   and HEAD can be NULL to create basic block at the end of INSN chain.
   AFTER is the basic block we should be put after.  */

basic_block
create_basic_block_structure (head, end, bb_note, after)
     rtx head, end, bb_note;
     basic_block after;
{
  basic_block bb;

  if (bb_note
      && ! RTX_INTEGRATED_P (bb_note)
      && (bb = NOTE_BASIC_BLOCK (bb_note)) != NULL
      && bb->aux == NULL)
    {
      /* If we found an existing note, thread it back onto the chain.  */

      rtx after;

      if (GET_CODE (head) == CODE_LABEL)
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

      if (!head && !end)
	head = end = bb_note
	  = emit_note_after (NOTE_INSN_BASIC_BLOCK, get_last_insn ());
      else if (GET_CODE (head) == CODE_LABEL && end)
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

  bb->head = head;
  bb->end = end;
  bb->index = last_basic_block++;
  bb->flags = BB_NEW;
  link_block (bb, after);
  BASIC_BLOCK (bb->index) = bb;
  update_bb_for_insn (bb);

  /* Tag the block so that we know it has been used when considering
     other basic block notes.  */
  bb->aux = bb;

  return bb;
}

/* Create new basic block consisting of instructions in between HEAD and END
   and place it to the BB chain after block AFTER.  END can be NULL in to
   create new empty basic block before HEAD.  Both END and HEAD can be NULL to
   create basic block at the end of INSN chain.  */

basic_block
create_basic_block (head, end, after)
     rtx head, end;
     basic_block after;
{
  basic_block bb;

  /* Place the new block just after the end.  */
  VARRAY_GROW (basic_block_info, last_basic_block+1);

  n_basic_blocks++;

  bb = create_basic_block_structure (head, end, NULL, after);
  bb->aux = NULL;
  return bb;
}

/* Delete the insns in a (non-live) block.  We physically delete every
   non-deleted-note insn, and update the flow graph appropriately.

   Return nonzero if we deleted an exception handler.  */

/* ??? Preserving all such notes strikes me as wrong.  It would be nice
   to post-process the stream to remove empty blocks, loops, ranges, etc.  */

int
flow_delete_block_noexpunge (b)
     basic_block b;
{
  int deleted_handler = 0;
  rtx insn, end, tmp;

  /* If the head of this block is a CODE_LABEL, then it might be the
     label for an exception handler which can't be reached.

     We need to remove the label from the exception_handler_label list
     and remove the associated NOTE_INSN_EH_REGION_BEG and
     NOTE_INSN_EH_REGION_END notes.  */

  /* Get rid of all NOTE_INSN_PREDICTIONs and NOTE_INSN_LOOP_CONTs
     hanging before the block.  */

  for (insn = PREV_INSN (b->head); insn; insn = PREV_INSN (insn))
    {
      if (GET_CODE (insn) != NOTE)
	break;
      if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_PREDICTION
	  || NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_CONT)
	NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;
    }

  insn = b->head;

  never_reached_warning (insn, b->end);

  if (GET_CODE (insn) == CODE_LABEL)
    maybe_remove_eh_handler (insn);

  /* Include any jump table following the basic block.  */
  end = b->end;
  if (GET_CODE (end) == JUMP_INSN
      && (tmp = JUMP_LABEL (end)) != NULL_RTX
      && (tmp = NEXT_INSN (tmp)) != NULL_RTX
      && GET_CODE (tmp) == JUMP_INSN
      && (GET_CODE (PATTERN (tmp)) == ADDR_VEC
	  || GET_CODE (PATTERN (tmp)) == ADDR_DIFF_VEC))
    end = tmp;

  /* Include any barrier that may follow the basic block.  */
  tmp = next_nonnote_insn (end);
  if (tmp && GET_CODE (tmp) == BARRIER)
    end = tmp;

  /* Selectively delete the entire chain.  */
  b->head = NULL;
  delete_insn_chain (insn, end);

  /* Remove the edges into and out of this block.  Note that there may
     indeed be edges in, if we are removing an unreachable loop.  */
  while (b->pred != NULL)
    remove_edge (b->pred);
  while (b->succ != NULL)
    remove_edge (b->succ);

  b->pred = NULL;
  b->succ = NULL;

  return deleted_handler;
}

int
flow_delete_block (b)
     basic_block b;
{
  int deleted_handler = flow_delete_block_noexpunge (b);

  /* Remove the basic block from the array.  */
  expunge_block (b);

  return deleted_handler;
}

/* Records the basic block struct in BLOCK_FOR_INSN for every insn.  */

void
compute_bb_for_insn ()
{
  basic_block bb;

  FOR_EACH_BB (bb)
    {
      rtx end = bb->end;
      rtx insn;

      for (insn = bb->head; ; insn = NEXT_INSN (insn))
	{
	  BLOCK_FOR_INSN (insn) = bb;
	  if (insn == end)
	    break;
	}
    }
}

/* Release the basic_block_for_insn array.  */

void
free_bb_for_insn ()
{
  rtx insn;
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) != BARRIER)
      BLOCK_FOR_INSN (insn) = NULL;
}

/* Update insns block within BB.  */

void
update_bb_for_insn (bb)
     basic_block bb;
{
  rtx insn;

  for (insn = bb->head; ; insn = NEXT_INSN (insn))
    {
      set_block_for_insn (insn, bb);
      if (insn == bb->end)
	break;
    }
}

/* Split a block BB after insn INSN creating a new fallthru edge.
   Return the new edge.  Note that to keep other parts of the compiler happy,
   this function renumbers all the basic blocks so that the new
   one has a number one greater than the block split.  */

edge
split_block (bb, insn)
     basic_block bb;
     rtx insn;
{
  basic_block new_bb;
  edge new_edge;
  edge e;

  /* There is no point splitting the block after its end.  */
  if (bb->end == insn)
    return 0;

  /* Create the new basic block.  */
  new_bb = create_basic_block (NEXT_INSN (insn), bb->end, bb);
  new_bb->count = bb->count;
  new_bb->frequency = bb->frequency;
  new_bb->loop_depth = bb->loop_depth;
  bb->end = insn;

  /* Redirect the outgoing edges.  */
  new_bb->succ = bb->succ;
  bb->succ = NULL;
  for (e = new_bb->succ; e; e = e->succ_next)
    e->src = new_bb;

  new_edge = make_single_succ_edge (bb, new_bb, EDGE_FALLTHRU);

  if (bb->global_live_at_start)
    {
      new_bb->global_live_at_start = OBSTACK_ALLOC_REG_SET (&flow_obstack);
      new_bb->global_live_at_end = OBSTACK_ALLOC_REG_SET (&flow_obstack);
      COPY_REG_SET (new_bb->global_live_at_end, bb->global_live_at_end);

      /* We now have to calculate which registers are live at the end
	 of the split basic block and at the start of the new basic
	 block.  Start with those registers that are known to be live
	 at the end of the original basic block and get
	 propagate_block to determine which registers are live.  */
      COPY_REG_SET (new_bb->global_live_at_start, bb->global_live_at_end);
      propagate_block (new_bb, new_bb->global_live_at_start, NULL, NULL, 0);
      COPY_REG_SET (bb->global_live_at_end,
		    new_bb->global_live_at_start);
#ifdef HAVE_conditional_execution
      /* In the presence of conditional execution we are not able to update
	 liveness precisely.  */
      if (reload_completed)
	{
	  bb->flags |= BB_DIRTY;
	  new_bb->flags |= BB_DIRTY;
	}
#endif
    }

  return new_edge;
}

/* Blocks A and B are to be merged into a single block A.  The insns
   are already contiguous, hence `nomove'.  */

void
merge_blocks_nomove (a, b)
     basic_block a, b;
{
  rtx b_head = b->head, b_end = b->end, a_end = a->end;
  rtx del_first = NULL_RTX, del_last = NULL_RTX;
  int b_empty = 0;
  edge e;

  /* If there was a CODE_LABEL beginning B, delete it.  */
  if (GET_CODE (b_head) == CODE_LABEL)
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
  if (GET_CODE (a_end) == JUMP_INSN)
    {
      rtx prev;

      for (prev = PREV_INSN (a_end); ; prev = PREV_INSN (prev))
	if (GET_CODE (prev) != NOTE
	    || NOTE_LINE_NUMBER (prev) == NOTE_INSN_BASIC_BLOCK
	    || prev == a->head)
	  break;

      del_first = a_end;

#ifdef HAVE_cc0
      /* If this was a conditional jump, we need to also delete
	 the insn that set cc0.  */
      if (only_sets_cc0_p (prev))
	{
	  rtx tmp = prev;

	  prev = prev_nonnote_insn (prev);
	  if (!prev)
	    prev = a->head;
	  del_first = tmp;
	}
#endif

      a_end = PREV_INSN (del_first);
    }
  else if (GET_CODE (NEXT_INSN (a_end)) == BARRIER)
    del_first = NEXT_INSN (a_end);

  /* Normally there should only be one successor of A and that is B, but
     partway though the merge of blocks for conditional_execution we'll
     be merging a TEST block with THEN and ELSE successors.  Free the
     whole lot of them and hope the caller knows what they're doing.  */
  while (a->succ)
    remove_edge (a->succ);

  /* Adjust the edges out of B for the new owner.  */
  for (e = b->succ; e; e = e->succ_next)
    e->src = a;
  a->succ = b->succ;
  a->flags |= b->flags;

  /* B hasn't quite yet ceased to exist.  Attempt to prevent mishap.  */
  b->pred = b->succ = NULL;
  a->global_live_at_end = b->global_live_at_end;

  expunge_block (b);

  /* Delete everything marked above as well as crap that might be
     hanging out between the two blocks.  */
  delete_insn_chain (del_first, del_last);

  /* Reassociate the insns of B with A.  */
  if (!b_empty)
    {
      rtx x;

      for (x = a_end; x != b_end; x = NEXT_INSN (x))
	set_block_for_insn (x, a);

      set_block_for_insn (b_end, a);

      a_end = b_end;
    }

  a->end = a_end;
}

/* Return the label in the head of basic block BLOCK.  Create one if it doesn't
   exist.  */

rtx
block_label (block)
     basic_block block;
{
  if (block == EXIT_BLOCK_PTR)
    return NULL_RTX;

  if (GET_CODE (block->head) != CODE_LABEL)
    {
      block->head = emit_label_before (gen_label_rtx (), block->head);
    }

  return block->head;
}

/* Attempt to perform edge redirection by replacing possibly complex jump
   instruction by unconditional jump or removing jump completely.  This can
   apply only if all edges now point to the same block.  The parameters and
   return values are equivalent to redirect_edge_and_branch.  */

static bool
try_redirect_by_replacing_jump (e, target)
     edge e;
     basic_block target;
{
  basic_block src = e->src;
  rtx insn = src->end, kill_from;
  edge tmp;
  rtx set, table;
  int fallthru = 0;

  /* Verify that all targets will be TARGET.  */
  for (tmp = src->succ; tmp; tmp = tmp->succ_next)
    if (tmp->dest != target && tmp != e)
      break;

  if (tmp || !onlyjump_p (insn))
    return false;
  if (flow2_completed && JUMP_LABEL (insn)
      && (table = NEXT_INSN (JUMP_LABEL (insn))) != NULL_RTX
      && GET_CODE (table) == JUMP_INSN
      && (GET_CODE (PATTERN (table)) == ADDR_VEC
	  || GET_CODE (PATTERN (table)) == ADDR_DIFF_VEC))
    return false;

  /* Avoid removing branch with side effects.  */
  set = single_set (insn);
  if (!set || side_effects_p (set))
    return false;

  /* In case we zap a conditional jump, we'll need to kill
     the cc0 setter too.  */
  kill_from = insn;
#ifdef HAVE_cc0
  if (reg_mentioned_p (cc0_rtx, PATTERN (insn)))
    kill_from = PREV_INSN (insn);
#endif

  /* See if we can create the fallthru edge.  */
  if (can_fallthru (src, target))
    {
      if (rtl_dump_file)
	fprintf (rtl_dump_file, "Removing jump %i.\n", INSN_UID (insn));
      fallthru = 1;

      /* Selectively unlink whole insn chain.  */
      delete_insn_chain (kill_from, PREV_INSN (target->head));
    }

  /* If this already is simplejump, redirect it.  */
  else if (simplejump_p (insn))
    {
      if (e->dest == target)
	return false;
      if (rtl_dump_file)
	fprintf (rtl_dump_file, "Redirecting jump %i from %i to %i.\n",
		 INSN_UID (insn), e->dest->index, target->index);
      if (!redirect_jump (insn, block_label (target), 0))
	{
	  if (target == EXIT_BLOCK_PTR)
	    return false;
	  abort ();
	}
    }

  /* Cannot do anything for target exit block.  */
  else if (target == EXIT_BLOCK_PTR)
    return false;

  /* Or replace possibly complicated jump insn by simple jump insn.  */
  else
    {
      rtx target_label = block_label (target);
      rtx barrier, tmp;

      emit_jump_insn_after (gen_jump (target_label), insn);
      JUMP_LABEL (src->end) = target_label;
      LABEL_NUSES (target_label)++;
      if (rtl_dump_file)
	fprintf (rtl_dump_file, "Replacing insn %i by jump %i\n",
		 INSN_UID (insn), INSN_UID (src->end));


      delete_insn_chain (kill_from, insn);

      /* Recognize a tablejump that we are converting to a
	 simple jump and remove its associated CODE_LABEL
	 and ADDR_VEC or ADDR_DIFF_VEC.  */
      if ((tmp = JUMP_LABEL (insn)) != NULL_RTX
	  && (tmp = NEXT_INSN (tmp)) != NULL_RTX
	  && GET_CODE (tmp) == JUMP_INSN
	  && (GET_CODE (PATTERN (tmp)) == ADDR_VEC
	      || GET_CODE (PATTERN (tmp)) == ADDR_DIFF_VEC))
	{
	  delete_insn_chain (JUMP_LABEL (insn), tmp);
	}

      barrier = next_nonnote_insn (src->end);
      if (!barrier || GET_CODE (barrier) != BARRIER)
	emit_barrier_after (src->end);
    }

  /* Keep only one edge out and set proper flags.  */
  while (src->succ->succ_next)
    remove_edge (src->succ);
  e = src->succ;
  if (fallthru)
    e->flags = EDGE_FALLTHRU;
  else
    e->flags = 0;

  e->probability = REG_BR_PROB_BASE;
  e->count = src->count;

  /* We don't want a block to end on a line-number note since that has
     the potential of changing the code between -g and not -g.  */
  while (GET_CODE (e->src->end) == NOTE
	 && NOTE_LINE_NUMBER (e->src->end) >= 0)
    delete_insn (e->src->end);

  if (e->dest != target)
    redirect_edge_succ (e, target);

  return true;
}

/* Return last loop_beg note appearing after INSN, before start of next
   basic block.  Return INSN if there are no such notes.

   When emitting jump to redirect a fallthru edge, it should always appear
   after the LOOP_BEG notes, as loop optimizer expect loop to either start by
   fallthru edge or jump following the LOOP_BEG note jumping to the loop exit
   test.  */

static rtx
last_loop_beg_note (insn)
     rtx insn;
{
  rtx last = insn;

  for (insn = NEXT_INSN (insn); insn && GET_CODE (insn) == NOTE
       && NOTE_LINE_NUMBER (insn) != NOTE_INSN_BASIC_BLOCK;
       insn = NEXT_INSN (insn))
    if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_BEG)
      last = insn;

  return last;
}

/* Attempt to change code to redirect edge E to TARGET.  Don't do that on
   expense of adding new instructions or reordering basic blocks.

   Function can be also called with edge destination equivalent to the TARGET.
   Then it should try the simplifications and do nothing if none is possible.

   Return true if transformation succeeded.  We still return false in case E
   already destinated TARGET and we didn't managed to simplify instruction
   stream.  */

bool
redirect_edge_and_branch (e, target)
     edge e;
     basic_block target;
{
  rtx tmp;
  rtx old_label = e->dest->head;
  basic_block src = e->src;
  rtx insn = src->end;

  if (e->flags & (EDGE_ABNORMAL_CALL | EDGE_EH))
    return false;

  if (try_redirect_by_replacing_jump (e, target))
    return true;

  /* Do this fast path late, as we want above code to simplify for cases
     where called on single edge leaving basic block containing nontrivial
     jump insn.  */
  else if (e->dest == target)
    return false;

  /* We can only redirect non-fallthru edges of jump insn.  */
  if (e->flags & EDGE_FALLTHRU)
    return false;
  else if (GET_CODE (insn) != JUMP_INSN)
    return false;

  /* Recognize a tablejump and adjust all matching cases.  */
  if ((tmp = JUMP_LABEL (insn)) != NULL_RTX
      && (tmp = NEXT_INSN (tmp)) != NULL_RTX
      && GET_CODE (tmp) == JUMP_INSN
      && (GET_CODE (PATTERN (tmp)) == ADDR_VEC
	  || GET_CODE (PATTERN (tmp)) == ADDR_DIFF_VEC))
    {
      rtvec vec;
      int j;
      rtx new_label = block_label (target);

      if (target == EXIT_BLOCK_PTR)
	return false;
      if (GET_CODE (PATTERN (tmp)) == ADDR_VEC)
	vec = XVEC (PATTERN (tmp), 0);
      else
	vec = XVEC (PATTERN (tmp), 1);

      for (j = GET_NUM_ELEM (vec) - 1; j >= 0; --j)
	if (XEXP (RTVEC_ELT (vec, j), 0) == old_label)
	  {
	    RTVEC_ELT (vec, j) = gen_rtx_LABEL_REF (Pmode, new_label);
	    --LABEL_NUSES (old_label);
	    ++LABEL_NUSES (new_label);
	  }

      /* Handle casesi dispatch insns */
      if ((tmp = single_set (insn)) != NULL
	  && SET_DEST (tmp) == pc_rtx
	  && GET_CODE (SET_SRC (tmp)) == IF_THEN_ELSE
	  && GET_CODE (XEXP (SET_SRC (tmp), 2)) == LABEL_REF
	  && XEXP (XEXP (SET_SRC (tmp), 2), 0) == old_label)
	{
	  XEXP (SET_SRC (tmp), 2) = gen_rtx_LABEL_REF (VOIDmode,
						       new_label);
	  --LABEL_NUSES (old_label);
	  ++LABEL_NUSES (new_label);
	}
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

      /* If the insn doesn't go where we think, we're confused.  */
      if (JUMP_LABEL (insn) != old_label)
	abort ();

      /* If the substitution doesn't succeed, die.  This can happen
	 if the back end emitted unrecognizable instructions or if
	 target is exit block on some arches.  */
      if (!redirect_jump (insn, block_label (target), 0))
	{
	  if (target == EXIT_BLOCK_PTR)
	    return false;
	  abort ();
	}
    }

  if (rtl_dump_file)
    fprintf (rtl_dump_file, "Edge %i->%i redirected to %i\n",
	     e->src->index, e->dest->index, target->index);

  if (e->dest != target)
    redirect_edge_succ_nodup (e, target);

  return true;
}

/* Like force_nonfallthru below, but additionally performs redirection
   Used by redirect_edge_and_branch_force.  */

static basic_block
force_nonfallthru_and_redirect (e, target)
     edge e;
     basic_block target;
{
  basic_block jump_block, new_bb = NULL, src = e->src;
  rtx note;
  edge new_edge;
  int abnormal_edge_flags = 0;

  /* In the case the last instruction is conditional jump to the next
     instruction, first redirect the jump itself and then continue
     by creating an basic block afterwards to redirect fallthru edge.  */
  if (e->src != ENTRY_BLOCK_PTR && e->dest != EXIT_BLOCK_PTR
      && any_condjump_p (e->src->end)
      /* When called from cfglayout, fallthru edges do not
         neccessarily go to the next block.  */
      && e->src->next_bb == e->dest
      && JUMP_LABEL (e->src->end) == e->dest->head)
    {
      rtx note;
      edge b = unchecked_make_edge (e->src, target, 0);

      if (!redirect_jump (e->src->end, block_label (target), 0))
	abort ();
      note = find_reg_note (e->src->end, REG_BR_PROB, NULL_RTX);
      if (note)
	{
	  int prob = INTVAL (XEXP (note, 0));

	  b->probability = prob;
	  b->count = e->count * prob / REG_BR_PROB_BASE;
	  e->probability -= e->probability;
	  e->count -= b->count;
	  if (e->probability < 0)
	    e->probability = 0;
	  if (e->count < 0)
	    e->count = 0;
	}
    }

  if (e->flags & EDGE_ABNORMAL)
    {
      /* Irritating special case - fallthru edge to the same block as abnormal
	 edge.
	 We can't redirect abnormal edge, but we still can split the fallthru
	 one and create separate abnormal edge to original destination. 
	 This allows bb-reorder to make such edge non-fallthru.  */
      if (e->dest != target)
	abort ();
      abnormal_edge_flags = e->flags & ~(EDGE_FALLTHRU | EDGE_CAN_FALLTHRU);
      e->flags &= EDGE_FALLTHRU | EDGE_CAN_FALLTHRU;
    }
  else if (!(e->flags & EDGE_FALLTHRU))
    abort ();
  else if (e->src == ENTRY_BLOCK_PTR)
    {
      /* We can't redirect the entry block.  Create an empty block at the
         start of the function which we use to add the new jump.  */
      edge *pe1;
      basic_block bb = create_basic_block (e->dest->head, NULL, ENTRY_BLOCK_PTR);

      /* Change the existing edge's source to be the new block, and add
	 a new edge from the entry block to the new block.  */
      e->src = bb;
      for (pe1 = &ENTRY_BLOCK_PTR->succ; *pe1; pe1 = &(*pe1)->succ_next)
	if (*pe1 == e)
	  {
	    *pe1 = e->succ_next;
	    break;
	  }
      e->succ_next = 0;
      bb->succ = e;
      make_single_succ_edge (ENTRY_BLOCK_PTR, bb, EDGE_FALLTHRU);
    }

  if (e->src->succ->succ_next || abnormal_edge_flags)
    {
      /* Create the new structures.  */

      /* Position the new block correctly relative to loop notes.  */
      note = last_loop_beg_note (e->src->end);
      note = NEXT_INSN (note);

      /* ... and ADDR_VECs.  */
      if (note != NULL
	  && GET_CODE (note) == CODE_LABEL
	  && NEXT_INSN (note)
	  && GET_CODE (NEXT_INSN (note)) == JUMP_INSN
	  && (GET_CODE (PATTERN (NEXT_INSN (note))) == ADDR_DIFF_VEC
	      || GET_CODE (PATTERN (NEXT_INSN (note))) == ADDR_VEC))
	note = NEXT_INSN (NEXT_INSN (note));

      jump_block = create_basic_block (note, NULL, e->src);
      jump_block->count = e->count;
      jump_block->frequency = EDGE_FREQUENCY (e);
      jump_block->loop_depth = target->loop_depth;

      if (target->global_live_at_start)
	{
	  jump_block->global_live_at_start
	    = OBSTACK_ALLOC_REG_SET (&flow_obstack);
	  jump_block->global_live_at_end
	    = OBSTACK_ALLOC_REG_SET (&flow_obstack);
	  COPY_REG_SET (jump_block->global_live_at_start,
			target->global_live_at_start);
	  COPY_REG_SET (jump_block->global_live_at_end,
			target->global_live_at_start);
	}

      /* Wire edge in.  */
      new_edge = make_edge (e->src, jump_block, EDGE_FALLTHRU);
      new_edge->probability = e->probability;
      new_edge->count = e->count;

      /* Redirect old edge.  */
      redirect_edge_pred (e, jump_block);
      e->probability = REG_BR_PROB_BASE;

      new_bb = jump_block;
    }
  else
    jump_block = e->src;

  e->flags &= ~EDGE_FALLTHRU;
  if (target == EXIT_BLOCK_PTR)
    {
      if (HAVE_return)
	emit_jump_insn_after (gen_return (), jump_block->end);
      else
	abort ();
    }
  else
    {
      rtx label = block_label (target);
      emit_jump_insn_after (gen_jump (label), jump_block->end);
      JUMP_LABEL (jump_block->end) = label;
      LABEL_NUSES (label)++;
    }

  emit_barrier_after (jump_block->end);
  redirect_edge_succ_nodup (e, target);

  if (abnormal_edge_flags)
    make_edge (src, target, abnormal_edge_flags);

  return new_bb;
}

/* Edge E is assumed to be fallthru edge.  Emit needed jump instruction
   (and possibly create new basic block) to make edge non-fallthru.
   Return newly created BB or NULL if none.  */

basic_block
force_nonfallthru (e)
     edge e;
{
  return force_nonfallthru_and_redirect (e, e->dest);
}

/* Redirect edge even at the expense of creating new jump insn or
   basic block.  Return new basic block if created, NULL otherwise.
   Abort if conversion is impossible.  */

basic_block
redirect_edge_and_branch_force (e, target)
     edge e;
     basic_block target;
{
  if (redirect_edge_and_branch (e, target)
      || e->dest == target)
    return NULL;

  /* In case the edge redirection failed, try to force it to be non-fallthru
     and redirect newly created simplejump.  */
  return force_nonfallthru_and_redirect (e, target);
}

/* The given edge should potentially be a fallthru edge.  If that is in
   fact true, delete the jump and barriers that are in the way.  */

void
tidy_fallthru_edge (e, b, c)
     edge e;
     basic_block b, c;
{
  rtx q;

  /* ??? In a late-running flow pass, other folks may have deleted basic
     blocks by nopping out blocks, leaving multiple BARRIERs between here
     and the target label. They ought to be chastized and fixed.

     We can also wind up with a sequence of undeletable labels between
     one block and the next.

     So search through a sequence of barriers, labels, and notes for
     the head of block C and assert that we really do fall through.  */

  for (q = NEXT_INSN (b->end); q != c->head; q = NEXT_INSN (q))
    if (INSN_P (q))
      return;

  /* Remove what will soon cease being the jump insn from the source block.
     If block B consisted only of this single jump, turn it into a deleted
     note.  */
  q = b->end;
  if (GET_CODE (q) == JUMP_INSN
      && onlyjump_p (q)
      && (any_uncondjump_p (q)
	  || (b->succ == e && e->succ_next == NULL)))
    {
#ifdef HAVE_cc0
      /* If this was a conditional jump, we need to also delete
	 the insn that set cc0.  */
      if (any_condjump_p (q) && only_sets_cc0_p (PREV_INSN (q)))
	q = PREV_INSN (q);
#endif

      q = PREV_INSN (q);

      /* We don't want a block to end on a line-number note since that has
	 the potential of changing the code between -g and not -g.  */
      while (GET_CODE (q) == NOTE && NOTE_LINE_NUMBER (q) >= 0)
	q = PREV_INSN (q);
    }

  /* Selectively unlink the sequence.  */
  if (q != PREV_INSN (c->head))
    delete_insn_chain (NEXT_INSN (q), PREV_INSN (c->head));

  e->flags |= EDGE_FALLTHRU;
}

/* Fix up edges that now fall through, or rather should now fall through
   but previously required a jump around now deleted blocks.  Simplify
   the search by only examining blocks numerically adjacent, since this
   is how find_basic_blocks created them.  */

void
tidy_fallthru_edges ()
{
  basic_block b, c;

  if (ENTRY_BLOCK_PTR->next_bb == EXIT_BLOCK_PTR)
    return;

  FOR_BB_BETWEEN (b, ENTRY_BLOCK_PTR->next_bb, EXIT_BLOCK_PTR->prev_bb, next_bb)
    {
      edge s;

      c = b->next_bb;

      /* We care about simple conditional or unconditional jumps with
	 a single successor.

	 If we had a conditional branch to the next instruction when
	 find_basic_blocks was called, then there will only be one
	 out edge for the block which ended with the conditional
	 branch (since we do not create duplicate edges).

	 Furthermore, the edge will be marked as a fallthru because we
	 merge the flags for the duplicate edges.  So we do not want to
	 check that the edge is not a FALLTHRU edge.  */

      if ((s = b->succ) != NULL
	  && ! (s->flags & EDGE_COMPLEX)
	  && s->succ_next == NULL
	  && s->dest == c
	  /* If the jump insn has side effects, we can't tidy the edge.  */
	  && (GET_CODE (b->end) != JUMP_INSN
	      || onlyjump_p (b->end)))
	tidy_fallthru_edge (s, b, c);
    }
}

/* Helper function for split_edge.  Return true in case edge BB2 to BB1
   is back edge of syntactic loop.  */

static bool
back_edge_of_syntactic_loop_p (bb1, bb2)
	basic_block bb1, bb2;
{
  rtx insn;
  int count = 0;
  basic_block bb;

  if (bb1 == bb2)
    return true;

  /* ??? Could we guarantee that bb indices are monotone, so that we could
     just compare them?  */
  for (bb = bb1; bb && bb != bb2; bb = bb->next_bb)
    continue;

  if (!bb)
    return false;

  for (insn = bb1->end; insn != bb2->head && count >= 0;
       insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == NOTE)
      {
	if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_BEG)
	  count++;
	else if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_END)
	  count--;
      }

  return count >= 0;
}

/* Split a (typically critical) edge.  Return the new block.
   Abort on abnormal edges.

   ??? The code generally expects to be called on critical edges.
   The case of a block ending in an unconditional jump to a
   block with multiple predecessors is not handled optimally.  */

basic_block
split_edge (edge_in)
     edge edge_in;
{
  basic_block bb;
  edge edge_out;
  rtx before;

  /* Abnormal edges cannot be split.  */
  if ((edge_in->flags & EDGE_ABNORMAL) != 0)
    abort ();

  /* We are going to place the new block in front of edge destination.
     Avoid existence of fallthru predecessors.  */
  if ((edge_in->flags & EDGE_FALLTHRU) == 0)
    {
      edge e;

      for (e = edge_in->dest->pred; e; e = e->pred_next)
	if (e->flags & EDGE_FALLTHRU)
	  break;

      if (e)
	force_nonfallthru (e);
    }

  /* Create the basic block note.

     Where we place the note can have a noticeable impact on the generated
     code.  Consider this cfg:

		        E
			|
			0
		       / \
		   +->1-->2--->E
                   |  |
		   +--+

      If we need to insert an insn on the edge from block 0 to block 1,
      we want to ensure the instructions we insert are outside of any
      loop notes that physically sit between block 0 and block 1.  Otherwise
      we confuse the loop optimizer into thinking the loop is a phony.  */

  if (edge_in->dest != EXIT_BLOCK_PTR
      && PREV_INSN (edge_in->dest->head)
      && GET_CODE (PREV_INSN (edge_in->dest->head)) == NOTE
      && (NOTE_LINE_NUMBER (PREV_INSN (edge_in->dest->head))
	  == NOTE_INSN_LOOP_BEG)
      && !back_edge_of_syntactic_loop_p (edge_in->dest, edge_in->src))
    before = PREV_INSN (edge_in->dest->head);
  else if (edge_in->dest != EXIT_BLOCK_PTR)
    before = edge_in->dest->head;
  else
    before = NULL_RTX;

  bb = create_basic_block (before, NULL, edge_in->dest->prev_bb);
  bb->count = edge_in->count;
  bb->frequency = EDGE_FREQUENCY (edge_in);

  /* ??? This info is likely going to be out of date very soon.  */
  if (edge_in->dest->global_live_at_start)
    {
      bb->global_live_at_start = OBSTACK_ALLOC_REG_SET (&flow_obstack);
      bb->global_live_at_end = OBSTACK_ALLOC_REG_SET (&flow_obstack);
      COPY_REG_SET (bb->global_live_at_start,
		    edge_in->dest->global_live_at_start);
      COPY_REG_SET (bb->global_live_at_end,
		    edge_in->dest->global_live_at_start);
    }

  edge_out = make_single_succ_edge (bb, edge_in->dest, EDGE_FALLTHRU);

  /* For non-fallthry edges, we must adjust the predecessor's
     jump instruction to target our new block.  */
  if ((edge_in->flags & EDGE_FALLTHRU) == 0)
    {
      if (!redirect_edge_and_branch (edge_in, bb))
	abort ();
    }
  else
    redirect_edge_succ (edge_in, bb);

  return bb;
}

/* Queue instructions for insertion on an edge between two basic blocks.
   The new instructions and basic blocks (if any) will not appear in the
   CFG until commit_edge_insertions is called.  */

void
insert_insn_on_edge (pattern, e)
     rtx pattern;
     edge e;
{
  /* We cannot insert instructions on an abnormal critical edge.
     It will be easier to find the culprit if we die now.  */
  if ((e->flags & EDGE_ABNORMAL) && EDGE_CRITICAL_P (e))
    abort ();

  if (e->insns == NULL_RTX)
    start_sequence ();
  else
    push_to_sequence (e->insns);

  emit_insn (pattern);

  e->insns = get_insns ();
  end_sequence ();
}

/* Update the CFG for the instructions queued on edge E.  */

static void
commit_one_edge_insertion (e, watch_calls)
     edge e;
     int watch_calls;
{
  rtx before = NULL_RTX, after = NULL_RTX, insns, tmp, last;
  basic_block bb = NULL;

  /* Pull the insns off the edge now since the edge might go away.  */
  insns = e->insns;
  e->insns = NULL_RTX;

  /* Special case -- avoid inserting code between call and storing
     its return value.  */
  if (watch_calls && (e->flags & EDGE_FALLTHRU) && !e->dest->pred->pred_next
      && e->src != ENTRY_BLOCK_PTR
      && GET_CODE (e->src->end) == CALL_INSN)
    {
      rtx next = next_nonnote_insn (e->src->end);

      after = e->dest->head;
      /* The first insn after the call may be a stack pop, skip it.  */
      while (next
	     && keep_with_call_p (next))
	{
	  after = next;
	  next = next_nonnote_insn (next);
	}
      bb = e->dest;
    }
  if (!before && !after)
    {
      /* Figure out where to put these things.  If the destination has
         one predecessor, insert there.  Except for the exit block.  */
      if (e->dest->pred->pred_next == NULL && e->dest != EXIT_BLOCK_PTR)
	{
	  bb = e->dest;

	  /* Get the location correct wrt a code label, and "nice" wrt
	     a basic block note, and before everything else.  */
	  tmp = bb->head;
	  if (GET_CODE (tmp) == CODE_LABEL)
	    tmp = NEXT_INSN (tmp);
	  if (NOTE_INSN_BASIC_BLOCK_P (tmp))
	    tmp = NEXT_INSN (tmp);
	  if (tmp == bb->head)
	    before = tmp;
	  else if (tmp)
	    after = PREV_INSN (tmp);
	  else
	    after = get_last_insn ();
	}

      /* If the source has one successor and the edge is not abnormal,
         insert there.  Except for the entry block.  */
      else if ((e->flags & EDGE_ABNORMAL) == 0
	       && e->src->succ->succ_next == NULL
	       && e->src != ENTRY_BLOCK_PTR)
	{
	  bb = e->src;

	  /* It is possible to have a non-simple jump here.  Consider a target
	     where some forms of unconditional jumps clobber a register.  This
	     happens on the fr30 for example.

	     We know this block has a single successor, so we can just emit
	     the queued insns before the jump.  */
	  if (GET_CODE (bb->end) == JUMP_INSN)
	    for (before = bb->end;
		 GET_CODE (PREV_INSN (before)) == NOTE
		 && NOTE_LINE_NUMBER (PREV_INSN (before)) ==
		 NOTE_INSN_LOOP_BEG; before = PREV_INSN (before))
	      ;
	  else
	    {
	      /* We'd better be fallthru, or we've lost track of what's what.  */
	      if ((e->flags & EDGE_FALLTHRU) == 0)
		abort ();

	      after = bb->end;
	    }
	}
      /* Otherwise we must split the edge.  */
      else
	{
	  bb = split_edge (e);
	  after = bb->end;
	}
    }

  /* Now that we've found the spot, do the insertion.  */

  if (before)
    {
      emit_insn_before (insns, before);
      last = prev_nonnote_insn (before);
    }
  else
    last = emit_insn_after (insns, after);

  if (returnjump_p (last))
    {
      /* ??? Remove all outgoing edges from BB and add one for EXIT.
         This is not currently a problem because this only happens
         for the (single) epilogue, which already has a fallthru edge
         to EXIT.  */

      e = bb->succ;
      if (e->dest != EXIT_BLOCK_PTR
	  || e->succ_next != NULL || (e->flags & EDGE_FALLTHRU) == 0)
	abort ();

      e->flags &= ~EDGE_FALLTHRU;
      emit_barrier_after (last);

      if (before)
	delete_insn (before);
    }
  else if (GET_CODE (last) == JUMP_INSN)
    abort ();

  /* Mark the basic block for find_sub_basic_blocks.  */
  bb->aux = &bb->aux;
}

/* Update the CFG for all queued instructions.  */

void
commit_edge_insertions ()
{
  basic_block bb;
  sbitmap blocks;
  bool changed = false;

#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, EXIT_BLOCK_PTR, next_bb)
    {
      edge e, next;

      for (e = bb->succ; e; e = next)
	{
	  next = e->succ_next;
	  if (e->insns)
	    {
	      changed = true;
	      commit_one_edge_insertion (e, false);
	    }
	}
    }

  if (!changed)
    return;

  blocks = sbitmap_alloc (last_basic_block);
  sbitmap_zero (blocks);
  FOR_EACH_BB (bb)
    if (bb->aux)
      {
        SET_BIT (blocks, bb->index);
	bb->aux = NULL;
      }
  find_many_sub_basic_blocks (blocks);
  sbitmap_free (blocks);
}

/* Update the CFG for all queued instructions, taking special care of inserting
   code on edges between call and storing its return value.  */

void
commit_edge_insertions_watch_calls ()
{
  basic_block bb;
  sbitmap blocks;
  bool changed = false;

#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, EXIT_BLOCK_PTR, next_bb)
    {
      edge e, next;

      for (e = bb->succ; e; e = next)
	{
	  next = e->succ_next;
	  if (e->insns)
	    {
	      changed = true;
	      commit_one_edge_insertion (e, true);
	    }
	}
    }

  if (!changed)
    return;

  blocks = sbitmap_alloc (last_basic_block);
  sbitmap_zero (blocks);
  FOR_EACH_BB (bb)
    if (bb->aux)
      {
        SET_BIT (blocks, bb->index);
	bb->aux = NULL;
      }
  find_many_sub_basic_blocks (blocks);
  sbitmap_free (blocks);
}

/* Print out one basic block with live information at start and end.  */

void
dump_bb (bb, outf)
     basic_block bb;
     FILE *outf;
{
  rtx insn;
  rtx last;
  edge e;

  fprintf (outf, ";; Basic block %d, loop depth %d, count ",
	   bb->index, bb->loop_depth);
  fprintf (outf, HOST_WIDEST_INT_PRINT_DEC, (HOST_WIDEST_INT) bb->count);
  putc ('\n', outf);

  fputs (";; Predecessors: ", outf);
  for (e = bb->pred; e; e = e->pred_next)
    dump_edge_info (outf, e, 0);
  putc ('\n', outf);

  fputs (";; Registers live at start:", outf);
  dump_regset (bb->global_live_at_start, outf);
  putc ('\n', outf);

  for (insn = bb->head, last = NEXT_INSN (bb->end); insn != last;
       insn = NEXT_INSN (insn))
    print_rtl_single (outf, insn);

  fputs (";; Registers live at end:", outf);
  dump_regset (bb->global_live_at_end, outf);
  putc ('\n', outf);

  fputs (";; Successors: ", outf);
  for (e = bb->succ; e; e = e->succ_next)
    dump_edge_info (outf, e, 1);
  putc ('\n', outf);
}

void
debug_bb (bb)
     basic_block bb;
{
  dump_bb (bb, stderr);
}

void
debug_bb_n (n)
     int n;
{
  dump_bb (BASIC_BLOCK (n), stderr);
}

/* Like print_rtl, but also print out live information for the start of each
   basic block.  */

void
print_rtl_with_bb (outf, rtx_first)
     FILE *outf;
     rtx rtx_first;
{
  rtx tmp_rtx;

  if (rtx_first == 0)
    fprintf (outf, "(nil)\n");
  else
    {
      enum bb_state { NOT_IN_BB, IN_ONE_BB, IN_MULTIPLE_BB };
      int max_uid = get_max_uid ();
      basic_block *start
	= (basic_block *) xcalloc (max_uid, sizeof (basic_block));
      basic_block *end
	= (basic_block *) xcalloc (max_uid, sizeof (basic_block));
      enum bb_state *in_bb_p
	= (enum bb_state *) xcalloc (max_uid, sizeof (enum bb_state));

      basic_block bb;

      FOR_EACH_BB_REVERSE (bb)
	{
	  rtx x;

	  start[INSN_UID (bb->head)] = bb;
	  end[INSN_UID (bb->end)] = bb;
	  for (x = bb->head; x != NULL_RTX; x = NEXT_INSN (x))
	    {
	      enum bb_state state = IN_MULTIPLE_BB;

	      if (in_bb_p[INSN_UID (x)] == NOT_IN_BB)
		state = IN_ONE_BB;
	      in_bb_p[INSN_UID (x)] = state;

	      if (x == bb->end)
		break;
	    }
	}

      for (tmp_rtx = rtx_first; NULL != tmp_rtx; tmp_rtx = NEXT_INSN (tmp_rtx))
	{
	  int did_output;

	  if ((bb = start[INSN_UID (tmp_rtx)]) != NULL)
	    {
	      fprintf (outf, ";; Start of basic block %d, registers live:",
		       bb->index);
	      dump_regset (bb->global_live_at_start, outf);
	      putc ('\n', outf);
	    }

	  if (in_bb_p[INSN_UID (tmp_rtx)] == NOT_IN_BB
	      && GET_CODE (tmp_rtx) != NOTE
	      && GET_CODE (tmp_rtx) != BARRIER)
	    fprintf (outf, ";; Insn is not within a basic block\n");
	  else if (in_bb_p[INSN_UID (tmp_rtx)] == IN_MULTIPLE_BB)
	    fprintf (outf, ";; Insn is in multiple basic blocks\n");

	  did_output = print_rtl_single (outf, tmp_rtx);

	  if ((bb = end[INSN_UID (tmp_rtx)]) != NULL)
	    {
	      fprintf (outf, ";; End of basic block %d, registers live:\n",
		       bb->index);
	      dump_regset (bb->global_live_at_end, outf);
	      putc ('\n', outf);
	    }

	  if (did_output)
	    putc ('\n', outf);
	}

      free (start);
      free (end);
      free (in_bb_p);
    }

  if (current_function_epilogue_delay_list != 0)
    {
      fprintf (outf, "\n;; Insns in epilogue delay list:\n\n");
      for (tmp_rtx = current_function_epilogue_delay_list; tmp_rtx != 0;
	   tmp_rtx = XEXP (tmp_rtx, 1))
	print_rtl_single (outf, XEXP (tmp_rtx, 0));
    }
}

void
update_br_prob_note (bb)
     basic_block bb;
{
  rtx note;
  if (GET_CODE (bb->end) != JUMP_INSN)
    return;
  note = find_reg_note (bb->end, REG_BR_PROB, NULL_RTX);
  if (!note || INTVAL (XEXP (note, 0)) == BRANCH_EDGE (bb)->probability)
    return;
  XEXP (note, 0) = GEN_INT (BRANCH_EDGE (bb)->probability);
}

/* Verify the CFG consistency.  This function check some CFG invariants and
   aborts when something is wrong.  Hope that this function will help to
   convert many optimization passes to preserve CFG consistent.

   Currently it does following checks:

   - test head/end pointers
   - overlapping of basic blocks
   - edge list correctness
   - headers of basic blocks (the NOTE_INSN_BASIC_BLOCK note)
   - tails of basic blocks (ensure that boundary is necessary)
   - scans body of the basic block for JUMP_INSN, CODE_LABEL
     and NOTE_INSN_BASIC_BLOCK
   - check that all insns are in the basic blocks
     (except the switch handling code, barriers and notes)
   - check that all returns are followed by barriers

   In future it can be extended check a lot of other stuff as well
   (reachability of basic blocks, life information, etc. etc.).  */

void
verify_flow_info ()
{
  const int max_uid = get_max_uid ();
  const rtx rtx_first = get_insns ();
  rtx last_head = get_last_insn ();
  basic_block *bb_info, *last_visited;
  size_t *edge_checksum;
  rtx x;
  int num_bb_notes, err = 0;
  basic_block bb, last_bb_seen;

  bb_info = (basic_block *) xcalloc (max_uid, sizeof (basic_block));
  last_visited = (basic_block *) xcalloc (last_basic_block + 2,
					  sizeof (basic_block));
  edge_checksum = (size_t *) xcalloc (last_basic_block + 2, sizeof (size_t));

  /* Check bb chain & numbers.  */
  last_bb_seen = ENTRY_BLOCK_PTR;
  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR->next_bb, NULL, next_bb)
    {
      if (bb != EXIT_BLOCK_PTR
	  && bb != BASIC_BLOCK (bb->index))
	{
	  error ("bb %d on wrong place", bb->index);
	  err = 1;
	}

      if (bb->prev_bb != last_bb_seen)
	{
	  error ("prev_bb of %d should be %d, not %d",
		 bb->index, last_bb_seen->index, bb->prev_bb->index);
	  err = 1;
	}

      last_bb_seen = bb;
    }

  FOR_EACH_BB_REVERSE (bb)
    {
      rtx head = bb->head;
      rtx end = bb->end;

      /* Verify the end of the basic block is in the INSN chain.  */
      for (x = last_head; x != NULL_RTX; x = PREV_INSN (x))
	if (x == end)
	  break;

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
	     in only one basic block and initialize the BB_INFO array
	     used by other passes.  */
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

      last_head = x;
    }

  /* Now check the basic blocks (boundaries etc.) */
  FOR_EACH_BB_REVERSE (bb)
    {
      int n_fallthru = 0, n_eh = 0, n_call = 0, n_abnormal = 0, n_branch = 0;
      edge e;
      rtx note;

      if (INSN_P (bb->end)
	  && (note = find_reg_note (bb->end, REG_BR_PROB, NULL_RTX))
	  && bb->succ && bb->succ->succ_next
	  && any_condjump_p (bb->end))
	{
	  if (INTVAL (XEXP (note, 0)) != BRANCH_EDGE (bb)->probability)
	    {
	      error ("verify_flow_info: REG_BR_PROB does not match cfg %i %i",
		     INTVAL (XEXP (note, 0)), BRANCH_EDGE (bb)->probability);
	      err = 1;
	    }
	}
      if (bb->count < 0)
	{
	  error ("verify_flow_info: Wrong count of block %i %i",
	         bb->index, (int)bb->count);
	  err = 1;
	}
      if (bb->frequency < 0)
	{
	  error ("verify_flow_info: Wrong frequency of block %i %i",
	         bb->index, bb->frequency);
	  err = 1;
	}
      for (e = bb->succ; e; e = e->succ_next)
	{
	  if (last_visited [e->dest->index + 2] == bb)
	    {
	      error ("verify_flow_info: Duplicate edge %i->%i",
		     e->src->index, e->dest->index);
	      err = 1;
	    }
	  if (e->probability < 0 || e->probability > REG_BR_PROB_BASE)
	    {
	      error ("verify_flow_info: Wrong probability of edge %i->%i %i",
		     e->src->index, e->dest->index, e->probability);
	      err = 1;
	    }
	  if (e->count < 0)
	    {
	      error ("verify_flow_info: Wrong count of edge %i->%i %i",
		     e->src->index, e->dest->index, (int)e->count);
	      err = 1;
	    }

	  last_visited [e->dest->index + 2] = bb;

	  if (e->flags & EDGE_FALLTHRU)
	    n_fallthru++;

	  if ((e->flags & ~EDGE_DFS_BACK) == 0)
	    n_branch++;

	  if (e->flags & EDGE_ABNORMAL_CALL)
	    n_call++;

	  if (e->flags & EDGE_EH)
	    n_eh++;
	  else if (e->flags & EDGE_ABNORMAL)
	    n_abnormal++;

	  if ((e->flags & EDGE_FALLTHRU)
	      && e->src != ENTRY_BLOCK_PTR
	      && e->dest != EXIT_BLOCK_PTR)
	    {
	      rtx insn;

	      if (e->src->next_bb != e->dest)
		{
		  error
		    ("verify_flow_info: Incorrect blocks for fallthru %i->%i",
		     e->src->index, e->dest->index);
		  err = 1;
		}
	      else
		for (insn = NEXT_INSN (e->src->end); insn != e->dest->head;
		     insn = NEXT_INSN (insn))
		  if (GET_CODE (insn) == BARRIER
#ifndef CASE_DROPS_THROUGH
		      || INSN_P (insn)
#else
		      || (INSN_P (insn) && ! JUMP_TABLE_DATA_P (insn))
#endif
		      )
		    {
		      error ("verify_flow_info: Incorrect fallthru %i->%i",
			     e->src->index, e->dest->index);
		      fatal_insn ("wrong insn in the fallthru edge", insn);
		      err = 1;
		    }
	    }

	  if (e->src != bb)
	    {
	      error ("verify_flow_info: Basic block %d succ edge is corrupted",
		     bb->index);
	      fprintf (stderr, "Predecessor: ");
	      dump_edge_info (stderr, e, 0);
	      fprintf (stderr, "\nSuccessor: ");
	      dump_edge_info (stderr, e, 1);
	      fprintf (stderr, "\n");
	      err = 1;
	    }

	  edge_checksum[e->dest->index + 2] += (size_t) e;
	}

      if (n_eh && GET_CODE (PATTERN (bb->end)) != RESX
	  && !find_reg_note (bb->end, REG_EH_REGION, NULL_RTX))
	{
	  error ("Missing REG_EH_REGION note in the end of bb %i", bb->index);
	  err = 1;
	}
      if (n_branch
	  && (GET_CODE (bb->end) != JUMP_INSN
	      || (n_branch > 1 && (any_uncondjump_p (bb->end)
				   || any_condjump_p (bb->end)))))
	{
	  error ("Too many outgoing branch edges from bb %i", bb->index);
	  err = 1;
	}
      if (n_fallthru && any_uncondjump_p (bb->end))
	{
	  error ("Fallthru edge after unconditional jump %i", bb->index);
	  err = 1;
	}
      if (n_branch != 1 && any_uncondjump_p (bb->end))
	{
	  error ("Wrong amount of branch edges after unconditional jump %i", bb->index);
	  err = 1;
	}
      if (n_branch != 1 && any_condjump_p (bb->end)
	  && JUMP_LABEL (bb->end) != bb->next_bb->head)
	{
	  error ("Wrong amount of branch edges after conditional jump %i", bb->index);
	  err = 1;
	}
      if (n_call && GET_CODE (bb->end) != CALL_INSN)
	{
	  error ("Call edges for non-call insn in bb %i", bb->index);
	  err = 1;
	}
      if (n_abnormal
	  && (GET_CODE (bb->end) != CALL_INSN && n_call != n_abnormal)
	  && (GET_CODE (bb->end) != JUMP_INSN
	      || any_condjump_p (bb->end)
	      || any_uncondjump_p (bb->end)))
	{
	  error ("Abnormal edges for no purpose in bb %i", bb->index);
	  err = 1;
	}

      if (!n_fallthru)
	{
	  rtx insn;

	  /* Ensure existence of barrier in BB with no fallthru edges.  */
	  for (insn = bb->end; !insn || GET_CODE (insn) != BARRIER;
	       insn = NEXT_INSN (insn))
	    if (!insn
		|| (GET_CODE (insn) == NOTE
		    && NOTE_LINE_NUMBER (insn) == NOTE_INSN_BASIC_BLOCK))
		{
		  error ("missing barrier after block %i", bb->index);
		  err = 1;
		  break;
		}
	}

      for (e = bb->pred; e; e = e->pred_next)
	{
	  if (e->dest != bb)
	    {
	      error ("basic block %d pred edge is corrupted", bb->index);
	      fputs ("Predecessor: ", stderr);
	      dump_edge_info (stderr, e, 0);
	      fputs ("\nSuccessor: ", stderr);
	      dump_edge_info (stderr, e, 1);
	      fputc ('\n', stderr);
	      err = 1;
	    }
	  edge_checksum[e->dest->index + 2] -= (size_t) e;
	}

      for (x = bb->head; x != NEXT_INSN (bb->end); x = NEXT_INSN (x))
	if (BLOCK_FOR_INSN (x) != bb)
	  {
	    debug_rtx (x);
	    if (! BLOCK_FOR_INSN (x))
	      error
		("insn %d inside basic block %d but block_for_insn is NULL",
		 INSN_UID (x), bb->index);
	    else
	      error
		("insn %d inside basic block %d but block_for_insn is %i",
		 INSN_UID (x), bb->index, BLOCK_FOR_INSN (x)->index);

	    err = 1;
	  }

      /* OK pointers are correct.  Now check the header of basic
         block.  It ought to contain optional CODE_LABEL followed
	 by NOTE_BASIC_BLOCK.  */
      x = bb->head;
      if (GET_CODE (x) == CODE_LABEL)
	{
	  if (bb->end == x)
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

      if (bb->end == x)
	/* Do checks for empty blocks her. e */
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

	    if (x == bb->end)
	      break;

	    if (GET_CODE (x) == JUMP_INSN
		|| GET_CODE (x) == CODE_LABEL
		|| GET_CODE (x) == BARRIER)
	      {
		error ("in basic block %d:", bb->index);
		fatal_insn ("flow control insn inside a basic block", x);
	      }
	  }
    }

  /* Complete edge checksumming for ENTRY and EXIT.  */
  {
    edge e;

    for (e = ENTRY_BLOCK_PTR->succ; e ; e = e->succ_next)
      edge_checksum[e->dest->index + 2] += (size_t) e;

    for (e = EXIT_BLOCK_PTR->pred; e ; e = e->pred_next)
      edge_checksum[e->dest->index + 2] -= (size_t) e;
  }

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
    if (edge_checksum[bb->index + 2])
      {
	error ("basic block %i edge lists are corrupted", bb->index);
	err = 1;
      }

  num_bb_notes = 0;
  last_bb_seen = ENTRY_BLOCK_PTR;

  for (x = rtx_first; x; x = NEXT_INSN (x))
    {
      if (NOTE_INSN_BASIC_BLOCK_P (x))
	{
	  bb = NOTE_BASIC_BLOCK (x);

	  num_bb_notes++;
	  if (bb != last_bb_seen->next_bb)
	    internal_error ("basic blocks not numbered consecutively");

	  last_bb_seen = bb;
	}

      if (!bb_info[INSN_UID (x)])
	{
	  switch (GET_CODE (x))
	    {
	    case BARRIER:
	    case NOTE:
	      break;

	    case CODE_LABEL:
	      /* An addr_vec is placed outside any block block.  */
	      if (NEXT_INSN (x)
		  && GET_CODE (NEXT_INSN (x)) == JUMP_INSN
		  && (GET_CODE (PATTERN (NEXT_INSN (x))) == ADDR_DIFF_VEC
		      || GET_CODE (PATTERN (NEXT_INSN (x))) == ADDR_VEC))
		x = NEXT_INSN (x);

	      /* But in any case, non-deletable labels can appear anywhere.  */
	      break;

	    default:
	      fatal_insn ("insn outside basic block", x);
	    }
	}

      if (INSN_P (x)
	  && GET_CODE (x) == JUMP_INSN
	  && returnjump_p (x) && ! condjump_p (x)
	  && ! (NEXT_INSN (x) && GET_CODE (NEXT_INSN (x)) == BARRIER))
	    fatal_insn ("return not followed by barrier", x);
    }

  if (num_bb_notes != n_basic_blocks)
    internal_error
      ("number of bb notes in insn chain (%d) != n_basic_blocks (%d)",
       num_bb_notes, n_basic_blocks);

  if (err)
    internal_error ("verify_flow_info failed");

  /* Clean up.  */
  free (bb_info);
  free (last_visited);
  free (edge_checksum);
}

/* Assume that the preceding pass has possibly eliminated jump instructions
   or converted the unconditional jumps.  Eliminate the edges from CFG.
   Return true if any edges are eliminated.  */

bool
purge_dead_edges (bb)
     basic_block bb;
{
  edge e, next;
  rtx insn = bb->end, note;
  bool purged = false;

  /* If this instruction cannot trap, remove REG_EH_REGION notes.  */
  if (GET_CODE (insn) == INSN
      && (note = find_reg_note (insn, REG_EH_REGION, NULL)))
    {
      rtx eqnote;

      if (! may_trap_p (PATTERN (insn))
	  || ((eqnote = find_reg_equal_equiv_note (insn))
	      && ! may_trap_p (XEXP (eqnote, 0))))
	remove_note (insn, note);
    }

  /* Cleanup abnormal edges caused by exceptions or non-local gotos.  */
  for (e = bb->succ; e; e = next)
    {
      next = e->succ_next;
      if (e->flags & EDGE_EH)
	{
	  if (can_throw_internal (bb->end))
	    continue;
	}
      else if (e->flags & EDGE_ABNORMAL_CALL)
	{
	  if (GET_CODE (bb->end) == CALL_INSN
	      && (! (note = find_reg_note (insn, REG_EH_REGION, NULL))
		  || INTVAL (XEXP (note, 0)) >= 0))
	    continue;
	}
      else
	continue;

      remove_edge (e);
      bb->flags |= BB_DIRTY;
      purged = true;
    }

  if (GET_CODE (insn) == JUMP_INSN)
    {
      rtx note;
      edge b,f;

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

      for (e = bb->succ; e; e = next)
	{
	  next = e->succ_next;

	  /* Avoid abnormal flags to leak from computed jumps turned
	     into simplejumps.  */

	  e->flags &= ~EDGE_ABNORMAL;

	  /* See if this edge is one we should keep.  */
	  if ((e->flags & EDGE_FALLTHRU) && any_condjump_p (insn))
	    /* A conditional jump can fall through into the next
	       block, so we should keep the edge.  */
	    continue;
	  else if (e->dest != EXIT_BLOCK_PTR
		   && e->dest->head == JUMP_LABEL (insn))
	    /* If the destination block is the target of the jump,
	       keep the edge.  */
	    continue;
	  else if (e->dest == EXIT_BLOCK_PTR && returnjump_p (insn))
	    /* If the destination block is the exit block, and this
	       instruction is a return, then keep the edge.  */
	    continue;
	  else if ((e->flags & EDGE_EH) && can_throw_internal (insn))
	    /* Keep the edges that correspond to exceptions thrown by
	       this instruction.  */
	    continue;

	  /* We do not need this edge.  */
	  bb->flags |= BB_DIRTY;
	  purged = true;
	  remove_edge (e);
	}

      if (!bb->succ || !purged)
	return purged;

      if (rtl_dump_file)
	fprintf (rtl_dump_file, "Purged edges from bb %i\n", bb->index);

      if (!optimize)
	return purged;

      /* Redistribute probabilities.  */
      if (!bb->succ->succ_next)
	{
	  bb->succ->probability = REG_BR_PROB_BASE;
	  bb->succ->count = bb->count;
	}
      else
	{
	  note = find_reg_note (insn, REG_BR_PROB, NULL);
	  if (!note)
	    return purged;

	  b = BRANCH_EDGE (bb);
	  f = FALLTHRU_EDGE (bb);
	  b->probability = INTVAL (XEXP (note, 0));
	  f->probability = REG_BR_PROB_BASE - b->probability;
	  b->count = bb->count * b->probability / REG_BR_PROB_BASE;
	  f->count = bb->count * f->probability / REG_BR_PROB_BASE;
	}

      return purged;
    }

  /* If we don't see a jump insn, we don't know exactly why the block would
     have been broken at this point.  Look for a simple, non-fallthru edge,
     as these are only created by conditional branches.  If we find such an
     edge we know that there used to be a jump here and can then safely
     remove all non-fallthru edges.  */
  for (e = bb->succ; e && (e->flags & (EDGE_COMPLEX | EDGE_FALLTHRU));
       e = e->succ_next)
    ;

  if (!e)
    return purged;

  for (e = bb->succ; e; e = next)
    {
      next = e->succ_next;
      if (!(e->flags & EDGE_FALLTHRU))
	{
	  bb->flags |= BB_DIRTY;
	  remove_edge (e);
	  purged = true;
	}
    }

  if (!bb->succ || bb->succ->succ_next)
    abort ();

  bb->succ->probability = REG_BR_PROB_BASE;
  bb->succ->count = bb->count;

  if (rtl_dump_file)
    fprintf (rtl_dump_file, "Purged non-fallthru edges from bb %i\n",
	     bb->index);
  return purged;
}

/* Search all basic blocks for potentially dead edges and purge them.  Return
   true if some edge has been eliminated.  */

bool
purge_all_dead_edges (update_life_p)
     int update_life_p;
{
  int purged = false;
  sbitmap blocks = 0;
  basic_block bb;

  if (update_life_p)
    {
      blocks = sbitmap_alloc (last_basic_block);
      sbitmap_zero (blocks);
    }

  FOR_EACH_BB (bb)
    {
      bool purged_here = purge_dead_edges (bb);

      purged |= purged_here;
      if (purged_here && update_life_p)
	SET_BIT (blocks, bb->index);
    }

  if (update_life_p && purged)
    update_life_info (blocks, UPDATE_LIFE_GLOBAL,
		      PROP_DEATH_NOTES | PROP_SCAN_DEAD_CODE
		      | PROP_KILL_DEAD_CODE);

  if (update_life_p)
    sbitmap_free (blocks);
  return purged;
}
