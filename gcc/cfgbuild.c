/* Control flow graph building code for GNU compiler.
   Copyright (C) 1987, 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2003 Free Software Foundation, Inc.

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

/* find_basic_blocks divides the current function's rtl into basic
   blocks and constructs the CFG.  The blocks are recorded in the
   basic_block_info array; the CFG exists in the edge structures
   referenced by the blocks.

   find_basic_blocks also finds any unreachable loops and deletes them.

   Available functionality:
     - CFG construction
         find_basic_blocks
     - Local CFG construction
         find_sub_basic_blocks		 */

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
#include "timevar.h"
#include "obstack.h"

static int count_basic_blocks		PARAMS ((rtx));
static void find_basic_blocks_1		PARAMS ((rtx));
static rtx find_label_refs		PARAMS ((rtx, rtx));
static void make_edges			PARAMS ((rtx, int, int, int));
static void make_label_edge		PARAMS ((sbitmap *, basic_block,
						 rtx, int));
static void make_eh_edge		PARAMS ((sbitmap *, basic_block, rtx));
static void find_bb_boundaries		PARAMS ((basic_block));
static void compute_outgoing_frequencies PARAMS ((basic_block));
static bool inside_basic_block_p	PARAMS ((rtx));

/* Return true if insn is something that should be contained inside basic
   block.  */

static bool
inside_basic_block_p (insn)
     rtx insn;
{
  switch (GET_CODE (insn))
    {
    case CODE_LABEL:
      /* Avoid creating of basic block for jumptables.  */
      return (NEXT_INSN (insn) == 0
	      || GET_CODE (NEXT_INSN (insn)) != JUMP_INSN
	      || (GET_CODE (PATTERN (NEXT_INSN (insn))) != ADDR_VEC
		  && GET_CODE (PATTERN (NEXT_INSN (insn))) != ADDR_DIFF_VEC));

    case JUMP_INSN:
      return (GET_CODE (PATTERN (insn)) != ADDR_VEC
	      && GET_CODE (PATTERN (insn)) != ADDR_DIFF_VEC);

    case CALL_INSN:
    case INSN:
      return true;

    case BARRIER:
    case NOTE:
      return false;

    default:
      abort ();
    }
}

/* Return true if INSN may cause control flow transfer, so it should be last in
   the basic block.  */

bool
control_flow_insn_p (insn)
     rtx insn;
{
  rtx note;

  switch (GET_CODE (insn))
    {
      case NOTE:
      case CODE_LABEL:
	return false;

      case JUMP_INSN:
	/* Jump insn always causes control transfer except for tablejumps.  */
	return (GET_CODE (PATTERN (insn)) != ADDR_VEC
		&& GET_CODE (PATTERN (insn)) != ADDR_DIFF_VEC);

      case CALL_INSN:
	/* Call insn may return to the nonlocal goto handler.  */
	return ((nonlocal_goto_handler_labels
		 && (0 == (note = find_reg_note (insn, REG_EH_REGION,
						 NULL_RTX))
		     || INTVAL (XEXP (note, 0)) >= 0))
		/* Or may trap.  */
		|| can_throw_internal (insn));

      case INSN:
	return (flag_non_call_exceptions && can_throw_internal (insn));

      case BARRIER:
	/* It is nonsence to reach barrier when looking for the
	   end of basic block, but before dead code is eliminated
	   this may happen.  */
	return false;

      default:
	abort ();
    }
}

/* Count the basic blocks of the function.  */

static int
count_basic_blocks (f)
     rtx f;
{
  int count = 0;
  bool saw_insn = false;
  rtx insn;

  for (insn = f; insn; insn = NEXT_INSN (insn))
    {
      /* Code labels and barriers causes curent basic block to be
         terminated at previous real insn.  */
      if ((GET_CODE (insn) == CODE_LABEL || GET_CODE (insn) == BARRIER)
	  && saw_insn)
	count++, saw_insn = false;

      /* Start basic block if needed.  */
      if (!saw_insn && inside_basic_block_p (insn))
	saw_insn = true;

      /* Control flow insn causes current basic block to be terminated.  */
      if (saw_insn && control_flow_insn_p (insn))
	count++, saw_insn = false;
    }

  if (saw_insn)
    count++;

  /* The rest of the compiler works a bit smoother when we don't have to
     check for the edge case of do-nothing functions with no basic blocks.  */
  if (count == 0)
    {
      emit_insn (gen_rtx_USE (VOIDmode, const0_rtx));
      count = 1;
    }

  return count;
}

/* Scan a list of insns for labels referred to other than by jumps.
   This is used to scan the alternatives of a call placeholder.  */

static rtx
find_label_refs (f, lvl)
     rtx f;
     rtx lvl;
{
  rtx insn;

  for (insn = f; insn; insn = NEXT_INSN (insn))
    if (INSN_P (insn) && GET_CODE (insn) != JUMP_INSN)
      {
	rtx note;

	/* Make a list of all labels referred to other than by jumps
	   (which just don't have the REG_LABEL notes).

	   Make a special exception for labels followed by an ADDR*VEC,
	   as this would be a part of the tablejump setup code.

	   Make a special exception to registers loaded with label
	   values just before jump insns that use them.  */

	for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
	  if (REG_NOTE_KIND (note) == REG_LABEL)
	    {
	      rtx lab = XEXP (note, 0), next;

	      if ((next = next_nonnote_insn (lab)) != NULL
		       && GET_CODE (next) == JUMP_INSN
		       && (GET_CODE (PATTERN (next)) == ADDR_VEC
			   || GET_CODE (PATTERN (next)) == ADDR_DIFF_VEC))
		;
	      else if (GET_CODE (lab) == NOTE)
		;
	      else if (GET_CODE (NEXT_INSN (insn)) == JUMP_INSN
		       && find_reg_note (NEXT_INSN (insn), REG_LABEL, lab))
		;
	      else
		lvl = alloc_EXPR_LIST (0, XEXP (note, 0), lvl);
	    }
      }

  return lvl;
}

/* Create an edge between two basic blocks.  FLAGS are auxiliary information
   about the edge that is accumulated between calls.  */

/* Create an edge from a basic block to a label.  */

static void
make_label_edge (edge_cache, src, label, flags)
     sbitmap *edge_cache;
     basic_block src;
     rtx label;
     int flags;
{
  if (GET_CODE (label) != CODE_LABEL)
    abort ();

  /* If the label was never emitted, this insn is junk, but avoid a
     crash trying to refer to BLOCK_FOR_INSN (label).  This can happen
     as a result of a syntax error and a diagnostic has already been
     printed.  */

  if (INSN_UID (label) == 0)
    return;

  cached_make_edge (edge_cache, src, BLOCK_FOR_INSN (label), flags);
}

/* Create the edges generated by INSN in REGION.  */

static void
make_eh_edge (edge_cache, src, insn)
     sbitmap *edge_cache;
     basic_block src;
     rtx insn;
{
  int is_call = GET_CODE (insn) == CALL_INSN ? EDGE_ABNORMAL_CALL : 0;
  rtx handlers, i;

  handlers = reachable_handlers (insn);

  for (i = handlers; i; i = XEXP (i, 1))
    make_label_edge (edge_cache, src, XEXP (i, 0),
		     EDGE_ABNORMAL | EDGE_EH | is_call);

  free_INSN_LIST_list (&handlers);
}

/* Identify the edges between basic blocks MIN to MAX.

   NONLOCAL_LABEL_LIST is a list of non-local labels in the function.  Blocks
   that are otherwise unreachable may be reachable with a non-local goto.

   BB_EH_END is an array indexed by basic block number in which we record
   the list of exception regions active at the end of the basic block.  */

static void
make_edges (label_value_list, min, max, update_p)
     rtx label_value_list;
     int min, max, update_p;
{
  int i;
  sbitmap *edge_cache = NULL;

  /* Assume no computed jump; revise as we create edges.  */
  current_function_has_computed_jump = 0;

  /* Heavy use of computed goto in machine-generated code can lead to
     nearly fully-connected CFGs.  In that case we spend a significant
     amount of time searching the edge lists for duplicates.  */
  if (forced_labels || label_value_list)
    {
      edge_cache = sbitmap_vector_alloc (n_basic_blocks, n_basic_blocks);
      sbitmap_vector_zero (edge_cache, n_basic_blocks);

      if (update_p)
	for (i = min; i <= max; ++i)
	  {
	    edge e;

	    for (e = BASIC_BLOCK (i)->succ; e ; e = e->succ_next)
	      if (e->dest != EXIT_BLOCK_PTR)
	        SET_BIT (edge_cache[i], e->dest->index);
	  }
    }

  /* By nature of the way these get numbered, block 0 is always the entry.  */
  if (min == 0)
    cached_make_edge (edge_cache, ENTRY_BLOCK_PTR, BASIC_BLOCK (0),
		      EDGE_FALLTHRU);

  for (i = min; i <= max; ++i)
    {
      basic_block bb = BASIC_BLOCK (i);
      rtx insn, x;
      enum rtx_code code;
      int force_fallthru = 0;

      if (GET_CODE (bb->head) == CODE_LABEL && LABEL_ALTERNATE_NAME (bb->head))
	cached_make_edge (NULL, ENTRY_BLOCK_PTR, bb, 0);

      /* Examine the last instruction of the block, and discover the
	 ways we can leave the block.  */

      insn = bb->end;
      code = GET_CODE (insn);

      /* A branch.  */
      if (code == JUMP_INSN)
	{
	  rtx tmp;

	  /* Recognize exception handling placeholders.  */
	  if (GET_CODE (PATTERN (insn)) == RESX)
	    make_eh_edge (edge_cache, bb, insn);

	  /* Recognize a non-local goto as a branch outside the
	     current function.  */
	  else if (find_reg_note (insn, REG_NON_LOCAL_GOTO, NULL_RTX))
	    ;

	  /* ??? Recognize a tablejump and do the right thing.  */
	  else if ((tmp = JUMP_LABEL (insn)) != NULL_RTX
		   && (tmp = NEXT_INSN (tmp)) != NULL_RTX
		   && GET_CODE (tmp) == JUMP_INSN
		   && (GET_CODE (PATTERN (tmp)) == ADDR_VEC
		       || GET_CODE (PATTERN (tmp)) == ADDR_DIFF_VEC))
	    {
	      rtvec vec;
	      int j;

	      if (GET_CODE (PATTERN (tmp)) == ADDR_VEC)
		vec = XVEC (PATTERN (tmp), 0);
	      else
		vec = XVEC (PATTERN (tmp), 1);

	      for (j = GET_NUM_ELEM (vec) - 1; j >= 0; --j)
		make_label_edge (edge_cache, bb,
				 XEXP (RTVEC_ELT (vec, j), 0), 0);

	      /* Some targets (eg, ARM) emit a conditional jump that also
		 contains the out-of-range target.  Scan for these and
		 add an edge if necessary.  */
	      if ((tmp = single_set (insn)) != NULL
		  && SET_DEST (tmp) == pc_rtx
		  && GET_CODE (SET_SRC (tmp)) == IF_THEN_ELSE
		  && GET_CODE (XEXP (SET_SRC (tmp), 2)) == LABEL_REF)
		make_label_edge (edge_cache, bb,
				 XEXP (XEXP (SET_SRC (tmp), 2), 0), 0);

#ifdef CASE_DROPS_THROUGH
	      /* Silly VAXen.  The ADDR_VEC is going to be in the way of
		 us naturally detecting fallthru into the next block.  */
	      force_fallthru = 1;
#endif
	    }

	  /* If this is a computed jump, then mark it as reaching
	     everything on the label_value_list and forced_labels list.  */
	  else if (computed_jump_p (insn))
	    {
	      current_function_has_computed_jump = 1;

	      for (x = label_value_list; x; x = XEXP (x, 1))
		make_label_edge (edge_cache, bb, XEXP (x, 0), EDGE_ABNORMAL);

	      for (x = forced_labels; x; x = XEXP (x, 1))
		make_label_edge (edge_cache, bb, XEXP (x, 0), EDGE_ABNORMAL);
	    }

	  /* Returns create an exit out.  */
	  else if (returnjump_p (insn))
	    cached_make_edge (edge_cache, bb, EXIT_BLOCK_PTR, 0);

	  /* Otherwise, we have a plain conditional or unconditional jump.  */
	  else
	    {
	      if (! JUMP_LABEL (insn))
		abort ();
	      make_label_edge (edge_cache, bb, JUMP_LABEL (insn), 0);
	    }
	}

      /* If this is a sibling call insn, then this is in effect a combined call
	 and return, and so we need an edge to the exit block.  No need to
	 worry about EH edges, since we wouldn't have created the sibling call
	 in the first place.  */
      if (code == CALL_INSN && SIBLING_CALL_P (insn))
	cached_make_edge (edge_cache, bb, EXIT_BLOCK_PTR,
		   EDGE_ABNORMAL | EDGE_ABNORMAL_CALL);

      /* If this is a CALL_INSN, then mark it as reaching the active EH
	 handler for this CALL_INSN.  If we're handling non-call
	 exceptions then any insn can reach any of the active handlers.
	 Also mark the CALL_INSN as reaching any nonlocal goto handler.  */
      else if (code == CALL_INSN || flag_non_call_exceptions)
	{
	  /* Add any appropriate EH edges.  */
	  make_eh_edge (edge_cache, bb, insn);

	  if (code == CALL_INSN && nonlocal_goto_handler_labels)
	    {
	      /* ??? This could be made smarter: in some cases it's possible
		 to tell that certain calls will not do a nonlocal goto.
		 For example, if the nested functions that do the nonlocal
		 gotos do not have their addresses taken, then only calls to
		 those functions or to other nested functions that use them
		 could possibly do nonlocal gotos.  */

	      /* We do know that a REG_EH_REGION note with a value less
		 than 0 is guaranteed not to perform a non-local goto.  */
	      rtx note = find_reg_note (insn, REG_EH_REGION, NULL_RTX);

	      if (!note || INTVAL (XEXP (note, 0)) >=  0)
		for (x = nonlocal_goto_handler_labels; x; x = XEXP (x, 1))
		  make_label_edge (edge_cache, bb, XEXP (x, 0),
				   EDGE_ABNORMAL | EDGE_ABNORMAL_CALL);
	    }
	}

      /* Find out if we can drop through to the next block.  */
      insn = next_nonnote_insn (insn);
      if (!insn || (i + 1 == n_basic_blocks && force_fallthru))
	cached_make_edge (edge_cache, bb, EXIT_BLOCK_PTR, EDGE_FALLTHRU);
      else if (i + 1 < n_basic_blocks)
	{
	  rtx tmp = BLOCK_HEAD (i + 1);
	  if (GET_CODE (tmp) == NOTE)
	    tmp = next_nonnote_insn (tmp);
	  if (force_fallthru || insn == tmp)
	    cached_make_edge (edge_cache, bb, BASIC_BLOCK (i + 1),
			      EDGE_FALLTHRU);
	}
    }

  if (edge_cache)
    sbitmap_vector_free (edge_cache);
}

/* Find all basic blocks of the function whose first insn is F.

   Collect and return a list of labels whose addresses are taken.  This
   will be used in make_edges for use with computed gotos.  */

static void
find_basic_blocks_1 (f)
     rtx f;
{
  rtx insn, next;
  int i = 0;
  rtx bb_note = NULL_RTX;
  rtx lvl = NULL_RTX;
  rtx trll = NULL_RTX;
  rtx head = NULL_RTX;
  rtx end = NULL_RTX;

  /* We process the instructions in a slightly different way than we did
     previously.  This is so that we see a NOTE_BASIC_BLOCK after we have
     closed out the previous block, so that it gets attached at the proper
     place.  Since this form should be equivalent to the previous,
     count_basic_blocks continues to use the old form as a check.  */

  for (insn = f; insn; insn = next)
    {
      enum rtx_code code = GET_CODE (insn);

      next = NEXT_INSN (insn);

      if ((GET_CODE (insn) == CODE_LABEL || GET_CODE (insn) == BARRIER)
	  && head)
	{
	  create_basic_block_structure (i++, head, end, bb_note);
	  head = end = NULL_RTX;
	  bb_note = NULL_RTX;
	}

      if (inside_basic_block_p (insn))
	{
	  if (head == NULL_RTX)
	    head = insn;
	  end = insn;
	}

      if (head && control_flow_insn_p (insn))
	{
	  create_basic_block_structure (i++, head, end, bb_note);
	  head = end = NULL_RTX;
	  bb_note = NULL_RTX;
	}

      switch (code)
	{
	case NOTE:
	  {
	    int kind = NOTE_LINE_NUMBER (insn);

	    /* Look for basic block notes with which to keep the
	       basic_block_info pointers stable.  Unthread the note now;
	       we'll put it back at the right place in create_basic_block.
	       Or not at all if we've already found a note in this block.  */
	    if (kind == NOTE_INSN_BASIC_BLOCK)
	      {
		if (bb_note == NULL_RTX)
		  bb_note = insn;
		else
		  next = delete_insn (insn);
	      }
	    break;
	  }

	case CODE_LABEL:
	case JUMP_INSN:
	case INSN:
	case BARRIER:
	  break;

	case CALL_INSN:
	  if (GET_CODE (PATTERN (insn)) == CALL_PLACEHOLDER)
	    {
	      /* Scan each of the alternatives for label refs.  */
	      lvl = find_label_refs (XEXP (PATTERN (insn), 0), lvl);
	      lvl = find_label_refs (XEXP (PATTERN (insn), 1), lvl);
	      lvl = find_label_refs (XEXP (PATTERN (insn), 2), lvl);
	      /* Record its tail recursion label, if any.  */
	      if (XEXP (PATTERN (insn), 3) != NULL_RTX)
		trll = alloc_EXPR_LIST (0, XEXP (PATTERN (insn), 3), trll);
	    }
	  break;

	default:
	  abort ();
	}

      if (GET_CODE (insn) == INSN || GET_CODE (insn) == CALL_INSN)
	{
	  rtx note;

	  /* Make a list of all labels referred to other than by jumps.

	     Make a special exception for labels followed by an ADDR*VEC,
	     as this would be a part of the tablejump setup code.

	     Make a special exception to registers loaded with label
	     values just before jump insns that use them.  */

	  for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
	    if (REG_NOTE_KIND (note) == REG_LABEL)
	      {
		rtx lab = XEXP (note, 0), next;

		if ((next = next_nonnote_insn (lab)) != NULL
			 && GET_CODE (next) == JUMP_INSN
			 && (GET_CODE (PATTERN (next)) == ADDR_VEC
			     || GET_CODE (PATTERN (next)) == ADDR_DIFF_VEC))
		  ;
		else if (GET_CODE (lab) == NOTE)
		  ;
		else if (GET_CODE (NEXT_INSN (insn)) == JUMP_INSN
			 && find_reg_note (NEXT_INSN (insn), REG_LABEL, lab))
		  ;
		else
		  lvl = alloc_EXPR_LIST (0, XEXP (note, 0), lvl);
	      }
	}
    }

  if (head != NULL_RTX)
    create_basic_block_structure (i++, head, end, bb_note);
  else if (bb_note)
    delete_insn (bb_note);

  if (i != n_basic_blocks)
    abort ();

  label_value_list = lvl;
  tail_recursion_label_list = trll;
}


/* Find basic blocks of the current function.
   F is the first insn of the function and NREGS the number of register
   numbers in use.  */

void
find_basic_blocks (f, nregs, file)
     rtx f;
     int nregs ATTRIBUTE_UNUSED;
     FILE *file ATTRIBUTE_UNUSED;
{
  int max_uid;
  timevar_push (TV_CFG);

  basic_block_for_insn = 0;

  /* Flush out existing data.  */
  if (basic_block_info != NULL)
    {
      int i;

      clear_edges ();

      /* Clear bb->aux on all extant basic blocks.  We'll use this as a
	 tag for reuse during create_basic_block, just in case some pass
	 copies around basic block notes improperly.  */
      for (i = 0; i < n_basic_blocks; ++i)
	BASIC_BLOCK (i)->aux = NULL;

      VARRAY_FREE (basic_block_info);
    }

  n_basic_blocks = count_basic_blocks (f);

  /* Size the basic block table.  The actual structures will be allocated
     by find_basic_blocks_1, since we want to keep the structure pointers
     stable across calls to find_basic_blocks.  */
  /* ??? This whole issue would be much simpler if we called find_basic_blocks
     exactly once, and thereafter we don't have a single long chain of
     instructions at all until close to the end of compilation when we
     actually lay them out.  */

  VARRAY_BB_INIT (basic_block_info, n_basic_blocks, "basic_block_info");

  find_basic_blocks_1 (f);

  /* Record the block to which an insn belongs.  */
  /* ??? This should be done another way, by which (perhaps) a label is
     tagged directly with the basic block that it starts.  It is used for
     more than that currently, but IMO that is the only valid use.  */

  max_uid = get_max_uid ();
#ifdef AUTO_INC_DEC
  /* Leave space for insns life_analysis makes in some cases for auto-inc.
     These cases are rare, so we don't need too much space.  */
  max_uid += max_uid / 10;
#endif

  compute_bb_for_insn (max_uid);

  /* Discover the edges of our cfg.  */
  make_edges (label_value_list, 0, n_basic_blocks - 1, 0);

  /* Do very simple cleanup now, for the benefit of code that runs between
     here and cleanup_cfg, e.g. thread_prologue_and_epilogue_insns.  */
  tidy_fallthru_edges ();

#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif
  timevar_pop (TV_CFG);
}

/* State of basic block as seen by find_sub_basic_blocks.  */
enum state {BLOCK_NEW = 0, BLOCK_ORIGINAL, BLOCK_TO_SPLIT};

#define STATE(BB) (enum state) ((size_t) (BB)->aux)
#define SET_STATE(BB, STATE) ((BB)->aux = (void *) (size_t) (STATE))

/* Scan basic block BB for possible BB boundaries inside the block
   and create new basic blocks in the progress.  */

static void
find_bb_boundaries (bb)
     basic_block bb;
{
  rtx insn = bb->head;
  rtx end = bb->end;
  rtx flow_transfer_insn = NULL_RTX;
  edge fallthru = NULL;

  if (insn == bb->end)
    return;

  if (GET_CODE (insn) == CODE_LABEL)
    insn = NEXT_INSN (insn);

  /* Scan insn chain and try to find new basic block boundaries.  */
  while (1)
    {
      enum rtx_code code = GET_CODE (insn);

      /* On code label, split current basic block.  */
      if (code == CODE_LABEL)
	{
	  fallthru = split_block (bb, PREV_INSN (insn));
	  if (flow_transfer_insn)
	    bb->end = flow_transfer_insn;

	  bb = fallthru->dest;
	  remove_edge (fallthru);
	  flow_transfer_insn = NULL_RTX;
	  if (LABEL_ALTERNATE_NAME (insn))
	    make_edge (ENTRY_BLOCK_PTR, bb, 0);
	}

      /* In case we've previously seen an insn that effects a control
	 flow transfer, split the block.  */
      if (flow_transfer_insn && inside_basic_block_p (insn))
	{
	  fallthru = split_block (bb, PREV_INSN (insn));
	  bb->end = flow_transfer_insn;
	  bb = fallthru->dest;
	  remove_edge (fallthru);
	  flow_transfer_insn = NULL_RTX;
	}

      if (control_flow_insn_p (insn))
	flow_transfer_insn = insn;
      if (insn == end)
	break;
      insn = NEXT_INSN (insn);
    }

  /* In case expander replaced normal insn by sequence terminating by
     return and barrier, or possibly other sequence not behaving like
     ordinary jump, we need to take care and move basic block boundary.  */
  if (flow_transfer_insn)
    bb->end = flow_transfer_insn;

  /* We've possibly replaced the conditional jump by conditional jump
     followed by cleanup at fallthru edge, so the outgoing edges may
     be dead.  */
  purge_dead_edges (bb);
}

/*  Assume that frequency of basic block B is known.  Compute frequencies
    and probabilities of outgoing edges.  */

static void
compute_outgoing_frequencies (b)
     basic_block b;
{
  edge e, f;

  if (b->succ && b->succ->succ_next && !b->succ->succ_next->succ_next)
    {
      rtx note = find_reg_note (b->end, REG_BR_PROB, NULL);
      int probability;

      if (!note)
	return;

      probability = INTVAL (XEXP (find_reg_note (b->end,
						 REG_BR_PROB, NULL),
				  0));
      e = BRANCH_EDGE (b);
      e->probability = probability;
      e->count = ((b->count * probability + REG_BR_PROB_BASE / 2)
		  / REG_BR_PROB_BASE);
      f = FALLTHRU_EDGE (b);
      f->probability = REG_BR_PROB_BASE - probability;
      f->count = b->count - e->count;
    }

  if (b->succ && !b->succ->succ_next)
    {
      e = b->succ;
      e->probability = REG_BR_PROB_BASE;
      e->count = b->count;
    }
}

/* Assume that someone emitted code with control flow instructions to the
   basic block.  Update the data structure.  */

void
find_many_sub_basic_blocks (blocks)
     sbitmap blocks;
{
  int i;
  int min, max;

  for (i = 0; i < n_basic_blocks; i++)
    SET_STATE (BASIC_BLOCK (i),
	       TEST_BIT (blocks, i) ? BLOCK_TO_SPLIT : BLOCK_ORIGINAL);

  for (i = 0; i < n_basic_blocks; i++)
    if (STATE (BASIC_BLOCK (i)) == BLOCK_TO_SPLIT)
      find_bb_boundaries (BASIC_BLOCK (i));

  for (i = 0; i < n_basic_blocks; i++)
    if (STATE (BASIC_BLOCK (i)) != BLOCK_ORIGINAL)
      break;

  min = max = i;
  for (; i < n_basic_blocks; i++)
    if (STATE (BASIC_BLOCK (i)) != BLOCK_ORIGINAL)
      max = i;

  /* Now re-scan and wire in all edges.  This expect simple (conditional)
     jumps at the end of each new basic blocks.  */
  make_edges (NULL, min, max, 1);

  /* Update branch probabilities.  Expect only (un)conditional jumps
     to be created with only the forward edges.  */
  for (i = min; i <= max; i++)
    {
      edge e;
      basic_block b = BASIC_BLOCK (i);

      if (STATE (b) == BLOCK_ORIGINAL)
	continue;
      if (STATE (b) == BLOCK_NEW)
	{
	  b->count = 0;
	  b->frequency = 0;
	  for (e = b->pred; e; e=e->pred_next)
	    {
	      b->count += e->count;
	      b->frequency += EDGE_FREQUENCY (e);
	    }
	}

      compute_outgoing_frequencies (b);
    }

  for (i = 0; i < n_basic_blocks; i++)
    SET_STATE (BASIC_BLOCK (i), 0);
}

/* Like above but for single basic block only.  */

void
find_sub_basic_blocks (bb)
    basic_block bb;
{
  int i;
  int min, max;
  basic_block next = (bb->index == n_basic_blocks - 1
		      ? NULL : BASIC_BLOCK (bb->index + 1));

  min = bb->index;
  find_bb_boundaries (bb);
  max = (next ? next->index : n_basic_blocks) - 1;

  /* Now re-scan and wire in all edges.  This expect simple (conditional)
     jumps at the end of each new basic blocks.  */
  make_edges (NULL, min, max, 1);

  /* Update branch probabilities.  Expect only (un)conditional jumps
     to be created with only the forward edges.  */
  for (i = min; i <= max; i++)
    {
      edge e;
      basic_block b = BASIC_BLOCK (i);

      if (i != min)
	{
	  b->count = 0;
	  b->frequency = 0;
	  for (e = b->pred; e; e=e->pred_next)
	    {
	      b->count += e->count;
	      b->frequency += EDGE_FREQUENCY (e);
	    }
	}

      compute_outgoing_frequencies (b);
    }
}
