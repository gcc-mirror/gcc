/* Basic block reordering routines for the GNU compiler.
   Copyright (C) 2000, 2001, 2003 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "insn-config.h"
#include "output.h"
#include "function.h"
#include "obstack.h"
#include "cfglayout.h"
#include "cfgloop.h"
#include "target.h"
#include "ggc.h"
#include "alloc-pool.h"

/* The contents of the current function definition are allocated
   in this obstack, and all are freed at the end of the function.  */
extern struct obstack flow_obstack;

alloc_pool cfg_layout_pool;

/* Holds the interesting trailing notes for the function.  */
rtx cfg_layout_function_footer, cfg_layout_function_header;

static rtx skip_insns_after_block (basic_block);
static void record_effective_endpoints (void);
static rtx label_for_bb (basic_block);
static void fixup_reorder_chain (void);

static void set_block_levels (tree, int);
static void change_scope (rtx, tree, tree);

void verify_insn_chain (void);
static void fixup_fallthru_exit_predecessor (void);
static rtx duplicate_insn_chain (rtx, rtx);
static void break_superblocks (void);
static tree insn_scope (rtx);

rtx
unlink_insn_chain (rtx first, rtx last)
{
  rtx prevfirst = PREV_INSN (first);
  rtx nextlast = NEXT_INSN (last);

  PREV_INSN (first) = NULL;
  NEXT_INSN (last) = NULL;
  if (prevfirst)
    NEXT_INSN (prevfirst) = nextlast;
  if (nextlast)
    PREV_INSN (nextlast) = prevfirst;
  else
    set_last_insn (prevfirst);
  if (!prevfirst)
    set_first_insn (nextlast);
  return first;
}

/* Skip over inter-block insns occurring after BB which are typically
   associated with BB (e.g., barriers). If there are any such insns,
   we return the last one. Otherwise, we return the end of BB.  */

static rtx
skip_insns_after_block (basic_block bb)
{
  rtx insn, last_insn, next_head, prev;

  next_head = NULL_RTX;
  if (bb->next_bb != EXIT_BLOCK_PTR)
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
	  switch (NOTE_LINE_NUMBER (insn))
	    {
	    case NOTE_INSN_LOOP_END:
	    case NOTE_INSN_BLOCK_END:
	      last_insn = insn;
	      continue;
	    case NOTE_INSN_DELETED:
	    case NOTE_INSN_DELETED_LABEL:
	      continue;

	    default:
	      continue;
	      break;
	    }
	  break;

	case CODE_LABEL:
	  if (NEXT_INSN (insn)
	      && GET_CODE (NEXT_INSN (insn)) == JUMP_INSN
	      && (GET_CODE (PATTERN (NEXT_INSN (insn))) == ADDR_VEC
	          || GET_CODE (PATTERN (NEXT_INSN (insn))) == ADDR_DIFF_VEC))
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
     NOTE_INSN_LOOP_BEG
     barrier

     Where barrier belongs to jump_insn, but the note does not.  This can be
     created by removing the basic block originally following
     NOTE_INSN_LOOP_BEG.  In such case reorder the notes.  */

  for (insn = last_insn; insn != BB_END (bb); insn = prev)
    {
      prev = PREV_INSN (insn);
      if (GET_CODE (insn) == NOTE)
	switch (NOTE_LINE_NUMBER (insn))
	  {
	  case NOTE_INSN_LOOP_END:
	  case NOTE_INSN_BLOCK_END:
	  case NOTE_INSN_DELETED:
	  case NOTE_INSN_DELETED_LABEL:
	    continue;
	  default:
	    reorder_insns (insn, insn, last_insn);
	  }
    }

  return last_insn;
}

/* Locate or create a label for a given basic block.  */

static rtx
label_for_bb (basic_block bb)
{
  rtx label = BB_HEAD (bb);

  if (GET_CODE (label) != CODE_LABEL)
    {
      if (rtl_dump_file)
	fprintf (rtl_dump_file, "Emitting label for block %d\n", bb->index);

      label = block_label (bb);
    }

  return label;
}

/* Locate the effective beginning and end of the insn chain for each
   block, as defined by skip_insns_after_block above.  */

static void
record_effective_endpoints (void)
{
  rtx next_insn;
  basic_block bb;
  rtx insn;

  for (insn = get_insns ();
       insn
       && GET_CODE (insn) == NOTE
       && NOTE_LINE_NUMBER (insn) != NOTE_INSN_BASIC_BLOCK;
       insn = NEXT_INSN (insn))
    continue;
  if (!insn)
    abort ();  /* No basic blocks at all?  */
  if (PREV_INSN (insn))
    cfg_layout_function_header =
	    unlink_insn_chain (get_insns (), PREV_INSN (insn));
  else
    cfg_layout_function_header = NULL_RTX;

  next_insn = get_insns ();
  FOR_EACH_BB (bb)
    {
      rtx end;

      if (PREV_INSN (BB_HEAD (bb)) && next_insn != BB_HEAD (bb))
	bb->rbi->header = unlink_insn_chain (next_insn,
					      PREV_INSN (BB_HEAD (bb)));
      end = skip_insns_after_block (bb);
      if (NEXT_INSN (BB_END (bb)) && BB_END (bb) != end)
	bb->rbi->footer = unlink_insn_chain (NEXT_INSN (BB_END (bb)), end);
      next_insn = NEXT_INSN (BB_END (bb));
    }

  cfg_layout_function_footer = next_insn;
  if (cfg_layout_function_footer)
    cfg_layout_function_footer = unlink_insn_chain (cfg_layout_function_footer, get_last_insn ());
}

/* Data structures representing mapping of INSN_LOCATOR into scope blocks, line
   numbers and files.  In order to be GGC friendly we need to use separate
   varrays.  This also slightly improve the memory locality in binary search.
   The _locs array contains locators where the given property change.  The
   block_locators_blocks contains the scope block that is used for all insn
   locator greater than corresponding block_locators_locs value and smaller
   than the following one.  Similarly for the other properties.  */
static GTY(()) varray_type block_locators_locs;
static GTY(()) varray_type block_locators_blocks;
static GTY(()) varray_type line_locators_locs;
static GTY(()) varray_type line_locators_lines;
static GTY(()) varray_type file_locators_locs;
static GTY(()) varray_type file_locators_files;
int prologue_locator;
int epilogue_locator;

/* During the RTL expansion the lexical blocks and line numbers are
   represented via INSN_NOTEs.  Replace them by representation using
   INSN_LOCATORs.  */

void
insn_locators_initialize (void)
{
  tree block = NULL;
  tree last_block = NULL;
  rtx insn, next;
  int loc = 0;
  int line_number = 0, last_line_number = 0;
  char *file_name = NULL, *last_file_name = NULL;

  prologue_locator = epilogue_locator = 0;

  VARRAY_INT_INIT (block_locators_locs, 32, "block_locators_locs");
  VARRAY_TREE_INIT (block_locators_blocks, 32, "block_locators_blocks");
  VARRAY_INT_INIT (line_locators_locs, 32, "line_locators_locs");
  VARRAY_INT_INIT (line_locators_lines, 32, "line_locators_lines");
  VARRAY_INT_INIT (file_locators_locs, 32, "file_locators_locs");
  VARRAY_CHAR_PTR_INIT (file_locators_files, 32, "file_locators_files");

  for (insn = get_insns (); insn; insn = next)
    {
      next = NEXT_INSN (insn);

      if ((active_insn_p (insn)
	   && GET_CODE (PATTERN (insn)) != ADDR_VEC
	   && GET_CODE (PATTERN (insn)) != ADDR_DIFF_VEC)
	  || !NEXT_INSN (insn)
	  || (!prologue_locator && file_name))
	{
	  if (last_block != block)
	    {
	      loc++;
	      VARRAY_PUSH_INT (block_locators_locs, loc);
	      VARRAY_PUSH_TREE (block_locators_blocks, block);
	      last_block = block;
	    }
	  if (last_line_number != line_number)
	    {
	      loc++;
	      VARRAY_PUSH_INT (line_locators_locs, loc);
	      VARRAY_PUSH_INT (line_locators_lines, line_number);
	      last_line_number = line_number;
	    }
	  if (last_file_name != file_name)
	    {
	      loc++;
	      VARRAY_PUSH_INT (file_locators_locs, loc);
	      VARRAY_PUSH_CHAR_PTR (file_locators_files, file_name);
	      last_file_name = file_name;
	    }
	}
      if (!prologue_locator && file_name)
	prologue_locator = loc;
      if (!NEXT_INSN (insn))
	epilogue_locator = loc;
      if (active_insn_p (insn))
        INSN_LOCATOR (insn) = loc;
      else if (GET_CODE (insn) == NOTE)
	{
	  switch (NOTE_LINE_NUMBER (insn))
	    {
	    case NOTE_INSN_BLOCK_BEG:
	      block = NOTE_BLOCK (insn);
	      delete_insn (insn);
	      break;
	    case NOTE_INSN_BLOCK_END:
	      block = BLOCK_SUPERCONTEXT (block);
	      if (block && TREE_CODE (block) == FUNCTION_DECL)
		block = 0;
	      delete_insn (insn);
	      break;
	    default:
	      if (NOTE_LINE_NUMBER (insn) > 0)
		{
		  line_number = NOTE_LINE_NUMBER (insn);
		  file_name = (char *)NOTE_SOURCE_FILE (insn);
		}
	      break;
	    }
	}
    }

  /* Tag the blocks with a depth number so that change_scope can find
     the common parent easily.  */
  set_block_levels (DECL_INITIAL (cfun->decl), 0);
}

/* For each lexical block, set BLOCK_NUMBER to the depth at which it is
   found in the block tree.  */

static void
set_block_levels (tree block, int level)
{
  while (block)
    {
      BLOCK_NUMBER (block) = level;
      set_block_levels (BLOCK_SUBBLOCKS (block), level + 1);
      block = BLOCK_CHAIN (block);
    }
}

/* Return sope resulting from combination of S1 and S2.  */
tree
choose_inner_scope (tree s1, tree s2)
{
   if (!s1)
     return s2;
   if (!s2)
     return s1;
   if (BLOCK_NUMBER (s1) > BLOCK_NUMBER (s2))
     return s1;
   return s2;
}

/* Emit lexical block notes needed to change scope from S1 to S2.  */

static void
change_scope (rtx orig_insn, tree s1, tree s2)
{
  rtx insn = orig_insn;
  tree com = NULL_TREE;
  tree ts1 = s1, ts2 = s2;
  tree s;

  while (ts1 != ts2)
    {
      if (ts1 == NULL || ts2 == NULL)
	abort ();
      if (BLOCK_NUMBER (ts1) > BLOCK_NUMBER (ts2))
	ts1 = BLOCK_SUPERCONTEXT (ts1);
      else if (BLOCK_NUMBER (ts1) < BLOCK_NUMBER (ts2))
	ts2 = BLOCK_SUPERCONTEXT (ts2);
      else
	{
	  ts1 = BLOCK_SUPERCONTEXT (ts1);
	  ts2 = BLOCK_SUPERCONTEXT (ts2);
	}
    }
  com = ts1;

  /* Close scopes.  */
  s = s1;
  while (s != com)
    {
      rtx note = emit_note_before (NOTE_INSN_BLOCK_END, insn);
      NOTE_BLOCK (note) = s;
      s = BLOCK_SUPERCONTEXT (s);
    }

  /* Open scopes.  */
  s = s2;
  while (s != com)
    {
      insn = emit_note_before (NOTE_INSN_BLOCK_BEG, insn);
      NOTE_BLOCK (insn) = s;
      s = BLOCK_SUPERCONTEXT (s);
    }
}

/* Return lexical scope block insn belong to.  */
static tree
insn_scope (rtx insn)
{
  int max = VARRAY_ACTIVE_SIZE (block_locators_locs);
  int min = 0;
  int loc = INSN_LOCATOR (insn);

  /* When block_locators_locs was initialized, the pro- and epilogue
     insns didn't exist yet and can therefore not be found this way.
     But we know that they belong to the outer most block of the
     current function.
     Without this test, the prologue would be put inside the block of
     the first valid instruction in the function and when that first
     insn is part of an inlined function then the low_pc of that
     inlined function is messed up.  Likewise for the epilogue and
     the last valid instruction.  */
  if (loc == prologue_locator || loc == epilogue_locator)
    return DECL_INITIAL (cfun->decl);

  if (!max || !loc)
    return NULL;
  while (1)
    {
      int pos = (min + max) / 2;
      int tmp = VARRAY_INT (block_locators_locs, pos);

      if (tmp <= loc && min != pos)
	min = pos;
      else if (tmp > loc && max != pos)
	max = pos;
      else
	{
	  min = pos;
	  break;
	}
    }
   return VARRAY_TREE (block_locators_blocks, min);
}

/* Return line number of the statement specified by the locator.  */
int
locator_line (int loc)
{
  int max = VARRAY_ACTIVE_SIZE (line_locators_locs);
  int min = 0;

  if (!max || !loc)
    return 0;
  while (1)
    {
      int pos = (min + max) / 2;
      int tmp = VARRAY_INT (line_locators_locs, pos);

      if (tmp <= loc && min != pos)
	min = pos;
      else if (tmp > loc && max != pos)
	max = pos;
      else
	{
	  min = pos;
	  break;
	}
    }
   return VARRAY_INT (line_locators_lines, min);
}

/* Return line number of the statement that produced this insn.  */
int
insn_line (rtx insn)
{
  return locator_line (INSN_LOCATOR (insn));
}

/* Return source file of the statement specified by LOC.  */
const char *
locator_file (int loc)
{
  int max = VARRAY_ACTIVE_SIZE (file_locators_locs);
  int min = 0;

  if (!max || !loc)
    return NULL;
  while (1)
    {
      int pos = (min + max) / 2;
      int tmp = VARRAY_INT (file_locators_locs, pos);

      if (tmp <= loc && min != pos)
	min = pos;
      else if (tmp > loc && max != pos)
	max = pos;
      else
	{
	  min = pos;
	  break;
	}
    }
   return VARRAY_CHAR_PTR (file_locators_files, min);
}

/* Return source file of the statement that produced this insn.  */
const char *
insn_file (rtx insn)
{
  return locator_file (INSN_LOCATOR (insn));
}

/* Rebuild all the NOTE_INSN_BLOCK_BEG and NOTE_INSN_BLOCK_END notes based
   on the scope tree and the newly reordered instructions.  */

void
reemit_insn_block_notes (void)
{
  tree cur_block = DECL_INITIAL (cfun->decl);
  rtx insn, note;

  insn = get_insns ();
  if (!active_insn_p (insn))
    insn = next_active_insn (insn);
  for (; insn; insn = next_active_insn (insn))
    {
      tree this_block;

      this_block = insn_scope (insn);
      /* For sequences compute scope resulting from merging all scopes
         of instructions nested inside.  */
      if (GET_CODE (PATTERN (insn)) == SEQUENCE)
	{
	  int i;
	  rtx body = PATTERN (insn);

	  this_block = NULL;
	  for (i = 0; i < XVECLEN (body, 0); i++)
	    this_block = choose_inner_scope (this_block,
					 insn_scope (XVECEXP (body, 0, i)));
	}
      if (! this_block)
	continue;

      if (this_block != cur_block)
	{
	  change_scope (insn, cur_block, this_block);
	  cur_block = this_block;
	}
    }

  /* change_scope emits before the insn, not after.  */
  note = emit_note (NOTE_INSN_DELETED);
  change_scope (note, cur_block, DECL_INITIAL (cfun->decl));
  delete_insn (note);

  reorder_blocks ();
}

/* Given a reorder chain, rearrange the code to match.  */

static void
fixup_reorder_chain (void)
{
  basic_block bb, prev_bb;
  int index;
  rtx insn = NULL;

  if (cfg_layout_function_header)
    {
      set_first_insn (cfg_layout_function_header);
      insn = cfg_layout_function_header;
      while (NEXT_INSN (insn))
	insn = NEXT_INSN (insn);
    }

  /* First do the bulk reordering -- rechain the blocks without regard to
     the needed changes to jumps and labels.  */

  for (bb = ENTRY_BLOCK_PTR->next_bb, index = 0;
       bb != 0;
       bb = bb->rbi->next, index++)
    {
      if (bb->rbi->header)
	{
	  if (insn)
	    NEXT_INSN (insn) = bb->rbi->header;
	  else
	    set_first_insn (bb->rbi->header);
	  PREV_INSN (bb->rbi->header) = insn;
	  insn = bb->rbi->header;
	  while (NEXT_INSN (insn))
	    insn = NEXT_INSN (insn);
	}
      if (insn)
	NEXT_INSN (insn) = BB_HEAD (bb);
      else
	set_first_insn (BB_HEAD (bb));
      PREV_INSN (BB_HEAD (bb)) = insn;
      insn = BB_END (bb);
      if (bb->rbi->footer)
	{
	  NEXT_INSN (insn) = bb->rbi->footer;
	  PREV_INSN (bb->rbi->footer) = insn;
	  while (NEXT_INSN (insn))
	    insn = NEXT_INSN (insn);
	}
    }

  if (index != n_basic_blocks)
    abort ();

  NEXT_INSN (insn) = cfg_layout_function_footer;
  if (cfg_layout_function_footer)
    PREV_INSN (cfg_layout_function_footer) = insn;

  while (NEXT_INSN (insn))
    insn = NEXT_INSN (insn);

  set_last_insn (insn);
#ifdef ENABLE_CHECKING
  verify_insn_chain ();
#endif
  delete_dead_jumptables ();

  /* Now add jumps and labels as needed to match the blocks new
     outgoing edges.  */

  for (bb = ENTRY_BLOCK_PTR->next_bb; bb ; bb = bb->rbi->next)
    {
      edge e_fall, e_taken, e;
      rtx bb_end_insn;
      basic_block nb;

      if (bb->succ == NULL)
	continue;

      /* Find the old fallthru edge, and another non-EH edge for
	 a taken jump.  */
      e_taken = e_fall = NULL;
      for (e = bb->succ; e ; e = e->succ_next)
	if (e->flags & EDGE_FALLTHRU)
	  e_fall = e;
	else if (! (e->flags & EDGE_EH))
	  e_taken = e;

      bb_end_insn = BB_END (bb);
      if (GET_CODE (bb_end_insn) == JUMP_INSN)
	{
	  if (any_condjump_p (bb_end_insn))
	    {
	      /* If the old fallthru is still next, nothing to do.  */
	      if (bb->rbi->next == e_fall->dest
	          || (!bb->rbi->next
		      && e_fall->dest == EXIT_BLOCK_PTR))
		continue;

	      /* The degenerated case of conditional jump jumping to the next
		 instruction can happen on target having jumps with side
		 effects.

		 Create temporarily the duplicated edge representing branch.
		 It will get unidentified by force_nonfallthru_and_redirect
		 that would otherwise get confused by fallthru edge not pointing
		 to the next basic block.  */
	      if (!e_taken)
		{
		  rtx note;
		  edge e_fake;

		  e_fake = unchecked_make_edge (bb, e_fall->dest, 0);

		  if (!redirect_jump (BB_END (bb), block_label (bb), 0))
		    abort ();
		  note = find_reg_note (BB_END (bb), REG_BR_PROB, NULL_RTX);
		  if (note)
		    {
		      int prob = INTVAL (XEXP (note, 0));

		      e_fake->probability = prob;
		      e_fake->count = e_fall->count * prob / REG_BR_PROB_BASE;
		      e_fall->probability -= e_fall->probability;
		      e_fall->count -= e_fake->count;
		      if (e_fall->probability < 0)
			e_fall->probability = 0;
		      if (e_fall->count < 0)
			e_fall->count = 0;
		    }
		}
	      /* There is one special case: if *neither* block is next,
		 such as happens at the very end of a function, then we'll
		 need to add a new unconditional jump.  Choose the taken
		 edge based on known or assumed probability.  */
	      else if (bb->rbi->next != e_taken->dest)
		{
		  rtx note = find_reg_note (bb_end_insn, REG_BR_PROB, 0);

		  if (note
		      && INTVAL (XEXP (note, 0)) < REG_BR_PROB_BASE / 2
		      && invert_jump (bb_end_insn,
				      label_for_bb (e_fall->dest), 0))
		    {
		      e_fall->flags &= ~EDGE_FALLTHRU;
		      e_taken->flags |= EDGE_FALLTHRU;
		      update_br_prob_note (bb);
		      e = e_fall, e_fall = e_taken, e_taken = e;
		    }
		}

	      /* Otherwise we can try to invert the jump.  This will
		 basically never fail, however, keep up the pretense.  */
	      else if (invert_jump (bb_end_insn,
				    label_for_bb (e_fall->dest), 0))
		{
		  e_fall->flags &= ~EDGE_FALLTHRU;
		  e_taken->flags |= EDGE_FALLTHRU;
		  update_br_prob_note (bb);
		  continue;
		}
	    }
	  else if (returnjump_p (bb_end_insn))
	    continue;
	  else
	    {
	      /* Otherwise we have some switch or computed jump.  In the
		 99% case, there should not have been a fallthru edge.  */
	      if (! e_fall)
		continue;

#ifdef CASE_DROPS_THROUGH
	      /* Except for VAX.  Since we didn't have predication for the
		 tablejump, the fallthru block should not have moved.  */
	      if (bb->rbi->next == e_fall->dest)
		continue;
	      bb_end_insn = skip_insns_after_block (bb);
#else
	      abort ();
#endif
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
	  if (bb->rbi->next == e_fall->dest)
	    continue;

	  /* A fallthru to exit block.  */
	  if (!bb->rbi->next && e_fall->dest == EXIT_BLOCK_PTR)
	    continue;
	}

      /* We got here if we need to add a new jump insn.  */
      nb = force_nonfallthru (e_fall);
      if (nb)
	{
	  cfg_layout_initialize_rbi (nb);
	  nb->rbi->visited = 1;
	  nb->rbi->next = bb->rbi->next;
	  bb->rbi->next = nb;
	  /* Don't process this new block.  */
	  bb = nb;
	}
    }

  /* Put basic_block_info in the new order.  */

  if (rtl_dump_file)
    {
      fprintf (rtl_dump_file, "Reordered sequence:\n");
      for (bb = ENTRY_BLOCK_PTR->next_bb, index = 0; bb; bb = bb->rbi->next, index ++)
	{
	  fprintf (rtl_dump_file, " %i ", index);
	  if (bb->rbi->original)
	    fprintf (rtl_dump_file, "duplicate of %i ",
		     bb->rbi->original->index);
	  else if (forwarder_block_p (bb) && GET_CODE (BB_HEAD (bb)) != CODE_LABEL)
	    fprintf (rtl_dump_file, "compensation ");
	  else
	    fprintf (rtl_dump_file, "bb %i ", bb->index);
	  fprintf (rtl_dump_file, " [%i]\n", bb->frequency);
	}
    }

  prev_bb = ENTRY_BLOCK_PTR;
  bb = ENTRY_BLOCK_PTR->next_bb;
  index = 0;

  for (; bb; prev_bb = bb, bb = bb->rbi->next, index ++)
    {
      bb->index = index;
      BASIC_BLOCK (index) = bb;

      bb->prev_bb = prev_bb;
      prev_bb->next_bb = bb;
    }
  prev_bb->next_bb = EXIT_BLOCK_PTR;
  EXIT_BLOCK_PTR->prev_bb = prev_bb;

  /* Annoying special case - jump around dead jumptables left in the code.  */
  FOR_EACH_BB (bb)
    {
      edge e;
      for (e = bb->succ; e && !(e->flags & EDGE_FALLTHRU); e = e->succ_next)
	continue;
      if (e && !can_fallthru (e->src, e->dest))
	force_nonfallthru (e);
    }
}

/* Perform sanity checks on the insn chain.
   1. Check that next/prev pointers are consistent in both the forward and
      reverse direction.
   2. Count insns in chain, going both directions, and check if equal.
   3. Check that get_last_insn () returns the actual end of chain.  */

void
verify_insn_chain (void)
{
  rtx x, prevx, nextx;
  int insn_cnt1, insn_cnt2;

  for (prevx = NULL, insn_cnt1 = 1, x = get_insns ();
       x != 0;
       prevx = x, insn_cnt1++, x = NEXT_INSN (x))
    if (PREV_INSN (x) != prevx)
      abort ();

  if (prevx != get_last_insn ())
    abort ();

  for (nextx = NULL, insn_cnt2 = 1, x = get_last_insn ();
       x != 0;
       nextx = x, insn_cnt2++, x = PREV_INSN (x))
    if (NEXT_INSN (x) != nextx)
      abort ();

  if (insn_cnt1 != insn_cnt2)
    abort ();
}

/* The block falling through to exit must be the last one in the
   reordered chain.  Ensure that this condition is met.  */
static void
fixup_fallthru_exit_predecessor (void)
{
  edge e;
  basic_block bb = NULL;

  for (e = EXIT_BLOCK_PTR->pred; e; e = e->pred_next)
    if (e->flags & EDGE_FALLTHRU)
      bb = e->src;

  if (bb && bb->rbi->next)
    {
      basic_block c = ENTRY_BLOCK_PTR->next_bb;

      while (c->rbi->next != bb)
	c = c->rbi->next;

      c->rbi->next = bb->rbi->next;
      while (c->rbi->next)
	c = c->rbi->next;

      c->rbi->next = bb;
      bb->rbi->next = NULL;
    }
}

/* Return true in case it is possible to duplicate the basic block BB.  */

bool
cfg_layout_can_duplicate_bb_p (basic_block bb)
{
  edge s;

  if (bb == EXIT_BLOCK_PTR || bb == ENTRY_BLOCK_PTR)
    return false;

  /* Duplicating fallthru block to exit would require adding a jump
     and splitting the real last BB.  */
  for (s = bb->succ; s; s = s->succ_next)
    if (s->dest == EXIT_BLOCK_PTR && s->flags & EDGE_FALLTHRU)
       return false;

  /* Do not attempt to duplicate tablejumps, as we need to unshare
     the dispatch table.  This is difficult to do, as the instructions
     computing jump destination may be hoisted outside the basic block.  */
  if (tablejump_p (BB_END (bb), NULL, NULL))
    return false;

  /* Do not duplicate blocks containing insns that can't be copied.  */
  if (targetm.cannot_copy_insn_p)
    {
      rtx insn = BB_HEAD (bb);
      while (1)
	{
	  if (INSN_P (insn) && (*targetm.cannot_copy_insn_p) (insn))
	    return false;
	  if (insn == BB_END (bb))
	    break;
	  insn = NEXT_INSN (insn);
	}
    }

  return true;
}

static rtx
duplicate_insn_chain (rtx from, rtx to)
{
  rtx insn, last;

  /* Avoid updating of boundaries of previous basic block.  The
     note will get removed from insn stream in fixup.  */
  last = emit_note (NOTE_INSN_DELETED);

  /* Create copy at the end of INSN chain.  The chain will
     be reordered later.  */
  for (insn = from; insn != NEXT_INSN (to); insn = NEXT_INSN (insn))
    {
      switch (GET_CODE (insn))
	{
	case INSN:
	case CALL_INSN:
	case JUMP_INSN:
	  /* Avoid copying of dispatch tables.  We never duplicate
	     tablejumps, so this can hit only in case the table got
	     moved far from original jump.  */
	  if (GET_CODE (PATTERN (insn)) == ADDR_VEC
	      || GET_CODE (PATTERN (insn)) == ADDR_DIFF_VEC)
	    break;
	  emit_copy_of_insn_after (insn, get_last_insn ());
	  break;

	case CODE_LABEL:
	  break;

	case BARRIER:
	  emit_barrier ();
	  break;

	case NOTE:
	  switch (NOTE_LINE_NUMBER (insn))
	    {
	      /* In case prologue is empty and function contain label
	         in first BB, we may want to copy the block.  */
	    case NOTE_INSN_PROLOGUE_END:

	    case NOTE_INSN_LOOP_VTOP:
	    case NOTE_INSN_LOOP_CONT:
	    case NOTE_INSN_LOOP_BEG:
	    case NOTE_INSN_LOOP_END:
	      /* Strip down the loop notes - we don't really want to keep
	         them consistent in loop copies.  */
	    case NOTE_INSN_DELETED:
	    case NOTE_INSN_DELETED_LABEL:
	      /* No problem to strip these.  */
	    case NOTE_INSN_EPILOGUE_BEG:
	    case NOTE_INSN_FUNCTION_END:
	      /* Debug code expect these notes to exist just once.
	         Keep them in the master copy.
	         ??? It probably makes more sense to duplicate them for each
	         epilogue copy.  */
	    case NOTE_INSN_FUNCTION_BEG:
	      /* There is always just single entry to function.  */
	    case NOTE_INSN_BASIC_BLOCK:
	      break;

	      /* There is no purpose to duplicate prologue.  */
	    case NOTE_INSN_BLOCK_BEG:
	    case NOTE_INSN_BLOCK_END:
	      /* The BLOCK_BEG/BLOCK_END notes should be eliminated when BB
	         reordering is in the progress.  */
	    case NOTE_INSN_EH_REGION_BEG:
	    case NOTE_INSN_EH_REGION_END:
	      /* Should never exist at BB duplication time.  */
	      abort ();
	      break;
	    case NOTE_INSN_REPEATED_LINE_NUMBER:
	      emit_note_copy (insn);
	      break;

	    default:
	      if (NOTE_LINE_NUMBER (insn) < 0)
		abort ();
	      /* It is possible that no_line_number is set and the note
	         won't be emitted.  */
	      emit_note_copy (insn);
	    }
	  break;
	default:
	  abort ();
	}
    }
  insn = NEXT_INSN (last);
  delete_insn (last);
  return insn;
}
/* Create a duplicate of the basic block BB and redirect edge E into it.
   If E is not specified, BB is just copied, but updating the frequencies
   etc. is left to the caller.  */

basic_block
cfg_layout_duplicate_bb (basic_block bb, edge e)
{
  rtx insn;
  edge s, n;
  basic_block new_bb;
  gcov_type new_count = e ? e->count : 0;

  if (bb->count < new_count)
    new_count = bb->count;
  if (!bb->pred)
    abort ();
#ifdef ENABLE_CHECKING
  if (!cfg_layout_can_duplicate_bb_p (bb))
    abort ();
#endif

  insn = duplicate_insn_chain (BB_HEAD (bb), BB_END (bb));
  new_bb = create_basic_block (insn,
			       insn ? get_last_insn () : NULL,
			       EXIT_BLOCK_PTR->prev_bb);

  if (bb->rbi->header)
    {
      insn = bb->rbi->header;
      while (NEXT_INSN (insn))
	insn = NEXT_INSN (insn);
      insn = duplicate_insn_chain (bb->rbi->header, insn);
      if (insn)
	new_bb->rbi->header = unlink_insn_chain (insn, get_last_insn ());
    }

  if (bb->rbi->footer)
    {
      insn = bb->rbi->footer;
      while (NEXT_INSN (insn))
	insn = NEXT_INSN (insn);
      insn = duplicate_insn_chain (bb->rbi->footer, insn);
      if (insn)
	new_bb->rbi->footer = unlink_insn_chain (insn, get_last_insn ());
    }

  if (bb->global_live_at_start)
    {
      new_bb->global_live_at_start = OBSTACK_ALLOC_REG_SET (&flow_obstack);
      new_bb->global_live_at_end = OBSTACK_ALLOC_REG_SET (&flow_obstack);
      COPY_REG_SET (new_bb->global_live_at_start, bb->global_live_at_start);
      COPY_REG_SET (new_bb->global_live_at_end, bb->global_live_at_end);
    }

  new_bb->loop_depth = bb->loop_depth;
  new_bb->flags = bb->flags;
  for (s = bb->succ; s; s = s->succ_next)
    {
      /* Since we are creating edges from a new block to successors
	 of another block (which therefore are known to be disjoint), there
	 is no need to actually check for duplicated edges.  */
      n = unchecked_make_edge (new_bb, s->dest, s->flags);
      n->probability = s->probability;
      if (e && bb->count)
	{
	  /* Take care for overflows!  */
	  n->count = s->count * (new_count * 10000 / bb->count) / 10000;
	  s->count -= n->count;
	}
      else
	n->count = s->count;
      n->aux = s->aux;
    }

  if (e)
    {
      new_bb->count = new_count;
      bb->count -= new_count;

      new_bb->frequency = EDGE_FREQUENCY (e);
      bb->frequency -= EDGE_FREQUENCY (e);

      redirect_edge_and_branch_force (e, new_bb);

      if (bb->count < 0)
	bb->count = 0;
      if (bb->frequency < 0)
	bb->frequency = 0;
    }
  else
    {
      new_bb->count = bb->count;
      new_bb->frequency = bb->frequency;
    }

  new_bb->rbi->original = bb;
  bb->rbi->copy = new_bb;

  return new_bb;
}

void
cfg_layout_initialize_rbi (basic_block bb)
{
  if (bb->rbi)
    abort ();
  bb->rbi = pool_alloc (cfg_layout_pool);
  memset (bb->rbi, 0, sizeof (struct reorder_block_def));
}

/* Main entry point to this module - initialize the datastructures for
   CFG layout changes.  It keeps LOOPS up-to-date if not null.

   FLAGS is a set of additional flags to pass to cleanup_cfg().  It should
   include CLEANUP_UPDATE_LIFE if liveness information must be kept up
   to date.  */

void
cfg_layout_initialize (unsigned int flags)
{
  basic_block bb;

  /* Our algorithm depends on fact that there are now dead jumptables
     around the code.  */
  cfg_layout_pool =
    create_alloc_pool ("cfg layout pool", sizeof (struct reorder_block_def),
		       n_basic_blocks + 2);
  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
    cfg_layout_initialize_rbi (bb);

  cfg_layout_rtl_register_cfg_hooks ();

  record_effective_endpoints ();

  cleanup_cfg (CLEANUP_CFGLAYOUT | flags);
}

/* Splits superblocks.  */
static void
break_superblocks (void)
{
  sbitmap superblocks;
  int i, need;

  superblocks = sbitmap_alloc (n_basic_blocks);
  sbitmap_zero (superblocks);

  need = 0;

  for (i = 0; i < n_basic_blocks; i++)
    if (BASIC_BLOCK(i)->flags & BB_SUPERBLOCK)
      {
	BASIC_BLOCK(i)->flags &= ~BB_SUPERBLOCK;
	SET_BIT (superblocks, i);
	need = 1;
      }

  if (need)
    {
      rebuild_jump_labels (get_insns ());
      find_many_sub_basic_blocks (superblocks);
    }

  free (superblocks);
}

/* Finalize the changes: reorder insn list according to the sequence, enter
   compensation code, rebuild scope forest.  */

void
cfg_layout_finalize (void)
{
  basic_block bb;

#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif
  rtl_register_cfg_hooks ();
  fixup_fallthru_exit_predecessor ();
  fixup_reorder_chain ();

#ifdef ENABLE_CHECKING
  verify_insn_chain ();
#endif

  free_alloc_pool (cfg_layout_pool);
  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
    bb->rbi = NULL;

  break_superblocks ();

#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif
}

/* Checks whether all N blocks in BBS array can be copied.  */
bool
can_copy_bbs_p (basic_block *bbs, unsigned n)
{
  unsigned i;
  edge e;
  int ret = true;

  for (i = 0; i < n; i++)
    bbs[i]->rbi->duplicated = 1;

  for (i = 0; i < n; i++)
    {
      /* In case we should redirect abnormal edge during duplication, fail.  */
      for (e = bbs[i]->succ; e; e = e->succ_next)
	if ((e->flags & EDGE_ABNORMAL)
	    && e->dest->rbi->duplicated)
	  {
	    ret = false;
	    goto end;
	  }

      if (!cfg_layout_can_duplicate_bb_p (bbs[i]))
	{
	  ret = false;
	  break;
	}
    }

end:
  for (i = 0; i < n; i++)
    bbs[i]->rbi->duplicated = 0;

  return ret;
}

/* Duplicates N basic blocks stored in array BBS.  Newly created basic blocks
   are placed into array NEW_BBS in the same order.  Edges from basic blocks
   in BBS are also duplicated and copies of those of them
   that lead into BBS are redirected to appropriate newly created block.  The
   function assigns bbs into loops (copy of basic block bb is assigned to
   bb->loop_father->copy loop, so this must be set up correctly in advance)
   and updates dominators locally (LOOPS structure that contains the information
   about dominators is passed to enable this).

   BASE is the superloop to that basic block belongs; if its header or latch
   is copied, we do not set the new blocks as header or latch.

   Created copies of N_EDGES edges in array EDGES are stored in array NEW_EDGES,
   also in the same order.  */

void
copy_bbs (basic_block *bbs, unsigned n, basic_block *new_bbs,
	  edge *edges, unsigned n_edges, edge *new_edges,
	  struct loop *base)
{
  unsigned i, j;
  basic_block bb, new_bb, dom_bb;
  edge e;

  /* Duplicate bbs, update dominators, assign bbs to loops.  */
  for (i = 0; i < n; i++)
    {
      /* Duplicate.  */
      bb = bbs[i];
      new_bb = new_bbs[i] = cfg_layout_duplicate_bb (bb, NULL);
      bb->rbi->duplicated = 1;
      /* Add to loop.  */
      add_bb_to_loop (new_bb, bb->loop_father->copy);
      add_to_dominance_info (CDI_DOMINATORS, new_bb);
      /* Possibly set header.  */
      if (bb->loop_father->header == bb && bb->loop_father != base)
	new_bb->loop_father->header = new_bb;
      /* Or latch.  */
      if (bb->loop_father->latch == bb && bb->loop_father != base)
	new_bb->loop_father->latch = new_bb;
    }

  /* Set dominators.  */
  for (i = 0; i < n; i++)
    {
      bb = bbs[i];
      new_bb = new_bbs[i];

      dom_bb = get_immediate_dominator (CDI_DOMINATORS, bb);
      if (dom_bb->rbi->duplicated)
	{
	  dom_bb = dom_bb->rbi->copy;
	  set_immediate_dominator (CDI_DOMINATORS, new_bb, dom_bb);
	}
    }

  /* Redirect edges.  */
  for (j = 0; j < n_edges; j++)
    new_edges[j] = NULL;
  for (i = 0; i < n; i++)
    {
      new_bb = new_bbs[i];
      bb = bbs[i];

      for (e = new_bb->succ; e; e = e->succ_next)
	{
	  for (j = 0; j < n_edges; j++)
	    if (edges[j] && edges[j]->src == bb && edges[j]->dest == e->dest)
	      new_edges[j] = e;

	  if (!e->dest->rbi->duplicated)
	    continue;
	  redirect_edge_and_branch_force (e, e->dest->rbi->copy);
	}
    }

  /* Clear information about duplicates.  */
  for (i = 0; i < n; i++)
    bbs[i]->rbi->duplicated = 0;
}

#include "gt-cfglayout.h"
