/* Basic block reordering routines for the GNU compiler.
   Copyright (C) 2000, 2001, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.

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
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "hard-reg-set.h"
#include "obstack.h"
#include "basic-block.h"
#include "insn-config.h"
#include "output.h"
#include "function.h"
#include "cfglayout.h"
#include "cfgloop.h"
#include "target.h"
#include "ggc.h"
#include "alloc-pool.h"
#include "flags.h"
#include "tree-pass.h"
#include "df.h"
#include "vecprim.h"

/* Holds the interesting trailing notes for the function.  */
rtx cfg_layout_function_footer;
rtx cfg_layout_function_header;

static rtx skip_insns_after_block (basic_block);
static void record_effective_endpoints (void);
static rtx label_for_bb (basic_block);
static void fixup_reorder_chain (void);

static void change_scope (rtx, tree, tree);

void verify_insn_chain (void);
static void fixup_fallthru_exit_predecessor (void);
static tree insn_scope (const_rtx);

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
  rtx next_insn;
  basic_block bb;
  rtx insn;

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
    cfg_layout_function_header = NULL_RTX;

  next_insn = get_insns ();
  FOR_EACH_BB (bb)
    {
      rtx end;

      if (PREV_INSN (BB_HEAD (bb)) && next_insn != BB_HEAD (bb))
	bb->il.rtl->header = unlink_insn_chain (next_insn,
					      PREV_INSN (BB_HEAD (bb)));
      end = skip_insns_after_block (bb);
      if (NEXT_INSN (BB_END (bb)) && BB_END (bb) != end)
	bb->il.rtl->footer = unlink_insn_chain (NEXT_INSN (BB_END (bb)), end);
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
static VEC(int,heap) *block_locators_locs;
static GTY(()) VEC(tree,gc) *block_locators_blocks;
static VEC(int,heap) *locations_locators_locs;
DEF_VEC_O(location_t);
DEF_VEC_ALLOC_O(location_t,heap);
static VEC(location_t,heap) *locations_locators_vals;
int prologue_locator;
int epilogue_locator;

/* Hold current location information and last location information, so the
   datastructures are built lazily only when some instructions in given
   place are needed.  */
static location_t curr_location, last_location;
static tree curr_block, last_block;
static int curr_rtl_loc = -1;

/* Allocate insn locator datastructure.  */
void
insn_locators_alloc (void)
{
  prologue_locator = epilogue_locator = 0;

  block_locators_locs = VEC_alloc (int, heap, 32);
  block_locators_blocks = VEC_alloc (tree, gc, 32);
  locations_locators_locs = VEC_alloc (int, heap, 32);
  locations_locators_vals = VEC_alloc (location_t, heap, 32);

  last_location = -1;
  curr_location = -1;
  curr_block = NULL;
  last_block = NULL;
  curr_rtl_loc = 0;
}

/* At the end of emit stage, clear current location.  */
void
insn_locators_finalize (void)
{
  if (curr_rtl_loc >= 0)
    epilogue_locator = curr_insn_locator ();
  curr_rtl_loc = -1;
}

/* Allocate insn locator datastructure.  */
void
insn_locators_free (void)
{
  prologue_locator = epilogue_locator = 0;

  VEC_free (int, heap, block_locators_locs);
  VEC_free (tree,gc, block_locators_blocks);
  VEC_free (int, heap, locations_locators_locs);
  VEC_free (location_t, heap, locations_locators_vals);
}


/* Set current location.  */
void
set_curr_insn_source_location (location_t location)
{
  /* IV opts calls into RTL expansion to compute costs of operations.  At this
     time locators are not initialized.  */
  if (curr_rtl_loc == -1)
    return;
  curr_location = location;
}

/* Get current location.  */
location_t
get_curr_insn_source_location (void)
{
  return curr_location;
}

/* Set current scope block.  */
void
set_curr_insn_block (tree b)
{
  /* IV opts calls into RTL expansion to compute costs of operations.  At this
     time locators are not initialized.  */
  if (curr_rtl_loc == -1)
    return;
  if (b)
    curr_block = b;
}

/* Get current scope block.  */
tree
get_curr_insn_block (void)
{
  return curr_block;
}

/* Return current insn locator.  */
int
curr_insn_locator (void)
{
  if (curr_rtl_loc == -1)
    return 0;
  if (last_block != curr_block)
    {
      curr_rtl_loc++;
      VEC_safe_push (int, heap, block_locators_locs, curr_rtl_loc);
      VEC_safe_push (tree, gc, block_locators_blocks, curr_block);
      last_block = curr_block;
    }
  if (last_location != curr_location)
    {
      curr_rtl_loc++;
      VEC_safe_push (int, heap, locations_locators_locs, curr_rtl_loc);
      VEC_safe_push (location_t, heap, locations_locators_vals, &curr_location);
      last_location = curr_location;
    }
  return curr_rtl_loc;
}

static unsigned int
into_cfg_layout_mode (void)
{
  cfg_layout_initialize (0);
  return 0;
}

static unsigned int
outof_cfg_layout_mode (void)
{
  basic_block bb;

  FOR_EACH_BB (bb)
    if (bb->next_bb != EXIT_BLOCK_PTR)
      bb->aux = bb->next_bb;

  cfg_layout_finalize ();

  return 0;
}

struct rtl_opt_pass pass_into_cfg_layout_mode =
{
 {
  RTL_PASS,
  "into_cfglayout",                     /* name */
  NULL,                                 /* gate */
  into_cfg_layout_mode,                 /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_NONE,                              /* tv_id */
  0,                                    /* properties_required */
  PROP_cfglayout,                       /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_dump_func,                       /* todo_flags_finish */
 }
};

struct rtl_opt_pass pass_outof_cfg_layout_mode =
{
 {
  RTL_PASS,
  "outof_cfglayout",                    /* name */
  NULL,                                 /* gate */
  outof_cfg_layout_mode,                /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_NONE,                              /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  PROP_cfglayout,                       /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_dump_func,                       /* todo_flags_finish */
 }
};

/* Return scope resulting from combination of S1 and S2.  */
static tree
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
      gcc_assert (ts1 && ts2);
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

/* Return lexical scope block locator belongs to.  */
static tree
locator_scope (int loc)
{
  int max = VEC_length (int, block_locators_locs);
  int min = 0;

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
      int tmp = VEC_index (int, block_locators_locs, pos);

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
  return VEC_index (tree, block_locators_blocks, min);
}

/* Return lexical scope block insn belongs to.  */
static tree
insn_scope (const_rtx insn)
{
  return locator_scope (INSN_LOCATOR (insn));
}

/* Return line number of the statement specified by the locator.  */
location_t
locator_location (int loc)
{
  int max = VEC_length (int, locations_locators_locs);
  int min = 0;

  while (1)
    {
      int pos = (min + max) / 2;
      int tmp = VEC_index (int, locations_locators_locs, pos);

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
  return *VEC_index (location_t, locations_locators_vals, min);
}

/* Return source line of the statement that produced this insn.  */
int
locator_line (int loc)
{
  expanded_location xloc;
  if (!loc)
    return 0;
  else
    xloc = expand_location (locator_location (loc));
  return xloc.line;
}

/* Return line number of the statement that produced this insn.  */
int
insn_line (const_rtx insn)
{
  return locator_line (INSN_LOCATOR (insn));
}

/* Return source file of the statement specified by LOC.  */
const char *
locator_file (int loc)
{
  expanded_location xloc;
  if (!loc)
    return 0;
  else
    xloc = expand_location (locator_location (loc));
  return xloc.file;
}

/* Return source file of the statement that produced this insn.  */
const char *
insn_file (const_rtx insn)
{
  return locator_file (INSN_LOCATOR (insn));
}

/* Return true if LOC1 and LOC2 locators have the same location and scope.  */
bool
locator_eq (int loc1, int loc2)
{
  if (loc1 == loc2)
    return true;
  if (locator_location (loc1) != locator_location (loc2))
    return false;
  return locator_scope (loc1) == locator_scope (loc2);
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

      /* Avoid putting scope notes between jump table and its label.  */
      if (JUMP_TABLE_DATA_P (insn))
	continue;

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


/* Link the basic blocks in the correct order, compacting the basic
   block queue while at it.  This also clears the visited flag on
   all basic blocks.  If STAY_IN_CFGLAYOUT_MODE is false, this function
   also clears the basic block header and footer fields.

   This function is usually called after a pass (e.g. tracer) finishes
   some transformations while in cfglayout mode.  The required sequence
   of the basic blocks is in a linked list along the bb->aux field.
   This functions re-links the basic block prev_bb and next_bb pointers
   accordingly, and it compacts and renumbers the blocks.  */

void
relink_block_chain (bool stay_in_cfglayout_mode)
{
  basic_block bb, prev_bb;
  int index;

  /* Maybe dump the re-ordered sequence.  */
  if (dump_file)
    {
      fprintf (dump_file, "Reordered sequence:\n");
      for (bb = ENTRY_BLOCK_PTR->next_bb, index = NUM_FIXED_BLOCKS;
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
	  fprintf (dump_file, " [%i]\n", bb->frequency);
	}
    }

  /* Now reorder the blocks.  */
  prev_bb = ENTRY_BLOCK_PTR;
  bb = ENTRY_BLOCK_PTR->next_bb;
  for (; bb; prev_bb = bb, bb = (basic_block) bb->aux)
    {
      bb->prev_bb = prev_bb;
      prev_bb->next_bb = bb;
    }
  prev_bb->next_bb = EXIT_BLOCK_PTR;
  EXIT_BLOCK_PTR->prev_bb = prev_bb;

  /* Then, clean up the aux and visited fields.  */
  FOR_ALL_BB (bb)
    {
      bb->aux = NULL;
      bb->il.rtl->visited = 0;
      if (!stay_in_cfglayout_mode)
	bb->il.rtl->header = bb->il.rtl->footer = NULL;
    }

  /* Maybe reset the original copy tables, they are not valid anymore
     when we renumber the basic blocks in compact_blocks.  If we are
     are going out of cfglayout mode, don't re-allocate the tables.  */
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

  for (bb = ENTRY_BLOCK_PTR->next_bb; bb; bb = (basic_block) bb->aux)
    {
      if (bb->il.rtl->header)
	{
	  if (insn)
	    NEXT_INSN (insn) = bb->il.rtl->header;
	  else
	    set_first_insn (bb->il.rtl->header);
	  PREV_INSN (bb->il.rtl->header) = insn;
	  insn = bb->il.rtl->header;
	  while (NEXT_INSN (insn))
	    insn = NEXT_INSN (insn);
	}
      if (insn)
	NEXT_INSN (insn) = BB_HEAD (bb);
      else
	set_first_insn (BB_HEAD (bb));
      PREV_INSN (BB_HEAD (bb)) = insn;
      insn = BB_END (bb);
      if (bb->il.rtl->footer)
	{
	  NEXT_INSN (insn) = bb->il.rtl->footer;
	  PREV_INSN (bb->il.rtl->footer) = insn;
	  while (NEXT_INSN (insn))
	    insn = NEXT_INSN (insn);
	}
    }

  NEXT_INSN (insn) = cfg_layout_function_footer;
  if (cfg_layout_function_footer)
    PREV_INSN (cfg_layout_function_footer) = insn;

  while (NEXT_INSN (insn))
    insn = NEXT_INSN (insn);

  set_last_insn (insn);
#ifdef ENABLE_CHECKING
  verify_insn_chain ();
#endif

  /* Now add jumps and labels as needed to match the blocks new
     outgoing edges.  */

  for (bb = ENTRY_BLOCK_PTR->next_bb; bb ; bb = (basic_block) bb->aux)
    {
      edge e_fall, e_taken, e;
      rtx bb_end_insn;
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
      if (JUMP_P (bb_end_insn))
	{
	  if (any_condjump_p (bb_end_insn))
	    {
	      /* This might happen if the conditional jump has side
		 effects and could therefore not be optimized away.
		 Make the basic block to end with a barrier in order
		 to prevent rtl_verify_flow_info from complaining.  */
	      if (!e_fall)
		{
		  gcc_assert (!onlyjump_p (bb_end_insn)
			      || returnjump_p (bb_end_insn));
		  bb->il.rtl->footer = emit_barrier_after (bb_end_insn);
		  continue;
		}

	      /* If the old fallthru is still next, nothing to do.  */
	      if (bb->aux == e_fall->dest
		  || e_fall->dest == EXIT_BLOCK_PTR)
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
		  rtx note = find_reg_note (bb_end_insn, REG_BR_PROB, 0);

		  if (note
		      && INTVAL (XEXP (note, 0)) < REG_BR_PROB_BASE / 2
		      && invert_jump (bb_end_insn,
				      (e_fall->dest == EXIT_BLOCK_PTR
				       ? NULL_RTX
				       : label_for_bb (e_fall->dest)), 0))
		    {
		      e_fall->flags &= ~EDGE_FALLTHRU;
#ifdef ENABLE_CHECKING
		      gcc_assert (could_fall_through
				  (e_taken->src, e_taken->dest));
#endif
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
	      else if (invert_jump (bb_end_insn,
				    (e_fall->dest == EXIT_BLOCK_PTR
				     ? NULL_RTX
				     : label_for_bb (e_fall->dest)), 0))
		{
		  e_fall->flags &= ~EDGE_FALLTHRU;
#ifdef ENABLE_CHECKING
		  gcc_assert (could_fall_through
			      (e_taken->src, e_taken->dest));
#endif
		  e_taken->flags |= EDGE_FALLTHRU;
		  update_br_prob_note (bb);
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
		  || e_fall->dest == EXIT_BLOCK_PTR)
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
	  if (e_fall->dest == EXIT_BLOCK_PTR)
	    continue;
	}

      /* We got here if we need to add a new jump insn.  */
      nb = force_nonfallthru (e_fall);
      if (nb)
	{
	  nb->il.rtl->visited = 1;
	  nb->aux = bb->aux;
	  bb->aux = nb;
	  /* Don't process this new block.  */
	  bb = nb;

	  /* Make sure new bb is tagged for correct section (same as
	     fall-thru source, since you cannot fall-throu across
	     section boundaries).  */
	  BB_COPY_PARTITION (e_fall->src, single_pred (bb));
	  if (flag_reorder_blocks_and_partition
	      && targetm.have_named_sections
	      && JUMP_P (BB_END (bb))
	      && !any_condjump_p (BB_END (bb))
	      && (EDGE_SUCC (bb, 0)->flags & EDGE_CROSSING))
	    add_reg_note (BB_END (bb), REG_CROSSING_JUMP, NULL_RTX);
	}
    }

  relink_block_chain (/*stay_in_cfglayout_mode=*/false);

  /* Annoying special case - jump around dead jumptables left in the code.  */
  FOR_EACH_BB (bb)
    {
      edge e;
      edge_iterator ei;

      FOR_EACH_EDGE (e, ei, bb->succs)
	if (e->flags & EDGE_FALLTHRU)
	  break;

      if (e && !can_fallthru (e->src, e->dest))
	force_nonfallthru (e);
    }

  /* Ensure goto_locus from edges has some instructions with that locus
     in RTL.  */
  if (!optimize)
    FOR_EACH_BB (bb)
      {
        edge e;
        edge_iterator ei;

        FOR_EACH_EDGE (e, ei, bb->succs)
	  if (e->goto_locus && !(e->flags & EDGE_ABNORMAL))
	    {
	      basic_block nb;
	      rtx end;

	      insn = BB_END (e->src);
	      end = PREV_INSN (BB_HEAD (e->src));
	      while (insn != end
		     && (!INSN_P (insn) || INSN_LOCATOR (insn) == 0))
		insn = PREV_INSN (insn);
	      if (insn != end
		  && locator_eq (INSN_LOCATOR (insn), (int) e->goto_locus))
		continue;
	      if (simplejump_p (BB_END (e->src))
		  && INSN_LOCATOR (BB_END (e->src)) == 0)
		{
		  INSN_LOCATOR (BB_END (e->src)) = e->goto_locus;
		  continue;
		}
	      if (e->dest != EXIT_BLOCK_PTR)
		{
		  insn = BB_HEAD (e->dest);
		  end = NEXT_INSN (BB_END (e->dest));
		  while (insn != end && !INSN_P (insn))
		    insn = NEXT_INSN (insn);
		  if (insn != end && INSN_LOCATOR (insn)
		      && locator_eq (INSN_LOCATOR (insn), (int) e->goto_locus))
		    continue;
		}
	      nb = split_edge (e);
	      if (!INSN_P (BB_END (nb)))
		BB_END (nb) = emit_insn_after_noloc (gen_nop (), BB_END (nb),
						     nb);
	      INSN_LOCATOR (BB_END (nb)) = e->goto_locus;
	    }
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
  edge_iterator ei;
  basic_block bb = NULL;

  /* This transformation is not valid before reload, because we might
     separate a call from the instruction that copies the return
     value.  */
  gcc_assert (reload_completed);

  FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR->preds)
    if (e->flags & EDGE_FALLTHRU)
      bb = e->src;

  if (bb && bb->aux)
    {
      basic_block c = ENTRY_BLOCK_PTR->next_bb;

      /* If the very first block is the one with the fall-through exit
	 edge, we have to split that block.  */
      if (c == bb)
	{
	  bb = split_block (bb, NULL)->dest;
	  bb->aux = c->aux;
	  c->aux = bb;
	  bb->il.rtl->footer = c->il.rtl->footer;
	  c->il.rtl->footer = NULL;
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

  FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR->preds)
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
  for (ei = ei_start (EXIT_BLOCK_PTR->preds); (e = ei_safe_edge (ei)); )
    {
      if (e->src == forwarder
	  || !(e->flags & EDGE_FALLTHRU))
	ei_next (&ei);
      else
	redirect_edge_and_branch_force (e, forwarder);
    }

  /* Fix up the chain of blocks -- make FORWARDER immediately precede the
     exit block.  */
  FOR_EACH_BB (bb)
    {
      if (bb->aux == NULL && bb != forwarder)
	{
	  bb->aux = forwarder;
	  break;
	}
    }
}

/* Return true in case it is possible to duplicate the basic block BB.  */

/* We do not want to declare the function in a header file, since it should
   only be used through the cfghooks interface, and we do not want to move
   it to cfgrtl.c since it would require also moving quite a lot of related
   code.  */
extern bool cfg_layout_can_duplicate_bb_p (const_basic_block);

bool
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
      rtx insn = BB_HEAD (bb);
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

rtx
duplicate_insn_chain (rtx from, rtx to)
{
  rtx insn, last, copy;

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
	case INSN:
	case CALL_INSN:
	case JUMP_INSN:
	  /* Avoid copying of dispatch tables.  We never duplicate
	     tablejumps, so this can hit only in case the table got
	     moved far from original jump.  */
	  if (GET_CODE (PATTERN (insn)) == ADDR_VEC
	      || GET_CODE (PATTERN (insn)) == ADDR_DIFF_VEC)
	    {
	      /* Avoid copying following barrier as well if any
		 (and debug insns in between).  */
	      rtx next;

	      for (next = NEXT_INSN (insn);
		   next != NEXT_INSN (to);
		   next = NEXT_INSN (next))
		if (!DEBUG_INSN_P (next))
		  break;
	      if (next != NEXT_INSN (to) && BARRIER_P (next))
		insn = next;
	      break;
	    }
	  copy = emit_copy_of_insn_after (insn, get_last_insn ());
          maybe_copy_epilogue_insn (insn, copy);
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
	      /* No problem to strip these.  */
	    case NOTE_INSN_FUNCTION_BEG:
	      /* There is always just single entry to function.  */
	    case NOTE_INSN_BASIC_BLOCK:
	      break;

	    case NOTE_INSN_EPILOGUE_BEG:
	    case NOTE_INSN_SWITCH_TEXT_SECTIONS:
	      emit_note_copy (insn);
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

/* We do not want to declare the function in a header file, since it should
   only be used through the cfghooks interface, and we do not want to move
   it to cfgrtl.c since it would require also moving quite a lot of related
   code.  */
extern basic_block cfg_layout_duplicate_bb (basic_block);

basic_block
cfg_layout_duplicate_bb (basic_block bb)
{
  rtx insn;
  basic_block new_bb;

  insn = duplicate_insn_chain (BB_HEAD (bb), BB_END (bb));
  new_bb = create_basic_block (insn,
			       insn ? get_last_insn () : NULL,
			       EXIT_BLOCK_PTR->prev_bb);

  BB_COPY_PARTITION (new_bb, bb);
  if (bb->il.rtl->header)
    {
      insn = bb->il.rtl->header;
      while (NEXT_INSN (insn))
	insn = NEXT_INSN (insn);
      insn = duplicate_insn_chain (bb->il.rtl->header, insn);
      if (insn)
	new_bb->il.rtl->header = unlink_insn_chain (insn, get_last_insn ());
    }

  if (bb->il.rtl->footer)
    {
      insn = bb->il.rtl->footer;
      while (NEXT_INSN (insn))
	insn = NEXT_INSN (insn);
      insn = duplicate_insn_chain (bb->il.rtl->footer, insn);
      if (insn)
	new_bb->il.rtl->footer = unlink_insn_chain (insn, get_last_insn ());
    }

  return new_bb;
}


/* Main entry point to this module - initialize the datastructures for
   CFG layout changes.  It keeps LOOPS up-to-date if not null.

   FLAGS is a set of additional flags to pass to cleanup_cfg().  */

void
cfg_layout_initialize (unsigned int flags)
{
  rtx x;
  basic_block bb;

  initialize_original_copy_tables ();

  cfg_layout_rtl_register_cfg_hooks ();

  record_effective_endpoints ();

  /* Make sure that the targets of non local gotos are marked.  */
  for (x = nonlocal_goto_handler_labels; x; x = XEXP (x, 1))
    {
      bb = BLOCK_FOR_INSN (XEXP (x, 0));
      bb->flags |= BB_NON_LOCAL_GOTO_TARGET;
    }

  cleanup_cfg (CLEANUP_CFGLAYOUT | flags);
}

/* Splits superblocks.  */
void
break_superblocks (void)
{
  sbitmap superblocks;
  bool need = false;
  basic_block bb;

  superblocks = sbitmap_alloc (last_basic_block);
  sbitmap_zero (superblocks);

  FOR_EACH_BB (bb)
    if (bb->flags & BB_SUPERBLOCK)
      {
	bb->flags &= ~BB_SUPERBLOCK;
	SET_BIT (superblocks, bb->index);
	need = true;
      }

  if (need)
    {
      rebuild_jump_labels (get_insns ());
      find_many_sub_basic_blocks (superblocks);
    }

  free (superblocks);
}

/* Finalize the changes: reorder insn list according to the sequence specified
   by aux pointers, enter compensation code, rebuild scope forest.  */

void
cfg_layout_finalize (void)
{
#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif
  force_one_exit_fallthru ();
  rtl_register_cfg_hooks ();
  if (reload_completed
#ifdef HAVE_epilogue
      && !HAVE_epilogue
#endif
      )
    fixup_fallthru_exit_predecessor ();
  fixup_reorder_chain ();

  rebuild_jump_labels (get_insns ());
  delete_dead_jumptables ();

#ifdef ENABLE_CHECKING
  verify_insn_chain ();
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
    bbs[i]->flags |= BB_DUPLICATED;

  for (i = 0; i < n; i++)
    {
      /* In case we should redirect abnormal edge during duplication, fail.  */
      edge_iterator ei;
      FOR_EACH_EDGE (e, ei, bbs[i]->succs)
	if ((e->flags & EDGE_ABNORMAL)
	    && (e->dest->flags & BB_DUPLICATED))
	  {
	    ret = false;
	    goto end;
	  }

      if (!can_duplicate_block_p (bbs[i]))
	{
	  ret = false;
	  break;
	}
    }

end:
  for (i = 0; i < n; i++)
    bbs[i]->flags &= ~BB_DUPLICATED;

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
   also in the same order.

   Newly created basic blocks are put after the basic block AFTER in the
   instruction stream, and the order of the blocks in BBS array is preserved.  */

void
copy_bbs (basic_block *bbs, unsigned n, basic_block *new_bbs,
	  edge *edges, unsigned num_edges, edge *new_edges,
	  struct loop *base, basic_block after)
{
  unsigned i, j;
  basic_block bb, new_bb, dom_bb;
  edge e;

  /* Duplicate bbs, update dominators, assign bbs to loops.  */
  for (i = 0; i < n; i++)
    {
      /* Duplicate.  */
      bb = bbs[i];
      new_bb = new_bbs[i] = duplicate_block (bb, NULL, after);
      after = new_bb;
      bb->flags |= BB_DUPLICATED;
      /* Possibly set loop header.  */
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
      if (dom_bb->flags & BB_DUPLICATED)
	{
	  dom_bb = get_bb_copy (dom_bb);
	  set_immediate_dominator (CDI_DOMINATORS, new_bb, dom_bb);
	}
    }

  /* Redirect edges.  */
  for (j = 0; j < num_edges; j++)
    new_edges[j] = NULL;
  for (i = 0; i < n; i++)
    {
      edge_iterator ei;
      new_bb = new_bbs[i];
      bb = bbs[i];

      FOR_EACH_EDGE (e, ei, new_bb->succs)
	{
	  for (j = 0; j < num_edges; j++)
	    if (edges[j] && edges[j]->src == bb && edges[j]->dest == e->dest)
	      new_edges[j] = e;

	  if (!(e->dest->flags & BB_DUPLICATED))
	    continue;
	  redirect_edge_and_branch_force (e, get_bb_copy (e->dest));
	}
    }

  /* Clear information about duplicates.  */
  for (i = 0; i < n; i++)
    bbs[i]->flags &= ~BB_DUPLICATED;
}

#include "gt-cfglayout.h"
