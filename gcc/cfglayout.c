/* Basic block reordering routines for the GNU compiler.
   Copyright (C) 2000, 2001 Free Software Foundation, Inc.

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
#include "rtl.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "insn-config.h"
#include "output.h"
#include "function.h"
#include "obstack.h"
#include "cfglayout.h"

/* The contents of the current function definition are allocated
   in this obstack, and all are freed at the end of the function.  */

extern struct obstack flow_obstack;

/* Structure to hold information about lexical scopes.  */
struct scope_def
{
  int level;

  /* The NOTE_INSN_BLOCK_BEG that started this scope.  */
  rtx note_beg;

  /* The NOTE_INSN_BLOCK_END that ended this scope.  */
  rtx note_end;

  /* The bb containing note_beg (if any).  */
  basic_block bb_beg;

  /* The bb containing note_end (if any).  */
  basic_block bb_end;

  /* List of basic blocks contained within this scope.  */
  basic_block *bbs;

  /* Number of blocks contained within this scope.  */
  int num_bbs;

  /* The outer scope or NULL if outermost scope.  */
  struct scope_def *outer;

  /* The first inner scope or NULL if innermost scope.  */
  struct scope_def *inner;

  /* The last inner scope or NULL if innermost scope.  */
  struct scope_def *inner_last;

  /* Link to the next (sibling) scope.  */
  struct scope_def *next;
};

/* Structure to hold information about the scope forest.  */
typedef struct
{
  /* Number of trees in forest.  */
  int num_trees;

  /* List of tree roots.  */
  scope *trees;
} scope_forest_info;

/* Holds the interesting trailing notes for the function.  */
static rtx function_tail_eff_head;

/* The scope forest of current function.  */
static scope_forest_info forest;

static rtx skip_insns_after_block	PARAMS ((basic_block));
static void record_effective_endpoints	PARAMS ((void));
static rtx label_for_bb			PARAMS ((basic_block));
static void fixup_reorder_chain		PARAMS ((void));

static void relate_bbs_with_scopes	PARAMS ((scope));
static scope make_new_scope		PARAMS ((int, rtx));
static void build_scope_forest		PARAMS ((scope_forest_info *));
static void remove_scope_notes		PARAMS ((void));
static void insert_intra_before_1	PARAMS ((scope, rtx *, basic_block));
static void insert_intra_1		PARAMS ((scope, rtx *, basic_block));
static void insert_intra_bb_scope_notes PARAMS ((basic_block));
static void insert_inter_bb_scope_notes PARAMS ((basic_block, basic_block));
static void rebuild_scope_notes		PARAMS ((scope_forest_info *));
static void free_scope_forest_1		PARAMS ((scope));
static void free_scope_forest		PARAMS ((scope_forest_info *));
void dump_scope_forest			PARAMS ((scope_forest_info *));
static void dump_scope_forest_1		PARAMS ((scope, int));

static rtx get_next_bb_note		PARAMS ((rtx));
static rtx get_prev_bb_note		PARAMS ((rtx));

void verify_insn_chain			PARAMS ((void));
static void fixup_fallthru_exit_predecessor PARAMS ((void));

/* Skip over inter-block insns occurring after BB which are typically
   associated with BB (e.g., barriers). If there are any such insns,
   we return the last one. Otherwise, we return the end of BB.  */

static rtx
skip_insns_after_block (bb)
     basic_block bb;
{
  rtx insn, last_insn, next_head, prev;

  next_head = NULL_RTX;
  if (bb->index + 1 != n_basic_blocks)
    next_head = BASIC_BLOCK (bb->index + 1)->head;

  for (last_insn = insn = bb->end; (insn = NEXT_INSN (insn)) != 0; )
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

  for (insn = last_insn; insn != bb->end; insn = prev)
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
label_for_bb (bb)
     basic_block bb;
{
  rtx label = bb->head;

  if (GET_CODE (label) != CODE_LABEL)
    {
      if (rtl_dump_file)
	fprintf (rtl_dump_file, "Emitting label for block %d\n", bb->index);

      label = block_label (bb);
      if (bb->head == PREV_INSN (RBI (bb)->eff_head))
	RBI (bb)->eff_head = label;
    }

  return label;
}

/* Locate the effective beginning and end of the insn chain for each
   block, as defined by skip_insns_after_block above.  */

static void
record_effective_endpoints ()
{
  rtx next_insn = get_insns ();
  int i;
  
  for (i = 0; i < n_basic_blocks; i++)
    {
      basic_block bb = BASIC_BLOCK (i);
      rtx end;

      RBI (bb)->eff_head = next_insn;
      end = skip_insns_after_block (bb);
      RBI (bb)->eff_end = end;
      next_insn = NEXT_INSN (end);
    }

  function_tail_eff_head = next_insn;
}

/* Return the next NOTE_INSN_BASIC_BLOCK after X.  */

static rtx
get_next_bb_note (x)
     rtx x;
{
  for (; x; x = NEXT_INSN (x))
    if (NOTE_INSN_BASIC_BLOCK_P (x))
      return x;

  return NULL;
}

/* Return the fist NOTE_INSN_BASIC_BLOCK before X.  */

static rtx
get_prev_bb_note (x)
     rtx x;
{
  for (; x; x = PREV_INSN (x))
    if (NOTE_INSN_BASIC_BLOCK_P (x))
      return x;

  return NULL;
}

/* Determine and record the relationships between basic blocks and
   scopes in scope tree S.  */

static void
relate_bbs_with_scopes (s)
     scope s;
{
  scope p;
  int i, bbi1, bbi2, bbs_spanned;
  rtx bbnote;

  for (p = s->inner; p; p = p->next)
    relate_bbs_with_scopes (p);

  bbi1 = bbi2 = -1;
  bbs_spanned = 0;

  /* If the begin and end notes are both inside the same basic block,
     or if they are both outside of basic blocks, then we know immediately
     how they are related. Otherwise, we need to poke around to make the
     determination.  */
  if (s->bb_beg != s->bb_end)
    {
      if (s->bb_beg && s->bb_end)
        {
	  /* Both notes are in different bbs. This implies that all the
	     basic blocks spanned by the pair of notes are contained in
             this scope.  */
	  bbi1 = s->bb_beg->index;
	  bbi2 = s->bb_end->index;
	  bbs_spanned = 1;
	}
      else if (! s->bb_beg)
        {
	  /* First note is outside of a bb. If the scope spans more than
	     one basic block, then they all are contained within this
             scope. Otherwise, this scope is contained within the basic
	     block.  */
	  bbnote = get_next_bb_note (s->note_beg);
	  if (! bbnote)
	    abort ();

	  if (NOTE_BASIC_BLOCK (bbnote) == s->bb_end)
	    {
	      bbs_spanned = 0;
	      s->bb_beg = NOTE_BASIC_BLOCK (bbnote);
	    }
	  else
	    {
	      bbi1 = NOTE_BASIC_BLOCK (bbnote)->index;
	      bbi2 = s->bb_end->index;
	      s->bb_end = NULL;
	      bbs_spanned = 1;
	    }
	}
      else /* ! s->bb_end */
        {
	  /* Second note is outside of a bb. If the scope spans more than
	     one basic block, then they all are contained within this
             scope. Otherwise, this scope is contained within the basic
	     block.  */
	  bbnote = get_prev_bb_note (s->note_end);
	  if (! bbnote)
	    abort ();

	  if (NOTE_BASIC_BLOCK (bbnote) == s->bb_beg)
	    {
	      bbs_spanned = 0;
	      s->bb_end = NOTE_BASIC_BLOCK (bbnote);
	    }
	  else
	    {
	      bbi1 = s->bb_beg->index;
	      bbi2 = NOTE_BASIC_BLOCK (bbnote)->index;
	      s->bb_beg = NULL;
	      bbs_spanned = 1;
	    }
	}
    }
  else
    {
      if (s->bb_beg)
        /* Both notes are in the same bb, which implies the block
	   contains this scope.  */
	bbs_spanned = 0;
      else
	{
	  /* Both notes are outside of any bbs. This implies that all the
	     basic blocks spanned by the pair of notes are contained in
             this scope. 
	     There is a degenerate case to consider. If the notes do not
	     span any basic blocks, then it is an empty scope that can
	     safely be deleted or ignored. Mark these with level = -1.  */
	  rtx x1 = get_next_bb_note (s->note_beg);
	  rtx x2 = get_prev_bb_note (s->note_end);

	  if (! (x1 && x2))
	    {
	      s->level = -1; 
	      bbs_spanned = 0; 
	    }
	  else
	    {
	      bbi1 = NOTE_BASIC_BLOCK (x1)->index;
	      bbi2 = NOTE_BASIC_BLOCK (x2)->index;
	      bbs_spanned = 1;
	    }
	}
    }

  /* If the scope spans one or more basic blocks, we record them. We
     only record the bbs that are immediately contained within this
     scope. Note that if a scope is contained within a bb, we can tell
     by checking that bb_beg = bb_end and that they are non-null.  */
  if (bbs_spanned)
    {
      int j = 0;

      s->num_bbs = 0;
      for (i = bbi1; i <= bbi2; i++)
	if (! RBI (BASIC_BLOCK (i))->scope)
	  s->num_bbs++;

      s->bbs = xmalloc (s->num_bbs * sizeof (basic_block));
      for (i = bbi1; i <= bbi2; i++)
	{
	  basic_block curr_bb = BASIC_BLOCK (i);
	  if (! RBI (curr_bb)->scope)
	    {
	      s->bbs[j++] = curr_bb;
	      RBI (curr_bb)->scope = s;
	    }
	}
    }
  else
    s->num_bbs = 0;
}

/* Allocate and initialize a new scope structure with scope level LEVEL,
   and record the NOTE beginning the scope.  */

static scope 
make_new_scope (level, note)
     int level;
     rtx note;
{
  scope new_scope = xcalloc (1, sizeof (struct scope_def));

  new_scope->level = level;
  new_scope->note_beg = note;
  return new_scope;
}


/* Build a forest representing the scope structure of the function.
   Return a pointer to a structure describing the forest.  */

static void
build_scope_forest (forest)
    scope_forest_info *forest;
{
  rtx x;
  int level, bbi, i;
  basic_block curr_bb;
  scope root, curr_scope = 0;

  forest->num_trees = 0;
  forest->trees = NULL;
  level = -1;
  root = NULL;
  curr_bb = NULL;
  bbi = 0;

  for (x = get_insns (); x; x = NEXT_INSN (x))
    {
      if (bbi < n_basic_blocks && x == BASIC_BLOCK (bbi)->head)
	curr_bb = BASIC_BLOCK (bbi);

      if (GET_CODE (x) == NOTE)
	{
	  if (NOTE_LINE_NUMBER (x) == NOTE_INSN_BLOCK_BEG)
	    {
	      if (root)
		{
		  scope new_scope;

		  if (! curr_scope)
		    abort();

		  level++;
		  new_scope = make_new_scope (level, x);
		  new_scope->outer = curr_scope;
		  new_scope->next = NULL;
		  if (! curr_scope->inner)
		    {
		      curr_scope->inner = new_scope;
		      curr_scope->inner_last = new_scope;
		    }
		  else
		    {
		      curr_scope->inner_last->next = new_scope;
		      curr_scope->inner_last = new_scope;
		    }
		  curr_scope = curr_scope->inner_last;

		}
	      else
		{
		  int ntrees = forest->num_trees;

		  level++;
	          curr_scope = make_new_scope (level, x);
		  root = curr_scope;
		  forest->trees = xrealloc (forest->trees,
					    sizeof (scope) * (ntrees + 1));
		  forest->trees[forest->num_trees++] = root;
		}

	      curr_scope->bb_beg = curr_bb;
	    }
	  else if (NOTE_LINE_NUMBER (x) == NOTE_INSN_BLOCK_END)
	    {
	      curr_scope->bb_end = curr_bb;
	      curr_scope->note_end = x;
	      level--;
	      curr_scope = curr_scope->outer;
	      if (level == -1)
		root = NULL;
	    }
	}

      if (curr_bb && curr_bb->end == x)
	{
	  curr_bb = NULL;
	  bbi++;
	}
    } 

  for (i = 0; i < forest->num_trees; i++)
    relate_bbs_with_scopes (forest->trees[i]);
}

/* Remove all NOTE_INSN_BLOCK_BEG and NOTE_INSN_BLOCK_END notes from the insn
   chain.  */

static void
remove_scope_notes ()
{
  rtx x, next;
  basic_block currbb = NULL;

  for (x = get_insns (); x; x = next)
    {
      next = NEXT_INSN (x);
      if (NOTE_INSN_BASIC_BLOCK_P (x))
	currbb = NOTE_BASIC_BLOCK (x);

      if (GET_CODE (x) == NOTE
	  && (NOTE_LINE_NUMBER (x) == NOTE_INSN_BLOCK_BEG
	      || NOTE_LINE_NUMBER (x) == NOTE_INSN_BLOCK_END))
	{
	  /* Check if the scope note happens to be the end of a bb.  */
	  if (currbb && x == currbb->end)
	    currbb->end = PREV_INSN (x);
	  if (currbb && x == currbb->head)
	    abort ();

	  if (PREV_INSN (x))
	    {
	      NEXT_INSN (PREV_INSN (x)) = next;
	      if (next)
	        PREV_INSN (next) = PREV_INSN (x);

              NEXT_INSN (x) = NULL;
              PREV_INSN (x) = NULL;
	    }
	  else
	    abort ();
	}
    }
}

/* Insert scope note pairs for a contained scope tree S after insn IP.  */

static void
insert_intra_1 (s, ip, bb)
     scope s;
     rtx *ip;
     basic_block bb;
{
  scope p;

  if (NOTE_BLOCK (s->note_beg))
    {  
      *ip = emit_note_after (NOTE_INSN_BLOCK_BEG, *ip);
      NOTE_BLOCK (*ip) = NOTE_BLOCK (s->note_beg);
    } 

  for (p = s->inner; p; p = p->next)
    insert_intra_1 (p, ip, bb);

  if (NOTE_BLOCK (s->note_beg))
    {  
      *ip = emit_note_after (NOTE_INSN_BLOCK_END, *ip);
      NOTE_BLOCK (*ip) = NOTE_BLOCK (s->note_end);
    }
}

/* Insert scope note pairs for a contained scope tree S before insn IP.  */

static void
insert_intra_before_1 (s, ip, bb)
     scope s;
     rtx *ip;
     basic_block bb;
{
  scope p;

  if (NOTE_BLOCK (s->note_beg))
    {  
      *ip = emit_note_before (NOTE_INSN_BLOCK_END, *ip);
      NOTE_BLOCK (*ip) = NOTE_BLOCK (s->note_end);
    } 

  for (p = s->inner; p; p = p->next)
    insert_intra_before_1 (p, ip, bb);

  if (NOTE_BLOCK (s->note_beg))
    {  
      *ip = emit_note_before (NOTE_INSN_BLOCK_BEG, *ip);
      NOTE_BLOCK (*ip) = NOTE_BLOCK (s->note_beg);
    }
}

/* Insert NOTE_INSN_BLOCK_END notes and NOTE_INSN_BLOCK_BEG notes for
   scopes that are contained within BB.  */

static void
insert_intra_bb_scope_notes (bb)
     basic_block bb;
{
  scope s = RBI (bb)->scope;
  scope p;
  rtx ip;

  if (! s)
    return;

  ip = bb->head;
  if (GET_CODE (ip) == CODE_LABEL)
    ip = NEXT_INSN (ip);

  for (p = s->inner; p; p = p->next)
    {
      if (p->bb_beg != NULL && p->bb_beg == p->bb_end && p->bb_beg == bb)
	insert_intra_1 (p, &ip, bb);
    }
}

/* Given two consecutive basic blocks BB1 and BB2 with different scopes,
   insert NOTE_INSN_BLOCK_END notes after BB1 and NOTE_INSN_BLOCK_BEG
   notes before BB2 such that the notes are correctly balanced. If BB1 or
   BB2 is NULL, we are inserting scope notes for the first and last basic
   blocks, respectively.  */

static void
insert_inter_bb_scope_notes (bb1, bb2)
     basic_block bb1;
     basic_block bb2;
{
  rtx ip;
  scope com;

  /* It is possible that a basic block is not contained in any scope.
     In that case, we either open or close a scope but not both.  */
  if (bb1 && bb2)
    {
      scope s1 = RBI (bb1)->scope;
      scope s2 = RBI (bb2)->scope;

      if (! s1 && ! s2)
	return;

      if (! s1)
	bb1 = NULL;
      else if (! s2)
	bb2 = NULL;
    }

  /* Find common ancestor scope.  */
  if (bb1 && bb2)
    {
      scope s1 = RBI (bb1)->scope;
      scope s2 = RBI (bb2)->scope;

      while (s1 != s2)
	{
	  if (s1->level > s2->level)
	    s1 = s1->outer;
	  else if (s2->level > s1->level)
	    s2 = s2->outer;
	  else
	    {
	      s1 = s1->outer;
	      s2 = s2->outer;
	    }
	}

      com = s1;
    }
  else
    com = NULL;

  /* Close scopes.  */
  if (bb1)
    {
      rtx end = bb1->end;
      scope s, p;

      ip = RBI (bb1)->eff_end;
      for (s = RBI (bb1)->scope; s != com; s = s->outer)
	{
	  if (NOTE_BLOCK (s->note_beg))
	    {  
	      ip = emit_note_after (NOTE_INSN_BLOCK_END, ip);
	      NOTE_BLOCK (ip) = NOTE_BLOCK (s->note_end);
	    }

	  /* Now emit all sibling scopes which don't span any basic
	     blocks.  */
	  if (s->outer)
	    for (p = s->outer->inner; p; p = p->next)
	      if (p != s && p->bb_beg == bb1 && p->bb_beg == p->bb_end)
		insert_intra_1 (p, &ip, bb1);
	}

      /* Emitting note may move the end of basic block to unwanted place.  */
      bb1->end = end;
    }

  /* Open scopes.  */
  if (bb2)
    {
      scope s, p;

      ip = bb2->head;
      for (s = RBI (bb2)->scope; s != com; s = s->outer)
	{
	  if (NOTE_BLOCK (s->note_beg))
	    {  
	      ip = emit_note_before (NOTE_INSN_BLOCK_BEG, ip);
	      NOTE_BLOCK (ip) = NOTE_BLOCK (s->note_beg);
	    }

	  /* Now emit all sibling scopes which don't span any basic
	     blocks.  */
	  if (s->outer)
	    for (p = s->outer->inner; p; p = p->next)
	      if (p != s && p->bb_beg == bb2 && p->bb_beg == p->bb_end)
		insert_intra_before_1 (p, &ip, bb2);
	}
    }
}


/* Rebuild all the NOTE_INSN_BLOCK_BEG and NOTE_INSN_BLOCK_END notes based
   on the scope forest and the newly reordered basic blocks.  */

static void
rebuild_scope_notes (forest)
    scope_forest_info *forest;
{
  int i;

  if (forest->num_trees == 0)
    return;

  /* Start by opening the scopes before the first basic block.  */
  insert_inter_bb_scope_notes (NULL, BASIC_BLOCK (0));

  /* Then, open and close scopes as needed between blocks.  */
  for (i = 0; i < n_basic_blocks - 1; i++)
    {
      basic_block bb1 = BASIC_BLOCK (i);
      basic_block bb2 = BASIC_BLOCK (i + 1);

      if (RBI (bb1)->scope != RBI (bb2)->scope)
	insert_inter_bb_scope_notes (bb1, bb2);
      insert_intra_bb_scope_notes (bb1);
    }

  /* Finally, close the scopes after the last basic block.  */
  insert_inter_bb_scope_notes (BASIC_BLOCK (n_basic_blocks - 1), NULL);
  insert_intra_bb_scope_notes (BASIC_BLOCK (n_basic_blocks - 1));
}

/* Free the storage associated with the scope tree at S.  */

static void
free_scope_forest_1 (s)
    scope s;
{
  scope p, next;

  for (p = s->inner; p; p = next)
    {
      next = p->next;
      free_scope_forest_1 (p);
    }

  if (s->bbs)
    free (s->bbs);
  free (s);
}

/* Free the storage associated with the scope forest.  */

static void
free_scope_forest (forest)
    scope_forest_info *forest;
{
  int i;

  for (i = 0; i < forest->num_trees; i++)
    free_scope_forest_1 (forest->trees[i]);
}

/* Visualize the scope forest.  */

void
dump_scope_forest (forest)
    scope_forest_info *forest;
{
  int i;

  if (forest->num_trees == 0)
    fprintf (stderr, "\n< Empty scope forest >\n");
  else
    fprintf (stderr, "\n< Scope forest >\n");

  for (i = 0; i < forest->num_trees; i++)
    dump_scope_forest_1 (forest->trees[i], 0);
}

/* Recursive portion of dump_scope_forest.  */

static void
dump_scope_forest_1 (s, indent)
     scope s;
     int indent;
{
  scope p;
  int i;

  if (s->bb_beg != NULL && s->bb_beg == s->bb_end
      && RBI (s->bb_beg)->scope
      && RBI (s->bb_beg)->scope->level + 1 == s->level)
    {
      fprintf (stderr, "%*s", indent, "");
      fprintf (stderr, "BB%d:\n", s->bb_beg->index);
    }

  fprintf (stderr, "%*s", indent, "");
  fprintf (stderr, "{ level %d (block %p)\n", s->level,
	   (PTR) NOTE_BLOCK (s->note_beg));

  fprintf (stderr, "%*s%s", indent, "", "bbs:");
  for (i = 0; i < s->num_bbs; i++)
    fprintf (stderr, " %d", s->bbs[i]->index);
  fprintf (stderr, "\n");
  
  for (p = s->inner; p; p = p->next)
    dump_scope_forest_1 (p, indent + 2);

  fprintf (stderr, "%*s", indent, "");
  fprintf (stderr, "}\n");
}

/* Given a reorder chain, rearrange the code to match.  */

static void
fixup_reorder_chain ()
{
  basic_block bb, last_bb;
  int index;
  rtx insn;
  int old_n_basic_blocks = n_basic_blocks;

  /* First do the bulk reordering -- rechain the blocks without regard to
     the needed changes to jumps and labels.  */

  for (last_bb = BASIC_BLOCK (0), bb = RBI (last_bb)->next, index = 1;
       bb != 0;
       last_bb = bb, bb = RBI (bb)->next, index++)
    {
      rtx last_e = RBI (last_bb)->eff_end;
      rtx curr_h = RBI (bb)->eff_head;

      NEXT_INSN (last_e) = curr_h;
      PREV_INSN (curr_h) = last_e;
    }

  if (index != n_basic_blocks)
    abort ();

  insn = RBI (last_bb)->eff_end;
  NEXT_INSN (insn) = function_tail_eff_head;
  if (function_tail_eff_head)
    PREV_INSN (function_tail_eff_head) = insn;

  while (NEXT_INSN (insn))
    insn = NEXT_INSN (insn);

  set_last_insn (insn);
#ifdef ENABLE_CHECKING
  verify_insn_chain ();
#endif

  /* Now add jumps and labels as needed to match the blocks new
     outgoing edges.  */

  for (bb = BASIC_BLOCK (0); bb ; bb = RBI (bb)->next)
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

      bb_end_insn = bb->end;
      if (GET_CODE (bb_end_insn) == JUMP_INSN)
	{
	  if (any_condjump_p (bb_end_insn))
	    {
	      /* If the old fallthru is still next, nothing to do.  */
	      if (RBI (bb)->next == e_fall->dest
	          || (!RBI (bb)->next
		      && e_fall->dest == EXIT_BLOCK_PTR))
		continue;

	      /* There is one special case: if *neither* block is next,
		 such as happens at the very end of a function, then we'll
		 need to add a new unconditional jump.  Choose the taken
		 edge based on known or assumed probability.  */
	      if (RBI (bb)->next != e_taken->dest)
		{
		  rtx note = find_reg_note (bb_end_insn, REG_BR_PROB, 0);

		  if (note
		      && INTVAL (XEXP (note, 0)) < REG_BR_PROB_BASE / 2
		      && invert_jump (bb_end_insn,
				      label_for_bb (e_fall->dest), 0))
		    {
		      e_fall->flags &= ~EDGE_FALLTHRU;
		      e_taken->flags |= EDGE_FALLTHRU;
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
	      if (RBI (bb)->next == e_fall->dest)
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
	  if (RBI (bb)->next == e_fall->dest)
	    continue;

	  /* A fallthru to exit block.  */
	  if (!RBI (bb)->next && e_fall->dest == EXIT_BLOCK_PTR)
	    continue;
	}

      /* We got here if we need to add a new jump insn.  */
      nb = force_nonfallthru (e_fall);
      if (nb)
	{
	  alloc_aux_for_block (nb, sizeof (struct reorder_block_def));
	  RBI (nb)->eff_head = nb->head;
	  RBI (nb)->eff_end = NEXT_INSN (nb->end);
	  RBI (nb)->scope = RBI (bb)->scope;
	  RBI (nb)->visited = 1;
	  RBI (nb)->next = RBI (bb)->next;
	  RBI (bb)->next = nb;
	  /* Don't process this new block.  */
	  bb = nb;
	}
    }

  /* Put basic_block_info in the new order.  */
  bb = BASIC_BLOCK (0);
  index = 0;

  if (rtl_dump_file)
    fprintf (rtl_dump_file, "Reordered sequence:\n");

  for (; bb; bb = RBI (bb)->next, index++)
    {
      if (rtl_dump_file)
	fprintf (rtl_dump_file, " %i %sbb %i freq %i\n", index,
		 bb->index >= old_n_basic_blocks ? "compensation " : "",
		 bb->index,
	   	 bb->frequency);

      bb->index = index;
      BASIC_BLOCK (index) = bb;
    }
}

/* Perform sanity checks on the insn chain.
   1. Check that next/prev pointers are consistent in both the forward and
      reverse direction.
   2. Count insns in chain, going both directions, and check if equal.
   3. Check that get_last_insn () returns the actual end of chain.  */

void
verify_insn_chain ()
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

/* The block falling through to exit must be the last one in the reordered
   chain.  Ensure it is.  */

static void
fixup_fallthru_exit_predecessor ()
{
  edge e;
  basic_block bb = NULL;

  for (e = EXIT_BLOCK_PTR->pred; e; e = e->pred_next)
    if (e->flags & EDGE_FALLTHRU)
      bb = e->src;

  if (bb && RBI (bb)->next)
    {
      basic_block c = BASIC_BLOCK (0);

      while (RBI (c)->next != bb)
	c = RBI (c)->next;

      RBI (c)->next = RBI (bb)->next;
      while (RBI (c)->next)
	c = RBI (c)->next;

      RBI (c)->next = bb;
      RBI (bb)->next = NULL;
    }
}

/* Main entry point to this module: initialize the datastructures for CFG
   layout changes.  */

void
cfg_layout_initialize ()
{
  alloc_aux_for_blocks (sizeof (struct reorder_block_def));

  build_scope_forest (&forest);
  remove_scope_notes ();

  record_effective_endpoints ();
}

/* Finalize the changes: reorder insn list according to the sequence, enter
   compensation code, rebuild scope forest.  */

void
cfg_layout_finalize ()
{
  fixup_fallthru_exit_predecessor ();
  fixup_reorder_chain ();

#ifdef ENABLE_CHECKING
  verify_insn_chain ();
#endif

  rebuild_scope_notes (&forest);
  free_scope_forest (&forest);
  reorder_blocks ();

  free_aux_for_blocks ();

#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif
}
