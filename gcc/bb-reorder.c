/* Basic block reordering routines for the GNU compiler.
   Copyright (C) 2000 Free Software Foundation, Inc.

   This file is part of GNU CC.

   GNU CC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GNU CC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* References:

   "Profile Guided Code Positioning"
   Pettis and Hanson; PLDI '90.
*/

#include "config.h"
#include "system.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "basic-block.h"
#include "insn-config.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "flags.h"
#include "output.h"
#include "function.h"
#include "except.h"
#include "toplev.h"
#include "recog.h"
#include "insn-flags.h"
#include "expr.h"
#include "obstack.h"


/* The contents of the current function definition are allocated
   in this obstack, and all are freed at the end of the function.
   For top-level functions, this is temporary_obstack.
   Separate obstacks are made for nested functions.  */

extern struct obstack *function_obstack;


/* Structure to hold information about lexical scopes.  */
typedef struct scope_def
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
} *scope;

/* Structure to hold information about the scope forest.  */
typedef struct
{
  /* Number of trees in forest.  */
  int num_trees;

  /* List of tree roots.  */
  scope *trees;
} scope_forest_info;


typedef struct reorder_block_def {
  int flags;
  int index;
  basic_block add_jump;
  edge succ;
  rtx end;
  int block_begin;
  int block_end;
  rtx eff_head;
  rtx eff_end;
  scope scope;
} *reorder_block_def;

static struct reorder_block_def rbd_init
= {
    0,			/* flags */
    0,			/* index */
    NULL,		/* add_jump */
    NULL,		/* succ */
    NULL_RTX,		/* end */
    0,			/* block_begin */
    0,			/* block_end */
    NULL_RTX,		/* eff_head */
    NULL_RTX,		/* eff_end */
    NULL		/* scope */
};


#define REORDER_BLOCK_HEAD	0x1
#define REORDER_BLOCK_VISITED	0x2
  
#define REORDER_BLOCK_FLAGS(bb) \
  ((reorder_block_def) (bb)->aux)->flags

#define REORDER_BLOCK_INDEX(bb) \
  ((reorder_block_def) (bb)->aux)->index

#define REORDER_BLOCK_ADD_JUMP(bb) \
  ((reorder_block_def) (bb)->aux)->add_jump

#define REORDER_BLOCK_SUCC(bb) \
  ((reorder_block_def) (bb)->aux)->succ

#define REORDER_BLOCK_OLD_END(bb) \
  ((reorder_block_def) (bb)->aux)->end

#define REORDER_BLOCK_BEGIN(bb) \
  ((reorder_block_def) (bb)->aux)->block_begin

#define REORDER_BLOCK_END(bb) \
  ((reorder_block_def) (bb)->aux)->block_end

#define REORDER_BLOCK_EFF_HEAD(bb) \
  ((reorder_block_def) (bb)->aux)->eff_head

#define REORDER_BLOCK_EFF_END(bb) \
  ((reorder_block_def) (bb)->aux)->eff_end

#define REORDER_BLOCK_SCOPE(bb) \
  ((reorder_block_def) (bb)->aux)->scope


static int reorder_index;
static basic_block reorder_last_visited;

enum reorder_skip_type {REORDER_SKIP_BEFORE, REORDER_SKIP_AFTER,
			REORDER_SKIP_BLOCK_END};


/* Local function prototypes.  */
static rtx skip_insns_between_block	PARAMS ((basic_block,
						 enum reorder_skip_type));
static basic_block get_common_dest	PARAMS ((basic_block, basic_block));
static basic_block chain_reorder_blocks	PARAMS ((edge, basic_block));
static void make_reorder_chain		PARAMS ((basic_block));
static void fixup_reorder_chain		PARAMS ((void));
#ifdef ENABLE_CHECKING
static void verify_insn_chain		PARAMS ((void));
#endif
static void relate_bbs_with_scopes	PARAMS ((scope));
static scope make_new_scope		PARAMS ((int, rtx));
static void build_scope_forest		PARAMS ((scope_forest_info *));
static void remove_scope_notes		PARAMS ((void));
static void insert_intra_1		PARAMS ((scope, rtx *));
static void insert_intra_bb_scope_notes PARAMS ((basic_block));
static void insert_inter_bb_scope_notes PARAMS ((basic_block, basic_block));
static void rebuild_scope_notes		PARAMS ((scope_forest_info *));
static void free_scope_forest_1		PARAMS ((scope));
static void free_scope_forest		PARAMS ((scope_forest_info *));
void dump_scope_forest			PARAMS ((scope_forest_info *));
static void dump_scope_forest_1		PARAMS ((scope, int));

/* Skip over insns BEFORE or AFTER BB which are typically associated with
   basic block BB.  */

static rtx
skip_insns_between_block (bb, skip_type)
     basic_block bb;
     enum reorder_skip_type skip_type;
{
  rtx insn, last_insn;

  if (skip_type == REORDER_SKIP_BEFORE)
    {
      if (bb == ENTRY_BLOCK_PTR)
	return 0;

      last_insn = bb->head;
      for (insn = PREV_INSN (bb->head);
	   insn && insn != BASIC_BLOCK (bb->index - 1)->end;
	   last_insn = insn, insn = PREV_INSN (insn))
	{
	  if (NEXT_INSN (insn) != last_insn)
	    break;

	  if (GET_CODE (insn) == NOTE
	      && NOTE_LINE_NUMBER (insn) != NOTE_INSN_LOOP_END
	      && NOTE_LINE_NUMBER (insn) != NOTE_INSN_BASIC_BLOCK
	      && NOTE_LINE_NUMBER (insn) != NOTE_INSN_BLOCK_END)
	    continue;
	  
	  break;
	}
    }
  else
    {
      last_insn = bb->end;

      if (bb == EXIT_BLOCK_PTR)
	return 0;

      for (insn = NEXT_INSN (bb->end); 
	   insn;
	   last_insn = insn, insn = NEXT_INSN (insn))
	{
	  if (bb->index + 1 != n_basic_blocks
	      && insn == BASIC_BLOCK (bb->index + 1)->head)
	    break;

	  if (GET_CODE (insn) == BARRIER
	      || GET_CODE (insn) == JUMP_INSN 
	      || (GET_CODE (insn) == NOTE
		  && (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_END
		      || NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_END)))
	    continue;

	  if (GET_CODE (insn) == CODE_LABEL
	      && GET_CODE (NEXT_INSN (insn)) == JUMP_INSN
	      && (GET_CODE (PATTERN (NEXT_INSN (insn))) == ADDR_VEC
		  || GET_CODE (PATTERN
			       (NEXT_INSN (insn))) == ADDR_DIFF_VEC))
	    {
	      insn = NEXT_INSN (insn);
	      continue;
	    }

	  /* Skip to next non-deleted insn.  */
	  if (GET_CODE (insn) == NOTE
	      && (NOTE_LINE_NUMBER (insn) == NOTE_INSN_DELETED
		  || NOTE_LINE_NUMBER (insn) == NOTE_INSN_DELETED_LABEL))
	    continue; 

	  break;
	}

      if (skip_type == REORDER_SKIP_BLOCK_END)
	{
	  int found_block_end = 0;

	  for (; insn; last_insn = insn, insn = NEXT_INSN (insn))
	    {
	      if (bb->index + 1 != n_basic_blocks
		  && insn == BASIC_BLOCK (bb->index + 1)->head)
		break;

	      if (GET_CODE (insn) == NOTE
		  && NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_END)
		{
		  found_block_end = 1;
		  continue;
		}

	      if (GET_CODE (insn) == NOTE
		  && NOTE_LINE_NUMBER (insn) == NOTE_INSN_DELETED)
		continue;

	      if (GET_CODE (insn) == NOTE
		  && NOTE_LINE_NUMBER (insn) >= 0
		  && NEXT_INSN (insn)
		  && GET_CODE (NEXT_INSN (insn)) == NOTE
		  && (NOTE_LINE_NUMBER (NEXT_INSN (insn))
		      == NOTE_INSN_BLOCK_END))
		continue;
	      break;
	    }

	  if (! found_block_end)
	    last_insn = 0;
	}
    }

  return last_insn;
}


/* Return common destination for blocks BB0 and BB1.  */

static basic_block
get_common_dest (bb0, bb1)
     basic_block bb0, bb1;
{
  edge e0, e1;

  for (e0 = bb0->succ; e0; e0 = e0->succ_next)
    {
      for (e1 = bb1->succ; e1; e1 = e1->succ_next)
	{
	  if (e0->dest == e1->dest)
	    {
	      return e0->dest;
	    }
	}
    }
  return 0;
}


/* Move the destination block for edge E after chain end block CEB
   Adding jumps and labels is deferred until fixup_reorder_chain.  */

static basic_block
chain_reorder_blocks (e, ceb)
     edge e;
     basic_block ceb;
{
  basic_block sb = e->src;
  basic_block db = e->dest;
  rtx cebe_insn, cebbe_insn, dbh_insn, dbe_insn;
  edge ee, last_edge;

  enum cond_types {NO_COND, PREDICT_THEN_WITH_ELSE, PREDICT_ELSE,
		   PREDICT_THEN_NO_ELSE, PREDICT_NOT_THEN_NO_ELSE};
  enum cond_types cond_type;
  enum cond_block_types {NO_COND_BLOCK, THEN_BLOCK, ELSE_BLOCK,
			 NO_ELSE_BLOCK};
  enum cond_block_types cond_block_type;

  if (rtl_dump_file)
    fprintf (rtl_dump_file,
	     "Edge from basic block %d to basic block %d last visited %d\n",
	     sb->index, db->index, ceb->index);

  dbh_insn = REORDER_BLOCK_EFF_HEAD (db);
  cebe_insn = REORDER_BLOCK_EFF_END (ceb);
  cebbe_insn = skip_insns_between_block (ceb, REORDER_SKIP_BLOCK_END);

  {
    int block_begins = 0;
    rtx insn;

    for (insn = dbh_insn; insn && insn != db->end; insn = NEXT_INSN (insn))
      {
	if (GET_CODE (insn) == NOTE
	    && NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_BEG)
	  {
	    block_begins += 1;
	    break;
	  }
      }
    REORDER_BLOCK_BEGIN (sb) = block_begins;
  }

  if (cebbe_insn)
    {
      int block_ends = 0;
      rtx insn;

      for (insn = cebe_insn; insn; insn = NEXT_INSN (insn))
	{
	  if (PREV_INSN (insn) == cebbe_insn)
	    break;
	  if (GET_CODE (insn) == NOTE
	      && NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_END)
	    {
	      block_ends += 1;
	      continue;
	    }
	}
      REORDER_BLOCK_END (ceb) = block_ends;
    }

  /* Blocks are in original order.  */
  if (sb->index == ceb->index
      && ceb->index + 1 == db->index && NEXT_INSN (cebe_insn))
    return db;

  /* Get the type of block and type of condition.  */
  cond_type = NO_COND;
  cond_block_type = NO_COND_BLOCK;
  if (GET_CODE (sb->end) == JUMP_INSN && ! simplejump_p (sb->end)
      && condjump_p (sb->end))
    {
      if (e->flags & EDGE_FALLTHRU)
	cond_block_type = THEN_BLOCK;
      else if (get_common_dest (sb->succ->dest, sb))
	cond_block_type = NO_ELSE_BLOCK;
      else 
	cond_block_type = ELSE_BLOCK;

      if (sb->succ->succ_next
	  && get_common_dest (sb->succ->dest, sb))
	{
	  if (cond_block_type == THEN_BLOCK)
	    {
	      if (! (REORDER_BLOCK_FLAGS (sb->succ->succ_next->dest)
		     & REORDER_BLOCK_VISITED))
		cond_type = PREDICT_THEN_NO_ELSE;
	      else
		cond_type = PREDICT_NOT_THEN_NO_ELSE;
	    }
	  else if (cond_block_type == NO_ELSE_BLOCK)
	    {
	      if (! (REORDER_BLOCK_FLAGS (sb->succ->dest)
		     & REORDER_BLOCK_VISITED))
		cond_type = PREDICT_NOT_THEN_NO_ELSE;
	      else
		cond_type = PREDICT_THEN_NO_ELSE;
	    }
	}
      else
	{
	  if (cond_block_type == THEN_BLOCK)
	    {
	      if (! (REORDER_BLOCK_FLAGS (sb->succ->succ_next->dest)
		     & REORDER_BLOCK_VISITED))
		cond_type = PREDICT_THEN_WITH_ELSE;
	      else
		cond_type = PREDICT_ELSE;
	    }
	  else if (cond_block_type == ELSE_BLOCK
		   && sb->succ->dest != EXIT_BLOCK_PTR)
	    {
	      if (! (REORDER_BLOCK_FLAGS (sb->succ->dest)
		     & REORDER_BLOCK_VISITED))
		cond_type = PREDICT_ELSE;
	      else
		cond_type = PREDICT_THEN_WITH_ELSE;
	    }
	}
    }
  
  if (rtl_dump_file)
    {
      static const char * cond_type_str [] = {"not cond jump", "predict then",
					      "predict else",
					      "predict then w/o else",
					      "predict not then w/o else"};
      static const char * cond_block_type_str [] = {"not then or else block",
						    "then block",
						    "else block",
						    "then w/o else block"};

      fprintf (rtl_dump_file, "     %s (looking at %s)\n",
	       cond_type_str[(int)cond_type],
	       cond_block_type_str[(int)cond_block_type]);
    }

  /* Reflect that then block will move and we'll jump to it.  */
  if (cond_block_type != THEN_BLOCK
      && (cond_type == PREDICT_ELSE
	  || cond_type == PREDICT_NOT_THEN_NO_ELSE))
    {
      if (rtl_dump_file)
	fprintf (rtl_dump_file,
		 "    then jump from block %d to block %d\n",
		 sb->index, sb->succ->dest->index);

      /* Jump to reordered then block.  */
      REORDER_BLOCK_ADD_JUMP (sb) = sb->succ->dest;
    }
  
  /* Reflect that then block will jump back when we have no else.  */
  if (cond_block_type != THEN_BLOCK
      && cond_type == PREDICT_NOT_THEN_NO_ELSE)
    {
      for (ee = sb->succ->dest->succ;
	   ee && ! (ee->flags & EDGE_FALLTHRU);
	   ee = ee->succ_next)
	continue;

      if (ee && ! (GET_CODE (sb->succ->dest->end) == JUMP_INSN
		   && ! simplejump_p (sb->succ->dest->end)))
	{
	  REORDER_BLOCK_ADD_JUMP (sb->succ->dest) = ee->dest;
	}
    }

  /* Reflect that else block will jump back.  */
  if (cond_block_type == ELSE_BLOCK
      && (cond_type == PREDICT_THEN_WITH_ELSE || cond_type == PREDICT_ELSE))
    {
      last_edge=db->succ;

      if (last_edge
	  && last_edge->dest != EXIT_BLOCK_PTR
	  && GET_CODE (last_edge->dest->head) == CODE_LABEL
	  && ! (GET_CODE (db->end) == JUMP_INSN))
	{
	  if (rtl_dump_file)
	    fprintf (rtl_dump_file,
		     "     else jump from block %d to block %d\n",
		     db->index, last_edge->dest->index);

	  REORDER_BLOCK_ADD_JUMP (db) = last_edge->dest;
	}
    }

  /* This block's successor has already been reordered. This can happen
     when we reorder a chain starting at a then or else.  */
  for (last_edge = db->succ;
       last_edge && ! (last_edge->flags & EDGE_FALLTHRU);
       last_edge = last_edge->succ_next)
    continue;

  if (last_edge
      && last_edge->dest != EXIT_BLOCK_PTR
      && (REORDER_BLOCK_FLAGS (last_edge->dest)
	  & REORDER_BLOCK_VISITED))
    {
      if (rtl_dump_file)
	fprintf (rtl_dump_file,
		 "     end of chain jump from block %d to block %d\n",
		 db->index, last_edge->dest->index);

      REORDER_BLOCK_ADD_JUMP (db) = last_edge->dest;
    }

  dbh_insn = REORDER_BLOCK_EFF_HEAD (db);
  cebe_insn = REORDER_BLOCK_EFF_END (ceb);
  dbe_insn = REORDER_BLOCK_EFF_END (db);

  /* Leave behind any lexical block markers.  */
  if (0 && debug_info_level > DINFO_LEVEL_TERSE
      && ceb->index + 1 < db->index)
    {
      rtx insn, last_insn = get_last_insn ();
      insn = NEXT_INSN (ceb->end);
      if (! insn)
	insn = REORDER_BLOCK_OLD_END (ceb);

      if (NEXT_INSN (cebe_insn) == 0)
	  set_last_insn (cebe_insn);
      for (; insn && insn != db->head/*dbh_insn*/;
	   insn = NEXT_INSN (insn))
	{
	  if (GET_CODE (insn) == NOTE
	      && (NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_BEG))
	    {
	      cebe_insn = emit_note_after (NOTE_INSN_BLOCK_BEG, cebe_insn);
	      delete_insn (insn);
	    }
	  if (GET_CODE (insn) == NOTE
	      && (NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_END))
	    {
	      cebe_insn = emit_note_after (NOTE_INSN_BLOCK_END, cebe_insn);
	      delete_insn (insn);
	    }      
	}
      set_last_insn (last_insn);
    }

  /* Rechain predicted block.  */
  NEXT_INSN (cebe_insn) = dbh_insn;
  PREV_INSN (dbh_insn) = cebe_insn;

  REORDER_BLOCK_OLD_END (db) = NEXT_INSN (dbe_insn);
  if (db->index != n_basic_blocks - 1)
    NEXT_INSN (dbe_insn) = 0;

  return db;
}


/* Reorder blocks starting at block BB.  */

static void
make_reorder_chain (bb)
     basic_block bb;
{
  edge e;
  basic_block visited_edge = NULL;
  rtx block_end;
  int probability;

  if (bb == EXIT_BLOCK_PTR)
    return;

  /* Find the most probable block.  */
  e = bb->succ;
  block_end = bb->end;
  if (GET_CODE (block_end) == JUMP_INSN && condjump_p (block_end))
    {
      rtx note = find_reg_note (block_end, REG_BR_PROB, 0);

      if (note) 
	probability = INTVAL (XEXP (note, 0));
      else
	probability = 0;

      if (probability >= REG_BR_PROB_BASE / 2)
	e = bb->succ->succ_next;
    }

  /* Add chosen successor to chain and recurse on it.  */
  if (e && e->dest != EXIT_BLOCK_PTR
      && e->dest != e->src
      && (! (REORDER_BLOCK_FLAGS (e->dest) & REORDER_BLOCK_VISITED)
	  || (REORDER_BLOCK_FLAGS (e->dest) == REORDER_BLOCK_HEAD)))
    {
      if (! (REORDER_BLOCK_FLAGS (bb) & REORDER_BLOCK_VISITED))
	{
	  REORDER_BLOCK_FLAGS (bb) |= REORDER_BLOCK_HEAD;
	  REORDER_BLOCK_INDEX (bb) = reorder_index++;
	  REORDER_BLOCK_FLAGS (bb) |= REORDER_BLOCK_VISITED;
	}

      if (REORDER_BLOCK_FLAGS (e->dest) & REORDER_BLOCK_VISITED)
	REORDER_BLOCK_FLAGS (e->dest) &= ~REORDER_BLOCK_HEAD;
	
      REORDER_BLOCK_SUCC (bb) = e;

      visited_edge = e->dest;

      reorder_last_visited = chain_reorder_blocks (e, bb);

      if (e->dest
	  && ! (REORDER_BLOCK_FLAGS (e->dest)
		& REORDER_BLOCK_VISITED))
	make_reorder_chain (e->dest);
    }
  else
    {
      if (! (REORDER_BLOCK_FLAGS (bb) & REORDER_BLOCK_VISITED))
	{
	  REORDER_BLOCK_INDEX (bb) = reorder_index++;
	  REORDER_BLOCK_FLAGS (bb) |= REORDER_BLOCK_VISITED;
	}
    }

  /* Recurse on the successors.  */
  for (e = bb->succ; e; e = e->succ_next)
    {
      if (e->dest && e->dest == EXIT_BLOCK_PTR)
	continue;

      if (e->dest
	  && e->dest != e->src
	  && e->dest != visited_edge
	  && ! (REORDER_BLOCK_FLAGS (e->dest)
		& REORDER_BLOCK_VISITED))
	{
	  reorder_last_visited
	    = chain_reorder_blocks (e, reorder_last_visited);
	  make_reorder_chain (e->dest);
	}
    }
}


/* Fixup jumps and labels after reordering basic blocks.  */ 

static void
fixup_reorder_chain ()
{
  int i, j;
  rtx insn;
  int orig_num_blocks = n_basic_blocks;

  /* Set the new last insn.  */
  {
    int max_val = 0;
    int max_index = 0;
    for (j = 0; j < n_basic_blocks; j++) 
      {
	int val = REORDER_BLOCK_INDEX (BASIC_BLOCK (j));
	if (val > max_val)
	  {
	    max_val = val;
	    max_index = j;
	  }
      }
    insn = REORDER_BLOCK_EFF_END (BASIC_BLOCK (max_index));
    NEXT_INSN (insn) = NULL_RTX;
    set_last_insn (insn);
  }

  /* Add jumps and labels to fixup blocks.  */
  for (i = 0; i < orig_num_blocks; i++)
    {
      int need_block = 0;
      basic_block bbi = BASIC_BLOCK (i);
      if (REORDER_BLOCK_ADD_JUMP (bbi))
	{
	  rtx label_insn, jump_insn, barrier_insn;

	  if (GET_CODE (REORDER_BLOCK_ADD_JUMP (bbi)->head) == CODE_LABEL)
	    label_insn  = REORDER_BLOCK_ADD_JUMP (bbi)->head;
	  else
	    {
	      rtx new_label = gen_label_rtx ();
	      label_insn = emit_label_before (new_label,
			      REORDER_BLOCK_ADD_JUMP (bbi)->head);
	      REORDER_BLOCK_ADD_JUMP (bbi)->head = label_insn;	 
	    }

	  if (GET_CODE (bbi->end) != JUMP_INSN)
	    {
	      jump_insn = emit_jump_insn_after (gen_jump (label_insn),
						bbi->end);
	      bbi->end = jump_insn;
	      need_block = 0;
	    }
	  else
	    {
	      jump_insn = emit_jump_insn_after (gen_jump (label_insn),
						REORDER_BLOCK_EFF_END (bbi));
	      need_block = 1;
	    }

	  JUMP_LABEL (jump_insn) = label_insn;
	  ++LABEL_NUSES (label_insn);
	  barrier_insn = emit_barrier_after (jump_insn);

	  /* Add block for jump.  Typically this is when a then is not
	     predicted and we are jumping to the moved then block.  */
	  if (need_block)
	    {
	      basic_block nb;

	      VARRAY_GROW (basic_block_info, ++n_basic_blocks);
	      create_basic_block (n_basic_blocks - 1, jump_insn,
				  jump_insn, NULL);
	      nb = BASIC_BLOCK (n_basic_blocks - 1);
	      nb->global_live_at_start
		= OBSTACK_ALLOC_REG_SET (function_obstack);
	      nb->global_live_at_end
		= OBSTACK_ALLOC_REG_SET (function_obstack);

	      COPY_REG_SET (nb->global_live_at_start,
			    bbi->global_live_at_start);
	      COPY_REG_SET (nb->global_live_at_end,
			    bbi->global_live_at_start);
	      BASIC_BLOCK (nb->index)->local_set = 0;

	      nb->aux = xcalloc (1, sizeof (struct reorder_block_def));
	      REORDER_BLOCK_INDEX (nb) = REORDER_BLOCK_INDEX (bbi) + 1;
	      /* Relink to new block.  */
	      nb->succ = bbi->succ;
	      nb->succ->src = nb;

	      make_edge (NULL, bbi, nb, 0);
	      bbi->succ->succ_next
		= bbi->succ->succ_next->succ_next;
	      nb->succ->succ_next = 0;
	      /* Fix reorder block index to reflect new block.  */
	      for (j = 0; j < n_basic_blocks - 1; j++)
		{
		  basic_block bbj = BASIC_BLOCK (j);
		  if (REORDER_BLOCK_INDEX (bbj)
		      >= REORDER_BLOCK_INDEX (bbi) + 1)
		    REORDER_BLOCK_INDEX (bbj)++;
		}
	      REORDER_BLOCK_SCOPE (nb) = REORDER_BLOCK_SCOPE (bbi);
	      REORDER_BLOCK_EFF_HEAD (nb) = nb->head;
	      REORDER_BLOCK_EFF_END (nb) = barrier_insn;
	    }
	  else
	    REORDER_BLOCK_EFF_END (bbi) = barrier_insn;
	}
    }
}


/* Perform sanity checks on the insn chain.
   1. Check that next/prev pointers are consistent in both the forward and
      reverse direction.
   2. Count insns in chain, going both directions, and check if equal.
   3. Check that get_last_insn () returns the actual end of chain.  */
#ifdef ENABLE_CHECKING
static void
verify_insn_chain ()
{
  rtx x,
      prevx,
      nextx;
  int insn_cnt1,
      insn_cnt2;

  prevx = NULL;
  insn_cnt1 = 1;
  for (x = get_insns (); x; x = NEXT_INSN (x))
    {
      if (PREV_INSN (x) != prevx)
	{
	  fprintf (stderr, "Forward traversal: insn chain corrupt.\n");
	  fprintf (stderr, "previous insn:\n");
	  debug_rtx (prevx);
	  fprintf (stderr, "current insn:\n");
	  debug_rtx (x);
	  abort ();
	}
      ++insn_cnt1;
      prevx = x;
    }

  if (prevx != get_last_insn ())
    {
      fprintf (stderr, "last_insn corrupt.\n");
      abort ();
    }

  nextx = NULL;
  insn_cnt2 = 1;
  for (x = get_last_insn (); x; x = PREV_INSN (x))
    {
      if (NEXT_INSN (x) != nextx)
	{
	  fprintf (stderr, "Reverse traversal: insn chain corrupt.\n");
	  fprintf (stderr, "current insn:\n");
	  debug_rtx (x);
	  fprintf (stderr, "next insn:\n");
	  debug_rtx (nextx);
	  abort ();
	}
      ++insn_cnt2;
      nextx = x;
    }

  if (insn_cnt1 != insn_cnt2)
    {
      fprintf (stderr, "insn_cnt1 (%d) not equal to insn_cnt2 (%d).\n",
	       insn_cnt1, insn_cnt2);
      abort ();
    }
}
#endif

static rtx
get_next_bb_note (x)
     rtx x;
{
  while (x)
    {
      if (GET_CODE (x) == NOTE
	  && NOTE_LINE_NUMBER (x) == NOTE_INSN_BASIC_BLOCK)
	return x;
      x = NEXT_INSN (x);
    }
  return NULL;
}


static rtx
get_prev_bb_note (x)
     rtx x;
{
  while (x)
    {
      if (GET_CODE (x) == NOTE
	  && NOTE_LINE_NUMBER (x) == NOTE_INSN_BASIC_BLOCK)
	return x;
      x = PREV_INSN (x);
    }
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
          rtx x1, x2;
	  /* Both notes are outside of any bbs. This implies that all the
	     basic blocks spanned by the pair of notes are contained in
             this scope. 
	     There is a degenerate case to consider. If the notes do not
	     span any basic blocks, then it is an empty scope that can
	     safely be deleted or ignored. Mark these with level = -1.  */

	  x1 = get_next_bb_note (s->note_beg);
	  x2 = get_prev_bb_note (s->note_end);
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
	if (! REORDER_BLOCK_SCOPE (BASIC_BLOCK (i)))
	  s->num_bbs++;

      s->bbs = xcalloc (s->num_bbs, sizeof (struct basic_block_def));
      for (i = bbi1; i <= bbi2; i++)
	{
	  basic_block curr_bb = BASIC_BLOCK (i);
	  if (! REORDER_BLOCK_SCOPE (curr_bb))
	    {
	      s->bbs[j++] = curr_bb;
	      REORDER_BLOCK_SCOPE (curr_bb) = s;
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
  new_scope->note_end = NULL;
  new_scope->bb_beg = NULL;
  new_scope->bb_end = NULL;
  new_scope->inner = NULL;
  new_scope->inner_last = NULL;
  new_scope->outer = NULL;
  new_scope->next = NULL;
  new_scope->num_bbs = 0;
  new_scope->bbs = NULL;
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
  scope root, curr_scope;

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
		  if (ntrees == 0)
		    forest->trees = xcalloc (1, sizeof (scope));
		  else
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
	} /* if note */

      if (curr_bb && curr_bb->end == x)
	{
	  curr_bb = NULL;
	  bbi++;
	}

    } /* for */

  for (i = 0; i < forest->num_trees; i++)
    relate_bbs_with_scopes (forest->trees[i]);
}


/* Remove all the NOTE_INSN_BLOCK_BEG and NOTE_INSN_BLOCK_END notes from
   the insn chain.  */

static void
remove_scope_notes ()
{
  rtx x, next;
  basic_block currbb = NULL;

  for (x = get_insns (); x; x = next)
    {
      next = NEXT_INSN (x);
      if (GET_CODE (x) == NOTE
	  && NOTE_LINE_NUMBER (x) == NOTE_INSN_BASIC_BLOCK)
	currbb = NOTE_BASIC_BLOCK (x);

      if (GET_CODE (x) == NOTE
	  && (NOTE_LINE_NUMBER (x) == NOTE_INSN_BLOCK_BEG
	      || NOTE_LINE_NUMBER (x) == NOTE_INSN_BLOCK_END))
	{
	  /* Check if the scope end happens to be the end of a bb.  */
	  if (currbb && x == currbb->end
	      && NOTE_LINE_NUMBER (x) == NOTE_INSN_BLOCK_END)
	    currbb->end = PREV_INSN (x);

	  if (PREV_INSN (x))
	    {
	      NEXT_INSN (PREV_INSN (x)) = next;
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
insert_intra_1 (s, ip)
     scope s;
     rtx *ip;
{
  scope p;

  if (NOTE_BLOCK (s->note_beg))
    {  
      *ip = emit_note_after (NOTE_INSN_BLOCK_BEG, *ip);
      NOTE_BLOCK (*ip) = NOTE_BLOCK (s->note_beg);
    } 

  for (p = s->inner; p; p = p->next)
    insert_intra_1 (p, ip);

  if (NOTE_BLOCK (s->note_beg))
    {  
      *ip = emit_note_after (NOTE_INSN_BLOCK_END, *ip);
      NOTE_BLOCK (*ip) = NOTE_BLOCK (s->note_end);
    }
}


/* Insert NOTE_INSN_BLOCK_END notes and NOTE_INSN_BLOCK_BEG notes for
   scopes that are contained within BB.  */

static void
insert_intra_bb_scope_notes (bb)
     basic_block bb;
{
  scope s = REORDER_BLOCK_SCOPE (bb);
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
	insert_intra_1 (p, &ip);
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
      scope s1 = REORDER_BLOCK_SCOPE (bb1);
      scope s2 = REORDER_BLOCK_SCOPE (bb2);
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
      scope s1 = REORDER_BLOCK_SCOPE (bb1);
      scope s2 = REORDER_BLOCK_SCOPE (bb2);
      while (s1 != s2)
	{
          if (! (s1 && s2))
	    abort ();
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
      scope s = REORDER_BLOCK_SCOPE (bb1);
      ip = REORDER_BLOCK_EFF_END (bb1);
      while (s != com)
	{
	  if (NOTE_BLOCK (s->note_beg))
	    {  
	      ip = emit_note_after (NOTE_INSN_BLOCK_END, ip);
	      NOTE_BLOCK (ip) = NOTE_BLOCK (s->note_end);
	    }
	  s = s->outer;
	}
    }

  /* Open scopes.  */
  if (bb2)
    {
      scope s = REORDER_BLOCK_SCOPE (bb2);
      ip = bb2->head;
      while (s != com)
	{
	  if (NOTE_BLOCK (s->note_beg))
	    {  
	      ip = emit_note_before (NOTE_INSN_BLOCK_BEG, ip);
	      NOTE_BLOCK (ip) = NOTE_BLOCK (s->note_beg);
	    }
	  s = s->outer;
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
      if (REORDER_BLOCK_SCOPE (bb1) != REORDER_BLOCK_SCOPE (bb2))
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
  if (forest->num_trees == 0)
    fprintf (stderr, "\n< Empty scope forest >\n");
  else
    {
      int i;
      fprintf (stderr, "\n< Scope forest >\n");
      for (i = 0; i < forest->num_trees; i++)
	dump_scope_forest_1 (forest->trees[i], 0);
    }
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
      && REORDER_BLOCK_SCOPE (s->bb_beg)
      && REORDER_BLOCK_SCOPE (s->bb_beg)->level + 1 == s->level)
    {
      fprintf (stderr, "%*s", indent, "");
      fprintf (stderr, "BB%d:\n", s->bb_beg->index);
    }

  fprintf (stderr, "%*s", indent, "");
  fprintf (stderr, "{ level %d (block %p)\n", s->level,
	   NOTE_BLOCK (s->note_beg));

  fprintf (stderr, "%*s%s", indent, "", "bbs:");
  for (i = 0; i < s->num_bbs; i++)
    fprintf (stderr, " %d", s->bbs[i]->index);
  fprintf (stderr, "\n");
  
  for (p = s->inner; p; p = p->next)
    dump_scope_forest_1 (p, indent + 2);

  fprintf (stderr, "%*s", indent, "");
  fprintf (stderr, "}\n");
}


/* Reorder basic blocks.  */

void
reorder_basic_blocks ()
{
  int i, j;
  struct loops loops_info;
  int num_loops;
  scope_forest_info forest;

  if (profile_arc_flag)
    return;

  if (n_basic_blocks <= 1)
    return;

  /* Exception edges are not currently handled.  */
  for (i = 0; i < n_basic_blocks; i++)
    {
      edge e;

      for (e = BASIC_BLOCK (i)->succ; e && ! (e->flags & EDGE_EH);
	   e = e->succ_next)
	continue;

      if (e && (e->flags & EDGE_EH))
	return;
    }

  reorder_index = 0;

  /* Find natural loops using the CFG.  */
  num_loops = flow_loops_find (&loops_info);

  /* Dump loop information.  */
  flow_loops_dump (&loops_info, rtl_dump_file, 0);

  reorder_last_visited = BASIC_BLOCK (0);

  for (i = 0; i < n_basic_blocks; i++)
    {
      basic_block bbi = BASIC_BLOCK (i);
      bbi->aux = xcalloc (1, sizeof (struct reorder_block_def));
      *((struct reorder_block_def *)bbi->aux) = rbd_init;
    }

  build_scope_forest (&forest);
  remove_scope_notes ();

  for (i = 0; i < n_basic_blocks; i++)
    {
      basic_block bbi = BASIC_BLOCK (i);
      REORDER_BLOCK_EFF_END (bbi)
	= skip_insns_between_block (bbi, REORDER_SKIP_AFTER);
      if (i == 0)
	REORDER_BLOCK_EFF_HEAD (bbi) = get_insns ();
      else 
	{
	  rtx prev_eff_end = REORDER_BLOCK_EFF_END (BASIC_BLOCK (i - 1));
	  REORDER_BLOCK_EFF_HEAD (bbi) = NEXT_INSN (prev_eff_end);
	}
    }

  /* If we've not got epilogue in RTL, we must fallthru to the exit.
     Force the last block to be at the end.  */
  /* ??? Some ABIs (e.g. MIPS) require the return insn to be at the
     end of the function for stack unwinding purposes.  */

#ifndef HAVE_epilogue
#define HAVE_epilogue 0
#endif

  if (! HAVE_epilogue)
    {
      basic_block last = BASIC_BLOCK (n_basic_blocks - 1);
      REORDER_BLOCK_INDEX (last) = n_basic_blocks - 1;
      REORDER_BLOCK_FLAGS (last) |= REORDER_BLOCK_VISITED;
    }
      
  make_reorder_chain (BASIC_BLOCK (0));

  fixup_reorder_chain ();

#ifdef ENABLE_CHECKING
  verify_insn_chain ();
#endif

  /* Put basic_block_info in new order.  */
  for (i = 0; i < n_basic_blocks - 1; i++)
    {
      for (j = i; i != REORDER_BLOCK_INDEX (BASIC_BLOCK (j)); j++)
	continue;

      if (REORDER_BLOCK_INDEX (BASIC_BLOCK (j)) == i
	  && i != j)
	{
	  basic_block tempbb;
	  int temprbi;
	  int rbi = REORDER_BLOCK_INDEX (BASIC_BLOCK (j));

	  temprbi = BASIC_BLOCK (rbi)->index;
	  BASIC_BLOCK (rbi)->index = BASIC_BLOCK (j)->index;
	  BASIC_BLOCK (j)->index = temprbi;
	  tempbb = BASIC_BLOCK (rbi);
	  BASIC_BLOCK (rbi) = BASIC_BLOCK (j);
	  BASIC_BLOCK (j) = tempbb;
	}
    }

  rebuild_scope_notes (&forest);
  free_scope_forest (&forest);
  reorder_blocks ();

#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif

  for (i = 0; i < n_basic_blocks; i++)
    free (BASIC_BLOCK (i)->aux);

  /* Free loop information.  */
  flow_loops_free (&loops_info);

}

