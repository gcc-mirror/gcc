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

   TODO:

   (1) Consider:

		if (p) goto A;		// predict taken
		foo ();
	      A:
		if (q) goto B;		// predict taken
		bar ();
	      B:
		baz ();
		return;

       We'll currently reorder this as

		if (!p) goto C;
	      A:
		if (!q) goto D;
	      B:
		baz ();
		return;
	      D:
		bar ();
		goto B;
	      C:
		foo ();
		goto A;

       A better ordering is

		if (!p) goto C;
		if (!q) goto D;
	      B:
		baz ();
		return;
	      C:
		foo ();
		if (q) goto B;
	      D:
		bar ();
		goto B;

       This requires that we be able to duplicate the jump at A, and
       adjust the graph traversal such that greedy placement doesn't
       fix D before C is considered.

   (2) Coordinate with shorten_branches to minimize the number of
       long branches.

   (3) Invent a method by which sufficiently non-predicted code can
       be moved to either the end of the section or another section
       entirely.  Some sort of NOTE_INSN note would work fine.

       This completely scroggs all debugging formats, so the user
       would have to explicitly ask for it.
*/

#include "config.h"
#include "system.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "insn-config.h"
#include "regs.h"
#include "flags.h"
#include "output.h"
#include "function.h"
#include "toplev.h"
#include "recog.h"
#include "expr.h"
#include "obstack.h"


#ifndef HAVE_epilogue
#define HAVE_epilogue 0
#endif


/* The contents of the current function definition are allocated
   in this obstack, and all are freed at the end of the function.
   For top-level functions, this is temporary_obstack.
   Separate obstacks are made for nested functions.  */

extern struct obstack flow_obstack;


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

/* Structure to hold information about the blocks during reordering.  */
typedef struct reorder_block_def
{
  rtx eff_head;
  rtx eff_end;
  scope scope;
  basic_block next;
  int index;
  int visited;
} *reorder_block_def;

#define RBI(BB)	((reorder_block_def) (BB)->aux)


/* Local function prototypes.  */
static rtx skip_insns_after_block	PARAMS ((basic_block));
static void record_effective_endpoints	PARAMS ((void));
static void make_reorder_chain		PARAMS ((void));
static basic_block make_reorder_chain_1	PARAMS ((basic_block, basic_block));
static rtx label_for_bb			PARAMS ((basic_block));
static rtx emit_jump_to_block_after	PARAMS ((basic_block, rtx));
static void fixup_reorder_chain		PARAMS ((void));
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
static rtx get_next_bb_note		PARAMS ((rtx));
static rtx get_prev_bb_note		PARAMS ((rtx));

void verify_insn_chain			PARAMS ((void));

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

  for (last_insn = insn = bb->end; (insn = NEXT_INSN (insn)); )
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
  /* It is possible to hit contradicting sequence.  For instance:
    
     jump_insn
     NOTE_INSN_LOOP_BEG
     barrier

     Where barrier belongs to jump_insn, but the note does not.
     This can be created by removing the basic block originally
     following NOTE_INSN_LOOP_BEG.

     In such case reorder the notes.  */
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


/* Locate the effective beginning and end of the insn chain for each
   block, as defined by skip_insns_after_block above.  */

static void
record_effective_endpoints ()
{
  rtx next_insn = get_insns ();
  int i;
  
  for (i = 0; i < n_basic_blocks; ++i)
    {
      basic_block bb = BASIC_BLOCK (i);
      rtx end;

      RBI (bb)->eff_head = next_insn;
      end = skip_insns_after_block (bb);
      RBI (bb)->eff_end = end;
      next_insn = NEXT_INSN (end);
    }
}


/* Compute an ordering for a subgraph beginning with block BB.  Record the
   ordering in RBI()->index and chained through RBI()->next.  */

static void
make_reorder_chain ()
{
  basic_block last_block = NULL;
  basic_block prev = NULL;
  int nbb_m1 = n_basic_blocks - 1;

  /* If we've not got epilogue in RTL, we must fallthru to the exit.
     Force the last block to be at the end.  */
  /* ??? Some ABIs (e.g. MIPS) require the return insn to be at the
     end of the function for stack unwinding purposes.  */
  if (! HAVE_epilogue)
    {
      last_block = BASIC_BLOCK (nbb_m1);
      RBI (last_block)->visited = 1;
      nbb_m1 -= 1;
    }

  /* Loop until we've placed every block.  */
  do
    {
      int i;
      basic_block next = NULL;

      /* Find the next unplaced block.  */
      /* ??? Get rid of this loop, and track which blocks are not yet
	 placed more directly, so as to avoid the O(N^2) worst case.
	 Perhaps keep a doubly-linked list of all to-be-placed blocks;
	 remove from the list as we place.  The head of that list is
	 what we're looking for here.  */

      for (i = 0; i <= nbb_m1; ++i)
	{
	  basic_block bb = BASIC_BLOCK (i);
	  if (! RBI (bb)->visited)
	    {
	      next = bb;
	      break;
	    }
	}
      if (! next)
	abort ();

      prev = make_reorder_chain_1 (next, prev);
    }
  while (RBI (prev)->index < nbb_m1);

  /* Terminate the chain.  */
  if (! HAVE_epilogue)
    {
      RBI (prev)->next = last_block;
      RBI (last_block)->index = RBI (prev)->index + 1;
      prev = last_block;
    }
  RBI (prev)->next = NULL;
}

/* A helper function for make_reorder_chain.

   We do not follow EH edges, or non-fallthru edges to noreturn blocks.
   These are assumed to be the error condition and we wish to cluster
   all of them at the very end of the function for the benefit of cache
   locality for the rest of the function.

   ??? We could do slightly better by noticing earlier that some subgraph
   has all paths leading to noreturn functions, but for there to be more
   than one block in such a subgraph is rare.  */

static basic_block
make_reorder_chain_1 (bb, prev)
     basic_block bb;
     basic_block prev;
{
  edge e;
  basic_block next;
  rtx note;

  /* Mark this block visited.  */
  if (prev)
    {
      int new_index;

 restart:
      RBI (prev)->next = bb;
      new_index = RBI (prev)->index + 1;
      RBI (bb)->index = new_index;

      if (rtl_dump_file && prev->index + 1 != bb->index)
	fprintf (rtl_dump_file, "Reordering block %d (%d) after %d (%d)\n",
		 bb->index, RBI (bb)->index, prev->index, RBI (prev)->index);
    }
  else
    RBI (bb)->index = 0;
  RBI (bb)->visited = 1;
  prev = bb;

  if (bb->succ == NULL)
    return prev;

  /* Find the most probable block.  */

  next = NULL;
  if (any_condjump_p (bb->end)
      && (note = find_reg_note (bb->end, REG_BR_PROB, 0)) != NULL)
    {
      int taken, probability;
      edge e_taken, e_fall;

      probability = INTVAL (XEXP (note, 0));
      taken = probability > REG_BR_PROB_BASE / 2;

      /* Find the normal taken edge and the normal fallthru edge.

	 Note, conditional jumps with other side effects may not
	 be fully optimized.  In this case it is possible for
	 the conditional jump to branch to the same location as
	 the fallthru path.

	 We should probably work to improve optimization of that
	 case; however, it seems silly not to also deal with such
	 problems here if they happen to occur.  */

      e_taken = e_fall = NULL;
      for (e = bb->succ; e ; e = e->succ_next)
	{
	  if (e->flags & EDGE_FALLTHRU)
	    e_fall = e;
	  else if (! (e->flags & EDGE_EH))
	    e_taken = e;
	}

      next = (taken ? e_taken : e_fall)->dest;
    }

  /* In the absence of a prediction, disturb things as little as possible
     by selecting the old "next" block from the list of successors.  If
     there had been a fallthru edge, that will be the one.  */
  if (! next)
    {
      for (e = bb->succ; e ; e = e->succ_next)
	if (e->dest->index == bb->index + 1)
	  {
	    if ((e->flags & EDGE_FALLTHRU)
	        || (e->dest->succ
	            && ! (e->flags & (EDGE_ABNORMAL_CALL | EDGE_EH))))
	      next = e->dest;
	    break;
	  }
    }

  /* Make sure we didn't select a silly next block.  */
  if (! next || next == EXIT_BLOCK_PTR || RBI (next)->visited)
    next = NULL;

  /* Recurse on the successors.  Unroll the last call, as the normal
     case is exactly one or two edges, and we can tail recurse.  */
  for (e = bb->succ; e; e = e->succ_next)
    if (e->dest != EXIT_BLOCK_PTR
	&& ! RBI (e->dest)->visited
	&& e->dest->succ
	&& ! (e->flags & (EDGE_ABNORMAL_CALL | EDGE_EH)))
      {
	if (next)
	  {
	    prev = make_reorder_chain_1 (next, prev);
	    next = RBI (e->dest)->visited ? NULL : e->dest;
	  }
	else
	  next = e->dest;
      }
  if (next)
    {
      bb = next;
      goto restart;
    }

  return prev;
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
	fprintf (rtl_dump_file, "Emitting label for block %d (%d)\n",
		 bb->index, RBI (bb)->index);

      label = emit_label_before (gen_label_rtx (), label);
      if (bb->head == RBI (bb)->eff_head)
	RBI (bb)->eff_head = label;
      bb->head = label;
    }

  return label;
}


/* Emit a jump to BB after insn AFTER.  */

static rtx
emit_jump_to_block_after (bb, after)
     basic_block bb;
     rtx after;
{
  rtx jump;

  if (bb != EXIT_BLOCK_PTR)
    {
      rtx label = label_for_bb (bb);
      jump = emit_jump_insn_after (gen_jump (label), after);
      JUMP_LABEL (jump) = label;
      LABEL_NUSES (label) += 1;
      if (basic_block_for_insn)
	set_block_for_new_insns (jump, bb);

      if (rtl_dump_file)
	fprintf (rtl_dump_file, "Emitting jump to block %d (%d)\n",
		 bb->index, RBI (bb)->index);
    }
  else
    {
#ifdef HAVE_return
      if (! HAVE_return)
	abort ();
      jump = emit_jump_insn_after (gen_return (), after);

      if (rtl_dump_file)
	fprintf (rtl_dump_file, "Emitting return\n");
#else
      abort ();
#endif
    }

  return jump;
}


/* Given a reorder chain, rearrange the code to match.  */

static void
fixup_reorder_chain ()
{
  basic_block bb, last_bb;

  /* First do the bulk reordering -- rechain the blocks without regard to
     the needed changes to jumps and labels.  */

  last_bb = BASIC_BLOCK (0);
  bb = RBI (last_bb)->next;
  while (bb)
    {
      rtx last_e = RBI (last_bb)->eff_end;
      rtx curr_h = RBI (bb)->eff_head;

      NEXT_INSN (last_e) = curr_h;
      PREV_INSN (curr_h) = last_e;

      last_bb = bb;
      bb = RBI (bb)->next;
    }
  NEXT_INSN (RBI (last_bb)->eff_end) = NULL_RTX;
  set_last_insn (RBI (last_bb)->eff_end);

  /* Now add jumps and labels as needed to match the blocks new
     outgoing edges.  */

  for (bb = BASIC_BLOCK (0); bb ; bb = RBI (bb)->next)
    {
      edge e_fall, e_taken, e;
      rtx jump_insn, barrier_insn, bb_end_insn;
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
	  if (any_uncondjump_p (bb_end_insn))
	    {
	      /* If the destination is still not next, nothing to do.  */
	      if (RBI (bb)->index + 1 != RBI (e_taken->dest)->index)
		continue;

	      /* Otherwise, we can remove the jump and cleanup the edge.  */
	      tidy_fallthru_edge (e_taken, bb, e_taken->dest);
	      RBI (bb)->eff_end = skip_insns_after_block (bb);
	      RBI (e_taken->dest)->eff_head = NEXT_INSN (RBI (bb)->eff_end);

	      if (rtl_dump_file)
		fprintf (rtl_dump_file, "Removing jump in block %d (%d)\n",
			 bb->index, RBI (bb)->index);
	      continue;
	    }
	  else if (any_condjump_p (bb_end_insn))
	    {
	      /* If the old fallthru is still next, nothing to do.  */
	      if (RBI (bb)->index + 1 == RBI (e_fall->dest)->index
	          || (RBI (bb)->index == n_basic_blocks - 1
		      && e_fall->dest == EXIT_BLOCK_PTR))
		continue;

	      /* There is one special case: if *neither* block is next,
		 such as happens at the very end of a function, then we'll
		 need to add a new unconditional jump.  Choose the taken
		 edge based on known or assumed probability.  */
	      if (RBI (bb)->index + 1 != RBI (e_taken->dest)->index)
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
	      if (RBI (bb)->index + 1 == RBI (e_fall->dest)->index)
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
	  if (RBI (bb)->index + 1 == RBI (e_fall->dest)->index
	      || (RBI (bb)->index == n_basic_blocks - 1
		  && e_fall->dest == EXIT_BLOCK_PTR))
	    continue;

	  /* We need a new jump insn.  If the block has only one outgoing
	     edge, then we can stuff the new jump insn in directly.  */
	  if (bb->succ->succ_next == NULL)
	    {
	      e_fall->flags &= ~EDGE_FALLTHRU;

	      jump_insn = emit_jump_to_block_after (e_fall->dest, bb_end_insn);
	      bb->end = jump_insn;
	      barrier_insn = emit_barrier_after (jump_insn);
	      RBI (bb)->eff_end = barrier_insn;
	      continue;
	    }
	}

      /* We got here if we need to add a new jump insn in a new block
	 across the edge e_fall.  */

      jump_insn = emit_jump_to_block_after (e_fall->dest, bb_end_insn);
      barrier_insn = emit_barrier_after (jump_insn);

      VARRAY_GROW (basic_block_info, ++n_basic_blocks);
      create_basic_block (n_basic_blocks - 1, jump_insn, jump_insn, NULL);

      nb = BASIC_BLOCK (n_basic_blocks - 1);
      nb->global_live_at_start = OBSTACK_ALLOC_REG_SET (&flow_obstack);
      nb->global_live_at_end = OBSTACK_ALLOC_REG_SET (&flow_obstack);
      nb->local_set = 0;
      nb->count = e_fall->count;
      nb->frequency = EDGE_FREQUENCY (e_fall);

      COPY_REG_SET (nb->global_live_at_start, bb->global_live_at_start);
      COPY_REG_SET (nb->global_live_at_end, bb->global_live_at_start);

      nb->aux = xmalloc (sizeof (struct reorder_block_def));
      RBI (nb)->eff_head = nb->head;
      RBI (nb)->eff_end = barrier_insn;
      RBI (nb)->scope = RBI (bb)->scope;
      RBI (nb)->index = RBI (bb)->index + 1;
      RBI (nb)->visited = 1;
      RBI (nb)->next = RBI (bb)->next;
      RBI (bb)->next = nb;

      /* Link to new block.  */
      make_edge (NULL, nb, e_fall->dest, 0);
      redirect_edge_succ (e_fall, nb);
      nb->succ->count = e_fall->count;
      nb->succ->probability = REG_BR_PROB_BASE;

      /* Don't process this new block.  */
      bb = nb;

      /* Fix subsequent reorder block indices to reflect new block.  */
      while ((nb = RBI (nb)->next) != NULL)
	RBI (nb)->index += 1;
    }

  /* Put basic_block_info in the new order.  */
  for (bb = BASIC_BLOCK (0); bb ; bb = RBI (bb)->next)
    {
      bb->index = RBI (bb)->index;
      BASIC_BLOCK (bb->index) = bb;
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

static rtx
get_next_bb_note (x)
     rtx x;
{
  while (x)
    {
      if (NOTE_INSN_BASIC_BLOCK_P (x))
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
      if (NOTE_INSN_BASIC_BLOCK_P (x))
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
      scope s = RBI (bb1)->scope;
      ip = RBI (bb1)->eff_end;
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
      scope s = RBI (bb2)->scope;
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


/* Reorder basic blocks.  The main entry point to this file.  */

void
reorder_basic_blocks ()
{
  scope_forest_info forest;
  int i;

  if (n_basic_blocks <= 1)
    return;

  for (i = 0; i < n_basic_blocks; i++)
    BASIC_BLOCK (i)->aux = xcalloc (1, sizeof (struct reorder_block_def));

  EXIT_BLOCK_PTR->aux = xcalloc (1, sizeof (struct reorder_block_def));

  build_scope_forest (&forest);
  remove_scope_notes ();

  record_effective_endpoints ();
  make_reorder_chain ();
  fixup_reorder_chain ();

#ifdef ENABLE_CHECKING
  verify_insn_chain ();
#endif

  rebuild_scope_notes (&forest);
  free_scope_forest (&forest);
  reorder_blocks ();

  for (i = 0; i < n_basic_blocks; i++)
    free (BASIC_BLOCK (i)->aux);

  free (EXIT_BLOCK_PTR->aux);

#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif
}
