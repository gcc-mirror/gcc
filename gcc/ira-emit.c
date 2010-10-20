/* Integrated Register Allocator.  Changing code and generating moves.
   Copyright (C) 2006, 2007, 2008, 2009
   Free Software Foundation, Inc.
   Contributed by Vladimir Makarov <vmakarov@redhat.com>.

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
#include "regs.h"
#include "rtl.h"
#include "tm_p.h"
#include "target.h"
#include "flags.h"
#include "obstack.h"
#include "bitmap.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "expr.h"
#include "recog.h"
#include "params.h"
#include "timevar.h"
#include "tree-pass.h"
#include "output.h"
#include "reload.h"
#include "errors.h"
#include "df.h"
#include "ira-int.h"


typedef struct move *move_t;

/* The structure represents an allocno move.  Both allocnos have the
   same origional regno but different allocation.  */
struct move
{
  /* The allocnos involved in the move.  */
  ira_allocno_t from, to;
  /* The next move in the move sequence.  */
  move_t next;
  /* Used for finding dependencies.  */
  bool visited_p;
  /* The size of the following array. */
  int deps_num;
  /* Moves on which given move depends on.  Dependency can be cyclic.
     It means we need a temporary to generates the moves.  Sequence
     A1->A2, B1->B2 where A1 and B2 are assigned to reg R1 and A2 and
     B1 are assigned to reg R2 is an example of the cyclic
     dependencies.  */
  move_t *deps;
  /* First insn generated for the move.  */
  rtx insn;
};

/* Array of moves (indexed by BB index) which should be put at the
   start/end of the corresponding basic blocks.  */
static move_t *at_bb_start, *at_bb_end;

/* Max regno before renaming some pseudo-registers.  For example, the
   same pseudo-register can be renamed in a loop if its allocation is
   different outside the loop.  */
static int max_regno_before_changing;

/* Return new move of allocnos TO and FROM.  */
static move_t
create_move (ira_allocno_t to, ira_allocno_t from)
{
  move_t move;

  move = (move_t) ira_allocate (sizeof (struct move));
  move->deps = NULL;
  move->deps_num = 0;
  move->to = to;
  move->from = from;
  move->next = NULL;
  move->insn = NULL_RTX;
  move->visited_p = false;
  return move;
}

/* Free memory for MOVE and its dependencies.  */
static void
free_move (move_t move)
{
  if (move->deps != NULL)
    ira_free (move->deps);
  ira_free (move);
}

/* Free memory for list of the moves given by its HEAD.  */
static void
free_move_list (move_t head)
{
  move_t next;

  for (; head != NULL; head = next)
    {
      next = head->next;
      free_move (head);
    }
}

/* Return TRUE if the the move list LIST1 and LIST2 are equal (two
   moves are equal if they involve the same allocnos).  */
static bool
eq_move_lists_p (move_t list1, move_t list2)
{
  for (; list1 != NULL && list2 != NULL;
       list1 = list1->next, list2 = list2->next)
    if (list1->from != list2->from || list1->to != list2->to)
      return false;
  return list1 == list2;
}

/* Print move list LIST into file F.  */
static void
print_move_list (FILE *f, move_t list)
{
  for (; list != NULL; list = list->next)
    fprintf (f, " a%dr%d->a%dr%d",
	     ALLOCNO_NUM (list->from), ALLOCNO_REGNO (list->from),
	     ALLOCNO_NUM (list->to), ALLOCNO_REGNO (list->to));
  fprintf (f, "\n");
}

extern void ira_debug_move_list (move_t list);

/* Print move list LIST into stderr.  */
void
ira_debug_move_list (move_t list)
{
  print_move_list (stderr, list);
}

/* This recursive function changes pseudo-registers in *LOC if it is
   necessary.  The function returns TRUE if a change was done.  */
static bool
change_regs (rtx *loc)
{
  int i, regno, result = false;
  const char *fmt;
  enum rtx_code code;
  rtx reg;

  if (*loc == NULL_RTX)
    return false;
  code = GET_CODE (*loc);
  if (code == REG)
    {
      regno = REGNO (*loc);
      if (regno < FIRST_PSEUDO_REGISTER)
	return false;
      if (regno >= max_regno_before_changing)
	/* It is a shared register which was changed already.  */
	return false;
      if (ira_curr_regno_allocno_map[regno] == NULL)
	return false;
      reg = ALLOCNO_REG (ira_curr_regno_allocno_map[regno]);
      if (reg == *loc)
	return false;
      *loc = reg;
      return true;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	result = change_regs (&XEXP (*loc, i)) || result;
      else if (fmt[i] == 'E')
	{
	  int j;

	  for (j = XVECLEN (*loc, i) - 1; j >= 0; j--)
	    result = change_regs (&XVECEXP (*loc, i, j)) || result;
	}
    }
  return result;
}

/* Attach MOVE to the edge E.  The move is attached to the head of the
   list if HEAD_P is TRUE.  */
static void
add_to_edge_list (edge e, move_t move, bool head_p)
{
  move_t last;

  if (head_p || e->aux == NULL)
    {
      move->next = (move_t) e->aux;
      e->aux = move;
    }
  else
    {
      for (last = (move_t) e->aux; last->next != NULL; last = last->next)
	;
      last->next = move;
      move->next = NULL;
    }
}

/* Create and return new pseudo-register with the same attributes as
   ORIGINAL_REG.  */
static rtx
create_new_reg (rtx original_reg)
{
  rtx new_reg;

  new_reg = gen_reg_rtx (GET_MODE (original_reg));
  ORIGINAL_REGNO (new_reg) = ORIGINAL_REGNO (original_reg);
  REG_USERVAR_P (new_reg) = REG_USERVAR_P (original_reg);
  REG_POINTER (new_reg) = REG_POINTER (original_reg);
  REG_ATTRS (new_reg) = REG_ATTRS (original_reg);
  if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
    fprintf (ira_dump_file, "      Creating newreg=%i from oldreg=%i\n",
	     REGNO (new_reg), REGNO (original_reg));
  return new_reg;
}

/* Return TRUE if loop given by SUBNODE inside the loop given by
   NODE.  */
static bool
subloop_tree_node_p (ira_loop_tree_node_t subnode, ira_loop_tree_node_t node)
{
  for (; subnode != NULL; subnode = subnode->parent)
    if (subnode == node)
      return true;
  return false;
}

/* Set up member `reg' to REG for allocnos which has the same regno as
   ALLOCNO and which are inside the loop corresponding to ALLOCNO. */
static void
set_allocno_reg (ira_allocno_t allocno, rtx reg)
{
  int regno;
  ira_allocno_t a;
  ira_loop_tree_node_t node;

  node = ALLOCNO_LOOP_TREE_NODE (allocno);
  for (a = ira_regno_allocno_map[ALLOCNO_REGNO (allocno)];
       a != NULL;
       a = ALLOCNO_NEXT_REGNO_ALLOCNO (a))
    if (subloop_tree_node_p (ALLOCNO_LOOP_TREE_NODE (a), node))
      ALLOCNO_REG (a) = reg;
  for (a = ALLOCNO_CAP (allocno); a != NULL; a = ALLOCNO_CAP (a))
    ALLOCNO_REG (a) = reg;
  regno = ALLOCNO_REGNO (allocno);
  for (a = allocno;;)
    {
      if (a == NULL || (a = ALLOCNO_CAP (a)) == NULL)
	{
	  node = node->parent;
	  if (node == NULL)
	    break;
	  a = node->regno_allocno_map[regno];
	}
      if (a == NULL)
	continue;
      if (ALLOCNO_CHILD_RENAMED_P (a))
	break;
      ALLOCNO_CHILD_RENAMED_P (a) = true;
    }
}

/* Return true if there is an entry to given loop not from its parent
   (or grandparent) block.  For example, it is possible for two
   adjacent loops inside another loop.  */
static bool
entered_from_non_parent_p (ira_loop_tree_node_t loop_node)
{
  ira_loop_tree_node_t bb_node, src_loop_node, parent;
  edge e;
  edge_iterator ei;

  for (bb_node = loop_node->children; bb_node != NULL; bb_node = bb_node->next)
    if (bb_node->bb != NULL)
      {
	FOR_EACH_EDGE (e, ei, bb_node->bb->preds)
	  if (e->src != ENTRY_BLOCK_PTR
	      && (src_loop_node = IRA_BB_NODE (e->src)->parent) != loop_node)
	    {
	      for (parent = src_loop_node->parent;
		   parent != NULL;
		   parent = parent->parent)
		if (parent == loop_node)
		  break;
	      if (parent != NULL)
		/* That is an exit from a nested loop -- skip it.  */
		continue;
	      for (parent = loop_node->parent;
		   parent != NULL;
		   parent = parent->parent)
		if (src_loop_node == parent)
		  break;
	      if (parent == NULL)
		return true;
	    }
      }
  return false;
}

/* Set up ENTERED_FROM_NON_PARENT_P for each loop region.  */
static void
setup_entered_from_non_parent_p (void)
{
  unsigned int i;
  loop_p loop;

  for (i = 0; VEC_iterate (loop_p, ira_loops.larray, i, loop); i++)
    if (ira_loop_nodes[i].regno_allocno_map != NULL)
      ira_loop_nodes[i].entered_from_non_parent_p
	= entered_from_non_parent_p (&ira_loop_nodes[i]);
}

/* Return TRUE if move of SRC_ALLOCNO (assigned to hard register) to
   DEST_ALLOCNO (assigned to memory) can be removed beacuse it does
   not change value of the destination.  One possible reason for this
   is the situation when SRC_ALLOCNO is not modified in the
   corresponding loop.  */
static bool
store_can_be_removed_p (ira_allocno_t src_allocno, ira_allocno_t dest_allocno)
{
  int regno, orig_regno;
  ira_allocno_t a;
  ira_loop_tree_node_t node;

  ira_assert (ALLOCNO_CAP_MEMBER (src_allocno) == NULL
	      && ALLOCNO_CAP_MEMBER (dest_allocno) == NULL);
  orig_regno = ALLOCNO_REGNO (src_allocno);
  regno = REGNO (ALLOCNO_REG (dest_allocno));
  for (node = ALLOCNO_LOOP_TREE_NODE (src_allocno);
       node != NULL;
       node = node->parent)
    {
      a = node->regno_allocno_map[orig_regno];
      ira_assert (a != NULL);
      if (REGNO (ALLOCNO_REG (a)) == (unsigned) regno)
	/* We achieved the destination and everything is ok.  */
	return true;
      else if (bitmap_bit_p (node->modified_regnos, orig_regno))
	return false;
      else if (node->entered_from_non_parent_p)
	/* If there is a path from a destination loop block to the
	   source loop header containing basic blocks of non-parents
	   (grandparents) of the source loop, we should have checked
	   modifications of the pseudo on this path too to decide
	   about possibility to remove the store.  It could be done by
	   solving a data-flow problem.  Unfortunately such global
	   solution would complicate IR flattening.  Therefore we just
	   prohibit removal of the store in such complicated case.  */
	return false;
    }
  /* It is actually a loop entry -- do not remove the store.  */
  return false;
}

/* Generate and attach moves to the edge E.  This looks at the final
   regnos of allocnos living on the edge with the same original regno
   to figure out when moves should be generated.  */
static void
generate_edge_moves (edge e)
{
  ira_loop_tree_node_t src_loop_node, dest_loop_node;
  unsigned int regno;
  bitmap_iterator bi;
  ira_allocno_t src_allocno, dest_allocno, *src_map, *dest_map;
  move_t move;

  src_loop_node = IRA_BB_NODE (e->src)->parent;
  dest_loop_node = IRA_BB_NODE (e->dest)->parent;
  e->aux = NULL;
  if (src_loop_node == dest_loop_node)
    return;
  src_map = src_loop_node->regno_allocno_map;
  dest_map = dest_loop_node->regno_allocno_map;
  EXECUTE_IF_SET_IN_REG_SET (DF_LR_IN (e->dest),
			     FIRST_PSEUDO_REGISTER, regno, bi)
    if (bitmap_bit_p (DF_LR_OUT (e->src), regno))
      {
	src_allocno = src_map[regno];
	dest_allocno = dest_map[regno];
	if (REGNO (ALLOCNO_REG (src_allocno))
	    == REGNO (ALLOCNO_REG (dest_allocno)))
	  continue;
	/* Remove unnecessary stores at the region exit.  We should do
	   this for readonly memory for sure and this is guaranteed by
	   that we never generate moves on region borders (see
	   checking ira_reg_equiv_invariant_p in function
	   change_loop).  */
 	if (ALLOCNO_HARD_REGNO (dest_allocno) < 0
	    && ALLOCNO_HARD_REGNO (src_allocno) >= 0
	    && store_can_be_removed_p (src_allocno, dest_allocno))
	  {
	    ALLOCNO_MEM_OPTIMIZED_DEST (src_allocno) = dest_allocno;
	    ALLOCNO_MEM_OPTIMIZED_DEST_P (dest_allocno) = true;
	    if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
	      fprintf (ira_dump_file, "      Remove r%d:a%d->a%d(mem)\n",
		       regno, ALLOCNO_NUM (src_allocno),
		       ALLOCNO_NUM (dest_allocno));
	    continue;
	  }
	move = create_move (dest_allocno, src_allocno);
	add_to_edge_list (e, move, true);
    }
}

/* Bitmap of allocnos local for the current loop.  */
static bitmap local_allocno_bitmap;

/* This bitmap is used to find that we need to generate and to use a
   new pseudo-register when processing allocnos with the same original
   regno.  */
static bitmap used_regno_bitmap;

/* This bitmap contains regnos of allocnos which were renamed locally
   because the allocnos correspond to disjoint live ranges in loops
   with a common parent.  */
static bitmap renamed_regno_bitmap;

/* Change (if necessary) pseudo-registers inside loop given by loop
   tree node NODE.  */
static void
change_loop (ira_loop_tree_node_t node)
{
  bitmap_iterator bi;
  unsigned int i;
  int regno;
  bool used_p;
  ira_allocno_t allocno, parent_allocno, *map;
  rtx insn, original_reg;
  enum reg_class cover_class;
  ira_loop_tree_node_t parent;

  if (node != ira_loop_tree_root)
    {

      if (node->bb != NULL)
	{
	  FOR_BB_INSNS (node->bb, insn)
	    if (INSN_P (insn) && change_regs (&insn))
	      {
		df_insn_rescan (insn);
		df_notes_rescan (insn);
	      }
	  return;
	}

      if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
	fprintf (ira_dump_file,
		 "      Changing RTL for loop %d (header bb%d)\n",
		 node->loop->num, node->loop->header->index);

      parent = ira_curr_loop_tree_node->parent;
      map = parent->regno_allocno_map;
      EXECUTE_IF_SET_IN_REG_SET (ira_curr_loop_tree_node->border_allocnos,
				 0, i, bi)
	{
	  allocno = ira_allocnos[i];
	  regno = ALLOCNO_REGNO (allocno);
	  cover_class = ALLOCNO_COVER_CLASS (allocno);
	  parent_allocno = map[regno];
	  ira_assert (regno < ira_reg_equiv_len);
	  /* We generate the same hard register move because the
	     reload pass can put an allocno into memory in this case
	     we will have live range splitting.  If it does not happen
	     such the same hard register moves will be removed.  The
	     worst case when the both allocnos are put into memory by
	     the reload is very rare.  */
	  if (parent_allocno != NULL
	      && (ALLOCNO_HARD_REGNO (allocno)
		  == ALLOCNO_HARD_REGNO (parent_allocno))
	      && (ALLOCNO_HARD_REGNO (allocno) < 0
		  || (parent->reg_pressure[cover_class] + 1
		      <= ira_available_class_regs[cover_class])
		  || TEST_HARD_REG_BIT (ira_prohibited_mode_move_regs
					[ALLOCNO_MODE (allocno)],
					ALLOCNO_HARD_REGNO (allocno))
		  /* don't create copies because reload can spill an
		     allocno set by copy although the allocno will not
		     get memory slot.  */
		  || ira_reg_equiv_invariant_p[regno]
		  || ira_reg_equiv_const[regno] != NULL_RTX))
	    continue;
	  original_reg = ALLOCNO_REG (allocno);
	  if (parent_allocno == NULL
	      || REGNO (ALLOCNO_REG (parent_allocno)) == REGNO (original_reg))
	    {
	      if (internal_flag_ira_verbose > 3 && ira_dump_file)
		fprintf (ira_dump_file, "  %i vs parent %i:",
			 ALLOCNO_HARD_REGNO (allocno),
			 ALLOCNO_HARD_REGNO (parent_allocno));
	      set_allocno_reg (allocno, create_new_reg (original_reg));
	    }
	}
    }
  /* Rename locals: Local allocnos with same regno in different loops
     might get the different hard register.  So we need to change
     ALLOCNO_REG.  */
  bitmap_and_compl (local_allocno_bitmap,
		    ira_curr_loop_tree_node->all_allocnos,
		    ira_curr_loop_tree_node->border_allocnos);
  EXECUTE_IF_SET_IN_REG_SET (local_allocno_bitmap, 0, i, bi)
    {
      allocno = ira_allocnos[i];
      regno = ALLOCNO_REGNO (allocno);
      if (ALLOCNO_CAP_MEMBER (allocno) != NULL)
	continue;
      used_p = bitmap_bit_p (used_regno_bitmap, regno);
      bitmap_set_bit (used_regno_bitmap, regno);
      ALLOCNO_SOMEWHERE_RENAMED_P (allocno) = true;
      if (! used_p)
	continue;
      bitmap_set_bit (renamed_regno_bitmap, regno);
      set_allocno_reg (allocno, create_new_reg (ALLOCNO_REG (allocno)));
    }
}

/* Process to set up flag somewhere_renamed_p.  */
static void
set_allocno_somewhere_renamed_p (void)
{
  unsigned int regno;
  ira_allocno_t allocno;
  ira_allocno_iterator ai;

  FOR_EACH_ALLOCNO (allocno, ai)
    {
      regno = ALLOCNO_REGNO (allocno);
      if (bitmap_bit_p (renamed_regno_bitmap, regno)
	  && REGNO (ALLOCNO_REG (allocno)) == regno)
	ALLOCNO_SOMEWHERE_RENAMED_P (allocno) = true;
    }
}

/* Return TRUE if move lists on all edges given in vector VEC are
   equal.  */
static bool
eq_edge_move_lists_p (VEC(edge,gc) *vec)
{
  move_t list;
  int i;

  list = (move_t) EDGE_I (vec, 0)->aux;
  for (i = EDGE_COUNT (vec) - 1; i > 0; i--)
    if (! eq_move_lists_p (list, (move_t) EDGE_I (vec, i)->aux))
      return false;
  return true;
}

/* Look at all entry edges (if START_P) or exit edges of basic block
   BB and put move lists at the BB start or end if it is possible.  In
   other words, this decreases code duplication of allocno moves.  */
static void
unify_moves (basic_block bb, bool start_p)
{
  int i;
  edge e;
  move_t list;
  VEC(edge,gc) *vec;

  vec = (start_p ? bb->preds : bb->succs);
  if (EDGE_COUNT (vec) == 0 || ! eq_edge_move_lists_p (vec))
    return;
  e = EDGE_I (vec, 0);
  list = (move_t) e->aux;
  if (! start_p && control_flow_insn_p (BB_END (bb)))
    return;
  e->aux = NULL;
  for (i = EDGE_COUNT (vec) - 1; i > 0; i--)
    {
      e = EDGE_I (vec, i);
      free_move_list ((move_t) e->aux);
      e->aux = NULL;
    }
  if (start_p)
    at_bb_start[bb->index] = list;
  else
    at_bb_end[bb->index] = list;
}

/* Last move (in move sequence being processed) setting up the
   corresponding hard register.  */
static move_t hard_regno_last_set[FIRST_PSEUDO_REGISTER];

/* If the element value is equal to CURR_TICK then the corresponding
   element in `hard_regno_last_set' is defined and correct.  */
static int hard_regno_last_set_check[FIRST_PSEUDO_REGISTER];

/* Last move (in move sequence being processed) setting up the
   corresponding allocno.  */
static move_t *allocno_last_set;

/* If the element value is equal to CURR_TICK then the corresponding
   element in . `allocno_last_set' is defined and correct.  */
static int *allocno_last_set_check;

/* Definition of vector of moves.  */
DEF_VEC_P(move_t);
DEF_VEC_ALLOC_P(move_t, heap);

/* This vec contains moves sorted topologically (depth-first) on their
   dependency graph.  */
static VEC(move_t,heap) *move_vec;

/* The variable value is used to check correctness of values of
   elements of arrays `hard_regno_last_set' and
   `allocno_last_set_check'.  */
static int curr_tick;

/* This recursive function traverses dependencies of MOVE and produces
   topological sorting (in depth-first order).  */
static void
traverse_moves (move_t move)
{
  int i;

  if (move->visited_p)
    return;
  move->visited_p = true;
  for (i = move->deps_num - 1; i >= 0; i--)
    traverse_moves (move->deps[i]);
  VEC_safe_push (move_t, heap, move_vec, move);
}

/* Remove unnecessary moves in the LIST, makes topological sorting,
   and removes cycles on hard reg dependencies by introducing new
   allocnos assigned to memory and additional moves.  It returns the
   result move list.  */
static move_t
modify_move_list (move_t list)
{
  int i, n, nregs, hard_regno;
  ira_allocno_t to, from, new_allocno;
  move_t move, new_move, set_move, first, last;

  if (list == NULL)
    return NULL;
  /* Creat move deps.  */
  curr_tick++;
  for (move = list; move != NULL; move = move->next)
    {
      to = move->to;
      if ((hard_regno = ALLOCNO_HARD_REGNO (to)) < 0)
	continue;
      nregs = hard_regno_nregs[hard_regno][ALLOCNO_MODE (to)];
      for (i = 0; i < nregs; i++)
	{
	  hard_regno_last_set[hard_regno + i] = move;
	  hard_regno_last_set_check[hard_regno + i] = curr_tick;
	}
    }
  for (move = list; move != NULL; move = move->next)
    {
      from = move->from;
      to = move->to;
      if ((hard_regno = ALLOCNO_HARD_REGNO (from)) >= 0)
	{
	  nregs = hard_regno_nregs[hard_regno][ALLOCNO_MODE (from)];
	  for (n = i = 0; i < nregs; i++)
	    if (hard_regno_last_set_check[hard_regno + i] == curr_tick
		&& (ALLOCNO_REGNO (hard_regno_last_set[hard_regno + i]->to)
		    != ALLOCNO_REGNO (from)))
	      n++;
	  move->deps = (move_t *) ira_allocate (n * sizeof (move_t));
	  for (n = i = 0; i < nregs; i++)
	    if (hard_regno_last_set_check[hard_regno + i] == curr_tick
		&& (ALLOCNO_REGNO (hard_regno_last_set[hard_regno + i]->to)
		    != ALLOCNO_REGNO (from)))
	      move->deps[n++] = hard_regno_last_set[hard_regno + i];
	  move->deps_num = n;
	}
    }
  /* Toplogical sorting:  */
  VEC_truncate (move_t, move_vec, 0);
  for (move = list; move != NULL; move = move->next)
    traverse_moves (move);
  last = NULL;
  for (i = (int) VEC_length (move_t, move_vec) - 1; i >= 0; i--)
    {
      move = VEC_index (move_t, move_vec, i);
      move->next = NULL;
      if (last != NULL)
	last->next = move;
      last = move;
    }
  first = VEC_last (move_t, move_vec);
  /* Removing cycles:  */
  curr_tick++;
  VEC_truncate (move_t, move_vec, 0);
  for (move = first; move != NULL; move = move->next)
    {
      from = move->from;
      to = move->to;
      if ((hard_regno = ALLOCNO_HARD_REGNO (from)) >= 0)
	{
	  nregs = hard_regno_nregs[hard_regno][ALLOCNO_MODE (from)];
	  for (i = 0; i < nregs; i++)
	    if (hard_regno_last_set_check[hard_regno + i] == curr_tick
		&& ALLOCNO_HARD_REGNO
		   (hard_regno_last_set[hard_regno + i]->to) >= 0)
	      {
		set_move = hard_regno_last_set[hard_regno + i];
		/* It does not matter what loop_tree_node (of TO or
		   FROM) to use for the new allocno because of
		   subsequent IRA internal representation
		   flattening.  */
		new_allocno
		  = ira_create_allocno (ALLOCNO_REGNO (set_move->to), false,
					ALLOCNO_LOOP_TREE_NODE (set_move->to));
		ALLOCNO_MODE (new_allocno) = ALLOCNO_MODE (set_move->to);
		ira_set_allocno_cover_class
		  (new_allocno, ALLOCNO_COVER_CLASS (set_move->to));
		ALLOCNO_ASSIGNED_P (new_allocno) = true;
		ALLOCNO_HARD_REGNO (new_allocno) = -1;
		ALLOCNO_REG (new_allocno)
		  = create_new_reg (ALLOCNO_REG (set_move->to));
		ALLOCNO_CONFLICT_ID (new_allocno) = ALLOCNO_NUM (new_allocno);
		/* Make it possibly conflicting with all earlier
		   created allocnos.  Cases where temporary allocnos
		   created to remove the cycles are quite rare.  */
		ALLOCNO_MIN (new_allocno) = 0;
		ALLOCNO_MAX (new_allocno) = ira_allocnos_num - 1;
		new_move = create_move (set_move->to, new_allocno);
		set_move->to = new_allocno;
		VEC_safe_push (move_t, heap, move_vec, new_move);
		ira_move_loops_num++;
		if (internal_flag_ira_verbose > 2 && ira_dump_file != NULL)
		  fprintf (ira_dump_file,
			   "    Creating temporary allocno a%dr%d\n",
			   ALLOCNO_NUM (new_allocno),
			   REGNO (ALLOCNO_REG (new_allocno)));
	      }
	}
      if ((hard_regno = ALLOCNO_HARD_REGNO (to)) < 0)
	continue;
      nregs = hard_regno_nregs[hard_regno][ALLOCNO_MODE (to)];
      for (i = 0; i < nregs; i++)
	{
	  hard_regno_last_set[hard_regno + i] = move;
	  hard_regno_last_set_check[hard_regno + i] = curr_tick;
	}
    }
  for (i = (int) VEC_length (move_t, move_vec) - 1; i >= 0; i--)
    {
      move = VEC_index (move_t, move_vec, i);
      move->next = NULL;
      last->next = move;
      last = move;
    }
  return first;
}

/* Generate RTX move insns from the move list LIST.  This updates
   allocation cost using move execution frequency FREQ.  */
static rtx
emit_move_list (move_t list, int freq)
{
  int cost;
  rtx result, insn;
  enum machine_mode mode;
  enum reg_class cover_class;

  start_sequence ();
  for (; list != NULL; list = list->next)
    {
      start_sequence ();
      emit_move_insn (ALLOCNO_REG (list->to), ALLOCNO_REG (list->from));
      list->insn = get_insns ();
      end_sequence ();
      /* The reload needs to have set up insn codes.  If the reload
	 sets up insn codes by itself, it may fail because insns will
	 have hard registers instead of pseudos and there may be no
	 machine insn with given hard registers.  */
      for (insn = list->insn; insn != NULL_RTX; insn = NEXT_INSN (insn))
	recog_memoized (insn);
      emit_insn (list->insn);
      mode = ALLOCNO_MODE (list->to);
      cover_class = ALLOCNO_COVER_CLASS (list->to);
      cost = 0;
      if (ALLOCNO_HARD_REGNO (list->to) < 0)
	{
	  if (ALLOCNO_HARD_REGNO (list->from) >= 0)
	    {
	      cost = ira_memory_move_cost[mode][cover_class][0] * freq;
	      ira_store_cost += cost;
	    }
	}
      else if (ALLOCNO_HARD_REGNO (list->from) < 0)
	{
	  if (ALLOCNO_HARD_REGNO (list->to) >= 0)
	    {
	      cost = ira_memory_move_cost[mode][cover_class][0] * freq;
	      ira_load_cost += cost;
	    }
	}
      else
	{
	  cost = (ira_get_register_move_cost (mode, cover_class, cover_class)
		  * freq);
	  ira_shuffle_cost += cost;
	}
      ira_overall_cost += cost;
    }
  result = get_insns ();
  end_sequence ();
  return result;
}

/* Generate RTX move insns from move lists attached to basic blocks
   and edges.  */
static void
emit_moves (void)
{
  basic_block bb;
  edge_iterator ei;
  edge e;
  rtx insns, tmp;

  FOR_EACH_BB (bb)
    {
      if (at_bb_start[bb->index] != NULL)
	{
	  at_bb_start[bb->index] = modify_move_list (at_bb_start[bb->index]);
	  insns = emit_move_list (at_bb_start[bb->index],
				  REG_FREQ_FROM_BB (bb));
	  tmp = BB_HEAD (bb);
	  if (LABEL_P (tmp))
	    tmp = NEXT_INSN (tmp);
	  if (NOTE_INSN_BASIC_BLOCK_P (tmp))
	    tmp = NEXT_INSN (tmp);
	  if (tmp == BB_HEAD (bb))
	    emit_insn_before (insns, tmp);
	  else if (tmp != NULL_RTX)
	    emit_insn_after (insns, PREV_INSN (tmp));
	  else
	    emit_insn_after (insns, get_last_insn ());
	}

      if (at_bb_end[bb->index] != NULL)
	{
	  at_bb_end[bb->index] = modify_move_list (at_bb_end[bb->index]);
	  insns = emit_move_list (at_bb_end[bb->index], REG_FREQ_FROM_BB (bb));
	  ira_assert (! control_flow_insn_p (BB_END (bb)));
	  emit_insn_after (insns, BB_END (bb));
	}

      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  if (e->aux == NULL)
	    continue;
	  ira_assert ((e->flags & EDGE_ABNORMAL) == 0
		      || ! EDGE_CRITICAL_P (e));
	  e->aux = modify_move_list ((move_t) e->aux);
	  insert_insn_on_edge
	    (emit_move_list ((move_t) e->aux,
			     REG_FREQ_FROM_EDGE_FREQ (EDGE_FREQUENCY (e))),
	     e);
	  if (e->src->next_bb != e->dest)
	    ira_additional_jumps_num++;
	}
    }
}

/* Update costs of A and corresponding allocnos on upper levels on the
   loop tree from reading (if READ_P) or writing A on an execution
   path with FREQ.  */
static void
update_costs (ira_allocno_t a, bool read_p, int freq)
{
  ira_loop_tree_node_t parent;

  for (;;)
    {
      ALLOCNO_NREFS (a)++;
      ALLOCNO_FREQ (a) += freq;
      ALLOCNO_MEMORY_COST (a)
	+= (ira_memory_move_cost[ALLOCNO_MODE (a)][ALLOCNO_COVER_CLASS (a)]
	    [read_p ? 1 : 0] * freq);
      if (ALLOCNO_CAP (a) != NULL)
	a = ALLOCNO_CAP (a);
      else if ((parent = ALLOCNO_LOOP_TREE_NODE (a)->parent) == NULL
	       || (a = parent->regno_allocno_map[ALLOCNO_REGNO (a)]) == NULL)
	break;
    }
}

/* Process moves from LIST with execution FREQ to add ranges, copies,
   and modify costs for allocnos involved in the moves.  All regnos
   living through the list is in LIVE_THROUGH, and the loop tree node
   used to find corresponding allocnos is NODE.  */
static void
add_range_and_copies_from_move_list (move_t list, ira_loop_tree_node_t node,
				     bitmap live_through, int freq)
{
  int start, n;
  unsigned int regno;
  move_t move;
  ira_allocno_t to, from, a;
  ira_copy_t cp;
  allocno_live_range_t r;
  bitmap_iterator bi;
  HARD_REG_SET hard_regs_live;

  if (list == NULL)
    return;
  n = 0;
  EXECUTE_IF_SET_IN_BITMAP (live_through, FIRST_PSEUDO_REGISTER, regno, bi)
    n++;
  REG_SET_TO_HARD_REG_SET (hard_regs_live, live_through);
  /* This is a trick to guarantee that new ranges is not merged with
     the old ones.  */
  ira_max_point++;
  start = ira_max_point;
  for (move = list; move != NULL; move = move->next)
    {
      from = move->from;
      to = move->to;
      if (ALLOCNO_CONFLICT_ALLOCNO_ARRAY (to) == NULL)
	{
	  if (internal_flag_ira_verbose > 2 && ira_dump_file != NULL)
	    fprintf (ira_dump_file, "    Allocate conflicts for a%dr%d\n",
		     ALLOCNO_NUM (to), REGNO (ALLOCNO_REG (to)));
	  ira_allocate_allocno_conflicts (to, n);
	}
      bitmap_clear_bit (live_through, ALLOCNO_REGNO (from));
      bitmap_clear_bit (live_through, ALLOCNO_REGNO (to));
      IOR_HARD_REG_SET (ALLOCNO_CONFLICT_HARD_REGS (from), hard_regs_live);
      IOR_HARD_REG_SET (ALLOCNO_CONFLICT_HARD_REGS (to), hard_regs_live);
      IOR_HARD_REG_SET (ALLOCNO_TOTAL_CONFLICT_HARD_REGS (from),
			hard_regs_live);
      IOR_HARD_REG_SET (ALLOCNO_TOTAL_CONFLICT_HARD_REGS (to), hard_regs_live);
      update_costs (from, true, freq);
      update_costs (to, false, freq);
      cp = ira_add_allocno_copy (from, to, freq, false, move->insn, NULL);
      if (internal_flag_ira_verbose > 2 && ira_dump_file != NULL)
	fprintf (ira_dump_file, "    Adding cp%d:a%dr%d-a%dr%d\n",
		 cp->num, ALLOCNO_NUM (cp->first),
		 REGNO (ALLOCNO_REG (cp->first)), ALLOCNO_NUM (cp->second),
		 REGNO (ALLOCNO_REG (cp->second)));
      r = ALLOCNO_LIVE_RANGES (from);
      if (r == NULL || r->finish >= 0)
	{
	  ALLOCNO_LIVE_RANGES (from)
	    = ira_create_allocno_live_range (from, start, ira_max_point, r);
	  if (internal_flag_ira_verbose > 2 && ira_dump_file != NULL)
	    fprintf (ira_dump_file,
		     "    Adding range [%d..%d] to allocno a%dr%d\n",
		     start, ira_max_point, ALLOCNO_NUM (from),
		     REGNO (ALLOCNO_REG (from)));
	}
      else
	{
	  r->finish = ira_max_point;
	  if (internal_flag_ira_verbose > 2 && ira_dump_file != NULL)
	    fprintf (ira_dump_file,
		     "    Adding range [%d..%d] to allocno a%dr%d\n",
		     r->start, ira_max_point, ALLOCNO_NUM (from),
		     REGNO (ALLOCNO_REG (from)));
	}
      ira_max_point++;
      ALLOCNO_LIVE_RANGES (to)
	= ira_create_allocno_live_range (to, ira_max_point, -1,
					 ALLOCNO_LIVE_RANGES (to));
      ira_max_point++;
    }
  for (move = list; move != NULL; move = move->next)
    {
      r = ALLOCNO_LIVE_RANGES (move->to);
      if (r->finish < 0)
	{
	  r->finish = ira_max_point - 1;
	  if (internal_flag_ira_verbose > 2 && ira_dump_file != NULL)
	    fprintf (ira_dump_file,
		     "    Adding range [%d..%d] to allocno a%dr%d\n",
		     r->start, r->finish, ALLOCNO_NUM (move->to),
		     REGNO (ALLOCNO_REG (move->to)));
	}
    }
  EXECUTE_IF_SET_IN_BITMAP (live_through, FIRST_PSEUDO_REGISTER, regno, bi)
    {
      a = node->regno_allocno_map[regno];
      if ((to = ALLOCNO_MEM_OPTIMIZED_DEST (a)) != NULL)
	a = to;
      ALLOCNO_LIVE_RANGES (a)
	= ira_create_allocno_live_range (a, start, ira_max_point - 1,
					 ALLOCNO_LIVE_RANGES (a));
      if (internal_flag_ira_verbose > 2 && ira_dump_file != NULL)
	fprintf
	  (ira_dump_file,
	   "    Adding range [%d..%d] to live through %s allocno a%dr%d\n",
	   start, ira_max_point - 1,
	   to != NULL ? "upper level" : "",
	   ALLOCNO_NUM (a), REGNO (ALLOCNO_REG (a)));
    }
}

/* Process all move list to add ranges, conflicts, copies, and modify
   costs for allocnos involved in the moves.  */
static void
add_ranges_and_copies (void)
{
  basic_block bb;
  edge_iterator ei;
  edge e;
  ira_loop_tree_node_t node;
  bitmap live_through;

  live_through = ira_allocate_bitmap ();
  FOR_EACH_BB (bb)
    {
      /* It does not matter what loop_tree_node (of source or
	 destination block) to use for searching allocnos by their
	 regnos because of subsequent IR flattening.  */
      node = IRA_BB_NODE (bb)->parent;
      bitmap_copy (live_through, DF_LR_IN (bb));
      add_range_and_copies_from_move_list
	(at_bb_start[bb->index], node, live_through, REG_FREQ_FROM_BB (bb));
      bitmap_copy (live_through, DF_LR_OUT (bb));
      add_range_and_copies_from_move_list
	(at_bb_end[bb->index], node, live_through, REG_FREQ_FROM_BB (bb));
      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  bitmap_and (live_through, DF_LR_IN (e->dest), DF_LR_OUT (bb));
	  add_range_and_copies_from_move_list
	    ((move_t) e->aux, node, live_through,
	     REG_FREQ_FROM_EDGE_FREQ (EDGE_FREQUENCY (e)));
	}
    }
  ira_free_bitmap (live_through);
}

/* The entry function changes code and generates shuffling allocnos on
   region borders for the regional (LOOPS_P is TRUE in this case)
   register allocation.  */
void
ira_emit (bool loops_p)
{
  basic_block bb;
  rtx insn;
  edge_iterator ei;
  edge e;
  ira_allocno_t a;
  ira_allocno_iterator ai;

  FOR_EACH_ALLOCNO (a, ai)
    ALLOCNO_REG (a) = regno_reg_rtx[ALLOCNO_REGNO (a)];
  if (! loops_p)
    return;
  at_bb_start = (move_t *) ira_allocate (sizeof (move_t) * last_basic_block);
  memset (at_bb_start, 0, sizeof (move_t) * last_basic_block);
  at_bb_end = (move_t *) ira_allocate (sizeof (move_t) * last_basic_block);
  memset (at_bb_end, 0, sizeof (move_t) * last_basic_block);
  local_allocno_bitmap = ira_allocate_bitmap ();
  used_regno_bitmap = ira_allocate_bitmap ();
  renamed_regno_bitmap = ira_allocate_bitmap ();
  max_regno_before_changing = max_reg_num ();
  ira_traverse_loop_tree (true, ira_loop_tree_root, change_loop, NULL);
  set_allocno_somewhere_renamed_p ();
  ira_free_bitmap (used_regno_bitmap);
  ira_free_bitmap (renamed_regno_bitmap);
  ira_free_bitmap (local_allocno_bitmap);
  setup_entered_from_non_parent_p ();
  FOR_EACH_BB (bb)
    {
      at_bb_start[bb->index] = NULL;
      at_bb_end[bb->index] = NULL;
      FOR_EACH_EDGE (e, ei, bb->succs)
	if (e->dest != EXIT_BLOCK_PTR)
	  generate_edge_moves (e);
    }
  allocno_last_set
    = (move_t *) ira_allocate (sizeof (move_t) * max_reg_num ());
  allocno_last_set_check
    = (int *) ira_allocate (sizeof (int) * max_reg_num ());
  memset (allocno_last_set_check, 0, sizeof (int) * max_reg_num ());
  memset (hard_regno_last_set_check, 0, sizeof (hard_regno_last_set_check));
  curr_tick = 0;
  FOR_EACH_BB (bb)
    unify_moves (bb, true);
  FOR_EACH_BB (bb)
    unify_moves (bb, false);
  move_vec = VEC_alloc (move_t, heap, ira_allocnos_num);
  emit_moves ();
  add_ranges_and_copies ();
  /* Clean up: */
  FOR_EACH_BB (bb)
    {
      free_move_list (at_bb_start[bb->index]);
      free_move_list (at_bb_end[bb->index]);
      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  free_move_list ((move_t) e->aux);
	  e->aux = NULL;
	}
    }
  VEC_free (move_t, heap, move_vec);
  ira_free (allocno_last_set_check);
  ira_free (allocno_last_set);
  commit_edge_insertions ();
  /* Fix insn codes.  It is necessary to do it before reload because
     reload assumes initial insn codes defined.  The insn codes can be
     invalidated by CFG infrastructure for example in jump
     redirection.  */
  FOR_EACH_BB (bb)
    FOR_BB_INSNS_REVERSE (bb, insn)
      if (INSN_P (insn))
	recog_memoized (insn);
  ira_free (at_bb_end);
  ira_free (at_bb_start);
}
