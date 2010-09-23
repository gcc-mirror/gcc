/* IRA allocation based on graph coloring.
   Copyright (C) 2006, 2007, 2008, 2009, 2010
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
#include "rtl.h"
#include "tm_p.h"
#include "target.h"
#include "regs.h"
#include "flags.h"
#include "sbitmap.h"
#include "bitmap.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "expr.h"
#include "toplev.h"
#include "reload.h"
#include "params.h"
#include "df.h"
#include "splay-tree.h"
#include "ira-int.h"

/* This file contains code for regional graph coloring, spill/restore
   code placement optimization, and code helping the reload pass to do
   a better job.  */

/* Bitmap of allocnos which should be colored.  */
static bitmap coloring_allocno_bitmap;

/* Bitmap of allocnos which should be taken into account during
   coloring.  In general case it contains allocnos from
   coloring_allocno_bitmap plus other already colored conflicting
   allocnos.  */
static bitmap consideration_allocno_bitmap;

/* TRUE if we coalesced some allocnos.  In other words, if we got
   loops formed by members first_coalesced_allocno and
   next_coalesced_allocno containing more one allocno.  */
static bool allocno_coalesced_p;

/* Bitmap used to prevent a repeated allocno processing because of
   coalescing.  */
static bitmap processed_coalesced_allocno_bitmap;

/* All allocnos sorted according their priorities.  */
static ira_allocno_t *sorted_allocnos;

/* Vec representing the stack of allocnos used during coloring.  */
static VEC(ira_allocno_t,heap) *allocno_stack_vec;

/* Array used to choose an allocno for spilling.  */
static ira_allocno_t *allocnos_for_spilling;

/* Pool for splay tree nodes.  */
static alloc_pool splay_tree_node_pool;

/* When an allocno is removed from the splay tree, it is put in the
   following vector for subsequent inserting it into the splay tree
   after putting all colorable allocnos onto the stack.  The allocno
   could be removed from and inserted to the splay tree every time
   when its spilling priority is changed but such solution would be
   more costly although simpler.  */
static VEC(ira_allocno_t,heap) *removed_splay_allocno_vec;

/* Helper for qsort comparison callbacks - return a positive integer if
   X > Y, or a negative value otherwise.  Use a conditional expression
   instead of a difference computation to insulate from possible overflow
   issues, e.g. X - Y < 0 for some X > 0 and Y < 0.  */
#define SORTGT(x,y) (((x) > (y)) ? 1 : -1)



/* This page contains functions used to find conflicts using allocno
   live ranges.  */

/* Return TRUE if live ranges of allocnos A1 and A2 intersect.  It is
   used to find a conflict for new allocnos or allocnos with the
   different cover classes.  */
static bool
allocnos_have_intersected_live_ranges_p (ira_allocno_t a1, ira_allocno_t a2)
{
  if (a1 == a2)
    return false;
  if (ALLOCNO_REG (a1) != NULL && ALLOCNO_REG (a2) != NULL
      && (ORIGINAL_REGNO (ALLOCNO_REG (a1))
	  == ORIGINAL_REGNO (ALLOCNO_REG (a2))))
    return false;
  return ira_allocno_live_ranges_intersect_p (ALLOCNO_LIVE_RANGES (a1),
					      ALLOCNO_LIVE_RANGES (a2));
}

#ifdef ENABLE_IRA_CHECKING

/* Return TRUE if live ranges of pseudo-registers REGNO1 and REGNO2
   intersect.  This should be used when there is only one region.
   Currently this is used during reload.  */
static bool
pseudos_have_intersected_live_ranges_p (int regno1, int regno2)
{
  ira_allocno_t a1, a2;

  ira_assert (regno1 >= FIRST_PSEUDO_REGISTER
	      && regno2 >= FIRST_PSEUDO_REGISTER);
  /* Reg info caclulated by dataflow infrastructure can be different
     from one calculated by regclass.  */
  if ((a1 = ira_loop_tree_root->regno_allocno_map[regno1]) == NULL
      || (a2 = ira_loop_tree_root->regno_allocno_map[regno2]) == NULL)
    return false;
  return allocnos_have_intersected_live_ranges_p (a1, a2);
}

#endif



/* This page contains functions used to choose hard registers for
   allocnos.  */

/* Array whose element value is TRUE if the corresponding hard
   register was already allocated for an allocno.  */
static bool allocated_hardreg_p[FIRST_PSEUDO_REGISTER];

/* Describes one element in a queue of allocnos whose costs need to be
   updated.  Each allocno in the queue is known to have a cover class.  */
struct update_cost_queue_elem
{
  /* This element is in the queue iff CHECK == update_cost_check.  */
  int check;

  /* COST_HOP_DIVISOR**N, where N is the length of the shortest path
     connecting this allocno to the one being allocated.  */
  int divisor;

  /* The next allocno in the queue, or null if this is the last element.  */
  ira_allocno_t next;
};

/* The first element in a queue of allocnos whose copy costs need to be
   updated.  Null if the queue is empty.  */
static ira_allocno_t update_cost_queue;

/* The last element in the queue described by update_cost_queue.
   Not valid if update_cost_queue is null.  */
static struct update_cost_queue_elem *update_cost_queue_tail;

/* A pool of elements in the queue described by update_cost_queue.
   Elements are indexed by ALLOCNO_NUM.  */
static struct update_cost_queue_elem *update_cost_queue_elems;

/* The current value of update_copy_cost call count.  */
static int update_cost_check;

/* Allocate and initialize data necessary for function
   update_copy_costs.  */
static void
initiate_cost_update (void)
{
  size_t size;

  size = ira_allocnos_num * sizeof (struct update_cost_queue_elem);
  update_cost_queue_elems
    = (struct update_cost_queue_elem *) ira_allocate (size);
  memset (update_cost_queue_elems, 0, size);
  update_cost_check = 0;
}

/* Deallocate data used by function update_copy_costs.  */
static void
finish_cost_update (void)
{
  ira_free (update_cost_queue_elems);
}

/* When we traverse allocnos to update hard register costs, the cost
   divisor will be multiplied by the following macro value for each
   hop from given allocno to directly connected allocnos.  */
#define COST_HOP_DIVISOR 4

/* Start a new cost-updating pass.  */
static void
start_update_cost (void)
{
  update_cost_check++;
  update_cost_queue = NULL;
}

/* Add (ALLOCNO, DIVISOR) to the end of update_cost_queue,
   unless ALLOCNO is already in the queue, or has no cover class.  */
static inline void
queue_update_cost (ira_allocno_t allocno, int divisor)
{
  struct update_cost_queue_elem *elem;

  elem = &update_cost_queue_elems[ALLOCNO_NUM (allocno)];
  if (elem->check != update_cost_check
      && ALLOCNO_COVER_CLASS (allocno) != NO_REGS)
    {
      elem->check = update_cost_check;
      elem->divisor = divisor;
      elem->next = NULL;
      if (update_cost_queue == NULL)
	update_cost_queue = allocno;
      else
	update_cost_queue_tail->next = allocno;
      update_cost_queue_tail = elem;
    }
}

/* Try to remove the first element from update_cost_queue.  Return false
   if the queue was empty, otherwise make (*ALLOCNO, *DIVISOR) describe
   the removed element.  */
static inline bool
get_next_update_cost (ira_allocno_t *allocno, int *divisor)
{
  struct update_cost_queue_elem *elem;

  if (update_cost_queue == NULL)
    return false;

  *allocno = update_cost_queue;
  elem = &update_cost_queue_elems[ALLOCNO_NUM (*allocno)];
  *divisor = elem->divisor;
  update_cost_queue = elem->next;
  return true;
}

/* Update the cost of allocnos to increase chances to remove some
   copies as the result of subsequent assignment.  */
static void
update_copy_costs (ira_allocno_t allocno, bool decr_p)
{
  int i, cost, update_cost, hard_regno, divisor;
  enum machine_mode mode;
  enum reg_class rclass, cover_class;
  ira_allocno_t another_allocno;
  ira_copy_t cp, next_cp;

  hard_regno = ALLOCNO_HARD_REGNO (allocno);
  ira_assert (hard_regno >= 0);

  cover_class = ALLOCNO_COVER_CLASS (allocno);
  if (cover_class == NO_REGS)
    return;
  i = ira_class_hard_reg_index[cover_class][hard_regno];
  ira_assert (i >= 0);
  rclass = REGNO_REG_CLASS (hard_regno);

  start_update_cost ();
  divisor = 1;
  do
    {
      mode = ALLOCNO_MODE (allocno);
      for (cp = ALLOCNO_COPIES (allocno); cp != NULL; cp = next_cp)
	{
	  if (cp->first == allocno)
	    {
	      next_cp = cp->next_first_allocno_copy;
	      another_allocno = cp->second;
	    }
	  else if (cp->second == allocno)
	    {
	      next_cp = cp->next_second_allocno_copy;
	      another_allocno = cp->first;
	    }
	  else
	    gcc_unreachable ();

	  cover_class = ALLOCNO_COVER_CLASS (another_allocno);
	  if (! ira_reg_classes_intersect_p[rclass][cover_class]
	      || ALLOCNO_ASSIGNED_P (another_allocno))
	    continue;

	  cost = (cp->second == allocno
		  ? ira_get_register_move_cost (mode, rclass, cover_class)
		  : ira_get_register_move_cost (mode, cover_class, rclass));
	  if (decr_p)
	    cost = -cost;

	  update_cost = cp->freq * cost / divisor;
	  if (update_cost == 0)
	    continue;

	  ira_allocate_and_set_or_copy_costs
	    (&ALLOCNO_UPDATED_HARD_REG_COSTS (another_allocno), cover_class,
	     ALLOCNO_UPDATED_COVER_CLASS_COST (another_allocno),
	     ALLOCNO_HARD_REG_COSTS (another_allocno));
	  ira_allocate_and_set_or_copy_costs
	    (&ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS (another_allocno),
	     cover_class, 0,
	     ALLOCNO_CONFLICT_HARD_REG_COSTS (another_allocno));
	  i = ira_class_hard_reg_index[cover_class][hard_regno];
	  ira_assert (i >= 0);
	  ALLOCNO_UPDATED_HARD_REG_COSTS (another_allocno)[i] += update_cost;
	  ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS (another_allocno)[i]
	    += update_cost;

	  queue_update_cost (another_allocno, divisor * COST_HOP_DIVISOR);
	}
    }
  while (get_next_update_cost (&allocno, &divisor));
}

/* This function updates COSTS (decrease if DECR_P) for hard_registers
   of COVER_CLASS by conflict costs of the unassigned allocnos
   connected by copies with allocnos in update_cost_queue.  This
   update increases chances to remove some copies.  */
static void
update_conflict_hard_regno_costs (int *costs, enum reg_class cover_class,
				  bool decr_p)
{
  int i, cost, class_size, freq, mult, div, divisor;
  int index, hard_regno;
  int *conflict_costs;
  bool cont_p;
  enum reg_class another_cover_class;
  ira_allocno_t allocno, another_allocno;
  ira_copy_t cp, next_cp;

  while (get_next_update_cost (&allocno, &divisor))
    for (cp = ALLOCNO_COPIES (allocno); cp != NULL; cp = next_cp)
      {
	if (cp->first == allocno)
	  {
	    next_cp = cp->next_first_allocno_copy;
	    another_allocno = cp->second;
	  }
	else if (cp->second == allocno)
	  {
	    next_cp = cp->next_second_allocno_copy;
	    another_allocno = cp->first;
	  }
	else
	  gcc_unreachable ();
 	another_cover_class = ALLOCNO_COVER_CLASS (another_allocno);
 	if (! ira_reg_classes_intersect_p[cover_class][another_cover_class]
	    || ALLOCNO_ASSIGNED_P (another_allocno)
	    || ALLOCNO_MAY_BE_SPILLED_P (ALLOCNO_FIRST_COALESCED_ALLOCNO
					 (another_allocno)))
	  continue;
	class_size = ira_class_hard_regs_num[another_cover_class];
	ira_allocate_and_copy_costs
	  (&ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS (another_allocno),
	   another_cover_class,
	   ALLOCNO_CONFLICT_HARD_REG_COSTS (another_allocno));
	conflict_costs
	  = ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS (another_allocno);
	if (conflict_costs == NULL)
	  cont_p = true;
	else
	  {
	    mult = cp->freq;
	    freq = ALLOCNO_FREQ (another_allocno);
	    if (freq == 0)
	      freq = 1;
	    div = freq * divisor;
	    cont_p = false;
	    for (i = class_size - 1; i >= 0; i--)
	      {
		hard_regno = ira_class_hard_regs[another_cover_class][i];
		ira_assert (hard_regno >= 0);
		index = ira_class_hard_reg_index[cover_class][hard_regno];
		if (index < 0)
		  continue;
		cost = conflict_costs [i] * mult / div;
		if (cost == 0)
		  continue;
		cont_p = true;
		if (decr_p)
		  cost = -cost;
		costs[index] += cost;
	      }
	  }
	/* Probably 5 hops will be enough.  */
	if (cont_p
	    && divisor <= (COST_HOP_DIVISOR
			   * COST_HOP_DIVISOR
			   * COST_HOP_DIVISOR
			   * COST_HOP_DIVISOR))
	  queue_update_cost (another_allocno, divisor * COST_HOP_DIVISOR);
      }
}

/* Sort allocnos according to the profit of usage of a hard register
   instead of memory for them. */
static int
allocno_cost_compare_func (const void *v1p, const void *v2p)
{
  ira_allocno_t p1 = *(const ira_allocno_t *) v1p;
  ira_allocno_t p2 = *(const ira_allocno_t *) v2p;
  int c1, c2;

  c1 = ALLOCNO_UPDATED_MEMORY_COST (p1) - ALLOCNO_UPDATED_COVER_CLASS_COST (p1);
  c2 = ALLOCNO_UPDATED_MEMORY_COST (p2) - ALLOCNO_UPDATED_COVER_CLASS_COST (p2);
  if (c1 - c2)
    return c1 - c2;

  /* If regs are equally good, sort by allocno numbers, so that the
     results of qsort leave nothing to chance.  */
  return ALLOCNO_NUM (p1) - ALLOCNO_NUM (p2);
}

/* Print all allocnos coalesced with ALLOCNO.  */
static void
print_coalesced_allocno (ira_allocno_t allocno)
{
  ira_allocno_t a;

  for (a = ALLOCNO_NEXT_COALESCED_ALLOCNO (allocno);;
       a = ALLOCNO_NEXT_COALESCED_ALLOCNO (a))
    {
      ira_print_expanded_allocno (a);
      if (a == allocno)
	break;
      fprintf (ira_dump_file, "+");
    }
}

/* Choose a hard register for ALLOCNO (or for all coalesced allocnos
   represented by ALLOCNO).  If RETRY_P is TRUE, it means that the
   function called from function `ira_reassign_conflict_allocnos' and
   `allocno_reload_assign'.  This function implements the optimistic
   coalescing too: if we failed to assign a hard register to set of
   the coalesced allocnos, we put them onto the coloring stack for
   subsequent separate assigning.  */
static bool
assign_hard_reg (ira_allocno_t allocno, bool retry_p)
{
  HARD_REG_SET conflicting_regs;
  int i, j, k, hard_regno, best_hard_regno, class_size;
  int cost, mem_cost, min_cost, full_cost, min_full_cost, add_cost;
  int *a_costs;
  int *conflict_costs;
  enum reg_class cover_class, rclass, conflict_cover_class;
  enum machine_mode mode;
  ira_allocno_t a, conflict_allocno;
  ira_allocno_conflict_iterator aci;
  static int costs[FIRST_PSEUDO_REGISTER], full_costs[FIRST_PSEUDO_REGISTER];
#ifdef STACK_REGS
  bool no_stack_reg_p;
#endif

  ira_assert (! ALLOCNO_ASSIGNED_P (allocno));
  cover_class = ALLOCNO_COVER_CLASS (allocno);
  class_size = ira_class_hard_regs_num[cover_class];
  mode = ALLOCNO_MODE (allocno);
  CLEAR_HARD_REG_SET (conflicting_regs);
  best_hard_regno = -1;
  memset (full_costs, 0, sizeof (int) * class_size);
  mem_cost = 0;
  if (allocno_coalesced_p)
    bitmap_clear (processed_coalesced_allocno_bitmap);
  memset (costs, 0, sizeof (int) * class_size);
  memset (full_costs, 0, sizeof (int) * class_size);
#ifdef STACK_REGS
  no_stack_reg_p = false;
#endif
  start_update_cost ();
  for (a = ALLOCNO_NEXT_COALESCED_ALLOCNO (allocno);;
       a = ALLOCNO_NEXT_COALESCED_ALLOCNO (a))
    {
      mem_cost += ALLOCNO_UPDATED_MEMORY_COST (a);
      IOR_HARD_REG_SET (conflicting_regs,
			ALLOCNO_TOTAL_CONFLICT_HARD_REGS (a));
      ira_allocate_and_copy_costs (&ALLOCNO_UPDATED_HARD_REG_COSTS (a),
				   cover_class, ALLOCNO_HARD_REG_COSTS (a));
      a_costs = ALLOCNO_UPDATED_HARD_REG_COSTS (a);
#ifdef STACK_REGS
      no_stack_reg_p = no_stack_reg_p || ALLOCNO_TOTAL_NO_STACK_REG_P (a);
#endif
      for (cost = ALLOCNO_UPDATED_COVER_CLASS_COST (a), i = 0;
	   i < class_size;
	   i++)
	if (a_costs != NULL)
	  {
	    costs[i] += a_costs[i];
	    full_costs[i] += a_costs[i];
	  }
	else
	  {
	    costs[i] += cost;
	    full_costs[i] += cost;
	  }
      /* Take preferences of conflicting allocnos into account.  */
      FOR_EACH_ALLOCNO_CONFLICT (a, conflict_allocno, aci)
	/* Reload can give another class so we need to check all
	   allocnos.  */
	if (retry_p || bitmap_bit_p (consideration_allocno_bitmap,
				     ALLOCNO_NUM (conflict_allocno)))
	  {
	    conflict_cover_class = ALLOCNO_COVER_CLASS (conflict_allocno);
	    ira_assert (ira_reg_classes_intersect_p
			[cover_class][conflict_cover_class]);
	    if (allocno_coalesced_p)
	      {
		if (bitmap_bit_p (processed_coalesced_allocno_bitmap,
				  ALLOCNO_NUM (conflict_allocno)))
		  continue;
		bitmap_set_bit (processed_coalesced_allocno_bitmap,
				ALLOCNO_NUM (conflict_allocno));
	      }
	    if (ALLOCNO_ASSIGNED_P (conflict_allocno))
	      {
		if ((hard_regno = ALLOCNO_HARD_REGNO (conflict_allocno)) >= 0
		    && ira_class_hard_reg_index[cover_class][hard_regno] >= 0)
		  {
		    IOR_HARD_REG_SET
		      (conflicting_regs,
		       ira_reg_mode_hard_regset
		       [hard_regno][ALLOCNO_MODE (conflict_allocno)]);
		    if (hard_reg_set_subset_p (reg_class_contents[cover_class],
					       conflicting_regs))
		      goto fail;
		  }
	      }
	    else if (! ALLOCNO_MAY_BE_SPILLED_P (ALLOCNO_FIRST_COALESCED_ALLOCNO
						 (conflict_allocno)))
	      {
		ira_allocate_and_copy_costs
		  (&ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS (conflict_allocno),
		   conflict_cover_class,
		   ALLOCNO_CONFLICT_HARD_REG_COSTS (conflict_allocno));
		conflict_costs
		  = ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS (conflict_allocno);
		if (conflict_costs != NULL)
		  for (j = class_size - 1; j >= 0; j--)
 		    {
 		      hard_regno = ira_class_hard_regs[cover_class][j];
 		      ira_assert (hard_regno >= 0);
 		      k = (ira_class_hard_reg_index
 			   [conflict_cover_class][hard_regno]);
 		      if (k < 0)
 			continue;
 		      full_costs[j] -= conflict_costs[k];
 		    }
		queue_update_cost (conflict_allocno, COST_HOP_DIVISOR);
	      }
	  }
      if (a == allocno)
	break;
    }
  /* Take into account preferences of allocnos connected by copies to
     the conflict allocnos.  */
  update_conflict_hard_regno_costs (full_costs, cover_class, true);

  /* Take preferences of allocnos connected by copies into
     account.  */
  start_update_cost ();
  for (a = ALLOCNO_NEXT_COALESCED_ALLOCNO (allocno);;
       a = ALLOCNO_NEXT_COALESCED_ALLOCNO (a))
    {
      queue_update_cost (a, COST_HOP_DIVISOR);
      if (a == allocno)
	break;
    }
  update_conflict_hard_regno_costs (full_costs, cover_class, false);
  min_cost = min_full_cost = INT_MAX;
  /* We don't care about giving callee saved registers to allocnos no
     living through calls because call clobbered registers are
     allocated first (it is usual practice to put them first in
     REG_ALLOC_ORDER).  */
  for (i = 0; i < class_size; i++)
    {
      hard_regno = ira_class_hard_regs[cover_class][i];
#ifdef STACK_REGS
      if (no_stack_reg_p
	  && FIRST_STACK_REG <= hard_regno && hard_regno <= LAST_STACK_REG)
	continue;
#endif
      if (! ira_hard_reg_not_in_set_p (hard_regno, mode, conflicting_regs)
	  || TEST_HARD_REG_BIT (prohibited_class_mode_regs[cover_class][mode],
				hard_regno))
	continue;
      cost = costs[i];
      full_cost = full_costs[i];
      if (! allocated_hardreg_p[hard_regno]
	  && ira_hard_reg_not_in_set_p (hard_regno, mode, call_used_reg_set))
	/* We need to save/restore the hard register in
	   epilogue/prologue.  Therefore we increase the cost.  */
	{
	  /* ??? If only part is call clobbered.  */
	  rclass = REGNO_REG_CLASS (hard_regno);
	  add_cost = (ira_memory_move_cost[mode][rclass][0]
		      + ira_memory_move_cost[mode][rclass][1] - 1);
	  cost += add_cost;
	  full_cost += add_cost;
	}
      if (min_cost > cost)
	min_cost = cost;
      if (min_full_cost > full_cost)
	{
	  min_full_cost = full_cost;
	  best_hard_regno = hard_regno;
	  ira_assert (hard_regno >= 0);
	}
    }
  if (min_full_cost > mem_cost)
    {
      if (! retry_p && internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
	fprintf (ira_dump_file, "(memory is more profitable %d vs %d) ",
		 mem_cost, min_full_cost);
      best_hard_regno = -1;
    }
 fail:
  if (flag_ira_algorithm != IRA_ALGORITHM_PRIORITY
      && best_hard_regno < 0
      && ALLOCNO_NEXT_COALESCED_ALLOCNO (allocno) != allocno)
    {
      for (j = 0, a = ALLOCNO_NEXT_COALESCED_ALLOCNO (allocno);;
	   a = ALLOCNO_NEXT_COALESCED_ALLOCNO (a))
	{
	  ira_assert (! ALLOCNO_IN_GRAPH_P (a));
	  sorted_allocnos[j++] = a;
	  if (a == allocno)
	    break;
	}
      qsort (sorted_allocnos, j, sizeof (ira_allocno_t),
	     allocno_cost_compare_func);
      for (i = 0; i < j; i++)
	{
	  a = sorted_allocnos[i];
	  ALLOCNO_FIRST_COALESCED_ALLOCNO (a) = a;
	  ALLOCNO_NEXT_COALESCED_ALLOCNO (a) = a;
	  VEC_safe_push (ira_allocno_t, heap, allocno_stack_vec, a);
	  if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
	    {
	      fprintf (ira_dump_file, "        Pushing");
	      print_coalesced_allocno (a);
	      fprintf (ira_dump_file, "\n");
	    }
	}
      return false;
    }
  if (best_hard_regno >= 0)
    allocated_hardreg_p[best_hard_regno] = true;
  for (a = ALLOCNO_NEXT_COALESCED_ALLOCNO (allocno);;
       a = ALLOCNO_NEXT_COALESCED_ALLOCNO (a))
    {
      ALLOCNO_HARD_REGNO (a) = best_hard_regno;
      ALLOCNO_ASSIGNED_P (a) = true;
      if (best_hard_regno >= 0)
	update_copy_costs (a, true);
      ira_assert (ALLOCNO_COVER_CLASS (a) == cover_class);
      /* We don't need updated costs anymore: */
      ira_free_allocno_updated_costs (a);
      if (a == allocno)
	break;
    }
  return best_hard_regno >= 0;
}



/* This page contains the allocator based on the Chaitin-Briggs algorithm.  */

/* Bucket of allocnos that can colored currently without spilling.  */
static ira_allocno_t colorable_allocno_bucket;

/* Bucket of allocnos that might be not colored currently without
   spilling.  */
static ira_allocno_t uncolorable_allocno_bucket;

/* Each element of the array contains the current number of allocnos
   of given *cover* class in the uncolorable_bucket.  */
static int uncolorable_allocnos_num[N_REG_CLASSES];

/* Return the current spill priority of allocno A.  The less the
   number, the more preferable the allocno for spilling.  */
static int
allocno_spill_priority (ira_allocno_t a)
{
  return (ALLOCNO_TEMP (a)
	  / (ALLOCNO_LEFT_CONFLICTS_SIZE (a)
	     * ira_reg_class_nregs[ALLOCNO_COVER_CLASS (a)][ALLOCNO_MODE (a)]
	     + 1));
}

/* Add ALLOCNO to bucket *BUCKET_PTR.  ALLOCNO should be not in a bucket
   before the call.  */
static void
add_allocno_to_bucket (ira_allocno_t allocno, ira_allocno_t *bucket_ptr)
{
  ira_allocno_t first_allocno;
  enum reg_class cover_class;

  if (bucket_ptr == &uncolorable_allocno_bucket
      && (cover_class = ALLOCNO_COVER_CLASS (allocno)) != NO_REGS)
    {
      uncolorable_allocnos_num[cover_class]++;
      ira_assert (uncolorable_allocnos_num[cover_class] > 0);
    }
  first_allocno = *bucket_ptr;
  ALLOCNO_NEXT_BUCKET_ALLOCNO (allocno) = first_allocno;
  ALLOCNO_PREV_BUCKET_ALLOCNO (allocno) = NULL;
  if (first_allocno != NULL)
    ALLOCNO_PREV_BUCKET_ALLOCNO (first_allocno) = allocno;
  *bucket_ptr = allocno;
}

/* The function returns frequency and number of available hard
   registers for allocnos coalesced with ALLOCNO.  */
static void
get_coalesced_allocnos_attributes (ira_allocno_t allocno, int *freq, int *num)
{
  ira_allocno_t a;

  *freq = 0;
  *num = 0;
  for (a = ALLOCNO_NEXT_COALESCED_ALLOCNO (allocno);;
       a = ALLOCNO_NEXT_COALESCED_ALLOCNO (a))
    {
      *freq += ALLOCNO_FREQ (a);
      *num += ALLOCNO_AVAILABLE_REGS_NUM (a);
      if (a == allocno)
	break;
    }
}

/* Compare two allocnos to define which allocno should be pushed first
   into the coloring stack.  If the return is a negative number, the
   allocno given by the first parameter will be pushed first.  In this
   case such allocno has less priority than the second one and the
   hard register will be assigned to it after assignment to the second
   one.  As the result of such assignment order, the second allocno
   has a better chance to get the best hard register.  */
static int
bucket_allocno_compare_func (const void *v1p, const void *v2p)
{
  ira_allocno_t a1 = *(const ira_allocno_t *) v1p;
  ira_allocno_t a2 = *(const ira_allocno_t *) v2p;
  int diff, a1_freq, a2_freq, a1_num, a2_num;

  if ((diff = (int) ALLOCNO_COVER_CLASS (a2) - ALLOCNO_COVER_CLASS (a1)) != 0)
    return diff;
  get_coalesced_allocnos_attributes (a1, &a1_freq, &a1_num);
  get_coalesced_allocnos_attributes (a2, &a2_freq, &a2_num);
  if ((diff = a2_num - a1_num) != 0)
    return diff;
  else if ((diff = a1_freq - a2_freq) != 0)
    return diff;
  return ALLOCNO_NUM (a2) - ALLOCNO_NUM (a1);
}

/* Sort bucket *BUCKET_PTR and return the result through
   BUCKET_PTR.  */
static void
sort_bucket (ira_allocno_t *bucket_ptr)
{
  ira_allocno_t a, head;
  int n;

  for (n = 0, a = *bucket_ptr; a != NULL; a = ALLOCNO_NEXT_BUCKET_ALLOCNO (a))
    sorted_allocnos[n++] = a;
  if (n <= 1)
    return;
  qsort (sorted_allocnos, n, sizeof (ira_allocno_t),
	 bucket_allocno_compare_func);
  head = NULL;
  for (n--; n >= 0; n--)
    {
      a = sorted_allocnos[n];
      ALLOCNO_NEXT_BUCKET_ALLOCNO (a) = head;
      ALLOCNO_PREV_BUCKET_ALLOCNO (a) = NULL;
      if (head != NULL)
	ALLOCNO_PREV_BUCKET_ALLOCNO (head) = a;
      head = a;
    }
  *bucket_ptr = head;
}

/* Add ALLOCNO to bucket *BUCKET_PTR maintaining the order according
   their priority.  ALLOCNO should be not in a bucket before the
   call.  */
static void
add_allocno_to_ordered_bucket (ira_allocno_t allocno,
			       ira_allocno_t *bucket_ptr)
{
  ira_allocno_t before, after;
  enum reg_class cover_class;

  if (bucket_ptr == &uncolorable_allocno_bucket
      && (cover_class = ALLOCNO_COVER_CLASS (allocno)) != NO_REGS)
    {
      uncolorable_allocnos_num[cover_class]++;
      ira_assert (uncolorable_allocnos_num[cover_class] > 0);
    }
  for (before = *bucket_ptr, after = NULL;
       before != NULL;
       after = before, before = ALLOCNO_NEXT_BUCKET_ALLOCNO (before))
    if (bucket_allocno_compare_func (&allocno, &before) < 0)
      break;
  ALLOCNO_NEXT_BUCKET_ALLOCNO (allocno) = before;
  ALLOCNO_PREV_BUCKET_ALLOCNO (allocno) = after;
  if (after == NULL)
    *bucket_ptr = allocno;
  else
    ALLOCNO_NEXT_BUCKET_ALLOCNO (after) = allocno;
  if (before != NULL)
    ALLOCNO_PREV_BUCKET_ALLOCNO (before) = allocno;
}

/* Delete ALLOCNO from bucket *BUCKET_PTR.  It should be there before
   the call.  */
static void
delete_allocno_from_bucket (ira_allocno_t allocno, ira_allocno_t *bucket_ptr)
{
  ira_allocno_t prev_allocno, next_allocno;
  enum reg_class cover_class;

  if (bucket_ptr == &uncolorable_allocno_bucket
      && (cover_class = ALLOCNO_COVER_CLASS (allocno)) != NO_REGS)
    {
      uncolorable_allocnos_num[cover_class]--;
      ira_assert (uncolorable_allocnos_num[cover_class] >= 0);
    }
  prev_allocno = ALLOCNO_PREV_BUCKET_ALLOCNO (allocno);
  next_allocno = ALLOCNO_NEXT_BUCKET_ALLOCNO (allocno);
  if (prev_allocno != NULL)
    ALLOCNO_NEXT_BUCKET_ALLOCNO (prev_allocno) = next_allocno;
  else
    {
      ira_assert (*bucket_ptr == allocno);
      *bucket_ptr = next_allocno;
    }
  if (next_allocno != NULL)
    ALLOCNO_PREV_BUCKET_ALLOCNO (next_allocno) = prev_allocno;
}

/* Splay tree for each cover class.  The trees are indexed by the
   corresponding cover classes.  Splay trees contain uncolorable
   allocnos.  */
static splay_tree uncolorable_allocnos_splay_tree[N_REG_CLASSES];

/* If the following macro is TRUE, splay tree is used to choose an
   allocno of the corresponding cover class for spilling.  When the
   number uncolorable allocnos of given cover class decreases to some
   threshold, linear array search is used to find the best allocno for
   spilling.  This threshold is actually pretty big because, although
   splay trees asymptotically is much faster, each splay tree
   operation is sufficiently costly especially taking cache locality
   into account.  */
#define USE_SPLAY_P(CLASS) (uncolorable_allocnos_num[CLASS] > 4000)

/* Put ALLOCNO onto the coloring stack without removing it from its
   bucket.  Pushing allocno to the coloring stack can result in moving
   conflicting allocnos from the uncolorable bucket to the colorable
   one.  */
static void
push_allocno_to_stack (ira_allocno_t allocno)
{
  int left_conflicts_size, conflict_size, size;
  ira_allocno_t a, conflict_allocno;
  enum reg_class cover_class;
  ira_allocno_conflict_iterator aci;

  ALLOCNO_IN_GRAPH_P (allocno) = false;
  VEC_safe_push (ira_allocno_t, heap, allocno_stack_vec, allocno);
  cover_class = ALLOCNO_COVER_CLASS (allocno);
  if (cover_class == NO_REGS)
    return;
  size = ira_reg_class_nregs[cover_class][ALLOCNO_MODE (allocno)];
  if (allocno_coalesced_p)
    bitmap_clear (processed_coalesced_allocno_bitmap);
  for (a = ALLOCNO_NEXT_COALESCED_ALLOCNO (allocno);;
       a = ALLOCNO_NEXT_COALESCED_ALLOCNO (a))
    {
      FOR_EACH_ALLOCNO_CONFLICT (a, conflict_allocno, aci)
	{
	  conflict_allocno = ALLOCNO_FIRST_COALESCED_ALLOCNO (conflict_allocno);
	  if (bitmap_bit_p (coloring_allocno_bitmap,
			    ALLOCNO_NUM (conflict_allocno)))
	    {
	      ira_assert (cover_class
			  == ALLOCNO_COVER_CLASS (conflict_allocno));
	      if (allocno_coalesced_p)
		{
		  if (bitmap_bit_p (processed_coalesced_allocno_bitmap,
				    ALLOCNO_NUM (conflict_allocno)))
		    continue;
		  bitmap_set_bit (processed_coalesced_allocno_bitmap,
				  ALLOCNO_NUM (conflict_allocno));
		}
	      if (ALLOCNO_IN_GRAPH_P (conflict_allocno)
		  && ! ALLOCNO_ASSIGNED_P (conflict_allocno))
		{
		  left_conflicts_size
		    = ALLOCNO_LEFT_CONFLICTS_SIZE (conflict_allocno);
		  conflict_size
		    = (ira_reg_class_nregs
		       [cover_class][ALLOCNO_MODE (conflict_allocno)]);
		  ira_assert
		    (ALLOCNO_LEFT_CONFLICTS_SIZE (conflict_allocno) >= size);
		  if (left_conflicts_size + conflict_size
		      <= ALLOCNO_AVAILABLE_REGS_NUM (conflict_allocno))
		    {
		      ALLOCNO_LEFT_CONFLICTS_SIZE (conflict_allocno) -= size;
		      continue;
		    }
		  left_conflicts_size
		    = ALLOCNO_LEFT_CONFLICTS_SIZE (conflict_allocno) - size;
		  if (uncolorable_allocnos_splay_tree[cover_class] != NULL
		      && !ALLOCNO_SPLAY_REMOVED_P (conflict_allocno)
		      && USE_SPLAY_P (cover_class))
		    {
		      ira_assert
		      (splay_tree_lookup
		       (uncolorable_allocnos_splay_tree[cover_class],
			(splay_tree_key) conflict_allocno) != NULL);
		      splay_tree_remove
			(uncolorable_allocnos_splay_tree[cover_class],
			 (splay_tree_key) conflict_allocno);
		      ALLOCNO_SPLAY_REMOVED_P (conflict_allocno) = true;
		      VEC_safe_push (ira_allocno_t, heap,
				     removed_splay_allocno_vec,
				     conflict_allocno);
		    }
		  ALLOCNO_LEFT_CONFLICTS_SIZE (conflict_allocno)
		    = left_conflicts_size;
		  if (left_conflicts_size + conflict_size
		      <= ALLOCNO_AVAILABLE_REGS_NUM (conflict_allocno))
		    {
		      delete_allocno_from_bucket
			(conflict_allocno, &uncolorable_allocno_bucket);
		      add_allocno_to_ordered_bucket
			(conflict_allocno, &colorable_allocno_bucket);
		    }
		}
	    }
	}
      if (a == allocno)
	break;
    }
}

/* Put ALLOCNO onto the coloring stack and remove it from its bucket.
   The allocno is in the colorable bucket if COLORABLE_P is TRUE.  */
static void
remove_allocno_from_bucket_and_push (ira_allocno_t allocno, bool colorable_p)
{
  enum reg_class cover_class;

  if (colorable_p)
    delete_allocno_from_bucket (allocno, &colorable_allocno_bucket);
  else
    delete_allocno_from_bucket (allocno, &uncolorable_allocno_bucket);
  if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
    {
      fprintf (ira_dump_file, "      Pushing");
      print_coalesced_allocno (allocno);
      if (colorable_p)
	fprintf (ira_dump_file, "\n");
      else
	fprintf (ira_dump_file, "(potential spill: %spri=%d, cost=%d)\n",
		 ALLOCNO_BAD_SPILL_P (allocno) ? "bad spill, " : "",
		 allocno_spill_priority (allocno), ALLOCNO_TEMP (allocno));
    }
  cover_class = ALLOCNO_COVER_CLASS (allocno);
  ira_assert ((colorable_p
	       && (ALLOCNO_LEFT_CONFLICTS_SIZE (allocno)
		   + ira_reg_class_nregs[cover_class][ALLOCNO_MODE (allocno)]
		   <= ALLOCNO_AVAILABLE_REGS_NUM (allocno)))
	      || (! colorable_p
		  && (ALLOCNO_LEFT_CONFLICTS_SIZE (allocno)
		      + ira_reg_class_nregs[cover_class][ALLOCNO_MODE
							 (allocno)]
		      > ALLOCNO_AVAILABLE_REGS_NUM (allocno))));
  if (! colorable_p)
    ALLOCNO_MAY_BE_SPILLED_P (allocno) = true;
  push_allocno_to_stack (allocno);
}

/* Put all allocnos from colorable bucket onto the coloring stack.  */
static void
push_only_colorable (void)
{
  sort_bucket (&colorable_allocno_bucket);
  for (;colorable_allocno_bucket != NULL;)
    remove_allocno_from_bucket_and_push (colorable_allocno_bucket, true);
}

/* Puts ALLOCNO chosen for potential spilling onto the coloring
   stack.  */
static void
push_allocno_to_spill (ira_allocno_t allocno)
{
  delete_allocno_from_bucket (allocno, &uncolorable_allocno_bucket);
  ALLOCNO_MAY_BE_SPILLED_P (allocno) = true;
  if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
    fprintf (ira_dump_file, "      Pushing p%d(%d) (spill for NO_REGS)\n",
	     ALLOCNO_NUM (allocno), ALLOCNO_REGNO (allocno));
  push_allocno_to_stack (allocno);
}

/* Return the frequency of exit edges (if EXIT_P) or entry from/to the
   loop given by its LOOP_NODE.  */
int
ira_loop_edge_freq (ira_loop_tree_node_t loop_node, int regno, bool exit_p)
{
  int freq, i;
  edge_iterator ei;
  edge e;
  VEC (edge, heap) *edges;

  ira_assert (loop_node->loop != NULL
	      && (regno < 0 || regno >= FIRST_PSEUDO_REGISTER));
  freq = 0;
  if (! exit_p)
    {
      FOR_EACH_EDGE (e, ei, loop_node->loop->header->preds)
	if (e->src != loop_node->loop->latch
	    && (regno < 0
		|| (bitmap_bit_p (DF_LR_OUT (e->src), regno)
		    && bitmap_bit_p (DF_LR_IN (e->dest), regno))))
	  freq += EDGE_FREQUENCY (e);
    }
  else
    {
      edges = get_loop_exit_edges (loop_node->loop);
      for (i = 0; VEC_iterate (edge, edges, i, e); i++)
	if (regno < 0
	    || (bitmap_bit_p (DF_LR_OUT (e->src), regno)
		&& bitmap_bit_p (DF_LR_IN (e->dest), regno)))
	  freq += EDGE_FREQUENCY (e);
      VEC_free (edge, heap, edges);
    }

  return REG_FREQ_FROM_EDGE_FREQ (freq);
}

/* Calculate and return the cost of putting allocno A into memory.  */
static int
calculate_allocno_spill_cost (ira_allocno_t a)
{
  int regno, cost;
  enum machine_mode mode;
  enum reg_class rclass;
  ira_allocno_t parent_allocno;
  ira_loop_tree_node_t parent_node, loop_node;

  regno = ALLOCNO_REGNO (a);
  cost = ALLOCNO_UPDATED_MEMORY_COST (a) - ALLOCNO_UPDATED_COVER_CLASS_COST (a);
  if (ALLOCNO_CAP (a) != NULL)
    return cost;
  loop_node = ALLOCNO_LOOP_TREE_NODE (a);
  if ((parent_node = loop_node->parent) == NULL)
    return cost;
  if ((parent_allocno = parent_node->regno_allocno_map[regno]) == NULL)
    return cost;
  mode = ALLOCNO_MODE (a);
  rclass = ALLOCNO_COVER_CLASS (a);
  if (ALLOCNO_HARD_REGNO (parent_allocno) < 0)
    cost -= (ira_memory_move_cost[mode][rclass][0]
	     * ira_loop_edge_freq (loop_node, regno, true)
	     + ira_memory_move_cost[mode][rclass][1]
	     * ira_loop_edge_freq (loop_node, regno, false));
  else
    cost += ((ira_memory_move_cost[mode][rclass][1]
	      * ira_loop_edge_freq (loop_node, regno, true)
	      + ira_memory_move_cost[mode][rclass][0]
	      * ira_loop_edge_freq (loop_node, regno, false))
	     - (ira_get_register_move_cost (mode, rclass, rclass)
		* (ira_loop_edge_freq (loop_node, regno, false)
		   + ira_loop_edge_freq (loop_node, regno, true))));
  return cost;
}

/* Compare keys in the splay tree used to choose best allocno for
   spilling.  The best allocno has the minimal key.  */
static int
allocno_spill_priority_compare (splay_tree_key k1, splay_tree_key k2)
{
  int pri1, pri2, diff;
  ira_allocno_t a1 = (ira_allocno_t) k1, a2 = (ira_allocno_t) k2;

  pri1 = (ALLOCNO_TEMP (a1)
	  / (ALLOCNO_LEFT_CONFLICTS_SIZE (a1)
	     * ira_reg_class_nregs[ALLOCNO_COVER_CLASS (a1)][ALLOCNO_MODE (a1)]
	     + 1));
  pri2 = (ALLOCNO_TEMP (a2)
	  / (ALLOCNO_LEFT_CONFLICTS_SIZE (a2)
	     * ira_reg_class_nregs[ALLOCNO_COVER_CLASS (a2)][ALLOCNO_MODE (a2)]
	     + 1));
  if ((diff = pri1 - pri2) != 0)
    return diff;
  if ((diff = ALLOCNO_TEMP (a1) - ALLOCNO_TEMP (a2)) != 0)
    return diff;
  return ALLOCNO_NUM (a1) - ALLOCNO_NUM (a2);
}

/* Allocate data of SIZE for the splay trees.  We allocate only spay
   tree roots or splay tree nodes.  If you change this, please rewrite
   the function.  */
static void *
splay_tree_allocate (int size, void *data ATTRIBUTE_UNUSED)
{
  if (size != sizeof (struct splay_tree_node_s))
    return ira_allocate (size);
  return pool_alloc (splay_tree_node_pool);
}

/* Free data NODE for the splay trees.  We allocate and free only spay
   tree roots or splay tree nodes.  If you change this, please rewrite
   the function.  */
static void
splay_tree_free (void *node, void *data ATTRIBUTE_UNUSED)
{
  int i;
  enum reg_class cover_class;

  for (i = 0; i < ira_reg_class_cover_size; i++)
    {
      cover_class = ira_reg_class_cover[i];
      if (node == uncolorable_allocnos_splay_tree[cover_class])
	{
	  ira_free (node);
	  return;
	}
    }
  pool_free (splay_tree_node_pool, node);
}

/* Push allocnos to the coloring stack.  The order of allocnos in the
   stack defines the order for the subsequent coloring.  */
static void
push_allocnos_to_stack (void)
{
  ira_allocno_t allocno, a, i_allocno, *allocno_vec;
  enum reg_class cover_class, rclass;
  int allocno_pri, i_allocno_pri, allocno_cost, i_allocno_cost;
  int i, j, num, cover_class_allocnos_num[N_REG_CLASSES];
  ira_allocno_t *cover_class_allocnos[N_REG_CLASSES];
  int cost;

  /* Initialize.  */
  VEC_truncate(ira_allocno_t, removed_splay_allocno_vec, 0);
  for (i = 0; i < ira_reg_class_cover_size; i++)
    {
      cover_class = ira_reg_class_cover[i];
      cover_class_allocnos_num[cover_class] = 0;
      cover_class_allocnos[cover_class] = NULL;
      uncolorable_allocnos_splay_tree[cover_class] = NULL;
    }
  /* Calculate uncolorable allocno spill costs.  */
  for (allocno = uncolorable_allocno_bucket;
       allocno != NULL;
       allocno = ALLOCNO_NEXT_BUCKET_ALLOCNO (allocno))
    if ((cover_class = ALLOCNO_COVER_CLASS (allocno)) != NO_REGS)
      {
	cover_class_allocnos_num[cover_class]++;
	cost = 0;
	for (a = ALLOCNO_NEXT_COALESCED_ALLOCNO (allocno);;
	     a = ALLOCNO_NEXT_COALESCED_ALLOCNO (a))
	  {
	    cost += calculate_allocno_spill_cost (a);
	    if (a == allocno)
	      break;
	  }
	/* ??? Remove cost of copies between the coalesced
	   allocnos.  */
	ALLOCNO_TEMP (allocno) = cost;
      }
  /* Define place where to put uncolorable allocnos of the same cover
     class.  */
  for (num = i = 0; i < ira_reg_class_cover_size; i++)
    {
      cover_class = ira_reg_class_cover[i];
      ira_assert (cover_class_allocnos_num[cover_class]
		  == uncolorable_allocnos_num[cover_class]);
      if (cover_class_allocnos_num[cover_class] != 0)
 	{
	  cover_class_allocnos[cover_class] = allocnos_for_spilling + num;
	  num += cover_class_allocnos_num[cover_class];
	  cover_class_allocnos_num[cover_class] = 0;
	}
      if (USE_SPLAY_P (cover_class))
	uncolorable_allocnos_splay_tree[cover_class]
	  = splay_tree_new_with_allocator (allocno_spill_priority_compare,
					   NULL, NULL, splay_tree_allocate,
					   splay_tree_free, NULL);
    }
  ira_assert (num <= ira_allocnos_num);
  /* Collect uncolorable allocnos of each cover class.  */
  for (allocno = uncolorable_allocno_bucket;
       allocno != NULL;
       allocno = ALLOCNO_NEXT_BUCKET_ALLOCNO (allocno))
    if ((cover_class = ALLOCNO_COVER_CLASS (allocno)) != NO_REGS)
      {
	cover_class_allocnos
	  [cover_class][cover_class_allocnos_num[cover_class]++] = allocno;
	if (uncolorable_allocnos_splay_tree[cover_class] != NULL)
	  splay_tree_insert (uncolorable_allocnos_splay_tree[cover_class],
			     (splay_tree_key) allocno,
			     (splay_tree_value) allocno);
      }
  for (;;)
    {
      push_only_colorable ();
      allocno = uncolorable_allocno_bucket;
      if (allocno == NULL)
	break;
      cover_class = ALLOCNO_COVER_CLASS (allocno);
      if (cover_class == NO_REGS)
	{
	  push_allocno_to_spill (allocno);
	  continue;
	}
      /* Potential spilling.  */
      ira_assert
	(ira_reg_class_nregs[cover_class][ALLOCNO_MODE (allocno)] > 0);
      if (USE_SPLAY_P (cover_class))
	{
	  for (;VEC_length (ira_allocno_t, removed_splay_allocno_vec) != 0;)
	    {
	      allocno = VEC_pop (ira_allocno_t, removed_splay_allocno_vec);
	      ALLOCNO_SPLAY_REMOVED_P (allocno) = false;
	      rclass = ALLOCNO_COVER_CLASS (allocno);
	      if (ALLOCNO_LEFT_CONFLICTS_SIZE (allocno)
		  + ira_reg_class_nregs [rclass][ALLOCNO_MODE (allocno)]
		  > ALLOCNO_AVAILABLE_REGS_NUM (allocno))
		splay_tree_insert
		  (uncolorable_allocnos_splay_tree[rclass],
		   (splay_tree_key) allocno, (splay_tree_value) allocno);
	    }
	  allocno = ((ira_allocno_t)
		     splay_tree_min
		     (uncolorable_allocnos_splay_tree[cover_class])->key);
	  splay_tree_remove (uncolorable_allocnos_splay_tree[cover_class],
			     (splay_tree_key) allocno);
	}
      else
	{
	  num = cover_class_allocnos_num[cover_class];
	  ira_assert (num > 0);
	  allocno_vec = cover_class_allocnos[cover_class];
	  allocno = NULL;
	  allocno_pri = allocno_cost = 0;
	  /* Sort uncolorable allocno to find the one with the lowest
	     spill cost.  */
	  for (i = 0, j = num - 1; i <= j;)
	    {
	      i_allocno = allocno_vec[i];
	      if (! ALLOCNO_IN_GRAPH_P (i_allocno)
		  && ALLOCNO_IN_GRAPH_P (allocno_vec[j]))
		{
		  i_allocno = allocno_vec[j];
		  allocno_vec[j] = allocno_vec[i];
		  allocno_vec[i] = i_allocno;
		}
	      if (ALLOCNO_IN_GRAPH_P (i_allocno))
		{
		  i++;
		  ira_assert (ALLOCNO_TEMP (i_allocno) != INT_MAX);
		  i_allocno_cost = ALLOCNO_TEMP (i_allocno);
		  i_allocno_pri = allocno_spill_priority (i_allocno);
		  if (allocno == NULL
		      || (! ALLOCNO_BAD_SPILL_P (i_allocno)
			  && ALLOCNO_BAD_SPILL_P (allocno))
		      || (! (ALLOCNO_BAD_SPILL_P (i_allocno)
			     && ! ALLOCNO_BAD_SPILL_P (allocno))
			  && (allocno_pri > i_allocno_pri
			      || (allocno_pri == i_allocno_pri
				  && (allocno_cost > i_allocno_cost
				      || (allocno_cost == i_allocno_cost
					  && (ALLOCNO_NUM (allocno)
					      > ALLOCNO_NUM (i_allocno))))))))
		    {
		      allocno = i_allocno;
		      allocno_cost = i_allocno_cost;
		      allocno_pri = i_allocno_pri;
		    }
		}
	      if (! ALLOCNO_IN_GRAPH_P (allocno_vec[j]))
		j--;
	    }
	  ira_assert (allocno != NULL && j >= 0);
	  cover_class_allocnos_num[cover_class] = j + 1;
	}
      ira_assert (ALLOCNO_IN_GRAPH_P (allocno)
		  && ALLOCNO_COVER_CLASS (allocno) == cover_class
		  && (ALLOCNO_LEFT_CONFLICTS_SIZE (allocno)
		      + ira_reg_class_nregs[cover_class][ALLOCNO_MODE
							 (allocno)]
		      > ALLOCNO_AVAILABLE_REGS_NUM (allocno)));
      remove_allocno_from_bucket_and_push (allocno, false);
    }
  ira_assert (colorable_allocno_bucket == NULL
	      && uncolorable_allocno_bucket == NULL);
  for (i = 0; i < ira_reg_class_cover_size; i++)
    {
      cover_class = ira_reg_class_cover[i];
      ira_assert (uncolorable_allocnos_num[cover_class] == 0);
      if (uncolorable_allocnos_splay_tree[cover_class] != NULL)
	splay_tree_delete (uncolorable_allocnos_splay_tree[cover_class]);
    }
}

/* Pop the coloring stack and assign hard registers to the popped
   allocnos.  */
static void
pop_allocnos_from_stack (void)
{
  ira_allocno_t allocno;
  enum reg_class cover_class;

  for (;VEC_length (ira_allocno_t, allocno_stack_vec) != 0;)
    {
      allocno = VEC_pop (ira_allocno_t, allocno_stack_vec);
      cover_class = ALLOCNO_COVER_CLASS (allocno);
      if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
	{
	  fprintf (ira_dump_file, "      Popping");
	  print_coalesced_allocno (allocno);
	  fprintf (ira_dump_file, "  -- ");
	}
      if (cover_class == NO_REGS)
	{
	  ALLOCNO_HARD_REGNO (allocno) = -1;
	  ALLOCNO_ASSIGNED_P (allocno) = true;
	  ira_assert (ALLOCNO_UPDATED_HARD_REG_COSTS (allocno) == NULL);
	  ira_assert
	    (ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS (allocno) == NULL);
	  if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
	    fprintf (ira_dump_file, "assign memory\n");
	}
      else if (assign_hard_reg (allocno, false))
	{
	  if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
	    fprintf (ira_dump_file, "assign reg %d\n",
		     ALLOCNO_HARD_REGNO (allocno));
	}
      else if (ALLOCNO_ASSIGNED_P (allocno))
	{
	  if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
	    fprintf (ira_dump_file, "spill\n");
	}
      ALLOCNO_IN_GRAPH_P (allocno) = true;
    }
}

/* Set up number of available hard registers for ALLOCNO.  */
static void
setup_allocno_available_regs_num (ira_allocno_t allocno)
{
  int i, n, hard_regs_num, hard_regno;
  enum machine_mode mode;
  enum reg_class cover_class;
  ira_allocno_t a;
  HARD_REG_SET temp_set;

  cover_class = ALLOCNO_COVER_CLASS (allocno);
  ALLOCNO_AVAILABLE_REGS_NUM (allocno) = ira_available_class_regs[cover_class];
  if (cover_class == NO_REGS)
    return;
  CLEAR_HARD_REG_SET (temp_set);
  ira_assert (ALLOCNO_FIRST_COALESCED_ALLOCNO (allocno) == allocno);
  hard_regs_num = ira_class_hard_regs_num[cover_class];
  for (a = ALLOCNO_NEXT_COALESCED_ALLOCNO (allocno);;
       a = ALLOCNO_NEXT_COALESCED_ALLOCNO (a))
    {
      IOR_HARD_REG_SET (temp_set, ALLOCNO_TOTAL_CONFLICT_HARD_REGS (a));
      if (a == allocno)
	break;
    }
  mode = ALLOCNO_MODE (allocno);
  for (n = 0, i = hard_regs_num - 1; i >= 0; i--)
    {
      hard_regno = ira_class_hard_regs[cover_class][i];
      if (TEST_HARD_REG_BIT (temp_set, hard_regno)
	  || TEST_HARD_REG_BIT (prohibited_class_mode_regs[cover_class][mode],
				hard_regno))
	n++;
    }
  if (internal_flag_ira_verbose > 2 && n > 0 && ira_dump_file != NULL)
    fprintf (ira_dump_file, "    Reg %d of %s has %d regs less\n",
	     ALLOCNO_REGNO (allocno), reg_class_names[cover_class], n);
  ALLOCNO_AVAILABLE_REGS_NUM (allocno) -= n;
}

/* Set up ALLOCNO_LEFT_CONFLICTS_SIZE for ALLOCNO.  */
static void
setup_allocno_left_conflicts_size (ira_allocno_t allocno)
{
  int i, hard_regs_num, hard_regno, conflict_allocnos_size;
  ira_allocno_t a, conflict_allocno;
  enum reg_class cover_class;
  HARD_REG_SET temp_set;
  ira_allocno_conflict_iterator aci;

  cover_class = ALLOCNO_COVER_CLASS (allocno);
  hard_regs_num = ira_class_hard_regs_num[cover_class];
  CLEAR_HARD_REG_SET (temp_set);
  ira_assert (ALLOCNO_FIRST_COALESCED_ALLOCNO (allocno) == allocno);
  for (a = ALLOCNO_NEXT_COALESCED_ALLOCNO (allocno);;
       a = ALLOCNO_NEXT_COALESCED_ALLOCNO (a))
    {
      IOR_HARD_REG_SET (temp_set, ALLOCNO_TOTAL_CONFLICT_HARD_REGS (a));
      if (a == allocno)
	break;
    }
  AND_HARD_REG_SET (temp_set, reg_class_contents[cover_class]);
  AND_COMPL_HARD_REG_SET (temp_set, ira_no_alloc_regs);
  conflict_allocnos_size = 0;
  if (! hard_reg_set_empty_p (temp_set))
    for (i = 0; i < (int) hard_regs_num; i++)
      {
	hard_regno = ira_class_hard_regs[cover_class][i];
	if (TEST_HARD_REG_BIT (temp_set, hard_regno))
	  {
	    conflict_allocnos_size++;
	    CLEAR_HARD_REG_BIT (temp_set, hard_regno);
	    if (hard_reg_set_empty_p (temp_set))
	      break;
	  }
      }
  CLEAR_HARD_REG_SET (temp_set);
  if (allocno_coalesced_p)
    bitmap_clear (processed_coalesced_allocno_bitmap);
  if (cover_class != NO_REGS)
    for (a = ALLOCNO_NEXT_COALESCED_ALLOCNO (allocno);;
	 a = ALLOCNO_NEXT_COALESCED_ALLOCNO (a))
      {
	FOR_EACH_ALLOCNO_CONFLICT (a, conflict_allocno, aci)
	  {
	    conflict_allocno
	      = ALLOCNO_FIRST_COALESCED_ALLOCNO (conflict_allocno);
	    if (bitmap_bit_p (consideration_allocno_bitmap,
			      ALLOCNO_NUM (conflict_allocno)))
	      {
		ira_assert (cover_class
			    == ALLOCNO_COVER_CLASS (conflict_allocno));
		if (allocno_coalesced_p)
		  {
		    if (bitmap_bit_p (processed_coalesced_allocno_bitmap,
				      ALLOCNO_NUM (conflict_allocno)))
		      continue;
		    bitmap_set_bit (processed_coalesced_allocno_bitmap,
				    ALLOCNO_NUM (conflict_allocno));
		  }
		if (! ALLOCNO_ASSIGNED_P (conflict_allocno))
		  conflict_allocnos_size
		    += (ira_reg_class_nregs
			[cover_class][ALLOCNO_MODE (conflict_allocno)]);
		else if ((hard_regno = ALLOCNO_HARD_REGNO (conflict_allocno))
			 >= 0)
		  {
		    int last = (hard_regno
				+ hard_regno_nregs
			        [hard_regno][ALLOCNO_MODE (conflict_allocno)]);

		    while (hard_regno < last)
		      {
			if (! TEST_HARD_REG_BIT (temp_set, hard_regno))
			  {
			    conflict_allocnos_size++;
			    SET_HARD_REG_BIT (temp_set, hard_regno);
			  }
			hard_regno++;
		      }
		  }
	      }
	  }
        if (a == allocno)
	  break;
      }
  ALLOCNO_LEFT_CONFLICTS_SIZE (allocno) = conflict_allocnos_size;
}

/* Put ALLOCNO in a bucket corresponding to its number and size of its
   conflicting allocnos and hard registers.  */
static void
put_allocno_into_bucket (ira_allocno_t allocno)
{
  enum reg_class cover_class;

  cover_class = ALLOCNO_COVER_CLASS (allocno);
  if (ALLOCNO_FIRST_COALESCED_ALLOCNO (allocno) != allocno)
    return;
  ALLOCNO_IN_GRAPH_P (allocno) = true;
  setup_allocno_left_conflicts_size (allocno);
  setup_allocno_available_regs_num (allocno);
  if (ALLOCNO_LEFT_CONFLICTS_SIZE (allocno)
      + ira_reg_class_nregs[cover_class][ALLOCNO_MODE (allocno)]
      <= ALLOCNO_AVAILABLE_REGS_NUM (allocno))
    add_allocno_to_bucket (allocno, &colorable_allocno_bucket);
  else
    add_allocno_to_bucket (allocno, &uncolorable_allocno_bucket);
}

/* The function is used to sort allocnos according to their execution
   frequencies.  */
static int
copy_freq_compare_func (const void *v1p, const void *v2p)
{
  ira_copy_t cp1 = *(const ira_copy_t *) v1p, cp2 = *(const ira_copy_t *) v2p;
  int pri1, pri2;

  pri1 = cp1->freq;
  pri2 = cp2->freq;
  if (pri2 - pri1)
    return pri2 - pri1;

  /* If freqencies are equal, sort by copies, so that the results of
     qsort leave nothing to chance.  */
  return cp1->num - cp2->num;
}

/* Merge two sets of coalesced allocnos given correspondingly by
   allocnos A1 and A2 (more accurately merging A2 set into A1
   set).  */
static void
merge_allocnos (ira_allocno_t a1, ira_allocno_t a2)
{
  ira_allocno_t a, first, last, next;

  first = ALLOCNO_FIRST_COALESCED_ALLOCNO (a1);
  if (first == ALLOCNO_FIRST_COALESCED_ALLOCNO (a2))
    return;
  for (last = a2, a = ALLOCNO_NEXT_COALESCED_ALLOCNO (a2);;
       a = ALLOCNO_NEXT_COALESCED_ALLOCNO (a))
    {
      ALLOCNO_FIRST_COALESCED_ALLOCNO (a) = first;
      if (a == a2)
	break;
      last = a;
    }
  next = ALLOCNO_NEXT_COALESCED_ALLOCNO (first);
  ALLOCNO_NEXT_COALESCED_ALLOCNO (first) = a2;
  ALLOCNO_NEXT_COALESCED_ALLOCNO (last) = next;
}

/* Return TRUE if there are conflicting allocnos from two sets of
   coalesced allocnos given correspondingly by allocnos A1 and A2.  If
   RELOAD_P is TRUE, we use live ranges to find conflicts because
   conflicts are represented only for allocnos of the same cover class
   and during the reload pass we coalesce allocnos for sharing stack
   memory slots.  */
static bool
coalesced_allocno_conflict_p (ira_allocno_t a1, ira_allocno_t a2,
			      bool reload_p)
{
  ira_allocno_t a, conflict_allocno;
  ira_allocno_conflict_iterator aci;

  if (allocno_coalesced_p)
    {
      bitmap_clear (processed_coalesced_allocno_bitmap);
      for (a = ALLOCNO_NEXT_COALESCED_ALLOCNO (a1);;
	   a = ALLOCNO_NEXT_COALESCED_ALLOCNO (a))
	{
	  bitmap_set_bit (processed_coalesced_allocno_bitmap, ALLOCNO_NUM (a));
	  if (a == a1)
	    break;
	}
    }
  for (a = ALLOCNO_NEXT_COALESCED_ALLOCNO (a2);;
       a = ALLOCNO_NEXT_COALESCED_ALLOCNO (a))
    {
      if (reload_p)
	{
	  for (conflict_allocno = ALLOCNO_NEXT_COALESCED_ALLOCNO (a1);;
	       conflict_allocno
		 = ALLOCNO_NEXT_COALESCED_ALLOCNO (conflict_allocno))
	    {
	      if (allocnos_have_intersected_live_ranges_p (a,
							   conflict_allocno))
		return true;
	      if (conflict_allocno == a1)
		break;
	    }
	}
      else
	{
	  FOR_EACH_ALLOCNO_CONFLICT (a, conflict_allocno, aci)
	    if (conflict_allocno == a1
		|| (allocno_coalesced_p
		    && bitmap_bit_p (processed_coalesced_allocno_bitmap,
				     ALLOCNO_NUM (conflict_allocno))))
	      return true;
	}
      if (a == a2)
	break;
    }
  return false;
}

/* The major function for aggressive allocno coalescing.  For the
   reload pass (RELOAD_P) we coalesce only spilled allocnos.  If some
   allocnos have been coalesced, we set up flag
   allocno_coalesced_p.  */
static void
coalesce_allocnos (bool reload_p)
{
  ira_allocno_t a;
  ira_copy_t cp, next_cp, *sorted_copies;
  enum reg_class cover_class;
  enum machine_mode mode;
  unsigned int j;
  int i, n, cp_num, regno;
  bitmap_iterator bi;

  sorted_copies = (ira_copy_t *) ira_allocate (ira_copies_num
					       * sizeof (ira_copy_t));
  cp_num = 0;
  /* Collect copies.  */
  EXECUTE_IF_SET_IN_BITMAP (coloring_allocno_bitmap, 0, j, bi)
    {
      a = ira_allocnos[j];
      regno = ALLOCNO_REGNO (a);
      if ((! reload_p && ALLOCNO_ASSIGNED_P (a))
	  || (reload_p
	      && (! ALLOCNO_ASSIGNED_P (a) || ALLOCNO_HARD_REGNO (a) >= 0
		  || (regno < ira_reg_equiv_len
		      && (ira_reg_equiv_const[regno] != NULL_RTX
			  || ira_reg_equiv_invariant_p[regno])))))
	continue;
      cover_class = ALLOCNO_COVER_CLASS (a);
      mode = ALLOCNO_MODE (a);
      for (cp = ALLOCNO_COPIES (a); cp != NULL; cp = next_cp)
	{
	  if (cp->first == a)
	    {
	      next_cp = cp->next_first_allocno_copy;
	      regno = ALLOCNO_REGNO (cp->second);
	      /* For priority coloring we coalesce allocnos only with
		 the same cover class not with intersected cover
		 classes as it were possible.  It is done for
		 simplicity.  */
	      if ((reload_p
		   || (ALLOCNO_COVER_CLASS (cp->second) == cover_class
		       && ALLOCNO_MODE (cp->second) == mode))
		  && (cp->insn != NULL || cp->constraint_p)
		  && ((! reload_p && ! ALLOCNO_ASSIGNED_P (cp->second))
		      || (reload_p
			  && ALLOCNO_ASSIGNED_P (cp->second)
			  && ALLOCNO_HARD_REGNO (cp->second) < 0
			  && (regno >= ira_reg_equiv_len
			      || (! ira_reg_equiv_invariant_p[regno]
				  && ira_reg_equiv_const[regno] == NULL_RTX)))))
		sorted_copies[cp_num++] = cp;
	    }
	  else if (cp->second == a)
	    next_cp = cp->next_second_allocno_copy;
	  else
	    gcc_unreachable ();
	}
    }
  qsort (sorted_copies, cp_num, sizeof (ira_copy_t), copy_freq_compare_func);
  /* Coalesced copies, most frequently executed first.  */
  for (; cp_num != 0;)
    {
      for (i = 0; i < cp_num; i++)
	{
	  cp = sorted_copies[i];
	  if (! coalesced_allocno_conflict_p (cp->first, cp->second, reload_p))
	    {
	      allocno_coalesced_p = true;
	      if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
		fprintf
		  (ira_dump_file,
		   "      Coalescing copy %d:a%dr%d-a%dr%d (freq=%d)\n",
		   cp->num, ALLOCNO_NUM (cp->first), ALLOCNO_REGNO (cp->first),
		   ALLOCNO_NUM (cp->second), ALLOCNO_REGNO (cp->second),
		   cp->freq);
	      merge_allocnos (cp->first, cp->second);
	      i++;
	      break;
	    }
	}
      /* Collect the rest of copies.  */
      for (n = 0; i < cp_num; i++)
	{
	  cp = sorted_copies[i];
	  if (ALLOCNO_FIRST_COALESCED_ALLOCNO (cp->first)
	      != ALLOCNO_FIRST_COALESCED_ALLOCNO (cp->second))
	    sorted_copies[n++] = cp;
	}
      cp_num = n;
    }
  ira_free (sorted_copies);
}

/* Map: allocno number -> allocno priority.  */
static int *allocno_priorities;

/* Set up priorities for N allocnos in array
   CONSIDERATION_ALLOCNOS.  */
static void
setup_allocno_priorities (ira_allocno_t *consideration_allocnos, int n)
{
  int i, length, nrefs, priority, max_priority, mult;
  ira_allocno_t a;

  max_priority = 0;
  for (i = 0; i < n; i++)
    {
      a = consideration_allocnos[i];
      nrefs = ALLOCNO_NREFS (a);
      ira_assert (nrefs >= 0);
      mult = floor_log2 (ALLOCNO_NREFS (a)) + 1;
      ira_assert (mult >= 0);
      allocno_priorities[ALLOCNO_NUM (a)]
	= priority
	= (mult
	   * (ALLOCNO_MEMORY_COST (a) - ALLOCNO_COVER_CLASS_COST (a))
	   * ira_reg_class_nregs[ALLOCNO_COVER_CLASS (a)][ALLOCNO_MODE (a)]);
      if (priority < 0)
	priority = -priority;
      if (max_priority < priority)
	max_priority = priority;
    }
  mult = max_priority == 0 ? 1 : INT_MAX / max_priority;
  for (i = 0; i < n; i++)
    {
      a = consideration_allocnos[i];
      length = ALLOCNO_EXCESS_PRESSURE_POINTS_NUM (a);
      if (length <= 0)
	length = 1;
      allocno_priorities[ALLOCNO_NUM (a)]
	= allocno_priorities[ALLOCNO_NUM (a)] * mult / length;
    }
}

/* Sort allocnos according to their priorities which are calculated
   analogous to ones in file `global.c'.  */
static int
allocno_priority_compare_func (const void *v1p, const void *v2p)
{
  ira_allocno_t a1 = *(const ira_allocno_t *) v1p;
  ira_allocno_t a2 = *(const ira_allocno_t *) v2p;
  int pri1, pri2;

  pri1 = allocno_priorities[ALLOCNO_NUM (a1)];
  pri2 = allocno_priorities[ALLOCNO_NUM (a2)];
  if (pri2 != pri1)
    return SORTGT (pri2, pri1);

  /* If regs are equally good, sort by allocnos, so that the results of
     qsort leave nothing to chance.  */
  return ALLOCNO_NUM (a1) - ALLOCNO_NUM (a2);
}

/* Chaitin-Briggs coloring for allocnos in COLORING_ALLOCNO_BITMAP
   taking into account allocnos in CONSIDERATION_ALLOCNO_BITMAP.  */
static void
color_allocnos (void)
{
  unsigned int i, n;
  bitmap_iterator bi;
  ira_allocno_t a;

  allocno_coalesced_p = false;
  processed_coalesced_allocno_bitmap = ira_allocate_bitmap ();
  if (flag_ira_coalesce)
    coalesce_allocnos (false);
  if (flag_ira_algorithm == IRA_ALGORITHM_PRIORITY)
    {
      n = 0;
      EXECUTE_IF_SET_IN_BITMAP (coloring_allocno_bitmap, 0, i, bi)
	{
	  a = ira_allocnos[i];
	  if (ALLOCNO_COVER_CLASS (a) == NO_REGS)
	    {
	      ALLOCNO_HARD_REGNO (a) = -1;
	      ALLOCNO_ASSIGNED_P (a) = true;
	      ira_assert (ALLOCNO_UPDATED_HARD_REG_COSTS (a) == NULL);
	      ira_assert (ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS (a) == NULL);
	      if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
		{
		  fprintf (ira_dump_file, "      Spill");
		  print_coalesced_allocno (a);
		  fprintf (ira_dump_file, "\n");
		}
	      continue;
	    }
	  sorted_allocnos[n++] = a;
	}
      if (n != 0)
	{
	  setup_allocno_priorities (sorted_allocnos, n);
	  qsort (sorted_allocnos, n, sizeof (ira_allocno_t),
		 allocno_priority_compare_func);
	  for (i = 0; i < n; i++)
	    {
	      a = sorted_allocnos[i];
	      if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
		{
		  fprintf (ira_dump_file, "      ");
		  print_coalesced_allocno (a);
		  fprintf (ira_dump_file, "  -- ");
		}
	      if (assign_hard_reg (a, false))
		{
		  if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
		    fprintf (ira_dump_file, "assign hard reg %d\n",
			     ALLOCNO_HARD_REGNO (a));
		}
	      else
		{
		  if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
		    fprintf (ira_dump_file, "assign memory\n");
		}
	    }
	}
    }
  else
    {
      /* Put the allocnos into the corresponding buckets.  */
      colorable_allocno_bucket = NULL;
      uncolorable_allocno_bucket = NULL;
      EXECUTE_IF_SET_IN_BITMAP (coloring_allocno_bitmap, 0, i, bi)
	{
	  a = ira_allocnos[i];
	  if (ALLOCNO_COVER_CLASS (a) == NO_REGS)
	    {
	      ALLOCNO_HARD_REGNO (a) = -1;
	      ALLOCNO_ASSIGNED_P (a) = true;
	      ira_assert (ALLOCNO_UPDATED_HARD_REG_COSTS (a) == NULL);
	      ira_assert (ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS (a) == NULL);
	      if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
		{
		  fprintf (ira_dump_file, "      Spill");
		  print_coalesced_allocno (a);
		  fprintf (ira_dump_file, "\n");
		}
	      continue;
	    }
	  put_allocno_into_bucket (a);
	}
      push_allocnos_to_stack ();
      pop_allocnos_from_stack ();
    }
  if (flag_ira_coalesce)
    /* We don't need coalesced allocnos for ira_reassign_pseudos.  */
    EXECUTE_IF_SET_IN_BITMAP (coloring_allocno_bitmap, 0, i, bi)
      {
	a = ira_allocnos[i];
	ALLOCNO_FIRST_COALESCED_ALLOCNO (a) = a;
	ALLOCNO_NEXT_COALESCED_ALLOCNO (a) = a;
      }
  ira_free_bitmap (processed_coalesced_allocno_bitmap);
  allocno_coalesced_p = false;
}



/* Output information about the loop given by its LOOP_TREE_NODE. */
static void
print_loop_title (ira_loop_tree_node_t loop_tree_node)
{
  unsigned int j;
  bitmap_iterator bi;
  ira_loop_tree_node_t subloop_node, dest_loop_node;
  edge e;
  edge_iterator ei;

  ira_assert (loop_tree_node->loop != NULL);
  fprintf (ira_dump_file,
	   "\n  Loop %d (parent %d, header bb%d, depth %d)\n    bbs:",
	   loop_tree_node->loop->num,
	   (loop_tree_node->parent == NULL
	    ? -1 : loop_tree_node->parent->loop->num),
	   loop_tree_node->loop->header->index,
	   loop_depth (loop_tree_node->loop));
  for (subloop_node = loop_tree_node->children;
       subloop_node != NULL;
       subloop_node = subloop_node->next)
    if (subloop_node->bb != NULL)
      {
	fprintf (ira_dump_file, " %d", subloop_node->bb->index);
	FOR_EACH_EDGE (e, ei, subloop_node->bb->succs)
	  if (e->dest != EXIT_BLOCK_PTR
	      && ((dest_loop_node = IRA_BB_NODE (e->dest)->parent)
		  != loop_tree_node))
	    fprintf (ira_dump_file, "(->%d:l%d)",
		     e->dest->index, dest_loop_node->loop->num);
      }
  fprintf (ira_dump_file, "\n    all:");
  EXECUTE_IF_SET_IN_BITMAP (loop_tree_node->all_allocnos, 0, j, bi)
    fprintf (ira_dump_file, " %dr%d", j, ALLOCNO_REGNO (ira_allocnos[j]));
  fprintf (ira_dump_file, "\n    modified regnos:");
  EXECUTE_IF_SET_IN_BITMAP (loop_tree_node->modified_regnos, 0, j, bi)
    fprintf (ira_dump_file, " %d", j);
  fprintf (ira_dump_file, "\n    border:");
  EXECUTE_IF_SET_IN_BITMAP (loop_tree_node->border_allocnos, 0, j, bi)
    fprintf (ira_dump_file, " %dr%d", j, ALLOCNO_REGNO (ira_allocnos[j]));
  fprintf (ira_dump_file, "\n    Pressure:");
  for (j = 0; (int) j < ira_reg_class_cover_size; j++)
    {
      enum reg_class cover_class;

      cover_class = ira_reg_class_cover[j];
      if (loop_tree_node->reg_pressure[cover_class] == 0)
	continue;
      fprintf (ira_dump_file, " %s=%d", reg_class_names[cover_class],
	       loop_tree_node->reg_pressure[cover_class]);
    }
  fprintf (ira_dump_file, "\n");
}

/* Color the allocnos inside loop (in the extreme case it can be all
   of the function) given the corresponding LOOP_TREE_NODE.  The
   function is called for each loop during top-down traverse of the
   loop tree.  */
static void
color_pass (ira_loop_tree_node_t loop_tree_node)
{
  int regno, hard_regno, index = -1;
  int cost, exit_freq, enter_freq;
  unsigned int j;
  bitmap_iterator bi;
  enum machine_mode mode;
  enum reg_class rclass, cover_class;
  ira_allocno_t a, subloop_allocno;
  ira_loop_tree_node_t subloop_node;

  ira_assert (loop_tree_node->bb == NULL);
  if (internal_flag_ira_verbose > 1 && ira_dump_file != NULL)
    print_loop_title (loop_tree_node);

  bitmap_copy (coloring_allocno_bitmap, loop_tree_node->all_allocnos);
  bitmap_copy (consideration_allocno_bitmap, coloring_allocno_bitmap);
  EXECUTE_IF_SET_IN_BITMAP (consideration_allocno_bitmap, 0, j, bi)
    {
      a = ira_allocnos[j];
      if (! ALLOCNO_ASSIGNED_P (a))
	continue;
      bitmap_clear_bit (coloring_allocno_bitmap, ALLOCNO_NUM (a));
    }
  /* Color all mentioned allocnos including transparent ones.  */
  color_allocnos ();
  /* Process caps.  They are processed just once.  */
  if (flag_ira_region == IRA_REGION_MIXED
      || flag_ira_region == IRA_REGION_ALL)
    EXECUTE_IF_SET_IN_BITMAP (loop_tree_node->all_allocnos, 0, j, bi)
      {
	a = ira_allocnos[j];
	if (ALLOCNO_CAP_MEMBER (a) == NULL)
	  continue;
	/* Remove from processing in the next loop.  */
	bitmap_clear_bit (consideration_allocno_bitmap, j);
	rclass = ALLOCNO_COVER_CLASS (a);
	if (flag_ira_region == IRA_REGION_MIXED
	    && (loop_tree_node->reg_pressure[rclass]
		<= ira_available_class_regs[rclass]))
	  {
	    mode = ALLOCNO_MODE (a);
	    hard_regno = ALLOCNO_HARD_REGNO (a);
	    if (hard_regno >= 0)
	      {
		index = ira_class_hard_reg_index[rclass][hard_regno];
		ira_assert (index >= 0);
	      }
	    regno = ALLOCNO_REGNO (a);
	    subloop_allocno = ALLOCNO_CAP_MEMBER (a);
	    subloop_node = ALLOCNO_LOOP_TREE_NODE (subloop_allocno);
	    ira_assert (!ALLOCNO_ASSIGNED_P (subloop_allocno));
	    ALLOCNO_HARD_REGNO (subloop_allocno) = hard_regno;
	    ALLOCNO_ASSIGNED_P (subloop_allocno) = true;
	    if (hard_regno >= 0)
	      update_copy_costs (subloop_allocno, true);
	    /* We don't need updated costs anymore: */
	    ira_free_allocno_updated_costs (subloop_allocno);
	  }
      }
  /* Update costs of the corresponding allocnos (not caps) in the
     subloops.  */
  for (subloop_node = loop_tree_node->subloops;
       subloop_node != NULL;
       subloop_node = subloop_node->subloop_next)
    {
      ira_assert (subloop_node->bb == NULL);
      EXECUTE_IF_SET_IN_BITMAP (consideration_allocno_bitmap, 0, j, bi)
        {
	  a = ira_allocnos[j];
	  ira_assert (ALLOCNO_CAP_MEMBER (a) == NULL);
	  mode = ALLOCNO_MODE (a);
	  rclass = ALLOCNO_COVER_CLASS (a);
	  hard_regno = ALLOCNO_HARD_REGNO (a);
	  /* Use hard register class here.  ??? */
	  if (hard_regno >= 0)
	    {
	      index = ira_class_hard_reg_index[rclass][hard_regno];
	      ira_assert (index >= 0);
	    }
	  regno = ALLOCNO_REGNO (a);
	  /* ??? conflict costs */
	  subloop_allocno = subloop_node->regno_allocno_map[regno];
	  if (subloop_allocno == NULL
	      || ALLOCNO_CAP (subloop_allocno) != NULL)
	    continue;
	  ira_assert (ALLOCNO_COVER_CLASS (subloop_allocno) == rclass);
	  ira_assert (bitmap_bit_p (subloop_node->all_allocnos,
				    ALLOCNO_NUM (subloop_allocno)));
	  if ((flag_ira_region == IRA_REGION_MIXED)
	      && (loop_tree_node->reg_pressure[rclass]
		  <= ira_available_class_regs[rclass]))
	    {
	      if (! ALLOCNO_ASSIGNED_P (subloop_allocno))
		{
		  ALLOCNO_HARD_REGNO (subloop_allocno) = hard_regno;
		  ALLOCNO_ASSIGNED_P (subloop_allocno) = true;
		  if (hard_regno >= 0)
		    update_copy_costs (subloop_allocno, true);
		  /* We don't need updated costs anymore: */
		  ira_free_allocno_updated_costs (subloop_allocno);
		}
	      continue;
	    }
	  exit_freq = ira_loop_edge_freq (subloop_node, regno, true);
	  enter_freq = ira_loop_edge_freq (subloop_node, regno, false);
	  ira_assert (regno < ira_reg_equiv_len);
	  if (ira_reg_equiv_invariant_p[regno]
	      || ira_reg_equiv_const[regno] != NULL_RTX)
	    {
	      if (! ALLOCNO_ASSIGNED_P (subloop_allocno))
		{
		  ALLOCNO_HARD_REGNO (subloop_allocno) = hard_regno;
		  ALLOCNO_ASSIGNED_P (subloop_allocno) = true;
		  if (hard_regno >= 0)
		    update_copy_costs (subloop_allocno, true);
		  /* We don't need updated costs anymore: */
		  ira_free_allocno_updated_costs (subloop_allocno);
		}
	    }
	  else if (hard_regno < 0)
	    {
	      ALLOCNO_UPDATED_MEMORY_COST (subloop_allocno)
		-= ((ira_memory_move_cost[mode][rclass][1] * enter_freq)
		    + (ira_memory_move_cost[mode][rclass][0] * exit_freq));
	    }
	  else
	    {
	      cover_class = ALLOCNO_COVER_CLASS (subloop_allocno);
	      cost = (ira_get_register_move_cost (mode, rclass, rclass)
		      * (exit_freq + enter_freq));
	      ira_allocate_and_set_or_copy_costs
		(&ALLOCNO_UPDATED_HARD_REG_COSTS (subloop_allocno), cover_class,
		 ALLOCNO_UPDATED_COVER_CLASS_COST (subloop_allocno),
		 ALLOCNO_HARD_REG_COSTS (subloop_allocno));
	      ira_allocate_and_set_or_copy_costs
		(&ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS (subloop_allocno),
		 cover_class, 0,
		 ALLOCNO_CONFLICT_HARD_REG_COSTS (subloop_allocno));
	      ALLOCNO_UPDATED_HARD_REG_COSTS (subloop_allocno)[index] -= cost;
	      ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS (subloop_allocno)[index]
		-= cost;
	      if (ALLOCNO_UPDATED_COVER_CLASS_COST (subloop_allocno)
		  > ALLOCNO_UPDATED_HARD_REG_COSTS (subloop_allocno)[index])
		ALLOCNO_UPDATED_COVER_CLASS_COST (subloop_allocno)
		  = ALLOCNO_UPDATED_HARD_REG_COSTS (subloop_allocno)[index];
	      ALLOCNO_UPDATED_MEMORY_COST (subloop_allocno)
		+= (ira_memory_move_cost[mode][rclass][0] * enter_freq
		    + ira_memory_move_cost[mode][rclass][1] * exit_freq);
	    }
	}
    }
}

/* Initialize the common data for coloring and calls functions to do
   Chaitin-Briggs and regional coloring.  */
static void
do_coloring (void)
{
  coloring_allocno_bitmap = ira_allocate_bitmap ();
  allocnos_for_spilling
    = (ira_allocno_t *) ira_allocate (sizeof (ira_allocno_t)
				      * ira_allocnos_num);
  splay_tree_node_pool = create_alloc_pool ("splay tree nodes",
					    sizeof (struct splay_tree_node_s),
					    100);
  if (internal_flag_ira_verbose > 0 && ira_dump_file != NULL)
    fprintf (ira_dump_file, "\n**** Allocnos coloring:\n\n");

  ira_traverse_loop_tree (false, ira_loop_tree_root, color_pass, NULL);

  if (internal_flag_ira_verbose > 1 && ira_dump_file != NULL)
    ira_print_disposition (ira_dump_file);

  free_alloc_pool (splay_tree_node_pool);
  ira_free_bitmap (coloring_allocno_bitmap);
  ira_free (allocnos_for_spilling);
}



/* Move spill/restore code, which are to be generated in ira-emit.c,
   to less frequent points (if it is profitable) by reassigning some
   allocnos (in loop with subloops containing in another loop) to
   memory which results in longer live-range where the corresponding
   pseudo-registers will be in memory.  */
static void
move_spill_restore (void)
{
  int cost, regno, hard_regno, hard_regno2, index;
  bool changed_p;
  int enter_freq, exit_freq;
  enum machine_mode mode;
  enum reg_class rclass;
  ira_allocno_t a, parent_allocno, subloop_allocno;
  ira_loop_tree_node_t parent, loop_node, subloop_node;
  ira_allocno_iterator ai;

  for (;;)
    {
      changed_p = false;
      if (internal_flag_ira_verbose > 0 && ira_dump_file != NULL)
	fprintf (ira_dump_file, "New iteration of spill/restore move\n");
      FOR_EACH_ALLOCNO (a, ai)
	{
	  regno = ALLOCNO_REGNO (a);
	  loop_node = ALLOCNO_LOOP_TREE_NODE (a);
	  if (ALLOCNO_CAP_MEMBER (a) != NULL
	      || ALLOCNO_CAP (a) != NULL
	      || (hard_regno = ALLOCNO_HARD_REGNO (a)) < 0
	      || loop_node->children == NULL
	      /* don't do the optimization because it can create
		 copies and the reload pass can spill the allocno set
		 by copy although the allocno will not get memory
		 slot.  */
	      || ira_reg_equiv_invariant_p[regno]
	      || ira_reg_equiv_const[regno] != NULL_RTX
	      || !bitmap_bit_p (loop_node->border_allocnos, ALLOCNO_NUM (a)))
	    continue;
	  mode = ALLOCNO_MODE (a);
	  rclass = ALLOCNO_COVER_CLASS (a);
	  index = ira_class_hard_reg_index[rclass][hard_regno];
	  ira_assert (index >= 0);
	  cost = (ALLOCNO_MEMORY_COST (a)
		  - (ALLOCNO_HARD_REG_COSTS (a) == NULL
		     ? ALLOCNO_COVER_CLASS_COST (a)
		     : ALLOCNO_HARD_REG_COSTS (a)[index]));
	  for (subloop_node = loop_node->subloops;
	       subloop_node != NULL;
	       subloop_node = subloop_node->subloop_next)
	    {
	      ira_assert (subloop_node->bb == NULL);
	      subloop_allocno = subloop_node->regno_allocno_map[regno];
	      if (subloop_allocno == NULL)
		continue;
	      ira_assert (rclass == ALLOCNO_COVER_CLASS (subloop_allocno));
	      /* We have accumulated cost.  To get the real cost of
		 allocno usage in the loop we should subtract costs of
		 the subloop allocnos.  */
	      cost -= (ALLOCNO_MEMORY_COST (subloop_allocno)
		       - (ALLOCNO_HARD_REG_COSTS (subloop_allocno) == NULL
			  ? ALLOCNO_COVER_CLASS_COST (subloop_allocno)
			  : ALLOCNO_HARD_REG_COSTS (subloop_allocno)[index]));
	      exit_freq = ira_loop_edge_freq (subloop_node, regno, true);
	      enter_freq = ira_loop_edge_freq (subloop_node, regno, false);
	      if ((hard_regno2 = ALLOCNO_HARD_REGNO (subloop_allocno)) < 0)
		cost -= (ira_memory_move_cost[mode][rclass][0] * exit_freq
			 + ira_memory_move_cost[mode][rclass][1] * enter_freq);
	      else
		{
		  cost
		    += (ira_memory_move_cost[mode][rclass][0] * exit_freq
			+ ira_memory_move_cost[mode][rclass][1] * enter_freq);
		  if (hard_regno2 != hard_regno)
		    cost -= (ira_get_register_move_cost (mode, rclass, rclass)
			     * (exit_freq + enter_freq));
		}
	    }
	  if ((parent = loop_node->parent) != NULL
	      && (parent_allocno = parent->regno_allocno_map[regno]) != NULL)
	    {
	      ira_assert (rclass == ALLOCNO_COVER_CLASS (parent_allocno));
	      exit_freq	= ira_loop_edge_freq (loop_node, regno, true);
	      enter_freq = ira_loop_edge_freq (loop_node, regno, false);
	      if ((hard_regno2 = ALLOCNO_HARD_REGNO (parent_allocno)) < 0)
		cost -= (ira_memory_move_cost[mode][rclass][0] * exit_freq
			 + ira_memory_move_cost[mode][rclass][1] * enter_freq);
	      else
		{
		  cost
		    += (ira_memory_move_cost[mode][rclass][1] * exit_freq
			+ ira_memory_move_cost[mode][rclass][0] * enter_freq);
		  if (hard_regno2 != hard_regno)
		    cost -= (ira_get_register_move_cost (mode, rclass, rclass)
			     * (exit_freq + enter_freq));
		}
	    }
	  if (cost < 0)
	    {
	      ALLOCNO_HARD_REGNO (a) = -1;
	      if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
		{
		  fprintf
		    (ira_dump_file,
		     "      Moving spill/restore for a%dr%d up from loop %d",
		     ALLOCNO_NUM (a), regno, loop_node->loop->num);
		  fprintf (ira_dump_file, " - profit %d\n", -cost);
		}
	      changed_p = true;
	    }
	}
      if (! changed_p)
	break;
    }
}



/* Update current hard reg costs and current conflict hard reg costs
   for allocno A.  It is done by processing its copies containing
   other allocnos already assigned.  */
static void
update_curr_costs (ira_allocno_t a)
{
  int i, hard_regno, cost;
  enum machine_mode mode;
  enum reg_class cover_class, rclass;
  ira_allocno_t another_a;
  ira_copy_t cp, next_cp;

  ira_assert (! ALLOCNO_ASSIGNED_P (a));
  cover_class = ALLOCNO_COVER_CLASS (a);
  if (cover_class == NO_REGS)
    return;
  mode = ALLOCNO_MODE (a);
  for (cp = ALLOCNO_COPIES (a); cp != NULL; cp = next_cp)
    {
      if (cp->first == a)
	{
	  next_cp = cp->next_first_allocno_copy;
	  another_a = cp->second;
	}
      else if (cp->second == a)
	{
	  next_cp = cp->next_second_allocno_copy;
	  another_a = cp->first;
	}
      else
	gcc_unreachable ();
      if (! ira_reg_classes_intersect_p[cover_class][ALLOCNO_COVER_CLASS
						     (another_a)]
	  || ! ALLOCNO_ASSIGNED_P (another_a)
	  || (hard_regno = ALLOCNO_HARD_REGNO (another_a)) < 0)
	continue;
      rclass = REGNO_REG_CLASS (hard_regno);
      i = ira_class_hard_reg_index[cover_class][hard_regno];
      if (i < 0)
	continue;
      cost = (cp->first == a
	      ? ira_get_register_move_cost (mode, rclass, cover_class)
	      : ira_get_register_move_cost (mode, cover_class, rclass));
      ira_allocate_and_set_or_copy_costs
	(&ALLOCNO_UPDATED_HARD_REG_COSTS (a),
	 cover_class, ALLOCNO_COVER_CLASS_COST (a),
	 ALLOCNO_HARD_REG_COSTS (a));
      ira_allocate_and_set_or_copy_costs
	(&ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS (a),
	 cover_class, 0, ALLOCNO_CONFLICT_HARD_REG_COSTS (a));
      ALLOCNO_UPDATED_HARD_REG_COSTS (a)[i] -= cp->freq * cost;
      ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS (a)[i] -= cp->freq * cost;
    }
}

/* Try to assign hard registers to the unassigned allocnos and
   allocnos conflicting with them or conflicting with allocnos whose
   regno >= START_REGNO.  The function is called after ira_flattening,
   so more allocnos (including ones created in ira-emit.c) will have a
   chance to get a hard register.  We use simple assignment algorithm
   based on priorities.  */
void
ira_reassign_conflict_allocnos (int start_regno)
{
  int i, allocnos_to_color_num;
  ira_allocno_t a, conflict_a;
  ira_allocno_conflict_iterator aci;
  enum reg_class cover_class;
  bitmap allocnos_to_color;
  ira_allocno_iterator ai;

  allocnos_to_color = ira_allocate_bitmap ();
  allocnos_to_color_num = 0;
  FOR_EACH_ALLOCNO (a, ai)
    {
      if (! ALLOCNO_ASSIGNED_P (a)
	  && ! bitmap_bit_p (allocnos_to_color, ALLOCNO_NUM (a)))
	{
	  if (ALLOCNO_COVER_CLASS (a) != NO_REGS)
	    sorted_allocnos[allocnos_to_color_num++] = a;
	  else
	    {
	      ALLOCNO_ASSIGNED_P (a) = true;
	      ALLOCNO_HARD_REGNO (a) = -1;
	      ira_assert (ALLOCNO_UPDATED_HARD_REG_COSTS (a) == NULL);
	      ira_assert (ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS (a) == NULL);
	    }
	  bitmap_set_bit (allocnos_to_color, ALLOCNO_NUM (a));
	}
      if (ALLOCNO_REGNO (a) < start_regno
	  || (cover_class = ALLOCNO_COVER_CLASS (a)) == NO_REGS)
	continue;
      FOR_EACH_ALLOCNO_CONFLICT (a, conflict_a, aci)
	{
	  ira_assert (ira_reg_classes_intersect_p
		      [cover_class][ALLOCNO_COVER_CLASS (conflict_a)]);
	  if (bitmap_bit_p (allocnos_to_color, ALLOCNO_NUM (conflict_a)))
	    continue;
	  bitmap_set_bit (allocnos_to_color, ALLOCNO_NUM (conflict_a));
	  sorted_allocnos[allocnos_to_color_num++] = conflict_a;
	}
    }
  ira_free_bitmap (allocnos_to_color);
  if (allocnos_to_color_num > 1)
    {
      setup_allocno_priorities (sorted_allocnos, allocnos_to_color_num);
      qsort (sorted_allocnos, allocnos_to_color_num, sizeof (ira_allocno_t),
	     allocno_priority_compare_func);
    }
  for (i = 0; i < allocnos_to_color_num; i++)
    {
      a = sorted_allocnos[i];
      ALLOCNO_ASSIGNED_P (a) = false;
      ira_assert (ALLOCNO_UPDATED_HARD_REG_COSTS (a) == NULL);
      ira_assert (ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS (a) == NULL);
      update_curr_costs (a);
    }
  for (i = 0; i < allocnos_to_color_num; i++)
    {
      a = sorted_allocnos[i];
      if (assign_hard_reg (a, true))
	{
	  if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
	    fprintf
	      (ira_dump_file,
	       "      Secondary allocation: assign hard reg %d to reg %d\n",
	       ALLOCNO_HARD_REGNO (a), ALLOCNO_REGNO (a));
	}
    }
}



/* This page contains code to coalesce memory stack slots used by
   spilled allocnos.  This results in smaller stack frame, better data
   locality, and in smaller code for some architectures like
   x86/x86_64 where insn size depends on address displacement value.
   On the other hand, it can worsen insn scheduling after the RA but
   in practice it is less important than smaller stack frames.  */

/* Usage cost and order number of coalesced allocno set to which
   given pseudo register belongs to.  */
static int *regno_coalesced_allocno_cost;
static int *regno_coalesced_allocno_num;

/* Sort pseudos according frequencies of coalesced allocno sets they
   belong to (putting most frequently ones first), and according to
   coalesced allocno set order numbers.  */
static int
coalesced_pseudo_reg_freq_compare (const void *v1p, const void *v2p)
{
  const int regno1 = *(const int *) v1p;
  const int regno2 = *(const int *) v2p;
  int diff;

  if ((diff = (regno_coalesced_allocno_cost[regno2]
	       - regno_coalesced_allocno_cost[regno1])) != 0)
    return diff;
  if ((diff = (regno_coalesced_allocno_num[regno1]
	       - regno_coalesced_allocno_num[regno2])) != 0)
    return diff;
  return regno1 - regno2;
}

/* Widest width in which each pseudo reg is referred to (via subreg).
   It is used for sorting pseudo registers.  */
static unsigned int *regno_max_ref_width;

/* Redefine STACK_GROWS_DOWNWARD in terms of 0 or 1.  */
#ifdef STACK_GROWS_DOWNWARD
# undef STACK_GROWS_DOWNWARD
# define STACK_GROWS_DOWNWARD 1
#else
# define STACK_GROWS_DOWNWARD 0
#endif

/* Sort pseudos according their slot numbers (putting ones with
  smaller numbers first, or last when the frame pointer is not
  needed).  */
static int
coalesced_pseudo_reg_slot_compare (const void *v1p, const void *v2p)
{
  const int regno1 = *(const int *) v1p;
  const int regno2 = *(const int *) v2p;
  ira_allocno_t a1 = ira_regno_allocno_map[regno1];
  ira_allocno_t a2 = ira_regno_allocno_map[regno2];
  int diff, slot_num1, slot_num2;
  int total_size1, total_size2;

  if (a1 == NULL || ALLOCNO_HARD_REGNO (a1) >= 0)
    {
      if (a2 == NULL || ALLOCNO_HARD_REGNO (a2) >= 0)
	return regno1 - regno2;
      return 1;
    }
  else if (a2 == NULL || ALLOCNO_HARD_REGNO (a2) >= 0)
    return -1;
  slot_num1 = -ALLOCNO_HARD_REGNO (a1);
  slot_num2 = -ALLOCNO_HARD_REGNO (a2);
  if ((diff = slot_num1 - slot_num2) != 0)
    return (frame_pointer_needed
	    || !FRAME_GROWS_DOWNWARD == STACK_GROWS_DOWNWARD ? diff : -diff);
  total_size1 = MAX (PSEUDO_REGNO_BYTES (regno1), regno_max_ref_width[regno1]);
  total_size2 = MAX (PSEUDO_REGNO_BYTES (regno2), regno_max_ref_width[regno2]);
  if ((diff = total_size2 - total_size1) != 0)
    return diff;
  return regno1 - regno2;
}

/* Setup REGNO_COALESCED_ALLOCNO_COST and REGNO_COALESCED_ALLOCNO_NUM
   for coalesced allocno sets containing allocnos with their regnos
   given in array PSEUDO_REGNOS of length N.  */
static void
setup_coalesced_allocno_costs_and_nums (int *pseudo_regnos, int n)
{
  int i, num, regno, cost;
  ira_allocno_t allocno, a;

  for (num = i = 0; i < n; i++)
    {
      regno = pseudo_regnos[i];
      allocno = ira_regno_allocno_map[regno];
      if (allocno == NULL)
	{
	  regno_coalesced_allocno_cost[regno] = 0;
	  regno_coalesced_allocno_num[regno] = ++num;
	  continue;
	}
      if (ALLOCNO_FIRST_COALESCED_ALLOCNO (allocno) != allocno)
	continue;
      num++;
      for (cost = 0, a = ALLOCNO_NEXT_COALESCED_ALLOCNO (allocno);;
	   a = ALLOCNO_NEXT_COALESCED_ALLOCNO (a))
	{
	  cost += ALLOCNO_FREQ (a);
	  if (a == allocno)
	    break;
	}
      for (a = ALLOCNO_NEXT_COALESCED_ALLOCNO (allocno);;
	   a = ALLOCNO_NEXT_COALESCED_ALLOCNO (a))
	{
	  regno_coalesced_allocno_num[ALLOCNO_REGNO (a)] = num;
	  regno_coalesced_allocno_cost[ALLOCNO_REGNO (a)] = cost;
	  if (a == allocno)
	    break;
	}
    }
}

/* Collect spilled allocnos representing coalesced allocno sets (the
   first coalesced allocno).  The collected allocnos are returned
   through array SPILLED_COALESCED_ALLOCNOS.  The function returns the
   number of the collected allocnos.  The allocnos are given by their
   regnos in array PSEUDO_REGNOS of length N.  */
static int
collect_spilled_coalesced_allocnos (int *pseudo_regnos, int n,
				    ira_allocno_t *spilled_coalesced_allocnos)
{
  int i, num, regno;
  ira_allocno_t allocno;

  for (num = i = 0; i < n; i++)
    {
      regno = pseudo_regnos[i];
      allocno = ira_regno_allocno_map[regno];
      if (allocno == NULL || ALLOCNO_HARD_REGNO (allocno) >= 0
	  || ALLOCNO_FIRST_COALESCED_ALLOCNO (allocno) != allocno)
	continue;
      spilled_coalesced_allocnos[num++] = allocno;
    }
  return num;
}

/* Array of live ranges of size IRA_ALLOCNOS_NUM.  Live range for
   given slot contains live ranges of coalesced allocnos assigned to
   given slot.  */
static allocno_live_range_t *slot_coalesced_allocnos_live_ranges;

/* Return TRUE if coalesced allocnos represented by ALLOCNO has live
   ranges intersected with live ranges of coalesced allocnos assigned
   to slot with number N.  */
static bool
slot_coalesced_allocno_live_ranges_intersect_p (ira_allocno_t allocno, int n)
{
  ira_allocno_t a;

  for (a = ALLOCNO_NEXT_COALESCED_ALLOCNO (allocno);;
       a = ALLOCNO_NEXT_COALESCED_ALLOCNO (a))
    {
      if (ira_allocno_live_ranges_intersect_p
	  (slot_coalesced_allocnos_live_ranges[n], ALLOCNO_LIVE_RANGES (a)))
	return true;
      if (a == allocno)
	break;
    }
  return false;
}

/* Update live ranges of slot to which coalesced allocnos represented
   by ALLOCNO were assigned.  */
static void
setup_slot_coalesced_allocno_live_ranges (ira_allocno_t allocno)
{
  int n;
  ira_allocno_t a;
  allocno_live_range_t r;

  n = ALLOCNO_TEMP (allocno);
  for (a = ALLOCNO_NEXT_COALESCED_ALLOCNO (allocno);;
       a = ALLOCNO_NEXT_COALESCED_ALLOCNO (a))
    {
      r = ira_copy_allocno_live_range_list (ALLOCNO_LIVE_RANGES (a));
      slot_coalesced_allocnos_live_ranges[n]
	= ira_merge_allocno_live_ranges
	  (slot_coalesced_allocnos_live_ranges[n], r);
      if (a == allocno)
	break;
    }
}

/* We have coalesced allocnos involving in copies.  Coalesce allocnos
   further in order to share the same memory stack slot.  Allocnos
   representing sets of allocnos coalesced before the call are given
   in array SPILLED_COALESCED_ALLOCNOS of length NUM.  Return TRUE if
   some allocnos were coalesced in the function.  */
static bool
coalesce_spill_slots (ira_allocno_t *spilled_coalesced_allocnos, int num)
{
  int i, j, n, last_coalesced_allocno_num;
  ira_allocno_t allocno, a;
  bool merged_p = false;
  bitmap set_jump_crosses = regstat_get_setjmp_crosses ();

  slot_coalesced_allocnos_live_ranges
    = (allocno_live_range_t *) ira_allocate (sizeof (allocno_live_range_t)
					     * ira_allocnos_num);
  memset (slot_coalesced_allocnos_live_ranges, 0,
	  sizeof (allocno_live_range_t) * ira_allocnos_num);
  last_coalesced_allocno_num = 0;
  /* Coalesce non-conflicting spilled allocnos preferring most
     frequently used.  */
  for (i = 0; i < num; i++)
    {
      allocno = spilled_coalesced_allocnos[i];
      if (ALLOCNO_FIRST_COALESCED_ALLOCNO (allocno) != allocno
	  || bitmap_bit_p (set_jump_crosses, ALLOCNO_REGNO (allocno))
	  || (ALLOCNO_REGNO (allocno) < ira_reg_equiv_len
	      && (ira_reg_equiv_const[ALLOCNO_REGNO (allocno)] != NULL_RTX
		  || ira_reg_equiv_invariant_p[ALLOCNO_REGNO (allocno)])))
	continue;
      for (j = 0; j < i; j++)
	{
	  a = spilled_coalesced_allocnos[j];
	  n = ALLOCNO_TEMP (a);
	  if (ALLOCNO_FIRST_COALESCED_ALLOCNO (a) == a
	      && ! bitmap_bit_p (set_jump_crosses, ALLOCNO_REGNO (a))
	      && (ALLOCNO_REGNO (a) >= ira_reg_equiv_len
		  || (! ira_reg_equiv_invariant_p[ALLOCNO_REGNO (a)]
		      && ira_reg_equiv_const[ALLOCNO_REGNO (a)] == NULL_RTX))
	      && ! slot_coalesced_allocno_live_ranges_intersect_p (allocno, n))
	    break;
	}
      if (j >= i)
	{
	  /* No coalescing: set up number for coalesced allocnos
	     represented by ALLOCNO.  */
	  ALLOCNO_TEMP (allocno) = last_coalesced_allocno_num++;
	  setup_slot_coalesced_allocno_live_ranges (allocno);
	}
      else
	{
	  allocno_coalesced_p = true;
	  merged_p = true;
	  if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
	    fprintf (ira_dump_file,
		     "      Coalescing spilled allocnos a%dr%d->a%dr%d\n",
		     ALLOCNO_NUM (allocno), ALLOCNO_REGNO (allocno),
		     ALLOCNO_NUM (a), ALLOCNO_REGNO (a));
	  ALLOCNO_TEMP (allocno) = ALLOCNO_TEMP (a);
	  setup_slot_coalesced_allocno_live_ranges (allocno);
	  merge_allocnos (a, allocno);
	  ira_assert (ALLOCNO_FIRST_COALESCED_ALLOCNO (a) == a);
	}
    }
  for (i = 0; i < ira_allocnos_num; i++)
    ira_finish_allocno_live_range_list
      (slot_coalesced_allocnos_live_ranges[i]);
  ira_free (slot_coalesced_allocnos_live_ranges);
  return merged_p;
}

/* Sort pseudo-register numbers in array PSEUDO_REGNOS of length N for
   subsequent assigning stack slots to them in the reload pass.  To do
   this we coalesce spilled allocnos first to decrease the number of
   memory-memory move insns.  This function is called by the
   reload.  */
void
ira_sort_regnos_for_alter_reg (int *pseudo_regnos, int n,
			       unsigned int *reg_max_ref_width)
{
  int max_regno = max_reg_num ();
  int i, regno, num, slot_num;
  ira_allocno_t allocno, a;
  ira_allocno_iterator ai;
  ira_allocno_t *spilled_coalesced_allocnos;

  processed_coalesced_allocno_bitmap = ira_allocate_bitmap ();
  /* Set up allocnos can be coalesced.  */
  coloring_allocno_bitmap = ira_allocate_bitmap ();
  for (i = 0; i < n; i++)
    {
      regno = pseudo_regnos[i];
      allocno = ira_regno_allocno_map[regno];
      if (allocno != NULL)
	bitmap_set_bit (coloring_allocno_bitmap,
			ALLOCNO_NUM (allocno));
    }
  allocno_coalesced_p = false;
  coalesce_allocnos (true);
  ira_free_bitmap (coloring_allocno_bitmap);
  regno_coalesced_allocno_cost
    = (int *) ira_allocate (max_regno * sizeof (int));
  regno_coalesced_allocno_num
    = (int *) ira_allocate (max_regno * sizeof (int));
  memset (regno_coalesced_allocno_num, 0, max_regno * sizeof (int));
  setup_coalesced_allocno_costs_and_nums (pseudo_regnos, n);
  /* Sort regnos according frequencies of the corresponding coalesced
     allocno sets.  */
  qsort (pseudo_regnos, n, sizeof (int), coalesced_pseudo_reg_freq_compare);
  spilled_coalesced_allocnos
    = (ira_allocno_t *) ira_allocate (ira_allocnos_num
				      * sizeof (ira_allocno_t));
  /* Collect allocnos representing the spilled coalesced allocno
     sets.  */
  num = collect_spilled_coalesced_allocnos (pseudo_regnos, n,
					    spilled_coalesced_allocnos);
  if (flag_ira_share_spill_slots
      && coalesce_spill_slots (spilled_coalesced_allocnos, num))
    {
      setup_coalesced_allocno_costs_and_nums (pseudo_regnos, n);
      qsort (pseudo_regnos, n, sizeof (int),
	     coalesced_pseudo_reg_freq_compare);
      num = collect_spilled_coalesced_allocnos (pseudo_regnos, n,
						spilled_coalesced_allocnos);
    }
  ira_free_bitmap (processed_coalesced_allocno_bitmap);
  allocno_coalesced_p = false;
  /* Assign stack slot numbers to spilled allocno sets, use smaller
     numbers for most frequently used coalesced allocnos.  -1 is
     reserved for dynamic search of stack slots for pseudos spilled by
     the reload.  */
  slot_num = 1;
  for (i = 0; i < num; i++)
    {
      allocno = spilled_coalesced_allocnos[i];
      if (ALLOCNO_FIRST_COALESCED_ALLOCNO (allocno) != allocno
	  || ALLOCNO_HARD_REGNO (allocno) >= 0
	  || (ALLOCNO_REGNO (allocno) < ira_reg_equiv_len
	      && (ira_reg_equiv_const[ALLOCNO_REGNO (allocno)] != NULL_RTX
		  || ira_reg_equiv_invariant_p[ALLOCNO_REGNO (allocno)])))
	continue;
      if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
	fprintf (ira_dump_file, "      Slot %d (freq,size):", slot_num);
      slot_num++;
      for (a = ALLOCNO_NEXT_COALESCED_ALLOCNO (allocno);;
	   a = ALLOCNO_NEXT_COALESCED_ALLOCNO (a))
	{
	  ira_assert (ALLOCNO_HARD_REGNO (a) < 0);
	  ALLOCNO_HARD_REGNO (a) = -slot_num;
	  if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
	    fprintf (ira_dump_file, " a%dr%d(%d,%d)",
		     ALLOCNO_NUM (a), ALLOCNO_REGNO (a), ALLOCNO_FREQ (a),
		     MAX (PSEUDO_REGNO_BYTES (ALLOCNO_REGNO (a)),
			  reg_max_ref_width[ALLOCNO_REGNO (a)]));

	  if (a == allocno)
	    break;
	}
      if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
	fprintf (ira_dump_file, "\n");
    }
  ira_spilled_reg_stack_slots_num = slot_num - 1;
  ira_free (spilled_coalesced_allocnos);
  /* Sort regnos according the slot numbers.  */
  regno_max_ref_width = reg_max_ref_width;
  qsort (pseudo_regnos, n, sizeof (int), coalesced_pseudo_reg_slot_compare);
  /* Uncoalesce allocnos which is necessary for (re)assigning during
     the reload pass.  */
  FOR_EACH_ALLOCNO (a, ai)
    {
      ALLOCNO_FIRST_COALESCED_ALLOCNO (a) = a;
      ALLOCNO_NEXT_COALESCED_ALLOCNO (a) = a;
    }
  ira_free (regno_coalesced_allocno_num);
  ira_free (regno_coalesced_allocno_cost);
}



/* This page contains code used by the reload pass to improve the
   final code.  */

/* The function is called from reload to mark changes in the
   allocation of REGNO made by the reload.  Remember that reg_renumber
   reflects the change result.  */
void
ira_mark_allocation_change (int regno)
{
  ira_allocno_t a = ira_regno_allocno_map[regno];
  int old_hard_regno, hard_regno, cost;
  enum reg_class cover_class = ALLOCNO_COVER_CLASS (a);

  ira_assert (a != NULL);
  hard_regno = reg_renumber[regno];
  if ((old_hard_regno = ALLOCNO_HARD_REGNO (a)) == hard_regno)
    return;
  if (old_hard_regno < 0)
    cost = -ALLOCNO_MEMORY_COST (a);
  else
    {
      ira_assert (ira_class_hard_reg_index[cover_class][old_hard_regno] >= 0);
      cost = -(ALLOCNO_HARD_REG_COSTS (a) == NULL
	       ? ALLOCNO_COVER_CLASS_COST (a)
	       : ALLOCNO_HARD_REG_COSTS (a)
	         [ira_class_hard_reg_index[cover_class][old_hard_regno]]);
      update_copy_costs (a, false);
    }
  ira_overall_cost -= cost;
  ALLOCNO_HARD_REGNO (a) = hard_regno;
  if (hard_regno < 0)
    {
      ALLOCNO_HARD_REGNO (a) = -1;
      cost += ALLOCNO_MEMORY_COST (a);
    }
  else if (ira_class_hard_reg_index[cover_class][hard_regno] >= 0)
    {
      cost += (ALLOCNO_HARD_REG_COSTS (a) == NULL
	       ? ALLOCNO_COVER_CLASS_COST (a)
	       : ALLOCNO_HARD_REG_COSTS (a)
	         [ira_class_hard_reg_index[cover_class][hard_regno]]);
      update_copy_costs (a, true);
    }
  else
    /* Reload changed class of the allocno.  */
    cost = 0;
  ira_overall_cost += cost;
}

/* This function is called when reload deletes memory-memory move.  In
   this case we marks that the allocation of the corresponding
   allocnos should be not changed in future.  Otherwise we risk to get
   a wrong code.  */
void
ira_mark_memory_move_deletion (int dst_regno, int src_regno)
{
  ira_allocno_t dst = ira_regno_allocno_map[dst_regno];
  ira_allocno_t src = ira_regno_allocno_map[src_regno];

  ira_assert (dst != NULL && src != NULL
	      && ALLOCNO_HARD_REGNO (dst) < 0
	      && ALLOCNO_HARD_REGNO (src) < 0);
  ALLOCNO_DONT_REASSIGN_P (dst) = true;
  ALLOCNO_DONT_REASSIGN_P (src) = true;
}

/* Try to assign a hard register (except for FORBIDDEN_REGS) to
   allocno A and return TRUE in the case of success.  */
static bool
allocno_reload_assign (ira_allocno_t a, HARD_REG_SET forbidden_regs)
{
  int hard_regno;
  enum reg_class cover_class;
  int regno = ALLOCNO_REGNO (a);

  IOR_HARD_REG_SET (ALLOCNO_TOTAL_CONFLICT_HARD_REGS (a), forbidden_regs);
  if (! flag_caller_saves && ALLOCNO_CALLS_CROSSED_NUM (a) != 0)
    IOR_HARD_REG_SET (ALLOCNO_TOTAL_CONFLICT_HARD_REGS (a), call_used_reg_set);
  ALLOCNO_ASSIGNED_P (a) = false;
  ira_assert (ALLOCNO_UPDATED_HARD_REG_COSTS (a) == NULL);
  ira_assert (ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS (a) == NULL);
  cover_class = ALLOCNO_COVER_CLASS (a);
  update_curr_costs (a);
  assign_hard_reg (a, true);
  hard_regno = ALLOCNO_HARD_REGNO (a);
  reg_renumber[regno] = hard_regno;
  if (hard_regno < 0)
    ALLOCNO_HARD_REGNO (a) = -1;
  else
    {
      ira_assert (ira_class_hard_reg_index[cover_class][hard_regno] >= 0);
      ira_overall_cost -= (ALLOCNO_MEMORY_COST (a)
			   - (ALLOCNO_HARD_REG_COSTS (a) == NULL
			      ? ALLOCNO_COVER_CLASS_COST (a)
			      : ALLOCNO_HARD_REG_COSTS (a)
			        [ira_class_hard_reg_index
				 [cover_class][hard_regno]]));
      if (ALLOCNO_CALLS_CROSSED_NUM (a) != 0
	  && ! ira_hard_reg_not_in_set_p (hard_regno, ALLOCNO_MODE (a),
					  call_used_reg_set))
	{
	  ira_assert (flag_caller_saves);
	  caller_save_needed = 1;
	}
    }

  /* If we found a hard register, modify the RTL for the pseudo
     register to show the hard register, and mark the pseudo register
     live.  */
  if (reg_renumber[regno] >= 0)
    {
      if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
	fprintf (ira_dump_file, ": reassign to %d\n", reg_renumber[regno]);
      SET_REGNO (regno_reg_rtx[regno], reg_renumber[regno]);
      mark_home_live (regno);
    }
  else if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
    fprintf (ira_dump_file, "\n");

  return reg_renumber[regno] >= 0;
}

/* Sort pseudos according their usage frequencies (putting most
   frequently ones first).  */
static int
pseudo_reg_compare (const void *v1p, const void *v2p)
{
  int regno1 = *(const int *) v1p;
  int regno2 = *(const int *) v2p;
  int diff;

  if ((diff = REG_FREQ (regno2) - REG_FREQ (regno1)) != 0)
    return diff;
  return regno1 - regno2;
}

/* Try to allocate hard registers to SPILLED_PSEUDO_REGS (there are
   NUM of them) or spilled pseudos conflicting with pseudos in
   SPILLED_PSEUDO_REGS.  Return TRUE and update SPILLED, if the
   allocation has been changed.  The function doesn't use
   BAD_SPILL_REGS and hard registers in PSEUDO_FORBIDDEN_REGS and
   PSEUDO_PREVIOUS_REGS for the corresponding pseudos.  The function
   is called by the reload pass at the end of each reload
   iteration.  */
bool
ira_reassign_pseudos (int *spilled_pseudo_regs, int num,
		      HARD_REG_SET bad_spill_regs,
		      HARD_REG_SET *pseudo_forbidden_regs,
		      HARD_REG_SET *pseudo_previous_regs,  bitmap spilled)
{
  int i, m, n, regno;
  bool changed_p;
  ira_allocno_t a, conflict_a;
  HARD_REG_SET forbidden_regs;
  ira_allocno_conflict_iterator aci;

  if (num > 1)
    qsort (spilled_pseudo_regs, num, sizeof (int), pseudo_reg_compare);
  changed_p = false;
  /* Try to assign hard registers to pseudos from
     SPILLED_PSEUDO_REGS.  */
  for (m = i = 0; i < num; i++)
    {
      regno = spilled_pseudo_regs[i];
      COPY_HARD_REG_SET (forbidden_regs, bad_spill_regs);
      IOR_HARD_REG_SET (forbidden_regs, pseudo_forbidden_regs[regno]);
      IOR_HARD_REG_SET (forbidden_regs, pseudo_previous_regs[regno]);
      gcc_assert (reg_renumber[regno] < 0);
      a = ira_regno_allocno_map[regno];
      ira_mark_allocation_change (regno);
      ira_assert (reg_renumber[regno] < 0);
      if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
	fprintf (ira_dump_file,
		 "      Spill %d(a%d), cost=%d", regno, ALLOCNO_NUM (a),
		 ALLOCNO_MEMORY_COST (a)
		 - ALLOCNO_COVER_CLASS_COST (a));
      allocno_reload_assign (a, forbidden_regs);
      if (reg_renumber[regno] >= 0)
	{
	  CLEAR_REGNO_REG_SET (spilled, regno);
	  changed_p = true;
	}
      else
	spilled_pseudo_regs[m++] = regno;
    }
  if (m == 0)
    return changed_p;
  if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
    {
      fprintf (ira_dump_file, "      Spilled regs");
      for (i = 0; i < m; i++)
	fprintf (ira_dump_file, " %d", spilled_pseudo_regs[i]);
      fprintf (ira_dump_file, "\n");
    }
  /* Try to assign hard registers to pseudos conflicting with ones
     from SPILLED_PSEUDO_REGS.  */
  for (i = n = 0; i < m; i++)
    {
      regno = spilled_pseudo_regs[i];
      a = ira_regno_allocno_map[regno];
      FOR_EACH_ALLOCNO_CONFLICT (a, conflict_a, aci)
	if (ALLOCNO_HARD_REGNO (conflict_a) < 0
	    && ! ALLOCNO_DONT_REASSIGN_P (conflict_a)
	    && ! bitmap_bit_p (consideration_allocno_bitmap,
			       ALLOCNO_NUM (conflict_a)))
	  {
	    sorted_allocnos[n++] = conflict_a;
	    bitmap_set_bit (consideration_allocno_bitmap,
			    ALLOCNO_NUM (conflict_a));
	  }
    }
  if (n != 0)
    {
      setup_allocno_priorities (sorted_allocnos, n);
      qsort (sorted_allocnos, n, sizeof (ira_allocno_t),
	     allocno_priority_compare_func);
      for (i = 0; i < n; i++)
	{
	  a = sorted_allocnos[i];
	  regno = ALLOCNO_REGNO (a);
	  COPY_HARD_REG_SET (forbidden_regs, bad_spill_regs);
	  IOR_HARD_REG_SET (forbidden_regs, pseudo_forbidden_regs[regno]);
	  IOR_HARD_REG_SET (forbidden_regs, pseudo_previous_regs[regno]);
	  if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
	    fprintf (ira_dump_file,
		     "        Try assign %d(a%d), cost=%d",
		     regno, ALLOCNO_NUM (a),
		     ALLOCNO_MEMORY_COST (a)
		     - ALLOCNO_COVER_CLASS_COST (a));
	  if (allocno_reload_assign (a, forbidden_regs))
	    {
	      changed_p = true;
	      bitmap_clear_bit (spilled, regno);
	    }
	}
    }
  return changed_p;
}

/* The function is called by reload and returns already allocated
   stack slot (if any) for REGNO with given INHERENT_SIZE and
   TOTAL_SIZE.  In the case of failure to find a slot which can be
   used for REGNO, the function returns NULL.  */
rtx
ira_reuse_stack_slot (int regno, unsigned int inherent_size,
		      unsigned int total_size)
{
  unsigned int i;
  int slot_num, best_slot_num;
  int cost, best_cost;
  ira_copy_t cp, next_cp;
  ira_allocno_t another_allocno, allocno = ira_regno_allocno_map[regno];
  rtx x;
  bitmap_iterator bi;
  struct ira_spilled_reg_stack_slot *slot = NULL;

  ira_assert (inherent_size == PSEUDO_REGNO_BYTES (regno)
	      && inherent_size <= total_size
	      && ALLOCNO_HARD_REGNO (allocno) < 0);
  if (! flag_ira_share_spill_slots)
    return NULL_RTX;
  slot_num = -ALLOCNO_HARD_REGNO (allocno) - 2;
  if (slot_num != -1)
    {
      slot = &ira_spilled_reg_stack_slots[slot_num];
      x = slot->mem;
    }
  else
    {
      best_cost = best_slot_num = -1;
      x = NULL_RTX;
      /* It means that the pseudo was spilled in the reload pass, try
	 to reuse a slot.  */
      for (slot_num = 0;
	   slot_num < ira_spilled_reg_stack_slots_num;
	   slot_num++)
	{
	  slot = &ira_spilled_reg_stack_slots[slot_num];
	  if (slot->mem == NULL_RTX)
	    continue;
	  if (slot->width < total_size
	      || GET_MODE_SIZE (GET_MODE (slot->mem)) < inherent_size)
	    continue;

	  EXECUTE_IF_SET_IN_BITMAP (&slot->spilled_regs,
				    FIRST_PSEUDO_REGISTER, i, bi)
	    {
	      another_allocno = ira_regno_allocno_map[i];
	      if (allocnos_have_intersected_live_ranges_p (allocno,
							   another_allocno))
		goto cont;
	    }
	  for (cost = 0, cp = ALLOCNO_COPIES (allocno);
	       cp != NULL;
	       cp = next_cp)
	    {
	      if (cp->first == allocno)
		{
		  next_cp = cp->next_first_allocno_copy;
		  another_allocno = cp->second;
		}
	      else if (cp->second == allocno)
		{
		  next_cp = cp->next_second_allocno_copy;
		  another_allocno = cp->first;
		}
	      else
		gcc_unreachable ();
	      if (cp->insn == NULL_RTX)
		continue;
	      if (bitmap_bit_p (&slot->spilled_regs,
				ALLOCNO_REGNO (another_allocno)))
		cost += cp->freq;
	    }
	  if (cost > best_cost)
	    {
	      best_cost = cost;
	      best_slot_num = slot_num;
	    }
	cont:
	  ;
	}
      if (best_cost >= 0)
	{
	  slot_num = best_slot_num;
	  slot = &ira_spilled_reg_stack_slots[slot_num];
	  SET_REGNO_REG_SET (&slot->spilled_regs, regno);
	  x = slot->mem;
	  ALLOCNO_HARD_REGNO (allocno) = -slot_num - 2;
	}
    }
  if (x != NULL_RTX)
    {
      ira_assert (slot->width >= total_size);
#ifdef ENABLE_IRA_CHECKING
      EXECUTE_IF_SET_IN_BITMAP (&slot->spilled_regs,
				FIRST_PSEUDO_REGISTER, i, bi)
	{
	  ira_assert (! pseudos_have_intersected_live_ranges_p (regno, i));
	}
#endif
      SET_REGNO_REG_SET (&slot->spilled_regs, regno);
      if (internal_flag_ira_verbose > 3 && ira_dump_file)
	{
	  fprintf (ira_dump_file, "      Assigning %d(freq=%d) slot %d of",
		   regno, REG_FREQ (regno), slot_num);
	  EXECUTE_IF_SET_IN_BITMAP (&slot->spilled_regs,
				    FIRST_PSEUDO_REGISTER, i, bi)
	    {
	      if ((unsigned) regno != i)
		fprintf (ira_dump_file, " %d", i);
	    }
	  fprintf (ira_dump_file, "\n");
	}
    }
  return x;
}

/* This is called by reload every time a new stack slot X with
   TOTAL_SIZE was allocated for REGNO.  We store this info for
   subsequent ira_reuse_stack_slot calls.  */
void
ira_mark_new_stack_slot (rtx x, int regno, unsigned int total_size)
{
  struct ira_spilled_reg_stack_slot *slot;
  int slot_num;
  ira_allocno_t allocno;

  ira_assert (PSEUDO_REGNO_BYTES (regno) <= total_size);
  allocno = ira_regno_allocno_map[regno];
  slot_num = -ALLOCNO_HARD_REGNO (allocno) - 2;
  if (slot_num == -1)
    {
      slot_num = ira_spilled_reg_stack_slots_num++;
      ALLOCNO_HARD_REGNO (allocno) = -slot_num - 2;
    }
  slot = &ira_spilled_reg_stack_slots[slot_num];
  INIT_REG_SET (&slot->spilled_regs);
  SET_REGNO_REG_SET (&slot->spilled_regs, regno);
  slot->mem = x;
  slot->width = total_size;
  if (internal_flag_ira_verbose > 3 && ira_dump_file)
    fprintf (ira_dump_file, "      Assigning %d(freq=%d) a new slot %d\n",
	     regno, REG_FREQ (regno), slot_num);
}


/* Return spill cost for pseudo-registers whose numbers are in array
   REGNOS (with a negative number as an end marker) for reload with
   given IN and OUT for INSN.  Return also number points (through
   EXCESS_PRESSURE_LIVE_LENGTH) where the pseudo-register lives and
   the register pressure is high, number of references of the
   pseudo-registers (through NREFS), number of callee-clobbered
   hard-registers occupied by the pseudo-registers (through
   CALL_USED_COUNT), and the first hard regno occupied by the
   pseudo-registers (through FIRST_HARD_REGNO).  */
static int
calculate_spill_cost (int *regnos, rtx in, rtx out, rtx insn,
		      int *excess_pressure_live_length,
		      int *nrefs, int *call_used_count, int *first_hard_regno)
{
  int i, cost, regno, hard_regno, j, count, saved_cost, nregs;
  bool in_p, out_p;
  int length;
  ira_allocno_t a;

  *nrefs = 0;
  for (length = count = cost = i = 0;; i++)
    {
      regno = regnos[i];
      if (regno < 0)
	break;
      *nrefs += REG_N_REFS (regno);
      hard_regno = reg_renumber[regno];
      ira_assert (hard_regno >= 0);
      a = ira_regno_allocno_map[regno];
      length += ALLOCNO_EXCESS_PRESSURE_POINTS_NUM (a);
      cost += ALLOCNO_MEMORY_COST (a) - ALLOCNO_COVER_CLASS_COST (a);
      nregs = hard_regno_nregs[hard_regno][ALLOCNO_MODE (a)];
      for (j = 0; j < nregs; j++)
	if (! TEST_HARD_REG_BIT (call_used_reg_set, hard_regno + j))
	  break;
      if (j == nregs)
	count++;
      in_p = in && REG_P (in) && (int) REGNO (in) == hard_regno;
      out_p = out && REG_P (out) && (int) REGNO (out) == hard_regno;
      if ((in_p || out_p)
	  && find_regno_note (insn, REG_DEAD, hard_regno) != NULL_RTX)
	{
	  saved_cost = 0;
	  if (in_p)
	    saved_cost += ira_memory_move_cost
	                  [ALLOCNO_MODE (a)][ALLOCNO_COVER_CLASS (a)][1];
	  if (out_p)
	    saved_cost
	      += ira_memory_move_cost
	         [ALLOCNO_MODE (a)][ALLOCNO_COVER_CLASS (a)][0];
	  cost -= REG_FREQ_FROM_BB (BLOCK_FOR_INSN (insn)) * saved_cost;
	}
    }
  *excess_pressure_live_length = length;
  *call_used_count = count;
  hard_regno = -1;
  if (regnos[0] >= 0)
    {
      hard_regno = reg_renumber[regnos[0]];
    }
  *first_hard_regno = hard_regno;
  return cost;
}

/* Return TRUE if spilling pseudo-registers whose numbers are in array
   REGNOS is better than spilling pseudo-registers with numbers in
   OTHER_REGNOS for reload with given IN and OUT for INSN.  The
   function used by the reload pass to make better register spilling
   decisions.  */
bool
ira_better_spill_reload_regno_p (int *regnos, int *other_regnos,
				 rtx in, rtx out, rtx insn)
{
  int cost, other_cost;
  int length, other_length;
  int nrefs, other_nrefs;
  int call_used_count, other_call_used_count;
  int hard_regno, other_hard_regno;

  cost = calculate_spill_cost (regnos, in, out, insn,
			       &length, &nrefs, &call_used_count, &hard_regno);
  other_cost = calculate_spill_cost (other_regnos, in, out, insn,
				     &other_length, &other_nrefs,
				     &other_call_used_count,
				     &other_hard_regno);
  if (nrefs == 0 && other_nrefs != 0)
    return true;
  if (nrefs != 0 && other_nrefs == 0)
    return false;
  if (cost != other_cost)
    return cost < other_cost;
  if (length != other_length)
    return length > other_length;
#ifdef REG_ALLOC_ORDER
  if (hard_regno >= 0 && other_hard_regno >= 0)
    return (inv_reg_alloc_order[hard_regno]
	    < inv_reg_alloc_order[other_hard_regno]);
#else
  if (call_used_count != other_call_used_count)
    return call_used_count > other_call_used_count;
#endif
  return false;
}



/* Allocate and initialize data necessary for assign_hard_reg.  */
void
ira_initiate_assign (void)
{
  sorted_allocnos
    = (ira_allocno_t *) ira_allocate (sizeof (ira_allocno_t)
				      * ira_allocnos_num);
  consideration_allocno_bitmap = ira_allocate_bitmap ();
  initiate_cost_update ();
  allocno_priorities = (int *) ira_allocate (sizeof (int) * ira_allocnos_num);
}

/* Deallocate data used by assign_hard_reg.  */
void
ira_finish_assign (void)
{
  ira_free (sorted_allocnos);
  ira_free_bitmap (consideration_allocno_bitmap);
  finish_cost_update ();
  ira_free (allocno_priorities);
}



/* Entry function doing color-based register allocation.  */
static void
color (void)
{
  allocno_stack_vec = VEC_alloc (ira_allocno_t, heap, ira_allocnos_num);
  removed_splay_allocno_vec
    = VEC_alloc (ira_allocno_t, heap, ira_allocnos_num);
  memset (allocated_hardreg_p, 0, sizeof (allocated_hardreg_p));
  ira_initiate_assign ();
  do_coloring ();
  ira_finish_assign ();
  VEC_free (ira_allocno_t, heap, removed_splay_allocno_vec);
  VEC_free (ira_allocno_t, heap, allocno_stack_vec);
  move_spill_restore ();
}



/* This page contains a simple register allocator without usage of
   allocno conflicts.  This is used for fast allocation for -O0.  */

/* Do register allocation by not using allocno conflicts.  It uses
   only allocno live ranges.  The algorithm is close to Chow's
   priority coloring.  */
static void
fast_allocation (void)
{
  int i, j, k, num, class_size, hard_regno;
#ifdef STACK_REGS
  bool no_stack_reg_p;
#endif
  enum reg_class cover_class;
  enum machine_mode mode;
  ira_allocno_t a;
  ira_allocno_iterator ai;
  allocno_live_range_t r;
  HARD_REG_SET conflict_hard_regs, *used_hard_regs;

  sorted_allocnos = (ira_allocno_t *) ira_allocate (sizeof (ira_allocno_t)
						    * ira_allocnos_num);
  num = 0;
  FOR_EACH_ALLOCNO (a, ai)
    sorted_allocnos[num++] = a;
  allocno_priorities = (int *) ira_allocate (sizeof (int) * ira_allocnos_num);
  setup_allocno_priorities (sorted_allocnos, num);
  used_hard_regs = (HARD_REG_SET *) ira_allocate (sizeof (HARD_REG_SET)
						  * ira_max_point);
  for (i = 0; i < ira_max_point; i++)
    CLEAR_HARD_REG_SET (used_hard_regs[i]);
  qsort (sorted_allocnos, num, sizeof (ira_allocno_t),
	 allocno_priority_compare_func);
  for (i = 0; i < num; i++)
    {
      a = sorted_allocnos[i];
      COPY_HARD_REG_SET (conflict_hard_regs, ALLOCNO_CONFLICT_HARD_REGS (a));
      for (r = ALLOCNO_LIVE_RANGES (a); r != NULL; r = r->next)
	for (j =  r->start; j <= r->finish; j++)
	  IOR_HARD_REG_SET (conflict_hard_regs, used_hard_regs[j]);
      cover_class = ALLOCNO_COVER_CLASS (a);
      ALLOCNO_ASSIGNED_P (a) = true;
      ALLOCNO_HARD_REGNO (a) = -1;
      if (hard_reg_set_subset_p (reg_class_contents[cover_class],
				 conflict_hard_regs))
	continue;
      mode = ALLOCNO_MODE (a);
#ifdef STACK_REGS
      no_stack_reg_p = ALLOCNO_NO_STACK_REG_P (a);
#endif
      class_size = ira_class_hard_regs_num[cover_class];
      for (j = 0; j < class_size; j++)
	{
	  hard_regno = ira_class_hard_regs[cover_class][j];
#ifdef STACK_REGS
	  if (no_stack_reg_p && FIRST_STACK_REG <= hard_regno
	      && hard_regno <= LAST_STACK_REG)
	    continue;
#endif
	  if (!ira_hard_reg_not_in_set_p (hard_regno, mode, conflict_hard_regs)
	      || (TEST_HARD_REG_BIT
		  (prohibited_class_mode_regs[cover_class][mode], hard_regno)))
	    continue;
	  ALLOCNO_HARD_REGNO (a) = hard_regno;
	  for (r = ALLOCNO_LIVE_RANGES (a); r != NULL; r = r->next)
	    for (k = r->start; k <= r->finish; k++)
	      IOR_HARD_REG_SET (used_hard_regs[k],
				ira_reg_mode_hard_regset[hard_regno][mode]);
	  break;
	}
    }
  ira_free (sorted_allocnos);
  ira_free (used_hard_regs);
  ira_free (allocno_priorities);
  if (internal_flag_ira_verbose > 1 && ira_dump_file != NULL)
    ira_print_disposition (ira_dump_file);
}



/* Entry function doing coloring.  */
void
ira_color (void)
{
  ira_allocno_t a;
  ira_allocno_iterator ai;

  /* Setup updated costs.  */
  FOR_EACH_ALLOCNO (a, ai)
    {
      ALLOCNO_UPDATED_MEMORY_COST (a) = ALLOCNO_MEMORY_COST (a);
      ALLOCNO_UPDATED_COVER_CLASS_COST (a) = ALLOCNO_COVER_CLASS_COST (a);
    }
  if (ira_conflicts_p)
    color ();
  else
    fast_allocation ();
}
