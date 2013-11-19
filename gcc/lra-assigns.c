/* Assign reload pseudos.
   Copyright (C) 2010-2013 Free Software Foundation, Inc.
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
along with GCC; see the file COPYING3.	If not see
<http://www.gnu.org/licenses/>.	 */


/* This file's main objective is to assign hard registers to reload
   pseudos.  It also tries to allocate hard registers to other
   pseudos, but at a lower priority than the reload pseudos.  The pass
   does not transform the RTL.

   We must allocate a hard register to every reload pseudo.  We try to
   increase the chances of finding a viable allocation by assigning
   the pseudos in order of fewest available hard registers first.  If
   we still fail to find a hard register, we spill other (non-reload)
   pseudos in order to make room.

   find_hard_regno_for finds hard registers for allocation without
   spilling.  spill_for does the same with spilling.  Both functions
   use a cost model to determine the most profitable choice of hard
   and spill registers.

   Once we have finished allocating reload pseudos, we also try to
   assign registers to other (non-reload) pseudos.  This is useful if
   hard registers were freed up by the spilling just described.

   We try to assign hard registers by collecting pseudos into threads.
   These threads contain reload and inheritance pseudos that are
   connected by copies (move insns).  Doing this improves the chances
   of pseudos in the thread getting the same hard register and, as a
   result, of allowing some move insns to be deleted.

   When we assign a hard register to a pseudo, we decrease the cost of
   using the same hard register for pseudos that are connected by
   copies.

   If two hard registers have the same frequency-derived cost, we
   prefer hard registers with higher priorities.  The mapping of
   registers to priorities is controlled by the register_priority
   target hook.  For example, x86-64 has a few register priorities:
   hard registers with and without REX prefixes have different
   priorities.  This permits us to generate smaller code as insns
   without REX prefixes are shorter.

   If a few hard registers are still equally good for the assignment,
   we choose the least used hard register.  It is called leveling and
   may be profitable for some targets.

   Only insns with changed allocation pseudos are processed on the
   next constraint pass.

   The pseudo live-ranges are used to find conflicting pseudos.

   For understanding the code, it is important to keep in mind that
   inheritance, split, and reload pseudos created since last
   constraint pass have regno >= lra_constraint_new_regno_start.
   Inheritance and split pseudos created on any pass are in the
   corresponding bitmaps.  Inheritance and split pseudos since the
   last constraint pass have also the corresponding non-negative
   restore_regno.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "hard-reg-set.h"
#include "rtl.h"
#include "rtl-error.h"
#include "tm_p.h"
#include "target.h"
#include "insn-config.h"
#include "recog.h"
#include "output.h"
#include "regs.h"
#include "function.h"
#include "expr.h"
#include "basic-block.h"
#include "except.h"
#include "df.h"
#include "ira.h"
#include "sparseset.h"
#include "lra-int.h"

/* Array containing corresponding values of function
   lra_get_allocno_class.  It is used to speed up the code.  */
static enum reg_class *regno_allocno_class_array;

/* Information about the thread to which a pseudo belongs.  Threads are
   a set of connected reload and inheritance pseudos with the same set of
   available hard registers.  Lone registers belong to their own threads.  */
struct regno_assign_info
{
  /* First/next pseudo of the same thread.  */
  int first, next;
  /* Frequency of the thread (execution frequency of only reload
     pseudos in the thread when the thread contains a reload pseudo).
     Defined only for the first thread pseudo.	*/
  int freq;
};

/* Map regno to the corresponding regno assignment info.  */
static struct regno_assign_info *regno_assign_info;

/* All inherited, subreg or optional pseudos created before last spill
   sub-pass.  Such pseudos are permitted to get memory instead of hard
   regs.  */
static bitmap_head non_reload_pseudos;

/* Process a pseudo copy with execution frequency COPY_FREQ connecting
   REGNO1 and REGNO2 to form threads.  */
static void
process_copy_to_form_thread (int regno1, int regno2, int copy_freq)
{
  int last, regno1_first, regno2_first;

  lra_assert (regno1 >= lra_constraint_new_regno_start
	      && regno2 >= lra_constraint_new_regno_start);
  regno1_first = regno_assign_info[regno1].first;
  regno2_first = regno_assign_info[regno2].first;
  if (regno1_first != regno2_first)
    {
      for (last = regno2_first;
	   regno_assign_info[last].next >= 0;
	   last = regno_assign_info[last].next)
	regno_assign_info[last].first = regno1_first;
      regno_assign_info[last].first = regno1_first;
      regno_assign_info[last].next = regno_assign_info[regno1_first].next;
      regno_assign_info[regno1_first].next = regno2_first;
      regno_assign_info[regno1_first].freq
	+= regno_assign_info[regno2_first].freq;
    }
  regno_assign_info[regno1_first].freq -= 2 * copy_freq;
  lra_assert (regno_assign_info[regno1_first].freq >= 0);
}

/* Initialize REGNO_ASSIGN_INFO and form threads.  */
static void
init_regno_assign_info (void)
{
  int i, regno1, regno2, max_regno = max_reg_num ();
  lra_copy_t cp;

  regno_assign_info = XNEWVEC (struct regno_assign_info, max_regno);
  for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
    {
      regno_assign_info[i].first = i;
      regno_assign_info[i].next = -1;
      regno_assign_info[i].freq = lra_reg_info[i].freq;
    }
  /* Form the threads.	*/
  for (i = 0; (cp = lra_get_copy (i)) != NULL; i++)
    if ((regno1 = cp->regno1) >= lra_constraint_new_regno_start
	&& (regno2 = cp->regno2) >= lra_constraint_new_regno_start
	&& reg_renumber[regno1] < 0 && lra_reg_info[regno1].nrefs != 0
	&& reg_renumber[regno2] < 0 && lra_reg_info[regno2].nrefs != 0
	&& (ira_class_hard_regs_num[regno_allocno_class_array[regno1]]
	    == ira_class_hard_regs_num[regno_allocno_class_array[regno2]]))
      process_copy_to_form_thread (regno1, regno2, cp->freq);
}

/* Free REGNO_ASSIGN_INFO.  */
static void
finish_regno_assign_info (void)
{
  free (regno_assign_info);
}

/* The function is used to sort *reload* and *inheritance* pseudos to
   try to assign them hard registers.  We put pseudos from the same
   thread always nearby.  */
static int
reload_pseudo_compare_func (const void *v1p, const void *v2p)
{
  int r1 = *(const int *) v1p, r2 = *(const int *) v2p;
  enum reg_class cl1 = regno_allocno_class_array[r1];
  enum reg_class cl2 = regno_allocno_class_array[r2];
  int diff;

  lra_assert (r1 >= lra_constraint_new_regno_start
	      && r2 >= lra_constraint_new_regno_start);

  /* Prefer to assign reload registers with smaller classes first to
     guarantee assignment to all reload registers.  */
  if ((diff = (ira_class_hard_regs_num[cl1]
	       - ira_class_hard_regs_num[cl2])) != 0)
    return diff;
  if ((diff
       = (ira_reg_class_max_nregs[cl2][lra_reg_info[r2].biggest_mode]
	  - ira_reg_class_max_nregs[cl1][lra_reg_info[r1].biggest_mode])) != 0
      /* The code below executes rarely as nregs == 1 in most cases.
	 So we should not worry about using faster data structures to
	 check reload pseudos.  */
      && ! bitmap_bit_p (&non_reload_pseudos, r1)
      && ! bitmap_bit_p (&non_reload_pseudos, r2))
    return diff;
  if ((diff = (regno_assign_info[regno_assign_info[r2].first].freq
	       - regno_assign_info[regno_assign_info[r1].first].freq)) != 0)
    return diff;
  /* Allocate bigger pseudos first to avoid register file
     fragmentation.  */
  if ((diff
       = (ira_reg_class_max_nregs[cl2][lra_reg_info[r2].biggest_mode]
	  - ira_reg_class_max_nregs[cl1][lra_reg_info[r1].biggest_mode])) != 0)
    return diff;
  /* Put pseudos from the thread nearby.  */
  if ((diff = regno_assign_info[r1].first - regno_assign_info[r2].first) != 0)
    return diff;
  /* If regs are equally good, sort by their numbers, so that the
     results of qsort leave nothing to chance.	*/
  return r1 - r2;
}

/* The function is used to sort *non-reload* pseudos to try to assign
   them hard registers.	 The order calculation is simpler than in the
   previous function and based on the pseudo frequency usage.  */
static int
pseudo_compare_func (const void *v1p, const void *v2p)
{
  int r1 = *(const int *) v1p, r2 = *(const int *) v2p;
  int diff;

  /* Prefer to assign more frequently used registers first.  */
  if ((diff = lra_reg_info[r2].freq - lra_reg_info[r1].freq) != 0)
    return diff;

  /* If regs are equally good, sort by their numbers, so that the
     results of qsort leave nothing to chance.	*/
  return r1 - r2;
}

/* Arrays of size LRA_LIVE_MAX_POINT mapping a program point to the
   pseudo live ranges with given start point.  We insert only live
   ranges of pseudos interesting for assignment purposes.  They are
   reload pseudos and pseudos assigned to hard registers.  */
static lra_live_range_t *start_point_ranges;

/* Used as a flag that a live range is not inserted in the start point
   chain.  */
static struct lra_live_range not_in_chain_mark;

/* Create and set up START_POINT_RANGES.  */
static void
create_live_range_start_chains (void)
{
  int i, max_regno;
  lra_live_range_t r;

  start_point_ranges = XCNEWVEC (lra_live_range_t, lra_live_max_point);
  max_regno = max_reg_num ();
  for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
    if (i >= lra_constraint_new_regno_start || reg_renumber[i] >= 0)
      {
	for (r = lra_reg_info[i].live_ranges; r != NULL; r = r->next)
	  {
	    r->start_next = start_point_ranges[r->start];
	    start_point_ranges[r->start] = r;
	  }
      }
    else
      {
	for (r = lra_reg_info[i].live_ranges; r != NULL; r = r->next)
	  r->start_next = &not_in_chain_mark;
      }
}

/* Insert live ranges of pseudo REGNO into start chains if they are
   not there yet.  */
static void
insert_in_live_range_start_chain (int regno)
{
  lra_live_range_t r = lra_reg_info[regno].live_ranges;

  if (r->start_next != &not_in_chain_mark)
    return;
  for (; r != NULL; r = r->next)
    {
      r->start_next = start_point_ranges[r->start];
      start_point_ranges[r->start] = r;
    }
}

/* Free START_POINT_RANGES.  */
static void
finish_live_range_start_chains (void)
{
  gcc_assert (start_point_ranges != NULL);
  free (start_point_ranges);
  start_point_ranges = NULL;
}

/* Map: program point -> bitmap of all pseudos living at the point and
   assigned to hard registers.	*/
static bitmap_head *live_hard_reg_pseudos;
static bitmap_obstack live_hard_reg_pseudos_bitmap_obstack;

/* reg_renumber corresponding to pseudos marked in
   live_hard_reg_pseudos.  reg_renumber might be not matched to
   live_hard_reg_pseudos but live_pseudos_reg_renumber always reflects
   live_hard_reg_pseudos.  */
static int *live_pseudos_reg_renumber;

/* Sparseset used to calculate living hard reg pseudos for some program
   point range.	 */
static sparseset live_range_hard_reg_pseudos;

/* Sparseset used to calculate living reload/inheritance pseudos for
   some program point range.  */
static sparseset live_range_reload_inheritance_pseudos;

/* Allocate and initialize the data about living pseudos at program
   points.  */
static void
init_lives (void)
{
  int i, max_regno = max_reg_num ();

  live_range_hard_reg_pseudos = sparseset_alloc (max_regno);
  live_range_reload_inheritance_pseudos = sparseset_alloc (max_regno);
  live_hard_reg_pseudos = XNEWVEC (bitmap_head, lra_live_max_point);
  bitmap_obstack_initialize (&live_hard_reg_pseudos_bitmap_obstack);
  for (i = 0; i < lra_live_max_point; i++)
    bitmap_initialize (&live_hard_reg_pseudos[i],
		       &live_hard_reg_pseudos_bitmap_obstack);
  live_pseudos_reg_renumber = XNEWVEC (int, max_regno);
  for (i = 0; i < max_regno; i++)
    live_pseudos_reg_renumber[i] = -1;
}

/* Free the data about living pseudos at program points.  */
static void
finish_lives (void)
{
  sparseset_free (live_range_hard_reg_pseudos);
  sparseset_free (live_range_reload_inheritance_pseudos);
  free (live_hard_reg_pseudos);
  bitmap_obstack_release (&live_hard_reg_pseudos_bitmap_obstack);
  free (live_pseudos_reg_renumber);
}

/* Update the LIVE_HARD_REG_PSEUDOS and LIVE_PSEUDOS_REG_RENUMBER
   entries for pseudo REGNO.  Assume that the register has been
   spilled if FREE_P, otherwise assume that it has been assigned
   reg_renumber[REGNO] (if >= 0).  We also insert the pseudo live
   ranges in the start chains when it is assumed to be assigned to a
   hard register because we use the chains of pseudos assigned to hard
   registers during allocation.  */
static void
update_lives (int regno, bool free_p)
{
  int p;
  lra_live_range_t r;

  if (reg_renumber[regno] < 0)
    return;
  live_pseudos_reg_renumber[regno] = free_p ? -1 : reg_renumber[regno];
  for (r = lra_reg_info[regno].live_ranges; r != NULL; r = r->next)
    {
      for (p = r->start; p <= r->finish; p++)
	if (free_p)
	  bitmap_clear_bit (&live_hard_reg_pseudos[p], regno);
	else
	  {
	    bitmap_set_bit (&live_hard_reg_pseudos[p], regno);
	    insert_in_live_range_start_chain (regno);
	  }
    }
}

/* Sparseset used to calculate reload pseudos conflicting with a given
   pseudo when we are trying to find a hard register for the given
   pseudo.  */
static sparseset conflict_reload_and_inheritance_pseudos;

/* Map: program point -> bitmap of all reload and inheritance pseudos
   living at the point.	 */
static bitmap_head *live_reload_and_inheritance_pseudos;
static bitmap_obstack live_reload_and_inheritance_pseudos_bitmap_obstack;

/* Allocate and initialize data about living reload pseudos at any
   given program point.  */
static void
init_live_reload_and_inheritance_pseudos (void)
{
  int i, p, max_regno = max_reg_num ();
  lra_live_range_t r;

  conflict_reload_and_inheritance_pseudos = sparseset_alloc (max_regno);
  live_reload_and_inheritance_pseudos = XNEWVEC (bitmap_head, lra_live_max_point);
  bitmap_obstack_initialize (&live_reload_and_inheritance_pseudos_bitmap_obstack);
  for (p = 0; p < lra_live_max_point; p++)
    bitmap_initialize (&live_reload_and_inheritance_pseudos[p],
		       &live_reload_and_inheritance_pseudos_bitmap_obstack);
  for (i = lra_constraint_new_regno_start; i < max_regno; i++)
    {
      for (r = lra_reg_info[i].live_ranges; r != NULL; r = r->next)
	for (p = r->start; p <= r->finish; p++)
	  bitmap_set_bit (&live_reload_and_inheritance_pseudos[p], i);
    }
}

/* Finalize data about living reload pseudos at any given program
   point.  */
static void
finish_live_reload_and_inheritance_pseudos (void)
{
  sparseset_free (conflict_reload_and_inheritance_pseudos);
  free (live_reload_and_inheritance_pseudos);
  bitmap_obstack_release (&live_reload_and_inheritance_pseudos_bitmap_obstack);
}

/* The value used to check that cost of given hard reg is really
   defined currently.  */
static int curr_hard_regno_costs_check = 0;
/* Array used to check that cost of the corresponding hard reg (the
   array element index) is really defined currently.  */
static int hard_regno_costs_check[FIRST_PSEUDO_REGISTER];
/* The current costs of allocation of hard regs.  Defined only if the
   value of the corresponding element of the previous array is equal to
   CURR_HARD_REGNO_COSTS_CHECK.	 */
static int hard_regno_costs[FIRST_PSEUDO_REGISTER];

/* Adjust cost of HARD_REGNO by INCR.  Reset the cost first if it is
   not defined yet.  */
static inline void
adjust_hard_regno_cost (int hard_regno, int incr)
{
  if (hard_regno_costs_check[hard_regno] != curr_hard_regno_costs_check)
    hard_regno_costs[hard_regno] = 0;
  hard_regno_costs_check[hard_regno] = curr_hard_regno_costs_check;
  hard_regno_costs[hard_regno] += incr;
}

/* Try to find a free hard register for pseudo REGNO.  Return the
   hard register on success and set *COST to the cost of using
   that register.  (If several registers have equal cost, the one with
   the highest priority wins.)  Return -1 on failure.

   If TRY_ONLY_HARD_REGNO >= 0, consider only that hard register,
   otherwise consider all hard registers in REGNO's class.  */
static int
find_hard_regno_for (int regno, int *cost, int try_only_hard_regno)
{
  HARD_REG_SET conflict_set;
  int best_cost = INT_MAX, best_priority = INT_MIN, best_usage = INT_MAX;
  lra_live_range_t r;
  int p, i, j, rclass_size, best_hard_regno, priority, hard_regno;
  int hr, conflict_hr, nregs;
  enum machine_mode biggest_mode;
  unsigned int k, conflict_regno;
  int offset, val, biggest_nregs, nregs_diff;
  enum reg_class rclass;
  bitmap_iterator bi;
  bool *rclass_intersect_p;
  HARD_REG_SET impossible_start_hard_regs;

  COPY_HARD_REG_SET (conflict_set, lra_no_alloc_regs);
  rclass = regno_allocno_class_array[regno];
  rclass_intersect_p = ira_reg_classes_intersect_p[rclass];
  curr_hard_regno_costs_check++;
  sparseset_clear (conflict_reload_and_inheritance_pseudos);
  sparseset_clear (live_range_hard_reg_pseudos);
  IOR_HARD_REG_SET (conflict_set, lra_reg_info[regno].conflict_hard_regs);
  biggest_mode = lra_reg_info[regno].biggest_mode;
  for (r = lra_reg_info[regno].live_ranges; r != NULL; r = r->next)
    {
      EXECUTE_IF_SET_IN_BITMAP (&live_hard_reg_pseudos[r->start], 0, k, bi)
	if (rclass_intersect_p[regno_allocno_class_array[k]])
	  sparseset_set_bit (live_range_hard_reg_pseudos, k);
      EXECUTE_IF_SET_IN_BITMAP (&live_reload_and_inheritance_pseudos[r->start],
				0, k, bi)
	if (lra_reg_info[k].preferred_hard_regno1 >= 0
	    && live_pseudos_reg_renumber[k] < 0
	    && rclass_intersect_p[regno_allocno_class_array[k]])
	  sparseset_set_bit (conflict_reload_and_inheritance_pseudos, k);
      for (p = r->start + 1; p <= r->finish; p++)
	{
	  lra_live_range_t r2;

	  for (r2 = start_point_ranges[p];
	       r2 != NULL;
	       r2 = r2->start_next)
	    {
	      if (r2->regno >= lra_constraint_new_regno_start
		  && lra_reg_info[r2->regno].preferred_hard_regno1 >= 0
		  && live_pseudos_reg_renumber[r2->regno] < 0
		  && rclass_intersect_p[regno_allocno_class_array[r2->regno]])
		sparseset_set_bit (conflict_reload_and_inheritance_pseudos,
				   r2->regno);
	      if (live_pseudos_reg_renumber[r2->regno] >= 0
		  && rclass_intersect_p[regno_allocno_class_array[r2->regno]])
		sparseset_set_bit (live_range_hard_reg_pseudos, r2->regno);
	    }
	}
    }
  if ((hard_regno = lra_reg_info[regno].preferred_hard_regno1) >= 0)
    {
      adjust_hard_regno_cost
	(hard_regno, -lra_reg_info[regno].preferred_hard_regno_profit1);
      if ((hard_regno = lra_reg_info[regno].preferred_hard_regno2) >= 0)
	adjust_hard_regno_cost
	  (hard_regno, -lra_reg_info[regno].preferred_hard_regno_profit2);
    }
#ifdef STACK_REGS
  if (lra_reg_info[regno].no_stack_p)
    for (i = FIRST_STACK_REG; i <= LAST_STACK_REG; i++)
      SET_HARD_REG_BIT (conflict_set, i);
#endif
  sparseset_clear_bit (conflict_reload_and_inheritance_pseudos, regno);
  val = lra_reg_info[regno].val;
  offset = lra_reg_info[regno].offset;
  CLEAR_HARD_REG_SET (impossible_start_hard_regs);
  EXECUTE_IF_SET_IN_SPARSESET (live_range_hard_reg_pseudos, conflict_regno)
    if (lra_reg_val_equal_p (conflict_regno, val, offset))
      {
	conflict_hr = live_pseudos_reg_renumber[conflict_regno];
	nregs = (hard_regno_nregs[conflict_hr]
		 [lra_reg_info[conflict_regno].biggest_mode]);
	/* Remember about multi-register pseudos.  For example, 2 hard
	   register pseudos can start on the same hard register but can
	   not start on HR and HR+1/HR-1.  */
	for (hr = conflict_hr + 1;
	     hr < FIRST_PSEUDO_REGISTER && hr < conflict_hr + nregs;
	     hr++)
	  SET_HARD_REG_BIT (impossible_start_hard_regs, hr);
	for (hr = conflict_hr - 1;
	     hr >= 0 && hr + hard_regno_nregs[hr][biggest_mode] > conflict_hr;
	     hr--)
	  SET_HARD_REG_BIT (impossible_start_hard_regs, hr);
      }
    else
      {
	add_to_hard_reg_set (&conflict_set,
			     lra_reg_info[conflict_regno].biggest_mode,
			     live_pseudos_reg_renumber[conflict_regno]);
	if (hard_reg_set_subset_p (reg_class_contents[rclass],
				   conflict_set))
	  return -1;
      }
  EXECUTE_IF_SET_IN_SPARSESET (conflict_reload_and_inheritance_pseudos,
			       conflict_regno)
    if (!lra_reg_val_equal_p (conflict_regno, val, offset))
      {
	lra_assert (live_pseudos_reg_renumber[conflict_regno] < 0);
	if ((hard_regno
	     = lra_reg_info[conflict_regno].preferred_hard_regno1) >= 0)
	  {
	    adjust_hard_regno_cost
	      (hard_regno,
	       lra_reg_info[conflict_regno].preferred_hard_regno_profit1);
	    if ((hard_regno
		 = lra_reg_info[conflict_regno].preferred_hard_regno2) >= 0)
	      adjust_hard_regno_cost
		(hard_regno,
		 lra_reg_info[conflict_regno].preferred_hard_regno_profit2);
	  }
      }
  /* Make sure that all registers in a multi-word pseudo belong to the
     required class.  */
  IOR_COMPL_HARD_REG_SET (conflict_set, reg_class_contents[rclass]);
  lra_assert (rclass != NO_REGS);
  rclass_size = ira_class_hard_regs_num[rclass];
  best_hard_regno = -1;
  hard_regno = ira_class_hard_regs[rclass][0];
  biggest_nregs = hard_regno_nregs[hard_regno][biggest_mode];
  nregs_diff = (biggest_nregs
		- hard_regno_nregs[hard_regno][PSEUDO_REGNO_MODE (regno)]);
  for (i = 0; i < rclass_size; i++)
    {
      if (try_only_hard_regno >= 0)
	hard_regno = try_only_hard_regno;
      else
	hard_regno = ira_class_hard_regs[rclass][i];
      if (! overlaps_hard_reg_set_p (conflict_set,
				     PSEUDO_REGNO_MODE (regno), hard_regno)
	  /* We can not use prohibited_class_mode_regs because it is
	     not defined for all classes.  */
	  && HARD_REGNO_MODE_OK (hard_regno, PSEUDO_REGNO_MODE (regno))
	  && ! TEST_HARD_REG_BIT (impossible_start_hard_regs, hard_regno)
	  && (nregs_diff == 0
	      || (WORDS_BIG_ENDIAN
		  ? (hard_regno - nregs_diff >= 0
		     && TEST_HARD_REG_BIT (reg_class_contents[rclass],
					   hard_regno - nregs_diff))
		  : TEST_HARD_REG_BIT (reg_class_contents[rclass],
				       hard_regno + nregs_diff))))
	{
	  if (hard_regno_costs_check[hard_regno]
	      != curr_hard_regno_costs_check)
	    {
	      hard_regno_costs_check[hard_regno] = curr_hard_regno_costs_check;
	      hard_regno_costs[hard_regno] = 0;
	    }
	  for (j = 0;
	       j < hard_regno_nregs[hard_regno][PSEUDO_REGNO_MODE (regno)];
	       j++)
	    if (! TEST_HARD_REG_BIT (call_used_reg_set, hard_regno + j)
		&& ! df_regs_ever_live_p (hard_regno + j))
	      /* It needs save restore.	 */
	      hard_regno_costs[hard_regno]
		+= 2 * ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb->frequency + 1;
	  priority = targetm.register_priority (hard_regno);
	  if (best_hard_regno < 0 || hard_regno_costs[hard_regno] < best_cost
	      || (hard_regno_costs[hard_regno] == best_cost
		  && (priority > best_priority
		      || (targetm.register_usage_leveling_p ()
			  && priority == best_priority
			  && best_usage > lra_hard_reg_usage[hard_regno]))))
	    {
	      best_hard_regno = hard_regno;
	      best_cost = hard_regno_costs[hard_regno];
	      best_priority = priority;
	      best_usage = lra_hard_reg_usage[hard_regno];
	    }
	}
      if (try_only_hard_regno >= 0)
	break;
    }
  if (best_hard_regno >= 0)
    *cost = best_cost - lra_reg_info[regno].freq;
  return best_hard_regno;
}

/* Current value used for checking elements in
   update_hard_regno_preference_check.	*/
static int curr_update_hard_regno_preference_check;
/* If an element value is equal to the above variable value, then the
   corresponding regno has been processed for preference
   propagation.	 */
static int *update_hard_regno_preference_check;

/* Update the preference for using HARD_REGNO for pseudos that are
   connected directly or indirectly with REGNO.  Apply divisor DIV
   to any preference adjustments.

   The more indirectly a pseudo is connected, the smaller its effect
   should be.  We therefore increase DIV on each "hop".  */
static void
update_hard_regno_preference (int regno, int hard_regno, int div)
{
  int another_regno, cost;
  lra_copy_t cp, next_cp;

  /* Search depth 5 seems to be enough.	 */
  if (div > (1 << 5))
    return;
  for (cp = lra_reg_info[regno].copies; cp != NULL; cp = next_cp)
    {
      if (cp->regno1 == regno)
	{
	  next_cp = cp->regno1_next;
	  another_regno = cp->regno2;
	}
      else if (cp->regno2 == regno)
	{
	  next_cp = cp->regno2_next;
	  another_regno = cp->regno1;
	}
      else
	gcc_unreachable ();
      if (reg_renumber[another_regno] < 0
	  && (update_hard_regno_preference_check[another_regno]
	      != curr_update_hard_regno_preference_check))
	{
	  update_hard_regno_preference_check[another_regno]
	    = curr_update_hard_regno_preference_check;
	  cost = cp->freq < div ? 1 : cp->freq / div;
	  lra_setup_reload_pseudo_preferenced_hard_reg
	    (another_regno, hard_regno, cost);
	  update_hard_regno_preference (another_regno, hard_regno, div * 2);
	}
    }
}

/* Return prefix title for pseudo REGNO.  */
static const char *
pseudo_prefix_title (int regno)
{
  return
    (regno < lra_constraint_new_regno_start ? ""
     : bitmap_bit_p (&lra_inheritance_pseudos, regno) ? "inheritance "
     : bitmap_bit_p (&lra_split_regs, regno) ? "split "
     : bitmap_bit_p (&lra_optional_reload_pseudos, regno) ? "optional reload "
     : bitmap_bit_p (&lra_subreg_reload_pseudos, regno) ? "subreg reload "
     : "reload ");
}

/* Update REG_RENUMBER and other pseudo preferences by assignment of
   HARD_REGNO to pseudo REGNO and print about it if PRINT_P.  */
void
lra_setup_reg_renumber (int regno, int hard_regno, bool print_p)
{
  int i, hr;

  /* We can not just reassign hard register.  */
  lra_assert (hard_regno < 0 || reg_renumber[regno] < 0);
  if ((hr = hard_regno) < 0)
    hr = reg_renumber[regno];
  reg_renumber[regno] = hard_regno;
  lra_assert (hr >= 0);
  for (i = 0; i < hard_regno_nregs[hr][PSEUDO_REGNO_MODE (regno)]; i++)
    if (hard_regno < 0)
      lra_hard_reg_usage[hr + i] -= lra_reg_info[regno].freq;
    else
      lra_hard_reg_usage[hr + i] += lra_reg_info[regno].freq;
  if (print_p && lra_dump_file != NULL)
    fprintf (lra_dump_file, "	   Assign %d to %sr%d (freq=%d)\n",
	     reg_renumber[regno], pseudo_prefix_title (regno),
	     regno, lra_reg_info[regno].freq);
  if (hard_regno >= 0)
    {
      curr_update_hard_regno_preference_check++;
      update_hard_regno_preference (regno, hard_regno, 1);
    }
}

/* Pseudos which occur in insns containing a particular pseudo.  */
static bitmap_head insn_conflict_pseudos;

/* Bitmaps used to contain spill pseudos for given pseudo hard regno
   and best spill pseudos for given pseudo (and best hard regno).  */
static bitmap_head spill_pseudos_bitmap, best_spill_pseudos_bitmap;

/* Current pseudo check for validity of elements in
   TRY_HARD_REG_PSEUDOS.  */
static int curr_pseudo_check;
/* Array used for validity of elements in TRY_HARD_REG_PSEUDOS.	 */
static int try_hard_reg_pseudos_check[FIRST_PSEUDO_REGISTER];
/* Pseudos who hold given hard register at the considered points.  */
static bitmap_head try_hard_reg_pseudos[FIRST_PSEUDO_REGISTER];

/* Set up try_hard_reg_pseudos for given program point P and class
   RCLASS.  Those are pseudos living at P and assigned to a hard
   register of RCLASS.	In other words, those are pseudos which can be
   spilled to assign a hard register of RCLASS to a pseudo living at
   P.  */
static void
setup_try_hard_regno_pseudos (int p, enum reg_class rclass)
{
  int i, hard_regno;
  enum machine_mode mode;
  unsigned int spill_regno;
  bitmap_iterator bi;

  /* Find what pseudos could be spilled.  */
  EXECUTE_IF_SET_IN_BITMAP (&live_hard_reg_pseudos[p], 0, spill_regno, bi)
    {
      mode = PSEUDO_REGNO_MODE (spill_regno);
      hard_regno = live_pseudos_reg_renumber[spill_regno];
      if (overlaps_hard_reg_set_p (reg_class_contents[rclass],
				   mode, hard_regno))
	{
	  for (i = hard_regno_nregs[hard_regno][mode] - 1; i >= 0; i--)
	    {
	      if (try_hard_reg_pseudos_check[hard_regno + i]
		  != curr_pseudo_check)
		{
		  try_hard_reg_pseudos_check[hard_regno + i]
		    = curr_pseudo_check;
		  bitmap_clear (&try_hard_reg_pseudos[hard_regno + i]);
		}
	      bitmap_set_bit (&try_hard_reg_pseudos[hard_regno + i],
			      spill_regno);
	    }
	}
    }
}

/* Assign temporarily HARD_REGNO to pseudo REGNO.  Temporary
   assignment means that we might undo the data change.	 */
static void
assign_temporarily (int regno, int hard_regno)
{
  int p;
  lra_live_range_t r;

  for (r = lra_reg_info[regno].live_ranges; r != NULL; r = r->next)
    {
      for (p = r->start; p <= r->finish; p++)
	if (hard_regno < 0)
	  bitmap_clear_bit (&live_hard_reg_pseudos[p], regno);
	else
	  {
	    bitmap_set_bit (&live_hard_reg_pseudos[p], regno);
	    insert_in_live_range_start_chain (regno);
	  }
    }
  live_pseudos_reg_renumber[regno] = hard_regno;
}

/* Array used for sorting reload pseudos for subsequent allocation
   after spilling some pseudo.	*/
static int *sorted_reload_pseudos;

/* Spill some pseudos for a reload pseudo REGNO and return hard
   register which should be used for pseudo after spilling.  The
   function adds spilled pseudos to SPILLED_PSEUDO_BITMAP.  When we
   choose hard register (and pseudos occupying the hard registers and
   to be spilled), we take into account not only how REGNO will
   benefit from the spills but also how other reload pseudos not yet
   assigned to hard registers benefit from the spills too.  In very
   rare cases, the function can fail and return -1.  */
static int
spill_for (int regno, bitmap spilled_pseudo_bitmap)
{
  int i, j, n, p, hard_regno, best_hard_regno, cost, best_cost, rclass_size;
  int reload_hard_regno, reload_cost;
  enum machine_mode mode;
  enum reg_class rclass;
  unsigned int spill_regno, reload_regno, uid;
  int insn_pseudos_num, best_insn_pseudos_num;
  lra_live_range_t r;
  bitmap_iterator bi;

  rclass = regno_allocno_class_array[regno];
  lra_assert (reg_renumber[regno] < 0 && rclass != NO_REGS);
  bitmap_clear (&insn_conflict_pseudos);
  bitmap_clear (&best_spill_pseudos_bitmap);
  EXECUTE_IF_SET_IN_BITMAP (&lra_reg_info[regno].insn_bitmap, 0, uid, bi)
    {
      struct lra_insn_reg *ir;

      for (ir = lra_get_insn_regs (uid); ir != NULL; ir = ir->next)
	if (ir->regno >= FIRST_PSEUDO_REGISTER)
	  bitmap_set_bit (&insn_conflict_pseudos, ir->regno);
    }
  best_hard_regno = -1;
  best_cost = INT_MAX;
  best_insn_pseudos_num = INT_MAX;
  rclass_size = ira_class_hard_regs_num[rclass];
  mode = PSEUDO_REGNO_MODE (regno);
  /* Invalidate try_hard_reg_pseudos elements.  */
  curr_pseudo_check++;
  for (r = lra_reg_info[regno].live_ranges; r != NULL; r = r->next)
    for (p = r->start; p <= r->finish; p++)
      setup_try_hard_regno_pseudos (p, rclass);
  for (i = 0; i < rclass_size; i++)
    {
      hard_regno = ira_class_hard_regs[rclass][i];
      bitmap_clear (&spill_pseudos_bitmap);
      for (j = hard_regno_nregs[hard_regno][mode] - 1; j >= 0; j--)
	{
	  if (try_hard_reg_pseudos_check[hard_regno + j] != curr_pseudo_check)
	    continue;
	  lra_assert (!bitmap_empty_p (&try_hard_reg_pseudos[hard_regno + j]));
	  bitmap_ior_into (&spill_pseudos_bitmap,
			   &try_hard_reg_pseudos[hard_regno + j]);
	}
      /* Spill pseudos.	 */
      EXECUTE_IF_SET_IN_BITMAP (&spill_pseudos_bitmap, 0, spill_regno, bi)
	if ((int) spill_regno >= lra_constraint_new_regno_start
	    && ! bitmap_bit_p (&lra_inheritance_pseudos, spill_regno)
	    && ! bitmap_bit_p (&lra_split_regs, spill_regno)
	    && ! bitmap_bit_p (&lra_subreg_reload_pseudos, spill_regno)
	    && ! bitmap_bit_p (&lra_optional_reload_pseudos, spill_regno))
	  goto fail;
      insn_pseudos_num = 0;
      if (lra_dump_file != NULL)
	fprintf (lra_dump_file, "	 Trying %d:", hard_regno);
      sparseset_clear (live_range_reload_inheritance_pseudos);
      EXECUTE_IF_SET_IN_BITMAP (&spill_pseudos_bitmap, 0, spill_regno, bi)
	{
	  if (bitmap_bit_p (&insn_conflict_pseudos, spill_regno))
	    insn_pseudos_num++;
	  for (r = lra_reg_info[spill_regno].live_ranges;
	       r != NULL;
	       r = r->next)
	    {
	      for (p = r->start; p <= r->finish; p++)
		{
		  lra_live_range_t r2;

		  for (r2 = start_point_ranges[p];
		       r2 != NULL;
		       r2 = r2->start_next)
		    if (r2->regno >= lra_constraint_new_regno_start)
		      sparseset_set_bit (live_range_reload_inheritance_pseudos,
					 r2->regno);
		}
	    }
	}
      n = 0;
      EXECUTE_IF_SET_IN_SPARSESET (live_range_reload_inheritance_pseudos,
				   reload_regno)
	if ((int) reload_regno != regno
	    && (ira_reg_classes_intersect_p
		[rclass][regno_allocno_class_array[reload_regno]])
	    && live_pseudos_reg_renumber[reload_regno] < 0
	    && find_hard_regno_for (reload_regno, &cost, -1) < 0)
	  sorted_reload_pseudos[n++] = reload_regno;
      EXECUTE_IF_SET_IN_BITMAP (&spill_pseudos_bitmap, 0, spill_regno, bi)
	{
	  update_lives (spill_regno, true);
	  if (lra_dump_file != NULL)
	    fprintf (lra_dump_file, " spill %d(freq=%d)",
		     spill_regno, lra_reg_info[spill_regno].freq);
	}
      hard_regno = find_hard_regno_for (regno, &cost, -1);
      if (hard_regno >= 0)
	{
	  assign_temporarily (regno, hard_regno);
	  qsort (sorted_reload_pseudos, n, sizeof (int),
		 reload_pseudo_compare_func);
	  for (j = 0; j < n; j++)
	    {
	      reload_regno = sorted_reload_pseudos[j];
	      lra_assert (live_pseudos_reg_renumber[reload_regno] < 0);
	      if ((reload_hard_regno
		   = find_hard_regno_for (reload_regno,
					  &reload_cost, -1)) >= 0)
		{
		  if (lra_dump_file != NULL)
		    fprintf (lra_dump_file, " assign %d(cost=%d)",
			     reload_regno, reload_cost);
		  assign_temporarily (reload_regno, reload_hard_regno);
		  cost += reload_cost;
		}
	    }
	  EXECUTE_IF_SET_IN_BITMAP (&spill_pseudos_bitmap, 0, spill_regno, bi)
	    {
	      rtx x;

	      cost += lra_reg_info[spill_regno].freq;
	      if (ira_reg_equiv[spill_regno].memory != NULL
		  || ira_reg_equiv[spill_regno].constant != NULL)
		for (x = ira_reg_equiv[spill_regno].init_insns;
		     x != NULL;
		     x = XEXP (x, 1))
		  cost -= REG_FREQ_FROM_BB (BLOCK_FOR_INSN (XEXP (x, 0)));
	    }
	  if (best_insn_pseudos_num > insn_pseudos_num
	      || (best_insn_pseudos_num == insn_pseudos_num
		  && best_cost > cost))
	    {
	      best_insn_pseudos_num = insn_pseudos_num;
	      best_cost = cost;
	      best_hard_regno = hard_regno;
	      bitmap_copy (&best_spill_pseudos_bitmap, &spill_pseudos_bitmap);
	      if (lra_dump_file != NULL)
		fprintf (lra_dump_file, "	 Now best %d(cost=%d)\n",
			 hard_regno, cost);
	    }
	  assign_temporarily (regno, -1);
	  for (j = 0; j < n; j++)
	    {
	      reload_regno = sorted_reload_pseudos[j];
	      if (live_pseudos_reg_renumber[reload_regno] >= 0)
		assign_temporarily (reload_regno, -1);
	    }
	}
      if (lra_dump_file != NULL)
	fprintf (lra_dump_file, "\n");
      /* Restore the live hard reg pseudo info for spilled pseudos.  */
      EXECUTE_IF_SET_IN_BITMAP (&spill_pseudos_bitmap, 0, spill_regno, bi)
	update_lives (spill_regno, false);
    fail:
      ;
    }
  /* Spill: */
  EXECUTE_IF_SET_IN_BITMAP (&best_spill_pseudos_bitmap, 0, spill_regno, bi)
    {
      if (lra_dump_file != NULL)
	fprintf (lra_dump_file, "      Spill %sr%d(hr=%d, freq=%d) for r%d\n",
		 pseudo_prefix_title (spill_regno),
		 spill_regno, reg_renumber[spill_regno],
		 lra_reg_info[spill_regno].freq, regno);
      update_lives (spill_regno, true);
      lra_setup_reg_renumber (spill_regno, -1, false);
    }
  bitmap_ior_into (spilled_pseudo_bitmap, &best_spill_pseudos_bitmap);
  return best_hard_regno;
}

/* Assign HARD_REGNO to REGNO.	*/
static void
assign_hard_regno (int hard_regno, int regno)
{
  int i;

  lra_assert (hard_regno >= 0);
  lra_setup_reg_renumber (regno, hard_regno, true);
  update_lives (regno, false);
  for (i = 0;
       i < hard_regno_nregs[hard_regno][lra_reg_info[regno].biggest_mode];
       i++)
    df_set_regs_ever_live (hard_regno + i, true);
}

/* Array used for sorting different pseudos.  */
static int *sorted_pseudos;

/* The constraints pass is allowed to create equivalences between
   pseudos that make the current allocation "incorrect" (in the sense
   that pseudos are assigned to hard registers from their own conflict
   sets).  The global variable lra_risky_transformations_p says
   whether this might have happened.

   Process pseudos assigned to hard registers (less frequently used
   first), spill if a conflict is found, and mark the spilled pseudos
   in SPILLED_PSEUDO_BITMAP.  Set up LIVE_HARD_REG_PSEUDOS from
   pseudos, assigned to hard registers.	 */
static void
setup_live_pseudos_and_spill_after_risky_transforms (bitmap
						     spilled_pseudo_bitmap)
{
  int p, i, j, n, regno, hard_regno;
  unsigned int k, conflict_regno;
  int val, offset;
  HARD_REG_SET conflict_set;
  enum machine_mode mode;
  lra_live_range_t r;
  bitmap_iterator bi;
  int max_regno = max_reg_num ();

  if (! lra_risky_transformations_p)
    {
      for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
	if (reg_renumber[i] >= 0 && lra_reg_info[i].nrefs > 0)
	  update_lives (i, false);
      return;
    }
  for (n = 0, i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
    if (reg_renumber[i] >= 0 && lra_reg_info[i].nrefs > 0)
      sorted_pseudos[n++] = i;
  qsort (sorted_pseudos, n, sizeof (int), pseudo_compare_func);
  for (i = n - 1; i >= 0; i--)
    {
      regno = sorted_pseudos[i];
      hard_regno = reg_renumber[regno];
      lra_assert (hard_regno >= 0);
      mode = lra_reg_info[regno].biggest_mode;
      sparseset_clear (live_range_hard_reg_pseudos);
      for (r = lra_reg_info[regno].live_ranges; r != NULL; r = r->next)
	{
	  EXECUTE_IF_SET_IN_BITMAP (&live_hard_reg_pseudos[r->start], 0, k, bi)
	    sparseset_set_bit (live_range_hard_reg_pseudos, k);
	  for (p = r->start + 1; p <= r->finish; p++)
	    {
	      lra_live_range_t r2;

	      for (r2 = start_point_ranges[p];
		   r2 != NULL;
		   r2 = r2->start_next)
		if (live_pseudos_reg_renumber[r2->regno] >= 0)
		  sparseset_set_bit (live_range_hard_reg_pseudos, r2->regno);
	    }
	}
      COPY_HARD_REG_SET (conflict_set, lra_no_alloc_regs);
      IOR_HARD_REG_SET (conflict_set, lra_reg_info[regno].conflict_hard_regs);
      val = lra_reg_info[regno].val;
      offset = lra_reg_info[regno].offset;
      EXECUTE_IF_SET_IN_SPARSESET (live_range_hard_reg_pseudos, conflict_regno)
	if (!lra_reg_val_equal_p (conflict_regno, val, offset)
	    /* If it is multi-register pseudos they should start on
	       the same hard register.	*/
	    || hard_regno != reg_renumber[conflict_regno])
	  add_to_hard_reg_set (&conflict_set,
			       lra_reg_info[conflict_regno].biggest_mode,
			       reg_renumber[conflict_regno]);
      if (! overlaps_hard_reg_set_p (conflict_set, mode, hard_regno))
	{
	  update_lives (regno, false);
	  continue;
	}
      bitmap_set_bit (spilled_pseudo_bitmap, regno);
      for (j = 0;
	   j < hard_regno_nregs[hard_regno][PSEUDO_REGNO_MODE (regno)];
	   j++)
	lra_hard_reg_usage[hard_regno + j] -= lra_reg_info[regno].freq;
      reg_renumber[regno] = -1;
      if (lra_dump_file != NULL)
	fprintf (lra_dump_file, "    Spill r%d after risky transformations\n",
		 regno);
    }
}

/* Improve allocation by assigning the same hard regno of inheritance
   pseudos to the connected pseudos.  We need this because inheritance
   pseudos are allocated after reload pseudos in the thread and when
   we assign a hard register to a reload pseudo we don't know yet that
   the connected inheritance pseudos can get the same hard register.
   Add pseudos with changed allocation to bitmap CHANGED_PSEUDOS.  */
static void
improve_inheritance (bitmap changed_pseudos)
{
  unsigned int k;
  int regno, another_regno, hard_regno, another_hard_regno, cost, i, n;
  lra_copy_t cp, next_cp;
  bitmap_iterator bi;

  if (lra_inheritance_iter > LRA_MAX_INHERITANCE_PASSES)
    return;
  n = 0;
  EXECUTE_IF_SET_IN_BITMAP (&lra_inheritance_pseudos, 0, k, bi)
    if (reg_renumber[k] >= 0 && lra_reg_info[k].nrefs != 0)
      sorted_pseudos[n++] = k;
  qsort (sorted_pseudos, n, sizeof (int), pseudo_compare_func);
  for (i = 0; i < n; i++)
    {
      regno = sorted_pseudos[i];
      hard_regno = reg_renumber[regno];
      lra_assert (hard_regno >= 0);
      for (cp = lra_reg_info[regno].copies; cp != NULL; cp = next_cp)
	{
	  if (cp->regno1 == regno)
	    {
	      next_cp = cp->regno1_next;
	      another_regno = cp->regno2;
	    }
	  else if (cp->regno2 == regno)
	    {
	      next_cp = cp->regno2_next;
	      another_regno = cp->regno1;
	    }
	  else
	    gcc_unreachable ();
	  /* Don't change reload pseudo allocation.  It might have
	     this allocation for a purpose and changing it can result
	     in LRA cycling.  */
	  if ((another_regno < lra_constraint_new_regno_start
	       || bitmap_bit_p (&lra_inheritance_pseudos, another_regno))
	      && (another_hard_regno = reg_renumber[another_regno]) >= 0
	      && another_hard_regno != hard_regno)
	    {
	      if (lra_dump_file != NULL)
		fprintf
		  (lra_dump_file,
		   "	Improving inheritance for %d(%d) and %d(%d)...\n",
		   regno, hard_regno, another_regno, another_hard_regno);
	      update_lives (another_regno, true);
	      lra_setup_reg_renumber (another_regno, -1, false);
	      if (hard_regno
		  == find_hard_regno_for (another_regno, &cost, hard_regno))
		assign_hard_regno (hard_regno, another_regno);
	      else
		assign_hard_regno (another_hard_regno, another_regno);
	      bitmap_set_bit (changed_pseudos, another_regno);
	    }
	}
    }
}


/* Bitmap finally containing all pseudos spilled on this assignment
   pass.  */
static bitmap_head all_spilled_pseudos;
/* All pseudos whose allocation was changed.  */
static bitmap_head changed_pseudo_bitmap;

/* Assign hard registers to reload pseudos and other pseudos.  */
static void
assign_by_spills (void)
{
  int i, n, nfails, iter, regno, hard_regno, cost, restore_regno;
  rtx insn;
  basic_block bb;
  bitmap_head changed_insns, do_not_assign_nonreload_pseudos;
  unsigned int u;
  bitmap_iterator bi;
  bool reload_p;
  int max_regno = max_reg_num ();

  for (n = 0, i = lra_constraint_new_regno_start; i < max_regno; i++)
    if (reg_renumber[i] < 0 && lra_reg_info[i].nrefs != 0
	&& regno_allocno_class_array[i] != NO_REGS)
      sorted_pseudos[n++] = i;
  bitmap_initialize (&insn_conflict_pseudos, &reg_obstack);
  bitmap_initialize (&spill_pseudos_bitmap, &reg_obstack);
  bitmap_initialize (&best_spill_pseudos_bitmap, &reg_obstack);
  update_hard_regno_preference_check = XCNEWVEC (int, max_regno);
  curr_update_hard_regno_preference_check = 0;
  memset (try_hard_reg_pseudos_check, 0, sizeof (try_hard_reg_pseudos_check));
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    bitmap_initialize (&try_hard_reg_pseudos[i], &reg_obstack);
  curr_pseudo_check = 0;
  bitmap_initialize (&changed_insns, &reg_obstack);
  bitmap_initialize (&non_reload_pseudos, &reg_obstack);
  bitmap_ior (&non_reload_pseudos, &lra_inheritance_pseudos, &lra_split_regs);
  bitmap_ior_into (&non_reload_pseudos, &lra_subreg_reload_pseudos);
  bitmap_ior_into (&non_reload_pseudos, &lra_optional_reload_pseudos);
  for (iter = 0; iter <= 1; iter++)
    {
      qsort (sorted_pseudos, n, sizeof (int), reload_pseudo_compare_func);
      nfails = 0;
      for (i = 0; i < n; i++)
	{
	  regno = sorted_pseudos[i];
	  if (lra_dump_file != NULL)
	    fprintf (lra_dump_file, "	 Assigning to %d "
		     "(cl=%s, orig=%d, freq=%d, tfirst=%d, tfreq=%d)...\n",
		     regno, reg_class_names[regno_allocno_class_array[regno]],
		     ORIGINAL_REGNO (regno_reg_rtx[regno]),
		     lra_reg_info[regno].freq, regno_assign_info[regno].first,
		     regno_assign_info[regno_assign_info[regno].first].freq);
	  hard_regno = find_hard_regno_for (regno, &cost, -1);
	  reload_p = ! bitmap_bit_p (&non_reload_pseudos, regno);
	  if (hard_regno < 0 && reload_p)
	    hard_regno = spill_for (regno, &all_spilled_pseudos);
	  if (hard_regno < 0)
	    {
	      if (reload_p)
		sorted_pseudos[nfails++] = regno;
	    }
	  else
	    {
	      /* This register might have been spilled by the previous
		 pass.  Indicate that it is no longer spilled.  */
	      bitmap_clear_bit (&all_spilled_pseudos, regno);
	      assign_hard_regno (hard_regno, regno);
	      if (! reload_p)
		/* As non-reload pseudo assignment is changed we
		   should reconsider insns referring for the
		   pseudo.  */
		bitmap_set_bit (&changed_pseudo_bitmap, regno);
	    }
	}
      if (nfails == 0)
	break;
      if (iter > 0)
	{
	  /* We did not assign hard regs to reload pseudos after two
	     iteration.  It means something is wrong with asm insn
	     constraints.  Report it.  */
	  bool asm_p = false;
	  bitmap_head failed_reload_insns;

	  bitmap_initialize (&failed_reload_insns, &reg_obstack);
	  for (i = 0; i < nfails; i++)
	    {
	      regno = sorted_pseudos[i];
	      bitmap_ior_into (&failed_reload_insns,
			       &lra_reg_info[regno].insn_bitmap);
	      /* Assign an arbitrary hard register of regno class to
		 avoid further trouble with the asm insns.  */
	      bitmap_clear_bit (&all_spilled_pseudos, regno);
	      assign_hard_regno
		(ira_class_hard_regs[regno_allocno_class_array[regno]][0],
		 regno);
	    }
	  EXECUTE_IF_SET_IN_BITMAP (&failed_reload_insns, 0, u, bi)
	    {
	      insn = lra_insn_recog_data[u]->insn;
	      if (asm_noperands (PATTERN (insn)) >= 0)
		{
		  asm_p = true;
		  error_for_asm (insn,
				 "%<asm%> operand has impossible constraints");
		  /* Avoid further trouble with this insn.
		     For asm goto, instead of fixing up all the edges
		     just clear the template and clear input operands
		     (asm goto doesn't have any output operands).  */
		  if (JUMP_P (insn))
		    {
		      rtx asm_op = extract_asm_operands (PATTERN (insn));
		      ASM_OPERANDS_TEMPLATE (asm_op) = ggc_strdup ("");
		      ASM_OPERANDS_INPUT_VEC (asm_op) = rtvec_alloc (0);
		      ASM_OPERANDS_INPUT_CONSTRAINT_VEC (asm_op) = rtvec_alloc (0);
		      lra_update_insn_regno_info (insn);
		    }
		  else
		    {
		      PATTERN (insn) = gen_rtx_USE (VOIDmode, const0_rtx);
		      lra_set_insn_deleted (insn);
		    }
		}
	    }
	  lra_assert (asm_p);
	  break;
	}
      /* This is a very rare event.  We can not assign a hard
	 register to reload pseudo because the hard register was
	 assigned to another reload pseudo on a previous
	 assignment pass.  For x86 example, on the 1st pass we
	 assigned CX (although another hard register could be used
	 for this) to reload pseudo in an insn, on the 2nd pass we
	 need CX (and only this) hard register for a new reload
	 pseudo in the same insn.  */
      if (lra_dump_file != NULL)
	fprintf (lra_dump_file, "  2nd iter for reload pseudo assignments:\n");
      for (i = 0; i < nfails; i++)
	{
	  if (lra_dump_file != NULL)
	    fprintf (lra_dump_file, "	 Reload r%d assignment failure\n",
		     sorted_pseudos[i]);
	  bitmap_ior_into (&changed_insns,
			   &lra_reg_info[sorted_pseudos[i]].insn_bitmap);
	}

      /* FIXME: Look up the changed insns in the cached LRA insn data using
	 an EXECUTE_IF_SET_IN_BITMAP over changed_insns.  */
      FOR_EACH_BB (bb)
	FOR_BB_INSNS (bb, insn)
	if (bitmap_bit_p (&changed_insns, INSN_UID (insn)))
	  {
	    lra_insn_recog_data_t data;
	    struct lra_insn_reg *r;

	    data = lra_get_insn_recog_data (insn);
	    for (r = data->regs; r != NULL; r = r->next)
	      {
		regno = r->regno;
		/* A reload pseudo did not get a hard register on the
		   first iteration because of the conflict with
		   another reload pseudos in the same insn.  So we
		   consider only reload pseudos assigned to hard
		   registers.  We shall exclude inheritance pseudos as
		   they can occur in original insns (not reload ones).
		   We can omit the check for split pseudos because
		   they occur only in move insns containing non-reload
		   pseudos.  */
		if (regno < lra_constraint_new_regno_start
		    || bitmap_bit_p (&lra_inheritance_pseudos, regno)
		    || reg_renumber[regno] < 0)
		  continue;
		sorted_pseudos[nfails++] = regno;
		if (lra_dump_file != NULL)
		  fprintf (lra_dump_file,
			   "	  Spill reload r%d(hr=%d, freq=%d)\n",
			   regno, reg_renumber[regno],
			   lra_reg_info[regno].freq);
		update_lives (regno, true);
		lra_setup_reg_renumber (regno, -1, false);
	      }
	  }
      n = nfails;
    }
  improve_inheritance (&changed_pseudo_bitmap);
  bitmap_clear (&non_reload_pseudos);
  bitmap_clear (&changed_insns);
  if (! lra_simple_p)
    {
      /* We should not assign to original pseudos of inheritance
	 pseudos or split pseudos if any its inheritance pseudo did
	 not get hard register or any its split pseudo was not split
	 because undo inheritance/split pass will extend live range of
	 such inheritance or split pseudos.  */
      bitmap_initialize (&do_not_assign_nonreload_pseudos, &reg_obstack);
      EXECUTE_IF_SET_IN_BITMAP (&lra_inheritance_pseudos, 0, u, bi)
	if ((restore_regno = lra_reg_info[u].restore_regno) >= 0
	    && reg_renumber[u] < 0
	    && bitmap_bit_p (&lra_inheritance_pseudos, u))
	  bitmap_set_bit (&do_not_assign_nonreload_pseudos, restore_regno);
      EXECUTE_IF_SET_IN_BITMAP (&lra_split_regs, 0, u, bi)
	if ((restore_regno = lra_reg_info[u].restore_regno) >= 0
	    && reg_renumber[u] >= 0)
	  bitmap_set_bit (&do_not_assign_nonreload_pseudos, restore_regno);
      for (n = 0, i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
	if (((i < lra_constraint_new_regno_start
	      && ! bitmap_bit_p (&do_not_assign_nonreload_pseudos, i))
	     || (bitmap_bit_p (&lra_inheritance_pseudos, i)
		 && lra_reg_info[i].restore_regno >= 0)
	     || (bitmap_bit_p (&lra_split_regs, i)
		 && lra_reg_info[i].restore_regno >= 0)
	     || bitmap_bit_p (&lra_subreg_reload_pseudos, i)
	     || bitmap_bit_p (&lra_optional_reload_pseudos, i))
	    && reg_renumber[i] < 0 && lra_reg_info[i].nrefs != 0
	    && regno_allocno_class_array[i] != NO_REGS)
	  sorted_pseudos[n++] = i;
      bitmap_clear (&do_not_assign_nonreload_pseudos);
      if (n != 0 && lra_dump_file != NULL)
	fprintf (lra_dump_file, "  Reassigning non-reload pseudos\n");
      qsort (sorted_pseudos, n, sizeof (int), pseudo_compare_func);
      for (i = 0; i < n; i++)
	{
	  regno = sorted_pseudos[i];
	  hard_regno = find_hard_regno_for (regno, &cost, -1);
	  if (hard_regno >= 0)
	    {
	      assign_hard_regno (hard_regno, regno);
	      /* We change allocation for non-reload pseudo on this
		 iteration -- mark the pseudo for invalidation of used
		 alternatives of insns containing the pseudo.  */
	      bitmap_set_bit (&changed_pseudo_bitmap, regno);
	    }
	}
    }
  free (update_hard_regno_preference_check);
  bitmap_clear (&best_spill_pseudos_bitmap);
  bitmap_clear (&spill_pseudos_bitmap);
  bitmap_clear (&insn_conflict_pseudos);
}


/* Entry function to assign hard registers to new reload pseudos
   starting with LRA_CONSTRAINT_NEW_REGNO_START (by possible spilling
   of old pseudos) and possibly to the old pseudos.  The function adds
   what insns to process for the next constraint pass.	Those are all
   insns who contains non-reload and non-inheritance pseudos with
   changed allocation.

   Return true if we did not spill any non-reload and non-inheritance
   pseudos.  */
bool
lra_assign (void)
{
  int i;
  unsigned int u;
  bitmap_iterator bi;
  bitmap_head insns_to_process;
  bool no_spills_p;
  int max_regno = max_reg_num ();

  timevar_push (TV_LRA_ASSIGN);
  init_lives ();
  sorted_pseudos = XNEWVEC (int, max_regno);
  sorted_reload_pseudos = XNEWVEC (int, max_regno);
  regno_allocno_class_array = XNEWVEC (enum reg_class, max_regno);
  for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
    regno_allocno_class_array[i] = lra_get_allocno_class (i);
  init_regno_assign_info ();
  bitmap_initialize (&all_spilled_pseudos, &reg_obstack);
  create_live_range_start_chains ();
  setup_live_pseudos_and_spill_after_risky_transforms (&all_spilled_pseudos);
#ifdef ENABLE_CHECKING
  for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
    if (lra_reg_info[i].nrefs != 0 && reg_renumber[i] >= 0
	&& lra_reg_info[i].call_p
	&& overlaps_hard_reg_set_p (call_used_reg_set,
				    PSEUDO_REGNO_MODE (i), reg_renumber[i]))
      gcc_unreachable ();
#endif
  /* Setup insns to process on the next constraint pass.  */
  bitmap_initialize (&changed_pseudo_bitmap, &reg_obstack);
  init_live_reload_and_inheritance_pseudos ();
  assign_by_spills ();
  finish_live_reload_and_inheritance_pseudos ();
  bitmap_ior_into (&changed_pseudo_bitmap, &all_spilled_pseudos);
  no_spills_p = true;
  EXECUTE_IF_SET_IN_BITMAP (&all_spilled_pseudos, 0, u, bi)
    /* We ignore spilled pseudos created on last inheritance pass
       because they will be removed.  */
    if (lra_reg_info[u].restore_regno < 0)
      {
	no_spills_p = false;
	break;
      }
  finish_live_range_start_chains ();
  bitmap_clear (&all_spilled_pseudos);
  bitmap_initialize (&insns_to_process, &reg_obstack);
  EXECUTE_IF_SET_IN_BITMAP (&changed_pseudo_bitmap, 0, u, bi)
    bitmap_ior_into (&insns_to_process, &lra_reg_info[u].insn_bitmap);
  bitmap_clear (&changed_pseudo_bitmap);
  EXECUTE_IF_SET_IN_BITMAP (&insns_to_process, 0, u, bi)
    {
      lra_push_insn_by_uid (u);
      /* Invalidate alternatives for insn should be processed.	*/
      lra_set_used_insn_alternative_by_uid (u, -1);
    }
  bitmap_clear (&insns_to_process);
  finish_regno_assign_info ();
  free (regno_allocno_class_array);
  free (sorted_pseudos);
  free (sorted_reload_pseudos);
  finish_lives ();
  timevar_pop (TV_LRA_ASSIGN);
  return no_spills_p;
}
