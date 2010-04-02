/* Coalesce SSA_NAMES together for the out-of-ssa pass.
   Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "flags.h"
#include "diagnostic.h"
#include "bitmap.h"
#include "tree-flow.h"
#include "hashtab.h"
#include "tree-dump.h"
#include "tree-ssa-live.h"
#include "toplev.h"


/* This set of routines implements a coalesce_list.  This is an object which
   is used to track pairs of ssa_names which are desirable to coalesce
   together to avoid copies.  Costs are associated with each pair, and when
   all desired information has been collected, the object can be used to
   order the pairs for processing.  */

/* This structure defines a pair entry.  */

typedef struct coalesce_pair
{
  int first_element;
  int second_element;
  int cost;
} * coalesce_pair_p;
typedef const struct coalesce_pair *const_coalesce_pair_p;

typedef struct cost_one_pair_d
{
  int first_element;
  int second_element;
  struct cost_one_pair_d *next;
} * cost_one_pair_p;

/* This structure maintains the list of coalesce pairs.  */

typedef struct coalesce_list_d
{
  htab_t list;			/* Hash table.  */
  coalesce_pair_p *sorted;	/* List when sorted.  */
  int num_sorted;		/* Number in the sorted list.  */
  cost_one_pair_p cost_one_list;/* Single use coalesces with cost 1.  */
} *coalesce_list_p;

#define NO_BEST_COALESCE	-1
#define MUST_COALESCE_COST	INT_MAX


/* Return cost of execution of copy instruction with FREQUENCY.  */

static inline int
coalesce_cost (int frequency, bool optimize_for_size)
{
  /* Base costs on BB frequencies bounded by 1.  */
  int cost = frequency;

  if (!cost)
    cost = 1;

  if (optimize_for_size)
    cost = 1;

  return cost;
}


/* Return the cost of executing a copy instruction in basic block BB.  */

static inline int
coalesce_cost_bb (basic_block bb)
{
  return coalesce_cost (bb->frequency, optimize_bb_for_size_p (bb));
}


/* Return the cost of executing a copy instruction on edge E.  */

static inline int
coalesce_cost_edge (edge e)
{
  int mult = 1;

  /* Inserting copy on critical edge costs more than inserting it elsewhere.  */
  if (EDGE_CRITICAL_P (e))
    mult = 2;
  if (e->flags & EDGE_ABNORMAL)
    return MUST_COALESCE_COST;
  if (e->flags & EDGE_EH)
    {
      edge e2;
      edge_iterator ei;
      FOR_EACH_EDGE (e2, ei, e->dest->preds)
	if (e2 != e)
	  {
	    /* Putting code on EH edge that leads to BB
	       with multiple predecestors imply splitting of
	       edge too.  */
	    if (mult < 2)
	      mult = 2;
	    /* If there are multiple EH predecestors, we
	       also copy EH regions and produce separate
	       landing pad.  This is expensive.  */
	    if (e2->flags & EDGE_EH)
	      {
	        mult = 5;
	        break;
	      }
	  }
    }

  return coalesce_cost (EDGE_FREQUENCY (e),
			optimize_edge_for_size_p (e)) * mult;
}


/* Retrieve a pair to coalesce from the cost_one_list in CL.  Returns the
   2 elements via P1 and P2.  1 is returned by the function if there is a pair,
   NO_BEST_COALESCE is returned if there aren't any.  */

static inline int
pop_cost_one_pair (coalesce_list_p cl, int *p1, int *p2)
{
  cost_one_pair_p ptr;

  ptr = cl->cost_one_list;
  if (!ptr)
    return NO_BEST_COALESCE;

  *p1 = ptr->first_element;
  *p2 = ptr->second_element;
  cl->cost_one_list = ptr->next;

  free (ptr);

  return 1;
}

/* Retrieve the most expensive remaining pair to coalesce from CL.  Returns the
   2 elements via P1 and P2.  Their calculated cost is returned by the function.
   NO_BEST_COALESCE is returned if the coalesce list is empty.  */

static inline int
pop_best_coalesce (coalesce_list_p cl, int *p1, int *p2)
{
  coalesce_pair_p node;
  int ret;

  if (cl->sorted == NULL)
    return pop_cost_one_pair (cl, p1, p2);

  if (cl->num_sorted == 0)
    return pop_cost_one_pair (cl, p1, p2);

  node = cl->sorted[--(cl->num_sorted)];
  *p1 = node->first_element;
  *p2 = node->second_element;
  ret = node->cost;
  free (node);

  return ret;
}


#define COALESCE_HASH_FN(R1, R2) ((R2) * ((R2) - 1) / 2 + (R1))

/* Hash function for coalesce list.  Calculate hash for PAIR.   */

static unsigned int
coalesce_pair_map_hash (const void *pair)
{
  hashval_t a = (hashval_t)(((const_coalesce_pair_p)pair)->first_element);
  hashval_t b = (hashval_t)(((const_coalesce_pair_p)pair)->second_element);

  return COALESCE_HASH_FN (a,b);
}


/* Equality function for coalesce list hash table.  Compare PAIR1 and PAIR2,
   returning TRUE if the two pairs are equivalent.  */

static int
coalesce_pair_map_eq (const void *pair1, const void *pair2)
{
  const_coalesce_pair_p const p1 = (const_coalesce_pair_p) pair1;
  const_coalesce_pair_p const p2 = (const_coalesce_pair_p) pair2;

  return (p1->first_element == p2->first_element
	  && p1->second_element == p2->second_element);
}


/* Create a new empty coalesce list object and return it.  */

static inline coalesce_list_p
create_coalesce_list (void)
{
  coalesce_list_p list;
  unsigned size = num_ssa_names * 3;

  if (size < 40)
    size = 40;

  list = (coalesce_list_p) xmalloc (sizeof (struct coalesce_list_d));
  list->list = htab_create (size, coalesce_pair_map_hash,
  			    coalesce_pair_map_eq, NULL);
  list->sorted = NULL;
  list->num_sorted = 0;
  list->cost_one_list = NULL;
  return list;
}


/* Delete coalesce list CL.  */

static inline void
delete_coalesce_list (coalesce_list_p cl)
{
  gcc_assert (cl->cost_one_list == NULL);
  htab_delete (cl->list);
  if (cl->sorted)
    free (cl->sorted);
  gcc_assert (cl->num_sorted == 0);
  free (cl);
}


/* Find a matching coalesce pair object in CL for the pair P1 and P2.  If
   one isn't found, return NULL if CREATE is false, otherwise create a new
   coalesce pair object and return it.  */

static coalesce_pair_p
find_coalesce_pair (coalesce_list_p cl, int p1, int p2, bool create)
{
  struct coalesce_pair p;
  void **slot;
  unsigned int hash;

  /* Normalize so that p1 is the smaller value.  */
  if (p2 < p1)
    {
      p.first_element = p2;
      p.second_element = p1;
    }
  else
    {
      p.first_element = p1;
      p.second_element = p2;
    }

  hash = coalesce_pair_map_hash (&p);
  slot = htab_find_slot_with_hash (cl->list, &p, hash,
				   create ? INSERT : NO_INSERT);
  if (!slot)
    return NULL;

  if (!*slot)
    {
      struct coalesce_pair * pair = XNEW (struct coalesce_pair);
      gcc_assert (cl->sorted == NULL);
      pair->first_element = p.first_element;
      pair->second_element = p.second_element;
      pair->cost = 0;
      *slot = (void *)pair;
    }

  return (struct coalesce_pair *) *slot;
}

static inline void
add_cost_one_coalesce (coalesce_list_p cl, int p1, int p2)
{
  cost_one_pair_p pair;

  pair = XNEW (struct cost_one_pair_d);
  pair->first_element = p1;
  pair->second_element = p2;
  pair->next = cl->cost_one_list;
  cl->cost_one_list = pair;
}


/* Add a coalesce between P1 and P2 in list CL with a cost of VALUE.  */

static inline void
add_coalesce (coalesce_list_p cl, int p1, int p2, int value)
{
  coalesce_pair_p node;

  gcc_assert (cl->sorted == NULL);
  if (p1 == p2)
    return;

  node = find_coalesce_pair (cl, p1, p2, true);

  /* Once the value is at least MUST_COALESCE_COST - 1, leave it that way.  */
  if (node->cost < MUST_COALESCE_COST - 1)
    {
      if (value < MUST_COALESCE_COST - 1)
	node->cost += value;
      else
	node->cost = value;
    }
}


/* Comparison function to allow qsort to sort P1 and P2 in Ascending order.  */

static int
compare_pairs (const void *p1, const void *p2)
{
  const_coalesce_pair_p const *const pp1 = (const_coalesce_pair_p const *) p1;
  const_coalesce_pair_p const *const pp2 = (const_coalesce_pair_p const *) p2;
  int result;

  result = (* pp1)->cost - (* pp2)->cost;
  /* Since qsort does not guarantee stability we use the elements
     as a secondary key.  This provides us with independence from
     the host's implementation of the sorting algorithm.  */
  if (result == 0)
    {
      result = (* pp2)->first_element - (* pp1)->first_element;
      if (result == 0)
	result = (* pp2)->second_element - (* pp1)->second_element;
    }

  return result;
}


/* Return the number of unique coalesce pairs in CL.  */

static inline int
num_coalesce_pairs (coalesce_list_p cl)
{
  return htab_elements (cl->list);
}


/* Iterator over hash table pairs.  */
typedef struct
{
  htab_iterator hti;
} coalesce_pair_iterator;


/* Return first partition pair from list CL, initializing iterator ITER.  */

static inline coalesce_pair_p
first_coalesce_pair (coalesce_list_p cl, coalesce_pair_iterator *iter)
{
  coalesce_pair_p pair;

  pair = (coalesce_pair_p) first_htab_element (&(iter->hti), cl->list);
  return pair;
}


/* Return TRUE if there are no more partitions in for ITER to process.  */

static inline bool
end_coalesce_pair_p (coalesce_pair_iterator *iter)
{
  return end_htab_p (&(iter->hti));
}


/* Return the next partition pair to be visited by ITER.  */

static inline coalesce_pair_p
next_coalesce_pair (coalesce_pair_iterator *iter)
{
  coalesce_pair_p pair;

  pair = (coalesce_pair_p) next_htab_element (&(iter->hti));
  return pair;
}


/* Iterate over CL using ITER, returning values in PAIR.  */

#define FOR_EACH_PARTITION_PAIR(PAIR, ITER, CL)		\
  for ((PAIR) = first_coalesce_pair ((CL), &(ITER));	\
       !end_coalesce_pair_p (&(ITER));			\
       (PAIR) = next_coalesce_pair (&(ITER)))


/* Prepare CL for removal of preferred pairs.  When finished they are sorted
   in order from most important coalesce to least important.  */

static void
sort_coalesce_list (coalesce_list_p cl)
{
  unsigned x, num;
  coalesce_pair_p p;
  coalesce_pair_iterator ppi;

  gcc_assert (cl->sorted == NULL);

  num = num_coalesce_pairs (cl);
  cl->num_sorted = num;
  if (num == 0)
    return;

  /* Allocate a vector for the pair pointers.  */
  cl->sorted = XNEWVEC (coalesce_pair_p, num);

  /* Populate the vector with pointers to the pairs.  */
  x = 0;
  FOR_EACH_PARTITION_PAIR (p, ppi, cl)
    cl->sorted[x++] = p;
  gcc_assert (x == num);

  /* Already sorted.  */
  if (num == 1)
    return;

  /* If there are only 2, just pick swap them if the order isn't correct.  */
  if (num == 2)
    {
      if (cl->sorted[0]->cost > cl->sorted[1]->cost)
        {
	  p = cl->sorted[0];
	  cl->sorted[0] = cl->sorted[1];
	  cl->sorted[1] = p;
	}
      return;
    }

  /* Only call qsort if there are more than 2 items.  */
  if (num > 2)
      qsort (cl->sorted, num, sizeof (coalesce_pair_p), compare_pairs);
}


/* Send debug info for coalesce list CL to file F.  */

static void
dump_coalesce_list (FILE *f, coalesce_list_p cl)
{
  coalesce_pair_p node;
  coalesce_pair_iterator ppi;
  int x;
  tree var;

  if (cl->sorted == NULL)
    {
      fprintf (f, "Coalesce List:\n");
      FOR_EACH_PARTITION_PAIR (node, ppi, cl)
        {
	  tree var1 = ssa_name (node->first_element);
	  tree var2 = ssa_name (node->second_element);
	  print_generic_expr (f, var1, TDF_SLIM);
	  fprintf (f, " <-> ");
	  print_generic_expr (f, var2, TDF_SLIM);
	  fprintf (f, "  (%1d), ", node->cost);
	  fprintf (f, "\n");
	}
    }
  else
    {
      fprintf (f, "Sorted Coalesce list:\n");
      for (x = cl->num_sorted - 1 ; x >=0; x--)
        {
	  node = cl->sorted[x];
	  fprintf (f, "(%d) ", node->cost);
	  var = ssa_name (node->first_element);
	  print_generic_expr (f, var, TDF_SLIM);
	  fprintf (f, " <-> ");
	  var = ssa_name (node->second_element);
	  print_generic_expr (f, var, TDF_SLIM);
	  fprintf (f, "\n");
	}
    }
}


/* This represents a conflict graph.  Implemented as an array of bitmaps.
   A full matrix is used for conflicts rather than just upper triangular form.
   this make sit much simpler and faster to perform conflict merges.  */

typedef struct ssa_conflicts_d
{
  unsigned size;
  bitmap *conflicts;
} * ssa_conflicts_p;


/* Return an empty new conflict graph for SIZE elements.  */

static inline ssa_conflicts_p
ssa_conflicts_new (unsigned size)
{
  ssa_conflicts_p ptr;

  ptr = XNEW (struct ssa_conflicts_d);
  ptr->conflicts = XCNEWVEC (bitmap, size);
  ptr->size = size;
  return ptr;
}


/* Free storage for conflict graph PTR.  */

static inline void
ssa_conflicts_delete (ssa_conflicts_p ptr)
{
  unsigned x;
  for (x = 0; x < ptr->size; x++)
    if (ptr->conflicts[x])
      BITMAP_FREE (ptr->conflicts[x]);

  free (ptr->conflicts);
  free (ptr);
}


/* Test if elements X and Y conflict in graph PTR.  */

static inline bool
ssa_conflicts_test_p (ssa_conflicts_p ptr, unsigned x, unsigned y)
{
  bitmap b;

#ifdef ENABLE_CHECKING
  gcc_assert (x < ptr->size);
  gcc_assert (y < ptr->size);
  gcc_assert (x != y);
#endif

  b = ptr->conflicts[x];
  if (b)
    /* Avoid the lookup if Y has no conflicts.  */
    return ptr->conflicts[y] ? bitmap_bit_p (b, y) : false;
  else
    return false;
}


/* Add a conflict with Y to the bitmap for X in graph PTR.  */

static inline void
ssa_conflicts_add_one (ssa_conflicts_p ptr, unsigned x, unsigned y)
{
  /* If there are no conflicts yet, allocate the bitmap and set bit.  */
  if (!ptr->conflicts[x])
    ptr->conflicts[x] = BITMAP_ALLOC (NULL);
  bitmap_set_bit (ptr->conflicts[x], y);
}


/* Add conflicts between X and Y in graph PTR.  */

static inline void
ssa_conflicts_add (ssa_conflicts_p ptr, unsigned x, unsigned y)
{
#ifdef ENABLE_CHECKING
  gcc_assert (x < ptr->size);
  gcc_assert (y < ptr->size);
  gcc_assert (x != y);
#endif
  ssa_conflicts_add_one (ptr, x, y);
  ssa_conflicts_add_one (ptr, y, x);
}


/* Merge all Y's conflict into X in graph PTR.  */

static inline void
ssa_conflicts_merge (ssa_conflicts_p ptr, unsigned x, unsigned y)
{
  unsigned z;
  bitmap_iterator bi;

  gcc_assert (x != y);
  if (!(ptr->conflicts[y]))
    return;

  /* Add a conflict between X and every one Y has.  If the bitmap doesn't
     exist, then it has already been coalesced, and we don't need to add a
     conflict.  */
  EXECUTE_IF_SET_IN_BITMAP (ptr->conflicts[y], 0, z, bi)
    if (ptr->conflicts[z])
      bitmap_set_bit (ptr->conflicts[z], x);

  if (ptr->conflicts[x])
    {
      /* If X has conflicts, add Y's to X.  */
      bitmap_ior_into (ptr->conflicts[x], ptr->conflicts[y]);
      BITMAP_FREE (ptr->conflicts[y]);
    }
  else
    {
      /* If X has no conflicts, simply use Y's.  */
      ptr->conflicts[x] = ptr->conflicts[y];
      ptr->conflicts[y] = NULL;
    }
}


/* Dump a conflicts graph.  */

static void
ssa_conflicts_dump (FILE *file, ssa_conflicts_p ptr)
{
  unsigned x;

  fprintf (file, "\nConflict graph:\n");

  for (x = 0; x < ptr->size; x++)
    if (ptr->conflicts[x])
      {
	fprintf (dump_file, "%d: ", x);
	dump_bitmap (file, ptr->conflicts[x]);
      }
}


/* This structure is used to efficiently record the current status of live
   SSA_NAMES when building a conflict graph.
   LIVE_BASE_VAR has a bit set for each base variable which has at least one
   ssa version live.
   LIVE_BASE_PARTITIONS is an array of bitmaps using the basevar table as an
   index, and is used to track what partitions of each base variable are
   live.  This makes it easy to add conflicts between just live partitions
   with the same base variable.
   The values in LIVE_BASE_PARTITIONS are only valid if the base variable is
   marked as being live.  This delays clearing of these bitmaps until
   they are actually needed again.  */

typedef struct live_track_d
{
  bitmap live_base_var;		/* Indicates if a basevar is live.  */
  bitmap *live_base_partitions;	/* Live partitions for each basevar.  */
  var_map map;			/* Var_map being used for partition mapping.  */
} * live_track_p;


/* This routine will create a new live track structure based on the partitions
   in MAP.  */

static live_track_p
new_live_track (var_map map)
{
  live_track_p ptr;
  int lim, x;

  /* Make sure there is a partition view in place.  */
  gcc_assert (map->partition_to_base_index != NULL);

  ptr = (live_track_p) xmalloc (sizeof (struct live_track_d));
  ptr->map = map;
  lim = num_basevars (map);
  ptr->live_base_partitions = (bitmap *) xmalloc(sizeof (bitmap *) * lim);
  ptr->live_base_var = BITMAP_ALLOC (NULL);
  for (x = 0; x < lim; x++)
    ptr->live_base_partitions[x] = BITMAP_ALLOC (NULL);
  return ptr;
}


/* This routine will free the memory associated with PTR.  */

static void
delete_live_track (live_track_p ptr)
{
  int x, lim;

  lim = num_basevars (ptr->map);
  for (x = 0; x < lim; x++)
    BITMAP_FREE (ptr->live_base_partitions[x]);
  BITMAP_FREE (ptr->live_base_var);
  free (ptr->live_base_partitions);
  free (ptr);
}


/* This function will remove PARTITION from the live list in PTR.  */

static inline void
live_track_remove_partition (live_track_p ptr, int partition)
{
  int root;

  root = basevar_index (ptr->map, partition);
  bitmap_clear_bit (ptr->live_base_partitions[root], partition);
  /* If the element list is empty, make the base variable not live either.  */
  if (bitmap_empty_p (ptr->live_base_partitions[root]))
    bitmap_clear_bit (ptr->live_base_var, root);
}


/* This function will adds PARTITION to the live list in PTR.  */

static inline void
live_track_add_partition (live_track_p ptr, int partition)
{
  int root;

  root = basevar_index (ptr->map, partition);
  /* If this base var wasn't live before, it is now.  Clear the element list
     since it was delayed until needed.  */
  if (!bitmap_bit_p (ptr->live_base_var, root))
    {
      bitmap_set_bit (ptr->live_base_var, root);
      bitmap_clear (ptr->live_base_partitions[root]);
    }
  bitmap_set_bit (ptr->live_base_partitions[root], partition);

}


/* Clear the live bit for VAR in PTR.  */

static inline void
live_track_clear_var (live_track_p ptr, tree var)
{
  int p;

  p = var_to_partition (ptr->map, var);
  if (p != NO_PARTITION)
    live_track_remove_partition (ptr, p);
}


/* Return TRUE if VAR is live in PTR.  */

static inline bool
live_track_live_p (live_track_p ptr, tree var)
{
  int p, root;

  p = var_to_partition (ptr->map, var);
  if (p != NO_PARTITION)
    {
      root = basevar_index (ptr->map, p);
      if (bitmap_bit_p (ptr->live_base_var, root))
	return bitmap_bit_p (ptr->live_base_partitions[root], p);
    }
  return false;
}


/* This routine will add USE to PTR.  USE will be marked as live in both the
   ssa live map and the live bitmap for the root of USE.  */

static inline void
live_track_process_use (live_track_p ptr, tree use)
{
  int p;

  p = var_to_partition (ptr->map, use);
  if (p == NO_PARTITION)
    return;

  /* Mark as live in the appropriate live list.  */
  live_track_add_partition (ptr, p);
}


/* This routine will process a DEF in PTR.  DEF will be removed from the live
   lists, and if there are any other live partitions with the same base
   variable, conflicts will be added to GRAPH.  */

static inline void
live_track_process_def (live_track_p ptr, tree def, ssa_conflicts_p graph)
{
  int p, root;
  bitmap b;
  unsigned x;
  bitmap_iterator bi;

  p = var_to_partition (ptr->map, def);
  if (p == NO_PARTITION)
    return;

  /* Clear the liveness bit.  */
  live_track_remove_partition (ptr, p);

  /* If the bitmap isn't empty now, conflicts need to be added.  */
  root = basevar_index (ptr->map, p);
  if (bitmap_bit_p (ptr->live_base_var, root))
    {
      b = ptr->live_base_partitions[root];
      EXECUTE_IF_SET_IN_BITMAP (b, 0, x, bi)
        ssa_conflicts_add (graph, p, x);
    }
}


/* Initialize PTR with the partitions set in INIT.  */

static inline void
live_track_init (live_track_p ptr, bitmap init)
{
  unsigned p;
  bitmap_iterator bi;

  /* Mark all live on exit partitions.  */
  EXECUTE_IF_SET_IN_BITMAP (init, 0, p, bi)
    live_track_add_partition (ptr, p);
}


/* This routine will clear all live partitions in PTR.   */

static inline void
live_track_clear_base_vars (live_track_p ptr)
{
  /* Simply clear the live base list.  Anything marked as live in the element
     lists will be cleared later if/when the base variable ever comes alive
     again.  */
  bitmap_clear (ptr->live_base_var);
}


/* Build a conflict graph based on LIVEINFO.  Any partitions which are in the
   partition view of the var_map liveinfo is based on get entries in the
   conflict graph.  Only conflicts between ssa_name partitions with the same
   base variable are added.  */

static ssa_conflicts_p
build_ssa_conflict_graph (tree_live_info_p liveinfo)
{
  ssa_conflicts_p graph;
  var_map map;
  basic_block bb;
  ssa_op_iter iter;
  live_track_p live;

  map = live_var_map (liveinfo);
  graph = ssa_conflicts_new (num_var_partitions (map));

  live = new_live_track (map);

  FOR_EACH_BB (bb)
    {
      gimple_stmt_iterator gsi;

      /* Start with live on exit temporaries.  */
      live_track_init (live, live_on_exit (liveinfo, bb));

      for (gsi = gsi_last_bb (bb); !gsi_end_p (gsi); gsi_prev (&gsi))
        {
	  tree var;
	  gimple stmt = gsi_stmt (gsi);

	  /* A copy between 2 partitions does not introduce an interference
	     by itself.  If they did, you would never be able to coalesce
	     two things which are copied.  If the two variables really do
	     conflict, they will conflict elsewhere in the program.

	     This is handled by simply removing the SRC of the copy from the
	     live list, and processing the stmt normally.  */
	  if (is_gimple_assign (stmt))
	    {
	      tree lhs = gimple_assign_lhs (stmt);
	      tree rhs1 = gimple_assign_rhs1 (stmt);
	      if (gimple_assign_copy_p (stmt)
                  && TREE_CODE (lhs) == SSA_NAME
                  && TREE_CODE (rhs1) == SSA_NAME)
		live_track_clear_var (live, rhs1);
	    }
	  else if (is_gimple_debug (stmt))
	    continue;

	  FOR_EACH_SSA_TREE_OPERAND (var, stmt, iter, SSA_OP_DEF)
	    live_track_process_def (live, var, graph);

	  FOR_EACH_SSA_TREE_OPERAND (var, stmt, iter, SSA_OP_USE)
	    live_track_process_use (live, var);
	}

      /* If result of a PHI is unused, looping over the statements will not
	 record any conflicts since the def was never live.  Since the PHI node
	 is going to be translated out of SSA form, it will insert a copy.
	 There must be a conflict recorded between the result of the PHI and
	 any variables that are live.  Otherwise the out-of-ssa translation
	 may create incorrect code.  */
      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple phi = gsi_stmt (gsi);
	  tree result = PHI_RESULT (phi);
	  if (live_track_live_p (live, result))
	    live_track_process_def (live, result, graph);
	}

     live_track_clear_base_vars (live);
    }

  delete_live_track (live);
  return graph;
}


/* Shortcut routine to print messages to file F of the form:
   "STR1 EXPR1 STR2 EXPR2 STR3."  */

static inline void
print_exprs (FILE *f, const char *str1, tree expr1, const char *str2,
	     tree expr2, const char *str3)
{
  fprintf (f, "%s", str1);
  print_generic_expr (f, expr1, TDF_SLIM);
  fprintf (f, "%s", str2);
  print_generic_expr (f, expr2, TDF_SLIM);
  fprintf (f, "%s", str3);
}


/* Called if a coalesce across and abnormal edge cannot be performed.  PHI is
   the phi node at fault, I is the argument index at fault.  A message is
   printed and compilation is then terminated.  */

static inline void
abnormal_corrupt (gimple phi, int i)
{
  edge e = gimple_phi_arg_edge (phi, i);
  tree res = gimple_phi_result (phi);
  tree arg = gimple_phi_arg_def (phi, i);

  fprintf (stderr, " Corrupt SSA across abnormal edge BB%d->BB%d\n",
	   e->src->index, e->dest->index);
  fprintf (stderr, "Argument %d (", i);
  print_generic_expr (stderr, arg, TDF_SLIM);
  if (TREE_CODE (arg) != SSA_NAME)
    fprintf (stderr, ") is not an SSA_NAME.\n");
  else
    {
      gcc_assert (SSA_NAME_VAR (res) != SSA_NAME_VAR (arg));
      fprintf (stderr, ") does not have the same base variable as the result ");
      print_generic_stmt (stderr, res, TDF_SLIM);
    }

  internal_error ("SSA corruption");
}


/* Print a failure to coalesce a MUST_COALESCE pair X and Y.  */

static inline void
fail_abnormal_edge_coalesce (int x, int y)
{
  fprintf (stderr, "\nUnable to coalesce ssa_names %d and %d",x, y);
  fprintf (stderr, " which are marked as MUST COALESCE.\n");
  print_generic_expr (stderr, ssa_name (x), TDF_SLIM);
  fprintf (stderr, " and  ");
  print_generic_stmt (stderr, ssa_name (y), TDF_SLIM);

  internal_error ("SSA corruption");
}


/* This function creates a var_map for the current function as well as creating
   a coalesce list for use later in the out of ssa process.  */

static var_map
create_outofssa_var_map (coalesce_list_p cl, bitmap used_in_copy)
{
  gimple_stmt_iterator gsi;
  basic_block bb;
  tree var;
  gimple stmt;
  tree first;
  var_map map;
  ssa_op_iter iter;
  int v1, v2, cost;
  unsigned i;

#ifdef ENABLE_CHECKING
  bitmap used_in_real_ops;
  bitmap used_in_virtual_ops;

  used_in_real_ops = BITMAP_ALLOC (NULL);
  used_in_virtual_ops = BITMAP_ALLOC (NULL);
#endif

  map = init_var_map (num_ssa_names);

  FOR_EACH_BB (bb)
    {
      tree arg;

      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple phi = gsi_stmt (gsi);
	  size_t i;
	  int ver;
	  tree res;
	  bool saw_copy = false;

	  res = gimple_phi_result (phi);
	  ver = SSA_NAME_VERSION (res);
	  register_ssa_partition (map, res);

	  /* Register ssa_names and coalesces between the args and the result
	     of all PHI.  */
	  for (i = 0; i < gimple_phi_num_args (phi); i++)
	    {
	      edge e = gimple_phi_arg_edge (phi, i);
	      arg = PHI_ARG_DEF (phi, i);
	      if (TREE_CODE (arg) == SSA_NAME)
		register_ssa_partition (map, arg);
	      if (TREE_CODE (arg) == SSA_NAME
		  && SSA_NAME_VAR (arg) == SSA_NAME_VAR (res))
	        {
		  saw_copy = true;
		  bitmap_set_bit (used_in_copy, SSA_NAME_VERSION (arg));
		  if ((e->flags & EDGE_ABNORMAL) == 0)
		    {
		      int cost = coalesce_cost_edge (e);
		      if (cost == 1 && has_single_use (arg))
		        add_cost_one_coalesce (cl, ver, SSA_NAME_VERSION (arg));
		      else
			add_coalesce (cl, ver, SSA_NAME_VERSION (arg), cost);
		    }
		}
	      else
	        if (e->flags & EDGE_ABNORMAL)
		  abnormal_corrupt (phi, i);
	    }
	  if (saw_copy)
	    bitmap_set_bit (used_in_copy, ver);
	}

      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
        {
	  stmt = gsi_stmt (gsi);

	  if (is_gimple_debug (stmt))
	    continue;

	  /* Register USE and DEF operands in each statement.  */
	  FOR_EACH_SSA_TREE_OPERAND (var, stmt, iter, (SSA_OP_DEF|SSA_OP_USE))
	    register_ssa_partition (map, var);

	  /* Check for copy coalesces.  */
	  switch (gimple_code (stmt))
	    {
	    case GIMPLE_ASSIGN:
	      {
		tree lhs = gimple_assign_lhs (stmt);
		tree rhs1 = gimple_assign_rhs1 (stmt);

		if (gimple_assign_copy_p (stmt)
                    && TREE_CODE (lhs) == SSA_NAME
		    && TREE_CODE (rhs1) == SSA_NAME
		    && SSA_NAME_VAR (lhs) == SSA_NAME_VAR (rhs1))
		  {
		    v1 = SSA_NAME_VERSION (lhs);
		    v2 = SSA_NAME_VERSION (rhs1);
		    cost = coalesce_cost_bb (bb);
		    add_coalesce (cl, v1, v2, cost);
		    bitmap_set_bit (used_in_copy, v1);
		    bitmap_set_bit (used_in_copy, v2);
		  }
	      }
	      break;

	    case GIMPLE_ASM:
	      {
		unsigned long noutputs, i;
		unsigned long ninputs;
		tree *outputs, link;
		noutputs = gimple_asm_noutputs (stmt);
		ninputs = gimple_asm_ninputs (stmt);
		outputs = (tree *) alloca (noutputs * sizeof (tree));
		for (i = 0; i < noutputs; ++i) {
		  link = gimple_asm_output_op (stmt, i);
		  outputs[i] = TREE_VALUE (link);
                }

		for (i = 0; i < ninputs; ++i)
		  {
                    const char *constraint;
                    tree input;
		    char *end;
		    unsigned long match;

		    link = gimple_asm_input_op (stmt, i);
		    constraint
		      = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (link)));
		    input = TREE_VALUE (link);

		    if (TREE_CODE (input) != SSA_NAME)
		      continue;

		    match = strtoul (constraint, &end, 10);
		    if (match >= noutputs || end == constraint)
		      continue;

		    if (TREE_CODE (outputs[match]) != SSA_NAME)
		      continue;

		    v1 = SSA_NAME_VERSION (outputs[match]);
		    v2 = SSA_NAME_VERSION (input);

		    if (SSA_NAME_VAR (outputs[match]) == SSA_NAME_VAR (input))
		      {
			cost = coalesce_cost (REG_BR_PROB_BASE,
					      optimize_bb_for_size_p (bb));
			add_coalesce (cl, v1, v2, cost);
			bitmap_set_bit (used_in_copy, v1);
			bitmap_set_bit (used_in_copy, v2);
		      }
		  }
		break;
	      }

	    default:
	      break;
	    }

#ifdef ENABLE_CHECKING
	  /* Mark real uses and defs.  */
	  FOR_EACH_SSA_TREE_OPERAND (var, stmt, iter, (SSA_OP_DEF|SSA_OP_USE))
	    bitmap_set_bit (used_in_real_ops, DECL_UID (SSA_NAME_VAR (var)));

	  /* Validate that virtual ops don't get used in funny ways.  */
	  if (gimple_vuse (stmt))
	    bitmap_set_bit (used_in_virtual_ops,
			    DECL_UID (SSA_NAME_VAR (gimple_vuse (stmt))));
#endif /* ENABLE_CHECKING */
	}
    }

  /* Now process result decls and live on entry variables for entry into
     the coalesce list.  */
  first = NULL_TREE;
  for (i = 1; i < num_ssa_names; i++)
    {
      var = ssa_name (i);
      if (var != NULL_TREE && is_gimple_reg (var))
        {
	  /* Add coalesces between all the result decls.  */
	  if (TREE_CODE (SSA_NAME_VAR (var)) == RESULT_DECL)
	    {
	      if (first == NULL_TREE)
		first = var;
	      else
		{
		  gcc_assert (SSA_NAME_VAR (var) == SSA_NAME_VAR (first));
		  v1 = SSA_NAME_VERSION (first);
		  v2 = SSA_NAME_VERSION (var);
		  bitmap_set_bit (used_in_copy, v1);
		  bitmap_set_bit (used_in_copy, v2);
		  cost = coalesce_cost_bb (EXIT_BLOCK_PTR);
		  add_coalesce (cl, v1, v2, cost);
		}
	    }
	  /* Mark any default_def variables as being in the coalesce list
	     since they will have to be coalesced with the base variable.  If
	     not marked as present, they won't be in the coalesce view. */
	  if (gimple_default_def (cfun, SSA_NAME_VAR (var)) == var
	      && !has_zero_uses (var))
	    bitmap_set_bit (used_in_copy, SSA_NAME_VERSION (var));
	}
    }

#if defined ENABLE_CHECKING
  {
    unsigned i;
    bitmap both = BITMAP_ALLOC (NULL);
    bitmap_and (both, used_in_real_ops, used_in_virtual_ops);
    if (!bitmap_empty_p (both))
      {
	bitmap_iterator bi;

	EXECUTE_IF_SET_IN_BITMAP (both, 0, i, bi)
	  fprintf (stderr, "Variable %s used in real and virtual operands\n",
		   get_name (referenced_var (i)));
	internal_error ("SSA corruption");
      }

    BITMAP_FREE (used_in_real_ops);
    BITMAP_FREE (used_in_virtual_ops);
    BITMAP_FREE (both);
  }
#endif

  return map;
}


/* Attempt to coalesce ssa versions X and Y together using the partition
   mapping in MAP and checking conflicts in GRAPH.  Output any debug info to
   DEBUG, if it is nun-NULL.  */

static inline bool
attempt_coalesce (var_map map, ssa_conflicts_p graph, int x, int y,
		  FILE *debug)
{
  int z;
  tree var1, var2;
  int p1, p2;

  p1 = var_to_partition (map, ssa_name (x));
  p2 = var_to_partition (map, ssa_name (y));

  if (debug)
    {
      fprintf (debug, "(%d)", x);
      print_generic_expr (debug, partition_to_var (map, p1), TDF_SLIM);
      fprintf (debug, " & (%d)", y);
      print_generic_expr (debug, partition_to_var (map, p2), TDF_SLIM);
    }

  if (p1 == p2)
    {
      if (debug)
	fprintf (debug, ": Already Coalesced.\n");
      return true;
    }

  if (debug)
    fprintf (debug, " [map: %d, %d] ", p1, p2);


  if (!ssa_conflicts_test_p (graph, p1, p2))
    {
      var1 = partition_to_var (map, p1);
      var2 = partition_to_var (map, p2);
      z = var_union (map, var1, var2);
      if (z == NO_PARTITION)
	{
	  if (debug)
	    fprintf (debug, ": Unable to perform partition union.\n");
	  return false;
	}

      /* z is the new combined partition.  Remove the other partition from
	 the list, and merge the conflicts.  */
      if (z == p1)
	ssa_conflicts_merge (graph, p1, p2);
      else
	ssa_conflicts_merge (graph, p2, p1);

      if (debug)
	fprintf (debug, ": Success -> %d\n", z);
      return true;
    }

  if (debug)
    fprintf (debug, ": Fail due to conflict\n");

  return false;
}


/* Attempt to Coalesce partitions in MAP which occur in the list CL using
   GRAPH.  Debug output is sent to DEBUG if it is non-NULL.  */

static void
coalesce_partitions (var_map map, ssa_conflicts_p graph, coalesce_list_p cl,
		     FILE *debug)
{
  int x = 0, y = 0;
  tree var1, var2;
  int cost;
  basic_block bb;
  edge e;
  edge_iterator ei;

  /* First, coalesce all the copies across abnormal edges.  These are not placed
     in the coalesce list because they do not need to be sorted, and simply
     consume extra memory/compilation time in large programs.  */

  FOR_EACH_BB (bb)
    {
      FOR_EACH_EDGE (e, ei, bb->preds)
	if (e->flags & EDGE_ABNORMAL)
	  {
	    gimple_stmt_iterator gsi;
	    for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
		 gsi_next (&gsi))
	      {
		gimple phi = gsi_stmt (gsi);
		tree res = PHI_RESULT (phi);
	        tree arg = PHI_ARG_DEF (phi, e->dest_idx);
		int v1 = SSA_NAME_VERSION (res);
		int v2 = SSA_NAME_VERSION (arg);

		if (SSA_NAME_VAR (arg) != SSA_NAME_VAR (res))
		  abnormal_corrupt (phi, e->dest_idx);

		if (debug)
		  fprintf (debug, "Abnormal coalesce: ");

		if (!attempt_coalesce (map, graph, v1, v2, debug))
		  fail_abnormal_edge_coalesce (v1, v2);
	      }
	  }
    }

  /* Now process the items in the coalesce list.  */

  while ((cost = pop_best_coalesce (cl, &x, &y)) != NO_BEST_COALESCE)
    {
      var1 = ssa_name (x);
      var2 = ssa_name (y);

      /* Assert the coalesces have the same base variable.  */
      gcc_assert (SSA_NAME_VAR (var1) == SSA_NAME_VAR (var2));

      if (debug)
	fprintf (debug, "Coalesce list: ");
      attempt_coalesce (map, graph, x, y, debug);
    }
}

/* Returns a hash code for P.  */

static hashval_t
hash_ssa_name_by_var (const void *p)
{
  const_tree n = (const_tree) p;
  return (hashval_t) htab_hash_pointer (SSA_NAME_VAR (n));
}

/* Returns nonzero if P1 and P2 are equal.  */

static int
eq_ssa_name_by_var (const void *p1, const void *p2)
{
  const_tree n1 = (const_tree) p1;
  const_tree n2 = (const_tree) p2;
  return SSA_NAME_VAR (n1) == SSA_NAME_VAR (n2);
}

/* Reduce the number of copies by coalescing variables in the function.  Return
   a partition map with the resulting coalesces.  */

extern var_map
coalesce_ssa_name (void)
{
  tree_live_info_p liveinfo;
  ssa_conflicts_p graph;
  coalesce_list_p cl;
  bitmap used_in_copies = BITMAP_ALLOC (NULL);
  var_map map;
  unsigned int i;
  static htab_t ssa_name_hash;

  cl = create_coalesce_list ();
  map = create_outofssa_var_map (cl, used_in_copies);

  /* We need to coalesce all names originating same SSA_NAME_VAR
     so debug info remains undisturbed.  */
  if (!optimize)
    {
      ssa_name_hash = htab_create (10, hash_ssa_name_by_var,
      				   eq_ssa_name_by_var, NULL);
      for (i = 1; i < num_ssa_names; i++)
	{
	  tree a = ssa_name (i);

	  if (a
	      && SSA_NAME_VAR (a)
	      && !DECL_ARTIFICIAL (SSA_NAME_VAR (a))
	      && (!has_zero_uses (a) || !SSA_NAME_IS_DEFAULT_DEF (a)))
	    {
	      tree *slot = (tree *) htab_find_slot (ssa_name_hash, a, INSERT);

	      if (!*slot)
		*slot = a;
	      else
		{
		  add_coalesce (cl, SSA_NAME_VERSION (a), SSA_NAME_VERSION (*slot),
				MUST_COALESCE_COST - 1);
		  bitmap_set_bit (used_in_copies, SSA_NAME_VERSION (a));
		  bitmap_set_bit (used_in_copies, SSA_NAME_VERSION (*slot));
		}
	    }
	}
      htab_delete (ssa_name_hash);
    }
  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_var_map (dump_file, map);

  /* Don't calculate live ranges for variables not in the coalesce list.  */
  partition_view_bitmap (map, used_in_copies, true);
  BITMAP_FREE (used_in_copies);

  if (num_var_partitions (map) < 1)
    {
      delete_coalesce_list (cl);
      return map;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_var_map (dump_file, map);

  liveinfo = calculate_live_ranges (map);

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_live_info (dump_file, liveinfo, LIVEDUMP_ENTRY);

  /* Build a conflict graph.  */
  graph = build_ssa_conflict_graph (liveinfo);
  delete_tree_live_info (liveinfo);
  if (dump_file && (dump_flags & TDF_DETAILS))
    ssa_conflicts_dump (dump_file, graph);

  sort_coalesce_list (cl);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nAfter sorting:\n");
      dump_coalesce_list (dump_file, cl);
    }

  /* First, coalesce all live on entry variables to their base variable.
     This will ensure the first use is coming from the correct location.  */

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_var_map (dump_file, map);

  /* Now coalesce everything in the list.  */
  coalesce_partitions (map, graph, cl,
		       ((dump_flags & TDF_DETAILS) ? dump_file
						   : NULL));

  delete_coalesce_list (cl);
  ssa_conflicts_delete (graph);

  return map;
}
