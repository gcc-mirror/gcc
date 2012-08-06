/* IRA allocation based on graph coloring.
   Copyright (C) 2006, 2007, 2008, 2009, 2010, 2011, 2012
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
#include "diagnostic-core.h"
#include "reload.h"
#include "params.h"
#include "df.h"
#include "ira-int.h"

typedef struct allocno_hard_regs *allocno_hard_regs_t;

/* The structure contains information about hard registers can be
   assigned to allocnos.  Usually it is allocno profitable hard
   registers but in some cases this set can be a bit different.  Major
   reason of the difference is a requirement to use hard register sets
   that form a tree or a forest (set of trees), i.e. hard register set
   of a node should contain hard register sets of its subnodes.  */
struct allocno_hard_regs
{
  /* Hard registers can be assigned to an allocno.  */
  HARD_REG_SET set;
  /* Overall (spilling) cost of all allocnos with given register
     set.  */
  HOST_WIDEST_INT cost;
};

typedef struct allocno_hard_regs_node *allocno_hard_regs_node_t;

/* A node representing allocno hard registers.  Such nodes form a
   forest (set of trees).  Each subnode of given node in the forest
   refers for hard register set (usually allocno profitable hard
   register set) which is a subset of one referred from given
   node.  */
struct allocno_hard_regs_node
{
  /* Set up number of the node in preorder traversing of the forest.  */
  int preorder_num;
  /* Used for different calculation like finding conflict size of an
     allocno.  */
  int check;
  /* Used for calculation of conflict size of an allocno.  The
     conflict size of the allocno is maximal number of given allocno
     hard registers needed for allocation of the conflicting allocnos.
     Given allocno is trivially colored if this number plus the number
     of hard registers needed for given allocno is not greater than
     the number of given allocno hard register set.  */
  int conflict_size;
  /* The number of hard registers given by member hard_regs.  */
  int hard_regs_num;
  /* The following member is used to form the final forest.  */
  bool used_p;
  /* Pointer to the corresponding profitable hard registers.  */
  allocno_hard_regs_t hard_regs;
  /* Parent, first subnode, previous and next node with the same
     parent in the forest.  */
  allocno_hard_regs_node_t parent, first, prev, next;
};

/* To decrease footprint of ira_allocno structure we store all data
   needed only for coloring in the following structure.  */
struct allocno_color_data
{
  /* TRUE value means that the allocno was not removed yet from the
     conflicting graph during colouring.  */
  unsigned int in_graph_p : 1;
  /* TRUE if it is put on the stack to make other allocnos
     colorable.  */
  unsigned int may_be_spilled_p : 1;
  /* TRUE if the allocno is trivially colorable.  */
  unsigned int colorable_p : 1;
  /* Number of hard registers of the allocno class really
     available for the allocno allocation.  It is number of the
     profitable hard regs.  */
  int available_regs_num;
  /* Allocnos in a bucket (used in coloring) chained by the following
     two members.  */
  ira_allocno_t next_bucket_allocno;
  ira_allocno_t prev_bucket_allocno;
  /* Used for temporary purposes.  */
  int temp;
  /* Used to exclude repeated processing.  */
  int last_process;
  /* Profitable hard regs available for this pseudo allocation.  It
     means that the set excludes unavailable hard regs and hard regs
     conflicting with given pseudo.  They should be of the allocno
     class.  */
  HARD_REG_SET profitable_hard_regs;
  /* The allocno hard registers node.  */
  allocno_hard_regs_node_t hard_regs_node;
  /* Array of structures allocno_hard_regs_subnode representing
     given allocno hard registers node (the 1st element in the array)
     and all its subnodes in the tree (forest) of allocno hard
     register nodes (see comments above).  */
  int hard_regs_subnodes_start;
  /* The length of the previous array. */
  int hard_regs_subnodes_num;
};

/* See above.  */
typedef struct allocno_color_data *allocno_color_data_t;

/* Container for storing allocno data concerning coloring.  */
static allocno_color_data_t allocno_color_data;

/* Macro to access the data concerning coloring.  */
#define ALLOCNO_COLOR_DATA(a) ((allocno_color_data_t) ALLOCNO_ADD_DATA (a))

/* Used for finding allocno colorability to exclude repeated allocno
   processing and for updating preferencing to exclude repeated
   allocno processing during assignment.  */
static int curr_allocno_process;

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

/* All allocnos sorted according their priorities.  */
static ira_allocno_t *sorted_allocnos;

/* Vec representing the stack of allocnos used during coloring.  */
static VEC(ira_allocno_t,heap) *allocno_stack_vec;

/* Helper for qsort comparison callbacks - return a positive integer if
   X > Y, or a negative value otherwise.  Use a conditional expression
   instead of a difference computation to insulate from possible overflow
   issues, e.g. X - Y < 0 for some X > 0 and Y < 0.  */
#define SORTGT(x,y) (((x) > (y)) ? 1 : -1)



/* Definition of vector of allocno hard registers.  */
DEF_VEC_P(allocno_hard_regs_t);
DEF_VEC_ALLOC_P(allocno_hard_regs_t, heap);

/* Vector of unique allocno hard registers.  */
static VEC(allocno_hard_regs_t, heap) *allocno_hard_regs_vec;

/* Returns hash value for allocno hard registers V.  */
static hashval_t
allocno_hard_regs_hash (const void *v)
{
  const struct allocno_hard_regs *hv = (const struct allocno_hard_regs *) v;

  return iterative_hash (&hv->set, sizeof (HARD_REG_SET), 0);
}

/* Compares allocno hard registers V1 and V2.  */
static int
allocno_hard_regs_eq (const void *v1, const void *v2)
{
  const struct allocno_hard_regs *hv1 = (const struct allocno_hard_regs *) v1;
  const struct allocno_hard_regs *hv2 = (const struct allocno_hard_regs *) v2;

  return hard_reg_set_equal_p (hv1->set, hv2->set);
}

/* Hash table of unique allocno hard registers.  */
static htab_t allocno_hard_regs_htab;

/* Return allocno hard registers in the hash table equal to HV.  */
static allocno_hard_regs_t
find_hard_regs (allocno_hard_regs_t hv)
{
  return (allocno_hard_regs_t) htab_find (allocno_hard_regs_htab, hv);
}

/* Insert allocno hard registers HV in the hash table (if it is not
   there yet) and return the value which in the table.  */
static allocno_hard_regs_t
insert_hard_regs (allocno_hard_regs_t hv)
{
  PTR *slot = htab_find_slot (allocno_hard_regs_htab, hv, INSERT);

  if (*slot == NULL)
    *slot = hv;
  return (allocno_hard_regs_t) *slot;
}

/* Initialize data concerning allocno hard registers.  */
static void
init_allocno_hard_regs (void)
{
  allocno_hard_regs_vec = VEC_alloc (allocno_hard_regs_t, heap, 200);
  allocno_hard_regs_htab
    = htab_create (200, allocno_hard_regs_hash, allocno_hard_regs_eq, NULL);
}

/* Add (or update info about) allocno hard registers with SET and
   COST.  */
static allocno_hard_regs_t
add_allocno_hard_regs (HARD_REG_SET set, HOST_WIDEST_INT cost)
{
  struct allocno_hard_regs temp;
  allocno_hard_regs_t hv;

  gcc_assert (! hard_reg_set_empty_p (set));
  COPY_HARD_REG_SET (temp.set, set);
  if ((hv = find_hard_regs (&temp)) != NULL)
    hv->cost += cost;
  else
    {
      hv = ((struct allocno_hard_regs *)
	    ira_allocate (sizeof (struct allocno_hard_regs)));
      COPY_HARD_REG_SET (hv->set, set);
      hv->cost = cost;
      VEC_safe_push (allocno_hard_regs_t, heap, allocno_hard_regs_vec, hv);
      insert_hard_regs (hv);
    }
  return hv;
}

/* Finalize data concerning allocno hard registers.  */
static void
finish_allocno_hard_regs (void)
{
  int i;
  allocno_hard_regs_t hv;

  for (i = 0;
       VEC_iterate (allocno_hard_regs_t, allocno_hard_regs_vec, i, hv);
       i++)
    ira_free (hv);
  htab_delete (allocno_hard_regs_htab);
  VEC_free (allocno_hard_regs_t, heap, allocno_hard_regs_vec);
}

/* Sort hard regs according to their frequency of usage. */
static int
allocno_hard_regs_compare (const void *v1p, const void *v2p)
{
  allocno_hard_regs_t hv1 = *(const allocno_hard_regs_t *) v1p;
  allocno_hard_regs_t hv2 = *(const allocno_hard_regs_t *) v2p;

  if (hv2->cost > hv1->cost)
    return 1;
  else if (hv2->cost < hv1->cost)
    return -1;
  else
    return 0;
}



/* Used for finding a common ancestor of two allocno hard registers
   nodes in the forest.  We use the current value of
   'node_check_tick' to mark all nodes from one node to the top and
   then walking up from another node until we find a marked node.

   It is also used to figure out allocno colorability as a mark that
   we already reset value of member 'conflict_size' for the forest
   node corresponding to the processed allocno.  */
static int node_check_tick;

/* Roots of the forest containing hard register sets can be assigned
   to allocnos.  */
static allocno_hard_regs_node_t hard_regs_roots;

/* Definition of vector of allocno hard register nodes.  */
DEF_VEC_P(allocno_hard_regs_node_t);
DEF_VEC_ALLOC_P(allocno_hard_regs_node_t, heap);

/* Vector used to create the forest.  */
static VEC(allocno_hard_regs_node_t, heap) *hard_regs_node_vec;

/* Create and return allocno hard registers node containing allocno
   hard registers HV.  */
static allocno_hard_regs_node_t
create_new_allocno_hard_regs_node (allocno_hard_regs_t hv)
{
  allocno_hard_regs_node_t new_node;

  new_node = ((struct allocno_hard_regs_node *)
	      ira_allocate (sizeof (struct allocno_hard_regs_node)));
  new_node->check = 0;
  new_node->hard_regs = hv;
  new_node->hard_regs_num = hard_reg_set_size (hv->set);
  new_node->first = NULL;
  new_node->used_p = false;
  return new_node;
}

/* Add allocno hard registers node NEW_NODE to the forest on its level
   given by ROOTS.  */
static void
add_new_allocno_hard_regs_node_to_forest (allocno_hard_regs_node_t *roots,
					  allocno_hard_regs_node_t new_node)
{
  new_node->next = *roots;
  if (new_node->next != NULL)
    new_node->next->prev = new_node;
  new_node->prev = NULL;
  *roots = new_node;
}

/* Add allocno hard registers HV (or its best approximation if it is
   not possible) to the forest on its level given by ROOTS.  */
static void
add_allocno_hard_regs_to_forest (allocno_hard_regs_node_t *roots,
				 allocno_hard_regs_t hv)
{
  unsigned int i, start;
  allocno_hard_regs_node_t node, prev, new_node;
  HARD_REG_SET temp_set;
  allocno_hard_regs_t hv2;

  start = VEC_length (allocno_hard_regs_node_t, hard_regs_node_vec);
  for (node = *roots; node != NULL; node = node->next)
    {
      if (hard_reg_set_equal_p (hv->set, node->hard_regs->set))
	return;
      if (hard_reg_set_subset_p (hv->set, node->hard_regs->set))
	{
	  add_allocno_hard_regs_to_forest (&node->first, hv);
	  return;
	}
      if (hard_reg_set_subset_p (node->hard_regs->set, hv->set))
	VEC_safe_push (allocno_hard_regs_node_t, heap,
		       hard_regs_node_vec, node);
      else if (hard_reg_set_intersect_p (hv->set, node->hard_regs->set))
	{
	  COPY_HARD_REG_SET (temp_set, hv->set);
	  AND_HARD_REG_SET (temp_set, node->hard_regs->set);
	  hv2 = add_allocno_hard_regs (temp_set, hv->cost);
	  add_allocno_hard_regs_to_forest (&node->first, hv2);
	}
    }
  if (VEC_length (allocno_hard_regs_node_t, hard_regs_node_vec)
      > start + 1)
    {
      /* Create a new node which contains nodes in hard_regs_node_vec.  */
      CLEAR_HARD_REG_SET (temp_set);
      for (i = start;
	   i < VEC_length (allocno_hard_regs_node_t, hard_regs_node_vec);
	   i++)
	{
	  node = VEC_index (allocno_hard_regs_node_t, hard_regs_node_vec, i);
	  IOR_HARD_REG_SET (temp_set, node->hard_regs->set);
	}
      hv = add_allocno_hard_regs (temp_set, hv->cost);
      new_node = create_new_allocno_hard_regs_node (hv);
      prev = NULL;
      for (i = start;
	   i < VEC_length (allocno_hard_regs_node_t, hard_regs_node_vec);
	   i++)
	{
	  node = VEC_index (allocno_hard_regs_node_t, hard_regs_node_vec, i);
	  if (node->prev == NULL)
	    *roots = node->next;
	  else
	    node->prev->next = node->next;
	  if (node->next != NULL)
	    node->next->prev = node->prev;
	  if (prev == NULL)
	    new_node->first = node;
	  else
	    prev->next = node;
	  node->prev = prev;
	  node->next = NULL;
	  prev = node;
	}
      add_new_allocno_hard_regs_node_to_forest (roots, new_node);
    }
  VEC_truncate (allocno_hard_regs_node_t, hard_regs_node_vec, start);
}

/* Add allocno hard registers nodes starting with the forest level
   given by FIRST which contains biggest set inside SET.  */
static void
collect_allocno_hard_regs_cover (allocno_hard_regs_node_t first,
				 HARD_REG_SET set)
{
  allocno_hard_regs_node_t node;

  ira_assert (first != NULL);
  for (node = first; node != NULL; node = node->next)
    if (hard_reg_set_subset_p (node->hard_regs->set, set))
      VEC_safe_push (allocno_hard_regs_node_t, heap, hard_regs_node_vec,
		     node);
    else if (hard_reg_set_intersect_p (set, node->hard_regs->set))
      collect_allocno_hard_regs_cover (node->first, set);
}

/* Set up field parent as PARENT in all allocno hard registers nodes
   in forest given by FIRST.  */
static void
setup_allocno_hard_regs_nodes_parent (allocno_hard_regs_node_t first,
				      allocno_hard_regs_node_t parent)
{
  allocno_hard_regs_node_t node;

  for (node = first; node != NULL; node = node->next)
    {
      node->parent = parent;
      setup_allocno_hard_regs_nodes_parent (node->first, node);
    }
}

/* Return allocno hard registers node which is a first common ancestor
   node of FIRST and SECOND in the forest.  */
static allocno_hard_regs_node_t
first_common_ancestor_node (allocno_hard_regs_node_t first,
			    allocno_hard_regs_node_t second)
{
  allocno_hard_regs_node_t node;

  node_check_tick++;
  for (node = first; node != NULL; node = node->parent)
    node->check = node_check_tick;
  for (node = second; node != NULL; node = node->parent)
    if (node->check == node_check_tick)
      return node;
  return first_common_ancestor_node (second, first);
}

/* Print hard reg set SET to F.  */
static void
print_hard_reg_set (FILE *f, HARD_REG_SET set, bool new_line_p)
{
  int i, start;

  for (start = -1, i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (TEST_HARD_REG_BIT (set, i))
	{
	  if (i == 0 || ! TEST_HARD_REG_BIT (set, i - 1))
	    start = i;
	}
      if (start >= 0
	  && (i == FIRST_PSEUDO_REGISTER - 1 || ! TEST_HARD_REG_BIT (set, i)))
	{
	  if (start == i - 1)
	    fprintf (f, " %d", start);
	  else if (start == i - 2)
	    fprintf (f, " %d %d", start, start + 1);
	  else
	    fprintf (f, " %d-%d", start, i - 1);
	  start = -1;
	}
    }
  if (new_line_p)
    fprintf (f, "\n");
}

/* Print allocno hard register subforest given by ROOTS and its LEVEL
   to F.  */
static void
print_hard_regs_subforest (FILE *f, allocno_hard_regs_node_t roots,
			   int level)
{
  int i;
  allocno_hard_regs_node_t node;

  for (node = roots; node != NULL; node = node->next)
    {
      fprintf (f, "    ");
      for (i = 0; i < level * 2; i++)
	fprintf (f, " ");
      fprintf (f, "%d:(", node->preorder_num);
      print_hard_reg_set (f, node->hard_regs->set, false);
      fprintf (f, ")@" HOST_WIDEST_INT_PRINT_DEC "\n", node->hard_regs->cost);
      print_hard_regs_subforest (f, node->first, level + 1);
    }
}

/* Print the allocno hard register forest to F.  */
static void
print_hard_regs_forest (FILE *f)
{
  fprintf (f, "    Hard reg set forest:\n");
  print_hard_regs_subforest (f, hard_regs_roots, 1);
}

/* Print the allocno hard register forest to stderr.  */
void
ira_debug_hard_regs_forest (void)
{
  print_hard_regs_forest (stderr);
}

/* Remove unused allocno hard registers nodes from forest given by its
   *ROOTS.  */
static void
remove_unused_allocno_hard_regs_nodes (allocno_hard_regs_node_t *roots)
{
  allocno_hard_regs_node_t node, prev, next, last;

  for (prev = NULL, node = *roots; node != NULL; node = next)
    {
      next = node->next;
      if (node->used_p)
	{
	  remove_unused_allocno_hard_regs_nodes (&node->first);
	  prev = node;
	}
      else
	{
	  for (last = node->first;
	       last != NULL && last->next != NULL;
	       last = last->next)
	    ;
	  if (last != NULL)
	    {
	      if (prev == NULL)
		*roots = node->first;
	      else 
		prev->next = node->first;
	      if (next != NULL)
		next->prev = last;
	      last->next = next;
	      next = node->first;
	    }
	  else
	    {
	      if (prev == NULL)
		*roots = next;
	      else
		prev->next = next;
	      if (next != NULL)
		next->prev = prev;
	    }
	  ira_free (node);
	}
    }
}

/* Set up fields preorder_num starting with START_NUM in all allocno
   hard registers nodes in forest given by FIRST.  Return biggest set
   PREORDER_NUM increased by 1.  */
static int
enumerate_allocno_hard_regs_nodes (allocno_hard_regs_node_t first,
				   allocno_hard_regs_node_t parent,
				   int start_num)
{
  allocno_hard_regs_node_t node;

  for (node = first; node != NULL; node = node->next)
    {
      node->preorder_num = start_num++;
      node->parent = parent;
      start_num = enumerate_allocno_hard_regs_nodes (node->first, node,
						     start_num);
    }
  return start_num;
}

/* Number of allocno hard registers nodes in the forest.  */
static int allocno_hard_regs_nodes_num;

/* Table preorder number of allocno hard registers node in the forest
   -> the allocno hard registers node.  */
static allocno_hard_regs_node_t *allocno_hard_regs_nodes;

/* See below.  */
typedef struct allocno_hard_regs_subnode *allocno_hard_regs_subnode_t;

/* The structure is used to describes all subnodes (not only immediate
   ones) in the mentioned above tree for given allocno hard register
   node.  The usage of such data accelerates calculation of
   colorability of given allocno.  */
struct allocno_hard_regs_subnode
{
  /* The conflict size of conflicting allocnos whose hard register
     sets are equal sets (plus supersets if given node is given
     allocno hard registers node) of one in the given node.  */
  int left_conflict_size;
  /* The summary conflict size of conflicting allocnos whose hard
     register sets are strict subsets of one in the given node.
     Overall conflict size is
     left_conflict_subnodes_size
       + MIN (max_node_impact - left_conflict_subnodes_size,
              left_conflict_size)
  */
  short left_conflict_subnodes_size;
  short max_node_impact;
};

/* Container for hard regs subnodes of all allocnos.  */
static allocno_hard_regs_subnode_t allocno_hard_regs_subnodes;

/* Table (preorder number of allocno hard registers node in the
   forest, preorder number of allocno hard registers subnode) -> index
   of the subnode relative to the node.  -1 if it is not a
   subnode.  */
static int *allocno_hard_regs_subnode_index;

/* Setup arrays ALLOCNO_HARD_REGS_NODES and
   ALLOCNO_HARD_REGS_SUBNODE_INDEX.  */
static void
setup_allocno_hard_regs_subnode_index (allocno_hard_regs_node_t first)
{
  allocno_hard_regs_node_t node, parent;
  int index;

  for (node = first; node != NULL; node = node->next)
    {
      allocno_hard_regs_nodes[node->preorder_num] = node;
      for (parent = node; parent != NULL; parent = parent->parent)
	{
	  index = parent->preorder_num * allocno_hard_regs_nodes_num;
	  allocno_hard_regs_subnode_index[index + node->preorder_num]
	    = node->preorder_num - parent->preorder_num;
	}
      setup_allocno_hard_regs_subnode_index (node->first);
    }
}

/* Count all allocno hard registers nodes in tree ROOT.  */
static int
get_allocno_hard_regs_subnodes_num (allocno_hard_regs_node_t root)
{
  int len = 1;

  for (root = root->first; root != NULL; root = root->next)
    len += get_allocno_hard_regs_subnodes_num (root);
  return len;
}

/* Build the forest of allocno hard registers nodes and assign each
   allocno a node from the forest.  */
static void
form_allocno_hard_regs_nodes_forest (void)
{
  unsigned int i, j, size, len;
  int start;
  ira_allocno_t a;
  allocno_hard_regs_t hv;
  bitmap_iterator bi;
  HARD_REG_SET temp;
  allocno_hard_regs_node_t node, allocno_hard_regs_node;
  allocno_color_data_t allocno_data;

  node_check_tick = 0;
  init_allocno_hard_regs ();
  hard_regs_roots = NULL;
  hard_regs_node_vec = VEC_alloc (allocno_hard_regs_node_t, heap, 100);
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (! TEST_HARD_REG_BIT (ira_no_alloc_regs, i))
      {
	CLEAR_HARD_REG_SET (temp);
	SET_HARD_REG_BIT (temp, i);
	hv = add_allocno_hard_regs (temp, 0);
	node = create_new_allocno_hard_regs_node (hv);
	add_new_allocno_hard_regs_node_to_forest (&hard_regs_roots, node);
      }
  start = VEC_length (allocno_hard_regs_t, allocno_hard_regs_vec);
  EXECUTE_IF_SET_IN_BITMAP (coloring_allocno_bitmap, 0, i, bi)
    {
      a = ira_allocnos[i];
      allocno_data = ALLOCNO_COLOR_DATA (a);
      
      if (hard_reg_set_empty_p (allocno_data->profitable_hard_regs))
	continue;
      hv = (add_allocno_hard_regs
	    (allocno_data->profitable_hard_regs,
	     ALLOCNO_MEMORY_COST (a) - ALLOCNO_CLASS_COST (a)));
    }
  SET_HARD_REG_SET (temp);
  AND_COMPL_HARD_REG_SET (temp, ira_no_alloc_regs);
  add_allocno_hard_regs (temp, 0);
  qsort (VEC_address (allocno_hard_regs_t, allocno_hard_regs_vec) + start,
	 VEC_length (allocno_hard_regs_t, allocno_hard_regs_vec) - start,
	 sizeof (allocno_hard_regs_t), allocno_hard_regs_compare);
  for (i = start;
       VEC_iterate (allocno_hard_regs_t, allocno_hard_regs_vec, i, hv);
       i++)
    {
      add_allocno_hard_regs_to_forest (&hard_regs_roots, hv);
      ira_assert (VEC_length (allocno_hard_regs_node_t,
			      hard_regs_node_vec) == 0);
    }
  /* We need to set up parent fields for right work of
     first_common_ancestor_node. */
  setup_allocno_hard_regs_nodes_parent (hard_regs_roots, NULL);
  EXECUTE_IF_SET_IN_BITMAP (coloring_allocno_bitmap, 0, i, bi)
    {
      a = ira_allocnos[i];
      allocno_data = ALLOCNO_COLOR_DATA (a);
      if (hard_reg_set_empty_p (allocno_data->profitable_hard_regs))
	continue;
      VEC_truncate (allocno_hard_regs_node_t, hard_regs_node_vec, 0);
      collect_allocno_hard_regs_cover (hard_regs_roots,
				       allocno_data->profitable_hard_regs);
      allocno_hard_regs_node = NULL;
      for (j = 0;
	   VEC_iterate (allocno_hard_regs_node_t, hard_regs_node_vec,
			j, node);
	   j++)
	allocno_hard_regs_node
	  = (j == 0
	     ? node
	     : first_common_ancestor_node (node, allocno_hard_regs_node));
      /* That is a temporary storage.  */
      allocno_hard_regs_node->used_p = true;
      allocno_data->hard_regs_node = allocno_hard_regs_node;
    }
  ira_assert (hard_regs_roots->next == NULL);
  hard_regs_roots->used_p = true;
  remove_unused_allocno_hard_regs_nodes (&hard_regs_roots);
  allocno_hard_regs_nodes_num
    = enumerate_allocno_hard_regs_nodes (hard_regs_roots, NULL, 0);
  allocno_hard_regs_nodes
    = ((allocno_hard_regs_node_t *)
       ira_allocate (allocno_hard_regs_nodes_num
		     * sizeof (allocno_hard_regs_node_t)));
  size = allocno_hard_regs_nodes_num * allocno_hard_regs_nodes_num;
  allocno_hard_regs_subnode_index
    = (int *) ira_allocate (size * sizeof (int));
  for (i = 0; i < size; i++)
    allocno_hard_regs_subnode_index[i] = -1;
  setup_allocno_hard_regs_subnode_index (hard_regs_roots);
  start = 0;
  EXECUTE_IF_SET_IN_BITMAP (coloring_allocno_bitmap, 0, i, bi)
    {
      a = ira_allocnos[i];
      allocno_data = ALLOCNO_COLOR_DATA (a);
      if (hard_reg_set_empty_p (allocno_data->profitable_hard_regs))
	continue;
      len = get_allocno_hard_regs_subnodes_num (allocno_data->hard_regs_node);
      allocno_data->hard_regs_subnodes_start = start;
      allocno_data->hard_regs_subnodes_num = len;
      start += len;
    }
  allocno_hard_regs_subnodes
    = ((allocno_hard_regs_subnode_t)
       ira_allocate (sizeof (struct allocno_hard_regs_subnode) * start));
  VEC_free (allocno_hard_regs_node_t, heap, hard_regs_node_vec);
}

/* Free tree of allocno hard registers nodes given by its ROOT.  */
static void
finish_allocno_hard_regs_nodes_tree (allocno_hard_regs_node_t root)
{
  allocno_hard_regs_node_t child, next;

  for (child = root->first; child != NULL; child = next)
    {
      next = child->next;
      finish_allocno_hard_regs_nodes_tree (child);
    }
  ira_free (root);
}

/* Finish work with the forest of allocno hard registers nodes.  */
static void
finish_allocno_hard_regs_nodes_forest (void)
{
  allocno_hard_regs_node_t node, next;
  
  ira_free (allocno_hard_regs_subnodes);
  for (node = hard_regs_roots; node != NULL; node = next)
    {
      next = node->next;
      finish_allocno_hard_regs_nodes_tree (node);
    }
  ira_free (allocno_hard_regs_nodes);
  ira_free (allocno_hard_regs_subnode_index);
  finish_allocno_hard_regs ();
}

/* Set up left conflict sizes and left conflict subnodes sizes of hard
   registers subnodes of allocno A.  Return TRUE if allocno A is
   trivially colorable.  */
static bool
setup_left_conflict_sizes_p (ira_allocno_t a)
{
  int i, k, nobj, start;
  int conflict_size, left_conflict_subnodes_size, node_preorder_num;
  allocno_color_data_t data;
  HARD_REG_SET profitable_hard_regs;
  allocno_hard_regs_subnode_t subnodes;
  allocno_hard_regs_node_t node;
  HARD_REG_SET node_set;

  nobj = ALLOCNO_NUM_OBJECTS (a);
  conflict_size = 0;
  data = ALLOCNO_COLOR_DATA (a);
  subnodes = allocno_hard_regs_subnodes + data->hard_regs_subnodes_start;
  COPY_HARD_REG_SET (profitable_hard_regs, data->profitable_hard_regs);
  node = data->hard_regs_node;
  node_preorder_num = node->preorder_num;
  COPY_HARD_REG_SET (node_set, node->hard_regs->set);
  node_check_tick++;
  for (k = 0; k < nobj; k++)
    {
      ira_object_t obj = ALLOCNO_OBJECT (a, k);
      ira_object_t conflict_obj;
      ira_object_conflict_iterator oci;
      
      FOR_EACH_OBJECT_CONFLICT (obj, conflict_obj, oci)
	{
	  int size;
 	  ira_allocno_t conflict_a = OBJECT_ALLOCNO (conflict_obj);
	  allocno_hard_regs_node_t conflict_node, temp_node;
	  HARD_REG_SET conflict_node_set;
	  allocno_color_data_t conflict_data;

	  conflict_data = ALLOCNO_COLOR_DATA (conflict_a);
	  if (! ALLOCNO_COLOR_DATA (conflict_a)->in_graph_p
	      || ! hard_reg_set_intersect_p (profitable_hard_regs,
					     conflict_data
					     ->profitable_hard_regs))
	    continue;
	  conflict_node = conflict_data->hard_regs_node;
	  COPY_HARD_REG_SET (conflict_node_set, conflict_node->hard_regs->set);
	  if (hard_reg_set_subset_p (node_set, conflict_node_set))
	    temp_node = node;
	  else
	    {
	      ira_assert (hard_reg_set_subset_p (conflict_node_set, node_set));
	      temp_node = conflict_node;
	    }
	  if (temp_node->check != node_check_tick)
	    {
	      temp_node->check = node_check_tick;
	      temp_node->conflict_size = 0;
	    }
	  size = (ira_reg_class_max_nregs
		  [ALLOCNO_CLASS (conflict_a)][ALLOCNO_MODE (conflict_a)]);
	  if (ALLOCNO_NUM_OBJECTS (conflict_a) > 1)
	    /* We will deal with the subwords individually.  */
	    size = 1;
	  temp_node->conflict_size += size;
	}
    }
  for (i = 0; i < data->hard_regs_subnodes_num; i++)
    {
      allocno_hard_regs_node_t temp_node;
      
      temp_node = allocno_hard_regs_nodes[i + node_preorder_num];
      ira_assert (temp_node->preorder_num == i + node_preorder_num);
      subnodes[i].left_conflict_size = (temp_node->check != node_check_tick
					? 0 : temp_node->conflict_size);
      if (hard_reg_set_subset_p (temp_node->hard_regs->set,
				 profitable_hard_regs))
	subnodes[i].max_node_impact = temp_node->hard_regs_num;
      else
	{
	  HARD_REG_SET temp_set;
	  int j, n, hard_regno;
	  enum reg_class aclass;
	  
	  COPY_HARD_REG_SET (temp_set, temp_node->hard_regs->set);
	  AND_HARD_REG_SET (temp_set, profitable_hard_regs);
	  aclass = ALLOCNO_CLASS (a);
	  for (n = 0, j = ira_class_hard_regs_num[aclass] - 1; j >= 0; j--)
	    {
	      hard_regno = ira_class_hard_regs[aclass][j];
	      if (TEST_HARD_REG_BIT (temp_set, hard_regno))
		n++;
	    }
	  subnodes[i].max_node_impact = n;
	}
      subnodes[i].left_conflict_subnodes_size = 0;
    }
  start = node_preorder_num * allocno_hard_regs_nodes_num;
  for (i = data->hard_regs_subnodes_num - 1; i >= 0; i--)
    {
      int size, parent_i;
      allocno_hard_regs_node_t parent;
      
      size = (subnodes[i].left_conflict_subnodes_size
	      + MIN (subnodes[i].max_node_impact
		     - subnodes[i].left_conflict_subnodes_size,
		     subnodes[i].left_conflict_size));
      parent = allocno_hard_regs_nodes[i + node_preorder_num]->parent;
      if (parent == NULL)
	continue;
      parent_i
	= allocno_hard_regs_subnode_index[start + parent->preorder_num];
      if (parent_i < 0)
	continue;
      subnodes[parent_i].left_conflict_subnodes_size += size;
    }
  left_conflict_subnodes_size = subnodes[0].left_conflict_subnodes_size;
  conflict_size
    += (left_conflict_subnodes_size
	+ MIN (subnodes[0].max_node_impact - left_conflict_subnodes_size,
	       subnodes[0].left_conflict_size));
  conflict_size += ira_reg_class_max_nregs[ALLOCNO_CLASS (a)][ALLOCNO_MODE (a)];
  data->colorable_p = conflict_size <= data->available_regs_num;
  return data->colorable_p;
}

/* Update left conflict sizes of hard registers subnodes of allocno A
   after removing allocno REMOVED_A with SIZE from the conflict graph.
   Return TRUE if A is trivially colorable.  */
static bool
update_left_conflict_sizes_p (ira_allocno_t a,
			      ira_allocno_t removed_a, int size)
{
  int i, conflict_size, before_conflict_size, diff, start;
  int node_preorder_num, parent_i;
  allocno_hard_regs_node_t node, removed_node, parent;
  allocno_hard_regs_subnode_t subnodes;
  allocno_color_data_t data = ALLOCNO_COLOR_DATA (a);

  ira_assert (! data->colorable_p);
  node = data->hard_regs_node;
  node_preorder_num = node->preorder_num;
  removed_node = ALLOCNO_COLOR_DATA (removed_a)->hard_regs_node;
  ira_assert (hard_reg_set_subset_p (removed_node->hard_regs->set,
			       node->hard_regs->set)
	      || hard_reg_set_subset_p (node->hard_regs->set,
					removed_node->hard_regs->set));
  start = node_preorder_num * allocno_hard_regs_nodes_num;
  i = allocno_hard_regs_subnode_index[start + removed_node->preorder_num];
  if (i < 0)
    i = 0;
  subnodes = allocno_hard_regs_subnodes + data->hard_regs_subnodes_start;
  before_conflict_size
    = (subnodes[i].left_conflict_subnodes_size
       + MIN (subnodes[i].max_node_impact
	      - subnodes[i].left_conflict_subnodes_size,
	      subnodes[i].left_conflict_size));
  subnodes[i].left_conflict_size -= size;
  for (;;)
    {
      conflict_size
	= (subnodes[i].left_conflict_subnodes_size
	   + MIN (subnodes[i].max_node_impact
		  - subnodes[i].left_conflict_subnodes_size,
		  subnodes[i].left_conflict_size));
      if ((diff = before_conflict_size - conflict_size) == 0)
	break;
      ira_assert (conflict_size < before_conflict_size);
      parent = allocno_hard_regs_nodes[i + node_preorder_num]->parent;
      if (parent == NULL)
	break;
      parent_i
	= allocno_hard_regs_subnode_index[start + parent->preorder_num];
      if (parent_i < 0)
	break;
      i = parent_i;
      before_conflict_size
	= (subnodes[i].left_conflict_subnodes_size
	   + MIN (subnodes[i].max_node_impact
		  - subnodes[i].left_conflict_subnodes_size,
		  subnodes[i].left_conflict_size));
      subnodes[i].left_conflict_subnodes_size -= diff;
    }
  if (i != 0
      || (conflict_size 
	  + ira_reg_class_max_nregs[ALLOCNO_CLASS (a)][ALLOCNO_MODE (a)]
	  > data->available_regs_num))
    return false;
  data->colorable_p = true;
  return true;
}

/* Return true if allocno A has empty profitable hard regs.  */
static bool
empty_profitable_hard_regs (ira_allocno_t a)
{
  allocno_color_data_t data = ALLOCNO_COLOR_DATA (a);

  return hard_reg_set_empty_p (data->profitable_hard_regs);
}

/* Set up profitable hard registers for each allocno being
   colored.  */
static void
setup_profitable_hard_regs (void)
{
  unsigned int i;
  int j, k, nobj, hard_regno, nregs, class_size;
  ira_allocno_t a;
  bitmap_iterator bi;
  enum reg_class aclass;
  enum machine_mode mode;
  allocno_color_data_t data;

  /* Initial set up from allocno classes and explicitly conflicting
     hard regs.  */
  EXECUTE_IF_SET_IN_BITMAP (coloring_allocno_bitmap, 0, i, bi)
    {
      a = ira_allocnos[i];
      if ((aclass = ALLOCNO_CLASS (a)) == NO_REGS)
	continue;
      data = ALLOCNO_COLOR_DATA (a);
      if (ALLOCNO_UPDATED_HARD_REG_COSTS (a) == NULL
	  && ALLOCNO_CLASS_COST (a) > ALLOCNO_MEMORY_COST (a))
	CLEAR_HARD_REG_SET (data->profitable_hard_regs);
      else
	{
	  COPY_HARD_REG_SET (data->profitable_hard_regs,
			     reg_class_contents[aclass]);
	  AND_COMPL_HARD_REG_SET (data->profitable_hard_regs,
				  ira_no_alloc_regs);
	  nobj = ALLOCNO_NUM_OBJECTS (a);
	  for (k = 0; k < nobj; k++)
	    {
	      ira_object_t obj = ALLOCNO_OBJECT (a, k);
	      
	      AND_COMPL_HARD_REG_SET (data->profitable_hard_regs,
				      OBJECT_TOTAL_CONFLICT_HARD_REGS (obj));
	    }
	}
    }
  /* Exclude hard regs already assigned for conflicting objects.  */
  EXECUTE_IF_SET_IN_BITMAP (consideration_allocno_bitmap, 0, i, bi)
    {
      a = ira_allocnos[i];
      if ((aclass = ALLOCNO_CLASS (a)) == NO_REGS
	  || ! ALLOCNO_ASSIGNED_P (a)
	  || (hard_regno = ALLOCNO_HARD_REGNO (a)) < 0)
	continue;
      mode = ALLOCNO_MODE (a);
      nregs = hard_regno_nregs[hard_regno][mode];
      nobj = ALLOCNO_NUM_OBJECTS (a);
      for (k = 0; k < nobj; k++)
	{
	  ira_object_t obj = ALLOCNO_OBJECT (a, k);
	  ira_object_t conflict_obj;
	  ira_object_conflict_iterator oci;

	  FOR_EACH_OBJECT_CONFLICT (obj, conflict_obj, oci)
	    {
	      ira_allocno_t conflict_a = OBJECT_ALLOCNO (conflict_obj);

	      /* We can process the conflict allocno repeatedly with
		 the same result.  */
	      if (nregs == nobj && nregs > 1)
		{
		  int num = OBJECT_SUBWORD (conflict_obj);
		  
		  if (REG_WORDS_BIG_ENDIAN)
		    CLEAR_HARD_REG_BIT
		      (ALLOCNO_COLOR_DATA (conflict_a)->profitable_hard_regs,
		       hard_regno + nobj - num - 1);
		  else
		    CLEAR_HARD_REG_BIT
		      (ALLOCNO_COLOR_DATA (conflict_a)->profitable_hard_regs,
		       hard_regno + num);
		}
	      else
		AND_COMPL_HARD_REG_SET
		  (ALLOCNO_COLOR_DATA (conflict_a)->profitable_hard_regs,
		   ira_reg_mode_hard_regset[hard_regno][mode]);
	    }
	}
    }
  /* Exclude too costly hard regs.  */
  EXECUTE_IF_SET_IN_BITMAP (coloring_allocno_bitmap, 0, i, bi)
    {
      int min_cost = INT_MAX;
      int *costs;

      a = ira_allocnos[i];
      if ((aclass = ALLOCNO_CLASS (a)) == NO_REGS
	  || empty_profitable_hard_regs (a))
	continue;
      data = ALLOCNO_COLOR_DATA (a);
      mode = ALLOCNO_MODE (a);
      if ((costs = ALLOCNO_UPDATED_HARD_REG_COSTS (a)) != NULL
	  || (costs = ALLOCNO_HARD_REG_COSTS (a)) != NULL)
	{
	  class_size = ira_class_hard_regs_num[aclass];
	  for (j = 0; j < class_size; j++)
	    {
	      hard_regno = ira_class_hard_regs[aclass][j];
	      if (! TEST_HARD_REG_BIT (data->profitable_hard_regs,
				       hard_regno))
		continue;
	      if (ALLOCNO_UPDATED_MEMORY_COST (a) < costs[j])
		CLEAR_HARD_REG_BIT (data->profitable_hard_regs,
				    hard_regno);
	      else if (min_cost > costs[j])
		min_cost = costs[j];
	    }
	}
      else if (ALLOCNO_UPDATED_MEMORY_COST (a)
	       < ALLOCNO_UPDATED_CLASS_COST (a))
	CLEAR_HARD_REG_SET (data->profitable_hard_regs);
      if (ALLOCNO_UPDATED_CLASS_COST (a) > min_cost)
	ALLOCNO_UPDATED_CLASS_COST (a) = min_cost;
    }
}



/* This page contains functions used to choose hard registers for
   allocnos.  */

/* Array whose element value is TRUE if the corresponding hard
   register was already allocated for an allocno.  */
static bool allocated_hardreg_p[FIRST_PSEUDO_REGISTER];

/* Describes one element in a queue of allocnos whose costs need to be
   updated.  Each allocno in the queue is known to have an allocno
   class.  */
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

/* Add (ALLOCNO, DIVISOR) to the end of update_cost_queue, unless
   ALLOCNO is already in the queue, or has NO_REGS class.  */
static inline void
queue_update_cost (ira_allocno_t allocno, int divisor)
{
  struct update_cost_queue_elem *elem;

  elem = &update_cost_queue_elems[ALLOCNO_NUM (allocno)];
  if (elem->check != update_cost_check
      && ALLOCNO_CLASS (allocno) != NO_REGS)
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
  enum reg_class rclass, aclass;
  ira_allocno_t another_allocno;
  ira_copy_t cp, next_cp;

  hard_regno = ALLOCNO_HARD_REGNO (allocno);
  ira_assert (hard_regno >= 0);

  aclass = ALLOCNO_CLASS (allocno);
  if (aclass == NO_REGS)
    return;
  i = ira_class_hard_reg_index[aclass][hard_regno];
  ira_assert (i >= 0);
  rclass = REGNO_REG_CLASS (hard_regno);

  start_update_cost ();
  divisor = 1;
  do
    {
      mode = ALLOCNO_MODE (allocno);
      ira_init_register_move_cost_if_necessary (mode);
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

	  aclass = ALLOCNO_CLASS (another_allocno);
	  if (! TEST_HARD_REG_BIT (reg_class_contents[aclass],
				   hard_regno)
	      || ALLOCNO_ASSIGNED_P (another_allocno))
	    continue;

	  cost = (cp->second == allocno
		  ? ira_register_move_cost[mode][rclass][aclass]
		  : ira_register_move_cost[mode][aclass][rclass]);
	  if (decr_p)
	    cost = -cost;

	  update_cost = cp->freq * cost / divisor;
	  if (update_cost == 0)
	    continue;

	  ira_allocate_and_set_or_copy_costs
	    (&ALLOCNO_UPDATED_HARD_REG_COSTS (another_allocno), aclass,
	     ALLOCNO_UPDATED_CLASS_COST (another_allocno),
	     ALLOCNO_HARD_REG_COSTS (another_allocno));
	  ira_allocate_and_set_or_copy_costs
	    (&ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS (another_allocno),
	     aclass, 0, ALLOCNO_CONFLICT_HARD_REG_COSTS (another_allocno));
	  i = ira_class_hard_reg_index[aclass][hard_regno];
	  if (i < 0)
	    continue;
	  ALLOCNO_UPDATED_HARD_REG_COSTS (another_allocno)[i] += update_cost;
	  ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS (another_allocno)[i]
	    += update_cost;

	  queue_update_cost (another_allocno, divisor * COST_HOP_DIVISOR);
	}
    }
  while (get_next_update_cost (&allocno, &divisor));
}

/* This function updates COSTS (decrease if DECR_P) for hard_registers
   of ACLASS by conflict costs of the unassigned allocnos
   connected by copies with allocnos in update_cost_queue.  This
   update increases chances to remove some copies.  */
static void
update_conflict_hard_regno_costs (int *costs, enum reg_class aclass,
				  bool decr_p)
{
  int i, cost, class_size, freq, mult, div, divisor;
  int index, hard_regno;
  int *conflict_costs;
  bool cont_p;
  enum reg_class another_aclass;
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
 	another_aclass = ALLOCNO_CLASS (another_allocno);
 	if (! ira_reg_classes_intersect_p[aclass][another_aclass]
	    || ALLOCNO_ASSIGNED_P (another_allocno)
	    || ALLOCNO_COLOR_DATA (another_allocno)->may_be_spilled_p)
	  continue;
	class_size = ira_class_hard_regs_num[another_aclass];
	ira_allocate_and_copy_costs
	  (&ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS (another_allocno),
	   another_aclass, ALLOCNO_CONFLICT_HARD_REG_COSTS (another_allocno));
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
		hard_regno = ira_class_hard_regs[another_aclass][i];
		ira_assert (hard_regno >= 0);
		index = ira_class_hard_reg_index[aclass][hard_regno];
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

/* Set up conflicting (through CONFLICT_REGS) for each object of
   allocno A and the start allocno profitable regs (through
   START_PROFITABLE_REGS).  Remember that the start profitable regs
   exclude hard regs which can not hold value of mode of allocno A.
   This covers mostly cases when multi-register value should be
   aligned.  */
static inline void
get_conflict_and_start_profitable_regs (ira_allocno_t a, bool retry_p,
					HARD_REG_SET *conflict_regs,
					HARD_REG_SET *start_profitable_regs)
{
  int i, nwords;
  ira_object_t obj;

  nwords = ALLOCNO_NUM_OBJECTS (a);
  for (i = 0; i < nwords; i++)
    {
      obj = ALLOCNO_OBJECT (a, i);
      COPY_HARD_REG_SET (conflict_regs[i],
			 OBJECT_TOTAL_CONFLICT_HARD_REGS (obj));
    }
  if (retry_p)
    {
      COPY_HARD_REG_SET (*start_profitable_regs,
			 reg_class_contents[ALLOCNO_CLASS (a)]);
      AND_COMPL_HARD_REG_SET (*start_profitable_regs,
			      ira_prohibited_class_mode_regs
			      [ALLOCNO_CLASS (a)][ALLOCNO_MODE (a)]);
    }
  else
    COPY_HARD_REG_SET (*start_profitable_regs,
		       ALLOCNO_COLOR_DATA (a)->profitable_hard_regs);
}

/* Return true if HARD_REGNO is ok for assigning to allocno A with
   PROFITABLE_REGS and whose objects have CONFLICT_REGS.  */
static inline bool
check_hard_reg_p (ira_allocno_t a, int hard_regno,
		  HARD_REG_SET *conflict_regs, HARD_REG_SET profitable_regs)
{
  int j, nwords, nregs;
  enum reg_class aclass;
  enum machine_mode mode;

  aclass = ALLOCNO_CLASS (a);
  mode = ALLOCNO_MODE (a);
  if (TEST_HARD_REG_BIT (ira_prohibited_class_mode_regs[aclass][mode],
			 hard_regno))
    return false;
  /* Checking only profitable hard regs.  */
  if (! TEST_HARD_REG_BIT (profitable_regs, hard_regno))
    return false;
  nregs = hard_regno_nregs[hard_regno][mode];
  nwords = ALLOCNO_NUM_OBJECTS (a);
  for (j = 0; j < nregs; j++)
    {
      int k;
      int set_to_test_start = 0, set_to_test_end = nwords;
      
      if (nregs == nwords)
	{
	  if (REG_WORDS_BIG_ENDIAN)
	    set_to_test_start = nwords - j - 1;
	  else
	    set_to_test_start = j;
	  set_to_test_end = set_to_test_start + 1;
	}
      for (k = set_to_test_start; k < set_to_test_end; k++)
	if (TEST_HARD_REG_BIT (conflict_regs[k], hard_regno + j))
	  break;
      if (k != set_to_test_end)
	break;
    }
  return j == nregs;
}
#ifndef HONOR_REG_ALLOC_ORDER

/* Return number of registers needed to be saved and restored at
   function prologue/epilogue if we allocate HARD_REGNO to hold value
   of MODE.  */
static int
calculate_saved_nregs (int hard_regno, enum machine_mode mode)
{
  int i;
  int nregs = 0;

  ira_assert (hard_regno >= 0);
  for (i = hard_regno_nregs[hard_regno][mode] - 1; i >= 0; i--)
    if (!allocated_hardreg_p[hard_regno + i]
	&& !TEST_HARD_REG_BIT (call_used_reg_set, hard_regno + i)
	&& !LOCAL_REGNO (hard_regno + i))
      nregs++;
  return nregs;
}
#endif

/* Choose a hard register for allocno A.  If RETRY_P is TRUE, it means
   that the function called from function
   `ira_reassign_conflict_allocnos' and `allocno_reload_assign'.  In
   this case some allocno data are not defined or updated and we
   should not touch these data.  The function returns true if we
   managed to assign a hard register to the allocno.

   To assign a hard register, first of all we calculate all conflict
   hard registers which can come from conflicting allocnos with
   already assigned hard registers.  After that we find first free
   hard register with the minimal cost.  During hard register cost
   calculation we take conflict hard register costs into account to
   give a chance for conflicting allocnos to get a better hard
   register in the future.

   If the best hard register cost is bigger than cost of memory usage
   for the allocno, we don't assign a hard register to given allocno
   at all.

   If we assign a hard register to the allocno, we update costs of the
   hard register for allocnos connected by copies to improve a chance
   to coalesce insns represented by the copies when we assign hard
   registers to the allocnos connected by the copies.  */
static bool
assign_hard_reg (ira_allocno_t a, bool retry_p)
{
  HARD_REG_SET conflicting_regs[2], profitable_hard_regs;
  int i, j, hard_regno, best_hard_regno, class_size;
  int cost, mem_cost, min_cost, full_cost, min_full_cost, nwords, word;
  int *a_costs;
  enum reg_class aclass;
  enum machine_mode mode;
  static int costs[FIRST_PSEUDO_REGISTER], full_costs[FIRST_PSEUDO_REGISTER];
#ifndef HONOR_REG_ALLOC_ORDER
  int saved_nregs;
  enum reg_class rclass;
  int add_cost;
#endif
#ifdef STACK_REGS
  bool no_stack_reg_p;
#endif

  ira_assert (! ALLOCNO_ASSIGNED_P (a));
  get_conflict_and_start_profitable_regs (a, retry_p,
					  conflicting_regs,
					  &profitable_hard_regs);
  aclass = ALLOCNO_CLASS (a);
  class_size = ira_class_hard_regs_num[aclass];
  best_hard_regno = -1;
  memset (full_costs, 0, sizeof (int) * class_size);
  mem_cost = 0;
  memset (costs, 0, sizeof (int) * class_size);
  memset (full_costs, 0, sizeof (int) * class_size);
#ifdef STACK_REGS
  no_stack_reg_p = false;
#endif
  if (! retry_p)
    start_update_cost ();
  mem_cost += ALLOCNO_UPDATED_MEMORY_COST (a);
  
  ira_allocate_and_copy_costs (&ALLOCNO_UPDATED_HARD_REG_COSTS (a),
			       aclass, ALLOCNO_HARD_REG_COSTS (a));
  a_costs = ALLOCNO_UPDATED_HARD_REG_COSTS (a);
#ifdef STACK_REGS
  no_stack_reg_p = no_stack_reg_p || ALLOCNO_TOTAL_NO_STACK_REG_P (a);
#endif
  cost = ALLOCNO_UPDATED_CLASS_COST (a);
  for (i = 0; i < class_size; i++)
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
  nwords = ALLOCNO_NUM_OBJECTS (a);
  curr_allocno_process++;
  for (word = 0; word < nwords; word++)
    {
      ira_object_t conflict_obj;
      ira_object_t obj = ALLOCNO_OBJECT (a, word);
      ira_object_conflict_iterator oci;
      
      /* Take preferences of conflicting allocnos into account.  */
      FOR_EACH_OBJECT_CONFLICT (obj, conflict_obj, oci)
        {
	  ira_allocno_t conflict_a = OBJECT_ALLOCNO (conflict_obj);
	  enum reg_class conflict_aclass;

	  /* Reload can give another class so we need to check all
	     allocnos.  */
	  if (!retry_p
	      && (!bitmap_bit_p (consideration_allocno_bitmap,
				 ALLOCNO_NUM (conflict_a))
		  || ((!ALLOCNO_ASSIGNED_P (conflict_a)
		       || ALLOCNO_HARD_REGNO (conflict_a) < 0)
		      && !(hard_reg_set_intersect_p
			   (profitable_hard_regs,
			    ALLOCNO_COLOR_DATA
			    (conflict_a)->profitable_hard_regs)))))
	    continue;
	  conflict_aclass = ALLOCNO_CLASS (conflict_a);
	  ira_assert (ira_reg_classes_intersect_p
		      [aclass][conflict_aclass]);
	  if (ALLOCNO_ASSIGNED_P (conflict_a))
	    {
	      hard_regno = ALLOCNO_HARD_REGNO (conflict_a);
	      if (hard_regno >= 0
		  && (ira_hard_reg_set_intersection_p
		      (hard_regno, ALLOCNO_MODE (conflict_a),
		       reg_class_contents[aclass])))
		{
		  int n_objects = ALLOCNO_NUM_OBJECTS (conflict_a);
		  int conflict_nregs;

		  mode = ALLOCNO_MODE (conflict_a);
		  conflict_nregs = hard_regno_nregs[hard_regno][mode];
		  if (conflict_nregs == n_objects && conflict_nregs > 1)
		    {
		      int num = OBJECT_SUBWORD (conflict_obj);

		      if (REG_WORDS_BIG_ENDIAN)
			SET_HARD_REG_BIT (conflicting_regs[word],
					  hard_regno + n_objects - num - 1);
		      else
			SET_HARD_REG_BIT (conflicting_regs[word],
					  hard_regno + num);
		    }
		  else
		    IOR_HARD_REG_SET
		      (conflicting_regs[word],
		       ira_reg_mode_hard_regset[hard_regno][mode]);
		  if (hard_reg_set_subset_p (profitable_hard_regs,
					     conflicting_regs[word]))
		    goto fail;
		}
	    }
	  else if (! retry_p
		   && ! ALLOCNO_COLOR_DATA (conflict_a)->may_be_spilled_p
		   /* Don't process the conflict allocno twice.  */
		   && (ALLOCNO_COLOR_DATA (conflict_a)->last_process
		       != curr_allocno_process))
	    {
	      int k, *conflict_costs;
	      
	      ALLOCNO_COLOR_DATA (conflict_a)->last_process
		= curr_allocno_process;
	      ira_allocate_and_copy_costs
		(&ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS (conflict_a),
		 conflict_aclass,
		 ALLOCNO_CONFLICT_HARD_REG_COSTS (conflict_a));
	      conflict_costs
		= ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS (conflict_a);
	      if (conflict_costs != NULL)
		for (j = class_size - 1; j >= 0; j--)
		  {
		    hard_regno = ira_class_hard_regs[aclass][j];
		    ira_assert (hard_regno >= 0);
		    k = ira_class_hard_reg_index[conflict_aclass][hard_regno];
		    if (k < 0)
		      continue;
		    full_costs[j] -= conflict_costs[k];
		  }
	      queue_update_cost (conflict_a, COST_HOP_DIVISOR);
	    }
	}
    }
  if (! retry_p)
    /* Take into account preferences of allocnos connected by copies to
       the conflict allocnos.  */
    update_conflict_hard_regno_costs (full_costs, aclass, true);

  /* Take preferences of allocnos connected by copies into
     account.  */
  if (! retry_p)
    {
      start_update_cost ();
      queue_update_cost (a, COST_HOP_DIVISOR);
      update_conflict_hard_regno_costs (full_costs, aclass, false);
    }
  min_cost = min_full_cost = INT_MAX;
  /* We don't care about giving callee saved registers to allocnos no
     living through calls because call clobbered registers are
     allocated first (it is usual practice to put them first in
     REG_ALLOC_ORDER).  */
  mode = ALLOCNO_MODE (a);
  for (i = 0; i < class_size; i++)
    {
      hard_regno = ira_class_hard_regs[aclass][i];
#ifdef STACK_REGS
      if (no_stack_reg_p
	  && FIRST_STACK_REG <= hard_regno && hard_regno <= LAST_STACK_REG)
	continue;
#endif
      if (! check_hard_reg_p (a, hard_regno,
			      conflicting_regs, profitable_hard_regs))
	continue;
      cost = costs[i];
      full_cost = full_costs[i];
#ifndef HONOR_REG_ALLOC_ORDER
      if ((saved_nregs = calculate_saved_nregs (hard_regno, mode)) != 0)
	/* We need to save/restore the hard register in
	   epilogue/prologue.  Therefore we increase the cost.  */
	{
	  rclass = REGNO_REG_CLASS (hard_regno);
	  add_cost = ((ira_memory_move_cost[mode][rclass][0]
		       + ira_memory_move_cost[mode][rclass][1])
		      * saved_nregs / hard_regno_nregs[hard_regno][mode] - 1);
	  cost += add_cost;
	  full_cost += add_cost;
	}
#endif
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
  if (best_hard_regno >= 0)
    {
      for (i = hard_regno_nregs[best_hard_regno][mode] - 1; i >= 0; i--)
	allocated_hardreg_p[best_hard_regno + i] = true;
    }
  ALLOCNO_HARD_REGNO (a) = best_hard_regno;
  ALLOCNO_ASSIGNED_P (a) = true;
  if (best_hard_regno >= 0)
    update_copy_costs (a, true);
  ira_assert (ALLOCNO_CLASS (a) == aclass);
  /* We don't need updated costs anymore: */
  ira_free_allocno_updated_costs (a);
  return best_hard_regno >= 0;
}



/* This page contains the allocator based on the Chaitin-Briggs algorithm.  */

/* Bucket of allocnos that can colored currently without spilling.  */
static ira_allocno_t colorable_allocno_bucket;

/* Bucket of allocnos that might be not colored currently without
   spilling.  */
static ira_allocno_t uncolorable_allocno_bucket;

/* The current number of allocnos in the uncolorable_bucket.  */
static int uncolorable_allocnos_num;

/* Return the current spill priority of allocno A.  The less the
   number, the more preferable the allocno for spilling.  */
static inline int
allocno_spill_priority (ira_allocno_t a)
{
  allocno_color_data_t data = ALLOCNO_COLOR_DATA (a);

  return (data->temp
	  / (ALLOCNO_EXCESS_PRESSURE_POINTS_NUM (a)
	     * ira_reg_class_max_nregs[ALLOCNO_CLASS (a)][ALLOCNO_MODE (a)]
	     + 1));
}

/* Add allocno A to bucket *BUCKET_PTR.  A should be not in a bucket
   before the call.  */
static void
add_allocno_to_bucket (ira_allocno_t a, ira_allocno_t *bucket_ptr)
{
  ira_allocno_t first_a;
  allocno_color_data_t data;

  if (bucket_ptr == &uncolorable_allocno_bucket
      && ALLOCNO_CLASS (a) != NO_REGS)
    {
      uncolorable_allocnos_num++;
      ira_assert (uncolorable_allocnos_num > 0);
    }
  first_a = *bucket_ptr;
  data = ALLOCNO_COLOR_DATA (a);
  data->next_bucket_allocno = first_a;
  data->prev_bucket_allocno = NULL;
  if (first_a != NULL)
    ALLOCNO_COLOR_DATA (first_a)->prev_bucket_allocno = a;
  *bucket_ptr = a;
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
  int cl1 = ALLOCNO_CLASS (a1), cl2 = ALLOCNO_CLASS (a2);

  /* Push pseudos requiring less hard registers first.  It means that
     we will assign pseudos requiring more hard registers first
     avoiding creation small holes in free hard register file into
     which the pseudos requiring more hard registers can not fit.  */
  if ((diff = (ira_reg_class_max_nregs[cl1][ALLOCNO_MODE (a1)]
	       - ira_reg_class_max_nregs[cl2][ALLOCNO_MODE (a2)])) != 0)
    return diff;
  a1_freq = ALLOCNO_FREQ (a1);
  a2_freq = ALLOCNO_FREQ (a2);
  if ((diff = a1_freq - a2_freq) != 0)
    return diff;
  a1_num = ALLOCNO_COLOR_DATA (a1)->available_regs_num;
  a2_num = ALLOCNO_COLOR_DATA (a2)->available_regs_num;
  if ((diff = a2_num - a1_num) != 0)
    return diff;
  return ALLOCNO_NUM (a2) - ALLOCNO_NUM (a1);
}

/* Sort bucket *BUCKET_PTR and return the result through
   BUCKET_PTR.  */
static void
sort_bucket (ira_allocno_t *bucket_ptr,
	     int (*compare_func) (const void *, const void *))
{
  ira_allocno_t a, head;
  int n;

  for (n = 0, a = *bucket_ptr;
       a != NULL;
       a = ALLOCNO_COLOR_DATA (a)->next_bucket_allocno)
    sorted_allocnos[n++] = a;
  if (n <= 1)
    return;
  qsort (sorted_allocnos, n, sizeof (ira_allocno_t), compare_func);
  head = NULL;
  for (n--; n >= 0; n--)
    {
      a = sorted_allocnos[n];
      ALLOCNO_COLOR_DATA (a)->next_bucket_allocno = head;
      ALLOCNO_COLOR_DATA (a)->prev_bucket_allocno = NULL;
      if (head != NULL)
	ALLOCNO_COLOR_DATA (head)->prev_bucket_allocno = a;
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

  if (bucket_ptr == &uncolorable_allocno_bucket
      && ALLOCNO_CLASS (allocno) != NO_REGS)
    {
      uncolorable_allocnos_num++;
      ira_assert (uncolorable_allocnos_num > 0);
    }
  for (before = *bucket_ptr, after = NULL;
       before != NULL;
       after = before,
	 before = ALLOCNO_COLOR_DATA (before)->next_bucket_allocno)
    if (bucket_allocno_compare_func (&allocno, &before) < 0)
      break;
  ALLOCNO_COLOR_DATA (allocno)->next_bucket_allocno = before;
  ALLOCNO_COLOR_DATA (allocno)->prev_bucket_allocno = after;
  if (after == NULL)
    *bucket_ptr = allocno;
  else
    ALLOCNO_COLOR_DATA (after)->next_bucket_allocno = allocno;
  if (before != NULL)
    ALLOCNO_COLOR_DATA (before)->prev_bucket_allocno = allocno;
}

/* Delete ALLOCNO from bucket *BUCKET_PTR.  It should be there before
   the call.  */
static void
delete_allocno_from_bucket (ira_allocno_t allocno, ira_allocno_t *bucket_ptr)
{
  ira_allocno_t prev_allocno, next_allocno;

  if (bucket_ptr == &uncolorable_allocno_bucket
      && ALLOCNO_CLASS (allocno) != NO_REGS)
    {
      uncolorable_allocnos_num--;
      ira_assert (uncolorable_allocnos_num >= 0);
    }
  prev_allocno = ALLOCNO_COLOR_DATA (allocno)->prev_bucket_allocno;
  next_allocno = ALLOCNO_COLOR_DATA (allocno)->next_bucket_allocno;
  if (prev_allocno != NULL)
    ALLOCNO_COLOR_DATA (prev_allocno)->next_bucket_allocno = next_allocno;
  else
    {
      ira_assert (*bucket_ptr == allocno);
      *bucket_ptr = next_allocno;
    }
  if (next_allocno != NULL)
    ALLOCNO_COLOR_DATA (next_allocno)->prev_bucket_allocno = prev_allocno;
}

/* Put allocno A onto the coloring stack without removing it from its
   bucket.  Pushing allocno to the coloring stack can result in moving
   conflicting allocnos from the uncolorable bucket to the colorable
   one.  */
static void
push_allocno_to_stack (ira_allocno_t a)
{
  enum reg_class aclass;
  allocno_color_data_t data, conflict_data;
  int size, i, n = ALLOCNO_NUM_OBJECTS (a);
    
  data = ALLOCNO_COLOR_DATA (a);
  data->in_graph_p = false;
  VEC_safe_push (ira_allocno_t, heap, allocno_stack_vec, a);
  aclass = ALLOCNO_CLASS (a);
  if (aclass == NO_REGS)
    return;
  size = ira_reg_class_max_nregs[aclass][ALLOCNO_MODE (a)];
  if (n > 1)
    {
      /* We will deal with the subwords individually.  */
      gcc_assert (size == ALLOCNO_NUM_OBJECTS (a));
      size = 1;
    }
  for (i = 0; i < n; i++)
    {
      ira_object_t obj = ALLOCNO_OBJECT (a, i);
      ira_object_t conflict_obj;
      ira_object_conflict_iterator oci;
      
      FOR_EACH_OBJECT_CONFLICT (obj, conflict_obj, oci)
	{
	  ira_allocno_t conflict_a = OBJECT_ALLOCNO (conflict_obj);
	  
	  conflict_data = ALLOCNO_COLOR_DATA (conflict_a);
	  if (conflict_data->colorable_p
	      || ! conflict_data->in_graph_p
	      || ALLOCNO_ASSIGNED_P (conflict_a)
	      || !(hard_reg_set_intersect_p
		   (ALLOCNO_COLOR_DATA (a)->profitable_hard_regs,
		    conflict_data->profitable_hard_regs)))
	    continue;
	  ira_assert (bitmap_bit_p (coloring_allocno_bitmap,
				    ALLOCNO_NUM (conflict_a)));
	  if (update_left_conflict_sizes_p (conflict_a, a, size))
	    {
	      delete_allocno_from_bucket
		(conflict_a, &uncolorable_allocno_bucket);
	      add_allocno_to_ordered_bucket
		(conflict_a, &colorable_allocno_bucket);
	      if (internal_flag_ira_verbose > 4 && ira_dump_file != NULL)
		{
		  fprintf (ira_dump_file, "        Making");
		  ira_print_expanded_allocno (conflict_a);
		  fprintf (ira_dump_file, " colorable\n");
		}
	    }
	  
	}
    }
}

/* Put ALLOCNO onto the coloring stack and remove it from its bucket.
   The allocno is in the colorable bucket if COLORABLE_P is TRUE.  */
static void
remove_allocno_from_bucket_and_push (ira_allocno_t allocno, bool colorable_p)
{
  if (colorable_p)
    delete_allocno_from_bucket (allocno, &colorable_allocno_bucket);
  else
    delete_allocno_from_bucket (allocno, &uncolorable_allocno_bucket);
  if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
    {
      fprintf (ira_dump_file, "      Pushing");
      ira_print_expanded_allocno (allocno);
      if (colorable_p)
	fprintf (ira_dump_file, "(cost %d)\n",
		 ALLOCNO_COLOR_DATA (allocno)->temp);
      else
	fprintf (ira_dump_file, "(potential spill: %spri=%d, cost=%d)\n",
		 ALLOCNO_BAD_SPILL_P (allocno) ? "bad spill, " : "",
		 allocno_spill_priority (allocno),
		 ALLOCNO_COLOR_DATA (allocno)->temp);
    }
  if (! colorable_p)
    ALLOCNO_COLOR_DATA (allocno)->may_be_spilled_p = true;
  push_allocno_to_stack (allocno);
}

/* Put all allocnos from colorable bucket onto the coloring stack.  */
static void
push_only_colorable (void)
{
  sort_bucket (&colorable_allocno_bucket, bucket_allocno_compare_func);
  for (;colorable_allocno_bucket != NULL;)
    remove_allocno_from_bucket_and_push (colorable_allocno_bucket, true);
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

  ira_assert (current_loops != NULL && loop_node->loop != NULL
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
      FOR_EACH_VEC_ELT (edge, edges, i, e)
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
  cost = ALLOCNO_UPDATED_MEMORY_COST (a) - ALLOCNO_UPDATED_CLASS_COST (a);
  if (ALLOCNO_CAP (a) != NULL)
    return cost;
  loop_node = ALLOCNO_LOOP_TREE_NODE (a);
  if ((parent_node = loop_node->parent) == NULL)
    return cost;
  if ((parent_allocno = parent_node->regno_allocno_map[regno]) == NULL)
    return cost;
  mode = ALLOCNO_MODE (a);
  rclass = ALLOCNO_CLASS (a);
  if (ALLOCNO_HARD_REGNO (parent_allocno) < 0)
    cost -= (ira_memory_move_cost[mode][rclass][0]
	     * ira_loop_edge_freq (loop_node, regno, true)
	     + ira_memory_move_cost[mode][rclass][1]
	     * ira_loop_edge_freq (loop_node, regno, false));
  else
    {
      ira_init_register_move_cost_if_necessary (mode);
      cost += ((ira_memory_move_cost[mode][rclass][1]
		* ira_loop_edge_freq (loop_node, regno, true)
		+ ira_memory_move_cost[mode][rclass][0]
		* ira_loop_edge_freq (loop_node, regno, false))
	       - (ira_register_move_cost[mode][rclass][rclass]
		  * (ira_loop_edge_freq (loop_node, regno, false)
		     + ira_loop_edge_freq (loop_node, regno, true))));
    }
  return cost;
}

/* Used for sorting allocnos for spilling.  */
static inline int
allocno_spill_priority_compare (ira_allocno_t a1, ira_allocno_t a2)
{
  int pri1, pri2, diff;

  if (ALLOCNO_BAD_SPILL_P (a1) && ! ALLOCNO_BAD_SPILL_P (a2))
    return 1;
  if (ALLOCNO_BAD_SPILL_P (a2) && ! ALLOCNO_BAD_SPILL_P (a1))
    return -1;
  pri1 = allocno_spill_priority (a1);
  pri2 = allocno_spill_priority (a2);
  if ((diff = pri1 - pri2) != 0)
    return diff;
  if ((diff
       = ALLOCNO_COLOR_DATA (a1)->temp - ALLOCNO_COLOR_DATA (a2)->temp) != 0)
    return diff;
  return ALLOCNO_NUM (a1) - ALLOCNO_NUM (a2);
}

/* Used for sorting allocnos for spilling.  */
static int
allocno_spill_sort_compare (const void *v1p, const void *v2p)
{
  ira_allocno_t p1 = *(const ira_allocno_t *) v1p;
  ira_allocno_t p2 = *(const ira_allocno_t *) v2p;

  return allocno_spill_priority_compare (p1, p2);
}

/* Push allocnos to the coloring stack.  The order of allocnos in the
   stack defines the order for the subsequent coloring.  */
static void
push_allocnos_to_stack (void)
{
  ira_allocno_t a;
  int cost;

  /* Calculate uncolorable allocno spill costs.  */
  for (a = uncolorable_allocno_bucket;
       a != NULL;
       a = ALLOCNO_COLOR_DATA (a)->next_bucket_allocno)
    if (ALLOCNO_CLASS (a) != NO_REGS)
      {
	cost = calculate_allocno_spill_cost (a);
	/* ??? Remove cost of copies between the coalesced
	   allocnos.  */
	ALLOCNO_COLOR_DATA (a)->temp = cost;
      }
  sort_bucket (&uncolorable_allocno_bucket, allocno_spill_sort_compare);
  for (;;)
    {
      push_only_colorable ();
      a = uncolorable_allocno_bucket;
      if (a == NULL)
	break;
      remove_allocno_from_bucket_and_push (a, false);
    }
  ira_assert (colorable_allocno_bucket == NULL
	      && uncolorable_allocno_bucket == NULL);
  ira_assert (uncolorable_allocnos_num == 0);
}

/* Pop the coloring stack and assign hard registers to the popped
   allocnos.  */
static void
pop_allocnos_from_stack (void)
{
  ira_allocno_t allocno;
  enum reg_class aclass;

  for (;VEC_length (ira_allocno_t, allocno_stack_vec) != 0;)
    {
      allocno = VEC_pop (ira_allocno_t, allocno_stack_vec);
      aclass = ALLOCNO_CLASS (allocno);
      if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
	{
	  fprintf (ira_dump_file, "      Popping");
	  ira_print_expanded_allocno (allocno);
	  fprintf (ira_dump_file, "  -- ");
	}
      if (aclass == NO_REGS)
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
      ALLOCNO_COLOR_DATA (allocno)->in_graph_p = true;
    }
}

/* Set up number of available hard registers for allocno A.  */
static void
setup_allocno_available_regs_num (ira_allocno_t a)
{
  int i, n, hard_regno, hard_regs_num, nwords;
  enum reg_class aclass;
  allocno_color_data_t data;

  aclass = ALLOCNO_CLASS (a);
  data = ALLOCNO_COLOR_DATA (a);
  data->available_regs_num = 0;
  if (aclass == NO_REGS)
    return;
  hard_regs_num = ira_class_hard_regs_num[aclass];
  nwords = ALLOCNO_NUM_OBJECTS (a);
  for (n = 0, i = hard_regs_num - 1; i >= 0; i--)
    {
      hard_regno = ira_class_hard_regs[aclass][i];
      /* Checking only profitable hard regs.  */
      if (TEST_HARD_REG_BIT (data->profitable_hard_regs, hard_regno))
	n++;
    }
  data->available_regs_num = n;
  if (internal_flag_ira_verbose <= 2 || ira_dump_file == NULL)
    return;
  fprintf
    (ira_dump_file,
     "      Allocno a%dr%d of %s(%d) has %d avail. regs ",
     ALLOCNO_NUM (a), ALLOCNO_REGNO (a),
     reg_class_names[aclass], ira_class_hard_regs_num[aclass], n);
  print_hard_reg_set (ira_dump_file, data->profitable_hard_regs, false);
  fprintf (ira_dump_file, ", %snode: ",
	   hard_reg_set_equal_p (data->profitable_hard_regs,
				 data->hard_regs_node->hard_regs->set)
	   ? "" : "^");
  print_hard_reg_set (ira_dump_file,
		      data->hard_regs_node->hard_regs->set, false);
  for (i = 0; i < nwords; i++)
    {
      ira_object_t obj = ALLOCNO_OBJECT (a, i);

      if (nwords != 1)
	{
	  if (i != 0)
	    fprintf (ira_dump_file, ", ");
	  fprintf (ira_dump_file, " obj %d", i);
	}
      fprintf (ira_dump_file, " (confl regs = ");
      print_hard_reg_set (ira_dump_file, OBJECT_TOTAL_CONFLICT_HARD_REGS (obj),
			  false);
      fprintf (ira_dump_file, ")");
    }
  fprintf (ira_dump_file, "\n");
}

/* Put ALLOCNO in a bucket corresponding to its number and size of its
   conflicting allocnos and hard registers.  */
static void
put_allocno_into_bucket (ira_allocno_t allocno)
{
  ALLOCNO_COLOR_DATA (allocno)->in_graph_p = true;
  setup_allocno_available_regs_num (allocno);
  if (setup_left_conflict_sizes_p (allocno))
    add_allocno_to_bucket (allocno, &colorable_allocno_bucket);
  else
    add_allocno_to_bucket (allocno, &uncolorable_allocno_bucket);
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
	   * (ALLOCNO_MEMORY_COST (a) - ALLOCNO_CLASS_COST (a))
	   * ira_reg_class_max_nregs[ALLOCNO_CLASS (a)][ALLOCNO_MODE (a)]);
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
      if (ALLOCNO_NUM_OBJECTS (a) > 1)
	length /= ALLOCNO_NUM_OBJECTS (a);
      if (length <= 0)
	length = 1;
      allocno_priorities[ALLOCNO_NUM (a)]
	= allocno_priorities[ALLOCNO_NUM (a)] * mult / length;
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

  c1 = ALLOCNO_UPDATED_MEMORY_COST (p1) - ALLOCNO_UPDATED_CLASS_COST (p1);
  c2 = ALLOCNO_UPDATED_MEMORY_COST (p2) - ALLOCNO_UPDATED_CLASS_COST (p2);
  if (c1 - c2)
    return c1 - c2;

  /* If regs are equally good, sort by allocno numbers, so that the
     results of qsort leave nothing to chance.  */
  return ALLOCNO_NUM (p1) - ALLOCNO_NUM (p2);
}

/* We used Chaitin-Briggs coloring to assign as many pseudos as
   possible to hard registers.  Let us try to improve allocation with
   cost point of view.  This function improves the allocation by
   spilling some allocnos and assigning the freed hard registers to
   other allocnos if it decreases the overall allocation cost.  */
static void
improve_allocation (void)
{
  unsigned int i;
  int j, k, n, hregno, conflict_hregno, base_cost, class_size, word, nwords;
  int check, spill_cost, min_cost, nregs, conflict_nregs, r, best;
  bool try_p;
  enum reg_class aclass;
  enum machine_mode mode;
  int *allocno_costs;
  int costs[FIRST_PSEUDO_REGISTER];
  HARD_REG_SET conflicting_regs[2], profitable_hard_regs;
  ira_allocno_t a;
  bitmap_iterator bi;

  /* Clear counts used to process conflicting allocnos only once for
     each allocno.  */
  EXECUTE_IF_SET_IN_BITMAP (coloring_allocno_bitmap, 0, i, bi)
    ALLOCNO_COLOR_DATA (ira_allocnos[i])->temp = 0;
  check = n = 0;
  /* Process each allocno and try to assign a hard register to it by
     spilling some its conflicting allocnos.  */
  EXECUTE_IF_SET_IN_BITMAP (coloring_allocno_bitmap, 0, i, bi)
    {
      a = ira_allocnos[i];
      ALLOCNO_COLOR_DATA (a)->temp = 0;
      if (empty_profitable_hard_regs (a))
	continue;
      check++;
      aclass = ALLOCNO_CLASS (a);
      allocno_costs = ALLOCNO_UPDATED_HARD_REG_COSTS (a);
      if (allocno_costs == NULL)
	allocno_costs = ALLOCNO_HARD_REG_COSTS (a);
      if ((hregno = ALLOCNO_HARD_REGNO (a)) < 0)
	base_cost = ALLOCNO_UPDATED_MEMORY_COST (a);
      else if (allocno_costs == NULL)
	/* It means that assigning a hard register is not profitable
	   (we don't waste memory for hard register costs in this
	   case).  */
	continue;
      else
	base_cost = allocno_costs[ira_class_hard_reg_index[aclass][hregno]];
      try_p = false;
      get_conflict_and_start_profitable_regs (a, false,
					      conflicting_regs,
					      &profitable_hard_regs);
      class_size = ira_class_hard_regs_num[aclass];
      /* Set up cost improvement for usage of each profitable hard
	 register for allocno A.  */
      for (j = 0; j < class_size; j++)
	{
	  hregno = ira_class_hard_regs[aclass][j];
	  if (! check_hard_reg_p (a, hregno,
				  conflicting_regs, profitable_hard_regs))
	    continue;
	  ira_assert (ira_class_hard_reg_index[aclass][hregno] == j);
	  k = allocno_costs == NULL ? 0 : j;
	  costs[hregno] = (allocno_costs == NULL
			   ? ALLOCNO_UPDATED_CLASS_COST (a) : allocno_costs[k]);
	  costs[hregno] -= base_cost;
	  if (costs[hregno] < 0)
	    try_p = true;
	}
      if (! try_p)
	/* There is no chance to improve the allocation cost by
	   assigning hard register to allocno A even without spilling
	   conflicting allocnos.  */
	continue;
      mode = ALLOCNO_MODE (a);
      nwords = ALLOCNO_NUM_OBJECTS (a);
      /* Process each allocno conflicting with A and update the cost
	 improvement for profitable hard registers of A.  To use a
	 hard register for A we need to spill some conflicting
	 allocnos and that creates penalty for the cost
	 improvement.  */
      for (word = 0; word < nwords; word++)
	{
	  ira_object_t conflict_obj;
	  ira_object_t obj = ALLOCNO_OBJECT (a, word);
	  ira_object_conflict_iterator oci;
      
	  FOR_EACH_OBJECT_CONFLICT (obj, conflict_obj, oci)
	    {
	      ira_allocno_t conflict_a = OBJECT_ALLOCNO (conflict_obj);

	      if (ALLOCNO_COLOR_DATA (conflict_a)->temp == check)
		/* We already processed this conflicting allocno
		   because we processed earlier another object of the
		   conflicting allocno.  */
		continue;
	      ALLOCNO_COLOR_DATA (conflict_a)->temp = check;
	      if ((conflict_hregno = ALLOCNO_HARD_REGNO (conflict_a)) < 0)
		continue;
	      spill_cost = ALLOCNO_UPDATED_MEMORY_COST (conflict_a);
	      k = (ira_class_hard_reg_index
		   [ALLOCNO_CLASS (conflict_a)][conflict_hregno]);
	      ira_assert (k >= 0);
	      if ((allocno_costs = ALLOCNO_UPDATED_HARD_REG_COSTS (conflict_a))
		  != NULL)
		spill_cost -= allocno_costs[k];
	      else if ((allocno_costs = ALLOCNO_HARD_REG_COSTS (conflict_a))
		       != NULL)
		spill_cost -= allocno_costs[k];
	      else
		spill_cost -= ALLOCNO_UPDATED_CLASS_COST (conflict_a);
	      conflict_nregs
		= hard_regno_nregs[conflict_hregno][ALLOCNO_MODE (conflict_a)];
	      for (r = conflict_hregno;
		   r >= 0 && r + hard_regno_nregs[r][mode] > conflict_hregno;
		   r--)
		if (check_hard_reg_p (a, r,
				      conflicting_regs, profitable_hard_regs))
		  costs[r] += spill_cost;
	      for (r = conflict_hregno + 1;
		   r < conflict_hregno + conflict_nregs;
		   r++)
		if (check_hard_reg_p (a, r,
				      conflicting_regs, profitable_hard_regs))
		  costs[r] += spill_cost;
	    }
	}
      min_cost = INT_MAX;
      best = -1;
      /* Now we choose hard register for A which results in highest
	 allocation cost improvement.  */
      for (j = 0; j < class_size; j++)
	{
	  hregno = ira_class_hard_regs[aclass][j];
	  if (check_hard_reg_p (a, hregno,
				conflicting_regs, profitable_hard_regs)
	      && min_cost > costs[hregno])
	    {
	      best = hregno;
	      min_cost = costs[hregno];
	    }
	}
      if (min_cost >= 0)
	/* We are in a situation when assigning any hard register to A
	   by spilling some conflicting allocnos does not improve the
	   allocation cost.  */
	continue;
      nregs = hard_regno_nregs[best][mode];
      /* Now spill conflicting allocnos which contain a hard register
	 of A when we assign the best chosen hard register to it.  */
      for (word = 0; word < nwords; word++)
	{
	  ira_object_t conflict_obj;
	  ira_object_t obj = ALLOCNO_OBJECT (a, word);
	  ira_object_conflict_iterator oci;
      
	  FOR_EACH_OBJECT_CONFLICT (obj, conflict_obj, oci)
	    {
	      ira_allocno_t conflict_a = OBJECT_ALLOCNO (conflict_obj);

	      if ((conflict_hregno = ALLOCNO_HARD_REGNO (conflict_a)) < 0)
		continue;
	      conflict_nregs
		= hard_regno_nregs[conflict_hregno][ALLOCNO_MODE (conflict_a)];
	      if (best + nregs <= conflict_hregno
		  || conflict_hregno + conflict_nregs <= best)
		/* No intersection.  */
		continue;
	      ALLOCNO_HARD_REGNO (conflict_a) = -1;
	      sorted_allocnos[n++] = conflict_a;
	      if (internal_flag_ira_verbose > 2 && ira_dump_file != NULL)
		fprintf (ira_dump_file, "Spilling a%dr%d for a%dr%d\n",
			 ALLOCNO_NUM (conflict_a), ALLOCNO_REGNO (conflict_a),
			 ALLOCNO_NUM (a), ALLOCNO_REGNO (a));
	    }
	}
      /* Assign the best chosen hard register to A.  */
      ALLOCNO_HARD_REGNO (a) = best;
      if (internal_flag_ira_verbose > 2 && ira_dump_file != NULL)
	fprintf (ira_dump_file, "Assigning %d to a%dr%d\n",
		 best, ALLOCNO_NUM (a), ALLOCNO_REGNO (a));
    }
  if (n == 0)
    return;
  /* We spilled some allocnos to assign their hard registers to other
     allocnos.  The spilled allocnos are now in array
     'sorted_allocnos'.  There is still a possibility that some of the
     spilled allocnos can get hard registers.  So let us try assign
     them hard registers again (just a reminder -- function
     'assign_hard_reg' assigns hard registers only if it is possible
     and profitable).  We process the spilled allocnos with biggest
     benefit to get hard register first -- see function
     'allocno_cost_compare_func'.  */
  qsort (sorted_allocnos, n, sizeof (ira_allocno_t),
	 allocno_cost_compare_func);
  for (j = 0; j < n; j++)
    {
      a = sorted_allocnos[j];
      ALLOCNO_ASSIGNED_P (a) = false;
      if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
	{
	  fprintf (ira_dump_file, "      ");
	  ira_print_expanded_allocno (a);
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

  setup_profitable_hard_regs ();
  if (flag_ira_algorithm == IRA_ALGORITHM_PRIORITY)
    {
      n = 0;
      EXECUTE_IF_SET_IN_BITMAP (coloring_allocno_bitmap, 0, i, bi)
	{
	  a = ira_allocnos[i];
	  if (ALLOCNO_CLASS (a) == NO_REGS)
	    {
	      ALLOCNO_HARD_REGNO (a) = -1;
	      ALLOCNO_ASSIGNED_P (a) = true;
	      ira_assert (ALLOCNO_UPDATED_HARD_REG_COSTS (a) == NULL);
	      ira_assert (ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS (a) == NULL);
	      if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
		{
		  fprintf (ira_dump_file, "      Spill");
		  ira_print_expanded_allocno (a);
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
		  ira_print_expanded_allocno (a);
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
      form_allocno_hard_regs_nodes_forest ();
      if (internal_flag_ira_verbose > 2 && ira_dump_file != NULL)
	print_hard_regs_forest (ira_dump_file);
      EXECUTE_IF_SET_IN_BITMAP (coloring_allocno_bitmap, 0, i, bi)
	{
	  a = ira_allocnos[i];
	  if (ALLOCNO_CLASS (a) != NO_REGS && ! empty_profitable_hard_regs (a))
	    ALLOCNO_COLOR_DATA (a)->in_graph_p = true;
	  else
	    {
	      ALLOCNO_HARD_REGNO (a) = -1;
	      ALLOCNO_ASSIGNED_P (a) = true;
	      /* We don't need updated costs anymore.  */
	      ira_free_allocno_updated_costs (a);
	      if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
		{
		  fprintf (ira_dump_file, "      Spill");
		  ira_print_expanded_allocno (a);
		  fprintf (ira_dump_file, "\n");
		}
	    }
	}
      /* Put the allocnos into the corresponding buckets.  */
      colorable_allocno_bucket = NULL;
      uncolorable_allocno_bucket = NULL;
      EXECUTE_IF_SET_IN_BITMAP (coloring_allocno_bitmap, 0, i, bi)
	{
	  a = ira_allocnos[i];
	  if (ALLOCNO_COLOR_DATA (a)->in_graph_p)
	    put_allocno_into_bucket (a);
	}
      push_allocnos_to_stack ();
      pop_allocnos_from_stack ();
      finish_allocno_hard_regs_nodes_forest ();
    }
  improve_allocation ();
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

  if (loop_tree_node->parent == NULL)
    fprintf (ira_dump_file,
	     "\n  Loop 0 (parent -1, header bb%d, depth 0)\n    bbs:",
	     NUM_FIXED_BLOCKS);
  else
    {
      ira_assert (current_loops != NULL && loop_tree_node->loop != NULL);
      fprintf (ira_dump_file,
	       "\n  Loop %d (parent %d, header bb%d, depth %d)\n    bbs:",
	       loop_tree_node->loop_num, loop_tree_node->parent->loop_num,
	       loop_tree_node->loop->header->index,
	       loop_depth (loop_tree_node->loop));
    }
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
		     e->dest->index, dest_loop_node->loop_num);
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
  for (j = 0; (int) j < ira_pressure_classes_num; j++)
    {
      enum reg_class pclass;

      pclass = ira_pressure_classes[j];
      if (loop_tree_node->reg_pressure[pclass] == 0)
	continue;
      fprintf (ira_dump_file, " %s=%d", reg_class_names[pclass],
	       loop_tree_node->reg_pressure[pclass]);
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
  int regno, hard_regno, index = -1, n;
  int cost, exit_freq, enter_freq;
  unsigned int j;
  bitmap_iterator bi;
  enum machine_mode mode;
  enum reg_class rclass, aclass, pclass;
  ira_allocno_t a, subloop_allocno;
  ira_loop_tree_node_t subloop_node;

  ira_assert (loop_tree_node->bb == NULL);
  if (internal_flag_ira_verbose > 1 && ira_dump_file != NULL)
    print_loop_title (loop_tree_node);

  bitmap_copy (coloring_allocno_bitmap, loop_tree_node->all_allocnos);
  bitmap_copy (consideration_allocno_bitmap, coloring_allocno_bitmap);
  n = 0;
  EXECUTE_IF_SET_IN_BITMAP (consideration_allocno_bitmap, 0, j, bi)
    {
      a = ira_allocnos[j];
      n++;
      if (! ALLOCNO_ASSIGNED_P (a))
	continue;
      bitmap_clear_bit (coloring_allocno_bitmap, ALLOCNO_NUM (a));
    }
  allocno_color_data
    = (allocno_color_data_t) ira_allocate (sizeof (struct allocno_color_data)
					   * n);
  memset (allocno_color_data, 0, sizeof (struct allocno_color_data) * n);
  curr_allocno_process = 0;
  n = 0;
  EXECUTE_IF_SET_IN_BITMAP (consideration_allocno_bitmap, 0, j, bi)
    {
      a = ira_allocnos[j];
      ALLOCNO_ADD_DATA (a) = allocno_color_data + n;
      n++;
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
	rclass = ALLOCNO_CLASS (a);
	pclass = ira_pressure_class_translate[rclass];
	if (flag_ira_region == IRA_REGION_MIXED
	    && (loop_tree_node->reg_pressure[pclass]
		<= ira_available_class_regs[pclass]))
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
	  rclass = ALLOCNO_CLASS (a);
	  pclass = ira_pressure_class_translate[rclass];
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
	  ira_assert (ALLOCNO_CLASS (subloop_allocno) == rclass);
	  ira_assert (bitmap_bit_p (subloop_node->all_allocnos,
				    ALLOCNO_NUM (subloop_allocno)));
	  if ((flag_ira_region == IRA_REGION_MIXED)
	      && (loop_tree_node->reg_pressure[pclass]
		  <= ira_available_class_regs[pclass]))
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
	      aclass = ALLOCNO_CLASS (subloop_allocno);
	      ira_init_register_move_cost_if_necessary (mode);
	      cost = (ira_register_move_cost[mode][rclass][rclass]
		      * (exit_freq + enter_freq));
	      ira_allocate_and_set_or_copy_costs
		(&ALLOCNO_UPDATED_HARD_REG_COSTS (subloop_allocno), aclass,
		 ALLOCNO_UPDATED_CLASS_COST (subloop_allocno),
		 ALLOCNO_HARD_REG_COSTS (subloop_allocno));
	      ira_allocate_and_set_or_copy_costs
		(&ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS (subloop_allocno),
		 aclass, 0, ALLOCNO_CONFLICT_HARD_REG_COSTS (subloop_allocno));
	      ALLOCNO_UPDATED_HARD_REG_COSTS (subloop_allocno)[index] -= cost;
	      ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS (subloop_allocno)[index]
		-= cost;
	      if (ALLOCNO_UPDATED_CLASS_COST (subloop_allocno)
		  > ALLOCNO_UPDATED_HARD_REG_COSTS (subloop_allocno)[index])
		ALLOCNO_UPDATED_CLASS_COST (subloop_allocno)
		  = ALLOCNO_UPDATED_HARD_REG_COSTS (subloop_allocno)[index];
	      ALLOCNO_UPDATED_MEMORY_COST (subloop_allocno)
		+= (ira_memory_move_cost[mode][rclass][0] * enter_freq
		    + ira_memory_move_cost[mode][rclass][1] * exit_freq);
	    }
	}
    }
  ira_free (allocno_color_data);
  EXECUTE_IF_SET_IN_BITMAP (coloring_allocno_bitmap, 0, j, bi)
    {
      a = ira_allocnos[j];
      ALLOCNO_ADD_DATA (a) = NULL;
    }
}

/* Initialize the common data for coloring and calls functions to do
   Chaitin-Briggs and regional coloring.  */
static void
do_coloring (void)
{
  coloring_allocno_bitmap = ira_allocate_bitmap ();
  if (internal_flag_ira_verbose > 0 && ira_dump_file != NULL)
    fprintf (ira_dump_file, "\n**** Allocnos coloring:\n\n");

  ira_traverse_loop_tree (false, ira_loop_tree_root, color_pass, NULL);

  if (internal_flag_ira_verbose > 1 && ira_dump_file != NULL)
    ira_print_disposition (ira_dump_file);

  ira_free_bitmap (coloring_allocno_bitmap);
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
	  rclass = ALLOCNO_CLASS (a);
	  index = ira_class_hard_reg_index[rclass][hard_regno];
	  ira_assert (index >= 0);
	  cost = (ALLOCNO_MEMORY_COST (a)
		  - (ALLOCNO_HARD_REG_COSTS (a) == NULL
		     ? ALLOCNO_CLASS_COST (a)
		     : ALLOCNO_HARD_REG_COSTS (a)[index]));
	  ira_init_register_move_cost_if_necessary (mode);
	  for (subloop_node = loop_node->subloops;
	       subloop_node != NULL;
	       subloop_node = subloop_node->subloop_next)
	    {
	      ira_assert (subloop_node->bb == NULL);
	      subloop_allocno = subloop_node->regno_allocno_map[regno];
	      if (subloop_allocno == NULL)
		continue;
	      ira_assert (rclass == ALLOCNO_CLASS (subloop_allocno));
	      /* We have accumulated cost.  To get the real cost of
		 allocno usage in the loop we should subtract costs of
		 the subloop allocnos.  */
	      cost -= (ALLOCNO_MEMORY_COST (subloop_allocno)
		       - (ALLOCNO_HARD_REG_COSTS (subloop_allocno) == NULL
			  ? ALLOCNO_CLASS_COST (subloop_allocno)
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
		    cost -= (ira_register_move_cost[mode][rclass][rclass]
			     * (exit_freq + enter_freq));
		}
	    }
	  if ((parent = loop_node->parent) != NULL
	      && (parent_allocno = parent->regno_allocno_map[regno]) != NULL)
	    {
	      ira_assert (rclass == ALLOCNO_CLASS (parent_allocno));
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
		    cost -= (ira_register_move_cost[mode][rclass][rclass]
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
		     ALLOCNO_NUM (a), regno, loop_node->loop_num);
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
  enum reg_class aclass, rclass;
  ira_allocno_t another_a;
  ira_copy_t cp, next_cp;

  ira_free_allocno_updated_costs (a);
  ira_assert (! ALLOCNO_ASSIGNED_P (a));
  aclass = ALLOCNO_CLASS (a);
  if (aclass == NO_REGS)
    return;
  mode = ALLOCNO_MODE (a);
  ira_init_register_move_cost_if_necessary (mode);
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
      if (! ira_reg_classes_intersect_p[aclass][ALLOCNO_CLASS (another_a)]
	  || ! ALLOCNO_ASSIGNED_P (another_a)
	  || (hard_regno = ALLOCNO_HARD_REGNO (another_a)) < 0)
	continue;
      rclass = REGNO_REG_CLASS (hard_regno);
      i = ira_class_hard_reg_index[aclass][hard_regno];
      if (i < 0)
	continue;
      cost = (cp->first == a
	      ? ira_register_move_cost[mode][rclass][aclass]
	      : ira_register_move_cost[mode][aclass][rclass]);
      ira_allocate_and_set_or_copy_costs
	(&ALLOCNO_UPDATED_HARD_REG_COSTS (a), aclass, ALLOCNO_CLASS_COST (a),
	 ALLOCNO_HARD_REG_COSTS (a));
      ira_allocate_and_set_or_copy_costs
	(&ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS (a),
	 aclass, 0, ALLOCNO_CONFLICT_HARD_REG_COSTS (a));
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
  ira_allocno_t a;
  enum reg_class aclass;
  bitmap allocnos_to_color;
  ira_allocno_iterator ai;

  allocnos_to_color = ira_allocate_bitmap ();
  allocnos_to_color_num = 0;
  FOR_EACH_ALLOCNO (a, ai)
    {
      int n = ALLOCNO_NUM_OBJECTS (a);

      if (! ALLOCNO_ASSIGNED_P (a)
	  && ! bitmap_bit_p (allocnos_to_color, ALLOCNO_NUM (a)))
	{
	  if (ALLOCNO_CLASS (a) != NO_REGS)
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
	  || (aclass = ALLOCNO_CLASS (a)) == NO_REGS)
	continue;
      for (i = 0; i < n; i++)
	{
	  ira_object_t obj = ALLOCNO_OBJECT (a, i);
	  ira_object_t conflict_obj;
	  ira_object_conflict_iterator oci;

	  FOR_EACH_OBJECT_CONFLICT (obj, conflict_obj, oci)
	    {
	      ira_allocno_t conflict_a = OBJECT_ALLOCNO (conflict_obj);

	      ira_assert (ira_reg_classes_intersect_p
			  [aclass][ALLOCNO_CLASS (conflict_a)]);
	      if (!bitmap_set_bit (allocnos_to_color, ALLOCNO_NUM (conflict_a)))
		continue;
	      sorted_allocnos[allocnos_to_color_num++] = conflict_a;
	    }
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



/* This page contains functions used to find conflicts using allocno
   live ranges.  */

/* Return TRUE if live ranges of allocnos A1 and A2 intersect.  It is
   used to find a conflict for new allocnos or allocnos with the
   different allocno classes.  */
static bool
allocnos_conflict_by_live_ranges_p (ira_allocno_t a1, ira_allocno_t a2)
{
  rtx reg1, reg2;
  int i, j;
  int n1 = ALLOCNO_NUM_OBJECTS (a1);
  int n2 = ALLOCNO_NUM_OBJECTS (a2);

  if (a1 == a2)
    return false;
  reg1 = regno_reg_rtx[ALLOCNO_REGNO (a1)];
  reg2 = regno_reg_rtx[ALLOCNO_REGNO (a2)];
  if (reg1 != NULL && reg2 != NULL
      && ORIGINAL_REGNO (reg1) == ORIGINAL_REGNO (reg2))
    return false;

  for (i = 0; i < n1; i++)
    {
      ira_object_t c1 = ALLOCNO_OBJECT (a1, i);

      for (j = 0; j < n2; j++)
	{
	  ira_object_t c2 = ALLOCNO_OBJECT (a2, j);

	  if (ira_live_ranges_intersect_p (OBJECT_LIVE_RANGES (c1),
					   OBJECT_LIVE_RANGES (c2)))
	    return true;
	}
    }
  return false;
}

#ifdef ENABLE_IRA_CHECKING

/* Return TRUE if live ranges of pseudo-registers REGNO1 and REGNO2
   intersect.  This should be used when there is only one region.
   Currently this is used during reload.  */
static bool
conflict_by_live_ranges_p (int regno1, int regno2)
{
  ira_allocno_t a1, a2;

  ira_assert (regno1 >= FIRST_PSEUDO_REGISTER
	      && regno2 >= FIRST_PSEUDO_REGISTER);
  /* Reg info caclulated by dataflow infrastructure can be different
     from one calculated by regclass.  */
  if ((a1 = ira_loop_tree_root->regno_allocno_map[regno1]) == NULL
      || (a2 = ira_loop_tree_root->regno_allocno_map[regno2]) == NULL)
    return false;
  return allocnos_conflict_by_live_ranges_p (a1, a2);
}

#endif



/* This page contains code to coalesce memory stack slots used by
   spilled allocnos.  This results in smaller stack frame, better data
   locality, and in smaller code for some architectures like
   x86/x86_64 where insn size depends on address displacement value.
   On the other hand, it can worsen insn scheduling after the RA but
   in practice it is less important than smaller stack frames.  */

/* TRUE if we coalesced some allocnos.  In other words, if we got
   loops formed by members first_coalesced_allocno and
   next_coalesced_allocno containing more one allocno.  */
static bool allocno_coalesced_p;

/* Bitmap used to prevent a repeated allocno processing because of
   coalescing.  */
static bitmap processed_coalesced_allocno_bitmap;

/* See below.  */
typedef struct coalesce_data *coalesce_data_t;

/* To decrease footprint of ira_allocno structure we store all data
   needed only for coalescing in the following structure.  */
struct coalesce_data
{
  /* Coalesced allocnos form a cyclic list.  One allocno given by
     FIRST represents all coalesced allocnos.  The
     list is chained by NEXT.  */
  ira_allocno_t first;
  ira_allocno_t next;
  int temp;
};

/* Container for storing allocno data concerning coalescing.  */
static coalesce_data_t allocno_coalesce_data;

/* Macro to access the data concerning coalescing.  */
#define ALLOCNO_COALESCE_DATA(a) ((coalesce_data_t) ALLOCNO_ADD_DATA (a))

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

  first = ALLOCNO_COALESCE_DATA (a1)->first;
  a = ALLOCNO_COALESCE_DATA (a2)->first;
  if (first == a)
    return;
  for (last = a2, a = ALLOCNO_COALESCE_DATA (a2)->next;;
       a = ALLOCNO_COALESCE_DATA (a)->next)
    {
      ALLOCNO_COALESCE_DATA (a)->first = first;
      if (a == a2)
	break;
      last = a;
    }
  next = allocno_coalesce_data[ALLOCNO_NUM (first)].next;
  allocno_coalesce_data[ALLOCNO_NUM (first)].next = a2;
  allocno_coalesce_data[ALLOCNO_NUM (last)].next = next;
}

/* Return TRUE if there are conflicting allocnos from two sets of
   coalesced allocnos given correspondingly by allocnos A1 and A2.  We
   use live ranges to find conflicts because conflicts are represented
   only for allocnos of the same allocno class and during the reload
   pass we coalesce allocnos for sharing stack memory slots.  */
static bool
coalesced_allocno_conflict_p (ira_allocno_t a1, ira_allocno_t a2)
{
  ira_allocno_t a, conflict_a;

  if (allocno_coalesced_p)
    {
      bitmap_clear (processed_coalesced_allocno_bitmap);
      for (a = ALLOCNO_COALESCE_DATA (a1)->next;;
	   a = ALLOCNO_COALESCE_DATA (a)->next)
	{
	  bitmap_set_bit (processed_coalesced_allocno_bitmap, ALLOCNO_NUM (a));
	  if (a == a1)
	    break;
	}
    }
  for (a = ALLOCNO_COALESCE_DATA (a2)->next;;
       a = ALLOCNO_COALESCE_DATA (a)->next)
    {
      for (conflict_a = ALLOCNO_COALESCE_DATA (a1)->next;;
	   conflict_a = ALLOCNO_COALESCE_DATA (conflict_a)->next)
	{
	  if (allocnos_conflict_by_live_ranges_p (a, conflict_a))
	    return true;
	  if (conflict_a == a1)
	    break;
	}
      if (a == a2)
	break;
    }
  return false;
}

/* The major function for aggressive allocno coalescing.  We coalesce
   only spilled allocnos.  If some allocnos have been coalesced, we
   set up flag allocno_coalesced_p.  */
static void
coalesce_allocnos (void)
{
  ira_allocno_t a;
  ira_copy_t cp, next_cp, *sorted_copies;
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
      if (! ALLOCNO_ASSIGNED_P (a) || ALLOCNO_HARD_REGNO (a) >= 0
	  || (regno < ira_reg_equiv_len
	      && (ira_reg_equiv_const[regno] != NULL_RTX
		  || ira_reg_equiv_invariant_p[regno])))
	continue;
      for (cp = ALLOCNO_COPIES (a); cp != NULL; cp = next_cp)
	{
	  if (cp->first == a)
	    {
	      next_cp = cp->next_first_allocno_copy;
	      regno = ALLOCNO_REGNO (cp->second);
	      /* For priority coloring we coalesce allocnos only with
		 the same allocno class not with intersected allocno
		 classes as it were possible.  It is done for
		 simplicity.  */
	      if ((cp->insn != NULL || cp->constraint_p)
		  && ALLOCNO_ASSIGNED_P (cp->second)
		  && ALLOCNO_HARD_REGNO (cp->second) < 0
		  && (regno >= ira_reg_equiv_len
		      || (! ira_reg_equiv_invariant_p[regno]
			  && ira_reg_equiv_const[regno] == NULL_RTX)))
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
	  if (! coalesced_allocno_conflict_p (cp->first, cp->second))
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
	  if (allocno_coalesce_data[ALLOCNO_NUM (cp->first)].first
	      != allocno_coalesce_data[ALLOCNO_NUM (cp->second)].first)
	    sorted_copies[n++] = cp;
	}
      cp_num = n;
    }
  ira_free (sorted_copies);
}

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
  total_size1 = MAX (PSEUDO_REGNO_BYTES (regno1),
		     regno_max_ref_width[regno1]);
  total_size2 = MAX (PSEUDO_REGNO_BYTES (regno2),
		     regno_max_ref_width[regno2]);
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
      if (ALLOCNO_COALESCE_DATA (allocno)->first != allocno)
	continue;
      num++;
      for (cost = 0, a = ALLOCNO_COALESCE_DATA (allocno)->next;;
	   a = ALLOCNO_COALESCE_DATA (a)->next)
	{
	  cost += ALLOCNO_FREQ (a);
	  if (a == allocno)
	    break;
	}
      for (a = ALLOCNO_COALESCE_DATA (allocno)->next;;
	   a = ALLOCNO_COALESCE_DATA (a)->next)
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
	  || ALLOCNO_COALESCE_DATA (allocno)->first != allocno)
	continue;
      spilled_coalesced_allocnos[num++] = allocno;
    }
  return num;
}

/* Array of live ranges of size IRA_ALLOCNOS_NUM.  Live range for
   given slot contains live ranges of coalesced allocnos assigned to
   given slot.  */
static live_range_t *slot_coalesced_allocnos_live_ranges;

/* Return TRUE if coalesced allocnos represented by ALLOCNO has live
   ranges intersected with live ranges of coalesced allocnos assigned
   to slot with number N.  */
static bool
slot_coalesced_allocno_live_ranges_intersect_p (ira_allocno_t allocno, int n)
{
  ira_allocno_t a;

  for (a = ALLOCNO_COALESCE_DATA (allocno)->next;;
       a = ALLOCNO_COALESCE_DATA (a)->next)
    {
      int i;
      int nr = ALLOCNO_NUM_OBJECTS (a);

      for (i = 0; i < nr; i++)
	{
	  ira_object_t obj = ALLOCNO_OBJECT (a, i);

	  if (ira_live_ranges_intersect_p
	      (slot_coalesced_allocnos_live_ranges[n],
	       OBJECT_LIVE_RANGES (obj)))
	    return true;
	}
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
  int i, n;
  ira_allocno_t a;
  live_range_t r;

  n = ALLOCNO_COALESCE_DATA (allocno)->temp;
  for (a = ALLOCNO_COALESCE_DATA (allocno)->next;;
       a = ALLOCNO_COALESCE_DATA (a)->next)
    {
      int nr = ALLOCNO_NUM_OBJECTS (a);
      for (i = 0; i < nr; i++)
	{
	  ira_object_t obj = ALLOCNO_OBJECT (a, i);

	  r = ira_copy_live_range_list (OBJECT_LIVE_RANGES (obj));
	  slot_coalesced_allocnos_live_ranges[n]
	    = ira_merge_live_ranges
	      (slot_coalesced_allocnos_live_ranges[n], r);
	}
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
    = (live_range_t *) ira_allocate (sizeof (live_range_t) * ira_allocnos_num);
  memset (slot_coalesced_allocnos_live_ranges, 0,
	  sizeof (live_range_t) * ira_allocnos_num);
  last_coalesced_allocno_num = 0;
  /* Coalesce non-conflicting spilled allocnos preferring most
     frequently used.  */
  for (i = 0; i < num; i++)
    {
      allocno = spilled_coalesced_allocnos[i];
      if (ALLOCNO_COALESCE_DATA (allocno)->first != allocno
	  || bitmap_bit_p (set_jump_crosses, ALLOCNO_REGNO (allocno))
	  || (ALLOCNO_REGNO (allocno) < ira_reg_equiv_len
	      && (ira_reg_equiv_const[ALLOCNO_REGNO (allocno)] != NULL_RTX
		  || ira_reg_equiv_invariant_p[ALLOCNO_REGNO (allocno)])))
	continue;
      for (j = 0; j < i; j++)
	{
	  a = spilled_coalesced_allocnos[j];
	  n = ALLOCNO_COALESCE_DATA (a)->temp;
	  if (ALLOCNO_COALESCE_DATA (a)->first == a
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
	  ALLOCNO_COALESCE_DATA (allocno)->temp = last_coalesced_allocno_num++;
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
	  ALLOCNO_COALESCE_DATA (allocno)->temp
	    = ALLOCNO_COALESCE_DATA (a)->temp;
	  setup_slot_coalesced_allocno_live_ranges (allocno);
	  merge_allocnos (a, allocno);
	  ira_assert (ALLOCNO_COALESCE_DATA (a)->first == a);
	}
    }
  for (i = 0; i < ira_allocnos_num; i++)
    ira_finish_live_range_list (slot_coalesced_allocnos_live_ranges[i]);
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

  /* Set up allocnos can be coalesced.  */
  coloring_allocno_bitmap = ira_allocate_bitmap ();
  for (i = 0; i < n; i++)
    {
      regno = pseudo_regnos[i];
      allocno = ira_regno_allocno_map[regno];
      if (allocno != NULL)
	bitmap_set_bit (coloring_allocno_bitmap, ALLOCNO_NUM (allocno));
    }
  allocno_coalesced_p = false;
  processed_coalesced_allocno_bitmap = ira_allocate_bitmap ();
  allocno_coalesce_data
    = (coalesce_data_t) ira_allocate (sizeof (struct coalesce_data)
				      * ira_allocnos_num);
  /* Initialize coalesce data for allocnos.  */
  FOR_EACH_ALLOCNO (a, ai)
    {
      ALLOCNO_ADD_DATA (a) = allocno_coalesce_data + ALLOCNO_NUM (a);
      ALLOCNO_COALESCE_DATA (a)->first = a;
      ALLOCNO_COALESCE_DATA (a)->next = a;
    }
  coalesce_allocnos ();
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
      if (ALLOCNO_COALESCE_DATA (allocno)->first != allocno
	  || ALLOCNO_HARD_REGNO (allocno) >= 0
	  || (ALLOCNO_REGNO (allocno) < ira_reg_equiv_len
	      && (ira_reg_equiv_const[ALLOCNO_REGNO (allocno)] != NULL_RTX
		  || ira_reg_equiv_invariant_p[ALLOCNO_REGNO (allocno)])))
	continue;
      if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
	fprintf (ira_dump_file, "      Slot %d (freq,size):", slot_num);
      slot_num++;
      for (a = ALLOCNO_COALESCE_DATA (allocno)->next;;
	   a = ALLOCNO_COALESCE_DATA (a)->next)
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
  FOR_EACH_ALLOCNO (a, ai)
    ALLOCNO_ADD_DATA (a) = NULL;
  ira_free (allocno_coalesce_data);
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
  enum reg_class aclass = ALLOCNO_CLASS (a);

  ira_assert (a != NULL);
  hard_regno = reg_renumber[regno];
  if ((old_hard_regno = ALLOCNO_HARD_REGNO (a)) == hard_regno)
    return;
  if (old_hard_regno < 0)
    cost = -ALLOCNO_MEMORY_COST (a);
  else
    {
      ira_assert (ira_class_hard_reg_index[aclass][old_hard_regno] >= 0);
      cost = -(ALLOCNO_HARD_REG_COSTS (a) == NULL
	       ? ALLOCNO_CLASS_COST (a)
	       : ALLOCNO_HARD_REG_COSTS (a)
	         [ira_class_hard_reg_index[aclass][old_hard_regno]]);
      update_copy_costs (a, false);
    }
  ira_overall_cost -= cost;
  ALLOCNO_HARD_REGNO (a) = hard_regno;
  if (hard_regno < 0)
    {
      ALLOCNO_HARD_REGNO (a) = -1;
      cost += ALLOCNO_MEMORY_COST (a);
    }
  else if (ira_class_hard_reg_index[aclass][hard_regno] >= 0)
    {
      cost += (ALLOCNO_HARD_REG_COSTS (a) == NULL
	       ? ALLOCNO_CLASS_COST (a)
	       : ALLOCNO_HARD_REG_COSTS (a)
	         [ira_class_hard_reg_index[aclass][hard_regno]]);
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
  enum reg_class aclass;
  int regno = ALLOCNO_REGNO (a);
  HARD_REG_SET saved[2];
  int i, n;

  n = ALLOCNO_NUM_OBJECTS (a);
  for (i = 0; i < n; i++)
    {
      ira_object_t obj = ALLOCNO_OBJECT (a, i);
      COPY_HARD_REG_SET (saved[i], OBJECT_TOTAL_CONFLICT_HARD_REGS (obj));
      IOR_HARD_REG_SET (OBJECT_TOTAL_CONFLICT_HARD_REGS (obj), forbidden_regs);
      if (! flag_caller_saves && ALLOCNO_CALLS_CROSSED_NUM (a) != 0)
	IOR_HARD_REG_SET (OBJECT_TOTAL_CONFLICT_HARD_REGS (obj),
			  call_used_reg_set);
    }
  ALLOCNO_ASSIGNED_P (a) = false;
  aclass = ALLOCNO_CLASS (a);
  update_curr_costs (a);
  assign_hard_reg (a, true);
  hard_regno = ALLOCNO_HARD_REGNO (a);
  reg_renumber[regno] = hard_regno;
  if (hard_regno < 0)
    ALLOCNO_HARD_REGNO (a) = -1;
  else
    {
      ira_assert (ira_class_hard_reg_index[aclass][hard_regno] >= 0);
      ira_overall_cost
	-= (ALLOCNO_MEMORY_COST (a)
	    - (ALLOCNO_HARD_REG_COSTS (a) == NULL
	       ? ALLOCNO_CLASS_COST (a)
	       : ALLOCNO_HARD_REG_COSTS (a)[ira_class_hard_reg_index
					    [aclass][hard_regno]]));
      if (ALLOCNO_CALLS_CROSSED_NUM (a) != 0
	  && ira_hard_reg_set_intersection_p (hard_regno, ALLOCNO_MODE (a),
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
  for (i = 0; i < n; i++)
    {
      ira_object_t obj = ALLOCNO_OBJECT (a, i);
      COPY_HARD_REG_SET (OBJECT_TOTAL_CONFLICT_HARD_REGS (obj), saved[i]);
    }
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
		      HARD_REG_SET *pseudo_previous_regs,
		      bitmap spilled)
{
  int i, n, regno;
  bool changed_p;
  ira_allocno_t a;
  HARD_REG_SET forbidden_regs;
  bitmap temp = BITMAP_ALLOC (NULL);

  /* Add pseudos which conflict with pseudos already in
     SPILLED_PSEUDO_REGS to SPILLED_PSEUDO_REGS.  This is preferable
     to allocating in two steps as some of the conflicts might have
     a higher priority than the pseudos passed in SPILLED_PSEUDO_REGS.  */
  for (i = 0; i < num; i++)
    bitmap_set_bit (temp, spilled_pseudo_regs[i]);

  for (i = 0, n = num; i < n; i++)
    {
      int nr, j;
      int regno = spilled_pseudo_regs[i];
      bitmap_set_bit (temp, regno);

      a = ira_regno_allocno_map[regno];
      nr = ALLOCNO_NUM_OBJECTS (a);
      for (j = 0; j < nr; j++)
	{
	  ira_object_t conflict_obj;
	  ira_object_t obj = ALLOCNO_OBJECT (a, j);
	  ira_object_conflict_iterator oci;

	  FOR_EACH_OBJECT_CONFLICT (obj, conflict_obj, oci)
	    {
	      ira_allocno_t conflict_a = OBJECT_ALLOCNO (conflict_obj);
	      if (ALLOCNO_HARD_REGNO (conflict_a) < 0
		  && ! ALLOCNO_DONT_REASSIGN_P (conflict_a)
		  && bitmap_set_bit (temp, ALLOCNO_REGNO (conflict_a)))
		{
		  spilled_pseudo_regs[num++] = ALLOCNO_REGNO (conflict_a);
		  /* ?!? This seems wrong.  */
		  bitmap_set_bit (consideration_allocno_bitmap,
				  ALLOCNO_NUM (conflict_a));
		}
	    }
	}
    }

  if (num > 1)
    qsort (spilled_pseudo_regs, num, sizeof (int), pseudo_reg_compare);
  changed_p = false;
  /* Try to assign hard registers to pseudos from
     SPILLED_PSEUDO_REGS.  */
  for (i = 0; i < num; i++)
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
		 "      Try Assign %d(a%d), cost=%d", regno, ALLOCNO_NUM (a),
		 ALLOCNO_MEMORY_COST (a)
		 - ALLOCNO_CLASS_COST (a));
      allocno_reload_assign (a, forbidden_regs);
      if (reg_renumber[regno] >= 0)
	{
	  CLEAR_REGNO_REG_SET (spilled, regno);
	  changed_p = true;
	}
    }
  BITMAP_FREE (temp);
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
	      if (allocnos_conflict_by_live_ranges_p (allocno,
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
	  ira_assert (! conflict_by_live_ranges_p (regno, i));
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
      length += ALLOCNO_EXCESS_PRESSURE_POINTS_NUM (a) / ALLOCNO_NUM_OBJECTS (a);
      cost += ALLOCNO_MEMORY_COST (a) - ALLOCNO_CLASS_COST (a);
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
	                  [ALLOCNO_MODE (a)][ALLOCNO_CLASS (a)][1];
	  if (out_p)
	    saved_cost
	      += ira_memory_move_cost
	         [ALLOCNO_MODE (a)][ALLOCNO_CLASS (a)][0];
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
  memset (allocated_hardreg_p, 0, sizeof (allocated_hardreg_p));
  ira_initiate_assign ();
  do_coloring ();
  ira_finish_assign ();
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
  enum reg_class aclass;
  enum machine_mode mode;
  ira_allocno_t a;
  ira_allocno_iterator ai;
  live_range_t r;
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
      int nr, l;

      a = sorted_allocnos[i];
      nr = ALLOCNO_NUM_OBJECTS (a);
      CLEAR_HARD_REG_SET (conflict_hard_regs);
      for (l = 0; l < nr; l++)
	{
	  ira_object_t obj = ALLOCNO_OBJECT (a, l);
	  IOR_HARD_REG_SET (conflict_hard_regs,
			    OBJECT_CONFLICT_HARD_REGS (obj));
	  for (r = OBJECT_LIVE_RANGES (obj); r != NULL; r = r->next)
	    for (j = r->start; j <= r->finish; j++)
	      IOR_HARD_REG_SET (conflict_hard_regs, used_hard_regs[j]);
	}
      aclass = ALLOCNO_CLASS (a);
      ALLOCNO_ASSIGNED_P (a) = true;
      ALLOCNO_HARD_REGNO (a) = -1;
      if (hard_reg_set_subset_p (reg_class_contents[aclass],
				 conflict_hard_regs))
	continue;
      mode = ALLOCNO_MODE (a);
#ifdef STACK_REGS
      no_stack_reg_p = ALLOCNO_NO_STACK_REG_P (a);
#endif
      class_size = ira_class_hard_regs_num[aclass];
      for (j = 0; j < class_size; j++)
	{
	  hard_regno = ira_class_hard_regs[aclass][j];
#ifdef STACK_REGS
	  if (no_stack_reg_p && FIRST_STACK_REG <= hard_regno
	      && hard_regno <= LAST_STACK_REG)
	    continue;
#endif
	  if (ira_hard_reg_set_intersection_p (hard_regno, mode, conflict_hard_regs)
	      || (TEST_HARD_REG_BIT
		  (ira_prohibited_class_mode_regs[aclass][mode], hard_regno)))
	    continue;
	  ALLOCNO_HARD_REGNO (a) = hard_regno;
	  for (l = 0; l < nr; l++)
	    {
	      ira_object_t obj = ALLOCNO_OBJECT (a, l);
	      for (r = OBJECT_LIVE_RANGES (obj); r != NULL; r = r->next)
		for (k = r->start; k <= r->finish; k++)
		  IOR_HARD_REG_SET (used_hard_regs[k],
				    ira_reg_mode_hard_regset[hard_regno][mode]);
	    }
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
      ALLOCNO_UPDATED_CLASS_COST (a) = ALLOCNO_CLASS_COST (a);
    }
  if (ira_conflicts_p)
    color ();
  else
    fast_allocation ();
}
