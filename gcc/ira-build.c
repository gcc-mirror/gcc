/* Building internal representation for IRA.
   Copyright (C) 2006-2013 Free Software Foundation, Inc.
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
#include "hard-reg-set.h"
#include "basic-block.h"
#include "insn-config.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "params.h"
#include "df.h"
#include "reload.h"
#include "sparseset.h"
#include "ira-int.h"
#include "emit-rtl.h"  /* FIXME: Can go away once crtl is moved to rtl.h.  */

static ira_copy_t find_allocno_copy (ira_allocno_t, ira_allocno_t, rtx,
				     ira_loop_tree_node_t);

/* The root of the loop tree corresponding to the all function.  */
ira_loop_tree_node_t ira_loop_tree_root;

/* Height of the loop tree.  */
int ira_loop_tree_height;

/* All nodes representing basic blocks are referred through the
   following array.  We can not use basic block member `aux' for this
   because it is used for insertion of insns on edges.  */
ira_loop_tree_node_t ira_bb_nodes;

/* All nodes representing loops are referred through the following
   array.  */
ira_loop_tree_node_t ira_loop_nodes;

/* And size of the ira_loop_nodes array.  */
unsigned int ira_loop_nodes_count;

/* Map regno -> allocnos with given regno (see comments for
   allocno member `next_regno_allocno').  */
ira_allocno_t *ira_regno_allocno_map;

/* Array of references to all allocnos.  The order number of the
   allocno corresponds to the index in the array.  Removed allocnos
   have NULL element value.  */
ira_allocno_t *ira_allocnos;

/* Sizes of the previous array.  */
int ira_allocnos_num;

/* Count of conflict record structures we've created, used when creating
   a new conflict id.  */
int ira_objects_num;

/* Map a conflict id to its conflict record.  */
ira_object_t *ira_object_id_map;

/* Array of references to all allocno preferences.  The order number
   of the preference corresponds to the index in the array.  */
ira_pref_t *ira_prefs;

/* Size of the previous array.  */
int ira_prefs_num;

/* Array of references to all copies.  The order number of the copy
   corresponds to the index in the array.  Removed copies have NULL
   element value.  */
ira_copy_t *ira_copies;

/* Size of the previous array.  */
int ira_copies_num;



/* LAST_BASIC_BLOCK before generating additional insns because of live
   range splitting.  Emitting insns on a critical edge creates a new
   basic block.  */
static int last_basic_block_before_change;

/* Initialize some members in loop tree node NODE.  Use LOOP_NUM for
   the member loop_num.  */
static void
init_loop_tree_node (struct ira_loop_tree_node *node, int loop_num)
{
  int max_regno = max_reg_num ();

  node->regno_allocno_map
    = (ira_allocno_t *) ira_allocate (sizeof (ira_allocno_t) * max_regno);
  memset (node->regno_allocno_map, 0, sizeof (ira_allocno_t) * max_regno);
  memset (node->reg_pressure, 0, sizeof (node->reg_pressure));
  node->all_allocnos = ira_allocate_bitmap ();
  node->modified_regnos = ira_allocate_bitmap ();
  node->border_allocnos = ira_allocate_bitmap ();
  node->local_copies = ira_allocate_bitmap ();
  node->loop_num = loop_num;
  node->children = NULL;
  node->subloops = NULL;
}


/* The following function allocates the loop tree nodes.  If
   CURRENT_LOOPS is NULL, the nodes corresponding to the loops (except
   the root which corresponds the all function) will be not allocated
   but nodes will still be allocated for basic blocks.  */
static void
create_loop_tree_nodes (void)
{
  unsigned int i, j;
  bool skip_p;
  edge_iterator ei;
  edge e;
  vec<edge> edges;
  loop_p loop;

  ira_bb_nodes
    = ((struct ira_loop_tree_node *)
       ira_allocate (sizeof (struct ira_loop_tree_node)
		     * last_basic_block_for_fn (cfun)));
  last_basic_block_before_change = last_basic_block_for_fn (cfun);
  for (i = 0; i < (unsigned int) last_basic_block_for_fn (cfun); i++)
    {
      ira_bb_nodes[i].regno_allocno_map = NULL;
      memset (ira_bb_nodes[i].reg_pressure, 0,
	      sizeof (ira_bb_nodes[i].reg_pressure));
      ira_bb_nodes[i].all_allocnos = NULL;
      ira_bb_nodes[i].modified_regnos = NULL;
      ira_bb_nodes[i].border_allocnos = NULL;
      ira_bb_nodes[i].local_copies = NULL;
    }
  if (current_loops == NULL)
    {
      ira_loop_nodes_count = 1;
      ira_loop_nodes = ((struct ira_loop_tree_node *)
			ira_allocate (sizeof (struct ira_loop_tree_node)));
      init_loop_tree_node (ira_loop_nodes, 0);
      return;
    }
  ira_loop_nodes_count = number_of_loops (cfun);
  ira_loop_nodes = ((struct ira_loop_tree_node *)
		    ira_allocate (sizeof (struct ira_loop_tree_node)
				  * ira_loop_nodes_count));
  FOR_EACH_VEC_SAFE_ELT (get_loops (cfun), i, loop)
    {
      if (loop_outer (loop) != NULL)
	{
	  ira_loop_nodes[i].regno_allocno_map = NULL;
	  skip_p = false;
	  FOR_EACH_EDGE (e, ei, loop->header->preds)
	    if (e->src != loop->latch
		&& (e->flags & EDGE_ABNORMAL) && EDGE_CRITICAL_P (e))
	      {
		skip_p = true;
		break;
	      }
	  if (skip_p)
	    continue;
	  edges = get_loop_exit_edges (loop);
	  FOR_EACH_VEC_ELT (edges, j, e)
	    if ((e->flags & EDGE_ABNORMAL) && EDGE_CRITICAL_P (e))
	      {
		skip_p = true;
		break;
	      }
	  edges.release ();
	  if (skip_p)
	    continue;
	}
      init_loop_tree_node (&ira_loop_nodes[i], loop->num);
    }
}

/* The function returns TRUE if there are more one allocation
   region.  */
static bool
more_one_region_p (void)
{
  unsigned int i;
  loop_p loop;

  if (current_loops != NULL)
    FOR_EACH_VEC_SAFE_ELT (get_loops (cfun), i, loop)
      if (ira_loop_nodes[i].regno_allocno_map != NULL
	  && ira_loop_tree_root != &ira_loop_nodes[i])
	return true;
  return false;
}

/* Free the loop tree node of a loop.  */
static void
finish_loop_tree_node (ira_loop_tree_node_t loop)
{
  if (loop->regno_allocno_map != NULL)
    {
      ira_assert (loop->bb == NULL);
      ira_free_bitmap (loop->local_copies);
      ira_free_bitmap (loop->border_allocnos);
      ira_free_bitmap (loop->modified_regnos);
      ira_free_bitmap (loop->all_allocnos);
      ira_free (loop->regno_allocno_map);
      loop->regno_allocno_map = NULL;
    }
}

/* Free the loop tree nodes.  */
static void
finish_loop_tree_nodes (void)
{
  unsigned int i;

  for (i = 0; i < ira_loop_nodes_count; i++)
    finish_loop_tree_node (&ira_loop_nodes[i]);
  ira_free (ira_loop_nodes);
  for (i = 0; i < (unsigned int) last_basic_block_before_change; i++)
    {
      if (ira_bb_nodes[i].local_copies != NULL)
	ira_free_bitmap (ira_bb_nodes[i].local_copies);
      if (ira_bb_nodes[i].border_allocnos != NULL)
	ira_free_bitmap (ira_bb_nodes[i].border_allocnos);
      if (ira_bb_nodes[i].modified_regnos != NULL)
	ira_free_bitmap (ira_bb_nodes[i].modified_regnos);
      if (ira_bb_nodes[i].all_allocnos != NULL)
	ira_free_bitmap (ira_bb_nodes[i].all_allocnos);
      if (ira_bb_nodes[i].regno_allocno_map != NULL)
	ira_free (ira_bb_nodes[i].regno_allocno_map);
    }
  ira_free (ira_bb_nodes);
}



/* The following recursive function adds LOOP to the loop tree
   hierarchy.  LOOP is added only once.  If LOOP is NULL we adding
   loop designating the whole function when CFG loops are not
   built.  */
static void
add_loop_to_tree (struct loop *loop)
{
  int loop_num;
  struct loop *parent;
  ira_loop_tree_node_t loop_node, parent_node;

  /* We can not use loop node access macros here because of potential
     checking and because the nodes are not initialized enough
     yet.  */
  if (loop != NULL && loop_outer (loop) != NULL)
    add_loop_to_tree (loop_outer (loop));
  loop_num = loop != NULL ? loop->num : 0;
  if (ira_loop_nodes[loop_num].regno_allocno_map != NULL
      && ira_loop_nodes[loop_num].children == NULL)
    {
      /* We have not added loop node to the tree yet.  */
      loop_node = &ira_loop_nodes[loop_num];
      loop_node->loop = loop;
      loop_node->bb = NULL;
      if (loop == NULL)
	parent = NULL;
      else
	{
	  for (parent = loop_outer (loop);
	       parent != NULL;
	       parent = loop_outer (parent))
	    if (ira_loop_nodes[parent->num].regno_allocno_map != NULL)
	      break;
	}
      if (parent == NULL)
	{
	  loop_node->next = NULL;
	  loop_node->subloop_next = NULL;
	  loop_node->parent = NULL;
	}
      else
	{
	  parent_node = &ira_loop_nodes[parent->num];
	  loop_node->next = parent_node->children;
	  parent_node->children = loop_node;
	  loop_node->subloop_next = parent_node->subloops;
	  parent_node->subloops = loop_node;
	  loop_node->parent = parent_node;
	}
    }
}

/* The following recursive function sets up levels of nodes of the
   tree given its root LOOP_NODE.  The enumeration starts with LEVEL.
   The function returns maximal value of level in the tree + 1.  */
static int
setup_loop_tree_level (ira_loop_tree_node_t loop_node, int level)
{
  int height, max_height;
  ira_loop_tree_node_t subloop_node;

  ira_assert (loop_node->bb == NULL);
  loop_node->level = level;
  max_height = level + 1;
  for (subloop_node = loop_node->subloops;
       subloop_node != NULL;
       subloop_node = subloop_node->subloop_next)
    {
      ira_assert (subloop_node->bb == NULL);
      height = setup_loop_tree_level (subloop_node, level + 1);
      if (height > max_height)
	max_height = height;
    }
  return max_height;
}

/* Create the loop tree.  The algorithm is designed to provide correct
   order of loops (they are ordered by their last loop BB) and basic
   blocks in the chain formed by member next.  */
static void
form_loop_tree (void)
{
  basic_block bb;
  struct loop *parent;
  ira_loop_tree_node_t bb_node, loop_node;

  /* We can not use loop/bb node access macros because of potential
     checking and because the nodes are not initialized enough
     yet.  */
  FOR_EACH_BB_FN (bb, cfun)
    {
      bb_node = &ira_bb_nodes[bb->index];
      bb_node->bb = bb;
      bb_node->loop = NULL;
      bb_node->subloops = NULL;
      bb_node->children = NULL;
      bb_node->subloop_next = NULL;
      bb_node->next = NULL;
      if (current_loops == NULL)
	parent = NULL;
      else
	{
	  for (parent = bb->loop_father;
	       parent != NULL;
	       parent = loop_outer (parent))
	    if (ira_loop_nodes[parent->num].regno_allocno_map != NULL)
	      break;
	}
      add_loop_to_tree (parent);
      loop_node = &ira_loop_nodes[parent == NULL ? 0 : parent->num];
      bb_node->next = loop_node->children;
      bb_node->parent = loop_node;
      loop_node->children = bb_node;
    }
  ira_loop_tree_root = IRA_LOOP_NODE_BY_INDEX (0);
  ira_loop_tree_height = setup_loop_tree_level (ira_loop_tree_root, 0);
  ira_assert (ira_loop_tree_root->regno_allocno_map != NULL);
}



/* Rebuild IRA_REGNO_ALLOCNO_MAP and REGNO_ALLOCNO_MAPs of the loop
   tree nodes.  */
static void
rebuild_regno_allocno_maps (void)
{
  unsigned int l;
  int max_regno, regno;
  ira_allocno_t a;
  ira_loop_tree_node_t loop_tree_node;
  loop_p loop;
  ira_allocno_iterator ai;

  ira_assert (current_loops != NULL);
  max_regno = max_reg_num ();
  FOR_EACH_VEC_SAFE_ELT (get_loops (cfun), l, loop)
    if (ira_loop_nodes[l].regno_allocno_map != NULL)
      {
	ira_free (ira_loop_nodes[l].regno_allocno_map);
	ira_loop_nodes[l].regno_allocno_map
	  = (ira_allocno_t *) ira_allocate (sizeof (ira_allocno_t)
					    * max_regno);
	memset (ira_loop_nodes[l].regno_allocno_map, 0,
		sizeof (ira_allocno_t) * max_regno);
      }
  ira_free (ira_regno_allocno_map);
  ira_regno_allocno_map
    = (ira_allocno_t *) ira_allocate (max_regno * sizeof (ira_allocno_t));
  memset (ira_regno_allocno_map, 0, max_regno * sizeof (ira_allocno_t));
  FOR_EACH_ALLOCNO (a, ai)
    {
      if (ALLOCNO_CAP_MEMBER (a) != NULL)
	/* Caps are not in the regno allocno maps.  */
	continue;
      regno = ALLOCNO_REGNO (a);
      loop_tree_node = ALLOCNO_LOOP_TREE_NODE (a);
      ALLOCNO_NEXT_REGNO_ALLOCNO (a) = ira_regno_allocno_map[regno];
      ira_regno_allocno_map[regno] = a;
      if (loop_tree_node->regno_allocno_map[regno] == NULL)
	/* Remember that we can create temporary allocnos to break
	   cycles in register shuffle.  */
	loop_tree_node->regno_allocno_map[regno] = a;
    }
}


/* Pools for allocnos, allocno live ranges and objects.  */
static alloc_pool allocno_pool, live_range_pool, object_pool;

/* Vec containing references to all created allocnos.  It is a
   container of array allocnos.  */
static vec<ira_allocno_t> allocno_vec;

/* Vec containing references to all created ira_objects.  It is a
   container of ira_object_id_map.  */
static vec<ira_object_t> ira_object_id_map_vec;

/* Initialize data concerning allocnos.  */
static void
initiate_allocnos (void)
{
  live_range_pool
    = create_alloc_pool ("live ranges",
			 sizeof (struct live_range), 100);
  allocno_pool
    = create_alloc_pool ("allocnos", sizeof (struct ira_allocno), 100);
  object_pool
    = create_alloc_pool ("objects", sizeof (struct ira_object), 100);
  allocno_vec.create (max_reg_num () * 2);
  ira_allocnos = NULL;
  ira_allocnos_num = 0;
  ira_objects_num = 0;
  ira_object_id_map_vec.create (max_reg_num () * 2);
  ira_object_id_map = NULL;
  ira_regno_allocno_map
    = (ira_allocno_t *) ira_allocate (max_reg_num ()
				      * sizeof (ira_allocno_t));
  memset (ira_regno_allocno_map, 0, max_reg_num () * sizeof (ira_allocno_t));
}

/* Create and return an object corresponding to a new allocno A.  */
static ira_object_t
ira_create_object (ira_allocno_t a, int subword)
{
  enum reg_class aclass = ALLOCNO_CLASS (a);
  ira_object_t obj = (ira_object_t) pool_alloc (object_pool);

  OBJECT_ALLOCNO (obj) = a;
  OBJECT_SUBWORD (obj) = subword;
  OBJECT_CONFLICT_ID (obj) = ira_objects_num;
  OBJECT_CONFLICT_VEC_P (obj) = false;
  OBJECT_CONFLICT_ARRAY (obj) = NULL;
  OBJECT_NUM_CONFLICTS (obj) = 0;
  COPY_HARD_REG_SET (OBJECT_CONFLICT_HARD_REGS (obj), ira_no_alloc_regs);
  COPY_HARD_REG_SET (OBJECT_TOTAL_CONFLICT_HARD_REGS (obj), ira_no_alloc_regs);
  IOR_COMPL_HARD_REG_SET (OBJECT_CONFLICT_HARD_REGS (obj),
			  reg_class_contents[aclass]);
  IOR_COMPL_HARD_REG_SET (OBJECT_TOTAL_CONFLICT_HARD_REGS (obj),
			  reg_class_contents[aclass]);
  OBJECT_MIN (obj) = INT_MAX;
  OBJECT_MAX (obj) = -1;
  OBJECT_LIVE_RANGES (obj) = NULL;

  ira_object_id_map_vec.safe_push (obj);
  ira_object_id_map
    = ira_object_id_map_vec.address ();
  ira_objects_num = ira_object_id_map_vec.length ();

  return obj;
}

/* Create and return the allocno corresponding to REGNO in
   LOOP_TREE_NODE.  Add the allocno to the list of allocnos with the
   same regno if CAP_P is FALSE.  */
ira_allocno_t
ira_create_allocno (int regno, bool cap_p,
		    ira_loop_tree_node_t loop_tree_node)
{
  ira_allocno_t a;

  a = (ira_allocno_t) pool_alloc (allocno_pool);
  ALLOCNO_REGNO (a) = regno;
  ALLOCNO_LOOP_TREE_NODE (a) = loop_tree_node;
  if (! cap_p)
    {
      ALLOCNO_NEXT_REGNO_ALLOCNO (a) = ira_regno_allocno_map[regno];
      ira_regno_allocno_map[regno] = a;
      if (loop_tree_node->regno_allocno_map[regno] == NULL)
	/* Remember that we can create temporary allocnos to break
	   cycles in register shuffle on region borders (see
	   ira-emit.c).  */
	loop_tree_node->regno_allocno_map[regno] = a;
    }
  ALLOCNO_CAP (a) = NULL;
  ALLOCNO_CAP_MEMBER (a) = NULL;
  ALLOCNO_NUM (a) = ira_allocnos_num;
  bitmap_set_bit (loop_tree_node->all_allocnos, ALLOCNO_NUM (a));
  ALLOCNO_NREFS (a) = 0;
  ALLOCNO_FREQ (a) = 0;
  ALLOCNO_HARD_REGNO (a) = -1;
  ALLOCNO_CALL_FREQ (a) = 0;
  ALLOCNO_CALLS_CROSSED_NUM (a) = 0;
  ALLOCNO_CHEAP_CALLS_CROSSED_NUM (a) = 0;
#ifdef STACK_REGS
  ALLOCNO_NO_STACK_REG_P (a) = false;
  ALLOCNO_TOTAL_NO_STACK_REG_P (a) = false;
#endif
  ALLOCNO_DONT_REASSIGN_P (a) = false;
  ALLOCNO_BAD_SPILL_P (a) = false;
  ALLOCNO_ASSIGNED_P (a) = false;
  ALLOCNO_MODE (a) = (regno < 0 ? VOIDmode : PSEUDO_REGNO_MODE (regno));
  ALLOCNO_PREFS (a) = NULL;
  ALLOCNO_COPIES (a) = NULL;
  ALLOCNO_HARD_REG_COSTS (a) = NULL;
  ALLOCNO_CONFLICT_HARD_REG_COSTS (a) = NULL;
  ALLOCNO_UPDATED_HARD_REG_COSTS (a) = NULL;
  ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS (a) = NULL;
  ALLOCNO_CLASS (a) = NO_REGS;
  ALLOCNO_UPDATED_CLASS_COST (a) = 0;
  ALLOCNO_CLASS_COST (a) = 0;
  ALLOCNO_MEMORY_COST (a) = 0;
  ALLOCNO_UPDATED_MEMORY_COST (a) = 0;
  ALLOCNO_EXCESS_PRESSURE_POINTS_NUM (a) = 0;
  ALLOCNO_NUM_OBJECTS (a) = 0;

  ALLOCNO_ADD_DATA (a) = NULL;
  allocno_vec.safe_push (a);
  ira_allocnos = allocno_vec.address ();
  ira_allocnos_num = allocno_vec.length ();

  return a;
}

/* Set up register class for A and update its conflict hard
   registers.  */
void
ira_set_allocno_class (ira_allocno_t a, enum reg_class aclass)
{
  ira_allocno_object_iterator oi;
  ira_object_t obj;

  ALLOCNO_CLASS (a) = aclass;
  FOR_EACH_ALLOCNO_OBJECT (a, obj, oi)
    {
      IOR_COMPL_HARD_REG_SET (OBJECT_CONFLICT_HARD_REGS (obj),
			      reg_class_contents[aclass]);
      IOR_COMPL_HARD_REG_SET (OBJECT_TOTAL_CONFLICT_HARD_REGS (obj),
			      reg_class_contents[aclass]);
    }
}

/* Determine the number of objects we should associate with allocno A
   and allocate them.  */
void
ira_create_allocno_objects (ira_allocno_t a)
{
  enum machine_mode mode = ALLOCNO_MODE (a);
  enum reg_class aclass = ALLOCNO_CLASS (a);
  int n = ira_reg_class_max_nregs[aclass][mode];
  int i;

  if (GET_MODE_SIZE (mode) != 2 * UNITS_PER_WORD || n != 2)
    n = 1;

  ALLOCNO_NUM_OBJECTS (a) = n;
  for (i = 0; i < n; i++)
    ALLOCNO_OBJECT (a, i) = ira_create_object (a, i);
}

/* For each allocno, set ALLOCNO_NUM_OBJECTS and create the
   ALLOCNO_OBJECT structures.  This must be called after the allocno
   classes are known.  */
static void
create_allocno_objects (void)
{
  ira_allocno_t a;
  ira_allocno_iterator ai;

  FOR_EACH_ALLOCNO (a, ai)
    ira_create_allocno_objects (a);
}

/* Merge hard register conflict information for all objects associated with
   allocno TO into the corresponding objects associated with FROM.
   If TOTAL_ONLY is true, we only merge OBJECT_TOTAL_CONFLICT_HARD_REGS.  */
static void
merge_hard_reg_conflicts (ira_allocno_t from, ira_allocno_t to,
			  bool total_only)
{
  int i;
  gcc_assert (ALLOCNO_NUM_OBJECTS (to) == ALLOCNO_NUM_OBJECTS (from));
  for (i = 0; i < ALLOCNO_NUM_OBJECTS (to); i++)
    {
      ira_object_t from_obj = ALLOCNO_OBJECT (from, i);
      ira_object_t to_obj = ALLOCNO_OBJECT (to, i);

      if (!total_only)
	IOR_HARD_REG_SET (OBJECT_CONFLICT_HARD_REGS (to_obj),
			  OBJECT_CONFLICT_HARD_REGS (from_obj));
      IOR_HARD_REG_SET (OBJECT_TOTAL_CONFLICT_HARD_REGS (to_obj),
			OBJECT_TOTAL_CONFLICT_HARD_REGS (from_obj));
    }
#ifdef STACK_REGS
  if (!total_only && ALLOCNO_NO_STACK_REG_P (from))
    ALLOCNO_NO_STACK_REG_P (to) = true;
  if (ALLOCNO_TOTAL_NO_STACK_REG_P (from))
    ALLOCNO_TOTAL_NO_STACK_REG_P (to) = true;
#endif
}

/* Update hard register conflict information for all objects associated with
   A to include the regs in SET.  */
void
ior_hard_reg_conflicts (ira_allocno_t a, HARD_REG_SET *set)
{
  ira_allocno_object_iterator i;
  ira_object_t obj;

  FOR_EACH_ALLOCNO_OBJECT (a, obj, i)
    {
      IOR_HARD_REG_SET (OBJECT_CONFLICT_HARD_REGS (obj), *set);
      IOR_HARD_REG_SET (OBJECT_TOTAL_CONFLICT_HARD_REGS (obj), *set);
    }
}

/* Return TRUE if a conflict vector with NUM elements is more
   profitable than a conflict bit vector for OBJ.  */
bool
ira_conflict_vector_profitable_p (ira_object_t obj, int num)
{
  int nw;
  int max = OBJECT_MAX (obj);
  int min = OBJECT_MIN (obj);

  if (max < min)
    /* We prefer a bit vector in such case because it does not result
       in allocation.  */
    return false;

  nw = (max - min + IRA_INT_BITS) / IRA_INT_BITS;
  return (2 * sizeof (ira_object_t) * (num + 1)
	  < 3 * nw * sizeof (IRA_INT_TYPE));
}

/* Allocates and initialize the conflict vector of OBJ for NUM
   conflicting objects.  */
void
ira_allocate_conflict_vec (ira_object_t obj, int num)
{
  int size;
  ira_object_t *vec;

  ira_assert (OBJECT_CONFLICT_ARRAY (obj) == NULL);
  num++; /* for NULL end marker  */
  size = sizeof (ira_object_t) * num;
  OBJECT_CONFLICT_ARRAY (obj) = ira_allocate (size);
  vec = (ira_object_t *) OBJECT_CONFLICT_ARRAY (obj);
  vec[0] = NULL;
  OBJECT_NUM_CONFLICTS (obj) = 0;
  OBJECT_CONFLICT_ARRAY_SIZE (obj) = size;
  OBJECT_CONFLICT_VEC_P (obj) = true;
}

/* Allocate and initialize the conflict bit vector of OBJ.  */
static void
allocate_conflict_bit_vec (ira_object_t obj)
{
  unsigned int size;

  ira_assert (OBJECT_CONFLICT_ARRAY (obj) == NULL);
  size = ((OBJECT_MAX (obj) - OBJECT_MIN (obj) + IRA_INT_BITS)
	  / IRA_INT_BITS * sizeof (IRA_INT_TYPE));
  OBJECT_CONFLICT_ARRAY (obj) = ira_allocate (size);
  memset (OBJECT_CONFLICT_ARRAY (obj), 0, size);
  OBJECT_CONFLICT_ARRAY_SIZE (obj) = size;
  OBJECT_CONFLICT_VEC_P (obj) = false;
}

/* Allocate and initialize the conflict vector or conflict bit vector
   of OBJ for NUM conflicting allocnos whatever is more profitable.  */
void
ira_allocate_object_conflicts (ira_object_t obj, int num)
{
  if (ira_conflict_vector_profitable_p (obj, num))
    ira_allocate_conflict_vec (obj, num);
  else
    allocate_conflict_bit_vec (obj);
}

/* Add OBJ2 to the conflicts of OBJ1.  */
static void
add_to_conflicts (ira_object_t obj1, ira_object_t obj2)
{
  int num;
  unsigned int size;

  if (OBJECT_CONFLICT_VEC_P (obj1))
    {
      ira_object_t *vec = OBJECT_CONFLICT_VEC (obj1);
      int curr_num = OBJECT_NUM_CONFLICTS (obj1);
      num = curr_num + 2;
      if (OBJECT_CONFLICT_ARRAY_SIZE (obj1) < num * sizeof (ira_object_t))
	{
	  ira_object_t *newvec;
	  size = (3 * num / 2 + 1) * sizeof (ira_allocno_t);
	  newvec = (ira_object_t *) ira_allocate (size);
	  memcpy (newvec, vec, curr_num * sizeof (ira_object_t));
	  ira_free (vec);
	  vec = newvec;
	  OBJECT_CONFLICT_ARRAY (obj1) = vec;
	  OBJECT_CONFLICT_ARRAY_SIZE (obj1) = size;
	}
      vec[num - 2] = obj2;
      vec[num - 1] = NULL;
      OBJECT_NUM_CONFLICTS (obj1)++;
    }
  else
    {
      int nw, added_head_nw, id;
      IRA_INT_TYPE *vec = OBJECT_CONFLICT_BITVEC (obj1);

      id = OBJECT_CONFLICT_ID (obj2);
      if (OBJECT_MIN (obj1) > id)
	{
	  /* Expand head of the bit vector.  */
	  added_head_nw = (OBJECT_MIN (obj1) - id - 1) / IRA_INT_BITS + 1;
	  nw = (OBJECT_MAX (obj1) - OBJECT_MIN (obj1)) / IRA_INT_BITS + 1;
	  size = (nw + added_head_nw) * sizeof (IRA_INT_TYPE);
	  if (OBJECT_CONFLICT_ARRAY_SIZE (obj1) >= size)
	    {
	      memmove ((char *) vec + added_head_nw * sizeof (IRA_INT_TYPE),
		       vec, nw * sizeof (IRA_INT_TYPE));
	      memset (vec, 0, added_head_nw * sizeof (IRA_INT_TYPE));
	    }
	  else
	    {
	      size
		= (3 * (nw + added_head_nw) / 2 + 1) * sizeof (IRA_INT_TYPE);
	      vec = (IRA_INT_TYPE *) ira_allocate (size);
	      memcpy ((char *) vec + added_head_nw * sizeof (IRA_INT_TYPE),
		      OBJECT_CONFLICT_ARRAY (obj1), nw * sizeof (IRA_INT_TYPE));
	      memset (vec, 0, added_head_nw * sizeof (IRA_INT_TYPE));
	      memset ((char *) vec
		      + (nw + added_head_nw) * sizeof (IRA_INT_TYPE),
		      0, size - (nw + added_head_nw) * sizeof (IRA_INT_TYPE));
	      ira_free (OBJECT_CONFLICT_ARRAY (obj1));
	      OBJECT_CONFLICT_ARRAY (obj1) = vec;
	      OBJECT_CONFLICT_ARRAY_SIZE (obj1) = size;
	    }
	  OBJECT_MIN (obj1) -= added_head_nw * IRA_INT_BITS;
	}
      else if (OBJECT_MAX (obj1) < id)
	{
	  nw = (id - OBJECT_MIN (obj1)) / IRA_INT_BITS + 1;
	  size = nw * sizeof (IRA_INT_TYPE);
	  if (OBJECT_CONFLICT_ARRAY_SIZE (obj1) < size)
	    {
	      /* Expand tail of the bit vector.  */
	      size = (3 * nw / 2 + 1) * sizeof (IRA_INT_TYPE);
	      vec = (IRA_INT_TYPE *) ira_allocate (size);
	      memcpy (vec, OBJECT_CONFLICT_ARRAY (obj1), OBJECT_CONFLICT_ARRAY_SIZE (obj1));
	      memset ((char *) vec + OBJECT_CONFLICT_ARRAY_SIZE (obj1),
		      0, size - OBJECT_CONFLICT_ARRAY_SIZE (obj1));
	      ira_free (OBJECT_CONFLICT_ARRAY (obj1));
	      OBJECT_CONFLICT_ARRAY (obj1) = vec;
	      OBJECT_CONFLICT_ARRAY_SIZE (obj1) = size;
	    }
	  OBJECT_MAX (obj1) = id;
	}
      SET_MINMAX_SET_BIT (vec, id, OBJECT_MIN (obj1), OBJECT_MAX (obj1));
    }
}

/* Add OBJ1 to the conflicts of OBJ2 and vice versa.  */
static void
ira_add_conflict (ira_object_t obj1, ira_object_t obj2)
{
  add_to_conflicts (obj1, obj2);
  add_to_conflicts (obj2, obj1);
}

/* Clear all conflicts of OBJ.  */
static void
clear_conflicts (ira_object_t obj)
{
  if (OBJECT_CONFLICT_VEC_P (obj))
    {
      OBJECT_NUM_CONFLICTS (obj) = 0;
      OBJECT_CONFLICT_VEC (obj)[0] = NULL;
    }
  else if (OBJECT_CONFLICT_ARRAY_SIZE (obj) != 0)
    {
      int nw;

      nw = (OBJECT_MAX (obj) - OBJECT_MIN (obj)) / IRA_INT_BITS + 1;
      memset (OBJECT_CONFLICT_BITVEC (obj), 0, nw * sizeof (IRA_INT_TYPE));
    }
}

/* The array used to find duplications in conflict vectors of
   allocnos.  */
static int *conflict_check;

/* The value used to mark allocation presence in conflict vector of
   the current allocno.  */
static int curr_conflict_check_tick;

/* Remove duplications in conflict vector of OBJ.  */
static void
compress_conflict_vec (ira_object_t obj)
{
  ira_object_t *vec, conflict_obj;
  int i, j;

  ira_assert (OBJECT_CONFLICT_VEC_P (obj));
  vec = OBJECT_CONFLICT_VEC (obj);
  curr_conflict_check_tick++;
  for (i = j = 0; (conflict_obj = vec[i]) != NULL; i++)
    {
      int id = OBJECT_CONFLICT_ID (conflict_obj);
      if (conflict_check[id] != curr_conflict_check_tick)
	{
	  conflict_check[id] = curr_conflict_check_tick;
	  vec[j++] = conflict_obj;
	}
    }
  OBJECT_NUM_CONFLICTS (obj) = j;
  vec[j] = NULL;
}

/* Remove duplications in conflict vectors of all allocnos.  */
static void
compress_conflict_vecs (void)
{
  ira_object_t obj;
  ira_object_iterator oi;

  conflict_check = (int *) ira_allocate (sizeof (int) * ira_objects_num);
  memset (conflict_check, 0, sizeof (int) * ira_objects_num);
  curr_conflict_check_tick = 0;
  FOR_EACH_OBJECT (obj, oi)
    {
      if (OBJECT_CONFLICT_VEC_P (obj))
	compress_conflict_vec (obj);
    }
  ira_free (conflict_check);
}

/* This recursive function outputs allocno A and if it is a cap the
   function outputs its members.  */
void
ira_print_expanded_allocno (ira_allocno_t a)
{
  basic_block bb;

  fprintf (ira_dump_file, " a%d(r%d", ALLOCNO_NUM (a), ALLOCNO_REGNO (a));
  if ((bb = ALLOCNO_LOOP_TREE_NODE (a)->bb) != NULL)
    fprintf (ira_dump_file, ",b%d", bb->index);
  else
    fprintf (ira_dump_file, ",l%d", ALLOCNO_LOOP_TREE_NODE (a)->loop_num);
  if (ALLOCNO_CAP_MEMBER (a) != NULL)
    {
      fprintf (ira_dump_file, ":");
      ira_print_expanded_allocno (ALLOCNO_CAP_MEMBER (a));
    }
  fprintf (ira_dump_file, ")");
}

/* Create and return the cap representing allocno A in the
   parent loop.  */
static ira_allocno_t
create_cap_allocno (ira_allocno_t a)
{
  ira_allocno_t cap;
  ira_loop_tree_node_t parent;
  enum reg_class aclass;

  parent = ALLOCNO_LOOP_TREE_NODE (a)->parent;
  cap = ira_create_allocno (ALLOCNO_REGNO (a), true, parent);
  ALLOCNO_MODE (cap) = ALLOCNO_MODE (a);
  aclass = ALLOCNO_CLASS (a);
  ira_set_allocno_class (cap, aclass);
  ira_create_allocno_objects (cap);
  ALLOCNO_CAP_MEMBER (cap) = a;
  ALLOCNO_CAP (a) = cap;
  ALLOCNO_CLASS_COST (cap) = ALLOCNO_CLASS_COST (a);
  ALLOCNO_MEMORY_COST (cap) = ALLOCNO_MEMORY_COST (a);
  ira_allocate_and_copy_costs
    (&ALLOCNO_HARD_REG_COSTS (cap), aclass, ALLOCNO_HARD_REG_COSTS (a));
  ira_allocate_and_copy_costs
    (&ALLOCNO_CONFLICT_HARD_REG_COSTS (cap), aclass,
     ALLOCNO_CONFLICT_HARD_REG_COSTS (a));
  ALLOCNO_BAD_SPILL_P (cap) = ALLOCNO_BAD_SPILL_P (a);
  ALLOCNO_NREFS (cap) = ALLOCNO_NREFS (a);
  ALLOCNO_FREQ (cap) = ALLOCNO_FREQ (a);
  ALLOCNO_CALL_FREQ (cap) = ALLOCNO_CALL_FREQ (a);

  merge_hard_reg_conflicts (a, cap, false);

  ALLOCNO_CALLS_CROSSED_NUM (cap) = ALLOCNO_CALLS_CROSSED_NUM (a);
  ALLOCNO_CHEAP_CALLS_CROSSED_NUM (cap) = ALLOCNO_CHEAP_CALLS_CROSSED_NUM (a);
  if (internal_flag_ira_verbose > 2 && ira_dump_file != NULL)
    {
      fprintf (ira_dump_file, "    Creating cap ");
      ira_print_expanded_allocno (cap);
      fprintf (ira_dump_file, "\n");
    }
  return cap;
}

/* Create and return a live range for OBJECT with given attributes.  */
live_range_t
ira_create_live_range (ira_object_t obj, int start, int finish,
		       live_range_t next)
{
  live_range_t p;

  p = (live_range_t) pool_alloc (live_range_pool);
  p->object = obj;
  p->start = start;
  p->finish = finish;
  p->next = next;
  return p;
}

/* Create a new live range for OBJECT and queue it at the head of its
   live range list.  */
void
ira_add_live_range_to_object (ira_object_t object, int start, int finish)
{
  live_range_t p;
  p = ira_create_live_range (object, start, finish,
			     OBJECT_LIVE_RANGES (object));
  OBJECT_LIVE_RANGES (object) = p;
}

/* Copy allocno live range R and return the result.  */
static live_range_t
copy_live_range (live_range_t r)
{
  live_range_t p;

  p = (live_range_t) pool_alloc (live_range_pool);
  *p = *r;
  return p;
}

/* Copy allocno live range list given by its head R and return the
   result.  */
live_range_t
ira_copy_live_range_list (live_range_t r)
{
  live_range_t p, first, last;

  if (r == NULL)
    return NULL;
  for (first = last = NULL; r != NULL; r = r->next)
    {
      p = copy_live_range (r);
      if (first == NULL)
	first = p;
      else
	last->next = p;
      last = p;
    }
  return first;
}

/* Merge ranges R1 and R2 and returns the result.  The function
   maintains the order of ranges and tries to minimize number of the
   result ranges.  */
live_range_t
ira_merge_live_ranges (live_range_t r1, live_range_t r2)
{
  live_range_t first, last, temp;

  if (r1 == NULL)
    return r2;
  if (r2 == NULL)
    return r1;
  for (first = last = NULL; r1 != NULL && r2 != NULL;)
    {
      if (r1->start < r2->start)
	{
	  temp = r1;
	  r1 = r2;
	  r2 = temp;
	}
      if (r1->start <= r2->finish + 1)
	{
	  /* Intersected ranges: merge r1 and r2 into r1.  */
	  r1->start = r2->start;
	  if (r1->finish < r2->finish)
	    r1->finish = r2->finish;
	  temp = r2;
	  r2 = r2->next;
	  ira_finish_live_range (temp);
	  if (r2 == NULL)
	    {
	      /* To try to merge with subsequent ranges in r1.  */
	      r2 = r1->next;
	      r1->next = NULL;
	    }
	}
      else
	{
	  /* Add r1 to the result.  */
	  if (first == NULL)
	    first = last = r1;
	  else
	    {
	      last->next = r1;
	      last = r1;
	    }
	  r1 = r1->next;
	  if (r1 == NULL)
	    {
	      /* To try to merge with subsequent ranges in r2.  */
	      r1 = r2->next;
	      r2->next = NULL;
	    }
	}
    }
  if (r1 != NULL)
    {
      if (first == NULL)
	first = r1;
      else
	last->next = r1;
      ira_assert (r1->next == NULL);
    }
  else if (r2 != NULL)
    {
      if (first == NULL)
	first = r2;
      else
	last->next = r2;
      ira_assert (r2->next == NULL);
    }
  else
    {
      ira_assert (last->next == NULL);
    }
  return first;
}

/* Return TRUE if live ranges R1 and R2 intersect.  */
bool
ira_live_ranges_intersect_p (live_range_t r1, live_range_t r2)
{
  /* Remember the live ranges are always kept ordered.  */
  while (r1 != NULL && r2 != NULL)
    {
      if (r1->start > r2->finish)
	r1 = r1->next;
      else if (r2->start > r1->finish)
	r2 = r2->next;
      else
	return true;
    }
  return false;
}

/* Free allocno live range R.  */
void
ira_finish_live_range (live_range_t r)
{
  pool_free (live_range_pool, r);
}

/* Free list of allocno live ranges starting with R.  */
void
ira_finish_live_range_list (live_range_t r)
{
  live_range_t next_r;

  for (; r != NULL; r = next_r)
    {
      next_r = r->next;
      ira_finish_live_range (r);
    }
}

/* Free updated register costs of allocno A.  */
void
ira_free_allocno_updated_costs (ira_allocno_t a)
{
  enum reg_class aclass;

  aclass = ALLOCNO_CLASS (a);
  if (ALLOCNO_UPDATED_HARD_REG_COSTS (a) != NULL)
    ira_free_cost_vector (ALLOCNO_UPDATED_HARD_REG_COSTS (a), aclass);
  ALLOCNO_UPDATED_HARD_REG_COSTS (a) = NULL;
  if (ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS (a) != NULL)
    ira_free_cost_vector (ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS (a),
			  aclass);
  ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS (a) = NULL;
}

/* Free and nullify all cost vectors allocated earlier for allocno
   A.  */
static void
ira_free_allocno_costs (ira_allocno_t a)
{
  enum reg_class aclass = ALLOCNO_CLASS (a);
  ira_object_t obj;
  ira_allocno_object_iterator oi;

  FOR_EACH_ALLOCNO_OBJECT (a, obj, oi)
    {
      ira_finish_live_range_list (OBJECT_LIVE_RANGES (obj));
      ira_object_id_map[OBJECT_CONFLICT_ID (obj)] = NULL;
      if (OBJECT_CONFLICT_ARRAY (obj) != NULL)
	ira_free (OBJECT_CONFLICT_ARRAY (obj));
      pool_free (object_pool, obj);
    }

  ira_allocnos[ALLOCNO_NUM (a)] = NULL;
  if (ALLOCNO_HARD_REG_COSTS (a) != NULL)
    ira_free_cost_vector (ALLOCNO_HARD_REG_COSTS (a), aclass);
  if (ALLOCNO_CONFLICT_HARD_REG_COSTS (a) != NULL)
    ira_free_cost_vector (ALLOCNO_CONFLICT_HARD_REG_COSTS (a), aclass);
  if (ALLOCNO_UPDATED_HARD_REG_COSTS (a) != NULL)
    ira_free_cost_vector (ALLOCNO_UPDATED_HARD_REG_COSTS (a), aclass);
  if (ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS (a) != NULL)
    ira_free_cost_vector (ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS (a),
			  aclass);
  ALLOCNO_HARD_REG_COSTS (a) = NULL;
  ALLOCNO_CONFLICT_HARD_REG_COSTS (a) = NULL;
  ALLOCNO_UPDATED_HARD_REG_COSTS (a) = NULL;
  ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS (a) = NULL;
}

/* Free the memory allocated for allocno A.  */
static void
finish_allocno (ira_allocno_t a)
{
  ira_free_allocno_costs (a);
  pool_free (allocno_pool, a);
}

/* Free the memory allocated for all allocnos.  */
static void
finish_allocnos (void)
{
  ira_allocno_t a;
  ira_allocno_iterator ai;

  FOR_EACH_ALLOCNO (a, ai)
    finish_allocno (a);
  ira_free (ira_regno_allocno_map);
  ira_object_id_map_vec.release ();
  allocno_vec.release ();
  free_alloc_pool (allocno_pool);
  free_alloc_pool (object_pool);
  free_alloc_pool (live_range_pool);
}



/* Pools for allocno preferences.  */
static alloc_pool pref_pool;

/* Vec containing references to all created preferences.  It is a
   container of array ira_prefs.  */
static vec<ira_pref_t> pref_vec;

/* The function initializes data concerning allocno prefs.  */
static void
initiate_prefs (void)
{
  pref_pool
    = create_alloc_pool ("prefs", sizeof (struct ira_allocno_pref), 100);
  pref_vec.create (get_max_uid ());
  ira_prefs = NULL;
  ira_prefs_num = 0;
}

/* Return pref for A and HARD_REGNO if any.  */
static ira_pref_t
find_allocno_pref (ira_allocno_t a, int hard_regno)
{
  ira_pref_t pref;

  for (pref = ALLOCNO_PREFS (a); pref != NULL; pref = pref->next_pref)
    if (pref->allocno == a && pref->hard_regno == hard_regno)
      return pref;
  return NULL;
}

/* Create and return pref with given attributes A, HARD_REGNO, and FREQ.  */
ira_pref_t
ira_create_pref (ira_allocno_t a, int hard_regno, int freq)
{
  ira_pref_t pref;

  pref = (ira_pref_t) pool_alloc (pref_pool);
  pref->num = ira_prefs_num;
  pref->allocno = a;
  pref->hard_regno = hard_regno;
  pref->freq = freq;
  pref_vec.safe_push (pref);
  ira_prefs = pref_vec.address ();
  ira_prefs_num = pref_vec.length ();
  return pref;
}

/* Attach a pref PREF to the cooresponding allocno.  */
static void
add_allocno_pref_to_list (ira_pref_t pref)
{
  ira_allocno_t a = pref->allocno;

  pref->next_pref = ALLOCNO_PREFS (a);
  ALLOCNO_PREFS (a) = pref;
}

/* Create (or update frequency if the pref already exists) the pref of
   allocnos A preferring HARD_REGNO with frequency FREQ.  */
void
ira_add_allocno_pref (ira_allocno_t a, int hard_regno, int freq)
{
  ira_pref_t pref;

  if (freq <= 0)
    return;
  if ((pref = find_allocno_pref (a, hard_regno)) != NULL)
    {
      pref->freq += freq;
      return;
    }
  pref = ira_create_pref (a, hard_regno, freq);
  ira_assert (a != NULL);
  add_allocno_pref_to_list (pref);
}

/* Print info about PREF into file F.  */
static void
print_pref (FILE *f, ira_pref_t pref)
{
  fprintf (f, "  pref%d:a%d(r%d)<-hr%d@%d\n", pref->num,
	   ALLOCNO_NUM (pref->allocno), ALLOCNO_REGNO (pref->allocno),
	   pref->hard_regno, pref->freq);
}

/* Print info about PREF into stderr.  */
void
ira_debug_pref (ira_pref_t pref)
{
  print_pref (stderr, pref);
}

/* Print info about all prefs into file F.  */
static void
print_prefs (FILE *f)
{
  ira_pref_t pref;
  ira_pref_iterator pi;

  FOR_EACH_PREF (pref, pi)
    print_pref (f, pref);
}

/* Print info about all prefs into stderr.  */
void
ira_debug_prefs (void)
{
  print_prefs (stderr);
}

/* Print info about prefs involving allocno A into file F.  */
static void
print_allocno_prefs (FILE *f, ira_allocno_t a)
{
  ira_pref_t pref;

  fprintf (f, " a%d(r%d):", ALLOCNO_NUM (a), ALLOCNO_REGNO (a));
  for (pref = ALLOCNO_PREFS (a); pref != NULL; pref = pref->next_pref)
    fprintf (f, " pref%d:hr%d@%d", pref->num, pref->hard_regno, pref->freq);
  fprintf (f, "\n");
}

/* Print info about prefs involving allocno A into stderr.  */
void
ira_debug_allocno_prefs (ira_allocno_t a)
{
  print_allocno_prefs (stderr, a);
}

/* The function frees memory allocated for PREF.  */
static void
finish_pref (ira_pref_t pref)
{
  ira_prefs[pref->num] = NULL;
  pool_free (pref_pool, pref);
}

/* Remove PREF from the list of allocno prefs and free memory for
   it.  */
void
ira_remove_pref (ira_pref_t pref)
{
  ira_pref_t cpref, prev;

  if (internal_flag_ira_verbose > 1 && ira_dump_file != NULL)
    fprintf (ira_dump_file, " Removing pref%d:hr%d@%d\n",
	     pref->num, pref->hard_regno, pref->freq);
  for (prev = NULL, cpref = ALLOCNO_PREFS (pref->allocno);
       cpref != NULL;
       prev = cpref, cpref = cpref->next_pref)
    if (cpref == pref)
      break;
  ira_assert (cpref != NULL);
  if (prev == NULL)
    ALLOCNO_PREFS (pref->allocno) = pref->next_pref;
  else
    prev->next_pref = pref->next_pref;
  finish_pref (pref);
}

/* Remove all prefs of allocno A.  */
void
ira_remove_allocno_prefs (ira_allocno_t a)
{
  ira_pref_t pref, next_pref;

  for (pref = ALLOCNO_PREFS (a); pref != NULL; pref = next_pref)
    {
      next_pref = pref->next_pref;
      finish_pref (pref);
    }
  ALLOCNO_PREFS (a) = NULL;
}

/* Free memory allocated for all prefs.  */
static void
finish_prefs (void)
{
  ira_pref_t pref;
  ira_pref_iterator pi;

  FOR_EACH_PREF (pref, pi)
    finish_pref (pref);
  pref_vec.release ();
  free_alloc_pool (pref_pool);
}



/* Pools for copies.  */
static alloc_pool copy_pool;

/* Vec containing references to all created copies.  It is a
   container of array ira_copies.  */
static vec<ira_copy_t> copy_vec;

/* The function initializes data concerning allocno copies.  */
static void
initiate_copies (void)
{
  copy_pool
    = create_alloc_pool ("copies", sizeof (struct ira_allocno_copy), 100);
  copy_vec.create (get_max_uid ());
  ira_copies = NULL;
  ira_copies_num = 0;
}

/* Return copy connecting A1 and A2 and originated from INSN of
   LOOP_TREE_NODE if any.  */
static ira_copy_t
find_allocno_copy (ira_allocno_t a1, ira_allocno_t a2, rtx insn,
		   ira_loop_tree_node_t loop_tree_node)
{
  ira_copy_t cp, next_cp;
  ira_allocno_t another_a;

  for (cp = ALLOCNO_COPIES (a1); cp != NULL; cp = next_cp)
    {
      if (cp->first == a1)
	{
	  next_cp = cp->next_first_allocno_copy;
	  another_a = cp->second;
	}
      else if (cp->second == a1)
	{
	  next_cp = cp->next_second_allocno_copy;
	  another_a = cp->first;
	}
      else
	gcc_unreachable ();
      if (another_a == a2 && cp->insn == insn
	  && cp->loop_tree_node == loop_tree_node)
	return cp;
    }
  return NULL;
}

/* Create and return copy with given attributes LOOP_TREE_NODE, FIRST,
   SECOND, FREQ, CONSTRAINT_P, and INSN.  */
ira_copy_t
ira_create_copy (ira_allocno_t first, ira_allocno_t second, int freq,
		 bool constraint_p, rtx insn,
		 ira_loop_tree_node_t loop_tree_node)
{
  ira_copy_t cp;

  cp = (ira_copy_t) pool_alloc (copy_pool);
  cp->num = ira_copies_num;
  cp->first = first;
  cp->second = second;
  cp->freq = freq;
  cp->constraint_p = constraint_p;
  cp->insn = insn;
  cp->loop_tree_node = loop_tree_node;
  copy_vec.safe_push (cp);
  ira_copies = copy_vec.address ();
  ira_copies_num = copy_vec.length ();
  return cp;
}

/* Attach a copy CP to allocnos involved into the copy.  */
static void
add_allocno_copy_to_list (ira_copy_t cp)
{
  ira_allocno_t first = cp->first, second = cp->second;

  cp->prev_first_allocno_copy = NULL;
  cp->prev_second_allocno_copy = NULL;
  cp->next_first_allocno_copy = ALLOCNO_COPIES (first);
  if (cp->next_first_allocno_copy != NULL)
    {
      if (cp->next_first_allocno_copy->first == first)
	cp->next_first_allocno_copy->prev_first_allocno_copy = cp;
      else
	cp->next_first_allocno_copy->prev_second_allocno_copy = cp;
    }
  cp->next_second_allocno_copy = ALLOCNO_COPIES (second);
  if (cp->next_second_allocno_copy != NULL)
    {
      if (cp->next_second_allocno_copy->second == second)
	cp->next_second_allocno_copy->prev_second_allocno_copy = cp;
      else
	cp->next_second_allocno_copy->prev_first_allocno_copy = cp;
    }
  ALLOCNO_COPIES (first) = cp;
  ALLOCNO_COPIES (second) = cp;
}

/* Make a copy CP a canonical copy where number of the
   first allocno is less than the second one.  */
static void
swap_allocno_copy_ends_if_necessary (ira_copy_t cp)
{
  ira_allocno_t temp;
  ira_copy_t temp_cp;

  if (ALLOCNO_NUM (cp->first) <= ALLOCNO_NUM (cp->second))
    return;

  temp = cp->first;
  cp->first = cp->second;
  cp->second = temp;

  temp_cp = cp->prev_first_allocno_copy;
  cp->prev_first_allocno_copy = cp->prev_second_allocno_copy;
  cp->prev_second_allocno_copy = temp_cp;

  temp_cp = cp->next_first_allocno_copy;
  cp->next_first_allocno_copy = cp->next_second_allocno_copy;
  cp->next_second_allocno_copy = temp_cp;
}

/* Create (or update frequency if the copy already exists) and return
   the copy of allocnos FIRST and SECOND with frequency FREQ
   corresponding to move insn INSN (if any) and originated from
   LOOP_TREE_NODE.  */
ira_copy_t
ira_add_allocno_copy (ira_allocno_t first, ira_allocno_t second, int freq,
		      bool constraint_p, rtx insn,
		      ira_loop_tree_node_t loop_tree_node)
{
  ira_copy_t cp;

  if ((cp = find_allocno_copy (first, second, insn, loop_tree_node)) != NULL)
    {
      cp->freq += freq;
      return cp;
    }
  cp = ira_create_copy (first, second, freq, constraint_p, insn,
			loop_tree_node);
  ira_assert (first != NULL && second != NULL);
  add_allocno_copy_to_list (cp);
  swap_allocno_copy_ends_if_necessary (cp);
  return cp;
}

/* Print info about copy CP into file F.  */
static void
print_copy (FILE *f, ira_copy_t cp)
{
  fprintf (f, "  cp%d:a%d(r%d)<->a%d(r%d)@%d:%s\n", cp->num,
	   ALLOCNO_NUM (cp->first), ALLOCNO_REGNO (cp->first),
	   ALLOCNO_NUM (cp->second), ALLOCNO_REGNO (cp->second), cp->freq,
	   cp->insn != NULL
	   ? "move" : cp->constraint_p ? "constraint" : "shuffle");
}

DEBUG_FUNCTION void
debug (ira_allocno_copy &ref)
{
  print_copy (stderr, &ref);
}

DEBUG_FUNCTION void
debug (ira_allocno_copy *ptr)
{
  if (ptr)
    debug (*ptr);
  else
    fprintf (stderr, "<nil>\n");
}

/* Print info about copy CP into stderr.  */
void
ira_debug_copy (ira_copy_t cp)
{
  print_copy (stderr, cp);
}

/* Print info about all copies into file F.  */
static void
print_copies (FILE *f)
{
  ira_copy_t cp;
  ira_copy_iterator ci;

  FOR_EACH_COPY (cp, ci)
    print_copy (f, cp);
}

/* Print info about all copies into stderr.  */
void
ira_debug_copies (void)
{
  print_copies (stderr);
}

/* Print info about copies involving allocno A into file F.  */
static void
print_allocno_copies (FILE *f, ira_allocno_t a)
{
  ira_allocno_t another_a;
  ira_copy_t cp, next_cp;

  fprintf (f, " a%d(r%d):", ALLOCNO_NUM (a), ALLOCNO_REGNO (a));
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
      fprintf (f, " cp%d:a%d(r%d)@%d", cp->num,
	       ALLOCNO_NUM (another_a), ALLOCNO_REGNO (another_a), cp->freq);
    }
  fprintf (f, "\n");
}

DEBUG_FUNCTION void
debug (ira_allocno &ref)
{
  print_allocno_copies (stderr, &ref);
}

DEBUG_FUNCTION void
debug (ira_allocno *ptr)
{
  if (ptr)
    debug (*ptr);
  else
    fprintf (stderr, "<nil>\n");
}


/* Print info about copies involving allocno A into stderr.  */
void
ira_debug_allocno_copies (ira_allocno_t a)
{
  print_allocno_copies (stderr, a);
}

/* The function frees memory allocated for copy CP.  */
static void
finish_copy (ira_copy_t cp)
{
  pool_free (copy_pool, cp);
}


/* Free memory allocated for all copies.  */
static void
finish_copies (void)
{
  ira_copy_t cp;
  ira_copy_iterator ci;

  FOR_EACH_COPY (cp, ci)
    finish_copy (cp);
  copy_vec.release ();
  free_alloc_pool (copy_pool);
}



/* Pools for cost vectors.  It is defined only for allocno classes.  */
static alloc_pool cost_vector_pool[N_REG_CLASSES];

/* The function initiates work with hard register cost vectors.  It
   creates allocation pool for each allocno class.  */
static void
initiate_cost_vectors (void)
{
  int i;
  enum reg_class aclass;

  for (i = 0; i < ira_allocno_classes_num; i++)
    {
      aclass = ira_allocno_classes[i];
      cost_vector_pool[aclass]
	= create_alloc_pool ("cost vectors",
			     sizeof (int) * ira_class_hard_regs_num[aclass],
			     100);
    }
}

/* Allocate and return a cost vector VEC for ACLASS.  */
int *
ira_allocate_cost_vector (reg_class_t aclass)
{
  return (int *) pool_alloc (cost_vector_pool[(int) aclass]);
}

/* Free a cost vector VEC for ACLASS.  */
void
ira_free_cost_vector (int *vec, reg_class_t aclass)
{
  ira_assert (vec != NULL);
  pool_free (cost_vector_pool[(int) aclass], vec);
}

/* Finish work with hard register cost vectors.  Release allocation
   pool for each allocno class.  */
static void
finish_cost_vectors (void)
{
  int i;
  enum reg_class aclass;

  for (i = 0; i < ira_allocno_classes_num; i++)
    {
      aclass = ira_allocno_classes[i];
      free_alloc_pool (cost_vector_pool[aclass]);
    }
}



/* Compute a post-ordering of the reverse control flow of the loop body
   designated by the children nodes of LOOP_NODE, whose body nodes in
   pre-order are input as LOOP_PREORDER.  Return a VEC with a post-order
   of the reverse loop body.

   For the post-order of the reverse CFG, we visit the basic blocks in
   LOOP_PREORDER array in the reverse order of where they appear.
   This is important: We do not just want to compute a post-order of
   the reverse CFG, we want to make a best-guess for a visiting order that
   minimizes the number of chain elements per allocno live range.  If the
   blocks would be visited in a different order, we would still compute a
   correct post-ordering but it would be less likely that two nodes
   connected by an edge in the CFG are neighbours in the topsort.  */

static vec<ira_loop_tree_node_t>
ira_loop_tree_body_rev_postorder (ira_loop_tree_node_t loop_node ATTRIBUTE_UNUSED,
				  vec<ira_loop_tree_node_t> loop_preorder)
{
  vec<ira_loop_tree_node_t> topsort_nodes = vNULL;
  unsigned int n_loop_preorder;

  n_loop_preorder = loop_preorder.length ();
  if (n_loop_preorder != 0)
    {
      ira_loop_tree_node_t subloop_node;
      unsigned int i;
      auto_vec<ira_loop_tree_node_t> dfs_stack;

      /* This is a bit of strange abuse of the BB_VISITED flag:  We use
	 the flag to mark blocks we still have to visit to add them to
	 our post-order.  Define an alias to avoid confusion.  */
#define BB_TO_VISIT BB_VISITED

      FOR_EACH_VEC_ELT (loop_preorder, i, subloop_node)
	{
	  gcc_checking_assert (! (subloop_node->bb->flags & BB_TO_VISIT));
	  subloop_node->bb->flags |= BB_TO_VISIT;
	}

      topsort_nodes.create (n_loop_preorder);
      dfs_stack.create (n_loop_preorder);

      FOR_EACH_VEC_ELT_REVERSE (loop_preorder, i, subloop_node)
	{
	  if (! (subloop_node->bb->flags & BB_TO_VISIT))
	    continue;

	  subloop_node->bb->flags &= ~BB_TO_VISIT;
	  dfs_stack.quick_push (subloop_node);
	  while (! dfs_stack.is_empty ())
	    {
	      edge e;
	      edge_iterator ei;

	      ira_loop_tree_node_t n = dfs_stack.last ();
	      FOR_EACH_EDGE (e, ei, n->bb->preds)
		{
		  ira_loop_tree_node_t pred_node;
		  basic_block pred_bb = e->src;

		  if (e->src == ENTRY_BLOCK_PTR_FOR_FN (cfun))
		    continue;

		  pred_node = IRA_BB_NODE_BY_INDEX (pred_bb->index);
		  if (pred_node != n
		      && (pred_node->bb->flags & BB_TO_VISIT))
		    {
		      pred_node->bb->flags &= ~BB_TO_VISIT;
		      dfs_stack.quick_push (pred_node);
		    }
		}
	      if (n == dfs_stack.last ())
		{
		  dfs_stack.pop ();
		  topsort_nodes.quick_push (n);
		}
	    }
	}

#undef BB_TO_VISIT
    }

  gcc_assert (topsort_nodes.length () == n_loop_preorder);
  return topsort_nodes;
}

/* The current loop tree node and its regno allocno map.  */
ira_loop_tree_node_t ira_curr_loop_tree_node;
ira_allocno_t *ira_curr_regno_allocno_map;

/* This recursive function traverses loop tree with root LOOP_NODE
   calling non-null functions PREORDER_FUNC and POSTORDER_FUNC
   correspondingly in preorder and postorder.  The function sets up
   IRA_CURR_LOOP_TREE_NODE and IRA_CURR_REGNO_ALLOCNO_MAP.  If BB_P,
   basic block nodes of LOOP_NODE is also processed (before its
   subloop nodes).
   
   If BB_P is set and POSTORDER_FUNC is given, the basic blocks in
   the loop are passed in the *reverse* post-order of the *reverse*
   CFG.  This is only used by ira_create_allocno_live_ranges, which
   wants to visit basic blocks in this order to minimize the number
   of elements per live range chain.
   Note that the loop tree nodes are still visited in the normal,
   forward post-order of  the loop tree.  */

void
ira_traverse_loop_tree (bool bb_p, ira_loop_tree_node_t loop_node,
			void (*preorder_func) (ira_loop_tree_node_t),
			void (*postorder_func) (ira_loop_tree_node_t))
{
  ira_loop_tree_node_t subloop_node;

  ira_assert (loop_node->bb == NULL);
  ira_curr_loop_tree_node = loop_node;
  ira_curr_regno_allocno_map = ira_curr_loop_tree_node->regno_allocno_map;

  if (preorder_func != NULL)
    (*preorder_func) (loop_node);

  if (bb_p)
    {
      auto_vec<ira_loop_tree_node_t> loop_preorder;
      unsigned int i;

      /* Add all nodes to the set of nodes to visit.  The IRA loop tree
	 is set up such that nodes in the loop body appear in a pre-order
	 of their place in the CFG.  */
      for (subloop_node = loop_node->children;
	   subloop_node != NULL;
	   subloop_node = subloop_node->next)
	if (subloop_node->bb != NULL)
	  loop_preorder.safe_push (subloop_node);

      if (preorder_func != NULL)
	FOR_EACH_VEC_ELT (loop_preorder, i, subloop_node)
	  (*preorder_func) (subloop_node);

      if (postorder_func != NULL)
	{
	  vec<ira_loop_tree_node_t> loop_rev_postorder =
	    ira_loop_tree_body_rev_postorder (loop_node, loop_preorder);
	  FOR_EACH_VEC_ELT_REVERSE (loop_rev_postorder, i, subloop_node)
	    (*postorder_func) (subloop_node);
	  loop_rev_postorder.release ();
	}
    }

  for (subloop_node = loop_node->subloops;
       subloop_node != NULL;
       subloop_node = subloop_node->subloop_next)
    {
      ira_assert (subloop_node->bb == NULL);
      ira_traverse_loop_tree (bb_p, subloop_node,
			      preorder_func, postorder_func);
    }

  ira_curr_loop_tree_node = loop_node;
  ira_curr_regno_allocno_map = ira_curr_loop_tree_node->regno_allocno_map;

  if (postorder_func != NULL)
    (*postorder_func) (loop_node);
}



/* The basic block currently being processed.  */
static basic_block curr_bb;

/* This recursive function creates allocnos corresponding to
   pseudo-registers containing in X.  True OUTPUT_P means that X is
   a lvalue.  */
static void
create_insn_allocnos (rtx x, bool output_p)
{
  int i, j;
  const char *fmt;
  enum rtx_code code = GET_CODE (x);

  if (code == REG)
    {
      int regno;

      if ((regno = REGNO (x)) >= FIRST_PSEUDO_REGISTER)
	{
	  ira_allocno_t a;

	  if ((a = ira_curr_regno_allocno_map[regno]) == NULL)
	    a = ira_create_allocno (regno, false, ira_curr_loop_tree_node);

	  ALLOCNO_NREFS (a)++;
	  ALLOCNO_FREQ (a) += REG_FREQ_FROM_BB (curr_bb);
	  if (output_p)
	    bitmap_set_bit (ira_curr_loop_tree_node->modified_regnos, regno);
	}
      return;
    }
  else if (code == SET)
    {
      create_insn_allocnos (SET_DEST (x), true);
      create_insn_allocnos (SET_SRC (x), false);
      return;
    }
  else if (code == CLOBBER)
    {
      create_insn_allocnos (XEXP (x, 0), true);
      return;
    }
  else if (code == MEM)
    {
      create_insn_allocnos (XEXP (x, 0), false);
      return;
    }
  else if (code == PRE_DEC || code == POST_DEC || code == PRE_INC ||
	   code == POST_INC || code == POST_MODIFY || code == PRE_MODIFY)
    {
      create_insn_allocnos (XEXP (x, 0), true);
      create_insn_allocnos (XEXP (x, 0), false);
      return;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	create_insn_allocnos (XEXP (x, i), output_p);
      else if (fmt[i] == 'E')
	for (j = 0; j < XVECLEN (x, i); j++)
	  create_insn_allocnos (XVECEXP (x, i, j), output_p);
    }
}

/* Create allocnos corresponding to pseudo-registers living in the
   basic block represented by the corresponding loop tree node
   BB_NODE.  */
static void
create_bb_allocnos (ira_loop_tree_node_t bb_node)
{
  basic_block bb;
  rtx insn;
  unsigned int i;
  bitmap_iterator bi;

  curr_bb = bb = bb_node->bb;
  ira_assert (bb != NULL);
  FOR_BB_INSNS_REVERSE (bb, insn)
    if (NONDEBUG_INSN_P (insn))
      create_insn_allocnos (PATTERN (insn), false);
  /* It might be a allocno living through from one subloop to
     another.  */
  EXECUTE_IF_SET_IN_REG_SET (df_get_live_in (bb), FIRST_PSEUDO_REGISTER, i, bi)
    if (ira_curr_regno_allocno_map[i] == NULL)
      ira_create_allocno (i, false, ira_curr_loop_tree_node);
}

/* Create allocnos corresponding to pseudo-registers living on edge E
   (a loop entry or exit).  Also mark the allocnos as living on the
   loop border. */
static void
create_loop_allocnos (edge e)
{
  unsigned int i;
  bitmap live_in_regs, border_allocnos;
  bitmap_iterator bi;
  ira_loop_tree_node_t parent;

  live_in_regs = df_get_live_in (e->dest);
  border_allocnos = ira_curr_loop_tree_node->border_allocnos;
  EXECUTE_IF_SET_IN_REG_SET (df_get_live_out (e->src),
			     FIRST_PSEUDO_REGISTER, i, bi)
    if (bitmap_bit_p (live_in_regs, i))
      {
	if (ira_curr_regno_allocno_map[i] == NULL)
	  {
	    /* The order of creations is important for right
	       ira_regno_allocno_map.  */
	    if ((parent = ira_curr_loop_tree_node->parent) != NULL
		&& parent->regno_allocno_map[i] == NULL)
	      ira_create_allocno (i, false, parent);
	    ira_create_allocno (i, false, ira_curr_loop_tree_node);
	  }
	bitmap_set_bit (border_allocnos,
			ALLOCNO_NUM (ira_curr_regno_allocno_map[i]));
      }
}

/* Create allocnos corresponding to pseudo-registers living in loop
   represented by the corresponding loop tree node LOOP_NODE.  This
   function is called by ira_traverse_loop_tree.  */
static void
create_loop_tree_node_allocnos (ira_loop_tree_node_t loop_node)
{
  if (loop_node->bb != NULL)
    create_bb_allocnos (loop_node);
  else if (loop_node != ira_loop_tree_root)
    {
      int i;
      edge_iterator ei;
      edge e;
      vec<edge> edges;

      ira_assert (current_loops != NULL);
      FOR_EACH_EDGE (e, ei, loop_node->loop->header->preds)
	if (e->src != loop_node->loop->latch)
	  create_loop_allocnos (e);

      edges = get_loop_exit_edges (loop_node->loop);
      FOR_EACH_VEC_ELT (edges, i, e)
	create_loop_allocnos (e);
      edges.release ();
    }
}

/* Propagate information about allocnos modified inside the loop given
   by its LOOP_TREE_NODE to its parent.  */
static void
propagate_modified_regnos (ira_loop_tree_node_t loop_tree_node)
{
  if (loop_tree_node == ira_loop_tree_root)
    return;
  ira_assert (loop_tree_node->bb == NULL);
  bitmap_ior_into (loop_tree_node->parent->modified_regnos,
		   loop_tree_node->modified_regnos);
}

/* Propagate new info about allocno A (see comments about accumulated
   info in allocno definition) to the corresponding allocno on upper
   loop tree level.  So allocnos on upper levels accumulate
   information about the corresponding allocnos in nested regions.
   The new info means allocno info finally calculated in this
   file.  */
static void
propagate_allocno_info (void)
{
  int i;
  ira_allocno_t a, parent_a;
  ira_loop_tree_node_t parent;
  enum reg_class aclass;

  if (flag_ira_region != IRA_REGION_ALL
      && flag_ira_region != IRA_REGION_MIXED)
    return;
  for (i = max_reg_num () - 1; i >= FIRST_PSEUDO_REGISTER; i--)
    for (a = ira_regno_allocno_map[i];
	 a != NULL;
	 a = ALLOCNO_NEXT_REGNO_ALLOCNO (a))
      if ((parent = ALLOCNO_LOOP_TREE_NODE (a)->parent) != NULL
	  && (parent_a = parent->regno_allocno_map[i]) != NULL
	  /* There are no caps yet at this point.  So use
	     border_allocnos to find allocnos for the propagation.  */
	  && bitmap_bit_p (ALLOCNO_LOOP_TREE_NODE (a)->border_allocnos,
			   ALLOCNO_NUM (a)))
	{
	  if (! ALLOCNO_BAD_SPILL_P (a))
	    ALLOCNO_BAD_SPILL_P (parent_a) = false;
	  ALLOCNO_NREFS (parent_a) += ALLOCNO_NREFS (a);
	  ALLOCNO_FREQ (parent_a) += ALLOCNO_FREQ (a);
	  ALLOCNO_CALL_FREQ (parent_a) += ALLOCNO_CALL_FREQ (a);
	  merge_hard_reg_conflicts (a, parent_a, true);
	  ALLOCNO_CALLS_CROSSED_NUM (parent_a)
	    += ALLOCNO_CALLS_CROSSED_NUM (a);
	  ALLOCNO_CHEAP_CALLS_CROSSED_NUM (parent_a)
	    += ALLOCNO_CHEAP_CALLS_CROSSED_NUM (a);
	  ALLOCNO_EXCESS_PRESSURE_POINTS_NUM (parent_a)
	    += ALLOCNO_EXCESS_PRESSURE_POINTS_NUM (a);
	  aclass = ALLOCNO_CLASS (a);
	  ira_assert (aclass == ALLOCNO_CLASS (parent_a));
	  ira_allocate_and_accumulate_costs
	    (&ALLOCNO_HARD_REG_COSTS (parent_a), aclass,
	     ALLOCNO_HARD_REG_COSTS (a));
	  ira_allocate_and_accumulate_costs
	    (&ALLOCNO_CONFLICT_HARD_REG_COSTS (parent_a),
	     aclass,
	     ALLOCNO_CONFLICT_HARD_REG_COSTS (a));
	  ALLOCNO_CLASS_COST (parent_a)
	    += ALLOCNO_CLASS_COST (a);
	  ALLOCNO_MEMORY_COST (parent_a) += ALLOCNO_MEMORY_COST (a);
	}
}

/* Create allocnos corresponding to pseudo-registers in the current
   function.  Traverse the loop tree for this.  */
static void
create_allocnos (void)
{
  /* We need to process BB first to correctly link allocnos by member
     next_regno_allocno.  */
  ira_traverse_loop_tree (true, ira_loop_tree_root,
			  create_loop_tree_node_allocnos, NULL);
  if (optimize)
    ira_traverse_loop_tree (false, ira_loop_tree_root, NULL,
			    propagate_modified_regnos);
}



/* The page contains function to remove some regions from a separate
   register allocation.  We remove regions whose separate allocation
   will hardly improve the result.  As a result we speed up regional
   register allocation.  */

/* The function changes the object in range list given by R to OBJ.  */
static void
change_object_in_range_list (live_range_t r, ira_object_t obj)
{
  for (; r != NULL; r = r->next)
    r->object = obj;
}

/* Move all live ranges associated with allocno FROM to allocno TO.  */
static void
move_allocno_live_ranges (ira_allocno_t from, ira_allocno_t to)
{
  int i;
  int n = ALLOCNO_NUM_OBJECTS (from);

  gcc_assert (n == ALLOCNO_NUM_OBJECTS (to));

  for (i = 0; i < n; i++)
    {
      ira_object_t from_obj = ALLOCNO_OBJECT (from, i);
      ira_object_t to_obj = ALLOCNO_OBJECT (to, i);
      live_range_t lr = OBJECT_LIVE_RANGES (from_obj);

      if (internal_flag_ira_verbose > 4 && ira_dump_file != NULL)
	{
	  fprintf (ira_dump_file,
		   "      Moving ranges of a%dr%d to a%dr%d: ",
		   ALLOCNO_NUM (from), ALLOCNO_REGNO (from),
		   ALLOCNO_NUM (to), ALLOCNO_REGNO (to));
	  ira_print_live_range_list (ira_dump_file, lr);
	}
      change_object_in_range_list (lr, to_obj);
      OBJECT_LIVE_RANGES (to_obj)
	= ira_merge_live_ranges (lr, OBJECT_LIVE_RANGES (to_obj));
      OBJECT_LIVE_RANGES (from_obj) = NULL;
    }
}

static void
copy_allocno_live_ranges (ira_allocno_t from, ira_allocno_t to)
{
  int i;
  int n = ALLOCNO_NUM_OBJECTS (from);

  gcc_assert (n == ALLOCNO_NUM_OBJECTS (to));

  for (i = 0; i < n; i++)
    {
      ira_object_t from_obj = ALLOCNO_OBJECT (from, i);
      ira_object_t to_obj = ALLOCNO_OBJECT (to, i);
      live_range_t lr = OBJECT_LIVE_RANGES (from_obj);

      if (internal_flag_ira_verbose > 4 && ira_dump_file != NULL)
	{
	  fprintf (ira_dump_file, "      Copying ranges of a%dr%d to a%dr%d: ",
		   ALLOCNO_NUM (from), ALLOCNO_REGNO (from),
		   ALLOCNO_NUM (to), ALLOCNO_REGNO (to));
	  ira_print_live_range_list (ira_dump_file, lr);
	}
      lr = ira_copy_live_range_list (lr);
      change_object_in_range_list (lr, to_obj);
      OBJECT_LIVE_RANGES (to_obj)
	= ira_merge_live_ranges (lr, OBJECT_LIVE_RANGES (to_obj));
    }
}

/* Return TRUE if NODE represents a loop with low register
   pressure.  */
static bool
low_pressure_loop_node_p (ira_loop_tree_node_t node)
{
  int i;
  enum reg_class pclass;

  if (node->bb != NULL)
    return false;

  for (i = 0; i < ira_pressure_classes_num; i++)
    {
      pclass = ira_pressure_classes[i];
      if (node->reg_pressure[pclass] > ira_class_hard_regs_num[pclass]
	  && ira_class_hard_regs_num[pclass] > 1)
	return false;
    }
  return true;
}

#ifdef STACK_REGS
/* Return TRUE if LOOP has a complex enter or exit edge.  We don't
   form a region from such loop if the target use stack register
   because reg-stack.c can not deal with such edges.  */
static bool
loop_with_complex_edge_p (struct loop *loop)
{
  int i;
  edge_iterator ei;
  edge e;
  vec<edge> edges;
  bool res;

  FOR_EACH_EDGE (e, ei, loop->header->preds)
    if (e->flags & EDGE_EH)
      return true;
  edges = get_loop_exit_edges (loop);
  res = false;
  FOR_EACH_VEC_ELT (edges, i, e)
    if (e->flags & EDGE_COMPLEX)
      {
	res = true;
	break;
      }
  edges.release ();
  return res;
}
#endif

/* Sort loops for marking them for removal.  We put already marked
   loops first, then less frequent loops next, and then outer loops
   next.  */
static int
loop_compare_func (const void *v1p, const void *v2p)
{
  int diff;
  ira_loop_tree_node_t l1 = *(const ira_loop_tree_node_t *) v1p;
  ira_loop_tree_node_t l2 = *(const ira_loop_tree_node_t *) v2p;

  ira_assert (l1->parent != NULL && l2->parent != NULL);
  if (l1->to_remove_p && ! l2->to_remove_p)
    return -1;
  if (! l1->to_remove_p && l2->to_remove_p)
    return 1;
  if ((diff = l1->loop->header->frequency - l2->loop->header->frequency) != 0)
    return diff;
  if ((diff = (int) loop_depth (l1->loop) - (int) loop_depth (l2->loop)) != 0)
    return diff;
  /* Make sorting stable.  */
  return l1->loop_num - l2->loop_num;
}

/* Mark loops which should be removed from regional allocation.  We
   remove a loop with low register pressure inside another loop with
   register pressure.  In this case a separate allocation of the loop
   hardly helps (for irregular register file architecture it could
   help by choosing a better hard register in the loop but we prefer
   faster allocation even in this case).  We also remove cheap loops
   if there are more than IRA_MAX_LOOPS_NUM of them.  Loop with EH
   exit or enter edges are removed too because the allocation might
   require put pseudo moves on the EH edges (we could still do this
   for pseudos with caller saved hard registers in some cases but it
   is impossible to say here or during top-down allocation pass what
   hard register the pseudos get finally).  */
static void
mark_loops_for_removal (void)
{
  int i, n;
  ira_loop_tree_node_t *sorted_loops;
  loop_p loop;

  ira_assert (current_loops != NULL);
  sorted_loops
    = (ira_loop_tree_node_t *) ira_allocate (sizeof (ira_loop_tree_node_t)
					     * number_of_loops (cfun));
  for (n = i = 0; vec_safe_iterate (get_loops (cfun), i, &loop); i++)
    if (ira_loop_nodes[i].regno_allocno_map != NULL)
      {
	if (ira_loop_nodes[i].parent == NULL)
	  {
	    /* Don't remove the root.  */
	    ira_loop_nodes[i].to_remove_p = false;
	    continue;
	  }
	sorted_loops[n++] = &ira_loop_nodes[i];
	ira_loop_nodes[i].to_remove_p
	  = ((low_pressure_loop_node_p (ira_loop_nodes[i].parent)
	      && low_pressure_loop_node_p (&ira_loop_nodes[i]))
#ifdef STACK_REGS
	     || loop_with_complex_edge_p (ira_loop_nodes[i].loop)
#endif
	     );
      }
  qsort (sorted_loops, n, sizeof (ira_loop_tree_node_t), loop_compare_func);
  for (i = 0; n - i + 1 > IRA_MAX_LOOPS_NUM; i++)
    {
      sorted_loops[i]->to_remove_p = true;
      if (internal_flag_ira_verbose > 1 && ira_dump_file != NULL)
	fprintf
	  (ira_dump_file,
	   "  Mark loop %d (header %d, freq %d, depth %d) for removal (%s)\n",
	   sorted_loops[i]->loop_num, sorted_loops[i]->loop->header->index,
	   sorted_loops[i]->loop->header->frequency,
	   loop_depth (sorted_loops[i]->loop),
	   low_pressure_loop_node_p (sorted_loops[i]->parent)
	   && low_pressure_loop_node_p (sorted_loops[i])
	   ? "low pressure" : "cheap loop");
    }
  ira_free (sorted_loops);
}

/* Mark all loops but root for removing.  */
static void
mark_all_loops_for_removal (void)
{
  int i;
  loop_p loop;

  ira_assert (current_loops != NULL);
  FOR_EACH_VEC_SAFE_ELT (get_loops (cfun), i, loop)
    if (ira_loop_nodes[i].regno_allocno_map != NULL)
      {
	if (ira_loop_nodes[i].parent == NULL)
	  {
	    /* Don't remove the root.  */
	    ira_loop_nodes[i].to_remove_p = false;
	    continue;
	  }
	ira_loop_nodes[i].to_remove_p = true;
	if (internal_flag_ira_verbose > 1 && ira_dump_file != NULL)
	  fprintf
	    (ira_dump_file,
	     "  Mark loop %d (header %d, freq %d, depth %d) for removal\n",
	     ira_loop_nodes[i].loop_num,
	     ira_loop_nodes[i].loop->header->index,
	     ira_loop_nodes[i].loop->header->frequency,
	     loop_depth (ira_loop_nodes[i].loop));
      }
}

/* Definition of vector of loop tree nodes.  */

/* Vec containing references to all removed loop tree nodes.  */
static vec<ira_loop_tree_node_t> removed_loop_vec;

/* Vec containing references to all children of loop tree nodes.  */
static vec<ira_loop_tree_node_t> children_vec;

/* Remove subregions of NODE if their separate allocation will not
   improve the result.  */
static void
remove_uneccesary_loop_nodes_from_loop_tree (ira_loop_tree_node_t node)
{
  unsigned int start;
  bool remove_p;
  ira_loop_tree_node_t subnode;

  remove_p = node->to_remove_p;
  if (! remove_p)
    children_vec.safe_push (node);
  start = children_vec.length ();
  for (subnode = node->children; subnode != NULL; subnode = subnode->next)
    if (subnode->bb == NULL)
      remove_uneccesary_loop_nodes_from_loop_tree (subnode);
    else
      children_vec.safe_push (subnode);
  node->children = node->subloops = NULL;
  if (remove_p)
    {
      removed_loop_vec.safe_push (node);
      return;
    }
  while (children_vec.length () > start)
    {
      subnode = children_vec.pop ();
      subnode->parent = node;
      subnode->next = node->children;
      node->children = subnode;
      if (subnode->bb == NULL)
	{
	  subnode->subloop_next = node->subloops;
	  node->subloops = subnode;
	}
    }
}

/* Return TRUE if NODE is inside PARENT.  */
static bool
loop_is_inside_p (ira_loop_tree_node_t node, ira_loop_tree_node_t parent)
{
  for (node = node->parent; node != NULL; node = node->parent)
    if (node == parent)
      return true;
  return false;
}

/* Sort allocnos according to their order in regno allocno list.  */
static int
regno_allocno_order_compare_func (const void *v1p, const void *v2p)
{
  ira_allocno_t a1 = *(const ira_allocno_t *) v1p;
  ira_allocno_t a2 = *(const ira_allocno_t *) v2p;
  ira_loop_tree_node_t n1 = ALLOCNO_LOOP_TREE_NODE (a1);
  ira_loop_tree_node_t n2 = ALLOCNO_LOOP_TREE_NODE (a2);

  if (loop_is_inside_p (n1, n2))
    return -1;
  else if (loop_is_inside_p (n2, n1))
    return 1;
  /* If allocnos are equally good, sort by allocno numbers, so that
     the results of qsort leave nothing to chance.  We put allocnos
     with higher number first in the list because it is the original
     order for allocnos from loops on the same levels.  */
  return ALLOCNO_NUM (a2) - ALLOCNO_NUM (a1);
}

/* This array is used to sort allocnos to restore allocno order in
   the regno allocno list.  */
static ira_allocno_t *regno_allocnos;

/* Restore allocno order for REGNO in the regno allocno list.  */
static void
ira_rebuild_regno_allocno_list (int regno)
{
  int i, n;
  ira_allocno_t a;

  for (n = 0, a = ira_regno_allocno_map[regno];
       a != NULL;
       a = ALLOCNO_NEXT_REGNO_ALLOCNO (a))
    regno_allocnos[n++] = a;
  ira_assert (n > 0);
  qsort (regno_allocnos, n, sizeof (ira_allocno_t),
	 regno_allocno_order_compare_func);
  for (i = 1; i < n; i++)
    ALLOCNO_NEXT_REGNO_ALLOCNO (regno_allocnos[i - 1]) = regno_allocnos[i];
  ALLOCNO_NEXT_REGNO_ALLOCNO (regno_allocnos[n - 1]) = NULL;
  ira_regno_allocno_map[regno] = regno_allocnos[0];
  if (internal_flag_ira_verbose > 1 && ira_dump_file != NULL)
    fprintf (ira_dump_file, " Rebuilding regno allocno list for %d\n", regno);
}

/* Propagate info from allocno FROM_A to allocno A.  */
static void
propagate_some_info_from_allocno (ira_allocno_t a, ira_allocno_t from_a)
{
  enum reg_class aclass;

  merge_hard_reg_conflicts (from_a, a, false);
  ALLOCNO_NREFS (a) += ALLOCNO_NREFS (from_a);
  ALLOCNO_FREQ (a) += ALLOCNO_FREQ (from_a);
  ALLOCNO_CALL_FREQ (a) += ALLOCNO_CALL_FREQ (from_a);
  ALLOCNO_CALLS_CROSSED_NUM (a) += ALLOCNO_CALLS_CROSSED_NUM (from_a);
  ALLOCNO_CHEAP_CALLS_CROSSED_NUM (a)
    += ALLOCNO_CHEAP_CALLS_CROSSED_NUM (from_a);
  ALLOCNO_EXCESS_PRESSURE_POINTS_NUM (a)
    += ALLOCNO_EXCESS_PRESSURE_POINTS_NUM (from_a);
  if (! ALLOCNO_BAD_SPILL_P (from_a))
    ALLOCNO_BAD_SPILL_P (a) = false;
  aclass = ALLOCNO_CLASS (from_a);
  ira_assert (aclass == ALLOCNO_CLASS (a));
  ira_allocate_and_accumulate_costs (&ALLOCNO_HARD_REG_COSTS (a), aclass,
				     ALLOCNO_HARD_REG_COSTS (from_a));
  ira_allocate_and_accumulate_costs (&ALLOCNO_CONFLICT_HARD_REG_COSTS (a),
				     aclass,
				     ALLOCNO_CONFLICT_HARD_REG_COSTS (from_a));
  ALLOCNO_CLASS_COST (a) += ALLOCNO_CLASS_COST (from_a);
  ALLOCNO_MEMORY_COST (a) += ALLOCNO_MEMORY_COST (from_a);
}

/* Remove allocnos from loops removed from the allocation
   consideration.  */
static void
remove_unnecessary_allocnos (void)
{
  int regno;
  bool merged_p, rebuild_p;
  ira_allocno_t a, prev_a, next_a, parent_a;
  ira_loop_tree_node_t a_node, parent;

  merged_p = false;
  regno_allocnos = NULL;
  for (regno = max_reg_num () - 1; regno >= FIRST_PSEUDO_REGISTER; regno--)
    {
      rebuild_p = false;
      for (prev_a = NULL, a = ira_regno_allocno_map[regno];
	   a != NULL;
	   a = next_a)
	{
	  next_a = ALLOCNO_NEXT_REGNO_ALLOCNO (a);
	  a_node = ALLOCNO_LOOP_TREE_NODE (a);
	  if (! a_node->to_remove_p)
	    prev_a = a;
	  else
	    {
	      for (parent = a_node->parent;
		   (parent_a = parent->regno_allocno_map[regno]) == NULL
		     && parent->to_remove_p;
		   parent = parent->parent)
		;
	      if (parent_a == NULL)
		{
		  /* There are no allocnos with the same regno in
		     upper region -- just move the allocno to the
		     upper region.  */
		  prev_a = a;
		  ALLOCNO_LOOP_TREE_NODE (a) = parent;
		  parent->regno_allocno_map[regno] = a;
		  bitmap_set_bit (parent->all_allocnos, ALLOCNO_NUM (a));
		  rebuild_p = true;
		}
	      else
		{
		  /* Remove the allocno and update info of allocno in
		     the upper region.  */
		  if (prev_a == NULL)
		    ira_regno_allocno_map[regno] = next_a;
		  else
		    ALLOCNO_NEXT_REGNO_ALLOCNO (prev_a) = next_a;
		  move_allocno_live_ranges (a, parent_a);
		  merged_p = true;
		  propagate_some_info_from_allocno (parent_a, a);
		  /* Remove it from the corresponding regno allocno
		     map to avoid info propagation of subsequent
		     allocno into this already removed allocno.  */
		  a_node->regno_allocno_map[regno] = NULL;
		  ira_remove_allocno_prefs (a);
		  finish_allocno (a);
		}
	    }
	}
      if (rebuild_p)
	/* We need to restore the order in regno allocno list.  */
	{
	  if (regno_allocnos == NULL)
	    regno_allocnos
	      = (ira_allocno_t *) ira_allocate (sizeof (ira_allocno_t)
						* ira_allocnos_num);
	  ira_rebuild_regno_allocno_list (regno);
	}
    }
  if (merged_p)
    ira_rebuild_start_finish_chains ();
  if (regno_allocnos != NULL)
    ira_free (regno_allocnos);
}

/* Remove allocnos from all loops but the root.  */
static void
remove_low_level_allocnos (void)
{
  int regno;
  bool merged_p, propagate_p;
  ira_allocno_t a, top_a;
  ira_loop_tree_node_t a_node, parent;
  ira_allocno_iterator ai;

  merged_p = false;
  FOR_EACH_ALLOCNO (a, ai)
    {
      a_node = ALLOCNO_LOOP_TREE_NODE (a);
      if (a_node == ira_loop_tree_root || ALLOCNO_CAP_MEMBER (a) != NULL)
	continue;
      regno = ALLOCNO_REGNO (a);
      if ((top_a = ira_loop_tree_root->regno_allocno_map[regno]) == NULL)
	{
	  ALLOCNO_LOOP_TREE_NODE (a) = ira_loop_tree_root;
	  ira_loop_tree_root->regno_allocno_map[regno] = a;
	  continue;
	}
      propagate_p = a_node->parent->regno_allocno_map[regno] == NULL;
      /* Remove the allocno and update info of allocno in the upper
	 region.  */
      move_allocno_live_ranges (a, top_a);
      merged_p = true;
      if (propagate_p)
	propagate_some_info_from_allocno (top_a, a);
    }
  FOR_EACH_ALLOCNO (a, ai)
    {
      a_node = ALLOCNO_LOOP_TREE_NODE (a);
      if (a_node == ira_loop_tree_root)
	continue;
      parent = a_node->parent;
      regno = ALLOCNO_REGNO (a);
      if (ALLOCNO_CAP_MEMBER (a) != NULL)
	ira_assert (ALLOCNO_CAP (a) != NULL);
      else if (ALLOCNO_CAP (a) == NULL)
 	ira_assert (parent->regno_allocno_map[regno] != NULL);
    }
  FOR_EACH_ALLOCNO (a, ai)
    {
      regno = ALLOCNO_REGNO (a);
      if (ira_loop_tree_root->regno_allocno_map[regno] == a)
	{
	  ira_object_t obj;
	  ira_allocno_object_iterator oi;

	  ira_regno_allocno_map[regno] = a;
	  ALLOCNO_NEXT_REGNO_ALLOCNO (a) = NULL;
	  ALLOCNO_CAP_MEMBER (a) = NULL;
	  FOR_EACH_ALLOCNO_OBJECT (a, obj, oi)
	    COPY_HARD_REG_SET (OBJECT_CONFLICT_HARD_REGS (obj),
			       OBJECT_TOTAL_CONFLICT_HARD_REGS (obj));
#ifdef STACK_REGS
	  if (ALLOCNO_TOTAL_NO_STACK_REG_P (a))
	    ALLOCNO_NO_STACK_REG_P (a) = true;
#endif
	}
      else
	{
	  ira_remove_allocno_prefs (a);
	  finish_allocno (a);
	}
    }
  if (merged_p)
    ira_rebuild_start_finish_chains ();
}

/* Remove loops from consideration.  We remove all loops except for
   root if ALL_P or loops for which a separate allocation will not
   improve the result.  We have to do this after allocno creation and
   their costs and allocno class evaluation because only after that
   the register pressure can be known and is calculated.  */
static void
remove_unnecessary_regions (bool all_p)
{
  if (current_loops == NULL)
    return;
  if (all_p)
    mark_all_loops_for_removal ();
  else
    mark_loops_for_removal ();
  children_vec.create (last_basic_block_for_fn (cfun)
		       + number_of_loops (cfun));
  removed_loop_vec.create (last_basic_block_for_fn (cfun)
			   + number_of_loops (cfun));
  remove_uneccesary_loop_nodes_from_loop_tree (ira_loop_tree_root);
  children_vec.release ();
  if (all_p)
    remove_low_level_allocnos ();
  else
    remove_unnecessary_allocnos ();
  while (removed_loop_vec.length () > 0)
    finish_loop_tree_node (removed_loop_vec.pop ());
  removed_loop_vec.release ();
}



/* At this point true value of allocno attribute bad_spill_p means
   that there is an insn where allocno occurs and where the allocno
   can not be used as memory.  The function updates the attribute, now
   it can be true only for allocnos which can not be used as memory in
   an insn and in whose live ranges there is other allocno deaths.
   Spilling allocnos with true value will not improve the code because
   it will not make other allocnos colorable and additional reloads
   for the corresponding pseudo will be generated in reload pass for
   each insn it occurs.

   This is a trick mentioned in one classic article of Chaitin etc
   which is frequently omitted in other implementations of RA based on
   graph coloring.  */
static void
update_bad_spill_attribute (void)
{
  int i;
  ira_allocno_t a;
  ira_allocno_iterator ai;
  ira_allocno_object_iterator aoi;
  ira_object_t obj;
  live_range_t r;
  enum reg_class aclass;
  bitmap_head dead_points[N_REG_CLASSES];

  for (i = 0; i < ira_allocno_classes_num; i++)
    {
      aclass = ira_allocno_classes[i];
      bitmap_initialize (&dead_points[aclass], &reg_obstack);
    }
  FOR_EACH_ALLOCNO (a, ai)
    {
      aclass = ALLOCNO_CLASS (a);
      if (aclass == NO_REGS)
	continue;
      FOR_EACH_ALLOCNO_OBJECT (a, obj, aoi)
	for (r = OBJECT_LIVE_RANGES (obj); r != NULL; r = r->next)
	  bitmap_set_bit (&dead_points[aclass], r->finish);
    }
  FOR_EACH_ALLOCNO (a, ai)
    {
      aclass = ALLOCNO_CLASS (a);
      if (aclass == NO_REGS)
	continue;
      if (! ALLOCNO_BAD_SPILL_P (a))
	continue;
      FOR_EACH_ALLOCNO_OBJECT (a, obj, aoi)
	{
	  for (r = OBJECT_LIVE_RANGES (obj); r != NULL; r = r->next)
	    {
	      for (i = r->start + 1; i < r->finish; i++)
		if (bitmap_bit_p (&dead_points[aclass], i))
		  break;
	      if (i < r->finish)
		break;
	    }
	  if (r != NULL)
	    {
	      ALLOCNO_BAD_SPILL_P (a) = false;
	      break;
	    }
	}
    }
  for (i = 0; i < ira_allocno_classes_num; i++)
    {
      aclass = ira_allocno_classes[i];
      bitmap_clear (&dead_points[aclass]);
    }
}



/* Set up minimal and maximal live range points for allocnos.  */
static void
setup_min_max_allocno_live_range_point (void)
{
  int i;
  ira_allocno_t a, parent_a, cap;
  ira_allocno_iterator ai;
#ifdef ENABLE_IRA_CHECKING
  ira_object_iterator oi;
  ira_object_t obj;
#endif
  live_range_t r;
  ira_loop_tree_node_t parent;

  FOR_EACH_ALLOCNO (a, ai)
    {
      int n = ALLOCNO_NUM_OBJECTS (a);

      for (i = 0; i < n; i++)
	{
	  ira_object_t obj = ALLOCNO_OBJECT (a, i);
	  r = OBJECT_LIVE_RANGES (obj);
	  if (r == NULL)
	    continue;
	  OBJECT_MAX (obj) = r->finish;
	  for (; r->next != NULL; r = r->next)
	    ;
	  OBJECT_MIN (obj) = r->start;
	}
    }
  for (i = max_reg_num () - 1; i >= FIRST_PSEUDO_REGISTER; i--)
    for (a = ira_regno_allocno_map[i];
	 a != NULL;
	 a = ALLOCNO_NEXT_REGNO_ALLOCNO (a))
      {
	int j;
	int n = ALLOCNO_NUM_OBJECTS (a);

	for (j = 0; j < n; j++)
	  {
	    ira_object_t obj = ALLOCNO_OBJECT (a, j);
	    ira_object_t parent_obj;

	    if (OBJECT_MAX (obj) < 0)
	      continue;
	    ira_assert (ALLOCNO_CAP_MEMBER (a) == NULL);
	    /* Accumulation of range info.  */
	    if (ALLOCNO_CAP (a) != NULL)
	      {
		for (cap = ALLOCNO_CAP (a); cap != NULL; cap = ALLOCNO_CAP (cap))
		  {
		    ira_object_t cap_obj = ALLOCNO_OBJECT (cap, j);
		    if (OBJECT_MAX (cap_obj) < OBJECT_MAX (obj))
		      OBJECT_MAX (cap_obj) = OBJECT_MAX (obj);
		    if (OBJECT_MIN (cap_obj) > OBJECT_MIN (obj))
		      OBJECT_MIN (cap_obj) = OBJECT_MIN (obj);
		  }
		continue;
	      }
	    if ((parent = ALLOCNO_LOOP_TREE_NODE (a)->parent) == NULL)
	      continue;
	    parent_a = parent->regno_allocno_map[i];
	    parent_obj = ALLOCNO_OBJECT (parent_a, j);
	    if (OBJECT_MAX (parent_obj) < OBJECT_MAX (obj))
	      OBJECT_MAX (parent_obj) = OBJECT_MAX (obj);
	    if (OBJECT_MIN (parent_obj) > OBJECT_MIN (obj))
	      OBJECT_MIN (parent_obj) = OBJECT_MIN (obj);
	  }
      }
#ifdef ENABLE_IRA_CHECKING
  FOR_EACH_OBJECT (obj, oi)
    {
      if ((0 <= OBJECT_MIN (obj) && OBJECT_MIN (obj) <= ira_max_point)
	  && (0 <= OBJECT_MAX (obj) && OBJECT_MAX (obj) <= ira_max_point))
	continue;
      gcc_unreachable ();
    }
#endif
}

/* Sort allocnos according to their live ranges.  Allocnos with
   smaller allocno class are put first unless we use priority
   coloring.  Allocnos with the same class are ordered according
   their start (min).  Allocnos with the same start are ordered
   according their finish (max).  */
static int
object_range_compare_func (const void *v1p, const void *v2p)
{
  int diff;
  ira_object_t obj1 = *(const ira_object_t *) v1p;
  ira_object_t obj2 = *(const ira_object_t *) v2p;
  ira_allocno_t a1 = OBJECT_ALLOCNO (obj1);
  ira_allocno_t a2 = OBJECT_ALLOCNO (obj2);

  if ((diff = OBJECT_MIN (obj1) - OBJECT_MIN (obj2)) != 0)
    return diff;
  if ((diff = OBJECT_MAX (obj1) - OBJECT_MAX (obj2)) != 0)
     return diff;
  return ALLOCNO_NUM (a1) - ALLOCNO_NUM (a2);
}

/* Sort ira_object_id_map and set up conflict id of allocnos.  */
static void
sort_conflict_id_map (void)
{
  int i, num;
  ira_allocno_t a;
  ira_allocno_iterator ai;

  num = 0;
  FOR_EACH_ALLOCNO (a, ai)
    {
      ira_allocno_object_iterator oi;
      ira_object_t obj;

      FOR_EACH_ALLOCNO_OBJECT (a, obj, oi)
	ira_object_id_map[num++] = obj;
    }
  qsort (ira_object_id_map, num, sizeof (ira_object_t),
	 object_range_compare_func);
  for (i = 0; i < num; i++)
    {
      ira_object_t obj = ira_object_id_map[i];

      gcc_assert (obj != NULL);
      OBJECT_CONFLICT_ID (obj) = i;
    }
  for (i = num; i < ira_objects_num; i++)
    ira_object_id_map[i] = NULL;
}

/* Set up minimal and maximal conflict ids of allocnos with which
   given allocno can conflict.  */
static void
setup_min_max_conflict_allocno_ids (void)
{
  int aclass;
  int i, j, min, max, start, finish, first_not_finished, filled_area_start;
  int *live_range_min, *last_lived;
  int word0_min, word0_max;
  ira_allocno_t a;
  ira_allocno_iterator ai;

  live_range_min = (int *) ira_allocate (sizeof (int) * ira_objects_num);
  aclass = -1;
  first_not_finished = -1;
  for (i = 0; i < ira_objects_num; i++)
    {
      ira_object_t obj = ira_object_id_map[i];

      if (obj == NULL)
	continue;

      a = OBJECT_ALLOCNO (obj);

      if (aclass < 0)
	{
	  aclass = ALLOCNO_CLASS (a);
	  min = i;
	  first_not_finished = i;
	}
      else
	{
	  start = OBJECT_MIN (obj);
	  /* If we skip an allocno, the allocno with smaller ids will
	     be also skipped because of the secondary sorting the
	     range finishes (see function
	     object_range_compare_func).  */
	  while (first_not_finished < i
		 && start > OBJECT_MAX (ira_object_id_map
					[first_not_finished]))
	    first_not_finished++;
	  min = first_not_finished;
	}
      if (min == i)
	/* We could increase min further in this case but it is good
	   enough.  */
	min++;
      live_range_min[i] = OBJECT_MIN (obj);
      OBJECT_MIN (obj) = min;
    }
  last_lived = (int *) ira_allocate (sizeof (int) * ira_max_point);
  aclass = -1;
  filled_area_start = -1;
  for (i = ira_objects_num - 1; i >= 0; i--)
    {
      ira_object_t obj = ira_object_id_map[i];

      if (obj == NULL)
	continue;

      a = OBJECT_ALLOCNO (obj);
      if (aclass < 0)
	{
	  aclass = ALLOCNO_CLASS (a);
	  for (j = 0; j < ira_max_point; j++)
	    last_lived[j] = -1;
	  filled_area_start = ira_max_point;
	}
      min = live_range_min[i];
      finish = OBJECT_MAX (obj);
      max = last_lived[finish];
      if (max < 0)
	/* We could decrease max further in this case but it is good
	   enough.  */
	max = OBJECT_CONFLICT_ID (obj) - 1;
      OBJECT_MAX (obj) = max;
      /* In filling, we can go further A range finish to recognize
	 intersection quickly because if the finish of subsequently
	 processed allocno (it has smaller conflict id) range is
	 further A range finish than they are definitely intersected
	 (the reason for this is the allocnos with bigger conflict id
	 have their range starts not smaller than allocnos with
	 smaller ids.  */
      for (j = min; j < filled_area_start; j++)
	last_lived[j] = i;
      filled_area_start = min;
    }
  ira_free (last_lived);
  ira_free (live_range_min);

  /* For allocnos with more than one object, we may later record extra conflicts in
     subobject 0 that we cannot really know about here.
     For now, simply widen the min/max range of these subobjects.  */

  word0_min = INT_MAX;
  word0_max = INT_MIN;

  FOR_EACH_ALLOCNO (a, ai)
    {
      int n = ALLOCNO_NUM_OBJECTS (a);
      ira_object_t obj0;

      if (n < 2)
	continue;
      obj0 = ALLOCNO_OBJECT (a, 0);
      if (OBJECT_CONFLICT_ID (obj0) < word0_min)
	word0_min = OBJECT_CONFLICT_ID (obj0);
      if (OBJECT_CONFLICT_ID (obj0) > word0_max)
	word0_max = OBJECT_CONFLICT_ID (obj0);
    }
  FOR_EACH_ALLOCNO (a, ai)
    {
      int n = ALLOCNO_NUM_OBJECTS (a);
      ira_object_t obj0;

      if (n < 2)
	continue;
      obj0 = ALLOCNO_OBJECT (a, 0);
      if (OBJECT_MIN (obj0) > word0_min)
	OBJECT_MIN (obj0) = word0_min;
      if (OBJECT_MAX (obj0) < word0_max)
	OBJECT_MAX (obj0) = word0_max;
    }
}



static void
create_caps (void)
{
  ira_allocno_t a;
  ira_allocno_iterator ai;
  ira_loop_tree_node_t loop_tree_node;

  FOR_EACH_ALLOCNO (a, ai)
    {
      if (ALLOCNO_LOOP_TREE_NODE (a) == ira_loop_tree_root)
	continue;
      if (ALLOCNO_CAP_MEMBER (a) != NULL)
	create_cap_allocno (a);
      else if (ALLOCNO_CAP (a) == NULL)
	{
	  loop_tree_node = ALLOCNO_LOOP_TREE_NODE (a);
	  if (!bitmap_bit_p (loop_tree_node->border_allocnos, ALLOCNO_NUM (a)))
	    create_cap_allocno (a);
	}
    }
}



/* The page contains code transforming more one region internal
   representation (IR) to one region IR which is necessary for reload.
   This transformation is called IR flattening.  We might just rebuild
   the IR for one region but we don't do it because it takes a lot of
   time.  */

/* Map: regno -> allocnos which will finally represent the regno for
   IR with one region.  */
static ira_allocno_t *regno_top_level_allocno_map;

/* Find the allocno that corresponds to A at a level one higher up in the
   loop tree.  Returns NULL if A is a cap, or if it has no parent.  */
ira_allocno_t
ira_parent_allocno (ira_allocno_t a)
{
  ira_loop_tree_node_t parent;

  if (ALLOCNO_CAP (a) != NULL)
    return NULL;

  parent = ALLOCNO_LOOP_TREE_NODE (a)->parent;
  if (parent == NULL)
    return NULL;

  return parent->regno_allocno_map[ALLOCNO_REGNO (a)];
}

/* Find the allocno that corresponds to A at a level one higher up in the
   loop tree.  If ALLOCNO_CAP is set for A, return that.  */
ira_allocno_t
ira_parent_or_cap_allocno (ira_allocno_t a)
{
  if (ALLOCNO_CAP (a) != NULL)
    return ALLOCNO_CAP (a);

  return ira_parent_allocno (a);
}

/* Process all allocnos originated from pseudo REGNO and copy live
   ranges, hard reg conflicts, and allocno stack reg attributes from
   low level allocnos to final allocnos which are destinations of
   removed stores at a loop exit.  Return true if we copied live
   ranges.  */
static bool
copy_info_to_removed_store_destinations (int regno)
{
  ira_allocno_t a;
  ira_allocno_t parent_a = NULL;
  ira_loop_tree_node_t parent;
  bool merged_p;

  merged_p = false;
  for (a = ira_regno_allocno_map[regno];
       a != NULL;
       a = ALLOCNO_NEXT_REGNO_ALLOCNO (a))
    {
      if (a != regno_top_level_allocno_map[REGNO (allocno_emit_reg (a))])
	/* This allocno will be removed.  */
	continue;

      /* Caps will be removed.  */
      ira_assert (ALLOCNO_CAP_MEMBER (a) == NULL);
      for (parent = ALLOCNO_LOOP_TREE_NODE (a)->parent;
	   parent != NULL;
	   parent = parent->parent)
	if ((parent_a = parent->regno_allocno_map[regno]) == NULL
	    || (parent_a
		== regno_top_level_allocno_map[REGNO
					       (allocno_emit_reg (parent_a))]
		&& ALLOCNO_EMIT_DATA (parent_a)->mem_optimized_dest_p))
	  break;
      if (parent == NULL || parent_a == NULL)
	continue;

      copy_allocno_live_ranges (a, parent_a);
      merge_hard_reg_conflicts (a, parent_a, true);

      ALLOCNO_CALL_FREQ (parent_a) += ALLOCNO_CALL_FREQ (a);
      ALLOCNO_CALLS_CROSSED_NUM (parent_a)
	+= ALLOCNO_CALLS_CROSSED_NUM (a);
      ALLOCNO_CHEAP_CALLS_CROSSED_NUM (parent_a)
	+= ALLOCNO_CHEAP_CALLS_CROSSED_NUM (a);
      ALLOCNO_EXCESS_PRESSURE_POINTS_NUM (parent_a)
	+= ALLOCNO_EXCESS_PRESSURE_POINTS_NUM (a);
      merged_p = true;
    }
  return merged_p;
}

/* Flatten the IR.  In other words, this function transforms IR as if
   it were built with one region (without loops).  We could make it
   much simpler by rebuilding IR with one region, but unfortunately it
   takes a lot of time.  MAX_REGNO_BEFORE_EMIT and
   IRA_MAX_POINT_BEFORE_EMIT are correspondingly MAX_REG_NUM () and
   IRA_MAX_POINT before emitting insns on the loop borders.  */
void
ira_flattening (int max_regno_before_emit, int ira_max_point_before_emit)
{
  int i, j;
  bool keep_p;
  int hard_regs_num;
  bool new_pseudos_p, merged_p, mem_dest_p;
  unsigned int n;
  enum reg_class aclass;
  ira_allocno_t a, parent_a, first, second, node_first, node_second;
  ira_copy_t cp;
  ira_loop_tree_node_t node;
  live_range_t r;
  ira_allocno_iterator ai;
  ira_copy_iterator ci;

  regno_top_level_allocno_map
    = (ira_allocno_t *) ira_allocate (max_reg_num ()
				      * sizeof (ira_allocno_t));
  memset (regno_top_level_allocno_map, 0,
	  max_reg_num () * sizeof (ira_allocno_t));
  new_pseudos_p = merged_p = false;
  FOR_EACH_ALLOCNO (a, ai)
    {
      ira_allocno_object_iterator oi;
      ira_object_t obj;

      if (ALLOCNO_CAP_MEMBER (a) != NULL)
	/* Caps are not in the regno allocno maps and they are never
	   will be transformed into allocnos existing after IR
	   flattening.  */
	continue;
      FOR_EACH_ALLOCNO_OBJECT (a, obj, oi)
	COPY_HARD_REG_SET (OBJECT_TOTAL_CONFLICT_HARD_REGS (obj),
			   OBJECT_CONFLICT_HARD_REGS (obj));
#ifdef STACK_REGS
      ALLOCNO_TOTAL_NO_STACK_REG_P (a) = ALLOCNO_NO_STACK_REG_P (a);
#endif
    }
  /* Fix final allocno attributes.  */
  for (i = max_regno_before_emit - 1; i >= FIRST_PSEUDO_REGISTER; i--)
    {
      mem_dest_p = false;
      for (a = ira_regno_allocno_map[i];
	   a != NULL;
	   a = ALLOCNO_NEXT_REGNO_ALLOCNO (a))
	{
	  ira_emit_data_t parent_data, data = ALLOCNO_EMIT_DATA (a);

	  ira_assert (ALLOCNO_CAP_MEMBER (a) == NULL);
	  if (data->somewhere_renamed_p)
	    new_pseudos_p = true;
	  parent_a = ira_parent_allocno (a);
	  if (parent_a == NULL)
	    {
	      ALLOCNO_COPIES (a) = NULL;
	      regno_top_level_allocno_map[REGNO (data->reg)] = a;
	      continue;
	    }
	  ira_assert (ALLOCNO_CAP_MEMBER (parent_a) == NULL);

	  if (data->mem_optimized_dest != NULL)
	    mem_dest_p = true;
	  parent_data = ALLOCNO_EMIT_DATA (parent_a);
	  if (REGNO (data->reg) == REGNO (parent_data->reg))
	    {
	      merge_hard_reg_conflicts (a, parent_a, true);
	      move_allocno_live_ranges (a, parent_a);
	      merged_p = true;
	      parent_data->mem_optimized_dest_p
		= (parent_data->mem_optimized_dest_p
		   || data->mem_optimized_dest_p);
	      continue;
	    }
	  new_pseudos_p = true;
	  for (;;)
	    {
	      ALLOCNO_NREFS (parent_a) -= ALLOCNO_NREFS (a);
	      ALLOCNO_FREQ (parent_a) -= ALLOCNO_FREQ (a);
	      ALLOCNO_CALL_FREQ (parent_a) -= ALLOCNO_CALL_FREQ (a);
	      ALLOCNO_CALLS_CROSSED_NUM (parent_a)
		-= ALLOCNO_CALLS_CROSSED_NUM (a);
	      ALLOCNO_CHEAP_CALLS_CROSSED_NUM (parent_a)
		-= ALLOCNO_CHEAP_CALLS_CROSSED_NUM (a);
	      ALLOCNO_EXCESS_PRESSURE_POINTS_NUM (parent_a)
		-= ALLOCNO_EXCESS_PRESSURE_POINTS_NUM (a);
	      ira_assert (ALLOCNO_CALLS_CROSSED_NUM (parent_a) >= 0
			  && ALLOCNO_NREFS (parent_a) >= 0
			  && ALLOCNO_FREQ (parent_a) >= 0);
	      aclass = ALLOCNO_CLASS (parent_a);
	      hard_regs_num = ira_class_hard_regs_num[aclass];
	      if (ALLOCNO_HARD_REG_COSTS (a) != NULL
		  && ALLOCNO_HARD_REG_COSTS (parent_a) != NULL)
		for (j = 0; j < hard_regs_num; j++)
		  ALLOCNO_HARD_REG_COSTS (parent_a)[j]
		    -= ALLOCNO_HARD_REG_COSTS (a)[j];
	      if (ALLOCNO_CONFLICT_HARD_REG_COSTS (a) != NULL
		  && ALLOCNO_CONFLICT_HARD_REG_COSTS (parent_a) != NULL)
		for (j = 0; j < hard_regs_num; j++)
		  ALLOCNO_CONFLICT_HARD_REG_COSTS (parent_a)[j]
		    -= ALLOCNO_CONFLICT_HARD_REG_COSTS (a)[j];
	      ALLOCNO_CLASS_COST (parent_a)
		-= ALLOCNO_CLASS_COST (a);
	      ALLOCNO_MEMORY_COST (parent_a) -= ALLOCNO_MEMORY_COST (a);
	      parent_a = ira_parent_allocno (parent_a);
	      if (parent_a == NULL)
		break;
	    }
	  ALLOCNO_COPIES (a) = NULL;
	  regno_top_level_allocno_map[REGNO (data->reg)] = a;
	}
      if (mem_dest_p && copy_info_to_removed_store_destinations (i))
	merged_p = true;
    }
  ira_assert (new_pseudos_p || ira_max_point_before_emit == ira_max_point);
  if (merged_p || ira_max_point_before_emit != ira_max_point)
    ira_rebuild_start_finish_chains ();
  if (new_pseudos_p)
    {
      sparseset objects_live;

      /* Rebuild conflicts.  */
      FOR_EACH_ALLOCNO (a, ai)
	{
	  ira_allocno_object_iterator oi;
	  ira_object_t obj;

	  if (a != regno_top_level_allocno_map[REGNO (allocno_emit_reg (a))]
	      || ALLOCNO_CAP_MEMBER (a) != NULL)
	    continue;
	  FOR_EACH_ALLOCNO_OBJECT (a, obj, oi)
	    {
	      for (r = OBJECT_LIVE_RANGES (obj); r != NULL; r = r->next)
		ira_assert (r->object == obj);
	      clear_conflicts (obj);
	    }
	}
      objects_live = sparseset_alloc (ira_objects_num);
      for (i = 0; i < ira_max_point; i++)
	{
	  for (r = ira_start_point_ranges[i]; r != NULL; r = r->start_next)
	    {
	      ira_object_t obj = r->object;

	      a = OBJECT_ALLOCNO (obj);
	      if (a != regno_top_level_allocno_map[REGNO (allocno_emit_reg (a))]
		  || ALLOCNO_CAP_MEMBER (a) != NULL)
		continue;

	      aclass = ALLOCNO_CLASS (a);
	      sparseset_set_bit (objects_live, OBJECT_CONFLICT_ID (obj));
	      EXECUTE_IF_SET_IN_SPARSESET (objects_live, n)
		{
		  ira_object_t live_obj = ira_object_id_map[n];
		  ira_allocno_t live_a = OBJECT_ALLOCNO (live_obj);
		  enum reg_class live_aclass = ALLOCNO_CLASS (live_a);

		  if (ira_reg_classes_intersect_p[aclass][live_aclass]
		      /* Don't set up conflict for the allocno with itself.  */
		      && live_a != a)
		    ira_add_conflict (obj, live_obj);
		}
	    }

	  for (r = ira_finish_point_ranges[i]; r != NULL; r = r->finish_next)
	    sparseset_clear_bit (objects_live, OBJECT_CONFLICT_ID (r->object));
	}
      sparseset_free (objects_live);
      compress_conflict_vecs ();
    }
  /* Mark some copies for removing and change allocnos in the rest
     copies.  */
  FOR_EACH_COPY (cp, ci)
    {
      if (ALLOCNO_CAP_MEMBER (cp->first) != NULL
	  || ALLOCNO_CAP_MEMBER (cp->second) != NULL)
	{
	  if (internal_flag_ira_verbose > 4 && ira_dump_file != NULL)
	    fprintf
	      (ira_dump_file, "      Remove cp%d:%c%dr%d-%c%dr%d\n",
	       cp->num, ALLOCNO_CAP_MEMBER (cp->first) != NULL ? 'c' : 'a',
	       ALLOCNO_NUM (cp->first),
	       REGNO (allocno_emit_reg (cp->first)),
	       ALLOCNO_CAP_MEMBER (cp->second) != NULL ? 'c' : 'a',
	       ALLOCNO_NUM (cp->second),
	       REGNO (allocno_emit_reg (cp->second)));
	  cp->loop_tree_node = NULL;
	  continue;
	}
      first
	= regno_top_level_allocno_map[REGNO (allocno_emit_reg (cp->first))];
      second
	= regno_top_level_allocno_map[REGNO (allocno_emit_reg (cp->second))];
      node = cp->loop_tree_node;
      if (node == NULL)
	keep_p = true; /* It copy generated in ira-emit.c.  */
      else
	{
	  /* Check that the copy was not propagated from level on
	     which we will have different pseudos.  */
	  node_first = node->regno_allocno_map[ALLOCNO_REGNO (cp->first)];
	  node_second = node->regno_allocno_map[ALLOCNO_REGNO (cp->second)];
	  keep_p = ((REGNO (allocno_emit_reg (first))
		     == REGNO (allocno_emit_reg (node_first)))
		     && (REGNO (allocno_emit_reg (second))
			 == REGNO (allocno_emit_reg (node_second))));
	}
      if (keep_p)
	{
	  cp->loop_tree_node = ira_loop_tree_root;
	  cp->first = first;
	  cp->second = second;
	}
      else
	{
	  cp->loop_tree_node = NULL;
	  if (internal_flag_ira_verbose > 4 && ira_dump_file != NULL)
	    fprintf (ira_dump_file, "      Remove cp%d:a%dr%d-a%dr%d\n",
		     cp->num, ALLOCNO_NUM (cp->first),
		     REGNO (allocno_emit_reg (cp->first)),
		     ALLOCNO_NUM (cp->second),
		     REGNO (allocno_emit_reg (cp->second)));
	}
    }
  /* Remove unnecessary allocnos on lower levels of the loop tree.  */
  FOR_EACH_ALLOCNO (a, ai)
    {
      if (a != regno_top_level_allocno_map[REGNO (allocno_emit_reg (a))]
	  || ALLOCNO_CAP_MEMBER (a) != NULL)
	{
	  if (internal_flag_ira_verbose > 4 && ira_dump_file != NULL)
	    fprintf (ira_dump_file, "      Remove a%dr%d\n",
		     ALLOCNO_NUM (a), REGNO (allocno_emit_reg (a)));
	  ira_remove_allocno_prefs (a);
	  finish_allocno (a);
	  continue;
	}
      ALLOCNO_LOOP_TREE_NODE (a) = ira_loop_tree_root;
      ALLOCNO_REGNO (a) = REGNO (allocno_emit_reg (a));
      ALLOCNO_CAP (a) = NULL;
      /* Restore updated costs for assignments from reload.  */
      ALLOCNO_UPDATED_MEMORY_COST (a) = ALLOCNO_MEMORY_COST (a);
      ALLOCNO_UPDATED_CLASS_COST (a) = ALLOCNO_CLASS_COST (a);
      if (! ALLOCNO_ASSIGNED_P (a))
	ira_free_allocno_updated_costs (a);
      ira_assert (ALLOCNO_UPDATED_HARD_REG_COSTS (a) == NULL);
      ira_assert (ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS (a) == NULL);
    }
  /* Remove unnecessary copies.  */
  FOR_EACH_COPY (cp, ci)
    {
      if (cp->loop_tree_node == NULL)
	{
	  ira_copies[cp->num] = NULL;
	  finish_copy (cp);
	  continue;
	}
      ira_assert
	(ALLOCNO_LOOP_TREE_NODE (cp->first) == ira_loop_tree_root
	 && ALLOCNO_LOOP_TREE_NODE (cp->second) == ira_loop_tree_root);
      add_allocno_copy_to_list (cp);
      swap_allocno_copy_ends_if_necessary (cp);
    }
  rebuild_regno_allocno_maps ();
  if (ira_max_point != ira_max_point_before_emit)
    ira_compress_allocno_live_ranges ();
  ira_free (regno_top_level_allocno_map);
}



#ifdef ENABLE_IRA_CHECKING
/* Check creation of all allocnos.  Allocnos on lower levels should
   have allocnos or caps on all upper levels.  */
static void
check_allocno_creation (void)
{
  ira_allocno_t a;
  ira_allocno_iterator ai;
  ira_loop_tree_node_t loop_tree_node;

  FOR_EACH_ALLOCNO (a, ai)
    {
      loop_tree_node = ALLOCNO_LOOP_TREE_NODE (a);
      ira_assert (bitmap_bit_p (loop_tree_node->all_allocnos,
				ALLOCNO_NUM (a)));
      if (loop_tree_node == ira_loop_tree_root)
	continue;
      if (ALLOCNO_CAP_MEMBER (a) != NULL)
	ira_assert (ALLOCNO_CAP (a) != NULL);
      else if (ALLOCNO_CAP (a) == NULL)
	ira_assert (loop_tree_node->parent
		    ->regno_allocno_map[ALLOCNO_REGNO (a)] != NULL
		    && bitmap_bit_p (loop_tree_node->border_allocnos,
				     ALLOCNO_NUM (a)));
    }
}
#endif

/* Identify allocnos which prefer a register class with a single hard register.
   Adjust ALLOCNO_CONFLICT_HARD_REG_COSTS so that conflicting allocnos are
   less likely to use the preferred singleton register.  */
static void
update_conflict_hard_reg_costs (void)
{
  ira_allocno_t a;
  ira_allocno_iterator ai;
  int i, index, min;

  FOR_EACH_ALLOCNO (a, ai)
    {
      reg_class_t aclass = ALLOCNO_CLASS (a);
      reg_class_t pref = reg_preferred_class (ALLOCNO_REGNO (a));
      int singleton = ira_class_singleton[pref][ALLOCNO_MODE (a)];
      if (singleton < 0)
	continue;
      index = ira_class_hard_reg_index[(int) aclass][singleton];
      if (index < 0)
	continue;
      if (ALLOCNO_CONFLICT_HARD_REG_COSTS (a) == NULL
	  || ALLOCNO_HARD_REG_COSTS (a) == NULL)
	continue;
      min = INT_MAX;
      for (i = ira_class_hard_regs_num[(int) aclass] - 1; i >= 0; i--)
	if (ALLOCNO_HARD_REG_COSTS (a)[i] > ALLOCNO_CLASS_COST (a)
	    && min > ALLOCNO_HARD_REG_COSTS (a)[i])
	  min = ALLOCNO_HARD_REG_COSTS (a)[i];
      if (min == INT_MAX)
	continue;
      ira_allocate_and_set_costs (&ALLOCNO_CONFLICT_HARD_REG_COSTS (a),
				  aclass, 0);
      ALLOCNO_CONFLICT_HARD_REG_COSTS (a)[index]
	-= min - ALLOCNO_CLASS_COST (a);
    }
}

/* Create a internal representation (IR) for IRA (allocnos, copies,
   loop tree nodes).  The function returns TRUE if we generate loop
   structure (besides nodes representing all function and the basic
   blocks) for regional allocation.  A true return means that we
   really need to flatten IR before the reload.  */
bool
ira_build (void)
{
  bool loops_p;

  df_analyze ();
  initiate_cost_vectors ();
  initiate_allocnos ();
  initiate_prefs ();
  initiate_copies ();
  create_loop_tree_nodes ();
  form_loop_tree ();
  create_allocnos ();
  ira_costs ();
  create_allocno_objects ();
  ira_create_allocno_live_ranges ();
  remove_unnecessary_regions (false);
  ira_compress_allocno_live_ranges ();
  update_bad_spill_attribute ();
  loops_p = more_one_region_p ();
  if (loops_p)
    {
      propagate_allocno_info ();
      create_caps ();
    }
  ira_tune_allocno_costs ();
#ifdef ENABLE_IRA_CHECKING
  check_allocno_creation ();
#endif
  setup_min_max_allocno_live_range_point ();
  sort_conflict_id_map ();
  setup_min_max_conflict_allocno_ids ();
  ira_build_conflicts ();
  update_conflict_hard_reg_costs ();
  if (! ira_conflicts_p)
    {
      ira_allocno_t a;
      ira_allocno_iterator ai;

      /* Remove all regions but root one.  */
      if (loops_p)
	{
	  remove_unnecessary_regions (true);
	  loops_p = false;
	}
      /* We don't save hard registers around calls for fast allocation
	 -- add caller clobbered registers as conflicting ones to
	 allocno crossing calls.  */
      FOR_EACH_ALLOCNO (a, ai)
	if (ALLOCNO_CALLS_CROSSED_NUM (a) != 0)
	  ior_hard_reg_conflicts (a, &call_used_reg_set);
    }
  if (internal_flag_ira_verbose > 2 && ira_dump_file != NULL)
    print_copies (ira_dump_file);
  if (internal_flag_ira_verbose > 2 && ira_dump_file != NULL)
    print_prefs (ira_dump_file);
  if (internal_flag_ira_verbose > 0 && ira_dump_file != NULL)
    {
      int n, nr, nr_big;
      ira_allocno_t a;
      live_range_t r;
      ira_allocno_iterator ai;

      n = 0;
      nr = 0;
      nr_big = 0;
      FOR_EACH_ALLOCNO (a, ai)
	{
	  int j, nobj = ALLOCNO_NUM_OBJECTS (a);

	  if (nobj > 1)
	    nr_big++;
	  for (j = 0; j < nobj; j++)
	    {
	      ira_object_t obj = ALLOCNO_OBJECT (a, j);
	      n += OBJECT_NUM_CONFLICTS (obj);
	      for (r = OBJECT_LIVE_RANGES (obj); r != NULL; r = r->next)
		nr++;
	    }
	}
      fprintf (ira_dump_file, "  regions=%d, blocks=%d, points=%d\n",
	       current_loops == NULL ? 1 : number_of_loops (cfun),
	       n_basic_blocks_for_fn (cfun), ira_max_point);
      fprintf (ira_dump_file,
	       "    allocnos=%d (big %d), copies=%d, conflicts=%d, ranges=%d\n",
	       ira_allocnos_num, nr_big, ira_copies_num, n, nr);
    }
  return loops_p;
}

/* Release the data created by function ira_build.  */
void
ira_destroy (void)
{
  finish_loop_tree_nodes ();
  finish_prefs ();
  finish_copies ();
  finish_allocnos ();
  finish_cost_vectors ();
  ira_finish_allocno_live_ranges ();
}
