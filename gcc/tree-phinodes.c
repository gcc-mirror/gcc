/* Generic routines for manipulating PHIs
   Copyright (C) 2003 Free Software Foundation, Inc.
                                                                                
This file is part of GCC.
                                                                                
GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.
                                                                                
GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
                                                                                
You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */
                                                                                
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "varray.h"
#include "ggc.h"
#include "basic-block.h"
#include "tree-flow.h"
#include "toplev.h"

/* Rewriting a function into SSA form can create a huge number of PHIs
   many of which may be thrown away shortly after their creation if jumps
   were threaded through PHI nodes.  

   While our garbage collection mechanisms will handle this situation, it
   is extremely wasteful to create nodes and throw them away, especially
   when the nodes can be reused.

   For PR 8361, we can significantly reduce the number of nodes allocated
   and thus the total amount of memory allocated by managing PHIs a
   little.  This additionally helps reduce the amount of work done by the
   garbage collector.  Similar results have been seen on a wider variety
   of tests (such as the compiler itself).

   Right now we maintain our free list on a per-function basis.  It may
   or may not make sense to maintain the free list for the duration of
   a compilation unit. 

   We could also use a zone allocator for these objects since they have
   a very well defined lifetime.  If someone wants to experiment with that
   this is the place to try it.
   
   PHI nodes have different sizes, so we can't have a single list of all
   the PHI nodes as it would be too expensive to walk down that list to
   find a PHI of a suitable size.

   Instead we have an array of lists of free PHI nodes.  The array is
   indexed by the number of PHI alternatives that PHI node can hold.
   Except for the last array member, which holds all remaining PHI
   nodes.

   So to find a free PHI node, we compute its index into the free PHI
   node array and see if there are any elements with an exact match.
   If so, then we are done.  Otherwise, we test the next larger size
   up and continue until we are in the last array element.

   We do not actually walk members of the last array element.  While it
   might allow us to pick up a few reusable PHI nodes, it could potentially
   be very expensive if the program has released a bunch of large PHI nodes,
   but keeps asking for even larger PHI nodes.  Experiments have shown that
   walking the elements of the last array entry would result in finding less
   than .1% additional reusable PHI nodes. 

   Note that we can never have less than two PHI argument slots.  Thus,
   the -2 on all the calculations below.  */

#define NUM_BUCKETS 10
static GTY ((deletable (""))) tree free_phinodes[NUM_BUCKETS - 2];
static unsigned long free_phinode_count;

static int ideal_phi_node_len (int);
static void resize_phi_node (tree *, int);

#ifdef GATHER_STATISTICS
unsigned int phi_nodes_reused;
unsigned int phi_nodes_created;
#endif

/* Initialize management of PHIs.  */

void
init_phinodes (void)
{
  int i;

  for (i = 0; i < NUM_BUCKETS - 2; i++)
    free_phinodes[i] = NULL;
  free_phinode_count = 0;
}

/* Finalize management of PHIs.  */

void
fini_phinodes (void)
{
  int i;

  for (i = 0; i < NUM_BUCKETS - 2; i++)
    free_phinodes[i] = NULL;
  free_phinode_count = 0;
}

/* Dump some simple statistics regarding the re-use of PHI nodes.  */

#ifdef GATHER_STATISTICS
void
phinodes_print_statistics (void)
{
  fprintf (stderr, "PHI nodes allocated: %u\n", phi_nodes_created);
  fprintf (stderr, "PHI nodes reused: %u\n", phi_nodes_reused);
}
#endif

/* Given LEN, the original number of requested PHI arguments, return
   a new, "ideal" length for the PHI node.  The "ideal" length rounds
   the total size of the PHI node up to the next power of two bytes.

   Rounding up will not result in wasting any memory since the size request
   will be rounded up by the GC system anyway.  [ Note this is not entirely
   true since the original length might have fit on one of the special
   GC pages. ]  By rounding up, we may avoid the need to reallocate the
   PHI node later if we increase the number of arguments for the PHI.  */

static int
ideal_phi_node_len (int len)
{
  size_t size, new_size;
  int log2, new_len;

  /* We do not support allocations of less than two PHI argument slots.  */
  if (len < 2)
    len = 2;

  /* Compute the number of bytes of the original request.  */
  size = sizeof (struct tree_phi_node) + (len - 1) * sizeof (struct phi_arg_d);

  /* Round it up to the next power of two.  */
  log2 = ceil_log2 (size);
  new_size = 1 << log2;
  
  /* Now compute and return the number of PHI argument slots given an 
     ideal size allocation.  */
  new_len = len + (new_size - size) / sizeof (struct phi_arg_d);
  return new_len;
}

/* Return a PHI node for variable VAR defined in statement STMT.
   STMT may be an empty statement for artificial references (e.g., default
   definitions created when a variable is used without a preceding
   definition).  */

tree
make_phi_node (tree var, int len)
{
  tree phi;
  int size;
  int bucket = NUM_BUCKETS - 2;

  len = ideal_phi_node_len (len);

  size = sizeof (struct tree_phi_node) + (len - 1) * sizeof (struct phi_arg_d);

  if (free_phinode_count)
    for (bucket = len - 2; bucket < NUM_BUCKETS - 2; bucket++)
      if (free_phinodes[bucket])
	break;

  /* If our free list has an element, then use it.  */
  if (bucket < NUM_BUCKETS - 2
      && PHI_ARG_CAPACITY (free_phinodes[bucket]) >= len)
    {
      free_phinode_count--;
      phi = free_phinodes[bucket];
      free_phinodes[bucket] = PHI_CHAIN (free_phinodes[bucket]);
#ifdef GATHER_STATISTICS
      phi_nodes_reused++;
#endif
    }
  else
    {
      phi = ggc_alloc (size);
#ifdef GATHER_STATISTICS
      phi_nodes_created++;
      tree_node_counts[(int) phi_kind]++;
      tree_node_sizes[(int) phi_kind] += size;
#endif

    }

  memset (phi, 0, size);
  TREE_SET_CODE (phi, PHI_NODE);
  PHI_ARG_CAPACITY (phi) = len;
  TREE_TYPE (phi) = TREE_TYPE (var);
  if (TREE_CODE (var) == SSA_NAME)
    SET_PHI_RESULT (phi, var);
  else
    SET_PHI_RESULT (phi, make_ssa_name (var, phi));

  return phi;
}

/* We no longer need PHI, release it so that it may be reused.  */

void
release_phi_node (tree phi)
{
  int bucket;
  int len = PHI_ARG_CAPACITY (phi);

  bucket = len > NUM_BUCKETS - 1 ? NUM_BUCKETS - 1 : len;
  bucket -= 2;
  PHI_CHAIN (phi) = free_phinodes[bucket];
  free_phinodes[bucket] = phi;
  free_phinode_count++;
}

/* Resize an existing PHI node.  The only way is up.  Return the
   possibly relocated phi.  */
                                                                                
static void
resize_phi_node (tree *phi, int len)
{
  int size, old_size;
  tree new_phi;
  int i, old_len, bucket = NUM_BUCKETS - 2;

  gcc_assert (len >= PHI_ARG_CAPACITY (*phi));

  /* Note that OLD_SIZE is guaranteed to be smaller than SIZE.  */
  old_size = (sizeof (struct tree_phi_node)
	     + (PHI_ARG_CAPACITY (*phi) - 1) * sizeof (struct phi_arg_d));
  size = sizeof (struct tree_phi_node) + (len - 1) * sizeof (struct phi_arg_d);

  if (free_phinode_count)
    for (bucket = len - 2; bucket < NUM_BUCKETS - 2; bucket++)
      if (free_phinodes[bucket])
	break;

  /* If our free list has an element, then use it.  */
  if (bucket < NUM_BUCKETS - 2
      && PHI_ARG_CAPACITY (free_phinodes[bucket]) >= len)
    {
      free_phinode_count--;
      new_phi = free_phinodes[bucket];
      free_phinodes[bucket] = PHI_CHAIN (free_phinodes[bucket]);
#ifdef GATHER_STATISTICS
      phi_nodes_reused++;
#endif
    }
  else
    {
      new_phi = ggc_alloc (size);
#ifdef GATHER_STATISTICS
      phi_nodes_created++;
      tree_node_counts[(int) phi_kind]++;
      tree_node_sizes[(int) phi_kind] += size;
#endif
    }

  memcpy (new_phi, *phi, old_size);

  old_len = PHI_ARG_CAPACITY (new_phi);
  PHI_ARG_CAPACITY (new_phi) = len;
                                                                                
  for (i = old_len; i < len; i++)
    {
      SET_PHI_ARG_DEF (new_phi, i, NULL_TREE);
      PHI_ARG_EDGE (new_phi, i) = NULL;
      PHI_ARG_NONZERO (new_phi, i) = false;
    }

  *phi = new_phi;
}

/* Create a new PHI node for variable VAR at basic block BB.  */

tree
create_phi_node (tree var, basic_block bb)
{
  tree phi;

  phi = make_phi_node (var, EDGE_COUNT (bb->preds));

  /* This is a new phi node, so note that is has not yet been
     rewritten.  */
  PHI_REWRITTEN (phi) = 0;

  /* Add the new PHI node to the list of PHI nodes for block BB.  */
  PHI_CHAIN (phi) = phi_nodes (bb);
  bb_ann (bb)->phi_nodes = phi;

  /* Associate BB to the PHI node.  */
  set_bb_for_stmt (phi, bb);

  return phi;
}

/* Add a new argument to PHI node PHI.  DEF is the incoming reaching
   definition and E is the edge through which DEF reaches PHI.  The new
   argument is added at the end of the argument list.
   If PHI has reached its maximum capacity, add a few slots.  In this case,
   PHI points to the reallocated phi node when we return.  */

void
add_phi_arg (tree *phi, tree def, edge e)
{
  int i = PHI_NUM_ARGS (*phi);

  if (i >= PHI_ARG_CAPACITY (*phi))
    {
      tree old_phi = *phi;

      /* Resize the phi.  Unfortunately, this may also relocate it.  */
      resize_phi_node (phi, ideal_phi_node_len (i + 4));

      /* The result of the phi is defined by this phi node.  */
      SSA_NAME_DEF_STMT (PHI_RESULT (*phi)) = *phi;

      /* If the PHI was relocated, update the PHI chains appropriately and
	 release the old PHI node.  */
      if (*phi != old_phi)
	{
	  /* Extract the basic block for the PHI from the PHI's annotation
	     rather than the edge.  This works better as the edge's
	     destination may not currently be the block with the PHI
	     node if we are in the process of threading the edge to
	     a new destination.  */
	  basic_block bb = bb_for_stmt (*phi);

	  release_phi_node (old_phi);

	  /* Update the list head if replacing the first listed phi.  */
	  if (phi_nodes (bb) == old_phi)
	    bb_ann (bb)->phi_nodes = *phi;
	  else
	    {
	      /* Traverse the list looking for the phi node to chain to.  */
	      tree p;

	      for (p = phi_nodes (bb);
		   p && PHI_CHAIN (p) != old_phi;
		   p = PHI_CHAIN (p))
		;

	      gcc_assert (p);
	      PHI_CHAIN (p) = *phi;
	    }
	}
    }

  /* Copy propagation needs to know what object occur in abnormal
     PHI nodes.  This is a convenient place to record such information.  */
  if (e->flags & EDGE_ABNORMAL)
    {
      SSA_NAME_OCCURS_IN_ABNORMAL_PHI (def) = 1;
      SSA_NAME_OCCURS_IN_ABNORMAL_PHI (PHI_RESULT (*phi)) = 1;
    }

  SET_PHI_ARG_DEF (*phi, i, def);
  PHI_ARG_EDGE (*phi, i) = e;
  PHI_ARG_NONZERO (*phi, i) = false;
  PHI_NUM_ARGS (*phi)++;
}

/* Remove a PHI argument from PHI.  BLOCK is the predecessor block where
   the PHI argument is coming from.  */

void
remove_phi_arg (tree phi, basic_block block)
{
  int i, num_elem = PHI_NUM_ARGS (phi);

  for (i = 0; i < num_elem; i++)
    {
      basic_block src_bb;

      src_bb = PHI_ARG_EDGE (phi, i)->src;

      if (src_bb == block)
	{
	  remove_phi_arg_num (phi, i);
	  return;
	}
    }
}


/* Remove the Ith argument from PHI's argument list.  This routine assumes
   ordering of alternatives in the vector is not important and implements
   removal by swapping the last alternative with the alternative we want to
   delete, then shrinking the vector.  */

void
remove_phi_arg_num (tree phi, int i)
{
  int num_elem = PHI_NUM_ARGS (phi);

  /* If we are not at the last element, switch the last element
     with the element we want to delete.  */
  if (i != num_elem - 1)
    {
      SET_PHI_ARG_DEF (phi, i, PHI_ARG_DEF (phi, num_elem - 1));
      PHI_ARG_EDGE (phi, i) = PHI_ARG_EDGE (phi, num_elem - 1);
      PHI_ARG_NONZERO (phi, i) = PHI_ARG_NONZERO (phi, num_elem - 1);
    }

  /* Shrink the vector and return.  */
  SET_PHI_ARG_DEF (phi, num_elem - 1, NULL_TREE);
  PHI_ARG_EDGE (phi, num_elem - 1) = NULL;
  PHI_ARG_NONZERO (phi, num_elem - 1) = false;
  PHI_NUM_ARGS (phi)--;

  /* If we removed the last PHI argument, then go ahead and
     remove the PHI node.  */
  if (PHI_NUM_ARGS (phi) == 0)
    remove_phi_node (phi, NULL, bb_for_stmt (phi));
}

/* Remove PHI node PHI from basic block BB.  If PREV is non-NULL, it is
   used as the node immediately before PHI in the linked list.  */

void
remove_phi_node (tree phi, tree prev, basic_block bb)
{
  if (prev)
    {
      /* Rewire the list if we are given a PREV pointer.  */
      PHI_CHAIN (prev) = PHI_CHAIN (phi);

      /* If we are deleting the PHI node, then we should release the
	 SSA_NAME node so that it can be reused.  */
      release_ssa_name (PHI_RESULT (phi));
      release_phi_node (phi);
    }
  else if (phi == phi_nodes (bb))
    {
      /* Update the list head if removing the first element.  */
      bb_ann (bb)->phi_nodes = PHI_CHAIN (phi);

      /* If we are deleting the PHI node, then we should release the
	 SSA_NAME node so that it can be reused.  */
      release_ssa_name (PHI_RESULT (phi));
      release_phi_node (phi);
    }
  else
    {
      /* Traverse the list looking for the node to remove.  */
      tree prev, t;
      prev = NULL_TREE;
      for (t = phi_nodes (bb); t && t != phi; t = PHI_CHAIN (t))
	prev = t;
      if (t)
	remove_phi_node (t, prev, bb);
    }
}


/* Remove all the PHI nodes for variables in the VARS bitmap.  */

void
remove_all_phi_nodes_for (bitmap vars)
{
  basic_block bb;

  FOR_EACH_BB (bb)
    {
      /* Build a new PHI list for BB without variables in VARS.  */
      tree phi, new_phi_list, last_phi, next;

      last_phi = new_phi_list = NULL_TREE;
      for (phi = phi_nodes (bb), next = NULL; phi; phi = next)
	{
	  tree var = SSA_NAME_VAR (PHI_RESULT (phi));

	  next = PHI_CHAIN (phi);
	  /* Only add PHI nodes for variables not in VARS.  */
	  if (!bitmap_bit_p (vars, var_ann (var)->uid))
	    {
	      /* If we're not removing this PHI node, then it must have
		 been rewritten by a previous call into the SSA rewriter.
		 Note that fact in PHI_REWRITTEN.  */
	      PHI_REWRITTEN (phi) = 1;

	      if (new_phi_list == NULL_TREE)
		new_phi_list = last_phi = phi;
	      else
		{
		  PHI_CHAIN (last_phi) = phi;
		  last_phi = phi;
		}
	    }
	  else
	    {
	      /* If we are deleting the PHI node, then we should release the
		 SSA_NAME node so that it can be reused.  */
	      release_ssa_name (PHI_RESULT (phi));
	      release_phi_node (phi);
	    }
	}

      /* Make sure the last node in the new list has no successors.  */
      if (last_phi)
	PHI_CHAIN (last_phi) = NULL_TREE;
      bb_ann (bb)->phi_nodes = new_phi_list;

#if defined ENABLE_CHECKING
      for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
	{
	  tree var = SSA_NAME_VAR (PHI_RESULT (phi));
	  gcc_assert (!bitmap_bit_p (vars, var_ann (var)->uid));
	}
#endif
    }
}


#include "gt-tree-phinodes.h"

