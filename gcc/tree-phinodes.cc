/* Generic routines for manipulating PHIs
   Copyright (C) 2003-2025 Free Software Foundation, Inc.

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
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "fold-const.h"
#include "gimple-iterator.h"
#include "tree-ssa.h"

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
static GTY ((deletable (""))) vec<gimple *, va_gc> *free_phinodes[NUM_BUCKETS - 2];
static unsigned long free_phinode_count;

static int ideal_phi_node_len (int);

unsigned int phi_nodes_reused;
unsigned int phi_nodes_created;

/* Dump some simple statistics regarding the re-use of PHI nodes.  */

void
phinodes_print_statistics (void)
{
  fprintf (stderr, "%-32s" PRsa (11) "\n", "PHI nodes allocated:",
	   SIZE_AMOUNT (phi_nodes_created));
  fprintf (stderr, "%-32s" PRsa (11) "\n", "PHI nodes reused:",
	   SIZE_AMOUNT (phi_nodes_reused));
}

/* Allocate a PHI node with at least LEN arguments.  If the free list
   happens to contain a PHI node with LEN arguments or more, return
   that one.  */

static inline gphi *
allocate_phi_node (size_t len)
{
  gphi *phi;
  size_t bucket = NUM_BUCKETS - 2;
  size_t size = sizeof (struct gphi)
	        + (len - 1) * sizeof (struct phi_arg_d);

  if (free_phinode_count)
    for (bucket = len - 2; bucket < NUM_BUCKETS - 2; bucket++)
      if (free_phinodes[bucket])
	break;

  /* If our free list has an element, then use it.  */
  if (bucket < NUM_BUCKETS - 2
      && gimple_phi_capacity ((*free_phinodes[bucket])[0]) >= len)
    {
      free_phinode_count--;
      phi = as_a <gphi *> (free_phinodes[bucket]->pop ());
      if (free_phinodes[bucket]->is_empty ())
	vec_free (free_phinodes[bucket]);
      if (GATHER_STATISTICS)
	phi_nodes_reused++;
    }
  else
    {
      phi = static_cast <gphi *> (ggc_internal_alloc (size));
      if (GATHER_STATISTICS)
	{
	  enum gimple_alloc_kind kind = gimple_alloc_kind (GIMPLE_PHI);
	  phi_nodes_created++;
	  gimple_alloc_counts[(int) kind]++;
	  gimple_alloc_sizes[(int) kind] += size;
	}
    }

  return phi;
}

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
  size = sizeof (struct gphi)
	 + (len - 1) * sizeof (struct phi_arg_d);

  /* Round it up to the next power of two.  */
  log2 = ceil_log2 (size);
  new_size = 1 << log2;

  /* Now compute and return the number of PHI argument slots given an
     ideal size allocation.  */
  new_len = len + (new_size - size) / sizeof (struct phi_arg_d);
  return new_len;
}

/* Return a PHI node with LEN argument slots for variable VAR.  */

static gphi *
make_phi_node (tree var, int len)
{
  gphi *phi;
  int capacity, i;

  capacity = ideal_phi_node_len (len);

  phi = allocate_phi_node (capacity);

  /* We need to clear the entire PHI node, including the argument
     portion, because we represent a "missing PHI argument" by placing
     NULL_TREE in PHI_ARG_DEF.  */
  memset (phi, 0, (sizeof (struct gphi)
		   - sizeof (struct phi_arg_d)
		   + sizeof (struct phi_arg_d) * len));
  phi->code = GIMPLE_PHI;
  gimple_init_singleton (phi);
  phi->nargs = len;
  phi->capacity = capacity;
  if (!var)
    ;
  else if (TREE_CODE (var) == SSA_NAME)
    gimple_phi_set_result (phi, var);
  else
    gimple_phi_set_result (phi, make_ssa_name (var, phi));

  for (i = 0; i < len; i++)
    {
      use_operand_p  imm;

      gimple_phi_arg_set_location (phi, i, UNKNOWN_LOCATION);
      imm = gimple_phi_arg_imm_use_ptr (phi, i);
      imm->use = gimple_phi_arg_def_ptr (phi, i);
      imm->prev = NULL;
      imm->next = NULL;
      imm->loc.stmt = phi;
    }

  return phi;
}

/* We no longer need PHI, release it so that it may be reused.  */

static void
release_phi_node (gimple *phi)
{
  size_t bucket;
  size_t len = gimple_phi_capacity (phi);
  size_t x;

  for (x = 0; x < gimple_phi_num_args (phi); x++)
    {
      use_operand_p  imm;
      imm = gimple_phi_arg_imm_use_ptr (phi, x);
      delink_imm_use (imm);
    }

  /* Immediately return the memory to the allocator when we would
     only ever re-use it for a smaller size allocation.  */
  if (len - 2 >= NUM_BUCKETS - 2)
    {
      ggc_free (phi);
      return;
    }

  bucket = len > NUM_BUCKETS - 1 ? NUM_BUCKETS - 1 : len;
  bucket -= 2;
  vec_safe_push (free_phinodes[bucket], phi);
  free_phinode_count++;
}


/* Resize an existing PHI node.  The only way is up.  Return the
   possibly relocated phi.  */

static gphi *
resize_phi_node (gphi *phi, size_t len)
{
  size_t old_size, i;
  gphi *new_phi;

  gcc_assert (len > gimple_phi_capacity (phi));

  /* The garbage collector will not look at the PHI node beyond the
     first PHI_NUM_ARGS elements.  Therefore, all we have to copy is a
     portion of the PHI node currently in use.  */
  old_size = sizeof (struct gphi)
	     + (gimple_phi_num_args (phi) - 1) * sizeof (struct phi_arg_d);

  new_phi = allocate_phi_node (len);

  memcpy (new_phi, phi, old_size);
  memset ((char *)new_phi + old_size, 0,
	  (sizeof (struct gphi)
	   - sizeof (struct phi_arg_d)
	   + sizeof (struct phi_arg_d) * len) - old_size);

  for (i = 0; i < gimple_phi_num_args (new_phi); i++)
    {
      use_operand_p imm, old_imm;
      imm = gimple_phi_arg_imm_use_ptr (new_phi, i);
      old_imm = gimple_phi_arg_imm_use_ptr (phi, i);
      imm->use = gimple_phi_arg_def_ptr (new_phi, i);
      relink_imm_use_stmt (imm, old_imm, new_phi);
    }

  new_phi->capacity = len;

  return new_phi;
}

/* Reserve PHI arguments for a new edge to basic block BB.  */

void
reserve_phi_args_for_new_edge (basic_block bb)
{
  size_t len = EDGE_COUNT (bb->preds);
  size_t cap = ideal_phi_node_len (len + 4);
  gphi_iterator gsi;

  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *stmt = gsi.phi ();

      if (len > gimple_phi_capacity (stmt))
	{
	  gphi *new_phi = resize_phi_node (stmt, cap);

	  /* The result of the PHI is defined by this PHI node.  */
	  SSA_NAME_DEF_STMT (gimple_phi_result (new_phi)) = new_phi;
	  gsi_set_stmt (&gsi, new_phi);

	  release_phi_node (stmt);
	  stmt = new_phi;
	}

      stmt->nargs++;

      /* We represent a "missing PHI argument" by placing NULL_TREE in
	 the corresponding slot.  If PHI arguments were added
	 immediately after an edge is created, this zeroing would not
	 be necessary, but unfortunately this is not the case.  For
	 example, the loop optimizer duplicates several basic blocks,
	 redirects edges, and then fixes up PHI arguments later in
	 batch.  */
      use_operand_p imm = gimple_phi_arg_imm_use_ptr (stmt, len - 1);
      imm->use = gimple_phi_arg_def_ptr (stmt, len - 1);
      imm->prev = NULL;
      imm->next = NULL;
      imm->loc.stmt = stmt;
      SET_PHI_ARG_DEF (stmt, len - 1, NULL_TREE);
      gimple_phi_arg_set_location (stmt, len - 1, UNKNOWN_LOCATION);
    }
}

/* Adds PHI to BB.  */

static void
add_phi_node_to_bb (gphi *phi, basic_block bb)
{
  gimple_seq seq = phi_nodes (bb);
  /* Add the new PHI node to the list of PHI nodes for block BB.  */
  if (seq == NULL)
    set_phi_nodes (bb, gimple_seq_alloc_with_stmt (phi));
  else
    {
      gimple_seq_add_stmt (&seq, phi);
      gcc_assert (seq == phi_nodes (bb));
    }

  /* Associate BB to the PHI node.  */
  gimple_set_bb (phi, bb);
}

/* Create a new PHI node for variable VAR at basic block BB.  */

gphi *
create_phi_node (tree var, basic_block bb)
{
  gphi *phi = make_phi_node (var, EDGE_COUNT (bb->preds));

  add_phi_node_to_bb (phi, bb);
  return phi;
}


/* Add a new argument to PHI node PHI.  DEF is the incoming reaching
   definition and E is the edge through which DEF reaches PHI.  The new
   argument is added at the end of the argument list.
   If PHI has reached its maximum capacity, add a few slots.  In this case,
   PHI points to the reallocated phi node when we return.  */

void
add_phi_arg (gphi *phi, tree def, edge e, location_t locus)
{
  basic_block bb = e->dest;

  gcc_assert (bb == gimple_bb (phi));

  /* We resize PHI nodes upon edge creation.  We should always have
     enough room at this point.  */
  gcc_assert (gimple_phi_num_args (phi) <= gimple_phi_capacity (phi));

  /* We resize PHI nodes upon edge creation.  We should always have
     enough room at this point.  */
  gcc_assert (e->dest_idx < gimple_phi_num_args (phi));

  /* Copy propagation needs to know what object occur in abnormal
     PHI nodes.  This is a convenient place to record such information.  */
  if (e->flags & EDGE_ABNORMAL)
    {
      SSA_NAME_OCCURS_IN_ABNORMAL_PHI (def) = 1;
      SSA_NAME_OCCURS_IN_ABNORMAL_PHI (PHI_RESULT (phi)) = 1;
    }

  SET_PHI_ARG_DEF (phi, e->dest_idx, def);
  gimple_phi_arg_set_location (phi, e->dest_idx, locus);
}


/* Remove the Ith argument from PHI's argument list.  This routine
   implements removal by swapping the last alternative with the
   alternative we want to delete and then shrinking the vector, which
   is consistent with how we remove an edge from the edge vector.  */

static void
remove_phi_arg_num (gphi *phi, int i)
{
  int num_elem = gimple_phi_num_args (phi);

  gcc_assert (i < num_elem);

  /* Delink the item which is being removed.  */
  delink_imm_use (gimple_phi_arg_imm_use_ptr (phi, i));

  /* If it is not the last element, move the last element
     to the element we want to delete, resetting all the links. */
  if (i != num_elem - 1)
    {
      use_operand_p old_p, new_p;
      old_p = gimple_phi_arg_imm_use_ptr (phi, num_elem - 1);
      new_p = gimple_phi_arg_imm_use_ptr (phi, i);
      /* Set use on new node, and link into last element's place.  */
      *(new_p->use) = *(old_p->use);
      relink_imm_use (new_p, old_p);
      /* Move the location as well.  */
      gimple_phi_arg_set_location (phi, i,
				   gimple_phi_arg_location (phi, num_elem - 1));
    }

  /* Shrink the vector and return.  Note that we do not have to clear
     PHI_ARG_DEF because the garbage collector will not look at those
     elements beyond the first PHI_NUM_ARGS elements of the array.  */
  phi->nargs--;
}


/* Remove all PHI arguments associated with edge E.  */

void
remove_phi_args (edge e)
{
  gphi_iterator gsi;

  for (gsi = gsi_start_phis (e->dest); !gsi_end_p (gsi); gsi_next (&gsi))
    remove_phi_arg_num (gsi.phi (),
			e->dest_idx);
}


/* Remove the PHI node pointed-to by iterator GSI from basic block BB.  After
   removal, iterator GSI is updated to point to the next PHI node in the
   sequence. If RELEASE_LHS_P is true, the LHS of this PHI node is released
   into the free pool of SSA names.  */

void
remove_phi_node (gimple_stmt_iterator *gsi, bool release_lhs_p)
{
  gimple *phi = gsi_stmt (*gsi);

  if (release_lhs_p)
    insert_debug_temps_for_defs (gsi);

  gsi_remove (gsi, false);

  /* If we are deleting the PHI node, then we should release the
     SSA_NAME node so that it can be reused.  */
  if (release_lhs_p)
    release_ssa_name (gimple_phi_result (phi));
  release_phi_node (phi);
}

/* Remove all the phi nodes from BB.  */

void
remove_phi_nodes (basic_block bb)
{
  gphi_iterator gsi;

  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); )
    remove_phi_node (&gsi, true);

  set_phi_nodes (bb, NULL);
}

/* Given PHI, return its RHS if the PHI is a degenerate, otherwise return
   NULL.  */

tree
degenerate_phi_result (gphi *phi)
{
  tree lhs = gimple_phi_result (phi);
  tree val = NULL;
  size_t i;

  /* Ignoring arguments which are the same as LHS, if all the remaining
     arguments are the same, then the PHI is a degenerate and has the
     value of that common argument.  */
  for (i = 0; i < gimple_phi_num_args (phi); i++)
    {
      tree arg = gimple_phi_arg_def (phi, i);

      if (arg == lhs)
	continue;
      else if (!arg)
	break;
      else if (!val)
	val = arg;
      else if (arg == val)
	continue;
      /* We bring in some of operand_equal_p not only to speed things
	 up, but also to avoid crashing when dereferencing the type of
	 a released SSA name.  */
      else if (TREE_CODE (val) != TREE_CODE (arg)
	       || TREE_CODE (val) == SSA_NAME
	       || !operand_equal_p (arg, val, 0))
	break;
    }
  return (i == gimple_phi_num_args (phi) ? val : NULL);
}

/* Set PHI nodes of a basic block BB to SEQ.  */

void
set_phi_nodes (basic_block bb, gimple_seq seq)
{
  gimple_stmt_iterator i;

  gcc_checking_assert (!(bb->flags & BB_RTL));
  bb->il.gimple.phi_nodes = seq;
  if (seq)
    for (i = gsi_start (seq); !gsi_end_p (i); gsi_next (&i))
      gimple_set_bb (gsi_stmt (i), bb);
}

#include "gt-tree-phinodes.h"
