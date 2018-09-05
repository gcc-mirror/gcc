/* Control flow graph analysis code for GNU compiler.
   Copyright (C) 1987-2018 Free Software Foundation, Inc.

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

/* This file contains various simple utilities to analyze the CFG.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "cfghooks.h"
#include "timevar.h"
#include "cfganal.h"
#include "cfgloop.h"

namespace {
/* Store the data structures necessary for depth-first search.  */
class depth_first_search
  {
public:
    depth_first_search ();

    basic_block execute (basic_block);
    void add_bb (basic_block);

private:
  /* stack for backtracking during the algorithm */
  auto_vec<basic_block, 20> m_stack;

  /* record of basic blocks already seen by depth-first search */
  auto_sbitmap m_visited_blocks;
};
}

/* Mark the back edges in DFS traversal.
   Return nonzero if a loop (natural or otherwise) is present.
   Inspired by Depth_First_Search_PP described in:

     Advanced Compiler Design and Implementation
     Steven Muchnick
     Morgan Kaufmann, 1997

   and heavily borrowed from pre_and_rev_post_order_compute.  */

bool
mark_dfs_back_edges (void)
{
  int *pre;
  int *post;
  int prenum = 1;
  int postnum = 1;
  bool found = false;

  /* Allocate the preorder and postorder number arrays.  */
  pre = XCNEWVEC (int, last_basic_block_for_fn (cfun));
  post = XCNEWVEC (int, last_basic_block_for_fn (cfun));

  /* Allocate stack for back-tracking up CFG.  */
  auto_vec<edge_iterator, 20> stack (n_basic_blocks_for_fn (cfun) + 1);

  /* Allocate bitmap to track nodes that have been visited.  */
  auto_sbitmap visited (last_basic_block_for_fn (cfun));

  /* None of the nodes in the CFG have been visited yet.  */
  bitmap_clear (visited);

  /* Push the first edge on to the stack.  */
  stack.quick_push (ei_start (ENTRY_BLOCK_PTR_FOR_FN (cfun)->succs));

  while (!stack.is_empty ())
    {
      basic_block src;
      basic_block dest;

      /* Look at the edge on the top of the stack.  */
      edge_iterator ei = stack.last ();
      src = ei_edge (ei)->src;
      dest = ei_edge (ei)->dest;
      ei_edge (ei)->flags &= ~EDGE_DFS_BACK;

      /* Check if the edge destination has been visited yet.  */
      if (dest != EXIT_BLOCK_PTR_FOR_FN (cfun) && ! bitmap_bit_p (visited,
								  dest->index))
	{
	  /* Mark that we have visited the destination.  */
	  bitmap_set_bit (visited, dest->index);

	  pre[dest->index] = prenum++;
	  if (EDGE_COUNT (dest->succs) > 0)
	    {
	      /* Since the DEST node has been visited for the first
		 time, check its successors.  */
	      stack.quick_push (ei_start (dest->succs));
	    }
	  else
	    post[dest->index] = postnum++;
	}
      else
	{
	  if (dest != EXIT_BLOCK_PTR_FOR_FN (cfun)
	      && src != ENTRY_BLOCK_PTR_FOR_FN (cfun)
	      && pre[src->index] >= pre[dest->index]
	      && post[dest->index] == 0)
	    ei_edge (ei)->flags |= EDGE_DFS_BACK, found = true;

	  if (ei_one_before_end_p (ei)
	      && src != ENTRY_BLOCK_PTR_FOR_FN (cfun))
	    post[src->index] = postnum++;

	  if (!ei_one_before_end_p (ei))
	    ei_next (&stack.last ());
	  else
	    stack.pop ();
	}
    }

  free (pre);
  free (post);

  return found;
}

/* Find unreachable blocks.  An unreachable block will have 0 in
   the reachable bit in block->flags.  A nonzero value indicates the
   block is reachable.  */

void
find_unreachable_blocks (void)
{
  edge e;
  edge_iterator ei;
  basic_block *tos, *worklist, bb;

  tos = worklist = XNEWVEC (basic_block, n_basic_blocks_for_fn (cfun));

  /* Clear all the reachability flags.  */

  FOR_EACH_BB_FN (bb, cfun)
    bb->flags &= ~BB_REACHABLE;

  /* Add our starting points to the worklist.  Almost always there will
     be only one.  It isn't inconceivable that we might one day directly
     support Fortran alternate entry points.  */

  FOR_EACH_EDGE (e, ei, ENTRY_BLOCK_PTR_FOR_FN (cfun)->succs)
    {
      *tos++ = e->dest;

      /* Mark the block reachable.  */
      e->dest->flags |= BB_REACHABLE;
    }

  /* Iterate: find everything reachable from what we've already seen.  */

  while (tos != worklist)
    {
      basic_block b = *--tos;

      FOR_EACH_EDGE (e, ei, b->succs)
	{
	  basic_block dest = e->dest;

	  if (!(dest->flags & BB_REACHABLE))
	    {
	      *tos++ = dest;
	      dest->flags |= BB_REACHABLE;
	    }
	}
    }

  free (worklist);
}

/* Verify that there are no unreachable blocks in the current function.  */

void
verify_no_unreachable_blocks (void)
{
  find_unreachable_blocks ();

  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    gcc_assert ((bb->flags & BB_REACHABLE) != 0);
}


/* Functions to access an edge list with a vector representation.
   Enough data is kept such that given an index number, the
   pred and succ that edge represents can be determined, or
   given a pred and a succ, its index number can be returned.
   This allows algorithms which consume a lot of memory to
   represent the normally full matrix of edge (pred,succ) with a
   single indexed vector,  edge (EDGE_INDEX (pred, succ)), with no
   wasted space in the client code due to sparse flow graphs.  */

/* This functions initializes the edge list. Basically the entire
   flowgraph is processed, and all edges are assigned a number,
   and the data structure is filled in.  */

struct edge_list *
create_edge_list (void)
{
  struct edge_list *elist;
  edge e;
  int num_edges;
  basic_block bb;
  edge_iterator ei;

  /* Determine the number of edges in the flow graph by counting successor
     edges on each basic block.  */
  num_edges = 0;
  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (cfun),
		  EXIT_BLOCK_PTR_FOR_FN (cfun), next_bb)
    {
      num_edges += EDGE_COUNT (bb->succs);
    }

  elist = XNEW (struct edge_list);
  elist->num_edges = num_edges;
  elist->index_to_edge = XNEWVEC (edge, num_edges);

  num_edges = 0;

  /* Follow successors of blocks, and register these edges.  */
  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (cfun),
		  EXIT_BLOCK_PTR_FOR_FN (cfun), next_bb)
    FOR_EACH_EDGE (e, ei, bb->succs)
      elist->index_to_edge[num_edges++] = e;

  return elist;
}

/* This function free's memory associated with an edge list.  */

void
free_edge_list (struct edge_list *elist)
{
  if (elist)
    {
      free (elist->index_to_edge);
      free (elist);
    }
}

/* This function provides debug output showing an edge list.  */

DEBUG_FUNCTION void
print_edge_list (FILE *f, struct edge_list *elist)
{
  int x;

  fprintf (f, "Compressed edge list, %d BBs + entry & exit, and %d edges\n",
	   n_basic_blocks_for_fn (cfun), elist->num_edges);

  for (x = 0; x < elist->num_edges; x++)
    {
      fprintf (f, " %-4d - edge(", x);
      if (INDEX_EDGE_PRED_BB (elist, x) == ENTRY_BLOCK_PTR_FOR_FN (cfun))
	fprintf (f, "entry,");
      else
	fprintf (f, "%d,", INDEX_EDGE_PRED_BB (elist, x)->index);

      if (INDEX_EDGE_SUCC_BB (elist, x) == EXIT_BLOCK_PTR_FOR_FN (cfun))
	fprintf (f, "exit)\n");
      else
	fprintf (f, "%d)\n", INDEX_EDGE_SUCC_BB (elist, x)->index);
    }
}

/* This function provides an internal consistency check of an edge list,
   verifying that all edges are present, and that there are no
   extra edges.  */

DEBUG_FUNCTION void
verify_edge_list (FILE *f, struct edge_list *elist)
{
  int pred, succ, index;
  edge e;
  basic_block bb, p, s;
  edge_iterator ei;

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (cfun),
		  EXIT_BLOCK_PTR_FOR_FN (cfun), next_bb)
    {
      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  pred = e->src->index;
	  succ = e->dest->index;
	  index = EDGE_INDEX (elist, e->src, e->dest);
	  if (index == EDGE_INDEX_NO_EDGE)
	    {
	      fprintf (f, "*p* No index for edge from %d to %d\n", pred, succ);
	      continue;
	    }

	  if (INDEX_EDGE_PRED_BB (elist, index)->index != pred)
	    fprintf (f, "*p* Pred for index %d should be %d not %d\n",
		     index, pred, INDEX_EDGE_PRED_BB (elist, index)->index);
	  if (INDEX_EDGE_SUCC_BB (elist, index)->index != succ)
	    fprintf (f, "*p* Succ for index %d should be %d not %d\n",
		     index, succ, INDEX_EDGE_SUCC_BB (elist, index)->index);
	}
    }

  /* We've verified that all the edges are in the list, now lets make sure
     there are no spurious edges in the list.  This is an expensive check!  */

  FOR_BB_BETWEEN (p, ENTRY_BLOCK_PTR_FOR_FN (cfun),
		  EXIT_BLOCK_PTR_FOR_FN (cfun), next_bb)
    FOR_BB_BETWEEN (s, ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb, NULL, next_bb)
      {
	int found_edge = 0;

	FOR_EACH_EDGE (e, ei, p->succs)
	  if (e->dest == s)
	    {
	      found_edge = 1;
	      break;
	    }

	FOR_EACH_EDGE (e, ei, s->preds)
	  if (e->src == p)
	    {
	      found_edge = 1;
	      break;
	    }

	if (EDGE_INDEX (elist, p, s)
	    == EDGE_INDEX_NO_EDGE && found_edge != 0)
	  fprintf (f, "*** Edge (%d, %d) appears to not have an index\n",
		   p->index, s->index);
	if (EDGE_INDEX (elist, p, s)
	    != EDGE_INDEX_NO_EDGE && found_edge == 0)
	  fprintf (f, "*** Edge (%d, %d) has index %d, but there is no edge\n",
		   p->index, s->index, EDGE_INDEX (elist, p, s));
      }
}


/* Functions to compute control dependences.  */

/* Indicate block BB is control dependent on an edge with index EDGE_INDEX.  */
void
control_dependences::set_control_dependence_map_bit (basic_block bb,
						     int edge_index)
{
  if (bb == ENTRY_BLOCK_PTR_FOR_FN (cfun))
    return;
  gcc_assert (bb != EXIT_BLOCK_PTR_FOR_FN (cfun));
  bitmap_set_bit (control_dependence_map[bb->index], edge_index);
}

/* Clear all control dependences for block BB.  */
void
control_dependences::clear_control_dependence_bitmap (basic_block bb)
{
  bitmap_clear (control_dependence_map[bb->index]);
}

/* Find the immediate postdominator PDOM of the specified basic block BLOCK.
   This function is necessary because some blocks have negative numbers.  */

static inline basic_block
find_pdom (basic_block block)
{
  gcc_assert (block != ENTRY_BLOCK_PTR_FOR_FN (cfun));

  if (block == EXIT_BLOCK_PTR_FOR_FN (cfun))
    return EXIT_BLOCK_PTR_FOR_FN (cfun);
  else
    {
      basic_block bb = get_immediate_dominator (CDI_POST_DOMINATORS, block);
      if (! bb)
	return EXIT_BLOCK_PTR_FOR_FN (cfun);
      return bb;
    }
}

/* Determine all blocks' control dependences on the given edge with edge_list
   EL index EDGE_INDEX, ala Morgan, Section 3.6.  */

void
control_dependences::find_control_dependence (int edge_index)
{
  basic_block current_block;
  basic_block ending_block;

  gcc_assert (get_edge_src (edge_index) != EXIT_BLOCK_PTR_FOR_FN (cfun));

  /* For abnormal edges, we don't make current_block control
     dependent because instructions that throw are always necessary
     anyway.  */
  edge e = find_edge (get_edge_src (edge_index), get_edge_dest (edge_index));
  if (e->flags & EDGE_ABNORMAL)
    return;

  if (get_edge_src (edge_index) == ENTRY_BLOCK_PTR_FOR_FN (cfun))
    ending_block = single_succ (ENTRY_BLOCK_PTR_FOR_FN (cfun));
  else
    ending_block = find_pdom (get_edge_src (edge_index));

  for (current_block = get_edge_dest (edge_index);
       current_block != ending_block
       && current_block != EXIT_BLOCK_PTR_FOR_FN (cfun);
       current_block = find_pdom (current_block))
    set_control_dependence_map_bit (current_block, edge_index);
}

/* Record all blocks' control dependences on all edges in the edge
   list EL, ala Morgan, Section 3.6.  */

control_dependences::control_dependences ()
{
  timevar_push (TV_CONTROL_DEPENDENCES);

  /* Initialize the edge list.  */
  int num_edges = 0;
  basic_block bb;
  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (cfun),
		  EXIT_BLOCK_PTR_FOR_FN (cfun), next_bb)
    num_edges += EDGE_COUNT (bb->succs);
  m_el.create (num_edges);
  edge e;
  edge_iterator ei;
  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (cfun),
		  EXIT_BLOCK_PTR_FOR_FN (cfun), next_bb)
    FOR_EACH_EDGE (e, ei, bb->succs)
      m_el.quick_push (std::make_pair (e->src->index, e->dest->index));

  control_dependence_map.create (last_basic_block_for_fn (cfun));
  for (int i = 0; i < last_basic_block_for_fn (cfun); ++i)
    control_dependence_map.quick_push (BITMAP_ALLOC (NULL));
  for (int i = 0; i < num_edges; ++i)
    find_control_dependence (i);

  timevar_pop (TV_CONTROL_DEPENDENCES);
}

/* Free control dependences and the associated edge list.  */

control_dependences::~control_dependences ()
{
  for (unsigned i = 0; i < control_dependence_map.length (); ++i)
    BITMAP_FREE (control_dependence_map[i]);
  control_dependence_map.release ();
  m_el.release ();
}

/* Returns the bitmap of edges the basic-block I is dependent on.  */

bitmap
control_dependences::get_edges_dependent_on (int i)
{
  return control_dependence_map[i];
}

/* Returns the edge source with index I from the edge list.  */

basic_block
control_dependences::get_edge_src (int i)
{
  return BASIC_BLOCK_FOR_FN (cfun, m_el[i].first);
}

/* Returns the edge destination with index I from the edge list.  */

basic_block
control_dependences::get_edge_dest (int i)
{
  return BASIC_BLOCK_FOR_FN (cfun, m_el[i].second);
}


/* Given PRED and SUCC blocks, return the edge which connects the blocks.
   If no such edge exists, return NULL.  */

edge
find_edge (basic_block pred, basic_block succ)
{
  edge e;
  edge_iterator ei;

  if (EDGE_COUNT (pred->succs) <= EDGE_COUNT (succ->preds))
    {
      FOR_EACH_EDGE (e, ei, pred->succs)
	if (e->dest == succ)
	  return e;
    }
  else
    {
      FOR_EACH_EDGE (e, ei, succ->preds)
	if (e->src == pred)
	  return e;
    }

  return NULL;
}

/* This routine will determine what, if any, edge there is between
   a specified predecessor and successor.  */

int
find_edge_index (struct edge_list *edge_list, basic_block pred, basic_block succ)
{
  int x;

  for (x = 0; x < NUM_EDGES (edge_list); x++)
    if (INDEX_EDGE_PRED_BB (edge_list, x) == pred
	&& INDEX_EDGE_SUCC_BB (edge_list, x) == succ)
      return x;

  return (EDGE_INDEX_NO_EDGE);
}

/* This routine will remove any fake predecessor edges for a basic block.
   When the edge is removed, it is also removed from whatever successor
   list it is in.  */

static void
remove_fake_predecessors (basic_block bb)
{
  edge e;
  edge_iterator ei;

  for (ei = ei_start (bb->preds); (e = ei_safe_edge (ei)); )
    {
      if ((e->flags & EDGE_FAKE) == EDGE_FAKE)
	remove_edge (e);
      else
	ei_next (&ei);
    }
}

/* This routine will remove all fake edges from the flow graph.  If
   we remove all fake successors, it will automatically remove all
   fake predecessors.  */

void
remove_fake_edges (void)
{
  basic_block bb;

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb, NULL, next_bb)
    remove_fake_predecessors (bb);
}

/* This routine will remove all fake edges to the EXIT_BLOCK.  */

void
remove_fake_exit_edges (void)
{
  remove_fake_predecessors (EXIT_BLOCK_PTR_FOR_FN (cfun));
}


/* This function will add a fake edge between any block which has no
   successors, and the exit block. Some data flow equations require these
   edges to exist.  */

void
add_noreturn_fake_exit_edges (void)
{
  basic_block bb;

  FOR_EACH_BB_FN (bb, cfun)
    if (EDGE_COUNT (bb->succs) == 0)
      make_single_succ_edge (bb, EXIT_BLOCK_PTR_FOR_FN (cfun), EDGE_FAKE);
}

/* This function adds a fake edge between any infinite loops to the
   exit block.  Some optimizations require a path from each node to
   the exit node.

   See also Morgan, Figure 3.10, pp. 82-83.

   The current implementation is ugly, not attempting to minimize the
   number of inserted fake edges.  To reduce the number of fake edges
   to insert, add fake edges from _innermost_ loops containing only
   nodes not reachable from the exit block.  */

void
connect_infinite_loops_to_exit (void)
{
  /* Perform depth-first search in the reverse graph to find nodes
     reachable from the exit block.  */
  depth_first_search dfs;
  dfs.add_bb (EXIT_BLOCK_PTR_FOR_FN (cfun));

  /* Repeatedly add fake edges, updating the unreachable nodes.  */
  basic_block unvisited_block = EXIT_BLOCK_PTR_FOR_FN (cfun);
  while (1)
    {
      unvisited_block = dfs.execute (unvisited_block);
      if (!unvisited_block)
	break;

      basic_block deadend_block = dfs_find_deadend (unvisited_block);
      edge e = make_edge (deadend_block, EXIT_BLOCK_PTR_FOR_FN (cfun),
			  EDGE_FAKE);
      e->probability = profile_probability::never ();
      dfs.add_bb (deadend_block);
    }
}

/* Compute reverse top sort order.  This is computing a post order
   numbering of the graph.  If INCLUDE_ENTRY_EXIT is true, then
   ENTRY_BLOCK and EXIT_BLOCK are included.  If DELETE_UNREACHABLE is
   true, unreachable blocks are deleted.  */

int
post_order_compute (int *post_order, bool include_entry_exit,
		    bool delete_unreachable)
{
  int post_order_num = 0;
  int count;

  if (include_entry_exit)
    post_order[post_order_num++] = EXIT_BLOCK;

  /* Allocate stack for back-tracking up CFG.  */
  auto_vec<edge_iterator, 20> stack (n_basic_blocks_for_fn (cfun) + 1);

  /* Allocate bitmap to track nodes that have been visited.  */
  auto_sbitmap visited (last_basic_block_for_fn (cfun));

  /* None of the nodes in the CFG have been visited yet.  */
  bitmap_clear (visited);

  /* Push the first edge on to the stack.  */
  stack.quick_push (ei_start (ENTRY_BLOCK_PTR_FOR_FN (cfun)->succs));

  while (!stack.is_empty ())
    {
      basic_block src;
      basic_block dest;

      /* Look at the edge on the top of the stack.  */
      edge_iterator ei = stack.last ();
      src = ei_edge (ei)->src;
      dest = ei_edge (ei)->dest;

      /* Check if the edge destination has been visited yet.  */
      if (dest != EXIT_BLOCK_PTR_FOR_FN (cfun)
	  && ! bitmap_bit_p (visited, dest->index))
	{
	  /* Mark that we have visited the destination.  */
	  bitmap_set_bit (visited, dest->index);

	  if (EDGE_COUNT (dest->succs) > 0)
	    /* Since the DEST node has been visited for the first
	       time, check its successors.  */
	    stack.quick_push (ei_start (dest->succs));
	  else
	    post_order[post_order_num++] = dest->index;
	}
      else
	{
	  if (ei_one_before_end_p (ei)
	      && src != ENTRY_BLOCK_PTR_FOR_FN (cfun))
	    post_order[post_order_num++] = src->index;

	  if (!ei_one_before_end_p (ei))
	    ei_next (&stack.last ());
	  else
	    stack.pop ();
	}
    }

  if (include_entry_exit)
    {
      post_order[post_order_num++] = ENTRY_BLOCK;
      count = post_order_num;
    }
  else
    count = post_order_num + 2;

  /* Delete the unreachable blocks if some were found and we are
     supposed to do it.  */
  if (delete_unreachable && (count != n_basic_blocks_for_fn (cfun)))
    {
      basic_block b;
      basic_block next_bb;
      for (b = ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb; b
	   != EXIT_BLOCK_PTR_FOR_FN (cfun); b = next_bb)
	{
	  next_bb = b->next_bb;

	  if (!(bitmap_bit_p (visited, b->index)))
	    delete_basic_block (b);
	}

      tidy_fallthru_edges ();
    }

  return post_order_num;
}


/* Helper routine for inverted_post_order_compute
   flow_dfs_compute_reverse_execute, and the reverse-CFG
   deapth first search in dominance.c.
   BB has to belong to a region of CFG
   unreachable by inverted traversal from the exit.
   i.e. there's no control flow path from ENTRY to EXIT
   that contains this BB.
   This can happen in two cases - if there's an infinite loop
   or if there's a block that has no successor
   (call to a function with no return).
   Some RTL passes deal with this condition by
   calling connect_infinite_loops_to_exit () and/or
   add_noreturn_fake_exit_edges ().
   However, those methods involve modifying the CFG itself
   which may not be desirable.
   Hence, we deal with the infinite loop/no return cases
   by identifying a unique basic block that can reach all blocks
   in such a region by inverted traversal.
   This function returns a basic block that guarantees
   that all blocks in the region are reachable
   by starting an inverted traversal from the returned block.  */

basic_block
dfs_find_deadend (basic_block bb)
{
  auto_bitmap visited;
  basic_block next = bb;

  for (;;)
    {
      if (EDGE_COUNT (next->succs) == 0)
	return next;

      if (! bitmap_set_bit (visited, next->index))
	return bb;

      bb = next;
      /* If we are in an analyzed cycle make sure to try exiting it.
         Note this is a heuristic only and expected to work when loop
	 fixup is needed as well.  */
      if (! bb->loop_father
	  || ! loop_outer (bb->loop_father))
	next = EDGE_SUCC (bb, 0)->dest;
      else
	{
	  edge_iterator ei;
	  edge e;
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    if (loop_exit_edge_p (bb->loop_father, e))
	      break;
	  next = e ? e->dest : EDGE_SUCC (bb, 0)->dest;
	}
    }

  gcc_unreachable ();
}


/* Compute the reverse top sort order of the inverted CFG
   i.e. starting from the exit block and following the edges backward
   (from successors to predecessors).
   This ordering can be used for forward dataflow problems among others.

   Optionally if START_POINTS is specified, start from exit block and all
   basic blocks in START_POINTS.  This is used by CD-DCE.

   This function assumes that all blocks in the CFG are reachable
   from the ENTRY (but not necessarily from EXIT).

   If there's an infinite loop,
   a simple inverted traversal starting from the blocks
   with no successors can't visit all blocks.
   To solve this problem, we first do inverted traversal
   starting from the blocks with no successor.
   And if there's any block left that's not visited by the regular
   inverted traversal from EXIT,
   those blocks are in such problematic region.
   Among those, we find one block that has
   any visited predecessor (which is an entry into such a region),
   and start looking for a "dead end" from that block
   and do another inverted traversal from that block.  */

void
inverted_post_order_compute (vec<int> *post_order,
			     sbitmap *start_points)
{
  basic_block bb;
  post_order->reserve_exact (n_basic_blocks_for_fn (cfun));

  if (flag_checking)
    verify_no_unreachable_blocks ();

  /* Allocate stack for back-tracking up CFG.  */
  auto_vec<edge_iterator, 20> stack (n_basic_blocks_for_fn (cfun) + 1);

  /* Allocate bitmap to track nodes that have been visited.  */
  auto_sbitmap visited (last_basic_block_for_fn (cfun));

  /* None of the nodes in the CFG have been visited yet.  */
  bitmap_clear (visited);

  if (start_points)
    {
      FOR_ALL_BB_FN (bb, cfun)
        if (bitmap_bit_p (*start_points, bb->index)
	    && EDGE_COUNT (bb->preds) > 0)
	  {
	    stack.quick_push (ei_start (bb->preds));
            bitmap_set_bit (visited, bb->index);
	  }
      if (EDGE_COUNT (EXIT_BLOCK_PTR_FOR_FN (cfun)->preds))
	{
	  stack.quick_push (ei_start (EXIT_BLOCK_PTR_FOR_FN (cfun)->preds));
          bitmap_set_bit (visited, EXIT_BLOCK_PTR_FOR_FN (cfun)->index);
	}
    }
  else
  /* Put all blocks that have no successor into the initial work list.  */
  FOR_ALL_BB_FN (bb, cfun)
    if (EDGE_COUNT (bb->succs) == 0)
      {
        /* Push the initial edge on to the stack.  */
        if (EDGE_COUNT (bb->preds) > 0)
          {
	    stack.quick_push (ei_start (bb->preds));
            bitmap_set_bit (visited, bb->index);
          }
      }

  do
    {
      bool has_unvisited_bb = false;

      /* The inverted traversal loop. */
      while (!stack.is_empty ())
        {
          edge_iterator ei;
          basic_block pred;

          /* Look at the edge on the top of the stack.  */
	  ei = stack.last ();
          bb = ei_edge (ei)->dest;
          pred = ei_edge (ei)->src;

          /* Check if the predecessor has been visited yet.  */
          if (! bitmap_bit_p (visited, pred->index))
            {
              /* Mark that we have visited the destination.  */
              bitmap_set_bit (visited, pred->index);

              if (EDGE_COUNT (pred->preds) > 0)
                /* Since the predecessor node has been visited for the first
                   time, check its predecessors.  */
		stack.quick_push (ei_start (pred->preds));
              else
		post_order->quick_push (pred->index);
            }
          else
            {
	      if (bb != EXIT_BLOCK_PTR_FOR_FN (cfun)
		  && ei_one_before_end_p (ei))
		post_order->quick_push (bb->index);

              if (!ei_one_before_end_p (ei))
		ei_next (&stack.last ());
              else
		stack.pop ();
            }
        }

      /* Detect any infinite loop and activate the kludge.
         Note that this doesn't check EXIT_BLOCK itself
	 since EXIT_BLOCK is always added after the outer do-while loop.  */
      FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (cfun),
		      EXIT_BLOCK_PTR_FOR_FN (cfun), next_bb)
        if (!bitmap_bit_p (visited, bb->index))
          {
            has_unvisited_bb = true;

            if (EDGE_COUNT (bb->preds) > 0)
              {
                edge_iterator ei;
                edge e;
                basic_block visited_pred = NULL;

                /* Find an already visited predecessor.  */
                FOR_EACH_EDGE (e, ei, bb->preds)
                  {
                    if (bitmap_bit_p (visited, e->src->index))
                      visited_pred = e->src;
                  }

                if (visited_pred)
                  {
                    basic_block be = dfs_find_deadend (bb);
                    gcc_assert (be != NULL);
                    bitmap_set_bit (visited, be->index);
		    stack.quick_push (ei_start (be->preds));
                    break;
                  }
              }
          }

      if (has_unvisited_bb && stack.is_empty ())
        {
	  /* No blocks are reachable from EXIT at all.
             Find a dead-end from the ENTRY, and restart the iteration. */
	  basic_block be = dfs_find_deadend (ENTRY_BLOCK_PTR_FOR_FN (cfun));
          gcc_assert (be != NULL);
          bitmap_set_bit (visited, be->index);
	  stack.quick_push (ei_start (be->preds));
        }

      /* The only case the below while fires is
         when there's an infinite loop.  */
    }
  while (!stack.is_empty ());

  /* EXIT_BLOCK is always included.  */
  post_order->quick_push (EXIT_BLOCK);
}

/* Compute the depth first search order of FN and store in the array
   PRE_ORDER if nonzero.  If REV_POST_ORDER is nonzero, return the
   reverse completion number for each node.  Returns the number of nodes
   visited.  A depth first search tries to get as far away from the starting
   point as quickly as possible.

   In case the function has unreachable blocks the number of nodes
   visited does not include them.

   pre_order is a really a preorder numbering of the graph.
   rev_post_order is really a reverse postorder numbering of the graph.  */

int
pre_and_rev_post_order_compute_fn (struct function *fn,
				   int *pre_order, int *rev_post_order,
				   bool include_entry_exit)
{
  int pre_order_num = 0;
  int rev_post_order_num = n_basic_blocks_for_fn (cfun) - 1;

  /* Allocate stack for back-tracking up CFG.  */
  auto_vec<edge_iterator, 20> stack (n_basic_blocks_for_fn (cfun) + 1);

  if (include_entry_exit)
    {
      if (pre_order)
	pre_order[pre_order_num] = ENTRY_BLOCK;
      pre_order_num++;
      if (rev_post_order)
	rev_post_order[rev_post_order_num--] = EXIT_BLOCK;
    }
  else
    rev_post_order_num -= NUM_FIXED_BLOCKS;

  /* Allocate bitmap to track nodes that have been visited.  */
  auto_sbitmap visited (last_basic_block_for_fn (cfun));

  /* None of the nodes in the CFG have been visited yet.  */
  bitmap_clear (visited);

  /* Push the first edge on to the stack.  */
  stack.quick_push (ei_start (ENTRY_BLOCK_PTR_FOR_FN (fn)->succs));

  while (!stack.is_empty ())
    {
      basic_block src;
      basic_block dest;

      /* Look at the edge on the top of the stack.  */
      edge_iterator ei = stack.last ();
      src = ei_edge (ei)->src;
      dest = ei_edge (ei)->dest;

      /* Check if the edge destination has been visited yet.  */
      if (dest != EXIT_BLOCK_PTR_FOR_FN (fn)
	  && ! bitmap_bit_p (visited, dest->index))
	{
	  /* Mark that we have visited the destination.  */
	  bitmap_set_bit (visited, dest->index);

	  if (pre_order)
	    pre_order[pre_order_num] = dest->index;

	  pre_order_num++;

	  if (EDGE_COUNT (dest->succs) > 0)
	    /* Since the DEST node has been visited for the first
	       time, check its successors.  */
	    stack.quick_push (ei_start (dest->succs));
	  else if (rev_post_order)
	    /* There are no successors for the DEST node so assign
	       its reverse completion number.  */
	    rev_post_order[rev_post_order_num--] = dest->index;
	}
      else
	{
	  if (ei_one_before_end_p (ei)
	      && src != ENTRY_BLOCK_PTR_FOR_FN (fn)
	      && rev_post_order)
	    /* There are no more successors for the SRC node
	       so assign its reverse completion number.  */
	    rev_post_order[rev_post_order_num--] = src->index;

	  if (!ei_one_before_end_p (ei))
	    ei_next (&stack.last ());
	  else
	    stack.pop ();
	}
    }

  if (include_entry_exit)
    {
      if (pre_order)
	pre_order[pre_order_num] = EXIT_BLOCK;
      pre_order_num++;
      if (rev_post_order)
	rev_post_order[rev_post_order_num--] = ENTRY_BLOCK;
    }

  return pre_order_num;
}

/* Like pre_and_rev_post_order_compute_fn but operating on the
   current function and asserting that all nodes were visited.  */

int
pre_and_rev_post_order_compute (int *pre_order, int *rev_post_order,
				bool include_entry_exit)
{
  int pre_order_num
    = pre_and_rev_post_order_compute_fn (cfun, pre_order, rev_post_order,
					 include_entry_exit);
  if (include_entry_exit)
    /* The number of nodes visited should be the number of blocks.  */
    gcc_assert (pre_order_num == n_basic_blocks_for_fn (cfun));
  else
    /* The number of nodes visited should be the number of blocks minus
       the entry and exit blocks which are not visited here.  */
    gcc_assert (pre_order_num
		== (n_basic_blocks_for_fn (cfun) - NUM_FIXED_BLOCKS));

  return pre_order_num;
}

/* Unlike pre_and_rev_post_order_compute we fill rev_post_order backwards
   so iterating in RPO order needs to start with rev_post_order[n - 1]
   going to rev_post_order[0].  If FOR_ITERATION is true then try to
   make CFG cycles fit into small contiguous regions of the RPO order.
   When FOR_ITERATION is true this requires up-to-date loop structures.  */

int
rev_post_order_and_mark_dfs_back_seme (struct function *fn, edge entry,
				       bitmap exit_bbs, bool for_iteration,
				       int *rev_post_order)
{
  int pre_order_num = 0;
  int rev_post_order_num = 0;

  /* Allocate stack for back-tracking up CFG.  Worst case we need
     O(n^2) edges but the following should suffice in practice without
     a need to re-allocate.  */
  auto_vec<edge, 20> stack (2 * n_basic_blocks_for_fn (fn));

  int *pre = XNEWVEC (int, 2 * last_basic_block_for_fn (fn));
  int *post = pre + last_basic_block_for_fn (fn);

  /* BB flag to track nodes that have been visited.  */
  auto_bb_flag visited (fn);
  /* BB flag to track which nodes have post[] assigned to avoid
     zeroing post.  */
  auto_bb_flag post_assigned (fn);

  /* Push the first edge on to the stack.  */
  stack.quick_push (entry);

  while (!stack.is_empty ())
    {
      basic_block src;
      basic_block dest;

      /* Look at the edge on the top of the stack.  */
      int idx = stack.length () - 1;
      edge e = stack[idx];
      src = e->src;
      dest = e->dest;
      e->flags &= ~EDGE_DFS_BACK;

      /* Check if the edge destination has been visited yet.  */
      if (! bitmap_bit_p (exit_bbs, dest->index)
	  && ! (dest->flags & visited))
	{
	  /* Mark that we have visited the destination.  */
	  dest->flags |= visited;

	  pre[dest->index] = pre_order_num++;

	  if (EDGE_COUNT (dest->succs) > 0)
	    {
	      /* Since the DEST node has been visited for the first
		 time, check its successors.  */
	      /* Push the edge vector in reverse to match previous behavior.  */
	      stack.reserve (EDGE_COUNT (dest->succs));
	      for (int i = EDGE_COUNT (dest->succs) - 1; i >= 0; --i)
		stack.quick_push (EDGE_SUCC (dest, i));
	      /* Generalize to handle more successors?  */
	      if (for_iteration
		  && EDGE_COUNT (dest->succs) == 2)
		{
		  edge &e1 = stack[stack.length () - 2];
		  if (loop_exit_edge_p (e1->src->loop_father, e1))
		    std::swap (e1, stack.last ());
		}
	    }
	  else
	    {
	      /* There are no successors for the DEST node so assign
		 its reverse completion number.  */
	      post[dest->index] = rev_post_order_num;
	      dest->flags |= post_assigned;
	      rev_post_order[rev_post_order_num] = dest->index;
	      rev_post_order_num++;
	    }
	}
      else
	{
	  if (dest->flags & visited
	      && src != entry->src
	      && pre[src->index] >= pre[dest->index]
	      && !(dest->flags & post_assigned))
	    e->flags |= EDGE_DFS_BACK;

	  if (idx != 0 && stack[idx - 1]->src != src)
	    {
	      /* There are no more successors for the SRC node
		 so assign its reverse completion number.  */
	      post[src->index] = rev_post_order_num;
	      src->flags |= post_assigned;
	      rev_post_order[rev_post_order_num] = src->index;
	      rev_post_order_num++;
	    }

	  stack.pop ();
	}
    }

  XDELETEVEC (pre);

  /* Clear the temporarily allocated flags.  */
  for (int i = 0; i < rev_post_order_num; ++i)
    BASIC_BLOCK_FOR_FN (fn, rev_post_order[i])->flags
      &= ~(post_assigned|visited);

  return rev_post_order_num;
}



/* Compute the depth first search order on the _reverse_ graph and
   store it in the array DFS_ORDER, marking the nodes visited in VISITED.
   Returns the number of nodes visited.

   The computation is split into three pieces:

   flow_dfs_compute_reverse_init () creates the necessary data
   structures.

   flow_dfs_compute_reverse_add_bb () adds a basic block to the data
   structures.  The block will start the search.

   flow_dfs_compute_reverse_execute () continues (or starts) the
   search using the block on the top of the stack, stopping when the
   stack is empty.

   flow_dfs_compute_reverse_finish () destroys the necessary data
   structures.

   Thus, the user will probably call ..._init(), call ..._add_bb() to
   add a beginning basic block to the stack, call ..._execute(),
   possibly add another bb to the stack and again call ..._execute(),
   ..., and finally call _finish().  */

/* Initialize the data structures used for depth-first search on the
   reverse graph.  If INITIALIZE_STACK is nonzero, the exit block is
   added to the basic block stack.  DATA is the current depth-first
   search context.  If INITIALIZE_STACK is nonzero, there is an
   element on the stack.  */

depth_first_search::depth_first_search () :
  m_stack (n_basic_blocks_for_fn (cfun)),
  m_visited_blocks (last_basic_block_for_fn (cfun))
{
  bitmap_clear (m_visited_blocks);
}

/* Add the specified basic block to the top of the dfs data
   structures.  When the search continues, it will start at the
   block.  */

void
depth_first_search::add_bb (basic_block bb)
{
  m_stack.quick_push (bb);
  bitmap_set_bit (m_visited_blocks, bb->index);
}

/* Continue the depth-first search through the reverse graph starting with the
   block at the stack's top and ending when the stack is empty.  Visited nodes
   are marked.  Returns an unvisited basic block, or NULL if there is none
   available.  */

basic_block
depth_first_search::execute (basic_block last_unvisited)
{
  basic_block bb;
  edge e;
  edge_iterator ei;

  while (!m_stack.is_empty ())
    {
      bb = m_stack.pop ();

      /* Perform depth-first search on adjacent vertices.  */
      FOR_EACH_EDGE (e, ei, bb->preds)
	if (!bitmap_bit_p (m_visited_blocks, e->src->index))
	  add_bb (e->src);
    }

  /* Determine if there are unvisited basic blocks.  */
  FOR_BB_BETWEEN (bb, last_unvisited, NULL, prev_bb)
    if (!bitmap_bit_p (m_visited_blocks, bb->index))
      return bb;

  return NULL;
}

/* Performs dfs search from BB over vertices satisfying PREDICATE;
   if REVERSE, go against direction of edges.  Returns number of blocks
   found and their list in RSLT.  RSLT can contain at most RSLT_MAX items.  */
int
dfs_enumerate_from (basic_block bb, int reverse,
		    bool (*predicate) (const_basic_block, const void *),
		    basic_block *rslt, int rslt_max, const void *data)
{
  basic_block *st, lbb;
  int sp = 0, tv = 0;

  auto_bb_flag visited (cfun);

#define MARK_VISITED(BB) ((BB)->flags |= visited)
#define UNMARK_VISITED(BB) ((BB)->flags &= ~visited)
#define VISITED_P(BB) (((BB)->flags & visited) != 0)

  st = XNEWVEC (basic_block, rslt_max);
  rslt[tv++] = st[sp++] = bb;
  MARK_VISITED (bb);
  while (sp)
    {
      edge e;
      edge_iterator ei;
      lbb = st[--sp];
      if (reverse)
	{
	  FOR_EACH_EDGE (e, ei, lbb->preds)
	    if (!VISITED_P (e->src) && predicate (e->src, data))
	      {
		gcc_assert (tv != rslt_max);
		rslt[tv++] = st[sp++] = e->src;
		MARK_VISITED (e->src);
	      }
	}
      else
	{
	  FOR_EACH_EDGE (e, ei, lbb->succs)
	    if (!VISITED_P (e->dest) && predicate (e->dest, data))
	      {
		gcc_assert (tv != rslt_max);
		rslt[tv++] = st[sp++] = e->dest;
		MARK_VISITED (e->dest);
	      }
	}
    }
  free (st);
  for (sp = 0; sp < tv; sp++)
    UNMARK_VISITED (rslt[sp]);
  return tv;
#undef MARK_VISITED
#undef UNMARK_VISITED
#undef VISITED_P
}


/* Compute dominance frontiers, ala Harvey, Ferrante, et al.

   This algorithm can be found in Timothy Harvey's PhD thesis, at
   http://www.cs.rice.edu/~harv/dissertation.pdf in the section on iterative
   dominance algorithms.

   First, we identify each join point, j (any node with more than one
   incoming edge is a join point).

   We then examine each predecessor, p, of j and walk up the dominator tree
   starting at p.

   We stop the walk when we reach j's immediate dominator - j is in the
   dominance frontier of each of  the nodes in the walk, except for j's
   immediate dominator. Intuitively, all of the rest of j's dominators are
   shared by j's predecessors as well.
   Since they dominate j, they will not have j in their dominance frontiers.

   The number of nodes touched by this algorithm is equal to the size
   of the dominance frontiers, no more, no less.
*/


static void
compute_dominance_frontiers_1 (bitmap_head *frontiers)
{
  edge p;
  edge_iterator ei;
  basic_block b;
  FOR_EACH_BB_FN (b, cfun)
    {
      if (EDGE_COUNT (b->preds) >= 2)
	{
	  FOR_EACH_EDGE (p, ei, b->preds)
	    {
	      basic_block runner = p->src;
	      basic_block domsb;
	      if (runner == ENTRY_BLOCK_PTR_FOR_FN (cfun))
		continue;

	      domsb = get_immediate_dominator (CDI_DOMINATORS, b);
	      while (runner != domsb)
		{
		  if (!bitmap_set_bit (&frontiers[runner->index],
				       b->index))
		    break;
		  runner = get_immediate_dominator (CDI_DOMINATORS,
						    runner);
		}
	    }
	}
    }
}


void
compute_dominance_frontiers (bitmap_head *frontiers)
{
  timevar_push (TV_DOM_FRONTIERS);

  compute_dominance_frontiers_1 (frontiers);

  timevar_pop (TV_DOM_FRONTIERS);
}

/* Given a set of blocks with variable definitions (DEF_BLOCKS),
   return a bitmap with all the blocks in the iterated dominance
   frontier of the blocks in DEF_BLOCKS.  DFS contains dominance
   frontier information as returned by compute_dominance_frontiers.

   The resulting set of blocks are the potential sites where PHI nodes
   are needed.  The caller is responsible for freeing the memory
   allocated for the return value.  */

bitmap
compute_idf (bitmap def_blocks, bitmap_head *dfs)
{
  bitmap_iterator bi;
  unsigned bb_index, i;
  bitmap phi_insertion_points;

  /* Each block can appear at most twice on the work-stack.  */
  auto_vec<int> work_stack (2 * n_basic_blocks_for_fn (cfun));
  phi_insertion_points = BITMAP_ALLOC (NULL);

  /* Seed the work list with all the blocks in DEF_BLOCKS.  We use
     vec::quick_push here for speed.  This is safe because we know that
     the number of definition blocks is no greater than the number of
     basic blocks, which is the initial capacity of WORK_STACK.  */
  EXECUTE_IF_SET_IN_BITMAP (def_blocks, 0, bb_index, bi)
    work_stack.quick_push (bb_index);

  /* Pop a block off the worklist, add every block that appears in
     the original block's DF that we have not already processed to
     the worklist.  Iterate until the worklist is empty.   Blocks
     which are added to the worklist are potential sites for
     PHI nodes.  */
  while (work_stack.length () > 0)
    {
      bb_index = work_stack.pop ();

      /* Since the registration of NEW -> OLD name mappings is done
	 separately from the call to update_ssa, when updating the SSA
	 form, the basic blocks where new and/or old names are defined
	 may have disappeared by CFG cleanup calls.  In this case,
	 we may pull a non-existing block from the work stack.  */
      gcc_checking_assert (bb_index
			   < (unsigned) last_basic_block_for_fn (cfun));

      EXECUTE_IF_AND_COMPL_IN_BITMAP (&dfs[bb_index], phi_insertion_points,
	                              0, i, bi)
	{
	  work_stack.quick_push (i);
	  bitmap_set_bit (phi_insertion_points, i);
	}
    }

  return phi_insertion_points;
}

/* Intersection and union of preds/succs for sbitmap based data flow
   solvers.  All four functions defined below take the same arguments:
   B is the basic block to perform the operation for.  DST is the
   target sbitmap, i.e. the result.  SRC is an sbitmap vector of size
   last_basic_block so that it can be indexed with basic block indices.
   DST may be (but does not have to be) SRC[B->index].  */

/* Set the bitmap DST to the intersection of SRC of successors of
   basic block B.  */

void
bitmap_intersection_of_succs (sbitmap dst, sbitmap *src, basic_block b)
{
  unsigned int set_size = dst->size;
  edge e;
  unsigned ix;

  for (e = NULL, ix = 0; ix < EDGE_COUNT (b->succs); ix++)
    {
      e = EDGE_SUCC (b, ix);
      if (e->dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
	continue;

      bitmap_copy (dst, src[e->dest->index]);
      break;
    }

  if (e == 0)
    bitmap_ones (dst);
  else
    for (++ix; ix < EDGE_COUNT (b->succs); ix++)
      {
	unsigned int i;
	SBITMAP_ELT_TYPE *p, *r;

	e = EDGE_SUCC (b, ix);
	if (e->dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
	  continue;

	p = src[e->dest->index]->elms;
	r = dst->elms;
	for (i = 0; i < set_size; i++)
	  *r++ &= *p++;
      }
}

/* Set the bitmap DST to the intersection of SRC of predecessors of
   basic block B.  */

void
bitmap_intersection_of_preds (sbitmap dst, sbitmap *src, basic_block b)
{
  unsigned int set_size = dst->size;
  edge e;
  unsigned ix;

  for (e = NULL, ix = 0; ix < EDGE_COUNT (b->preds); ix++)
    {
      e = EDGE_PRED (b, ix);
      if (e->src == ENTRY_BLOCK_PTR_FOR_FN (cfun))
	continue;

      bitmap_copy (dst, src[e->src->index]);
      break;
    }

  if (e == 0)
    bitmap_ones (dst);
  else
    for (++ix; ix < EDGE_COUNT (b->preds); ix++)
      {
	unsigned int i;
	SBITMAP_ELT_TYPE *p, *r;

	e = EDGE_PRED (b, ix);
	if (e->src == ENTRY_BLOCK_PTR_FOR_FN (cfun))
	  continue;

	p = src[e->src->index]->elms;
	r = dst->elms;
	for (i = 0; i < set_size; i++)
	  *r++ &= *p++;
      }
}

/* Set the bitmap DST to the union of SRC of successors of
   basic block B.  */

void
bitmap_union_of_succs (sbitmap dst, sbitmap *src, basic_block b)
{
  unsigned int set_size = dst->size;
  edge e;
  unsigned ix;

  for (ix = 0; ix < EDGE_COUNT (b->succs); ix++)
    {
      e = EDGE_SUCC (b, ix);
      if (e->dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
	continue;

      bitmap_copy (dst, src[e->dest->index]);
      break;
    }

  if (ix == EDGE_COUNT (b->succs))
    bitmap_clear (dst);
  else
    for (ix++; ix < EDGE_COUNT (b->succs); ix++)
      {
	unsigned int i;
	SBITMAP_ELT_TYPE *p, *r;

	e = EDGE_SUCC (b, ix);
	if (e->dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
	  continue;

	p = src[e->dest->index]->elms;
	r = dst->elms;
	for (i = 0; i < set_size; i++)
	  *r++ |= *p++;
      }
}

/* Set the bitmap DST to the union of SRC of predecessors of
   basic block B.  */

void
bitmap_union_of_preds (sbitmap dst, sbitmap *src, basic_block b)
{
  unsigned int set_size = dst->size;
  edge e;
  unsigned ix;

  for (ix = 0; ix < EDGE_COUNT (b->preds); ix++)
    {
      e = EDGE_PRED (b, ix);
      if (e->src== ENTRY_BLOCK_PTR_FOR_FN (cfun))
	continue;

      bitmap_copy (dst, src[e->src->index]);
      break;
    }

  if (ix == EDGE_COUNT (b->preds))
    bitmap_clear (dst);
  else
    for (ix++; ix < EDGE_COUNT (b->preds); ix++)
      {
	unsigned int i;
	SBITMAP_ELT_TYPE *p, *r;

	e = EDGE_PRED (b, ix);
	if (e->src == ENTRY_BLOCK_PTR_FOR_FN (cfun))
	  continue;

	p = src[e->src->index]->elms;
	r = dst->elms;
	for (i = 0; i < set_size; i++)
	  *r++ |= *p++;
      }
}

/* Returns the list of basic blocks in the function in an order that guarantees
   that if a block X has just a single predecessor Y, then Y is after X in the
   ordering.  */

basic_block *
single_pred_before_succ_order (void)
{
  basic_block x, y;
  basic_block *order = XNEWVEC (basic_block, n_basic_blocks_for_fn (cfun));
  unsigned n = n_basic_blocks_for_fn (cfun) - NUM_FIXED_BLOCKS;
  unsigned np, i;
  auto_sbitmap visited (last_basic_block_for_fn (cfun));

#define MARK_VISITED(BB) (bitmap_set_bit (visited, (BB)->index))
#define VISITED_P(BB) (bitmap_bit_p (visited, (BB)->index))

  bitmap_clear (visited);

  MARK_VISITED (ENTRY_BLOCK_PTR_FOR_FN (cfun));
  FOR_EACH_BB_FN (x, cfun)
    {
      if (VISITED_P (x))
	continue;

      /* Walk the predecessors of x as long as they have precisely one
	 predecessor and add them to the list, so that they get stored
	 after x.  */
      for (y = x, np = 1;
	   single_pred_p (y) && !VISITED_P (single_pred (y));
	   y = single_pred (y))
	np++;
      for (y = x, i = n - np;
	   single_pred_p (y) && !VISITED_P (single_pred (y));
	   y = single_pred (y), i++)
	{
	  order[i] = y;
	  MARK_VISITED (y);
	}
      order[i] = y;
      MARK_VISITED (y);

      gcc_assert (i == n - 1);
      n -= np;
    }

  gcc_assert (n == 0);
  return order;

#undef MARK_VISITED
#undef VISITED_P
}

/* Ignoring loop backedges, if BB has precisely one incoming edge then
   return that edge.  Otherwise return NULL.

   When IGNORE_NOT_EXECUTABLE is true, also ignore edges that are not marked
   as executable.  */

edge
single_pred_edge_ignoring_loop_edges (basic_block bb,
				      bool ignore_not_executable)
{
  edge retval = NULL;
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      /* A loop back edge can be identified by the destination of
	 the edge dominating the source of the edge.  */
      if (dominated_by_p (CDI_DOMINATORS, e->src, e->dest))
	continue;

      /* We can safely ignore edges that are not executable.  */
      if (ignore_not_executable
	  && (e->flags & EDGE_EXECUTABLE) == 0)
	continue;

      /* If we have already seen a non-loop edge, then we must have
	 multiple incoming non-loop edges and thus we return NULL.  */
      if (retval)
	return NULL;

      /* This is the first non-loop incoming edge we have found.  Record
	 it.  */
      retval = e;
    }

  return retval;
}
