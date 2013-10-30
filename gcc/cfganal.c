/* Control flow graph analysis code for GNU compiler.
   Copyright (C) 1987-2013 Free Software Foundation, Inc.

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
#include "basic-block.h"
#include "vec.h"
#include "bitmap.h"
#include "sbitmap.h"
#include "timevar.h"

/* Store the data structures necessary for depth-first search.  */
struct depth_first_search_dsS {
  /* stack for backtracking during the algorithm */
  basic_block *stack;

  /* number of edges in the stack.  That is, positions 0, ..., sp-1
     have edges.  */
  unsigned int sp;

  /* record of basic blocks already seen by depth-first search */
  sbitmap visited_blocks;
};
typedef struct depth_first_search_dsS *depth_first_search_ds;

static void flow_dfs_compute_reverse_init (depth_first_search_ds);
static void flow_dfs_compute_reverse_add_bb (depth_first_search_ds,
					     basic_block);
static basic_block flow_dfs_compute_reverse_execute (depth_first_search_ds,
						     basic_block);
static void flow_dfs_compute_reverse_finish (depth_first_search_ds);

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
  edge_iterator *stack;
  int *pre;
  int *post;
  int sp;
  int prenum = 1;
  int postnum = 1;
  sbitmap visited;
  bool found = false;

  /* Allocate the preorder and postorder number arrays.  */
  pre = XCNEWVEC (int, last_basic_block);
  post = XCNEWVEC (int, last_basic_block);

  /* Allocate stack for back-tracking up CFG.  */
  stack = XNEWVEC (edge_iterator, n_basic_blocks + 1);
  sp = 0;

  /* Allocate bitmap to track nodes that have been visited.  */
  visited = sbitmap_alloc (last_basic_block);

  /* None of the nodes in the CFG have been visited yet.  */
  bitmap_clear (visited);

  /* Push the first edge on to the stack.  */
  stack[sp++] = ei_start (ENTRY_BLOCK_PTR->succs);

  while (sp)
    {
      edge_iterator ei;
      basic_block src;
      basic_block dest;

      /* Look at the edge on the top of the stack.  */
      ei = stack[sp - 1];
      src = ei_edge (ei)->src;
      dest = ei_edge (ei)->dest;
      ei_edge (ei)->flags &= ~EDGE_DFS_BACK;

      /* Check if the edge destination has been visited yet.  */
      if (dest != EXIT_BLOCK_PTR && ! bitmap_bit_p (visited, dest->index))
	{
	  /* Mark that we have visited the destination.  */
	  bitmap_set_bit (visited, dest->index);

	  pre[dest->index] = prenum++;
	  if (EDGE_COUNT (dest->succs) > 0)
	    {
	      /* Since the DEST node has been visited for the first
		 time, check its successors.  */
	      stack[sp++] = ei_start (dest->succs);
	    }
	  else
	    post[dest->index] = postnum++;
	}
      else
	{
	  if (dest != EXIT_BLOCK_PTR && src != ENTRY_BLOCK_PTR
	      && pre[src->index] >= pre[dest->index]
	      && post[dest->index] == 0)
	    ei_edge (ei)->flags |= EDGE_DFS_BACK, found = true;

	  if (ei_one_before_end_p (ei) && src != ENTRY_BLOCK_PTR)
	    post[src->index] = postnum++;

	  if (!ei_one_before_end_p (ei))
	    ei_next (&stack[sp - 1]);
	  else
	    sp--;
	}
    }

  free (pre);
  free (post);
  free (stack);
  sbitmap_free (visited);

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

  tos = worklist = XNEWVEC (basic_block, n_basic_blocks);

  /* Clear all the reachability flags.  */

  FOR_EACH_BB (bb)
    bb->flags &= ~BB_REACHABLE;

  /* Add our starting points to the worklist.  Almost always there will
     be only one.  It isn't inconceivable that we might one day directly
     support Fortran alternate entry points.  */

  FOR_EACH_EDGE (e, ei, ENTRY_BLOCK_PTR->succs)
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
  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, EXIT_BLOCK_PTR, next_bb)
    {
      num_edges += EDGE_COUNT (bb->succs);
    }

  elist = XNEW (struct edge_list);
  elist->num_edges = num_edges;
  elist->index_to_edge = XNEWVEC (edge, num_edges);

  num_edges = 0;

  /* Follow successors of blocks, and register these edges.  */
  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, EXIT_BLOCK_PTR, next_bb)
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
	   n_basic_blocks, elist->num_edges);

  for (x = 0; x < elist->num_edges; x++)
    {
      fprintf (f, " %-4d - edge(", x);
      if (INDEX_EDGE_PRED_BB (elist, x) == ENTRY_BLOCK_PTR)
	fprintf (f, "entry,");
      else
	fprintf (f, "%d,", INDEX_EDGE_PRED_BB (elist, x)->index);

      if (INDEX_EDGE_SUCC_BB (elist, x) == EXIT_BLOCK_PTR)
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

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, EXIT_BLOCK_PTR, next_bb)
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

  FOR_BB_BETWEEN (p, ENTRY_BLOCK_PTR, EXIT_BLOCK_PTR, next_bb)
    FOR_BB_BETWEEN (s, ENTRY_BLOCK_PTR->next_bb, NULL, next_bb)
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
  if (bb == ENTRY_BLOCK_PTR)
    return;
  gcc_assert (bb != EXIT_BLOCK_PTR);
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
  gcc_assert (block != ENTRY_BLOCK_PTR);

  if (block == EXIT_BLOCK_PTR)
    return EXIT_BLOCK_PTR;
  else
    {
      basic_block bb = get_immediate_dominator (CDI_POST_DOMINATORS, block);
      if (! bb)
	return EXIT_BLOCK_PTR;
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

  gcc_assert (INDEX_EDGE_PRED_BB (m_el, edge_index) != EXIT_BLOCK_PTR);

  if (INDEX_EDGE_PRED_BB (m_el, edge_index) == ENTRY_BLOCK_PTR)
    ending_block = single_succ (ENTRY_BLOCK_PTR);
  else
    ending_block = find_pdom (INDEX_EDGE_PRED_BB (m_el, edge_index));

  for (current_block = INDEX_EDGE_SUCC_BB (m_el, edge_index);
       current_block != ending_block && current_block != EXIT_BLOCK_PTR;
       current_block = find_pdom (current_block))
    {
      edge e = INDEX_EDGE (m_el, edge_index);

      /* For abnormal edges, we don't make current_block control
	 dependent because instructions that throw are always necessary
	 anyway.  */
      if (e->flags & EDGE_ABNORMAL)
	continue;

      set_control_dependence_map_bit (current_block, edge_index);
    }
}

/* Record all blocks' control dependences on all edges in the edge
   list EL, ala Morgan, Section 3.6.  */

control_dependences::control_dependences (struct edge_list *edges)
  : m_el (edges)
{
  timevar_push (TV_CONTROL_DEPENDENCES);
  control_dependence_map.create (last_basic_block);
  for (int i = 0; i < last_basic_block; ++i)
    control_dependence_map.quick_push (BITMAP_ALLOC (NULL));
  for (int i = 0; i < NUM_EDGES (m_el); ++i)
    find_control_dependence (i);
  timevar_pop (TV_CONTROL_DEPENDENCES);
}

/* Free control dependences and the associated edge list.  */

control_dependences::~control_dependences ()
{
  for (unsigned i = 0; i < control_dependence_map.length (); ++i)
    BITMAP_FREE (control_dependence_map[i]);
  control_dependence_map.release ();
  free_edge_list (m_el);
}

/* Returns the bitmap of edges the basic-block I is dependent on.  */

bitmap
control_dependences::get_edges_dependent_on (int i)
{
  return control_dependence_map[i];
}

/* Returns the edge with index I from the edge list.  */

edge
control_dependences::get_edge (int i)
{
  return INDEX_EDGE (m_el, i);
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

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR->next_bb, NULL, next_bb)
    remove_fake_predecessors (bb);
}

/* This routine will remove all fake edges to the EXIT_BLOCK.  */

void
remove_fake_exit_edges (void)
{
  remove_fake_predecessors (EXIT_BLOCK_PTR);
}


/* This function will add a fake edge between any block which has no
   successors, and the exit block. Some data flow equations require these
   edges to exist.  */

void
add_noreturn_fake_exit_edges (void)
{
  basic_block bb;

  FOR_EACH_BB (bb)
    if (EDGE_COUNT (bb->succs) == 0)
      make_single_succ_edge (bb, EXIT_BLOCK_PTR, EDGE_FAKE);
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
  basic_block unvisited_block = EXIT_BLOCK_PTR;
  basic_block deadend_block;
  struct depth_first_search_dsS dfs_ds;

  /* Perform depth-first search in the reverse graph to find nodes
     reachable from the exit block.  */
  flow_dfs_compute_reverse_init (&dfs_ds);
  flow_dfs_compute_reverse_add_bb (&dfs_ds, EXIT_BLOCK_PTR);

  /* Repeatedly add fake edges, updating the unreachable nodes.  */
  while (1)
    {
      unvisited_block = flow_dfs_compute_reverse_execute (&dfs_ds,
							  unvisited_block);
      if (!unvisited_block)
	break;

      deadend_block = dfs_find_deadend (unvisited_block);
      make_edge (deadend_block, EXIT_BLOCK_PTR, EDGE_FAKE);
      flow_dfs_compute_reverse_add_bb (&dfs_ds, deadend_block);
    }

  flow_dfs_compute_reverse_finish (&dfs_ds);
  return;
}

/* Compute reverse top sort order.  This is computing a post order
   numbering of the graph.  If INCLUDE_ENTRY_EXIT is true, then
   ENTRY_BLOCK and EXIT_BLOCK are included.  If DELETE_UNREACHABLE is
   true, unreachable blocks are deleted.  */

int
post_order_compute (int *post_order, bool include_entry_exit,
		    bool delete_unreachable)
{
  edge_iterator *stack;
  int sp;
  int post_order_num = 0;
  sbitmap visited;
  int count;

  if (include_entry_exit)
    post_order[post_order_num++] = EXIT_BLOCK;

  /* Allocate stack for back-tracking up CFG.  */
  stack = XNEWVEC (edge_iterator, n_basic_blocks + 1);
  sp = 0;

  /* Allocate bitmap to track nodes that have been visited.  */
  visited = sbitmap_alloc (last_basic_block);

  /* None of the nodes in the CFG have been visited yet.  */
  bitmap_clear (visited);

  /* Push the first edge on to the stack.  */
  stack[sp++] = ei_start (ENTRY_BLOCK_PTR->succs);

  while (sp)
    {
      edge_iterator ei;
      basic_block src;
      basic_block dest;

      /* Look at the edge on the top of the stack.  */
      ei = stack[sp - 1];
      src = ei_edge (ei)->src;
      dest = ei_edge (ei)->dest;

      /* Check if the edge destination has been visited yet.  */
      if (dest != EXIT_BLOCK_PTR && ! bitmap_bit_p (visited, dest->index))
	{
	  /* Mark that we have visited the destination.  */
	  bitmap_set_bit (visited, dest->index);

	  if (EDGE_COUNT (dest->succs) > 0)
	    /* Since the DEST node has been visited for the first
	       time, check its successors.  */
	    stack[sp++] = ei_start (dest->succs);
	  else
	    post_order[post_order_num++] = dest->index;
	}
      else
	{
	  if (ei_one_before_end_p (ei) && src != ENTRY_BLOCK_PTR)
	    post_order[post_order_num++] = src->index;

	  if (!ei_one_before_end_p (ei))
	    ei_next (&stack[sp - 1]);
	  else
	    sp--;
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
  if (delete_unreachable && (count != n_basic_blocks))
    {
      basic_block b;
      basic_block next_bb;
      for (b = ENTRY_BLOCK_PTR->next_bb; b != EXIT_BLOCK_PTR; b = next_bb)
	{
	  next_bb = b->next_bb;

	  if (!(bitmap_bit_p (visited, b->index)))
	    delete_basic_block (b);
	}

      tidy_fallthru_edges ();
    }

  free (stack);
  sbitmap_free (visited);
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
  bitmap visited = BITMAP_ALLOC (NULL);

  for (;;)
    {
      if (EDGE_COUNT (bb->succs) == 0
	  || ! bitmap_set_bit (visited, bb->index))
        {
          BITMAP_FREE (visited);
          return bb;
        }

      bb = EDGE_SUCC (bb, 0)->dest;
    }

  gcc_unreachable ();
}


/* Compute the reverse top sort order of the inverted CFG
   i.e. starting from the exit block and following the edges backward
   (from successors to predecessors).
   This ordering can be used for forward dataflow problems among others.

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

int
inverted_post_order_compute (int *post_order)
{
  basic_block bb;
  edge_iterator *stack;
  int sp;
  int post_order_num = 0;
  sbitmap visited;

  /* Allocate stack for back-tracking up CFG.  */
  stack = XNEWVEC (edge_iterator, n_basic_blocks + 1);
  sp = 0;

  /* Allocate bitmap to track nodes that have been visited.  */
  visited = sbitmap_alloc (last_basic_block);

  /* None of the nodes in the CFG have been visited yet.  */
  bitmap_clear (visited);

  /* Put all blocks that have no successor into the initial work list.  */
  FOR_ALL_BB (bb)
    if (EDGE_COUNT (bb->succs) == 0)
      {
        /* Push the initial edge on to the stack.  */
        if (EDGE_COUNT (bb->preds) > 0)
          {
            stack[sp++] = ei_start (bb->preds);
            bitmap_set_bit (visited, bb->index);
          }
      }

  do
    {
      bool has_unvisited_bb = false;

      /* The inverted traversal loop. */
      while (sp)
        {
          edge_iterator ei;
          basic_block pred;

          /* Look at the edge on the top of the stack.  */
          ei = stack[sp - 1];
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
                stack[sp++] = ei_start (pred->preds);
              else
                post_order[post_order_num++] = pred->index;
            }
          else
            {
              if (bb != EXIT_BLOCK_PTR && ei_one_before_end_p (ei))
                post_order[post_order_num++] = bb->index;

              if (!ei_one_before_end_p (ei))
                ei_next (&stack[sp - 1]);
              else
                sp--;
            }
        }

      /* Detect any infinite loop and activate the kludge.
         Note that this doesn't check EXIT_BLOCK itself
         since EXIT_BLOCK is always added after the outer do-while loop.  */
      FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, EXIT_BLOCK_PTR, next_bb)
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
                    stack[sp++] = ei_start (be->preds);
                    break;
                  }
              }
          }

      if (has_unvisited_bb && sp == 0)
        {
          /* No blocks are reachable from EXIT at all.
             Find a dead-end from the ENTRY, and restart the iteration. */
          basic_block be = dfs_find_deadend (ENTRY_BLOCK_PTR);
          gcc_assert (be != NULL);
          bitmap_set_bit (visited, be->index);
          stack[sp++] = ei_start (be->preds);
        }

      /* The only case the below while fires is
         when there's an infinite loop.  */
    }
  while (sp);

  /* EXIT_BLOCK is always included.  */
  post_order[post_order_num++] = EXIT_BLOCK;

  free (stack);
  sbitmap_free (visited);
  return post_order_num;
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
  edge_iterator *stack;
  int sp;
  int pre_order_num = 0;
  int rev_post_order_num = n_basic_blocks - 1;
  sbitmap visited;

  /* Allocate stack for back-tracking up CFG.  */
  stack = XNEWVEC (edge_iterator, n_basic_blocks + 1);
  sp = 0;

  if (include_entry_exit)
    {
      if (pre_order)
	pre_order[pre_order_num] = ENTRY_BLOCK;
      pre_order_num++;
      if (rev_post_order)
	rev_post_order[rev_post_order_num--] = ENTRY_BLOCK;
    }
  else
    rev_post_order_num -= NUM_FIXED_BLOCKS;

  /* Allocate bitmap to track nodes that have been visited.  */
  visited = sbitmap_alloc (last_basic_block);

  /* None of the nodes in the CFG have been visited yet.  */
  bitmap_clear (visited);

  /* Push the first edge on to the stack.  */
  stack[sp++] = ei_start (ENTRY_BLOCK_PTR_FOR_FUNCTION (fn)->succs);

  while (sp)
    {
      edge_iterator ei;
      basic_block src;
      basic_block dest;

      /* Look at the edge on the top of the stack.  */
      ei = stack[sp - 1];
      src = ei_edge (ei)->src;
      dest = ei_edge (ei)->dest;

      /* Check if the edge destination has been visited yet.  */
      if (dest != EXIT_BLOCK_PTR_FOR_FUNCTION (fn)
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
	    stack[sp++] = ei_start (dest->succs);
	  else if (rev_post_order)
	    /* There are no successors for the DEST node so assign
	       its reverse completion number.  */
	    rev_post_order[rev_post_order_num--] = dest->index;
	}
      else
	{
	  if (ei_one_before_end_p (ei)
	      && src != ENTRY_BLOCK_PTR_FOR_FUNCTION (fn)
	      && rev_post_order)
	    /* There are no more successors for the SRC node
	       so assign its reverse completion number.  */
	    rev_post_order[rev_post_order_num--] = src->index;

	  if (!ei_one_before_end_p (ei))
	    ei_next (&stack[sp - 1]);
	  else
	    sp--;
	}
    }

  free (stack);
  sbitmap_free (visited);

  if (include_entry_exit)
    {
      if (pre_order)
	pre_order[pre_order_num] = EXIT_BLOCK;
      pre_order_num++;
      if (rev_post_order)
	rev_post_order[rev_post_order_num--] = EXIT_BLOCK;
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
    gcc_assert (pre_order_num == n_basic_blocks);
  else
    /* The number of nodes visited should be the number of blocks minus
       the entry and exit blocks which are not visited here.  */
    gcc_assert (pre_order_num == n_basic_blocks - NUM_FIXED_BLOCKS);

  return pre_order_num;
}

/* Compute the depth first search order on the _reverse_ graph and
   store in the array DFS_ORDER, marking the nodes visited in VISITED.
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

static void
flow_dfs_compute_reverse_init (depth_first_search_ds data)
{
  /* Allocate stack for back-tracking up CFG.  */
  data->stack = XNEWVEC (basic_block, n_basic_blocks);
  data->sp = 0;

  /* Allocate bitmap to track nodes that have been visited.  */
  data->visited_blocks = sbitmap_alloc (last_basic_block);

  /* None of the nodes in the CFG have been visited yet.  */
  bitmap_clear (data->visited_blocks);

  return;
}

/* Add the specified basic block to the top of the dfs data
   structures.  When the search continues, it will start at the
   block.  */

static void
flow_dfs_compute_reverse_add_bb (depth_first_search_ds data, basic_block bb)
{
  data->stack[data->sp++] = bb;
  bitmap_set_bit (data->visited_blocks, bb->index);
}

/* Continue the depth-first search through the reverse graph starting with the
   block at the stack's top and ending when the stack is empty.  Visited nodes
   are marked.  Returns an unvisited basic block, or NULL if there is none
   available.  */

static basic_block
flow_dfs_compute_reverse_execute (depth_first_search_ds data,
				  basic_block last_unvisited)
{
  basic_block bb;
  edge e;
  edge_iterator ei;

  while (data->sp > 0)
    {
      bb = data->stack[--data->sp];

      /* Perform depth-first search on adjacent vertices.  */
      FOR_EACH_EDGE (e, ei, bb->preds)
	if (!bitmap_bit_p (data->visited_blocks, e->src->index))
	  flow_dfs_compute_reverse_add_bb (data, e->src);
    }

  /* Determine if there are unvisited basic blocks.  */
  FOR_BB_BETWEEN (bb, last_unvisited, NULL, prev_bb)
    if (!bitmap_bit_p (data->visited_blocks, bb->index))
      return bb;

  return NULL;
}

/* Destroy the data structures needed for depth-first search on the
   reverse graph.  */

static void
flow_dfs_compute_reverse_finish (depth_first_search_ds data)
{
  free (data->stack);
  sbitmap_free (data->visited_blocks);
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
  unsigned size;

  /* A bitmap to keep track of visited blocks.  Allocating it each time
     this function is called is not possible, since dfs_enumerate_from
     is often used on small (almost) disjoint parts of cfg (bodies of
     loops), and allocating a large sbitmap would lead to quadratic
     behavior.  */
  static sbitmap visited;
  static unsigned v_size;

#define MARK_VISITED(BB) (bitmap_set_bit (visited, (BB)->index))
#define UNMARK_VISITED(BB) (bitmap_clear_bit (visited, (BB)->index))
#define VISITED_P(BB) (bitmap_bit_p (visited, (BB)->index))

  /* Resize the VISITED sbitmap if necessary.  */
  size = last_basic_block;
  if (size < 10)
    size = 10;

  if (!visited)
    {

      visited = sbitmap_alloc (size);
      bitmap_clear (visited);
      v_size = size;
    }
  else if (v_size < size)
    {
      /* Ensure that we increase the size of the sbitmap exponentially.  */
      if (2 * v_size > size)
	size = 2 * v_size;

      visited = sbitmap_resize (visited, size, 0);
      v_size = size;
    }

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
  FOR_EACH_BB (b)
    {
      if (EDGE_COUNT (b->preds) >= 2)
	{
	  FOR_EACH_EDGE (p, ei, b->preds)
	    {
	      basic_block runner = p->src;
	      basic_block domsb;
	      if (runner == ENTRY_BLOCK_PTR)
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
  vec<int> work_stack;
  bitmap phi_insertion_points;

  /* Each block can appear at most twice on the work-stack.  */
  work_stack.create (2 * n_basic_blocks);
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
      gcc_checking_assert (bb_index < (unsigned) last_basic_block);

      EXECUTE_IF_AND_COMPL_IN_BITMAP (&dfs[bb_index], phi_insertion_points,
	                              0, i, bi)
	{
	  work_stack.quick_push (i);
	  bitmap_set_bit (phi_insertion_points, i);
	}
    }

  work_stack.release ();

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

  gcc_assert (!dst->popcount);

  for (e = NULL, ix = 0; ix < EDGE_COUNT (b->succs); ix++)
    {
      e = EDGE_SUCC (b, ix);
      if (e->dest == EXIT_BLOCK_PTR)
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
	if (e->dest == EXIT_BLOCK_PTR)
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

  gcc_assert (!dst->popcount);

  for (e = NULL, ix = 0; ix < EDGE_COUNT (b->preds); ix++)
    {
      e = EDGE_PRED (b, ix);
      if (e->src == ENTRY_BLOCK_PTR)
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
	if (e->src == ENTRY_BLOCK_PTR)
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

  gcc_assert (!dst->popcount);

  for (ix = 0; ix < EDGE_COUNT (b->succs); ix++)
    {
      e = EDGE_SUCC (b, ix);
      if (e->dest == EXIT_BLOCK_PTR)
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
	if (e->dest == EXIT_BLOCK_PTR)
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

  gcc_assert (!dst->popcount);

  for (ix = 0; ix < EDGE_COUNT (b->preds); ix++)
    {
      e = EDGE_PRED (b, ix);
      if (e->src== ENTRY_BLOCK_PTR)
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
	if (e->src == ENTRY_BLOCK_PTR)
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
  basic_block *order = XNEWVEC (basic_block, n_basic_blocks);
  unsigned n = n_basic_blocks - NUM_FIXED_BLOCKS;
  unsigned np, i;
  sbitmap visited = sbitmap_alloc (last_basic_block);

#define MARK_VISITED(BB) (bitmap_set_bit (visited, (BB)->index))
#define VISITED_P(BB) (bitmap_bit_p (visited, (BB)->index))

  bitmap_clear (visited);

  MARK_VISITED (ENTRY_BLOCK_PTR);
  FOR_EACH_BB (x)
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

  sbitmap_free (visited);
  gcc_assert (n == 0);
  return order;

#undef MARK_VISITED
#undef VISITED_P
}
