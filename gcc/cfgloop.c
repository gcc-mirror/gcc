/* Natural loop discovery code for GNU compiler.
   Copyright (C) 2000, 2001 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "hard-reg-set.h"
#include "basic-block.h"

static void flow_loops_cfg_dump		PARAMS ((const struct loops *,
						 FILE *));
static int flow_loop_nested_p		PARAMS ((struct loop *,
						 struct loop *));
static int flow_loop_entry_edges_find	PARAMS ((basic_block, const sbitmap,
						 edge **));
static int flow_loop_exit_edges_find	PARAMS ((const sbitmap, edge **));
static int flow_loop_nodes_find		PARAMS ((basic_block, basic_block,
						 sbitmap));
static void flow_loop_pre_header_scan	PARAMS ((struct loop *));
static basic_block flow_loop_pre_header_find PARAMS ((basic_block,
						      const sbitmap *));
static void flow_loop_tree_node_add	PARAMS ((struct loop *,
						 struct loop *));
static void flow_loops_tree_build	PARAMS ((struct loops *));
static int flow_loop_level_compute	PARAMS ((struct loop *, int));
static int flow_loops_level_compute	PARAMS ((struct loops *));

/* Dump loop related CFG information.  */

static void
flow_loops_cfg_dump (loops, file)
     const struct loops *loops;
     FILE *file;
{
  int i;

  if (! loops->num || ! file || ! loops->cfg.dom)
    return;

  for (i = 0; i < n_basic_blocks; i++)
    {
      edge succ;

      fprintf (file, ";; %d succs { ", i);
      for (succ = BASIC_BLOCK (i)->succ; succ; succ = succ->succ_next)
	fprintf (file, "%d ", succ->dest->index);
      flow_nodes_print ("} dom", loops->cfg.dom[i], file);
    }

  /* Dump the DFS node order.  */
  if (loops->cfg.dfs_order)
    {
      fputs (";; DFS order: ", file);
      for (i = 0; i < n_basic_blocks; i++)
	fprintf (file, "%d ", loops->cfg.dfs_order[i]);

      fputs ("\n", file);
    }

  /* Dump the reverse completion node order.  */
  if (loops->cfg.rc_order)
    {
      fputs (";; RC order: ", file);
      for (i = 0; i < n_basic_blocks; i++)
	fprintf (file, "%d ", loops->cfg.rc_order[i]);

      fputs ("\n", file);
    }
}

/* Return non-zero if the nodes of LOOP are a subset of OUTER.  */

static int
flow_loop_nested_p (outer, loop)
     struct loop *outer;
     struct loop *loop;
{
  return sbitmap_a_subset_b_p (loop->nodes, outer->nodes);
}

/* Dump the loop information specified by LOOP to the stream FILE
   using auxiliary dump callback function LOOP_DUMP_AUX if non null.  */

void
flow_loop_dump (loop, file, loop_dump_aux, verbose)
     const struct loop *loop;
     FILE *file;
     void (*loop_dump_aux) PARAMS((const struct loop *, FILE *, int));
     int verbose;
{
  if (! loop || ! loop->header)
    return;

  if (loop->first->head && loop->last->end)
    fprintf (file, ";;\n;; Loop %d (%d to %d):%s%s\n",
	    loop->num, INSN_UID (loop->first->head),
	    INSN_UID (loop->last->end),
	    loop->shared ? " shared" : "", loop->invalid ? " invalid" : "");
  else
    fprintf (file, ";;\n;; Loop %d:%s%s\n", loop->num,
	     loop->shared ? " shared" : "", loop->invalid ? " invalid" : "");

  fprintf (file, ";;  header %d, latch %d, pre-header %d, first %d, last %d\n",
	   loop->header->index, loop->latch->index,
	   loop->pre_header ? loop->pre_header->index : -1,
	   loop->first->index, loop->last->index);
  fprintf (file, ";;  depth %d, level %d, outer %ld\n",
	   loop->depth, loop->level,
	   (long) (loop->outer ? loop->outer->num : -1));

  if (loop->pre_header_edges)
    flow_edge_list_print (";;  pre-header edges", loop->pre_header_edges,
			  loop->num_pre_header_edges, file);

  flow_edge_list_print (";;  entry edges", loop->entry_edges,
			loop->num_entries, file);
  fprintf (file, ";;  %d", loop->num_nodes);
  flow_nodes_print (" nodes", loop->nodes, file);
  flow_edge_list_print (";;  exit edges", loop->exit_edges,
			loop->num_exits, file);

  if (loop->exits_doms)
    flow_nodes_print (";;  exit doms", loop->exits_doms, file);

  if (loop_dump_aux)
    loop_dump_aux (loop, file, verbose);
}

/* Dump the loop information specified by LOOPS to the stream FILE,
   using auxiliary dump callback function LOOP_DUMP_AUX if non null.  */

void
flow_loops_dump (loops, file, loop_dump_aux, verbose)
     const struct loops *loops;
     FILE *file;
     void (*loop_dump_aux) PARAMS((const struct loop *, FILE *, int));
     int verbose;
{
  int i, j;
  int num_loops;

  num_loops = loops->num;
  if (! num_loops || ! file)
    return;

  fprintf (file, ";; %d loops found, %d levels\n", num_loops, loops->levels);
  for (i = 0; i < num_loops; i++)
    {
      struct loop *loop = &loops->array[i];

      flow_loop_dump (loop, file, loop_dump_aux, verbose);
      if (loop->shared)
	for (j = 0; j < i; j++)
	  {
	    struct loop *oloop = &loops->array[j];

	    if (loop->header == oloop->header)
	      {
		int disjoint;
		int smaller;

		smaller = loop->num_nodes < oloop->num_nodes;

		/* If the union of LOOP and OLOOP is different than
		   the larger of LOOP and OLOOP then LOOP and OLOOP
		   must be disjoint.  */
		disjoint = ! flow_loop_nested_p (smaller ? loop : oloop,
						 smaller ? oloop : loop);
		fprintf (file,
			 ";; loop header %d shared by loops %d, %d %s\n",
			 loop->header->index, i, j,
			 disjoint ? "disjoint" : "nested");
	      }
	  }
    }

  if (verbose)
    flow_loops_cfg_dump (loops, file);
}

/* Free all the memory allocated for LOOPS.  */

void
flow_loops_free (loops)
     struct loops *loops;
{
  if (loops->array)
    {
      int i;

      if (! loops->num)
	abort ();

      /* Free the loop descriptors.  */
      for (i = 0; i < loops->num; i++)
	{
	  struct loop *loop = &loops->array[i];

	  if (loop->pre_header_edges)
	    free (loop->pre_header_edges);
	  if (loop->nodes)
	    sbitmap_free (loop->nodes);
	  if (loop->entry_edges)
	    free (loop->entry_edges);
	  if (loop->exit_edges)
	    free (loop->exit_edges);
	  if (loop->exits_doms)
	    sbitmap_free (loop->exits_doms);
	}

      free (loops->array);
      loops->array = NULL;

      if (loops->cfg.dom)
	sbitmap_vector_free (loops->cfg.dom);

      if (loops->cfg.dfs_order)
	free (loops->cfg.dfs_order);

      if (loops->shared_headers)
	sbitmap_free (loops->shared_headers);
    }
}

/* Find the entry edges into the loop with header HEADER and nodes
   NODES and store in ENTRY_EDGES array.  Return the number of entry
   edges from the loop.  */

static int
flow_loop_entry_edges_find (header, nodes, entry_edges)
     basic_block header;
     const sbitmap nodes;
     edge **entry_edges;
{
  edge e;
  int num_entries;

  *entry_edges = NULL;

  num_entries = 0;
  for (e = header->pred; e; e = e->pred_next)
    {
      basic_block src = e->src;

      if (src == ENTRY_BLOCK_PTR || ! TEST_BIT (nodes, src->index))
	num_entries++;
    }

  if (! num_entries)
    abort ();

  *entry_edges = (edge *) xmalloc (num_entries * sizeof (edge));

  num_entries = 0;
  for (e = header->pred; e; e = e->pred_next)
    {
      basic_block src = e->src;

      if (src == ENTRY_BLOCK_PTR || ! TEST_BIT (nodes, src->index))
	(*entry_edges)[num_entries++] = e;
    }

  return num_entries;
}

/* Find the exit edges from the loop using the bitmap of loop nodes
   NODES and store in EXIT_EDGES array.  Return the number of
   exit edges from the loop.  */

static int
flow_loop_exit_edges_find (nodes, exit_edges)
     const sbitmap nodes;
     edge **exit_edges;
{
  edge e;
  int node;
  int num_exits;

  *exit_edges = NULL;

  /* Check all nodes within the loop to see if there are any
     successors not in the loop.  Note that a node may have multiple
     exiting edges ?????  A node can have one jumping edge and one fallthru
     edge so only one of these can exit the loop.  */
  num_exits = 0;
  EXECUTE_IF_SET_IN_SBITMAP (nodes, 0, node, {
    for (e = BASIC_BLOCK (node)->succ; e; e = e->succ_next)
      {
	basic_block dest = e->dest;

	if (dest == EXIT_BLOCK_PTR || ! TEST_BIT (nodes, dest->index))
	    num_exits++;
      }
  });

  if (! num_exits)
    return 0;

  *exit_edges = (edge *) xmalloc (num_exits * sizeof (edge));

  /* Store all exiting edges into an array.  */
  num_exits = 0;
  EXECUTE_IF_SET_IN_SBITMAP (nodes, 0, node, {
    for (e = BASIC_BLOCK (node)->succ; e; e = e->succ_next)
      {
	basic_block dest = e->dest;

	if (dest == EXIT_BLOCK_PTR || ! TEST_BIT (nodes, dest->index))
	  (*exit_edges)[num_exits++] = e;
      }
  });

  return num_exits;
}

/* Find the nodes contained within the loop with header HEADER and
   latch LATCH and store in NODES.  Return the number of nodes within
   the loop.  */

static int
flow_loop_nodes_find (header, latch, nodes)
     basic_block header;
     basic_block latch;
     sbitmap nodes;
{
  basic_block *stack;
  int sp;
  int num_nodes = 0;

  stack = (basic_block *) xmalloc (n_basic_blocks * sizeof (basic_block));
  sp = 0;

  /* Start with only the loop header in the set of loop nodes.  */
  sbitmap_zero (nodes);
  SET_BIT (nodes, header->index);
  num_nodes++;
  header->loop_depth++;

  /* Push the loop latch on to the stack.  */
  if (! TEST_BIT (nodes, latch->index))
    {
      SET_BIT (nodes, latch->index);
      latch->loop_depth++;
      num_nodes++;
      stack[sp++] = latch;
    }

  while (sp)
    {
      basic_block node;
      edge e;

      node = stack[--sp];
      for (e = node->pred; e; e = e->pred_next)
	{
	  basic_block ancestor = e->src;

	  /* If each ancestor not marked as part of loop, add to set of
	     loop nodes and push on to stack.  */
	  if (ancestor != ENTRY_BLOCK_PTR
	      && ! TEST_BIT (nodes, ancestor->index))
	    {
	      SET_BIT (nodes, ancestor->index);
	      ancestor->loop_depth++;
	      num_nodes++;
	      stack[sp++] = ancestor;
	    }
	}
    }
  free (stack);
  return num_nodes;
}

/* Find the root node of the loop pre-header extended basic block and
   the edges along the trace from the root node to the loop header.  */

static void
flow_loop_pre_header_scan (loop)
     struct loop *loop;
{
  int num;
  basic_block ebb;
  edge e;

  loop->num_pre_header_edges = 0;
  if (loop->num_entries != 1)
    return;

  ebb = loop->entry_edges[0]->src;
  if (ebb == ENTRY_BLOCK_PTR)
    return;

  /* Count number of edges along trace from loop header to
     root of pre-header extended basic block.  Usually this is
     only one or two edges.  */
  for (num = 1; ebb->pred->src != ENTRY_BLOCK_PTR && ! ebb->pred->pred_next;
       num++)
    ebb = ebb->pred->src;

  loop->pre_header_edges = (edge *) xmalloc (num * sizeof (edge));
  loop->num_pre_header_edges = num;

  /* Store edges in order that they are followed.  The source of the first edge
     is the root node of the pre-header extended basic block and the
     destination of the last last edge is the loop header.  */
  for (e = loop->entry_edges[0]; num; e = e->src->pred)
    loop->pre_header_edges[--num] = e;
}

/* Return the block for the pre-header of the loop with header
   HEADER where DOM specifies the dominator information.  Return NULL if
   there is no pre-header.  */

static basic_block
flow_loop_pre_header_find (header, dom)
     basic_block header;
     const sbitmap *dom;
{
  basic_block pre_header;
  edge e;

  /* If block p is a predecessor of the header and is the only block
     that the header does not dominate, then it is the pre-header.  */
  pre_header = NULL;
  for (e = header->pred; e; e = e->pred_next)
    {
      basic_block node = e->src;

      if (node != ENTRY_BLOCK_PTR
	  && ! TEST_BIT (dom[node->index], header->index))
	{
	  if (pre_header == NULL)
	    pre_header = node;
	  else
	    {
	      /* There are multiple edges into the header from outside
		 the loop so there is no pre-header block.  */
	      pre_header = NULL;
	      break;
	    }
	}
    }

  return pre_header;
}

/* Add LOOP to the loop hierarchy tree where PREVLOOP was the loop
   previously added.  The insertion algorithm assumes that the loops
   are added in the order found by a depth first search of the CFG.  */

static void
flow_loop_tree_node_add (prevloop, loop)
     struct loop *prevloop;
     struct loop *loop;
{

  if (flow_loop_nested_p (prevloop, loop))
    {
      prevloop->inner = loop;
      loop->outer = prevloop;
      return;
    }

  for (; prevloop->outer; prevloop = prevloop->outer)
    if (flow_loop_nested_p (prevloop->outer, loop))
      {
	prevloop->next = loop;
	loop->outer = prevloop->outer;
	return;
      }

  prevloop->next = loop;
  loop->outer = NULL;
}

/* Build the loop hierarchy tree for LOOPS.  */

static void
flow_loops_tree_build (loops)
     struct loops *loops;
{
  int i;
  int num_loops;

  num_loops = loops->num;
  if (! num_loops)
    return;

  /* Root the loop hierarchy tree with the first loop found.
     Since we used a depth first search this should be the
     outermost loop.  */
  loops->tree_root = &loops->array[0];
  loops->tree_root->outer = loops->tree_root->inner
    = loops->tree_root->next = NULL;

  /* Add the remaining loops to the tree.  */
  for (i = 1; i < num_loops; i++)
    flow_loop_tree_node_add (&loops->array[i - 1], &loops->array[i]);
}

/* Helper function to compute loop nesting depth and enclosed loop level
   for the natural loop specified by LOOP at the loop depth DEPTH.
   Returns the loop level.  */

static int
flow_loop_level_compute (loop, depth)
     struct loop *loop;
     int depth;
{
  struct loop *inner;
  int level = 1;

  if (! loop)
    return 0;

  /* Traverse loop tree assigning depth and computing level as the
     maximum level of all the inner loops of this loop.  The loop
     level is equivalent to the height of the loop in the loop tree
     and corresponds to the number of enclosed loop levels (including
     itself).  */
  for (inner = loop->inner; inner; inner = inner->next)
    {
      int ilevel = flow_loop_level_compute (inner, depth + 1) + 1;

      level = MAX (ilevel, level);
    }

  loop->level = level;
  loop->depth = depth;
  return level;
}

/* Compute the loop nesting depth and enclosed loop level for the loop
   hierarchy tree specified by LOOPS.  Return the maximum enclosed loop
   level.  */

static int
flow_loops_level_compute (loops)
     struct loops *loops;
{
  int levels = 0;
  struct loop *loop;
  int level;

  /* Traverse all the outer level loops.  */
  for (loop = loops->tree_root; loop; loop = loop->next)
    {
      level = flow_loop_level_compute (loop, 1);
      levels = MAX (levels, level);
    }

  return levels;
}

/* Scan a single natural loop specified by LOOP collecting information
   about it specified by FLAGS.  */

int
flow_loop_scan (loops, loop, flags)
     struct loops *loops;
     struct loop *loop;
     int flags;
{
  /* Determine prerequisites.  */
  if ((flags & LOOP_EXITS_DOMS) && ! loop->exit_edges)
    flags |= LOOP_EXIT_EDGES;

  if (flags & LOOP_ENTRY_EDGES)
    /* Find edges which enter the loop header.  Note that the entry edges
       should only enter the header of a natural loop.  */
    loop->num_entries = flow_loop_entry_edges_find (loop->header, loop->nodes,
						    &loop->entry_edges);

  if (flags & LOOP_EXIT_EDGES)
    /* Find edges which exit the loop.  */
    loop->num_exits
      = flow_loop_exit_edges_find (loop->nodes, &loop->exit_edges);

  if (flags & LOOP_EXITS_DOMS)
    {
      int j;

      /* Determine which loop nodes dominate all the exits
	 of the loop.  */
      loop->exits_doms = sbitmap_alloc (n_basic_blocks);
      sbitmap_copy (loop->exits_doms, loop->nodes);
      for (j = 0; j < loop->num_exits; j++)
	sbitmap_a_and_b (loop->exits_doms, loop->exits_doms,
			 loops->cfg.dom[loop->exit_edges[j]->src->index]);

      /* The header of a natural loop must dominate
	 all exits.  */
      if (! TEST_BIT (loop->exits_doms, loop->header->index))
	abort ();
    }

  if (flags & LOOP_PRE_HEADER)
    {
      /* Look to see if the loop has a pre-header node.  */
      loop->pre_header
	= flow_loop_pre_header_find (loop->header, loops->cfg.dom);

      /* Find the blocks within the extended basic block of
	 the loop pre-header.  */
      flow_loop_pre_header_scan (loop);
    }

  return 1;
}

/* Find all the natural loops in the function and save in LOOPS structure and
   recalculate loop_depth information in basic block structures.  FLAGS
   controls which loop information is collected.  Return the number of natural
   loops found.  */

int
flow_loops_find (loops, flags)
     struct loops *loops;
     int flags;
{
  int i;
  int b;
  int num_loops;
  edge e;
  sbitmap headers;
  sbitmap *dom;
  int *dfs_order;
  int *rc_order;

  /* This function cannot be repeatedly called with different
     flags to build up the loop information.  The loop tree
     must always be built if this function is called.  */
  if (! (flags & LOOP_TREE))
    abort ();

  memset (loops, 0, sizeof *loops);

  /* Taking care of this degenerate case makes the rest of
     this code simpler.  */
  if (n_basic_blocks == 0)
    return 0;

  dfs_order = NULL;
  rc_order = NULL;

  /* Compute the dominators.  */
  dom = sbitmap_vector_alloc (n_basic_blocks, n_basic_blocks);
  calculate_dominance_info (NULL, dom, CDI_DOMINATORS);

  /* Count the number of loop edges (back edges).  This should be the
     same as the number of natural loops.  */
  num_loops = 0;
  for (b = 0; b < n_basic_blocks; b++)
    {
      basic_block header;

      header = BASIC_BLOCK (b);
      header->loop_depth = 0;

      for (e = header->pred; e; e = e->pred_next)
	{
	  basic_block latch = e->src;

	  /* Look for back edges where a predecessor is dominated
	     by this block.  A natural loop has a single entry
	     node (header) that dominates all the nodes in the
	     loop.  It also has single back edge to the header
	     from a latch node.  Note that multiple natural loops
	     may share the same header.  */
	  if (b != header->index)
	    abort ();

	  if (latch != ENTRY_BLOCK_PTR && TEST_BIT (dom[latch->index], b))
	    num_loops++;
	}
    }

  if (num_loops)
    {
      /* Compute depth first search order of the CFG so that outer
	 natural loops will be found before inner natural loops.  */
      dfs_order = (int *) xmalloc (n_basic_blocks * sizeof (int));
      rc_order = (int *) xmalloc (n_basic_blocks * sizeof (int));
      flow_depth_first_order_compute (dfs_order, rc_order);

      /* Save CFG derived information to avoid recomputing it.  */
      loops->cfg.dom = dom;
      loops->cfg.dfs_order = dfs_order;
      loops->cfg.rc_order = rc_order;

      /* Allocate loop structures.  */
      loops->array
	= (struct loop *) xcalloc (num_loops, sizeof (struct loop));

      headers = sbitmap_alloc (n_basic_blocks);
      sbitmap_zero (headers);

      loops->shared_headers = sbitmap_alloc (n_basic_blocks);
      sbitmap_zero (loops->shared_headers);

      /* Find and record information about all the natural loops
	 in the CFG.  */
      num_loops = 0;
      for (b = n_basic_blocks - 1; b >= 0; b--)
	{
	  basic_block latch;

	  /* Search the nodes of the CFG in reverse completion order
	     so that we can find outer loops first.  */
	  latch = BASIC_BLOCK (rc_order[b]);

	  /* Look for all the possible headers for this latch block.  */
	  for (e = latch->succ; e; e = e->succ_next)
	    {
	      basic_block header = e->dest;

	      /* Look for forward edges where this block is dominated by
		 a successor of this block.  A natural loop has a single
		 entry node (header) that dominates all the nodes in the
		 loop.  It also has single back edge to the header from a
		 latch node.  Note that multiple natural loops may share
		 the same header.  */
	      if (header != EXIT_BLOCK_PTR
		  && TEST_BIT (dom[latch->index], header->index))
		{
		  struct loop *loop;

		  loop = loops->array + num_loops;

		  loop->header = header;
		  loop->latch = latch;
		  loop->num = num_loops;

		  num_loops++;
		}
	    }
	}

      for (i = 0; i < num_loops; i++)
	{
	  struct loop *loop = &loops->array[i];

	  /* Keep track of blocks that are loop headers so
	     that we can tell which loops should be merged.  */
	  if (TEST_BIT (headers, loop->header->index))
	    SET_BIT (loops->shared_headers, loop->header->index);
	  SET_BIT (headers, loop->header->index);

	  /* Find nodes contained within the loop.  */
	  loop->nodes = sbitmap_alloc (n_basic_blocks);
	  loop->num_nodes
	    = flow_loop_nodes_find (loop->header, loop->latch, loop->nodes);

	  /* Compute first and last blocks within the loop.
	     These are often the same as the loop header and
	     loop latch respectively, but this is not always
	     the case.  */
	  loop->first
	    = BASIC_BLOCK (sbitmap_first_set_bit (loop->nodes));
	  loop->last
	    = BASIC_BLOCK (sbitmap_last_set_bit (loop->nodes));

	  flow_loop_scan (loops, loop, flags);
	}

      /* Natural loops with shared headers may either be disjoint or
	 nested.  Disjoint loops with shared headers cannot be inner
	 loops and should be merged.  For now just mark loops that share
	 headers.  */
      for (i = 0; i < num_loops; i++)
	if (TEST_BIT (loops->shared_headers, loops->array[i].header->index))
	  loops->array[i].shared = 1;

      sbitmap_free (headers);
    }
  else
    sbitmap_vector_free (dom);

  loops->num = num_loops;

  /* Build the loop hierarchy tree.  */
  flow_loops_tree_build (loops);

  /* Assign the loop nesting depth and enclosed loop level for each
     loop.  */
  loops->levels = flow_loops_level_compute (loops);

  return num_loops;
}

/* Update the information regarding the loops in the CFG
   specified by LOOPS.  */

int
flow_loops_update (loops, flags)
     struct loops *loops;
     int flags;
{
  /* One day we may want to update the current loop data.  For now
     throw away the old stuff and rebuild what we need.  */
  if (loops->array)
    flow_loops_free (loops);

  return flow_loops_find (loops, flags);
}

/* Return non-zero if edge E enters header of LOOP from outside of LOOP.  */

int
flow_loop_outside_edge_p (loop, e)
     const struct loop *loop;
     edge e;
{
  if (e->dest != loop->header)
    abort ();

  return (e->src == ENTRY_BLOCK_PTR)
    || ! TEST_BIT (loop->nodes, e->src->index);
}
