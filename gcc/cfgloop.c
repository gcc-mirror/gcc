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
#include "toplev.h"

/* Ratio of frequencies of edges so that one of more latch edges is
   considered to belong to inner loop with same header.  */
#define HEAVY_EDGE_RATIO 8

static void flow_loops_cfg_dump		PARAMS ((const struct loops *,
						 FILE *));
static void flow_loop_entry_edges_find	PARAMS ((struct loop *));
static void flow_loop_exit_edges_find	PARAMS ((struct loop *));
static int flow_loop_nodes_find		PARAMS ((basic_block, struct loop *));
static void flow_loop_pre_header_scan	PARAMS ((struct loop *));
static basic_block flow_loop_pre_header_find PARAMS ((basic_block,
						      dominance_info));
static int flow_loop_level_compute	PARAMS ((struct loop *));
static int flow_loops_level_compute	PARAMS ((struct loops *));
static basic_block make_forwarder_block PARAMS ((basic_block, int, int,
						 edge, int));
static void canonicalize_loop_headers   PARAMS ((void));
static bool glb_enum_p PARAMS ((basic_block, void *));
static void redirect_edge_with_latch_update PARAMS ((edge, basic_block));
static void flow_loop_free PARAMS ((struct loop *));

/* Dump loop related CFG information.  */

static void
flow_loops_cfg_dump (loops, file)
     const struct loops *loops;
     FILE *file;
{
  int i;
  basic_block bb;

  if (! loops->num || ! file || ! loops->cfg.dom)
    return;

  FOR_EACH_BB (bb)
    {
      edge succ;

      fprintf (file, ";; %d succs { ", bb->index);
      for (succ = bb->succ; succ; succ = succ->succ_next)
	fprintf (file, "%d ", succ->dest->index);
      fprintf (file, "}\n");
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

/* Return nonzero if the nodes of LOOP are a subset of OUTER.  */

bool
flow_loop_nested_p (outer, loop)
     const struct loop *outer;
     const struct loop *loop;
{
  return loop->depth > outer->depth
	 && loop->pred[outer->depth] == outer;
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
  basic_block *bbs;
  int i;

  if (! loop || ! loop->header)
    return;

  fprintf (file, ";;\n;; Loop %d:%s\n", loop->num,
	     loop->invalid ? " invalid" : "");

  fprintf (file, ";;  header %d, latch %d, pre-header %d\n",
	   loop->header->index, loop->latch->index,
	   loop->pre_header ? loop->pre_header->index : -1);
  fprintf (file, ";;  depth %d, level %d, outer %ld\n",
	   loop->depth, loop->level,
	   (long) (loop->outer ? loop->outer->num : -1));

  if (loop->pre_header_edges)
    flow_edge_list_print (";;  pre-header edges", loop->pre_header_edges,
			  loop->num_pre_header_edges, file);

  flow_edge_list_print (";;  entry edges", loop->entry_edges,
			loop->num_entries, file);
  fprintf (file, ";;  nodes:");
  bbs = get_loop_body (loop);
  for (i = 0; i < loop->num_nodes; i++)
    fprintf (file, " %d", bbs[i]->index);
  free (bbs);
  fprintf (file, "\n");
  flow_edge_list_print (";;  exit edges", loop->exit_edges,
			loop->num_exits, file);

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
  int i;
  int num_loops;

  num_loops = loops->num;
  if (! num_loops || ! file)
    return;

  fprintf (file, ";; %d loops found, %d levels\n",
	   num_loops, loops->levels);

  for (i = 0; i < num_loops; i++)
    {
      struct loop *loop = loops->parray[i];

      if (!loop)
	continue;

      flow_loop_dump (loop, file, loop_dump_aux, verbose);
    }

  if (verbose)
    flow_loops_cfg_dump (loops, file);
}

/* Free data allocated for LOOP.  */
static void
flow_loop_free (loop)
     struct loop *loop;
{
  if (loop->pre_header_edges)
    free (loop->pre_header_edges);
  if (loop->entry_edges)
    free (loop->entry_edges);
  if (loop->exit_edges)
    free (loop->exit_edges);
  if (loop->pred)
    free (loop->pred);
  free (loop);
}

/* Free all the memory allocated for LOOPS.  */

void
flow_loops_free (loops)
     struct loops *loops;
{
  if (loops->parray)
    {
      int i;

      if (! loops->num)
	abort ();

      /* Free the loop descriptors.  */
      for (i = 0; i < loops->num; i++)
	{
	  struct loop *loop = loops->parray[i];

	  if (!loop)
	    continue;

	  flow_loop_free (loop);
	}

      free (loops->parray);
      loops->parray = NULL;

      if (loops->cfg.dom)
	free_dominance_info (loops->cfg.dom);

      if (loops->cfg.dfs_order)
	free (loops->cfg.dfs_order);
      if (loops->cfg.rc_order)
	free (loops->cfg.rc_order);

    }
}

/* Find the entry edges into the LOOP.  */

static void 
flow_loop_entry_edges_find (loop)
     struct loop *loop;
{
  edge e;
  int num_entries;

  num_entries = 0;
  for (e = loop->header->pred; e; e = e->pred_next)
    {
      if (flow_loop_outside_edge_p (loop, e))
	num_entries++;
    }

  if (! num_entries)
    abort ();

  loop->entry_edges = (edge *) xmalloc (num_entries * sizeof (edge *));

  num_entries = 0;
  for (e = loop->header->pred; e; e = e->pred_next)
    {
      if (flow_loop_outside_edge_p (loop, e))
	loop->entry_edges[num_entries++] = e;
    }

  loop->num_entries = num_entries;
}

/* Find the exit edges from the LOOP.  */

static void
flow_loop_exit_edges_find (loop)
     struct loop *loop;
{
  edge e;
  basic_block node, *bbs;
  int num_exits, i;

  loop->exit_edges = NULL;
  loop->num_exits = 0;

  /* Check all nodes within the loop to see if there are any
     successors not in the loop.  Note that a node may have multiple
     exiting edges.  */
  num_exits = 0;
  bbs = get_loop_body (loop);
  for (i = 0; i < loop->num_nodes; i++)
    {
      node = bbs[i];
      for (e = node->succ; e; e = e->succ_next)
	{
	  basic_block dest = e->dest;

	  if (!flow_bb_inside_loop_p (loop, dest))
	    num_exits++;
	}
    }

  if (! num_exits)
    {
      free (bbs);
      return;
    }

  loop->exit_edges = (edge *) xmalloc (num_exits * sizeof (edge *));

  /* Store all exiting edges into an array.  */
  num_exits = 0;
  for (i = 0; i < loop->num_nodes; i++)
    {
      node = bbs[i];
      for (e = node->succ; e; e = e->succ_next)
	{
	  basic_block dest = e->dest;

	  if (!flow_bb_inside_loop_p (loop, dest))
	    loop->exit_edges[num_exits++] = e;
      }
    }
  free (bbs);
  loop->num_exits = num_exits;
}

/* Find the nodes contained within the LOOP with header HEADER.
   Return the number of nodes within the loop.  */

static int
flow_loop_nodes_find (header, loop)
     basic_block header;
     struct loop *loop;
{
  basic_block *stack;
  int sp;
  int num_nodes = 1;
  int findex, lindex;

  header->loop_father = loop;
  header->loop_depth = loop->depth;
  findex = lindex = header->index;

  if (loop->latch->loop_father != loop)
    {
      stack = (basic_block *) xmalloc (n_basic_blocks * sizeof (basic_block));
      sp = 0;
      num_nodes++;
      stack[sp++] = loop->latch;
      loop->latch->loop_father = loop;
      loop->latch->loop_depth = loop->depth;
 
      while (sp)
	{
	  basic_block node;
	  edge e;

	  node = stack[--sp];
      
	  for (e = node->pred; e; e = e->pred_next)
	    {
	      basic_block ancestor = e->src;

	      if (ancestor != ENTRY_BLOCK_PTR
		  && ancestor->loop_father != loop)
		{
		  ancestor->loop_father = loop;
		  ancestor->loop_depth = loop->depth;
		  num_nodes++;
		  stack[sp++] = ancestor;
		}
	    }
	}
      free (stack);
    }
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
     dominance_info dom;
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
	  && ! dominated_by_p (dom, node, header))
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

/* Add LOOP to the loop hierarchy tree where FATHER is father of the
   added loop.  */

void
flow_loop_tree_node_add (father, loop)
     struct loop *father;
     struct loop *loop;
{
  loop->next = father->inner;
  father->inner = loop;
  loop->outer = father;

  loop->depth = father->depth + 1;
  loop->pred = xmalloc (sizeof (struct loop *) * loop->depth);
  memcpy (loop->pred, father->pred, sizeof (struct loop *) * father->depth);
  loop->pred[father->depth] = father;
}

/* Remove LOOP from the loop hierarchy tree.  */

void
flow_loop_tree_node_remove (loop)
     struct loop *loop;
{
  struct loop *prev, *father;

  father = loop->outer;
  loop->outer = NULL;

  /* Remove loop from the list of sons.  */
  if (father->inner == loop)
    father->inner = loop->next;
  else
    {
      for (prev = father->inner; prev->next != loop; prev = prev->next);
      prev->next = loop->next;
    }

  loop->depth = -1;
  free (loop->pred);
  loop->pred = NULL;
}

/* Helper function to compute loop nesting depth and enclosed loop level
   for the natural loop specified by LOOP.  Returns the loop level.  */

static int
flow_loop_level_compute (loop)
     struct loop *loop;
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
      int ilevel = flow_loop_level_compute (inner) + 1;

      if (ilevel > level)
	level = ilevel;
    }

  loop->level = level;
  return level;
}

/* Compute the loop nesting depth and enclosed loop level for the loop
   hierarchy tree specified by LOOPS.  Return the maximum enclosed loop
   level.  */

static int
flow_loops_level_compute (loops)
     struct loops *loops;
{
  return flow_loop_level_compute (loops->tree_root);
}

/* Scan a single natural loop specified by LOOP collecting information
   about it specified by FLAGS.  */

int
flow_loop_scan (loops, loop, flags)
     struct loops *loops;
     struct loop *loop;
     int flags;
{
  if (flags & LOOP_ENTRY_EDGES)
    {
      /* Find edges which enter the loop header.
	 Note that the entry edges should only
	 enter the header of a natural loop.  */
      flow_loop_entry_edges_find (loop);
    }

  if (flags & LOOP_EXIT_EDGES)
    {
      /* Find edges which exit the loop.  */
      flow_loop_exit_edges_find (loop);
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

#define HEADER_BLOCK(B) (* (int *) (B)->aux)
#define LATCH_EDGE(E) (*(int *) (E)->aux)

/* Redirect edge and update latch and header info.  */
static void
redirect_edge_with_latch_update (e, to)
     edge e;
     basic_block to;
{
  basic_block jump;

  jump = redirect_edge_and_branch_force (e, to);
  if (jump)
    {
      alloc_aux_for_block (jump, sizeof (int));
      HEADER_BLOCK (jump) = 0;
      alloc_aux_for_edge (jump->pred, sizeof (int));
      LATCH_EDGE (jump->succ) = LATCH_EDGE (e);
      LATCH_EDGE (jump->pred) = 0;
    }
}

/* Split BB into entry part and rest; if REDIRECT_LATCH, redirect edges
   marked as latch into entry part, analogically for REDIRECT_NONLATCH.
   In both of these cases, ignore edge EXCEPT.  If CONN_LATCH, set edge
   between created entry part and BB as latch one.  Return created entry
   part.  */

static basic_block
make_forwarder_block (bb, redirect_latch, redirect_nonlatch, except,
		      conn_latch)
     basic_block bb;
     int redirect_latch;
     int redirect_nonlatch;
     edge except;
     int conn_latch;
{
  edge e, next_e, fallthru;
  basic_block dummy;
  rtx insn;

  insn = PREV_INSN (first_insn_after_basic_block_note (bb));

  fallthru = split_block (bb, insn);
  dummy = fallthru->src;
  bb = fallthru->dest;

  bb->aux = xmalloc (sizeof (int));
  HEADER_BLOCK (dummy) = 0;
  HEADER_BLOCK (bb) = 1;

  /* Redirect back edges we want to keep.  */
  for (e = dummy->pred; e; e = next_e)
    {
      next_e = e->pred_next;
      if (e == except
	  || !((redirect_latch && LATCH_EDGE (e))
	       || (redirect_nonlatch && !LATCH_EDGE (e))))
	{
	  dummy->frequency -= EDGE_FREQUENCY (e);
	  dummy->count -= e->count;
	  if (dummy->frequency < 0)
	    dummy->frequency = 0;
	  if (dummy->count < 0)
	    dummy->count = 0;
	  redirect_edge_with_latch_update (e, bb);
	}
    }

  alloc_aux_for_edge (fallthru, sizeof (int));
  LATCH_EDGE (fallthru) = conn_latch;

  return dummy;
}

/* Takes care of merging natural loops with shared headers.  */
static void
canonicalize_loop_headers ()
{
  dominance_info dom;
  basic_block header;
  edge e;
  
  /* Compute the dominators.  */
  dom = calculate_dominance_info (CDI_DOMINATORS);

  alloc_aux_for_blocks (sizeof (int));
  alloc_aux_for_edges (sizeof (int));

  /* Split blocks so that each loop has only single latch.  */
  FOR_EACH_BB (header)
    {
      int num_latches = 0;
      int have_abnormal_edge = 0;

      for (e = header->pred; e; e = e->pred_next)
	{
	  basic_block latch = e->src;

	  if (e->flags & EDGE_ABNORMAL)
	    have_abnormal_edge = 1;

	  if (latch != ENTRY_BLOCK_PTR
	      && dominated_by_p (dom, latch, header))
	    {
	      num_latches++;
	      LATCH_EDGE (e) = 1;
	    }
	}
      if (have_abnormal_edge)
	HEADER_BLOCK (header) = 0;
      else
	HEADER_BLOCK (header) = num_latches;
    }

  if (HEADER_BLOCK (ENTRY_BLOCK_PTR->succ->dest))
    {
      basic_block bb;

      /* We could not redirect edges freely here. On the other hand,
	 we can simply split the edge from entry block.  */
      bb = split_edge (ENTRY_BLOCK_PTR->succ);
 
      alloc_aux_for_edge (bb->succ, sizeof (int));
      LATCH_EDGE (bb->succ) = 0;
      alloc_aux_for_block (bb, sizeof (int));
      HEADER_BLOCK (bb) = 0;
    }

  FOR_EACH_BB (header)
    {
      int num_latch;
      int want_join_latch;
      int max_freq, is_heavy;
      edge heavy;

      if (!HEADER_BLOCK (header))
	continue;

      num_latch = HEADER_BLOCK (header);

      want_join_latch = (num_latch > 1);

      if (!want_join_latch)
	continue;

      /* Find a heavy edge.  */
      is_heavy = 1;
      heavy = NULL;
      max_freq = 0;
      for (e = header->pred; e; e = e->pred_next)
	if (LATCH_EDGE (e) &&
	    EDGE_FREQUENCY (e) > max_freq)
	  max_freq = EDGE_FREQUENCY (e);
      for (e = header->pred; e; e = e->pred_next)
	if (LATCH_EDGE (e) &&
	    EDGE_FREQUENCY (e) >= max_freq / HEAVY_EDGE_RATIO)
	  {
	    if (heavy)
	      {
		is_heavy = 0;
		break;
	      }
	    else
	      heavy = e;
	  }

      if (is_heavy)
	{
	  basic_block new_header =
	    make_forwarder_block (header, true, true, heavy, 0);
	  if (num_latch > 2)
	    make_forwarder_block (new_header, true, false, NULL, 1);
	}
      else
	make_forwarder_block (header, true, false, NULL, 1);
    }

  free_aux_for_blocks ();
  free_aux_for_edges ();
  free_dominance_info (dom);
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
  dominance_info dom;
  int *dfs_order;
  int *rc_order;
  basic_block header;
  basic_block bb;

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

  /* Join loops with shared headers.  */
  canonicalize_loop_headers ();

  /* Compute the dominators.  */
  dom = loops->cfg.dom = calculate_dominance_info (CDI_DOMINATORS);

  /* Count the number of loop headers.  This should be the
     same as the number of natural loops.  */
  headers = sbitmap_alloc (last_basic_block);
  sbitmap_zero (headers);

  num_loops = 0;
  FOR_EACH_BB (header)
    {
      int more_latches = 0;
     
      header->loop_depth = 0;

      /* If we have an abnormal predecessor, do not consider the
	 loop (not worth the problems).  */
      for (e = header->pred; e; e = e->pred_next)
	if (e->flags & EDGE_ABNORMAL)
	  break;
      if (e)
	continue;

      for (e = header->pred; e; e = e->pred_next)
	{
	  basic_block latch = e->src;

	  if (e->flags & EDGE_ABNORMAL)
	    abort ();

	  /* Look for back edges where a predecessor is dominated
	     by this block.  A natural loop has a single entry
	     node (header) that dominates all the nodes in the
	     loop.  It also has single back edge to the header
	     from a latch node.  */
	  if (latch != ENTRY_BLOCK_PTR && dominated_by_p (dom, latch, header))
	    {
	      /* Shared headers should be eliminated by now.  */
	      if (more_latches)
		abort ();
	      more_latches = 1;
	      SET_BIT (headers, header->index);
	      num_loops++;
	    }
	}
    }

  /* Allocate loop structures.  */
  loops->parray = (struct loop **) xcalloc (num_loops + 1, sizeof (struct loop *));

  /* Dummy loop containing whole function.  */
  loops->parray[0] = xcalloc (1, sizeof (struct loop));
  loops->parray[0]->next = NULL;
  loops->parray[0]->inner = NULL;
  loops->parray[0]->outer = NULL;
  loops->parray[0]->depth = 0;
  loops->parray[0]->pred = NULL;
  loops->parray[0]->num_nodes = n_basic_blocks + 2;
  loops->parray[0]->latch = EXIT_BLOCK_PTR;
  loops->parray[0]->header = ENTRY_BLOCK_PTR;
  ENTRY_BLOCK_PTR->loop_father = loops->parray[0];
  EXIT_BLOCK_PTR->loop_father = loops->parray[0];

  loops->tree_root = loops->parray[0];

  /* Find and record information about all the natural loops
     in the CFG.  */
  loops->num = 1;
  FOR_EACH_BB (bb)
    bb->loop_father = loops->tree_root;

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

      num_loops = 1;

      for (b = 0; b < n_basic_blocks; b++)
	{
	  struct loop *loop;

	  /* Search the nodes of the CFG in reverse completion order
	     so that we can find outer loops first.  */
	  if (!TEST_BIT (headers, rc_order[b]))
	    continue;

	  header = BASIC_BLOCK (rc_order[b]);
	  
	  loop = loops->parray[num_loops] = xcalloc (1, sizeof (struct loop));

	  loop->header = header;
	  loop->num = num_loops;
	  num_loops++;

	  /* Look for the latch for this header block.  */
	  for (e = header->pred; e; e = e->pred_next)
	    {
	      basic_block latch = e->src;

	      if (latch != ENTRY_BLOCK_PTR
		  && dominated_by_p (dom, latch, header))
		{
		  loop->latch = latch;
		  break;
		}
	    }

	  flow_loop_tree_node_add (header->loop_father, loop);
	  loop->num_nodes = flow_loop_nodes_find (loop->header, loop);
	}

      sbitmap_free (headers);

      /* Assign the loop nesting depth and enclosed loop level for each
	 loop.  */
      loops->levels = flow_loops_level_compute (loops);

      /* Scan the loops.  */
      for (i = 1; i < num_loops; i++)
	flow_loop_scan (loops, loops->parray[i], flags);

      loops->num = num_loops;
    }
  else
    {
      loops->cfg.dom = NULL;
      free_dominance_info (dom);
    }
#ifdef ENABLE_CHECKING
  verify_flow_info ();
  verify_loop_structure (loops, 0);
#endif

  return loops->num;
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
  if (loops->parray)
    flow_loops_free (loops);

  return flow_loops_find (loops, flags);
}

/* Return nonzero if basic block BB belongs to LOOP.  */
bool
flow_bb_inside_loop_p (loop, bb)
     const struct loop *loop;
     const basic_block bb;
{
  struct loop *source_loop;

  if (bb == ENTRY_BLOCK_PTR || bb == EXIT_BLOCK_PTR)
    return 0;

  source_loop = bb->loop_father;
  return loop == source_loop || flow_loop_nested_p (loop, source_loop);
}

/* Return nonzero if edge E enters header of LOOP from outside of LOOP.  */

bool
flow_loop_outside_edge_p (loop, e)
     const struct loop *loop;
     edge e;
{
  if (e->dest != loop->header)
    abort ();
  return !flow_bb_inside_loop_p (loop, e->src);
}

/* Enumeration predicate for get_loop_body.  */
static bool
glb_enum_p (bb, glb_header)
     basic_block bb;
     void *glb_header;
{
  return bb != (basic_block) glb_header;
}

/* Gets basic blocks of a loop.  */
basic_block *
get_loop_body (loop)
     const struct loop *loop;
{
  basic_block *tovisit, bb;
  int tv = 0;

  if (!loop->num_nodes)
    abort ();

  tovisit = xcalloc (loop->num_nodes, sizeof (basic_block));
  tovisit[tv++] = loop->header;

  if (loop->latch == EXIT_BLOCK_PTR)
    {
      /* There may be blocks unreachable from EXIT_BLOCK.  */
      if (loop->num_nodes != n_basic_blocks + 2)
	abort ();
      FOR_EACH_BB (bb)
	tovisit[tv++] = bb;
      tovisit[tv++] = EXIT_BLOCK_PTR;
    }
  else if (loop->latch != loop->header)
    {
      tv = dfs_enumerate_from (loop->latch, 1, glb_enum_p,
			       tovisit + 1, loop->num_nodes - 1,
			       loop->header) + 1;
    }

  if (tv != loop->num_nodes)
    abort ();
  return tovisit;
}

/* Adds basic block BB to LOOP.  */
void
add_bb_to_loop (bb, loop)
     basic_block bb;
     struct loop *loop;
 {
   int i;
 
   bb->loop_father = loop;
   bb->loop_depth = loop->depth;
   loop->num_nodes++;
   for (i = 0; i < loop->depth; i++)
     loop->pred[i]->num_nodes++;
 }

/* Remove basic block BB from loops.  */
void
remove_bb_from_loops (bb)
     basic_block bb;
 {
   int i;
   struct loop *loop = bb->loop_father;

   loop->num_nodes--;
   for (i = 0; i < loop->depth; i++)
     loop->pred[i]->num_nodes--;
   bb->loop_father = NULL;
   bb->loop_depth = 0;
 }

/* Finds nearest common ancestor in loop tree for given loops.  */
struct loop *
find_common_loop (loop_s, loop_d)
    struct loop *loop_s;
    struct loop *loop_d;
{
  if (!loop_s) return loop_d;
  if (!loop_d) return loop_s;
  
  if (loop_s->depth < loop_d->depth)
    loop_d = loop_d->pred[loop_s->depth];
  else if (loop_s->depth > loop_d->depth)
    loop_s = loop_s->pred[loop_d->depth];

  while (loop_s != loop_d)
    {
      loop_s = loop_s->outer;
      loop_d = loop_d->outer;
    }
  return loop_s;
}

/* Checks that LOOPS are allright:
     -- sizes of loops are allright
     -- results of get_loop_body really belong to the loop
     -- loop header have just single entry edge and single latch edge
     -- loop latches have only single successor that is header of their loop
  */
void
verify_loop_structure (loops, flags)
     struct loops *loops;
     int flags;
{
  int *sizes, i, j;
  basic_block *bbs, bb;
  struct loop *loop;
  int err = 0;

  /* Check sizes.  */
  sizes = xcalloc (loops->num, sizeof (int));
  sizes[0] = 2;

  FOR_EACH_BB (bb)
    for (loop = bb->loop_father; loop; loop = loop->outer)
      sizes[loop->num]++;

  for (i = 0; i < loops->num; i++)
    {
      if (!loops->parray[i])
        continue;

      if (loops->parray[i]->num_nodes != sizes[i])
	{
	  error ("Size of loop %d should be %d, not %d.",
		   i, sizes[i], loops->parray[i]->num_nodes);
	  err = 1;
	}
    }

  free (sizes);

  /* Check get_loop_body.  */
  for (i = 1; i < loops->num; i++)
    {
      loop = loops->parray[i];
      if (!loop)
	continue;
      bbs = get_loop_body (loop);

      for (j = 0; j < loop->num_nodes; j++)
	if (!flow_bb_inside_loop_p (loop, bbs[j]))
	  {
	    error ("Bb %d do not belong to loop %d.",
		    bbs[j]->index, i);
	    err = 1;
	  }
      free (bbs);
    }

  /* Check headers and latches.  */
  for (i = 1; i < loops->num; i++)
    {
      loop = loops->parray[i];
      if (!loop)
	continue;

      if ((flags & VLS_EXPECT_PREHEADERS)
	  && (!loop->header->pred->pred_next
	      || loop->header->pred->pred_next->pred_next))
	{
	  error ("Loop %d's header does not have exactly 2 entries.", i);
	  err = 1;
	}
      if (flags & VLS_EXPECT_SIMPLE_LATCHES)
	{
	  if (!loop->latch->succ
	      || loop->latch->succ->succ_next)
	    {
	      error ("Loop %d's latch does not have exactly 1 successor.", i);
	      err = 1;
	    }
	  if (loop->latch->succ->dest != loop->header)
	    {
	      error ("Loop %d's latch does not have header as successor.", i);
	      err = 1;
	    }
	  if (loop->latch->loop_father != loop)
	    {
	      error ("Loop %d's latch does not belong directly to it.", i);
	      err = 1;
	    }
	}
      if (loop->header->loop_father != loop)
	{
	  error ("Loop %d's header does not belong directly to it.", i);
	  err = 1;
	}
    }

  if (err)
    abort ();
}

/* Returns latch edge of LOOP.  */
edge
loop_latch_edge (loop)
     struct loop *loop;
{
  edge e;

  for (e = loop->header->pred; e->src != loop->latch; e = e->pred_next)
    continue;

  return e;
}

/* Returns preheader edge of LOOP.  */
edge
loop_preheader_edge (loop)
     struct loop *loop;
{
  edge e;

  for (e = loop->header->pred; e->src == loop->latch; e = e->pred_next)
    continue;

  return e;
}

