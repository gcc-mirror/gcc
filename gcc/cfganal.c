/* Control flow graph analysis code for GNU compiler.
   Copyright (C) 1987, 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001 Free Software Foundation, Inc.

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

/* This file contains various simple utilities to analyze the CFG.  */
#include "config.h"
#include "system.h"
#include "rtl.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "insn-config.h"
#include "recog.h"
#include "toplev.h"
#include "obstack.h"
#include "tm_p.h"

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

static void flow_dfs_compute_reverse_init
  PARAMS ((depth_first_search_ds));
static void flow_dfs_compute_reverse_add_bb
  PARAMS ((depth_first_search_ds, basic_block));
static basic_block flow_dfs_compute_reverse_execute
  PARAMS ((depth_first_search_ds));
static void flow_dfs_compute_reverse_finish
  PARAMS ((depth_first_search_ds));
static void remove_fake_successors	PARAMS ((basic_block));
static bool need_fake_edge_p		PARAMS ((rtx));
static bool keep_with_call_p		PARAMS ((rtx));
static bool flow_active_insn_p		PARAMS ((rtx));

/* Like active_insn_p, except keep the return value clobber around
   even after reload.  */

static bool
flow_active_insn_p (insn)
     rtx insn;
{
  if (active_insn_p (insn))
    return true;

  /* A clobber of the function return value exists for buggy 
     programs that fail to return a value.  It's effect is to
     keep the return value from being live across the entire
     function.  If we allow it to be skipped, we introduce the
     possibility for register livetime aborts.  */
  if (GET_CODE (PATTERN (insn)) == CLOBBER
      && GET_CODE (XEXP (PATTERN (insn), 0)) == REG
      && REG_FUNCTION_VALUE_P (XEXP (PATTERN (insn), 0)))
    return true;

  return false;
}

/* Return true if the block has no effect and only forwards control flow to
   its single destination.  */

bool
forwarder_block_p (bb)
     basic_block bb;
{
  rtx insn;

  if (bb == EXIT_BLOCK_PTR || bb == ENTRY_BLOCK_PTR
      || !bb->succ || bb->succ->succ_next)
    return false;

  for (insn = bb->head; insn != bb->end; insn = NEXT_INSN (insn))
    if (INSN_P (insn) && flow_active_insn_p (insn))
      return false;

  return (!INSN_P (insn)
	  || (GET_CODE (insn) == JUMP_INSN && simplejump_p (insn))
	  || !flow_active_insn_p (insn));
}

/* Return nonzero if we can reach target from src by falling through.  */

bool
can_fallthru (src, target)
     basic_block src, target;
{
  rtx insn = src->end;
  rtx insn2 = target->head;

  if (src->index + 1 == target->index && !active_insn_p (insn2))
    insn2 = next_active_insn (insn2);

  /* ??? Later we may add code to move jump tables offline.  */
  return next_active_insn (insn) == insn2;
}

/* Mark the back edges in DFS traversal.
   Return non-zero if a loop (natural or otherwise) is present.
   Inspired by Depth_First_Search_PP described in:

     Advanced Compiler Design and Implementation
     Steven Muchnick
     Morgan Kaufmann, 1997

   and heavily borrowed from flow_depth_first_order_compute.  */

bool
mark_dfs_back_edges ()
{
  edge *stack;
  int *pre;
  int *post;
  int sp;
  int prenum = 1;
  int postnum = 1;
  sbitmap visited;
  bool found = false;

  /* Allocate the preorder and postorder number arrays.  */
  pre = (int *) xcalloc (n_basic_blocks, sizeof (int));
  post = (int *) xcalloc (n_basic_blocks, sizeof (int));

  /* Allocate stack for back-tracking up CFG.  */
  stack = (edge *) xmalloc ((n_basic_blocks + 1) * sizeof (edge));
  sp = 0;

  /* Allocate bitmap to track nodes that have been visited.  */
  visited = sbitmap_alloc (n_basic_blocks);

  /* None of the nodes in the CFG have been visited yet.  */
  sbitmap_zero (visited);

  /* Push the first edge on to the stack.  */
  stack[sp++] = ENTRY_BLOCK_PTR->succ;

  while (sp)
    {
      edge e;
      basic_block src;
      basic_block dest;

      /* Look at the edge on the top of the stack.  */
      e = stack[sp - 1];
      src = e->src;
      dest = e->dest;
      e->flags &= ~EDGE_DFS_BACK;

      /* Check if the edge destination has been visited yet.  */
      if (dest != EXIT_BLOCK_PTR && ! TEST_BIT (visited, dest->index))
	{
	  /* Mark that we have visited the destination.  */
	  SET_BIT (visited, dest->index);

	  pre[dest->index] = prenum++;
	  if (dest->succ)
	    {
	      /* Since the DEST node has been visited for the first
		 time, check its successors.  */
	      stack[sp++] = dest->succ;
	    }
	  else
	    post[dest->index] = postnum++;
	}
      else
	{
	  if (dest != EXIT_BLOCK_PTR && src != ENTRY_BLOCK_PTR
	      && pre[src->index] >= pre[dest->index]
	      && post[dest->index] == 0)
	    e->flags |= EDGE_DFS_BACK, found = true;

	  if (! e->succ_next && src != ENTRY_BLOCK_PTR)
	    post[src->index] = postnum++;

	  if (e->succ_next)
	    stack[sp - 1] = e->succ_next;
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

/* Return true if we need to add fake edge to exit.
   Helper function for the flow_call_edges_add.  */

static bool
need_fake_edge_p (insn)
     rtx insn;
{
  if (!INSN_P (insn))
    return false;

  if ((GET_CODE (insn) == CALL_INSN
       && !SIBLING_CALL_P (insn)
       && !find_reg_note (insn, REG_NORETURN, NULL)
       && !find_reg_note (insn, REG_ALWAYS_RETURN, NULL)
       && !CONST_OR_PURE_CALL_P (insn)))
    return true;

  return ((GET_CODE (PATTERN (insn)) == ASM_OPERANDS
	   && MEM_VOLATILE_P (PATTERN (insn)))
	  || (GET_CODE (PATTERN (insn)) == PARALLEL
	      && asm_noperands (insn) != -1
	      && MEM_VOLATILE_P (XVECEXP (PATTERN (insn), 0, 0)))
	  || GET_CODE (PATTERN (insn)) == ASM_INPUT);
}

/* Return true if INSN should be kept in the same block as a preceding call.
   This is done for a single-set whose destination is a fixed register or
   whose source is the function return value.  This is a helper function for
   flow_call_edges_add.  */

static bool
keep_with_call_p (insn)
     rtx insn;
{
  rtx set;

  if (INSN_P (insn) && (set = single_set (insn)) != NULL)
    {
      if (GET_CODE (SET_DEST (set)) == REG
	  && fixed_regs[REGNO (SET_DEST (set))]
	  && general_operand (SET_SRC (set), VOIDmode))
	return true;
      if (GET_CODE (SET_SRC (set)) == REG
	  && FUNCTION_VALUE_REGNO_P (REGNO (SET_SRC (set)))
	  && GET_CODE (SET_DEST (set)) == REG
	  && REGNO (SET_DEST (set)) >= FIRST_PSEUDO_REGISTER)
	return true;
    }
  return false;
}

/* Add fake edges to the function exit for any non constant and non noreturn
   calls, volatile inline assembly in the bitmap of blocks specified by
   BLOCKS or to the whole CFG if BLOCKS is zero.  Return the number of blocks
   that were split.

   The goal is to expose cases in which entering a basic block does not imply
   that all subsequent instructions must be executed.  */

int
flow_call_edges_add (blocks)
     sbitmap blocks;
{
  int i;
  int blocks_split = 0;
  int bb_num = 0;
  basic_block *bbs;
  bool check_last_block = false;

  /* Map bb indices into basic block pointers since split_block
     will renumber the basic blocks.  */

  bbs = xmalloc (n_basic_blocks * sizeof (*bbs));

  if (! blocks)
    {
      for (i = 0; i < n_basic_blocks; i++)
	bbs[bb_num++] = BASIC_BLOCK (i);

      check_last_block = true;
    }
  else
    EXECUTE_IF_SET_IN_SBITMAP (blocks, 0, i,
			       {
				 bbs[bb_num++] = BASIC_BLOCK (i);
				 if (i == n_basic_blocks - 1)
				   check_last_block = true;
			       });

  /* In the last basic block, before epilogue generation, there will be
     a fallthru edge to EXIT.  Special care is required if the last insn
     of the last basic block is a call because make_edge folds duplicate
     edges, which would result in the fallthru edge also being marked
     fake, which would result in the fallthru edge being removed by
     remove_fake_edges, which would result in an invalid CFG.

     Moreover, we can't elide the outgoing fake edge, since the block
     profiler needs to take this into account in order to solve the minimal
     spanning tree in the case that the call doesn't return.

     Handle this by adding a dummy instruction in a new last basic block.  */
  if (check_last_block)
    {
      basic_block bb = BASIC_BLOCK (n_basic_blocks - 1);
      rtx insn = bb->end;

      /* Back up past insns that must be kept in the same block as a call.  */
      while (insn != bb->head
	     && keep_with_call_p (insn))
	insn = PREV_INSN (insn);

      if (need_fake_edge_p (insn))
	{
	  edge e;

	  for (e = bb->succ; e; e = e->succ_next)
	    if (e->dest == EXIT_BLOCK_PTR)
	      break;

	  insert_insn_on_edge (gen_rtx_USE (VOIDmode, const0_rtx), e);
	  commit_edge_insertions ();
	}
    }

  /* Now add fake edges to the function exit for any non constant
     calls since there is no way that we can determine if they will
     return or not...  */

  for (i = 0; i < bb_num; i++)
    {
      basic_block bb = bbs[i];
      rtx insn;
      rtx prev_insn;

      for (insn = bb->end; ; insn = prev_insn)
	{
	  prev_insn = PREV_INSN (insn);
	  if (need_fake_edge_p (insn))
	    {
	      edge e;
	      rtx split_at_insn = insn;

	      /* Don't split the block between a call and an insn that should
	         remain in the same block as the call.  */
	      if (GET_CODE (insn) == CALL_INSN)
		while (split_at_insn != bb->end
		       && keep_with_call_p (NEXT_INSN (split_at_insn)))
		  split_at_insn = NEXT_INSN (split_at_insn);

	      /* The handling above of the final block before the epilogue
	         should be enough to verify that there is no edge to the exit
		 block in CFG already.  Calling make_edge in such case would
		 cause us to mark that edge as fake and remove it later.  */

#ifdef ENABLE_CHECKING
	      if (split_at_insn == bb->end)
		for (e = bb->succ; e; e = e->succ_next)
		  if (e->dest == EXIT_BLOCK_PTR)
		    abort ();
#endif

	      /* Note that the following may create a new basic block
		 and renumber the existing basic blocks.  */
	      e = split_block (bb, split_at_insn);
	      if (e)
		blocks_split++;

	      make_edge (bb, EXIT_BLOCK_PTR, EDGE_FAKE);
	    }

	  if (insn == bb->head)
	    break;
	}
    }

  if (blocks_split)
    verify_flow_info ();

  free (bbs);
  return blocks_split;
}

/* Find unreachable blocks.  An unreachable block will have 0 in
   the reachable bit in block->flags.  A non-zero value indicates the
   block is reachable.  */

void
find_unreachable_blocks ()
{
  edge e;
  int i, n;
  basic_block *tos, *worklist;

  n = n_basic_blocks;
  tos = worklist = (basic_block *) xmalloc (sizeof (basic_block) * n);

  /* Clear all the reachability flags.  */

  for (i = 0; i < n; ++i)
    BASIC_BLOCK (i)->flags &= ~BB_REACHABLE;

  /* Add our starting points to the worklist.  Almost always there will
     be only one.  It isn't inconceivable that we might one day directly
     support Fortran alternate entry points.  */

  for (e = ENTRY_BLOCK_PTR->succ; e; e = e->succ_next)
    {
      *tos++ = e->dest;

      /* Mark the block reachable.  */
      e->dest->flags |= BB_REACHABLE;
    }

  /* Iterate: find everything reachable from what we've already seen.  */

  while (tos != worklist)
    {
      basic_block b = *--tos;

      for (e = b->succ; e; e = e->succ_next)
	if (!(e->dest->flags & BB_REACHABLE))
	  {
	    *tos++ = e->dest;
	    e->dest->flags |= BB_REACHABLE;
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
create_edge_list ()
{
  struct edge_list *elist;
  edge e;
  int num_edges;
  int x;
  int block_count;

  block_count = n_basic_blocks + 2;   /* Include the entry and exit blocks.  */

  num_edges = 0;

  /* Determine the number of edges in the flow graph by counting successor
     edges on each basic block.  */
  for (x = 0; x < n_basic_blocks; x++)
    {
      basic_block bb = BASIC_BLOCK (x);

      for (e = bb->succ; e; e = e->succ_next)
	num_edges++;
    }

  /* Don't forget successors of the entry block.  */
  for (e = ENTRY_BLOCK_PTR->succ; e; e = e->succ_next)
    num_edges++;

  elist = (struct edge_list *) xmalloc (sizeof (struct edge_list));
  elist->num_blocks = block_count;
  elist->num_edges = num_edges;
  elist->index_to_edge = (edge *) xmalloc (sizeof (edge) * num_edges);

  num_edges = 0;

  /* Follow successors of the entry block, and register these edges.  */
  for (e = ENTRY_BLOCK_PTR->succ; e; e = e->succ_next)
    elist->index_to_edge[num_edges++] = e;

  for (x = 0; x < n_basic_blocks; x++)
    {
      basic_block bb = BASIC_BLOCK (x);

      /* Follow all successors of blocks, and register these edges.  */
      for (e = bb->succ; e; e = e->succ_next)
	elist->index_to_edge[num_edges++] = e;
    }

  return elist;
}

/* This function free's memory associated with an edge list.  */

void
free_edge_list (elist)
     struct edge_list *elist;
{
  if (elist)
    {
      free (elist->index_to_edge);
      free (elist);
    }
}

/* This function provides debug output showing an edge list.  */

void
print_edge_list (f, elist)
     FILE *f;
     struct edge_list *elist;
{
  int x;

  fprintf (f, "Compressed edge list, %d BBs + entry & exit, and %d edges\n",
	   elist->num_blocks - 2, elist->num_edges);

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

void
verify_edge_list (f, elist)
     FILE *f;
     struct edge_list *elist;
{
  int x, pred, succ, index;
  edge e;

  for (x = 0; x < n_basic_blocks; x++)
    {
      basic_block bb = BASIC_BLOCK (x);

      for (e = bb->succ; e; e = e->succ_next)
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

  for (e = ENTRY_BLOCK_PTR->succ; e; e = e->succ_next)
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

  /* We've verified that all the edges are in the list, no lets make sure
     there are no spurious edges in the list.  */

  for (pred = 0; pred < n_basic_blocks; pred++)
    for (succ = 0; succ < n_basic_blocks; succ++)
      {
	basic_block p = BASIC_BLOCK (pred);
	basic_block s = BASIC_BLOCK (succ);
	int found_edge = 0;

	for (e = p->succ; e; e = e->succ_next)
	  if (e->dest == s)
	    {
	      found_edge = 1;
	      break;
	    }

	for (e = s->pred; e; e = e->pred_next)
	  if (e->src == p)
	    {
	      found_edge = 1;
	      break;
	    }

	if (EDGE_INDEX (elist, BASIC_BLOCK (pred), BASIC_BLOCK (succ))
	    == EDGE_INDEX_NO_EDGE && found_edge != 0)
	  fprintf (f, "*** Edge (%d, %d) appears to not have an index\n",
		   pred, succ);
	if (EDGE_INDEX (elist, BASIC_BLOCK (pred), BASIC_BLOCK (succ))
	    != EDGE_INDEX_NO_EDGE && found_edge == 0)
	  fprintf (f, "*** Edge (%d, %d) has index %d, but there is no edge\n",
		   pred, succ, EDGE_INDEX (elist, BASIC_BLOCK (pred),
					   BASIC_BLOCK (succ)));
      }

  for (succ = 0; succ < n_basic_blocks; succ++)
    {
      basic_block p = ENTRY_BLOCK_PTR;
      basic_block s = BASIC_BLOCK (succ);
      int found_edge = 0;

      for (e = p->succ; e; e = e->succ_next)
	if (e->dest == s)
	  {
	    found_edge = 1;
	    break;
	  }

      for (e = s->pred; e; e = e->pred_next)
	if (e->src == p)
	  {
	    found_edge = 1;
	    break;
	  }

      if (EDGE_INDEX (elist, ENTRY_BLOCK_PTR, BASIC_BLOCK (succ))
	  == EDGE_INDEX_NO_EDGE && found_edge != 0)
	fprintf (f, "*** Edge (entry, %d) appears to not have an index\n",
		 succ);
      if (EDGE_INDEX (elist, ENTRY_BLOCK_PTR, BASIC_BLOCK (succ))
	  != EDGE_INDEX_NO_EDGE && found_edge == 0)
	fprintf (f, "*** Edge (entry, %d) has index %d, but no edge exists\n",
		 succ, EDGE_INDEX (elist, ENTRY_BLOCK_PTR,
				   BASIC_BLOCK (succ)));
    }

  for (pred = 0; pred < n_basic_blocks; pred++)
    {
      basic_block p = BASIC_BLOCK (pred);
      basic_block s = EXIT_BLOCK_PTR;
      int found_edge = 0;

      for (e = p->succ; e; e = e->succ_next)
	if (e->dest == s)
	  {
	    found_edge = 1;
	    break;
	  }

      for (e = s->pred; e; e = e->pred_next)
	if (e->src == p)
	  {
	    found_edge = 1;
	    break;
	  }

      if (EDGE_INDEX (elist, BASIC_BLOCK (pred), EXIT_BLOCK_PTR)
	  == EDGE_INDEX_NO_EDGE && found_edge != 0)
	fprintf (f, "*** Edge (%d, exit) appears to not have an index\n",
		 pred);
      if (EDGE_INDEX (elist, BASIC_BLOCK (pred), EXIT_BLOCK_PTR)
	  != EDGE_INDEX_NO_EDGE && found_edge == 0)
	fprintf (f, "*** Edge (%d, exit) has index %d, but no edge exists\n",
		 pred, EDGE_INDEX (elist, BASIC_BLOCK (pred),
				   EXIT_BLOCK_PTR));
    }
}

/* This routine will determine what, if any, edge there is between
   a specified predecessor and successor.  */

int
find_edge_index (edge_list, pred, succ)
     struct edge_list *edge_list;
     basic_block pred, succ;
{
  int x;

  for (x = 0; x < NUM_EDGES (edge_list); x++)
    if (INDEX_EDGE_PRED_BB (edge_list, x) == pred
	&& INDEX_EDGE_SUCC_BB (edge_list, x) == succ)
      return x;

  return (EDGE_INDEX_NO_EDGE);
}

/* Dump the list of basic blocks in the bitmap NODES.  */

void
flow_nodes_print (str, nodes, file)
     const char *str;
     const sbitmap nodes;
     FILE *file;
{
  int node;

  if (! nodes)
    return;

  fprintf (file, "%s { ", str);
  EXECUTE_IF_SET_IN_SBITMAP (nodes, 0, node, {fprintf (file, "%d ", node);});
  fputs ("}\n", file);
}

/* Dump the list of edges in the array EDGE_LIST.  */

void
flow_edge_list_print (str, edge_list, num_edges, file)
     const char *str;
     const edge *edge_list;
     int num_edges;
     FILE *file;
{
  int i;

  if (! edge_list)
    return;

  fprintf (file, "%s { ", str);
  for (i = 0; i < num_edges; i++)
    fprintf (file, "%d->%d ", edge_list[i]->src->index,
	     edge_list[i]->dest->index);

  fputs ("}\n", file);
}


/* This routine will remove any fake successor edges for a basic block.
   When the edge is removed, it is also removed from whatever predecessor
   list it is in.  */

static void
remove_fake_successors (bb)
     basic_block bb;
{
  edge e;

  for (e = bb->succ; e;)
    {
      edge tmp = e;

      e = e->succ_next;
      if ((tmp->flags & EDGE_FAKE) == EDGE_FAKE)
	remove_edge (tmp);
    }
}

/* This routine will remove all fake edges from the flow graph.  If
   we remove all fake successors, it will automatically remove all
   fake predecessors.  */

void
remove_fake_edges ()
{
  int x;

  for (x = 0; x < n_basic_blocks; x++)
    remove_fake_successors (BASIC_BLOCK (x));

  /* We've handled all successors except the entry block's.  */
  remove_fake_successors (ENTRY_BLOCK_PTR);
}

/* This function will add a fake edge between any block which has no
   successors, and the exit block. Some data flow equations require these
   edges to exist.  */

void
add_noreturn_fake_exit_edges ()
{
  int x;

  for (x = 0; x < n_basic_blocks; x++)
    if (BASIC_BLOCK (x)->succ == NULL)
      make_single_succ_edge (BASIC_BLOCK (x), EXIT_BLOCK_PTR, EDGE_FAKE);
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
connect_infinite_loops_to_exit ()
{
  basic_block unvisited_block;
  struct depth_first_search_dsS dfs_ds;

  /* Perform depth-first search in the reverse graph to find nodes
     reachable from the exit block.  */
  flow_dfs_compute_reverse_init (&dfs_ds);
  flow_dfs_compute_reverse_add_bb (&dfs_ds, EXIT_BLOCK_PTR);

  /* Repeatedly add fake edges, updating the unreachable nodes.  */
  while (1)
    {
      unvisited_block = flow_dfs_compute_reverse_execute (&dfs_ds);
      if (!unvisited_block)
	break;

      make_edge (unvisited_block, EXIT_BLOCK_PTR, EDGE_FAKE);
      flow_dfs_compute_reverse_add_bb (&dfs_ds, unvisited_block);
    }

  flow_dfs_compute_reverse_finish (&dfs_ds);
  return;
}

/* Compute reverse top sort order */

void
flow_reverse_top_sort_order_compute (rts_order)
     int *rts_order;
{
  edge *stack;
  int sp;
  int postnum = 0;
  sbitmap visited;

  /* Allocate stack for back-tracking up CFG.  */
  stack = (edge *) xmalloc ((n_basic_blocks + 1) * sizeof (edge));
  sp = 0;

  /* Allocate bitmap to track nodes that have been visited.  */
  visited = sbitmap_alloc (n_basic_blocks);

  /* None of the nodes in the CFG have been visited yet.  */
  sbitmap_zero (visited);

  /* Push the first edge on to the stack.  */
  stack[sp++] = ENTRY_BLOCK_PTR->succ;

  while (sp)
    {
      edge e;
      basic_block src;
      basic_block dest;

      /* Look at the edge on the top of the stack.  */
      e = stack[sp - 1];
      src = e->src;
      dest = e->dest;

      /* Check if the edge destination has been visited yet.  */
      if (dest != EXIT_BLOCK_PTR && ! TEST_BIT (visited, dest->index))
	{
	  /* Mark that we have visited the destination.  */
	  SET_BIT (visited, dest->index);

	  if (dest->succ)
	    /* Since the DEST node has been visited for the first
	       time, check its successors.  */
	    stack[sp++] = dest->succ;
	  else
	    rts_order[postnum++] = dest->index;
	}
      else
	{
	  if (! e->succ_next && src != ENTRY_BLOCK_PTR)
	   rts_order[postnum++] = src->index;

	  if (e->succ_next)
	    stack[sp - 1] = e->succ_next;
	  else
	    sp--;
	}
    }

  free (stack);
  sbitmap_free (visited);
}

/* Compute the depth first search order and store in the array
  DFS_ORDER if non-zero, marking the nodes visited in VISITED.  If
  RC_ORDER is non-zero, return the reverse completion number for each
  node.  Returns the number of nodes visited.  A depth first search
  tries to get as far away from the starting point as quickly as
  possible.  */

int
flow_depth_first_order_compute (dfs_order, rc_order)
     int *dfs_order;
     int *rc_order;
{
  edge *stack;
  int sp;
  int dfsnum = 0;
  int rcnum = n_basic_blocks - 1;
  sbitmap visited;

  /* Allocate stack for back-tracking up CFG.  */
  stack = (edge *) xmalloc ((n_basic_blocks + 1) * sizeof (edge));
  sp = 0;

  /* Allocate bitmap to track nodes that have been visited.  */
  visited = sbitmap_alloc (n_basic_blocks);

  /* None of the nodes in the CFG have been visited yet.  */
  sbitmap_zero (visited);

  /* Push the first edge on to the stack.  */
  stack[sp++] = ENTRY_BLOCK_PTR->succ;

  while (sp)
    {
      edge e;
      basic_block src;
      basic_block dest;

      /* Look at the edge on the top of the stack.  */
      e = stack[sp - 1];
      src = e->src;
      dest = e->dest;

      /* Check if the edge destination has been visited yet.  */
      if (dest != EXIT_BLOCK_PTR && ! TEST_BIT (visited, dest->index))
	{
	  /* Mark that we have visited the destination.  */
	  SET_BIT (visited, dest->index);

	  if (dfs_order)
	    dfs_order[dfsnum] = dest->index;

	  dfsnum++;

	  if (dest->succ)
	    /* Since the DEST node has been visited for the first
	       time, check its successors.  */
	    stack[sp++] = dest->succ;
	  else if (rc_order)
	    /* There are no successors for the DEST node so assign
	       its reverse completion number.  */
	    rc_order[rcnum--] = dest->index;
	}
      else
	{
	  if (! e->succ_next && src != ENTRY_BLOCK_PTR
	      && rc_order)
	    /* There are no more successors for the SRC node
	       so assign its reverse completion number.  */
	    rc_order[rcnum--] = src->index;

	  if (e->succ_next)
	    stack[sp - 1] = e->succ_next;
	  else
	    sp--;
	}
    }

  free (stack);
  sbitmap_free (visited);

  /* The number of nodes visited should not be greater than
     n_basic_blocks.  */
  if (dfsnum > n_basic_blocks)
    abort ();

  /* There are some nodes left in the CFG that are unreachable.  */
  if (dfsnum < n_basic_blocks)
    abort ();

  return dfsnum;
}

struct dfst_node
{
    unsigned nnodes;
    struct dfst_node **node;
    struct dfst_node *up;
};

/* Compute a preorder transversal ordering such that a sub-tree which
   is the source of a cross edge appears before the sub-tree which is
   the destination of the cross edge.  This allows for easy detection
   of all the entry blocks for a loop.

   The ordering is compute by:

     1) Generating a depth first spanning tree.

     2) Walking the resulting tree from right to left.  */

void
flow_preorder_transversal_compute (pot_order)
     int *pot_order;
{
  edge e;
  edge *stack;
  int i;
  int max_successors;
  int sp;
  sbitmap visited;
  struct dfst_node *node;
  struct dfst_node *dfst;

  /* Allocate stack for back-tracking up CFG.  */
  stack = (edge *) xmalloc ((n_basic_blocks + 1) * sizeof (edge));
  sp = 0;

  /* Allocate the tree.  */
  dfst = (struct dfst_node *) xcalloc (n_basic_blocks,
				       sizeof (struct dfst_node));

  for (i = 0; i < n_basic_blocks; i++)
    {
      max_successors = 0;
      for (e = BASIC_BLOCK (i)->succ; e; e = e->succ_next)
	max_successors++;

      dfst[i].node
	= (max_successors
	   ? (struct dfst_node **) xcalloc (max_successors,
					    sizeof (struct dfst_node *))
	   : NULL);
    }

  /* Allocate bitmap to track nodes that have been visited.  */
  visited = sbitmap_alloc (n_basic_blocks);

  /* None of the nodes in the CFG have been visited yet.  */
  sbitmap_zero (visited);

  /* Push the first edge on to the stack.  */
  stack[sp++] = ENTRY_BLOCK_PTR->succ;

  while (sp)
    {
      basic_block src;
      basic_block dest;

      /* Look at the edge on the top of the stack.  */
      e = stack[sp - 1];
      src = e->src;
      dest = e->dest;

      /* Check if the edge destination has been visited yet.  */
      if (dest != EXIT_BLOCK_PTR && ! TEST_BIT (visited, dest->index))
	{
	  /* Mark that we have visited the destination.  */
	  SET_BIT (visited, dest->index);

	  /* Add the destination to the preorder tree.  */
	  if (src != ENTRY_BLOCK_PTR)
	    {
	      dfst[src->index].node[dfst[src->index].nnodes++]
		= &dfst[dest->index];
	      dfst[dest->index].up = &dfst[src->index];
	    }

	  if (dest->succ)
	    /* Since the DEST node has been visited for the first
	       time, check its successors.  */
	    stack[sp++] = dest->succ;
	}

      else if (e->succ_next)
	stack[sp - 1] = e->succ_next;
      else
	sp--;
    }

  free (stack);
  sbitmap_free (visited);

  /* Record the preorder transversal order by
     walking the tree from right to left.  */

  i = 0;
  node = &dfst[0];
  pot_order[i++] = 0;

  while (node)
    {
      if (node->nnodes)
	{
	  node = node->node[--node->nnodes];
	  pot_order[i++] = node - dfst;
	}
      else
	node = node->up;
    }

  /* Free the tree.  */

  for (i = 0; i < n_basic_blocks; i++)
    if (dfst[i].node)
      free (dfst[i].node);

  free (dfst);
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
   search context.  If INITIALIZE_STACK is non-zero, there is an
   element on the stack.  */

static void
flow_dfs_compute_reverse_init (data)
     depth_first_search_ds data;
{
  /* Allocate stack for back-tracking up CFG.  */
  data->stack = (basic_block *) xmalloc ((n_basic_blocks - (INVALID_BLOCK + 1))
					 * sizeof (basic_block));
  data->sp = 0;

  /* Allocate bitmap to track nodes that have been visited.  */
  data->visited_blocks = sbitmap_alloc (n_basic_blocks - (INVALID_BLOCK + 1));

  /* None of the nodes in the CFG have been visited yet.  */
  sbitmap_zero (data->visited_blocks);

  return;
}

/* Add the specified basic block to the top of the dfs data
   structures.  When the search continues, it will start at the
   block.  */

static void
flow_dfs_compute_reverse_add_bb (data, bb)
     depth_first_search_ds data;
     basic_block bb;
{
  data->stack[data->sp++] = bb;
  SET_BIT (data->visited_blocks, bb->index - (INVALID_BLOCK + 1));
}

/* Continue the depth-first search through the reverse graph starting with the
   block at the stack's top and ending when the stack is empty.  Visited nodes
   are marked.  Returns an unvisited basic block, or NULL if there is none
   available.  */

static basic_block
flow_dfs_compute_reverse_execute (data)
     depth_first_search_ds data;
{
  basic_block bb;
  edge e;
  int i;

  while (data->sp > 0)
    {
      bb = data->stack[--data->sp];

      /* Perform depth-first search on adjacent vertices.  */
      for (e = bb->pred; e; e = e->pred_next)
	if (!TEST_BIT (data->visited_blocks,
		       e->src->index - (INVALID_BLOCK + 1)))
	  flow_dfs_compute_reverse_add_bb (data, e->src);
    }

  /* Determine if there are unvisited basic blocks.  */
  for (i = n_basic_blocks - (INVALID_BLOCK + 1); --i >= 0; )
    if (!TEST_BIT (data->visited_blocks, i))
      return BASIC_BLOCK (i + (INVALID_BLOCK + 1));

  return NULL;
}

/* Destroy the data structures needed for depth-first search on the
   reverse graph.  */

static void
flow_dfs_compute_reverse_finish (data)
     depth_first_search_ds data;
{
  free (data->stack);
  sbitmap_free (data->visited_blocks);
}
