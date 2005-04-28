/* Generic partial redundancy elimination with lazy code motion support.
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005
   Free Software Foundation, Inc.

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

/* These routines are meant to be used by various optimization
   passes which can be modeled as lazy code motion problems.
   Including, but not limited to:

	* Traditional partial redundancy elimination.

	* Placement of caller/caller register save/restores.

	* Load/store motion.

	* Copy motion.

	* Conversion of flat register files to a stacked register
	model.

	* Dead load/store elimination.

  These routines accept as input:

	* Basic block information (number of blocks, lists of
	predecessors and successors).  Note the granularity
	does not need to be basic block, they could be statements
	or functions.

	* Bitmaps of local properties (computed, transparent and
	anticipatable expressions).

  The output of these routines is bitmap of redundant computations
  and a bitmap of optimal placement points.  */


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "flags.h"
#include "real.h"
#include "insn-config.h"
#include "recog.h"
#include "basic-block.h"
#include "output.h"
#include "tm_p.h"
#include "function.h"

/* We want target macros for the mode switching code to be able to refer
   to instruction attribute values.  */
#include "insn-attr.h"

/* Edge based LCM routines.  */
static void compute_antinout_edge (sbitmap *, sbitmap *, sbitmap *, sbitmap *);
static void compute_earliest (struct edge_list *, int, sbitmap *, sbitmap *,
			      sbitmap *, sbitmap *, sbitmap *);
static void compute_laterin (struct edge_list *, sbitmap *, sbitmap *,
			     sbitmap *, sbitmap *);
static void compute_insert_delete (struct edge_list *edge_list, sbitmap *,
				   sbitmap *, sbitmap *, sbitmap *, sbitmap *);

/* Edge based LCM routines on a reverse flowgraph.  */
static void compute_farthest (struct edge_list *, int, sbitmap *, sbitmap *,
			      sbitmap*, sbitmap *, sbitmap *);
static void compute_nearerout (struct edge_list *, sbitmap *, sbitmap *,
			       sbitmap *, sbitmap *);
static void compute_rev_insert_delete (struct edge_list *edge_list, sbitmap *,
				       sbitmap *, sbitmap *, sbitmap *,
				       sbitmap *);

/* Edge based lcm routines.  */

/* Compute expression anticipatability at entrance and exit of each block.
   This is done based on the flow graph, and not on the pred-succ lists.
   Other than that, its pretty much identical to compute_antinout.  */

static void
compute_antinout_edge (sbitmap *antloc, sbitmap *transp, sbitmap *antin,
		       sbitmap *antout)
{
  basic_block bb;
  edge e;
  basic_block *worklist, *qin, *qout, *qend;
  unsigned int qlen;
  edge_iterator ei;

  /* Allocate a worklist array/queue.  Entries are only added to the
     list if they were not already on the list.  So the size is
     bounded by the number of basic blocks.  */
  qin = qout = worklist = xmalloc (sizeof (basic_block) * n_basic_blocks);

  /* We want a maximal solution, so make an optimistic initialization of
     ANTIN.  */
  sbitmap_vector_ones (antin, last_basic_block);

  /* Put every block on the worklist; this is necessary because of the
     optimistic initialization of ANTIN above.  */
  FOR_EACH_BB_REVERSE (bb)
    {
      *qin++ = bb;
      bb->aux = bb;
    }

  qin = worklist;
  qend = &worklist[n_basic_blocks];
  qlen = n_basic_blocks;

  /* Mark blocks which are predecessors of the exit block so that we
     can easily identify them below.  */
  FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR->preds)
    e->src->aux = EXIT_BLOCK_PTR;

  /* Iterate until the worklist is empty.  */
  while (qlen)
    {
      /* Take the first entry off the worklist.  */
      bb = *qout++;
      qlen--;

      if (qout >= qend)
	qout = worklist;

      if (bb->aux == EXIT_BLOCK_PTR)
	/* Do not clear the aux field for blocks which are predecessors of
	   the EXIT block.  That way we never add then to the worklist
	   again.  */
	sbitmap_zero (antout[bb->index]);
      else
	{
	  /* Clear the aux field of this block so that it can be added to
	     the worklist again if necessary.  */
	  bb->aux = NULL;
	  sbitmap_intersection_of_succs (antout[bb->index], antin, bb->index);
	}

      if (sbitmap_a_or_b_and_c_cg (antin[bb->index], antloc[bb->index],
				   transp[bb->index], antout[bb->index]))
	/* If the in state of this block changed, then we need
	   to add the predecessors of this block to the worklist
	   if they are not already on the worklist.  */
	FOR_EACH_EDGE (e, ei, bb->preds)
	  if (!e->src->aux && e->src != ENTRY_BLOCK_PTR)
	    {
	      *qin++ = e->src;
	      e->src->aux = e;
	      qlen++;
	      if (qin >= qend)
		qin = worklist;
	    }
    }

  clear_aux_for_edges ();
  clear_aux_for_blocks ();
  free (worklist);
}

/* Compute the earliest vector for edge based lcm.  */

static void
compute_earliest (struct edge_list *edge_list, int n_exprs, sbitmap *antin,
		  sbitmap *antout, sbitmap *avout, sbitmap *kill,
		  sbitmap *earliest)
{
  sbitmap difference, temp_bitmap;
  int x, num_edges;
  basic_block pred, succ;

  num_edges = NUM_EDGES (edge_list);

  difference = sbitmap_alloc (n_exprs);
  temp_bitmap = sbitmap_alloc (n_exprs);

  for (x = 0; x < num_edges; x++)
    {
      pred = INDEX_EDGE_PRED_BB (edge_list, x);
      succ = INDEX_EDGE_SUCC_BB (edge_list, x);
      if (pred == ENTRY_BLOCK_PTR)
	sbitmap_copy (earliest[x], antin[succ->index]);
      else
	{
	  if (succ == EXIT_BLOCK_PTR)
	    sbitmap_zero (earliest[x]);
	  else
	    {
	      sbitmap_difference (difference, antin[succ->index],
				  avout[pred->index]);
	      sbitmap_not (temp_bitmap, antout[pred->index]);
	      sbitmap_a_and_b_or_c (earliest[x], difference,
				    kill[pred->index], temp_bitmap);
	    }
	}
    }

  sbitmap_free (temp_bitmap);
  sbitmap_free (difference);
}

/* later(p,s) is dependent on the calculation of laterin(p).
   laterin(p) is dependent on the calculation of later(p2,p).

     laterin(ENTRY) is defined as all 0's
     later(ENTRY, succs(ENTRY)) are defined using laterin(ENTRY)
     laterin(succs(ENTRY)) is defined by later(ENTRY, succs(ENTRY)).

   If we progress in this manner, starting with all basic blocks
   in the work list, anytime we change later(bb), we need to add
   succs(bb) to the worklist if they are not already on the worklist.

   Boundary conditions:

     We prime the worklist all the normal basic blocks.   The ENTRY block can
     never be added to the worklist since it is never the successor of any
     block.  We explicitly prevent the EXIT block from being added to the
     worklist.

     We optimistically initialize LATER.  That is the only time this routine
     will compute LATER for an edge out of the entry block since the entry
     block is never on the worklist.  Thus, LATERIN is neither used nor
     computed for the ENTRY block.

     Since the EXIT block is never added to the worklist, we will neither
     use nor compute LATERIN for the exit block.  Edges which reach the
     EXIT block are handled in the normal fashion inside the loop.  However,
     the insertion/deletion computation needs LATERIN(EXIT), so we have
     to compute it.  */

static void
compute_laterin (struct edge_list *edge_list, sbitmap *earliest,
		 sbitmap *antloc, sbitmap *later, sbitmap *laterin)
{
  int num_edges, i;
  edge e;
  basic_block *worklist, *qin, *qout, *qend, bb;
  unsigned int qlen;
  edge_iterator ei;

  num_edges = NUM_EDGES (edge_list);

  /* Allocate a worklist array/queue.  Entries are only added to the
     list if they were not already on the list.  So the size is
     bounded by the number of basic blocks.  */
  qin = qout = worklist
    = xmalloc (sizeof (basic_block) * (n_basic_blocks + 1));

  /* Initialize a mapping from each edge to its index.  */
  for (i = 0; i < num_edges; i++)
    INDEX_EDGE (edge_list, i)->aux = (void *) (size_t) i;

  /* We want a maximal solution, so initially consider LATER true for
     all edges.  This allows propagation through a loop since the incoming
     loop edge will have LATER set, so if all the other incoming edges
     to the loop are set, then LATERIN will be set for the head of the
     loop.

     If the optimistic setting of LATER on that edge was incorrect (for
     example the expression is ANTLOC in a block within the loop) then
     this algorithm will detect it when we process the block at the head
     of the optimistic edge.  That will requeue the affected blocks.  */
  sbitmap_vector_ones (later, num_edges);

  /* Note that even though we want an optimistic setting of LATER, we
     do not want to be overly optimistic.  Consider an outgoing edge from
     the entry block.  That edge should always have a LATER value the
     same as EARLIEST for that edge.  */
  FOR_EACH_EDGE (e, ei, ENTRY_BLOCK_PTR->succs)
    sbitmap_copy (later[(size_t) e->aux], earliest[(size_t) e->aux]);

  /* Add all the blocks to the worklist.  This prevents an early exit from
     the loop given our optimistic initialization of LATER above.  */
  FOR_EACH_BB (bb)
    {
      *qin++ = bb;
      bb->aux = bb;
    }

  /* Note that we do not use the last allocated element for our queue,
     as EXIT_BLOCK is never inserted into it. In fact the above allocation
     of n_basic_blocks + 1 elements is not necessary.  */
  qin = worklist;
  qend = &worklist[n_basic_blocks];
  qlen = n_basic_blocks;

  /* Iterate until the worklist is empty.  */
  while (qlen)
    {
      /* Take the first entry off the worklist.  */
      bb = *qout++;
      bb->aux = NULL;
      qlen--;
      if (qout >= qend)
	qout = worklist;

      /* Compute the intersection of LATERIN for each incoming edge to B.  */
      sbitmap_ones (laterin[bb->index]);
      FOR_EACH_EDGE (e, ei, bb->preds)
	sbitmap_a_and_b (laterin[bb->index], laterin[bb->index],
			 later[(size_t)e->aux]);

      /* Calculate LATER for all outgoing edges.  */
      FOR_EACH_EDGE (e, ei, bb->succs)
	if (sbitmap_union_of_diff_cg (later[(size_t) e->aux],
				      earliest[(size_t) e->aux],
				      laterin[e->src->index],
				      antloc[e->src->index])
	    /* If LATER for an outgoing edge was changed, then we need
	       to add the target of the outgoing edge to the worklist.  */
	    && e->dest != EXIT_BLOCK_PTR && e->dest->aux == 0)
	  {
	    *qin++ = e->dest;
	    e->dest->aux = e;
	    qlen++;
	    if (qin >= qend)
	      qin = worklist;
	  }
    }

  /* Computation of insertion and deletion points requires computing LATERIN
     for the EXIT block.  We allocated an extra entry in the LATERIN array
     for just this purpose.  */
  sbitmap_ones (laterin[last_basic_block]);
  FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR->preds)
    sbitmap_a_and_b (laterin[last_basic_block],
		     laterin[last_basic_block],
		     later[(size_t) e->aux]);

  clear_aux_for_edges ();
  free (worklist);
}

/* Compute the insertion and deletion points for edge based LCM.  */

static void
compute_insert_delete (struct edge_list *edge_list, sbitmap *antloc,
		       sbitmap *later, sbitmap *laterin, sbitmap *insert,
		       sbitmap *delete)
{
  int x;
  basic_block bb;

  FOR_EACH_BB (bb)
    sbitmap_difference (delete[bb->index], antloc[bb->index],
			laterin[bb->index]);

  for (x = 0; x < NUM_EDGES (edge_list); x++)
    {
      basic_block b = INDEX_EDGE_SUCC_BB (edge_list, x);

      if (b == EXIT_BLOCK_PTR)
	sbitmap_difference (insert[x], later[x], laterin[last_basic_block]);
      else
	sbitmap_difference (insert[x], later[x], laterin[b->index]);
    }
}

/* Given local properties TRANSP, ANTLOC, AVOUT, KILL return the insert and
   delete vectors for edge based LCM.  Returns an edgelist which is used to
   map the insert vector to what edge an expression should be inserted on.  */

struct edge_list *
pre_edge_lcm (FILE *file ATTRIBUTE_UNUSED, int n_exprs, sbitmap *transp,
	      sbitmap *avloc, sbitmap *antloc, sbitmap *kill,
	      sbitmap **insert, sbitmap **delete)
{
  sbitmap *antin, *antout, *earliest;
  sbitmap *avin, *avout;
  sbitmap *later, *laterin;
  struct edge_list *edge_list;
  int num_edges;

  edge_list = create_edge_list ();
  num_edges = NUM_EDGES (edge_list);

#ifdef LCM_DEBUG_INFO
  if (file)
    {
      fprintf (file, "Edge List:\n");
      verify_edge_list (file, edge_list);
      print_edge_list (file, edge_list);
      dump_sbitmap_vector (file, "transp", "", transp, last_basic_block);
      dump_sbitmap_vector (file, "antloc", "", antloc, last_basic_block);
      dump_sbitmap_vector (file, "avloc", "", avloc, last_basic_block);
      dump_sbitmap_vector (file, "kill", "", kill, last_basic_block);
    }
#endif

  /* Compute global availability.  */
  avin = sbitmap_vector_alloc (last_basic_block, n_exprs);
  avout = sbitmap_vector_alloc (last_basic_block, n_exprs);
  compute_available (avloc, kill, avout, avin);
  sbitmap_vector_free (avin);

  /* Compute global anticipatability.  */
  antin = sbitmap_vector_alloc (last_basic_block, n_exprs);
  antout = sbitmap_vector_alloc (last_basic_block, n_exprs);
  compute_antinout_edge (antloc, transp, antin, antout);

#ifdef LCM_DEBUG_INFO
  if (file)
    {
      dump_sbitmap_vector (file, "antin", "", antin, last_basic_block);
      dump_sbitmap_vector (file, "antout", "", antout, last_basic_block);
    }
#endif

  /* Compute earliestness.  */
  earliest = sbitmap_vector_alloc (num_edges, n_exprs);
  compute_earliest (edge_list, n_exprs, antin, antout, avout, kill, earliest);

#ifdef LCM_DEBUG_INFO
  if (file)
    dump_sbitmap_vector (file, "earliest", "", earliest, num_edges);
#endif

  sbitmap_vector_free (antout);
  sbitmap_vector_free (antin);
  sbitmap_vector_free (avout);

  later = sbitmap_vector_alloc (num_edges, n_exprs);

  /* Allocate an extra element for the exit block in the laterin vector.  */
  laterin = sbitmap_vector_alloc (last_basic_block + 1, n_exprs);
  compute_laterin (edge_list, earliest, antloc, later, laterin);

#ifdef LCM_DEBUG_INFO
  if (file)
    {
      dump_sbitmap_vector (file, "laterin", "", laterin, last_basic_block + 1);
      dump_sbitmap_vector (file, "later", "", later, num_edges);
    }
#endif

  sbitmap_vector_free (earliest);

  *insert = sbitmap_vector_alloc (num_edges, n_exprs);
  *delete = sbitmap_vector_alloc (last_basic_block, n_exprs);
  compute_insert_delete (edge_list, antloc, later, laterin, *insert, *delete);

  sbitmap_vector_free (laterin);
  sbitmap_vector_free (later);

#ifdef LCM_DEBUG_INFO
  if (file)
    {
      dump_sbitmap_vector (file, "pre_insert_map", "", *insert, num_edges);
      dump_sbitmap_vector (file, "pre_delete_map", "", *delete,
			   last_basic_block);
    }
#endif

  return edge_list;
}

/* Compute the AVIN and AVOUT vectors from the AVLOC and KILL vectors.
   Return the number of passes we performed to iterate to a solution.  */

void
compute_available (sbitmap *avloc, sbitmap *kill, sbitmap *avout,
		   sbitmap *avin)
{
  edge e;
  basic_block *worklist, *qin, *qout, *qend, bb;
  unsigned int qlen;
  edge_iterator ei;

  /* Allocate a worklist array/queue.  Entries are only added to the
     list if they were not already on the list.  So the size is
     bounded by the number of basic blocks.  */
  qin = qout = worklist = xmalloc (sizeof (basic_block) * n_basic_blocks);

  /* We want a maximal solution.  */
  sbitmap_vector_ones (avout, last_basic_block);

  /* Put every block on the worklist; this is necessary because of the
     optimistic initialization of AVOUT above.  */
  FOR_EACH_BB (bb)
    {
      *qin++ = bb;
      bb->aux = bb;
    }

  qin = worklist;
  qend = &worklist[n_basic_blocks];
  qlen = n_basic_blocks;

  /* Mark blocks which are successors of the entry block so that we
     can easily identify them below.  */
  FOR_EACH_EDGE (e, ei, ENTRY_BLOCK_PTR->succs)
    e->dest->aux = ENTRY_BLOCK_PTR;

  /* Iterate until the worklist is empty.  */
  while (qlen)
    {
      /* Take the first entry off the worklist.  */
      bb = *qout++;
      qlen--;

      if (qout >= qend)
	qout = worklist;

      /* If one of the predecessor blocks is the ENTRY block, then the
	 intersection of avouts is the null set.  We can identify such blocks
	 by the special value in the AUX field in the block structure.  */
      if (bb->aux == ENTRY_BLOCK_PTR)
	/* Do not clear the aux field for blocks which are successors of the
	   ENTRY block.  That way we never add then to the worklist again.  */
	sbitmap_zero (avin[bb->index]);
      else
	{
	  /* Clear the aux field of this block so that it can be added to
	     the worklist again if necessary.  */
	  bb->aux = NULL;
	  sbitmap_intersection_of_preds (avin[bb->index], avout, bb->index);
	}

      if (sbitmap_union_of_diff_cg (avout[bb->index], avloc[bb->index],
				    avin[bb->index], kill[bb->index]))
	/* If the out state of this block changed, then we need
	   to add the successors of this block to the worklist
	   if they are not already on the worklist.  */
	FOR_EACH_EDGE (e, ei, bb->succs)
	  if (!e->dest->aux && e->dest != EXIT_BLOCK_PTR)
	    {
	      *qin++ = e->dest;
	      e->dest->aux = e;
	      qlen++;

	      if (qin >= qend)
		qin = worklist;
	    }
    }

  clear_aux_for_edges ();
  clear_aux_for_blocks ();
  free (worklist);
}

/* Compute the farthest vector for edge based lcm.  */

static void
compute_farthest (struct edge_list *edge_list, int n_exprs,
		  sbitmap *st_avout, sbitmap *st_avin, sbitmap *st_antin,
		  sbitmap *kill, sbitmap *farthest)
{
  sbitmap difference, temp_bitmap;
  int x, num_edges;
  basic_block pred, succ;

  num_edges = NUM_EDGES (edge_list);

  difference = sbitmap_alloc (n_exprs);
  temp_bitmap = sbitmap_alloc (n_exprs);

  for (x = 0; x < num_edges; x++)
    {
      pred = INDEX_EDGE_PRED_BB (edge_list, x);
      succ = INDEX_EDGE_SUCC_BB (edge_list, x);
      if (succ == EXIT_BLOCK_PTR)
	sbitmap_copy (farthest[x], st_avout[pred->index]);
      else
	{
	  if (pred == ENTRY_BLOCK_PTR)
	    sbitmap_zero (farthest[x]);
	  else
	    {
	      sbitmap_difference (difference, st_avout[pred->index],
				  st_antin[succ->index]);
	      sbitmap_not (temp_bitmap, st_avin[succ->index]);
	      sbitmap_a_and_b_or_c (farthest[x], difference,
				    kill[succ->index], temp_bitmap);
	    }
	}
    }

  sbitmap_free (temp_bitmap);
  sbitmap_free (difference);
}

/* Compute nearer and nearerout vectors for edge based lcm.

   This is the mirror of compute_laterin, additional comments on the
   implementation can be found before compute_laterin.  */

static void
compute_nearerout (struct edge_list *edge_list, sbitmap *farthest,
		   sbitmap *st_avloc, sbitmap *nearer, sbitmap *nearerout)
{
  int num_edges, i;
  edge e;
  basic_block *worklist, *tos, bb;
  edge_iterator ei;

  num_edges = NUM_EDGES (edge_list);

  /* Allocate a worklist array/queue.  Entries are only added to the
     list if they were not already on the list.  So the size is
     bounded by the number of basic blocks.  */
  tos = worklist = xmalloc (sizeof (basic_block) * (n_basic_blocks + 1));

  /* Initialize NEARER for each edge and build a mapping from an edge to
     its index.  */
  for (i = 0; i < num_edges; i++)
    INDEX_EDGE (edge_list, i)->aux = (void *) (size_t) i;

  /* We want a maximal solution.  */
  sbitmap_vector_ones (nearer, num_edges);

  /* Note that even though we want an optimistic setting of NEARER, we
     do not want to be overly optimistic.  Consider an incoming edge to
     the exit block.  That edge should always have a NEARER value the
     same as FARTHEST for that edge.  */
  FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR->preds)
    sbitmap_copy (nearer[(size_t)e->aux], farthest[(size_t)e->aux]);

  /* Add all the blocks to the worklist.  This prevents an early exit
     from the loop given our optimistic initialization of NEARER.  */
  FOR_EACH_BB (bb)
    {
      *tos++ = bb;
      bb->aux = bb;
    }

  /* Iterate until the worklist is empty.  */
  while (tos != worklist)
    {
      /* Take the first entry off the worklist.  */
      bb = *--tos;
      bb->aux = NULL;

      /* Compute the intersection of NEARER for each outgoing edge from B.  */
      sbitmap_ones (nearerout[bb->index]);
      FOR_EACH_EDGE (e, ei, bb->succs)
	sbitmap_a_and_b (nearerout[bb->index], nearerout[bb->index],
			 nearer[(size_t) e->aux]);

      /* Calculate NEARER for all incoming edges.  */
      FOR_EACH_EDGE (e, ei, bb->preds)
	if (sbitmap_union_of_diff_cg (nearer[(size_t) e->aux],
				      farthest[(size_t) e->aux],
				      nearerout[e->dest->index],
				      st_avloc[e->dest->index])
	    /* If NEARER for an incoming edge was changed, then we need
	       to add the source of the incoming edge to the worklist.  */
	    && e->src != ENTRY_BLOCK_PTR && e->src->aux == 0)
	  {
	    *tos++ = e->src;
	    e->src->aux = e;
	  }
    }

  /* Computation of insertion and deletion points requires computing NEAREROUT
     for the ENTRY block.  We allocated an extra entry in the NEAREROUT array
     for just this purpose.  */
  sbitmap_ones (nearerout[last_basic_block]);
  FOR_EACH_EDGE (e, ei, ENTRY_BLOCK_PTR->succs)
    sbitmap_a_and_b (nearerout[last_basic_block],
		     nearerout[last_basic_block],
		     nearer[(size_t) e->aux]);

  clear_aux_for_edges ();
  free (tos);
}

/* Compute the insertion and deletion points for edge based LCM.  */

static void
compute_rev_insert_delete (struct edge_list *edge_list, sbitmap *st_avloc,
			   sbitmap *nearer, sbitmap *nearerout,
			   sbitmap *insert, sbitmap *delete)
{
  int x;
  basic_block bb;

  FOR_EACH_BB (bb)
    sbitmap_difference (delete[bb->index], st_avloc[bb->index],
			nearerout[bb->index]);

  for (x = 0; x < NUM_EDGES (edge_list); x++)
    {
      basic_block b = INDEX_EDGE_PRED_BB (edge_list, x);
      if (b == ENTRY_BLOCK_PTR)
	sbitmap_difference (insert[x], nearer[x], nearerout[last_basic_block]);
      else
	sbitmap_difference (insert[x], nearer[x], nearerout[b->index]);
    }
}

/* Given local properties TRANSP, ST_AVLOC, ST_ANTLOC, KILL return the
   insert and delete vectors for edge based reverse LCM.  Returns an
   edgelist which is used to map the insert vector to what edge
   an expression should be inserted on.  */

struct edge_list *
pre_edge_rev_lcm (FILE *file ATTRIBUTE_UNUSED, int n_exprs, sbitmap *transp,
		  sbitmap *st_avloc, sbitmap *st_antloc, sbitmap *kill,
		  sbitmap **insert, sbitmap **delete)
{
  sbitmap *st_antin, *st_antout;
  sbitmap *st_avout, *st_avin, *farthest;
  sbitmap *nearer, *nearerout;
  struct edge_list *edge_list;
  int num_edges;

  edge_list = create_edge_list ();
  num_edges = NUM_EDGES (edge_list);

  st_antin = sbitmap_vector_alloc (last_basic_block, n_exprs);
  st_antout = sbitmap_vector_alloc (last_basic_block, n_exprs);
  sbitmap_vector_zero (st_antin, last_basic_block);
  sbitmap_vector_zero (st_antout, last_basic_block);
  compute_antinout_edge (st_antloc, transp, st_antin, st_antout);

  /* Compute global anticipatability.  */
  st_avout = sbitmap_vector_alloc (last_basic_block, n_exprs);
  st_avin = sbitmap_vector_alloc (last_basic_block, n_exprs);
  compute_available (st_avloc, kill, st_avout, st_avin);

#ifdef LCM_DEBUG_INFO
  if (file)
    {
      fprintf (file, "Edge List:\n");
      verify_edge_list (file, edge_list);
      print_edge_list (file, edge_list);
      dump_sbitmap_vector (file, "transp", "", transp, last_basic_block);
      dump_sbitmap_vector (file, "st_avloc", "", st_avloc, last_basic_block);
      dump_sbitmap_vector (file, "st_antloc", "", st_antloc, last_basic_block);
      dump_sbitmap_vector (file, "st_antin", "", st_antin, last_basic_block);
      dump_sbitmap_vector (file, "st_antout", "", st_antout, last_basic_block);
      dump_sbitmap_vector (file, "st_kill", "", kill, last_basic_block);
    }
#endif

#ifdef LCM_DEBUG_INFO
  if (file)
    {
      dump_sbitmap_vector (file, "st_avout", "", st_avout, last_basic_block);
      dump_sbitmap_vector (file, "st_avin", "", st_avin, last_basic_block);
    }
#endif

  /* Compute farthestness.  */
  farthest = sbitmap_vector_alloc (num_edges, n_exprs);
  compute_farthest (edge_list, n_exprs, st_avout, st_avin, st_antin,
		    kill, farthest);

#ifdef LCM_DEBUG_INFO
  if (file)
    dump_sbitmap_vector (file, "farthest", "", farthest, num_edges);
#endif

  sbitmap_vector_free (st_antin);
  sbitmap_vector_free (st_antout);

  sbitmap_vector_free (st_avin);
  sbitmap_vector_free (st_avout);

  nearer = sbitmap_vector_alloc (num_edges, n_exprs);

  /* Allocate an extra element for the entry block.  */
  nearerout = sbitmap_vector_alloc (last_basic_block + 1, n_exprs);
  compute_nearerout (edge_list, farthest, st_avloc, nearer, nearerout);

#ifdef LCM_DEBUG_INFO
  if (file)
    {
      dump_sbitmap_vector (file, "nearerout", "", nearerout,
			   last_basic_block + 1);
      dump_sbitmap_vector (file, "nearer", "", nearer, num_edges);
    }
#endif

  sbitmap_vector_free (farthest);

  *insert = sbitmap_vector_alloc (num_edges, n_exprs);
  *delete = sbitmap_vector_alloc (last_basic_block, n_exprs);
  compute_rev_insert_delete (edge_list, st_avloc, nearer, nearerout,
			     *insert, *delete);

  sbitmap_vector_free (nearerout);
  sbitmap_vector_free (nearer);

#ifdef LCM_DEBUG_INFO
  if (file)
    {
      dump_sbitmap_vector (file, "pre_insert_map", "", *insert, num_edges);
      dump_sbitmap_vector (file, "pre_delete_map", "", *delete,
			   last_basic_block);
    }
#endif
  return edge_list;
}

/* Mode switching:

   The algorithm for setting the modes consists of scanning the insn list
   and finding all the insns which require a specific mode.  Each insn gets
   a unique struct seginfo element.  These structures are inserted into a list
   for each basic block.  For each entity, there is an array of bb_info over
   the flow graph basic blocks (local var 'bb_info'), and contains a list
   of all insns within that basic block, in the order they are encountered.

   For each entity, any basic block WITHOUT any insns requiring a specific
   mode are given a single entry, without a mode.  (Each basic block
   in the flow graph must have at least one entry in the segment table.)

   The LCM algorithm is then run over the flow graph to determine where to
   place the sets to the highest-priority value in respect of first the first
   insn in any one block.  Any adjustments required to the transparency
   vectors are made, then the next iteration starts for the next-lower
   priority mode, till for each entity all modes are exhausted.

   More details are located in the code for optimize_mode_switching().  */

/* This structure contains the information for each insn which requires
   either single or double mode to be set.
   MODE is the mode this insn must be executed in.
   INSN_PTR is the insn to be executed (may be the note that marks the
   beginning of a basic block).
   BBNUM is the flow graph basic block this insn occurs in.
   NEXT is the next insn in the same basic block.  */
struct seginfo
{
  int mode;
  rtx insn_ptr;
  int bbnum;
  struct seginfo *next;
  HARD_REG_SET regs_live;
};

struct bb_info
{
  struct seginfo *seginfo;
  int computing;
};

/* These bitmaps are used for the LCM algorithm.  */

#ifdef OPTIMIZE_MODE_SWITCHING
static sbitmap *antic;
static sbitmap *transp;
static sbitmap *comp;

static struct seginfo * new_seginfo (int, rtx, int, HARD_REG_SET);
static void add_seginfo (struct bb_info *, struct seginfo *);
static void reg_dies (rtx, HARD_REG_SET);
static void reg_becomes_live (rtx, rtx, void *);
static void make_preds_opaque (basic_block, int);
#endif

#ifdef OPTIMIZE_MODE_SWITCHING

/* This function will allocate a new BBINFO structure, initialized
   with the MODE, INSN, and basic block BB parameters.  */

static struct seginfo *
new_seginfo (int mode, rtx insn, int bb, HARD_REG_SET regs_live)
{
  struct seginfo *ptr;
  ptr = xmalloc (sizeof (struct seginfo));
  ptr->mode = mode;
  ptr->insn_ptr = insn;
  ptr->bbnum = bb;
  ptr->next = NULL;
  COPY_HARD_REG_SET (ptr->regs_live, regs_live);
  return ptr;
}

/* Add a seginfo element to the end of a list.
   HEAD is a pointer to the list beginning.
   INFO is the structure to be linked in.  */

static void
add_seginfo (struct bb_info *head, struct seginfo *info)
{
  struct seginfo *ptr;

  if (head->seginfo == NULL)
    head->seginfo = info;
  else
    {
      ptr = head->seginfo;
      while (ptr->next != NULL)
	ptr = ptr->next;
      ptr->next = info;
    }
}

/* Make all predecessors of basic block B opaque, recursively, till we hit
   some that are already non-transparent, or an edge where aux is set; that
   denotes that a mode set is to be done on that edge.
   J is the bit number in the bitmaps that corresponds to the entity that
   we are currently handling mode-switching for.  */

static void
make_preds_opaque (basic_block b, int j)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, b->preds)
    {
      basic_block pb = e->src;

      if (e->aux || ! TEST_BIT (transp[pb->index], j))
	continue;

      RESET_BIT (transp[pb->index], j);
      make_preds_opaque (pb, j);
    }
}

/* Record in LIVE that register REG died.  */

static void
reg_dies (rtx reg, HARD_REG_SET live)
{
  int regno, nregs;

  if (!REG_P (reg))
    return;

  regno = REGNO (reg);
  if (regno < FIRST_PSEUDO_REGISTER)
    for (nregs = hard_regno_nregs[regno][GET_MODE (reg)] - 1; nregs >= 0;
	 nregs--)
      CLEAR_HARD_REG_BIT (live, regno + nregs);
}

/* Record in LIVE that register REG became live.
   This is called via note_stores.  */

static void
reg_becomes_live (rtx reg, rtx setter ATTRIBUTE_UNUSED, void *live)
{
  int regno, nregs;

  if (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);

  if (!REG_P (reg))
    return;

  regno = REGNO (reg);
  if (regno < FIRST_PSEUDO_REGISTER)
    for (nregs = hard_regno_nregs[regno][GET_MODE (reg)] - 1; nregs >= 0;
	 nregs--)
      SET_HARD_REG_BIT (* (HARD_REG_SET *) live, regno + nregs);
}

/* Make sure if MODE_ENTRY is defined the MODE_EXIT is defined
   and vice versa.  */
#if defined (MODE_ENTRY) != defined (MODE_EXIT)
 #error "Both MODE_ENTRY and MODE_EXIT must be defined"
#endif

#if defined (MODE_ENTRY) && defined (MODE_EXIT)
/* Split the fallthrough edge to the exit block, so that we can note
   that there NORMAL_MODE is required.  Return the new block if it's
   inserted before the exit block.  Otherwise return null.  */

static basic_block
create_pre_exit (int n_entities, int *entity_map, const int *num_modes)
{
  edge eg;
  edge_iterator ei;
  basic_block pre_exit;

  /* The only non-call predecessor at this stage is a block with a
     fallthrough edge; there can be at most one, but there could be
     none at all, e.g. when exit is called.  */
  pre_exit = 0;
  FOR_EACH_EDGE (eg, ei, EXIT_BLOCK_PTR->preds)
    if (eg->flags & EDGE_FALLTHRU)
      {
	basic_block src_bb = eg->src;
	regset live_at_end = src_bb->global_live_at_end;
	rtx last_insn, ret_reg;

	gcc_assert (!pre_exit);
	/* If this function returns a value at the end, we have to
	   insert the final mode switch before the return value copy
	   to its hard register.  */
	if (EDGE_COUNT (EXIT_BLOCK_PTR->preds) == 1
	    && NONJUMP_INSN_P ((last_insn = BB_END (src_bb)))
	    && GET_CODE (PATTERN (last_insn)) == USE
	    && GET_CODE ((ret_reg = XEXP (PATTERN (last_insn), 0))) == REG)
	  {
	    int ret_start = REGNO (ret_reg);
	    int nregs = hard_regno_nregs[ret_start][GET_MODE (ret_reg)];
	    int ret_end = ret_start + nregs;
	    int short_block = 0;
	    int maybe_builtin_apply = 0;
	    int forced_late_switch = 0;
	    rtx before_return_copy;

	    do
	      {
		rtx return_copy = PREV_INSN (last_insn);
		rtx return_copy_pat, copy_reg;
		int copy_start, copy_num;
		int j;

		if (INSN_P (return_copy))
		  {
		    if (GET_CODE (PATTERN (return_copy)) == USE
			&& GET_CODE (XEXP (PATTERN (return_copy), 0)) == REG
			&& (FUNCTION_VALUE_REGNO_P
			    (REGNO (XEXP (PATTERN (return_copy), 0)))))
		      {
			maybe_builtin_apply = 1;
			last_insn = return_copy;
			continue;
		      }
		    /* If the return register is not (in its entirety)
		       likely spilled, the return copy might be
		       partially or completely optimized away.  */
		    return_copy_pat = single_set (return_copy);
		    if (!return_copy_pat)
		      {
			return_copy_pat = PATTERN (return_copy);
			if (GET_CODE (return_copy_pat) != CLOBBER)
			  break;
		      }
		    copy_reg = SET_DEST (return_copy_pat);
		    if (GET_CODE (copy_reg) == REG)
		      copy_start = REGNO (copy_reg);
		    else if (GET_CODE (copy_reg) == SUBREG
			     && GET_CODE (SUBREG_REG (copy_reg)) == REG)
		      copy_start = REGNO (SUBREG_REG (copy_reg));
		    else
		      break;
		    if (copy_start >= FIRST_PSEUDO_REGISTER)
		      break;
		    copy_num
		      = hard_regno_nregs[copy_start][GET_MODE (copy_reg)];

		    /* If the return register is not likely spilled, - as is
		       the case for floating point on SH4 - then it might
		       be set by an arithmetic operation that needs a
		       different mode than the exit block.  */
		    for (j = n_entities - 1; j >= 0; j--)
		      {
			int e = entity_map[j];
			int mode = MODE_NEEDED (e, return_copy);

			if (mode != num_modes[e] && mode != MODE_EXIT (e))
			  break;
		      }
		    if (j >= 0)
		      {
			/* For the SH4, floating point loads depend on fpscr,
			   thus we might need to put the final mode switch
			   after the return value copy.  That is still OK,
			   because a floating point return value does not
			   conflict with address reloads.  */
			if (copy_start >= ret_start
			    && copy_start + copy_num <= ret_end
			    && OBJECT_P (SET_SRC (return_copy_pat)))
			  forced_late_switch = 1;
			break;
		      }

		    if (copy_start >= ret_start
			&& copy_start + copy_num <= ret_end)
		      nregs -= copy_num;
		    else if (!maybe_builtin_apply
			     || !FUNCTION_VALUE_REGNO_P (copy_start))
		      break;
		    last_insn = return_copy;
		  }
		/* ??? Exception handling can lead to the return value
		   copy being already separated from the return value use,
		   as in  unwind-dw2.c .
		   Similarly, conditionally returning without a value,
		   and conditionally using builtin_return can lead to an
		   isolated use.  */
		if (return_copy == BB_HEAD (src_bb))
		  {
		    short_block = 1;
		    break;
		  }
		last_insn = return_copy;
	      }
	    while (nregs);
	    
	    /* If we didn't see a full return value copy, verify that there
	       is a plausible reason for this.  If some, but not all of the
	       return register is likely spilled, we can expect that there
	       is a copy for the likely spilled part.  */
	    gcc_assert (!nregs
			|| forced_late_switch
			|| short_block
			|| !(CLASS_LIKELY_SPILLED_P
			     (REGNO_REG_CLASS (ret_start)))
			|| (nregs
			    != hard_regno_nregs[ret_start][GET_MODE (ret_reg)])
			/* For multi-hard-register floating point
		   	   values, sometimes the likely-spilled part
		   	   is ordinarily copied first, then the other
		   	   part is set with an arithmetic operation.
		   	   This doesn't actually cause reload
		   	   failures, so let it pass.  */
			|| (GET_MODE_CLASS (GET_MODE (ret_reg)) != MODE_INT
			    && nregs != 1));
	    
	    if (INSN_P (last_insn))
	      {
		before_return_copy
		  = emit_note_before (NOTE_INSN_DELETED, last_insn);
		/* Instructions preceding LAST_INSN in the same block might
		   require a different mode than MODE_EXIT, so if we might
		   have such instructions, keep them in a separate block
		   from pre_exit.  */
		if (last_insn != BB_HEAD (src_bb))
		  src_bb = split_block (src_bb,
					PREV_INSN (before_return_copy))->dest;
	      }
	    else
	      before_return_copy = last_insn;
	    pre_exit = split_block (src_bb, before_return_copy)->src;
	  }
	else
	  {
	    pre_exit = split_edge (eg);
	    COPY_REG_SET (pre_exit->global_live_at_start, live_at_end);
	    COPY_REG_SET (pre_exit->global_live_at_end, live_at_end);
	  }
      }

  return pre_exit;
}
#endif

/* Find all insns that need a particular mode setting, and insert the
   necessary mode switches.  Return true if we did work.  */

int
optimize_mode_switching (FILE *file)
{
  rtx insn;
  int e;
  basic_block bb;
  int need_commit = 0;
  sbitmap *kill;
  struct edge_list *edge_list;
  static const int num_modes[] = NUM_MODES_FOR_MODE_SWITCHING;
#define N_ENTITIES ARRAY_SIZE (num_modes)
  int entity_map[N_ENTITIES];
  struct bb_info *bb_info[N_ENTITIES];
  int i, j;
  int n_entities;
  int max_num_modes = 0;
  bool emited = false;
  basic_block post_entry ATTRIBUTE_UNUSED, pre_exit ATTRIBUTE_UNUSED;

  clear_bb_flags ();

  for (e = N_ENTITIES - 1, n_entities = 0; e >= 0; e--)
    if (OPTIMIZE_MODE_SWITCHING (e))
      {
	int entry_exit_extra = 0;

	/* Create the list of segments within each basic block.
	   If NORMAL_MODE is defined, allow for two extra
	   blocks split from the entry and exit block.  */
#if defined (MODE_ENTRY) && defined (MODE_EXIT)
	entry_exit_extra = 3;
#endif
	bb_info[n_entities]
	  = xcalloc (last_basic_block + entry_exit_extra, sizeof **bb_info);
	entity_map[n_entities++] = e;
	if (num_modes[e] > max_num_modes)
	  max_num_modes = num_modes[e];
      }

  if (! n_entities)
    return 0;

#if defined (MODE_ENTRY) && defined (MODE_EXIT)
  /* Split the edge from the entry block, so that we can note that
     there NORMAL_MODE is supplied.  */
  post_entry = split_edge (single_succ_edge (ENTRY_BLOCK_PTR));
  pre_exit = create_pre_exit (n_entities, entity_map, num_modes);
#endif

  /* Create the bitmap vectors.  */

  antic = sbitmap_vector_alloc (last_basic_block, n_entities);
  transp = sbitmap_vector_alloc (last_basic_block, n_entities);
  comp = sbitmap_vector_alloc (last_basic_block, n_entities);

  sbitmap_vector_ones (transp, last_basic_block);

  for (j = n_entities - 1; j >= 0; j--)
    {
      int e = entity_map[j];
      int no_mode = num_modes[e];
      struct bb_info *info = bb_info[j];

      /* Determine what the first use (if any) need for a mode of entity E is.
	 This will be the mode that is anticipatable for this block.
	 Also compute the initial transparency settings.  */
      FOR_EACH_BB (bb)
	{
	  struct seginfo *ptr;
	  int last_mode = no_mode;
	  HARD_REG_SET live_now;

	  REG_SET_TO_HARD_REG_SET (live_now,
				   bb->global_live_at_start);
	  for (insn = BB_HEAD (bb);
	       insn != NULL && insn != NEXT_INSN (BB_END (bb));
	       insn = NEXT_INSN (insn))
	    {
	      if (INSN_P (insn))
		{
		  int mode = MODE_NEEDED (e, insn);
		  rtx link;

		  if (mode != no_mode && mode != last_mode)
		    {
		      last_mode = mode;
		      ptr = new_seginfo (mode, insn, bb->index, live_now);
		      add_seginfo (info + bb->index, ptr);
		      RESET_BIT (transp[bb->index], j);
		    }
#ifdef MODE_AFTER
		  last_mode = MODE_AFTER (last_mode, insn);
#endif
		  /* Update LIVE_NOW.  */
		  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
		    if (REG_NOTE_KIND (link) == REG_DEAD)
		      reg_dies (XEXP (link, 0), live_now);

		  note_stores (PATTERN (insn), reg_becomes_live, &live_now);
		  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
		    if (REG_NOTE_KIND (link) == REG_UNUSED)
		      reg_dies (XEXP (link, 0), live_now);
		}
	    }

	  info[bb->index].computing = last_mode;
	  /* Check for blocks without ANY mode requirements.  */
	  if (last_mode == no_mode)
	    {
	      ptr = new_seginfo (no_mode, BB_END (bb), bb->index, live_now);
	      add_seginfo (info + bb->index, ptr);
	    }
	}
#if defined (MODE_ENTRY) && defined (MODE_EXIT)
      {
	int mode = MODE_ENTRY (e);

	if (mode != no_mode)
	  {
	    bb = post_entry;

	    /* By always making this nontransparent, we save
	       an extra check in make_preds_opaque.  We also
	       need this to avoid confusing pre_edge_lcm when
	       antic is cleared but transp and comp are set.  */
	    RESET_BIT (transp[bb->index], j);

	    /* Insert a fake computing definition of MODE into entry
	       blocks which compute no mode. This represents the mode on
	       entry.  */
	    info[bb->index].computing = mode;

	    if (pre_exit)
	      info[pre_exit->index].seginfo->mode = MODE_EXIT (e);
	  }
      }
#endif /* NORMAL_MODE */
    }

  kill = sbitmap_vector_alloc (last_basic_block, n_entities);
  for (i = 0; i < max_num_modes; i++)
    {
      int current_mode[N_ENTITIES];
      sbitmap *delete;
      sbitmap *insert;

      /* Set the anticipatable and computing arrays.  */
      sbitmap_vector_zero (antic, last_basic_block);
      sbitmap_vector_zero (comp, last_basic_block);
      for (j = n_entities - 1; j >= 0; j--)
	{
	  int m = current_mode[j] = MODE_PRIORITY_TO_MODE (entity_map[j], i);
	  struct bb_info *info = bb_info[j];

	  FOR_EACH_BB (bb)
	    {
	      if (info[bb->index].seginfo->mode == m)
		SET_BIT (antic[bb->index], j);

	      if (info[bb->index].computing == m)
		SET_BIT (comp[bb->index], j);
	    }
	}

      /* Calculate the optimal locations for the
	 placement mode switches to modes with priority I.  */

      FOR_EACH_BB (bb)
	sbitmap_not (kill[bb->index], transp[bb->index]);
      edge_list = pre_edge_lcm (file, 1, transp, comp, antic,
				kill, &insert, &delete);

      for (j = n_entities - 1; j >= 0; j--)
	{
	  /* Insert all mode sets that have been inserted by lcm.  */
	  int no_mode = num_modes[entity_map[j]];

	  /* Wherever we have moved a mode setting upwards in the flow graph,
	     the blocks between the new setting site and the now redundant
	     computation ceases to be transparent for any lower-priority
	     mode of the same entity.  First set the aux field of each
	     insertion site edge non-transparent, then propagate the new
	     non-transparency from the redundant computation upwards till
	     we hit an insertion site or an already non-transparent block.  */
	  for (e = NUM_EDGES (edge_list) - 1; e >= 0; e--)
	    {
	      edge eg = INDEX_EDGE (edge_list, e);
	      int mode;
	      basic_block src_bb;
	      HARD_REG_SET live_at_edge;
	      rtx mode_set;

	      eg->aux = 0;

	      if (! TEST_BIT (insert[e], j))
		continue;

	      eg->aux = (void *)1;

	      mode = current_mode[j];
	      src_bb = eg->src;

	      REG_SET_TO_HARD_REG_SET (live_at_edge,
				       src_bb->global_live_at_end);

	      start_sequence ();
	      EMIT_MODE_SET (entity_map[j], mode, live_at_edge);
	      mode_set = get_insns ();
	      end_sequence ();

	      /* Do not bother to insert empty sequence.  */
	      if (mode_set == NULL_RTX)
		continue;

	      /* If this is an abnormal edge, we'll insert at the end
		 of the previous block.  */
	      if (eg->flags & EDGE_ABNORMAL)
		{
		  emited = true;
		  if (JUMP_P (BB_END (src_bb)))
		    emit_insn_before (mode_set, BB_END (src_bb));
		  else
		    {
		      /* It doesn't make sense to switch to normal
		         mode after a CALL_INSN.  The cases in which a
		         CALL_INSN may have an abnormal edge are
		         sibcalls and EH edges.  In the case of
		         sibcalls, the dest basic-block is the
		         EXIT_BLOCK, that runs in normal mode; it is
		         assumed that a sibcall insn requires normal
		         mode itself, so no mode switch would be
		         required after the call (it wouldn't make
		         sense, anyway).  In the case of EH edges, EH
		         entry points also start in normal mode, so a
		         similar reasoning applies.  */
		      gcc_assert (NONJUMP_INSN_P (BB_END (src_bb)));
		      emit_insn_after (mode_set, BB_END (src_bb));
		    }
		  bb_info[j][src_bb->index].computing = mode;
		  RESET_BIT (transp[src_bb->index], j);
		}
	      else
		{
		  need_commit = 1;
		  insert_insn_on_edge (mode_set, eg);
		}
	    }

	  FOR_EACH_BB_REVERSE (bb)
	    if (TEST_BIT (delete[bb->index], j))
	      {
		make_preds_opaque (bb, j);
		/* Cancel the 'deleted' mode set.  */
		bb_info[j][bb->index].seginfo->mode = no_mode;
	      }
	}

      sbitmap_vector_free (delete);
      sbitmap_vector_free (insert);
      clear_aux_for_edges ();
      free_edge_list (edge_list);
    }

  /* Now output the remaining mode sets in all the segments.  */
  for (j = n_entities - 1; j >= 0; j--)
    {
      int no_mode = num_modes[entity_map[j]];

      FOR_EACH_BB_REVERSE (bb)
	{
	  struct seginfo *ptr, *next;
	  for (ptr = bb_info[j][bb->index].seginfo; ptr; ptr = next)
	    {
	      next = ptr->next;
	      if (ptr->mode != no_mode)
		{
		  rtx mode_set;

		  start_sequence ();
		  EMIT_MODE_SET (entity_map[j], ptr->mode, ptr->regs_live);
		  mode_set = get_insns ();
		  end_sequence ();

		  /* Insert MODE_SET only if it is nonempty.  */
		  if (mode_set != NULL_RTX)
		    {
		      emited = true;
		      if (NOTE_P (ptr->insn_ptr)
			  && (NOTE_LINE_NUMBER (ptr->insn_ptr)
			      == NOTE_INSN_BASIC_BLOCK))
			emit_insn_after (mode_set, ptr->insn_ptr);
		      else
			emit_insn_before (mode_set, ptr->insn_ptr);
		    }
		}

	      free (ptr);
	    }
	}

      free (bb_info[j]);
    }

  /* Finished. Free up all the things we've allocated.  */

  sbitmap_vector_free (kill);
  sbitmap_vector_free (antic);
  sbitmap_vector_free (transp);
  sbitmap_vector_free (comp);

  if (need_commit)
    commit_edge_insertions ();

#if defined (MODE_ENTRY) && defined (MODE_EXIT)
  cleanup_cfg (CLEANUP_NO_INSN_DEL);
#else
  if (!need_commit && !emited)
    return 0;
#endif

  max_regno = max_reg_num ();
  allocate_reg_info (max_regno, FALSE, FALSE);
  update_life_info_in_dirty_blocks (UPDATE_LIFE_GLOBAL_RM_NOTES,
				    (PROP_DEATH_NOTES | PROP_KILL_DEAD_CODE
				     | PROP_SCAN_DEAD_CODE));

  return 1;
}
#endif /* OPTIMIZE_MODE_SWITCHING */
