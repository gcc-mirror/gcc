/* Generic partial redundancy elimination with lazy code motion support.
   Copyright (C) 1998, 1999, 2000, 2001, 2002 Free Software Foundation, Inc.

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

/* We want target macros for the mode switching code to be able to refer
   to instruction attribute values.  */
#include "insn-attr.h"

/* Edge based LCM routines.  */
static void compute_antinout_edge	PARAMS ((sbitmap *, sbitmap *,
						 sbitmap *, sbitmap *));
static void compute_earliest		PARAMS ((struct edge_list *, int,
						 sbitmap *, sbitmap *,
						 sbitmap *, sbitmap *,
						 sbitmap *));
static void compute_laterin		PARAMS ((struct edge_list *, sbitmap *,
						 sbitmap *, sbitmap *,
						 sbitmap *));
static void compute_insert_delete	PARAMS ((struct edge_list *edge_list,
						 sbitmap *, sbitmap *,
						 sbitmap *, sbitmap *,
						 sbitmap *));

/* Edge based LCM routines on a reverse flowgraph.  */
static void compute_farthest		PARAMS ((struct edge_list *, int,
						 sbitmap *, sbitmap *,
						 sbitmap*, sbitmap *,
						 sbitmap *));
static void compute_nearerout		PARAMS ((struct edge_list *, sbitmap *,
						 sbitmap *, sbitmap *,
						 sbitmap *));
static void compute_rev_insert_delete	PARAMS ((struct edge_list *edge_list,
						 sbitmap *, sbitmap *,
						 sbitmap *, sbitmap *,
						 sbitmap *));

/* Edge based lcm routines.  */

/* Compute expression anticipatability at entrance and exit of each block.
   This is done based on the flow graph, and not on the pred-succ lists.
   Other than that, its pretty much identical to compute_antinout.  */

static void
compute_antinout_edge (antloc, transp, antin, antout)
     sbitmap *antloc;
     sbitmap *transp;
     sbitmap *antin;
     sbitmap *antout;
{
  basic_block bb;
  edge e;
  basic_block *worklist, *qin, *qout, *qend;
  unsigned int qlen;

  /* Allocate a worklist array/queue.  Entries are only added to the
     list if they were not already on the list.  So the size is
     bounded by the number of basic blocks.  */
  qin = qout = worklist
    = (basic_block *) xmalloc (sizeof (basic_block) * n_basic_blocks);

  /* We want a maximal solution, so make an optimistic initialization of
     ANTIN.  */
  sbitmap_vector_ones (antin, last_basic_block);

  /* Put every block on the worklist; this is necessary because of the
     optimistic initialization of ANTIN above.  */
  FOR_EACH_BB_REVERSE (bb)
    {
      *qin++ =bb;
      bb->aux = bb;
    }

  qin = worklist;
  qend = &worklist[n_basic_blocks];
  qlen = n_basic_blocks;

  /* Mark blocks which are predecessors of the exit block so that we
     can easily identify them below.  */
  for (e = EXIT_BLOCK_PTR->pred; e; e = e->pred_next)
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
	for (e = bb->pred; e; e = e->pred_next)
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
compute_earliest (edge_list, n_exprs, antin, antout, avout, kill, earliest)
     struct edge_list *edge_list;
     int n_exprs;
     sbitmap *antin, *antout, *avout, *kill, *earliest;
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
compute_laterin (edge_list, earliest, antloc, later, laterin)
     struct edge_list *edge_list;
     sbitmap *earliest, *antloc, *later, *laterin;
{
  int num_edges, i;
  edge e;
  basic_block *worklist, *qin, *qout, *qend, bb;
  unsigned int qlen;

  num_edges = NUM_EDGES (edge_list);

  /* Allocate a worklist array/queue.  Entries are only added to the
     list if they were not already on the list.  So the size is
     bounded by the number of basic blocks.  */
  qin = qout = worklist
    = (basic_block *) xmalloc (sizeof (basic_block) * (n_basic_blocks + 1));

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
  for (e = ENTRY_BLOCK_PTR->succ; e; e = e->succ_next)
    sbitmap_copy (later[(size_t) e->aux], earliest[(size_t) e->aux]);

  /* Add all the blocks to the worklist.  This prevents an early exit from
     the loop given our optimistic initialization of LATER above.  */
  FOR_EACH_BB (bb)
    {
      *qin++ = bb;
      bb->aux = bb;
    }
  qin = worklist;
  /* Note that we do not use the last allocated element for our queue,
     as EXIT_BLOCK is never inserted into it. In fact the above allocation
     of n_basic_blocks + 1 elements is not encessary.  */
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
      for (e = bb->pred; e != NULL; e = e->pred_next)
	sbitmap_a_and_b (laterin[bb->index], laterin[bb->index], later[(size_t)e->aux]);

      /* Calculate LATER for all outgoing edges.  */
      for (e = bb->succ; e != NULL; e = e->succ_next)
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
  for (e = EXIT_BLOCK_PTR->pred; e != NULL; e = e->pred_next)
    sbitmap_a_and_b (laterin[last_basic_block],
		     laterin[last_basic_block],
		     later[(size_t) e->aux]);

  clear_aux_for_edges ();
  free (worklist);
}

/* Compute the insertion and deletion points for edge based LCM.  */

static void
compute_insert_delete (edge_list, antloc, later, laterin,
		       insert, delete)
     struct edge_list *edge_list;
     sbitmap *antloc, *later, *laterin, *insert, *delete;
{
  int x;
  basic_block bb;

  FOR_EACH_BB (bb)
    sbitmap_difference (delete[bb->index], antloc[bb->index], laterin[bb->index]);

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
pre_edge_lcm (file, n_exprs, transp, avloc, antloc, kill, insert, delete)
     FILE *file ATTRIBUTE_UNUSED;
     int n_exprs;
     sbitmap *transp;
     sbitmap *avloc;
     sbitmap *antloc;
     sbitmap *kill;
     sbitmap **insert;
     sbitmap **delete;
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
compute_available (avloc, kill, avout, avin)
     sbitmap *avloc, *kill, *avout, *avin;
{
  edge e;
  basic_block *worklist, *qin, *qout, *qend, bb;
  unsigned int qlen;

  /* Allocate a worklist array/queue.  Entries are only added to the
     list if they were not already on the list.  So the size is
     bounded by the number of basic blocks.  */
  qin = qout = worklist
    = (basic_block *) xmalloc (sizeof (basic_block) * n_basic_blocks);

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
  for (e = ENTRY_BLOCK_PTR->succ; e; e = e->succ_next)
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

      if (sbitmap_union_of_diff_cg (avout[bb->index], avloc[bb->index], avin[bb->index], kill[bb->index]))
	/* If the out state of this block changed, then we need
	   to add the successors of this block to the worklist
	   if they are not already on the worklist.  */
	for (e = bb->succ; e; e = e->succ_next)
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
compute_farthest (edge_list, n_exprs, st_avout, st_avin, st_antin,
		  kill, farthest)
     struct edge_list *edge_list;
     int n_exprs;
     sbitmap *st_avout, *st_avin, *st_antin, *kill, *farthest;
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
compute_nearerout (edge_list, farthest, st_avloc, nearer, nearerout)
     struct edge_list *edge_list;
     sbitmap *farthest, *st_avloc, *nearer, *nearerout;
{
  int num_edges, i;
  edge e;
  basic_block *worklist, *tos, bb;

  num_edges = NUM_EDGES (edge_list);

  /* Allocate a worklist array/queue.  Entries are only added to the
     list if they were not already on the list.  So the size is
     bounded by the number of basic blocks.  */
  tos = worklist
    = (basic_block *) xmalloc (sizeof (basic_block) * (n_basic_blocks + 1));

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
  for (e = EXIT_BLOCK_PTR->pred; e; e = e->pred_next)
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
      for (e = bb->succ; e != NULL; e = e->succ_next)
	sbitmap_a_and_b (nearerout[bb->index], nearerout[bb->index],
			 nearer[(size_t) e->aux]);

      /* Calculate NEARER for all incoming edges.  */
      for (e = bb->pred; e != NULL; e = e->pred_next)
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
  for (e = ENTRY_BLOCK_PTR->succ; e != NULL; e = e->succ_next)
    sbitmap_a_and_b (nearerout[last_basic_block],
		     nearerout[last_basic_block],
		     nearer[(size_t) e->aux]);

  clear_aux_for_edges ();
  free (tos);
}

/* Compute the insertion and deletion points for edge based LCM.  */

static void
compute_rev_insert_delete (edge_list, st_avloc, nearer, nearerout,
			   insert, delete)
     struct edge_list *edge_list;
     sbitmap *st_avloc, *nearer, *nearerout, *insert, *delete;
{
  int x;
  basic_block bb;

  FOR_EACH_BB (bb)
    sbitmap_difference (delete[bb->index], st_avloc[bb->index], nearerout[bb->index]);

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
pre_edge_rev_lcm (file, n_exprs, transp, st_avloc, st_antloc, kill,
		  insert, delete)
     FILE *file ATTRIBUTE_UNUSED;
     int n_exprs;
     sbitmap *transp;
     sbitmap *st_avloc;
     sbitmap *st_antloc;
     sbitmap *kill;
     sbitmap **insert;
     sbitmap **delete;
{
  sbitmap *st_antin, *st_antout;
  sbitmap *st_avout, *st_avin, *farthest;
  sbitmap *nearer, *nearerout;
  struct edge_list *edge_list;
  int num_edges;

  edge_list = create_edge_list ();
  num_edges = NUM_EDGES (edge_list);

  st_antin = (sbitmap *) sbitmap_vector_alloc (last_basic_block, n_exprs);
  st_antout = (sbitmap *) sbitmap_vector_alloc (last_basic_block, n_exprs);
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
   insn in any one block.  Any adjustments required to the transparancy
   vectors are made, then the next iteration starts for the next-lower
   priority mode, till for each entity all modes are exhasted.

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
static sbitmap *delete;
static sbitmap *insert;

static struct seginfo * new_seginfo PARAMS ((int, rtx, int, HARD_REG_SET));
static void add_seginfo PARAMS ((struct bb_info *, struct seginfo *));
static void reg_dies PARAMS ((rtx, HARD_REG_SET));
static void reg_becomes_live PARAMS ((rtx, rtx, void *));
static void make_preds_opaque PARAMS ((basic_block, int));
#endif

#ifdef OPTIMIZE_MODE_SWITCHING

/* This function will allocate a new BBINFO structure, initialized
   with the MODE, INSN, and basic block BB parameters.  */

static struct seginfo *
new_seginfo (mode, insn, bb, regs_live)
     int mode;
     rtx insn;
     int bb;
     HARD_REG_SET regs_live;
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
add_seginfo (head, info)
     struct bb_info *head;
     struct seginfo *info;
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
make_preds_opaque (b, j)
     basic_block b;
     int j;
{
  edge e;

  for (e = b->pred; e; e = e->pred_next)
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
reg_dies (reg, live)
     rtx reg;
     HARD_REG_SET live;
{
  int regno, nregs;

  if (GET_CODE (reg) != REG)
    return;

  regno = REGNO (reg);
  if (regno < FIRST_PSEUDO_REGISTER)
    for (nregs = HARD_REGNO_NREGS (regno, GET_MODE (reg)) - 1; nregs >= 0;
	 nregs--)
      CLEAR_HARD_REG_BIT (live, regno + nregs);
}

/* Record in LIVE that register REG became live.
   This is called via note_stores.  */

static void
reg_becomes_live (reg, setter, live)
     rtx reg;
     rtx setter ATTRIBUTE_UNUSED;
     void *live;
{
  int regno, nregs;

  if (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);

  if (GET_CODE (reg) != REG)
    return;

  regno = REGNO (reg);
  if (regno < FIRST_PSEUDO_REGISTER)
    for (nregs = HARD_REGNO_NREGS (regno, GET_MODE (reg)) - 1; nregs >= 0;
	 nregs--)
      SET_HARD_REG_BIT (* (HARD_REG_SET *) live, regno + nregs);
}

/* Find all insns that need a particular mode setting, and insert the
   necessary mode switches.  Return true if we did work.  */

int
optimize_mode_switching (file)
     FILE *file;
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
#ifdef NORMAL_MODE
	entry_exit_extra = 2;
#endif
	bb_info[n_entities]
	  = (struct bb_info *) xcalloc (last_basic_block + entry_exit_extra,
					sizeof **bb_info);
	entity_map[n_entities++] = e;
	if (num_modes[e] > max_num_modes)
	  max_num_modes = num_modes[e];
      }

  if (! n_entities)
    return 0;

#ifdef NORMAL_MODE
  {
    /* Split the edge from the entry block and the fallthrough edge to the
       exit block, so that we can note that there NORMAL_MODE is supplied /
       required.  */
    edge eg;
    post_entry = split_edge (ENTRY_BLOCK_PTR->succ);
    /* The only non-call predecessor at this stage is a block with a
       fallthrough edge; there can be at most one, but there could be
       none at all, e.g. when exit is called.  */
    for (pre_exit = 0, eg = EXIT_BLOCK_PTR->pred; eg; eg = eg->pred_next)
      if (eg->flags & EDGE_FALLTHRU)
	{
	  regset live_at_end = eg->src->global_live_at_end;

	  if (pre_exit)
	    abort ();
	  pre_exit = split_edge (eg);
	  COPY_REG_SET (pre_exit->global_live_at_start, live_at_end);
	  COPY_REG_SET (pre_exit->global_live_at_end, live_at_end);
	}
  }
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
	  for (insn = bb->head;
	       insn != NULL && insn != NEXT_INSN (bb->end);
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
	      ptr = new_seginfo (no_mode, bb->end, bb->index, live_now);
	      add_seginfo (info + bb->index, ptr);
	    }
	}
#ifdef NORMAL_MODE
      {
	int mode = NORMAL_MODE (e);

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
	      info[pre_exit->index].seginfo->mode = mode;
	  }
      }
#endif /* NORMAL_MODE */
    }

  kill = sbitmap_vector_alloc (last_basic_block, n_entities);
  for (i = 0; i < max_num_modes; i++)
    {
      int current_mode[N_ENTITIES];

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
		  if (GET_CODE (src_bb->end) == JUMP_INSN)
		    emit_insn_before (mode_set, src_bb->end);
		  /* It doesn't make sense to switch to normal mode
		     after a CALL_INSN, so we're going to abort if we
		     find one.  The cases in which a CALL_INSN may
		     have an abnormal edge are sibcalls and EH edges.
		     In the case of sibcalls, the dest basic-block is
		     the EXIT_BLOCK, that runs in normal mode; it is
		     assumed that a sibcall insn requires normal mode
		     itself, so no mode switch would be required after
		     the call (it wouldn't make sense, anyway).  In
		     the case of EH edges, EH entry points also start
		     in normal mode, so a similar reasoning applies.  */
		  else if (GET_CODE (src_bb->end) == INSN)
		    emit_insn_after (mode_set, src_bb->end);
		  else
		    abort ();
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

		  /* Do not bother to insert empty sequence.  */
		  if (mode_set == NULL_RTX)
		    continue;

		  emited = true;
		  if (GET_CODE (ptr->insn_ptr) == NOTE
		      && (NOTE_LINE_NUMBER (ptr->insn_ptr)
			  == NOTE_INSN_BASIC_BLOCK))
		    emit_insn_after (mode_set, ptr->insn_ptr);
		  else
		    emit_insn_before (mode_set, ptr->insn_ptr);
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
  sbitmap_vector_free (delete);
  sbitmap_vector_free (insert);

  if (need_commit)
    commit_edge_insertions ();

#ifdef NORMAL_MODE
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
