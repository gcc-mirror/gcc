/* Generic partial redundancy elimination with lazy code motion
   support.
   Copyright (C) 1998 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

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

/* Edge based LCM routines.  */
static void compute_antinout_edge  PROTO ((sbitmap *, sbitmap *,
					   sbitmap *, sbitmap *));
static void compute_earliest  PROTO((struct edge_list *, int, sbitmap *,
				     sbitmap *, sbitmap *, sbitmap *,
				     sbitmap *));
static void compute_laterin  PROTO((struct edge_list *, int, sbitmap *,
				     sbitmap *, sbitmap *, sbitmap *));
static void compute_insert_delete  PROTO ((struct edge_list *edge_list,
					   sbitmap *, sbitmap *, sbitmap *,
					   sbitmap *, sbitmap *));

/* Edge based LCM routines on a reverse flowgraph.  */
static void compute_farthest	PROTO  ((struct edge_list *, int, sbitmap *,
					 sbitmap *, sbitmap*, sbitmap *,
					 sbitmap *));
static void compute_nearerout	PROTO((struct edge_list *, int, sbitmap *,
				       sbitmap *, sbitmap *, sbitmap *));
static void compute_rev_insert_delete  PROTO ((struct edge_list *edge_list,
					       sbitmap *, sbitmap *, sbitmap *,
					       sbitmap *, sbitmap *));


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
  int i, changed, passes;
  sbitmap old_changed, new_changed;
  edge e;

  sbitmap_vector_zero (antout, n_basic_blocks);
  sbitmap_vector_ones (antin, n_basic_blocks);

  old_changed = sbitmap_alloc (n_basic_blocks);
  new_changed = sbitmap_alloc (n_basic_blocks);
  sbitmap_ones (old_changed);

  passes = 0;
  changed = 1;
  while (changed)
    {
      changed = 0;
      sbitmap_zero (new_changed);

      /* We scan the blocks in the reverse order to speed up
	 the convergence.  */
      for (i = n_basic_blocks - 1; i >= 0; i--)
	{
	  basic_block bb = BASIC_BLOCK (i);
	  /* If none of the successors of this block have changed,
	     then this block is not going to change.  */
	  for (e = bb->succ ; e; e = e->succ_next)
	    {
	      if (e->dest == EXIT_BLOCK_PTR)
		break;

	      if (TEST_BIT (old_changed, e->dest->index)
		  || TEST_BIT (new_changed, e->dest->index))
		break;
	    }

	  if (!e)
	    continue;

          /* If an Exit blocks is the ONLY successor, its has a zero ANTIN, 
	     which is the opposite of the default definition for an 
	     intersection of succs definition.  */
	  if (e->dest == EXIT_BLOCK_PTR && e->succ_next == NULL 
	      && e->src->succ == e)
	    sbitmap_zero (antout[bb->index]);
	  else
	    {
	      sbitmap_intersection_of_succs (antout[bb->index],
					     antin, 
					     bb->index);
	    }

 	  if (sbitmap_a_or_b_and_c (antin[bb->index], antloc[bb->index],
				    transp[bb->index], antout[bb->index]))
	    {
	      changed = 1;
	      SET_BIT (new_changed, bb->index);
	    }
	}
      sbitmap_copy (old_changed, new_changed);
      passes++;
    }

  free (old_changed);
  free (new_changed);
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
	    {
	      sbitmap_zero (earliest[x]);
	    }
	  else
	    {
	      sbitmap_difference (difference, antin[succ->index], 
	      			  avout[pred->index]);
	      sbitmap_not (temp_bitmap, antout[pred->index]);
	      sbitmap_a_and_b_or_c (earliest[x], difference, kill[pred->index], 
				    temp_bitmap);
	    }
	}
    }
  free (temp_bitmap);
  free (difference);
}

/* Compute later and laterin vectors for edge based lcm.  */
static void
compute_laterin (edge_list, n_exprs,
		 earliest, antloc, later, laterin)
     struct edge_list *edge_list;
     int n_exprs;
     sbitmap *earliest, *antloc, *later, *laterin;
{
  sbitmap difference, temp_bitmap;
  int x, num_edges; 
  basic_block pred, succ;
  int done = 0;

  num_edges = NUM_EDGES (edge_list);

  /* Laterin has an extra block allocated for the exit block.  */
  sbitmap_vector_ones (laterin, n_basic_blocks + 1);
  sbitmap_vector_zero (later, num_edges);

  /* Initialize laterin to the intersection of EARLIEST for all edges
     from predecessors to this block. */

  for (x = 0; x < num_edges; x++)
    {
      succ = INDEX_EDGE_SUCC_BB (edge_list, x);
      pred = INDEX_EDGE_PRED_BB (edge_list, x);
      if (succ != EXIT_BLOCK_PTR)
	sbitmap_a_and_b (laterin[succ->index], laterin[succ->index], 
			 earliest[x]);
      /* We already know the correct value of later for edges from
         the entry node, so set it now.  */
      if (pred == ENTRY_BLOCK_PTR)
	sbitmap_copy (later[x], earliest[x]);
    }

  difference = sbitmap_alloc (n_exprs);

  while (!done)
    {
      done = 1;
      for (x = 0; x < num_edges; x++)
	{
          pred = INDEX_EDGE_PRED_BB (edge_list, x);
	  if (pred != ENTRY_BLOCK_PTR)
	    {
	      sbitmap_difference (difference, laterin[pred->index], 
	      			  antloc[pred->index]);
	      if (sbitmap_a_or_b (later[x], difference, earliest[x]))
		done = 0;
	    }
	}
      if (done)
        break;

      sbitmap_vector_ones (laterin, n_basic_blocks);

      for (x = 0; x < num_edges; x++)
	{
	  succ = INDEX_EDGE_SUCC_BB (edge_list, x);
	  if (succ != EXIT_BLOCK_PTR)
	    sbitmap_a_and_b (laterin[succ->index], laterin[succ->index], 
	    		     later[x]);
	  else
	    /* We allocated an extra block for the exit node.  */
	    sbitmap_a_and_b (laterin[n_basic_blocks], laterin[n_basic_blocks], 
	    		     later[x]);
	}
    }

  free (difference);
}

/* Compute the insertion and deletion points for edge based LCM.  */
static void
compute_insert_delete (edge_list, antloc, later, laterin,
		       insert, delete)
     struct edge_list *edge_list;
     sbitmap *antloc, *later, *laterin, *insert, *delete;
{
  int x;

  for (x = 0; x < n_basic_blocks; x++)
    sbitmap_difference (delete[x], antloc[x], laterin[x]);
     
  for (x = 0; x < NUM_EDGES (edge_list); x++)
    {
      basic_block b = INDEX_EDGE_SUCC_BB (edge_list, x);
      if (b == EXIT_BLOCK_PTR)
	sbitmap_difference (insert[x], later[x], laterin[n_basic_blocks]);
      else
	sbitmap_difference (insert[x], later[x], laterin[b->index]);
    }
}

/* Given local properties TRANSP, ANTLOC, AVOUT, KILL return the 
   insert and delete vectors for edge based LCM.  Returns an
   edgelist which is used to map the insert vector to what edge
   an expression should be inserted on.  */

struct edge_list *
pre_edge_lcm (file, n_exprs, transp, avloc, antloc, kill, insert, delete)
     FILE *file;
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
      dump_sbitmap_vector (file, "transp", "", transp, n_basic_blocks);
      dump_sbitmap_vector (file, "antloc", "", antloc, n_basic_blocks);
      dump_sbitmap_vector (file, "avloc", "", avloc, n_basic_blocks);
      dump_sbitmap_vector (file, "kill", "", kill, n_basic_blocks);
    }
#endif

  /* Compute global availability.  */
  avin = sbitmap_vector_alloc (n_basic_blocks, n_exprs);
  avout = sbitmap_vector_alloc (n_basic_blocks, n_exprs);
  compute_available (avloc, kill, avout, avin);

  free (avin);

  /* Compute global anticipatability.  */
  antin = sbitmap_vector_alloc (n_basic_blocks, n_exprs);
  antout = sbitmap_vector_alloc (n_basic_blocks, n_exprs);
  compute_antinout_edge (antloc, transp, antin, antout);

#ifdef LCM_DEBUG_INFO
  if (file)
    {
      dump_sbitmap_vector (file, "antin", "", antin, n_basic_blocks);
      dump_sbitmap_vector (file, "antout", "", antout, n_basic_blocks);
    }
#endif

  /* Compute earliestness.  */
  earliest = sbitmap_vector_alloc (num_edges, n_exprs);
  compute_earliest (edge_list, n_exprs, antin, antout, avout, kill, earliest);

#ifdef LCM_DEBUG_INFO
  if (file)
    dump_sbitmap_vector (file, "earliest", "", earliest, num_edges);
#endif

  free (antout);
  free (antin);
  free (avout);

  later = sbitmap_vector_alloc (num_edges, n_exprs);
  /* Allocate an extra element for the exit block in the laterin vector.  */
  laterin = sbitmap_vector_alloc (n_basic_blocks + 1, n_exprs);
  compute_laterin (edge_list, n_exprs, earliest, antloc, later, laterin);

#ifdef LCM_DEBUG_INFO
  if (file)
    {
      dump_sbitmap_vector (file, "laterin", "", laterin, n_basic_blocks + 1);
      dump_sbitmap_vector (file, "later", "", later, num_edges);
    }
#endif

  free (earliest);

  *insert = sbitmap_vector_alloc (num_edges, n_exprs);
  *delete = sbitmap_vector_alloc (n_basic_blocks, n_exprs);
  compute_insert_delete (edge_list, antloc, later, laterin, *insert, *delete);

  free (laterin);
  free (later);

#ifdef LCM_DEBUG_INFO
  if (file)
    {
      dump_sbitmap_vector (file, "pre_insert_map", "", *insert, num_edges);
      dump_sbitmap_vector (file, "pre_delete_map", "", *delete, n_basic_blocks);
    }
#endif

  return edge_list;
}

/* Compute the AVIN and AVOUT vectors from the AVLOC and KILL vectors.
   Return the number of passes we performed to iterate to a solution.  */
int
compute_available (avloc, kill, avout, avin)
     sbitmap *avloc, *kill, *avout, *avin;  
{
  int bb, changed, passes;
  int last = n_basic_blocks - 1;

  sbitmap_zero (avin[0]);
  sbitmap_copy (avout[0] /*dst*/, avloc[0] /*src*/);

  for (bb = 1; bb < n_basic_blocks; bb++)
    sbitmap_not (avout[bb], kill[bb]);
    
  passes = 0;
  changed = 1;
  while (changed)
    {
      changed = 0;
      for (bb = 1; bb < n_basic_blocks; bb++)
        {
          sbitmap_intersection_of_preds (avin[bb], avout, bb);
          changed |= sbitmap_union_of_diff (avout[bb], avloc[bb],
                                            avin[bb], kill[bb]);
        }
      passes++;
    }
  return passes;
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
	    {
	      sbitmap_zero (farthest[x]);
	    }
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
  free (temp_bitmap);
  free (difference);
}

/* Compute nearer and nearerout vectors for edge based lcm.  */
static void
compute_nearerout (edge_list, n_exprs,
		   farthest, st_avloc, nearer, nearerout)
     struct edge_list *edge_list;
     int n_exprs;
     sbitmap *farthest, *st_avloc, *nearer, *nearerout;
{
  sbitmap difference, temp_bitmap;
  int x, num_edges; 
  basic_block pred, succ;
  int done = 0;

  num_edges = NUM_EDGES (edge_list);

  /* nearout has an extra block allocated for the entry block.  */
  sbitmap_vector_ones (nearerout, n_basic_blocks + 1);
  sbitmap_vector_zero (nearer, num_edges);

  /* Initialize nearerout to the intersection of FARTHEST for all edges
     from predecessors to this block. */

  for (x = 0; x < num_edges; x++)
    {
      succ = INDEX_EDGE_SUCC_BB (edge_list, x);
      pred = INDEX_EDGE_PRED_BB (edge_list, x);
      if (pred != ENTRY_BLOCK_PTR)
        {
	  sbitmap_a_and_b (nearerout[pred->index], nearerout[pred->index], 
			   farthest[x]);
	}
      /* We already know the correct value of nearer for edges to 
         the exit node.  */
      if (succ == EXIT_BLOCK_PTR)
	sbitmap_copy (nearer[x], farthest[x]);
    }

  difference = sbitmap_alloc (n_exprs);

  while (!done)
    {
      done = 1;
      for (x = 0; x < num_edges; x++)
	{
          succ = INDEX_EDGE_SUCC_BB (edge_list, x);
	  if (succ != EXIT_BLOCK_PTR)
	    {
	      sbitmap_difference (difference, nearerout[succ->index], 
				  st_avloc[succ->index]);
	      if (sbitmap_a_or_b (nearer[x], difference, farthest[x]))
		done = 0;
	    }
	}

      if (done)
        break;

      sbitmap_vector_zero (nearerout, n_basic_blocks);

      for (x = 0; x < num_edges; x++)
	{
	  pred = INDEX_EDGE_PRED_BB (edge_list, x);
	  if (pred != ENTRY_BLOCK_PTR)
	      sbitmap_a_and_b (nearerout[pred->index], 
			       nearerout[pred->index], nearer[x]);
	    else
	      sbitmap_a_and_b (nearerout[n_basic_blocks], 
			       nearerout[n_basic_blocks], nearer[x]);
	}
    }

  free (difference);
}

/* Compute the insertion and deletion points for edge based LCM.  */
static void
compute_rev_insert_delete (edge_list, st_avloc, nearer, nearerout,
			   insert, delete)
     struct edge_list *edge_list;
     sbitmap *st_avloc, *nearer, *nearerout, *insert, *delete;
{
  int x;

  for (x = 0; x < n_basic_blocks; x++)
    sbitmap_difference (delete[x], st_avloc[x], nearerout[x]);
     
  for (x = 0; x < NUM_EDGES (edge_list); x++)
    {
      basic_block b = INDEX_EDGE_PRED_BB (edge_list, x);
      if (b == ENTRY_BLOCK_PTR)
	sbitmap_difference (insert[x], nearer[x], nearerout[n_basic_blocks]);
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
     FILE *file;
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
  int x,num_edges;

  edge_list = create_edge_list ();
  num_edges = NUM_EDGES (edge_list);

  st_antin = (sbitmap *) sbitmap_vector_alloc (n_basic_blocks, n_exprs);
  st_antout = (sbitmap *) sbitmap_vector_alloc (n_basic_blocks, n_exprs);
  sbitmap_vector_zero (st_antin, n_basic_blocks);
  sbitmap_vector_zero (st_antout, n_basic_blocks);
  compute_antinout_edge (st_antloc, transp, st_antin, st_antout);

  /* Compute global anticipatability.  */
  st_avout = sbitmap_vector_alloc (n_basic_blocks, n_exprs);
  st_avin = sbitmap_vector_alloc (n_basic_blocks, n_exprs);
  compute_available (st_avloc, kill, st_avout, st_avin);

#ifdef LCM_DEBUG_INFO
  if (file)
    {
      fprintf (file, "Edge List:\n");
      verify_edge_list (file, edge_list);
      print_edge_list (file, edge_list);
      dump_sbitmap_vector (file, "transp", "", transp, n_basic_blocks);
      dump_sbitmap_vector (file, "st_avloc", "", st_avloc, n_basic_blocks);
      dump_sbitmap_vector (file, "st_antloc", "", st_antloc, n_basic_blocks);
      dump_sbitmap_vector (file, "st_antin", "", st_antin, n_basic_blocks);
      dump_sbitmap_vector (file, "st_antout", "", st_antout, n_basic_blocks);
      dump_sbitmap_vector (file, "st_kill", "", kill, n_basic_blocks);
    }
#endif

#ifdef LCM_DEBUG_INFO
  if (file)
    {
      dump_sbitmap_vector (file, "st_avout", "", st_avout, n_basic_blocks);
      dump_sbitmap_vector (file, "st_avin", "", st_avin, n_basic_blocks);
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

  free (st_avin);
  free (st_avout);

  nearer = sbitmap_vector_alloc (num_edges, n_exprs);
  /* Allocate an extra element for the entry block.  */
  nearerout = sbitmap_vector_alloc (n_basic_blocks + 1, n_exprs);
  compute_nearerout (edge_list, n_exprs, farthest, st_avloc, nearer, nearerout);

#ifdef LCM_DEBUG_INFO
  if (file)
    {
      dump_sbitmap_vector (file, "nearerout", "", nearerout, 
			   n_basic_blocks + 1);
      dump_sbitmap_vector (file, "nearer", "", nearer, num_edges);
    }
#endif

  free (farthest);

  *insert = sbitmap_vector_alloc (num_edges, n_exprs);
  *delete = sbitmap_vector_alloc (n_basic_blocks, n_exprs);
  compute_rev_insert_delete (edge_list, st_avloc, nearer, nearerout, *insert, *delete);

  free (nearerout);
  free (nearer);

#ifdef LCM_DEBUG_INFO
  if (file)
    {
      dump_sbitmap_vector (file, "pre_insert_map", "", *insert, num_edges);
      dump_sbitmap_vector (file, "pre_delete_map", "", *delete, n_basic_blocks);
    }
#endif

  return edge_list;
}
