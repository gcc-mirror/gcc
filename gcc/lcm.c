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

static void compute_antinout 	PROTO ((int, int_list_ptr *, sbitmap *,
					sbitmap *, sbitmap *, sbitmap *));
static void compute_earlyinout	PROTO ((int, int, int_list_ptr *, sbitmap *,
					sbitmap *, sbitmap *, sbitmap *));
static void compute_delayinout  PROTO ((int, int, int_list_ptr *, sbitmap *,
					sbitmap *, sbitmap *,
					sbitmap *, sbitmap *));
static void compute_latein	PROTO ((int, int, int_list_ptr *, sbitmap *,
					sbitmap *, sbitmap *));
static void compute_isoinout	PROTO ((int, int_list_ptr *, sbitmap *,
					sbitmap *, sbitmap *, sbitmap *));
static void compute_optimal	PROTO ((int, sbitmap *,
					sbitmap *, sbitmap *));
static void compute_redundant	PROTO ((int, int, sbitmap *,
					sbitmap *, sbitmap *, sbitmap *));

/* Similarly, but for the reversed flowgraph.  */
static void compute_avinout 	PROTO ((int, int_list_ptr *, sbitmap *,
					sbitmap *, sbitmap *, sbitmap *));
static void compute_fartherinout	PROTO ((int, int, int_list_ptr *,
						sbitmap *, sbitmap *,
						sbitmap *, sbitmap *));
static void compute_earlierinout  PROTO ((int, int, int_list_ptr *, sbitmap *,
					  sbitmap *, sbitmap *,
					  sbitmap *, sbitmap *));
static void compute_firstout	PROTO ((int, int, int_list_ptr *, sbitmap *,
					sbitmap *, sbitmap *));
static void compute_rev_isoinout PROTO ((int, int_list_ptr *, sbitmap *,
					 sbitmap *, sbitmap *, sbitmap *));

/* Given local properties TRANSP, ANTLOC, return the redundant and optimal
   computation points for expressions.

   To reduce overall memory consumption, we allocate memory immediately
   before its needed and deallocate it as soon as possible.  */
void
pre_lcm (n_blocks, n_exprs, s_preds, s_succs, transp,
	 antloc, redundant, optimal)
     int n_blocks;
     int n_exprs;
     int_list_ptr *s_preds;
     int_list_ptr *s_succs;
     sbitmap *transp;
     sbitmap *antloc;
     sbitmap *redundant;
     sbitmap *optimal;
{
  sbitmap *antin, *antout, *earlyin, *earlyout, *delayin, *delayout;
  sbitmap *latein, *isoin, *isoout;

  /* Compute global anticipatability.  ANTOUT is not needed except to
     compute ANTIN, so free its memory as soon as we return from
     compute_antinout.  */
  antin = sbitmap_vector_alloc (n_blocks, n_exprs);
  antout = sbitmap_vector_alloc (n_blocks, n_exprs);
  compute_antinout (n_blocks, s_succs, antloc,
		    transp, antin, antout);
  free (antout);
  antout = NULL;

  /* Compute earliestness.  EARLYOUT is not needed except to compute
     EARLYIN, so free its memory as soon as we return from
     compute_earlyinout.  */
  earlyin = sbitmap_vector_alloc (n_blocks, n_exprs);
  earlyout = sbitmap_vector_alloc (n_blocks, n_exprs);
  compute_earlyinout (n_blocks, n_exprs, s_preds, transp, antin,
		      earlyin, earlyout);
  free (earlyout);
  earlyout = NULL;

  /* Compute delayedness.  DELAYOUT is not needed except to compute
     DELAYIN, so free its memory as soon as we return from
     compute_delayinout.  We also no longer need ANTIN and EARLYIN.  */
  delayin = sbitmap_vector_alloc (n_blocks, n_exprs);
  delayout = sbitmap_vector_alloc (n_blocks, n_exprs);
  compute_delayinout (n_blocks, n_exprs, s_preds, antloc,
		      antin, earlyin, delayin, delayout);
  free (delayout);
  delayout = NULL;
  free (antin);
  antin = NULL;
  free (earlyin);
  earlyin = NULL;

  /* Compute latestness.  We no longer need DELAYIN after we compute
     LATEIN.  */
  latein = sbitmap_vector_alloc (n_blocks, n_exprs);
  compute_latein (n_blocks, n_exprs, s_succs, antloc, delayin, latein);
  free (delayin);
  delayin = NULL;

  /* Compute isolatedness.  ISOIN is not needed except to compute
     ISOOUT, so free its memory as soon as we return from
     compute_isoinout.  */
  isoin = sbitmap_vector_alloc (n_blocks, n_exprs);
  isoout = sbitmap_vector_alloc (n_blocks, n_exprs);
  compute_isoinout (n_blocks, s_succs, antloc, latein, isoin, isoout);
  free (isoin);
  isoin = NULL;

  /* Now compute optimal placement points and the redundant expressions.  */
  compute_optimal (n_blocks, latein, isoout, optimal);
  compute_redundant (n_blocks, n_exprs, antloc, latein, isoout, redundant);
  free (latein);
  latein = NULL;
  free (isoout);
  isoout = NULL;
}

/* Given local properties TRANSP, AVLOC, return the redundant and optimal
   computation points for expressions on the reverse flowgraph.

   To reduce overall memory consumption, we allocate memory immediately
   before its needed and deallocate it as soon as possible.  */

void
pre_rev_lcm (n_blocks, n_exprs, s_preds, s_succs, transp,
	     avloc, redundant, optimal)
     int n_blocks;
     int n_exprs;
     int_list_ptr *s_preds;
     int_list_ptr *s_succs;
     sbitmap *transp;
     sbitmap *avloc;
     sbitmap *redundant;
     sbitmap *optimal;
{
  sbitmap *avin, *avout, *fartherin, *fartherout, *earlierin, *earlierout;
  sbitmap *firstout, *rev_isoin, *rev_isoout;

  /* Compute global availability.  AVIN is not needed except to
     compute AVOUT, so free its memory as soon as we return from
     compute_avinout.  */
  avin = sbitmap_vector_alloc (n_blocks, n_exprs);
  avout = sbitmap_vector_alloc (n_blocks, n_exprs);
  compute_avinout (n_blocks, s_preds, avloc, transp, avin, avout);
  free (avin);
  avin = NULL;

  /* Compute fartherness.  FARTHERIN is not needed except to compute
     FARTHEROUT, so free its memory as soon as we return from
     compute_earlyinout.  */
  fartherin = sbitmap_vector_alloc (n_blocks, n_exprs);
  fartherout = sbitmap_vector_alloc (n_blocks, n_exprs);
  compute_fartherinout (n_blocks, n_exprs, s_succs, transp,
			avout, fartherin, fartherout);
  free (fartherin);
  fartherin = NULL;

  /* Compute earlierness.  EARLIERIN is not needed except to compute
     EARLIEROUT, so free its memory as soon as we return from
     compute_delayinout.  We also no longer need AVOUT and FARTHEROUT.  */
  earlierin = sbitmap_vector_alloc (n_blocks, n_exprs);
  earlierout = sbitmap_vector_alloc (n_blocks, n_exprs);
  compute_earlierinout (n_blocks, n_exprs, s_succs, avloc,
		        avout, fartherout, earlierin, earlierout);
  free (earlierin);
  earlierin = NULL;
  free (avout);
  avout = NULL;
  free (fartherout);
  fartherout = NULL;

  /* Compute firstness.  We no longer need EARLIEROUT after we compute
     FIRSTOUT.  */
  firstout = sbitmap_vector_alloc (n_blocks, n_exprs);
  compute_firstout (n_blocks, n_exprs, s_preds, avloc, earlierout, firstout);
  free (earlierout);
  earlierout = NULL;

  /* Compute rev_isolatedness.  ISOIN is not needed except to compute
     ISOOUT, so free its memory as soon as we return from
     compute_isoinout.  */
  rev_isoin = sbitmap_vector_alloc (n_blocks, n_exprs);
  rev_isoout = sbitmap_vector_alloc (n_blocks, n_exprs);
  compute_rev_isoinout (n_blocks, s_preds, avloc, firstout,
			rev_isoin, rev_isoout);
  free (rev_isoout);
  rev_isoout = NULL;

  /* Now compute optimal placement points and the redundant expressions.  */
  compute_optimal (n_blocks, firstout, rev_isoin, optimal);
  compute_redundant (n_blocks, n_exprs, avloc, firstout, rev_isoin, redundant);
  free (firstout);
  firstout = NULL;
  free (rev_isoin);
  rev_isoin = NULL;
}

/* Compute expression anticipatability at entrance and exit of each block.  */

static void
compute_antinout (n_blocks, s_succs, antloc, transp, antin, antout)
     int n_blocks;
     int_list_ptr *s_succs;
     sbitmap *antloc;
     sbitmap *transp;
     sbitmap *antin;
     sbitmap *antout;
{
  int bb, changed, passes;
  sbitmap old_changed, new_changed;

  sbitmap_zero (antout[n_blocks - 1]);
  sbitmap_vector_ones (antin, n_blocks);

  old_changed = sbitmap_alloc (n_blocks);
  new_changed = sbitmap_alloc (n_blocks);
  sbitmap_ones (old_changed);

  passes = 0;
  changed = 1;
  while (changed)
    {
      changed = 0;
      sbitmap_zero (new_changed);
      /* We scan the blocks in the reverse order to speed up
	 the convergence.  */
      for (bb = n_blocks - 1; bb >= 0; bb--)
	{
	  int_list_ptr ps;

	  /* If none of the successors of this block have changed,
	     then this block is not going to change.  */
	  for (ps = s_succs[bb] ; ps; ps = ps->next)
	    {
	      if (INT_LIST_VAL (ps) == EXIT_BLOCK
		  || INT_LIST_VAL (ps) == ENTRY_BLOCK)
		break;

	      if (TEST_BIT (old_changed, INT_LIST_VAL (ps))
		  || TEST_BIT (new_changed, INT_LIST_VAL (ps)))
		break;
	    }

	  if (!ps)
	    continue;

	  if (bb != n_blocks - 1)
	    sbitmap_intersect_of_successors (antout[bb], antin,
					     bb, s_succs);
 	  if (sbitmap_a_or_b_and_c (antin[bb], antloc[bb],
				    transp[bb], antout[bb]))
	    {
	      changed = 1;
	      SET_BIT (new_changed, bb);
	    }
	}
      sbitmap_copy (old_changed, new_changed);
      passes++;
    }
  free (old_changed);
  free (new_changed);
}

/* Compute expression earliestness at entrance and exit of each block.

   From Advanced Compiler Design and Implementation pp411.

   An expression is earliest at the entrance to basic block BB if no
   block from entry to block BB both evaluates the expression and
   produces the same value as evaluating it at the entry to block BB
   does.  Similarly for earlistness at basic block BB exit.  */

static void
compute_earlyinout (n_blocks, n_exprs, s_preds, transp, antin,
		    earlyin, earlyout)
     int n_blocks;
     int n_exprs;
     int_list_ptr *s_preds;
     sbitmap *transp;
     sbitmap *antin;
     sbitmap *earlyin;
     sbitmap *earlyout;
{
  int bb, changed, passes;
  sbitmap temp_bitmap;
  sbitmap old_changed, new_changed;

  temp_bitmap = sbitmap_alloc (n_exprs);

  sbitmap_vector_zero (earlyout, n_blocks);
  sbitmap_ones (earlyin[0]);

  old_changed = sbitmap_alloc (n_blocks);
  new_changed = sbitmap_alloc (n_blocks);
  sbitmap_ones (old_changed);

  passes = 0;
  changed = 1;
  while (changed)
    {
      changed = 0;
      sbitmap_zero (new_changed);
      for (bb = 0; bb < n_blocks; bb++)
	{
	  int_list_ptr ps;

	  /* If none of the predecessors of this block have changed,
	     then this block is not going to change.  */
	  for (ps = s_preds[bb] ; ps; ps = ps->next)
	    {
	      if (INT_LIST_VAL (ps) == EXIT_BLOCK
		  || INT_LIST_VAL (ps) == ENTRY_BLOCK)
		break;

	      if (TEST_BIT (old_changed, INT_LIST_VAL (ps))
		  || TEST_BIT (new_changed, INT_LIST_VAL (ps)))
		break;
	    }

	  if (!ps)
	    continue;

	  if (bb != 0)
	    sbitmap_union_of_predecessors (earlyin[bb], earlyout,
					   bb, s_preds);
	  sbitmap_not (temp_bitmap, transp[bb]);
	  if (sbitmap_union_of_diff (earlyout[bb], temp_bitmap,
				     earlyin[bb], antin[bb]))
	    {
	      changed = 1;
	      SET_BIT (new_changed, bb);
	    }
	}
      sbitmap_copy (old_changed, new_changed);
      passes++;
    }
  free (old_changed);
  free (new_changed);
  free (temp_bitmap);
}

/* Compute expression delayedness at entrance and exit of each block.

   From Advanced Compiler Design and Implementation pp411.

   An expression is delayed at the entrance to BB if it is anticipatable
   and earliest at that point and if all subsequent computations of
   the expression are in block BB.   */

static void
compute_delayinout (n_blocks, n_exprs, s_preds, antloc,
		    antin, earlyin, delayin, delayout)
     int n_blocks;
     int n_exprs;
     int_list_ptr *s_preds;
     sbitmap *antloc;
     sbitmap *antin;
     sbitmap *earlyin;
     sbitmap *delayin;
     sbitmap *delayout;
{
  int bb, changed, passes;
  sbitmap *anti_and_early;
  sbitmap temp_bitmap;

  temp_bitmap = sbitmap_alloc (n_exprs);

  /* This is constant throughout the flow equations below, so compute
     it once to save time.  */
  anti_and_early = sbitmap_vector_alloc (n_blocks, n_exprs);
  for (bb = 0; bb < n_blocks; bb++)
    sbitmap_a_and_b (anti_and_early[bb], antin[bb], earlyin[bb]);
  
  sbitmap_vector_zero (delayout, n_blocks);
  sbitmap_copy (delayin[0], anti_and_early[0]);

  passes = 0;
  changed = 1;
  while (changed)
    {
      changed = 0;
      for (bb = 0; bb < n_blocks; bb++)
	{
	  if (bb != 0)
	    {
	      sbitmap_intersect_of_predecessors (temp_bitmap, delayout,
						 bb, s_preds);
	      changed |= sbitmap_a_or_b (delayin[bb],
					 anti_and_early[bb],
					 temp_bitmap);
	    }
	  sbitmap_not (temp_bitmap, antloc[bb]);
	  changed |= sbitmap_a_and_b (delayout[bb],
				      temp_bitmap,
				      delayin[bb]);
	}
      passes++;
    }

  /* We're done with this, so go ahead and free it's memory now instead
     of waiting until the end of pre.  */
  free (anti_and_early);
  free (temp_bitmap);
}

/* Compute latestness.

   From Advanced Compiler Design and Implementation pp412.

   An expression is latest at the entrance to block BB if that is an optimal
   point for computing the expression and if on every path from block BB's
   entrance to the exit block, any optimal computation point for the 
   expression occurs after one of the points at which the expression was
   computed in the original flowgraph.  */

static void
compute_latein (n_blocks, n_exprs, s_succs, antloc, delayin, latein)
     int n_blocks;
     int n_exprs;
     int_list_ptr *s_succs;
     sbitmap *antloc;
     sbitmap *delayin;
     sbitmap *latein;
{
  int bb;
  sbitmap temp_bitmap;

  temp_bitmap = sbitmap_alloc (n_exprs);

  for (bb = 0; bb < n_blocks; bb++)
    {
      /* The last block is succeeded only by the exit block; therefore,
	 temp_bitmap will not be set by the following call!  */
      if (bb == n_blocks - 1)
	{
          sbitmap_intersect_of_successors (temp_bitmap, delayin,
				           bb, s_succs);
	  sbitmap_not (temp_bitmap, temp_bitmap);
	}
      else
	sbitmap_ones (temp_bitmap);
      sbitmap_a_and_b_or_c (latein[bb], delayin[bb],
			    antloc[bb], temp_bitmap);
    }
  free (temp_bitmap);
}

/* Compute isolated.

   From Advanced Compiler Design and Implementation pp413.

   A computationally optimal placement for the evaluation of an expression
   is defined to be isolated if and only if on every path from a successor
   of the block in which it is computed to the exit block, every original
   computation of the expression is preceded by the optimal placement point.  */

static void
compute_isoinout (n_blocks, s_succs, antloc, latein, isoin, isoout)
     int n_blocks;
     int_list_ptr *s_succs;
     sbitmap *antloc;
     sbitmap *latein;
     sbitmap *isoin;
     sbitmap *isoout;
{
  int bb, changed, passes;

  sbitmap_vector_zero (isoin, n_blocks);
  sbitmap_zero (isoout[n_blocks - 1]);

  passes = 0;
  changed = 1;
  while (changed)
    {
      changed = 0;
      for (bb = n_blocks - 1; bb >= 0; bb--)
	{
	  if (bb != n_blocks - 1)
	    sbitmap_intersect_of_successors (isoout[bb], isoin,
					     bb, s_succs);
	  changed |= sbitmap_union_of_diff (isoin[bb], latein[bb],
					    isoout[bb], antloc[bb]);
	}
      passes++;
    }
}

/* Compute the set of expressions which have optimal computational points
   in each basic block.  This is the set of expressions that are latest, but
   that are not isolated in the block.  */

static void
compute_optimal (n_blocks, latein, isoout, optimal)
     int n_blocks;
     sbitmap *latein;
     sbitmap *isoout;
     sbitmap *optimal;
{
  int bb;

  for (bb = 0; bb < n_blocks; bb++)
    sbitmap_difference (optimal[bb], latein[bb], isoout[bb]);
}

/* Compute the set of expressions that are redundant in a block.  They are
   the expressions that are used in the block and that are neither isolated
   or latest.  */

static void
compute_redundant (n_blocks, n_exprs, antloc, latein, isoout, redundant)
     int n_blocks;
     int n_exprs;
     sbitmap *antloc;
     sbitmap *latein;
     sbitmap *isoout;
     sbitmap *redundant;
{
  int bb;
  sbitmap temp_bitmap;

  temp_bitmap = sbitmap_alloc (n_exprs);

  for (bb = 0; bb < n_blocks; bb++)
    {
      sbitmap_a_or_b (temp_bitmap, latein[bb], isoout[bb]);
      sbitmap_difference (redundant[bb], antloc[bb], temp_bitmap);
    }
  free (temp_bitmap);
}

/* Compute expression availability at entrance and exit of each block.  */

static void
compute_avinout (n_blocks, s_preds, avloc, transp, avin, avout)
     int n_blocks;
     int_list_ptr *s_preds;
     sbitmap *avloc;
     sbitmap *transp;
     sbitmap *avin;
     sbitmap *avout;
{
  int bb, changed, passes;

  sbitmap_zero (avin[0]);
  sbitmap_vector_ones (avout, n_blocks);

  passes = 0;
  changed = 1;
  while (changed)
    {
      changed = 0;
      for (bb = 0; bb < n_blocks; bb++)
	{
	  if (bb != 0)
	    sbitmap_intersect_of_predecessors (avin[bb], avout,
					       bb, s_preds);
	  changed |= sbitmap_a_or_b_and_c (avout[bb], avloc[bb],
					   transp[bb], avin[bb]);
	}
      passes++;
    }
}

/* Compute expression latestness.

   This is effectively the same as earliestness computed on the reverse
   flow graph.  */

static void
compute_fartherinout (n_blocks, n_exprs, s_succs,
		      transp, avout, fartherin, fartherout)
     int n_blocks;
     int n_exprs;
     int_list_ptr *s_succs;
     sbitmap *transp;
     sbitmap *avout;
     sbitmap *fartherin;
     sbitmap *fartherout;
{
  int bb, changed, passes;
  sbitmap temp_bitmap;

  temp_bitmap = sbitmap_alloc (n_exprs);

  sbitmap_vector_zero (fartherin, n_blocks);
  sbitmap_ones (fartherout[n_blocks - 1]);

  passes = 0;
  changed = 1;
  while (changed)
    {
      changed = 0;
      for (bb = n_blocks - 1; bb >= 0; bb--)
	{
	  if (bb != n_blocks - 1)
	    sbitmap_union_of_successors (fartherout[bb], fartherin,
					 bb, s_succs);
	  sbitmap_not (temp_bitmap, transp[bb]);
	  changed |= sbitmap_union_of_diff (fartherin[bb], temp_bitmap,
					    fartherout[bb], avout[bb]);
	}
      passes++;
    }

  free (temp_bitmap);
}

/* Compute expression earlierness at entrance and exit of each block.

   This is effectively the same as delayedness computed on the reverse
   flow graph.  */

static void
compute_earlierinout (n_blocks, n_exprs, s_succs, avloc,
		      avout, fartherout, earlierin, earlierout)
     int n_blocks;
     int n_exprs;
     int_list_ptr *s_succs;
     sbitmap *avloc;
     sbitmap *avout;
     sbitmap *fartherout;
     sbitmap *earlierin;
     sbitmap *earlierout;
{
  int bb, changed, passes;
  sbitmap *av_and_farther;
  sbitmap temp_bitmap;

  temp_bitmap = sbitmap_alloc (n_exprs);

  /* This is constant throughout the flow equations below, so compute
     it once to save time.  */
  av_and_farther = sbitmap_vector_alloc (n_blocks, n_exprs);
  for (bb = 0; bb < n_blocks; bb++)
    sbitmap_a_and_b (av_and_farther[bb], avout[bb], fartherout[bb]);
  
  sbitmap_vector_zero (earlierin, n_blocks);
  sbitmap_copy (earlierout[n_blocks - 1], av_and_farther[n_blocks - 1]);

  passes = 0;
  changed = 1;
  while (changed)
    {
      changed = 0;
      for (bb = n_blocks - 1; bb >= 0; bb--)
	{
	  if (bb != n_blocks - 1)
	    {
	      sbitmap_intersect_of_successors (temp_bitmap, earlierin,
					       bb, s_succs);
	      changed |= sbitmap_a_or_b (earlierout[bb],
					 av_and_farther[bb],
					 temp_bitmap);
	    }
	  sbitmap_not (temp_bitmap, avloc[bb]);
	  changed |= sbitmap_a_and_b (earlierin[bb],
				      temp_bitmap,
				      earlierout[bb]);
	}
      passes++;
    }

  /* We're done with this, so go ahead and free it's memory now instead
     of waiting until the end of pre.  */
  free (av_and_farther);
  free (temp_bitmap);
}

/* Compute firstness. 

   This is effectively the same as latestness computed on the reverse
   flow graph.  */

static void
compute_firstout (n_blocks, n_exprs, s_preds, avloc, earlierout, firstout)
     int n_blocks;
     int n_exprs;
     int_list_ptr *s_preds;
     sbitmap *avloc;
     sbitmap *earlierout;
     sbitmap *firstout;
{
  int bb;
  sbitmap temp_bitmap;

  temp_bitmap = sbitmap_alloc (n_exprs);

  for (bb = 0; bb < n_blocks; bb++)
    {
      /* The first block is preceded only by the entry block; therefore,
	 temp_bitmap will not be set by the following call!  */
      if (bb != 0)
	{
	  sbitmap_intersect_of_predecessors (temp_bitmap, earlierout,
					     bb, s_preds);
	  sbitmap_not (temp_bitmap, temp_bitmap);
	}
      else
	{
	  sbitmap_ones (temp_bitmap);
	}
      sbitmap_a_and_b_or_c (firstout[bb], earlierout[bb],
			    avloc[bb], temp_bitmap);
    }
  free (temp_bitmap);
}

/* Compute reverse isolated.

   This is effectively the same as isolatedness computed on the reverse
   flow graph.  */

static void
compute_rev_isoinout (n_blocks, s_preds, avloc, firstout,
		      rev_isoin, rev_isoout)
     int n_blocks;
     int_list_ptr *s_preds;
     sbitmap *avloc;
     sbitmap *firstout;
     sbitmap *rev_isoin;
     sbitmap *rev_isoout;
{
  int bb, changed, passes;

  sbitmap_vector_zero (rev_isoout, n_blocks);
  sbitmap_zero (rev_isoin[0]);

  passes = 0;
  changed = 1;
  while (changed)
    {
      changed = 0;
      for (bb = 0; bb < n_blocks; bb++)
	{
	  if (bb != 0)
	    sbitmap_intersect_of_predecessors (rev_isoin[bb], rev_isoout,
					       bb, s_preds);
	  changed |= sbitmap_union_of_diff (rev_isoout[bb], firstout[bb],
					    rev_isoin[bb], avloc[bb]);
	}
      passes++;
    }
}
