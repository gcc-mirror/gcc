/* Loop unrolling and peeling.
   Copyright (C) 2002, 2003, 2004 Free Software Foundation, Inc.

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
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "cfgloop.h"
#include "cfglayout.h"
#include "params.h"
#include "output.h"
#include "expr.h"
/* We need to use the macro exact_log2. */
#include "toplev.h"

/* This pass performs loop unrolling and peeling.  We only perform these
   optimizations on innermost loops (with single exception) because
   the impact on performance is greatest here, and we want to avoid
   unnecessary code size growth.  The gain is caused by greater sequentiality
   of code, better code to optimize for further passes and in some cases
   by fewer testings of exit conditions.  The main problem is code growth,
   that impacts performance negatively due to effect of caches.

   What we do:

   -- complete peeling of once-rolling loops; this is the above mentioned
      exception, as this causes loop to be cancelled completely and
      does not cause code growth
   -- complete peeling of loops that roll (small) constant times.
   -- simple peeling of first iterations of loops that do not roll much
      (according to profile feedback)
   -- unrolling of loops that roll constant times; this is almost always
      win, as we get rid of exit condition tests.
   -- unrolling of loops that roll number of times that we can compute
      in runtime; we also get rid of exit condition tests here, but there
      is the extra expense for calculating the number of iterations
   -- simple unrolling of remaining loops; this is performed only if we
      are asked to, as the gain is questionable in this case and often
      it may even slow down the code
   For more detailed descriptions of each of those, see comments at
   appropriate function below.

   There is a lot of parameters (defined and described in params.def) that
   control how much we unroll/peel.

   ??? A great problem is that we don't have a good way how to determine
   how many times we should unroll the loop; the experiments I have made
   showed that this choice may affect performance in order of several %.
   */

static void decide_unrolling_and_peeling (struct loops *, int);
static void peel_loops_completely (struct loops *, int);
static void decide_peel_simple (struct loop *, int);
static void decide_peel_once_rolling (struct loop *, int);
static void decide_peel_completely (struct loop *, int);
static void decide_unroll_stupid (struct loop *, int);
static void decide_unroll_constant_iterations (struct loop *, int);
static void decide_unroll_runtime_iterations (struct loop *, int);
static void peel_loop_simple (struct loops *, struct loop *);
static void peel_loop_completely (struct loops *, struct loop *);
static void unroll_loop_stupid (struct loops *, struct loop *);
static void unroll_loop_constant_iterations (struct loops *, struct loop *);
static void unroll_loop_runtime_iterations (struct loops *, struct loop *);
static void expand_bct (edge, int);
static bool discard_increment (struct loop *);

/* Unroll and/or peel (depending on FLAGS) LOOPS.  */
void
unroll_and_peel_loops (struct loops *loops, int flags)
{
  struct loop *loop, *next;
  int check;

  /* First perform complete loop peeling (it is almost surely a win,
     and affects parameters for further decision a lot).  */
  peel_loops_completely (loops, flags);

  /* Now decide rest of unrolling and peeling.  */
  decide_unrolling_and_peeling (loops, flags);

  loop = loops->tree_root;
  while (loop->inner)
    loop = loop->inner;

  /* Scan the loops, inner ones first.  */
  while (loop != loops->tree_root)
    {
      if (loop->next)
	{
	  next = loop->next;
	  while (next->inner)
	    next = next->inner;
	}
      else
	next = loop->outer;

      check = 1;
      /* And perform the appropriate transformations.  */
      switch (loop->lpt_decision.decision)
	{
	case LPT_PEEL_COMPLETELY:
	  /* Already done.  */
	  abort ();
	case LPT_PEEL_SIMPLE:
	  peel_loop_simple (loops, loop);
	  break;
	case LPT_UNROLL_CONSTANT:
	  unroll_loop_constant_iterations (loops, loop);
	  break;
	case LPT_UNROLL_RUNTIME:
	  unroll_loop_runtime_iterations (loops, loop);
	  break;
	case LPT_UNROLL_STUPID:
	  unroll_loop_stupid (loops, loop);
	  break;
	case LPT_NONE:
	  check = 0;
	  break;
	default:
	  abort ();
	}
      if (check)
	{
#ifdef ENABLE_CHECKING
	  verify_dominators (CDI_DOMINATORS);
	  verify_loop_structure (loops);
#endif
	}
      loop = next;
    }
}

/* Check whether to peel LOOPS (depending on FLAGS) completely and do so.  */
static void
peel_loops_completely (struct loops *loops, int flags)
{
  struct loop *loop, *next;

  loop = loops->tree_root;
  while (loop->inner)
    loop = loop->inner;

  while (loop != loops->tree_root)
    {
      if (loop->next)
	{
	  next = loop->next;
	  while (next->inner)
	    next = next->inner;
	}
      else
	next = loop->outer;

      loop->lpt_decision.decision = LPT_NONE;
      loop->has_desc = 0;

      if (rtl_dump_file)
	fprintf (rtl_dump_file, ";; Considering loop %d for complete peeling\n",
		 loop->num);

      loop->ninsns = num_loop_insns (loop);

      decide_peel_once_rolling (loop, flags);
      if (loop->lpt_decision.decision == LPT_NONE)
	decide_peel_completely (loop, flags);

      if (loop->lpt_decision.decision == LPT_PEEL_COMPLETELY)
	{
	  peel_loop_completely (loops, loop);
#ifdef ENABLE_CHECKING
	  verify_dominators (CDI_DOMINATORS);
	  verify_loop_structure (loops);
#endif
	}
      loop = next;
    }
}

/* Decide whether unroll or peel LOOPS (depending on FLAGS) and how much.  */
static void
decide_unrolling_and_peeling (struct loops *loops, int flags)
{
  struct loop *loop = loops->tree_root, *next;

  while (loop->inner)
    loop = loop->inner;

  /* Scan the loops, inner ones first.  */
  while (loop != loops->tree_root)
    {
      if (loop->next)
	{
	  next = loop->next;
	  while (next->inner)
	    next = next->inner;
	}
      else
	next = loop->outer;

      loop->lpt_decision.decision = LPT_NONE;

      if (rtl_dump_file)
	fprintf (rtl_dump_file, ";; Considering loop %d\n", loop->num);

      /* Do not peel cold areas.  */
      if (!maybe_hot_bb_p (loop->header))
	{
	  if (rtl_dump_file)
	    fprintf (rtl_dump_file, ";; Not considering loop, cold area\n");
	  loop = next;
	  continue;
	}

      /* Can the loop be manipulated?  */
      if (!can_duplicate_loop_p (loop))
	{
	  if (rtl_dump_file)
	    fprintf (rtl_dump_file,
		     ";; Not considering loop, cannot duplicate\n");
	  loop = next;
	  continue;
	}

      /* Skip non-innermost loops.  */
      if (loop->inner)
	{
	  if (rtl_dump_file)
	    fprintf (rtl_dump_file, ";; Not considering loop, is not innermost\n");
	  loop = next;
	  continue;
	}

      loop->ninsns = num_loop_insns (loop);
      loop->av_ninsns = average_num_loop_insns (loop);

      /* Try transformations one by one in decreasing order of
	 priority.  */

      decide_unroll_constant_iterations (loop, flags);
      if (loop->lpt_decision.decision == LPT_NONE)
	decide_unroll_runtime_iterations (loop, flags);
      if (loop->lpt_decision.decision == LPT_NONE)
	decide_unroll_stupid (loop, flags);
      if (loop->lpt_decision.decision == LPT_NONE)
	decide_peel_simple (loop, flags);

      loop = next;
    }
}

/* Decide whether the LOOP is once rolling and suitable for complete
   peeling.  */
static void
decide_peel_once_rolling (struct loop *loop, int flags ATTRIBUTE_UNUSED)
{
  if (rtl_dump_file)
    fprintf (rtl_dump_file, ";; Considering peeling once rolling loop\n");

  /* Is the loop small enough?  */
  if ((unsigned) PARAM_VALUE (PARAM_MAX_ONCE_PEELED_INSNS) < loop->ninsns)
    {
      if (rtl_dump_file)
	fprintf (rtl_dump_file, ";; Not considering loop, is too big\n");
      return;
    }

  /* Check for simple loops.  */
  loop->simple = simple_loop_p (loop, &loop->desc);
  loop->has_desc = 1;

  /* Check number of iterations.  */
  if (!loop->simple || !loop->desc.const_iter || loop->desc.niter != 0)
    {
      if (rtl_dump_file)
	fprintf (rtl_dump_file, ";; Unable to prove that the loop rolls exactly once\n");
      return;
    }

  /* Success.  */
  if (rtl_dump_file)
    fprintf (rtl_dump_file, ";; Decided to peel exactly once rolling loop\n");
  loop->lpt_decision.decision = LPT_PEEL_COMPLETELY;
}

/* Decide whether the LOOP is suitable for complete peeling.  */
static void
decide_peel_completely (struct loop *loop, int flags ATTRIBUTE_UNUSED)
{
  unsigned npeel;

  if (rtl_dump_file)
    fprintf (rtl_dump_file, ";; Considering peeling completely\n");

  /* Skip non-innermost loops.  */
  if (loop->inner)
    {
      if (rtl_dump_file)
	fprintf (rtl_dump_file, ";; Not considering loop, is not innermost\n");
      return;
    }

  /* Do not peel cold areas.  */
  if (!maybe_hot_bb_p (loop->header))
    {
      if (rtl_dump_file)
	fprintf (rtl_dump_file, ";; Not considering loop, cold area\n");
      return;
    }

  /* Can the loop be manipulated?  */
  if (!can_duplicate_loop_p (loop))
    {
      if (rtl_dump_file)
	fprintf (rtl_dump_file,
		 ";; Not considering loop, cannot duplicate\n");
      return;
    }

  /* npeel = number of iterations to peel.  */
  npeel = PARAM_VALUE (PARAM_MAX_COMPLETELY_PEELED_INSNS) / loop->ninsns;
  if (npeel > (unsigned) PARAM_VALUE (PARAM_MAX_COMPLETELY_PEEL_TIMES))
    npeel = PARAM_VALUE (PARAM_MAX_COMPLETELY_PEEL_TIMES);

  /* Is the loop small enough?  */
  if (!npeel)
    {
      if (rtl_dump_file)
	fprintf (rtl_dump_file, ";; Not considering loop, is too big\n");
      return;
    }

  /* Check for simple loops.  */
  if (!loop->has_desc)
    {
      loop->simple = simple_loop_p (loop, &loop->desc);
      loop->has_desc = 1;
    }

  /* Check number of iterations.  */
  if (!loop->simple || !loop->desc.const_iter)
    {
      if (rtl_dump_file)
	fprintf (rtl_dump_file, ";; Unable to prove that the loop iterates constant times\n");
      return;
    }

  if (loop->desc.niter > npeel - 1)
    {
      if (rtl_dump_file)
	{
	  fprintf (rtl_dump_file, ";; Not peeling loop completely, rolls too much (");
	  fprintf (rtl_dump_file, HOST_WIDEST_INT_PRINT_DEC,(HOST_WIDEST_INT) loop->desc.niter);
	  fprintf (rtl_dump_file, " iterations > %d [maximum peelings])\n", npeel);
	}
      return;
    }

  /* Success.  */
  if (rtl_dump_file)
    fprintf (rtl_dump_file, ";; Decided to peel loop completely\n");
  loop->lpt_decision.decision = LPT_PEEL_COMPLETELY;
}

/* Peel all iterations of LOOP, remove exit edges and cancel the loop
   completely.  The transformation done:

   for (i = 0; i < 4; i++)
     body;

   ==>

   i = 0;
   body; i++;
   body; i++;
   body; i++;
   body; i++;
   */
static void
peel_loop_completely (struct loops *loops, struct loop *loop)
{
  sbitmap wont_exit;
  unsigned HOST_WIDE_INT npeel;
  unsigned n_remove_edges, i;
  edge *remove_edges;
  struct loop_desc *desc = &loop->desc;
  bool discard_inc = false;
  bool is_bct;

  if ((is_bct = is_bct_cond (BB_END (loop->desc.out_edge->src))))
    discard_inc = discard_increment (loop);

  npeel = desc->niter;

  if (npeel)
    {
      wont_exit = sbitmap_alloc (npeel + 1);
      sbitmap_ones (wont_exit);
      RESET_BIT (wont_exit, 0);
      if (desc->may_be_zero)
	RESET_BIT (wont_exit, 1);

      remove_edges = xcalloc (npeel, sizeof (edge));
      n_remove_edges = 0;

      if (!duplicate_loop_to_header_edge (loop, loop_preheader_edge (loop),
		loops, npeel,
		wont_exit, desc->out_edge, remove_edges, &n_remove_edges,
		DLTHE_FLAG_UPDATE_FREQ))
	abort ();

      free (wont_exit);

      /* Expand the branch and count.  */
      if (is_bct)
	for (i = 0; i < n_remove_edges; i++)
	  expand_bct (remove_edges[i], discard_inc);

      /* Remove the exit edges.  */
      for (i = 0; i < n_remove_edges; i++)
	remove_path (loops, remove_edges[i]);
      free (remove_edges);
    }

  /* Expand the branch and count.  */
  if (is_bct)
    expand_bct (desc->in_edge, discard_inc);

  /* Now remove the unreachable part of the last iteration and cancel
     the loop.  */
  remove_path (loops, desc->in_edge);

  if (rtl_dump_file)
    fprintf (rtl_dump_file, ";; Peeled loop completely, %d times\n", (int) npeel);
}

/* Decide whether to unroll LOOP iterating constant number of times and how much.  */
static void
decide_unroll_constant_iterations (struct loop *loop, int flags)
{
  unsigned nunroll, nunroll_by_av, best_copies, best_unroll = -1, n_copies, i;

  if (!(flags & UAP_UNROLL))
    {
      /* We were not asked to, just return back silently.  */
      return;
    }

  if (rtl_dump_file)
    fprintf (rtl_dump_file, ";; Considering unrolling loop with constant number of iterations\n");

  /* nunroll = total number of copies of the original loop body in
     unrolled loop (i.e. if it is 2, we have to duplicate loop body once.  */
  nunroll = PARAM_VALUE (PARAM_MAX_UNROLLED_INSNS) / loop->ninsns;
  nunroll_by_av = PARAM_VALUE (PARAM_MAX_AVERAGE_UNROLLED_INSNS) / loop->av_ninsns;
  if (nunroll > nunroll_by_av)
    nunroll = nunroll_by_av;
  if (nunroll > (unsigned) PARAM_VALUE (PARAM_MAX_UNROLL_TIMES))
    nunroll = PARAM_VALUE (PARAM_MAX_UNROLL_TIMES);

  /* Skip big loops.  */
  if (nunroll <= 1)
    {
      if (rtl_dump_file)
	fprintf (rtl_dump_file, ";; Not considering loop, is too big\n");
      return;
    }

  /* Check for simple loops.  */
  if (!loop->has_desc)
    {
      loop->simple = simple_loop_p (loop, &loop->desc);
      loop->has_desc = 1;
    }

  /* Check number of iterations.  */
  if (!loop->simple || !loop->desc.const_iter)
    {
      if (rtl_dump_file)
	fprintf (rtl_dump_file, ";; Unable to prove that the loop iterates constant times\n");
      return;
    }

  /* Check whether the loop rolls enough to consider.  */
  if (loop->desc.niter < 2 * nunroll)
    {
      if (rtl_dump_file)
	fprintf (rtl_dump_file, ";; Not unrolling loop, doesn't roll\n");
      return;
    }

  /* Success; now compute number of iterations to unroll.  We alter
     nunroll so that as few as possible copies of loop body are
     necessary, while still not decreasing the number of unrollings
     too much (at most by 1).  */
  best_copies = 2 * nunroll + 10;

  i = 2 * nunroll + 2;
  if ((unsigned) i - 1 >= loop->desc.niter)
    i = loop->desc.niter - 2;

  for (; i >= nunroll - 1; i--)
    {
      unsigned exit_mod = loop->desc.niter % (i + 1);

      if (loop->desc.postincr)
	n_copies = exit_mod + i + 1;
      else if (exit_mod != (unsigned) i || loop->desc.may_be_zero)
	n_copies = exit_mod + i + 2;
      else
	n_copies = i + 1;

      if (n_copies < best_copies)
	{
	  best_copies = n_copies;
	  best_unroll = i;
	}
    }

  if (rtl_dump_file)
    fprintf (rtl_dump_file, ";; max_unroll %d (%d copies, initial %d).\n",
	     best_unroll + 1, best_copies, nunroll);

  loop->lpt_decision.decision = LPT_UNROLL_CONSTANT;
  loop->lpt_decision.times = best_unroll;
}

/* Unroll LOOP with constant number of iterations LOOP->LPT_DECISION.TIMES + 1
   times.  The transformation does this:

   for (i = 0; i < 102; i++)
     body;

   ==>

   i = 0;
   body; i++;
   body; i++;
   while (i < 102)
     {
       body; i++;
       body; i++;
       body; i++;
       body; i++;
     }
  */
static void
unroll_loop_constant_iterations (struct loops *loops, struct loop *loop)
{
  unsigned HOST_WIDE_INT niter;
  unsigned exit_mod;
  sbitmap wont_exit;
  unsigned n_remove_edges, i;
  edge *remove_edges;
  unsigned max_unroll = loop->lpt_decision.times;
  struct loop_desc *desc = &loop->desc;
  bool discard_inc = false;
  bool is_bct;

  niter = desc->niter;

  if (niter <= (unsigned) max_unroll + 1)
    abort ();  /* Should not get here (such loop should be peeled instead).  */

  exit_mod = niter % (max_unroll + 1);

  wont_exit = sbitmap_alloc (max_unroll + 1);
  sbitmap_ones (wont_exit);

  remove_edges = xcalloc (max_unroll + exit_mod + 1, sizeof (edge));
  n_remove_edges = 0;

  /* For a loop ending with a branch and count for which the increment
     of the count register will be discarded, adjust the initialization of
     the count register.  */
  if ((is_bct = is_bct_cond (BB_END (desc->out_edge->src)))
      && (discard_inc = discard_increment (loop)))
    { 
      rtx ini_var;
     
      rtx init_code;
      int n_peel, new_bct_value;

      /* Get expression for number of iterations.  */
      start_sequence ();
      
      n_peel = (niter+1) % (max_unroll+1);
      new_bct_value = (niter+1 - n_peel) / (max_unroll+1) ;
      ini_var = GEN_INT (new_bct_value);        
      
      emit_move_insn (desc->var, ini_var);
      init_code = get_insns ();
      end_sequence ();

      loop_split_edge_with (loop_preheader_edge (loop), init_code); 
    }    

  if (desc->postincr)
    {
      /* Counter is incremented after the exit test; leave exit test
	 in the first copy, so that the loops that start with test
	 of exit condition have continuous body after unrolling.  */

      if (rtl_dump_file)
	fprintf (rtl_dump_file, ";; Condition on beginning of loop.\n");

      /* Peel exit_mod iterations.  */
      RESET_BIT (wont_exit, 0);
      if (desc->may_be_zero)
	RESET_BIT (wont_exit, 1);

      if (exit_mod
	  && !duplicate_loop_to_header_edge (loop, loop_preheader_edge (loop),
		loops, exit_mod,
		wont_exit, desc->out_edge, remove_edges, &n_remove_edges,
		DLTHE_FLAG_UPDATE_FREQ))
	abort ();

      SET_BIT (wont_exit, 1);
    }
  else
    {
      /* Leave exit test in last copy, for the same reason as above if
	 the loop tests the condition at the end of loop body.  */

      if (rtl_dump_file)
	fprintf (rtl_dump_file, ";; Condition on end of loop.\n");

      /* We know that niter >= max_unroll + 2; so we do not need to care of
	 case when we would exit before reaching the loop.  So just peel
	 exit_mod + 1 iterations.
	 */
      if (exit_mod != (unsigned) max_unroll || desc->may_be_zero)
	{
	  RESET_BIT (wont_exit, 0);
	  if (desc->may_be_zero)
	    RESET_BIT (wont_exit, 1);

	  if (!duplicate_loop_to_header_edge (loop, loop_preheader_edge (loop),
		loops, exit_mod + 1,
		wont_exit, desc->out_edge, remove_edges, &n_remove_edges,
		DLTHE_FLAG_UPDATE_FREQ))
	    abort ();

	  SET_BIT (wont_exit, 0);
	  SET_BIT (wont_exit, 1);
	}

      RESET_BIT (wont_exit, max_unroll);
    }

  /* Now unroll the loop.  */
  if (!duplicate_loop_to_header_edge (loop, loop_latch_edge (loop),
		loops, max_unroll,
		wont_exit, desc->out_edge, remove_edges, &n_remove_edges,
		DLTHE_FLAG_UPDATE_FREQ))
    abort ();

  free (wont_exit);

  /* Expand the branch and count.  */
  if (is_bct)
    for (i = 0; i < n_remove_edges; i++)
      expand_bct (remove_edges[i], discard_inc);

  /* Remove the edges.  */
  for (i = 0; i < n_remove_edges; i++)
    remove_path (loops, remove_edges[i]);
  free (remove_edges);

  if (rtl_dump_file)
    fprintf (rtl_dump_file, ";; Unrolled loop %d times, constant # of iterations %i insns\n",max_unroll, num_loop_insns (loop));
}

/* Decide whether to unroll LOOP iterating runtime computable number of times
   and how much.  */
static void
decide_unroll_runtime_iterations (struct loop *loop, int flags)
{
  unsigned nunroll, nunroll_by_av, i;

  if (!(flags & UAP_UNROLL))
    {
      /* We were not asked to, just return back silently.  */
      return;
    }

  if (rtl_dump_file)
    fprintf (rtl_dump_file, ";; Considering unrolling loop with runtime computable number of iterations\n");

  /* nunroll = total number of copies of the original loop body in
     unrolled loop (i.e. if it is 2, we have to duplicate loop body once.  */
  nunroll = PARAM_VALUE (PARAM_MAX_UNROLLED_INSNS) / loop->ninsns;
  nunroll_by_av = PARAM_VALUE (PARAM_MAX_AVERAGE_UNROLLED_INSNS) / loop->av_ninsns;
  if (nunroll > nunroll_by_av)
    nunroll = nunroll_by_av;
  if (nunroll > (unsigned) PARAM_VALUE (PARAM_MAX_UNROLL_TIMES))
    nunroll = PARAM_VALUE (PARAM_MAX_UNROLL_TIMES);

  /* Skip big loops.  */
  if (nunroll <= 1)
    {
      if (rtl_dump_file)
	fprintf (rtl_dump_file, ";; Not considering loop, is too big\n");
      return;
    }

  /* Check for simple loops.  */
  if (!loop->has_desc)
    {
      loop->simple = simple_loop_p (loop, &loop->desc);
      loop->has_desc = 1;
    }

  /* Check simpleness.  */
  if (!loop->simple)
    {
      if (rtl_dump_file)
	fprintf (rtl_dump_file, ";; Unable to prove that the number of iterations can be counted in runtime\n");
      return;
    }

  if (loop->desc.const_iter)
    {
      if (rtl_dump_file)
	fprintf (rtl_dump_file, ";; Loop iterates constant times\n");
      return;
    }

  /* If we have profile feedback, check whether the loop rolls.  */
  if (loop->header->count && expected_loop_iterations (loop) < 2 * nunroll)
    {
      if (rtl_dump_file)
	fprintf (rtl_dump_file, ";; Not unrolling loop, doesn't roll\n");
      return;
    }

  /* Success; now force nunroll to be power of 2, as we are unable to
     cope with overflows in computation of number of iterations.  */
  for (i = 1; 2 * i <= nunroll; i *= 2);

  loop->lpt_decision.decision = LPT_UNROLL_RUNTIME;
  loop->lpt_decision.times = i - 1;
}

/* Unroll LOOP for that we are able to count number of iterations in runtime
   LOOP->LPT_DECISION.TIMES + 1 times.  The transformation does this (with some
   extra care for case n < 0):

   for (i = 0; i < n; i++)
     body;

   ==>

   i = 0;
   mod = n % 4;

   switch (mod)
     {
       case 3:
         body; i++;
       case 2:
         body; i++;
       case 1:
         body; i++;
       case 0: ;
     }

   while (i < n)
     {
       body; i++;
       body; i++;
       body; i++;
       body; i++;
     }
   */
static void
unroll_loop_runtime_iterations (struct loops *loops, struct loop *loop)
{
  rtx niter, init_code, branch_code, jump, label;
  unsigned i, j, p;
  basic_block preheader, *body, *dom_bbs, swtch, ezc_swtch;
  unsigned n_dom_bbs;
  sbitmap wont_exit;
  int may_exit_copy;
  unsigned n_peel, n_remove_edges;
  edge *remove_edges, e;
  bool extra_zero_check, last_may_exit;
  unsigned max_unroll = loop->lpt_decision.times;
  struct loop_desc *desc = &loop->desc;
  bool discard_inc = false;
  bool is_bct;

  /* Remember blocks whose dominators will have to be updated.  */
  dom_bbs = xcalloc (n_basic_blocks, sizeof (basic_block));
  n_dom_bbs = 0;

  body = get_loop_body (loop);
  for (i = 0; i < loop->num_nodes; i++)
    {
      unsigned nldom;
      basic_block *ldom;

      nldom = get_dominated_by (CDI_DOMINATORS, body[i], &ldom);
      for (j = 0; j < nldom; j++)
	if (!flow_bb_inside_loop_p (loop, ldom[j]))
	  dom_bbs[n_dom_bbs++] = ldom[j];

      free (ldom);
    }
  free (body);

  if (desc->postincr)
    {
      /* Leave exit in first copy (for explanation why see comment in
	 unroll_loop_constant_iterations).  */
      may_exit_copy = 0;
      n_peel = max_unroll - 1;
      extra_zero_check = true;
      last_may_exit = false;
    }
  else
    {
      /* Leave exit in last copy (for explanation why see comment in
	 unroll_loop_constant_iterations).  */
      may_exit_copy = max_unroll;
      n_peel = max_unroll;
      extra_zero_check = false;
      last_may_exit = true;
    }

  /* Get expression for number of iterations.  */
  start_sequence ();
  niter = count_loop_iterations (desc, NULL, NULL);
  if (!niter)
    abort ();
  niter = force_operand (niter, NULL);

  /* Count modulo by ANDing it with max_unroll; we use the fact that
     the number of unrollings is a power of two, and thus this is correct
     even if there is overflow in the computation.  */
  niter = expand_simple_binop (GET_MODE (desc->var), AND,
			       niter,
			       GEN_INT (max_unroll),
			       NULL_RTX, 0, OPTAB_LIB_WIDEN);

  /* For a loop ending with a branch and count for which the increment
     of the count register will be discarded, adjust the initialization of
     the count register.  */
  if ((is_bct = is_bct_cond (BB_END (desc->out_edge->src)))
      && (discard_inc = discard_increment (loop)))
    { 
      rtx count, count2, count_unroll_mod;
      int count_unroll;

      /* start_sequence (); */
                  
      count = count_loop_iterations (desc, NULL, NULL);
      
      count_unroll = loop->lpt_decision.times+1;



      count_unroll_mod =  GEN_INT (exact_log2 (count_unroll));
      count = expand_simple_binop (GET_MODE (desc->var), LSHIFTRT,
				  count, count_unroll_mod,
				  0, 0, OPTAB_LIB_WIDEN);

      count2 = expand_simple_binop (GET_MODE (desc->var), PLUS,
				     count, GEN_INT (2),
				     0, 0, OPTAB_LIB_WIDEN);
     
      emit_move_insn (desc->var, count2);
    }

  init_code = get_insns ();
  end_sequence ();

  /* Precondition the loop.  */
  loop_split_edge_with (loop_preheader_edge (loop), init_code);

  remove_edges = xcalloc (max_unroll + n_peel + 1, sizeof (edge));
  n_remove_edges = 0;

  wont_exit = sbitmap_alloc (max_unroll + 2);

  /* Peel the first copy of loop body (almost always we must leave exit test
     here; the only exception is when we have extra zero check and the number
     of iterations is reliable (i.e. comes out of NE condition).  Also record
     the place of (possible) extra zero check.  */
  sbitmap_zero (wont_exit);
  if (extra_zero_check && desc->cond == NE)
    SET_BIT (wont_exit, 1);
  ezc_swtch = loop_preheader_edge (loop)->src;
  if (!duplicate_loop_to_header_edge (loop, loop_preheader_edge (loop),
		loops, 1,
		wont_exit, desc->out_edge, remove_edges, &n_remove_edges,
		DLTHE_FLAG_UPDATE_FREQ))
    abort ();

  /* Record the place where switch will be built for preconditioning.  */
  swtch = loop_split_edge_with (loop_preheader_edge (loop),
				NULL_RTX);

  for (i = 0; i < n_peel; i++)
    {
      /* Peel the copy.  */
      sbitmap_zero (wont_exit);
      if (i != n_peel - 1 || !last_may_exit)
	SET_BIT (wont_exit, 1);
      if (!duplicate_loop_to_header_edge (loop, loop_preheader_edge (loop),
		loops, 1,
		wont_exit, desc->out_edge, remove_edges, &n_remove_edges,
		DLTHE_FLAG_UPDATE_FREQ))
	abort ();

      /* Create item for switch.  */
      j = n_peel - i - (extra_zero_check ? 0 : 1);
      p = REG_BR_PROB_BASE / (i + 2);

      /* If modulo is zero do not jumo to the header of the unrolled loops.  
         Jump instead to the last branch and count that precedes it.  */
      if (is_bct && discard_inc && (j == 0))
	{
	  basic_block lastbb = loop_preheader_edge(loop)->src;
	  rtx split_after;

          /* Skip dummy basic blocks generated during the unrolling.  */	  
	  while (!is_bct_cond (BB_END (lastbb))) 
	    lastbb = lastbb->pred->src;

	  split_after = PREV_INSN (BB_END (lastbb));

	  preheader = split_loop_bb (lastbb , split_after)->dest;
	}
      else
	preheader = loop_split_edge_with (loop_preheader_edge (loop),
					  NULL_RTX);
      label = block_label (preheader);
      start_sequence ();
      do_compare_rtx_and_jump (copy_rtx (niter), GEN_INT (j), EQ, 0,
			       GET_MODE (desc->var), NULL_RTX, NULL_RTX,
			       label);
      jump = get_last_insn ();
      JUMP_LABEL (jump) = label;
      REG_NOTES (jump)
	      = gen_rtx_EXPR_LIST (REG_BR_PROB,
				   GEN_INT (p), REG_NOTES (jump));

      LABEL_NUSES (label)++;
      branch_code = get_insns ();
      end_sequence ();

      swtch = loop_split_edge_with (swtch->pred, branch_code);
      set_immediate_dominator (CDI_DOMINATORS, preheader, swtch);
      swtch->succ->probability = REG_BR_PROB_BASE - p;
      e = make_edge (swtch, preheader,
		     swtch->succ->flags & EDGE_IRREDUCIBLE_LOOP);
      e->probability = p;
    }

  if (extra_zero_check)
    {
      /* Add branch for zero iterations.  */
      p = REG_BR_PROB_BASE / (max_unroll + 1);
      swtch = ezc_swtch;
      preheader = loop_split_edge_with (loop_preheader_edge (loop), NULL_RTX);
      label = block_label (preheader);
      start_sequence ();
      do_compare_rtx_and_jump (copy_rtx (niter), const0_rtx, EQ, 0,
			       GET_MODE (desc->var), NULL_RTX, NULL_RTX,
			       label);
      jump = get_last_insn ();
      JUMP_LABEL (jump) = label;
      REG_NOTES (jump)
	      = gen_rtx_EXPR_LIST (REG_BR_PROB,
				   GEN_INT (p), REG_NOTES (jump));

      LABEL_NUSES (label)++;
      branch_code = get_insns ();
      end_sequence ();

      swtch = loop_split_edge_with (swtch->succ, branch_code);
      set_immediate_dominator (CDI_DOMINATORS, preheader, swtch);
      swtch->succ->probability = REG_BR_PROB_BASE - p;
      e = make_edge (swtch, preheader,
		     swtch->succ->flags & EDGE_IRREDUCIBLE_LOOP);
      e->probability = p;
    }

  /* Recount dominators for outer blocks.  */
  iterate_fix_dominators (CDI_DOMINATORS, dom_bbs, n_dom_bbs);

  /* And unroll loop.  */

  sbitmap_ones (wont_exit);
  RESET_BIT (wont_exit, may_exit_copy);

  if (!duplicate_loop_to_header_edge (loop, loop_latch_edge (loop),
		loops, max_unroll,
		wont_exit, desc->out_edge, remove_edges, &n_remove_edges,
		DLTHE_FLAG_UPDATE_FREQ))
    abort ();

  free (wont_exit);

  /* Expand the branch and count.  */
  if (is_bct)
    for (i = 0; i < n_remove_edges; i++)
      expand_bct (remove_edges[i], discard_inc);

  /* Remove the edges.  */
  for (i = 0; i < n_remove_edges; i++)
    remove_path (loops, remove_edges[i]);
  free (remove_edges);

  if (rtl_dump_file)
    fprintf (rtl_dump_file,
	     ";; Unrolled loop %d times, counting # of iterations in runtime, %i insns\n",
	     max_unroll, num_loop_insns (loop));
}

/* Decide whether to simply peel LOOP and how much.  */
static void
decide_peel_simple (struct loop *loop, int flags)
{
  unsigned npeel;

  if (!(flags & UAP_PEEL))
    {
      /* We were not asked to, just return back silently.  */
      return;
    }

  if (rtl_dump_file)
    fprintf (rtl_dump_file, ";; Considering simply peeling loop\n");

  /* npeel = number of iterations to peel.  */
  npeel = PARAM_VALUE (PARAM_MAX_PEELED_INSNS) / loop->ninsns;
  if (npeel > (unsigned) PARAM_VALUE (PARAM_MAX_PEEL_TIMES))
    npeel = PARAM_VALUE (PARAM_MAX_PEEL_TIMES);

  /* Skip big loops.  */
  if (!npeel)
    {
      if (rtl_dump_file)
	fprintf (rtl_dump_file, ";; Not considering loop, is too big\n");
      return;
    }

  /* Check for simple loops.  */
  if (!loop->has_desc)
    {
      loop->simple = simple_loop_p (loop, &loop->desc);
      loop->has_desc = 1;
    }

  /* Check number of iterations.  */
  if (loop->simple && loop->desc.const_iter)
    {
      if (rtl_dump_file)
	fprintf (rtl_dump_file, ";; Loop iterates constant times\n");
      return;
    }

  /* Do not simply peel loops with branches inside -- it increases number
     of mispredicts.  */
  if (loop->desc.n_branches > 1)
    {
      if (rtl_dump_file)
	fprintf (rtl_dump_file, ";; Not peeling, contains branches\n");
      return;
    }

  if (loop->header->count)
    {
      unsigned niter = expected_loop_iterations (loop);
      if (niter + 1 > npeel)
	{
	  if (rtl_dump_file)
	    {
	      fprintf (rtl_dump_file, ";; Not peeling loop, rolls too much (");
	      fprintf (rtl_dump_file, HOST_WIDEST_INT_PRINT_DEC, (HOST_WIDEST_INT) (niter + 1));
	      fprintf (rtl_dump_file, " iterations > %d [maximum peelings])\n", npeel);
	    }
	  return;
	}
      npeel = niter + 1;
    }
  else
    {
      /* For now we have no good heuristics to decide whether loop peeling
         will be effective, so disable it.  */
      if (rtl_dump_file)
	fprintf (rtl_dump_file,
		 ";; Not peeling loop, no evidence it will be profitable\n");
      return;
    }

  /* Success.  */
  loop->lpt_decision.decision = LPT_PEEL_SIMPLE;
  loop->lpt_decision.times = npeel;
}

/* Peel a LOOP LOOP->LPT_DECISION.TIMES times.  The transformation:
   while (cond)
     body;

   ==>

   if (!cond) goto end;
   body;
   if (!cond) goto end;
   body;
   while (cond)
     body;
   end: ;
   */
static void
peel_loop_simple (struct loops *loops, struct loop *loop)
{
  sbitmap wont_exit;
  unsigned npeel = loop->lpt_decision.times;

  wont_exit = sbitmap_alloc (npeel + 1);
  sbitmap_zero (wont_exit);

  if (!duplicate_loop_to_header_edge (loop, loop_preheader_edge (loop),
		loops, npeel, wont_exit, NULL, NULL, NULL,
		DLTHE_FLAG_UPDATE_FREQ))
    abort ();

  free (wont_exit);

  if (rtl_dump_file)
    fprintf (rtl_dump_file, ";; Peeling loop %d times\n", npeel);
}

/* Decide whether to unroll LOOP stupidly and how much.  */
static void
decide_unroll_stupid (struct loop *loop, int flags)
{
  unsigned nunroll, nunroll_by_av, i;

  if (!(flags & UAP_UNROLL_ALL))
    {
      /* We were not asked to, just return back silently.  */
      return;
    }

  if (rtl_dump_file)
    fprintf (rtl_dump_file, ";; Considering unrolling loop stupidly\n");

  /* nunroll = total number of copies of the original loop body in
     unrolled loop (i.e. if it is 2, we have to duplicate loop body once.  */
  nunroll = PARAM_VALUE (PARAM_MAX_UNROLLED_INSNS) / loop->ninsns;
  nunroll_by_av = PARAM_VALUE (PARAM_MAX_AVERAGE_UNROLLED_INSNS) / loop->av_ninsns;
  if (nunroll > nunroll_by_av)
    nunroll = nunroll_by_av;
  if (nunroll > (unsigned) PARAM_VALUE (PARAM_MAX_UNROLL_TIMES))
    nunroll = PARAM_VALUE (PARAM_MAX_UNROLL_TIMES);

  /* Skip big loops.  */
  if (nunroll <= 1)
    {
      if (rtl_dump_file)
	fprintf (rtl_dump_file, ";; Not considering loop, is too big\n");
      return;
    }

  /* Check for simple loops.  */
  if (!loop->has_desc)
    {
      loop->simple = simple_loop_p (loop, &loop->desc);
      loop->has_desc = 1;
    }

  /* Check simpleness.  */
  if (loop->simple)
    {
      if (rtl_dump_file)
	fprintf (rtl_dump_file, ";; The loop is simple\n");
      return;
    }

  /* Do not unroll loops with branches inside -- it increases number
     of mispredicts.  */
  if (loop->desc.n_branches > 1)
    {
      if (rtl_dump_file)
	fprintf (rtl_dump_file, ";; Not unrolling, contains branches\n");
      return;
    }

  /* If we have profile feedback, check whether the loop rolls.  */
  if (loop->header->count && expected_loop_iterations (loop) < 2 * nunroll)
    {
      if (rtl_dump_file)
	fprintf (rtl_dump_file, ";; Not unrolling loop, doesn't roll\n");
      return;
    }

  /* Success.  Now force nunroll to be power of 2, as it seems that this
     improves results (partially because of better alignments, partially
     because of some dark magic).  */
  for (i = 1; 2 * i <= nunroll; i *= 2);

  loop->lpt_decision.decision = LPT_UNROLL_STUPID;
  loop->lpt_decision.times = i - 1;
}

/* Unroll a LOOP LOOP->LPT_DECISION.TIMES times.  The transformation:
   while (cond)
     body;

   ==>

   while (cond)
     {
       body;
       if (!cond) break;
       body;
       if (!cond) break;
       body;
       if (!cond) break;
       body;
     }
   */
static void
unroll_loop_stupid (struct loops *loops, struct loop *loop)
{
  sbitmap wont_exit;
  unsigned nunroll = loop->lpt_decision.times;

  wont_exit = sbitmap_alloc (nunroll + 1);
  sbitmap_zero (wont_exit);

  if (!duplicate_loop_to_header_edge (loop, loop_latch_edge (loop),
		loops, nunroll, wont_exit, NULL, NULL, NULL,
		DLTHE_FLAG_UPDATE_FREQ))
    abort ();

  free (wont_exit);

  if (rtl_dump_file)
    fprintf (rtl_dump_file, ";; Unrolled loop %d times, %i insns\n",
	     nunroll, num_loop_insns (loop));
}

/* Expand a bct instruction in a branch and an increment.
   If flag_inc is set, the induction variable does not need to be
   incremented.  */
void expand_bct (edge e, int flag_inc)
{
  rtx bct_insn = BB_END (e->src);
  rtx cmp;
  rtx inc;
  rtx seq;

  rtx tgt;
  rtx condition;
  rtx label;
  rtx reg;
  rtx jump;
  rtx pattern = PATTERN(bct_insn);
  
  if (!(is_bct_cond(bct_insn)))
    return;

  inc = get_var_set_from_bct (bct_insn);
  cmp = XVECEXP (pattern, 0, 0);
  reg = SET_DEST (inc);

  start_sequence ();
  if (!flag_inc)
    {
      tgt = force_operand (XEXP (inc, 1), XEXP (inc, 0));
      if (tgt != XEXP (inc, 0))
	emit_move_insn (XEXP (inc, 0), tgt);
    }

  condition = XEXP (SET_SRC (cmp), 0);
  label = XEXP (SET_SRC (cmp), 1);

  do_compare_rtx_and_jump (copy_rtx (reg), XEXP (condition, 1), 
			   GET_CODE (condition), 0,
			   GET_MODE (reg), NULL_RTX, NULL_RTX,
			   label);
  jump = get_last_insn ();
  JUMP_LABEL (jump) = label;
  seq = get_insns ();
  end_sequence ();
  emit_insn_after (seq, bct_insn);

  delete_insn (bct_insn);

  return;
}

/* Check that the increment of the count register can be discarded.  */
bool
discard_increment (struct loop *loop)
{
  struct loop_desc *desc = &loop->desc;
  rtx inc, set_src, reg;
  rtx bct_insn;
  unsigned int i;
  basic_block *body;
  
  bct_insn = BB_END (desc->out_edge->src);
  if (!is_bct_cond (bct_insn))
    abort();  

  inc = get_var_set_from_bct (bct_insn);

  /* Check that inc is of the form reg = reg - 1.  */
  reg = SET_DEST (inc);
  set_src = SET_SRC (inc);

  if (GET_CODE (set_src) != PLUS)
    return false;

  if (!rtx_equal_p (XEXP (set_src, 0), reg))
    return false;
  
  if (!CONSTANT_P (XEXP (set_src, 1)))
     return false;

  if (INTVAL (XEXP (set_src, 1)) != -1)
     return false;
  
  /* We need to check that the register has no other uses beside the branch and
     count.  */
  body = get_loop_body (loop);
  for(i=0; i < loop->num_nodes; i++)
    {
      if (reg_mentioned_p (desc->var, BB_HEAD (body[i])))
	  return false;

      if (body[i] != desc->out_edge->src)
	if (reg_mentioned_p (desc->var, BB_END (body[i])))
	  return false;

      if (reg_used_between_p (desc->var, BB_HEAD (body[i]), BB_END (body[i])))
	  return false;
    }

  /* Check that the branch and count ends the latch.  */
  if (desc->out_edge->src != loop->latch)
    {
      rtx insn;

      /* Latch is a dummy block generated by loop-init.  */
      if (BRANCH_EDGE(desc->out_edge->src)->dest != loop->latch)
	  return false;

      for (insn = BB_HEAD (loop->latch); insn != NEXT_INSN (BB_END (loop->latch)); 
	   insn = NEXT_INSN (insn))
        if (INSN_P (insn)) return false;
    }

  return true;
}

