/* Natural loop analysis code for GNU compiler.
   Copyright (C) 2002-2016 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "predict.h"
#include "emit-rtl.h"
#include "cfgloop.h"
#include "explow.h"
#include "expr.h"
#include "graphds.h"
#include "params.h"

struct target_cfgloop default_target_cfgloop;
#if SWITCHABLE_TARGET
struct target_cfgloop *this_target_cfgloop = &default_target_cfgloop;
#endif

/* Checks whether BB is executed exactly once in each LOOP iteration.  */

bool
just_once_each_iteration_p (const struct loop *loop, const_basic_block bb)
{
  /* It must be executed at least once each iteration.  */
  if (!dominated_by_p (CDI_DOMINATORS, loop->latch, bb))
    return false;

  /* And just once.  */
  if (bb->loop_father != loop)
    return false;

  /* But this was not enough.  We might have some irreducible loop here.  */
  if (bb->flags & BB_IRREDUCIBLE_LOOP)
    return false;

  return true;
}

/* Marks blocks and edges that are part of non-recognized loops; i.e. we
   throw away all latch edges and mark blocks inside any remaining cycle.
   Everything is a bit complicated due to fact we do not want to do this
   for parts of cycles that only "pass" through some loop -- i.e. for
   each cycle, we want to mark blocks that belong directly to innermost
   loop containing the whole cycle.

   LOOPS is the loop tree.  */

#define LOOP_REPR(LOOP) ((LOOP)->num + last_basic_block_for_fn (cfun))
#define BB_REPR(BB) ((BB)->index + 1)

bool
mark_irreducible_loops (void)
{
  basic_block act;
  struct graph_edge *ge;
  edge e;
  edge_iterator ei;
  int src, dest;
  unsigned depth;
  struct graph *g;
  int num = number_of_loops (cfun);
  struct loop *cloop;
  bool irred_loop_found = false;
  int i;

  gcc_assert (current_loops != NULL);

  /* Reset the flags.  */
  FOR_BB_BETWEEN (act, ENTRY_BLOCK_PTR_FOR_FN (cfun),
		  EXIT_BLOCK_PTR_FOR_FN (cfun), next_bb)
    {
      act->flags &= ~BB_IRREDUCIBLE_LOOP;
      FOR_EACH_EDGE (e, ei, act->succs)
	e->flags &= ~EDGE_IRREDUCIBLE_LOOP;
    }

  /* Create the edge lists.  */
  g = new_graph (last_basic_block_for_fn (cfun) + num);

  FOR_BB_BETWEEN (act, ENTRY_BLOCK_PTR_FOR_FN (cfun),
		  EXIT_BLOCK_PTR_FOR_FN (cfun), next_bb)
    FOR_EACH_EDGE (e, ei, act->succs)
      {
	/* Ignore edges to exit.  */
	if (e->dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
	  continue;

	src = BB_REPR (act);
	dest = BB_REPR (e->dest);

	/* Ignore latch edges.  */
	if (e->dest->loop_father->header == e->dest
	    && e->dest->loop_father->latch == act)
	  continue;

	/* Edges inside a single loop should be left where they are.  Edges
	   to subloop headers should lead to representative of the subloop,
	   but from the same place.

	   Edges exiting loops should lead from representative
	   of the son of nearest common ancestor of the loops in that
	   act lays.  */

	if (e->dest->loop_father->header == e->dest)
	  dest = LOOP_REPR (e->dest->loop_father);

	if (!flow_bb_inside_loop_p (act->loop_father, e->dest))
	  {
	    depth = 1 + loop_depth (find_common_loop (act->loop_father,
						      e->dest->loop_father));
	    if (depth == loop_depth (act->loop_father))
	      cloop = act->loop_father;
	    else
	      cloop = (*act->loop_father->superloops)[depth];

	    src = LOOP_REPR (cloop);
	  }

	add_edge (g, src, dest)->data = e;
      }

  /* Find the strongly connected components.  */
  graphds_scc (g, NULL);

  /* Mark the irreducible loops.  */
  for (i = 0; i < g->n_vertices; i++)
    for (ge = g->vertices[i].succ; ge; ge = ge->succ_next)
      {
	edge real = (edge) ge->data;
	/* edge E in graph G is irreducible if it connects two vertices in the
	   same scc.  */

	/* All edges should lead from a component with higher number to the
	   one with lower one.  */
	gcc_assert (g->vertices[ge->src].component >= g->vertices[ge->dest].component);

	if (g->vertices[ge->src].component != g->vertices[ge->dest].component)
	  continue;

	real->flags |= EDGE_IRREDUCIBLE_LOOP;
	irred_loop_found = true;
	if (flow_bb_inside_loop_p (real->src->loop_father, real->dest))
	  real->src->flags |= BB_IRREDUCIBLE_LOOP;
      }

  free_graph (g);

  loops_state_set (LOOPS_HAVE_MARKED_IRREDUCIBLE_REGIONS);
  return irred_loop_found;
}

/* Counts number of insns inside LOOP.  */
int
num_loop_insns (const struct loop *loop)
{
  basic_block *bbs, bb;
  unsigned i, ninsns = 0;
  rtx_insn *insn;

  bbs = get_loop_body (loop);
  for (i = 0; i < loop->num_nodes; i++)
    {
      bb = bbs[i];
      FOR_BB_INSNS (bb, insn)
	if (NONDEBUG_INSN_P (insn))
	  ninsns++;
    }
  free (bbs);

  if (!ninsns)
    ninsns = 1;	/* To avoid division by zero.  */

  return ninsns;
}

/* Counts number of insns executed on average per iteration LOOP.  */
int
average_num_loop_insns (const struct loop *loop)
{
  basic_block *bbs, bb;
  unsigned i, binsns, ninsns, ratio;
  rtx_insn *insn;

  ninsns = 0;
  bbs = get_loop_body (loop);
  for (i = 0; i < loop->num_nodes; i++)
    {
      bb = bbs[i];

      binsns = 0;
      FOR_BB_INSNS (bb, insn)
	if (NONDEBUG_INSN_P (insn))
	  binsns++;

      ratio = loop->header->frequency == 0
	      ? BB_FREQ_MAX
	      : (bb->frequency * BB_FREQ_MAX) / loop->header->frequency;
      ninsns += binsns * ratio;
    }
  free (bbs);

  ninsns /= BB_FREQ_MAX;
  if (!ninsns)
    ninsns = 1; /* To avoid division by zero.  */

  return ninsns;
}

/* Returns expected number of iterations of LOOP, according to
   measured or guessed profile.  No bounding is done on the
   value.  */

gcov_type
expected_loop_iterations_unbounded (const struct loop *loop,
				    bool *read_profile_p)
{
  edge e;
  edge_iterator ei;
  gcov_type expected;
  
  if (read_profile_p)
    *read_profile_p = false;

  /* If we have no profile at all, use AVG_LOOP_NITER.  */
  if (profile_status_for_fn (cfun) == PROFILE_ABSENT)
    expected = PARAM_VALUE (PARAM_AVG_LOOP_NITER);
  else if (loop->latch->count || loop->header->count)
    {
      gcov_type count_in, count_latch;

      count_in = 0;
      count_latch = 0;

      FOR_EACH_EDGE (e, ei, loop->header->preds)
	if (e->src == loop->latch)
	  count_latch = e->count;
	else
	  count_in += e->count;

      if (count_in == 0)
	expected = count_latch * 2;
      else
	{
	  expected = (count_latch + count_in - 1) / count_in;
	  if (read_profile_p)
	    *read_profile_p = true;
	}
    }
  else
    {
      int freq_in, freq_latch;

      freq_in = 0;
      freq_latch = 0;

      FOR_EACH_EDGE (e, ei, loop->header->preds)
	if (e->src == loop->latch)
	  freq_latch = EDGE_FREQUENCY (e);
	else
	  freq_in += EDGE_FREQUENCY (e);

      if (freq_in == 0)
	{
	  /* If we have no profile at all, use AVG_LOOP_NITER iterations.  */
	  if (!freq_latch)
	    expected = PARAM_VALUE (PARAM_AVG_LOOP_NITER);
	  else
	    expected = freq_latch * 2;
	}
      else
        expected = (freq_latch + freq_in - 1) / freq_in;
    }

  HOST_WIDE_INT max = get_max_loop_iterations_int (loop);
  if (max != -1 && max < expected)
    return max;
  return expected;
}

/* Returns expected number of LOOP iterations.  The returned value is bounded
   by REG_BR_PROB_BASE.  */

unsigned
expected_loop_iterations (struct loop *loop)
{
  gcov_type expected = expected_loop_iterations_unbounded (loop);
  return (expected > REG_BR_PROB_BASE ? REG_BR_PROB_BASE : expected);
}

/* Returns the maximum level of nesting of subloops of LOOP.  */

unsigned
get_loop_level (const struct loop *loop)
{
  const struct loop *ploop;
  unsigned mx = 0, l;

  for (ploop = loop->inner; ploop; ploop = ploop->next)
    {
      l = get_loop_level (ploop);
      if (l >= mx)
	mx = l + 1;
    }
  return mx;
}

/* Initialize the constants for computing set costs.  */

void
init_set_costs (void)
{
  int speed;
  rtx_insn *seq;
  rtx reg1 = gen_raw_REG (SImode, LAST_VIRTUAL_REGISTER + 1);
  rtx reg2 = gen_raw_REG (SImode, LAST_VIRTUAL_REGISTER + 2);
  rtx addr = gen_raw_REG (Pmode, LAST_VIRTUAL_REGISTER + 3);
  rtx mem = validize_mem (gen_rtx_MEM (SImode, addr));
  unsigned i;

  target_avail_regs = 0;
  target_clobbered_regs = 0;
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (TEST_HARD_REG_BIT (reg_class_contents[GENERAL_REGS], i)
	&& !fixed_regs[i])
      {
	target_avail_regs++;
	if (call_used_regs[i])
	  target_clobbered_regs++;
      }

  target_res_regs = 3;

  for (speed = 0; speed < 2; speed++)
     {
      crtl->maybe_hot_insn_p = speed;
      /* Set up the costs for using extra registers:

	 1) If not many free registers remain, we should prefer having an
	    additional move to decreasing the number of available registers.
	    (TARGET_REG_COST).
	 2) If no registers are available, we need to spill, which may require
	    storing the old value to memory and loading it back
	    (TARGET_SPILL_COST).  */

      start_sequence ();
      emit_move_insn (reg1, reg2);
      seq = get_insns ();
      end_sequence ();
      target_reg_cost [speed] = seq_cost (seq, speed);

      start_sequence ();
      emit_move_insn (mem, reg1);
      emit_move_insn (reg2, mem);
      seq = get_insns ();
      end_sequence ();
      target_spill_cost [speed] = seq_cost (seq, speed);
    }
  default_rtl_profile ();
}

/* Estimates cost of increased register pressure caused by making N_NEW new
   registers live around the loop.  N_OLD is the number of registers live
   around the loop.  If CALL_P is true, also take into account that
   call-used registers may be clobbered in the loop body, reducing the
   number of available registers before we spill.  */

unsigned
estimate_reg_pressure_cost (unsigned n_new, unsigned n_old, bool speed,
			    bool call_p)
{
  unsigned cost;
  unsigned regs_needed = n_new + n_old;
  unsigned available_regs = target_avail_regs;

  /* If there is a call in the loop body, the call-clobbered registers
     are not available for loop invariants.  */
  if (call_p)
    available_regs = available_regs - target_clobbered_regs;

  /* If we have enough registers, we should use them and not restrict
     the transformations unnecessarily.  */
  if (regs_needed + target_res_regs <= available_regs)
    return 0;

  if (regs_needed <= available_regs)
    /* If we are close to running out of registers, try to preserve
       them.  */
    cost = target_reg_cost [speed] * n_new;
  else
    /* If we run out of registers, it is very expensive to add another
       one.  */
    cost = target_spill_cost [speed] * n_new;

  if (optimize && (flag_ira_region == IRA_REGION_ALL
		   || flag_ira_region == IRA_REGION_MIXED)
      && number_of_loops (cfun) <= (unsigned) IRA_MAX_LOOPS_NUM)
    /* IRA regional allocation deals with high register pressure
       better.  So decrease the cost (to do more accurate the cost
       calculation for IRA, we need to know how many registers lives
       through the loop transparently).  */
    cost /= 2;

  return cost;
}

/* Sets EDGE_LOOP_EXIT flag for all loop exits.  */

void
mark_loop_exit_edges (void)
{
  basic_block bb;
  edge e;

  if (number_of_loops (cfun) <= 1)
    return;

  FOR_EACH_BB_FN (bb, cfun)
    {
      edge_iterator ei;

      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  if (loop_outer (bb->loop_father)
	      && loop_exit_edge_p (bb->loop_father, e))
	    e->flags |= EDGE_LOOP_EXIT;
	  else
	    e->flags &= ~EDGE_LOOP_EXIT;
	}
    }
}

/* Return exit edge if loop has only one exit that is likely
   to be executed on runtime (i.e. it is not EH or leading
   to noreturn call.  */

edge
single_likely_exit (struct loop *loop)
{
  edge found = single_exit (loop);
  vec<edge> exits;
  unsigned i;
  edge ex;

  if (found)
    return found;
  exits = get_loop_exit_edges (loop);
  FOR_EACH_VEC_ELT (exits, i, ex)
    {
      if (ex->flags & (EDGE_EH | EDGE_ABNORMAL_CALL))
	continue;
      /* The constant of 5 is set in a way so noreturn calls are
	 ruled out by this test.  The static branch prediction algorithm
         will not assign such a low probability to conditionals for usual
         reasons.  */
      if (profile_status_for_fn (cfun) != PROFILE_ABSENT
	  && ex->probability < 5 && !ex->count)
	continue;
      if (!found)
	found = ex;
      else
	{
	  exits.release ();
	  return NULL;
	}
    }
  exits.release ();
  return found;
}


/* Gets basic blocks of a LOOP.  Header is the 0-th block, rest is in dfs
   order against direction of edges from latch.  Specially, if
   header != latch, latch is the 1-st block.  */

vec<basic_block>
get_loop_hot_path (const struct loop *loop)
{
  basic_block bb = loop->header;
  vec<basic_block> path = vNULL;
  bitmap visited = BITMAP_ALLOC (NULL);

  while (true)
    {
      edge_iterator ei;
      edge e;
      edge best = NULL;

      path.safe_push (bb);
      bitmap_set_bit (visited, bb->index);
      FOR_EACH_EDGE (e, ei, bb->succs)
        if ((!best || e->probability > best->probability)
	    && !loop_exit_edge_p (loop, e)
	    && !bitmap_bit_p (visited, e->dest->index))
	  best = e;
      if (!best || best->dest == loop->header)
	break;
      bb = best->dest;
    }
  BITMAP_FREE (visited);
  return path;
}
