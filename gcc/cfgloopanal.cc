/* Natural loop analysis code for GNU compiler.
   Copyright (C) 2002-2024 Free Software Foundation, Inc.

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
#include "memmodel.h"
#include "emit-rtl.h"
#include "cfgloop.h"
#include "explow.h"
#include "expr.h"
#include "graphds.h"
#include "sreal.h"
#include "regs.h"
#include "function-abi.h"

struct target_cfgloop default_target_cfgloop;
#if SWITCHABLE_TARGET
struct target_cfgloop *this_target_cfgloop = &default_target_cfgloop;
#endif

/* Checks whether BB is executed exactly once in each LOOP iteration.  */

bool
just_once_each_iteration_p (const class loop *loop, const_basic_block bb)
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
  class loop *cloop;
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
	    && dominated_by_p (CDI_DOMINATORS, act, e->dest))
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
num_loop_insns (const class loop *loop)
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
average_num_loop_insns (const class loop *loop)
{
  basic_block *bbs, bb;
  unsigned i, binsns;
  sreal ninsns;
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

      ninsns += (sreal)binsns * bb->count.to_sreal_scale (loop->header->count);
      /* Avoid overflows.   */
      if (ninsns > 1000000)
	{
	  free (bbs);
	  return 1000000;
	}
    }
  free (bbs);

  int64_t ret = ninsns.to_int ();
  if (!ret)
    ret = 1; /* To avoid division by zero.  */

  return ret;
}

/* Compute how many times loop is entered.  */

profile_count
loop_count_in (const class loop *loop)
{
  /* Compute number of invocations of the loop.  */
  profile_count count_in = profile_count::zero ();
  edge e;
  edge_iterator ei;
  bool found_latch = false;

  if (loops_state_satisfies_p (LOOPS_MAY_HAVE_MULTIPLE_LATCHES))
    FOR_EACH_EDGE (e, ei, loop->header->preds)
      if (!flow_bb_inside_loop_p (loop, e->src))
	count_in += e->count ();
      else
	found_latch = true;
  else
    FOR_EACH_EDGE (e, ei, loop->header->preds)
      if (e->src != loop->latch)
	count_in += e->count ();
      else
	found_latch = true;
  gcc_checking_assert (found_latch);
  return count_in;
}

/* Return true if BB profile can be used to determine the expected number of
   iterations (that is number of executions of latch edge(s) for each
   entry of the loop.  If this is the case initialize RET with the number
   of iterations.

   RELIABLE is set if profile indiates that the returned value should be
   realistic estimate.  (This is the case if we read profile and did not
   messed it up yet and not the case of guessed profiles.)

   This function uses only CFG profile.  We track more reliable info in
   loop_info structure and for loop optimization heuristics more relevant
   is get_estimated_loop_iterations API.  */

bool
expected_loop_iterations_by_profile (const class loop *loop, sreal *ret,
				     bool *reliable)
{
  profile_count header_count = loop->header->count;
  if (reliable)
    *reliable = false;

  /* TODO: For single exit loops we can use loop exit edge probability.
     It also may be reliable while loop itself was adjusted.  */
  if (!header_count.initialized_p ()
      || !header_count.nonzero_p ())
    return false;

  profile_count count_in = loop_count_in (loop);

  bool known;
  /* Number of iterations is number of executions of latch edge.  */
  *ret = (header_count - count_in).to_sreal_scale (count_in, &known);
  if (!known)
    return false;
  if (reliable)
    {
      /* Header should have at least count_in many executions.
	 Give up on clearly inconsistent profile.  */
      if (header_count < count_in && header_count.differs_from_p (count_in))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Inconsistent bb profile of loop %i\n",
		     loop->num);
	  *reliable = false;
	}
      else
	*reliable = count_in.reliable_p () && header_count.reliable_p ();
    }
  return true;
}

/* Return true if loop CFG profile may be unrealistically flat.
   This is a common case, since average loops iterate only about 5 times.
   In the case we do not have profile feedback or do not know real number of
   iterations during profile estimation, we are likely going to predict it with
   similar low iteration count.  For static loop profiles we also artificially
   cap profile of loops with known large iteration count so they do not appear
   significantly more hot than other loops with unknown iteration counts.

   For loop optimization heuristics we ignore CFG profile and instead
   use get_estimated_loop_iterations API which returns estimate
   only when it is realistic.  For unknown counts some optimizations,
   like vectorizer or unroller make guess that iteration count will
   be large.  In this case we need to avoid scaling down the profile
   after the loop transform.  */

bool
maybe_flat_loop_profile (const class loop *loop)
{
  bool reliable;
  sreal ret;

  if (!expected_loop_iterations_by_profile (loop, &ret, &reliable))
    return true;

  /* Reliable CFG estimates ought never be flat.  Sanity check with
     nb_iterations_estimate.  If those differ, it is a but in profile
     updating code  */
  if (reliable)
    {
      int64_t intret = ret.to_nearest_int ();
      if (loop->any_estimate
	  && (wi::ltu_p (intret * 2, loop->nb_iterations_estimate)
	      || wi::gtu_p (intret, loop->nb_iterations_estimate * 2)))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file,
		    "Loop %i has inconsistent iterations estimates: "
		    "reliable CFG based iteration estimate is %f "
		    "while nb_iterations_estimate is %i\n",
		    loop->num,
		    ret.to_double (),
		    (int)loop->nb_iterations_estimate.to_shwi ());
	  return true;
	}
      return false;
    }

  /* Allow some margin of error and see if we are close to known bounds.
     sreal (9,-3) is 9/8  */
  int64_t intret = (ret * sreal (9, -3)).to_nearest_int ();
  if (loop->any_upper_bound && wi::geu_p (intret, loop->nb_iterations_upper_bound))
    return false;
  if (loop->any_likely_upper_bound
      && wi::geu_p (intret, loop->nb_iterations_likely_upper_bound))
    return false;
  if (loop->any_estimate
      && wi::geu_p (intret, loop->nb_iterations_estimate))
    return false;
  return true;
}

/* Returns expected number of iterations of LOOP, according to
   measured or guessed profile.

   This functions attempts to return "sane" value even if profile
   information is not good enough to derive osmething.  */

gcov_type
expected_loop_iterations_unbounded (const class loop *loop,
				    bool *read_profile_p)
{
  gcov_type expected = -1;

  if (read_profile_p)
    *read_profile_p = false;

  sreal sreal_expected;
  if (expected_loop_iterations_by_profile
	  (loop, &sreal_expected, read_profile_p))
    expected = sreal_expected.to_nearest_int ();
  else
    expected = param_avg_loop_niter;

  HOST_WIDE_INT max = get_max_loop_iterations_int (loop);
  if (max != -1 && max < expected)
    return max;

  return expected;
}

/* Returns expected number of LOOP iterations.  The returned value is bounded
   by REG_BR_PROB_BASE.  */

unsigned
expected_loop_iterations (class loop *loop)
{
  gcov_type expected = expected_loop_iterations_unbounded (loop);
  return (expected > REG_BR_PROB_BASE ? REG_BR_PROB_BASE : expected);
}

/* Returns the maximum level of nesting of subloops of LOOP.  */

unsigned
get_loop_level (const class loop *loop)
{
  const class loop *ploop;
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
	/* ??? This is only a rough heuristic.  It doesn't cope well
	   with alternative ABIs, but that's an optimization rather than
	   correctness issue.  */
	if (default_function_abi.clobbers_full_reg_p (i))
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
      && number_of_loops (cfun) <= (unsigned) param_ira_max_loops_num)
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
single_likely_exit (class loop *loop, const vec<edge> &exits)
{
  edge found = single_exit (loop);
  unsigned i;
  edge ex;

  if (found)
    return found;
  FOR_EACH_VEC_ELT (exits, i, ex)
    {
      if (probably_never_executed_edge_p (cfun, ex)
	  /* We want to rule out paths to noreturns but not low probabilities
	     resulting from adjustments or combining.
	     FIXME: once we have better quality tracking, make this more
	     robust.  */
	  || ex->probability <= profile_probability::very_unlikely ())
	continue;
      if (!found)
	found = ex;
      else
	return NULL;
    }
  return found;
}


/* Gets basic blocks of a LOOP.  Header is the 0-th block, rest is in dfs
   order against direction of edges from latch.  Specially, if
   header != latch, latch is the 1-st block.  */

auto_vec<basic_block>
get_loop_hot_path (const class loop *loop)
{
  basic_block bb = loop->header;
  auto_vec<basic_block> path;
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
