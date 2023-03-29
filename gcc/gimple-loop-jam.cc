/* Loop unroll-and-jam.
   Copyright (C) 2017-2023 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree-pass.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "fold-const.h"
#include "tree-cfg.h"
#include "tree-ssa.h"
#include "tree-ssa-loop-niter.h"
#include "tree-ssa-loop.h"
#include "tree-ssa-loop-manip.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "gimple-iterator.h"
#include "cfghooks.h"
#include "tree-data-ref.h"
#include "tree-ssa-loop-ivopts.h"
#include "tree-vectorizer.h"
#include "tree-ssa-sccvn.h"
#include "tree-cfgcleanup.h"

/* Unroll and Jam transformation
   
   This is a combination of two transformations, where the second
   is not always valid.  It's applicable if a loop nest has redundancies
   over the iterations of an outer loop while not having that with
   an inner loop.

   Given this nest:
       for (i) {
	 for (j) {
	   B(i,j)
	 }
       }

   first unroll:
       for (i by 2) {
	 for (j) {
	   B(i,j)
	 }
	 for (j) {
	   B(i+1,j)
	 }
       }

   then fuse the two adjacent inner loops resulting from that:
       for (i by 2) {
	 for (j) {
	   B(i,j)
	   B(i+1,j)
	 }
       }

   As the order of evaluations of the body B changes this is valid
   only in certain situations: all distance vectors need to be forward.
   Additionally if there are multiple induction variables than just
   a counting control IV (j above) we can also deal with some situations.

   The validity is checked by unroll_jam_possible_p, and the data-dep
   testing below.

   A trivial example where the fusion is wrong would be when
   B(i,j) == x[j-1] = x[j];
       for (i by 2) {
	 for (j) {
	   x[j-1] = x[j];
	 }
	 for (j) {
	   x[j-1] = x[j];
	 }
       }  effect: move content to front by two elements
       -->
       for (i by 2) {
	 for (j) {
	   x[j-1] = x[j];
	   x[j-1] = x[j];
	 }
       }  effect: move content to front by one element
*/

/* Modify the loop tree for the fact that all code once belonging
   to the OLD loop or the outer loop of OLD now is inside LOOP.  */

static void
merge_loop_tree (class loop *loop, class loop *old)
{
  basic_block *bbs;
  int i, n;
  class loop *subloop;
  edge e;
  edge_iterator ei;

  /* Find its nodes.  */
  bbs = XNEWVEC (basic_block, n_basic_blocks_for_fn (cfun));
  n = get_loop_body_with_size (loop, bbs, n_basic_blocks_for_fn (cfun));

  for (i = 0; i < n; i++)
    {
      /* If the block was direct child of OLD loop it's now part
	 of LOOP.  If it was outside OLD, then it moved into LOOP
	 as well.  This avoids changing the loop father for BBs
	 in inner loops of OLD.  */
      if (bbs[i]->loop_father == old
	  || loop_depth (bbs[i]->loop_father) < loop_depth (old))
	{
	  remove_bb_from_loops (bbs[i]);
	  add_bb_to_loop (bbs[i], loop);
	  continue;
	}

      /* If we find a direct subloop of OLD, move it to LOOP.  */
      subloop = bbs[i]->loop_father;
      if (loop_outer (subloop) == old && subloop->header == bbs[i])
	{
	  flow_loop_tree_node_remove (subloop);
	  flow_loop_tree_node_add (loop, subloop);
	}
    }

  /* Update the information about loop exit edges.  */
  for (i = 0; i < n; i++)
    {
      FOR_EACH_EDGE (e, ei, bbs[i]->succs)
	{
	  rescan_loop_exit (e, false, false);
	}
    }

  loop->num_nodes = n;

  free (bbs);
}

/* BB is part of the outer loop of an unroll-and-jam situation.
   Check if any statements therein would prevent the transformation.  */

static bool
bb_prevents_fusion_p (basic_block bb)
{
  gimple_stmt_iterator gsi;
  /* BB is duplicated by outer unrolling and then all N-1 first copies
     move into the body of the fused inner loop.  If BB exits the outer loop
     the last copy still does so, and the first N-1 copies are cancelled
     by loop unrolling, so also after fusion it's the exit block.
     But there might be other reasons that prevent fusion:
       * stores or unknown side-effects prevent fusion
       * loads don't
       * computations into SSA names: these aren't problematic.  Their
	 result will be unused on the exit edges of the first N-1 copies
	 (those aren't taken after unrolling).  If they are used on the
	 other edge (the one leading to the outer latch block) they are
	 loop-carried (on the outer loop) and the Nth copy of BB will
	 compute them again (i.e. the first N-1 copies will be dead).  */
  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple *g = gsi_stmt (gsi);
      if (gimple_vdef (g) || gimple_has_side_effects (g))
	return true;
    }
  return false;
}

/* Given an inner loop LOOP (of some OUTER loop) determine if
   we can safely fuse copies of it (generated by outer unrolling).
   If so return true, otherwise return false.  */

static bool
unroll_jam_possible_p (class loop *outer, class loop *loop)
{
  basic_block *bbs;
  int i, n;
  class tree_niter_desc niter;

  /* When fusing the loops we skip the latch block
     of the first one, so it mustn't have any effects to
     preserve.  */
  if (!empty_block_p (loop->latch))
    return false;

  edge exit;
  if (!(exit = single_exit (loop)))
    return false;

  /* We need a perfect nest.  Quick check for adjacent inner loops.  */
  if (outer->inner != loop || loop->next)
    return false;

  /* Prevent head-controlled inner loops, that we usually have.
     The guard block would need to be accepted
     (invariant condition either entering or skipping the loop),
     without also accepting arbitrary control flow.  When unswitching
     ran before us (as with -O3) this won't be a problem because its
     outer loop unswitching will have moved out the invariant condition.

     If we do that we need to extend fuse_loops() to cope with this
     by threading through the (still invariant) copied condition
     between the two loop copies.  */
  if (!dominated_by_p (CDI_DOMINATORS, outer->latch, loop->header))
    return false;

  /* The number of iterations of the inner loop must be loop invariant
     with respect to the outer loop.  */
  if (!number_of_iterations_exit (loop, single_exit (loop), &niter,
				 false, true)
      || niter.cmp == ERROR_MARK
      || !integer_zerop (niter.may_be_zero)
      || !expr_invariant_in_loop_p (outer, niter.niter))
    return false;

  /* If the inner loop produces any values that are used inside the
     outer loop (except the virtual op) then it can flow
     back (perhaps indirectly) into the inner loop.  This prevents
     fusion: without fusion the value at the last iteration is used,
     with fusion the value after the initial iteration is used.

     If all uses are outside the outer loop this doesn't prevent fusion;
     the value of the last iteration is still used (and the values from
     all intermediate iterations are dead).  */
  gphi_iterator psi;
  for (psi = gsi_start_phis (single_exit (loop)->dest);
       !gsi_end_p (psi); gsi_next (&psi))
    {
      imm_use_iterator imm_iter;
      use_operand_p use_p;
      tree op = gimple_phi_result (psi.phi ());
      if (virtual_operand_p (op))
	continue;
      FOR_EACH_IMM_USE_FAST (use_p, imm_iter, op)
	{
	  gimple *use_stmt = USE_STMT (use_p);
	  if (!is_gimple_debug (use_stmt)
	      && flow_bb_inside_loop_p (outer, gimple_bb (use_stmt)))
	    return false;
	}
    }

  /* And check blocks belonging to just outer loop.  */
  bbs = XNEWVEC (basic_block, n_basic_blocks_for_fn (cfun));
  n = get_loop_body_with_size (outer, bbs, n_basic_blocks_for_fn (cfun));

  for (i = 0; i < n; i++)
    if (bbs[i]->loop_father == outer
	&& (bb_prevents_fusion_p (bbs[i])
	    /* Outer loop exits must come after the inner loop, otherwise
	       we'll put the outer loop exit into the fused inner loop.  */
	    || (loop_exits_from_bb_p (outer, bbs[i])
		&& !dominated_by_p (CDI_DOMINATORS, bbs[i], exit->src))))
      break;
  free (bbs);
  if (i != n)
    return false;

  /* For now we can safely fuse copies of LOOP only if all
     loop carried variables are inductions (or the virtual op).

     We could handle reductions as well (the initial value in the second
     body would be the after-iter value of the first body) if it's over
     an associative and commutative operation.  We wouldn't
     be able to handle unknown cycles.  */
  for (psi = gsi_start_phis (loop->header); !gsi_end_p (psi); gsi_next (&psi))
    {
      affine_iv iv;
      tree op = gimple_phi_result (psi.phi ());

      if (virtual_operand_p (op))
	continue;
      if (!simple_iv (loop, loop, op, &iv, true))
	return false;
      /* The inductions must be regular, loop invariant step and initial
	 value.  */
      if (!expr_invariant_in_loop_p (outer, iv.step)
	  || !expr_invariant_in_loop_p (outer, iv.base))
	return false;
      /* XXX With more effort we could also be able to deal with inductions
	 where the initial value is loop variant but a simple IV in the
	 outer loop.  The initial value for the second body would be
	 the original initial value plus iv.base.step.  The next value
	 for the fused loop would be the original next value of the first
	 copy, _not_ the next value of the second body.  */
    }

  return true;
}

/* Fuse LOOP with all further neighbors.  The loops are expected to
   be in appropriate form.  */

static void
fuse_loops (class loop *loop)
{
  class loop *next = loop->next;

  while (next)
    {
      edge e;

      remove_branch (single_pred_edge (loop->latch));
      /* Make delete_basic_block not fiddle with the loop structure.  */
      basic_block oldlatch = loop->latch;
      loop->latch = NULL;
      delete_basic_block (oldlatch);
      e = redirect_edge_and_branch (loop_latch_edge (next),
				    loop->header);
      loop->latch = e->src;
      flush_pending_stmts (e);

      gcc_assert (EDGE_COUNT (next->header->preds) == 1);

      /* The PHI nodes of the second body (single-argument now)
	 need adjustments to use the right values: either directly
	 the value of the corresponding PHI in the first copy or
	 the one leaving the first body which unrolling did for us.

	 See also unroll_jam_possible_p() for further possibilities.  */
      gphi_iterator psi_first, psi_second;
      e = single_pred_edge (next->header);
      for (psi_first = gsi_start_phis (loop->header),
	   psi_second = gsi_start_phis (next->header);
	   !gsi_end_p (psi_first);
	   gsi_next (&psi_first), gsi_next (&psi_second))
	{
	  gphi *phi_first = psi_first.phi ();
	  gphi *phi_second = psi_second.phi ();
	  tree firstop = gimple_phi_result (phi_first);
	  /* The virtual operand is correct already as it's
	     always live at exit, hence has a LCSSA node and outer
	     loop unrolling updated SSA form.  */
	  if (virtual_operand_p (firstop))
	    continue;

	  /* Due to unroll_jam_possible_p() we know that this is
	     an induction.  The second body goes over the same
	     iteration space.  */
	  add_phi_arg (phi_second, firstop, e,
		       gimple_location (phi_first));
	}
      gcc_assert (gsi_end_p (psi_second));

      merge_loop_tree (loop, next);
      gcc_assert (!next->num_nodes);
      class loop *ln = next->next;
      delete_loop (next);
      next = ln;
    }
}

/* Return true if any of the access functions for dataref A
   isn't invariant with respect to loop LOOP_NEST.  */
static bool
any_access_function_variant_p (const struct data_reference *a,
			       const class loop *loop_nest)
{
  vec<tree> fns = DR_ACCESS_FNS (a);

  for (tree t : fns)
    if (!evolution_function_is_invariant_p (t, loop_nest->num))
      return true;

  return false;
}

/* Returns true if the distance in DDR can be determined and adjusts
   the unroll factor in *UNROLL to make unrolling valid for that distance.
   Otherwise return false.  DDR is with respect to the outer loop of INNER.

   If this data dep can lead to a removed memory reference, increment
   *REMOVED and adjust *PROFIT_UNROLL to be the necessary unroll factor
   for this to happen.  */

static bool
adjust_unroll_factor (class loop *inner, struct data_dependence_relation *ddr,
		      unsigned *unroll, unsigned *profit_unroll,
		      unsigned *removed)
{
  bool ret = false;
  if (DDR_ARE_DEPENDENT (ddr) != chrec_known)
    {
      if (DDR_NUM_DIST_VECTS (ddr) == 0)
	return false;
      unsigned i;
      lambda_vector dist_v;
      FOR_EACH_VEC_ELT (DDR_DIST_VECTS (ddr), i, dist_v)
	{
	  /* A distance (a,b) is at worst transformed into (a/N,b) by the
	     unrolling (factor N), so the transformation is valid if
	     a >= N, or b > 0, or b is zero and a > 0.  Otherwise the unroll
	     factor needs to be limited so that the first condition holds.
	     That may limit the factor down to zero in the worst case.  */
	  lambda_int dist = dist_v[0];
	  if (dist < 0)
	    gcc_unreachable ();
	  else if (dist >= (lambda_int)*unroll)
	    ;
	  else if (lambda_vector_zerop (dist_v + 1, DDR_NB_LOOPS (ddr) - 1))
	    {
	      /* We have (a,0) with a < N, so this will be transformed into
	         (0,0) after unrolling by N.  This might potentially be a
		 problem, if it's not a read-read dependency.  */
	      if (DR_IS_READ (DDR_A (ddr)) && DR_IS_READ (DDR_B (ddr)))
		;
	      else
		{
		  /* So, at least one is a write, and we might reduce the
		     distance vector to (0,0).  This is still no problem
		     if both data-refs are affine with respect to the inner
		     loops.  But if one of them is invariant with respect
		     to an inner loop our reordering implicit in loop fusion
		     corrupts the program, as our data dependences don't
		     capture this.  E.g. for:
		       for (0 <= i < n)
		         for (0 <= j < m)
		           a[i][0] = a[i+1][0] + 2;    // (1)
		           b[i][j] = b[i+1][j] + 2;    // (2)
		     the distance vector for both statements is (-1,0),
		     but exchanging the order for (2) is okay, while
		     for (1) it is not.  To see this, write out the original
		     accesses (assume m is 2):
		       a i j original
		       0 0 0 r a[1][0] b[1][0]
		       1 0 0 w a[0][0] b[0][0]
		       2 0 1 r a[1][0] b[1][1]
		       3 0 1 w a[0][0] b[0][1]
		       4 1 0 r a[2][0] b[2][0]
		       5 1 0 w a[1][0] b[1][0]
		     after unroll-by-2 and fusion the accesses are done in
		     this order (from column a): 0,1, 4,5, 2,3, i.e. this:
		       a i j transformed
		       0 0 0 r a[1][0] b[1][0]
		       1 0 0 w a[0][0] b[0][0]
		       4 1 0 r a[2][0] b[2][0]
		       5 1 0 w a[1][0] b[1][0]
		       2 0 1 r a[1][0] b[1][1]  
		       3 0 1 w a[0][0] b[0][1]
		     Note how access 2 accesses the same element as access 5
		     for array 'a' but not for array 'b'.  */
		  if (any_access_function_variant_p (DDR_A (ddr), inner)
		      && any_access_function_variant_p (DDR_B (ddr), inner))
		    ;
		  else
		    /* And if any dataref of this pair is invariant with
		       respect to the inner loop, we have no chance than
		       to reduce the unroll factor.  */
		    *unroll = dist;
		}
	    }
	  else if (lambda_vector_lexico_pos (dist_v + 1, DDR_NB_LOOPS (ddr) - 1))
	    ;
	  else
	    *unroll = dist;

	  /* With a distance (a,0) it's always profitable to unroll-and-jam
	     (by a+1), because one memory reference will go away.  With
	     (a,b) and b != 0 that's less clear.  We will increase the
	     number of streams without lowering the number of mem refs.
	     So for now only handle the first situation.  */
	  if (lambda_vector_zerop (dist_v + 1, DDR_NB_LOOPS (ddr) - 1))
	    {
	      *profit_unroll = MAX (*profit_unroll, (unsigned)dist + 1);
	      (*removed)++;
	    }

	  ret = true;
	}
    }
  return ret;
}

/* Main entry point for the unroll-and-jam transformation
   described above.  */

static unsigned int
tree_loop_unroll_and_jam (void)
{
  unsigned int todo = 0;

  gcc_assert (scev_initialized_p ());

  /* Go through all innermost loops.  */
  for (auto loop : loops_list (cfun, LI_ONLY_INNERMOST))
    {
      class loop *outer = loop_outer (loop);

      if (loop_depth (loop) < 2
	  || optimize_loop_nest_for_size_p (outer))
	continue;

      if (!unroll_jam_possible_p (outer, loop))
	continue;

      vec<data_reference_p> datarefs = vNULL;
      vec<ddr_p> dependences = vNULL;
      unsigned unroll_factor, profit_unroll, removed;
      class tree_niter_desc desc;
      bool unroll = false;

      auto_vec<loop_p, 3> loop_nest;
      if (!compute_data_dependences_for_loop (outer, true, &loop_nest,
					      &datarefs, &dependences))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Cannot analyze data dependencies\n");
	  free_data_refs (datarefs);
	  free_dependence_relations (dependences);
	  continue;
	}
      if (!datarefs.length ())
	continue;

      if (dump_file && (dump_flags & TDF_DETAILS))
	dump_data_dependence_relations (dump_file, dependences);

      unroll_factor = (unsigned)-1;
      profit_unroll = 1;
      removed = 0;

      /* Check all dependencies.  */
      unsigned i;
      struct data_dependence_relation *ddr;
      FOR_EACH_VEC_ELT (dependences, i, ddr)
	{
	  struct data_reference *dra, *drb;

	  /* If the refs are independend there's nothing to do.  */
	  if (DDR_ARE_DEPENDENT (ddr) == chrec_known)
	    continue;

	  dra = DDR_A (ddr);
	  drb = DDR_B (ddr);

	  /* Nothing interesting for the self dependencies, except for WAW if
	     the access function is not affine or constant because we may end
	     up reordering writes to the same location.  */
	  if (dra == drb)
	    {
	      if (DR_IS_WRITE (dra)
		  && !DR_ACCESS_FNS (dra).is_empty ()
		  && DDR_ARE_DEPENDENT (ddr) == chrec_dont_know)
		{
		  unroll_factor = 0;
		  break;
		}
	      else
		continue;
	    }

	  /* Now check the distance vector, for determining a sensible
	     outer unroll factor, and for validity of merging the inner
	     loop copies.  */
	  if (!adjust_unroll_factor (loop, ddr, &unroll_factor, &profit_unroll,
				     &removed))
	    {
	      /* Couldn't get the distance vector.  For two reads that's
		 harmless (we assume we should unroll).  For at least
		 one write this means we can't check the dependence direction
		 and hence can't determine safety.  */

	      if (DR_IS_WRITE (dra) || DR_IS_WRITE (drb))
		{
		  unroll_factor = 0;
		  break;
		}
	    }
	}

      /* We regard a user-specified minimum percentage of zero as a request
	 to ignore all profitability concerns and apply the transformation
	 always.  */
      if (!param_unroll_jam_min_percent)
	profit_unroll = MAX(2, profit_unroll);
      else if (removed * 100 / datarefs.length ()
	  < (unsigned)param_unroll_jam_min_percent)
	profit_unroll = 1;
      if (unroll_factor > profit_unroll)
	unroll_factor = profit_unroll;
      if (unroll_factor > (unsigned)param_unroll_jam_max_unroll)
	unroll_factor = param_unroll_jam_max_unroll;
      unroll = (unroll_factor > 1
		&& can_unroll_loop_p (outer, unroll_factor, &desc));

      if (unroll)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_OPTIMIZED_LOCATIONS | TDF_DETAILS,
			     find_loop_location (outer),
			     "applying unroll and jam with factor %d\n",
			     unroll_factor);
	  initialize_original_copy_tables ();
	  tree_unroll_loop (outer, unroll_factor, &desc);
	  free_original_copy_tables ();
	  fuse_loops (outer->inner);
	  todo |= TODO_cleanup_cfg;

	  auto_bitmap exit_bbs;
	  bitmap_set_bit (exit_bbs, single_exit (outer)->dest->index);
	  todo |= do_rpo_vn (cfun, loop_preheader_edge (outer), exit_bbs);
	}

      loop_nest.release ();
      free_dependence_relations (dependences);
      free_data_refs (datarefs);
    }

  if (todo)
    {
      free_dominance_info (CDI_DOMINATORS);
      /* We need to cleanup the CFG first since otherwise SSA form can
	 be not up-to-date from the VN run.  */
      if (todo & TODO_cleanup_cfg)
	{
	  cleanup_tree_cfg ();
	  todo &= ~TODO_cleanup_cfg;
	}
      rewrite_into_loop_closed_ssa (NULL, 0);
      scev_reset ();
    }
  return todo;
}

/* Pass boilerplate */

namespace {

const pass_data pass_data_loop_jam =
{
  GIMPLE_PASS, /* type */
  "unrolljam", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  TV_LOOP_JAM, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_loop_jam : public gimple_opt_pass
{
public:
  pass_loop_jam (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_loop_jam, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override { return flag_unroll_jam != 0; }
  unsigned int execute (function *) final override;

};

unsigned int
pass_loop_jam::execute (function *fun)
{
  if (number_of_loops (fun) <= 1)
    return 0;

  return tree_loop_unroll_and_jam ();
}

}

gimple_opt_pass *
make_pass_loop_jam (gcc::context *ctxt)
{
  return new pass_loop_jam (ctxt);
}
