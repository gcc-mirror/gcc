/* Induction variable canonicalization and loop peeling.
   Copyright (C) 2004-2017 Free Software Foundation, Inc.

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

/* This pass detects the loops that iterate a constant number of times,
   adds a canonical induction variable (step -1, tested against 0)
   and replaces the exit test.  This enables the less powerful rtl
   level analysis to use this information.

   This might spoil the code in some cases (by increasing register pressure).
   Note that in the case the new variable is not needed, ivopts will get rid
   of it, so it might only be a problem when there are no other linear induction
   variables.  In that case the created optimization possibilities are likely
   to pay up.

   We also perform
     - complete unrolling (or peeling) when the loops is rolling few enough
       times
     - simple peeling (i.e. copying few initial iterations prior the loop)
       when number of iteration estimate is known (typically by the profile
       info).  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "cgraph.h"
#include "gimple-pretty-print.h"
#include "fold-const.h"
#include "profile.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-manip.h"
#include "tree-ssa-loop-niter.h"
#include "tree-ssa-loop.h"
#include "tree-into-ssa.h"
#include "cfgloop.h"
#include "tree-chrec.h"
#include "tree-scalar-evolution.h"
#include "params.h"
#include "tree-inline.h"
#include "tree-cfgcleanup.h"
#include "builtins.h"

/* Specifies types of loops that may be unrolled.  */

enum unroll_level
{
  UL_SINGLE_ITER,	/* Only loops that exit immediately in the first
			   iteration.  */
  UL_NO_GROWTH,		/* Only loops whose unrolling will not cause increase
			   of code size.  */
  UL_ALL		/* All suitable loops.  */
};

/* Adds a canonical induction variable to LOOP iterating NITER times.  EXIT
   is the exit edge whose condition is replaced.  */

static void
create_canonical_iv (struct loop *loop, edge exit, tree niter)
{
  edge in;
  tree type, var;
  gcond *cond;
  gimple_stmt_iterator incr_at;
  enum tree_code cmp;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Added canonical iv to loop %d, ", loop->num);
      print_generic_expr (dump_file, niter, TDF_SLIM);
      fprintf (dump_file, " iterations.\n");
    }

  cond = as_a <gcond *> (last_stmt (exit->src));
  in = EDGE_SUCC (exit->src, 0);
  if (in == exit)
    in = EDGE_SUCC (exit->src, 1);

  /* Note that we do not need to worry about overflows, since
     type of niter is always unsigned and all comparisons are
     just for equality/nonequality -- i.e. everything works
     with a modulo arithmetics.  */

  type = TREE_TYPE (niter);
  niter = fold_build2 (PLUS_EXPR, type,
		       niter,
		       build_int_cst (type, 1));
  incr_at = gsi_last_bb (in->src);
  create_iv (niter,
	     build_int_cst (type, -1),
	     NULL_TREE, loop,
	     &incr_at, false, NULL, &var);

  cmp = (exit->flags & EDGE_TRUE_VALUE) ? EQ_EXPR : NE_EXPR;
  gimple_cond_set_code (cond, cmp);
  gimple_cond_set_lhs (cond, var);
  gimple_cond_set_rhs (cond, build_int_cst (type, 0));
  update_stmt (cond);
}

/* Describe size of loop as detected by tree_estimate_loop_size.  */
struct loop_size
{
  /* Number of instructions in the loop.  */
  int overall;

  /* Number of instructions that will be likely optimized out in
     peeled iterations of loop  (i.e. computation based on induction
     variable where induction variable starts at known constant.)  */
  int eliminated_by_peeling;

  /* Same statistics for last iteration of loop: it is smaller because
     instructions after exit are not executed.  */
  int last_iteration;
  int last_iteration_eliminated_by_peeling;
  
  /* If some IV computation will become constant.  */
  bool constant_iv;

  /* Number of call stmts that are not a builtin and are pure or const
     present on the hot path.  */
  int num_pure_calls_on_hot_path;
  /* Number of call stmts that are not a builtin and are not pure nor const
     present on the hot path.  */
  int num_non_pure_calls_on_hot_path;
  /* Number of statements other than calls in the loop.  */
  int non_call_stmts_on_hot_path;
  /* Number of branches seen on the hot path.  */
  int num_branches_on_hot_path;
};

/* Return true if OP in STMT will be constant after peeling LOOP.  */

static bool
constant_after_peeling (tree op, gimple *stmt, struct loop *loop)
{
  if (is_gimple_min_invariant (op))
    return true;

  /* We can still fold accesses to constant arrays when index is known.  */
  if (TREE_CODE (op) != SSA_NAME)
    {
      tree base = op;

      /* First make fast look if we see constant array inside.  */
      while (handled_component_p (base))
	base = TREE_OPERAND (base, 0);
      if ((DECL_P (base)
	   && ctor_for_folding (base) != error_mark_node)
	  || CONSTANT_CLASS_P (base))
	{
	  /* If so, see if we understand all the indices.  */
	  base = op;
	  while (handled_component_p (base))
	    {
	      if (TREE_CODE (base) == ARRAY_REF
		  && !constant_after_peeling (TREE_OPERAND (base, 1), stmt, loop))
		return false;
	      base = TREE_OPERAND (base, 0);
	    }
	  return true;
	}
      return false;
    }

  /* Induction variables are constants when defined in loop.  */
  if (loop_containing_stmt (stmt) != loop)
    return false;
  tree ev = analyze_scalar_evolution (loop, op);
  if (chrec_contains_undetermined (ev)
      || chrec_contains_symbols (ev))
    return false;
  return true;
}

/* Computes an estimated number of insns in LOOP.
   EXIT (if non-NULL) is an exite edge that will be eliminated in all but last
   iteration of the loop.
   EDGE_TO_CANCEL (if non-NULL) is an non-exit edge eliminated in the last iteration
   of loop.
   Return results in SIZE, estimate benefits for complete unrolling exiting by EXIT. 
   Stop estimating after UPPER_BOUND is met.  Return true in this case.  */

static bool
tree_estimate_loop_size (struct loop *loop, edge exit, edge edge_to_cancel,
			 struct loop_size *size, int upper_bound)
{
  basic_block *body = get_loop_body (loop);
  gimple_stmt_iterator gsi;
  unsigned int i;
  bool after_exit;
  vec<basic_block> path = get_loop_hot_path (loop);

  size->overall = 0;
  size->eliminated_by_peeling = 0;
  size->last_iteration = 0;
  size->last_iteration_eliminated_by_peeling = 0;
  size->num_pure_calls_on_hot_path = 0;
  size->num_non_pure_calls_on_hot_path = 0;
  size->non_call_stmts_on_hot_path = 0;
  size->num_branches_on_hot_path = 0;
  size->constant_iv = 0;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Estimating sizes for loop %i\n", loop->num);
  for (i = 0; i < loop->num_nodes; i++)
    {
      if (edge_to_cancel && body[i] != edge_to_cancel->src
	  && dominated_by_p (CDI_DOMINATORS, body[i], edge_to_cancel->src))
	after_exit = true;
      else
	after_exit = false;
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, " BB: %i, after_exit: %i\n", body[i]->index,
		 after_exit);

      for (gsi = gsi_start_bb (body[i]); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  int num = estimate_num_insns (stmt, &eni_size_weights);
	  bool likely_eliminated = false;
	  bool likely_eliminated_last = false;
	  bool likely_eliminated_peeled = false;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "  size: %3i ", num);
	      print_gimple_stmt (dump_file, gsi_stmt (gsi), 0);
	    }

	  /* Look for reasons why we might optimize this stmt away. */

	  if (!gimple_has_side_effects (stmt))
	    {
	      /* Exit conditional.  */
	      if (exit && body[i] == exit->src
		  && stmt == last_stmt (exit->src))
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "   Exit condition will be eliminated "
			     "in peeled copies.\n");
		  likely_eliminated_peeled = true;
		}
	      if (edge_to_cancel && body[i] == edge_to_cancel->src
		  && stmt == last_stmt (edge_to_cancel->src))
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "   Exit condition will be eliminated "
			     "in last copy.\n");
		  likely_eliminated_last = true;
		}
	      /* Sets of IV variables  */
	      if (gimple_code (stmt) == GIMPLE_ASSIGN
		  && constant_after_peeling (gimple_assign_lhs (stmt), stmt, loop))
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "   Induction variable computation will"
			     " be folded away.\n");
		  likely_eliminated = true;
		}
	      /* Assignments of IV variables.  */
	      else if (gimple_code (stmt) == GIMPLE_ASSIGN
		       && TREE_CODE (gimple_assign_lhs (stmt)) == SSA_NAME
		       && constant_after_peeling (gimple_assign_rhs1 (stmt),
						  stmt, loop)
		       && (gimple_assign_rhs_class (stmt) != GIMPLE_BINARY_RHS
			   || constant_after_peeling (gimple_assign_rhs2 (stmt),
						      stmt, loop)))
		{
		  size->constant_iv = true;
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file,
			     "   Constant expression will be folded away.\n");
		  likely_eliminated = true;
		}
	      /* Conditionals.  */
	      else if ((gimple_code (stmt) == GIMPLE_COND
			&& constant_after_peeling (gimple_cond_lhs (stmt), stmt,
						   loop)
			&& constant_after_peeling (gimple_cond_rhs (stmt), stmt,
						   loop)
			/* We don't simplify all constant compares so make sure
			   they are not both constant already.  See PR70288.  */
			&& (! is_gimple_min_invariant (gimple_cond_lhs (stmt))
			    || ! is_gimple_min_invariant
				 (gimple_cond_rhs (stmt))))
		       || (gimple_code (stmt) == GIMPLE_SWITCH
			   && constant_after_peeling (gimple_switch_index (
							as_a <gswitch *>
							  (stmt)),
						      stmt, loop)
			   && ! is_gimple_min_invariant
				   (gimple_switch_index
				      (as_a <gswitch *> (stmt)))))
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "   Constant conditional.\n");
		  likely_eliminated = true;
		}
	    }

	  size->overall += num;
	  if (likely_eliminated || likely_eliminated_peeled)
	    size->eliminated_by_peeling += num;
	  if (!after_exit)
	    {
	      size->last_iteration += num;
	      if (likely_eliminated || likely_eliminated_last)
		size->last_iteration_eliminated_by_peeling += num;
	    }
	  if ((size->overall * 3 / 2 - size->eliminated_by_peeling
	      - size->last_iteration_eliminated_by_peeling) > upper_bound)
	    {
              free (body);
	      path.release ();
	      return true;
	    }
	}
    }
  while (path.length ())
    {
      basic_block bb = path.pop ();
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  if (gimple_code (stmt) == GIMPLE_CALL
	      && !gimple_inexpensive_call_p (as_a <gcall *>  (stmt)))
	    {
	      int flags = gimple_call_flags (stmt);
	      if (flags & (ECF_PURE | ECF_CONST))
		size->num_pure_calls_on_hot_path++;
	      else
		size->num_non_pure_calls_on_hot_path++;
	      size->num_branches_on_hot_path ++;
	    }
	  /* Count inexpensive calls as non-calls, because they will likely
	     expand inline.  */
	  else if (gimple_code (stmt) != GIMPLE_DEBUG)
	    size->non_call_stmts_on_hot_path++;
	  if (((gimple_code (stmt) == GIMPLE_COND
	        && (!constant_after_peeling (gimple_cond_lhs (stmt), stmt, loop)
		    || constant_after_peeling (gimple_cond_rhs (stmt), stmt,
					       loop)))
	       || (gimple_code (stmt) == GIMPLE_SWITCH
		   && !constant_after_peeling (gimple_switch_index (
						 as_a <gswitch *> (stmt)),
					       stmt, loop)))
	      && (!exit || bb != exit->src))
	    size->num_branches_on_hot_path++;
	}
    }
  path.release ();
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "size: %i-%i, last_iteration: %i-%i\n", size->overall,
    	     size->eliminated_by_peeling, size->last_iteration,
	     size->last_iteration_eliminated_by_peeling);

  free (body);
  return false;
}

/* Estimate number of insns of completely unrolled loop.
   It is (NUNROLL + 1) * size of loop body with taking into account
   the fact that in last copy everything after exit conditional
   is dead and that some instructions will be eliminated after
   peeling.

   Loop body is likely going to simplify further, this is difficult
   to guess, we just decrease the result by 1/3.  */

static unsigned HOST_WIDE_INT
estimated_unrolled_size (struct loop_size *size,
			 unsigned HOST_WIDE_INT nunroll)
{
  HOST_WIDE_INT unr_insns = ((nunroll)
  			     * (HOST_WIDE_INT) (size->overall
			     			- size->eliminated_by_peeling));
  if (!nunroll)
    unr_insns = 0;
  unr_insns += size->last_iteration - size->last_iteration_eliminated_by_peeling;

  unr_insns = unr_insns * 2 / 3;
  if (unr_insns <= 0)
    unr_insns = 1;

  return unr_insns;
}

/* Loop LOOP is known to not loop.  See if there is an edge in the loop
   body that can be remove to make the loop to always exit and at
   the same time it does not make any code potentially executed 
   during the last iteration dead.  

   After complete unrolling we still may get rid of the conditional
   on the exit in the last copy even if we have no idea what it does.
   This is quite common case for loops of form

     int a[5];
     for (i=0;i<b;i++)
       a[i]=0;

   Here we prove the loop to iterate 5 times but we do not know
   it from induction variable.

   For now we handle only simple case where there is exit condition
   just before the latch block and the latch block contains no statements
   with side effect that may otherwise terminate the execution of loop
   (such as by EH or by terminating the program or longjmp).

   In the general case we may want to cancel the paths leading to statements
   loop-niter identified as having undefined effect in the last iteration.
   The other cases are hopefully rare and will be cleaned up later.  */

static edge
loop_edge_to_cancel (struct loop *loop)
{
  vec<edge> exits;
  unsigned i;
  edge edge_to_cancel;
  gimple_stmt_iterator gsi;

  /* We want only one predecestor of the loop.  */
  if (EDGE_COUNT (loop->latch->preds) > 1)
    return NULL;

  exits = get_loop_exit_edges (loop);

  FOR_EACH_VEC_ELT (exits, i, edge_to_cancel)
    {
       /* Find the other edge than the loop exit
          leaving the conditoinal.  */
       if (EDGE_COUNT (edge_to_cancel->src->succs) != 2)
         continue;
       if (EDGE_SUCC (edge_to_cancel->src, 0) == edge_to_cancel)
         edge_to_cancel = EDGE_SUCC (edge_to_cancel->src, 1);
       else
         edge_to_cancel = EDGE_SUCC (edge_to_cancel->src, 0);

      /* We only can handle conditionals.  */
      if (!(edge_to_cancel->flags & (EDGE_TRUE_VALUE | EDGE_FALSE_VALUE)))
	continue;

      /* We should never have conditionals in the loop latch. */
      gcc_assert (edge_to_cancel->dest != loop->header);

      /* Check that it leads to loop latch.  */
      if (edge_to_cancel->dest != loop->latch)
        continue;

      exits.release ();

      /* Verify that the code in loop latch does nothing that may end program
         execution without really reaching the exit.  This may include
	 non-pure/const function calls, EH statements, volatile ASMs etc.  */
      for (gsi = gsi_start_bb (loop->latch); !gsi_end_p (gsi); gsi_next (&gsi))
	if (gimple_has_side_effects (gsi_stmt (gsi)))
	   return NULL;
      return edge_to_cancel;
    }
  exits.release ();
  return NULL;
}

/* Remove all tests for exits that are known to be taken after LOOP was
   peeled NPEELED times. Put gcc_unreachable before every statement
   known to not be executed.  */

static bool
remove_exits_and_undefined_stmts (struct loop *loop, unsigned int npeeled)
{
  struct nb_iter_bound *elt;
  bool changed = false;

  for (elt = loop->bounds; elt; elt = elt->next)
    {
      /* If statement is known to be undefined after peeling, turn it
	 into unreachable (or trap when debugging experience is supposed
	 to be good).  */
      if (!elt->is_exit
	  && wi::ltu_p (elt->bound, npeeled))
	{
	  gimple_stmt_iterator gsi = gsi_for_stmt (elt->stmt);
	  gcall *stmt = gimple_build_call
	      (builtin_decl_implicit (BUILT_IN_UNREACHABLE), 0);
	  gimple_set_location (stmt, gimple_location (elt->stmt));
	  gsi_insert_before (&gsi, stmt, GSI_NEW_STMT);
	  split_block (gimple_bb (stmt), stmt);
	  changed = true;
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Forced statement unreachable: ");
	      print_gimple_stmt (dump_file, elt->stmt, 0);
	    }
	}
      /* If we know the exit will be taken after peeling, update.  */
      else if (elt->is_exit
	       && wi::leu_p (elt->bound, npeeled))
	{
	  basic_block bb = gimple_bb (elt->stmt);
	  edge exit_edge = EDGE_SUCC (bb, 0);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Forced exit to be taken: ");
	      print_gimple_stmt (dump_file, elt->stmt, 0);
	    }
	  if (!loop_exit_edge_p (loop, exit_edge))
	    exit_edge = EDGE_SUCC (bb, 1);
	  exit_edge->probability = profile_probability::always ();
	  gcc_checking_assert (loop_exit_edge_p (loop, exit_edge));
	  gcond *cond_stmt = as_a <gcond *> (elt->stmt);
	  if (exit_edge->flags & EDGE_TRUE_VALUE)
	    gimple_cond_make_true (cond_stmt);
	  else
	    gimple_cond_make_false (cond_stmt);
	  update_stmt (cond_stmt);
	  changed = true;
	}
    }
  return changed;
}

/* Remove all exits that are known to be never taken because of the loop bound
   discovered.  */

static bool
remove_redundant_iv_tests (struct loop *loop)
{
  struct nb_iter_bound *elt;
  bool changed = false;

  if (!loop->any_upper_bound)
    return false;
  for (elt = loop->bounds; elt; elt = elt->next)
    {
      /* Exit is pointless if it won't be taken before loop reaches
	 upper bound.  */
      if (elt->is_exit && loop->any_upper_bound
          && wi::ltu_p (loop->nb_iterations_upper_bound, elt->bound))
	{
	  basic_block bb = gimple_bb (elt->stmt);
	  edge exit_edge = EDGE_SUCC (bb, 0);
	  struct tree_niter_desc niter;

	  if (!loop_exit_edge_p (loop, exit_edge))
	    exit_edge = EDGE_SUCC (bb, 1);

	  /* Only when we know the actual number of iterations, not
	     just a bound, we can remove the exit.  */
	  if (!number_of_iterations_exit (loop, exit_edge,
					  &niter, false, false)
	      || !integer_onep (niter.assumptions)
	      || !integer_zerop (niter.may_be_zero)
	      || !niter.niter
	      || TREE_CODE (niter.niter) != INTEGER_CST
	      || !wi::ltu_p (loop->nb_iterations_upper_bound,
			     wi::to_widest (niter.niter)))
	    continue;
	  
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Removed pointless exit: ");
	      print_gimple_stmt (dump_file, elt->stmt, 0);
	    }
	  gcond *cond_stmt = as_a <gcond *> (elt->stmt);
	  if (exit_edge->flags & EDGE_TRUE_VALUE)
	    gimple_cond_make_false (cond_stmt);
	  else
	    gimple_cond_make_true (cond_stmt);
	  update_stmt (cond_stmt);
	  changed = true;
	}
    }
  return changed;
}

/* Stores loops that will be unlooped and edges that will be removed
   after we process whole loop tree. */
static vec<loop_p> loops_to_unloop;
static vec<int> loops_to_unloop_nunroll;
static vec<edge> edges_to_remove;
/* Stores loops that has been peeled.  */
static bitmap peeled_loops;

/* Cancel all fully unrolled loops by putting __builtin_unreachable
   on the latch edge.  
   We do it after all unrolling since unlooping moves basic blocks
   across loop boundaries trashing loop closed SSA form as well
   as SCEV info needed to be intact during unrolling. 

   IRRED_INVALIDATED is used to bookkeep if information about
   irreducible regions may become invalid as a result
   of the transformation.  
   LOOP_CLOSED_SSA_INVALIDATED is used to bookkepp the case
   when we need to go into loop closed SSA form.  */

static void
unloop_loops (bitmap loop_closed_ssa_invalidated,
	      bool *irred_invalidated)
{
  while (loops_to_unloop.length ())
    {
      struct loop *loop = loops_to_unloop.pop ();
      int n_unroll = loops_to_unloop_nunroll.pop ();
      basic_block latch = loop->latch;
      edge latch_edge = loop_latch_edge (loop);
      int flags = latch_edge->flags;
      location_t locus = latch_edge->goto_locus;
      gcall *stmt;
      gimple_stmt_iterator gsi;

      remove_exits_and_undefined_stmts (loop, n_unroll);

      /* Unloop destroys the latch edge.  */
      unloop (loop, irred_invalidated, loop_closed_ssa_invalidated);

      /* Create new basic block for the latch edge destination and wire
	 it in.  */
      stmt = gimple_build_call (builtin_decl_implicit (BUILT_IN_UNREACHABLE), 0);
      latch_edge = make_edge (latch, create_basic_block (NULL, NULL, latch), flags);
      latch_edge->probability = profile_probability::never ();
      latch_edge->flags |= flags;
      latch_edge->goto_locus = locus;

      add_bb_to_loop (latch_edge->dest, current_loops->tree_root);
      latch_edge->dest->count = profile_count::zero ();
      latch_edge->dest->frequency = 0;
      set_immediate_dominator (CDI_DOMINATORS, latch_edge->dest, latch_edge->src);

      gsi = gsi_start_bb (latch_edge->dest);
      gsi_insert_after (&gsi, stmt, GSI_NEW_STMT);
    }
  loops_to_unloop.release ();
  loops_to_unloop_nunroll.release ();

  /* Remove edges in peeled copies.  */
  unsigned i;
  edge e;
  FOR_EACH_VEC_ELT (edges_to_remove, i, e)
    {
      bool ok = remove_path (e, irred_invalidated, loop_closed_ssa_invalidated);
      gcc_assert (ok);
    }
  edges_to_remove.release ();
}

/* Tries to unroll LOOP completely, i.e. NITER times.
   UL determines which loops we are allowed to unroll.
   EXIT is the exit of the loop that should be eliminated.
   MAXITER specfy bound on number of iterations, -1 if it is
   not known or too large for HOST_WIDE_INT.  The location
   LOCUS corresponding to the loop is used when emitting
   a summary of the unroll to the dump file.  */

static bool
try_unroll_loop_completely (struct loop *loop,
			    edge exit, tree niter,
			    enum unroll_level ul,
			    HOST_WIDE_INT maxiter,
			    location_t locus)
{
  unsigned HOST_WIDE_INT n_unroll = 0, ninsns, unr_insns;
  struct loop_size size;
  bool n_unroll_found = false;
  edge edge_to_cancel = NULL;
  dump_flags_t report_flags = MSG_OPTIMIZED_LOCATIONS | TDF_DETAILS;

  /* See if we proved number of iterations to be low constant.

     EXIT is an edge that will be removed in all but last iteration of 
     the loop.

     EDGE_TO_CACNEL is an edge that will be removed from the last iteration
     of the unrolled sequence and is expected to make the final loop not
     rolling. 

     If the number of execution of loop is determined by standard induction
     variable test, then EXIT and EDGE_TO_CANCEL are the two edges leaving
     from the iv test.  */
  if (tree_fits_uhwi_p (niter))
    {
      n_unroll = tree_to_uhwi (niter);
      n_unroll_found = true;
      edge_to_cancel = EDGE_SUCC (exit->src, 0);
      if (edge_to_cancel == exit)
	edge_to_cancel = EDGE_SUCC (exit->src, 1);
    }
  /* We do not know the number of iterations and thus we can not eliminate
     the EXIT edge.  */
  else
    exit = NULL;

  /* See if we can improve our estimate by using recorded loop bounds.  */
  if (maxiter >= 0
      && (!n_unroll_found || (unsigned HOST_WIDE_INT)maxiter < n_unroll))
    {
      n_unroll = maxiter;
      n_unroll_found = true;
      /* Loop terminates before the IV variable test, so we can not
	 remove it in the last iteration.  */
      edge_to_cancel = NULL;
    }

  if (!n_unroll_found)
    return false;

  if (n_unroll > (unsigned) PARAM_VALUE (PARAM_MAX_COMPLETELY_PEEL_TIMES))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Not unrolling loop %d "
		 "(--param max-completely-peel-times limit reached).\n",
		 loop->num);
      return false;
    }

  if (!edge_to_cancel)
    edge_to_cancel = loop_edge_to_cancel (loop);

  if (n_unroll)
    {
      bool large;
      if (ul == UL_SINGLE_ITER)
	return false;

      /* EXIT can be removed only if we are sure it passes first N_UNROLL
	 iterations.  */
      bool remove_exit = (exit && niter
			  && TREE_CODE (niter) == INTEGER_CST
			  && wi::leu_p (n_unroll, wi::to_widest (niter)));

      large = tree_estimate_loop_size
		 (loop, remove_exit ? exit : NULL, edge_to_cancel, &size,
		  PARAM_VALUE (PARAM_MAX_COMPLETELY_PEELED_INSNS));
      ninsns = size.overall;
      if (large)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Not unrolling loop %d: it is too large.\n",
		     loop->num);
	  return false;
	}

      unr_insns = estimated_unrolled_size (&size, n_unroll);
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "  Loop size: %d\n", (int) ninsns);
	  fprintf (dump_file, "  Estimated size after unrolling: %d\n",
		   (int) unr_insns);
	}

      /* If the code is going to shrink, we don't need to be extra cautious
	 on guessing if the unrolling is going to be profitable.  */
      if (unr_insns
	  /* If there is IV variable that will become constant, we save
	     one instruction in the loop prologue we do not account
	     otherwise.  */
	  <= ninsns + (size.constant_iv != false))
	;
      /* We unroll only inner loops, because we do not consider it profitable
	 otheriwse.  We still can cancel loopback edge of not rolling loop;
	 this is always a good idea.  */
      else if (ul == UL_NO_GROWTH)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Not unrolling loop %d: size would grow.\n",
		     loop->num);
	  return false;
	}
      /* Outer loops tend to be less interesting candidates for complete
	 unrolling unless we can do a lot of propagation into the inner loop
	 body.  For now we disable outer loop unrolling when the code would
	 grow.  */
      else if (loop->inner)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Not unrolling loop %d: "
		     "it is not innermost and code would grow.\n",
		     loop->num);
	  return false;
	}
      /* If there is call on a hot path through the loop, then
	 there is most probably not much to optimize.  */
      else if (size.num_non_pure_calls_on_hot_path)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Not unrolling loop %d: "
		     "contains call and code would grow.\n",
		     loop->num);
	  return false;
	}
      /* If there is pure/const call in the function, then we
	 can still optimize the unrolled loop body if it contains
	 some other interesting code than the calls and code
	 storing or cumulating the return value.  */
      else if (size.num_pure_calls_on_hot_path
	       /* One IV increment, one test, one ivtmp store
		  and one useful stmt.  That is about minimal loop
		  doing pure call.  */
	       && (size.non_call_stmts_on_hot_path
		   <= 3 + size.num_pure_calls_on_hot_path))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Not unrolling loop %d: "
		     "contains just pure calls and code would grow.\n",
		     loop->num);
	  return false;
	}
      /* Complete unrolling is a major win when control flow is removed and
	 one big basic block is created.  If the loop contains control flow
	 the optimization may still be a win because of eliminating the loop
	 overhead but it also may blow the branch predictor tables.
	 Limit number of branches on the hot path through the peeled
	 sequence.  */
      else if (size.num_branches_on_hot_path * (int)n_unroll
	       > PARAM_VALUE (PARAM_MAX_PEEL_BRANCHES))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Not unrolling loop %d: "
		     " number of branches on hot path in the unrolled sequence"
		     " reach --param max-peel-branches limit.\n",
		     loop->num);
	  return false;
	}
      else if (unr_insns
	       > (unsigned) PARAM_VALUE (PARAM_MAX_COMPLETELY_PEELED_INSNS))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Not unrolling loop %d: "
		     "(--param max-completely-peeled-insns limit reached).\n",
		     loop->num);
	  return false;
	}
      if (!n_unroll)
        dump_printf_loc (report_flags, locus,
                         "loop turned into non-loop; it never loops.\n");

      initialize_original_copy_tables ();
      auto_sbitmap wont_exit (n_unroll + 1);
      if (exit && niter
	  && TREE_CODE (niter) == INTEGER_CST
	  && wi::leu_p (n_unroll, wi::to_widest (niter)))
	{
	  bitmap_ones (wont_exit);
	  if (wi::eq_p (wi::to_widest (niter), n_unroll)
	      || edge_to_cancel)
	    bitmap_clear_bit (wont_exit, 0);
	}
      else
	{
	  exit = NULL;
	  bitmap_clear (wont_exit);
	}

      if (!gimple_duplicate_loop_to_header_edge (loop, loop_preheader_edge (loop),
						 n_unroll, wont_exit,
						 exit, &edges_to_remove,
						 DLTHE_FLAG_UPDATE_FREQ
						 | DLTHE_FLAG_COMPLETTE_PEEL))
	{
          free_original_copy_tables ();
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Failed to duplicate the loop\n");
	  return false;
	}

      free_original_copy_tables ();
    }

  /* Remove the conditional from the last copy of the loop.  */
  if (edge_to_cancel)
    {
      gcond *cond = as_a <gcond *> (last_stmt (edge_to_cancel->src));
      force_edge_cold (edge_to_cancel, true);
      if (edge_to_cancel->flags & EDGE_TRUE_VALUE)
	gimple_cond_make_false (cond);
      else
	gimple_cond_make_true (cond);
      update_stmt (cond);
      /* Do not remove the path. Doing so may remove outer loop
	 and confuse bookkeeping code in tree_unroll_loops_completelly.  */
    }

  /* Store the loop for later unlooping and exit removal.  */
  loops_to_unloop.safe_push (loop);
  loops_to_unloop_nunroll.safe_push (n_unroll);

  if (dump_enabled_p ())
    {
      if (!n_unroll)
        dump_printf_loc (MSG_OPTIMIZED_LOCATIONS | TDF_DETAILS, locus,
                         "loop turned into non-loop; it never loops\n");
      else
        {
          dump_printf_loc (MSG_OPTIMIZED_LOCATIONS | TDF_DETAILS, locus,
                           "loop with %d iterations completely unrolled",
			   (int) (n_unroll + 1));
          if (loop->header->count.initialized_p ())
            dump_printf (MSG_OPTIMIZED_LOCATIONS | TDF_DETAILS,
                         " (header execution count %d)",
                         (int)loop->header->count.to_gcov_type ());
          dump_printf (MSG_OPTIMIZED_LOCATIONS | TDF_DETAILS, "\n");
        }
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      if (exit)
        fprintf (dump_file, "Exit condition of peeled iterations was "
		 "eliminated.\n");
      if (edge_to_cancel)
        fprintf (dump_file, "Last iteration exit edge was proved true.\n");
      else
        fprintf (dump_file, "Latch of last iteration was marked by "
		 "__builtin_unreachable ().\n");
    }

  return true;
}

/* Return number of instructions after peeling.  */
static unsigned HOST_WIDE_INT
estimated_peeled_sequence_size (struct loop_size *size,
			        unsigned HOST_WIDE_INT npeel)
{
  return MAX (npeel * (HOST_WIDE_INT) (size->overall
			     	       - size->eliminated_by_peeling), 1);
}

/* If the loop is expected to iterate N times and is
   small enough, duplicate the loop body N+1 times before
   the loop itself.  This way the hot path will never
   enter the loop.  
   Parameters are the same as for try_unroll_loops_completely */

static bool
try_peel_loop (struct loop *loop,
	       edge exit, tree niter,
	       HOST_WIDE_INT maxiter)
{
  HOST_WIDE_INT npeel;
  struct loop_size size;
  int peeled_size;

  if (!flag_peel_loops || PARAM_VALUE (PARAM_MAX_PEEL_TIMES) <= 0
      || !peeled_loops)
    return false;

  if (bitmap_bit_p (peeled_loops, loop->num))
    {
      if (dump_file)
        fprintf (dump_file, "Not peeling: loop is already peeled\n");
      return false;
    }

  /* Peel only innermost loops.
     While the code is perfectly capable of peeling non-innermost loops,
     the heuristics would probably need some improvements. */
  if (loop->inner)
    {
      if (dump_file)
        fprintf (dump_file, "Not peeling: outer loop\n");
      return false;
    }

  if (!optimize_loop_for_speed_p (loop))
    {
      if (dump_file)
        fprintf (dump_file, "Not peeling: cold loop\n");
      return false;
    }

  /* Check if there is an estimate on the number of iterations.  */
  npeel = estimated_loop_iterations_int (loop);
  if (npeel < 0)
    npeel = likely_max_loop_iterations_int (loop);
  if (npeel < 0)
    {
      if (dump_file)
        fprintf (dump_file, "Not peeling: number of iterations is not "
	         "estimated\n");
      return false;
    }
  if (maxiter >= 0 && maxiter <= npeel)
    {
      if (dump_file)
        fprintf (dump_file, "Not peeling: upper bound is known so can "
		 "unroll completely\n");
      return false;
    }

  /* We want to peel estimated number of iterations + 1 (so we never
     enter the loop on quick path).  Check against PARAM_MAX_PEEL_TIMES
     and be sure to avoid overflows.  */
  if (npeel > PARAM_VALUE (PARAM_MAX_PEEL_TIMES) - 1)
    {
      if (dump_file)
        fprintf (dump_file, "Not peeling: rolls too much "
		 "(%i + 1 > --param max-peel-times)\n", (int) npeel);
      return false;
    }
  npeel++;

  /* Check peeled loops size.  */
  tree_estimate_loop_size (loop, exit, NULL, &size,
			   PARAM_VALUE (PARAM_MAX_PEELED_INSNS));
  if ((peeled_size = estimated_peeled_sequence_size (&size, (int) npeel))
      > PARAM_VALUE (PARAM_MAX_PEELED_INSNS))
    {
      if (dump_file)
        fprintf (dump_file, "Not peeling: peeled sequence size is too large "
		 "(%i insns > --param max-peel-insns)", peeled_size);
      return false;
    }

  /* Duplicate possibly eliminating the exits.  */
  initialize_original_copy_tables ();
  auto_sbitmap wont_exit (npeel + 1);
  if (exit && niter
      && TREE_CODE (niter) == INTEGER_CST
      && wi::leu_p (npeel, wi::to_widest (niter)))
    {
      bitmap_ones (wont_exit);
      bitmap_clear_bit (wont_exit, 0);
    }
  else
    {
      exit = NULL;
      bitmap_clear (wont_exit);
    }
  if (!gimple_duplicate_loop_to_header_edge (loop, loop_preheader_edge (loop),
					     npeel, wont_exit,
					     exit, &edges_to_remove,
					     DLTHE_FLAG_UPDATE_FREQ))
    {
      free_original_copy_tables ();
      return false;
    }
  free_original_copy_tables ();
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Peeled loop %d, %i times.\n",
	       loop->num, (int) npeel);
    }
  if (loop->any_estimate)
    {
      if (wi::ltu_p (npeel, loop->nb_iterations_estimate))
        loop->nb_iterations_estimate -= npeel;
      else
	loop->nb_iterations_estimate = 0;
    }
  if (loop->any_upper_bound)
    {
      if (wi::ltu_p (npeel, loop->nb_iterations_upper_bound))
        loop->nb_iterations_upper_bound -= npeel;
      else
        loop->nb_iterations_upper_bound = 0;
    }
  if (loop->any_likely_upper_bound)
    {
      if (wi::ltu_p (npeel, loop->nb_iterations_likely_upper_bound))
	loop->nb_iterations_likely_upper_bound -= npeel;
      else
	{
	  loop->any_estimate = true;
	  loop->nb_iterations_estimate = 0;
	  loop->nb_iterations_likely_upper_bound = 0;
	}
    }
  profile_count entry_count = profile_count::zero ();
  int entry_freq = 0;

  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, loop->header->preds)
    if (e->src != loop->latch)
      {
	if (e->src->count.initialized_p ())
	  entry_count = e->src->count + e->src->count;
	entry_freq += e->src->frequency;
	gcc_assert (!flow_bb_inside_loop_p (loop, e->src));
      }
  profile_probability p = profile_probability::very_unlikely ();
  if (loop->header->count > 0)
    p = entry_count.probability_in (loop->header->count);
  else if (loop->header->frequency)
    p = profile_probability::probability_in_gcov_type
		 (entry_freq, loop->header->frequency);
  scale_loop_profile (loop, p, 0);
  bitmap_set_bit (peeled_loops, loop->num);
  return true;
}
/* Adds a canonical induction variable to LOOP if suitable.
   CREATE_IV is true if we may create a new iv.  UL determines
   which loops we are allowed to completely unroll.  If TRY_EVAL is true, we try
   to determine the number of iterations of a loop by direct evaluation.
   Returns true if cfg is changed.   */

static bool
canonicalize_loop_induction_variables (struct loop *loop,
				       bool create_iv, enum unroll_level ul,
				       bool try_eval)
{
  edge exit = NULL;
  tree niter;
  HOST_WIDE_INT maxiter;
  bool modified = false;
  location_t locus = UNKNOWN_LOCATION;

  niter = number_of_latch_executions (loop);
  exit = single_exit (loop);
  if (TREE_CODE (niter) == INTEGER_CST)
    locus = gimple_location (last_stmt (exit->src));
  else
    {
      /* If the loop has more than one exit, try checking all of them
	 for # of iterations determinable through scev.  */
      if (!exit)
	niter = find_loop_niter (loop, &exit);

      /* Finally if everything else fails, try brute force evaluation.  */
      if (try_eval
	  && (chrec_contains_undetermined (niter)
	      || TREE_CODE (niter) != INTEGER_CST))
	niter = find_loop_niter_by_eval (loop, &exit);

      if (exit)
        locus = gimple_location (last_stmt (exit->src));

      if (TREE_CODE (niter) != INTEGER_CST)
	exit = NULL;
    }

  /* We work exceptionally hard here to estimate the bound
     by find_loop_niter_by_eval.  Be sure to keep it for future.  */
  if (niter && TREE_CODE (niter) == INTEGER_CST)
    {
      record_niter_bound (loop, wi::to_widest (niter),
			  exit == single_likely_exit (loop), true);
    }

  /* Force re-computation of loop bounds so we can remove redundant exits.  */
  maxiter = max_loop_iterations_int (loop);

  if (dump_file && (dump_flags & TDF_DETAILS)
      && TREE_CODE (niter) == INTEGER_CST)
    {
      fprintf (dump_file, "Loop %d iterates ", loop->num);
      print_generic_expr (dump_file, niter, TDF_SLIM);
      fprintf (dump_file, " times.\n");
    }
  if (dump_file && (dump_flags & TDF_DETAILS)
      && maxiter >= 0)
    {
      fprintf (dump_file, "Loop %d iterates at most %i times.\n", loop->num,
	       (int)maxiter);
    }
  if (dump_file && (dump_flags & TDF_DETAILS)
      && likely_max_loop_iterations_int (loop) >= 0)
    {
      fprintf (dump_file, "Loop %d likely iterates at most %i times.\n",
	       loop->num, (int)likely_max_loop_iterations_int (loop));
    }

  /* Remove exits that are known to be never taken based on loop bound.
     Needs to be called after compilation of max_loop_iterations_int that
     populates the loop bounds.  */
  modified |= remove_redundant_iv_tests (loop);

  if (try_unroll_loop_completely (loop, exit, niter, ul, maxiter, locus))
    return true;

  if (create_iv
      && niter && !chrec_contains_undetermined (niter)
      && exit && just_once_each_iteration_p (loop, exit->src))
    create_canonical_iv (loop, exit, niter);

  if (ul == UL_ALL)
    modified |= try_peel_loop (loop, exit, niter, maxiter);

  return modified;
}

/* The main entry point of the pass.  Adds canonical induction variables
   to the suitable loops.  */

unsigned int
canonicalize_induction_variables (void)
{
  struct loop *loop;
  bool changed = false;
  bool irred_invalidated = false;
  bitmap loop_closed_ssa_invalidated = BITMAP_ALLOC (NULL);

  estimate_numbers_of_iterations (cfun);

  FOR_EACH_LOOP (loop, LI_FROM_INNERMOST)
    {
      changed |= canonicalize_loop_induction_variables (loop,
							true, UL_SINGLE_ITER,
							true);
    }
  gcc_assert (!need_ssa_update_p (cfun));

  unloop_loops (loop_closed_ssa_invalidated, &irred_invalidated);
  if (irred_invalidated
      && loops_state_satisfies_p (LOOPS_HAVE_MARKED_IRREDUCIBLE_REGIONS))
    mark_irreducible_loops ();

  /* Clean up the information about numbers of iterations, since brute force
     evaluation could reveal new information.  */
  free_numbers_of_iterations_estimates (cfun);
  scev_reset ();

  if (!bitmap_empty_p (loop_closed_ssa_invalidated))
    {
      gcc_checking_assert (loops_state_satisfies_p (LOOP_CLOSED_SSA));
      rewrite_into_loop_closed_ssa (NULL, TODO_update_ssa);
    }
  BITMAP_FREE (loop_closed_ssa_invalidated);

  if (changed)
    return TODO_cleanup_cfg;
  return 0;
}

/* Propagate constant SSA_NAMEs defined in basic block BB.  */

static void
propagate_constants_for_unrolling (basic_block bb)
{
  /* Look for degenerate PHI nodes with constant argument.  */
  for (gphi_iterator gsi = gsi_start_phis (bb); !gsi_end_p (gsi); )
    {
      gphi *phi = gsi.phi ();
      tree result = gimple_phi_result (phi);
      tree arg = gimple_phi_arg_def (phi, 0);

      if (! SSA_NAME_OCCURS_IN_ABNORMAL_PHI (result)
	  && gimple_phi_num_args (phi) == 1
	  && CONSTANT_CLASS_P (arg))
	{
	  replace_uses_by (result, arg);
	  gsi_remove (&gsi, true);
	  release_ssa_name (result);
	}
      else
	gsi_next (&gsi);
    }

  /* Look for assignments to SSA names with constant RHS.  */
  for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi); )
    {
      gimple *stmt = gsi_stmt (gsi);
      tree lhs;

      if (is_gimple_assign (stmt)
	  && TREE_CODE_CLASS (gimple_assign_rhs_code (stmt)) == tcc_constant
	  && (lhs = gimple_assign_lhs (stmt), TREE_CODE (lhs) == SSA_NAME)
	  && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs))
	{
	  replace_uses_by (lhs, gimple_assign_rhs1 (stmt));
	  gsi_remove (&gsi, true);
	  release_ssa_name (lhs);
	}
      else
	gsi_next (&gsi);
    }
}

/* Process loops from innermost to outer, stopping at the innermost
   loop we unrolled.  */

static bool
tree_unroll_loops_completely_1 (bool may_increase_size, bool unroll_outer,
				bitmap father_bbs, struct loop *loop)
{
  struct loop *loop_father;
  bool changed = false;
  struct loop *inner;
  enum unroll_level ul;

  /* Process inner loops first.  */
  for (inner = loop->inner; inner != NULL; inner = inner->next)
    changed |= tree_unroll_loops_completely_1 (may_increase_size,
					       unroll_outer, father_bbs,
					       inner);
 
  /* If we changed an inner loop we cannot process outer loops in this
     iteration because SSA form is not up-to-date.  Continue with
     siblings of outer loops instead.  */
  if (changed)
    return true;

  /* Don't unroll #pragma omp simd loops until the vectorizer
     attempts to vectorize those.  */
  if (loop->force_vectorize)
    return false;

  /* Try to unroll this loop.  */
  loop_father = loop_outer (loop);
  if (!loop_father)
    return false;

  if (may_increase_size && optimize_loop_nest_for_speed_p (loop)
      /* Unroll outermost loops only if asked to do so or they do
	 not cause code growth.  */
      && (unroll_outer || loop_outer (loop_father)))
    ul = UL_ALL;
  else
    ul = UL_NO_GROWTH;

  if (canonicalize_loop_induction_variables
        (loop, false, ul, !flag_tree_loop_ivcanon))
    {
      /* If we'll continue unrolling, we need to propagate constants
	 within the new basic blocks to fold away induction variable
	 computations; otherwise, the size might blow up before the
	 iteration is complete and the IR eventually cleaned up.  */
      if (loop_outer (loop_father))
	bitmap_set_bit (father_bbs, loop_father->header->index);

      return true;
    }

  return false;
}

/* Unroll LOOPS completely if they iterate just few times.  Unless
   MAY_INCREASE_SIZE is true, perform the unrolling only if the
   size of the code does not increase.  */

unsigned int
tree_unroll_loops_completely (bool may_increase_size, bool unroll_outer)
{
  bitmap father_bbs = BITMAP_ALLOC (NULL);
  bool changed;
  int iteration = 0;
  bool irred_invalidated = false;

  estimate_numbers_of_iterations (cfun);

  do
    {
      changed = false;
      bitmap loop_closed_ssa_invalidated = NULL;

      if (loops_state_satisfies_p (LOOP_CLOSED_SSA))
	loop_closed_ssa_invalidated = BITMAP_ALLOC (NULL);

      free_numbers_of_iterations_estimates (cfun);
      estimate_numbers_of_iterations (cfun);

      changed = tree_unroll_loops_completely_1 (may_increase_size,
						unroll_outer, father_bbs,
						current_loops->tree_root);
      if (changed)
	{
	  unsigned i;

          unloop_loops (loop_closed_ssa_invalidated, &irred_invalidated);

	  /* We can not use TODO_update_ssa_no_phi because VOPS gets confused.  */
	  if (loop_closed_ssa_invalidated
	      && !bitmap_empty_p (loop_closed_ssa_invalidated))
            rewrite_into_loop_closed_ssa (loop_closed_ssa_invalidated,
					  TODO_update_ssa);
	  else
	    update_ssa (TODO_update_ssa);

	  /* father_bbs is a bitmap of loop father header BB indices.
	     Translate that to what non-root loops these BBs belong to now.  */
	  bitmap_iterator bi;
	  bitmap fathers = BITMAP_ALLOC (NULL);
	  EXECUTE_IF_SET_IN_BITMAP (father_bbs, 0, i, bi)
	    {
	      basic_block unrolled_loop_bb = BASIC_BLOCK_FOR_FN (cfun, i);
	      if (! unrolled_loop_bb)
		continue;
	      if (loop_outer (unrolled_loop_bb->loop_father))
		bitmap_set_bit (fathers,
				unrolled_loop_bb->loop_father->num);
	    }
	  bitmap_clear (father_bbs);
	  /* Propagate the constants within the new basic blocks.  */
	  EXECUTE_IF_SET_IN_BITMAP (fathers, 0, i, bi)
	    {
	      loop_p father = get_loop (cfun, i);
	      basic_block *body = get_loop_body_in_dom_order (father);
	      for (unsigned j = 0; j < father->num_nodes; j++)
		propagate_constants_for_unrolling (body[j]);
	      free (body);
	    }
	  BITMAP_FREE (fathers);

	  /* This will take care of removing completely unrolled loops
	     from the loop structures so we can continue unrolling now
	     innermost loops.  */
	  if (cleanup_tree_cfg ())
	    update_ssa (TODO_update_ssa_only_virtuals);

	  /* Clean up the information about numbers of iterations, since
	     complete unrolling might have invalidated it.  */
	  scev_reset ();
	  if (flag_checking && loops_state_satisfies_p (LOOP_CLOSED_SSA))
	    verify_loop_closed_ssa (true);
	}
      if (loop_closed_ssa_invalidated)
        BITMAP_FREE (loop_closed_ssa_invalidated);
    }
  while (changed
	 && ++iteration <= PARAM_VALUE (PARAM_MAX_UNROLL_ITERATIONS));

  BITMAP_FREE (father_bbs);

  if (irred_invalidated
      && loops_state_satisfies_p (LOOPS_HAVE_MARKED_IRREDUCIBLE_REGIONS))
    mark_irreducible_loops ();

  return 0;
}

/* Canonical induction variable creation pass.  */

namespace {

const pass_data pass_data_iv_canon =
{
  GIMPLE_PASS, /* type */
  "ivcanon", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  TV_TREE_LOOP_IVCANON, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_iv_canon : public gimple_opt_pass
{
public:
  pass_iv_canon (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_iv_canon, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return flag_tree_loop_ivcanon != 0; }
  virtual unsigned int execute (function *fun);

}; // class pass_iv_canon

unsigned int
pass_iv_canon::execute (function *fun)
{
  if (number_of_loops (fun) <= 1)
    return 0;

  return canonicalize_induction_variables ();
}

} // anon namespace

gimple_opt_pass *
make_pass_iv_canon (gcc::context *ctxt)
{
  return new pass_iv_canon (ctxt);
}

/* Complete unrolling of loops.  */

namespace {

const pass_data pass_data_complete_unroll =
{
  GIMPLE_PASS, /* type */
  "cunroll", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  TV_COMPLETE_UNROLL, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_complete_unroll : public gimple_opt_pass
{
public:
  pass_complete_unroll (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_complete_unroll, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *);

}; // class pass_complete_unroll

unsigned int
pass_complete_unroll::execute (function *fun)
{
  if (number_of_loops (fun) <= 1)
    return 0;

  /* If we ever decide to run loop peeling more than once, we will need to
     track loops already peeled in loop structures themselves to avoid
     re-peeling the same loop multiple times.  */
  if (flag_peel_loops)
    peeled_loops = BITMAP_ALLOC (NULL);
  int val = tree_unroll_loops_completely (flag_unroll_loops
					  || flag_peel_loops
					  || optimize >= 3, true);
  if (peeled_loops)
    {
      BITMAP_FREE (peeled_loops);
      peeled_loops = NULL;
    }
  return val;
}

} // anon namespace

gimple_opt_pass *
make_pass_complete_unroll (gcc::context *ctxt)
{
  return new pass_complete_unroll (ctxt);
}

/* Complete unrolling of inner loops.  */

namespace {

const pass_data pass_data_complete_unrolli =
{
  GIMPLE_PASS, /* type */
  "cunrolli", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  TV_COMPLETE_UNROLL, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_complete_unrolli : public gimple_opt_pass
{
public:
  pass_complete_unrolli (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_complete_unrolli, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return optimize >= 2; }
  virtual unsigned int execute (function *);

}; // class pass_complete_unrolli

unsigned int
pass_complete_unrolli::execute (function *fun)
{
  unsigned ret = 0;

  loop_optimizer_init (LOOPS_NORMAL
		       | LOOPS_HAVE_RECORDED_EXITS);
  if (number_of_loops (fun) > 1)
    {
      scev_initialize ();
      ret = tree_unroll_loops_completely (optimize >= 3, false);
      scev_finalize ();
    }
  loop_optimizer_finalize ();

  return ret;
}

} // anon namespace

gimple_opt_pass *
make_pass_complete_unrolli (gcc::context *ctxt)
{
  return new pass_complete_unrolli (ctxt);
}


