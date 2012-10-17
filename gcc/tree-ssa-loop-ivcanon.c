/* Induction variable canonicalization.
   Copyright (C) 2004, 2005, 2007, 2008, 2010
   Free Software Foundation, Inc.

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

   Additionally in case we detect that it is beneficial to unroll the
   loop completely, we do it right here to expose the optimization
   possibilities to the following passes.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "tm_p.h"
#include "basic-block.h"
#include "gimple-pretty-print.h"
#include "tree-flow.h"
#include "cfgloop.h"
#include "tree-pass.h"
#include "tree-chrec.h"
#include "tree-scalar-evolution.h"
#include "params.h"
#include "flags.h"
#include "tree-inline.h"
#include "target.h"

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
  gimple cond;
  gimple_stmt_iterator incr_at;
  enum tree_code cmp;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Added canonical iv to loop %d, ", loop->num);
      print_generic_expr (dump_file, niter, TDF_SLIM);
      fprintf (dump_file, " iterations.\n");
    }

  cond = last_stmt (exit->src);
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

/* Computes an estimated number of insns in LOOP, weighted by WEIGHTS.  */

unsigned
tree_num_loop_insns (struct loop *loop, eni_weights *weights)
{
  basic_block *body = get_loop_body (loop);
  gimple_stmt_iterator gsi;
  unsigned size = 0, i;

  for (i = 0; i < loop->num_nodes; i++)
    for (gsi = gsi_start_bb (body[i]); !gsi_end_p (gsi); gsi_next (&gsi))
      size += estimate_num_insns (gsi_stmt (gsi), weights);
  free (body);

  return size;
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
};

/* Return true if OP in STMT will be constant after peeling LOOP.  */

static bool
constant_after_peeling (tree op, gimple stmt, struct loop *loop)
{
  affine_iv iv;

  if (is_gimple_min_invariant (op))
    return true;

  /* We can still fold accesses to constant arrays when index is known.  */
  if (TREE_CODE (op) != SSA_NAME)
    {
      tree base = op;

      /* First make fast look if we see constant array inside.  */
      while (handled_component_p (base))
	base = TREE_OPERAND (base, 0);
      if ((DECL_P (base) == VAR_DECL
	   && const_value_known_p (base))
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

  /* Induction variables are constants.  */
  if (!simple_iv (loop, loop_containing_stmt (stmt), op, &iv, false))
    return false;
  if (!is_gimple_min_invariant (iv.base))
    return false;
  if (!is_gimple_min_invariant (iv.step))
    return false;
  return true;
}

/* Computes an estimated number of insns in LOOP, weighted by WEIGHTS.
   Return results in SIZE, estimate benefits for complete unrolling exiting by EXIT.  */

static void
tree_estimate_loop_size (struct loop *loop, edge exit, edge edge_to_cancel, struct loop_size *size)
{
  basic_block *body = get_loop_body (loop);
  gimple_stmt_iterator gsi;
  unsigned int i;
  bool after_exit;

  size->overall = 0;
  size->eliminated_by_peeling = 0;
  size->last_iteration = 0;
  size->last_iteration_eliminated_by_peeling = 0;

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
	fprintf (dump_file, " BB: %i, after_exit: %i\n", body[i]->index, after_exit);

      for (gsi = gsi_start_bb (body[i]); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple stmt = gsi_stmt (gsi);
	  int num = estimate_num_insns (stmt, &eni_size_weights);
	  bool likely_eliminated = false;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "  size: %3i ", num);
	      print_gimple_stmt (dump_file, gsi_stmt (gsi), 0, 0);
	    }

	  /* Look for reasons why we might optimize this stmt away. */

	  /* Exit conditional.  */
	  if (exit && body[i] == exit->src && stmt == last_stmt (exit->src))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
	        fprintf (dump_file, "   Exit condition will be eliminated.\n");
	      likely_eliminated = true;
	    }
	  /* Sets of IV variables  */
	  else if (gimple_code (stmt) == GIMPLE_ASSIGN
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
		   && constant_after_peeling (gimple_assign_rhs1 (stmt), stmt,loop)
		   && (gimple_assign_rhs_class (stmt) != GIMPLE_BINARY_RHS
		       || constant_after_peeling (gimple_assign_rhs2 (stmt),
		       				  stmt, loop)))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
	        fprintf (dump_file, "   Constant expression will be folded away.\n");
	      likely_eliminated = true;
	    }
	  /* Conditionals.  */
	  else if (gimple_code (stmt) == GIMPLE_COND
		   && constant_after_peeling (gimple_cond_lhs (stmt), stmt, loop)
		   && constant_after_peeling (gimple_cond_rhs (stmt), stmt, loop))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
	        fprintf (dump_file, "   Constant conditional.\n");
	      likely_eliminated = true;
	    }

	  size->overall += num;
	  if (likely_eliminated)
	    size->eliminated_by_peeling += num;
	  if (!after_exit)
	    {
	      size->last_iteration += num;
	      if (likely_eliminated)
		size->last_iteration_eliminated_by_peeling += num;
	    }
	}
    }
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "size: %i-%i, last_iteration: %i-%i\n", size->overall,
    	     size->eliminated_by_peeling, size->last_iteration,
	     size->last_iteration_eliminated_by_peeling);

  free (body);
}

/* Estimate number of insns of completely unrolled loop.
   It is (NUNROLL + 1) * size of loop body with taking into account
   the fact that in last copy everything after exit conditional
   is dead and that some instructions will be eliminated after
   peeling.

   Loop body is likely going to simplify futher, this is difficult
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

   After complette unrolling we still may get rid of the conditional
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

edge
loop_edge_to_cancel (struct loop *loop)
{
  VEC (edge, heap) *exits;
  unsigned i;
  edge edge_to_cancel;
  gimple_stmt_iterator gsi;

  /* We want only one predecestor of the loop.  */
  if (EDGE_COUNT (loop->latch->preds) > 1)
    return NULL;

  exits = get_loop_exit_edges (loop);

  FOR_EACH_VEC_ELT (edge, exits, i, edge_to_cancel)
    {
       /* Find the other edge than the loop exit
          leaving the conditoinal.  */
       if (EDGE_COUNT (edge_to_cancel->src->succs) != 2)
         continue;
       if (EDGE_SUCC (edge_to_cancel->src, 0) == edge_to_cancel)
         edge_to_cancel = EDGE_SUCC (edge_to_cancel->src, 1);
       else
         edge_to_cancel = EDGE_SUCC (edge_to_cancel->src, 0);

      /* We should never have conditionals in the loop latch. */
      gcc_assert (edge_to_cancel->dest != loop->header);

      /* Check that it leads to loop latch.  */
      if (edge_to_cancel->dest != loop->latch)
        continue;

      VEC_free (edge, heap, exits);

      /* Verify that the code in loop latch does nothing that may end program
         execution without really reaching the exit.  This may include
	 non-pure/const function calls, EH statements, volatile ASMs etc.  */
      for (gsi = gsi_start_bb (loop->latch); !gsi_end_p (gsi); gsi_next (&gsi))
	if (gimple_has_side_effects (gsi_stmt (gsi)))
	   return NULL;
      return edge_to_cancel;
    }
  VEC_free (edge, heap, exits);
  return NULL;
}

/* Tries to unroll LOOP completely, i.e. NITER times.
   UL determines which loops we are allowed to unroll.
   EXIT is the exit of the loop that should be eliminated.  
   IRRED_INVALIDATED is used to bookkeep if information about
   irreducible regions may become invalid as a result
   of the transformation.  */

static bool
try_unroll_loop_completely (struct loop *loop,
			    edge exit, tree niter,
			    enum unroll_level ul,
			    bool *irred_invalidated)
{
  unsigned HOST_WIDE_INT n_unroll, ninsns, max_unroll, unr_insns;
  gimple cond;
  struct loop_size size;
  bool n_unroll_found = false;
  HOST_WIDE_INT maxiter;
  basic_block latch;
  edge latch_edge;
  location_t locus;
  int flags;
  gimple stmt;
  gimple_stmt_iterator gsi;
  edge edge_to_cancel = NULL;
  int num = loop->num;

  /* See if we proved number of iterations to be low constant.

     EXIT is an edge that will be removed in all but last iteration of 
     the loop.

     EDGE_TO_CACNEL is an edge that will be removed from the last iteration
     of the unrolled sequence and is expected to make the final loop not
     rolling. 

     If the number of execution of loop is determined by standard induction
     variable test, then EXIT and EDGE_TO_CANCEL are the two edges leaving
     from the iv test.  */
  if (host_integerp (niter, 1))
    {
      n_unroll = tree_low_cst (niter, 1);
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
  maxiter = max_loop_iterations_int (loop);
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

  max_unroll = PARAM_VALUE (PARAM_MAX_COMPLETELY_PEEL_TIMES);
  if (n_unroll > max_unroll)
    return false;

  if (!edge_to_cancel)
    edge_to_cancel = loop_edge_to_cancel (loop);

  if (n_unroll)
    {
      sbitmap wont_exit;
      edge e;
      unsigned i;
      VEC (edge, heap) *to_remove = NULL;
      if (ul == UL_SINGLE_ITER)
	return false;

      tree_estimate_loop_size (loop, exit, edge_to_cancel, &size);
      ninsns = size.overall;

      unr_insns = estimated_unrolled_size (&size, n_unroll);
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "  Loop size: %d\n", (int) ninsns);
	  fprintf (dump_file, "  Estimated size after unrolling: %d\n",
		   (int) unr_insns);
	}

      /* We unroll only inner loops, because we do not consider it profitable
	 otheriwse.  We still can cancel loopback edge of not rolling loop;
	 this is always a good idea.  */
      if (loop->inner && unr_insns > ninsns)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Not unrolling loop %d:"
		     "it is not innermost and code would grow.\n",
		     loop->num);
	  return false;
	}

      if (unr_insns > ninsns
	  && (unr_insns
	      > (unsigned) PARAM_VALUE (PARAM_MAX_COMPLETELY_PEELED_INSNS)))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Not unrolling loop %d "
		     "(--param max-completely-peeled-insns limit reached).\n",
		     loop->num);
	  return false;
	}

      if (ul == UL_NO_GROWTH
	  && unr_insns > ninsns)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Not unrolling loop %d: size would grow.\n",
		     loop->num);
	  return false;
	}

      initialize_original_copy_tables ();
      wont_exit = sbitmap_alloc (n_unroll + 1);
      sbitmap_ones (wont_exit);
      RESET_BIT (wont_exit, 0);

      if (!gimple_duplicate_loop_to_header_edge (loop, loop_preheader_edge (loop),
						 n_unroll, wont_exit,
						 exit, &to_remove,
						 DLTHE_FLAG_UPDATE_FREQ
						 | DLTHE_FLAG_COMPLETTE_PEEL))
	{
          free_original_copy_tables ();
	  free (wont_exit);
	  return false;
	}

      FOR_EACH_VEC_ELT (edge, to_remove, i, e)
	{
	  bool ok = remove_path (e);
	  gcc_assert (ok);
	}

      VEC_free (edge, heap, to_remove);
      free (wont_exit);
      free_original_copy_tables ();
    }

  /* Remove the conditional from the last copy of the loop.  */
  if (edge_to_cancel)
    {
      cond = last_stmt (edge_to_cancel->src);
      if (edge_to_cancel->flags & EDGE_TRUE_VALUE)
	gimple_cond_make_false (cond);
      else
	gimple_cond_make_true (cond);
      update_stmt (cond);
      /* Do not remove the path. Doing so may remove outer loop
	 and confuse bookkeeping code in tree_unroll_loops_completelly.  */
    }
  /* We did not manage to cancel the loop.
     The loop latch remains reachable even if it will never be reached
     at runtime.  We must redirect it to somewhere, so create basic
     block containg __builtin_unreachable call for this reason.  */
  else
    {
      latch = loop->latch;
      latch_edge = loop_latch_edge (loop);
      flags = latch_edge->flags;
      locus = latch_edge->goto_locus;

      /* Unloop destroys the latch edge.  */
      unloop (loop, irred_invalidated);

      /* Create new basic block for the latch edge destination and wire
	 it in.  */
      stmt = gimple_build_call (builtin_decl_implicit (BUILT_IN_UNREACHABLE), 0);
      latch_edge = make_edge (latch, create_basic_block (NULL, NULL, latch), flags);
      latch_edge->probability = 0;
      latch_edge->count = 0;
      latch_edge->flags |= flags;
      latch_edge->goto_locus = locus;

      latch_edge->dest->loop_father = current_loops->tree_root;
      latch_edge->dest->count = 0;
      latch_edge->dest->frequency = 0;
      set_immediate_dominator (CDI_DOMINATORS, latch_edge->dest, latch_edge->src);

      gsi = gsi_start_bb (latch_edge->dest);
      gsi_insert_after (&gsi, stmt, GSI_NEW_STMT);
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      if (!n_unroll)
        fprintf (dump_file, "Turned loop %d to non-loop; it never loops.\n",
		 num);
      else
        fprintf (dump_file, "Unrolled loop %d completely "
		 "(duplicated %i times).\n", num, (int)n_unroll);
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

/* Adds a canonical induction variable to LOOP if suitable.
   CREATE_IV is true if we may create a new iv.  UL determines
   which loops we are allowed to completely unroll.  If TRY_EVAL is true, we try
   to determine the number of iterations of a loop by direct evaluation.
   Returns true if cfg is changed.  

   IRRED_INVALIDATED is used to keep if irreducible reginos needs to be recomputed.  */

static bool
canonicalize_loop_induction_variables (struct loop *loop,
				       bool create_iv, enum unroll_level ul,
				       bool try_eval,
				       bool *irred_invalidated)
{
  edge exit = NULL;
  tree niter;

  niter = number_of_latch_executions (loop);
  if (TREE_CODE (niter) == INTEGER_CST)
    {
      exit = single_exit (loop);
      if (!just_once_each_iteration_p (loop, exit->src))
	return false;
    }
  else
    {
      /* If the loop has more than one exit, try checking all of them
	 for # of iterations determinable through scev.  */
      if (!single_exit (loop))
	niter = find_loop_niter (loop, &exit);

      /* Finally if everything else fails, try brute force evaluation.  */
      if (try_eval
	  && (chrec_contains_undetermined (niter)
	      || TREE_CODE (niter) != INTEGER_CST))
	niter = find_loop_niter_by_eval (loop, &exit);

      if (TREE_CODE (niter) != INTEGER_CST)
	exit = NULL;
    }

  /* We work exceptionally hard here to estimate the bound
     by find_loop_niter_by_eval.  Be sure to keep it for future.  */
  if (niter && TREE_CODE (niter) == INTEGER_CST)
    record_niter_bound (loop, tree_to_double_int (niter), false, true);

  if (dump_file && (dump_flags & TDF_DETAILS)
      && TREE_CODE (niter) == INTEGER_CST)
    {
      fprintf (dump_file, "Loop %d iterates ", loop->num);
      print_generic_expr (dump_file, niter, TDF_SLIM);
      fprintf (dump_file, " times.\n");
    }
  if (dump_file && (dump_flags & TDF_DETAILS)
      && max_loop_iterations_int (loop) >= 0)
    {
      fprintf (dump_file, "Loop %d iterates at most %i times.\n", loop->num,
	       (int)max_loop_iterations_int (loop));
    }

  if (try_unroll_loop_completely (loop, exit, niter, ul, irred_invalidated))
    return true;

  if (create_iv
      && niter && !chrec_contains_undetermined (niter))
    create_canonical_iv (loop, exit, niter);

  return false;
}

/* The main entry point of the pass.  Adds canonical induction variables
   to the suitable loops.  */

unsigned int
canonicalize_induction_variables (void)
{
  loop_iterator li;
  struct loop *loop;
  bool changed = false;
  bool irred_invalidated = false;

  FOR_EACH_LOOP (li, loop, 0)
    {
      changed |= canonicalize_loop_induction_variables (loop,
							true, UL_SINGLE_ITER,
							true,
							&irred_invalidated);
    }
  gcc_assert (!need_ssa_update_p (cfun));

  if (irred_invalidated
      && loops_state_satisfies_p (LOOPS_HAVE_MARKED_IRREDUCIBLE_REGIONS))
    mark_irreducible_loops ();

  /* Clean up the information about numbers of iterations, since brute force
     evaluation could reveal new information.  */
  scev_reset ();

  if (changed)
    return TODO_cleanup_cfg;
  return 0;
}

/* Propagate VAL into all uses of SSA_NAME.  */

static void
propagate_into_all_uses (tree ssa_name, tree val)
{
  imm_use_iterator iter;
  gimple use_stmt;

  FOR_EACH_IMM_USE_STMT (use_stmt, iter, ssa_name)
    {
      gimple_stmt_iterator use_stmt_gsi = gsi_for_stmt (use_stmt);
      use_operand_p use;

      FOR_EACH_IMM_USE_ON_STMT (use, iter)
	SET_USE (use, val);

      if (is_gimple_assign (use_stmt)
	  && get_gimple_rhs_class (gimple_assign_rhs_code (use_stmt))
	     == GIMPLE_SINGLE_RHS)
	{
	  tree rhs = gimple_assign_rhs1 (use_stmt);

	  if (TREE_CODE (rhs) == ADDR_EXPR)
	    recompute_tree_invariant_for_addr_expr (rhs);
	}

      fold_stmt_inplace (&use_stmt_gsi);
      update_stmt (use_stmt);
      maybe_clean_or_replace_eh_stmt (use_stmt, use_stmt);
    }
}

/* Propagate constant SSA_NAMEs defined in basic block BB.  */

static void
propagate_constants_for_unrolling (basic_block bb)
{
  gimple_stmt_iterator gsi;

  /* Look for degenerate PHI nodes with constant argument.  */
  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); )
    {
      gimple phi = gsi_stmt (gsi);
      tree result = gimple_phi_result (phi);
      tree arg = gimple_phi_arg_def (phi, 0);

      if (gimple_phi_num_args (phi) == 1 && TREE_CODE (arg) == INTEGER_CST)
	{
	  propagate_into_all_uses (result, arg);
	  gsi_remove (&gsi, true);
	  release_ssa_name (result);
	}
      else
	gsi_next (&gsi);
    }

  /* Look for assignments to SSA names with constant RHS.  */
  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); )
    {
      gimple stmt = gsi_stmt (gsi);
      tree lhs;

      if (is_gimple_assign (stmt)
	  && (lhs = gimple_assign_lhs (stmt), TREE_CODE (lhs) == SSA_NAME)
	  && gimple_assign_rhs_code (stmt) == INTEGER_CST)
	{
	  propagate_into_all_uses (lhs, gimple_assign_rhs1 (stmt));
	  gsi_remove (&gsi, true);
	  release_ssa_name (lhs);
	}
      else
	gsi_next (&gsi);
    }
}

/* Unroll LOOPS completely if they iterate just few times.  Unless
   MAY_INCREASE_SIZE is true, perform the unrolling only if the
   size of the code does not increase.  */

unsigned int
tree_unroll_loops_completely (bool may_increase_size, bool unroll_outer)
{
  VEC(loop_p,stack) *father_stack = VEC_alloc (loop_p, stack, 16);
  loop_iterator li;
  struct loop *loop;
  bool changed;
  enum unroll_level ul;
  int iteration = 0;

  do
    {
      bool irred_invalidated = false;
      changed = false;

      FOR_EACH_LOOP (li, loop, 0)
	{
	  struct loop *loop_father = loop_outer (loop);

	  if (may_increase_size && optimize_loop_for_speed_p (loop)
	      /* Unroll outermost loops only if asked to do so or they do
		 not cause code growth.  */
	      && (unroll_outer || loop_outer (loop_father)))
	    ul = UL_ALL;
	  else
	    ul = UL_NO_GROWTH;

	  if (canonicalize_loop_induction_variables (loop, false, ul,
						     !flag_tree_loop_ivcanon,
						     &irred_invalidated))
	    {
	      changed = true;
	      /* If we'll continue unrolling, we need to propagate constants
		 within the new basic blocks to fold away induction variable
		 computations; otherwise, the size might blow up before the
		 iteration is complete and the IR eventually cleaned up.  */
	      if (loop_outer (loop_father) && !loop_father->aux)
	        {
		  VEC_safe_push (loop_p, stack, father_stack, loop_father);
		  loop_father->aux = loop_father;
		}
	    }
	}

      if (changed)
	{
	  struct loop **iter;
	  unsigned i;

	  if (irred_invalidated
	      && loops_state_satisfies_p (LOOPS_HAVE_MARKED_IRREDUCIBLE_REGIONS))
	    mark_irreducible_loops ();

	  update_ssa (TODO_update_ssa);

	  /* Propagate the constants within the new basic blocks.  */
	  FOR_EACH_VEC_ELT (loop_p, father_stack, i, iter)
	    {
	      unsigned j;
	      basic_block *body = get_loop_body_in_dom_order (*iter);
	      for (j = 0; j < (*iter)->num_nodes; j++)
		propagate_constants_for_unrolling (body[j]);
	      free (body);
	      (*iter)->aux = NULL;
	    }
	  VEC_truncate (loop_p, father_stack, 0);

	  /* This will take care of removing completely unrolled loops
	     from the loop structures so we can continue unrolling now
	     innermost loops.  */
	  if (cleanup_tree_cfg ())
	    update_ssa (TODO_update_ssa_only_virtuals);

	  /* Clean up the information about numbers of iterations, since
	     complete unrolling might have invalidated it.  */
	  scev_reset ();
	}
    }
  while (changed
	 && ++iteration <= PARAM_VALUE (PARAM_MAX_UNROLL_ITERATIONS));

  VEC_free (loop_p, stack, father_stack);

  return 0;
}
