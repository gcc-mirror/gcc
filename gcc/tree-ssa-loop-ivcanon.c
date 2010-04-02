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
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "output.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "cfgloop.h"
#include "tree-pass.h"
#include "ggc.h"
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
      if ((DECL_P (base)
      	   && TREE_STATIC (base)
	   && TREE_READONLY (base)
           && (DECL_INITIAL (base)
	       || (!DECL_EXTERNAL (base)
		   && targetm.binds_local_p (base))))
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
tree_estimate_loop_size (struct loop *loop, edge exit, struct loop_size *size)
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
      if (exit && body[i] != exit->src
	  && dominated_by_p (CDI_DOMINATORS, body[i], exit->src))
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
	  if (body[i] == exit->src && stmt == last_stmt (exit->src))
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

/* Tries to unroll LOOP completely, i.e. NITER times.
   UL determines which loops we are allowed to unroll.
   EXIT is the exit of the loop that should be eliminated.  */

static bool
try_unroll_loop_completely (struct loop *loop,
			    edge exit, tree niter,
			    enum unroll_level ul)
{
  unsigned HOST_WIDE_INT n_unroll, ninsns, max_unroll, unr_insns;
  gimple cond;
  struct loop_size size;

  if (loop->inner)
    return false;

  if (!host_integerp (niter, 1))
    return false;
  n_unroll = tree_low_cst (niter, 1);

  max_unroll = PARAM_VALUE (PARAM_MAX_COMPLETELY_PEEL_TIMES);
  if (n_unroll > max_unroll)
    return false;

  if (n_unroll)
    {
      if (ul == UL_SINGLE_ITER)
	return false;

      tree_estimate_loop_size (loop, exit, &size);
      ninsns = size.overall;

      unr_insns = estimated_unrolled_size (&size, n_unroll);
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "  Loop size: %d\n", (int) ninsns);
	  fprintf (dump_file, "  Estimated size after unrolling: %d\n",
		   (int) unr_insns);
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
	    fprintf (dump_file, "Not unrolling loop %d.\n", loop->num);
	  return false;
	}
    }

  if (n_unroll)
    {
      sbitmap wont_exit;
      edge e;
      unsigned i;
      VEC (edge, heap) *to_remove = NULL;

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

      for (i = 0; VEC_iterate (edge, to_remove, i, e); i++)
	{
	  bool ok = remove_path (e);
	  gcc_assert (ok);
	}

      VEC_free (edge, heap, to_remove);
      free (wont_exit);
      free_original_copy_tables ();
    }

  cond = last_stmt (exit->src);
  if (exit->flags & EDGE_TRUE_VALUE)
    gimple_cond_make_true (cond);
  else
    gimple_cond_make_false (cond);
  update_stmt (cond);
  update_ssa (TODO_update_ssa);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Unrolled loop %d completely.\n", loop->num);

  return true;
}

/* Adds a canonical induction variable to LOOP if suitable.
   CREATE_IV is true if we may create a new iv.  UL determines
   which loops we are allowed to completely unroll.  If TRY_EVAL is true, we try
   to determine the number of iterations of a loop by direct evaluation.
   Returns true if cfg is changed.  */

static bool
canonicalize_loop_induction_variables (struct loop *loop,
				       bool create_iv, enum unroll_level ul,
				       bool try_eval)
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

      if (chrec_contains_undetermined (niter)
	  || TREE_CODE (niter) != INTEGER_CST)
	return false;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Loop %d iterates ", loop->num);
      print_generic_expr (dump_file, niter, TDF_SLIM);
      fprintf (dump_file, " times.\n");
    }

  if (try_unroll_loop_completely (loop, exit, niter, ul))
    return true;

  if (create_iv)
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

  FOR_EACH_LOOP (li, loop, 0)
    {
      changed |= canonicalize_loop_induction_variables (loop,
							true, UL_SINGLE_ITER,
							true);
    }

  /* Clean up the information about numbers of iterations, since brute force
     evaluation could reveal new information.  */
  scev_reset ();

  if (changed)
    return TODO_cleanup_cfg;
  return 0;
}

/* Unroll LOOPS completely if they iterate just few times.  Unless
   MAY_INCREASE_SIZE is true, perform the unrolling only if the
   size of the code does not increase.  */

unsigned int
tree_unroll_loops_completely (bool may_increase_size, bool unroll_outer)
{
  loop_iterator li;
  struct loop *loop;
  bool changed;
  enum unroll_level ul;
  int iteration = 0;

  do
    {
      changed = false;

      FOR_EACH_LOOP (li, loop, LI_ONLY_INNERMOST)
	{
	  if (may_increase_size && optimize_loop_for_speed_p (loop)
	      /* Unroll outermost loops only if asked to do so or they do
		 not cause code growth.  */
	      && (unroll_outer
		  || loop_outer (loop_outer (loop))))
	    ul = UL_ALL;
	  else
	    ul = UL_NO_GROWTH;
	  changed |= canonicalize_loop_induction_variables
		       (loop, false, ul, !flag_tree_loop_ivcanon);
	}

      if (changed)
	{
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

  return 0;
}
