/* Induction variable canonicalization.
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.
   
This file is part of GCC.
   
GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.
   
GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.
   
You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

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

/* Adds a canonical induction variable to LOOP iterating NITER times.  EXIT
   is the exit edge whose condition is replaced.  */

static void
create_canonical_iv (struct loop *loop, edge exit, tree niter)
{
  edge in;
  tree cond, type, var;
  block_stmt_iterator incr_at;
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
  niter = fold (build2 (PLUS_EXPR, type,
			niter,
			build_int_cst (type, 1)));
  incr_at = bsi_last (in->src);
  create_iv (niter,
	     fold_convert (type, integer_minus_one_node),
	     NULL_TREE, loop,
	     &incr_at, false, NULL, &var);

  cmp = (exit->flags & EDGE_TRUE_VALUE) ? EQ_EXPR : NE_EXPR;
  COND_EXPR_COND (cond) = build2 (cmp, boolean_type_node,
				  var,
				  build_int_cst (type, 0));
  modify_stmt (cond);
}

/* Computes an estimated number of insns in LOOP.  */

unsigned
tree_num_loop_insns (struct loop *loop)
{
  basic_block *body = get_loop_body (loop);
  block_stmt_iterator bsi;
  unsigned size = 1, i;

  for (i = 0; i < loop->num_nodes; i++)
    for (bsi = bsi_start (body[i]); !bsi_end_p (bsi); bsi_next (&bsi))
      size += estimate_num_insns (bsi_stmt (bsi));
  free (body);

  return size;
}

/* Tries to unroll LOOP completely, i.e. NITER times.  LOOPS is the
   loop tree.  COMPLETELY_UNROLL is true if we should unroll the loop
   even if it may cause code growth.  EXIT is the exit of the loop
   that should be eliminated.  */

static bool
try_unroll_loop_completely (struct loops *loops ATTRIBUTE_UNUSED,
			    struct loop *loop,
			    edge exit, tree niter,
			    bool completely_unroll)
{
  unsigned HOST_WIDE_INT n_unroll, ninsns, max_unroll;
  tree old_cond, cond, dont_exit, do_exit;

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
      if (!completely_unroll)
	return false;

      ninsns = tree_num_loop_insns (loop);

      if (n_unroll * ninsns
	  > (unsigned) PARAM_VALUE (PARAM_MAX_COMPLETELY_PEELED_INSNS))
	return false;
    }

  if (exit->flags & EDGE_TRUE_VALUE)
    {
      dont_exit = boolean_false_node;
      do_exit = boolean_true_node;
    }
  else
    {
      dont_exit = boolean_true_node;
      do_exit = boolean_false_node;
    }
  cond = last_stmt (exit->src);
    
  if (n_unroll)
    {
      old_cond = COND_EXPR_COND (cond);
      COND_EXPR_COND (cond) = dont_exit;
      modify_stmt (cond);

      if (!tree_duplicate_loop_to_header_edge (loop, loop_preheader_edge (loop),
					       loops, n_unroll, NULL,
					       NULL, NULL, NULL, 0))
	{
	  COND_EXPR_COND (cond) = old_cond;
	  return false;
	}
    }
  
  COND_EXPR_COND (cond) = do_exit;
  modify_stmt (cond);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Unrolled loop %d completely.\n", loop->num);

  return true;
}

/* Adds a canonical induction variable to LOOP if suitable.  LOOPS is the loops
   tree.  CREATE_IV is true if we may create a new iv.  COMPLETELY_UNROLL is
   true if we should do complete unrolling even if it may cause the code
   growth.  If TRY_EVAL is true, we try to determine the number of iterations
   of a loop by direct evaluation.  Returns true if cfg is changed.  */

static bool
canonicalize_loop_induction_variables (struct loops *loops, struct loop *loop,
				       bool create_iv, bool completely_unroll,
				       bool try_eval)
{
  edge exit = NULL;
  tree niter;

  niter = number_of_iterations_in_loop (loop);
  if (TREE_CODE (niter) == INTEGER_CST)
    {
      exit = loop->single_exit;
      if (!just_once_each_iteration_p (loop, exit->src))
	return false;

      /* The result of number_of_iterations_in_loop is by one higher than
	 we expect (i.e. it returns number of executions of the exit
	 condition, not of the loop latch edge).  */
      niter = fold (build2 (MINUS_EXPR, TREE_TYPE (niter), niter,
			    build_int_cst (TREE_TYPE (niter), 1)));
    }
  else
    {
      /* If the loop has more than one exit, try checking all of them
	 for # of iterations determinable through scev.  */
      if (!loop->single_exit)
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

  if (try_unroll_loop_completely (loops, loop, exit, niter, completely_unroll))
    return true;

  if (create_iv)
    create_canonical_iv (loop, exit, niter);

  return false;
}

/* The main entry point of the pass.  Adds canonical induction variables
   to the suitable LOOPS.  */

void
canonicalize_induction_variables (struct loops *loops)
{
  unsigned i;
  struct loop *loop;
  
  for (i = 1; i < loops->num; i++)
    {
      loop = loops->parray[i];

      if (loop)
	canonicalize_loop_induction_variables (loops, loop, true, false, true);
    }

  /* Clean up the information about numbers of iterations, since brute force
     evaluation could reveal new information.  */
  scev_reset ();

#if 0
  /* The necessary infrastructure is not in yet.  */
  if (changed)
    cleanup_tree_cfg_loop ();
#endif
}

/* Unroll LOOPS completely if they iterate just few times.  */

void
tree_unroll_loops_completely (struct loops *loops)
{
  unsigned i;
  struct loop *loop;
  bool changed = false;

  for (i = 1; i < loops->num; i++)
    {
      loop = loops->parray[i];

      if (!loop)
	continue;

      changed |= canonicalize_loop_induction_variables (loops, loop,
							false, true,
							!flag_tree_loop_ivcanon);
    }

  /* Clean up the information about numbers of iterations, since complete
     unrolling might have invalidated it.  */
  scev_reset ();

#if 0
  /* The necessary infrastructure is not in yet.  */
  if (changed)
    cleanup_tree_cfg_loop ();
#endif
}
