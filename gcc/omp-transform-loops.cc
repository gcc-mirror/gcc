/* OMP loop transformation pass. Transforms loops according to
   loop transformations directives such as "omp unroll".

   Copyright (C) 2023 Free Software Foundation, Inc.

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
#include "pretty-print.h"
#include "diagnostic-core.h"
#include "backend.h"
#include "target.h"
#include "tree.h"
#include "tree-inline.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "tree-pass.h"
#include "gimple-walk.h"
#include "gimple-pretty-print.h"
#include "gimplify.h"
#include "ssa.h"
#include "tree-into-ssa.h"
#include "fold-const.h"
#include "print-tree.h"
#include "omp-general.h"

/* Context information for walk_omp_for_loops. */
struct walk_ctx
{
  /* The most recently visited gomp_for that has been transformed and
     for which gimple_omp_for_set_combined_into_p returned true. */
  gomp_for *inner_combined_loop;

  /* The innermost bind enclosing the currently visited node. */
  gbind *bind;
};

static unsigned int walk_omp_for_loops (gimple_seq *, walk_ctx *);
static enum tree_code omp_adjust_neq_condition (tree v, tree step);

static bool
non_rectangular_p (const gomp_for *omp_for)
{
  size_t collapse = gimple_omp_for_collapse (omp_for);
  for (size_t i = 0; i < collapse; i++)
    {
      if (TREE_CODE (gimple_omp_for_final (omp_for, i)) == TREE_VEC
	  || TREE_CODE (gimple_omp_for_initial (omp_for, i)) == TREE_VEC)
	return true;
    }

  return false;
}

/* Callback for subst_var. */

static tree
subst_var_in_op (tree *t, int *subtrees ATTRIBUTE_UNUSED, void *data)
{

  auto *wi = (struct walk_stmt_info *)data;
  auto from_to = (std::pair<tree, tree> *)wi->info;

  if (*t == from_to->first)
    {
      *t = from_to->second;
      wi->changed = true;
    }

  return NULL_TREE;
}

/* Substitute all occurrences of FROM in the operands of the GIMPLE statements
   in SEQ by TO. */

static void
subst_var (gimple_seq *seq, tree from, tree to)
{
  gcc_assert (VAR_P (from));
  gcc_assert (VAR_P (to));

  std::pair<tree, tree> from_to (from, to);
  struct walk_stmt_info wi;
  memset (&wi, 0, sizeof (wi));
  wi.info = (void *)&from_to;

  walk_gimple_seq_mod (seq, NULL, subst_var_in_op, &wi);
}

/* Return the type that should be used for computing the iteration count of a
   loop with the given index VAR and upper/lower bound FINAL according to
   OpenMP 5.1. */

tree
gomp_for_iter_count_type (tree var, tree final)
{
  tree var_type = TREE_TYPE (var);

  if (POINTER_TYPE_P (var_type))
    return ptrdiff_type_node;

  tree operand_type = TREE_TYPE (final);
  if (TYPE_UNSIGNED (var_type) && !TYPE_UNSIGNED (operand_type))
    return signed_type_for (operand_type);

  return var_type;
}

extern tree
gimple_assign_rhs_to_tree (gimple *stmt);

/* Substitute all definitions from SEQ bottom-up into EXPR. This is used to
   reconstruct a tree for a gimplified expression for determinig whether or not
   the number of iterations of a loop is constant. */

tree
subst_defs (tree expr, gimple_seq seq)
{
  gimple_seq_node last = gimple_seq_last (seq);
  gimple_seq_node first = gimple_seq_first (seq);
  for (auto n = last; n != NULL; n = n != first ? n->prev : NULL)
    {
      if (!is_gimple_assign (n))
	continue;

      tree lhs = gimple_assign_lhs (n);
      tree rhs = gimple_assign_rhs_to_tree (n);
      std::pair<tree, tree> from_to (lhs, rhs);
      struct walk_stmt_info wi;
      memset (&wi, 0, sizeof (wi));
      wi.info = (void *)&from_to;
      walk_tree (&expr, subst_var_in_op, &wi, NULL);
      expr = fold (expr);
    }

  return expr;
}

/* Return an expression for the number of iterations of the outermost loop of
   OMP_FOR. */

tree
gomp_for_number_of_iterations (const gomp_for *omp_for, size_t level)
{
  gcc_assert (!non_rectangular_p (omp_for));

  tree init = gimple_omp_for_initial (omp_for, level);
  tree final = gimple_omp_for_final (omp_for, level);
  tree_code cond = gimple_omp_for_cond (omp_for, level);
  tree index = gimple_omp_for_index (omp_for, level);
  tree type = gomp_for_iter_count_type (index, final);
  tree step = TREE_OPERAND (gimple_omp_for_incr (omp_for, level), 1);

  init = subst_defs (init, gimple_omp_for_pre_body (omp_for));
  init = fold (init);
  final = subst_defs (final, gimple_omp_for_pre_body (omp_for));
  final = fold (final);

  tree_code minus_code = MINUS_EXPR;
  tree diff_type = type;
  if (POINTER_TYPE_P (TREE_TYPE (final)))
    {
      minus_code = POINTER_DIFF_EXPR;
      diff_type = ptrdiff_type_node;
    }

  tree diff;
  if (cond == GT_EXPR)
    diff = fold_build2 (minus_code, diff_type, init, final);
  else if (cond == LT_EXPR)
    diff = fold_build2 (minus_code, diff_type, final, init);
  else
    gcc_unreachable ();

  diff = fold_build2 (CEIL_DIV_EXPR, type, diff, step);
  diff = fold_build1 (ABS_EXPR, type, diff);

  return diff;
}

/* Return true if the expression representing the number of iterations for
   OMP_FOR is a constant expression, false otherwise. */

bool
gomp_for_constant_iterations_p (gomp_for *omp_for,
				unsigned HOST_WIDE_INT *iterations)
{
  tree t = gomp_for_number_of_iterations (omp_for, 0);
  if (!TREE_CONSTANT (t)
      || !tree_fits_uhwi_p (t))
    return false;

  *iterations = tree_to_uhwi (t);
  return true;
}

/* Split a gomp_for that represents a collapsed loop-nest into single
   loops. The result is a gomp_for of the same kind which is not collapsed
   (i.e. gimple_omp_for_collapse (OMP_FOR) == 1) and which contains nested,
   non-collapsed gomp_for loops whose kind is GF_OMP_FOR_KIND_TRANSFORM_LOOP
   (i.e. they will be lowered into plain, non-omp loops by this pass) for each
   of the loops of OMP_FOR.  All loops whose depth is strictly less than
   FROM_DEPTH are left collapsed. */

static gomp_for*
gomp_for_uncollapse (gomp_for *omp_for, int from_depth = 0)
{
  int collapse = gimple_omp_for_collapse (omp_for);
  gcc_assert (from_depth < collapse);

  if (collapse <= 1)
    return omp_for;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE | MSG_PRIORITY_INTERNALS, omp_for,
		     "Uncollapsing loop:\n %G\n",
		     static_cast <gimple *> (omp_for));

  gimple_seq body = gimple_omp_body (omp_for);
  gomp_for *level_omp_for = omp_for;
  for (int level = collapse - 1; level >= from_depth; level--)
    {
      level_omp_for = gimple_build_omp_for (body,
					    GF_OMP_FOR_KIND_TRANSFORM_LOOP,
					    NULL, 1, NULL);
      gimple_omp_for_set_cond (level_omp_for, 0,
			       gimple_omp_for_cond (omp_for, level));
      gimple_omp_for_set_initial (level_omp_for, 0,
				  gimple_omp_for_initial (omp_for, level));
      gimple_omp_for_set_final (level_omp_for, 0,
				gimple_omp_for_final (omp_for, level));
      gimple_omp_for_set_incr (level_omp_for, 0,
			       gimple_omp_for_incr (omp_for, level));
      gimple_omp_for_set_index (level_omp_for, 0,
				gimple_omp_for_index (omp_for, level));

      body = level_omp_for;
    }

  omp_for->collapse = from_depth;

  if (from_depth > 0)
    {
      gimple_omp_set_body (omp_for, body);
      return omp_for;
    }

  gimple_omp_for_set_clauses (level_omp_for, gimple_omp_for_clauses (omp_for));
  gimple_omp_for_set_pre_body (level_omp_for, gimple_omp_for_pre_body (omp_for));
  gimple_omp_for_set_combined_into_p (level_omp_for,
				      gimple_omp_for_combined_into_p (omp_for));
  gimple_omp_for_set_combined_p (level_omp_for,
				 gimple_omp_for_combined_p (omp_for));

  return level_omp_for;
}

static tree
build_loop_exit_cond (tree index, tree_code cond, tree final, gimple_seq *seq)
{
  tree exit_cond
      = fold_build1 (TRUTH_NOT_EXPR, boolean_type_node,
		     fold_build2 (cond, boolean_type_node, index, final));
  tree res = create_tmp_var (boolean_type_node);
  gimplify_assign (res, exit_cond, seq);

  return res;
}

/* Returns a register that contains the final value of a loop as described by
   FINAL. This is necessary for non-rectangular loops. */

static tree
build_loop_final (tree final, gimple_seq *seq)
{
  if (TREE_CODE (final) != TREE_VEC) /* rectangular loop-nest */
    return final;

  tree coeff = TREE_VEC_ELT (final, 0);
  tree outer_var = TREE_VEC_ELT (final, 1);
  tree constt = TREE_VEC_ELT (final, 2);

  tree type = TREE_TYPE (outer_var);
  tree val = fold_build2 (MULT_EXPR, type, coeff, outer_var);
  val = fold_build2 (PLUS_EXPR, type, val, constt);

  tree res = create_tmp_var (type);
  gimplify_assign (res, val, seq);

  return res;
}

/* Unroll the loop BODY UNROLL_FACTOR times, replacing the INDEX
   variable by a local copy in each copy of the body that will be
   incremented as specified by INCR.  If BUILD_EXIT_CONDS is true,
   insert a test of the loop exit condition given COND and FINAL
   before each copy of the body that will exit the loop if the value
   of the local index variable satisfies the loop exit condition.

   For example, the unrolling with BUILD_EXIT_CONDS == true turns

    for (i = 0; i < 3; i = i + 1)
    {
	BODY
    }

    into

    for (i = 0; i < n; i = i + 1)
    {
       i.0 = i
       if (!(i_0 < n))
	  goto exit
       BODY_COPY_1[i/i.0]		i.e. index var i replaced by i.0
       if (!(i_1 < n))
	  goto exit
       i.1 = i.0 + 1
       BODY_COPY_2[i/i.1]
       if (!(i_3 < n))
	  goto exit
       i.2 = i.2 + 1
       BODY_COPY_3[i/i.2]
       exit:
    }
 */
static gimple_seq
build_unroll_body (gimple_seq body, tree unroll_factor, tree index, tree incr,
		   bool build_exit_conds = false, tree final = NULL_TREE,
		   tree_code *cond = NULL)
{
  gcc_assert ((!build_exit_conds && !final && !cond)
	      || (build_exit_conds && final && cond));

  gimple_seq new_body = NULL;

  push_gimplify_context ();

  if (build_exit_conds)
    final = build_loop_final (final, &new_body);

  tree local_index = create_tmp_var (TREE_TYPE (index));
  subst_var (&body, index, local_index);
  tree local_incr = unshare_expr (incr);
  TREE_OPERAND (local_incr, 0) = local_index;

  tree exit_label = create_artificial_label (gimple_location (body));

  unsigned HOST_WIDE_INT n = tree_to_uhwi (unroll_factor);
  for (unsigned HOST_WIDE_INT i = 0; i < n; i++)
    {
      if (i == 0)
	gimplify_assign (local_index, index, &new_body);
      else
	gimplify_assign (local_index, local_incr, &new_body);

      tree body_copy_label = create_artificial_label (gimple_location (body));

      if (build_exit_conds)
	{
	  tree exit_cond
	      = build_loop_exit_cond (local_index, *cond, final, &new_body);
	  gimple_seq_add_stmt (
	      &new_body,
	      gimple_build_cond (EQ_EXPR, exit_cond, boolean_true_node,
				 exit_label, body_copy_label));
	}

      gimple_seq body_copy = copy_gimple_seq_and_replace_locals (body);
      gimple_seq_add_stmt (&new_body, gimple_build_label (body_copy_label));
      gimple_seq_add_seq (&new_body, body_copy);
    }


  gbind *bind = gimple_build_bind (NULL, new_body, NULL);
  pop_gimplify_context (bind);

  gimple_seq result = NULL;
  gimple_seq_add_stmt (&result, bind);
  gimple_seq_add_stmt (&result, gimple_build_label (exit_label));
  return result;
}

static gimple_seq transform_gomp_for (gomp_for *, tree, walk_ctx *ctx);

/* Execute the partial unrolling transformation for OMP_FOR with the given
   UNROLL_FACTOR and return the resulting gimple bind. LOC is the location for
   diagnostic messages.

   Example
   --------
   --------

    Original loop
    -------------

    #pragma omp for unroll_partial(3)
    for (i = 0; i < 100; i = i + 1)
    {
	BODY
    }

    gets, roughly, translated to

    {
    #pragma omp for
    for (i = 0; i < 100; i = i + 3)
    {
	i.0 = i
	if i.0 > 100:
	    goto exit_label
	BODY_COPY_1[i/i.0]		i.e. index var replaced
	i.1 = i + 1
	if i.1 > 100:
	    goto exit_label
	BODY_COPY_2[i/1.1]
	i.2 = i + 2
	if i.2 > 100:
	    goto exit_label
	BODY_COPY_3[i/i.2]

	exit_label:
    }
*/

static gimple_seq
partial_unroll (gomp_for *omp_for, tree unroll_factor,
		location_t loc, tree transformation_clauses, walk_ctx *ctx)
{
  gcc_assert (unroll_factor);
  gcc_assert (
      OMP_CLAUSE_CODE (transformation_clauses) == OMP_CLAUSE_UNROLL_PARTIAL
      || OMP_CLAUSE_CODE (transformation_clauses) == OMP_CLAUSE_UNROLL_NONE);

  /* Partial unrolling reduces the loop nest depth of a canonical loop nest to 1
     hence outer directives cannot require a greater collapse. */
  gcc_assert (gimple_omp_for_collapse (omp_for) <= 1);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE | MSG_PRIORITY_INTERNALS,
		     dump_user_location_t::from_location_t (loc),
		     "Partially unrolling loop:\n %G\n",
		     static_cast<gimple *> (omp_for));

  gomp_for *unrolled_for = as_a<gomp_for *> (copy_gimple_seq_and_replace_locals (omp_for));

  tree final = gimple_omp_for_final (unrolled_for, 0);
  tree incr = gimple_omp_for_incr (unrolled_for, 0);
  tree index = gimple_omp_for_index (unrolled_for, 0);
  gimple_seq body = gimple_omp_body (unrolled_for);

  tree_code cond = gimple_omp_for_cond (unrolled_for, 0);
  tree step = TREE_OPERAND (incr, 1);
  gimple_omp_set_body (unrolled_for,
		       build_unroll_body (body, unroll_factor, index, incr,
					  true, final, &cond));

  gbind *result_bind = gimple_build_bind (NULL, NULL, NULL);

  push_gimplify_context ();

  tree scaled_step
      = fold_build2 (MULT_EXPR, TREE_TYPE (step),
		     fold_convert (TREE_TYPE (step), unroll_factor), step);

  /* For combined constructs, step will be gimplified on the outer
     gomp_for. */
  if (!gimple_omp_for_combined_into_p (omp_for)
      && !TREE_CONSTANT (scaled_step))
    {
      tree var = create_tmp_var (TREE_TYPE (step), ".omp_unroll_step");
      gimplify_assign (var, scaled_step,
		       gimple_omp_for_pre_body_ptr (unrolled_for));
      scaled_step = var;
    }
  TREE_OPERAND (incr, 1) = scaled_step;
  gimple_omp_for_set_incr (unrolled_for, 0, incr);

  pop_gimplify_context (result_bind);

  if (gimple_omp_for_combined_into_p (omp_for))
    ctx->inner_combined_loop = unrolled_for;

  tree remaining_clauses = OMP_CLAUSE_CHAIN (transformation_clauses);
  gimple_seq_add_stmt (
      gimple_bind_body_ptr (result_bind),
      transform_gomp_for (unrolled_for, remaining_clauses, ctx));

  return result_bind;
}

static gimple_seq
full_unroll (gomp_for *omp_for, location_t loc, walk_ctx *ctx ATTRIBUTE_UNUSED)
{
  tree init = gimple_omp_for_initial (omp_for, 0);
  unsigned HOST_WIDE_INT niter = 0;
  if (!gomp_for_constant_iterations_p (omp_for, &niter))
    {
      error_at (loc, "Cannot apply full unrolling to loop with "
		     "non-constant number of iterations");
      return omp_for;
    }

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE | MSG_PRIORITY_INTERNALS,
		     dump_user_location_t::from_location_t (loc),
		     "Fully unrolling loop with "
		     HOST_WIDE_INT_PRINT_UNSIGNED
		     " iterations :\n %G\n", niter,
		     static_cast <gimple *>(omp_for));

  tree incr = gimple_omp_for_incr (omp_for, 0);
  tree index = gimple_omp_for_index (omp_for, 0);
  gimple_seq body = gimple_omp_body (omp_for);

  tree unroll_factor = build_int_cst (TREE_TYPE (init), niter);

  gimple_seq unrolled = NULL;
  gimple_seq_add_seq (&unrolled, gimple_omp_for_pre_body (omp_for));
  push_gimplify_context ();
  gimple_seq_add_seq (&unrolled,
		      build_unroll_body (body, unroll_factor, index, incr));

  gbind *result_bind = gimple_build_bind (NULL, unrolled, NULL);
  pop_gimplify_context (result_bind);
  return result_bind;
}

/* Decides if the OMP_FOR for which the user did not specify the type of
   unrolling to apply in the 'unroll' directive represented by the TRANSFORM
   clause should be fully unrolled. */

static bool
assign_unroll_full_clause_p (gomp_for *omp_for, tree transform)
{
  gcc_assert (OMP_CLAUSE_CODE (transform) == OMP_CLAUSE_UNROLL_NONE);
  gcc_assert (OMP_CLAUSE_CHAIN (transform) == NULL);

  /* Full unrolling turns the loop into a non-loop and hence
     the following transformations would fail. */
  if (TREE_CHAIN (transform) != NULL_TREE)
    return false;

  unsigned HOST_WIDE_INT num_iters;
  if (!gomp_for_constant_iterations_p (omp_for, &num_iters)
      || num_iters
	     > (unsigned HOST_WIDE_INT)param_omp_unroll_full_max_iterations)
    return false;

  if (dump_enabled_p ())
    {
      auto loc = dump_user_location_t::from_location_t (
	  OMP_CLAUSE_LOCATION (transform));
      dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, loc,
		       "assigned %<full%> clause to %<omp unroll%> with small "
		       "constant number of iterations\n");
    }

  return true;
}

/* If the OMP_FOR for which the user did not specify the type of unrolling in
   the 'unroll' directive in the TRANSFORM clause should be partially unrolled,
   return the unroll factor, otherwise return null. */

static tree
assign_unroll_partial_clause_p (gomp_for *omp_for ATTRIBUTE_UNUSED,
				tree transform)
{
  gcc_assert (OMP_CLAUSE_CODE (transform) == OMP_CLAUSE_UNROLL_NONE);

  if (param_omp_unroll_default_factor == 0)
    return NULL;

  tree unroll_factor
      = build_int_cst (integer_type_node, param_omp_unroll_default_factor);

  if (dump_enabled_p ())
    {
      auto loc = dump_user_location_t::from_location_t (
	  OMP_CLAUSE_LOCATION (transform));
      dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, loc,
	  "added %<partial(%u)%> clause to %<omp unroll%> directive\n",
	  param_omp_unroll_default_factor);
    }

  return unroll_factor;
}

/* Generate the code for an OMP_FOR that represents the result of a
   loop transformation which is not associated with any directive and
   which will hence not be lowered in the omp-expansion. */

static gimple_seq
expand_transformed_loop (gomp_for *omp_for)
{
  gcc_assert (gimple_omp_for_kind (omp_for)
		       == GF_OMP_FOR_KIND_TRANSFORM_LOOP);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE | MSG_PRIORITY_INTERNALS, omp_for,
		     "Expanding loop:\n %G\n",
		     static_cast <gimple *> (omp_for));

  push_gimplify_context ();

  omp_for = gomp_for_uncollapse (omp_for);

  tree incr = gimple_omp_for_incr (omp_for, 0);
  tree index = gimple_omp_for_index (omp_for, 0);
  tree init = gimple_omp_for_initial (omp_for, 0);
  tree final = gimple_omp_for_final (omp_for, 0);
  tree_code cond = gimple_omp_for_cond (omp_for, 0);
  gimple_seq body = gimple_omp_body (omp_for);
  gimple_seq pre_body = gimple_omp_for_pre_body (omp_for);

  gimple_seq loop = NULL;

  tree exit_label = create_artificial_label (UNKNOWN_LOCATION);
  tree cycle_label = create_artificial_label (UNKNOWN_LOCATION);
  tree body_label = create_artificial_label (UNKNOWN_LOCATION);

  gimple_seq_add_seq (&loop, pre_body);
  gimplify_assign (index, init, &loop);
  tree final_var = final;
  if (TREE_CODE (final) != VAR_DECL)
    {
      final_var = create_tmp_var (TREE_TYPE (final));
      gimplify_assign (final_var, final, &loop);
    }

  gimple_seq_add_stmt (&loop, gimple_build_label (cycle_label));
  gimple_seq_add_stmt (&loop, gimple_build_cond (cond, index, final_var,
						 body_label, exit_label));
  gimple_seq_add_stmt (&loop, gimple_build_label (body_label));
  gimple_seq_add_seq (&loop, body);
  gimplify_assign (index, incr, &loop);
  gimple_seq_add_stmt (&loop, gimple_build_goto (cycle_label));
  gimple_seq_add_stmt (&loop, gimple_build_label (exit_label));

  gbind *bind = gimple_build_bind (NULL, loop, NULL);
  pop_gimplify_context (bind);

  return bind;
}

static enum tree_code
omp_adjust_neq_condition (tree v, tree step)
{
  gcc_assert (TREE_CODE (step) == INTEGER_CST);
  if (TREE_CODE (TREE_TYPE (v)) == INTEGER_TYPE)
    {
      if (integer_onep (step))
	return LT_EXPR;
      else
	{
	  gcc_assert (integer_minus_onep (step));
	  return GT_EXPR;
	}
    }
  else
    {
      tree unit = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (v)));
      gcc_assert (TREE_CODE (unit) == INTEGER_CST);
      if (tree_int_cst_equal (unit, step))
	return LT_EXPR;
      else
	{
	  gcc_assert (wi::neg (wi::to_widest (unit))
		      == wi::to_widest (step));
	  return GT_EXPR;
	}
    }
}

/* Adjust *COND_CODE and *N2 so that the former is either LT_EXPR or GT_EXPR,
   given that V is the loop index variable and STEP is loop step.

   This function has been derived from omp_adjust_for_condition.
   In contrast to the original function it does not add 1 or
   -1 to the the final value when converting <=,>= to <,>
   for a pointer-type index variable. Instead, this function
   adds or subtracts the type size in bytes. This is necessary
   to determine the number of iterations correctly. */

void
omp_adjust_for_condition2 (location_t loc, enum tree_code *cond_code, tree *n2,
			  tree v, tree step)
{
  switch (*cond_code)
    {
    case LT_EXPR:
    case GT_EXPR:
      break;

    case NE_EXPR:
      *cond_code = omp_adjust_neq_condition (v, step);
      break;

    case LE_EXPR:
      if (POINTER_TYPE_P (TREE_TYPE (*n2)))
	{
	  tree unit = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (v)));
	  HOST_WIDE_INT type_unit = tree_to_shwi (unit);

	  *n2 = fold_build_pointer_plus_hwi_loc (loc, *n2, type_unit);
	}
      else
	*n2 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (*n2), *n2,
			       build_int_cst (TREE_TYPE (*n2), 1));
      *cond_code = LT_EXPR;
      break;
    case GE_EXPR:
      if (POINTER_TYPE_P (TREE_TYPE (*n2)))
	{
	  tree unit = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (v)));
	  HOST_WIDE_INT type_unit = tree_to_shwi (unit);
	  *n2 = fold_build_pointer_plus_hwi_loc (loc, *n2, -1 * type_unit);
	}
      else
	*n2 = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (*n2), *n2,
			       build_int_cst (TREE_TYPE (*n2), 1));
      *cond_code = GT_EXPR;
      break;
    default:
      gcc_unreachable ();
    }
}

/* Transform the condition of OMP_FOR to either LT_EXPR or GT_EXPR and adjust
   the final value as necessary. */

static bool
canonicalize_conditions (gomp_for *omp_for)
{
  size_t collapse = gimple_omp_for_collapse (omp_for);
  location_t loc = gimple_location (omp_for);
  bool new_decls = false;

  gimple_seq *pre_body = gimple_omp_for_pre_body_ptr (omp_for);
  for (size_t l = 0; l < collapse; l++)
    {
      enum tree_code cond = gimple_omp_for_cond (omp_for, l);

      if (cond == LT_EXPR || cond == GT_EXPR)
	continue;

      tree incr = gimple_omp_for_incr (omp_for, l);
      tree step = omp_get_for_step_from_incr (loc, incr);
      tree index = gimple_omp_for_index (omp_for, l);
      tree final = gimple_omp_for_final (omp_for, l);
      tree orig_final = final;
      /* If final refers to the index variable of an outer level, i.e.
	 the loop nest is non-rectangular, only convert NE_EXPR. This
	 is necessary for unrolling.  Unrolling needs to multiply the
	 step by the unrolling factor, but non-constant step values
	 are impossible with NE_EXPR. */
      if (TREE_CODE (final) == TREE_VEC)
	{
	  cond = omp_adjust_neq_condition (TREE_VEC_ELT (final, 1),
					   TREE_OPERAND (incr, 1));
	  gimple_omp_for_set_cond (omp_for, l, cond);
	  continue;
	}

      omp_adjust_for_condition2 (loc, &cond, &final, index, step);

      gimple_omp_for_set_cond (omp_for, l, cond);
      if (final == orig_final)
	continue;

      /* If this is a combined construct, gimplify the final on the
	 outer construct. */
      if (TREE_CODE (final) != INTEGER_CST
	  && !gimple_omp_for_combined_into_p (omp_for))
	{
	  tree new_final = create_tmp_var (TREE_TYPE (final));
	  gimplify_assign (new_final, final, pre_body);
	  final = new_final;
	  new_decls = true;
	}

      gimple_omp_for_set_final (omp_for, l, final);
    }

  return new_decls;
}

/* Combined distribute or taskloop constructs are represented by two
   or more nested gomp_for constructs which are created during
   gimplification. Loop transformations on the combined construct are
   executed on the innermost gomp_for. This function adjusts the loop
   header of an outer OMP_FOR loop to the changes made by the
   transformations on the inner loop which is provided by the CTX. */
static gimple_seq
adjust_combined_loop (gomp_for *omp_for, walk_ctx *ctx)
{
  gcc_assert (gimple_omp_for_combined_p (omp_for));
  gcc_assert (ctx->inner_combined_loop);

  gomp_for *inner_omp_for = ctx->inner_combined_loop;
  size_t collapse = gimple_omp_for_collapse (inner_omp_for);

  int kind = gimple_omp_for_kind (omp_for);
  if (kind == GF_OMP_FOR_KIND_DISTRIBUTE || kind == GF_OMP_FOR_KIND_TASKLOOP)
    {
      for (size_t level = 0; level < collapse; ++level)
	{
	  tree outer_incr = gimple_omp_for_incr (omp_for, level);
	  tree inner_incr = gimple_omp_for_incr (inner_omp_for, level);
	  gcc_assert (TREE_TYPE (inner_incr) == TREE_TYPE (outer_incr));

	  tree inner_final = gimple_omp_for_final (inner_omp_for, level);
	  enum tree_code inner_cond
	      = gimple_omp_for_cond (inner_omp_for, level);
	  gimple_omp_for_set_cond (omp_for, level, inner_cond);

	  tree inner_step = TREE_OPERAND (inner_incr, 1);
	  /* If this omp_for is the outermost loop belonging to a
	     combined construct, gimplify the step into its
	     prebody. Otherwise, just gimplify the step on the inner
	     gomp_for and move the ungimplified step expression
	     here. */
	  if (!gimple_omp_for_combined_into_p (omp_for)
	      && !TREE_CONSTANT (inner_step))
	    {
	      push_gimplify_context ();
	      tree step = create_tmp_var (TREE_TYPE (inner_incr),
					  ".omp_combined_step");
	      gimplify_assign (step, inner_step,
			       gimple_omp_for_pre_body_ptr (omp_for));
	      pop_gimplify_context (ctx->bind);
	      TREE_OPERAND (outer_incr, 1) = step;
	    }
	  else
	    TREE_OPERAND (outer_incr, 1) = inner_step;

	  if (!gimple_omp_for_combined_into_p (omp_for)
	      && !TREE_CONSTANT (inner_final))
	    {
	      push_gimplify_context ();
	      tree final = create_tmp_var (TREE_TYPE (inner_final),
					   ".omp_combined_final");
	      gimplify_assign (final, inner_final,
			       gimple_omp_for_pre_body_ptr (omp_for));
	      pop_gimplify_context (ctx->bind);
	      gimple_omp_for_set_final (omp_for, level, final);
	    }
	  else
	    gimple_omp_for_set_final (omp_for, level, inner_final);

	  /* Gimplify the step on the inner loop of the combined construct. */
	  if (!TREE_CONSTANT (inner_step))
	    {
	      push_gimplify_context ();
	      tree step = create_tmp_var (TREE_TYPE (inner_incr),
					  ".omp_combined_step");
	      gimplify_assign (step, inner_step,
			       gimple_omp_for_pre_body_ptr (inner_omp_for));
	      TREE_OPERAND (inner_incr, 1) = step;
	      pop_gimplify_context (ctx->bind);

	      tree private_clause = build_omp_clause (
		  gimple_location (omp_for), OMP_CLAUSE_PRIVATE);
	      OMP_CLAUSE_DECL (private_clause) = step;
	      tree *clauses = gimple_omp_for_clauses_ptr (inner_omp_for);
	      *clauses = chainon (*clauses, private_clause);
	    }

	  /* Gimplify the final on the inner loop of the combined construct. */
	  if (!TREE_CONSTANT (inner_final))
	    {
	      push_gimplify_context ();
	      tree final = create_tmp_var (TREE_TYPE (inner_incr),
					   ".omp_combined_final");
	      gimplify_assign (final, inner_final,
			       gimple_omp_for_pre_body_ptr (inner_omp_for));
	      gimple_omp_for_set_final (inner_omp_for, level, final);
	      pop_gimplify_context (ctx->bind);

	      tree private_clause = build_omp_clause (
		  gimple_location (omp_for), OMP_CLAUSE_PRIVATE);
	      OMP_CLAUSE_DECL (private_clause) = final;
	      tree *clauses = gimple_omp_for_clauses_ptr (inner_omp_for);
	      *clauses = chainon (*clauses, private_clause);
	    }
	}
    }

  if (gimple_omp_for_combined_into_p (omp_for))
    ctx->inner_combined_loop = omp_for;
  else
    ctx->inner_combined_loop = NULL;

  return omp_for;
}

/* Transform OMP_FOR recursively according to the clause chain
   TRANSFORMATION. Return the resulting sequence of gimple statements.

   This function dispatches OMP_FOR to the handler function for the
   TRANSFORMATION clause. The handler function is responsible for invoking this
   function recursively for executing the remaining transformations. */

static gimple_seq
transform_gomp_for (gomp_for *omp_for, tree transformation, walk_ctx *ctx)
{
  if (!transformation)
    {
      if (gimple_omp_for_kind (omp_for) == GF_OMP_FOR_KIND_TRANSFORM_LOOP)
	return expand_transformed_loop (omp_for);

      return omp_for;
    }

  push_gimplify_context ();

  bool added_decls = canonicalize_conditions (omp_for);

  gimple_seq result = NULL;
  location_t loc = OMP_CLAUSE_LOCATION (transformation);
  auto dump_loc = dump_user_location_t::from_location_t (loc);
  switch (OMP_CLAUSE_CODE (transformation))
    {
    case OMP_CLAUSE_UNROLL_FULL:
      gcc_assert (TREE_CHAIN (transformation) == NULL);
      result = full_unroll (omp_for, loc, ctx);
      break;
    case OMP_CLAUSE_UNROLL_NONE:
      gcc_assert (TREE_CHAIN (transformation) == NULL);
      if (assign_unroll_full_clause_p (omp_for, transformation))
	{
	  result = full_unroll (omp_for, loc, ctx);
	}
      else if (tree unroll_factor
	       = assign_unroll_partial_clause_p (omp_for, transformation))
	{
	  result = partial_unroll (omp_for, unroll_factor, loc,
				   transformation, ctx);
	}
      else {
	  if (dump_enabled_p ())
	    {
	      /* TODO Try to inform the unrolling pass that the user
		 wants to unroll this loop. This could relax some
		 restrictions there, e.g. on the code size? */
	      dump_printf_loc (
		  MSG_MISSED_OPTIMIZATION, dump_loc,
		  "not unrolling loop with %<omp unroll%> directive. Add "
		  "clause to specify unrolling type or invoke the "
		  "compiler with --param=omp-unroll-default-factor=n for some"
		  "constant integer n");
	    }
	  result = transform_gomp_for (omp_for, NULL, ctx);
      }

      break;
    case OMP_CLAUSE_UNROLL_PARTIAL:
      {
	tree unroll_factor = OMP_CLAUSE_UNROLL_PARTIAL_EXPR (transformation);
	if (!unroll_factor)
	  {
	    // TODO Use target architecture dependent constants?
	    unsigned factor = param_omp_unroll_default_factor > 0
				  ? param_omp_unroll_default_factor
				  : 5;
	    unroll_factor = build_int_cst (integer_type_node, factor);

	    if (dump_enabled_p ())
	      dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, dump_loc,
			       "%<partial%> clause without unrolling "
			       "factor turned into %<partial(%u)%> clause\n",
			       factor);
	  }
	result = partial_unroll (omp_for, unroll_factor, loc, transformation,
				 ctx);
      }
      break;
    default:
      gcc_unreachable ();
    }

  if (added_decls && gimple_code (result) != GIMPLE_BIND)
    result = gimple_build_bind (NULL, result, NULL);
  pop_gimplify_context (added_decls ? result : NULL); /* for decls from canonicalize_loops */

  return result;
}

/* Remove all loop transformation clauses from the clauses of OMP_FOR and
   return a new tree chain containing just those clauses.

   The clauses correspond to transformation *directives* associated with the
   OMP_FOR's loop. The returned clauses are ordered from the innermost
   directive to the outermost, i.e. in the order in which the transformations
   should execute.

   Example:
   --------
   --------

   The loop

   #pragma omp for nowait
   #pragma omp unroll partial(5)
   #pragma omp tile sizes(2,2)
   LOOP

   is represented as

   #pragma omp for nowait unroll_partial(5) tile_sizes(2,2)
   LOOP

   Gimplification may add clauses after the transformation clauses added
   by the front ends. This function will leave only the "nowait" clause on
   OMP_FOR and return the clauses "tile_sizes(2,2) unroll_partial(5)". */

static tree
gomp_for_remove_transformation_clauses (gomp_for *omp_for)
{
  tree *clauses = gimple_omp_for_clauses_ptr (omp_for);
  tree trans_clauses = NULL;
  tree last_other_clause = NULL;

  for (tree c = gimple_omp_for_clauses (omp_for); c != NULL_TREE;)
    {
      tree chain_tail = OMP_CLAUSE_CHAIN (c);
      if (omp_loop_transform_clause_p (c))
	{
	  if (last_other_clause)
	    OMP_CLAUSE_CHAIN (last_other_clause) = chain_tail;
	  else
	    *clauses = OMP_CLAUSE_CHAIN (c);

	  OMP_CLAUSE_CHAIN (c) = NULL;
	  trans_clauses = chainon (trans_clauses, c);
	}
      else
	{
	  /* There should be no other clauses between loop transformations ... */
	  gcc_assert (!trans_clauses || !last_other_clause
		      || TREE_CHAIN (last_other_clause) == c);
	  /* ... and hence stop if transformations were found before the
	     non-transformation clause C. */
	  if (trans_clauses)
	    break;
	  last_other_clause = c;
	 }

      c = chain_tail;
    }

  return nreverse (trans_clauses);
}

static void
print_optimized_unroll_partial_msg (tree c)
{
  gcc_assert (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_UNROLL_PARTIAL);
  location_t loc = OMP_CLAUSE_LOCATION (c);
  dump_user_location_t dump_loc;
  dump_loc = dump_user_location_t::from_location_t (loc);

  tree unroll_factor = OMP_CLAUSE_UNROLL_PARTIAL_EXPR (c);
  dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, dump_loc,
		   "replaced consecutive %<omp unroll%> directives by "
		   "%<omp unroll auto(" HOST_WIDE_INT_PRINT_UNSIGNED
		   ")%>\n", tree_to_uhwi (unroll_factor));
}

/* Optimize CLAUSES by removing and merging redundant clauses.  Return the
   optimized clause chain. */

static tree
optimize_transformation_clauses (tree clauses)
{
  /* The last unroll_partial clause seen in clauses, if any,
     or the last merged unroll partial clause. */
  tree unroll_partial = NULL;
  /* The last clause was not a unroll_partial clause, if any.
     unroll_full and unroll_none are not relevant because
     they appear only at the end of a chain. */
  tree last_non_unroll = NULL;
  /* Indicates that at least two unroll_partial clauses have been merged
     since last_non_unroll was seen. */
  bool merged_unroll_partial = false;

  for (tree c = clauses; c != NULL_TREE; c = OMP_CLAUSE_CHAIN (c))
    {
      enum omp_clause_code code = OMP_CLAUSE_CODE (c);

      switch (code)
	{
	case OMP_CLAUSE_UNROLL_NONE:
	  /* 'unroll' without a clause cannot be followed by any
	     transformations because its result does not have canonical loop
	     nest form. */
	  gcc_assert (OMP_CLAUSE_CHAIN (c) == NULL);
	  unroll_partial = NULL;
	  merged_unroll_partial = false;
	  break;
	case OMP_CLAUSE_UNROLL_FULL:
	  /* 'unroll full' cannot be followed by any transformations because
	     its result does not have canonical loop nest form. */
	  gcc_assert (OMP_CLAUSE_CHAIN (c) == NULL);

	  /* Previous 'unroll partial' directives are useless. */
	  if (unroll_partial)
	    {
	      if (last_non_unroll)
		OMP_CLAUSE_CHAIN (last_non_unroll) = c;
	      else
		clauses = c;

	      if (dump_enabled_p ())
		{
		  location_t loc = OMP_CLAUSE_LOCATION (c);
		  dump_user_location_t dump_loc;
		  dump_loc = dump_user_location_t::from_location_t (loc);

		  dump_printf_loc (
		      MSG_OPTIMIZED_LOCATIONS, dump_loc,
		      "removed useless %<omp unroll auto%> directives "
		      "preceding 'omp unroll full'\n");
		}
	    }
	  unroll_partial = NULL;
	  merged_unroll_partial = false;
	  break;
	case OMP_CLAUSE_UNROLL_PARTIAL:
	  {
	    /* Merge a sequence of consecutive 'unroll partial' directives.
	       Note that it impossible for 'unroll full' or 'unroll' to
	       appear inbetween the 'unroll partial' clauses because they
	       remove the loop-nest. */
	    if (unroll_partial)
	      {
		tree factor = OMP_CLAUSE_UNROLL_PARTIAL_EXPR (unroll_partial);
		tree c_factor = OMP_CLAUSE_UNROLL_PARTIAL_EXPR (c);
		if (factor && c_factor)
		  factor = fold_build2 (MULT_EXPR, TREE_TYPE (factor), factor,
					c_factor);
		else if (!factor && c_factor)
		  factor = c_factor;

		gcc_assert (!factor || TREE_CODE (factor) == INTEGER_CST);

		OMP_CLAUSE_UNROLL_PARTIAL_EXPR (unroll_partial) = factor;
		OMP_CLAUSE_CHAIN (unroll_partial) = OMP_CLAUSE_CHAIN (c);
		OMP_CLAUSE_LOCATION (unroll_partial) = OMP_CLAUSE_LOCATION (c);
		merged_unroll_partial = true;
	      }
	    else
	      unroll_partial = c;
	  }
	  break;
	default:
	  gcc_unreachable ();
	}
    }

  if (merged_unroll_partial && dump_enabled_p ())
    print_optimized_unroll_partial_msg (unroll_partial);

  return clauses;
}

/* Visit the current statement in GSI_P in the walk_omp_for_loops walk and
   execute all loop transformations found on it. */

void
process_omp_for (gomp_for *omp_for, gimple_seq *containing_seq, walk_ctx *ctx)
{
  auto gsi_p = gsi_for_stmt (omp_for, containing_seq);
  tree transform_clauses = gomp_for_remove_transformation_clauses (omp_for);

  /* Do not attempt to transform broken code which might violate the
     assumptions of the loop transformation implementations.

     Transformation clauses must be dropped first because following
     passes do not handle them. */
  if (seen_error ())
    return;

  transform_clauses = optimize_transformation_clauses (transform_clauses);

  gimple *transformed = omp_for;
  if (gimple_omp_for_combined_p (omp_for)
      && ctx->inner_combined_loop)
    transformed = adjust_combined_loop (omp_for, ctx);
  else
    transformed = transform_gomp_for (omp_for, transform_clauses, ctx);

  if (transformed == omp_for)
    return;

  gsi_replace_with_seq (&gsi_p, transformed, true);

  if (!dump_enabled_p () || !(dump_flags & TDF_DETAILS))
    return;

  dump_printf_loc (MSG_NOTE | MSG_PRIORITY_INTERNALS, transformed,
		   "Transformed loop: %G\n\n", transformed);
}

/* Traverse SEQ in depth-first order and apply the loop transformation
   found on gomp_for statements. */

static unsigned int
walk_omp_for_loops (gimple_seq *seq, walk_ctx *ctx)
{
  gimple_stmt_iterator gsi;
  for (gsi = gsi_start (*seq); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      switch (gimple_code (stmt))
	{
	case GIMPLE_OMP_CRITICAL:
	case GIMPLE_OMP_MASTER:
	case GIMPLE_OMP_MASKED:
	case GIMPLE_OMP_TASKGROUP:
	case GIMPLE_OMP_ORDERED:
	case GIMPLE_OMP_SCAN:
	case GIMPLE_OMP_SECTION:
	case GIMPLE_OMP_PARALLEL:
	case GIMPLE_OMP_TASK:
	case GIMPLE_OMP_SCOPE:
	case GIMPLE_OMP_SECTIONS:
	case GIMPLE_OMP_SINGLE:
	case GIMPLE_OMP_TARGET:
	case GIMPLE_OMP_TEAMS:
	  {
	    gbind *bind = ctx->bind;
	    walk_omp_for_loops (gimple_omp_body_ptr (stmt), ctx);
	    ctx->bind = bind;
	    break;
	  }
	case GIMPLE_OMP_FOR:
	  {
	    gbind *bind = ctx->bind;
	    walk_omp_for_loops (gimple_omp_for_pre_body_ptr (stmt), ctx);
	    walk_omp_for_loops (gimple_omp_body_ptr (stmt), ctx);
	    ctx->bind = bind;
	    process_omp_for (as_a<gomp_for *> (stmt), seq, ctx);
	    break;
	  }
	case GIMPLE_BIND:
	  {
	    gbind *bind = as_a<gbind *> (stmt);
	    ctx->bind = bind;
	    walk_omp_for_loops (gimple_bind_body_ptr (bind), ctx);
	    ctx->bind = bind;
	    break;
	  }
	case GIMPLE_TRY:
	  {
	    gbind *bind = ctx->bind;
	    walk_omp_for_loops (gimple_try_eval_ptr (as_a<gtry *> (stmt)),
				ctx);
	    walk_omp_for_loops (gimple_try_cleanup_ptr (as_a<gtry *> (stmt)),
				ctx);
	    ctx->bind = bind;
	    break;
	  }

	case GIMPLE_CATCH:
	  {
	    gbind *bind = ctx->bind;
	    walk_omp_for_loops (
		gimple_catch_handler_ptr (as_a<gcatch *> (stmt)), ctx);
	    ctx->bind = bind;
	    break;
	  }

	case GIMPLE_EH_FILTER:
	  {
	    gbind *bind = ctx->bind;
	    walk_omp_for_loops (gimple_eh_filter_failure_ptr (stmt), ctx);
	    ctx->bind = bind;
	    break;
	  }

	case GIMPLE_EH_ELSE:
	  {
	    gbind *bind = ctx->bind;
	    geh_else *eh_else_stmt = as_a<geh_else *> (stmt);
	    walk_omp_for_loops (gimple_eh_else_n_body_ptr (eh_else_stmt), ctx);
	    walk_omp_for_loops (gimple_eh_else_e_body_ptr (eh_else_stmt), ctx);
	    ctx->bind = bind;
	    break;
	  }
	  break;

	case GIMPLE_WITH_CLEANUP_EXPR:
	  {
	    gbind *bind = ctx->bind;
	    walk_omp_for_loops (gimple_wce_cleanup_ptr (stmt), ctx);
	    ctx->bind = bind;
	    break;
	  }

	case GIMPLE_TRANSACTION:
	  {
	    gbind *bind = ctx->bind;
	    auto trans = as_a<gtransaction *> (stmt);
	    walk_omp_for_loops (gimple_transaction_body_ptr (trans), ctx);
	    ctx->bind = bind;
	    break;
	  }

	case GIMPLE_ASSUME:
	  break;

	default:
	  gcc_assert (!gimple_has_substatements (stmt));
	  continue;
	}
    }

  return true;
}

static unsigned int
execute_omp_transform_loops ()
{
  gimple_seq body = gimple_body (current_function_decl);
  walk_ctx ctx;
  ctx.inner_combined_loop = NULL;
  ctx.bind = NULL;
  walk_omp_for_loops (&body, &ctx);

  return 0;
}

namespace
{

const pass_data pass_data_omp_transform_loops = {
  GIMPLE_PASS,           /* type */
  "omp_transform_loops", /* name */
  OPTGROUP_OMP,          /* optinfo_flags */
  TV_NONE,               /* tv_id */
  PROP_gimple_any,       /* properties_required */
  0,                     /* properties_provided */
  0,                     /* properties_destroyed */
  0,                     /* todo_flags_start */
  0,                     /* todo_flags_finish */
};

class pass_omp_transform_loops : public gimple_opt_pass
{
public:
  pass_omp_transform_loops (gcc::context *ctxt)
      : gimple_opt_pass (pass_data_omp_transform_loops, ctxt)
  {
  }

  /* opt_pass methods: */
  virtual unsigned int
  execute (function *)
  {
    return execute_omp_transform_loops ();
  }
  virtual bool
  gate (function *)
  {
    return flag_openmp || flag_openmp_simd;
  }

}; // class pass_omp_transform_loops

} // anon namespace

gimple_opt_pass *
make_pass_omp_transform_loops (gcc::context *ctxt)
{
  return new pass_omp_transform_loops (ctxt);
}
