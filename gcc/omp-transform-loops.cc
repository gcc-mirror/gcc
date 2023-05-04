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
   reconstruct a tree from a gimplified expression for determinig whether or not
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

static gimple_seq
expand_transformed_loop (gomp_for *omp_for);

/* Split a gomp_for that represents a collapsed loop-nest into single
   loops. The result is a gomp_for of the same kind which is not collapsed
   (i.e. gimple_omp_for_collapse (OMP_FOR) == 1) and which contains nested,
   non-collapsed gomp_for loops whose kind is GF_OMP_FOR_KIND_TRANSFORM_LOOP
   (i.e. they will be lowered into plain, non-omp loops by this pass) for each
   of the loops of OMP_FOR.  All loops whose depth is strictly less than
   FROM_DEPTH are left collapsed. */

static gomp_for*
gomp_for_uncollapse (gomp_for *omp_for, int from_depth = 0, bool expand = false)
{
  int collapse = gimple_omp_for_collapse (omp_for);
  gcc_assert (from_depth < collapse);
  gcc_assert (from_depth >= 0);

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


      if (expand)
	body = expand_transformed_loop (level_omp_for);
      else
	body = level_omp_for;
    }

  omp_for->collapse = from_depth;

  if (from_depth > 0)
    {
      gimple_omp_set_body (omp_for, body);
      omp_for->collapse = from_depth;
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
partial_unroll (gomp_for *omp_for, size_t level, tree unroll_factor,
		location_t loc, tree transformation_clauses, walk_ctx *ctx)
{
  gcc_assert (unroll_factor);
  gcc_assert (
      OMP_CLAUSE_CODE (transformation_clauses) == OMP_CLAUSE_UNROLL_PARTIAL
      || OMP_CLAUSE_CODE (transformation_clauses) == OMP_CLAUSE_UNROLL_NONE);

  /* Partial unrolling reduces the loop nest depth of a canonical loop nest to 1
     hence outer directives cannot require a greater collapse. */
  gcc_assert (gimple_omp_for_collapse (omp_for) <= level + 1);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE | MSG_PRIORITY_INTERNALS,
		     dump_user_location_t::from_location_t (loc),
		     "Partially unrolling loop:\n %G\n",
		     static_cast<gimple *> (omp_for));

  gomp_for *unrolled_for = as_a<gomp_for *> (copy_gimple_seq_and_replace_locals (omp_for));

  tree final = gimple_omp_for_final (unrolled_for, level);
  tree incr = gimple_omp_for_incr (unrolled_for, level);
  tree index = gimple_omp_for_index (unrolled_for, level);
  gimple_seq body = gimple_omp_body (unrolled_for);

  tree_code cond = gimple_omp_for_cond (unrolled_for, level);
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
  gimple_omp_for_set_incr (unrolled_for, level, incr);

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
  gimplify_assign (index, init, &unrolled);
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

/* Execute the tiling transformation for OMP_FOR with the given TILE_SIZES and
   return the resulting gimple bind. TILE_SIZES must be a non-empty tree chain
   of integer constants and the collapse of OMP_FOR must be at least the length
   of TILE_SIZES. TRANSFORMATION_CLAUSES are the loop transformations that
   must be applied to OMP_FOR. Those are applied on the result of the tiling
   transformation. LOC is the location for diagnostic messages.

   Example 1
   ---------
   ---------

   Original loop
   -------------

   #pragma omp for
   #pragma omp tile sizes(3)
   for (i = 1; i <= n; i = i + 1)
   {
       body;
   }

   Internally, the tile directive is represented as a clause on the
   omp for, i.e. as #pragma omp for tile_sizes(3).

   Transformed loop
   ----------------

   #pragma omp for
   for (.omp_tile_index = 1; .omp_tile_index < ceil(n/3); .omp_tile_index = .omp_tile_index + 3)
   {
	D.4287 = .omp_tile_index + 3 + 1
	#pragma omp loop_transform
	for (i = .omp_tile_index; i < D.4287; i = i + 1)
	{
	    if (i.0 > n)
		goto L.0
	    body;
	}
	L_0:
    }

   The outer loop is the "floor loop" and the inner loop is the "tile
   loop".  The tile loop is never in canonical loop nest form and
   hence it cannot be associated with any loop construct. The
   GCC-internal "omp loop transform" construct will be lowered after
   the tiling transformation.
 */

static gimple_seq
tile (gomp_for *omp_for, location_t loc, size_t start_level, tree tile_sizes,
      tree transformation_clauses, walk_ctx *ctx)
{
  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE | MSG_PRIORITY_INTERNALS,
		     dump_user_location_t::from_location_t (loc),
		     "Executing tile transformation %T:\n %G\n",
		     transformation_clauses, static_cast<gimple *> (omp_for));

  gimple_seq tile_loops = copy_gimple_seq_and_replace_locals (omp_for);
  gimple_seq floor_loops = copy_gimple_seq_and_replace_locals (omp_for);

  size_t collapse = gimple_omp_for_collapse (omp_for);
  size_t tiling_depth = list_length (tile_sizes);
  tree clauses = gimple_omp_for_clauses (omp_for);
  size_t clause_collapse = 1;
  tree collapse_clause = NULL;

  if (tree c = omp_find_clause (clauses, OMP_CLAUSE_ORDERED))
    {
      error_at (OMP_CLAUSE_LOCATION (c),
		"%<ordered%> invalid in conjunction with %<omp tile%>");
      return omp_for;
    }

  if (tree c = omp_find_clause (clauses, OMP_CLAUSE_COLLAPSE))
    {
      tree expr = OMP_CLAUSE_COLLAPSE_EXPR (c);
      clause_collapse = tree_to_uhwi (expr);
      collapse_clause = c;
    }

  /* The tiled loop nest is a canonical loop nest with nesting depth
     tiling_depth. The tile loops below that level are not in
     canonical loop nest form and hence cannot be associated with a
     loop construct. */
  if (clause_collapse > tiling_depth + start_level)
    {
      error_at (OMP_CLAUSE_LOCATION (collapse_clause),
		"collapse cannot extend below the floor loops "
		"generated by the %<omp tile%> construct");
      OMP_CLAUSE_COLLAPSE_EXPR (collapse_clause)
	  = build_int_cst (unsigned_type_node, start_level + tiling_depth);
      return transform_gomp_for (omp_for, NULL, ctx);
    }

  if (start_level + tiling_depth > collapse)
    return transform_gomp_for (omp_for, NULL, ctx);

  gcc_assert (collapse >= clause_collapse);

  push_gimplify_context ();

  /* Create the index variables for iterating the tiles in the floor
     loops which will be the loops at levels start_level
     ... start_level + tiling_depth of the transformed loop nest. The
     loops at level 0 ... start_level - 1 are left unchanged. */
  gimple_seq floor_loops_pre_body = NULL;
  size_t tile_level = 0;
  auto_vec<tree> sizes_vec;
  for (tree el = tile_sizes; el; el = TREE_CHAIN (el), tile_level++)
    {
      size_t nest_level = start_level + tile_level;
      tree index = gimple_omp_for_index (omp_for, nest_level);
      tree init = gimple_omp_for_initial (omp_for, nest_level);
      tree incr = gimple_omp_for_incr (omp_for, nest_level);
      tree step = TREE_OPERAND (incr, 1);

      /* Initialize original index variables in the pre-body.  The
	 loop lowering will not initialize them because of the changed
	 index variables. */
      gimplify_assign (index, init, &floor_loops_pre_body);

      tree tile_size = fold_convert (TREE_TYPE (step), TREE_VALUE (el));
      sizes_vec.safe_push (tile_size);
      tree tile_index = create_tmp_var (TREE_TYPE (index), ".omp_tile_index");
      gimplify_assign (tile_index, init, &floor_loops_pre_body);

      /* Floor loops */
      step = fold_build2 (MULT_EXPR, TREE_TYPE (step), step, tile_size);
      tree tile_step = step;
      /* For combined constructs, step will be gimplified on the outer
	 gomp_for. */
      if (!gimple_omp_for_combined_into_p (omp_for) && !TREE_CONSTANT (step))
	{
	  tile_step = create_tmp_var (TREE_TYPE (step), ".omp_tile_step");
	  gimplify_assign (tile_step, step, &floor_loops_pre_body);
	}
      incr = fold_build2 (TREE_CODE (incr), TREE_TYPE (incr), tile_index,
			  tile_step);
      gimple_omp_for_set_incr (floor_loops, nest_level, incr);
      gimple_omp_for_set_index (floor_loops, nest_level, tile_index);
    }

  gbind *result_bind = gimple_build_bind (NULL, NULL, NULL);
  pop_gimplify_context (result_bind);
  gimple_seq_add_seq (gimple_omp_for_pre_body_ptr (floor_loops),
		      floor_loops_pre_body);

  /* The tiling loops will not form a perfect loop nest because the
     loop for each tiling dimension needs to check if the current tile
     is incomplete and this check is intervening code. Since OpenMP
     5.1 does not allow the collapse of the loop-nest to extend beyond
     the floor loops, this is not a problem.

     "Uncollapse" the tiling loop nest, i.e. split the loop nest into
     nested separate gomp_for structures for each level. This allows
     to add the incomplete tile checks to each level loop. */

  tile_loops = gomp_for_uncollapse (as_a <gomp_for *> (tile_loops));
  for (size_t i = 0; i < start_level; i++)
    tile_loops = gimple_omp_body (tile_loops);

  gimple_omp_for_set_kind (as_a<gomp_for *> (tile_loops),
			   GF_OMP_FOR_KIND_TRANSFORM_LOOP);
  gimple_omp_for_set_clauses (tile_loops, NULL_TREE);
  gimple_omp_for_set_pre_body (tile_loops, NULL);

  /* Transform the loop bodies of the "uncollapsed" tiling loops and
     add them to the body of the floor loops.  At this point, the
     loop nest consists of perfectly nested gimple_omp_for constructs,
     each representing a single loop. */
  gimple_seq floor_loops_body = NULL;
  gimple *level_loop = tile_loops;
  gimple_seq_add_stmt (&floor_loops_body, tile_loops);
  gimple_seq *surrounding_seq = &floor_loops_body;

  push_gimplify_context ();

  tree break_label = create_artificial_label (UNKNOWN_LOCATION);
  gimple_seq_add_stmt (surrounding_seq, gimple_build_label (break_label));
  for (size_t tile_level = 0; tile_level < tiling_depth; tile_level++)
    {
      gimple_seq level_preamble = NULL;
      gimple_seq level_body = gimple_omp_body (level_loop);
      auto gsi = gsi_start (level_body);

      int nest_level = start_level + tile_level;
      tree original_index = gimple_omp_for_index (omp_for, nest_level);
      tree original_final = gimple_omp_for_final (omp_for, nest_level);

      tree tile_index
	  = gimple_omp_for_index (floor_loops, nest_level);
      tree tile_size = sizes_vec[tile_level];
      tree type = TREE_TYPE (tile_index);
      tree plus_type = type;

      tree incr = gimple_omp_for_incr (omp_for, nest_level);
      tree step = omp_get_for_step_from_incr (gimple_location (omp_for), incr);

      gimple_seq *pre_body = gimple_omp_for_pre_body_ptr (level_loop);
      gcc_assert (gimple_omp_for_collapse (level_loop) == 1);
      tree_code original_cond = gimple_omp_for_cond (omp_for, nest_level);

      gimple_omp_for_set_initial (level_loop, 0, tile_index);

      tree tile_final = create_tmp_var (type);
      tree scaled_tile_size
	  = fold_build2 (MULT_EXPR, TREE_TYPE (tile_size), tile_size, step);

      tree_code plus_code = PLUS_EXPR;
      if (POINTER_TYPE_P (TREE_TYPE (tile_index)))
	{
	  plus_code = POINTER_PLUS_EXPR;
	  int unsignedp = TYPE_UNSIGNED (TREE_TYPE (scaled_tile_size));
	  plus_type
	      = signed_or_unsigned_type_for (unsignedp, ptrdiff_type_node);
	}

      scaled_tile_size = fold_convert (plus_type, scaled_tile_size);
      gimplify_assign (
	  tile_final,
	  fold_build2 (plus_code, type, tile_index, scaled_tile_size),
	  pre_body);
      gimple_omp_for_set_final (level_loop, 0, tile_final);

      push_gimplify_context ();

      tree body_label = create_artificial_label (UNKNOWN_LOCATION);

      /* Handle partial tiles, i.e. add a check that breaks from the tile loop
	 if the new index value does not belong to the iteration space of the
	 original loop. */
      gimple_seq_add_stmt (&level_preamble,
			   gimple_build_cond (original_cond, original_index,
					      original_final, body_label,
					      break_label));
      gimple_seq_add_stmt (&level_preamble, gimple_build_label (body_label));

      gsi_insert_seq_before (&gsi, level_preamble, GSI_SAME_STMT);
      gbind *level_bind = gimple_build_bind (NULL, NULL, NULL);
      pop_gimplify_context (level_bind);
      gimple_bind_set_body (level_bind, level_body);
      gimple_omp_set_body (level_loop, level_bind);

      surrounding_seq = &level_body;
      level_loop = gsi_stmt (gsi);

      /* The label for jumping out of the loop at the next
	 nesting level. For the outermost level, the label is put
	 after the loop-nest, for the last one it is not necessary. */
      if (tile_level != tiling_depth - 1)
	{
	  break_label = create_artificial_label (UNKNOWN_LOCATION);
	  gsi_insert_after (&gsi, gimple_build_label (break_label),
			    GSI_NEW_STMT);
	}
    }

  gbind *tile_loops_bind;
  tile_loops_bind = gimple_build_bind (NULL, tile_loops, NULL);
  pop_gimplify_context (tile_loops_bind);

  gimple_omp_set_body (floor_loops, tile_loops_bind);

  tree remaining_clauses = OMP_CLAUSE_CHAIN (transformation_clauses);

  /* Collapsing of the OMP_FOR is used both for the "omp tile"
     implementation and for the actual "collapse" clause. If the
     tiling depth was greater than the collapse depth required by the
     clauses on OMP_FOR, the collapse of OMP_FOR must be adjusted to
     the latter value and all loops below the new collapse depth must
     be transformed to GF_OMP_FOR_KIND_TRANSFORM_LOOP to ensure their
     lowering in this pass. */
  size_t new_collapse = clause_collapse;

  /* Keep the omp_for collapsed if there are further transformations */
  if (remaining_clauses)
    {
      size_t next_transform_depth = 1;
      if (OMP_CLAUSE_CODE (remaining_clauses) == OMP_CLAUSE_TILE)
	next_transform_depth
	    = list_length (OMP_CLAUSE_TILE_SIZES (remaining_clauses));

      size_t next_level
	  = tree_to_uhwi (OMP_CLAUSE_TRANSFORM_LEVEL (remaining_clauses));
      /* The current "omp tile" transformation reduces the nesting depth
	 of the canonical loop-nest to TILING_DEPTH.
	 Hence the following "omp tile" transformation is invalid if
	 it requires a greater nesting depth. */
      gcc_assert (next_level + next_transform_depth <=
		  start_level + tiling_depth);
      if (next_level + next_transform_depth > new_collapse)
	new_collapse = next_level + next_transform_depth;
    }

  if (collapse > new_collapse)
    floor_loops = gomp_for_uncollapse (as_a<gomp_for *> (floor_loops),
				       new_collapse, true);

  /* Lower the uncollapsed tile loops. */
  walk_omp_for_loops (gimple_bind_body_ptr (tile_loops_bind), ctx);

  gcc_assert (remaining_clauses || !collapse_clause
	      || gimple_omp_for_collapse (floor_loops)
	      == (size_t)clause_collapse);

  if (gimple_omp_for_combined_into_p (omp_for))
    ctx->inner_combined_loop = as_a<gomp_for *> (floor_loops);

  /* Apply remaining transformation clauses and assemble the transformation
     result. */
  gimple_bind_set_body (result_bind,
			transform_gomp_for (as_a<gomp_for *> (floor_loops),
					    remaining_clauses, ctx));

  return result_bind;
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
  size_t level = tree_to_uhwi (OMP_CLAUSE_TRANSFORM_LEVEL (transformation));
  switch (OMP_CLAUSE_CODE (transformation))
    {
    case OMP_CLAUSE_UNROLL_FULL:
      gcc_assert (TREE_CHAIN (transformation) == NULL);
      gcc_assert (level == 0);
      result = full_unroll (omp_for, loc, ctx);
      break;
    case OMP_CLAUSE_UNROLL_NONE:
      gcc_assert (TREE_CHAIN (transformation) == NULL);
      gcc_assert (level == 0);
      if (assign_unroll_full_clause_p (omp_for, transformation))
	{
	  result = full_unroll (omp_for, loc, ctx);
	}
      else if (tree unroll_factor
	       = assign_unroll_partial_clause_p (omp_for, transformation))
	{
	  result = partial_unroll (omp_for, level, unroll_factor, loc,
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

	result = partial_unroll (omp_for, level,
				 unroll_factor, loc, transformation, ctx);
      }
      break;
    case OMP_CLAUSE_TILE:
      result = tile (omp_for, loc, level,
		     OMP_CLAUSE_TILE_SIZES (transformation),
		     transformation, ctx);
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
  if (!clauses)
    return NULL_TREE;

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

  size_t level = tree_to_uhwi (OMP_CLAUSE_TRANSFORM_LEVEL (clauses));
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
	case OMP_CLAUSE_TILE:
	  {
	    /* No optimization for those clauses yet, but they end any chain of
	       "unroll partial" clauses. */
	    if (merged_unroll_partial && dump_enabled_p ())
	      print_optimized_unroll_partial_msg (unroll_partial);

	    if (unroll_partial)
	      OMP_CLAUSE_CHAIN (unroll_partial) = c;

	    unroll_partial = NULL;
	    merged_unroll_partial = false;
	    last_non_unroll = c;
	  }
	  break;
	default:
	  gcc_unreachable ();
	}

      /* The transformations are ordered by the level of the loop-nest to which
	 they apply in decreasing order. Handle the different levels separately
	 as long as we do not implement optimizations across the levels. */
      tree next_c = OMP_CLAUSE_CHAIN (c);
      if (!next_c)
	break;

      size_t next_level = tree_to_uhwi (OMP_CLAUSE_TRANSFORM_LEVEL (next_c));
      if (next_level != level)
	{
	  gcc_assert (next_level < level);
	  tree tail = optimize_transformation_clauses (next_c);
	  OMP_CLAUSE_CHAIN (c) = tail;
	  break;
	}
      else level = next_level;

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

	case GIMPLE_OMP_METADIRECTIVE:
	  {
	    gimple *variant = gimple_omp_metadirective_variants (stmt);

	    while (variant)
	      {
		gbind *bind = ctx->bind;
		walk_omp_for_loops (gimple_omp_body_ptr (variant), ctx);
		ctx->bind = bind;

		variant = variant->next;
	      }
	  }
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
