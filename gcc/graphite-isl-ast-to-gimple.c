/* Translation of ISL AST to Gimple.
   Copyright (C) 2014-2015 Free Software Foundation, Inc.
   Contributed by Roman Gareev <gareevroman@gmail.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"

#ifdef HAVE_isl
#include <isl/constraint.h>
#include <isl/set.h>
#include <isl/union_set.h>
#include <isl/map.h>
#include <isl/union_map.h>
#include <isl/ast_build.h>

/* Since ISL-0.13, the extern is in val_gmp.h.  */
#if !defined(HAVE_ISL_SCHED_CONSTRAINTS_COMPUTE_SCHEDULE) && defined(__cplusplus)
extern "C" {
#endif
#include <isl/val_gmp.h>
#if !defined(HAVE_ISL_SCHED_CONSTRAINTS_COMPUTE_SCHEDULE) && defined(__cplusplus)
}
#endif
#endif

#define INCLUDE_MAP
#include "system.h"
#include "coretypes.h"
#include "hash-set.h"
#include "machmode.h"
#include "vec.h"
#include "double-int.h"
#include "input.h"
#include "alias.h"
#include "symtab.h"
#include "options.h"
#include "wide-int.h"
#include "inchash.h"
#include "tree.h"
#include "fold-const.h"
#include "predict.h"
#include "tm.h"
#include "hard-reg-set.h"
#include "input.h"
#include "function.h"
#include "dominance.h"
#include "cfg.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "tree-ssa-loop.h"
#include "tree-pass.h"
#include "cfgloop.h"
#include "tree-data-ref.h"
#include "sese.h"
#include "tree-ssa-loop-manip.h"
#include "tree-scalar-evolution.h"
#include "gimple-ssa.h"
#include "tree-into-ssa.h"

#ifdef HAVE_isl
#include "graphite-poly.h"
#include "graphite-isl-ast-to-gimple.h"

/* This flag is set when an error occurred during the translation of
   ISL AST to Gimple.  */

static bool graphite_regenerate_error;

/* We always try to use signed 128 bit types, but fall back to smaller types
   in case a platform does not provide types of these sizes. In the future we
   should use isl to derive the optimal type for each subexpression.  */

static int max_mode_int_precision =
  GET_MODE_PRECISION (mode_for_size (MAX_FIXED_MODE_SIZE, MODE_INT, 0));
static int graphite_expression_type_precision = 128 <= max_mode_int_precision ?
						128 : max_mode_int_precision;

struct ast_build_info
{
  ast_build_info()
    : is_parallelizable(false)
  { };
  bool is_parallelizable;
};

/* Converts a GMP constant VAL to a tree and returns it.  */

static tree
gmp_cst_to_tree (tree type, mpz_t val)
{
  tree t = type ? type : integer_type_node;
  mpz_t tmp;

  mpz_init (tmp);
  mpz_set (tmp, val);
  wide_int wi = wi::from_mpz (t, tmp, true);
  mpz_clear (tmp);

  return wide_int_to_tree (t, wi);
}

/* Verifies properties that GRAPHITE should maintain during translation.  */

static inline void
graphite_verify (void)
{
#ifdef ENABLE_CHECKING
  verify_loop_structure ();
  verify_loop_closed_ssa (true);
#endif
}

/* IVS_PARAMS maps ISL's scattering and parameter identifiers
   to corresponding trees.  */

typedef std::map<isl_id *, tree> ivs_params;

/* Free all memory allocated for ISL's identifiers.  */

void ivs_params_clear (ivs_params &ip)
{
  std::map<isl_id *, tree>::iterator it;
  for (it = ip.begin ();
       it != ip.end (); it++)
    {
      isl_id_free (it->first);
    }
}

static tree
gcc_expression_from_isl_expression (tree type, __isl_take isl_ast_expr *,
				    ivs_params &ip);

/* Return the tree variable that corresponds to the given isl ast identifier
   expression (an isl_ast_expr of type isl_ast_expr_id).

   FIXME: We should replace blind conversation of id's type with derivation
   of the optimal type when we get the corresponding isl support. Blindly
   converting type sizes may be problematic when we switch to smaller
   types.  */

static tree
gcc_expression_from_isl_ast_expr_id (tree type,
				     __isl_keep isl_ast_expr *expr_id,
				     ivs_params &ip)
{
  gcc_assert (isl_ast_expr_get_type (expr_id) == isl_ast_expr_id);
  isl_id *tmp_isl_id = isl_ast_expr_get_id (expr_id);
  std::map<isl_id *, tree>::iterator res;
  res = ip.find (tmp_isl_id);
  isl_id_free (tmp_isl_id);
  gcc_assert (res != ip.end () &&
              "Could not map isl_id to tree expression");
  isl_ast_expr_free (expr_id);
  return fold_convert (type, res->second);
}

/* Converts an isl_ast_expr_int expression E to a GCC expression tree of
   type TYPE.  */

static tree
gcc_expression_from_isl_expr_int (tree type, __isl_take isl_ast_expr *expr)
{
  gcc_assert (isl_ast_expr_get_type (expr) == isl_ast_expr_int);
  isl_val *val = isl_ast_expr_get_val (expr);
  mpz_t val_mpz_t;
  mpz_init (val_mpz_t);
  tree res;
  if (isl_val_get_num_gmp (val, val_mpz_t) == -1)
    res = NULL_TREE;
  else
    res = gmp_cst_to_tree (type, val_mpz_t);
  isl_val_free (val);
  isl_ast_expr_free (expr);
  mpz_clear (val_mpz_t);
  return res;
}

/* Converts a binary isl_ast_expr_op expression E to a GCC expression tree of
   type TYPE.  */

static tree
binary_op_to_tree (tree type, __isl_take isl_ast_expr *expr, ivs_params &ip)
{
  isl_ast_expr *arg_expr = isl_ast_expr_get_op_arg (expr, 0);
  tree tree_lhs_expr = gcc_expression_from_isl_expression (type, arg_expr, ip);
  arg_expr = isl_ast_expr_get_op_arg (expr, 1);
  tree tree_rhs_expr = gcc_expression_from_isl_expression (type, arg_expr, ip);
  enum isl_ast_op_type expr_type = isl_ast_expr_get_op_type (expr);
  isl_ast_expr_free (expr);
  switch (expr_type)
    {
    case isl_ast_op_add:
      return fold_build2 (PLUS_EXPR, type, tree_lhs_expr, tree_rhs_expr);

    case isl_ast_op_sub:
      return fold_build2 (MINUS_EXPR, type, tree_lhs_expr, tree_rhs_expr);

    case isl_ast_op_mul:
      return fold_build2 (MULT_EXPR, type, tree_lhs_expr, tree_rhs_expr);

    case isl_ast_op_div:
      return fold_build2 (EXACT_DIV_EXPR, type, tree_lhs_expr, tree_rhs_expr);

    case isl_ast_op_pdiv_q:
      return fold_build2 (TRUNC_DIV_EXPR, type, tree_lhs_expr, tree_rhs_expr);

    case isl_ast_op_pdiv_r:
      return fold_build2 (TRUNC_MOD_EXPR, type, tree_lhs_expr, tree_rhs_expr);

    case isl_ast_op_fdiv_q:
      return fold_build2 (FLOOR_DIV_EXPR, type, tree_lhs_expr, tree_rhs_expr);

    case isl_ast_op_and:
      return fold_build2 (TRUTH_ANDIF_EXPR, type,
			  tree_lhs_expr, tree_rhs_expr);

    case isl_ast_op_or:
      return fold_build2 (TRUTH_ORIF_EXPR, type, tree_lhs_expr, tree_rhs_expr);

    case isl_ast_op_eq:
      return fold_build2 (EQ_EXPR, type, tree_lhs_expr, tree_rhs_expr);

    case isl_ast_op_le:
      return fold_build2 (LE_EXPR, type, tree_lhs_expr, tree_rhs_expr);

    case isl_ast_op_lt:
      return fold_build2 (LT_EXPR, type, tree_lhs_expr, tree_rhs_expr);

    case isl_ast_op_ge:
      return fold_build2 (GE_EXPR, type, tree_lhs_expr, tree_rhs_expr);

    case isl_ast_op_gt:
      return fold_build2 (GT_EXPR, type, tree_lhs_expr, tree_rhs_expr);

    default:
      gcc_unreachable ();
    }
}

/* Converts a ternary isl_ast_expr_op expression E to a GCC expression tree of
   type TYPE.  */

static tree
ternary_op_to_tree (tree type, __isl_take isl_ast_expr *expr, ivs_params &ip)
{
  gcc_assert (isl_ast_expr_get_op_type (expr) == isl_ast_op_minus);
  isl_ast_expr *arg_expr = isl_ast_expr_get_op_arg (expr, 0);
  tree tree_first_expr
    = gcc_expression_from_isl_expression (type, arg_expr, ip);
  arg_expr = isl_ast_expr_get_op_arg (expr, 1);
  tree tree_second_expr
    = gcc_expression_from_isl_expression (type, arg_expr, ip);
  arg_expr = isl_ast_expr_get_op_arg (expr, 2);
  tree tree_third_expr
    = gcc_expression_from_isl_expression (type, arg_expr, ip);
  isl_ast_expr_free (expr);
  return fold_build3 (COND_EXPR, type, tree_first_expr,
		      tree_second_expr, tree_third_expr);
}

/* Converts a unary isl_ast_expr_op expression E to a GCC expression tree of
   type TYPE.  */

static tree
unary_op_to_tree (tree type, __isl_take isl_ast_expr *expr, ivs_params &ip)
{
  gcc_assert (isl_ast_expr_get_op_type (expr) == isl_ast_op_minus);
  isl_ast_expr *arg_expr = isl_ast_expr_get_op_arg (expr, 0);
  tree tree_expr = gcc_expression_from_isl_expression (type, arg_expr, ip);
  isl_ast_expr_free (expr);
  return fold_build1 (NEGATE_EXPR, type, tree_expr);
}

/* Converts an isl_ast_expr_op expression E with unknown number of arguments
   to a GCC expression tree of type TYPE.  */

static tree
nary_op_to_tree (tree type, __isl_take isl_ast_expr *expr, ivs_params &ip)
{
  enum tree_code op_code;
  switch (isl_ast_expr_get_op_type (expr))
    {
    case isl_ast_op_max:
      op_code = MAX_EXPR;
      break;

    case isl_ast_op_min:
      op_code = MIN_EXPR;
      break;

    default:
      gcc_unreachable ();    
    }
  isl_ast_expr *arg_expr = isl_ast_expr_get_op_arg (expr, 0);
  tree res = gcc_expression_from_isl_expression (type, arg_expr, ip);
  int i;
  for (i = 1; i < isl_ast_expr_get_op_n_arg (expr); i++)
    {
      arg_expr = isl_ast_expr_get_op_arg (expr, i);
      tree t = gcc_expression_from_isl_expression (type, arg_expr, ip);
      res = fold_build2 (op_code, type, res, t);
    }
  isl_ast_expr_free (expr);
  return res;
}


/* Converts an isl_ast_expr_op expression E to a GCC expression tree of
   type TYPE.  */

static tree
gcc_expression_from_isl_expr_op (tree type, __isl_take isl_ast_expr *expr,
				 ivs_params &ip)
{
  gcc_assert (isl_ast_expr_get_type (expr) == isl_ast_expr_op);
  switch (isl_ast_expr_get_op_type (expr))
    {
    /* These isl ast expressions are not supported yet.  */
    case isl_ast_op_error:
    case isl_ast_op_call:
    case isl_ast_op_and_then:
    case isl_ast_op_or_else:
    case isl_ast_op_select:
      gcc_unreachable ();

    case isl_ast_op_max:
    case isl_ast_op_min:
      return nary_op_to_tree (type, expr, ip);

    case isl_ast_op_add:
    case isl_ast_op_sub:
    case isl_ast_op_mul:
    case isl_ast_op_div:
    case isl_ast_op_pdiv_q:
    case isl_ast_op_pdiv_r:
    case isl_ast_op_fdiv_q:
    case isl_ast_op_and:
    case isl_ast_op_or:
    case isl_ast_op_eq:
    case isl_ast_op_le:
    case isl_ast_op_lt:
    case isl_ast_op_ge:
    case isl_ast_op_gt:
      return binary_op_to_tree (type, expr, ip);

    case isl_ast_op_minus:
      return unary_op_to_tree (type, expr, ip);

    case isl_ast_op_cond:
      return ternary_op_to_tree (type, expr, ip);

    default:
      gcc_unreachable ();
    }

  return NULL_TREE;
}

/* Converts an ISL AST expression E back to a GCC expression tree of
   type TYPE.  */

static tree
gcc_expression_from_isl_expression (tree type, __isl_take isl_ast_expr *expr,
				    ivs_params &ip)
{
  switch (isl_ast_expr_get_type (expr))
    {
    case isl_ast_expr_id:
      return gcc_expression_from_isl_ast_expr_id (type, expr, ip);

    case isl_ast_expr_int:
      return gcc_expression_from_isl_expr_int (type, expr);

    case isl_ast_expr_op:
      return gcc_expression_from_isl_expr_op (type, expr, ip);

    default:
      gcc_unreachable ();
    }

  return NULL_TREE;
}

/* Creates a new LOOP corresponding to isl_ast_node_for.  Inserts an
   induction variable for the new LOOP.  New LOOP is attached to CFG
   starting at ENTRY_EDGE.  LOOP is inserted into the loop tree and
   becomes the child loop of the OUTER_LOOP.  NEWIVS_INDEX binds
   ISL's scattering name to the induction variable created for the
   loop of STMT.  The new induction variable is inserted in the NEWIVS
   vector and is of type TYPE.  */

static struct loop *
graphite_create_new_loop (edge entry_edge, __isl_keep isl_ast_node *node_for,
			  loop_p outer, tree type, tree lb, tree ub,
			  ivs_params &ip)
{
  isl_ast_expr *for_inc = isl_ast_node_for_get_inc (node_for);
  tree stride = gcc_expression_from_isl_expression (type, for_inc, ip);
  tree ivvar = create_tmp_var (type, "graphite_IV");
  tree iv, iv_after_increment;
  loop_p loop = create_empty_loop_on_edge
    (entry_edge, lb, stride, ub, ivvar, &iv, &iv_after_increment,
     outer ? outer : entry_edge->src->loop_father);

  isl_ast_expr *for_iterator = isl_ast_node_for_get_iterator (node_for);
  isl_id *id = isl_ast_expr_get_id (for_iterator);
  std::map<isl_id *, tree>::iterator res;
  res = ip.find (id);
  if (ip.count (id))
    isl_id_free (res->first);
  ip[id] = iv;
  isl_ast_expr_free (for_iterator);
  return loop;
}

static edge
translate_isl_ast (loop_p context_loop, __isl_keep isl_ast_node *node,
		   edge next_e, ivs_params &ip);

/* Create the loop for a isl_ast_node_for.

   - NEXT_E is the edge where new generated code should be attached.  */

static edge
translate_isl_ast_for_loop (loop_p context_loop,
			    __isl_keep isl_ast_node *node_for, edge next_e,
			    tree type, tree lb, tree ub,
			    ivs_params &ip)
{
  gcc_assert (isl_ast_node_get_type (node_for) == isl_ast_node_for);
  struct loop *loop = graphite_create_new_loop (next_e, node_for, context_loop,
						type, lb, ub, ip);
  edge last_e = single_exit (loop);
  edge to_body = single_succ_edge (loop->header);
  basic_block after = to_body->dest;

  /* Create a basic block for loop close phi nodes.  */
  last_e = single_succ_edge (split_edge (last_e));

  /* Translate the body of the loop.  */
  isl_ast_node *for_body = isl_ast_node_for_get_body (node_for);
  next_e = translate_isl_ast (loop, for_body, to_body, ip);
  isl_ast_node_free (for_body);
  redirect_edge_succ_nodup (next_e, after);
  set_immediate_dominator (CDI_DOMINATORS, next_e->dest, next_e->src);

  if (flag_loop_parallelize_all)
  {
    isl_id *id = isl_ast_node_get_annotation (node_for);
    gcc_assert (id);
    ast_build_info *for_info = (ast_build_info *) isl_id_get_user (id);
    loop->can_be_parallel = for_info->is_parallelizable;
    free (for_info);
    isl_id_free (id);
  }

  return last_e;
}

/* We use this function to get the upper bound because of the form,
   which is used by isl to represent loops:

   for (iterator = init; cond; iterator += inc)

   {

     ...

   }

   The loop condition is an arbitrary expression, which contains the
   current loop iterator.

   (e.g. iterator + 3 < B && C > iterator + A)

   We have to know the upper bound of the iterator to generate a loop
   in Gimple form. It can be obtained from the special representation
   of the loop condition, which is generated by isl,
   if the ast_build_atomic_upper_bound option is set. In this case,
   isl generates a loop condition that consists of the current loop
   iterator, + an operator (< or <=) and an expression not involving
   the iterator, which is processed and returned by this function.

   (e.g iterator <= upper-bound-expression-without-iterator)  */

static __isl_give isl_ast_expr *
get_upper_bound (__isl_keep isl_ast_node *node_for)
{
  gcc_assert (isl_ast_node_get_type (node_for) == isl_ast_node_for);
  isl_ast_expr *for_cond = isl_ast_node_for_get_cond (node_for);
  gcc_assert (isl_ast_expr_get_type (for_cond) == isl_ast_expr_op);
  isl_ast_expr *res;
  switch (isl_ast_expr_get_op_type (for_cond))
    {
    case isl_ast_op_le:
      res = isl_ast_expr_get_op_arg (for_cond, 1);
      break;

    case isl_ast_op_lt:
      {
        // (iterator < ub) => (iterator <= ub - 1)
        isl_val *one =
          isl_val_int_from_si (isl_ast_expr_get_ctx (for_cond), 1);
        isl_ast_expr *ub = isl_ast_expr_get_op_arg (for_cond, 1);
        res = isl_ast_expr_sub (ub, isl_ast_expr_from_val (one));
        break;
      }

    default:
      gcc_unreachable ();
    }
  isl_ast_expr_free (for_cond);
  return res;
}

/* All loops generated by create_empty_loop_on_edge have the form of
   a post-test loop:

   do

   {
     body of the loop;
   } while (lower bound < upper bound);

   We create a new if region protecting the loop to be executed, if
   the execution count is zero (lower bound > upper bound).  */

static edge
graphite_create_new_loop_guard (edge entry_edge,
				__isl_keep isl_ast_node *node_for, tree *type,
				tree *lb, tree *ub, ivs_params &ip)
{
  gcc_assert (isl_ast_node_get_type (node_for) == isl_ast_node_for);
  tree cond_expr;
  edge exit_edge;

  *type =
    build_nonstandard_integer_type (graphite_expression_type_precision, 0);
  isl_ast_expr *for_init = isl_ast_node_for_get_init (node_for);
  *lb = gcc_expression_from_isl_expression (*type, for_init, ip);
  isl_ast_expr *upper_bound = get_upper_bound (node_for);
  *ub = gcc_expression_from_isl_expression (*type, upper_bound, ip);
  
  /* When ub is simply a constant or a parameter, use lb <= ub.  */
  if (TREE_CODE (*ub) == INTEGER_CST || TREE_CODE (*ub) == SSA_NAME)
    cond_expr = fold_build2 (LE_EXPR, boolean_type_node, *lb, *ub);
  else
    {
      tree one = (POINTER_TYPE_P (*type)
		  ? convert_to_ptrofftype (integer_one_node)
		  : fold_convert (*type, integer_one_node));
      /* Adding +1 and using LT_EXPR helps with loop latches that have a
	 loop iteration count of "PARAMETER - 1".  For PARAMETER == 0 this
	 becomes 2^k-1 due to integer overflow, and the condition lb <= ub
	 is true, even if we do not want this.  However lb < ub + 1 is false,
	 as expected.  */
      tree ub_one = fold_build2 (POINTER_TYPE_P (*type) ? POINTER_PLUS_EXPR
				 : PLUS_EXPR, *type, *ub, one);

      cond_expr = fold_build2 (LT_EXPR, boolean_type_node, *lb, ub_one);
    }

  exit_edge = create_empty_if_region_on_edge (entry_edge, cond_expr);

  return exit_edge;
}

/* Translates an isl_ast_node_for to Gimple. */

static edge
translate_isl_ast_node_for (loop_p context_loop, __isl_keep isl_ast_node *node,
			    edge next_e, ivs_params &ip)
{
  gcc_assert (isl_ast_node_get_type (node) == isl_ast_node_for);
  tree type, lb, ub;
  edge last_e = graphite_create_new_loop_guard (next_e, node, &type,
						&lb, &ub, ip);
  edge true_e = get_true_edge_from_guard_bb (next_e->dest);

  translate_isl_ast_for_loop (context_loop, node, true_e,
			      type, lb, ub, ip);
  return last_e;
}

/* Inserts in iv_map a tuple (OLD_LOOP->num, NEW_NAME) for the induction
   variables of the loops around GBB in SESE.
 
   FIXME: Instead of using a vec<tree> that maps each loop id to a possible
   chrec, we could consider using a map<int, tree> that maps loop ids to the
   corresponding tree expressions.  */

static void
build_iv_mapping (vec<tree> iv_map, gimple_bb_p gbb,
		  __isl_keep isl_ast_expr *user_expr, ivs_params &ip,
		  sese region)
{
  gcc_assert (isl_ast_expr_get_type (user_expr) == isl_ast_expr_op &&
              isl_ast_expr_get_op_type (user_expr) == isl_ast_op_call);
  int i;
  isl_ast_expr *arg_expr;
  for (i = 1; i < isl_ast_expr_get_op_n_arg (user_expr); i++)
    {
      arg_expr = isl_ast_expr_get_op_arg (user_expr, i);
      tree type =
        build_nonstandard_integer_type (graphite_expression_type_precision, 0);
      tree t = gcc_expression_from_isl_expression (type, arg_expr, ip);
      loop_p old_loop = gbb_loop_at_index (gbb, region, i - 1);
      iv_map[old_loop->num] = t;
    }

}

/* Translates an isl_ast_node_user to Gimple.

   FIXME: We should remove iv_map.create (loop->num + 1), if it is possible.  */

static edge
translate_isl_ast_node_user (__isl_keep isl_ast_node *node,
			     edge next_e, ivs_params &ip)
{
  gcc_assert (isl_ast_node_get_type (node) == isl_ast_node_user);
  isl_ast_expr *user_expr = isl_ast_node_user_get_expr (node);
  isl_ast_expr *name_expr = isl_ast_expr_get_op_arg (user_expr, 0);
  gcc_assert (isl_ast_expr_get_type (name_expr) == isl_ast_expr_id);
  isl_id *name_id = isl_ast_expr_get_id (name_expr);
  poly_bb_p pbb = (poly_bb_p) isl_id_get_user (name_id);
  gcc_assert (pbb);
  gimple_bb_p gbb = PBB_BLACK_BOX (pbb);
  vec<tree> iv_map;
  isl_ast_expr_free (name_expr);
  isl_id_free (name_id);

  gcc_assert (GBB_BB (gbb) != ENTRY_BLOCK_PTR_FOR_FN (cfun) &&
	      "The entry block should not even appear within a scop");

  int nb_loops = number_of_loops (cfun);
  iv_map.create (nb_loops);
  iv_map.safe_grow_cleared (nb_loops);

  build_iv_mapping (iv_map, gbb, user_expr, ip, SCOP_REGION (pbb->scop));
  isl_ast_expr_free (user_expr);
  next_e = copy_bb_and_scalar_dependences (GBB_BB (gbb),
					   SCOP_REGION (pbb->scop), next_e,
					   iv_map,
					   &graphite_regenerate_error);
  iv_map.release ();
  mark_virtual_operands_for_renaming (cfun);
  update_ssa (TODO_update_ssa);
  return next_e;
}

/* Translates an isl_ast_node_block to Gimple. */

static edge
translate_isl_ast_node_block (loop_p context_loop,
			      __isl_keep isl_ast_node *node,
			      edge next_e, ivs_params &ip)
{
  gcc_assert (isl_ast_node_get_type (node) == isl_ast_node_block);
  isl_ast_node_list *node_list = isl_ast_node_block_get_children (node);
  int i;
  for (i = 0; i < isl_ast_node_list_n_ast_node (node_list); i++)
    {
      isl_ast_node *tmp_node = isl_ast_node_list_get_ast_node (node_list, i);
      next_e = translate_isl_ast (context_loop, tmp_node, next_e, ip);
      isl_ast_node_free (tmp_node);
    }
  isl_ast_node_list_free (node_list);
  return next_e;
}
 
/* Creates a new if region corresponding to ISL's cond.  */

static edge
graphite_create_new_guard (edge entry_edge, __isl_take isl_ast_expr *if_cond,
			   ivs_params &ip)
{
  tree type =
    build_nonstandard_integer_type (graphite_expression_type_precision, 0);
  tree cond_expr = gcc_expression_from_isl_expression (type, if_cond, ip);
  edge exit_edge = create_empty_if_region_on_edge (entry_edge, cond_expr);
  return exit_edge;
}

/* Translates an isl_ast_node_if to Gimple.  */

static edge
translate_isl_ast_node_if (loop_p context_loop,
			   __isl_keep isl_ast_node *node,
			   edge next_e, ivs_params &ip)
{
  gcc_assert (isl_ast_node_get_type (node) == isl_ast_node_if);
  isl_ast_expr *if_cond = isl_ast_node_if_get_cond (node);
  edge last_e = graphite_create_new_guard (next_e, if_cond, ip);

  edge true_e = get_true_edge_from_guard_bb (next_e->dest);
  isl_ast_node *then_node = isl_ast_node_if_get_then (node);
  translate_isl_ast (context_loop, then_node, true_e, ip);
  isl_ast_node_free (then_node);

  edge false_e = get_false_edge_from_guard_bb (next_e->dest);
  isl_ast_node *else_node = isl_ast_node_if_get_else (node);
  if (isl_ast_node_get_type (else_node) != isl_ast_node_error)
    translate_isl_ast (context_loop, else_node, false_e, ip);
  isl_ast_node_free (else_node);
  return last_e;
}

/* Translates an ISL AST node NODE to GCC representation in the
   context of a SESE.  */

static edge
translate_isl_ast (loop_p context_loop, __isl_keep isl_ast_node *node,
		   edge next_e, ivs_params &ip)
{
  switch (isl_ast_node_get_type (node))
    {
    case isl_ast_node_error:
      gcc_unreachable ();

    case isl_ast_node_for:
      return translate_isl_ast_node_for (context_loop, node,
					 next_e, ip);

    case isl_ast_node_if:
      return translate_isl_ast_node_if (context_loop, node,
					next_e, ip);

    case isl_ast_node_user:
      return translate_isl_ast_node_user (node, next_e, ip);

    case isl_ast_node_block:
      return translate_isl_ast_node_block (context_loop, node,
					   next_e, ip);

    default:
      gcc_unreachable ();
    }
}

/* Prints NODE to FILE.  */

void
print_isl_ast_node (FILE *file, __isl_keep isl_ast_node *node, 
		    __isl_keep isl_ctx *ctx)
{
  isl_printer *prn = isl_printer_to_file (ctx, file);
  prn = isl_printer_set_output_format (prn, ISL_FORMAT_C);
  prn = isl_printer_print_ast_node (prn, node);
  prn = isl_printer_print_str (prn, "\n");
  isl_printer_free (prn);
}

/* Add ISL's parameter identifiers and corresponding.trees to ivs_params  */

static void
add_parameters_to_ivs_params (scop_p scop, ivs_params &ip)
{
  sese region = SCOP_REGION (scop);
  unsigned nb_parameters = isl_set_dim (scop->context, isl_dim_param);
  gcc_assert (nb_parameters == SESE_PARAMS (region).length ());
  unsigned i;
  for (i = 0; i < nb_parameters; i++)
    {
      isl_id *tmp_id = isl_set_get_dim_id (scop->context, isl_dim_param, i);
      ip[tmp_id] = SESE_PARAMS (region)[i];
    }
}


/* Generates a build, which specifies the constraints on the parameters.  */

static __isl_give isl_ast_build *
generate_isl_context (scop_p scop)
{
  isl_set *context_isl = isl_set_params (isl_set_copy (scop->context));
  return isl_ast_build_from_context (context_isl);
}

/* Get the maximal number of schedule dimensions in the scop SCOP.  */

static
int get_max_schedule_dimensions (scop_p scop)
{
  int i;
  poly_bb_p pbb;
  int schedule_dims = 0;

  FOR_EACH_VEC_ELT (SCOP_BBS (scop), i, pbb)
    {
      int pbb_schedule_dims = isl_map_dim (pbb->transformed, isl_dim_out);
      if (pbb_schedule_dims > schedule_dims)
	schedule_dims = pbb_schedule_dims;
    }

  return schedule_dims;
}

/* Extend the schedule to NB_SCHEDULE_DIMS schedule dimensions.

   For schedules with different dimensionality, the isl AST generator can not
   define an order and will just randomly choose an order. The solution to this
   problem is to extend all schedules to the maximal number of schedule
   dimensions (using '0's for the remaining values).  */

static __isl_give isl_map *
extend_schedule (__isl_take isl_map *schedule, int nb_schedule_dims)
{
  int tmp_dims = isl_map_dim (schedule, isl_dim_out);
  schedule =
    isl_map_add_dims (schedule, isl_dim_out, nb_schedule_dims - tmp_dims);
  isl_val *zero =
    isl_val_int_from_si (isl_map_get_ctx (schedule), 0);
  int i;
  for (i = tmp_dims; i < nb_schedule_dims; i++)
    {
      schedule =
        isl_map_fix_val (schedule, isl_dim_out, i, isl_val_copy (zero));
    }
  isl_val_free (zero);
  return schedule;
}

/* Set the separation_class option for unroll and jam. */

static __isl_give isl_union_map *
generate_luj_sepclass_opt (scop_p scop, __isl_take isl_union_set *domain, 
			int dim, int cl)
{
  isl_map  *map;
  isl_space *space, *space_sep;
  isl_ctx *ctx;
  isl_union_map *mapu;
  int nsched = get_max_schedule_dimensions (scop);
 
  ctx = scop->ctx;
  space_sep = isl_space_alloc (ctx, 0, 1, 1);
  space_sep = isl_space_wrap (space_sep);
  space_sep = isl_space_set_tuple_name (space_sep, isl_dim_set,
				        "separation_class");
  space = isl_set_get_space (scop->context);
  space_sep = isl_space_align_params (space_sep, isl_space_copy(space));
  space = isl_space_map_from_domain_and_range (space, space_sep);
  space = isl_space_add_dims (space,isl_dim_in, nsched);
  map = isl_map_universe (space);
  isl_map_fix_si (map,isl_dim_out,0,dim);
  isl_map_fix_si (map,isl_dim_out,1,cl);

  mapu = isl_union_map_intersect_domain (isl_union_map_from_map (map), 
					 domain);
  return (mapu);
}

/* Compute the separation class for loop unroll and jam.  */

static __isl_give isl_union_set *
generate_luj_sepclass (scop_p scop)
{
  int i;
  poly_bb_p pbb;
  isl_union_set *domain_isl;

  domain_isl = isl_union_set_empty (isl_set_get_space (scop->context));

  FOR_EACH_VEC_ELT (SCOP_BBS (scop), i, pbb)
    {
      isl_set *bb_domain;
      isl_set *bb_domain_s;

      if (pbb->map_sepclass == NULL)
	continue;

      if (isl_set_is_empty (pbb->domain))
	continue;

      bb_domain = isl_set_copy (pbb->domain);
      bb_domain_s = isl_set_apply (bb_domain, pbb->map_sepclass);
      pbb->map_sepclass = NULL;

      domain_isl =
	isl_union_set_union (domain_isl, isl_union_set_from_set (bb_domain_s));
    }

  return domain_isl;
}

/* Set the AST built options for loop unroll and jam. */
 
static __isl_give isl_union_map *
generate_luj_options (scop_p scop)
{
  isl_union_set *domain_isl;
  isl_union_map *options_isl_ss;
  isl_union_map *options_isl =
    isl_union_map_empty (isl_set_get_space (scop->context));
  int dim = get_max_schedule_dimensions (scop) - 1;
  int dim1 = dim - PARAM_VALUE (PARAM_LOOP_UNROLL_JAM_DEPTH);

  if (!flag_loop_unroll_jam)
    return options_isl;

  domain_isl = generate_luj_sepclass (scop);

  options_isl_ss = generate_luj_sepclass_opt (scop, domain_isl, dim1, 0);
  options_isl = isl_union_map_union (options_isl, options_isl_ss);

  return options_isl;
}

/* Generates a schedule, which specifies an order used to
   visit elements in a domain.  */

static __isl_give isl_union_map *
generate_isl_schedule (scop_p scop)
{
  int nb_schedule_dims = get_max_schedule_dimensions (scop);
  int i;
  poly_bb_p pbb;
  isl_union_map *schedule_isl =
    isl_union_map_empty (isl_set_get_space (scop->context));

  FOR_EACH_VEC_ELT (SCOP_BBS (scop), i, pbb)
    {
      /* Dead code elimination: when the domain of a PBB is empty,
	 don't generate code for the PBB.  */
      if (isl_set_is_empty (pbb->domain))
	continue;

      isl_map *bb_schedule = isl_map_copy (pbb->transformed);
      bb_schedule = isl_map_intersect_domain (bb_schedule,
					      isl_set_copy (pbb->domain));
      bb_schedule = extend_schedule (bb_schedule, nb_schedule_dims);
      schedule_isl =
        isl_union_map_union (schedule_isl,
			     isl_union_map_from_map (bb_schedule));
    }
  return schedule_isl;
}

/* This method is executed before the construction of a for node.  */
static __isl_give isl_id *
ast_build_before_for (__isl_keep isl_ast_build *build, void *user)
{
  isl_union_map *dependences = (isl_union_map *) user;
  ast_build_info *for_info = XNEW (struct ast_build_info);
  isl_union_map *schedule = isl_ast_build_get_schedule (build);
  isl_space *schedule_space = isl_ast_build_get_schedule_space (build);
  int dimension = isl_space_dim (schedule_space, isl_dim_out);
  for_info->is_parallelizable =
    !carries_deps (schedule, dependences, dimension);
  isl_union_map_free (schedule);
  isl_space_free (schedule_space);
  isl_id *id = isl_id_alloc (isl_ast_build_get_ctx (build), "", for_info);
  return id;
}

/* Set the separate option for all dimensions.
   This helps to reduce control overhead.
   Set the options for unroll and jam.  */

static __isl_give isl_ast_build *
set_options (__isl_take isl_ast_build *control,
	     __isl_keep isl_union_map *schedule,
	     __isl_take isl_union_map *opt_luj)
{
  isl_ctx *ctx = isl_union_map_get_ctx (schedule);
  isl_space *range_space = isl_space_set_alloc (ctx, 0, 1);
  range_space =
    isl_space_set_tuple_name (range_space, isl_dim_set, "separate");
  isl_union_set *range =
    isl_union_set_from_set (isl_set_universe (range_space));  
  isl_union_set *domain = isl_union_map_range (isl_union_map_copy (schedule));
  domain = isl_union_set_universe (domain);
  isl_union_map *options = isl_union_map_from_domain_and_range (domain, range);

  options = isl_union_map_union (options, opt_luj);

  return isl_ast_build_set_options (control, options);
}

static __isl_give isl_ast_node *
scop_to_isl_ast (scop_p scop, ivs_params &ip)
{
  /* Generate loop upper bounds that consist of the current loop iterator,
  an operator (< or <=) and an expression not involving the iterator.
  If this option is not set, then the current loop iterator may appear several
  times in the upper bound. See the isl manual for more details.  */
  isl_options_set_ast_build_atomic_upper_bound (scop->ctx, true);

  add_parameters_to_ivs_params (scop, ip);

  isl_union_map *options_luj = generate_luj_options (scop);

  isl_union_map *schedule_isl = generate_isl_schedule (scop);
  isl_ast_build *context_isl = generate_isl_context (scop);

  context_isl = set_options (context_isl, schedule_isl, options_luj);

  isl_union_map *dependences = NULL;
  if (flag_loop_parallelize_all)
  {
    dependences = scop_get_dependences (scop);
    context_isl =
      isl_ast_build_set_before_each_for (context_isl, ast_build_before_for,
					 dependences);
  }
  isl_ast_node *ast_isl = isl_ast_build_ast_from_schedule (context_isl,
							   schedule_isl);
  if(dependences)
    isl_union_map_free (dependences);
  isl_ast_build_free (context_isl);
  return ast_isl;
}

/* GIMPLE Loop Generator: generates loops from STMT in GIMPLE form for
   the given SCOP.  Return true if code generation succeeded.

   FIXME: This is not yet a full implementation of the code generator
          with ISL ASTs. Generation of GIMPLE code has to be completed.  */

bool
graphite_regenerate_ast_isl (scop_p scop)
{
  loop_p context_loop;
  sese region = SCOP_REGION (scop);
  ifsese if_region = NULL;
  isl_ast_node *root_node;
  ivs_params ip;

  timevar_push (TV_GRAPHITE_CODE_GEN);
  graphite_regenerate_error = false;
  root_node = scop_to_isl_ast (scop, ip);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nISL AST generated by ISL: \n");
      print_isl_ast_node (dump_file, root_node, scop->ctx);
      fprintf (dump_file, "\n");
    }

  recompute_all_dominators ();
  graphite_verify ();

  if_region = move_sese_in_condition (region);
  sese_insert_phis_for_liveouts (region,
				 if_region->region->exit->src,
				 if_region->false_region->exit,
				 if_region->true_region->exit);
  recompute_all_dominators ();
  graphite_verify ();

  context_loop = SESE_ENTRY (region)->src->loop_father;

  translate_isl_ast (context_loop, root_node, if_region->true_region->entry,
		     ip);
  graphite_verify ();
  scev_reset ();
  recompute_all_dominators ();
  graphite_verify ();

  if (graphite_regenerate_error)
    set_ifsese_condition (if_region, integer_zero_node);

  free (if_region->true_region);
  free (if_region->region);
  free (if_region);

  ivs_params_clear (ip);
  isl_ast_node_free (root_node);
  timevar_pop (TV_GRAPHITE_CODE_GEN);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      loop_p loop;
      int num_no_dependency = 0;

      FOR_EACH_LOOP (loop, 0)
	if (loop->can_be_parallel)
	  num_no_dependency++;

      fprintf (dump_file, "\n%d loops carried no dependency.\n",
	       num_no_dependency);
    }

  return !graphite_regenerate_error;
}
#endif
