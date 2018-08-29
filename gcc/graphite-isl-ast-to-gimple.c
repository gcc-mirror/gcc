/* Translation of isl AST to Gimple.
   Copyright (C) 2014-2018 Free Software Foundation, Inc.
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

#define USES_ISL

#include "config.h"

#ifdef HAVE_isl

#define INCLUDE_MAP
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "cfghooks.h"
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "params.h"
#include "fold-const.h"
#include "gimple-fold.h"
#include "gimple-iterator.h"
#include "gimplify.h"
#include "gimplify-me.h"
#include "tree-eh.h"
#include "tree-ssa-loop.h"
#include "tree-ssa-operands.h"
#include "tree-ssa-propagate.h"
#include "tree-pass.h"
#include "cfgloop.h"
#include "tree-data-ref.h"
#include "tree-ssa-loop-manip.h"
#include "tree-scalar-evolution.h"
#include "gimple-ssa.h"
#include "tree-phinodes.h"
#include "tree-into-ssa.h"
#include "ssa-iterators.h"
#include "tree-cfg.h"
#include "gimple-pretty-print.h"
#include "cfganal.h"
#include "value-prof.h"
#include "tree-ssa.h"
#include "tree-vectorizer.h"
#include "graphite.h"

struct ast_build_info
{
  ast_build_info()
    : is_parallelizable(false)
  { }
  bool is_parallelizable;
};

/* IVS_PARAMS maps isl's scattering and parameter identifiers
   to corresponding trees.  */

typedef std::map<isl_id *, tree> ivs_params;

/* Free all memory allocated for isl's identifiers.  */

static void ivs_params_clear (ivs_params &ip)
{
  std::map<isl_id *, tree>::iterator it;
  for (it = ip.begin ();
       it != ip.end (); it++)
    {
      isl_id_free (it->first);
    }
}

/* Set the "separate" option for the schedule node.  */

static isl_schedule_node *
set_separate_option (__isl_take isl_schedule_node *node, void *user)
{
  if (user)
    return node;

  if (isl_schedule_node_get_type (node) != isl_schedule_node_band)
    return node;

  /* Set the "separate" option unless it is set earlier to another option.  */
  if (isl_schedule_node_band_member_get_ast_loop_type (node, 0)
      == isl_ast_loop_default)
    return isl_schedule_node_band_member_set_ast_loop_type
      (node, 0, isl_ast_loop_separate);

  return node;
}

/* Print SCHEDULE under an AST form on file F.  */

void
print_schedule_ast (FILE *f, __isl_keep isl_schedule *schedule, scop_p scop)
{
  isl_set *set = isl_set_params (isl_set_copy (scop->param_context));
  isl_ast_build *context = isl_ast_build_from_context (set);
  isl_ast_node *ast
    = isl_ast_build_node_from_schedule (context, isl_schedule_copy (schedule));
  isl_ast_build_free (context);
  print_isl_ast (f, ast);
  isl_ast_node_free (ast);
}

DEBUG_FUNCTION void
debug_schedule_ast (__isl_keep isl_schedule *s, scop_p scop)
{
  print_schedule_ast (stderr, s, scop);
}

enum phi_node_kind
{
  unknown_phi,
  loop_phi,
  close_phi,
  cond_phi
};

class translate_isl_ast_to_gimple
{
 public:
  translate_isl_ast_to_gimple (sese_info_p r);
  edge translate_isl_ast (loop_p context_loop, __isl_keep isl_ast_node *node,
			  edge next_e, ivs_params &ip);
  edge translate_isl_ast_node_for (loop_p context_loop,
				   __isl_keep isl_ast_node *node,
				   edge next_e, ivs_params &ip);
  edge translate_isl_ast_for_loop (loop_p context_loop,
				   __isl_keep isl_ast_node *node_for,
				   edge next_e,
				   tree type, tree lb, tree ub,
				   ivs_params &ip);
  edge translate_isl_ast_node_if (loop_p context_loop,
				  __isl_keep isl_ast_node *node,
				  edge next_e, ivs_params &ip);
  edge translate_isl_ast_node_user (__isl_keep isl_ast_node *node,
				    edge next_e, ivs_params &ip);
  edge translate_isl_ast_node_block (loop_p context_loop,
				     __isl_keep isl_ast_node *node,
				     edge next_e, ivs_params &ip);
  tree unary_op_to_tree (tree type, __isl_take isl_ast_expr *expr,
			 ivs_params &ip);
  tree binary_op_to_tree (tree type, __isl_take isl_ast_expr *expr,
			  ivs_params &ip);
  tree ternary_op_to_tree (tree type, __isl_take isl_ast_expr *expr,
			   ivs_params &ip);
  tree nary_op_to_tree (tree type, __isl_take isl_ast_expr *expr,
			ivs_params &ip);
  tree gcc_expression_from_isl_expression (tree type,
					   __isl_take isl_ast_expr *,
					   ivs_params &ip);
  tree gcc_expression_from_isl_ast_expr_id (tree type,
					    __isl_keep isl_ast_expr *expr_id,
					    ivs_params &ip);
  widest_int widest_int_from_isl_expr_int (__isl_keep isl_ast_expr *expr);
  tree gcc_expression_from_isl_expr_int (tree type,
					 __isl_take isl_ast_expr *expr);
  tree gcc_expression_from_isl_expr_op (tree type,
					__isl_take isl_ast_expr *expr,
					ivs_params &ip);
  struct loop *graphite_create_new_loop (edge entry_edge,
					 __isl_keep isl_ast_node *node_for,
					 loop_p outer, tree type,
					 tree lb, tree ub, ivs_params &ip);
  edge graphite_create_new_guard (edge entry_edge,
				  __isl_take isl_ast_expr *if_cond,
				  ivs_params &ip);
  void build_iv_mapping (vec<tree> iv_map, gimple_poly_bb_p gbb,
			 __isl_keep isl_ast_expr *user_expr, ivs_params &ip,
			 sese_l &region);
  void add_parameters_to_ivs_params (scop_p scop, ivs_params &ip);
  __isl_give isl_ast_build *generate_isl_context (scop_p scop);

  __isl_give isl_ast_node * scop_to_isl_ast (scop_p scop);

  tree get_rename_from_scev (tree old_name, gimple_seq *stmts, loop_p loop,
			     vec<tree> iv_map);
  void graphite_copy_stmts_from_block (basic_block bb, basic_block new_bb,
				       vec<tree> iv_map);
  edge copy_bb_and_scalar_dependences (basic_block bb, edge next_e,
				       vec<tree> iv_map);
  void set_rename (tree old_name, tree expr);
  void gsi_insert_earliest (gimple_seq seq);
  bool codegen_error_p () const { return codegen_error; }

  void set_codegen_error ()
  {
    codegen_error = true;
    gcc_assert (! flag_checking
		|| PARAM_VALUE (PARAM_GRAPHITE_ALLOW_CODEGEN_ERRORS));
  }

  bool is_constant (tree op) const
  {
    return TREE_CODE (op) == INTEGER_CST
      || TREE_CODE (op) == REAL_CST
      || TREE_CODE (op) == COMPLEX_CST
      || TREE_CODE (op) == VECTOR_CST;
  }

private:
  /* The region to be translated.  */
  sese_info_p region;

  /* This flag is set when an error occurred during the translation of isl AST
     to Gimple.  */
  bool codegen_error;

  /* A vector of all the edges at if_condition merge points.  */
  auto_vec<edge, 2> merge_points;

  tree graphite_expr_type;
};

translate_isl_ast_to_gimple::translate_isl_ast_to_gimple (sese_info_p r)
  : region (r), codegen_error (false)
{
  /* We always try to use signed 128 bit types, but fall back to smaller types
     in case a platform does not provide types of these sizes. In the future we
     should use isl to derive the optimal type for each subexpression.  */
  int max_mode_int_precision
    = GET_MODE_PRECISION (int_mode_for_size (MAX_FIXED_MODE_SIZE, 0).require ());
  int graphite_expr_type_precision
    = 128 <= max_mode_int_precision ?  128 : max_mode_int_precision;
  graphite_expr_type
    = build_nonstandard_integer_type (graphite_expr_type_precision, 0);
}

/* Return the tree variable that corresponds to the given isl ast identifier
   expression (an isl_ast_expr of type isl_ast_expr_id).

   FIXME: We should replace blind conversion of id's type with derivation
   of the optimal type when we get the corresponding isl support.  Blindly
   converting type sizes may be problematic when we switch to smaller
   types.  */

tree translate_isl_ast_to_gimple::
gcc_expression_from_isl_ast_expr_id (tree type,
				     __isl_take isl_ast_expr *expr_id,
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
  tree t = res->second;
  if (useless_type_conversion_p (type, TREE_TYPE (t)))
    return t;
  return fold_convert (type, t);
}

/* Converts an isl_ast_expr_int expression E to a widest_int.
   Raises a code generation error when the constant doesn't fit.  */

widest_int translate_isl_ast_to_gimple::
widest_int_from_isl_expr_int (__isl_keep isl_ast_expr *expr)
{
  gcc_assert (isl_ast_expr_get_type (expr) == isl_ast_expr_int);
  isl_val *val = isl_ast_expr_get_val (expr);
  size_t n = isl_val_n_abs_num_chunks (val, sizeof (HOST_WIDE_INT));
  HOST_WIDE_INT *chunks = XALLOCAVEC (HOST_WIDE_INT, n);
  if (n > WIDE_INT_MAX_ELTS
      || isl_val_get_abs_num_chunks (val, sizeof (HOST_WIDE_INT), chunks) == -1)
    {
      isl_val_free (val);
      set_codegen_error ();
      return 0;
    }
  widest_int wi = widest_int::from_array (chunks, n, true);
  if (isl_val_is_neg (val))
    wi = -wi;
  isl_val_free (val);
  return wi;
}

/* Converts an isl_ast_expr_int expression E to a GCC expression tree of
   type TYPE.  Raises a code generation error when the constant doesn't fit.  */

tree translate_isl_ast_to_gimple::
gcc_expression_from_isl_expr_int (tree type, __isl_take isl_ast_expr *expr)
{
  widest_int wi = widest_int_from_isl_expr_int (expr);
  isl_ast_expr_free (expr);
  if (codegen_error_p ())
    return NULL_TREE;
  if (wi::min_precision (wi, TYPE_SIGN (type)) > TYPE_PRECISION (type))
    {
      set_codegen_error ();
      return NULL_TREE;
    }
  return wide_int_to_tree (type, wi);
}

/* Converts a binary isl_ast_expr_op expression E to a GCC expression tree of
   type TYPE.  */

tree translate_isl_ast_to_gimple::
binary_op_to_tree (tree type, __isl_take isl_ast_expr *expr, ivs_params &ip)
{
  enum isl_ast_op_type expr_type = isl_ast_expr_get_op_type (expr);
  isl_ast_expr *arg_expr = isl_ast_expr_get_op_arg (expr, 0);
  tree tree_lhs_expr = gcc_expression_from_isl_expression (type, arg_expr, ip);
  arg_expr = isl_ast_expr_get_op_arg (expr, 1);
  isl_ast_expr_free (expr);

  /* From our constraint generation we may get modulo operations that
     we cannot represent explicitely but that are no-ops for TYPE.
     Elide those.  */
  if ((expr_type == isl_ast_op_pdiv_r
       || expr_type == isl_ast_op_zdiv_r
       || expr_type == isl_ast_op_add)
      && isl_ast_expr_get_type (arg_expr) == isl_ast_expr_int
      && (wi::exact_log2 (widest_int_from_isl_expr_int (arg_expr))
	  >= TYPE_PRECISION (type)))
    {
      isl_ast_expr_free (arg_expr);
      return tree_lhs_expr;
    }

  tree tree_rhs_expr = gcc_expression_from_isl_expression (type, arg_expr, ip);
  if (codegen_error_p ())
    return NULL_TREE;

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

    case isl_ast_op_zdiv_r:
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

tree translate_isl_ast_to_gimple::
ternary_op_to_tree (tree type, __isl_take isl_ast_expr *expr, ivs_params &ip)
{
  enum isl_ast_op_type t = isl_ast_expr_get_op_type (expr);
  gcc_assert (t == isl_ast_op_cond || t == isl_ast_op_select);
  isl_ast_expr *arg_expr = isl_ast_expr_get_op_arg (expr, 0);
  tree a = gcc_expression_from_isl_expression (type, arg_expr, ip);
  arg_expr = isl_ast_expr_get_op_arg (expr, 1);
  tree b = gcc_expression_from_isl_expression (type, arg_expr, ip);
  arg_expr = isl_ast_expr_get_op_arg (expr, 2);
  tree c = gcc_expression_from_isl_expression (type, arg_expr, ip);
  isl_ast_expr_free (expr);

  if (codegen_error_p ())
    return NULL_TREE;

  return fold_build3 (COND_EXPR, type, a, b, c);
}

/* Converts a unary isl_ast_expr_op expression E to a GCC expression tree of
   type TYPE.  */

tree translate_isl_ast_to_gimple::
unary_op_to_tree (tree type, __isl_take isl_ast_expr *expr, ivs_params &ip)
{
  gcc_assert (isl_ast_expr_get_op_type (expr) == isl_ast_op_minus);
  isl_ast_expr *arg_expr = isl_ast_expr_get_op_arg (expr, 0);
  tree tree_expr = gcc_expression_from_isl_expression (type, arg_expr, ip);
  isl_ast_expr_free (expr);
  return codegen_error_p () ? NULL_TREE
    : fold_build1 (NEGATE_EXPR, type, tree_expr);
}

/* Converts an isl_ast_expr_op expression E with unknown number of arguments
   to a GCC expression tree of type TYPE.  */

tree translate_isl_ast_to_gimple::
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

  if (codegen_error_p ())
    {
      isl_ast_expr_free (expr);
      return NULL_TREE;
    }

  int i;
  for (i = 1; i < isl_ast_expr_get_op_n_arg (expr); i++)
    {
      arg_expr = isl_ast_expr_get_op_arg (expr, i);
      tree t = gcc_expression_from_isl_expression (type, arg_expr, ip);

      if (codegen_error_p ())
	{
	  isl_ast_expr_free (expr);
	  return NULL_TREE;
	}

      res = fold_build2 (op_code, type, res, t);
    }
  isl_ast_expr_free (expr);
  return res;
}

/* Converts an isl_ast_expr_op expression E to a GCC expression tree of
   type TYPE.  */

tree translate_isl_ast_to_gimple::
gcc_expression_from_isl_expr_op (tree type, __isl_take isl_ast_expr *expr,
				 ivs_params &ip)
{
  if (codegen_error_p ())
    {
      isl_ast_expr_free (expr);
      return NULL_TREE;
    }

  gcc_assert (isl_ast_expr_get_type (expr) == isl_ast_expr_op);
  switch (isl_ast_expr_get_op_type (expr))
    {
    /* These isl ast expressions are not supported yet.  */
    case isl_ast_op_error:
    case isl_ast_op_call:
    case isl_ast_op_and_then:
    case isl_ast_op_or_else:
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
    case isl_ast_op_zdiv_r:
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
    case isl_ast_op_select:
      return ternary_op_to_tree (type, expr, ip);

    default:
      gcc_unreachable ();
    }

  return NULL_TREE;
}

/* Converts an isl AST expression E back to a GCC expression tree of
   type TYPE.  */

tree translate_isl_ast_to_gimple::
gcc_expression_from_isl_expression (tree type, __isl_take isl_ast_expr *expr,
				    ivs_params &ip)
{
  if (codegen_error_p ())
    {
      isl_ast_expr_free (expr);
      return NULL_TREE;
    }

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
   isl's scattering name to the induction variable created for the
   loop of STMT.  The new induction variable is inserted in the NEWIVS
   vector and is of type TYPE.  */

struct loop *translate_isl_ast_to_gimple::
graphite_create_new_loop (edge entry_edge, __isl_keep isl_ast_node *node_for,
			  loop_p outer, tree type, tree lb, tree ub,
			  ivs_params &ip)
{
  isl_ast_expr *for_inc = isl_ast_node_for_get_inc (node_for);
  tree stride = gcc_expression_from_isl_expression (type, for_inc, ip);

  /* To fail code generation, we generate wrong code until we discard it.  */
  if (codegen_error_p ())
    stride = integer_zero_node;

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

/* Create the loop for a isl_ast_node_for.

   - NEXT_E is the edge where new generated code should be attached.  */

edge translate_isl_ast_to_gimple::
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

  /* Translate the body of the loop.  */
  isl_ast_node *for_body = isl_ast_node_for_get_body (node_for);
  next_e = translate_isl_ast (loop, for_body, to_body, ip);
  isl_ast_node_free (for_body);

  /* Early return if we failed to translate loop body.  */
  if (!next_e || codegen_error_p ())
    return NULL;

  if (next_e->dest != after)
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
	/* (iterator < ub) => (iterator <= ub - 1).  */
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

/* Translates an isl_ast_node_for to Gimple. */

edge translate_isl_ast_to_gimple::
translate_isl_ast_node_for (loop_p context_loop, __isl_keep isl_ast_node *node,
			    edge next_e, ivs_params &ip)
{
  gcc_assert (isl_ast_node_get_type (node) == isl_ast_node_for);
  tree type = graphite_expr_type;

  isl_ast_expr *for_init = isl_ast_node_for_get_init (node);
  tree lb = gcc_expression_from_isl_expression (type, for_init, ip);
  /* To fail code generation, we generate wrong code until we discard it.  */
  if (codegen_error_p ())
    lb = integer_zero_node;

  isl_ast_expr *upper_bound = get_upper_bound (node);
  tree ub = gcc_expression_from_isl_expression (type, upper_bound, ip);
  /* To fail code generation, we generate wrong code until we discard it.  */
  if (codegen_error_p ())
    ub = integer_zero_node;

  edge last_e = single_succ_edge (split_edge (next_e));

  /* Compensate for the fact that we emit a do { } while loop from
     a for ISL AST.
     ???  We often miss constraints on niter because the SESE region
     doesn't cover loop header copies.  Ideally we'd add constraints
     for all relevant dominating conditions.  */
  if (TREE_CODE (lb) == INTEGER_CST && TREE_CODE (ub) == INTEGER_CST
      && tree_int_cst_compare (lb, ub) <= 0)
    ;
  else
    {
      tree one = build_one_cst (POINTER_TYPE_P (type) ? sizetype : type);
      /* Adding +1 and using LT_EXPR helps with loop latches that have a
	 loop iteration count of "PARAMETER - 1".  For PARAMETER == 0 this
	 becomes 2^k-1 due to integer overflow, and the condition lb <= ub
	 is true, even if we do not want this.  However lb < ub + 1 is false,
	 as expected.  */
      tree ub_one = fold_build2 (POINTER_TYPE_P (type)
				 ? POINTER_PLUS_EXPR : PLUS_EXPR,
				 type, unshare_expr (ub), one);
      create_empty_if_region_on_edge (next_e,
				      fold_build2 (LT_EXPR, boolean_type_node,
						   unshare_expr (lb), ub_one));
      next_e = get_true_edge_from_guard_bb (next_e->dest);
    }

  translate_isl_ast_for_loop (context_loop, node, next_e,
			      type, lb, ub, ip);
  return last_e;
}

/* Inserts in iv_map a tuple (OLD_LOOP->num, NEW_NAME) for the induction
   variables of the loops around GBB in SESE.
 
   FIXME: Instead of using a vec<tree> that maps each loop id to a possible
   chrec, we could consider using a map<int, tree> that maps loop ids to the
   corresponding tree expressions.  */

void translate_isl_ast_to_gimple::
build_iv_mapping (vec<tree> iv_map, gimple_poly_bb_p gbb,
		  __isl_keep isl_ast_expr *user_expr, ivs_params &ip,
		  sese_l &region)
{
  gcc_assert (isl_ast_expr_get_type (user_expr) == isl_ast_expr_op &&
	      isl_ast_expr_get_op_type (user_expr) == isl_ast_op_call);
  int i;
  isl_ast_expr *arg_expr;
  for (i = 1; i < isl_ast_expr_get_op_n_arg (user_expr); i++)
    {
      arg_expr = isl_ast_expr_get_op_arg (user_expr, i);
      tree type = graphite_expr_type;
      tree t = gcc_expression_from_isl_expression (type, arg_expr, ip);

      /* To fail code generation, we generate wrong code until we discard it.  */
      if (codegen_error_p ())
	t = integer_zero_node;

      loop_p old_loop = gbb_loop_at_index (gbb, region, i - 1);
      iv_map[old_loop->num] = t;
    }
}

/* Translates an isl_ast_node_user to Gimple.

   FIXME: We should remove iv_map.create (loop->num + 1), if it is possible.  */

edge translate_isl_ast_to_gimple::
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

  gimple_poly_bb_p gbb = PBB_BLACK_BOX (pbb);

  isl_ast_expr_free (name_expr);
  isl_id_free (name_id);

  gcc_assert (GBB_BB (gbb) != ENTRY_BLOCK_PTR_FOR_FN (cfun) &&
	      "The entry block should not even appear within a scop");

  const int nb_loops = number_of_loops (cfun);
  vec<tree> iv_map;
  iv_map.create (nb_loops);
  iv_map.safe_grow_cleared (nb_loops);

  build_iv_mapping (iv_map, gbb, user_expr, ip, pbb->scop->scop_info->region);
  isl_ast_expr_free (user_expr);

  basic_block old_bb = GBB_BB (gbb);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file,
	       "[codegen] copying from bb_%d on edge (bb_%d, bb_%d)\n",
	       old_bb->index, next_e->src->index, next_e->dest->index);
      print_loops_bb (dump_file, GBB_BB (gbb), 0, 3);
    }

  next_e = copy_bb_and_scalar_dependences (old_bb, next_e, iv_map);

  iv_map.release ();

  if (codegen_error_p ())
    return NULL;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "[codegen] (after copy) new basic block\n");
      print_loops_bb (dump_file, next_e->src, 0, 3);
    }

  return next_e;
}

/* Translates an isl_ast_node_block to Gimple. */

edge translate_isl_ast_to_gimple::
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
 
/* Creates a new if region corresponding to isl's cond.  */

edge translate_isl_ast_to_gimple::
graphite_create_new_guard (edge entry_edge, __isl_take isl_ast_expr *if_cond,
			   ivs_params &ip)
{
  tree type = graphite_expr_type;
  tree cond_expr = gcc_expression_from_isl_expression (type, if_cond, ip);

  /* To fail code generation, we generate wrong code until we discard it.  */
  if (codegen_error_p ())
    cond_expr = integer_zero_node;

  edge exit_edge = create_empty_if_region_on_edge (entry_edge, cond_expr);
  return exit_edge;
}

/* Translates an isl_ast_node_if to Gimple.  */

edge translate_isl_ast_to_gimple::
translate_isl_ast_node_if (loop_p context_loop,
			   __isl_keep isl_ast_node *node,
			   edge next_e, ivs_params &ip)
{
  gcc_assert (isl_ast_node_get_type (node) == isl_ast_node_if);
  isl_ast_expr *if_cond = isl_ast_node_if_get_cond (node);
  edge last_e = graphite_create_new_guard (next_e, if_cond, ip);
  edge true_e = get_true_edge_from_guard_bb (next_e->dest);
  merge_points.safe_push (last_e);

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

/* Translates an isl AST node NODE to GCC representation in the
   context of a SESE.  */

edge translate_isl_ast_to_gimple::
translate_isl_ast (loop_p context_loop, __isl_keep isl_ast_node *node,
		   edge next_e, ivs_params &ip)
{
  if (codegen_error_p ())
    return NULL;

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

    case isl_ast_node_mark:
      {
	isl_ast_node *n = isl_ast_node_mark_get_node (node);
	edge e = translate_isl_ast (context_loop, n, next_e, ip);
	isl_ast_node_free (n);
	return e;
      }

    default:
      gcc_unreachable ();
    }
}

/* Register in RENAME_MAP the rename tuple (OLD_NAME, EXPR).
   When OLD_NAME and EXPR are the same we assert.  */

void translate_isl_ast_to_gimple::
set_rename (tree old_name, tree expr)
{
  if (dump_file)
    {
      fprintf (dump_file, "[codegen] setting rename: old_name = ");
      print_generic_expr (dump_file, old_name);
      fprintf (dump_file, ", new decl = ");
      print_generic_expr (dump_file, expr);
      fprintf (dump_file, "\n");
    }
  bool res = region->rename_map->put (old_name, expr);
  gcc_assert (! res);
}

/* Return an iterator to the instructions comes last in the execution order.
   Either GSI1 and GSI2 should belong to the same basic block or one of their
   respective basic blocks should dominate the other.  */

gimple_stmt_iterator
later_of_the_two (gimple_stmt_iterator gsi1, gimple_stmt_iterator gsi2)
{
  basic_block bb1 = gsi_bb (gsi1);
  basic_block bb2 = gsi_bb (gsi2);

  /* Find the iterator which is the latest.  */
  if (bb1 == bb2)
    {
      gimple *stmt1 = gsi_stmt (gsi1);
      gimple *stmt2 = gsi_stmt (gsi2);

      if (stmt1 != NULL && stmt2 != NULL)
	{
	  bool is_phi1 = gimple_code (stmt1) == GIMPLE_PHI;
	  bool is_phi2 = gimple_code (stmt2) == GIMPLE_PHI;

	  if (is_phi1 != is_phi2)
	    return is_phi1 ? gsi2 : gsi1;
	}

      /* For empty basic blocks gsis point to the end of the sequence.  Since
	 there is no operator== defined for gimple_stmt_iterator and for gsis
	 not pointing to a valid statement gsi_next would assert.  */
      gimple_stmt_iterator gsi = gsi1;
      do {
	if (gsi_stmt (gsi) == gsi_stmt (gsi2))
	  return gsi2;
	gsi_next (&gsi);
      } while (!gsi_end_p (gsi));

      return gsi1;
    }

  /* Find the basic block closest to the basic block which defines stmt.  */
  if (dominated_by_p (CDI_DOMINATORS, bb1, bb2))
    return gsi1;

  gcc_assert (dominated_by_p (CDI_DOMINATORS, bb2, bb1));
  return gsi2;
}

/* Insert each statement from SEQ at its earliest insertion p.  */

void translate_isl_ast_to_gimple::
gsi_insert_earliest (gimple_seq seq)
{
  update_modified_stmts (seq);
  sese_l &codegen_region = region->if_region->true_region->region;
  basic_block begin_bb = get_entry_bb (codegen_region);

  /* Inserting the gimple statements in a vector because gimple_seq behave
     in strage ways when inserting the stmts from it into different basic
     blocks one at a time.  */
  auto_vec<gimple *, 3> stmts;
  for (gimple_stmt_iterator gsi = gsi_start (seq); !gsi_end_p (gsi);
       gsi_next (&gsi))
    stmts.safe_push (gsi_stmt (gsi));

  int i;
  gimple *use_stmt;
  FOR_EACH_VEC_ELT (stmts, i, use_stmt)
    {
      gcc_assert (gimple_code (use_stmt) != GIMPLE_PHI);
      gimple_stmt_iterator gsi_def_stmt = gsi_start_nondebug_bb (begin_bb);

      use_operand_p use_p;
      ssa_op_iter op_iter;
      FOR_EACH_SSA_USE_OPERAND (use_p, use_stmt, op_iter, SSA_OP_USE)
	{
	  /* Iterator to the current def of use_p.  For function parameters or
	     anything where def is not found, insert at the beginning of the
	     generated region.  */
	  gimple_stmt_iterator gsi_stmt = gsi_def_stmt;

	  tree op = USE_FROM_PTR (use_p);
	  gimple *stmt = SSA_NAME_DEF_STMT (op);
	  if (stmt && (gimple_code (stmt) != GIMPLE_NOP))
	    gsi_stmt = gsi_for_stmt (stmt);

	  /* For region parameters, insert at the beginning of the generated
	     region.  */
	  if (!bb_in_sese_p (gsi_bb (gsi_stmt), codegen_region))
	    gsi_stmt = gsi_def_stmt;

	  gsi_def_stmt = later_of_the_two (gsi_stmt, gsi_def_stmt);
	}

      if (!gsi_stmt (gsi_def_stmt))
	{
	  gimple_stmt_iterator gsi = gsi_after_labels (gsi_bb (gsi_def_stmt));
	  gsi_insert_before (&gsi, use_stmt, GSI_NEW_STMT);
	}
      else if (gimple_code (gsi_stmt (gsi_def_stmt)) == GIMPLE_PHI)
	{
	  gimple_stmt_iterator bsi
	    = gsi_start_nondebug_bb (gsi_bb (gsi_def_stmt));
	  /* Insert right after the PHI statements.  */
	  gsi_insert_before (&bsi, use_stmt, GSI_NEW_STMT);
	}
      else
	gsi_insert_after (&gsi_def_stmt, use_stmt, GSI_NEW_STMT);

      if (dump_file)
	{
	  fprintf (dump_file, "[codegen] inserting statement in BB %d: ",
		   gimple_bb (use_stmt)->index);
	  print_gimple_stmt (dump_file, use_stmt, 0, TDF_VOPS | TDF_MEMSYMS);
	}
    }
}

/* For ops which are scev_analyzeable, we can regenerate a new name from its
   scalar evolution around LOOP.  */

tree translate_isl_ast_to_gimple::
get_rename_from_scev (tree old_name, gimple_seq *stmts, loop_p loop,
		      vec<tree> iv_map)
{
  tree scev = scalar_evolution_in_region (region->region, loop, old_name);

  /* At this point we should know the exact scev for each
     scalar SSA_NAME used in the scop: all the other scalar
     SSA_NAMEs should have been translated out of SSA using
     arrays with one element.  */
  tree new_expr;
  if (chrec_contains_undetermined (scev))
    {
      set_codegen_error ();
      return build_zero_cst (TREE_TYPE (old_name));
    }

  new_expr = chrec_apply_map (scev, iv_map);

  /* The apply should produce an expression tree containing
     the uses of the new induction variables.  We should be
     able to use new_expr instead of the old_name in the newly
     generated loop nest.  */
  if (chrec_contains_undetermined (new_expr)
      || tree_contains_chrecs (new_expr, NULL))
    {
      set_codegen_error ();
      return build_zero_cst (TREE_TYPE (old_name));
    }

  /* Replace the old_name with the new_expr.  */
  return force_gimple_operand (unshare_expr (new_expr), stmts,
			       true, NULL_TREE);
}


/* Return true if STMT should be copied from region to the new code-generated
   region.  LABELs, CONDITIONS, induction-variables and region parameters need
   not be copied.  */

static bool
should_copy_to_new_region (gimple *stmt, sese_info_p region)
{
  /* Do not copy labels or conditions.  */
  if (gimple_code (stmt) == GIMPLE_LABEL
      || gimple_code (stmt) == GIMPLE_COND)
    return false;

  tree lhs;
  /* Do not copy induction variables.  */
  if (is_gimple_assign (stmt)
      && (lhs = gimple_assign_lhs (stmt))
      && TREE_CODE (lhs) == SSA_NAME
      && scev_analyzable_p (lhs, region->region)
      /* But to code-generate liveouts - liveout PHI generation is
         in generic sese.c code that cannot do code generation.  */
      && ! bitmap_bit_p (region->liveout, SSA_NAME_VERSION (lhs)))
    return false;

  return true;
}

/* Duplicates the statements of basic block BB into basic block NEW_BB
   and compute the new induction variables according to the IV_MAP.  */

void translate_isl_ast_to_gimple::
graphite_copy_stmts_from_block (basic_block bb, basic_block new_bb,
				vec<tree> iv_map)
{
  /* Iterator poining to the place where new statement (s) will be inserted.  */
  gimple_stmt_iterator gsi_tgt = gsi_last_bb (new_bb);

  for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      if (!should_copy_to_new_region (stmt, region))
	continue;

      /* Create a new copy of STMT and duplicate STMT's virtual
	 operands.  */
      gimple *copy = gimple_copy (stmt);

      /* Rather than not copying debug stmts we reset them.
         ???  Where we can rewrite uses without inserting new
	 stmts we could simply do that.  */
      if (is_gimple_debug (copy))
	{
	  if (gimple_debug_bind_p (copy))
	    gimple_debug_bind_reset_value (copy);
	  else if (gimple_debug_source_bind_p (copy)
		   || gimple_debug_nonbind_marker_p (copy))
	    ;
	  else
	    gcc_unreachable ();
	}

      maybe_duplicate_eh_stmt (copy, stmt);
      gimple_duplicate_stmt_histograms (cfun, copy, cfun, stmt);

      /* Crete new names for each def in the copied stmt.  */
      def_operand_p def_p;
      ssa_op_iter op_iter;
      FOR_EACH_SSA_DEF_OPERAND (def_p, copy, op_iter, SSA_OP_ALL_DEFS)
	{
	  tree old_name = DEF_FROM_PTR (def_p);
	  create_new_def_for (old_name, copy, def_p);
	}

      gsi_insert_after (&gsi_tgt, copy, GSI_NEW_STMT);
      if (dump_file)
	{
	  fprintf (dump_file, "[codegen] inserting statement: ");
	  print_gimple_stmt (dump_file, copy, 0);
	}

      /* For each SCEV analyzable SSA_NAME, rename their usage.  */
      ssa_op_iter iter;
      use_operand_p use_p;
      if (!is_gimple_debug (copy))
	{
	  bool changed = false;
	  FOR_EACH_SSA_USE_OPERAND (use_p, copy, iter, SSA_OP_USE)
	    {
	      tree old_name = USE_FROM_PTR (use_p);

	      if (TREE_CODE (old_name) != SSA_NAME
		  || SSA_NAME_IS_DEFAULT_DEF (old_name)
		  || ! scev_analyzable_p (old_name, region->region))
		continue;

	      gimple_seq stmts = NULL;
	      tree new_name = get_rename_from_scev (old_name, &stmts,
						    bb->loop_father, iv_map);
	      if (! codegen_error_p ())
		gsi_insert_earliest (stmts);
	      replace_exp (use_p, new_name);
	      changed = true;
	    }
	  if (changed)
	    fold_stmt_inplace (&gsi_tgt);
	}

      update_stmt (copy);
    }
}


/* Copies BB and includes in the copied BB all the statements that can
   be reached following the use-def chains from the memory accesses,
   and returns the next edge following this new block.  */

edge translate_isl_ast_to_gimple::
copy_bb_and_scalar_dependences (basic_block bb, edge next_e, vec<tree> iv_map)
{
  basic_block new_bb = split_edge (next_e);
  gimple_stmt_iterator gsi_tgt = gsi_last_bb (new_bb);
  for (gphi_iterator psi = gsi_start_phis (bb); !gsi_end_p (psi);
       gsi_next (&psi))
    {
      gphi *phi = psi.phi ();
      tree res = gimple_phi_result (phi);
      if (virtual_operand_p (res)
	  || scev_analyzable_p (res, region->region))
	continue;

      tree new_phi_def;
      tree *rename = region->rename_map->get (res);
      if (! rename)
	{
	  new_phi_def = create_tmp_reg (TREE_TYPE (res));
	  set_rename (res, new_phi_def);
	}
      else
	new_phi_def = *rename;

      gassign *ass = gimple_build_assign (NULL_TREE, new_phi_def);
      create_new_def_for (res, ass, NULL);
      gsi_insert_after (&gsi_tgt, ass, GSI_NEW_STMT);
    }

  graphite_copy_stmts_from_block (bb, new_bb, iv_map);

  /* Insert out-of SSA copies on the original BB outgoing edges.  */
  gsi_tgt = gsi_last_bb (new_bb);
  basic_block bb_for_succs = bb;
  if (bb_for_succs == bb_for_succs->loop_father->latch
      && bb_in_sese_p (bb_for_succs, region->region)
      && sese_trivially_empty_bb_p (bb_for_succs))
    bb_for_succs = NULL;
  while (bb_for_succs)
    {
      basic_block latch = NULL;
      edge_iterator ei;
      edge e;
      FOR_EACH_EDGE (e, ei, bb_for_succs->succs)
	{
	  for (gphi_iterator psi = gsi_start_phis (e->dest); !gsi_end_p (psi);
	       gsi_next (&psi))
	    {
	      gphi *phi = psi.phi ();
	      tree res = gimple_phi_result (phi);
	      if (virtual_operand_p (res)
		  || scev_analyzable_p (res, region->region))
		continue;

	      tree new_phi_def;
	      tree *rename = region->rename_map->get (res);
	      if (! rename)
		{
		  new_phi_def = create_tmp_reg (TREE_TYPE (res));
		  set_rename (res, new_phi_def);
		}
	      else
		new_phi_def = *rename;

	      tree arg = PHI_ARG_DEF_FROM_EDGE (phi, e);
	      if (TREE_CODE (arg) == SSA_NAME
		  && scev_analyzable_p (arg, region->region))
		{
		  gimple_seq stmts = NULL;
		  tree new_name = get_rename_from_scev (arg, &stmts,
							bb->loop_father,
							iv_map);
		  if (! codegen_error_p ())
		    gsi_insert_earliest (stmts);
		  arg = new_name;
		}
	      gassign *ass = gimple_build_assign (new_phi_def, arg);
	      gsi_insert_after (&gsi_tgt, ass, GSI_NEW_STMT);
	    }
	  if (e->dest == bb_for_succs->loop_father->latch
	      && bb_in_sese_p (e->dest, region->region)
	      && sese_trivially_empty_bb_p (e->dest))
	    latch = e->dest;
	}
      bb_for_succs = latch;
    }

  return single_succ_edge (new_bb);
}

/* Add isl's parameter identifiers and corresponding trees to ivs_params.  */

void translate_isl_ast_to_gimple::
add_parameters_to_ivs_params (scop_p scop, ivs_params &ip)
{
  sese_info_p region = scop->scop_info;
  unsigned nb_parameters = isl_set_dim (scop->param_context, isl_dim_param);
  gcc_assert (nb_parameters == sese_nb_params (region));
  unsigned i;
  tree param;
  FOR_EACH_VEC_ELT (region->params, i, param)
    {
      isl_id *tmp_id = isl_set_get_dim_id (scop->param_context,
					   isl_dim_param, i);
      ip[tmp_id] = param;
    }
}


/* Generates a build, which specifies the constraints on the parameters.  */

__isl_give isl_ast_build *translate_isl_ast_to_gimple::
generate_isl_context (scop_p scop)
{
  isl_set *context_isl = isl_set_params (isl_set_copy (scop->param_context));
  return isl_ast_build_from_context (context_isl);
}

/* This method is executed before the construction of a for node.  */
__isl_give isl_id *
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

/* Generate isl AST from schedule of SCOP.  */

__isl_give isl_ast_node *translate_isl_ast_to_gimple::
scop_to_isl_ast (scop_p scop)
{
  int old_err = isl_options_get_on_error (scop->isl_context);
  int old_max_operations = isl_ctx_get_max_operations (scop->isl_context);
  int max_operations = PARAM_VALUE (PARAM_MAX_ISL_OPERATIONS);
  if (max_operations)
    isl_ctx_set_max_operations (scop->isl_context, max_operations);
  isl_options_set_on_error (scop->isl_context, ISL_ON_ERROR_CONTINUE);

  gcc_assert (scop->transformed_schedule);

  /* Set the separate option to reduce control flow overhead.  */
  isl_schedule *schedule = isl_schedule_map_schedule_node_bottom_up
    (isl_schedule_copy (scop->transformed_schedule), set_separate_option, NULL);
  isl_ast_build *context_isl = generate_isl_context (scop);

  if (flag_loop_parallelize_all)
    {
      scop_get_dependences (scop);
      context_isl =
	isl_ast_build_set_before_each_for (context_isl, ast_build_before_for,
					   scop->dependence);
    }

  isl_ast_node *ast_isl = isl_ast_build_node_from_schedule
    (context_isl, schedule);
  isl_ast_build_free (context_isl);

  isl_options_set_on_error (scop->isl_context, old_err);
  isl_ctx_reset_operations (scop->isl_context);
  isl_ctx_set_max_operations (scop->isl_context, old_max_operations);
  if (isl_ctx_last_error (scop->isl_context) != isl_error_none)
    {
      dump_user_location_t loc = find_loop_location
	(scop->scop_info->region.entry->dest->loop_father);
      if (isl_ctx_last_error (scop->isl_context) == isl_error_quota)
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, loc,
			 "loop nest not optimized, AST generation timed out "
			 "after %d operations [--param max-isl-operations]\n",
			 max_operations);
      else
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, loc,
			 "loop nest not optimized, ISL AST generation "
			 "signalled an error\n");
      isl_ast_node_free (ast_isl);
      return NULL;
    }

  return ast_isl;
}

/* Generate out-of-SSA copies for the entry edge FALSE_ENTRY/TRUE_ENTRY
   in REGION.  */

static void
generate_entry_out_of_ssa_copies (edge false_entry,
				  edge true_entry,
				  sese_info_p region)
{
  gimple_stmt_iterator gsi_tgt = gsi_start_bb (true_entry->dest);
  for (gphi_iterator psi = gsi_start_phis (false_entry->dest);
       !gsi_end_p (psi); gsi_next (&psi))
    {
      gphi *phi = psi.phi ();
      tree res = gimple_phi_result (phi);
      if (virtual_operand_p (res))
	continue;
      /* When there's no out-of-SSA var registered do not bother
         to create one.  */
      tree *rename = region->rename_map->get (res);
      if (! rename)
	continue;
      tree new_phi_def = *rename;
      gassign *ass = gimple_build_assign (new_phi_def,
					  PHI_ARG_DEF_FROM_EDGE (phi,
								 false_entry));
      gsi_insert_after (&gsi_tgt, ass, GSI_NEW_STMT);
    }
}

/* GIMPLE Loop Generator: generates loops in GIMPLE form for the given SCOP.
   Return true if code generation succeeded.  */

bool
graphite_regenerate_ast_isl (scop_p scop)
{
  sese_info_p region = scop->scop_info;
  translate_isl_ast_to_gimple t (region);

  ifsese if_region = NULL;
  isl_ast_node *root_node;
  ivs_params ip;

  timevar_push (TV_GRAPHITE_CODE_GEN);
  t.add_parameters_to_ivs_params (scop, ip);
  root_node = t.scop_to_isl_ast (scop);
  if (! root_node)
    {
      ivs_params_clear (ip);
      timevar_pop (TV_GRAPHITE_CODE_GEN);
      return false;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "[scheduler] original schedule:\n");
      print_isl_schedule (dump_file, scop->original_schedule);
      fprintf (dump_file, "[scheduler] isl transformed schedule:\n");
      print_isl_schedule (dump_file, scop->transformed_schedule);

      fprintf (dump_file, "[scheduler] original ast:\n");
      print_schedule_ast (dump_file, scop->original_schedule, scop);
      fprintf (dump_file, "[scheduler] AST generated by isl:\n");
      print_isl_ast (dump_file, root_node);
    }

  if_region = move_sese_in_condition (region);
  region->if_region = if_region;

  loop_p context_loop = region->region.entry->src->loop_father;
  edge e = single_succ_edge (if_region->true_region->region.entry->dest);
  basic_block bb = split_edge (e);

  /* Update the true_region exit edge.  */
  region->if_region->true_region->region.exit = single_succ_edge (bb);

  t.translate_isl_ast (context_loop, root_node, e, ip);
  if (! t.codegen_error_p ())
    {
      generate_entry_out_of_ssa_copies (if_region->false_region->region.entry,
					if_region->true_region->region.entry,
					region);
      sese_insert_phis_for_liveouts (region,
				     if_region->region->region.exit->src,
				     if_region->false_region->region.exit,
				     if_region->true_region->region.exit);
      if (dump_file)
	fprintf (dump_file, "[codegen] isl AST to Gimple succeeded.\n");
    }

  if (t.codegen_error_p ())
    {
      dump_user_location_t loc = find_loop_location
	(scop->scop_info->region.entry->dest->loop_father);
      dump_printf_loc (MSG_MISSED_OPTIMIZATION, loc,
		       "loop nest not optimized, code generation error\n");

      /* Remove the unreachable region.  */
      remove_edge_and_dominated_blocks (if_region->true_region->region.entry);
      basic_block ifb = if_region->false_region->region.entry->src;
      gimple_stmt_iterator gsi = gsi_last_bb (ifb);
      gsi_remove (&gsi, true);
      if_region->false_region->region.entry->flags &= ~EDGE_FALSE_VALUE;
      if_region->false_region->region.entry->flags |= EDGE_FALLTHRU;
      /* remove_edge_and_dominated_blocks marks loops for removal but
	 doesn't actually remove them (fix that...).  */
      loop_p loop;
      FOR_EACH_LOOP (loop, LI_FROM_INNERMOST)
	if (! loop->header)
	  delete_loop (loop);
    }

  /* We are delaying SSA update to after code-generating all SCOPs.
     This is because we analyzed DRs and parameters on the unmodified
     IL and thus rely on SSA update to pick up new dominating definitions
     from for example SESE liveout PHIs.  This is also for efficiency
     as SSA update does work depending on the size of the function.  */

  free (if_region->true_region);
  free (if_region->region);
  free (if_region);

  ivs_params_clear (ip);
  isl_ast_node_free (root_node);
  timevar_pop (TV_GRAPHITE_CODE_GEN);

  return !t.codegen_error_p ();
}

#endif  /* HAVE_isl */
