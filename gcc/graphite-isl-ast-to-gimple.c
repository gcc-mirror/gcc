/* Translation of isl AST to Gimple.
   Copyright (C) 2014-2017 Free Software Foundation, Inc.
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
#include "graphite.h"

/* We always try to use signed 128 bit types, but fall back to smaller types
   in case a platform does not provide types of these sizes. In the future we
   should use isl to derive the optimal type for each subexpression.  */

static int max_mode_int_precision =
  GET_MODE_PRECISION (int_mode_for_size (MAX_FIXED_MODE_SIZE, 0).require ());
static int graphite_expression_type_precision = 128 <= max_mode_int_precision ?
						128 : max_mode_int_precision;

struct ast_build_info
{
  ast_build_info()
    : is_parallelizable(false)
  { }
  bool is_parallelizable;
};

/* Verifies properties that GRAPHITE should maintain during translation.  */

static inline void
graphite_verify (void)
{
  checking_verify_loop_structure ();
  checking_verify_loop_closed_ssa (true);
}

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
  translate_isl_ast_to_gimple (sese_info_p r)
    : region (r), codegen_error (false) { }
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
  void translate_pending_phi_nodes (void);
  void add_parameters_to_ivs_params (scop_p scop, ivs_params &ip);
  __isl_give isl_ast_build *generate_isl_context (scop_p scop);

  __isl_give isl_ast_node * scop_to_isl_ast (scop_p scop);

  bool is_valid_rename (tree rename, basic_block def_bb, basic_block use_bb,
			phi_node_kind, tree old_name, basic_block old_bb) const;
  tree get_rename (basic_block new_bb, tree old_name,
		   basic_block old_bb, phi_node_kind) const;
  tree get_rename_from_scev (tree old_name, gimple_seq *stmts, loop_p loop,
			     basic_block new_bb, basic_block old_bb,
			     vec<tree> iv_map);
  basic_block get_def_bb_for_const (basic_block bb, basic_block old_bb) const;
  tree get_new_name (basic_block new_bb, tree op,
		     basic_block old_bb, phi_node_kind) const;
  void collect_all_ssa_names (tree new_expr, vec<tree> *vec_ssa);
  bool copy_loop_phi_args (gphi *old_phi, init_back_edge_pair_t &ibp_old_bb,
			   gphi *new_phi, init_back_edge_pair_t &ibp_new_bb,
			   bool postpone);
  bool copy_loop_phi_nodes (basic_block bb, basic_block new_bb);
  bool add_close_phis_to_merge_points (gphi *old_phi, gphi *new_phi,
				       tree default_value);
  tree add_close_phis_to_outer_loops (tree last_merge_name, edge merge_e,
				      gimple *old_close_phi);
  bool copy_loop_close_phi_args (basic_block old_bb, basic_block new_bb,
				 vec<tree> iv_map, bool postpone);
  bool copy_loop_close_phi_nodes (basic_block old_bb, basic_block new_bb,
				  vec<tree> iv_map);
  bool copy_cond_phi_args (gphi *phi, gphi *new_phi, vec<tree> iv_map,
			   bool postpone);
  bool copy_cond_phi_nodes (basic_block bb, basic_block new_bb,
			    vec<tree> iv_map);
  bool graphite_copy_stmts_from_block (basic_block bb, basic_block new_bb,
				       vec<tree> iv_map);
  edge copy_bb_and_scalar_dependences (basic_block bb, edge next_e,
				       vec<tree> iv_map);
  edge edge_for_new_close_phis (basic_block bb);
  bool add_phi_arg_for_new_expr (tree old_phi_args[2], tree new_phi_args[2],
				 edge old_bb_dominating_edge,
				 edge old_bb_non_dominating_edge,
				 gphi *phi, gphi *new_phi,
				 basic_block new_bb);
  bool rename_uses (gimple *copy, gimple_stmt_iterator *gsi_tgt,
		    basic_block old_bb, loop_p loop, vec<tree> iv_map);
  void set_rename (tree old_name, tree expr);
  void set_rename_for_each_def (gimple *stmt);
  void gsi_insert_earliest (gimple_seq seq);
  tree rename_all_uses (tree new_expr, basic_block new_bb, basic_block old_bb);
  bool codegen_error_p () const { return codegen_error; }
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
};

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
  tree *val = region->parameter_rename_map->get(t);

  if (!val)
   val = &t;
  return fold_convert (type, *val);
}

/* Converts an isl_ast_expr_int expression E to a GCC expression tree of
   type TYPE.  */

tree translate_isl_ast_to_gimple::
gcc_expression_from_isl_expr_int (tree type, __isl_take isl_ast_expr *expr)
{
  gcc_assert (isl_ast_expr_get_type (expr) == isl_ast_expr_int);
  isl_val *val = isl_ast_expr_get_val (expr);
  size_t n = isl_val_n_abs_num_chunks (val, sizeof (HOST_WIDE_INT));
  HOST_WIDE_INT *chunks = XALLOCAVEC (HOST_WIDE_INT, n);
  tree res;
  if (isl_val_get_abs_num_chunks (val, sizeof (HOST_WIDE_INT), chunks) == -1)
    res = NULL_TREE;
  else
    {
      widest_int wi = widest_int::from_array (chunks, n, true);
      if (isl_val_is_neg (val))
	wi = -wi;
      res = wide_int_to_tree (type, wi);
    }
  isl_val_free (val);
  isl_ast_expr_free (expr);
  return res;
}

/* Converts a binary isl_ast_expr_op expression E to a GCC expression tree of
   type TYPE.  */

tree translate_isl_ast_to_gimple::
binary_op_to_tree (tree type, __isl_take isl_ast_expr *expr, ivs_params &ip)
{
  isl_ast_expr *arg_expr = isl_ast_expr_get_op_arg (expr, 0);
  tree tree_lhs_expr = gcc_expression_from_isl_expression (type, arg_expr, ip);
  arg_expr = isl_ast_expr_get_op_arg (expr, 1);
  tree tree_rhs_expr = gcc_expression_from_isl_expression (type, arg_expr, ip);

  enum isl_ast_op_type expr_type = isl_ast_expr_get_op_type (expr);
  isl_ast_expr_free (expr);

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
      /* As isl operates on arbitrary precision numbers, we may end up with
	 division by 2^64 that is folded to 0.  */
      if (integer_zerop (tree_rhs_expr))
	{
	  codegen_error = true;
	  return NULL_TREE;
	}
      return fold_build2 (EXACT_DIV_EXPR, type, tree_lhs_expr, tree_rhs_expr);

    case isl_ast_op_pdiv_q:
      /* As isl operates on arbitrary precision numbers, we may end up with
	 division by 2^64 that is folded to 0.  */
      if (integer_zerop (tree_rhs_expr))
	{
	  codegen_error = true;
	  return NULL_TREE;
	}
      return fold_build2 (TRUNC_DIV_EXPR, type, tree_lhs_expr, tree_rhs_expr);

    case isl_ast_op_zdiv_r:
    case isl_ast_op_pdiv_r:
      /* As isl operates on arbitrary precision numbers, we may end up with
	 division by 2^64 that is folded to 0.  */
      if (integer_zerop (tree_rhs_expr))
	{
	  codegen_error = true;
	  return NULL_TREE;
	}
      return fold_build2 (TRUNC_MOD_EXPR, type, tree_lhs_expr, tree_rhs_expr);

    case isl_ast_op_fdiv_q:
      /* As isl operates on arbitrary precision numbers, we may end up with
	 division by 2^64 that is folded to 0.  */
      if (integer_zerop (tree_rhs_expr))
	{
	  codegen_error = true;
	  return NULL_TREE;
	}
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
  tree type
    = build_nonstandard_integer_type (graphite_expression_type_precision, 0);

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
      tree type =
	build_nonstandard_integer_type (graphite_expression_type_precision, 0);
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
  if (dump_file)
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

  if (dump_file)
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
  tree type =
    build_nonstandard_integer_type (graphite_expression_type_precision, 0);
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

/* Return true when BB contains loop close phi nodes.  A loop close phi node is
   at the exit of loop which takes one argument that is the last value of the
   variable being used out of the loop.  */

static bool
bb_contains_loop_close_phi_nodes (basic_block bb)
{
  return single_pred_p (bb)
    && bb->loop_father != single_pred_edge (bb)->src->loop_father;
}

/* Return true when BB contains loop phi nodes.  A loop phi node is the loop
   header containing phi nodes which has one init-edge and one back-edge.  */

static bool
bb_contains_loop_phi_nodes (basic_block bb)
{
  if (EDGE_COUNT (bb->preds) != 2)
    return false;

  unsigned depth = loop_depth (bb->loop_father);

  edge preds[2] = { (*bb->preds)[0], (*bb->preds)[1] };

  if (depth > loop_depth (preds[0]->src->loop_father)
      || depth > loop_depth (preds[1]->src->loop_father))
    return true;

  /* When one of the edges correspond to the same loop father and other
     doesn't.  */
  if (bb->loop_father != preds[0]->src->loop_father
      && bb->loop_father == preds[1]->src->loop_father)
    return true;

  if (bb->loop_father != preds[1]->src->loop_father
      && bb->loop_father == preds[0]->src->loop_father)
    return true;

  return false;
}

/* Check if USE is defined in a basic block from where the definition of USE can
   propagate from all the paths.  FIXME: Verify checks for virtual operands.  */

static bool
is_loop_closed_ssa_use (basic_block bb, tree use)
{
  if (TREE_CODE (use) != SSA_NAME || virtual_operand_p (use))
    return true;

  /* For close-phi nodes def always comes from a loop which has a back-edge.  */
  if (bb_contains_loop_close_phi_nodes (bb))
    return true;

  gimple *def = SSA_NAME_DEF_STMT (use);
  basic_block def_bb = gimple_bb (def);
  return (!def_bb
	  || flow_bb_inside_loop_p (def_bb->loop_father, bb));
}

/* Return the number of phi nodes in BB.  */

static int
number_of_phi_nodes (basic_block bb)
{
  int num_phis = 0;
  for (gphi_iterator psi = gsi_start_phis (bb); !gsi_end_p (psi);
       gsi_next (&psi))
    num_phis++;
  return num_phis;
}

/* Returns true if BB uses name in one of its PHIs.  */

static bool
phi_uses_name (basic_block bb, tree name)
{
  for (gphi_iterator psi = gsi_start_phis (bb); !gsi_end_p (psi);
       gsi_next (&psi))
    {
      gphi *phi = psi.phi ();
      for (unsigned i = 0; i < gimple_phi_num_args (phi); i++)
	{
	  tree use_arg = gimple_phi_arg_def (phi, i);
	  if (use_arg == name)
	    return true;
	}
    }
  return false;
}

/* Return true if RENAME (defined in BB) is a valid use in NEW_BB.  The
   definition should flow into use, and the use should respect the loop-closed
   SSA form.  */

bool translate_isl_ast_to_gimple::
is_valid_rename (tree rename, basic_block def_bb, basic_block use_bb,
		 phi_node_kind phi_kind, tree old_name, basic_block old_bb) const
{
  if (SSA_NAME_IS_DEFAULT_DEF (rename))
    return true;

  /* The def of the rename must either dominate the uses or come from a
     back-edge.  Also the def must respect the loop closed ssa form.  */
  if (!is_loop_closed_ssa_use (use_bb, rename))
    {
      if (dump_file)
	{
	  fprintf (dump_file, "[codegen] rename not in loop closed ssa: ");
	  print_generic_expr (dump_file, rename);
	  fprintf (dump_file, "\n");
	}
      return false;
    }

  if (dominated_by_p (CDI_DOMINATORS, use_bb, def_bb))
    return true;

  if (bb_contains_loop_phi_nodes (use_bb) && phi_kind == loop_phi)
    {
      /* The loop-header dominates the loop-body.  */
      if (!dominated_by_p (CDI_DOMINATORS, def_bb, use_bb))
	return false;

      /* RENAME would be used in loop-phi.  */
      gcc_assert (number_of_phi_nodes (use_bb));

      /* For definitions coming from back edges, we should check that
	 old_name is used in a loop PHI node.
	 FIXME: Verify if this is true.  */
      if (phi_uses_name (old_bb, old_name))
	return true;
    }
  return false;
}

/* Returns the expression associated to OLD_NAME (which is used in OLD_BB), in
   NEW_BB from RENAME_MAP.  PHI_KIND determines the kind of phi node.  */

tree translate_isl_ast_to_gimple::
get_rename (basic_block new_bb, tree old_name, basic_block old_bb,
	    phi_node_kind phi_kind) const
{
  gcc_assert (TREE_CODE (old_name) == SSA_NAME);
  vec <tree> *renames = region->rename_map->get (old_name);

  if (!renames || renames->is_empty ())
    return NULL_TREE;

  if (1 == renames->length ())
    {
      tree rename = (*renames)[0];
      if (TREE_CODE (rename) == SSA_NAME)
	{
	  basic_block bb = gimple_bb (SSA_NAME_DEF_STMT (rename));
	  if (is_valid_rename (rename, bb, new_bb, phi_kind, old_name, old_bb)
	      && (phi_kind == close_phi
		  || ! bb
		  || flow_bb_inside_loop_p (bb->loop_father, new_bb)))
	    return rename;
	  return NULL_TREE;
	}

      if (is_constant (rename))
	return rename;

      return NULL_TREE;
    }

  /* More than one renames corresponding to the old_name.  Find the rename for
     which the definition flows into usage at new_bb.  */
  int i;
  tree t1 = NULL_TREE, t2;
  basic_block t1_bb = NULL;
  FOR_EACH_VEC_ELT (*renames, i, t2)
    {
      basic_block t2_bb = gimple_bb (SSA_NAME_DEF_STMT (t2));

      /* Defined in the same basic block as used.  */
      if (t2_bb == new_bb)
	return t2;

      /* NEW_BB and T2_BB are in two unrelated if-clauses.  */
      if (!dominated_by_p (CDI_DOMINATORS, new_bb, t2_bb))
	continue;

      if (!flow_bb_inside_loop_p (t2_bb->loop_father, new_bb))
	continue;

      /* Compute the nearest dominator.  */
      if (!t1 || dominated_by_p (CDI_DOMINATORS, t2_bb, t1_bb))
	{
	  t1_bb = t2_bb;
	  t1 = t2;
	}
    }

  return t1;
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
      fprintf (dump_file, ", new_name = ");
      print_generic_expr (dump_file, expr);
      fprintf (dump_file, "\n");
    }

  if (old_name == expr)
    return;

  vec <tree> *renames = region->rename_map->get (old_name);

  if (renames)
    renames->safe_push (expr);
  else
    {
      vec<tree> r;
      r.create (2);
      r.safe_push (expr);
      region->rename_map->put (old_name, r);
    }

  tree t;
  int i;
  /* For a parameter of a scop we don't want to rename it.  */
  FOR_EACH_VEC_ELT (region->params, i, t)
    if (old_name == t)
      region->parameter_rename_map->put(old_name, expr);
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
      gimple_stmt_iterator gsi_def_stmt = gsi_start_bb_nondebug (begin_bb);

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
	    = gsi_start_bb_nondebug (gsi_bb (gsi_def_stmt));
	  /* Insert right after the PHI statements.  */
	  gsi_insert_before (&bsi, use_stmt, GSI_NEW_STMT);
	}
      else
	gsi_insert_after (&gsi_def_stmt, use_stmt, GSI_NEW_STMT);

      if (dump_file)
	{
	  fprintf (dump_file, "[codegen] inserting statement: ");
	  print_gimple_stmt (dump_file, use_stmt, 0, TDF_VOPS | TDF_MEMSYMS);
	  print_loops_bb (dump_file, gimple_bb (use_stmt), 0, 3);
	}
    }
}

/* Collect all the operands of NEW_EXPR by recursively visiting each
   operand.  */

void translate_isl_ast_to_gimple::
collect_all_ssa_names (tree new_expr, vec<tree> *vec_ssa)
{
  if (new_expr == NULL_TREE)
    return;

  /* Rename all uses in new_expr.  */
  if (TREE_CODE (new_expr) == SSA_NAME)
    {
      vec_ssa->safe_push (new_expr);
      return;
    }

  /* Iterate over SSA_NAMES in NEW_EXPR.  */
  for (int i = 0; i < (TREE_CODE_LENGTH (TREE_CODE (new_expr))); i++)
    {
      tree op = TREE_OPERAND (new_expr, i);
      collect_all_ssa_names (op, vec_ssa);
    }
}

/* This is abridged version of the function copied from:
   tree.c:substitute_in_expr (tree exp, tree f, tree r).  */

static tree
substitute_ssa_name (tree exp, tree f, tree r)
{
  enum tree_code code = TREE_CODE (exp);
  tree op0, op1, op2, op3;
  tree new_tree;

  /* We handle TREE_LIST and COMPONENT_REF separately.  */
  if (code == TREE_LIST)
    {
      op0 = substitute_ssa_name (TREE_CHAIN (exp), f, r);
      op1 = substitute_ssa_name (TREE_VALUE (exp), f, r);
      if (op0 == TREE_CHAIN (exp) && op1 == TREE_VALUE (exp))
	return exp;

      return tree_cons (TREE_PURPOSE (exp), op1, op0);
    }
  else if (code == COMPONENT_REF)
    {
      tree inner;

      /* If this expression is getting a value from a PLACEHOLDER_EXPR
	 and it is the right field, replace it with R.  */
      for (inner = TREE_OPERAND (exp, 0);
	   REFERENCE_CLASS_P (inner);
	   inner = TREE_OPERAND (inner, 0))
	;

      /* The field.  */
      op1 = TREE_OPERAND (exp, 1);

      if (TREE_CODE (inner) == PLACEHOLDER_EXPR && op1 == f)
	return r;

      /* If this expression hasn't been completed let, leave it alone.  */
      if (TREE_CODE (inner) == PLACEHOLDER_EXPR && !TREE_TYPE (inner))
	return exp;

      op0 = substitute_ssa_name (TREE_OPERAND (exp, 0), f, r);
      if (op0 == TREE_OPERAND (exp, 0))
	return exp;

      new_tree
	= fold_build3 (COMPONENT_REF, TREE_TYPE (exp), op0, op1, NULL_TREE);
    }
  else
    switch (TREE_CODE_CLASS (code))
      {
      case tcc_constant:
	return exp;

      case tcc_declaration:
	if (exp == f)
	  return r;
	else
	  return exp;

      case tcc_expression:
	if (exp == f)
	  return r;

	/* Fall through.  */

      case tcc_exceptional:
      case tcc_unary:
      case tcc_binary:
      case tcc_comparison:
      case tcc_reference:
	switch (TREE_CODE_LENGTH (code))
	  {
	  case 0:
	    if (exp == f)
	      return r;
	    return exp;

	  case 1:
	    op0 = substitute_ssa_name (TREE_OPERAND (exp, 0), f, r);
	    if (op0 == TREE_OPERAND (exp, 0))
	      return exp;

	    new_tree = fold_build1 (code, TREE_TYPE (exp), op0);
	    break;

	  case 2:
	    op0 = substitute_ssa_name (TREE_OPERAND (exp, 0), f, r);
	    op1 = substitute_ssa_name (TREE_OPERAND (exp, 1), f, r);

	    if (op0 == TREE_OPERAND (exp, 0) && op1 == TREE_OPERAND (exp, 1))
	      return exp;

	    new_tree = fold_build2 (code, TREE_TYPE (exp), op0, op1);
	    break;

	  case 3:
	    op0 = substitute_ssa_name (TREE_OPERAND (exp, 0), f, r);
	    op1 = substitute_ssa_name (TREE_OPERAND (exp, 1), f, r);
	    op2 = substitute_ssa_name (TREE_OPERAND (exp, 2), f, r);

	    if (op0 == TREE_OPERAND (exp, 0) && op1 == TREE_OPERAND (exp, 1)
		&& op2 == TREE_OPERAND (exp, 2))
	      return exp;

	    new_tree = fold_build3 (code, TREE_TYPE (exp), op0, op1, op2);
	    break;

	  case 4:
	    op0 = substitute_ssa_name (TREE_OPERAND (exp, 0), f, r);
	    op1 = substitute_ssa_name (TREE_OPERAND (exp, 1), f, r);
	    op2 = substitute_ssa_name (TREE_OPERAND (exp, 2), f, r);
	    op3 = substitute_ssa_name (TREE_OPERAND (exp, 3), f, r);

	    if (op0 == TREE_OPERAND (exp, 0) && op1 == TREE_OPERAND (exp, 1)
		&& op2 == TREE_OPERAND (exp, 2)
		&& op3 == TREE_OPERAND (exp, 3))
	      return exp;

	    new_tree
	      = fold (build4 (code, TREE_TYPE (exp), op0, op1, op2, op3));
	    break;

	  default:
	    gcc_unreachable ();
	  }
	break;

      case tcc_vl_exp:
      default:
	gcc_unreachable ();
      }

  TREE_READONLY (new_tree) |= TREE_READONLY (exp);

  if (code == INDIRECT_REF || code == ARRAY_REF || code == ARRAY_RANGE_REF)
    TREE_THIS_NOTRAP (new_tree) |= TREE_THIS_NOTRAP (exp);

  return new_tree;
}

/* Rename all the operands of NEW_EXPR by recursively visiting each operand.  */

tree translate_isl_ast_to_gimple::
rename_all_uses (tree new_expr, basic_block new_bb, basic_block old_bb)
{
  auto_vec<tree, 2> ssa_names;
  collect_all_ssa_names (new_expr, &ssa_names);
  tree t;
  int i;
  FOR_EACH_VEC_ELT (ssa_names, i, t)
    if (tree r = get_rename (new_bb, t, old_bb, unknown_phi))
      new_expr = substitute_ssa_name (new_expr, t, r);

  return new_expr;
}

/* For ops which are scev_analyzeable, we can regenerate a new name from its
   scalar evolution around LOOP.  */

tree translate_isl_ast_to_gimple::
get_rename_from_scev (tree old_name, gimple_seq *stmts, loop_p loop,
		      basic_block new_bb, basic_block old_bb,
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
      codegen_error = true;
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
      codegen_error = true;
      return build_zero_cst (TREE_TYPE (old_name));
    }

  if (TREE_CODE (new_expr) == SSA_NAME)
    {
      basic_block bb = gimple_bb (SSA_NAME_DEF_STMT (new_expr));
      if (bb && !dominated_by_p (CDI_DOMINATORS, new_bb, bb))
	{
	  codegen_error = true;
	  return build_zero_cst (TREE_TYPE (old_name));
	}
    }

  new_expr = rename_all_uses (new_expr, new_bb, old_bb);

  /* We check all the operands and all of them should dominate the use at
     new_expr.  */
  auto_vec <tree, 2> new_ssa_names;
  collect_all_ssa_names (new_expr, &new_ssa_names);
  int i;
  tree new_ssa_name;
  FOR_EACH_VEC_ELT (new_ssa_names, i, new_ssa_name)
    {
      if (TREE_CODE (new_ssa_name) == SSA_NAME)
	{
	  basic_block bb = gimple_bb (SSA_NAME_DEF_STMT (new_ssa_name));
	  if (bb && !dominated_by_p (CDI_DOMINATORS, new_bb, bb))
	    {
	      codegen_error = true;
	      return build_zero_cst (TREE_TYPE (old_name));
	    }
	}
    }

  /* Replace the old_name with the new_expr.  */
  return force_gimple_operand (unshare_expr (new_expr), stmts,
			       true, NULL_TREE);
}

/* Renames the scalar uses of the statement COPY, using the
   substitution map RENAME_MAP, inserting the gimplification code at
   GSI_TGT, for the translation REGION, with the original copied
   statement in LOOP, and using the induction variable renaming map
   IV_MAP.  Returns true when something has been renamed.  */

bool translate_isl_ast_to_gimple::
rename_uses (gimple *copy, gimple_stmt_iterator *gsi_tgt, basic_block old_bb,
	     loop_p loop, vec<tree> iv_map)
{
  bool changed = false;

  if (is_gimple_debug (copy))
    {
      if (gimple_debug_bind_p (copy))
	gimple_debug_bind_reset_value (copy);
      else if (gimple_debug_source_bind_p (copy))
	return false;
      else
	gcc_unreachable ();

      return false;
    }

  if (dump_file)
    {
      fprintf (dump_file, "[codegen] renaming uses of stmt: ");
      print_gimple_stmt (dump_file, copy, 0);
    }

  use_operand_p use_p;
  ssa_op_iter op_iter;
  FOR_EACH_SSA_USE_OPERAND (use_p, copy, op_iter, SSA_OP_USE)
    {
      tree old_name = USE_FROM_PTR (use_p);

      if (dump_file)
	{
	  fprintf (dump_file, "[codegen] renaming old_name = ");
	  print_generic_expr (dump_file, old_name);
	  fprintf (dump_file, "\n");
	}

      if (TREE_CODE (old_name) != SSA_NAME
	  || SSA_NAME_IS_DEFAULT_DEF (old_name))
	continue;

      changed = true;
      tree new_expr = get_rename (gsi_tgt->bb, old_name,
				  old_bb, unknown_phi);

      if (new_expr)
	{
	  tree type_old_name = TREE_TYPE (old_name);
	  tree type_new_expr = TREE_TYPE (new_expr);

	  if (dump_file)
	    {
	      fprintf (dump_file, "[codegen] from rename_map: new_name = ");
	      print_generic_expr (dump_file, new_expr);
	      fprintf (dump_file, "\n");
	    }

	  if (type_old_name != type_new_expr
	      || TREE_CODE (new_expr) != SSA_NAME)
	    {
	      tree var = create_tmp_var (type_old_name, "var");

	      if (!useless_type_conversion_p (type_old_name, type_new_expr))
		new_expr = fold_convert (type_old_name, new_expr);

	      gimple_seq stmts;
	      new_expr = force_gimple_operand (new_expr, &stmts, true, var);
	      gsi_insert_earliest (stmts);
	    }

	  replace_exp (use_p, new_expr);
	  continue;
	}

      gimple_seq stmts;
      new_expr = get_rename_from_scev (old_name, &stmts, loop, gimple_bb (copy),
				       old_bb, iv_map);
      if (!new_expr || codegen_error_p ())
	return false;

      if (dump_file)
	{
	  fprintf (dump_file, "[codegen] not in rename map, scev: ");
	  print_generic_expr (dump_file, new_expr);
	  fprintf (dump_file, "\n");
	}

      gsi_insert_earliest (stmts);
      replace_exp (use_p, new_expr);

      if (TREE_CODE (new_expr) == INTEGER_CST
	  && is_gimple_assign (copy))
	{
	  tree rhs = gimple_assign_rhs1 (copy);

	  if (TREE_CODE (rhs) == ADDR_EXPR)
	    recompute_tree_invariant_for_addr_expr (rhs);
	}

      set_rename (old_name, new_expr);
    }

  return changed;
}

/* Returns a basic block that could correspond to where a constant was defined
   in the original code.  In the original code OLD_BB had the definition, we
   need to find which basic block out of the copies of old_bb, in the new
   region, should a definition correspond to if it has to reach BB.  */

basic_block translate_isl_ast_to_gimple::
get_def_bb_for_const (basic_block bb, basic_block old_bb) const
{
  vec <basic_block> *bbs = region->copied_bb_map->get (old_bb);

  if (!bbs || bbs->is_empty ())
    return NULL;

  if (1 == bbs->length ())
    return (*bbs)[0];

  int i;
  basic_block b1 = NULL, b2;
  FOR_EACH_VEC_ELT (*bbs, i, b2)
    {
      if (b2 == bb)
	return bb;

      /* BB and B2 are in two unrelated if-clauses.  */
      if (!dominated_by_p (CDI_DOMINATORS, bb, b2))
	continue;

      /* Compute the nearest dominator.  */
      if (!b1 || dominated_by_p (CDI_DOMINATORS, b2, b1))
	b1 = b2;
    }

  return b1;
}

/* Get the new name of OP (from OLD_BB) to be used in NEW_BB.  PHI_KIND
   determines the kind of phi node.  */

tree translate_isl_ast_to_gimple::
get_new_name (basic_block new_bb, tree op,
	      basic_block old_bb, phi_node_kind phi_kind) const
{
  /* For constants the names are the same.  */
  if (TREE_CODE (op) != SSA_NAME)
    return op;

  return get_rename (new_bb, op, old_bb, phi_kind);
}

/* Return a debug location for OP.  */

static location_t
get_loc (tree op)
{
  location_t loc = UNKNOWN_LOCATION;

  if (TREE_CODE (op) == SSA_NAME)
    loc = gimple_location (SSA_NAME_DEF_STMT (op));
  return loc;
}

/* Returns the incoming edges of basic_block BB in the pair.  The first edge is
   the init edge (from outside the loop) and the second one is the back edge
   from the same loop.  */

std::pair<edge, edge>
get_edges (basic_block bb)
{
  std::pair<edge, edge> edges;
  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, bb->preds)
    if (bb->loop_father != e->src->loop_father)
      edges.first = e;
    else
      edges.second = e;
  return edges;
}

/* Copy the PHI arguments from OLD_PHI to the NEW_PHI.  The arguments to NEW_PHI
   must be found unless they can be POSTPONEd for later.  */

bool translate_isl_ast_to_gimple::
copy_loop_phi_args (gphi *old_phi, init_back_edge_pair_t &ibp_old_bb,
		    gphi *new_phi, init_back_edge_pair_t &ibp_new_bb,
		    bool postpone)
{
  gcc_assert (gimple_phi_num_args (old_phi) == gimple_phi_num_args (new_phi));

  basic_block new_bb = gimple_bb (new_phi);
  for (unsigned i = 0; i < gimple_phi_num_args (old_phi); i++)
    {
      edge e;
      if (gimple_phi_arg_edge (old_phi, i) == ibp_old_bb.first)
	e = ibp_new_bb.first;
      else
	e = ibp_new_bb.second;

      tree old_name = gimple_phi_arg_def (old_phi, i);
      tree new_name = get_new_name (new_bb, old_name,
				    gimple_bb (old_phi), loop_phi);
      if (new_name)
	{
	  add_phi_arg (new_phi, new_name, e, get_loc (old_name));
	  continue;
	}

      gimple *old_def_stmt = SSA_NAME_DEF_STMT (old_name);
      if (!old_def_stmt || gimple_code (old_def_stmt) == GIMPLE_NOP)
	/* If the phi arg was a function arg, or wasn't defined, just use the
	   old name.  */
	add_phi_arg (new_phi, old_name, e, get_loc (old_name));
      else if (postpone)
	{
	  /* Postpone code gen for later for those back-edges we don't have the
	     names yet.  */
	  region->incomplete_phis.safe_push (std::make_pair (old_phi, new_phi));
	  if (dump_file)
	    fprintf (dump_file, "[codegen] postpone loop phi nodes.\n");
	}
      else
	/* Either we should add the arg to phi or, we should postpone.  */
	return false;
    }
  return true;
}

/* Copy loop phi nodes from BB to NEW_BB.  */

bool translate_isl_ast_to_gimple::
copy_loop_phi_nodes (basic_block bb, basic_block new_bb)
{
  if (dump_file)
    fprintf (dump_file, "[codegen] copying loop phi nodes in bb_%d.\n",
	     new_bb->index);

  /* Loop phi nodes should have only two arguments.  */
  gcc_assert (2 == EDGE_COUNT (bb->preds));

  /* First edge is the init edge and second is the back edge.  */
  init_back_edge_pair_t ibp_old_bb = get_edges (bb);

  /* First edge is the init edge and second is the back edge.  */
  init_back_edge_pair_t ibp_new_bb = get_edges (new_bb);

  for (gphi_iterator psi = gsi_start_phis (bb); !gsi_end_p (psi);
       gsi_next (&psi))
    {
      gphi *phi = psi.phi ();
      tree res = gimple_phi_result (phi);
      if (virtual_operand_p (res))
	continue;
      if (is_gimple_reg (res) && scev_analyzable_p (res, region->region))
	continue;

      gphi *new_phi = create_phi_node (NULL_TREE, new_bb);
      tree new_res = create_new_def_for (res, new_phi,
					 gimple_phi_result_ptr (new_phi));
      set_rename (res, new_res);
      codegen_error = !copy_loop_phi_args (phi, ibp_old_bb, new_phi,
					   ibp_new_bb, true);
      update_stmt (new_phi);

      if (dump_file)
	{
	  fprintf (dump_file, "[codegen] creating loop-phi node: ");
	  print_gimple_stmt (dump_file, new_phi, 0);
	}
    }

  return true;
}

/* Return the init value of PHI, the value coming from outside the loop.  */

static tree
get_loop_init_value (gphi *phi)
{

  loop_p loop = gimple_bb (phi)->loop_father;

  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, gimple_bb (phi)->preds)
    if (e->src->loop_father != loop)
      return gimple_phi_arg_def (phi, e->dest_idx);

  return NULL_TREE;
}

/* Find the init value (the value which comes from outside the loop), of one of
   the operands of DEF which is defined by a loop phi.  */

static tree
find_init_value (gimple *def)
{
  if (gimple_code (def) == GIMPLE_PHI)
    return get_loop_init_value (as_a <gphi*> (def));

  if (gimple_vuse (def))
    return NULL_TREE;

  ssa_op_iter iter;
  use_operand_p use_p;
  FOR_EACH_SSA_USE_OPERAND (use_p, def, iter, SSA_OP_USE)
    {
      tree use = USE_FROM_PTR (use_p);
      if (TREE_CODE (use) == SSA_NAME)
	{
	  if (tree res = find_init_value (SSA_NAME_DEF_STMT (use)))
	    return res;
	}
    }

  return NULL_TREE;
}

/* Return the init value, the value coming from outside the loop.  */

static tree
find_init_value_close_phi (gphi *phi)
{
  gcc_assert (gimple_phi_num_args (phi) == 1);
  tree use_arg = gimple_phi_arg_def (phi, 0);
  gimple *def = SSA_NAME_DEF_STMT (use_arg);
  return find_init_value (def);
}


tree translate_isl_ast_to_gimple::
add_close_phis_to_outer_loops (tree last_merge_name, edge last_e,
			       gimple *old_close_phi)
{
  sese_l &codegen_region = region->if_region->true_region->region;
  gimple *stmt = SSA_NAME_DEF_STMT (last_merge_name);
  basic_block bb = gimple_bb (stmt);
  if (!bb_in_sese_p (bb, codegen_region))
    return last_merge_name;

  loop_p loop = bb->loop_father;
  if (!loop_in_sese_p (loop, codegen_region))
    return last_merge_name;

  edge e = single_exit (loop);

  if (dominated_by_p (CDI_DOMINATORS, e->dest, last_e->src))
    return last_merge_name;

  tree old_name = gimple_phi_arg_def (old_close_phi, 0);
  tree old_close_phi_name = gimple_phi_result (old_close_phi);

  bb = e->dest;
  if (!bb_contains_loop_close_phi_nodes (bb) || !single_succ_p (bb))
    bb = split_edge (e);

  gphi *close_phi = create_phi_node (NULL_TREE, bb);
  tree res = create_new_def_for (last_merge_name, close_phi,
				 gimple_phi_result_ptr (close_phi));
  set_rename (old_close_phi_name, res);
  add_phi_arg (close_phi, last_merge_name, e, get_loc (old_name));
  last_merge_name = res;

  return add_close_phis_to_outer_loops (last_merge_name, last_e, old_close_phi);
}

/* Add phi nodes to all merge points of all the diamonds enclosing the loop of
   the close phi node PHI.  */

bool translate_isl_ast_to_gimple::
add_close_phis_to_merge_points (gphi *old_close_phi, gphi *new_close_phi,
				tree default_value)
{
  sese_l &codegen_region = region->if_region->true_region->region;
  basic_block default_value_bb = get_entry_bb (codegen_region);
  if (SSA_NAME == TREE_CODE (default_value))
    {
      gimple *stmt = SSA_NAME_DEF_STMT (default_value);
      if (!stmt || gimple_code (stmt) == GIMPLE_NOP)
	return false;
      default_value_bb = gimple_bb (stmt);
    }

  basic_block new_close_phi_bb = gimple_bb (new_close_phi);

  tree old_close_phi_name = gimple_phi_result (old_close_phi);
  tree new_close_phi_name = gimple_phi_result (new_close_phi);
  tree last_merge_name = new_close_phi_name;
  tree old_name = gimple_phi_arg_def (old_close_phi, 0);

  int i;
  edge merge_e;
  FOR_EACH_VEC_ELT_REVERSE (merge_points, i, merge_e)
    {
      basic_block new_merge_bb = merge_e->src;
      if (!dominated_by_p (CDI_DOMINATORS, new_merge_bb, default_value_bb))
	continue;

      last_merge_name = add_close_phis_to_outer_loops (last_merge_name, merge_e,
						       old_close_phi);

      gphi *merge_phi = create_phi_node (NULL_TREE, new_merge_bb);
      tree merge_res = create_new_def_for (old_close_phi_name, merge_phi,
					   gimple_phi_result_ptr (merge_phi));
      set_rename (old_close_phi_name, merge_res);

      edge from_loop = NULL, from_default_value = NULL;
      edge e;
      edge_iterator ei;
      FOR_EACH_EDGE (e, ei, new_merge_bb->preds)
	if (dominated_by_p (CDI_DOMINATORS, e->src, new_close_phi_bb))
	  from_loop = e;
	else
	  from_default_value = e;

      /* Because CDI_POST_DOMINATORS are not updated, we only rely on
	 CDI_DOMINATORS, which may not handle all cases where new_close_phi_bb
	 is contained in another condition.  */
      if (!from_default_value || !from_loop)
	return false;

      add_phi_arg (merge_phi, last_merge_name, from_loop, get_loc (old_name));
      add_phi_arg (merge_phi, default_value, from_default_value, get_loc (old_name));

      if (dump_file)
	{
	  fprintf (dump_file, "[codegen] Adding guard-phi: ");
	  print_gimple_stmt (dump_file, merge_phi, 0);
	}

      update_stmt (merge_phi);
      last_merge_name = merge_res;
    }

  return true;
}

/* Copy all the loop-close phi args from BB to NEW_BB.  */

bool translate_isl_ast_to_gimple::
copy_loop_close_phi_args (basic_block old_bb, basic_block new_bb,
			  vec<tree> iv_map, bool postpone)
{
  for (gphi_iterator psi = gsi_start_phis (old_bb); !gsi_end_p (psi);
       gsi_next (&psi))
    {
      gphi *old_close_phi = psi.phi ();
      tree res = gimple_phi_result (old_close_phi);
      if (virtual_operand_p (res))
	continue;

      gphi *new_close_phi = create_phi_node (NULL_TREE, new_bb);
      tree new_res = create_new_def_for (res, new_close_phi,
					 gimple_phi_result_ptr (new_close_phi));
      set_rename (res, new_res);

      tree old_name = gimple_phi_arg_def (old_close_phi, 0);
      tree new_name;
      if (is_gimple_reg (res) && scev_analyzable_p (res, region->region))
	{
	  gimple_seq stmts;
	  new_name = get_rename_from_scev (old_name, &stmts,
					   old_bb->loop_father,
					   new_bb, old_bb, iv_map);
	  if (! codegen_error_p ())
	    gsi_insert_earliest (stmts);
	}
      else
	new_name = get_new_name (new_bb, old_name, old_bb, close_phi);

      /* Predecessor basic blocks of a loop close phi should have been code
	 generated before.  FIXME: This is fixable by merging PHIs from inner
	 loops as well.  See: gfortran.dg/graphite/interchange-3.f90.  */
      if (!new_name || codegen_error_p ())
	return false;

      add_phi_arg (new_close_phi, new_name, single_pred_edge (new_bb),
		   get_loc (old_name));
      if (dump_file)
	{
	  fprintf (dump_file, "[codegen] Adding loop close phi: ");
	  print_gimple_stmt (dump_file, new_close_phi, 0);
	}

      update_stmt (new_close_phi);

      /* When there is no loop guard around this codegenerated loop, there is no
	 need to collect the close-phi arg.  */
      if (merge_points.is_empty ())
	continue;

      /* Add a PHI in the succ_new_bb for each close phi of the loop.  */
      tree default_value = find_init_value_close_phi (new_close_phi);

      /* A close phi must come from a loop-phi having a default value.  */
      if (!default_value)
	{
	  if (!postpone)
	    return false;

	  region->incomplete_phis.safe_push (std::make_pair (old_close_phi,
							     new_close_phi));
	  if (dump_file)
	    {
	      fprintf (dump_file, "[codegen] postpone close phi nodes: ");
	      print_gimple_stmt (dump_file, new_close_phi, 0);
	    }
	  continue;
	}

      if (!add_close_phis_to_merge_points (old_close_phi, new_close_phi,
					   default_value))
	return false;
    }

  return true;
}

/* Copy loop close phi nodes from BB to NEW_BB.  */

bool translate_isl_ast_to_gimple::
copy_loop_close_phi_nodes (basic_block old_bb, basic_block new_bb,
			   vec<tree> iv_map)
{
  if (dump_file)
    fprintf (dump_file, "[codegen] copying loop close phi nodes in bb_%d.\n",
	     new_bb->index);
  /* Loop close phi nodes should have only one argument.  */
  gcc_assert (1 == EDGE_COUNT (old_bb->preds));

  return copy_loop_close_phi_args (old_bb, new_bb, iv_map, true);
}


/* Add NEW_NAME as the ARGNUM-th arg of NEW_PHI which is in NEW_BB.
   DOMINATING_PRED is the predecessor basic block of OLD_BB which dominates the
   other pred of OLD_BB as well.  If no such basic block exists then it is NULL.
   NON_DOMINATING_PRED is a pred which does not dominate OLD_BB, it cannot be
   NULL.

   Case1: OLD_BB->preds {BB1, BB2} and BB1 does not dominate BB2 and vice versa.
   In this case DOMINATING_PRED = NULL.

   Case2: OLD_BB->preds {BB1, BB2} and BB1 dominates BB2.

   Returns true on successful copy of the args, false otherwise.  */

bool translate_isl_ast_to_gimple::
add_phi_arg_for_new_expr (tree old_phi_args[2], tree new_phi_args[2],
			  edge old_bb_dominating_edge,
			  edge old_bb_non_dominating_edge,
			  gphi *phi, gphi *new_phi,
			  basic_block new_bb)
{
  basic_block def_pred[2] = { NULL, NULL };
  int not_found_bb_index = -1;
  for (int i = 0; i < 2; i++)
    {
      /* If the corresponding def_bb could not be found the entry will be
	 NULL.  */
      if (TREE_CODE (old_phi_args[i]) == INTEGER_CST)
	def_pred[i] = get_def_bb_for_const (new_bb,
					    gimple_phi_arg_edge (phi, i)->src);
      else if (new_phi_args[i] && (TREE_CODE (new_phi_args[i]) == SSA_NAME))
	def_pred[i] = gimple_bb (SSA_NAME_DEF_STMT (new_phi_args[i]));

      if (!def_pred[i])
	{
	  /* When non are available bail out.  */
	  if (not_found_bb_index != -1)
	    return false;
	  not_found_bb_index = i;
	}
    }

  /* Here we are pattern matching on the structure of CFG w.r.t. old one.  */
  if (old_bb_dominating_edge)
    {
      if (not_found_bb_index != -1)
	return false;

      basic_block new_pred1 = (*new_bb->preds)[0]->src;
      basic_block new_pred2 = (*new_bb->preds)[1]->src;
      vec <basic_block> *bbs
	= region->copied_bb_map->get (old_bb_non_dominating_edge->src);

      /* Could not find a mapping.  */
      if (!bbs)
	return false;

      basic_block new_pred = NULL;
      basic_block b;
      int i;
      FOR_EACH_VEC_ELT (*bbs, i, b)
	{
	  if (dominated_by_p (CDI_DOMINATORS, new_pred1, b))
	    {
	      /* FIXME: If we have already found new_pred then we have to
		 disambiguate, bail out for now.  */
	      if (new_pred)
		return false;
	      new_pred = new_pred1;
	    }
	  if (dominated_by_p (CDI_DOMINATORS, new_pred2, b))
	    {
	      /* FIXME: If we have already found new_pred then we have to either
		 it dominates both or we have to disambiguate, bail out.  */
	      if (new_pred)
		return false;
	      new_pred = new_pred2;
	    }
	}

      if (!new_pred)
	return false;

      edge new_non_dominating_edge = find_edge (new_pred, new_bb);
      gcc_assert (new_non_dominating_edge);
      /* FIXME: Validate each args just like in loop-phis.  */
      /* By the process of elimination we first insert insert phi-edge for
	 non-dominating pred which is computed above and then we insert the
	 remaining one.  */
      int inserted_edge = 0;
      for (; inserted_edge < 2; inserted_edge++)
	{
	  edge new_bb_pred_edge = gimple_phi_arg_edge (new_phi, inserted_edge);
	  if (new_non_dominating_edge == new_bb_pred_edge)
	    {
	      add_phi_arg (new_phi, new_phi_args[inserted_edge],
			   new_non_dominating_edge,
			   get_loc (old_phi_args[inserted_edge]));
	      break;
	    }
	}
      if (inserted_edge == 2)
	return false;

      int edge_dominating = inserted_edge == 0 ? 1 : 0;

      edge new_dominating_edge = NULL;
      for (inserted_edge = 0; inserted_edge < 2; inserted_edge++)
	{
	  edge e = gimple_phi_arg_edge (new_phi, inserted_edge);
	  if (e != new_non_dominating_edge)
	    {
	      new_dominating_edge = e;
	      add_phi_arg (new_phi, new_phi_args[edge_dominating],
			   new_dominating_edge,
			   get_loc (old_phi_args[inserted_edge]));
	      break;
	    }
	}
      gcc_assert (new_dominating_edge);
    }
  else
    {
      /* Classic diamond structure: both edges are non-dominating.  We need to
	 find one unique edge then the other can be found be elimination.  If
	 any definition (def_pred) dominates both the preds of new_bb then we
	 bail out.  Entries of def_pred maybe NULL, in that case we must
	 uniquely find pred with help of only one entry.  */
      edge new_e[2] = { NULL, NULL };
      for (int i = 0; i < 2; i++)
	{
	  edge e;
	  edge_iterator ei;
	  FOR_EACH_EDGE (e, ei, new_bb->preds)
	    if (def_pred[i]
		&& dominated_by_p (CDI_DOMINATORS, e->src, def_pred[i]))
	      {
		if (new_e[i])
		  /* We do not know how to handle the case when def_pred
		     dominates more than a predecessor.  */
		  return false;
		new_e[i] = e;
	      }
	}

      gcc_assert (new_e[0] || new_e[1]);

      /* Find the other edge by process of elimination.  */
      if (not_found_bb_index != -1)
	{
	  gcc_assert (!new_e[not_found_bb_index]);
	  int found_bb_index = not_found_bb_index == 1 ? 0 : 1;
	  edge e;
	  edge_iterator ei;
	  FOR_EACH_EDGE (e, ei, new_bb->preds)
	    {
	      if (new_e[found_bb_index] == e)
		continue;
	      new_e[not_found_bb_index] = e;
	    }
	}

      /* Add edges to phi args.  */
      for (int i = 0; i < 2; i++)
	add_phi_arg (new_phi, new_phi_args[i], new_e[i],
		     get_loc (old_phi_args[i]));
    }

  return true;
}

/* Copy the arguments of cond-phi node PHI, to NEW_PHI in the codegenerated
   region.  If postpone is true and it isn't possible to copy any arg of PHI,
   the PHI is added to the REGION->INCOMPLETE_PHIS to be codegenerated later.
   Returns false if the copying was unsuccessful.  */

bool translate_isl_ast_to_gimple::
copy_cond_phi_args (gphi *phi, gphi *new_phi, vec<tree> iv_map, bool postpone)
{
  if (dump_file)
    fprintf (dump_file, "[codegen] copying cond phi args.\n");
  gcc_assert (2 == gimple_phi_num_args (phi));

  basic_block new_bb = gimple_bb (new_phi);
  loop_p loop = gimple_bb (phi)->loop_father;

  basic_block old_bb = gimple_bb (phi);
  edge old_bb_non_dominating_edge = NULL, old_bb_dominating_edge = NULL;

  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, old_bb->preds)
    if (!dominated_by_p (CDI_DOMINATORS, old_bb, e->src))
      old_bb_non_dominating_edge = e;
    else
      old_bb_dominating_edge = e;

  gcc_assert (!dominated_by_p (CDI_DOMINATORS, old_bb,
			       old_bb_non_dominating_edge->src));

  tree new_phi_args[2];
  tree old_phi_args[2];

  for (unsigned i = 0; i < gimple_phi_num_args (phi); i++)
    {
      tree old_name = gimple_phi_arg_def (phi, i);
      tree new_name = get_new_name (new_bb, old_name, old_bb, cond_phi);
      old_phi_args[i] = old_name;
      if (new_name)
	{
	  new_phi_args [i] = new_name;
	  continue;
	}

      /* If the phi-arg was a parameter.  */
      if (vec_find (region->params, old_name) != -1)
	{
	  new_phi_args [i] = old_name;
	  if (dump_file)
	    {
	      fprintf (dump_file,
		       "[codegen] parameter argument to phi, new_expr: ");
	      print_generic_expr (dump_file, new_phi_args[i]);
	      fprintf (dump_file, "\n");
	    }
	  continue;
	}

      gimple *old_def_stmt = SSA_NAME_DEF_STMT (old_name);
      if (!old_def_stmt || gimple_code (old_def_stmt) == GIMPLE_NOP)
	/* FIXME: If the phi arg was a function arg, or wasn't defined, just use
	   the old name.  */
	return false;

      if (postpone)
	{
	  /* If the phi-arg is scev-analyzeable but only in the first stage.  */
	  if (is_gimple_reg (old_name)
	      && scev_analyzable_p (old_name, region->region))
	    {
	      gimple_seq stmts;
	      tree new_expr = get_rename_from_scev (old_name, &stmts, loop,
						    new_bb, old_bb, iv_map);
	      if (codegen_error_p ())
		return false;

	      gcc_assert (new_expr);
	      if (dump_file)
		{
		  fprintf (dump_file,
			   "[codegen] scev analyzeable, new_expr: ");
		  print_generic_expr (dump_file, new_expr);
		  fprintf (dump_file, "\n");
		}
	      gsi_insert_earliest (stmts);
	      new_phi_args[i] = new_expr;
	      continue;
	    }

	  /* Postpone code gen for later for back-edges.  */
	  region->incomplete_phis.safe_push (std::make_pair (phi, new_phi));

	  if (dump_file)
	    {
	      fprintf (dump_file, "[codegen] postpone cond phi nodes: ");
	      print_gimple_stmt (dump_file, new_phi, 0);
	    }

	  new_phi_args [i] = NULL_TREE;
	  continue;
	}
      else
	/* Either we should add the arg to phi or, we should postpone.  */
	return false;
    }

  /* If none of the args have been determined in the first stage then wait until
     later.  */
  if (postpone && !new_phi_args[0] && !new_phi_args[1])
    return true;

  return add_phi_arg_for_new_expr (old_phi_args, new_phi_args,
				   old_bb_dominating_edge,
				   old_bb_non_dominating_edge,
				   phi, new_phi, new_bb);
}

/* Copy cond phi nodes from BB to NEW_BB.  A cond-phi node is a basic block
   containing phi nodes coming from two predecessors, and none of them are back
   edges.  */

bool translate_isl_ast_to_gimple::
copy_cond_phi_nodes (basic_block bb, basic_block new_bb, vec<tree> iv_map)
{

  gcc_assert (!bb_contains_loop_close_phi_nodes (bb));

  /* TODO: Handle cond phi nodes with more than 2 predecessors.  */
  if (EDGE_COUNT (bb->preds) != 2)
    return false;

  if (dump_file)
    fprintf (dump_file, "[codegen] copying cond phi nodes in bb_%d.\n",
	     new_bb->index);

  for (gphi_iterator psi = gsi_start_phis (bb); !gsi_end_p (psi);
       gsi_next (&psi))
    {
      gphi *phi = psi.phi ();
      tree res = gimple_phi_result (phi);
      if (virtual_operand_p (res))
	continue;

      gphi *new_phi = create_phi_node (NULL_TREE, new_bb);
      tree new_res = create_new_def_for (res, new_phi,
					 gimple_phi_result_ptr (new_phi));
      set_rename (res, new_res);

      if (!copy_cond_phi_args (phi, new_phi, iv_map, true))
	return false;

      update_stmt (new_phi);
    }

  return true;
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
      && is_gimple_reg (lhs)
      && scev_analyzable_p (lhs, region->region))
    return false;

  /* Do not copy parameters that have been generated in the header of the
     scop.  */
  if (is_gimple_assign (stmt)
      && (lhs = gimple_assign_lhs (stmt))
      && TREE_CODE (lhs) == SSA_NAME
      && region->parameter_rename_map->get(lhs))
    return false;

  return true;
}

/* Create new names for all the definitions created by COPY and add replacement
   mappings for each new name.  */

void translate_isl_ast_to_gimple::
set_rename_for_each_def (gimple *stmt)
{
  def_operand_p def_p;
  ssa_op_iter op_iter;
  FOR_EACH_SSA_DEF_OPERAND (def_p, stmt, op_iter, SSA_OP_ALL_DEFS)
    {
      tree old_name = DEF_FROM_PTR (def_p);
      tree new_name = create_new_def_for (old_name, stmt, def_p);
      set_rename (old_name, new_name);
    }
}

/* Duplicates the statements of basic block BB into basic block NEW_BB
   and compute the new induction variables according to the IV_MAP.  */

bool translate_isl_ast_to_gimple::
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
      gsi_insert_after (&gsi_tgt, copy, GSI_NEW_STMT);

      if (dump_file)
	{
	  fprintf (dump_file, "[codegen] inserting statement: ");
	  print_gimple_stmt (dump_file, copy, 0);
	}

      maybe_duplicate_eh_stmt (copy, stmt);
      gimple_duplicate_stmt_histograms (cfun, copy, cfun, stmt);

      /* Crete new names for each def in the copied stmt.  */
      set_rename_for_each_def (copy);

      loop_p loop = bb->loop_father;
      if (rename_uses (copy, &gsi_tgt, bb, loop, iv_map))
	{
	  fold_stmt_inplace (&gsi_tgt);
	  gcc_assert (gsi_stmt (gsi_tgt) == copy);
	}

      if (codegen_error_p ())
	return false;

      /* For each SSA_NAME in the parameter_rename_map rename their usage.  */
      ssa_op_iter iter;
      use_operand_p use_p;
      if (!is_gimple_debug (copy))
	FOR_EACH_SSA_USE_OPERAND (use_p, copy, iter, SSA_OP_USE)
	  {
	    tree old_name = USE_FROM_PTR (use_p);

	    if (TREE_CODE (old_name) != SSA_NAME
		|| SSA_NAME_IS_DEFAULT_DEF (old_name))
	      continue;

	    tree *new_expr = region->parameter_rename_map->get (old_name);
	    if (!new_expr)
	      continue;

	    replace_exp (use_p, *new_expr);
	  }

      update_stmt (copy);
    }

  return true;
}


/* Given a basic block containing close-phi it returns the new basic block where
   to insert a copy of the close-phi nodes.  All the uses in close phis should
   come from a single loop otherwise it returns NULL.  */

edge translate_isl_ast_to_gimple::
edge_for_new_close_phis (basic_block bb)
{
  /* Make sure that NEW_BB is the new_loop->exit->dest.  We find the definition
     of close phi in the original code and then find the mapping of basic block
     defining that variable.  If there are multiple close-phis and they are
     defined in different loops (in the original or in the new code) because of
     loop splitting, then we bail out.  */
  loop_p new_loop = NULL;
  for (gphi_iterator psi = gsi_start_phis (bb); !gsi_end_p (psi);
       gsi_next (&psi))
    {
      gphi *phi = psi.phi ();
      tree name = gimple_phi_arg_def (phi, 0);
      basic_block old_loop_bb = gimple_bb (SSA_NAME_DEF_STMT (name));

      vec <basic_block> *bbs = region->copied_bb_map->get (old_loop_bb);
      if (!bbs || bbs->length () != 1)
	/* This is one of the places which shows preserving original structure
	   is not always possible, as we may need to insert close PHI for a loop
	   where the latch does not have any mapping, or the mapping is
	   ambiguous.  */
	return NULL;

      if (!new_loop)
	new_loop = (*bbs)[0]->loop_father;
      else if (new_loop != (*bbs)[0]->loop_father)
	return NULL;
    }

  if (!new_loop)
    return NULL;

  return single_exit (new_loop);
}

/* Copies BB and includes in the copied BB all the statements that can
   be reached following the use-def chains from the memory accesses,
   and returns the next edge following this new block.  */

edge translate_isl_ast_to_gimple::
copy_bb_and_scalar_dependences (basic_block bb, edge next_e, vec<tree> iv_map)
{
  int num_phis = number_of_phi_nodes (bb);

  if (region->copied_bb_map->get (bb))
    {
      /* FIXME: we should be able to handle phi nodes with args coming from
	 outside the region.  */
      if (num_phis)
	{
	  codegen_error = true;
	  return NULL;
	}
    }

  basic_block new_bb = NULL;
  if (bb_contains_loop_close_phi_nodes (bb))
    {
      if (dump_file)
	fprintf (dump_file, "[codegen] bb_%d contains close phi nodes.\n",
		 bb->index);

      edge e = edge_for_new_close_phis (bb);
      if (!e)
	{
	  codegen_error = true;
	  return NULL;
	}

      basic_block phi_bb = e->dest;

      if (!bb_contains_loop_close_phi_nodes (phi_bb) || !single_succ_p (phi_bb))
	phi_bb = split_edge (e);

      gcc_assert (single_pred_edge (phi_bb)->src->loop_father
		  != single_pred_edge (phi_bb)->dest->loop_father);

      if (!copy_loop_close_phi_nodes (bb, phi_bb, iv_map))
	{
	  codegen_error = true;
	  return NULL;
	}

      if (e == next_e)
	new_bb = phi_bb;
      else
	new_bb = split_edge (next_e);
    }
  else
    {
      new_bb = split_edge (next_e);
      if (num_phis > 0 && bb_contains_loop_phi_nodes (bb))
	{
	  basic_block phi_bb = next_e->dest->loop_father->header;

	  /* At this point we are unable to codegenerate by still preserving the SSA
	     structure because maybe the loop is completely unrolled and the PHIs
	     and cross-bb scalar dependencies are untrackable w.r.t. the original
	     code.  See gfortran.dg/graphite/pr29832.f90.  */
	  if (EDGE_COUNT (bb->preds) != EDGE_COUNT (phi_bb->preds))
	    {
	      codegen_error = true;
	      return NULL;
	    }

	  /* In case isl did some loop peeling, like this:

	       S_8(0);
	       for (int c1 = 1; c1 <= 5; c1 += 1) {
	         S_8(c1);
	       }
	       S_8(6);

	     there should be no loop-phi nodes in S_8(0).

	     FIXME: We need to reason about dynamic instances of S_8, i.e., the
	     values of all scalar variables: for the moment we instantiate only
	     SCEV analyzable expressions on the iteration domain, and we need to
	     extend that to reductions that cannot be analyzed by SCEV.  */
	  if (!bb_in_sese_p (phi_bb, region->if_region->true_region->region))
	    {
	      codegen_error = true;
	      return NULL;
	    }

	  if (dump_file)
	    fprintf (dump_file, "[codegen] bb_%d contains loop phi nodes.\n",
		     bb->index);
	  if (!copy_loop_phi_nodes (bb, phi_bb))
	    {
	      codegen_error = true;
	      return NULL;
	    }
	}
      else if (num_phis > 0)
	{
	  if (dump_file)
	    fprintf (dump_file, "[codegen] bb_%d contains cond phi nodes.\n",
		     bb->index);

	  basic_block phi_bb = single_pred (new_bb);
	  loop_p loop_father = new_bb->loop_father;

	  /* Move back until we find the block with two predecessors.  */
	  while (single_pred_p (phi_bb))
	    phi_bb = single_pred_edge (phi_bb)->src;

	  /* If a corresponding merge-point was not found, then abort codegen.  */
	  if (phi_bb->loop_father != loop_father
	      || !bb_in_sese_p (phi_bb, region->if_region->true_region->region)
	      || !copy_cond_phi_nodes (bb, phi_bb, iv_map))
	    {
	      codegen_error = true;
	      return NULL;
	    }
	}
    }

  if (dump_file)
    fprintf (dump_file, "[codegen] copying from bb_%d to bb_%d.\n",
	     bb->index, new_bb->index);

  vec <basic_block> *copied_bbs = region->copied_bb_map->get (bb);
  if (copied_bbs)
    copied_bbs->safe_push (new_bb);
  else
    {
      vec<basic_block> bbs;
      bbs.create (2);
      bbs.safe_push (new_bb);
      region->copied_bb_map->put (bb, bbs);
    }

  if (!graphite_copy_stmts_from_block (bb, new_bb, iv_map))
    {
      codegen_error = true;
      return NULL;
    }

  return single_succ_edge (new_bb);
}

/* Patch the missing arguments of the phi nodes.  */

void translate_isl_ast_to_gimple::
translate_pending_phi_nodes ()
{
  int i;
  phi_rename *rename;
  FOR_EACH_VEC_ELT (region->incomplete_phis, i, rename)
    {
      gphi *old_phi = rename->first;
      gphi *new_phi = rename->second;
      basic_block old_bb = gimple_bb (old_phi);
      basic_block new_bb = gimple_bb (new_phi);

      /* First edge is the init edge and second is the back edge.  */
      init_back_edge_pair_t ibp_old_bb = get_edges (old_bb);
      init_back_edge_pair_t ibp_new_bb = get_edges (new_bb);

      if (dump_file)
	{
	  fprintf (dump_file, "[codegen] translating pending old-phi: ");
	  print_gimple_stmt (dump_file, old_phi, 0);
	}

      auto_vec <tree, 1> iv_map;
      if (bb_contains_loop_phi_nodes (new_bb))
	codegen_error = !copy_loop_phi_args (old_phi, ibp_old_bb, new_phi,
					    ibp_new_bb, false);
      else if (bb_contains_loop_close_phi_nodes (new_bb))
	codegen_error = !copy_loop_close_phi_args (old_bb, new_bb, iv_map, false);
      else
	codegen_error = !copy_cond_phi_args (old_phi, new_phi, iv_map, false);

      if (dump_file)
	{
	  fprintf (dump_file, "[codegen] to new-phi: ");
	  print_gimple_stmt (dump_file, new_phi, 0);
	}
      if (codegen_error_p ())
	return;
    }
}

/* Add isl's parameter identifiers and corresponding trees to ivs_params.  */

void translate_isl_ast_to_gimple::
add_parameters_to_ivs_params (scop_p scop, ivs_params &ip)
{
  sese_info_p region = scop->scop_info;
  unsigned nb_parameters = isl_set_dim (scop->param_context, isl_dim_param);
  gcc_assert (nb_parameters == region->params.length ());
  unsigned i;
  for (i = 0; i < nb_parameters; i++)
    {
      isl_id *tmp_id = isl_set_get_dim_id (scop->param_context,
					   isl_dim_param, i);
      ip[tmp_id] = region->params[i];
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
  return ast_isl;
}

/* Copy def from sese REGION to the newly created TO_REGION. TR is defined by
   DEF_STMT. GSI points to entry basic block of the TO_REGION.  */

static void
copy_def (tree tr, gimple *def_stmt, sese_info_p region, sese_info_p to_region,
	  gimple_stmt_iterator *gsi)
{
  if (!defined_in_sese_p (tr, region->region))
    return;

  ssa_op_iter iter;
  use_operand_p use_p;
  FOR_EACH_SSA_USE_OPERAND (use_p, def_stmt, iter, SSA_OP_USE)
    {
      tree use_tr = USE_FROM_PTR (use_p);

      /* Do not copy parameters that have been generated in the header of the
	 scop.  */
      if (region->parameter_rename_map->get(use_tr))
	continue;

      gimple *def_of_use = SSA_NAME_DEF_STMT (use_tr);
      if (!def_of_use)
	continue;

      copy_def (use_tr, def_of_use, region, to_region, gsi);
    }

  gimple *copy = gimple_copy (def_stmt);
  gsi_insert_after (gsi, copy, GSI_NEW_STMT);

  /* Create new names for all the definitions created by COPY and
     add replacement mappings for each new name.  */
  def_operand_p def_p;
  ssa_op_iter op_iter;
  FOR_EACH_SSA_DEF_OPERAND (def_p, copy, op_iter, SSA_OP_ALL_DEFS)
    {
      tree old_name = DEF_FROM_PTR (def_p);
      tree new_name = create_new_def_for (old_name, copy, def_p);
      region->parameter_rename_map->put(old_name, new_name);
    }

  update_stmt (copy);
}

static void
copy_internal_parameters (sese_info_p region, sese_info_p to_region)
{
  /* For all the parameters which definitino is in the if_region->false_region,
     insert code on true_region (if_region->true_region->entry). */

  int i;
  tree tr;
  gimple_stmt_iterator gsi = gsi_start_bb(to_region->region.entry->dest);

  FOR_EACH_VEC_ELT (region->params, i, tr)
    {
      // If def is not in region.
      gimple *def_stmt = SSA_NAME_DEF_STMT (tr);
      if (def_stmt)
	copy_def (tr, def_stmt, region, to_region, &gsi);
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

  recompute_all_dominators ();
  graphite_verify ();

  if_region = move_sese_in_condition (region);
  region->if_region = if_region;
  recompute_all_dominators ();

  loop_p context_loop = region->region.entry->src->loop_father;

  /* Copy all the parameters which are defined in the region.  */
  copy_internal_parameters(if_region->false_region, if_region->true_region);

  edge e = single_succ_edge (if_region->true_region->region.entry->dest);
  basic_block bb = split_edge (e);

  /* Update the true_region exit edge.  */
  region->if_region->true_region->region.exit = single_succ_edge (bb);

  t.translate_isl_ast (context_loop, root_node, e, ip);
  if (t.codegen_error_p ())
    {
      if (dump_file)
	fprintf (dump_file, "codegen error: "
		 "reverting back to the original code.\n");
      set_ifsese_condition (if_region, integer_zero_node);
    }
  else
    {
      t.translate_pending_phi_nodes ();
      if (!t.codegen_error_p ())
	{
	  sese_insert_phis_for_liveouts (region,
					 if_region->region->region.exit->src,
					 if_region->false_region->region.exit,
					 if_region->true_region->region.exit);
	  mark_virtual_operands_for_renaming (cfun);
	  update_ssa (TODO_update_ssa);


	  graphite_verify ();
	  scev_reset ();
	  recompute_all_dominators ();
	  graphite_verify ();

	  if (dump_file)
	    fprintf (dump_file, "[codegen] isl AST to Gimple succeeded.\n");
	}
      else
	{
	  if (dump_file)
	    fprintf (dump_file, "[codegen] unsuccessful in translating"
		     " pending phis, reverting back to the original code.\n");
	  set_ifsese_condition (if_region, integer_zero_node);
	}
    }

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

      fprintf (dump_file, "%d loops carried no dependency.\n",
	       num_no_dependency);
    }

  return !t.codegen_error_p ();
}

#endif  /* HAVE_isl */
