/* This file is part of the Intel(R) Cilk(TM) Plus support
   It contains routines to handle Array Notation expression
   handling routines in the C++ Compiler.
   Copyright (C) 2013-2014 Free Software Foundation, Inc.
   Contributed by Balaji V. Iyer <balaji.v.iyer@intel.com>,
                  Intel Corporation

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* The Array Notation Transformation Technique:

   An array notation expression has 4 major components:
   1. The array name
   2. Start Index
   3. Number of elements we need to access (we call it length)
   4. Stride

   So, if we have something like A[0:5:2], we are accessing A[0], A[2], A[4],
   A[6] and A[8]. The user is responsible to make sure the access length does
   not step outside the array's size.
   
   In this section, I highlight the overall method on how array notations are
   broken up into C/C++ code.  Almost all the functions follows this step:

   Let's say the user has used the array notation in a statement like this:

   A[St1:Ln:Str1] = B[St2:Ln:Str2] + <NON ARRAY_NOT STMT>

   where St{1,2} = Starting index, Ln = Number of elements we need to access,
   and Str{1,2} = the stride.
   Note: The length of both the array notation expressions must be the same.
   
   The above expression is broken into the following:

   for (Tmp_Var = 0; Tmp_Var < Ln; Tmp_Var++)
     A[St1 + Tmp_Var * Str1] = B[St1 + Tmp_Var * Str2] + <NON_ARRAY_NOT_STMT>;
*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "cp-tree.h"
#include "c-family/c-common.h"
#include "diagnostic.h"
#include "tree-iterator.h"
#include "vec.h"

/* Creates a FOR_STMT with INIT, COND, INCR and BODY as the initializer,
   condition, increment expression and the loop-body, respectively.  */

static void
create_an_loop (tree init, tree cond, tree incr, tree body)
{
  tree for_stmt;

  finish_expr_stmt (init);
  for_stmt = begin_for_stmt (NULL_TREE, NULL_TREE);
  finish_for_init_stmt (for_stmt);
  finish_for_cond (cond, for_stmt, false);
  finish_for_expr (incr, for_stmt);
  finish_expr_stmt (body);
  finish_for_stmt (for_stmt);
}

/* If *VALUE is not a constant integer, then this function replaces it with
   a variable to make it loop invariant for array notations.  */

static inline void
make_triplet_val_inv (tree *value)
{
  if (TREE_CODE (*value) != INTEGER_CST
      && TREE_CODE (*value) != PARM_DECL
      && TREE_CODE (*value) != VAR_DECL)
    *value = get_temp_regvar (ptrdiff_type_node, *value);
}

/* Returns a vector of size RANK that contains an ARRAY_REF.  This vector is
   created using array notation-triplet information stored in AN_INFO. The
   induction var is taken from AN_LOOP_INFO.

   For example: For an array notation A[5:10:2], the vector start will be
   of size 1 holding '5', stride of same size as start but holding the value of
   as 2, and is_vector as true.   Let's assume VAR is 'x'
   This function returns a vector of size 1 with the following data:
   A[5 + (x * 2)] .
*/

static vec<tree, va_gc> *
create_array_refs (location_t loc, vec<vec<an_parts> > an_info,
		   vec<an_loop_parts> an_loop_info, size_t size,  size_t rank)
{
  tree ind_mult, ind_incr;
  vec<tree, va_gc> *array_operand = NULL;

  for (size_t ii = 0; ii < size; ii++)
    if (an_info[ii][0].is_vector)
      {
	tree array_opr = an_info[ii][rank - 1].value;
	for (int s_jj = rank -1; s_jj >= 0; s_jj--)
	  {
	    tree start = cp_fold_convert (ptrdiff_type_node, 
					  an_info[ii][s_jj].start);
	    tree stride = cp_fold_convert (ptrdiff_type_node, 
					   an_info[ii][s_jj].stride);
	    tree var = cp_fold_convert (ptrdiff_type_node, 
					an_loop_info[s_jj].var);

	    ind_mult = build2 (MULT_EXPR, TREE_TYPE (var), var, stride);
	    ind_incr = build2 (PLUS_EXPR, TREE_TYPE (var), start, ind_mult);
	    /* Array [ start_index + (induction_var * stride)]  */
	    array_opr = grok_array_decl	(loc, array_opr, ind_incr, false);
	  }
	vec_safe_push (array_operand, array_opr);
      }
    else
      vec_safe_push (array_operand, integer_one_node);
  return array_operand;
}

/* Populates the INCR and CMP fields in *NODE with the increment
   (of type POSTINCREMENT) and comparison (of TYPE LT_EXPR) expressions, using 
   data from AN_INFO.  */

void
create_cmp_incr (location_t loc, vec <an_loop_parts> *node, size_t rank, 
		 vec<vec<an_parts> > an_info, tsubst_flags_t complain)
{
  for (size_t ii = 0; ii < rank; ii++)
    {
      (*node)[ii].incr = build_x_unary_op (loc, POSTINCREMENT_EXPR, 
					   (*node)[ii].var, complain);
      (*node)[ii].cmp = build_x_binary_op (loc, LT_EXPR, (*node)[ii].var,
					   TREE_CODE ((*node)[ii].var),
					   an_info[0][ii].length,
					   TREE_CODE (an_info[0][ii].length),
					   NULL, complain);
    }
}

/* Replaces all the scalar expressions in *NODE.  Returns a STATEMENT LIST that
   holds the NODE along with the variables that hold the results of the
   invariant expressions.  */

static tree
replace_invariant_exprs (tree *node)
{
  size_t ix = 0;
  tree node_list = NULL_TREE;
  tree t = NULL_TREE, new_var = NULL_TREE;
  struct inv_list data;

  data.list_values = NULL;
  data.replacement = NULL;
  data.additional_tcodes = NULL;
  cp_walk_tree (node, find_inv_trees, (void *) &data, NULL);

  if (vec_safe_length (data.list_values))
    {
      node_list = push_stmt_list ();
      for (ix = 0; vec_safe_iterate (data.list_values, ix, &t); ix++)
	{ 
	  /* Sometimes, when comma_expr has a function call in it, it will
	     typecast it to void.  Find_inv_trees finds those nodes and so
	     if it void type, then don't bother creating a new var to hold 
	     the return value.   */
	  if (VOID_TYPE_P (TREE_TYPE (t)))
	    {
	      finish_expr_stmt (t);
	      new_var = void_node;
	    }
	  else 
	    new_var = get_temp_regvar (TREE_TYPE (t), t); 
	  vec_safe_push (data.replacement, new_var);
	}
      cp_walk_tree (node, replace_inv_trees, (void *) &data, NULL);
      node_list = pop_stmt_list (node_list);
    }
  return node_list;
}

/* Replace array notation's built-in function passed in AN_BUILTIN_FN with
   the appropriate loop and computation (all stored in variable LOOP of type
   tree node).  The output of the function function is always a scalar and that
   result is returned in *NEW_VAR.  *NEW_VAR is NULL_TREE if the function is
   __sec_reduce_mutating.  */

static tree
expand_sec_reduce_builtin (tree an_builtin_fn, tree *new_var)
{
  tree new_var_type = NULL_TREE, func_parm, new_yes_expr, new_no_expr;
  tree array_ind_value = NULL_TREE, new_no_ind, new_yes_ind, new_no_list;
  tree new_yes_list, new_cond_expr, new_expr = NULL_TREE; 
  vec<tree, va_gc> *array_list = NULL, *array_operand = NULL;
  size_t list_size = 0, rank = 0, ii = 0;
  tree  body, an_init, loop_with_init = alloc_stmt_list ();
  tree array_op0, comp_node = NULL_TREE;
  tree call_fn = NULL_TREE, identity_value = NULL_TREE;
  tree init = NULL_TREE, cond_init = NULL_TREE;
  enum tree_code code = NOP_EXPR;
  location_t location = UNKNOWN_LOCATION;
  vec<vec<an_parts> > an_info = vNULL;
  vec<an_loop_parts> an_loop_info = vNULL; 
  enum built_in_function an_type =
    is_cilkplus_reduce_builtin (CALL_EXPR_FN (an_builtin_fn));
  vec <tree, va_gc> *func_args;
  
  if (an_type == BUILT_IN_NONE)
    return NULL_TREE;

  if (an_type != BUILT_IN_CILKPLUS_SEC_REDUCE
      && an_type != BUILT_IN_CILKPLUS_SEC_REDUCE_MUTATING)
    func_parm = CALL_EXPR_ARG (an_builtin_fn, 0);
  else
    {
      call_fn = CALL_EXPR_ARG (an_builtin_fn, 2);

      /* We need to do this because we are "faking" the builtin function types,
	 so the compiler does a bunch of typecasts and this will get rid of
	 all that!  */
      STRIP_NOPS (call_fn);
      if (TREE_CODE (call_fn) != OVERLOAD
	  && TREE_CODE (call_fn) != FUNCTION_DECL)
	call_fn = TREE_OPERAND (call_fn, 0);
      identity_value = CALL_EXPR_ARG (an_builtin_fn, 0);
      func_parm = CALL_EXPR_ARG (an_builtin_fn, 1);
      STRIP_NOPS (identity_value);
    }
  STRIP_NOPS (func_parm);
  
  location = EXPR_LOCATION (an_builtin_fn);
  
  /* Note about using find_rank (): If find_rank returns false, then it must
     have already reported an error, thus we just return an error_mark_node
     without any doing any error emission.  */  
  if (!find_rank (location, an_builtin_fn, an_builtin_fn, true, &rank))
      return error_mark_node;
  if (rank == 0)
    return an_builtin_fn;
  else if (rank > 1 
	   && (an_type == BUILT_IN_CILKPLUS_SEC_REDUCE_MAX_IND
	       || an_type == BUILT_IN_CILKPLUS_SEC_REDUCE_MIN_IND))
    { 
      error_at (location, "__sec_reduce_min_ind or __sec_reduce_max_ind cannot "
		"have arrays with dimension greater than 1");
      return error_mark_node;
    }
  
  extract_array_notation_exprs (func_parm, true, &array_list);
  list_size = vec_safe_length (array_list);
  switch (an_type)
    {
    case BUILT_IN_CILKPLUS_SEC_REDUCE_ADD:
    case BUILT_IN_CILKPLUS_SEC_REDUCE_MUL:
    case BUILT_IN_CILKPLUS_SEC_REDUCE_MAX:
    case BUILT_IN_CILKPLUS_SEC_REDUCE_MIN:
      new_var_type = TREE_TYPE ((*array_list)[0]);
      break;
    case BUILT_IN_CILKPLUS_SEC_REDUCE_ALL_ZERO:
    case BUILT_IN_CILKPLUS_SEC_REDUCE_ANY_ZERO:
    case BUILT_IN_CILKPLUS_SEC_REDUCE_ANY_NONZERO:
    case BUILT_IN_CILKPLUS_SEC_REDUCE_ALL_NONZERO:
      new_var_type = boolean_type_node;
      break;
    case BUILT_IN_CILKPLUS_SEC_REDUCE_MAX_IND:
    case BUILT_IN_CILKPLUS_SEC_REDUCE_MIN_IND:
      new_var_type = size_type_node;
      break;
    case BUILT_IN_CILKPLUS_SEC_REDUCE:
      if (call_fn && identity_value)
	new_var_type = TREE_TYPE ((*array_list)[0]);
      break;
    case BUILT_IN_CILKPLUS_SEC_REDUCE_MUTATING:
      new_var_type = NULL_TREE;
      break;
    default:
      gcc_unreachable ();
    }
    
  if (new_var_type && TREE_CODE (new_var_type) == ARRAY_TYPE)
    new_var_type = TREE_TYPE (new_var_type);
  an_loop_info.safe_grow_cleared (rank);

  an_init = push_stmt_list ();

  /* Assign the array notation components to variable so that they can satisfy
     the exec-once rule.  */
  for (ii = 0; ii < list_size; ii++)
    if (TREE_CODE ((*array_list)[ii]) == ARRAY_NOTATION_REF)
      {
	tree anode = (*array_list)[ii];
	make_triplet_val_inv (&ARRAY_NOTATION_START (anode));
	make_triplet_val_inv (&ARRAY_NOTATION_LENGTH (anode));
	make_triplet_val_inv (&ARRAY_NOTATION_STRIDE (anode));
      }
  cilkplus_extract_an_triplets (array_list, list_size, rank, &an_info);
  for (ii = 0; ii < rank; ii++)
    {
      tree typ = ptrdiff_type_node;

      /* In this place, we are using get_temp_regvar instead of 
	 create_temporary_var if an_type is SEC_REDUCE_MAX/MIN_IND because
	 the array_ind_value depends on this value being initalized to 0.  */
      if (an_type == BUILT_IN_CILKPLUS_SEC_REDUCE_MAX_IND
	  || an_type == BUILT_IN_CILKPLUS_SEC_REDUCE_MIN_IND) 
	an_loop_info[ii].var = get_temp_regvar (typ, build_zero_cst (typ));
      else
	{
	  an_loop_info[ii].var = create_temporary_var (typ);
	  add_decl_expr (an_loop_info[ii].var);
	}
      an_loop_info[ii].ind_init = 
	build_x_modify_expr (location, an_loop_info[ii].var, INIT_EXPR,
			     build_zero_cst (typ), tf_warning_or_error);
    }
  array_operand = create_array_refs (location, an_info, an_loop_info,
				      list_size, rank);
  replace_array_notations (&func_parm, true, array_list, array_operand);
  
  if (!TREE_TYPE (func_parm))      
    TREE_TYPE (func_parm) = TREE_TYPE ((*array_list)[0]);
  
  create_cmp_incr (location, &an_loop_info, rank, an_info, tf_warning_or_error);
  if (an_type == BUILT_IN_CILKPLUS_SEC_REDUCE_MAX_IND
      || an_type == BUILT_IN_CILKPLUS_SEC_REDUCE_MIN_IND) 
    array_ind_value = get_temp_regvar (TREE_TYPE (func_parm), func_parm);

  array_op0 = (*array_operand)[0];
  if (TREE_CODE (array_op0) == INDIRECT_REF)
    array_op0 = TREE_OPERAND (array_op0, 0);
  switch (an_type)
    {
    case BUILT_IN_CILKPLUS_SEC_REDUCE_ADD:
      code = PLUS_EXPR;
      init = build_zero_cst (new_var_type);
      break;
    case BUILT_IN_CILKPLUS_SEC_REDUCE_MUL:
      code = MULT_EXPR;
      init = build_one_cst (new_var_type);
      break;
    case BUILT_IN_CILKPLUS_SEC_REDUCE_ANY_ZERO:
    case BUILT_IN_CILKPLUS_SEC_REDUCE_ANY_NONZERO:
      code = ((an_type == BUILT_IN_CILKPLUS_SEC_REDUCE_ANY_ZERO) ? EQ_EXPR
	: NE_EXPR);
      init = build_zero_cst (new_var_type);
      cond_init = build_one_cst (new_var_type);
      comp_node = build_zero_cst (TREE_TYPE (func_parm));
      break;
    case BUILT_IN_CILKPLUS_SEC_REDUCE_ALL_ZERO:
    case BUILT_IN_CILKPLUS_SEC_REDUCE_ALL_NONZERO:
      code = ((an_type == BUILT_IN_CILKPLUS_SEC_REDUCE_ALL_ZERO) ? NE_EXPR
	: EQ_EXPR);
      init = build_one_cst (new_var_type);
      cond_init = build_zero_cst (new_var_type);
      comp_node = build_zero_cst (TREE_TYPE (func_parm));
      break;
    case BUILT_IN_CILKPLUS_SEC_REDUCE_MAX:
      code = MAX_EXPR;
      init = (TYPE_MIN_VALUE (new_var_type) ? TYPE_MIN_VALUE (new_var_type)
	: func_parm);
      break;
    case BUILT_IN_CILKPLUS_SEC_REDUCE_MIN:
      code = MIN_EXPR;
      init = (TYPE_MAX_VALUE (new_var_type) ? TYPE_MAX_VALUE (new_var_type)
	: func_parm);
      break;
    case BUILT_IN_CILKPLUS_SEC_REDUCE_MIN_IND:
    case BUILT_IN_CILKPLUS_SEC_REDUCE_MAX_IND:
      code = (an_type == BUILT_IN_CILKPLUS_SEC_REDUCE_MAX_IND ? LE_EXPR
	: GE_EXPR);
      init = an_loop_info[0].var;
      break;
    case BUILT_IN_CILKPLUS_SEC_REDUCE:
      init = identity_value;
      break;
    case BUILT_IN_CILKPLUS_SEC_REDUCE_MUTATING:
      init = NULL_TREE;
      break;
    default:
      gcc_unreachable ();
    }

  if (an_type != BUILT_IN_CILKPLUS_SEC_REDUCE_MUTATING)
    *new_var = get_temp_regvar (new_var_type, init);
  else
    *new_var = NULL_TREE;

  switch (an_type)
    {
    case BUILT_IN_CILKPLUS_SEC_REDUCE_ADD:
    case BUILT_IN_CILKPLUS_SEC_REDUCE_MUL:      
      new_expr = build_x_modify_expr (location, *new_var, code, func_parm,
				      tf_warning_or_error);
      break;
    case BUILT_IN_CILKPLUS_SEC_REDUCE_ALL_ZERO:
    case BUILT_IN_CILKPLUS_SEC_REDUCE_ALL_NONZERO:
    case BUILT_IN_CILKPLUS_SEC_REDUCE_ANY_ZERO:
    case BUILT_IN_CILKPLUS_SEC_REDUCE_ANY_NONZERO:
      /* In all these cases, assume the false case is true and as soon as
	 we find a true case,  set the true flag on and latch it in.  */
      new_yes_expr = build_x_modify_expr (location, *new_var, NOP_EXPR,
					  cond_init, tf_warning_or_error);
      new_no_expr = build_x_modify_expr (location, *new_var, NOP_EXPR,
					 *new_var, tf_warning_or_error);
      new_cond_expr = build_x_binary_op
	(location, code, func_parm, TREE_CODE (func_parm), comp_node,
	 TREE_CODE (comp_node), NULL, tf_warning_or_error);
      new_expr = build_x_conditional_expr (location, new_cond_expr,
					   new_yes_expr, new_no_expr,
					   tf_warning_or_error);
      break;
    case BUILT_IN_CILKPLUS_SEC_REDUCE_MAX:
    case BUILT_IN_CILKPLUS_SEC_REDUCE_MIN:
      new_cond_expr = build_x_binary_op
	(location, code, *new_var, TREE_CODE (*new_var), func_parm,
	 TREE_CODE (func_parm), NULL, tf_warning_or_error);
      new_expr = build_x_modify_expr (location, *new_var, NOP_EXPR, func_parm,
				      tf_warning_or_error);
      break;
    case BUILT_IN_CILKPLUS_SEC_REDUCE_MAX_IND:
    case BUILT_IN_CILKPLUS_SEC_REDUCE_MIN_IND:
      new_yes_expr = build_x_modify_expr (location, array_ind_value, NOP_EXPR,
					  func_parm, tf_warning_or_error);
      new_no_expr = build_x_modify_expr (location, array_ind_value, NOP_EXPR,
					 array_ind_value, tf_warning_or_error);
      if (list_size > 1)
	new_yes_ind = build_x_modify_expr (location, *new_var, NOP_EXPR,
					   an_loop_info[0].var,
					   tf_warning_or_error);
      else
	new_yes_ind = build_x_modify_expr (location, *new_var, NOP_EXPR,
					   TREE_OPERAND (array_op0, 1),
					   tf_warning_or_error);
      new_no_ind = build_x_modify_expr (location, *new_var, NOP_EXPR, *new_var,
					tf_warning_or_error);
      new_yes_list = alloc_stmt_list ();
      append_to_statement_list (new_yes_ind, &new_yes_list);
      append_to_statement_list (new_yes_expr, &new_yes_list);

      new_no_list = alloc_stmt_list ();
      append_to_statement_list (new_no_ind, &new_no_list);
      append_to_statement_list (new_no_expr, &new_no_list);

      new_cond_expr = build_x_binary_op (location, code, array_ind_value,
					 TREE_CODE (array_ind_value), func_parm,
					 TREE_CODE (func_parm), NULL,
					 tf_warning_or_error);
      new_expr = build_x_conditional_expr (location, new_cond_expr,
					   new_yes_list, new_no_list,
					   tf_warning_or_error);
      break;
    case BUILT_IN_CILKPLUS_SEC_REDUCE:
    case BUILT_IN_CILKPLUS_SEC_REDUCE_MUTATING:
      func_args = make_tree_vector ();
      if (an_type == BUILT_IN_CILKPLUS_SEC_REDUCE)
	vec_safe_push (func_args, *new_var);
      else
	vec_safe_push (func_args, identity_value);
      vec_safe_push (func_args, func_parm);

      new_expr = finish_call_expr (call_fn, &func_args, false, true,
				   tf_warning_or_error);
      if (an_type == BUILT_IN_CILKPLUS_SEC_REDUCE)
	new_expr = build_x_modify_expr (location, *new_var, NOP_EXPR, new_expr,
					tf_warning_or_error);
      release_tree_vector (func_args);
      break;
    default:
      gcc_unreachable ();
    }
  an_init = pop_stmt_list (an_init);
  append_to_statement_list (an_init, &loop_with_init);
  body = new_expr;

  for (ii = 0; ii < rank; ii++)
    {
      tree new_loop = push_stmt_list ();
      create_an_loop (an_loop_info[ii].ind_init, an_loop_info[ii].cmp,
		      an_loop_info[ii].incr, body);
      body = pop_stmt_list (new_loop);
    }
  append_to_statement_list (body, &loop_with_init);

  an_info.release ();
  an_loop_info.release ();

  return loop_with_init;
}

/* Returns a loop with ARRAY_REF inside it with an appropriate modify expr.
   The LHS and/or RHS will be array notation expressions that have a
   MODIFYCODE.  The location of the variable is specified by LOCATION. */

static tree
expand_an_in_modify_expr (location_t location, tree lhs,
			  enum tree_code modifycode, tree rhs,
			  tsubst_flags_t complain)
{
  tree array_expr_lhs = NULL_TREE, array_expr_rhs = NULL_TREE;
  tree array_expr = NULL_TREE;
  tree body = NULL_TREE;
  vec<tree> cond_expr = vNULL;
  vec<tree, va_gc> *lhs_array_operand = NULL, *rhs_array_operand = NULL;
  size_t lhs_rank = 0, rhs_rank = 0, ii = 0;
  vec<tree, va_gc> *rhs_list = NULL, *lhs_list = NULL;
  size_t rhs_list_size = 0, lhs_list_size = 0;
  tree new_modify_expr, new_var = NULL_TREE, builtin_loop, scalar_mods;
  bool found_builtin_fn = false;
  tree an_init, loop_with_init = alloc_stmt_list ();
  vec<vec<an_parts> > lhs_an_info = vNULL, rhs_an_info = vNULL;
  vec<an_loop_parts> lhs_an_loop_info = vNULL, rhs_an_loop_info = vNULL;

  if (!find_rank (location, rhs, rhs, false, &rhs_rank))
    return error_mark_node;
  extract_array_notation_exprs (rhs, false, &rhs_list);
  rhs_list_size = vec_safe_length (rhs_list);
  an_init = push_stmt_list ();
  if (rhs_rank)
    {
      scalar_mods = replace_invariant_exprs (&rhs);
      if (scalar_mods)
	finish_expr_stmt (scalar_mods);
    }
  for (ii = 0; ii < rhs_list_size; ii++)
    {
      tree rhs_node = (*rhs_list)[ii];
      if (TREE_CODE (rhs_node) == CALL_EXPR)
	{
	  builtin_loop = expand_sec_reduce_builtin (rhs_node, &new_var);
	  if (builtin_loop == error_mark_node)
	    return error_mark_node;
	  else if (builtin_loop)
	    {
	      finish_expr_stmt (builtin_loop);
	      found_builtin_fn = true;
	      if (new_var)
		{
		  vec <tree, va_gc> *rhs_sub_list = NULL, *new_var_list = NULL;
		  vec_safe_push (rhs_sub_list, rhs_node);
		  vec_safe_push (new_var_list, new_var);
		  replace_array_notations (&rhs, false, rhs_sub_list,
					   new_var_list);
		}
	    }
	}
    }
  lhs_rank = 0;
  rhs_rank = 0;
  if (!find_rank (location, lhs, lhs, true, &lhs_rank)
      || !find_rank (location, rhs, rhs, true, &rhs_rank))
    {
      pop_stmt_list (an_init);
      return error_mark_node;
    }

  /* If both are scalar, then the only reason why we will get this far is if
     there is some array notations inside it and was using a builtin array
     notation functions.  If so, we have already broken those guys up and now 
     a simple build_x_modify_expr would do.  */
  if (lhs_rank == 0 && rhs_rank == 0)
    {
      if (found_builtin_fn)
	{
	  new_modify_expr = build_x_modify_expr (location, lhs,
						 modifycode, rhs, complain);
	  finish_expr_stmt (new_modify_expr);
	  pop_stmt_list (an_init);
	  return an_init;
	}
      else
	gcc_unreachable ();
    }

  /* If for some reason location is not set, then find if LHS or RHS has
     location info.  If so, then use that so we atleast have an idea.  */
  if (location == UNKNOWN_LOCATION)
    {
      if (EXPR_LOCATION (lhs) != UNKNOWN_LOCATION)
	location = EXPR_LOCATION (lhs);
      else if (EXPR_LOCATION (rhs) != UNKNOWN_LOCATION)
	location = EXPR_LOCATION (rhs);
    }
      
  /* We need this when we have a scatter issue.  */
  extract_array_notation_exprs (lhs, true, &lhs_list);
  rhs_list = NULL;
  extract_array_notation_exprs (rhs, true, &rhs_list);
  rhs_list_size = vec_safe_length (rhs_list);
  lhs_list_size = vec_safe_length (lhs_list);
    
  if (lhs_rank == 0 && rhs_rank != 0)
    {
      error_at (location, "%qD cannot be scalar when %qD is not", lhs, rhs);
      return error_mark_node;
    }
  if (lhs_rank != 0 && rhs_rank != 0 && lhs_rank != rhs_rank)
    {
      error_at (location, "rank mismatch between %qE and %qE", lhs, rhs);
      return error_mark_node;
    }
  
  /* Assign the array notation components to variable so that they can satisfy
     the execute-once rule.  */
  for (ii = 0; ii < lhs_list_size; ii++)
    {
      tree anode = (*lhs_list)[ii];
      make_triplet_val_inv (&ARRAY_NOTATION_START (anode));
      make_triplet_val_inv (&ARRAY_NOTATION_LENGTH (anode));
      make_triplet_val_inv (&ARRAY_NOTATION_STRIDE (anode));
    }
  for (ii = 0; ii < rhs_list_size; ii++)
    if ((*rhs_list)[ii] && TREE_CODE ((*rhs_list)[ii]) == ARRAY_NOTATION_REF)
      {
	tree aa = (*rhs_list)[ii];
	make_triplet_val_inv (&ARRAY_NOTATION_START (aa));
	make_triplet_val_inv (&ARRAY_NOTATION_LENGTH (aa));
	make_triplet_val_inv (&ARRAY_NOTATION_STRIDE (aa));
      }
  lhs_an_loop_info.safe_grow_cleared (lhs_rank);
  
  if (rhs_rank)
    rhs_an_loop_info.safe_grow_cleared (rhs_rank);

  cond_expr.safe_grow_cleared (MAX (lhs_rank, rhs_rank));
  cilkplus_extract_an_triplets (lhs_list, lhs_list_size, lhs_rank,
				&lhs_an_info);
  if (rhs_list)
    cilkplus_extract_an_triplets (rhs_list, rhs_list_size, rhs_rank,
				  &rhs_an_info);
  if (length_mismatch_in_expr_p (EXPR_LOCATION (lhs), lhs_an_info)
      || (rhs_list && length_mismatch_in_expr_p (EXPR_LOCATION (rhs),
						 rhs_an_info)))
    {
      pop_stmt_list (an_init);
      return error_mark_node;
    }
  tree rhs_len = ((rhs_list_size > 0 && rhs_rank > 0) ?
    rhs_an_info[0][0].length : NULL_TREE);
  tree lhs_len = ((lhs_list_size > 0 && lhs_rank > 0) ?
    lhs_an_info[0][0].length : NULL_TREE);
  if (lhs_list_size > 0 && rhs_list_size > 0 && lhs_rank > 0 && rhs_rank > 0
      && TREE_CODE (lhs_len) == INTEGER_CST && rhs_len
      && TREE_CODE (rhs_len) == INTEGER_CST 
      && !tree_int_cst_equal (rhs_len, lhs_len))
    { 
      error_at (location, "length mismatch between LHS and RHS"); 
      pop_stmt_list (an_init); 
      return error_mark_node;
    }
   for (ii = 0; ii < lhs_rank; ii++) 
     {
       tree typ = ptrdiff_type_node; 
       lhs_an_loop_info[ii].var = create_temporary_var (typ);
       add_decl_expr (lhs_an_loop_info[ii].var);
       lhs_an_loop_info[ii].ind_init = build_x_modify_expr 
	 (location, lhs_an_loop_info[ii].var, INIT_EXPR, build_zero_cst (typ), 
	  complain);
     }
   
   if (rhs_list_size > 0)
     {
       rhs_array_operand = fix_sec_implicit_args (location, rhs_list,
						  lhs_an_loop_info, lhs_rank,
						  lhs); 
       if (!rhs_array_operand)
	 return error_mark_node;
     }
  replace_array_notations (&rhs, true, rhs_list, rhs_array_operand);
  rhs_list_size = 0;
  rhs_list = NULL;
  extract_array_notation_exprs (rhs, true, &rhs_list);
  rhs_list_size = vec_safe_length (rhs_list);    

  for (ii = 0; ii < rhs_rank; ii++)
    {
      tree typ = ptrdiff_type_node;
      rhs_an_loop_info[ii].var = create_temporary_var (typ);
      add_decl_expr (rhs_an_loop_info[ii].var);
      rhs_an_loop_info[ii].ind_init = build_x_modify_expr
	(location, rhs_an_loop_info[ii].var, INIT_EXPR, build_zero_cst (typ), 
	 complain);
    }

  if (lhs_rank)
    {
      lhs_array_operand =
	create_array_refs (location, lhs_an_info, lhs_an_loop_info,
			    lhs_list_size, lhs_rank);
      replace_array_notations (&lhs, true, lhs_list, lhs_array_operand);
    }
  
  if (rhs_array_operand)
    vec_safe_truncate (rhs_array_operand, 0);
  if (rhs_rank)
    {
      rhs_array_operand = create_array_refs (location, rhs_an_info,
					      rhs_an_loop_info, rhs_list_size,
					      rhs_rank);
      /* Replace all the array refs created by the above function because this
	 variable is blown away by the fix_sec_implicit_args function below.  */
      replace_array_notations (&rhs, true, rhs_list, rhs_array_operand);
      vec_safe_truncate (rhs_array_operand , 0);
      rhs_array_operand = fix_sec_implicit_args (location, rhs_list,
						 rhs_an_loop_info, rhs_rank,
						 rhs);
      if (!rhs_array_operand)
	return error_mark_node;
      replace_array_notations (&rhs, true, rhs_list, rhs_array_operand);
    }

  array_expr_rhs = rhs;
  array_expr_lhs = lhs;
  
  array_expr = build_x_modify_expr (location, array_expr_lhs, modifycode,
				    array_expr_rhs, complain);
  create_cmp_incr (location, &lhs_an_loop_info, lhs_rank, lhs_an_info,
		   complain);
  if (rhs_rank) 
    create_cmp_incr (location, &rhs_an_loop_info, rhs_rank, rhs_an_info, 
		     complain);
  for (ii = 0; ii < MAX (rhs_rank, lhs_rank); ii++)
    if (ii < lhs_rank && ii < rhs_rank)
      cond_expr[ii] = build_x_binary_op
	(location, TRUTH_ANDIF_EXPR, lhs_an_loop_info[ii].cmp,
	 TREE_CODE (lhs_an_loop_info[ii].cmp), rhs_an_loop_info[ii].cmp,
	 TREE_CODE (rhs_an_loop_info[ii].cmp), NULL, complain);
    else if (ii < lhs_rank && ii >= rhs_rank)
      cond_expr[ii] = lhs_an_loop_info[ii].cmp;
    else
      /* No need to compare ii < rhs_rank && ii >= lhs_rank because in a valid 
	 Array notation expression, rank of RHS cannot be greater than LHS.  */
      gcc_unreachable ();
  
  an_init = pop_stmt_list (an_init);
  append_to_statement_list (an_init, &loop_with_init);
  body = array_expr;
  for (ii = 0; ii < MAX (lhs_rank, rhs_rank); ii++)
    {
      tree incr_list = alloc_stmt_list ();
      tree init_list = alloc_stmt_list ();
      tree new_loop = push_stmt_list ();

      if (lhs_rank)
	{
	  append_to_statement_list (lhs_an_loop_info[ii].ind_init, &init_list);
	  append_to_statement_list (lhs_an_loop_info[ii].incr, &incr_list);
	}
      if (rhs_rank)
	{
	  append_to_statement_list (rhs_an_loop_info[ii].ind_init, &init_list);
	  append_to_statement_list (rhs_an_loop_info[ii].incr, &incr_list);
	}
      create_an_loop (init_list, cond_expr[ii], incr_list, body);
      body = pop_stmt_list (new_loop);
    }
  append_to_statement_list (body, &loop_with_init);

  lhs_an_info.release ();
  lhs_an_loop_info.release ();
  if (rhs_rank) 
    { 
      rhs_an_info.release (); 
      rhs_an_loop_info.release ();
    }
  cond_expr.release ();

  return loop_with_init;
}

/* Helper function for expand_conditonal_array_notations.  Encloses the
   conditional statement passed in ORIG_STMT with a loop around it and
   replaces the condition in STMT with a ARRAY_REF tree-node to the array.  
   The condition must have a ARRAY_NOTATION_REF tree.  */

static tree
cp_expand_cond_array_notations (tree orig_stmt)
{
  vec<tree, va_gc> *array_list = NULL, *array_operand = NULL;
  size_t list_size = 0;
  size_t rank = 0, ii = 0;
  tree an_init, body, stmt = NULL_TREE;
  tree builtin_loop, new_var = NULL_TREE;
  tree loop_with_init = alloc_stmt_list ();
  location_t location = UNKNOWN_LOCATION;
  vec<vec<an_parts> > an_info = vNULL;
  vec<an_loop_parts> an_loop_info = vNULL;

  if (TREE_CODE (orig_stmt) == COND_EXPR)
    {
      size_t cond_rank = 0, yes_rank = 0, no_rank = 0;
      tree yes_expr = COND_EXPR_THEN (orig_stmt);
      tree no_expr = COND_EXPR_ELSE (orig_stmt);
      tree cond = COND_EXPR_COND (orig_stmt);
      if (!find_rank (EXPR_LOCATION (cond), cond, cond, true, &cond_rank)
	  || !find_rank (EXPR_LOCATION (yes_expr), yes_expr, yes_expr, true,
			 &yes_rank)
	  || find_rank (EXPR_LOCATION (no_expr), no_expr, no_expr, true,
			&no_rank))
	return error_mark_node;
      /* If the condition has a zero rank, then handle array notations in body
	 separately.  */
      if (cond_rank == 0)
	return orig_stmt;
      if (cond_rank != yes_rank && yes_rank != 0)
	{
	  error_at (EXPR_LOCATION (yes_expr), "rank mismatch with controlling"
		    " expression of parent if-statement");
	  return error_mark_node;
	}
      else if (cond_rank != no_rank && no_rank != 0)
	{
	  error_at (EXPR_LOCATION (no_expr), "rank mismatch with controlling "
		    "expression of parent if-statement");
	  return error_mark_node;
	}
    }
  else if (TREE_CODE (orig_stmt) == IF_STMT)
    {
      size_t cond_rank = 0, yes_rank = 0, no_rank = 0;
      tree yes_expr = THEN_CLAUSE (orig_stmt);
      tree no_expr = ELSE_CLAUSE (orig_stmt);
      tree cond = IF_COND (orig_stmt);
      if (!find_rank (EXPR_LOCATION (cond), cond, cond, true, &cond_rank)
	  || (yes_expr
	      && !find_rank (EXPR_LOCATION (yes_expr), yes_expr, yes_expr, true,
			     &yes_rank))
	  || (no_expr
	      && !find_rank (EXPR_LOCATION (no_expr), no_expr, no_expr, true,
			     &no_rank)))
	return error_mark_node;

      /* Same reasoning as for COND_EXPR.  */
      if (cond_rank == 0)
	return orig_stmt;
      else if (cond_rank != yes_rank && yes_rank != 0)
	{
	  error_at (EXPR_LOCATION (yes_expr), "rank mismatch with controlling"
		    " expression of parent if-statement");
	  return error_mark_node;
	}
      else if (cond_rank != no_rank && no_rank != 0)
	{
	  error_at (EXPR_LOCATION (no_expr), "rank mismatch with controlling "
		    "expression of parent if-statement");
	  return error_mark_node;
	}
    }
  else if (truth_value_p (TREE_CODE (orig_stmt)))
    {
      size_t left_rank = 0, right_rank = 0;
      tree left_expr = TREE_OPERAND (orig_stmt, 0);
      tree right_expr = TREE_OPERAND (orig_stmt, 1);
      if (!find_rank (EXPR_LOCATION (left_expr), left_expr, left_expr, true,
		      &left_rank)
	  || !find_rank (EXPR_LOCATION (right_expr), right_expr, right_expr,
			 true, &right_rank))
	return error_mark_node;
      if (right_rank == 0 && left_rank == 0)
	return orig_stmt;
    }

  if (!find_rank (EXPR_LOCATION (orig_stmt), orig_stmt, orig_stmt, true,
		  &rank))
    return error_mark_node;
  if (rank == 0)
    return orig_stmt;

  extract_array_notation_exprs (orig_stmt, false, &array_list);
  stmt = alloc_stmt_list ();
  for (ii = 0; ii < vec_safe_length (array_list); ii++)
    {
      tree array_node = (*array_list)[ii];
      if (TREE_CODE (array_node) == CALL_EXPR
	  || TREE_CODE (array_node) == AGGR_INIT_EXPR)
	{
	  builtin_loop = expand_sec_reduce_builtin (array_node, &new_var);
	  if (builtin_loop == error_mark_node)
	    finish_expr_stmt (error_mark_node);
	  else if (new_var)
	    {
	      vec<tree, va_gc> *sub_list = NULL, *new_var_list = NULL;
	      vec_safe_push (sub_list, array_node);
	      vec_safe_push (new_var_list, new_var);
	      replace_array_notations (&orig_stmt, false, sub_list,
				       new_var_list);
	      append_to_statement_list (builtin_loop, &stmt);
	    }
	}
    }
  append_to_statement_list (orig_stmt, &stmt);
  rank = 0;
  array_list = NULL;
  if (!find_rank (EXPR_LOCATION (stmt), stmt, stmt, true, &rank))
    return error_mark_node;
  if (rank == 0)
    return stmt;
  
  extract_array_notation_exprs (stmt, true, &array_list);
  list_size = vec_safe_length (array_list);
  if (list_size == 0)
    return stmt;

  location = EXPR_LOCATION (orig_stmt);
  list_size = vec_safe_length (array_list);
  an_loop_info.safe_grow_cleared (rank);
  
  an_init = push_stmt_list ();

  /* Assign the array notation components to variable so that they can
     satisfy the exec-once rule.  */
  for (ii = 0; ii < list_size; ii++)
    {
      tree anode = (*array_list)[ii];
      make_triplet_val_inv (&ARRAY_NOTATION_START (anode));
      make_triplet_val_inv (&ARRAY_NOTATION_LENGTH (anode));
      make_triplet_val_inv (&ARRAY_NOTATION_STRIDE (anode));
    }
  cilkplus_extract_an_triplets (array_list, list_size, rank, &an_info);

  for (ii = 0; ii < rank; ii++) 
    {
      tree typ = ptrdiff_type_node;
      an_loop_info[ii].var = create_temporary_var (typ);
      add_decl_expr (an_loop_info[ii].var);
      an_loop_info[ii].ind_init =
	build_x_modify_expr (location, an_loop_info[ii].var, INIT_EXPR,
			     build_zero_cst (typ), tf_warning_or_error);
    }
  array_operand = create_array_refs (location, an_info, an_loop_info,
				     list_size, rank);
  replace_array_notations (&stmt, true, array_list, array_operand);
  create_cmp_incr (location, &an_loop_info, rank, an_info, tf_warning_or_error);
  
  an_init = pop_stmt_list (an_init);
  append_to_statement_list (an_init, &loop_with_init);
  body = stmt;

  for (ii = 0; ii < rank; ii++)
    {
      tree new_loop = push_stmt_list ();
      create_an_loop (an_loop_info[ii].ind_init, an_loop_info[ii].cmp,
		      an_loop_info[ii].incr, body);
      body = pop_stmt_list (new_loop);
    }
  append_to_statement_list (body, &loop_with_init);

  an_info.release ();
  an_loop_info.release ();
  
  return loop_with_init;
}

/* Transforms array notations inside unary expression ORIG_STMT with an
   appropriate loop and ARRAY_REF (and returns all this as a super-tree called
   LOOP).  */

static tree
expand_unary_array_notation_exprs (tree orig_stmt)
{
  vec<tree, va_gc> *array_list = NULL, *array_operand = NULL;
  size_t list_size = 0, rank = 0, ii = 0;
  tree body;
  tree builtin_loop, stmt = NULL_TREE, new_var = NULL_TREE;
  location_t location = EXPR_LOCATION (orig_stmt);
  tree an_init, loop_with_init = alloc_stmt_list ();
  vec<vec<an_parts> > an_info = vNULL;
  vec<an_loop_parts> an_loop_info = vNULL;
  
  if (!find_rank (location, orig_stmt, orig_stmt, true, &rank))
    return error_mark_node;
  if (rank == 0)
    return orig_stmt;  
  
  extract_array_notation_exprs (orig_stmt, false, &array_list);
  list_size = vec_safe_length (array_list);
  location = EXPR_LOCATION (orig_stmt);
  stmt = NULL_TREE;
  for (ii = 0; ii < list_size; ii++)
    if (TREE_CODE ((*array_list)[ii]) == CALL_EXPR
	|| TREE_CODE ((*array_list)[ii]) == AGGR_INIT_EXPR)
      {
	tree list_node = (*array_list)[ii];
	builtin_loop = expand_sec_reduce_builtin (list_node, &new_var);
	if (builtin_loop == error_mark_node)
	  return error_mark_node;
	else if (builtin_loop)
	  {
	    vec<tree, va_gc> *sub_list = NULL, *new_var_list = NULL;
	    stmt = alloc_stmt_list ();
	    append_to_statement_list (builtin_loop, &stmt);
	    vec_safe_push (sub_list, list_node);
	    vec_safe_push (new_var_list, new_var);
	    replace_array_notations (&orig_stmt, false, sub_list, new_var_list);
	  }	
      }
  if (stmt != NULL_TREE)
    append_to_statement_list (finish_expr_stmt (orig_stmt), &stmt);
  else
    stmt = orig_stmt;
  rank = 0;
  list_size = 0;
  array_list = NULL;
  extract_array_notation_exprs (stmt, true, &array_list);
  list_size = vec_safe_length (array_list);

  if (!find_rank (EXPR_LOCATION (stmt), stmt, stmt, true, &rank))
    return error_mark_node;
  if (rank == 0 || list_size == 0)
    return stmt;
  an_loop_info.safe_grow_cleared (rank);
  an_init = push_stmt_list ();  
    /* Assign the array notation components to variable so that they can satisfy
     the exec-once rule.  */
  for (ii = 0; ii < list_size; ii++)
    {
      tree array_node = (*array_list)[ii];
      make_triplet_val_inv (&ARRAY_NOTATION_START (array_node));
      make_triplet_val_inv (&ARRAY_NOTATION_LENGTH (array_node));
      make_triplet_val_inv (&ARRAY_NOTATION_STRIDE (array_node));
    }
  cilkplus_extract_an_triplets (array_list, list_size, rank, &an_info);
  
  for (ii = 0; ii < rank; ii++)
    {
      tree typ = ptrdiff_type_node;
      an_loop_info[ii].var = create_temporary_var (typ);
      add_decl_expr (an_loop_info[ii].var);
      an_loop_info[ii].ind_init = build_x_modify_expr
	(location, an_loop_info[ii].var, INIT_EXPR, build_zero_cst (typ), 
	 tf_warning_or_error);
    }
  array_operand = create_array_refs (location, an_info, an_loop_info,
				     list_size, rank);
  replace_array_notations (&stmt, true, array_list, array_operand);
  create_cmp_incr (location, &an_loop_info, rank, an_info, tf_warning_or_error);
  
  an_init = pop_stmt_list (an_init);
  append_to_statement_list (an_init, &loop_with_init);
  body = stmt;
  
  for (ii = 0; ii < rank; ii++)
    {
      tree new_loop = push_stmt_list ();
      create_an_loop (an_loop_info[ii].ind_init, an_loop_info[ii].cmp,
		      an_loop_info[ii].incr, body);
      body = pop_stmt_list (new_loop);
    }
  append_to_statement_list (body, &loop_with_init);

  an_info.release ();
  an_loop_info.release ();

  return loop_with_init;
}

/* Expands the array notation's builtin reduction function in EXPR
   (of type RETURN_EXPR) and returns a STATEMENT_LIST that contains a loop
   with the builtin function expansion and a return statement at the end.  */

static tree
expand_return_expr (tree expr)
{
  tree new_mod_list, new_var, new_mod, retval_expr;
  size_t rank  = 0;
  location_t loc = EXPR_LOCATION (expr);
  if (TREE_CODE (expr) != RETURN_EXPR)
    return expr;
      
  if (!find_rank (loc, expr, expr, false, &rank))
    return error_mark_node;

  /* If the return expression contains array notations, then flag it as
     error.  */
  if (rank >= 1)
    {
      error_at (loc, "array notation expression cannot be used as a return "
		"value");
      return error_mark_node;
    }
  
  new_mod_list = push_stmt_list ();
  retval_expr = TREE_OPERAND (expr, 0);
  new_var = create_temporary_var (TREE_TYPE (retval_expr));
  add_decl_expr (new_var);
  new_mod = expand_an_in_modify_expr (loc, new_var, NOP_EXPR,
				      TREE_OPERAND (retval_expr, 1),
				      tf_warning_or_error);
  TREE_OPERAND (retval_expr, 1) = new_var;
  TREE_OPERAND (expr, 0) = retval_expr;
  add_stmt (new_mod);
  add_stmt (expr);
  new_mod_list = pop_stmt_list (new_mod_list);
  return new_mod_list;
}

/* Expands ARRAY_NOTATION_REF and builtin functions in a compound statement,
   STMT. Returns the STMT with expanded array notations.  */

tree
expand_array_notation_exprs (tree t)
{
  enum tree_code code;
  bool is_expr;
  location_t loc = UNKNOWN_LOCATION;
  
  if (!t)
    return t;

  loc = EXPR_LOCATION (t);

  code = TREE_CODE (t); 
  is_expr = IS_EXPR_CODE_CLASS (TREE_CODE_CLASS (code));
  switch (code)
    {
    case ERROR_MARK:
    case IDENTIFIER_NODE:
    case VOID_CST:
    case INTEGER_CST:
    case REAL_CST:
    case FIXED_CST:
    case STRING_CST:
    case BLOCK:
    case PLACEHOLDER_EXPR:
    case FIELD_DECL:
    case VOID_TYPE:
    case REAL_TYPE:
    case SSA_NAME:
    case LABEL_DECL:
    case RESULT_DECL:
    case VAR_DECL:
    case PARM_DECL:
    case NON_LVALUE_EXPR:
    case NOP_EXPR:
    case INIT_EXPR:
    case ADDR_EXPR:
    case ARRAY_REF:
    case BIT_FIELD_REF:
    case VECTOR_CST:
    case COMPLEX_CST:
      return t;
    case MODIFY_EXPR:
      if (contains_array_notation_expr (t))
	t = expand_an_in_modify_expr (loc, TREE_OPERAND (t, 0), NOP_EXPR, 
					 TREE_OPERAND (t, 1), 
					 tf_warning_or_error);
      return t;
    case MODOP_EXPR:
      if (contains_array_notation_expr (t) && !processing_template_decl)
	t = expand_an_in_modify_expr
	  (loc, TREE_OPERAND (t, 0), TREE_CODE (TREE_OPERAND (t, 1)),
	   TREE_OPERAND (t, 2), tf_warning_or_error);
      return t;
    case CONSTRUCTOR:
      return t;
    case BIND_EXPR:
      {
	BIND_EXPR_BODY (t) =
	  expand_array_notation_exprs  (BIND_EXPR_BODY (t));
	return t;
      }
    case DECL_EXPR:
      {
	tree x = DECL_EXPR_DECL (t);
	if (t && TREE_CODE (x) != FUNCTION_DECL)
	  if (DECL_INITIAL (x))
	    t = expand_unary_array_notation_exprs (t);
      return t;
      }
    case STATEMENT_LIST:
      {
	tree_stmt_iterator i;
	for (i = tsi_start (t); !tsi_end_p (i); tsi_next (&i))
	  *tsi_stmt_ptr (i) =
	    expand_array_notation_exprs (*tsi_stmt_ptr (i));
	return t;
      }

    case OMP_PARALLEL:
    case OMP_TASK:
    case OMP_FOR:
    case OMP_SINGLE:
    case OMP_SECTION:
    case OMP_SECTIONS:
    case OMP_MASTER:
    case OMP_TASKGROUP:
    case OMP_ORDERED:
    case OMP_CRITICAL:
    case OMP_ATOMIC:
    case OMP_CLAUSE:
    case TARGET_EXPR:
    case INTEGER_TYPE:
    case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:
    case POINTER_TYPE:
    case ARRAY_TYPE:
    case RECORD_TYPE:
    case METHOD_TYPE:
      return t;
    case RETURN_EXPR:
      if (contains_array_notation_expr (t))
	t = expand_return_expr (t);
      return t;
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case AGGR_INIT_EXPR:
    case CALL_EXPR:
      t = expand_unary_array_notation_exprs (t);
      return t;
    case CONVERT_EXPR:
    case CLEANUP_POINT_EXPR:
    case EXPR_STMT:
      TREE_OPERAND (t, 0) = expand_array_notation_exprs (TREE_OPERAND (t, 0));
      /* It is not necessary to wrap error_mark_node in EXPR_STMT.  */
      if (TREE_OPERAND (t, 0) == error_mark_node)
	return TREE_OPERAND (t, 0); 
      return t;
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
    case TRUTH_NOT_EXPR:
    case COND_EXPR:
      t = cp_expand_cond_array_notations (t);
      if (TREE_CODE (t) == COND_EXPR)
	{
	  COND_EXPR_THEN (t) =
	    expand_array_notation_exprs (COND_EXPR_THEN (t));
	  COND_EXPR_ELSE (t) =
	    expand_array_notation_exprs (COND_EXPR_ELSE (t));
	}
      return t;
    case FOR_STMT:
      if (contains_array_notation_expr (FOR_COND (t)))
	{
	  error_at (EXPR_LOCATION (FOR_COND (t)),
		    "array notation cannot be used in a condition for "
		    "a for-loop");
	  return error_mark_node;
	}
      /* FIXME: Add a check for CILK_FOR_STMT here when we add Cilk tasking 
	 keywords.  */
      if (TREE_CODE (t) == FOR_STMT)
	{ 
	  FOR_BODY (t) = expand_array_notation_exprs (FOR_BODY (t));
	  FOR_EXPR (t) = expand_array_notation_exprs (FOR_EXPR (t));
	}
      else
	t = expand_array_notation_exprs (t);
      return t;
    case IF_STMT:
      t = cp_expand_cond_array_notations (t);
      /* If the above function added some extra instructions above the original
	 if statement, then we can't assume it is still IF_STMT so we have to
	 check again.  */
      if (TREE_CODE (t) == IF_STMT)
	{
	  if (THEN_CLAUSE (t))
	    THEN_CLAUSE (t) = expand_array_notation_exprs (THEN_CLAUSE (t));
	  if (ELSE_CLAUSE (t))
	    ELSE_CLAUSE (t) = expand_array_notation_exprs (ELSE_CLAUSE (t));
	}
      else
	t = expand_array_notation_exprs (t);
      return t;
    case SWITCH_STMT:
      if (contains_array_notation_expr (SWITCH_STMT_COND (t)))
	{
	  error_at (EXPR_LOCATION (SWITCH_STMT_COND (t)),
		    "array notation cannot be used as a condition for "
		    "switch statement");
	  return error_mark_node;
	}
      if (SWITCH_STMT_BODY (t))
	SWITCH_STMT_BODY (t) =
	  expand_array_notation_exprs (SWITCH_STMT_BODY (t));
      return t;
    case WHILE_STMT:
      if (contains_array_notation_expr (WHILE_COND (t)))
	{
	  if (EXPR_LOCATION (WHILE_COND (t)) != UNKNOWN_LOCATION)
	    loc = EXPR_LOCATION (WHILE_COND (t));
	  error_at (loc, "array notation cannot be used as a condition for "
		    "while statement");
	  return error_mark_node;
	}
      if (WHILE_BODY (t))
	WHILE_BODY (t) = expand_array_notation_exprs (WHILE_BODY (t));
      return t;
    case DO_STMT:
      if (contains_array_notation_expr (DO_COND (t)))
	{
	  error_at (EXPR_LOCATION (DO_COND (t)),
		    "array notation cannot be used as a condition for a "
		    "do-while statement");
	  return error_mark_node;
	}
      if (DO_BODY (t))
	DO_BODY (t) = expand_array_notation_exprs (DO_BODY (t));
      return t;
    default:
      if (is_expr)
	{
	  int i, len;

	  /* Walk over all the sub-trees of this operand.  */
	  len = TREE_CODE_LENGTH (code);

	  /* Go through the subtrees.  We need to do this in forward order so
	     that the scope of a FOR_EXPR is handled properly.  */
	  for (i = 0; i < len; ++i)
	    TREE_OPERAND (t, i) =
	      expand_array_notation_exprs (TREE_OPERAND (t, i));
	}
      return t;
    }
  return t;
}

/* Given the base of an array (ARRAY), the START (start_index), the number of 
   elements to be accessed (LENGTH) and the STRIDE, construct an 
   ARRAY_NOTATION_REF tree of type TYPE and return it.  Restrictions on START, 
   LENGTH and STRIDE are the same as that of index field passed into ARRAY_REF. 
   The only additional restriction is that, unlike index in ARRAY_REF, stride, 
   length and start_index cannot contain array notations.  */

tree
build_array_notation_ref (location_t loc, tree array, tree start, tree length, 
			  tree stride, tree type)
{
  tree array_ntn_expr = NULL_TREE;

  /* If we enter the then-case of the if-statement below, we have hit a case 
     like this: ARRAY [:].  */
  if (!start && !length)
    {
      if (TREE_CODE (type) != ARRAY_TYPE)
	{
	  error_at (loc, "start-index and length fields necessary for "
		    "using array notation in pointers or records");
	  return error_mark_node;
	}
      tree domain = TYPE_DOMAIN (type);
      if (!domain)
	{
	  error_at (loc, "start-index and length fields necessary for "
		    "using array notation with array of unknown bound");
	  return error_mark_node;
	}
      start = cp_fold_convert (ptrdiff_type_node, TYPE_MINVAL (domain));
      length = size_binop (PLUS_EXPR, TYPE_MAXVAL (domain), size_one_node);
      length = cp_fold_convert (ptrdiff_type_node, length);
    }
    
  if (!stride) 
    stride = build_one_cst (ptrdiff_type_node);
  
  /* When dealing with templates, triplet type-checking will be done in pt.c 
     after type substitution.  */
  if (processing_template_decl 
      && (type_dependent_expression_p (array) 
	  || type_dependent_expression_p (length) 
	  || type_dependent_expression_p (start) 
	  || type_dependent_expression_p (stride))) 
    array_ntn_expr = build_min_nt_loc (loc, ARRAY_NOTATION_REF, array, start, 
				       length, stride, NULL_TREE);
  else 
    { 
      if (!cilkplus_an_triplet_types_ok_p (loc, start, length, stride, type))
	return error_mark_node;
      array_ntn_expr = build4 (ARRAY_NOTATION_REF, NULL_TREE, array, start, 
			       length, stride);
    }
  if (TREE_CODE (type) == ARRAY_TYPE || TREE_CODE (type) == POINTER_TYPE)
    TREE_TYPE (array_ntn_expr) = TREE_TYPE (type);
  else
    gcc_unreachable ();

  SET_EXPR_LOCATION (array_ntn_expr, loc);
  return array_ntn_expr;
}

/* Returns false if any of the Array notation triplet values: START_INDEX,
   LENGTH and STRIDE, are not of integral type and have a rank greater than
   zero.  */

bool
cilkplus_an_triplet_types_ok_p (location_t loc, tree start_index, tree length,
				tree stride, tree type)
{
  size_t stride_rank = 0, length_rank = 0, start_rank = 0;
  if (!TREE_TYPE (start_index) || !INTEGRAL_TYPE_P (TREE_TYPE (start_index)))
    {
      error_at (loc, "start-index of array notation triplet is not an integer");
      return false;
    }
  if (!TREE_TYPE (length) || !INTEGRAL_TYPE_P (TREE_TYPE (length)))
    {
      error_at (loc, "length of array notation triplet is not an integer");
      return false;
    }
  if (!TREE_TYPE (stride) || !INTEGRAL_TYPE_P (TREE_TYPE (stride)))
    {
      error_at (loc, "stride of array notation triplet is not an integer");
      return false;
    }
  if (TREE_CODE (type) == FUNCTION_TYPE)
    {
      error_at (loc, "array notation cannot be used with function type");
      return false;
    }
  if (!find_rank (loc, start_index, start_index, false, &start_rank)
      || !find_rank (loc, length, length, false, &length_rank)
      || !find_rank (loc, stride, stride, false, &stride_rank))
    return false;

  if (start_rank != 0)
    {
      error_at (loc, "rank of an array notation triplet%'s start-index is not "
		"zero");
      return false;
    }
  if (length_rank != 0)
    {
      error_at (loc, "rank of an array notation triplet%'s length is not zero");
      return false;
    }
  if (stride_rank != 0)
    {
      error_at (loc, "rank of array notation triplet%'s stride is not zero");
      return false;
    }
  return true;
}
