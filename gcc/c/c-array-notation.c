/* This file is part of the Intel(R) Cilk(TM) Plus support
   This file contains routines to handle Array Notation expression
   handling routines in the C Compiler.
   Copyright (C) 2013-2017 Free Software Foundation, Inc.
   Contributed by Balaji V. Iyer <balaji.v.iyer@intel.com>,
                  Intel Corporation.

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

   For example, A[0:5:2], implies that we are accessing A[0], A[2], A[4],
   A[6] and A[8]. The user is responsible to make sure the access length does
   not step outside the array's size.
   
   In this section, I highlight the overall method on how array notations are
   broken up into C/C++ code.  Almost all the functions follows this overall
   technique:

   Let's say we have an array notation in a statement like this:

   A[St1:Ln:Str1] = B[St2:Ln:Str2] + <NON ARRAY_NOTATION_STMT>

   where St{1,2} = Starting index,
   Ln = Number of elements we need to access,
   and Str{1,2} = the stride.
   Note: The length of both the array notation expressions must be the same.
   
   The above expression is broken into the following
   (with the help of c_finish_loop function from c-typeck.c):
   
   Tmp_Var = 0;
   goto compare_label:
   body_label:

   A[St1+Tmp_Var*Str1] = B[St1+Tmp_Var*Str2] + <NON ARRAY_NOTATION_STMT>;
   Tmp_Var++;
   
   compare_label:				
     if (Tmp_Var < Ln)
       goto body_label;
     else
       goto exit_label;
   exit_label:		  	      

*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "c-tree.h"
#include "gimple-expr.h"
#include "tree-iterator.h"

/* If *VALUE is not of type INTEGER_CST, PARM_DECL or VAR_DECL, then map it
   to a variable and then set *VALUE to the new variable.  */

static inline void
make_triplet_val_inv (location_t loc, tree *value)
{
  tree var, new_exp;
  if (TREE_CODE (*value) != INTEGER_CST
      && TREE_CODE (*value) != PARM_DECL
      && !VAR_P (*value))
    {
      var = build_decl (loc, VAR_DECL, NULL_TREE, integer_type_node);
      new_exp = build_modify_expr (loc, var, TREE_TYPE (var), NOP_EXPR, loc,
				   *value, TREE_TYPE (*value));
      add_stmt (new_exp);
      *value = var;
    }
}

/* Populates the INCR and CMP vectors with the increment (of type POSTINCREMENT
   or POSTDECREMENT) and comparison (of TYPE GT_EXPR or LT_EXPR) expressions,
   using data from LENGTH, COUNT_DOWN, and VAR.  INCR and CMP vectors are of
   size RANK.  */

static void
create_cmp_incr (location_t loc, vec<an_loop_parts> *node, size_t rank,
		 vec<vec<an_parts> > an_info)
{
  for (size_t ii = 0; ii < rank; ii++)
    {
      tree var = (*node)[ii].var;
      tree length = an_info[0][ii].length;
      (*node)[ii].incr = build_unary_op (loc, POSTINCREMENT_EXPR, var, false);
      (*node)[ii].cmp = build2 (LT_EXPR, boolean_type_node, var, length);
    }
}

/* Returns a vector of size RANK that contains an array ref that is derived from
   array notation triplet parameters stored in VALUE, START, STRIDE.  IS_VECTOR
   is used to check if the data stored at its corresponding location is an
   array notation. VAR is the induction variable passed in by the caller.

   For example: For an array notation A[5:10:2], the vector start  will be
   of size 1 holding '5', stride of same size as start but holding the value of
   as 2, is_vector as true and count_down as false. Let's assume VAR is 'x'
   This function returns a vector of size 1 with the following data:
   A[5 + (x * 2)] .
*/

static vec<tree, va_gc> *
create_array_refs (location_t loc, vec<vec<an_parts> > an_info,
		   vec<an_loop_parts> an_loop_info, size_t size, size_t rank)
{
  tree ind_mult, ind_incr;
  vec<tree, va_gc> *array_operand = NULL;
  for (size_t ii = 0; ii < size; ii++)
    if (an_info[ii][0].is_vector)
      {
	tree array_opr = an_info[ii][rank - 1].value;
	for (int s_jj = rank - 1; s_jj >= 0; s_jj--)
	  {
	    tree var = an_loop_info[s_jj].var;
	    tree stride = an_info[ii][s_jj].stride;
	    tree start = an_info[ii][s_jj].start;
	    ind_mult = build2 (MULT_EXPR, TREE_TYPE (var), var, stride);
	    ind_incr = build2 (PLUS_EXPR, TREE_TYPE (var), start, ind_mult);
	    array_opr = build_array_ref (loc, array_opr, ind_incr);
	  }
	vec_safe_push (array_operand, array_opr);
      }
    else
      /* This is just a dummy node to make sure both the list sizes for both
	 array list and array operand list are the same.  */
      vec_safe_push (array_operand, integer_one_node);
  return array_operand;
}		     
  
/* Replaces all the scalar expressions in *NODE.  Returns a STATEMENT_LIST that
   holds the NODE along with variables that holds the results of the invariant
   expressions.  */

tree
replace_invariant_exprs (tree *node)
{
  size_t ix = 0;
  tree node_list = NULL_TREE;
  tree t = NULL_TREE, new_var = NULL_TREE, new_node; 
  struct inv_list data;

  data.list_values = NULL;
  data.replacement = NULL;
  data.additional_tcodes = NULL;
  walk_tree (node, find_inv_trees, (void *)&data, NULL);

  if (vec_safe_length (data.list_values))
    {
      node_list = push_stmt_list ();
      for (ix = 0; vec_safe_iterate (data.list_values, ix, &t); ix++)
	{
	  new_var = build_decl (EXPR_LOCATION (t), VAR_DECL, NULL_TREE,
				TREE_TYPE (t));
	  gcc_assert (new_var != NULL_TREE && new_var != error_mark_node);
	  new_node = build2 (MODIFY_EXPR, TREE_TYPE (t), new_var, t);
	  add_stmt (new_node);
	  vec_safe_push (data.replacement, new_var);
	}
      walk_tree (node, replace_inv_trees, (void *)&data, NULL);
      node_list = pop_stmt_list (node_list);
    }
  return node_list;
}

/* Given a CALL_EXPR to an array notation built-in function in
   AN_BUILTIN_FN, replace the call with the appropriate loop and
   computation.  Return the computation in *NEW_VAR.

   The return value in *NEW_VAR will always be a scalar.  If the
   built-in is __sec_reduce_mutating, *NEW_VAR is set to NULL_TREE.  */

static tree
fix_builtin_array_notation_fn (tree an_builtin_fn, tree *new_var)
{
  tree new_var_type = NULL_TREE, func_parm, new_expr, new_yes_expr, new_no_expr;
  tree array_ind_value = NULL_TREE, new_no_ind, new_yes_ind, new_no_list;
  tree new_yes_list, new_cond_expr, new_var_init = NULL_TREE;
  tree new_exp_init = NULL_TREE;
  vec<tree, va_gc> *array_list = NULL, *array_operand = NULL;
  size_t list_size = 0, rank = 0, ii = 0;
  tree loop_init, array_op0;
  tree identity_value = NULL_TREE, call_fn = NULL_TREE, new_call_expr, body;
  location_t location = UNKNOWN_LOCATION;
  tree loop_with_init = alloc_stmt_list ();
  vec<vec<an_parts> > an_info = vNULL;
  auto_vec<an_loop_parts> an_loop_info;
  enum built_in_function an_type =
    is_cilkplus_reduce_builtin (CALL_EXPR_FN (an_builtin_fn));
  if (an_type == BUILT_IN_NONE)
    return NULL_TREE;

  /* Builtin call should contain at least one argument.  */
  if (call_expr_nargs (an_builtin_fn) == 0)
    {
      error_at (EXPR_LOCATION (an_builtin_fn), "Invalid builtin arguments");
      return error_mark_node;
    }

  if (an_type == BUILT_IN_CILKPLUS_SEC_REDUCE
      || an_type == BUILT_IN_CILKPLUS_SEC_REDUCE_MUTATING)
    {
      call_fn = CALL_EXPR_ARG (an_builtin_fn, 2);
      if (TREE_CODE (call_fn) == ADDR_EXPR)
	call_fn = TREE_OPERAND (call_fn, 0);
      identity_value = CALL_EXPR_ARG (an_builtin_fn, 0);
      func_parm = CALL_EXPR_ARG (an_builtin_fn, 1);
    }
  else
    func_parm = CALL_EXPR_ARG (an_builtin_fn, 0);
  
  /* Fully fold any EXCESSIVE_PRECISION EXPR that can occur in the function
     parameter.  */
  func_parm = c_fully_fold (func_parm, false, NULL);
  if (func_parm == error_mark_node)
    return error_mark_node;
  
  location = EXPR_LOCATION (an_builtin_fn);
  
  if (!find_rank (location, an_builtin_fn, an_builtin_fn, true, &rank))
    return error_mark_node;
 
  if (rank == 0)
    {
      error_at (location, "Invalid builtin arguments");
      return error_mark_node;
    }
  else if (rank > 1 
	   && (an_type == BUILT_IN_CILKPLUS_SEC_REDUCE_MAX_IND
	       || an_type == BUILT_IN_CILKPLUS_SEC_REDUCE_MIN_IND))
    {
      error_at (location, "__sec_reduce_min_ind or __sec_reduce_max_ind cannot"
		" have arrays with dimension greater than 1");
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
    case BUILT_IN_CILKPLUS_SEC_REDUCE_ALL_NONZERO:
    case BUILT_IN_CILKPLUS_SEC_REDUCE_ANY_ZERO:
    case BUILT_IN_CILKPLUS_SEC_REDUCE_ANY_NONZERO:
      new_var_type = integer_type_node;
      break;
    case BUILT_IN_CILKPLUS_SEC_REDUCE_MAX_IND:
    case BUILT_IN_CILKPLUS_SEC_REDUCE_MIN_IND:
      new_var_type = integer_type_node;
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

  an_loop_info.safe_grow_cleared (rank);
  cilkplus_extract_an_triplets (array_list, list_size, rank, &an_info);
  loop_init = alloc_stmt_list ();

  for (ii = 0; ii < rank; ii++)
    {
      an_loop_info[ii].var = create_tmp_var (integer_type_node);
      an_loop_info[ii].ind_init =
	build_modify_expr (location, an_loop_info[ii].var,
			   TREE_TYPE (an_loop_info[ii].var), NOP_EXPR,
			   location,
			   build_int_cst (TREE_TYPE (an_loop_info[ii].var), 0),
			   TREE_TYPE (an_loop_info[ii].var));	
    }
  array_operand = create_array_refs (location, an_info, an_loop_info,
				     list_size, rank);
  replace_array_notations (&func_parm, true, array_list, array_operand);

  create_cmp_incr (location, &an_loop_info, rank, an_info);
  if (an_type != BUILT_IN_CILKPLUS_SEC_REDUCE_MUTATING)
    {
      *new_var = build_decl (location, VAR_DECL, NULL_TREE, new_var_type);
      gcc_assert (*new_var && *new_var != error_mark_node);
    }
  else
    *new_var = NULL_TREE;
  
  if (an_type == BUILT_IN_CILKPLUS_SEC_REDUCE_MAX_IND
      || an_type == BUILT_IN_CILKPLUS_SEC_REDUCE_MIN_IND)
    array_ind_value = build_decl (location, VAR_DECL, NULL_TREE, 
				  TREE_TYPE (func_parm));
  array_op0 = (*array_operand)[0];
  if (INDIRECT_REF_P (array_op0))
    array_op0 = TREE_OPERAND (array_op0, 0);
  switch (an_type)
    {
    case BUILT_IN_CILKPLUS_SEC_REDUCE_ADD:
      new_var_init = build_modify_expr
	(location, *new_var, TREE_TYPE (*new_var), NOP_EXPR,
	 location, build_zero_cst (new_var_type), new_var_type);
      new_expr = build_modify_expr
	(location, *new_var, TREE_TYPE (*new_var), PLUS_EXPR,
	 location, func_parm, TREE_TYPE (func_parm));
      break;
    case BUILT_IN_CILKPLUS_SEC_REDUCE_MUL:
      new_var_init = build_modify_expr
	(location, *new_var, TREE_TYPE (*new_var), NOP_EXPR,
	 location, build_one_cst (new_var_type), new_var_type);
      new_expr = build_modify_expr
	(location, *new_var, TREE_TYPE (*new_var), MULT_EXPR,
	 location, func_parm, TREE_TYPE (func_parm));
      break;
    case BUILT_IN_CILKPLUS_SEC_REDUCE_ALL_ZERO:
      new_var_init = build_modify_expr
	(location, *new_var, TREE_TYPE (*new_var), NOP_EXPR,
	 location, build_one_cst (new_var_type), new_var_type);
      /* Initially you assume everything is zero, now if we find a case where 
	 it is NOT true, then we set the result to false. Otherwise 
	 we just keep the previous value.  */
      new_yes_expr = build_modify_expr
	(location, *new_var, TREE_TYPE (*new_var), NOP_EXPR,
	 location, build_zero_cst (TREE_TYPE (*new_var)),
	 TREE_TYPE (*new_var));
      new_no_expr = build_modify_expr
	(location, *new_var, TREE_TYPE (*new_var), NOP_EXPR,
	 location, *new_var, TREE_TYPE (*new_var));
      new_cond_expr = build2 (NE_EXPR, TREE_TYPE (func_parm), func_parm,
			      build_zero_cst (TREE_TYPE (func_parm)));
      new_expr = build_conditional_expr
	(location, new_cond_expr, false, new_yes_expr,
	 TREE_TYPE (new_yes_expr), new_no_expr, TREE_TYPE (new_no_expr));
      break;
    case BUILT_IN_CILKPLUS_SEC_REDUCE_ALL_NONZERO:
      new_var_init = build_modify_expr
	(location, *new_var, TREE_TYPE (*new_var), NOP_EXPR,
	 location, build_one_cst (new_var_type), new_var_type);
      /* Initially you assume everything is non-zero, now if we find a case
	 where it is NOT true, then we set the result to false.  Otherwise
	 we just keep the previous value.  */
      new_yes_expr = build_modify_expr
	(location, *new_var, TREE_TYPE (*new_var), NOP_EXPR,
	 location, build_zero_cst (TREE_TYPE (*new_var)),
	 TREE_TYPE (*new_var));
      new_no_expr = build_modify_expr
	(location, *new_var, TREE_TYPE (*new_var), NOP_EXPR,
	 location, *new_var, TREE_TYPE (*new_var));
      new_cond_expr = build2 (EQ_EXPR, TREE_TYPE (func_parm), func_parm,
			      build_zero_cst (TREE_TYPE (func_parm)));
      new_expr = build_conditional_expr
	(location, new_cond_expr, false, new_yes_expr,
	 TREE_TYPE (new_yes_expr), new_no_expr, TREE_TYPE (new_no_expr));
      break;
    case BUILT_IN_CILKPLUS_SEC_REDUCE_ANY_ZERO:
      new_var_init = build_modify_expr
	(location, *new_var, TREE_TYPE (*new_var), NOP_EXPR,
	 location, build_zero_cst (new_var_type), new_var_type);
      /* Initially we assume there are NO zeros in the list. When we find 
	 a non-zero, we keep the previous value.  If we find a zero, we 
	 set the value to true.  */
      new_yes_expr = build_modify_expr
	(location, *new_var, TREE_TYPE (*new_var), NOP_EXPR,
	 location, build_one_cst (new_var_type), new_var_type);
      new_no_expr = build_modify_expr
	(location, *new_var, TREE_TYPE (*new_var), NOP_EXPR,
	 location, *new_var, TREE_TYPE (*new_var));
      new_cond_expr = build2 (EQ_EXPR, TREE_TYPE (func_parm), func_parm,
			      build_zero_cst (TREE_TYPE (func_parm)));
      new_expr = build_conditional_expr
	(location, new_cond_expr, false, new_yes_expr,
	 TREE_TYPE (new_yes_expr), new_no_expr, TREE_TYPE (new_no_expr));   
      break;
    case BUILT_IN_CILKPLUS_SEC_REDUCE_ANY_NONZERO:
      new_var_init = build_modify_expr
	(location, *new_var, TREE_TYPE (*new_var), NOP_EXPR,
	 location, build_zero_cst (new_var_type), new_var_type);
      /* Initially we assume there are NO non-zeros in the list. When we find 
	 a zero, we keep the previous value.  If we find a non-zero, we set 
	 the value to true.  */
      new_yes_expr = build_modify_expr
	(location, *new_var, TREE_TYPE (*new_var), NOP_EXPR,
	 location, build_one_cst (new_var_type), new_var_type);
      new_no_expr = build_modify_expr
	(location, *new_var, TREE_TYPE (*new_var), NOP_EXPR,
	 location, *new_var, TREE_TYPE (*new_var));
      new_cond_expr = build2 (NE_EXPR, TREE_TYPE (func_parm), func_parm,
			      build_zero_cst (TREE_TYPE (func_parm)));
      new_expr = build_conditional_expr
	(location, new_cond_expr, false, new_yes_expr,
	 TREE_TYPE (new_yes_expr), new_no_expr, TREE_TYPE (new_no_expr));   
      break;
    case BUILT_IN_CILKPLUS_SEC_REDUCE_MAX:
      if (TYPE_MIN_VALUE (new_var_type))
	new_var_init = build_modify_expr
	  (location, *new_var, TREE_TYPE (*new_var), NOP_EXPR,
	   location, TYPE_MIN_VALUE (new_var_type), new_var_type);
      else
	new_var_init = build_modify_expr
	  (location, *new_var, TREE_TYPE (*new_var), NOP_EXPR,
	   location, func_parm, new_var_type);
      new_no_expr = build_modify_expr
	(location, *new_var, TREE_TYPE (*new_var), NOP_EXPR,
	 location, *new_var, TREE_TYPE (*new_var));
      new_yes_expr = build_modify_expr
	(location, *new_var, TREE_TYPE (*new_var), NOP_EXPR,
	 location, func_parm, TREE_TYPE (*new_var));
      new_expr = build_conditional_expr
	(location,
	 build2 (LT_EXPR, TREE_TYPE (*new_var), *new_var, func_parm), false,
	 new_yes_expr, TREE_TYPE (*new_var), new_no_expr, TREE_TYPE (*new_var));
      break;
    case BUILT_IN_CILKPLUS_SEC_REDUCE_MIN:
      if (TYPE_MAX_VALUE (new_var_type))
	new_var_init = build_modify_expr
	  (location, *new_var, TREE_TYPE (*new_var), NOP_EXPR,
	   location, TYPE_MAX_VALUE (new_var_type), new_var_type);
      else
	new_var_init = build_modify_expr
	  (location, *new_var, TREE_TYPE (*new_var), NOP_EXPR,
	   location, func_parm, new_var_type);
      new_no_expr = build_modify_expr
	(location, *new_var, TREE_TYPE (*new_var), NOP_EXPR,
	 location, *new_var, TREE_TYPE (*new_var));
      new_yes_expr = build_modify_expr
	(location, *new_var, TREE_TYPE (*new_var), NOP_EXPR,
	 location, func_parm, TREE_TYPE (*new_var));
      new_expr = build_conditional_expr
	(location,
	 build2 (GT_EXPR, TREE_TYPE (*new_var), *new_var, func_parm), false,
	 new_yes_expr, TREE_TYPE (*new_var), new_no_expr, TREE_TYPE (*new_var));
      break;
    case BUILT_IN_CILKPLUS_SEC_REDUCE_MAX_IND:
      new_var_init = build_modify_expr
	(location, *new_var, TREE_TYPE (*new_var), NOP_EXPR,
	 location, build_zero_cst (new_var_type), new_var_type);
      new_exp_init = build_modify_expr
	(location, array_ind_value, TREE_TYPE (array_ind_value),
	 NOP_EXPR, location, func_parm, TREE_TYPE (func_parm));
      new_no_ind = build_modify_expr
	(location, *new_var, TREE_TYPE (*new_var), NOP_EXPR,
	 location, *new_var, TREE_TYPE (*new_var));
      new_no_expr = build_modify_expr
	(location, array_ind_value, TREE_TYPE (array_ind_value),
	 NOP_EXPR,
	 location, array_ind_value, TREE_TYPE (array_ind_value));
      if (list_size > 1)
	{
	  new_yes_ind = build_modify_expr
	    (location, *new_var, TREE_TYPE (*new_var), NOP_EXPR,
	     location, an_loop_info[0].var, TREE_TYPE (an_loop_info[0].var));
	  new_yes_expr = build_modify_expr
	    (location, array_ind_value, TREE_TYPE (array_ind_value),
	     NOP_EXPR,
	     location, func_parm, TREE_TYPE ((*array_operand)[0]));
	}
      else
	{
	  new_yes_ind = build_modify_expr
	    (location, *new_var, TREE_TYPE (*new_var), NOP_EXPR,
	     location, TREE_OPERAND (array_op0, 1),
	     TREE_TYPE (TREE_OPERAND (array_op0, 1)));
	  new_yes_expr = build_modify_expr
	    (location, array_ind_value, TREE_TYPE (array_ind_value),
	     NOP_EXPR,
	     location, func_parm, TREE_TYPE (TREE_OPERAND (array_op0, 1)));
	}
      new_yes_list = alloc_stmt_list ();
      append_to_statement_list (new_yes_ind, &new_yes_list);
      append_to_statement_list (new_yes_expr, &new_yes_list);

      new_no_list = alloc_stmt_list ();
      append_to_statement_list (new_no_ind, &new_no_list);
      append_to_statement_list (new_no_expr, &new_no_list);
 
      new_expr = build_conditional_expr
	(location,
	 build2 (LE_EXPR, TREE_TYPE (array_ind_value), array_ind_value,
		 func_parm),
	 false,
	 new_yes_list, TREE_TYPE (*new_var), new_no_list, TREE_TYPE (*new_var));
      break;
    case BUILT_IN_CILKPLUS_SEC_REDUCE_MIN_IND:
      new_var_init = build_modify_expr
	(location, *new_var, TREE_TYPE (*new_var), NOP_EXPR,
	 location, build_zero_cst (new_var_type), new_var_type);
      new_exp_init = build_modify_expr
	(location, array_ind_value, TREE_TYPE (array_ind_value),
	 NOP_EXPR, location, func_parm, TREE_TYPE (func_parm));
      new_no_ind = build_modify_expr
	(location, *new_var, TREE_TYPE (*new_var), NOP_EXPR,
	 location, *new_var, TREE_TYPE (*new_var));
      new_no_expr = build_modify_expr
	(location, array_ind_value, TREE_TYPE (array_ind_value),
	 NOP_EXPR,
	 location, array_ind_value, TREE_TYPE (array_ind_value));
      if (list_size > 1)
	{
	  new_yes_ind = build_modify_expr
	    (location, *new_var, TREE_TYPE (*new_var), NOP_EXPR,
	     location, an_loop_info[0].var, TREE_TYPE (an_loop_info[0].var));
	  new_yes_expr = build_modify_expr
	    (location, array_ind_value, TREE_TYPE (array_ind_value),
	     NOP_EXPR,
	     location, func_parm, TREE_TYPE (array_op0));
	}
      else
	{
	  new_yes_ind = build_modify_expr
	    (location, *new_var, TREE_TYPE (*new_var), NOP_EXPR,
	     location, TREE_OPERAND (array_op0, 1),
	     TREE_TYPE (TREE_OPERAND (array_op0, 1)));
	  new_yes_expr = build_modify_expr
	    (location, array_ind_value, TREE_TYPE (array_ind_value),
	     NOP_EXPR,
	     location, func_parm, TREE_TYPE (TREE_OPERAND (array_op0, 1)));
	}
      new_yes_list = alloc_stmt_list ();
      append_to_statement_list (new_yes_ind, &new_yes_list);
      append_to_statement_list (new_yes_expr, &new_yes_list);

      new_no_list = alloc_stmt_list ();
      append_to_statement_list (new_no_ind, &new_no_list);
      append_to_statement_list (new_no_expr, &new_no_list);
 
      new_expr = build_conditional_expr
	(location,
	 build2 (GE_EXPR, TREE_TYPE (array_ind_value), array_ind_value,
		 func_parm),
	 false,
	 new_yes_list, TREE_TYPE (*new_var), new_no_list, TREE_TYPE (*new_var));
      break;
    case BUILT_IN_CILKPLUS_SEC_REDUCE:
      new_var_init = build_modify_expr
	(location, *new_var, TREE_TYPE (*new_var), NOP_EXPR,
	 location, identity_value, new_var_type);
      new_call_expr = build_call_expr (call_fn, 2, *new_var, func_parm);
      new_expr = build_modify_expr
	(location, *new_var, TREE_TYPE (*new_var), NOP_EXPR,
	 location, new_call_expr, TREE_TYPE (*new_var));
      break;
    case BUILT_IN_CILKPLUS_SEC_REDUCE_MUTATING:
      new_expr = build_call_expr (call_fn, 2, identity_value, func_parm);
      break;
    default:
      gcc_unreachable ();
      break;
    }

  for (ii = 0; ii < rank; ii++)
    append_to_statement_list (an_loop_info[ii].ind_init, &loop_init);

  if (an_type == BUILT_IN_CILKPLUS_SEC_REDUCE_MAX_IND
      || an_type == BUILT_IN_CILKPLUS_SEC_REDUCE_MIN_IND)
    append_to_statement_list (new_exp_init, &loop_init);
  if (an_type != BUILT_IN_CILKPLUS_SEC_REDUCE_MUTATING)
    append_to_statement_list (new_var_init, &loop_init);

  append_to_statement_list_force (loop_init, &loop_with_init);
  body = new_expr;
  for (ii = 0; ii < rank; ii++)
    {
      tree new_loop = push_stmt_list ();
      c_finish_loop (location, an_loop_info[ii].cmp, an_loop_info[ii].incr,
		     body, NULL_TREE, NULL_TREE, true);
      body = pop_stmt_list (new_loop);
    }
  append_to_statement_list_force (body, &loop_with_init);

  release_vec_vec (an_info);
  
  return loop_with_init;
}

/* Returns a loop with ARRAY_REF inside it with an appropriate modify expr.
   The LHS and/or RHS will be array notation expressions that have a MODIFYCODE
   Their locations are specified by LHS_LOC, RHS_LOC.  The location of the
   modify expression is location.  The original type of LHS and RHS are passed
   in LHS_ORIGTYPE and RHS_ORIGTYPE.  */

tree
build_array_notation_expr (location_t location, tree lhs, tree lhs_origtype,
			   enum tree_code modifycode, location_t rhs_loc,
			   tree rhs, tree rhs_origtype)
{
  bool found_builtin_fn = false;
  tree array_expr_lhs = NULL_TREE, array_expr_rhs = NULL_TREE;
  tree array_expr = NULL_TREE;
  tree an_init = NULL_TREE;
  auto_vec<tree> cond_expr;
  tree body, loop_with_init = alloc_stmt_list();
  tree scalar_mods = NULL_TREE;
  vec<tree, va_gc> *rhs_array_operand = NULL, *lhs_array_operand = NULL;
  size_t lhs_rank = 0, rhs_rank = 0;
  size_t ii = 0;
  vec<tree, va_gc> *lhs_list = NULL, *rhs_list = NULL;
  tree new_modify_expr, new_var = NULL_TREE, builtin_loop = NULL_TREE;
  size_t rhs_list_size = 0, lhs_list_size = 0; 
  vec<vec<an_parts> > lhs_an_info = vNULL, rhs_an_info = vNULL;
  auto_vec<an_loop_parts> lhs_an_loop_info, rhs_an_loop_info;
  
  /* If either of this is true, an error message must have been send out
     already.  Not necessary to send out multiple error messages.  */
  if (lhs == error_mark_node || rhs == error_mark_node)
    return error_mark_node;
  
  if (!find_rank (location, rhs, rhs, false, &rhs_rank))
    return error_mark_node;
  
  extract_array_notation_exprs (rhs, false, &rhs_list);
  rhs_list_size = vec_safe_length (rhs_list);
  an_init = push_stmt_list ();
  if (rhs_rank)
    {
      scalar_mods = replace_invariant_exprs (&rhs);
      if (scalar_mods)
	add_stmt (scalar_mods);
    }
  for (ii = 0; ii < rhs_list_size; ii++)
    {
      tree rhs_node = (*rhs_list)[ii];
      if (TREE_CODE (rhs_node) == CALL_EXPR)
	{
	  builtin_loop = fix_builtin_array_notation_fn (rhs_node, &new_var);
	  if (builtin_loop == error_mark_node)
	    {
	      pop_stmt_list (an_init); 
	      return error_mark_node;
	    }
	  else if (builtin_loop)
	    {
	      add_stmt (builtin_loop);
	      found_builtin_fn = true;
	      if (new_var)
		{
		  vec<tree, va_gc> *rhs_sub_list = NULL, *new_var_list = NULL;
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
  if (!find_rank (location, lhs, lhs, true, &lhs_rank))
    {
      pop_stmt_list (an_init);
      return error_mark_node;
    }
  
  if (!find_rank (location, rhs, rhs, true, &rhs_rank))
    {
      pop_stmt_list (an_init);
      return error_mark_node;
    }

  if (lhs_rank == 0 && rhs_rank == 0)
    {
      if (found_builtin_fn)
	{
	  new_modify_expr = build_modify_expr (location, lhs, lhs_origtype,
					       modifycode, rhs_loc, rhs,
					       rhs_origtype);
	  add_stmt (new_modify_expr);
	  pop_stmt_list (an_init);	  
	  return an_init;
	}
      else
	{
	  pop_stmt_list (an_init);
	  return NULL_TREE;
	}
    }
  rhs_list_size = 0;
  rhs_list = NULL;
  extract_array_notation_exprs (rhs, true, &rhs_list);
  extract_array_notation_exprs (lhs, true, &lhs_list);
  rhs_list_size = vec_safe_length (rhs_list);
  lhs_list_size = vec_safe_length (lhs_list);
  
  if (lhs_rank == 0 && rhs_rank != 0)
    {
      tree rhs_base = rhs;
      if (TREE_CODE (rhs_base) == ARRAY_NOTATION_REF)
	{
	  for (ii = 0; ii < (size_t) rhs_rank; ii++)
	    rhs_base = ARRAY_NOTATION_ARRAY (rhs);
      
	  error_at (location, "%qE cannot be scalar when %qE is not", lhs,
		    rhs_base);
	  return error_mark_node;
	}
      else
	{
	  error_at (location, "%qE cannot be scalar when %qE is not", lhs,
		    rhs_base);
	  return error_mark_node;
	}
    }
  if (lhs_rank != 0 && rhs_rank != 0 && lhs_rank != rhs_rank)
    {
      error_at (location, "rank mismatch between %qE and %qE", lhs, rhs);
      pop_stmt_list (an_init);
      return error_mark_node;
    }
  
  /* Here we assign the array notation components to variable so that we can
     satisfy the exec once rule.  */
  for (ii = 0; ii < lhs_list_size; ii++)
    { 
      tree array_node = (*lhs_list)[ii];
      make_triplet_val_inv (location, &ARRAY_NOTATION_START (array_node));
      make_triplet_val_inv (location, &ARRAY_NOTATION_LENGTH (array_node));
      make_triplet_val_inv (location, &ARRAY_NOTATION_STRIDE (array_node));
    }
  for (ii = 0; ii < rhs_list_size; ii++)
    if ((*rhs_list)[ii] && TREE_CODE ((*rhs_list)[ii]) == ARRAY_NOTATION_REF)
      {  
	tree array_node = (*rhs_list)[ii];
	make_triplet_val_inv (location, &ARRAY_NOTATION_START (array_node));
	make_triplet_val_inv (location, &ARRAY_NOTATION_LENGTH (array_node));
	make_triplet_val_inv (location, &ARRAY_NOTATION_STRIDE (array_node));
      }
  
  cond_expr.safe_grow_cleared (MAX (lhs_rank, rhs_rank));

  lhs_an_loop_info.safe_grow_cleared (lhs_rank);
  if (rhs_rank)
    rhs_an_loop_info.safe_grow_cleared (rhs_rank);

  cilkplus_extract_an_triplets (lhs_list, lhs_list_size, lhs_rank,
				&lhs_an_info);
  if (rhs_rank)
    {
      rhs_an_loop_info.safe_grow_cleared (rhs_rank);
      cilkplus_extract_an_triplets (rhs_list, rhs_list_size, rhs_rank,
				    &rhs_an_info);
    }
  if (length_mismatch_in_expr_p (EXPR_LOCATION (lhs), lhs_an_info)
      || (rhs_rank
	  && length_mismatch_in_expr_p (EXPR_LOCATION (rhs), rhs_an_info)))
    {
      pop_stmt_list (an_init);
      goto error;
    }
  if (lhs_list_size > 0 && rhs_list_size > 0 && lhs_rank > 0 && rhs_rank > 0
      && TREE_CODE (lhs_an_info[0][0].length) == INTEGER_CST
      && rhs_an_info[0][0].length
      && TREE_CODE (rhs_an_info[0][0].length) == INTEGER_CST)
    {
      HOST_WIDE_INT l_length = int_cst_value (lhs_an_info[0][0].length);
      HOST_WIDE_INT r_length = int_cst_value (rhs_an_info[0][0].length);
      /* Length can be negative or positive.  As long as the magnitude is OK,
	 then the array notation is valid.  */
      if (absu_hwi (l_length) != absu_hwi (r_length))
	{
	  error_at (location, "length mismatch between LHS and RHS");
	  pop_stmt_list (an_init);
	  goto error;
	}
    }
  for (ii = 0; ii < lhs_rank; ii++)
    if (lhs_an_info[0][ii].is_vector)
      {
	lhs_an_loop_info[ii].var = create_tmp_var (integer_type_node);
	lhs_an_loop_info[ii].ind_init = build_modify_expr
	  (location, lhs_an_loop_info[ii].var,
	   TREE_TYPE (lhs_an_loop_info[ii].var), NOP_EXPR,
	   location, build_zero_cst (TREE_TYPE (lhs_an_loop_info[ii].var)),
	   TREE_TYPE (lhs_an_loop_info[ii].var));
      }
  for (ii = 0; ii < rhs_rank; ii++)
    {
      /* When we have a polynomial, we assume that the indices are of type 
	 integer.  */
      rhs_an_loop_info[ii].var = create_tmp_var (integer_type_node);
      rhs_an_loop_info[ii].ind_init = build_modify_expr
	(location, rhs_an_loop_info[ii].var,
	 TREE_TYPE (rhs_an_loop_info[ii].var), NOP_EXPR,
	 location, build_int_cst (TREE_TYPE (rhs_an_loop_info[ii].var), 0),
	 TREE_TYPE (rhs_an_loop_info[ii].var));
    }
  if (lhs_rank)
    {
      lhs_array_operand = create_array_refs
	(location, lhs_an_info, lhs_an_loop_info, lhs_list_size, lhs_rank);
      replace_array_notations (&lhs, true, lhs_list, lhs_array_operand);
      array_expr_lhs = lhs;
    }
  if (rhs_array_operand)
    vec_safe_truncate (rhs_array_operand, 0);
  if (rhs_rank)
    {
      rhs_array_operand = create_array_refs
	(location, rhs_an_info, rhs_an_loop_info, rhs_list_size, rhs_rank);
      replace_array_notations (&rhs, true, rhs_list, rhs_array_operand);
      vec_safe_truncate (rhs_array_operand, 0);
      rhs_array_operand = fix_sec_implicit_args (location, rhs_list,
						 rhs_an_loop_info, rhs_rank,
						 rhs);
      if (!rhs_array_operand)
	goto error;
      replace_array_notations (&rhs, true, rhs_list, rhs_array_operand);
    }
  else if (rhs_list_size > 0)
    {
      rhs_array_operand = fix_sec_implicit_args (location, rhs_list,
						 lhs_an_loop_info, lhs_rank,
						 lhs);
      if (!rhs_array_operand)
	goto error;
      replace_array_notations (&rhs, true, rhs_list, rhs_array_operand);
    }
  array_expr_lhs = lhs;
  array_expr_rhs = rhs;
  array_expr = build_modify_expr (location, array_expr_lhs, lhs_origtype, 
				  modifycode, rhs_loc, array_expr_rhs, 
				  rhs_origtype);
  create_cmp_incr (location, &lhs_an_loop_info, lhs_rank, lhs_an_info);
  if (rhs_rank)
    create_cmp_incr (location, &rhs_an_loop_info, rhs_rank, rhs_an_info);
  
  for (ii = 0; ii < MAX (lhs_rank, rhs_rank); ii++)
    if (ii < lhs_rank && ii < rhs_rank)
      cond_expr[ii] = build2 (TRUTH_ANDIF_EXPR, boolean_type_node,
			      lhs_an_loop_info[ii].cmp,
			      rhs_an_loop_info[ii].cmp);
    else if (ii < lhs_rank && ii >= rhs_rank)
      cond_expr[ii] = lhs_an_loop_info[ii].cmp;
    else
      gcc_unreachable ();

  an_init = pop_stmt_list (an_init);
  append_to_statement_list_force (an_init, &loop_with_init);
  body = array_expr;
  for (ii = 0; ii < MAX (lhs_rank, rhs_rank); ii++)
    {
      tree incr_list = alloc_stmt_list ();
      tree new_loop = push_stmt_list ();
      if (lhs_rank)
	add_stmt (lhs_an_loop_info[ii].ind_init);
      if (rhs_rank)
	add_stmt (rhs_an_loop_info[ii].ind_init);
      if (lhs_rank)
	append_to_statement_list_force (lhs_an_loop_info[ii].incr, &incr_list);
      if (rhs_rank && rhs_an_loop_info[ii].incr)
	append_to_statement_list_force (rhs_an_loop_info[ii].incr, &incr_list);
      c_finish_loop (location, cond_expr[ii], incr_list, body, NULL_TREE,
		     NULL_TREE, true);
      body = pop_stmt_list (new_loop);
    }
  append_to_statement_list_force (body, &loop_with_init);

  release_vec_vec (lhs_an_info);
  release_vec_vec (rhs_an_info);
  return loop_with_init;

error:
  release_vec_vec (lhs_an_info);
  release_vec_vec (rhs_an_info);

  return error_mark_node;
}

/* Helper function for fix_conditional_array_notations.  Encloses the 
   conditional statement passed in STMT with a loop around it
   and replaces the condition in STMT with a ARRAY_REF tree-node to the array.
   The condition must have an ARRAY_NOTATION_REF tree.  An expansion of array
   notation in STMT is returned in a STATEMENT_LIST.  */

static tree
fix_conditional_array_notations_1 (tree stmt)
{
  vec<tree, va_gc> *array_list = NULL, *array_operand = NULL;
  size_t list_size = 0;
  tree cond = NULL_TREE, builtin_loop = NULL_TREE, new_var = NULL_TREE;
  size_t rank = 0, ii = 0;
  tree loop_init;
  location_t location = EXPR_LOCATION (stmt);
  tree body = NULL_TREE, loop_with_init = alloc_stmt_list ();
  vec<vec<an_parts> > an_info = vNULL;
  auto_vec<an_loop_parts> an_loop_info;
 
  if (TREE_CODE (stmt) == COND_EXPR)
    cond = COND_EXPR_COND (stmt);
  else if (TREE_CODE (stmt) == SWITCH_EXPR)
    cond = SWITCH_COND (stmt);
  else if (truth_value_p (TREE_CODE (stmt)))
    cond = TREE_OPERAND (stmt, 0);
  else
    /* Otherwise dont even touch the statement.  */
    return stmt;

  if (!find_rank (location, cond, cond, false, &rank))
    return error_mark_node;
  
  extract_array_notation_exprs (stmt, false, &array_list);
  loop_init = push_stmt_list ();
  for (ii = 0; ii < vec_safe_length (array_list); ii++)
    { 
      tree array_node = (*array_list)[ii];
      if (TREE_CODE (array_node) == CALL_EXPR)
	{
	  builtin_loop = fix_builtin_array_notation_fn (array_node, &new_var);
	  if (builtin_loop == error_mark_node)
	    {
	      add_stmt (error_mark_node);
	      pop_stmt_list (loop_init);
	      return loop_init;
	    }
	  else if (builtin_loop)
	    {
	      vec <tree, va_gc>* sub_list = NULL, *new_var_list = NULL;
	      vec_safe_push (sub_list, array_node);
	      vec_safe_push (new_var_list, new_var);
	      add_stmt (builtin_loop);
	      replace_array_notations (&stmt, false, sub_list, new_var_list); 
	    }
	}
    }
  if (!find_rank (location, stmt, stmt, true, &rank))
    {
      pop_stmt_list (loop_init);
      return error_mark_node;
    }
  if (rank == 0)
    {
      add_stmt (stmt);
      pop_stmt_list (loop_init); 
      return loop_init;
    }  
  extract_array_notation_exprs (stmt, true, &array_list);

  if (vec_safe_length (array_list) == 0)
    return stmt;

  list_size = vec_safe_length (array_list);
  an_loop_info.safe_grow_cleared (rank);
  
  for (ii = 0; ii < list_size; ii++)
    if ((*array_list)[ii]
	&& TREE_CODE ((*array_list)[ii]) == ARRAY_NOTATION_REF)
      {
	tree array_node = (*array_list)[ii];
	make_triplet_val_inv (location, &ARRAY_NOTATION_START (array_node));
	make_triplet_val_inv (location, &ARRAY_NOTATION_LENGTH (array_node));
	make_triplet_val_inv (location, &ARRAY_NOTATION_STRIDE (array_node));
      }
  cilkplus_extract_an_triplets (array_list, list_size, rank, &an_info);
  for (ii = 0; ii < rank; ii++)
    {
      an_loop_info[ii].var = create_tmp_var (integer_type_node);
      an_loop_info[ii].ind_init =
	build_modify_expr (location, an_loop_info[ii].var,
			   TREE_TYPE (an_loop_info[ii].var), NOP_EXPR,
			   location,
			   build_int_cst (TREE_TYPE (an_loop_info[ii].var), 0),
			   TREE_TYPE (an_loop_info[ii].var));
    }
  array_operand = create_array_refs (location, an_info, an_loop_info,
				     list_size, rank);
  replace_array_notations (&stmt, true, array_list, array_operand);
  create_cmp_incr (location, &an_loop_info, rank, an_info);
  
  loop_init = pop_stmt_list (loop_init);
  body = stmt;
  append_to_statement_list_force (loop_init, &loop_with_init);

  for (ii = 0; ii < rank; ii++)
    {
      tree new_loop = push_stmt_list ();
      add_stmt (an_loop_info[ii].ind_init);
      c_finish_loop (location, an_loop_info[ii].cmp, an_loop_info[ii].incr,
		     body, NULL_TREE, NULL_TREE, true);
      body = pop_stmt_list (new_loop);
    }
  append_to_statement_list_force (body, &loop_with_init);
  release_vec_vec (an_info);

  return loop_with_init;
}

/* Top-level function to replace ARRAY_NOTATION_REF in a conditional statement
   in STMT.   An expansion of array notation in STMT is returned as a 
   STATEMENT_LIST.  */

tree
fix_conditional_array_notations (tree stmt)
{
  if (TREE_CODE (stmt) == STATEMENT_LIST)
    {
      tree_stmt_iterator tsi;
      for (tsi = tsi_start (stmt); !tsi_end_p (tsi); tsi_next (&tsi))
	{
	  tree single_stmt = *tsi_stmt_ptr (tsi);
	  *tsi_stmt_ptr (tsi) =
	    fix_conditional_array_notations_1 (single_stmt);
	}
      return stmt;
    }
  else
    return fix_conditional_array_notations_1 (stmt);
}

/* Create a struct c_expr that contains a loop with ARRAY_REF expr at location
   LOCATION with the tree_code CODE and the array notation expr is
   passed in ARG.  Returns the fixed c_expr in ARG itself.  */

struct c_expr 
fix_array_notation_expr (location_t location, enum tree_code code,
			 struct c_expr arg)
{

  vec<tree, va_gc> *array_list = NULL, *array_operand = NULL;
  size_t list_size = 0, rank = 0, ii = 0;
  tree loop_init;
  tree body, loop_with_init = alloc_stmt_list ();
  vec<vec<an_parts> > an_info = vNULL;
  auto_vec<an_loop_parts> an_loop_info;
  
  if (!find_rank (location, arg.value, arg.value, false, &rank))
    {
      /* If this function returns a NULL, we convert the tree value in the
	 structure to error_mark_node and the parser should take care of the
	 rest.  */
      arg.value = error_mark_node;
      return arg;
    }
  
  if (rank == 0)
    return arg;
  
  extract_array_notation_exprs (arg.value, true, &array_list);

  if (vec_safe_length (array_list) == 0)
    return arg;

  list_size = vec_safe_length (array_list);

  an_loop_info.safe_grow_cleared (rank);
  cilkplus_extract_an_triplets (array_list, list_size, rank, &an_info);
  
  loop_init = push_stmt_list ();
  for (ii = 0; ii < rank; ii++)
    {
      an_loop_info[ii].var = create_tmp_var (integer_type_node);
      an_loop_info[ii].ind_init =
	build_modify_expr (location, an_loop_info[ii].var,
			   TREE_TYPE (an_loop_info[ii].var), NOP_EXPR,
			   location,
			   build_int_cst (TREE_TYPE (an_loop_info[ii].var), 0),
			   TREE_TYPE (an_loop_info[ii].var));;
	
    }
  array_operand = create_array_refs (location, an_info, an_loop_info,
				     list_size, rank);
  replace_array_notations (&arg.value, true, array_list, array_operand);
  create_cmp_incr (location, &an_loop_info, rank, an_info);

  arg = default_function_array_read_conversion (location, arg);
  if (code == POSTINCREMENT_EXPR || code == POSTDECREMENT_EXPR)
    arg.value = build_unary_op (location, code, arg.value, false);
  else if (code == PREINCREMENT_EXPR || code == PREDECREMENT_EXPR)
    arg = parser_build_unary_op (location, code, arg);

  loop_init = pop_stmt_list (loop_init);
  append_to_statement_list_force (loop_init, &loop_with_init);
  body = arg.value;

  for (ii = 0; ii < rank; ii++)
    {
      tree new_loop = push_stmt_list ();
      add_stmt (an_loop_info[ii].ind_init);
      c_finish_loop (location, an_loop_info[ii].cmp,
		     an_loop_info[ii].incr, body, NULL_TREE,
		     NULL_TREE, true);
      body = pop_stmt_list (new_loop);
    }
  append_to_statement_list_force (body, &loop_with_init);
  arg.value = loop_with_init;
  release_vec_vec (an_info);
  return arg;
}

/* Replaces array notations in a void function call arguments in ARG and returns
   a STATEMENT_LIST.  */

static tree
fix_array_notation_call_expr (tree arg)
{
  vec<tree, va_gc> *array_list = NULL, *array_operand = NULL;
  tree new_var = NULL_TREE;
  size_t list_size = 0, rank = 0, ii = 0;
  tree loop_init;
  tree body, loop_with_init = alloc_stmt_list ();
  location_t location = UNKNOWN_LOCATION;
  vec<vec<an_parts> > an_info = vNULL;
  auto_vec<an_loop_parts> an_loop_info;

  if (TREE_CODE (arg) == CALL_EXPR
      && is_cilkplus_reduce_builtin (CALL_EXPR_FN (arg)))
    {
      loop_init = fix_builtin_array_notation_fn (arg, &new_var);
      /* We are ignoring the new var because either the user does not want to
	 capture it OR he is using sec_reduce_mutating function.  */
      return loop_init;
    }  
  if (!find_rank (location, arg, arg, false, &rank))
    return error_mark_node;
  
  if (rank == 0)
    return arg;
  
  extract_array_notation_exprs (arg, true, &array_list);
  if (vec_safe_length (array_list) == 0)
    return arg;
  
  list_size = vec_safe_length (array_list);
  location = EXPR_LOCATION (arg);
  an_loop_info.safe_grow_cleared (rank);
  
  loop_init = push_stmt_list ();
  for (ii = 0; ii < list_size; ii++)
    if ((*array_list)[ii]
	&& TREE_CODE ((*array_list)[ii]) == ARRAY_NOTATION_REF)
	{
	  tree array_node = (*array_list)[ii];
	  make_triplet_val_inv (location, &ARRAY_NOTATION_START (array_node));
	  make_triplet_val_inv (location, &ARRAY_NOTATION_LENGTH (array_node));
	  make_triplet_val_inv (location, &ARRAY_NOTATION_STRIDE (array_node));
	}
  cilkplus_extract_an_triplets (array_list, list_size, rank, &an_info);
  if (length_mismatch_in_expr_p (location, an_info))
    {
      pop_stmt_list (loop_init);
      return error_mark_node;
    }
  for (ii = 0; ii < rank; ii++)
    {
      an_loop_info[ii].var = create_tmp_var (integer_type_node);
      an_loop_info[ii].ind_init =
	build_modify_expr (location, an_loop_info[ii].var,
			   TREE_TYPE (an_loop_info[ii].var), NOP_EXPR, location,
			   build_int_cst (TREE_TYPE (an_loop_info[ii].var), 0),
			   TREE_TYPE (an_loop_info[ii].var));
	
    }
  array_operand = create_array_refs (location, an_info, an_loop_info,
				     list_size, rank);
  replace_array_notations (&arg, true, array_list, array_operand);
  create_cmp_incr (location, &an_loop_info, rank, an_info);
  loop_init = pop_stmt_list (loop_init);
  append_to_statement_list_force (loop_init, &loop_with_init);
  body = arg;
  for (ii = 0; ii < rank; ii++)
    {
      tree new_loop = push_stmt_list ();
      add_stmt (an_loop_info[ii].ind_init);
      c_finish_loop (location, an_loop_info[ii].cmp, an_loop_info[ii].incr,
		     body, NULL_TREE, NULL_TREE, true);
      body = pop_stmt_list (new_loop);
    }
  append_to_statement_list_force (body, &loop_with_init);
  release_vec_vec (an_info);
  return loop_with_init;
}

/* Expands the built-in functions in a return.  EXPR is a RETURN_EXPR with
   a built-in reduction function.  This function returns the expansion code for
   the built-in function.  */

static tree
fix_return_expr (tree expr)
{
  tree new_mod_list, new_var, new_mod, retval_expr, retval_type;
  location_t loc = EXPR_LOCATION (expr);

  new_mod_list = alloc_stmt_list ();
  retval_expr = TREE_OPERAND (expr, 0);
  retval_type = TREE_TYPE (TREE_OPERAND (retval_expr, 1));
  new_var = build_decl (loc, VAR_DECL, NULL_TREE, TREE_TYPE (retval_expr));
  new_mod = build_array_notation_expr (loc, new_var, TREE_TYPE (new_var),
				       NOP_EXPR, loc,
				       TREE_OPERAND (retval_expr, 1),
				       retval_type);
  TREE_OPERAND (retval_expr, 1) = new_var;
  TREE_OPERAND (expr, 0) = retval_expr;
  append_to_statement_list_force (new_mod, &new_mod_list);
  append_to_statement_list_force (expr, &new_mod_list);
  return new_mod_list;
}

/* Callback for walk_tree.  Expands all array notations in *TP.  *WALK_SUBTREES
   is set to 1 unless *TP contains no array notation expressions.  */

static tree
expand_array_notations (tree *tp, int *walk_subtrees, void *)
{
  if (!contains_array_notation_expr (*tp))
    {
      *walk_subtrees = 0;
      return NULL_TREE;
    }
  *walk_subtrees = 1;

  switch (TREE_CODE (*tp))
    {
    case TRUTH_ORIF_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_XOR_EXPR:
    case TRUTH_NOT_EXPR:
    case COND_EXPR:
      *tp = fix_conditional_array_notations (*tp);
      break;
    case MODIFY_EXPR:
      {
	location_t loc = EXPR_HAS_LOCATION (*tp) ? EXPR_LOCATION (*tp) :
	  UNKNOWN_LOCATION;
	tree lhs = TREE_OPERAND (*tp, 0);
	tree rhs = TREE_OPERAND (*tp, 1);
	location_t rhs_loc = EXPR_HAS_LOCATION (rhs) ? EXPR_LOCATION (rhs) :
	  UNKNOWN_LOCATION;
	*tp = build_array_notation_expr (loc, lhs, TREE_TYPE (lhs), NOP_EXPR,
					 rhs_loc, rhs, TREE_TYPE (rhs));
      }
      break;
    case DECL_EXPR:
      {
	tree x = DECL_EXPR_DECL (*tp);
	if (DECL_INITIAL (x))
	  {
	    location_t loc = DECL_SOURCE_LOCATION (x);
	    tree lhs = x;
	    tree rhs = DECL_INITIAL (x);
	    DECL_INITIAL (x) = NULL;
	    tree new_modify_expr = build_modify_expr (loc, lhs,
						      TREE_TYPE (lhs),
						      NOP_EXPR,
						      loc, rhs,
						      TREE_TYPE(rhs));
	    expand_array_notations (&new_modify_expr, walk_subtrees, NULL);
	    *tp = new_modify_expr;
	  }
      }
      break;
    case CALL_EXPR:
      *tp = fix_array_notation_call_expr (*tp);
      break;
    case RETURN_EXPR:
      *tp = fix_return_expr (*tp);
      break;
    case COMPOUND_EXPR:
      if (TREE_CODE (TREE_OPERAND (*tp, 0)) == SAVE_EXPR)
	{
	  /* In here we are calling expand_array_notations because
	     we need to be able to catch the return value and check if
	     it is an error_mark_node.  */
	  expand_array_notations (&TREE_OPERAND (*tp, 1), walk_subtrees, NULL);

	  /* SAVE_EXPR cannot have an error_mark_node inside it.  This check
	     will make sure that if there is an error in expanding of
	     array notations (e.g. rank mismatch) then replace the entire
	     SAVE_EXPR with an error_mark_node.  */
	  if (TREE_OPERAND (*tp, 1) == error_mark_node)
	    *tp = error_mark_node;
	}
      break;
    case ARRAY_NOTATION_REF:
      /* If we are here, then we are dealing with cases like this:
	 A[:];
	 A[x:y:z];
	 A[x:y];
	 Replace those with just void zero node.  */
      *tp = void_node;
    default:
      break;
    }
  return NULL_TREE;
} 

/* Walks through tree node T and expands all array notations in its subtrees.
   The return value is the same type as T but with all array notations 
   replaced with appropriate ARRAY_REFS with a loop around it.  */

tree
expand_array_notation_exprs (tree t)
{
  walk_tree (&t, expand_array_notations, NULL, NULL);
  return t;
}

/* This handles expression of the form "a[i:j:k]" or "a[:]" or "a[i:j]," which
   denotes an array notation expression.  If a is a variable or a member, then
   we generate a ARRAY_NOTATION_REF front-end tree and return it.
   This tree is broken down to ARRAY_REF toward the end of parsing.
   ARRAY_NOTATION_REF tree holds the START_INDEX, LENGTH, STRIDE and the TYPE
   of ARRAY_REF.  Restrictions on START_INDEX, LENGTH and STRIDE is same as that
   of the index field passed into ARRAY_REF.  The only additional restriction
   is that, unlike index in ARRAY_REF, stride, length and start_index cannot
   contain ARRAY_NOTATIONS.   */

tree
build_array_notation_ref (location_t loc, tree array, tree start_index, 
			  tree length, tree stride, tree type)
{
  tree array_ntn_tree = NULL_TREE;
  size_t stride_rank = 0, length_rank = 0, start_rank = 0;
  
  if (!INTEGRAL_TYPE_P (TREE_TYPE (start_index)))
    {
      error_at (loc,
		"start-index of array notation triplet is not an integer");
      return error_mark_node;
    }
  if (!INTEGRAL_TYPE_P (TREE_TYPE (length)))
    {
      error_at (loc, "length of array notation triplet is not an integer");
      return error_mark_node;
    }

  /* The stride is an optional field.  */
  if (stride && !INTEGRAL_TYPE_P (TREE_TYPE (stride)))
    {
      error_at (loc, "stride of array notation triplet is not an integer");
      return error_mark_node;
    }  
  if (!stride)
    {
      if (TREE_CONSTANT (start_index) && TREE_CONSTANT (length) 
	  && tree_int_cst_lt (length, start_index))
	stride = build_int_cst (TREE_TYPE (start_index), -1);
      else
	stride = build_int_cst (TREE_TYPE (start_index), 1);
    }	      

  if (!find_rank (loc, start_index, start_index, false, &start_rank))
    return error_mark_node;
  if (!find_rank (loc, length, length, false, &length_rank))
    return error_mark_node;
  if (!find_rank (loc, stride, stride, false, &stride_rank))
    return error_mark_node;

  if (start_rank != 0)
    {
      error_at (loc, "rank of an array notation triplet's start-index is not "
		"zero");
      return error_mark_node;
    }
  if (length_rank != 0)
    {
      error_at (loc, "rank of an array notation triplet's length is not zero");
      return error_mark_node;
    }
  if (stride_rank != 0)
    {
      error_at (loc, "rank of array notation triplet's stride is not zero");
      return error_mark_node;
    }  
  array_ntn_tree = build4 (ARRAY_NOTATION_REF, NULL_TREE, NULL_TREE, NULL_TREE,
			   NULL_TREE, NULL_TREE);
  ARRAY_NOTATION_ARRAY (array_ntn_tree) = array;
  ARRAY_NOTATION_START (array_ntn_tree) = start_index;
  ARRAY_NOTATION_LENGTH (array_ntn_tree) = length;
  ARRAY_NOTATION_STRIDE (array_ntn_tree) = stride;
  TREE_TYPE (array_ntn_tree) = type;
  
  return array_ntn_tree;
}
