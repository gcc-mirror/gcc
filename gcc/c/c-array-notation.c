/* This file is part of the Intel(R) Cilk(TM) Plus support
   This file contains routines to handle Array Notation expression
   handling routines in the C Compiler.
   Copyright (C) 2013  Free Software Foundation, Inc.
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
   3. Number of elements we need to acess (we call it length)
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
#include "tree.h"
#include "c-tree.h"
#include "tree-iterator.h"
#include "opts.h"
#include "c-family/c-common.h"

static void replace_array_notations (tree *, bool, vec<tree, va_gc> *,
				     vec<tree, va_gc> *);
static void extract_array_notation_exprs (tree, bool, vec<tree, va_gc> **);

/* This structure holds all the scalar values and its appropriate variable 
   replacment.  It is mainly used by the function that pulls all the invariant
   parts that should be executed only once, which comes with array notation 
   expressions.  */
struct inv_list
{
  vec<tree, va_gc> *list_values;
  vec<tree, va_gc> *replacement;
};

/* Returns true if there is length mismatch among expressions
   on the same dimension and on the same side of the equal sign.  The
   expressions (or ARRAY_NOTATION lengths) are passed in through 2-D array
   **LIST where X and Y indicate first and second dimension sizes of LIST,
   respectively.  */

static bool
length_mismatch_in_expr_p (location_t loc, tree **list, size_t x, size_t y)
{
  size_t ii, jj;
  tree start = NULL_TREE;
  HOST_WIDE_INT l_start, l_node;
  for (jj = 0; jj < y; jj++)
    {
      start = NULL_TREE;
      for (ii = 0; ii < x; ii++)
	{
	  if (!start)
	    start = list[ii][jj];
	  else if (TREE_CODE (start) == INTEGER_CST)
	    {
	      /* If start is a INTEGER, and list[ii][jj] is an integer then
		 check if they are equal.  If they are not equal then return
		 true.  */
	      if (TREE_CODE (list[ii][jj]) == INTEGER_CST)
		{
		  l_node = int_cst_value (list[ii][jj]);
		  l_start = int_cst_value (start);
		  if (absu_hwi (l_start) != absu_hwi (l_node))
		    {
		      error_at (loc, "length mismatch in expression");
		      return true;
		    }
		}
	    }
	  else
	    /* We set the start node as the current node just in case it turns
	       out to be an integer.  */
	    start = list[ii][jj];
	}
    }
  return false;
}


/* Given an FNDECL of type FUNCTION_DECL or ADDR_EXPR, return the corresponding
   BUILT_IN_CILKPLUS_SEC_REDUCE_* being called.  If none, return
   BUILT_IN_NONE.  */

enum built_in_function
is_cilkplus_reduce_builtin (tree fndecl)
{
  if (TREE_CODE (fndecl) == ADDR_EXPR)
    fndecl = TREE_OPERAND (fndecl, 0);

  if (TREE_CODE (fndecl) == FUNCTION_DECL
      && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL)
    switch (DECL_FUNCTION_CODE (fndecl))
      {
      case BUILT_IN_CILKPLUS_SEC_REDUCE_ADD:
      case BUILT_IN_CILKPLUS_SEC_REDUCE_MUL:
      case BUILT_IN_CILKPLUS_SEC_REDUCE_ALL_ZERO:
      case BUILT_IN_CILKPLUS_SEC_REDUCE_ANY_ZERO:
      case BUILT_IN_CILKPLUS_SEC_REDUCE_MAX:
      case BUILT_IN_CILKPLUS_SEC_REDUCE_MIN:
      case BUILT_IN_CILKPLUS_SEC_REDUCE_MIN_IND:
      case BUILT_IN_CILKPLUS_SEC_REDUCE_MAX_IND:
      case BUILT_IN_CILKPLUS_SEC_REDUCE_ANY_NONZERO:
      case BUILT_IN_CILKPLUS_SEC_REDUCE_ALL_NONZERO:
      case BUILT_IN_CILKPLUS_SEC_REDUCE:
      case BUILT_IN_CILKPLUS_SEC_REDUCE_MUTATING:
	return DECL_FUNCTION_CODE (fndecl);
      default:
	break;
      }

  return BUILT_IN_NONE;
}

/* This function will recurse into EXPR finding any
   ARRAY_NOTATION_EXPRs and calculate the overall rank of EXPR,
   storing it in *RANK. LOC is the location of the original expression.

   ORIG_EXPR is the original expression used to display if any rank
   mismatch errors are found.

   Upon entry, *RANK must be either 0, or the rank of a parent
   expression that must have the same rank as the one being
   calculated.  It is illegal to have multiple array notation with different
   rank in the same expression (see examples below for clarification).

   If there were any rank mismatches while calculating the rank, an
   error will be issued, and FALSE will be returned.  Otherwise, TRUE
   is returned.  

   If IGNORE_BUILTIN_FN is TRUE, ignore array notation specific
   built-in functions (__sec_reduce_*, etc).

   Here are some examples of array notations and their rank:

   Expression			    RANK
   5				    0
   X (a variable)		    0
   *Y (a pointer)		    0
   A[5]				    0
   B[5][10]			    0
   A[:]				    1 
   B[0:10]			    1
   C[0:10:2]			    1
   D[5][0:10:2]			    1 (since D[5] is considered "scalar")
   D[5][:][10]			    1 
   E[:] + 5			    1 
   F[:][:][:] + 5 + X		    3
   F[:][:][:] + E[:] + 5 + X	    RANKMISMATCH-ERROR since rank (E[:]) = 1 and
                                    rank (F[:][:][:]) = 3.  They must be equal 
				    or have a rank of zero.
   F[:][5][10] + E[:] * 5 + *Y      1

   int func (int);
   func (A[:])			    1
   func (B[:][:][:][:])             4 
   
   int func2 (int, int)
   func2 (A[:], B[:][:][:][:])	    RANKMISMATCH-ERROR -- Since Rank (A[:]) = 1 
				    and Rank (B[:][:][:][:]) = 4

   A[:] + func (B[:][:][:][:])	    RANKMISMATCH-ERROR
   func2 (A[:], B[:]) + func (A)    1 

 */

bool
find_rank (location_t loc, tree orig_expr, tree expr, bool ignore_builtin_fn,
	   size_t *rank)
{
  tree ii_tree;
  size_t ii = 0, current_rank = 0;
 
  if (TREE_CODE (expr) == ARRAY_NOTATION_REF)
    {
      ii_tree = expr;
      while (ii_tree)
	{
	  if (TREE_CODE (ii_tree) == ARRAY_NOTATION_REF)
	    {
	      current_rank++;
	      ii_tree = ARRAY_NOTATION_ARRAY (ii_tree);
	    }
	  else if (TREE_CODE (ii_tree) == ARRAY_REF)
	    ii_tree = TREE_OPERAND (ii_tree, 0);
	  else if (TREE_CODE (ii_tree) == PARM_DECL
		   || TREE_CODE (ii_tree) == VAR_DECL)
	    break;
	}
      if (*rank == 0)
	/* In this case, all the expressions this function has encountered thus
	   far have been scalars or expressions with zero rank.  Please see
	   header comment for examples of such expression.  */
	*rank = current_rank;
      else if (*rank != current_rank)
	{
	  /* In this case, find rank is being recursed through a set of 
	     expression of the form A <OPERATION> B, where A and B both have
	     array notations in them and the rank of A is not equal to rank of
	     B.  
	     A simple example of such case is the following: X[:] + Y[:][:] */ 
	  *rank = current_rank;
	  return false;
	}
    }
  else if (TREE_CODE (expr) == STATEMENT_LIST)
    {
      tree_stmt_iterator ii_tsi;
      for (ii_tsi = tsi_start (expr); !tsi_end_p (ii_tsi);
	   tsi_next (&ii_tsi))
	if (!find_rank (loc, orig_expr, *tsi_stmt_ptr (ii_tsi),
			ignore_builtin_fn, rank))
	  return false;
    }
  else
    {
      if (TREE_CODE (expr) == CALL_EXPR)
	{
	  tree func_name = CALL_EXPR_FN (expr);
	  tree prev_arg = NULL_TREE, arg;
	  call_expr_arg_iterator iter;
	  size_t prev_rank = 0;
	  if (TREE_CODE (func_name) == ADDR_EXPR)
	    if (!ignore_builtin_fn)
	      if (is_cilkplus_reduce_builtin (func_name))
		/* If it is a built-in function, then we know it returns a 
		   scalar.  */
		return true;
	  FOR_EACH_CALL_EXPR_ARG (arg, iter, expr)
	    {
	      if (!find_rank (loc, orig_expr, arg, ignore_builtin_fn, rank))
		{
		  if (prev_arg && EXPR_HAS_LOCATION (prev_arg)
		      && prev_rank != *rank)
		    error_at (EXPR_LOCATION (prev_arg),
			      "rank mismatch between %qE and %qE", prev_arg,
			      arg);
		  else if (prev_arg && prev_rank != *rank)
		    /* Here the original expression is printed as a "heads-up"
		       to the programmer.  This is because since there is no 
		       location information for the offending argument, the 
		       error could be in some internally generated code that is
		       not visible for the programmer.  Thus, the correct fix
		       may lie in the original expression.  */
		    error_at (loc, "rank mismatch in expression %qE",
			      orig_expr);
		  return false;
		}
	      prev_arg = arg;
	      prev_rank = *rank;
	    }	
	}
      else
	{
	  tree prev_arg = NULL_TREE;
	  for (ii = 0; ii < TREE_CODE_LENGTH (TREE_CODE (expr)); ii++)
	    {
	      if (TREE_OPERAND (expr, ii)
		  && !find_rank (loc, orig_expr, TREE_OPERAND (expr, ii),
				 ignore_builtin_fn, rank))
		{
		  if (prev_arg && EXPR_HAS_LOCATION (prev_arg))
		    error_at (EXPR_LOCATION (prev_arg),
			      "rank mismatch between %qE and %qE", prev_arg,
			      TREE_OPERAND (expr, ii));
		  else if (prev_arg)
		    error_at (loc, "rank mismatch in expression %qE",
			      orig_expr);
		  return false;
		}
	      prev_arg = TREE_OPERAND (expr, ii);
	    }
	}
    }
  return true;
}

/* Extracts all array notations in NODE and stores them in ARRAY_LIST.  If 
   IGNORE_BUILTIN_FN is set, then array notations inside array notation
   specific built-in functions are ignored.  The NODE can be constants,
   VAR_DECL, PARM_DECLS, STATEMENT_LISTS or full expressions.   */

static void
extract_array_notation_exprs (tree node, bool ignore_builtin_fn,
			      vec<tree, va_gc> **array_list)
{
  size_t ii = 0;  
  if (TREE_CODE (node) == ARRAY_NOTATION_REF)
    {
      vec_safe_push (*array_list, node);
      return;
    }
  else if (TREE_CODE (node) == STATEMENT_LIST)
    {
      tree_stmt_iterator ii_tsi;
      for (ii_tsi = tsi_start (node); !tsi_end_p (ii_tsi); tsi_next (&ii_tsi))
	extract_array_notation_exprs (*tsi_stmt_ptr (ii_tsi),
				      ignore_builtin_fn, array_list);
    }
  else if (TREE_CODE (node) == CALL_EXPR)
    {
      tree arg;
      call_expr_arg_iterator iter;
      if (is_cilkplus_reduce_builtin (CALL_EXPR_FN (node)))
	{
	  if (ignore_builtin_fn)
	    return;
	  else
	    {
	      vec_safe_push (*array_list, node);
	      return;
	    }
	}
      if (is_sec_implicit_index_fn (CALL_EXPR_FN (node)))
	{
	  vec_safe_push (*array_list, node);
	  return;
	}
      FOR_EACH_CALL_EXPR_ARG (arg, iter, node)
	extract_array_notation_exprs (arg, ignore_builtin_fn, array_list);
    } 
  else 
    for (ii = 0; ii < TREE_CODE_LENGTH (TREE_CODE (node)); ii++) 
      if (TREE_OPERAND (node, ii))
	extract_array_notation_exprs (TREE_OPERAND (node, ii),
				      ignore_builtin_fn, array_list);
  return;
}

/* LIST contains all the array notations found in *ORIG and ARRAY_OPERAND
   contains the expanded ARRAY_REF.  E.g., if LIST[<some_index>] contains
   an array_notation expression, then ARRAY_OPERAND[<some_index>] contains its
   expansion.  If *ORIG matches LIST[<some_index>] then *ORIG is set to
   ARRAY_OPERAND[<some_index>].  This function recursively steps through
   all the sub-trees of *ORIG, if it is larger than a single
   ARRAY_NOTATION_REF.  */

static void
replace_array_notations (tree *orig, bool ignore_builtin_fn,
			 vec<tree, va_gc> *list,
			 vec<tree, va_gc> *array_operand)
{
  size_t ii = 0;
  tree node = NULL_TREE, node_replacement = NULL_TREE;
  
  if (vec_safe_length (list) == 0)
    return;

  if (TREE_CODE (*orig) == ARRAY_NOTATION_REF)
    {
      for (ii = 0; vec_safe_iterate (list, ii, &node); ii++) 
	if (*orig == node)
	  {
	    node_replacement = (*array_operand)[ii];
	    *orig = node_replacement;
	  }
    }
  else if (TREE_CODE (*orig) == STATEMENT_LIST)
    {
      tree_stmt_iterator ii_tsi;
      for (ii_tsi = tsi_start (*orig); !tsi_end_p (ii_tsi); tsi_next (&ii_tsi))
	replace_array_notations (tsi_stmt_ptr (ii_tsi), ignore_builtin_fn, list,
				 array_operand);
    }
  else if (TREE_CODE (*orig) == CALL_EXPR)
    {
      tree arg;
      call_expr_arg_iterator iter;
      if (is_cilkplus_reduce_builtin (CALL_EXPR_FN (*orig)))
	{
	  if (!ignore_builtin_fn)
	    {
	      for (ii = 0; vec_safe_iterate (list, ii, &node); ii++) 
		if (*orig == node)
		  {
		    node_replacement = (*array_operand)[ii];
		    *orig = node_replacement;
		  }
	    }
	  return;
	}
      if (is_sec_implicit_index_fn (CALL_EXPR_FN (*orig)))
	{
	  for (ii = 0; vec_safe_iterate (list, ii, &node); ii++)
	    if (*orig == node)
	      {
		node_replacement = (*array_operand)[ii];
		*orig = node_replacement;
	      }
	  return;
	}
      ii = 0;
      FOR_EACH_CALL_EXPR_ARG (arg, iter, *orig)
	{
	  replace_array_notations (&arg, ignore_builtin_fn, list,
				   array_operand);
	  CALL_EXPR_ARG (*orig, ii) = arg;
	  ii++;
	}     
    }
  else
    {
      for (ii = 0; ii < (size_t) TREE_CODE_LENGTH (TREE_CODE (*orig)); ii++) 
	if (TREE_OPERAND (*orig, ii))
	  replace_array_notations (&TREE_OPERAND (*orig, ii), ignore_builtin_fn,
				   list, array_operand);
    }
  return;
}

/* Callback for walk_tree.  Find all the scalar expressions in *TP and push 
   them in DATA struct, typecasted to (void *).  If *WALK_SUBTREES is set to 0 
   then do not go into the *TP's subtrees.  Since this function steps through 
   all the subtrees, *TP and TP can be NULL_TREE and NULL, respectively.  The 
   function returns NULL_TREE unconditionally.  */

static tree
find_inv_trees (tree *tp, int *walk_subtrees, void *data)
{
  struct inv_list *i_list = (struct inv_list *) data;

  if (!tp || !*tp)
    return NULL_TREE;
  if (TREE_CONSTANT (*tp))
    return NULL_TREE; /* No need to save constant to a variable.  */
  if (TREE_CODE (*tp) != COMPOUND_EXPR && !contains_array_notation_expr (*tp))
    {
      vec_safe_push (i_list->list_values, *tp);
      *walk_subtrees = 0;
    }
  else if (TREE_CODE (*tp) == ARRAY_NOTATION_REF
	   || TREE_CODE (*tp) == ARRAY_REF
	   || TREE_CODE (*tp) == CALL_EXPR)
    /* No need to step through the internals of array notation.  */
    *walk_subtrees = 0;
  else
    *walk_subtrees = 1;
  return NULL_TREE;
}

/* Callback for walk_tree.  Replace all the scalar expressions in *TP with the 
   appropriate replacement stored in the struct *DATA (typecasted to void*).  
   The subtrees are not touched if *WALK_SUBTREES is set to zero.  */

static tree
replace_inv_trees (tree *tp, int *walk_subtrees, void *data)
{
  size_t ii = 0;
  tree t, r;
  struct inv_list *i_list = (struct inv_list *) data;

  if (vec_safe_length (i_list->list_values))
    {
      for (ii = 0; vec_safe_iterate (i_list->list_values, ii, &t); ii++)
	if (simple_cst_equal (*tp, t) == 1)
	  {
	    vec_safe_iterate (i_list->replacement, ii, &r);
	    gcc_assert (r != NULL_TREE);
	    *tp = r;
	    *walk_subtrees = 0;
	  }
    }
  else
    *walk_subtrees = 0;
  return NULL_TREE;
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
  size_t list_size = 0, rank = 0, ii = 0, jj = 0;
  int s_jj = 0;
  tree **array_ops, *array_var, jj_tree, loop_init, array_op0;
  tree **array_value, **array_stride, **array_length, **array_start;
  tree *compare_expr, *expr_incr, *ind_init;
  tree identity_value = NULL_TREE, call_fn = NULL_TREE, new_call_expr, body;
  bool **count_down, **array_vector;
  location_t location = UNKNOWN_LOCATION;
  tree loop_with_init = alloc_stmt_list ();
  
  enum built_in_function an_type =
    is_cilkplus_reduce_builtin (CALL_EXPR_FN (an_builtin_fn));
  if (an_type == BUILT_IN_NONE)
    return NULL_TREE;

  if (an_type == BUILT_IN_CILKPLUS_SEC_REDUCE
      || an_type == BUILT_IN_CILKPLUS_SEC_REDUCE_MUTATING)
    {
      call_fn = CALL_EXPR_ARG (an_builtin_fn, 2);
      while (TREE_CODE (call_fn) == CONVERT_EXPR
	     || TREE_CODE (call_fn) == NOP_EXPR)
	call_fn = TREE_OPERAND (call_fn, 0);
      call_fn = TREE_OPERAND (call_fn, 0);
      
      identity_value = CALL_EXPR_ARG (an_builtin_fn, 0);
      while (TREE_CODE (identity_value) == CONVERT_EXPR
	     || TREE_CODE (identity_value) == NOP_EXPR)
	identity_value = TREE_OPERAND (identity_value, 0);
      func_parm = CALL_EXPR_ARG (an_builtin_fn, 1);
    }
  else
    func_parm = CALL_EXPR_ARG (an_builtin_fn, 0);
  
  while (TREE_CODE (func_parm) == CONVERT_EXPR
	 || TREE_CODE (func_parm) == EXCESS_PRECISION_EXPR
	 || TREE_CODE (func_parm) == NOP_EXPR)
    func_parm = TREE_OPERAND (func_parm, 0);

  location = EXPR_LOCATION (an_builtin_fn);
  
  if (!find_rank (location, an_builtin_fn, an_builtin_fn, true, &rank))
    return error_mark_node;
 
  if (rank == 0)
    return an_builtin_fn;
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
  
  array_ops = XNEWVEC (tree *, list_size);
  for (ii = 0; ii < list_size; ii++)
    array_ops[ii] = XNEWVEC (tree, rank);
  
  array_vector = XNEWVEC (bool *, list_size);
  for (ii = 0; ii < list_size; ii++)
    array_vector[ii] = XNEWVEC (bool, rank);

  array_value = XNEWVEC (tree *, list_size);
  array_stride = XNEWVEC (tree *, list_size);
  array_length = XNEWVEC (tree *, list_size);
  array_start = XNEWVEC (tree *, list_size);

  for (ii = 0; ii < list_size; ii++)
    {
      array_value[ii]  = XNEWVEC (tree, rank);
      array_stride[ii] = XNEWVEC (tree, rank);
      array_length[ii] = XNEWVEC (tree, rank);
      array_start[ii]  = XNEWVEC (tree, rank);
    }

  compare_expr = XNEWVEC (tree, rank);
  expr_incr = XNEWVEC (tree,  rank);
  ind_init = XNEWVEC (tree, rank);
  
  count_down = XNEWVEC (bool *, list_size);
  for (ii = 0; ii < list_size; ii++)
    count_down[ii] = XNEWVEC (bool,  rank);
  
  array_var = XNEWVEC (tree, rank);

  for (ii = 0; ii < list_size; ii++)
    {
      jj = 0;
      for (jj_tree = (*array_list)[ii];
	   jj_tree && TREE_CODE (jj_tree) == ARRAY_NOTATION_REF;
	   jj_tree = ARRAY_NOTATION_ARRAY (jj_tree))
	{
	  array_ops[ii][jj] = jj_tree;
	  jj++;
	}
    }

  for (ii = 0; ii < list_size; ii++)
    {
      tree array_node = (*array_list)[ii];
      if (TREE_CODE (array_node) == ARRAY_NOTATION_REF)
	{
	  for (jj = 0; jj < rank; jj++)
	    {
	      if (TREE_CODE (array_ops[ii][jj]) == ARRAY_NOTATION_REF)
		{
		  array_value[ii][jj] =
		    ARRAY_NOTATION_ARRAY (array_ops[ii][jj]);
		  array_start[ii][jj] =
		    ARRAY_NOTATION_START (array_ops[ii][jj]);
		  array_length[ii][jj] =
		    fold_build1 (CONVERT_EXPR, integer_type_node,
				 ARRAY_NOTATION_LENGTH (array_ops[ii][jj]));
		  array_stride[ii][jj] =
		    fold_build1 (CONVERT_EXPR, integer_type_node,
				 ARRAY_NOTATION_STRIDE (array_ops[ii][jj]));
		  array_vector[ii][jj] = true;

		  if (!TREE_CONSTANT (array_length[ii][jj]))
		    count_down[ii][jj] = false;
		  else if (tree_int_cst_lt
			   (array_length[ii][jj],
			    build_int_cst (TREE_TYPE (array_length[ii][jj]),
					   0)))
		    count_down[ii][jj] = true;
		  else
		    count_down[ii][jj] = false;
		}
	      else
		array_vector[ii][jj] = false;
	    }
	}
    }

  loop_init = alloc_stmt_list ();

  for (ii = 0; ii < rank; ii++)
    {
      array_var[ii] = build_decl (location, VAR_DECL, NULL_TREE,
				  integer_type_node);
      ind_init[ii] =
	build_modify_expr (location, array_var[ii],
			   TREE_TYPE (array_var[ii]), NOP_EXPR,
			   location,
			   build_int_cst (TREE_TYPE (array_var[ii]), 0),
			   TREE_TYPE (array_var[ii]));	
    }
  for (ii = 0; ii < list_size; ii++)
    {
      if (array_vector[ii][0])
	{
	  tree array_opr_node  = array_value[ii][rank - 1];
	  for (s_jj = rank - 1; s_jj >= 0; s_jj--)
	    {
	      if (count_down[ii][s_jj])
		{
		  /* Array[start_index - (induction_var * stride)] */
		  array_opr_node = build_array_ref
		    (location, array_opr_node,
		     build2 (MINUS_EXPR, TREE_TYPE (array_var[s_jj]),
			     array_start[ii][s_jj],
			     build2 (MULT_EXPR, TREE_TYPE (array_var[s_jj]),
				     array_var[s_jj], array_stride[ii][s_jj])));
		}
	      else
		{
		  /* Array[start_index + (induction_var * stride)] */
		  array_opr_node = build_array_ref
		    (location, array_opr_node,
		     build2 (PLUS_EXPR, TREE_TYPE (array_var[s_jj]),
			     array_start[ii][s_jj],
			     build2 (MULT_EXPR, TREE_TYPE (array_var[s_jj]),
				     array_var[s_jj], array_stride[ii][s_jj])));
		}
	    }
	  vec_safe_push (array_operand, array_opr_node);
	}
      else
	/* This is just a dummy node to make sure the list sizes for both
	   array list and array operand list are the same.  */
	vec_safe_push (array_operand, integer_one_node);
    }
  replace_array_notations (&func_parm, true, array_list, array_operand);
  for (ii = 0; ii < rank; ii++)
    expr_incr[ii] =
      build2 (MODIFY_EXPR, void_type_node, array_var[ii],
	      build2 (PLUS_EXPR, TREE_TYPE (array_var[ii]), array_var[ii],
		      build_int_cst (TREE_TYPE (array_var[ii]), 1)));
  for (jj = 0; jj < rank; jj++)
    {
      if (rank && expr_incr[jj])
	{
	  if (count_down[0][jj])
	    compare_expr[jj] =
	      build2 (LT_EXPR, boolean_type_node, array_var[jj],
		      build2 (MULT_EXPR, TREE_TYPE (array_var[jj]),
			      array_length[0][jj],
			      build_int_cst (TREE_TYPE (array_var[jj]), -1)));
	  else
	    compare_expr[jj] = build2 (LT_EXPR, boolean_type_node,
				       array_var[jj], array_length[0][jj]);
	}
    }

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
	     location, array_var[0], TREE_TYPE (array_var[0]));
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
	     location, func_parm, TREE_OPERAND (array_op0, 1));
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
	     location, array_var[0], TREE_TYPE (array_var[0]));
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
	     location, func_parm, TREE_OPERAND (array_op0, 1));
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
    append_to_statement_list (ind_init [ii], &loop_init);

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
      c_finish_loop (location, compare_expr[ii], expr_incr[ii], body, NULL_TREE,
		     NULL_TREE, true);
      body = pop_stmt_list (new_loop);
    }
  append_to_statement_list_force (body, &loop_with_init);
  
  XDELETEVEC (compare_expr);
  XDELETEVEC (expr_incr);
  XDELETEVEC (ind_init);
  XDELETEVEC (array_var);  
  for (ii = 0; ii < list_size; ii++)
    {
      XDELETEVEC (count_down[ii]);
      XDELETEVEC (array_value[ii]);
      XDELETEVEC (array_stride[ii]);
      XDELETEVEC (array_length[ii]);
      XDELETEVEC (array_start[ii]);
      XDELETEVEC (array_ops[ii]);
      XDELETEVEC (array_vector[ii]);
    }
  XDELETEVEC (count_down);
  XDELETEVEC (array_value);
  XDELETEVEC (array_stride);
  XDELETEVEC (array_length);
  XDELETEVEC (array_start);
  XDELETEVEC (array_ops);
  XDELETEVEC (array_vector);
  
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
  bool **lhs_vector = NULL, **rhs_vector = NULL, found_builtin_fn = false;
  tree **lhs_array = NULL, **rhs_array = NULL;
  tree array_expr_lhs = NULL_TREE, array_expr_rhs = NULL_TREE;
  tree array_expr = NULL_TREE;
  tree **lhs_value = NULL, **rhs_value = NULL;
  tree **lhs_stride = NULL, **lhs_length = NULL, **lhs_start = NULL;
  tree **rhs_stride = NULL, **rhs_length = NULL, **rhs_start = NULL;
  tree an_init = NULL_TREE, *lhs_var = NULL, *rhs_var = NULL;
  tree *cond_expr = NULL;
  tree body, loop_with_init = alloc_stmt_list();
  tree scalar_mods = NULL_TREE;
  tree *lhs_expr_incr = NULL, *rhs_expr_incr = NULL;
  tree *lhs_ind_init = NULL, *rhs_ind_init = NULL;
  bool **lhs_count_down = NULL, **rhs_count_down = NULL;
  tree *lhs_compare = NULL, *rhs_compare = NULL;
  vec<tree, va_gc> *rhs_array_operand = NULL, *lhs_array_operand = NULL;
  size_t lhs_rank = 0, rhs_rank = 0;
  size_t ii = 0, jj = 0;
  int s_jj = 0;
  tree ii_tree = NULL_TREE, new_modify_expr;
  vec<tree, va_gc> *lhs_list = NULL, *rhs_list = NULL;
  tree new_var = NULL_TREE, builtin_loop = NULL_TREE;
  tree begin_var, lngth_var, strde_var;
  size_t rhs_list_size = 0, lhs_list_size = 0;

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
  
  if (lhs_rank == 0 && rhs_rank != 0 && TREE_CODE (rhs) != CALL_EXPR)
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
      tree lhs_base = lhs;
      tree rhs_base = rhs;
      
      for (ii = 0; ii < lhs_rank; ii++)
	lhs_base = ARRAY_NOTATION_ARRAY (lhs_base);

      while (rhs_base && TREE_CODE (rhs_base) != ARRAY_NOTATION_REF)
	rhs_base = TREE_OPERAND (rhs_base, 0);
      for (ii = 0; ii < rhs_rank; ii++)
	rhs_base = ARRAY_NOTATION_ARRAY (rhs_base);
      
      error_at (location, "rank mismatch between %qE and %qE", lhs, rhs);
      pop_stmt_list (an_init);
      return error_mark_node;
    }
  
  /* Here we assign the array notation components to variable so that we can
     satisfy the exec once rule.  */
  for (ii = 0; ii < lhs_list_size; ii++)
    {
      tree array_node = (*lhs_list)[ii];
      tree array_begin = ARRAY_NOTATION_START (array_node);
      tree array_lngth = ARRAY_NOTATION_LENGTH (array_node);
      tree array_strde = ARRAY_NOTATION_STRIDE (array_node);

      if (TREE_CODE (array_begin) != INTEGER_CST)
	{
	  begin_var = build_decl (location, VAR_DECL, NULL_TREE,
				  integer_type_node);
	  add_stmt (build_modify_expr (location, begin_var,
				       TREE_TYPE (begin_var),
				       NOP_EXPR, location, array_begin,
				       TREE_TYPE (array_begin)));      
	  ARRAY_NOTATION_START (array_node) = begin_var;
	}

      if (TREE_CODE (array_lngth) != INTEGER_CST)
	{
	  lngth_var = build_decl (location, VAR_DECL, NULL_TREE,
				  integer_type_node);
	  add_stmt (build_modify_expr (location, lngth_var,
				       TREE_TYPE (lngth_var),
				       NOP_EXPR, location, array_lngth,
				       TREE_TYPE (array_lngth)));
	  ARRAY_NOTATION_LENGTH (array_node) = lngth_var;
	}
      if (TREE_CODE (array_strde) != INTEGER_CST)
	{
	  strde_var = build_decl (location, VAR_DECL, NULL_TREE,
				  integer_type_node);

	  add_stmt (build_modify_expr (location, strde_var,
				       TREE_TYPE (strde_var),
				       NOP_EXPR, location, array_strde,
				       TREE_TYPE (array_strde)));
	  ARRAY_NOTATION_STRIDE (array_node) = strde_var;
	}
    }
  for (ii = 0; ii < rhs_list_size; ii++)
    {
      tree array_node = (*rhs_list)[ii];
      if (array_node && TREE_CODE (array_node) == ARRAY_NOTATION_REF)
	{
	  tree array_begin = ARRAY_NOTATION_START (array_node);
	  tree array_lngth = ARRAY_NOTATION_LENGTH (array_node);
	  tree array_strde = ARRAY_NOTATION_STRIDE (array_node);

	  if (TREE_CODE (array_begin) != INTEGER_CST)
	    {
	      begin_var = build_decl (location, VAR_DECL, NULL_TREE,
				      integer_type_node);
	      add_stmt (build_modify_expr (location, begin_var,
					   TREE_TYPE (begin_var),
					   NOP_EXPR, location, array_begin,
					   TREE_TYPE (array_begin)));
	      ARRAY_NOTATION_START (array_node) = begin_var;
	    }
	  if (TREE_CODE (array_lngth) != INTEGER_CST)
	    {
	      lngth_var = build_decl (location, VAR_DECL, NULL_TREE,
				      integer_type_node);
	      add_stmt (build_modify_expr (location, lngth_var,
					   TREE_TYPE (lngth_var),
					   NOP_EXPR, location, array_lngth,
					   TREE_TYPE (array_lngth)));
	      ARRAY_NOTATION_LENGTH (array_node) = lngth_var;
	    }
	  if (TREE_CODE (array_strde) != INTEGER_CST)
	    {
	      strde_var = build_decl (location, VAR_DECL, NULL_TREE,
				      integer_type_node);

	      add_stmt (build_modify_expr (location, strde_var,
					   TREE_TYPE (strde_var),
					   NOP_EXPR, location, array_strde,
					   TREE_TYPE (array_strde)));
	      ARRAY_NOTATION_STRIDE (array_node) = strde_var;
	    }
	}
    }
  
  lhs_vector = XNEWVEC (bool *, lhs_list_size);
  for (ii = 0; ii < lhs_list_size; ii++)
    lhs_vector[ii] = XNEWVEC (bool, lhs_rank);
  
  rhs_vector = XNEWVEC (bool *, rhs_list_size);
  for (ii = 0; ii < rhs_list_size; ii++)
    rhs_vector[ii] = XNEWVEC (bool, rhs_rank);
  
  lhs_array = XNEWVEC (tree *, lhs_list_size);
  for (ii = 0; ii < lhs_list_size; ii++)
    lhs_array[ii] = XNEWVEC (tree, lhs_rank);
  
  rhs_array = XNEWVEC (tree *, rhs_list_size);
  for (ii = 0; ii < rhs_list_size; ii++)
    rhs_array[ii] = XNEWVEC (tree, rhs_rank);

  lhs_value = XNEWVEC (tree *, lhs_list_size);
  for (ii = 0; ii < lhs_list_size; ii++)
    lhs_value[ii] = XNEWVEC (tree, lhs_rank);
  
  rhs_value = XNEWVEC (tree *, rhs_list_size);
  for (ii = 0; ii < rhs_list_size; ii++)
    rhs_value[ii] = XNEWVEC (tree, rhs_rank);

  lhs_stride = XNEWVEC (tree *, lhs_list_size);
  for (ii = 0; ii < lhs_list_size; ii++)
    lhs_stride[ii] = XNEWVEC (tree, lhs_rank);
  
  rhs_stride = XNEWVEC (tree *, rhs_list_size);
  for (ii = 0; ii < rhs_list_size; ii++)
    rhs_stride[ii] = XNEWVEC (tree, rhs_rank);

  lhs_length = XNEWVEC (tree *, lhs_list_size);
  for (ii = 0; ii < lhs_list_size; ii++)
    lhs_length[ii] = XNEWVEC (tree, lhs_rank);
  
  rhs_length = XNEWVEC (tree *, rhs_list_size);
  for (ii = 0; ii < rhs_list_size; ii++)
    rhs_length[ii] = XNEWVEC (tree, rhs_rank);
  
  lhs_start = XNEWVEC (tree *, lhs_list_size);
  for (ii = 0; ii < lhs_list_size; ii++)
    lhs_start[ii] = XNEWVEC (tree, lhs_rank);
  
  rhs_start = XNEWVEC (tree *, rhs_list_size);
  for (ii = 0; ii < rhs_list_size; ii++)
    rhs_start[ii] = XNEWVEC (tree, rhs_rank);

  lhs_var = XNEWVEC (tree, lhs_rank);
  rhs_var = XNEWVEC (tree, rhs_rank);
  cond_expr = XNEWVEC (tree, MAX (lhs_rank, rhs_rank));

  lhs_expr_incr = XNEWVEC (tree, lhs_rank);
  rhs_expr_incr =XNEWVEC (tree, rhs_rank);

  lhs_ind_init = XNEWVEC (tree, lhs_rank);
  rhs_ind_init = XNEWVEC (tree, rhs_rank);

  lhs_count_down = XNEWVEC (bool *, lhs_list_size);
  for (ii = 0; ii < lhs_list_size; ii++)
    lhs_count_down[ii] =  XNEWVEC (bool, lhs_rank);
  
  rhs_count_down =  XNEWVEC (bool *, rhs_list_size);
  for (ii = 0; ii < rhs_list_size; ii++)
    rhs_count_down[ii] = XNEWVEC (bool, rhs_rank);

  lhs_compare =  XNEWVEC (tree, lhs_rank);
  rhs_compare =  XNEWVEC (tree, rhs_rank);
  
  if (lhs_rank)
    {
      for (ii = 0; ii < lhs_list_size; ii++)
	{
	  jj = 0;
	  ii_tree = (*lhs_list)[ii];
	  while (ii_tree)
	    {
	      if (TREE_CODE (ii_tree) == ARRAY_NOTATION_REF)
		{
		  lhs_array[ii][jj] = ii_tree;
		  jj++;
		  ii_tree = ARRAY_NOTATION_ARRAY (ii_tree);
		}
	      else if (TREE_CODE (ii_tree) == ARRAY_REF)
		ii_tree = TREE_OPERAND (ii_tree, 0);
	      else if (TREE_CODE (ii_tree) == VAR_DECL
		       || TREE_CODE (ii_tree) == PARM_DECL)
		break;
	    }
	}
    }
  else
    lhs_array[0][0] = NULL_TREE;
  
  if (rhs_rank)
    {
      for (ii = 0; ii < rhs_list_size; ii++)
	{ 
	  jj = 0; 
	  ii_tree = (*rhs_list)[ii];
	  while (ii_tree)
	    {
	      if (TREE_CODE (ii_tree) == ARRAY_NOTATION_REF)
		{
		  rhs_array[ii][jj] = ii_tree;
		  jj++;
		  ii_tree = ARRAY_NOTATION_ARRAY (ii_tree);
		}
	      else if (TREE_CODE (ii_tree) == ARRAY_REF)
		ii_tree = TREE_OPERAND (ii_tree, 0);
	      else if (TREE_CODE (ii_tree) == VAR_DECL
		       || TREE_CODE (ii_tree) == PARM_DECL
		       || TREE_CODE (ii_tree) == CALL_EXPR)
		break;
	    }
	}
    }

  for (ii = 0; ii < lhs_list_size; ii++)
    {
      tree lhs_node = (*lhs_list)[ii];
      if (TREE_CODE (lhs_node) == ARRAY_NOTATION_REF)
	{
	  for (jj = 0; jj < lhs_rank; jj++)
	    {
	      if (TREE_CODE (lhs_array[ii][jj]) == ARRAY_NOTATION_REF)
		{
		  lhs_value[ii][jj] = ARRAY_NOTATION_ARRAY (lhs_array[ii][jj]);
		  lhs_start[ii][jj] = ARRAY_NOTATION_START (lhs_array[ii][jj]);
		  lhs_length[ii][jj] =
		    fold_build1 (CONVERT_EXPR, integer_type_node,
				 ARRAY_NOTATION_LENGTH (lhs_array[ii][jj]));
		  lhs_stride[ii][jj] =
		    fold_build1 (CONVERT_EXPR, integer_type_node,
				 ARRAY_NOTATION_STRIDE (lhs_array[ii][jj]));
		  lhs_vector[ii][jj] = true;
		  /* IF the stride value is variable (i.e. not constant) then 
		     assume that the length is positive.  */
		  if (!TREE_CONSTANT (lhs_length[ii][jj]))
		    lhs_count_down[ii][jj] = false;
		  else if (tree_int_cst_lt
			   (lhs_length[ii][jj],
			    build_zero_cst (TREE_TYPE (lhs_length[ii][jj]))))
		    lhs_count_down[ii][jj] = true;
		  else
		    lhs_count_down[ii][jj] = false;
		}
	      else
		lhs_vector[ii][jj] = false;
	    }
	}
    }
  for (ii = 0; ii < rhs_list_size; ii++)
    {
      if (TREE_CODE ((*rhs_list)[ii]) == ARRAY_NOTATION_REF)
	{
	  for (jj = 0; jj < rhs_rank; jj++)
	    {
	      if (TREE_CODE (rhs_array[ii][jj]) == ARRAY_NOTATION_REF)
		{
		  rhs_value[ii][jj]  = ARRAY_NOTATION_ARRAY (rhs_array[ii][jj]);
		  rhs_start[ii][jj]  = ARRAY_NOTATION_START (rhs_array[ii][jj]);
		  rhs_length[ii][jj] =
		    fold_build1 (CONVERT_EXPR, integer_type_node,
				 ARRAY_NOTATION_LENGTH (rhs_array[ii][jj]));
		  rhs_stride[ii][jj] =
		    fold_build1 (CONVERT_EXPR, integer_type_node,
				 ARRAY_NOTATION_STRIDE (rhs_array[ii][jj]));
		  rhs_vector[ii][jj] = true;
		  /* If the stride value is variable (i.e. not constant) then 
		     assume that the length is positive.  */
		  if (!TREE_CONSTANT (rhs_length[ii][jj]))
		    rhs_count_down[ii][jj] = false;
		  else if (tree_int_cst_lt
			   (rhs_length[ii][jj],
			    build_int_cst (TREE_TYPE (rhs_length[ii][jj]), 0)))
		    rhs_count_down[ii][jj] = true;
		  else
		    rhs_count_down[ii][jj] = false;	
		}
	      else
		rhs_vector[ii][jj] = false;
	    }
	}
      else
	for (jj = 0; jj < rhs_rank; jj++)
	  rhs_vector[ii][jj] = false;
    }

  if (length_mismatch_in_expr_p (EXPR_LOCATION (lhs), lhs_length,
				 lhs_list_size, lhs_rank)
      || length_mismatch_in_expr_p (EXPR_LOCATION (rhs), rhs_length,
				    rhs_list_size, rhs_rank))
    {
      pop_stmt_list (an_init);
      return error_mark_node;
    }

  if (lhs_list_size > 0 && rhs_list_size > 0 && lhs_rank > 0 && rhs_rank > 0
      && TREE_CODE (lhs_length[0][0]) == INTEGER_CST
      && TREE_CODE (rhs_length[0][0]) == INTEGER_CST)
    {
      HOST_WIDE_INT l_length = int_cst_value (lhs_length[0][0]);
      HOST_WIDE_INT r_length = int_cst_value (rhs_length[0][0]);
      /* Length can be negative or positive.  As long as the magnitude is OK,
	 then the array notation is valid.  */
      if (absu_hwi (l_length) != absu_hwi (r_length))
	{
	  error_at (location, "length mismatch between LHS and RHS");
	  pop_stmt_list (an_init);
	  return error_mark_node;
	}
    }
  for (ii = 0; ii < lhs_rank; ii++)
    {
      if (lhs_vector[0][ii])
	{
	  lhs_var[ii] = build_decl (location, VAR_DECL, NULL_TREE,
				    integer_type_node);
	  lhs_ind_init[ii] = build_modify_expr
	    (location, lhs_var[ii], TREE_TYPE (lhs_var[ii]),
	     NOP_EXPR,
	     location, build_zero_cst (TREE_TYPE (lhs_var[ii])),
	     TREE_TYPE (lhs_var[ii]));
	}
    }

  for (ii = 0; ii < rhs_rank; ii++)
    {
      /* When we have a polynomial, we assume that the indices are of type 
	 integer.  */
      rhs_var[ii] = build_decl (location, VAR_DECL, NULL_TREE,
				integer_type_node);
      rhs_ind_init[ii] = build_modify_expr
	(location, rhs_var[ii], TREE_TYPE (rhs_var[ii]),
	 NOP_EXPR,
	 location, build_int_cst (TREE_TYPE (rhs_var[ii]), 0),
	 TREE_TYPE (rhs_var[ii]));
    }
  if (lhs_rank)
    {
      for (ii = 0; ii < lhs_list_size; ii++)
	{
	  if (lhs_vector[ii][0])
	    { 
	      /* The last ARRAY_NOTATION element's ARRAY component should be 
		 the array's base value.  */
	      tree lhs_array_opr = lhs_value[ii][lhs_rank - 1];
	      for (s_jj = lhs_rank - 1; s_jj >= 0; s_jj--)
		{
		  if (lhs_count_down[ii][s_jj])
	  	      /* Array[start_index + (induction_var * stride)].  */
		      lhs_array_opr = build_array_ref
			(location, lhs_array_opr,
			 build2 (MINUS_EXPR, TREE_TYPE (lhs_var[s_jj]),
				 lhs_start[ii][s_jj],
				 build2 (MULT_EXPR, TREE_TYPE (lhs_var[s_jj]),
					 lhs_var[s_jj],
					 lhs_stride[ii][s_jj])));
		  else
		    lhs_array_opr = build_array_ref
		      (location, lhs_array_opr,
		       build2 (PLUS_EXPR, TREE_TYPE (lhs_var[s_jj]),
			       lhs_start[ii][s_jj],
			       build2 (MULT_EXPR, TREE_TYPE (lhs_var[s_jj]),
				       lhs_var[s_jj],
				       lhs_stride[ii][s_jj])));
		}
	      vec_safe_push (lhs_array_operand, lhs_array_opr);
	    }
	  else
	    vec_safe_push (lhs_array_operand, integer_one_node);
	}
      replace_array_notations (&lhs, true, lhs_list, lhs_array_operand);
      array_expr_lhs = lhs;
    }

  if (rhs_rank)
    {
      for (ii = 0; ii < rhs_list_size; ii++)
	{
	  if (rhs_vector[ii][0])
	    {
	      tree rhs_array_opr = rhs_value[ii][rhs_rank - 1];
	      for (s_jj = rhs_rank - 1; s_jj >= 0; s_jj--)
		{
		  if (rhs_count_down[ii][s_jj])
		    /* Array[start_index - (induction_var * stride)] */
		    rhs_array_opr = build_array_ref
		      (location, rhs_array_opr,
		       build2 (MINUS_EXPR, TREE_TYPE (rhs_var[s_jj]),
			       rhs_start[ii][s_jj],
			       build2 (MULT_EXPR, TREE_TYPE (rhs_var[s_jj]),
				       rhs_var[s_jj],
				       rhs_stride[ii][s_jj])));
		  else
		    /* Array[start_index  + (induction_var * stride)] */
		    rhs_array_opr = build_array_ref
		      (location, rhs_array_opr,
		       build2 (PLUS_EXPR, TREE_TYPE (rhs_var[s_jj]),
			       rhs_start[ii][s_jj],
			       build2 (MULT_EXPR, TREE_TYPE (rhs_var[s_jj]),
				       rhs_var[s_jj],
				       rhs_stride[ii][s_jj])));
		}
	      vec_safe_push (rhs_array_operand, rhs_array_opr);
	    }
	  else
	  /* This is just a dummy node to make sure the list sizes for both
	     array list and array operand list are the same.  */
	  vec_safe_push (rhs_array_operand, integer_one_node);
	}

      for (ii = 0; ii < rhs_list_size; ii++)
	{
	  tree rhs_node = (*rhs_list)[ii];
	  if (TREE_CODE (rhs_node) == CALL_EXPR)
	    {
	      int idx_value = 0;
	      tree func_name = CALL_EXPR_FN (rhs_node);
	      if (TREE_CODE (func_name) == ADDR_EXPR)
		if (is_sec_implicit_index_fn (func_name))
		  {
		    idx_value = 
		      extract_sec_implicit_index_arg (location, rhs_node);
		    if (idx_value == -1) /* This means we have an error.  */
		      return error_mark_node;
		    else if (idx_value < (int) lhs_rank && idx_value >= 0)
		      vec_safe_push (rhs_array_operand, lhs_var[idx_value]);
		    else
		      {
			size_t ee = 0;
			tree lhs_base = (*lhs_list)[ii];
			for (ee = 0; ee < lhs_rank; ee++)
			  lhs_base = ARRAY_NOTATION_ARRAY (lhs_base);
			error_at (location, "__sec_implicit_index argument %d "
				  "must be less than rank of %qD", idx_value,
				  lhs_base);
			return error_mark_node;
		      }
		  }  
	    }
	}
      replace_array_notations (&rhs, true, rhs_list, rhs_array_operand);
      array_expr_rhs = rhs;
    }
  else
    {
      for (ii = 0; ii < rhs_list_size; ii++)
	{
	  tree rhs_node = (*rhs_list)[ii];
	  if (TREE_CODE (rhs_node) == CALL_EXPR)
	    {
	      int idx_value = 0;
	      tree func_name = CALL_EXPR_FN (rhs_node);
	      if (TREE_CODE (func_name) == ADDR_EXPR)
		if (is_sec_implicit_index_fn (func_name))
		  {
		    idx_value = 
		      extract_sec_implicit_index_arg (location, rhs_node);
		    if (idx_value == -1) /* This means we have an error.  */
		      return error_mark_node;
		    else if (idx_value < (int) lhs_rank && idx_value >= 0)
		      vec_safe_push (rhs_array_operand, lhs_var[idx_value]);
		    else
		      {
			size_t ee = 0;
			tree lhs_base = (*lhs_list)[ii];
			for (ee = 0; ee < lhs_rank; ee++)
			  lhs_base = ARRAY_NOTATION_ARRAY (lhs_base);
			error_at (location, "__sec_implicit_index argument %d "
				  "must be less than rank of %qD", idx_value,
				  lhs_base);
			return error_mark_node;
		      }
		  }  
	    }
	}
      replace_array_notations (&rhs, true, rhs_list, rhs_array_operand);
      array_expr_rhs = rhs;
      rhs_expr_incr[0] = NULL_TREE;
    }

  for (ii = 0; ii < rhs_rank; ii++) 
    rhs_expr_incr[ii] = build2 (MODIFY_EXPR, void_type_node, rhs_var[ii], 
				build2 
				(PLUS_EXPR, TREE_TYPE (rhs_var[ii]), 
				 rhs_var[ii], 
				 build_one_cst (TREE_TYPE (rhs_var[ii]))));

  for (ii = 0; ii < lhs_rank; ii++) 
    lhs_expr_incr[ii] = build2 
      (MODIFY_EXPR, void_type_node, lhs_var[ii], 
       build2 (PLUS_EXPR, TREE_TYPE (lhs_var[ii]), lhs_var[ii], 
	       build_one_cst (TREE_TYPE (lhs_var[ii]))));
  
  /* If array_expr_lhs is NULL, then we have function that returns void or
     its return value is ignored.  */
  if (!array_expr_lhs)
    array_expr_lhs = lhs;

  array_expr = build_modify_expr (location, array_expr_lhs, lhs_origtype, 
				  modifycode, rhs_loc, array_expr_rhs, 
				  rhs_origtype);

  for (jj = 0; jj < MAX (lhs_rank, rhs_rank); jj++)
    {
      if (rhs_rank && rhs_expr_incr[jj])
	{
	  size_t iii = 0;
	  if (lhs_rank == 0)
	    lhs_compare[jj] = integer_one_node;
	  else if (lhs_count_down[0][jj])
	    lhs_compare[jj] = build2
	      (GT_EXPR, boolean_type_node, lhs_var[jj], lhs_length[0][jj]);
	  else
	    lhs_compare[jj] = build2
	      (LT_EXPR, boolean_type_node, lhs_var[jj], lhs_length[0][jj]);


	  /* The reason why we have this here is for the following case:
	         Array[:][:] = function_call(something) + Array2[:][:];

	     So, we will skip the first operand of RHS and then go to the
	     2nd to find whether we should count up or down.  */
	 
	  for (iii = 0; iii < rhs_list_size; iii++)
	    if (rhs_vector[iii][jj])
	      break;
	      
	  /* What we are doing here is this:
	     We always count up, so:
	       if (length is negative ==> which means we count down)
	          we multiply length by -1 and count up => ii < -LENGTH
	       else
	          we just count up, so we compare for  ii < LENGTH
	   */
	  if (rhs_count_down[iii][jj])
	    /* We use iii for rhs_length because that is the correct countdown
	       we have to use.  */
	      rhs_compare[jj] = build2
		(LT_EXPR, boolean_type_node, rhs_var[jj],
		 build2 (MULT_EXPR, TREE_TYPE (rhs_var[jj]),
			 rhs_length[iii][jj],
			 build_int_cst (TREE_TYPE (rhs_var[jj]), -1)));
	  else
	    rhs_compare[jj] = build2 (LT_EXPR, boolean_type_node, rhs_var[jj],
				      rhs_length[iii][jj]);
	  if (lhs_compare[ii] != integer_one_node)
	    cond_expr[jj] = build2 (TRUTH_ANDIF_EXPR, void_type_node,
				    lhs_compare[jj], rhs_compare[jj]);
	  else
	    cond_expr[jj] = rhs_compare[jj];
	}
      else
	{
	  if (lhs_count_down[0][jj])
	    cond_expr[jj] = build2
	      (GT_EXPR, boolean_type_node, lhs_var[jj], lhs_length[0][jj]);
	  else
	    cond_expr[jj] = build2
	      (LT_EXPR, boolean_type_node, lhs_var[jj], lhs_length[0][jj]);
	}
    }

  an_init = pop_stmt_list (an_init);
  append_to_statement_list_force (an_init, &loop_with_init);
  body = array_expr;
  for (ii = 0; ii < MAX (lhs_rank, rhs_rank); ii++)
    {
      tree incr_list = alloc_stmt_list ();
      tree new_loop = push_stmt_list ();
      if (lhs_rank)
	add_stmt (lhs_ind_init[ii]);
      if (rhs_rank)
	add_stmt (rhs_ind_init[ii]);
      if (lhs_rank)
	append_to_statement_list_force (lhs_expr_incr[ii], &incr_list);
      if (rhs_rank && rhs_expr_incr[ii])
	append_to_statement_list_force (rhs_expr_incr[ii], &incr_list);
      c_finish_loop (location, cond_expr[ii], incr_list, body, NULL_TREE,
		     NULL_TREE, true);
      body = pop_stmt_list (new_loop);
    }
  append_to_statement_list_force (body, &loop_with_init);
  return loop_with_init;
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
  size_t rank = 0, ii = 0, jj = 0;
  int s_jj = 0;
  tree **array_ops, *array_var, jj_tree, loop_init;
  tree **array_value, **array_stride, **array_length, **array_start;
  tree *compare_expr, *expr_incr, *ind_init;
  bool **count_down, **array_vector;
  tree begin_var, lngth_var, strde_var;
  location_t location = EXPR_LOCATION (stmt);
  tree body = NULL_TREE, loop_with_init = alloc_stmt_list ();
  if (TREE_CODE (stmt) == COND_EXPR)
    cond = COND_EXPR_COND (stmt);
  else if (TREE_CODE (stmt) == SWITCH_EXPR)
    cond = SWITCH_COND (stmt);
  else
    /* Otherwise dont even touch the statement.  */
    return stmt;

  if (!find_rank (location, cond, cond, false, &rank))
    return error_mark_node;
  
  extract_array_notation_exprs (cond, false, &array_list);
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
	      replace_array_notations (&cond, false, sub_list, new_var_list); 
	    }
	}
    }

  if (!find_rank (location, cond, cond, true, &rank))
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
  extract_array_notation_exprs (cond, true, &array_list);

  if (vec_safe_length (array_list) == 0)
    return stmt;

  list_size = vec_safe_length (array_list);

  array_ops =  XNEWVEC (tree *, list_size);
  for (ii = 0; ii < list_size; ii++)
    array_ops[ii] =  XNEWVEC (tree, rank);

  array_vector =  XNEWVEC (bool *, list_size);
  for (ii = 0; ii < list_size; ii++)
    array_vector[ii] =  XNEWVEC (bool, rank);

  array_value = XNEWVEC (tree *, list_size);
  array_stride = XNEWVEC (tree *, list_size);
  array_length = XNEWVEC (tree *, list_size);
  array_start =  XNEWVEC (tree *, list_size);

  for (ii = 0; ii < list_size; ii++)
    {
      array_value[ii]  = XNEWVEC (tree, rank);
      array_stride[ii] = XNEWVEC (tree, rank);
      array_length[ii] = XNEWVEC (tree, rank);
      array_start[ii]  = XNEWVEC (tree, rank);
    }

  compare_expr = XNEWVEC (tree, rank);
  expr_incr = XNEWVEC (tree, rank);
  ind_init = XNEWVEC (tree,  rank);

  count_down = XNEWVEC (bool *, list_size);
  for (ii = 0; ii < list_size; ii++)
    count_down[ii] = XNEWVEC (bool, rank);

  array_var = XNEWVEC (tree, rank);

  for (ii = 0; ii < list_size; ii++)
    {
      tree array_node = (*array_list)[ii];
      if (array_node && TREE_CODE (array_node) == ARRAY_NOTATION_REF)
	{
	  tree array_begin = ARRAY_NOTATION_START (array_node);
	  tree array_lngth = ARRAY_NOTATION_LENGTH (array_node);
	  tree array_strde = ARRAY_NOTATION_STRIDE (array_node);

	  if (TREE_CODE (array_begin) != INTEGER_CST)
	    {
	      begin_var = build_decl (location, VAR_DECL, NULL_TREE,
				      integer_type_node);
	      add_stmt (build_modify_expr (location, begin_var,
					   TREE_TYPE (begin_var),
					   NOP_EXPR, location, array_begin,
					   TREE_TYPE (array_begin)));
	      ARRAY_NOTATION_START (array_node) = begin_var;
	    }
	  if (TREE_CODE (array_lngth) != INTEGER_CST)
	    {
	      lngth_var = build_decl (location, VAR_DECL, NULL_TREE,
				      integer_type_node);
	      add_stmt (build_modify_expr (location, lngth_var,
					   TREE_TYPE (lngth_var),
					   NOP_EXPR, location, array_lngth,
					   TREE_TYPE (array_lngth)));
	      ARRAY_NOTATION_LENGTH (array_node) = lngth_var;
	    }
	  if (TREE_CODE (array_strde) != INTEGER_CST)
	    {
	      strde_var = build_decl (location, VAR_DECL, NULL_TREE,
				      integer_type_node);
	      add_stmt (build_modify_expr (location, strde_var,
					   TREE_TYPE (strde_var),
					   NOP_EXPR, location, array_strde,
					   TREE_TYPE (array_strde)));
	      ARRAY_NOTATION_STRIDE (array_node) = strde_var;
	    }
	}
    }  
  for (ii = 0; ii < list_size; ii++)
    {
      tree array_node = (*array_list)[ii];
      jj = 0;
      for (jj_tree = array_node;
	   jj_tree && TREE_CODE (jj_tree) == ARRAY_NOTATION_REF;
	   jj_tree = ARRAY_NOTATION_ARRAY (jj_tree))
	{
	  array_ops[ii][jj] = jj_tree;
	  jj++;
	}
    }
  for (ii = 0; ii < list_size; ii++)
    {
      tree array_node = (*array_list)[ii];
      if (TREE_CODE (array_node) == ARRAY_NOTATION_REF)
	{
	  for (jj = 0; jj < rank; jj++)
	    {
	      if (TREE_CODE (array_ops[ii][jj]) == ARRAY_NOTATION_REF)
		{
		  array_value[ii][jj] =
		    ARRAY_NOTATION_ARRAY (array_ops[ii][jj]);
		  array_start[ii][jj] =
		    ARRAY_NOTATION_START (array_ops[ii][jj]);
		  array_length[ii][jj] =
		    fold_build1 (CONVERT_EXPR, integer_type_node,
				 ARRAY_NOTATION_LENGTH (array_ops[ii][jj]));
		  array_stride[ii][jj] =
		    fold_build1 (CONVERT_EXPR, integer_type_node,
				 ARRAY_NOTATION_STRIDE (array_ops[ii][jj]));
		  array_vector[ii][jj] = true;

		  if (!TREE_CONSTANT (array_length[ii][jj]))
		      count_down[ii][jj] = false;
		  else if (tree_int_cst_lt
			   (array_length[ii][jj],
			    build_int_cst (TREE_TYPE (array_length[ii][jj]),
					   0)))
		    count_down[ii][jj] = true;
		  else
		    count_down[ii][jj] = false;
		}
	      else
		array_vector[ii][jj] = false;
	    }
	}
    }

  for (ii = 0; ii < rank; ii++)
    {
      array_var[ii] = build_decl (location, VAR_DECL, NULL_TREE,
				  integer_type_node);
      ind_init[ii] =
	build_modify_expr (location, array_var[ii],
			   TREE_TYPE (array_var[ii]), NOP_EXPR,
			   location,
			   build_int_cst (TREE_TYPE (array_var[ii]), 0),
			   TREE_TYPE (array_var[ii]));
    }
  
  for (ii = 0; ii < list_size; ii++)
    {
      if (array_vector[ii][0])
	{
	  tree array_opr = array_value[ii][rank - 1];
	  for (s_jj = rank - 1; s_jj >= 0; s_jj--)
	    {
	      if (count_down[ii][s_jj])
		/* Array[start_index - (induction_var * stride)] */
		array_opr = build_array_ref
		  (location, array_opr,
		   build2 (MINUS_EXPR, TREE_TYPE (array_var[s_jj]),
			   array_start[ii][s_jj],
			   build2 (MULT_EXPR, TREE_TYPE (array_var[s_jj]),
				   array_var[s_jj], array_stride[ii][s_jj])));
	      else
		/* Array[start_index + (induction_var * stride)] */
		array_opr = build_array_ref
		  (location, array_opr,
		   build2 (PLUS_EXPR, TREE_TYPE (array_var[s_jj]),
			   array_start[ii][s_jj],
			   build2 (MULT_EXPR, TREE_TYPE (array_var[s_jj]),
				   array_var[s_jj], array_stride[ii][s_jj])));
	    }
	  vec_safe_push (array_operand, array_opr);
	}
      else
	/* This is just a dummy node to make sure the list sizes for both
	   array list and array operand list are the same.  */
	vec_safe_push (array_operand, integer_one_node);
    }
  replace_array_notations (&stmt, true, array_list, array_operand);
  for (ii = 0; ii < rank; ii++) 
    expr_incr[ii] = build2 (MODIFY_EXPR, void_type_node, array_var[ii], 
			    build2 (PLUS_EXPR, TREE_TYPE (array_var[ii]), 
				    array_var[ii], 
				    build_int_cst (TREE_TYPE (array_var[ii]), 
						   1)));
  for (jj = 0; jj < rank; jj++)
    {
      if (rank && expr_incr[jj])
	{
	  if (count_down[0][jj])
	    compare_expr[jj] =
	      build2 (LT_EXPR, boolean_type_node, array_var[jj],
		      build2 (MULT_EXPR, TREE_TYPE (array_var[jj]),
			      array_length[0][jj],
			      build_int_cst (TREE_TYPE (array_var[jj]), -1)));
	  else
	    compare_expr[jj] = build2 (LT_EXPR, boolean_type_node,
				       array_var[jj], array_length[0][jj]);
	}
    }

  loop_init = pop_stmt_list (loop_init);
  body = stmt;
  append_to_statement_list_force (loop_init, &loop_with_init);

  for (ii = 0; ii < rank; ii++)
    {
      tree new_loop = push_stmt_list ();
      add_stmt (ind_init[ii]);
      c_finish_loop (location, compare_expr[ii], expr_incr[ii], body, NULL_TREE,
		     NULL_TREE, true);
      body = pop_stmt_list (new_loop);
    }
  append_to_statement_list_force (body, &loop_with_init);
  XDELETEVEC (expr_incr);
  XDELETEVEC (ind_init);
  
  for (ii = 0; ii < list_size; ii++)
    {
      XDELETEVEC (count_down[ii]);
      XDELETEVEC (array_value[ii]);
      XDELETEVEC (array_stride[ii]);
      XDELETEVEC (array_length[ii]);
      XDELETEVEC (array_start[ii]);
      XDELETEVEC (array_ops[ii]);
      XDELETEVEC (array_vector[ii]);
    }

  XDELETEVEC (count_down);
  XDELETEVEC (array_value);
  XDELETEVEC (array_stride);
  XDELETEVEC (array_length);
  XDELETEVEC (array_start);
  XDELETEVEC (array_ops);
  XDELETEVEC (array_vector);

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
  size_t list_size = 0, rank = 0, ii = 0, jj = 0;
  int s_jj = 0;
  tree **array_ops, *array_var, jj_tree, loop_init;
  tree **array_value, **array_stride, **array_length, **array_start;
  tree *compare_expr, *expr_incr, *ind_init;
  tree body, loop_with_init = alloc_stmt_list ();
  bool **count_down, **array_vector;
  
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
  
  array_ops = XNEWVEC (tree *, list_size);
  for (ii = 0; ii < list_size; ii++)
    array_ops[ii] = XNEWVEC (tree,  rank);
  
  array_vector =  XNEWVEC (bool *, list_size);
  for (ii = 0; ii < list_size; ii++)
    array_vector[ii] = XNEWVEC (bool, rank);

  array_value = XNEWVEC (tree *, list_size);
  array_stride = XNEWVEC (tree *, list_size);
  array_length = XNEWVEC (tree *, list_size);
  array_start = XNEWVEC (tree *, list_size);

  for (ii = 0; ii < list_size; ii++)
    {
      array_value[ii]  = XNEWVEC (tree, rank);
      array_stride[ii] = XNEWVEC (tree, rank);
      array_length[ii] = XNEWVEC (tree, rank);
      array_start[ii]  = XNEWVEC (tree, rank);
    }

  compare_expr = XNEWVEC (tree, rank);
  expr_incr = XNEWVEC (tree, rank);
  ind_init = XNEWVEC (tree, rank);
  
  count_down = XNEWVEC (bool *, list_size);
  for (ii = 0; ii < list_size; ii++)
    count_down[ii] = XNEWVEC (bool, rank);
  array_var = XNEWVEC (tree, rank);

  for (ii = 0; ii < list_size; ii++)
    {
      jj = 0;
      for (jj_tree = (*array_list)[ii];
	   jj_tree && TREE_CODE (jj_tree) == ARRAY_NOTATION_REF;
	   jj_tree = ARRAY_NOTATION_ARRAY (jj_tree))
	{
	  array_ops[ii][jj] = jj_tree;
	  jj++;
	}
    }
  
  loop_init = push_stmt_list ();

  for (ii = 0; ii < list_size; ii++)
    {
      tree array_node = (*array_list)[ii];
      if (TREE_CODE (array_node) == ARRAY_NOTATION_REF)
	{
	  for (jj = 0; jj < rank; jj++)
	    {
	      if (TREE_CODE (array_ops[ii][jj]) == ARRAY_NOTATION_REF)
		{
		  array_value[ii][jj] =
		    ARRAY_NOTATION_ARRAY (array_ops[ii][jj]);
		  array_start[ii][jj] =
		    ARRAY_NOTATION_START (array_ops[ii][jj]);
		  array_length[ii][jj] =
		    fold_build1 (CONVERT_EXPR, integer_type_node,
				 ARRAY_NOTATION_LENGTH (array_ops[ii][jj]));
		  array_stride[ii][jj] =
		    fold_build1 (CONVERT_EXPR, integer_type_node,
				 ARRAY_NOTATION_STRIDE (array_ops[ii][jj]));
		  array_vector[ii][jj] = true;

		  if (!TREE_CONSTANT (array_length[ii][jj])) 
		    count_down[ii][jj] = false;
		  else if (tree_int_cst_lt
			   (array_length[ii][jj],
			    build_int_cst (TREE_TYPE (array_length[ii][jj]),
					   0)))
		    count_down[ii][jj] = true;
		  else
		    count_down[ii][jj] = false;
		}
	      else
		array_vector[ii][jj] = false;
	    }
	}
    }

  for (ii = 0; ii < rank; ii++)
    {
      array_var[ii] = build_decl (location, VAR_DECL, NULL_TREE,
				  integer_type_node);
      ind_init[ii] =
	build_modify_expr (location, array_var[ii],
			   TREE_TYPE (array_var[ii]), NOP_EXPR,
			   location,
			   build_int_cst (TREE_TYPE (array_var[ii]), 0),
			   TREE_TYPE (array_var[ii]));
	
    }
  for (ii = 0; ii < list_size; ii++)
    {
      if (array_vector[ii][0])
	{
	  tree array_opr = array_value[ii][rank - 1];
	  for (s_jj = rank - 1; s_jj >= 0; s_jj--)
	    {
	      if (count_down[ii][s_jj])
		/* Array[start_index - (induction_var * stride)] */
		array_opr = build_array_ref
		  (location, array_opr,
		   build2 (MINUS_EXPR, TREE_TYPE (array_var[s_jj]),
			   array_start[ii][s_jj],
			   build2 (MULT_EXPR, TREE_TYPE (array_var[s_jj]),
				   array_var[s_jj], array_stride[ii][s_jj])));
	      else
		/* Array[start_index + (induction_var * stride)] */
		array_opr = build_array_ref
		  (location, array_opr,
		   build2 (PLUS_EXPR, TREE_TYPE (array_var[s_jj]),
			   array_start[ii][s_jj],
			   build2 (MULT_EXPR, TREE_TYPE (array_var[s_jj]),
				   array_var[s_jj], array_stride[ii][s_jj])));
	    }
	  vec_safe_push (array_operand, array_opr);
	}
      else
      	/* This is just a dummy node to make sure the list sizes for both
	   array list and array operand list are the same.  */
	vec_safe_push (array_operand, integer_one_node);
    }
  replace_array_notations (&arg.value, true, array_list, array_operand);

  for (ii = 0; ii < rank; ii++)
    expr_incr[ii] =
      build2 (MODIFY_EXPR, void_type_node, array_var[ii],
	      build2 (PLUS_EXPR, TREE_TYPE (array_var[ii]), array_var[ii],
		      build_int_cst (TREE_TYPE (array_var[ii]), 1)));
  
  for (jj = 0; jj < rank; jj++)
    {
      if (rank && expr_incr[jj])
	{
	  if (count_down[0][jj])
	    compare_expr[jj] =
	      build2 (LT_EXPR, boolean_type_node, array_var[jj],
		      build2 (MULT_EXPR, TREE_TYPE (array_var[jj]),
			      array_length[0][jj],
			      build_int_cst (TREE_TYPE (array_var[jj]), -1)));
	  else
	    compare_expr[jj] = build2 (LT_EXPR, boolean_type_node,
				       array_var[jj], array_length[0][jj]);
	}
    }
  
  if (code == POSTINCREMENT_EXPR || code == POSTDECREMENT_EXPR)
    {
      arg = default_function_array_read_conversion (location, arg);
      arg.value = build_unary_op (location, code, arg.value, 0);
    }
  else if (code == PREINCREMENT_EXPR || code == PREDECREMENT_EXPR)
    {
      arg = default_function_array_read_conversion (location, arg);
      arg = parser_build_unary_op (location, code, arg);
    }

  loop_init = pop_stmt_list (loop_init);
  append_to_statement_list_force (loop_init, &loop_with_init);
  body = arg.value;

  for (ii = 0; ii < rank; ii++)
    {
      tree new_loop = push_stmt_list ();
      add_stmt (ind_init[ii]);
      c_finish_loop (location, compare_expr[ii], expr_incr[ii], body, NULL_TREE,
		     NULL_TREE, true);
      body = pop_stmt_list (new_loop);
    }
  append_to_statement_list_force (body, &loop_with_init);
   XDELETEVEC (expr_incr);
  XDELETEVEC (ind_init);
  XDELETEVEC (array_var);
  
  for (ii = 0; ii < list_size; ii++)
    {
      XDELETEVEC (count_down[ii]);
      XDELETEVEC (array_value[ii]);
      XDELETEVEC (array_stride[ii]);
      XDELETEVEC (array_length[ii]);
      XDELETEVEC (array_start[ii]);
      XDELETEVEC (array_ops[ii]);
      XDELETEVEC (array_vector[ii]);
    }

  XDELETEVEC (count_down);
  XDELETEVEC (array_value);
  XDELETEVEC (array_stride);
  XDELETEVEC (array_length);
  XDELETEVEC (array_start);
  XDELETEVEC (array_ops);
  XDELETEVEC (array_vector);

  arg.value = loop_with_init;
  return arg;
}

/* Returns true if EXPR or any of its subtrees contain ARRAY_NOTATION_EXPR 
   node.  */

bool
contains_array_notation_expr (tree expr)
{
  vec<tree, va_gc> *array_list = NULL;

  if (!expr)
    return false;
  if (TREE_CODE (expr) == FUNCTION_DECL)
    if (is_cilkplus_reduce_builtin (expr))
      return true;
  
  extract_array_notation_exprs (expr, false, &array_list);
  if (vec_safe_length (array_list) == 0)
    return false;
  else
    return true;
}

/* Replaces array notations in a void function call arguments in ARG and returns
   a STATEMENT_LIST.  */

static tree
fix_array_notation_call_expr (tree arg)
{
  vec<tree, va_gc> *array_list = NULL, *array_operand = NULL;
  tree new_var = NULL_TREE;
  size_t list_size = 0, rank = 0, ii = 0, jj = 0;
  int s_jj = 0;
  tree **array_ops, *array_var, jj_tree, loop_init;
  tree **array_value, **array_stride, **array_length, **array_start;
  tree body, loop_with_init = alloc_stmt_list ();
  tree *compare_expr, *expr_incr, *ind_init;
  bool **count_down, **array_vector;
  tree begin_var, lngth_var, strde_var;
  location_t location = UNKNOWN_LOCATION;

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

  array_ops = XNEWVEC (tree *, list_size);
  for (ii = 0; ii < list_size; ii++)
    array_ops[ii] = XNEWVEC (tree, rank);
  
  array_vector = XNEWVEC (bool *, list_size);
  for (ii = 0; ii < list_size; ii++)
    array_vector[ii] = (bool *) XNEWVEC (bool, rank);

  array_value = XNEWVEC (tree *, list_size);
  array_stride = XNEWVEC (tree *, list_size);
  array_length = XNEWVEC (tree *, list_size);
  array_start = XNEWVEC (tree *, list_size);

  for (ii = 0; ii < list_size; ii++)
    {
      array_value[ii]  = XNEWVEC (tree, rank);
      array_stride[ii] = XNEWVEC (tree, rank);
      array_length[ii] = XNEWVEC (tree, rank);
      array_start[ii]  = XNEWVEC (tree, rank);
    }

  compare_expr = XNEWVEC (tree, rank);
  expr_incr = XNEWVEC (tree, rank);
  ind_init = XNEWVEC (tree, rank);
  
  count_down =  XNEWVEC (bool *, list_size);
  for (ii = 0; ii < list_size; ii++)
    count_down[ii] = XNEWVEC (bool, rank);
  
  array_var = XNEWVEC (tree, rank);
  
  loop_init = push_stmt_list ();
  for (ii = 0; ii < list_size; ii++)
    {
      tree array_node = (*array_list)[ii];
      if (array_node && TREE_CODE (array_node) == ARRAY_NOTATION_REF)
	{
	  tree array_begin = ARRAY_NOTATION_START (array_node);
	  tree array_lngth = ARRAY_NOTATION_LENGTH (array_node);
	  tree array_strde = ARRAY_NOTATION_STRIDE (array_node);

	  if (TREE_CODE (array_begin) != INTEGER_CST)
	    {
	      begin_var = build_decl (location, VAR_DECL, NULL_TREE,
				      integer_type_node);
	      add_stmt (build_modify_expr (location, begin_var,
					   TREE_TYPE (begin_var),
					   NOP_EXPR, location, array_begin,
					   TREE_TYPE (array_begin)));
	      ARRAY_NOTATION_START (array_node) = begin_var;
	    }
	  if (TREE_CODE (array_lngth) != INTEGER_CST)
	    {
	      lngth_var = build_decl (location, VAR_DECL, NULL_TREE,
				      integer_type_node);
	      add_stmt (build_modify_expr (location, lngth_var,
					   TREE_TYPE (lngth_var),
					   NOP_EXPR, location, array_lngth,
					   TREE_TYPE (array_lngth)));
	      ARRAY_NOTATION_LENGTH (array_node) = lngth_var;
	    }
	  if (TREE_CODE (array_strde) != INTEGER_CST)
	    {
	      strde_var = build_decl (location, VAR_DECL, NULL_TREE,
				      integer_type_node);
	      add_stmt (build_modify_expr (location, strde_var,
					   TREE_TYPE (strde_var),
					   NOP_EXPR, location, array_strde,
					   TREE_TYPE (array_strde)));
	      ARRAY_NOTATION_STRIDE (array_node) = strde_var;
	    }
	}
    }
  for (ii = 0; ii < list_size; ii++)
    {
      jj = 0;
      for (jj_tree = (*array_list)[ii];
	   jj_tree && TREE_CODE (jj_tree) == ARRAY_NOTATION_REF;
	   jj_tree = ARRAY_NOTATION_ARRAY (jj_tree))
	{
	  array_ops[ii][jj] = jj_tree;
	  jj++;
	}
    }

  for (ii = 0; ii < list_size; ii++)
    {
      tree array_node = (*array_list)[ii];
      if (TREE_CODE (array_node) == ARRAY_NOTATION_REF)
	{
	  for (jj = 0; jj < rank; jj++)
	    {
	      if (TREE_CODE (array_ops[ii][jj]) == ARRAY_NOTATION_REF)
		{
		  array_value[ii][jj] =
		    ARRAY_NOTATION_ARRAY (array_ops[ii][jj]);
		  array_start[ii][jj] =
		    ARRAY_NOTATION_START (array_ops[ii][jj]);
		  array_length[ii][jj] =
		    fold_build1 (CONVERT_EXPR, integer_type_node,
				 ARRAY_NOTATION_LENGTH (array_ops[ii][jj]));
		  array_stride[ii][jj] =
		    fold_build1 (CONVERT_EXPR, integer_type_node,
				 ARRAY_NOTATION_STRIDE (array_ops[ii][jj]));
		  array_vector[ii][jj] = true;

		  if (!TREE_CONSTANT (array_length[ii][jj])) 
		    count_down[ii][jj] = false;
		  else if (tree_int_cst_lt
			   (array_length[ii][jj],
			    build_int_cst (TREE_TYPE (array_length[ii][jj]),
					   0)))
		    count_down[ii][jj] = true;
		  else
		    count_down[ii][jj] = false;
		}
	      else
		array_vector[ii][jj] = false;
	    }
	}
    }

  if (length_mismatch_in_expr_p (location, array_length, list_size, rank))
    {
      pop_stmt_list (loop_init);
      return error_mark_node;
    }
  
  for (ii = 0; ii < rank; ii++)
    {
      array_var[ii] = build_decl (location, VAR_DECL, NULL_TREE,
				  integer_type_node);
      ind_init[ii] =
	build_modify_expr (location, array_var[ii],
			   TREE_TYPE (array_var[ii]), NOP_EXPR,
			   location,
			   build_int_cst (TREE_TYPE (array_var[ii]), 0),
			   TREE_TYPE (array_var[ii]));
	
    }
  for (ii = 0; ii < list_size; ii++)
    {
      if (array_vector[ii][0])
	{
	  tree array_opr_node = array_value[ii][rank - 1];
	  for (s_jj = rank - 1; s_jj >= 0; s_jj--)
	    {
	      if (count_down[ii][s_jj])
		/* Array[start_index - (induction_var * stride)] */
		array_opr_node = build_array_ref
		  (location, array_opr_node,
		   build2 (MINUS_EXPR, TREE_TYPE (array_var[s_jj]),
			   array_start[ii][s_jj],
			   build2 (MULT_EXPR, TREE_TYPE (array_var[s_jj]),
				   array_var[s_jj], array_stride[ii][s_jj])));
	      else
		/* Array[start_index + (induction_var * stride)] */
		array_opr_node = build_array_ref
		  (location, array_opr_node,
		   build2 (PLUS_EXPR, TREE_TYPE (array_var[s_jj]),
			   array_start[ii][s_jj],
			   build2 (MULT_EXPR, TREE_TYPE (array_var[s_jj]),
				   array_var[s_jj], array_stride[ii][s_jj])));
	    }
	  vec_safe_push (array_operand, array_opr_node);
	}
      else
	/* This is just a dummy node to make sure the list sizes for both
	   array list and array operand list are the same.  */
	vec_safe_push (array_operand, integer_one_node);
    }
  replace_array_notations (&arg, true, array_list, array_operand);
  for (ii = 0; ii < rank; ii++) 
    expr_incr[ii] = 
      build2 (MODIFY_EXPR, void_type_node, array_var[ii], 
	      build2 (PLUS_EXPR, TREE_TYPE (array_var[ii]), array_var[ii], 
		      build_int_cst (TREE_TYPE (array_var[ii]), 1)));
  
  for (jj = 0; jj < rank; jj++)
    {
      if (rank && expr_incr[jj])
	{
	  if (count_down[0][jj])
	    compare_expr[jj] =
	      build2 (LT_EXPR, boolean_type_node, array_var[jj],
		      build2 (MULT_EXPR, TREE_TYPE (array_var[jj]),
			      array_length[0][jj],
			      build_int_cst (TREE_TYPE (array_var[jj]), -1)));
	  else
	    compare_expr[jj] = build2 (LT_EXPR, boolean_type_node,
				       array_var[jj], array_length[0][jj]);
	}
    }

  loop_init = pop_stmt_list (loop_init);
  append_to_statement_list_force (loop_init, &loop_with_init);
  body = arg;
  for (ii = 0; ii < rank; ii++)
    {
      tree new_loop = push_stmt_list ();
      add_stmt (ind_init[ii]);
      c_finish_loop (location, compare_expr[ii], expr_incr[ii], body, NULL_TREE,
		     NULL_TREE, true);
      body = pop_stmt_list (new_loop);
    }
  append_to_statement_list_force (body, &loop_with_init);
  XDELETEVEC (compare_expr);
  XDELETEVEC (expr_incr);
  XDELETEVEC (ind_init);
  XDELETEVEC (array_var);
  
  for (ii = 0; ii < list_size; ii++)
    {
      XDELETEVEC (count_down[ii]);
      XDELETEVEC (array_value[ii]);
      XDELETEVEC (array_stride[ii]);
      XDELETEVEC (array_length[ii]);
      XDELETEVEC (array_start[ii]);
      XDELETEVEC (array_ops[ii]);
      XDELETEVEC (array_vector[ii]);
    }

  XDELETEVEC (count_down);
  XDELETEVEC (array_value);
  XDELETEVEC (array_stride);
  XDELETEVEC (array_length);
  XDELETEVEC (array_start);
  XDELETEVEC (array_ops);
  XDELETEVEC (array_vector);
  
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

/* Walks through tree node T and find all the call-statements that do not return
   anything and fix up any array notations they may carry.  The return value
   is the same type as T but with all array notations replaced with appropriate
   STATEMENT_LISTS.  */

tree
expand_array_notation_exprs (tree t)
{
  if (!contains_array_notation_expr (t))
    return t;

  switch (TREE_CODE (t))
    {
    case BIND_EXPR:
      t = expand_array_notation_exprs (BIND_EXPR_BODY (t));
      return t;
    case COND_EXPR:
      t = fix_conditional_array_notations (t);

      /* After the expansion if they are still a COND_EXPR, we go into its
	 subtrees.  */
      if (TREE_CODE (t) == COND_EXPR)
	{
	  if (COND_EXPR_THEN (t))
	    COND_EXPR_THEN (t) =
	      expand_array_notation_exprs (COND_EXPR_THEN (t));
	  if (COND_EXPR_ELSE (t))
	    COND_EXPR_ELSE (t) =
	      expand_array_notation_exprs (COND_EXPR_ELSE (t));
	}
      else
	t = expand_array_notation_exprs (t);
      return t;
    case STATEMENT_LIST:
      {
	tree_stmt_iterator ii_tsi;
	for (ii_tsi = tsi_start (t); !tsi_end_p (ii_tsi); tsi_next (&ii_tsi))
	  *tsi_stmt_ptr (ii_tsi) = 
	    expand_array_notation_exprs (*tsi_stmt_ptr (ii_tsi));
      }
      return t;
    case CALL_EXPR:
      t = fix_array_notation_call_expr (t);
      return t;
    case RETURN_EXPR:
      if (contains_array_notation_expr (t))
	t = fix_return_expr (t);
    default:
      return t;
    }
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

/* This function will check if OP is a CALL_EXPR that is a built-in array 
   notation function.  If so, then we will return its type to be the type of
   the array notation inside.  */

tree
find_correct_array_notation_type (tree op)
{
  tree fn_arg, return_type = NULL_TREE;

  if (op)
    {
      return_type = TREE_TYPE (op); /* This is the default case.  */
      if (TREE_CODE (op) == CALL_EXPR) 
	if (is_cilkplus_reduce_builtin (CALL_EXPR_FN (op))) 
	  { 
	    fn_arg = CALL_EXPR_ARG (op, 0); 
	    if (fn_arg) 
	      return_type = TREE_TYPE (fn_arg); 
	  }
    } 
  return return_type;
}
