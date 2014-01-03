/* This file is part of the Intel(R) Cilk(TM) Plus support
   This file contains the builtin functions for Array
   notations.
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

#include "config.h"
#include "system.h" 
#include "coretypes.h"
#include "tree.h"
#include "langhooks.h" 
#include "tree-iterator.h"
#include "c-family/c-common.h"
#include "diagnostic-core.h"

/* Returns true if the function call in FNDECL is  __sec_implicit_index.  */

bool
is_sec_implicit_index_fn (tree fndecl)
{
  if (TREE_CODE (fndecl) == ADDR_EXPR)
    fndecl = TREE_OPERAND (fndecl, 0);

  return
    (TREE_CODE (fndecl) == FUNCTION_DECL
     && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
     && DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CILKPLUS_SEC_IMPLICIT_INDEX);
}

/* Returns the first and only argument for FN, which should be a
   sec_implicit_index function.  FN's location in the source file is as 
   indicated by LOCATION.  The argument to FN must be a constant integer
   expression, otherwise returns -1.  */

HOST_WIDE_INT
extract_sec_implicit_index_arg (location_t location, tree fn)
{
  tree fn_arg;
  HOST_WIDE_INT return_int = 0;

  if (TREE_CODE (fn) == CALL_EXPR)
    {
      fn_arg = CALL_EXPR_ARG (fn, 0);
      if (TREE_CODE (fn_arg) == INTEGER_CST)
	return_int = int_cst_value (fn_arg);
      else
	{
	  /* If the location is unknown, and if fn has a location, then use that
	     information so that the user has a better idea where the error
	     could be.  */
	  if (location == UNKNOWN_LOCATION && EXPR_HAS_LOCATION (fn))
	    location = EXPR_LOCATION (fn);
	  error_at (location, "__sec_implicit_index parameter must be an " 
		    "integer constant expression");
	  return -1;
	}
    }
  return return_int;
}

/* Returns true if there is a length mismatch among exprssions that are at the
   same dimension and one the same side of the equal sign.  The Array notation
   lengths (LIST->LENGTH) is passed in as a 2D vector of trees.  */

bool
length_mismatch_in_expr_p (location_t loc, vec<vec<an_parts> >list)
{
  size_t ii, jj;
  tree length = NULL_TREE;
  
  size_t x = list.length ();
  size_t y = list[0].length ();
  
  for (jj = 0; jj < y; jj++)
    {
      length = NULL_TREE;
      for (ii = 0; ii < x; ii++)
	{
	  if (!length)
	    length = list[ii][jj].length;
	  else if (TREE_CODE (length) == INTEGER_CST)
	    {
	      /* If length is a INTEGER, and list[ii][jj] is an integer then
		 check if they are equal.  If they are not equal then return
		 true.  */
	      if (TREE_CODE (list[ii][jj].length) == INTEGER_CST
		  && !tree_int_cst_equal (list[ii][jj].length, length))
		{ 
		  error_at (loc, "length mismatch in expression"); 
		  return true;
		}
	    }
	  else
	    /* We set the length node as the current node just in case it turns
	       out to be an integer.  */
	    length = list[ii][jj].length;
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
  if (!fndecl)
    return BUILT_IN_NONE;
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
	  if (!find_rank (loc, orig_expr, func_name, ignore_builtin_fn, rank))
	    return false;
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

void
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
      /* This will extract array notations in function pointers.  */
      extract_array_notation_exprs (CALL_EXPR_FN (node), ignore_builtin_fn,
				    array_list);
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

void
replace_array_notations (tree *orig, bool ignore_builtin_fn,
			 vec<tree, va_gc> *list,
			 vec<tree, va_gc> *array_operand)
{
  size_t ii = 0;
  extern tree build_c_cast (location_t, tree, tree);
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
		*orig = build_c_cast (EXPR_LOCATION (*orig),
				      TREE_TYPE (*orig), node_replacement);
	      }
	  return;
	}
      /* Fixes array notations in array notations in function pointers.  */
      replace_array_notations (&CALL_EXPR_FN (*orig), ignore_builtin_fn, list,
			       array_operand);
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

tree
find_inv_trees (tree *tp, int *walk_subtrees, void *data)
{
  struct inv_list *i_list = (struct inv_list *) data;
  unsigned int ii = 0;

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
    {
      *walk_subtrees = 1;

      /* This function is used by C and C++ front-ends.  In C++, additional
	 tree codes such as TARGET_EXPR must be eliminated.  These codes are
	 passed into additional_tcodes and are walked through and checked.  */
      for (ii = 0; ii < vec_safe_length (i_list->additional_tcodes); ii++)
	if (TREE_CODE (*tp) == (*(i_list->additional_tcodes))[ii])
	  *walk_subtrees = 0;
    }
  return NULL_TREE;
}

/* Callback for walk_tree.  Replace all the scalar expressions in *TP with the 
   appropriate replacement stored in the struct *DATA (typecasted to void*).  
   The subtrees are not touched if *WALK_SUBTREES is set to zero.  */

tree
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

/* Extracts all the array notation triplet information from LIST and stores
   them in the following fields of the 2-D array NODE(size x rank):
   START, LENGTH and STRIDE, holding the starting index, length, and stride,
   respectively.  In addition, it also sets two bool fields, IS_VECTOR and
   COUNT_DOWN, in NODE indicating whether a certain value at a certain field
   is a vector and if the array is accessed from high to low.  */

void
cilkplus_extract_an_triplets (vec<tree, va_gc> *list, size_t size, size_t rank,
			      vec<vec<struct cilkplus_an_parts> > *node)
{
  vec<vec<tree> > array_exprs = vNULL;

  node->safe_grow_cleared (size);
  array_exprs.safe_grow_cleared (size);

  if (rank > 0)
    for (size_t ii = 0; ii < size; ii++)
      {
	(*node)[ii].safe_grow_cleared (rank);
	array_exprs[ii].safe_grow_cleared (rank);
      }
  for (size_t ii = 0; ii < size; ii++)
    {
      size_t jj = 0;
      tree ii_tree = (*list)[ii];
      while (ii_tree)
	{
	  if (TREE_CODE (ii_tree) == ARRAY_NOTATION_REF)
	    {
	      array_exprs[ii][jj] = ii_tree;
	      jj++;
	      ii_tree = ARRAY_NOTATION_ARRAY (ii_tree);
	    }
	  else if (TREE_CODE (ii_tree) == ARRAY_REF)
	    ii_tree = TREE_OPERAND (ii_tree, 0);
	  else
	    break;
	}
    }
    for (size_t ii = 0; ii < size; ii++)
      if (TREE_CODE ((*list)[ii]) == ARRAY_NOTATION_REF)
	for (size_t jj = 0; jj < rank; jj++)
	  {
	    tree ii_tree = array_exprs[ii][jj];
	    (*node)[ii][jj].is_vector = true;
	    (*node)[ii][jj].value = ARRAY_NOTATION_ARRAY (ii_tree);
	    (*node)[ii][jj].start = ARRAY_NOTATION_START (ii_tree);
	    (*node)[ii][jj].length =
	      fold_build1 (CONVERT_EXPR, integer_type_node,
			   ARRAY_NOTATION_LENGTH (ii_tree));
	    (*node)[ii][jj].stride =
	      fold_build1 (CONVERT_EXPR, integer_type_node,
			   ARRAY_NOTATION_STRIDE (ii_tree));
	  }
}

/* Replaces all the __sec_implicit_arg functions in LIST with the induction
   variable stored in VAR at the appropriate location pointed by the
   __sec_implicit_arg's first parameter.  Emits an error if the parameter is
   not between 0 and RANK.  */

vec <tree, va_gc> *
fix_sec_implicit_args (location_t loc, vec <tree, va_gc> *list,
		       vec<an_loop_parts> an_loop_info, size_t rank,
		       tree orig_stmt)
{
  vec <tree, va_gc> *array_operand = NULL;
  for (size_t ii = 0; ii < vec_safe_length (list); ii++)
    if (TREE_CODE ((*list)[ii]) == CALL_EXPR
	&& is_sec_implicit_index_fn (CALL_EXPR_FN ((*list)[ii])))
      {
	int idx = extract_sec_implicit_index_arg (loc, (*list)[ii]);
	if (idx < 0)
	  /* In this case, the returning function would have emitted an
	     error thus it is not necessary to do so again.  */
	  return NULL;
	else if (idx < (int) rank)
	  vec_safe_push (array_operand, an_loop_info[idx].var);
	else
	  {
	    error_at (loc, "__sec_implicit_index argument %d must be "
		      "less than the rank of %qE", idx, orig_stmt);
	    return NULL;
	  }
      }
    else
      /* Save the existing value into the array operand.  */
      vec_safe_push (array_operand, (*list)[ii]);
  return array_operand;
}
