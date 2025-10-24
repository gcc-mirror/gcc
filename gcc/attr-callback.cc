/* Callback attribute handling
   Copyright (C) 2025 Free Software Foundation, Inc.
   Contributed by Josef Melcr <jmelcr@gcc.gnu.org>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "alloc-pool.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "builtins.h"
#include "options.h"
#include "gimple-range.h"
#include "attribs.h"
#include "attr-callback.h"

/* Returns a callback attribute with callback index FN_IDX, and ARG_COUNT
   arguments specified by VA_ARGS.  */
tree
callback_build_attr (unsigned fn_idx, unsigned arg_count...)
{
  va_list args;
  va_start (args, arg_count);

  tree cblist = NULL_TREE;
  tree *pp = &cblist;
  unsigned i;
  for (i = 0; i < arg_count; i++)
    {
      int num = va_arg (args, int);
      tree tnum = build_int_cst (integer_type_node, num);
      *pp = build_tree_list (NULL, tnum);
      pp = &TREE_CHAIN (*pp);
    }
  cblist
    = tree_cons (NULL_TREE, build_int_cst (integer_type_node, fn_idx), cblist);
  tree attr
    = tree_cons (get_identifier (CALLBACK_ATTR_IDENT), cblist, NULL_TREE);
  return attr;
}

/* Returns TRUE if a function should be treated as if it had a callback
   attribute despite the DECL not having it.  STMT can be passed NULL
   if the call statement is not available at the time, for example WPA, but it
   should be called with the statement itself whenever possible.  */
bool
callback_is_special_cased (tree decl, gcall *stmt)
{
  if (fndecl_built_in_p (decl, BUILT_IN_GOMP_TASK))
    {
      if (stmt)
	return gimple_call_arg (stmt, 2) == null_pointer_node;
      return true;
    }
  return false;
}

/* Returns an attribute for a special cased function.  */
tree
callback_special_case_attr (tree decl)
{
  if (fndecl_built_in_p (decl, BUILT_IN_GOMP_TASK))
    return callback_build_attr (1, 1, 2);
  gcc_unreachable ();
}

/* Given an instance of callback attribute, return the 0-based
   index of the called function in question.  */
int
callback_get_fn_index (tree cb_attr)
{
  tree args = TREE_VALUE (cb_attr);
  int idx = TREE_INT_CST_LOW (TREE_VALUE (args)) - 1;
  return idx;
}

/* For a given callback pair, retrieves the callback attribute used
   to create E from the callee of CARRYING.  */
tree
callback_fetch_attr_by_edge (cgraph_edge *e, cgraph_edge *carrying)
{
  gcc_checking_assert (e->call_stmt == carrying->call_stmt
		       && e->lto_stmt_uid == carrying->lto_stmt_uid);

  if (callback_is_special_cased (carrying->callee->decl, e->call_stmt))
    return callback_special_case_attr (carrying->callee->decl);

  tree cb_attr = lookup_attribute (CALLBACK_ATTR_IDENT,
				   DECL_ATTRIBUTES (carrying->callee->decl));
  gcc_checking_assert (cb_attr);
  tree res = NULL_TREE;
  for (; cb_attr;
       cb_attr = lookup_attribute (CALLBACK_ATTR_IDENT, TREE_CHAIN (cb_attr)))
    {
      unsigned id = callback_get_fn_index (cb_attr);
      if (id == e->callback_id)
	{
	  res = cb_attr;
	  break;
	}
    }
  gcc_checking_assert (res != NULL_TREE);
  return res;
}

/* Given an instance of callback attribute, return the 0-base indices
   of arguments passed to the callback.  For a callback function taking
   n parameters, returns a vector of n indices of their values in the parameter
   list of it's caller.  Indices with unknown positions contain -1.  */
auto_vec<int>
callback_get_arg_mapping (cgraph_edge *e, cgraph_edge *carrying)
{
  tree attr = callback_fetch_attr_by_edge (e, carrying);
  gcc_checking_assert (attr);
  tree args = TREE_VALUE (attr);
  auto_vec<int> res;
  tree it;

  /* Skip over the first argument, which denotes
     which argument is the called function.  */
  for (it = TREE_CHAIN (args); it != NULL_TREE; it = TREE_CHAIN (it))
    {
      int idx = TREE_INT_CST_LOW (TREE_VALUE (it));
      /* Subtract 1 to account for 1-based indexing.  If the value is unknown,
	 use constant -1 instead.  */
      idx = idx == CB_UNKNOWN_POS ? -1 : idx - 1;
      res.safe_push (idx);
    }

  return res;
}

/* For a callback pair, returns the 0-based index of the address of
   E's callee in the argument list of CARRYING's callee decl.  */
int
callback_fetch_fn_position (cgraph_edge *e, cgraph_edge *carrying)
{
  tree attr = callback_fetch_attr_by_edge (e, carrying);
  return callback_get_fn_index (attr);
}

/* Returns the element at index idx in the list or NULL_TREE if
   the list isn't long enough.  NULL_TREE is used as the endpoint.  */
static tree
get_nth_list_elem (tree list, unsigned idx)
{
  tree res = NULL_TREE;
  unsigned i = 0;
  tree it;
  for (it = list; it != NULL_TREE; it = TREE_CHAIN (it), i++)
    {
      if (i == idx)
	{
	  res = TREE_VALUE (it);
	  break;
	}
    }
  return res;
}

/* Handle a "callback" attribute; arguments as in
   struct attribute_spec.handler.  */
tree
handle_callback_attribute (tree *node, tree name, tree args,
			   int ARG_UNUSED (flags), bool *no_add_attrs)
{
  tree decl = *node;
  if (TREE_CODE (decl) != FUNCTION_DECL)
    {
      error_at (DECL_SOURCE_LOCATION (decl),
		"%qE attribute can only be used on functions", name);
      *no_add_attrs = true;
    }

  tree cb_fn_idx_node = TREE_VALUE (args);
  if (TREE_CODE (cb_fn_idx_node) != INTEGER_CST)
    {
      error_at (DECL_SOURCE_LOCATION (decl),
		"argument specifying callback function position is not an "
		"integer constant");
      *no_add_attrs = true;
      return NULL_TREE;
    }
  /* We have to use the function type for validation, as
     DECL_ARGUMENTS returns NULL at this point.  */
  int callback_fn_idx = TREE_INT_CST_LOW (cb_fn_idx_node);
  tree decl_type_args = TYPE_ARG_TYPES (TREE_TYPE (decl));
  tree it;
  int decl_nargs = list_length (decl_type_args);
  for (it = decl_type_args; it != NULL_TREE; it = TREE_CHAIN (it))
    if (it == void_list_node)
      {
	--decl_nargs;
	break;
      }
  if (callback_fn_idx == CB_UNKNOWN_POS)
    {
      error_at (DECL_SOURCE_LOCATION (decl),
		"callback function position cannot be marked as unknown");
      *no_add_attrs = true;
      return NULL_TREE;
    }
  --callback_fn_idx;
  if (callback_fn_idx >= decl_nargs)
    {
      error_at (DECL_SOURCE_LOCATION (decl),
		"callback function position out of range");
      *no_add_attrs = true;
      return NULL_TREE;
    }

  /* Search for the type of the callback function
     in parameters of the original function.  */
  tree cfn = get_nth_list_elem (decl_type_args, callback_fn_idx);
  if (cfn == NULL_TREE)
    {
      error_at (DECL_SOURCE_LOCATION (decl),
		"could not retrieve callback function from arguments");
      *no_add_attrs = true;
      return NULL_TREE;
    }
  tree cfn_pointee_type = TREE_TYPE (cfn);
  if (TREE_CODE (cfn) != POINTER_TYPE
      || TREE_CODE (cfn_pointee_type) != FUNCTION_TYPE)
    {
      error_at (DECL_SOURCE_LOCATION (decl),
		"argument no. %d is not an address of a function",
		callback_fn_idx + 1);
      *no_add_attrs = true;
      return NULL_TREE;
    }

  tree type_args = TYPE_ARG_TYPES (cfn_pointee_type);
  /* Compare the length of the list of argument indices
     and the real number of parameters the callback takes.  */
  unsigned cfn_nargs = list_length (TREE_CHAIN (args));
  unsigned type_nargs = list_length (type_args);
  for (it = type_args; it != NULL_TREE; it = TREE_CHAIN (it))
    if (it == void_list_node)
      {
	--type_nargs;
	break;
      }
  if (cfn_nargs != type_nargs)
    {
      error_at (DECL_SOURCE_LOCATION (decl),
		"argument number mismatch, %d expected, got %d", type_nargs,
		cfn_nargs);
      *no_add_attrs = true;
      return NULL_TREE;
    }

  unsigned curr = 0;
  tree cfn_it;
  /* Validate type compatibility of the arguments passed
     from caller function to callback.  "it" is used to step
     through the parameters of the caller, "cfn_it" is
     stepping through the parameters of the callback.  */
  for (it = type_args, cfn_it = TREE_CHAIN (args); curr < type_nargs;
       it = TREE_CHAIN (it), cfn_it = TREE_CHAIN (cfn_it), curr++)
    {
      if (TREE_CODE (TREE_VALUE (cfn_it)) != INTEGER_CST)
	{
	  error_at (DECL_SOURCE_LOCATION (decl),
		    "argument no. %d is not an integer constant", curr + 1);
	  *no_add_attrs = true;
	  continue;
	}

      int arg_idx = TREE_INT_CST_LOW (TREE_VALUE (cfn_it));

      /* No need to check for type compatibility,
	 if we don't know what we are passing.  */
      if (arg_idx == CB_UNKNOWN_POS)
	continue;

      arg_idx -= 1;
      /* Report an error if the position is out of bounds,
	 but we can still check the rest of the arguments.  */
      if (arg_idx >= decl_nargs)
	{
	  error_at (DECL_SOURCE_LOCATION (decl),
		    "callback argument index %d is out of range", arg_idx + 1);
	  *no_add_attrs = true;
	  continue;
	}

      tree arg_type = get_nth_list_elem (decl_type_args, arg_idx);
      tree expected_type = TREE_VALUE (it);
      /* Check the type of the value we are about to pass ("arg_type")
	 for compatibility with the actual type the callback function
	 expects ("expected_type").  */
      if (!types_compatible_p (expected_type, arg_type))
	{
	  error_at (DECL_SOURCE_LOCATION (decl),
		    "argument type at index %d is not compatible with callback "
		    "argument type at index %d",
		    arg_idx + 1, curr + 1);
	  *no_add_attrs = true;
	  continue;
	}
    }

  /* Check that the decl does not already have a callback attribute describing
     the same argument.  */
  it = lookup_attribute (CALLBACK_ATTR_IDENT, DECL_ATTRIBUTES (decl));
  for (; it; it = lookup_attribute (CALLBACK_ATTR_IDENT, TREE_CHAIN (it)))
    if (callback_get_fn_index (it) == callback_fn_idx)
      {
	error_at (DECL_SOURCE_LOCATION (decl),
		  "function declaration has multiple callback attributes "
		  "describing argument no. %d",
		  callback_fn_idx + 1);
	*no_add_attrs = true;
	break;
      }

  return NULL_TREE;
}

/* Returns TRUE if E is considered useful in the callgraph, FALSE otherwise.  If
   this predicate returns FALSE, then E wasn't used to optimize its callee and
   can be safely removed from the callgraph.  */
bool
callback_edge_useful_p (cgraph_edge *e)
{
  gcc_checking_assert (e->callback);
  /* If the edge is not pointing towards a clone, it is no longer useful as its
     entire purpose is to produce clones of callbacks.  */
  if (!e->callee->clone_of)
    return false;
  return true;
}

/* Returns the number of arguments the callback function described by ATTR
   takes.  */

size_t
callback_num_args (tree attr)
{
  tree args = TREE_VALUE (attr);
  size_t res = 0;
  tree it;

  for (it = TREE_CHAIN (args); it != NULL_TREE; it = TREE_CHAIN (it), ++res)
    ;
  return res;
}
