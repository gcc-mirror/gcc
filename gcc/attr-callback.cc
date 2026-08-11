/* Callback attribute handling
   Copyright (C) 2025-2026 Free Software Foundation, Inc.
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
#include "symbol-summary.h"
#include "lto-streamer.h"
#include "attr-callback.h"
#include "callback-info.h"

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
    = tree_cons (get_identifier ("callback_only"), cblist, NULL_TREE);
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

/* Returns TRUE if the callee of E has a callback attribute.  */
bool
callback_edge_callee_has_attr (cgraph_edge *e)
{
  return lookup_attribute ("callback_only",
			   DECL_ATTRIBUTES (e->callee->decl))
	 || callback_is_special_cased (e->callee->decl, e->call_stmt);
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

  tree cb_attr = lookup_attribute ("callback_only",
				   DECL_ATTRIBUTES (carrying->callee->decl));
  gcc_checking_assert (cb_attr);
  callback_info *ci = callback_info_sum->get (e);
  tree res = NULL_TREE;
  for (; cb_attr;
       cb_attr = lookup_attribute ("callback_only", TREE_CHAIN (cb_attr)))
    {
      unsigned fn_idx = callback_get_fn_index (cb_attr);
      if (fn_idx == ci->get_id ())
	{
	  res = cb_attr;
	  break;
	}
    }
  gcc_checking_assert (res != NULL_TREE);
  return res;
}

/* Returns the argument mapping from the dispatching function to the callback
   function parsed from the attribute.  */
auto_vec<int>
callback_get_arg_mapping_from_attr (tree attr)
{
  tree args = TREE_VALUE (attr);
  auto_vec<int> res;
  tree it;

  /* Skip over the first argument, which denotes
     which argument is the called function.  */
  for (it = TREE_CHAIN (args); it != NULL_TREE; it = TREE_CHAIN (it))
    {
      int idx = TREE_INT_CST_LOW (TREE_VALUE (it));
      /* Subtract 1 to account for 1-based indexing.  If the value is unknown,
	 use ARG_MAPPING_UNKNOWN_IDX instead.  */
      idx = idx == CB_UNKNOWN_POS ? ARG_MAPPING_UNKNOWN_IDX : idx - 1;
      res.safe_push (idx);
    }

  return res;
}

/* Returns TRUE if E is considered useful in the callgraph, FALSE otherwise.  If
   this predicate returns FALSE, then E wasn't used to optimize its callee and
   can be safely removed from the callgraph.  */
bool
callback_edge_useful_p (cgraph_edge *e)
{
  gcc_checking_assert (e->callback);
  callback_info *ci = callback_info_sum->get (e);
  return ci->redirected;
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
