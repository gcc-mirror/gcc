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

#ifndef ATTR_CALLBACK_H
#define ATTR_CALLBACK_H

/* This enum relates to the attribute's arguments.  */

enum callback_position
{
  /* Value used when an argument of a callback function
     is unknown or when multiple values may be used. */
  CB_UNKNOWN_POS = 0
};

/* This enum describes values present in the arg_mapping vector.  */

enum arg_mapping_constants
{
  /* Value representing an unknown argument position in the arg_mapping
     vector.  */
  ARG_MAPPING_UNKNOWN_IDX = -1
};

/* Returns a callback attribute with callback index FN_IDX, and ARG_COUNT
   arguments specified by VA_ARGS.  */
tree callback_build_attr (unsigned fn_idx, unsigned arg_count...);

/* Returns TRUE if a function should be treated as if it had a callback
   attribute despite the DECL not having it.  STMT can be passed NULL
   if the call statement is not available at the time, for example WPA, but it
   should be called with the statement itself whenever possible.  */
bool callback_is_special_cased (tree decl, gcall *stmt);

/* Returns an attribute for a special cased function.  */
tree callback_special_case_attr (tree decl);

/* Returns TRUE if the callee of E has a callback attribute.  */
bool callback_edge_callee_has_attr (cgraph_edge *e);

/* Given an instance of callback attribute, return the 0-based
   index of the called function in question.  */
int callback_get_fn_index (tree cb_attr);

/* For a given callback pair, retrieves the callback attribute used
   to create E from the callee of CARRYING.  */
tree callback_fetch_attr_by_edge (cgraph_edge *e, cgraph_edge *carrying);

/* Returns the argument mapping from the dispatching function to the callback
   function parsed from the attribute.  */
auto_vec<int> callback_get_arg_mapping_from_attr (tree attr);

/* Returns TRUE if E is considered useful in the callgraph, FALSE otherwise.  If
   this predicate returns FALSE, then E wasn't used to optimize its callee and
   can be safely removed from the callgraph.  */
bool callback_edge_useful_p (cgraph_edge *e);

/* Returns the number of arguments the callback function described by ATTR
   takes.  */
size_t callback_num_args (tree attr);

#endif /* ATTR_CALLBACK_H  */
