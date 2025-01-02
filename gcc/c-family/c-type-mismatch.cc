/* Implementations of classes for reporting type mismatches.
   Copyright (C) 2014-2025 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "hash-set.h"
#include "vec.h"
#include "input.h"
#include "alias.h"
#include "symtab.h"
#include "inchash.h"
#include "tree-core.h"
#include "tree.h"
#include "diagnostic-core.h"
#include "c-family/c-type-mismatch.h"
#include "print-tree.h"
#include "pretty-print.h"
#include "intl.h"
#include "cpplib.h"
#include "diagnostic.h"
#include "diagnostic-highlight-colors.h"

/* Implementation of range_label::get_text for
   maybe_range_label_for_tree_type_mismatch.

   If both expressions are non-NULL, then generate text describing
   the first expression's type (using the other expression's type
   for comparison, analogous to %H and %I in the C++ frontend, but
   on expressions rather than types).  */

label_text
maybe_range_label_for_tree_type_mismatch::get_text (unsigned range_idx) const
{
  if (m_expr == NULL_TREE
      || !EXPR_P (m_expr))
    return label_text::borrow (NULL);
  tree expr_type = TREE_TYPE (m_expr);

  tree other_type = NULL_TREE;
  if (m_other_expr && EXPR_P (m_other_expr))
    other_type = TREE_TYPE (m_other_expr);

  range_label_for_type_mismatch inner (expr_type, other_type);
  return inner.get_text (range_idx);
}

/* binary_op_rich_location's ctor.

   If use_operator_loc_p (LOC, ARG0, ARG1), then attempt to make a 3-location
   rich_location of the form:

     arg_0 op arg_1
     ~~~~~ ^~ ~~~~~
       |        |
       |        arg1 type
       arg0 type

   labelling the types of the arguments if SHOW_TYPES is true,
   and using highlight_colors::lhs and highlight_colors::rhs for the ranges.

   Otherwise, make a 1-location rich_location using the compound
   location within LOC:

     arg_0 op arg_1
     ~~~~~~^~~~~~~~

   for which we can't label the types.  */

binary_op_rich_location::binary_op_rich_location (const op_location_t &loc,
						  tree arg0, tree arg1,
						  bool show_types)
: gcc_rich_location (loc.m_combined_loc),
  m_label_for_arg0 (arg0, arg1),
  m_label_for_arg1 (arg1, arg0)
{
  /* Default (above) to using the combined loc.
     Potentially override it here: if we have location information for the
     operator and for both arguments, then split them all out.
     Alternatively, override it if we don't have the combined location.  */
  if (use_operator_loc_p (loc, arg0, arg1))
    {
      set_range (0, loc.m_operator_loc, SHOW_RANGE_WITH_CARET);
      maybe_add_expr (arg0, show_types ? &m_label_for_arg0 : NULL,
		      highlight_colors::lhs);
      maybe_add_expr (arg1, show_types ? &m_label_for_arg1 : NULL,
		      highlight_colors::rhs);
    }
}

/* Determine if binary_op_rich_location's ctor should attempt to make
   a 3-location rich_location (the location of the operator and of
   the 2 arguments), or fall back to a 1-location rich_location showing
   just the combined location of the operation as a whole.  */

bool
binary_op_rich_location::use_operator_loc_p (const op_location_t &loc,
					     tree arg0, tree arg1)
{
  /* If we don't have a combined location, then use the operator location,
     and try to add ranges for the operators.  */
  if (loc.m_combined_loc == UNKNOWN_LOCATION)
    return true;

  /* If we don't have the operator location, then use the
     combined location.  */
  if (loc.m_operator_loc == UNKNOWN_LOCATION)
    return false;

  /* We have both operator location and combined location: only use the
     operator location if we have locations for both arguments.  */
  return (EXPR_HAS_LOCATION (arg0)
	  && EXPR_HAS_LOCATION (arg1));
}
