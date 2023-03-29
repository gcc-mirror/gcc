/* Implementation of gcc_rich_location class
   Copyright (C) 2014-2023 Free Software Foundation, Inc.

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
#include "rtl.h"
#include "hash-set.h"
#include "vec.h"
#include "input.h"
#include "alias.h"
#include "symtab.h"
#include "inchash.h"
#include "tree-core.h"
#include "tree.h"
#include "diagnostic-core.h"
#include "gcc-rich-location.h"
#include "print-tree.h"
#include "pretty-print.h"
#include "intl.h"
#include "cpplib.h"
#include "diagnostic.h"

/* Add a range to the rich_location, covering expression EXPR,
   using LABEL if non-NULL. */

void
gcc_rich_location::add_expr (tree expr, range_label *label)
{
  gcc_assert (expr);

  if (CAN_HAVE_RANGE_P (expr))
    add_range (EXPR_LOCATION (expr), SHOW_RANGE_WITHOUT_CARET, label);
}

/* If T is an expression, add a range for it to the rich_location,
   using LABEL if non-NULL. */

void
gcc_rich_location::maybe_add_expr (tree t, range_label *label)
{
  if (EXPR_P (t))
    add_expr (t, label);
}

/* Add a fixit hint suggesting replacing the range at MISSPELLED_TOKEN_LOC
   with the identifier HINT_ID.  */

void
gcc_rich_location::add_fixit_misspelled_id (location_t misspelled_token_loc,
					    tree hint_id)
{
  gcc_assert (TREE_CODE (hint_id) == IDENTIFIER_NODE);

  add_fixit_replace (misspelled_token_loc, IDENTIFIER_POINTER (hint_id));
}

/* Return true if there is nothing on LOC's line before LOC.  */

static bool
blank_line_before_p (location_t loc)
{
  expanded_location exploc = expand_location (loc);
  char_span line = location_get_source_line (exploc.file, exploc.line);
  if (!line)
    return false;
  if (line.length () < (size_t)exploc.column)
    return false;
  /* Columns are 1-based.  */
  for (int column = 1; column < exploc.column; ++column)
    if (!ISSPACE (line[column - 1]))
      return false;
  return true;
}

/* Subroutine of gcc_rich_location::add_fixit_insert_formatted.
   Return true if we should add the content on its own line,
   false otherwise.
   If true is returned then *OUT_START_OF_LINE is written to.  */

static bool
use_new_line (location_t insertion_point, location_t indent,
	      location_t *out_start_of_line)
{
  if (indent == UNKNOWN_LOCATION)
    return false;
  const line_map *indent_map = linemap_lookup (line_table, indent);
  if (linemap_macro_expansion_map_p (indent_map))
    return false;

  if (!blank_line_before_p (insertion_point))
    return false;

  /* Locate the start of the line containing INSERTION_POINT.  */
  const line_map *insertion_point_map
    = linemap_lookup (line_table, insertion_point);
  if (linemap_macro_expansion_map_p (insertion_point_map))
    return false;
  const line_map_ordinary *ordmap
    = linemap_check_ordinary (insertion_point_map);
  expanded_location exploc_insertion_point = expand_location (insertion_point);
  location_t start_of_line
    = linemap_position_for_line_and_column (line_table, ordmap,
					    exploc_insertion_point.line, 1);
  *out_start_of_line = start_of_line;
  return true;
}

/* Add a fix-it hint suggesting the insertion of CONTENT before
   INSERTION_POINT.

   Attempt to handle formatting: if INSERTION_POINT is the first thing on
   its line, and INDENT is sufficiently sane, then add CONTENT on its own
   line, using the indentation of INDENT.
   Otherwise, add CONTENT directly before INSERTION_POINT.

   For example, adding "CONTENT;" with the closing brace as the insertion
   point and "INDENT;" as the indentation point:

   if ()
     {
       INDENT;
     }

  would lead to:

   if ()
     {
       INDENT;
       CONTENT;
     }

  but adding it to:

    if () {INDENT;}

  would lead to:

    if () {INDENT;CONTENT;}
*/

void
gcc_rich_location::add_fixit_insert_formatted (const char *content,
					       location_t insertion_point,
					       location_t indent)
{
  location_t start_of_line;
  if (use_new_line (insertion_point, indent, &start_of_line))
    {
      /* Add CONTENT on its own line, using the indentation of INDENT.  */

      /* Generate an insertion string, indenting by the amount INDENT
	 was indented.  */
      int indent_column = LOCATION_COLUMN (get_start (indent));
      pretty_printer tmp_pp;
      pretty_printer *pp = &tmp_pp;
      /* Columns are 1-based.  */
      for (int column = 1; column < indent_column; ++column)
	pp_space (pp);
      pp_string (pp, content);
      pp_newline (pp);

      add_fixit_insert_before (start_of_line, pp_formatted_text (pp));
    }
  else
    add_fixit_insert_before (insertion_point, content);
}

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

   labelling the types of the arguments if SHOW_TYPES is true.

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
      maybe_add_expr (arg0, show_types ? &m_label_for_arg0 : NULL);
      maybe_add_expr (arg1, show_types ? &m_label_for_arg1 : NULL);
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
