/* Implementation of gcc_rich_location class
   Copyright (C) 2014-2024 Free Software Foundation, Inc.

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
gcc_rich_location::add_expr (tree expr,
			     range_label *label,
			     const char *highlight_color)
{
  gcc_assert (expr);

  if (CAN_HAVE_RANGE_P (expr))
    add_range (EXPR_LOCATION (expr), SHOW_RANGE_WITHOUT_CARET, label,
	       highlight_color);
}

/* If T is an expression, add a range for it to the rich_location,
   using LABEL if non-NULL. */

void
gcc_rich_location::maybe_add_expr (tree t, range_label *label,
				   const char *highlight_color)
{
  if (EXPR_P (t))
    add_expr (t, label, highlight_color);
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
blank_line_before_p (file_cache &fc,
		     location_t loc)
{
  expanded_location exploc = expand_location (loc);
  char_span line = fc.get_source_line (exploc.file, exploc.line);
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
use_new_line (file_cache &fc,
	      location_t insertion_point, location_t indent,
	      location_t *out_start_of_line)
{
  if (indent == UNKNOWN_LOCATION)
    return false;
  const line_map *indent_map = linemap_lookup (line_table, indent);
  if (linemap_macro_expansion_map_p (indent_map))
    return false;

  if (!blank_line_before_p (fc, insertion_point))
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
  if (use_new_line (global_dc->get_file_cache (),
		    insertion_point, indent, &start_of_line))
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
