/* Declarations relating to class gcc_rich_location
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

#ifndef GCC_RICH_LOCATION_H
#define GCC_RICH_LOCATION_H

#include "rich-location.h"

class diagnostic_source_print_policy;

/* A gcc_rich_location is libcpp's rich_location with additional
   helper methods for working with gcc's types.  The class is not
   copyable or assignable because rich_location isn't. */

class gcc_rich_location : public rich_location
{
 public:
  /* Constructors.  */

  /* Constructing from a location.  */
  explicit gcc_rich_location (location_t loc)
    : rich_location (line_table, loc, nullptr, nullptr)
  {
  }

  /* Constructing from a location with a label and a highlight color.  */
  explicit gcc_rich_location (location_t loc,
			      const range_label *label,
			      const char *highlight_color)
    : rich_location (line_table, loc, label, highlight_color)
  {
  }

  /* Methods for adding ranges via gcc entities.  */
  void
  add_expr (tree expr,
	    range_label *label,
	    const char *highlight_color);

  void
  maybe_add_expr (tree t,
		  range_label *label,
		  const char *highlight_color);

  void add_fixit_misspelled_id (location_t misspelled_token_loc,
				tree hint_id);

  /* If LOC is within the spans of lines that will already be printed for
     this gcc_rich_location, then add it as a secondary location
     and return true.

     Otherwise return false.

     This allows for a diagnostic to compactly print secondary locations
     in one diagnostic when these are near enough the primary locations for
     diagnostics-show-locus.c to cope with them, and to fall back to
     printing them via a note otherwise e.g.:

	gcc_rich_location richloc (primary_loc);
	bool added secondary = richloc.add_location_if_nearby (*global_dc,
							       secondary_loc);
	error_at (&richloc, "main message");
	if (!added secondary)
	  inform (secondary_loc, "message for secondary");

     Implemented in diagnostic-show-locus.cc.  */

  bool add_location_if_nearby (const diagnostic_source_print_policy &policy,
			       location_t loc,
			       bool restrict_to_current_line_spans = true,
			       const range_label *label = NULL);

  bool add_location_if_nearby (const diagnostic_context &dc,
			       location_t loc,
			       bool restrict_to_current_line_spans = true,
			       const range_label *label = NULL);

  /* Add a fix-it hint suggesting the insertion of CONTENT before
     INSERTION_POINT.

     Attempt to handle formatting: if INSERTION_POINT is the first thing on
     its line, and INDENT is sufficiently sane, then add CONTENT on its own
     line, using the indentation of INDENT.
     Otherwise, add CONTENT directly before INSERTION_POINT.

     For example, adding "CONTENT;" with the closing brace as the insertion
     point and using "INDENT;" for indentation:

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
  void add_fixit_insert_formatted (const char *content,
				   location_t insertion_point,
				   location_t indent);
};

#endif /* GCC_RICH_LOCATION_H */
