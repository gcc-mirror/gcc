/* Options relating to printing the user's source code.
   Copyright (C) 2000-2026 Free Software Foundation, Inc.

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

#ifndef GCC_DIAGNOSTICS_SOURCE_PRINTING_OPTIONS_H
#define GCC_DIAGNOSTICS_SOURCE_PRINTING_OPTIONS_H

#include "rich-location.h"

namespace diagnostics {

/* A bundle of options relating to printing the user's source code
   (potentially with a margin, underlining, labels, etc).  */

struct source_printing_options
{
  /* True if we should print the source line with a caret indicating
     the location.
     Corresponds to -fdiagnostics-show-caret.  */
  bool enabled;

  /* Maximum width of the source line printed.  */
  int max_width;

  /* Character used at the caret when printing source locations.  */
  char caret_chars[rich_location::STATICALLY_ALLOCATED_RANGES];

  /* When printing source code, should the characters at carets and ranges
     be colorized? (assuming colorization is on at all).
     This should be true for frontends that generate range information
     (so that the ranges of code are colorized),
     and false for frontends that merely specify points within the
     source code (to avoid e.g. colorizing just the first character in
     a token, which would look strange).  */
  bool colorize_source_p;

  /* When printing source code, should labelled ranges be printed?
     Corresponds to -fdiagnostics-show-labels.  */
  bool show_labels_p;

  /* When printing source code, should there be a left-hand margin
     showing line numbers?
     Corresponds to -fdiagnostics-show-line-numbers.  */
  bool show_line_numbers_p;

  /* If printing source code, what should the minimum width of the margin
     be?  Line numbers will be right-aligned, and padded to this width.
     Corresponds to -fdiagnostics-minimum-margin-width=VALUE.  */
  int min_margin_width;

  /* Usable by plugins; if true, print a debugging ruler above the
     source output.  */
  bool show_ruler_p;

  /* When printing events in an inline path, should we print lines
     visualizing links between related events (e.g. for CFG paths)?
     Corresponds to -fdiagnostics-show-event-links.  */
  bool show_event_links_p;
};

} // namespace diagnostics

#endif /* ! GCC_DIAGNOSTICS_SOURCE_PRINTING_OPTIONS_H */
