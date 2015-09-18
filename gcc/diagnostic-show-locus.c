/* Diagnostic subroutines for printing source-code
   Copyright (C) 1999-2015 Free Software Foundation, Inc.
   Contributed by Gabriel Dos Reis <gdr@codesourcery.com>

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
#include "version.h"
#include "demangle.h"
#include "intl.h"
#include "backtrace.h"
#include "diagnostic.h"
#include "diagnostic-color.h"

#ifdef HAVE_TERMIOS_H
# include <termios.h>
#endif

#ifdef GWINSZ_IN_SYS_IOCTL
# include <sys/ioctl.h>
#endif

/* If LINE is longer than MAX_WIDTH, and COLUMN is not smaller than
   MAX_WIDTH by some margin, then adjust the start of the line such
   that the COLUMN is smaller than MAX_WIDTH minus the margin.  The
   margin is either CARET_LINE_MARGIN characters or the difference
   between the column and the length of the line, whatever is smaller.
   The length of LINE is given by LINE_WIDTH.  */
static const char *
adjust_line (const char *line, int line_width,
	     int max_width, int *column_p)
{
  int right_margin = CARET_LINE_MARGIN;
  int column = *column_p;

  gcc_checking_assert (line_width >= column);
  right_margin = MIN (line_width - column, right_margin);
  right_margin = max_width - right_margin;
  if (line_width >= max_width && column > right_margin)
    {
      line += column - right_margin;
      *column_p = right_margin;
    }
  return line;
}

/* Print the physical source line corresponding to the location of
   this diagnostic, and a caret indicating the precise column.  This
   function only prints two caret characters if the two locations
   given by DIAGNOSTIC are on the same line according to
   diagnostic_same_line().  */
void
diagnostic_show_locus (diagnostic_context * context,
		       const diagnostic_info *diagnostic)
{
  if (!context->show_caret
      || diagnostic_location (diagnostic, 0) <= BUILTINS_LOCATION
      || diagnostic_location (diagnostic, 0) == context->last_location)
    return;

  context->last_location = diagnostic_location (diagnostic, 0);
  expanded_location s0 = diagnostic_expand_location (diagnostic, 0);
  expanded_location s1 = { };
  /* Zero-initialized. This is checked later by diagnostic_print_caret_line.  */

  if (diagnostic_location (diagnostic, 1) > BUILTINS_LOCATION)
    s1 = diagnostic_expand_location (diagnostic, 1);

  diagnostic_print_caret_line (context, s0, s1,
			       context->caret_chars[0],
			       context->caret_chars[1]);
}

/* Print (part) of the source line given by xloc1 with caret1 pointing
   at the column.  If xloc2.column != 0 and it fits within the same
   line as xloc1 according to diagnostic_same_line (), then caret2 is
   printed at xloc2.colum.  Otherwise, the caller has to set up things
   to print a second caret line for xloc2.  */
void
diagnostic_print_caret_line (diagnostic_context * context,
			     expanded_location xloc1,
			     expanded_location xloc2,
			     char caret1, char caret2)
{
  if (!diagnostic_same_line (context, xloc1, xloc2))
    /* This will mean ignore xloc2.  */
    xloc2.column = 0;
  else if (xloc1.column == xloc2.column)
    xloc2.column++;

  int cmax = MAX (xloc1.column, xloc2.column);
  int line_width;
  const char *line = location_get_source_line (xloc1.file, xloc1.line,
					       &line_width);
  if (line == NULL || cmax > line_width)
    return;

  /* Center the interesting part of the source line to fit in
     max_width, and adjust all columns accordingly.  */
  int max_width = context->caret_max_width;
  int offset = (int) cmax;
  line = adjust_line (line, line_width, max_width, &offset);
  offset -= cmax;
  cmax += offset;
  xloc1.column += offset;
  if (xloc2.column)
    xloc2.column += offset;

  /* Print the source line.  */
  pp_newline (context->printer);
  const char *saved_prefix = pp_get_prefix (context->printer);
  pp_set_prefix (context->printer, NULL);
  pp_space (context->printer);
  while (max_width > 0 && line_width > 0)
    {
      char c = *line == '\t' ? ' ' : *line;
      if (c == '\0')
	c = ' ';
      pp_character (context->printer, c);
      max_width--;
      line_width--;
      line++;
    }
  pp_newline (context->printer);

  /* Print the caret under the line.  */
  const char *caret_cs, *caret_ce;
  caret_cs = colorize_start (pp_show_color (context->printer), "caret");
  caret_ce = colorize_stop (pp_show_color (context->printer));
  int cmin = xloc2.column
    ? MIN (xloc1.column, xloc2.column) : xloc1.column;
  int caret_min = cmin == xloc1.column ? caret1 : caret2;
  int caret_max = cmin == xloc1.column ? caret2 : caret1;

  /* cmin is >= 1, but we indent with an extra space at the start like
     we did above.  */
  int i;
  for (i = 0; i < cmin; i++)
    pp_space (context->printer);
  pp_printf (context->printer, "%s%c%s", caret_cs, caret_min, caret_ce);

  if (xloc2.column)
    {
      for (i++; i < cmax; i++)
	pp_space (context->printer);
      pp_printf (context->printer, "%s%c%s", caret_cs, caret_max, caret_ce);
    }
  pp_set_prefix (context->printer, saved_prefix);
  pp_needs_newline (context->printer) = true;
}
