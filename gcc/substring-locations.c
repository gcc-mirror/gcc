/* Source locations within string literals.
   Copyright (C) 2016-2017 Free Software Foundation, Inc.

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
#include "diagnostic.h"
#include "cpplib.h"
#include "tree.h"
#include "langhooks.h"
#include "substring-locations.h"

/* Emit a warning governed by option OPT, using GMSGID as the format
   string and AP as its arguments.

   Attempt to obtain precise location information within a string
   literal from FMT_LOC.

   Case 1: if substring location is available, and is within the range of
   the format string itself, the primary location of the
   diagnostic is the substring range obtained from FMT_LOC, with the
   caret at the *end* of the substring range.

   For example:

     test.c:90:10: warning: problem with '%i' here [-Wformat=]
     printf ("hello %i", msg);
                    ~^

   Case 2: if the substring location is available, but is not within
   the range of the format string, the primary location is that of the
   format string, and an note is emitted showing the substring location.

   For example:
     test.c:90:10: warning: problem with '%i' here [-Wformat=]
     printf("hello " INT_FMT " world", msg);
            ^~~~~~~~~~~~~~~~~~~~~~~~~
     test.c:19: note: format string is defined here
     #define INT_FMT "%i"
                      ~^

   Case 3: if precise substring information is unavailable, the primary
   location is that of the whole string passed to FMT_LOC's constructor.
   For example:

     test.c:90:10: warning: problem with '%i' here [-Wformat=]
     printf(fmt, msg);
            ^~~

   For each of cases 1-3, if param_loc is not UNKNOWN_LOCATION, then it is used
   as a secondary range within the warning.  For example, here it
   is used with case 1:

     test.c:90:16: warning: '%s' here but arg 2 has 'long' type [-Wformat=]
     printf ("foo %s bar", long_i + long_j);
                  ~^       ~~~~~~~~~~~~~~~

   and here with case 2:

     test.c:90:16: warning: '%s' here but arg 2 has 'long' type [-Wformat=]
     printf ("foo " STR_FMT " bar", long_i + long_j);
             ^~~~~~~~~~~~~~~~~~~~~  ~~~~~~~~~~~~~~~
     test.c:89:16: note: format string is defined here
     #define STR_FMT "%s"
                      ~^

   and with case 3:

     test.c:90:10: warning: '%i' here, but arg 2 is "const char *' [-Wformat=]
     printf(fmt, msg);
            ^~~  ~~~

   If CORRECTED_SUBSTRING is non-NULL, use it for cases 1 and 2 to provide
   a fix-it hint, suggesting that it should replace the text within the
   substring range.  For example:

     test.c:90:10: warning: problem with '%i' here [-Wformat=]
     printf ("hello %i", msg);
                    ~^
                    %s

   Return true if a warning was emitted, false otherwise.  */

ATTRIBUTE_GCC_DIAG (5,0)
bool
format_warning_va (const substring_loc &fmt_loc,
		   location_t param_loc,
		   const char *corrected_substring,
		   int opt, const char *gmsgid, va_list *ap)
{
  bool substring_within_range = false;
  location_t primary_loc;
  location_t fmt_substring_loc = UNKNOWN_LOCATION;
  source_range fmt_loc_range
    = get_range_from_loc (line_table, fmt_loc.get_fmt_string_loc ());
  const char *err = fmt_loc.get_location (&fmt_substring_loc);
  source_range fmt_substring_range
    = get_range_from_loc (line_table, fmt_substring_loc);
  if (err)
    /* Case 3: unable to get substring location.  */
    primary_loc = fmt_loc.get_fmt_string_loc ();
  else
    {
      if (fmt_substring_range.m_start >= fmt_loc_range.m_start
	  && fmt_substring_range.m_start <= fmt_loc_range.m_finish
	  && fmt_substring_range.m_finish >= fmt_loc_range.m_start
	  && fmt_substring_range.m_finish <= fmt_loc_range.m_finish)
	/* Case 1.  */
	{
	  substring_within_range = true;
	  primary_loc = fmt_substring_loc;
	}
      else
	/* Case 2.  */
	{
	  substring_within_range = false;
	  primary_loc = fmt_loc.get_fmt_string_loc ();
	}
    }

  rich_location richloc (line_table, primary_loc);

  if (param_loc != UNKNOWN_LOCATION)
    richloc.add_range (param_loc, false);

  if (!err && corrected_substring && substring_within_range)
    richloc.add_fixit_replace (fmt_substring_range, corrected_substring);

  diagnostic_info diagnostic;
  diagnostic_set_info (&diagnostic, gmsgid, ap, &richloc, DK_WARNING);
  diagnostic.option_index = opt;
  bool warned = diagnostic_report_diagnostic (global_dc, &diagnostic);

  if (!err && fmt_substring_loc && !substring_within_range)
    /* Case 2.  */
    if (warned)
      {
	rich_location substring_richloc (line_table, fmt_substring_loc);
	if (corrected_substring)
	  substring_richloc.add_fixit_replace (fmt_substring_range,
					       corrected_substring);
	inform (&substring_richloc,
		"format string is defined here");
      }

  return warned;
}

/* Variadic call to format_warning_va.  */

bool
format_warning_at_substring (const substring_loc &fmt_loc,
			     location_t param_loc,
			     const char *corrected_substring,
			     int opt, const char *gmsgid, ...)
{
  va_list ap;
  va_start (ap, gmsgid);
  bool warned = format_warning_va (fmt_loc, param_loc, corrected_substring,
				   opt, gmsgid, &ap);
  va_end (ap);

  return warned;
}

/* Attempt to determine the source location of the substring.
   If successful, return NULL and write the source location to *OUT_LOC.
   Otherwise return an error message.  Error messages are intended
   for GCC developers (to help debugging) rather than for end-users.  */

const char *
substring_loc::get_location (location_t *out_loc) const
{
  gcc_assert (out_loc);
  return lang_hooks.get_substring_location (*this, out_loc);
}
