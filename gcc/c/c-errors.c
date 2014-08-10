/* Various diagnostic subroutines for the GNU C language.
   Copyright (C) 2000-2014 Free Software Foundation, Inc.
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
#include "tm.h"
#include "tree.h"
#include "c-tree.h"
#include "tm_p.h"
#include "flags.h"
#include "diagnostic.h"

/* Issue an ISO C99 pedantic warning MSGID.  */

void
pedwarn_c99 (location_t location, int opt, const char *gmsgid, ...)
{
  diagnostic_info diagnostic;
  va_list ap;

  va_start (ap, gmsgid);
  diagnostic_set_info (&diagnostic, gmsgid, &ap, location,
		       flag_isoc99 ? DK_PEDWARN : DK_WARNING);
  diagnostic.option_index = opt;
  report_diagnostic (&diagnostic);
  va_end (ap);
}

/* Issue an ISO C90 pedantic warning MSGID if -pedantic outside C99 mode,
   otherwise issue warning MSGID if -Wc90-c99-compat is specified, or if
   a specific option such as -Wlong-long is specified.
   This function is supposed to be used for matters that are allowed in
   ISO C99 but not supported in ISO C90, thus we explicitly don't pedwarn
   when C99 is specified.  (There is no flag_c90.)  */

void
pedwarn_c90 (location_t location, int opt, const char *gmsgid, ...)
{
  diagnostic_info diagnostic;
  va_list ap;
  bool warned = false;

  va_start (ap, gmsgid);
  if (pedantic && !flag_isoc99)
    {
      diagnostic_set_info (&diagnostic, gmsgid, &ap, location, DK_PEDWARN);
      diagnostic.option_index = opt;
      warned = report_diagnostic (&diagnostic);
    }
  else if (opt != OPT_Wpedantic)
    {
      diagnostic_set_info (&diagnostic, gmsgid, &ap, location, DK_WARNING);
      diagnostic.option_index = opt;
      warned = report_diagnostic (&diagnostic);
    }
  if (warn_c90_c99_compat && !warned)
    {
      diagnostic_set_info (&diagnostic, gmsgid, &ap, location, DK_WARNING);
      diagnostic.option_index = OPT_Wc90_c99_compat;
      report_diagnostic (&diagnostic);
    }
  va_end (ap);
}
