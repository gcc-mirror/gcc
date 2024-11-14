/* Various diagnostic subroutines for the GNU C language.
   Copyright (C) 2000-2024 Free Software Foundation, Inc.
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

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "c-tree.h"
#include "opts.h"

/* Issue an ISO C23 pedantic warning MSGID if -pedantic outside C2Y mode,
   otherwise issue warning MSGID if -Wc23-c2y-compat is specified.
   This function is supposed to be used for matters that are allowed in
   ISO C2Y but not supported in ISO C23, thus we explicitly don't pedwarn
   when C2Y is specified.  */

bool
pedwarn_c23 (location_t location,
	     diagnostic_option_id option_id,
	     const char *gmsgid, ...)
{
  diagnostic_info diagnostic;
  va_list ap;
  bool warned = false;
  rich_location richloc (line_table, location);

  va_start (ap, gmsgid);
  /* If desired, issue the C23/C2Y compat warning, which is more specific
     than -pedantic.  */
  if (warn_c23_c2y_compat > 0)
    {
      diagnostic_set_info (&diagnostic, gmsgid, &ap, &richloc,
			   (pedantic && !flag_isoc2y)
			   ? DK_PEDWARN : DK_WARNING);
      diagnostic.option_id = OPT_Wc23_c2y_compat;
      warned = diagnostic_report_diagnostic (global_dc, &diagnostic);
    }
  /* -Wno-c23-c2y-compat suppresses even the pedwarns.  */
  else if (warn_c23_c2y_compat == 0)
    ;
  /* For -pedantic outside C2Y, issue a pedwarn.  */
  else if (pedantic && !flag_isoc2y)
    {
      diagnostic_set_info (&diagnostic, gmsgid, &ap, &richloc, DK_PEDWARN);
      diagnostic.option_id = option_id;
      warned = diagnostic_report_diagnostic (global_dc, &diagnostic);
    }
  va_end (ap);
  return warned;
}

/* Issue an ISO C11 pedantic warning MSGID if -pedantic outside C23 mode,
   otherwise issue warning MSGID if -Wc11-c23-compat is specified.
   This function is supposed to be used for matters that are allowed in
   ISO C23 but not supported in ISO C11, thus we explicitly don't pedwarn
   when C23 is specified.

   Additionally, warn if OPTION_ID is not OPT_Wpedantic nor
   OPT_Wc11_c23_compat.  */

bool
pedwarn_c11 (location_t location,
	     diagnostic_option_id option_id,
	     const char *gmsgid, ...)
{
  diagnostic_info diagnostic;
  va_list ap;
  bool warned = false;
  rich_location richloc (line_table, location);

  va_start (ap, gmsgid);
  /* If desired, issue the C11/C23 compat warning, which is more specific than
     -pedantic, or the warning specified by option_id.  */
  if (warn_c11_c23_compat > 0 || (option_id.m_idx != OPT_Wpedantic
				  && option_id.m_idx != OPT_Wc11_c23_compat))
    {
      diagnostic_set_info (&diagnostic, gmsgid, &ap, &richloc,
			   (pedantic && !flag_isoc23)
			   ? DK_PEDWARN : DK_WARNING);
      if (option_id == OPT_Wpedantic)
	diagnostic.option_id = OPT_Wc11_c23_compat;
      else
	diagnostic.option_id = option_id;
      warned = diagnostic_report_diagnostic (global_dc, &diagnostic);
    }
  /* -Wno-c11-c23-compat suppresses even the pedwarns.  */
  else if (warn_c11_c23_compat == 0)
    ;
  /* For -pedantic outside C23, issue a pedwarn.  */
  else if (pedantic && !flag_isoc23)
    {
      diagnostic_set_info (&diagnostic, gmsgid, &ap, &richloc, DK_PEDWARN);
      diagnostic.option_id = option_id;
      warned = diagnostic_report_diagnostic (global_dc, &diagnostic);
    }
  va_end (ap);
  return warned;
}

/* Issue an ISO C99 pedantic warning MSGID if -pedantic outside C11 mode,
   otherwise issue warning MSGID if -Wc99-c11-compat is specified.
   This function is supposed to be used for matters that are allowed in
   ISO C11 but not supported in ISO C99, thus we explicitly don't pedwarn
   when C11 is specified.  */

bool
pedwarn_c99 (location_t location,
	     diagnostic_option_id option_id,
	     const char *gmsgid, ...)
{
  diagnostic_info diagnostic;
  va_list ap;
  bool warned = false;
  rich_location richloc (line_table, location);

  va_start (ap, gmsgid);
  /* If desired, issue the C99/C11 compat warning, which is more specific
     than -pedantic.  */
  if (warn_c99_c11_compat > 0)
    {
      diagnostic_set_info (&diagnostic, gmsgid, &ap, &richloc,
			   (pedantic && !flag_isoc11)
			   ? DK_PEDWARN : DK_WARNING);
      diagnostic.option_id = OPT_Wc99_c11_compat;
      warned = diagnostic_report_diagnostic (global_dc, &diagnostic);
    }
  /* -Wno-c99-c11-compat suppresses even the pedwarns.  */
  else if (warn_c99_c11_compat == 0)
    ;
  /* For -pedantic outside C11, issue a pedwarn.  */
  else if (pedantic && !flag_isoc11)
    {
      diagnostic_set_info (&diagnostic, gmsgid, &ap, &richloc, DK_PEDWARN);
      diagnostic.option_id = option_id;
      warned = diagnostic_report_diagnostic (global_dc, &diagnostic);
    }
  va_end (ap);
  return warned;
}

/* Issue an ISO C90 pedantic warning MSGID if -pedantic outside C99 mode,
   otherwise issue warning MSGID if -Wc90-c99-compat is specified, or if
   a specific option such as -Wlong-long is specified.
   This function is supposed to be used for matters that are allowed in
   ISO C99 but not supported in ISO C90, thus we explicitly don't pedwarn
   when C99 is specified.  (There is no flag_c90.)  */

bool
pedwarn_c90 (location_t location,
	     diagnostic_option_id option_id,
	     const char *gmsgid, ...)
{
  diagnostic_info diagnostic;
  va_list ap;
  bool warned = false;
  rich_location richloc (line_table, location);

  va_start (ap, gmsgid);
  /* Warnings such as -Wvla are the most specific ones.  */
  if (option_id.m_idx != OPT_Wpedantic)
    {
      int opt_var = *(int *) option_flag_var (option_id.m_idx, &global_options);
      if (opt_var == 0)
        goto out;
      else if (opt_var > 0)
	{
	  diagnostic_set_info (&diagnostic, gmsgid, &ap, &richloc,
			       (pedantic && !flag_isoc99)
			       ? DK_PEDWARN : DK_WARNING);
	  diagnostic.option_id = option_id;
	  diagnostic_report_diagnostic (global_dc, &diagnostic);
	  warned = true;
	  goto out;
	}
    }
  /* Maybe we want to issue the C90/C99 compat warning, which is more
     specific than -pedantic.  */
  if (warn_c90_c99_compat > 0)
    {
      diagnostic_set_info (&diagnostic, gmsgid, &ap, &richloc,
			   (pedantic && !flag_isoc99)
			   ? DK_PEDWARN : DK_WARNING);
      diagnostic.option_id = OPT_Wc90_c99_compat;
      diagnostic_report_diagnostic (global_dc, &diagnostic);
    }
  /* -Wno-c90-c99-compat suppresses the pedwarns.  */
  else if (warn_c90_c99_compat == 0)
    ;
  /* For -pedantic outside C99, issue a pedwarn.  */
  else if (pedantic && !flag_isoc99)
    {
      diagnostic_set_info (&diagnostic, gmsgid, &ap, &richloc, DK_PEDWARN);
      diagnostic.option_id = option_id;
      diagnostic_report_diagnostic (global_dc, &diagnostic);
      warned = true;
    }
out:
  va_end (ap);
  return warned;
}
