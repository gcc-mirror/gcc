/* Handle options that are passed from environment variables.

   Copyright (C) 2004, 2009, 2012 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "fixlib.h"

te_verbose  verbose_level = VERB_PROGRESS;

fixinc_mode_t fixinc_mode = TESTING_OFF;

#define _ENV_(v,m,n,t) char const * v = NULL;
ENV_TABLE
#undef _ENV_

static void
show_not_def (char const * vname)
{
  static const char var_not_found[] =
    "fixincl ERROR:  %s environment variable not defined\n"
    "each of these must be defined:\n";
  static char const not_found_var[] = "\t%s\n";

  fprintf (stderr, var_not_found, vname);
# define _ENV_(vv,mm,nn,tt) \
  if (mm) fprintf (stderr, not_found_var, nn);
  ENV_TABLE
# undef _ENV_

  exit (EXIT_FAILURE);
}

void
initialize_opts (void)
{
#define _ENV_(v,m,n,t)   {                      \
    static char const var[] = n;                \
    v = getenv (var);                           \
    if (m && (v == NULL)) show_not_def (var);   \
  }

  ENV_TABLE;
#undef _ENV_

  if ((pz_test_mode != NULL) && (strcmp (pz_test_mode, "true") == 0))
    fixinc_mode = TESTING_ON;
}
