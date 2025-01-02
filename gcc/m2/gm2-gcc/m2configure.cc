/* m2configure.cc provides an interface to some configuration values.

Copyright (C) 2022-2025 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "libiberty.h"

#include "gcc-consolidation.h"

#include "../gm2-lang.h"
#include "../m2-tree.h"
#include "m2convert.h"

/* Prototypes.  */

#define m2configure_c

#include "m2assert.h"
#include "m2builtins.h"
#include "m2convert.h"
#include "m2decl.h"
#include "m2expr.h"
#include "m2options.h"
#include "m2configure.h"

#include "m2/gm2version.h"
#include "m2/gm2config.h"

#define CPPPROGRAM  "cc1"


/* gen_gm2_libexec returns a string containing libexec /
   DEFAULT_TARGET_MACHINE string / DEFAULT_TARGET_MACHINE.  */

static char *
gen_gm2_libexec (const char *libexec)
{
  int l = strlen (libexec) + 1 + strlen (DEFAULT_TARGET_MACHINE) + 1
          + strlen (DEFAULT_TARGET_VERSION) + 1;
  char *s = (char *)xmalloc (l);
  char dir_sep[2];

  dir_sep[0] = DIR_SEPARATOR;
  dir_sep[1] = (char)0;

  strcpy (s, libexec);
  strcat (s, dir_sep);
  strcat (s, DEFAULT_TARGET_MACHINE);
  strcat (s, dir_sep);
  strcat (s, DEFAULT_TARGET_VERSION);
  return s;
}

/* FullPathCPP returns the fullpath and program name to cpp.  */

char *
m2configure_FullPathCPP (void)
{
  if (M2Options_GetCpp ())
    {
      char *path = (char *) M2Options_GetB ();

      if (path == NULL)
	path = gen_gm2_libexec (STANDARD_LIBEXEC_PREFIX);

      if (strcmp (path, "") == 0)
	return xstrdup (CPPPROGRAM);

      char *full = (char *)xmalloc (strlen (path) + 1 + strlen (CPPPROGRAM) + 1);
      strcpy (full, path);
      char *sep = (char *)alloca (2);
      sep[0] = DIR_SEPARATOR;
      sep[1] = (char)0;
      strcat (full, sep);
      strcat (full, CPPPROGRAM);
      return full;
    }
  return NULL;
}

/* Return the value of TARGET_IEEEQUAD_DEFAULT.  If it is undefined
   -1 is returned.  A value of 0 indicates the default target long
   double uses the IBM 128 representation.  A value of 1 indicates
   the default target long double (LONGREAL) is __float128.  */

int
m2configure_TargetIEEEQuadDefault (void)
{
#ifdef TARGET_IEEEQUAD_DEFAULT
  return TARGET_IEEEQUAD_DEFAULT;
#else
  return -1;
#endif
}
