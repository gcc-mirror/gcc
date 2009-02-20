/* Helper routines for cygwin-specific command-line parsing.
   Contributed by Christopher Faylor (cgf@redhat.com)
   Copyright 2003, 2005, 2007, 2008 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include <string.h>

void
mingw_scan (int argc ATTRIBUTE_UNUSED,
            const char *const *argv,
            const char **spec_machine)
{
  putenv (xstrdup ("GCC_CYGWIN_MINGW=0"));
 
  while (*++argv)
    if (strcmp (*argv, "-mno-win32") == 0)
      putenv (xstrdup ("GCC_CYGWIN_WIN32=0"));
    else if (strcmp (*argv, "-mwin32") == 0)
      putenv (xstrdup ("GCC_CYGWIN_WIN32=1"));
    else if (strcmp (*argv, "-mno-cygwin") == 0)
      {
	char *p = strstr (*spec_machine, "-cygwin");
	if (p)
	  {
	    int len = p - *spec_machine;
	    char *s = XNEWVEC (char, strlen (*spec_machine) + 3);
	    memcpy (s, *spec_machine, len);
	    strcpy (s + len, "-mingw32");
	    *spec_machine = s;
	  }
	putenv (xstrdup ("GCC_CYGWIN_MINGW=1"));
      }
  return;
}
