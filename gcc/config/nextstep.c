/* Functions for generic NeXT as target machine for GNU C compiler.
   Copyright (C) 1989, 90-93, 96, 1997 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include <stdio.h>
#include "flags.h"
#include "tree.h"

/* Make everything that used to go in the text section really go there.  */

int flag_no_mach_text_sections = 0;

#define OPT_STRCMP(opt) (!strncmp (opt, p, sizeof (opt)-1))

/* 1 if handle_pragma has been called yet.  */

static int pragma_initialized;

/* Initial setting of `optimize'.  */

static int initial_optimize_flag;

extern char *get_directive_line ();

/* Called from check_newline via the macro HANDLE_PRAGMA.
   FINPUT is the source file input stream.
   CH is the first character after `#pragma'.
   The result is 1 if the pragma was handled.  */

int
handle_pragma (p_getc, p_ungetc, pname)
     int (*  p_getc) PROTO ((void));
     void (* p_ungetc) PROTO ((int));
     char * pname;
{
  int retval = 0;

  /* Record initial setting of optimize flag, so we can restore it.  */
  if (!pragma_initialized)
    {
      pragma_initialized = 1;
      initial_optimize_flag = optimize;
    }

  if (strcmp (pname, "CC_OPT_ON") == 0)
    {
      optimize = 1;
      warning ("optimization turned on");
      retval = 1;
    }
  else if (strcmp (pname, "CC_OPT_OFF") == 0)
    {
      optimize = 0;
      warning ("optimization turned off");
      retval = 1;
    }
  else if (strcmp (pname, "CC_OPT_RESTORE") == 0)
    {
      extern int initial_optimize_flag;

      if (optimize != initial_optimize_flag)
	optimize = initial_optimize_flag;
      warning ("optimization level restored");
      retval = 1;
    }
  else if (strcmp (pname, "CC_WRITABLE_STRINGS") == 0)
    flag_writable_strings = retval = 1;
  else if (strcmp (pname, "CC_NON_WRITABLE_STRINGS") == 0)
    flag_writable_strings = 0, retval = 1;
  else if (strcmp (pname, "CC_NO_MACH_TEXT_SECTIONS") == 0)
    flag_no_mach_text_sections = retval = 1;

  return retval;
}
