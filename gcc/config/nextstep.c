/* Functions for generic NeXT as target machine for GNU C compiler.
   Copyright (C) 1989, 1990, 1991, 1992, 1993 Free Software Foundation, Inc.

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

/* Make everything that used to go in the text section really go there.  */

int flag_no_mach_text_sections = 0;

#define OPT_STRCMP(opt) (!strncmp (opt, p, sizeof (opt)-1))

/* 1 if handle_pragma has been called yet.  */

static int pragma_initialized;

/* Initial setting of `optimize'.  */

static int initial_optimize_flag;

extern char *get_directive_line ();

/* Called from check_newline via the macro HANDLE_PRAGMA.
   FINPUT is the source file input stream.  */

void
handle_pragma (finput, get_line_function)
     FILE *finput;
     char *(*get_line_function) ();
{
  register char *p = (*get_line_function) (finput);

  /* Record initial setting of optimize flag, so we can restore it.  */
  if (!pragma_initialized)
    {
      pragma_initialized = 1;
      initial_optimize_flag = optimize;
    }

  if (OPT_STRCMP ("CC_OPT_ON"))
    {
      optimize = 1, obey_regdecls = 0;
      warning ("optimization turned on");
    }
  else if (OPT_STRCMP ("CC_OPT_OFF"))
    {
      optimize = 0, obey_regdecls = 1;
      warning ("optimization turned off");
    }
  else if (OPT_STRCMP ("CC_OPT_RESTORE"))
    {
      extern int initial_optimize_flag;

      if (optimize != initial_optimize_flag)
	{
	  if (initial_optimize_flag)
	    obey_regdecls = 0;
	  else
	    obey_regdecls = 1;
	  optimize = initial_optimize_flag;
	}
      warning ("optimization level restored");
    }
  else if (OPT_STRCMP ("CC_WRITABLE_STRINGS"))
    flag_writable_strings = 1;
  else if (OPT_STRCMP ("CC_NON_WRITABLE_STRINGS"))
    flag_writable_strings = 0;
  else if (OPT_STRCMP ("CC_NO_MACH_TEXT_SECTIONS"))
    flag_no_mach_text_sections = 1;
}
