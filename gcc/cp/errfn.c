/* Provide a call-back mechanism for handling error output.
   Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.
   Contributed by Jason Merrill (jason@cygnus.com)

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
#include "tree.h"
#include <stdio.h>
#include <ctype.h>

/* cp_printer is the type of a function which converts an argument into
   a string for digestion by printf.  The cp_printer function should deal
   with all memory management; the functions in this file will not free
   the char*s returned.  See error.c for an example use of this code.  */

typedef char* cp_printer PROTO((HOST_WIDE_INT, int));
extern cp_printer * cp_printers[256];

/* Whether or not we should try to be quiet for errors and warnings; this is
   used to avoid being too talkative about problems with tentative choices
   when we're computing the conversion costs for a method call.  */
int cp_silent = 0;

typedef void errorfn ();	/* deliberately vague */

extern char* cp_file_of PROTO((tree));
extern int   cp_line_of PROTO((tree));

#define STRDUP(f) (ap = (char *) alloca (strlen (f) +1), strcpy (ap, (f)), ap)

#define NARGS 5
#define arglist a1, a2, a3, a4, a5
#define arglist_dcl HOST_WIDE_INT a1, a2, a3, a4, a5;
#define ARGSINIT \
  args[0] = a1; args[1] = a2; args[2] = a3; args[3] = a4; args[4] = a5;
#define ARGSLIST args[0], args[1], args[2], args[3], args[4]

static void
cp_thing (errfn, atarg1, format, arglist)
     errorfn *errfn;
     int atarg1;
     char *format;
     arglist_dcl
{
  char *fmt;
  char *f;
  char *ap;
  int arg;
  HOST_WIDE_INT atarg = atarg1 ? a1 : 0;
  HOST_WIDE_INT args[NARGS];
  ARGSINIT

  fmt = STRDUP(format);
  
  for (f = fmt, arg = 0; *f; ++f)
    {
      cp_printer * function;
      int alternate;
      int maybe_here;
      
      /* ignore text */
      if (*f != '%') continue;

      ++f;

      alternate = 0;
      maybe_here = 0;

      /* ignore most flags */
      while (*f == ' ' || *f == '-' || *f == '+' || *f == '#')
	{
	  if (*f == '+')
	    maybe_here = 1;
	  else if (*f == '#')
	    alternate = 1;
	  ++f;
	}

      /* ignore field width */
      if (*f == '*')
	{
	  ++f;
	  ++arg;
	}
      else
	while (isdigit (*f))
	  ++f;

      /* ignore precision */
      if (*f == '.')
	{
	  ++f;
	  if (*f == '*')
	    {
	      ++f;
	      ++arg;
	    }
	  else
	    while (isdigit (*f))
	      ++f;
	}

      /* ignore "long" */
      if (*f == 'l')
	++f;

      function = cp_printers[(int)*f];

      if (function)
	{
	  char *p;

	  if (arg >= NARGS) abort ();
	  
	  if (maybe_here && atarg1)
	    atarg = args[arg];

	  /* Must use a temporary to avoid calling *function twice */
	  p = (*function) (args[arg], alternate);
	  args[arg] = (HOST_WIDE_INT) STRDUP(p);
	  *f = 's';
	}

      ++arg;			/* Assume valid format string */

    }

  if (atarg)
    {
      char *file = cp_file_of ((tree) atarg);
      int   line = cp_line_of ((tree) atarg);
      (*errfn) (file, line, fmt, ARGSLIST);
    }
  else
    (*errfn) (fmt, ARGSLIST);

}

void
cp_error (format, arglist)
     char *format;
     arglist_dcl
{
  extern errorfn error;
  if (! cp_silent)
    cp_thing (error, 0, format, arglist);
}

void
cp_warning (format, arglist)
     char *format;
     arglist_dcl
{
  extern errorfn warning;
  if (! cp_silent)
    cp_thing (warning, 0, format, arglist);
}

void
cp_pedwarn (format, arglist)
     char *format;
     arglist_dcl
{
  extern errorfn pedwarn;
  if (! cp_silent)
    cp_thing (pedwarn, 0, format, arglist);
}

void
cp_compiler_error (format, arglist)
     char *format;
     arglist_dcl
{
  extern errorfn compiler_error;
  if (! cp_silent)
    cp_thing (compiler_error, 0, format, arglist);
}

void
cp_sprintf (format, arglist)
     char *format;
     arglist_dcl
{
  cp_thing ((errorfn *) sprintf, 0, format, arglist);
}

void
cp_error_at (format, arglist)
     char *format;
     arglist_dcl
{
  extern errorfn error_with_file_and_line;
  if (! cp_silent)
    cp_thing (error_with_file_and_line, 1, format, arglist);
}

void
cp_warning_at (format, arglist)
     char *format;
     arglist_dcl
{
  extern errorfn warning_with_file_and_line;
  if (! cp_silent)
    cp_thing (warning_with_file_and_line, 1, format, arglist);
}

void
cp_pedwarn_at (format, arglist)
     char *format;
     arglist_dcl
{
  extern errorfn pedwarn_with_file_and_line;
  if (! cp_silent)
    cp_thing (pedwarn_with_file_and_line, 1, format, arglist);
}
