/* Provide a call-back mechanism for handling error output.
   Copyright (C) 1993, 94, 95, 96, 97, 98, 99, 2000
   Free Software Foundation, Inc.
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
#include "system.h"
#include "tree.h"
#include "cp-tree.h"
#include "toplev.h"

/* cp_printer is the type of a function which converts an argument into
   a string for digestion by printf.  The cp_printer function should deal
   with all memory management; the functions in this file will not free
   the char*s returned.  See error.c for an example use of this code.  */

typedef const char *cp_printer PARAMS ((tree, int));
extern cp_printer * cp_printers[256];

/* Whether or not we should try to be quiet for errors and warnings; this is
   used to avoid being too talkative about problems with tentative choices
   when we're computing the conversion costs for a method call.  */
int cp_silent = 0;

typedef void errorfn ();	/* deliberately vague */

static void cp_thing PARAMS ((errorfn *, int, const char *, va_list));

#define STRDUP(f) (ap = (char *) alloca (strlen (f) +1), strcpy (ap, (f)), ap)

/* This function supports only `%s', `%d', `%%', and the C++ print
   codes.  */

static void
cp_thing (errfn, atarg1, format, ap)
     errorfn *errfn;
     int atarg1;
     const char *format;
     va_list ap;
{
  static char *buf;
  static long buflen;
  int nargs = 0;
  long len;
  long offset;
  const char *f;
  tree atarg = 0;

  len = strlen (format) + 1;
  if (len > buflen)
    {
      buflen = len;
      buf = xrealloc (buf, buflen);
    }
  offset = 0;

  for (f = format; *f; ++f)
    {
      cp_printer * function;
      int alternate;
      int maybe_here;
      
      /* ignore text */
      if (*f != '%')
	{
	  buf[offset++] = *f;
	  continue;
	}

      ++f;

      alternate = 0;
      maybe_here = 0;

      /* Check for '+' and '#' (in that order). */
      if (*f == '+')
	{
	  maybe_here = 1;
	  ++f;
	}
      if (*f == '#')
	{
	  alternate = 1;
	  ++f;
	}

      /* no field width or precision */

      function = cp_printers[(int)*f];

      if (function || *f == 's')
	{
	  const char *p;
	  int plen;

	  if (*f == 's')
	    {
	      p = va_arg (ap, char *);
	      nargs++;
	    }
	  else
	    {
	      tree t = va_arg (ap, tree);
	      nargs++;

	      /* This indicates that ATARG comes from a different
		 location than normal.  */
	      if (maybe_here && atarg1)
		atarg = t;

	      /* If atarg1 is set and this is the first argument, then
		 set ATARG appropriately.  */
	      if (atarg1 && nargs == 1)
		atarg = t;

	      p = (*function) (t, alternate);
	    }

	  plen = strlen (p);
	  len += plen;
	  if (len > buflen)
	    {
	      buflen = len;
	      buf = xrealloc (buf, len);
	    }
	  strcpy (buf + offset, p);
	  offset += plen;
	}
      else if (*f == '%')
	{
	  /* A `%%' has occurred in the input string. Replace it with
	     a `%' in the formatted message buf. */

	  if (++len > buflen)
	    {
	      buflen = len;
	      buf = xrealloc (buf, len);
	    }
	  buf[offset++] = '%';
	}
      else
	{
	  if (*f != 'd')
	    abort ();
	  len += HOST_BITS_PER_INT / 2;
	  if (len > buflen)
	    {
	      buflen = len;
	      buf = xrealloc (buf, len);
	    }
	  sprintf (buf + offset, "%d", va_arg (ap, int));
	  nargs++;
	  offset += strlen (buf + offset);
	  /* With an ANSI C library one could write
	     out += sprintf (...); */
	}
    }
  buf[offset] = '\0';

  /* If ATARG1 is set, but we haven't extracted any arguments, then
     extract one tree argument for ATARG.  */  
  if (nargs == 0 && atarg1)
    atarg = va_arg (ap, tree);

  if (atarg)
    {
      const char *file = cp_file_of (atarg);
      int   line = cp_line_of (atarg);
      (*errfn) (file, line, "%s", buf);
    }
  else
    (*errfn) ("%s", buf);

}

void
cp_error VPARAMS ((const char *format, ...))
{
#ifndef ANSI_PROTOTYPES
  char *format;
#endif
  va_list ap;

  VA_START (ap, format);

#ifndef ANSI_PROTOTYPES
  format = va_arg (ap, char *);
#endif

  if (! cp_silent)
    cp_thing ((errorfn *) error, 0, format, ap);
  va_end (ap);
}

void
cp_warning VPARAMS ((const char *format, ...))
{
#ifndef ANSI_PROTOTYPES
  char *format;
#endif
  va_list ap;

  VA_START (ap, format);

#ifndef ANSI_PROTOTYPES
  format = va_arg (ap, char *);
#endif

  if (! cp_silent)
    cp_thing ((errorfn *) warning, 0, format, ap);
  va_end (ap);
}

void
cp_pedwarn VPARAMS ((const char *format, ...))
{
#ifndef ANSI_PROTOTYPES
  char *format;
#endif
  va_list ap;

  VA_START (ap, format);

#ifndef ANSI_PROTOTYPES
  format = va_arg (ap, char *);
#endif

  if (! cp_silent)
    cp_thing ((errorfn *) pedwarn, 0, format, ap);
  va_end (ap);
}

void
cp_compiler_error VPARAMS ((const char *format, ...))
{
#ifndef ANSI_PROTOTYPES
  char *format;
#endif
  va_list ap;

  VA_START (ap, format);

#ifndef ANSI_PROTOTYPES
  format = va_arg (ap, char *);
#endif

  if (! cp_silent)
    cp_thing ((errorfn *) compiler_error, 0, format, ap);
  va_end (ap);
}

void
cp_deprecated (msg)
  const char *msg;
{
  extern int warn_deprecated;
  if (!warn_deprecated)
    return;
  cp_warning ("%s is deprecated.", msg);
  cp_warning ("Please see the documentation for details.");
}

void
cp_sprintf VPARAMS ((const char *format, ...))
{
#ifndef ANSI_PROTOTYPES
  char *format;
#endif
  va_list ap;

  VA_START (ap, format);

#ifndef ANSI_PROTOTYPES
  format = va_arg (ap, char *);
#endif

  cp_thing ((errorfn *) sprintf, 0, format, ap);
  va_end (ap);
}

void
cp_error_at VPARAMS ((const char *format, ...))
{
#ifndef ANSI_PROTOTYPES
  char *format;
#endif
  va_list ap;

  VA_START (ap, format);

#ifndef ANSI_PROTOTYPES
  format = va_arg (ap, char *);
#endif

  if (! cp_silent)
    cp_thing ((errorfn *) error_with_file_and_line, 1, format, ap);
  va_end (ap);
}

void
cp_warning_at VPARAMS ((const char *format, ...))
{
#ifndef ANSI_PROTOTYPES
  char *format;
#endif
  va_list ap;

  VA_START (ap, format);

#ifndef ANSI_PROTOTYPES
  format = va_arg (ap, char *);
#endif

  if (! cp_silent)
    cp_thing ((errorfn *) warning_with_file_and_line, 1, format, ap);
  va_end (ap);
}

void
cp_pedwarn_at VPARAMS ((const char *format, ...))
{
#ifndef ANSI_PROTOTYPES
  char *format;
#endif
  va_list ap;

  VA_START (ap, format);

#ifndef ANSI_PROTOTYPES
  format = va_arg (ap, char *);
#endif

  if (! cp_silent)
    cp_thing ((errorfn *) pedwarn_with_file_and_line, 1, format, ap);
  va_end (ap);
}
