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
#include "system.h"
#include "tree.h"
#include "toplev.h"
#include "cp-tree.h"

/* cp_printer is the type of a function which converts an argument into
   a string for digestion by printf.  The cp_printer function should deal
   with all memory management; the functions in this file will not free
   the char*s returned.  See error.c for an example use of this code.  */

typedef char* cp_printer PROTO((tree, int));
extern cp_printer * cp_printers[256];

/* Whether or not we should try to be quiet for errors and warnings; this is
   used to avoid being too talkative about problems with tentative choices
   when we're computing the conversion costs for a method call.  */
int cp_silent = 0;

/* The error messages themselves.  */
typedef struct cp_err_msg {
  /* The format of the error message.  */
  char* format;

  /* The code which we should check when deciding whether or not to
     issue this message.  Used to indicate that some errors are "the
     same" even though they have different formats.  */
  error_code equiv_code;

  /* A count of how many more times this warning has been enabled than
     disabled.  (Ignored for errors.)  */
  int enabled;
} cp_err_msg;

static cp_err_msg err_msgs[] = {
#undef DEFERROR
#undef DEFERRORNUM
#define DEFERROR(code, format) DEFERRORNUM(code, format, code)
#define DEFERRORNUM(code, format, equiv_code) \
  { format, equiv_code, 1 },
#include "cp-error.def"
  { 0, 0, 0 }
};

typedef void errorfn ();	/* deliberately vague */

extern char* cp_file_of PROTO((tree));
extern int   cp_line_of PROTO((tree));
static int   is_warning_enabled PROTO((error_code));

#define STRDUP(f) (ap = (char *) alloca (strlen (f) +1), strcpy (ap, (f)), ap)

/* This function supports only `%s', `%d', `%%', and the C++ print
   codes.  */

#ifdef __STDC__
static void
cp_thing (errorfn *errfn, int atarg1, error_code ec, va_list ap)
#else
static void
cp_thing (errfn, atarg1, ec, ap)
     errorfn *errfn;
     int atarg1;
     error_code ec;
     va_list ap;
#endif
{
  static char *buf;
  static long buflen;
  int nargs = 0;
  long len;
  long offset;
  const char *f;
  tree atarg = 0;
  char* format;

  my_friendly_assert ((int) ec >= 0 && (int) ec < ec_last_error_code, 
		      0);
    
  format = err_msgs[(int) ec].format;

  my_friendly_assert (format != 0, 0);

  len = strlen (format) + 1 /* '\0' */ + 16 /* code number */;
  if (len > buflen)
    {
      buflen = len;
      buf = xrealloc (buf, buflen);
    }
  if (flag_diag_codes) 
    {
      sprintf (buf, "[%d] ", (int) ec);
      for (offset = 0; buf[offset]; ++offset)
	;
    }
  else
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
	  char *p;
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
	  /* A `%%' has occurred in the input string.  Since the
	     string we produce here will be passed to vprintf we must
	     preserve both `%' characters.  */

	  len += 2;
	  if (len > buflen)
	    {
	      buflen = len;
	      buf = xrealloc (buf, len);
	    }
	  strcpy (buf + offset, "%%");
	  offset += 2;
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
      char *file = cp_file_of (atarg);
      int   line = cp_line_of (atarg);
      (*errfn) (file, line, buf);
    }
  else
    (*errfn) (buf);

}

#ifdef __STDC__
#define DECLARE(name) void name (error_code ec, ...)
#define INIT va_start (ap, ec)
#else
#define DECLARE(name) void name (ec, va_alist) error_code ec; va_dcl
#define INIT va_start (ap)
#endif

DECLARE (cp_error)
{
  va_list ap;
  INIT;
  if (! cp_silent)
    cp_thing ((errorfn *) error, 0, ec, ap);
  va_end (ap);
}

DECLARE (cp_warning)
{
  va_list ap;
  INIT;
  if (! cp_silent && is_warning_enabled (ec))
    cp_thing ((errorfn *) warning, 0, ec, ap);
  va_end (ap);
}

DECLARE (cp_pedwarn)
{
  va_list ap;
  INIT;
  if (! cp_silent && is_warning_enabled (ec))
    cp_thing ((errorfn *) pedwarn, 0, ec, ap);
  va_end (ap);
}

DECLARE (cp_compiler_error)
{
  extern errorfn compiler_error;
  va_list ap;
  INIT;
  if (! cp_silent)
    cp_thing (compiler_error, 0, ec, ap);
  va_end (ap);
}

DECLARE (cp_sprintf)
{
  va_list ap;
  INIT;
  cp_thing ((errorfn *) sprintf, 0, ec, ap);
  va_end (ap);
}

DECLARE (cp_error_at)
{
  va_list ap;
  INIT;
  if (! cp_silent)
    cp_thing ((errorfn *) error_with_file_and_line, 1, ec, ap);
  va_end (ap);
}

DECLARE (cp_warning_at)
{
  va_list ap;
  INIT;
  if (! cp_silent && is_warning_enabled (ec))
    cp_thing ((errorfn *) warning_with_file_and_line, 1, ec, ap);
  va_end (ap);
}

DECLARE (cp_pedwarn_at)
{
  va_list ap;
  INIT;
  if (! cp_silent && is_warning_enabled (ec))
    cp_thing ((errorfn *) pedwarn_with_file_and_line, 1, ec, ap);
  va_end (ap);
}

/* If ON is non-zero, enable the warning with the indicated NUMBER.
   If OFF is zero, disable it.  Actually, this function manipulates a
   counter, so that enabling/disabling of warnings can nest
   appropriately.  */

void 
cp_enable_warning (number, on)
     int number;
     int on;
{
  if (number < 0 || number > (int) ec_last_error_code)
    error ("invalid warning number %d", number);
  else if (on)
    err_msgs[number].enabled++;
  else
    {
      if (!err_msgs[number].enabled)
	warning ("warning %d not enabled", number);
      else
	err_msgs[number].enabled--;
    }
}

/* Returns non-zero if EC corresponds to an enabled error message.  */

int
is_warning_enabled (ec)
     error_code ec;
{
  my_friendly_assert ((int) ec >= 0 && (int) ec < ec_last_error_code, 
		      0);
    
  return err_msgs[(int) ec].enabled;
}
