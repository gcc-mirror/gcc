/* Basic error reporting routines.
   Copyright (C) 1999, 2000, 2001 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* warning, error, and fatal.  These definitions are suitable for use
   in the generator programs; eventually we would like to use them in
   cc1 too, but that's a longer term project.  */

#include "config.h"
#include "system.h"
#include "errors.h"

/* Set this to argv[0] at the beginning of main.  */

const char *progname;

/* Starts out 0, set to 1 if error is called.  */

int have_error = 0;

/* Print a warning message - output produced, but there may be problems.  */

void
warning VPARAMS ((const char *format, ...))
{
  VA_OPEN (ap, format);
  VA_FIXEDARG (ap, const char *, format);

  fprintf (stderr, "%s: warning: ", progname);
  vfprintf (stderr, format, ap);
  VA_CLOSE (ap);
  fputc('\n', stderr);
}


/* Print an error message - we keep going but the output is unusable.  */

void
error VPARAMS ((const char *format, ...))
{
  VA_OPEN (ap, format);
  VA_FIXEDARG (ap, const char *, format);

  fprintf (stderr, "%s: ", progname);
  vfprintf (stderr, format, ap);
  VA_CLOSE (ap);
  fputc('\n', stderr);

  have_error = 1;
}


/* Fatal error - terminate execution immediately.  Does not return.  */

void
fatal VPARAMS ((const char *format, ...))
{
  VA_OPEN (ap, format);
  VA_FIXEDARG (ap, const char *, format);

  fprintf (stderr, "%s: ", progname);
  vfprintf (stderr, format, ap);
  VA_CLOSE (ap);
  fputc('\n', stderr);
  exit (FATAL_EXIT_CODE);
}

/* Similar, but say we got an internal error.  */

void
internal_error VPARAMS ((const char *format, ...))
{
  VA_OPEN (ap, format);
  VA_FIXEDARG (ap, const char *, format);

  fprintf (stderr, "%s: Internal error: ", progname);
  vfprintf (stderr, format, ap);
  VA_CLOSE (ap);
  fputc ('\n', stderr);
  exit (FATAL_EXIT_CODE);
}

/* Given a partial pathname as input, return another pathname that
   shares no directory elements with the pathname of __FILE__.  This
   is used by fancy_abort() to print `Internal compiler error in expr.c'
   instead of `Internal compiler error in ../../GCC/gcc/expr.c'.  This
   version if for the gen* programs and so needn't handle subdirectories.  */

const char *
trim_filename (name)
     const char *name;
{
  static const char this_file[] = __FILE__;
  const char *p = name, *q = this_file;

  /* Skip any parts the two filenames have in common.  */
  while (*p == *q && *p != 0 && *q != 0)
    p++, q++;

  /* Now go backwards until the previous directory separator.  */
  while (p > name && p[-1] != DIR_SEPARATOR
#ifdef DIR_SEPARATOR_2
	 && p[-1] != DIR_SEPARATOR_2
#endif
	 )
    p--;

  return p;
}

/* "Fancy" abort.  Reports where in the compiler someone gave up.
   This file is used only by build programs, so we're not as polite as
   the version in diagnostic.c.  */
void
fancy_abort (file, line, func)
     const char *file;
     int line;
     const char *func;
{
  internal_error ("abort in %s, at %s:%d", func, file, line);
}
