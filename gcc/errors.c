/* Basic error reporting routines.
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.

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
#ifndef ANSI_PROTOTYPES
  const char *format;
#endif
  va_list ap;

  VA_START (ap, format);

#ifndef ANSI_PROTOTYPES
  format = va_arg (ap, const char *);
#endif

  fprintf (stderr, "%s: warning: ", progname);
  vfprintf (stderr, format, ap);
  va_end (ap);
  fputc('\n', stderr);
}


/* Print an error message - we keep going but the output is unusable.  */

void
error VPARAMS ((const char *format, ...))
{
#ifndef ANSI_PROTOTYPES
  const char *format;
#endif
  va_list ap;

  VA_START (ap, format);

#ifndef ANSI_PROTOTYPES
  format = va_arg (ap, const char *);
#endif

  fprintf (stderr, "%s: ", progname);
  vfprintf (stderr, format, ap);
  va_end (ap);
  fputc('\n', stderr);

  have_error = 1;
}


/* Fatal error - terminate execution immediately.  Does not return.  */

void
fatal VPARAMS ((const char *format, ...))
{
#ifndef ANSI_PROTOTYPES
  const char *format;
#endif
  va_list ap;

  VA_START (ap, format);

#ifndef ANSI_PROTOTYPES
  format = va_arg (ap, const char *);
#endif

  fprintf (stderr, "%s: ", progname);
  vfprintf (stderr, format, ap);
  va_end (ap);
  fputc('\n', stderr);
  exit (FATAL_EXIT_CODE);
}
