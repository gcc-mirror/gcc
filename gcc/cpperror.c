/* Default error handlers for CPP Library.
   Copyright (C) 1986, 87, 89, 92 - 95, 1998 Free Software Foundation, Inc.
   Written by Per Bothner, 1994.
   Based on CCCP program by Paul Rubin, June 1986
   Adapted to ANSI C, Richard Stallman, Jan 1987

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

 In other words, you are welcome to use, share and improve this program.
 You are forbidden to forbid anyone else to use, share and improve
 what you give them.   Help stamp out software-hoarding!  */

#ifndef EMACS
#include "config.h"
#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#include "system.h"
#include "gansidecl.h"
#else
#include <stdio.h>
#endif /* not EMACS */

#include "cpplib.h"

/* Print the file names and line numbers of the #include
   commands which led to the current file.  */

void
cpp_print_containing_files (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *ip;
  int first = 1;

  /* If stack of files hasn't changed since we last printed
     this info, don't repeat it.  */
  if (pfile->input_stack_listing_current)
    return;

  ip = cpp_file_buffer (pfile);

  /* Give up if we don't find a source file.  */
  if (ip == NULL)
    return;

  /* Find the other, outer source files.  */
  while ((ip = CPP_PREV_BUFFER (ip)), ip != CPP_NULL_BUFFER (pfile))
    {
      long line, col;
      cpp_buf_line_and_col (ip, &line, &col);
      if (ip->fname != NULL)
	{
	  if (first)
	    {
	      first = 0;
	      fprintf (stderr, "In file included");
	    }
	  else
	    fprintf (stderr, ",\n                ");
	}

      fprintf (stderr, " from %s:%ld", ip->nominal_fname, line);
    }
  if (! first)
    fprintf (stderr, ":\n");

  /* Record we have printed the status as of this time.  */
  pfile->input_stack_listing_current = 1;
}

void
cpp_file_line_for_message (pfile, filename, line, column)
     cpp_reader *pfile ATTRIBUTE_UNUSED;
     char *filename;
     int line, column;
{
  if (column > 0)
    fprintf (stderr, "%s:%d:%d: ", filename, line, column);
  else
    fprintf (stderr, "%s:%d: ", filename, line);
}

/* IS_ERROR is 2 for "fatal" error, 1 for error, 0 for warning */

void
v_cpp_message (pfile, is_error, msg, ap)
  cpp_reader * pfile;
  int is_error;
  const char *msg;
  va_list ap;
{
  if (!is_error)
    fprintf (stderr, "warning: ");
  else if (is_error == 2)
    pfile->errors = CPP_FATAL_LIMIT;
  else if (pfile->errors < CPP_FATAL_LIMIT)
    pfile->errors++;
  vfprintf (stderr, msg, ap);
  fprintf (stderr, "\n");
}

void
cpp_message VPROTO ((cpp_reader *pfile, int is_error, const char *msg, ...))
{
#ifndef __STDC__
  cpp_reader *pfile;
  int is_error;
  const char *msg;
#endif
  va_list ap;
  
  VA_START (ap, msg);
  
#ifndef __STDC__
  pfile = va_arg (ap, cpp_reader *);
  is_error = va_arg (ap, int);
  msg = va_arg (ap, const char *);
#endif

  v_cpp_message(pfile, is_error, msg, ap);
  va_end(ap);
}

/* Same as cpp_error, except we consider the error to be "fatal",
   such as inconsistent options.  I.e. there is little point in continuing.
   (We do not exit, to support use of cpplib as a library.
   Instead, it is the caller's responsibility to check
   CPP_FATAL_ERRORS.  */

void
cpp_fatal VPROTO ((cpp_reader *pfile, const char *str, ...))
{  
#ifndef __STDC__
  cpp_reader *pfile;
  const char *str;
#endif
  va_list ap;
  
  VA_START (ap, str);
  
#ifndef __STDC__
  pfile = va_arg (ap, cpp_reader *);
  str = va_arg (ap, const char *);
#endif

  fprintf (stderr, "%s: ", progname);
  v_cpp_message (pfile, 2, str, ap);
  va_end(ap);
}

void
cpp_pfatal_with_name (pfile, name)
     cpp_reader *pfile;
     const char *name;
{
  cpp_perror_with_name (pfile, name);
#ifdef VMS
  exit (vaxc$errno);
#else
  exit (FATAL_EXIT_CODE);
#endif
}
