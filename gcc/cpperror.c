/* Default error handlers for CPP Library.
   Copyright (C) 1986, 87, 89, 92, 93, 94, 1995 Free Software Foundation, Inc.
   Written by Per Bothner, 1994.
   Based on CCCP program by by Paul Rubin, June 1986
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
#endif /* not EMACS */

#include "cpplib.h"
#include <stdio.h>

/* Print the file names and line numbers of the #include
   commands which led to the current file.  */

void
cpp_print_containing_files (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *ip;
  int i;
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

      fprintf (stderr, " from %s:%d", ip->nominal_fname, line);
    }
  if (! first)
    fprintf (stderr, ":\n");

  /* Record we have printed the status as of this time.  */
  pfile->input_stack_listing_current = 1;
}

void
cpp_file_line_for_message (pfile, filename, line, column)
     cpp_reader *pfile;
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
cpp_message (pfile, is_error, msg, arg1, arg2, arg3)
     int is_error;
     cpp_reader *pfile;
     char *msg;
     char *arg1, *arg2, *arg3;
{
  if (!is_error)
    fprintf (stderr, "warning: ");
  else if (is_error == 2)
    pfile->errors = CPP_FATAL_LIMIT;
  else if (pfile->errors < CPP_FATAL_LIMIT)
    pfile->errors++;
  fprintf (stderr, msg, arg1, arg2, arg3);
  fprintf (stderr, "\n");
}

/* Same as cpp_error, except we consider the error to be "fatal",
   such as inconsistent options.  I.e. there is little point in continuing.
   (We do not exit, to support use of cpplib as a library.
   Instead, it is the caller's responsibility to check
   CPP_FATAL_ERRORS.  */

void
cpp_fatal (pfile, str, arg)
     cpp_reader *pfile;
     char *str, *arg;
{
  fprintf (stderr, "%s: ", progname);
  cpp_message (pfile, 2, str, arg);
}

void
cpp_pfatal_with_name (pfile, name)
     cpp_reader *pfile;
     char *name;
{
  cpp_perror_with_name (pfile, name);
#ifdef VMS
  exit (vaxc$errno);
#else
  exit (FATAL_EXIT_CODE);
#endif
}
