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
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

 In other words, you are welcome to use, share and improve this program.
 You are forbidden to forbid anyone else to use, share and improve
 what you give them.   Help stamp out software-hoarding!  */

#include "cpplib.h"
#include <stdio.h>

/* This defines "errno" properly for VMS, and gives us EACCES. */
#include <errno.h>
#ifndef errno
extern int errno;
#endif

#ifndef VMS
#ifndef HAVE_STRERROR
extern int sys_nerr;
#if defined(bsd4_4)
extern const char *const sys_errlist[];
#else
extern char *sys_errlist[];
#endif
#else	/* HAVE_STERRROR */
char *strerror ();
#endif
#else	/* VMS */
char *strerror (int,...);
#endif

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

/* start-sanitize-mpw */
#ifdef MPW
      fprintf (stderr, " File \"%s\"; Line %d  # ", ip->nominal_fname, line);
#else
/* end-sanitize-mpw */
      fprintf (stderr, " from %s:%d", ip->nominal_fname, line);
/* start-sanitize-mpw */
#endif /* MPW */
/* end-sanitize-mpw */
    }
  if (! first)
    fprintf (stderr, ":\n");

  /* Record we have printed the status as of this time.  */
  pfile->input_stack_listing_current = 1;
}

void
cpp_print_file_and_line (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *ip = cpp_file_buffer (pfile);

  if (ip != NULL)
    {
      long line, col;
      cpp_buf_line_and_col (ip, &line, &col);
      if (pfile->show_column)
	fprintf (stderr, "%s:%d:%d: ", ip->nominal_fname, line, col);
      else
	fprintf (stderr, "%s:%d: ", ip->nominal_fname, line);
    }
}

void
cpp_error (pfile, msg, arg1, arg2, arg3)
     cpp_reader *pfile;
     char *msg;
     char *arg1, *arg2, *arg3;
{
  cpp_print_containing_files (pfile);
  cpp_print_file_and_line (pfile);
  fprintf (stderr, msg, arg1, arg2, arg3);
  fprintf (stderr, "\n");
  pfile->errors++;
}

/* Print error message but don't count it.  */

void
cpp_warning (pfile, msg, arg1, arg2, arg3)
     cpp_reader *pfile;
     char *msg;
     char *arg1, *arg2, *arg3;
{
  if (CPP_OPTIONS (pfile)->inhibit_warnings)
    return;

  if (CPP_OPTIONS (pfile)->warnings_are_errors)
    pfile->errors++;

  cpp_print_containing_files (pfile);
  cpp_print_file_and_line (pfile);
  fprintf (stderr, "warning: ");
  fprintf (stderr, msg, arg1, arg2, arg3);
  fprintf (stderr, "\n");
}

void
cpp_error_with_line (pfile, line, msg, arg1, arg2, arg3)
     cpp_reader *pfile;
     int line;
     char *msg;
     char *arg1, *arg2, *arg3;
{
  int i;
  cpp_buffer *ip = cpp_file_buffer (pfile);

  cpp_print_containing_files (pfile);

  if (ip != NULL)
    fprintf (stderr, "%s:%d: ", ip->nominal_fname, line);

  fprintf (stderr, msg, arg1, arg2, arg3);
  fprintf (stderr, "\n");
  pfile->errors++;
}

void
cpp_warning_with_line (pfile, line, msg, arg1, arg2, arg3)
     cpp_reader *pfile;
     int line;
     char *msg;
     char *arg1, *arg2, *arg3;
{
  int i;
  cpp_buffer *ip;

  if (CPP_OPTIONS (pfile)->inhibit_warnings)
    return;

  if (CPP_OPTIONS (pfile)->warnings_are_errors)
    pfile->errors++;

  cpp_print_containing_files (pfile);

  ip = cpp_file_buffer (pfile);

  if (ip != NULL)
    fprintf (stderr, "%s:%d: ", ip->nominal_fname, line);
  fprintf (stderr, "warning: ");
  fprintf (stderr, msg, arg1, arg2, arg3);
  fprintf (stderr, "\n");
}

/* Print an error message and maybe count it.  */

void
cpp_pedwarn (pfile, msg, arg1, arg2, arg3)
     cpp_reader *pfile;
     char *msg;
     char *arg1, *arg2, *arg3;
{
  if (CPP_OPTIONS (pfile)->pedantic_errors)
    cpp_error (pfile, msg, arg1, arg2, arg3);
  else
    cpp_warning (pfile, msg, arg1, arg2, arg3);
}

void
cpp_pedwarn_with_line (pfile, line, msg, arg1, arg2, arg3)
     cpp_reader *pfile;
     int line;
     char *msg;
     char *arg1, *arg2, *arg3;
{
  if (CPP_OPTIONS (pfile)->pedantic_errors)
    cpp_error_with_line (pfile, line, msg, arg1, arg2, arg3);
  else
    cpp_warning_with_line (pfile, line, msg, arg1, arg2, arg3);
}

/* Report a warning (or an error if pedantic_errors)
   giving specified file name and line number, not current.  */

void
cpp_pedwarn_with_file_and_line (pfile, file, line, msg, arg1, arg2, arg3)
     cpp_reader *pfile;
     char *file;
     int line;
     char *msg;
     char *arg1, *arg2, *arg3;
{
  if (!CPP_OPTIONS (pfile)->pedantic_errors
      && CPP_OPTIONS (pfile)->inhibit_warnings)
    return;
  if (file != NULL)
    fprintf (stderr, "%s:%d: ", file, line);
  if (CPP_OPTIONS (pfile)->pedantic_errors)
    pfile->errors++;
  else
    fprintf (stderr, "warning: ");
  fprintf (stderr, msg, arg1, arg2, arg3);
  fprintf (stderr, "\n");
}

void
fatal (str, arg)
     char *str, *arg;
{
  fprintf (stderr, "%s: ", progname);
  fprintf (stderr, str, arg);
  fprintf (stderr, "\n");
  exit (FAILURE_EXIT_CODE);
}


/*
 * my_strerror - return the descriptive text associated with an `errno' code.
 */

char *
my_strerror (errnum)
     int errnum;
{
  char *result;

#ifndef VMS
#ifndef HAVE_STRERROR
  result = (char *) ((errnum < sys_nerr) ? sys_errlist[errnum] : 0);
#else
  result = strerror (errnum);
#endif
#else	/* VMS */
  /* VAXCRTL's strerror() takes an optional second argument, which only
     matters when the first argument is EVMSERR.  However, it's simplest
     just to pass it unconditionally.  `vaxc$errno' is declared in
     <errno.h>, and maintained by the library in parallel with `errno'.
     We assume that caller's `errnum' either matches the last setting of
     `errno' by the library or else does not have the value `EVMSERR'.  */

  result = strerror (errnum, vaxc$errno);
#endif

  if (!result)
    result = "undocumented I/O error";

  return result;
}

/* Error including a message from `errno'.  */

void
cpp_error_from_errno (pfile, name)
     cpp_reader *pfile;
     char *name;
{
  int i;
  cpp_buffer *ip = cpp_file_buffer (pfile);

  cpp_print_containing_files (pfile);

  if (ip != NULL)
    fprintf (stderr, "%s:%d: ", ip->nominal_fname, ip->lineno);

  fprintf (stderr, "%s: %s\n", name, my_strerror (errno));

  pfile->errors++;
}

void
cpp_perror_with_name (pfile, name)
     cpp_reader *pfile;
     char *name;
{
  fprintf (stderr, "%s: ", progname);
  fprintf (stderr, "%s: %s\n", name, my_strerror (errno));
  pfile->errors++;
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
  exit (FAILURE_EXIT_CODE);
#endif
}
