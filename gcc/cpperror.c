/* Default error handlers for CPP Library.
   Copyright (C) 1986, 87, 89, 92-95, 98, 99, 2000
   Free Software Foundation, Inc.
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

#include "config.h"
#include "system.h"
#include "cpplib.h"
#include "intl.h"

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
	      cpp_message (pfile, -1, "In file included from %s:%ld",
			   ip->nominal_fname, line);
	    }
	  else
	    cpp_message (pfile, -1, ",\n                 from %s:%ld",
			 ip->nominal_fname, line);
	}
    }
  if (! first)
    fputs (":\n", stderr);

  /* Record we have printed the status as of this time.  */
  pfile->input_stack_listing_current = 1;
}

void
cpp_file_line_for_message (pfile, filename, line, column)
     cpp_reader *pfile ATTRIBUTE_UNUSED;
     const char *filename;
     int line, column;
{
  if (filename == 0 || *filename == '\0')
    filename = "<stdin>";
  if (line == 0)
    fputs (_("<command line>: "), stderr);
  else if (column > 0)
    fprintf (stderr, "%s:%d:%d: ", filename, line, column);
  else
    fprintf (stderr, "%s:%d: ", filename, line);
}

/* IS_ERROR is 2 for "fatal" error, 1 for error, 0 for warning, -1 for notice */

void
v_cpp_message (pfile, is_error, msgid, ap)
  cpp_reader * pfile;
  int is_error;
  const char *msgid;
  va_list ap;
{
  switch (is_error)
    {
    case -1:
      break;
    case 0:
      fprintf (stderr, _("warning: "));
      break;
    case 1:
      if (pfile->errors < CPP_FATAL_LIMIT)
	pfile->errors++;
      break;
    case 2:
      pfile->errors = CPP_FATAL_LIMIT;
      break;
    default:
      cpp_fatal (pfile, "internal error: bad is_error(%d) in v_cpp_message", is_error);
    }

  vfprintf (stderr, _(msgid), ap);

  if (0 <= is_error)
    fprintf (stderr, "\n");
}

void
cpp_message VPARAMS ((cpp_reader *pfile, int is_error, const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  cpp_reader *pfile;
  int is_error;
  const char *msgid;
#endif
  va_list ap;
  
  VA_START (ap, msgid);
  
#ifndef ANSI_PROTOTYPES
  pfile = va_arg (ap, cpp_reader *);
  is_error = va_arg (ap, int);
  msgid = va_arg (ap, const char *);
#endif

  v_cpp_message(pfile, is_error, msgid, ap);
  va_end(ap);
}

/* Same as cpp_error, except we consider the error to be "fatal",
   such as inconsistent options.  I.e. there is little point in continuing.
   (We do not exit, to support use of cpplib as a library.
   Instead, it is the caller's responsibility to check
   CPP_FATAL_ERRORS.  */

void
cpp_fatal VPARAMS ((cpp_reader *pfile, const char *msgid, ...))
{  
#ifndef ANSI_PROTOTYPES
  cpp_reader *pfile;
  const char *msgid;
#endif
  va_list ap;
  
  VA_START (ap, msgid);
  
#ifndef ANSI_PROTOTYPES
  pfile = va_arg (ap, cpp_reader *);
  msgid = va_arg (ap, const char *);
#endif

  fprintf (stderr, "%s: ", progname);
  v_cpp_message (pfile, 2, msgid, ap);
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

/* Print an error message.  */

void
cpp_notice VPARAMS ((const char *msgid, ...))
{  
#ifndef ANSI_PROTOTYPES
  const char *msgid;
#endif
  va_list ap;
  
  VA_START (ap, msgid);
  
#ifndef ANSI_PROTOTYPES
  msgid = va_arg (ap, const char *);
#endif

  fprintf (stderr, "%s: ", progname);
  v_cpp_message ((cpp_reader *) 0, -1, msgid, ap);
  va_end(ap);
}

