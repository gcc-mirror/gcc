/* Default error handlers for CPP Library.
   Copyright (C) 1986, 1987, 1989, 1992, 1993, 1994, 1995, 1998, 1999, 2000
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
#include "hashtab.h"
#include "cpplib.h"
#include "cpphash.h"
#include "intl.h"

static void print_containing_files	PARAMS ((cpp_reader *, cpp_buffer *));
static void print_file_and_line		PARAMS ((const char *, unsigned int,
						 unsigned int));
static void v_message			PARAMS ((cpp_reader *, int,
						 const char *,
						 unsigned int, unsigned int,
						 const char *, va_list));

/* Print the file names and line numbers of the #include
   commands which led to the current file.  */

static void
print_containing_files (pfile, ip)
     cpp_reader *pfile;
     cpp_buffer *ip;
{
  int first = 1;

  /* If stack of files hasn't changed since we last printed
     this info, don't repeat it.  */
  if (pfile->input_stack_listing_current)
    return;

  /* Find the other, outer source files.  */
  for (ip = CPP_PREV_BUFFER (ip); ip != NULL; ip = CPP_PREV_BUFFER (ip))
    {
      if (first)
	{
	  first = 0;
	  fprintf (stderr,  _("In file included from %s:%u"),
		   ip->nominal_fname, CPP_BUF_LINE (ip));
	}
      else
	/* Translators note: this message is used in conjunction
	   with "In file included from %s:%ld" and some other
	   tricks.  We want something like this:

	   In file included from sys/select.h:123,
	                    from sys/types.h:234,
	                    from userfile.c:31:
	   bits/select.h:45: <error message here>

	   The trailing comma is at the beginning of this message,
	   and the trailing colon is not translated.  */
	fprintf (stderr, _(",\n                 from %s:%u"),
		 ip->nominal_fname, CPP_BUF_LINE (ip));
    }
  if (first == 0)
    fputs (":\n", stderr);

  /* Record we have printed the status as of this time.  */
  pfile->input_stack_listing_current = 1;
}

static void
print_file_and_line (filename, line, column)
     const char *filename;
     unsigned int line, column;
{
  if (filename == 0 || *filename == '\0')
    filename = "<stdin>";
  if (line == 0)
    fputs (_("<command line>: "), stderr);
  else if (column > 0)
    fprintf (stderr, "%s:%u:%u: ", filename, line, column);
  else
    fprintf (stderr, "%s:%u: ", filename, line);
}

/* IS_ERROR is 3 for ICE, 2 for merely "fatal" error,
   1 for error, 0 for warning.  */

static void
v_message (pfile, is_error, file, line, col, msg, ap)
     cpp_reader *pfile;
     int is_error;
     const char *file;
     unsigned int line;
     unsigned int col;
     const char *msg;
     va_list ap;
{
  cpp_buffer *ip = cpp_file_buffer (pfile);

  if (ip)
    {
      if (file == NULL)
	file = ip->nominal_fname;
      if (line == 0)
	{
	  line = CPP_BUF_LINE (ip);
	  col = CPP_BUF_COL (ip);
	}
      print_containing_files (pfile, ip);
      print_file_and_line (file, line,
			   CPP_OPTION (pfile, show_column) ? col : 0);
    }
  else
    fprintf (stderr, "%s: ", progname);

  switch (is_error)
    {
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
    case 3:
      fprintf (stderr, _("internal error: "));
      pfile->errors = CPP_FATAL_LIMIT;
      break;
    default:
      cpp_ice (pfile, "bad is_error(%d) in v_message", is_error);
    }

  vfprintf (stderr, _(msg), ap);
  putc ('\n', stderr);
}

/* Exported interface.  */

/* For reporting internal errors.  Prints "internal error: " for you,
   otherwise identical to cpp_fatal.  */

void
cpp_ice VPARAMS ((cpp_reader *pfile, const char *msgid, ...))
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

  v_message (pfile, 3, NULL, 0, 0, msgid, ap);
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

  v_message (pfile, 2, NULL, 0, 0, msgid, ap);
  va_end(ap);
}

void
cpp_error VPARAMS ((cpp_reader * pfile, const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  cpp_reader *pfile;
  const char *msgid;
#endif
  va_list ap;

  VA_START(ap, msgid);
  
#ifndef ANSI_PROTOTYPES
  pfile = va_arg (ap, cpp_reader *);
  msgid = va_arg (ap, const char *);
#endif

  if (CPP_OPTION (pfile, inhibit_errors))
    return;

  v_message (pfile, 1, NULL, 0, 0, msgid, ap);
  va_end(ap);
}

void
cpp_error_with_line VPARAMS ((cpp_reader *pfile, int line, int column,
			     const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  cpp_reader *pfile;
  int line;
  int column;
  const char *msgid;
#endif
  va_list ap;
  
  VA_START (ap, msgid);
  
#ifndef ANSI_PROTOTYPES
  pfile = va_arg (ap, cpp_reader *);
  line = va_arg (ap, int);
  column = va_arg (ap, int);
  msgid = va_arg (ap, const char *);
#endif

  if (CPP_OPTION (pfile, inhibit_errors))
    return;

  v_message (pfile, 1, NULL, line, column, msgid, ap);
  va_end(ap);
}

/* Error including a message from `errno'.  */
void
cpp_error_from_errno (pfile, name)
     cpp_reader *pfile;
     const char *name;
{
  cpp_error (pfile, "%s: %s", name, xstrerror (errno));
}

void
cpp_warning VPARAMS ((cpp_reader * pfile, const char *msgid, ...))
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

  if (CPP_OPTION (pfile, inhibit_warnings))
    return;

  v_message (pfile, 0, NULL, 0, 0, msgid, ap);
  va_end(ap);
}

void
cpp_warning_with_line VPARAMS ((cpp_reader * pfile, int line, int column,
			       const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  cpp_reader *pfile;
  int line;
  int column;
  const char *msgid;
#endif
  va_list ap;
  
  VA_START (ap, msgid);
  
#ifndef ANSI_PROTOTYPES
  pfile = va_arg (ap, cpp_reader *);
  line = va_arg (ap, int);
  column = va_arg (ap, int);
  msgid = va_arg (ap, const char *);
#endif

  if (CPP_OPTION (pfile, inhibit_warnings))
    return;

  v_message (pfile, 0, NULL, line, column, msgid, ap);
  va_end(ap);
}

void
cpp_pedwarn VPARAMS ((cpp_reader * pfile, const char *msgid, ...))
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

  if (CPP_OPTION (pfile, pedantic_errors)
      ? CPP_OPTION (pfile, inhibit_errors)
      : CPP_OPTION (pfile, inhibit_warnings))
    return;

  v_message (pfile, CPP_OPTION (pfile, pedantic_errors),
		 NULL, 0, 0, msgid, ap);
  va_end(ap);
}

void
cpp_pedwarn_with_line VPARAMS ((cpp_reader * pfile, int line, int column,
			       const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  cpp_reader *pfile;
  int line;
  int column;
  const char *msgid;
#endif
  va_list ap;
  
  VA_START (ap, msgid);
  
#ifndef ANSI_PROTOTYPES
  pfile = va_arg (ap, cpp_reader *);
  line = va_arg (ap, int);
  column = va_arg (ap, int);
  msgid = va_arg (ap, const char *);
#endif

  if (CPP_OPTION (pfile, pedantic_errors)
      ? CPP_OPTION (pfile, inhibit_errors)
      : CPP_OPTION (pfile, inhibit_warnings))
    return;

  v_message (pfile, CPP_OPTION (pfile, pedantic_errors),
		 NULL, line, column, msgid, ap);
  va_end(ap);
}

/* Report a warning (or an error if pedantic_errors)
   giving specified file name and line number, not current.  */

void
cpp_pedwarn_with_file_and_line VPARAMS ((cpp_reader *pfile,
					 const char *file, int line, int col,
					 const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  cpp_reader *pfile;
  const char *file;
  int line;
  int col;
  const char *msgid;
#endif
  va_list ap;
  
  VA_START (ap, msgid);

#ifndef ANSI_PROTOTYPES
  pfile = va_arg (ap, cpp_reader *);
  file = va_arg (ap, const char *);
  line = va_arg (ap, int);
  col = va_arg (ap, int);
  msgid = va_arg (ap, const char *);
#endif

  if (CPP_OPTION (pfile, pedantic_errors)
      ? CPP_OPTION (pfile, inhibit_errors)
      : CPP_OPTION (pfile, inhibit_warnings))
    return;

  v_message (pfile, CPP_OPTION (pfile, pedantic_errors),
		 file, line, col, msgid, ap);
  va_end(ap);
}

/* Print an error message not associated with a file.  */
void
cpp_notice VPARAMS ((cpp_reader *pfile, const char *msgid, ...))
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

  if (pfile->errors < CPP_FATAL_LIMIT)
    pfile->errors++;

  vfprintf (stderr, _(msgid), ap);
  putc('\n', stderr);

  va_end(ap);
}

void
cpp_notice_from_errno (pfile, name)
     cpp_reader *pfile;
     const char *name;
{
  if (name[0] == '\0')
    name = "stdout";
  cpp_notice (pfile, "%s: %s", name, xstrerror (errno));
}
