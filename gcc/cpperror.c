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
#include "cpplib.h"
#include "cpphash.h"
#include "intl.h"

static void print_containing_files	PARAMS ((cpp_reader *, cpp_buffer *));
static void print_file_and_line		PARAMS ((const char *, unsigned int,
						 unsigned int));

#define v_message(msgid, ap) \
do { vfprintf (stderr, _(msgid), ap); putc ('\n', stderr); } while (0)

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
	  /* N.B. The current line in each outer source file is one
	     greater than the line of the #include, so we must
	     subtract one to correct for that.  */
	  fprintf (stderr,  _("In file included from %s:%u"),
		   ip->nominal_fname, CPP_BUF_LINE (ip) - 1);
	}
      else
	/* Translators note: this message is used in conjunction
	   with "In file included from %s:%ld" and some other
	   tricks.  We want something like this:

	   | In file included from sys/select.h:123,
	   |                  from sys/types.h:234,
	   |                  from userfile.c:31:
	   | bits/select.h:45: <error message here>

	   with all the "from"s lined up.
	   The trailing comma is at the beginning of this message,
	   and the trailing colon is not translated.  */
	fprintf (stderr, _(",\n                 from %s:%u"),
		 ip->nominal_fname, CPP_BUF_LINE (ip) - 1);
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
  if (line == 0)
    fputs (_("<command line>: "), stderr);
  else
    {
      if (filename == 0 || *filename == '\0')
	filename = "<stdin>";
      if (column > 0)
	fprintf (stderr, "%s:%u:%u: ", filename, line, column);
      else
	fprintf (stderr, "%s:%u: ", filename, line);
    }
}

/* Set up for an error message: print the file and line, bump the error
   counter, etc.
   If it returns 0, this error has been suppressed.  */

int
_cpp_begin_message (pfile, code, file, line, col)
     cpp_reader *pfile;
     enum error_type code;
     const char *file;
     unsigned int line;
     unsigned int col;
{
  cpp_buffer *ip = CPP_BUFFER (pfile);
  int is_warning = 0;

  switch (code)
    {
    case WARNING:
      if (! CPP_OPTION (pfile, warnings_are_errors))
	{
	  if (CPP_OPTION (pfile, inhibit_warnings))
	    return 0;
	  is_warning = 1;
	}
      else
	{
	  if (CPP_OPTION (pfile, inhibit_errors))
	    return 0;
	  if (pfile->errors < CPP_FATAL_LIMIT)
	    pfile->errors++;
	}
      break;

    case PEDWARN:
      if (! CPP_OPTION (pfile, pedantic_errors))
	{
	  if (CPP_OPTION (pfile, inhibit_warnings))
	    return 0;
	  is_warning = 1;
	}
      else
	{
	  if (CPP_OPTION (pfile, inhibit_errors))
	    return 0;
	  if (pfile->errors < CPP_FATAL_LIMIT)
	    pfile->errors++;
	}
      break;
	
    case ERROR:
      if (CPP_OPTION (pfile, inhibit_errors))
	return 0;
      if (pfile->errors < CPP_FATAL_LIMIT)
	pfile->errors++;
      break;
      /* Fatal errors cannot be inhibited.  */
    case FATAL:
      pfile->errors = CPP_FATAL_LIMIT;
      break;
    case ICE:
      fprintf (stderr, _("internal error: "));
      pfile->errors = CPP_FATAL_LIMIT;
      break;
    }

  if (ip)
    {
      if (file == NULL)
	file = ip->nominal_fname;
      if (line == 0)
	line = _cpp_get_line (pfile, &col);
      print_containing_files (pfile, ip);
      print_file_and_line (file, line,
			   CPP_OPTION (pfile, show_column) ? col : 0);
    }
  else
    fprintf (stderr, "%s: ", progname);

  if (is_warning)
    fputs (_("warning: "), stderr);

  return 1;
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

  if (_cpp_begin_message (pfile, ICE, NULL, 0, 0))
    v_message (msgid, ap);
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

  if (_cpp_begin_message (pfile, FATAL, NULL, 0, 0))
    v_message (msgid, ap);
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

  if (_cpp_begin_message (pfile, ERROR, NULL, 0, 0))
    v_message (msgid, ap);
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

  if (_cpp_begin_message (pfile, ERROR, NULL, line, column))
    v_message (msgid, ap);
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

  if (_cpp_begin_message (pfile, WARNING, NULL, 0, 0))
    v_message (msgid, ap);
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

  if (_cpp_begin_message (pfile, WARNING, NULL, line, column))
    v_message (msgid, ap);
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

  if (_cpp_begin_message (pfile, PEDWARN, NULL, 0, 0))
    v_message (msgid, ap);
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

  if (_cpp_begin_message (pfile, PEDWARN, NULL, line, column))
    v_message (msgid, ap);
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

  if (_cpp_begin_message (pfile, PEDWARN, file, line, col))
    v_message (msgid, ap);
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

const char *
cpp_type2name (type)
     enum cpp_ttype type;
{
  return (const char *) _cpp_token_spellings[type].name;
}

