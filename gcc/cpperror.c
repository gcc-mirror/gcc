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

static void print_containing_files	PARAMS ((cpp_buffer *));
static void print_location		PARAMS ((cpp_reader *,
						 const char *,
						 const cpp_lexer_pos *));

/* Don't remove the blank before do, as otherwise the exgettext
   script will mistake this as a function definition */
#define v_message(msgid, ap) \
 do { vfprintf (stderr, _(msgid), ap); putc ('\n', stderr); } while (0)

/* Print the file names and line numbers of the #include
   commands which led to the current file.  */
static void
print_containing_files (ip)
     cpp_buffer *ip;
{
  int first = 1;

  /* Find the other, outer source files.  */
  for (ip = ip->prev; ip; ip = ip->prev)
    {
      if (first)
	{
	  first = 0;
	  /* The current line in each outer source file is now the
	     same as the line of the #include.  */
	  fprintf (stderr,  _("In file included from %s:%u"),
		   ip->nominal_fname, CPP_BUF_LINE (ip));
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
		 ip->nominal_fname, CPP_BUF_LINE (ip));
    }
  fputs (":\n", stderr);
}

static void
print_location (pfile, filename, pos)
     cpp_reader *pfile;
     const char *filename;
     const cpp_lexer_pos *pos;
{
  cpp_buffer *buffer = pfile->buffer;

  if (!buffer)
    fprintf (stderr, "%s: ", progname);
  else
    {
      unsigned int line, col = 0;
      enum cpp_buffer_type type = buffer->type;

      /* For _Pragma buffers, we want to print the location as
	 "foo.c:5:8: _Pragma:", where foo.c is the containing buffer.
	 For diagnostics relating to command line options, we want to
	 print "<command line>:" with no line number.  */
      if (type == BUF_CL_OPTION || type == BUF_BUILTIN)
	line = 0;
      else
	{
	  if (type == BUF_PRAGMA)
	    {
	      buffer = buffer->prev;
	      line = CPP_BUF_LINE (buffer);
	      col = CPP_BUF_COL (buffer);
	    }
	  else
	    {
	      if (pos == 0)
		pos = cpp_get_line (pfile);
	      line = pos->line;
	      col = pos->col;
	    }

	  if (col == 0)
	    col = 1;

	  /* Don't repeat the include stack unnecessarily.  */
	  if (buffer->prev && ! buffer->include_stack_listed)
	    {
	      buffer->include_stack_listed = 1;
	      print_containing_files (buffer);
	    }
	}

      if (filename == 0)
	filename = buffer->nominal_fname;

      if (line == 0)
	fprintf (stderr, "%s: ", filename);
      else if (CPP_OPTION (pfile, show_column) == 0)
	fprintf (stderr, "%s:%u: ", filename, line);
      else
	fprintf (stderr, "%s:%u:%u: ", filename, line, col);

      if (type == BUF_PRAGMA)
	fprintf (stderr, "_Pragma: ");
    }
}

/* Set up for an error message: print the file and line, bump the error
   counter, etc.
   If it returns 0, this error has been suppressed.  */

int
_cpp_begin_message (pfile, code, file, pos)
     cpp_reader *pfile;
     enum error_type code;
     const char *file;
     const cpp_lexer_pos *pos;
{
  int is_warning = 0;

  switch (code)
    {
    case PEDWARN:
    case WARNING:
      if (CPP_IN_SYSTEM_HEADER (pfile)
	  && ! CPP_OPTION (pfile, warn_system_headers))
	return 0;
    case WARNING_SYSHDR:
      if (CPP_OPTION (pfile, warnings_are_errors)
	  || (code == PEDWARN && CPP_OPTION (pfile, pedantic_errors)))
	{
	  if (CPP_OPTION (pfile, inhibit_errors))
	    return 0;
	  if (pfile->errors < CPP_FATAL_LIMIT)
	    pfile->errors++;
	}
      else
	{
          if (CPP_OPTION (pfile, inhibit_warnings))
	    return 0;
	  is_warning = 1;
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

  print_location (pfile, file, pos);
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

  if (_cpp_begin_message (pfile, ICE, NULL, 0))
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

  if (_cpp_begin_message (pfile, FATAL, NULL, 0))
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

  if (_cpp_begin_message (pfile, ERROR, NULL, 0))
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
  cpp_lexer_pos pos;
  
  VA_START (ap, msgid);
  
#ifndef ANSI_PROTOTYPES
  pfile = va_arg (ap, cpp_reader *);
  line = va_arg (ap, int);
  column = va_arg (ap, int);
  msgid = va_arg (ap, const char *);
#endif

  pos.line = line;
  pos.col = column;
  if (_cpp_begin_message (pfile, ERROR, NULL, &pos))
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

  if (_cpp_begin_message (pfile, WARNING, NULL, 0))
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
  cpp_lexer_pos pos;
  
  VA_START (ap, msgid);
  
#ifndef ANSI_PROTOTYPES
  pfile = va_arg (ap, cpp_reader *);
  line = va_arg (ap, int);
  column = va_arg (ap, int);
  msgid = va_arg (ap, const char *);
#endif

  pos.line = line;
  pos.col = column;
  if (_cpp_begin_message (pfile, WARNING, NULL, &pos))
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

  if (_cpp_begin_message (pfile, PEDWARN, NULL, 0))
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
  cpp_lexer_pos pos;
  
  VA_START (ap, msgid);
  
#ifndef ANSI_PROTOTYPES
  pfile = va_arg (ap, cpp_reader *);
  line = va_arg (ap, int);
  column = va_arg (ap, int);
  msgid = va_arg (ap, const char *);
#endif

  pos.line = line;
  pos.col = column;
  if (_cpp_begin_message (pfile, PEDWARN, NULL, &pos))
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
  cpp_lexer_pos pos;
  
  VA_START (ap, msgid);

#ifndef ANSI_PROTOTYPES
  pfile = va_arg (ap, cpp_reader *);
  file = va_arg (ap, const char *);
  line = va_arg (ap, int);
  col = va_arg (ap, int);
  msgid = va_arg (ap, const char *);
#endif

  pos.line = line;
  pos.col = col;
  if (_cpp_begin_message (pfile, PEDWARN, file, &pos))
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
