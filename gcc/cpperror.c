/* Default error handlers for CPP Library.
   Copyright (C) 1986, 1987, 1989, 1992, 1993, 1994, 1995, 1998, 1999, 2000,
   2001, 2002  Free Software Foundation, Inc.
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

static void print_location PARAMS ((cpp_reader *, unsigned int, unsigned int));

/* Don't remove the blank before do, as otherwise the exgettext
   script will mistake this as a function definition */
#define v_message(msgid, ap) \
 do { vfprintf (stderr, _(msgid), ap); putc ('\n', stderr); } while (0)

/* Print the logical file location (LINE, COL) in preparation for a
   diagnostic.  Outputs the #include chain if it has changed.  */
static void
print_location (pfile, line, col)
     cpp_reader *pfile;
     unsigned int line, col;
{
  cpp_buffer *buffer = pfile->buffer;

  if (!buffer)
    fprintf (stderr, "%s: ", progname);
  else
    {
      const struct line_map *map;

      if (line == 0)
	{
	  line = pfile->cur_token[-1].line;
	  col = pfile->cur_token[-1].col;
	}

      map = lookup_line (&pfile->line_maps, line);
      print_containing_files (&pfile->line_maps, map);

      line = SOURCE_LINE (map, line);
      if (col == 0)
	col = 1;

      if (line == 0)
	fprintf (stderr, "%s:", map->to_file);
      else if (CPP_OPTION (pfile, show_column) == 0)
	fprintf (stderr, "%s:%u:", map->to_file, line);
      else
	fprintf (stderr, "%s:%u:%u:", map->to_file, line, col);

      fputc (' ', stderr);
    }
}

/* Set up for an error message: print the file and line, bump the error
   counter, etc.  LINE is the logical line number; zero means to print
   at the location of the previously lexed token, which tends to be the
   correct place by default.  Returns 0 if the error has been suppressed.  */
int
_cpp_begin_message (pfile, code, line, column)
     cpp_reader *pfile;
     enum error_type code;
     unsigned int line, column;
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

  print_location (pfile, line, column);
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
  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, cpp_reader *, pfile);
  VA_FIXEDARG (ap, const char *, msgid);

  if (_cpp_begin_message (pfile, ICE, 0, 0))
    v_message (msgid, ap);

  VA_CLOSE (ap);
}

/* Same as cpp_error, except we consider the error to be "fatal",
   such as inconsistent options.  I.e. there is little point in continuing.
   (We do not exit, to support use of cpplib as a library.
   Instead, it is the caller's responsibility to check
   CPP_FATAL_ERRORS.  */
void
cpp_fatal VPARAMS ((cpp_reader *pfile, const char *msgid, ...))
{  
  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, cpp_reader *, pfile);
  VA_FIXEDARG (ap, const char *, msgid);

  if (_cpp_begin_message (pfile, FATAL, 0, 0))
    v_message (msgid, ap);

  VA_CLOSE (ap);
}

/* Print an error at the location of the previously lexed token.  */
void
cpp_error VPARAMS ((cpp_reader * pfile, const char *msgid, ...))
{
  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, cpp_reader *, pfile);
  VA_FIXEDARG (ap, const char *, msgid);

  if (_cpp_begin_message (pfile, ERROR, 0, 0))
    v_message (msgid, ap);

  VA_CLOSE (ap);
}

/* Print an error at a specific location.  */
void
cpp_error_with_line VPARAMS ((cpp_reader *pfile, int line, int column,
			     const char *msgid, ...))
{
  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, cpp_reader *, pfile);
  VA_FIXEDARG (ap, int, line);
  VA_FIXEDARG (ap, int, column);
  VA_FIXEDARG (ap, const char *, msgid);

  if (_cpp_begin_message (pfile, ERROR, line, column))
    v_message (msgid, ap);

  VA_CLOSE (ap);
}

/* Error including a message from `errno'.  */
void
cpp_error_from_errno (pfile, name)
     cpp_reader *pfile;
     const char *name;
{
  cpp_error (pfile, "%s: %s", name, xstrerror (errno));
}

/* Print a warning at the location of the previously lexed token.  */
void
cpp_warning VPARAMS ((cpp_reader * pfile, const char *msgid, ...))
{
  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, cpp_reader *, pfile);
  VA_FIXEDARG (ap, const char *, msgid);

  if (_cpp_begin_message (pfile, WARNING, 0, 0))
    v_message (msgid, ap);

  VA_CLOSE (ap);
}

/* Print a warning at a specific location.  */
void
cpp_warning_with_line VPARAMS ((cpp_reader * pfile, int line, int column,
			       const char *msgid, ...))
{
  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, cpp_reader *, pfile);
  VA_FIXEDARG (ap, int, line);
  VA_FIXEDARG (ap, int, column);
  VA_FIXEDARG (ap, const char *, msgid);

  if (_cpp_begin_message (pfile, WARNING, line, column))
    v_message (msgid, ap);

  VA_CLOSE (ap);
}

/* Pedwarn at the location of the previously lexed token.  */
void
cpp_pedwarn VPARAMS ((cpp_reader * pfile, const char *msgid, ...))
{
  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, cpp_reader *, pfile);
  VA_FIXEDARG (ap, const char *, msgid);

  if (_cpp_begin_message (pfile, PEDWARN, 0, 0))
    v_message (msgid, ap);

  VA_CLOSE (ap);
}

/* Pedwarn at a specific location.  */
void
cpp_pedwarn_with_line VPARAMS ((cpp_reader * pfile, int line, int column,
			       const char *msgid, ...))
{
  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, cpp_reader *, pfile);
  VA_FIXEDARG (ap, int, line);
  VA_FIXEDARG (ap, int, column);
  VA_FIXEDARG (ap, const char *, msgid);

  if (_cpp_begin_message (pfile, PEDWARN, line, column))
    v_message (msgid, ap);

  VA_CLOSE (ap);
}

/* Print an error message not associated with the translation unit.  */
void
cpp_notice VPARAMS ((cpp_reader *pfile, const char *msgid, ...))
{
  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, cpp_reader *, pfile);
  VA_FIXEDARG (ap, const char *, msgid);

  if (pfile->errors < CPP_FATAL_LIMIT)
    pfile->errors++;

  v_message (msgid, ap);

  VA_CLOSE (ap);
}

/* Print an error message originating from ERRNO and not associated
   with the translation unit.  */
void
cpp_notice_from_errno (pfile, name)
     cpp_reader *pfile;
     const char *name;
{
  if (name[0] == '\0')
    name = "stdout";
  cpp_notice (pfile, "%s: %s", name, xstrerror (errno));
}
