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

/* Print the logical file location (LINE, COL) in preparation for a
   diagnostic.  Outputs the #include chain if it has changed.  A line
   of zero suppresses the include stack, and outputs the program name
   instead.  */
static void
print_location (pfile, line, col)
     cpp_reader *pfile;
     unsigned int line, col;
{
  if (!pfile->buffer || line == 0)
    fprintf (stderr, "%s: ", progname);
  else
    {
      const struct line_map *map;

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

/* Set up for a diagnostic: print the file and line, bump the error
   counter, etc.  LINE is the logical line number; zero means to print
   at the location of the previously lexed token, which tends to be
   the correct place by default.  Returns 0 if the error has been
   suppressed.  */
int
_cpp_begin_message (pfile, code, line, column)
     cpp_reader *pfile;
     int code;
     unsigned int line, column;
{
  int level = DL_EXTRACT (code);

  switch (level)
    {
    case DL_WARNING:
    case DL_PEDWARN:
      if (CPP_IN_SYSTEM_HEADER (pfile)
	  && ! CPP_OPTION (pfile, warn_system_headers))
	return 0;
      /* Fall through.  */

    case DL_WARNING_SYSHDR:
      if (CPP_OPTION (pfile, warnings_are_errors)
	  || (level == DL_PEDWARN && CPP_OPTION (pfile, pedantic_errors)))
	{
	  if (CPP_OPTION (pfile, inhibit_errors))
	    return 0;
	  level = DL_ERROR;
	  pfile->errors++;
	}
      else if (CPP_OPTION (pfile, inhibit_warnings))
	return 0;
      break;

    case DL_ERROR:
      if (CPP_OPTION (pfile, inhibit_errors))
	return 0;
      /* ICEs cannot be inhibited.  */
    case DL_ICE:
      pfile->errors++;
      break;
    }

  print_location (pfile, line, column);
  if (DL_WARNING_P (level))
    fputs (_("warning: "), stderr);
  else if (level == DL_ICE)
    fputs (_("internal error: "), stderr);

  return 1;
}

/* Don't remove the blank before do, as otherwise the exgettext
   script will mistake this as a function definition */
#define v_message(msgid, ap) \
 do { vfprintf (stderr, _(msgid), ap); putc ('\n', stderr); } while (0)

/* Exported interface.  */

/* Print an error at the location of the previously lexed token.  */
void
cpp_error VPARAMS ((cpp_reader * pfile, int level, const char *msgid, ...))
{
  unsigned int line, column;

  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, cpp_reader *, pfile);
  VA_FIXEDARG (ap, int, level);
  VA_FIXEDARG (ap, const char *, msgid);

  if (pfile->buffer)
    {
      if (CPP_OPTION (pfile, traditional))
	{
	  if (pfile->state.in_directive)
	    line = pfile->directive_line;
	  else
	    line = pfile->line;
	  column = 0;
	}
      else
	{
	  line = pfile->cur_token[-1].line;
	  column = pfile->cur_token[-1].col;
	}
    }
  else
    line = column = 0;

  if (_cpp_begin_message (pfile, level, line, column))
    v_message (msgid, ap);

  VA_CLOSE (ap);
}

/* Print an error at a specific location.  */
void
cpp_error_with_line VPARAMS ((cpp_reader *pfile, int level,
			      unsigned int line, unsigned int column,
			      const char *msgid, ...))
{
  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, cpp_reader *, pfile);
  VA_FIXEDARG (ap, int, level);
  VA_FIXEDARG (ap, unsigned int, line);
  VA_FIXEDARG (ap, unsigned int, column);
  VA_FIXEDARG (ap, const char *, msgid);

  if (_cpp_begin_message (pfile, level, line, column))
    v_message (msgid, ap);

  VA_CLOSE (ap);
}

void
cpp_errno (pfile, level, msgid)
     cpp_reader *pfile;
     int level;
     const char *msgid;
{
  if (msgid[0] == '\0')
    msgid = _("stdout");

  cpp_error (pfile, level, "%s: %s", msgid, xstrerror (errno));
}
