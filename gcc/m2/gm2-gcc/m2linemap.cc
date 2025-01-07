/* m2linemap.cc provides an interface to GCC linemaps.

Copyright (C) 2012-2025 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#define INCLUDE_STRING
#include "gcc-consolidation.h"

/* Utilize some of the C build routines */

#include "../gm2-lang.h"
#include "../m2-tree.h"

#include "m2assert.h"
#include "m2block.h"
#include "m2decl.h"
#include "m2expr.h"
#include "m2options.h"
#include "m2tree.h"
#include "m2type.h"
#define m2linemap_c
#include "m2linemap.h"
#include "m2color.h"

static int inFile = FALSE;

#if defined(__cplusplus)
#define EXTERN extern "C"
#else
#define EXTERN
#endif

/* Start getting locations from a new file.  */

EXTERN
void
m2linemap_StartFile (void *filename, unsigned int linebegin)
{
  if (inFile)
    m2linemap_EndFile ();
  linemap_add (line_table, LC_ENTER, false,
               xstrdup (reinterpret_cast<char *> (filename)), linebegin);
  inFile = TRUE;
}

/* Tell the line table the file has ended.  */

EXTERN
void
m2linemap_EndFile (void)
{
  linemap_add (line_table, LC_LEAVE, 0, NULL, 0);
  inFile = FALSE;
}

/* Indicate that there is a new source file line number with a
   maximum width.  */

EXTERN
void
m2linemap_StartLine (unsigned int linenumber, unsigned int linesize)
{
  linemap_line_start (line_table, linenumber, linesize);
}

/* GetLocationColumn, returns a location_t based on the current line
   number and column.  */

EXTERN
location_t
m2linemap_GetLocationColumn (unsigned int column)
{
  return linemap_position_for_column (line_table, column);
}

/* GetLocationRange, returns a location based on the start column
   and end column.  */

EXTERN
location_t
m2linemap_GetLocationRange (unsigned int start, unsigned int end)
{
  location_t caret = m2linemap_GetLocationColumn (start);

  source_range where;
  where.m_start = linemap_position_for_column (line_table, start);
  where.m_finish = linemap_position_for_column (line_table, end);
  return make_location (caret, where);
}


static
int
isSrcLocation (location_t location)
{
  return (location != BUILTINS_LOCATION) && (location != UNKNOWN_LOCATION);
}


/* GetLocationBinary, returns a location based on the expression
   start caret finish locations.  */

EXTERN
location_t
m2linemap_GetLocationBinary (location_t caret, location_t start, location_t finish)
{
  if (isSrcLocation (start) && isSrcLocation (finish) && isSrcLocation (caret)
    && (m2linemap_GetFilenameFromLocation (start) != NULL))
    {
      linemap_add (line_table, LC_ENTER, false, xstrdup (m2linemap_GetFilenameFromLocation (start)), 1);
      gcc_assert (inFile);
      location_t location = make_location (caret, start, finish);
      linemap_add (line_table, LC_LEAVE, false, NULL, 0);
      return location;
    }
  return caret;
}

/* GetLineNoFromLocation - returns the lineno given a location.  */

EXTERN
int
m2linemap_GetLineNoFromLocation (location_t location)
{
  if (isSrcLocation (location) && (!M2Options_GetCpp ()))
    {
      expanded_location xl = expand_location (location);
      return xl.line;
    }
  return 0;
}

/* GetColumnNoFromLocation - returns the columnno given a location.  */

EXTERN
int
m2linemap_GetColumnNoFromLocation (location_t location)
{
  if (isSrcLocation (location) && (!M2Options_GetCpp ()))
    {
      expanded_location xl = expand_location (location);
      return xl.column;
    }
  return 0;
}

/* GetFilenameFromLocation - returns the filename given a location.  */

EXTERN
const char *
m2linemap_GetFilenameFromLocation (location_t location)
{
  if (isSrcLocation (location) && (!M2Options_GetCpp ()))
    {
      expanded_location xl = expand_location (location);
      return xl.file;
    }
  return NULL;
}

/* ErrorAt - issue an error message.  */

EXTERN
void
m2linemap_ErrorAt (location_t location, char *message)
{
  error_at (location, "%s", message);
}

/* m2linemap_ErrorAtf - wraps up an error message.  */

static void
m2linemap_ErrorAtf_1 (location_t location, const char *message, ...)
{
  diagnostic_info diagnostic;
  va_list ap;
  rich_location richloc (line_table, location);

  va_start (ap, message);
  diagnostic_set_info (&diagnostic, message, &ap, &richloc, DK_ERROR);
  diagnostic_report_diagnostic (global_dc, &diagnostic);
  va_end (ap);
}

void
m2linemap_ErrorAtf (location_t location, const char *message)
{
  m2linemap_ErrorAtf_1 (location, "%s", message);
}

/* m2linemap_WarningAtf - wraps up a warning message.  */

static void
m2linemap_WarningAtf_1 (location_t location, const char *message, ...)
{
  diagnostic_info diagnostic;
  va_list ap;
  rich_location richloc (line_table, location);

  va_start (ap, message);
  diagnostic_set_info (&diagnostic, message, &ap, &richloc, DK_WARNING);
  diagnostic_report_diagnostic (global_dc, &diagnostic);
  va_end (ap);
}

void
m2linemap_WarningAtf (location_t location, const char *message)
{
  m2linemap_WarningAtf_1 (location, "%s", message);
}

/* m2linemap_NoteAtf - wraps up a note message.  */

static void
m2linemap_NoteAtf_1 (location_t location, const char *message, ...)
{
  diagnostic_info diagnostic;
  va_list ap;
  rich_location richloc (line_table, location);

  va_start (ap, message);
  diagnostic_set_info (&diagnostic, message, &ap, &richloc, DK_NOTE);
  diagnostic_report_diagnostic (global_dc, &diagnostic);
  va_end (ap);
}

void
m2linemap_NoteAtf (location_t location, const char *message)
{
  m2linemap_NoteAtf_1 (location, "%s", message);
}

/* m2linemap_internal_error - allow Modula-2 to use the GCC internal error.  */

void
m2linemap_internal_error (const char *message)
{
  internal_error ("%s", message);
}


/* Code derived from rust.  */

static std::string
mformat_value ()
{
  return std::string (xstrerror (errno));
}


static std::string
expand_format (const char *fmt)
{
  std::string result;
  for (const char *c = fmt; *c; ++c)
    {
      if (*c != '%')
	{
	  result += *c;
	  continue;
	}
      c++;
      switch (*c)
	{
	  case '\0': {
	    // malformed format string
	    gcc_unreachable ();
	  }
	  case '%': {
	    result += '%';
	    break;
	  }
	  case 'm': {
	    result += mformat_value ();
	    break;
	  }
	  case '<': {
	    result += m2color_open_quote ();
	    break;
	  }
	  case '>': {
	    result += m2color_close_quote ();
	    break;
	  }
	  case 'q': {
	    result += m2color_open_quote ();
	    c++;
	    if (*c == 'm')
	      result += mformat_value ();
	    else
	      {
		result += '%';
		result += *c;
	      }
	    result += m2color_close_quote ();
	    break;
	  }
	  default: {
	    result += '%';
	    result += *c;
	  }
	}
    }
  return result;
}

static std::string
expand_message (const char *fmt, va_list ap)
{
  char *mbuf = 0;
  std::string expanded_fmt = expand_format (fmt);
  int nwr = vasprintf (&mbuf, expanded_fmt.c_str (), ap);
  if (nwr == -1)
    {
      // memory allocation failed
      error_at (UNKNOWN_LOCATION,
		"memory allocation failed in vasprintf");
      gcc_assert (0);
    }
  std::string rval = std::string (mbuf);
  free (mbuf);
  return rval;
}


static void
gm2_internal_error_at (location_t location, const std::string &errmsg)
{
  expanded_location exp_loc = expand_location (location);
  std::string loc_str;
  std::string file_str;

  if (exp_loc.file == NULL)
    file_str.clear ();
  else
    file_str = std::string (exp_loc.file);

  if (! file_str.empty ())
    {
      loc_str += file_str;
      loc_str += ':';
      loc_str += std::to_string (exp_loc.line);
      loc_str += ':';
      loc_str += std::to_string (exp_loc.column);
    }
  if (loc_str.empty ())
    internal_error ("%s", errmsg.c_str ());
  else
    internal_error ("at %s, %s", loc_str.c_str (), errmsg.c_str ());
}


void
m2linemap_internal_error_at (location_t location, const char *fmt, ...)
{
  va_list ap;

  va_start (ap, fmt);
  gm2_internal_error_at (location, expand_message (fmt, ap));
  va_end (ap);
}

/* UnknownLocation - return the predefined location representing an
   unknown location.  */

EXTERN
location_t
m2linemap_UnknownLocation (void)
{
  return UNKNOWN_LOCATION;
}

/* BuiltinsLocation - return the predefined location representing a
   builtin.  */

EXTERN
location_t
m2linemap_BuiltinsLocation (void)
{
  return BUILTINS_LOCATION;
}
