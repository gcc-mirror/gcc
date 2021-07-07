/* m2linemap.c provides an interface to GCC linemaps.

Copyright (C) 2012-2021 Free Software Foundation, Inc.
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

#include "gcc-consolidation.h"

/* utilize some of the C build routines */

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
#if 1
  if (isSrcLocation (start) && isSrcLocation (finish) && isSrcLocation (caret)
    && (m2linemap_GetFilenameFromLocation (start) != NULL))
    {
      linemap_add (line_table, LC_ENTER, false, xstrdup (m2linemap_GetFilenameFromLocation (start)), 1);
      gcc_assert (inFile);
      location_t location = make_location (caret, start, finish);
      /* error_at (location, "testing here"); */
      return location;
    }
#endif
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
  error_at (location, message);
}

/* m2linemap_ErrorAtf - wraps up an error message.  */

void
m2linemap_ErrorAtf (location_t location, const char *message, ...)
{
  diagnostic_info diagnostic;
  va_list ap;
  rich_location richloc (line_table, location);

  va_start (ap, message);
  diagnostic_set_info (&diagnostic, message, &ap, &richloc, DK_ERROR);
  diagnostic_report_diagnostic (global_dc, &diagnostic);
  va_end (ap);
}

/* m2linemap_WarningAtf - wraps up a warning message.  */

void
m2linemap_WarningAtf (location_t location, const char *message, ...)
{
  diagnostic_info diagnostic;
  va_list ap;
  rich_location richloc (line_table, location);

  va_start (ap, message);
  diagnostic_set_info (&diagnostic, message, &ap, &richloc, DK_WARNING);
  diagnostic_report_diagnostic (global_dc, &diagnostic);
  va_end (ap);
}

/* m2linemap_NoteAtf - wraps up a note message.  */

void
m2linemap_NoteAtf (location_t location, const char *message, ...)
{
  diagnostic_info diagnostic;
  va_list ap;
  rich_location richloc (line_table, location);

  va_start (ap, message);
  diagnostic_set_info (&diagnostic, message, &ap, &richloc, DK_NOTE);
  diagnostic_report_diagnostic (global_dc, &diagnostic);
  va_end (ap);
}

/* m2linemap_internal_error - allow Modula-2 to use the GCC internal error.  */

void
m2linemap_internal_error (const char *message)
{
  internal_error (message);
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
