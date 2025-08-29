/* Utilities for implementing "dump" functions for the diagnostics subsystem.
   Copyright (C) 2025 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "diagnostics/logging.h"

namespace diagnostics {
namespace logging {

logger::logger (output_file outfile)
: m_outfile (std::move (outfile)),
  m_log_depth (0)
{
}

void
logger::log_printf (const char *fmt, ...)
{
  emit_indent ();

  va_list ap;
  va_start (ap, fmt);
  vfprintf (get_stream (), fmt, ap);
  va_end (ap);

  emit_newline ();
}

void
logger::log_bool_return (const char *function_name, bool retval)
{
  log_printf ("%s <- %s",
	      retval ? "true" : "false",
	      function_name);
}

/* Emit indentation to OUTFILE for the start of a log line.  */

void
logger::emit_indent () const
{
  fprintf (get_stream (), "%*s", get_indent (), "");
}

void
logger::emit_newline () const
{
  fputc ('\n', get_stream ());
}

} // namespace logging {
} // namespace diagnostics
