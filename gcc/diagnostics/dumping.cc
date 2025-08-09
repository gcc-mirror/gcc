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
#include "diagnostics/dumping.h"

namespace diagnostics {
namespace dumping {

/* Emit indentation to OUTFILE for the start of a dump line.  */

void
emit_indent (FILE *outfile, int indent)
{
  fprintf (outfile, "%*s", indent, "");
}

/* Emit an indented line to OUTFILE showing a heading.  */

void
emit_heading (FILE *outfile, int indent,
	      const char *text)
{
  emit_indent (outfile, indent);
  fprintf (outfile, "%s:\n", text);
}

/* Various functions that emit an indented line to OUTFILE
   showing "label: value".  */

void
emit_string_field (FILE *outfile, int indent,
		      const char *label, const char *value)
{
  emit_indent (outfile, indent);
  fprintf (outfile, "%s: %s\n", label, value);
}

void
emit_bool_field (FILE *outfile, int indent,
		 const char *label, bool value)
{
  emit_string_field (outfile, indent, label,
		     value ? "true" : "false");
}

void
emit_size_t_field (FILE *outfile, int indent,
		   const char *label, size_t value)
{
  emit_indent (outfile, indent);
  fprintf (outfile, "%s: " HOST_SIZE_T_PRINT_DEC "\n", label, value);
}

void
emit_int_field (FILE *outfile, int indent,
		const char *label, int value)
{
  emit_indent (outfile, indent);
  fprintf (outfile, "%s: %i\n", label, value);
}

void
emit_unsigned_field (FILE *outfile, int indent,
		     const char *label, unsigned value)
{
  emit_indent (outfile, indent);
  fprintf (outfile, "%s: %u\n", label, value);
}

/* Emit an indented line to OUTFILE reading "(none)".  */

void
emit_none (FILE *outfile, int indent)
{
  emit_indent (outfile, indent);
  fprintf (outfile, "(none)\n");
}


} // namespace dumping {
} // namespace diagnostics
