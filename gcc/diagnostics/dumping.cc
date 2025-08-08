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

/* Various specializattions that emit an indented line to OUTFILE
   showing "label: value".  */

template <>
void
emit_field<const char *> (FILE *outfile, int indent,
			  const char *label, const char *value)
{
  emit_indent (outfile, indent);
  fprintf (outfile, "%s: %s\n", label, value);
}

template <>
void
emit_field<char *> (FILE *outfile, int indent,
		    const char *label, char *value)
{
  emit_indent (outfile, indent);
  fprintf (outfile, "%s: %s\n", label, value);
}

template <>
void
emit_field<bool> (FILE *outfile, int indent,
		  const char *label, bool value)
{
  emit_field<const char *> (outfile, indent, label,
			    value ? "true" : "false");
}

template <>
void
emit_field<size_t> (FILE *outfile, int indent,
		    const char *label, size_t value)
{
  emit_indent (outfile, indent);
  fprintf (outfile, "%s: %zi\n", label, value);
}

template <>
void
emit_field<int> (FILE *outfile, int indent,
		 const char *label, int value)
{
  emit_indent (outfile, indent);
  fprintf (outfile, "%s: %i\n", label, value);
}

template <>
void
emit_field<unsigned> (FILE *outfile, int indent,
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
