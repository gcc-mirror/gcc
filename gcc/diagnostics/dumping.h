/* Utilities for implementing "dump" functions for the diagnostics subsystem.
   Copyright (C) 2025-2026 Free Software Foundation, Inc.
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

#ifndef GCC_DIAGNOSTICS_DUMP_H
#define GCC_DIAGNOSTICS_DUMP_H

namespace diagnostics {
namespace dumping {

extern void emit_indent (FILE *outfile, int indent);
extern void emit_heading (FILE *outfile, int indent,
			  const char *text);

extern void emit_string_field (FILE *outfile, int indent,
			       const char *label, const char *value);
extern void emit_bool_field (FILE *outfile, int indent,
				const char *label, bool value);
extern void emit_size_t_field (FILE *outfile, int indent,
			       const char *label, size_t value);
extern void emit_int_field (FILE *outfile, int indent,
			    const char *label, int value);
extern void emit_unsigned_field (FILE *outfile, int indent,
				 const char *label, unsigned value);

extern void emit_none (FILE *outfile, int indent);

#define DIAGNOSTICS_DUMPING_EMIT_BOOL_FIELD(FLAG) \
  dumping::emit_bool_field (outfile, indent, #FLAG, FLAG)

} // namespace dumping
} // namespace diagnostics

#endif /* ! GCC_DIAGNOSTICS_DUMP_H */
