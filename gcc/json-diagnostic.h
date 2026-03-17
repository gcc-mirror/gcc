/* Diagnostics relating to JSON values.
   Copyright (C) 2026 Free Software Foundation, Inc.
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

#ifndef GCC_JSON_DIAGNOSTIC_H
#define GCC_JSON_DIAGNOSTIC_H

#include "json-parsing.h"

/* Implementation of json::location_map for use with
   GCC diagnostics.
   Stores location information for json::value * from parsing, and
   can generate location_t values for the.  */

class gcc_json_context : public json::simple_location_map
{
public:
  gcc_json_context (const char *filename)
  : m_filename (filename)
  {
  }

  location_t
  make_location_for_point (const json::location_map::point &);

  location_t
  make_location_for_range (const json::location_map::range &);

private:
  const char *m_filename;
};

/* Emit an error on gcc's global_dc relating to JS_VAL.  */

extern void
json_error (gcc_json_context &ctxt,
	    const json::value &js_val,
	    const char *gmsgid, ...)
  ATTRIBUTE_GCC_DIAG(3,4);

/* Emit a warning on gcc's global_dc relating to JS_VAL.  */

extern bool
json_warning (gcc_json_context &ctxt,
	      const json::value &js_val,
	      diagnostics::option_id option_id,
	      const char *gmsgid, ...)
  ATTRIBUTE_GCC_DIAG(4,5);

/* Emit a note on gcc's global_dc relating to JS_VAL.  */

extern void
json_note (gcc_json_context &ctxt,
	   const json::value &js_val,
	   const char *gmsgid, ...)
  ATTRIBUTE_GCC_DIAG(3,4);

#endif  /* GCC_JSON_DIAGNOSTIC_H  */
