/* Support for suggestions about missing #include directives.
   Copyright (C) 2017-2020 Free Software Foundation, Inc.

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

#ifndef GCC_KNOWN_HEADERS_H
#define GCC_KNOWN_HEADERS_H

extern const char *get_c_stdlib_header_for_name (const char *name);
extern const char *get_cp_stdlib_header_for_name (const char *name);

/* Subclass of deferred_diagnostic for suggesting to the user
   that they have missed a #include.  */

class suggest_missing_header : public deferred_diagnostic
{
 public:
  suggest_missing_header (location_t loc, const char *name,
			  const char *header_hint);
  ~suggest_missing_header ();

 private:
  const char *m_name_str;
  const char *m_header_hint;
};

#endif /* GCC_KNOWN_HEADERS_H */
