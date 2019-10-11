/* Copyright (C) 2019 Free Software Foundation, Inc.
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

#ifndef GCC_DIAGNOSTIC_URL_H
#define GCC_DIAGNOSTIC_URL_H

/* Whether to add URLs to diagnostics:
   - DIAGNOSTICS_URL_NO: never
   - DIAGNOSTICS_URL_YES: always
   - DIAGNOSTICS_URL_AUTO: depending on the output stream.  */
typedef enum
{
  DIAGNOSTICS_URL_NO       = 0,
  DIAGNOSTICS_URL_YES      = 1,
  DIAGNOSTICS_URL_AUTO     = 2
} diagnostic_url_rule_t;

extern bool diagnostic_urls_enabled_p (diagnostic_url_rule_t);

#endif /* ! GCC_DIAGNOSTIC_URL_H */
