/* Declaration of struct diagnostics::option_id.
   Copyright (C) 2024-2026 Free Software Foundation, Inc.
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

#ifndef GCC_DIAGNOSTICS_OPTION_ID_H
#define GCC_DIAGNOSTICS_OPTION_ID_H

namespace diagnostics {

/* A class to use for the ID of an option that controls
   a particular diagnostic.
   This is just a wrapper around "int", but better documents
   the intent of the code.  */

struct option_id
{
  option_id () : m_idx (0) {}

  option_id (int idx) : m_idx (idx) {}
  /* Ideally we'd take an enum opt_code here, but we don't
     want to depend on its decl.  */

  bool operator== (option_id other) const
  {
    return m_idx == other.m_idx;
  }

  int m_idx;
};

} // namespace diagnostics

#endif /* ! GCC_DIAGNOSTICS_OPTION_ID_H */
