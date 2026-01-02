/* Counts of per-kind diagnostics.
   Copyright (C) 2000-2026 Free Software Foundation, Inc.

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

#ifndef GCC_DIAGNOSTICS_COUNTERS_H
#define GCC_DIAGNOSTICS_COUNTERS_H

#include "diagnostics/kinds.h"

namespace diagnostics {

/* A collection of counters of diagnostics, per-kind
   (e.g. "3 errors and 1 warning"), for use by both diagnostics::context
   and by diagnostics::buffer.  */

struct counters
{
  counters ();

  void dump (FILE *out, int indent) const;
  void DEBUG_FUNCTION dump () const { dump (stderr, 0); }

  int get_count (enum kind kind) const
  {
    return m_count_for_kind[static_cast<size_t> (kind)];
  }

  void move_to (counters &dest);
  void clear ();

  int m_count_for_kind[static_cast<size_t> (kind::last_diagnostic_kind)];
};

} // namespace diagnostics

#endif /* ! GCC_DIAGNOSTICS_COUNTERS_H */
