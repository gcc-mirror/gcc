/* A class for referring to events within a diagnostics::paths::path.
   Copyright (C) 2019-2025 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>

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

#ifndef GCC_DIAGNOSTICS_EVENT_ID_H
#define GCC_DIAGNOSTICS_EVENT_ID_H

/* A class for referring to events within a diagnostics::paths::path.

   They are stored as 0-based offsets into the events, but
   printed (e.g. via %@) as 1-based numbers.

   For example, a 3-event path has event offsets 0, 1, and 2,
   which would be shown to the user as "(1)", "(2)" and "(3)".

   This has its own header so that pretty-print.cc can use this
   to implement "%@" without bringing in all of diagnostics::paths.

   This has to be in the global namespace for compatibility with
   c-format.cc in GCC 10 onwards.  */

class diagnostic_event_id_t
{
 public:
  diagnostic_event_id_t () : m_index (UNKNOWN_EVENT_IDX) {}
  diagnostic_event_id_t (int zero_based_idx) : m_index (zero_based_idx) {}

  bool known_p () const { return m_index != UNKNOWN_EVENT_IDX; }

  int zero_based () const
  {
    gcc_assert (known_p ());
    return m_index;
  }

  int one_based () const
  {
    gcc_assert (known_p ());
    return m_index + 1;
  }

 private:
  static const int UNKNOWN_EVENT_IDX = -1;
  int m_index; // zero-based
};

namespace diagnostics {
namespace paths {

typedef diagnostic_event_id_t event_id_t;

/* A type for compactly referring to a particular thread within a
   diagnostics::paths::path.  Typically there is just one thread per path,
   with id 0.  */
typedef int thread_id_t;

} // namespace paths
} // namespace diagnostics


/* A pointer to a diagnostic_event_id_t, for use with the "%@" format
   code, which will print a 1-based representation for it, with suitable
   colorization, e.g. "(1)".
   The %@ format code requires that known_p be true for the event ID. */
typedef diagnostic_event_id_t *diagnostic_event_id_ptr;

#endif /* ! GCC_DIAGNOSTICS_EVENT_ID_H */
