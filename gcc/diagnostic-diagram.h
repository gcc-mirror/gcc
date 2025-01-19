/* Support for diagrams within diagnostics.
   Copyright (C) 2023-2025 Free Software Foundation, Inc.
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

#ifndef GCC_DIAGNOSTIC_DIAGRAM_H
#define GCC_DIAGNOSTIC_DIAGRAM_H

namespace text_art
{
  class canvas;
} // namespace text_art

/* A text art diagram, along with an "alternative text" string
   describing it.  */

class diagnostic_diagram
{
 public:
  diagnostic_diagram (const text_art::canvas &canvas,
		      const char *alt_text)
  : m_canvas (canvas),
    m_alt_text (alt_text)
  {
    gcc_assert (alt_text);
  }

  const text_art::canvas &get_canvas () const { return m_canvas; }
  const char *get_alt_text () const { return m_alt_text; }

 private:
  const text_art::canvas &m_canvas;
  const char *const m_alt_text;
};

#endif /* ! GCC_DIAGNOSTIC_DIAGRAM_H */
