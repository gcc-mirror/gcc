/* Copyright (C) 2024 Free Software Foundation, Inc.
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

#ifndef GCC_PRETTY_PRINT_MARKUP_H
#define GCC_PRETTY_PRINT_MARKUP_H

#include "diagnostic-color.h"

namespace pp_markup {

class context
{
public:
  context (pretty_printer &pp,
	   output_buffer &buf,
	   unsigned chunk_idx,
	   bool &quoted,
	   const urlifier *urlifier)
  : m_pp (pp),
    m_buf (buf),
    m_chunk_idx (chunk_idx),
    m_quoted (quoted),
    m_urlifier (urlifier)
  {
  }

  void begin_quote ();
  void end_quote ();

  void begin_highlight_color (const char *color_name);
  void end_highlight_color ();

  pretty_printer &m_pp;
  output_buffer &m_buf;
  unsigned m_chunk_idx;
  bool &m_quoted;
  const urlifier *m_urlifier;
};

/* Abstract base class for use in pp_format for handling "%e".
   This can add arbitrary content to the buffer being constructed, and
   isolates the non-typesafe part of the formatting call in one place.  */

class element
{
public:
  virtual ~element () {}
  virtual void add_to_phase_2 (context &ctxt) = 0;

protected:
  element () {}

private:
  DISABLE_COPY_AND_ASSIGN (element);
};

} // namespace pp_markup

#endif /* GCC_PRETTY_PRINT_MARKUP_H */
