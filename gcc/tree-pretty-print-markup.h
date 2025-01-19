/* Copyright (C) 2024-2025 Free Software Foundation, Inc.
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

#ifndef GCC_TREE_PRETTY_PRINT_MARKUP_H
#define GCC_TREE_PRETTY_PRINT_MARKUP_H

#include "pretty-print-markup.h"
#include "diagnostic-highlight-colors.h"

namespace pp_markup {

/* Concrete subclass of pp_markup::element.
   Print a type in quotes with the given highlighting color.  */

class element_quoted_type : public element
{
public:
  element_quoted_type (tree type, const char *highlight_color)
  : m_type (type),
    m_highlight_color (highlight_color)
  {
  }

  void add_to_phase_2 (context &ctxt) override
  {
    ctxt.begin_quote ();
    ctxt.begin_highlight_color (m_highlight_color);

    print_type (ctxt);

    ctxt.end_highlight_color ();
    ctxt.end_quote ();
  }

  void print_type (context &ctxt);

private:
  tree m_type;
  const char *m_highlight_color;
};

/* Concrete subclass of pp_markup::element.
   Print a type in quotes highlighted as the "expected" type.  */

class element_expected_type : public element_quoted_type
{
public:
  element_expected_type (tree type)
  : element_quoted_type (type, highlight_colors::expected)
  {
  }
};

/* Concrete subclass of pp_markup::element.
   Print a type in quotes highlighted as the "actual" type.  */

class element_actual_type : public element_quoted_type
{
public:
  element_actual_type (tree type)
  : element_quoted_type (type, highlight_colors::actual)
  {
  }
};

} // namespace pp_markup

#endif /* GCC_TREE_PRETTY_PRINT_MARKUP_H */
