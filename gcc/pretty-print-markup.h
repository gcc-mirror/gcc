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

#ifndef GCC_PRETTY_PRINT_MARKUP_H
#define GCC_PRETTY_PRINT_MARKUP_H

#include "diagnostic-color.h"

class pp_token_list;

namespace pp_markup {

class context
{
public:
  context (pretty_printer &pp,
	   bool &quoted,
	   pp_token_list *formatted_token_list)
  : m_pp (pp),
    m_buf (*pp_buffer (&pp)),
    m_quoted (quoted),
    m_formatted_token_list (formatted_token_list)
  {
  }

  void begin_quote ();
  void end_quote ();

  void begin_highlight_color (const char *color_name);
  void end_highlight_color ();

  void push_back_any_text ();

  pretty_printer &m_pp;
  output_buffer &m_buf;
  bool &m_quoted;
  pp_token_list *m_formatted_token_list;
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

/* Concrete subclass: handle "%e" by printing a comma-separated list
   of quoted strings.  */

class comma_separated_quoted_strings : public element
{
public:
  comma_separated_quoted_strings (const auto_vec<const char *> &strings)
  : m_strings (strings)
  {
  }

  void add_to_phase_2 (context &ctxt) final override;

private:
  const auto_vec<const char *> &m_strings;
};

} // namespace pp_markup

class pp_element_quoted_string : public pp_element
{
public:
  pp_element_quoted_string (const char *text,
			    const char *highlight_color = nullptr)
  : m_text (text),
    m_highlight_color (highlight_color)
  {}

  void add_to_phase_2 (pp_markup::context &ctxt) final override
  {
    ctxt.begin_quote ();
    if (m_highlight_color)
      ctxt.begin_highlight_color (m_highlight_color);
    pp_string (&ctxt.m_pp, m_text);
    if (m_highlight_color)
      ctxt.end_highlight_color ();
    ctxt.end_quote ();
  }

private:
  const char *m_text;
  const char *m_highlight_color;
};

#endif /* GCC_PRETTY_PRINT_MARKUP_H */
