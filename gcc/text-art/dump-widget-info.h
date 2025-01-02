/* Support for creating dump widgets.
   Copyright (C) 2024-2025 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_TEXT_ART_DUMP_WIDGET_INFO_H
#define GCC_TEXT_ART_DUMP_WIDGET_INFO_H

namespace text_art {

/* A bundle of state for use by make_dump_widget implementations.
   The referenced objects are expected to outlive the widgets
   themselves.  */

struct dump_widget_info
{
  dump_widget_info (text_art::style_manager &sm,
		    const text_art::theme &theme,
		    text_art::style::id_t tree_style_id)
  : m_sm (sm),
    m_theme (theme),
    m_tree_style_id (tree_style_id)
  {
  }

  text_art::style::id_t get_tree_style_id () const
  {
    return m_tree_style_id;
  }

  text_art::style_manager &m_sm;
  const text_art::theme &m_theme;
  text_art::style::id_t m_tree_style_id;
};

} // namespace text_art

#endif /* GCC_TEXT_ART_DUMP_WIDGET_INFO_H */
