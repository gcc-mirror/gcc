/* Tree diagrams.
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

#ifndef GCC_TEXT_ART_TREE_WIDGET_H
#define GCC_TEXT_ART_TREE_WIDGET_H

#include "text-art/canvas.h"
#include "text-art/widget.h"

namespace text_art {

class dump_widget_info;

class tree_widget : public widget
{
public:
  tree_widget (std::unique_ptr<widget> node,
	       const theme &theme,
	       style::id_t style_id)
  : m_node (std::move (node)),
    m_theme (theme),
    m_style_id (style_id)
  {
  }

  static std::unique_ptr<tree_widget>
  make (styled_string str, const theme &theme, style::id_t style_id);

  static std::unique_ptr<tree_widget>
  make (const dump_widget_info &dwi, pretty_printer *pp);

  static std::unique_ptr<tree_widget>
  make (const dump_widget_info &dwi, const char *str);

  static std::unique_ptr<tree_widget>
  from_fmt (const dump_widget_info &dwi,
	    printer_fn format_decoder,
	    const char *fmt, ...)
    ATTRIBUTE_GCC_PPDIAG(3, 4);

  const char *get_desc () const override;
  canvas::size_t calc_req_size () final override;
  void update_child_alloc_rects () final override;
  void paint_to_canvas (canvas &canvas) final override;

  void add_child (std::unique_ptr<widget> child)
  {
    if (child)
      m_children.push_back (std::move (child));
  }

  size_t get_num_children () const
  {
    return m_children.size ();
  }

private:
  std::unique_ptr<widget> m_node;
  std::vector<std::unique_ptr<widget>> m_children;
  const theme &m_theme;
  style::id_t m_style_id;
};

} // namespace text_art

#endif /* GCC_TEXT_ART_TREE_WIDGET_H */
