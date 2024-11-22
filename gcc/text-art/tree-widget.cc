/* Tree diagrams.
   Copyright (C) 2024 Free Software Foundation, Inc.
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

#include "config.h"
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "pretty-print.h"
#include "selftest.h"
#include "make-unique.h"
#include "text-art/selftests.h"
#include "text-art/tree-widget.h"
#include "text-art/dump-widget-info.h"

using namespace text_art;

/* class text_art::tree_widget : public text_art::widget.  */

static const int margin_width = 3;

std::unique_ptr<tree_widget>
tree_widget::make (styled_string str, const theme &theme, style::id_t style_id)
{
  return ::make_unique <tree_widget>
    (::make_unique <text_widget> (std::move (str)),
     theme,
     style_id);
}

std::unique_ptr<tree_widget>
tree_widget::make (const dump_widget_info &dwi, pretty_printer *pp)
{
  return tree_widget::make (styled_string (dwi.m_sm, pp_formatted_text (pp)),
			    dwi.m_theme,
			    dwi.get_tree_style_id ());
}

std::unique_ptr<tree_widget>
tree_widget::make (const dump_widget_info &dwi, const char *str)
{
  return tree_widget::make (styled_string (dwi.m_sm, str),
			    dwi.m_theme,
			    dwi.get_tree_style_id ());
}

std::unique_ptr<tree_widget>
tree_widget::from_fmt (const dump_widget_info &dwi,
		       printer_fn format_decoder,
		       const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  styled_string styled_str
    (styled_string::from_fmt_va (dwi.m_sm, format_decoder, fmt, &ap));
  va_end (ap);
  return make (std::move (styled_str), dwi.m_theme, dwi.get_tree_style_id ());
}

const char *
tree_widget::get_desc () const
{
  return "tree_widget";
}

canvas::size_t
tree_widget::calc_req_size ()
{
  canvas::size_t result (0, 0);
  if (m_node)
    {
      canvas::size_t node_req_size = m_node->get_req_size ();
      result.h += node_req_size.h;
      result.w = std::max (result.w, node_req_size.w);
    }
  for (auto &child : m_children)
    {
      canvas::size_t child_req_size = child->get_req_size ();
      result.h += child_req_size.h;
      result.w = std::max (result.w, child_req_size.w + margin_width);
    }
  return result;
}

void
tree_widget::update_child_alloc_rects ()
{
  const int x = get_min_x ();
  int y = get_min_y ();
  if (m_node)
    {
      m_node->set_alloc_rect
	(canvas::rect_t (canvas::coord_t (x, y),
			 canvas::size_t (get_alloc_w (),
					 m_node->get_req_h ())));
      y += m_node->get_req_h ();
    }
  for (auto &child : m_children)
    {
      child->set_alloc_rect
	(canvas::rect_t (canvas::coord_t (x + margin_width, y),
			 canvas::size_t (get_alloc_w () - margin_width,
					 child->get_req_h ())));
      y += child->get_req_h ();
    }
}

void
tree_widget::paint_to_canvas (canvas &canvas)
{
  if (m_node)
    m_node->paint_to_canvas (canvas);
  const int min_x = get_min_x ();
  const canvas::cell_t cell_child_non_final
    (m_theme.get_cell (theme::cell_kind::TREE_CHILD_NON_FINAL, m_style_id));
  const canvas::cell_t cell_child_final
    (m_theme.get_cell (theme::cell_kind::TREE_CHILD_FINAL, m_style_id));
  const canvas::cell_t cell_x_connector
    (m_theme.get_cell (theme::cell_kind::TREE_X_CONNECTOR, m_style_id));
  const canvas::cell_t cell_y_connector
    (m_theme.get_cell (theme::cell_kind::TREE_Y_CONNECTOR, m_style_id));
  size_t idx = 0;
  for (auto &child : m_children)
    {
      child->paint_to_canvas (canvas);

      const bool last_child = (++idx == m_children.size ());
      canvas.paint (canvas::coord_t (min_x + 1, child->get_min_y ()),
		    cell_x_connector);
      canvas.paint (canvas::coord_t (min_x, child->get_min_y ()),
		    last_child ? cell_child_final : cell_child_non_final);
      if (!last_child)
	for (int y = child->get_min_y () + 1; y <= child ->get_max_y (); y++)
	  canvas.paint (canvas::coord_t (min_x, y), cell_y_connector);
    }
}

#if CHECKING_P

namespace selftest {

static std::unique_ptr<tree_widget>
make_test_tree_widget (const dump_widget_info &dwi)
{
  std::unique_ptr<tree_widget> w
    (tree_widget::from_fmt (dwi, nullptr, "Root"));
  for (int i = 0; i < 3; i++)
    {
      std::unique_ptr<tree_widget> c
	(tree_widget::from_fmt (dwi, nullptr, "Child %i", i));
      for (int j = 0; j < 3; j++)
	c->add_child (tree_widget::from_fmt (dwi, nullptr,
					     "Grandchild %i %i", i, j));
      w->add_child (std::move (c));
    }
  return w;
}

static void
test_tree_widget ()
{
  style_manager sm;

  style::id_t default_style_id (sm.get_or_create_id (style ()));

  {
    ascii_theme theme;
    dump_widget_info dwi (sm, theme, default_style_id);
    canvas c (make_test_tree_widget (dwi)->to_canvas (sm));
    ASSERT_CANVAS_STREQ
      (c, false,
       ("Root\n"
	"+- Child 0\n"
	"|  +- Grandchild 0 0\n"
	"|  +- Grandchild 0 1\n"
	"|  `- Grandchild 0 2\n"
	"+- Child 1\n"
	"|  +- Grandchild 1 0\n"
	"|  +- Grandchild 1 1\n"
	"|  `- Grandchild 1 2\n"
	"`- Child 2\n"
	"   +- Grandchild 2 0\n"
	"   +- Grandchild 2 1\n"
	"   `- Grandchild 2 2\n"));
  }

  {
    unicode_theme theme;
    dump_widget_info dwi (sm, theme, default_style_id);
    canvas c (make_test_tree_widget (dwi)->to_canvas (sm));
    ASSERT_CANVAS_STREQ
      (c, false,
       ("Root\n"
	"├─ Child 0\n"
	"│  ├─ Grandchild 0 0\n"
	"│  ├─ Grandchild 0 1\n"
	"│  ╰─ Grandchild 0 2\n"
	"├─ Child 1\n"
	"│  ├─ Grandchild 1 0\n"
	"│  ├─ Grandchild 1 1\n"
	"│  ╰─ Grandchild 1 2\n"
	"╰─ Child 2\n"
	"   ├─ Grandchild 2 0\n"
	"   ├─ Grandchild 2 1\n"
	"   ╰─ Grandchild 2 2\n"));
  }
}

/* Run all selftests in this file.  */

void
text_art_tree_widget_cc_tests ()
{
  test_tree_widget ();
}

} // namespace selftest


#endif /* #if CHECKING_P */
