/* Hierarchical diagram elements.
   Copyright (C) 2023-2024 Free Software Foundation, Inc.
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
#include "text-art/widget.h"

using namespace text_art;

/* class text_art::widget.  */

canvas
widget::to_canvas (const style_manager &style_mgr)
{
  const canvas::size_t req_size = get_req_size ();

  /* For now we don't constrain the allocation; we give
     the widget the full size it requested, and widgets
     assume they got their full size request.  */
  const canvas::size_t alloc_size = req_size;

  set_alloc_rect (canvas::rect_t (canvas::coord_t (0, 0), alloc_size));
  canvas c (alloc_size, style_mgr);
  paint_to_canvas (c);
  return c;
}

/* class text_art::vbox_widget : public text_art::container_widget.  */

const char *
vbox_widget::get_desc () const
{
  return "vbox_widget";
}

canvas::size_t
vbox_widget::calc_req_size ()
{
  canvas::size_t result (0, 0);
  for (auto &child : m_children)
    {
      canvas::size_t child_req_size = child->get_req_size();
      result.h += child_req_size.h;
      result.w = std::max (result.w, child_req_size.w);
    }
  return result;
}

void
vbox_widget::update_child_alloc_rects ()
{
  const int x = get_min_x ();
  int y = get_min_y ();
  for (auto &child : m_children)
    {
      child->set_alloc_rect
	(canvas::rect_t (canvas::coord_t (x, y),
			 canvas::size_t (get_alloc_w (),
					 child->get_req_h ())));
      y += child->get_req_h ();
    }
}

/* class text_art::text_widget : public text_art::leaf_widget.  */

const char *
text_widget::get_desc () const
{
  return "text_widget";
}

canvas::size_t
text_widget::calc_req_size ()
{
  return canvas::size_t (m_str.size (), 1);
}

void
text_widget::paint_to_canvas (canvas &canvas)
{
  canvas.paint_text (get_top_left (), m_str);
}

/* class text_art::canvas_widget : public text_art::leaf_widget.  */

const char *
canvas_widget::get_desc () const
{
  return "canvas_widget";
}

canvas::size_t
canvas_widget::calc_req_size ()
{
  return m_canvas.get_size ();
}

void
canvas_widget::paint_to_canvas (canvas &canvas)
{
  for (int y = 0; y < m_canvas.get_size ().h; y++)
    for (int x = 0; x < m_canvas.get_size ().w; x++)
      {
	canvas::coord_t rel_xy (x, y);
	canvas.paint (get_top_left () + rel_xy,
		      m_canvas.get (rel_xy));
      }
}

#if CHECKING_P

namespace selftest {

/* Concrete widget subclass for writing selftests.
   Requests a hard-coded size, and fills its allocated rectangle
   with a specific character.  */

class test_widget : public leaf_widget
{
public:
  test_widget (canvas::size_t size, char ch)
  : m_test_size (size), m_ch (ch)
  {}

  const char *get_desc () const final override
  {
    return "test_widget";
  }
  canvas::size_t calc_req_size () final override
  {
    return m_test_size;
  }
  void paint_to_canvas (canvas &canvas) final override
  {
    canvas.fill (get_alloc_rect (), canvas::cell_t (m_ch));
  }

private:
  canvas::size_t m_test_size;
  char m_ch;
};

static void
test_test_widget ()
{
  style_manager sm;
  test_widget w (canvas::size_t (3, 3), 'A');
  canvas c (w.to_canvas (sm));
  ASSERT_CANVAS_STREQ
    (c, false,
     ("AAA\n"
      "AAA\n"
      "AAA\n"));
}

static void
test_text_widget ()
{
  style_manager sm;
  text_widget w (styled_string (sm, "hello world"));
  canvas c (w.to_canvas (sm));
  ASSERT_CANVAS_STREQ
    (c, false,
     ("hello world\n"));
}

static void
test_wrapper_widget ()
{
  style_manager sm;
  wrapper_widget w (::make_unique<test_widget> (canvas::size_t (3, 3), 'B'));
  canvas c (w.to_canvas (sm));
  ASSERT_CANVAS_STREQ
    (c, false,
     ("BBB\n"
      "BBB\n"
      "BBB\n"));
}

static void
test_vbox_1 ()
{
  style_manager sm;
  vbox_widget w;
  for (int i = 0; i < 5; i++)
    w.add_child
      (::make_unique <text_widget>
       (styled_string::from_fmt (sm, nullptr,
				 "this is line %i", i)));
  canvas c (w.to_canvas (sm));
  ASSERT_CANVAS_STREQ
    (c, false,
     ("this is line 0\n"
      "this is line 1\n"
      "this is line 2\n"
      "this is line 3\n"
      "this is line 4\n"));
}

static void
test_vbox_2 ()
{
  style_manager sm;
  vbox_widget w;
  w.add_child (::make_unique<test_widget> (canvas::size_t (1, 3), 'A'));
  w.add_child (::make_unique<test_widget> (canvas::size_t (4, 1), 'B'));
  w.add_child (::make_unique<test_widget> (canvas::size_t (1, 2), 'C'));
  canvas c (w.to_canvas (sm));
  ASSERT_CANVAS_STREQ
    (c, false,
     ("AAAA\n"
      "AAAA\n"
      "AAAA\n"
      "BBBB\n"
      "CCCC\n"
      "CCCC\n"));
}

static void
test_canvas_widget ()
{
  style_manager sm;
  canvas inner_canvas (canvas::size_t (5, 3), sm);
  inner_canvas.fill (canvas::rect_t (canvas::coord_t (0, 0),
				     canvas::size_t (5, 3)),
		     canvas::cell_t ('a'));
  canvas_widget cw (std::move (inner_canvas));
  canvas c (cw.to_canvas (sm));
  ASSERT_CANVAS_STREQ
    (c, false,
     ("aaaaa\n"
      "aaaaa\n"
      "aaaaa\n"));
}

/* Run all selftests in this file.  */

void
text_art_widget_cc_tests ()
{
  test_test_widget ();
  test_text_widget ();
  test_wrapper_widget ();
  test_vbox_1 ();
  test_vbox_2 ();
  test_canvas_widget ();
}

} // namespace selftest


#endif /* #if CHECKING_P */
