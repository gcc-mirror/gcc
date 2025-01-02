/* Classes for printing labelled rulers.
   Copyright (C) 2023-2025 Free Software Foundation, Inc.
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
#define INCLUDE_ALGORITHM
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "pretty-print.h"
#include "selftest.h"
#include "text-art/selftests.h"
#include "text-art/ruler.h"
#include "text-art/theme.h"

using namespace text_art;

void
x_ruler::add_label (const canvas::range_t &r,
		    styled_string text,
		    style::id_t style_id,
		    label_kind kind)
{
  m_labels.push_back (label (r, std::move (text), style_id, kind));
  m_has_layout = false;
}

int
x_ruler::get_canvas_y (int rel_y) const
{
  gcc_assert (rel_y >= 0);
  gcc_assert (rel_y < m_size.h);
  switch (m_label_dir)
    {
    default:
      gcc_unreachable ();
    case label_dir::ABOVE:
      return m_size.h - (rel_y + 1);
    case label_dir::BELOW:
      return rel_y;
    }
}

void
x_ruler::paint_to_canvas (canvas &canvas,
			  canvas::coord_t offset,
			  const theme &theme)
{
  ensure_layout ();

  if (0)
    canvas.fill (canvas::rect_t (offset, m_size),
		 canvas::cell_t ('*'));

  for (size_t idx = 0; idx < m_labels.size (); idx++)
    {
      const label &iter_label = m_labels[idx];

      /* Paint the ruler itself.  */
      const int ruler_rel_y = get_canvas_y (0);
      for (int rel_x = iter_label.m_range.start;
	   rel_x < iter_label.m_range.next;
	   rel_x++)
	{
	  enum theme::cell_kind kind = theme::cell_kind::X_RULER_MIDDLE;

	  if (rel_x == iter_label.m_range.start)
	    {
	      kind = theme::cell_kind::X_RULER_LEFT_EDGE;
	      if (idx > 0)
		{
		  const label &prev_label = m_labels[idx - 1];
		  if (prev_label.m_range.get_max () == iter_label.m_range.start)
		    kind = theme::cell_kind::X_RULER_INTERNAL_EDGE;
		}
	    }
	  else if (rel_x == iter_label.m_range.get_max ())
	    kind = theme::cell_kind::X_RULER_RIGHT_EDGE;
	  else if (rel_x == iter_label.m_connector_x)
	    {
	      switch (m_label_dir)
		{
		default:
		  gcc_unreachable ();
		case label_dir::ABOVE:
		  kind = theme::cell_kind::X_RULER_CONNECTOR_TO_LABEL_ABOVE;
		  break;
		case label_dir::BELOW:
		  kind = theme::cell_kind::X_RULER_CONNECTOR_TO_LABEL_BELOW;
		  break;
		}
	    }
	  canvas.paint (canvas::coord_t (rel_x, ruler_rel_y) + offset,
			theme.get_cell (kind, iter_label.m_style_id));
	}

      /* Paint the connector to the text.  */
      for (int connector_rel_y = 1;
	   connector_rel_y < iter_label.m_text_rect.get_min_y ();
	   connector_rel_y++)
	{
	  canvas.paint
	    ((canvas::coord_t (iter_label.m_connector_x,
			       get_canvas_y (connector_rel_y))
	      + offset),
	     theme.get_cell (theme::cell_kind::X_RULER_VERTICAL_CONNECTOR,
			     iter_label.m_style_id));
	}

      /* Paint the text.  */
      switch (iter_label.m_kind)
	{
	default:
	  gcc_unreachable ();
	case x_ruler::label_kind::TEXT:
	  canvas.paint_text
	    ((canvas::coord_t (iter_label.m_text_rect.get_min_x (),
			       get_canvas_y (iter_label.m_text_rect.get_min_y ()))
	      + offset),
	     iter_label.m_text);
	  break;

	case x_ruler::label_kind::TEXT_WITH_BORDER:
	  {
	    const canvas::range_t rel_x_range
	      (iter_label.m_text_rect.get_x_range ());

	    enum theme::cell_kind inner_left_kind;
	    enum theme::cell_kind inner_connector_kind;
	    enum theme::cell_kind inner_right_kind;
	    enum theme::cell_kind outer_left_kind;
	    enum theme::cell_kind outer_right_kind;

	      switch (m_label_dir)
		{
		default:
		  gcc_unreachable ();
		case label_dir::ABOVE:
		  outer_left_kind = theme::cell_kind::TEXT_BORDER_TOP_LEFT;
		  outer_right_kind = theme::cell_kind::TEXT_BORDER_TOP_RIGHT;
		  inner_left_kind = theme::cell_kind::TEXT_BORDER_BOTTOM_LEFT;
		  inner_connector_kind = theme::cell_kind::X_RULER_CONNECTOR_TO_LABEL_BELOW;
		  inner_right_kind = theme::cell_kind::TEXT_BORDER_BOTTOM_RIGHT;
		  break;
		case label_dir::BELOW:
		  inner_left_kind = theme::cell_kind::TEXT_BORDER_TOP_LEFT;
		  inner_connector_kind = theme::cell_kind::X_RULER_CONNECTOR_TO_LABEL_ABOVE;
		  inner_right_kind = theme::cell_kind::TEXT_BORDER_TOP_RIGHT;
		  outer_left_kind = theme::cell_kind::TEXT_BORDER_BOTTOM_LEFT;
		  outer_right_kind = theme::cell_kind::TEXT_BORDER_BOTTOM_RIGHT;
		  break;
		}
	    /* Inner border.  */
	    {
	      const int rel_canvas_y
		= get_canvas_y (iter_label.m_text_rect.get_min_y ());
	      /* Left corner.  */
	      canvas.paint ((canvas::coord_t (rel_x_range.get_min (),
					      rel_canvas_y)
			     + offset),
			    theme.get_cell (inner_left_kind,
					    iter_label.m_style_id));
	      /* Edge.  */
	      const canvas::cell_t edge_border_cell
		= theme.get_cell (theme::cell_kind::TEXT_BORDER_HORIZONTAL,
				  iter_label.m_style_id);
	      const canvas::cell_t connector_border_cell
		= theme.get_cell (inner_connector_kind,
				  iter_label.m_style_id);
	      for (int rel_x = rel_x_range.get_min () + 1;
		   rel_x < rel_x_range.get_max ();
		   rel_x++)
		if (rel_x == iter_label.m_connector_x)
		  canvas.paint ((canvas::coord_t (rel_x, rel_canvas_y)
				 + offset),
				connector_border_cell);
		else
		  canvas.paint ((canvas::coord_t (rel_x, rel_canvas_y)
				 + offset),
				edge_border_cell);

	      /* Right corner.  */
	      canvas.paint ((canvas::coord_t (rel_x_range.get_max (),
					      rel_canvas_y)
			     + offset),
			    theme.get_cell (inner_right_kind,
					    iter_label.m_style_id));
	    }

	    {
	      const int rel_canvas_y
		= get_canvas_y (iter_label.m_text_rect.get_min_y () + 1);
	      const canvas::cell_t border_cell
		= theme.get_cell (theme::cell_kind::TEXT_BORDER_VERTICAL,
				  iter_label.m_style_id);

	      /* Left border.  */
	      canvas.paint ((canvas::coord_t (rel_x_range.get_min (),
					      rel_canvas_y)
			     + offset),
			    border_cell);
	      /* Text.  */
	      canvas.paint_text ((canvas::coord_t (rel_x_range.get_min () + 1,
						   rel_canvas_y)
				  + offset),
				 iter_label.m_text);
	      /* Right border.  */
	      canvas.paint ((canvas::coord_t (rel_x_range.get_max (),
					      rel_canvas_y)
			     + offset),
			    border_cell);
	    }

	    /* Outer border.  */
	    {
	      const int rel_canvas_y
		= get_canvas_y (iter_label.m_text_rect.get_max_y ());
	      /* Left corner.  */
	      canvas.paint ((canvas::coord_t (rel_x_range.get_min (),
					      rel_canvas_y)
			     + offset),
			    theme.get_cell (outer_left_kind,
					    iter_label.m_style_id));
	      /* Edge.  */
	      const canvas::cell_t border_cell
		= theme.get_cell (theme::cell_kind::TEXT_BORDER_HORIZONTAL,
				  iter_label.m_style_id);
	      for (int rel_x = rel_x_range.get_min () + 1;
		   rel_x < rel_x_range.get_max ();
		   rel_x++)
		canvas.paint ((canvas::coord_t (rel_x, rel_canvas_y)
			       + offset),
			      border_cell);

	      /* Right corner.  */
	      canvas.paint ((canvas::coord_t (rel_x_range.get_max (),
					      rel_canvas_y)
			     + offset),
			    theme.get_cell (outer_right_kind,
					    iter_label.m_style_id));
	    }
	  }
	  break;
	}
    }
}

DEBUG_FUNCTION void
x_ruler::debug (const style_manager &sm)
{
  canvas c (get_size (), sm);
  paint_to_canvas (c, canvas::coord_t (0, 0), unicode_theme ());
  c.debug (true);
}

x_ruler::label::label (const canvas::range_t &range,
		       styled_string text,
		       style::id_t style_id,
		       label_kind kind)
: m_range (range),
  m_text (std::move (text)),
  m_style_id (style_id),
  m_kind (kind),
  m_text_rect (canvas::coord_t (0, 0),
	       canvas::size_t (m_text.calc_canvas_width (), 1)),
  m_connector_x ((m_range.get_min () + m_range.get_max ()) / 2)
{
  if (kind == label_kind::TEXT_WITH_BORDER)
    {
      m_text_rect.m_size.w += 2;
      m_text_rect.m_size.h += 2;
    }
}

bool
x_ruler::label::operator< (const label &other) const
{
  int cmp = m_range.start - other.m_range.start;
  if (cmp)
    return cmp < 0;
  return m_range.next < other.m_range.next;
}

void
x_ruler::ensure_layout ()
{
  if (m_has_layout)
    return;
  update_layout ();
  m_has_layout = true;
}

void
x_ruler::update_layout ()
{
  if (m_labels.empty ())
    return;

  std::sort (m_labels.begin (), m_labels.end ());

  /* Place labels.  */
  int ruler_width = m_labels.back ().m_range.get_next ();
  int width_with_labels = ruler_width;

  /* Get x coordinates of text parts of each label
     (m_text_rect.m_top_left.x for each label).  */
  for (size_t idx = 0; idx < m_labels.size (); idx++)
    {
      label &iter_label = m_labels[idx];
      /* Attempt to center the text label.  */
      int min_x;
      if (idx > 0)
	{
	  /* ...but don't overlap with the connector to the left.  */
	  int left_neighbor_connector_x = m_labels[idx - 1].m_connector_x;
	  min_x = left_neighbor_connector_x + 1;
	}
      else
	{
	  /* ...or go beyond the leftmost column.  */
	  min_x = 0;
	}
      int connector_x = iter_label.m_connector_x;
      int centered_x
	= connector_x - ((int)iter_label.m_text_rect.get_width () / 2);
      int text_x = std::max (min_x, centered_x);
      iter_label.m_text_rect.m_top_left.x = text_x;
    }

  /* Now walk backwards trying to place them vertically,
     setting m_text_rect.m_top_left.y for each label,
     consolidating the rows where possible.
     The y cooordinates are stored with respect to label_dir::BELOW.  */
  int label_y = 2;
  for (int idx = m_labels.size () - 1; idx >= 0; idx--)
    {
      label &iter_label = m_labels[idx];
      /* Does it fit on the same row as the text label to the right?  */
      size_t text_len = iter_label.m_text_rect.get_width ();
      /* Get the x-coord of immediately beyond iter_label's text.  */
      int next_x = iter_label.m_text_rect.get_min_x () + text_len;
      if (idx < (int)m_labels.size () - 1)
	{
	  if (next_x >= m_labels[idx + 1].m_text_rect.get_min_x ())
	    {
	      /* If not, start a new row.  */
	      label_y += m_labels[idx + 1].m_text_rect.get_height ();
	    }
	}
      iter_label.m_text_rect.m_top_left.y = label_y;
      width_with_labels = std::max (width_with_labels, next_x);
    }

  m_size = canvas::size_t (width_with_labels,
			   label_y + m_labels[0].m_text_rect.get_height ());
}

#if CHECKING_P

namespace selftest {

static void
assert_x_ruler_streq (const location &loc,
		      x_ruler &ruler,
		      const theme &theme,
		      const style_manager &sm,
		      bool styled,
		      const char *expected_str)
{
  canvas c (ruler.get_size (), sm);
  ruler.paint_to_canvas (c, canvas::coord_t (0, 0), theme);
  if (0)
    c.debug (styled);
  assert_canvas_streq (loc, c, styled, expected_str);
}

#define ASSERT_X_RULER_STREQ(RULER, THEME, SM, STYLED, EXPECTED_STR)	\
  SELFTEST_BEGIN_STMT							\
    assert_x_ruler_streq ((SELFTEST_LOCATION),				\
			  (RULER),					\
			  (THEME),					\
			  (SM),						\
			  (STYLED),					\
			  (EXPECTED_STR));				\
  SELFTEST_END_STMT

static void
test_single ()
{
  style_manager sm;
  x_ruler r (x_ruler::label_dir::BELOW);
  r.add_label (canvas::range_t (0, 11), styled_string (sm, "foo"),
	       style::id_plain, x_ruler::label_kind::TEXT);
  ASSERT_X_RULER_STREQ
    (r, ascii_theme (), sm, true,
     ("|~~~~+~~~~|\n"
      "     |\n"
      "    foo\n"));
  ASSERT_X_RULER_STREQ
    (r, unicode_theme (), sm, true,
     ("├────┬────┤\n"
      "     │\n"
      "    foo\n"));
}

static void
test_single_above ()
{
  style_manager sm;
  x_ruler r (x_ruler::label_dir::ABOVE);
  r.add_label (canvas::range_t (0, 11), styled_string (sm, "hello world"),
	       style::id_plain);
  ASSERT_X_RULER_STREQ
    (r, ascii_theme (), sm, true,
     ("hello world\n"
      "     |\n"
      "|~~~~+~~~~|\n"));
  ASSERT_X_RULER_STREQ
    (r, unicode_theme (), sm, true,
     ("hello world\n"
      "     │\n"
      "├────┴────┤\n"));
}

static void
test_multiple_contiguous ()
{
  style_manager sm;
  x_ruler r (x_ruler::label_dir::BELOW);
  r.add_label (canvas::range_t (0, 11), styled_string (sm, "foo"),
	       style::id_plain);
  r.add_label (canvas::range_t (10, 16), styled_string (sm, "bar"),
	       style::id_plain);
  ASSERT_X_RULER_STREQ
    (r, ascii_theme (), sm, true,
     ("|~~~~+~~~~|~+~~|\n"
      "     |      |\n"
      "    foo    bar\n"));
  ASSERT_X_RULER_STREQ
    (r, unicode_theme (), sm, true,
     ("├────┬────┼─┬──┤\n"
      "     │      │\n"
      "    foo    bar\n"));
}

static void
test_multiple_contiguous_above ()
{
  style_manager sm;
  x_ruler r (x_ruler::label_dir::ABOVE);
  r.add_label (canvas::range_t (0, 11), styled_string (sm, "foo"),
	       style::id_plain);
  r.add_label (canvas::range_t (10, 16), styled_string (sm, "bar"),
	       style::id_plain);
  ASSERT_X_RULER_STREQ
    (r, ascii_theme (), sm, true,
     ("    foo    bar\n"
      "     |      |\n"
      "|~~~~+~~~~|~+~~|\n"));
  ASSERT_X_RULER_STREQ
    (r, unicode_theme (), sm, true,
     ("    foo    bar\n"
      "     │      │\n"
      "├────┴────┼─┴──┤\n"));
}

static void
test_multiple_contiguous_abutting_labels ()
{
  style_manager sm;
  x_ruler r (x_ruler::label_dir::BELOW);
  r.add_label (canvas::range_t (0, 11), styled_string (sm, "12345678"),
	       style::id_plain);
  r.add_label (canvas::range_t (10, 16), styled_string (sm, "1234678"),
	       style::id_plain);
  ASSERT_X_RULER_STREQ
    (r, unicode_theme (), sm, true,
     ("├────┬────┼─┬──┤\n"
      "     │      │\n"
      "     │   1234678\n"
      " 12345678\n"));
}

static void
test_multiple_contiguous_overlapping_labels ()
{
  style_manager sm;
  x_ruler r (x_ruler::label_dir::BELOW);
  r.add_label (canvas::range_t (0, 11), styled_string (sm, "123456789"),
	       style::id_plain);
  r.add_label (canvas::range_t (10, 16), styled_string (sm, "12346789"),
	       style::id_plain);
  ASSERT_X_RULER_STREQ
    (r, unicode_theme (), sm, true,
     ("├────┬────┼─┬──┤\n"
      "     │      │\n"
      "     │  12346789\n"
      " 123456789\n"));
}
static void
test_abutting_left_border ()
{
  style_manager sm;
  x_ruler r (x_ruler::label_dir::BELOW);
  r.add_label (canvas::range_t (0, 6),
	       styled_string (sm, "this is a long label"),
	       style::id_plain);
  ASSERT_X_RULER_STREQ
    (r, unicode_theme (), sm, true,
     ("├─┬──┤\n"
      "  │\n"
      "this is a long label\n"));
}

static void
test_too_long_to_consolidate_vertically ()
{
  style_manager sm;
  x_ruler r (x_ruler::label_dir::BELOW);
  r.add_label (canvas::range_t (0, 11),
	       styled_string (sm, "long string A"),
	       style::id_plain);
  r.add_label (canvas::range_t (10, 16),
	       styled_string (sm, "long string B"),
	       style::id_plain);
  ASSERT_X_RULER_STREQ
    (r, unicode_theme (), sm, true,
     ("├────┬────┼─┬──┤\n"
      "     │      │\n"
      "     │long string B\n"
      "long string A\n"));
}

static void
test_abutting_neighbor ()
{
  style_manager sm;
  x_ruler r (x_ruler::label_dir::BELOW);
  r.add_label (canvas::range_t (0, 11),
	       styled_string (sm, "very long string A"),
	       style::id_plain);
  r.add_label (canvas::range_t (10, 16),
	       styled_string (sm, "very long string B"),
	       style::id_plain);
  ASSERT_X_RULER_STREQ
    (r, unicode_theme (), sm, true,
     ("├────┬────┼─┬──┤\n"
      "     │      │\n"
      "     │very long string B\n"
      "very long string A\n"));
}

static void
test_gaps ()
{
  style_manager sm;
  x_ruler r (x_ruler::label_dir::BELOW);
  r.add_label (canvas::range_t (0, 5),
	       styled_string (sm, "foo"),
	       style::id_plain);
  r.add_label (canvas::range_t (10, 15),
	       styled_string (sm, "bar"),
	       style::id_plain);
  ASSERT_X_RULER_STREQ
    (r, ascii_theme (), sm, true,
     ("|~+~|     |~+~|\n"
      "  |         |\n"
      " foo       bar\n"));
}

static void
test_styled ()
{
  style_manager sm;
  style s1, s2;
  s1.m_bold = true;
  s1.m_fg_color = style::named_color::YELLOW;
  s2.m_bold = true;
  s2.m_fg_color = style::named_color::BLUE;
  style::id_t sid1 = sm.get_or_create_id (s1);
  style::id_t sid2 = sm.get_or_create_id (s2);

  x_ruler r (x_ruler::label_dir::BELOW);
  r.add_label (canvas::range_t (0, 5), styled_string (sm, "foo"), sid1);
  r.add_label (canvas::range_t (10, 15), styled_string (sm, "bar"), sid2);
  ASSERT_X_RULER_STREQ
    (r, ascii_theme (), sm, true,
     ("[00;01;33m[K|~+~|[00m[K     [00;01;34m[K|~+~|[00m[K\n"
      "  [00;01;33m[K|[00m[K         [00;01;34m[K|[00m[K\n"
      " foo       bar\n"));
}

static void
test_borders ()
{
  style_manager sm;
  {
    x_ruler r (x_ruler::label_dir::BELOW);
    r.add_label (canvas::range_t (0, 5),
		 styled_string (sm, "label 1"),
		 style::id_plain,
		 x_ruler::label_kind::TEXT_WITH_BORDER);
    r.add_label (canvas::range_t (10, 15),
		 styled_string (sm, "label 2"),
		 style::id_plain);
    r.add_label (canvas::range_t (20, 25),
		 styled_string (sm, "label 3"),
		 style::id_plain,
		 x_ruler::label_kind::TEXT_WITH_BORDER);
    ASSERT_X_RULER_STREQ
      (r, ascii_theme (), sm, true,
       "|~+~|     |~+~|     |~+~|\n"
       "  |         |         |\n"
       "  |      label 2  +---+---+\n"
       "+-+-----+         |label 3|\n"
       "|label 1|         +-------+\n"
       "+-------+\n");
    ASSERT_X_RULER_STREQ
      (r, unicode_theme (), sm, true,
       "├─┬─┤     ├─┬─┤     ├─┬─┤\n"
       "  │         │         │\n"
       "  │      label 2  ╭───┴───╮\n"
       "╭─┴─────╮         │label 3│\n"
       "│label 1│         ╰───────╯\n"
       "╰───────╯\n");
  }
  {
    x_ruler r (x_ruler::label_dir::ABOVE);
    r.add_label (canvas::range_t (0, 5),
		 styled_string (sm, "label 1"),
		 style::id_plain,
		 x_ruler::label_kind::TEXT_WITH_BORDER);
    r.add_label (canvas::range_t (10, 15),
		 styled_string (sm, "label 2"),
		 style::id_plain);
    r.add_label (canvas::range_t (20, 25),
		 styled_string (sm, "label 3"),
		 style::id_plain,
		 x_ruler::label_kind::TEXT_WITH_BORDER);
    ASSERT_X_RULER_STREQ
      (r, ascii_theme (), sm, true,
       "+-------+\n"
       "|label 1|         +-------+\n"
       "+-+-----+         |label 3|\n"
       "  |      label 2  +---+---+\n"
       "  |         |         |\n"
       "|~+~|     |~+~|     |~+~|\n");
    ASSERT_X_RULER_STREQ
      (r, unicode_theme (), sm, true,
       "╭───────╮\n"
       "│label 1│         ╭───────╮\n"
       "╰─┬─────╯         │label 3│\n"
       "  │      label 2  ╰───┬───╯\n"
       "  │         │         │\n"
       "├─┴─┤     ├─┴─┤     ├─┴─┤\n");
  }
}

static void
test_emoji ()
{
  style_manager sm;

  styled_string s;
  s.append (styled_string (0x26A0, /* U+26A0 WARNING SIGN.  */
			   true));
  s.append (styled_string (sm, "  "));
  s.append (styled_string (sm, "this is a warning"));

  x_ruler r (x_ruler::label_dir::BELOW);
  r.add_label (canvas::range_t (0, 5),
	       std::move (s),
	       style::id_plain,
	       x_ruler::label_kind::TEXT_WITH_BORDER);

  ASSERT_X_RULER_STREQ
    (r, ascii_theme (), sm, true,
     "|~+~|\n"
     "  |\n"
     "+-+------------------+\n"
     "|⚠️  this is a warning|\n"
     "+--------------------+\n");
}

/* Run all selftests in this file.  */

void
text_art_ruler_cc_tests ()
{
  test_single ();
  test_single_above ();
  test_multiple_contiguous ();
  test_multiple_contiguous_above ();
  test_multiple_contiguous_abutting_labels ();
  test_multiple_contiguous_overlapping_labels ();
  test_abutting_left_border ();
  test_too_long_to_consolidate_vertically ();
  test_abutting_neighbor ();
  test_gaps ();
  test_styled ();
  test_borders ();
  test_emoji ();
}

} // namespace selftest


#endif /* #if CHECKING_P */
