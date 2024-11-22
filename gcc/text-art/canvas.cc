/* Canvas for random-access procedural text art.
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
#include "text-art/selftests.h"
#include "text-art/canvas.h"

using namespace text_art;

canvas::canvas (size_t size, const style_manager &style_mgr)
: m_cells (size_t (size.w, size.h)),
  m_style_mgr (style_mgr)
{
  m_cells.fill (cell_t (' '));
}

void
canvas::paint (coord_t coord, styled_unichar ch)
{
  m_cells.set (coord, std::move (ch));
}

void
canvas::paint_text (coord_t coord, const styled_string &text)
{
  for (auto ch : text)
    {
      paint (coord, ch);
      if (ch.double_width_p ())
	coord.x += 2;
      else
	coord.x++;
    }
}

void
canvas::fill (rect_t rect, cell_t c)
{
  for (int y = rect.get_min_y (); y < rect.get_next_y (); y++)
    for (int x = rect.get_min_x (); x < rect.get_next_x (); x++)
      paint(coord_t (x, y), c);
}

void
canvas::debug_fill ()
{
  fill (rect_t (coord_t (0, 0), get_size ()), cell_t ('*'));
}

void
canvas::print_to_pp (pretty_printer *pp,
		     const char *per_line_prefix) const
{
  for (int y = 0; y < m_cells.get_size ().h; y++)
    {
      style::id_t curr_style_id = 0;
      if (per_line_prefix)
	pp_string (pp, per_line_prefix);

      pretty_printer line_pp;
      pp_show_color (&line_pp) = pp_show_color (pp);
      line_pp.set_url_format (pp->get_url_format ());
      const int final_x_in_row = get_final_x_in_row (y);
      for (int x = 0; x <= final_x_in_row; x++)
	{
	  if (x > 0)
	    {
	      const cell_t prev_cell = m_cells.get (coord_t (x - 1, y));
	      if (prev_cell.double_width_p ())
		 /* This cell is just a placeholder for the
		    2nd column of a double width cell; skip it.  */
		continue;
	    }
	  const cell_t cell = m_cells.get (coord_t (x, y));
	  if (cell.get_style_id () != curr_style_id)
	    {
	      m_style_mgr.print_any_style_changes (&line_pp,
						   curr_style_id,
						   cell.get_style_id ());
	      curr_style_id = cell.get_style_id ();
	    }
	  pp_unicode_character (&line_pp, cell.get_code ());
	  if (cell.emoji_variant_p ())
	    /* Append U+FE0F VARIATION SELECTOR-16 to select the emoji
	       variation of the char.  */
	    pp_unicode_character (&line_pp, 0xFE0F);
	}
      /* Reset the style at the end of each line.  */
      m_style_mgr.print_any_style_changes (&line_pp, curr_style_id, 0);

      /* Print from line_pp to pp, stripping trailing whitespace from
	 the line.  */
      const char *line_buf = pp_formatted_text (&line_pp);
      ::size_t len = strlen (line_buf);
      while (len > 0)
	{
	  if (line_buf[len - 1] == ' ')
	    len--;
	  else
	    break;
	}
      pp_append_text (pp, line_buf, line_buf + len);
      pp_newline (pp);
    }
}

DEBUG_FUNCTION void
canvas::debug (bool styled) const
{
  pretty_printer pp;
  if (styled)
    {
      pp_show_color (&pp) = true;
      pp.set_url_format (determine_url_format (DIAGNOSTICS_URL_AUTO));
    }
  print_to_pp (&pp);
  fprintf (stderr, "%s\n", pp_formatted_text (&pp));
}

/* Find right-most non-default cell in this row,
   or -1 if all are default.  */

int
canvas::get_final_x_in_row (int y) const
{
  for (int x = m_cells.get_size ().w - 1; x >= 0; x--)
    {
      cell_t cell = m_cells.get (coord_t (x, y));
      if (cell.get_code () != ' '
	  || cell.get_style_id () != style::id_plain)
	return x;
    }
  return -1;
}

#if CHECKING_P

namespace selftest {

static void
test_blank ()
{
  style_manager sm;
  canvas c (canvas::size_t (5, 5), sm);
  ASSERT_CANVAS_STREQ (c, false,
		       ("\n"
			"\n"
			"\n"
			"\n"
			"\n"));
}

static void
test_abc ()
{
  style_manager sm;
  canvas c (canvas::size_t (3, 3), sm);
  c.paint (canvas::coord_t (0, 0), styled_unichar ('A'));
  c.paint (canvas::coord_t (1, 1), styled_unichar ('B'));
  c.paint (canvas::coord_t (2, 2), styled_unichar ('C'));

  ASSERT_CANVAS_STREQ (c, false,
		       "A\n B\n  C\n");
}

static void
test_debug_fill ()
{
  style_manager sm;
  canvas c (canvas::size_t (5, 3), sm);
  c.debug_fill();
  ASSERT_CANVAS_STREQ (c, false,
		       ("*****\n"
			"*****\n"
			"*****\n"));
}

static void
test_text ()
{
  style_manager sm;
  canvas c (canvas::size_t (6, 1), sm);
  c.paint_text (canvas::coord_t (0, 0), styled_string (sm, "012345"));
  ASSERT_CANVAS_STREQ (c, false,
		       ("012345\n"));

  /* Paint an emoji character that should occupy two canvas columns when
     printed.  */
  c.paint_text (canvas::coord_t (2, 0), styled_string ((cppchar_t)0x1f642));
  ASSERT_CANVAS_STREQ (c, false,
		       ("01🙂45\n"));
}

static void
test_circle ()
{
  canvas::size_t sz (30, 30);
  style_manager sm;
  canvas canvas (sz, sm);
  canvas::coord_t center (sz.w / 2, sz.h / 2);
  const int radius = 12;
  const int radius_squared = radius * radius;
  for (int x = 0; x < sz.w; x++)
    for (int y = 0; y < sz.h; y++)
      {
	int dx = x - center.x;
	int dy = y - center.y;
	char ch = "AB"[(x + y) % 2];
	if (dx * dx + dy * dy < radius_squared)
	  canvas.paint (canvas::coord_t (x, y), styled_unichar (ch));
      }
  ASSERT_CANVAS_STREQ
    (canvas, false,
     ("\n"
      "\n"
      "\n"
      "\n"
      "           BABABABAB\n"
      "         ABABABABABABA\n"
      "        ABABABABABABABA\n"
      "       ABABABABABABABABA\n"
      "      ABABABABABABABABABA\n"
      "     ABABABABABABABABABABA\n"
      "     BABABABABABABABABABAB\n"
      "    BABABABABABABABABABABAB\n"
      "    ABABABABABABABABABABABA\n"
      "    BABABABABABABABABABABAB\n"
      "    ABABABABABABABABABABABA\n"
      "    BABABABABABABABABABABAB\n"
      "    ABABABABABABABABABABABA\n"
      "    BABABABABABABABABABABAB\n"
      "    ABABABABABABABABABABABA\n"
      "    BABABABABABABABABABABAB\n"
      "     BABABABABABABABABABAB\n"
      "     ABABABABABABABABABABA\n"
      "      ABABABABABABABABABA\n"
      "       ABABABABABABABABA\n"
      "        ABABABABABABABA\n"
      "         ABABABABABABA\n"
      "           BABABABAB\n"
      "\n"
      "\n"
      "\n"));
}

static void
test_color_circle ()
{
  const canvas::size_t sz (10, 10);
  const canvas::coord_t center (sz.w / 2, sz.h / 2);
  const int outer_r2 = 25;
  const int inner_r2 = 10;
  style_manager sm;
  canvas c (sz, sm);
  for (int x = 0; x < sz.w; x++)
    for (int y = 0; y < sz.h; y++)
      {
	const int dist_from_center_squared
	  = ((x - center.x) * (x - center.x) + (y - center.y) * (y - center.y));
	if (dist_from_center_squared < outer_r2)
	  {
	    style s;
	    if (dist_from_center_squared < inner_r2)
	      s.m_fg_color = style::named_color::RED;
	    else
	      s.m_fg_color = style::named_color::GREEN;
	    c.paint (canvas::coord_t (x, y),
		     styled_unichar ('*', false, sm.get_or_create_id (s)));
	  }
      }
  ASSERT_EQ (sm.get_num_styles (), 3);
  ASSERT_CANVAS_STREQ
    (c, false,
     ("\n"
      "   *****\n"
      "  *******\n"
      " *********\n"
      " *********\n"
      " *********\n"
      " *********\n"
      " *********\n"
      "  *******\n"
      "   *****\n"));
  ASSERT_CANVAS_STREQ
    (c, true,
     ("\n"
      "   [32m[K*****[m[K\n"
      "  [32m[K***[31m[K*[32m[K***[m[K\n"
      " [32m[K**[31m[K*****[32m[K**[m[K\n"
      " [32m[K**[31m[K*****[32m[K**[m[K\n"
      " [32m[K*[31m[K*******[32m[K*[m[K\n"
      " [32m[K**[31m[K*****[32m[K**[m[K\n"
      " [32m[K**[31m[K*****[32m[K**[m[K\n"
      "  [32m[K***[31m[K*[32m[K***[m[K\n"
      "   [32m[K*****[m[K\n"));
}

static void
test_bold ()
{
  auto_fix_quotes fix_quotes;
  style_manager sm;
  styled_string s (styled_string::from_fmt (sm, nullptr,
					    "before %qs after", "foo"));
  canvas c (canvas::size_t (s.calc_canvas_width (), 1), sm);
  c.paint_text (canvas::coord_t (0, 0), s);
  ASSERT_CANVAS_STREQ (c, false,
		       "before `foo' after\n");
  ASSERT_CANVAS_STREQ (c, true,
		       "before `[00;01m[Kfoo[00m[K' after\n");
}

static void
test_emoji ()
{
  style_manager sm;
  styled_string s (0x26A0, /* U+26A0 WARNING SIGN.  */
		   true);
  canvas c (canvas::size_t (s.calc_canvas_width (), 1), sm);
  c.paint_text (canvas::coord_t (0, 0), s);
  ASSERT_CANVAS_STREQ (c, false, "⚠️\n");
  ASSERT_CANVAS_STREQ (c, true, "⚠️\n");
}

static void
test_emoji_2 ()
{
  style_manager sm;
  styled_string s;
  s.append (styled_string (0x26A0, /* U+26A0 WARNING SIGN.  */
			   true));
  s.append (styled_string (sm, "test"));
  ASSERT_EQ (s.size (), 5);
  ASSERT_EQ (s.calc_canvas_width (), 5);
  canvas c (canvas::size_t (s.calc_canvas_width (), 1), sm);
  c.paint_text (canvas::coord_t (0, 0), s);
  ASSERT_CANVAS_STREQ (c, false,
		       /* U+26A0 WARNING SIGN, as UTF-8: 0xE2 0x9A 0xA0.  */
		       "\xE2\x9A\xA0"
		       /* U+FE0F VARIATION SELECTOR-16, as UTF-8: 0xEF 0xB8 0x8F.  */
		       "\xEF\xB8\x8F"
		       "test\n");
}

static void
test_canvas_urls ()
{
  style_manager sm;
  canvas canvas (canvas::size_t (9, 3), sm);
  styled_string foo_ss (sm, "foo");
  foo_ss.set_url (sm, "https://www.example.com/foo");
  styled_string bar_ss (sm, "bar");
  bar_ss.set_url (sm, "https://www.example.com/bar");
  canvas.paint_text(canvas::coord_t (1, 1), foo_ss);
  canvas.paint_text(canvas::coord_t (5, 1), bar_ss);

  ASSERT_CANVAS_STREQ (canvas, false,
		       ("\n"
			" foo bar\n"
			"\n"));
  {
    pretty_printer pp;
    pp_show_color (&pp) = true;
    pp.set_url_format (URL_FORMAT_ST);
    assert_canvas_streq (SELFTEST_LOCATION, canvas, &pp,
			 (/* Line 1.  */
			  "\n"
			  /* Line 2.  */
			  " "
			  "\33]8;;https://www.example.com/foo\33\\foo\33]8;;\33\\"
			  " "
			  "\33]8;;https://www.example.com/bar\33\\bar\33]8;;\33\\"
			  "\n"
			  /* Line 3.  */
			  "\n"));
  }

  {
    pretty_printer pp;
    pp_show_color (&pp) = true;
    pp.set_url_format (URL_FORMAT_BEL);
    assert_canvas_streq (SELFTEST_LOCATION, canvas, &pp,
			 (/* Line 1.  */
			  "\n"
			  /* Line 2.  */
			  " "
			  "\33]8;;https://www.example.com/foo\afoo\33]8;;\a"
			  " "
			  "\33]8;;https://www.example.com/bar\abar\33]8;;\a"
			  "\n"
			  /* Line 3.  */
			  "\n"));
  }
}

/* Run all selftests in this file.  */

void
text_art_canvas_cc_tests ()
{
  test_blank ();
  test_abc ();
  test_debug_fill ();
  test_text ();
  test_circle ();
  test_color_circle ();
  test_bold ();
  test_emoji ();
  test_emoji_2 ();
  test_canvas_urls ();
}

} // namespace selftest


#endif /* #if CHECKING_P */
