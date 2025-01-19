/* Selftests for text art.
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
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "selftest.h"
#include "pretty-print.h"
#include "text-art/selftests.h"
#include "text-art/canvas.h"

#if CHECKING_P

/* Run all tests, aborting if any fail.  */

void
selftest::text_art_tests ()
{
  text_art_style_cc_tests ();
  text_art_styled_string_cc_tests ();

  text_art_box_drawing_cc_tests ();
  text_art_canvas_cc_tests ();
  text_art_ruler_cc_tests ();
  text_art_table_cc_tests ();
  text_art_widget_cc_tests ();
  text_art_tree_widget_cc_tests ();
}

/* Implementation detail of ASSERT_CANVAS_STREQ.  */

void
selftest::assert_canvas_streq (const location &loc,
			       const text_art::canvas &canvas,
			       pretty_printer *pp,
			       const char *expected_str)
{
  canvas.print_to_pp (pp);
  if (0)
    fprintf (stderr, "%s\n", pp_formatted_text (pp));
  ASSERT_STREQ_AT (loc, pp_formatted_text (pp), expected_str);
}

/* Implementation detail of ASSERT_CANVAS_STREQ.  */

void
selftest::assert_canvas_streq (const location &loc,
			       const text_art::canvas &canvas,
			       bool styled,
			       const char *expected_str)
{
  pretty_printer pp;
  if (styled)
    {
      pp_show_color (&pp) = true;
      pp.set_url_format (URL_FORMAT_DEFAULT);
    }
  assert_canvas_streq (loc, canvas, &pp, expected_str);
}

#endif /* #if CHECKING_P */
