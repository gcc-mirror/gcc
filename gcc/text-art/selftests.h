/* Copyright (C) 2023-2024 Free Software Foundation, Inc.
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

#ifndef GCC_TEXT_ART_SELFTESTS_H
#define GCC_TEXT_ART_SELFTESTS_H

#if CHECKING_P

namespace text_art {
  class canvas;
} // namespace text_art

namespace selftest {

extern void text_art_box_drawing_cc_tests ();
extern void text_art_canvas_cc_tests ();
extern void text_art_ruler_cc_tests ();
extern void text_art_style_cc_tests ();
extern void text_art_styled_string_cc_tests ();
extern void text_art_table_cc_tests ();
extern void text_art_widget_cc_tests ();

extern void text_art_tests ();

extern void assert_canvas_streq (const location &loc,
				 const text_art::canvas &canvas,
				 pretty_printer *pp,
				 const char *expected_str);
extern void assert_canvas_streq (const location &loc,
				 const text_art::canvas &canvas,
				 bool styled,
				 const char *expected_str);

#define ASSERT_CANVAS_STREQ(CANVAS, STYLED, EXPECTED_STR)		\
  SELFTEST_BEGIN_STMT							\
    assert_canvas_streq ((SELFTEST_LOCATION),				\
			 (CANVAS),					\
			 (STYLED), 					\
			 (EXPECTED_STR));				\
  SELFTEST_END_STMT

} /* end of namespace selftest.  */

#endif /* #if CHECKING_P */

#endif /* GCC_TEXT_ART_SELFTESTS_H */
