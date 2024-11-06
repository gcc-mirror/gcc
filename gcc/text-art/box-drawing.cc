/* Procedural lookup of box drawing characters.
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
#define INCLUDE_MEMORY
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "text-art/box-drawing.h"
#include "selftest.h"
#include "text-art/selftests.h"


/* According to
     https://en.wikipedia.org/wiki/Box-drawing_character#Character_code
   "DOS line- and box-drawing characters are not ordered in any programmatic
   manner, so calculating a particular character shape needs to use a look-up
   table. "
   Hence this array.  */
static const cppchar_t box_drawing_chars[] = {
#include "text-art/box-drawing-chars.inc"
};

cppchar_t
text_art::get_box_drawing_char (directions line_dirs)
{
  const size_t idx = line_dirs.as_index ();
  gcc_assert (idx < 16);
  return box_drawing_chars[idx];
}

#if CHECKING_P

namespace selftest {

/* Run all selftests in this file.  */

void
text_art_box_drawing_cc_tests ()
{
  ASSERT_EQ (text_art::get_box_drawing_char
	      (text_art::directions (false, false, false, false)),
	     ' ');
  ASSERT_EQ (text_art::get_box_drawing_char
	       (text_art::directions (false, false, true, true)),
	     0x2500); /* BOX DRAWINGS LIGHT HORIZONTAL */
  ASSERT_EQ (text_art::get_box_drawing_char
	       (text_art::directions (true, true, false, false)),
	     0x2502); /* BOX DRAWINGS LIGHT VERTICAL */
  ASSERT_EQ (text_art::get_box_drawing_char
	       (text_art::directions (true, false, true, false)),
	     0x2518); /* BOX DRAWINGS LIGHT UP AND LEFT */
}

} // namespace selftest

#endif /* #if CHECKING_P */
