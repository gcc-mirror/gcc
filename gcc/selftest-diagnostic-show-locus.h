/* Support for selftests involving diagnostic_show_locus.
   Copyright (C) 1999-2025 Free Software Foundation, Inc.

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

#ifndef GCC_SELFTEST_DIAGNOSTIC_SHOW_LOCUS_H
#define GCC_SELFTEST_DIAGNOSTIC_SHOW_LOCUS_H

#include "selftest.h"

/* The selftest code should entirely disappear in a production
   configuration, hence we guard all of it with #if CHECKING_P.  */

#if CHECKING_P

namespace selftest {

/* RAII class for use in selftests involving diagnostic_show_locus.

   Manages creating and cleaning up the following:
   - writing out a temporary .c file containing CONTENT
   - temporarily override the global "line_table" (using CASE_) and
     push a line_map starting at the first line of the temporary file
   - provide a file_cache.  */

struct diagnostic_show_locus_fixture
{
  diagnostic_show_locus_fixture (const line_table_case &case_,
				 const char *content);

  const char *get_filename () const
  {
    return m_tmp_source_file.get_filename ();
  }

  const char *m_content;
  temp_source_file m_tmp_source_file;
  line_table_test m_ltt;
  file_cache m_fc;
};

/* Fixture for one-liner tests exercising multibyte awareness.  For
   simplicity we stick to using two multibyte characters in the test, U+1F602
   == "\xf0\x9f\x98\x82", which uses 4 bytes and 2 display columns, and U+03C0
   == "\xcf\x80", which uses 2 bytes and 1 display column.

   This works with the following 1-line source file:

     .0000000001111111111222222   display
     .1234567890123456789012345   columns
     "SS_foo = P_bar.SS_fieldP;\n"
     .0000000111111111222222223   byte
     .1356789012456789134567891   columns

   Here SS represents the two display columns for the U+1F602 emoji and
   P represents the one display column for the U+03C0 pi symbol.  */

struct diagnostic_show_locus_fixture_one_liner_utf8
  : public diagnostic_show_locus_fixture
{
  diagnostic_show_locus_fixture_one_liner_utf8 (const line_table_case &case_);
};

} // namespace selftest

#endif /* #if CHECKING_P */

#endif /* GCC_SELFTEST_DIAGNOSTIC_SHOW_LOCUS_H */
