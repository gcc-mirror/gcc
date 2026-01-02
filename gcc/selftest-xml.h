/* Selftest support for XML.
   Copyright (C) 2025-2026 Free Software Foundation, Inc.
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

#ifndef GCC_SELFTEST_XML_H
#define GCC_SELFTEST_XML_H

#include "xml.h"

/* The selftest code should entirely disappear in a production
   configuration, hence we guard all of it with #if CHECKING_P.  */

#if CHECKING_P

namespace selftest {

/* Assert that NODE written with indentation as xml source
   equals EXPECTED_VALUE.
   Use LOC for any failures.  */

void
assert_xml_print_eq (const location &loc,
		     const xml::node &node,
		     const char *expected_value);
#define ASSERT_XML_PRINT_EQ(XML_NODE, EXPECTED_VALUE)		\
  assert_xml_print_eq ((SELFTEST_LOCATION),			\
		       (XML_NODE),				\
		       (EXPECTED_VALUE))

} // namespace selftest

#endif /* #if CHECKING_P */

#endif /* GCC_SELFTEST_XML_H */
