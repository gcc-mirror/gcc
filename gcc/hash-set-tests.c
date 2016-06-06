/* Unit tests for hash-set.h.
   Copyright (C) 2015-2016 Free Software Foundation, Inc.

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
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "opts.h"
#include "signop.h"
#include "hash-set.h"
#include "selftest.h"

#if CHECKING_P

namespace selftest {

/* Construct a hash_set <const char *> and verify that various operations
   work correctly.  */

static void
test_set_of_strings ()
{
  hash_set <const char *> s;
  ASSERT_EQ (0, s.elements ());

  const char *red = "red";
  const char *green = "green";
  const char *blue = "blue";

  ASSERT_EQ (false, s.contains (red));

  /* Populate the hash_set.  */
  ASSERT_EQ (false, s.add (red));
  ASSERT_EQ (false, s.add (green));
  ASSERT_EQ (false, s.add (blue));

  /* Verify that the values are now within the set.  */
  ASSERT_EQ (true, s.contains (red));
  ASSERT_EQ (true, s.contains (green));
  ASSERT_EQ (true, s.contains (blue));
}

/* Run all of the selftests within this file.  */

void
hash_set_tests_c_tests ()
{
  test_set_of_strings ();
}

} // namespace selftest

#endif /* #if CHECKING_P */
