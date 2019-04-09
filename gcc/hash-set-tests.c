/* Unit tests for hash-set.h.
   Copyright (C) 2015-2019 Free Software Foundation, Inc.

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

  for (hash_set<const char *>::iterator it = s.begin (); it != s.end (); ++it)
    ASSERT_EQ (true, false);

  /* Populate the hash_set.  */
  ASSERT_EQ (false, s.add (red));
  ASSERT_EQ (false, s.add (green));
  ASSERT_EQ (false, s.add (blue));
  ASSERT_EQ (true, s.add (green));

  /* Verify that the values are now within the set.  */
  ASSERT_EQ (true, s.contains (red));
  ASSERT_EQ (true, s.contains (green));
  ASSERT_EQ (true, s.contains (blue));
  ASSERT_EQ (3, s.elements ());

  /* Test removal.  */
  s.remove (red);
  ASSERT_EQ (false, s.contains (red));
  ASSERT_EQ (true, s.contains (green));
  ASSERT_EQ (true, s.contains (blue));
  ASSERT_EQ (2, s.elements ());

  s.remove (red);
  ASSERT_EQ (false, s.contains (red));
  ASSERT_EQ (true, s.contains (green));
  ASSERT_EQ (true, s.contains (blue));
  ASSERT_EQ (2, s.elements ());

  int seen = 0;
  for (hash_set<const char *>::iterator it = s.begin (); it != s.end (); ++it)
    {
      int n = *it == green;
      if (n == 0)
	ASSERT_EQ (*it, blue);
      ASSERT_EQ (seen & (1 << n), 0);
      seen |= 1 << n;
    }
  ASSERT_EQ (seen, 3);

  hash_set <const char *, true> t;
  ASSERT_EQ (0, t.elements ());

  ASSERT_EQ (false, t.contains (red));

  for (hash_set<const char *, true>::iterator it = t.begin ();
       it != t.end (); ++it)
    ASSERT_EQ (true, false);

  /* Populate the hash_set.  */
  ASSERT_EQ (false, t.add (red));
  ASSERT_EQ (false, t.add (green));
  ASSERT_EQ (false, t.add (blue));
  ASSERT_EQ (true, t.add (green));

  /* Verify that the values are now within the set.  */
  ASSERT_EQ (true, t.contains (red));
  ASSERT_EQ (true, t.contains (green));
  ASSERT_EQ (true, t.contains (blue));
  ASSERT_EQ (3, t.elements ());

  seen = 0;
  for (hash_set<const char *, true>::iterator it = t.begin ();
       it != t.end (); ++it)
    {
      int n = 2;
      if (*it == green)
	n = 0;
      else if (*it == blue)
	n = 1;
      else
	ASSERT_EQ (*it, red);
      ASSERT_EQ (seen & (1 << n), 0);
      seen |= 1 << n;
    }
  ASSERT_EQ (seen, 7);

  /* Test removal.  */
  t.remove (red);
  ASSERT_EQ (false, t.contains (red));
  ASSERT_EQ (true, t.contains (green));
  ASSERT_EQ (true, t.contains (blue));
  ASSERT_EQ (2, t.elements ());

  t.remove (red);
  ASSERT_EQ (false, t.contains (red));
  ASSERT_EQ (true, t.contains (green));
  ASSERT_EQ (true, t.contains (blue));
  ASSERT_EQ (2, t.elements ());
}

/* Run all of the selftests within this file.  */

void
hash_set_tests_c_tests ()
{
  test_set_of_strings ();
}

} // namespace selftest

#endif /* #if CHECKING_P */
