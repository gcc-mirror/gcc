/* Unit tests for hash-map.h.
   Copyright (C) 2015-2017 Free Software Foundation, Inc.

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
#include "fixed-value.h"
#include "alias.h"
#include "flags.h"
#include "symtab.h"
#include "tree-core.h"
#include "stor-layout.h"
#include "tree.h"
#include "stringpool.h"
#include "selftest.h"

#if CHECKING_P

namespace selftest {

/* Construct a hash_map <const char *, int> and verify that
   various operations work correctly.  */

static void
test_map_of_strings_to_int ()
{
  hash_map <const char *, int> m;

  const char *ostrich = "ostrich";
  const char *elephant = "elephant";
  const char *ant = "ant";
  const char *spider = "spider";
  const char *millipede = "Illacme plenipes";
  const char *eric = "half a bee";

  /* A fresh hash_map should be empty.  */
  ASSERT_EQ (0, m.elements ());
  ASSERT_EQ (NULL, m.get (ostrich));

  /* Populate the hash_map.  */
  ASSERT_EQ (false, m.put (ostrich, 2));
  ASSERT_EQ (false, m.put (elephant, 4));
  ASSERT_EQ (false, m.put (ant, 6));
  ASSERT_EQ (false, m.put (spider, 8));
  ASSERT_EQ (false, m.put (millipede, 750));
  ASSERT_EQ (false, m.put (eric, 3));

  /* Verify that we can recover the stored values.  */
  ASSERT_EQ (6, m.elements ());
  ASSERT_EQ (2, *m.get (ostrich));
  ASSERT_EQ (4, *m.get (elephant));
  ASSERT_EQ (6, *m.get (ant));
  ASSERT_EQ (8, *m.get (spider));
  ASSERT_EQ (750, *m.get (millipede));
  ASSERT_EQ (3, *m.get (eric));

  /* Verify removing an item.  */
  m.remove (eric);
  ASSERT_EQ (5, m.elements ());
  ASSERT_EQ (NULL, m.get (eric));
}

/* Run all of the selftests within this file.  */

void
hash_map_tests_c_tests ()
{
  test_map_of_strings_to_int ();
}

} // namespace selftest

#endif /* CHECKING_P */
