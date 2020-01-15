/* Unit tests for ordered-hash-map.h.
   Copyright (C) 2015-2020 Free Software Foundation, Inc.

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
#include "fixed-value.h"
#include "alias.h"
#include "flags.h"
#include "symtab.h"
#include "tree-core.h"
#include "stor-layout.h"
#include "tree.h"
#include "stringpool.h"
#include "ordered-hash-map.h"
#include "selftest.h"

#if CHECKING_P

namespace selftest {

/* Populate *OUT_KVS with the key/value pairs of M.  */

template <typename HashMap, typename Key, typename Value>
static void
get_kv_pairs (const HashMap &m,
	      auto_vec<std::pair<Key, Value> > *out_kvs)
{
  for (typename HashMap::iterator iter = m.begin ();
       iter != m.end ();
       ++iter)
    out_kvs->safe_push (std::make_pair ((*iter).first, (*iter).second));
}

/* Construct an ordered_hash_map <const char *, int> and verify that
   various operations work correctly.  */

static void
test_map_of_strings_to_int ()
{
  ordered_hash_map <const char *, int> m;

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

  /* Verify that the order of insertion is preserved.  */
  auto_vec<std::pair<const char *, int> > kvs;
  get_kv_pairs (m, &kvs);
  ASSERT_EQ (kvs.length (), 6);
  ASSERT_EQ (kvs[0].first, ostrich);
  ASSERT_EQ (kvs[0].second, 2);
  ASSERT_EQ (kvs[1].first, elephant);
  ASSERT_EQ (kvs[1].second, 4);
  ASSERT_EQ (kvs[2].first, ant);
  ASSERT_EQ (kvs[2].second, 6);
  ASSERT_EQ (kvs[3].first, spider);
  ASSERT_EQ (kvs[3].second, 8);
  ASSERT_EQ (kvs[4].first, millipede);
  ASSERT_EQ (kvs[4].second, 750);
  ASSERT_EQ (kvs[5].first, eric);
  ASSERT_EQ (kvs[5].second, 3);
}

/* Construct an ordered_hash_map using int_hash and verify that various
   operations work correctly.  */

static void
test_map_of_int_to_strings ()
{
  const int EMPTY = -1;
  const int DELETED = -2;
  typedef int_hash <int, EMPTY, DELETED> int_hash_t;
  ordered_hash_map <int_hash_t, const char *> m;

  const char *ostrich = "ostrich";
  const char *elephant = "elephant";
  const char *ant = "ant";
  const char *spider = "spider";
  const char *millipede = "Illacme plenipes";
  const char *eric = "half a bee";

  /* A fresh hash_map should be empty.  */
  ASSERT_EQ (0, m.elements ());
  ASSERT_EQ (NULL, m.get (2));

  /* Populate the hash_map.  */
  ASSERT_EQ (false, m.put (2, ostrich));
  ASSERT_EQ (false, m.put (4, elephant));
  ASSERT_EQ (false, m.put (6, ant));
  ASSERT_EQ (false, m.put (8, spider));
  ASSERT_EQ (false, m.put (750, millipede));
  ASSERT_EQ (false, m.put (3, eric));

  /* Verify that we can recover the stored values.  */
  ASSERT_EQ (6, m.elements ());
  ASSERT_EQ (*m.get (2), ostrich);
  ASSERT_EQ (*m.get (4), elephant);
  ASSERT_EQ (*m.get (6), ant);
  ASSERT_EQ (*m.get (8), spider);
  ASSERT_EQ (*m.get (750), millipede);
  ASSERT_EQ (*m.get (3), eric);

  /* Verify that the order of insertion is preserved.  */
  auto_vec<std::pair<int, const char *> > kvs;
  get_kv_pairs (m, &kvs);
  ASSERT_EQ (kvs.length (), 6);
  ASSERT_EQ (kvs[0].first, 2);
  ASSERT_EQ (kvs[0].second, ostrich);
  ASSERT_EQ (kvs[1].first, 4);
  ASSERT_EQ (kvs[1].second, elephant);
  ASSERT_EQ (kvs[2].first, 6);
  ASSERT_EQ (kvs[2].second, ant);
  ASSERT_EQ (kvs[3].first, 8);
  ASSERT_EQ (kvs[3].second, spider);
  ASSERT_EQ (kvs[4].first, 750);
  ASSERT_EQ (kvs[4].second, millipede);
  ASSERT_EQ (kvs[5].first, 3);
  ASSERT_EQ (kvs[5].second, eric);
}

/* Verify that we can remove items from an ordered_hash_map.  */

static void
test_removal ()
{
  ordered_hash_map <const char *, int> m;

  const char *ostrich = "ostrich";
  ASSERT_EQ (false, m.put (ostrich, 2));

  ASSERT_EQ (1, m.elements ());
  ASSERT_EQ (2, *m.get (ostrich));

  {
    auto_vec<std::pair<const char *, int> > kvs;
    get_kv_pairs (m, &kvs);
    ASSERT_EQ (kvs.length (), 1);
    ASSERT_EQ (kvs[0].first, ostrich);
    ASSERT_EQ (kvs[0].second, 2);
  }

  m.remove (ostrich);

  ASSERT_EQ (0, m.elements ());
  {
    auto_vec<std::pair<const char *, int> > kvs;
    get_kv_pairs (m, &kvs);
    ASSERT_EQ (kvs.length (), 0);
  }

  /* Reinsertion (with a different value).  */
  ASSERT_EQ (false, m.put (ostrich, 42));
  ASSERT_EQ (1, m.elements ());
  ASSERT_EQ (42, *m.get (ostrich));
  {
    auto_vec<std::pair<const char *, int> > kvs;
    get_kv_pairs (m, &kvs);
    ASSERT_EQ (kvs.length (), 1);
    ASSERT_EQ (kvs[0].first, ostrich);
    ASSERT_EQ (kvs[0].second, 42);
  }
}

/* Verify that ordered_hash_map's copy-ctor works.  */

static void
test_copy_ctor ()
{
  ordered_hash_map <const char *, int> m;

  const char *ostrich = "ostrich";
  ASSERT_EQ (false, m.put (ostrich, 2));

  ASSERT_EQ (1, m.elements ());
  ASSERT_EQ (2, *m.get (ostrich));

  ordered_hash_map <const char *, int> copy (m);
  ASSERT_EQ (1, copy.elements ());
  ASSERT_EQ (2, *copy.get (ostrich));

  /* Remove from source.  */
  m.remove (ostrich);
  ASSERT_EQ (0, m.elements ());

  /* Copy should be unaffected.  */
  ASSERT_EQ (1, copy.elements ());
  ASSERT_EQ (2, *copy.get (ostrich));
}

/* Run all of the selftests within this file.  */

void
ordered_hash_map_tests_cc_tests ()
{
  test_map_of_strings_to_int ();
  test_map_of_int_to_strings ();
  test_removal ();
  test_copy_ctor ();
}

} // namespace selftest

#endif /* CHECKING_P */
