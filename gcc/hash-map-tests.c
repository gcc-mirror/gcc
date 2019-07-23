/* Unit tests for hash-map.h.
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
  ASSERT_TRUE (m.is_empty ());
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

  m.remove (eric);
  ASSERT_EQ (5, m.elements ());
  ASSERT_EQ (NULL, m.get (eric));

  /* A plain char * key is hashed based on its value (address), rather
     than the string it points to.  */
  char *another_ant = static_cast <char *> (xcalloc (4, 1));
  another_ant[0] = 'a';
  another_ant[1] = 'n';
  another_ant[2] = 't';
  another_ant[3] = 0;
  ASSERT_NE (ant, another_ant);
  unsigned prev_size = m.elements ();
  ASSERT_EQ (false, m.put (another_ant, 7));
  ASSERT_EQ (prev_size + 1, m.elements ());

  /* Need to use string_hash or nofree_string_hash key types to hash
     based on the string contents.  */
  hash_map <nofree_string_hash, int> string_map;
  ASSERT_EQ (false, string_map.put (ant, 1));
  ASSERT_EQ (1, string_map.elements ());
  ASSERT_EQ (true, string_map.put (another_ant, 5));
  ASSERT_EQ (1, string_map.elements ());
}

typedef class hash_map_test_val_t
{
public:
  static int ndefault;
  static int ncopy;
  static int nassign;
  static int ndtor;

  hash_map_test_val_t ()
    : ptr (&ptr)
  {
    ++ndefault;
  }

  hash_map_test_val_t (const hash_map_test_val_t &)
    : ptr (&ptr)
  {
    ++ncopy;
  }

  hash_map_test_val_t& operator= (const hash_map_test_val_t &)
    {
     ++nassign;
     return *this;
    }

  ~hash_map_test_val_t ()
    {
     gcc_assert (ptr == &ptr);
     ++ndtor;
    }

  void *ptr;
} val_t;

int val_t::ndefault;
int val_t::ncopy;
int val_t::nassign;
int val_t::ndtor;

static void
test_map_of_type_with_ctor_and_dtor ()
{
  typedef hash_map <void *, val_t> Map;

  {
    /* Test default ctor.  */
    Map m;
    (void)&m;
  }

  ASSERT_TRUE (val_t::ndefault == 0);
  ASSERT_TRUE (val_t::ncopy == 0);
  ASSERT_TRUE (val_t::nassign == 0);
  ASSERT_TRUE (val_t::ndtor == 0);

  {
    /* Test single insertion.  */
    Map m;
    void *p = &p;
    m.get_or_insert (p);
  }

  ASSERT_TRUE (val_t::ndefault + val_t::ncopy == val_t::ndtor);

  {
    /* Test copy ctor.  */
    Map m1;
    void *p = &p;
    val_t &rv1 = m1.get_or_insert (p);

    int ncopy = val_t::ncopy;
    int nassign = val_t::nassign;

    Map m2 (m1);
    val_t *pv2 = m2.get (p);

    ASSERT_TRUE (ncopy + 1 == val_t::ncopy);
    ASSERT_TRUE (nassign == val_t::nassign);

    ASSERT_TRUE (&rv1 != pv2);
    ASSERT_TRUE (pv2->ptr == &pv2->ptr);
  }

  ASSERT_TRUE (val_t::ndefault + val_t::ncopy == val_t::ndtor);

#if 0   /* Avoid testing until bug 90959 is fixed.  */
  {
    /* Test copy assignment into an empty map.  */
    Map m1;
    void *p = &p;
    val_t &rv1 = m1.get_or_insert (p);

    int ncopy = val_t::ncopy;
    int nassign = val_t::nassign;

    Map m2;
    m2 = m1;
    val_t *pv2 = m2.get (p);

    ASSERT_TRUE (ncopy == val_t::ncopy);
    ASSERT_TRUE (nassign + 1 == val_t::nassign);

    ASSERT_TRUE (&rv1 != pv2);
    ASSERT_TRUE (pv2->ptr == &pv2->ptr);
  }

  ASSERT_TRUE (val_t::ndefault + val_t::ncopy == val_t::ndtor);

#endif

  {
    Map m;
    void *p = &p, *q = &q;
    val_t &v1 = m.get_or_insert (p);
    val_t &v2 = m.get_or_insert (q);

    ASSERT_TRUE (v1.ptr == &v1.ptr && &v2.ptr == v2.ptr);
  }

  ASSERT_TRUE (val_t::ndefault + val_t::ncopy == val_t::ndtor);

  {
    Map m;
    void *p = &p, *q = &q;
    m.get_or_insert (p);
    m.remove (p);
    m.get_or_insert (q);
    m.remove (q);

    ASSERT_TRUE (val_t::ndefault + val_t::ncopy == val_t::ndtor);
  }
}

/* Run all of the selftests within this file.  */

void
hash_map_tests_c_tests ()
{
  test_map_of_strings_to_int ();
  test_map_of_type_with_ctor_and_dtor ();
}

} // namespace selftest

#endif /* CHECKING_P */
