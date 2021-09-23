/* Unit tests for hash-map.h.
   Copyright (C) 2015-2021 Free Software Foundation, Inc.

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

  free (another_ant);
}

/* Construct a hash_map using int_hash and verify that
   various operations work correctly.  */

static void
test_map_of_int_to_strings ()
{
  const int EMPTY = -1;
  const int DELETED = -2;
  typedef int_hash <int, EMPTY, DELETED> int_hash_t;
  hash_map <int_hash_t, const char *> m;

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

  hash_map_test_val_t (const hash_map_test_val_t &rhs)
    : ptr (&ptr)
  {
    ++ncopy;
    gcc_assert (rhs.ptr == &rhs.ptr);
  }

  hash_map_test_val_t& operator= (const hash_map_test_val_t &rhs)
  {
    ++nassign;
    gcc_assert (ptr == &ptr);
    gcc_assert (rhs.ptr == &rhs.ptr);
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


  /* Verify basic construction and destruction of Value objects.  */
  {
    /* Configure, arbitrary.  */
    const size_t N_init = 0;
    const int N_elem = 28;

    void *a[N_elem];
    for (size_t i = 0; i < N_elem; ++i)
      a[i] = &a[i];

    val_t::ndefault = 0;
    val_t::ncopy = 0;
    val_t::nassign = 0;
    val_t::ndtor = 0;
    Map m (N_init);
    ASSERT_EQ (val_t::ndefault
	       + val_t::ncopy
	       + val_t::nassign
	       + val_t::ndtor, 0);

    for (int i = 0; i < N_elem; ++i)
      {
	m.get_or_insert (a[i]);
	ASSERT_EQ (val_t::ndefault, 1 + i);
	ASSERT_EQ (val_t::ncopy, 0);
	ASSERT_EQ (val_t::nassign, 0);
	ASSERT_EQ (val_t::ndtor, i);

	m.remove (a[i]);
	ASSERT_EQ (val_t::ndefault, 1 + i);
	ASSERT_EQ (val_t::ncopy, 0);
	ASSERT_EQ (val_t::nassign, 0);
	ASSERT_EQ (val_t::ndtor, 1 + i);
      }
  }
}

/* Verify aspects of 'hash_table::expand', in particular that it doesn't leak
   Value objects.  */

static void
test_map_of_type_with_ctor_and_dtor_expand (bool remove_some_inline)
{
  /* Configure, so that hash table expansion triggers a few times.  */
  const size_t N_init = 0;
  const int N_elem = 70;
  size_t expand_c_expected = 4;
  size_t expand_c = 0;

  /* For stability of this testing, we need all Key values 'k' to produce
     unique hash values 'Traits::hash (k)', as otherwise the dynamic
     insert/remove behavior may diverge across different architectures.  This
     is, for example, a problem when using the standard 'pointer_hash::hash',
     which is simply doing a 'k >> 3' operation, which is fine on 64-bit
     architectures, but on 32-bit architectures produces the same hash value
     for subsequent 'a[i] = &a[i]' array elements.  Therefore, use an
     'int_hash'.  */

  int a[N_elem];
  for (size_t i = 0; i < N_elem; ++i)
    a[i] = i;

  const int EMPTY = -1;
  const int DELETED = -2;
  typedef hash_map<int_hash<int, EMPTY, DELETED>, val_t> Map;

  /* Note that we are starting with a fresh 'Map'.  Even if an existing one has
     been cleared out completely, there remain 'deleted' elements, and these
     would disturb the following logic, where we don't have access to the
     actual 'm_n_deleted' value.  */
  size_t m_n_deleted = 0;

  val_t::ndefault = 0;
  val_t::ncopy = 0;
  val_t::nassign = 0;
  val_t::ndtor = 0;
  Map m (N_init);

  /* In the following, in particular related to 'expand', we're adapting from
     the internal logic of 'hash_table', glossing over "some details" not
     relevant for this testing here.  */

  /* Per 'hash_table::hash_table'.  */
  size_t m_size;
  {
    unsigned int size_prime_index_ = hash_table_higher_prime_index (N_init);
    m_size = prime_tab[size_prime_index_].prime;
  }

  int n_expand_moved = 0;

  for (int i = 0; i < N_elem; ++i)
    {
      size_t elts = m.elements ();

      /* Per 'hash_table::find_slot_with_hash'.  */
      size_t m_n_elements = elts + m_n_deleted;
      bool expand = m_size * 3 <= m_n_elements * 4;

      m.get_or_insert (a[i]);
      if (expand)
	{
	  ++expand_c;

	  /* Per 'hash_table::expand'.  */
	  {
	    unsigned int nindex = hash_table_higher_prime_index (elts * 2);
	    m_size = prime_tab[nindex].prime;
	  }
	  m_n_deleted = 0;

	  /* All non-deleted elements have been moved.  */
	  n_expand_moved += i;
	  if (remove_some_inline)
	    n_expand_moved -= (i + 2) / 3;
	}

      ASSERT_EQ (val_t::ndefault, 1 + i);
      ASSERT_EQ (val_t::ncopy, n_expand_moved);
      ASSERT_EQ (val_t::nassign, 0);
      if (remove_some_inline)
	ASSERT_EQ (val_t::ndtor, n_expand_moved + (i + 2) / 3);
      else
	ASSERT_EQ (val_t::ndtor, n_expand_moved);

      /* Remove some inline.  This never triggers an 'expand' here, but via
	 'm_n_deleted' does influence any following one.  */
      if (remove_some_inline
	  && !(i % 3))
	{
	  m.remove (a[i]);
	  /* Per 'hash_table::remove_elt_with_hash'.  */
	  m_n_deleted++;

	  ASSERT_EQ (val_t::ndefault, 1 + i);
	  ASSERT_EQ (val_t::ncopy, n_expand_moved);
	  ASSERT_EQ (val_t::nassign, 0);
	  ASSERT_EQ (val_t::ndtor, n_expand_moved + 1 + (i + 2) / 3);
	}
    }
  ASSERT_EQ (expand_c, expand_c_expected);

  int ndefault = val_t::ndefault;
  int ncopy = val_t::ncopy;
  int nassign = val_t::nassign;
  int ndtor = val_t::ndtor;

  for (int i = 0; i < N_elem; ++i)
    {
      if (remove_some_inline
	  && !(i % 3))
	continue;

      m.remove (a[i]);
      ++ndtor;
      ASSERT_EQ (val_t::ndefault, ndefault);
      ASSERT_EQ (val_t::ncopy, ncopy);
      ASSERT_EQ (val_t::nassign, nassign);
      ASSERT_EQ (val_t::ndtor, ndtor);
    }
  ASSERT_EQ (val_t::ndefault + val_t::ncopy, val_t::ndtor);
}

/* Test calling empty on a hash_map that has a key type with non-zero
   "empty" value.  */

static void
test_nonzero_empty_key ()
{
  typedef int_hash<int, INT_MIN, INT_MAX> IntHash;
  hash_map<int, int, simple_hashmap_traits<IntHash, int> > x;

  for (int i = 1; i != 32; ++i)
    x.put (i, i);

  ASSERT_EQ (x.get (0), NULL);
  ASSERT_EQ (*x.get (1), 1);

  x.empty ();

  ASSERT_EQ (x.get (0), NULL);
  ASSERT_EQ (x.get (1), NULL);
}

/* Run all of the selftests within this file.  */

void
hash_map_tests_c_tests ()
{
  test_map_of_strings_to_int ();
  test_map_of_int_to_strings ();
  test_map_of_type_with_ctor_and_dtor ();
  test_map_of_type_with_ctor_and_dtor_expand (false);
  test_map_of_type_with_ctor_and_dtor_expand (true);
  test_nonzero_empty_key ();
}

} // namespace selftest

#endif /* CHECKING_P */
