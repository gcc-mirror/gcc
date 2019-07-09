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

typedef class hash_set_test_value_t
{
public:
  static int ndefault;
  static int ncopy;
  static int nassign;
  static int ndtor;

  hash_set_test_value_t (int v = 1): pval (&val), val (v)
  {
    ++ndefault;
  }

  hash_set_test_value_t (const hash_set_test_value_t &rhs)
    : pval (&val), val (rhs.val)
  {
    ++ncopy;
  }

  hash_set_test_value_t& operator= (const hash_set_test_value_t &rhs)
    {
     ++nassign;
     val = rhs.val;
     return *this;
    }

  ~hash_set_test_value_t ()
    {
     /* Verify that the value hasn't been corrupted.  */
     gcc_assert (*pval > 0);
     gcc_assert (pval == &val);
     *pval = -3;
     ++ndtor;
    }

  int *pval;
  int val;
} val_t;

int val_t::ndefault;
int val_t::ncopy;
int val_t::nassign;
int val_t::ndtor;

struct value_hash_traits: int_hash<int, -1, -2>
{
  typedef int_hash<int, -1, -2> base_type;
  typedef val_t                 value_type;
  typedef value_type            compare_type;

  static hashval_t hash (const value_type &v)
  {
    return base_type::hash (v.val);
  }

  static bool equal (const value_type &a, const compare_type &b)
  {
    return base_type::equal (a.val, b.val);
  }

  static void mark_deleted (value_type &v)
  {
    base_type::mark_deleted (v.val);
  }

  static void mark_empty (value_type &v)
  {
    base_type::mark_empty (v.val);
  }

  static bool is_deleted (const value_type &v)
  {
    return base_type::is_deleted (v.val);
  }

  static bool is_empty (const value_type &v)
  {
    return base_type::is_empty (v.val);
  }

  static void remove (value_type &v)
  {
    v.~value_type ();
  }
};

static void
test_set_of_type_with_ctor_and_dtor ()
{
  typedef hash_set <val_t, false, value_hash_traits> Set;

  {
    Set s;
    (void)&s;
  }

  ASSERT_TRUE (val_t::ndefault == 0);
  ASSERT_TRUE (val_t::ncopy == 0);
  ASSERT_TRUE (val_t::nassign == 0);
  ASSERT_TRUE (val_t::ndtor == 0);

  {
    Set s;
    ASSERT_EQ (false, s.add (val_t ()));
    ASSERT_EQ (true, 1 == s.elements ());
  }

  ASSERT_TRUE (val_t::ndefault + val_t::ncopy == val_t::ndtor);

  {
    Set s;
    ASSERT_EQ (false, s.add (val_t ()));
    ASSERT_EQ (true, s.add (val_t ()));
    ASSERT_EQ (true, 1 == s.elements ());
  }

  ASSERT_TRUE (val_t::ndefault + val_t::ncopy == val_t::ndtor);

  {
    Set s;
    val_t v1 (1), v2 (2), v3 (3);
    int ndefault = val_t::ndefault;
    int nassign = val_t::nassign;

    ASSERT_EQ (false, s.add (v1));
    ASSERT_EQ (true, s.contains (v1));
    ASSERT_EQ (true, 1 == s.elements ());

    ASSERT_EQ (false, s.add (v2));
    ASSERT_EQ (true, s.contains (v2));
    ASSERT_EQ (true, 2 == s.elements ());

    ASSERT_EQ (false, s.add (v3));
    ASSERT_EQ (true, s.contains (v3));
    ASSERT_EQ (true, 3 == s.elements ());

    ASSERT_EQ (true, s.add (v2));
    ASSERT_EQ (true, s.contains (v2));
    ASSERT_EQ (true, 3 == s.elements ());

    s.remove (v2);
    ASSERT_EQ (true, 2 == s.elements ());
    s.remove (v3);
    ASSERT_EQ (true, 1 == s.elements ());

    /* Verify that no default ctors or assignment operators have
       been called.  */
    ASSERT_EQ (true, ndefault == val_t::ndefault);
    ASSERT_EQ (true, nassign == val_t::nassign);
  }

  ASSERT_TRUE (val_t::ndefault + val_t::ncopy == val_t::ndtor);
}

/* Run all of the selftests within this file.  */

void
hash_set_tests_c_tests ()
{
  test_set_of_strings ();
  test_set_of_type_with_ctor_and_dtor ();
}

} // namespace selftest

#endif /* #if CHECKING_P */
