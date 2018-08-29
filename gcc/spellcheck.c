/* Find near-matches for strings.
   Copyright (C) 2015-2018 Free Software Foundation, Inc.

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
#include "tree.h"
#include "spellcheck.h"
#include "selftest.h"

/* Get the edit distance between the two strings: the minimal
   number of edits that are needed to change one string into another,
   where edits can be one-character insertions, removals, or substitutions,
   or transpositions of two adjacent characters (counting as one "edit").

   This implementation uses the Wagner-Fischer algorithm for the
   Damerau-Levenshtein distance; specifically, the "optimal string alignment
   distance" or "restricted edit distance" variant.  */

edit_distance_t
get_edit_distance (const char *s, int len_s,
		   const char *t, int len_t)
{
  const bool debug = false;

  if (debug)
    {
      printf ("s: \"%s\" (len_s=%i)\n", s, len_s);
      printf ("t: \"%s\" (len_t=%i)\n", t, len_t);
    }

  if (len_s == 0)
    return len_t;
  if (len_t == 0)
    return len_s;

  /* We effectively build a matrix where each (i, j) contains the
     distance between the prefix strings s[0:j] and t[0:i].
     Rather than actually build an (len_t + 1) * (len_s + 1) matrix,
     we simply keep track of the last two rows, v_one_ago and v_two_ago,
     and a new row, v_next, which avoids an (len_t + 1) * (len_s + 1)
     allocation and memory accesses in favor of three (len_s + 1)
     allocations.  These could potentially be
     statically-allocated if we impose a maximum length on the
     strings of interest.  */
  edit_distance_t *v_two_ago = new edit_distance_t[len_s + 1];
  edit_distance_t *v_one_ago = new edit_distance_t[len_s + 1];
  edit_distance_t *v_next = new edit_distance_t[len_s + 1];

  /* The first row is for the case of an empty target string, which
     we can reach by deleting every character in the source string.  */
  for (int i = 0; i < len_s + 1; i++)
    v_one_ago[i] = i;

  /* Build successive rows.  */
  for (int i = 0; i < len_t; i++)
    {
      if (debug)
	{
	  printf ("i:%i v_one_ago = ", i);
	  for (int j = 0; j < len_s + 1; j++)
	    printf ("%i ", v_one_ago[j]);
	  printf ("\n");
	}

      /* The initial column is for the case of an empty source string; we
	 can reach prefixes of the target string of length i
	 by inserting i characters.  */
      v_next[0] = i + 1;

      /* Build the rest of the row by considering neighbors to
	 the north, west and northwest.  */
      for (int j = 0; j < len_s; j++)
	{
	  edit_distance_t cost = (s[j] == t[i] ? 0 : 1);
	  edit_distance_t deletion     = v_next[j] + 1;
	  edit_distance_t insertion    = v_one_ago[j + 1] + 1;
	  edit_distance_t substitution = v_one_ago[j] + cost;
	  edit_distance_t cheapest = MIN (deletion, insertion);
	  cheapest = MIN (cheapest, substitution);
	  if (i > 0 && j > 0 && s[j] == t[i - 1] && s[j - 1] == t[i])
	    {
	      edit_distance_t transposition = v_two_ago[j - 1] + 1;
	      cheapest = MIN (cheapest, transposition);
	    }
	  v_next[j + 1] = cheapest;
	}

      /* Prepare to move on to next row.  */
      for (int j = 0; j < len_s + 1; j++)
	{
	  v_two_ago[j] = v_one_ago[j];
	  v_one_ago[j] = v_next[j];
	}
    }

  if (debug)
    {
      printf ("final v_next = ");
      for (int j = 0; j < len_s + 1; j++)
	printf ("%i ", v_next[j]);
      printf ("\n");
    }

  edit_distance_t result = v_next[len_s];
  delete[] v_two_ago;
  delete[] v_one_ago;
  delete[] v_next;
  return result;
}

/* Get the edit distance between two nil-terminated strings.  */

edit_distance_t
get_edit_distance (const char *s, const char *t)
{
  return get_edit_distance (s, strlen (s), t, strlen (t));
}

/* Given TARGET, a non-NULL string, and CANDIDATES, a non-NULL ptr to
   an autovec of non-NULL strings, determine which element within
   CANDIDATES has the lowest edit distance to TARGET.  If there are
   multiple elements with the same minimal distance, the first in the
   vector wins.

   If more than half of the letters were misspelled, the suggestion is
   likely to be meaningless, so return NULL for this case.  */

const char *
find_closest_string (const char *target,
		     const auto_vec<const char *> *candidates)
{
  gcc_assert (target);
  gcc_assert (candidates);

  int i;
  const char *candidate;
  best_match<const char *, const char *> bm (target);
  FOR_EACH_VEC_ELT (*candidates, i, candidate)
    {
      gcc_assert (candidate);
      bm.consider (candidate);
    }

  return bm.get_best_meaningful_candidate ();
}

#if CHECKING_P

namespace selftest {

/* Selftests.  */

/* Verify that get_edit_distance (A, B) equals the expected value.  */

static void
test_edit_distance_unit_test_oneway (const char *a, const char *b,
				    edit_distance_t expected)
{
  edit_distance_t actual = get_edit_distance (a, b);
  ASSERT_EQ (actual, expected);
}

/* Verify that both
     get_edit_distance (A, B)
   and
     get_edit_distance (B, A)
   equal the expected value, to ensure that the function is symmetric.  */

static void
test_get_edit_distance_unit (const char *a, const char *b,
			     edit_distance_t expected)
{
  test_edit_distance_unit_test_oneway (a, b, expected);
  test_edit_distance_unit_test_oneway (b, a, expected);
}

/* Verify that find_closest_string is sane.  */

static void
test_find_closest_string ()
{
  auto_vec<const char *> candidates;

  /* Verify that it can handle an empty vec.  */
  ASSERT_EQ (NULL, find_closest_string ("", &candidates));

  /* Verify that it works sanely for non-empty vecs.  */
  candidates.safe_push ("apple");
  candidates.safe_push ("banana");
  candidates.safe_push ("cherry");

  ASSERT_STREQ ("apple", find_closest_string ("app", &candidates));
  ASSERT_STREQ ("banana", find_closest_string ("banyan", &candidates));
  ASSERT_STREQ ("cherry", find_closest_string ("berry", &candidates));
  ASSERT_EQ (NULL, find_closest_string ("not like the others", &candidates));

  /* The order of the vec can matter, but it should not matter for these
     inputs.  */
  candidates.truncate (0);
  candidates.safe_push ("cherry");
  candidates.safe_push ("banana");
  candidates.safe_push ("apple");
  ASSERT_STREQ ("apple", find_closest_string ("app", &candidates));
  ASSERT_STREQ ("banana", find_closest_string ("banyan", &candidates));
  ASSERT_STREQ ("cherry", find_closest_string ("berry", &candidates));
  ASSERT_EQ (NULL, find_closest_string ("not like the others", &candidates));

  /* If the goal string somehow makes it into the candidate list, offering
     it as a suggestion will be nonsensical.  Verify that we don't offer such
     suggestions.  */
  ASSERT_EQ (NULL, find_closest_string ("banana", &candidates));

  /* Example from PR 69968 where transposition helps.  */
  candidates.truncate (0);
  candidates.safe_push("coordx");
  candidates.safe_push("coordy");
  candidates.safe_push("coordz");
  candidates.safe_push("coordx1");
  candidates.safe_push("coordy1");
  candidates.safe_push("coordz1");
  ASSERT_STREQ ("coordz1", find_closest_string ("coorzd1", &candidates));
}

/* Test data for test_metric_conditions.  */

static const char * const test_data[] = {
  "",
  "foo",
  "food",
  "boo",
  "1234567890123456789012345678901234567890123456789012345678901234567890"
};

/* Verify that get_edit_distance appears to be a sane distance function,
   i.e. the conditions for being a metric.  This is done directly for a
   small set of examples, using test_data above.  This is O(N^3) in the size
   of the array, due to the test for the triangle inequality, so we keep the
   array small.  */

static void
test_metric_conditions ()
{
  const int num_test_cases = sizeof (test_data) / sizeof (test_data[0]);

  for (int i = 0; i < num_test_cases; i++)
    {
      for (int j = 0; j < num_test_cases; j++)
	{
	  edit_distance_t dist_ij
	    = get_edit_distance (test_data[i], test_data[j]);

	  /* Identity of indiscernibles: d(i, j) > 0 iff i == j.  */
	  if (i == j)
	    ASSERT_EQ (dist_ij, 0);
	  else
	    ASSERT_TRUE (dist_ij > 0);

	  /* Symmetry: d(i, j) == d(j, i).  */
	  edit_distance_t dist_ji
	    = get_edit_distance (test_data[j], test_data[i]);
	  ASSERT_EQ (dist_ij, dist_ji);

	  /* Triangle inequality.  */
	  for (int k = 0; k < num_test_cases; k++)
	    {
	      edit_distance_t dist_ik
		= get_edit_distance (test_data[i], test_data[k]);
	      edit_distance_t dist_jk
		= get_edit_distance (test_data[j], test_data[k]);
	      ASSERT_TRUE (dist_ik <= dist_ij + dist_jk);
	    }
	}
    }
}

/* Verify get_edit_distance for a variety of pairs of pre-canned
   inputs, comparing against known-good values.  */

void
spellcheck_c_tests ()
{
  test_get_edit_distance_unit ("", "nonempty", strlen ("nonempty"));
  test_get_edit_distance_unit ("saturday", "sunday", 3);
  test_get_edit_distance_unit ("foo", "m_foo", 2);
  test_get_edit_distance_unit ("hello_world", "HelloWorld", 3);
  test_get_edit_distance_unit
    ("the quick brown fox jumps over the lazy dog", "dog", 40);
  test_get_edit_distance_unit
    ("the quick brown fox jumps over the lazy dog",
     "the quick brown dog jumps over the lazy fox",
     4);
  test_get_edit_distance_unit
    ("Lorem ipsum dolor sit amet, consectetur adipiscing elit,",
     "All your base are belong to us",
     44);
  test_get_edit_distance_unit ("foo", "FOO", 3);
  test_get_edit_distance_unit ("fee", "deed", 2);
  test_get_edit_distance_unit ("coorzd1", "coordx1", 2);

  /* Examples where transposition helps.  */
  test_get_edit_distance_unit ("ab", "ba", 1);
  test_get_edit_distance_unit ("ba", "abc", 2);
  test_get_edit_distance_unit ("coorzd1", "coordz1", 1);
  test_get_edit_distance_unit ("abcdefghijklmnopqrstuvwxyz",
			       "bacdefghijklmnopqrstuvwxzy", 2);
  test_get_edit_distance_unit ("saturday", "sundya", 4);
  test_get_edit_distance_unit ("signed", "singed", 1);

  test_find_closest_string ();
  test_metric_conditions ();
}

} // namespace selftest

#endif /* #if CHECKING_P */
