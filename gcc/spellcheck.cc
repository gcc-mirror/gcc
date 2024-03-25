/* Find near-matches for strings.
   Copyright (C) 2015-2024 Free Software Foundation, Inc.

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

/* Cost of a case transformation.  */
#define CASE_COST 1

/* Cost of another kind of edit.  */
#define BASE_COST 2

/* Get the edit distance between the two strings: the minimal
   number of edits that are needed to change one string into another,
   where edits can be one-character insertions, removals, or substitutions,
   or transpositions of two adjacent characters (counting as one "edit").

   This implementation uses a modified variant of the Wagner-Fischer
   algorithm for the Damerau-Levenshtein distance; specifically, the
   "optimal string alignment distance" or "restricted edit distance"
   variant.  This implementation has been further modified to take
   case into account.  */

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
    return BASE_COST * len_t;
  if (len_t == 0)
    return BASE_COST * len_s;

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
    v_one_ago[i] = i * BASE_COST;

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
      v_next[0] = (i + 1) * BASE_COST;

      /* Build the rest of the row by considering neighbors to
	 the north, west and northwest.  */
      for (int j = 0; j < len_s; j++)
	{
	  edit_distance_t cost;

	  if (s[j] == t[i])
	    cost = 0;
	  else if (TOLOWER (s[j]) == TOLOWER (t[i]))
	    cost = CASE_COST;
	  else
	    cost = BASE_COST;
	  edit_distance_t deletion     = v_next[j] + BASE_COST;
	  edit_distance_t insertion    = v_one_ago[j + 1] + BASE_COST;
	  edit_distance_t substitution = v_one_ago[j] + cost;
	  edit_distance_t cheapest = MIN (deletion, insertion);
	  cheapest = MIN (cheapest, substitution);
	  if (i > 0 && j > 0 && s[j] == t[i - 1] && s[j - 1] == t[i])
	    {
	      edit_distance_t transposition = v_two_ago[j - 1] + BASE_COST;
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

/* Generate the maximum edit distance for which we consider a suggestion
   to be meaningful, given a goal of length GOAL_LEN and a candidate of
   length CANDIDATE_LEN.

   This is a third of the length of the candidate or of the goal,
   whichever is bigger.  */

edit_distance_t
get_edit_distance_cutoff (size_t goal_len, size_t candidate_len)
{
  size_t max_length = MAX (goal_len, candidate_len);
  size_t min_length = MIN (goal_len, candidate_len);

  gcc_assert (max_length >= min_length);

  /* Special case: don't offer suggestions for a pair of
     length == 1 strings (or empty strings).  */
  if (max_length <= 1)
    return 0;

  /* If the lengths are close, then round down.  */
  if (max_length - min_length <= 1)
    /* ...but allow an edit distance of at least 1.  */
    return BASE_COST * MAX (max_length / 3, 1);

  /* Otherwise, round up (thus giving a little extra leeway to some cases
     involving insertions/deletions).  */
  return BASE_COST * (max_length + 2) / 3;
}

#if CHECKING_P

namespace selftest {

/* Selftests.  */

/* Verify that get_edit_distance (A, B) equals the expected value.  */

static void
test_get_edit_distance_one_way (const char *a, const char *b,
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
test_get_edit_distance_both_ways (const char *a, const char *b,
			     edit_distance_t expected)
{
  test_get_edit_distance_one_way (a, b, expected);
  test_get_edit_distance_one_way (b, a, expected);
}

/* Verify get_edit_distance for a variety of pairs of pre-canned
   inputs, comparing against known-good values.  */

static void
test_edit_distances ()
{
  test_get_edit_distance_both_ways ("", "nonempty",
				    BASE_COST * strlen ("nonempty"));
  test_get_edit_distance_both_ways ("saturday", "sunday",
				    BASE_COST * 3);
  test_get_edit_distance_both_ways ("foo", "m_foo", BASE_COST * 2);
  test_get_edit_distance_both_ways ("hello_world", "HelloWorld", 4);
  test_get_edit_distance_both_ways
    ("the quick brown fox jumps over the lazy dog", "dog", BASE_COST * 40);
  test_get_edit_distance_both_ways
    ("the quick brown fox jumps over the lazy dog",
     "the quick brown dog jumps over the lazy fox",
     BASE_COST * 4);
  test_get_edit_distance_both_ways
    ("Lorem ipsum dolor sit amet, consectetur adipiscing elit,",
     "All your base are belong to us",
     BASE_COST * 44);
  test_get_edit_distance_both_ways ("foo", "FOO", 3);
  test_get_edit_distance_both_ways ("fee", "deed", BASE_COST * 2);
  test_get_edit_distance_both_ways ("coorzd1", "coordx1", BASE_COST * 2);
  test_get_edit_distance_both_ways ("assert", "sqrt", BASE_COST * 3);
  test_get_edit_distance_both_ways ("PATH_MAX", "INT8_MAX", BASE_COST * 3);
  test_get_edit_distance_both_ways ("time", "nice", BASE_COST * 2);
  test_get_edit_distance_both_ways ("bar", "carg", BASE_COST * 2);
  test_get_edit_distance_both_ways ("gtk_widget_show_all",
				    "GtkWidgetShowAll", 10);
  test_get_edit_distance_both_ways ("m_bar", "bar", BASE_COST * 2);
  test_get_edit_distance_both_ways ("MACRO", "MACRAME", BASE_COST * 3);
  test_get_edit_distance_both_ways ("ab", "ac", BASE_COST * 1);
  test_get_edit_distance_both_ways ("ab", "a", BASE_COST * 1);
  test_get_edit_distance_both_ways ("a", "b", BASE_COST * 1);
  test_get_edit_distance_both_ways ("nanl", "name", BASE_COST * 2);
  test_get_edit_distance_both_ways ("char", "bar", BASE_COST * 2);
  test_get_edit_distance_both_ways ("-optimize", "fsanitize", BASE_COST * 5);
  test_get_edit_distance_both_ways ("__DATE__", "__i386__", BASE_COST * 4);

  /* Examples where transposition helps.  */
  test_get_edit_distance_both_ways ("ab", "ba", BASE_COST * 1);
  test_get_edit_distance_both_ways ("ba", "abc", BASE_COST * 2);
  test_get_edit_distance_both_ways ("coorzd1", "coordz1", BASE_COST * 1);
  test_get_edit_distance_both_ways ("abcdefghijklmnopqrstuvwxyz",
				    "bacdefghijklmnopqrstuvwxzy",
				    BASE_COST * 2);
  test_get_edit_distance_both_ways ("saturday", "sundya", BASE_COST * 4);
  test_get_edit_distance_both_ways ("signed", "singed", BASE_COST * 1);
}

/* Subroutine of test_get_edit_distance_cutoff, for emulating the
   spellchecking cutoff in up to GCC 8.  */

static edit_distance_t
get_old_cutoff (size_t goal_len, size_t candidate_len)
{
  return BASE_COST * MAX (goal_len, candidate_len) / 2;
}

/* Verify that the cutoff for "meaningfulness" of suggestions is at least as
   conservative as in older GCC releases.

   This should ensure that we don't offer additional meaningless
   suggestions (apart from those for which transposition has helped).  */

static void
test_get_edit_distance_cutoff ()
{
  for (size_t goal_len = 0; goal_len < 30; goal_len++)
    for (size_t candidate_len = 0; candidate_len < 30; candidate_len++)
      ASSERT_TRUE (get_edit_distance_cutoff (goal_len, candidate_len)
		   <= get_old_cutoff (goal_len, candidate_len));
}

/* Assert that CANDIDATE is offered as a suggestion for TARGET.  */

static void
assert_suggested_for (const location &loc, const char *candidate,
		      const char *target)
{
  auto_vec<const char *> candidates;
  candidates.safe_push (candidate);
  ASSERT_EQ_AT (loc, candidate, find_closest_string (target, &candidates));
}

/* Assert that CANDIDATE is offered as a suggestion for TARGET.  */

#define ASSERT_SUGGESTED_FOR(CANDIDATE, TARGET)			\
  SELFTEST_BEGIN_STMT							\
    assert_suggested_for (SELFTEST_LOCATION, CANDIDATE, TARGET);	\
  SELFTEST_END_STMT

/* Assert that CANDIDATE is not offered as a suggestion for TARGET.  */

static void
assert_not_suggested_for (const location &loc, const char *candidate,
			  const char *target)
{
  auto_vec<const char *> candidates;
  candidates.safe_push (candidate);
  ASSERT_EQ_AT (loc, NULL, find_closest_string (target, &candidates));
}

/* Assert that CANDIDATE is not offered as a suggestion for TARGET.  */

#define ASSERT_NOT_SUGGESTED_FOR(CANDIDATE, TARGET)			\
  SELFTEST_BEGIN_STMT							\
    assert_not_suggested_for (SELFTEST_LOCATION, CANDIDATE, TARGET);	\
  SELFTEST_END_STMT

/* Verify that we offer varous suggestions that are meaningful,
   and that we don't offer various other ones that aren't (PR c/82967).  */

static void
test_suggestions ()
{
  /* Good suggestions.  */

  ASSERT_SUGGESTED_FOR ("m_bar", "bar");
  // dist == 2, max_length == 5, min_length == 3

  ASSERT_SUGGESTED_FOR ("MACRO", "MACRAME");
  // dist == 3, max_length == 7, min_length == 5

  ASSERT_SUGGESTED_FOR ("gtk_widget_show_all", "GtkWidgetShowAll");
  // dist == 7, max_length == 16, min_length = 19

  ASSERT_SUGGESTED_FOR ("ab", "ac");
  // dist == 1, max_length == min_length = 2

  ASSERT_SUGGESTED_FOR ("ab", "a");
  // dist == 1, max_length == 2, min_length = 1

  /* Bad suggestions.  */

  ASSERT_NOT_SUGGESTED_FOR ("a", "b");
  // dist == 1, max_length == min_length = 1

  ASSERT_NOT_SUGGESTED_FOR ("sqrt", "assert");
  // dist == 3, max_length 6, min_length == 4

  ASSERT_NOT_SUGGESTED_FOR ("INT8_MAX", "PATH_MAX");
  // dist == 3, max_length == min_length == 8

  ASSERT_NOT_SUGGESTED_FOR ("nice", "time");
  ASSERT_NOT_SUGGESTED_FOR ("nanl", "name");
  // dist == 2, max_length == min_length == 4

  ASSERT_NOT_SUGGESTED_FOR ("carg", "bar");
  ASSERT_NOT_SUGGESTED_FOR ("char", "bar");
  // dist == 2, max_length == 4, min_length == 3

  ASSERT_NOT_SUGGESTED_FOR ("-optimize", "fsanitize");
  // dist == 5, max_length == min_length == 9

  ASSERT_NOT_SUGGESTED_FOR ("__DATE__", "__i386__");
  // dist == 4, max_length == min_length == 8

  ASSERT_NOT_SUGGESTED_FOR ("start_input_device", "InputDevice");
  // dist == 9, max_length == 18, min_length == 11
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

  candidates.truncate (0);
  candidates.safe_push ("DWARF_GNAT_ENCODINGS_GDB");
  candidates.safe_push ("DWARF_GNAT_ENCODINGS_ALL");
  candidates.safe_push ("DWARF_GNAT_ENCODINGS_MINIMAL");
  ASSERT_STREQ ("DWARF_GNAT_ENCODINGS_ALL",
		find_closest_string ("DWARF_GNAT_ENCODINGS_all",
				     &candidates));

  /* The same as the previous test, but with a different order of
     candidates.  */
  candidates.truncate (0);
  candidates.safe_push ("DWARF_GNAT_ENCODINGS_ALL");
  candidates.safe_push ("DWARF_GNAT_ENCODINGS_GDB");
  candidates.safe_push ("DWARF_GNAT_ENCODINGS_MINIMAL");
  ASSERT_STREQ ("DWARF_GNAT_ENCODINGS_ALL",
		find_closest_string ("DWARF_GNAT_ENCODINGS_all",
				     &candidates));

  /* Example from PR 105564 where option name with missing equal
     sign should win.  */
  candidates.truncate (0);
  candidates.safe_push ("-Wtrivial-auto-var-init");
  candidates.safe_push ("-ftrivial-auto-var-init=");
  ASSERT_STREQ ("-ftrivial-auto-var-init=",
		find_closest_string ("-ftrivial-auto-var-init",
				     &candidates));
}

/* Test data for test_metric_conditions.  */

static const char * const test_data[] = {
  "",
  "foo",
  "food",
  "boo",
  "1234567890123456789012345678901234567890123456789012345678901234567890",
  "abc",
  "ac",
  "ca",
};

/* Verify that get_edit_distance appears to be a sane distance function,
   even though it doesn't satisfy the conditions for being a metric.  (This
   is because the triangle inequality fails to hold: the distance between
   "ca" and "ac" is 1, and so is the distance between "abc" and "ac", but
   the distance between "abc" and "ca" is 3. Algorithms that calculate the
   true Levenshtein-Damerau metric are much more expensive.)  */

static void
test_metric_conditions ()
{
  const int num_test_cases = ARRAY_SIZE (test_data);

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
	}
    }
}

/* Run all of the selftests within this file.  */

void
spellcheck_cc_tests ()
{
  test_edit_distances ();
  test_get_edit_distance_cutoff ();
  test_suggestions ();
  test_find_closest_string ();
  test_metric_conditions ();
}

} // namespace selftest

#endif /* #if CHECKING_P */
