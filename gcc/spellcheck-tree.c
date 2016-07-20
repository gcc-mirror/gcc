/* Find near-matches for identifiers.
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
#include "tree.h"
#include "cpplib.h"
#include "spellcheck-tree.h"
#include "selftest.h"
#include "stringpool.h"

/* Calculate Levenshtein distance between two identifiers.  */

edit_distance_t
levenshtein_distance (tree ident_s, tree ident_t)
{
  gcc_assert (TREE_CODE (ident_s) == IDENTIFIER_NODE);
  gcc_assert (TREE_CODE (ident_t) == IDENTIFIER_NODE);

  return levenshtein_distance (IDENTIFIER_POINTER (ident_s),
			       IDENTIFIER_LENGTH (ident_s),
			       IDENTIFIER_POINTER (ident_t),
			       IDENTIFIER_LENGTH (ident_t));
}

/* Given TARGET, an identifier, and CANDIDATES, a vec of identifiers,
   determine which element within CANDIDATES has the lowest edit
   distance to TARGET.  If there are multiple elements with the
   same minimal distance, the first in the vector wins.

   If more than half of the letters were misspelled, the suggestion is
   likely to be meaningless, so return NULL_TREE for this case.  */

tree
find_closest_identifier (tree target, const auto_vec<tree> *candidates)
{
  gcc_assert (TREE_CODE (target) == IDENTIFIER_NODE);

  best_match<tree, tree> bm (target);
  int i;
  tree identifier;
  FOR_EACH_VEC_ELT (*candidates, i, identifier)
    {
      gcc_assert (TREE_CODE (identifier) == IDENTIFIER_NODE);
      bm.consider (identifier);
    }

  return bm.get_best_meaningful_candidate ();
}

/* A callback for cpp_forall_identifiers, for use by best_macro_match's ctor.
   Process HASHNODE and update the best_macro_match instance pointed to be
   USER_DATA.  */

static int
find_closest_macro_cpp_cb (cpp_reader *, cpp_hashnode *hashnode,
			   void *user_data)
{
  if (hashnode->type != NT_MACRO)
    return 1;

  best_macro_match *bmm = (best_macro_match *)user_data;
  bmm->consider (hashnode);

  /* Keep iterating.  */
  return 1;
}

/* Constructor for best_macro_match.
   Use find_closest_macro_cpp_cb to find the closest matching macro to
   NAME within distance < best_distance_so_far. */

best_macro_match::best_macro_match (tree goal,
				    edit_distance_t best_distance_so_far,
				    cpp_reader *reader)
  : best_match (goal, best_distance_so_far)
{
  cpp_forall_identifiers (reader, find_closest_macro_cpp_cb, this);
}

#if CHECKING_P

namespace selftest {

/* Selftests.  */

/* Verify that find_closest_identifier is sane.  */

static void
test_find_closest_identifier ()
{
  auto_vec<tree> candidates;

  /* Verify that it can handle an empty vec.  */
  ASSERT_EQ (NULL, find_closest_identifier (get_identifier (""), &candidates));

  /* Verify that it works sanely for non-empty vecs.  */
  tree apple = get_identifier ("apple");
  tree banana = get_identifier ("banana");
  tree cherry = get_identifier ("cherry");
  candidates.safe_push (apple);
  candidates.safe_push (banana);
  candidates.safe_push (cherry);

  ASSERT_EQ (apple, find_closest_identifier (get_identifier ("app"),
					     &candidates));
  ASSERT_EQ (banana, find_closest_identifier (get_identifier ("banyan"),
					      &candidates));;
  ASSERT_EQ (cherry, find_closest_identifier (get_identifier ("berry"),
					      &candidates));
  ASSERT_EQ (NULL,
	     find_closest_identifier (get_identifier ("not like the others"),
				      &candidates));
}

/* Run all of the selftests within this file.  */

void
spellcheck_tree_c_tests ()
{
  test_find_closest_identifier ();
}

} // namespace selftest

#endif /* #if CHECKING_P */
