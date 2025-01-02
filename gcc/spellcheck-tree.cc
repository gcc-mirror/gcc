/* Find near-matches for identifiers.
   Copyright (C) 2015-2025 Free Software Foundation, Inc.

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

/* Calculate edit distance between two identifiers.  */

edit_distance_t
get_edit_distance (tree ident_s, tree ident_t)
{
  gcc_assert (TREE_CODE (ident_s) == IDENTIFIER_NODE);
  gcc_assert (TREE_CODE (ident_t) == IDENTIFIER_NODE);

  return get_edit_distance (IDENTIFIER_POINTER (ident_s),
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
					      &candidates));
  ASSERT_EQ (cherry, find_closest_identifier (get_identifier ("berry"),
					      &candidates));
  ASSERT_EQ (NULL,
	     find_closest_identifier (get_identifier ("not like the others"),
				      &candidates));
}

/* Run all of the selftests within this file.  */

void
spellcheck_tree_cc_tests ()
{
  test_find_closest_identifier ();
}

} // namespace selftest

#endif /* #if CHECKING_P */
