/* Find near-matches for macros.
   Copyright (C) 2016-2017 Free Software Foundation, Inc.

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
#include "c-family/c-spellcheck.h"

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
: best_match <goal_t, candidate_t> (goal, best_distance_so_far)
{
  cpp_forall_identifiers (reader, find_closest_macro_cpp_cb, this);
}
