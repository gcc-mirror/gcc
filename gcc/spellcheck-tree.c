/* Find near-matches for identifiers.
   Copyright (C) 2015 Free Software Foundation, Inc.

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
