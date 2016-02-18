/* Find near-matches for strings and identifiers.
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

#ifndef GCC_SPELLCHECK_H
#define GCC_SPELLCHECK_H

typedef unsigned int edit_distance_t;
const edit_distance_t MAX_EDIT_DISTANCE = UINT_MAX;

/* spellcheck.c  */
extern edit_distance_t
levenshtein_distance (const char *s, int len_s,
		      const char *t, int len_t);

extern edit_distance_t
levenshtein_distance (const char *s, const char *t);

extern const char *
find_closest_string (const char *target,
		     const auto_vec<const char *> *candidates);

/* spellcheck-tree.c  */

extern edit_distance_t
levenshtein_distance (tree ident_s, tree ident_t);

extern tree
find_closest_identifier (tree target, const auto_vec<tree> *candidates);

#endif  /* GCC_SPELLCHECK_H  */
