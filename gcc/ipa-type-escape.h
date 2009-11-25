/* Type based alias analysis.
   Copyright (C) 2004, 2007, 2008 Free Software Foundation, Inc.
   Contributed by Kenneth Zadeck <zadeck@naturalbridge.com>

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

#ifndef GCC_IPA_TYPE_ESCAPE_H
#define GCC_IPA_TYPE_ESCAPE_H
#include "tree.h"

bool   ipa_type_escape_type_contained_p (tree type);
bool   ipa_type_escape_field_does_not_clobber_p (tree, tree);
int    ipa_type_escape_star_count_of_interesting_type (tree type);
int    ipa_type_escape_star_count_of_interesting_or_array_type (tree type);
bool   is_array_access_through_pointer_and_index (enum tree_code, tree, tree,
						  tree *, tree *, gimple *);


#endif  /* GCC_IPA_TYPE_ESCAPE_H  */

