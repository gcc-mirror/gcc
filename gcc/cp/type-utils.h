/* Utilities for querying and manipulating type trees.
   Copyright (C) 2013-2020 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_CP_TYPE_UTILS_H
#define GCC_CP_TYPE_UTILS_H

/* Returns a pointer to the first tree within *TP that is directly matched by
   PRED.  *TP may be a type or PARM_DECL and is incrementally decomposed toward
   its type-specifier until a match is found.  NULL is returned if PRED does not
   match any part of *TP.

   This is primarily intended for detecting whether *TP uses `auto' or a concept
   identifier.  Since either of these can only appear as a type-specifier for
   the declaration in question, only top-level qualifications are traversed;
   find_type_usage does not look through the whole type.  */

inline tree *
find_type_usage (tree *tp, bool (*pred) (const_tree))
{
  tree t = *tp;
  if (pred (t))
    return tp;

  enum tree_code code = TREE_CODE (t);

  if (code == POINTER_TYPE || code == REFERENCE_TYPE
      || code == PARM_DECL || code == OFFSET_TYPE
      || code == FUNCTION_TYPE || code == METHOD_TYPE
      || code == ARRAY_TYPE)
    return find_type_usage (&TREE_TYPE (t), pred);

  if (TYPE_PTRMEMFUNC_P (t))
    return find_type_usage
      (&TREE_TYPE (TYPE_PTRMEMFUNC_FN_TYPE (t)), pred);

  return NULL;
}

#endif // GCC_CP_TYPE_UTILS_H
