/* m2tree.h header file for m2tree.c.

Copyright (C) 2012-2021 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#if !defined(m2tree_h)
#define m2tree_h
#if defined(m2tree_c)
#if defined(__GNUG__)
#define EXTERN extern "C"
#else /* !__GNUG__.  */
#define EXTERN
#endif /* !__GNUG__.  */
#else /* m2tree_c.  */
#if defined(__GNUG__)
#define EXTERN extern "C"
#else /* !__GNUG__.  */
#define EXTERN extern
#endif /* !__GNUG__.  */
#endif /* m2tree_c.  */

#include "input.h"

EXTERN int m2tree_is_var (tree var);
EXTERN int m2tree_is_array (tree array);
EXTERN int m2tree_is_type (tree type);
EXTERN tree m2tree_skip_type_decl (tree type);
EXTERN tree m2tree_skip_const_decl (tree exp);
EXTERN int m2tree_IsTreeOverflow (tree value);
EXTERN int m2tree_IsOrdinal (tree type);
EXTERN int m2tree_IsAConstant (tree t);
EXTERN void m2tree_debug_tree (tree t);
EXTERN tree m2tree_skip_reference_type (tree exp);


#ifndef SET_WORD_SIZE
/* gross hack.  */
#define SET_WORD_SIZE INT_TYPE_SIZE
#endif /* SET_WORD_SIZE.  */

#undef EXTERN
#endif /* m2tree_h.  */
