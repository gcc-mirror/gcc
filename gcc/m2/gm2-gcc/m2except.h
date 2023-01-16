/* m2except.h header file for m2except.cc.

Copyright (C) 2012-2023 Free Software Foundation, Inc.
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

#if !defined(m2except_h)
#define m2except_h
#if defined(m2except_c)
#if defined(__GNUG__)
#define EXTERN extern "C"
#else /* !__GNUG__.  */
#define EXTERN
#endif /* !__GNUG__.  */
#else /* !m2except_c.  */
#if defined(__GNUG__)
#define EXTERN extern "C"
#else /* !__GNUG__.  */
#define EXTERN extern
#endif /* !__GNUG__.  */
#endif /* !m2except_c.  */

/* InitExceptions - initialize this module, it declares the external
   functions and assigns them to the appropriate global tree
   variables.  */

EXTERN void m2except_InitExceptions (location_t location);

/* BuildThrow - builds a throw statement and return the tree.  */

EXTERN tree m2except_BuildThrow (location_t location, tree exp);

/* BuildTryBegin - returns a tree representing the 'try' block.  */

EXTERN tree m2except_BuildTryBegin (location_t location);

/* BuildTryEnd - builds the end of the Try block and prepares for the
   catch handlers.  */

EXTERN void m2except_BuildTryEnd (tree tryBlock);

/* BuildCatchBegin - creates a handler tree for the C++ statement
   'catch (...) {'.  It returns the handler tree.  */

EXTERN tree m2except_BuildCatchBegin (location_t location);

/* BuildCatchEnd - completes a try catch block.  It returns the,
   try_block, tree.  It creates the C++ statement

'}' which matches the catch above.  */

EXTERN tree m2except_BuildCatchEnd (location_t location, tree handler,
                                    tree tryBlock);

#endif  /* m2except_h.  */
