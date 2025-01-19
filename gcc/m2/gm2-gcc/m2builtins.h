/* m2builtins.h header file for m2builtins.cc.

Copyright (C) 2012-2025 Free Software Foundation, Inc.
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

#if !defined(m2builtins_h)

#define m2builtins_h
#if defined(m2builtins_c)
#if defined(__GNUG__)
#define EXTERN extern "C"
#else /* !__GNUG__.  */
#define EXTERN
#endif /* !__GNUG__.  */
#else /* !m2builtins_c.  */
#if defined(__GNUG__)
#define EXTERN extern "C"
#else /* !__GNUG__.  */
#define EXTERN extern
#endif /* !__GNUG__.  */
#endif /* !m2builtins_c.  */

#include <stdbool.h>

EXTERN tree m2builtins_GetBuiltinConst (char *name);
EXTERN unsigned int m2builtins_GetBuiltinConstType (char *name);
EXTERN unsigned int m2builtins_GetBuiltinTypeInfoType (const char *ident);
EXTERN tree m2builtins_GetBuiltinTypeInfo (location_t location, tree type,
                                           const char *ident);
EXTERN tree m2builtins_BuiltinMemCopy (location_t location, tree dest,
                                       tree src, tree n);
EXTERN tree m2builtins_BuiltinMemSet (location_t location, tree dest,
				      tree bytevalue, tree nbytes);
EXTERN tree m2builtins_BuiltInAlloca (location_t location, tree n);
EXTERN tree m2builtins_BuiltInIsfinite (location_t location, tree e);
EXTERN bool m2builtins_BuiltinExists (char *name);
EXTERN tree m2builtins_BuildBuiltinTree (location_t location, char *name);
EXTERN tree m2builtins_BuiltInHugeVal (location_t location);
EXTERN tree m2builtins_BuiltInHugeValShort (location_t location);
EXTERN tree m2builtins_BuiltInHugeValLong (location_t location);
EXTERN void m2builtins_init (location_t location);

#undef EXTERN
#endif /* m2builtins_h.  */
