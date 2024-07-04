/* m2block.h header file for m2block.cc.

Copyright (C) 2012-2024 Free Software Foundation, Inc.
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

#if !defined(m2block_h)
#define m2block_h
#if defined(m2block_c)
#if defined(__GNUG__)
#define EXTERN extern "C"
#else /* !__GNUG__.  */
#define EXTERN
#endif /* !__GNUG__.  */
#else /* !m2block_c.  */
#if defined(__GNUG__)
#define EXTERN extern "C"
#else /* !m2block_h.  */
#define EXTERN extern
#endif /* !m2block_c.  */
#endif /* !m2block_h.  */

#include <stdbool.h>

EXTERN tree m2block_getLabel (location_t location, char *name);
EXTERN void m2block_pushFunctionScope (tree fndecl);
EXTERN tree m2block_popFunctionScope (void);
EXTERN void m2block_pushGlobalScope (void);
EXTERN void m2block_popGlobalScope (void);
EXTERN tree m2block_pushDecl (tree decl);
EXTERN void m2block_addDeclExpr (tree t);

EXTERN tree m2block_begin_statement_list (void);
EXTERN tree m2block_push_statement_list (tree t);
EXTERN tree m2block_pop_statement_list (void);

EXTERN void m2block_finishFunctionDecl (location_t location, tree fndecl);
EXTERN void m2block_finishFunctionCode (tree fndecl);

EXTERN tree m2block_RememberType (tree t);
EXTERN tree m2block_RememberConstant (tree t);
EXTERN tree m2block_DumpGlobalConstants (void);
EXTERN tree m2block_RememberInitModuleFunction (tree t);
EXTERN tree m2block_global_constant (tree t);
EXTERN bool m2block_toplevel (void);
EXTERN tree m2block_GetErrorNode (void);

EXTERN void m2block_addStmtNote (location_t location);

EXTERN tree m2block_cur_stmt_list (void);
EXTERN tree *m2block_cur_stmt_list_addr (void);
EXTERN int m2block_is_building_stmt_list (void);
EXTERN tree m2block_GetGlobals (void);
EXTERN tree m2block_GetGlobalContext (void);
EXTERN void m2block_finishGlobals (void);
EXTERN void m2block_includeDecl (tree);
EXTERN tree m2block_add_stmt (location_t location, tree t);
EXTERN void m2block_addStmtNote (location_t location);
EXTERN void m2block_removeStmtNote (void);

EXTERN void m2block_init (void);

#undef EXTERN
#endif /* m2block_h.  */
