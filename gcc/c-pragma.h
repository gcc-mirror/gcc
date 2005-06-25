/* Pragma related interfaces.
   Copyright (C) 1995, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

#ifndef GCC_C_PRAGMA_H
#define GCC_C_PRAGMA_H

#include <cpplib.h> /* For enum cpp_ttype.  */

/* Cause the `yydebug' variable to be defined.  */
#define YYDEBUG 1
extern int yydebug;

extern struct cpp_reader* parse_in;

#define HANDLE_PRAGMA_WEAK SUPPORTS_WEAK

#ifdef HANDLE_SYSV_PRAGMA
/* We always support #pragma pack for SYSV pragmas.  */
#ifndef HANDLE_PRAGMA_PACK
#define HANDLE_PRAGMA_PACK 1
#endif
#endif /* HANDLE_SYSV_PRAGMA */


#ifdef HANDLE_PRAGMA_PACK_PUSH_POP
/* If we are supporting #pragma pack(push... then we automatically
   support #pragma pack(<n>)  */
#define HANDLE_PRAGMA_PACK 1
#endif /* HANDLE_PRAGMA_PACK_PUSH_POP */

/* It's safe to always leave visibility pragma enabled as if
   visibility is not supported on the host OS platform the
   statements are ignored.  */
#define HANDLE_PRAGMA_VISIBILITY 1

extern void init_pragma (void);

/* Front-end wrappers for pragma registration to avoid dragging
   cpplib.h in almost everywhere.  */
extern void c_register_pragma (const char *, const char *,
			       void (*) (struct cpp_reader *));
extern void c_register_pragma_with_expansion (const char *, const char *,
					      void (*) (struct cpp_reader *));
extern void maybe_apply_pragma_weak (tree);
extern void maybe_apply_pending_pragma_weaks (void);
extern tree maybe_apply_renaming_pragma (tree, tree);
extern void add_to_renaming_pragma_list (tree, tree);

extern enum cpp_ttype c_lex (tree *);
extern enum cpp_ttype c_lex_with_flags (tree *, location_t *, unsigned char *);

/* If 1, then lex strings into the execution character set.  
   If 0, lex strings into the host character set.
   If -1, lex both, and chain them together, such that the former
   is the TREE_CHAIN of the latter.  */
extern int c_lex_string_translate;

/* If true, strings should be passed to the caller of c_lex completely
   unmolested (no concatenation, no translation).  */
extern bool c_lex_return_raw_strings;

#endif /* GCC_C_PRAGMA_H */
