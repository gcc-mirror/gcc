/* Subroutines for targets on Windows.
Copyright (C) 2024-2025 Free Software Foundation, Inc.

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
http://www.gnu.org/licenses/.  */

#ifndef GCC_MINGW_WINNT_H
#define GCC_MINGW_WINNT_H

#ifndef USED_FOR_TARGET

extern tree mingw_handle_selectany_attribute (tree *, tree, tree, int, bool *);

extern void mingw_pe_asm_named_section (const char *, unsigned int, tree);
extern void mingw_pe_asm_lto_start (void);
extern void mingw_pe_asm_lto_end (void);
extern void mingw_pe_declare_type (FILE *, const char *, bool, bool);
extern void mingw_pe_encode_section_info (tree, rtx, int);
extern void mingw_pe_file_end (void);
extern void mingw_pe_maybe_record_exported_symbol (tree, const char *, int);
extern void mingw_pe_record_stub (const char *, bool);
extern unsigned int mingw_pe_section_type_flags (tree, const char *, int);
extern void mingw_pe_unique_section (tree, int);
extern bool mingw_pe_valid_dllimport_attribute_p (const_tree);

#endif /* not USED_FOR_TARGET.  */

#endif /* GCC_MINGW_WINNT_H.  */
