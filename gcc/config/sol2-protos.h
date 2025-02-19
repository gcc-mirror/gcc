/* Operating system specific prototypes to be used when targeting GCC for any
   Solaris 2 system.
   Copyright (C) 2004-2025 Free Software Foundation, Inc.

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

/* In sol2.cc.  */
extern void solaris_assemble_visibility (tree, int);
extern void solaris_elf_asm_comdat_section (const char *, unsigned int, tree);
extern void solaris_file_end (void);
extern void solaris_insert_attributes (tree, tree *);
extern void solaris_output_init_fini (FILE *, tree);

/* In sol2-c.cc.  */
extern void solaris_register_pragmas (void);

/* In sol2-cxx.cc.  */
extern tree solaris_cxx_decl_mangling_context (const_tree);
