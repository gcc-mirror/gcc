/* Operating system specific prototypes to be used when targeting GCC for any
   Solaris 2 system.
   Copyright 2004, 2007, 2010, 2011, 2012 Free Software Foundation, Inc.

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

/* In sol2.c.  */
extern void solaris_assemble_visibility (tree, int);
extern void solaris_elf_asm_comdat_section (const char *, unsigned int, tree);
extern void solaris_file_end (void);
extern void solaris_insert_attributes (tree, tree *);
extern void solaris_output_init_fini (FILE *, tree);
extern void solaris_override_options (void);

/* In sol2-c.c.  */
extern void solaris_register_pragmas (void);

/* In sol2-cxx.c.  */
extern tree solaris_cxx_decl_mangling_context (const_tree);
