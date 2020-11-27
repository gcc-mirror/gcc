/* rust-c.h -- Header file for rust frontend gcc C interface.
   Copyright (C) 2009-2019 Free Software Foundation, Inc.

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

#ifndef RUST_RUST_C_H
#define RUST_RUST_C_H

#define RUST_EXTERN_C

class Linemap;

extern Linemap* rust_get_linemap();

extern void rust_create_rustly(bool only_check_syntax, Linemap* linemap);

extern void rust_parse_input_files (const char**, unsigned int);

extern unsigned int rust_field_alignment (tree);

extern void rust_write_globals (void);

extern void rust_preserve_from_gc (tree);

extern bool saw_errors (void);

extern unsigned int rust_field_alignment (tree);

extern void rust_write_export_data (const char *, unsigned int);

#endif /* !defined(RUST_RUST_C_H) */
