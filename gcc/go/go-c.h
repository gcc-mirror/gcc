/* go-c.h -- Header file for go frontend gcc C interface.
   Copyright (C) 2009-2016 Free Software Foundation, Inc.

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

#ifndef GO_GO_C_H
#define GO_GO_C_H

#define GO_EXTERN_C

class Linemap;
class Backend;

/* Functions defined in the Go frontend proper called by the GCC
   interface.  */

extern int go_enable_dump (const char*);
extern int go_enable_optimize (const char*);

extern void go_add_search_path (const char*);

struct go_create_gogo_args
{
  int int_type_size;
  int pointer_size;
  const char* pkgpath;
  const char* prefix;
  const char* relative_import_path;
  const char* c_header;
  Backend* backend;
  Linemap* linemap;
  bool check_divide_by_zero;
  bool check_divide_overflow;
  bool compiling_runtime;
  int debug_escape_level;
};

extern void go_create_gogo (const struct go_create_gogo_args*);

extern void go_parse_input_files (const char**, unsigned int,
				  bool only_check_syntax,
				  bool require_return_statement);
extern void go_write_globals (void);

/* Functions defined in the GCC interface called by the Go frontend
   proper.  */

extern void go_preserve_from_gc (tree);

extern bool saw_errors (void);

extern const char *go_localize_identifier (const char*);

extern unsigned int go_field_alignment (tree);

extern void go_imported_unsafe (void);

extern void go_write_export_data (const char *, unsigned int);

extern const char *go_read_export_data (int, off_t, char **, size_t *, int *);

extern GTY(()) tree go_non_zero_struct;

#endif /* !defined(GO_GO_C_H) */
