/* go-c.h -- Header file for go frontend gcc C interface.
   Copyright (C) 2009, 2010, 2011, 2012 Free Software Foundation, Inc.

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

#ifdef ENABLE_BUILD_WITH_CXX
#define GO_EXTERN_C
#else
#define GO_EXTERN_C extern "C"
#endif

#if defined(__cplusplus) && !defined(ENABLE_BUILD_WITH_CXX)
extern "C"
{
#endif

#include "machmode.h"

/* Functions defined in the Go frontend proper called by the GCC
   interface.  */

extern int go_enable_dump (const char*);
extern int go_enable_optimize (const char*);

extern void go_add_search_path (const char*);

extern void go_create_gogo (int int_type_size, int pointer_size,
			    const char* pkgpath, const char *prefix,
			    const char *relative_import_path);

extern void go_parse_input_files (const char**, unsigned int,
				  bool only_check_syntax,
				  bool require_return_statement);
extern void go_write_globals (void);

extern tree go_type_for_size (unsigned int bits, int unsignedp);
extern tree go_type_for_mode (enum machine_mode, int unsignedp);

/* Functions defined in the GCC interface called by the Go frontend
   proper.  */

extern void go_preserve_from_gc (tree);

extern const char *go_localize_identifier (const char*);

extern unsigned int go_field_alignment (tree);

extern void go_trampoline_info (unsigned int *size, unsigned int *alignment);

extern void go_imported_unsafe (void);

extern void go_write_export_data (const char *, unsigned int);

extern const char *go_read_export_data (int, off_t, char **, size_t *, int *);

extern GTY(()) tree go_non_zero_struct;

#if defined(__cplusplus) && !defined(ENABLE_BUILD_WITH_CXX)
} /* End extern "C".  */
#endif

#endif /* !defined(GO_GO_C_H) */
