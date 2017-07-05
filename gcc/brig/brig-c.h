/* brig-c.h -- Header file for brig input's gcc C interface.
   Copyright (C) 2016-2017 Free Software Foundation, Inc.
   Contributed by Pekka Jaaskelainen <pekka.jaaskelainen@parmance.com>
   for General Processor Tech.

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

#ifndef BRIG_BRIG_C_H
#define BRIG_BRIG_C_H

#define BRIG_EXTERN_C

#include "machmode.h"

/* Functions defined in the Brig frontend proper called by the GCC
   interface.  */

extern int brig_enable_dump (const char *);
extern int brig_enable_optimize (const char *);

extern void brig_add_search_path (const char *);

extern void brig_create_brigbrig (int int_type_size, int pointer_size,
				  const char *pkgpath, const char *prefix,
				  const char *relative_import_path);

extern void brig_parse_input_files (const char **, unsigned int,
				    bool only_check_syntax,
				    bool require_return_statement);
extern void brig_write_globals (void);

extern tree brig_type_for_size (unsigned int bits, int unsignedp);
extern tree brig_type_for_mode (machine_mode, int unsignedp);

/* Functions defined in the GCC interface called by the Brig frontend
   proper.  */

extern void brig_preserve_from_gc (tree);

extern const char *brig_localize_identifier (const char *);

extern unsigned int brig_field_alignment (tree);

extern void brig_trampoline_info (unsigned int *size, unsigned int *alignment);

extern void brig_imported_unsafe (void);

extern void brig_write_export_data (const char *, unsigned int);

extern const char *brig_read_export_data (int, off_t, char **, size_t *, int *);

#endif /* !defined (BRIG_BRIG_C_H) */
