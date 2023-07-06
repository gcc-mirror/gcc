/* LTO declarations.
   Copyright (C) 2009-2023 Free Software Foundation, Inc.
   Contributed by CodeSourcery, Inc.

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

#ifndef LTO_H
#define LTO_H

/* In lto-lang.cc  */
extern const char *resolution_file_name;

/* In lto.cc  */
extern tree lto_eh_personality (void);
extern void lto_main (void);
extern void lto_read_all_file_options (void);

extern int lto_link_dump_id, decl_merge_dump_id, partition_dump_id;

extern unsigned int lto_option_lang_mask (void);

#endif /* LTO_H */
