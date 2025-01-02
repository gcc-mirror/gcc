/* LTO common functions between lto.cc and lto-dump.cc header file.
   Copyright (C) 2018-2025 Free Software Foundation, Inc.

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

#ifndef LTO_COMMON_H
#define LTO_COMMON_H

void lto_fe_init (void);
void read_cgraph_and_symbols (unsigned, const char **);
void print_lto_report_1 (void);

extern tree lto_eh_personality_decl;
extern GTY(()) vec<tree, va_gc> *tree_with_vars;
extern tree first_personality_decl;

#endif

