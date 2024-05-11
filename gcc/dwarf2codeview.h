/* dwarf2codeview.h - DWARF interface for CodeView generation.
   Copyright (C) 2023 Free Software Foundation, Inc.

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

#ifndef GCC_DWARF2CODEVIEW_H
#define GCC_DWARF2CODEVIEW_H 1

#include "dwarf2out.h"
#include "flags.h"

/* Debug Format Interface.  Used in dwarf2out.cc.  */

extern void codeview_debug_finish (void);
extern void codeview_source_line (unsigned int, const char *);
extern void codeview_start_source_file (const char *);
extern void codeview_switch_text_section ();
extern void codeview_end_epilogue (void);

#endif /* GCC_DWARF2CODEVIEW_H */
