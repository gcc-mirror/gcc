/* dwarf2out.h - Various declarations for functions found in dwarf2out.c
   Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

extern void dwarf2out_decl		PARAMS ((tree));	
extern void dwarf2out_frame_debug	PARAMS ((rtx));

extern void debug_dwarf			PARAMS ((void));
struct die_struct;
extern void debug_dwarf_die		PARAMS ((struct die_struct *));
extern void dwarf2out_set_demangle_name_func PARAMS ((const char *(*) (const char *)));
extern void dwarf2out_add_library_unit_info PARAMS ((const char *, const char *));
