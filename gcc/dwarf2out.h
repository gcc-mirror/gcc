/* dwarf2out.h - Various declarations for functions found in dwarf2out.c
   Copyright (C) 1998 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

extern void dwarf2out_init 		PROTO ((FILE *asm_out_file, 
						char *main_input_filename));
extern void dwarf2out_finish		PROTO ((void));

extern void dwarf2out_define		PROTO ((unsigned, char *));
extern void dwarf2out_undef 		PROTO ((unsigned, char *));                                       
extern void dwarf2out_start_source_file 	PROTO ((char *));
extern void dwarf2out_end_source_file 	PROTO ((void));

extern void dwarf2out_begin_block	PROTO ((unsigned));
extern void dwarf2out_end_block		PROTO ((unsigned));
extern void dwarf2out_label		PROTO ((rtx));			
extern void dwarf2out_decl		PROTO ((tree));	
extern void dwarf2out_line		PROTO ((char *, unsigned));			
extern void dwarf2out_frame_init	PROTO ((void));
extern void dwarf2out_frame_debug	PROTO ((rtx));
extern void dwarf2out_frame_finish	PROTO ((void));

extern void debug_dwarf			PROTO ((void));
struct die_struct;
extern void debug_dwarf_die		PROTO ((struct die_struct *));
