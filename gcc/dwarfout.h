/* dwarfout.h - Various declarations for functions found in dwarfout.c
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

extern void dwarfout_init 		PROTO ((FILE *asm_out_file, 
						char *main_input_filename));
extern void dwarfout_finish		PROTO ((void));

extern void dwarfout_define		PROTO ((unsigned, char *));
extern void dwarfout_undef 		PROTO ((unsigned, char *));                                       
extern void dwarfout_file_scope_decl 	PROTO ((tree , int));
extern void dwarfout_start_new_source_file 	PROTO ((char *));
extern void dwarfout_resume_previous_source_file	PROTO((unsigned));

extern void dwarfout_begin_function	PROTO ((void));
extern void dwarfout_end_function	PROTO ((void));
extern void dwarfout_begin_epilogue	PROTO ((void));
extern void dwarfout_end_epilogue	PROTO ((void));
extern void dwarfout_begin_block	PROTO ((unsigned));
extern void dwarfout_end_block		PROTO ((unsigned));

#ifdef RTX_CODE
extern void dwarfout_label		PROTO ((rtx));
#endif
extern void dwarfout_line		PROTO ((char *, unsigned));

