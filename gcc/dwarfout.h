/* dwarfout.h - Various declarations for functions found in dwarfout.c
   Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.

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

extern void dwarfout_init 		PARAMS ((FILE *asm_out_file, 
						char *main_input_filename));
extern void dwarfout_finish		PARAMS ((void));

extern void dwarfout_define		PARAMS ((unsigned, const char *));
extern void dwarfout_undef 		PARAMS ((unsigned, const char *));
extern void dwarfout_file_scope_decl 	PARAMS ((tree , int));
extern void dwarfout_start_new_source_file PARAMS ((const char *));
extern void dwarfout_resume_previous_source_file PARAMS ((unsigned));

extern void dwarfout_begin_function	PARAMS ((void));
extern void dwarfout_end_function	PARAMS ((void));
extern void dwarfout_begin_epilogue	PARAMS ((void));
extern void dwarfout_end_epilogue	PARAMS ((void));
extern void dwarfout_begin_block	PARAMS ((unsigned));
extern void dwarfout_end_block		PARAMS ((unsigned));

#ifdef RTX_CODE
extern void dwarfout_label		PARAMS ((rtx));
#endif
extern void dwarfout_line		PARAMS ((const char *, unsigned));

