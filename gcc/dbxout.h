/* dbxout.h - Various declarations for functions found in dbxout.c
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

extern void dbxout_init 		PROTO ((FILE *, char *, tree));
extern void dbxout_finish		PROTO ((FILE *, char *));

extern void dbxout_start_new_source_file 	PROTO ((char *));
extern void dbxout_resume_previous_source_file	PROTO ((void));

extern void dbxout_symbol		PROTO ((tree, int));
extern void dbxout_parms		PROTO ((tree));
extern void dbxout_reg_parms		PROTO ((tree));
extern void dbxout_syms			PROTO ((tree));
extern void dbxout_function		PROTO ((tree));
extern void dbxout_source_line		PROTO ((FILE *, char*, int));
extern void dbxout_begin_function	PROTO ((tree));
