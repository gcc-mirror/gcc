/* dbxout.h - Various declarations for functions found in dbxout.c
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

extern void dbxout_init 		PARAMS ((FILE *, const char *, tree));
extern void dbxout_finish		PARAMS ((FILE *, const char *));

extern void dbxout_start_new_source_file 	PARAMS ((const char *));
extern void dbxout_resume_previous_source_file	PARAMS ((void));

extern void dbxout_source_file		PARAMS ((FILE *, const char *));
extern void dbxout_types		PARAMS ((tree));
extern void dbxout_args			PARAMS ((tree));
extern int dbxout_symbol		PARAMS ((tree, int));
extern void dbxout_parms		PARAMS ((tree));
extern void dbxout_reg_parms		PARAMS ((tree));
extern int dbxout_syms			PARAMS ((tree));
extern void dbxout_function		PARAMS ((tree));
extern void dbxout_source_line		PARAMS ((FILE *, const char *, int));
extern void dbxout_begin_function	PARAMS ((tree));
