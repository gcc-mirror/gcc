/* sdbout.h - Various declarations for functions found in sdbout.c
   Copyright (C) 1998, 2000 Free Software Foundation, Inc.

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

extern void sdbout_init			PARAMS ((FILE *, char*, tree));

extern void sdbout_begin_function	PARAMS ((int));
extern void sdbout_end_function		PARAMS ((int));

extern void sdbout_begin_block		PARAMS ((FILE *, int, int));
extern void sdbout_end_block		PARAMS ((FILE *, int, int));

extern void sdbout_label		PARAMS ((rtx));
extern void sdbout_symbol		PARAMS ((tree, int));
extern void sdbout_toplevel_data	PARAMS ((tree));
extern void sdbout_types		PARAMS ((tree));

extern void sdbout_end_epilogue		PARAMS ((void));

extern void sdbout_start_new_source_file 	PARAMS ((char *));
extern void sdbout_resume_previous_source_file	PARAMS ((void));
extern void sdbout_mark_begin_function	PARAMS ((void));

