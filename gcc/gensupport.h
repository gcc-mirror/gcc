/* Declarations for rtx-reader support for gen* routines.
   Copyright (C) 2000 Free Software Foundation, Inc.

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

struct obstack;
extern struct obstack *rtl_obstack;

extern int init_md_reader_args	PARAMS ((int, char **));
extern int init_md_reader	PARAMS ((const char *));
extern rtx read_md_rtx		PARAMS ((int *, int *));

extern void message_with_line	PARAMS ((int, const char *, ...))
     ATTRIBUTE_PRINTF_2;
