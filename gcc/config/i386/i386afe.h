/* Copyright (C) 2001 Free Software Foundation, Inc.

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
Boston, MA 02111-1307, USA. */

/* Irritatingly, config/elfos.h defines its own version of ASM_FILE_END,
   conflicting with a definition which we wish to have in i386/i386.h.
   We _really_ need to clean up the hodge-podge of random macro placement
   in the configury...  */

/* This macro is invoked at the end of compilation.  It is used here to
   output code for -fpic that will load the return address into %ebx.  */

#undef ASM_FILE_END
#define ASM_FILE_END(FILE)				\
  do							\
    {				 			\
      ix86_asm_file_end (FILE);				\
      if (!flag_no_ident)				\
	fprintf ((FILE), "%s\"GCC: (GNU) %s\"\n",	\
		 IDENT_ASM_OP, version_string);		\
    }							\
  while (0)
