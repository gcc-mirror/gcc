/* Definitions for IBM PS2 running AIX/386 with gas.
   From: Minh Tran-Le <TRANLE@intellicorp.com>
   Copyright (C) 1988 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "i386/aix386ng.h"

/* Define a few machine-specific details of the implementation of
   constructors.  */

#undef INIT_SECTION_ASM_OP
#define INIT_SECTION_ASM_OP     ".section .init,\"x\""

#define CTOR_LIST_BEGIN				\
  asm (INIT_SECTION_ASM_OP);			\
  asm ("pushl $0")
#define CTOR_LIST_END CTOR_LIST_BEGIN

#define ASM_OUTPUT_CONSTRUCTOR(FILE,NAME)	\
  do {						\
    init_section ();				\
    fprintf (FILE, "\tpushl $");		\
    assemble_name (FILE, NAME);			\
    fprintf (FILE, "\n");			\
  } while (0)
