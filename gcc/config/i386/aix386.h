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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* 
 * This configuration file is for gcc with gas-2.x and gnu ld 2.x
 * with aix ps/2 1.3.x.
 */

/* Define USE_GAS if you have the new version of gas that can handle
 * multiple segments and .section pseudo op.  This will allow gcc to
 * use the .init section for g++ ctor/dtor.
 *
 * If you don't have gas then undefined USE_GAS.  You will also have
 * to use collect if you want to use g++
 */
#define USE_GAS

#include "i386/aix386ng.h"

/* Use crt1.o as a startup file and crtn.o as a closing file.
   And add crtbegin.o and crtend.o for ctors and dtors */

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt1.o%s}%{!p:crt1.o%s}} crtbegin.o%s"
#undef  ENDFILE_SPEC
#define ENDFILE_SPEC \
  "crtend.o%s crtn.o%s"

/* Removed the -K flags because the gnu ld does not handle it */
#undef LINK_SPEC
#define LINK_SPEC "%{T*} %{z:-lm}"

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
