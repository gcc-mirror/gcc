/* Operating system specific defines to be used when targeting GCC for some
   embedded system running pSOS. We assume GNU tools with ELF, but
   try to maintain compatibility with the MRI tools. Based on svr4.h.
   Copyright (C) 1996, 1999, 2000 Free Software Foundation, Inc.

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
Boston, MA 02111-1307, USA.

   To use this file, make up a file with a name like:

	?????-psos.h

   where ????? is replaced by the name of the basic hardware that you
   are targeting for.  Then, in the file ?????-psos.h, put something
   like:

	#include "?????.h"
	#include "psos.h"

   followed by any really system-specific defines (or overrides of
   defines) which you find that you need.
*/


/* Define a symbol indicating that we are using psos.h.  */

#define USING_PSOS_H


/* All pSOS targets currently use the ELF object file format.  */

#define OBJECT_FORMAT_ELF


/* Provide a NULL STARTFILE_SPEC. The startfile cannot be specified
   here because it depends on the architecture (e.g. 68K), the
   board-support package (e.g. M162) and the run-time configuration
   (e.g. application vs. ram-image vs. rom-image). Specify the
   startfile in a linker-script created from the generic
   architecture-specific linker-scripts. */

#undef	STARTFILE_SPEC
#define STARTFILE_SPEC ""


/* Predefined macros (independent of processor type). */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dpsos"


/* Implicit library calls should use ANSI memcpy rather than BSD
   bcopy, etc.  */

#define TARGET_MEM_FUNCTIONS

/* This is how we tell the assembler that a symbol is weak.  */

#define ASM_WEAKEN_LABEL(FILE,NAME) \
  do { fputs ("\t.weak\t", FILE); assemble_name (FILE, NAME); \
       fputc ('\n', FILE); } while (0)

/* Switch into a generic section.  */
#define TARGET_ASM_NAMED_SECTION  default_elf_asm_named_section

/* Use DBX debugging info by default.  */

#ifndef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG
#endif

/* For pSOS we use DBX debugging info.  */

#define DBX_DEBUGGING_INFO
