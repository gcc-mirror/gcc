/* Definitions for IBM PS2 running AIX/386.
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


#include "i386.h"

/* Get the generic definitions for system V.3.  */

#include "svr3.h"

/* Use the ATT assembler syntax.
   This overrides at least one macro (ASM_OUTPUT_LABELREF) from svr3.h.  */

#include "att386.h"

/* Use crt1.o as a startup file and crtn.o as a closing file.  */

#define STARTFILE_SPEC  \
  "%{pg:gcrt1.o%s}%{!pg:%{p:mcrt1.o%s}%{!p:crt1.o%s}} crtbegin.o%s"

#define LIB_SPEC "%{p:-L/usr/lib/libp}%{pg:-L/usr/lib/libp} -lc \
  crtend.o%s crtn.o%s"

/* Special flags for the linker.  I don't know what they do.  */

#define LINK_SPEC "%{K} %{!K:-K} %{T*} %{z:-lm}"

/* Specify predefined symbols in preprocessor.  */

#define CPP_PREDEFINES "-D_I386 -Di386 -DAIX -D_AIX"

/* special flags for the aix assembler to generate the short form for all
   qualifying forward reference */

#define ASM_SPEC "-s2"

#undef ASM_FILE_START
#define ASM_FILE_START(FILE) 					\
  do { fprintf (FILE, "\t.file\t\"%s\"\n", dump_base_name);	\
       if (optimize)						\
          ASM_FILE_START_1 (FILE); 				\
       else							\
          fprintf (FILE, "\t.noopt\n");				\
     } while (0)

/* This was suggested, but it shouldn't be right for DBX output. -- RMS
   #define ASM_OUTPUT_SOURCE_FILENAME(FILE, NAME) */

/* Writing `int' for a bitfield forces int alignment for the structure.  */

#define PCC_BITFIELD_TYPE_MATTERS 1

#if 0
/* Don't write a `.optim' pseudo; this assembler
   is said to have a bug when .optim is used.  */

#undef ASM_FILE_START_1
#define ASM_FILE_START_1(FILE) fprintf (FILE, "\t.noopt\n");
#endif

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO)  \
   fprintf (FILE, "\tleal %sP%d,%%eax\n\tcall mcount\n", LPREFIX, (LABELNO));

/* Note that using bss_section here caused errors
   in building shared libraries on system V.3.
   but AIX 1.2 does not have yet shareable libraries on PS2 */
#undef ASM_OUTPUT_LOCAL
#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  \
  (bss_section (),					\
   ASM_OUTPUT_LABEL ((FILE), (NAME)),			\
   fprintf ((FILE), "\t.set .,.+%u\n", (ROUNDED)))

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
