/* Definitions for Intel 386 running system V.
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


#include "i386/i386.h"

/* Use default settings for system V.3.  */

#include "svr3.h"

/* Use the ATT assembler syntax.
   This overrides at least one macro (ASM_OUTPUT_LABELREF) from svr3.h.  */

#include "i386/att.h"

/* Use crt1.o as a startup file and crtn.o as a closing file.  */

#define STARTFILE_SPEC  \
  "%{pg:gcrt1.o%s}%{!pg:%{posix:%{p:mcrtp1.o%s}%{!p:crtp1.o%s}}%{!posix:%{p:mcrt1.o%s}%{!p:crt1.o%s}}} crtbegin.o%s\
   %{p:-L/usr/lib/libp}%{pg:-L/usr/lib/libp}"

/* ??? There is a suggestion that -lg is needed here.
   Does anyone know whether this is right?  */
#define LIB_SPEC "%{posix:-lcposix} %{shlib:-lc_s} -lc crtend.o%s crtn.o%s"

/* Specify predefined symbols in preprocessor.  */

#define CPP_PREDEFINES "-Dunix -Di386 -Asystem(unix) -Asystem(svr3) -Acpu(i386) -Amachine(i386)"

#define CPP_SPEC "%{posix:-D_POSIX_SOURCE}"

/* Writing `int' for a bitfield forces int alignment for the structure.  */

#define PCC_BITFIELD_TYPE_MATTERS 1

/* Don't write a `.optim' pseudo; this assembler doesn't handle them.  */

#undef ASM_FILE_START_1
#define ASM_FILE_START_1(FILE)

/* We want to be able to get DBX debugging information via -gstabs.  */

#undef DBX_DEBUGGING_INFO
#define DBX_DEBUGGING_INFO

#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE SDB_DEBUG

/* longjmp may fail to restore the registers if called from the same
   function that called setjmp.  To compensate, the compiler avoids
   putting variables in registers in functions that use both setjmp
   and longjmp.  */

#define NON_SAVING_SETJMP \
  (current_function_calls_setjmp && current_function_calls_longjmp)

/* longjmp may fail to restore the stack pointer if the saved frame
   pointer is the same as the caller's frame pointer.  Requiring a frame
   pointer in any function that calls setjmp or longjmp avoids this
   problem, unless setjmp and longjmp are called from the same function.
   Since a frame pointer will be required in such a function, it is OK
   that the stack pointer is not restored.  */

#undef FRAME_POINTER_REQUIRED
#define FRAME_POINTER_REQUIRED \
  (current_function_calls_setjmp || current_function_calls_longjmp)

/* Modify ASM_OUTPUT_LOCAL slightly to test -msvr3-shlib.  */
#undef ASM_OUTPUT_LOCAL
#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)	\
  do {							\
    int align = exact_log2 (ROUNDED);			\
    if (align > 2) align = 2;				\
    if (TARGET_SVR3_SHLIB)				\
      data_section ();					\
    else						\
      bss_section ();					\
    ASM_OUTPUT_ALIGN ((FILE), align == -1 ? 2 : align);	\
    ASM_OUTPUT_LABEL ((FILE), (NAME));			\
    fprintf ((FILE), "\t.set .,.+%u\n", (ROUNDED));	\
  } while (0)

/* Define a few machine-specific details of the implementation of
   constructors.

   The __CTORS_LIST__ goes in the .init section.  Define CTOR_LIST_BEGIN
   and CTOR_LIST_END to contribute to the .init section an instruction to
   push a word containing 0 (or some equivalent of that).

   ASM_OUTPUT_CONSTRUCTOR should be defined to push the address of the
   constructor.  */

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
