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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include "i386.h"

/* Use default settings for system V.3.  */

#include "svr3.h"

/* Use the ATT assembler syntax.
   This overrides at least one macro (ASM_OUTPUT_LABELREF) from svr3.h.  */

#include "att386.h"

/* By default, target has a 80387.  */

#define TARGET_DEFAULT 1

/* Use crt1.o as a startup file and crtn.o as a closing file.  */

#define STARTFILE_SPEC  \
  "%{pg:gcrt1.o%s}%{!pg:%{posix:%{p:mcrtp1.o%s}%{!p:crtp1.o%s}}%{!posix:%{p:mcrt1.o%s}%{!p:crt1.o%s}}} crtbegin.o%s\
   %{p:-L/usr/lib/libp}%{pg:-L/usr/lib/libp}"

/* ??? There is a suggestion that -lg is needed here.
   Does anyone know whether this is right?  */
#define LIB_SPEC "%{posix:-lcposix} %{shlib:-lc_s} -lc crtend.o%s crtn.o%s"

/* Specify predefined symbols in preprocessor.  */

#define CPP_PREDEFINES "-Dunix -Di386"

#define CPP_SPEC "%{posix:-D_POSIX_SOURCE}"

/* Writing `int' for a bitfield forces int alignment for the structure.  */

#define PCC_BITFIELD_TYPE_MATTERS 1

/* Don't write a `.optim' pseudo; this assembler doesn't handle them.  */

#undef ASM_FILE_START_1
#define ASM_FILE_START_1(FILE)

/* Machines that use the AT&T assembler syntax
   also return floating point values in an FP register.  */
/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */

#define VALUE_REGNO(MODE) \
  (((MODE) == SFmode || (MODE) == DFmode) ? FIRST_FLOAT_REG : 0)

/* 1 if N is a possible register number for a function value. */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == 0 || (N)== FIRST_FLOAT_REG)

#if 0 /* This symbol is expected to exist only on BSD,
	 and besides, it describes the host rather than the target.
	 It certainly does not belong here.  */
#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif
#endif

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

/* Define a few machine-specific details
   of the implementation of constructors.

   CTORS_SECTION_ASM_OP should be defined to concatenate
   the macro INIT_SECTION_ASM_OP, a newline, and a push instruction
   to push a word containing 0 (or some equivalent of that).

   ASM_OUTPUT_CONSTRUCTOR should be defined
   to push the address of the constructor.  */

#undef INIT_SECTION_ASM_OP
#define INIT_SECTION_ASM_OP     ".section .init,\"x\""

#define CTORS_SECTION_ASM_OP \
  INIT_SECTION_ASM_OP "\n"	\
  "\tpushl $0\n\t"		\
  DATA_SECTION_ASM_OP
/* The reason we end with DATA_SECTION_ASM_OP is to prevent the
   initial and final table elements (see crtstuff.c) from getting into
   the .init section and causing a crash.  */

#define ASM_OUTPUT_CONSTRUCTOR(FILE,NAME)	\
  do {						\
    init_section ();				\
    fprintf (FILE, "\tpushl $");		\
    assemble_name (FILE, NAME);			\
    fprintf (FILE, "\n");			\
  } while (0)
