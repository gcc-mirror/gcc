/* Definitions of target machine for GNU compiler.
   Motorola m88100 running the AT&T/Unisoft/Motorla V.3 reference port.
   Copyright (C) 1990, 1991 Free Software Foundation, Inc.
   Contributed by Ray Essick (ressick@mot.com)
   Currently supported by Tom Wood (wood@dg-rtp.dg.com)

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

#include "m88k.h"

/* Default switches */
#undef	TARGET_DEFAULT
#define TARGET_DEFAULT	(MASK_CHECK_ZERO_DIV	| \
			 MASK_OCS_DEBUG_INFO	| \
			 MASK_OCS_FRAME_POSITION)

/* Macros to be automatically defined.  */
#undef	CPP_PREDEFINES
#define CPP_PREDEFINES "-Dm88000 -Dm88k -DsysV88 -D__CLASSIFY_TYPE__=2"

/* Override svr3.h to link with ?crt0.o instead of ?crt1.o and ?crtn.o.
   From arul@sdsu.edu.  */
#undef	STARTFILE_SPEC
#define STARTFILE_SPEC \
   "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}} crtbegin.o%s"

/* Profiled libraries live in a different directory but keep the same
   names other than that.  arul@sdsu.edu says -lg is always needed.  */
#undef	LIB_SPEC
#define LIB_SPEC "%{p:-L/lib/libp}%{pg:%{!p:-L/lib/libp}} -lg -lc crtend.o%s"

/* Hot version of the profiler that uses r10 to pass the address of
   the counter.  the _gcc_mcount routine knows not to screw with
   the parameter registers.

   DG/UX does this; i wrote a gnu-c/88k specific version and put it
   in libgcc2.c -- RBE; this macro knows about the leading underscore
   convention.  */
#undef	FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO) \
  output_function_profiler (FILE, LABELNO, "_gcc_mcount", 0)

/* Various other changes that we want to have in place without
   too many changes to the m88k.h file.  */
#undef	USE_LIBG
#define	USE_LIBG

/* Define a few machine-specific details
   of the implementation of constructors.

   CTORS_SECTION_ASM_OP should be defined to concatenate
   the macro INIT_SECTION_ASM_OP, a newline, and a push instruction
   to push a word containing 0 (or some equivalent of that).

   ASM_OUTPUT_CONSTRUCTOR should be defined
   to push the address of the constructor.  */

#undef	DO_GLOBAL_CTORS_BODY
#define DO_GLOBAL_CTORS_BODY						\
do {									\
  func_ptr *__CTOR_LIST__ = __builtin_alloca (0), *p;			\
  for (p = __CTOR_LIST__; *p; p += 4)					\
    (*p) ();								\
} while (0)

#undef	CTORS_SECTION_ASM_OP
#define CTORS_SECTION_ASM_OP  			\
  INIT_SECTION_ASM_OP "\n"			\
  "subu\t r31,r31,16\n\tst\t r0,r31,32\n\t"	\
  DATA_SECTION_ASM_OP

/* The reason we end with DATA_SECTION_ASM_OP is to prevent the
   initial and final table elements (see crtstuff.c) from getting into
   the .init section and causing a crash.  */

#undef	ASM_OUTPUT_CONSTRUCTOR
#define ASM_OUTPUT_CONSTRUCTOR(FILE,NAME)	\
  do {						\
    init_section ();				\
    fprintf (FILE, "\tor.u r13,r0,hi16(");	\
    assemble_name (FILE, NAME);			\
    fprintf (FILE, ")\nor\t r13,r13,lo16(");	\
    assemble_name (FILE, NAME);			\
    fprintf (FILE, ")\n\tsubu r31,r31,16\nst\t r13,r31,32\n"); \
  } while (0)

#undef	DTORS_SECTION_ASM_OP
#define DTORS_SECTION_ASM_OP FINI_SECTION_ASM_OP
