/* Definitions of target machine for GNU compiler.  Apollo 680X0 version.
   Copyright (C) 1989 Free Software Foundation, Inc.

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

#include "m68k.h"

/* This symbol may be tested in other files for special Apollo handling */

#define TM_APOLLO

/* See m68k.h.  7 means 68020 with 68881.  */

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT 7
#endif

/* Target switches for the Apollo is the same as in m68k.h, except
   there is no Sun FPA. */

#undef TARGET_SWITCHES
#define TARGET_SWITCHES  \
  { { "68020", 5},				\
    { "c68020", 5},				\
    { "68881", 2},				\
    { "bitfield", 4},				\
    { "68000", -5},				\
    { "c68000", -5},				\
    { "soft-float", -0102},			\
    { "nobitfield", -4},			\
    { "rtd", 8},				\
    { "nortd", -8},				\
    { "short", 040},				\
    { "noshort", -040},				\
    { "", TARGET_DEFAULT}}

/* Define __HAVE_68881__ in preprocessor,
   according to the -m flags.
   This will control the use of inline 68881 insns in certain macros.
   Also inform the program which CPU this is for.  */

#if TARGET_DEFAULT & 02

/* -m68881 is the default */
#define CPP_SPEC \
"%{!msoft-float:%{mfpa:-D__HAVE_FPA__ }%{!mfpa:-D__HAVE_68881__ }}\
%{!ansi:%{m68000:-Dmc68010 }%{mc68000:-Dmc68010 }%{!mc68000:%{!m68000:-Dmc68020 }}\
%{!ansi:-D_APOLLO_SOURCE}}"

#else

/* -msoft-float is the default */
#define CPP_SPEC \
"%{m68881:-D__HAVE_68881__ }%{mfpa:-D__HAVE_FPA__ }\
%{!ansi:%{m68000:-Dmc68010 }%{mc68000:-Dmc68010 }%{!mc68000:%{!m68000:-Dmc68020 }}\
%{!ansi:-D_APOLLO_SOURCE}}"

#endif

/* Names to predefine in the preprocessor for this target machine.  */
/* These are the ones defined by Apollo, plus mc68000 for uniformity with
   GCC on other 68000 systems.  */

#define CPP_PREDEFINES "-Dapollo -Daegis -Dunix"

/* cpp has to support a #sccs directive for the /usr/include files */

#define SCCS_DIRECTIVE

/* Allow #ident but output nothing for it.  */

#define IDENT_DIRECTIVE
#define ASM_OUTPUT_IDENT(FILE, NAME)

/* Allow dollarsigns in identifiers */

#define DOLLARS_IN_IDENTIFIERS 2

/* -m68000 requires special flags to the assembler.
   The -C flag is passed to a modified GNU assembler to cause COFF
   modules to be produced.  Remove it if you're not using this.
   (See vasta@apollo.com.)  */

#define ASM_SPEC \
 "-C %{m68000:-mc68010}%{mc68000:-mc68010}%{!mc68000:%{!m68000:-mc68020}}"

/* STARTFILE_SPEC
   Note that includes knowledge of the default specs for gcc, ie. no
   args translates to the same effect as -m68881 */

#if TARGET_DEFAULT & 2
/* -m68881 is the default */
#define STARTFILE_SPEC					\
  "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}"
#else
/* -msoft-float is the default */
#define STARTFILE_SPEC					\
  "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}"
#endif

/* Specify library to handle `-a' basic block profiling.  */

#define LIB_SPEC "%{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p} \
%{a:/usr/lib/bb_link.o} "

/* Debugging is not supported yet */

#undef DBX_DEBUGGING_INFO
#undef SDB_DEBUGGING_INFO

/* Every structure or union's size must be a multiple of 2 bytes.  */

#define STRUCTURE_SIZE_BOUNDARY 16

/* Functions which return large structures get the address
   to place the wanted value at offset 8 from the frame.  */

#undef  PCC_STATIC_STRUCT_RETURN
#undef  STRUCT_VALUE_REGNUM

/* Caller treats address of return area like a parm.  */
#define STRUCT_VALUE 0

#define STRUCT_VALUE_INCOMING \
  gen_rtx (MEM, Pmode,					\
	   gen_rtx (PLUS, SImode, frame_pointer_rtx,	\
		    gen_rtx (CONST_INT, VOIDmode, 8)))

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#undef STACK_BOUNDARY
#define STACK_BOUNDARY 32

/* Specify how to pad function arguments.
   Arguments are not padded at all; the stack is kept aligned on long
   boundaries. */

#define FUNCTION_ARG_PADDING(mode, size) none

/* Short integral argument prototype promotion is not done */

#undef  PROMOTE_PROTOTYPES

/* The definition of this macro imposes a limit on the size of
   an aggregate object which can be treated as if it were a scalar
   object. */

#define MAX_FIXED_MODE_SIZE    BITS_PER_WORD

/* The definition of this macro implies that there are cases where
   a scalar value cannot be returned in registers.
   For Apollo, anything larger than one integer register is returned
   using the structure-value mechanism, i.e. objects of DFmode are
   returned that way. */

#define RETURN_IN_MEMORY(type) \
  (GET_MODE_SIZE (TYPE_MODE (type)) > UNITS_PER_WORD)

/* This is how to output a reference to a user-level label named NAME.
   In order to link to Apollo libraries, no underscore is prepended to names.
   `assemble_name' uses this.  */

#undef  ASM_OUTPUT_LABELREF
#define ASM_OUTPUT_LABELREF(FILE,NAME)	\
  fprintf (FILE, "%s", NAME)


