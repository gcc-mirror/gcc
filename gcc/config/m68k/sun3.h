/* Definitions of target machine for GNU compiler.  Sun 68000/68020 version.
   Copyright (C) 1987, 1988 Free Software Foundation, Inc.

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

/* This comment is here to see if it will keep Sun's cpp from dying.  */

/* If you do not need to generate floating point code for the optional
   Sun FPA board, you can safely comment out the SUPPORT_SUN_FPA define
   to gain a little speed and code size.  */

#define SUPPORT_SUN_FPA

#include "m68k.h"

/* See m68k.h.  7 means 68020 with 68881.  */

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT 7
#endif

/* Define __HAVE_FPA__ or __HAVE_68881__ in preprocessor,
   according to the -m flags.
   This will control the use of inline 68881 insns in certain macros.
   Also inform the program which CPU this is for.  */

#if TARGET_DEFAULT & 02

/* -m68881 is the default */
#define CPP_SPEC \
"%{!msoft-float:%{mfpa:-D__HAVE_FPA__ }%{!mfpa:-D__HAVE_68881__ }}\
%{m68000:-D__mc68010__}%{mc68000:-D__mc68010__}%{!mc68000:%{!m68000:-D__mc68020__}} \
%{!ansi:%{m68000:-Dmc68010}%{mc68000:-Dmc68010}%{!mc68000:%{!m68000:-Dmc68020}}}"

#else

/* -msoft-float is the default */
#define CPP_SPEC \
"%{m68881:-D__HAVE_68881__ }%{mfpa:-D__HAVE_FPA__ }\
%{m68000:-D__mc68010__}%{mc68000:-D__mc68010__}%{!mc68000:%{!m68000:-D__mc68020__}} \
%{!ansi:%{m68000:-Dmc68010}%{mc68000:-Dmc68010}%{!mc68000:%{!m68000:-Dmc68020}}}"

#endif

/* Prevent error on `-sun3' and `-target sun3' options.  */

#define CC1_SPEC "%{sun3:} %{target:}"

#define PTRDIFF_TYPE "int"
#define SIZE_TYPE "int"

/* We must override m68k.h.  */
#undef WCHAR_TYPE
#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE "short unsigned int"
#define WCHAR_TYPE_SIZE 16

/* These compiler options take an argument.  We ignore -target for now.  */

#define WORD_SWITCH_TAKES_ARG(STR)				\
 (!strcmp (STR, "Tdata") || !strcmp (STR, "Ttext")		\
  || !strcmp (STR, "Tbss") || !strcmp (STR, "include")		\
  || !strcmp (STR, "imacros") || !strcmp (STR, "target")	\
  || !strcmp (STR, "assert") || !strcmp (STR, "aux-info"))

/* -m68000 requires special flags to the assembler.  */

#define ASM_SPEC \
 "%{m68000:-mc68010}%{mc68000:-mc68010}%{!mc68000:%{!m68000:-mc68020}} \
  %{fpic:-k} %{fPIC:-k}"

/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-Dmc68000 -Dsun -Dunix"

/* STARTFILE_SPEC to include sun floating point initialization
   This is necessary (tr: Sun does it) for both the m68881 and the fpa
   routines.
   Note that includes knowledge of the default specs for gcc, ie. no
   args translates to the same effect as -m68881
   I'm not sure what would happen below if people gave contradictory
   arguments (eg. -msoft-float -mfpa) */

#if TARGET_DEFAULT & 0100
/* -mfpa is the default */
#define STARTFILE_SPEC					\
  "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}	\
   %{m68881:Mcrt1.o%s}					\
   %{msoft-float:Fcrt1.o%s}				\
   %{!m68881:%{!msoft-float:Wcrt1.o%s}}"
#else
#if TARGET_DEFAULT & 2
/* -m68881 is the default */
#define STARTFILE_SPEC					\
  "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}	\
   %{mfpa:Wcrt1.o%s}					\
   %{msoft-float:Fcrt1.o%s}				\
   %{!mfpa:%{!msoft-float:Mcrt1.o%s}}"
#else
/* -msoft-float is the default */
#define STARTFILE_SPEC					\
  "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}	\
   %{m68881:Mcrt1.o%s}					\
   %{mfpa:Wcrt1.o%s}					\
   %{!m68881:%{!mfpa:Fcrt1.o%s}}"
#endif
#endif

/* Specify library to handle `-a' basic block profiling.
   Control choice of libm.a (if user says -lm)
   based on fp arith default and options.  */

#if TARGET_DEFAULT & 0100
/* -mfpa is the default */
#define LIB_SPEC "%{g:-lg} %{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p} \
%{a:/usr/lib/bb_link.o} %{g:-lg} \
%{msoft-float:-L/usr/lib/fsoft}%{m68881:-L/usr/lib/f68881}\
%{!msoft_float:%{!m68881:-L/usr/lib/ffpa}}"
#else
#if TARGET_DEFAULT & 2
/* -m68881 is the default */
#define LIB_SPEC "%{g:-lg} %{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p} \
%{a:/usr/lib/bb_link.o} %{g:-lg} \
%{msoft-float:-L/usr/lib/fsoft}%{!msoft-float:%{!mfpa:-L/usr/lib/f68881}}\
%{mfpa:-L/usr/lib/ffpa}"
#else
/* -msoft-float is the default */
#define LIB_SPEC "%{g:-lg} %{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p} \
%{a:/usr/lib/bb_link.o} %{g:-lg} \
%{!m68881:%{!mfpa:-L/usr/lib/fsoft}}%{m68881:-L/usr/lib/f68881}\
%{mfpa:-L/usr/lib/ffpa}"
#endif
#endif

/* Provide required defaults for linker -e and -d switches.  */

#define LINK_SPEC \
 "%{!nostdlib:%{!r*:%{!e*:-e start}}} -dc -dp %{static:-Bstatic} %{assert*}"

/* Every structure or union's size must be a multiple of 2 bytes.  */

#define STRUCTURE_SIZE_BOUNDARY 16

/* This is BSD, so it wants DBX format.  */

#define DBX_DEBUGGING_INFO

/* Allow folding division by zero.  */
#define REAL_INFINITY

/* Generate calls to memcpy, memcmp and memset.  */
#define TARGET_MEM_FUNCTIONS

/* This is how to output an assembler line defining a `double' constant.  */

#undef ASM_OUTPUT_DOUBLE
#define ASM_OUTPUT_DOUBLE(FILE,VALUE)					\
  {									\
    if (REAL_VALUE_ISINF (VALUE))					\
      fprintf (FILE, "\t.double 0r%s99e999\n", (VALUE) > 0 ? "" : "-");	\
    else if (REAL_VALUE_ISNAN (VALUE))					\
      {									\
	union { double d; long l[2];} t;				\
	t.d = (VALUE);							\
	fprintf (FILE, "\t.long 0x%lx\n\t.long 0x%lx\n", t.l[0], t.l[1]); \
      }									\
    else								\
      fprintf (FILE, "\t.double 0r%.17g\n", VALUE);			\
  }

/* This is how to output an assembler line defining a `float' constant.  */

#undef ASM_OUTPUT_FLOAT
#define ASM_OUTPUT_FLOAT(FILE,VALUE)					\
  {									\
    if (REAL_VALUE_ISINF (VALUE))					\
      fprintf (FILE, "\t.single 0r%s99e999\n", (VALUE) > 0 ? "" : "-");	\
    else if (REAL_VALUE_ISNAN (VALUE))					\
      {									\
	union { float f; long l;} t;					\
	t.f = (VALUE);							\
	fprintf (FILE, "\t.long 0x%lx\n", t.l);				\
      }									\
    else								\
      fprintf (FILE, "\t.single 0r%.9g\n", VALUE);			\
  }

/* This is how to output an assembler lines defining floating operands.
   There's no way to output a NaN's fraction, so we lose it.  */
  
#undef ASM_OUTPUT_FLOAT_OPERAND
#define ASM_OUTPUT_FLOAT_OPERAND(FILE,VALUE)				\
  (REAL_VALUE_ISINF ((VALUE))						\
   ? (asm_fprintf (FILE, "%I0r%s99e999", ((VALUE) > 0 ? "" : "-")), 0)	\
   : REAL_VALUE_MINUS_ZERO (VALUE)					\
   ? (asm_fprintf (FILE, "%I0r-0.0"), 0)				\
   : (asm_fprintf (FILE, "%I0r%.9g", (VALUE)), 0))

#undef ASM_OUTPUT_DOUBLE_OPERAND
#define ASM_OUTPUT_DOUBLE_OPERAND(FILE,VALUE)				\
  (REAL_VALUE_ISINF ((VALUE))						\
   ? (asm_fprintf (FILE, "%I0r%s99e999", ((VALUE) > 0 ? "" : "-")), 0)	\
   : REAL_VALUE_MINUS_ZERO (VALUE)					\
   ? (asm_fprintf (FILE, "%I0r-0.0"), 0)				\
   : (asm_fprintf (FILE, "%I0r%.17g", (VALUE)), 0))
