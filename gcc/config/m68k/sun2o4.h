/* Definitions of target machine for GNU compiler.  Sun 2 running Sunos 4.
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

#include "sun2.h"


/* Define __HAVE_SKY__ in preprocessor, according to the -m flags.
   Also inform the program which CPU this is for.  */

#undef CPP_SPEC

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"
#undef SIZE_TYPE
#define SIZE_TYPE "int"
#undef WCHAR_TYPE
#define WCHAR_TYPE "short unsigned int"
#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 16

#if TARGET_DEFAULT & 0200

/* -msky is the default */
#define CPP_SPEC \
"%{!msoft-float:-D__HAVE_SKY__}\
%{!ansi:%{m68020:-Dmc68020}%{mc68020:-Dmc68020}%{!mc68020:%{!m68020:-Dmc68010}}}"

#else

/* -msoft-float is the default */
#define CPP_SPEC \
"%{msky:-D__HAVE_SKY__ }\
%{!ansi:%{m68020:-Dmc68020}%{mc68020:-Dmc68020}%{!mc68020:%{!m68020:-Dmc68010}}}"

#endif

/* STARTFILE_SPEC to include sun floating point initialization
   This is necessary (tr: Sun does it) for the sky routines.
   I'm not sure what would happen below if people gave contradictory
   arguments (eg. -msoft-float -mfpa) */

#undef STARTFILE_SPEC

#if TARGET_DEFAULT & 0200
/* -msky is the default */
#define STARTFILE_SPEC					\
  "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}	\
   %{msoft-float:Fcrt1.o%s}				\
   %{!msoft-float:Scrt1.o%s}"
#else
/* -msoft-float is the default */
#define STARTFILE_SPEC					\
  "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}	\
   %{msky:Scrt1.o%s}					\
   %{!msky:Fcrt1.o%s}"
#endif

/* Specify library to handle `-a' basic block profiling.
   Control choice of libm.a (if user says -lm)
   based on fp arith default and options.  */

#undef LIB_SPEC

#if TARGET_DEFAULT & 0200
/* -msky is the default */
#define LIB_SPEC "%{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p} \
%{a:/usr/lib/bb_link.o -lc} %{g:-lg} \
%{msoft-float:-L/usr/lib/fsoft} \
%{!msoft_float:-L/usr/lib/fsky}"
#else
/* -msoft-float is the default */
#define LIB_SPEC "%{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p} \
%{a:/usr/lib/bb_link.o -lc} %{g:-lg} \
%{!msky:-L/usr/lib/fsoft} \
%{msky:-L/usr/lib/ffpa}"
#endif

#undef LINK_SPEC
#define LINK_SPEC \
  "%{!nostdlib:%{!r*:%{!e*:-e start}}} -dc -dp %{static:-Bstatic}"

#undef ASM_OUTPUT_DOUBLE
#define ASM_OUTPUT_DOUBLE(FILE,VALUE)					\
  (REAL_VALUE_ISINF ((VALUE))						\
   ? fprintf (FILE, "\t.double 0r%s99e999\n", ((VALUE) > 0 ? "" : "-")) \
   : REAL_VALUE_MINUS_ZERO ((VALUE))					\
   ? fprintf (FILE, "\t.long 0x80000000,0\n")				\
   : fprintf (FILE, "\t.double 0r%.20e\n", (VALUE)))

/* This is how to output an assembler line defining a `float' constant.  */

#undef ASM_OUTPUT_FLOAT
#define ASM_OUTPUT_FLOAT(FILE,VALUE)					\
  (REAL_VALUE_ISINF ((VALUE))						\
   ? fprintf (FILE, "\t.single 0r%s99e999\n", ((VALUE) > 0 ? "" : "-")) \
   : REAL_VALUE_MINUS_ZERO ((VALUE))					\
   ? fprintf (FILE, "\t.long 0x80000000\n")				\
   : fprintf (FILE, "\t.single 0r%.20e\n", (VALUE)))

#undef ASM_OUTPUT_FLOAT_OPERAND
#define ASM_OUTPUT_FLOAT_OPERAND(FILE,VALUE)				\
  (REAL_VALUE_ISINF ((VALUE))						\
   ? fprintf (FILE, "#0r%s99e999", ((VALUE) > 0 ? "" : "-")) 		\
   : REAL_VALUE_MINUS_ZERO ((VALUE))					\
   ? fprintf (FILE, "#0r-0.0")						\
   : fprintf (FILE, "#0r%.9g", (VALUE)))

#undef ASM_OUTPUT_DOUBLE_OPERAND
#define ASM_OUTPUT_DOUBLE_OPERAND(FILE,VALUE)				\
  (REAL_VALUE_ISINF ((VALUE))						\
   ? fprintf (FILE, "#0r%s99e999", ((VALUE) > 0 ? "" : "-"))		\
   : REAL_VALUE_MINUS_ZERO ((VALUE))					\
   ? fprintf (FILE, "#0r-0.0")						\
   : fprintf (FILE, "#0r%.20g", (VALUE)))
