/* Definitions of target machine for GNU compiler.  Sun 68000/68020 version.
   Copyright (C) 1987, 1988, 1993, 1995, 1996, 1998, 1999
   Free Software Foundation, Inc.

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

/* This comment is here to see if it will keep Sun's cpp from dying.  */

/* If you do not need to generate floating point code for the optional
   Sun FPA board, you can safely comment out the SUPPORT_SUN_FPA define
   to gain a little speed and code size.  */

#define SUPPORT_SUN_FPA

#include "m68k/m68k.h"

/* See m68k.h.  7 means 68020 with 68881.  */

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_BITFIELD|MASK_68881|MASK_68020)
#endif

/* Define __HAVE_FPA__ or __HAVE_68881__ in preprocessor,
   according to the -m flags.
   This will control the use of inline 68881 insns in certain macros.
   Also inform the program which CPU this is for.  */

#if TARGET_DEFAULT & MASK_68881

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

/* We must override m68k.h.  */
#undef WCHAR_TYPE
#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE "short unsigned int"
#define WCHAR_TYPE_SIZE 16

/* These compiler options take an argument.  We ignore -target for now.  */

#define WORD_SWITCH_TAKES_ARG(STR)				\
 (DEFAULT_WORD_SWITCH_TAKES_ARG (STR)				\
  || !strcmp (STR, "target") || !strcmp (STR, "assert"))

/* -m68000 requires special flags to the assembler.  */

#define ASM_SPEC \
 "%{m68000:-mc68010}%{mc68000:-mc68010}%{!mc68000:%{!m68000:-mc68020}} \
  %{fpic:-k} %{fPIC:-k} %{R} %{j} %{J} %{h} %{d2} %{keep-local-as-symbols:-L}"

/* Names to predefine in the preprocessor for this target machine.  */
/* For a while,  -D_CROSS_TARGET_ARCH=SUN3 was included here,
   but it doesn't work, partly because SUN3 etc. aren't defined
   (and shouldn't be).  It seems that on a native compiler _CROSS_TARGET_ARCH
   should not be defined.  For cross compilers, let's do things as we
   normally do in GCC.  -- rms.  */

#define CPP_PREDEFINES "-Dmc68000 -Dsun -Dunix -Asystem=unix  -Asystem=bsd -Acpu=m68k -Amachine=m68k"

/* STARTFILE_SPEC to include sun floating point initialization
   This is necessary (tr: Sun does it) for both the m68881 and the fpa
   routines.
   Note that includes knowledge of the default specs for gcc, ie. no
   args translates to the same effect as -m68881
   I'm not sure what would happen below if people gave contradictory
   arguments (eg. -msoft-float -mfpa) */

#if TARGET_DEFAULT & MASK_FPA
/* -mfpa is the default */
#define STARTFILE_SPEC					\
  "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}	\
   %{m68881:Mcrt1.o%s}					\
   %{msoft-float:Fcrt1.o%s}				\
   %{!m68881:%{!msoft-float:Wcrt1.o%s}}"
#else
#if TARGET_DEFAULT & MASK_68881
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

#if TARGET_DEFAULT & MASK_FPA
/* -mfpa is the default */
#define LIB_SPEC "%{g:-lg} %{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p} \
%{g:-lg} \
%{msoft-float:-L/usr/lib/fsoft}%{m68881:-L/usr/lib/f68881}\
%{!msoft_float:%{!m68881:-L/usr/lib/ffpa}}"
#else
#if TARGET_DEFAULT & MASK_68881
/* -m68881 is the default */
#define LIB_SPEC "%{g:-lg} %{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p} \
%{g:-lg} \
%{msoft-float:-L/usr/lib/fsoft}%{!msoft-float:%{!mfpa:-L/usr/lib/f68881}}\
%{mfpa:-L/usr/lib/ffpa}"
#else
/* -msoft-float is the default */
#define LIB_SPEC "%{g:-lg} %{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p} \
%{g:-lg} \
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

#define DBX_DEBUGGING_INFO 1

/* Generate calls to memcpy, memcmp and memset.  */
#define TARGET_MEM_FUNCTIONS

/* This is not a good idea.  It prevents interoperation between
   files compiled with -m68881 and those compiled with -msoft-float.  */
#if 0
#define FUNCTION_VALUEX(MODE)					\
  gen_rtx_REG ((MODE),						\
	       ((TARGET_68881					\
		 && ((MODE) == SFmode || (MODE) == DFmode	\
		     || (MODE) == XFmode))			\
		? 16 : 0))

#undef FUNCTION_VALUE
#define FUNCTION_VALUE(VALTYPE,FUNC) FUNCTION_VALUEX (TYPE_MODE (VALTYPE))
#endif /* 0 */

/* This is how to output an assembler lines defining floating operands.
   There's no way to output a NaN's fraction, so we lose it.  */
  
#undef ASM_OUTPUT_FLOAT_OPERAND
#define ASM_OUTPUT_FLOAT_OPERAND(CODE,FILE,VALUE)		\
 do { 								\
      if (CODE != 'f')						\
        {							\
          long l;						\
          REAL_VALUE_TO_TARGET_SINGLE (VALUE, l);		\
          if (sizeof (int) == sizeof (long))			\
            asm_fprintf ((FILE), "%I0x%x", (int) l);		\
          else							\
            asm_fprintf ((FILE), "%I0x%lx", l);			\
        }							\
      else if (REAL_VALUE_ISINF (VALUE))			\
        {							\
          if (REAL_VALUE_NEGATIVE (VALUE))			\
            asm_fprintf (FILE, "%I0r-99e999");			\
          else							\
            asm_fprintf (FILE, "%I0r99e999");			\
        }							\
      else if (REAL_VALUE_MINUS_ZERO (VALUE))			\
        {							\
          asm_fprintf (FILE, "%I0r-0.0");			\
        }							\
      else							\
        { char dstr[30];					\
	  real_to_decimal (dstr, &(VALUE), sizeof (dstr), 9, 0); \
          asm_fprintf (FILE, "%I0r%s", dstr);			\
        }							\
    } while (0)

#undef ASM_OUTPUT_DOUBLE_OPERAND
#define ASM_OUTPUT_DOUBLE_OPERAND(FILE,VALUE)				\
 do { if (REAL_VALUE_ISINF (VALUE))					\
        {								\
          if (REAL_VALUE_NEGATIVE (VALUE))				\
            asm_fprintf (FILE, "%I0r-99e999");				\
          else								\
            asm_fprintf (FILE, "%I0r99e999");				\
        }								\
      else if (REAL_VALUE_MINUS_ZERO (VALUE))				\
        {								\
          asm_fprintf (FILE, "%I0r-0.0");				\
        }								\
      else								\
        { char dstr[30];						\
	  real_to_decimal (dstr, &(VALUE), sizeof (dstr), 0, 1);	\
          asm_fprintf (FILE, "%I0r%s", dstr);				\
        }								\
    } while (0)
