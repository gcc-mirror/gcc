/* Definitions of target machine for GNU compiler.
   Encore Multimax (OSF/1 with OSF/rose) version.
   Copyright (C) 1991 Free Software Foundation, Inc.

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

#include "i386gas.h"

#define OSF_OS

/* Use a more friendly abort which prints internal compiler error,
   rather than just dumping core.  */

#ifndef abort
#define abort fancy_abort
#endif

/* Prefix that appears before all global/static identifiers, except for
   temporary labels.  */

#define IDENTIFIER_PREFIX "_"

/* Suffix that appears after all global/static identifiers, except for
   temporary labels.  */

#define IDENTIFIER_SUFFIX ""

/* Change default predefines.  */
#ifdef CPP_PREDEFINES
#undef CPP_PREDEFINES
#endif
#define CPP_PREDEFINES "-DOSF -DOSF1 -Dunix -Di386"

#ifdef  CPP_SPEC
#undef  CPP_SPEC
#endif
#define CPP_SPEC "\
%{.S:	-D__LANGUAGE_ASSEMBLY %{!ansi:-DLANGUAGE_ASSEMBLY}} \
%{.cc:	-D__LANGUAGE_C_PLUS_PLUS} \
%{.cxx:	-D__LANGUAGE_C_PLUS_PLUS} \
%{.C:	-D__LANGUAGE_C_PLUS_PLUS} \
%{.m:	-D__LANGUAGE_OBJECTIVE_C} \
%{!.S:	-D__LANGUAGE_C %{!ansi:-DLANGUAGE_C}}"

#ifdef  CC1_SPEC
#undef  CC1_SPEC
#endif
#define CC1_SPEC       ""

#ifdef ASM_SPEC
#undef ASM_SPEC
#endif
#define ASM_SPEC       ""

#ifdef  LINK_SPEC
#undef  LINK_SPEC
#endif
#define LINK_SPEC      "%{v*: -v}                           \
                        %{pic-none: -noshrlib} %{noshrlib}  \
			%{!pic-none: -warn_nopic}           \
                        %{nostdlib} %{glue}"

#ifdef  LIB_SPEC
#undef  LIB_SPEC
#endif

/* For now, force static libraries instead of shared, but do so that
   does not use -noshrlib, since the old linker does not provide it.  */

#define LIB_SPEC "%{!pic-none: %{!pic-lib: -L/usr/ccs/lib }} -lc"

#ifdef  LIBG_SPEC
#undef  LIBG_SPEC
#endif
#define LIBG_SPEC      ""

#ifdef  STARTFILE_SPEC
#undef  STARTFILE_SPEC
#endif
#define STARTFILE_SPEC "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}"

#ifdef  MACHINE_TYPE
#undef  MACHINE_TYPE
#endif

#define MACHINE_TYPE   ((!TARGET_486) ? "80386 running OSF/1 with OSF/rose objects" :  \
                                        "80486 running OSF/1 with OSF/rose objects")

#ifdef  MD_EXEC_PREFIX
#undef  MD_EXEC_PREFIX
#endif
#define MD_EXEC_PREFIX		"/usr/ccs/gcc/"

#ifdef  MD_STARTFILE_PREFIX
#undef  MD_STARTFILE_PREFIX
#endif
#define MD_STARTFILE_PREFIX	"/usr/ccs/lib/"

/* Tell final.c we don't need a label passed to mcount.  */
#define NO_PROFILE_DATA

#undef  FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO) fprintf (FILE, "\tcall _mcount\n")

/* Tell collect that the object format is OSF/rose.  */
#define OBJECT_FORMAT_ROSE

/* Use atexit for static constructors/destructors, instead of defining
   our own exit function.  */
#define HAVE_ATEXIT

/* Define this macro meaning that gcc should find the library 'libgcc.a'
   by hand, rather than passing the argeument '-lgcc' to tell the linker
   to do the search */
#define LINK_LIBGCC_SPECIAL

/* This is how to output an assembler line defining a `double' constant.
   Use "word" pseudos to avoid printing NaNs, infinity, etc.  */

/* This is how to output an assembler line defining a `double' constant.  */

#ifdef ASM_OUTPUT_DOUBLE
#undef ASM_OUTPUT_DOUBLE
#endif
#define ASM_OUTPUT_DOUBLE(STREAM,VALUE)					\
{									\
  union { double d; long l[2]; } u2;					\
  u2.d = VALUE;								\
  fprintf (STREAM, "\t.long\t0x%08lx\t\t# %.20g\n\t.long\t0x%08lx\n",	\
	   u2.l[0], u2.d, u2.l[1]);					\
}

/* This is how to output an assembler line defining a `float' constant.  */

#ifdef ASM_OUTPUT_FLOAT
#undef ASM_OUTPUT_FLOAT
#endif
#define ASM_OUTPUT_FLOAT(STREAM,VALUE)					\
{									\
  union { float f; long l; } u2;					\
  u2.f = VALUE;								\
  fprintf (STREAM, "\t.long\t0x%08lx\t\t# %.12g\n", u2.l, u2.f);	\
}

/* Generate calls to memcpy, etc., not bcopy, etc.  */
#define TARGET_MEM_FUNCTIONS

/* A C statement to output assembler commands which will identify
   the object file as having been compiled with GNU CC (or another
   GNU compiler).

   If you don't define this macro, the string `gcc2_compiled.:' is
   output.  This string is calculated to define a symbol which, on
   BSD systems, will never be defined for any other reason.  GDB
   checks for the presence of this symbol when reading the symbol
   table of an executable.

   On non-BSD systems, you must arrange communication with GDB in
   some other fashion.  If GDB is not used on your system, you can
   define this macro with an empty body.

   On OSF/1, gcc2_compiled. confuses the kernel debugger, so don't
   put it out.  */

#define ASM_IDENTIFY_GCC(STREAM)


/* Defines to be able to build libgcc.a with GCC.  */

#define perform_udivsi3(a,b)						\
{									\
  register int dx asm("dx");						\
  register int ax asm("ax");						\
									\
  dx = 0;								\
  ax = a;								\
  asm ("divl %3" : "=a" (ax), "=d" (dx) : "a" (ax), "g" (b), "d" (dx));	\
  return ax;								\
}

#define perform_divsi3(a,b)						\
{									\
  register int dx asm("dx");						\
  register int ax asm("ax");						\
									\
  ax = a;								\
  asm ("cltd\n\tidivl %3" : "=a" (ax), "=d" (dx) : "a" (ax), "g" (b));	\
  return ax;								\
}

#define perform_umodsi3(a,b)						\
{									\
  register int dx asm("dx");						\
  register int ax asm("ax");						\
									\
  dx = 0;								\
  ax = a;								\
  asm ("divl %3" : "=a" (ax), "=d" (dx) : "a" (ax), "g" (b), "d" (dx));	\
  return dx;								\
}

#define perform_modsi3(a,b)						\
{									\
  register int dx asm("dx");						\
  register int ax asm("ax");						\
									\
  ax = a;								\
  asm ("cltd\n\tidivl %3" : "=a" (ax), "=d" (dx) : "a" (ax), "g" (b));	\
  return dx;								\
}

#define perform_fixdfsi(a)						\
{									\
  auto unsigned short ostatus;						\
  auto unsigned short nstatus;						\
  auto int ret;								\
									\
  &ostatus;			/* guarantee these land in memory */	\
  &nstatus;								\
  &ret;									\
									\
  asm volatile ("fnstcw %0" : "=m" (ostatus));				\
  nstatus = ostatus | 0x0c00;						\
  asm volatile ("fldcw %0" : /* no outputs */ : "m" (nstatus));		\
  asm volatile ("fldl %0" : /* no outputs */ : "m" (a));		\
  asm volatile ("fistpl %0" : "=m" (ret));				\
  asm volatile ("fldcw %0" : /* no outputs */ : "m" (ostatus));		\
									\
  return ret;								\
}
