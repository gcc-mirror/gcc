/* Configuration for an OpenBSD i386 target.
   
   Copyright (C) 1999 Free Software Foundation, Inc.

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

/* This is tested by i386gas.h.  */
#define YES_UNDERSCORES

#include <i386/gstabs.h>

/* Get perform_* macros to build libgcc.a.  */
#include <i386/perform.h>

/* Get generic OpenBSD definitions.  */
#define OBSD_OLD_GAS
#include <openbsd.h>

/* This goes away when the math-emulator is fixed */
#undef TARGET_DEFAULT
#define TARGET_DEFAULT \
  (MASK_80387 | MASK_IEEE_FP | MASK_FLOAT_RETURNS | MASK_NO_FANCY_MATH_387)

/* Run-time target specifications */
#define CPP_PREDEFINES "-D__unix__ -D__OpenBSD__ \
 -Asystem(unix) -Asystem(bsd) -Asystem(OpenBSD)"

/* Layout of source language data types.  */

/* This must agree with <machine/ansi.h> */
#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

/* Assembler format: overall framework.  */

#undef ASM_APP_ON
#define ASM_APP_ON "#APP\n"

#undef ASM_APP_OFF
#define ASM_APP_OFF "#NO_APP\n"

/* The following macros were originally stolen from i386v4.h.
   These have to be defined to get PIC code correct.  */

/* Assembler format: dispatch tables.  */

/* How to output an element of a case-vector that is relative.
   This is only used for PIC code.  See comments by the `casesi' insn in
   i386.md for an explanation of the expression this outputs.  */
#undef ASM_OUTPUT_ADDR_DIFF_ELT
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL) \
  fprintf (FILE, "\t.long _GLOBAL_OFFSET_TABLE_+[.-%s%d]\n", LPREFIX, VALUE)

/* Assembler format: sections.  */

/* Indicate when jump tables go in the text section.  This is
   necessary when compiling PIC code.  */
#define JUMP_TABLES_IN_TEXT_SECTION  (flag_pic)

/* Stack & calling: aggregate returns.  */

/* Don't default to pcc-struct-return, because gcc is the only compiler, and
   we want to retain compatibility with older gcc versions.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* Assembler format: alignment output.  */

/* Kludgy test: when gas is upgraded, it will have p2align, and no problems
   with nops.  */
#ifndef HAVE_GAS_MAX_SKIP_P2ALIGN
/* i386 OpenBSD still uses an older gas that doesn't insert nops by default
   when the .align directive demands to insert extra space in the text
   segment.  */
#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG) \
  if ((LOG)!=0) fprintf ((FILE), "\t.align %d,0x90\n", (LOG))
#endif

/* Stack & calling: profiling.  */

/* OpenBSD's profiler recovers all information from the stack pointer.
   The icky part is not here, but in machine/profile.h.  */
#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO)  \
  fputs (flag_pic ? "\tcall mcount@PLT\n": "\tcall mcount\n", FILE);

/* Assembler format: exception region output.  */

/* All configurations that don't use elf must be explicit about not using
   dwarf unwind information. egcs doesn't try too hard to check internal
   configuration files...  */
#define DWARF2_UNWIND_INFO 0

/* Assembler format: alignment output.  */

/* A C statement to output to the stdio stream FILE an assembler
   command to advance the location counter to a multiple of 1<<LOG
   bytes if it is within MAX_SKIP bytes.

   This will be used to align code labels according to Intel 
   recommendations, in prevision of binutils upgrade.  */
#ifdef HAVE_GAS_MAX_SKIP_P2ALIGN
#define ASM_OUTPUT_MAX_SKIP_ALIGN(FILE,LOG,MAX_SKIP)			\
  do {									\
    if ((LOG) != 0) {							\
      if ((MAX_SKIP) == 0) fprintf ((FILE), "\t.p2align %d\n", (LOG));	\
      else fprintf ((FILE), "\t.p2align %d,,%d\n", (LOG), (MAX_SKIP));	\
    }									\
  } while (0)
#endif

/* Note that we pick up ASM_OUTPUT_MI_THUNK from unix.h.  */

