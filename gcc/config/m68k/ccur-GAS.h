/* Definitions of target machine for GNU compiler.  Concurrent 68k version.
   Copyright (C) 1987, 1988, 1995, 1996, 1997 Free Software Foundation, Inc.

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

#ifndef MASSCOMP
#define MASSCOMP
#endif

#ifndef CONCURRENT
#define CONCURRENT
#endif

#ifndef __mc68000__
#define __mc68000__
#endif

#ifndef __mc68020__
#define __mc68020__
#endif

#define USE_GAS
#define MOVE_RATIO 100

#define SPACE_AFTER_L_OPTION
#define SWITCHES_NEED_SPACES "oL"

/* See m68k.h.  7 means 68020 with 68881.  */
#define TARGET_DEFAULT (MASK_68040|MASK_BITFIELD|MASK_68881|MASK_68020)

#include "m68k/m68k.h"

#define SIZE_TYPE "int"

/* for 68k machines this only needs to be TRUE for the 68000 */

#undef STRICT_ALIGNMENT     
#define STRICT_ALIGNMENT 0

/* Names to predefine in the preprocessor for this target machine.  */
#define CPP_PREDEFINES \
    "-Dmc68000 -Dmasscomp -DMASSCOMP -Dunix -DLANGUAGE_C -Dconcurrent -DCONCURRENT"

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (68k, GNU GAS syntax)");

/* Discard internal local symbols beginning with 'L'.  */
#define LINK_SPEC "-X"

/* Every structure or union's size must be a multiple of 4 bytes.  */
#define STRUCTURE_SIZE_BOUNDARY 16 

/* No data type wants to be aligned rounder than this.  */
#undef BIGGEST_ALIGNMENT
#define BIGGEST_ALIGNMENT 32

/* Allocation boundary (in *bits*) for storing pointers in memory.  */
#undef POINTER_BOUNDARY
#define POINTER_BOUNDARY 32

/* Alignment of field after `int : 0' in a structure.  */
#undef EMPTY_FIELD_BOUNDARY
#define EMPTY_FIELD_BOUNDARY 32

/* Allocation boundary in bits for the code of a function */
#undef  FUNCTION_BOUNDARY
#define FUNCTION_BOUNDARY 32

/* Make strings long-word aligned so dhrystones will run faster.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN) \
   (TREE_CODE (EXP) == STRING_CST \
    && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

/* This is BSD, so it wants DBX format.  */
#define DBX_DEBUGGING_INFO 1

/* Override parts of m68k.h */

#undef CALL_USED_REGISTERS
#define CALL_USED_REGISTERS \
 {1, 1, 0, 0, 0, 0, 0, 0, \
  1, 1, 0, 0, 0, 0, 0, 1, \
  1, 1, 0, 0, 0, 0, 1, 1 }

#undef REG_ALLOC_ORDER
#define REG_ALLOC_ORDER \
{  0,  1,  2,  3,  4,  5,  6,  7,\
   8,  9, 10, 11, 12, 13, 14, 15, \
  16, 17, 22, 23, 18, 19, 20, 21 }

#undef ASM_FILE_START
#define ASM_FILE_START(FILE)	\
  fprintf (FILE, "#NO_APP\n.globl fltused\n");

#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
{ int _LOG = LOG;                       \
  if (_LOG == 1)			\
    fprintf (FILE, "\t.even\n");	\
  else if (_LOG == 2)                   \
    fprintf (FILE, "\t.align 4\n");     \
  else if (_LOG != 0)			\
    fprintf (FILE, "\t.align %d\n", _LOG);\
}

/* crt0.c should use the vax-bsd style of entry, with a dummy arg.  */

#define CRT0_DUMMIES bogus_fp,
