/* Definitions of target machine for GNU compiler, for a 3b1 using GAS.
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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "m68k/m68k.h"

/* See m68k.h.  0 means 68000 with no 68881.  */
#define TARGET_DEFAULT 0

/* Don't try using XFmode.  */
#undef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE 64

/* Define __HAVE_68881 in preprocessor only if -m68881 is specified.
   This will control the use of inline 68881 insns in certain macros.
   Also inform the program which CPU this is for.  */
#define CPP_SPEC "%{m68881:-D__HAVE_68881__} \
%{!ansi:%{m68020:-Dmc68020}%{mc68020:-Dmc68020}%{!mc68020:%{!m68020:-Dmc68010}}}"

/* -m68020 requires special flags to the assembler.  */
#define ASM_SPEC \
 "%{m68020:-mc68020}%{mc68020:-mc68020}%{!mc68020:%{!m68020:-mc68010}}"

/* Names to predefine in the preprocessor for this target machine.  */
#define CPP_PREDEFINES "-Dmc68000 -Dmc68k -Dunix -Dunixpc -Asystem=unix  -Asystem=svr3 -Acpu=m68k -Amachine=m68k"

/* This is (not really) BSD, so (but) it wants DBX format.  */
#define DBX_DEBUGGING_INFO

/* Brain damage.  */
#define SCCS_DIRECTIVE

/* Specify how to pad function arguments.
   Value should be `upward', `downward' or `none'.
   Same as the default, except no padding for large or variable-size args.  */
#define FUNCTION_ARG_PADDING(MODE, TYPE)				\
  (((MODE) == BLKmode							\
    ? ((TYPE) && TREE_CODE (TYPE_SIZE (TYPE))	== INTEGER_CST		\
       && int_size_in_bytes (TYPE) < PARM_BOUNDARY / BITS_PER_UNIT)	\
    : GET_MODE_BITSIZE (MODE) < PARM_BOUNDARY)				\
   ? downward : none)

/* Every structure or union's size must be a multiple of 2 bytes.  */
#define STRUCTURE_SIZE_BOUNDARY 16

/* Generate calls to memcpy, memcmp and memset.  */
#define TARGET_MEM_FUNCTIONS

