/* Definitions of target machine for GNU compiler.  Altos 3068 68020 version.
   Copyright (C) 1988, 1989, 1993 Free Software Foundation, Inc.

Written by Jyrki Kuoppala <jkp@cs.hut.fi>
Last modified: Mon Mar  6 22:47:58 1989

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


#include "m68k/m68k.h"

/* See m68k.h.  7 means 68020 with 68881.  */
/* 5 is without 68881.  Change to 7 if you have 68881 */

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT 5

/* Don't try using XFmode.  */
#undef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE 64
#endif

/* Define __HAVE_68881__ in preprocessor,
   according to the -m flags.
   This will control the use of inline 68881 insns in certain macros.
   Also inform the program which CPU this is for.  */

#if TARGET_DEFAULT & 02

/* -m68881 is the default */
#define CPP_SPEC \
"%{!msoft-float:-D__HAVE_68881__ }\
%{!ansi:%{m68000:-Dmc68010}%{mc68000:-Dmc68010}%{!mc68000:%{!m68000:-Dmc68020}}}"

#else

/* -msoft-float is the default */
#define CPP_SPEC \
"%{m68881:-D__HAVE_68881__ }\
%{!ansi:%{m68000:-Dmc68010}%{mc68000:-Dmc68010}%{!mc68000:%{!m68000:-Dmc68020}}}"

#endif

/* -m68000 requires special flags to the assembler.  */

#define ASM_SPEC \
 "%{m68000:-mc68010}%{mc68000:-mc68010}%{!mc68000:%{!m68000:-mc68020}}"

/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-Dmc68000 -DPORTAR -Dmc68k32 -Uvax -Dm68k -Dunix -Asystem(unix)  -Acpu(m68k) -Amachine(m68k)"

/* Every structure or union's size must be a multiple of 2 bytes.  */

#define STRUCTURE_SIZE_BOUNDARY 16

/* Generate calls to memcpy, memcmp and memset.  */
#define TARGET_MEM_FUNCTIONS

/* We use gnu assembler, linker and gdb, so we want DBX format.  */

#define DBX_DEBUGGING_INFO

/* Tell some conditionals we will use GAS.  Is this really used?  */

#define USE_GAS

/* This is how to output an assembler line defining a `double' constant.  */

#undef ASM_OUTPUT_DOUBLE
#define ASM_OUTPUT_DOUBLE(FILE,VALUE)			\
  do { char dstr[30];					\
       REAL_VALUE_TO_DECIMAL (VALUE, "%.20e", dstr);	\
       fprintf (FILE, "\t.double 0r%s\n", dstr);	\
     } while (0)

/* This is how to output an assembler line defining a `float' constant.  */

#undef ASM_OUTPUT_FLOAT
#define ASM_OUTPUT_FLOAT(FILE,VALUE)                    \
  do { char dstr[30];					\
       REAL_VALUE_TO_DECIMAL (VALUE, "%.20e", dstr);	\
       fprintf (FILE, "\t.single 0r%s\n", dstr);	\
     } while (0)

#undef ASM_OUTPUT_FLOAT_OPERAND
#define ASM_OUTPUT_FLOAT_OPERAND(CODE,FILE,VALUE)		\
 do {								\
      if (CODE == 'f')						\
        {							\
          char dstr[30];					\
          REAL_VALUE_TO_DECIMAL (VALUE, "%.9g", dstr);		\
	  fprintf (FILE, "#0r%s", dstr);			\
        }							\
      else							\
        {							\
          long l;						\
          REAL_VALUE_TO_TARGET_SINGLE (VALUE, l);		\
          if (sizeof (int) == sizeof (long))			\
            asm_fprintf ((FILE), "%I0x%x", l);			\
          else							\
            asm_fprintf ((FILE), "%I0x%lx", l);			\
        }							\
     } while (0)

#undef ASM_OUTPUT_DOUBLE_OPERAND
#define ASM_OUTPUT_DOUBLE_OPERAND(FILE,VALUE)		\
  do { char dstr[30];					\
       REAL_VALUE_TO_DECIMAL (VALUE, "%.20g", dstr);	\
       fprintf (FILE, "#0r%s", dstr);			\
     } while (0)

/* Return pointer values in both d0 and a0.  */

#undef FUNCTION_EXTRA_EPILOGUE
#define FUNCTION_EXTRA_EPILOGUE(FILE, SIZE)			\
{								\
  extern int current_function_returns_pointer;			\
  if ((current_function_returns_pointer) && 			\
      ! find_equiv_reg (0, get_last_insn (), 0, 0, 0, 8, Pmode))\
    fprintf (FILE, "\tmovel d0,a0\n");				\
}
