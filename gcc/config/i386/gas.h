/* Definitions for Intel 386 running system V with gnu tools
   Copyright (C) 1988, 1993, 1994 Free Software Foundation, Inc.

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

/* Note that i386/seq-gas.h is a GAS configuration that does not use this
   file. */

#include "i386/i386.h"

#ifndef YES_UNDERSCORES
/* Define this now, because i386/bsd.h tests it.  */
#define NO_UNDERSCORES
#endif

/* Use the bsd assembler syntax.  */
/* we need to do this because gas is really a bsd style assembler,
 * and so doesn't work well this these att-isms:
 *
 *  ASM_OUTPUT_SKIP is .set .,.+N, which isn't implemented in gas
 *  ASM_OUTPUT_LOCAL is done with .set .,.+N, but that can't be
 *   used to define bss static space
 *
 * Next is the question of whether to uses underscores.  RMS didn't
 * like this idea at first, but since it is now obvious that we
 * need this separate tm file for use with gas, at least to get
 * dbx debugging info, I think we should also switch to underscores.
 * We can keep i386v for real att style output, and the few
 * people who want both form will have to compile twice.
 */

#include "i386/bsd.h"

/* these come from i386/bsd.h, but are specific to sequent */
#undef DBX_NO_XREFS
#undef DBX_CONTIN_LENGTH

/* Ask for COFF symbols.  */

#define SDB_DEBUGGING_INFO

/* Specify predefined symbols in preprocessor.  */

#define CPP_PREDEFINES "-Dunix -Di386 -Asystem(unix) -Acpu(i386) -Amachine(i386)"
#define CPP_SPEC "%{posix:-D_POSIX_SOURCE}"

/* Allow #sccs in preprocessor.  */

#define SCCS_DIRECTIVE

/* Output #ident as a .ident.  */

#define ASM_OUTPUT_IDENT(FILE, NAME) fprintf (FILE, "\t.ident \"%s\"\n", NAME);

/* Implicit library calls should use memcpy, not bcopy, etc.  */

#define TARGET_MEM_FUNCTIONS

#if 0  /* People say gas uses the log as the arg to .align.  */
/* When using gas, .align N aligns to an N-byte boundary.  */

#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
     if ((LOG)!=0) fprintf ((FILE), "\t.align %d\n", 1<<(LOG))
#endif

/* Align labels, etc. at 4-byte boundaries.
   For the 486, align to 16-byte boundary for sake of cache.  */

#undef ASM_OUTPUT_ALIGN_CODE
#define ASM_OUTPUT_ALIGN_CODE(FILE)			\
     fprintf ((FILE), "\t.align %d,0x90\n",		\
	      TARGET_486 ? 4 : 2);  /* Use log of 16 or log of 4 as arg.  */

/* Align start of loop at 4-byte boundary.  */

#undef ASM_OUTPUT_LOOP_ALIGN
#define ASM_OUTPUT_LOOP_ALIGN(FILE) \
     fprintf ((FILE), "\t.align 2,0x90\n");  /* Use log of 4 as arg.  */

/* A C statement or statements which output an assembler instruction
   opcode to the stdio stream STREAM.  The macro-operand PTR is a
   variable of type `char *' which points to the opcode name in its
   "internal" form--the form that is written in the machine description.

   GAS version 1.38.1 doesn't understand the `repz' opcode mnemonic.
   So use `repe' instead.  */

#define ASM_OUTPUT_OPCODE(STREAM, PTR)	\
{									\
  if ((PTR)[0] == 'r'							\
      && (PTR)[1] == 'e'						\
      && (PTR)[2] == 'p')						\
    {									\
      if ((PTR)[3] == 'z')						\
	{								\
	  fprintf (STREAM, "repe");					\
	  (PTR) += 4;							\
	}								\
      else if ((PTR)[3] == 'n' && (PTR)[4] == 'z')			\
	{								\
	  fprintf (STREAM, "repne");					\
	  (PTR) += 5;							\
	}								\
    }									\
}

/* Define macro used to output shift-double opcodes when the shift
   count is in %cl.  Some assemblers require %cl as an argument;
   some don't.

   GAS requires the %cl argument, so override i386/unix.h. */

#undef AS3_SHIFT_DOUBLE
#define AS3_SHIFT_DOUBLE(a,b,c,d) AS3 (a,b,c,d)

/* Print opcodes the way that GAS expects them. */
#define GAS_MNEMONICS 1

#ifdef NO_UNDERSCORES /* If user-symbols don't have underscores,
			 then it must take more than `L' to identify
			 a label that should be ignored.  */

/* This is how to store into the string BUF
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#undef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(BUF,PREFIX,NUMBER)	\
    sprintf ((BUF), ".%s%d", (PREFIX), (NUMBER))

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#undef ASM_OUTPUT_INTERNAL_LABEL
#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  fprintf (FILE, ".%s%d:\n", PREFIX, NUM)

#endif /* NO_UNDERSCORES */
