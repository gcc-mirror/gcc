/* Definitions for AT&T assembler syntax for the Intel 80386.
   Copyright (C) 1988, 1996 Free Software Foundation, Inc.

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

/* Include common aspects of all 386 Unix assemblers.  */
#include "i386/unix.h"

#define TARGET_VERSION fprintf (stderr, " (80386, ATT syntax)");

/* Define the syntax of instructions and addresses.  */

/* Prefix for internally generated assembler labels.  */
#define LPREFIX ".L"

/* Assembler pseudos to introduce constants of various size.  */

/* #define ASM_BYTE_OP "\t.byte"  Now in svr3.h or svr4.h.  */
#define ASM_SHORT "\t.value"
#define ASM_LONG "\t.long"
#define ASM_DOUBLE "\t.double"

/* How to output an ASCII string constant.  */

#define ASM_OUTPUT_ASCII(FILE, p, size) \
do								\
{ int i = 0; 							\
  while (i < (size))						\
    { if (i%10 == 0) { if (i!=0) fprintf ((FILE), "\n");	\
		       fprintf ((FILE), "%s ", ASM_BYTE_OP); }	\
      else fprintf ((FILE), ",");				\
	fprintf ((FILE), "0x%x", ((p)[i++] & 0377)) ;}		\
      fprintf ((FILE), "\n");					\
} while (0)

/* Do use .optim by default on this machine.  */
#undef ASM_FILE_START_1
#define ASM_FILE_START_1(FILE) fprintf (FILE, "\t.optim\n")

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
    if ((LOG)!=0) fprintf ((FILE), "\t.align %d\n", 1<<(LOG))

/* This is how to output an assembler line
   that says to advance the location counter by SIZE bytes.  */

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf ((FILE), "\t.set .,.+%u\n", (SIZE))

/* Can't use ASM_OUTPUT_SKIP in text section; it doesn't leave 0s.  */

#define ASM_NO_SKIP_IN_TEXT 1

/* Define the syntax of labels and symbol definitions/declarations.  */

/* The prefix to add for compiler private assembler symbols.  */
#undef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX "."

/* This is how to store into the string BUF
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#undef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(BUF,PREFIX,NUMBER)	\
  sprintf ((BUF), "%s%s%d", LOCAL_LABEL_PREFIX, (PREFIX), (NUMBER))

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#undef ASM_OUTPUT_INTERNAL_LABEL
#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  fprintf (FILE, "%s%s%d:\n", LOCAL_LABEL_PREFIX, PREFIX, NUM)

/* The prefix to add to user-visible assembler symbols. */

#undef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX ""
