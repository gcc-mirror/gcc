/* Definitions for Sun assembler syntax for the Intel 80386.
   Copyright (C) 1988 Free Software Foundation, Inc.

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


/* Include common aspects of all 386 Unix assemblers.  */
#include "i386/unix.h"

#define TARGET_VERSION fprintf (stderr, " (80386, Sun syntax)");

/* Define the syntax of instructions and addresses.  */

/* Prefix for internally generated assembler labels.  */
#define LPREFIX ".L"

/* Define the syntax of pseudo-ops, labels and comments.  */

/* Assembler pseudos to introduce constants of various size.  */

#define ASM_BYTE_OP "\t.byte"
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

/* Output at beginning of assembler file.  */
/* The .file command should always begin the output.  */

#undef ASM_FILE_START
#define ASM_FILE_START(FILE) \
  do {							\
    extern char *version_string, *language_string;	\
    {							\
      int len = strlen (dump_base_name);		\
      char *na = dump_base_name + len;			\
      char shorter[15];					\
      /* NA gets DUMP_BASE_NAME sans directory names.  */\
      while (na > dump_base_name)			\
	{						\
	  if (na[-1] == '/')				\
	    break;					\
	  na--;						\
	}						\
      strncpy (shorter, na, 14);			\
      shorter[14] = 0;					\
      fprintf (FILE, "\t.file\t");			\
      output_quoted_string (FILE, shorter);		\
      fprintf (FILE, "\n");				\
    }							\
    fprintf (FILE, "\t.version\t\"%s %s\"\n",		\
	     language_string, version_string);		\
    if (optimize) ASM_FILE_START_1 (FILE);		\
  } while (0)

#define ASM_FILE_START_1(FILE) fprintf (FILE, "\t.optim\n")

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
    if ((LOG)!=0) fprintf ((FILE), "\t.align %d\n", 1<<(LOG))

/* This is how to output an assembler line
   that says to advance the location counter by SIZE bytes.  */

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf ((FILE), "\t.set\t.,.+%u\n", (SIZE))

/* Output before read-only data.  */

#undef TEXT_SECTION_ASM_OP
#define TEXT_SECTION_ASM_OP ".text"

/* Output before writable data.  */

#undef DATA_SECTION_ASM_OP
#define DATA_SECTION_ASM_OP ".data"

/* Define the syntax of labels and symbol definitions/declarations.  */

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)  \
( fputs (".comm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u\n", (ROUNDED)))

/* This says how to output an assembler line
   to define a local common symbol.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  \
( fputs (".lcomm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u\n", (ROUNDED)))

/* This is how to store into the string BUF
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(BUF,PREFIX,NUMBER)	\
  sprintf ((BUF), "*.%s%d", (PREFIX), (NUMBER))

/* This is how to output a reference to a user-level label named NAME.  */

#define ASM_OUTPUT_LABELREF(FILE,NAME)	\
  fprintf (FILE, "%s", NAME)

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  fprintf (FILE, ".%s%d:\n", PREFIX, NUM)
