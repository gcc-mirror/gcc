/* Definitions for Sun assembler syntax for the Intel 80386.
   Copyright (C) 1988, 1996, 2000 Free Software Foundation, Inc.

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

#define TARGET_VERSION fprintf (stderr, " (80386, Sun syntax)");

/* Define the syntax of instructions and addresses.  */

/* Prefix for internally generated assembler labels.  */
#define LPREFIX ".L"

/* Define the syntax of pseudo-ops, labels and comments.  */

/* Assembler pseudos to introduce constants of various size.  */

#define ASM_SHORT "\t.value\t"
#define ASM_LONG "\t.long\t"
#define ASM_QUAD "\t.quad\t"  /* Should not be used for 32bit compilation.  */


/* How to output an ASCII string constant.  */

#define ASM_OUTPUT_ASCII(FILE, PTR, SIZE) \
do								\
{ size_t i = 0, limit = (SIZE); 				\
  while (i < limit)						\
    { if (i%10 == 0) { if (i!=0) fprintf ((FILE), "\n");	\
		       fputs ("\t.byte\t", (FILE)); }		\
      else fprintf ((FILE), ",");				\
      fprintf ((FILE), "0x%x", ((PTR)[i++] & 0377)) ;}		\
      fprintf ((FILE), "\n");					\
} while (0)

/* Output at beginning of assembler file.  */
/* The .file command should always begin the output.  */

#undef ASM_FILE_START
#define ASM_FILE_START(FILE) \
  do {							\
    {							\
      const int len = strlen (main_input_filename);	\
      const char *na = main_input_filename + len;	\
      char shorter[15];					\
      /* NA gets MAIN_INPUT_FILENAME sans directory names.  */\
      while (na > main_input_filename)			\
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
	     lang_hooks.name, version_string);		\
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
#define TEXT_SECTION_ASM_OP "\t.text"

/* Output before writable data.  */

#undef DATA_SECTION_ASM_OP
#define DATA_SECTION_ASM_OP "\t.data"

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
  sprintf ((BUF), "*.%s%ld", (PREFIX), (long)(NUMBER))

/* The prefix to add to user-visible assembler symbols.  */

#define USER_LABEL_PREFIX ""

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  fprintf (FILE, ".%s%d:\n", PREFIX, NUM)
