/* Definitions for BSD assembler syntax for Intel 386
   (actually AT&T syntax for insns and operands,
   adapted to BSD conventions for symbol names and debugging.)
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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Include common aspects of all 386 Unix assemblers.  */
#include "i386/unix.h"

/* Use the Sequent Symmetry assembler syntax.  */

#define TARGET_VERSION fprintf (stderr, " (80386, BSD syntax)");

/* Define the syntax of pseudo-ops, labels and comments.  */

/* Prefix for internally generated assembler labels.  If we aren't using
   underscores, we are using prefix `.'s to identify labels that should
   be ignored, as in `i386/gas.h' --karl@cs.umb.edu  */
#ifdef NO_UNDERSCORES
#define LPREFIX ".L"
#else
#define LPREFIX "L"
#endif /* not NO_UNDERSCORES */

/* Assembler pseudos to introduce constants of various size.  */

#define ASM_BYTE_OP "\t.byte"
#define ASM_SHORT "\t.word"
#define ASM_LONG "\t.long"
#define ASM_DOUBLE "\t.double"

/* Output at beginning of assembler file.
   ??? I am skeptical of this -- RMS.  */

#define ASM_FILE_START(FILE) \
  do {	fprintf (FILE, "\t.file\t");				\
	output_quoted_string (FILE, dump_base_name);		\
	fprintf (FILE, "\n");					\
  } while (0)

/* This was suggested, but it shouldn't be right for DBX output. -- RMS
   #define ASM_OUTPUT_SOURCE_FILENAME(FILE, NAME) */


/* Define the syntax of labels and symbol definitions/declarations.  */

/* This is how to output an assembler line
   that says to advance the location counter by SIZE bytes.  */

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.space %u\n", (SIZE))

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

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG)!=0) fprintf ((FILE), "\t.align %d\n", (LOG))

/* This is how to store into the string BUF
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#ifdef NO_UNDERSCORES
#define ASM_GENERATE_INTERNAL_LABEL(BUF,PREFIX,NUMBER)	\
    sprintf ((BUF), "*.%s%d", (PREFIX), (NUMBER))
#else
#define ASM_GENERATE_INTERNAL_LABEL(BUF,PREFIX,NUMBER)	\
    sprintf ((BUF), "*%s%d", (PREFIX), (NUMBER))
#endif

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#ifdef NO_UNDERSCORES
#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  fprintf (FILE, ".%s%d:\n", PREFIX, NUM)
#else
#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  fprintf (FILE, "%s%d:\n", PREFIX, NUM)
#endif

/* This is how to output a reference to a user-level label named NAME.  */

#ifdef NO_UNDERSCORES
#define ASM_OUTPUT_LABELREF(FILE,NAME) fprintf (FILE, "%s", NAME)
#else
#define ASM_OUTPUT_LABELREF(FILE,NAME) fprintf (FILE, "_%s", NAME)
#endif /* not NO_UNDERSCORES */

/* Sequent has some changes in the format of DBX symbols.  */
#define DBX_NO_XREFS 1

/* Don't split DBX symbols into continuations.  */
#define DBX_CONTIN_LENGTH 0
