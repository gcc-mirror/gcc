/* Definitions of target machine for GNU compiler.  ENCORE NS32000 version.
   Copyright (C) 1988, 1993 Free Software Foundation, Inc.
   Adapted by Robert Brown (brown@harvard.harvard.edu) from the Sequent
   version by Michael Tiemann (tiemann@mcc.com).

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


#define EXTERNAL_PREFIX '?'
#define IMMEDIATE_PREFIX '$'

#include "ns32k/ns32k.h"

#define SDB_DEBUGGING_INFO
#undef DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* Cause long-jump assembler to be used,
   since otherwise some files fail to be assembled right.  */
#define ASM_SPEC "-j"

#undef ASM_FILE_START
#undef ASM_GENERATE_INTERNAL_LABEL
#undef ASM_OUTPUT_ADDR_DIFF_ELT
#undef ASM_OUTPUT_ALIGN
#undef ASM_OUTPUT_ASCII
#undef ASM_OUTPUT_DOUBLE
#undef ASM_OUTPUT_INT
#undef ASM_OUTPUT_INTERNAL_LABEL
#undef ASM_OUTPUT_LOCAL
#undef CPP_PREDEFINES
#undef FUNCTION_BOUNDARY
#undef PRINT_OPERAND
#undef PRINT_OPERAND_ADDRESS
#undef TARGET_VERSION
#undef FUNCTION_PROFILER
#undef ASM_OUTPUT_LABELREF_AS_INT

#define TARGET_DEFAULT 9  /* 32332 with 32081.  */
#define TARGET_VERSION fprintf (stderr, " (32000, Encore syntax)");
/* Note Encore does not standardly do -Dencore.  */
/* budd: should have a -ns32332 (or -apc) switch! but no harm for now */
#define CPP_PREDEFINES "-Dns32000 -Dn16 -Dns16000 -Dns32332 -Dunix -Asystem(unix) -Acpu(ns32k) -Amachine(ns32k)"

/* Ignore certain cpp directives used in header files on sysV.  */
#define SCCS_DIRECTIVE

/* Output #ident as a .ident.  */
#define ASM_OUTPUT_IDENT(FILE, NAME) fprintf (FILE, "\t.ident \"%s\"\n", NAME);

/* The .file command should always begin the output.  */
#define ASM_FILE_START(FILE) \
output_file_directive ((FILE), main_input_filename)

#define FUNCTION_BOUNDARY 128		/* speed optimization */

/*
 *  The Encore assembler uses ".align 2" to align on 2-byte boundaries.
 */

#define ASM_OUTPUT_ALIGN(FILE,LOG)					\
	fprintf (FILE, "\t.align %d\n", 1 << (LOG))

/* The Encore assembler doesn't seem to accept the usual second argument
   and warns that .align may not work in the text section if optimization
   is on.  */
#undef ASM_OUTPUT_ALIGN_CODE
#define ASM_OUTPUT_ALIGN_CODE(FILE)

/*
 *  Internal labels are prefixed with a period.
 */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)			\
	sprintf (LABEL, "*.%s%d", PREFIX, NUM)
#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)			\
	fprintf (FILE, ".%s%d:\n", PREFIX, NUM)
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, VALUE, REL)			\
	fprintf (FILE, "\t.double .L%d-.LI%d\n", VALUE, REL)

/*
 *  Different syntax for integer constants, double constants, and
 *  uninitialized locals.
 */

#define ASM_OUTPUT_INT(FILE,VALUE)				\
( fprintf (FILE, "\t.double "),					\
  output_addr_const (FILE, (VALUE)),				\
  fprintf (FILE, "\n"))

#define ASM_OUTPUT_LABELREF_AS_INT(STREAM, NAME)			\
do {									\
  fprintf (STREAM, "\t.double\t");					\
  ASM_OUTPUT_LABELREF (STREAM, NAME);					\
  fprintf (STREAM, "\n");						\
} while (0)


#define ASM_OUTPUT_DOUBLE(FILE,VALUE)				\
 fprintf (FILE, "\t.long 0f%.20e\n", (VALUE))

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)		\
( fputs ("\t.bss ", (FILE)),					\
  assemble_name ((FILE), (NAME)),				\
  fprintf ((FILE), ",%u,%u\n", (SIZE), (ROUNDED)))

 /*
  *  Encore assembler can't handle huge string constants like the one in
  *  gcc.c.  If the default routine in varasm.c were more conservative, this
  *  code could be eliminated.  It starts a new .ascii directive every 40
  *  characters.
  */

#define ASM_OUTPUT_ASCII(file, p, size)			\
do {							\
  int i;						\
  for (i = 0; i < (size); i++)				\
    {							\
      register int c = (p)[i];				\
      if ((i / 40) * 40 == i)				\
      if (i == 0)					\
        fprintf ((file), "\t.ascii \"");		\
      else						\
        fprintf ((file), "\"\n\t.ascii \"");		\
      if (c == '\"' || c == '\\')			\
        putc ('\\', (file));				\
      if (c >= ' ' && c < 0177)				\
        putc (c, (file));				\
      else						\
        {						\
          fprintf ((file), "\\%o", c);			\
          if (i < (size) - 1 				\
              && (p)[i + 1] >= '0' && (p)[i + 1] <= '9')\
          fprintf ((file), "\"\n\t.ascii \"");		\
        }						\
    }							\
  fprintf ((file), "\"\n");				\
} while (0)

/* Modify syntax of jsr instructions.  */
#define CALL_MEMREF_IMPLICIT

#define NO_ABSOLUTE_PREFIX_IF_SYMBOLIC

#define PRINT_OPERAND(FILE, X, CODE) print_operand(FILE, X, CODE)

#define PRINT_OPERAND_ADDRESS(FILE, ADDR)  print_operand_address(FILE, ADDR)

/* Change the way in which data is allocated and initialized on the
   encore so that both private and shared data are supported.  Shared data
   that is initialized must be contained in the ".shrdata" section
   of the program.  This is accomplished by defining the SHARED_SECTION_ASM_OP
   macro.  Share data that is simply allocated, and not initialized must
   be prefixed with the ".shrcomm" or ".shrbss" pseudo op, for common or
   local data respectively.  This is accomplished by redefining the
   ASM_OUTPUT_COMMON and ASM_OUTPUT_LOCAL macros. */
    
/* Assembler pseudo-op for shared data segment. */

#define SHARED_SECTION_ASM_OP ".shrdata"

/* This says how to output an assembler line
   to define a shared common symbol. */

#define ASM_OUTPUT_SHARED_COMMON(FILE, NAME, SIZE, ROUNDED) \
( fputs (".shrcomm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%d\n", (ROUNDED)))

/* This says how to output an assembler line
   to define a shared local symbol. */

#define ASM_OUTPUT_SHARED_LOCAL(FILE, NAME, SIZE, ROUNDED) \
( fputs ("\t.shrbss ", (FILE)),				\
  assemble_name ((FILE), (NAME)),			\
  fprintf ((FILE), ",%d,%d\n", (SIZE), (ROUNDED)))

#define FUNCTION_PROFILER(FILE, LABELNO)  \
   fprintf (FILE, "\taddr .LP%d,r0\n\tjsr mcount\n", (LABELNO))

#define ENCORE_ASM
