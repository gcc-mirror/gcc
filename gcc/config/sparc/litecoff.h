/* Definitions of target machine for GNU compiler, for SPARClite w/o FPU, COFF.
   Copyright (C) 1994 Free Software Foundation, Inc.
   Written by Ken Raeburn (raeburn@cygnus.com).

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

#include "sparc/lite.h"

#undef ASM_OUTPUT_IDENT

#undef SELECT_SECTION
#undef SELECT_RTX_SECTION
#define BSS_SECTION_ASM_OP	".section\t\".bss\""

#include "svr3.h"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dsparc -Dsparclite -Acpu(sparc) -Amachine(sparc)"

/* just in case */
#undef DBX_DEBUGGING_INFO
#undef PREFERRED_DEBUGGING_TYPE
#define DBX_DEBUGGING_INFO
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

/* These are all necessary because this is how gdb expects gcc to output
   stabs in coff.  */

/* Be function-relative for block and source line stab directives.  */

#define DBX_BLOCKS_FUNCTION_RELATIVE 1

/* but, to make this work, functions must appear prior to line info.  */

#define DBX_FUNCTION_FIRST

/* Generate a blank trailing N_SO to mark the end of the .o file, since
   we can't depend upon the linker to mark .o file boundaries with
   embedded stabs.  */

#define DBX_OUTPUT_MAIN_SOURCE_FILE_END(FILE, FILENAME)			\
  fprintf (FILE,							\
	   "\t.text\n\t.stabs \"\",%d,0,0,Letext\nLetext:\n", N_SO)

/* This is copied from final.c and sparc.h.  */
#undef ASM_OUTPUT_SOURCE_LINE
#define ASM_OUTPUT_SOURCE_LINE(FILE, LINE)			\
{ if (write_symbols == SDB_DEBUG) {				\
    fprintf ((FILE), "\t.ln\t%d\n",				\
	     ((sdb_begin_function_line > -1)			\
	      ? (LINE) - sdb_begin_function_line : 1));		\
  } else if (write_symbols == DBX_DEBUG) {			\
    static int sym_lineno = 1;					\
    fprintf (FILE, ".stabn 68,0,%d,.LM%d-",			\
	     LINE, sym_lineno);					\
    assemble_name (FILE,					\
		   XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0)); \
    fprintf (FILE, "\n.LM%d:\n", sym_lineno);			\
    sym_lineno += 1;						\
  } }

/* Support the ctors and dtors sections for g++.  */

#undef INIT_SECTION_ASM_OP

/* Support the ctors and dtors sections for g++.  */

#undef CTORS_SECTION_ASM_OP
#define CTORS_SECTION_ASM_OP	".section\t.ctors,\"x\""
#undef DTORS_SECTION_ASM_OP
#define DTORS_SECTION_ASM_OP	".section\t.dtors,\"x\""

/* A list of other sections which the compiler might be "in" at any
   given time.  */

#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS in_const, in_bss, in_ctors, in_dtors

/* A list of extra section function definitions.  */

#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS						\
  CONST_SECTION_FUNCTION						\
  CTORS_SECTION_FUNCTION						\
  DTORS_SECTION_FUNCTION

#define CTORS_SECTION_FUNCTION						\
void									\
ctors_section ()							\
{									\
  if (in_section != in_ctors)						\
    {									\
      fprintf (asm_out_file, "%s\n", CTORS_SECTION_ASM_OP);		\
      in_section = in_ctors;						\
    }									\
}

#define DTORS_SECTION_FUNCTION						\
void									\
dtors_section ()							\
{									\
  if (in_section != in_dtors)						\
    {									\
      fprintf (asm_out_file, "%s\n", DTORS_SECTION_ASM_OP);		\
      in_section = in_dtors;						\
    }									\
}

#define INT_ASM_OP ".long"

/* A C statement (sans semicolon) to output an element in the table of
   global constructors.  */
#undef ASM_OUTPUT_CONSTRUCTOR
#define ASM_OUTPUT_CONSTRUCTOR(FILE,NAME)				\
  do {									\
    ctors_section ();							\
    fprintf (FILE, "\t%s\t ", INT_ASM_OP);				\
    assemble_name (FILE, NAME);						\
    fprintf (FILE, "\n");						\
  } while (0)

/* A C statement (sans semicolon) to output an element in the table of
   global destructors.  */
#undef ASM_OUTPUT_DESTRUCTOR
#define ASM_OUTPUT_DESTRUCTOR(FILE,NAME)       				\
  do {									\
    dtors_section ();                   				\
    fprintf (FILE, "\t%s\t ", INT_ASM_OP);				\
    assemble_name (FILE, NAME);              				\
    fprintf (FILE, "\n");						\
  } while (0)

#undef DO_GLOBAL_CTORS_BODY
#undef DO_GLOBAL_DTORS_BODY
