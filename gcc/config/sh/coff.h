/* Definitions of target machine for gcc for Hitachi / SuperH SH using ELF.
   Copyright (C) 1997, 1998, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Jörn Rennecke <joern.rennecke@superh.com>.

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

/* Generate SDB debugging information.  */

#define SDB_DEBUGGING_INFO 1

/* Output DBX (stabs) debugging information if doing -gstabs.  */

#include "dbxcoff.h"

#define SDB_DELIM ";"

#ifndef MAX_OFILE_ALIGNMENT
#define MAX_OFILE_ALIGNMENT 128
#endif

#define IDENT_ASM_OP "\t.ident\t"

/* Switch into a generic section.  */
#define TARGET_ASM_NAMED_SECTION default_coff_asm_named_section

/* The prefix to add to user-visible assembler symbols.  */

#define USER_LABEL_PREFIX "_"

/* The prefix to add to an internally generated label.  */

#define LOCAL_LABEL_PREFIX ""

/* Make an internal label into a string.  */
#define ASM_GENERATE_INTERNAL_LABEL(STRING, PREFIX, NUM) \
  sprintf ((STRING), "*%s%s%ld", LOCAL_LABEL_PREFIX, (PREFIX), (long)(NUM))

/* Output an internal label definition.  */
#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM) \
  asm_fprintf ((FILE), "%L%s%d:\n", (PREFIX), (NUM))

/* This is how to output an assembler line
   that says to advance the location counter by SIZE bytes.  */

#define ASM_OUTPUT_SKIP(FILE,SIZE) \
  fprintf ((FILE), "\t.space %d\n", (SIZE))

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)	\
( fputs ("\t.comm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%d\n", (SIZE)))

/* This says how to output an assembler line
   to define a local common symbol.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)	\
( fputs ("\t.lcomm ", (FILE)),				\
  assemble_name ((FILE), (NAME)),			\
  fprintf ((FILE), ",%d\n", (SIZE)))

#define TARGET_OBJFMT_CPP_BUILTINS()

#define DWARF2_UNWIND_INFO 0
