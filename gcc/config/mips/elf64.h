/* Definitions of target machine for GNU compiler.  MIPS R4000 version with
   GOFAST floating point library.
   Copyright (C) 1994, 1995, 1996 Free Software Foundation, Inc.

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

#define OBJECT_FORMAT_ELF

/* Default to -mips3.  */
#define TARGET_DEFAULT MASK_FLOAT64|MASK_64BIT
#define MIPS_ISA_DEFAULT 3

#ifndef MULTILIB_DEFAULTS
#ifndef TARGET_ENDIAN_DEFAULT
#define MULTILIB_DEFAULTS { "EB", "mips3" }
#else
#define MULTILIB_DEFAULTS { "EL", "mips3" }
#endif
#endif

/* Until we figure out what MIPS ELF targets normally use, just do
   stabs in ELF.  */
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

#include "mips/mips.h"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dmips -DMIPSEB -DR4000 -D_mips -D_MIPSEB -D_R4000"

/* I would rather put this in CPP_PREDEFINES, but the gcc driver
   doesn't handle -U options in CPP_PREDEFINES.  */
#undef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC "\
%{!mips1:%{!mips2:-U__mips -D__mips=3 -D__mips64}}"

/* Use memcpy, et. al., rather than bcopy.  */
#define TARGET_MEM_FUNCTIONS

/* US Software GOFAST library support.  */
#include "gofast.h"
#define INIT_TARGET_OPTABS INIT_GOFAST_OPTABS

/* Biggest alignment supported by the object file format of this
   machine.  Use this macro to limit the alignment which can be
   specified using the `__attribute__ ((aligned (N)))' construct.  If
   not defined, the default value is `BIGGEST_ALIGNMENT'.  */

#define MAX_OFILE_ALIGNMENT (32768*8)

/* We need to use .esize and .etype instead of .size and .type to
   avoid conflicting with ELF directives.  */
#undef PUT_SDB_SIZE
#define PUT_SDB_SIZE(a)					\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file, "\t.esize\t%d;", (a));	\
} while (0)

#undef PUT_SDB_TYPE
#define PUT_SDB_TYPE(a)					\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file, "\t.etype\t0x%x;", (a));	\
} while (0)

/* A C statement to output something to the assembler file to switch to section
   NAME for object DECL which is either a FUNCTION_DECL, a VAR_DECL or
   NULL_TREE.  Some target formats do not support arbitrary sections.  Do not
   define this macro in such cases.  */

#define ASM_OUTPUT_SECTION_NAME(F, DECL, NAME, RELOC) \
do {								\
  extern FILE *asm_out_text_file;				\
  if ((DECL) && TREE_CODE (DECL) == FUNCTION_DECL)		\
    fprintf (asm_out_text_file, "\t.section %s,\"ax\",@progbits\n", (NAME)); \
  else if ((DECL) && DECL_READONLY_SECTION (DECL, RELOC))	\
    fprintf (F, "\t.section %s,\"a\",@progbits\n", (NAME));	\
  else								\
    fprintf (F, "\t.section %s,\"aw\",@progbits\n", (NAME));	\
} while (0)
