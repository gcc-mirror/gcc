/* Definitions of target machine for GNU compiler.  MIPS R4000 version with
   GOFAST floating point library.
   Copyright (C) 1994, 1995 Free Software Foundation, Inc.

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

#ifndef TARGET_ENDIAN_DEFAULT
#define MULTILIB_DEFAULTS { "EB", "mips3" }
#else
#define MULTILIB_DEFAULTS { "EL", "mips3" }
#endif

/* Until we figure out what MIPS ELF targets normally use, just do
   stabs in ELF.  */
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

#include "mips/mips.h"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dmips -DMIPSEB -DR4000 -D_mips -D_MIPSEB -D_R4000"

/* This is the same as the one in mips64.h except that it defines __mips=3
   at the end.  I would rather put this in CPP_PREDEFINES, but the gcc
   driver doesn't handle -U options in CPP_PREDEFINES.  */
#undef CPP_SPEC
#define CPP_SPEC "\
%{.cc:	-D__LANGUAGE_C_PLUS_PLUS -D_LANGUAGE_C_PLUS_PLUS} \
%{.cxx:	-D__LANGUAGE_C_PLUS_PLUS -D_LANGUAGE_C_PLUS_PLUS} \
%{.C:	-D__LANGUAGE_C_PLUS_PLUS -D_LANGUAGE_C_PLUS_PLUS} \
%{.m:	-D__LANGUAGE_OBJECTIVE_C -D_LANGUAGE_OBJECTIVE_C} \
%{.S:	-D__LANGUAGE_ASSEMBLY -D_LANGUAGE_ASSEMBLY %{!ansi:-DLANGUAGE_ASSEMBLY}} \
%{.s:	-D__LANGUAGE_ASSEMBLY -D_LANGUAGE_ASSEMBLY %{!ansi:-DLANGUAGE_ASSEMBLY}} \
%{!.S:%{!.s:	-D__LANGUAGE_C -D_LANGUAGE_C %{!ansi:-DLANGUAGE_C}}} \
%{mlong64:-D__SIZE_TYPE__=long\\ unsigned\\ int -D__PTRDIFF_TYPE__=long\\ int} \
%{!mlong64:-D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int} \
%{!mips1:%{!mips2:-U__mips -D__mips=3 -D__mips64}} \
%{mgp32:-U__mips64} %{mgp64:-D__mips64} \
%{EB:-UMIPSEL -U_MIPSEL -U__MIPSEL -U__MIPSEL__ -D_MIPSEB -D__MIPSEB -D__MIPSEB__ %{!ansi:-DMIPSEB}} \
%{EL:-UMIPSEB -U_MIPSEB -U__MIPSEB -U__MIPSEB__ -D_MIPSEL -D__MIPSEL -D__MIPSEL__ %{!ansi:-DMIPSEL}}"

/* Use memcpy, et. al., rather than bcopy.  */
#define TARGET_MEM_FUNCTIONS

/* US Software GOFAST library support.  */
#include "gofast.h"
#define INIT_TARGET_OPTABS INIT_GOFAST_OPTABS

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

#define ASM_OUTPUT_SECTION_NAME(F, DECL, NAME) \
do {								\
  extern FILE *asm_out_text_file;				\
  if ((DECL) && TREE_CODE (DECL) == FUNCTION_DECL)		\
    fprintf (asm_out_text_file, "\t.section %s,\"ax\",@progbits\n", (NAME)); \
  else if ((DECL) && TREE_READONLY (DECL))			\
    fprintf (F, "\t.section %s,\"a\",@progbits\n", (NAME));	\
  else								\
    fprintf (F, "\t.section %s,\"aw\",@progbits\n", (NAME));	\
} while (0)
