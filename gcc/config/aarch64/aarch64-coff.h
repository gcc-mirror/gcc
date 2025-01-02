/* Machine description for AArch64 architecture.
   Copyright (C) 2024-2025 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_AARCH64_COFF_H
#define GCC_AARCH64_COFF_H

#undef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX  "."

/* Using long long breaks -ansi and -std=c90, so these will need to be
   made conditional for an LLP64 ABI.  */
#undef SIZE_TYPE
#define SIZE_TYPE	"long long unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE	"long long int"

#undef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE 32

#ifndef ASM_GENERATE_INTERNAL_LABEL
# define ASM_GENERATE_INTERNAL_LABEL(STRING, PREFIX, NUM)  \
  sprintf (STRING, "*%s%s%u", LOCAL_LABEL_PREFIX, PREFIX, (unsigned int)(NUM))
#endif

#define ASM_OUTPUT_ALIGN(STREAM, POWER)		\
  fprintf (STREAM, "\t.align\t%d\n", (int)POWER)

/* Output a common block.  */
#ifndef ASM_OUTPUT_COMMON
# define ASM_OUTPUT_COMMON(STREAM, NAME, SIZE, ROUNDED)	\
    {							\
      fprintf (STREAM, "\t.comm\t");			\
      assemble_name (STREAM, NAME);			\
      asm_fprintf (STREAM, ", %d, %d\n", 		\
      (int)(ROUNDED), (int)(SIZE));	\
    }
#endif

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  \
( fputs (".lcomm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), "," HOST_WIDE_INT_PRINT_UNSIGNED "\n", (ROUNDED)))

#define ASM_OUTPUT_SKIP(STREAM, NBYTES) 	\
  fprintf (STREAM, "\t.space\t%d  // skip\n", (int) (NBYTES))

/* Definitions that are not yet supported by binutils for the
   aarch64-w64-mingw32 target.  */
#define ASM_OUTPUT_TYPE_DIRECTIVE(STREAM, NAME, TYPE)
#define ASM_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL)

#define TEXT_SECTION_ASM_OP	"\t.text"
#define DATA_SECTION_ASM_OP	"\t.data"
#define BSS_SECTION_ASM_OP	"\t.bss"

#define CTORS_SECTION_ASM_OP	"\t.section\t.ctors, \"aw\""
#define DTORS_SECTION_ASM_OP	"\t.section\t.dtors, \"aw\""

#define GLOBAL_ASM_OP "\t.global\t"

#undef SUPPORTS_INIT_PRIORITY
#define SUPPORTS_INIT_PRIORITY 0

#endif
