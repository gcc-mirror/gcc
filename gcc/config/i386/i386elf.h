/* Target definitions for GNU compiler for Intel 80386 using ELF
   Copyright (C) 1988, 1991, 1995 Free Software Foundation, Inc.

   Derived from sysv4.h written by Ron Guilmette (rfg@netcom.com).

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

/* Use stabs instead of DWARF debug format.  */
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

#include "i386/i386.h"
#include "i386/att.h"
#include "elfos.h"

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (i386 bare ELF target)");

/* By default, target has a 80387, uses IEEE compatible arithmetic,
   and returns float values in the 387.  */

#define TARGET_DEFAULT (MASK_80387 | MASK_IEEE_FP | MASK_FLOAT_RETURNS)

/* The svr4 ABI for the i386 says that records and unions are returned
   in memory.  */

#undef RETURN_IN_MEMORY
#define RETURN_IN_MEMORY(TYPE) \
  (TYPE_MODE (TYPE) == BLKmode)

/* Define which macros to predefine.  __svr4__ is our extension.  */
/* This used to define X86, but james@bigtex.cactus.org says that
   is supposed to be defined optionally by user programs--not by default.  */
#define CPP_PREDEFINES \
  "-D__i386__ -Acpu(i386) -Amachine(i386)"

#undef CPP_SPEC
#define CPP_SPEC "%(cpp_cpu)"

/* This is how to output assembly code to define a `float' constant.
   We always have to use a .long pseudo-op to do this because the native
   SVR4 ELF assembler is buggy and it generates incorrect values when we
   try to use the .float pseudo-op instead.  */

#undef ASM_OUTPUT_FLOAT
#define ASM_OUTPUT_FLOAT(FILE,VALUE)					\
do { long value;							\
     REAL_VALUE_TO_TARGET_SINGLE ((VALUE), value);			\
     if (sizeof (int) == sizeof (long))					\
         fprintf((FILE), "%s\t0x%x\n", ASM_LONG, value);		\
     else								\
         fprintf((FILE), "%s\t0x%lx\n", ASM_LONG, value);		\
   } while (0)

/* This is how to output assembly code to define a `double' constant.
   We always have to use a pair of .long pseudo-ops to do this because
   the native SVR4 ELF assembler is buggy and it generates incorrect
   values when we try to use the the .double pseudo-op instead.  */

#undef ASM_OUTPUT_DOUBLE
#define ASM_OUTPUT_DOUBLE(FILE,VALUE)					\
do { long value[2];							\
     REAL_VALUE_TO_TARGET_DOUBLE ((VALUE), value);			\
     if (sizeof (int) == sizeof (long))					\
       {								\
         fprintf((FILE), "%s\t0x%x\n", ASM_LONG, value[0]);		\
         fprintf((FILE), "%s\t0x%x\n", ASM_LONG, value[1]);		\
       }								\
     else								\
       {								\
         fprintf((FILE), "%s\t0x%lx\n", ASM_LONG, value[0]);		\
         fprintf((FILE), "%s\t0x%lx\n", ASM_LONG, value[1]);		\
       }								\
   } while (0)


#undef ASM_OUTPUT_LONG_DOUBLE
#define ASM_OUTPUT_LONG_DOUBLE(FILE,VALUE)				\
do { long value[3];							\
     REAL_VALUE_TO_TARGET_LONG_DOUBLE ((VALUE), value);			\
     if (sizeof (int) == sizeof (long))					\
       {								\
         fprintf((FILE), "%s\t0x%x\n", ASM_LONG, value[0]);		\
         fprintf((FILE), "%s\t0x%x\n", ASM_LONG, value[1]);		\
         fprintf((FILE), "%s\t0x%x\n", ASM_LONG, value[2]);		\
       }								\
     else								\
       {								\
         fprintf((FILE), "%s\t0x%lx\n", ASM_LONG, value[0]);		\
         fprintf((FILE), "%s\t0x%lx\n", ASM_LONG, value[1]);		\
         fprintf((FILE), "%s\t0x%lx\n", ASM_LONG, value[2]);		\
       }								\
   } while (0)

/* Output at beginning of assembler file.  */
/* The .file command should always begin the output.  */

#undef ASM_FILE_START
#define ASM_FILE_START(FILE)						\
  do {									\
	output_file_directive (FILE, main_input_filename);		\
	fprintf (FILE, "\t.version\t\"01.01\"\n");			\
  } while (0)

#undef DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(n)  svr4_dbx_register_map[n]

/* The routine used to output sequences of byte values.  We use a special
   version of this for most svr4 targets because doing so makes the
   generated assembly code more compact (and thus faster to assemble)
   as well as more readable.  Note that if we find subparts of the
   character sequence which end with NUL (and which are shorter than
   STRING_LIMIT) we output those using ASM_OUTPUT_LIMITED_STRING.  */

#undef ASM_OUTPUT_ASCII
#define ASM_OUTPUT_ASCII(FILE, STR, LENGTH)				\
  do									\
    {									\
      register unsigned char *_ascii_bytes = (unsigned char *) (STR);	\
      register unsigned char *limit = _ascii_bytes + (LENGTH);		\
      register unsigned bytes_in_chunk = 0;				\
      for (; _ascii_bytes < limit; _ascii_bytes++)			\
        {								\
	  register unsigned char *p;					\
	  if (bytes_in_chunk >= 64)					\
	    {								\
	      fputc ('\n', (FILE));					\
	      bytes_in_chunk = 0;					\
	    }								\
	  for (p = _ascii_bytes; p < limit && *p != '\0'; p++)		\
	    continue;							\
	  if (p < limit && (p - _ascii_bytes) <= STRING_LIMIT)		\
	    {								\
	      if (bytes_in_chunk > 0)					\
		{							\
		  fputc ('\n', (FILE));					\
		  bytes_in_chunk = 0;					\
		}							\
	      ASM_OUTPUT_LIMITED_STRING ((FILE), _ascii_bytes);		\
	      _ascii_bytes = p;						\
	    }								\
	  else								\
	    {								\
	      if (bytes_in_chunk == 0)					\
		fprintf ((FILE), "\t.byte\t");				\
	      else							\
		fputc (',', (FILE));					\
	      fprintf ((FILE), "0x%02x", *_ascii_bytes);		\
	      bytes_in_chunk += 5;					\
	    }								\
	}								\
      if (bytes_in_chunk > 0)						\
        fprintf ((FILE), "\n");						\
    }									\
  while (0)

/* This is how to output an element of a case-vector that is relative.
   This is only used for PIC code.  See comments by the `casesi' insn in
   i386.md for an explanation of the expression this outputs. */

#undef ASM_OUTPUT_ADDR_DIFF_ELT
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL) \
  fprintf (FILE, "\t.long _GLOBAL_OFFSET_TABLE_+[.-%s%d]\n", LPREFIX, VALUE)

/* Indicate that jump tables go in the text section.  This is
   necessary when compiling PIC code.  */

#define JUMP_TABLES_IN_TEXT_SECTION 1

#define LOCAL_LABEL_PREFIX	"."

/* A C statement to output something to the assembler file to switch to section
   NAME for object DECL which is either a FUNCTION_DECL, a VAR_DECL or
   NULL_TREE.  Some target formats do not support arbitrary sections.  Do not
   define this macro in such cases.  */

#undef  ASM_OUTPUT_SECTION_NAME
#define ASM_OUTPUT_SECTION_NAME(FILE, DECL, NAME, RELOC) \
do {									\
  if ((DECL) && TREE_CODE (DECL) == FUNCTION_DECL)			\
    fprintf (FILE, ".section\t%s,\"ax\"\n", (NAME));			\
  else if ((DECL) && DECL_READONLY_SECTION (DECL, RELOC))		\
    fprintf (FILE, ".section\t%s,\"a\"\n", (NAME));			\
  else									\
    fprintf (FILE, ".section\t%s,\"aw\"\n", (NAME));			\
} while (0)

/* If defined, a C expression whose value is a string containing the
   assembler operation to identify the following data as
   uninitialized global data.  If not defined, and neither
   `ASM_OUTPUT_BSS' nor `ASM_OUTPUT_ALIGNED_BSS' are defined,
   uninitialized global data will be output in the data section if
   `-fno-common' is passed, otherwise `ASM_OUTPUT_COMMON' will be
   used.  */
#undef BSS_SECTION_ASM_OP
#define BSS_SECTION_ASM_OP ".section\t.bss"

/* Like `ASM_OUTPUT_BSS' except takes the required alignment as a
   separate, explicit argument.  If you define this macro, it is used
   in place of `ASM_OUTPUT_BSS', and gives you more flexibility in
   handling the required alignment of the variable.  The alignment is
   specified as the number of bits.

   Try to use function `asm_output_aligned_bss' defined in file
   `varasm.c' when defining this macro. */
#undef ASM_OUTPUT_ALIGNED_BSS
#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
  asm_output_aligned_bss (FILE, DECL, NAME, SIZE, ALIGN)
