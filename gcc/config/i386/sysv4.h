/* Target definitions for GNU compiler for Intel 80386 running System V.4
   Copyright (C) 1991 Free Software Foundation, Inc.

   Written by Ron Guilmette (rfg@ncd.com).

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

#include "i386.h"	/* Base i386 target machine definitions */
#include "att386.h"	/* Use the i386 AT&T assembler syntax */
#include "svr4.h"	/* Definitions common to all SVR4 targets */

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (i386 System V Release 4)");

/* By default, target has a 80387.  */

#define TARGET_DEFAULT 1

/* Machines that use the AT&T assembler syntax
   also return floating point values in an FP register.  */
/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */

#define VALUE_REGNO(MODE) \
  (((MODE) == SFmode || (MODE) == DFmode) ? FIRST_FLOAT_REG : 0)

/* 1 if N is a possible register number for a function value. */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == 0 || (N)== FIRST_FLOAT_REG)

/* The svr4 ABI for the i386 says that records and unions are returned
   in memory.  */

#undef RETURN_IN_MEMORY
#define RETURN_IN_MEMORY(TYPE) \
  (TREE_CODE (TYPE) == RECORD_TYPE || TREE_CODE(TYPE) == UNION_TYPE)

/* Define which macros to predefine.  __svr4__ is our extension.  */
/* This used to define X86, but james@bigtex.cactus.org says that
   is supposed to be defined optionally by user programs--not by default.  */
#define CPP_PREDEFINES \
  "-Di386 -Dunix -D__svr4__ -Asystem(unix) -Acpu(i386) -Amachine(i386)"

/* Output at beginning of assembler file.  */
/* The .file command should always begin the output.  */

#undef ASM_FILE_START
#define ASM_FILE_START(FILE)						\
  do {									\
	output_file_directive (FILE, main_input_filename);		\
	fprintf (FILE, "\t.version\t\"01.01\"\n");			\
  } while (0)

/* Define the register numbers to be used in Dwarf debugging information.
   The SVR4 reference port C compiler uses the following register numbers
   in its Dwarf output code:

	0 for %eax (regno = 0)
	1 for %ecx (regno = 2)
	2 for %edx (regno = 1)
	3 for %ebx (regno = 3)
	4 for %esp (regno = 7)
	5 for %ebp (regno = 6)
	6 for %esi (regno = 4)
	7 for %edi (regno = 5)

	8  for FP_REGS[tos]   (regno = 8)
	9  for FP_REGS[tos-1] (regno = 9)
	10 for FP_REGS[tos-2] (regno = 10)
	11 for FP_REGS[tos-3] (regno = 11)
	12 for FP_REGS[tos-4] (regno = 12)
	13 for FP_REGS[tos-5] (regno = 13)
	14 for FP_REGS[tos-6] (regno = 14)
	15 for FP_REGS[tos-7] (regno = 15)
*/

#undef DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(n) \
((n) == 0 ? 0 \
 : (n) == 1 ? 2 \
 : (n) == 2 ? 1 \
 : (n) == 3 ? 3 \
 : (n) == 4 ? 6 \
 : (n) == 5 ? 7 \
 : (n) == 6 ? 5 \
 : (n) == 7 ? 4 \
 : ((n) >= FIRST_STACK_REG && (n) <= LAST_STACK_REG) ? (n) \
 : (abort (), 0))

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
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, VALUE, REL) \
  fprintf (FILE, "\t.long _GLOBAL_OFFSET_TABLE_+[.-%s%d]\n", LPREFIX, VALUE)

/* Indicate that jump tables go in the text section.  This is
   necessary when compiling PIC code.  */

#define JUMP_TABLES_IN_TEXT_SECTION

#define WEAK_ASM_OP ".weak"
#define DEF_ASM_OP  ".set"

/* Biggest alignment that any structure field can require on this
   machine, in bits.  If packing is in effect, this can be smaller than
   normal.  */

#define BIGGEST_FIELD_ALIGNMENT \
  (maximum_field_alignment ? maximum_field_alignment : 32)

extern int maximum_field_alignment;

/* If bit field type is int, don't let it cross an int,
   and give entire struct the alignment of an int.  */
/* Required on the 386 since it doesn't have bitfield insns.  */
/* If packing is in effect, then the type doesn't matter.  */

#undef PCC_BITFIELD_TYPE_MATTERS
#define PCC_BITFIELD_TYPE_MATTERS (maximum_field_alignment == 0)

/* Code to handle #pragma directives.  The interface is a bit messy,
   but there's no simpler way to do this while still using yylex.  */
#define HANDLE_PRAGMA(FILE)					\
  do {								\
    while (c == ' ' || c == '\t')				\
      c = getc (FILE);						\
    if (c == '\n' || c == EOF)					\
      {								\
	handle_pragma_token (0, 0);				\
	return c;						\
      }								\
    ungetc (c, FILE);						\
    switch (yylex ())						\
      {								\
      case IDENTIFIER:						\
      case TYPENAME:						\
      case STRING:						\
      case CONSTANT:						\
	handle_pragma_token (token_buffer, yylval.ttype);	\
	break;							\
      default:							\
	handle_pragma_token (token_buffer, 0);			\
      }								\
    if (nextchar >= 0)						\
      c = nextchar, nextchar = -1;				\
    else							\
      c = getc (FILE);						\
  } while (1)
