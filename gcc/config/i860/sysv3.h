/* Target definitions for GNU compiler for Intel 80860 running System V.3
   Copyright (C) 1991, 1996 Free Software Foundation, Inc.
   Contributed by Ron Guilmette (rfg@monkeys.com).

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

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (i860, System V Release 3)")

/* Provide a set of pre-definitions and pre-assertions appropriate for
   the i860 running svr3.  */
#define CPP_PREDEFINES "-Di860 -Dunix -D__svr3__ -Asystem(unix) -Asystem(svr3) -Acpu(i860) -Amachine(i860)"

/* Use crt1.o as a startup file and crtn.o as a closing file.  */

#define STARTFILE_SPEC  \
  "%{pg:gcrt1.o%s}%{!pg:%{p:mcrt1.o%s}%{!p:crt1.o%s}}"

#define LIB_SPEC "%{p:-L/usr/lib/libp}%{pg:-L/usr/lib/libp} -lc crtn.o%s"

/* Special flags for the linker.  I don't know what they do.  */

#define LINK_SPEC "%{T*} %{z:-lm}"

/* The prefix to be used in assembler output for all names of registers.
   None is needed in V.3.  */

#define I860_REG_PREFIX	""

/* Delimiter that starts comments in the assembler code.  */

#define ASM_COMMENT_START "//"

/* Don't renumber the regusters for debugger output.  */

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* Output the special word the System V SDB wants to see just before
   the first word of each function's prologue code.  */

extern char *current_function_original_name;

/* This special macro is used to output a magic word just before the
   first word of each function.  On some versions of UNIX running on
   the i860, this word can be any word that looks like a NOP, however
   under svr4, this neds to be an `shr r0,r0,r0' instruction in which
   the normally unused low-order bits contain the length of the function
   prologue code (in bytes).  This is needed to make the System V SDB
   debugger happy.  */

#undef ASM_OUTPUT_FUNCTION_PREFIX
#define ASM_OUTPUT_FUNCTION_PREFIX(FILE, FNNAME)			\
  do {	ASM_OUTPUT_ALIGN (FILE, 2);					\
  	fprintf ((FILE), "\t.long\t.ep.");				\
	assemble_name (FILE, FNNAME);					\
	fprintf (FILE, "-");						\
	assemble_name (FILE, FNNAME);					\
	fprintf (FILE, "+0xc8000000\n");				\
	current_function_original_name = (FNNAME);			\
  } while (0)

/* Output the special label that must go just after each function's
   prologue code to support svr4 SDB.  */

#define ASM_OUTPUT_PROLOGUE_SUFFIX(FILE)				\
  do {	fprintf (FILE, ".ep.");						\
	assemble_name (FILE, current_function_original_name);		\
	fprintf (FILE, ":\n");						\
  } while (0)

/* This says how to output an assembler line
   to define a local common symbol.
   The difference from svr3.h is we don't limit align to 2.  */

#undef ASM_OUTPUT_LOCAL
#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)	\
  do {							\
    int align = exact_log2 (ROUNDED);			\
    data_section ();					\
    ASM_OUTPUT_ALIGN ((FILE), align == -1 ? 2 : align);	\
    ASM_OUTPUT_LABEL ((FILE), (NAME));			\
    fprintf ((FILE), "\t.set .,.+%u\n", (ROUNDED));	\
  } while (0)

/* The routine used to output string literals.

#define ASCII_DATA_ASM_OP	".byte"

#define ASM_OUTPUT_ASCII(FILE, STR, LENGTH)				\
  do									\
    {									\
      register const unsigned char *str = (const unsigned char *) (STR); \
      register const unsigned char *limit = str + (LENGTH);		\
      register unsigned bytes_in_chunk = 0;				\
      for (; str < limit; str++)					\
        {								\
          register unsigned ch = *str;					\
          if (ch < 32 || ch == '\\' || ch == '"' || ch >= 127)		\
	    {								\
	      if (bytes_in_chunk > 0)					\
	        {							\
	          fprintf ((FILE), "\"\n");				\
	          bytes_in_chunk = 0;					\
	        }							\
	      fprintf ((FILE), "\t%s\t%d\n", ASM_BYTE_OP, ch);		\
	    }								\
          else								\
	    {								\
	      if (bytes_in_chunk >= 60)					\
	        {							\
	          fprintf ((FILE), "\"\n");				\
	          bytes_in_chunk = 0;					\
	        }							\
	      if (bytes_in_chunk == 0)					\
	        fprintf ((FILE), "\t%s\t\"", ASCII_DATA_ASM_OP);	\
	      putc (ch, (FILE));					\
	      bytes_in_chunk++;						\
	    }								\
        }								\
      if (bytes_in_chunk > 0)						\
        fprintf ((FILE), "\"\n");					\
    }									\
  while (0)


#undef CTORS_SECTION_ASM_OP
#define CTORS_SECTION_ASM_OP	".section\t.ctors,\"x\""
#undef DTORS_SECTION_ASM_OP
#define DTORS_SECTION_ASM_OP	".section\t.dtors,\"x\""

/* Add definitions to support the .tdesc section as specified in the svr4
   ABI for the i860.  */

#define TDESC_SECTION_ASM_OP    ".section\t.tdesc"

#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS in_const, in_ctors, in_dtors, in_tdesc

#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS						\
  CONST_SECTION_FUNCTION						\
  CTORS_SECTION_FUNCTION						\
  DTORS_SECTION_FUNCTION						\
  TDESC_SECTION_FUNCTION

#define TDESC_SECTION_FUNCTION						\
void									\
tdesc_section ()							\
{									\
  if (in_section != in_tdesc)						\
    {									\
      fprintf (asm_out_file, "%s\n", TDESC_SECTION_ASM_OP);		\
      in_section = in_tdesc;						\
    }									\
}

/* Enable the `const' section that svr3.h defines how to use.  */
#undef USE_CONST_SECTION
#define USE_CONST_SECTION	1
