/* Target definitions for GNU compiler for Intel 80860 running System V.4
   Copyright (C) 1991, 1996, 2000 Free Software Foundation, Inc.
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
#define TARGET_VERSION fprintf (stderr, " (i860 System V Release 4)");

/* Provide a set of pre-definitions and pre-assertions appropriate for
   the i860 running svr4.  Note that the symbol `__svr4__' MUST BE
   DEFINED!  It is needed so that the va_list struct in va-i860.h
   will get correctly defined for the svr4 (ABI compliant) case rather
   than for the previous (svr3, svr2, ...) case.  It also needs to be
   defined so that the correct (svr4) version of __builtin_saveregs
   will be selected when we are building gnulib2.c.
   __svr4__ is our extension.  */

#define CPP_PREDEFINES \
  "-Di860 -Dunix -DSVR4 -D__svr4__ -Asystem=unix -Asystem=svr4 -Acpu=i860 -Amachine=i860"

/* For the benefit of i860_va_arg, flag it this way too.  */

#define I860_SVR4_VA_LIST 1

/* The prefix to be used in assembler output for all names of registers.
   This string gets prepended to all i860 register names (svr4 only).  */

#define I860_REG_PREFIX	"%"

#define ASM_COMMENT_START "#"

#undef TYPE_OPERAND_FMT
#define TYPE_OPERAND_FMT      "\"%s\""

/* The following macro definition overrides the one in i860.h
   because the svr4 i860 assembler requires a different syntax
   for getting parts of constant/relocatable values.  */

#undef PRINT_OPERAND_PART
#define PRINT_OPERAND_PART(FILE, X, PART_CODE)				\
  do { fprintf (FILE, "[");						\
	output_address (X);						\
	fprintf (FILE, "]@%s", PART_CODE);				\
  } while (0)

#undef ASM_FILE_START
#define ASM_FILE_START(FILE)						\
  do {	output_file_directive (FILE, main_input_filename);		\
	fprintf (FILE, "\t.version\t\"01.01\"\n");			\
  } while (0)

/* Output the special word the svr4 SDB wants to see just before
   the first word of each function's prologue code.  */

extern const char *current_function_original_name;

/* This special macro is used to output a magic word just before the
   first word of each function.  On some versions of UNIX running on
   the i860, this word can be any word that looks like a NOP, however
   under svr4, this neds to be an `shr r0,r0,r0' instruction in which
   the normally unused low-order bits contain the length of the function
   prologue code (in bytes).  This is needed to make the svr4 SDB debugger
   happy.  */

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

/* Define the pseudo-ops used to switch to the .ctors and .dtors sections.
 
   Note that we want to give these sections the SHF_WRITE attribute
   because these sections will actually contain data (i.e. tables of
   addresses of functions in the current root executable or shared library
   file) and, in the case of a shared library, the relocatable addresses
   will have to be properly resolved/relocated (and then written into) by
   the dynamic linker when it actually attaches the given shared library
   to the executing process.  (Note that on SVR4, you may wish to use the
   `-z text' option to the ELF linker, when building a shared library, as
   an additional check that you are doing everything right.  But if you do
   use the `-z text' option when building a shared library, you will get
   errors unless the .ctors and .dtors sections are marked as writable
   via the SHF_WRITE attribute.)  */
 
#undef CTORS_SECTION_ASM_OP
#define CTORS_SECTION_ASM_OP	"\t.section\t.ctors,\"aw\""
#undef DTORS_SECTION_ASM_OP
#define DTORS_SECTION_ASM_OP	"\t.section\t.dtors,\"aw\""

/* Add definitions to support the .tdesc section as specified in the svr4
   ABI for the i860.  */

#define TDESC_SECTION_ASM_OP    "\t.section\t.tdesc"

#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS in_const, in_tdesc

#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS						\
  CONST_SECTION_FUNCTION						\
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

