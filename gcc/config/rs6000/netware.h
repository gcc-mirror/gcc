/* Core target definitions for GNU compiler
   for IBM RS/6000 PowerPC running NetWare
   Copyright (C) 1994, 1995, 1996, 1998, 2000 Free Software Foundation, Inc.
   Contributed by Cygnus Support.

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

#define TARGET_AIX 0

#define CPP_DEFAULT_SPEC "-D_ARCH_PPC"

#define ASM_DEFAULT_SPEC "-mppc"

#include "rs6000/rs6000.h"

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_POWERPC | MASK_NEW_MNEMONICS)

#undef PROCESSOR_DEFAULT
#define PROCESSOR_DEFAULT PROCESSOR_PPC601

/* The XCOFF support uses weird symbol suffixes, which we don't want
   for ELF.  */

#undef STRIP_NAME_ENCODING

/* Undefine some things which are defined by the generic svr4.h.  */

#undef ASM_FILE_END
#undef ASM_OUTPUT_EXTERNAL_LIBCALL
#undef READONLY_DATA_SECTION
#undef SELECT_SECTION
#undef ASM_DECLARE_FUNCTION_NAME

/* Use the regular svr4 definitions.  */

#include "svr4.h"
#include "netware.h"

/* Create a function descriptor when we declare a function name.  This
   is a mixture of the ASM_DECLARE_FUNCTION_NAME macros in rs6000.h
   and svr4.h.  The unmodified function name is used to name the
   descriptor.  The function name with an initial `.' is used to name
   the code.  */

#undef ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)			\
  do {									\
    fprintf (FILE, "\t%s\t ", TYPE_ASM_OP);				\
    assemble_name (FILE, NAME);						\
    putc (',', FILE);							\
    fprintf (FILE, TYPE_OPERAND_FMT, "function");			\
    putc ('\n', FILE);							\
    ASM_DECLARE_RESULT (FILE, DECL_RESULT (DECL));			\
    if (TREE_PUBLIC (DECL))						\
      {									\
        fprintf (FILE, "\t.globl .");					\
	assemble_name (FILE, NAME);					\
        fprintf (FILE, "\n");						\
      }									\
    data_section ();							\
    ASM_OUTPUT_ALIGN (FILE, 2);						\
    ASM_OUTPUT_LABEL (FILE, NAME);					\
    fprintf (FILE, "\t.long .");					\
    assemble_name (FILE, NAME);						\
    fprintf (FILE, ", __GOT0, 0\n");					\
    text_section ();							\
    fprintf (FILE, ".");						\
    ASM_OUTPUT_LABEL (FILE, NAME);					\
  } while (0)

/* We need to override the .size output in order to put a `.' before
   the function name.  */

#undef ASM_DECLARE_FUNCTION_SIZE
#define ASM_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL)			\
  do {									\
    if (!flag_inhibit_size_directive)					\
      {									\
        char label[256];						\
	static int labelno;						\
	labelno++;							\
	ASM_GENERATE_INTERNAL_LABEL (label, "Lfe", labelno);		\
	ASM_OUTPUT_INTERNAL_LABEL (FILE, "Lfe", labelno);		\
	fprintf (FILE, "\t%s\t ", SIZE_ASM_OP);				\
	assemble_name (FILE, (FNAME));					\
        fprintf (FILE, ",");						\
	assemble_name (FILE, label);					\
        fprintf (FILE, "-.");						\
	assemble_name (FILE, (FNAME));					\
	putc ('\n', FILE);						\
      }									\
  } while (0)

/* Use ELF style section commands.  */

#undef TEXT_SECTION_ASM_OP
#define TEXT_SECTION_ASM_OP	"\t.section\t\".text\""

#undef DATA_SECTION_ASM_OP
#define DATA_SECTION_ASM_OP	"\t.section\t\".data\""

/* Besides the usual ELF sections, we need a toc section.  */

#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS in_const, in_ctors, in_dtors, in_toc

#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS						\
  CONST_SECTION_FUNCTION						\
  CTORS_SECTION_FUNCTION						\
  DTORS_SECTION_FUNCTION						\
  TOC_SECTION_FUNCTION

#define TOC_SECTION_FUNCTION						\
void									\
toc_section ()								\
{									\
  if (TARGET_MINIMAL_TOC)						\
    {									\
      static int toc_initialized = 0;					\
									\
      if (! toc_initialized)						\
	{								\
	  fprintf (asm_out_file, "%s\n", TOC_SECTION_ASM_OP);		\
	  fprintf (asm_out_file, ".LCTOC0:\n");				\
	  fprintf (asm_out_file, "\t.tc .LCTOC1\n");			\
	  fprintf (asm_out_file, "%s\n", MINIMAL_TOC_SECTION_ASM_OP);	\
	  fprintf (asm_out_file, ".LCTOC1:\n");				\
	  toc_initialized = 1;						\
	}								\
    }									\
									\
  if (in_section != in_toc)						\
    {									\
      fprintf (asm_out_file, "%s\n",					\
	       (TARGET_MINIMAL_TOC					\
		? MINIMAL_TOC_SECTION_ASM_OP				\
		: TOC_SECTION_ASM_OP));					\
      in_section = in_toc;						\
    }									\
}

#define TOC_SECTION_ASM_OP "\t.section\t.got,\"aw\""
#define MINIMAL_TOC_SECTION_ASM_OP "\t.section\t.got1,\"aw\""

/* Use the TOC section for TOC entries.  */

#undef SELECT_RTX_SECTION
#define SELECT_RTX_SECTION(MODE, X)		\
{ if (ASM_OUTPUT_SPECIAL_POOL_ENTRY_P (X))	\
    toc_section ();				\
  else						\
    const_section ();				\
}

/* How to renumber registers for dbx and gdb.  */

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* svr4.h overrides ASM_OUTPUT_INTERNAL_LABEL.  */

#undef ASM_OUTPUT_INTERNAL_LABEL_PREFIX
#define ASM_OUTPUT_INTERNAL_LABEL_PREFIX(FILE,PREFIX)	\
  fprintf (FILE, ".%s", PREFIX)

#undef ASM_SPEC
#define ASM_SPEC "-u %(asm_cpu) \
{v:-V} %{Qy:} %{!Qn:-Qy} %{n} %{T} %{Ym,*} %{Yd,*} %{Wa,*:%*}"
/* This is the end of what might become sysv4.h.  */

/* Enable output of DBX (stabs) debugging information when asked for it.  */

#define DBX_DEBUGGING_INFO

/* Prefer DBX (stabs) debugging information over the native (DWARF) format. */

#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

/* Line numbers are relative to the current function.  */

#undef  ASM_OUTPUT_SOURCE_LINE
#define ASM_OUTPUT_SOURCE_LINE(file, line)		\
  { static int sym_lineno = 1;				\
    fprintf (file, ".stabn 68,0,%d,.LM%d-.%s\n.LM%d:\n",\
	     line, sym_lineno, 				\
	     XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0), \
	     sym_lineno);				\
    sym_lineno += 1; }

/* But, to make this work, we have to output the stabs for the function
   name *first*...  */

#define	DBX_FUNCTION_FIRST

/* We need to output LBRAC and RBRAC lines specially to include the
   dot in from of the text symbol for a function.  */

#define DBX_OUTPUT_LBRAC(FILE, BUF)					   \
do									   \
  {									   \
    fprintf (FILE, "%s %d,0,0,", ASM_STABN_OP, N_LBRAC);		   \
    assemble_name (FILE, BUF);						   \
    fprintf (FILE, "-.");						   \
    assemble_name (asmfile,						   \
		   XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0));  \
    fprintf (asmfile, "\n");						   \
  }									   \
while (0)

#define DBX_OUTPUT_RBRAC(FILE, BUF)					   \
do									   \
  {									   \
    fprintf (FILE, "%s %d,0,0,", ASM_STABN_OP, N_RBRAC);		   \
    assemble_name (FILE, BUF);						   \
    fprintf (FILE, "-.");						   \
    assemble_name (asmfile,						   \
		   XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0));  \
    fprintf (asmfile, "\n");						   \
  }									   \
while (0)

/* We are using function descriptors, so the value of a function
   symbol is in the .data section.  However, we want the stabs entry
   for that function to point at the actual function code in the .text
   section, which we get by prefixing the symbol with a dot.  */

#define DBX_FINISH_SYMBOL(sym)						\
do {									\
  int line = 0;								\
  if (use_gnu_debug_info_extensions && sym != 0)			\
    line = DECL_SOURCE_LINE (sym);					\
									\
  fprintf (asmfile, "\",%d,0,%d,", current_sym_code, line);		\
  if (current_sym_addr)							\
    {									\
      if (TREE_CODE (sym) == FUNCTION_DECL)				\
	fprintf (asmfile, ".");						\
      output_addr_const (asmfile, current_sym_addr);			\
    }									\
  else									\
    fprintf (asmfile, "%d", current_sym_value);				\
  putc ('\n', asmfile);							\
} while (0)

/* This is the end of what might become sysv4dbx.h.  */

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (PowerPC Netware)");

/* FIXME: These should actually indicate PowerPC, when there is some
   standard way of expressing that.  */
#undef CPP_PREDEFINES
#define CPP_PREDEFINES \
  "-DPPC D__netware__ -Asystem(netware) -Acpu(powerpc) -Amachine(powerpc)"
