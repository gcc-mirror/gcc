/* Target definitions for GNU compiler for PowerPC running System V.4
   Copyright (C) 1995, Free Software Foundation, Inc.
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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* eABI local switches -- put here rather than eabi.h, so the switches
   can be tested in macros.  */

#define	MASK_NO_BITFIELD_TYPE	0x40000000	/* Set PCC_BITFIELD_TYPE_MATTERS to 0 */
#define	MASK_STRICT_ALIGN	0x20000000	/* Set STRICT_ALIGNMENT to 1.  */
#define MASK_RELOCATABLE	0x10000000	/* GOT pointers are PC relative */
#define	MASK_NO_TRACEBACK	0x08000000	/* eliminate traceback words */

#define	TARGET_NO_BITFIELD_TYPE	(target_flags & MASK_NO_BITFIELD_TYPE)
#define	TARGET_BITFIELD_TYPE	(! TARGET_NO_BITFIELD_TYPE)
#define TARGET_STRICT_ALIGN	(target_flags & MASK_STRICT_ALIGN)
#define TARGET_RELOCATABLE	(target_flags & MASK_RELOCATABLE)
#define TARGET_NO_TRACEBACK	(target_flags & MASK_NO_TRACEBACK)
#define	TARGET_TRACEBACK	(! TARGET_NO_TRACEBACK)

#undef	SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES						\
  { "bit-align",	-MASK_NO_BITFIELD_TYPE },			\
  { "no-bit-align",	 MASK_NO_BITFIELD_TYPE },			\
  { "strict-align",	 MASK_STRICT_ALIGN },				\
  { "no-strict-align",	-MASK_STRICT_ALIGN },				\
  { "relocatable",	 MASK_RELOCATABLE | MASK_MINIMAL_TOC | MASK_NO_FP_IN_TOC }, \
  { "no-relocatable",	-MASK_RELOCATABLE },				\
  { "traceback",	-MASK_NO_TRACEBACK },				\
  { "no-traceback",	 MASK_NO_TRACEBACK },

#include "rs6000/powerpc.h"

/* Don't generate XCOFF debugging information.  */

#undef XCOFF_DEBUGGING_INFO

/* Don't use the COFF object file format.  */

#undef OBJECT_FORMAT_COFF

/* The XCOFF support uses weird symbol suffixes, which we don't want
   for ELF.  */

#undef RS6000_OUTPUT_BASENAME
#define RS6000_OUTPUT_BASENAME(FILE, NAME) assemble_name (FILE, NAME)

/* Don't bother to output .extern pseudo-ops.  They are not needed by
   ELF assemblers.  */

#undef ASM_OUTPUT_EXTERNAL

/* Undefine some things which are defined by the generic svr4.h.  */

#undef ASM_FILE_END
#undef ASM_OUTPUT_EXTERNAL_LIBCALL
#undef READONLY_DATA_SECTION
#undef SELECT_SECTION
#undef ASM_DECLARE_FUNCTION_NAME

/* Use the regular svr4 definitions.  */

#include "svr4.h"

/* Prefix and suffix to use to saving floating point */
#undef	SAVE_FP_PREFIX
#undef	SAVE_FP_SUFFIX
#define	SAVE_FP_PREFIX "_savefpr_"
#define SAVE_FP_SUFFIX "_l"

/* Prefix and suffix to use to restoring floating point */
#undef	RESTORE_FP_PREFIX
#undef	RESTORE_FP_SUFFIX
#define	RESTORE_FP_PREFIX "_restfpr_"
#define RESTORE_FP_SUFFIX "_l"

/* Type used for ptrdiff_t, as a string used in a declaration.  */
#undef	PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

/* Type used for wchar_t, as a string used in a declaration.  */
#undef	WCHAR_TYPE
#define WCHAR_TYPE "short unsigned int"

/* Width of wchar_t in bits.  */
#undef	WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 16

/* Align stack to 16 byte boundaries */
#undef	STACK_BOUNDARY
#define	STACK_BOUNDARY	128

/* No data type wants to be aligned rounder than this.  */
#undef	BIGGEST_ALIGNMENT
#define BIGGEST_ALIGNMENT 128

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
  static int toc_initialized = 0;					\
									\
  if (in_section != in_toc)						\
    {									\
      if (! toc_initialized)						\
	{								\
	  if (!TARGET_RELOCATABLE)					\
	    fprintf (asm_out_file, "%s\n", TOC_SECTION_ASM_OP);		\
									\
	  if (TARGET_MINIMAL_TOC)					\
	    {								\
	      if (!TARGET_RELOCATABLE)					\
		{							\
		  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "LCTOC", 0);	\
		  fprintf (asm_out_file, "\t.tc ");			\
		  ASM_OUTPUT_INTERNAL_LABEL_PREFIX (asm_out_file, "LCTOC1[TC],"); \
		  ASM_OUTPUT_INTERNAL_LABEL_PREFIX (asm_out_file, "LCTOC1"); \
		  fprintf (asm_out_file, "\n");				\
		}							\
									\
	      fprintf (asm_out_file, "%s\n", MINIMAL_TOC_SECTION_ASM_OP); \
	      ASM_OUTPUT_INTERNAL_LABEL_PREFIX (asm_out_file, "LCTOC1"); \
	      fprintf (asm_out_file, " = .+32768\n");			\
	    }								\
									\
	  toc_initialized = 1;						\
	}								\
									\
      else								\
	fprintf (asm_out_file, "%s\n",					\
		 (TARGET_MINIMAL_TOC					\
		  ? MINIMAL_TOC_SECTION_ASM_OP				\
		  : TOC_SECTION_ASM_OP));				\
									\
      in_section = in_toc;						\
    }									\
}

#define TOC_SECTION_ASM_OP "\t.section\t\".got\",\"aw\""
#define MINIMAL_TOC_SECTION_ASM_OP "\t.section\t\".got1\",\"aw\""

/* Use the TOC section for TOC entries.  */

#undef SELECT_RTX_SECTION
#define SELECT_RTX_SECTION(MODE, X)		\
{ if (ASM_OUTPUT_SPECIAL_POOL_ENTRY_P (X))	\
    toc_section ();				\
  else						\
    const_section ();				\
}

/* These macros generate the special .type and .size directives which
   are used to set the corresponding fields of the linker symbol table
   entries in an ELF object file under SVR4.  These macros also output
   the starting labels for the relevant functions/objects.  */

/* Write the extra assembler code needed to declare a function properly.
   Some svr4 assemblers need to also have something extra said about the
   function's return value.  We allow for that here.  */

extern void svr4_traceback ();
extern int rs6000_pic_labelno;

#undef	ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)			\
  do {									\
    if (TARGET_RELOCATABLE && get_pool_size () != 0)			\
      {									\
	char buf[256];							\
									\
	ASM_OUTPUT_INTERNAL_LABEL (FILE, "LCL", rs6000_pic_labelno);	\
									\
	ASM_GENERATE_INTERNAL_LABEL (buf, "LCTOC", 1);			\
	fprintf (FILE, (TARGET_POWERPC64) ? "\t.quad " : "\t.long ");	\
	assemble_name (FILE, buf);					\
	putc ('-', FILE);						\
									\
	ASM_GENERATE_INTERNAL_LABEL (buf, "LCF", rs6000_pic_labelno);	\
	assemble_name (FILE, buf);					\
	putc ('\n', FILE);						\
      }									\
									\
    fprintf (FILE, "\t%s\t ", TYPE_ASM_OP);				\
    assemble_name (FILE, NAME);						\
    putc (',', FILE);							\
    fprintf (FILE, TYPE_OPERAND_FMT, "function");			\
    putc ('\n', FILE);							\
    if (TARGET_TRACEBACK)						\
      svr4_traceback (FILE, NAME, DECL);				\
    ASM_DECLARE_RESULT (FILE, DECL_RESULT (DECL));			\
    ASM_OUTPUT_LABEL(FILE, NAME);					\
  } while (0)

/* How to renumber registers for dbx and gdb.  */

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* svr4.h overrides ASM_OUTPUT_INTERNAL_LABEL.  */

#undef ASM_OUTPUT_INTERNAL_LABEL_PREFIX
#define ASM_OUTPUT_INTERNAL_LABEL_PREFIX(FILE,PREFIX)	\
  fprintf (FILE, ".%s", PREFIX)

/* Pass -mppc to the assembler, since that is what powerpc.h currently
   implies.  */
#undef ASM_SPEC
#define ASM_SPEC \
  "-u -mppc %{V} %{v:%{!V:-V}} %{Qy:} %{!Qn:-Qy} %{n} %{T} %{Ym,*} %{Yd,*} %{Wa,*:%*} %{mrelocatable}"
/* This is the end of what might become sysv4.h.  */

/* Allow stabs and dwarf, prefer dwarf.  */
#define PREFERRED_DEBUGGING_TYPE DWARF_DEBUG
#define	DBX_DEBUGGING_INFO
#define	DWARF_DEBUGGING_INFO

/* Line numbers are relative to the current function.  */

#undef  ASM_OUTPUT_SOURCE_LINE
#define ASM_OUTPUT_SOURCE_LINE(file, line)		\
  { static int sym_lineno = 1;				\
    fprintf (file, ".stabn 68,0,%d,.LM%d-%s\n.LM%d:\n",\
	     line, sym_lineno, 				\
	     XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0), \
	     sym_lineno);				\
    sym_lineno += 1; }

/* But, to make this work, we have to output the stabs for the function
   name *first*...  */

#define	DBX_FUNCTION_FIRST

/* This is the end of what might become sysv4dbx.h.  */

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (PowerPC System V.4)");

#undef CPP_PREDEFINES
#define CPP_PREDEFINES \
  "-DPPC -Dunix -D__svr4__ -Asystem(unix) -Asystem(svr4) -Acpu(powerpc) -Amachine(powerpc)"

#undef CPP_SPEC
#define CPP_SPEC "\
%{posix: -D_POSIX_SOURCE} \
%{mrelocatable: -D_RELOCATABLE} \
%{!mcpu*: \
  %{mpower: %{!mpower2: -D_ARCH_PWR}} \
  %{mpower2: -D_ARCH_PWR2} \
  %{mpowerpc*: -D_ARCH_PPC} \
  %{mno-powerpc: %{!mpower: %{!mpower2: -D_ARCH_COM}}} \
  %{!mno-powerpc: -D_ARCH_PPC}} \
%{mcpu=common: -D_ARCH_COM} \
%{mcpu=power: -D_ARCH_PWR} \
%{mcpu=powerpc: -D_ARCH_PPC} \
%{mcpu=rios: -D_ARCH_PWR} \
%{mcpu=rios1: -D_ARCH_PWR} \
%{mcpu=rios2: -D_ARCH_PWR2} \
%{mcpu=rsc: -D_ARCH_PWR} \
%{mcpu=rsc1: -D_ARCH_PWR} \
%{mcpu=403: -D_ARCH_PPC} \
%{mcpu=mpc403: -D_ARCH_PPC} \
%{mcpu=ppc403: -D_ARCH_PPC} \
%{mcpu=601: -D_ARCH_PPC -D_ARCH_PWR} \
%{mcpu=mpc601: -D_ARCH_PPC -D_ARCH_PWR} \
%{mcpu=ppc601: -D_ARCH_PPC -D_ARCH_PWR} \
%{mcpu=603: -D_ARCH_PPC} \
%{mcpu=mpc603: -D_ARCH_PPC} \
%{mcpu=ppc603: -D_ARCH_PPC} \
%{mcpu=604: -D_ARCH_PPC} \
%{mcpu=mpc604: -D_ARCH_PPC} \
%{mcpu=ppc604: -D_ARCH_PPC}"
