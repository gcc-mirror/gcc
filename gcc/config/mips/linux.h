/* Definitions for MIPS running Linux-based GNU systems with ELF format.
   Copyright (C) 1998, 1999, 2000, 2001 Free Software Foundation, Inc.

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

#include "gofast.h"

/* US Software GOFAST library support.  */
#define INIT_SUBTARGET_OPTABS INIT_GOFAST_OPTABS

#include "mips/mips.h"

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

/* If defined, a C expression whose value is a string containing the
   assembler operation to identify the following data as
   uninitialized global data.  If not defined, and neither
   `ASM_OUTPUT_BSS' nor `ASM_OUTPUT_ALIGNED_BSS' are defined,
   uninitialized global data will be output in the data section if
   `-fno-common' is passed, otherwise `ASM_OUTPUT_COMMON' will be
   used.  */
#define BSS_SECTION_ASM_OP	"\t.section\t.bss"

#define SBSS_SECTION_ASM_OP	"\t.section .sbss"

/* Like `ASM_OUTPUT_BSS' except takes the required alignment as a
   separate, explicit argument.  If you define this macro, it is used
   in place of `ASM_OUTPUT_BSS', and gives you more flexibility in
   handling the required alignment of the variable.  The alignment is
   specified as the number of bits.

   Try to use function `asm_output_aligned_bss' defined in file
   `varasm.c' when defining this macro. */
#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN)	\
do {								\
  ASM_GLOBALIZE_LABEL (FILE, NAME);				\
  if (SIZE > 0 && SIZE <= mips_section_threshold)		\
    sbss_section ();						\
  else								\
    bss_section ();						\
  ASM_OUTPUT_ALIGN (FILE, floor_log2 (ALIGN / BITS_PER_UNIT));	\
  last_assemble_variable_decl = DECL;				\
  ASM_DECLARE_OBJECT_NAME (FILE, NAME, DECL);			\
  ASM_OUTPUT_SKIP (FILE, SIZE ? SIZE : 1);			\
} while (0)

/* These macros generate the special .type and .size directives which
   are used to set the corresponding fields of the linker symbol table
   entries in an ELF object file under SVR4.  These macros also output
   the starting labels for the relevant functions/objects.  */

/* Write the extra assembler code needed to declare an object properly.  */

#undef ASM_DECLARE_OBJECT_NAME
#define ASM_DECLARE_OBJECT_NAME(FILE, NAME, DECL)		\
  do {								\
    fprintf (FILE, "%s", TYPE_ASM_OP);				\
    assemble_name (FILE, NAME);					\
    putc (',', FILE);						\
    fprintf (FILE, TYPE_OPERAND_FMT, "object");			\
    putc ('\n', FILE);						\
    size_directive_output = 0;					\
    if (!flag_inhibit_size_directive && DECL_SIZE (DECL))	\
      {								\
	size_directive_output = 1;				\
	fprintf (FILE, "%s", SIZE_ASM_OP);			\
	assemble_name (FILE, NAME);				\
	fprintf (FILE, ",%d\n",					\
		 int_size_in_bytes (TREE_TYPE (DECL)));		\
      }								\
    mips_declare_object (FILE, NAME, "", ":\n", 0);		\
  } while (0)

#undef UNIQUE_SECTION
#define UNIQUE_SECTION(DECL,RELOC) \
  mips_unique_section ((DECL), (RELOC))

/* A list of other sections which the compiler might be "in" at any
   given time.  */
#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS in_sdata, in_sbss, in_rdata, in_ctors, in_dtors
 
#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS                                         \
  SECTION_FUNCTION_TEMPLATE(sdata_section, in_sdata, SDATA_SECTION_ASM_OP) \
  SECTION_FUNCTION_TEMPLATE(sbss_section, in_sbss, SBSS_SECTION_ASM_OP) \
  SECTION_FUNCTION_TEMPLATE(rdata_section, in_rdata, RDATA_SECTION_ASM_OP) \
  SECTION_FUNCTION_TEMPLATE(ctors_section, in_ctors, CTORS_SECTION_ASM_OP) \
  SECTION_FUNCTION_TEMPLATE(dtors_section, in_dtors, DTORS_SECTION_ASM_OP)

#define SECTION_FUNCTION_TEMPLATE(FN, ENUM, OP)			\
void FN ()							\
{								\
  if (in_section != ENUM)					\
    {								\
      fprintf (asm_out_file, "%s\n", OP);			\
      in_section = ENUM;					\
    }								\
}

/* A C statement (sans semicolon) to output an element in the table of
   global constructors.  */
#undef ASM_OUTPUT_CONSTRUCTOR
#define ASM_OUTPUT_CONSTRUCTOR(FILE,NAME)			  \
  do {								  \
    ctors_section ();						  \
    fprintf (FILE, "\t%s\t", TARGET_LONG64 ? ".dword" : ".word"); \
    assemble_name (FILE, NAME);					  \
    fprintf (FILE, "\n");					  \
  } while (0)


/* A C statement (sans semicolon) to output an element in the table of
   global destructors.  */
#undef ASM_OUTPUT_DESTRUCTOR
#define ASM_OUTPUT_DESTRUCTOR(FILE,NAME)			  \
  do {								  \
    dtors_section ();						  \
    fprintf (FILE, "\t%s\t", TARGET_LONG64 ? ".dword" : ".word"); \
    assemble_name (FILE, NAME);					  \
    fprintf (FILE, "\n");					  \
  } while (0)

#undef TARGET_VERSION
#if TARGET_ENDIAN_DEFAULT == 0
#define TARGET_VERSION fprintf (stderr, " (MIPSel GNU/Linux with ELF)");
#else
#define TARGET_VERSION fprintf (stderr, " (MIPS GNU/Linux with ELF)");
#endif

#undef MD_EXEC_PREFIX
#undef MD_STARTFILE_PREFIX

/* Required to keep collect2.c happy */
#undef OBJECT_FORMAT_COFF

/* If we don't set MASK_ABICALLS, we can't default to PIC. */
#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_ABICALLS|MASK_GAS)

/* Specify predefined symbols in preprocessor.  */
#undef CPP_PREDEFINES
#if TARGET_ENDIAN_DEFAULT == 0
#define CPP_PREDEFINES "-DMIPSEL -D_MIPSEL -Dunix -Dmips -D_mips \
-DR3000 -D_R3000 -Dlinux -Asystem=posix -Acpu=mips \
-Amachine=mips -D__ELF__ -D__PIC__ -D__pic__"
#else
#define CPP_PREDEFINES "-DMIPSEB -D_MIPSEB -Dunix -Dmips -D_mips \
-DR3000 -D_R3000 -Dlinux -Asystem=posix -Acpu=mips \
-Amachine=mips -D__ELF__ -D__PIC__ -D__pic__"
#endif

#undef SUBTARGET_CPP_SIZE_SPEC
#define SUBTARGET_CPP_SIZE_SPEC "\
%{mabi=32: -D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int} \
%{mabi=n32: -D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int} \
%{mabi=64: -D__SIZE_TYPE__=long\\ unsigned\\ int -D__PTRDIFF_TYPE__=long\\ int} \
%{!mabi*: -D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int}"

/* We must make -mips3 do what -mlong64 used to do.  */
/* ??? If no mipsX option given, but a mabi=X option is, then should set
   _MIPS_ISA based on the mabi=X option.  */
/* ??? If no mabi=X option give, but a mipsX option is, then should set
   _MIPS_SIM based on the mipsX option.  */
/* ??? Same for _MIPS_SZINT.  */
/* ??? Same for _MIPS_SZPTR.  */
/* ??? Same for __SIZE_TYPE and __PTRDIFF_TYPE.  */
#undef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC "\
%{mfp32: -D_MIPS_FPSET=16} \
%{mfp64: -D_MIPS_FPSET=32} \
%{!mfp*: -D_MIPS_FPSET=32} \
%{mips1: -D_MIPS_ISA=_MIPS_ISA_MIPS1} \
%{mips2: -D_MIPS_ISA=_MIPS_ISA_MIPS2} \
%{mips3: -D_MIPS_ISA=_MIPS_ISA_MIPS3} \
%{mips4: -D_MIPS_ISA=_MIPS_ISA_MIPS4} \
%{!mips*: -D_MIPS_ISA=_MIPS_ISA_MIPS1} \
%{mabi=32: -D_MIPS_SIM=_MIPS_SIM_ABI32}	\
%{mabi=n32: -D_ABIN32=2 -D_MIPS_SIM=_ABIN32} \
%{mabi=64: -D_ABI64=3 -D_MIPS_SIM=_ABI64} \
%{!mabi*: -D_MIPS_SIM=_MIPS_SIM_ABI32}	\
%{!mint64: -D_MIPS_SZINT=32}%{mint64: -D_MIPS_SZINT=64} \
%{mabi=32: -D_MIPS_SZLONG=32} \
%{mabi=n32: -D_MIPS_SZLONG=32} \
%{mabi=64: -D_MIPS_SZLONG=64} \
%{!mabi*: -D_MIPS_SZLONG=32} \
%{mabi=32: -D_MIPS_SZPTR=32} \
%{mabi=n32: -D_MIPS_SZPTR=32} \
%{mabi=64: -D_MIPS_SZPTR=64} \
%{!mabi*: -D_MIPS_SZPTR=32} \
%{!mips*: -U__mips -D__mips} \
%{mabi=32: -U__mips64} \
%{mabi=n32: -D__mips64} \
%{mabi=64: -U__mips64} \
%{!mabi*: -U__mips64} \
%{fno-PIC:-U__PIC__ -U__pic__} %{fno-pic:-U__PIC__ -U__pic__} \
%{fPIC:-D__PIC__ -D__pic__} %{fpic:-D__PIC__ -D__pic__} \
%{pthread:-D_REENTRANT}"

/* The GNU C++ standard library requires that these macros be defined.  */
#undef CPLUSPLUS_CPP_SPEC
#define CPLUSPLUS_CPP_SPEC "\
-D__LANGUAGE_C_PLUS_PLUS -D_LANGUAGE_C_PLUS_PLUS \
-D_GNU_SOURCE %(cpp) \
"

/* From iris5.h */
/* -G is incompatible with -KPIC which is the default, so only allow objects
   in the small data section if the user explicitly asks for it.  */
#undef MIPS_DEFAULT_GVALUE
#define MIPS_DEFAULT_GVALUE 0

/* Borrowed from sparc/linux.h */
#undef LINK_SPEC
#define LINK_SPEC \
 "%(endian_spec) \
  %{shared:-shared} \
  %{!shared: \
    %{!ibcs: \
      %{!static: \
        %{rdynamic:-export-dynamic} \
        %{!dynamic-linker:-dynamic-linker /lib/ld.so.1}} \
        %{static:-static}}}"


#undef SUBTARGET_ASM_SPEC
#define SUBTARGET_ASM_SPEC "\
%{mabi=64: -64} \
%{!fno-PIC:%{!fno-pic:-KPIC}} \
%{fno-PIC:-non_shared} %{fno-pic:-non_shared}"

/* The MIPS assembler has different syntax for .set. We set it to
   .dummy to trap any errors.  */
#undef SET_ASM_OP
#define SET_ASM_OP "\t.dummy\t"

#undef ASM_OUTPUT_DEF
#define ASM_OUTPUT_DEF(FILE,LABEL1,LABEL2)				\
 do {									\
	fputc ( '\t', FILE);						\
	assemble_name (FILE, LABEL1);					\
	fputs ( " = ", FILE);						\
	assemble_name (FILE, LABEL2);					\
	fputc ( '\n', FILE);						\
 } while (0)

#undef ASM_OUTPUT_DEFINE_LABEL_DIFFERENCE_SYMBOL
#define ASM_OUTPUT_DEFINE_LABEL_DIFFERENCE_SYMBOL(FILE, SY, HI, LO)    	\
  do {									\
	fputc ('\t', FILE);						\
	assemble_name (FILE, SY);					\
	fputc ('=', FILE);						\
	assemble_name (FILE, HI);					\
	fputc ('-', FILE);						\
	assemble_name (FILE, LO);					\
  } while (0)

#undef ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(STREAM, NAME, DECL)			\
  do {									\
    if (!flag_inhibit_size_directive)					\
      {									\
	fputs ("\t.ent\t", STREAM);					\
	assemble_name (STREAM, NAME);					\
	putc ('\n', STREAM);						\
      }									\
    fprintf (STREAM, "\t%s\t ", TYPE_ASM_OP);				\
    assemble_name (STREAM, NAME);					\
    putc (',', STREAM);							\
    fprintf (STREAM, TYPE_OPERAND_FMT, "function");			\
    putc ('\n', STREAM);						\
    assemble_name (STREAM, NAME);					\
    fputs (":\n", STREAM);						\
  } while (0)

#undef ASM_DECLARE_FUNCTION_SIZE
#define ASM_DECLARE_FUNCTION_SIZE(STREAM, NAME, DECL)			\
  do {									\
    if (!flag_inhibit_size_directive)					\
      {									\
	fputs ("\t.end\t", STREAM);					\
	assemble_name (STREAM, NAME);					\
	putc ('\n', STREAM);						\
      }									\
  } while (0)

/* Tell function_prologue in mips.c that we have already output the .ent/.end
   pseudo-ops.  */
#define FUNCTION_NAME_ALREADY_DECLARED
