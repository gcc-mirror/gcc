/* Definitions for MIPS running Linux-based GNU systems with ELF format.
   Copyright (C) 1998, 1999, 2000, 2001, 2002 Free Software Foundation, Inc.

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
   `varasm.c' when defining this macro.  */
#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN)	\
do {								\
  (*targetm.asm_out.globalize_label) (FILE, NAME);		\
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
#define ASM_DECLARE_OBJECT_NAME(FILE, NAME, DECL)			\
  do {									\
    HOST_WIDE_INT size;							\
    ASM_OUTPUT_TYPE_DIRECTIVE (FILE, NAME, "object");			\
    size_directive_output = 0;						\
    if (!flag_inhibit_size_directive && DECL_SIZE (DECL))		\
      {									\
	size_directive_output = 1;					\
	size = int_size_in_bytes (TREE_TYPE (DECL));			\
	ASM_OUTPUT_SIZE_DIRECTIVE (FILE, NAME, size);			\
      }									\
    mips_declare_object (FILE, NAME, "", ":\n", 0);			\
  } while (0)

#define TARGET_ASM_UNIQUE_SECTION  mips_unique_section

/* A list of other sections which the compiler might be "in" at any
   given time.  */
#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS in_sdata, in_sbss

#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS                                         \
  SECTION_FUNCTION_TEMPLATE(sdata_section, in_sdata, SDATA_SECTION_ASM_OP) \
  SECTION_FUNCTION_TEMPLATE(sbss_section, in_sbss, SBSS_SECTION_ASM_OP)

#define SECTION_FUNCTION_TEMPLATE(FN, ENUM, OP)			\
void FN ()							\
{								\
  if (in_section != ENUM)					\
    {								\
      fprintf (asm_out_file, "%s\n", OP);			\
      in_section = ENUM;					\
    }								\
}

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

/* If we don't set MASK_ABICALLS, we can't default to PIC.  */
#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_ABICALLS|MASK_GAS)

#define TARGET_OS_CPP_BUILTINS()				\
    do {							\
	builtin_define ("__gnu_linux__");			\
	builtin_define ("__ELF__");				\
	builtin_define ("__PIC__");				\
	builtin_define ("__pic__");				\
	builtin_define_std ("unix");				\
	builtin_define_std ("linux");				\
	builtin_assert ("system=linux");			\
	/* The GNU C++ standard library requires this.  */	\
	if (c_language == clk_cplusplus)			\
	  builtin_define ("_GNU_SOURCE");			\
								\
      if (mips_abi == ABI_N32)					\
      {								\
        builtin_define ("_ABIN32=2");				\
        builtin_define ("_MIPS_SIM=_ABIN32");			\
        builtin_define ("_MIPS_SZLONG=32");			\
        builtin_define ("_MIPS_SZPTR=32");			\
      }								\
     else if (mips_abi == ABI_64)				\
      {								\
        builtin_define ("_ABI64=3");				\
        builtin_define ("_MIPS_SIM=_ABI64");			\
        builtin_define ("_MIPS_SZLONG=64");			\
        builtin_define ("_MIPS_SZPTR=64");			\
      }								\
     else							\
      {								\
        builtin_define ("_MIPS_SIM=_MIPS_SIM_ABI32");		\
        builtin_define ("_MIPS_SZLONG=32");			\
        builtin_define ("_MIPS_SZPTR=32");			\
      }								\
     if (TARGET_FLOAT64)					\
        builtin_define ("_MIPS_FPSET=32");			\
     else							\
        builtin_define ("_MIPS_FPSET=16");			\
								\
     if (TARGET_INT64)						\
        builtin_define ("_MIPS_SZINT=64");			\
     else							\
        builtin_define ("_MIPS_SZINT=32");			\
} while (0)

#undef  SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC "\
%{fno-PIC:-U__PIC__ -U__pic__} %{fno-pic:-U__PIC__ -U__pic__} \
%{fPIC:-D__PIC__ -D__pic__} %{fpic:-D__PIC__ -D__pic__} \
%{pthread:-D_REENTRANT}"

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

#undef  SUBTARGET_ASM_DEBUGGING_SPEC
#define SUBTARGET_ASM_DEBUGGING_SPEC "-g0"

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

#undef ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(STREAM, NAME, DECL)			\
  do {									\
    if (!flag_inhibit_size_directive)					\
      {									\
	fputs ("\t.ent\t", STREAM);					\
	assemble_name (STREAM, NAME);					\
	putc ('\n', STREAM);						\
      }									\
    ASM_OUTPUT_TYPE_DIRECTIVE (STREAM, NAME, "function");		\
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

#define ASM_PREFERRED_EH_DATA_FORMAT(CODE, GLOBAL)       		\
  (flag_pic								\
    ? ((GLOBAL) ? DW_EH_PE_indirect : 0) | DW_EH_PE_pcrel | DW_EH_PE_sdata4\
   : DW_EH_PE_absptr)

/* The glibc _mcount stub will save $v0 for us.  Don't mess with saving
   it, since ASM_OUTPUT_REG_PUSH/ASM_OUTPUT_REG_POP do not work in the
   presence of $gp-relative calls.  */
#undef ASM_OUTPUT_REG_PUSH
#undef ASM_OUTPUT_REG_POP

/* The current Linux binutils uses MIPS_STABS_ELF and doesn't support
   COFF.  */
#undef SDB_DEBUGGING_INFO

#undef LIB_SPEC
#define LIB_SPEC "\
%{shared: -lc} \
%{!static:-rpath-link %R/lib:%R/usr/lib} \
%{!shared: %{pthread:-lpthread} \
  %{profile:-lc_p} %{!profile: -lc}}"
