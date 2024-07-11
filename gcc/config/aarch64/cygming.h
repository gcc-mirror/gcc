/* Operating system specific defines to be used when targeting GCC for
   hosting on Windows32, using a Unix style C library and tools.
   Copyright (C) 1995-2024 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_AARCH64_CYGMING_H
#define GCC_AARCH64_CYGMING_H

#define DWARF2_DEBUGGING_INFO 1

#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

#undef DWARF2_UNWIND_INFO
#define DWARF2_UNWIND_INFO 0

#define FASTCALL_PREFIX '@'

#define print_reg(rtx, code, file) (gcc_unreachable ())

#define SYMBOL_FLAG_DLLIMPORT		(SYMBOL_FLAG_MACH_DEP << 0)
#define SYMBOL_REF_DLLIMPORT_P(X) \
	((SYMBOL_REF_FLAGS (X) & SYMBOL_FLAG_DLLIMPORT) != 0)

#define SYMBOL_FLAG_DLLEXPORT		(SYMBOL_FLAG_MACH_DEP << 1)
#define SYMBOL_REF_DLLEXPORT_P(X) \
	((SYMBOL_REF_FLAGS (X) & SYMBOL_FLAG_DLLEXPORT) != 0)

#define SYMBOL_FLAG_STUBVAR	(SYMBOL_FLAG_MACH_DEP << 2)
#define SYMBOL_REF_STUBVAR_P(X) \
	((SYMBOL_REF_FLAGS (X) & SYMBOL_FLAG_STUBVAR) != 0)

/* Disable SEH and declare the required SEH-related macros that are
still needed for compilation.  */
#undef TARGET_SEH
#define TARGET_SEH 0

#define SSE_REGNO_P(N) (gcc_unreachable (), 0)
#define GENERAL_REGNO_P(N) (gcc_unreachable (), 0)
#define SEH_MAX_FRAME_SIZE (gcc_unreachable (), 0)

#undef TARGET_PECOFF
#define TARGET_PECOFF 1

#include <stdbool.h>
#ifdef __MINGW32__
#include <stdio.h>
#endif

#define TARGET_ASM_NAMED_SECTION  mingw_pe_asm_named_section

/* Select attributes for named sections.  */
#define TARGET_SECTION_TYPE_FLAGS  mingw_pe_section_type_flags

#define TARGET_ASM_UNIQUE_SECTION mingw_pe_unique_section
#define TARGET_ENCODE_SECTION_INFO  mingw_pe_encode_section_info

#define TARGET_VALID_DLLIMPORT_ATTRIBUTE_P mingw_pe_valid_dllimport_attribute_p

/* Output function declarations at the end of the file.  */
#undef TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END mingw_pe_file_end

/* Declare the type properly for any external libcall.  */
#define ASM_OUTPUT_EXTERNAL_LIBCALL(FILE, FUN) \
  mingw_pe_declare_type (FILE, XSTR (FUN, 0), 1, 1)

/* Use section relative relocations for debugging offsets.  Unlike
   other targets that fake this by putting the section VMA at 0, PE
   won't allow it.  */
#define ASM_OUTPUT_DWARF_OFFSET(FILE, SIZE, LABEL, OFFSET, SECTION) \
  do {								\
    switch (SIZE)						\
      {								\
      case 4:							\
	fputs ("\t.secrel32\t", FILE);				\
	assemble_name (FILE, LABEL);				\
	if ((OFFSET) != 0)					\
	  fprintf (FILE, "+" HOST_WIDE_INT_PRINT_DEC,		\
		   (HOST_WIDE_INT) (OFFSET));			\
	break;							\
      case 8:							\
	/* This is a hack.  There is no 64-bit section relative	\
	   relocation.  However, the COFF format also does not	\
	   support 64-bit file offsets; 64-bit applications are	\
	   limited to 32-bits of code+data in any one module.	\
	   Fake the 64-bit offset by zero-extending it.  */	\
	fputs ("\t.secrel32\t", FILE);				\
	assemble_name (FILE, LABEL);				\
	if ((OFFSET) != 0)					\
	  fprintf (FILE, "+" HOST_WIDE_INT_PRINT_DEC,		\
		   (HOST_WIDE_INT) (OFFSET));			\
	fputs ("\n\t.long\t0", FILE);				\
	break;							\
      default:							\
	gcc_unreachable ();					\
      }								\
  } while (0)

#define TARGET_OS_CPP_BUILTINS()					\
  do									\
    {									\
      builtin_define ("__MSVCRT__");					\
      builtin_define ("__MINGW32__");					\
      builtin_define ("_WIN32");					\
      builtin_define_std ("WIN32");					\
      builtin_define_std ("WINNT");					\
      builtin_define_with_int_value ("_INTEGRAL_MAX_BITS",		\
				TYPE_PRECISION (intmax_type_node));	\
      builtin_define ("__MINGW64__");					\
      builtin_define_std ("WIN64");					\
      builtin_define ("_WIN64");					\
      builtin_define ("__stdcall=__attribute__((__stdcall__))");	\
      builtin_define ("__fastcall=__attribute__((__fastcall__))");	\
      builtin_define ("__thiscall=__attribute__((__thiscall__))");	\
      builtin_define ("__cdecl=__attribute__((__cdecl__))");		\
    }									\
  while (0)

/* Windows64 continues to use a 32-bit long type.  */
#undef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE 32

#undef DEFAULT_SIGNED_CHAR
#define DEFAULT_SIGNED_CHAR 1

#undef SIZE_TYPE
#undef PTRDIFF_TYPE
#define SIZE_TYPE "long long unsigned int"
#define PTRDIFF_TYPE "long long int"

#undef WCHAR_TYPE_SIZE
#undef WCHAR_TYPE
#define WCHAR_TYPE_SIZE 16
#define WCHAR_TYPE "short unsigned int"

#define drectve_section() \
  (fprintf (asm_out_file, "\t.section\t.drectve\n"), \
   in_section = NULL)


/* Enable alias attribute support.  */
#ifndef SET_ASM_OP
#define SET_ASM_OP "\t.set\t"
#endif

/* GNU as supports weak symbols on PECOFF.  */
#define ASM_WEAKEN_LABEL(FILE, NAME)	\
  do					\
    {					\
      fputs ("\t.weak\t", (FILE));	\
      assemble_name ((FILE), (NAME));	\
      fputc ('\n', (FILE));		\
    }					\
  while (0)

/* Get tree.cc to declare a target-specific specialization of
   merge_decl_attributes.  */
#define TARGET_DLLIMPORT_DECL_ATTRIBUTES 1

#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, BODY, VALUE, REL)		\
  do {									\
    switch (GET_MODE (BODY))						\
      {									\
      case E_QImode:							\
	asm_fprintf (STREAM, "\t.byte\t(%LL%d - %LLrtx%d) / 4\n",	\
		     VALUE, REL);					\
	break;								\
      case E_HImode:							\
	asm_fprintf (STREAM, "\t.2byte\t(%LL%d - %LLrtx%d) / 4\n",	\
		     VALUE, REL);					\
	break;								\
      case E_SImode:							\
      case E_DImode: /* See comment in aarch64_output_casesi.  */	\
	asm_fprintf (STREAM, "\t.word\t(%LL%d - %LLrtx%d) / 4\n",	\
		     VALUE, REL);					\
	break;								\
      default:								\
	gcc_unreachable ();						\
      }									\
  } while (0)

#define READONLY_DATA_SECTION_ASM_OP "\t.section\t.rdata,\"dr\""

#undef  SUBTARGET_OVERRIDE_OPTIONS
#define SUBTARGET_OVERRIDE_OPTIONS			\
  do {							\
    flag_stack_check = STATIC_BUILTIN_STACK_CHECK;	\
  } while (0)

#define SUBTARGET_ATTRIBUTE_TABLE \
  { "selectany", 0, 0, true, false, false, false, \
    mingw_handle_selectany_attribute, NULL }

#undef SUB_TARGET_RECORD_STUB
#define SUB_TARGET_RECORD_STUB(NAME, DECL) mingw_pe_record_stub((NAME), \
  DECL_WEAK ((DECL)))

#define SUPPORTS_ONE_ONLY 1

#undef ASM_DECLARE_OBJECT_NAME
#define ASM_DECLARE_OBJECT_NAME(STREAM, NAME, DECL)			\
  do {									\
    mingw_pe_declare_type (STREAM, NAME, TREE_PUBLIC (DECL), 0);	\
    ASM_OUTPUT_LABEL ((STREAM), (NAME));				\
  } while (0)


#undef ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(STREAM, NAME, DECL)	\
  do {									\
    mingw_pe_declare_type (STREAM, NAME, TREE_PUBLIC (DECL), 1);	\
    aarch64_declare_function_name (STREAM, NAME, DECL);			\
  } while (0)


/* Define this to be nonzero if static stack checking is supported.  */
#define STACK_CHECK_STATIC_BUILTIN 1

#define HAVE_GAS_ALIGNED_COMM 1

#undef MAX_OFILE_ALIGNMENT
#define MAX_OFILE_ALIGNMENT (8192 * 8)

#undef GOT_ALIAS_SET
#define GOT_ALIAS_SET mingw_GOT_alias_set ()

#define PE_COFF_LEGITIMIZE_EXTERN_DECL(RTX) \
  (GET_CODE (RTX) == SYMBOL_REF && SYMBOL_REF_WEAK (RTX))

#define HAVE_64BIT_POINTERS 1

/* Kludge because of missing PE-COFF support for early LTO debug.  */
#undef  TARGET_ASM_LTO_START
#define TARGET_ASM_LTO_START mingw_pe_asm_lto_start
#undef  TARGET_ASM_LTO_END
#define TARGET_ASM_LTO_END mingw_pe_asm_lto_end

#endif
