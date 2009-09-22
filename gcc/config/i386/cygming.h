/* Operating system specific defines to be used when targeting GCC for
   hosting on Windows32, using a Unix style C library and tools.
   Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
   2004, 2005, 2007, 2008, 2009
   Free Software Foundation, Inc.

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

#define DBX_DEBUGGING_INFO 1
#define SDB_DEBUGGING_INFO 1
#if TARGET_64BIT_DEFAULT || defined (HAVE_GAS_PE_SECREL32_RELOC)
#define DWARF2_DEBUGGING_INFO 1
#endif

#undef PREFERRED_DEBUGGING_TYPE
#if (DWARF2_DEBUGGING_INFO)
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG
#else
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG
#endif

#undef TARGET_64BIT_MS_ABI
#define TARGET_64BIT_MS_ABI (!cfun ? ix86_abi == MS_ABI : TARGET_64BIT && cfun->machine->call_abi == MS_ABI)

#undef DEFAULT_ABI
#define DEFAULT_ABI (TARGET_64BIT ? MS_ABI : SYSV_ABI)

#undef DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(n)				\
  (TARGET_64BIT ? dbx64_register_map[n]			\
   : (write_symbols == DWARF2_DEBUG			\
      ? svr4_dbx_register_map[n] : dbx_register_map[n]))

/* Map gcc register number to DWARF 2 CFA column number. For 32 bit
   target, always use the svr4_dbx_register_map for DWARF .eh_frame
   even if we don't use DWARF .debug_frame. */
#undef DWARF_FRAME_REGNUM
#define DWARF_FRAME_REGNUM(n) TARGET_64BIT \
	? dbx64_register_map[(n)] : svr4_dbx_register_map[(n)] 

#ifdef HAVE_GAS_PE_SECREL32_RELOC
/* Use section relative relocations for debugging offsets.  Unlike
   other targets that fake this by putting the section VMA at 0, PE
   won't allow it.  */
#define ASM_OUTPUT_DWARF_OFFSET(FILE, SIZE, LABEL, SECTION)	\
  do {								\
    if (SIZE != 4 && (!TARGET_64BIT || SIZE != 8))		\
      abort ();							\
								\
    fputs ("\t.secrel32\t", FILE);				\
    assemble_name (FILE, LABEL);				\
  } while (0)
#endif

#define TARGET_EXECUTABLE_SUFFIX ".exe"

#include <stdio.h>

#define TARGET_OS_CPP_BUILTINS()					\
  do									\
    {									\
	if (!TARGET_64BIT)						\
	  builtin_define ("_X86_=1");					\
	builtin_assert ("system=winnt");				\
	builtin_define ("__stdcall=__attribute__((__stdcall__))");	\
	builtin_define ("__fastcall=__attribute__((__fastcall__))");	\
	builtin_define ("__cdecl=__attribute__((__cdecl__))");		\
	if (!flag_iso)							\
	  {								\
	    builtin_define ("_stdcall=__attribute__((__stdcall__))");	\
	    builtin_define ("_fastcall=__attribute__((__fastcall__))");	\
	    builtin_define ("_cdecl=__attribute__((__cdecl__))");	\
	  }								\
	/* Even though linkonce works with static libs, this is needed 	\
	    to compare typeinfo symbols across dll boundaries.  */	\
	builtin_define ("__GXX_MERGED_TYPEINFO_NAMES=0");		\
	builtin_define ("__GXX_TYPEINFO_EQUALITY_INLINE=0");		\
	EXTRA_OS_CPP_BUILTINS ();					\
  }									\
  while (0)

/* Get tree.c to declare a target-specific specialization of
   merge_decl_attributes.  */
#define TARGET_DLLIMPORT_DECL_ATTRIBUTES 1

/* This macro defines names of additional specifications to put in the specs
   that can be used in various specifications like CC1_SPEC.  Its definition
   is an initializer with a subgrouping for each command option.

   Each subgrouping contains a string constant, that defines the
   specification name, and a string constant that used by the GCC driver
   program.

   Do not define this macro if it does not need to do anything.  */

#undef  SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS						\
  { "mingw_include_path", DEFAULT_TARGET_MACHINE }

#undef MATH_LIBRARY
#define MATH_LIBRARY ""

#define SIZE_TYPE (TARGET_64BIT ? "long long unsigned int" : "unsigned int")
#define PTRDIFF_TYPE (TARGET_64BIT ? "long long int" : "int")

#define WCHAR_TYPE_SIZE 16
#define WCHAR_TYPE "short unsigned int"

/* Windows64 continues to use a 32-bit long type.  */
#undef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE 32

/* Enable parsing of #pragma pack(push,<n>) and #pragma pack(pop).  */
#define HANDLE_PRAGMA_PACK_PUSH_POP 1
/* Enable push_macro & pop_macro */
#define HANDLE_PRAGMA_PUSH_POP_MACRO 1

union tree_node;
#define TREE union tree_node *

#define drectve_section() \
  (fprintf (asm_out_file, "\t.section .drectve\n"), \
   in_section = NULL)

/* Older versions of gas don't handle 'r' as data.
   Explicitly set data flag with 'd'.  */  
#define READONLY_DATA_SECTION_ASM_OP "\t.section .rdata,\"dr\""

/* Don't allow flag_pic to propagate since gas may produce invalid code
   otherwise.  */

#undef  SUBTARGET_OVERRIDE_OPTIONS
#define SUBTARGET_OVERRIDE_OPTIONS					\
do {									\
  if (flag_pic)								\
    {									\
      warning (0, "-f%s ignored for target (all code is position independent)",\
	       (flag_pic > 1) ? "PIC" : "pic");				\
      flag_pic = 0;							\
    }									\
} while (0)								\

/* Define this macro if references to a symbol must be treated
   differently depending on something about the variable or
   function named by the symbol (such as what section it is in).

   On i386 running Windows NT, modify the assembler name with a suffix
   consisting of an atsign (@) followed by string of digits that represents
   the number of bytes of arguments passed to the function, if it has the
   attribute STDCALL.

   In addition, we must mark dll symbols specially. Definitions of
   dllexport'd objects install some info in the .drectve section.
   References to dllimport'd objects are fetched indirectly via
   _imp__.  If both are declared, dllexport overrides.  This is also
   needed to implement one-only vtables: they go into their own
   section and we need to set DECL_SECTION_NAME so we do that here.
   Note that we can be called twice on the same decl.  */

#define SUBTARGET_ENCODE_SECTION_INFO  i386_pe_encode_section_info

/* Output a common block.  */
#undef ASM_OUTPUT_ALIGNED_DECL_COMMON
#define ASM_OUTPUT_ALIGNED_DECL_COMMON \
  i386_pe_asm_output_aligned_decl_common

/* Output the label for an initialized variable.  */
#undef ASM_DECLARE_OBJECT_NAME
#define ASM_DECLARE_OBJECT_NAME(STREAM, NAME, DECL)	\
do {							\
  i386_pe_maybe_record_exported_symbol (DECL, NAME, 1);	\
  ASM_OUTPUT_LABEL ((STREAM), (NAME));			\
} while (0)

/* Output a reference to a label. Fastcall function symbols
   keep their '@' prefix, while other symbols are prefixed
   with user_label_prefix.  */
#undef ASM_OUTPUT_LABELREF
#define  ASM_OUTPUT_LABELREF(STREAM, NAME)	\
do {						\
  if ((NAME)[0] != FASTCALL_PREFIX)		\
    fputs (user_label_prefix, (STREAM));	\
  fputs ((NAME), (STREAM));			\
} while (0)


/* Emit code to check the stack when allocating more than 4000
   bytes in one go.  */
#define CHECK_STACK_LIMIT 4000

#undef STACK_BOUNDARY
#define STACK_BOUNDARY	(ix86_abi == MS_ABI ? 128 : BITS_PER_WORD)

/* By default, target has a 80387, uses IEEE compatible arithmetic,
   returns float values in the 387 and needs stack probes.
   We also align doubles to 64-bits for MSVC default compatibility.  */

#undef TARGET_SUBTARGET_DEFAULT
#define TARGET_SUBTARGET_DEFAULT \
	(MASK_80387 | MASK_IEEE_FP | MASK_FLOAT_RETURNS \
	 | MASK_STACK_PROBE | MASK_ALIGN_DOUBLE)

#undef TARGET_SUBTARGET64_DEFAULT
#define TARGET_SUBTARGET64_DEFAULT \
	MASK_128BIT_LONG_DOUBLE

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
    if ((LOG)!=0) fprintf ((FILE), "\t.align %d\n", 1<<(LOG))

/* Windows uses explicit import from shared libraries.  */
#define MULTIPLE_SYMBOL_SPACES 1

#define TARGET_ASM_UNIQUE_SECTION i386_pe_unique_section
#define TARGET_ASM_FUNCTION_RODATA_SECTION default_no_function_rodata_section

#define SUPPORTS_ONE_ONLY 1

/* Switch into a generic section.  */
#define TARGET_ASM_NAMED_SECTION  i386_pe_asm_named_section

/* Select attributes for named sections.  */
#define TARGET_SECTION_TYPE_FLAGS  i386_pe_section_type_flags

/* Write the extra assembler code needed to declare a function
   properly.  If we are generating SDB debugging information, this
   will happen automatically, so we only need to handle other cases.  */
#undef ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)			\
  do									\
    {									\
      i386_pe_maybe_record_exported_symbol (DECL, NAME, 0);		\
      if (write_symbols != SDB_DEBUG)					\
	i386_pe_declare_function_type (FILE, NAME, TREE_PUBLIC (DECL));	\
      ASM_OUTPUT_LABEL (FILE, NAME);					\
    }									\
  while (0)

/* Add an external function to the list of functions to be declared at
   the end of the file.  */
#define ASM_OUTPUT_EXTERNAL(FILE, DECL, NAME)				\
  do									\
    {									\
      if (TREE_CODE (DECL) == FUNCTION_DECL)				\
	i386_pe_record_external_function ((DECL), (NAME));		\
    }									\
  while (0)

/* Declare the type properly for any external libcall.  */
#define ASM_OUTPUT_EXTERNAL_LIBCALL(FILE, FUN) \
  i386_pe_declare_function_type (FILE, XSTR (FUN, 0), 1)

/* This says out to put a global symbol in the BSS section.  */
#undef ASM_OUTPUT_ALIGNED_BSS
#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
  asm_output_aligned_bss ((FILE), (DECL), (NAME), (SIZE), (ALIGN))

/* Output function declarations at the end of the file.  */
#undef TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END i386_pe_file_end

#undef ASM_COMMENT_START
#define ASM_COMMENT_START " #"

#ifndef DWARF2_UNWIND_INFO
/* If configured with --disable-sjlj-exceptions, use DWARF2, else
   default to SJLJ.  */
#if  (defined (CONFIG_SJLJ_EXCEPTIONS) && !CONFIG_SJLJ_EXCEPTIONS)
/* The logic of this #if must be kept synchronised with the logic
   for selecting the tmake_eh_file fragment in config.gcc.  */
#define DWARF2_UNWIND_INFO 1
/* If multilib is selected break build as sjlj is required.  */
#if defined (TARGET_BI_ARCH)
#error For 64-bit windows and 32-bit based multilib version of gcc just SJLJ exceptions are supported.
#endif
#else
#define DWARF2_UNWIND_INFO 0
#endif
#endif

/* Don't assume anything about the header files.  */
#define NO_IMPLICIT_EXTERN_C

#undef PROFILE_HOOK
#define PROFILE_HOOK(LABEL)						\
  if (MAIN_NAME_P (DECL_NAME (current_function_decl)))			\
    {									\
      emit_call_insn (gen_rtx_CALL (VOIDmode,				\
	gen_rtx_MEM (FUNCTION_MODE,					\
		     gen_rtx_SYMBOL_REF (Pmode, "_monstartup")),	\
	const0_rtx));							\
    }

/* Java Native Interface (JNI) methods on Win32 are invoked using the
   stdcall calling convention.  */
#undef MODIFY_JNI_METHOD_CALL
#define MODIFY_JNI_METHOD_CALL(MDECL)					      \
  build_type_attribute_variant ((MDECL),				      \
			       build_tree_list (get_identifier ("stdcall"),   \
						NULL))

/* For Win32 ABI compatibility */
#undef DEFAULT_PCC_STRUCT_RETURN
#define DEFAULT_PCC_STRUCT_RETURN 0

/* MSVC returns aggregate types of up to 8 bytes via registers.
   See i386.c:ix86_return_in_memory.  */
#undef MS_AGGREGATE_RETURN
#define MS_AGGREGATE_RETURN 1

/* Biggest alignment supported by the object file format of this
   machine.  Use this macro to limit the alignment which can be
   specified using the `__attribute__ ((aligned (N)))' construct.  If
   not defined, the default value is `BIGGEST_ALIGNMENT'.  */
/* IMAGE_SCN_ALIGN_8192BYTES is the largest section alignment flag
   specified in the PECOFF60 spec.  Native MS compiler also limits
   user-specified alignment to 8192 bytes.  */
#undef MAX_OFILE_ALIGNMENT
#define MAX_OFILE_ALIGNMENT (8192 * 8)

/* BIGGEST_FIELD_ALIGNMENT macro is used directly by libobjc, There, we
   align internal doubles in structures on dword boundaries. Otherwise,
   support vector modes using ADJUST_FIELD_ALIGN, defined in i386.h.  */
#ifdef IN_TARGET_LIBS
#undef	BIGGEST_FIELD_ALIGNMENT
#define BIGGEST_FIELD_ALIGNMENT 64
#endif

/* A bit-field declared as `int' forces `int' alignment for the struct.  */
#undef PCC_BITFIELD_TYPE_MATTERS
#define PCC_BITFIELD_TYPE_MATTERS 1
#define GROUP_BITFIELDS_BY_ALIGN TYPE_NATIVE(rec)

/* Enable alias attribute support.  */
#ifndef SET_ASM_OP
#define SET_ASM_OP "\t.set\t"
#endif

/* This implements the `alias' attribute, keeping any stdcall or
   fastcall decoration.  */
#undef	ASM_OUTPUT_DEF_FROM_DECLS
#define	ASM_OUTPUT_DEF_FROM_DECLS(STREAM, DECL, TARGET)			\
  do									\
    {									\
      const char *alias							\
	= IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (DECL));		\
      if (TREE_CODE (DECL) == FUNCTION_DECL)				\
	i386_pe_declare_function_type (STREAM, alias,			\
				       TREE_PUBLIC (DECL));		\
      ASM_OUTPUT_DEF (STREAM, alias, IDENTIFIER_POINTER (TARGET));	\
    } while (0)

/* GNU as supports weak symbols on PECOFF. */
#ifdef HAVE_GAS_WEAK
#define ASM_WEAKEN_LABEL(FILE, NAME)  \
  do                                  \
    {                                 \
      fputs ("\t.weak\t", (FILE));    \
      assemble_name ((FILE), (NAME)); \
      fputc ('\n', (FILE));           \
    }                                 \
  while (0)
#endif /* HAVE_GAS_WEAK */

/* FIXME: SUPPORTS_WEAK && TARGET_HAVE_NAMED_SECTIONS is true,
   but for .jcr section to work we also need crtbegin and crtend
   objects.  */
#define TARGET_USE_JCR_SECTION 1

/* Decide whether it is safe to use a local alias for a virtual function
   when constructing thunks.  */
#undef TARGET_USE_LOCAL_THUNK_ALIAS_P
#define TARGET_USE_LOCAL_THUNK_ALIAS_P(DECL) (!DECL_ONE_ONLY (DECL))

#define SUBTARGET_ATTRIBUTE_TABLE \
  { "selectany", 0, 0, true, false, false, ix86_handle_selectany_attribute }
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler } */

/*  mcount() does not need a counter variable.  */
#undef NO_PROFILE_COUNTERS
#define NO_PROFILE_COUNTERS 1

#define TARGET_VALID_DLLIMPORT_ATTRIBUTE_P i386_pe_valid_dllimport_attribute_p
#define TARGET_CXX_ADJUST_CLASS_AT_DEFINITION i386_pe_adjust_class_at_definition
#define TARGET_MANGLE_DECL_ASSEMBLER_NAME i386_pe_mangle_decl_assembler_name

#undef TREE

#ifndef BUFSIZ
# undef FILE
#endif
