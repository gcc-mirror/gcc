/* Target definitions for GCC for Intel 80386 running Interix
   Parts Copyright (C) 1991, 1999, 2000, 2002, 2003, 2004, 2007, 2008, 2009,
   2010, 2011, 2012 Free Software Foundation, Inc.

   Parts:
     by Douglas B. Rupp (drupp@cs.washington.edu).
     by Ron Guilmette (rfg@netcom.com).
     by Donn Terry (donn@softway.com).
     by Mumit Khan (khan@xraylith.wisc.edu).

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

/* Note: Interix doesn't support user-written DLLs (use conventional
   shared libs (.so) instead).  Thus a lot of the stuff that might apply
   about dllimport/dllexport and the like does not apply here. */

#include <stdio.h>

/* Names to predefine in the preprocessor for this target machine.  */

#define DBX_DEBUGGING_INFO 1
#define SDB_DEBUGGING_INFO 1
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

/* Our strategy for finding global constructors is a bit different, although
   not a lot.  */
#define DO_GLOBAL_CTORS_BODY						\
do {									\
  int i;								\
  unsigned long nptrs;							\
  func_ptr *p;								\
  asm(									\
       "     .section .ctor_head, \"rw\"\n"				\
       "1:\n"								\
       "     .text \n"							\
       ASM_LOAD_ADDR(1b,%0)						\
       : "=r" (p) : : "cc");						\
  for (nptrs = 0; p[nptrs] != 0; nptrs++);				\
  for (i = nptrs-1; i >= 0; i--)					\
    p[i] ();								\
} while (0)

#define DO_GLOBAL_DTORS_BODY						\
do {									\
  func_ptr *p;								\
  asm(									\
       "     .section .dtor_head, \"rw\"\n"				\
       "1:\n"								\
       "     .text \n"							\
       ASM_LOAD_ADDR(1b,%0)						\
       : "=r" (p) : : "cc");						\
  while (*p)								\
    {									\
      p++;								\
      (*(p-1)) ();							\
    }									\
} while (0)

/* We don't use the "usual" push-an-address solution. */
#undef TARGET_ASM_CONSTRUCTOR

#undef  SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES \
{ "ms-bitfields", MASK_MS_BITFIELD_LAYOUT, N_("Use native (MS) bitfield layout") }, \
{ "no-ms-bitfields", -MASK_MS_BITFIELD_LAYOUT, N_("Use gcc default bitfield layout") },

#undef LIB_SPEC
#define LIB_SPEC "\
 %{!shared:%{!dynamic:-lc -lpsxdll \
 }} \
 %{!G:%{!dynamic:-lc -lpsxdll \
 }} \
 %{dynamic:-lc -lpsxdll \
 } \
 %{v}"

#undef LINK_SPEC
#define LINK_SPEC "%{!shared:-stack 0x4000000,0x10000} \
		   %{g} \
		   %{dynamic:-Bdynamic} \
		   %{static:-Bstatic} \
		   %{shared:--shared -Bdynamic} \
		   %{G:--shared -Bdynamic} \
		   %{symbolic:--shared -Bsymbolic -Bdynamic} \
   		   %{rpath*:--rpath %*} \
		   "

#undef STARTFILE_SPEC
#define STARTFILE_SPEC  \
  "%{!shared:%{pg:gcrt0%O%s}%{!pg:%{p:mcrt0%O%s}%{!p:crt0%O%s}}} %{shared:crti%O%s}"

#define TARGET_DECLSPEC 1

/* cpp handles __STDC__ */
#define TARGET_OS_CPP_BUILTINS()					\
  do									\
    {									\
	builtin_define ("__INTERIX");					\
	builtin_define ("_M_IX86=300");					\
	builtin_define ("_X86_=1");					\
	builtin_define ("__stdcall=__attribute__((__stdcall__))");	\
	builtin_define ("__cdecl=__attribute__((__cdecl__))");		\
	builtin_define ("__declspec(x)=__attribute__((x))");		\
	builtin_assert ("system=unix");					\
	builtin_assert ("system=interix");				\
	if (preprocessing_asm_p ())					\
	  builtin_define_std ("LANGUAGE_ASSEMBLY");			\
	else								\
	  {								\
	     builtin_define_std ("LANGUAGE_C");				\
	     if (c_dialect_cxx ())					\
	       builtin_define_std ("LANGUAGE_C_PLUS_PLUS");		\
	     if (c_dialect_objc ())					\
	       builtin_define_std ("LANGUAGE_OBJECTIVE_C");		\
	  } 								\
    }									\
  while (0)

#undef CPP_SPEC
#define CPP_SPEC "%{posix:-D_POSIX_SOURCE}"

#define SIZE_TYPE "unsigned int"
#define PTRDIFF_TYPE "int"
#define WCHAR_TYPE "short unsigned int"
#define WCHAR_TYPE_SIZE 16

/* Turn off long double being 96 bits.  */
#undef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE 64
#undef LIBGCC2_LONG_DOUBLE_TYPE_SIZE
#define LIBGCC2_LONG_DOUBLE_TYPE_SIZE 64

/* The following are needed for us to be able to use winnt.c, but are not
   otherwise meaningful to Interix.  (The functions that use these are
   never called because we don't do DLLs.) */
#define TARGET_NOP_FUN_DLLIMPORT 1
#define drectve_section()  /* nothing */


#define EH_FRAME_IN_DATA_SECTION

#define READONLY_DATA_SECTION_ASM_OP	"\t.section\t.rdata,\"r\""

/* Define this macro if references to a symbol must be treated
   differently depending on something about the variale or
   function named by the symbol (such as what section it is in).  */

#undef TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO i386_pe_encode_section_info
#undef  TARGET_STRIP_NAME_ENCODING
#define TARGET_STRIP_NAME_ENCODING  i386_pe_strip_name_encoding_full

/* Emit code to check the stack when allocating more that 4000
   bytes in one go.  */

#define CHECK_STACK_LIMIT 4000

/* By default, target has a 80387, uses IEEE compatible arithmetic,
   and returns float values in the 387 and needs stack probes
   We also align doubles to 64-bits forMSVC default compatibility
   Ditto for bitfields. */
#undef TARGET_SUBTARGET_DEFAULT
#define TARGET_SUBTARGET_DEFAULT \
   (MASK_80387 | MASK_IEEE_FP | MASK_FLOAT_RETURNS | MASK_STACK_PROBE | \
    MASK_ALIGN_DOUBLE | MASK_MS_BITFIELD_LAYOUT)

/* The MS compilers take alignment as a number of bytes, so we do as well */
#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG) \
  if ((LOG)!=0) fprintf ((FILE), "\t.balign %d\n", 1<<(LOG))


/* Define this macro if in some cases global symbols from one translation
   unit may not be bound to undefined symbols in another translation unit
   without user intervention.  For instance, under Microsoft Windows
   symbols must be explicitly imported from shared libraries (DLLs).  */
/*
 * Old gcc(3.3) did not have 1 here
 */
#define MULTIPLE_SYMBOL_SPACES	1

extern void i386_pe_unique_section PARAMS ((tree, int));
#define TARGET_ASM_UNIQUE_SECTION i386_pe_unique_section

/* Switch into a generic section.  */
#define TARGET_ASM_NAMED_SECTION  default_pe_asm_named_section

/* Select attributes for named sections.  */
#define TARGET_SECTION_TYPE_FLAGS  i386_pe_section_type_flags

/* Write the extra assembler code needed to declare a function
   properly.  If we are generating SDB debugging information, this
   will happen automatically, so we only need to handle other cases.  */
#undef ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)			\
  do									\
    {									\
      if (write_symbols != SDB_DEBUG)					\
       i386_pe_declare_function_type (FILE, NAME, TREE_PUBLIC (DECL));	\
      ASM_OUTPUT_LABEL (FILE, NAME);       				\
    }									\
  while (0)

/* Add an external function to the list of functions to be declared at
   the end of the file.  */
#define ASM_OUTPUT_EXTERNAL(FILE, DECL, NAME)				\
  do									\
    {									\
      if (TREE_CODE (DECL) == FUNCTION_DECL)				\
        i386_pe_record_external_function (DECL, NAME);			\
    }									\
  while (0)

/* Declare the type properly for any external libcall.  */
#define ASM_OUTPUT_EXTERNAL_LIBCALL(FILE, FUN) \
  i386_pe_declare_function_type (FILE, XSTR (FUN, 0), 1)

/* This says out to put a global symbol in the BSS section.  */
#undef ASM_OUTPUT_ALIGNED_BSS
#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
  asm_output_aligned_bss ((FILE), (DECL), (NAME), (SIZE), (ALIGN))

/* Don't assume anything about the header files.  */
#define NO_IMPLICIT_EXTERN_C

/* External function declarations.  */
extern void i386_pe_record_external_function (tree, const char *);
extern void i386_pe_declare_function_type (FILE *, const char *, int);
extern void i386_pe_record_exported_symbol (const char *, int);
extern void i386_pe_asm_file_end (FILE *);

/* For Win32 ABI compatibility */
#undef DEFAULT_PCC_STRUCT_RETURN
#define DEFAULT_PCC_STRUCT_RETURN 0

/* A bitfield declared as `int' forces `int' alignment for the struct.  */
#undef PCC_BITFIELD_TYPE_MATTERS
#define PCC_BITFIELD_TYPE_MATTERS 1

/* Enable alias attribute support.  */
#ifndef SET_ASM_OP
#define SET_ASM_OP "\t.set\t"
#endif

/* Note that there appears to be two different ways to support const
   sections at the moment.  You can either #define the symbol
   READONLY_DATA_SECTION (giving it some code which switches to the
   readonly data section) or else you can #define the symbols
   EXTRA_SECTIONS, EXTRA_SECTION_FUNCTIONS, SELECT_SECTION, and
   SELECT_RTX_SECTION.  We do both here just to be on the safe side.  */

#define USE_CONST_SECTION 1

/* The linker will take care of this, and having them causes problems with
   ld -r (specifically -rU).  */
#define CTOR_LISTS_DEFINED_EXTERNALLY 1

/* Output a definition (implements alias) */
#define ASM_OUTPUT_DEF(FILE,LABEL1,LABEL2)				\
do									\
{									\
    fputs (SET_ASM_OP, (FILE));						\
    assemble_name (FILE, LABEL1);					\
    fputc (',', (FILE));						\
    assemble_name (FILE, LABEL2);					\
    fputc ('\n', (FILE));						\
    }									\
while (0)

#define HOST_PTR_AS_INT unsigned long

/* The following two flags are usually "off" for i386, because some non-gnu
   tools (for the i386) don't handle them.  However, we don't have that
   problem, so....  */

/* Forward references to tags are allowed.  */
#define SDB_ALLOW_FORWARD_REFERENCES
/* Unknown tags are also allowed.  */
#define SDB_ALLOW_UNKNOWN_REFERENCES
/* DWARF2 Unwinding doesn't work with exception handling yet.  */
#define DWARF2_UNWIND_INFO 0
/* MSVC returns structs of up to 8 bytes via registers. */

#undef SUBTARGET_RETURN_IN_MEMORY
#define SUBTARGET_RETURN_IN_MEMORY(TYPE, FNTYPE) \
  (TYPE_MODE (TYPE) == BLKmode || \
     (AGGREGATE_TYPE_P (TYPE) && int_size_in_bytes(TYPE) > 8 ))

#define ASM_LOAD_ADDR(loc, reg)   "     leal " #loc "," #reg "\n"

/* The integer half of this list needs to be constant.  However, there's
   a lot of disagreement about what the floating point adjustments should
   be.  We pick one that works with gdb.  (The underlying problem is
   what to do about the segment registers.  Since we have access to them
   from /proc, we'll allow them to be accessed in gdb, even tho the
   gcc compiler can't generate them.  (There's some evidence that
   MSVC does, but possibly only for certain special "canned" sequences.)  */

#undef DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(n) \
(TARGET_64BIT ? dbx64_register_map[n] \
 : (n) == 0 ? (int) 0 \
 : (n) == 1 ? (int) 2 \
 : (n) == 2 ? (int) 1 \
 : (n) == 3 ? (int) 3 \
 : (n) == 4 ? (int) 6 \
 : (n) == 5 ? (int) 7 \
 : (n) == 6 ? (int) 5 \
 : (n) == 7 ? (int) 4 \
 : ((n) >= FIRST_STACK_REG && (n) <= LAST_STACK_REG) ? (int) (n)+8 \
 : (int) (-1))

#define EH_FRAME_IN_DATA_SECTION

/* the following are OSF linker (not gld) specific... we don't want them */
#undef HAS_INIT_SECTION
#undef LD_INIT_SWITCH
#undef LD_FINI_SWITCH

/* The following are needed for us to be able to use winnt.c, but are not
   otherwise meaningful to Interix.  (The functions that use these are
   never called because we don't do DLLs.) */
#define TARGET_NOP_FUN_DLLIMPORT 1
#define I386_PE_STRIP_ENCODING(SYM_NAME) \
  ((SYM_NAME) + ((SYM_NAME)[0] == '@' \
                 ? ((SYM_NAME)[3] == '*' ? 4 : 3) : 0) \
             + ((SYM_NAME)[0] == '*' ? 1 : 0))

#define drectve_section()  /* nothing */

