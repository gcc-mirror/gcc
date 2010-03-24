/* Target definitions for GCC for Intel 80386 running Solaris 2
   Copyright (C) 1993, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
   2004, 2007, 2008, 2009, 2010 Free Software Foundation, Inc.
   Contributed by Fred Fish (fnf@cygnus.com).

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

/* The Solaris 2.0 x86 linker botches alignment of code sections.
   It tries to align to a 16 byte boundary by padding with 0x00000090
   ints, rather than 0x90 bytes (nop).  This generates trash in the
   ".init" section since the contribution from crtbegin.o is only 7
   bytes.  The linker pads it to 16 bytes with a single 0x90 byte, and
   two 0x00000090 ints, which generates a segmentation violation when
   executed.  This macro forces the assembler to do the padding, since
   it knows what it is doing.  */
#define FORCE_CODE_SECTION_ALIGN  asm(ALIGN_ASM_OP "16");

/* Old versions of the Solaris assembler can not handle the difference of
   labels in different sections, so force DW_EH_PE_datarel.  */
#undef ASM_PREFERRED_EH_DATA_FORMAT
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE,GLOBAL)			\
  (flag_pic ? ((GLOBAL ? DW_EH_PE_indirect : 0)				\
	       | (TARGET_64BIT ? DW_EH_PE_pcrel | DW_EH_PE_sdata4	\
		  : DW_EH_PE_datarel))					\
   : DW_EH_PE_absptr)

/* The Solaris linker will not merge a read-only .eh_frame section
   with a read-write .eh_frame section.  None of the encodings used
   with non-PIC code require runtime relocations.  In 64-bit mode,
   since there is no backwards compatibility issue, we use a read-only
   section for .eh_frame.  In 32-bit mode, we use a writable .eh_frame
   section in order to be compatible with G++ for Solaris x86.  */
#undef EH_TABLES_CAN_BE_READ_ONLY
#define EH_TABLES_CAN_BE_READ_ONLY (TARGET_64BIT)

/* Solaris 2/Intel as chokes on #line directives.  */
#undef CPP_SPEC
#define CPP_SPEC "%{,assembler-with-cpp:-P} %(cpp_subtarget)"

/* FIXME: Removed -K PIC from generic Solaris 2 ASM_SPEC: the native assembler
   gives many warnings: R_386_32 relocation is used for symbol ".text".  */
#undef ASM_SPEC
#define ASM_SPEC "\
%{v:-V} %{Qy:} %{!Qn:-Qy} %{n} %{T} %{Ym,*} %{Wa,*:%*} -s \
%(asm_cpu) \
"

#define ASM_CPU_SPEC ""
 
#undef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS \
  { "cpp_subtarget",	CPP_SUBTARGET_SPEC },	\
  { "asm_cpu",		ASM_CPU_SPEC },		\
  { "startfile_arch",	STARTFILE_ARCH_SPEC },	\
  { "link_arch",	LINK_ARCH_SPEC }

#undef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX "."

/* The 32-bit Solaris assembler does not support .quad.  Do not use it.  */
#ifndef TARGET_BI_ARCH
#undef ASM_QUAD
#endif

/* The Solaris assembler wants a .local for non-exported aliases.  */
#define ASM_OUTPUT_DEF_FROM_DECLS(FILE, DECL, TARGET)	\
  do {							\
    const char *declname =				\
      IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (DECL));	\
    ASM_OUTPUT_DEF ((FILE), declname,			\
		    IDENTIFIER_POINTER (TARGET));	\
    if (! TREE_PUBLIC (DECL))				\
      {							\
	fprintf ((FILE), "%s", LOCAL_ASM_OP);		\
	assemble_name ((FILE), declname);		\
	fprintf ((FILE), "\n");				\
      }							\
  } while (0)

/* Follow Sun requirements for TLS code sequences and use Sun assembler TLS
   syntax.  */
#undef TARGET_SUN_TLS
#define TARGET_SUN_TLS 1

/* Follow Sun requirements for TLS code sequences and use Sun assembler TLS
   syntax.  */
#undef TARGET_SUN_TLS
#define TARGET_SUN_TLS 1

/* The Sun assembler uses .tcomm for TLS common sections.  */
#define TLS_COMMON_ASM_OP ".tcomm"

/* Similar to the Sun assembler on SPARC, the native assembler requires
   TLS objects to be declared as @tls_obj (not @tls_object).  Unlike SPARC,
   gas doesn't understand this variant.  */
#ifndef USE_GAS
#undef  ASM_DECLARE_OBJECT_NAME
#define ASM_DECLARE_OBJECT_NAME(FILE, NAME, DECL)		\
  do								\
    {								\
      HOST_WIDE_INT size;					\
								\
      if (targetm.have_tls && DECL_THREAD_LOCAL_P (DECL))	\
	ASM_OUTPUT_TYPE_DIRECTIVE (FILE, NAME, "tls_obj");	\
      else							\
	ASM_OUTPUT_TYPE_DIRECTIVE (FILE, NAME, "object");	\
								\
      size_directive_output = 0;				\
      if (!flag_inhibit_size_directive				\
	  && (DECL) && DECL_SIZE (DECL))			\
	{							\
	  size_directive_output = 1;				\
	  size = int_size_in_bytes (TREE_TYPE (DECL));		\
	  ASM_OUTPUT_SIZE_DIRECTIVE (FILE, NAME, size);		\
	}							\
								\
      ASM_OUTPUT_LABEL (FILE, NAME);				\
    }								\
  while (0)
#endif

/* The Solaris assembler cannot grok .stabd directives.  */
#undef NO_DBX_BNSYM_ENSYM
#define NO_DBX_BNSYM_ENSYM 1

/* Solaris-specific #pragmas are implemented on top of attributes.  Hook in
   the bits from config/sol2.c.  */
#define SUBTARGET_INSERT_ATTRIBUTES solaris_insert_attributes
#define SUBTARGET_ATTRIBUTE_TABLE SOLARIS_ATTRIBUTE_TABLE

/* Register the Solaris-specific #pragma directives.  */
#define REGISTER_SUBTARGET_PRAGMAS() solaris_register_pragmas ()

/* Output a simple call for .init/.fini.  */
#define ASM_OUTPUT_CALL(FILE, FN)				\
  do								\
    {								\
      fprintf (FILE, "\tcall\t");				\
      print_operand (FILE, XEXP (DECL_RTL (FN), 0), 'P');	\
      fprintf (FILE, "\n");					\
    }								\
  while (0)

/* We do not need NT_VERSION notes.  */
#undef X86_FILE_START_VERSION_DIRECTIVE
#define X86_FILE_START_VERSION_DIRECTIVE false

/* Only recent versions of Solaris 11 ld properly support hidden .gnu.linkonce
   sections, so don't use them.  */
#ifndef TARGET_GNU_LD
#define USE_HIDDEN_LINKONCE 0
#endif

#define MD_UNWIND_SUPPORT "config/i386/sol2-unwind.h"
