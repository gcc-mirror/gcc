/* Target definitions for GCC for Intel 80386 running Solaris 2
   Copyright (C) 1993-2019 Free Software Foundation, Inc.
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

#define SUBTARGET_OPTIMIZATION_OPTIONS				\
  { OPT_LEVELS_1_PLUS, OPT_momit_leaf_frame_pointer, NULL, 1 }

/* 32-bit Solaris/x86 only guarantees 4-byte stack alignment as required by
   the i386 psABI, so realign it as necessary for SSE instructions.  */
#undef STACK_REALIGN_DEFAULT
#define STACK_REALIGN_DEFAULT (TARGET_64BIT ? 0 : 1)

/* Old versions of the Solaris assembler cannot handle the difference of
   labels in different sections, so force DW_EH_PE_datarel if so.  */
#ifndef HAVE_AS_IX86_DIFF_SECT_DELTA
#undef ASM_PREFERRED_EH_DATA_FORMAT
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE,GLOBAL)			\
  (flag_pic ? ((GLOBAL ? DW_EH_PE_indirect : 0)				\
	       | (TARGET_64BIT ? DW_EH_PE_pcrel | DW_EH_PE_sdata4	\
		  : DW_EH_PE_datarel))					\
   : DW_EH_PE_absptr)
#endif

/* The Solaris linker will not merge a read-only .eh_frame section
   with a read-write .eh_frame section.  None of the encodings used
   with non-PIC code require runtime relocations.  In 64-bit mode,
   since there is no backwards compatibility issue, we use a read-only
   section for .eh_frame.  In 32-bit mode, we use a writable .eh_frame
   section in order to be compatible with G++ for Solaris x86.  */
#undef EH_TABLES_CAN_BE_READ_ONLY
#define EH_TABLES_CAN_BE_READ_ONLY (TARGET_64BIT)

/* Follow Sun requirements for TLS code sequences and use Sun assembler TLS
   syntax.  */
#undef TARGET_SUN_TLS
#define TARGET_SUN_TLS 1

#undef CPP_SPEC
#define CPP_SPEC "%(cpp_subtarget)"

#undef CC1_SPEC
#define CC1_SPEC "%(cc1_cpu) " ASAN_CC1_SPEC

/* GNU as understands --32 and --64, but the native Solaris
   assembler requires -xarch=generic or -xarch=generic64 instead.  */
#ifdef USE_GAS
#define ASM_CPU32_DEFAULT_SPEC "--32"
#define ASM_CPU64_DEFAULT_SPEC "--64"
#else
#define ASM_CPU32_DEFAULT_SPEC "-xarch=generic"
#define ASM_CPU64_DEFAULT_SPEC "-xarch=generic64"
#endif

/* Since Studio 12.6, as needs -xbrace_comment=no so its AVX512 syntax is
   fully compatible with gas.  */
#ifdef HAVE_AS_XBRACE_COMMENT_OPTION
#define ASM_XBRACE_COMMENT_SPEC "-xbrace_comment=no"
#else
#define ASM_XBRACE_COMMENT_SPEC ""
#endif

#undef ASM_CPU_SPEC
#define ASM_CPU_SPEC "%(asm_cpu_default) " ASM_XBRACE_COMMENT_SPEC

/* Don't include ASM_PIC_SPEC.  While the Solaris 10+ assembler accepts -K PIC,
   it gives many warnings: 
	Absolute relocation is used for symbol "<symbol>"
   GNU as doesn't recognize -K at all.  */
#undef ASM_SPEC
#define ASM_SPEC ASM_SPEC_BASE

#define DEFAULT_ARCH32_P !TARGET_64BIT_DEFAULT

#define ARCH64_SUBDIR "amd64"

#ifdef USE_GLD
/* Since binutils 2.21, GNU ld supports new *_sol2 emulations to strictly
   follow the Solaris 2 ABI.  Prefer them if present.  */
#ifdef HAVE_LD_SOL2_EMULATION
#define ARCH32_EMULATION "elf_i386_sol2"
#define ARCH64_EMULATION "elf_x86_64_sol2"
#else
#define ARCH32_EMULATION "elf_i386"
#define ARCH64_EMULATION "elf_x86_64"
#endif
#endif

#define ENDFILE_ARCH_SPEC \
  "%{mpc32:crtprec32.o%s} \
   %{mpc64:crtprec64.o%s} \
   %{mpc80:crtprec80.o%s}"

#define SUBTARGET_CPU_EXTRA_SPECS \
  { "cpp_subtarget",	 CPP_SUBTARGET_SPEC },		\
  { "asm_cpu",		 ASM_CPU_SPEC },		\
  { "asm_cpu_default",	 ASM_CPU_DEFAULT_SPEC },	\

/* Register the Solaris-specific #pragma directives.  */
#define REGISTER_SUBTARGET_PRAGMAS() solaris_register_pragmas ()

#undef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX "."

/* The Solaris 10 FCS as doesn't accept "#" comments, while later versions
   do.  */
#undef ASM_COMMENT_START
#define ASM_COMMENT_START "/"

/* The 32-bit Solaris assembler does not support .quad.  Do not use it.  */
#ifndef HAVE_AS_IX86_QUAD
#undef ASM_QUAD
#endif

/* The native Solaris assembler can't calculate the difference between
   symbols in different sections, which causes problems for -fPIC jump
   tables in .rodata.  */
#ifndef HAVE_AS_IX86_DIFF_SECT_DELTA
#undef JUMP_TABLES_IN_TEXT_SECTION
#define JUMP_TABLES_IN_TEXT_SECTION 1

/* The native Solaris assembler cannot handle the SYMBOL-. syntax, but
   requires SYMBOL@rel/@rel64 instead.  */
#define ASM_OUTPUT_DWARF_PCREL(FILE, SIZE, LABEL)	\
  do {							\
    fputs (integer_asm_op (SIZE, FALSE), FILE);		\
    assemble_name (FILE, LABEL);			\
    fputs (SIZE == 8 ? "@rel64" : "@rel", FILE);	\
  } while (0)
#endif

/* The Solaris assembler wants a .local for non-exported aliases.  */
#define ASM_OUTPUT_DEF_FROM_DECLS(FILE, DECL, TARGET)	\
  do {							\
    tree id = DECL_ASSEMBLER_NAME (DECL);		\
    ultimate_transparent_alias_target (&id);		\
    const char *declname = IDENTIFIER_POINTER (id);	\
    ASM_OUTPUT_DEF ((FILE), declname,			\
		    IDENTIFIER_POINTER (TARGET));	\
    if (! TREE_PUBLIC (DECL))				\
      {							\
	fprintf ((FILE), "%s", LOCAL_ASM_OP);		\
	assemble_name ((FILE), declname);		\
	fprintf ((FILE), "\n");				\
      }							\
  } while (0)

#ifndef USE_GAS
/* The Sun assembler uses .tcomm for TLS common sections.  */
#define TLS_COMMON_ASM_OP ".tcomm"

/* Similar to the Sun assembler on SPARC, the native assembler requires
   TLS objects to be declared as @tls_obj (not @tls_object).  Unlike SPARC,
   gas doesn't understand this variant.  */
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
#endif /* !USE_GAS */

/* As in sparc/sol2.h, override the default from i386/x86-64.h to work
   around Sun as TLS bug.  */
#undef  ASM_OUTPUT_ALIGNED_DECL_COMMON
#define ASM_OUTPUT_ALIGNED_DECL_COMMON(FILE, DECL, NAME, SIZE, ALIGN)	\
  do									\
    {									\
      if (TARGET_SUN_TLS						\
	  && in_section							\
	  && ((in_section->common.flags & SECTION_TLS) == SECTION_TLS))	\
	switch_to_section (bss_section);				\
      x86_elf_aligned_decl_common (FILE, DECL, NAME, SIZE, ALIGN);	\
    }									\
  while  (0)

/* Output a simple call for .init/.fini.  */
#define ASM_OUTPUT_CALL(FILE, FN)				\
  do								\
    {								\
      fprintf (FILE, "\tcall\t");				\
      ix86_print_operand (FILE, XEXP (DECL_RTL (FN), 0), 'P');	\
      fprintf (FILE, "\n");					\
    }								\
  while (0)

#undef TARGET_ASM_NAMED_SECTION
#define TARGET_ASM_NAMED_SECTION i386_solaris_elf_named_section

/* Sun as requires "h" flag for large sections, GNU as can do without, but
   accepts "l".  */
#ifdef USE_GAS
#define MACH_DEP_SECTION_ASM_FLAG 'l'
#else
#define MACH_DEP_SECTION_ASM_FLAG 'h'
#endif

#ifndef USE_GAS
/* Emit COMDAT group signature symbols for Sun as.  */
#undef TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END solaris_file_end
#endif

/* Unlike GNU ld, Sun ld doesn't coalesce .ctors.N/.dtors.N sections, so
   inhibit their creation.  Also cf. sparc/sysv4.h.  */
#ifndef USE_GLD
#define CTORS_SECTION_ASM_OP	"\t.section\t.ctors, \"aw\""
#define DTORS_SECTION_ASM_OP	"\t.section\t.dtors, \"aw\""
#endif

#ifndef USE_GAS
#define LARGECOMM_SECTION_ASM_OP "\t.lbcomm\t"
#endif

/* -fsanitize=address is currently only supported for 32-bit.  */
#define ASAN_REJECT_SPEC \
  DEF_ARCH64_SPEC("%e:-fsanitize=address is not supported in this configuration")

#define USE_IX86_FRAME_POINTER 1
#define USE_X86_64_FRAME_POINTER 1

#undef NO_PROFILE_COUNTERS

#undef MCOUNT_NAME
#define MCOUNT_NAME "_mcount"

/* We do not need NT_VERSION notes.  */
#undef X86_FILE_START_VERSION_DIRECTIVE
#define X86_FILE_START_VERSION_DIRECTIVE false
