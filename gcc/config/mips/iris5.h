/* Definitions of target machine for GNU compiler.  IRIX version 5.
   Copyright (C) 1993, 1995, 1996, 1998, 2000,
   2001, 2002, 2003, 2004 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* We are compiling for IRIX now.  */
#undef TARGET_IRIX
#define TARGET_IRIX 1

/* Allow some special handling for IRIX 5.  */
#undef TARGET_IRIX5
#define TARGET_IRIX5 1

#define ABICALLS_ASM_OP "\t.option pic2"

/* Dummy definition which allows EXTRA_SECTION_FUNCTIONS to be the same
   for IRIX 5 and 6.  */
#define BSS_SECTION_ASM_OP "\t.data"

/* ??? This is correct, but not very useful, because there is no file that
   uses this macro.  */
/* ??? The best way to handle global constructors under ELF is to use .init
   and .fini sections.  Unfortunately, there is apparently no way to get
   the IRIX 5.x (x <= 2) assembler to create these sections.  So we instead
   use collect.  The linker can create these sections via -init and -fini
   options, but using this would require modifying how crtstuff works, and
   I will leave that for another time (or someone else).  */
#define OBJECT_FORMAT_ELF
#define HAS_INIT_SECTION
#define LD_INIT_SWITCH "-init"
#define LD_FINI_SWITCH "-fini"

/* The linker needs a space after "-o".  */
#define SWITCHES_NEED_SPACES "o"

/* Specify wchar_t types.  */
#undef	WCHAR_TYPE
#undef	WCHAR_TYPE_SIZE
#undef	MAX_WCHAR_TYPE_SIZE

#define WCHAR_TYPE     "int"
#define WCHAR_TYPE_SIZE        INT_TYPE_SIZE
#define MAX_WCHAR_TYPE_SIZE    64

/* Plain char is unsigned in the SGI compiler.  */
#undef DEFAULT_SIGNED_CHAR
#define DEFAULT_SIGNED_CHAR 0

#define WORD_SWITCH_TAKES_ARG(STR)			\
 (DEFAULT_WORD_SWITCH_TAKES_ARG (STR)			\
  || !strcmp (STR, "rpath"))

/* We must pass -D_LONGLONG always, even when -ansi is used, because IRIX 5
   system header files require it.  This is OK, because gcc never warns
   when long long is used in system header files.  Alternatively, we can
   add support for the SGI builtin type __long_long.  */

#define TARGET_OS_CPP_BUILTINS()			\
    do {						\
	builtin_define_std ("host_mips");		\
	builtin_define_std ("sgi");			\
	builtin_define_std ("unix");			\
	builtin_define_std ("SYSTYPE_SVR4");		\
	builtin_define ("_LONGLONG");			\
	builtin_define ("_MODERN_C");			\
	builtin_define ("_SVR4_SOURCE");		\
	builtin_define ("__DSO__");			\
	builtin_define ("_ABIO32=1");			\
	builtin_define ("_MIPS_SIM=_ABIO32");		\
	builtin_define ("_MIPS_SZPTR=32");		\
	builtin_assert ("system=unix");			\
	builtin_assert ("system=svr4");			\
	builtin_assert ("machine=sgi");			\
							\
     if (!TARGET_FLOAT64)                               \
        builtin_define ("_MIPS_FPSET=16");              \
     else                                               \
        builtin_define ("_MIPS_FPSET=32");              \
							\
     if (!TARGET_INT64)                                 \
        builtin_define ("_MIPS_SZINT=32");              \
     else                                               \
        builtin_define ("_MIPS_SZINT=64");              \
							\
     if (!TARGET_LONG64)				\
	builtin_define ("_MIPS_SZLONG=32");		\
     else						\
	builtin_define ("_MIPS_SZLONG=64");		\
							\
     if (!flag_iso)					\
       {						\
	 builtin_define ("__EXTENSIONS__");		\
	 builtin_define ("_SGI_SOURCE");		\
       }						\
} while (0);

#undef SUBTARGET_CC1_SPEC
#define SUBTARGET_CC1_SPEC "%{static: -mno-abicalls}"

/* Override mips.h default: the IRIX 5 assembler warns about -O3:

   as1: Warning: <file>.s, line 1: Binasm file dictates -pic: 2
   uld:
   No ucode object file linked -- please use -O2 or lower.
   
   So avoid passing it in the first place.  */
#undef SUBTARGET_ASM_OPTIMIZING_SPEC
#define SUBTARGET_ASM_OPTIMIZING_SPEC "\
%{noasmopt:-O0} \
%{!noasmopt:%{O|O1|O2|O3:-O2}}"

#undef LINK_SPEC
#define LINK_SPEC "\
%{G*} %{EB} %{EL} %{mips1} %{mips2} %{mips3} \
%{bestGnum} %{shared} %{non_shared} \
%{call_shared} %{no_archive} %{exact_version} \
%{static: -non_shared} \
%{!static: \
  %{!shared:%{!non_shared:%{!call_shared: -call_shared -no_unresolved}}}} \
%{rpath} \
-_SYSTYPE_SVR4"

/* We now support shared libraries.  */
#define IRIX_STARTFILE_SPEC "\
%{!static: \
  %{!shared:%{pg:gcrt1.o%s}%{!pg:%{p:mcrt1.o%s libprof1.a%s}%{!p:crt1.o%s}}}} \
%{static: \
  %{pg:gcrt1.o%s} \
  %{!pg:%{p:/usr/lib/nonshared/mcrt1.o%s libprof1.a%s} \
  %{!p:/usr/lib/nonshared/crt1.o%s}}}"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%(irix_startfile_spec)"

#undef LIB_SPEC
#define LIB_SPEC "%{!shared:%{p:-lprof1} %{pg:-lprof1} -lc}"

#define IRIX_ENDFILE_SPEC "%{!shared:crtn.o%s}"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC "%(irix_endfile_spec)"

/* We do not want to run mips-tfile!  */
#undef ASM_FINAL_SPEC

/* The system header files are C++ aware.  */
/* ??? Unfortunately, most but not all of the headers are C++ aware.
   Specifically, curses.h is not, and as a consequence, defining this
   used to prevent libg++ building.  This is no longer the case so
   define it again to prevent other problems, e.g. with getopt in
   unistd.h.  We still need some way to fix just those files that need
   fixing.  */
#define NO_IMPLICIT_EXTERN_C 1

/* We don't support debugging info for now.  */
#undef DBX_DEBUGGING_INFO
#undef MIPS_DEBUGGING_INFO
#undef PREFERRED_DEBUGGING_TYPE

/* Likewise, the assembler doesn't handle DWARF2 directives.  */
#define DWARF2_UNWIND_INFO 0

#undef MACHINE_TYPE
#define MACHINE_TYPE "SGI running IRIX 5.x"

/* Always use 1 for .file number.  I [meissner@osf.org] wonder why
   IRIX needs this.  */

#undef SET_FILE_NUMBER
#define SET_FILE_NUMBER() num_source_filenames = 1

/* Put out a label after a .loc.  I [meissner@osf.org] wonder why
   IRIX needs this.  */

#undef LABEL_AFTER_LOC
#define LABEL_AFTER_LOC(STREAM) fprintf (STREAM, "LM%d:\n", ++sym_lineno)

 /* Dollar signs are OK in IRIX 5 but not in IRIX 3.  */
#undef DOLLARS_IN_IDENTIFIERS
#undef NO_DOLLAR_IN_LABEL

/* -G is incompatible with -KPIC which is the default, so only allow objects
   in the small data section if the user explicitly asks for it.  */
#undef MIPS_DEFAULT_GVALUE
#define MIPS_DEFAULT_GVALUE 0

/* Switch into a generic section.  */
#undef TARGET_ASM_NAMED_SECTION
#define TARGET_ASM_NAMED_SECTION  irix_asm_named_section

/* Define functions to read the name and flags of the current section.
   They are used by irix_asm_output_align.  */

#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS						\
const char *								\
current_section_name (void)						\
{									\
  switch (in_section)							\
    {									\
    case no_section:	return NULL;					\
    case in_text:	return ".text";					\
    case in_data:	return ".data";					\
    case in_bss:	return ".bss";					\
    case in_readonly_data:						\
      if (mips_abi != ABI_32 && mips_abi != ABI_O64)			\
	return ".rodata";						\
      else								\
	return ".rdata";						\
    case in_named:							\
      return in_named_name;						\
    }									\
  abort ();								\
}									\
									\
unsigned int								\
current_section_flags (void)						\
{									\
  switch (in_section)							\
    {									\
    case no_section:	return 0;					\
    case in_text:	return SECTION_CODE;				\
    case in_data:	return SECTION_WRITE;				\
    case in_bss:	return SECTION_WRITE | SECTION_BSS;		\
    case in_readonly_data: return 0;					\
    case in_named:	return get_named_section_flags (in_named_name);	\
    }									\
  abort ();								\
}

/* Some assemblers have a bug that causes backslash escaped chars in .ascii
   to be misassembled, so avoid it by using .byte instead.  Write the original
   string in a comment, partly to improve readability and partly for the sake
   of scan-assembler-type tests.  */
#undef ASM_OUTPUT_ASCII
#define ASM_OUTPUT_ASCII(FILE,PTR,LEN)				\
do {								\
  const unsigned char *s_ = (const unsigned char *)(PTR);	\
  unsigned len_ = (LEN);					\
  unsigned i_;							\
  mips_output_ascii (FILE, (const char *) s_, len_, "\t# ");	\
  for (i_ = 0; i_ < len_; s_++, i_++)				\
    {								\
      if ((i_ % 8) == 0)					\
	fputs ("\n\t.byte\t", (FILE));				\
      fprintf ((FILE), "%s0x%x", (i_%8?",":""), *s_);		\
    }								\
  fputs ("\n", (FILE));						\
} while (0)

/* Also do this for libcalls.  */
#undef TARGET_ASM_EXTERNAL_LIBCALL
#define TARGET_ASM_EXTERNAL_LIBCALL irix_output_external_libcall

/* This does for functions what ASM_DECLARE_OBJECT_NAME does for variables.
   This is used indirectly by ASM_OUTPUT_EXTERNAL.  */
#define ASM_DECLARE_FUNCTION_SIZE(STREAM, NAME, DECL)	\
do {							\
  tree name_tree = get_identifier (NAME);		\
  TREE_ASM_WRITTEN (name_tree) = 1;			\
} while (0)

/* This is how we tell the assembler that a symbol is weak.  */

#define ASM_OUTPUT_WEAK_ALIAS(FILE, NAME, VALUE)	\
  do							\
    {							\
      (*targetm.asm_out.globalize_label) (FILE, NAME);  \
      fputs ("\t.weakext\t", FILE);			\
      assemble_name (FILE, NAME);			\
      if (VALUE)					\
        {						\
          fputc (' ', FILE);				\
          assemble_name (FILE, VALUE);			\
        }						\
      fputc ('\n', FILE);				\
    }							\
  while (0)

#define ASM_WEAKEN_LABEL(FILE, NAME) ASM_OUTPUT_WEAK_ALIAS(FILE, NAME, 0)

/* Handle #pragma weak and #pragma pack.  */
#define HANDLE_SYSV_PRAGMA 1

#undef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS \
  { "irix_startfile_spec", IRIX_STARTFILE_SPEC }, \
  { "irix_endfile_spec", IRIX_ENDFILE_SPEC },
