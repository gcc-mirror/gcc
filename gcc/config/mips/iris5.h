/* Definitions of target machine for GNU compiler.  Iris version 5.
   Copyright (C) 1993, 1995, 1996, 1998, 2000,
   2001, 2002 Free Software Foundation, Inc.

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

#define TARGET_IRIX5 1

#ifndef TARGET_DEFAULT
#define	TARGET_DEFAULT	MASK_ABICALLS
#endif
#define ABICALLS_ASM_OP "\t.option pic2"

#include "mips/iris3.h"
#include "mips/mips.h"
#include "mips/iris4.h"

/* Irix 5 doesn't use COFF, so disable special COFF handling in collect2.c.  */
#undef OBJECT_FORMAT_COFF

/* ??? This is correct, but not very useful, because there is no file that
   uses this macro.  */
/* ??? The best way to handle global constructors under ELF is to use .init
   and .fini sections.  Unfortunately, there is apparently no way to get
   the Irix 5.x (x <= 2) assembler to create these sections.  So we instead
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

#define WORD_SWITCH_TAKES_ARG(STR)			\
 (DEFAULT_WORD_SWITCH_TAKES_ARG (STR)			\
  || !strcmp (STR, "rpath"))

#define TARGET_OS_CPP_BUILTINS()			\
    do {						\
	builtin_define_std ("host_mips");		\
	builtin_define_std ("sgi");			\
	builtin_define_std ("unix");			\
	builtin_define_std ("SYSTYPE_SVR4");		\
	builtin_define ("_MODERN_C");			\
	builtin_define ("_SVR4_SOURCE");		\
	builtin_define ("__DSO__");			\
	builtin_define ("_MIPS_SIM=_MIPS_SIM_ABI32");	\
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
#undef STARTFILE_SPEC
#define STARTFILE_SPEC "\
%{!static: \
  %{!shared:%{pg:gcrt1.o%s}%{!pg:%{p:mcrt1.o%s libprof1.a%s}%{!p:crt1.o%s}}}} \
%{static: \
  %{pg:gcrt1.o%s} \
  %{!pg:%{p:/usr/lib/nonshared/mcrt1.o%s libprof1.a%s} \
  %{!p:/usr/lib/nonshared/crt1.o%s}}}"

#undef LIB_SPEC
#define LIB_SPEC "%{!shared:%{p:-lprof1} %{pg:-lprof1} -lc}"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC "%{!shared:crtn.o%s}"

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
#undef SDB_DEBUGGING_INFO
#undef MIPS_DEBUGGING_INFO
#undef PREFERRED_DEBUGGING_TYPE

/* Likewise, the assembler doesn't handle DWARF2 directives.  */
#define DWARF2_UNWIND_INFO 0

#undef MACHINE_TYPE
#define MACHINE_TYPE "SGI running IRIX 5.x"

 /* Dollar signs are OK in Irix5 but not in Irix3.  */
#undef DOLLARS_IN_IDENTIFIERS
#undef NO_DOLLAR_IN_LABEL

/* -G is incompatible with -KPIC which is the default, so only allow objects
   in the small data section if the user explicitly asks for it.  */
#undef MIPS_DEFAULT_GVALUE
#define MIPS_DEFAULT_GVALUE 0

/* In Irix 5, we must output a `.global name .text' directive for every used
   but undefined function.  If we don't, the linker may perform an optimization
   (skipping over the insns that set $gp) when it is unsafe.  This is used
   indirectly by ASM_OUTPUT_EXTERNAL.  */
#define ASM_OUTPUT_UNDEF_FUNCTION(FILE, NAME)	\
do {						\
  fputs ("\t.globl ", FILE);			\
  assemble_name (FILE, NAME);			\
  fputs (" .text\n", FILE);			\
} while (0)

/* Also do this for libcalls.  */
#define ASM_OUTPUT_EXTERNAL_LIBCALL(FILE, FUN)	\
  mips_output_external_libcall (FILE, XSTR (FUN, 0))

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
