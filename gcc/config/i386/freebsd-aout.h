/* Definitions of target machine for GNU compiler for Intel 80386
   running FreeBSD.
   Copyright (C) 1988, 1992, 1994, 1996, 1997, 1999, 2000, 2002, 2003
   Free Software Foundation, Inc.
   Contributed by Poul-Henning Kamp <phk@login.dkuug.dk>
   Continued development by David O'Brien <obrien@NUXI.org>

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

/* Don't assume anything about the header files.  */
#define NO_IMPLICIT_EXTERN_C

/* This goes away when the math-emulator is fixed */
#undef TARGET_SUBTARGET_DEFAULT
#define TARGET_SUBTARGET_DEFAULT \
  (MASK_80387 | MASK_IEEE_FP | MASK_FLOAT_RETURNS | MASK_NO_FANCY_MATH_387)

/* The macro defined in i386.h doesn't work with the old gas of
   FreeBSD 2.x.  The definition in sco.h and sol2.h appears to work,
   but it turns out that, even though the assembler doesn't complain,
   we get incorrect results.  Fortunately, the definition in
   defaults.h works.  */
#undef ASM_PREFERRED_EH_DATA_FORMAT

#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
	builtin_define_std ("unix");		\
	builtin_define ("__FreeBSD__");		\
	builtin_assert ("system=unix");		\
	builtin_assert ("system=bsd");		\
	builtin_assert ("system=FreeBSD");	\
    }						\
  while (0)

/* Like the default, except no -lg.  */
#define LIB_SPEC "%{!shared:%{!pg:-lc}%{pg:-lc_p}}"

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD

/* Override the default comment-starter of "/".  */

#undef ASM_COMMENT_START
#define ASM_COMMENT_START "#"

#undef ASM_APP_ON
#define ASM_APP_ON "#APP\n"

#undef ASM_APP_OFF
#define ASM_APP_OFF "#NO_APP\n"

/* FreeBSD using a.out does not support DWARF2 unwinding mechanisms.  */
#define DWARF2_UNWIND_INFO 0

/* Don't default to pcc-struct-return, because in FreeBSD we prefer the
   superior nature of the older gcc way.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* Ensure we the configuration knows our system correctly so we can link with
   libraries compiled with the native cc.  */
#undef NO_DOLLAR_IN_LABEL

/* i386 freebsd still uses old binutils that don't insert nops by default
   when the .align directive demands to insert extra space in the text
   segment.  */
#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG) \
  if ((LOG)!=0) fprintf ((FILE), "\t.align %d,0x90\n", (LOG))

/* Profiling routines, partially copied from i386/osfrose.h.  */

/* Tell final.c that we don't need a label passed to mcount.  */
#define NO_PROFILE_COUNTERS 1

#undef MCOUNT_NAME
#define MCOUNT_NAME "mcount"
#undef PROFILE_COUNT_REGISTER
#define PROFILE_COUNT_REGISTER "eax"

/*
 * Some imports from svr4.h in support of shared libraries.
 * Currently, we need the DECLARE_OBJECT_SIZE stuff.
 */

/* Define the strings used for the special svr4 .type and .size directives.
   These strings generally do not vary from one system running svr4 to
   another, but if a given system (e.g. m88k running svr) needs to use
   different pseudo-op names for these, they may be overridden in the
   file which includes this one.  */

#define TYPE_ASM_OP	"\t.type\t"
#define SIZE_ASM_OP	"\t.size\t"
#define SET_ASM_OP	"\t.set\t"

/* The following macro defines the format used to output the second
   operand of the .type assembler directive.  Different svr4 assemblers
   expect various different forms for this operand.  The one given here
   is just a default.  You may need to override it in your machine-
   specific tm.h file (depending upon the particulars of your assembler).  */

#define TYPE_OPERAND_FMT	"@%s"

#define HANDLE_SYSV_PRAGMA	1

#define ASM_WEAKEN_LABEL(FILE,NAME) \
	do { fputs ("\t.weak\t", FILE); assemble_name (FILE, NAME); \
	fputc ('\n', FILE); } while (0)

/* Write the extra assembler code needed to declare a function's result.
   Most svr4 assemblers don't require any special declaration of the
   result value, but there are exceptions.  */

#ifndef ASM_DECLARE_RESULT
#define ASM_DECLARE_RESULT(FILE, RESULT)
#endif

/* These macros generate the special .type and .size directives which
   are used to set the corresponding fields of the linker symbol table
   entries in an ELF object file under SVR4.  These macros also output
   the starting labels for the relevant functions/objects.  */

/* Write the extra assembler code needed to declare a function properly.
   Some svr4 assemblers need to also have something extra said about the
   function's return value.  We allow for that here.  */

#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)		\
  do								\
    {								\
      ASM_OUTPUT_TYPE_DIRECTIVE (FILE, NAME, "function");	\
      ASM_DECLARE_RESULT (FILE, DECL_RESULT (DECL));		\
      ASM_OUTPUT_LABEL (FILE, NAME);				\
    }								\
  while (0)

/* Write the extra assembler code needed to declare an object properly.  */

#define ASM_DECLARE_OBJECT_NAME(FILE, NAME, DECL)		\
  do								\
    {								\
      HOST_WIDE_INT size;					\
								\
      ASM_OUTPUT_TYPE_DIRECTIVE (FILE, NAME, "object");		\
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

/* Output the size directive for a decl in rest_of_decl_compilation
   in the case where we did not do so before the initializer.
   Once we find the error_mark_node, we know that the value of
   size_directive_output was set
   by ASM_DECLARE_OBJECT_NAME when it was run for the same decl.  */

#undef ASM_FINISH_DECLARE_OBJECT
#define ASM_FINISH_DECLARE_OBJECT(FILE, DECL, TOP_LEVEL, AT_END)        \
do {                                                                    \
     const char *name = XSTR (XEXP (DECL_RTL (DECL), 0), 0);            \
     HOST_WIDE_INT size;						\
     if (!flag_inhibit_size_directive && DECL_SIZE (DECL)	        \
         && ! AT_END && TOP_LEVEL                                       \
         && DECL_INITIAL (DECL) == error_mark_node                      \
         && !size_directive_output)                                     \
       {                                                                \
	 size_directive_output = 1;					\
	 size = int_size_in_bytes (TREE_TYPE (DECL));			\
	 ASM_OUTPUT_SIZE_DIRECTIVE (FILE, name, size);			\
       }								\
   } while (0)

/* This is how to declare the size of a function.  */

#define ASM_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL)			\
  do {									\
    if (!flag_inhibit_size_directive)					\
      ASM_OUTPUT_MEASURED_SIZE (FILE, FNAME);				\
  } while (0)

#define AS_NEEDS_DASH_FOR_PIPED_INPUT
#define ASM_SPEC   "%{fpic|fpie|fPIC|fPIE:-k}"
#define LINK_SPEC \
  "%{p:%e`-p' not supported; use `-pg' and gprof(1)} \
   %{shared:-Bshareable} \
   %{!shared:%{!nostdlib:%{!r:%{!e*:-e start}}} -dc -dp %{static:-Bstatic} \
   %{pg:-Bstatic} %{Z}} \
   %{assert*} %{R*}"

#define STARTFILE_SPEC  \
  "%{shared:c++rt0.o%s} \
   %{!shared:%{pg:gcrt0.o%s}%{!pg:%{static:scrt0.o%s}%{!static:crt0.o%s}}}"

/* Define this so we can compile MS code for use with WINE.  */
#define HANDLE_PRAGMA_PACK_PUSH_POP

/* FreeBSD 2.2.7's assembler does not support .quad properly.  Do not
   use it.  */
#undef ASM_QUAD
