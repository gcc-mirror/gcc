/* Definitions of target machine for GNU compiler for Intel 80386
   running FreeBSD.
   Copyright (C) 1988, 1992, 1994 Free Software Foundation, Inc.
   Contributed by Poul-Henning Kamp <phk@login.dkuug.dk>

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* This goes away when the math-emulator is fixed */
#define TARGET_CPU_DEFAULT 0400		/* TARGET_NO_FANCY_MATH_387 */

/* This is tested by i386gas.h.  */
#define YES_UNDERSCORES

#include "i386/gstabs.h"

/* Get perform_* macros to build libgcc.a.  */
#include "i386/perform.h"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dunix -Di386 -D__FreeBSD__ -D__386BSD__ -Asystem(unix) -Asystem(FreeBSD) -Acpu(i386) -Amachine(i386)"

#define INCLUDE_DEFAULTS { \
	{ "/usr/include", 0 }, \
	{ "/usr/include/g++", 1 }, \
	{ 0, 0} \
	}

/* Like the default, except no -lg.  */
#define LIB_SPEC "%{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p}"

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "short unsigned int"

#define WCHAR_UNSIGNED 1

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 16

#define HAVE_ATEXIT

/* Redefine this to use %eax instead of %edx.  */
#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO)  \
{									\
  if (flag_pic)								\
    {									\
      fprintf (FILE, "\tleal %sP%d@GOTOFF(%%ebx),%%eax\n",		\
	       LPREFIX, (LABELNO));					\
      fprintf (FILE, "\tcall *mcount@GOT(%%ebx)\n");			\
    }									\
  else									\
    {									\
      fprintf (FILE, "\tmovl $%sP%d,%%eax\n", LPREFIX, (LABELNO));	\
      fprintf (FILE, "\tcall mcount\n");				\
    }									\
}

/* There are conflicting reports about whether this system uses
   a different assembler syntax.  wilson@cygnus.com says # is right.  */
#undef COMMENT_BEGIN
#define COMMENT_BEGIN "#"

#undef ASM_APP_ON
#define ASM_APP_ON "#APP\n"

#undef ASM_APP_OFF
#define ASM_APP_OFF "#NO_APP\n"

/* The following macros are stolen from i386v4.h */
/* These have to be defined to get PIC code correct */

/* This is how to output an element of a case-vector that is relative.
   This is only used for PIC code.  See comments by the `casesi' insn in
   i386.md for an explanation of the expression this outputs. */

#undef ASM_OUTPUT_ADDR_DIFF_ELT
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, VALUE, REL) \
  fprintf (FILE, "\t.long _GLOBAL_OFFSET_TABLE_+[.-%s%d]\n", LPREFIX, VALUE)

/* Indicate that jump tables go in the text section.  This is
   necessary when compiling PIC code.  */

#define JUMP_TABLES_IN_TEXT_SECTION

/* Don't default to pcc-struct-return, because gcc is the only compiler, and
   we want to retain compatibility with older gcc versions.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

/*
 * Some imports from svr4.h in support of shared libraries.
 * Currently, we need the DECLARE_OBJECT_SIZE stuff.
 */

/* Define the strings used for the special svr4 .type and .size directives.
   These strings generally do not vary from one system running svr4 to
   another, but if a given system (e.g. m88k running svr) needs to use
   different pseudo-op names for these, they may be overridden in the
   file which includes this one.  */

#define TYPE_ASM_OP	".type"
#define SIZE_ASM_OP	".size"
#define WEAK_ASM_OP	".weak"

/* The following macro defines the format used to output the second
   operand of the .type assembler directive.  Different svr4 assemblers
   expect various different forms for this operand.  The one given here
   is just a default.  You may need to override it in your machine-
   specific tm.h file (depending upon the particulars of your assembler).  */

#define TYPE_OPERAND_FMT	"@%s"

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

#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)			\
  do {									\
    fprintf (FILE, "\t%s\t ", TYPE_ASM_OP);				\
    assemble_name (FILE, NAME);						\
    putc (',', FILE);							\
    fprintf (FILE, TYPE_OPERAND_FMT, "function");			\
    putc ('\n', FILE);							\
    ASM_DECLARE_RESULT (FILE, DECL_RESULT (DECL));			\
    ASM_OUTPUT_LABEL(FILE, NAME);					\
  } while (0)

/* Write the extra assembler code needed to declare an object properly.  */

#define ASM_DECLARE_OBJECT_NAME(FILE, NAME, DECL)			\
  do {									\
    fprintf (FILE, "\t%s\t ", TYPE_ASM_OP);				\
    assemble_name (FILE, NAME);						\
    putc (',', FILE);							\
    fprintf (FILE, TYPE_OPERAND_FMT, "object");				\
    putc ('\n', FILE);							\
    size_directive_output = 0;						\
    if (!flag_inhibit_size_directive && DECL_SIZE (DECL))		\
      {									\
        size_directive_output = 1;					\
	fprintf (FILE, "\t%s\t ", SIZE_ASM_OP);				\
	assemble_name (FILE, NAME);					\
	fprintf (FILE, ",%d\n",  int_size_in_bytes (TREE_TYPE (DECL)));	\
      }									\
    ASM_OUTPUT_LABEL(FILE, NAME);					\
  } while (0)

/* Output the size directive for a decl in rest_of_decl_compilation
   in the case where we did not do so before the initializer.
   Once we find the error_mark_node, we know that the value of
   size_directive_output was set
   by ASM_DECLARE_OBJECT_NAME when it was run for the same decl.  */

#define ASM_FINISH_DECLARE_OBJECT(FILE, DECL, TOP_LEVEL, AT_END)        \
do {                                                                    \
     char *name = XSTR (XEXP (DECL_RTL (DECL), 0), 0);                  \
     if (!flag_inhibit_size_directive && DECL_SIZE (DECL)	        \
         && ! AT_END && TOP_LEVEL                                       \
         && DECL_INITIAL (DECL) == error_mark_node                      \
         && !size_directive_output)                                     \
       {                                                                \
         fprintf (FILE, "\t%s\t ", SIZE_ASM_OP);                        \
	 assemble_name (FILE, name);                                    \
	 fprintf (FILE, ",%d\n",  int_size_in_bytes (TREE_TYPE (DECL)));\
	}								\
   } while (0)


/* This is how to declare the size of a function.  */

#define ASM_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL)			\
  do {									\
    if (!flag_inhibit_size_directive)					\
      {									\
        char label[256];						\
	static int labelno;						\
	labelno++;							\
	ASM_GENERATE_INTERNAL_LABEL (label, "Lfe", labelno);		\
	ASM_OUTPUT_INTERNAL_LABEL (FILE, "Lfe", labelno);		\
	fprintf (FILE, "\t%s\t ", SIZE_ASM_OP);				\
	assemble_name (FILE, (FNAME));					\
        fprintf (FILE, ",");						\
	assemble_name (FILE, label);					\
        fprintf (FILE, "-");						\
	assemble_name (FILE, (FNAME));					\
	putc ('\n', FILE);						\
      }									\
  } while (0)

#define ASM_SPEC   " %| %{fpic:-k} %{fPIC:-k}"
#define LINK_SPEC \
  "%{!nostdlib:%{!r*:%{!e*:-e start}}} -dc -dp %{static:-Bstatic} %{assert*}"

/* This section copied from i386/osfrose.h */

/* A C statement or compound statement to output to FILE some
   assembler code to initialize basic-block profiling for the current
   object module.  This code should call the subroutine
   `__bb_init_func' once per object module, passing it as its sole
   argument the address of a block allocated in the object module.

   The name of the block is a local symbol made with this statement:

	ASM_GENERATE_INTERNAL_LABEL (BUFFER, "LPBX", 0);

   Of course, since you are writing the definition of
   `ASM_GENERATE_INTERNAL_LABEL' as well as that of this macro, you
   can take a short cut in the definition of this macro and use the
   name that you know will result.

   The first word of this block is a flag which will be nonzero if the
   object module has already been initialized.  So test this word
   first, and do not call `__bb_init_func' if the flag is nonzero.  */

#undef	FUNCTION_BLOCK_PROFILER
#define FUNCTION_BLOCK_PROFILER(STREAM, LABELNO)			\
do									\
  {									\
    if (!flag_pic)							\
      {									\
	fprintf (STREAM, "\tcmpl $0,%sPBX0\n", LPREFIX);		\
	fprintf (STREAM, "\tjne 0f\n");					\
	fprintf (STREAM, "\tpushl $%sPBX0\n", LPREFIX);			\
	fprintf (STREAM, "\tcall ___bb_init_func\n");			\
	fprintf (STREAM, "0:\n");					\
      }									\
    else								\
      {									\
	fprintf (STREAM, "\tpushl %eax\n");				\
	fprintf (STREAM, "\tmovl %sPBX0@GOT(%ebx),%eax\n");		\
	fprintf (STREAM, "\tcmpl $0,(%eax)\n");				\
	fprintf (STREAM, "\tjne 0f\n");					\
	fprintf (STREAM, "\tpushl %eax\n");				\
	fprintf (STREAM, "\tcall ___bb_init_func@PLT\n");		\
	fprintf (STREAM, "0:\n");					\
	fprintf (STREAM, "\tpopl %eax\n");				\
      }									\
  }									\
while (0)

/* A C statement or compound statement to increment the count
   associated with the basic block number BLOCKNO.  Basic blocks are
   numbered separately from zero within each compilation.  The count
   associated with block number BLOCKNO is at index BLOCKNO in a
   vector of words; the name of this array is a local symbol made
   with this statement:

	ASM_GENERATE_INTERNAL_LABEL (BUFFER, "LPBX", 2);

   Of course, since you are writing the definition of
   `ASM_GENERATE_INTERNAL_LABEL' as well as that of this macro, you
   can take a short cut in the definition of this macro and use the
   name that you know will result.  */

#undef	BLOCK_PROFILER
#define BLOCK_PROFILER(STREAM, BLOCKNO)					\
do									\
  {									\
    if (!flag_pic)							\
      fprintf (STREAM, "\tincl %sPBX2+%d\n", LPREFIX, (BLOCKNO)*4);	\
    else								\
      {									\
	fprintf (STREAM, "\tpushl %eax\n");				\
	fprintf (STREAM, "\tmovl %sPBX2@GOT(%ebx),%eax\n", LPREFIX);	\
	fprintf (STREAM, "\tincl %d(%eax)\n", (BLOCKNO)*4);		\
	fprintf (STREAM, "\tpopl %eax\n");				\
      }									\
  }									\
while (0)

/* This is defined when gcc is compiled in the BSD-directory-tree, and must
 * make up for the gap to all the stuff done in the GNU-makefiles.
 */

#ifdef FREEBSD_NATIVE

#undef MD_EXEC_PREFIX
#define MD_EXEC_PREFIX "/usr/libexec/"

#undef STANDARD_STARTFILE_PREFIX
#define STANDARD_STARTFILE_PREFIX "/usr/lib"

#define DEFAULT_TARGET_MACHINE "i386-unknown-freebsd_1.0"
#define GPLUSPLUS_INCLUDE_DIR "/usr/local/lib/gcc-lib/i386-unknown-freebsd_1.0/2.5.8/include"
#define TOOL_INCLUDE_DIR "/usr/local/i386-unknown-freebsd_1.0/include"
#define GCC_INCLUDE_DIR "/usr/local/lib/gcc-lib/i386-unknown-freebsd_1.0/2.5.8/include"

#endif /* FREEBSD_NATIVE */
