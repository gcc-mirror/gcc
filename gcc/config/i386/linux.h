/* Definitions for Intel 386 running Linux
 * Copyright (C) 1992 Free Software Foundation, Inc.
 *
 * Written by H.J. Lu (hlu@eecs.wsu.edu)
 *
 * Linux is a POSIX.1 compatible UNIX clone for i386, which uses GNU
 * stuffs as the native stuffs.
 */

#if 0	/* The FSF has fixed the known bugs. But ....... */

/* Linux has a hacked gas 1.38.1, which can handle repz, repnz
 * and fildll.
 */

#define GOOD_GAS

#endif

/* This is tested by i386/gas.h.  */
#define YES_UNDERSCORES

#include "i386/gstabs.h"

/* Specify predefined symbols in preprocessor.  */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dunix -Di386 -Dlinux"

#undef CPP_SPEC
#if TARGET_CPU_DEFAULT == 2
#define CPP_SPEC "%{!m386:-D__i486__} %{posix:-D_POSIX_SOURCE}"
#else
#define CPP_SPEC "%{m486:-D__i486__} %{posix:-D_POSIX_SOURCE}"
#endif

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "long int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD

#undef HAVE_ATEXIT
#define HAVE_ATEXIT

/* Linux uses ctype from glibc.a. I am not sure how complete it is.
 * For now, we play safe. It may change later.
 */
#if 0
#undef MULTIBYTE_CHARS
#define MULTIBYTE_CHARS	1
#endif

#undef LIB_SPEC
#define LIB_SPEC "%{g*:-lg} %{!g*:%{!p:%{!pg:-lc}}%{p:-lgmon -lc_p}%{pg:-lgmon -lc_p}}"


#undef STARTFILE_SPEC
#undef GPLUSPLUS_INCLUDE_DIR

#ifdef CROSS_COMPILE

/*
 * For cross-compile, we just need to search `$(tooldir)/lib'
 */

#define STARTFILE_SPEC  \
  "%{g*:crt0.o%s -static} %{!g*:%{pg:gcrt0.o%s -static} %{!pg:%{p:gcrt0.o%s -static} %{!p:crt0.o%s %{!static:%{nojump:-nojump}} %{static:-static}}}} -L"TOOLDIR"/lib"

/*
 *The cross-compile uses this.
 */
#define GPLUSPLUS_INCLUDE_DIR TOOLDIR"/g++-include"

#else

#define STARTFILE_SPEC  \
  "%{g*:crt0.o%s -static} %{!g*:%{pg:gcrt0.o%s -static} %{!pg:%{p:gcrt0.o%s -static} %{!p:crt0.o%s %{!static:%{nojump:-nojump}} %{static:-static}}}}"

/*
 *The native Linux system uses this.
 */
#define GPLUSPLUS_INCLUDE_DIR "/usr/g++-include"

#endif

/* The following macros are copied from i386bsd.h.  */

/* Redefine this to use %eax instead of %edx.  */
#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO)  \
{									\
  if (flag_pic)								\
    {									\
      fprintf (FILE, "\tleal %sP%d@GOTOFF(%%ebx),%%eax\n",		\
	       LPREFIX, (LABELNO));					\
      fprintf (FILE, "\tcall *_mcount@GOT(%%ebx)\n");			\
    }									\
  else									\
    {									\
      fprintf (FILE, "\tmovl $%sP%d,%%eax\n", LPREFIX, (LABELNO));	\
      fprintf (FILE, "\tcall _mcount\n");				\
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

/* The following macros are copied from i386v4.h.  */

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
