/* Configuration for an OpenBSD i386 target.
   
   Copyright (C) 2005-2019 Free Software Foundation, Inc.

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

#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
    	OPENBSD_OS_CPP_BUILTINS();		\
    }						\
  while (0)

#undef DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(n) \
  (TARGET_64BIT ? dbx64_register_map[n] : svr4_dbx_register_map[n])

/* This must agree with <machine/_types.h>.  */
#undef SIZE_TYPE
#define SIZE_TYPE "long unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "long int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

#undef WINT_TYPE
#define WINT_TYPE "int"

/* Don't default to pcc-struct-return, because gcc is the only compiler, and
   we want to retain compatibility with older gcc versions.  */

#undef DEFAULT_PCC_STRUCT_RETURN
#define DEFAULT_PCC_STRUCT_RETURN 0

/* Override the default comment-starter of "/".  */
#undef ASM_COMMENT_START
#define ASM_COMMENT_START "#"

#undef ASM_APP_ON
#define ASM_APP_ON "#APP\n"

#undef ASM_APP_OFF
#define ASM_APP_OFF "#NO_APP\n"

/* A C statement to output to the stdio stream FILE an assembler
   command to advance the location counter to a multiple of 1<<LOG
   bytes if it is within MAX_SKIP bytes.  */

#define SUBALIGN_LOG 3

#ifdef HAVE_GAS_MAX_SKIP_P2ALIGN
#define ASM_OUTPUT_MAX_SKIP_ALIGN(FILE,LOG,MAX_SKIP)			\
  do {									\
    if ((LOG) != 0) {							\
      if ((MAX_SKIP) == 0 || (MAX_SKIP) >= (1 << (LOG)) - 1)		\
	fprintf ((FILE), "\t.p2align %d\n", (LOG));			\
      else								\
	fprintf ((FILE), "\t.p2align %d,,%d\n", (LOG), (MAX_SKIP));	\
    }									\
  } while (0)
#endif

/* OpenBSD's profiler recovers all information from the stack pointer.
   The icky part is not here, but in <machine/profile.h>.  */
#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO)  \
  fputs (flag_pic ? "\tcall __mcount@PLT\n": "\tcall __mcount\n", FILE);

#undef LINK_SPEC
#define LINK_SPEC \
  "%{!shared:%{!nostdlib:%{!r:%{!e*:-e __start}}}} \
   %{shared:-shared} %{R*} \
   %{static:-Bstatic} \
   %{!static:-Bdynamic} \
   %{assert*} \
   -dynamic-linker /usr/libexec/ld.so"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "\
	%{!shared: %{pg:gcrt0%O%s} %{!pg:%{p:gcrt0%O%s} %{!p:crt0%O%s}} \
	crtbegin%O%s} %{shared:crtbeginS%O%s}"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC "%{!shared:crtend%O%s} %{shared:crtendS%O%s}"

#define OBSD_HAS_CORRECT_SPECS

#define HAVE_ENABLE_EXECUTE_STACK
