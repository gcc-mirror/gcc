/* Definitions of target machine GNU compiler.  IA-64/AIX version.
   Copyright (C) 1999, 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Timothy Wall (twall@cygnus.com)

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

/* AIX5 (aka Monterey): a mix of AIX and UnixWare.  
   This file is loosely based on ia64/linux.h.  */

/* This macro is a C statement to print on `stderr' a string describing the
   particular machine description choice.  */

#define TARGET_VERSION fprintf (stderr, " (IA-64) AIX");

#undef ASM_APP_ON
#define ASM_APP_ON "#APP\n"

#undef ASM_APP_OFF
#define ASM_APP_OFF "#NO_APP\n"

#define SET_ASM_OP	"\t.set\t"

#undef MD_EXEC_PREFIX
#undef MD_STARTFILE_PREFIX
#define MD_STARTFILE_PREFIX "/usr/lib/ia64l64/"

/* Output at beginning of assembler file.  */
/* The .file command should always begin the output.  */
#undef ASM_FILE_START
#define ASM_FILE_START(FILE)						\
  do {									\
	output_file_directive (FILE, main_input_filename);		\
	fprintf (FILE, "\t.version\t\"01.01\"\n");			\
  } while (0)

/* Provide a STARTFILE_SPEC appropriate for AIX.  Here we add
   the crti C++ startup files file which provide part of the support
   for getting C++ file-scope static object constructed before entering
   `main'.  */ 
   
#undef	STARTFILE_SPEC
#define STARTFILE_SPEC \
"%{!shared: \
   %{pg:gcrt1_64.o%s} %{!pg:%{p:mcrt1_64.o%s} \
                        %{!p:%{profile:gcrt1_64.o%s} \
                          %{!profile:crt1_64.o%s}}}} \
 crti.o%s %{!shared:crtbegin.o%s} %{shared:crtbeginS.o%s}"

/* Provide a ENDFILE_SPEC appropriate for AIX.  Here we tack on
   the crtn file which provides termination of the support for getting C++
   file-scope static object constructed before entering `main'.  */

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC "%{!shared:crtend.o%s} %{shared:crtendS.o%s} crtn.o%s"

/* Define this so we can compile MS code for use with WINE.  */
#define HANDLE_PRAGMA_PACK_PUSH_POP

/* Target OS builtins.  */
#define TARGET_OS_CPP_BUILTINS()			\
do {							\
	if (flag_iso)					\
	  builtin_define("_ANSI_C_SOURCE");		\
	builtin_define("_AIX");				\
	builtin_define("_AIX64");			\
	builtin_define("unix");				\
	builtin_assert("system=unix");			\
	builtin_assert("system=aix");			\
	builtin_define("__64BIT__");			\
	builtin_define("_LONG_LONG");			\
	builtin_define("_IA64");			\
	builtin_define("__int128=__size128_t");		\
	if (c_language == clk_cplusplus)		\
	  {						\
	    builtin_define("_XOPEN_SOURCE=500");	\
	    builtin_define("_XOPEN_SOURCE_EXTENDED=1");	\
	    builtin_define("_LARGE_FILE_API");		\
	    builtin_define("_ALL_SOURCE");		\
	  }						\
} while (0)

/* A C string constant that tells the GNU CC driver program options to pass to
   CPP.  It can also specify how to translate options you give to GNU CC into
   options for GNU CC to pass to the CPP.  */

#undef CPP_SPEC
#define CPP_SPEC "\
%{posix:-D_POSIX_SOURCE}"

/* Define this for shared library support.  */

#undef LINK_SPEC
#define LINK_SPEC "\
%{shared:-shared} \
%{!shared: \
  %{!static: \
    %{rdynamic:-export-dynamic} \
    %{!dynamic-linker:-dynamic-linker /usr/lib/ia64l64/libc.so.1}} \
    %{static:-static}}"

#define JMP_BUF_SIZE  85

/* A C statement or compound statement to output to FILE some assembler code to
   call the profiling subroutine `mcount'.  

   FIXME this is not supported until xlC supports it and can thus tell us
   how to do it.
*/

#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO)		\
do {							\
} while (0)

/* Tell the linker where to find the crt*.o files.  */

#ifndef CROSS_COMPILE
#undef STANDARD_STARTFILE_PREFIX
#define STANDARD_STARTFILE_PREFIX "/usr/lib/ia64l64/"
#endif

/* It is illegal to have relocations in shared segments on AIX.
   Pretend flag_pic is always set.  */
#undef	TARGET_ASM_SELECT_SECTION
#define TARGET_ASM_SELECT_SECTION  ia64_rwreloc_select_section
#undef	TARGET_ASM_UNIQUE_SECTION
#define TARGET_ASM_UNIQUE_SECTION  ia64_rwreloc_unique_section
#undef	TARGET_ASM_SELECT_RTX_SECTION
#define TARGET_ASM_SELECT_RTX_SECTION  ia64_rwreloc_select_rtx_section

/* Override ia64/sysv4.h setting with that used by AIX5.  */
#undef WCHAR_TYPE
#ifdef __64BIT__
#define WCHAR_TYPE "unsigned int"
#else
#define WCHAR_TYPE "unsigned short"
#endif

/* Define the `__builtin_va_list' type for AIX.  Use char* b/c that's what the
   system headers expect.  */
#define BUILD_VA_LIST_TYPE(VALIST) \
  (VALIST) = build_pointer_type(char_type_node)

/* End of aix.h */
