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

/* A C string constant that tells the GNU CC driver program options to pass to
   CPP.  It can also specify how to translate options you give to GNU CC into
   options for GNU CC to pass to the CPP.  */

/* If -ansi, we need to define _ANSI_C_SOURCE to get the right headers.  */
#undef CPP_SPEC
#define CPP_SPEC "\
%{mcpu=itanium:-D__itanium__} %{mbig-endian:-D__BIG_ENDIAN__} \
%{ansi:-D_ANSI_C_SOURCE} \
%{posix:-D_POSIX_SOURCE} \
%{cpp_cpu} \
-D__LONG_MAX__=9223372036854775807L"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "\
-D__ia64 -D__ia64__ -D_AIX -D_AIX64 -D_LONGLONG -Dunix \
-D__LP64__ -D__ELF__ -Asystem=unix -Asystem=aix -Acpu=ia64 -Amachine=ia64 \
-D__64BIT__ -D_LONG_LONG -D_IA64 -D__int128=__size128_t"

/* The GNU C++ standard library requires that these macros be defined.  */
#undef CPLUSPLUS_CPP_SPEC
#define CPLUSPLUS_CPP_SPEC                      \
  "-D_XOPEN_SOURCE=500                          \
   -D_XOPEN_SOURCE_EXTENDED=1                   \
   -D_LARGE_FILE_API                            \
   -D_ALL_SOURCE                                \
   -D__LONG_MAX__=9223372036854775807L          \
   %{cpp_cpu}"

/* ia64-specific options for gas */
#undef ASM_SPEC
#define ASM_SPEC "-x %{mconstant-gp} %{mauto-pic}"

/* Define this for shared library support.  */

#undef LINK_SPEC
#define LINK_SPEC "\
%{shared:-shared} \
%{!shared: \
  %{!static: \
    %{rdynamic:-export-dynamic} \
    %{!dynamic-linker:-dynamic-linker /usr/lib/ia64l64/libc.so.1}} \
    %{static:-static}}"

#define DONT_USE_BUILTIN_SETJMP
#define JMP_BUF_SIZE  85

/* Output any profiling code before the prologue.  */

#undef PROFILE_BEFORE_PROLOGUE
#define PROFILE_BEFORE_PROLOGUE 1

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

/* Override SELECT_SECTION and SELECT_RTX_SECTION from config/ia64/sysv4.h;  
   these definitions ignore flag_pic as if it were always set; 
   it is illegal to have relocations in shared segments on AIX.  */

/* A C statement or statements to switch to the appropriate
   section for output of DECL.  DECL is either a `VAR_DECL' node
   or a constant of some sort.  RELOC indicates whether forming
   the initial value of DECL requires link-time relocations.  */

#undef SELECT_SECTION
#define SELECT_SECTION(DECL,RELOC,ALIGN)				\
{									\
  if (TREE_CODE (DECL) == STRING_CST)					\
    {									\
      if (! flag_writable_strings)					\
	const_section ();						\
      else								\
	data_section ();						\
    }									\
  else if (TREE_CODE (DECL) == VAR_DECL)				\
    {									\
      if (XSTR (XEXP (DECL_RTL (DECL), 0), 0)[0]			\
	  == SDATA_NAME_FLAG_CHAR)					\
        sdata_section ();						\
      /* ??? We need the extra ! RELOC check, because the default is to \
	 only check RELOC if flag_pic is set, and we don't set flag_pic \
	 (yet?).  */							\
      else if (DECL_READONLY_SECTION (DECL, RELOC) && ! (RELOC))	\
	const_section ();						\
      else								\
	data_section ();						\
    }									\
  /* This could be a CONSTRUCTOR containing ADDR_EXPR of a VAR_DECL,	\
     in which case we can't put it in a shared library rodata.  */	\
  else if (RELOC)                                                       \
    data_section ();							\
  else									\
    const_section ();							\
}

/* Similarly for constant pool data.  */

extern unsigned int ia64_section_threshold;
#undef SELECT_RTX_SECTION
#define SELECT_RTX_SECTION(MODE, RTX, ALIGN)				\
{									\
  if (GET_MODE_SIZE (MODE) > 0						\
      && GET_MODE_SIZE (MODE) <= ia64_section_threshold)		\
    sdata_section ();							\
  else if (symbolic_operand ((RTX), (MODE)))		                \
    data_section ();							\
  else									\
    const_section ();							\
}

#undef UNIQUE_SECTION
#define UNIQUE_SECTION(DECL, RELOC)				\
  do								\
    {								\
      int len;							\
      int sec;							\
      const char *name;						\
      char *string;						\
      const char *prefix;					\
      static const char *const prefixes[/*4*/3][2] =		\
      {								\
	{ ".text.",   ".gnu.linkonce.t." },			\
	{ ".rodata.", ".gnu.linkonce.r." },			\
	{ ".data.",   ".gnu.linkonce.d." }			\
	/* Do not generate unique sections for uninitialised 	\
	   data since we do not have support for this in the    \
	   linker scripts yet...				\
        ,{ ".bss.",    ".gnu.linkonce.b." }  */			\
      };							\
      								\
      if (TREE_CODE (DECL) == FUNCTION_DECL)			\
	sec = 0;						\
  /*  else if (DECL_INITIAL (DECL) == 0				\
	       || DECL_INITIAL (DECL) == error_mark_node)	\
        sec =  3; */						\
      else if (DECL_READONLY_SECTION (DECL, RELOC) && ! (RELOC))\
	sec = 1;						\
      else							\
	sec = 2;						\
      								\
      name   = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (DECL));	\
      /* Strip off any encoding in name.  */			\
      STRIP_NAME_ENCODING (name, name);				\
      prefix = prefixes[sec][DECL_ONE_ONLY(DECL)];		\
      len    = strlen (name) + strlen (prefix);			\
      string = alloca (len + 1);				\
      								\
      sprintf (string, "%s%s", prefix, name);			\
      								\
      DECL_SECTION_NAME (DECL) = build_string (len, string);	\
    }								\
  while (0)

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
