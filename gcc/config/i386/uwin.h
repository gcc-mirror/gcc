/* Operating system specific defines to be used when targeting GCC for
   hosting on U/WIN (Windows32), using GNU tools and the Windows32 API 
   Library, as distinct from winnt.h, which is used to build GCC for use 
   with a windows style library and tool set and uses the Microsoft tools.
   Copyright (C) 1999 Free Software Foundation, Inc.
   Contributed by Mumit Khan  <khan@xraylith.wisc.edu>.

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

/* Most of this is the same as for Cygwin32, except for changing some
   specs.  */

#include "i386/cygwin.h"

#define STANDARD_INCLUDE_COMPONENT "UWIN"
#define SYSTEM_INCLUDE_DIR "/usr/gnu/include"
#undef MD_STARTFILE_PREFIX
#define MD_STARTFILE_PREFIX "/usr/gnu/lib/"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-D_WIN32 -D__WIN32__ \
  -D_UWIN -DWINNT  -D_X86_=1 -D__STDC__=1 \
  -D__UWIN__ -D__MSVCRT__ \
  -D_STD_INCLUDE_DIR=mingw32 \
  -D__stdcall=__attribute__((__stdcall__)) \
  _D_stdcall=__attribute__((__stdcall__)) \
  -D__cdecl=__attribute__((__cdecl__)) \
  -D__declspec(x)=__attribute__((x)) \
  -Asystem=winnt"

#undef CPP_SPEC
#define CPP_SPEC "-remap %(cpp_cpu) %{posix:-D_POSIX_SOURCE} \
  -include /usr/include/astwin32.h \
  -idirafter /usr/gnu/include/mingw32"

/* For Windows applications, include more libraries, but always include
   kernel32.  */
#undef LIB_SPEC
#define LIB_SPEC \
  "%{pg:-lgmon} %{mwindows:-luser32 -lgdi32 -lcomdlg32} -lkernel32 -ladvapi32"

/* This is needed in g77spec.c for now. Will be removed in the future.  */
#define WIN32_UWIN_TARGET 1

/* Include in the mingw32 libraries with libgcc */
#undef LIBGCC_SPEC
#define LIBGCC_SPEC "-lgnuwin -lposix -lgcc -last -lmoldname -lmsvcrt"

/* Specify a different entry point when linking a DLL */
#undef LINK_SPEC
#define LINK_SPEC \
  "%{mwindows:--subsystem windows} %{mdll:--dll -e _DllMainCRTStartup@12} \
  %{!mdll:-u _main}"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%{mdll:dllcrt2%O%s} %{!mdll:crt2%O%s} %{pg:gcrt2%O%s}"

/* These are PE BFD bug workarounds. Should go away eventually.  */

/* Write the extra assembler code needed to declare a function
   properly.  If we are generating SDB debugging information, this
   will happen automatically, so we only need to handle other cases.  */
#undef ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)			\
  do									\
    {									\
      if (i386_pe_dllexport_name_p (NAME))				\
	i386_pe_record_exported_symbol (NAME, 0);			\
      /* UWIN binutils bug workaround.  */				\
      if (0 && write_symbols != SDB_DEBUG)				\
	i386_pe_declare_function_type (FILE, NAME, TREE_PUBLIC (DECL));	\
      ASM_OUTPUT_LABEL (FILE, NAME);					\
    }									\
  while (0)

#undef ASM_OUTPUT_EXTERNAL
#undef ASM_OUTPUT_EXTERNAL_LIBCALL

/* Override Cygwin's definition. This is necessary now due to the way
   Cygwin profiling code is written. Once "fixed", we can remove this.  */
#undef SUBTARGET_PROLOGUE

