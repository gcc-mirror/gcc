/* Operating system specific defines to be used when targeting GCC for
   hosting on Windows32, using GNU tools and the Windows32 API Library,
   as distinct from winnt.h, which is used to build GCC for use with a
   windows style library and tool set and uses the Microsoft tools.
   Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation, Inc.

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
Boston, MA 02111-1307, USA. */

/* Most of this is the same as for cygwin, except for changing some
   specs.  */

#include "i386/cygwin.h"

/* Please keep changes to CPP_PREDEFINES in sync with i386/crtdll. The
   only difference between the two should be __MSVCRT__ needed to 
   distinguish MSVC from CRTDLL runtime in mingw headers. */
#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-D_WIN32 -DWIN32 \
  -D__MINGW32__=0.2 -D__MSVCRT__ -DWINNT -D_X86_=1 \
  -Asystem(winnt)"

/* Specific a different directory for the standard include files.  */
#undef STANDARD_INCLUDE_DIR
#define STANDARD_INCLUDE_DIR "/usr/local/i386-mingw32/include"

#define STANDARD_INCLUDE_COMPONENT "MINGW32"

#undef CPP_SPEC
#define CPP_SPEC \
  "-remap %(cpp_cpu) %{posix:-D_POSIX_SOURCE} %{mthreads:-D_MT} \
  -D__stdcall=__attribute__((__stdcall__)) \
  -D__cdecl=__attribute__((__cdecl__)) \
  %{!ansi:-D_stdcall=__attribute__((__stdcall__)) \
    -D_cdecl=__attribute__((__cdecl__))} \
  -D__declspec(x)=__attribute__((x))"


/* For Windows applications, include more libraries, but always include
   kernel32.  */
#undef LIB_SPEC
#define LIB_SPEC "%{pg:-lgmon} %{mwindows:-lgdi32 -lcomdlg32} \
                  -luser32 -lkernel32 -ladvapi32 -lshell32"

/* Include in the mingw32 libraries with libgcc */
#undef LIBGCC_SPEC
#define LIBGCC_SPEC \
  "%{mthreads:-lmingwthrd} -lmingw32 -lgcc -lmoldname -lmsvcrt"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%{mdll:dllcrt2%O%s} %{!mdll:crt2%O%s} %{pg:gcrt2%O%s}"

/* MS runtime does not need a separate math library. */
#define MATH_LIBRARY ""

/* Output STRING, a string representing a filename, to FILE.  We canonicalize
   it to be in MS-DOS format.  */
#define OUTPUT_QUOTED_STRING(FILE, STRING) \
do {						\
  char c;					\
						\
  putc ('\"', asm_file);			\
						\
  while ((c = *string++) != 0)			\
    {						\
      if (c == '\\')				\
	c = '/';				\
						\
      if (c == '\"')				\
	putc ('\\', asm_file);			\
      putc (c, asm_file);			\
    }						\
						\
  putc ('\"', asm_file);			\
} while (0)

/* Override Cygwin's definition. This is necessary now due to the way
   Cygwin profiling code is written. Once "fixed", we can remove this.  */
#undef SUBTARGET_PROLOGUE

