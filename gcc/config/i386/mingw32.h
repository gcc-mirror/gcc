/* Operating system specific defines to be used when targeting GCC for
   hosting on Windows32, using GNU tools and the Windows32 API Library,
   as distinct from winnt.h, which is used to build GCC for use with a
   windows style library and tool set and uses the Microsoft tools.
   Copyright (C) 1997, 1998 Free Software Foundation, Inc.

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

/* Most of this is the same as for Cygwin32, except for changing some
   specs.  */

#include "i386/cygwin32.h"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Di386 -D_WIN32 -DWIN32 -D__WIN32__ \
  -D__MINGW32__ -DWINNT  -D_X86_=1 -D__STDC__=1\
  -D__stdcall=__attribute__((__stdcall__)) \
  -D__cdecl=__attribute__((__cdecl__)) \
  -Asystem(winnt) -Acpu(i386) -Amachine(i386)"

/* Specific a different directory for the standard include files.  */
#undef STANDARD_INCLUDE_DIR
#define STANDARD_INCLUDE_DIR "/usr/mingw32/include"

#define STANDARD_INCLUDE_COMPONENT "MINGW32"

/* For Windows applications, include more libraries, but always include
   kernel32.  */
#undef LIB_SPEC
#define LIB_SPEC \
"%{mwindows:-luser32 -lgdi32 -lcomdlg32} -lkernel32 -ladvapi32 -lshell32"

/* Include in the mingw32 libraries with libgcc */
#undef LIBGCC_SPEC
#define LIBGCC_SPEC "-lmingw32 -lgcc -lmoldname -lmsvcrt"

/* Specify a different entry point when linking a DLL */
#undef LINK_SPEC
#define LINK_SPEC \
"%{mwindows:--subsystem windows} %{mdll:--dll -e _DllMainCRTStartup@12}"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%{mdll:dllcrt2%O%s} %{!mdll:crt2%O%s}"

#define MATH_LIBRARY "-lmsvcrt"

/* Output STRING, a string representing a filename, to FILE.  We canonicalize
   it to be in MS-DOS format.  */
#define OUTPUT_QUOTED_STRING(FILE, STRING) \
do {						\
  char c;					\
						\
  putc ('\"', asm_file);			\
  if (STRING[1] == ':'				\
      && (STRING[2] == '/' || STRING[2] == '\\')) \
    {						\
      putc ('/', asm_file);			\
      putc ('/', asm_file);			\
      putc (*string, asm_file);			\
      string += 2;				\
    }						\
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

/* Dwarf2 exception information does not work on this system for some
   unknown reason, so turn it off.  */
#undef INCOMING_RETURN_ADDR_RTX
