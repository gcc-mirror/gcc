/* Operating system specific defines to be used when targeting GCC for
   hosting on Windows32, using GNU tools and the Windows32 API Library,
   as distinct from winnt.h, which is used to build GCC for use with a
   windows style library and tool set and uses the Microsoft tools.
   Copyright (C) 1997 Free Software Foundation, Inc.

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

/* For Windows applications, include more libraries, but always include
   kernel32.  */
#undef LIB_SPEC
#define LIB_SPEC "%{windows:-luser32 -lgdi32 -lcomdlg32} -lkernel32"

/* Include in the Windows32 API libraries with libgcc */
#undef LIBGCC_SPEC
#define LIBGCC_SPEC "-lmingw32 -lgcc -lmoldname -lcrtdll"

/* Specify a different entry point when linking a DLL */
#undef LINK_SPEC
#define LINK_SPEC "%{windows:--subsystem windows} %{dll:--dll -e _DllMainCRTStartup@12}"
