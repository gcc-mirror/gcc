/* Operating system specific defines to be used when targeting GCC for
   hosting on Windows NT 3.x, using the Cygnus API 

   This is different to the winnt.h file, since that is used
   to build GCC for use with a windows style library and tool
   set, winnt.h uses the Microsoft tools to do that.

   Copyright (C) 1996 Free Software Foundation, Inc.

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


/* Ugly hack */
#include "rs6000/win-nt.h"


#ifdef CPP_PREDEFINES
#undef CPP_PREDEFINES
#endif

#define	CPP_PREDEFINES "-D_WIN32 -DWINNT -D__CYGWIN32__ -DPOSIX \
  -D_POWER -D_ARCH_PPC -D__PPC__ -Asystem(winnt) -Acpu(powerpc) -Amachine(powerpc)"

/* We have to dynamic link to get to the system dlls,
   and I've put all of libc and libm and the unix stuff into
   cygwin.dll, the import library is called 'libcygwin.a' */

#undef LIB_SPEC
#define LIB_SPEC "-lcygwin"


#undef	LINK_SPEC
#define	LINK_SPEC "%{v:-V}"


#undef STARTFILE_SPEC
#define STARTFILE_SPEC "crti%O%s %{!:crt0%O%s}"

#undef	ENDFILE_SPEC
#define	ENDFILE_SPEC "crtn%O%s"

#define PTRDIFF_TYPE "int"
#define WCHAR_UNSIGNED 1
#define WCHAR_TYPE_SIZE 16
#define WCHAR_TYPE "short unsigned int"

#define DBX_DEBUGGING_INFO 
#undef SDB_DEBUGGING_INFO 
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG
