/* Definitions of target machine for GNU compiler, for HP-UX.
   Copyright (C) 1991 Free Software Foundation, Inc.

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

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT 0
#endif

#include "pa/pa.h"

/* Make GCC agree with types.h.  */
#undef SIZE_TYPE
#undef PTRDIFF_TYPE

#define SIZE_TYPE "unsigned int"
#define PTRDIFF_TYPE "int"

/* HPUX doesn't use any debugging format that GCC knows about.  */
#undef DBX_DEBUGGING_INFO
#undef DEFAULT_GDB_EXTENSIONS
#define DEFAULT_GDB_EXTENSIONS 0

/* Like the default, except no -lg.  */
#define LIB_SPEC "%{!p:%{!pg:-lc}}%{p: -L/lib/libp/ -lc}%{pg: -L/lib/libp/ -lc}"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dhppa -Dhp9000s800 -D__hp9000s800 -Dhp9k8 -DPWB -Dhpux -Dunix -D_HPUX_SOURCE -Asystem(unix) -Asystem(hpux) -Acpu(hppa) -Amachine(hppa)"

#undef LINK_SPEC
#define LINK_SPEC "-u main %{static: -a archive}%{g*: -a archive}"

/* hpux8 and later have C++ compatable include files, so do not
   pretend they are `extern "C"'.  */
#define NO_IMPLICIT_EXTERN_C
