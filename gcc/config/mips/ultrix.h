/* Definitions of target machine for GNU compiler.  DECstation (ultrix) version.
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

#define DECSTATION

#ifndef CPP_PREDEFINES
#define CPP_PREDEFINES "\
-D__ANSI_COMPAT -DMIPSEL -DR3000 -DSYSTYPE_BSD -D_SYSTYPE_BSD \
-Dbsd4_2 -Dhost_mips -Dmips -Dultrix -Dunix \
-Asystem(unix) -Asystem(bsd) -Acpu(mips) -Amachine(mips)"
#endif

#ifndef LIB_SPEC
#define LIB_SPEC "%{p:-lprof1} %{pg:-lprof1} -lc"
#endif

#ifndef STARTFILE_SPEC
#define STARTFILE_SPEC "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}"
#endif

#ifndef MACHINE_TYPE
#define MACHINE_TYPE "DECstation running ultrix"
#endif

/* Use atexit for static constructors/destructors, instead of defining
   our own exit function.  Ultrix 4.x has this, 3.x probably does not.  */
#define HAVE_ATEXIT

/* Generate calls to memcpy, etc., not bcopy, etc.  */
#define TARGET_MEM_FUNCTIONS

#include "mips/mips.h"
