/* Definitions of target machine for GNU compiler.  Vxworks m68k version.
   Copyright (C) 1994 Free Software Foundation, Inc.

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

/* This comment is here to see if it will keep Sun's cpp from dying.  */

#define CPP_PREDEFINES "-Dmc68000 -D__vxworks -D__vxworks_5 -Acpu(m68k) -Amachine(m68k)"

#include "m68k/m68k-none.h"
#include "aoutos.h"

#define DBX_DEBUGGING_INFO
#undef SDB_DEBUGGING_INFO

#undef WCHAR_TYPE
#define WCHAR_TYPE "short unsigned int"
#undef SIZE_TYPE
#define SIZE_TYPE "long unsigned int"

/* VxWorks does all the library stuff itself.  */

#define LIB_SPEC ""

/* Provide required defaults for linker -e. */
 
#define LINK_SPEC "%{!nostdlib:%{!r*:%{!e*:-e start}}}"

/* VxWorks provides the functionality of crt0.o and friends itself.  */

#define STARTFILE_SPEC ""

/* Every structure or union's size must be a multiple of 2 bytes.  */

#define STRUCTURE_SIZE_BOUNDARY 16

/* Allow folding division by zero.  */
#define REAL_INFINITY

/* GCC is the primary compiler for VxWorks, so we don't need this.  */
#undef PCC_STATIC_STRUCT_RETURN
