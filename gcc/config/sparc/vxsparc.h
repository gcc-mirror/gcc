/* Definitions of target machine for GNU compiler.  Vxworks SPARC version.
   Copyright (C) 1994, 1996 Free Software Foundation, Inc.
   Contributed by David Henkel-Wallace (gumby@cygnus.com)

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


/* Specify what to link with.  */
/* VxWorks does all the library stuff itself.  */

#undef LIB_SPEC
#define LIB_SPEC ""

/* Provide required defaults for linker -e.  */
#undef LINK_SPEC
#define LINK_SPEC "%{!nostdlib:%{!r*:%{!e*:-e start}}}"

/* VxWorks provides the functionality of crt0.o and friends itself.  */
#undef STARTFILE_SPEC
#define STARTFILE_SPEC ""

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dsparc -Acpu=sparc -Amachine=sparc"

/* Note that we define CPU here even if the user has specified -ansi.
   This violates user namespace, but the VxWorks headers, and potentially
   user code, all explicitly rely upon the definition of CPU in order to get
   the proper processor information.  */
#undef CPP_SPEC
#define CPP_SPEC "%(cpp_cpu) -DCPU=SPARC"

#undef PTRDIFF_TYPE
#undef SIZE_TYPE
#undef WCHAR_TYPE
#undef WCHAR_TYPE_SIZE

#define PTRDIFF_TYPE "long int"
#define SIZE_TYPE "unsigned int"
#define WCHAR_TYPE "char"
#define WCHAR_TYPE_SIZE 8

/* US Software GOFAST library support.  */
#undef INIT_SUBTARGET_OPTABS
#define INIT_SUBTARGET_OPTABS INIT_GOFAST_OPTABS
