/* Definitions of target machine for GNU compiler.  Vxworks 29k version.
   Copyright (C) 1994, 1995 Free Software Foundation, Inc.

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

/* This file just exists to give specs for the 29k running on VxWorks.  */

#include "a29k/a29k-udi.h"

/* Names to predefine in the preprocessor for this target machine.  */

#undef  CPP_PREDEFINES
#define CPP_PREDEFINES "-D_AM29K -D_AM29000 -Acpu(a29k) -Amachine(a29k) -D__vxworks -D__vxworks_5"

/* VxWorks does all the library stuff itself.  */

#undef LIB_SPEC
#define LIB_SPEC ""

/* VxWorks provides the functionality of crt0.o and friends itself.  */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "crtbegin.o%s"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s"
