/* Definitions of target machine for GNU compiler.  MIPS version with
   GOFAST floating point library.
   Copyright (C) 1994, 1998 Free Software Foundation, Inc.

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

/* US Software GOFAST library support.  */
#define INIT_SUBTARGET_OPTABS INIT_GOFAST_OPTABS

#include "mips/mips.h"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dmips -DMIPSEB -DR3000 -D_mips -D_MIPSEB -D_R3000"

/* Use memcpy, et. al., rather than bcopy.  */
#define TARGET_MEM_FUNCTIONS

/* Don't assume anything about startfiles.  The linker script will load the
   appropriate startfiles.  */
#define STARTFILE_SPEC ""
