/* Definitions of target machine for GNU compiler, for SPARClite w/o FPU.
   Copyright (C) 1993 Free Software Foundation, Inc.
   Contributed by Jim Wilson (wilson@cygnus.com).

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

#include "sparc/sparc.h"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dsparc -Dsparclite -Acpu(sparc) -Amachine(sparc)"

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (sparclite)");

/* Enable sparclite and epilogue options.  Do not enable the fpu.  */

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (1024+130)

/* US Software GOFAST library support.  */
#include "gofast.h"
#undef INIT_SUBTARGET_OPTABS
#define INIT_SUBTARGET_OPTABS INIT_GOFAST_OPTABS
