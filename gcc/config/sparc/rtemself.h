/* Definitions for rtems targeting a SPARC using a.out.
   Copyright (C) 1996, 1997, 2000 Free Software Foundation, Inc.
   Contributed by Joel Sherrill (joel@OARcorp.com).

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

#include "sparc/elf.h"

/* Specify predefined symbols in preprocessor.  */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dsparc -D__GCC_NEW_VARARGS__ -Drtems -D__rtems__ \
  -Asystem(rtems) -Acpu(sparc) -Amachine(sparc)"

/* Generate calls to memcpy, memcmp and memset.  */
#ifndef TARGET_MEM_FUNCTIONS
#define TARGET_MEM_FUNCTIONS
#endif

/* Get machine-independent configuration parameters for RTEMS.  */
#include <rtems.h>

/* end of sparc/rtemself.h */
