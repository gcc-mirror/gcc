/* Support for GCC on simulated PowerPC systems targeted to embedded ELF
   systems.
   Copyright (C) 1995, 1996 Free Software Foundation, Inc.
   Contributed by Cygnus Support.

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

#include "rs6000/eabi.h"

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (PowerPC Simulated)");

#undef CPP_PREDEFINES
#define CPP_PREDEFINES \
  "-DPPC -D__embedded__ -D__simulator__ -Asystem(embedded) -Asystem(simulator) -Acpu(powerpc) -Amachine(powerpc)"

/* Make the simulator the default */
#undef	LIB_DEFAULT_SPEC
#define LIB_DEFAULT_SPEC LIB_SIM_SPEC

#undef	STARTFILE_DEFAULT_SPEC
#define STARTFILE_DEFAULT_SPEC STARTFILE_SIM_SPEC

#undef	ENDFILE_DEFAULT_SPEC
#define ENDFILE_DEFAULT_SPEC ENDFILE_SIM_SPEC

#undef	LINK_START_DEFAULT_SPEC
#define LINK_START_DEFAULT_SPEC LINK_START_SIM_SPEC
