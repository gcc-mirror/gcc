/* Definitions of target machine for GNU compiler,
   for IBM RS/6000 running AIX version 3.1.
   Copyright (C) 1996 Free Software Foundation, Inc.
   Contributed by Michael Meissner (meissner@cygnus.com).

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

#include "rs6000/sysv4.h"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES \
 "-DPPC -Dunix -Dlinux -Dpowerpc -Asystem(unix) -Asystem(linux) -Acpu(powerpc) -Amachine(powerpc)"

/* For now, remove most of the System V.4 stuff */
#undef LINK_SPEC
#define LINK_SPEC ""

#undef	LIB_DEFAULT_SPEC
#define LIB_DEFAULT_SPEC LIB_LINUX_SPEC

#undef	STARTFILE_DEFAULT_SPEC
#define STARTFILE_DEFAULT_SPEC STARTFILE_LINUX_SPEC

#undef	ENDFILE_DEFAULT_SPEC
#define ENDFILE_DEFAULT_SPEC ENDFILE_LINUX_SPEC

#undef	LINK_START_DEFAULT_SPEC
#define LINK_START_DEFAULT_SPEC LINK_START_LINUX_SPEC

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (PowerPC Linux)");
