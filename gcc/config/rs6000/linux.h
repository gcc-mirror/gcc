/* Definitions of target machine for GNU compiler,
   for IBM RS/6000 running AIX version 3.1.
   Copyright (C) 1996, 1997, 1998 Free Software Foundation, Inc.
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
 "-DPPC -D__ELF__ -Dunix -Dlinux -Dpowerpc -Asystem(unix) -Asystem(linux) -Acpu(powerpc) -Amachine(powerpc)"

#undef LINK_SPEC
#define LINK_SPEC "-m elf32ppc %{shared:-shared} \
  %{!shared: \
    %{!static: \
      %{rdynamic:-export-dynamic} \
      %{!dynamic-linker:-dynamic-linker /lib/ld.so.1}} \
    %{static:-static}}"

#undef	LIB_DEFAULT_SPEC
#define LIB_DEFAULT_SPEC "%(lib_linux)"

#undef	STARTFILE_DEFAULT_SPEC
#define STARTFILE_DEFAULT_SPEC "%(startfile_linux)"

#undef	ENDFILE_DEFAULT_SPEC
#define ENDFILE_DEFAULT_SPEC "%(endfile_linux)"

#undef	LINK_START_DEFAULT_SPEC
#define LINK_START_DEFAULT_SPEC "%(link_start_linux)"

#undef	LINK_OS_DEFAULT_SPEC
#define LINK_OS_DEFAULT_SPEC "%(link_os_linux)"

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (PowerPC Linux)");

/* Define this macro as a C expression for the initializer of an
   array of string to tell the driver program which options are
   defaults for this target and thus do not need to be handled
   specially when using `MULTILIB_OPTIONS'.

   Do not define this macro if `MULTILIB_OPTIONS' is not defined in
   the target makefile fragment or if none of the options listed in
   `MULTILIB_OPTIONS' are set by default.  *Note Target Fragment::.  */

#undef	MULTILIB_DEFAULTS
#define	MULTILIB_DEFAULTS { "mbig", "mcall-linux" }

#undef DEFAULT_VTABLE_THUNKS
#ifndef USE_GNULIBC_1
#define DEFAULT_VTABLE_THUNKS 1
#endif
