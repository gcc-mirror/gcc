/* Definitions of target machine for GNU compiler, for HI-UX.
   Copyright (C) 1993 Free Software Foundation, Inc.

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

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT 0
#endif

#include "pa/pa.h"

/* Make GCC agree with types.h.  */
#undef SIZE_TYPE
#undef PTRDIFF_TYPE

#define SIZE_TYPE "unsigned int"
#define PTRDIFF_TYPE "int"

/* HPUX doesn't use any debugging format that GCC knows about.  */
#undef DBX_DEBUGGING_INFO

/* Like the default, except no -lg.  */
#define LIB_SPEC "%{!p:%{!pg:-lc}}%{p:-L/lib/libp/ -lc}%{pg:-L/lib/libp/ -lc}"

#undef CPP_SPEC
#if (TARGET_DEFAULT & 1) == 0
#define CPP_SPEC "%{msnake:-D_PA_RISC1_1}\
 %{mpa-risc-1-1:-D_PA_RISC1_1}"
#else
#define CPP_SPEC "%{!mpa-risc-1-0:%{!mnosnake:-D_PA_RISC1_1}}"
#endif

#undef CC1_SPEC
#define CC1_SPEC "-fwritable-strings %{pg:} %{p:}"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dhppa -DPWB -Dunix -D_HIUX_SOURCE -D__H3050R -D__H3050RX"

#undef LINK_SPEC
#define LINK_SPEC "-u main -a archive"
