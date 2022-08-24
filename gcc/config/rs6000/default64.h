/* Definitions of target machine for GNU compiler,
   for 64 bit powerpc linux defaulting to -m64.
   Copyright (C) 2003-2022 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#define RS6000_CPU(NAME, CPU, FLAGS)
#include "rs6000-cpus.def"
#undef RS6000_CPU

#if (TARGET_DEFAULT & MASK_LITTLE_ENDIAN)
#undef TARGET_DEFAULT
#define TARGET_DEFAULT (ISA_2_7_MASKS_SERVER | MASK_POWERPC64 | MASK_64BIT \
			| MASK_LITTLE_ENDIAN)
#undef ASM_DEFAULT_SPEC
#define ASM_DEFAULT_SPEC "-mpower8"
#else
#undef TARGET_DEFAULT
#define TARGET_DEFAULT (OPTION_MASK_PPC_GFXOPT | OPTION_MASK_PPC_GPOPT \
			| OPTION_MASK_MFCRF | MASK_POWERPC64 | MASK_64BIT)
#undef ASM_DEFAULT_SPEC
#define ASM_DEFAULT_SPEC "-mpower4"
#endif
