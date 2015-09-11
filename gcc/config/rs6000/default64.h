/* Definitions of target machine for GNU compiler,
   for 64 bit powerpc linux defaulting to -m64.
   Copyright (C) 2003-2015 Free Software Foundation, Inc.

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
#define TARGET_DEFAULT (ISA_2_7_MASKS_SERVER | MASK_POWERPC64 | MASK_64BIT | MASK_LITTLE_ENDIAN)
#else
#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_PPC_GFXOPT | MASK_PPC_GPOPT | MASK_MFCRF | MASK_POWERPC64 | MASK_64BIT)
#endif
