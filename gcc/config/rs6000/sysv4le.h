/* Target definitions for GCC for a little endian PowerPC
   running System V.4
   Copyright (C) 1995-2014 Free Software Foundation, Inc.
   Contributed by Cygnus Support.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#undef  TARGET_DEFAULT
#define TARGET_DEFAULT MASK_LITTLE_ENDIAN

#undef	DEFAULT_ASM_ENDIAN
#define	DEFAULT_ASM_ENDIAN " -mlittle"

#undef	LINK_TARGET_SPEC
#define	LINK_TARGET_SPEC \
  ENDIAN_SELECT(" --oformat elf32-powerpc", "", "")

#undef	MULTILIB_DEFAULTS
#define	MULTILIB_DEFAULTS { "mlittle", "mcall-sysv" }

/* Little-endian PowerPC64 Linux uses the ELF v2 ABI by default.  */
#define LINUX64_DEFAULT_ABI_ELFv2

