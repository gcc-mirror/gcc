/* Target definitions for PowerPC64 running Darwin (Mac OS X) for a 64b host
   supporting a 32b multilib.
   Copyright (C) 2006-2023 Free Software Foundation, Inc.
   Contributed by Apple Computer Inc.

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
#define TARGET_DEFAULT (MASK_POWERPC64 | MASK_64BIT \
			| OPTION_MASK_MULTIPLE | OPTION_MASK_PPC_GFXOPT)

#undef DARWIN_ARCH_SPEC
#define DARWIN_ARCH_SPEC "%{m32:ppc;:ppc64}"

/* Actually, there's really only 970 as an active option.  */
#undef DARWIN_SUBARCH_SPEC
#define DARWIN_SUBARCH_SPEC DARWIN_ARCH_SPEC

#undef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS			\
  DARWIN_EXTRA_SPECS                            \
  { "darwin_arch", DARWIN_ARCH_SPEC },		\
  { "darwin_crt2", "" },			\
  { "darwin_subarch", DARWIN_SUBARCH_SPEC },
