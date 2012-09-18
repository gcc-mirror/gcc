/* Target macros for mips*-mti-linux* targets.
   Copyright (C) 2012
   Free Software Foundation, Inc.

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

/* This target is a multilib target, specify the sysroot paths.  */
#undef SYSROOT_SUFFIX_SPEC
#define SYSROOT_SUFFIX_SPEC \
    "%{mips32:/mips32}%{mips64:/mips64}%{mips64r2:/mips64r2}%{msoft-float:/sof}%{mel|EL:/el}%{mabi=64:/64}%{mabi=n32:/n32}"

#undef DRIVER_SELF_SPECS
#define DRIVER_SELF_SPECS						\
  /* Make sure a -mips option is present.  This helps us to pick	\
     the right multilib, and also makes the later specs easier		\
     to write.  */							\
  MIPS_ISA_LEVEL_SPEC,							\
									\
  /* Infer the default float setting from -march.  */			\
  MIPS_ARCH_FLOAT_SPEC,							\
									\
  /* Infer the -msynci setting from -march if not explicitly set.  */	\
  MIPS_ISA_SYNCI_SPEC,							\
									\
  /* Base SPECs.  */							\
  BASE_DRIVER_SELF_SPECS						\
									\
  /* Use the standard linux specs for everything else.  */		\
  LINUX64_DRIVER_SELF_SPECS
