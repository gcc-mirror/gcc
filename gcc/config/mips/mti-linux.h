/* Target macros for mips*-mti-linux* targets.
   Copyright (C) 2012-2025 Free Software Foundation, Inc.

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

/* This target is a multilib target, specify the sysroot paths.
   MIPS_SYSVERSION_SPEC defaults to 'r2' (mips32r2 or mips64r2) unless
   'r1' or 'r6' are specifically given so that mips32r3, mips32r5,
   mips64r3, and mips64r5 will all default to 'r2'.  See MULTILIB_MATCHES
   definition in t-mti-linux.  */

#define MIPS_SYSVERSION_SPEC \
    "%{mips32|mips64:r1;mips32r6|mips64r6:r6;:r2}%{mips16:-mips16}"

#undef SYSROOT_SUFFIX_SPEC
#define SYSROOT_SUFFIX_SPEC						\
    "/%{mmicromips:micro}mips%{mel|EL:el}-" MIPS_SYSVERSION_SPEC	\
    "%{msoft-float:-soft;:-hard}"					\
    "%{!mips32r6:%{!mips64r6:%{mnan=2008:-nan2008}}}%{muclibc:-uclibc}"

#define SYSROOT_HEADERS_SUFFIX_SPEC SYSROOT_SUFFIX_SPEC

#undef STARTFILE_PREFIX_SPEC
#define STARTFILE_PREFIX_SPEC                          \
  "%{mabi=32: /usr/local/lib/ /lib/ /usr/lib/}         \
   %{mabi=n32: /usr/local/lib32/ /lib32/ /usr/lib32/}  \
   %{mabi=64: /usr/local/lib64/ /lib64/ /usr/lib64/}"

#undef DRIVER_SELF_SPECS
#define DRIVER_SELF_SPECS						\
  /* Set the ISA for the default multilib.  */				\
  MIPS_DEFAULT_ISA_LEVEL_SPEC,						\
									\
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
  /* If no ABI option is specified, infer one from the ISA level	\
     or -mgp setting.  */						\
  "%{!mabi=*: %{" MIPS_32BIT_OPTION_SPEC ": -mabi=32;: -mabi=n32}}",	\
									\
  /* If no FP ABI option is specified, infer one from the		\
     ABI/ISA level unless there is a conflicting option.  */		\
  "%{!msoft-float: %{!msingle-float: %{!mfp*: %{!mmsa: %{mabi=32: %{"	\
  MIPS_FPXX_OPTION_SPEC ": -mfpxx}}}}}}",				\
									\
  /* Base SPECs.  */							\
  BASE_DRIVER_SELF_SPECS						\
									\
  /* Use the standard linux specs for everything else.  */		\
  LINUX_DRIVER_SELF_SPECS
