/* Target macros for mips*-elf targets that selected between o32 and o64
   based on the target architecture.
   Copyright (C) 1994-2025 Free Software Foundation, Inc.

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
  /* If no ABI option is specified, infer one from the ISA level	\
     or -mgp setting.  */						\
  "%{!mabi=*: %{" MIPS_32BIT_OPTION_SPEC ": -mabi=32;: -mabi=o64}}",	\
									\
  /* Remove a redundant -mfp64 for -mabi=o64; we want the !mfp64	\
     multilibs.  There's no need to check whether the architecture	\
     is 64-bit; cc1 will complain if it isn't.  */			\
  "%{mabi=o64: %<mfp64}",						\
									\
  /* Configuration-independent MIPS rules.*/				\
  BASE_DRIVER_SELF_SPECS

