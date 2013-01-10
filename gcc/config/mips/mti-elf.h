/* Target macros for mips*-mti-elf targets.
   Copyright (C) 2012-2013 Free Software Foundation, Inc.

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
  /* Make sure that an endian option is always present.  This makes	\
     things like LINK_SPEC easier to write.  */				\
  "%{!EB:%{!EL:%(endian_spec)}}",					\
									\
  /* Configuration-independent MIPS rules.  */				\
  BASE_DRIVER_SELF_SPECS
