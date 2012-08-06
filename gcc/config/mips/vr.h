/* Definitions of target machine for GNU compiler.
   NEC VR Series Processors
   Copyright (c) 2002, 2004, 2005, 2007, 2008 Free Software Foundation, Inc.
   Contributed by Red Hat, Inc.

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

#define DEFAULT_VR_ARCH "march=4300"

#undef  MULTILIB_ABI_DEFAULT
#define MULTILIB_ABI_DEFAULT "mabi=eabi"

#undef MULTILIB_DEFAULTS
#define MULTILIB_DEFAULTS \
	{ MULTILIB_ENDIAN_DEFAULT,		\
	  MULTILIB_ABI_DEFAULT,			\
	  DEFAULT_VR_ARCH }

#undef  SUBTARGET_LINK_SPEC
#define SUBTARGET_LINK_SPEC \
  "%{mabi=n32:%{EB:--oformat=elf32-nbigmips} %{!EB:--oformat=elf32-nlittlemips}}\
   %{mabi=64:%{EB:--oformat=elf64-bigmips} %{!EB:--oformat=elf64-littlemips}}"

#undef  DRIVER_SELF_SPECS
#define DRIVER_SELF_SPECS \
	/* Enforce the default architecture.  This is mostly for	\
	   the assembler's benefit.  */					\
	"%{!march=*:%{!mfix-vr4120:%{!mfix-vr4130:"			\
	"-" DEFAULT_VR_ARCH "}}}",					\
									\
	/* Same idea for -mfix-vr4130.  */				\
	"%{mfix-vr4130:%{!march=*:-march=vr4130}}",			\
									\
	/* Infer the default float setting from -march.  */		\
	MIPS_ARCH_FLOAT_SPEC,						\
									\
	/* Make -mabi=eabi -mlong32 the default.  */			\
	"%{!mabi=*:-mabi=eabi}",					\
									\
	/* Remove -mgp32 if it is redundant.  */			\
	"%{mabi=32:%<mgp32}",						\
									\
	/* Configuration-independent MIPS rules.  */			\
	BASE_DRIVER_SELF_SPECS
