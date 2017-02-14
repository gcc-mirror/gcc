/* Definitions of target machine for GNU compiler.
   NEC VR Series Processors
   Copyright (C) 2002-2017 Free Software Foundation, Inc.
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

#define DEFAULT_VR_ARCH "mfix-vr4130"
#undef MULTILIB_DEFAULTS
#define MULTILIB_DEFAULTS \
	{ MULTILIB_ENDIAN_DEFAULT,		\
	  MULTILIB_ABI_DEFAULT,			\
	  DEFAULT_VR_ARCH }

#undef DRIVER_SELF_SPECS
#define DRIVER_SELF_SPECS \
	/* Enforce the default architecture.  This is mostly for	\
	   the assembler's benefit.  */					\
	"%{!march=*:%{!mfix-vr4120:%{!mfix-vr4130:"			\
	"-" DEFAULT_VR_ARCH "}}}",					\
									\
	/* Make -mfix-vr4120 imply -march=vr4120.  This cuts down	\
	   on command-line tautology and makes it easier for t-vr to	\
	   provide a -mfix-vr4120 multilib.  */				\
	"%{mfix-vr4120:%{!march=*:-march=vr4120}}",			\
									\
	/* Same idea for -mfix-vr4130.  */				\
	"%{mfix-vr4130:%{!march=*:-march=vr4130}}",			\
									\
	/* Infer the default float setting from -march.  */		\
	MIPS_ARCH_FLOAT_SPEC,						\
									\
	/* Make -mabi=eabi -mlong32 the default.  */			\
	"%{!mabi=*:-mabi=eabi %{!mlong*:-mlong32}}",			\
									\
	/* Make sure -mlong64 multilibs are chosen when	64-bit longs	\
	   are needed.  */						\
	"%{mabi=eabi:%{!mlong*:%{!mgp32:-mlong64}}}",			\
									\
	/* Remove -mgp32 if it is redundant.  */			\
	"%{mabi=32:%<mgp32}",						\
									\
	/* Configuration-independent MIPS rules.  */			\
	BASE_DRIVER_SELF_SPECS
