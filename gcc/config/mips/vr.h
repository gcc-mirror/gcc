/* Definitions of target machine for GNU compiler.
   NEC VR Series Processors
   Copyright (c) 2002 Free Software Foundation, Inc.
   Contributed by Red Hat, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#define MIPS_CPU_STRING_DEFAULT "vr4100"
#define MULTILIB_DEFAULTS \
	{ MULTILIB_ENDIAN_DEFAULT, MULTILIB_ABI_DEFAULT, "march=vr4100" }

/* Make sure that -mlong64 always appears on the command line when
   64-bit longs are needed.  Also make sure that -mgp32 doesn't appear
   if it is redundant.  */
#define DRIVER_SELF_SPECS \
	"%{mabi=eabi:%{!mlong*:%{!mgp32:-mlong64}}}", \
	"%{mabi=32:%{<mgp32}}"
