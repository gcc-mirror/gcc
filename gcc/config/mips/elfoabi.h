/* Target macros for mips*-elf targets that selected between o32 and o64
   based on the target architecture.
   Copyright (C) 1994, 1997, 1999, 2000, 2002, 2003, 2004
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#define DRIVER_SELF_SPECS						\
  /* Infer a -mips option from a -march option.  This makes		\
     -march pick the right multilib, and also makes the later		\
     specs easier to write.  */						\
  MIPS_ISA_LEVEL_SPEC,							\
									\
  /* If no architecture option is specified, force the default to	\
     be listed explicitly.  This too makes later specs easier to	\
     write.  */							       	\
  "%{" MIPS_ISA_LEVEL_OPTION_SPEC ":;: -" MULTILIB_ISA_DEFAULT "}",	\
									\
  /* If no ABI option is specified, infer one from the ISA level	\
     or -mgp setting.  */						\
  "%{!mabi=*: %{mips1|mips2|mips32*|mgp32: -mabi=32;: -mabi=o64}}",	\
									\
  /* Remove a redundant -mfp64 for -mabi=o64; we want the !mfp64	\
     multilibs.  There's no need to check whether the architecture	\
     is 64-bit; cc1 will complain if it isn't.  */			\
  "%{mabi=o64: %<mfp64}"
