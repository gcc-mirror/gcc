/* Definitions for AArch64 running NetBSD
   Copyright (C) 2016-2025 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_AARCH64_NETBSD_H
#define GCC_AARCH64_NETBSD_H

#define TARGET_LINKER_BIG_EMULATION "aarch64nbsdb"
#define TARGET_LINKER_LITTLE_EMULATION "aarch64nbsd"

#if TARGET_BIG_ENDIAN_DEFAULT
#define TARGET_LINKER_EMULATION  TARGET_LINKER_BIG_EMULATION
#else
#define TARGET_LINKER_EMULATION  TARGET_LINKER_LITTLE_EMULATION
#endif

#undef  SUBTARGET_EXTRA_LINK_SPEC
#define SUBTARGET_EXTRA_LINK_SPEC " -m" TARGET_LINKER_EMULATION

#define NETBSD_ENTRY_POINT "__start"

#define NETBSD_TARGET_LINK_SPEC  "%{h*} "				\
  "-X %{mbig-endian:-EB -m " TARGET_LINKER_BIG_EMULATION "} "		\
  "%{mlittle-endian:-EL -m " TARGET_LINKER_LITTLE_EMULATION "} "	\
  "%(netbsd_link_spec)"


#ifndef CC1_SPEC
# define CC1_SPEC AARCH64_ERRATA_COMPILE_SPEC
#endif

#ifndef CC1PLUS_SPEC
# define CC1PLUS_SPEC AARCH64_ERRATA_COMPILE_SPEC
#endif

#undef  LINK_SPEC
#define LINK_SPEC NETBSD_LINK_SPEC_ELF		\
		  NETBSD_TARGET_LINK_SPEC	\
		  AARCH64_ERRATA_LINK_SPEC

#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
      NETBSD_OS_CPP_BUILTINS_ELF();		\
    }						\
  while (0)

#undef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC NETBSD_CPP_SPEC

#undef EXTRA_SPECS
#define EXTRA_SPECS \
  { "asm_cpu_spec",             ASM_CPU_SPEC }, \
  NETBSD_SUBTARGET_EXTRA_SPECS

#endif  /* GCC_AARCH64_NETBSD_H */
