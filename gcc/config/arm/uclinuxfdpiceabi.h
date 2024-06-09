/* Configuration file for ARM GNU/Linux FDPIC EABI targets.
   Copyright (C) 2018-2024 Free Software Foundation, Inc.
   Contributed by STMicroelectronics.

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

/* On uClibc EABI GNU/Linux, we want to force -mfdpic by default,
   which also means we produce PIE code by default.  */
#undef FDPIC_CC1_SPEC
#define FDPIC_CC1_SPEC \
  "%{!mno-fdpic:-mfdpic %{!fno-PIE:%{!fpie:%{!fPIC:%{!fpic: -fPIE}}}}}"

/* Add --fdpic assembler flag by default.  */
#undef TARGET_FDPIC_ASM_SPEC
#define TARGET_FDPIC_ASM_SPEC "%{!mno-fdpic: --fdpic}"

/* TARGET_BIG_ENDIAN_DEFAULT is set in
   config.gcc for big endian configurations.  */
#if TARGET_BIG_ENDIAN_DEFAULT
#define TARGET_FDPIC_LINKER_EMULATION "armelfb_linux_fdpiceabi"
#else
#define TARGET_FDPIC_LINKER_EMULATION "armelf_linux_fdpiceabi"
#endif

/* Unless we generate a shared library or a relocatable object, we
   force -pie.  */
/* -static is not supported, because we have to define the
   dynamic-linker, as we have some relocations to resolve at load
   time. We do not generate an error in case the user explictly passes
   the -dynamic-linker option to the linker.  */
#undef  SUBTARGET_EXTRA_LINK_SPEC
#define SUBTARGET_EXTRA_LINK_SPEC			    \
  "%{!mno-fdpic: -m " TARGET_FDPIC_LINKER_EMULATION	    \
   "%{!shared:%{!r: -pie}} }"				    \
  "%{mno-fdpic: -m " TARGET_LINKER_EMULATION "}"	    \
  "%{!r:%{!mno-fdpic: -z now}}"

#undef	STARTFILE_SPEC
#define STARTFILE_SPEC "%{!mno-fdpic:%{!shared:crtreloc.o%s}} " \
  LINUX_OR_ANDROID_LD (GNU_USER_TARGET_STARTFILE_SPEC, ANDROID_STARTFILE_SPEC)
