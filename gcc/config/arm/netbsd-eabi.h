/* Definitions of target machine for GNU compiler, NetBSD/arm ELF version.
   Copyright (C) 2002-2024 Free Software Foundation, Inc.
   Contributed by Wasabi Systems, Inc.

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

/* Run-time Target Specification.  */
#undef MULTILIB_DEFAULTS
#define MULTILIB_DEFAULTS { "mabi=aapcs-linux" }

#define TARGET_LINKER_EABI_SUFFIX_SOFT \
  "%{!mabi=apcs-gnu:%{!mabi=atpcs:%{mfloat-abi=hard:_eabihf;:_eabi}}}"
#define TARGET_LINKER_EABI_SUFFIX_HARD \
  "%{!mabi=apcs-gnu:%{!mabi=atpcs:%{mfloat-abi=soft:_eabi;:_eabihf}}}"

#define TARGET_LINKER_EABI_SUFFIX			\
  (TARGET_DEFAULT_FLOAT_ABI == ARM_FLOAT_ABI_SOFT	\
   ? TARGET_LINKER_EABI_SUFFIX_SOFT			\
   : TARGET_LINKER_EABI_SUFFIX_HARD)

#define TARGET_LINKER_BIG_EMULATION "armelfb_nbsd%(linker_eabi_suffix)"
#define TARGET_LINKER_LITTLE_EMULATION "armelf_nbsd%(linker_eabi_suffix)"

/* TARGET_BIG_ENDIAN_DEFAULT is set in
   config.gcc for big endian configurations.  */
#undef  TARGET_LINKER_EMULATION
#if TARGET_BIG_ENDIAN_DEFAULT
#define TARGET_LINKER_EMULATION TARGET_LINKER_BIG_EMULATION
#else
#define TARGET_LINKER_EMULATION TARGET_LINKER_LITTLE_EMULATION
#endif

#undef ARM_DEFAULT_ABI
#define ARM_DEFAULT_ABI ARM_ABI_AAPCS_LINUX

#undef ARM_UNWIND_INFO
#define ARM_UNWIND_INFO 0
#undef DWARF2_UNWIND_INFO
#define DWARF2_UNWIND_INFO 1

#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
      if (TARGET_AAPCS_BASED)			\
	TARGET_BPABI_CPP_BUILTINS();		\
      NETBSD_OS_CPP_BUILTINS_ELF();		\
      if (DWARF2_UNWIND_INFO)			\
	builtin_define ("__ARM_DWARF_EH__");	\
    }						\
  while (0)

#undef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC NETBSD_CPP_SPEC

/*
 * Override AAPCS types to remain compatible the existing NetBSD types.
 */
#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef SIZE_TYPE
#define SIZE_TYPE "long unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "long int"

#undef SUBTARGET_EXTRA_ASM_SPEC
#define SUBTARGET_EXTRA_ASM_SPEC		\
  "%{mabi=apcs-gnu|mabi=atpcs:-meabi=gnu} "	\
  "%{fpic|fpie:-k} "				\
  "%{fPIC|fPIE:-k}"

#undef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS						\
  { "subtarget_extra_asm_spec",	SUBTARGET_EXTRA_ASM_SPEC },		\
  { "linker_eabi_suffix",	TARGET_LINKER_EABI_SUFFIX },		\
  { "linker_emulation",		TARGET_LINKER_EMULATION },		\
  { "linker_big_emulation",	TARGET_LINKER_BIG_EMULATION },		\
  { "linker_little_emulation",	TARGET_LINKER_LITTLE_EMULATION },	\
  { "target_fix_v4bx_spec",	TARGET_FIX_V4BX_SPEC },			\
  NETBSD_SUBTARGET_EXTRA_SPECS

#define NETBSD_ENTRY_POINT "__start"

#undef LINK_SPEC
#define LINK_SPEC						\
  "-X %{mbig-endian:-EB -m %(linker_big_emulation)} "		\
  "%{mlittle-endian:-EL -m %(linker_liitle_emulation)} "	\
  "%{!mbig-endian:%{!mlittle-endian:-m %(linker_emulation)}} "	\
  "%(target_fix_v4bx_spec) %(netbsd_link_spec)"
