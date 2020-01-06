/* Configuration file for ARM GNU/Linux EABI targets.
   Copyright (C) 2004-2020 Free Software Foundation, Inc.
   Contributed by CodeSourcery, LLC   

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

/* On EABI GNU/Linux, we want both the BPABI builtins and the
   GNU/Linux builtins.  */
#undef  TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS() 		\
  do 						\
    {						\
      TARGET_BPABI_CPP_BUILTINS();		\
      GNU_USER_TARGET_OS_CPP_BUILTINS();	\
      ANDROID_TARGET_OS_CPP_BUILTINS();		\
    }						\
  while (false)

#define EXTRA_TARGET_D_OS_VERSIONS()		\
  ANDROID_TARGET_D_OS_VERSIONS();

/* We default to a soft-float ABI so that binaries can run on all
   target hardware.  If you override this to use the hard-float ABI then
   change the setting of GLIBC_DYNAMIC_LINKER_DEFAULT as well.  */
#undef  TARGET_DEFAULT_FLOAT_ABI
#define TARGET_DEFAULT_FLOAT_ABI ARM_FLOAT_ABI_SOFT

/* We default to the "aapcs-linux" ABI so that enums are int-sized by
   default.  */
#undef  ARM_DEFAULT_ABI
#define ARM_DEFAULT_ABI ARM_ABI_AAPCS_LINUX

/* TARGET_BIG_ENDIAN_DEFAULT is set in
   config.gcc for big endian configurations.  */
#undef  TARGET_LINKER_EMULATION
#if TARGET_BIG_ENDIAN_DEFAULT
#define TARGET_LINKER_EMULATION "armelfb_linux_eabi"
#else
#define TARGET_LINKER_EMULATION "armelf_linux_eabi"
#endif

#undef  SUBTARGET_EXTRA_LINK_SPEC
#define SUBTARGET_EXTRA_LINK_SPEC " -m " TARGET_LINKER_EMULATION

/* GNU/Linux on ARM currently supports three dynamic linkers:
   - ld-linux.so.2 - for the legacy ABI
   - ld-linux.so.3 - for the EABI-derived soft-float ABI
   - ld-linux-armhf.so.3 - for the EABI-derived hard-float ABI.
   All the dynamic linkers live in /lib.
   We default to soft-float, but this can be overridden by changing both
   GLIBC_DYNAMIC_LINKER_DEFAULT and TARGET_DEFAULT_FLOAT_ABI.  */

#undef  GLIBC_DYNAMIC_LINKER
#define GLIBC_DYNAMIC_LINKER_SOFT_FLOAT "/lib/ld-linux.so.3"
#define GLIBC_DYNAMIC_LINKER_HARD_FLOAT "/lib/ld-linux-armhf.so.3"
#define GLIBC_DYNAMIC_LINKER_DEFAULT GLIBC_DYNAMIC_LINKER_SOFT_FLOAT

#define GLIBC_DYNAMIC_LINKER \
   "%{mfloat-abi=hard:" GLIBC_DYNAMIC_LINKER_HARD_FLOAT "} \
    %{mfloat-abi=soft*:" GLIBC_DYNAMIC_LINKER_SOFT_FLOAT "} \
    %{!mfloat-abi=*:" GLIBC_DYNAMIC_LINKER_DEFAULT "}"

/* For ARM musl currently supports four dynamic linkers:
   - ld-musl-arm.so.1 - for the EABI-derived soft-float ABI
   - ld-musl-armhf.so.1 - for the EABI-derived hard-float ABI
   - ld-musl-armeb.so.1 - for the EABI-derived soft-float ABI, EB
   - ld-musl-armebhf.so.1 - for the EABI-derived hard-float ABI, EB
   musl does not support the legacy OABI mode.
   All the dynamic linkers live in /lib.
   We default to soft-float, EL. */
#undef  MUSL_DYNAMIC_LINKER
#if TARGET_BIG_ENDIAN_DEFAULT
#define MUSL_DYNAMIC_LINKER_E "%{mlittle-endian:;:eb}"
#else
#define MUSL_DYNAMIC_LINKER_E "%{mbig-endian:eb}"
#endif
#define MUSL_DYNAMIC_LINKER \
  "/lib/ld-musl-arm" MUSL_DYNAMIC_LINKER_E "%{mfloat-abi=hard:hf}%{mfdpic:-fdpic}.so.1"

/* At this point, bpabi.h will have clobbered LINK_SPEC.  We want to
   use the GNU/Linux version, not the generic BPABI version.  */
#undef  LINK_SPEC
#define LINK_SPEC EABI_LINK_SPEC					\
  LINUX_OR_ANDROID_LD (LINUX_TARGET_LINK_SPEC,				\
		       LINUX_TARGET_LINK_SPEC " " ANDROID_LINK_SPEC)

#undef  ASAN_CC1_SPEC
#define ASAN_CC1_SPEC "%{%:sanitize(address):-funwind-tables}"

#define FDPIC_CC1_SPEC ""

#undef  CC1_SPEC
#define CC1_SPEC							\
  LINUX_OR_ANDROID_CC (GNU_USER_TARGET_CC1_SPEC " " ASAN_CC1_SPEC " "	\
		       FDPIC_CC1_SPEC,					\
		       GNU_USER_TARGET_CC1_SPEC " " ASAN_CC1_SPEC " "	\
		       ANDROID_CC1_SPEC)

#define CC1PLUS_SPEC \
  LINUX_OR_ANDROID_CC ("", ANDROID_CC1PLUS_SPEC)

#undef  LIB_SPEC
#define LIB_SPEC							\
  LINUX_OR_ANDROID_LD (GNU_USER_TARGET_LIB_SPEC,			\
		    GNU_USER_TARGET_NO_PTHREADS_LIB_SPEC " " ANDROID_LIB_SPEC)

#undef	STARTFILE_SPEC
#define STARTFILE_SPEC \
  LINUX_OR_ANDROID_LD (GNU_USER_TARGET_STARTFILE_SPEC, ANDROID_STARTFILE_SPEC)

#undef	ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{Ofast|ffast-math|funsafe-math-optimizations:crtfastmath.o%s} "	\
  LINUX_OR_ANDROID_LD (GNU_USER_TARGET_ENDFILE_SPEC, ANDROID_ENDFILE_SPEC)

/* Use the default LIBGCC_SPEC, not the version in linux-elf.h, as we
   do not use -lfloat.  */
#undef LIBGCC_SPEC

/* Clear the instruction cache from `beg' to `end'.  This is
   implemented in lib1funcs.S, so ensure an error if this definition
   is used.  */
#undef  CLEAR_INSN_CACHE
#define CLEAR_INSN_CACHE(BEG, END) not_used

#define ARM_TARGET2_DWARF_FORMAT (DW_EH_PE_pcrel | DW_EH_PE_indirect)
