/* Configuration file for ARM GNU/Linux EABI targets.
   Copyright (C) 2004, 2005, 2006, 2007, 2009, 2010, 2011
   Free Software Foundation, Inc.
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
      LINUX_TARGET_OS_CPP_BUILTINS();		\
      ANDROID_TARGET_OS_CPP_BUILTINS();		\
    }						\
  while (false)

/* We default to a soft-float ABI so that binaries can run on all
   target hardware.  */
#undef  TARGET_DEFAULT_FLOAT_ABI
#define TARGET_DEFAULT_FLOAT_ABI ARM_FLOAT_ABI_SOFT

/* We default to the "aapcs-linux" ABI so that enums are int-sized by
   default.  */
#undef  ARM_DEFAULT_ABI
#define ARM_DEFAULT_ABI ARM_ABI_AAPCS_LINUX

/* Default to armv5t so that thumb shared libraries work.
   The ARM10TDMI core is the default for armv5t, so set
   SUBTARGET_CPU_DEFAULT to achieve this.  */
#undef  SUBTARGET_CPU_DEFAULT
#define SUBTARGET_CPU_DEFAULT TARGET_CPU_arm10tdmi

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

/* Use ld-linux.so.3 so that it will be possible to run "classic"
   GNU/Linux binaries on an EABI system.  */
#undef  GLIBC_DYNAMIC_LINKER
#define GLIBC_DYNAMIC_LINKER "/lib/ld-linux.so.3"

/* At this point, bpabi.h will have clobbered LINK_SPEC.  We want to
   use the GNU/Linux version, not the generic BPABI version.  */
#undef  LINK_SPEC
#define LINK_SPEC BE8_LINK_SPEC						\
  LINUX_OR_ANDROID_LD (LINUX_TARGET_LINK_SPEC,				\
		       LINUX_TARGET_LINK_SPEC " " ANDROID_LINK_SPEC)

#undef  CC1_SPEC
#define CC1_SPEC							\
  LINUX_OR_ANDROID_CC (GNU_USER_TARGET_CC1_SPEC,			\
		       GNU_USER_TARGET_CC1_SPEC " " ANDROID_CC1_SPEC)

#define CC1PLUS_SPEC \
  LINUX_OR_ANDROID_CC ("", ANDROID_CC1PLUS_SPEC)

#undef  LIB_SPEC
#define LIB_SPEC							\
  LINUX_OR_ANDROID_LD (GNU_USER_TARGET_LIB_SPEC,			\
		       GNU_USER_TARGET_LIB_SPEC " " ANDROID_LIB_SPEC)

#undef	STARTFILE_SPEC
#define STARTFILE_SPEC \
  LINUX_OR_ANDROID_LD (GNU_USER_TARGET_STARTFILE_SPEC, ANDROID_STARTFILE_SPEC)

#undef	ENDFILE_SPEC
#define ENDFILE_SPEC \
  LINUX_OR_ANDROID_LD (GNU_USER_TARGET_ENDFILE_SPEC, ANDROID_ENDFILE_SPEC)

/* Use the default LIBGCC_SPEC, not the version in linux-elf.h, as we
   do not use -lfloat.  */
#undef LIBGCC_SPEC

/* Clear the instruction cache from `beg' to `end'.  This is
   implemented in lib1funcs.asm, so ensure an error if this definition
   is used.  */
#undef  CLEAR_INSN_CACHE
#define CLEAR_INSN_CACHE(BEG, END) not_used
