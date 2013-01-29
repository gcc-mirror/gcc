/* Definitions for ARM running ucLinux using ELF
   Copyright (C) 1999-2013 Free Software Foundation, Inc.
   Contributed by Philip Blundell <pb@nexus.co.uk>

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

/* We don't want a PLT.  */
#undef  NEED_PLT_RELOC
#define NEED_PLT_RELOC 0

#undef  TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_SINGLE_PIC_BASE)

/* NOTE: The remaining definitions in this file are needed because uclinux
   does not use config/linux.h.  */

/* Add GNU/Linux builtins.  */
#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS() 		\
  do 						\
    {						\
      builtin_define ("__uClinux__");		\
      builtin_define ("__gnu_linux__");         \
      builtin_define_std ("linux");             \
      builtin_define_std ("unix");              \
      builtin_assert ("system=linux");          \
      builtin_assert ("system=unix");           \
      builtin_assert ("system=posix");          \
    }						\
  while (false)

/* Do not assume anything about header files.  */
#define NO_IMPLICIT_EXTERN_C

/* The GNU C++ standard library requires that these macros be defined.  */
#undef CPLUSPLUS_CPP_SPEC
#define CPLUSPLUS_CPP_SPEC "-D_GNU_SOURCE %(cpp)"

#undef SUBTARGET_EXTRA_LINK_SPEC
#define SUBTARGET_EXTRA_LINK_SPEC " -m armelf_linux"

/* Now we define the strings used to build the spec file.  */
#undef  STARTFILE_SPEC
#define STARTFILE_SPEC	"crt1%O%s crti%O%s crtbegin%O%s"

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC	"crtend%O%s crtn%O%s"

#undef  CC1_SPEC
#define CC1_SPEC "%{profile:-p}"

#undef LINK_GCC_C_SEQUENCE_SPEC
#define LINK_GCC_C_SEQUENCE_SPEC \
  "%{static:--start-group} %G %L %{static:--end-group}%{!static:%G}"

/* Use --as-needed -lgcc_s for eh support.  */
#ifdef HAVE_LD_AS_NEEDED
#define USE_LD_AS_NEEDED 1
#endif

#undef LINK_SPEC
#define LINK_SPEC "%{mbig-endian:-EB} %{mlittle-endian:-EL} -X -elf2flt"

#undef  LIB_SPEC
#define LIB_SPEC \
  "%{pthread:-lpthread} \
   %{shared:-lc} \
   %{!shared:%{profile:-lc_p}%{!profile:-lc}}"

#define TARGET_DEFAULT_WORD_RELOCATIONS 1
