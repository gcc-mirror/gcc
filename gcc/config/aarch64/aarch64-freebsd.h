/* Definitions for AArch64 running FreeBSD
   Copyright (C) 2016 Free Software Foundation, Inc.

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

#ifndef GCC_AARCH64_FREEBSD_H
#define GCC_AARCH64_FREEBSD_H

#undef  SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC FBSD_CPP_SPEC

#if TARGET_BIG_ENDIAN_DEFAULT
#define TARGET_LINKER_EMULATION  "aarch64fbsdb"
#else
#define TARGET_LINKER_EMULATION  "aarch64fbsd"
#endif

#undef  SUBTARGET_EXTRA_LINK_SPEC
#define SUBTARGET_EXTRA_LINK_SPEC " -m" TARGET_LINKER_EMULATION

#undef  FBSD_TARGET_LINK_SPEC
#define FBSD_TARGET_LINK_SPEC "                                 \
    %{p:%nconsider using `-pg' instead of `-p' with gprof (1) } \
    %{v:-V}                                                     \
    %{assert*} %{R*} %{rpath*} %{defsym*}                       \
    %{shared:-Bshareable %{h*} %{soname*}}                      \
    %{symbolic:-Bsymbolic}                                      \
    %{static:-Bstatic}                                          \
    %{!static:                                                  \
      %{rdynamic:-export-dynamic}                               \
      %{!shared:-dynamic-linker " FBSD_DYNAMIC_LINKER " }}      \
    -X" SUBTARGET_EXTRA_LINK_SPEC "                             \
    %{mbig-endian:-EB} %{mlittle-endian:-EL}"

#if TARGET_FIX_ERR_A53_835769_DEFAULT
#define CA53_ERR_835769_SPEC \
  " %{!mno-fix-cortex-a53-835769:--fix-cortex-a53-835769}"
#else
#define CA53_ERR_835769_SPEC \
  " %{mfix-cortex-a53-835769:--fix-cortex-a53-835769}"
#endif

#ifdef TARGET_FIX_ERR_A53_843419_DEFAULT
#define CA53_ERR_843419_SPEC \
  " %{!mno-fix-cortex-a53-843419:--fix-cortex-a53-843419}"
#else
#define CA53_ERR_843419_SPEC \
  " %{mfix-cortex-a53-843419:--fix-cortex-a53-843419}"
#endif

#undef  LINK_SPEC
#define LINK_SPEC FBSD_TARGET_LINK_SPEC	\
                  CA53_ERR_835769_SPEC	\
                  CA53_ERR_843419_SPEC

#define GNU_USER_TARGET_MATHFILE_SPEC \
  "%{Ofast|ffast-math|funsafe-math-optimizations:crtfastmath.o%s}"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC \
    GNU_USER_TARGET_MATHFILE_SPEC " " \
    FBSD_ENDFILE_SPEC

#undef  TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()              \
  do                                          \
  {                                           \
      FBSD_TARGET_OS_CPP_BUILTINS ();         \
  }                                           \
  while (false)

#define TARGET_ASM_FILE_END file_end_indicate_exec_stack

/* Uninitialized common symbols in non-PIE executables, even with
   strong definitions in dependent shared libraries, will resolve
   to COPY relocated symbol in the executable.  See PR65780.  */
#undef TARGET_BINDS_LOCAL_P
#define TARGET_BINDS_LOCAL_P default_binds_local_p_2

#endif  /* GCC_AARCH64_FREEBSD_H */
