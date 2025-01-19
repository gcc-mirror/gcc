/* Definitions for LoongArch systems using GNU (glibc-based) userspace,
   or other userspace with libc derived from glibc.
   Copyright (C) 2021-2025 Free Software Foundation, Inc.

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

/* Define the size of the wide character type.  */
#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32


/* GNU-specific SPEC definitions.  */
#define GNU_USER_LINK_EMULATION "elf" ABI_GRLEN_SPEC "loongarch"

#undef GLIBC_DYNAMIC_LINKER
#define GLIBC_DYNAMIC_LINKER \
  "/lib" ABI_GRLEN_SPEC "/ld-linux-loongarch-" ABI_SPEC ".so.1"

#define MUSL_ABI_SPEC \
  "%{mabi=lp64d:}" \
  "%{mabi=lp64f:-sp}" \
  "%{mabi=lp64s:-sf}"

#undef MUSL_DYNAMIC_LINKER
#define MUSL_DYNAMIC_LINKER \
  "/lib/ld-musl-loongarch" ABI_GRLEN_SPEC MUSL_ABI_SPEC ".so.1"

#undef GNU_USER_TARGET_LINK_SPEC
#define GNU_USER_TARGET_LINK_SPEC \
  "%{G*} %{shared} -m " GNU_USER_LINK_EMULATION \
  "%{!shared: %{static} " \
  "%{!static: %{!static-pie: %{rdynamic:-export-dynamic} " \
  "-dynamic-linker " GNU_USER_DYNAMIC_LINKER "}} " \
  "%{static-pie: -static -pie --no-dynamic-linker -z text}}" \
  "%{mno-relax: --no-relax}"


/* Similar to standard Linux, but adding -ffast-math support.  */
#undef GNU_USER_TARGET_MATHFILE_SPEC
#define GNU_USER_TARGET_MATHFILE_SPEC \
  "%{Ofast|ffast-math|funsafe-math-optimizations:%{!shared:crtfastmath.o%s}}"

#undef LIB_SPEC
#define LIB_SPEC GNU_USER_TARGET_LIB_SPEC

#undef LINK_SPEC
#define LINK_SPEC GNU_USER_TARGET_LINK_SPEC

#undef ENDFILE_SPEC
#define ENDFILE_SPEC \
  GNU_USER_TARGET_MATHFILE_SPEC " " \
  GNU_USER_TARGET_ENDFILE_SPEC

#undef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC "%{posix:-D_POSIX_SOURCE} %{pthread:-D_REENTRANT}"

/* A standard GNU/Linux mapping.  On most targets, it is included in
   CC1_SPEC itself by config/linux.h, but loongarch.h overrides CC1_SPEC
   and provides this hook instead.  */
#undef SUBTARGET_CC1_SPEC
#define SUBTARGET_CC1_SPEC GNU_USER_TARGET_CC1_SPEC

#define TARGET_OS_CPP_BUILTINS() \
  do \
    { \
      GNU_USER_TARGET_OS_CPP_BUILTINS (); \
      /* The GNU C++ standard library requires this.  */ \
      if (c_dialect_cxx ()) \
       builtin_define ("_GNU_SOURCE"); \
    } \
  while (0)
