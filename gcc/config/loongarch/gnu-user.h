/* Definitions for LoongArch systems using GNU userspace.
   Copyright (C) 2020-2021 Free Software Foundation, Inc.

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

/* GNU-specific SPEC definitions */
#define GNU_USER_LINK_EMULATION64 "elf64loongarch"

#define GLIBC_DYNAMIC_LINKER_LP64 "/lib64/ld.so.1"

#undef GNU_USER_TARGET_LINK_SPEC
#define GNU_USER_TARGET_LINK_SPEC "\
  %{G*} %{shared} \
  %{!shared: \
    %{!static: \
      %{rdynamic:-export-dynamic} \
      %{mabi=lp64: -dynamic-linker " GLIBC_DYNAMIC_LINKER_LP64 "}} \
    %{static}} \
  %{mabi=lp64:-m" GNU_USER_LINK_EMULATION64 "}"

/* Similar to standard Linux, but adding -ffast-math support.  */
#undef GNU_USER_TARGET_MATHFILE_SPEC
#define GNU_USER_TARGET_MATHFILE_SPEC \
  "%{Ofast|ffast-math|funsafe-math-optimizations:crtfastmath.o%s}"

#undef LIB_SPEC
#define LIB_SPEC GNU_USER_TARGET_LIB_SPEC

#undef LINK_SPEC
#define LINK_SPEC GNU_USER_TARGET_LINK_SPEC

#undef ENDFILE_SPEC
#define ENDFILE_SPEC \
  GNU_USER_TARGET_MATHFILE_SPEC " " \
  GNU_USER_TARGET_ENDFILE_SPEC

/* If we don't set MASK_ABICALLS, we can't default to PIC.  */
/* #undef TARGET_DEFAULT */
/* #define TARGET_DEFAULT MASK_ABICALLS */

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


/* -G is incompatible with -KPIC which is the default, so only allow objects
   in the small data section if the user explicitly asks for it.  */
#undef LARCH_DEFAULT_GVALUE
#define LARCH_DEFAULT_GVALUE 0

/* The glibc _mcount stub will save $v0 for us.  Don't mess with saving
   it, since ASM_OUTPUT_REG_PUSH/ASM_OUTPUT_REG_POP do not work in the
   presence of $gp-relative calls.  */
#undef ASM_OUTPUT_REG_PUSH
#undef ASM_OUTPUT_REG_POP


#undef ASM_DECLARE_OBJECT_NAME
#define ASM_DECLARE_OBJECT_NAME loongarch_declare_object_name
