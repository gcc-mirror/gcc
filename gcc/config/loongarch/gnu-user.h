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

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

#undef ASM_DECLARE_OBJECT_NAME
#define ASM_DECLARE_OBJECT_NAME loongarch_declare_object_name

/* If we don't set MASK_ABICALLS, we can't default to PIC.  */
/* #undef TARGET_DEFAULT */
/* #define TARGET_DEFAULT MASK_ABICALLS */

#define TARGET_OS_CPP_BUILTINS()				\
  do {								\
    GNU_USER_TARGET_OS_CPP_BUILTINS();				\
    /* The GNU C++ standard library requires this.  */		\
    if (c_dialect_cxx ())					\
      builtin_define ("_GNU_SOURCE");				\
  } while (0)

#undef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC "%{posix:-D_POSIX_SOURCE} %{pthread:-D_REENTRANT}"

/* A standard GNU/Linux mapping.  On most targets, it is included in
   CC1_SPEC itself by config/linux.h, but loongarch.h overrides CC1_SPEC
   and provides this hook instead.  */
#undef SUBTARGET_CC1_SPEC
#define SUBTARGET_CC1_SPEC GNU_USER_TARGET_CC1_SPEC

/* -G is incompatible with -KPIC which is the default, so only allow objects
   in the small data section if the user explicitly asks for it.  */
#undef LARCH_DEFAULT_GVALUE
#define LARCH_DEFAULT_GVALUE 0

#undef GNU_USER_TARGET_LINK_SPEC
#define GNU_USER_TARGET_LINK_SPEC "\
  %{G*} %{shared} \
  %{!shared: \
    %{!static: \
      %{rdynamic:-export-dynamic} \
      %{mabi=lp32: -dynamic-linker " GNU_USER_DYNAMIC_LINKERLP32 "} \
      %{mabi=lp64: -dynamic-linker " GNU_USER_DYNAMIC_LINKERLP64 "}} \
    %{static}} \
  %{mabi=lp32:-m" GNU_USER_LINK_EMULATION32 "} \
  %{mabi=lp64:-m" GNU_USER_LINK_EMULATION64 "}"

#undef LINK_SPEC
#define LINK_SPEC GNU_USER_TARGET_LINK_SPEC

/* The glibc _mcount stub will save $v0 for us.  Don't mess with saving
   it, since ASM_OUTPUT_REG_PUSH/ASM_OUTPUT_REG_POP do not work in the
   presence of $gp-relative calls.  */
#undef ASM_OUTPUT_REG_PUSH
#undef ASM_OUTPUT_REG_POP

#undef LIB_SPEC
#define LIB_SPEC GNU_USER_TARGET_LIB_SPEC

#define NO_SHARED_SPECS ""

/* -march=native handling only makes sense with compiler running on
   a LoongArch chip.  */
#if defined(__loongarch__)
extern const char *host_detect_local_cpu (int argc, const char **argv);
# define EXTRA_SPEC_FUNCTIONS \
  { "local_cpu_detect", host_detect_local_cpu },

# define MARCH_MTUNE_NATIVE_SPECS				\
  " %{march=native:%<march=native %:local_cpu_detect(arch)}"	\
  " %{mtune=native:%<mtune=native %:local_cpu_detect(tune)}"
#else
# define MARCH_MTUNE_NATIVE_SPECS ""
#endif

#define LINUX_DRIVER_SELF_SPECS \
  NO_SHARED_SPECS							\
  MARCH_MTUNE_NATIVE_SPECS,						\
  "%{!mabi=*: -" MULTILIB_ABI_DEFAULT "}"

#undef DRIVER_SELF_SPECS
#define DRIVER_SELF_SPECS \
  LINUX_DRIVER_SELF_SPECS, \
  BASE_DRIVER_SELF_SPECS

/* Similar to standard Linux, but adding -ffast-math support.  */
#undef	GNU_USER_TARGET_MATHFILE_SPEC
#define GNU_USER_TARGET_MATHFILE_SPEC \
  "%{Ofast|ffast-math|funsafe-math-optimizations:crtfastmath.o%s}"
#undef  ENDFILE_SPEC
#define ENDFILE_SPEC \
  GNU_USER_TARGET_MATHFILE_SPEC " " \
  GNU_USER_TARGET_ENDFILE_SPEC
