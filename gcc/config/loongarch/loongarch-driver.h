/* Subroutine headers for the gcc driver.
   Copyright (C) 2021-2025 Free Software Foundation, Inc.
   Contributed by Loongson Ltd.

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

#ifndef LOONGARCH_DRIVER_H
#define LOONGARCH_DRIVER_H

#include "loongarch-str.h"

#ifndef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC ""
#endif

#ifndef SUBTARGET_CC1_SPEC
#define SUBTARGET_CC1_SPEC ""
#endif

#ifndef SUBTARGET_ASM_SPEC
#define SUBTARGET_ASM_SPEC ""
#endif

#define EXTRA_SPECS \
  {"early_self_spec", ""}, \
  {"subtarget_cc1_spec", SUBTARGET_CC1_SPEC}, \
  {"subtarget_cpp_spec", SUBTARGET_CPP_SPEC}, \
  {"subtarget_asm_spec", SUBTARGET_ASM_SPEC},


#undef CPP_SPEC
#define CPP_SPEC \
  "%(subtarget_cpp_spec)"

#undef CC1_SPEC
#define CC1_SPEC \
  "%{G*} %{,ada:-gnatea %{mabi=*} -gnatez} " \
  "%(subtarget_cc1_spec)"

#if HAVE_AS_MRELAX_OPTION && HAVE_AS_COND_BRANCH_RELAXATION
#define ASM_MRELAX_DEFAULT "%{!mrelax:%{!mno-relax:-mrelax}}"
#else
#define ASM_MRELAX_DEFAULT "%{!mrelax:%{!mno-relax:-mno-relax}}"
#endif

#if HAVE_AS_MRELAX_OPTION
#define ASM_MRELAX_SPEC \
  "%{!mno-pass-mrelax-to-as:%{mrelax} %{mno-relax} " ASM_MRELAX_DEFAULT "}"
#else
#define ASM_MRELAX_SPEC \
  "%{mpass-mrelax-to-as:%{mrelax} %{mno-relax} " ASM_MRELAX_DEFAULT "}"
#endif

#undef ASM_SPEC
#define ASM_SPEC \
  "%{mabi=*} " ASM_MRELAX_SPEC " %(subtarget_asm_spec)"


extern const char*
la_driver_init (int argc, const char **argv);

extern const char*
driver_set_m_parm (int argc, const char **argv);

extern const char*
driver_set_no_link (int argc, const char **argv);

extern const char*
driver_get_normalized_m_opts (int argc, const char **argv);

#define EXTRA_SPEC_FUNCTIONS \
  { "driver_init", la_driver_init }, \
  { "set_m_parm", driver_set_m_parm  }, \
  { "set_no_link", driver_set_no_link }, \
  { "get_normalized_m_opts", driver_get_normalized_m_opts  },

/* Pre-process ABI-related options.  */
#define LA_SET_PARM_SPEC(NAME) \
  " %{m" OPTSTR_##NAME  "=*: %:set_m_parm(" OPTSTR_##NAME " %*)}" \

/* For MLIB_SELF_SPECS.  */
#include "loongarch-multilib.h"

#ifndef MLIB_SELF_SPECS
#define MLIB_SELF_SPECS ""
#endif

#define DRIVER_HANDLE_MACHINE_OPTIONS \
  " %(early_self_spec)", \
  MLIB_SELF_SPECS \
  " %:driver_init()" \
  " %{c|S|E|nostdlib: %:set_no_link()}" \
  " %{nostartfiles: %{nodefaultlibs: %:set_no_link()}}" \
  LA_SET_PARM_SPEC (ABI_BASE) \
  LA_SET_PARM_SPEC (ARCH) \
  LA_SET_PARM_SPEC (TUNE) \
  LA_SET_PARM_SPEC (ISA_EXT_FPU) \
  LA_SET_PARM_SPEC (ISA_EXT_SIMD) \
  LA_SET_PARM_SPEC (CMODEL) \
  " %:get_normalized_m_opts()"

#define DRIVER_SELF_SPECS \
  DRIVER_HANDLE_MACHINE_OPTIONS

/* ABI spec strings.  */
#define ABI_GRLEN_SPEC \
  "%{mabi=lp64*:64}"   \

#define ABI_SPEC \
  "%{mabi=lp64d:lp64d}" \
  "%{mabi=lp64f:lp64f}" \
  "%{mabi=lp64s:lp64s}" \

#endif /* LOONGARCH_DRIVER_H */
