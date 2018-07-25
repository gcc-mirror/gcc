/* Definitions for RISC-V GNU/Linux systems with ELF format.
   Copyright (C) 1998-2018 Free Software Foundation, Inc.

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

#define TARGET_OS_CPP_BUILTINS()				\
  do {								\
    GNU_USER_TARGET_OS_CPP_BUILTINS();				\
  } while (0)

#define GLIBC_DYNAMIC_LINKER "/lib/ld-linux-riscv" XLEN_SPEC "-" ABI_SPEC ".so.1"

#define MUSL_ABI_SUFFIX \
  "%{mabi=ilp32:-sf}" \
  "%{mabi=ilp32f:-sp}" \
  "%{mabi=ilp32d:}" \
  "%{mabi=lp64:-sf}" \
  "%{mabi=lp64f:-sp}" \
  "%{mabi=lp64d:}"

#undef MUSL_DYNAMIC_LINKER
#define MUSL_DYNAMIC_LINKER "/lib/ld-musl-riscv" XLEN_SPEC MUSL_ABI_SUFFIX ".so.1"

/* Because RISC-V only has word-sized atomics, it requries libatomic where
   others do not.  So link libatomic by default, as needed.  */
#undef LIB_SPEC
#ifdef LD_AS_NEEDED_OPTION
#define LIB_SPEC GNU_USER_TARGET_LIB_SPEC \
  " %{pthread:" LD_AS_NEEDED_OPTION " -latomic " LD_NO_AS_NEEDED_OPTION "}"
#else
#define LIB_SPEC GNU_USER_TARGET_LIB_SPEC " -latomic "
#endif

#define ICACHE_FLUSH_FUNC "__riscv_flush_icache"

#define CPP_SPEC "%{pthread:-D_REENTRANT}"

#define LD_EMUL_SUFFIX \
  "%{mabi=lp64d:}" \
  "%{mabi=lp64f:_lp64f}" \
  "%{mabi=lp64:_lp64}" \
  "%{mabi=ilp32d:}" \
  "%{mabi=ilp32f:_ilp32f}" \
  "%{mabi=ilp32:_ilp32}"

#define LINK_SPEC "\
-melf" XLEN_SPEC "lriscv" LD_EMUL_SUFFIX " \
%{mno-relax:--no-relax} \
%{shared} \
  %{!shared: \
    %{!static: \
      %{rdynamic:-export-dynamic} \
      -dynamic-linker " GNU_USER_DYNAMIC_LINKER "} \
    %{static:-static}}"

#define TARGET_ASM_FILE_END file_end_indicate_exec_stack
