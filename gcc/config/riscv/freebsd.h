/* Definitions for RISC-V FreeBSD systems with ELF format.
   Copyright (C) 2018-2021 Free Software Foundation, Inc.

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

#undef  SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS \
  { "fbsd_dynamic_linker", FBSD_DYNAMIC_LINKER }

#undef CC1_SPEC
#define CC1_SPEC "%(cc1_cpu) %{profile:-p}"

/* Provide a LINK_SPEC appropriate for FreeBSD.  Here we provide support
   for the special GCC options -static and -shared, which allow us to
   link things in one of these three modes by applying the appropriate
   combinations of options at link-time.

   When the -shared link option is used a final link is not being
   done.  */

#undef LINK_SPEC
#define LINK_SPEC "						\
  -melf" XLEN_SPEC DEFAULT_ENDIAN_SPEC "riscv			\
  %{p:%nconsider using `-pg' instead of `-p' with gprof (1)}	\
  %{v:-V}							\
  %{assert*} %{R*} %{rpath*} %{defsym*}				\
  %{mbig-endian:-EB}						\
  %{mlittle-endian:-EL}						\
  %{shared:-Bshareable %{h*} %{soname*}}			\
  %{symbolic:-Bsymbolic}					\
  %{static:-Bstatic}						\
  %{!shared:							\
      %{!static:						\
        %{rdynamic:-export-dynamic}				\
        -dynamic-linker " FBSD_DYNAMIC_LINKER "}		\
        %{static:-static}}"

#define STARTFILE_PREFIX_SPEC 			\
   "/lib" XLEN_SPEC "/" ABI_SPEC "/ "		\
   "/usr/lib" XLEN_SPEC "/" ABI_SPEC "/ "	\
   "/lib/ "					\
   "/usr/lib/ "
