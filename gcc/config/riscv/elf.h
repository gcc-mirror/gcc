/* Target macros for riscv*-elf targets.
   Copyright (C) 1994-2024 Free Software Foundation, Inc.

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

#define LINK_SPEC "\
-melf" XLEN_SPEC DEFAULT_ENDIAN_SPEC "riscv \
%{mno-relax:--no-relax} \
-X \
%{mbig-endian:-EB} \
%{mlittle-endian:-EL} \
%{shared}"

/* Link against Newlib libraries, because the ELF backend assumes Newlib.
   Handle the circular dependence between libc and libgloss. */
#undef  LIB_SPEC
#define LIB_SPEC \
  "--start-group -lc %{!specs=nosys.specs:-lgloss} --end-group " \
  "%{!nostartfiles:%{!nodefaultlibs:%{!nolibc:%{!nostdlib:%:riscv_multi_lib_check()}}}}"

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC "crt0%O%s crtbegin%O%s"

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC "crtend%O%s"

#define RISCV_USE_CUSTOMISED_MULTI_LIB select_by_abi_arch_cmodel
