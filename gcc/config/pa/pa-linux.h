/* Definitions for PA_RISC with ELF format
   Copyright (C) 1999 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* FIXME - this doesn't seem to be used anywhere */
#define LINUX_DEFAULT_ELF

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-D__ELF__ -Dunix -D__hppa__ -Dlinux -Asystem(unix) -Asystem(posix) -Acpu(hppa) -Amachine(hppa) -Amachine(bigendian)"

#undef CPP_SPEC
#define CPP_SPEC "%{posix:-D_POSIX_SOURCE}\
 %{msnake:-D_PA_RISC1_1}\
 %{mpa-risc-1-1:-D_PA_RISC1_1}"

#undef	LIB_SPEC
#define LIB_SPEC "%{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p} -lmilli"

/* How to renumber registers for dbx and gdb.

   It is entirely possible linux will use a different numbering scheme.
   Until we know for sure, it's the same as hpux, osf & bsd, but we're
   ready if it needs to be different.

   Registers 0  - 31 remain unchanged.

   Registers 32 - 87 are mapped to 72 - 127

   Register 88 is mapped to 32.  */

#define DBX_REGISTER_NUMBER(REGNO) \
  ((REGNO) <= 31 ? (REGNO) :						\
   ((REGNO) > 31 && (REGNO) <= 87 ? (REGNO) + 40 : 32))
