/* Definitions of target machine for GNU compiler,
   for NetBSD/vax ELF systems.
   Copyright (C) 2002-2019 Free Software Foundation, Inc.

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

/* Names to predefine in the preprocessor for this target OS.  */
#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
      NETBSD_OS_CPP_BUILTINS_ELF();		\
    }						\
  while (0)

#undef CPP_SPEC
#define CPP_SPEC NETBSD_CPP_SPEC

#ifndef NETBSD_CC1_AND_CC1PLUS_SPEC
#define NETBSD_CC1_AND_CC1PLUS_SPEC ""
#endif

#undef CC1_SPEC
#define CC1_SPEC NETBSD_CC1_AND_CC1PLUS_SPEC VAX_CC1_AND_CC1PLUS_SPEC

#undef CC1PLUS_SPEC
#define CC1PLUS_SPEC NETBSD_CC1_AND_CC1PLUS_SPEC VAX_CC1_AND_CC1PLUS_SPEC

#define NETBSD_ENTRY_POINT "__start"

#undef LINK_SPEC
#define LINK_SPEC NETBSD_LINK_SPEC_ELF

#undef EXTRA_SPECS
#define EXTRA_SPECS NETBSD_SUBTARGET_EXTRA_SPECS
#undef SUBTARGET_EXTRA_SPECS

#undef INTPTR_TYPE
#define INTPTR_TYPE "long int"

#undef UINTPTR_TYPE
#define UINTPTR_TYPE "long unsigned int"

/* We use gas, not the UNIX assembler.  */
#undef TARGET_DEFAULT
#define TARGET_DEFAULT MASK_QMATH
