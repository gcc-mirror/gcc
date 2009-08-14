/* Definitions of target machine GNU compiler. 64bit IA64-VMS version.
   Copyright (C) 2004-2009 Free Software Foundation, Inc.
   Contributed by Douglas B Rupp (rupp@gnat.com).

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

#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()		\
    do {					\
	builtin_define_std ("vms");		\
	builtin_define_std ("VMS");		\
	builtin_define ("__IA64");		\
	builtin_assert ("system=vms");		\
	builtin_define ("__IEEE_FLOAT");	\
	builtin_define ("__LONG_POINTERS=1");	\
    } while (0)

#undef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE 64

#undef POINTER_SIZE
#define POINTER_SIZE 64

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_DWARF2_ASM | MASK_GNU_AS | MASK_MALLOC64)

#include "config/vms/vms-crtl-64.h"
