/* Base configuration file for all Phoenix-RTOS targets.
   Copyright (C) 2016-2021 Free Software Foundation, Inc.

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
#define TARGET_OS_CPP_BUILTINS()           \
    do {                                   \
      builtin_define_std ("phoenix");      \
      builtin_define_std ("unix");         \
      builtin_assert ("system=phoenix");   \
      builtin_assert ("system=unix");      \
    } while (0)

#define STD_LIB_SPEC "%{!shared:%{g*:-lg} %{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p}}"

/* This will prevent selecting 'unsigned long int' instead of 'unsigned int' as 'uint32_t' in stdint-newlib.h. */
#undef STDINT_LONG32
#define STDINT_LONG32		0
