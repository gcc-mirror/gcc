/* Definitions for the moxiebox.
   Copyright (C) 2014-2023 Free Software Foundation, Inc.
   Contributed by Anthony Green (green@moxielogic.com)

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

/* Target OS preprocessor built-ins.  */
#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
      builtin_define_std ("moxie");		\
      builtin_define ("__moxiebox__");		\
      builtin_assert ("system=moxiebox");	\
    }						\
  while (0)

#undef LIB_SPEC
#define LIB_SPEC \
"%{!T*:-Tmoxiebox.ld} \
 %{!nostdlib: --start-group -lsandboxrt -lc -lgcc --end-group }"

#undef LINK_SPEC
#define LINK_SPEC "%{h*} %{v:-V} -EL -Bstatic"

#undef  ASM_SPEC
#define ASM_SPEC "-EL"

#undef CC1_SPEC
#define CC1_SPEC "-mel %{meb:%ethis target is little-endian}"

#undef CC1PLUS_SPEC
#define CC1PLUS_SPEC CC1_SPEC

#undef MULTILIB_DEFAULTS

#undef SIZE_TYPE
#undef PTRDIFF_TYPE
#undef WCHAR_TYPE
#undef WCHAR_TYPE_SIZE

#define TARGET_MOXIEBOX
