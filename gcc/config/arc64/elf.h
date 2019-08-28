/* Target macros for arc*-elf targets.

   Copyright (C) 2021 Free Software Foundation, Inc.

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

#undef DWARF2_UNWIND_INFO
#define DWARF2_UNWIND_INFO 0

/* If no specs file is enforced, default to nosys libarary.  */
#undef LINK_GCC_C_SEQUENCE_SPEC
#define LINK_GCC_C_SEQUENCE_SPEC				\
  "--start-group %G %{!specs=*:%{!nolibc:-lc -lnosys}} --end-group"

/* Make sure we include the crtbegin.o.  */
#undef STARTFILE_SPEC
#define STARTFILE_SPEC "crt0%O%s crti%O%s crtbegin%O%s"

/* ...and crtend.o.  */
#undef ENDFILE_SPEC
#define ENDFILE_SPEC "crtend%O%s crtn%O%s"

#undef UNALIGNED_ACCESS_DEFAULT
#define UNALIGNED_ACCESS_DEFAULT 1
