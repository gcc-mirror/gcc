/* Definitions of target machine for GNU compiler.
   Sun Chorus OS big-endian
   Copyright (c) 2001 Free Software Foundation, Inc.

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

#undef DWARF2_DEBUGGING_INFO
#define DWARF2_DEBUGGING_INFO

#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (Sun Chorus OS Embedded)");

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

/* Type used for wchar_t, as a string used in a declaration.  */
#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef LINK_SPEC
#define LINK_SPEC ""
