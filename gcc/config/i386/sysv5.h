/* Definitions for Intel 386 running System V Release 5 (i.e. UnixWare 7)
   Copyright (C) 1999, 2007 Free Software Foundation, Inc.
   Contributed by Robert Lipe (robertlipe@usa.net)

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


/* Dwarf2 is supported by native debuggers  */

#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

/* Add -lcrt for Dwarf2 abbreviation table */
#undef  LIB_SPEC
#define LIB_SPEC "%{pthread:-lthread} %{pthreadT:-lthreadT} \
	%{!shared:%{!symbolic:-lc -lcrt}}"

#undef CPP_SPEC
#define CPP_SPEC "%{pthread:-D_REENTRANT} %{pthreadT:-D_REENTRANT}"
