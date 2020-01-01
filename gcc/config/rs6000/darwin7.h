/* Target definitions for Darwin 7.x (Mac OS X) systems.
   Copyright (C) 2004-2020 Free Software Foundation, Inc.

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

/* This generation of tools (specifically the archive tool) did not
   export weak symbols from the TOC. */
#undef TARGET_WEAK_NOT_IN_ARCHIVE_TOC
#define TARGET_WEAK_NOT_IN_ARCHIVE_TOC 1

/* Default to the last version, with most support for C++.  */
#undef DEF_MIN_OSX_VERSION
#define DEF_MIN_OSX_VERSION "10.3.9"
