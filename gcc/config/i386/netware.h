/* Core target definitions for GCC for Intel 80386 running Netware 4.
   and using stabs-in-elf for the debugging format.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.

   Written by David V. Henkel-Wallace (gumby@cygnus.com)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (i386 Netware 4)");

#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
	builtin_define_std ("APX386");		\
	builtin_define ("__netware__");		\
	builtin_assert ("system=netware");	\
    }						\
  while (0)

#undef WCHAR_TYPE
#define WCHAR_TYPE "short unsigned int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 16
