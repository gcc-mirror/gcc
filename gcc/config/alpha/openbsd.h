/* Configuration file for an alpha OpenBSD target.
   Copyright (C) 1999-2016 Free Software Foundation, Inc.

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

/* Controlling the compilation driver.  */

/* run-time target specifications */
#define TARGET_OS_CPP_BUILTINS()		\
    do {					\
	OPENBSD_OS_CPP_BUILTINS_ELF();		\
	OPENBSD_OS_CPP_BUILTINS_LP64();		\
    } while (0)

/* Layout of source language data types.  */

/* This must agree with <machine/ansi.h> */
#undef SIZE_TYPE
#define SIZE_TYPE "long unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "long int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32


#define LOCAL_LABEL_PREFIX	"."
