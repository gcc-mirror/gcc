/* Configuration fragment for a VAX OpenBSD target.
   Copyright (C) 2000, 2002 Free Software Foundation, Inc.

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

/* Amend common OpenBSD definitions for VAX target.  */

#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
      builtin_define ("__unix__");		\
      builtin_define ("__OpenBSD__");		\
      builtin_assert ("system=unix");		\
      builtin_assert ("system=OpenBSD");	\
    }						\
  while (0)

/* Layout of source language data types.  */

/* This must agree with <machine/ansi.h>  */
#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32
