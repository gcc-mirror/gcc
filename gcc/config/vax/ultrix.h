/* Output variables, constants and external declarations, for GNU compiler.
   Copyright (C) 1999, 2002 Free Software Foundation, Inc.

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


#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
      builtin_define_std ("unix");		\
      builtin_define_std ("ultrix");		\
      builtin_define_std ("bsd4_2");		\
      builtin_assert ("system=unix");		\
      builtin_assert ("system=bsd");		\
						\
      builtin_define_std ("vax");		\
      if (TARGET_G_FLOAT)			\
	builtin_define_std ("GFLOAT");		\
    }						\
  while (0)

/* These are as defined in /usr/include/sys/stdtypes.h.
   These values are for ultrix 4.2 on the VAX.  */
#define SIZE_TYPE "unsigned int"
#define PTRDIFF_TYPE "int"
#define WCHAR_TYPE "unsigned int"
#define WCHAR_TYPE_SIZE 32
