/* Configuration file for Symbian OS on ARM processors.
   Copyright (C) 2004
   Free Software Foundation, Inc.
   Contributed by CodeSourcery, LLC   

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* Use the SYMBIAN ABI by default.  */
#define ARM_DEFAULT_ABI ARM_ABI_AAPCS

/* Do not expand builtin functions (unless explicitly prefixed with
   "__builtin").  Symbian OS code relies on properties of the standard
   library that go beyond those guaranteed by the ANSI/ISO standard.
   For example, "memcpy" works even with overlapping memory, like
   "memmove".  We cannot simply set flag_no_builtin in arm.c because
   (a) flag_no_builtin is not declared in language-independent code,
   and (b) that would prevent users from explicitly overriding the
   default with -fno-builtin, which may sometimes be useful.

   Make all symbols hidden by default.  Symbian OS expects that all
   exported symbols will be explicitly marked with
   "__declspec(dllexport)".  */
#define CC1_SPEC "-fno-builtin -fvisibility=hidden"
#define CC1PLUS_SPEC "-fno-builtin -fvisibility=hidden"

/* Symbian OS does not use crt0.o, unlike the generic unknown-elf
   configuration.  */
#undef STARTFILE_SPEC
#define STARTFILE_SPEC "crti%O%s crtbegin%O%s"

/* The generic link spec in elf.h does not support shared libraries.  */
#undef LINK_SPEC
#define LINK_SPEC "%{mbig-endian:-EB} %{mlittle-endian:-EL} "		\
  "%{static:-Bstatic} %{shared:-shared} %{symbolic:-Bsymbolic} "	\
  "-X"
