/* Definitions of target machine for GNU compiler.
   Renesas H8/300 (linux variant)
   Copyright (C) 2015-2024 Free Software Foundation, Inc.
   Contributed by Yoshinori Sato <ysato@users.sourceforge.jp>

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

#ifndef GCC_H8300_LINUX_H
#define GCC_H8300_LINUX_H

#define TARGET_OS_CPP_BUILTINS() \
  do						\
    {						\
      GNU_USER_TARGET_OS_CPP_BUILTINS();	\
    }						\
  while (0)

#undef LINK_SPEC
#define LINK_SPEC "%{mh:-mh8300helf_linux} %{ms:-m h8300self_linux} %{msx:-m h8300sxelf_linux}"

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_QUICKCALL | MASK_INT32 | MASK_H8300H)

/* Width of a word, in units (bytes).  */
#undef DOUBLE_TYPE_SIZE
#define DOUBLE_TYPE_SIZE	64

#undef DEFAULT_SIGNED_CHAR
#define DEFAULT_SIGNED_CHAR 1

#undef USER_LABEL_PREFIX

#define H8300_LINUX

#endif /* ! GCC_H8300_LINUX_H */
