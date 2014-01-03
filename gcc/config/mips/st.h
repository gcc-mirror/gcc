/* ST 2e / 2f GNU/Linux Configuration.
   Copyright (C) 2008-2014 Free Software Foundation, Inc.

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

/* The various C libraries each have their own subdirectory.  */
#undef SYSROOT_SUFFIX_SPEC
#define SYSROOT_SUFFIX_SPEC			\
  "%{march=loongson2e:/2e ;			\
     march=loongson2f:/2f}"

#undef STARTFILE_PREFIX_SPEC
#define STARTFILE_PREFIX_SPEC				\
  "%{mabi=32: /usr/local/lib/ /lib/ /usr/lib/}		\
   %{mabi=n32: /usr/local/lib32/ /lib32/ /usr/lib32/}	\
   %{mabi=64: /usr/local/lib64/ /lib64/ /usr/lib64/}"
