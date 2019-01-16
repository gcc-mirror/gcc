/* Copyright (C) 2009-2019 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{!shared: crt1%O%s} crti%O%s crtbegin%O%s"

#undef LINK_SPEC
#define LINK_SPEC "-elf2flt"

#define TARGET_OS_CPP_BUILTINS() GNU_USER_TARGET_OS_CPP_BUILTINS()

#undef TARGET_LIBC_HAS_FUNCTION
#define TARGET_LIBC_HAS_FUNCTION no_c99_libc_has_function

/* Like the definition in gcc.c, but for purposes of uClinux, every link is
   static.  */
#define MFWRAP_SPEC " %{fmudflap|fmudflapth: \
 --wrap=malloc --wrap=free --wrap=calloc --wrap=realloc\
 --wrap=mmap --wrap=munmap --wrap=alloca\
 %{fmudflapth: --wrap=pthread_create\
}} %{fmudflap|fmudflapth: --wrap=main}"
