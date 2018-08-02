/* Copyright (C) 2007-2018 Free Software Foundation, Inc.

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

#undef SUBTARGET_DRIVER_SELF_SPECS
#define SUBTARGET_DRIVER_SELF_SPECS \
  "%{!mno-fdpic:-mfdpic} -micplb",

#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS() GNU_USER_TARGET_OS_CPP_BUILTINS()

#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{!shared: %{pg|p|profile:gcrt1.o%s;pie:Scrt1.o%s;:crt1.o%s}} crtreloc.o%s \
   crti.o%s %{shared|pie:crtbeginS.o%s;:crtbegin.o%s}"

#undef LINK_GCC_C_SEQUENCE_SPEC
#define LINK_GCC_C_SEQUENCE_SPEC \
  "%{static|static-pie:--start-group} %{mfast-fp:-lbffastfp} %G %{!nolibc:%L} \
   %{static|static-pie:--end-group} \
   %{!static:%{!static-pie:%{mfast-fp:-lbffastfp} %G}}"

#undef CPP_SPEC
#define CPP_SPEC "%{posix:-D_POSIX_SOURCE} %{pthread:-D_REENTRANT}"

#undef LINK_SPEC
#define LINK_SPEC "\
  %{mfdpic: -m elf32bfinfd -z text} %{shared} %{pie} \
  %{static:-dn -Bstatic} \
  %{shared:-G -Bdynamic} \
  %{!shared: %{!static: \
   %{rdynamic:-export-dynamic} \
   -dynamic-linker /lib/ld-uClibc.so.0} \
   %{static}} -init __init -fini __fini"

#undef TARGET_SUPPORTS_SYNC_CALLS
#define TARGET_SUPPORTS_SYNC_CALLS 1
