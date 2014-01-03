/* Definitions for MicroBlaze running Linux.
   Copyright (C) 2009-2014 Free Software Foundation, Inc.

   This file is part of GCC.

   Contributed by Michael Eager <eager@eagercon.com>.

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

#undef TARGET_SUPPORTS_PIC
#define TARGET_SUPPORTS_PIC 1

#undef TLS_NEEDS_GOT
#define TLS_NEEDS_GOT 1

#define DYNAMIC_LINKER "/lib/ld.so.1"
#undef  SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS \
  { "dynamic_linker", DYNAMIC_LINKER }

#undef LINK_SPEC
#define LINK_SPEC "%{shared:-shared} \
  %{!shared: \
    %{!static: \
      %{rdynamic:-export-dynamic} \
      -dynamic-linker %(dynamic_linker)} \
    %{static:-static}} \
  %{mbig-endian:-EB} \
  %{mlittle-endian:-EL}"

/* For the microblaze-*-linux* subtarget.  */
#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS() GNU_USER_TARGET_OS_CPP_BUILTINS()
