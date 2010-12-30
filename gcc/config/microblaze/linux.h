/* Definitions for MicroBlaze running Linux.
   Copyright 2009, 2010 Free Software Foundation, Inc.

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
    %{static:-static}}"

