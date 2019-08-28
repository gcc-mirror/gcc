/* Target macros for arc64-*-linux targets.

   Copyright (C) 2021 Free Software Foundation, Inc.

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

#define GLIBC_DYNAMIC_LINKER "/lib/ld-linux-arc64.so.2"

/* Note that the default is to link against dynamic libraries, if they are
   available.  Override with -static.  */
#undef LINK_SPEC
#define LINK_SPEC "%{h*} \
  %{static:-Bstatic}					    \
  %{shared:-shared}					    \
  %{symbolic:-Bsymbolic}				    \
  %{!static:						    \
    %{rdynamic:-export-dynamic}				    \
    %{!shared:-dynamic-linker " GNU_USER_DYNAMIC_LINKER "}} \
  %{mcpu=hs5*:-m arc64linux32}				    \
  -X "

#undef ARC64_64BIT_DEFAULT
#define ARC64_64BIT_DEFAULT 1
