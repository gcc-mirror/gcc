/* Definitions of target machine for GNU compiler, for HAL 
   SPARC running Solaris 2 HALOS
   Copyright 1998 Free Software Foundation, Inc.
   Contributed by Carol LePage (carolo@hal.com)

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

/* Need different command line for assembler */

#undef ASM_SPEC
#define ASM_SPEC \
  "%{V} %{v:%{!V:-V}} %{Qy:} %{!Qn:-Qy} %{n} %{T} %{Ym,*} %{Wa,*:%*} -e1 \
   %{fpic:-K PIC} %{fPIC:-K PIC}"

/* Need DWARF for debuggers.  */

#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF_DEBUG
