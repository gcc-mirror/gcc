/* Definitions of target machine for GNU compiler,
   for IBM RS/6000 POWER running AIX version 4.1.
   Copyright (C) 1994 Free Software Foundation, Inc.
   Contributed by David Edelsohn (edelsohn@npac.syr.edu).

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include "rs6000/rs6000.h"

#undef ASM_SPEC
#define ASM_SPEC "-u -mpwr"

#undef LINK_SPEC
#define LINK_SPEC "-bpT:0x10000000 -bpD:0x20000000 %{!r:-btextro} -bnodelcsect\
   %{static:-bnso -bI:/lib/syscalls.exp} %{g*:-bexport:/usr/lib/libg.exp}"
