/* Target definitions for GNU compiler for Sparc running Solaris 2.x
   Copyright (C) 1992 Free Software Foundation, Inc.

   Written by Ron Guilmette (rfg@ncd.com).

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

/* This is basically just a Sparc running svr4.  */

#include "sparcv4.h"

/* Solaris 2.x on a Sparc uses BSD stabs, not DWARF.  */

#undef DWARF_DEBUGGING_INFO
#define DBX_DEBUGGING_INFO

/* If we cannot find the GNU *crt*.o files in the STANDARD_STARTFILE_PREFIX
   directory, our fallback strategy must be to look for these files instead
   in the Sun C 2.0 directory.  */

#define MD_STARTFILE_PREFIX_1	"/opt/SUNWste/SC2.0/"
