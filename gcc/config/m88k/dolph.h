/* Definitions of target machine for GNU compiler.
   Motorola m88100 running the Dolphin UNIX System V/88 Release 3.2,
   Version 3.5/5.60.
   Copyright (C) 1992 Free Software Foundation, Inc.

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

#include "m88kv3.h"

/* Don't output structure tag names when it causes a forward reference.
   Symptom:
   Error messages like
   as: "/usr/tmp/cca22733.s": cannot reduce symbol table, unused symbols remain
   when compiling some programs.
   example program (C++): struct bad { bad(); }; bad::bad() {}

   This problem seems to have gone away, perhaps with release 3.6 of the O/S
   from Dolphin.  */
/* #undef SDB_ALLOW_FORWARD_REFERENCES */

/* Use T_ARG as T_VOID.  T_VOID is not defined in <syms.h> as it should be.  */
#define T_VOID T_ARG
