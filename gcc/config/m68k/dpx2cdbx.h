/* Definitions for Bull dpx/2 200 and 300 with gas
   using dbx-in-coff encapsulation.
   Copyright (C) 1992, 1994 Free Software Foundation, Inc.

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


#include "m68k/dpx2g.h"

/* Use STABS debugging information inside COFF.  */
#ifndef DBX_DEBUGGING_INFO
#define DBX_DEBUGGING_INFO
#endif

/* Let sbd debugging be the default.  */
#define PREFERRED_DEBUGGING_TYPE SDB_DEBUG
