/* Definitions for "naked" Motorola 88k using a.out object format files
   and stabs debugging info.

   Copyright (C) 1994 Free Software Foundation, Inc.

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

#undef SDB_DEBUGGING_INFO
#define DBX_DEBUGGING_INFO

#include "m88k/m88k.h"
#include "aoutos.h"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dm88000 -Dm88k"

/* end of m88k-aout.h */
