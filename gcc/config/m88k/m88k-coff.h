/* Definitions for "naked" Motorola 88k using coff object format files
   and coff debugging info.

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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "m88k/m88k.h"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dm88000 -Dm88k -Acpu(m88k) -Amachine(m88k)"

#undef DBX_DEBUGGING_INFO
#define SDB_DEBUGGING_INFO

/* Output DBX (stabs) debugging information if using -gstabs.  */

#define DBX_DEBUGGING_INFO

#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE SDB_DEBUG

/* end of m88k-coff.h */
