/* Definitions of target machine for GNU compiler, for "naked" Intel
   80960 using coff object format and coff debugging symbols.
   Copyright (C) 1988, 1989, 1991 Intel Corp.
   Contributed by Steven McGeady (mcg@omepd.intel.com)
   Additional work by Glenn Colon-Bonet, Jonathan Shapiro, Andy Wilson
   Converted to GCC 2.0 by Michael Tiemann, Cygnus Support.
 */

/*
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

#include "i960/i960.h"

/* Generate SDB_DEBUGGING_INFO by default.  */
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE SDB_DEBUG

/* end of i960-coff.h */
