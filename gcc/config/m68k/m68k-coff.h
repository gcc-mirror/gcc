/* Definitions of target machine for GNU compiler.  "naked" 68020,
   COFF object files and debugging, version.
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

#define MOTOROLA	/* Use Motorola syntax rather than MIT.  */
#ifndef  USE_GAS  /* forces jsbr instead of jsr.  */
#define  USE_GAS
#endif

#include "m68k/m68k-none.h"
#include "m68k/m68kemb.h"
#include "m68k/coff.h"

/* end of m68k-coff.h */
