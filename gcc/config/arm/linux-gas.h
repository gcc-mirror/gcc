/* Definitions of target machine for GNU compiler.  ARM Linux version.
   Copyright (C) 1997 Free Software Foundation, Inc.
   Contributed by Russell King  <rmk92@ecs.soton.ac.uk>.

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
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Limit the length of a stabs entry (for the broken Acorn assembler) */
#define DBX_CONTIN_LENGTH 80

#include "arm/linux.h"

/*
 * We are using GAS, so stabs should work.
 */

#ifndef DBX_DEBUGGING_INFO
#define DBX_DEBUGGING_INFO 1
#endif
