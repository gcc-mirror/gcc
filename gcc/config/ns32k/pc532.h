/* Definitions of target machine for GNU compiler.
   PC532 with National 32532.
   Copyright (C) 1990, 1994 Free Software Foundation, Inc.
   Contributed by Jukka Virtanen <jtv@hut.fi>, Jyrki Kuoppala <jkp@cs.hut.fi>,
   Tatu Yl|nen <ylo@ngs.fi>, Johannes Helander <jvh@cs.hut.fi>.

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

#include "ns32k/ns32k.h"

/* Compile for the floating point unit & 32532 by default;
   also presume SB is zero and no bitfield instructions */

#define TARGET_DEFAULT (1 + 24 + 64)

/* Write DBX debugging info for gdb to read */

#define DBX_DEBUGGING_INFO

/* Use the re-entrant and potentially faster method */

#undef PCC_STATIC_STRUCT_RETURN

/* 32-bit alignment for efficiency */
#undef POINTER_BOUNDARY
#define POINTER_BOUNDARY 32

/* 32-bit alignment for efficiency */
#undef FUNCTION_BOUNDARY
#define FUNCTION_BOUNDARY 32

/* 32532 spec says it can handle any alignment.  Rumor from tm-ns32k.h
   tells this might not be actually true (but it's for 32032, perhaps
   National has fixed the bug for 32532).  You might have to change this
   if the bug still exists. */

#undef STRICT_ALIGNMENT
#define STRICT_ALIGNMENT 0

/* Maybe someone needs to know which processor we're running on */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dns32000 -Dns32532 -Dpc532 -Dunix -Asystem=unix -Acpu=ns32k -Amachine=ns32k"

/* Use pc relative addressing whenever possible,
   it's more efficient than absolute (ns32k.c)
   You have to fix a bug in gas 1.38.1 to make this work with gas,
   patch available from jkp@cs.hut.fi. */

#define PC_RELATIVE

/* Operand of bsr or jsr should be just the address.  */

#define CALL_MEMREF_IMPLICIT

/* movd insns may have floating point constant operands.  */

#define MOVD_FLOAT_OK
