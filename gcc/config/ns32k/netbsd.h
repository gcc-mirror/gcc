/* Configuration for a ns32532 running NetBSD as the target machine.
   Copyright (C) 1988, 1994, 1995, 1996, 1998 Free Software Foundation, Inc.

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
Boston, MA 02111-1307, USA.

*/

#include <ns32k/ns32k.h>

/* Compile for the floating point unit & 32532 by default;
   Don't assume SB is zero;
   Don't use bitfield instructions;
   FPU is 32381; */

#define TARGET_DEFAULT (1 + 24 + 32 + 64 + 256)

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

/* Use pc relative addressing whenever possible,
   it's more efficient than absolute (ns32k.c)
   You have to fix a bug in gas 1.38.1 to make this work with gas,
   patch available from jkp@cs.hut.fi.
   (NetBSD's gas version has this patch already applied) */

#define PC_RELATIVE

/* Operand of bsr or jsr should be just the address.  */

#define CALL_MEMREF_IMPLICIT

/* movd insns may have floating point constant operands.  */

#define MOVD_FLOAT_OK

/* Get generic NetBSD definitions. */
#include <netbsd.h>

/* Names to predefine in the preprocessor for this target machine.  */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dns32k -Dns32000 -Dns32532 -D__NetBSD__ -Dpc532 -D__ns32k__ -D__KPRINTF_ATTRIBUTE__ -Asystem(unix) -Asystem(NetBSD) -Acpu(ns32k) -Amachine(ns32k)"

/* Make gcc agree with <machine/ansi.h> */

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef WCHAR_TYPE
#define WCHAR_TYPE	"int"

#undef WCHAR_UNSIGNED
#define WCHAR_UNSIGNED	0

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE	32

/* This is BSD, so it wants DBX format.  */

#define DBX_DEBUGGING_INFO

/* Do not break .stabs pseudos into continuations.  */

#define DBX_CONTIN_LENGTH 0

/* This is the char to use for continuation (in case we need to turn
   continuation back on).  */

#define DBX_CONTIN_CHAR '?'

/* Don't default to pcc-struct-return, because gcc is the only compiler, and
   we want to retain compatibility with older gcc versions.  */

#undef PCC_STATIC_STRUCT_RETURN
#define DEFAULT_PCC_STRUCT_RETURN 0

/* Until they use ELF or something that handles dwarf2 unwinds
   and initialization stuff better.  */
#define DWARF2_UNWIND_INFO 0

