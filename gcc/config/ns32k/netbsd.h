/* Configuration for a ns32532 running NetBSD as the target machine.
   Copyright (C) 1988, 1994, 1995, 1996, 1998, 2002
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


#define TARGET_OS_CPP_BUILTINS()			\
  do							\
    {							\
      NETBSD_OS_CPP_BUILTINS_AOUT();			\
      builtin_define ("__ns32k__");			\
    }							\
  while (0)

/* Compile for the floating point unit & 32532 by default;
   Don't assume SB is zero;
   Don't use bit-field instructions;
   FPU is 32381;
   Use multiply-add instructions */

#undef TARGET_DEFAULT
#define TARGET_DEFAULT \
  (MASK_32532 | MASK_NO_SB | MASK_NO_BITFIELD | \
   MASK_32381 | MASK_IEEE_COMPARE | MASK_MULT_ADD)

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

/* Define a CPP_SPEC appropriate for NetBSD.  */

#undef CPP_SPEC
#define CPP_SPEC NETBSD_CPP_SPEC

/* Make gcc agree with <machine/ansi.h> */

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

/* This is BSD, so it wants DBX format.  */

#define DBX_DEBUGGING_INFO 1

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
