/* Definitions of target machine for GNU compiler, for DEC Alpha
   running Windows/NT.
   Copyright (C) 1995, 1996, 1998, 1999 Free Software Foundation, Inc.

   Derived from code
      Contributed by Richard Kenner (kenner@vlsi1.ultra.nyu.edu)

   Donn Terry, Softway Systems, Inc.

   This file contains the code-generation stuff common to the 32-bit
   versions of the DEC/Compaq Alpha architecture.  It is shared by
   Interix and NT/Win32 ports.   It should not contain compile-time
   or run-time dependent environment values (such as compiler options
   or anything containing a file or pathname.)

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

#undef TARGET_ABI_WINDOWS_NT
#define TARGET_ABI_WINDOWS_NT 1

/* WinNT (and thus Interix) use unsigned int */
#define SIZE_TYPE "unsigned int"

/* Pointer is 32 bits but the hardware has 64-bit addresses, sign extended.  */
#undef POINTER_SIZE
#define POINTER_SIZE 32
#define POINTERS_EXTEND_UNSIGNED 0

/* We don't change Pmode to the "obvious" SI mode... the above appears
   to affect the in-memory size; we want the registers to stay DImode
   to match the md file */

/* "long" is 32 bits.  */
#undef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE 32


/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.

   The trampoline should set the static chain pointer to value placed
   into the trampoline and should branch to the specified routine.  */

#undef TRAMPOLINE_TEMPLATE
#define TRAMPOLINE_TEMPLATE(FILE)			\
{							\
  fprintf (FILE, "\tbr $27,$LTRAMPP\n");		\
  fprintf (FILE, "$LTRAMPP:\n\tldl $1,12($27)\n");	\
  fprintf (FILE, "\tldl $27,16($27)\n");		\
  fprintf (FILE, "\tjmp $31,($27),0\n");		\
  fprintf (FILE, "\t.long 0,0\n");			\
}

/* Length in units of the trampoline for entering a nested function.  */

#undef TRAMPOLINE_SIZE
#define TRAMPOLINE_SIZE    24

/* The alignment of a trampoline, in bits.  */

#undef TRAMPOLINE_ALIGNMENT
#define TRAMPOLINE_ALIGNMENT  32

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

#undef INITIALIZE_TRAMPOLINE
#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT) \
  alpha_initialize_trampoline (TRAMP, FNADDR, CXT, 20, 16, 12)
