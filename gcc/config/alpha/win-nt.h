/* Definitions of target machine for GNU compiler, for DEC Alpha
   running Windows/NT.
   Copyright (C) 1995, 1996 Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@vlsi1.ultra.nyu.edu)

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

/* Names to predefine in the preprocessor for this target machine.  */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-DWIN32 -D_WIN32 -DWINNT -D__STDC__=0 -DALMOST_STDC\
  -D_M_ALPHA -D_ALPHA_ -D__alpha -D__alpha__\
  -D_LONGLONG -D__unaligned= -D__stdcall= \
  -Asystem(winnt) -Acpu(alpha) -Amachine(alpha)"

#undef ASM_SPEC
#undef ASM_FINAL_SPEC
#define ASM_SPEC "-nopp -nologo %{g:-Zi}"

/* Pointer is 32 bits but the hardware has 64-bit addresses, sign extended. */
#undef POINTER_SIZE
#define POINTER_SIZE 32
#define POINTERS_EXTEND_UNSIGNED 0

/* "long" is 32 bits.  */
#undef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE 32

#undef WCHAR_TYPE
#define WCHAR_TYPE "short unsigned int"
#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 16

/* We can't do any debugging.  */
#undef SDB_DEBUGGING_INFO
#undef DBX_DEBUGGING_INFO
#undef MIPS_DEBUGGING_INFO

#include "winnt/win-nt.h"

#undef ASM_FILE_START
#define ASM_FILE_START(FILE)					\
{								\
  alpha_write_verstamp (FILE);					\
  fprintf (FILE, "\t.set noreorder\n");				\
  fprintf (FILE, "\t.set volatile\n");                          \
  fprintf (FILE, "\t.set noat\n");				\
  fprintf (FILE, "\t.globl\t__fltused\n");			\
  ASM_OUTPUT_SOURCE_FILENAME (FILE, main_input_filename);	\
}

#undef LIB_SPEC
#define LIB_SPEC "%{mwindows:-subsystem windows -e _WinMainCRTStartup \
  USER32.LIB%s GDI32.LIB%s COMDLG32.LIB%s WINSPOOL.LIB%s} \
 %{!mwindows:-subsystem console -e _mainCRTStartup} \
 %{mcrtmt:LIBCMT.LIB%s KERNEL32.LIB%s} %{!mcrtmt:LIBC.LIB%s KERNEL32.LIB%s} \
 %{v}"


/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.

   The trampoline should set the static chain pointer to value placed
   into the trampoline and should branch to the specified routine.  */

#undef TRAMPOLINE_TEMPLATE
#define TRAMPOLINE_TEMPLATE(FILE)			\
{							\
  fprintf (FILE, "\tbr $27,$LTRAMPP\n");		\
  fprintf (FILE, "$LTRAMPP:\n\tldl $1,16($27)\n");	\
  fprintf (FILE, "\tldl $27,12($27)\n");		\
  fprintf (FILE, "\tjmp $31,($27),0\n");		\
  fprintf (FILE, "\t.long 0,0\n");			\
}

/* Length in units of the trampoline for entering a nested function.  */

#undef TRAMPOLINE_SIZE
#define TRAMPOLINE_SIZE    24

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function. 

   This differs from the standard version in that:

   We are not passed the current address in any register, and so have to 
   load it ourselves.

   We do not initialize the "hint" field because it only has an 8k
   range and so the target is in range of something on the stack. 
   Omitting the hint saves a bogus branch-prediction cache line load.

   Always have an executable stack -- no need for a system call.
 */

#undef INITIALIZE_TRAMPOLINE
#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)			\
{									\
  rtx _addr, _val;							\
									\
  _addr = memory_address (Pmode, plus_constant ((TRAMP), 16));		\
  _val = force_reg(Pmode, (FNADDR));					\
  emit_move_insn (gen_rtx (MEM, SImode, _addr),				\
		  gen_rtx (SUBREG, SImode, _val, 0));			\
  _addr = memory_address (Pmode, plus_constant ((TRAMP), 20));		\
  _val = force_reg(Pmode, (CXT));					\
  emit_move_insn (gen_rtx (MEM, SImode, _addr),				\
		  gen_rtx (SUBREG, SImode, _val, 0));			\
									\
  emit_insn (gen_rtx (UNSPEC_VOLATILE, VOIDmode,			\
                      gen_rtvec (1, const0_rtx), 0));			\
}
