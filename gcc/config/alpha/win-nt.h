/* Definitions of target machine for GNU compiler, for DEC Alpha
   running Windows/NT.
   Copyright (C) 1995, 1996, 1998 Free Software Foundation, Inc.
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

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_FP | MASK_FPREGS)

#undef TARGET_WINDOWS_NT
#define TARGET_WINDOWS_NT 1

/* Names to predefine in the preprocessor for this target machine.  */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-DWIN32 -D_WIN32 -DWINNT -D__STDC__=0 -DALMOST_STDC \
-D_M_ALPHA -D_ALPHA_ -D_LONGLONG -D__unaligned= -D__stdcall= -Asystem(winnt)"

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
  fprintf (FILE, "$LTRAMPP:\n\tldl $1,12($27)\n");	\
  fprintf (FILE, "\tldl $27,16($27)\n");		\
  fprintf (FILE, "\tjmp $31,($27),0\n");		\
  fprintf (FILE, "\t.long 0,0\n");			\
}

/* Length in units of the trampoline for entering a nested function.  */

#undef TRAMPOLINE_SIZE
#define TRAMPOLINE_SIZE    24

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.   */

#undef INITIALIZE_TRAMPOLINE
#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT) \
  alpha_initialize_trampoline (TRAMP, FNADDR, CXT, 16, 20, 12)

/* Output code to add DELTA to the first argument, and then jump to FUNCTION.
   Used for C++ multiple inheritance.  */

#define ASM_OUTPUT_MI_THUNK(FILE, THUNK_FNDECL, DELTA, FUNCTION)	\
do {									\
  char *op, *fn_name = XSTR (XEXP (DECL_RTL (FUNCTION), 0), 0);		\
  int reg;								\
									\
  /* Mark end of prologue.  */						\
  output_end_prologue (FILE);						\
									\
  /* Rely on the assembler to macro expand a large delta.  */		\
  reg = aggregate_value_p (TREE_TYPE (TREE_TYPE (FUNCTION))) ? 17 : 16; \
  fprintf (FILE, "\tlda $%d,%ld($%d)\n", reg, (long)(DELTA), reg);      \       
									\
  op = "jsr";								\
  if (current_file_function_operand (XEXP (DECL_RTL (FUNCTION), 0)))	\
    op = "br";								\
  fprintf (FILE, "\t%s $31,", op);					\
  assemble_name (FILE, fn_name);					\
  fputc ('\n', FILE);							\
} while (0)
