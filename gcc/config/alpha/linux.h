/* Definitions of target machine for GNU compiler,
   for Alpha Linux-based GNU systems.
   Copyright (C) 1996, 1997 Free Software Foundation, Inc.
   Contributed by Richard Henderson.

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

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_FP | MASK_FPREGS | MASK_GAS)

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "\
-D__alpha -D__alpha__ -D__linux__ -D__linux -D_LONGLONG -Dlinux -Dunix \
-Asystem(linux) -Acpu(alpha) -Amachine(alpha)" SUB_CPP_PREDEFINES

#undef LIB_SPEC
#define LIB_SPEC "%{pg:-lgmon} %{pg:-lc_p} %{!pg:-lc}"

#undef ASM_SPEC
#define ASM_SPEC "-nocpp"

/* Generate calls to memcpy, etc., not bcopy, etc. */
#define TARGET_MEM_FUNCTIONS

#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO)			\
	fputs ("\tlda $28,_mcount\n\tjsr $28,($28),_mcount\n", (FILE))

/* Show that we need a GP when profiling.  */
#define TARGET_PROFILING_NEEDS_GP

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function. 

   This differs from the standard version in that:

   We do not initialize the "hint" field because it only has an 8k
   range and so the target is in range of something on the stack. 
   Omitting the hint saves a bogus branch-prediction cache line load.

   GNU/Linux always has an executable stack -- no need for a system call. */

#undef INITIALIZE_TRAMPOLINE
#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)                       \
{                                                                       \
  rtx _addr;                                             		\
                                                                        \
  _addr = memory_address (Pmode, plus_constant ((TRAMP), 16));          \
  emit_move_insn (gen_rtx (MEM, Pmode, _addr), (FNADDR));               \
  _addr = memory_address (Pmode, plus_constant ((TRAMP), 24));          \
  emit_move_insn (gen_rtx (MEM, Pmode, _addr), (CXT));                  \
                                                                        \
  emit_insn (gen_rtx (UNSPEC_VOLATILE, VOIDmode,                        \
                      gen_rtvec (1, const0_rtx), 0));                   \
}
