/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                             S I G T R A M P                              *
 *                                                                          *
 *                         Asm Implementation File                          *
 *                                                                          *
 *           Copyright (C) 2015-2024, Free Software Foundation, Inc.        *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * In particular,  you can freely  distribute your programs  built with the *
 * GNAT Pro compiler, including any required library run-time units,  using *
 * any licensing terms  of your choosing.  See the AdaCore Software License *
 * for full details.                                                        *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/****************************************************
 * ARM64/IOS version of the __gnat_sigtramp service *
 ****************************************************/

#include <sys/ucontext.h>

#include "sigtramp.h"
/* See sigtramp.h for a general explanation of functionality.  */

/* ----------------------
   -- General comments --
   ----------------------

   Unfortunately the libunwind library used on this platform comes with severe
   limitations that make the implementation convoluted:

     1. At each step, the stack pointer register SP is restored with the CFA.
	This makes it impossible to set the CFA to an arbitrary value, for
	example to the address of the context saved on the stack, which means
	that the simple CFI directives cannot be used for the registers.

     2. For the ARM64 architecture (and only it), DWARF expressions are not
	supported to compute the CFA.  Only DW_CFA_def_cfa is supported, which
	means that the CFA (modulo offset) must be loaded into a register.

     3. The return column cannot be changed (30 for the ARM64 architecture).
	Since column 30 is that of the LR register, this makes it impossible
	to restore both the LR register and the PC.

   Therefore we need 2 distinct call-saved registers in the trampoline and
   we resort to manual encoding of CFI byte sequences.  */

/* -----------------------------------------
   -- Protypes for our internal asm stubs --
   -----------------------------------------

   Even though our symbols will remain local, the prototype claims "extern"
   and not "static" to prevent compiler complaints about a symbol used but
   never defined.  */

/* sigtramp stub providing unwind info for common registers.  */

#if defined(__cplusplus)
extern "C" {
#endif

extern void __gnat_sigtramp_common
  (int signo, void *siginfo, void *sigcontext,
   __sigtramphandler_t * handler);

void __gnat_sigtramp (int signo, void *si, void *ucontext,
                      __sigtramphandler_t * handler)
     __attribute__((optimize(2)));

void __gnat_sigtramp (int signo, void *si, void *ucontext,
                      __sigtramphandler_t * handler)
{
  mcontext_t mcontext = ((ucontext_t *) ucontext)->uc_mcontext;

  __gnat_sigtramp_common (signo, si, mcontext, handler);
}

#if defined(__cplusplus)
}
#endif

/* asm string construction helpers.  */

#define STR(TEXT) #TEXT
/* stringify expanded TEXT, surrounding it with double quotes.  */

#define S(E) STR(E)
/* stringify E, which will resolve as text but may contain macros
   still to be expanded.  */

/* asm (TEXT) outputs <tab>TEXT. These facilitate the output of
   multiline contents:  */
#define TAB(S) "\t" S
#define CR(S)  S "\n"

#undef TCR
#define TCR(S) TAB(CR(S))

/* Offset in uc_mcontext of the __ss structure containing the registers.  */
#define UC_MCONTEXT_SS 16

#define CFA_REG  19
#define BASE_REG 20

#define DW_CFA_def_cfa    0x0c
#define DW_CFA_expression 0x10

#define DW_OP_breg(n)     0x70+(n)

#define REG_REGNO_GR(n)   n
#define REG_REGNO_PC      30

/* The first byte of the SLEB128 value of the offset.  */
#define REG_OFFSET_GR(n)         (UC_MCONTEXT_SS + n * 8)
#define REG_OFFSET_LONG_GR(n)    (UC_MCONTEXT_SS + n * 8 + 128)
#define REG_OFFSET_LONG128_GR(n) (UC_MCONTEXT_SS + (n - 16) * 8 + 128)
#define REG_OFFSET_LONG256_GR(n) (UC_MCONTEXT_SS + (n - 32) * 8 + 128)

#define REG_OFFSET_LONG256_PC    REG_OFFSET_LONG256_GR(32)

#define CFI_DEF_CFA \
  TCR(".cfi_def_cfa " S(CFA_REG) ", 0")

/* We need 4 variants depending on the offset: 0+, 64+, 128+, 256+.  */
#define COMMON_CFI(REG) \
  ".cfi_escape " S(DW_CFA_expression) "," S(REG_REGNO_##REG) ",2," \
  S(DW_OP_breg(BASE_REG)) "," S(REG_OFFSET_##REG)

#define COMMON_LONG_CFI(REG) \
  ".cfi_escape " S(DW_CFA_expression) "," S(REG_REGNO_##REG) ",3," \
  S(DW_OP_breg(BASE_REG)) "," S(REG_OFFSET_LONG_##REG) ",0"

#define COMMON_LONG128_CFI(REG) \
  ".cfi_escape " S(DW_CFA_expression) "," S(REG_REGNO_##REG) ",3," \
  S(DW_OP_breg(BASE_REG)) "," S(REG_OFFSET_LONG128_##REG) ",1"

#define COMMON_LONG256_CFI(REG) \
  ".cfi_escape " S(DW_CFA_expression) "," S(REG_REGNO_##REG) ",3," \
  S(DW_OP_breg(BASE_REG)) "," S(REG_OFFSET_LONG256_##REG) ",2"

#define CFI_COMMON_REGS \
  CR("# CFI for common registers\n") \
  TCR(COMMON_CFI(GR(0)))  \
  TCR(COMMON_CFI(GR(1)))  \
  TCR(COMMON_CFI(GR(2)))  \
  TCR(COMMON_CFI(GR(3)))  \
  TCR(COMMON_CFI(GR(4)))  \
  TCR(COMMON_CFI(GR(5)))  \
  TCR(COMMON_LONG_CFI(GR(6)))  \
  TCR(COMMON_LONG_CFI(GR(7)))  \
  TCR(COMMON_LONG_CFI(GR(8)))  \
  TCR(COMMON_LONG_CFI(GR(9)))  \
  TCR(COMMON_LONG_CFI(GR(10))) \
  TCR(COMMON_LONG_CFI(GR(11))) \
  TCR(COMMON_LONG_CFI(GR(12))) \
  TCR(COMMON_LONG_CFI(GR(13))) \
  TCR(COMMON_LONG128_CFI(GR(14))) \
  TCR(COMMON_LONG128_CFI(GR(15))) \
  TCR(COMMON_LONG128_CFI(GR(16))) \
  TCR(COMMON_LONG128_CFI(GR(17))) \
  TCR(COMMON_LONG128_CFI(GR(18))) \
  TCR(COMMON_LONG128_CFI(GR(19))) \
  TCR(COMMON_LONG128_CFI(GR(20))) \
  TCR(COMMON_LONG128_CFI(GR(21))) \
  TCR(COMMON_LONG128_CFI(GR(22))) \
  TCR(COMMON_LONG128_CFI(GR(23))) \
  TCR(COMMON_LONG128_CFI(GR(24))) \
  TCR(COMMON_LONG128_CFI(GR(25))) \
  TCR(COMMON_LONG128_CFI(GR(26))) \
  TCR(COMMON_LONG128_CFI(GR(27))) \
  TCR(COMMON_LONG128_CFI(GR(28))) \
  TCR(COMMON_LONG128_CFI(GR(29))) \
  TCR(COMMON_LONG256_CFI(PC))

/* Trampoline body block
   ---------------------  */

#define SIGTRAMP_BODY \
  TCR("stp fp, lr, [sp, #-32]!") \
  TCR("stp x" S(CFA_REG) ", x" S(BASE_REG) ", [sp, #16]") \
  TCR("mov fp, sp") \
  TCR("# Load the saved value of the stack pointer as CFA") \
  TCR("ldr x" S(CFA_REG) ", [x2, #" S(REG_OFFSET_GR(31)) "]") \
  TCR("# Use x" S(BASE_REG) " as base register for the CFI") \
  TCR("mov x" S(BASE_REG) ", x2") \
  TCR("# Call the handler") \
  TCR("blr x3") \
  TCR("# Release our frame and return (should never get here!).") \
  TCR("ldp x" S(CFA_REG) ", x" S(BASE_REG)" , [sp, #16]") \
  TCR("ldp fp, lr, [sp], 32") \
  TCR("ret")

/* -----------------------------
   -- Symbol definition block --
   ----------------------------- */

#define SIGTRAMP_START(SYM) \
  CR("# " S(SYM) " signal trampoline") \
  CR(S(SYM) ":") \
  TCR(".cfi_startproc") \
  TCR(".cfi_signal_frame")

/* ------------------------------
   -- Symbol termination block --
   ------------------------------ */

#define SIGTRAMP_END(SYM) \
  TCR(".cfi_endproc")

/*----------------------------
  -- And now, the real code --
  ---------------------------- */

asm(".text\n"
    TCR(".align 2"));

/* sigtramp stub for common registers.  */

#define TRAMP_COMMON ___gnat_sigtramp_common

asm (SIGTRAMP_START(TRAMP_COMMON));
asm (CFI_DEF_CFA);
asm (CFI_COMMON_REGS);
asm (SIGTRAMP_BODY);
asm (SIGTRAMP_END(TRAMP_COMMON));
