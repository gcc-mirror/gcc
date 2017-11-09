/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                             S I G T R A M P                              *
 *                                                                          *
 *                         Asm Implementation File                          *
 *                                                                          *
 *           Copyright (C) 2017, Free Software Foundation, Inc.             *
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

/**********************************************
 * QNX version of the __gnat_sigtramp service *
 **********************************************/

#include <ucontext.h>

#include "sigtramp.h"
/* See sigtramp.h for a general explanation of functionality.  */

extern void __gnat_sigtramp_common
  (int signo, void *siginfo, void *sigcontext,
   __sigtramphandler_t * handler);

void __gnat_sigtramp (int signo, void *si, void *sc,
                      __sigtramphandler_t * handler)
     __attribute__((optimize(2)));

void __gnat_sigtramp (int signo, void *si, void *ucontext,
                      __sigtramphandler_t * handler)
{
  struct sigcontext *mcontext = &((ucontext_t *) ucontext)->uc_mcontext;

  __gnat_sigtramp_common (signo, si, mcontext, handler);
}

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

/* Trampoline body block
   ---------------------  */

#define COMMON_CFI(REG) \
  ".cfi_offset " S(REGNO_##REG) "," S(REG_OFFSET_##REG)

#ifdef __x86_64__
/*****************************************
 *               x86-64                  *
 *****************************************/

// CFI register numbers
#define REGNO_RAX 0
#define REGNO_RDX 1
#define REGNO_RCX 2
#define REGNO_RBX 3
#define REGNO_RSI 4
#define REGNO_RDI 5
#define REGNO_RBP 6
#define REGNO_RSP 7
#define REGNO_R8 8
#define REGNO_R9 9
#define REGNO_R10 10
#define REGNO_R11 11
#define REGNO_R12 12
#define REGNO_R13 13
#define REGNO_R14 14
#define REGNO_R15 15 /* Used as CFA */
#define REGNO_RPC 16 /* aka %rip */

//  Registers offset from the regset structure
#define REG_OFFSET_RDI 0x00
#define REG_OFFSET_RSI 0x08
#define REG_OFFSET_RDX 0x10
#define REG_OFFSET_R10 0x18
#define REG_OFFSET_R8  0x20
#define REG_OFFSET_R9  0x28
#define REG_OFFSET_RAX 0x30
#define REG_OFFSET_RBX 0x38
#define REG_OFFSET_RBP 0x40
#define REG_OFFSET_RCX 0x48
#define REG_OFFSET_R11 0x50
#define REG_OFFSET_R12 0x58
#define REG_OFFSET_R13 0x60
#define REG_OFFSET_R14 0x68
#define REG_OFFSET_R15 0x70
#define REG_OFFSET_RPC 0x78 /* RIP */
#define REG_OFFSET_RSP 0x90

#define CFI_COMMON_REGS \
CR("# CFI for common registers\n") \
TCR(COMMON_CFI(RSP)) \
TCR(COMMON_CFI(R15)) \
TCR(COMMON_CFI(R14)) \
TCR(COMMON_CFI(R13)) \
TCR(COMMON_CFI(R12)) \
TCR(COMMON_CFI(R11)) \
TCR(COMMON_CFI(RCX)) \
TCR(COMMON_CFI(RBP)) \
TCR(COMMON_CFI(RBX)) \
TCR(COMMON_CFI(RAX)) \
TCR(COMMON_CFI(R9)) \
TCR(COMMON_CFI(R8)) \
TCR(COMMON_CFI(R10)) \
TCR(COMMON_CFI(RSI)) \
TCR(COMMON_CFI(RDI)) \
TCR(COMMON_CFI(RDX)) \
TCR(COMMON_CFI(RPC)) \
TCR(".cfi_return_column " S(REGNO_RPC))

#define SIGTRAMP_BODY     \
TCR(".cfi_def_cfa 15, 0") \
CFI_COMMON_REGS \
CR("") \
TCR("# Allocate frame and save the non-volatile") \
TCR("# registers we're going to modify") \
TCR("subq	$8, %rsp") \
TCR("# Setup CFA_REG = context, which we'll retrieve as our CFA value") \
TCR("movq	%rdx, %r15") \
TCR("# Call the real handler. The signo, siginfo and sigcontext") \
TCR("# arguments are the same as those we received") \
TCR("call	*%rcx") \
TCR("# This part should never be executed") \
TCR("addq	$8, %rsp") \
TCR("ret")
#endif

#ifdef __aarch64__
/*****************************************
 *               Aarch64                 *
 *****************************************/

/* CFA reg: any callee saved register will do */
#define CFA_REG  19

/* General purpose registers */
#define REG_OFFSET_GR(n)     (n * 8)
#define REGNO_GR(n)   n

/* point to the ELR value of the mcontext registers list */
#define REG_OFFSET_ELR           (32 * 8)
#define REGNO_PC      30

#define CFI_DEF_CFA \
  TCR(".cfi_def_cfa " S(CFA_REG) ", 0")

#define CFI_COMMON_REGS \
  CR("# CFI for common registers\n") \
  TCR(COMMON_CFI(GR(0)))  \
  TCR(COMMON_CFI(GR(1)))  \
  TCR(COMMON_CFI(GR(2)))  \
  TCR(COMMON_CFI(GR(3)))  \
  TCR(COMMON_CFI(GR(4)))  \
  TCR(COMMON_CFI(GR(5)))  \
  TCR(COMMON_CFI(GR(6)))  \
  TCR(COMMON_CFI(GR(7)))  \
  TCR(COMMON_CFI(GR(8)))  \
  TCR(COMMON_CFI(GR(9)))  \
  TCR(COMMON_CFI(GR(10))) \
  TCR(COMMON_CFI(GR(11))) \
  TCR(COMMON_CFI(GR(12))) \
  TCR(COMMON_CFI(GR(13))) \
  TCR(COMMON_CFI(GR(14))) \
  TCR(COMMON_CFI(GR(15))) \
  TCR(COMMON_CFI(GR(16))) \
  TCR(COMMON_CFI(GR(17))) \
  TCR(COMMON_CFI(GR(18))) \
  TCR(COMMON_CFI(GR(19))) \
  TCR(COMMON_CFI(GR(20))) \
  TCR(COMMON_CFI(GR(21))) \
  TCR(COMMON_CFI(GR(22))) \
  TCR(COMMON_CFI(GR(23))) \
  TCR(COMMON_CFI(GR(24))) \
  TCR(COMMON_CFI(GR(25))) \
  TCR(COMMON_CFI(GR(26))) \
  TCR(COMMON_CFI(GR(27))) \
  TCR(COMMON_CFI(GR(28))) \
  TCR(COMMON_CFI(GR(29))) \
  TCR(".cfi_offset " S(REGNO_PC) "," S(REG_OFFSET_ELR)) \
  TCR(".cfi_return_column " S(REGNO_PC))

#define SIGTRAMP_BODY \
  CFI_DEF_CFA \
  CFI_COMMON_REGS \
  TCR("# Push FP and LR on stack") \
  TCR("stp x29, x30, [sp, #-16]!") \
  TCR("# Push CFA register on stack") \
  TCR("str x" S(CFA_REG) ", [sp, #-8]!" \
  TCR("# Set the CFA register to x2 value") \
  TCR("mov x" S(CFA_REG) ", x2") \
  TCR("# Call the handler") \
  TCR("blr x3") \
  TCR("# Release our frame and return (should never get here!).") \
  TCR("ldr x" S(CFA_REG) " , [sp], 8") \
  TCR("ldp x29, x30, [sp], 16") \
  TCR("ret")

#endif /* AARCH64 */

/* Symbol definition block
   -----------------------  */

#if defined (__x86_64__) || defined (__aarch64__)
#define FUNC_ALIGN TCR(".p2align 4,,15")
#else
#define FUNC_ALIGN
#endif

#define SIGTRAMP_START(SYM)       \
CR("# " S(SYM) " cfi trampoline") \
TCR(".type " S(SYM) ", @function") \
CR("") \
FUNC_ALIGN \
CR(S(SYM) ":") \
TCR(".cfi_startproc") \
TCR(".cfi_signal_frame")

/* Symbol termination block
   ------------------------  */

#define SIGTRAMP_END(SYM) \
CR(".cfi_endproc") \
TCR(".size " S(SYM) ", .-" S(SYM))

/*----------------------------
  -- And now, the real code --
  ---------------------------- */

/* Text section start.  The compiler isn't aware of that switch.  */

asm (".text\n"
     TCR(".align 2"));

/* sigtramp stub for common registers.  */

#define TRAMP_COMMON __gnat_sigtramp_common

asm (SIGTRAMP_START(TRAMP_COMMON));
asm (SIGTRAMP_BODY);
asm (SIGTRAMP_END(TRAMP_COMMON));
