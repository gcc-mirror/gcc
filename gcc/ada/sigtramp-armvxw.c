/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                             S I G T R A M P                              *
 *                                                                          *
 *                         Asm Implementation File                          *
 *                                                                          *
 *           Copyright (C) 2013, Free Software Foundation, Inc.             *
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

/******************************************************
 * ARM-VxWorks version of the __gnat_sigtramp service *
 ******************************************************/

#include "sigtramp.h"
/* See sigtramp.h for a general explanation of functionality.  */

#include <vxWorks.h>
#include <arch/../regs.h>
#include <sigLib.h>

/* ----------------------
   -- General comments --
   ----------------------

   Stubs are generated from toplevel asms and .cfi directives, much simpler
   to use and check for correctness than manual encodings of CFI byte
   sequences.  The general idea is to establish CFA as sigcontext->sc_pregs
   and state where to find the registers as offsets from there.

   As of today, we support a single stub, providing CFI info for common
   registers (GPRs, LR, ...). We might need variants with support for floating
   point or altivec registers as well at some point.

   Checking which variant should apply and getting at sc_pregs is simpler
   to express in C (we can't use offsetof in toplevel asms and hardcoding
   constants is not workable with the flurry of VxWorks variants), so this
   is the choice for our toplevel interface.

   Note that the registers we "restore" here are those to which we have
   direct access through the system sigcontext structure, which includes
   only a partial set of the non-volatiles ABI-wise.  */

/* -----------------------------------------
   -- Protypes for our internal asm stubs --
   -----------------------------------------

   SC_PREGS is always expected to be SIGCONTEXT->sc_pregs.  Eventhough our
   symbols will remain local, the prototype claims "extern" and not
   "static" to prevent compiler complaints about a symbol used but never
   defined.  */

/* sigtramp stub providing CFI info for common registers.  */

extern void __gnat_sigtramp_common
(int signo, void *siginfo, void *sigcontext,
 sighandler_t * handler, void * sc_pregs);


/* -------------------------------------
   -- Common interface implementation --
   -------------------------------------

   We enforce optimization to minimize the overhead of the extra layer.  */

void __gnat_sigtramp (int signo, void *si, void *sc,
		      sighandler_t * handler)
     __attribute__((optimize(2)));

void __gnat_sigtramp (int signo, void *si, void *sc,
		      sighandler_t * handler)
{
  struct sigcontext * sctx = (struct sigcontext *) sc;

  __gnat_sigtramp_common (signo, si, sctx, handler, sctx->sc_pregs);
}


/* ---------------------------
   -- And now the asm stubs --
   ---------------------------

   They all have a common structure with blocks of asm sequences queued one
   after the others.  Typically:

   SYMBOL_START

   CFI_DIRECTIVES
     CFI_DEF_CFA,
     CFI_COMMON_REGISTERS,
     ...

   STUB_BODY
     asm code to establish frame, setup the cfa reg value,
     call the real signal handler, ...

   SYMBOL_END
*/

/*--------------------------------
  -- Misc constants and helpers --
  -------------------------------- */

/* REGNO constants, dwarf column numbers for registers of interest.  */

#define REGNO_G_REG_OFFSET(N) (N)

#define REGNO_PC_OFFSET  15  /* PC_REGNUM  */

/* asm string construction helpers.  */

#define STR(TEXT) #TEXT
/* stringify expanded TEXT, surrounding it with double quotes.  */

#define S(E) STR(E)
/* stringify E, which will resolve as text but may contain macros
   still to be expanded.  */

/* asm (TEXT) outputs <tab>TEXT. These facilitate the output of
   multine contents:  */
#define TAB(S) "\t" S
#define CR(S)  S "\n"

#undef TCR
#define TCR(S) TAB(CR(S))

/*------------------------------
  -- Stub construction blocks --
  ------------------------------ */

/* CFA setup block
   ---------------
   Only non-volatile registers are suitable for a CFA base. These are the
   only ones we can expect to be able retrieve from the unwinding context
   while walking up the chain, saved by at least the bottom-most exception
   propagation services. We use r8 here and set it to the value we need
   in stub body that follows. Any of r4-r8 should work.  */

#define CFA_REG 8

#define CFI_DEF_CFA \
CR(".cfi_def_cfa " S(CFA_REG) ", 0")

/* Register location blocks
   ------------------------
   Rules to find registers of interest from the CFA. This should comprise
   all the non-volatile registers relevant to the interrupted context.

   ??? Note that r0 was excluded for consistency with the PPC version of
   this file, not sure if that's right.  */

#define COMMON_CFI(REG) \
  ".cfi_offset " S(REGNO_##REG) "," S(REG_SET_##REG)

#define CFI_COMMON_REGS \
CR("# CFI for common registers\n") \
TCR(COMMON_CFI(G_REG_OFFSET(1)))  \
TCR(COMMON_CFI(G_REG_OFFSET(2)))  \
TCR(COMMON_CFI(G_REG_OFFSET(3)))  \
TCR(COMMON_CFI(G_REG_OFFSET(4)))  \
TCR(COMMON_CFI(G_REG_OFFSET(5)))  \
TCR(COMMON_CFI(G_REG_OFFSET(6)))  \
TCR(COMMON_CFI(G_REG_OFFSET(7)))  \
TCR(COMMON_CFI(G_REG_OFFSET(8)))  \
TCR(COMMON_CFI(G_REG_OFFSET(9)))  \
TCR(COMMON_CFI(G_REG_OFFSET(10)))  \
TCR(COMMON_CFI(G_REG_OFFSET(11)))  \
TCR(COMMON_CFI(G_REG_OFFSET(12)))  \
TCR(COMMON_CFI(G_REG_OFFSET(13)))  \
TCR(COMMON_CFI(G_REG_OFFSET(14))) \
TCR(COMMON_CFI(PC_OFFSET)) \
TCR(".cfi_return_column " S(REGNO_PC_OFFSET))

/* Trampoline body block
   ---------------------  */

#define SIGTRAMP_BODY \
CR("") \
TCR("# Allocate frame and save the non-volatile") \
TCR("# registers we're going to modify") \
TCR("mov	ip, sp") \
TCR("stmfd	sp!, {r"S(CFA_REG)", fp, ip, lr, pc}") \
TCR("# Setup CFA_REG = sc_pregs, that we'll retrieve as our CFA value") \
TCR("ldr	r"S(CFA_REG)", [ip]") \
TCR("")                 \
TCR("# Call the real handler. The signo, siginfo and sigcontext") \
TCR("# arguments are the same as those we received in r0, r1 and r2") \
TCR("sub	fp, ip, #4") \
TCR("blx	r3") \
TCR("# Restore our callee-saved items, release our frame and return") \
TCR("ldmfd	sp, {r"S(CFA_REG)", fp, sp, pc}")


/* Symbol definition block
   -----------------------  */

#define SIGTRAMP_START(SYM) \
CR("# " S(SYM) " cfi trampoline") \
TCR(".type " S(SYM) ", %function") \
CR("") \
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
asm (CFI_DEF_CFA);
asm (CFI_COMMON_REGS);
asm (SIGTRAMP_BODY);
asm (SIGTRAMP_END(TRAMP_COMMON));


