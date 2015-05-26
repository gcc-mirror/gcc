/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                             S I G T R A M P                              *
 *                                                                          *
 *                         Asm Implementation File                          *
 *                                                                          *
 *         Copyright (C) 2011-2015, Free Software Foundation, Inc.          *
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

/********************************************************
 * VxWorks VXSIM version of the __gnat_sigtramp service *
 ********************************************************/

#undef CPU
#define CPU __VXSIM_CPU__

#include "sigtramp.h"
/* See sigtramp.h for a general explanation of functionality.  */

#include <vxWorks.h>
#include <arch/../regs.h>
#ifndef __RTP__
#include <sigLib.h>
#else
#include <signal.h>
#include <regs.h>

typedef struct mcontext
  {
    REG_SET     regs;
  } mcontext_t;

typedef struct ucontext
  {
    mcontext_t          uc_mcontext;    /* register set */
    struct ucontext *   uc_link;        /* not used */
    sigset_t            uc_sigmask;     /* set of signals blocked */
    stack_t             uc_stack;       /* stack of context signaled */
  } ucontext_t;
#endif

/* ----------------------
   -- General comments --
   ----------------------

   Stubs are generated from toplevel asms and .cfi directives, much simpler
   to use and check for correctness than manual encodings of CFI byte
   sequences.  The general idea is to establish CFA as sigcontext->sc_pregs
   (for DKM) and mcontext (for RTP) and state where to find the registers as
   offsets from there.

   As of today, we support a stub providing CFI info for common
   registers (GPRs, LR, ...). We might need variants with support for floating
   point or altivec registers as well at some point.

   Checking which variant should apply and getting at sc_pregs / mcontext
   is simpler to express in C (we can't use offsetof in toplevel asms and
   hardcoding constants is not workable with the flurry of VxWorks variants),
   so this is the choice for our toplevel interface.

   Note that the registers we "restore" here are those to which we have
   direct access through the system sigcontext structure, which includes
   only a partial set of the non-volatiles ABI-wise.  */

/* -------------------------------------------
   -- Prototypes for our internal asm stubs --
   -------------------------------------------

   Eventhough our symbols will remain local, the prototype claims "extern"
   and not "static" to prevent compiler complaints about a symbol used but
   never defined.  */

/* sigtramp stub providing CFI info for common registers.  */

extern void __gnat_sigtramp_vxsim_common
(int signo, void *siginfo, void *sigcontext,
 __sigtramphandler_t * handler, void * sc_pregs);


/* -------------------------------------
   -- Common interface implementation --
   -------------------------------------

   We enforce optimization to minimize the overhead of the extra layer.  */

void __gnat_sigtramp_vxsim (int signo, void *si, void *sc,
		      __sigtramphandler_t * handler)
     __attribute__((optimize(2)));

void __gnat_sigtramp_vxsim (int signo, void *si, void *sc,
		      __sigtramphandler_t * handler)
{
#ifdef __RTP__
  mcontext_t *mcontext = &((ucontext_t *) sc)->uc_mcontext;

  /* Pass MCONTEXT in the fifth position so that the assembly code can find
     it at the same stack location or in the same register as SC_PREGS.  */
  __gnat_sigtramp_vxsim_common (signo, si, mcontext, handler, mcontext);
#else
  struct sigcontext * sctx = (struct sigcontext *) sc;

  __gnat_sigtramp_vxsim_common (signo, si, sctx, handler, sctx->sc_pregs);
#endif
}

/* Include the target specific bits.  */
#include "sigtramp-vxworks-target.inc"

/* sigtramp stub for common registers.  */

#define TRAMP_COMMON __gnat_sigtramp_vxsim_common

asm (SIGTRAMP_START(TRAMP_COMMON));
asm (CFI_DEF_CFA);
asm (CFI_COMMON_REGS);
asm (SIGTRAMP_BODY);
asm (SIGTRAMP_END(TRAMP_COMMON));


