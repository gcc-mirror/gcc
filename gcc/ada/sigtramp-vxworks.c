/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                             S I G T R A M P                              *
 *                                                                          *
 *                         Asm Implementation File                          *
 *                                                                          *
 *         Copyright (C) 2011-2021, Free Software Foundation, Inc.          *
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

/**************************************************
 * VxWorks version of the __gnat_sigtramp service *
 **************************************************/

#include "sigtramp.h"
/* See sigtramp.h for a general explanation of functionality.  */

#include <vxWorks.h>
#include <arch/../regs.h>
#ifndef __RTP__
#if defined(__i386__)
#include <version.h>
#endif
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

#define TRAMP_COMMON __gnat_sigtramp_common

/* sigtramp stub providing CFI info for common registers.  */

extern void
TRAMP_COMMON (int signo, void *siginfo, void *sigcontext,
              __sigtramphandler_t * handler, REG_SET * sc_pregs);

/* -------------------------------------
   -- Common interface implementation --
   -------------------------------------

   We enforce optimization to minimize the overhead of the extra layer.  */

#if defined(__vxworks) && (defined (__i386__) || defined (__x86_64__)) && !defined (VTHREADS)
static int __gnat_is_vxsim = 0;

void __gnat_set_is_vxsim(int val) {
  __gnat_is_vxsim = val;
}
#endif

void __gnat_sigtramp (int signo, void *si, void *sc,
		      __sigtramphandler_t * handler)
     __attribute__((optimize(2)));

void __gnat_sigtramp (int signo, void *si, void *sc,
		      __sigtramphandler_t * handler)
{
  REG_SET *pregs;

  /* VXSIM uses a different signal context structure than the regular x86
     targets:
     * on x86-vx6: two 32-bit values are added at the end of the REG_SET, plus
       an explicit padding of 0xc8 characters (200 characters). The sigcontext
       containing a complete REG_SET just before the field 'sc_pregs', this
       adds a 208 bytes offset to get the value of 'sc_pregs'.
     * on x86-vx7: the same offset is used on vx7: 3 32-bit values are present
       at the end of the reg set, but the padding is then of 0xc4 characters.
     * on x86_64-vx7: two 64-bit values are added at the beginning of the
       REG_SET. This adds a 16 bytes offset to get the value of 'sc_pregs',
       and another 16 bytes offset within the pregs structure to retrieve the
       registers list.

     * See header file regsSimlinux.h.
  */

  /* Retrieve the registers to restore : */
#ifndef __RTP__
#ifdef __HANDLE_VXSIM_SC
#if defined(__i386__)
  /* move sctx 208 bytes further, so that the vxsim's sc_pregs field coincide
     with the expected x86 one */
  struct sigcontext * sctx =
    (struct sigcontext *) (sc + (__gnat_is_vxsim ?
				 (_WRS_VXWORKS_MAJOR == 7 ? 204 : 208)
				 : 0));
#elif defined(__x86_64__)
  /* move sctx 16 bytes further, so that the vxsim's sc_pregs field coincide
     with the expected x86_64 one */
  struct sigcontext * sctx =
    (struct sigcontext *) (sc + (__gnat_is_vxsim ? 16 : 0));
#endif /* __i386__ || __x86_64__ */
#else  /* __HANDLE_VXSIM_SC__ */
  struct sigcontext * sctx = (struct sigcontext *) sc;
#endif

  pregs = sctx->sc_pregs;

#else /* !defined(__RTP__) */

  mcontext_t *mcontext = &((ucontext_t *) sc)->uc_mcontext;
  /* No specific offset in this case for vxsim */
  pregs = &(mcontext->regs);

#endif /* !defined(__RTP__) */

#if defined (__HANDLE_VXSIM_SC) && defined (__x86_64__)
  /* Ignore the first two values, that are not registers in case of
     vxsim */
  pregs = (REG_SET *) ((void *)pregs + (__gnat_is_vxsim ? 16 : 0));
#endif

  /* And now call the real signal trampoline with the list of registers */
  __gnat_sigtramp_common (signo, si, sc, handler, pregs);
}

/* Include the target specific bits.  */
#include "sigtramp-vxworks-target.h"

/* sigtramp stub for common registers.  */

asm (SIGTRAMP_START(TRAMP_COMMON));
asm (CFI_DEF_CFA);
asm (CFI_COMMON_REGS);
asm (SIGTRAMP_BODY);
asm (SIGTRAMP_END(TRAMP_COMMON));
