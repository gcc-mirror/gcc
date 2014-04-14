/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                             S I G T R A M P                              *
 *                                                                          *
 *                         Asm Implementation File                          *
 *                                                                          *
 *           Copyright (C) 2014, Free Software Foundation, Inc.             *
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
 * ARM-Android version of the __gnat_sigtramp service *
 ******************************************************/

#include "sigtramp.h"
/* See sigtramp.h for a general explanation of functionality.  */

/* ----------------------
   -- General comments --
   ----------------------

   Stubs are generated from toplevel asms,
   The general idea is to establish CFA as the sigcontext
   and state where to find the registers as offsets from there.

   We support stubs for VxWorks and Android, providing unwind info for
   common registers. We might need variants with support for floating
   point or altivec registers as well at some point.

   For Android it would be simpler to write this in Asm since there's only
   one variant, but to keep it looking like the VxWorks stubs,
   C is the choice for our toplevel interface.

   Note that the registers we "restore" here are those to which we have
   direct access through the system sigcontext structure, which includes
   only a partial set of the non-volatiles ABI-wise.  */

/* -----------------------------------------
   -- Protypes for our internal asm stubs --
   -----------------------------------------

   The registers are expected to be at SIGCONTEXT + 12 (reference the
   sicontext structure in asm/sigcontext.h which describes the first
   3 * 4byte fields.)  Even though our symbols will remain local, the
   prototype claims "extern" and not "static" to prevent compiler complaints
   about a symbol used but never defined.  */

/* sigtramp stub providing unwind info for common registers.  */

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

#define SIGTRAMP_BODY \
CR("") \
TCR("# Allocate frame and also save r2 which is the argument register") \
TCR("# containing the sigcontext, so that we can restore it during") \
TCR("# unwinding and thereby load the rest of the desired context.") \
TCR("stmfd	sp!, {r2, r3, lr}") \
TCR("# The unwinder undo's these operations in reverse order so starting") \
TCR("# from bottom, restore r2 from the current vsp location, move r2 into") \
TCR("# the vsp, add 12 bytes to get the start of the register save area") \
TCR("# then restore the 15 general purpose registers of the frame which") \
TCR("# raised the signal.") \
TCR(".save {r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15}") \
TCR(".pad #12") \
TCR(".movsp r2") \
TCR(".save {r2}") \
TCR("# Call the real handler. The signo, siginfo and sigcontext") \
TCR("# arguments are the same as those we received in r0, r1 and r2.") \
TCR("blx	r3") \
TCR("# Restore our callee-saved items, release our frame and return") \
TCR("# (should never get here!).") \
TCR("ldmfd	sp, {r2, r3, pc}")

/* Symbol definition block
   -----------------------  */

#define SIGTRAMP_START(SYM) \
CR("# " S(SYM) " unwind trampoline") \
TCR(".type " S(SYM) ", %function") \
CR("") \
CR(S(SYM) ":") \
TCR(".fnstart")

/* Symbol termination block
   ------------------------  */

#define SIGTRAMP_END(SYM) \
CR(".fnend") \
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
