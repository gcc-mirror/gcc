/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                             S I G T R A M P                              *
 *                                                                          *
 *                         Asm Implementation File                          *
 *                                                                          *
 *         Copyright (C) 2011-2025, Free Software Foundation, Inc.          *
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
 * ARM-QNX version of the __gnat_sigtramp service *
 **************************************************/

#include <signal.h>
#include <ucontext.h>

#include "sigtramp.h"
/* See sigtramp.h for a general explanation of functionality.  */

/* -------------------------------------------
   -- Prototypes for our internal asm stubs --
   -------------------------------------------

   Eventhough our symbols will remain local, the prototype claims "extern"
   and not "static" to prevent compiler complaints about a symbol used but
   never defined.  */

/* sigtramp stub providing ARM unwinding info for common registers.  */

extern void __gnat_sigtramp_common
(int signo, void *siginfo, void *sigcontext,
 __sigtramphandler_t * handler, void * sc_pregs);

/* -------------------------------------
   -- Common interface implementation --
   -------------------------------------

   We enforce optimization to minimize the overhead of the extra layer.  */

void __gnat_sigtramp (int signo, void *si, void *sc,
		      __sigtramphandler_t * handler)
     __attribute__((optimize(2)));

void __gnat_sigtramp (int signo, void *si, void *sc,
		      __sigtramphandler_t * handler)
{
  mcontext_t *mcontext = &((ucontext_t *) sc)->uc_mcontext;

  /* Pass MCONTEXT in the fifth position so that the assembly code can find
     it at the same stack location as SC_PREGS.  */
  __gnat_sigtramp_common (signo, si, mcontext, handler, &mcontext->cpu);
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

/* The 5 arguments passed to __gnat_sigtramp_common are located in:
   - r0-r2: arguments to pass on to the actual handler
   - r3: the actual handler
   - sp: the address of the reg set to restore
   All we have to do then is to instruct the unwinder to restore the registers
   from the value in VSP. Unwinder instructions are executed backwards, so we
   1- instruct to pop r2 from the VSP (.save {r2})
   2- move the VSP to the address pointed to by r2 (.movsp r2)
   3- restore all registers from there. (.save {r0-r15})
   Once the unwinding instructions are set, we just need to call the handler
   as r0-r2 are already properly set.
*/
#define SIGTRAMP_BODY \
CR("") \
TCR(".save {r0-r15}") \
TCR(".movsp r2") \
TCR(".save {r2}") \
TCR("blx	r3") \
TCR("# No return here.")

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
