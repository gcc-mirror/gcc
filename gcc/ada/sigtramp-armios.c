/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                             S I G T R A M P                              *
 *                                                                          *
 *                         Asm Implementation File                          *
 *                                                                          *
 *           Copyright (C) 2015, Free Software Foundation, Inc.             *
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
 * ARM-IOS version of the __gnat_sigtramp service *
 **************************************************/

#include <sys/ucontext.h>

#include "sigtramp.h"
/* See sigtramp.h for a general explanation of functionality.  */

/* -----------------------------------------
   -- Protypes for our internal asm stubs --
   -----------------------------------------

   The registers are expected to be at SIGCONTEXT + OFFSET (reference to the
   machine context structure).  Even though our symbols will remain local, the
   prototype claims "extern" and not "static" to prevent compiler complaints
   about a symbol used but never defined.  */

/* sigtramp stub providing unwind info for common registers.  */

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

asm("\n"
"	.section	__TEXT,__text,regular,pure_instructions\n"
"	.align  2\n"
"___gnat_sigtramp_common:\n"
"	.cfi_startproc\n"
	/* Restore callee saved registers.  */
"	ldp	x19, x20, [x2, #168]\n"
"	ldp	x21, x22, [x2, #184]\n"
"	ldp	x23, x24, [x2, #200]\n"
"	ldp	x25, x26, [x2, #216]\n"
"	ldp	x27, x28, [x2, #232]\n"
"	ldp	q8, q9, [x2, #416]\n"
"	ldp	q10, q11, [x2, #448]\n"
"	ldp	q12, q13, [x2, #480]\n"
"	ldp	q14, q15, [x2, #512]\n"
	/* Read FP from mcontext.  */
"	ldr	fp, [x2, #248]\n"
	/* Read SP and PC from mcontext.  */
"	ldp	x6, lr, [x2, #264]\n"
"	mov	sp, x6\n"
	/* Create a minimal frame.  */
"	stp	fp, lr, [sp, #-16]!\n"
"	.cfi_def_cfa_offset 16\n"
"	.cfi_offset	30, -8\n"
"	.cfi_offset	29, -16\n"
"	blr	x3\n"
	/* Release our frame and return (should never get here!).  */
"	ldp	fp, lr, [sp, #16]\n"
"	ret\n"
"	.cfi_endproc\n"
);
