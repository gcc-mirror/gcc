/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                             S I G T R A M P                              *
 *                                                                          *
 *                         C/Asm Implementation File                        *
 *                                                                          *
 *           Copyright (C) 2015-2025, Free Software Foundation, Inc.        *
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
 * Android version of the __gnat_sigtramp service *
 **************************************************/

#include "sigtramp.h"

/* The ARM port relies on CFI info setup here.  Others such as aarch64
   rely on kernel CFI and may relay to the handler directly.  */

#if defined(__arm__)
#define __SETUP_CFI 1
#else
#define __SETUP_CFI 0
#endif

#if __SETUP_CFI

/* Craft a sigtramp stub providing unwind info for common registers.  */

#define TRAMP_COMMON __gnat_sigtramp_common
extern void TRAMP_COMMON
  (int signo, void *siginfo, void *sigcontext,
   __sigtramphandler_t * handler);

#include <sys/ucontext.h>

void __gnat_sigtramp (int signo, void *si, void *ucontext,
                      __sigtramphandler_t * handler)
{
  struct sigcontext *mcontext = &((ucontext_t *) ucontext)->uc_mcontext;
  TRAMP_COMMON (signo, si, mcontext, handler);
}

#include <sigtramp-android-asm.h>

asm (SIGTRAMP_START(TRAMP_COMMON));
asm (SIGTRAMP_BODY);
asm (SIGTRAMP_END(TRAMP_COMMON));

#else /* !__SETUP_CFI  */

void __gnat_sigtramp (int signo, void *si, void *ucontext,
                      __sigtramphandler_t * handler)
{
  handler (signo, si, ucontext);
}

#endif
