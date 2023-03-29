/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                             S I G T R A M P                              *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *          Copyright (C) 2011-2023, Free Software Foundation, Inc.         *
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
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* On targets where this is implemented, we resort to a signal trampoline to
   set up the DWARF Call Frame Information that lets unwinders walk through
   the signal frame up into the interrupted user code.  This file introduces
   the relevant declarations.  It should only be #included on targets that do
   implement the signal trampoline.  */

#ifdef __cplusplus
extern "C" {
#endif

/* This typedef signature sometimes conflicts with the sighandler_t from
   system headers so call it something unique.  */
typedef void __sigtramphandler_t (int signo, void *siginfo, void *sigcontext);

/* The vxsim target has a different sigcontext structure than the one we're
   compiling the run-time with. We thus need to adjust it in this case */
#if defined(__vxworks) && (defined (__i386__) || defined (__x86_64__)) && !defined (VTHREADS)
#define __HANDLE_VXSIM_SC
extern void __gnat_set_is_vxsim(int val);
#endif

extern void __gnat_sigtramp (int signo, void *siginfo, void *sigcontext,
			     __sigtramphandler_t * handler);

/* The signal trampoline is to be called from an established signal handler.
   It sets up the DWARF CFI and calls HANDLER (SIGNO, SIGINFO, SIGCONTEXT).

   The trampoline construct makes it so that the unwinder jumps over it + the
   signal handler + the kernel frame.  For a typical backtrace from the raise
   function:

     #0  __gnat_Unwind_RaiseException
     #1  Raise_From_Signal_Handler
     #2  __gnat_map_signal
     #3  __gnat_sigtramp
     #4  __gnat_error_handler
     #5  <kernel frame>
     #6  interrupted function

   The unwinder will unwind frames 0, 1 and 2 as usual.  But the CFI of frame
   3 is set up as if the caller of frame 3 was frame 6 so, when frame 3 is
   unwound, the unwinder ends up in frame 6 directly.  It's possible to do so
   because the kernel has saved the context of frame 6 and passed it on to
   __gnat_error_handler and __gnat_sigtramp.  */

#ifdef __cplusplus
}
#endif
