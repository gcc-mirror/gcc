/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                             S I G T R A M P                              *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *            Copyright (C) 2011, Free Software Foundation, Inc.            *
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

/* On targets where this is implemented, we resort to a signal handler
   trampoline to set-up the DWARF Call Frame Information that let unwinders
   walk through the signal frame up into the interrupted application code.
   This file introduces the relevant declarations.  */

/* This file should only be #included on targets that do implement the
   trampoline, which needs to expose the following interface:  */

#ifdef __cplusplus
extern "C" {
#endif

  typedef void sighandler_t (int signo, void *siginfo, void *sigcontext);

  void __gnat_sigtramp  (int signo, void *siginfo, void *sigcontext,
			 sighandler_t * handler);

  /* To be called from an established signal handler.  Setup the DWARF CFI
     bits letting unwinders walk through the signal frame up into the
     interrupted application code, and then call HANDLER (SIGNO, SIGINFO,
     SIGCONTEXT).  */

#ifdef __cplusplus
}
#endif
