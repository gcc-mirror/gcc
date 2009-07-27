/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                R A I S E                                 *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *             Copyright (C) 1992-2009, Free Software Foundation, Inc.      *
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

/* Shared routines to support exception handling.  __gnat_unhandled_terminate
   is shared between all exception handling mechanisms.  */

#ifdef IN_RTS
#include "tconfig.h"
#include "tsystem.h"
#else
#include "config.h"
#include "system.h"
#endif

#include "adaint.h"
#include "raise.h"

/*  Wrapper to builtin_longjmp.  This is for the compiler eh only, as the sjlj
    runtime library interfaces directly to the intrinsic.  We can't yet do
    this for the compiler itself, because this capability relies on changes
    made in april 2008 and we need to preserve the possibility to bootstrap
    with an older base version.  */

#if defined (IN_GCC) && !defined (IN_RTS)
void
_gnat_builtin_longjmp (void *ptr, int flag ATTRIBUTE_UNUSED)
{
   __builtin_longjmp (ptr, 1);
}
#endif

/* When an exception is raised for which no handler exists, the procedure
   Ada.Exceptions.Unhandled_Exception is called, which performs the call to
   adafinal to complete finalization, and then prints out the error messages
   for the unhandled exception. The final step is to call this routine, which
   performs any system dependent cleanup required.  */

void
__gnat_unhandled_terminate (void)
{
#ifdef VMS
  /* Special termination handling for VMS */
  long prvhnd;

  /* Remove the exception vector so it won't intercept any errors
     in the call to exit, and go into and endless loop */

  SYS$SETEXV (1, 0, 3, &prvhnd);
#endif

  /* Default termination handling */
  __gnat_os_exit (1);
}
