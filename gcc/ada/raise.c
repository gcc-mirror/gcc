/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                R A I S E                                 *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *             Copyright (C) 1992-2005, Free Software Foundation, Inc.      *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, *
 * Boston, MA 02110-1301, USA.                                              *
 *                                                                          *
 * As a  special  exception,  if you  link  this file  with other  files to *
 * produce an executable,  this file does not by itself cause the resulting *
 * executable to be covered by the GNU General Public License. This except- *
 * ion does not  however invalidate  any other reasons  why the  executable *
 * file might be covered by the  GNU Public License.                        *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* Shared routines to support exception handling.
   Note that _gnat_builtin_longjmp should disappear at some point, replaced
   by direct call to __builtin_longjmp from Ada code.
   __gnat_unhandled_terminate is code shared between all exception handling
   mechanisms */

#ifdef IN_RTS
#include "tconfig.h"
#include "tsystem.h"
#else
#include "config.h"
#include "system.h"
#endif

#include "adaint.h"
#include "raise.h"

/*  We have not yet figured out how to import this directly */

void
_gnat_builtin_longjmp (void *ptr, int flag ATTRIBUTE_UNUSED)
{
   __builtin_longjmp (ptr, 1);
}

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
