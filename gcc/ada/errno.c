/****************************************************************************
 *                                                                          *
 *                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 *
 *                                                                          *
 *                                E R R N O                                 *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *           Copyright (C) 1992-2019, Free Software Foundation, Inc.        *
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

/* This file provides access to the C-language errno to the Ada interface
   for POSIX.  It is not possible in general to import errno, even in
   Ada compilers that allow (as GNAT does) the importation of variables,
   as it may be defined using a macro.
*/


#define _REENTRANT
#define _THREAD_SAFE

#ifdef MaRTE

/* MaRTE OS provides its own implementation of errno related functionality. We
   want to ensure the use of the MaRTE version for tasking programs (the MaRTE
   library will not be linked if no tasking constructs are used), so we use the
   weak symbols mechanism to use the MaRTE version whenever is available. */

#pragma weak __get_errno
#pragma weak __set_errno

#endif

#include <errno.h>
int
__get_errno(void)
{
  return errno;
}

#ifndef __ANDROID__
void
__set_errno(int err)
{
  errno = err;
}
#endif
