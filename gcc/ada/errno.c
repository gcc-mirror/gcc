/****************************************************************************
 *                                                                          *
 *                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 *
 *                                                                          *
 *                                E R R N O                                 *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *           Copyright (C) 1992-2005, Free Software Foundation, Inc.        *
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

/* This file provides access to the C-language errno to the Ada interface
   for POSIX.  It is not possible in general to import errno, even in
   Ada compilers that allow (as GNAT does) the importation of variables,
   as it may be defined using a macro.
*/


#define _REENTRANT
#define _THREAD_SAFE
#define _SGI_MP_SOURCE

#include <errno.h>
int
__get_errno(void)
{
  return errno;
}

void
__set_errno(int err)
{
  errno = err;
}
