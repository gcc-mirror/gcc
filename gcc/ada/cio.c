/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                  C I O                                   *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *                            $Revision: 1.2 $
 *                                                                          *
 *          Copyright (C) 1992-2001 Free Software Foundation, Inc.          *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, *
 * MA 02111-1307, USA.                                                      *
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

#ifdef IN_RTS
#include "tconfig.h"
#include "tsystem.h"
#include <sys/stat.h>
#else
#include "config.h"
#include "system.h"
#endif

#include "adaint.h"

#ifdef __RT__

/* Linux kernel modules don't have inputs, so don't define get_int.
   Simple output can be done via printk. */

void
put_char (c)
     int c;
{
  printk ("%c", c);
}

void
put_char_stderr (c)
     int c;
{
  put_char (c);
}

void
put_int (x)
     int x;
{
  printk ("%d", x);
}

void
put_int_stderr (int x)
{
  put_int (x);
}

#else

/* Don't use macros on GNU/Linux since they cause incompatible changes between
   glibc 2.0 and 2.1 */
#ifdef linux
#undef putchar
#undef getchar
#undef fputc
#undef stderr
#endif

int
get_char ()
{
#ifdef VMS
  return decc$getchar();
#else
  return getchar ();
#endif
}

int
get_int ()
{
  int x;

  scanf (" %d", &x);
  return x;
}

void
put_int (x)
     int x;
{
  printf ("%d", x);
}

void
put_int_stderr (x)
   int x;
{
  fprintf (stderr, "%d", x);
}

void
put_char (c)
     int c;
{
  putchar (c);
}

void
put_char_stderr (c)
     int c;
{
  fputc (c, stderr);
}
#endif

#ifdef __vxworks

char *
mktemp (template)
     char *template;
{
  return tmpnam (NULL);
}
#endif
