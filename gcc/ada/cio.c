/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                  C I O                                   *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *          Copyright (C) 1992-2005 Free Software Foundation, Inc.          *
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

/* Don't use macros on GNU/Linux since they cause incompatible changes between
   glibc 2.0 and 2.1 */
#ifdef linux
#undef putchar
#undef getchar
#undef fputc
#undef stderr
#undef stdout
#endif

#ifdef VTHREADS
#undef putchar
#undef getchar
#endif

int
get_char (void)
{
#ifdef VMS
  return decc$getchar();
#else
  return getchar ();
#endif
}

int
get_int (void)
{
  int x;

  scanf (" %d", &x);
  return x;
}

void
put_int (int x)
{
   /* Use fprintf rather than printf, since the latter is unbuffered
      on vxworks */
   fprintf (stdout, "%d", x);
}

void
put_int_stderr (int x)
{
  fprintf (stderr, "%d", x);
}

void
put_char (int c)
{
  putchar (c);
}

void
put_char_stderr (int c)
{
  fputc (c, stderr);
}

#ifdef __vxworks

char *
mktemp (char *template)
{
  return tmpnam (NULL);
}
#endif
