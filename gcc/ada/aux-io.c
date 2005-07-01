/****************************************************************************
 *                                                                          *
 *                         GNAT RUN-TIME COMPONENTS                         *
 *                                                                          *
 *                              A - T R A N S                               *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *           Copyright (C) 1992-2003 Free Software Foundation, Inc.         *
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

#include <stdio.h>

#ifdef IN_RTS
#include "tconfig.h"
#include "tsystem.h"
#else
#include "config.h"
#include "system.h"
#endif

/* Function wrappers are needed to access the values from Ada which are
   defined as C macros.  */

FILE *c_stdin (void);
FILE *c_stdout (void);
FILE *c_stderr (void);
int seek_set_function (void);
int seek_end_function (void);
void *null_function (void);
int c_fileno (FILE *);

FILE *
c_stdin (void)
{
  return stdin;
}

FILE *
c_stdout (void)
{
  return stdout;
}

FILE *
c_stderr (void)
{
  return stderr;
}

#ifndef SEEK_SET    /* Symbolic constants for the "fseek" function: */
#define SEEK_SET 0  /* Set file pointer to offset */
#define SEEK_CUR 1  /* Set file pointer to its current value plus offset */
#define SEEK_END 2  /* Set file pointer to the size of the file plus offset */
#endif

int
seek_set_function (void)
{
  return SEEK_SET;
}

int
seek_end_function (void)
{
  return SEEK_END;
}

void *null_function (void)
{
  return NULL;
}

int
c_fileno (FILE *s)
{
  return fileno (s);
}
