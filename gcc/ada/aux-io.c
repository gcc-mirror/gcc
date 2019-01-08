/****************************************************************************
 *                                                                          *
 *                         GNAT RUN-TIME COMPONENTS                         *
 *                                                                          *
 *                              A - T R A N S                               *
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

#include <stdio.h>

#ifdef IN_RTS
#include "tconfig.h"
#include "tsystem.h"
#else
#include "config.h"
#include "system.h"
#endif

/* Don't use macros versions of this functions on VxWorks since they cause
   imcompatible changes in some VxWorks versions */
#ifdef __vxworks
#undef getchar
#undef putchar
#undef feof
#undef ferror
#undef fileno
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
