/****************************************************************************
 *                                                                          *
 *                         GNAT RUN-TIME COMPONENTS                         *
 *                                                                          *
 *                              A - T R A N S                               *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *                            $Revision: 1.2 $
 *                                                                          *
 *           Copyright (C) 1992-2001 Free Software Foundation, Inc.         *
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

#include <stdio.h>

#ifdef IN_RTS
#include "tconfig.h"
#else
#include "config.h"
#endif

/* Function wrappers are needed to access the values from Ada which are */
/* defined as C macros.                                                 */

FILE *c_stdin         PARAMS ((void));
FILE *c_stdout        PARAMS ((void));
FILE *c_stderr        PARAMS ((void));
int seek_set_function PARAMS ((void));
int seek_end_function PARAMS ((void));
void *null_function   PARAMS ((void));
int c_fileno          PARAMS ((FILE *));

FILE *
c_stdin () 
{ 
  return stdin; 
}

FILE *
c_stdout () 
{ 
  return stdout;
}

FILE *
c_stderr () 
{ 
  return stderr;
}

#ifndef SEEK_SET    /* Symbolic constants for the "fseek" function: */
#define SEEK_SET 0  /* Set file pointer to offset */
#define SEEK_CUR 1  /* Set file pointer to its current value plus offset */
#define SEEK_END 2  /* Set file pointer to the size of the file plus offset */
#endif

int   
seek_set_function ()  
{ 
  return SEEK_SET; 
}

int   
seek_end_function ()  
{ 
  return SEEK_END; 
}

void *null_function ()  
{ 
  return NULL;     
}

int 
c_fileno (s) 
     FILE *s;
{ 
  return fileno (s); 
}
