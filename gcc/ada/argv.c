/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                A R G V                                   *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *                                                                          *
 *         Copyright (C) 1992-2001 Free Software Foundation, Inc.           *
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

/* Routines for accessing command line arguments from both the runtime
   library and from the compiler itself. In the former case, gnat_argc
   and gnat_argv are the original argc and argv values as stored by the
   binder generated main program, and these routines are accessed from
   the Ada.Command_Line package. In the compiler case, gnat_argc and
   gnat_argv are the values as modified by toplev, and these routines
   are accessed from the Osint package. */

/* Also routines for accessing the environment from the runtime library.
   Gnat_envp is the original envp value as stored by the binder generated
   main program, and these routines are accessed from the
   Ada.Command_Line.Environment package. */

#ifdef IN_RTS
#include "tconfig.h"
#include "tsystem.h"
#include <sys/stat.h>
#else
#include "config.h"
#include "system.h"
#endif

#include "adaint.h"

/* argc and argv of the main program are saved under gnat_argc and gnat_argv,
   envp of the main program is saved under gnat_envp.  */

int gnat_argc = 0;
const char **gnat_argv = (const char **) 0;
const char **gnat_envp = (const char **) 0;

int
__gnat_arg_count ()
{
  return gnat_argc;
}

int
__gnat_len_arg (arg_num)
   int arg_num;
{
  return strlen (gnat_argv[arg_num]);
}

void
__gnat_fill_arg (a, i)
   char *a;
   int i;
{
  strncpy (a, gnat_argv[i], strlen(gnat_argv[i]));
}

int
__gnat_env_count ()
{
  int i;

  for (i = 0; gnat_envp[i]; i++)
    ;
  return i;
}

int
__gnat_len_env (env_num)
   int env_num;
{
  return strlen (gnat_envp[env_num]);
}

void
__gnat_fill_env (a, i)
   char *a;
   int i;
{
  strncpy (a, gnat_envp[i], strlen (gnat_envp[i]));
}
