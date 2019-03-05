/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                A R G V                                   *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *         Copyright (C) 1992-2019, Free Software Foundation, Inc.          *
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

/* Routines for accessing command line arguments from both the runtime
   library and from the compiler itself.  In the former case, gnat_argc
   and gnat_argv are the original argc and argv values as stored by the
   binder generated main program, and these routines are accessed from
   the Ada.Command_Line package.  In the compiler case, gnat_argc and
   gnat_argv are the values as modified by toplev, and these routines
   are accessed from the Osint package.  */

/* This version is for the ravenscar-cert runtime in LynxOS-178, with
   minimal support for Ada.Command_Line.Command_Name */

#include <sys/types.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

/* argc and argv of the main program are saved under gnat_argc and gnat_argv,
   envp of the main program is saved under gnat_envp.
   While gnat_argc and gnat_envp are not needed, they are referenced from
   the binder-generated file so they need to be defined here */

int gnat_argc = 0;
const char **gnat_argv = (const char **) 0;
const char **gnat_envp = (const char **) 0;

int
__gnat_len_arg (int arg_num)
{
  if (gnat_argv != NULL)
    return strlen (gnat_argv[arg_num]);
  else
    return 0;
}

void
__gnat_fill_arg (char *a, int i)
{
  if (gnat_argv != NULL)
    strncpy (a, gnat_argv[i], strlen(gnat_argv[i]));
}

#ifdef __cplusplus
}
#endif
