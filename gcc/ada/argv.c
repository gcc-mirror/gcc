/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                A R G V                                   *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *         Copyright (C) 1992-2011, Free Software Foundation, Inc.          *
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

/* Also routines for accessing the environment from the runtime library.
   Gnat_envp is the original envp value as stored by the binder generated
   main program, and these routines are accessed from the
   Ada.Command_Line.Environment package.  */

#ifdef __cplusplus
extern "C" {
#endif

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

#if defined (_WIN32) && !defined (RTX)
/* Note that on Windows environment the environ point to a buffer that could
   be reallocated if needed. It means that gnat_envp needs to be updated
   before using gnat_envp to point to the right environment space */
#include <stdlib.h>
/* for the environ variable definition */
#define gnat_envp (environ)
#endif

int
__gnat_arg_count (void)
{
  return gnat_argc;
}

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

int
__gnat_env_count (void)
{
  int i;

  for (i = 0; gnat_envp[i]; i++)
    ;
  return i;
}

int
__gnat_len_env (int env_num)
{
  if (gnat_envp != NULL)
    return strlen (gnat_envp[env_num]);
  else
    return 0;
}

void
__gnat_fill_env (char *a, int i)
{
  if (gnat_envp != NULL)
    strncpy (a, gnat_envp[i], strlen (gnat_envp[i]));
}

#ifdef __cplusplus
}
#endif
