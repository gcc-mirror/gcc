/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                  E N V                                   *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *            Copyright (C) 2005-2025, Free Software Foundation, Inc.       *
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

#ifdef IN_RTS
# include "runtime.h"
# include <stdio.h>
# include <stdlib.h>
# include <string.h>

/* We don't have libiberty, so use malloc.  */
# define xmalloc(S) malloc (S)
#else /* IN_RTS */
# include "config.h"
# include "system.h"
#endif /* IN_RTS */

#if defined (__MINGW32__)
#include <stdlib.h>
#endif

#if defined (__APPLE__) \
   && !(defined (__arm__) \
        || defined (__arm64__) \
        || defined (__IOS_SIMULATOR__))
/* On Darwin, _NSGetEnviron must be used for shared libraries; but it is not
   available on iOS (on device or on simulator).  */
#include <crt_externs.h>
#endif

#if defined (__vxworks)
  #include <vxWorks.h>
  #include <version.h>

  #if defined (__RTP__)
    /* On VxWorks 6 Real-Time process mode, environ is defined in unistd.h.  */
    #include <unistd.h>
  #elif defined (VTHREADS)
    /* VTHREADS mode applies to both VxWorks 653 and VxWorks MILS. The
       inclusion of vThreadsData.h is necessary to workaround a bug with
       envLib.h on VxWorks MILS and VxWorks 653.  */
    #include <vThreadsData.h>
    #include <envLib.h>
  #else
    /* Kernel mode */
    #include <envLib.h>
  #endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifdef VMS
#include <vms/descrip.h>
#endif

#include "env.h"

void
__gnat_getenv (char *name, int *len, char **value)
{
  *value = getenv (name);
  if (!*value)
    *len = 0;
  else
    *len = strlen (*value);

  return;
}

void
__gnat_setenv (char *name, char *value)
{
#if (defined (__vxworks) && (defined (__RTP__) || _WRS_VXWORKS_MAJOR >= 7)) \
    || defined (__APPLE__)
  setenv (name, value, 1);

#else
  size_t size = strlen (name) + strlen (value) + 2;
  char *expression;

  expression = (char *) xmalloc (size * sizeof (char));

  sprintf (expression, "%s=%s", name, value);
  putenv (expression);
#if defined (__MINGW32__) || defined (__vxworks)
  /* putenv for Windows and VxWorks 6 kernel modules makes a copy of the
     expression string, so we need to free it after the call to putenv. */
  free (expression);
#endif
#endif
}

char **
__gnat_environ (void)
{
#if defined (__MINGW32__)
  return _environ;
#elif defined (__sun__)
  extern char **_environ;
  return _environ;
#elif defined (__APPLE__) \
     && !(defined (__arm__) \
          || defined (__arm64__)                \
          || defined (__IOS_SIMULATOR__))
  return *_NSGetEnviron ();
#elif ! (defined (__vxworks))
  extern char **environ;
  return environ;
#else
  #if defined (__RTP__) || defined (VTHREADS)
    return environ;
  #else
    /* For VxWorks kernel modules use envGet to get the task's environment
       (either the task's private environment if it has one or the global
       environment otherwise). taskId parameter of 0 refers to the current
       task (the VxWorks documentation says to use NULL but the compiler
       complains that taskId is an int rather than a pointer. Internally,
       VxWorks uses 0 as well). */
    return envGet (0);
  #endif
#endif
}

void __gnat_unsetenv (char *name)
{
#if defined (__hpux__) || defined (__sun__) \
     || (defined (__vxworks) && ! defined (__RTP__) \
          && _WRS_VXWORKS_MAJOR <= 6) \
     || defined (_AIX) || defined (__Lynx__)

  /* On Solaris and HP-UX there is no function to clear an environment
     variable. So we look for the variable in the environ table and delete it
     by setting the entry to NULL. This can clearly cause some memory leaks
     but free cannot be used on this context as not all strings in the environ
     have been allocated using malloc. To avoid this memory leak another
     method can be used. It consists in forcing the reallocation of all the
     strings in the environ table using malloc on the first call on the
     functions related to environment variable management. The disadvantage
     is that if a program makes a direct call to getenv the return string
     may be deallocated at some point. */
  /* Note that on AIX, unsetenv is not supported on 5.1 but it is on 5.3.
     As we are still supporting AIX 5.1 we cannot use unsetenv */
  char **env = __gnat_environ ();
  int index = 0;
  size_t size = strlen (name);

  while (env[index] != NULL) {
     if (strlen (env[index]) > size) {
       if (strstr (env[index], name) == env[index] &&
	   env[index][size] == '=') {
#if defined (__vxworks)
         /* on Vxworks we are sure that the string has been allocated using
            malloc */
         free (env[index]);
#endif
         while (env[index] != NULL) {
          env[index]=env[index + 1];
          index++;
         }
       } else
           index++;
     } else
         index++;
  }
#elif defined (__MINGW32__)
  /* On Windows platform putenv ("key=") is equivalent to unsetenv (a
     subsequent call to getenv ("key") will return NULL and not the "\0"
     string */
  size_t size = strlen (name) + 2;
  char *expression;
  expression = (char *) xmalloc (size * sizeof (char));

  sprintf (expression, "%s=", name);
  putenv (expression);
  free (expression);
#else
  unsetenv (name);
#endif
}

void __gnat_clearenv (void)
{
#if defined (__sun__) \
  || (defined (__vxworks) && !defined (__RTP__) && _WRS_VXWORKS_MAJOR <= 6) \
  || defined (__Lynx__) \
  || defined (__PikeOS__)
  /* On Solaris, VxWorks kernel pre 7, and Lynx there is no system
     call to unset a variable or to clear the environment so set all
     the entries in the environ table to NULL (see comment in
     __gnat_unsetenv for more explanation). */
  char **env = __gnat_environ ();
  int index = 0;

  while (env[index] != NULL) {
    env[index]=NULL;
    index++;
  }
#elif defined (__MINGW32__) || defined (__FreeBSD__) || defined (__APPLE__) \
   || (defined (__vxworks) && defined (__RTP__) || _WRS_VXWORKS_MAJOR >= 7) \
   || defined (__CYGWIN__) \
   || defined (__NetBSD__) || defined (__OpenBSD__) || defined (__rtems__) \
   || defined (__DragonFly__) || defined (__DJGPP__)
  /* On Windows, FreeBSD and MacOS there is no function to clean all the
     environment but there is a "clean" way to unset a variable. So go
     through the environ table and call __gnat_unsetenv on all entries */
  char **env = __gnat_environ ();
  size_t size;

  while (env[0] != NULL) {
    size = 0;
    while (env[0][size] != '=')
      size++;
    /* create a string that contains "name" */
    size++;
    {
      char *expression;
      expression = (char *) xmalloc (size * sizeof (char));
      strncpy (expression, env[0], size);
      expression[size - 1] = 0;
      __gnat_unsetenv (expression);
      free (expression);
    }
  }
#else
  clearenv ();
#endif
}

#ifdef __cplusplus
}
#endif
