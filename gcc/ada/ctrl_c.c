/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                               C T R L _ C                                *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *        Copyright (C) 2002-2009, Free Software Foundation, Inc.           *
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
#include "tconfig.h"
#include "tsystem.h"
#include <sys/stat.h>
#else
#include "config.h"
#include "system.h"
#endif

/* Services to intercept Ctrl-C */

/* __gnat_install_int_handler will install the specified handler.
   If called for the first time, it will also save the original handler */
void __gnat_install_int_handler (void (*) (void));

/* __gnat_uninstall_int_handler will reinstall the original handler */
void __gnat_uninstall_int_handler (void);

/* POSIX implementation */

#if (defined (__unix__) || defined (_AIX) || defined (__APPLE__)) \
 && !defined (__vxworks)

#include <signal.h>

void (*sigint_intercepted) (void) = 0;

struct sigaction original_act;

static void
__gnat_int_handler (int sig __attribute__ ((unused)))
{
  if (sigint_intercepted != 0)
    sigint_intercepted ();
}

/* Install handler and save original handler. */

void
__gnat_install_int_handler (void (*proc) (void))
{
  struct sigaction act;

  if (sigint_intercepted == 0)
    {
      act.sa_handler = __gnat_int_handler;
#if defined (__Lynx__)
      /* LynxOS does not support SA_RESTART. */
      act.sa_flags = 0;
#else
      act.sa_flags = SA_RESTART;
#endif
      sigemptyset (&act.sa_mask);
      sigaction (SIGINT, &act, &original_act);
    }

  sigint_intercepted = proc;
}

/* Restore original handler */

void
__gnat_uninstall_int_handler (void)
{
 if (sigint_intercepted != 0)
   {
     sigaction (SIGINT, &original_act, 0);
     sigint_intercepted = 0;
   }
}

/* Windows implementation */

#elif defined (__MINGW32__)

#include "mingw32.h"
#include <windows.h>

void (*sigint_intercepted) (void) = NULL;

static BOOL WINAPI
__gnat_int_handler  (DWORD dwCtrlType)
{
  switch (dwCtrlType)
    {
    case CTRL_C_EVENT:
    case CTRL_BREAK_EVENT:
      if (sigint_intercepted != 0)
        {
          sigint_intercepted ();
          return TRUE;
        }
      break;

    case CTRL_CLOSE_EVENT:
    case CTRL_LOGOFF_EVENT:
    case CTRL_SHUTDOWN_EVENT:
      break;
    }

  return FALSE;
}

void
__gnat_install_int_handler (void (*proc) (void))
{
  if (sigint_intercepted == NULL)
    SetConsoleCtrlHandler (__gnat_int_handler, TRUE);

  sigint_intercepted = proc;
}

void
__gnat_uninstall_int_handler (void)
{
  if (sigint_intercepted != NULL)
    SetConsoleCtrlHandler (__gnat_int_handler, FALSE);

  sigint_intercepted = NULL;
}

/* Default implementation: do nothing */

#else

void
__gnat_install_int_handler (void (*proc) (void) __attribute__ ((unused)))
{
}

void
__gnat_uninstall_int_handler (void)
{
}
#endif
