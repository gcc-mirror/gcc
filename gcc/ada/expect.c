/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                               E X P E C T                                *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *                            $Revision: 1.1 $
 *                                                                          *
 *              Copyright (C) 2001 Ada Core Technologies, Inc.              *
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
 * It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). *
 *                                                                          *
 ****************************************************************************/

#ifdef __alpha_vxworks
#include "vxWorks.h"
#endif

#ifdef IN_RTS
#define POSIX
#include "tconfig.h"
#include "tsystem.h"
#else
#include "config.h"
#include "system.h"
#endif

/* This file provides the low level functionalities needed to implement Expect
   capabilities in GNAT.Expect.
   Implementations for unix and windows systems is provided.
   Dummy stubs are also provided for other systems. */

#ifdef _AIX
/* Work around the fact that gcc/cpp does not define "unix" under AiX.  */
#define unix
#endif

#ifdef _WIN32

#include <windows.h>
#include <process.h>

/* ??? Provide a no-op for now */

void
kill ()
{
}

int
__gnat_expect_fork ()
{
  return 0;
}

void
__gnat_expect_portable_execvp (cmd, argv)
     char *cmd;
     char *argv[];
{
  (void) spawnve (_P_NOWAIT, cmd, argv, NULL);
}

int
__gnat_pipe (fd)
     int *fd;
{
  HANDLE read, write;

  CreatePipe (&read, &write, NULL, 0);
  fd[0]=_open_osfhandle (read, 0);
  fd[1]=_open_osfhandle (write, 0);
  return 0;  /* always success */
}

int
__gnat_expect_poll (fd, num_fd, timeout, is_set)
     int *fd;
     int num_fd;
     int timeout;
     int *is_set;
{
  int i, num;
  DWORD avail;
  HANDLE handles[num_fd];

  for (i = 0; i < num_fd; i++)
    is_set[i] = 0;

  for (i = 0; i < num_fd; i++)
    handles[i] = (HANDLE) _get_osfhandle (fd [i]);

  num = timeout / 10;

  while (1)
    {
      for (i = 0; i < num_fd; i++)
	{
	  if (!PeekNamedPipe (handles [i], NULL, 0, NULL, &avail, NULL))
	    return -1;

	  if (avail > 0)
	    {
	      is_set[i] = 1;
	      return 1;
	    }
	}

      if (timeout >= 0 && num == 0)
	return 0;

      Sleep (10);
      num--;
    }
}

#elif defined (unix)

#include <sys/time.h>

#ifndef NO_FD_SET
#define SELECT_MASK fd_set
#else /* !NO_FD_SET */
#ifndef _AIX
typedef long fd_mask;
#endif /* _AIX */
#ifdef _IBMR2
#define SELECT_MASK void
#else /* !_IBMR2 */
#define SELECT_MASK int
#endif /* !_IBMR2 */
#endif /* !NO_FD_SET */

int
__gnat_pipe (fd)
     int *fd;
{
  return pipe (fd);
}

int
__gnat_expect_fork ()
{
  return fork ();
}

void
__gnat_expect_portable_execvp (cmd, argv) 
     char *cmd;
     char *argv[];
{
  execvp (cmd, argv);
}

int
__gnat_expect_poll (fd, num_fd, timeout, is_set)
     int *fd;
     int num_fd;
     int timeout;
     int *is_set;
{
  struct timeval tv;
  SELECT_MASK rset;
  int max_fd = 0;
  int ready;
  int i;

  FD_ZERO (&rset);

  for (i = 0; i < num_fd; i++)
    {
      FD_SET (fd [i], &rset);
      if (fd [i] > max_fd)
	max_fd = fd [i];
    }

  tv.tv_sec  = timeout / 1000;
  tv.tv_usec = (timeout % 1000) * 1000;

  ready = select (max_fd + 1, &rset, NULL, NULL, timeout == -1 ? NULL : &tv);

  if (ready > 0)
    for (i = 0; i < num_fd; i++)
      is_set [i] = (FD_ISSET (fd [i], &rset)  ? 1 : 0);

  return ready;
}

#else

int
__gnat_pipe (fd)
     int *fd;
{
  return -1;
}

int
__gnat_expect_fork ()
{
  return -1;
}

void
__gnat_expect_portable_execvp (cmd, argv)
     char *cmd;
     char *argv[];
{
}

int
__gnat_expect_poll (fd, num_fd, timeout, is_set)
     int *fd;
     int num_fd;
     int timeout;
     int *is_set;
{
  return -1;
}
#endif
