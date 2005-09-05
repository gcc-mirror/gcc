/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                               E X P E C T                                *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *           Copyright (C) 2001-2005 Ada Core Technologies, Inc.            *
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

#include <sys/types.h>

#ifdef __MINGW32__
#if OLD_MINGW
#include <sys/wait.h>
#endif
#elif defined (__vxworks) && defined (__RTP__)
#include <wait.h>
#else
#include <sys/wait.h>
#endif

/* This file provides the low level functionalities needed to implement Expect
   capabilities in GNAT.Expect.
   Implementations for unix and windows systems is provided.
   Dummy stubs are also provided for other systems. */

#ifdef _AIX
/* Work around the fact that gcc/cpp does not define "__unix__" under AiX.  */
#define __unix__
#endif

#ifdef __APPLE__
/* Work around the fact that gcc/cpp does not define "__unix__" on Darwin.  */
#define __unix__
#endif

#ifdef _WIN32

#include <windows.h>
#include <process.h>

void
__gnat_kill (int pid, int sig)
{
  HANDLE process_handle;

  if (sig == 9)
    {
      process_handle = OpenProcess (PROCESS_TERMINATE, FALSE, pid);
      if (process_handle != NULL)
	{
	  TerminateProcess (process_handle, 0);
	  CloseHandle (process_handle);
	}
    }
}

int
__gnat_waitpid (int pid)
{
  HANDLE process_handle;
  DWORD exitcode = 1;
  DWORD res;

  process_handle = OpenProcess (PROCESS_QUERY_INFORMATION, FALSE, pid);

  if (process_handle != NULL)
    {
      res = WaitForSingleObject (process_handle, INFINITE);
      GetExitCodeProcess (process_handle, &exitcode);
      CloseHandle (process_handle);
    }

  return (int) exitcode;
}

int
__gnat_expect_fork (void)
{
  return 0;
}

void
__gnat_expect_portable_execvp (int *pid, char *cmd, char *argv[])
{
  *pid = (int) spawnve (_P_NOWAIT, cmd, argv, NULL);
}

int
__gnat_pipe (int *fd)
{
  HANDLE read, write;

  CreatePipe (&read, &write, NULL, 0);
  fd[0]=_open_osfhandle ((long)read, 0);
  fd[1]=_open_osfhandle ((long)write, 0);
  return 0;  /* always success */
}

int
__gnat_expect_poll (int *fd, int num_fd, int timeout, int *is_set)
{
#define MAX_DELAY 100

  int i, delay, infinite = 0;
  DWORD avail;
  HANDLE handles[num_fd];

  for (i = 0; i < num_fd; i++)
    is_set[i] = 0;

  for (i = 0; i < num_fd; i++)
    handles[i] = (HANDLE) _get_osfhandle (fd [i]);

  /* Start with small delays, and then increase them, to avoid polling too
     much when waiting a long time */
  delay = 5;

  if (timeout < 0)
    infinite = 1;

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

      if (!infinite && timeout <= 0)
        return 0;

      Sleep (delay);
      timeout -= delay;

      if (delay < MAX_DELAY)
        delay += 10;
    }
}

#elif defined (VMS)
#include <unistd.h>
#include <stdio.h>
#include <unixio.h>
#include <stdlib.h>
#include <string.h>
#include <vms/descrip.h>
#include <stdio.h>
#include <vms/stsdef.h>
#include <vms/iodef.h>

int
__gnat_waitpid (int pid)
{
  int status = 0;

  waitpid (pid, &status, 0);
  status = WEXITSTATUS (status);

  return status;
}

int
__gnat_pipe (int *fd)
{
  return pipe (fd);
}

int
__gnat_expect_fork (void)
{
  return -1;
}

void
__gnat_expect_portable_execvp (int *pid, char *cmd, char *argv[])
{
  *pid = (int) getpid ();
  /* Since cmd is fully qualified, it is incorrect to call execvp */
  execv (cmd, argv);
  _exit (1);
}

int
__gnat_expect_poll (int *fd, int num_fd, int timeout, int *is_set)
{
  int i, num, ready = 0;
  unsigned int status;
  int mbxchans [num_fd];
  struct dsc$descriptor_s mbxname;
  struct io_status_block {
    short int condition;
    short int count;
    int dev;
  } iosb;
  char buf [256];

  for (i = 0; i < num_fd; i++)
    is_set[i] = 0;

  for (i = 0; i < num_fd; i++)
    {

      /* Get name of the mailbox used in the pipe */
      getname (fd [i], buf);

      /* Assign a channel to the mailbox */
      if (strlen (buf) > 0)
	{
	  mbxname.dsc$w_length = strlen (buf);
	  mbxname.dsc$b_dtype = DSC$K_DTYPE_T;
	  mbxname.dsc$b_class = DSC$K_CLASS_S;
	  mbxname.dsc$a_pointer = buf;

	  status = SYS$ASSIGN (&mbxname, &mbxchans[i], 0, 0, 0);

	  if ((status & 1) != 1)
	    {
	      ready = -1;
	      return ready;
	    }
	}
    }

  num = timeout / 100;

  while (1)
    {
      for (i = 0; i < num_fd; i++)
	{
	  if (mbxchans[i] > 0)
	    {

	      /* Peek in the mailbox to see if there's data */
	      status = SYS$QIOW
		(0, mbxchans[i], IO$_SENSEMODE|IO$M_READERCHECK,
		 &iosb, 0, 0, 0, 0, 0, 0, 0, 0);

	      if ((status & 1) != 1)
		{
		  ready = -1;
		  goto deassign;
		}

	      if (iosb.count > 0)
		{
		  is_set[i] = 1;
		  ready = 1;
		  goto deassign;
		}
	    }
	}

      if (timeout > 0 && num == 0)
	{
	  ready = 0;
	  goto deassign;
	}

      usleep (100000);
      num--;
    }

 deassign:

  /* Deassign channels assigned above */
  for (i = 0; i < num_fd; i++)
    {
      if (mbxchans[i] > 0)
	status = SYS$DASSGN (mbxchans[i]);
    }

  return ready;
}

#elif defined (__unix__)

#ifdef __hpux__
#include <sys/ptyio.h>
#endif

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

void
__gnat_kill (int pid, int sig)
{
  kill (pid, sig);
}

int
__gnat_waitpid (int pid)
{
  int status = 0;

  waitpid (pid, &status, 0);
  status = WEXITSTATUS (status);

  return status;
}

int
__gnat_pipe (int *fd)
{
  return pipe (fd);
}

int
__gnat_expect_fork (void)
{
  return fork ();
}

void
__gnat_expect_portable_execvp (int *pid, char *cmd, char *argv[])
{
  *pid = (int) getpid ();
  /* Since cmd is fully qualified, it is incorrect to call execvp */
  execv (cmd, argv);
  _exit (1);
}

int
__gnat_expect_poll (int *fd, int num_fd, int timeout, int *is_set)
{
  struct timeval tv;
  SELECT_MASK rset;
  SELECT_MASK eset;

  int max_fd = 0;
  int ready;
  int i;
  int received;

  tv.tv_sec  = timeout / 1000;
  tv.tv_usec = (timeout % 1000) * 1000;

  do {
    FD_ZERO (&rset);
    FD_ZERO (&eset);

    for (i = 0; i < num_fd; i++)
      {
        FD_SET (fd[i], &rset);
        FD_SET (fd[i], &eset);

        if (fd[i] > max_fd)
	  max_fd = fd[i];
      }

    ready =
      select (max_fd + 1, &rset, NULL, &eset, timeout == -1 ? NULL : &tv);

    if (ready > 0)
      {
	received = 0;

        for (i = 0; i < num_fd; i++)
	  {
	    if (FD_ISSET (fd[i], &rset))
	      {
		is_set[i] = 1;
		received = 1;
	      }
	    else
	      is_set[i] = 0;
	  }

#ifdef __hpux__
        for (i = 0; i < num_fd; i++)
	  {
	    if (FD_ISSET (fd[i], &eset))
	      {
	        struct request_info ei;

	        /* Only query and reset error state if no file descriptor
	           is ready to be read, otherwise we will be signalling a
	           died process too early */

	        if (!received)
		  {
	            ioctl (fd[i], TIOCREQCHECK, &ei);

	            if (ei.request == TIOCCLOSE)
		      {
		        ioctl (fd[i], TIOCREQSET, &ei);
		        return -1;
		      }

	            ioctl (fd[i], TIOCREQSET, &ei);
		  }
	        ready--;
	      }
	  }
#endif
      }
  } while (timeout == -1 && ready == 0);

  return ready;
}

#else

void
__gnat_kill (int pid, int sig)
{
}

int
__gnat_waitpid (int pid, int sig)
{
  return 0;
}

int
__gnat_pipe (int *fd)
{
  return -1;
}

int
__gnat_expect_fork (void)
{
  return -1;
}

void
__gnat_expect_portable_execvp (int *pid, char *cmd, char *argv[])
{
  *pid = 0;
}

int
__gnat_expect_poll (int *fd, int num_fd, int timeout, int *is_set)
{
  return -1;
}
#endif
