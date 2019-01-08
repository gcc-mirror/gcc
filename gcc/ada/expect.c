/****************************************************************************
 *                                                                          *
 *                         GNAT RUN-TIME COMPONENTS                         *
 *                                                                          *
 *                               E X P E C T                                *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *                     Copyright (C) 2001-2019, AdaCore                     *
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
# if OLD_MINGW
#  include <sys/wait.h>
# endif
#elif defined (__vxworks) && defined (__RTP__)
# include <wait.h>
#elif defined (__Lynx__)
  /* ??? See comment in adaint.c.  */
# define GCC_RESOURCE_H
# include <sys/wait.h>
#elif defined (__PikeOS__)
  /* No wait.h available */
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
#include <signal.h>
#include <io.h>
#include "mingw32.h"

int
__gnat_waitpid (int pid)
{
  HANDLE h = OpenProcess (PROCESS_ALL_ACCESS, FALSE, pid);
  DWORD exitcode = 1;
  DWORD res;

  if (h != NULL)
    {
      res = WaitForSingleObject (h, INFINITE);
      GetExitCodeProcess (h, &exitcode);
      CloseHandle (h);
    }

  __gnat_win32_remove_handle (NULL, pid);
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
  *pid = __gnat_portable_no_block_spawn (argv);
}

int
__gnat_pipe (int *fd)
{
  HANDLE read, write;

  CreatePipe (&read, &write, NULL, 0);
  fd[0]=_open_osfhandle ((intptr_t)read, 0);
  fd[1]=_open_osfhandle ((intptr_t)write, 0);
  return 0;  /* always success */
}

int
__gnat_expect_poll (int *fd,
                    int num_fd,
                    int timeout,
                    int *dead_process,
                    int *is_set)
{
#define MAX_DELAY 100

  int i, delay, infinite = 0;
  DWORD avail;
  HANDLE handles[num_fd];

  *dead_process = 0;

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
            {
              *dead_process = i + 1;
              return -1;
            }
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
#include <signal.h>

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
__gnat_expect_poll (int *fd,
                    int num_fd,
                    int timeout,
                    int *dead_process,
                    int *is_set)
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

  *dead_process = 0;

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
              dead_process = i + 1;
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
__gnat_expect_poll (int *fd,
                    int num_fd,
                    int timeout,
                    int *dead_process,
                    int *is_set)
{
  struct timeval tv;
  SELECT_MASK rset;
  SELECT_MASK eset;

  int max_fd = 0;
  int ready;
  int i;
#ifdef __hpux__
  int received;
#endif

  *dead_process = 0;

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
#ifdef __hpux__
	received = 0;
#endif

        for (i = 0; i < num_fd; i++)
	  {
	    if (FD_ISSET (fd[i], &rset))
	      {
		is_set[i] = 1;
#ifdef __hpux__
		received = 1;
#endif
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
                        dead_process = i + 1;
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

int
__gnat_waitpid (int pid ATTRIBUTE_UNUSED, int sig ATTRIBUTE_UNUSED)
{
  return 0;
}

int
__gnat_pipe (int *fd ATTRIBUTE_UNUSED)
{
  return -1;
}

int
__gnat_expect_fork (void)
{
  return -1;
}

void
__gnat_expect_portable_execvp (int *pid ATTRIBUTE_UNUSED,
			       char *cmd ATTRIBUTE_UNUSED,
			       char *argv[] ATTRIBUTE_UNUSED)
{
  *pid = 0;
}

int
__gnat_expect_poll (int *fd ATTRIBUTE_UNUSED,
                    int num_fd ATTRIBUTE_UNUSED,
                    int timeout ATTRIBUTE_UNUSED,
                    int *dead_process ATTRIBUTE_UNUSED,
                    int *is_set ATTRIBUTE_UNUSED)
{
  *dead_process = 0;
  return -1;
}
#endif
