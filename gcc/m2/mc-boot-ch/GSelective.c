/* GSelective.c provides access to select for Modula-2.

Copyright (C) 2016-2021 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* implementation module in C.  */

#include "config.h"
#include "system.h"
#include "ansidecl.h"

#include "gm2-libs-host.h"

/* PROCEDURE Select (nooffds: CARDINAL; readfds, writefds, exceptfds:
SetOfFd; timeout: Timeval) : INTEGER ; */

#if defined(HAVE_SELECT)
int
Selective_Select (int nooffds, fd_set *readfds, fd_set *writefds,
                  fd_set *exceptfds, struct timeval *timeout)
{
  return select (nooffds, readfds, writefds, exceptfds, timeout);
}
#else
int
Selective_Select (int nooffds, void *readfds, void *writefds, void *exceptfds,
                  void *timeout)
{
  return 0;
}
#endif

/* PROCEDURE InitTime (sec, usec) : Timeval ; */

#if defined(HAVE_SELECT)
struct timeval *
Selective_InitTime (unsigned int sec, unsigned int usec)
{
  struct timeval *t = (struct timeval *)malloc (sizeof (struct timeval));

  t->tv_sec = (long int)sec;
  t->tv_usec = (long int)usec;
  return t;
}

void
Selective_GetTime (struct timeval *t, unsigned int *sec, unsigned int *usec)
{
  *sec = (unsigned int)t->tv_sec;
  *usec = (unsigned int)t->tv_usec;
}

void
Selective_SetTime (struct timeval *t, unsigned int sec, unsigned int usec)
{
  t->tv_sec = sec;
  t->tv_usec = usec;
}

/* PROCEDURE KillTime (t: Timeval) : Timeval ; */

struct timeval *
Selective_KillTime (struct timeval *t)
{
  free (t);
  return NULL;
}

/* PROCEDURE InitSet () : SetOfFd ; */

fd_set *
Selective_InitSet (void)
{
  fd_set *s = (fd_set *)malloc (sizeof (fd_set));

  return s;
}

/* PROCEDURE KillSet (s: SetOfFd) : SetOfFd ; */

fd_set *
Selective_KillSet (fd_set *s)
{
  free (s);
  return NULL;
}

/* PROCEDURE FdZero (s: SetOfFd) ; */

void
Selective_FdZero (fd_set *s)
{
  FD_ZERO (s);
}

/* PROCEDURE Fd_Set (fd: INTEGER; SetOfFd) ; */

void
Selective_FdSet (int fd, fd_set *s)
{
  FD_SET (fd, s);
}

/* PROCEDURE FdClr (fd: INTEGER; SetOfFd) ; */

void
Selective_FdClr (int fd, fd_set *s)
{
  FD_CLR (fd, s);
}

/* PROCEDURE FdIsSet (fd: INTEGER; SetOfFd) : BOOLEAN ; */

int
Selective_FdIsSet (int fd, fd_set *s)
{
  return FD_ISSET (fd, s);
}

/* GetTimeOfDay - fills in a record, Timeval, filled in with the
current system time in seconds and microseconds.  It returns zero
(see man 3p gettimeofday) */

int
Selective_GetTimeOfDay (struct timeval *t)
{
  return gettimeofday (t, NULL);
}
#else

void *
Selective_InitTime (unsigned int sec, unsigned int usec)
{
  return NULL;
}

void *
Selective_KillTime (void *t)
{
  return NULL;
}

void
Selective_GetTime (struct timeval *t, unsigned int *sec, unsigned int *usec)
{
}

void
Selective_SetTime (struct timeval *t, unsigned int sec, unsigned int usec)
{
}

fd_set *
Selective_InitSet (void)
{
  return NULL;
}

void
Selective_FdZero (void *s)
{
}

void
Selective_FdSet (int fd, void *s)
{
}

void
Selective_FdClr (int fd, void *s)
{
}

int
Selective_FdIsSet (int fd, void *s)
{
  return 0;
}

int
Selective_GetTimeOfDay (struct timeval *t)
{
  return -1;
}
#endif

/* PROCEDURE MaxFdsPlusOne (a, b: File) : File ; */

int
Selective_MaxFdsPlusOne (int a, int b)
{
  if (a > b)
    return a + 1;
  else
    return b + 1;
}

/* PROCEDURE WriteCharRaw (fd: INTEGER; ch: CHAR) ; */

void
Selective_WriteCharRaw (int fd, char ch)
{
  write (fd, &ch, 1);
}

/* PROCEDURE ReadCharRaw (fd: INTEGER) : CHAR ; */

char
Selective_ReadCharRaw (int fd)
{
  char ch;

  read (fd, &ch, 1);
  return ch;
}

void
_M2_Selective_init ()
{
}

void
_M2_Selective_finish ()
{
}
