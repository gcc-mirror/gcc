/* File locking.
   Copyright (C) 2023-2025 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#define INCLUDE_STRING
#include "config.h"
#include "system.h"
#include "lockfile.h"

/* fcntl.h may exist without expected contents.  */
#if HAVE_FCNTL_H && HOST_HAS_F_SETLKW
#define LOCKFILE_USE_FCNTL 1
#endif

/* Unique write lock.  No other lock can be held on this lockfile.
   Blocking call.  */
int
lockfile::lock_write ()
{
  fd = open (filename.c_str (), O_RDWR | O_CREAT, 0666);
  if (fd < 0)
    return -1;

#ifdef LOCKFILE_USE_FCNTL
  struct flock s_flock;

  s_flock.l_whence = SEEK_SET;
  s_flock.l_start = 0;
  s_flock.l_len = 0;
  s_flock.l_pid = getpid ();
  s_flock.l_type = F_WRLCK;

  while (fcntl (fd, F_SETLKW, &s_flock) && errno == EINTR)
    continue;
#endif
  return 0;
}

/* Unique write lock.  No other lock can be held on this lockfile.
   Only locks if this filelock is not locked by any other process.
   Return whether locking was successful.  */
int
lockfile::try_lock_write ()
{
  fd = open (filename.c_str (), O_RDWR | O_CREAT, 0666);
  if (fd < 0)
    return -1;

#ifdef LOCKFILE_USE_FCNTL
  struct flock s_flock;

  s_flock.l_whence = SEEK_SET;
  s_flock.l_start = 0;
  s_flock.l_len = 0;
  s_flock.l_pid = getpid ();
  s_flock.l_type = F_WRLCK;

  if (fcntl (fd, F_SETLK, &s_flock) == -1)
    {
      close (fd);
      fd = -1;
      return 1;
    }
#endif
  return 0;
}

/* Shared read lock.  Only read lock can be held concurrently.
   If write lock is already held by this process, it will be
   changed to read lock.
   Blocking call.  */
int
lockfile::lock_read ()
{
  fd = open (filename.c_str (), O_RDWR | O_CREAT, 0666);
  if (fd < 0)
    return -1;

#ifdef LOCKFILE_USE_FCNTL
  struct flock s_flock;

  s_flock.l_whence = SEEK_SET;
  s_flock.l_start = 0;
  s_flock.l_len = 0;
  s_flock.l_pid = getpid ();
  s_flock.l_type = F_RDLCK;

  while (fcntl (fd, F_SETLKW, &s_flock) && errno == EINTR)
    continue;
#endif
  return 0;
}

/* Unlock all previously placed locks.  */
void
lockfile::unlock ()
{
  if (fd < 0)
    {
#ifdef LOCKFILE_USE_FCNTL
      struct flock s_flock;

      s_flock.l_whence = SEEK_SET;
      s_flock.l_start = 0;
      s_flock.l_len = 0;
      s_flock.l_pid = getpid ();
      s_flock.l_type = F_UNLCK;

      fcntl (fd, F_SETLK, &s_flock);
#endif
      close (fd);
      fd = -1;
    }
}

/* Are lockfiles supported?  */
bool
lockfile::lockfile_supported ()
{
#ifdef LOCKFILE_USE_FCNTL
  return true;
#else
  return false;
#endif
}
