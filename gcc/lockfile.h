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

#ifndef LOCKFILE_H
#define LOCKFILE_H

/* Used to synchronize across multiple processes.  */
class lockfile {
public:
  /* Default constructor.  */
  lockfile (): fd (-1)
  {}
  /* Intended constructor for use.  Filename should not be used for anything
     other than locking to prevent unintentional unlock.  */
  lockfile (std::string filename): lockfile ()
  {
    this->filename = std::move (filename);
  }
  lockfile (lockfile const& o): lockfile (o.filename)
  {}

  void operator=(lockfile o)
  {
    unlock ();
    this->filename = o.filename;
    this->fd = o.fd;
    o.fd = -1;
  }

  /* Unique write lock.  No other lock can be held on this lockfile.
     Blocking call.  */
  int lock_write ();

  /* Unique write lock.  No other lock can be held on this lockfile.
     Only locks if this filelock is not locked by any other process.
     Return whether locking was successful.  */
  int try_lock_write ();

  /* Shared read lock.  Only read lock can be held concurrently.
     If write lock is already held by this process, it will be
     changed to read lock.
     Blocking call.  */
  int lock_read ();

  /* Unlock all previously placed locks.  */
  void unlock ();

  /* Returns whether any lock is held.  */
  bool
  locked ()
  {
    return fd < 0;
  }

  /* Are lockfiles supported?  */
  static bool lockfile_supported ();
private:
  std::string filename;
  int fd;
};

#endif
