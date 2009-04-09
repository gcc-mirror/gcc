------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                          S Y S T E M .  L I N U X                        --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--             Copyright (C) 2009, Free Software Foundation, Inc.           --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                                                                          --
------------------------------------------------------------------------------

--  This is the hppa version of this package

--  This package encapsulates cpu specific differences between implementations
--  of GNU/Linux, in order to share s-osinte-linux.ads.

--  PLEASE DO NOT add any with-clauses to this package or remove the pragma
--  Preelaborate. This package is designed to be a bottom-level (leaf) package.

package System.Linux is
   pragma Preelaborate;

   -----------
   -- Errno --
   -----------

   EAGAIN    : constant := 11;
   EINTR     : constant := 4;
   EINVAL    : constant := 22;
   ENOMEM    : constant := 12;
   EPERM     : constant := 1;
   ETIMEDOUT : constant := 238;

   -------------
   -- Signals --
   -------------

   SIGHUP     : constant := 1; --  hangup
   SIGINT     : constant := 2; --  interrupt (rubout)
   SIGQUIT    : constant := 3; --  quit (ASCD FS)
   SIGILL     : constant := 4; --  illegal instruction (not reset)
   SIGTRAP    : constant := 5; --  trace trap (not reset)
   SIGIOT     : constant := 6; --  IOT instruction
   SIGABRT    : constant := 6; --  used by abort, replace SIGIOT in the  future
   SIGEMT     : constant := 7; --  EMT
   SIGFPE     : constant := 8; --  floating point exception
   SIGKILL    : constant := 9; --  kill (cannot be caught or ignored)
   SIGBUS     : constant := 10; --  bus error
   SIGSEGV    : constant := 11; --  segmentation violation
   SIGSYS     : constant := 12; --  bad system call
   SIGPIPE    : constant := 13; --  write on a pipe with no one to read it
   SIGALRM    : constant := 14; --  alarm clock
   SIGTERM    : constant := 15; --  software termination signal from kill
   SIGUSR1    : constant := 16; --  user defined signal 1
   SIGUSR2    : constant := 17; --  user defined signal 2
   SIGCLD     : constant := 18; --  alias for SIGCHLD
   SIGCHLD    : constant := 18; --  child status change
   SIGPWR     : constant := 19; --  power-fail restart
   SIGVTALRM  : constant := 20; --  virtual timer expired
   SIGPROF    : constant := 21; --  profiling timer expired
   SIGPOLL    : constant := 22; --  pollable event occurred
   SIGIO      : constant := 22; --  I/O now possible (4.2 BSD)
   SIGWINCH   : constant := 23; --  window size change
   SIGSTOP    : constant := 24; --  stop (cannot be caught or ignored)
   SIGTSTP    : constant := 25; --  user stop requested from tty
   SIGCONT    : constant := 26; --  stopped process has been continued
   SIGTTIN    : constant := 27; --  background tty read attempted
   SIGTTOU    : constant := 28; --  background tty write attempted
   SIGURG     : constant := 29; --  urgent condition on IO channel
   SIGLOST    : constant := 30; --  File lock lost
   SIGUNUSED  : constant := 31; --  unused signal (GNU/Linux)
   SIGXCPU    : constant := 33; --  CPU time limit exceeded
   SIGXFSZ    : constant := 34; --  filesize limit exceeded
   SIGSTKFLT  : constant := 36; --  coprocessor stack fault (Linux)
   SIGLTHRRES : constant := 37; --  GNU/LinuxThreads restart signal
   SIGLTHRCAN : constant := 38; --  GNU/LinuxThreads cancel signal
   SIGLTHRDBG : constant := 39; --  GNU/LinuxThreads debugger signal

   --  struct_sigaction offsets

   sa_flags_pos : constant := Standard'Address_Size / 8;
   sa_mask_pos  : constant := sa_flags_pos * 2;

   SA_SIGINFO : constant := 16#10#;
   SA_ONSTACK : constant := 16#01#;

   type lock_array is array (1 .. 4) of Integer;
   type atomic_lock_t is record
      lock : lock_array;
   end record;
   pragma Convention (C, atomic_lock_t);
   for atomic_lock_t'Alignment use 16;

   type struct_pthread_fast_lock is record
      spinlock : atomic_lock_t;
      status   : Long_Integer;
   end record;
   pragma Convention (C, struct_pthread_fast_lock);

   type pthread_mutex_t is record
      m_reserved : Integer;
      m_count    : Integer;
      m_owner    : System.Address;
      m_kind     : Integer;
      m_lock     : struct_pthread_fast_lock;
   end record;
   pragma Convention (C, pthread_mutex_t);

end System.Linux;
