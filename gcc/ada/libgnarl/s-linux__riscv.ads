------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                          S Y S T E M .  L I N U X                        --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--             Copyright (C) 2009-2019, Free Software Foundation, Inc.      --
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
------------------------------------------------------------------------------

--  This is the RISC-V version of this package

--  This package encapsulates cpu specific differences between implementations
--  of GNU/Linux, in order to share s-osinte-linux.ads.

--  PLEASE DO NOT add any with-clauses to this package or remove the pragma
--  Preelaborate. This package is designed to be a bottom-level (leaf) package

with Interfaces.C;

package System.Linux is
   pragma Preelaborate;

   ----------
   -- Time --
   ----------

   subtype int         is Interfaces.C.int;
   subtype long        is Interfaces.C.long;
   subtype suseconds_t is Interfaces.C.long;
   subtype time_t      is Interfaces.C.long;
   subtype clockid_t   is Interfaces.C.int;

   type timespec is record
      tv_sec  : time_t;
      tv_nsec : long;
   end record;
   pragma Convention (C, timespec);

   type timeval is record
      tv_sec  : time_t;
      tv_usec : suseconds_t;
   end record;
   pragma Convention (C, timeval);

   -----------
   -- Errno --
   -----------

   EAGAIN    : constant := 11;
   EINTR     : constant := 4;
   EINVAL    : constant := 22;
   ENOMEM    : constant := 12;
   EPERM     : constant := 1;
   ETIMEDOUT : constant := 110;

   -------------
   -- Signals --
   -------------

   SIGHUP     : constant := 1; --  hangup
   SIGINT     : constant := 2; --  interrupt (rubout)
   SIGQUIT    : constant := 3; --  quit (ASCD FS)
   SIGILL     : constant := 4; --  illegal instruction (not reset)
   SIGTRAP    : constant := 5; --  trace trap (not reset)
   SIGIOT     : constant := 6; --  IOT instruction
   SIGABRT    : constant := 6; --  used by abort, replace SIGIOT in the future
   SIGBUS     : constant := 7; --  bus error
   SIGFPE     : constant := 8; --  floating point exception
   SIGKILL    : constant := 9; --  kill (cannot be caught or ignored)
   SIGUSR1    : constant := 10; --  user defined signal 1
   SIGSEGV    : constant := 11; --  segmentation violation
   SIGUSR2    : constant := 12; --  user defined signal 2
   SIGPIPE    : constant := 13; --  write on a pipe with no one to read it
   SIGALRM    : constant := 14; --  alarm clock
   SIGTERM    : constant := 15; --  software termination signal from kill
   SIGSTKFLT  : constant := 16; --  coprocessor stack fault (Linux)
   SIGCLD     : constant := 17; --  alias for SIGCHLD
   SIGCHLD    : constant := 17; --  child status change
   SIGCONT    : constant := 18; --  stopped process has been continued
   SIGSTOP    : constant := 19; --  stop (cannot be caught or ignored)
   SIGTSTP    : constant := 20; --  user stop requested from tty
   SIGTTIN    : constant := 21; --  background tty read attempted
   SIGTTOU    : constant := 22; --  background tty write attempted
   SIGURG     : constant := 23; --  urgent condition on IO channel
   SIGXCPU    : constant := 24; --  CPU time limit exceeded
   SIGXFSZ    : constant := 25; --  filesize limit exceeded
   SIGVTALRM  : constant := 26; --  virtual timer expired
   SIGPROF    : constant := 28; --  profiling timer expired
   SIGWINCH   : constant := 28; --  window size change
   SIGPOLL    : constant := 29; --  pollable event occurred
   SIGIO      : constant := 29; --  I/O now possible (4.2 BSD)
   SIGPWR     : constant := 30; --  power-fail restart
   SIGSYS     : constant := 31; --  bad system call
   SIG32      : constant := 32; --  glibc internal signal
   SIG33      : constant := 33; --  glibc internal signal
   SIG34      : constant := 34; --  glibc internal signal

   --  These don't exist for Linux/RISC-V.  The constants are present
   --  so that we can continue to use a-intnam-linux.ads.
   SIGLOST    : constant := 0; --  File lock lost
   SIGUNUSED  : constant := 0; --  unused signal (GNU/Linux)
   SIGEMT     : constant := 0; --  EMT

   --  struct_sigaction offsets

   sa_handler_pos : constant := 0;
   sa_mask_pos    : constant := long'Size / 8;
   sa_flags_pos   : constant := long'Size / 8 + 128;

   SA_SIGINFO  : constant := 16#04#;
   SA_ONSTACK  : constant := 16#08000000#;

end System.Linux;
