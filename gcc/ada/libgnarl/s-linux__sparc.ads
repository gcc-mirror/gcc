------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                          S Y S T E M .  L I N U X                        --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--             Copyright (C) 2009-2021, Free Software Foundation, Inc.      --
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

--  This is the SPARC version of this package

--  This package encapsulates cpu specific differences between implementations
--  of GNU/Linux, in order to share s-osinte-linux.ads.

--  PLEASE DO NOT add any with-clauses to this package or remove the pragma
--  Preelaborate. This package is designed to be a bottom-level (leaf) package

with Interfaces.C;
with System.Parameters;

package System.Linux is
   pragma Preelaborate;

   ----------
   -- Time --
   ----------

   subtype long        is Interfaces.C.long;
   subtype suseconds_t is Interfaces.C.long;
   type time_t is range -2 ** (System.Parameters.time_t_bits - 1)
     .. 2 ** (System.Parameters.time_t_bits - 1) - 1;
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
   ETIMEDOUT : constant := 60;

   -------------
   -- Signals --
   -------------

   SIGHUP     : constant := 1; --  hangup
   SIGINT     : constant := 2; --  interrupt (rubout)
   SIGQUIT    : constant := 3; --  quit (ASCD FS)
   SIGILL     : constant := 4; --  illegal instruction (not reset)
   SIGTRAP    : constant := 5; --  trace trap (not reset)
   SIGABRT    : constant := 6; --  used by abort, replace SIGIOT in the future
   SIGIOT     : constant := 6; --  IOT instruction
   SIGEMT     : constant := 7; --  EMT
   SIGFPE     : constant := 8; --  floating point exception
   SIGKILL    : constant := 9; --  kill (cannot be caught or ignored)
   SIGBUS     : constant := 10; --  bus error
   SIGSEGV    : constant := 11; --  segmentation violation
   SIGSYS     : constant := 12; --  bad system call
   SIGPIPE    : constant := 13; --  write on a pipe with no one to read it
   SIGALRM    : constant := 14; --  alarm clock
   SIGTERM    : constant := 15; --  software termination signal from kill
   SIGURG     : constant := 16; --  urgent condition on IO channel
   SIGSTOP    : constant := 17; --  stop (cannot be caught or ignored)
   SIGTSTP    : constant := 18; --  user stop requested from tty
   SIGCONT    : constant := 19; --  stopped process has been continued
   SIGCHLD    : constant := 20; --  child status change
   SIGCLD     : constant := 20; --  alias for SIGCHLD
   SIGTTIN    : constant := 21; --  background tty read attempted
   SIGTTOU    : constant := 22; --  background tty write attempted
   SIGIO      : constant := 23; --  I/O now possible (4.2 BSD)
   SIGPOLL    : constant := 23; --  pollable event occurred
   SIGXCPU    : constant := 24; --  CPU time limit exceeded
   SIGXFSZ    : constant := 25; --  filesize limit exceeded
   SIGVTALRM  : constant := 26; --  virtual timer expired
   SIGPROF    : constant := 27; --  profiling timer expired
   SIGWINCH   : constant := 28; --  window size change
   SIGLOST    : constant := 29; --  File lock lost
   SIGPWR     : constant := 29; --  power-fail restart
   SIGUSR1    : constant := 30; --  user defined signal 1
   SIGUSR2    : constant := 31; --  user defined signal 2

   SIG32      : constant := 32; --  glibc internal signal
   SIG33      : constant := 33; --  glibc internal signal
   SIG34      : constant := 34; --  glibc internal signal

   SIGUNUSED  : constant := 0;
   SIGSTKFLT  : constant := 0;
   --  These don't exist for Linux/SPARC.  The constants are present
   --  so that we can continue to use a-intnam-linux.ads.

   --  struct_sigaction offsets

   sa_handler_pos : constant := 0;
   sa_mask_pos    : constant := Standard'Address_Size / 8;
   sa_flags_pos   : constant := 128 + sa_mask_pos;

   SA_SIGINFO  : constant := 16#200#;
   SA_ONSTACK  : constant := 16#001#;

end System.Linux;
