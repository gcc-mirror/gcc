------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                          S Y S T E M .  L I N U X                        --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--            Copyright (C) 2014-2025, Free Software Foundation, Inc.       --
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
-- In particular,  you can freely  distribute your programs  built with the --
-- GNAT Pro compiler, including any required library run-time units,  using --
-- any licensing terms  of your choosing.  See the AdaCore Software License --
-- for full details.                                                        --
--                                                                          --
--                                                                          --
------------------------------------------------------------------------------

--  This is the Android version of this package

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
   SIGFPE     : constant := 8; --  floating point exception
   SIGKILL    : constant := 9; --  kill (cannot be caught or ignored)
   SIGBUS     : constant := 7; --  bus error
   SIGSEGV    : constant := 11; --  segmentation violation
   SIGPIPE    : constant := 13; --  write on a pipe with no one to read it
   SIGALRM    : constant := 14; --  alarm clock
   SIGTERM    : constant := 15; --  software termination signal from kill
   SIGUSR1    : constant := 10; --  user defined signal 1
   SIGUSR2    : constant := 12; --  user defined signal 2
   SIGCLD     : constant := 17; --  alias for SIGCHLD
   SIGCHLD    : constant := 17; --  child status change
   SIGPWR     : constant := 30; --  power-fail restart
   SIGWINCH   : constant := 28; --  window size change
   SIGURG     : constant := 23; --  urgent condition on IO channel
   SIGPOLL    : constant := 29; --  pollable event occurred
   SIGIO      : constant := 29; --  I/O now possible (4.2 BSD)
   SIGLOST    : constant := 29; --  File lock lost
   SIGSTOP    : constant := 19; --  stop (cannot be caught or ignored)
   SIGTSTP    : constant := 20; --  user stop requested from tty
   SIGCONT    : constant := 18; --  stopped process has been continued
   SIGTTIN    : constant := 21; --  background tty read attempted
   SIGTTOU    : constant := 22; --  background tty write attempted
   SIGVTALRM  : constant := 26; --  virtual timer expired
   SIGPROF    : constant := 27; --  profiling timer expired
   SIGXCPU    : constant := 24; --  CPU time limit exceeded
   SIGXFSZ    : constant := 25; --  filesize limit exceeded
   SIGSYS     : constant := 31; --  bad argument to system call
   SIGUNUSED  : constant := 31; --  unused signal (GNU/Linux)
   SIGSTKFLT  : constant := 16; --  coprocessor stack fault (Linux)
   SIG32      : constant := 32; --  glibc internal signal
   SIG33      : constant := 33; --  glibc internal signal
   SIG34      : constant := 34; --  glibc internal signal

   --  struct_sigaction

   generic
      type sigset_t is private;
   package Android_Sigaction is
      type struct_sigaction is record
         sa_handler  : System.Address;
         sa_mask     : sigset_t;
         sa_flags    : Interfaces.C.int;
         sa_restorer : System.Address;
      end record;
      pragma Convention (C, struct_sigaction);
   end Android_Sigaction;

   SA_SIGINFO  : constant := 16#00000004#;
   SA_ONSTACK  : constant := 16#08000000#;
   SA_RESTART  : constant := 16#10000000#;
   SA_NODEFER  : constant := 16#40000000#;

end System.Linux;
