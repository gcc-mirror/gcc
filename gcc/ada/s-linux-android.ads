------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                          S Y S T E M .  L I N U X                        --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--            Copyright (C) 2014, Free Software Foundation, Inc.            --
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

package System.Linux is
   pragma Preelaborate;

   ------------
   -- time_t --
   ------------

   type time_t is new Long_Integer;

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
   SIGABRT    : constant := 6; --  used by abort, replace SIGIOT in the  future
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
   SIGUNUSED  : constant := 31; --  unused signal (GNU/Linux)
   SIGSTKFLT  : constant := 16; --  coprocessor stack fault (Linux)
   SIGLTHRRES : constant := 32; --  GNU/LinuxThreads restart signal
   SIGLTHRCAN : constant := 33; --  GNU/LinuxThreads cancel signal
   SIGLTHRDBG : constant := 34; --  GNU/LinuxThreads debugger signal

   --  struct_sigaction offsets

   sa_handler_pos : constant := 0;
   sa_mask_pos    : constant := Standard'Address_Size / 8;
   sa_flags_pos   : constant := 4 + sa_mask_pos;

   SA_SIGINFO  : constant := 16#00000004#;
   SA_ONSTACK  : constant := 16#08000000#;
   SA_RESTART  : constant := 16#10000000#;
   SA_NODEFER  : constant := 16#40000000#;

end System.Linux;
