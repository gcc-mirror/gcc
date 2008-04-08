------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                          S Y S T E M .  L I N U X                        --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--             Copyright (C) 2008, Free Software Foundation, Inc.           --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------

--  This is the alpha version of this package

--  This package encapsulates cpu specific differences between implementations
--  of GNU/Linux, in order to share s-osinte-linux.ads.

--  PLEASE DO NOT add any with-clauses to this package or remove the pragma
--  Preelaborate. This package is designed to be a bottom-level (leaf) package.

with Interfaces.C;

package System.Linux is
   pragma Preelaborate;

   -----------
   -- Errno --
   -----------

   EAGAIN    : constant := 35;
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
   SIGIOT     : constant := 6; --  IOT instruction
   SIGABRT    : constant := 6; --  used by abort, replace SIGIOT in the  future
   SIGFPE     : constant := 8; --  floating point exception
   SIGKILL    : constant := 9; --  kill (cannot be caught or ignored)
   SIGBUS     : constant := 10; --  bus error
   SIGSEGV    : constant := 11; --  segmentation violation
   SIGPIPE    : constant := 13; --  write on a pipe with no one to read it
   SIGALRM    : constant := 14; --  alarm clock
   SIGTERM    : constant := 15; --  software termination signal from kill
   SIGURG     : constant := 16; --  urgent condition on IO channel
   SIGSTOP    : constant := 17; --  stop (cannot be caught or ignored)
   SIGTSTP    : constant := 18; --  user stop requested from tty
   SIGCONT    : constant := 19; --  stopped process has been continued
   SIGCLD     : constant := 20; --  alias for SIGCHLD
   SIGCHLD    : constant := 20; --  child status change
   SIGTTIN    : constant := 21; --  background tty read attempted
   SIGTTOU    : constant := 22; --  background tty write attempted
   SIGIO      : constant := 23; --  I/O now possible (4.2 BSD)
   SIGPOLL    : constant := 23; --  pollable event occurred
   SIGXCPU    : constant := 24; --  CPU time limit exceeded
   SIGXFSZ    : constant := 25; --  filesize limit exceeded
   SIGVTALRM  : constant := 26; --  virtual timer expired
   SIGPROF    : constant := 27; --  profiling timer expired
   SIGWINCH   : constant := 28; --  window size change
   SIGPWR     : constant := 29; --  power-fail restart
   SIGUSR1    : constant := 30; --  user defined signal 1
   SIGUSR2    : constant := 31; --  user defined signal 2

   SIGLTHRRES : constant := 32; --  GNU/LinuxThreads restart signal
   SIGLTHRCAN : constant := 33; --  GNU/LinuxThreads cancel signal
   SIGLTHRDBG : constant := 34; --  GNU/LinuxThreads debugger signal

   SIGADAABORT : constant := SIGABRT;
   --  Change this if you want to use another signal for task abort.
   --  SIGTERM might be a good one.

   SIGUNUSED  : constant := 0;
   SIGSTKFLT  : constant := 0;
   SIGLOST    : constant := 0;
   --  These don't exist for Linux/Alpha.  The constants are present
   --  so that we can continue to use a-intnam-linux.ads.

   --  struct_sigaction offsets

   sa_mask_pos  : constant := Standard'Address_Size / 8;
   sa_flags_pos : constant := 128 + sa_mask_pos;

   SA_SIGINFO  : constant := 16#40#;
   SA_ONSTACK  : constant := 16#01#;

   type pthread_mutex_t is record
      dum0, dum1, dum2, dum3, dum4 : Interfaces.C.unsigned_long;
   end record;
   pragma Convention (C, pthread_mutex_t);

end System.Linux;
