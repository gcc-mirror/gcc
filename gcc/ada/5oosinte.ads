------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS              --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                                                                          --
--            Copyright (C) 1991-2001 Florida State University              --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNARL was developed by the GNARL team at Florida State University. It is --
-- now maintained by Ada Core Technologies Inc. in cooperation with Florida --
-- State University (http://www.gnat.com).                                  --
--                                                                          --
------------------------------------------------------------------------------

--  This is the OS/2 version of this package

--  This package encapsulates all direct interfaces to OS services
--  that are needed by children of System.

--  PLEASE DO NOT add any with-clauses to this package
--  or remove the pragma Preelaborate.

--  It is designed to be a bottom-level (leaf) package.

with Interfaces.C;
package System.OS_Interface is
   pragma Preelaborate;

   package C renames Interfaces.C;

   subtype int            is C.int;
   subtype unsigned_long  is C.unsigned_long;

   type Duration_In_Millisec is new C.long;
   --  New type to prevent confusing time functions in this package
   --  with time functions returning seconds or other units.

   type Thread_Id is new unsigned_long;

   -----------
   -- Errno --
   -----------

   function errno return int;
   pragma Import (C, errno, "__get_errno");

   EAGAIN   : constant := 5;
   EINTR    : constant := 13;
   EINVAL   : constant := 14;
   ENOMEM   : constant := 25;

   -------------
   -- Signals --
   -------------

   Max_Interrupt : constant := 15;
   type Signal is new int range 0 .. Max_Interrupt;

   --  Signals for OS/2, only SIGTERM used currently. The values are
   --  fake, since OS/2 uses 32 bit exception numbers that cannot be
   --  used to index arrays etc. The GNULLI maps these Unix-like signals
   --  to OS/2 exception numbers.

   --  SIGTERM is used for the abort interrupt.

   SIGHUP     : constant := 1;  --  hangup
   SIGINT     : constant := 2;  --  interrupt (rubout)
   SIGQUIT    : constant := 3;  --  quit (ASCD FS)
   SIGILL     : constant := 4;  --  illegal instruction (not reset)
   SIGTRAP    : constant := 5;  --  trace trap (not reset)
   SIGIOT     : constant := 6;  --  IOT instruction
   SIGEMT     : constant := 0;  --  EMT instruction
   SIGFPE     : constant := 8;  --  floating point exception
   SIGKILL    : constant := 9;  --  kill (cannot be caught or ignored)
   SIGBUS     : constant := 10; --  bus error
   SIGSEGV    : constant := 11; --  segmentation violation
   SIGSYS     : constant := 12; --  bad argument to system call
   SIGPIPE    : constant := 13; --  write on a pipe with no one to read it
   SIGALRM    : constant := 14; --  alarm clock
   SIGTERM    : constant := 15; --  software termination signal from kill

   subtype sigset_t is unsigned_long;

   ----------
   -- Time --
   ----------

   function Clock return Duration;
   pragma Inline (Clock);
   --  Clock measuring time since the epoch, which is the boot-time.
   --  The clock resolution is approximately 838 ns.

   procedure Delay_For (Period : in Duration_In_Millisec);
   pragma Inline (Delay_For);
   --  Changed Sleep to Delay_For, for consistency with System.Time_Operations

   ----------------
   -- Scheduling --
   ----------------

   --  Put the calling task at the end of the ready queue for its priority

   procedure Yield;
   pragma Inline (Yield);

end System.OS_Interface;
