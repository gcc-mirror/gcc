------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS              --
--                                                                          --
--                   A D A . I N T E R R U P T S . N A M E S                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                             $Revision: 1.1.16.1 $
--                                                                          --
--           Copyright (C) 1991-2001 Free Software Foundation, Inc.         --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is the VxWorks version of this package.
--
--  The following signals are reserved by the run time:
--
--  SIGFPE, SIGILL, SIGSEGV, SIGBUS, SIGABRT
--
--  The pragma Unreserve_All_Interrupts affects the following signal(s):
--
--  none

--  This target-dependent package spec contains names of interrupts
--  supported by the local system.

with System.OS_Interface;
with System.VxWorks;

package Ada.Interrupts.Names is

   subtype Hardware_Interrupts is Interrupt_ID
     range Interrupt_ID'First .. System.OS_Interface.Max_HW_Interrupt;
   --  Range of values that can be used for hardware interrupts.

   --  The following constants can be used for software interrupts mapped to
   --  user-level signals:

   SIGHUP : constant Interrupt_ID;
   --  hangup

   SIGINT : constant Interrupt_ID;
   --  interrupt

   SIGQUIT : constant Interrupt_ID;
   --  quit

   SIGILL : constant Interrupt_ID;
   --  illegal instruction (not reset)

   SIGTRAP : constant Interrupt_ID;
   --  trace trap (not reset)

   SIGIOT : constant Interrupt_ID;
   --  IOT instruction

   SIGABRT : constant Interrupt_ID;
   --  used by abort, replace SIGIOT

   SIGEMT : constant Interrupt_ID;
   --  EMT instruction

   SIGFPE : constant Interrupt_ID;
   --  floating point exception

   SIGKILL : constant Interrupt_ID;
   --  kill (cannot be caught or ignored)

   SIGBUS : constant Interrupt_ID;
   --  bus error

   SIGSEGV : constant Interrupt_ID;
   --  segmentation violation

   SIGSYS : constant Interrupt_ID;
   --  bad argument to system call

   SIGPIPE : constant Interrupt_ID;
   --  no one to read it

   SIGALRM : constant Interrupt_ID;
   --  alarm clock

   SIGTERM : constant Interrupt_ID;
   --  software termination signal from kill

   SIGURG : constant Interrupt_ID;
   --  urgent condition on IO channel

   SIGSTOP : constant Interrupt_ID;
   --  stop (cannot be caught or ignored)

   SIGTSTP : constant Interrupt_ID;
   --  user stop requested from tty

   SIGCONT : constant Interrupt_ID;
   --  stopped process has been continued

   SIGCHLD : constant Interrupt_ID;
   --  child status change

   SIGTTIN : constant Interrupt_ID;
   --  background tty read attempted

   SIGTTOU : constant Interrupt_ID;
   --  background tty write attempted

   SIGIO : constant Interrupt_ID;
   --  input/output possible,

   SIGXCPU : constant Interrupt_ID;
   --  CPU time limit exceeded

   SIGXFSZ : constant Interrupt_ID;
   --  filesize limit exceeded

   SIGVTALRM : constant Interrupt_ID;
   --  virtual timer expired

   SIGPROF : constant Interrupt_ID;
   --  profiling timer expired

   SIGWINCH : constant Interrupt_ID;
   --  window size change

   SIGUSR1 : constant Interrupt_ID;
   --  user defined signal 1

   SIGUSR2 : constant Interrupt_ID;
   --  user defined signal 2

private

   Signal_Base : constant := System.VxWorks.Num_HW_Interrupts;

   SIGHUP     : constant Interrupt_ID :=  1 + Signal_Base;
   SIGINT     : constant Interrupt_ID :=  2 + Signal_Base;
   SIGQUIT    : constant Interrupt_ID :=  3 + Signal_Base;
   SIGILL     : constant Interrupt_ID :=  4 + Signal_Base;
   SIGTRAP    : constant Interrupt_ID :=  5 + Signal_Base;
   SIGIOT     : constant Interrupt_ID :=  6 + Signal_Base;
   SIGABRT    : constant Interrupt_ID :=  6 + Signal_Base;
   SIGEMT     : constant Interrupt_ID :=  7 + Signal_Base;
   SIGFPE     : constant Interrupt_ID :=  8 + Signal_Base;
   SIGKILL    : constant Interrupt_ID :=  9 + Signal_Base;
   SIGBUS     : constant Interrupt_ID := 10 + Signal_Base;
   SIGSEGV    : constant Interrupt_ID := 11 + Signal_Base;
   SIGSYS     : constant Interrupt_ID := 12 + Signal_Base;
   SIGPIPE    : constant Interrupt_ID := 13 + Signal_Base;
   SIGALRM    : constant Interrupt_ID := 14 + Signal_Base;
   SIGTERM    : constant Interrupt_ID := 15 + Signal_Base;
   SIGURG     : constant Interrupt_ID := 16 + Signal_Base;
   SIGSTOP    : constant Interrupt_ID := 17 + Signal_Base;
   SIGTSTP    : constant Interrupt_ID := 18 + Signal_Base;
   SIGCONT    : constant Interrupt_ID := 19 + Signal_Base;
   SIGCHLD    : constant Interrupt_ID := 20 + Signal_Base;
   SIGTTIN    : constant Interrupt_ID := 21 + Signal_Base;
   SIGTTOU    : constant Interrupt_ID := 22 + Signal_Base;
   SIGIO      : constant Interrupt_ID := 23 + Signal_Base;
   SIGXCPU    : constant Interrupt_ID := 24 + Signal_Base;
   SIGXFSZ    : constant Interrupt_ID := 25 + Signal_Base;
   SIGVTALRM  : constant Interrupt_ID := 26 + Signal_Base;
   SIGPROF    : constant Interrupt_ID := 27 + Signal_Base;
   SIGWINCH   : constant Interrupt_ID := 28 + Signal_Base;
   SIGUSR1    : constant Interrupt_ID := 30 + Signal_Base;
   SIGUSR2    : constant Interrupt_ID := 31 + Signal_Base;

end Ada.Interrupts.Names;
