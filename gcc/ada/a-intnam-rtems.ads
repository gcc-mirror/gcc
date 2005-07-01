------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   A D A . I N T E R R U P T S . N A M E S                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--             Copyright (C) 1991-2002 Free Software Foundation, Inc.       --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
-- The GNARL files that were developed for RTEMS are maintained by  On-Line --
-- Applications Research Corporation (http://www.oarcorp.com)  in  coopera- --
-- tion with Ada Core Technologies Inc. and Florida State University.       --
--                                                                          --
------------------------------------------------------------------------------

--  This is a RTEMS version of this package
--
--  The following signals are reserved by the run time:
--
--  SIGFPE, SIGILL, SIGSEGV, SIGBUS, SIGTRAP, SIGABRT, SIGINT,
--  SIGALRM, SIGEMT, SIGKILL
--
--  The pragma Unreserve_All_Interrupts affects the following signal(s):
--
--  SIGINT: made available for Ada handlers

--  This target-dependent package spec contains names of interrupts
--  supported by the local system.

with System.OS_Interface;
--  used for names of interrupts

package Ada.Interrupts.Names is

   --  Beware that the mapping of names to signals may be
   --  many-to-one.  There may be aliases.  Also, for all
   --  signal names that are not supported on the current system
   --  the value of the corresponding constant will be zero.

   SIGHUP : constant Interrupt_ID :=
     System.OS_Interface.SIGHUP;      --  hangup

   SIGINT : constant Interrupt_ID :=
     System.OS_Interface.SIGINT;      --  interrupt (rubout)

   SIGQUIT : constant Interrupt_ID :=
     System.OS_Interface.SIGQUIT;     --  quit (ASCD FS)

   SIGILL : constant Interrupt_ID :=
     System.OS_Interface.SIGILL;      --  illegal instruction (not reset)

   SIGTRAP : constant Interrupt_ID :=
     System.OS_Interface.SIGTRAP;     --  trace trap (not reset)

   SIGIOT : constant Interrupt_ID :=
     System.OS_Interface.SIGIOT;      --  IOT instruction

   SIGABRT : constant Interrupt_ID := --  used by abort,
     System.OS_Interface.SIGABRT;     --  replace SIGIOT in the  future

   SIGEMT : constant Interrupt_ID :=
     System.OS_Interface.SIGEMT;      --  EMT instruction

   SIGFPE : constant Interrupt_ID :=
     System.OS_Interface.SIGFPE;      --  floating point exception

   SIGKILL : constant Interrupt_ID :=
     System.OS_Interface.SIGKILL;     --  kill (cannot be caught or ignored)

   SIGBUS : constant Interrupt_ID :=
     System.OS_Interface.SIGBUS;      --  bus error

   SIGSEGV : constant Interrupt_ID :=
     System.OS_Interface.SIGSEGV;     --  segmentation violation

   SIGSYS : constant Interrupt_ID :=
     System.OS_Interface.SIGSYS;      --  bad argument to system call

   SIGPIPE : constant Interrupt_ID := --  write on a pipe with
     System.OS_Interface.SIGPIPE;     --  no one to read it

   SIGALRM : constant Interrupt_ID :=
     System.OS_Interface.SIGALRM;     --  alarm clock

   SIGTERM : constant Interrupt_ID :=
     System.OS_Interface.SIGTERM;     --  software termination signal from kill

   SIGUSR1 : constant Interrupt_ID :=
     System.OS_Interface.SIGUSR1;     --  user defined signal 1

   SIGUSR2 : constant Interrupt_ID :=
     System.OS_Interface.SIGUSR2;     --  user defined signal 2

end Ada.Interrupts.Names;
