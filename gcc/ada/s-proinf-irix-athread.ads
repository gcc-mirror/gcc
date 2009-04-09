------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 S Y S T E M . P R O G R A M  _  I N F O                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1997-2009, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains the definitions and routines used as parameters to
--  the run-time system at program startup for the SGI implementation.

package System.Program_Info is
   pragma Preelaborate;

   function Initial_Sproc_Count return Integer;
   --  The number of sproc created at program startup for scheduling threads

   function Max_Sproc_Count return Integer;
   --  The maximum number of sprocs that can be created by the program for
   --  servicing threads. This limit includes both the pre-created sprocs and
   --  those explicitly created under program control.

   function Sproc_Stack_Size return Integer;
   --  The size, in bytes, of the sproc's initial stack

   function Default_Time_Slice  return Duration;
   --  The default time quanta for round-robin scheduling of threads of
   --  equal priority.  This default value can be overridden on a per-task
   --  basis by specifying an alternate value via the implementation-defined
   --  Task_Info pragma. See s-tasinf.ads for more information.

   function Default_Task_Stack return Integer;
   --  The default stack size for each created thread. This default value can
   --  be overridden on a per-task basis by the language-defined Storage_Size
   --  pragma.

   function Stack_Guard_Pages return Integer;
   --  The number of non-writable, guard pages to append to the bottom of
   --  each thread's stack.

   function Pthread_Sched_Signal return Integer;
   --  The signal used by the Pthreads library to affect scheduling actions
   --  in remote sprocs.

   function Pthread_Arena_Size return Integer;
   --  The size of the shared arena from which pthread locks are allocated.
   --  See the usinit(3p) man page for more information on shared arenas.

   function Os_Default_Priority return Integer;
   --  The default Irix Non-Degrading priority for each sproc created to
   --  service threads.

end System.Program_Info;
