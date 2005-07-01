------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . T A S K _ I N F O                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2003 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains the definitions and routines associated with the
--  implementation and use of the Task_Info pragma. It is specialized
--  appropriately for targets that make use of this pragma.

--  Note: the compiler generates direct calls to this interface, via Rtsfind.
--  Any changes to this interface may require corresponding compiler changes.

--  This unit may be used directly from an application program by providing
--  an appropriate WITH, and the interface can be expected to remain stable.

--  This is the IRIX (kernel threads) version of this package

with Interfaces.C;
with System.OS_Interface;

package System.Task_Info is
   pragma Elaborate_Body;
   --  To ensure that a body is allowed

   package OSI renames System.OS_Interface;

   -----------------------------------------
   -- Implementation of Task_Info Feature --
   -----------------------------------------

   --  Pragma Task_Info allows an application to set the underlying
   --  pthread scheduling attributes for a specific task.

   ------------------
   -- Declarations --
   ------------------

   type Thread_Scheduling_Scope is
     (PTHREAD_SCOPE_PROCESS, PTHREAD_SCOPE_SYSTEM);

   for Thread_Scheduling_Scope'Size use Interfaces.C.int'Size;

   type Thread_Scheduling_Inheritance is
      (PTHREAD_EXPLICIT_SCHED, PTHREAD_INHERIT_SCHED);

   for Thread_Scheduling_Inheritance'Size use Interfaces.C.int'Size;

   type Thread_Scheduling_Policy is
      (SCHED_FIFO,   --  The first-in-first-out real-time policy
       SCHED_RR,     --  The round-robin real-time scheduling policy
       SCHED_TS);    --  The timeshare earnings based scheduling policy

   for Thread_Scheduling_Policy'Size use Interfaces.C.int'Size;
   for Thread_Scheduling_Policy use
      (SCHED_FIFO => 1,
       SCHED_RR   => 2,
       SCHED_TS   => 3);

   function SCHED_OTHER return Thread_Scheduling_Policy renames SCHED_TS;

   No_Specified_Priority : constant := -1;

   subtype Thread_Scheduling_Priority is Integer range
     No_Specified_Priority .. 255;

   function Min (Policy : Interfaces.C.int) return Interfaces.C.int
     renames OSI.sched_get_priority_min;

   function Max (Policy : Interfaces.C.int) return Interfaces.C.int
     renames OSI.sched_get_priority_max;

   subtype FIFO_Priority is Thread_Scheduling_Priority range
      Thread_Scheduling_Priority (Min (OSI.SCHED_FIFO)) ..
      Thread_Scheduling_Priority (Max (OSI.SCHED_FIFO));

   subtype RR_Priority is Thread_Scheduling_Priority range
      Thread_Scheduling_Priority (Min (OSI.SCHED_RR)) ..
      Thread_Scheduling_Priority (Max (OSI.SCHED_RR));

   subtype TS_Priority is Thread_Scheduling_Priority range
      Thread_Scheduling_Priority (Min (OSI.SCHED_TS)) ..
      Thread_Scheduling_Priority (Max (OSI.SCHED_TS));

   subtype OTHER_Priority is Thread_Scheduling_Priority range
      Thread_Scheduling_Priority (Min (OSI.SCHED_OTHER)) ..
      Thread_Scheduling_Priority (Max (OSI.SCHED_OTHER));

   subtype CPU_Number is Integer range -1 .. Integer'Last;
   ANY_CPU : constant CPU_Number := CPU_Number'First;

   type Thread_Attributes is record
      Scope       : Thread_Scheduling_Scope       := PTHREAD_SCOPE_PROCESS;
      Inheritance : Thread_Scheduling_Inheritance := PTHREAD_EXPLICIT_SCHED;
      Policy      : Thread_Scheduling_Policy      := SCHED_RR;
      Priority    : Thread_Scheduling_Priority    := No_Specified_Priority;
      Runon_CPU   : CPU_Number                    := ANY_CPU;
   end record;

   Default_Thread_Attributes : constant Thread_Attributes :=
     (PTHREAD_SCOPE_PROCESS, PTHREAD_EXPLICIT_SCHED, SCHED_RR,
       No_Specified_Priority, ANY_CPU);

   type Task_Info_Type is access all Thread_Attributes;

   Unspecified_Task_Info : constant Task_Info_Type := null;
   --  Value passed to task in the absence of a Task_Info pragma

end System.Task_Info;
