------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                   A D A . E X E C U T I O N _ T I M E                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2007-2016, Free Software Foundation, Inc.          --
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

--  This is the Darwin version of this package

with Ada.Task_Identification;  use Ada.Task_Identification;
with Ada.Unchecked_Conversion;

with System.Tasking;
with System.OS_Interface; use System.OS_Interface;
with System.Task_Primitives.Operations; use System.Task_Primitives.Operations;

with Interfaces.C; use Interfaces.C;

package body Ada.Execution_Time is

   ---------
   -- "+" --
   ---------

   function "+"
     (Left  : CPU_Time;
      Right : Ada.Real_Time.Time_Span) return CPU_Time
   is
      use type Ada.Real_Time.Time;
   begin
      return CPU_Time (Ada.Real_Time.Time (Left) + Right);
   end "+";

   function "+"
     (Left  : Ada.Real_Time.Time_Span;
      Right : CPU_Time) return CPU_Time
   is
      use type Ada.Real_Time.Time;
   begin
      return CPU_Time (Left + Ada.Real_Time.Time (Right));
   end "+";

   ---------
   -- "-" --
   ---------

   function "-"
     (Left  : CPU_Time;
      Right : Ada.Real_Time.Time_Span) return CPU_Time
   is
      use type Ada.Real_Time.Time;
   begin
      return CPU_Time (Ada.Real_Time.Time (Left) - Right);
   end "-";

   function "-"
     (Left  : CPU_Time;
      Right : CPU_Time) return Ada.Real_Time.Time_Span
   is
      use type Ada.Real_Time.Time;
   begin
      return (Ada.Real_Time.Time (Left) - Ada.Real_Time.Time (Right));
   end "-";

   -----------
   -- Clock --
   -----------

   function Clock
     (T : Ada.Task_Identification.Task_Id :=
        Ada.Task_Identification.Current_Task) return CPU_Time
   is
      function Convert_Ids is new
        Ada.Unchecked_Conversion (Task_Id, System.Tasking.Task_Id);

      function To_CPU_Time is
        new Ada.Unchecked_Conversion (Duration, CPU_Time);
      --  Time is equal to Duration (although it is a private type) and
      --  CPU_Time is equal to Time.

      subtype integer_t is Interfaces.C.int;
      subtype mach_port_t is integer_t;
      --  Type definition for Mach.

      type time_value_t is record
         seconds : integer_t;
         microseconds : integer_t;
      end record;
      pragma Convention (C, time_value_t);
      --  Mach time_value_t

      type thread_basic_info_t is record
         user_time     : time_value_t;
         system_time   : time_value_t;
         cpu_usage     : integer_t;
         policy        : integer_t;
         run_state     : integer_t;
         flags         : integer_t;
         suspend_count : integer_t;
         sleep_time    : integer_t;
      end record;
      pragma Convention (C, thread_basic_info_t);
      --  Mach structure from thread_info.h

      THREAD_BASIC_INFO       : constant := 3;
      THREAD_BASIC_INFO_COUNT : constant := 10;
      --  Flavors for basic info

      function thread_info (Target : mach_port_t;
                            Flavor : integer_t;
                            Thread_Info : System.Address;
                            Count : System.Address) return integer_t;
      pragma Import (C, thread_info);
      --  Mach call to get info on a thread

      function pthread_mach_thread_np (Thread : pthread_t) return mach_port_t;
      pragma Import (C, pthread_mach_thread_np);
      --  Get Mach thread from posix thread

      Result    : Interfaces.C.int;
      Thread    : pthread_t;
      Port      : mach_port_t;
      Ti        : thread_basic_info_t;
      Count     : integer_t;
   begin
      if T = Ada.Task_Identification.Null_Task_Id then
         raise Program_Error;
      end if;

      Thread := Get_Thread_Id (Convert_Ids (T));
      Port := pthread_mach_thread_np (Thread);
      pragma Assert (Port > 0);

      Count := THREAD_BASIC_INFO_COUNT;
      Result := thread_info (Port, THREAD_BASIC_INFO,
                             Ti'Address, Count'Address);
      pragma Assert (Result = 0);
      pragma Assert (Count = THREAD_BASIC_INFO_COUNT);

      return To_CPU_Time
        (Duration (Ti.user_time.seconds + Ti.system_time.seconds)
           + Duration (Ti.user_time.microseconds
                         + Ti.system_time.microseconds) / 1E6);
   end Clock;

   --------------------------
   -- Clock_For_Interrupts --
   --------------------------

   function Clock_For_Interrupts return CPU_Time is
   begin
      --  According to AI 0170-1, D.14(18.1/3), if Interrupt_Clocks_Supported
      --  is set to False the function raises Program_Error.

      raise Program_Error;
      return CPU_Time_First;
   end Clock_For_Interrupts;

   -----------
   -- Split --
   -----------

   procedure Split
     (T  : CPU_Time;
      SC : out Ada.Real_Time.Seconds_Count;
      TS : out Ada.Real_Time.Time_Span)
   is
      use type Ada.Real_Time.Time;
   begin
      Ada.Real_Time.Split (Ada.Real_Time.Time (T), SC, TS);
   end Split;

   -------------
   -- Time_Of --
   -------------

   function Time_Of
     (SC : Ada.Real_Time.Seconds_Count;
      TS : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_Zero)
      return CPU_Time
   is
   begin
      return CPU_Time (Ada.Real_Time.Time_Of (SC, TS));
   end Time_Of;

end Ada.Execution_Time;
