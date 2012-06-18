------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--     S Y S T E M . T A S K _ P R I M I T I V E S . O P E R A T I O N S    --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--         Copyright (C) 1992-2012, Free Software Foundation, Inc.          --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This is a no tasking version of this package

--  This package contains all the GNULL primitives that interface directly with
--  the underlying OS.

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during tasking
--  operations. It causes infinite loops and other problems.

package body System.Task_Primitives.Operations is

   use System.Tasking;
   use System.Parameters;

   pragma Warnings (Off);
   --  Turn off warnings since so many unreferenced parameters

   --------------
   -- Specific --
   --------------

   --  Package Specific contains target specific routines, and the body of
   --  this package is target specific.

   package Specific is
      procedure Set (Self_Id : Task_Id);
      pragma Inline (Set);
      --  Set the self id for the current task
   end Specific;

   package body Specific is

      ---------
      -- Set --
      ---------

      procedure Set (Self_Id : Task_Id) is
      begin
         null;
      end Set;
   end Specific;

   ----------------------------------
   -- ATCB allocation/deallocation --
   ----------------------------------

   package body ATCB_Allocation is separate;
   --  The body of this package is shared across several targets

   ----------------
   -- Abort_Task --
   ----------------

   procedure Abort_Task (T : Task_Id) is
   begin
      null;
   end Abort_Task;

   ----------------
   -- Check_Exit --
   ----------------

   function Check_Exit (Self_ID : ST.Task_Id) return Boolean is
   begin
      return True;
   end Check_Exit;

   --------------------
   -- Check_No_Locks --
   --------------------

   function Check_No_Locks (Self_ID : ST.Task_Id) return Boolean is
   begin
      return True;
   end Check_No_Locks;

   -------------------
   -- Continue_Task --
   -------------------

   function Continue_Task (T : ST.Task_Id) return Boolean is
   begin
      return False;
   end Continue_Task;

   -------------------
   -- Current_State --
   -------------------

   function Current_State (S : Suspension_Object) return Boolean is
   begin
      return False;
   end Current_State;

   ----------------------
   -- Environment_Task --
   ----------------------

   function Environment_Task return Task_Id is
   begin
      return null;
   end Environment_Task;

   -----------------
   -- Create_Task --
   -----------------

   procedure Create_Task
     (T          : Task_Id;
      Wrapper    : System.Address;
      Stack_Size : System.Parameters.Size_Type;
      Priority   : System.Any_Priority;
      Succeeded  : out Boolean)
   is
   begin
      Succeeded := False;
   end Create_Task;

   ----------------
   -- Enter_Task --
   ----------------

   procedure Enter_Task (Self_ID : Task_Id) is
   begin
      null;
   end Enter_Task;

   ---------------
   -- Exit_Task --
   ---------------

   procedure Exit_Task is
   begin
      null;
   end Exit_Task;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (S : in out Suspension_Object) is
   begin
      null;
   end Finalize;

   -------------------
   -- Finalize_Lock --
   -------------------

   procedure Finalize_Lock (L : not null access Lock) is
   begin
      null;
   end Finalize_Lock;

   procedure Finalize_Lock (L : not null access RTS_Lock) is
   begin
      null;
   end Finalize_Lock;

   ------------------
   -- Finalize_TCB --
   ------------------

   procedure Finalize_TCB (T : Task_Id) is
   begin
      null;
   end Finalize_TCB;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority (T : Task_Id) return System.Any_Priority is
   begin
      return 0;
   end Get_Priority;

   --------------------
   -- Get_Thread_Id  --
   --------------------

   function Get_Thread_Id (T : ST.Task_Id) return OSI.Thread_Id is
   begin
      return OSI.Thread_Id (T.Common.LL.Thread);
   end Get_Thread_Id;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Environment_Task : Task_Id) is
      No_Tasking : Boolean;
   begin
      raise Program_Error with "tasking not implemented on this configuration";
   end Initialize;

   procedure Initialize (S : in out Suspension_Object) is
   begin
      null;
   end Initialize;

   ---------------------
   -- Initialize_Lock --
   ---------------------

   procedure Initialize_Lock
     (Prio : System.Any_Priority;
      L    : not null access Lock)
   is
   begin
      null;
   end Initialize_Lock;

   procedure Initialize_Lock
     (L : not null access RTS_Lock; Level : Lock_Level) is
   begin
      null;
   end Initialize_Lock;

   --------------------
   -- Initialize_TCB --
   --------------------

   procedure Initialize_TCB (Self_ID : Task_Id; Succeeded : out Boolean) is
   begin
      Succeeded := False;
   end Initialize_TCB;

   -------------------
   -- Is_Valid_Task --
   -------------------

   function Is_Valid_Task return Boolean is
   begin
      return False;
   end Is_Valid_Task;

   --------------
   -- Lock_RTS --
   --------------

   procedure Lock_RTS is
   begin
      null;
   end Lock_RTS;

   ---------------------
   -- Monotonic_Clock --
   ---------------------

   function Monotonic_Clock return Duration is
   begin
      return 0.0;
   end Monotonic_Clock;

   ---------------
   -- Read_Lock --
   ---------------

   procedure Read_Lock
     (L                 : not null access Lock;
      Ceiling_Violation : out Boolean)
   is
   begin
      Ceiling_Violation := False;
   end Read_Lock;

   -----------------------------
   -- Register_Foreign_Thread --
   -----------------------------

   function Register_Foreign_Thread return Task_Id is
   begin
      return null;
   end Register_Foreign_Thread;

   -----------------
   -- Resume_Task --
   -----------------

   function Resume_Task
     (T           : ST.Task_Id;
      Thread_Self : OSI.Thread_Id) return Boolean
   is
   begin
      return False;
   end Resume_Task;

   -------------------
   -- RT_Resolution --
   -------------------

   function RT_Resolution return Duration is
   begin
      return 10#1.0#E-6;
   end RT_Resolution;

   ----------
   -- Self --
   ----------

   function Self return Task_Id is
   begin
      return Null_Task;
   end Self;

   -----------------
   -- Set_Ceiling --
   -----------------

   procedure Set_Ceiling
     (L    : not null access Lock;
      Prio : System.Any_Priority)
   is
   begin
      null;
   end Set_Ceiling;

   ---------------
   -- Set_False --
   ---------------

   procedure Set_False (S : in out Suspension_Object) is
   begin
      null;
   end Set_False;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority
     (T                   : Task_Id;
      Prio                : System.Any_Priority;
      Loss_Of_Inheritance : Boolean := False)
   is
   begin
      null;
   end Set_Priority;

   -----------------------
   -- Set_Task_Affinity --
   -----------------------

   procedure Set_Task_Affinity (T : ST.Task_Id) is
   begin
      null;
   end Set_Task_Affinity;

   --------------
   -- Set_True --
   --------------

   procedure Set_True (S : in out Suspension_Object) is
   begin
      null;
   end Set_True;

   -----------
   -- Sleep --
   -----------

   procedure Sleep (Self_ID : Task_Id; Reason : System.Tasking.Task_States) is
   begin
      null;
   end Sleep;

   -----------------
   -- Stack_Guard --
   -----------------

   procedure Stack_Guard (T : ST.Task_Id; On : Boolean) is
   begin
      null;
   end Stack_Guard;

   ------------------
   -- Suspend_Task --
   ------------------

   function Suspend_Task
     (T           : ST.Task_Id;
      Thread_Self : OSI.Thread_Id) return Boolean
   is
   begin
      return False;
   end Suspend_Task;

   --------------------
   -- Stop_All_Tasks --
   --------------------

   procedure Stop_All_Tasks is
   begin
      null;
   end Stop_All_Tasks;

   ---------------
   -- Stop_Task --
   ---------------

   function Stop_Task (T : ST.Task_Id) return Boolean is
      pragma Unreferenced (T);
   begin
      return False;
   end Stop_Task;

   ------------------------
   -- Suspend_Until_True --
   ------------------------

   procedure Suspend_Until_True (S : in out Suspension_Object) is
   begin
      null;
   end Suspend_Until_True;

   -----------------
   -- Timed_Delay --
   -----------------

   procedure Timed_Delay
     (Self_ID : Task_Id;
      Time    : Duration;
      Mode    : ST.Delay_Modes)
   is
   begin
      null;
   end Timed_Delay;

   -----------------
   -- Timed_Sleep --
   -----------------

   procedure Timed_Sleep
     (Self_ID  : Task_Id;
      Time     : Duration;
      Mode     : ST.Delay_Modes;
      Reason   : System.Tasking.Task_States;
      Timedout : out Boolean;
      Yielded  : out Boolean)
   is
   begin
      Timedout := False;
      Yielded := False;
   end Timed_Sleep;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (L : not null access Lock) is
   begin
      null;
   end Unlock;

   procedure Unlock
     (L           : not null access RTS_Lock;
      Global_Lock : Boolean := False)
   is
   begin
      null;
   end Unlock;

   procedure Unlock (T : Task_Id) is
   begin
      null;
   end Unlock;

   ----------------
   -- Unlock_RTS --
   ----------------

   procedure Unlock_RTS is
   begin
      null;
   end Unlock_RTS;
   ------------
   -- Wakeup --
   ------------

   procedure Wakeup (T : Task_Id; Reason : System.Tasking.Task_States) is
   begin
      null;
   end Wakeup;

   ----------------
   -- Write_Lock --
   ----------------

   procedure Write_Lock
     (L                 : not null access Lock;
      Ceiling_Violation : out Boolean)
   is
   begin
      Ceiling_Violation := False;
   end Write_Lock;

   procedure Write_Lock
     (L           : not null access RTS_Lock;
      Global_Lock : Boolean := False)
   is
   begin
      null;
   end Write_Lock;

   procedure Write_Lock (T : Task_Id) is
   begin
      null;
   end Write_Lock;

   -----------
   -- Yield --
   -----------

   procedure Yield (Do_Yield : Boolean := True) is
   begin
      null;
   end Yield;

end System.Task_Primitives.Operations;
