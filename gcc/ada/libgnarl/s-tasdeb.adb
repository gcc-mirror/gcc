------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                  S Y S T E M . T A S K I N G . D E B U G                 --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1997-2018, Free Software Foundation, Inc.         --
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

--  This package encapsulates all direct interfaces to task debugging services
--  that are needed by gdb with gnat mode.

--  Note : This file *must* be compiled with debugging information

--  Do not add any dependency to GNARL packages since this package is used
--  in both normal and restricted (ravenscar) environments.

pragma Restriction_Warnings (No_Secondary_Stack);
--  We wish to avoid secondary stack usage here, because (e.g.) Trace is called
--  at delicate times, such as during task termination after the secondary
--  stack has been deallocated. It's just a warning, so we don't require
--  partition-wide consistency.

with System.CRTL;
with System.Storage_Elements; use System.Storage_Elements;
with System.Task_Primitives;
with System.Task_Primitives.Operations;

package body System.Tasking.Debug is

   package STPO renames System.Task_Primitives.Operations;

   type Trace_Flag_Set is array (Character) of Boolean;

   Trace_On : Trace_Flag_Set := ('A' .. 'Z' => False, others => True);

   Stderr_Fd : constant := 2;
   --  File descriptor for standard error

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Write (Fd : Integer; S : String; Count : Integer);
   --  Write Count characters of S to the file descriptor Fd

   procedure Put (S : String);
   --  Display S on standard error

   procedure Put_Line (S : String := "");
   --  Display S on standard error with an additional line terminator

   procedure Put_Task_Image (T : Task_Id);
   --  Display relevant characters from T.Common.Task_Image on standard error

   procedure Put_Task_Id_Image (T : Task_Id);
   --  Display address in hexadecimal form on standard error

   ------------------------
   -- Continue_All_Tasks --
   ------------------------

   procedure Continue_All_Tasks is
      C     : Task_Id;
      Dummy : Boolean;

   begin
      STPO.Lock_RTS;

      C := All_Tasks_List;
      while C /= null loop
         Dummy := STPO.Continue_Task (C);
         C := C.Common.All_Tasks_Link;
      end loop;

      STPO.Unlock_RTS;
   end Continue_All_Tasks;

   --------------------
   -- Get_User_State --
   --------------------

   function Get_User_State return Long_Integer is
   begin
      return STPO.Self.User_State;
   end Get_User_State;

   ----------------
   -- List_Tasks --
   ----------------

   procedure List_Tasks is
      C : Task_Id;
   begin
      C := All_Tasks_List;
      while C /= null loop
         Print_Task_Info (C);
         C := C.Common.All_Tasks_Link;
      end loop;
   end List_Tasks;

   ------------------------
   -- Print_Current_Task --
   ------------------------

   procedure Print_Current_Task is
   begin
      Print_Task_Info (STPO.Self);
   end Print_Current_Task;

   ---------------------
   -- Print_Task_Info --
   ---------------------

   procedure Print_Task_Info (T : Task_Id) is
      Entry_Call : Entry_Call_Link;
      Parent     : Task_Id;

   begin
      if T = null then
         Put_Line ("null task");
         return;
      end if;

      Put_Task_Image (T);
      Put (": " & Task_States'Image (T.Common.State));
      Parent := T.Common.Parent;

      if Parent = null then
         Put (", parent: <none>");
      else
         Put (", parent: ");
         Put_Task_Image (Parent);
      end if;

      Put (", prio:" & T.Common.Current_Priority'Img);

      if not T.Callable then
         Put (", not callable");
      end if;

      if T.Aborting then
         Put (", aborting");
      end if;

      if T.Deferral_Level /= 0 then
         Put (", abort deferred");
      end if;

      if T.Common.Call /= null then
         Entry_Call := T.Common.Call;
         Put (", serving:");

         while Entry_Call /= null loop
            Put_Task_Id_Image (Entry_Call.Self);
            Entry_Call := Entry_Call.Acceptor_Prev_Call;
         end loop;
      end if;

      if T.Open_Accepts /= null then
         Put (", accepting:");

         for J in T.Open_Accepts'Range loop
            Put (T.Open_Accepts (J).S'Img);
         end loop;

         if T.Terminate_Alternative then
            Put (" or terminate");
         end if;
      end if;

      if T.User_State /= 0 then
         Put (", state:" & T.User_State'Img);
      end if;

      Put_Line;
   end Print_Task_Info;

   ---------
   -- Put --
   ---------

   procedure Put (S : String) is
   begin
      Write (Stderr_Fd, S, S'Length);
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (S : String := "") is
   begin
      Write (Stderr_Fd, S & ASCII.LF, S'Length + 1);
   end Put_Line;

   -----------------------
   -- Put_Task_Id_Image --
   -----------------------

   procedure Put_Task_Id_Image (T : Task_Id) is
      Address_Image_Length : constant :=
        13 + (if Standard'Address_Size = 64 then 10 else 0);
      --  Length of string to be printed for address of task

      H : constant array (0 .. 15) of Character := "0123456789ABCDEF";
      --  Table of hex digits

      S : String (1 .. Address_Image_Length);
      P : Natural;
      N : Integer_Address;
      U : Natural := 0;

   begin
      if T = null then
         Put ("Null_Task_Id");

      else
         S (S'Last) := '#';
         P := Address_Image_Length - 1;
         N := To_Integer (T.all'Address);
         while P > 3 loop
            if U = 4 then
               S (P) := '_';
               P := P - 1;
               U := 1;
            else
               U := U + 1;
            end if;

            S (P) := H (Integer (N mod 16));
            P := P - 1;
            N := N / 16;
         end loop;

         S (1 .. 3) := "16#";
         Put (S);
      end if;
   end Put_Task_Id_Image;

   --------------------
   -- Put_Task_Image --
   --------------------

   procedure Put_Task_Image (T : Task_Id) is
   begin
      --  In case T.Common.Task_Image_Len is uninitialized junk, we check that
      --  it is in range, to make this more robust.

      if T.Common.Task_Image_Len in T.Common.Task_Image'Range then
         Put (T.Common.Task_Image (1 .. T.Common.Task_Image_Len));
      else
         Put (T.Common.Task_Image);
      end if;
   end Put_Task_Image;

   ----------------------
   -- Resume_All_Tasks --
   ----------------------

   procedure Resume_All_Tasks (Thread_Self : OS_Interface.Thread_Id) is
      C     : Task_Id;
      Dummy : Boolean;

   begin
      STPO.Lock_RTS;

      C := All_Tasks_List;
      while C /= null loop
         Dummy := STPO.Resume_Task (C, Thread_Self);
         C := C.Common.All_Tasks_Link;
      end loop;

      STPO.Unlock_RTS;
   end Resume_All_Tasks;

   ---------------
   -- Set_Trace --
   ---------------

   procedure Set_Trace (Flag  : Character; Value : Boolean := True) is
   begin
      Trace_On (Flag) := Value;
   end Set_Trace;

   --------------------
   -- Set_User_State --
   --------------------

   procedure Set_User_State (Value : Long_Integer) is
   begin
      STPO.Self.User_State := Value;
   end Set_User_State;

   ------------------------
   -- Signal_Debug_Event --
   ------------------------

   procedure Signal_Debug_Event
     (Event_Kind : Event_Kind_Type;
      Task_Value : Task_Id)
   is
   begin
      null;
   end Signal_Debug_Event;

   --------------------
   -- Stop_All_Tasks --
   --------------------

   procedure Stop_All_Tasks is
      C     : Task_Id;
      Dummy : Boolean;

   begin
      STPO.Lock_RTS;

      C := All_Tasks_List;
      while C /= null loop
         Dummy := STPO.Stop_Task (C);
         C := C.Common.All_Tasks_Link;
      end loop;

      STPO.Unlock_RTS;
   end Stop_All_Tasks;

   ----------------------------
   -- Stop_All_Tasks_Handler --
   ----------------------------

   procedure Stop_All_Tasks_Handler is
   begin
      STPO.Stop_All_Tasks;
   end Stop_All_Tasks_Handler;

   -----------------------
   -- Suspend_All_Tasks --
   -----------------------

   procedure Suspend_All_Tasks (Thread_Self : OS_Interface.Thread_Id) is
      C     : Task_Id;
      Dummy : Boolean;

   begin
      STPO.Lock_RTS;

      C := All_Tasks_List;
      while C /= null loop
         Dummy := STPO.Suspend_Task (C, Thread_Self);
         C := C.Common.All_Tasks_Link;
      end loop;

      STPO.Unlock_RTS;
   end Suspend_All_Tasks;

   ------------------------
   -- Task_Creation_Hook --
   ------------------------

   procedure Task_Creation_Hook (Thread : OS_Interface.Thread_Id) is
      pragma Inspection_Point (Thread);
      --  gdb needs to access the thread parameter in order to implement
      --  the multitask mode under VxWorks.

   begin
      null;
   end Task_Creation_Hook;

   ---------------------------
   -- Task_Termination_Hook --
   ---------------------------

   procedure Task_Termination_Hook is
   begin
      null;
   end Task_Termination_Hook;

   -----------
   -- Trace --
   -----------

   procedure Trace
     (Self_Id  : Task_Id;
      Msg      : String;
      Flag     : Character;
      Other_Id : Task_Id := null)
   is
   begin
      if Trace_On (Flag) then
         Put_Task_Id_Image (Self_Id);
         Put (":" & Flag & ":");
         Put_Task_Image (Self_Id);
         Put (":");

         if Other_Id /= null then
            Put_Task_Id_Image (Other_Id);
            Put (":");
         end if;

         Put_Line (Msg);
      end if;
   end Trace;

   -----------
   -- Write --
   -----------

   procedure Write (Fd : Integer; S : String; Count : Integer) is
      Discard : System.CRTL.ssize_t;
      --  Ignore write errors here; this is just debugging output, and there's
      --  nothing to be done about errors anyway.
   begin
      Discard :=
        System.CRTL.write
          (Fd, S'Address, System.CRTL.size_t (Count));
   end Write;

   -----------------
   -- Master_Hook --
   -----------------

   procedure Master_Hook
     (Dependent    : Task_Id;
      Parent       : Task_Id;
      Master_Level : Integer)
   is
      pragma Inspection_Point (Dependent);
      pragma Inspection_Point (Parent);
      pragma Inspection_Point (Master_Level);
   begin
      null;
   end Master_Hook;

   ---------------------------
   -- Master_Completed_Hook --
   ---------------------------

   procedure Master_Completed_Hook
     (Self_ID      : Task_Id;
      Master_Level : Integer)
   is
      pragma Inspection_Point (Self_ID);
      pragma Inspection_Point (Master_Level);
   begin
      null;
   end Master_Completed_Hook;

end System.Tasking.Debug;
