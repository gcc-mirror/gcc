------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                  S Y S T E M . T A S K I N G . D E B U G                 --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1997-2005 Free Software Foundation, Inc.          --
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
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This package encapsulates all direct interfaces to task debugging services
--  that are needed by gdb with gnat mode.

--  Note : This file *must* be compiled with debugging information

--  Do not add any dependency to GNARL packages since this package is used
--  in both normal and restricted (ravenscar) environments.

with System.CRTL;
with System.Task_Primitives.Operations;
with Unchecked_Conversion;

package body System.Tasking.Debug is

   package STPO renames System.Task_Primitives.Operations;

   function To_Integer is new
     Unchecked_Conversion (Task_Id, System.Address);

   type Trace_Flag_Set is array (Character) of Boolean;

   Trace_On : Trace_Flag_Set := ('A' .. 'Z' => False, others => True);

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Write (Fd : Integer; S : String; Count : Integer);

   procedure Put (S : String);
   --  Display S on standard output.

   procedure Put_Line (S : String := "");
   --  Display S on standard output with an additional line terminator.

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

      Put (T.Common.Task_Image (1 .. T.Common.Task_Image_Len) & ": " &
           Task_States'Image (T.Common.State));

      Parent := T.Common.Parent;

      if Parent = null then
         Put (", parent: <none>");
      else
         Put (", parent: " &
              Parent.Common.Task_Image (1 .. Parent.Common.Task_Image_Len));
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
            Put (To_Integer (Entry_Call.Self)'Img);
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
      Write (2, S, S'Length);
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (S : String := "") is
   begin
      Write (2, S & ASCII.LF, S'Length + 1);
   end Put_Line;

   ----------------------
   -- Resume_All_Tasks --
   ----------------------

   procedure Resume_All_Tasks (Thread_Self : OS_Interface.Thread_Id) is
      C     : Task_Id;
      Dummy : Boolean;
      pragma Unreferenced (Dummy);

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

   -----------------------
   -- Suspend_All_Tasks --
   -----------------------

   procedure Suspend_All_Tasks (Thread_Self : OS_Interface.Thread_Id) is
      C     : Task_Id;
      Dummy : Boolean;
      pragma Unreferenced (Dummy);

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
         Put (To_Integer (Self_Id)'Img &
              ':' & Flag & ':' &
              Self_Id.Common.Task_Image (1 .. Self_Id.Common.Task_Image_Len) &
              ':');

         if Other_Id /= null then
            Put (To_Integer (Other_Id)'Img & ':');
         end if;

         Put_Line (Msg);
      end if;
   end Trace;

   -----------
   -- Write --
   -----------

   procedure Write (Fd : Integer; S : String; Count : Integer) is
      Discard : Integer;
      pragma Unreferenced (Discard);
   begin
      Discard := System.CRTL.write (Fd, S (S'First)'Address, Count);
      --  Is it really right to ignore write errors here ???
   end Write;

end System.Tasking.Debug;
