------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS              --
--                                                                          --
--                  S Y S T E M . T A S K I N G . D E B U G                 --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--          Copyright (C) 1997-2002 Free Software Foundation, Inc.          --
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

--  This package encapsulates all direct interfaces to task debugging services
--  that are needed by gdb with gnat mode (1.13 and higher)

--  Note : This file *must* be compiled with debugging information

--  Do not add any dependency to GNARL packages since this package is used
--  in both normal and restricted (ravenscar) environments.

with System.Task_Info,
     System.Task_Primitives.Operations,
     Unchecked_Conversion;

package body System.Tasking.Debug is

   use Interfaces.C;

   package STPO renames System.Task_Primitives.Operations;

   type Integer_Address is mod 2 ** Standard'Address_Size;

   function "+" is new
     Unchecked_Conversion (Task_ID, Integer_Address);

   Hex_Address_Width : constant := (Standard'Address_Size / 4);

   Hex_Digits : constant array (0 .. Integer_Address'(15)) of Character :=
                  "0123456789abcdef";

   subtype Buf_Range is Integer range 1 .. 80;
   type Buf_Array is array (Buf_Range) of aliased Character;

   type Buffer is record
      Next  : Buf_Range := Buf_Range'First;
      Chars : Buf_Array := (Buf_Range => ' ');
   end record;

   type Buffer_Ptr is access all Buffer;

   type Trace_Flag_Set is array (Character) of Boolean;

   Trace_On : Trace_Flag_Set := ('A' .. 'Z' => False, others => True);

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Put
     (T      : ST.Task_ID;
      Width  : Integer;
      Buffer : Buffer_Ptr);
   --  Put TCB pointer T, (coded in hexadecimal) into Buffer
   --  right-justified in Width characters.

   procedure Put
     (N      : Integer_Address;
      Width  : Integer;
      Buffer : Buffer_Ptr);
   --  Put N (coded in decimal) into Buf right-justified in Width
   --  characters starting at Buf (Next).

   procedure Put
     (S      : String;
      Width  : Integer;
      Buffer : Buffer_Ptr);
   --  Put string S into Buf left-justified in Width characters
   --  starting with space in Buf (Next), truncated as necessary.

   procedure Put
     (C      : Character;
      Buffer : Buffer_Ptr);
   --  Put character C into Buf, left-justified, starting at Buf (Next)

   procedure Space (Buffer : Buffer_Ptr);
   --  Increment Next, resulting in a space

   procedure Space
     (N      : Integer;
      Buffer : Buffer_Ptr);
   --  Increment Next by N, resulting in N spaces

   procedure Clear (Buffer : Buffer_Ptr);
   --  Clear Buf and reset Next to 1

   procedure Write_Buf (Buffer : Buffer_Ptr);
   --  Write contents of Buf (1 .. Next) to standard output

   -----------
   -- Clear --
   -----------

   procedure Clear (Buffer : Buffer_Ptr) is
      Next : Buf_Range renames Buffer.Next;
      Buf  : Buf_Array renames Buffer.Chars;

   begin
      Buf := (Buf_Range => ' ');
      Next := 1;
   end Clear;

   -----------
   -- Image --
   -----------

   function Image (T : ST.Task_ID) return String is
      Buf    : aliased Buffer;
      Result : String (1 .. Hex_Address_Width + 21);

      use type System.Task_Info.Task_Image_Type;

   begin
      Clear (Buf'Unchecked_Access);
      Put (T, Hex_Address_Width, Buf'Unchecked_Access);
      Put (':', Buf'Unchecked_Access);
      Put (Integer_Address (T.Serial_Number), 4, Buf'Unchecked_Access);
      Space (Buf'Unchecked_Access);

      if T.Common.Task_Image = null then
         Put ("", 15, Buf'Unchecked_Access);
      else
         Put (T.Common.Task_Image.all, 15, Buf'Unchecked_Access);
      end if;

      for J in Result'Range loop
         Result (J) := Buf.Chars (J);
      end loop;

      return Result;
   end Image;

   ----------------
   -- List_Tasks --
   ----------------

   procedure List_Tasks is
      C : ST.Task_ID;

   begin
      Print_Task_Info_Header;
      C := All_Tasks_List;

      while C /= null loop
         Print_Task_Info (C);
         C := C.Common.All_Tasks_Link;
      end loop;
   end List_Tasks;

   -----------------------
   -- Print_Accept_Info --
   -----------------------

   procedure Print_Accept_Info (T : ST.Task_ID) is
      Buf : aliased Buffer;

   begin
      if T.Open_Accepts = null then
         return;
      end if;

      Clear (Buf'Unchecked_Access);
      Space (10, Buf'Unchecked_Access);
      Put ("accepting:", 11, Buf'Unchecked_Access);

      for J in T.Open_Accepts.all'Range loop
         Put (Integer_Address (T.Open_Accepts (J).S), 3, Buf'Unchecked_Access);
      end loop;

      Write_Buf (Buf'Unchecked_Access);
   end Print_Accept_Info;

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

   procedure Print_Task_Info (T : ST.Task_ID) is
      Entry_Call : Entry_Call_Link;
      Buf        : aliased Buffer;

      use type System.Task_Info.Task_Image_Type;

   begin
      Clear (Buf'Unchecked_Access);
      Put (T, Hex_Address_Width, Buf'Unchecked_Access);
      Put (':', Buf'Unchecked_Access);
      Put (' ', Buf'Unchecked_Access);
      Put (':', Buf'Unchecked_Access);

      if T = null then
         Put (" null task", 10, Buf'Unchecked_Access);
         Write_Buf (Buf'Unchecked_Access);
         return;
      end if;

      Put (Integer_Address (T.Serial_Number), 4, Buf'Unchecked_Access);
      Space (Buf'Unchecked_Access);

      if T.Common.Task_Image = null then
         Put ("", 15, Buf'Unchecked_Access);
      else
         Put (T.Common.Task_Image.all, 15, Buf'Unchecked_Access);
      end if;

      Space (Buf'Unchecked_Access);
      Put (Task_States'Image (T.Common.State), 10, Buf'Unchecked_Access);
      Space (Buf'Unchecked_Access);

      if T.Callable then
         Put ('C', Buf'Unchecked_Access);
      else
         Space (Buf'Unchecked_Access);
      end if;

      if T.Open_Accepts /= null then
         Put ('A', Buf'Unchecked_Access);
      else
         Space (Buf'Unchecked_Access);
      end if;

      if T.Common.Call /= null then
         Put ('C', Buf'Unchecked_Access);
      else
         Space (Buf'Unchecked_Access);
      end if;

      if T.Terminate_Alternative then
         Put ('T', Buf'Unchecked_Access);
      else
         Space (Buf'Unchecked_Access);
      end if;

      if T.Aborting then
         Put ('A', Buf'Unchecked_Access);
      else
         Space (Buf'Unchecked_Access);
      end if;

      if T.Deferral_Level = 0 then
         Space (3, Buf'Unchecked_Access);
      else
         Put ('D', Buf'Unchecked_Access);
         if T.Deferral_Level < 0 then
            Put ("<0", 2, Buf'Unchecked_Access);
         elsif T.Deferral_Level > 1 then
            Put (Integer_Address (T.Deferral_Level), 2, Buf'Unchecked_Access);
         else
            Space (2, Buf'Unchecked_Access);
         end if;
      end if;

      Space (Buf'Unchecked_Access);
      Put (Integer_Address (T.Master_of_Task), 1, Buf'Unchecked_Access);
      Space (Buf'Unchecked_Access);
      Put (Integer_Address (T.Master_Within), 1, Buf'Unchecked_Access);
      Put (',', Buf'Unchecked_Access);
      Space (Buf'Unchecked_Access);
      Put (Integer_Address (T.Awake_Count), 1, Buf'Unchecked_Access);
      Space (Buf'Unchecked_Access);
      Put (Integer_Address (T.Alive_Count), 1, Buf'Unchecked_Access);
      Put (',', Buf'Unchecked_Access);
      Space (Buf'Unchecked_Access);
      Put (Integer_Address (T.ATC_Nesting_Level), 1, Buf'Unchecked_Access);
      Space (Buf'Unchecked_Access);
      Put (Integer_Address (T.Pending_ATC_Level), 1, Buf'Unchecked_Access);
      Put (',', Buf'Unchecked_Access);
      Space (Buf'Unchecked_Access);
      Put (Integer_Address (T.Common.Wait_Count), 1, Buf'Unchecked_Access);
      Put (',', Buf'Unchecked_Access);
      Space (Buf'Unchecked_Access);
      Put (Integer_Address (T.User_State), 1, Buf'Unchecked_Access);
      Write_Buf (Buf'Unchecked_Access);

      if T.Common.Call /= null then
         Entry_Call := T.Common.Call;
         Clear (Buf'Unchecked_Access);
         Space (10, Buf'Unchecked_Access);
         Put ("serving:", 8, Buf'Unchecked_Access);

         while Entry_Call /= null loop
            Put (Integer_Address
              (Entry_Call.Self.Serial_Number), 5, Buf'Unchecked_Access);
            Entry_Call := Entry_Call.Acceptor_Prev_Call;
         end loop;

         Write_Buf (Buf'Unchecked_Access);
      end if;

      Print_Accept_Info (T);
   end Print_Task_Info;

   ----------------------------
   -- Print_Task_Info_Header --
   ----------------------------

   procedure Print_Task_Info_Header is
      Buf : aliased Buffer;

   begin
      Clear (Buf'Unchecked_Access);
      Put ("TASK_ID", Hex_Address_Width, Buf'Unchecked_Access);
      Put (':', Buf'Unchecked_Access);
      Put ('F', Buf'Unchecked_Access);
      Put (':', Buf'Unchecked_Access);
      Put ("SERIAL_NUMBER", 4, Buf'Unchecked_Access);
      Space (Buf'Unchecked_Access);
      Put (" NAME", 15, Buf'Unchecked_Access);
      Put (" STATE", 10, Buf'Unchecked_Access);
      Space (11, Buf'Unchecked_Access);
      Put ("MAST", 5, Buf'Unchecked_Access);
      Put ("AWAK", 5, Buf'Unchecked_Access);
      Put ("ATC", 5, Buf'Unchecked_Access);
      Put ("WT", 3, Buf'Unchecked_Access);
      Put ("DBG", 3, Buf'Unchecked_Access);
      Write_Buf (Buf'Unchecked_Access);
   end Print_Task_Info_Header;

   ---------
   -- Put --
   ---------

   procedure Put
     (T      : ST.Task_ID;
      Width  : Integer;
      Buffer : Buffer_Ptr)
   is
      J     : Integer;
      X     : Integer_Address := +T;
      Next  : Buf_Range renames Buffer.Next;
      Buf   : Buf_Array renames Buffer.Chars;
      First : constant Integer := Next;
      Wdth  : Integer := Width;

   begin
      if Wdth > Buf'Last - Next then
         Wdth := Buf'Last - Next;
      end if;

      J := Next + (Wdth - 1);

      if X = 0 then
         Buf (J) := '0';

      else
         while X > 0 loop
            Buf (J) := Hex_Digits (X rem 16);
            J := J - 1;
            X := X / 16;

            --  Check for overflow

            if J < First and then X > 0 then
               Buf (J + 1) := '*';
               exit;
            end if;

         end loop;
      end if;

      Next := Next + Wdth;
   end Put;

   procedure Put
     (N      : Integer_Address;
      Width  : Integer;
      Buffer : Buffer_Ptr)
   is
      J     : Integer;
      X     : Integer_Address := N;
      Next  : Buf_Range renames Buffer.Next;
      Buf   : Buf_Array renames Buffer.Chars;
      First : constant Integer := Next;
      Wdth  : Integer := Width;

   begin
      if Wdth > Buf'Last - Next then
         Wdth := Buf'Last - Next;
      end if;

      J := Next + (Wdth - 1);

      if N = 0 then
         Buf (J) := '0';

      else
         while X > 0 loop
            Buf (J) := Hex_Digits (X rem 10);
            J := J - 1;
            X := X / 10;

            --  Check for overflow

            if J < First and then X > 0 then
               Buf (J + 1) := '*';
               exit;
            end if;
         end loop;
      end if;

      Next := Next + Wdth;
   end Put;

   procedure Put
     (S      : String;
      Width  : Integer;
      Buffer : Buffer_Ptr)
   is
      Next  : Buf_Range renames Buffer.Next;
      Buf   : Buf_Array renames Buffer.Chars;
      Bound : constant Integer := Integer'Min (Next + Width, Buf'Last);
      J     : Integer := Next;

   begin
      for K in S'Range loop

         --  Check overflow

         if J >= Bound then
            Buf (J - 1) := '*';
            exit;
         end if;

         Buf (J) := S (K);
         J := J + 1;
      end loop;

      Next := Bound;
   end Put;

   procedure Put
     (C      : Character;
      Buffer : Buffer_Ptr)
   is
      Next : Buf_Range renames Buffer.Next;
      Buf  : Buf_Array renames Buffer.Chars;

   begin
      if Next >= Buf'Last then
         Buf (Next) := '*';
      else Buf (Next) := C;
         Next := Next + 1;
      end if;
   end Put;

   ----------------------
   -- Resume_All_Tasks --
   ----------------------

   procedure Resume_All_Tasks (Thread_Self : OS_Interface.Thread_Id) is
      C : ST.Task_ID;
      R : Boolean;

   begin
      STPO.Lock_RTS;
      C := All_Tasks_List;

      while C /= null loop
         R := STPO.Resume_Task (C, Thread_Self);
         C := C.Common.All_Tasks_Link;
      end loop;

      STPO.Unlock_RTS;
   end Resume_All_Tasks;

   ----------
   -- Self --
   ----------

   function Self return Task_ID is
   begin
      return STPO.Self;
   end Self;

   ---------------
   -- Set_Trace --
   ---------------

   procedure Set_Trace
     (Flag  : Character;
      Value : Boolean := True)
   is
   begin
      Trace_On (Flag) := Value;
   end Set_Trace;

   --------------------
   -- Set_User_State --
   --------------------

   procedure Set_User_State (Value : Integer) is
   begin
      STPO.Self.User_State := Value;
   end Set_User_State;

   -----------
   -- Space --
   -----------

   procedure Space (Buffer : Buffer_Ptr) is
      Next : Buf_Range renames Buffer.Next;
      Buf  : Buf_Array renames Buffer.Chars;

   begin
      if Next >= Buf'Last then
         Buf (Next) := '*';
      else
         Next := Next + 1;
      end if;
   end Space;

   procedure Space
     (N      : Integer;
      Buffer : Buffer_Ptr)
   is
      Next : Buf_Range renames Buffer.Next;
      Buf  : Buf_Array renames Buffer.Chars;

   begin
      if Next + N > Buf'Last then
         Buf (Next) := '*';
      else
         Next := Next + N;
      end if;
   end Space;

   -----------------------
   -- Suspend_All_Tasks --
   -----------------------

   procedure Suspend_All_Tasks (Thread_Self : OS_Interface.Thread_Id) is
      C : ST.Task_ID;
      R : Boolean;

   begin
      STPO.Lock_RTS;
      C := All_Tasks_List;

      while C /= null loop
         R := STPO.Suspend_Task (C, Thread_Self);
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
     (Self_ID  : ST.Task_ID;
      Msg      : String;
      Other_ID : ST.Task_ID;
      Flag     : Character)
   is
      Buf : aliased Buffer;
      use type System.Task_Info.Task_Image_Type;

   begin
      if Trace_On (Flag) then
         Clear (Buf'Unchecked_Access);
         Put (Self_ID, Hex_Address_Width, Buf'Unchecked_Access);
         Put (':', Buf'Unchecked_Access);
         Put (Flag, Buf'Unchecked_Access);
         Put (':', Buf'Unchecked_Access);
         Put
           (Integer_Address (Self_ID.Serial_Number),
            4, Buf'Unchecked_Access);
         Space (Buf'Unchecked_Access);

         if Self_ID.Common.Task_Image = null then
            Put ("", 15, Buf'Unchecked_Access);
         else
            Put (Self_ID.Common.Task_Image.all, 15, Buf'Unchecked_Access);
         end if;

         Space (Buf'Unchecked_Access);

         if Other_ID /= null then
            Put
              (Integer_Address (Other_ID.Serial_Number),
               4, Buf'Unchecked_Access);
            Space (Buf'Unchecked_Access);
         end if;

         Put (Msg, Buf.Chars'Last - Buf.Next + 1, Buf'Unchecked_Access);
         Write_Buf (Buf'Unchecked_Access);
      end if;
   end Trace;

   procedure Trace
     (Self_ID : ST.Task_ID;
      Msg     : String;
      Flag    : Character)
   is
   begin
      Trace (Self_ID, Msg, null, Flag);
   end Trace;

   procedure Trace
     (Msg : String;
      Flag : Character)
   is
      Self_ID : constant ST.Task_ID := STPO.Self;

   begin
      Trace (Self_ID, Msg, null, Flag);
   end Trace;

   procedure Trace
     (Msg      : String;
      Other_ID : ST.Task_ID;
      Flag     : Character)
   is
      pragma Warnings (Off, Other_ID);

      Self_ID : constant ST.Task_ID := STPO.Self;

   begin
      Trace (Self_ID, Msg, null, Flag);
   end Trace;

   ---------------
   -- Write_Buf --
   ---------------

   procedure Write_Buf (Buffer : Buffer_Ptr) is
      Next : Buf_Range renames Buffer.Next;
      Buf  : Buf_Array renames Buffer.Chars;

      procedure put_char (C : Integer);
      pragma Import (C, put_char, "put_char");

   begin
      for J in 1 .. Next - 1 loop
         put_char (Character'Pos (Buf (J)));
      end loop;

      put_char (Character'Pos (ASCII.LF));
   end Write_Buf;

end System.Tasking.Debug;
