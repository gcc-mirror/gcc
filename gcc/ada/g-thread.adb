------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                         G N A T . T H R E A D S                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 1998-2003 Ada Core Technologies, Inc.           --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Task_Identification; use Ada.Task_Identification;
with System.Task_Primitives.Operations;
with System.Tasking;
with System.Tasking.Stages;   use System.Tasking.Stages;
with System.OS_Interface;     use System.OS_Interface;
with System.Soft_Links;       use System.Soft_Links;
with Unchecked_Conversion;

package body GNAT.Threads is

   use System;

   package STPO renames System.Task_Primitives.Operations;

   type Thread_Id_Ptr is access all Thread_Id;

   function To_Addr is new Unchecked_Conversion (Task_Id, Address);
   function To_Id   is new Unchecked_Conversion (Address, Task_Id);
   function To_Id   is new Unchecked_Conversion (Address, Tasking.Task_ID);
   function To_Tid  is new Unchecked_Conversion
     (Address, Ada.Task_Identification.Task_Id);
   function To_Thread is new Unchecked_Conversion (Address, Thread_Id_Ptr);

   type Code_Proc is access procedure (Id : Address; Parm : Void_Ptr);

   task type Thread
     (Stsz : Natural;
      Prio : Any_Priority;
      Parm : Void_Ptr;
      Code : Code_Proc)
   is
      pragma Priority (Prio);
      pragma Storage_Size (Stsz);
   end Thread;

   task body Thread is
   begin
      Code.all (To_Addr (Current_Task), Parm);
   end Thread;

   type Tptr is access Thread;

   -------------------
   -- Create_Thread --
   -------------------

   function Create_Thread
     (Code : Address;
      Parm : Void_Ptr;
      Size : Natural;
      Prio : Integer) return System.Address
   is
      TP : Tptr;

      function To_CP is new Unchecked_Conversion (Address, Code_Proc);

   begin
      TP := new Thread (Size, Prio, Parm, To_CP (Code));
      return To_Addr (TP'Identity);
   end Create_Thread;

   ---------------------
   -- Register_Thread --
   ---------------------

   function Register_Thread return System.Address is
   begin
      return Task_Primitives.Operations.Register_Foreign_Thread.all'Address;
   end Register_Thread;

   -----------------------
   -- Unregister_Thread --
   -----------------------

   procedure Unregister_Thread is
      Self_Id : constant Tasking.Task_ID := Task_Primitives.Operations.Self;
   begin
      Self_Id.Common.State := Tasking.Terminated;
      Destroy_TSD (Self_Id.Common.Compiler_Data);
      Free_Task (Self_Id);
   end Unregister_Thread;

   --------------------------
   -- Unregister_Thread_Id --
   --------------------------

   procedure Unregister_Thread_Id (Thread : System.Address) is
      Thr : constant Thread_Id := To_Thread (Thread).all;
      T   : Tasking.Task_ID;

      use type Tasking.Task_ID;

   begin
      STPO.Lock_RTS;

      T := Tasking.All_Tasks_List;
      loop
         exit when T = null or else STPO.Get_Thread_Id (T) = Thr;

         T := T.Common.All_Tasks_Link;
      end loop;

      STPO.Unlock_RTS;

      if T /= null then
         T.Common.State := Tasking.Terminated;
         Destroy_TSD (T.Common.Compiler_Data);
         Free_Task (T);
      end if;
   end Unregister_Thread_Id;

   --------------------
   -- Destroy_Thread --
   --------------------

   procedure Destroy_Thread (Id : Address) is
      Tid : constant Task_Id := To_Id (Id);
   begin
      Abort_Task (Tid);
   end Destroy_Thread;

   ----------------
   -- Get_Thread --
   ----------------

   procedure Get_Thread (Id : Address; Thread : Address) is
      use System.OS_Interface;
      Thr : constant Thread_Id_Ptr := To_Thread (Thread);
   begin
      Thr.all := Task_Primitives.Operations.Get_Thread_Id (To_Id (Id));
   end Get_Thread;

   ----------------
   -- To_Task_Id --
   ----------------

   function To_Task_Id
     (Id   : System.Address) return Ada.Task_Identification.Task_Id
   is
   begin
      return To_Tid (Id);
   end To_Task_Id;

end GNAT.Threads;
