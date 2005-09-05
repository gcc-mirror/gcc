------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--             S Y S T E M . S O F T _ L I N K S . T A S K I N G            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2005, Free Software Foundation, Inc.         --
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

pragma Style_Checks (All_Checks);
--  Turn off subprogram alpha ordering check, since we group soft link
--  bodies and dummy soft link bodies together separately in this unit.

pragma Polling (Off);
--  Turn polling off for this package. We don't need polling during any
--  of the routines in this package, and more to the point, if we try
--  to poll it can cause infinite loops.

with System.Task_Primitives.Operations;
--  Used for Self
--           Timed_Delay

with System.Tasking;
--  Used for Task_Id

package body System.Soft_Links.Tasking is

   package STPO renames System.Task_Primitives.Operations;
   package SSL  renames System.Soft_Links;

   ----------------
   -- Local Data --
   ----------------

   Initialized : Boolean := False;
   --  Boolean flag that indicates whether the tasking soft links have
   --  already been set.

   -----------------------------------------------------------------
   -- Tasking Versions of Services Needed by Non-Tasking Programs --
   -----------------------------------------------------------------

   function  Get_Jmpbuf_Address return  Address;
   procedure Set_Jmpbuf_Address (Addr : Address);
   --  Get/Set Jmpbuf_Address for current task

   function  Get_Sec_Stack_Addr return  Address;
   procedure Set_Sec_Stack_Addr (Addr : Address);
   --  Get/Set location of current task's secondary stack

   function Get_Current_Excep return SSL.EOA;
   --  Task-safe version of SSL.Get_Current_Excep

   procedure Timed_Delay_T (Time : Duration; Mode : Integer);
   --  Task-safe version of SSL.Timed_Delay

   --------------------------
   -- Soft-Link Get Bodies --
   --------------------------

   function Get_Current_Excep return SSL.EOA is
   begin
      return STPO.Self.Common.Compiler_Data.Current_Excep'Access;
   end Get_Current_Excep;

   function Get_Jmpbuf_Address return  Address is
   begin
      return STPO.Self.Common.Compiler_Data.Jmpbuf_Address;
   end Get_Jmpbuf_Address;

   function Get_Sec_Stack_Addr return  Address is
   begin
      return STPO.Self.Common.Compiler_Data.Sec_Stack_Addr;
   end Get_Sec_Stack_Addr;

   --------------------------
   -- Soft-Link Set Bodies --
   --------------------------

   procedure Set_Jmpbuf_Address (Addr : Address) is
   begin
      STPO.Self.Common.Compiler_Data.Jmpbuf_Address := Addr;
   end Set_Jmpbuf_Address;

   procedure Set_Sec_Stack_Addr (Addr : Address) is
   begin
      STPO.Self.Common.Compiler_Data.Sec_Stack_Addr := Addr;
   end Set_Sec_Stack_Addr;

   -------------------
   -- Timed_Delay_T --
   -------------------

   procedure Timed_Delay_T (Time : Duration; Mode : Integer) is
      Self_Id : constant System.Tasking.Task_Id := STPO.Self;

   begin
      --  In case pragma Detect_Blocking is active then Program_Error
      --  must be raised if this potentially blocking operation
      --  is called from a protected operation.

      if System.Tasking.Detect_Blocking
        and then Self_Id.Common.Protected_Action_Nesting > 0
      then
         raise Program_Error with "potentially blocking operation";
      else
         Abort_Defer.all;
         STPO.Timed_Delay (Self_Id, Time, Mode);
         Abort_Undefer.all;
      end if;
   end Timed_Delay_T;

   -----------------------------
   -- Init_Tasking_Soft_Links --
   -----------------------------

   procedure Init_Tasking_Soft_Links is
   begin
      --  Set links only if not set already

      if not Initialized then

         --  Mark tasking soft links as initialized

         Initialized := True;

         --  The application being executed uses tasking so that the tasking
         --  version of the following soft links need to be used.

         SSL.Get_Jmpbuf_Address     := Get_Jmpbuf_Address'Access;
         SSL.Set_Jmpbuf_Address     := Set_Jmpbuf_Address'Access;
         SSL.Get_Sec_Stack_Addr     := Get_Sec_Stack_Addr'Access;
         SSL.Set_Sec_Stack_Addr     := Set_Sec_Stack_Addr'Access;
         SSL.Get_Current_Excep      := Get_Current_Excep'Access;
         SSL.Timed_Delay            := Timed_Delay_T'Access;

         --  No need to create a new Secondary Stack, since we will use the
         --  default one created in s-secsta.adb

         SSL.Set_Sec_Stack_Addr     (SSL.Get_Sec_Stack_Addr_NT);
         SSL.Set_Jmpbuf_Address     (SSL.Get_Jmpbuf_Address_NT);
      end if;
   end Init_Tasking_Soft_Links;

end System.Soft_Links.Tasking;
