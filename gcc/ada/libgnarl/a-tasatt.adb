------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--                  A D A . T A S K _ A T T R I B U T E S                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2014-2024, Free Software Foundation, Inc.       --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

with System.Storage_Elements;
with System.Tasking;
with System.Tasking.Initialization;
with System.Tasking.Task_Attributes;
pragma Elaborate_All (System.Tasking.Task_Attributes);

with System.Task_Primitives.Operations;

with Ada.Finalization; use Ada.Finalization;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package body Ada.Task_Attributes is

   use System,
       System.Storage_Elements,
       System.Tasking.Initialization,
       System.Tasking,
       System.Tasking.Task_Attributes;

   package STPO renames System.Task_Primitives.Operations;

   type Attribute_Cleanup is new Limited_Controlled with null record;
   procedure Finalize (Cleanup : in out Attribute_Cleanup);
   --  Finalize all tasks' attributes for this package

   Cleanup : Attribute_Cleanup;
   pragma Unreferenced (Cleanup);
   --  Will call Finalize when this instantiation gets out of scope

   ---------------------------
   -- Unchecked Conversions --
   ---------------------------

   type Real_Attribute is record
      Free  : Deallocator;
      Value : Attribute;
   end record;
   type Real_Attribute_Access is access all Real_Attribute;
   pragma No_Strict_Aliasing (Real_Attribute_Access);
   --  Each value in the task control block's Attributes array is either
   --  mapped to the attribute value directly if Fast_Path is True, or
   --  is in effect a Real_Attribute_Access.
   --
   --  Note: the Deallocator field must be first, for compatibility with
   --  System.Tasking.Task_Attributes.Attribute_Record and to allow unchecked
   --  conversions between Attribute_Access and Real_Attribute_Access.

   function New_Attribute (Val : Attribute) return System.Address;
   --  Create a new Real_Attribute using Val, and return its address. The
   --  returned value can be converted via To_Real_Attribute.

   procedure Deallocate (Ptr : System.Address);
   --  Free memory associated with Ptr, a Real_Attribute_Access in reality

   function To_Real_Attribute is new
     Ada.Unchecked_Conversion (System.Address, Real_Attribute_Access);

   pragma Warnings (Off);
   --  Kill warning about possible size mismatch

   function To_Address is new
     Ada.Unchecked_Conversion (Attribute, System.Address);
   function To_Attribute is new
     Ada.Unchecked_Conversion (System.Address, Attribute);

   type Unsigned is mod 2 ** Integer'Size;
   function To_Unsigned is new
     Ada.Unchecked_Conversion (Attribute, Unsigned);

   pragma Warnings (On);

   function To_Address is new
     Ada.Unchecked_Conversion (Real_Attribute_Access, System.Address);

   pragma Warnings (Off);
   --  Kill warning about possible aliasing

   function To_Handle is new
     Ada.Unchecked_Conversion (System.Address, Attribute_Handle);

   pragma Warnings (On);

   function To_Task_Id is new
     Ada.Unchecked_Conversion (Task_Identification.Task_Id, Task_Id);
   --  To access TCB of identified task

   procedure Free is new
     Ada.Unchecked_Deallocation (Real_Attribute, Real_Attribute_Access);

   Fast_Path : constant Boolean :=
                 (Attribute'Size = Integer'Size
                   and then Attribute'Alignment <= System.Address'Alignment
                   and then To_Unsigned (Initial_Value) = 0)
                 or else (Attribute'Size = System.Address'Size
                   and then Attribute'Alignment <= System.Address'Alignment
                   and then To_Address (Initial_Value) = Null_Address);
   --  If the attribute fits in a System.Address (both size and alignment)
   --  and Initial_Value is 0 (or null), then we will map the attribute
   --  directly into ATCB.Attributes (Index), otherwise we will create
   --  a level of indirection and instead use Attributes (Index) as a
   --  Real_Attribute_Access.

   Index : constant Integer :=
             Next_Index (Require_Finalization => not Fast_Path);
   --  Index in the task control block's Attributes array

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Cleanup : in out Attribute_Cleanup) is
      pragma Unreferenced (Cleanup);

   begin
      STPO.Lock_RTS;

      declare
         C : System.Tasking.Task_Id := System.Tasking.All_Tasks_List;

      begin
         while C /= null loop
            STPO.Write_Lock (C);

            if C.Attributes (Index) /= Null_Address
              and then Require_Finalization (Index)
            then
               Deallocate (C.Attributes (Index));
               C.Attributes (Index) := Null_Address;
            end if;

            STPO.Unlock (C);
            C := C.Common.All_Tasks_Link;
         end loop;
      end;

      Finalize (Index);
      STPO.Unlock_RTS;
   end Finalize;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate (Ptr : System.Address) is
      Obj : Real_Attribute_Access := To_Real_Attribute (Ptr);
   begin
      Free (Obj);
   end Deallocate;

   -------------------
   -- New_Attribute --
   -------------------

   function New_Attribute (Val : Attribute) return System.Address is
      Tmp : Real_Attribute_Access;
   begin
      Tmp := new Real_Attribute'(Free  => Deallocate'Unrestricted_Access,
                                 Value => Val);
      return To_Address (Tmp);
   end New_Attribute;

   ---------------
   -- Reference --
   ---------------

   function Reference
     (T : Task_Identification.Task_Id := Task_Identification.Current_Task)
      return Attribute_Handle
   is
      Self_Id       : Task_Id;
      TT            : constant Task_Id := To_Task_Id (T);
      Error_Message : constant String  := "trying to get the reference of a ";
      Result        : Attribute_Handle;

   begin
      if TT = null then
         raise Program_Error with Error_Message & "null task";
      end if;

      if TT.Common.State = Terminated then
         raise Tasking_Error with Error_Message & "terminated task";
      end if;

      if Fast_Path then
         --  Kill warning about possible alignment mismatch. If this happens,
         --  Fast_Path will be False anyway
         pragma Warnings (Off);
         return To_Handle (TT.Attributes (Index)'Address);
         pragma Warnings (On);
      else
         Self_Id := STPO.Self;
         Task_Lock (Self_Id);

         if TT.Attributes (Index) = Null_Address then
            TT.Attributes (Index) := New_Attribute (Initial_Value);
         end if;

         Result := To_Handle
           (To_Real_Attribute (TT.Attributes (Index)).Value'Address);
         Task_Unlock (Self_Id);

         return Result;
      end if;
   end Reference;

   ------------------
   -- Reinitialize --
   ------------------

   procedure Reinitialize
     (T : Task_Identification.Task_Id := Task_Identification.Current_Task)
   is
      Self_Id       : Task_Id;
      TT            : constant Task_Id := To_Task_Id (T);
      Error_Message : constant String  := "Trying to Reinitialize a ";

   begin
      if TT = null then
         raise Program_Error with Error_Message & "null task";
      end if;

      if TT.Common.State = Terminated then
         raise Tasking_Error with Error_Message & "terminated task";
      end if;

      if Fast_Path then

         --  No finalization needed, simply reset to Initial_Value

         TT.Attributes (Index) := To_Address (Initial_Value);

      else
         Self_Id := STPO.Self;
         Task_Lock (Self_Id);

         declare
            Attr : System.Address renames TT.Attributes (Index);
         begin
            if Attr /= Null_Address then
               Deallocate (Attr);
               Attr := Null_Address;
            end if;
         end;

         Task_Unlock (Self_Id);
      end if;
   end Reinitialize;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Val : Attribute;
      T   : Task_Identification.Task_Id := Task_Identification.Current_Task)
   is
      Self_Id       : Task_Id;
      TT            : constant Task_Id := To_Task_Id (T);
      Error_Message : constant String  := "trying to set the value of a ";

   begin
      if TT = null then
         raise Program_Error with Error_Message & "null task";
      end if;

      if TT.Common.State = Terminated then
         raise Tasking_Error with Error_Message & "terminated task";
      end if;

      if Fast_Path then

         --  No finalization needed, simply set to Val

         if Attribute'Size = Integer'Size then
            TT.Attributes (Index) :=
              To_Address (Integer_Address (To_Unsigned (Val)));
         else
            TT.Attributes (Index) := To_Address (Val);
         end if;

      else
         Self_Id := STPO.Self;
         Task_Lock (Self_Id);

         declare
            Attr : System.Address renames TT.Attributes (Index);

         begin
            if Attr /= Null_Address then
               Deallocate (Attr);
            end if;

            Attr := New_Attribute (Val);
         end;

         Task_Unlock (Self_Id);
      end if;
   end Set_Value;

   -----------
   -- Value --
   -----------

   function Value
     (T : Task_Identification.Task_Id := Task_Identification.Current_Task)
      return Attribute
   is
      Self_Id       : Task_Id;
      TT            : constant Task_Id := To_Task_Id (T);
      Error_Message : constant String  := "trying to get the value of a ";

   begin
      if TT = null then
         raise Program_Error with Error_Message & "null task";
      end if;

      if TT.Common.State = Terminated then
         raise Tasking_Error with Error_Message & "terminated task";
      end if;

      if Fast_Path then
         return To_Attribute (TT.Attributes (Index));

      else
         Self_Id := STPO.Self;
         Task_Lock (Self_Id);

         declare
            Attr : System.Address renames TT.Attributes (Index);

         begin
            if Attr = Null_Address then
               Task_Unlock (Self_Id);
               return Initial_Value;

            else
               declare
                  Result : constant Attribute :=
                             To_Real_Attribute (Attr).Value;
               begin
                  Task_Unlock (Self_Id);
                  return Result;
               end;
            end if;
         end;
      end if;
   end Value;

end Ada.Task_Attributes;
