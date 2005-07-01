------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . E X C E P T I O N _ T A B L E                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1996-2003 Free Software Foundation, Inc.          --
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

with System.HTable;
with System.Soft_Links;   use System.Soft_Links;

package body System.Exception_Table is

   use System.Standard_Library;

   type HTable_Headers is range 1 .. 37;

   procedure Set_HT_Link (T : Exception_Data_Ptr; Next : Exception_Data_Ptr);
   function  Get_HT_Link (T : Exception_Data_Ptr) return Exception_Data_Ptr;

   function Hash (F : Big_String_Ptr) return HTable_Headers;
   function Equal (A, B : Big_String_Ptr) return Boolean;
   function Get_Key (T : Exception_Data_Ptr) return Big_String_Ptr;

   package Exception_HTable is new System.HTable.Static_HTable (
     Header_Num => HTable_Headers,
     Element    => Exception_Data,
     Elmt_Ptr   => Exception_Data_Ptr,
     Null_Ptr   => null,
     Set_Next   => Set_HT_Link,
     Next       => Get_HT_Link,
     Key        => Big_String_Ptr,
     Get_Key    => Get_Key,
     Hash       => Hash,
     Equal      => Equal);

   -----------
   -- Equal --
   -----------

   function Equal (A, B : Big_String_Ptr) return Boolean is
      J    : Integer := 1;

   begin
      loop
         if A (J) /= B (J) then
            return False;

         elsif A (J) = ASCII.NUL then
            return True;

         else
            J := J + 1;
         end if;
      end loop;
   end Equal;

   -----------------
   -- Get_HT_Link --
   -----------------

   function  Get_HT_Link (T : Exception_Data_Ptr) return Exception_Data_Ptr is
   begin
      return T.HTable_Ptr;
   end Get_HT_Link;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (T : Exception_Data_Ptr) return Big_String_Ptr is
   begin
      return T.Full_Name;
   end Get_Key;

   -------------------------------
   -- Get_Registered_Exceptions --
   -------------------------------

   procedure Get_Registered_Exceptions
     (List : out Exception_Data_Array;
      Last : out Integer)
   is
      Data : Exception_Data_Ptr := Exception_HTable.Get_First;

   begin
      Lock_Task.all;
      Last := List'First - 1;

      while Last < List'Last and then Data /= null loop
         Last := Last + 1;
         List (Last) := Data;
         Data := Exception_HTable.Get_Next;
      end loop;

      Unlock_Task.all;
   end Get_Registered_Exceptions;

   ----------
   -- Hash --
   ----------

   function Hash (F : Big_String_Ptr) return HTable_Headers is
      type S is mod 2**8;

      Size : constant S := S (HTable_Headers'Last - HTable_Headers'First + 1);
      Tmp  : S := 0;
      J    : Positive;

   begin
      J := 1;
      loop
         if F (J) = ASCII.NUL then
            return HTable_Headers'First + HTable_Headers'Base (Tmp mod Size);
         else
            Tmp := Tmp xor S (Character'Pos (F (J)));
         end if;
         J := J + 1;
      end loop;
   end Hash;

   ------------------------
   -- Internal_Exception --
   ------------------------

   function Internal_Exception
     (X                   : String;
      Create_If_Not_Exist : Boolean := True) return Exception_Data_Ptr
   is
      type String_Ptr is access all String;

      Copy     : aliased String (X'First .. X'Last + 1);
      Res      : Exception_Data_Ptr;
      Dyn_Copy : String_Ptr;

   begin
      Copy (X'Range) := X;
      Copy (Copy'Last) := ASCII.NUL;
      Res := Exception_HTable.Get (To_Ptr (Copy'Address));

      --  If unknown exception, create it on the heap. This is a legitimate
      --  situation in the distributed case when an exception is defined only
      --  in a partition

      if Res = null and then Create_If_Not_Exist then
         Dyn_Copy := new String'(Copy);

         Res :=
           new Exception_Data'
             (Not_Handled_By_Others => False,
              Lang                  => 'A',
              Name_Length           => Copy'Length,
              Full_Name             => To_Ptr (Dyn_Copy.all'Address),
              HTable_Ptr            => null,
              Import_Code           => 0,
              Raise_Hook            => null);

         Register_Exception (Res);
      end if;

      return Res;
   end Internal_Exception;

   ------------------------
   -- Register_Exception --
   ------------------------

   procedure Register_Exception (X : Exception_Data_Ptr) is
   begin
      Exception_HTable.Set (X);
   end Register_Exception;

   ---------------------------------
   -- Registered_Exceptions_Count --
   ---------------------------------

   function Registered_Exceptions_Count return Natural is
      Count : Natural := 0;
      Data  : Exception_Data_Ptr := Exception_HTable.Get_First;

   begin
      --  We need to lock the runtime in the meantime, to avoid concurrent
      --  access since we have only one iterator.

      Lock_Task.all;

      while Data /= null loop
         Count := Count + 1;
         Data := Exception_HTable.Get_Next;
      end loop;

      Unlock_Task.all;
      return Count;
   end Registered_Exceptions_Count;

   -----------------
   -- Set_HT_Link --
   -----------------

   procedure Set_HT_Link
     (T    : Exception_Data_Ptr;
      Next : Exception_Data_Ptr)
   is
   begin
      T.HTable_Ptr := Next;
   end Set_HT_Link;

--  Register the standard exceptions at elaboration time

begin
   Register_Exception (Abort_Signal_Def'Access);
   Register_Exception (Tasking_Error_Def'Access);
   Register_Exception (Storage_Error_Def'Access);
   Register_Exception (Program_Error_Def'Access);
   Register_Exception (Numeric_Error_Def'Access);
   Register_Exception (Constraint_Error_Def'Access);

end System.Exception_Table;
