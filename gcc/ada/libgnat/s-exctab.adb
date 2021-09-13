------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . E X C E P T I O N _ T A B L E                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1996-2021, Free Software Foundation, Inc.         --
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

pragma Compiler_Unit_Warning;

with System.Soft_Links; use System.Soft_Links;

package body System.Exception_Table is

   use System.Standard_Library;

   type Hash_Val is mod 2 ** 8;
   subtype Hash_Idx is Hash_Val range 1 .. 37;

   HTable : array (Hash_Idx) of aliased Exception_Data_Ptr;
   --  Actual hash table containing all registered exceptions
   --
   --  The table is very small and the hash function weak, as looking up
   --  registered exceptions is rare and minimizing space and time overhead
   --  of registration is more important. In addition, it is expected that the
   --  exceptions that need to be looked up are registered dynamically, and
   --  therefore will be at the begin of the hash chains.
   --
   --  The table differs from System.HTable.Static_HTable in that the final
   --  element of each chain is not marked by null, but by a pointer to self.
   --  This way it is possible to defend against the same entry being inserted
   --  twice, without having to do a lookup which is relatively expensive for
   --  programs with large number
   --
   --  All non-local subprograms use the global Task_Lock to protect against
   --  concurrent use of the exception table. This is needed as local
   --  exceptions may be declared concurrently with those declared at the
   --  library level.

   --  Local Subprograms

   generic
      with procedure Process (T : Exception_Data_Ptr; More : out Boolean);
   procedure Iterate;
   --  Iterate over all

   function Lookup  (Name : String) return Exception_Data_Ptr;
   --  Find and return the Exception_Data of the exception with the given Name
   --  (which must be in all uppercase), or null if none was registered.

   procedure Register (Item : Exception_Data_Ptr);
   --  Register an exception with the given Exception_Data in the table.

   function Has_Name (Item : Exception_Data_Ptr; Name : String) return Boolean;
   --  Return True iff Item.Full_Name and Name are equal. Both names are
   --  assumed to be in all uppercase and end with ASCII.NUL.

   function Hash (S : String) return Hash_Idx;
   --  Return the index in the hash table for S, which is assumed to be all
   --  uppercase and end with ASCII.NUL.

   --------------
   -- Has_Name --
   --------------

   function Has_Name (Item : Exception_Data_Ptr; Name : String) return Boolean
   is
      S : constant Big_String_Ptr := To_Ptr (Item.Full_Name);
      J : Integer := S'First;

   begin
      for K in Name'Range loop

         --  Note that as both items are terminated with ASCII.NUL, the
         --  comparison below must fail for strings of different lengths.

         if S (J) /= Name (K) then
            return False;
         end if;

         J := J + 1;
      end loop;

      return True;
   end Has_Name;

   ------------
   -- Lookup --
   ------------

   function Lookup (Name : String) return Exception_Data_Ptr is
      Prev   : Exception_Data_Ptr;
      Curr   : Exception_Data_Ptr;

   begin
      Curr := HTable (Hash (Name));
      Prev := null;
      while Curr /= Prev loop
         if Has_Name (Curr, Name) then
            return Curr;
         end if;

         Prev := Curr;
         Curr := Curr.HTable_Ptr;
      end loop;

      return null;
   end Lookup;

   ----------
   -- Hash --
   ----------

   function Hash (S : String) return Hash_Idx is
      Hash : Hash_Val := 0;

   begin
      for J in S'Range loop
         exit when S (J) = ASCII.NUL;
         Hash := Hash xor Character'Pos (S (J));
      end loop;

      return Hash_Idx'First + Hash mod (Hash_Idx'Last - Hash_Idx'First + 1);
   end Hash;

   -------------
   -- Iterate --
   -------------

   procedure Iterate is
      More : Boolean;
      Prev, Curr : Exception_Data_Ptr;

   begin
      Outer : for Idx in HTable'Range loop
         Prev   := null;
         Curr   := HTable (Idx);

         while Curr /= Prev loop
               Process (Curr, More);

               exit Outer when not More;

               Prev := Curr;
               Curr := Curr.HTable_Ptr;
         end loop;
      end loop Outer;
   end Iterate;

   --------------
   -- Register --
   --------------

   procedure Register (Item : Exception_Data_Ptr) is
   begin
      if Item.HTable_Ptr = null then
         Prepend_To_Chain : declare
            Chain : Exception_Data_Ptr
                      renames HTable (Hash (To_Ptr (Item.Full_Name).all));

         begin
            if Chain = null then
               Item.HTable_Ptr := Item;
            else
               Item.HTable_Ptr := Chain;
            end if;

            Chain := Item;
         end Prepend_To_Chain;
      end if;
   end Register;

   -------------------------------
   -- Get_Registered_Exceptions --
   -------------------------------

   procedure Get_Registered_Exceptions
     (List : out Exception_Data_Array;
      Last : out Integer)
   is
      procedure Get_One (Item : Exception_Data_Ptr; More : out Boolean);
      --  Add Item to List (List'First .. Last) by first incrementing Last
      --  and storing Item in List (Last). Last should be in List'First - 1
      --  and List'Last.

      procedure Get_All is new Iterate (Get_One);
      --  Store all registered exceptions in List, updating Last

      -------------
      -- Get_One --
      -------------

      procedure Get_One (Item : Exception_Data_Ptr; More : out Boolean) is
      begin
         if Last < List'Last then
            Last := Last + 1;
            List (Last) := Item;
            More := True;

         else
            More := False;
         end if;
      end Get_One;

   begin
      --  In this routine the invariant is that List (List'First .. Last)
      --  contains the registered exceptions retrieved so far.

      Last := List'First - 1;

      Lock_Task.all;
      Get_All;
      Unlock_Task.all;
   end Get_Registered_Exceptions;

   ------------------------
   -- Internal_Exception --
   ------------------------

   function Internal_Exception
     (X                   : String;
      Create_If_Not_Exist : Boolean := True) return Exception_Data_Ptr
   is
      --  If X was not yet registered and Create_if_Not_Exist is True,
      --  dynamically allocate and register a new exception.

      type String_Ptr is access all String;

      Dyn_Copy : String_Ptr;
      Copy     : aliased String (X'First .. X'Last + 1);
      Result   : Exception_Data_Ptr;

   begin
      Lock_Task.all;

      Copy (X'Range) := X;
      Copy (Copy'Last) := ASCII.NUL;
      Result := Lookup (Copy);

      --  If unknown exception, create it on the heap. This is a legitimate
      --  situation in the distributed case when an exception is defined
      --  only in a partition

      if Result = null and then Create_If_Not_Exist then
         Dyn_Copy := new String'(Copy);

         Result :=
           new Exception_Data'
             (Not_Handled_By_Others => False,
              Lang                  => 'A',
              Name_Length           => Copy'Length,
              Full_Name             => Dyn_Copy.all'Address,
              HTable_Ptr            => null,
              Foreign_Data          => Null_Address,
              Raise_Hook            => null);

         Register (Result);
      end if;

      Unlock_Task.all;

      return Result;
   end Internal_Exception;

   ------------------------
   -- Register_Exception --
   ------------------------

   procedure Register_Exception (X : Exception_Data_Ptr) is
   begin
      Lock_Task.all;
      Register (X);
      Unlock_Task.all;
   end Register_Exception;

   ---------------------------------
   -- Registered_Exceptions_Count --
   ---------------------------------

   function Registered_Exceptions_Count return Natural is
      Count : Natural := 0;

      procedure Count_Item (Item : Exception_Data_Ptr; More : out Boolean);
      --  Update Count for given Item

      procedure Count_Item (Item : Exception_Data_Ptr; More : out Boolean) is
         pragma Unreferenced (Item);
      begin
         Count := Count + 1;
         More := Count < Natural'Last;
      end Count_Item;

      procedure Count_All is new Iterate (Count_Item);

   begin
      Lock_Task.all;
      Count_All;
      Unlock_Task.all;

      return Count;
   end Registered_Exceptions_Count;

begin
   --  Register the standard exceptions at elaboration time

   --  We don't need to use the locking version here as the elaboration
   --  will not be concurrent and no tasks can call any subprograms of this
   --  unit before it has been elaborated.

   Register (Abort_Signal_Def'Access);
   Register (Tasking_Error_Def'Access);
   Register (Storage_Error_Def'Access);
   Register (Program_Error_Def'Access);
   Register (Numeric_Error_Def'Access);
   Register (Constraint_Error_Def'Access);
end System.Exception_Table;
