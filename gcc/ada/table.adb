------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                T A B L E                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2016, Free Software Foundation, Inc.         --
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

with Debug;   use Debug;
with Opt;     use Opt;
with Output;  use Output;
with System;  use System;
with Tree_IO; use Tree_IO;

with System.Memory; use System.Memory;

with Unchecked_Conversion;

pragma Elaborate_All (Output);

package body Table is
   package body Table is

      Min : constant Int := Int (Table_Low_Bound);
      --  Subscript of the minimum entry in the currently allocated table

      Length : Int := 0;
      --  Number of entries in currently allocated table. The value of zero
      --  ensures that we initially allocate the table.

      -----------------------
      -- Local Subprograms --
      -----------------------

      procedure Reallocate;
      --  Reallocate the existing table according to the current value stored
      --  in Max. Works correctly to do an initial allocation if the table
      --  is currently null.

      function Tree_Get_Table_Address return Address;
      --  Return Null_Address if the table length is zero,
      --  Table (First)'Address if not.

      pragma Warnings (Off);
      --  Turn off warnings. The following unchecked conversions are only used
      --  internally in this package, and cannot never result in any instances
      --  of improperly aliased pointers for the client of the package.

      function To_Address is new Unchecked_Conversion (Table_Ptr, Address);
      function To_Pointer is new Unchecked_Conversion (Address, Table_Ptr);

      pragma Warnings (On);

      ------------
      -- Append --
      ------------

      procedure Append (New_Val : Table_Component_Type) is
      begin
         Set_Item (Table_Index_Type (Last_Val + 1), New_Val);
      end Append;

      ----------------
      -- Append_All --
      ----------------

      procedure Append_All (New_Vals : Table_Type) is
      begin
         for J in New_Vals'Range loop
            Append (New_Vals (J));
         end loop;
      end Append_All;

      --------------------
      -- Decrement_Last --
      --------------------

      procedure Decrement_Last is
      begin
         Last_Val := Last_Val - 1;
      end Decrement_Last;

      ----------
      -- Free --
      ----------

      procedure Free is
      begin
         Free (To_Address (Table));
         Table := null;
         Length := 0;
      end Free;

      --------------------
      -- Increment_Last --
      --------------------

      procedure Increment_Last is
      begin
         Last_Val := Last_Val + 1;

         if Last_Val > Max then
            Reallocate;
         end if;
      end Increment_Last;

      ----------
      -- Init --
      ----------

      procedure Init is
         Old_Length : constant Int := Length;

      begin
         Locked   := False;
         Last_Val := Min - 1;
         Max      := Min + (Table_Initial * Table_Factor) - 1;
         Length   := Max - Min + 1;

         --  If table is same size as before (happens when table is never
         --  expanded which is a common case), then simply reuse it. Note
         --  that this also means that an explicit Init call right after
         --  the implicit one in the package body is harmless.

         if Old_Length = Length then
            return;

         --  Otherwise we can use Reallocate to get a table of the right size.
         --  Note that Reallocate works fine to allocate a table of the right
         --  initial size when it is first allocated.

         else
            Reallocate;
         end if;
      end Init;

      ----------
      -- Last --
      ----------

      function Last return Table_Index_Type is
      begin
         return Table_Index_Type (Last_Val);
      end Last;

      ----------------
      -- Reallocate --
      ----------------

      procedure Reallocate is
         New_Size   : Memory.size_t;
         New_Length : Long_Long_Integer;

      begin
         if Max < Last_Val then
            pragma Assert (not Locked);

            --  Make sure that we have at least the initial allocation. This
            --  is needed in cases where a zero length table is written out.

            Length := Int'Max (Length, Table_Initial);

            --  Now increment table length until it is sufficiently large. Use
            --  the increment value or 10, which ever is larger (the reason
            --  for the use of 10 here is to ensure that the table does really
            --  increase in size (which would not be the case for a table of
            --  length 10 increased by 3% for instance). Do the intermediate
            --  calculation in Long_Long_Integer to avoid overflow.

            while Max < Last_Val loop
               New_Length :=
                 Long_Long_Integer (Length) *
                    (100 + Long_Long_Integer (Table_Increment)) / 100;
               Length := Int'Max (Int (New_Length), Length + 10);
               Max := Min + Length - 1;
            end loop;

            if Debug_Flag_D then
               Write_Str ("--> Allocating new ");
               Write_Str (Table_Name);
               Write_Str (" table, size = ");
               Write_Int (Max - Min + 1);
               Write_Eol;
            end if;
         end if;

         --  Do the intermediate calculation in size_t to avoid signed overflow

         New_Size :=
           Memory.size_t (Max - Min + 1) *
                                    (Table_Type'Component_Size / Storage_Unit);

         if Table = null then
            Table := To_Pointer (Alloc (New_Size));

         elsif New_Size > 0 then
            Table :=
              To_Pointer (Realloc (Ptr  => To_Address (Table),
                                   Size => New_Size));
         end if;

         if Length /= 0 and then Table = null then
            Set_Standard_Error;
            Write_Str ("available memory exhausted");
            Write_Eol;
            Set_Standard_Output;
            raise Unrecoverable_Error;
         end if;
      end Reallocate;

      -------------
      -- Release --
      -------------

      procedure Release is
         Extra_Length : Int;
         Size         : Memory.size_t;

      begin
         Length := Last_Val - Int (Table_Low_Bound) + 1;
         Size   := Memory.size_t (Length) *
                     (Table_Type'Component_Size / Storage_Unit);

         --  If the size of the table exceeds the release threshold then leave
         --  space to store as many extra elements as 0.1% of the table length.

         if Release_Threshold > 0
           and then Size > Memory.size_t (Release_Threshold)
         then
            Extra_Length := Length / 1000;
            Length := Length + Extra_Length;
            Max    := Int (Table_Low_Bound) + Length - 1;

            if Debug_Flag_D then
               Write_Str ("--> Release_Threshold reached (length=");
               Write_Int (Int (Size));
               Write_Str ("): leaving room space for ");
               Write_Int (Extra_Length);
               Write_Str (" components");
               Write_Eol;
            end if;
         else
            Max := Last_Val;
         end if;

         Reallocate;
      end Release;

      -------------
      -- Restore --
      -------------

      procedure Restore (T : Saved_Table) is
      begin
         Free (To_Address (Table));
         Last_Val := T.Last_Val;
         Max      := T.Max;
         Table    := T.Table;
         Length   := Max - Min + 1;
      end Restore;

      ----------
      -- Save --
      ----------

      function Save return Saved_Table is
         Res : Saved_Table;

      begin
         Res.Last_Val := Last_Val;
         Res.Max      := Max;
         Res.Table    := Table;

         Table  := null;
         Length := 0;
         Init;
         return Res;
      end Save;

      --------------
      -- Set_Item --
      --------------

      procedure Set_Item
         (Index : Table_Index_Type;
          Item  : Table_Component_Type)
      is
         --  If Item is a value within the current allocation, and we are going
         --  to reallocate, then we must preserve an intermediate copy here
         --  before calling Increment_Last. Otherwise, if Table_Component_Type
         --  is passed by reference, we are going to end up copying from
         --  storage that might have been deallocated from Increment_Last
         --  calling Reallocate.

         subtype Allocated_Table_T is
           Table_Type (Table'First .. Table_Index_Type (Max + 1));
         --  A constrained table subtype one element larger than the currently
         --  allocated table.

         Allocated_Table_Address : constant System.Address :=
                                     Table.all'Address;
         --  Used for address clause below (we can't use non-static expression
         --  Table.all'Address directly in the clause because some older
         --  versions of the compiler do not allow it).

         Allocated_Table : Allocated_Table_T;
         pragma Import (Ada, Allocated_Table);
         pragma Suppress (Range_Check, On => Allocated_Table);
         for Allocated_Table'Address use Allocated_Table_Address;
         --  Allocated_Table represents the currently allocated array, plus one
         --  element (the supplementary element is used to have a convenient
         --  way of computing the address just past the end of the current
         --  allocation). Range checks are suppressed because this unit
         --  uses direct calls to System.Memory for allocation, and this can
         --  yield misaligned storage (and we cannot rely on the bootstrap
         --  compiler supporting specifically disabling alignment checks, so we
         --  need to suppress all range checks). It is safe to suppress this
         --  check here because we know that a (possibly misaligned) object
         --  of that type does actually exist at that address.
         --  ??? We should really improve the allocation circuitry here to
         --  guarantee proper alignment.

         Need_Realloc : constant Boolean := Int (Index) > Max;
         --  True if this operation requires storage reallocation (which may
         --  involve moving table contents around).

      begin
         --  If we're going to reallocate, check whether Item references an
         --  element of the currently allocated table.

         if Need_Realloc
           and then Allocated_Table'Address <= Item'Address
           and then Item'Address <
                      Allocated_Table (Table_Index_Type (Max + 1))'Address
         then
            --  If so, save a copy on the stack because Increment_Last will
            --  reallocate storage and might deallocate the current table.

            declare
               Item_Copy : constant Table_Component_Type := Item;
            begin
               Set_Last (Index);
               Table (Index) := Item_Copy;
            end;

         else
            --  Here we know that either we won't reallocate (case of Index <
            --  Max) or that Item is not in the currently allocated table.

            if Int (Index) > Last_Val then
               Set_Last (Index);
            end if;

            Table (Index) := Item;
         end if;
      end Set_Item;

      --------------
      -- Set_Last --
      --------------

      procedure Set_Last (New_Val : Table_Index_Type) is
      begin
         if Int (New_Val) < Last_Val then
            Last_Val := Int (New_Val);

         else
            Last_Val := Int (New_Val);

            if Last_Val > Max then
               Reallocate;
            end if;
         end if;
      end Set_Last;

      ----------------------------
      -- Tree_Get_Table_Address --
      ----------------------------

      function Tree_Get_Table_Address return Address is
      begin
         if Length = 0 then
            return Null_Address;
         else
            return Table (First)'Address;
         end if;
      end Tree_Get_Table_Address;

      ---------------
      -- Tree_Read --
      ---------------

      --  Note: we allocate only the space required to accommodate the data
      --  actually written, which means that a Tree_Write/Tree_Read sequence
      --  does an implicit Release.

      procedure Tree_Read is
      begin
         Tree_Read_Int (Max);
         Last_Val := Max;
         Length := Max - Min + 1;
         Reallocate;

         Tree_Read_Data
           (Tree_Get_Table_Address,
             (Last_Val - Int (First) + 1) *

               --  Note the importance of parenthesizing the following division
               --  to avoid the possibility of intermediate overflow.

               (Table_Type'Component_Size / Storage_Unit));
      end Tree_Read;

      ----------------
      -- Tree_Write --
      ----------------

      --  Note: we write out only the currently valid data, not the entire
      --  contents of the allocated array. See note above on Tree_Read.

      procedure Tree_Write is
      begin
         Tree_Write_Int (Int (Last));
         Tree_Write_Data
           (Tree_Get_Table_Address,
            (Last_Val - Int (First) + 1) *
              (Table_Type'Component_Size / Storage_Unit));
      end Tree_Write;

   begin
      Init;
   end Table;
end Table;
