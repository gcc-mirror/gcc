------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   G N A T . D Y N A M I C _ T A B L E S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2000-2023, AdaCore                     --
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

with GNAT.Heap_Sort_G;

with Ada.Unchecked_Deallocation;
with System;

package body GNAT.Dynamic_Tables is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Last_Allocated (T : Instance) return Table_Last_Type;
   pragma Inline (Last_Allocated);
   --  Return the index of the last allocated element

   procedure Grow (T : in out Instance; New_Last : Table_Last_Type);
   --  This is called when we are about to set the value of Last to a value
   --  that is larger than Last_Allocated. This reallocates the table to the
   --  larger size, as indicated by New_Last. At the time this is called,
   --  Last (T) is still the old value, and this does not modify it.

   --------------
   -- Allocate --
   --------------

   procedure Allocate (T : in out Instance; Num : Integer := 1) is
   begin
      --  Note that Num can be negative

      pragma Assert (not T.Locked);
      Set_Last (T, Last (T) + Table_Index_Type'Base (Num));
   end Allocate;

   ------------
   -- Append --
   ------------

   procedure Append (T : in out Instance; New_Val : Table_Component_Type) is
      pragma Assert (not T.Locked);
      New_Last : constant Table_Last_Type := Last (T) + 1;

   begin
      if New_Last <= Last_Allocated (T) then

         --  Fast path

         T.P.Last := New_Last;
         T.Table (New_Last) := New_Val;

      else
         Set_Item (T, New_Last, New_Val);
      end if;
   end Append;

   ----------------
   -- Append_All --
   ----------------

   procedure Append_All (T : in out Instance; New_Vals : Table_Type) is
   begin
      for J in New_Vals'Range loop
         Append (T, New_Vals (J));
      end loop;
   end Append_All;

   --------------------
   -- Decrement_Last --
   --------------------

   procedure Decrement_Last (T : in out Instance) is
   begin
      pragma Assert (not T.Locked);
      Allocate (T, -1);
   end Decrement_Last;

   -----------
   -- First --
   -----------

   function First return Table_Index_Type is
   begin
      return Table_Low_Bound;
   end First;

   --------------
   -- For_Each --
   --------------

   procedure For_Each (Table : Instance) is
      Quit : Boolean := False;
   begin
      for Index in First .. Last (Table) loop
         Action (Index, Table.Table (Index), Quit);
         exit when Quit;
      end loop;
   end For_Each;

   ----------
   -- Grow --
   ----------

   procedure Grow (T : in out Instance; New_Last : Table_Last_Type) is

      --  Note: Type Alloc_Ptr below needs to be declared locally so we know
      --  the bounds. That means that the collection is local, so is finalized
      --  when leaving Grow. That's why this package doesn't support controlled
      --  types; the table elements would be finalized prematurely. An Ada
      --  implementation would also be within its rights to reclaim the
      --  storage. Fortunately, GNAT doesn't do that.

      pragma Assert (not T.Locked);
      pragma Assert (New_Last > Last_Allocated (T));

      subtype Table_Length_Type is Table_Index_Type'Base
        range 0 .. Table_Index_Type'Base'Last;

      Old_Last_Allocated   : constant Table_Last_Type := Last_Allocated (T);
      Old_Allocated_Length : constant Table_Length_Type :=
                               Old_Last_Allocated - First + 1;

      New_Length : constant Table_Length_Type := New_Last - First + 1;
      New_Allocated_Length : Table_Length_Type;

   begin
      if T.Table = Empty_Table_Ptr then
         New_Allocated_Length := Table_Length_Type (Table_Initial);
      else
         New_Allocated_Length :=
           Table_Length_Type
             (Long_Long_Integer (Old_Allocated_Length) *
               (100 + Long_Long_Integer (Table_Increment)) / 100);
      end if;

      --  Make sure it really did grow

      if New_Allocated_Length <= Old_Allocated_Length then
         New_Allocated_Length := Old_Allocated_Length + 10;
      end if;

      if New_Allocated_Length <= New_Length then
         New_Allocated_Length := New_Length + 10;
      end if;

      pragma Assert (New_Allocated_Length > Old_Allocated_Length);
      pragma Assert (New_Allocated_Length > New_Length);

      T.P.Last_Allocated := First + New_Allocated_Length - 1;

      declare
         subtype Old_Alloc_Type is Table_Type (First .. Old_Last_Allocated);
         type Old_Alloc_Ptr is access all Old_Alloc_Type;

         procedure Free is
           new Ada.Unchecked_Deallocation (Old_Alloc_Type, Old_Alloc_Ptr);
         function To_Old_Alloc_Ptr is
           new Ada.Unchecked_Conversion (Table_Ptr, Old_Alloc_Ptr);

         subtype Alloc_Type is
           Table_Type (First .. First + New_Allocated_Length - 1);
         type Alloc_Ptr is access all Alloc_Type;

         function To_Table_Ptr is
           new Ada.Unchecked_Conversion (Alloc_Ptr, Table_Ptr);

         Old_Table : Old_Alloc_Ptr := To_Old_Alloc_Ptr (T.Table);
         New_Table : constant Alloc_Ptr := new Alloc_Type;

      begin
         if T.Table /= Empty_Table_Ptr then
            New_Table (First .. Last (T)) := Old_Table (First .. Last (T));
            Free (Old_Table);
         end if;

         T.Table := To_Table_Ptr (New_Table);
      end;

      pragma Assert (New_Last <= Last_Allocated (T));
      pragma Assert (T.Table /= null);
      pragma Assert (T.Table /= Empty_Table_Ptr);
   end Grow;

   --------------------
   -- Increment_Last --
   --------------------

   procedure Increment_Last (T : in out Instance) is
   begin
      pragma Assert (not T.Locked);
      Allocate (T, 1);
   end Increment_Last;

   ----------
   -- Init --
   ----------

   procedure Init (T : in out Instance) is
      pragma Assert (not T.Locked);
      subtype Alloc_Type is Table_Type (First .. Last_Allocated (T));
      type Alloc_Ptr is access all Alloc_Type;

      procedure Free is new Ada.Unchecked_Deallocation (Alloc_Type, Alloc_Ptr);
      function To_Alloc_Ptr is
        new Ada.Unchecked_Conversion (Table_Ptr, Alloc_Ptr);

      Temp : Alloc_Ptr := To_Alloc_Ptr (T.Table);

   begin
      if T.Table = Empty_Table_Ptr then
         pragma Assert (T.P = (Last_Allocated | Last => First - 1));
         null;
      else
         Free (Temp);
         T.Table := Empty_Table_Ptr;
         T.P := (Last_Allocated | Last => First - 1);
      end if;
   end Init;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (T : Instance) return Boolean is
   begin
      return Last (T) = First - 1;
   end Is_Empty;

   ----------
   -- Last --
   ----------

   function Last (T : Instance) return Table_Last_Type is
   begin
      return T.P.Last;
   end Last;

   --------------------
   -- Last_Allocated --
   --------------------

   function Last_Allocated (T : Instance) return Table_Last_Type is
   begin
      return T.P.Last_Allocated;
   end Last_Allocated;

   ----------
   -- Move --
   ----------

   procedure Move (From, To : in out Instance) is
   begin
      pragma Assert (not From.Locked);
      pragma Assert (not To.Locked);
      pragma Assert (Is_Empty (To));
      To := From;

      From.Table            := Empty_Table_Ptr;
      From.Locked           := False;
      From.P.Last_Allocated := First - 1;
      From.P.Last           := First - 1;
      pragma Assert (Is_Empty (From));
   end Move;

   -------------
   -- Release --
   -------------

   procedure Release (T : in out Instance) is
      pragma Assert (not T.Locked);
      Old_Last_Allocated : constant Table_Last_Type := Last_Allocated (T);

      function New_Last_Allocated return Table_Last_Type;
      --  Compute the new value of Last_Allocated. This is normally equal to
      --  Last, but if Release_Threshold /= 0, then we need to take that into
      --  account.

      ------------------------
      -- New_Last_Allocated --
      ------------------------

      function New_Last_Allocated return Table_Last_Type is
         subtype Table_Length_Type is Table_Index_Type'Base
           range 0 .. Table_Index_Type'Base'Last;

         Length : constant Table_Length_Type := Last (T) - First + 1;

         Comp_Size_In_Bytes : constant Table_Length_Type :=
           Table_Type'Component_Size / System.Storage_Unit;

         Length_Threshold : constant Table_Length_Type :=
           Table_Length_Type (Release_Threshold) / Comp_Size_In_Bytes;

      begin
         if Release_Threshold = 0 or else Length < Length_Threshold then
            return Last (T);
         else
            declare
               Extra_Length : constant Table_Length_Type := Length / 1000;
            begin
               return (Length + Extra_Length) - 1 + First;
            end;
         end if;
      end New_Last_Allocated;

      --  Local variables

      New_Last_Alloc : constant Table_Last_Type := New_Last_Allocated;

   --  Start of processing for Release

   begin
      if New_Last_Alloc < Last_Allocated (T) then
         pragma Assert (Last (T) < Last_Allocated (T));
         pragma Assert (T.Table /= Empty_Table_Ptr);

         declare
            subtype Old_Alloc_Type is Table_Type (First .. Old_Last_Allocated);
            type Old_Alloc_Ptr is access all Old_Alloc_Type;

            procedure Free is
              new Ada.Unchecked_Deallocation (Old_Alloc_Type, Old_Alloc_Ptr);
            function To_Old_Alloc_Ptr is
              new Ada.Unchecked_Conversion (Table_Ptr, Old_Alloc_Ptr);

            subtype Alloc_Type is Table_Type (First .. New_Last_Alloc);
            type Alloc_Ptr is access all Alloc_Type;

            function To_Table_Ptr is
              new Ada.Unchecked_Conversion (Alloc_Ptr, Table_Ptr);

            Old_Table : Old_Alloc_Ptr := To_Old_Alloc_Ptr (T.Table);
            New_Table : constant Alloc_Ptr := new Alloc_Type;

         begin
            New_Table (First .. Last (T)) := Old_Table (First .. Last (T));
            T.P.Last_Allocated := New_Last_Alloc;
            Free (Old_Table);
            T.Table := To_Table_Ptr (New_Table);
         end;
      end if;
   end Release;

   --------------
   -- Set_Item --
   --------------

   procedure Set_Item
     (T     : in out Instance;
      Index : Valid_Table_Index_Type;
      Item  : Table_Component_Type)
   is
   begin
      pragma Assert (not T.Locked);

      --  If Set_Last is going to reallocate the table, we make a copy of Item,
      --  in case the call was "Set_Item (T, X, T.Table (Y));", and Item is
      --  passed by reference. Without the copy, we would deallocate the array
      --  containing Item, leaving a dangling pointer.

      if Index > Last_Allocated (T) then
         declare
            Item_Copy : constant Table_Component_Type := Item;
         begin
            Set_Last (T, Index);
            T.Table (Index) := Item_Copy;
         end;

      else
         if Index > Last (T) then
            Set_Last (T, Index);
         end if;

         T.Table (Index) := Item;
      end if;
   end Set_Item;

   --------------
   -- Set_Last --
   --------------

   procedure Set_Last (T : in out Instance; New_Val : Table_Last_Type) is
   begin
      pragma Assert (not T.Locked);
      if New_Val > Last_Allocated (T) then
         Grow (T, New_Val);
      end if;

      T.P.Last := New_Val;
   end Set_Last;

   ----------------
   -- Sort_Table --
   ----------------

   procedure Sort_Table (Table : in out Instance) is
      Temp : Table_Component_Type;
      --  A temporary position to simulate index 0

      --  Local subprograms

      function Index_Of (Idx : Natural) return Table_Index_Type'Base;
      --  Return index of Idx'th element of table

      function Lower_Than (Op1, Op2 : Natural) return Boolean;
      --  Compare two components

      procedure Move (From : Natural; To : Natural);
      --  Move one component

      package Heap_Sort is new GNAT.Heap_Sort_G (Move, Lower_Than);

      --------------
      -- Index_Of --
      --------------

      function Index_Of (Idx : Natural) return Table_Index_Type'Base is
         J : constant Integer'Base :=
               Table_Index_Type'Base'Pos (First) + Idx - 1;
      begin
         return Table_Index_Type'Base'Val (J);
      end Index_Of;

      ----------
      -- Move --
      ----------

      procedure Move (From : Natural; To : Natural) is
      begin
         if From = 0 then
            Table.Table (Index_Of (To)) := Temp;

         elsif To = 0 then
            Temp := Table.Table (Index_Of (From));

         else
            Table.Table (Index_Of (To)) :=
              Table.Table (Index_Of (From));
         end if;
      end Move;

      ----------------
      -- Lower_Than --
      ----------------

      function Lower_Than (Op1, Op2 : Natural) return Boolean is
      begin
         if Op1 = 0 then
            return Lt (Temp, Table.Table (Index_Of (Op2)));

         elsif Op2 = 0 then
            return Lt (Table.Table (Index_Of (Op1)), Temp);

         else
            return
              Lt (Table.Table (Index_Of (Op1)), Table.Table (Index_Of (Op2)));
         end if;
      end Lower_Than;

   --  Start of processing for Sort_Table

   begin
      Heap_Sort.Sort (Natural (Last (Table) - First) + 1);
   end Sort_Table;

end GNAT.Dynamic_Tables;
