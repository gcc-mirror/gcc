------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   G N A T . D Y N A M I C _ T A B L E S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2000-2004 Ada Core Technologies, Inc.            --
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

with GNAT.Heap_Sort_G;
with System;        use System;
with System.Memory; use System.Memory;

with Unchecked_Conversion;

package body GNAT.Dynamic_Tables is

   Min : constant Integer := Integer (Table_Low_Bound);
   --  Subscript of the minimum entry in the currently allocated table

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Reallocate (T : in out Instance);
   --  Reallocate the existing table according to the current value stored
   --  in Max. Works correctly to do an initial allocation if the table
   --  is currently null.

   pragma Warnings (Off);
   --  These unchecked conversions are in fact safe, since they never
   --  generate improperly aliased pointer values.

   function To_Address is new Unchecked_Conversion (Table_Ptr, Address);
   function To_Pointer is new Unchecked_Conversion (Address, Table_Ptr);

   pragma Warnings (On);

   --------------
   -- Allocate --
   --------------

   procedure Allocate
     (T   : in out Instance;
      Num : Integer := 1)
   is
   begin
      T.P.Last_Val := T.P.Last_Val + Num;

      if T.P.Last_Val > T.P.Max then
         Reallocate (T);
      end if;
   end Allocate;

   ------------
   -- Append --
   ------------

   procedure Append (T : in out Instance; New_Val : Table_Component_Type) is
   begin
      Increment_Last (T);
      T.Table (Table_Index_Type (T.P.Last_Val)) := New_Val;
   end Append;

   --------------------
   -- Decrement_Last --
   --------------------

   procedure Decrement_Last (T : in out Instance) is
   begin
      T.P.Last_Val := T.P.Last_Val - 1;
   end Decrement_Last;

   --------------
   -- For_Each --
   --------------

   procedure For_Each (Table : Instance) is
      Quit : Boolean := False;
   begin
      for Index in Table_Low_Bound .. Table_Index_Type (Table.P.Last_Val) loop
         Action (Index, Table.Table (Index), Quit);
         exit when Quit;
      end loop;
   end For_Each;

   ----------
   -- Free --
   ----------

   procedure Free (T : in out Instance) is
   begin
      Free (To_Address (T.Table));
      T.Table := null;
      T.P.Length := 0;
   end Free;

   --------------------
   -- Increment_Last --
   --------------------

   procedure Increment_Last (T : in out Instance) is
   begin
      T.P.Last_Val := T.P.Last_Val + 1;

      if T.P.Last_Val > T.P.Max then
         Reallocate (T);
      end if;
   end Increment_Last;

   ----------
   -- Init --
   ----------

   procedure Init (T : in out Instance) is
      Old_Length : constant Integer := T.P.Length;

   begin
      T.P.Last_Val := Min - 1;
      T.P.Max      := Min + Table_Initial - 1;
      T.P.Length   := T.P.Max - Min + 1;

      --  If table is same size as before (happens when table is never
      --  expanded which is a common case), then simply reuse it. Note
      --  that this also means that an explicit Init call right after
      --  the implicit one in the package body is harmless.

      if Old_Length = T.P.Length then
         return;

      --  Otherwise we can use Reallocate to get a table of the right size.
      --  Note that Reallocate works fine to allocate a table of the right
      --  initial size when it is first allocated.

      else
         Reallocate (T);
      end if;
   end Init;

   ----------
   -- Last --
   ----------

   function Last (T : in Instance) return Table_Index_Type is
   begin
      return Table_Index_Type (T.P.Last_Val);
   end Last;

   ----------------
   -- Reallocate --
   ----------------

   procedure Reallocate (T : in out Instance) is
      New_Length : Integer;
      New_Size   : size_t;

   begin
      if T.P.Max < T.P.Last_Val then
         while T.P.Max < T.P.Last_Val loop
            New_Length := T.P.Length * (100 + Table_Increment) / 100;

            if New_Length > T.P.Length then
               T.P.Length := New_Length;
            else
               T.P.Length := T.P.Length + 1;
            end if;

            T.P.Max := Min + T.P.Length - 1;
         end loop;
      end if;

      New_Size :=
        size_t ((T.P.Max - Min + 1) *
                (Table_Type'Component_Size / Storage_Unit));

      if T.Table = null then
         T.Table := To_Pointer (Alloc (New_Size));

      elsif New_Size > 0 then
         T.Table :=
           To_Pointer (Realloc (Ptr  => To_Address (T.Table),
                                Size => New_Size));
      end if;

      if T.P.Length /= 0 and then T.Table = null then
         raise Storage_Error;
      end if;
   end Reallocate;

   -------------
   -- Release --
   -------------

   procedure Release (T : in out Instance) is
   begin
      T.P.Length := T.P.Last_Val - Integer (Table_Low_Bound) + 1;
      T.P.Max    := T.P.Last_Val;
      Reallocate (T);
   end Release;

   --------------
   -- Set_Item --
   --------------

   procedure Set_Item
     (T     : in out Instance;
      Index : Table_Index_Type;
      Item  : Table_Component_Type)
   is
   begin
      if Integer (Index) > T.P.Last_Val then
         Set_Last (T, Index);
      end if;

      T.Table (Index) := Item;
   end Set_Item;

   --------------
   -- Set_Last --
   --------------

   procedure Set_Last (T : in out Instance; New_Val : Table_Index_Type) is
   begin
      if Integer (New_Val) < T.P.Last_Val then
         T.P.Last_Val := Integer (New_Val);

      else
         T.P.Last_Val := Integer (New_Val);

         if T.P.Last_Val > T.P.Max then
            Reallocate (T);
         end if;
      end if;
   end Set_Last;

   ----------------
   -- Sort_Table --
   ----------------

   procedure Sort_Table (Table : in out Instance) is

      Temp : Table_Component_Type;
      --  A temporary position to simulate index 0

      --  Local subprograms

      function Index_Of (Idx : Natural) return Table_Index_Type;
      --  Apply Natural to indexs of the table

      function Lower_Than (Op1, Op2 : Natural) return Boolean;
      --  Compare two components

      procedure Move (From : Natural; To : Natural);
      --  Move one component

      package Heap_Sort is new GNAT.Heap_Sort_G (Move, Lower_Than);

      --------------
      -- Index_Of --
      --------------

      function Index_Of (Idx : Natural) return Table_Index_Type is
      begin
         return First + Table_Index_Type (Idx) - 1;
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
              Lt (Table.Table (Index_Of (Op1)),
                   Table.Table (Index_Of (Op2)));
         end if;
      end Lower_Than;

   --  Start of processing for Sort_Table

   begin

      Heap_Sort.Sort (Natural (Last (Table) - First) + 1);

   end Sort_Table;

end GNAT.Dynamic_Tables;
