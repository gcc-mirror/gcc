------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   G N A T . D Y N A M I C _ T A B L E S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--           Copyright (C) 2000-2001 Ada Core Technologies, Inc.            --
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
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

with System;        use System;
with System.Memory; use System.Memory;
with System.Address_To_Access_Conversions;

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

   package Table_Conversions is
      new System.Address_To_Access_Conversions (Big_Table_Type);
   --  Address and Access conversions for a Table object.

   function To_Address (Table : Table_Ptr) return Address;
   pragma Inline (To_Address);
   --  Returns the Address for the Table object.

   function To_Pointer (Table : Address) return Table_Ptr;
   pragma Inline (To_Pointer);
   --  Returns the Access pointer for the Table object.

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
      New_Size : size_t;

   begin
      if T.P.Max < T.P.Last_Val then
         while T.P.Max < T.P.Last_Val loop
            T.P.Length := T.P.Length * (100 + Table_Increment) / 100;
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
      if Integer (Index) > T.P.Max then
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
   -- To_Address --
   ----------------

   function To_Address (Table : Table_Ptr) return Address is
   begin
      return Table_Conversions.To_Address
        (Table_Conversions.Object_Pointer (Table));
   end To_Address;

   ----------------
   -- To_Pointer --
   ----------------

   function To_Pointer (Table : Address) return Table_Ptr is
   begin
      return Table_Ptr (Table_Conversions.To_Pointer (Table));
   end To_Pointer;

end GNAT.Dynamic_Tables;
