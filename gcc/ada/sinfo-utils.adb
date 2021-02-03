------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                           S I N F O . U T I L S                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2020-2021, Free Software Foundation, Inc.        --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Atree;
with Seinfo;

package body Sinfo.Utils is

   -------------------------
   -- Iterator Procedures --
   -------------------------

   procedure Next_Entity       (N : in out Node_Id) is
   begin
      N := Next_Entity (N);
   end Next_Entity;

   procedure Next_Named_Actual (N : in out Node_Id) is
   begin
      N := Next_Named_Actual (N);
   end Next_Named_Actual;

   procedure Next_Rep_Item     (N : in out Node_Id) is
   begin
      N := Next_Rep_Item (N);
   end Next_Rep_Item;

   procedure Next_Use_Clause   (N : in out Node_Id) is
   begin
      N := Next_Use_Clause (N);
   end Next_Use_Clause;

   ------------------
   -- End_Location --
   ------------------

   function End_Location (N : Node_Id) return Source_Ptr is
      L : constant Uint := End_Span (N);
   begin
      if L = No_Uint then
         return No_Location;
      else
         return Source_Ptr (Int (Sloc (N)) + UI_To_Int (L));
      end if;
   end End_Location;

   --------------------
   -- Get_Pragma_Arg --
   --------------------

   function Get_Pragma_Arg (Arg : Node_Id) return Node_Id is
   begin
      if Nkind (Arg) = N_Pragma_Argument_Association then
         return Expression (Arg);
      else
         return Arg;
      end if;
   end Get_Pragma_Arg;

   ----------------------
   -- Set_End_Location --
   ----------------------

   procedure Set_End_Location (N : Node_Id; S : Source_Ptr) is
   begin
      Set_End_Span (N,
        UI_From_Int (Int (S) - Int (Sloc (N))));
   end Set_End_Location;

   --------------------------
   -- Pragma_Name_Unmapped --
   --------------------------

   function Pragma_Name_Unmapped (N : Node_Id) return Name_Id is
   begin
      return Chars (Pragma_Identifier (N));
   end Pragma_Name_Unmapped;

   ------------------------------------
   -- Helpers for Walk_Sinfo_Fields* --
   ------------------------------------

   function Get_Node_Field_Union is new
     Atree.Atree_Private_Part.Get_32_Bit_Field (Union_Id) with Inline;
   procedure Set_Node_Field_Union is new
     Atree.Atree_Private_Part.Set_32_Bit_Field (Union_Id) with Inline;

   use Seinfo;

   function Is_In_Union_Id (F_Kind : Field_Kind) return Boolean is
     (F_Kind in Node_Id_Field
              | List_Id_Field
              | Elist_Id_Field
              | Name_Id_Field
              | String_Id_Field
              | Uint_Field
              | Ureal_Field
              | Union_Id_Field);
   --  True if the field type is one that can be converted to Types.Union_Id

   -----------------------
   -- Walk_Sinfo_Fields --
   -----------------------

   procedure Walk_Sinfo_Fields (N : Node_Id) is
      Fields : Node_Field_Array renames
        Node_Field_Table (Nkind (N)).all;

   begin
      for J in Fields'Range loop
         if Fields (J) /= Link then -- Don't walk Parent!
            declare
               Desc : Field_Descriptor renames
                 Node_Field_Descriptors (Fields (J));
            begin
               if Is_In_Union_Id (Desc.Kind) then
                  Action (Get_Node_Field_Union (N, Desc.Offset));
               end if;
            end;
         end if;
      end loop;
   end Walk_Sinfo_Fields;

   --------------------------------
   -- Walk_Sinfo_Fields_Pairwise --
   --------------------------------

   procedure Walk_Sinfo_Fields_Pairwise (N1, N2 : Node_Id) is
      pragma Assert (Nkind (N1) = Nkind (N2));

      Fields : Node_Field_Array renames
        Node_Field_Table (Nkind (N1)).all;

   begin
      for J in Fields'Range loop
         if Fields (J) /= Link then -- Don't walk Parent!
            declare
               Desc : Field_Descriptor renames
                 Node_Field_Descriptors (Fields (J));
            begin
               if Is_In_Union_Id (Desc.Kind) then
                  Set_Node_Field_Union
                    (N1, Desc.Offset,
                     Transform (Get_Node_Field_Union (N2, Desc.Offset)));
               end if;
            end;
         end if;
      end loop;
   end Walk_Sinfo_Fields_Pairwise;

   ---------------------
   -- Map_Pragma_Name --
   ---------------------

   --  We don't want to introduce a dependence on some hash table package or
   --  similar, so we use a simple array of Key => Value pairs, and do a linear
   --  search. Linear search is plenty efficient, given that we don't expect
   --  more than a couple of entries in the mapping.

   type Name_Pair is record
      Key   : Name_Id;
      Value : Name_Id;
   end record;

   type Pragma_Map_Index is range 1 .. 100;
   Pragma_Map : array (Pragma_Map_Index) of Name_Pair;
   Last_Pair : Pragma_Map_Index'Base range 0 .. Pragma_Map_Index'Last := 0;

   procedure Map_Pragma_Name (From, To : Name_Id) is
   begin
      if Last_Pair = Pragma_Map'Last then
         raise Too_Many_Pragma_Mappings;
      end if;

      Last_Pair := Last_Pair + 1;
      Pragma_Map (Last_Pair) := (Key => From, Value => To);
   end Map_Pragma_Name;

   -----------------
   -- Pragma_Name --
   -----------------

   function Pragma_Name (N : Node_Id) return Name_Id is
      Result : constant Name_Id := Pragma_Name_Unmapped (N);
   begin
      for J in Pragma_Map'First .. Last_Pair loop
         if Result = Pragma_Map (J).Key then
            return Pragma_Map (J).Value;
         end if;
      end loop;

      return Result;
   end Pragma_Name;

end Sinfo.Utils;
