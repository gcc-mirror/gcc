------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                             A D A . T A G S                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2002 Free Software Foundation, Inc.          --
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

with Ada.Exceptions;

with System.HTable;

with Unchecked_Conversion;

pragma Elaborate_All (System.HTable);

package body Ada.Tags is

--  Structure of the GNAT Dispatch Table

--   +----------------------+
--   |      TSD pointer  ---|-----> Type Specific Data
--   +----------------------+       +-------------------+
--   | table of             |       | inheritance depth |
--   :   primitive ops      :       +-------------------+
--   |     pointers         |       |   expanded name   |
--   +----------------------+       +-------------------+
--                                  |   external tag    |
--                                  +-------------------+
--                                  |   Hash table link |
--                                  +-------------------+
--                                  | Remotely Callable |
--                                  +-------------------+
--                                  | Rec Ctrler offset |
--                                  +-------------------+
--                                  | table of          |
--                                  :   ancestor        :
--                                  |      tags         |
--                                  +-------------------+

   subtype Cstring is String (Positive);
   type Cstring_Ptr is access all Cstring;
   type Tag_Table is array (Natural range <>) of Tag;
   pragma Suppress_Initialization (Tag_Table);

   type Wide_Boolean is new Boolean;
   --  This name should probably be changed sometime ??? and indeed
   --  probably this field could simply be of type Standard.Boolean.

   type Type_Specific_Data is record
      Idepth             : Natural;
      Expanded_Name      : Cstring_Ptr;
      External_Tag       : Cstring_Ptr;
      HT_Link            : Tag;
      Remotely_Callable  : Wide_Boolean;
      RC_Offset          : SSE.Storage_Offset;
      Ancestor_Tags      : Tag_Table (Natural);
   end record;

   type Dispatch_Table is record
      TSD       : Type_Specific_Data_Ptr;
      Prims_Ptr : Address_Array (Positive);
   end record;

   -------------------------------------------
   -- Unchecked Conversions for Tag and TSD --
   -------------------------------------------

   function To_Type_Specific_Data_Ptr is
     new Unchecked_Conversion (S.Address, Type_Specific_Data_Ptr);

   function To_Address is
     new Unchecked_Conversion (Type_Specific_Data_Ptr, S.Address);

   ---------------------------------------------
   -- Unchecked Conversions for String Fields --
   ---------------------------------------------

   function To_Cstring_Ptr is
     new Unchecked_Conversion (S.Address, Cstring_Ptr);

   function To_Address is
     new Unchecked_Conversion (Cstring_Ptr, S.Address);

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Length (Str : Cstring_Ptr) return Natural;
   --  Length of string represented by the given pointer (treating the
   --  string as a C-style string, which is Nul terminated).

   -------------------------
   -- External_Tag_HTable --
   -------------------------

   type HTable_Headers is range 1 .. 64;

   --  The following internal package defines the routines used for
   --  the instantiation of a new System.HTable.Static_HTable (see
   --  below). See spec in g-htable.ads for details of usage.

   package HTable_Subprograms is
      procedure Set_HT_Link (T : Tag; Next : Tag);
      function  Get_HT_Link (T : Tag) return Tag;
      function Hash (F : S.Address) return HTable_Headers;
      function Equal (A, B : S.Address) return Boolean;
   end HTable_Subprograms;

   package External_Tag_HTable is new System.HTable.Static_HTable (
     Header_Num => HTable_Headers,
     Element    => Dispatch_Table,
     Elmt_Ptr   => Tag,
     Null_Ptr   => null,
     Set_Next   => HTable_Subprograms.Set_HT_Link,
     Next       => HTable_Subprograms.Get_HT_Link,
     Key        => S.Address,
     Get_Key    => Get_External_Tag,
     Hash       => HTable_Subprograms.Hash,
     Equal      => HTable_Subprograms.Equal);

   ------------------------
   -- HTable_Subprograms --
   ------------------------

   --  Bodies of routines for hash table instantiation

   package body HTable_Subprograms is

   -----------
   -- Equal --
   -----------

      function Equal (A, B : S.Address) return Boolean is
         Str1 : constant Cstring_Ptr := To_Cstring_Ptr (A);
         Str2 : constant Cstring_Ptr := To_Cstring_Ptr (B);
         J    : Integer := 1;

      begin
         loop
            if Str1 (J) /= Str2 (J) then
               return False;

            elsif Str1 (J) = ASCII.NUL then
               return True;

            else
               J := J + 1;
            end if;
         end loop;
      end Equal;

      -----------------
      -- Get_HT_Link --
      -----------------

      function Get_HT_Link (T : Tag) return Tag is
      begin
         return T.TSD.HT_Link;
      end Get_HT_Link;

      ----------
      -- Hash --
      ----------

      function Hash (F : S.Address) return HTable_Headers is
         function H is new System.HTable.Hash (HTable_Headers);
         Str : constant Cstring_Ptr    := To_Cstring_Ptr (F);
         Res : constant HTable_Headers := H (Str (1 .. Length (Str)));

      begin
         return Res;
      end Hash;

      -----------------
      -- Set_HT_Link --
      -----------------

      procedure Set_HT_Link (T : Tag; Next : Tag) is
      begin
         T.TSD.HT_Link := Next;
      end Set_HT_Link;

   end HTable_Subprograms;

   --------------------
   --  CW_Membership --
   --------------------

   --  Canonical implementation of Classwide Membership corresponding to:

   --     Obj in Typ'Class

   --  Each dispatch table contains a reference to a table of ancestors
   --  (Ancestor_Tags) and a count of the level of inheritance "Idepth" .

   --  Obj is in Typ'Class if Typ'Tag is in the table of ancestors that are
   --  contained in the dispatch table referenced by Obj'Tag . Knowing the
   --  level of inheritance of both types, this can be computed in constant
   --  time by the formula:

   --   Obj'tag.TSD.Ancestor_Tags (Obj'tag.TSD.Idepth - Typ'tag.TSD.Idepth)
   --     = Typ'tag

   function CW_Membership (Obj_Tag : Tag; Typ_Tag : Tag) return Boolean is
      Pos : constant Integer := Obj_Tag.TSD.Idepth - Typ_Tag.TSD.Idepth;

   begin
      return Pos >= 0 and then Obj_Tag.TSD.Ancestor_Tags (Pos) = Typ_Tag;
   end CW_Membership;

   -------------------
   -- Expanded_Name --
   -------------------

   function Expanded_Name (T : Tag) return String is
      Result : constant Cstring_Ptr := T.TSD.Expanded_Name;

   begin
      return Result (1 .. Length (Result));
   end Expanded_Name;

   ------------------
   -- External_Tag --
   ------------------

   function External_Tag (T : Tag) return String is
      Result : constant Cstring_Ptr := T.TSD.External_Tag;

   begin
      return Result (1 .. Length (Result));
   end External_Tag;

   -----------------------
   -- Get_Expanded_Name --
   -----------------------

   function Get_Expanded_Name (T : Tag) return S.Address is
   begin
      return To_Address (T.TSD.Expanded_Name);
   end Get_Expanded_Name;

   ----------------------
   -- Get_External_Tag --
   ----------------------

   function Get_External_Tag (T : Tag) return S.Address is
   begin
      return To_Address (T.TSD.External_Tag);
   end Get_External_Tag;

   ---------------------------
   -- Get_Inheritance_Depth --
   ---------------------------

   function Get_Inheritance_Depth (T : Tag) return Natural is
   begin
      return T.TSD.Idepth;
   end Get_Inheritance_Depth;

   -------------------------
   -- Get_Prim_Op_Address --
   -------------------------

   function Get_Prim_Op_Address
     (T        : Tag;
      Position : Positive)
      return     S.Address
   is
   begin
      return T.Prims_Ptr (Position);
   end Get_Prim_Op_Address;

   -------------------
   -- Get_RC_Offset --
   -------------------

   function Get_RC_Offset (T : Tag) return SSE.Storage_Offset is
   begin
      return T.TSD.RC_Offset;
   end Get_RC_Offset;

   ---------------------------
   -- Get_Remotely_Callable --
   ---------------------------

   function Get_Remotely_Callable (T : Tag) return Boolean is
   begin
      return T.TSD.Remotely_Callable = True;
   end Get_Remotely_Callable;

   -------------
   -- Get_TSD --
   -------------

   function Get_TSD  (T : Tag) return S.Address is
   begin
      return To_Address (T.TSD);
   end Get_TSD;

   ----------------
   -- Inherit_DT --
   ----------------

   procedure Inherit_DT
    (Old_T       : Tag;
     New_T       : Tag;
     Entry_Count : Natural)
   is
   begin
      if Old_T /= null then
         New_T.Prims_Ptr (1 .. Entry_Count) :=
           Old_T.Prims_Ptr (1 .. Entry_Count);
      end if;
   end Inherit_DT;

   -----------------
   -- Inherit_TSD --
   -----------------

   procedure Inherit_TSD (Old_TSD : S.Address; New_Tag : Tag) is
      TSD     : constant Type_Specific_Data_Ptr :=
                  To_Type_Specific_Data_Ptr (Old_TSD);
      New_TSD : Type_Specific_Data renames New_Tag.TSD.all;

   begin
      if TSD /= null then
         New_TSD.Idepth := TSD.Idepth + 1;
         New_TSD.Ancestor_Tags (1 .. New_TSD.Idepth)
                            := TSD.Ancestor_Tags (0 .. TSD.Idepth);
      else
         New_TSD.Idepth := 0;
      end if;

      New_TSD.Ancestor_Tags (0) := New_Tag;
   end Inherit_TSD;

   ------------------
   -- Internal_Tag --
   ------------------

   function Internal_Tag (External : String) return Tag is
      Ext_Copy : aliased String (External'First .. External'Last + 1);
      Res      : Tag;

   begin
      --  Make a copy of the string representing the external tag with
      --  a null at the end

      Ext_Copy (External'Range) := External;
      Ext_Copy (Ext_Copy'Last) := ASCII.NUL;
      Res := External_Tag_HTable.Get (Ext_Copy'Address);

      if Res = null then
         declare
            Msg1 : constant String := "unknown tagged type: ";
            Msg2 : String (1 .. Msg1'Length + External'Length);

         begin
            Msg2 (1 .. Msg1'Length) := Msg1;
            Msg2 (Msg1'Length + 1 .. Msg1'Length + External'Length) :=
              External;
            Ada.Exceptions.Raise_Exception (Tag_Error'Identity, Msg2);
         end;
      end if;

      return Res;
   end Internal_Tag;

   ------------
   -- Length --
   ------------

   function Length (Str : Cstring_Ptr) return Natural is
      Len : Integer := 1;

   begin
      while Str (Len) /= ASCII.Nul loop
         Len := Len + 1;
      end loop;

      return Len - 1;
   end Length;

   -----------------
   -- Parent_Size --
   -----------------

   type Acc_Size is access function (A : S.Address) return Long_Long_Integer;
   function To_Acc_Size is new Unchecked_Conversion (S.Address, Acc_Size);
   --  The profile of the implicitly defined _size primitive

   function Parent_Size
     (Obj : S.Address;
      T   : Tag)
      return SSE.Storage_Count is

      Parent_Tag : constant Tag := T.TSD.Ancestor_Tags (1);
      --  The tag of the parent type through the dispatch table

      F : constant Acc_Size := To_Acc_Size (Parent_Tag.Prims_Ptr (1));
      --  Access to the _size primitive of the parent. We assume that
      --  it is always in the first slot of the distatch table

   begin
      --  Here we compute the size of the _parent field of the object

      return SSE.Storage_Count (F.all (Obj));
   end Parent_Size;

   ----------------
   -- Parent_Tag --
   ----------------

   function Parent_Tag (T : Tag) return Tag is
   begin
      return T.TSD.Ancestor_Tags (1);
   end Parent_Tag;

   ------------------
   -- Register_Tag --
   ------------------

   procedure Register_Tag (T : Tag) is
   begin
      External_Tag_HTable.Set (T);
   end Register_Tag;

   -----------------------
   -- Set_Expanded_Name --
   -----------------------

   procedure Set_Expanded_Name (T : Tag; Value : S.Address) is
   begin
      T.TSD.Expanded_Name := To_Cstring_Ptr (Value);
   end Set_Expanded_Name;

   ----------------------
   -- Set_External_Tag --
   ----------------------

   procedure Set_External_Tag (T : Tag; Value : S.Address) is
   begin
      T.TSD.External_Tag := To_Cstring_Ptr (Value);
   end Set_External_Tag;

   ---------------------------
   -- Set_Inheritance_Depth --
   ---------------------------

   procedure Set_Inheritance_Depth
     (T     : Tag;
      Value : Natural)
   is
   begin
      T.TSD.Idepth := Value;
   end Set_Inheritance_Depth;

   -------------------------
   -- Set_Prim_Op_Address --
   -------------------------

   procedure Set_Prim_Op_Address
     (T        : Tag;
      Position : Positive;
      Value    : S.Address)
   is
   begin
      T.Prims_Ptr (Position) := Value;
   end Set_Prim_Op_Address;

   -------------------
   -- Set_RC_Offset --
   -------------------

   procedure Set_RC_Offset (T : Tag; Value : SSE.Storage_Offset) is
   begin
      T.TSD.RC_Offset := Value;
   end Set_RC_Offset;

   ---------------------------
   -- Set_Remotely_Callable --
   ---------------------------

   procedure Set_Remotely_Callable (T : Tag; Value : Boolean) is
   begin
      if Value then
         T.TSD.Remotely_Callable := True;
      else
         T.TSD.Remotely_Callable := False;
      end if;
   end Set_Remotely_Callable;

   -------------
   -- Set_TSD --
   -------------

   procedure Set_TSD (T : Tag; Value : S.Address) is
   begin
      T.TSD := To_Type_Specific_Data_Ptr (Value);
   end Set_TSD;

end Ada.Tags;
