------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                             A D A . T A G S                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
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

pragma Elaborate_All (System.HTable);

package body Ada.Tags is

--  Structure of the GNAT Dispatch Table

--           +-----------------------+
--           |     Offset_To_Top     |
--           +-----------------------+
--           | Typeinfo_Ptr/TSD_Ptr  |----> Type Specific Data
--  Tag ---> +-----------------------+      +-------------------+
--           |        table of       |      | inheritance depth |
--           :     primitive ops     :      +-------------------+
--           |        pointers       |      |   expanded name   |
--           +-----------------------+      +-------------------+
--                                          |   external tag    |
--                                          +-------------------+
--                                          |   Hash table link |
--                                          +-------------------+
--                                          | Remotely Callable |
--                                          +-------------------+
--                                          | Rec Ctrler offset |
--                                          +-------------------+
--                                          | table of          |
--                                          :   ancestor        :
--                                          |      tags         |
--                                          +-------------------+

   subtype Cstring is String (Positive);
   type Cstring_Ptr is access all Cstring;

   type Tag_Table is array (Natural range <>) of Tag;
   pragma Suppress_Initialization (Tag_Table);
   pragma Suppress (Index_Check, On => Tag_Table);
   --  We suppress index checks because the declared size in the record below
   --  is a dummy size of one (see below).

   type Wide_Boolean is new Boolean;
   --  This name should probably be changed sometime ??? and indeed probably
   --  this field could simply be of type Standard.Boolean.

   type Type_Specific_Data is record
      Idepth             : Natural;
      Expanded_Name      : Cstring_Ptr;
      External_Tag       : Cstring_Ptr;
      HT_Link            : Tag;
      Remotely_Callable  : Wide_Boolean;
      RC_Offset          : SSE.Storage_Offset;
      Ancestor_Tags      : Tag_Table (0 .. 1);
   end record;
   --  The size of the Ancestor_Tags array actually depends on the tagged type
   --  to which it applies. We are using the same mechanism as for the
   --  Prims_Ptr array in the Dispatch_Table record. See comments below for
   --  more details.

   type Dispatch_Table is record
      --  Offset_To_Top : Integer := 0;
      --  Typeinfo_Ptr  : System.Address; -- Currently TSD is also here???
      Prims_Ptr    : Address_Array (Positive);
   end record;

   --  Note on the commented out fields of the Dispatch_Table
   --  ------------------------------------------------------
   --  According to the C++ ABI the components Offset_To_Top and Typeinfo_Ptr
   --  are stored just "before" the dispatch table (that is, the Prims_Ptr
   --  table), and they are referenced with negative offsets referring to the
   --  base of the dispatch table. The _Tag (or the VTable_Ptr in C++ termi-
   --  nology) must point to the base of the virtual table, just after these
   --  components, to point to the Prims_Ptr table. For this purpose the
   --  expander generates a Prims_Ptr table that has enough space for these
   --  additional components, and generates code that displaces the _Tag to
   --  point after these components.
   --  -----------------------------------------------------------------------

   --  The size of the Prims_Ptr array actually depends on the tagged type to
   --  which it applies. For each tagged type, the expander computes the
   --  actual array size, allocates the Dispatch_Table record accordingly, and
   --  generates code that displaces the base of the record after the
   --  Typeinfo_Ptr component. For this reason the first two components have
   --  been commented in the previous declaration. The access to these
   --  components is done by means of local functions.
   --
   --  To avoid the use of discriminants to define the actual size of the
   --  dispatch table, we used to declare the tag as a pointer to a record
   --  that contains an arbitrary array of addresses, using Positive as its
   --  index. This ensures that there are never range checks when accessing
   --  the dispatch table, but it prevents GDB from displaying tagged types
   --  properly. A better approach is to declare this record type as holding a
   --  small number of addresses, and to explicitly suppress checks on it.
   --
   --  Note that in both cases, this type is never allocated, and serves only
   --  to declare the corresponding access type.

   ---------------------------------------------
   -- Unchecked Conversions for String Fields --
   ---------------------------------------------

   function To_Cstring_Ptr is
     new Unchecked_Conversion (System.Address, Cstring_Ptr);

   function To_Address is
     new Unchecked_Conversion (Cstring_Ptr, System.Address);

   -----------------------------------------------------------
   -- Unchecked Conversions for the component offset_to_top --
   -----------------------------------------------------------

   type Int_Ptr is access Integer;

   function To_Int_Ptr is
      new Unchecked_Conversion (System.Address, Int_Ptr);

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Length (Str : Cstring_Ptr) return Natural;
   --  Length of string represented by the given pointer (treating the string
   --  as a C-style string, which is Nul terminated).

   function Offset_To_Top (T : Tag) return Integer;
   --  Returns the current value of the offset_to_top component available in
   --  the prologue of the dispatch table.

   function Typeinfo_Ptr (T : Tag) return System.Address;
   --  Returns the current value of the typeinfo_ptr component available in
   --  the prologue of the dispatch table.

   pragma Unreferenced (Offset_To_Top);
   pragma Unreferenced (Typeinfo_Ptr);
   --  These functions will be used for full compatibility with the C++ ABI

   -------------------------
   -- External_Tag_HTable --
   -------------------------

   type HTable_Headers is range 1 .. 64;

   --  The following internal package defines the routines used for the
   --  instantiation of a new System.HTable.Static_HTable (see below). See
   --  spec in g-htable.ads for details of usage.

   package HTable_Subprograms is
      procedure Set_HT_Link (T : Tag; Next : Tag);
      function  Get_HT_Link (T : Tag) return Tag;
      function Hash (F : System.Address) return HTable_Headers;
      function Equal (A, B : System.Address) return Boolean;
   end HTable_Subprograms;

   package External_Tag_HTable is new System.HTable.Static_HTable (
     Header_Num => HTable_Headers,
     Element    => Dispatch_Table,
     Elmt_Ptr   => Tag,
     Null_Ptr   => null,
     Set_Next   => HTable_Subprograms.Set_HT_Link,
     Next       => HTable_Subprograms.Get_HT_Link,
     Key        => System.Address,
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

      function Equal (A, B : System.Address) return Boolean is
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
         return TSD (T).HT_Link;
      end Get_HT_Link;

      ----------
      -- Hash --
      ----------

      function Hash (F : System.Address) return HTable_Headers is
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
         TSD (T).HT_Link := Next;
      end Set_HT_Link;

   end HTable_Subprograms;

   -------------------
   -- CW_Membership --
   -------------------

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
      Pos : constant Integer := TSD (Obj_Tag).Idepth - TSD (Typ_Tag).Idepth;
   begin
      return Pos >= 0 and then TSD (Obj_Tag).Ancestor_Tags (Pos) = Typ_Tag;
   end CW_Membership;

   -------------------
   -- Expanded_Name --
   -------------------

   function Expanded_Name (T : Tag) return String is
      Result : constant Cstring_Ptr := TSD (T).Expanded_Name;
   begin
      return Result (1 .. Length (Result));
   end Expanded_Name;

   ------------------
   -- External_Tag --
   ------------------

   function External_Tag (T : Tag) return String is
      Result : constant Cstring_Ptr := TSD (T).External_Tag;
   begin
      return Result (1 .. Length (Result));
   end External_Tag;

   ----------------------
   -- Get_External_Tag --
   ----------------------

   function Get_External_Tag (T : Tag) return System.Address is
   begin
      return To_Address (TSD (T).External_Tag);
   end Get_External_Tag;

   -------------------------
   -- Get_Prim_Op_Address --
   -------------------------

   function Get_Prim_Op_Address
     (T        : Tag;
      Position : Positive) return System.Address
   is
   begin
      return T.Prims_Ptr (Position);
   end Get_Prim_Op_Address;

   -------------------
   -- Get_RC_Offset --
   -------------------

   function Get_RC_Offset (T : Tag) return SSE.Storage_Offset is
   begin
      return TSD (T).RC_Offset;
   end Get_RC_Offset;

   ---------------------------
   -- Get_Remotely_Callable --
   ---------------------------

   function Get_Remotely_Callable (T : Tag) return Boolean is
   begin
      return TSD (T).Remotely_Callable = True;
   end Get_Remotely_Callable;

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

   procedure Inherit_TSD (Old_Tag : Tag; New_Tag : Tag) is
      New_TSD_Ptr : constant Type_Specific_Data_Ptr := TSD (New_Tag);
      Old_TSD_Ptr : Type_Specific_Data_Ptr;

   begin
      if Old_Tag /= null then
         Old_TSD_Ptr        := TSD (Old_Tag);
         New_TSD_Ptr.Idepth := Old_TSD_Ptr.Idepth + 1;
         New_TSD_Ptr.Ancestor_Tags (1 .. New_TSD_Ptr.Idepth) :=
           Old_TSD_Ptr.Ancestor_Tags (0 .. Old_TSD_Ptr.Idepth);
      else
         New_TSD_Ptr.Idepth := 0;
      end if;

      New_TSD_Ptr.Ancestor_Tags (0) := New_Tag;
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

   type Acc_Size
     is access function (A : System.Address) return Long_Long_Integer;

   function To_Acc_Size is new Unchecked_Conversion (System.Address, Acc_Size);
   --  The profile of the implicitly defined _size primitive

   function Parent_Size
     (Obj : System.Address;
      T   : Tag) return SSE.Storage_Count
   is
      Parent_Tag : constant Tag := TSD (T).Ancestor_Tags (1);
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
      return TSD (T).Ancestor_Tags (1);
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

   procedure Set_Expanded_Name (T : Tag; Value : System.Address) is
   begin
      TSD (T).Expanded_Name := To_Cstring_Ptr (Value);
   end Set_Expanded_Name;

   ----------------------
   -- Set_External_Tag --
   ----------------------

   procedure Set_External_Tag (T : Tag; Value : System.Address) is
   begin
      TSD (T).External_Tag := To_Cstring_Ptr (Value);
   end Set_External_Tag;

   -------------------------
   -- Set_Prim_Op_Address --
   -------------------------

   procedure Set_Prim_Op_Address
     (T        : Tag;
      Position : Positive;
      Value    : System.Address)
   is
   begin
      T.Prims_Ptr (Position) := Value;
   end Set_Prim_Op_Address;

   -------------------
   -- Set_RC_Offset --
   -------------------

   procedure Set_RC_Offset (T : Tag; Value : SSE.Storage_Offset) is
   begin
      TSD (T).RC_Offset := Value;
   end Set_RC_Offset;

   ---------------------------
   -- Set_Remotely_Callable --
   ---------------------------

   procedure Set_Remotely_Callable (T : Tag; Value : Boolean) is
   begin
      if Value then
         TSD (T).Remotely_Callable := True;
      else
         TSD (T).Remotely_Callable := False;
      end if;
   end Set_Remotely_Callable;

   -------------
   -- Set_TSD --
   -------------

   procedure Set_TSD (T : Tag; Value : System.Address) is
      use type System.Storage_Elements.Storage_Offset;
      TSD_Ptr : constant Addr_Ptr :=
                  To_Addr_Ptr (To_Address (T) - DT_Typeinfo_Ptr_Size);
   begin
      TSD_Ptr.all := Value;
   end Set_TSD;

   -------------------
   -- Offset_To_Top --
   -------------------

   function Offset_To_Top (T : Tag) return Integer is
      use type System.Storage_Elements.Storage_Offset;
      TSD_Ptr : constant Int_Ptr :=
                  To_Int_Ptr (To_Address (T) - DT_Prologue_Size);
   begin
      return TSD_Ptr.all;
   end Offset_To_Top;

   ------------------
   -- Typeinfo_Ptr --
   ------------------

   function Typeinfo_Ptr (T : Tag) return System.Address is
      use type System.Storage_Elements.Storage_Offset;
      TSD_Ptr : constant Addr_Ptr :=
                  To_Addr_Ptr (To_Address (T) - DT_Typeinfo_Ptr_Size);
   begin
      return TSD_Ptr.all;
   end Typeinfo_Ptr;

   ---------
   -- TSD --
   ---------

   function TSD (T : Tag) return Type_Specific_Data_Ptr is
      use type System.Storage_Elements.Storage_Offset;
      TSD_Ptr : constant Addr_Ptr :=
                  To_Addr_Ptr (To_Address (T) - DT_Typeinfo_Ptr_Size);
   begin
      return To_Type_Specific_Data_Ptr (TSD_Ptr.all);
   end TSD;

end Ada.Tags;
