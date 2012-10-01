------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                             A D A . T A G S                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2012, Free Software Foundation, Inc.         --
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

with Ada.Exceptions;
with Ada.Unchecked_Conversion;
with System.HTable;
with System.Storage_Elements; use System.Storage_Elements;
with System.WCh_Con;          use System.WCh_Con;
with System.WCh_StW;          use System.WCh_StW;

pragma Elaborate_All (System.HTable);

package body Ada.Tags is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function CW_Membership (Obj_Tag : Tag; Typ_Tag : Tag) return Boolean;
   --  Given the tag of an object and the tag associated to a type, return
   --  true if Obj is in Typ'Class.

   function Get_External_Tag (T : Tag) return System.Address;
   --  Returns address of a null terminated string containing the external name

   function Is_Primary_DT (T : Tag) return Boolean;
   --  Given a tag returns True if it has the signature of a primary dispatch
   --  table.  This is Inline_Always since it is called from other Inline_
   --  Always subprograms where we want no out of line code to be generated.

   function Length (Str : Cstring_Ptr) return Natural;
   --  Length of string represented by the given pointer (treating the string
   --  as a C-style string, which is Nul terminated).

   function OSD (T : Tag) return Object_Specific_Data_Ptr;
   --  Ada 2005 (AI-251): Given a pointer T to a secondary dispatch table,
   --  retrieve the address of the record containing the Object Specific
   --  Data table.

   function SSD (T : Tag) return Select_Specific_Data_Ptr;
   --  Ada 2005 (AI-251): Given a pointer T to a dispatch Table, retrieves the
   --  address of the record containing the Select Specific Data in T's TSD.

   pragma Inline_Always (CW_Membership);
   pragma Inline_Always (Get_External_Tag);
   pragma Inline_Always (Is_Primary_DT);
   pragma Inline_Always (OSD);
   pragma Inline_Always (SSD);

   --  Unchecked conversions

   function To_Address is
     new Unchecked_Conversion (Cstring_Ptr, System.Address);

   function To_Cstring_Ptr is
     new Unchecked_Conversion (System.Address, Cstring_Ptr);

   --  Disable warnings on possible aliasing problem

   function To_Tag is
     new Unchecked_Conversion (Integer_Address, Tag);

   function To_Addr_Ptr is
      new Ada.Unchecked_Conversion (System.Address, Addr_Ptr);

   function To_Address is
     new Ada.Unchecked_Conversion (Tag, System.Address);

   function To_Dispatch_Table_Ptr is
      new Ada.Unchecked_Conversion (Tag, Dispatch_Table_Ptr);

   function To_Dispatch_Table_Ptr is
      new Ada.Unchecked_Conversion (System.Address, Dispatch_Table_Ptr);

   function To_Object_Specific_Data_Ptr is
     new Ada.Unchecked_Conversion (System.Address, Object_Specific_Data_Ptr);

   function To_Tag_Ptr is
     new Ada.Unchecked_Conversion (System.Address, Tag_Ptr);

   function To_Type_Specific_Data_Ptr is
     new Ada.Unchecked_Conversion (System.Address, Type_Specific_Data_Ptr);

   -------------------------------
   -- Inline_Always Subprograms --
   -------------------------------

   --  Inline_always subprograms must be placed before their first call to
   --  avoid defeating the frontend inlining mechanism and thus ensure the
   --  generation of their correct debug info.

   -------------------
   -- CW_Membership --
   -------------------

   --  Canonical implementation of Classwide Membership corresponding to:

   --     Obj in Typ'Class

   --  Each dispatch table contains a reference to a table of ancestors (stored
   --  in the first part of the Tags_Table) and a count of the level of
   --  inheritance "Idepth".

   --  Obj is in Typ'Class if Typ'Tag is in the table of ancestors that are
   --  contained in the dispatch table referenced by Obj'Tag . Knowing the
   --  level of inheritance of both types, this can be computed in constant
   --  time by the formula:

   --   TSD (Obj'tag).Tags_Table (TSD (Obj'tag).Idepth - TSD (Typ'tag).Idepth)
   --     = Typ'tag

   function CW_Membership (Obj_Tag : Tag; Typ_Tag : Tag) return Boolean is
      Obj_TSD_Ptr : constant Addr_Ptr :=
        To_Addr_Ptr (To_Address (Obj_Tag) - DT_Typeinfo_Ptr_Size);
      Typ_TSD_Ptr : constant Addr_Ptr :=
        To_Addr_Ptr (To_Address (Typ_Tag) - DT_Typeinfo_Ptr_Size);
      Obj_TSD     : constant Type_Specific_Data_Ptr :=
        To_Type_Specific_Data_Ptr (Obj_TSD_Ptr.all);
      Typ_TSD     : constant Type_Specific_Data_Ptr :=
        To_Type_Specific_Data_Ptr (Typ_TSD_Ptr.all);
      Pos         : constant Integer := Obj_TSD.Idepth - Typ_TSD.Idepth;
   begin
      return Pos >= 0 and then Obj_TSD.Tags_Table (Pos) = Typ_Tag;
   end CW_Membership;

   ----------------------
   -- Get_External_Tag --
   ----------------------

   function Get_External_Tag (T : Tag) return System.Address is
      TSD_Ptr : constant Addr_Ptr :=
        To_Addr_Ptr (To_Address (T) - DT_Typeinfo_Ptr_Size);
      TSD     : constant Type_Specific_Data_Ptr :=
        To_Type_Specific_Data_Ptr (TSD_Ptr.all);
   begin
      return To_Address (TSD.External_Tag);
   end Get_External_Tag;

   -------------------
   -- Is_Primary_DT --
   -------------------

   function Is_Primary_DT (T : Tag) return Boolean is
   begin
      return DT (T).Signature = Primary_DT;
   end Is_Primary_DT;

   ---------
   -- OSD --
   ---------

   function OSD (T : Tag) return Object_Specific_Data_Ptr is
      OSD_Ptr : constant Addr_Ptr :=
        To_Addr_Ptr (To_Address (T) - DT_Typeinfo_Ptr_Size);
   begin
      return To_Object_Specific_Data_Ptr (OSD_Ptr.all);
   end OSD;

   ---------
   -- SSD --
   ---------

   function SSD (T : Tag) return Select_Specific_Data_Ptr is
      TSD_Ptr : constant Addr_Ptr :=
        To_Addr_Ptr (To_Address (T) - DT_Typeinfo_Ptr_Size);
      TSD     : constant Type_Specific_Data_Ptr :=
        To_Type_Specific_Data_Ptr (TSD_Ptr.all);
   begin
      return TSD.SSD;
   end SSD;

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
         TSD_Ptr : constant Addr_Ptr :=
           To_Addr_Ptr (To_Address (T) - DT_Typeinfo_Ptr_Size);
         TSD     : constant Type_Specific_Data_Ptr :=
           To_Type_Specific_Data_Ptr (TSD_Ptr.all);
      begin
         return TSD.HT_Link.all;
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
         TSD_Ptr : constant Addr_Ptr :=
           To_Addr_Ptr (To_Address (T) - DT_Typeinfo_Ptr_Size);
         TSD     : constant Type_Specific_Data_Ptr :=
           To_Type_Specific_Data_Ptr (TSD_Ptr.all);
      begin
         TSD.HT_Link.all := Next;
      end Set_HT_Link;

   end HTable_Subprograms;

   ------------------
   -- Base_Address --
   ------------------

   function Base_Address (This : System.Address) return System.Address is
   begin
      return This - Offset_To_Top (This);
   end Base_Address;

   ---------------
   -- Check_TSD --
   ---------------

   procedure Check_TSD (TSD : Type_Specific_Data_Ptr) is
      T : Tag;

      E_Tag_Len : constant Integer := Length (TSD.External_Tag);
      E_Tag     : String (1 .. E_Tag_Len);
      for E_Tag'Address use TSD.External_Tag.all'Address;
      pragma Import (Ada, E_Tag);

      Dup_Ext_Tag : constant String := "duplicated external tag """;

   begin
      --  Verify that the external tag of this TSD is not registered in the
      --  runtime hash table.

      T := External_Tag_HTable.Get (To_Address (TSD.External_Tag));

      if T /= null then

         --  Avoid concatenation, as it is not allowed in no run time mode

         declare
            Msg : String (1 .. Dup_Ext_Tag'Length + E_Tag_Len + 1);
         begin
            Msg (1 .. Dup_Ext_Tag'Length) := Dup_Ext_Tag;
            Msg (Dup_Ext_Tag'Length + 1 .. Dup_Ext_Tag'Length + E_Tag_Len) :=
              E_Tag;
            Msg (Msg'Last) := '"';
            raise Program_Error with Msg;
         end;
      end if;
   end Check_TSD;

   --------------------
   -- Descendant_Tag --
   --------------------

   function Descendant_Tag (External : String; Ancestor : Tag) return Tag is
      Int_Tag : constant Tag := Internal_Tag (External);

   begin
      if not Is_Descendant_At_Same_Level (Int_Tag, Ancestor) then
         raise Tag_Error;
      end if;

      return Int_Tag;
   end Descendant_Tag;

   --------------
   -- Displace --
   --------------

   function Displace
     (This : System.Address;
      T    : Tag) return System.Address
   is
      Iface_Table : Interface_Data_Ptr;
      Obj_Base    : System.Address;
      Obj_DT      : Dispatch_Table_Ptr;
      Obj_DT_Tag  : Tag;

   begin
      if System."=" (This, System.Null_Address) then
         return System.Null_Address;
      end if;

      Obj_Base    := Base_Address (This);
      Obj_DT_Tag  := To_Tag_Ptr (Obj_Base).all;
      Obj_DT      := DT (To_Tag_Ptr (Obj_Base).all);
      Iface_Table := To_Type_Specific_Data_Ptr (Obj_DT.TSD).Interfaces_Table;

      if Iface_Table /= null then
         for Id in 1 .. Iface_Table.Nb_Ifaces loop
            if Iface_Table.Ifaces_Table (Id).Iface_Tag = T then

               --  Case of Static value of Offset_To_Top

               if Iface_Table.Ifaces_Table (Id).Static_Offset_To_Top then
                  Obj_Base := Obj_Base +
                    Iface_Table.Ifaces_Table (Id).Offset_To_Top_Value;

               --  Otherwise call the function generated by the expander to
               --  provide the value.

               else
                  Obj_Base := Obj_Base +
                    Iface_Table.Ifaces_Table (Id).Offset_To_Top_Func.all
                      (Obj_Base);
               end if;

               return Obj_Base;
            end if;
         end loop;
      end if;

      --  Check if T is an immediate ancestor. This is required to handle
      --  conversion of class-wide interfaces to tagged types.

      if CW_Membership (Obj_DT_Tag, T) then
         return Obj_Base;
      end if;

      --  If the object does not implement the interface we must raise CE

      raise Constraint_Error with "invalid interface conversion";
   end Displace;

   --------
   -- DT --
   --------

   function DT (T : Tag) return Dispatch_Table_Ptr is
      Offset : constant SSE.Storage_Offset :=
        To_Dispatch_Table_Ptr (T).Prims_Ptr'Position;
   begin
      return To_Dispatch_Table_Ptr (To_Address (T) - Offset);
   end DT;

   -------------------
   -- IW_Membership --
   -------------------

   --  Canonical implementation of Classwide Membership corresponding to:

   --     Obj in Iface'Class

   --  Each dispatch table contains a table with the tags of all the
   --  implemented interfaces.

   --  Obj is in Iface'Class if Iface'Tag is found in the table of interfaces
   --  that are contained in the dispatch table referenced by Obj'Tag.

   function IW_Membership (This : System.Address; T : Tag) return Boolean is
      Iface_Table : Interface_Data_Ptr;
      Obj_Base    : System.Address;
      Obj_DT      : Dispatch_Table_Ptr;
      Obj_TSD     : Type_Specific_Data_Ptr;

   begin
      Obj_Base    := Base_Address (This);
      Obj_DT      := DT (To_Tag_Ptr (Obj_Base).all);
      Obj_TSD     := To_Type_Specific_Data_Ptr (Obj_DT.TSD);
      Iface_Table := Obj_TSD.Interfaces_Table;

      if Iface_Table /= null then
         for Id in 1 .. Iface_Table.Nb_Ifaces loop
            if Iface_Table.Ifaces_Table (Id).Iface_Tag = T then
               return True;
            end if;
         end loop;
      end if;

      --  Look for the tag in the ancestor tags table. This is required for:
      --     Iface_CW in Typ'Class

      for Id in 0 .. Obj_TSD.Idepth loop
         if Obj_TSD.Tags_Table (Id) = T then
            return True;
         end if;
      end loop;

      return False;
   end IW_Membership;

   -------------------
   -- Expanded_Name --
   -------------------

   function Expanded_Name (T : Tag) return String is
      Result  : Cstring_Ptr;
      TSD_Ptr : Addr_Ptr;
      TSD     : Type_Specific_Data_Ptr;

   begin
      if T = No_Tag then
         raise Tag_Error;
      end if;

      TSD_Ptr := To_Addr_Ptr (To_Address (T) - DT_Typeinfo_Ptr_Size);
      TSD     := To_Type_Specific_Data_Ptr (TSD_Ptr.all);
      Result  := TSD.Expanded_Name;
      return Result (1 .. Length (Result));
   end Expanded_Name;

   ------------------
   -- External_Tag --
   ------------------

   function External_Tag (T : Tag) return String is
      Result  : Cstring_Ptr;
      TSD_Ptr : Addr_Ptr;
      TSD     : Type_Specific_Data_Ptr;

   begin
      if T = No_Tag then
         raise Tag_Error;
      end if;

      TSD_Ptr := To_Addr_Ptr (To_Address (T) - DT_Typeinfo_Ptr_Size);
      TSD     := To_Type_Specific_Data_Ptr (TSD_Ptr.all);
      Result  := TSD.External_Tag;
      return Result (1 .. Length (Result));
   end External_Tag;

   ---------------------
   -- Get_Entry_Index --
   ---------------------

   function Get_Entry_Index (T : Tag; Position : Positive) return Positive is
   begin
      return SSD (T).SSD_Table (Position).Index;
   end Get_Entry_Index;

   ----------------------
   -- Get_Prim_Op_Kind --
   ----------------------

   function Get_Prim_Op_Kind
     (T        : Tag;
      Position : Positive) return Prim_Op_Kind
   is
   begin
      return SSD (T).SSD_Table (Position).Kind;
   end Get_Prim_Op_Kind;

   ----------------------
   -- Get_Offset_Index --
   ----------------------

   function Get_Offset_Index
     (T        : Tag;
      Position : Positive) return Positive
   is
   begin
      if Is_Primary_DT (T) then
         return Position;
      else
         return OSD (T).OSD_Table (Position);
      end if;
   end Get_Offset_Index;

   ---------------------
   -- Get_Tagged_Kind --
   ---------------------

   function Get_Tagged_Kind (T : Tag) return Tagged_Kind is
   begin
      return DT (T).Tag_Kind;
   end Get_Tagged_Kind;

   -----------------------------
   -- Interface_Ancestor_Tags --
   -----------------------------

   function Interface_Ancestor_Tags (T : Tag) return Tag_Array is
      TSD_Ptr     : constant Addr_Ptr :=
        To_Addr_Ptr (To_Address (T) - DT_Typeinfo_Ptr_Size);
      TSD         : constant Type_Specific_Data_Ptr :=
        To_Type_Specific_Data_Ptr (TSD_Ptr.all);
      Iface_Table : constant Interface_Data_Ptr := TSD.Interfaces_Table;

   begin
      if Iface_Table = null then
         declare
            Table : Tag_Array (1 .. 0);
         begin
            return Table;
         end;
      else
         declare
            Table : Tag_Array (1 .. Iface_Table.Nb_Ifaces);
         begin
            for J in 1 .. Iface_Table.Nb_Ifaces loop
               Table (J) := Iface_Table.Ifaces_Table (J).Iface_Tag;
            end loop;

            return Table;
         end;
      end if;
   end Interface_Ancestor_Tags;

   ------------------
   -- Internal_Tag --
   ------------------

   --  Internal tags have the following format:
   --    "Internal tag at 16#ADDRESS#: <full-name-of-tagged-type>"

   Internal_Tag_Header : constant String    := "Internal tag at ";
   Header_Separator    : constant Character := '#';

   function Internal_Tag (External : String) return Tag is
      Ext_Copy : aliased String (External'First .. External'Last + 1);
      Res      : Tag := null;

   begin
      --  Handle locally defined tagged types

      if External'Length > Internal_Tag_Header'Length
        and then
         External (External'First ..
                     External'First + Internal_Tag_Header'Length - 1)
           = Internal_Tag_Header
      then
         declare
            Addr_First : constant Natural :=
              External'First + Internal_Tag_Header'Length;
            Addr_Last  : Natural;
            Addr       : Integer_Address;

         begin
            --  Search the second separator (#) to identify the address

            Addr_Last := Addr_First;

            for J in 1 .. 2 loop
               while Addr_Last <= External'Last
                 and then External (Addr_Last) /= Header_Separator
               loop
                  Addr_Last := Addr_Last + 1;
               end loop;

               --  Skip the first separator

               if J = 1 then
                  Addr_Last := Addr_Last + 1;
               end if;
            end loop;

            if Addr_Last <= External'Last then

               --  Protect the run-time against wrong internal tags. We
               --  cannot use exception handlers here because it would
               --  disable the use of this run-time compiling with
               --  restriction No_Exception_Handler.

               declare
                  C         : Character;
                  Wrong_Tag : Boolean := False;

               begin
                  if External (Addr_First) /= '1'
                    or else External (Addr_First + 1) /= '6'
                    or else External (Addr_First + 2) /= '#'
                  then
                     Wrong_Tag := True;

                  else
                     for J in Addr_First + 3 .. Addr_Last - 1 loop
                        C := External (J);

                        if not (C in '0' .. '9')
                          and then not (C in 'A' .. 'F')
                          and then not (C in 'a' .. 'f')
                        then
                           Wrong_Tag := True;
                           exit;
                        end if;
                     end loop;
                  end if;

                  --  Convert the numeric value into a tag

                  if not Wrong_Tag then
                     Addr := Integer_Address'Value
                               (External (Addr_First .. Addr_Last));

                     --  Internal tags never have value 0

                     if Addr /= 0 then
                        return To_Tag (Addr);
                     end if;
                  end if;
               end;
            end if;
         end;

      --  Handle library-level tagged types

      else
         --  Make NUL-terminated copy of external tag string

         Ext_Copy (External'Range) := External;
         Ext_Copy (Ext_Copy'Last)  := ASCII.NUL;
         Res := External_Tag_HTable.Get (Ext_Copy'Address);
      end if;

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

   ---------------------------------
   -- Is_Descendant_At_Same_Level --
   ---------------------------------

   function Is_Descendant_At_Same_Level
     (Descendant : Tag;
      Ancestor   : Tag) return Boolean
   is
      D_TSD_Ptr : constant Addr_Ptr :=
        To_Addr_Ptr (To_Address (Descendant) - DT_Typeinfo_Ptr_Size);
      A_TSD_Ptr : constant Addr_Ptr :=
        To_Addr_Ptr (To_Address (Ancestor) - DT_Typeinfo_Ptr_Size);
      D_TSD     : constant Type_Specific_Data_Ptr :=
        To_Type_Specific_Data_Ptr (D_TSD_Ptr.all);
      A_TSD     : constant Type_Specific_Data_Ptr :=
        To_Type_Specific_Data_Ptr (A_TSD_Ptr.all);

   begin
      return CW_Membership (Descendant, Ancestor)
        and then D_TSD.Access_Level = A_TSD.Access_Level;
   end Is_Descendant_At_Same_Level;

   ------------
   -- Length --
   ------------

   --  Should this be reimplemented using the strlen GCC builtin???

   function Length (Str : Cstring_Ptr) return Natural is
      Len : Integer;

   begin
      Len := 1;
      while Str (Len) /= ASCII.NUL loop
         Len := Len + 1;
      end loop;

      return Len - 1;
   end Length;

   -------------------
   -- Offset_To_Top --
   -------------------

   function Offset_To_Top
     (This : System.Address) return SSE.Storage_Offset
   is
      Tag_Size : constant SSE.Storage_Count :=
        SSE.Storage_Count (1 * (Standard'Address_Size / System.Storage_Unit));

      type Storage_Offset_Ptr is access SSE.Storage_Offset;
      function To_Storage_Offset_Ptr is
        new Unchecked_Conversion (System.Address, Storage_Offset_Ptr);

      Curr_DT : Dispatch_Table_Ptr;

   begin
      Curr_DT := DT (To_Tag_Ptr (This).all);

      if Curr_DT.Offset_To_Top = SSE.Storage_Offset'Last then
         return To_Storage_Offset_Ptr (This + Tag_Size).all;
      else
         return Curr_DT.Offset_To_Top;
      end if;
   end Offset_To_Top;

   ------------------------
   -- Needs_Finalization --
   ------------------------

   function Needs_Finalization (T : Tag) return Boolean is
      TSD_Ptr : constant Addr_Ptr :=
        To_Addr_Ptr (To_Address (T) - DT_Typeinfo_Ptr_Size);
      TSD     : constant Type_Specific_Data_Ptr :=
        To_Type_Specific_Data_Ptr (TSD_Ptr.all);
   begin
      return TSD.Needs_Finalization;
   end Needs_Finalization;

   -----------------
   -- Parent_Size --
   -----------------

   function Parent_Size
     (Obj : System.Address;
      T   : Tag) return SSE.Storage_Count
   is
      Parent_Slot : constant Positive := 1;
      --  The tag of the parent is always in the first slot of the table of
      --  ancestor tags.

      TSD_Ptr : constant Addr_Ptr :=
        To_Addr_Ptr (To_Address (T) - DT_Typeinfo_Ptr_Size);
      TSD     : constant Type_Specific_Data_Ptr :=
        To_Type_Specific_Data_Ptr (TSD_Ptr.all);
      --  Pointer to the TSD

      Parent_Tag     : constant Tag := TSD.Tags_Table (Parent_Slot);
      Parent_TSD_Ptr : constant Addr_Ptr :=
        To_Addr_Ptr (To_Address (Parent_Tag) - DT_Typeinfo_Ptr_Size);
      Parent_TSD     : constant Type_Specific_Data_Ptr :=
        To_Type_Specific_Data_Ptr (Parent_TSD_Ptr.all);

   begin
      --  Here we compute the size of the _parent field of the object

      return SSE.Storage_Count (Parent_TSD.Size_Func.all (Obj));
   end Parent_Size;

   ----------------
   -- Parent_Tag --
   ----------------

   function Parent_Tag (T : Tag) return Tag is
      TSD_Ptr : Addr_Ptr;
      TSD     : Type_Specific_Data_Ptr;

   begin
      if T = No_Tag then
         raise Tag_Error;
      end if;

      TSD_Ptr := To_Addr_Ptr (To_Address (T) - DT_Typeinfo_Ptr_Size);
      TSD     := To_Type_Specific_Data_Ptr (TSD_Ptr.all);

      --  The Parent_Tag of a root-level tagged type is defined to be No_Tag.
      --  The first entry in the Ancestors_Tags array will be null for such
      --  a type, but it's better to be explicit about returning No_Tag in
      --  this case.

      if TSD.Idepth = 0 then
         return No_Tag;
      else
         return TSD.Tags_Table (1);
      end if;
   end Parent_Tag;

   -------------------------------
   -- Register_Interface_Offset --
   -------------------------------

   procedure Register_Interface_Offset
     (This         : System.Address;
      Interface_T  : Tag;
      Is_Static    : Boolean;
      Offset_Value : SSE.Storage_Offset;
      Offset_Func  : Offset_To_Top_Function_Ptr)
   is
      Prim_DT     : Dispatch_Table_Ptr;
      Iface_Table : Interface_Data_Ptr;

   begin
      --  "This" points to the primary DT and we must save Offset_Value in
      --  the Offset_To_Top field of the corresponding dispatch table.

      Prim_DT     := DT (To_Tag_Ptr (This).all);
      Iface_Table := To_Type_Specific_Data_Ptr (Prim_DT.TSD).Interfaces_Table;

      --  Save Offset_Value in the table of interfaces of the primary DT.
      --  This data will be used by the subprogram "Displace" to give support
      --  to backward abstract interface type conversions.

      --  Register the offset in the table of interfaces

      if Iface_Table /= null then
         for Id in 1 .. Iface_Table.Nb_Ifaces loop
            if Iface_Table.Ifaces_Table (Id).Iface_Tag = Interface_T then
               if Is_Static or else Offset_Value = 0 then
                  Iface_Table.Ifaces_Table (Id).Static_Offset_To_Top := True;
                  Iface_Table.Ifaces_Table (Id).Offset_To_Top_Value :=
                    Offset_Value;
               else
                  Iface_Table.Ifaces_Table (Id).Static_Offset_To_Top := False;
                  Iface_Table.Ifaces_Table (Id).Offset_To_Top_Func :=
                    Offset_Func;
               end if;

               return;
            end if;
         end loop;
      end if;

      --  If we arrive here there is some error in the run-time data structure

      raise Program_Error;
   end Register_Interface_Offset;

   ------------------
   -- Register_Tag --
   ------------------

   procedure Register_Tag (T : Tag) is
   begin
      External_Tag_HTable.Set (T);
   end Register_Tag;

   -------------------
   -- Secondary_Tag --
   -------------------

   function Secondary_Tag (T, Iface : Tag) return Tag is
      Iface_Table : Interface_Data_Ptr;
      Obj_DT      : Dispatch_Table_Ptr;

   begin
      if not Is_Primary_DT (T) then
         raise Program_Error;
      end if;

      Obj_DT      := DT (T);
      Iface_Table := To_Type_Specific_Data_Ptr (Obj_DT.TSD).Interfaces_Table;

      if Iface_Table /= null then
         for Id in 1 .. Iface_Table.Nb_Ifaces loop
            if Iface_Table.Ifaces_Table (Id).Iface_Tag = Iface then
               return Iface_Table.Ifaces_Table (Id).Secondary_DT;
            end if;
         end loop;
      end if;

      --  If the object does not implement the interface we must raise CE

      raise Constraint_Error with "invalid interface conversion";
   end Secondary_Tag;

   ---------------------
   -- Set_Entry_Index --
   ---------------------

   procedure Set_Entry_Index
     (T        : Tag;
      Position : Positive;
      Value    : Positive)
   is
   begin
      SSD (T).SSD_Table (Position).Index := Value;
   end Set_Entry_Index;

   -----------------------
   -- Set_Offset_To_Top --
   -----------------------

   procedure Set_Dynamic_Offset_To_Top
     (This         : System.Address;
      Interface_T  : Tag;
      Offset_Value : SSE.Storage_Offset;
      Offset_Func  : Offset_To_Top_Function_Ptr)
   is
      Sec_Base : System.Address;
      Sec_DT   : Dispatch_Table_Ptr;
   begin
      --  Save the offset to top field in the secondary dispatch table

      if Offset_Value /= 0 then
         Sec_Base := This + Offset_Value;
         Sec_DT := DT (To_Tag_Ptr (Sec_Base).all);
         Sec_DT.Offset_To_Top := SSE.Storage_Offset'Last;
      end if;

      Register_Interface_Offset
        (This, Interface_T, False, Offset_Value, Offset_Func);
   end Set_Dynamic_Offset_To_Top;

   ----------------------
   -- Set_Prim_Op_Kind --
   ----------------------

   procedure Set_Prim_Op_Kind
     (T        : Tag;
      Position : Positive;
      Value    : Prim_Op_Kind)
   is
   begin
      SSD (T).SSD_Table (Position).Kind := Value;
   end Set_Prim_Op_Kind;

   ----------------------
   -- Type_Is_Abstract --
   ----------------------

   function Type_Is_Abstract (T : Tag) return Boolean is
      TSD_Ptr : Addr_Ptr;
      TSD     : Type_Specific_Data_Ptr;

   begin
      if T = No_Tag then
         raise Tag_Error;
      end if;

      TSD_Ptr := To_Addr_Ptr (To_Address (T) - DT_Typeinfo_Ptr_Size);
      TSD     := To_Type_Specific_Data_Ptr (TSD_Ptr.all);
      return TSD.Type_Is_Abstract;
   end Type_Is_Abstract;

   --------------------
   -- Unregister_Tag --
   --------------------

   procedure Unregister_Tag (T : Tag) is
   begin
      External_Tag_HTable.Remove (Get_External_Tag (T));
   end Unregister_Tag;

   ------------------------
   -- Wide_Expanded_Name --
   ------------------------

   WC_Encoding : Character;
   pragma Import (C, WC_Encoding, "__gl_wc_encoding");
   --  Encoding method for source, as exported by binder

   function Wide_Expanded_Name (T : Tag) return Wide_String is
      S : constant String := Expanded_Name (T);
      W : Wide_String (1 .. S'Length);
      L : Natural;
   begin
      String_To_Wide_String
        (S, W, L, Get_WC_Encoding_Method (WC_Encoding));
      return W (1 .. L);
   end Wide_Expanded_Name;

   -----------------------------
   -- Wide_Wide_Expanded_Name --
   -----------------------------

   function Wide_Wide_Expanded_Name (T : Tag) return Wide_Wide_String is
      S : constant String := Expanded_Name (T);
      W : Wide_Wide_String (1 .. S'Length);
      L : Natural;
   begin
      String_To_Wide_Wide_String
        (S, W, L, Get_WC_Encoding_Method (WC_Encoding));
      return W (1 .. L);
   end Wide_Wide_Expanded_Name;

end Ada.Tags;
