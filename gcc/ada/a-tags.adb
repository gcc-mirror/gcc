------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                             A D A . T A G S                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Foundation, Inc.         --
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

with Ada.Exceptions;
with System.HTable;
with System.Storage_Elements; use System.Storage_Elements;

pragma Elaborate_All (System.HTable);

package body Ada.Tags is

--  Structure of the GNAT Primary Dispatch Table

--           +-----------------------+
--           |       Signature       |
--           +-----------------------+
--           |     Offset_To_Top     |
--           +-----------------------+
--           | Typeinfo_Ptr/TSD_Ptr  | ---> Type Specific Data
--  Tag ---> +-----------------------+      +-------------------+
--           |        table of       |      | inheritance depth |
--           :     primitive ops     :      +-------------------+
--           |        pointers       |      |   access level    |
--           +-----------------------+      +-------------------+
--                                          |   expanded name   |
--                                          +-------------------+
--                                          |   external tag    |
--                                          +-------------------+
--                                          |   hash table link |
--                                          +-------------------+
--                                          | remotely callable |
--                                          +-------------------+
--                                          | rec ctrler offset |
--                                          +-------------------+
--                                          |   num prim ops    |
--                                          +-------------------+
--                                          |  num interfaces   |
--                                          +-------------------+
--           Select Specific Data      <--- |     SSD_Ptr       |
--           +-----------------------+      +-------------------+
--           | table of primitive    |      | table of          |
--           :    operation          :      :    ancestor       :
--           |       kinds           |      |       tags        |
--           +-----------------------+      +-------------------+
--           | table of              |      | table of          |
--           :    entry              :      :    interface      :
--           |       indices         |      |       tags        |
--           +-----------------------+      +-------------------+

--  Structure of the GNAT Secondary Dispatch Table

--           +-----------------------+
--           |       Signature       |
--           +-----------------------+
--           |     Offset_To_Top     |
--           +-----------------------+
--           |        OSD_Ptr        |---> Object Specific Data
--  Tag ---> +-----------------------+      +---------------+
--           |        table of       |      | num prim ops  |
--           :      primitive op     :      +---------------+
--           |     thunk pointers    |      | table of      |
--           +-----------------------+      +   primitive   |
--                                          |    op offsets |
--                                          +---------------+

   Offset_To_Signature : constant SSE.Storage_Count :=
                           DT_Typeinfo_Ptr_Size
                             + DT_Offset_To_Top_Size
                             + DT_Signature_Size;

   subtype Cstring is String (Positive);
   type Cstring_Ptr is access all Cstring;

   --  We suppress index checks because the declared size in the record below
   --  is a dummy size of one (see below).

   type Tag_Table is array (Natural range <>) of Tag;
   pragma Suppress_Initialization (Tag_Table);
   pragma Suppress (Index_Check, On => Tag_Table);

   --  Object specific data types

   type Object_Specific_Data_Array is array (Positive range <>) of Positive;

   type Object_Specific_Data (Nb_Prim : Positive) is record
      Num_Prim_Ops : Natural;
      --  Number of primitive operations of the dispatch table. This field is
      --  used by the run-time check routines that are activated when the
      --  run-time is compiled with assertions enabled.

      OSD_Table : Object_Specific_Data_Array (1 .. Nb_Prim);
      --  Table used in secondary DT to reference their counterpart in the
      --  select specific data (in the TSD of the primary DT). This construct
      --  is used in the handling of dispatching triggers in select statements.
      --  Nb_Prim is the number of non-predefined primitive operations.
   end record;

   --  Select specific data types

   type Select_Specific_Data_Element is record
      Index : Positive;
      Kind  : Prim_Op_Kind;
   end record;

   type Select_Specific_Data_Array is
     array (Positive range <>) of Select_Specific_Data_Element;

   type Select_Specific_Data (Nb_Prim : Positive) is record
      SSD_Table : Select_Specific_Data_Array (1 .. Nb_Prim);
      --  NOTE: Nb_Prim is the number of non-predefined primitive operations
   end record;

   --  Type specific data types

   type Type_Specific_Data is record
      Idepth : Natural;
      --  Inheritance Depth Level: Used to implement the membership test
      --  associated with single inheritance of tagged types in constant-time.
      --  In addition it also indicates the size of the first table stored in
      --  the Tags_Table component (see comment below).

      Access_Level : Natural;
      --  Accessibility level required to give support to Ada 2005 nested type
      --  extensions. This feature allows safe nested type extensions by
      --  shifting the accessibility checks to certain operations, rather than
      --  being enforced at the type declaration. In particular, by performing
      --  run-time accessibility checks on class-wide allocators, class-wide
      --  function return, and class-wide stream I/O, the danger of objects
      --  outliving their type declaration can be eliminated (Ada 2005: AI-344)

      Expanded_Name : Cstring_Ptr;
      External_Tag  : Cstring_Ptr;
      HT_Link       : Tag;
      --  Components used to give support to the Ada.Tags subprograms described
      --  in ARM 3.9

      Remotely_Callable : Boolean;
      --  Used to check ARM E.4 (18)

      RC_Offset : SSE.Storage_Offset;
      --  Controller Offset: Used to give support to tagged controlled objects
      --  (see Get_Deep_Controller at s-finimp)

      Num_Prim_Ops : Natural;
      --  Number of primitive operations of the dispatch table. This field is
      --  used for additional run-time checks when the run-time is compiled
      --  with assertions enabled.

      Num_Interfaces : Natural;
      --  Number of abstract interface types implemented by the tagged type.
      --  The value Idepth+Num_Interfaces indicates the end of the second table
      --  stored in the Tags_Table component. It is used to implement the
      --  membership test associated with interfaces (Ada 2005:AI-251).

      SSD_Ptr : System.Address;
      --  Pointer to a table of records used in dispatching selects. This
      --  field has a meaningful value for all tagged types that implement
      --  a limited, protected, synchronized or task interfaces and have
      --  non-predefined primitive operations.

      Tags_Table : Tag_Table (0 .. 1);
      --  The size of the Tags_Table array actually depends on the tagged type
      --  to which it applies. The compiler ensures that has enough space to
      --  store all the entries of the two tables phisically stored there: the
      --  "table of ancestor tags" and the "table of interface tags". For this
      --  purpose we are using the same mechanism as for the Prims_Ptr array in
      --  the Dispatch_Table record. See comments below on Prims_Ptr for
      --  further details.
   end record;

   type Dispatch_Table is record

      --  According to the C++ ABI the components Offset_To_Top and
      --  Typeinfo_Ptr are stored just "before" the dispatch table (that is,
      --  the Prims_Ptr table), and they are referenced with negative offsets
      --  referring to the base of the dispatch table. The _Tag (or the
      --  VTable_Ptr in C++ terminology) must point to the base of the virtual
      --  table, just after these components, to point to the Prims_Ptr table.
      --  For this purpose the expander generates a Prims_Ptr table that has
      --  enough space for these additional components, and generates code that
      --  displaces the _Tag to point after these components.

      --  Offset_To_Top : Natural;
      --  Typeinfo_Ptr  : System.Address;

      Prims_Ptr : Address_Array (1 .. 1);
      --  The size of the Prims_Ptr array actually depends on the tagged type
      --  to which it applies. For each tagged type, the expander computes the
      --  actual array size, allocates the Dispatch_Table record accordingly,
      --  and generates code that displaces the base of the record after the
      --  Typeinfo_Ptr component. For this reason the first two components have
      --  been commented in the previous declaration. The access to these
      --  components is done by means of local functions.
      --
      --  To avoid the use of discriminants to define the actual size of the
      --  dispatch table, we used to declare the tag as a pointer to a record
      --  that contains an arbitrary array of addresses, using Positive as its
      --  index. This ensures that there are never range checks when accessing
      --  the dispatch table, but it prevents GDB from displaying tagged types
      --  properly. A better approach is to declare this record type as holding
      --  small number of addresses, and to explicitly suppress checks on it.
      --
      --  Note that in both cases, this type is never allocated, and serves
      --  only to declare the corresponding access type.
   end record;

   --  Run-time check types and subprograms: These subprograms are used only
   --  when the run-time is compiled with assertions enabled.

   type Signature_Type is
      (Must_Be_Primary_DT,
       Must_Be_Secondary_DT,
       Must_Be_Primary_Or_Secondary_DT,
       Must_Be_Interface,
       Must_Be_Primary_Or_Interface);
   --  Type of signature accepted by primitives in this package that are called
   --  during the elaboration of tagged types. This type is used by the routine
   --  Check_Signature that is called only when the run-time is compiled with
   --  assertions enabled.

   ---------------------------------------------
   -- Unchecked Conversions for String Fields --
   ---------------------------------------------

   function To_Address is
     new Unchecked_Conversion (Cstring_Ptr, System.Address);

   function To_Cstring_Ptr is
     new Unchecked_Conversion (System.Address, Cstring_Ptr);

   ------------------------------------------------
   -- Unchecked Conversions for other components --
   ------------------------------------------------

   type Acc_Size
     is access function (A : System.Address) return Long_Long_Integer;

   function To_Acc_Size is new Unchecked_Conversion (System.Address, Acc_Size);
   --  The profile of the implicitly defined _size primitive

   type Storage_Offset_Ptr is access System.Storage_Elements.Storage_Offset;

   function To_Storage_Offset_Ptr is
     new Unchecked_Conversion (System.Address, Storage_Offset_Ptr);

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Check_Index
     (T     : Tag;
      Index : Natural) return Boolean;
   --  Check that Index references a valid entry of the dispatch table of T

   function Check_Signature (T : Tag; Kind : Signature_Type) return Boolean;
   --  Check that the signature of T is valid and corresponds with the subset
   --  specified by the signature Kind.

   function Check_Size
     (Old_T       : Tag;
      New_T       : Tag;
      Entry_Count : Natural) return Boolean;
   --  Verify that Old_T and New_T have at least Entry_Count entries

   function Get_Num_Prim_Ops (T : Tag) return Natural;
   --  Retrieve the number of primitive operations in the dispatch table of T

   function Is_Primary_DT (T : Tag) return Boolean;
   pragma Inline_Always (Is_Primary_DT);
   --  Given a tag returns True if it has the signature of a primary dispatch
   --  table.  This is Inline_Always since it is called from other Inline_
   --  Always subprograms where we want no out of line code to be generated.

   function Length (Str : Cstring_Ptr) return Natural;
   --  Length of string represented by the given pointer (treating the string
   --  as a C-style string, which is Nul terminated).

   function Offset_To_Top
     (T : Tag) return System.Storage_Elements.Storage_Offset;
   --  Returns the current value of the offset_to_top component available in
   --  the prologue of the dispatch table.

   function Typeinfo_Ptr (T : Tag) return System.Address;
   --  Returns the current value of the typeinfo_ptr component available in
   --  the prologue of the dispatch table.

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

   -----------------
   -- Check_Index --
   -----------------

   function Check_Index
     (T     : Tag;
      Index : Natural) return Boolean
   is
      Max_Entries : constant Natural := Get_Num_Prim_Ops (T);

   begin
      return Index /= 0 and then Index <= Max_Entries;
   end Check_Index;

   ---------------------
   -- Check_Signature --
   ---------------------

   function Check_Signature (T : Tag; Kind : Signature_Type) return Boolean is
      Offset_To_Top_Ptr : constant Storage_Offset_Ptr :=
                            To_Storage_Offset_Ptr (To_Address (T)
                              - Offset_To_Signature);

      Signature : constant Signature_Values :=
                    To_Signature_Values (Offset_To_Top_Ptr.all);

      Signature_Id : Signature_Kind;

   begin
      if Signature (1) /= Valid_Signature then
         Signature_Id := Unknown;

      elsif Signature (2) in Primary_DT .. Abstract_Interface then
         Signature_Id := Signature (2);

      else
         Signature_Id := Unknown;
      end if;

      case Signature_Id is
         when Primary_DT         =>
            if Kind = Must_Be_Secondary_DT
              or else Kind = Must_Be_Interface
            then
               return False;
            end if;

         when Secondary_DT       =>
            if Kind = Must_Be_Primary_DT
              or else Kind = Must_Be_Interface
            then
               return False;
            end if;

         when Abstract_Interface =>
            if Kind = Must_Be_Primary_DT
              or else Kind = Must_Be_Secondary_DT
              or else Kind = Must_Be_Primary_Or_Secondary_DT
            then
               return False;
            end if;

         when others =>
            return False;

      end case;

      return True;
   end Check_Signature;

   ----------------
   -- Check_Size --
   ----------------

   function Check_Size
     (Old_T       : Tag;
      New_T       : Tag;
      Entry_Count : Natural) return Boolean
   is
      Max_Entries_Old : constant Natural := Get_Num_Prim_Ops (Old_T);
      Max_Entries_New : constant Natural := Get_Num_Prim_Ops (New_T);

   begin
      return Entry_Count <= Max_Entries_Old
        and then Entry_Count <= Max_Entries_New;
   end Check_Size;

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

   --   Obj'tag.TSD.Ancestor_Tags (Obj'tag.TSD.Idepth - Typ'tag.TSD.Idepth)
   --     = Typ'tag

   function CW_Membership (Obj_Tag : Tag; Typ_Tag : Tag) return Boolean is
      Pos : Integer;
   begin
      pragma Assert (Check_Signature (Obj_Tag, Must_Be_Primary_DT));
      pragma Assert (Check_Signature (Typ_Tag, Must_Be_Primary_DT));
      Pos := TSD (Obj_Tag).Idepth - TSD (Typ_Tag).Idepth;
      return Pos >= 0 and then TSD (Obj_Tag).Tags_Table (Pos) = Typ_Tag;
   end CW_Membership;

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
      Curr_DT  : constant Tag := To_Tag_Ptr (This).all;
      Id       : Natural;
      Last_Id  : Natural;
      Obj_Base : System.Address;
      Obj_DT   : Tag;
      Obj_TSD  : Type_Specific_Data_Ptr;

   begin
      pragma Assert
        (Check_Signature (Curr_DT, Must_Be_Primary_Or_Secondary_DT));
      pragma Assert
        (Check_Signature (T, Must_Be_Primary_Or_Interface));

      Obj_Base := This - Offset_To_Top (Curr_DT);
      Obj_DT   := To_Tag_Ptr (Obj_Base).all;

      pragma Assert
        (Check_Signature (Curr_DT, Must_Be_Primary_DT));

      Obj_TSD := TSD (Obj_DT);
      Last_Id := Obj_TSD.Idepth + Obj_TSD.Num_Interfaces;

      if Obj_TSD.Num_Interfaces > 0 then

         --  Traverse the ancestor tags table plus the interface tags table.
         --  The former part is required for:

         --     Iface_CW in Typ'Class

         Id := 0;
         loop
            if Obj_TSD.Tags_Table (Id) = T then
               return True;
            end if;

            Id := Id + 1;
            exit when Id > Last_Id;
         end loop;
      end if;

      return False;
   end IW_Membership;

   --------------------
   -- Descendant_Tag --
   --------------------

   function Descendant_Tag (External : String; Ancestor : Tag) return Tag is
      Int_Tag : Tag;

   begin
      pragma Assert (Check_Signature (Ancestor, Must_Be_Primary_DT));
      Int_Tag := Internal_Tag (External);
      pragma Assert (Check_Signature (Int_Tag, Must_Be_Primary_DT));

      if not Is_Descendant_At_Same_Level (Int_Tag, Ancestor) then
         raise Tag_Error;
      end if;

      return Int_Tag;
   end Descendant_Tag;

   -------------------
   -- Expanded_Name --
   -------------------

   function Expanded_Name (T : Tag) return String is
      Result : Cstring_Ptr;

   begin
      if T = No_Tag then
         raise Tag_Error;
      end if;

      pragma Assert (Check_Signature (T, Must_Be_Primary_Or_Interface));
      Result := TSD (T).Expanded_Name;
      return Result (1 .. Length (Result));
   end Expanded_Name;

   ------------------
   -- External_Tag --
   ------------------

   function External_Tag (T : Tag) return String is
      Result : Cstring_Ptr;

   begin
      if T = No_Tag then
         raise Tag_Error;
      end if;

      pragma Assert (Check_Signature (T, Must_Be_Primary_Or_Interface));
      Result := TSD (T).External_Tag;

      return Result (1 .. Length (Result));
   end External_Tag;

   ----------------------
   -- Get_Access_Level --
   ----------------------

   function Get_Access_Level (T : Tag) return Natural is
   begin
      pragma Assert (Check_Signature (T, Must_Be_Primary_DT));
      return TSD (T).Access_Level;
   end Get_Access_Level;

   ---------------------
   -- Get_Entry_Index --
   ---------------------

   function Get_Entry_Index (T : Tag; Position : Positive) return Positive is
      Index : constant Integer := Position - Default_Prim_Op_Count;
   begin
      pragma Assert (Check_Signature (T, Must_Be_Primary_DT));
      pragma Assert (Index > 0);
      return SSD (T).SSD_Table (Index).Index;
   end Get_Entry_Index;

   ----------------------
   -- Get_External_Tag --
   ----------------------

   function Get_External_Tag (T : Tag) return System.Address is
   begin
      pragma Assert (Check_Signature (T, Must_Be_Primary_DT));
      return To_Address (TSD (T).External_Tag);
   end Get_External_Tag;

   ----------------------
   -- Get_Num_Prim_Ops --
   ----------------------

   function Get_Num_Prim_Ops (T : Tag) return Natural is
   begin
      pragma Assert (Check_Signature (T, Must_Be_Primary_Or_Secondary_DT));

      if Is_Primary_DT (T) then
         return TSD (T).Num_Prim_Ops;
      else
         return OSD (Interface_Tag (T)).Num_Prim_Ops;
      end if;
   end Get_Num_Prim_Ops;

   -------------------------
   -- Get_Prim_Op_Address --
   -------------------------

   function Get_Prim_Op_Address
     (T        : Tag;
      Position : Positive) return System.Address
   is
   begin
      pragma Assert (Check_Signature (T, Must_Be_Primary_Or_Secondary_DT));
      pragma Assert (Check_Index (T, Position));
      return T.Prims_Ptr (Position);
   end Get_Prim_Op_Address;

   ----------------------
   -- Get_Prim_Op_Kind --
   ----------------------

   function Get_Prim_Op_Kind
     (T        : Tag;
      Position : Positive) return Prim_Op_Kind
   is
      Index : constant Integer := Position - Default_Prim_Op_Count;
   begin
      pragma Assert (Check_Signature (T, Must_Be_Primary_DT));
      pragma Assert (Index > 0);
      return SSD (T).SSD_Table (Index).Kind;
   end Get_Prim_Op_Kind;

   ----------------------
   -- Get_Offset_Index --
   ----------------------

   function Get_Offset_Index
     (T        : Interface_Tag;
      Position : Positive) return Positive
   is
      Index : constant Integer := Position - Default_Prim_Op_Count;
   begin
      pragma Assert (Check_Signature (Tag (T), Must_Be_Secondary_DT));
      pragma Assert (Index > 0);
      return OSD (T).OSD_Table (Index);
   end Get_Offset_Index;

   -------------------
   -- Get_RC_Offset --
   -------------------

   function Get_RC_Offset (T : Tag) return SSE.Storage_Offset is
   begin
      pragma Assert (Check_Signature (T, Must_Be_Primary_DT));
      return TSD (T).RC_Offset;
   end Get_RC_Offset;

   ---------------------------
   -- Get_Remotely_Callable --
   ---------------------------

   function Get_Remotely_Callable (T : Tag) return Boolean is
   begin
      pragma Assert (Check_Signature (T, Must_Be_Primary_DT));
      return TSD (T).Remotely_Callable;
   end Get_Remotely_Callable;

   ----------------
   -- Inherit_DT --
   ----------------

   procedure Inherit_DT (Old_T : Tag; New_T : Tag; Entry_Count : Natural) is
   begin
      pragma Assert (Check_Signature (Old_T, Must_Be_Primary_Or_Secondary_DT));
      pragma Assert (Check_Signature (New_T, Must_Be_Primary_Or_Secondary_DT));
      pragma Assert (Check_Size (Old_T, New_T, Entry_Count));

      if Old_T /= null then
         New_T.Prims_Ptr (1 .. Entry_Count) :=
           Old_T.Prims_Ptr (1 .. Entry_Count);
      end if;
   end Inherit_DT;

   -----------------
   -- Inherit_TSD --
   -----------------

   procedure Inherit_TSD (Old_Tag : Tag; New_Tag : Tag) is
      New_TSD_Ptr : Type_Specific_Data_Ptr;
      Old_TSD_Ptr : Type_Specific_Data_Ptr;

   begin
      pragma Assert (Check_Signature (New_Tag, Must_Be_Primary_Or_Interface));
      New_TSD_Ptr := TSD (New_Tag);

      if Old_Tag /= null then
         pragma Assert
           (Check_Signature (Old_Tag, Must_Be_Primary_Or_Interface));
         Old_TSD_Ptr := TSD (Old_Tag);
         New_TSD_Ptr.Idepth := Old_TSD_Ptr.Idepth + 1;
         New_TSD_Ptr.Num_Interfaces := Old_TSD_Ptr.Num_Interfaces;

         --  Copy the "table of ancestor tags" plus the "table of interfaces"
         --  of the parent.

         New_TSD_Ptr.Tags_Table
           (1 .. New_TSD_Ptr.Idepth + New_TSD_Ptr.Num_Interfaces) :=
             Old_TSD_Ptr.Tags_Table
               (0 .. Old_TSD_Ptr.Idepth + Old_TSD_Ptr.Num_Interfaces);
      else
         New_TSD_Ptr.Idepth         := 0;
         New_TSD_Ptr.Num_Interfaces := 0;
      end if;

      New_TSD_Ptr.Tags_Table (0) := New_Tag;
   end Inherit_TSD;

   ------------------
   -- Internal_Tag --
   ------------------

   function Internal_Tag (External : String) return Tag is
      Ext_Copy : aliased String (External'First .. External'Last + 1);
      Res      : Tag;

   begin
      --  Make a copy of the string representing the external tag with
      --  a null at the end.

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

   ---------------------------------
   -- Is_Descendant_At_Same_Level --
   ---------------------------------

   function Is_Descendant_At_Same_Level
     (Descendant : Tag;
      Ancestor   : Tag) return Boolean
   is
   begin
      return CW_Membership (Descendant, Ancestor)
        and then TSD (Descendant).Access_Level = TSD (Ancestor).Access_Level;
   end Is_Descendant_At_Same_Level;

   -------------------
   -- Is_Primary_DT --
   -------------------

   function Is_Primary_DT (T : Tag) return Boolean is
      Offset_To_Top_Ptr : constant Storage_Offset_Ptr :=
                            To_Storage_Offset_Ptr (To_Address (T)
                              - Offset_To_Signature);
      Signature         : constant Signature_Values :=
                            To_Signature_Values (Offset_To_Top_Ptr.all);
   begin
      return Signature (2) = Primary_DT;
   end Is_Primary_DT;

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

   -------------------
   -- Offset_To_Top --
   -------------------

   function Offset_To_Top
     (T : Tag) return System.Storage_Elements.Storage_Offset
   is
      Offset_To_Top_Ptr : constant Storage_Offset_Ptr :=
                            To_Storage_Offset_Ptr (To_Address (T)
                              - DT_Typeinfo_Ptr_Size
                              - DT_Offset_To_Top_Size);

   begin
      return Offset_To_Top_Ptr.all;
   end Offset_To_Top;

   ---------
   -- OSD --
   ---------

   function OSD
     (T : Interface_Tag) return Object_Specific_Data_Ptr
   is
      OSD_Ptr : Addr_Ptr;

   begin
      OSD_Ptr := To_Addr_Ptr (To_Address (T) - DT_Typeinfo_Ptr_Size);
      return To_Object_Specific_Data_Ptr (OSD_Ptr.all);
   end OSD;

   -----------------
   -- Parent_Size --
   -----------------

   function Parent_Size
     (Obj : System.Address;
      T   : Tag) return SSE.Storage_Count
   is
      Parent_Tag : Tag;
      --  The tag of the parent type through the dispatch table

      F : Acc_Size;
      --  Access to the _size primitive of the parent. We assume that it is
      --  always in the first slot of the dispatch table.

   begin
      pragma Assert (Check_Signature (T, Must_Be_Primary_DT));
      Parent_Tag := TSD (T).Tags_Table (1);
      F := To_Acc_Size (Parent_Tag.Prims_Ptr (1));

      --  Here we compute the size of the _parent field of the object

      return SSE.Storage_Count (F.all (Obj));
   end Parent_Size;

   ----------------
   -- Parent_Tag --
   ----------------

   function Parent_Tag (T : Tag) return Tag is
   begin
      if T = No_Tag then
         raise Tag_Error;
      end if;

      pragma Assert (Check_Signature (T, Must_Be_Primary_DT));

      --  The Parent_Tag of a root-level tagged type is defined to be No_Tag.
      --  The first entry in the Ancestors_Tags array will be null for such
      --  a type, but it's better to be explicit about returning No_Tag in
      --  this case.

      if TSD (T).Idepth = 0 then
         return No_Tag;
      else
         return TSD (T).Tags_Table (1);
      end if;
   end Parent_Tag;

   ----------------------------
   -- Register_Interface_Tag --
   ----------------------------

   procedure Register_Interface_Tag (T : Tag; Interface_T : Tag) is
      New_T_TSD : Type_Specific_Data_Ptr;
      Index     : Natural;

   begin
      pragma Assert (Check_Signature (T, Must_Be_Primary_DT));
      pragma Assert (Check_Signature (Interface_T, Must_Be_Interface));

      New_T_TSD := TSD (T);

      --  Check if the interface is already registered

      if New_T_TSD.Num_Interfaces > 0 then
         declare
            Id      : Natural          := New_T_TSD.Idepth + 1;
            Last_Id : constant Natural := New_T_TSD.Idepth
                                            + New_T_TSD.Num_Interfaces;

         begin
            loop
               if New_T_TSD.Tags_Table (Id) = Interface_T then
                  return;
               end if;

               Id := Id + 1;
               exit when Id > Last_Id;
            end loop;
         end;
      end if;

      New_T_TSD.Num_Interfaces := New_T_TSD.Num_Interfaces + 1;
      Index := New_T_TSD.Idepth + New_T_TSD.Num_Interfaces;
      New_T_TSD.Tags_Table (Index) := Interface_T;
   end Register_Interface_Tag;

   ------------------
   -- Register_Tag --
   ------------------

   procedure Register_Tag (T : Tag) is
   begin
      External_Tag_HTable.Set (T);
   end Register_Tag;

   ----------------------
   -- Set_Access_Level --
   ----------------------

   procedure Set_Access_Level (T : Tag; Value : Natural) is
   begin
      pragma Assert (Check_Signature (T, Must_Be_Primary_DT));
      TSD (T).Access_Level := Value;
   end Set_Access_Level;

   ---------------------
   -- Set_Entry_Index --
   ---------------------

   procedure Set_Entry_Index
     (T        : Tag;
      Position : Positive;
      Value    : Positive)
   is
      Index : constant Integer := Position - Default_Prim_Op_Count;

   begin
      pragma Assert (Check_Signature (T, Must_Be_Primary_DT));
      pragma Assert (Index > 0);
      SSD (T).SSD_Table (Index).Index := Value;
   end Set_Entry_Index;

   -----------------------
   -- Set_Expanded_Name --
   -----------------------

   procedure Set_Expanded_Name (T : Tag; Value : System.Address) is
   begin
      pragma Assert
        (Check_Signature (T, Must_Be_Primary_Or_Interface));
      TSD (T).Expanded_Name := To_Cstring_Ptr (Value);
   end Set_Expanded_Name;

   ----------------------
   -- Set_External_Tag --
   ----------------------

   procedure Set_External_Tag (T : Tag; Value : System.Address) is
   begin
      pragma Assert (Check_Signature (T, Must_Be_Primary_Or_Interface));
      TSD (T).External_Tag := To_Cstring_Ptr (Value);
   end Set_External_Tag;

   ----------------------
   -- Set_Num_Prim_Ops --
   ----------------------

   procedure Set_Num_Prim_Ops (T : Tag; Value : Natural) is
   begin
      pragma Assert (Check_Signature (T, Must_Be_Primary_Or_Secondary_DT));

      if Is_Primary_DT (T) then
         TSD (T).Num_Prim_Ops := Value;
      else
         OSD (Interface_Tag (T)).Num_Prim_Ops := Value;
      end if;
   end Set_Num_Prim_Ops;

   ----------------------
   -- Set_Offset_Index --
   ----------------------

   procedure Set_Offset_Index
     (T        : Interface_Tag;
      Position : Positive;
      Value    : Positive)
   is
      Index : constant Integer := Position - Default_Prim_Op_Count;
   begin
      pragma Assert (Check_Signature (Tag (T), Must_Be_Secondary_DT));
      pragma Assert (Index > 0);
      OSD (T).OSD_Table (Index) := Value;
   end Set_Offset_Index;

   -----------------------
   -- Set_Offset_To_Top --
   -----------------------

   procedure Set_Offset_To_Top
     (T     : Tag;
      Value : System.Storage_Elements.Storage_Offset)
   is
      Offset_To_Top_Ptr : constant Storage_Offset_Ptr :=
                            To_Storage_Offset_Ptr (To_Address (T)
                              - DT_Typeinfo_Ptr_Size
                              - DT_Offset_To_Top_Size);
   begin
      pragma Assert (Check_Signature (T, Must_Be_Primary_Or_Secondary_DT));
      Offset_To_Top_Ptr.all := Value;
   end Set_Offset_To_Top;

   -------------
   -- Set_OSD --
   -------------

   procedure Set_OSD (T : Interface_Tag; Value : System.Address) is
      OSD_Ptr : Addr_Ptr;
   begin
      pragma Assert (Check_Signature (Tag (T), Must_Be_Secondary_DT));
      OSD_Ptr := To_Addr_Ptr (To_Address (T) - DT_Typeinfo_Ptr_Size);
      OSD_Ptr.all := Value;
   end Set_OSD;

   -------------------------
   -- Set_Prim_Op_Address --
   -------------------------

   procedure Set_Prim_Op_Address
     (T        : Tag;
      Position : Positive;
      Value    : System.Address)
   is
   begin
      pragma Assert (Check_Signature (T, Must_Be_Primary_Or_Secondary_DT));
      pragma Assert (Check_Index (T, Position));
      T.Prims_Ptr (Position) := Value;
   end Set_Prim_Op_Address;

   ----------------------
   -- Set_Prim_Op_Kind --
   ----------------------

   procedure Set_Prim_Op_Kind
     (T        : Tag;
      Position : Positive;
      Value    : Prim_Op_Kind)
   is
      Index : constant Integer := Position - Default_Prim_Op_Count;
   begin
      pragma Assert (Check_Signature (T, Must_Be_Primary_DT));
      pragma Assert (Index > 0);
      SSD (T).SSD_Table (Index).Kind := Value;
   end Set_Prim_Op_Kind;

   -------------------
   -- Set_RC_Offset --
   -------------------

   procedure Set_RC_Offset (T : Tag; Value : SSE.Storage_Offset) is
   begin
      pragma Assert (Check_Signature (T, Must_Be_Primary_DT));
      TSD (T).RC_Offset := Value;
   end Set_RC_Offset;

   ---------------------------
   -- Set_Remotely_Callable --
   ---------------------------

   procedure Set_Remotely_Callable (T : Tag; Value : Boolean) is
   begin
      pragma Assert (Check_Signature (T, Must_Be_Primary_DT));
      TSD (T).Remotely_Callable := Value;
   end Set_Remotely_Callable;

   -------------
   -- Set_SSD --
   -------------

   procedure Set_SSD (T : Tag; Value : System.Address) is
   begin
      pragma Assert (Check_Signature (T, Must_Be_Primary_DT));
      TSD (T).SSD_Ptr := Value;
   end Set_SSD;

   -------------
   -- Set_TSD --
   -------------

   procedure Set_TSD (T : Tag; Value : System.Address) is
      TSD_Ptr : Addr_Ptr;
   begin
      pragma Assert (Check_Signature (T, Must_Be_Primary_Or_Interface));
      TSD_Ptr := To_Addr_Ptr (To_Address (T) - DT_Typeinfo_Ptr_Size);
      TSD_Ptr.all := Value;
   end Set_TSD;

   ---------
   -- SSD --
   ---------

   function SSD (T : Tag) return Select_Specific_Data_Ptr is
   begin
      return To_Select_Specific_Data_Ptr (TSD (T).SSD_Ptr);
   end SSD;

   ------------------
   -- Typeinfo_Ptr --
   ------------------

   function Typeinfo_Ptr (T : Tag) return System.Address is
      TSD_Ptr : constant Addr_Ptr :=
                  To_Addr_Ptr (To_Address (T) - DT_Typeinfo_Ptr_Size);
   begin
      return TSD_Ptr.all;
   end Typeinfo_Ptr;

   ---------
   -- TSD --
   ---------

   function TSD (T : Tag) return Type_Specific_Data_Ptr is
      TSD_Ptr : constant Addr_Ptr :=
                  To_Addr_Ptr (To_Address (T) - DT_Typeinfo_Ptr_Size);
   begin
      return To_Type_Specific_Data_Ptr (TSD_Ptr.all);
   end TSD;

end Ada.Tags;
