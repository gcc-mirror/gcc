------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                             A D A . T A G S                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

with System;
with System.Storage_Elements;
with Unchecked_Conversion;

package Ada.Tags is
   pragma Preelaborate_05;
   --  In accordance with Ada 2005 AI-362

   type Tag is private;

   No_Tag : constant Tag;

   function Expanded_Name (T : Tag) return String;

   function External_Tag (T : Tag) return String;

   function Internal_Tag (External : String) return Tag;

   function Descendant_Tag
     (External : String;
      Ancestor : Tag) return Tag;
   pragma Ada_05 (Descendant_Tag);

   function Is_Descendant_At_Same_Level
     (Descendant : Tag;
      Ancestor   : Tag) return Boolean;
   pragma Ada_05 (Is_Descendant_At_Same_Level);

   function Parent_Tag (T : Tag) return Tag;
   pragma Ada_05 (Parent_Tag);

   Tag_Error : exception;

private
   --  The following subprogram specifications are placed here instead of
   --  the package body to see them from the frontend through rtsfind.

   ---------------------------------------------------------------
   -- Abstract Procedural Interface For The GNAT Dispatch Table --
   ---------------------------------------------------------------

   --  GNAT's Dispatch Table format is customizable in order to match the
   --  format used in another language. GNAT supports programs that use two
   --  different dispatch table formats at the same time: the native format
   --  that supports Ada 95 tagged types and which is described in Ada.Tags,
   --  and a foreign format for types that are imported from some other
   --  language (typically C++) which is described in Interfaces.CPP. The
   --  runtime information kept for each tagged type is separated into two
   --  objects: the Dispatch Table and the Type Specific Data record. These
   --  two objects are allocated statically using the constants:

   --      DT Size  = DT_Prologue_Size  + Nb_Prim * DT_Entry_Size
   --      TSD Size = TSD_Prologue_Size + (1 + Idepth)  * TSD_Entry_Size

   --  where Nb_prim is the number of primitive operations of the given
   --  type and Idepth its inheritance depth.

   --  In order to set or retrieve information from the Dispatch Table or
   --  the Type Specific Data record, GNAT generates calls to Set_XXX or
   --  Get_XXX routines, where XXX is the name of the field of interest.

   type Dispatch_Table;
   type Tag is access all Dispatch_Table;
   type Interface_Tag is access all Dispatch_Table;

   No_Tag : constant Tag := null;

   type Object_Specific_Data (Nb_Prim : Positive);
   type Object_Specific_Data_Ptr is access all Object_Specific_Data;
   --  Information associated with the secondary dispatch table of tagged-type
   --  objects implementing abstract interfaces.

   type Select_Specific_Data (Nb_Prim : Positive);
   type Select_Specific_Data_Ptr is access all Select_Specific_Data;
   --  A table used to store the primitive operation kind and entry index of
   --  primitive subprograms of a type that implements a limited interface.
   --  The Select Specific Data table resides in the Type Specific Data of a
   --  type. This construct is used in the handling of dispatching triggers
   --  in select statements.

   type Type_Specific_Data;
   type Type_Specific_Data_Ptr is access all Type_Specific_Data;

   --  Primitive operation kinds. These values differentiate the kinds of
   --  callable entities stored in the dispatch table. Certain kinds may
   --  not be used, but are added for completeness.

   type Prim_Op_Kind is
     (POK_Function,
      POK_Procedure,
      POK_Protected_Entry,
      POK_Protected_Function,
      POK_Protected_Procedure,
      POK_Task_Entry,
      POK_Task_Function,
      POK_Task_Procedure);

   Default_Prim_Op_Count : constant Positive := 15;
   --  Number of predefined primitive operations added by the Expander for a
   --  tagged type. It is utilized for indexing in the two auxiliary tables
   --  used for dispatching asynchronous, conditional and timed selects. In
   --  order to be space efficient, indexing is performed by subtracting this
   --  constant value from the provided position in the auxiliary tables (must
   --  match Exp_Disp.Default_Prim_Op_Count).

   package SSE renames System.Storage_Elements;

   function CW_Membership (Obj_Tag : Tag; Typ_Tag : Tag) return Boolean;
   --  Given the tag of an object and the tag associated to a type, return
   --  true if Obj is in Typ'Class.

   function IW_Membership (This : System.Address; T : Tag) return Boolean;
   --  Ada 2005 (AI-251): General routine that checks if a given object
   --  implements a tagged type. Its common usage is to check if Obj is in
   --  Iface'Class, but it is also used to check if a class-wide interface
   --  implements a given type (Iface_CW_Typ in T'Class). For example:
   --
   --      type I is interface;
   --      type T is tagged ...
   --
   --      function Test (O : in I'Class) is
   --      begin
   --         return O in T'Class.
   --      end Test;

   function Get_Access_Level (T : Tag) return Natural;
   --  Given the tag associated with a type, returns the accessibility level
   --  of the type.

   function Get_Entry_Index (T : Tag; Position : Positive) return Positive;
   --  Return a primitive operation's entry index (if entry) given a dispatch
   --  table T and a position of a primitive operation in T.

   function Get_External_Tag (T : Tag) return System.Address;
   --  Retrieve the address of a null terminated string containing
   --  the external name.

   function Get_Offset_Index
     (T        : Interface_Tag;
      Position : Positive) return Positive;
   --  Given a pointer to a secondary dispatch table (T) and a position of an
   --  operation in the DT, retrieve the corresponding operation's position in
   --  the primary dispatch table from the Offset Specific Data table of T.

   function Get_Prim_Op_Address
     (T        : Tag;
      Position : Positive) return System.Address;
   --  Given a pointer to a dispatch table (T) and a position in the DT
   --  this function returns the address of the virtual function stored
   --  in it (used for dispatching calls).

   function Get_Prim_Op_Kind
     (T        : Tag;
      Position : Positive) return Prim_Op_Kind;
   --  Return a primitive operation's kind given a dispatch table T and a
   --  position of a primitive operation in T.

   function Get_RC_Offset (T : Tag) return SSE.Storage_Offset;
   --  Return the Offset of the implicit record controller when the object
   --  has controlled components. O otherwise.

   pragma Export (Ada, Get_RC_Offset, "ada__tags__get_rc_offset");
   --  This procedure is used in s-finimp to compute the deep routines
   --  it is exported manually in order to avoid changing completely the
   --  organization of the run time.

   function Get_Remotely_Callable (T : Tag) return Boolean;
   --  Return the value previously set by Set_Remotely_Callable

   procedure Inherit_DT (Old_T : Tag; New_T : Tag; Entry_Count : Natural);
   --  Entry point used to initialize the DT of a type knowing the tag
   --  of the direct ancestor and the number of primitive ops that are
   --  inherited (Entry_Count).

   procedure Inherit_TSD (Old_Tag : Tag; New_Tag : Tag);
   --  Initialize the TSD of a type knowing the tag of the direct ancestor

   function OSD (T : Interface_Tag) return Object_Specific_Data_Ptr;
   --  Ada 2005 (AI-251): Given a pointer T to a secondary dispatch table,
   --  retrieve the address of the record containing the Objet Specific
   --  Data table.

   function Parent_Size
     (Obj : System.Address;
      T   : Tag) return SSE.Storage_Count;
   --  Computes the size the ancestor part of a tagged extension object whose
   --  address is 'obj' by calling indirectly the ancestor _size function. The
   --  ancestor is the parent of the type represented by tag T. This function
   --  assumes that _size is always in slot one of the dispatch table.

   pragma Export (Ada, Parent_Size, "ada__tags__parent_size");
   --  This procedure is used in s-finimp and is thus exported manually

   procedure Register_Interface_Tag (T : Tag; Interface_T : Tag);
   --  Ada 2005 (AI-251): Used to initialize the table of interfaces
   --  implemented by a type. Required to give support to IW_Membership.

   procedure Register_Tag (T : Tag);
   --  Insert the Tag and its associated external_tag in a table for the
   --  sake of Internal_Tag

   procedure Set_Entry_Index (T : Tag; Position : Positive; Value : Positive);
   --  Set the entry index of a primitive operation in T's TSD table indexed
   --  by Position.

   procedure Set_Num_Prim_Ops (T : Tag; Value : Natural);
   --  Set the number of primitive operations in the dispatch table of T. This
   --  is used for debugging purposes.

   procedure Set_Offset_Index
     (T        : Interface_Tag;
      Position : Positive;
      Value    : Positive);
   --  Set the offset value of a primitive operation in a secondary dispatch
   --  table denoted by T, indexed by Position.

   procedure Set_Offset_To_Top
     (T     : Tag;
      Value : System.Storage_Elements.Storage_Offset);
   --  Ada 2005 (AI-251): Initialize the Offset_To_Top field in the prologue of
   --  the dispatch table. In primary dispatch tables the value of this field
   --  is always 0; in secondary dispatch tables this is the offset to the base
   --  of the enclosing type.

   procedure Set_OSD (T : Interface_Tag; Value : System.Address);
   --  Given a pointer T to a secondary dispatch table, store the pointer to
   --  the record containing the Object Specific Data generated by GNAT.

   procedure Set_Prim_Op_Address
     (T        : Tag;
      Position : Positive;
      Value    : System.Address);
   --  Given a pointer to a dispatch Table (T) and a position in the dispatch
   --  Table put the address of the virtual function in it (used for
   --  overriding).

   procedure Set_Prim_Op_Kind
     (T        : Tag;
      Position : Positive;
      Value    : Prim_Op_Kind);
   --  Set the kind of a primitive operation in T's TSD table indexed by
   --  Position.

   procedure Set_SSD (T : Tag; Value : System.Address);
   --  Given a pointer T to a dispatch Table, stores the pointer to the record
   --  containing the Select Specific Data generated by GNAT.

   procedure Set_TSD (T : Tag; Value : System.Address);
   --  Given a pointer T to a dispatch Table, stores the address of the record
   --  containing the Type Specific Data generated by GNAT.

   procedure Set_Access_Level (T : Tag; Value : Natural);
   --  Sets the accessibility level of the tagged type associated with T
   --  in its TSD.

   procedure Set_Expanded_Name (T : Tag; Value : System.Address);
   --  Set the address of the string containing the expanded name
   --  in the Dispatch table.

   procedure Set_External_Tag (T : Tag; Value : System.Address);
   --  Set the address of the string containing the external tag
   --  in the Dispatch table.

   procedure Set_RC_Offset (T : Tag; Value : SSE.Storage_Offset);
   --  Sets the Offset of the implicit record controller when the object
   --  has controlled components. Set to O otherwise.

   procedure Set_Remotely_Callable (T : Tag; Value : Boolean);
   --  Set to true if the type has been declared in a context described
   --  in E.4 (18).

   function SSD (T : Tag) return Select_Specific_Data_Ptr;
   --  Given a pointer T to a dispatch Table, retrieves the address of the
   --  record containing the Select Specific Data in T's TSD.

   function TSD (T : Tag) return Type_Specific_Data_Ptr;
   --  Given a pointer T to a dispatch Table, retrieves the address of the
   --  record containing the Type Specific Data generated by GNAT.

   DT_Prologue_Size : constant SSE.Storage_Count :=
                        SSE.Storage_Count
                          (3 * (Standard'Address_Size / System.Storage_Unit));
   --  Size of the first part of the dispatch table

   DT_Signature_Size : constant SSE.Storage_Count :=
                         SSE.Storage_Count
                           (Standard'Address_Size / System.Storage_Unit);
   --  Size of the Signature field of the dispatch table

   DT_Offset_To_Top_Size : constant SSE.Storage_Count :=
                            SSE.Storage_Count
                              (Standard'Address_Size / System.Storage_Unit);
   --  Size of the Offset_To_Top field of the Dispatch Table

   DT_Typeinfo_Ptr_Size : constant SSE.Storage_Count :=
                            SSE.Storage_Count
                              (Standard'Address_Size / System.Storage_Unit);
   --  Size of the Typeinfo_Ptr field of the Dispatch Table

   DT_Entry_Size : constant SSE.Storage_Count :=
                     SSE.Storage_Count
                       (1 * (Standard'Address_Size / System.Storage_Unit));
   --  Size of each primitive operation entry in the Dispatch Table

   TSD_Prologue_Size : constant SSE.Storage_Count :=
                         SSE.Storage_Count
                          (10 * (Standard'Address_Size / System.Storage_Unit));
   --  Size of the first part of the type specific data

   TSD_Entry_Size : constant SSE.Storage_Count :=
     SSE.Storage_Count (1 * (Standard'Address_Size / System.Storage_Unit));
   --  Size of each ancestor tag entry in the TSD

   type Address_Array is array (Natural range <>) of System.Address;
   pragma Suppress (Index_Check, On => Address_Array);
   --  The reason we suppress index checks is that in the body, objects
   --  of this type are declared with a dummy size of 1, the actual size
   --  depending on the number of primitive operations.

   type Signature_Kind is
      (Unknown,
       Valid_Signature,
       Primary_DT,
       Secondary_DT,
       Abstract_Interface);
   for Signature_Kind'Size use 8;
   --  Kind of signature found in the header of the dispatch table. These
   --  signatures are generated by the frontend and are used by the Check_XXX
   --  routines to ensure that the kind of dispatch table managed by each of
   --  the routines in this package is correct. This additional check is only
   --  performed with this run-time package is compiled with assertions enabled

   --  The signature is a sequence of two bytes. The first byte must have the
   --  value Valid_Signature, and the second byte must have a value in the
   --  range Primary_DT .. Abstract_Interface. The Unknown value is used by
   --  the Check_XXX routines to indicate that the signature is wrong.

   --  Unchecked Conversions

   type Addr_Ptr is access System.Address;
   type Tag_Ptr  is access Tag;

   type Signature_Values is
      array (1 .. DT_Signature_Size) of Signature_Kind;
   --  Type used to see the signature as a sequence of Signature_Kind values

   function To_Addr_Ptr is
      new Unchecked_Conversion (System.Address, Addr_Ptr);

   function To_Type_Specific_Data_Ptr is
     new Unchecked_Conversion (System.Address, Type_Specific_Data_Ptr);

   function To_Address is
     new Unchecked_Conversion (Interface_Tag, System.Address);

   function To_Address is
     new Unchecked_Conversion (Tag, System.Address);

   function To_Address is
     new Unchecked_Conversion (Type_Specific_Data_Ptr, System.Address);

   function To_Object_Specific_Data_Ptr is
     new Unchecked_Conversion (System.Address, Object_Specific_Data_Ptr);

   function To_Select_Specific_Data_Ptr is
     new Unchecked_Conversion (System.Address, Select_Specific_Data_Ptr);

   function To_Signature_Values is
     new Unchecked_Conversion (System.Storage_Elements.Storage_Offset,
                               Signature_Values);

   function To_Tag_Ptr is
     new Unchecked_Conversion (System.Address, Tag_Ptr);

   --  Primitive dispatching operations are always inlined, to facilitate
   --  use in a minimal/no run-time environment for high integrity use.

   pragma Inline_Always (CW_Membership);
   pragma Inline_Always (IW_Membership);
   pragma Inline_Always (Get_Access_Level);
   pragma Inline_Always (Get_Entry_Index);
   pragma Inline_Always (Get_Offset_Index);
   pragma Inline_Always (Get_Prim_Op_Address);
   pragma Inline_Always (Get_Prim_Op_Kind);
   pragma Inline_Always (Get_RC_Offset);
   pragma Inline_Always (Get_Remotely_Callable);
   pragma Inline_Always (Inherit_DT);
   pragma Inline_Always (Inherit_TSD);
   pragma Inline_Always (OSD);
   pragma Inline_Always (Register_Interface_Tag);
   pragma Inline_Always (Register_Tag);
   pragma Inline_Always (Set_Access_Level);
   pragma Inline_Always (Set_Entry_Index);
   pragma Inline_Always (Set_Expanded_Name);
   pragma Inline_Always (Set_External_Tag);
   pragma Inline_Always (Set_Num_Prim_Ops);
   pragma Inline_Always (Set_Offset_Index);
   pragma Inline_Always (Set_Offset_To_Top);
   pragma Inline_Always (Set_Prim_Op_Address);
   pragma Inline_Always (Set_Prim_Op_Kind);
   pragma Inline_Always (Set_RC_Offset);
   pragma Inline_Always (Set_Remotely_Callable);
   pragma Inline_Always (Set_OSD);
   pragma Inline_Always (Set_SSD);
   pragma Inline_Always (Set_TSD);
   pragma Inline_Always (SSD);
   pragma Inline_Always (TSD);

end Ada.Tags;
