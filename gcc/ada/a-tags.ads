------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                             A D A . T A G S                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2006, Free Software Foundation, Inc.         --
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
   pragma Preelaborable_Initialization (Tag);

   No_Tag : constant Tag;

   function Expanded_Name (T : Tag) return String;

   function Wide_Expanded_Name (T : Tag) return Wide_String;
   pragma Ada_05 (Wide_Expanded_Name);

   function Wide_Wide_Expanded_Name (T : Tag) return Wide_Wide_String;
   pragma Ada_05 (Wide_Wide_Expanded_Name);

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

   type Tag_Array is array (Positive range <>) of Tag;

   function Interface_Ancestor_Tags (T : Tag) return Tag_Array;
   pragma Ada_05 (Interface_Ancestor_Tags);

   Tag_Error : exception;

private
   --  Structure of the GNAT Primary Dispatch Table

   --           +--------------------+
   --           |      table of      |
   --           :predefined primitive:
   --           |    ops pointers    |
   --           +--------------------+
   --           |      Signature     |
   --           +--------------------+
   --           |     Tagged_Kind    |
   --           +--------------------+
   --           |    Offset_To_Top   |
   --           +--------------------+
   --           |Typeinfo_Ptr/TSD_Ptr---> Type Specific Data
   --  Tag ---> +--------------------+   +-------------------+
   --           |      table of      |   | inheritance depth |
   --           :   primitive ops    :   +-------------------+
   --           |      pointers      |   |   access level    |
   --           +--------------------+   +-------------------+
   --                                    |   expanded name   |
   --                                    +-------------------+
   --                                    |   external tag    |
   --                                    +-------------------+
   --                                    |   hash table link |
   --                                    +-------------------+
   --                                    | remotely callable |
   --                                    +-------------------+
   --                                    | rec ctrler offset |
   --                                    +-------------------+
   --                                    |   num prim ops    |
   --                                    +-------------------+
   --                                    |  Ifaces_Table_Ptr --> Interface Data
   --                                    +-------------------+   +------------+
   --         Select Specific Data  <----     SSD_Ptr        |   |  table     |
   --         +------------------+       +-------------------+   :    of      :
   --         |table of primitive|       | table of          |   | interfaces |
   --         :   operation      :       :    ancestor       :   +------------+
   --         |      kinds       |       |       tags        |
   --         +------------------+       +-------------------+
   --         |table of          |
   --         :   entry          :
   --         |      indices     |
   --         +------------------+

   --  Structure of the GNAT Secondary Dispatch Table

   --           +-----------------------+
   --           |       table of        |
   --           :  predefined primitive :
   --           |     ops pointers      |
   --           +-----------------------+
   --           |       Signature       |
   --           +-----------------------+
   --           |      Tagged_Kind      |
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

   --  The runtime information kept for each tagged type is separated into two
   --  objects: the Dispatch Table and the Type Specific Data record. These
   --  two objects are allocated statically using the constants:

   --      DT Size  = DT_Prologue_Size  + Nb_Prim * DT_Entry_Size

   --  where Nb_prim is the number of primitive operations of the given
   --  type and Idepth its inheritance depth.

   type Address_Array is array (Natural range <>) of System.Address;
   pragma Suppress (Index_Check, On => Address_Array);
   --  The reason we suppress index checks is that in the dispatch table,
   --  the component of this type is declared with a dummy size of 1, the
   --  actual size depending on the number of primitive operations.

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

      --  Signature     : Signature_Kind;
      --  Tagged_Kind   : Tagged_Kind;
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

   subtype Cstring is String (Positive);
   type Cstring_Ptr is access all Cstring;
   pragma No_Strict_Aliasing (Cstring_Ptr);

   --  We suppress index checks because the declared size in the record below
   --  is a dummy size of one (see below).

   type Tag_Table is array (Natural range <>) of Tag;
   pragma Suppress_Initialization (Tag_Table);
   pragma Suppress (Index_Check, On => Tag_Table);

   package SSE renames System.Storage_Elements;

   --  Type specific data types

   type Type_Specific_Data (Idepth : Natural) is record
      --  Inheritance Depth Level: Used to implement the membership test
      --  associated with single inheritance of tagged types in constant-time.
      --  It also indicates the size of the Tags_Table component.

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
      --  Components used to support to the Ada.Tags subprograms in RM 3.9.
      --  Note: Expanded_Name is referenced by GDB ???

      Remotely_Callable : Boolean;
      --  Used to check ARM E.4 (18)

      RC_Offset : SSE.Storage_Offset;
      --  Controller Offset: Used to give support to tagged controlled objects
      --  (see Get_Deep_Controller at s-finimp)

      Ifaces_Table_Ptr : System.Address;
      --  Pointer to the table of interface tags. It is used to implement the
      --  membership test associated with interfaces and also for backward
      --  abstract interface type conversions (Ada 2005:AI-251)

      SSD_Ptr : System.Address;
      --  Pointer to a table of records used in dispatching selects. This
      --  field has a meaningful value for all tagged types that implement
      --  a limited, protected, synchronized or task interfaces and have
      --  non-predefined primitive operations.

      Tags_Table : Tag_Table (0 .. Idepth);
      --  Table of ancestor tags. Its size actually depends on the inheritance
      --  depth level of the tagged type.
   end record;

   --  Declarations for the table of interfaces

   type Interface_Data_Element is record
      Iface_Tag            : Tag;
      Static_Offset_To_Top : Boolean;
      Offset_To_Top_Value  : System.Storage_Elements.Storage_Offset;
      Offset_To_Top_Func   : System.Address;
   end record;
   --  If some ancestor of the tagged type has discriminants the field
   --  Static_Offset_To_Top is False and the field Offset_To_Top_Func
   --  is used to store the address of the function generated by the
   --  expander which provides this value; otherwise Static_Offset_To_Top
   --  is True and such value is stored in the Offset_To_Top_Value field.

   type Interfaces_Array is
     array (Natural range <>) of Interface_Data_Element;

   type Interface_Data (Nb_Ifaces : Positive) is record
      Ifaces_Table : Interfaces_Array (1 .. Nb_Ifaces);
   end record;

   --  Declaration of tag types

   type Tag is access all Dispatch_Table;
   type Tag_Ptr is access Tag;
   type Interface_Tag is access all Dispatch_Table;
   type Type_Specific_Data_Ptr is access all Type_Specific_Data;

   No_Tag : constant Tag := null;

   type Interface_Data_Ptr is access all Interface_Data;
   --  Table of abstract interfaces used to give support to backward interface
   --  conversions and also to IW_Membership.

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

   --  Tagged type kinds with respect to concurrency and limitedness

   type Tagged_Kind is
     (TK_Abstract_Limited_Tagged,
      TK_Abstract_Tagged,
      TK_Limited_Tagged,
      TK_Protected,
      TK_Tagged,
      TK_Task);

   type Tagged_Kind_Ptr is access all Tagged_Kind;

   Default_Prim_Op_Count : constant Positive := 15;
   --  Maximum number of predefined primitive operations of a tagged type.

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

   DT_Min_Prologue_Size : constant SSE.Storage_Count :=
                            SSE.Storage_Count
                              (2 * (Standard'Address_Size /
                                      System.Storage_Unit));
   --  Size of the hidden part of the dispatch table used when the program
   --  is compiled under restriction No_Dispatching_Calls. It contains the
   --  pointer to the TSD record plus a dummy entry whose address is used
   --  at run-time as the Tag.

   DT_Prologue_Size : constant SSE.Storage_Count :=
                        SSE.Storage_Count
                          ((Default_Prim_Op_Count + 4) *
                            (Standard'Address_Size / System.Storage_Unit));
   --  Size of the hidden part of the dispatch table. It contains the table of
   --  predefined primitive operations plus the C++ ABI header.

   DT_Signature_Size : constant SSE.Storage_Count :=
                         SSE.Storage_Count
                           (1 * (Standard'Address_Size / System.Storage_Unit));
   --  Size of the Signature field of the dispatch table

   DT_Tagged_Kind_Size : constant SSE.Storage_Count :=
     SSE.Storage_Count (1 * (Standard'Address_Size / System.Storage_Unit));
   --  Size of the Tagged_Type_Kind field of the dispatch table

   DT_Offset_To_Top_Size : constant SSE.Storage_Count :=
                             SSE.Storage_Count
                               (1 * (Standard'Address_Size /
                                       System.Storage_Unit));
   --  Size of the Offset_To_Top field of the Dispatch Table

   DT_Typeinfo_Ptr_Size : constant SSE.Storage_Count :=
                            SSE.Storage_Count
                              (1 * (Standard'Address_Size /
                                      System.Storage_Unit));
   --  Size of the Typeinfo_Ptr field of the Dispatch Table

   DT_Entry_Size : constant SSE.Storage_Count :=
                     SSE.Storage_Count
                       (1 * (Standard'Address_Size / System.Storage_Unit));
   --  Size of each primitive operation entry in the Dispatch Table

   Tag_Size : constant SSE.Storage_Count :=
     SSE.Storage_Count (1 * (Standard'Address_Size / System.Storage_Unit));
   --  Size of each tag

   --  Constants used by the code generated by the frontend to get access
   --  to the header of the dispatch table.

   K_Typeinfo      : constant SSE.Storage_Count := DT_Typeinfo_Ptr_Size;
   K_Offset_To_Top : constant SSE.Storage_Count :=
                       System.Storage_Elements."+"
                         (K_Typeinfo, DT_Offset_To_Top_Size);
   K_Tagged_Kind   : constant SSE.Storage_Count :=
                       System.Storage_Elements."+"
                         (K_Offset_To_Top, DT_Tagged_Kind_Size);
   K_Signature     : constant SSE.Storage_Count :=
                       System.Storage_Elements."+"
                         (K_Tagged_Kind, DT_Signature_Size);

   --  The following subprogram specifications are placed here instead of
   --  the package body to see them from the frontend through rtsfind.

   function Base_Address (This : System.Address) return System.Address;
   --  Ada 2005 (AI-251): Displace "This" to point to the base address of
   --  the object (that is, the address of the primary tag of the object).

   function CW_Membership (Obj_Tag : Tag; Typ_Tag : Tag) return Boolean;
   --  Given the tag of an object and the tag associated to a type, return
   --  true if Obj is in Typ'Class.

   function Displace (This : System.Address; T : Tag) return System.Address;
   --  Ada 2005 (AI-251): Displace "This" to point to the secondary dispatch
   --  table of T.

   function Get_Entry_Index (T : Tag; Position : Positive) return Positive;
   --  Ada 2005 (AI-251): Return a primitive operation's entry index (if entry)
   --  given a dispatch table T and a position of a primitive operation in T.

   function Get_External_Tag (T : Tag) return System.Address;
   --  Returns address of a null terminated string containing the external name

   function Get_Offset_Index
     (T        : Tag;
      Position : Positive) return Positive;
   --  Ada 2005 (AI-251): Given a pointer to a secondary dispatch table (T) and
   --  a position of an operation in the DT, retrieve the corresponding
   --  operation's position in the primary dispatch table from the Offset
   --  Specific Data table of T.

   function Get_Prim_Op_Kind
     (T        : Tag;
      Position : Positive) return Prim_Op_Kind;
   --  Ada 2005 (AI-251): Return a primitive operation's kind given a dispatch
   --  table T and a position of a primitive operation in T.

   function Get_RC_Offset (T : Tag) return SSE.Storage_Offset;
   --  Return the Offset of the implicit record controller when the object
   --  has controlled components. O otherwise.

   pragma Export (Ada, Get_RC_Offset, "ada__tags__get_rc_offset");
   --  This procedure is used in s-finimp to compute the deep routines
   --  it is exported manually in order to avoid changing completely the
   --  organization of the run time.

   function Get_Tagged_Kind (T : Tag) return Tagged_Kind;
   --  Ada 2005 (AI-345): Given a pointer to either a primary or a secondary
   --  dispatch table, return the tagged kind of a type in the context of
   --  concurrency and limitedness.

   function IW_Membership (This : System.Address; T : Tag) return Boolean;
   --  Ada 2005 (AI-251): General routine that checks if a given object
   --  implements a tagged type. Its common usage is to check if Obj is in
   --  Iface'Class, but it is also used to check if a class-wide interface
   --  implements a given type (Iface_CW_Typ in T'Class). For example:
   --
   --      type I is interface;
   --      type T is tagged ...
   --
   --      function Test (O : I'Class) is
   --      begin
   --         return O in T'Class.
   --      end Test;

   function Offset_To_Top
     (This : System.Address) return System.Storage_Elements.Storage_Offset;
   --  Ada 2005 (AI-251): Returns the current value of the offset_to_top
   --  component available in the prologue of the dispatch table. If the parent
   --  of the tagged type has discriminants this value is stored in a record
   --  component just immediately after the tag component.

   function OSD (T : Tag) return Object_Specific_Data_Ptr;
   --  Ada 2005 (AI-251): Given a pointer T to a secondary dispatch table,
   --  retrieve the address of the record containing the Object Specific
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

   procedure Register_Interface_Tag
     (T           : Tag;
      Interface_T : Tag;
      Position    : Positive);
   --  Ada 2005 (AI-251): Used to initialize the table of interfaces
   --  implemented by a type. Required to give support to backward interface
   --  conversions and also to IW_Membership.

   procedure Register_Tag (T : Tag);
   --  Insert the Tag and its associated external_tag in a table for the
   --  sake of Internal_Tag

   procedure Set_Entry_Index (T : Tag; Position : Positive; Value : Positive);
   --  Ada 2005 (AI-345): Set the entry index of a primitive operation in T's
   --  TSD table indexed by Position.

   procedure Set_Interface_Table (T : Tag; Value : System.Address);
   --  Ada 2005 (AI-251): Given a pointer T to a dispatch Table, stores the
   --  pointer to the table of interfaces.

   procedure Set_Offset_Index
     (T        : Tag;
      Position : Positive;
      Value    : Positive);
   --  Ada 2005 (AI-345): Set the offset value of a primitive operation in a
   --  secondary dispatch table denoted by T, indexed by Position.

   procedure Set_Offset_To_Top
     (This         : System.Address;
      Interface_T  : Tag;
      Is_Static    : Boolean;
      Offset_Value : System.Storage_Elements.Storage_Offset;
      Offset_Func  : System.Address);
   --  Ada 2005 (AI-251): Initialize the Offset_To_Top field in the prologue of
   --  the dispatch table. In primary dispatch tables the value of "This" is
   --  not required (and the compiler passes always the Null_Address value) and
   --  the Offset_Value is always cero; in secondary dispatch tables "This"
   --  points to the object, Interface_T is the interface for which the
   --  secondary dispatch table is being initialized, and Offset_Value is the
   --  distance from "This" to the object component containing the tag of the
   --  secondary dispatch table.

   procedure Set_OSD (T : Tag; Value : System.Address);
   --  Ada 2005 (AI-251): Given a pointer T to a secondary dispatch table,
   --  store the pointer to the record containing the Object Specific Data
   --  generated by GNAT.

   procedure Set_Prim_Op_Kind
     (T        : Tag;
      Position : Positive;
      Value    : Prim_Op_Kind);
   --  Ada 2005 (AI-251): Set the kind of a primitive operation in T's TSD
   --  table indexed by Position.

   procedure Set_Signature (T : Tag; Value : Signature_Kind);
   --  Given a pointer T to a dispatch table, store the signature id

   procedure Set_SSD (T : Tag; Value : System.Address);
   --  Ada 2005 (AI-345): Given a pointer T to a dispatch Table, stores the
   --  pointer to the record containing the Select Specific Data generated by
   --  GNAT.

   procedure Set_Tagged_Kind (T : Tag; Value : Tagged_Kind);
   --  Ada 2005 (AI-345): Set the tagged kind of a type in either a primary or
   --  a secondary dispatch table denoted by T.

   function SSD (T : Tag) return Select_Specific_Data_Ptr;
   --  Ada 2005 (AI-251): Given a pointer T to a dispatch Table, retrieves the
   --  address of the record containing the Select Specific Data in T's TSD.

   function TSD (T : Tag) return Type_Specific_Data_Ptr;
   --  Given a pointer T to a dispatch Table, retrieves the address of the
   --  record containing the Type Specific Data generated by GNAT.

   --  Unchecked Conversions

   type Addr_Ptr is access System.Address;

   type Signature_Values is
      array (1 .. DT_Signature_Size) of Signature_Kind;
   --  Type used to see the signature as a sequence of Signature_Kind values

   type Signature_Values_Ptr is access all Signature_Values;

   function To_Addr_Ptr is
      new Unchecked_Conversion (System.Address, Addr_Ptr);

   function To_Type_Specific_Data_Ptr is
     new Unchecked_Conversion (System.Address, Type_Specific_Data_Ptr);

   function To_Address is
     new Unchecked_Conversion (Tag, System.Address);

   function To_Interface_Data_Ptr is
     new Unchecked_Conversion (System.Address, Interface_Data_Ptr);

   function To_Object_Specific_Data_Ptr is
     new Unchecked_Conversion (System.Address, Object_Specific_Data_Ptr);

   function To_Select_Specific_Data_Ptr is
     new Unchecked_Conversion (System.Address, Select_Specific_Data_Ptr);

   function To_Signature_Values is
     new Unchecked_Conversion (System.Storage_Elements.Storage_Offset,
                               Signature_Values);

   function To_Signature_Values_Ptr is
     new Unchecked_Conversion (System.Address,
                               Signature_Values_Ptr);

   function To_Tag is
     new Unchecked_Conversion (System.Address, Tag);

   function To_Tag_Ptr is
     new Unchecked_Conversion (System.Address, Tag_Ptr);

   function To_Tagged_Kind_Ptr is
     new Unchecked_Conversion (System.Address, Tagged_Kind_Ptr);

   --  Primitive dispatching operations are always inlined, to facilitate
   --  use in a minimal/no run-time environment for high integrity use.

   pragma Inline_Always (CW_Membership);
   pragma Inline_Always (Displace);
   pragma Inline_Always (IW_Membership);
   pragma Inline_Always (Get_Entry_Index);
   pragma Inline_Always (Get_Offset_Index);
   pragma Inline_Always (Get_Prim_Op_Kind);
   pragma Inline_Always (Get_Tagged_Kind);
   pragma Inline_Always (OSD);
   pragma Inline_Always (Register_Interface_Tag);
   pragma Inline_Always (Register_Tag);
   pragma Inline_Always (Set_Entry_Index);
   pragma Inline_Always (Set_Interface_Table);
   pragma Inline_Always (Set_Offset_Index);
   pragma Inline_Always (Set_Offset_To_Top);
   pragma Inline_Always (Set_Prim_Op_Kind);
   pragma Inline_Always (Set_Signature);
   pragma Inline_Always (Set_OSD);
   pragma Inline_Always (Set_SSD);
   pragma Inline_Always (Set_Tagged_Kind);
   pragma Inline_Always (SSD);
   pragma Inline_Always (TSD);

end Ada.Tags;
