------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                             A D A . T A G S                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

--  For performance analysis, take into account that the operations in this
--  package provide the guarantee that all dispatching calls on primitive
--  operations of tagged types and interfaces take constant time (in terms
--  of source lines executed), that is to say, the cost of these calls is
--  independent of the number of primitives of the type or interface, and
--  independent of the number of ancestors or interface progenitors that a
--  tagged type may have.

--  The following subprograms of the public part of this package take constant
--  time (in terms of source lines executed):

--    Expanded_Name, Wide_Expanded_Name, Wide_Wide_Expanded_Name, External_Tag,
--    Is_Abstract, Is_Descendant_At_Same_Level, Parent_Tag,
--    Descendant_Tag (when used with a library-level tagged type),
--    Internal_Tag (when used with a library-level tagged type).

--  The following subprograms of the public part of this package execute in
--  time that is not constant (in terms of sources line executed):

--    Internal_Tag (when used with a locally defined tagged type), because in
--    such cases this routine processes the external tag, extracts from it an
--    address available there, and converts it into the tag value returned by
--    this function. The number of instructions executed is not constant since
--    it depends on the length of the external tag string.

--    Descendant_Tag (when used with a locally defined tagged type), because
--    it relies on the subprogram Internal_Tag() to provide its functionality.

--    Interface_Ancestor_Tags, because this function returns a table whose
--    length depends on the number of interfaces covered by a tagged type.

with System.Storage_Elements;

package Ada.Tags is
   pragma Preelaborate;
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

   function Is_Abstract (T : Tag) return Boolean;
   pragma Ada_2012 (Is_Abstract);

   Tag_Error : exception;

private
   --  Structure of the GNAT Primary Dispatch Table

   --           +--------------------+
   --           |      Signature     |
   --           +--------------------+
   --           |     Tagged_Kind    |
   --           +--------------------+                            Predef Prims
   --           |    Predef_Prims -----------------------------> +------------+
   --           +--------------------+                           |  table of  |
   --           |    Offset_To_Top   |                           | predefined |
   --           +--------------------+                           | primitives |
   --           |Typeinfo_Ptr/TSD_Ptr---> Type Specific Data     +------------+
   --  Tag ---> +--------------------+   +-------------------+
   --           |      table of      |   | inheritance depth |
   --           :   primitive ops    :   +-------------------+
   --           |      pointers      |   |   access level    |
   --           +--------------------+   +-------------------+
   --                                    |     alignment     |
   --                                    +-------------------+
   --                                    |   expanded name   |
   --                                    +-------------------+
   --                                    |   external tag    |
   --                                    +-------------------+
   --                                    |   hash table link |
   --                                    +-------------------+
   --                                    |   transportable   |
   --                                    +-------------------+
   --                                    |    is_abstract    |
   --                                    +-------------------+
   --                                    | needs finalization|
   --                                    +-------------------+
   --                                    |   Ifaces_Table   ---> Interface Data
   --                                    +-------------------+   +------------+
   --         Select Specific Data  <----        SSD         |   |  Nb_Ifaces |
   --         +------------------+       +-------------------+   +------------+
   --         |table of primitive|       | table of          |   |  table     |
   --         :   operation      :       :    ancestor       :   :    of      :
   --         |      kinds       |       |       tags        |   | interfaces |
   --         +------------------+       +-------------------+   +------------+
   --         |table of          |
   --         :   entry          :
   --         |      indexes     |
   --         +------------------+

   --  Structure of the GNAT Secondary Dispatch Table

   --           +--------------------+
   --           |      Signature     |
   --           +--------------------+
   --           |     Tagged_Kind    |
   --           +--------------------+                            Predef Prims
   --           |    Predef_Prims -----------------------------> +------------+
   --           +--------------------+                           |  table of  |
   --           |    Offset_To_Top   |                           | predefined |
   --           +--------------------+                           | primitives |
   --           |       OSD_Ptr      |---> Object Specific Data  |   thunks   |
   --  Tag ---> +--------------------+      +---------------+    +------------+
   --           |      table of      |      | num prim ops  |
   --           :    primitive op    :      +---------------+
   --           |   thunk pointers   |      | table of      |
   --           +--------------------+      +   primitive   |
   --                                       |    op offsets |
   --                                       +---------------+

   --  The runtime information kept for each tagged type is separated into two
   --  objects: the Dispatch Table and the Type Specific Data record.

   package SSE renames System.Storage_Elements;

   subtype Cstring is String (Positive);
   type Cstring_Ptr is access all Cstring;
   pragma No_Strict_Aliasing (Cstring_Ptr);

   --  Declarations for the table of interfaces

   type Offset_To_Top_Function_Ptr is
     access function (This : System.Address) return SSE.Storage_Offset;
   --  Type definition used to call the function that is generated by the
   --  expander in case of tagged types with discriminants that have secondary
   --  dispatch tables. This function provides the Offset_To_Top value in this
   --  specific case.

   type Interface_Data_Element is record
      Iface_Tag            : Tag;
      Static_Offset_To_Top : Boolean;
      Offset_To_Top_Value  : SSE.Storage_Offset;
      Offset_To_Top_Func   : Offset_To_Top_Function_Ptr;
      Secondary_DT         : Tag;
   end record;
   --  If some ancestor of the tagged type has discriminants the field
   --  Static_Offset_To_Top is False and the field Offset_To_Top_Func
   --  is used to store the access to the function generated by the
   --  expander which provides this value; otherwise Static_Offset_To_Top
   --  is True and such value is stored in the Offset_To_Top_Value field.
   --  Secondary_DT references a secondary dispatch table whose contents
   --  are pointers to the primitives of the tagged type that cover the
   --  interface primitives. Secondary_DT gives support to dispatching
   --  calls through interface types associated with Generic Dispatching
   --  Constructors.

   type Interfaces_Array is array (Natural range <>) of Interface_Data_Element;

   type Interface_Data (Nb_Ifaces : Positive) is record
      Ifaces_Table : Interfaces_Array (1 .. Nb_Ifaces);
   end record;

   type Interface_Data_Ptr is access all Interface_Data;
   --  Table of abstract interfaces used to give support to backward interface
   --  conversions and also to IW_Membership.

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

   type Select_Specific_Data_Ptr is access all Select_Specific_Data;
   --  A table used to store the primitive operation kind and entry index of
   --  primitive subprograms of a type that implements a limited interface.
   --  The Select Specific Data table resides in the Type Specific Data of a
   --  type. This construct is used in the handling of dispatching triggers
   --  in select statements.

   type Prim_Ptr is access procedure;
   type Address_Array is array (Positive range <>) of Prim_Ptr;

   subtype Dispatch_Table is Address_Array (1 .. 1);
   --  Used by GDB to identify the _tags and traverse the run-time structure
   --  associated with tagged types. For compatibility with older versions of
   --  gdb, its name must not be changed.

   type Tag is access all Dispatch_Table;
   pragma No_Strict_Aliasing (Tag);

   type Interface_Tag is access all Dispatch_Table;

   No_Tag : constant Tag := null;

   --  The expander ensures that Tag objects reference the Prims_Ptr component
   --  of the wrapper.

   type Tag_Ptr is access all Tag;
   pragma No_Strict_Aliasing (Tag_Ptr);

   type Offset_To_Top_Ptr is access all SSE.Storage_Offset;
   pragma No_Strict_Aliasing (Offset_To_Top_Ptr);

   type Tag_Table is array (Natural range <>) of Tag;

   type Size_Ptr is
     access function (A : System.Address) return Long_Long_Integer;

   type Type_Specific_Data (Idepth : Natural) is record
   --  The discriminant Idepth is the Inheritance Depth Level: Used to
   --  implement the membership test associated with single inheritance of
   --  tagged types in constant-time. It also indicates the size of the
   --  Tags_Table component.

      Access_Level : Natural;
      --  Accessibility level required to give support to Ada 2005 nested type
      --  extensions. This feature allows safe nested type extensions by
      --  shifting the accessibility checks to certain operations, rather than
      --  being enforced at the type declaration. In particular, by performing
      --  run-time accessibility checks on class-wide allocators, class-wide
      --  function return, and class-wide stream I/O, the danger of objects
      --  outliving their type declaration can be eliminated (Ada 2005: AI-344)

      Alignment     : Natural;
      Expanded_Name : Cstring_Ptr;
      External_Tag  : Cstring_Ptr;
      HT_Link       : Tag_Ptr;
      --  Components used to support to the Ada.Tags subprograms in RM 3.9

      --  Note: Expanded_Name is referenced by GDB to determine the actual name
      --  of the tagged type. Its requirements are: 1) it must have this exact
      --  name, and 2) its contents must point to a C-style Nul terminated
      --  string containing its expanded name. GDB has no requirement on a
      --  given position inside the record.

      Transportable : Boolean;
      --  Used to check RM E.4(18), set for types that satisfy the requirements
      --  for being used in remote calls as actuals for classwide formals or as
      --  return values for classwide functions.

      Is_Abstract : Boolean;
      --  True if the type is abstract (Ada 2012: AI05-0173)

      Needs_Finalization : Boolean;
      --  Used to dynamically check whether an object is controlled or not

      Size_Func : Size_Ptr;
      --  Pointer to the subprogram computing the _size of the object. Used by
      --  the run-time whenever a call to the 'size primitive is required. We
      --  cannot assume that the contents of dispatch tables are addresses
      --  because in some architectures the ABI allows descriptors.

      Interfaces_Table : Interface_Data_Ptr;
      --  Pointer to the table of interface tags. It is used to implement the
      --  membership test associated with interfaces and also for backward
      --  abstract interface type conversions (Ada 2005:AI-251)

      SSD : Select_Specific_Data_Ptr;
      --  Pointer to a table of records used in dispatching selects. This field
      --  has a meaningful value for all tagged types that implement a limited,
      --  protected, synchronized or task interfaces and have non-predefined
      --  primitive operations.

      Tags_Table : Tag_Table (0 .. Idepth);
      --  Table of ancestor tags. Its size actually depends on the inheritance
      --  depth level of the tagged type.
   end record;

   type Type_Specific_Data_Ptr is access all Type_Specific_Data;
   pragma No_Strict_Aliasing (Type_Specific_Data_Ptr);

   --  Declarations for the dispatch table record

   type Signature_Kind is
      (Unknown,
       Primary_DT,
       Secondary_DT);

   --  Tagged type kinds with respect to concurrency and limitedness

   type Tagged_Kind is
     (TK_Abstract_Limited_Tagged,
      TK_Abstract_Tagged,
      TK_Limited_Tagged,
      TK_Protected,
      TK_Tagged,
      TK_Task);

   type Dispatch_Table_Wrapper (Num_Prims : Natural) is record
      Signature     : Signature_Kind;
      Tag_Kind      : Tagged_Kind;
      Predef_Prims  : System.Address;
      --  Pointer to the dispatch table of predefined Ada primitives

      --  According to the C++ ABI the components Offset_To_Top and TSD are
      --  stored just "before" the dispatch table, and they are referenced with
      --  negative offsets referring to the base of the dispatch table. The
      --   _Tag (or the VTable_Ptr in C++ terminology) must point to the base
      --  of the virtual table, just after these components, to point to the
      --  Prims_Ptr table.

      Offset_To_Top : SSE.Storage_Offset;
      --  Offset between the _Tag field and the field that contains the
      --  reference to this dispatch table. For primary dispatch tables it is
      --  zero. For secondary dispatch tables: if the parent record type (if
      --  any) has a compile-time-known size, then Offset_To_Top contains the
      --  expected value, otherwise it contains SSE.Storage_Offset'Last and the
      --  actual offset is to be found in the tagged record, right after the
      --  field that contains the reference to this dispatch table. See the
      --  implementation of Ada.Tags.Offset_To_Top for the corresponding logic.

      TSD : System.Address;

      Prims_Ptr : aliased Address_Array (1 .. Num_Prims);
      --  The size of the Prims_Ptr array actually depends on the tagged type
      --  to which it applies. For each tagged type, the expander computes the
      --  actual array size, allocating the Dispatch_Table record accordingly.
   end record;

   type Dispatch_Table_Ptr is access all Dispatch_Table_Wrapper;
   pragma No_Strict_Aliasing (Dispatch_Table_Ptr);

   --  The following type declaration is used by the compiler when the program
   --  is compiled with restriction No_Dispatching_Calls. It is also used with
   --  interface types to generate the tag and run-time information associated
   --  with them.

   type No_Dispatch_Table_Wrapper is record
      NDT_TSD       : System.Address;
      NDT_Prims_Ptr : Natural;
   end record;

   DT_Predef_Prims_Size : constant SSE.Storage_Count :=
                            SSE.Storage_Count
                              (1 * (Standard'Address_Size /
                                      System.Storage_Unit));
   --  Size of the Predef_Prims field of the Dispatch_Table

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

   use type System.Storage_Elements.Storage_Offset;

   DT_Offset_To_Top_Offset : constant SSE.Storage_Count :=
                               DT_Typeinfo_Ptr_Size
                                 + DT_Offset_To_Top_Size;

   DT_Predef_Prims_Offset : constant SSE.Storage_Count :=
                              DT_Typeinfo_Ptr_Size
                                + DT_Offset_To_Top_Size
                                + DT_Predef_Prims_Size;
   --  Offset from Prims_Ptr to Predef_Prims component

   --  Object Specific Data record of secondary dispatch tables

   type Object_Specific_Data_Array is array (Positive range <>) of Positive;

   type Object_Specific_Data (OSD_Num_Prims : Positive) is record
      OSD_Table : Object_Specific_Data_Array (1 .. OSD_Num_Prims);
      --  Table used in secondary DT to reference their counterpart in the
      --  select specific data (in the TSD of the primary DT). This construct
      --  is used in the handling of dispatching triggers in select statements.
      --  Nb_Prim is the number of non-predefined primitive operations.
   end record;

   type Object_Specific_Data_Ptr is access all Object_Specific_Data;
   pragma No_Strict_Aliasing (Object_Specific_Data_Ptr);

   --  The following subprogram specifications are placed here instead of the
   --  package body to see them from the frontend through rtsfind.

   function Base_Address (This : System.Address) return System.Address;
   --  Ada 2005 (AI-251): Displace "This" to point to the base address of the
   --  object (that is, the address of the primary tag of the object).

   procedure Check_TSD (TSD : Type_Specific_Data_Ptr);
   --  Ada 2012 (AI-113): Raise Program_Error if the external tag of this TSD
   --  is the same as the external tag for some other tagged type declaration.

   function Displace (This : System.Address; T : Tag) return System.Address;
   --  Ada 2005 (AI-251): Displace "This" to point to the secondary dispatch
   --  table of T.

   function Secondary_Tag (T, Iface : Tag) return Tag;
   --  Ada 2005 (AI-251): Given a primary tag T associated with a tagged type
   --  Typ, search for the secondary tag of the interface type Iface covered
   --  by Typ.

   function DT (T : Tag) return Dispatch_Table_Ptr;
   --  Return the pointer to the TSD record associated with T

   function Get_Entry_Index (T : Tag; Position : Positive) return Positive;
   --  Ada 2005 (AI-251): Return a primitive operation's entry index (if entry)
   --  given a dispatch table T and a position of a primitive operation in T.

   function Get_Offset_Index
     (T        : Tag;
      Position : Positive) return Positive;
   --  Ada 2005 (AI-251): Given a pointer to a secondary dispatch table (T)
   --  and a position of an operation in the DT, retrieve the corresponding
   --  operation's position in the primary dispatch table from the Offset
   --  Specific Data table of T.

   function Get_Prim_Op_Kind
     (T        : Tag;
      Position : Positive) return Prim_Op_Kind;
   --  Ada 2005 (AI-251): Return a primitive operation's kind given a dispatch
   --  table T and a position of a primitive operation in T.

   function Get_Tagged_Kind (T : Tag) return Tagged_Kind;
   --  Ada 2005 (AI-345): Given a pointer to either a primary or a secondary
   --  dispatch table, return the tagged kind of a type in the context of
   --  concurrency and limitedness.

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
   --      function Test (O : I'Class) is
   --      begin
   --         return O in T'Class.
   --      end Test;

   function Offset_To_Top
     (This : System.Address) return SSE.Storage_Offset;
   --  Ada 2005 (AI-251): Returns the current value of the Offset_To_Top
   --  component available in the prologue of the dispatch table. If the parent
   --  of the tagged type has discriminants this value is stored in a record
   --  component just immediately after the tag component.

   function Needs_Finalization (T : Tag) return Boolean;
   --  A helper routine used in conjunction with finalization collections which
   --  service class-wide types. The function dynamically determines whether an
   --  object is controlled or has controlled components.

   function Parent_Size
     (Obj : System.Address;
      T   : Tag) return SSE.Storage_Count;
   --  Computes the size the ancestor part of a tagged extension object whose
   --  address is 'obj' by calling indirectly the ancestor _size function. The
   --  ancestor is the parent of the type represented by tag T. This function
   --  assumes that _size is always in slot one of the dispatch table.

   procedure Register_Interface_Offset
     (Prim_T       : Tag;
      Interface_T  : Tag;
      Is_Static    : Boolean;
      Offset_Value : SSE.Storage_Offset;
      Offset_Func  : Offset_To_Top_Function_Ptr);
   --  Register in the table of interfaces of the tagged type associated with
   --  Prim_T the offset of the record component associated with the progenitor
   --  Interface_T (that is, the distance from "This" to the object component
   --  containing the tag of the secondary dispatch table). In case of constant
   --  offset, Is_Static is true and Offset_Value has such value. In case of
   --  variable offset, Is_Static is false and Offset_Func is an access to
   --  function that must be called to evaluate the offset.

   procedure Register_Tag (T : Tag);
   --  Insert the Tag and its associated external_tag in a table for the sake
   --  of Internal_Tag.

   procedure Set_Dynamic_Offset_To_Top
     (This         : System.Address;
      Prim_T       : Tag;
      Interface_T  : Tag;
      Offset_Value : SSE.Storage_Offset;
      Offset_Func  : Offset_To_Top_Function_Ptr);
   --  Ada 2005 (AI-251): The compiler generates calls to this routine only
   --  when initializing the Offset_To_Top field of dispatch tables of tagged
   --  types that cover interface types whose parent type has variable size
   --  components.
   --
   --  "This" is the object whose dispatch table is being initialized. Prim_T
   --  is the primary tag of such object. Interface_T is the interface tag for
   --  which the secondary dispatch table is being initialized. Offset_Value
   --  is the distance from "This" to the object component containing the tag
   --  of the secondary dispatch table (a zero value means that this interface
   --  shares the primary dispatch table). Offset_Func references a function
   --  that must be called to evaluate the offset at run time. This routine
   --  also takes care of registering these values in the table of interfaces
   --  of the type.

   procedure Set_Entry_Index (T : Tag; Position : Positive; Value : Positive);
   --  Ada 2005 (AI-345): Set the entry index of a primitive operation in T's
   --  TSD table indexed by Position.

   procedure Set_Prim_Op_Kind
     (T        : Tag;
      Position : Positive;
      Value    : Prim_Op_Kind);
   --  Ada 2005 (AI-251): Set the kind of a primitive operation in T's TSD
   --  table indexed by Position.

   procedure Unregister_Tag (T : Tag);
   --  Remove a particular tag from the external tag hash table

   Max_Predef_Prims : constant Positive := 16;
   --  Number of reserved slots for the following predefined ada primitives:
   --
   --    1. Size
   --    2. Read
   --    3. Write
   --    4. Input
   --    5. Output
   --    6. "="
   --    7. assignment
   --    8. deep adjust
   --    9. deep finalize
   --   10. Put_Image
   --   11. async select
   --   12. conditional select
   --   13. prim_op kind
   --   14. task_id
   --   15. dispatching requeue
   --   16. timed select
   --
   --  The compiler checks that the value here is correct

   subtype Predef_Prims_Table  is Address_Array (1 .. Max_Predef_Prims);
   type Predef_Prims_Table_Ptr is access Predef_Prims_Table;
   pragma No_Strict_Aliasing (Predef_Prims_Table_Ptr);

   type Addr_Ptr is access System.Address;
   pragma No_Strict_Aliasing (Addr_Ptr);
   --  This type is used by the frontend to generate the code that handles
   --  dispatch table slots of types declared at the local level.

end Ada.Tags;
