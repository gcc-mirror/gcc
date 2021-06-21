------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          G E N _ I L . U T I L S                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2020-2021, Free Software Foundation, Inc.         --
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

with Ada.Containers.Vectors; use Ada.Containers;

with GNAT.Strings; use GNAT.Strings;

with Gen_IL.Types;  use Gen_IL.Types;
with Gen_IL.Fields; use Gen_IL.Fields;

package Gen_IL.Internals is

   function Image (T : Opt_Type_Enum) return String;

   function Image_Sans_N (T : Opt_Type_Enum) return String;
   --  Returns the image without the leading "N_"

   ----------------

   type Type_Set is array (Type_Enum) of Boolean;

   type Type_Index is new Positive;
   subtype Type_Count is Type_Index'Base range 0 .. Type_Index'Last;
   package Type_Vectors is new Vectors (Type_Index, Type_Enum);
   use Type_Vectors;
   subtype Type_Vector is Type_Vectors.Vector;

   type Type_Array is array (Type_Index range <>) of Type_Enum;

   ----------------

   procedure Put_Types_With_Bars (S : in out Sink; U : Type_Vector);
   procedure Put_Type_Ids_With_Bars (S : in out Sink; U : Type_Vector);
   --  Put the types with vertical bars in between, as in
   --     N_This | N_That | N_Other
   --  or
   --     N_This_Id | N_That_Id | N_Other_Id

   function Id_Image (T : Type_Enum) return String;
   --  Image of the type for use with _Id types

   function Get_Set_Id_Image (T : Type_Enum) return String;
   --  Image of the type for use with getters and setters

   ----------------

   type Fields_Present_Array is array (Field_Enum) of Type_Set;

   type Field_Set is array (Field_Enum) of Boolean;
   type Fields_Per_Node_Type is array (Node_Or_Entity_Type) of Field_Set;

   type Field_Index is new Positive;
   package Field_Vectors is new Vectors (Field_Index, Field_Enum);
   subtype Field_Vector is Field_Vectors.Vector;

   type Bit_Offset is new Root_Nat range 0 .. 32_000 - 1;
   --  Offset in bits. The number 32_000 is chosen because there are fewer than
   --  1000 fields, but offsets are in size units (1 bit for flags, 32 bits for
   --  most others, also 2, 4, and 8).

   type Field_Offset is new Bit_Offset;

   type Type_Info (Is_Union : Boolean) is record
      Parent : Opt_Abstract_Type;
      --  Parent of this type (single inheritance). No_Type for a root
      --  type (Node_Kind or Entity_Kind). For union types, this is
      --  a root type.

      Children : Type_Vector;
      --  Inverse of Parent

      Concrete_Descendants : Type_Vector;

      case Is_Union is
         when True =>
            null;

         when False =>
            First, Last : Concrete_Type;
            --  This type includes concrete types in the range First..Last. For
            --  a concrete type, First=Last. For an abstract type, First..Last
            --  includes two or more types.

            Fields : Field_Vector;
      end case;
   end record;

   type Type_Info_Ptr is access all Type_Info;

   Type_Table : array (Node_Or_Entity_Type) of Type_Info_Ptr;
   --  Table mapping from enumeration literals representing types to
   --  information about the type.

   procedure Verify_Type_Table;
   --  Check Type_Table for consistency

   function Num_Concrete_Descendants
     (T : Node_Or_Entity_Type) return Natural;
   --  Number of concrete descendants of T, including (if T is concrete)
   --  itself.

   type Field_Default_Value is
     (No_Default,
      Default_Empty, -- Node_Id
      Default_No_List, Default_Empty_List, -- List_Id
      Default_False, Default_True, -- Flag
      Default_No_Elist, -- Elist_Id
      Default_No_Name, -- Name_Id
      Default_Uint_0); -- Uint
   --  Default value for a field in the Nmake functions. No_Default if the
   --  field parameter has no default value. Otherwise this indicates the
   --  default value used, which must matcht the type of the field.

   function Image (Default : Field_Default_Value) return String;
   --  This will be something like "Default_Empty".
   function Value_Image (Default : Field_Default_Value) return String;
   --  This will be something like "Empty".

   type Type_Only_Enum is
     (No_Type_Only, Base_Type_Only, Impl_Base_Type_Only, Root_Type_Only);
   --  These correspond to the "[base type only]", "[implementation base type
   --  only]", and "[root type only]" annotations documented in einfo.ads.
   --  The default is No_Type_Only, indicating the field is not one of
   --  these special "[... only]" ones.

   type Field_Info is record
      Have_This_Field : Type_Vector;
      --  Types that have this field

      Field_Type      : Type_Enum;
      --  Type of the field. Currently, we use Node_Id for all node-valued
      --  fields, but we could narrow down to children of that. Similar for
      --  Entity_Id.

      Default_Value   : Field_Default_Value;
      Type_Only       : Type_Only_Enum;
      Pre, Pre_Get, Pre_Set : String_Access;
      --  Above record the information in the calls to Create_...Field.
      --  See Gen_IL.Gen for details.

      Offset          : Field_Offset;
      --  Offset of the field from the start of the node, in units of the field
      --  size. So if a field is 4 bits in size, it starts at bit number
      --  Offset*4 from the start of the node.
   end record;

   type Field_Info_Ptr is access all Field_Info;

   Field_Table : array (Field_Enum) of Field_Info_Ptr;
   --  Table mapping from enumeration literals representing fields to
   --  information about the field.

   ----------------

   subtype Node_Field is
     Field_Enum range
       Field_Enum'First ..
         Field_Enum'Pred (Between_Node_And_Entity_Fields);

   subtype Entity_Field is
     Field_Enum range
       Field_Enum'Succ (Between_Node_And_Entity_Fields) ..
         Field_Enum'Last;

   function Image (F : Opt_Field_Enum) return String;

   function F_Image (F : Opt_Field_Enum) return String is
     ("F_" & Image (F));
   --  Prepends "F_" to Image (F). This is used for the enumeration literals in
   --  the generated Sinfo.Nodes.Node_Field and Einfo.Entities.Entity_Field
   --  types. If we used Image (F), these enumeration literals would overload
   --  the getter functions, which confuses gdb.

   procedure Nil (T : Node_Or_Entity_Type);
   --  Null procedure

   procedure Iterate_Types
     (Root  : Node_Or_Entity_Type;
      Pre, Post : not null access procedure (T : Node_Or_Entity_Type) :=
        Nil'Access);
   --  Iterate top-down on the type hierarchy. Call Pre and Post before and
   --  after walking child types. Note that this ignores union types, because
   --  they are nonhierarchical.

   function Is_Descendant (Ancestor, Descendant : Node_Or_Entity_Type)
     return Boolean;
   --  True if Descendant is a descendant of Ancestor; that is,
   --  True if Ancestor is an ancestor of Descendant. True for
   --  a type itself.

   procedure Put_Type_Hierarchy (S : in out Sink; Root : Root_Type);

   function Pos (T : Concrete_Type) return Root_Nat;
   --  Return Node_Kind'Pos (T) or Entity_Kind'Pos (T)

   ----------------

   type Field_Desc is record
      F : Field_Enum;
      Is_Syntactic : Boolean;
      --  The same field can be syntactic in some nodes but semantic in others
   end record;

   type Field_Sequence_Index is new Positive;
   type Field_Sequence is array (Field_Sequence_Index range <>) of Field_Desc;
   No_Fields : constant Field_Sequence := (1 .. 0 => <>);

   type Field_Array is array (Bit_Offset range <>) of Opt_Field_Enum;
   type Field_Array_Ptr is access all Field_Array;

   type Concrete_Type_Layout_Array is array (Concrete_Type) of Field_Array_Ptr;
   --  Mapping from types to mappings from offsets to fields. Each bit offset
   --  is mapped to the corresponding field for the given type. An n-bit field
   --  will have n bit offsets mapped to the same field.

   type Offset_To_Fields_Mapping is
     array (Bit_Offset range <>) of Field_Array_Ptr;
   --  Mapping from bit offsets to fields using that offset

   function First_Abstract (Root : Root_Type) return Abstract_Type;
   function Last_Abstract (Root : Root_Type) return Abstract_Type;
   --  First and Last abstract types descended from the Root. So for example if
   --  Root = Node_Kind, then First_Abstract = Abstract_Node'First.

   function First_Concrete (Root : Root_Type) return Concrete_Type;
   function Last_Concrete (Root : Root_Type) return Concrete_Type;
   --  First and Last concrete types descended from the Root

   function First_Field (Root : Root_Type) return Field_Enum;
   function Last_Field (Root : Root_Type) return Field_Enum;
   --  First and Last node or entity fields

   function Node_Or_Entity (Root : Root_Type) return String;
   --  Return "Node" or "Entity" depending on whether Root = Node_Kind or
   --  Entity_Kind.

end Gen_IL.Internals;
