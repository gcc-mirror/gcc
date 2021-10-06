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

            Nmake_Assert : String_Access; -- only for concrete node types
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
   --  default value used, which must match the type of the field.

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

   Unknown_Offset : constant := -1;
   --  Initial value of Offset, so we can tell whether it has been set

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

      Offset : Field_Offset'Base range Unknown_Offset .. Field_Offset'Last;
      --  Offset of the field from the start of the node, in units of the field
      --  size. So if a field is 4 bits in size, it starts at bit number
      --  Offset*4 from the start of the node.
   end record;

   type Field_Info_Ptr is access all Field_Info;

   Field_Table : array (Field_Enum) of Field_Info_Ptr;
   --  Table mapping from enumeration literals representing fields to
   --  information about the field.

   --  Getters for fields of types Elist_Id and Uint need special treatment of
   --  defaults. In particular, if the field has its initial 0 value, getters
   --  need to return the appropriate default value. Note that these defaults
   --  have nothing to do with the defaults mentioned above for Nmake
   --  functions.

   function Field_Has_Special_Default
     (Field_Type : Type_Enum) return Boolean is
      (Field_Type in Elist_Id | Uint);
   --  These are the field types that have a default value that is not
   --  represented as zero.

   function Special_Default
     (Field_Type : Type_Enum) return String is
      (case Field_Type is
         when Elist_Id => "No_Elist",
         when Uint => "No_Uint",
         when others => "can't happen");

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
   --  they are nonhierarchical. The order in which concrete types are visited
   --  matches the order of the generated enumeration types Node_Kind and
   --  Entity_Kind, which is not the same as the order of the Type_Enum
   --  type in Gen_IL.Types.

   function Is_Descendant (Ancestor, Descendant : Node_Or_Entity_Type)
     return Boolean;
   --  True if Descendant is a descendant of Ancestor; that is,
   --  True if Ancestor is an ancestor of Descendant. True for
   --  a type itself.

   procedure Put_Type_Hierarchy (S : in out Sink; Root : Root_Type);

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

   pragma Style_Checks (Off);
   --  We don't want warnings about wrong casing in the Type_Frequency table;
   --  this table is not intended to be particularly readable.

   --  The Type_Frequency table shows the frequency of nodes and entity kinds
   --  printed by -gnatd.A for a large example. It is used in the field offset
   --  computations for efficiency. Note that N_Defining_Identifier,
   --  N_Defining_Operator_Symbol, and N_Defining_Character_Literal are set to
   --  zero, because the Ekind is what matters for those.

   Type_Frequency : constant array (Concrete_Type) of Type_Count :=
     (N_Identifier => 3496964, -- (0.354) 7 slots
      N_Defining_Identifier => 0, -- 1468484, -- (0.149) 8 slots
      N_Integer_Literal => 455415, -- (0.046) 6 slots
      E_In_Parameter => 391008, -- (0.040) 42 slots
      N_Attribute_Reference => 330825, -- (0.033) 9 slots
      N_Expanded_Name => 329509, -- (0.033) 8 slots
      N_Selected_Component => 328862, -- (0.033) 8 slots
      N_Parameter_Specification => 321313, -- (0.033) 7 slots
      E_Void => 173019, -- (0.018) 59 slots
      N_Explicit_Dereference => 155113, -- (0.016) 8 slots
      N_Procedure_Call_Statement => 125403, -- (0.013) 8 slots
      N_Object_Declaration => 115610, -- (0.012) 8 slots
      E_Component => 108208, -- (0.011) 49 slots
      N_Procedure_Specification => 106277, -- (0.011) 7 slots
      E_Procedure => 104063, -- (0.011) 62 slots
      N_Unchecked_Type_Conversion => 94477, -- (0.010) 7 slots
      N_Range => 91413, -- (0.009) 6 slots
      E_Function => 90035, -- (0.009) 62 slots
      N_Handled_Sequence_Of_Statements => 87930, -- (0.009) 8 slots
      N_Subprogram_Declaration => 85248, -- (0.009) 7 slots
      N_Parameter_Association => 81464, -- (0.008) 8 slots
      N_Indexed_Component => 80049, -- (0.008) 7 slots
      N_Freeze_Entity => 79904, -- (0.008) 8 slots
      N_Call_Marker => 79521, -- (0.008) 4 slots
      N_Assignment_Statement => 76554, -- (0.008) 8 slots
      N_Function_Specification => 76052, -- (0.008) 7 slots
      N_Function_Call => 75028, -- (0.008) 9 slots
      N_Op_Eq => 74874, -- (0.008) 8 slots
      E_Constant => 66667, -- (0.007) 47 slots
      N_If_Statement => 60066, -- (0.006) 8 slots
      N_Component_Association => 54642, -- (0.006) 7 slots
      N_Subprogram_Body => 53805, -- (0.005) 10 slots
      N_Type_Conversion => 53383, -- (0.005) 7 slots
      E_In_Out_Parameter => 52936, -- (0.005) 38 slots
      N_Simple_Return_Statement => 52436, -- (0.005) 7 slots
      N_Subtype_Indication => 49535, -- (0.005) 6 slots
      N_Raise_Constraint_Error => 49069, -- (0.005) 6 slots
      N_Null => 46850, -- (0.005) 5 slots
      N_Itype_Reference => 45422, -- (0.005) 4 slots
      E_Anonymous_Access_Type => 45149, -- (0.005) 44 slots
      N_And_Then => 44721, -- (0.005) 8 slots
      N_Block_Statement => 44328, -- (0.004) 10 slots
      N_Subtype_Declaration => 43149, -- (0.004) 6 slots
      N_Op_Not => 40531, -- (0.004) 7 slots
      E_Array_Subtype => 40051, -- (0.004) 50 slots
      N_Expression_With_Actions => 36726, -- (0.004) 7 slots
      E_Access_Subprogram_Type => 36700, -- (0.004) 45 slots
      E_Signed_Integer_Subtype => 36659, -- (0.004) 43 slots
      N_String_Literal => 34815, -- (0.004) 7 slots
      N_Aggregate => 33899, -- (0.003) 8 slots
      N_Index_Or_Discriminant_Constraint => 33546, -- (0.003) 4 slots
      E_Variable => 33102, -- (0.003) 55 slots
      E_Block => 32829, -- (0.003) 58 slots
      N_Op_Ne => 32127, -- (0.003) 8 slots
      N_Pragma_Argument_Association => 31504, -- (0.003) 7 slots
      N_Null_Statement => 30816, -- (0.003) 5 slots
      N_Aspect_Specification => 29667, -- (0.003) 9 slots
      N_Pragma => 28317, -- (0.003) 9 slots
      N_Generic_Association => 26297, -- (0.003) 8 slots
      N_Formal_Concrete_Subprogram_Declaration => 25843, -- (0.003) 6 slots
      N_Op_Lt => 25328, -- (0.003) 8 slots
      E_String_Literal_Subtype => 25272, -- (0.003) 48 slots
      N_Full_Type_Declaration => 25258, -- (0.003) 7 slots
      N_With_Clause => 24370, -- (0.002) 9 slots
      N_Op_Add => 23839, -- (0.002) 8 slots
      E_Subprogram_Body => 23790, -- (0.002) 42 slots
      E_Return_Statement => 23098, -- (0.002) 51 slots
      N_Or_Else => 22858, -- (0.002) 8 slots
      N_Implicit_Label_Declaration => 21687, -- (0.002) 5 slots
      N_Others_Choice => 21579, -- (0.002) 4 slots
      E_Out_Parameter => 21513, -- (0.002) 38 slots
      N_Op_Subtract => 21441, -- (0.002) 8 slots
      N_Op_Ge => 21116, -- (0.002) 8 slots
      N_Component_Definition => 21075, -- (0.002) 7 slots
      N_Case_Statement_Alternative => 19664, -- (0.002) 8 slots
      N_Loop_Statement => 19507, -- (0.002) 9 slots
      E_Package => 19029, -- (0.002) 53 slots
      N_Op_Gt => 18619, -- (0.002) 8 slots
      N_Op_Le => 16564, -- (0.002) 8 slots
      N_Formal_Object_Declaration => 16219, -- (0.002) 7 slots
      E_Discriminant => 16091, -- (0.002) 56 slots
      N_Component_Declaration => 15858, -- (0.002) 7 slots
      N_Iteration_Scheme => 15719, -- (0.002) 8 slots
      N_Access_To_Object_Definition => 14875, -- (0.002) 5 slots
      E_Record_Subtype => 14569, -- (0.001) 52 slots
      N_Generic_Subprogram_Declaration => 14320, -- (0.001) 7 slots
      N_Package_Specification => 13323, -- (0.001) 8 slots
      N_Exception_Handler => 12841, -- (0.001) 8 slots
      E_Enumeration_Literal => 11608, -- (0.001) 42 slots
      N_Subprogram_Renaming_Declaration => 10991, -- (0.001) 9 slots
      N_In => 10794, -- (0.001) 8 slots
      E_Allocator_Type => 10751, -- (0.001) 44 slots
      E_General_Access_Type => 10451, -- (0.001) 44 slots
      E_Generic_Procedure => 9837, -- (0.001) 41 slots
      N_Package_Renaming_Declaration => 9395, -- (0.001) 8 slots
      N_Access_Definition => 9388, -- (0.001) 6 slots
      N_Qualified_Expression => 9012, -- (0.001) 7 slots
      E_Enumeration_Subtype => 8560, -- (0.001) 46 slots
      N_Allocator => 8474, -- (0.001) 8 slots
      N_Package_Declaration => 8099, -- (0.001) 10 slots
      N_Formal_Type_Declaration => 7964, -- (0.001) 7 slots
      N_Exit_Statement => 7960, -- (0.001) 8 slots
      N_Component_List => 7829, -- (0.001) 5 slots
      N_Defining_Operator_Symbol => 0, -- 7525, -- (0.001) 8 slots
      N_Case_Statement => 7271, -- (0.001) 7 slots
      N_Expression_Function => 7242, -- (0.001) 9 slots
      N_Loop_Parameter_Specification => 7042, -- (0.001) 7 slots
      N_Character_Literal => 6842, -- (0.001) 7 slots
      N_Op_Concat => 6565, -- (0.001) 8 slots
      N_Not_In => 6341, -- (0.001) 8 slots
      N_Label => 6133, -- (0.001) 9 slots
      N_Goto_Statement => 6133, -- (0.001) 8 slots
      E_Label => 6133, -- (0.001) 57 slots
      E_Loop => 6008, -- (0.001) 41 slots
      N_Generic_Package_Declaration => 5808, -- (0.001) 10 slots
      N_If_Expression => 5800, -- (0.001) 7 slots
      N_Record_Definition => 5628, -- (0.001) 7 slots
      N_Slice => 5461, -- (0.001) 7 slots
      N_Reference => 5332, -- (0.001) 7 slots
      E_Generic_Package => 5268, -- (0.001) 59 slots
      E_Record_Type => 4838, -- (0.000) 51 slots
      N_Raise_Program_Error => 4675, -- (0.000) 6 slots
      N_Raise_Statement => 4628, -- (0.000) 8 slots
      N_Use_Type_Clause => 4487, -- (0.000) 9 slots
      E_Array_Type => 4325, -- (0.000) 48 slots
      E_Operator => 4308, -- (0.000) 55 slots
      N_Freeze_Generic_Entity => 4249, -- (0.000) 4 slots
      N_Constrained_Array_Definition => 4244, -- (0.000) 5 slots
      N_Object_Renaming_Declaration => 4067, -- (0.000) 8 slots
      N_Formal_Private_Type_Definition => 4018, -- (0.000) 8 slots
      E_Loop_Parameter => 3870, -- (0.000) 38 slots
      N_Real_Literal => 3759, -- (0.000) 7 slots
      N_Attribute_Definition_Clause => 3724, -- (0.000) 8 slots
      N_Exception_Renaming_Declaration => 3697, -- (0.000) 8 slots
      E_Class_Wide_Type => 3674, -- (0.000) 48 slots
      E_Exception => 3632, -- (0.000) 24 slots
      N_Range_Constraint => 3506, -- (0.000) 4 slots
      E_Access_Type => 3487, -- (0.000) 44 slots
      E_Subprogram_Type => 3248, -- (0.000) 47 slots
      N_Package_Instantiation => 3005, -- (0.000) 8 slots
      E_Access_Attribute_Type => 2959, -- (0.000) 44 slots
      N_Op_And => 2957, -- (0.000) 8 slots
      E_Generic_In_Parameter => 2704, -- (0.000) 31 slots
      N_Derived_Type_Definition => 2688, -- (0.000) 7 slots
      N_Variant => 2535, -- (0.000) 8 slots
      E_Record_Subtype_With_Private => 2327, -- (0.000) 50 slots
      N_Private_Type_Declaration => 2287, -- (0.000) 6 slots
      E_Private_Type => 1890, -- (0.000) 48 slots
      N_Discriminant_Specification => 1864, -- (0.000) 7 slots
      N_Procedure_Instantiation => 1659, -- (0.000) 8 slots
      N_Op_Multiply => 1634, -- (0.000) 8 slots
      E_Access_Subtype => 1606, -- (0.000) 44 slots
      N_Defining_Program_Unit_Name => 1463, -- (0.000) 8 slots
      N_Number_Declaration => 1461, -- (0.000) 7 slots
      E_Named_Integer => 1430, -- (0.000) 19 slots
      N_Use_Package_Clause => 1369, -- (0.000) 9 slots
      N_Compilation_Unit_Aux => 1341, -- (0.000) 8 slots
      N_Compilation_Unit => 1341, -- (0.000) 8 slots
      N_Elsif_Part => 1331, -- (0.000) 7 slots
      N_Operator_Symbol => 1305, -- (0.000) 7 slots
      E_Limited_Private_Type => 1299, -- (0.000) 48 slots
      E_Generic_Function => 1292, -- (0.000) 41 slots
      E_Enumeration_Type => 1186, -- (0.000) 47 slots
      N_Enumeration_Type_Definition => 1169, -- (0.000) 6 slots
      N_Unchecked_Expression => 1112, -- (0.000) 7 slots
      N_Op_Or => 1107, -- (0.000) 8 slots
      N_Designator => 1100, -- (0.000) 9 slots
      N_Formal_Discrete_Type_Definition => 1086, -- (0.000) 4 slots
      N_Variant_Part => 1072, -- (0.000) 8 slots
      N_Formal_Package_Declaration => 1047, -- (0.000) 8 slots
      N_Quantified_Expression => 1033, -- (0.000) 8 slots
      E_Record_Type_With_Private => 1017, -- (0.000) 51 slots
      N_Package_Body => 999, -- (0.000) 9 slots
      N_Unconstrained_Array_Definition => 973, -- (0.000) 5 slots
      E_Private_Subtype => 971, -- (0.000) 48 slots
      N_Incomplete_Type_Declaration => 863, -- (0.000) 6 slots
      E_Incomplete_Type => 863, -- (0.000) 48 slots
      N_Contract => 859, -- (0.000) 6 slots
      E_Package_Body => 852, -- (0.000) 46 slots
      N_Extended_Return_Statement => 801, -- (0.000) 8 slots
      N_Op_Divide => 724, -- (0.000) 8 slots
      N_Extension_Aggregate => 718, -- (0.000) 8 slots
      N_Function_Instantiation => 642, -- (0.000) 8 slots
      N_Exception_Declaration => 594, -- (0.000) 7 slots
      N_Discriminant_Association => 552, -- (0.000) 7 slots
      N_Iterator_Specification => 543, -- (0.000) 8 slots
      N_Private_Extension_Declaration => 540, -- (0.000) 8 slots
      N_Formal_Signed_Integer_Type_Definition => 512, -- (0.000) 4 slots
      E_Modular_Integer_Subtype => 490, -- (0.000) 44 slots
      N_Component_Clause => 468, -- (0.000) 7 slots
      E_Signed_Integer_Type => 399, -- (0.000) 43 slots
      N_Op_Minus => 356, -- (0.000) 7 slots
      N_Raise_Expression => 337, -- (0.000) 8 slots
      N_Case_Expression_Alternative => 336, -- (0.000) 8 slots
      N_Op_Expon => 280, -- (0.000) 8 slots
      N_Abstract_Subprogram_Declaration => 250, -- (0.000) 6 slots
      E_Modular_Integer_Type => 232, -- (0.000) 44 slots
      N_Modular_Type_Definition => 214, -- (0.000) 7 slots
      N_Compound_Statement => 212, -- (0.000) 6 slots
      N_Free_Statement => 209, -- (0.000) 8 slots
      N_Record_Representation_Clause => 197, -- (0.000) 9 slots
      N_Access_Procedure_Definition => 195, -- (0.000) 6 slots
      E_Limited_Private_Subtype => 178, -- (0.000) 48 slots
      N_Access_Function_Definition => 172, -- (0.000) 7 slots
      N_Op_Mod => 163, -- (0.000) 8 slots
      N_Validate_Unchecked_Conversion => 156, -- (0.000) 5 slots
      E_Anonymous_Access_Subprogram_Type => 155, -- (0.000) 44 slots
      N_Op_Rem => 147, -- (0.000) 8 slots
      N_Formal_Incomplete_Type_Definition => 140, -- (0.000) 4 slots
      N_Signed_Integer_Type_Definition => 137, -- (0.000) 6 slots
      N_Case_Expression => 132, -- (0.000) 7 slots
      N_Op_Plus => 129, -- (0.000) 7 slots
      E_Incomplete_Subtype => 129, -- (0.000) 48 slots
      N_Op_Abs => 119, -- (0.000) 7 slots
      N_Op_Shift_Right => 109, -- (0.000) 8 slots
      E_Floating_Point_Subtype => 94, -- (0.000) 43 slots
      N_Op_Shift_Left => 72, -- (0.000) 8 slots
      E_Floating_Point_Type => 59, -- (0.000) 43 slots
      N_Formal_Derived_Type_Definition => 53, -- (0.000) 7 slots
      N_Formal_Floating_Point_Definition => 40, -- (0.000) 4 slots
      N_Defining_Character_Literal => 0, -- 36, -- (0.000) 8 slots
      N_Formal_Modular_Type_Definition => 27, -- (0.000) 4 slots
      E_Ordinary_Fixed_Point_Subtype => 23, -- (0.000) 44 slots
      E_Abstract_State => 22, -- (0.000) 48 slots
      E_Named_Real => 20, -- (0.000) 19 slots
      N_Floating_Point_Definition => 19, -- (0.000) 6 slots
      N_Subunit => 17, -- (0.000) 8 slots
      N_Enumeration_Representation_Clause => 17, -- (0.000) 9 slots
      N_Entry_Declaration => 17, -- (0.000) 7 slots
      N_Subprogram_Body_Stub => 16, -- (0.000) 8 slots
      N_Unused_At_Start => 15, -- (0.000) 4 slots
      E_Entry => 14, -- (0.000) 42 slots
      N_Formal_Ordinary_Fixed_Point_Definition => 12, -- (0.000) 4 slots
      E_Class_Wide_Subtype => 9, -- (0.000) 52 slots
      E_Protected_Subtype => 8, -- (0.000) 48 slots
      E_Ordinary_Fixed_Point_Type => 8, -- (0.000) 44 slots
      N_Op_Xor => 7, -- (0.000) 8 slots
      E_Generic_In_Out_Parameter => 7, -- (0.000) 31 slots
      N_Protected_Type_Declaration => 6, -- (0.000) 8 slots
      N_Protected_Definition => 6, -- (0.000) 8 slots
      N_Task_Type_Declaration => 4, -- (0.000) 8 slots
      N_Task_Definition => 4, -- (0.000) 8 slots
      N_Protected_Body => 4, -- (0.000) 9 slots
      E_Task_Subtype => 4, -- (0.000) 50 slots
      E_Protected_Type => 4, -- (0.000) 49 slots
      E_Access_Protected_Subprogram_Type => 4, -- (0.000) 45 slots
      N_Entry_Call_Statement => 3, -- (0.000) 8 slots
      E_Task_Type => 3, -- (0.000) 50 slots
      N_Raise_Storage_Error => 2, -- (0.000) 6 slots
      N_Package_Body_Stub => 2, -- (0.000) 8 slots
      N_Generic_Procedure_Renaming_Declaration => 2, -- (0.000) 8 slots
      N_Task_Body => 1, -- (0.000) 10 slots
      N_Single_Protected_Declaration => 1, -- (0.000) 8 slots
      N_Real_Range_Specification => 1, -- (0.000) 6 slots
      N_Ordinary_Fixed_Point_Definition => 1, -- (0.000) 6 slots
      N_Error => 1, -- (0.000) 6 slots
      N_Entry_Body_Formal_Part => 1, -- (0.000) 6 slots
      N_Entry_Body => 1, -- (0.000) 10 slots
      N_Empty => 1, -- (0.000) 6 slots
      N_Delay_Relative_Statement => 1, -- (0.000) 7 slots
      E_Protected_Body => 1, -- (0.000) 35 slots

      Between_Concrete_Node_And_Concrete_Entity_Types => 0,

      --  The rest had frequency 0 (i.e. no such nodes were created in the
      --  example), but we set them to 1, so we won't lose information when
      --  multiplying. We use "others", so that if new node types are added,
      --  we don't have to modify the table; new node types are unlikely to
      --  be very common.

      others => 1
      --  N_Variable_Reference_Marker => 0, (0.000) 4 slots
      --  N_Unused_At_End => 0, (0.000) 4 slots
      --  N_Triggering_Alternative => 0, (0.000) 6 slots
      --  N_Timed_Entry_Call => 0, (0.000) 5 slots
      --  N_Terminate_Alternative => 0, (0.000) 6 slots
      --  N_Task_Body_Stub => 0, (0.000) 8 slots
      --  N_Target_Name => 0, (0.000) 5 slots
      --  N_Single_Task_Declaration => 0, (0.000) 8 slots
      --  N_Selective_Accept => 0, (0.000) 5 slots
      --  N_Scil_Membership_Test => 0, (0.000) 5 slots
      --  N_Scil_Dispatch_Table_Tag_Init => 0, (0.000) 4 slots
      --  N_Scil_Dispatching_Call => 0, (0.000) 6 slots
      --  N_Return_When_Statement => 0, (0.000) 7 slots
      --  N_Requeue_Statement => 0, (0.000) 8 slots
      --  N_Raise_When_Statement => 0, (0.000) 8 slots
      --  N_Push_Storage_Error_Label => 0, (0.000) 4 slots
      --  N_Push_Program_Error_Label => 0, (0.000) 4 slots
      --  N_Push_Constraint_Error_Label => 0, (0.000) 4 slots
      --  N_Protected_Body_Stub => 0, (0.000) 8 slots
      --  N_Pop_Storage_Error_Label => 0, (0.000) 4 slots
      --  N_Pop_Program_Error_Label => 0, (0.000) 4 slots
      --  N_Pop_Constraint_Error_Label => 0, (0.000) 4 slots
      --  N_Op_Shift_Right_Arithmetic => 0, (0.000) 8 slots
      --  N_Op_Rotate_Right => 0, (0.000) 8 slots
      --  N_Op_Rotate_Left => 0, (0.000) 8 slots
      --  N_Mod_Clause => 0, (0.000) 7 slots
      --  N_Iterated_Element_Association => 0, (0.000) 8 slots
      --  N_Iterated_Component_Association => 0, (0.000) 8 slots
      --  N_Goto_When_Statement => 0, (0.000) 8 slots
      --  N_Generic_Package_Renaming_Declaration => 0, (0.000) 8 slots
      --  N_Generic_Function_Renaming_Declaration => 0, (0.000) 8 slots
      --  N_Formal_Decimal_Fixed_Point_Definition => 0, (0.000) 4 slots
      --  N_Formal_Abstract_Subprogram_Declaration => 0, (0.000) 6 slots
      --  N_Entry_Index_Specification => 0, (0.000) 7 slots
      --  N_Entry_Call_Alternative => 0, (0.000) 6 slots
      --  N_Digits_Constraint => 0, (0.000) 6 slots
      --  N_Delta_Constraint => 0, (0.000) 6 slots
      --  N_Delta_Aggregate => 0, (0.000) 8 slots
      --  N_Delay_Until_Statement => 0, (0.000) 7 slots
      --  N_Delay_Alternative => 0, (0.000) 7 slots
      --  N_Decimal_Fixed_Point_Definition => 0, (0.000) 6 slots
      --  N_Conditional_Entry_Call => 0, (0.000) 5 slots
      --  N_Code_Statement => 0, (0.000) 7 slots
      --  N_At_Clause => 0, (0.000) 9 slots
      --  N_Asynchronous_Select => 0, (0.000) 5 slots
      --  N_Accept_Statement => 0, (0.000) 8 slots
      --  N_Accept_Alternative => 0, (0.000) 8 slots
      --  N_Abort_Statement => 0, (0.000) 4 slots
      --  N_Abortable_Part => 0, (0.000) 5 slots
      --  E_Task_Body => 0, (0.000) 39 slots
      --  E_Exception_Type => 0, (0.000) 45 slots
      --  E_Entry_Index_Parameter => 0, (0.000) 19 slots
      --  E_Entry_Family => 0, (0.000) 42 slots
      --  E_Decimal_Fixed_Point_Type => 0, (0.000) 52 slots
      --  E_Decimal_Fixed_Point_Subtype => 0, (0.000) 52 slots
      --  E_Anonymous_Access_Protected_Subprogram_Type => 0, (0.000) 45 slots
     ); -- Type_Frequency

end Gen_IL.Internals;
