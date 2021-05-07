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

with Gen_IL.Types;  use Gen_IL.Types;
with Gen_IL.Fields; use Gen_IL.Fields;

package Gen_IL.Utils is

   subtype Type_Enum is Opt_Type_Enum
     range Opt_Type_Enum'Succ (No_Type) .. Opt_Type_Enum'Last;
   --  Enumeration of types -- Opt_Type_Enum without the special null value
   --  No_Type.

   subtype Node_Or_Entity_Type is
     Type_Enum range
       Type_Enum'Succ (Between_Special_And_Abstract_Node_Types) ..
         Type_Enum'Last;

   subtype Abstract_Type is
     Type_Enum range
       Type_Enum'Succ (Between_Special_And_Abstract_Node_Types) ..
         Type_Enum'Pred (Between_Abstract_Entity_And_Concrete_Node_Types);
   subtype Abstract_Node is
     Abstract_Type range
       Type_Enum'Succ (Between_Special_And_Abstract_Node_Types) ..
         Type_Enum'Pred (Between_Abstract_Node_And_Abstract_Entity_Types);
   subtype Abstract_Entity is
     Abstract_Type range
       Type_Enum'Succ (Between_Abstract_Node_And_Abstract_Entity_Types) ..
         Type_Enum'Pred (Between_Abstract_Entity_And_Concrete_Node_Types);

   subtype Concrete_Type is
     Type_Enum range
       Type_Enum'Succ (Between_Abstract_Entity_And_Concrete_Node_Types) ..
         Type_Enum'Last;
   subtype Concrete_Node is
     Concrete_Type range
       Type_Enum'Succ (Between_Abstract_Entity_And_Concrete_Node_Types) ..
         Type_Enum'Pred (Between_Concrete_Node_And_Concrete_Entity_Types);
   subtype Concrete_Entity is
     Concrete_Type range
       Type_Enum'Succ (Between_Concrete_Node_And_Concrete_Entity_Types) ..
         Type_Enum'Last;

   subtype Root_Type is Abstract_Type with
        Predicate => Root_Type in Node_Kind | Entity_Kind;

   subtype Node_Type is Node_Or_Entity_Type with
        Predicate => Node_Type in Abstract_Node | Concrete_Node;
   subtype Entity_Type is Node_Or_Entity_Type with
        Predicate => Entity_Type in Abstract_Entity | Concrete_Entity;

   subtype Special_Type is Type_Enum range
     Flag .. Type_Enum'Pred (Between_Special_And_Abstract_Node_Types);

   subtype Traversal_Type is Type_Enum with Predicate =>
     Traversal_Type in Node_Id | List_Id | Node_Type;
   --  These are the types of fields traversed by Traverse_Func

   subtype Entity_Node is Node_Type with
     Predicate => Entity_Node in
        N_Defining_Character_Literal
      | N_Defining_Identifier
      | N_Defining_Operator_Symbol;

   function Image (T : Opt_Type_Enum) return String;

   function Image_Sans_N (T : Opt_Type_Enum) return String;
   --  Returns the image without the leading "N_"

   subtype Boundaries is Type_Enum with
        Predicate => Boundaries in
          Between_Abstract_Node_And_Abstract_Entity_Types |
          Between_Abstract_Entity_And_Concrete_Node_Types |
          Between_Concrete_Node_And_Concrete_Entity_Types;

   ----------------

   type Type_Set is array (Type_Enum) of Boolean;

   type Type_Index is new Positive;
   subtype Type_Count is Type_Index'Base range 0 .. Type_Index'Last;
   package Type_Vectors is new Vectors (Type_Index, Type_Enum);
   use Type_Vectors;
   subtype Type_Vector is Type_Vectors.Vector;

   procedure Ptypes (V : Type_Vector); -- for debugging

   type Type_Array is array (Type_Index range <>) of Type_Enum;

   subtype Field_Enum is Opt_Field_Enum
     range Opt_Field_Enum'Succ (No_Field) .. Opt_Field_Enum'Last;
   --  Enumeration of fields -- Opt_Field_Enum without the special null value
   --  No_Field.

   subtype Node_Header_Type is Type_Enum range
     Nkind_Type .. Union_Id;
   subtype Node_Header_Field is Field_Enum with Predicate =>
     Node_Header_Field in Nkind .. Link | Ekind;

   type Fields_Present_Array is array (Field_Enum) of Type_Set;

   type Field_Set is array (Field_Enum) of Boolean;
   type Fields_Per_Node_Type is array (Node_Or_Entity_Type) of Field_Set;

   type Field_Index is new Positive;
   subtype Field_Count is Field_Index'Base range 0 .. Field_Index'Last;
   package Field_Vectors is new Vectors (Field_Index, Field_Enum);
   subtype Field_Vector is Field_Vectors.Vector;
   procedure Pfields (V : Field_Vector); -- for debugging

   subtype Opt_Abstract_Type is Opt_Type_Enum with
        Predicate => Opt_Abstract_Type = No_Type or
        Opt_Abstract_Type in Abstract_Type;

   procedure Put_Images (S : in out Sink'Class; U : Type_Vector);
   procedure Put_Id_Images (S : in out Sink'Class; U : Type_Vector);
   --  Put the types with vertical bars in between, as in
   --     N_This | N_That | N_Other
   --  or
   --     N_This_Id | N_That_Id | N_Other_Id

   function Id_Image (T : Type_Enum) return String;
   function Get_Set_Id_Image (T : Type_Enum) return String;

   type Bit_Offset is new Root_Nat range 0 .. 32_000 - 1;
   --  There are fewer than 1000 fields. But offsets are in size units (1 bit
   --  for flags, 32 bits for most others, also 2, 4, and 8).

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

            Allow_Overlap : Boolean;
            --  True to allow overlapping subranges
      end case;
   end record;

   type Type_Info_Ptr is access all Type_Info;

   Type_Table : array (Node_Or_Entity_Type) of Type_Info_Ptr;
   --  Table mapping from enumeration literals representing types to
   --  information about the type.

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

   type Type_Only_Enum is
     (No_Type_Only, Base_Type_Only, Impl_Base_Type_Only, Root_Type_Only);
   --  ????These correspond to the "[base type only]", "[implementation base
   --  type only]", and "[root type only]" annotations in the old einfo.ads.
   --  Move the relevant comments here. There is no comment explaining
   --  [root type only] in the old einfo.ads.

   function Image (Default : Field_Default_Value) return String;
   function Value_Image (Default : Field_Default_Value) return String;

   type Field_Info is record
      Have_This_Field : Type_Vector;

      Field_Type      : Type_Enum;
      --  Type of the field. Currently, we use Node_Id for all node-valued
      --  fields, but we could narrow down to children of that. Similar for
      --  Entity_Id.

      Default_Value   : Field_Default_Value;
      Type_Only       : Type_Only_Enum;
      Pre             : String_Ptr;

      Offset          : Field_Offset;
      --  Offset of the field, in units of the field size. So if a field is 4
      --  bits, it starts at bit number Offset*4 from the start of the node.
   end record;

   type Field_Info_Ptr is access all Field_Info;

   Field_Table : array (Field_Enum) of Field_Info_Ptr;
   --  Table mapping from enumeration literals representing fields to
   --  information about the field.

   procedure Verify_Type_Table;

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

   procedure Put_Type_Hierarchy (S : in out Sink'Class; Root : Root_Type);

   function Pos (T : Concrete_Type) return Root_Nat;
   --  Return Node_Kind'Pos (T) or Entity_Kind'Pos (T)

   ----------------

   --  The same field can be syntactic in some nodes but semantic in others:

   type Field_Desc is record
      F : Field_Enum;
      Is_Syntactic : Boolean;
   end record;

   type Field_Sequence_Index is new Positive;
   type Field_Sequence is array (Field_Sequence_Index range <>) of Field_Desc;
   No_Fields : constant Field_Sequence := (1 .. 0 => <>);

   type Field_Array is array (Bit_Offset range <>) of Opt_Field_Enum;
   type Field_Array_Ptr is access all Field_Array;

   type Type_Layout_Array is array (Concrete_Type) of Field_Array_Ptr;
   --  Mapping from types to mappings from offsets to fields

   type Offset_To_Fields_Mapping is
     array (Bit_Offset range <>) of Field_Array_Ptr;
   --  Mapping from bit offsets to fields using that offset

   function First_Abstract (Root : Root_Type) return Abstract_Type;
   function Last_Abstract (Root : Root_Type) return Abstract_Type;
   --  First and Last abstract types descended from the Root

   function First_Concrete (Root : Root_Type) return Concrete_Type;
   function Last_Concrete (Root : Root_Type) return Concrete_Type;
   --  First and Last concrete types descended from the Root

   function First_Field (Root : Root_Type) return Field_Enum;
   function Last_Field (Root : Root_Type) return Field_Enum;
   --  First and Last node or entity fields

   function Node_Or_Entity (Root : Root_Type) return String;
   --  Return "Node" or "Entity" depending on whether Root = Node_Kind

   type Sinfo_Node_Order_Index is new Positive;
   Sinfo_Node_Order :
     constant array (Sinfo_Node_Order_Index range <>) of Node_Type :=
     --  The order in which the documentation of node kinds appears in the old
     --  sinfo.ads. This is the same order of the functions in Nmake.
     --  Sinfo_Node_Order was constructed  by massaging nmake.ads.
     (N_Unused_At_Start,
      N_Unused_At_End,
      N_Identifier,
      N_Integer_Literal,
      N_Real_Literal,
      N_Character_Literal,
      N_String_Literal,
      N_Pragma,
      N_Pragma_Argument_Association,
      N_Defining_Identifier,
      N_Full_Type_Declaration,
      N_Subtype_Declaration,
      N_Subtype_Indication,
      N_Object_Declaration,
      N_Number_Declaration,
      N_Derived_Type_Definition,
      N_Range_Constraint,
      N_Range,
      N_Enumeration_Type_Definition,
      N_Defining_Character_Literal,
      N_Signed_Integer_Type_Definition,
      N_Modular_Type_Definition,
      N_Floating_Point_Definition,
      N_Real_Range_Specification,
      N_Ordinary_Fixed_Point_Definition,
      N_Decimal_Fixed_Point_Definition,
      N_Digits_Constraint,
      N_Unconstrained_Array_Definition,
      N_Constrained_Array_Definition,
      N_Component_Definition,
      N_Discriminant_Specification,
      N_Index_Or_Discriminant_Constraint,
      N_Discriminant_Association,
      N_Record_Definition,
      N_Component_List,
      N_Component_Declaration,
      N_Variant_Part,
      N_Variant,
      N_Others_Choice,
      N_Access_To_Object_Definition,
      N_Access_Function_Definition,
      N_Access_Procedure_Definition,
      N_Access_Definition,
      N_Incomplete_Type_Declaration,
      N_Explicit_Dereference,
      N_Indexed_Component,
      N_Slice,
      N_Selected_Component,
      N_Attribute_Reference,
      N_Aggregate,
      N_Component_Association,
      N_Extension_Aggregate,
      N_Iterated_Component_Association,
      N_Delta_Aggregate,
      N_Iterated_Element_Association,
      N_Null,
      N_And_Then,
      N_Or_Else,
      N_In,
      N_Not_In,
      N_Op_And,
      N_Op_Or,
      N_Op_Xor,
      N_Op_Eq,
      N_Op_Ne,
      N_Op_Lt,
      N_Op_Le,
      N_Op_Gt,
      N_Op_Ge,
      N_Op_Add,
      N_Op_Subtract,
      N_Op_Concat,
      N_Op_Multiply,
      N_Op_Divide,
      N_Op_Mod,
      N_Op_Rem,
      N_Op_Expon,
      N_Op_Plus,
      N_Op_Minus,
      N_Op_Abs,
      N_Op_Not,
      N_If_Expression,
      N_Case_Expression,
      N_Case_Expression_Alternative,
      N_Quantified_Expression,
      N_Type_Conversion,
      N_Qualified_Expression,
      N_Allocator,
      N_Null_Statement,
      N_Label,
      N_Assignment_Statement,
      N_Target_Name,
      N_If_Statement,
      N_Elsif_Part,
      N_Case_Statement,
      N_Case_Statement_Alternative,
      N_Loop_Statement,
      N_Iteration_Scheme,
      N_Loop_Parameter_Specification,
      N_Iterator_Specification,
      N_Block_Statement,
      N_Exit_Statement,
      N_Goto_Statement,
      N_Subprogram_Declaration,
      N_Abstract_Subprogram_Declaration,
      N_Function_Specification,
      N_Procedure_Specification,
      N_Designator,
      N_Defining_Program_Unit_Name,
      N_Operator_Symbol,
      N_Defining_Operator_Symbol,
      N_Parameter_Specification,
      N_Subprogram_Body,
      N_Procedure_Call_Statement,
      N_Function_Call,
      N_Parameter_Association,
      N_Simple_Return_Statement,
      N_Extended_Return_Statement,
      N_Expression_Function,
      N_Package_Declaration,
      N_Package_Specification,
      N_Package_Body,
      N_Private_Type_Declaration,
      N_Private_Extension_Declaration,
      N_Use_Package_Clause,
      N_Use_Type_Clause,
      N_Object_Renaming_Declaration,
      N_Exception_Renaming_Declaration,
      N_Package_Renaming_Declaration,
      N_Subprogram_Renaming_Declaration,
      N_Generic_Package_Renaming_Declaration,
      N_Generic_Procedure_Renaming_Declaration,
      N_Generic_Function_Renaming_Declaration,
      N_Task_Type_Declaration,
      N_Single_Task_Declaration,
      N_Task_Definition,
      N_Task_Body,
      N_Protected_Type_Declaration,
      N_Single_Protected_Declaration,
      N_Protected_Definition,
      N_Protected_Body,
      N_Entry_Declaration,
      N_Accept_Statement,
      N_Entry_Body,
      N_Entry_Body_Formal_Part,
      N_Entry_Index_Specification,
      N_Entry_Call_Statement,
      N_Requeue_Statement,
      N_Delay_Until_Statement,
      N_Delay_Relative_Statement,
      N_Selective_Accept,
      N_Accept_Alternative,
      N_Delay_Alternative,
      N_Terminate_Alternative,
      N_Timed_Entry_Call,
      N_Entry_Call_Alternative,
      N_Conditional_Entry_Call,
      N_Asynchronous_Select,
      N_Triggering_Alternative,
      N_Abortable_Part,
      N_Abort_Statement,
      N_Compilation_Unit,
      N_Compilation_Unit_Aux,
      N_With_Clause,
      N_Subprogram_Body_Stub,
      N_Package_Body_Stub,
      N_Task_Body_Stub,
      N_Protected_Body_Stub,
      N_Subunit,
      N_Exception_Declaration,
      N_Handled_Sequence_Of_Statements,
      N_Exception_Handler,
      N_Raise_Statement,
      N_Raise_Expression,
      N_Generic_Subprogram_Declaration,
      N_Generic_Package_Declaration,
      N_Package_Instantiation,
      N_Procedure_Instantiation,
      N_Function_Instantiation,
      N_Generic_Association,
      N_Formal_Object_Declaration,
      N_Formal_Type_Declaration,
      N_Formal_Private_Type_Definition,
      N_Formal_Derived_Type_Definition,
      N_Formal_Incomplete_Type_Definition,
      N_Formal_Discrete_Type_Definition,
      N_Formal_Signed_Integer_Type_Definition,
      N_Formal_Modular_Type_Definition,
      N_Formal_Floating_Point_Definition,
      N_Formal_Ordinary_Fixed_Point_Definition,
      N_Formal_Decimal_Fixed_Point_Definition,
      N_Formal_Concrete_Subprogram_Declaration,
      N_Formal_Abstract_Subprogram_Declaration,
      N_Formal_Package_Declaration,
      N_Attribute_Definition_Clause,
      N_Aspect_Specification,
      N_Enumeration_Representation_Clause,
      N_Record_Representation_Clause,
      N_Component_Clause,
      N_Code_Statement,
      N_Op_Rotate_Left,
      N_Op_Rotate_Right,
      N_Op_Shift_Left,
      N_Op_Shift_Right_Arithmetic,
      N_Op_Shift_Right,
      N_Delta_Constraint,
      N_At_Clause,
      N_Mod_Clause,
      N_Call_Marker,
      N_Compound_Statement,
      N_Contract,
      N_Expanded_Name,
      N_Expression_With_Actions,
      N_Free_Statement,
      N_Freeze_Entity,
      N_Freeze_Generic_Entity,
      N_Implicit_Label_Declaration,
      N_Itype_Reference,
      N_Raise_Constraint_Error,
      N_Raise_Program_Error,
      N_Raise_Storage_Error,
      N_Push_Constraint_Error_Label,
      N_Push_Program_Error_Label,
      N_Push_Storage_Error_Label,
      N_Pop_Constraint_Error_Label,
      N_Pop_Program_Error_Label,
      N_Pop_Storage_Error_Label,
      N_Reference,
      N_SCIL_Dispatch_Table_Tag_Init,
      N_SCIL_Dispatching_Call,
      N_SCIL_Membership_Test,
      N_Unchecked_Expression,
      N_Unchecked_Type_Conversion,
      N_Validate_Unchecked_Conversion,
      N_Variable_Reference_Marker);

end Gen_IL.Utils;
