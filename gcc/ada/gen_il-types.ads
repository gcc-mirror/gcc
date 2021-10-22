------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          G E N _ I L . T Y P E S                         --
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

package Gen_IL.Types is

   --  Enumeration of all the types that are "of interest". We have an
   --  enumeration literal here for every node kind, every entity kind,
   --  and every type that can be the type of a field.

   --  The following is "optional type enumeration" -- i.e. it is Type_Enum
   --  (declared below) plus the special null value No_Type.  See the spec of
   --  Gen_IL.Gen for how to modify this. (Of course, in Ada we have to define
   --  this backwards from the above conceptual description.)

   --  Note that there are various subranges of this type declared below,
   --  which might need to be kept in sync when modifying this.

   --  The "Between_..." literals below are simply for making the subranges.
   --  When adding literals to this enumeration type, be sure to put them in
   --  the right place so they end up in the appropriate subranges
   --  (Abstract_Node, Abstract_Entity, Concrete_Node, Concrete_Entity).

   type Opt_Type_Enum is
     (No_Type,

      Flag,
      --  We use Flag for Boolean, so we don't conflict with
      --  Standard.Boolean.

      Node_Id,
      List_Id,
      Elist_Id,
      Name_Id,
      String_Id,
      Uint,
      Valid_Uint,
      Unat,
      Upos,
      Nonzero_Uint,
      Ureal,

      Node_Kind_Type, -- Type of result of Nkind function, i.e. Node_Kind
      Entity_Kind_Type, -- Type of result of Ekind function, i.e. Entity_Kind
      Source_Ptr,
      Small_Paren_Count_Type,
      Union_Id,
      Convention_Id,

      Component_Alignment_Kind,
      Mechanism_Type,

      Between_Special_And_Abstract_Node_Types,

      --  Abstract node types:

      Node_Kind, -- root of node type hierarchy
      N_Access_To_Subprogram_Definition,
      N_Alternative,
      N_Array_Type_Definition,
      N_Binary_Op,
      N_Body_Stub,
      N_Declaration,
      N_Delay_Statement,
      N_Direct_Name,
      N_Entity,
      N_Entity_Name,
      N_Formal_Subprogram_Declaration,
      N_Generic_Declaration,
      N_Generic_Instantiation,
      N_Generic_Renaming_Declaration,
      N_Has_Bounds,
      N_Has_Chars,
      N_Has_Condition,
      N_Has_Entity,
      N_Has_Etype,
      N_Is_Case_Choice,
      N_Is_Decl,
      N_Is_Exception_Choice,
      N_Is_Index,
      N_Is_Range,
      N_Multiplying_Operator,
      N_Later_Decl_Item,
      N_Membership_Test,
      N_Numeric_Or_String_Literal,
      N_Op,
      N_Op_Boolean,
      N_Op_Compare,
      N_Op_Shift,
      N_Proper_Body,
      N_Push_xxx_Label,
      N_Pop_xxx_Label,
      N_Push_Pop_xxx_Label,
      N_Raise_xxx_Error,
      N_Renaming_Declaration,
      N_Representation_Clause,
      N_Short_Circuit,
      N_SCIL_Node,
      N_Statement_Other_Than_Procedure_Call,
      N_Subprogram_Call,
      N_Subprogram_Instantiation,
      N_Subexpr,
      N_Subprogram_Specification,
      N_Unary_Op,
      N_Unit_Body,

      --  End of abstract node types.

      Between_Abstract_Node_And_Abstract_Entity_Types,

      --  Abstract entity types:

      Entity_Kind, -- root of entity type hierarchy
      Access_Kind,
      Access_Subprogram_Kind,
      Access_Protected_Kind,
      Aggregate_Kind,
      Allocatable_Kind,
      Anonymous_Access_Kind,
      Array_Kind,
      Assignable_Kind,
      Class_Wide_Kind,
      Composite_Kind,
      Concurrent_Kind,
      Concurrent_Body_Kind,
      Constant_Or_Variable_Kind,
      Decimal_Fixed_Point_Kind,
      Digits_Kind,
      Discrete_Kind,
      Discrete_Or_Fixed_Point_Kind,
      Elementary_Kind,
      Enumeration_Kind,
      Entry_Kind,
      Evaluable_Kind,
      Exception_Or_Object_Kind,
      Fixed_Point_Kind,
      Float_Kind,
      Formal_Kind,
      Formal_Object_Kind,
      Generic_Subprogram_Kind,
      Generic_Unit_Kind,
      Global_Name_Kind,
      Incomplete_Kind,
      Incomplete_Or_Private_Kind,
      Integer_Kind,
      Modular_Integer_Kind,
      Named_Access_Kind,
      Named_Kind,
      Numeric_Kind,
      Object_Kind,
      Ordinary_Fixed_Point_Kind,
      Overloadable_Kind,
      Private_Kind,
      Protected_Kind,
      Real_Kind,
      Record_Kind,
      Record_Field_Kind,
      Scalar_Kind,
      Signed_Integer_Kind,
      Subprogram_Type_Or_Kind,
      Subprogram_Kind,
      Task_Kind,
      Type_Kind,
      Void_Or_Type_Kind,

      --  End of abstract entity types.

      Between_Abstract_Entity_And_Concrete_Node_Types,

      --  Concrete node types:

      N_Unused_At_Start,
      N_At_Clause,
      N_Component_Clause,
      N_Enumeration_Representation_Clause,
      N_Mod_Clause,
      N_Record_Representation_Clause,
      N_Attribute_Definition_Clause,
      N_Empty,
      N_Pragma_Argument_Association,
      N_Error,
      N_Defining_Character_Literal,
      N_Defining_Identifier,
      N_Defining_Operator_Symbol,
      N_Expanded_Name,
      N_Identifier,
      N_Operator_Symbol,
      N_Character_Literal,
      N_Op_Add,
      N_Op_Concat,
      N_Op_Expon,
      N_Op_Subtract,
      N_Op_Divide,
      N_Op_Mod,
      N_Op_Multiply,
      N_Op_Rem,
      N_Op_And,
      N_Op_Eq,
      N_Op_Ge,
      N_Op_Gt,
      N_Op_Le,
      N_Op_Lt,
      N_Op_Ne,
      N_Op_Or,
      N_Op_Xor,
      N_Op_Rotate_Left,
      N_Op_Rotate_Right,
      N_Op_Shift_Left,
      N_Op_Shift_Right,
      N_Op_Shift_Right_Arithmetic,
      N_Op_Abs,
      N_Op_Minus,
      N_Op_Not,
      N_Op_Plus,
      N_Attribute_Reference,
      N_In,
      N_Not_In,
      N_And_Then,
      N_Or_Else,
      N_Function_Call,
      N_Procedure_Call_Statement,
      N_Raise_Constraint_Error,
      N_Raise_Program_Error,
      N_Raise_Storage_Error,
      N_Integer_Literal,
      N_Real_Literal,
      N_String_Literal,
      N_Explicit_Dereference,
      N_Expression_With_Actions,
      N_If_Expression,
      N_Indexed_Component,
      N_Null,
      N_Qualified_Expression,
      N_Quantified_Expression,
      N_Aggregate,
      N_Allocator,
      N_Case_Expression,
      N_Delta_Aggregate,
      N_Extension_Aggregate,
      N_Raise_Expression,
      N_Range,
      N_Reference,
      N_Selected_Component,
      N_Slice,
      N_Target_Name,
      N_Type_Conversion,
      N_Unchecked_Expression,
      N_Unchecked_Type_Conversion,
      N_Subtype_Indication,
      N_Component_Declaration,
      N_Entry_Declaration,
      N_Expression_Function,
      N_Formal_Object_Declaration,
      N_Formal_Type_Declaration,
      N_Full_Type_Declaration,
      N_Incomplete_Type_Declaration,
      N_Iterator_Specification,
      N_Loop_Parameter_Specification,
      N_Object_Declaration,
      N_Protected_Type_Declaration,
      N_Private_Extension_Declaration,
      N_Private_Type_Declaration,
      N_Subtype_Declaration,
      N_Function_Specification,
      N_Procedure_Specification,
      N_Access_Function_Definition,
      N_Access_Procedure_Definition,
      N_Task_Type_Declaration,
      N_Package_Body_Stub,
      N_Protected_Body_Stub,
      N_Subprogram_Body_Stub,
      N_Task_Body_Stub,
      N_Function_Instantiation,
      N_Procedure_Instantiation,
      N_Package_Instantiation,
      N_Package_Body,
      N_Subprogram_Body,
      N_Protected_Body,
      N_Task_Body,
      N_Implicit_Label_Declaration,
      N_Package_Declaration,
      N_Single_Task_Declaration,
      N_Subprogram_Declaration,
      N_Use_Package_Clause,
      N_Generic_Package_Declaration,
      N_Generic_Subprogram_Declaration,
      N_Constrained_Array_Definition,
      N_Unconstrained_Array_Definition,
      N_Exception_Renaming_Declaration,
      N_Object_Renaming_Declaration,
      N_Package_Renaming_Declaration,
      N_Subprogram_Renaming_Declaration,
      N_Generic_Function_Renaming_Declaration,
      N_Generic_Package_Renaming_Declaration,
      N_Generic_Procedure_Renaming_Declaration,
      N_Abort_Statement,
      N_Accept_Statement,
      N_Assignment_Statement,
      N_Asynchronous_Select,
      N_Block_Statement,
      N_Case_Statement,
      N_Code_Statement,
      N_Compound_Statement,
      N_Conditional_Entry_Call,
      N_Delay_Relative_Statement,
      N_Delay_Until_Statement,
      N_Entry_Call_Statement,
      N_Free_Statement,
      N_Goto_Statement,
      N_Goto_When_Statement,
      N_Loop_Statement,
      N_Null_Statement,
      N_Raise_Statement,
      N_Raise_When_Statement,
      N_Requeue_Statement,
      N_Simple_Return_Statement,
      N_Extended_Return_Statement,
      N_Return_When_Statement,
      N_Selective_Accept,
      N_Timed_Entry_Call,
      N_Exit_Statement,
      N_If_Statement,
      N_Accept_Alternative,
      N_Delay_Alternative,
      N_Elsif_Part,
      N_Entry_Body_Formal_Part,
      N_Iteration_Scheme,
      N_Terminate_Alternative,
      N_Formal_Abstract_Subprogram_Declaration,
      N_Formal_Concrete_Subprogram_Declaration,
      N_Push_Constraint_Error_Label,
      N_Push_Program_Error_Label,
      N_Push_Storage_Error_Label,
      N_Pop_Constraint_Error_Label,
      N_Pop_Program_Error_Label,
      N_Pop_Storage_Error_Label,
      N_SCIL_Dispatch_Table_Tag_Init,
      N_SCIL_Dispatching_Call,
      N_SCIL_Membership_Test,
      N_Abortable_Part,
      N_Abstract_Subprogram_Declaration,
      N_Access_Definition,
      N_Access_To_Object_Definition,
      N_Aspect_Specification,
      N_Call_Marker,
      N_Case_Expression_Alternative,
      N_Case_Statement_Alternative,
      N_Compilation_Unit,
      N_Compilation_Unit_Aux,
      N_Component_Association,
      N_Component_Definition,
      N_Component_List,
      N_Contract,
      N_Derived_Type_Definition,
      N_Decimal_Fixed_Point_Definition,
      N_Defining_Program_Unit_Name,
      N_Delta_Constraint,
      N_Designator,
      N_Digits_Constraint,
      N_Discriminant_Association,
      N_Discriminant_Specification,
      N_Enumeration_Type_Definition,
      N_Entry_Body,
      N_Entry_Call_Alternative,
      N_Entry_Index_Specification,
      N_Exception_Declaration,
      N_Exception_Handler,
      N_Floating_Point_Definition,
      N_Formal_Decimal_Fixed_Point_Definition,
      N_Formal_Derived_Type_Definition,
      N_Formal_Discrete_Type_Definition,
      N_Formal_Floating_Point_Definition,
      N_Formal_Modular_Type_Definition,
      N_Formal_Ordinary_Fixed_Point_Definition,
      N_Formal_Package_Declaration,
      N_Formal_Private_Type_Definition,
      N_Formal_Incomplete_Type_Definition,
      N_Formal_Signed_Integer_Type_Definition,
      N_Freeze_Entity,
      N_Freeze_Generic_Entity,
      N_Generic_Association,
      N_Handled_Sequence_Of_Statements,
      N_Index_Or_Discriminant_Constraint,
      N_Iterated_Component_Association,
      N_Iterated_Element_Association,
      N_Itype_Reference,
      N_Label,
      N_Modular_Type_Definition,
      N_Number_Declaration,
      N_Ordinary_Fixed_Point_Definition,
      N_Others_Choice,
      N_Package_Specification,
      N_Parameter_Association,
      N_Parameter_Specification,
      N_Pragma,
      N_Protected_Definition,
      N_Range_Constraint,
      N_Real_Range_Specification,
      N_Record_Definition,
      N_Signed_Integer_Type_Definition,
      N_Single_Protected_Declaration,
      N_Subunit,
      N_Task_Definition,
      N_Triggering_Alternative,
      N_Use_Type_Clause,
      N_Validate_Unchecked_Conversion,
      N_Variable_Reference_Marker,
      N_Variant,
      N_Variant_Part,
      N_With_Clause,
      N_Unused_At_End,

      --  End of concrete node types.

      Between_Concrete_Node_And_Concrete_Entity_Types,

      --  Concrete entity types:

      E_Void,
      E_Component,
      E_Constant,
      E_Discriminant,
      E_Loop_Parameter,
      E_Variable,
      E_Out_Parameter,
      E_In_Out_Parameter,
      E_In_Parameter,
      E_Generic_In_Out_Parameter,
      E_Generic_In_Parameter,
      E_Named_Integer,
      E_Named_Real,
      E_Enumeration_Type,
      E_Enumeration_Subtype,
      E_Signed_Integer_Type,
      E_Signed_Integer_Subtype,
      E_Modular_Integer_Type,
      E_Modular_Integer_Subtype,
      E_Ordinary_Fixed_Point_Type,
      E_Ordinary_Fixed_Point_Subtype,
      E_Decimal_Fixed_Point_Type,
      E_Decimal_Fixed_Point_Subtype,
      E_Floating_Point_Type,
      E_Floating_Point_Subtype,
      E_Access_Type,
      E_Access_Subtype,
      E_Access_Attribute_Type,
      E_Allocator_Type,
      E_General_Access_Type,
      E_Access_Subprogram_Type,
      E_Access_Protected_Subprogram_Type,
      E_Anonymous_Access_Protected_Subprogram_Type,
      E_Anonymous_Access_Subprogram_Type,
      E_Anonymous_Access_Type,
      E_Array_Type,
      E_Array_Subtype,
      E_String_Literal_Subtype,
      E_Class_Wide_Type,
      E_Class_Wide_Subtype,
      E_Record_Type,
      E_Record_Subtype,
      E_Record_Type_With_Private,
      E_Record_Subtype_With_Private,
      E_Private_Type,
      E_Private_Subtype,
      E_Limited_Private_Type,
      E_Limited_Private_Subtype,
      E_Incomplete_Type,
      E_Incomplete_Subtype,
      E_Task_Type,
      E_Task_Subtype,
      E_Protected_Type,
      E_Protected_Subtype,
      E_Exception_Type,
      E_Subprogram_Type,
      E_Enumeration_Literal,
      E_Function,
      E_Operator,
      E_Procedure,
      E_Abstract_State,
      E_Entry,
      E_Entry_Family,
      E_Block,
      E_Entry_Index_Parameter,
      E_Exception,
      E_Generic_Function,
      E_Generic_Procedure,
      E_Generic_Package,
      E_Label,
      E_Loop,
      E_Return_Statement,
      E_Package,
      E_Package_Body,
      E_Protected_Body,
      E_Task_Body,
      E_Subprogram_Body

      --  End of concrete entity types.

     ); -- Type_Enum

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

   subtype Traversed_Field_Type is Type_Enum with Predicate =>
     Traversed_Field_Type in Node_Id | List_Id | Node_Type;
   --  These are the types of fields traversed by Traverse_Func

   subtype Entity_Node is Node_Type with
     Predicate => Entity_Node in
        N_Defining_Character_Literal
      | N_Defining_Identifier
      | N_Defining_Operator_Symbol;

   subtype Opt_Abstract_Type is Opt_Type_Enum with
     Predicate => Opt_Abstract_Type = No_Type or
       Opt_Abstract_Type in Abstract_Type;

   subtype Type_Boundaries is Type_Enum with
     Predicate => Type_Boundaries in
       Between_Abstract_Node_And_Abstract_Entity_Types |
       Between_Abstract_Entity_And_Concrete_Node_Types |
       Between_Concrete_Node_And_Concrete_Entity_Types;
   --  These are not used, other than to separate the various subranges.

   subtype Uint_Subtype is Type_Enum with
     Predicate => Uint_Subtype in Valid_Uint | Unat | Upos | Nonzero_Uint;
   --  These are the subtypes of Uint that have predicates restricting their
   --  values.

end Gen_IL.Types;
