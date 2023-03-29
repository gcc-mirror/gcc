------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                  G E N _ I L . G E N . G E N _ N O D E S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2020-2023, Free Software Foundation, Inc.        --
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

procedure Gen_IL.Gen.Gen_Nodes is

   procedure Ab -- Short for "Abstract"
     (T : Abstract_Node; Parent : Abstract_Type;
      Fields : Field_Sequence := No_Fields)
      renames Create_Abstract_Node_Type;
   procedure Cc -- Short for "ConCrete"
     (T : Concrete_Node; Parent : Abstract_Type;
      Fields : Field_Sequence := No_Fields;
      Nmake_Assert : String := "")
      renames Create_Concrete_Node_Type;

   function Sy -- Short for "Syntactic"
     (Field : Node_Field; Field_Type : Type_Enum;
      Default_Value : Field_Default_Value := No_Default;
      Pre, Pre_Get, Pre_Set : String := "") return Field_Desc
      renames Create_Syntactic_Field;
   function Sm -- Short for "Semantic"
     (Field : Field_Enum; Field_Type : Type_Enum;
      Type_Only  : Type_Only_Enum := No_Type_Only;
      Pre, Pre_Get, Pre_Set : String := "") return Field_Desc
      renames Create_Semantic_Field;

   procedure Union (T : Abstract_Node; Children : Type_Array)
     renames Create_Node_Union_Type;

begin -- Gen_IL.Gen.Gen_Nodes
   pragma Style_Checks ("M200");

   Create_Root_Node_Type (Node_Kind,
       (Sm (Nkind, Node_Kind_Type),
        Sm (Sloc, Source_Ptr),
        Sm (In_List, Flag),
        Sm (Rewrite_Ins, Flag),
        Sm (Comes_From_Source, Flag),
        Sm (Analyzed, Flag),
        Sm (Error_Posted, Flag),
        Sm (Small_Paren_Count, Small_Paren_Count_Type),
        Sm (Check_Actuals, Flag),
        Sm (Has_Aspects, Flag),
        Sm (Is_Ignored_Ghost_Node, Flag),
        Sm (Link, Union_Id)));

   Cc (N_Unused_At_Start, Node_Kind);

   Ab (N_Representation_Clause, Node_Kind);

   Cc (N_At_Clause, N_Representation_Clause,
       (Sy (Identifier, Node_Id, Default_Empty),
        Sy (Expression, Node_Id, Default_Empty)));

   Cc (N_Component_Clause, N_Representation_Clause,
       (Sy (Component_Name, Node_Id),
        Sy (Position, Node_Id),
        Sy (First_Bit, Node_Id),
        Sy (Last_Bit, Node_Id)));

   Cc (N_Enumeration_Representation_Clause, N_Representation_Clause,
       (Sy (Identifier, Node_Id, Default_Empty),
        Sy (Array_Aggregate, Node_Id),
        Sm (Next_Rep_Item, Node_Id)));

   Cc (N_Mod_Clause, N_Representation_Clause,
       (Sy (Expression, Node_Id, Default_Empty),
        Sy (Pragmas_Before, List_Id, Default_No_List)));

   Cc (N_Record_Representation_Clause, N_Representation_Clause,
       (Sy (Identifier, Node_Id, Default_Empty),
        Sy (Mod_Clause, Node_Id, Default_Empty),
        Sy (Component_Clauses, List_Id),
        Sm (Next_Rep_Item, Node_Id)));

   Cc (N_Attribute_Definition_Clause, N_Representation_Clause,
       (Sy (Name, Node_Id, Default_Empty),
        Sy (Chars, Name_Id, Default_No_Name),
        Sy (Expression, Node_Id, Default_Empty),
        Sm (Address_Warning_Posted, Flag),
        Sm (Check_Address_Alignment, Flag),
        Sm (Entity_Or_Associated_Node, Node_Id), -- just Entity
        Sm (From_Aspect_Specification, Flag),
        Sm (From_At_Mod, Flag),
        Sm (Is_Delayed_Aspect, Flag),
        Sm (Next_Rep_Item, Node_Id)));

   Cc (N_Empty, Node_Kind,
       (Sy (Chars, Name_Id, Default_No_Name)));
   --  The following getters and setters are called on Empty,
   --  and are currently inherited from Node_Kind:
   --
   --  Set_Comes_From_Source
   --  Set_Sloc
   --
   --  Comes_From_Source
   --  Error_Posted
   --  In_List
   --  Link
   --  Rewrite_Ins
   --  Sloc
   --  Small_Paren_Count

   Cc (N_Pragma_Argument_Association, Node_Kind,
       (Sy (Chars, Name_Id, Default_No_Name),
        Sy (Expression, Node_Id, Default_Empty),
        Sm (Expression_Copy, Node_Id)));

   Ab (N_Has_Etype, Node_Kind,
       (Sm (Etype, Node_Id)));

   Cc (N_Error, N_Has_Etype,
       (Sy (Chars, Name_Id, Default_No_Name)));

   Ab (N_Entity, N_Has_Etype,
       (Sm (Next_Entity, Node_Id),
        Sm (Scope, Node_Id)));

   Cc (N_Defining_Character_Literal, N_Entity,
       (Sy (Chars, Name_Id, Default_No_Name)));

   Cc (N_Defining_Identifier, N_Entity,
       (Sy (Chars, Name_Id, Default_No_Name)));

   Cc (N_Defining_Operator_Symbol, N_Entity,
       (Sy (Chars, Name_Id, Default_No_Name)));

   Ab (N_Subexpr, N_Has_Etype,
   --  Nodes with expression fields
       (Sm (Assignment_OK, Flag),
        Sm (Do_Range_Check, Flag),
        Sm (Has_Dynamic_Length_Check, Flag),
        Sm (Is_Controlling_Actual, Flag),
        Sm (Is_Overloaded, Flag),
        Sm (Is_Static_Expression, Flag),
        Sm (Must_Not_Freeze, Flag),
        Sm (Raises_Constraint_Error, Flag)));

   Ab (N_Has_Entity, N_Subexpr,
   --  Nodes that have Entity fields
   --  Warning: DOES NOT INCLUDE N_Freeze_Entity, N_Freeze_Generic_Entity,
   --  N_Aspect_Specification, or N_Attribute_Definition_Clause.
       (Sm (Entity_Or_Associated_Node, Node_Id))); -- both

   Cc (N_Expanded_Name, N_Has_Entity,
       (Sy (Chars, Name_Id, Default_No_Name),
        Sy (Prefix, Node_Id),
        Sy (Selector_Name, Node_Id, Default_Empty),
        Sm (Atomic_Sync_Required, Flag),
        Sm (Has_Private_View, Flag),
        Sm (Is_Elaboration_Checks_OK_Node, Flag),
        Sm (Is_Elaboration_Warnings_OK_Node, Flag),
        Sm (Is_SPARK_Mode_On_Node, Flag),
        Sm (Redundant_Use, Flag)));

   Ab (N_Direct_Name, N_Has_Entity,
       (Sm (Has_Private_View, Flag)));

   Cc (N_Identifier, N_Direct_Name,
       (Sy (Chars, Name_Id, Default_No_Name),
        Sm (Atomic_Sync_Required, Flag),
        Sm (Is_Elaboration_Checks_OK_Node, Flag),
        Sm (Is_Elaboration_Warnings_OK_Node, Flag),
        Sm (Is_SPARK_Mode_On_Node, Flag),
        Sm (Original_Discriminant, Node_Id),
        Sm (Redundant_Use, Flag)));

   Cc (N_Operator_Symbol, N_Direct_Name,
       (Sy (Chars, Name_Id, Default_No_Name),
        Sy (Strval, String_Id)));

   Cc (N_Character_Literal, N_Direct_Name,
       (Sy (Chars, Name_Id, Default_No_Name),
        Sy (Char_Literal_Value, Unat)));

   Ab (N_Op, N_Has_Entity,
       (Sm (Do_Overflow_Check, Flag),
        Sm (Has_Private_View, Flag)));

   Ab (N_Binary_Op, N_Op);

   Cc (N_Op_Add, N_Binary_Op,
       (Sm (Chars, Name_Id),
        Sy (Left_Opnd, Node_Id),
        Sy (Right_Opnd, Node_Id)));

   Cc (N_Op_Concat, N_Binary_Op,
       (Sm (Chars, Name_Id),
        Sy (Left_Opnd, Node_Id),
        Sy (Right_Opnd, Node_Id),
        Sm (Is_Component_Left_Opnd, Flag),
        Sm (Is_Component_Right_Opnd, Flag)));

   Cc (N_Op_Expon, N_Binary_Op,
       (Sm (Chars, Name_Id),
        Sy (Left_Opnd, Node_Id),
        Sy (Right_Opnd, Node_Id),
        Sm (Is_Power_Of_2_For_Shift, Flag)));

   Cc (N_Op_Subtract, N_Binary_Op,
       (Sm (Chars, Name_Id),
        Sy (Left_Opnd, Node_Id),
        Sy (Right_Opnd, Node_Id)));

   Ab (N_Multiplying_Operator, N_Binary_Op);

   Cc (N_Op_Divide, N_Multiplying_Operator,
       (Sm (Chars, Name_Id),
        Sy (Left_Opnd, Node_Id),
        Sy (Right_Opnd, Node_Id),
        Sm (Do_Division_Check, Flag),
        Sm (Rounded_Result, Flag)));

   Cc (N_Op_Mod, N_Multiplying_Operator,
       (Sm (Chars, Name_Id),
        Sy (Left_Opnd, Node_Id),
        Sy (Right_Opnd, Node_Id),
        Sm (Do_Division_Check, Flag)));

   Cc (N_Op_Multiply, N_Multiplying_Operator,
       (Sm (Chars, Name_Id),
        Sy (Left_Opnd, Node_Id),
        Sy (Right_Opnd, Node_Id),
        Sm (Rounded_Result, Flag)));

   Cc (N_Op_Rem, N_Multiplying_Operator,
       (Sm (Chars, Name_Id),
        Sy (Left_Opnd, Node_Id),
        Sy (Right_Opnd, Node_Id),
        Sm (Do_Division_Check, Flag)));

   Ab (N_Op_Boolean, N_Binary_Op);
   --  Binary operators that take operands of a boolean type, and yield a
   --  result of a boolean type.

   Cc (N_Op_And, N_Op_Boolean,
       (Sm (Chars, Name_Id),
        Sy (Left_Opnd, Node_Id),
        Sy (Right_Opnd, Node_Id),
        Sm (Do_Length_Check, Flag)));

   Ab (N_Op_Compare, N_Op_Boolean);

   Cc (N_Op_Eq, N_Op_Compare,
       (Sm (Chars, Name_Id),
        Sy (Left_Opnd, Node_Id),
        Sy (Right_Opnd, Node_Id)));

   Cc (N_Op_Ge, N_Op_Compare,
       (Sm (Chars, Name_Id),
        Sy (Left_Opnd, Node_Id),
        Sy (Right_Opnd, Node_Id)));

   Cc (N_Op_Gt, N_Op_Compare,
       (Sm (Chars, Name_Id),
        Sy (Left_Opnd, Node_Id),
        Sy (Right_Opnd, Node_Id)));

   Cc (N_Op_Le, N_Op_Compare,
       (Sm (Chars, Name_Id),
        Sy (Left_Opnd, Node_Id),
        Sy (Right_Opnd, Node_Id)));

   Cc (N_Op_Lt, N_Op_Compare,
       (Sm (Chars, Name_Id),
        Sy (Left_Opnd, Node_Id),
        Sy (Right_Opnd, Node_Id)));

   Cc (N_Op_Ne, N_Op_Compare,
       (Sm (Chars, Name_Id),
        Sy (Left_Opnd, Node_Id),
        Sy (Right_Opnd, Node_Id)));

   Cc (N_Op_Or, N_Op_Boolean,
       (Sm (Chars, Name_Id),
        Sy (Left_Opnd, Node_Id),
        Sy (Right_Opnd, Node_Id),
        Sm (Do_Length_Check, Flag)));

   Cc (N_Op_Xor, N_Op_Boolean,
       (Sm (Chars, Name_Id),
        Sy (Left_Opnd, Node_Id),
        Sy (Right_Opnd, Node_Id),
        Sm (Do_Length_Check, Flag)));

   Ab (N_Op_Shift, N_Binary_Op,
       (Sm (Shift_Count_OK, Flag)));

   Cc (N_Op_Rotate_Left, N_Op_Shift,
       (Sm (Chars, Name_Id),
        Sy (Left_Opnd, Node_Id),
        Sy (Right_Opnd, Node_Id)));

   Cc (N_Op_Rotate_Right, N_Op_Shift,
       (Sm (Chars, Name_Id),
        Sy (Left_Opnd, Node_Id),
        Sy (Right_Opnd, Node_Id)));

   Cc (N_Op_Shift_Left, N_Op_Shift,
       (Sm (Chars, Name_Id),
        Sy (Left_Opnd, Node_Id),
        Sy (Right_Opnd, Node_Id)));

   Cc (N_Op_Shift_Right, N_Op_Shift,
       (Sm (Chars, Name_Id),
        Sy (Left_Opnd, Node_Id),
        Sy (Right_Opnd, Node_Id)));

   Cc (N_Op_Shift_Right_Arithmetic, N_Op_Shift,
       (Sm (Chars, Name_Id),
        Sy (Left_Opnd, Node_Id),
        Sy (Right_Opnd, Node_Id)));

   Ab (N_Unary_Op, N_Op);

   Cc (N_Op_Abs, N_Unary_Op,
       (Sm (Chars, Name_Id),
        Sy (Right_Opnd, Node_Id)));

   Cc (N_Op_Minus, N_Unary_Op,
       (Sm (Chars, Name_Id),
        Sy (Right_Opnd, Node_Id)));

   Cc (N_Op_Not, N_Unary_Op,
       (Sm (Chars, Name_Id),
        Sy (Right_Opnd, Node_Id)));

   Cc (N_Op_Plus, N_Unary_Op,
       (Sm (Chars, Name_Id),
        Sy (Right_Opnd, Node_Id)));

   Cc (N_Attribute_Reference, N_Has_Entity,
       (Sy (Prefix, Node_Id),
        Sy (Attribute_Name, Name_Id),
        Sy (Expressions, List_Id, Default_No_List),
        Sm (Do_Overflow_Check, Flag),
        Sm (Header_Size_Added, Flag),
        Sm (Is_Elaboration_Checks_OK_Node, Flag),
        Sm (Is_Elaboration_Warnings_OK_Node, Flag),
        Sm (Is_SPARK_Mode_On_Node, Flag),
        Sm (Must_Be_Byte_Aligned, Flag),
        Sm (Redundant_Use, Flag)));

   Ab (N_Membership_Test, N_Subexpr);

   Cc (N_In, N_Membership_Test,
       (Sy (Left_Opnd, Node_Id),
        Sy (Right_Opnd, Node_Id),
        Sy (Alternatives, List_Id, Default_No_List),
        Sy (No_Minimize_Eliminate, Flag)));

   Cc (N_Not_In, N_Membership_Test,
       (Sy (Left_Opnd, Node_Id),
        Sy (Right_Opnd, Node_Id),
        Sy (Alternatives, List_Id, Default_No_List),
        Sy (No_Minimize_Eliminate, Flag)));

   Ab (N_Short_Circuit, N_Subexpr);

   Cc (N_And_Then, N_Short_Circuit,
       (Sy (Left_Opnd, Node_Id),
        Sy (Right_Opnd, Node_Id),
        Sm (Actions, List_Id)));

   Cc (N_Or_Else, N_Short_Circuit,
       (Sy (Left_Opnd, Node_Id),
        Sy (Right_Opnd, Node_Id),
        Sm (Actions, List_Id)));

   Ab (N_Subprogram_Call, N_Subexpr,
       (Sm (Controlling_Argument, Node_Id),
        Sm (First_Named_Actual, Node_Id),
        Sm (Is_Elaboration_Checks_OK_Node, Flag),
        Sm (Is_Elaboration_Warnings_OK_Node, Flag),
        Sm (Is_Known_Guaranteed_ABE, Flag),
        Sm (Is_SPARK_Mode_On_Node, Flag),
        Sm (No_Elaboration_Check, Flag)));

   Cc (N_Function_Call, N_Subprogram_Call,
       (Sy (Name, Node_Id, Default_Empty),
        Sy (Parameter_Associations, List_Id, Default_No_List),
        Sm (Is_Expanded_Build_In_Place_Call, Flag),
        Sm (No_Side_Effect_Removal, Flag)));

   Cc (N_Procedure_Call_Statement, N_Subprogram_Call,
       (Sy (Name, Node_Id, Default_Empty),
        Sy (Parameter_Associations, List_Id, Default_No_List)));

   Ab (N_Raise_xxx_Error, N_Subexpr);

   Cc (N_Raise_Constraint_Error, N_Raise_xxx_Error,
       (Sy (Condition, Node_Id, Default_Empty),
        Sy (Reason, Unat)));

   Cc (N_Raise_Program_Error, N_Raise_xxx_Error,
       (Sy (Condition, Node_Id, Default_Empty),
        Sy (Reason, Unat)));

   Cc (N_Raise_Storage_Error, N_Raise_xxx_Error,
       (Sy (Condition, Node_Id, Default_Empty),
        Sy (Reason, Unat)));

   Ab (N_Numeric_Or_String_Literal, N_Subexpr);

   Cc (N_Integer_Literal, N_Numeric_Or_String_Literal,
       (Sy (Intval, Valid_Uint),
        Sm (Original_Entity, Node_Id),
        Sm (Print_In_Hex, Flag)));

   Cc (N_Real_Literal, N_Numeric_Or_String_Literal,
       (Sy (Realval, Ureal),
        Sm (Corresponding_Integer_Value, Valid_Uint),
        Sm (Is_Machine_Number, Flag),
        Sm (Original_Entity, Node_Id)));

   Cc (N_String_Literal, N_Numeric_Or_String_Literal,
       (Sy (Strval, String_Id),
        Sy (Is_Folded_In_Parser, Flag),
        Sm (Has_Wide_Character, Flag),
        Sm (Has_Wide_Wide_Character, Flag)));

   Cc (N_Interpolated_String_Literal, N_Numeric_Or_String_Literal,
       (Sy (Expressions, List_Id, Default_No_List)));

   Cc (N_Explicit_Dereference, N_Subexpr,
       (Sy (Prefix, Node_Id),
        Sm (Actual_Designated_Subtype, Node_Id),
        Sm (Atomic_Sync_Required, Flag),
        Sm (Has_Dereference_Action, Flag)));

   Cc (N_Expression_With_Actions, N_Subexpr,
       (Sy (Actions, List_Id, Default_No_List),
        Sy (Expression, Node_Id, Default_Empty)));

   Cc (N_If_Expression, N_Subexpr,
       (Sy (Expressions, List_Id, Default_No_List),
        Sy (Is_Elsif, Flag),
        Sm (Do_Overflow_Check, Flag),
        Sm (Else_Actions, List_Id),
        Sm (Then_Actions, List_Id)));

   Cc (N_Indexed_Component, N_Subexpr,
       (Sy (Prefix, Node_Id),
        Sy (Expressions, List_Id, Default_No_List),
        Sm (Atomic_Sync_Required, Flag),
        Sm (Generalized_Indexing, Node_Id)));

   Cc (N_Null, N_Subexpr);

   Cc (N_Qualified_Expression, N_Subexpr,
       (Sy (Subtype_Mark, Node_Id, Default_Empty),
        Sy (Expression, Node_Id, Default_Empty),
        Sm (Is_Qualified_Universal_Literal, Flag)));

   Cc (N_Quantified_Expression, N_Subexpr,
       (Sy (Iterator_Specification, Node_Id, Default_Empty),
        Sy (Loop_Parameter_Specification, Node_Id, Default_Empty),
        Sy (Condition, Node_Id, Default_Empty),
        Sy (All_Present, Flag)));

   Cc (N_Aggregate, N_Subexpr,
       (Sy (Expressions, List_Id, Default_No_List),
        Sy (Component_Associations, List_Id, Default_No_List),
        Sy (Null_Record_Present, Flag),
        Sy (Is_Parenthesis_Aggregate, Flag),
        Sy (Is_Homogeneous_Aggregate, Flag),
        Sy (Is_Enum_Array_Aggregate, Flag),
        Sm (Aggregate_Bounds, Node_Id),
        Sm (Entity_Or_Associated_Node, Node_Id), -- just Associated_Node
        Sm (Compile_Time_Known_Aggregate, Flag),
        Sm (Expansion_Delayed, Flag),
        Sm (Has_Self_Reference, Flag)));

   Cc (N_Allocator, N_Subexpr,
       (Sy (Expression, Node_Id, Default_Empty),
        Sy (Subpool_Handle_Name, Node_Id, Default_Empty),
        Sy (Null_Exclusion_Present, Flag, Default_False),
        Sm (For_Special_Return_Object, Flag),
        Sm (Do_Storage_Check, Flag),
        Sm (Is_Dynamic_Coextension, Flag),
        Sm (Is_Static_Coextension, Flag),
        Sm (No_Initialization, Flag),
        Sm (Procedure_To_Call, Node_Id),
        Sm (Storage_Pool, Node_Id)));

   Cc (N_Case_Expression, N_Subexpr,
       (Sy (Expression, Node_Id, Default_Empty),
        Sy (Alternatives, List_Id, Default_No_List),
        Sm (Do_Overflow_Check, Flag)));

   Cc (N_Delta_Aggregate, N_Subexpr,
       (Sy (Expression, Node_Id, Default_Empty),
        Sy (Is_Homogeneous_Aggregate, Flag),
        Sy (Component_Associations, List_Id, Default_No_List)));

   Cc (N_Extension_Aggregate, N_Subexpr,
       (Sy (Ancestor_Part, Node_Id),
        Sy (Expressions, List_Id, Default_No_List),
        Sy (Component_Associations, List_Id, Default_No_List),
        Sy (Null_Record_Present, Flag),
        Sm (Entity_Or_Associated_Node, Node_Id), -- just Associated_Node
        Sm (Expansion_Delayed, Flag),
        Sm (Has_Self_Reference, Flag)));

   Cc (N_Raise_Expression, N_Subexpr,
       (Sy (Name, Node_Id, Default_Empty),
        Sy (Expression, Node_Id, Default_Empty)));

   Cc (N_Range, N_Subexpr,
       (Sy (Low_Bound, Node_Id),
        Sy (High_Bound, Node_Id),
        Sy (Includes_Infinities, Flag)));

   Cc (N_Reference, N_Subexpr,
       (Sy (Prefix, Node_Id)));

   Cc (N_Selected_Component, N_Subexpr,
       (Sy (Prefix, Node_Id),
        Sy (Selector_Name, Node_Id, Default_Empty),
        Sm (Atomic_Sync_Required, Flag),
        Sm (Entity_Or_Associated_Node, Node_Id), -- just Associated_Node
        Sm (Do_Discriminant_Check, Flag),
        Sm (Is_In_Discriminant_Check, Flag),
        Sm (Is_Prefixed_Call, Flag)));

   Cc (N_Slice, N_Subexpr,
       (Sy (Prefix, Node_Id),
        Sy (Discrete_Range, Node_Id)));

   Cc (N_Target_Name, N_Subexpr);

   Cc (N_Type_Conversion, N_Subexpr,
       (Sy (Subtype_Mark, Node_Id, Default_Empty),
        Sy (Expression, Node_Id, Default_Empty),
        Sm (Conversion_OK, Flag),
        Sm (Do_Discriminant_Check, Flag),
        Sm (Do_Length_Check, Flag),
        Sm (Do_Overflow_Check, Flag),
        Sm (Float_Truncate, Flag),
        Sm (Rounded_Result, Flag)));

   Cc (N_Unchecked_Expression, N_Subexpr,
       (Sy (Expression, Node_Id, Default_Empty)));

   Cc (N_Unchecked_Type_Conversion, N_Subexpr,
       (Sy (Subtype_Mark, Node_Id, Default_Empty),
        Sy (Expression, Node_Id, Default_Empty),
        Sm (Kill_Range_Check, Flag),
        Sm (No_Truncation, Flag)),
       Nmake_Assert => "True or else Nkind (Expression) /= N_Unchecked_Type_Conversion");
--       Nmake_Assert => "Nkind (Expression) /= N_Unchecked_Type_Conversion");
   --  Assert that we don't have unchecked conversions of unchecked
   --  conversions; if Expression might be an unchecked conversion,
   --  then Tbuild.Unchecked_Convert_To should be used.

   Cc (N_Subtype_Indication, N_Has_Etype,
       (Sy (Subtype_Mark, Node_Id, Default_Empty),
        Sy (Constraint, Node_Id),
        Sm (Must_Not_Freeze, Flag)));

   Ab (N_Declaration, Node_Kind);
   --  Note: this includes all constructs normally thought of as declarations
   --  except those that are separately grouped in N_Later_Decl_Item. But
   --  Declaration_Node may return yet more node types; see N_Is_Decl below.

   Cc (N_Component_Declaration, N_Declaration,
       (Sy (Defining_Identifier, Node_Id),
        Sy (Component_Definition, Node_Id),
        Sy (Expression, Node_Id, Default_Empty),
        Sm (More_Ids, Flag),
        Sm (Prev_Ids, Flag)));

   Cc (N_Entry_Declaration, N_Declaration,
       (Sy (Defining_Identifier, Node_Id),
        Sy (Discrete_Subtype_Definition, Node_Id, Default_Empty),
        Sy (Parameter_Specifications, List_Id, Default_No_List),
        Sy (Must_Override, Flag),
        Sy (Must_Not_Override, Flag),
        Sm (Corresponding_Body, Node_Id)));

   Cc (N_Expression_Function, N_Declaration,
       (Sy (Specification, Node_Id),
        Sy (Expression, Node_Id, Default_Empty),
        Sm (Corresponding_Spec, Node_Id)));

   Cc (N_Formal_Object_Declaration, N_Declaration,
       (Sy (Defining_Identifier, Node_Id),
        Sy (In_Present, Flag),
        Sy (Out_Present, Flag),
        Sy (Null_Exclusion_Present, Flag, Default_False),
        Sy (Subtype_Mark, Node_Id, Default_Empty),
        Sy (Access_Definition, Node_Id, Default_Empty),
        Sy (Default_Expression, Node_Id, Default_Empty),
        Sm (More_Ids, Flag),
        Sm (Prev_Ids, Flag)));

   Cc (N_Formal_Type_Declaration, N_Declaration,
       (Sy (Defining_Identifier, Node_Id),
        Sy (Formal_Type_Definition, Node_Id),
        Sy (Discriminant_Specifications, List_Id, Default_No_List),
        Sy (Unknown_Discriminants_Present, Flag),
        Sy (Default_Subtype_Mark, Node_Id)));

   Cc (N_Full_Type_Declaration, N_Declaration,
       (Sy (Defining_Identifier, Node_Id),
        Sy (Discriminant_Specifications, List_Id, Default_No_List),
        Sy (Type_Definition, Node_Id),
        Sm (Discr_Check_Funcs_Built, Flag),
        Sm (Incomplete_View, Node_Id)));

   Cc (N_Incomplete_Type_Declaration, N_Declaration,
       (Sy (Defining_Identifier, Node_Id),
        Sy (Discriminant_Specifications, List_Id, Default_No_List),
        Sy (Unknown_Discriminants_Present, Flag),
        Sy (Tagged_Present, Flag),
        Sm (Premature_Use, Node_Id)));

   Cc (N_Iterator_Specification, N_Declaration,
       (Sy (Defining_Identifier, Node_Id),
        Sy (Name, Node_Id, Default_Empty),
        Sy (Reverse_Present, Flag),
        Sy (Of_Present, Flag),
        Sy (Iterator_Filter, Node_Id, Default_Empty),
        Sy (Subtype_Indication, Node_Id, Default_Empty)));

   Cc (N_Loop_Parameter_Specification, N_Declaration,
       (Sy (Defining_Identifier, Node_Id),
        Sy (Reverse_Present, Flag),
        Sy (Iterator_Filter, Node_Id, Default_Empty),
        Sy (Discrete_Subtype_Definition, Node_Id, Default_Empty)));

   Cc (N_Object_Declaration, N_Declaration,
       (Sy (Defining_Identifier, Node_Id),
        Sy (Aliased_Present, Flag),
        Sy (Constant_Present, Flag),
        Sy (Null_Exclusion_Present, Flag, Default_False),
        Sy (Object_Definition, Node_Id),
        Sy (Expression, Node_Id, Default_Empty),
        Sy (Has_Init_Expression, Flag),
        Sm (Assignment_OK, Flag),
        Sm (Corresponding_Generic_Association, Node_Id),
        Sm (Exception_Junk, Flag),
        Sm (Handler_List_Entry, Node_Id),
        Sm (Is_Subprogram_Descriptor, Flag),
        Sm (More_Ids, Flag),
        Sm (No_Initialization, Flag),
        Sm (Prev_Ids, Flag),
        Sm (Suppress_Assignment_Checks, Flag)));

   Cc (N_Protected_Type_Declaration, N_Declaration,
       (Sy (Defining_Identifier, Node_Id),
        Sy (Discriminant_Specifications, List_Id, Default_No_List),
        Sy (Interface_List, List_Id, Default_No_List),
        Sy (Protected_Definition, Node_Id),
        Sm (Corresponding_Body, Node_Id)));

   Cc (N_Private_Extension_Declaration, N_Declaration,
       (Sy (Defining_Identifier, Node_Id),
        Sy (Discriminant_Specifications, List_Id, Default_No_List),
        Sy (Unknown_Discriminants_Present, Flag),
        Sy (Abstract_Present, Flag),
        Sy (Limited_Present, Flag),
        Sy (Synchronized_Present, Flag),
        Sy (Subtype_Indication, Node_Id, Default_Empty),
        Sy (Interface_List, List_Id, Default_No_List),
        Sm (Uninitialized_Variable, Node_Id)));

   Cc (N_Private_Type_Declaration, N_Declaration,
       (Sy (Defining_Identifier, Node_Id),
        Sy (Discriminant_Specifications, List_Id, Default_No_List),
        Sy (Unknown_Discriminants_Present, Flag),
        Sy (Abstract_Present, Flag),
        Sy (Tagged_Present, Flag),
        Sy (Limited_Present, Flag)));

   Cc (N_Subtype_Declaration, N_Declaration,
       (Sy (Defining_Identifier, Node_Id),
        Sy (Null_Exclusion_Present, Flag, Default_False),
        Sy (Subtype_Indication, Node_Id, Default_Empty),
        Sm (Exception_Junk, Flag),
        Sm (Generic_Parent_Type, Node_Id)));

   Ab (N_Subprogram_Specification, N_Declaration,
       (Sm (Generic_Parent, Node_Id)));

   Cc (N_Function_Specification, N_Subprogram_Specification,
       (Sy (Defining_Unit_Name, Node_Id),
        Sy (Parameter_Specifications, List_Id, Default_No_List),
        Sy (Null_Exclusion_Present, Flag, Default_False),
        Sy (Result_Definition, Node_Id),
        Sy (Must_Override, Flag),
        Sy (Must_Not_Override, Flag)));

   Cc (N_Procedure_Specification, N_Subprogram_Specification,
       (Sy (Defining_Unit_Name, Node_Id),
        Sy (Parameter_Specifications, List_Id, Default_No_List),
        Sy (Null_Present, Flag),
        Sy (Must_Override, Flag),
        Sy (Must_Not_Override, Flag),
        Sm (Null_Statement, Node_Id)));

   Ab (N_Access_To_Subprogram_Definition, Node_Kind);

   Cc (N_Access_Function_Definition, N_Access_To_Subprogram_Definition,
       (Sy (Null_Exclusion_Present, Flag, Default_False),
        Sy (Null_Exclusion_In_Return_Present, Flag),
        Sy (Protected_Present, Flag),
        Sy (Parameter_Specifications, List_Id, Default_No_List),
        Sy (Result_Definition, Node_Id)));

   Cc (N_Access_Procedure_Definition, N_Access_To_Subprogram_Definition,
       (Sy (Null_Exclusion_Present, Flag, Default_False),
        Sy (Protected_Present, Flag),
        Sy (Parameter_Specifications, List_Id, Default_No_List)));

   Ab (N_Later_Decl_Item, Node_Kind);
   --  Note: this is Ada 83 relevant only (see Ada 83 RM 3.9 (2)) and includes
   --  only those items which can appear as later declarative items. This also
   --  includes N_Implicit_Label_Declaration which is not specifically in the
   --  grammar but may appear as a valid later declarative items. It does NOT
   --  include N_Pragma which can also appear among later declarative items.
   --  It does however include N_Protected_Body, which is a bit peculiar, but
   --  harmless since this cannot appear in Ada 83 mode anyway.

   Cc (N_Task_Type_Declaration, N_Later_Decl_Item,
       (Sy (Defining_Identifier, Node_Id),
        Sy (Discriminant_Specifications, List_Id, Default_No_List),
        Sy (Interface_List, List_Id, Default_No_List),
        Sy (Task_Definition, Node_Id, Default_Empty),
        Sm (Corresponding_Body, Node_Id)));

   Ab (N_Body_Stub, N_Later_Decl_Item,
       (Sm (Corresponding_Body, Node_Id),
        Sm (Corresponding_Spec_Of_Stub, Node_Id),
        Sm (Library_Unit, Node_Id)));

   Cc (N_Package_Body_Stub, N_Body_Stub,
       (Sy (Defining_Identifier, Node_Id)));

   Cc (N_Protected_Body_Stub, N_Body_Stub,
       (Sy (Defining_Identifier, Node_Id)));

   Cc (N_Subprogram_Body_Stub, N_Body_Stub,
       (Sy (Specification, Node_Id)));

   Cc (N_Task_Body_Stub, N_Body_Stub,
       (Sy (Defining_Identifier, Node_Id)));

   Ab (N_Generic_Instantiation, N_Later_Decl_Item,
       (Sm (Instance_Spec, Node_Id),
        Sm (Is_Declaration_Level_Node, Flag),
        Sm (Is_Elaboration_Checks_OK_Node, Flag),
        Sm (Is_Elaboration_Warnings_OK_Node, Flag),
        Sm (Is_Known_Guaranteed_ABE, Flag),
        Sm (Is_SPARK_Mode_On_Node, Flag),
        Sm (Parent_Spec, Node_Id)));

   Ab (N_Subprogram_Instantiation, N_Generic_Instantiation);

   Cc (N_Function_Instantiation, N_Subprogram_Instantiation,
       (Sy (Defining_Unit_Name, Node_Id),
        Sy (Name, Node_Id, Default_Empty),
        Sy (Generic_Associations, List_Id, Default_No_List),
        Sy (Must_Override, Flag),
        Sy (Must_Not_Override, Flag)));

   Cc (N_Procedure_Instantiation, N_Subprogram_Instantiation,
       (Sy (Defining_Unit_Name, Node_Id),
        Sy (Name, Node_Id, Default_Empty),
        Sy (Generic_Associations, List_Id, Default_No_List),
        Sy (Must_Override, Flag),
        Sy (Must_Not_Override, Flag)));

   Cc (N_Package_Instantiation, N_Generic_Instantiation,
       (Sy (Defining_Unit_Name, Node_Id),
        Sy (Name, Node_Id, Default_Empty),
        Sy (Generic_Associations, List_Id, Default_No_List)));

   Ab (N_Proper_Body, N_Later_Decl_Item,
       (Sm (Corresponding_Spec, Node_Id),
        Sm (Was_Originally_Stub, Flag)));

   Ab (N_Unit_Body, N_Proper_Body);

   Cc (N_Package_Body, N_Unit_Body,
       (Sy (Defining_Unit_Name, Node_Id),
        Sy (Declarations, List_Id, Default_No_List),
        Sy (Handled_Statement_Sequence, Node_Id, Default_Empty),
        Sy (At_End_Proc, Node_Id, Default_Empty)));

   Cc (N_Subprogram_Body, N_Unit_Body,
       (Sy (Specification, Node_Id),
        Sy (Declarations, List_Id, Default_No_List),
        Sy (Handled_Statement_Sequence, Node_Id, Default_Empty),
        Sy (Bad_Is_Detected, Flag),
        Sy (At_End_Proc, Node_Id, Default_Empty),
        Sm (Activation_Chain_Entity, Node_Id),
        Sm (Acts_As_Spec, Flag),
        Sm (Corresponding_Entry_Body, Node_Id),
        Sm (Do_Storage_Check, Flag),
        Sm (Has_Relative_Deadline_Pragma, Flag),
        Sm (Is_Entry_Barrier_Function, Flag),
        Sm (Is_Protected_Subprogram_Body, Flag),
        Sm (Is_Task_Body_Procedure, Flag),
        Sm (Is_Task_Master, Flag),
        Sm (Was_Attribute_Reference, Flag),
        Sm (Was_Expression_Function, Flag)));

   Cc (N_Protected_Body, N_Proper_Body,
       (Sy (Defining_Identifier, Node_Id),
        Sy (Declarations, List_Id, Default_No_List),
        Sy (End_Label, Node_Id, Default_Empty)));

   Cc (N_Task_Body, N_Proper_Body,
       (Sy (Defining_Identifier, Node_Id),
        Sy (Declarations, List_Id, Default_No_List),
        Sy (Handled_Statement_Sequence, Node_Id, Default_Empty),
        Sy (At_End_Proc, Node_Id, Default_Empty),
        Sm (Activation_Chain_Entity, Node_Id),
        Sm (Is_Task_Master, Flag)));

   Cc (N_Implicit_Label_Declaration, N_Later_Decl_Item,
       (Sy (Defining_Identifier, Node_Id),
        Sm (Label_Construct, Node_Id)));

   Cc (N_Package_Declaration, N_Later_Decl_Item,
       (Sy (Specification, Node_Id),
        Sm (Activation_Chain_Entity, Node_Id),
        Sm (Corresponding_Body, Node_Id),
        Sm (Parent_Spec, Node_Id)));

   Cc (N_Single_Task_Declaration, N_Later_Decl_Item,
       (Sy (Defining_Identifier, Node_Id),
        Sy (Interface_List, List_Id, Default_No_List),
        Sy (Task_Definition, Node_Id, Default_Empty)));

   Cc (N_Subprogram_Declaration, N_Later_Decl_Item,
       (Sy (Specification, Node_Id),
        Sm (Body_To_Inline, Node_Id),
        Sm (Corresponding_Body, Node_Id),
        Sm (Is_Entry_Barrier_Function, Flag),
        Sm (Is_Task_Body_Procedure, Flag),
        Sm (Parent_Spec, Node_Id)));

   Cc (N_Use_Package_Clause, N_Later_Decl_Item,
       (Sy (Name, Node_Id, Default_Empty),
        Sy (Is_Effective_Use_Clause, Flag),
        Sm (Entity_Or_Associated_Node, Node_Id), -- just Associated_Node
        Sm (Hidden_By_Use_Clause, Elist_Id),
        Sm (More_Ids, Flag),
        Sm (Next_Use_Clause, Node_Id),
        Sm (Prev_Ids, Flag),
        Sm (Prev_Use_Clause, Node_Id)));

   Ab (N_Generic_Declaration, N_Later_Decl_Item,
       (Sm (Corresponding_Body, Node_Id),
        Sm (Parent_Spec, Node_Id)));

   Cc (N_Generic_Package_Declaration, N_Generic_Declaration,
       (Sy (Specification, Node_Id),
        Sy (Generic_Formal_Declarations, List_Id),
        Sm (Activation_Chain_Entity, Node_Id)));

   Cc (N_Generic_Subprogram_Declaration, N_Generic_Declaration,
       (Sy (Specification, Node_Id),
        Sy (Generic_Formal_Declarations, List_Id)));

   Ab (N_Array_Type_Definition, Node_Kind);

   Cc (N_Constrained_Array_Definition, N_Array_Type_Definition,
       (Sy (Discrete_Subtype_Definitions, List_Id),
        Sy (Component_Definition, Node_Id)));

   Cc (N_Unconstrained_Array_Definition, N_Array_Type_Definition,
       (Sy (Subtype_Marks, List_Id),
        Sy (Component_Definition, Node_Id)));

   Ab (N_Renaming_Declaration, Node_Kind);

   Cc (N_Exception_Renaming_Declaration, N_Renaming_Declaration,
       (Sy (Defining_Identifier, Node_Id),
        Sy (Name, Node_Id, Default_Empty)));

   Cc (N_Object_Renaming_Declaration, N_Renaming_Declaration,
       (Sy (Defining_Identifier, Node_Id),
        Sy (Null_Exclusion_Present, Flag, Default_False),
        Sy (Subtype_Mark, Node_Id, Default_Empty),
        Sy (Access_Definition, Node_Id, Default_Empty),
        Sy (Name, Node_Id, Default_Empty),
        Sm (Comes_From_Iterator, Flag),
        Sm (Corresponding_Generic_Association, Node_Id)));

   Cc (N_Package_Renaming_Declaration, N_Renaming_Declaration,
       (Sy (Defining_Unit_Name, Node_Id),
        Sy (Name, Node_Id, Default_Empty),
        Sm (Parent_Spec, Node_Id)));

   Cc (N_Subprogram_Renaming_Declaration, N_Renaming_Declaration,
       (Sy (Specification, Node_Id),
        Sy (Name, Node_Id, Default_Empty),
        Sm (Corresponding_Formal_Spec, Node_Id),
        Sm (Corresponding_Spec, Node_Id),
        Sm (From_Default, Flag),
        Sm (Parent_Spec, Node_Id)));

   Ab (N_Generic_Renaming_Declaration, N_Renaming_Declaration,
       (Sm (Parent_Spec, Node_Id)));

   Cc (N_Generic_Function_Renaming_Declaration, N_Generic_Renaming_Declaration,
       (Sy (Defining_Unit_Name, Node_Id),
        Sy (Name, Node_Id, Default_Empty)));

   Cc (N_Generic_Package_Renaming_Declaration, N_Generic_Renaming_Declaration,
       (Sy (Defining_Unit_Name, Node_Id),
        Sy (Name, Node_Id, Default_Empty)));

   Cc (N_Generic_Procedure_Renaming_Declaration, N_Generic_Renaming_Declaration,
       (Sy (Defining_Unit_Name, Node_Id),
        Sy (Name, Node_Id, Default_Empty)));

   Ab (N_Statement_Other_Than_Procedure_Call, Node_Kind);
   --  Note that this includes all statement types except for the cases of the
   --  N_Procedure_Call_Statement which is considered to be a subexpression
   --  (since overloading is possible, so it needs to go through the normal
   --  overloading resolution for expressions).

   Cc (N_Abort_Statement, N_Statement_Other_Than_Procedure_Call,
       (Sy (Names, List_Id)));

   Cc (N_Accept_Statement, N_Statement_Other_Than_Procedure_Call,
       (Sy (Entry_Direct_Name, Node_Id),
        Sy (Entry_Index, Node_Id, Default_Empty),
        Sy (Parameter_Specifications, List_Id, Default_No_List),
        Sy (Handled_Statement_Sequence, Node_Id, Default_Empty),
        Sy (Declarations, List_Id, Default_No_List)));

   Cc (N_Assignment_Statement, N_Statement_Other_Than_Procedure_Call,
       (Sy (Name, Node_Id, Default_Empty),
        Sy (Expression, Node_Id, Default_Empty),
        Sm (Backwards_OK, Flag),
        Sm (Componentwise_Assignment, Flag),
        Sm (Do_Discriminant_Check, Flag),
        Sm (Do_Length_Check, Flag),
        Sm (Forwards_OK, Flag),
        Sm (Has_Target_Names, Flag),
        Sm (Is_Elaboration_Checks_OK_Node, Flag),
        Sm (Is_Elaboration_Code, Flag),
        Sm (Is_SPARK_Mode_On_Node, Flag),
        Sm (No_Ctrl_Actions, Flag),
        Sm (Suppress_Assignment_Checks, Flag)));

   Cc (N_Asynchronous_Select, N_Statement_Other_Than_Procedure_Call,
       (Sy (Triggering_Alternative, Node_Id),
        Sy (Abortable_Part, Node_Id)));

   Cc (N_Block_Statement, N_Statement_Other_Than_Procedure_Call,
       (Sy (Identifier, Node_Id, Default_Empty),
        Sy (Declarations, List_Id, Default_No_List),
        Sy (Handled_Statement_Sequence, Node_Id, Default_Empty),
        Sy (Has_Created_Identifier, Flag),
        Sy (Is_Asynchronous_Call_Block, Flag),
        Sy (Is_Task_Allocation_Block, Flag),
        Sy (At_End_Proc, Node_Id, Default_Empty),
        Sm (Activation_Chain_Entity, Node_Id),
        Sm (Cleanup_Actions, List_Id),
        Sm (Exception_Junk, Flag),
        Sm (Is_Abort_Block, Flag),
        Sm (Is_Finalization_Wrapper, Flag),
        Sm (Is_Initialization_Block, Flag),
        Sm (Is_Task_Master, Flag)));

   Cc (N_Case_Statement, N_Statement_Other_Than_Procedure_Call,
       (Sy (Expression, Node_Id, Default_Empty),
        Sy (Alternatives, List_Id, Default_No_List),
        Sy (End_Span, Unat, Default_Uint_0),
        Sm (From_Conditional_Expression, Flag)));

   Cc (N_Code_Statement, N_Statement_Other_Than_Procedure_Call,
       (Sy (Expression, Node_Id, Default_Empty)));

   Cc (N_Compound_Statement, N_Statement_Other_Than_Procedure_Call,
       (Sy (Actions, List_Id, Default_No_List)));

   Cc (N_Conditional_Entry_Call, N_Statement_Other_Than_Procedure_Call,
       (Sy (Entry_Call_Alternative, Node_Id),
        Sy (Else_Statements, List_Id, Default_No_List)));

   Ab (N_Delay_Statement, N_Statement_Other_Than_Procedure_Call);

   Cc (N_Delay_Relative_Statement, N_Delay_Statement,
       (Sy (Expression, Node_Id, Default_Empty)));

   Cc (N_Delay_Until_Statement, N_Delay_Statement,
       (Sy (Expression, Node_Id, Default_Empty)));

   Cc (N_Entry_Call_Statement, N_Statement_Other_Than_Procedure_Call,
       (Sy (Name, Node_Id, Default_Empty),
        Sy (Parameter_Associations, List_Id, Default_No_List),
        Sm (First_Named_Actual, Node_Id),
        Sm (Is_Elaboration_Checks_OK_Node, Flag),
        Sm (Is_Elaboration_Warnings_OK_Node, Flag),
        Sm (Is_SPARK_Mode_On_Node, Flag)));

   Cc (N_Free_Statement, N_Statement_Other_Than_Procedure_Call,
       (Sy (Expression, Node_Id, Default_Empty),
        Sm (Actual_Designated_Subtype, Node_Id),
        Sm (Procedure_To_Call, Node_Id),
        Sm (Storage_Pool, Node_Id)));

   Cc (N_Goto_Statement, N_Statement_Other_Than_Procedure_Call,
       (Sy (Name, Node_Id, Default_Empty),
        Sm (Exception_Junk, Flag)));

   Cc (N_Goto_When_Statement, N_Statement_Other_Than_Procedure_Call,
       (Sy (Name, Node_Id, Default_Empty),
        Sy (Condition, Node_Id, Default_Empty)));

   Cc (N_Loop_Statement, N_Statement_Other_Than_Procedure_Call,
       (Sy (Identifier, Node_Id, Default_Empty),
        Sy (Iteration_Scheme, Node_Id, Default_Empty),
        Sy (Statements, List_Id, Default_Empty_List),
        Sy (End_Label, Node_Id, Default_Empty),
        Sy (Has_Created_Identifier, Flag),
        Sy (Is_Null_Loop, Flag),
        Sy (Suppress_Loop_Warnings, Flag)));

   Cc (N_Null_Statement, N_Statement_Other_Than_Procedure_Call,
       (Sm (Next_Rep_Item, Node_Id)));

   Cc (N_Raise_Statement, N_Statement_Other_Than_Procedure_Call,
       (Sy (Name, Node_Id, Default_Empty),
        Sy (Expression, Node_Id, Default_Empty)));

   Cc (N_Raise_When_Statement, N_Statement_Other_Than_Procedure_Call,
       (Sy (Name, Node_Id, Default_Empty),
        Sy (Expression, Node_Id, Default_Empty),
        Sy (Condition, Node_Id, Default_Empty)));

   Cc (N_Requeue_Statement, N_Statement_Other_Than_Procedure_Call,
       (Sy (Name, Node_Id, Default_Empty),
        Sy (Abort_Present, Flag),
        Sm (Is_Elaboration_Checks_OK_Node, Flag),
        Sm (Is_Elaboration_Warnings_OK_Node, Flag),
        Sm (Is_SPARK_Mode_On_Node, Flag)));

   Cc (N_Simple_Return_Statement, N_Statement_Other_Than_Procedure_Call,
       (Sy (Expression, Node_Id, Default_Empty),
        Sm (Comes_From_Extended_Return_Statement, Flag),
        Sm (Procedure_To_Call, Node_Id),
        Sm (Return_Statement_Entity, Node_Id),
        Sm (Storage_Pool, Node_Id)));

   Cc (N_Extended_Return_Statement, N_Statement_Other_Than_Procedure_Call,
       (Sy (Return_Object_Declarations, List_Id),
        Sy (Handled_Statement_Sequence, Node_Id, Default_Empty),
        Sm (Procedure_To_Call, Node_Id),
        Sm (Return_Statement_Entity, Node_Id),
        Sm (Storage_Pool, Node_Id)));

   Cc (N_Return_When_Statement, N_Statement_Other_Than_Procedure_Call,
       (Sy (Expression, Node_Id, Default_Empty),
        Sy (Condition, Node_Id, Default_Empty)));

   Cc (N_Selective_Accept, N_Statement_Other_Than_Procedure_Call,
       (Sy (Select_Alternatives, List_Id),
        Sy (Else_Statements, List_Id, Default_No_List)));

   Cc (N_Timed_Entry_Call, N_Statement_Other_Than_Procedure_Call,
       (Sy (Entry_Call_Alternative, Node_Id),
        Sy (Delay_Alternative, Node_Id)));

   Cc (N_Exit_Statement, N_Statement_Other_Than_Procedure_Call,
       (Sy (Name, Node_Id, Default_Empty),
        Sy (Condition, Node_Id, Default_Empty),
        Sm (Next_Exit_Statement, Node_Id)));

   Cc (N_If_Statement, N_Statement_Other_Than_Procedure_Call,
       (Sy (Condition, Node_Id, Default_Empty),
        Sy (Then_Statements, List_Id),
        Sy (Elsif_Parts, List_Id, Default_No_List),
        Sy (Else_Statements, List_Id, Default_No_List),
        Sy (End_Span, Unat, Default_Uint_0),
        Sm (From_Conditional_Expression, Flag),
        Sm (Comes_From_Check_Or_Contract, Flag)));

   Cc (N_Accept_Alternative, Node_Kind,
       (Sy (Accept_Statement, Node_Id),
        Sy (Condition, Node_Id, Default_Empty),
        Sy (Statements, List_Id, Default_Empty_List),
        Sy (Pragmas_Before, List_Id, Default_No_List),
        Sm (Accept_Handler_Records, List_Id)));

   Cc (N_Delay_Alternative, Node_Kind,
       (Sy (Delay_Statement, Node_Id),
        Sy (Condition, Node_Id, Default_Empty),
        Sy (Statements, List_Id, Default_Empty_List),
        Sy (Pragmas_Before, List_Id, Default_No_List)));

   Cc (N_Elsif_Part, Node_Kind,
       (Sy (Condition, Node_Id, Default_Empty),
        Sy (Then_Statements, List_Id),
        Sm (Condition_Actions, List_Id)));

   Cc (N_Entry_Body_Formal_Part, Node_Kind,
       (Sy (Entry_Index_Specification, Node_Id, Default_Empty),
        Sy (Parameter_Specifications, List_Id, Default_No_List),
        Sy (Condition, Node_Id, Default_Empty)));

   Cc (N_Iteration_Scheme, Node_Kind,
       (Sy (Condition, Node_Id, Default_Empty),
        Sy (Iterator_Specification, Node_Id, Default_Empty),
        Sy (Loop_Parameter_Specification, Node_Id, Default_Empty),
        Sm (Condition_Actions, List_Id)));

   Cc (N_Terminate_Alternative, Node_Kind,
       (Sy (Condition, Node_Id, Default_Empty),
        Sy (Pragmas_Before, List_Id, Default_No_List),
        Sy (Pragmas_After, List_Id, Default_No_List)));

   Ab (N_Formal_Subprogram_Declaration, Node_Kind);

   Cc (N_Formal_Abstract_Subprogram_Declaration, N_Formal_Subprogram_Declaration,
       (Sy (Specification, Node_Id),
        Sy (Default_Name, Node_Id, Default_Empty),
        Sy (Expression, Node_Id, Default_Empty),
        Sy (Box_Present, Flag)));

   Cc (N_Formal_Concrete_Subprogram_Declaration, N_Formal_Subprogram_Declaration,
       (Sy (Specification, Node_Id),
        Sy (Default_Name, Node_Id, Default_Empty),
        Sy (Expression, Node_Id, Default_Empty),
        Sy (Box_Present, Flag)));

   Ab (N_Push_Pop_xxx_Label, Node_Kind);

   Ab (N_Push_xxx_Label, N_Push_Pop_xxx_Label,
       (Sm (Exception_Label, Node_Id)));

   Cc (N_Push_Constraint_Error_Label, N_Push_xxx_Label);

   Cc (N_Push_Program_Error_Label, N_Push_xxx_Label);

   Cc (N_Push_Storage_Error_Label, N_Push_xxx_Label);

   Ab (N_Pop_xxx_Label, N_Push_Pop_xxx_Label);

   Cc (N_Pop_Constraint_Error_Label, N_Pop_xxx_Label);

   Cc (N_Pop_Program_Error_Label, N_Pop_xxx_Label);

   Cc (N_Pop_Storage_Error_Label, N_Pop_xxx_Label);

   Ab (N_SCIL_Node, Node_Kind,
       (Sm (SCIL_Entity, Node_Id)));

   Cc (N_SCIL_Dispatch_Table_Tag_Init, N_SCIL_Node);

   Cc (N_SCIL_Dispatching_Call, N_SCIL_Node,
       (Sm (SCIL_Controlling_Tag, Node_Id),
        Sm (SCIL_Target_Prim, Node_Id)));

   Cc (N_SCIL_Membership_Test, N_SCIL_Node,
       (Sm (SCIL_Tag_Value, Node_Id)));

   Cc (N_Abortable_Part, Node_Kind,
       (Sy (Statements, List_Id, Default_Empty_List)));

   Cc (N_Abstract_Subprogram_Declaration, Node_Kind,
       (Sy (Specification, Node_Id)));

   Cc (N_Access_Definition, Node_Kind,
       (Sy (Null_Exclusion_Present, Flag, Default_False),
        Sy (All_Present, Flag),
        Sy (Constant_Present, Flag),
        Sy (Subtype_Mark, Node_Id, Default_Empty),
        Sy (Access_To_Subprogram_Definition, Node_Id, Default_Empty)));

   Cc (N_Access_To_Object_Definition, Node_Kind,
       (Sy (All_Present, Flag),
        Sy (Null_Exclusion_Present, Flag, Default_False),
        Sy (Null_Excluding_Subtype, Flag),
        Sy (Subtype_Indication, Node_Id, Default_Empty),
        Sy (Constant_Present, Flag)));

   Cc (N_Aspect_Specification, Node_Kind,
       (Sy (Identifier, Node_Id, Default_Empty),
        Sy (Expression, Node_Id, Default_Empty),
        Sy (Class_Present, Flag),
        Sy (Split_PPC, Flag),
        Sm (Aspect_On_Partial_View, Flag),
        Sm (Aspect_Rep_Item, Node_Id),
        Sm (Entity_Or_Associated_Node, Node_Id), -- just Entity
        Sm (Is_Boolean_Aspect, Flag),
        Sm (Is_Checked, Flag),
        Sm (Is_Delayed_Aspect, Flag),
        Sm (Is_Disabled, Flag),
        Sm (Is_Ignored, Flag),
        Sm (Next_Rep_Item, Node_Id)));

   Cc (N_Call_Marker, Node_Kind,
       (Sm (Is_Declaration_Level_Node, Flag),
        Sm (Is_Dispatching_Call, Flag),
        Sm (Is_Elaboration_Checks_OK_Node, Flag),
        Sm (Is_Elaboration_Warnings_OK_Node, Flag),
        Sm (Is_Known_Guaranteed_ABE, Flag),
        Sm (Is_Preelaborable_Call, Flag),
        Sm (Is_Source_Call, Flag),
        Sm (Is_SPARK_Mode_On_Node, Flag),
        Sm (Target, Node_Id)));

   Cc (N_Case_Expression_Alternative, Node_Kind,
       (Sm (Actions, List_Id),
        Sy (Discrete_Choices, List_Id),
        Sy (Expression, Node_Id, Default_Empty),
        Sm (Has_SP_Choice, Flag)));

   Cc (N_Case_Statement_Alternative, Node_Kind,
       (Sy (Discrete_Choices, List_Id),
        Sy (Statements, List_Id, Default_Empty_List),
        Sm (Has_SP_Choice, Flag),
        Sm (Multidefined_Bindings, Flag)));

   Cc (N_Compilation_Unit, Node_Kind,
       (Sy (Context_Items, List_Id),
        Sy (Private_Present, Flag),
        Sy (Unit, Node_Id),
        Sy (Aux_Decls_Node, Node_Id),
        Sm (Acts_As_Spec, Flag),
        Sm (Body_Required, Flag),
        Sm (Context_Pending, Flag),
        Sm (First_Inlined_Subprogram, Node_Id),
        Sm (Has_No_Elaboration_Code, Flag),
        Sm (Has_Pragma_Suppress_All, Flag),
        Sm (Library_Unit, Node_Id),
        Sm (Save_Invocation_Graph_Of_Body, Flag)));

   Cc (N_Compilation_Unit_Aux, Node_Kind,
       (Sy (Declarations, List_Id, Default_No_List),
        Sy (Actions, List_Id, Default_No_List),
        Sy (Pragmas_After, List_Id, Default_No_List),
        Sy (Config_Pragmas, List_Id, Default_Empty_List),
        Sm (Default_Storage_Pool, Node_Id)));

   Cc (N_Component_Association, Node_Kind,
       (Sy (Choices, List_Id),
        Sy (Expression, Node_Id, Default_Empty),
        Sy (Box_Present, Flag),
        Sy (Inherited_Discriminant, Flag),
        Sy (Binding_Chars, Name_Id, Default_No_Name),
        Sm (Loop_Actions, List_Id),
        Sm (Was_Default_Init_Box_Association, Flag)));

   Cc (N_Component_Definition, Node_Kind,
       (Sy (Aliased_Present, Flag),
        Sy (Null_Exclusion_Present, Flag, Default_False),
        Sy (Subtype_Indication, Node_Id, Default_Empty),
        Sy (Access_Definition, Node_Id, Default_Empty)));

   Cc (N_Component_List, Node_Kind,
       (Sy (Component_Items, List_Id),
        Sy (Variant_Part, Node_Id, Default_Empty),
        Sy (Null_Present, Flag)));

   Cc (N_Contract, Node_Kind,
       (Sm (Classifications, Node_Id),
        Sm (Contract_Test_Cases, Node_Id),
        Sm (Is_Expanded_Contract, Flag),
        Sm (Pre_Post_Conditions, Node_Id)));

   Cc (N_Derived_Type_Definition, Node_Kind,
       (Sy (Abstract_Present, Flag),
        Sy (Null_Exclusion_Present, Flag, Default_False),
        Sy (Subtype_Indication, Node_Id, Default_Empty),
        Sy (Record_Extension_Part, Node_Id, Default_Empty),
        Sy (Limited_Present, Flag),
        Sy (Task_Present, Flag),
        Sy (Protected_Present, Flag),
        Sy (Synchronized_Present, Flag),
        Sy (Interface_List, List_Id, Default_No_List),
        Sy (Interface_Present, Flag)));

   Cc (N_Decimal_Fixed_Point_Definition, Node_Kind,
       (Sy (Delta_Expression, Node_Id),
        Sy (Digits_Expression, Node_Id),
        Sy (Real_Range_Specification, Node_Id, Default_Empty)));

   Cc (N_Defining_Program_Unit_Name, Node_Kind,
       (Sy (Name, Node_Id, Default_Empty),
        Sy (Defining_Identifier, Node_Id)));

   Cc (N_Delta_Constraint, Node_Kind,
       (Sy (Delta_Expression, Node_Id),
        Sy (Range_Constraint, Node_Id, Default_Empty)));

   Cc (N_Designator, Node_Kind,
       (Sy (Name, Node_Id, Default_Empty),
        Sy (Identifier, Node_Id, Default_Empty)));

   Cc (N_Digits_Constraint, Node_Kind,
       (Sy (Digits_Expression, Node_Id),
        Sy (Range_Constraint, Node_Id, Default_Empty)));

   Cc (N_Discriminant_Association, Node_Kind,
       (Sy (Selector_Names, List_Id),
        Sy (Expression, Node_Id, Default_Empty)));

   Cc (N_Discriminant_Specification, Node_Kind,
       (Sy (Defining_Identifier, Node_Id),
        Sy (Null_Exclusion_Present, Flag, Default_False),
        Sy (Discriminant_Type, Node_Id),
        Sy (Expression, Node_Id, Default_Empty),
        Sm (More_Ids, Flag),
        Sm (Prev_Ids, Flag)));

   Cc (N_Enumeration_Type_Definition, Node_Kind,
       (Sy (Literals, List_Id),
        Sy (End_Label, Node_Id, Default_Empty)));

   Cc (N_Entry_Body, Node_Kind,
       (Sy (Defining_Identifier, Node_Id),
        Sy (Entry_Body_Formal_Part, Node_Id),
        Sy (Declarations, List_Id, Default_No_List),
        Sy (Handled_Statement_Sequence, Node_Id, Default_Empty),
        Sy (At_End_Proc, Node_Id, Default_Empty),
        Sm (Activation_Chain_Entity, Node_Id)));

   Cc (N_Entry_Call_Alternative, Node_Kind,
       (Sy (Entry_Call_Statement, Node_Id),
        Sy (Statements, List_Id, Default_Empty_List),
        Sy (Pragmas_Before, List_Id, Default_No_List)));

   Cc (N_Entry_Index_Specification, Node_Kind,
       (Sy (Defining_Identifier, Node_Id),
        Sy (Discrete_Subtype_Definition, Node_Id, Default_Empty)));

   Cc (N_Exception_Declaration, N_Declaration,
       (Sy (Defining_Identifier, Node_Id),
        Sm (Expression, Node_Id),
        Sm (More_Ids, Flag),
        Sm (Prev_Ids, Flag),
        Sm (Renaming_Exception, Node_Id)));

   Cc (N_Exception_Handler, Node_Kind,
       (Sy (Choice_Parameter, Node_Id, Default_Empty),
        Sy (Exception_Choices, List_Id),
        Sy (Statements, List_Id, Default_Empty_List),
        Sm (Exception_Label, Node_Id),
        Sm (Has_Local_Raise, Flag),
        Sm (Local_Raise_Not_OK, Flag),
        Sm (Local_Raise_Statements, Elist_Id)));

   Cc (N_Floating_Point_Definition, Node_Kind,
       (Sy (Digits_Expression, Node_Id),
        Sy (Real_Range_Specification, Node_Id, Default_Empty)));

   Cc (N_Formal_Decimal_Fixed_Point_Definition, Node_Kind);

   Cc (N_Formal_Derived_Type_Definition, Node_Kind,
       (Sy (Subtype_Mark, Node_Id, Default_Empty),
        Sy (Private_Present, Flag),
        Sy (Abstract_Present, Flag),
        Sy (Limited_Present, Flag),
        Sy (Synchronized_Present, Flag),
        Sy (Interface_List, List_Id, Default_No_List)));

   Cc (N_Formal_Discrete_Type_Definition, Node_Kind);

   Cc (N_Formal_Floating_Point_Definition, Node_Kind);

   Cc (N_Formal_Modular_Type_Definition, Node_Kind);

   Cc (N_Formal_Ordinary_Fixed_Point_Definition, Node_Kind);

   Cc (N_Formal_Package_Declaration, Node_Kind,
       (Sy (Defining_Identifier, Node_Id),
        Sy (Name, Node_Id, Default_Empty),
        Sy (Generic_Associations, List_Id, Default_No_List),
        Sy (Box_Present, Flag),
        Sm (Instance_Spec, Node_Id),
        Sm (Is_Known_Guaranteed_ABE, Flag)));

   Cc (N_Formal_Private_Type_Definition, Node_Kind,
       (Sy (Abstract_Present, Flag),
        Sy (Tagged_Present, Flag),
        Sy (Limited_Present, Flag),
        Sm (Uninitialized_Variable, Node_Id)));

   Cc (N_Formal_Incomplete_Type_Definition, Node_Kind,
       (Sy (Tagged_Present, Flag)));

   Cc (N_Formal_Signed_Integer_Type_Definition, Node_Kind);

   Cc (N_Freeze_Entity, Node_Kind,
       (Sy (Actions, List_Id, Default_No_List),
        Sm (Access_Types_To_Process, Elist_Id),
        Sm (Entity_Or_Associated_Node, Node_Id), -- just Entity
        Sm (First_Subtype_Link, Node_Id),
        Sm (TSS_Elist, Elist_Id)));

   Cc (N_Freeze_Generic_Entity, Node_Kind,
        Sm (Entity_Or_Associated_Node, Node_Id)); -- just Entity

   Cc (N_Generic_Association, Node_Kind,
       (Sy (Selector_Name, Node_Id, Default_Empty),
        Sy (Explicit_Generic_Actual_Parameter, Node_Id),
        Sy (Box_Present, Flag)));

   Cc (N_Handled_Sequence_Of_Statements, Node_Kind,
       (Sy (Statements, List_Id, Default_Empty_List),
        Sy (End_Label, Node_Id, Default_Empty),
        Sy (Exception_Handlers, List_Id, Default_No_List),
        Sy (At_End_Proc, Node_Id, Default_Empty)));

   Cc (N_Index_Or_Discriminant_Constraint, Node_Kind,
       (Sy (Constraints, List_Id)));

   Cc (N_Iterated_Component_Association, Node_Kind,
       (Sy (Defining_Identifier, Node_Id),
        Sy (Iterator_Specification, Node_Id, Default_Empty),
        Sy (Expression, Node_Id, Default_Empty),
        Sy (Discrete_Choices, List_Id),
        Sy (Box_Present, Flag),
        Sm (Loop_Actions, List_Id)));

   Cc (N_Iterated_Element_Association, Node_Kind,
       (Sy (Key_Expression, Node_Id),
        Sy (Iterator_Specification, Node_Id, Default_Empty),
        Sy (Expression, Node_Id, Default_Empty),
        Sy (Loop_Parameter_Specification, Node_Id, Default_Empty),
        Sy (Box_Present, Flag),
        Sm (Loop_Actions, List_Id)));

   Cc (N_Itype_Reference, Node_Kind,
       (Sm (Itype, Node_Id)));

   Cc (N_Label, Node_Kind,
       (Sy (Identifier, Node_Id, Default_Empty),
        Sm (Exception_Junk, Flag)));

   Cc (N_Modular_Type_Definition, Node_Kind,
       (Sy (Expression, Node_Id, Default_Empty)));

   Cc (N_Number_Declaration, Node_Kind,
       (Sy (Defining_Identifier, Node_Id),
        Sy (Expression, Node_Id, Default_Empty),
        Sm (More_Ids, Flag),
        Sm (Prev_Ids, Flag)));

   Cc (N_Ordinary_Fixed_Point_Definition, Node_Kind,
       (Sy (Delta_Expression, Node_Id),
        Sy (Real_Range_Specification, Node_Id, Default_Empty)));

   Cc (N_Others_Choice, Node_Kind,
       (Sm (All_Others, Flag),
        Sm (Others_Discrete_Choices, List_Id)));

   Cc (N_Package_Specification, Node_Kind,
       (Sy (Defining_Unit_Name, Node_Id),
        Sy (Visible_Declarations, List_Id),
        Sy (Private_Declarations, List_Id, Default_No_List),
        Sy (End_Label, Node_Id, Default_Empty),
        Sm (Generic_Parent, Node_Id),
        Sm (Limited_View_Installed, Flag)));

   Cc (N_Parameter_Association, Node_Kind,
       (Sy (Selector_Name, Node_Id, Default_Empty),
        Sy (Explicit_Actual_Parameter, Node_Id),
        Sm (Is_Accessibility_Actual, Flag),
        Sm (Next_Named_Actual, Node_Id)));

   Cc (N_Parameter_Specification, Node_Kind,
       (Sy (Defining_Identifier, Node_Id),
        Sy (Aliased_Present, Flag),
        Sy (In_Present, Flag),
        Sy (Out_Present, Flag),
        Sy (Null_Exclusion_Present, Flag, Default_False),
        Sy (Parameter_Type, Node_Id),
        Sy (Expression, Node_Id, Default_Empty),
        Sm (Default_Expression, Node_Id),
        Sm (More_Ids, Flag),
        Sm (Prev_Ids, Flag)));

   Cc (N_Pragma, Node_Kind,
       (Sy (Pragma_Argument_Associations, List_Id, Default_No_List),
        Sy (Pragma_Identifier, Node_Id),
        Sy (Class_Present, Flag),
        Sy (Split_PPC, Flag),
        Sm (Corresponding_Aspect, Node_Id),
        Sm (From_Aspect_Specification, Flag),
        Sm (Import_Interface_Present, Flag),
        Sm (Is_Analyzed_Pragma, Flag),
        Sm (Is_Checked, Flag),
        Sm (Is_Checked_Ghost_Pragma, Flag),
        Sm (Is_Delayed_Aspect, Flag),
        Sm (Is_Disabled, Flag),
        Sm (Is_Generic_Contract_Pragma, Flag),
        Sm (Is_Ignored, Flag),
        Sm (Is_Ignored_Ghost_Pragma, Flag),
        Sm (Is_Inherited_Pragma, Flag),
        Sm (Next_Pragma, Node_Id),
        Sm (Next_Rep_Item, Node_Id),
        Sm (Uneval_Old_Accept, Flag),
        Sm (Uneval_Old_Warn, Flag)));

   Cc (N_Protected_Definition, Node_Kind,
       (Sy (Visible_Declarations, List_Id),
        Sy (Private_Declarations, List_Id, Default_No_List),
        Sy (End_Label, Node_Id, Default_Empty)));

   Cc (N_Range_Constraint, Node_Kind,
       (Sy (Range_Expression, Node_Id)));

   Cc (N_Real_Range_Specification, Node_Kind,
       (Sy (Low_Bound, Node_Id),
        Sy (High_Bound, Node_Id)));

   Cc (N_Record_Definition, Node_Kind,
       (Sy (End_Label, Node_Id, Default_Empty),
        Sy (Abstract_Present, Flag),
        Sy (Tagged_Present, Flag),
        Sy (Limited_Present, Flag),
        Sy (Component_List, Node_Id),
        Sy (Null_Present, Flag),
        Sy (Task_Present, Flag),
        Sy (Protected_Present, Flag),
        Sy (Synchronized_Present, Flag),
        Sy (Interface_Present, Flag),
        Sy (Interface_List, List_Id, Default_No_List)));

   Cc (N_Signed_Integer_Type_Definition, Node_Kind,
       (Sy (Low_Bound, Node_Id),
        Sy (High_Bound, Node_Id)));

   Cc (N_Single_Protected_Declaration, Node_Kind,
       (Sy (Defining_Identifier, Node_Id),
        Sy (Interface_List, List_Id, Default_No_List),
        Sy (Protected_Definition, Node_Id)));

   Cc (N_Subunit, Node_Kind,
       (Sy (Name, Node_Id, Default_Empty),
        Sy (Proper_Body, Node_Id),
        Sm (Corresponding_Stub, Node_Id)));

   Cc (N_Task_Definition, Node_Kind,
       (Sy (Visible_Declarations, List_Id),
        Sy (Private_Declarations, List_Id, Default_No_List),
        Sy (End_Label, Node_Id, Default_Empty),
        Sm (Has_Relative_Deadline_Pragma, Flag),
        Sm (Has_Storage_Size_Pragma, Flag)));

   Cc (N_Triggering_Alternative, Node_Kind,
       (Sy (Triggering_Statement, Node_Id),
        Sy (Statements, List_Id, Default_Empty_List),
        Sy (Pragmas_Before, List_Id, Default_No_List)));

   Cc (N_Use_Type_Clause, Node_Kind,
       (Sy (Subtype_Mark, Node_Id, Default_Empty),
        Sy (Is_Effective_Use_Clause, Flag),
        Sy (All_Present, Flag),
        Sm (Hidden_By_Use_Clause, Elist_Id),
        Sm (More_Ids, Flag),
        Sm (Next_Use_Clause, Node_Id),
        Sm (Prev_Ids, Flag),
        Sm (Prev_Use_Clause, Node_Id),
        Sm (Used_Operations, Elist_Id)));

   Cc (N_Validate_Unchecked_Conversion, Node_Kind,
       (Sm (Source_Type, Node_Id),
        Sm (Target_Type, Node_Id)));

   Cc (N_Variable_Reference_Marker, Node_Kind,
       (Sm (Is_Elaboration_Checks_OK_Node, Flag),
        Sm (Is_Elaboration_Warnings_OK_Node, Flag),
        Sm (Is_Read, Flag),
        Sm (Is_SPARK_Mode_On_Node, Flag),
        Sm (Is_Write, Flag),
        Sm (Target, Node_Id)));

   Cc (N_Variant, Node_Kind,
       (Sy (Discrete_Choices, List_Id),
        Sy (Component_List, Node_Id),
        Sm (Dcheck_Function, Node_Id),
        Sm (Enclosing_Variant, Node_Id),
        Sm (Has_SP_Choice, Flag),
        Sm (Present_Expr, Valid_Uint)));

   Cc (N_Variant_Part, Node_Kind,
       (Sy (Name, Node_Id, Default_Empty),
        Sy (Variants, List_Id)));

   Cc (N_With_Clause, Node_Kind,
       (Sy (Name, Node_Id, Default_Empty),
        Sy (Private_Present, Flag),
        Sy (Limited_Present, Flag),
        Sy (First_Name, Flag, Default_True),
        Sy (Last_Name, Flag, Default_True),
        Sm (Context_Installed, Flag),
        Sm (Corresponding_Spec, Node_Id),
        Sm (Elaborate_All_Desirable, Flag),
        Sm (Elaborate_All_Present, Flag),
        Sm (Elaborate_Desirable, Flag),
        Sm (Elaborate_Present, Flag),
        Sm (Implicit_With, Flag),
        Sm (Library_Unit, Node_Id),
        Sm (Limited_View_Installed, Flag),
        Sm (Next_Implicit_With, Node_Id),
        Sm (No_Entities_Ref_In_Spec, Flag),
        Sm (Parent_With, Flag),
        Sm (Unreferenced_In_Spec, Flag)));

   Cc (N_Unused_At_End, Node_Kind);

   --  Union types. These don't fit into the normal parent/child hierarchy
   --  above.

   Union (N_Has_Chars,
          Children =>
            (N_Attribute_Definition_Clause,
             N_Empty,
             N_Pragma_Argument_Association,
             N_Error,
             N_Entity,
             N_Expanded_Name,
             N_Identifier,
             N_Operator_Symbol,
             N_Character_Literal,
             N_Op));

   Union (N_Has_Condition,
          Children =>
            (N_Exit_Statement,
             N_If_Statement,
             N_Accept_Alternative,
             N_Delay_Alternative,
             N_Elsif_Part,
             N_Entry_Body_Formal_Part,
             N_Iteration_Scheme,
             N_Terminate_Alternative));
   --  Nodes with condition fields (does not include N_Raise_xxx_Error)

   Union (N_Has_Bounds,
          Children =>
            (N_Range,
             N_Real_Range_Specification,
             N_Signed_Integer_Type_Definition));
   --  Nodes that have Low_Bound and High_Bound defined

   Union (N_Is_Index,
          Children =>
            (N_Has_Bounds,
             N_Has_Entity,
             N_Subtype_Indication));
   --  Nodes that can be an index of an array

   Union (N_Entity_Name,
          Children =>
            (N_Expanded_Name,
             N_Identifier,
             N_Operator_Symbol));
   --  Nodes that are definitely representing an entity.
   --  Some N_Attribute_Reference nodes may also represent an entity. See
   --  Is_Entity_Name.

   Union (N_Is_Decl,
          Children =>
            (N_Aggregate,
             N_Block_Statement,
             N_Declaration,
             N_Discriminant_Specification,
             N_Entry_Index_Specification,
             N_Enumeration_Type_Definition,
             N_Exception_Handler,
             N_Explicit_Dereference,
             N_Expression_With_Actions,
             N_Extension_Aggregate,
             N_Identifier,
             N_Iterated_Component_Association,
             N_Later_Decl_Item,
             N_Loop_Statement,
             N_Null_Statement,
             N_Number_Declaration,
             N_Package_Specification,
             N_Parameter_Specification,
             N_Renaming_Declaration,
             N_Quantified_Expression));
   --  Nodes that can be returned by Declaration_Node; it can also return
   --  Empty. Not all of these are true "declarations", but Declaration_Node
   --  can return them in some cases.

   Union (N_Is_Range,
          Children =>
            (N_Character_Literal,
             N_Entity_Name,
             N_Has_Bounds,
             N_Integer_Literal,
             N_Subtype_Indication));
   --  Nodes that can be used to specify a range

   Union (N_Is_Case_Choice,
          Children =>
            (N_Is_Range,
             N_Others_Choice));
   --  Nodes that can be in the choices of a case statement

   Union (N_Is_Exception_Choice,
          Children =>
            (N_Entity_Name,
             N_Others_Choice));
   --  Nodes that can be in the choices of an exception handler

   Union (N_Alternative,
          Children =>
            (N_Case_Statement_Alternative,
             N_Variant));
   --  Nodes that can be alternatives in case contructs

end Gen_IL.Gen.Gen_Nodes;
