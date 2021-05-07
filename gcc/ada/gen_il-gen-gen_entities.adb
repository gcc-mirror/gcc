------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               G E N _ I L . G E N . G E N _ E N T I T I E S              --
--                                                                          --
--                                 B o d y                                  --
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

procedure Gen_IL.Gen.Gen_Entities is

   procedure Ab
     (T : Abstract_Entity; Parent : Abstract_Type;
      Fields : Field_Sequence := No_Fields)
      renames Create_Abstract_Entity_Type;
   procedure Cc
     (T : Concrete_Entity; Parent : Abstract_Type;
      Fields : Field_Sequence := No_Fields)
      renames Create_Concrete_Entity_Type;

   function Sm
     (Field : Field_Enum; Field_Type : Type_Enum;
      Type_Only  : Type_Only_Enum := No_Type_Only;
      Pre : String := "") return Field_Desc
      renames Create_Semantic_Field;

   procedure Union (T : Abstract_Entity; Children : Type_Array)
     renames Create_Entity_Union;

begin -- Gen_IL.Gen.Gen_Entities
   pragma Style_Checks ("M200");

   Create_Root_Entity_Type (Entity_Kind,
       (Sm (Ekind, Ekind_Type),
        Sm (Basic_Convention, Convention_Id),
        Sm (Address_Taken, Flag),
        Sm (Associated_Entity, Node_Id),
        Sm (Can_Never_Be_Null, Flag),
        Sm (Checks_May_Be_Suppressed, Flag),
        Sm (Debug_Info_Off, Flag),
        Sm (Default_Expressions_Processed, Flag),
        Sm (Delay_Cleanups, Flag),
        Sm (Delay_Subprogram_Descriptors, Flag),
        Sm (Depends_On_Private, Flag),
        Sm (Disable_Controlled, Flag, Base_Type_Only),
        Sm (Discard_Names, Flag),
        Sm (First_Rep_Item, Node_Id),
        Sm (Freeze_Node, Node_Id),
        Sm (From_Limited_With, Flag),
        Sm (Has_Aliased_Components, Flag, Impl_Base_Type_Only),
        Sm (Has_Alignment_Clause, Flag),
        Sm (Has_All_Calls_Remote, Flag),
        Sm (Has_Atomic_Components, Flag, Impl_Base_Type_Only),
        Sm (Has_Biased_Representation, Flag),
        Sm (Has_Completion, Flag),
        Sm (Has_Contiguous_Rep, Flag),
        Sm (Has_Controlled_Component, Flag, Base_Type_Only),
        Sm (Has_Controlling_Result, Flag),
        Sm (Has_Convention_Pragma, Flag),
        Sm (Has_Default_Aspect, Flag, Base_Type_Only),
        Sm (Has_Delayed_Aspects, Flag),
        Sm (Has_Delayed_Freeze, Flag),
        Sm (Has_Delayed_Rep_Aspects, Flag),
        Sm (Has_Exit, Flag),
        Sm (Has_Forward_Instantiation, Flag),
        Sm (Has_Fully_Qualified_Name, Flag),
        Sm (Has_Gigi_Rep_Item, Flag),
        Sm (Has_Homonym, Flag),
        Sm (Has_Implicit_Dereference, Flag),
        Sm (Has_Independent_Components, Flag, Impl_Base_Type_Only),
        Sm (Has_Master_Entity, Flag),
        Sm (Has_Nested_Block_With_Handler, Flag),
        Sm (Has_Non_Standard_Rep, Flag, Impl_Base_Type_Only),
        Sm (Has_Per_Object_Constraint, Flag),
        Sm (Has_Pragma_Elaborate_Body, Flag),
        Sm (Has_Pragma_Inline, Flag),
        Sm (Has_Pragma_Inline_Always, Flag),
        Sm (Has_Pragma_No_Inline, Flag),
        Sm (Has_Pragma_Preelab_Init, Flag),
        Sm (Has_Pragma_Pure, Flag),
        Sm (Has_Pragma_Pure_Function, Flag),
        Sm (Has_Pragma_Thread_Local_Storage, Flag),
        Sm (Has_Pragma_Unmodified, Flag),
        Sm (Has_Pragma_Unreferenced, Flag),
        Sm (Has_Pragma_Unused, Flag),
        Sm (Has_Private_Ancestor, Flag),
        Sm (Has_Private_Declaration, Flag),
        Sm (Has_Protected, Flag, Base_Type_Only),
        Sm (Has_Qualified_Name, Flag),
        Sm (Has_Size_Clause, Flag),
        Sm (Has_Stream_Size_Clause, Flag),
        Sm (Has_Task, Flag, Base_Type_Only),
        Sm (Has_Timing_Event, Flag, Base_Type_Only),
        Sm (Has_Thunks, Flag),
        Sm (Has_Unchecked_Union, Flag, Base_Type_Only),
        Sm (Has_Volatile_Components, Flag, Impl_Base_Type_Only),
        Sm (Has_Xref_Entry, Flag),
        Sm (Has_Yield_Aspect, Flag),
        Sm (Homonym, Node_Id),
        Sm (In_Package_Body, Flag),
        Sm (In_Private_Part, Flag),
        Sm (In_Use, Flag),
        Sm (Is_Ada_2005_Only, Flag),
        Sm (Is_Ada_2012_Only, Flag),
        Sm (Is_Aliased, Flag),
        Sm (Is_Atomic, Flag),
        Sm (Is_Bit_Packed_Array, Flag, Impl_Base_Type_Only),
        Sm (Is_Character_Type, Flag),
        Sm (Is_Checked_Ghost_Entity, Flag),
        Sm (Is_Child_Unit, Flag),
        Sm (Is_Class_Wide_Clone, Flag),
        Sm (Is_Class_Wide_Equivalent_Type, Flag),
        Sm (Is_Compilation_Unit, Flag),
        Sm (Is_Concurrent_Record_Type, Flag),
        Sm (Is_Constr_Subt_For_U_Nominal, Flag),
        Sm (Is_Constr_Subt_For_UN_Aliased, Flag),
        Sm (Is_Constrained, Flag),
        Sm (Is_Constructor, Flag),
        Sm (Is_Controlled_Active, Flag, Base_Type_Only),
        Sm (Is_CPP_Class, Flag),
        Sm (Is_Descendant_Of_Address, Flag),
        Sm (Is_Discrim_SO_Function, Flag),
        Sm (Is_Discriminant_Check_Function, Flag),
        Sm (Is_Dispatch_Table_Entity, Flag),
        Sm (Is_Dispatching_Operation, Flag),
        Sm (Is_Eliminated, Flag),
        Sm (Is_Entry_Formal, Flag),
        Sm (Is_Entry_Wrapper, Flag),
        Sm (Is_Exported, Flag),
        Sm (Is_First_Subtype, Flag),
        Sm (Is_Formal_Subprogram, Flag),
        Sm (Is_Frozen, Flag),
        Sm (Is_Generic_Instance, Flag),
        Sm (Is_Generic_Type, Flag),
        Sm (Is_Hidden, Flag),
        Sm (Is_Hidden_Non_Overridden_Subpgm, Flag),
        Sm (Is_Hidden_Open_Scope, Flag),
        Sm (Is_Ignored_Ghost_Entity, Flag),
        Sm (Is_Immediately_Visible, Flag),
        Sm (Is_Implementation_Defined, Flag),
        Sm (Is_Imported, Flag),
        Sm (Is_Independent, Flag),
        Sm (Is_Inlined, Flag),
        Sm (Is_Instantiated, Flag),
        Sm (Is_Interface, Flag),
        Sm (Is_Internal, Flag),
        Sm (Is_Interrupt_Handler, Flag),
        Sm (Is_Intrinsic_Subprogram, Flag),
        Sm (Is_Itype, Flag),
        Sm (Is_Known_Non_Null, Flag),
        Sm (Is_Known_Null, Flag),
        Sm (Is_Known_Valid, Flag),
        Sm (Is_Limited_Composite, Flag),
        Sm (Is_Limited_Interface, Flag),
        Sm (Is_Limited_Record, Flag),
        Sm (Is_Loop_Parameter, Flag),
        Sm (Is_Obsolescent, Flag),
        Sm (Is_Package_Body_Entity, Flag),
        Sm (Is_Packed, Flag, Impl_Base_Type_Only),
        Sm (Is_Packed_Array_Impl_Type, Flag),
        Sm (Is_Potentially_Use_Visible, Flag),
        Sm (Is_Preelaborated, Flag),
        Sm (Is_Private_Descendant, Flag),
        Sm (Is_Public, Flag),
        Sm (Is_Pure, Flag),
        Sm (Is_Remote_Call_Interface, Flag),
        Sm (Is_Remote_Types, Flag),
        Sm (Is_Renaming_Of_Object, Flag),
        Sm (Is_Return_Object, Flag),
        Sm (Is_Safe_To_Reevaluate, Flag),
        Sm (Is_Shared_Passive, Flag),
        Sm (Is_Static_Type, Flag),
        Sm (Is_Statically_Allocated, Flag),
        Sm (Is_Tag, Flag),
        Sm (Is_Tagged_Type, Flag),
        Sm (Is_Thunk, Flag),
        Sm (Is_Trivial_Subprogram, Flag),
        Sm (Is_True_Constant, Flag),
        Sm (Is_Unchecked_Union, Flag, Impl_Base_Type_Only),
        Sm (Is_Underlying_Full_View, Flag),
        Sm (Is_Underlying_Record_View, Flag, Base_Type_Only),
        Sm (Is_Unimplemented, Flag),
        Sm (Is_Uplevel_Referenced_Entity, Flag),
        Sm (Is_Visible_Formal, Flag),
        Sm (Is_Visible_Lib_Unit, Flag),
        Sm (Is_Volatile_Type, Flag), -- Should be Base_Type_Only?????
        Sm (Is_Volatile_Object, Flag),
        Sm (Is_Volatile_Full_Access, Flag),
        Sm (Kill_Elaboration_Checks, Flag),
        Sm (Kill_Range_Checks, Flag),
        Sm (Low_Bound_Tested, Flag),
        Sm (Materialize_Entity, Flag),
        Sm (May_Inherit_Delayed_Rep_Aspects, Flag),
        Sm (Needs_Activation_Record, Flag),
        Sm (Needs_Debug_Info, Flag),
        Sm (Never_Set_In_Source, Flag),
        Sm (No_Return, Flag),
        Sm (Overlays_Constant, Flag),
        Sm (Prev_Entity, Node_Id),
        Sm (Reachable, Flag),
        Sm (Referenced, Flag),
        Sm (Referenced_As_LHS, Flag),
        Sm (Referenced_As_Out_Parameter, Flag),
        Sm (Return_Present, Flag),
        Sm (Returns_By_Ref, Flag),
        Sm (Sec_Stack_Needed_For_Return, Flag),
        Sm (Size_Depends_On_Discriminant, Flag),
        Sm (Size_Known_At_Compile_Time, Flag),
        Sm (Stores_Attribute_Old_Prefix, Flag),
        Sm (Strict_Alignment, Flag, Impl_Base_Type_Only),
        Sm (Suppress_Elaboration_Warnings, Flag),
        Sm (Suppress_Style_Checks, Flag),
        Sm (Suppress_Value_Tracking_On_Call, Flag),
        Sm (Treat_As_Volatile, Flag),
        Sm (Used_As_Generic_Actual, Flag),
        Sm (Uses_Sec_Stack, Flag),
        Sm (Warnings_Off, Flag),
        Sm (Warnings_Off_Used, Flag),
        Sm (Warnings_Off_Used_Unmodified, Flag),
        Sm (Warnings_Off_Used_Unreferenced, Flag),
        Sm (Was_Hidden, Flag)));

   Cc (E_Void, Entity_Kind,
       (Sm (Alignment, Uint),
        Sm (Contract, Node_Id),
        Sm (Is_Elaboration_Warnings_OK_Id, Flag),
        Sm (Original_Record_Component, Node_Id),
        Sm (Scope_Depth_Value, Uint),
        Sm (SPARK_Pragma, Node_Id),
        Sm (SPARK_Pragma_Inherited, Flag),
        Sm (Current_Value, Node_Id), -- setter only
        Sm (Has_Predicates, Flag), -- setter only
        Sm (Initialization_Statements, Node_Id), -- setter only
        Sm (Is_Param_Block_Component_Type, Flag, Base_Type_Only), -- setter only
        Sm (Package_Instantiation, Node_Id), -- setter only
        Sm (Related_Expression, Node_Id), -- setter only

        --  ????The following are not documented in the old einfo.ads as being
        --  fields of E_Void.
        Sm (Accept_Address, Elist_Id),
        Sm (Associated_Formal_Package, Node_Id),
        Sm (Associated_Node_For_Itype, Node_Id),
        Sm (Corresponding_Remote_Type, Node_Id),
        Sm (CR_Discriminant, Node_Id),
        Sm (Debug_Renaming_Link, Node_Id),
        Sm (Directly_Designated_Type, Node_Id),
        Sm (Discriminal_Link, Node_Id),
        Sm (Discriminant_Default_Value, Node_Id),
        Sm (Discriminant_Number, Uint),
        Sm (Enclosing_Scope, Node_Id),
        Sm (Entry_Bodies_Array, Node_Id,
            Pre => "Has_Entries (N)"), -- This can't be right????
        Sm (Entry_Cancel_Parameter, Node_Id),
        Sm (Entry_Component, Node_Id),
        Sm (Entry_Formal, Node_Id),
        Sm (Entry_Parameters_Type, Node_Id),
        Sm (Esize, Uint),
        Sm (RM_Size, Uint),
        Sm (Extra_Formal, Node_Id),
        Sm (First_Entity, Node_Id),
        Sm (Float_Rep, Float_Rep_Kind, Base_Type_Only),
        Sm (Generic_Homonym, Node_Id),
        Sm (Generic_Renamings, Elist_Id),
        Sm (Handler_Records, List_Id),
--  ????         Sm (Has_Protected, Flag),
        Sm (Has_Static_Discriminants, Flag),
        Sm (Inner_Instances, Elist_Id),
        Sm (Interface_Name, Node_Id),
        Sm (Last_Entity, Node_Id),
        Sm (Next_Inlined_Subprogram, Node_Id),
        Sm (Renamed_Or_Alias, Node_Id), -- See Einfo.Utils
        Sm (Renaming_Map, Uint),
        Sm (Return_Applies_To, Node_Id),
        Sm (Scalar_Range, Node_Id),
        Sm (Scale_Value, Uint),
        Sm (Unset_Reference, Node_Id)));
   --  In the previous version, the above "setter only" fields were allowed for
   --  E_Void only on the setters, not getters.

   --  ????This comment in the old version of einfo.adb:

   --  Note: in many of these set procedures an "obvious" assertion is missing.
   --  The reason for this is that in many cases, a field is set before the
   --  Ekind field is set, so that the field is set when Ekind = E_Void. It
   --  it is possible to add assertions that specifically include the E_Void
   --  possibility, but in some cases, we just omit the assertions.

   --  causes a lot of headaches. Plus some places used the low-level setters
   --  (e.g. Set_Node1), which bypasses any assertions.

   Ab (Object_Kind, Entity_Kind,
       (Sm (Current_Value, Node_Id),
        Sm (Renamed_Or_Alias, Node_Id)));

   Cc (E_Component, Object_Kind,
       (Sm (Component_Bit_Offset, Uint),
        Sm (Component_Clause, Node_Id),
        Sm (Corresponding_Record_Component, Node_Id),
        Sm (Discriminant_Checking_Func, Node_Id),
        Sm (DT_Entry_Count, Uint,
            Pre => "Is_Tag (N)"),
        Sm (DT_Offset_To_Top_Func, Node_Id,
            Pre => "Is_Tag (N)"),
        Sm (Entry_Formal, Node_Id),
        Sm (Esize, Uint),
        Sm (Interface_Name, Node_Id),
        Sm (Linker_Section_Pragma, Node_Id),
        Sm (Normalized_First_Bit, Uint),
        Sm (Normalized_Position, Uint),
        Sm (Normalized_Position_Max, Uint),
        Sm (Original_Record_Component, Node_Id),
        Sm (Prival, Node_Id,
            Pre => "Is_Protected_Component (N)"),
        Sm (Related_Type, Node_Id)));

   Cc (E_Constant, Object_Kind,
       (Sm (Activation_Record_Component, Node_Id),
        Sm (Actual_Subtype, Node_Id),
        Sm (Alignment, Uint),
        Sm (BIP_Initialization_Call, Node_Id),
        Sm (Contract, Node_Id),
        Sm (Discriminal_Link, Node_Id),
        Sm (Encapsulating_State, Node_Id),
        Sm (Esize, Uint),
        Sm (Extra_Accessibility, Node_Id),
        Sm (Full_View, Node_Id),
        Sm (Initialization_Statements, Node_Id),
        Sm (Interface_Name, Node_Id),
        Sm (Is_Elaboration_Checks_OK_Id, Flag),
        Sm (Is_Elaboration_Warnings_OK_Id, Flag),
        Sm (Is_Finalized_Transient, Flag),
        Sm (Is_Ignored_Transient, Flag),
        Sm (Last_Aggregate_Assignment, Node_Id),
        Sm (Linker_Section_Pragma, Node_Id),
        Sm (Optimize_Alignment_Space, Flag),
        Sm (Optimize_Alignment_Time, Flag),
        Sm (Prival_Link, Node_Id),
        Sm (Related_Expression, Node_Id),
        Sm (Related_Type, Node_Id),
        Sm (Size_Check_Code, Node_Id),
        Sm (SPARK_Pragma, Node_Id),
        Sm (SPARK_Pragma_Inherited, Flag),
        Sm (Status_Flag_Or_Transient_Decl, Node_Id)));

   Cc (E_Discriminant, Object_Kind,
       (Sm (Component_Bit_Offset, Uint),
        Sm (Component_Clause, Node_Id),
        Sm (Corresponding_Discriminant, Node_Id),
        Sm (Corresponding_Record_Component, Node_Id),
        Sm (CR_Discriminant, Node_Id),
        Sm (Discriminal, Node_Id),
        Sm (Discriminant_Default_Value, Node_Id),
        Sm (Discriminant_Number, Uint),
        Sm (Entry_Formal, Node_Id),
        Sm (Esize, Uint),
        Sm (Interface_Name, Node_Id),
        Sm (Is_Completely_Hidden, Flag),
        Sm (Linker_Section_Pragma, Node_Id),
        Sm (Normalized_First_Bit, Uint),
        Sm (Normalized_Position, Uint),
        Sm (Normalized_Position_Max, Uint),
        Sm (Original_Record_Component, Node_Id)));

   Cc (E_Loop_Parameter, Object_Kind,
       (Sm (Activation_Record_Component, Node_Id),
        Sm (Alignment, Uint),
        Sm (Esize, Uint),
        Sm (Is_Finalized_Transient, Flag),
        Sm (Is_Ignored_Transient, Flag),
        Sm (Linker_Section_Pragma, Node_Id),
        Sm (Related_Expression, Node_Id),
        Sm (Status_Flag_Or_Transient_Decl, Node_Id)));

   Cc (E_Variable, Object_Kind,
       (Sm (Activation_Record_Component, Node_Id),
        Sm (Actual_Subtype, Node_Id),
        Sm (Alignment, Uint),
        Sm (Anonymous_Designated_Type, Node_Id),
        Sm (BIP_Initialization_Call, Node_Id),
        Sm (Contract, Node_Id),
        Sm (Debug_Renaming_Link, Node_Id),
        Sm (Discriminal_Link, Node_Id),
        Sm (Encapsulating_State, Node_Id),
        Sm (Esize, Uint),
        Sm (Extra_Accessibility, Node_Id),
        Sm (Extra_Constrained, Node_Id),
        Sm (Has_Initial_Value, Flag),
        Sm (Hiding_Loop_Variable, Node_Id),
        Sm (Initialization_Statements, Node_Id),
        Sm (Interface_Name, Node_Id),
        Sm (Is_Elaboration_Checks_OK_Id, Flag),
        Sm (Is_Elaboration_Warnings_OK_Id, Flag),
        Sm (Is_Finalized_Transient, Flag),
        Sm (Is_Ignored_Transient, Flag),
        Sm (Last_Aggregate_Assignment, Node_Id),
        Sm (Last_Assignment, Node_Id),
        Sm (Linker_Section_Pragma, Node_Id),
        Sm (OK_To_Rename, Flag),
        Sm (Optimize_Alignment_Space, Flag),
        Sm (Optimize_Alignment_Time, Flag),
        Sm (Part_Of_Constituents, Elist_Id),
        Sm (Part_Of_References, Elist_Id),
        Sm (Prival_Link, Node_Id),
        Sm (Related_Expression, Node_Id),
        Sm (Related_Type, Node_Id),
        Sm (Shared_Var_Procs_Instance, Node_Id),
        Sm (Size_Check_Code, Node_Id),
        Sm (SPARK_Pragma, Node_Id),
        Sm (SPARK_Pragma_Inherited, Flag),
        Sm (Status_Flag_Or_Transient_Decl, Node_Id),
        Sm (Suppress_Initialization, Flag),
        Sm (Unset_Reference, Node_Id),
        Sm (Validated_Object, Node_Id)));

   Ab (Formal_Kind, Object_Kind,
       (Sm (Activation_Record_Component, Node_Id),
        Sm (Actual_Subtype, Node_Id),
        Sm (Alignment, Uint),
        Sm (Default_Expr_Function, Node_Id),
        Sm (Default_Value, Node_Id),
        Sm (Entry_Component, Node_Id),
        Sm (Esize, Uint),
        Sm (Extra_Accessibility, Node_Id),
        Sm (Extra_Constrained, Node_Id),
        Sm (Extra_Formal, Node_Id),
        Sm (Has_Initial_Value, Flag),
        Sm (Is_Controlling_Formal, Flag),
        Sm (Is_Only_Out_Parameter, Flag),
        Sm (Linker_Section_Pragma, Node_Id),
        Sm (Mechanism, Mechanism_Type),
        Sm (Minimum_Accessibility, Node_Id),
        Sm (Protected_Formal, Node_Id),
        Sm (Spec_Entity, Node_Id),
        Sm (Unset_Reference, Node_Id)));

   Cc (E_Out_Parameter, Formal_Kind,
       (Sm (Last_Assignment, Node_Id)));

   Cc (E_In_Out_Parameter, Formal_Kind,
       (Sm (Last_Assignment, Node_Id)));

   Cc (E_In_Parameter, Formal_Kind,
       (Sm (Discriminal_Link, Node_Id),
        Sm (Discriminant_Default_Value, Node_Id),
        Sm (Is_Activation_Record, Flag)));

   Ab (Formal_Object_Kind, Object_Kind,
       (Sm (Entry_Component, Node_Id),
        Sm (Esize, Uint)));

   Cc (E_Generic_In_Out_Parameter, Formal_Object_Kind,
       (Sm (Actual_Subtype, Node_Id)));

   Cc (E_Generic_In_Parameter, Formal_Object_Kind);

   Ab (Named_Kind, Entity_Kind,
       (Sm (Renamed_Or_Alias, Node_Id)));

   Cc (E_Named_Integer, Named_Kind);

   Cc (E_Named_Real, Named_Kind);

   Ab (Type_Kind, Entity_Kind,
       (Sm (Alignment, Uint),
        Sm (Associated_Node_For_Itype, Node_Id),
        Sm (Can_Use_Internal_Rep, Flag, Base_Type_Only,
            Pre => "Ekind (Base_Type (N)) in Access_Subprogram_Kind"),
        Sm (Class_Wide_Type, Node_Id),
        Sm (Contract, Node_Id),
        Sm (Current_Use_Clause, Node_Id),
        Sm (Derived_Type_Link, Node_Id),
        Sm (Predicates_Ignored, Flag),
        Sm (Esize, Uint),
        Sm (Finalize_Storage_Only, Flag, Base_Type_Only),
        Sm (Full_View, Node_Id),
        Sm (Has_Completion_In_Body, Flag),
        Sm (Has_Constrained_Partial_View, Flag, Base_Type_Only),
        Sm (Has_Discriminants, Flag),
        Sm (Has_Dispatch_Table, Flag,
            Pre => "Is_Tagged_Type (N)"),
        Sm (Has_Dynamic_Predicate_Aspect, Flag),
        Sm (Has_Inheritable_Invariants, Flag, Base_Type_Only),
        Sm (Has_Inherited_DIC, Flag, Base_Type_Only),
        Sm (Has_Inherited_Invariants, Flag, Base_Type_Only),
        Sm (Has_Object_Size_Clause, Flag),
        Sm (Has_Own_DIC, Flag, Base_Type_Only),
        Sm (Has_Own_Invariants, Flag, Base_Type_Only),
        Sm (Has_Pragma_Unreferenced_Objects, Flag),
        Sm (Has_Predicates, Flag),
        Sm (Has_Primitive_Operations, Flag, Base_Type_Only),
        Sm (Has_Private_Extension, Flag,
            Pre => "Is_Tagged_Type (N)"),
        Sm (Has_Specified_Layout, Flag, Impl_Base_Type_Only),
        Sm (Has_Specified_Stream_Input, Flag),
        Sm (Has_Specified_Stream_Output, Flag),
        Sm (Has_Specified_Stream_Read, Flag),
        Sm (Has_Specified_Stream_Write, Flag),
        Sm (Has_Static_Discriminants, Flag),
        Sm (Has_Static_Predicate, Flag),
        Sm (Has_Static_Predicate_Aspect, Flag),
        Sm (Has_Unknown_Discriminants, Flag),
        Sm (Interface_Name, Node_Id),
        Sm (Is_Abstract_Type, Flag),
        Sm (Is_Actual_Subtype, Flag),
        Sm (Is_Asynchronous, Flag),
        Sm (Is_Generic_Actual_Type, Flag),
        Sm (Is_Non_Static_Subtype, Flag),
        Sm (Is_Private_Composite, Flag),
        Sm (Is_RACW_Stub_Type, Flag),
        Sm (Is_Unsigned_Type, Flag),
        Sm (Itype_Printed, Flag,
            Pre => "Is_Itype (N)"),
        Sm (Known_To_Have_Preelab_Init, Flag),
        Sm (Linker_Section_Pragma, Node_Id),
        Sm (Must_Be_On_Byte_Boundary, Flag),
        Sm (Must_Have_Preelab_Init, Flag),
        Sm (No_Tagged_Streams_Pragma, Node_Id,
            Pre => "Is_Tagged_Type (N)"),
        Sm (Non_Binary_Modulus, Flag, Base_Type_Only),
        Sm (Optimize_Alignment_Space, Flag),
        Sm (Optimize_Alignment_Time, Flag),
        Sm (Partial_View_Has_Unknown_Discr, Flag),
        Sm (Pending_Access_Types, Elist_Id),
        Sm (Related_Expression, Node_Id),
        Sm (RM_Size, Uint),
        Sm (SPARK_Pragma, Node_Id),
        Sm (SPARK_Pragma_Inherited, Flag),
        Sm (Subprograms_For_Type, Elist_Id),
        Sm (Suppress_Initialization, Flag),
        Sm (Universal_Aliasing, Flag, Impl_Base_Type_Only),
        Sm (Renamed_Or_Alias, Node_Id)));

   Ab (Elementary_Kind, Type_Kind);

   Ab (Scalar_Kind, Elementary_Kind,
       (Sm (Default_Aspect_Value, Node_Id, Base_Type_Only),
        Sm (Scalar_Range, Node_Id)));

   Ab (Discrete_Kind, Scalar_Kind,
       (Sm (No_Dynamic_Predicate_On_Actual, Flag),
        Sm (No_Predicate_On_Actual, Flag),
        Sm (Static_Discrete_Predicate, List_Id)));

   Ab (Enumeration_Kind, Discrete_Kind,
       (Sm (First_Literal, Node_Id),
        Sm (Has_Enumeration_Rep_Clause, Flag),
        Sm (Has_Pragma_Ordered, Flag, Impl_Base_Type_Only),
        Sm (Lit_Indexes, Node_Id),
        Sm (Lit_Strings, Node_Id),
        Sm (Nonzero_Is_True, Flag, Base_Type_Only,
            Pre => "Root_Type (N) = Standard_Boolean"),
        Sm (Lit_Hash, Node_Id, Root_Type_Only)));

   Cc (E_Enumeration_Type, Enumeration_Kind,
       (Sm (Enum_Pos_To_Rep, Node_Id),
        Sm (First_Entity, Node_Id)));

   Cc (E_Enumeration_Subtype, Enumeration_Kind);

   Ab (Integer_Kind, Discrete_Kind,
       (Sm (Has_Shift_Operator, Flag, Base_Type_Only)));

   Ab (Signed_Integer_Kind, Integer_Kind,
       (Sm (First_Entity, Node_Id)));

   Cc (E_Signed_Integer_Type, Signed_Integer_Kind,
       (Sm (Direct_Primitive_Operations, Elist_Id,
            Pre => "Is_Tagged_Type (N)")));

   Cc (E_Signed_Integer_Subtype, Signed_Integer_Kind);

   Ab (Modular_Integer_Kind, Integer_Kind,
       (Sm (Modulus, Uint, Base_Type_Only),
        Sm (Original_Array_Type, Node_Id)));

   Cc (E_Modular_Integer_Type, Modular_Integer_Kind);

   Cc (E_Modular_Integer_Subtype, Modular_Integer_Kind);

   Ab (Real_Kind, Scalar_Kind,
       (Sm (Static_Real_Or_String_Predicate, Node_Id)));

   Ab (Fixed_Point_Kind, Real_Kind,
       (Sm (Delta_Value, Ureal),
        Sm (Small_Value, Ureal)));

   Ab (Ordinary_Fixed_Point_Kind, Fixed_Point_Kind,
       (Sm (Has_Small_Clause, Flag)));

   Cc (E_Ordinary_Fixed_Point_Type, Ordinary_Fixed_Point_Kind);

   Cc (E_Ordinary_Fixed_Point_Subtype, Ordinary_Fixed_Point_Kind);

   Ab (Decimal_Fixed_Point_Kind, Fixed_Point_Kind,
       (Sm (Digits_Value, Uint),
        Sm (Has_Machine_Radix_Clause, Flag),
        Sm (Machine_Radix_10, Flag),
        Sm (Scale_Value, Uint)));

   Cc (E_Decimal_Fixed_Point_Type, Decimal_Fixed_Point_Kind);

   Cc (E_Decimal_Fixed_Point_Subtype, Decimal_Fixed_Point_Kind);

   Ab (Float_Kind, Real_Kind,
       (Sm (Digits_Value, Uint),
        Sm (Float_Rep, Float_Rep_Kind, Base_Type_Only)));

   Cc (E_Floating_Point_Type, Float_Kind);

   Cc (E_Floating_Point_Subtype, Float_Kind);

   Ab (Access_Kind, Elementary_Kind,
       (Sm (Associated_Storage_Pool, Node_Id, Root_Type_Only),
        Sm (Directly_Designated_Type, Node_Id),
        Sm (Finalization_Master, Node_Id, Root_Type_Only),
        Sm (Has_Pragma_Controlled, Flag, Impl_Base_Type_Only),
        Sm (Has_Storage_Size_Clause, Flag, Impl_Base_Type_Only),
        Sm (Is_Access_Constant, Flag),
        Sm (Is_Local_Anonymous_Access, Flag),
        Sm (Is_Param_Block_Component_Type, Flag, Base_Type_Only),
        Sm (Is_Pure_Unit_Access_Type, Flag),
        Sm (Master_Id, Node_Id),
        Sm (No_Pool_Assigned, Flag, Root_Type_Only),
        Sm (No_Strict_Aliasing, Flag, Base_Type_Only),
        Sm (Storage_Size_Variable, Node_Id, Impl_Base_Type_Only)));

   Cc (E_Access_Type, Access_Kind,
       (Sm (Direct_Primitive_Operations, Elist_Id,
            Pre => "Is_Tagged_Type (N)")));

   Cc (E_Access_Subtype, Access_Kind);

   Cc (E_Access_Attribute_Type, Access_Kind);

   Cc (E_Allocator_Type, Access_Kind);

   Cc (E_General_Access_Type, Access_Kind,
       (Sm (First_Entity, Node_Id)));

   Ab (Access_Subprogram_Kind, Access_Kind);

   Cc (E_Access_Subprogram_Type, Access_Subprogram_Kind,
       (Sm (Equivalent_Type, Node_Id),
        Sm (Original_Access_Type, Node_Id)));

   Ab (Access_Protected_Kind, Access_Subprogram_Kind,
       (Sm (Equivalent_Type, Node_Id)));

   Cc (E_Access_Protected_Subprogram_Type, Access_Protected_Kind);

   Cc (E_Anonymous_Access_Protected_Subprogram_Type, Access_Protected_Kind);

   Cc (E_Anonymous_Access_Subprogram_Type, Access_Subprogram_Kind);

   Cc (E_Anonymous_Access_Type, Access_Kind);

   Ab (Composite_Kind, Type_Kind,
--  ????This fails for the same reason as DT_Position of E_Function;
--  see comment there.
--       (Sm (Discriminant_Constraint, Elist_Id,
--            Pre => "Has_Discriminants (N) or else Is_Constrained (N)")));
       (Sm (Discriminant_Constraint, Elist_Id)));

   Ab (Aggregate_Kind, Composite_Kind,
       (Sm (Component_Alignment, Component_Alignment_Kind, Base_Type_Only),
        Sm (Has_Pragma_Pack, Flag, Impl_Base_Type_Only),
        Sm (Reverse_Storage_Order, Flag, Base_Type_Only),
        Sm (SSO_Set_High_By_Default, Flag, Base_Type_Only),
        Sm (SSO_Set_Low_By_Default, Flag, Base_Type_Only)));

   Ab (Array_Kind, Aggregate_Kind,
       (Sm (Component_Size, Uint, Impl_Base_Type_Only),
        Sm (Component_Type, Node_Id, Impl_Base_Type_Only),
        Sm (Default_Aspect_Component_Value, Node_Id, Base_Type_Only),
        Sm (First_Index, Node_Id),
        Sm (Has_Component_Size_Clause, Flag, Impl_Base_Type_Only),
        Sm (Original_Array_Type, Node_Id),
        Sm (Packed_Array_Impl_Type, Node_Id),
        Sm (Related_Array_Object, Node_Id)));

   Cc (E_Array_Type, Array_Kind,
       (Sm (First_Entity, Node_Id),
        Sm (Static_Real_Or_String_Predicate, Node_Id)));

   Cc (E_Array_Subtype, Array_Kind,
       (Sm (Predicated_Parent, Node_Id),
        Sm (Direct_Primitive_Operations, Elist_Id,
            Pre => "Is_Tagged_Type (N)"),
        Sm (First_Entity, Node_Id),
        Sm (Static_Real_Or_String_Predicate, Node_Id)));

   Cc (E_String_Literal_Subtype, Array_Kind,
       (Sm (String_Literal_Length, Uint),
        Sm (String_Literal_Low_Bound, Node_Id)));

   Ab (Class_Wide_Kind, Aggregate_Kind,
       (Sm (C_Pass_By_Copy, Flag, Impl_Base_Type_Only),
        Sm (Direct_Primitive_Operations, Elist_Id,
            Pre => "Is_Tagged_Type (N)"),
        Sm (Equivalent_Type, Node_Id),
        Sm (First_Entity, Node_Id),
        Sm (Has_Complex_Representation, Flag, Impl_Base_Type_Only),
        Sm (Has_Record_Rep_Clause, Flag, Impl_Base_Type_Only),
        Sm (Interfaces, Elist_Id),
        Sm (Last_Entity, Node_Id),
        Sm (No_Reordering, Flag, Impl_Base_Type_Only),
        Sm (Non_Limited_View, Node_Id),
        Sm (Parent_Subtype, Node_Id, Base_Type_Only),
        Sm (Reverse_Bit_Order, Flag, Base_Type_Only),
        Sm (Stored_Constraint, Elist_Id)));

   Cc (E_Class_Wide_Type, Class_Wide_Kind,
       (Sm (Corresponding_Remote_Type, Node_Id),
        Sm (Scalar_Range, Node_Id)));

   Cc (E_Class_Wide_Subtype, Class_Wide_Kind,
       (Sm (Cloned_Subtype, Node_Id)));

   Cc (E_Record_Type, Aggregate_Kind,
       (Sm (Access_Disp_Table, Elist_Id, Impl_Base_Type_Only),
        Sm (Access_Disp_Table_Elab_Flag, Node_Id, Impl_Base_Type_Only),
        Sm (C_Pass_By_Copy, Flag, Impl_Base_Type_Only),
        Sm (Corresponding_Concurrent_Type, Node_Id),
        Sm (Corresponding_Remote_Type, Node_Id),
        Sm (Direct_Primitive_Operations, Elist_Id,
            Pre => "Is_Tagged_Type (N)"),
        Sm (Dispatch_Table_Wrappers, Elist_Id, Impl_Base_Type_Only),
        Sm (First_Entity, Node_Id),
        Sm (Has_Complex_Representation, Flag, Impl_Base_Type_Only),
        Sm (Has_Record_Rep_Clause, Flag, Impl_Base_Type_Only),
        Sm (Interfaces, Elist_Id),
        Sm (Last_Entity, Node_Id),
        Sm (No_Reordering, Flag, Impl_Base_Type_Only),
        Sm (Parent_Subtype, Node_Id, Base_Type_Only),
        Sm (Reverse_Bit_Order, Flag, Base_Type_Only),
        Sm (Stored_Constraint, Elist_Id),
        Sm (Underlying_Record_View, Node_Id)));

   Cc (E_Record_Subtype, Aggregate_Kind,
       (Sm (Access_Disp_Table, Elist_Id, Impl_Base_Type_Only),
        Sm (Access_Disp_Table_Elab_Flag, Node_Id, Impl_Base_Type_Only),
        Sm (C_Pass_By_Copy, Flag, Impl_Base_Type_Only),
        Sm (Cloned_Subtype, Node_Id),
        Sm (Corresponding_Remote_Type, Node_Id),
        Sm (Predicated_Parent, Node_Id),
        Sm (Direct_Primitive_Operations, Elist_Id,
            Pre => "Is_Tagged_Type (N)"),
        Sm (Dispatch_Table_Wrappers, Elist_Id, Impl_Base_Type_Only),
        Sm (First_Entity, Node_Id),
        Sm (Has_Complex_Representation, Flag, Impl_Base_Type_Only),
        Sm (Has_Record_Rep_Clause, Flag, Impl_Base_Type_Only),
        Sm (Interfaces, Elist_Id),
        Sm (Last_Entity, Node_Id),
        Sm (No_Reordering, Flag, Impl_Base_Type_Only),
        Sm (Parent_Subtype, Node_Id, Base_Type_Only),
        Sm (Reverse_Bit_Order, Flag, Base_Type_Only),
        Sm (Stored_Constraint, Elist_Id),
        Sm (Underlying_Record_View, Node_Id)));

   Ab (Incomplete_Or_Private_Kind, Composite_Kind,
       (Sm (First_Entity, Node_Id),
        Sm (Last_Entity, Node_Id),
        Sm (Private_Dependents, Elist_Id),
        Sm (Stored_Constraint, Elist_Id)));

   Ab (Private_Kind, Incomplete_Or_Private_Kind,
       (Sm (Underlying_Full_View, Node_Id)));

   Cc (E_Record_Type_With_Private, Private_Kind,
       (Sm (Access_Disp_Table, Elist_Id, Impl_Base_Type_Only),
        Sm (Access_Disp_Table_Elab_Flag, Node_Id, Impl_Base_Type_Only),
        Sm (C_Pass_By_Copy, Flag, Impl_Base_Type_Only),
        Sm (Component_Alignment, Component_Alignment_Kind, Base_Type_Only),
        Sm (Corresponding_Remote_Type, Node_Id),
        Sm (Direct_Primitive_Operations, Elist_Id,
            Pre => "Is_Tagged_Type (N)"),
        Sm (Has_Complex_Representation, Flag, Impl_Base_Type_Only),
        Sm (Has_Pragma_Pack, Flag, Impl_Base_Type_Only),
        Sm (Has_Record_Rep_Clause, Flag, Impl_Base_Type_Only),
        Sm (Interfaces, Elist_Id),
        Sm (No_Reordering, Flag, Impl_Base_Type_Only),
        Sm (Parent_Subtype, Node_Id, Base_Type_Only),
        Sm (Reverse_Bit_Order, Flag, Base_Type_Only),
        Sm (Reverse_Storage_Order, Flag, Base_Type_Only),
        Sm (SSO_Set_High_By_Default, Flag, Base_Type_Only),
        Sm (SSO_Set_Low_By_Default, Flag, Base_Type_Only),
        Sm (Underlying_Record_View, Node_Id)));

   Cc (E_Record_Subtype_With_Private, Private_Kind,
       (Sm (C_Pass_By_Copy, Flag, Impl_Base_Type_Only),
        Sm (Component_Alignment, Component_Alignment_Kind, Base_Type_Only),
        Sm (Corresponding_Remote_Type, Node_Id),
        Sm (Predicated_Parent, Node_Id),
        Sm (Direct_Primitive_Operations, Elist_Id,
            Pre => "Is_Tagged_Type (N)"),
        Sm (Has_Complex_Representation, Flag, Impl_Base_Type_Only),
        Sm (Has_Pragma_Pack, Flag, Impl_Base_Type_Only),
        Sm (Has_Record_Rep_Clause, Flag, Impl_Base_Type_Only),
        Sm (Interfaces, Elist_Id),
        Sm (No_Reordering, Flag, Impl_Base_Type_Only),
        Sm (Parent_Subtype, Node_Id, Base_Type_Only),
        Sm (Reverse_Bit_Order, Flag, Base_Type_Only),
        Sm (Reverse_Storage_Order, Flag, Base_Type_Only),
        Sm (SSO_Set_High_By_Default, Flag, Base_Type_Only),
        Sm (SSO_Set_Low_By_Default, Flag, Base_Type_Only)));

   Cc (E_Private_Type, Private_Kind,
       (Sm (Direct_Primitive_Operations, Elist_Id,
            Pre => "Is_Tagged_Type (N)"),
        Sm (Scalar_Range, Node_Id),
        Sm (Scope_Depth_Value, Uint),
        Sm (Directly_Designated_Type, Node_Id)));
   --  ????Directly_Designated_Type was allowed to be Set_, but not get.
   --  Same for E_Limited_Private_Type. And incomplete.

   Cc (E_Private_Subtype, Private_Kind,
       (Sm (Direct_Primitive_Operations, Elist_Id,
            Pre => "Is_Tagged_Type (N)"),
        Sm (Scope_Depth_Value, Uint)));

   Cc (E_Limited_Private_Type, Private_Kind,
       (Sm (Scalar_Range, Node_Id),
        Sm (Scope_Depth_Value, Uint),
        Sm (Directly_Designated_Type, Node_Id)));

   Cc (E_Limited_Private_Subtype, Private_Kind,
       (Sm (Scope_Depth_Value, Uint)));

   Ab (Incomplete_Kind, Incomplete_Or_Private_Kind,
       (Sm (Direct_Primitive_Operations, Elist_Id,
            Pre => "Is_Tagged_Type (N)"),
        Sm (Non_Limited_View, Node_Id),
        Sm (Directly_Designated_Type, Node_Id)));

   Cc (E_Incomplete_Type, Incomplete_Kind,
       (Sm (Scalar_Range, Node_Id)));

   Cc (E_Incomplete_Subtype, Incomplete_Kind);

   Ab (Concurrent_Kind, Composite_Kind,
       (Sm (Corresponding_Record_Type, Node_Id),
        Sm (Direct_Primitive_Operations, Elist_Id,
            Pre => "Is_Tagged_Type (N)"),
        Sm (First_Entity, Node_Id),
        Sm (First_Private_Entity, Node_Id),
        Sm (Last_Entity, Node_Id),
        Sm (Scope_Depth_Value, Uint),
        Sm (Stored_Constraint, Elist_Id)));

   Ab (Task_Kind, Concurrent_Kind,
       (Sm (Has_Storage_Size_Clause, Flag, Impl_Base_Type_Only),
        Sm (Is_Elaboration_Checks_OK_Id, Flag),
        Sm (Is_Elaboration_Warnings_OK_Id, Flag),
        Sm (Relative_Deadline_Variable, Node_Id, Impl_Base_Type_Only),
        Sm (Storage_Size_Variable, Node_Id, Impl_Base_Type_Only),
        Sm (Task_Body_Procedure, Node_Id)));

   Cc (E_Task_Type, Task_Kind,
       (Sm (Anonymous_Object, Node_Id),
        Sm (Ignore_SPARK_Mode_Pragmas, Flag),
        Sm (SPARK_Aux_Pragma, Node_Id),
        Sm (SPARK_Aux_Pragma_Inherited, Flag)));

   Cc (E_Task_Subtype, Task_Kind);

   Ab (Protected_Kind, Concurrent_Kind,
       (Sm (Entry_Bodies_Array, Node_Id,
            Pre => "Has_Entries (N)"),
        Sm (Uses_Lock_Free, Flag)));

   Cc (E_Protected_Type, Protected_Kind,
       (Sm (Anonymous_Object, Node_Id),
        Sm (Entry_Max_Queue_Lengths_Array, Node_Id),
        Sm (Ignore_SPARK_Mode_Pragmas, Flag),
        Sm (SPARK_Aux_Pragma, Node_Id),
        Sm (SPARK_Aux_Pragma_Inherited, Flag)));

   Cc (E_Protected_Subtype, Protected_Kind);

   Cc (E_Exception_Type, Type_Kind,
       (Sm (Equivalent_Type, Node_Id)));

   Cc (E_Subprogram_Type, Type_Kind,
       (Sm (Access_Subprogram_Wrapper, Node_Id),
        Sm (Extra_Accessibility_Of_Result, Node_Id),
        Sm (Extra_Formals, Node_Id),
        Sm (First_Entity, Node_Id),
        Sm (Last_Entity, Node_Id),
        Sm (Needs_No_Actuals, Flag)));

   Ab (Overloadable_Kind, Entity_Kind,
       (Sm (Renamed_Or_Alias, Node_Id),
        Sm (Extra_Formals, Node_Id),
        Sm (Is_Abstract_Subprogram, Flag),
        Sm (Is_Primitive, Flag),
        Sm (Needs_No_Actuals, Flag),
        Sm (Requires_Overriding, Flag)));

   Cc (E_Enumeration_Literal, Overloadable_Kind,
       (Sm (Enumeration_Pos, Uint),
        Sm (Enumeration_Rep, Uint),
        Sm (Enumeration_Rep_Expr, Node_Id),
        Sm (Esize, Uint),
        Sm (Alignment, Uint),
        Sm (Interface_Name, Node_Id)));

   Ab (Subprogram_Kind, Overloadable_Kind,
       (Sm (Body_Needed_For_SAL, Flag),
        Sm (Class_Wide_Clone, Node_Id),
        Sm (Contract, Node_Id),
        Sm (Elaboration_Entity, Node_Id),
        Sm (Elaboration_Entity_Required, Flag),
        Sm (First_Entity, Node_Id),
        Sm (Has_Expanded_Contract, Flag),
        Sm (Has_Nested_Subprogram, Flag),
        Sm (Has_Out_Or_In_Out_Parameter, Flag),
        Sm (Has_Recursive_Call, Flag),
        Sm (Ignore_SPARK_Mode_Pragmas, Flag),
        Sm (Import_Pragma, Node_Id),
        Sm (Interface_Alias, Node_Id),
        Sm (Interface_Name, Node_Id),
        Sm (Is_Elaboration_Checks_OK_Id, Flag),
        Sm (Is_Elaboration_Warnings_OK_Id, Flag),
        Sm (Is_Machine_Code_Subprogram, Flag),
        Sm (Last_Entity, Node_Id),
        Sm (Linker_Section_Pragma, Node_Id),
        Sm (Overridden_Operation, Node_Id),
        Sm (Protected_Body_Subprogram, Node_Id),
        Sm (Scope_Depth_Value, Uint),
        Sm (SPARK_Pragma, Node_Id),
        Sm (SPARK_Pragma_Inherited, Flag),
        Sm (Subps_Index, Uint)));

   Cc (E_Function, Subprogram_Kind,
       (Sm (Anonymous_Masters, Elist_Id),
        Sm (Corresponding_Equality, Node_Id,
            Pre => "not Comes_From_Source (N) and then Chars (N) = Name_Op_Ne"),
        Sm (Corresponding_Procedure, Node_Id),
--  ????In the old version, we had the following assertion in the getter, but
--  not the setter, and in fact we sometimes violate it in the setter, for
--  example, sem_disp.adb:1635 says "Set_DT_Position_Value (Subp, No_Uint);".
--        Sm (DT_Position, Uint,
--            Pre => "Present (DTC_Entity (N))"),
--  Perhaps we should have "getter-only preconditions".
        Sm (DT_Position, Uint),
        Sm (DTC_Entity, Node_Id),
        Sm (Extra_Accessibility_Of_Result, Node_Id),
        Sm (Generic_Renamings, Elist_Id),
        Sm (Handler_Records, List_Id),
        Sm (Has_Missing_Return, Flag),
        Sm (Inner_Instances, Elist_Id),
        Sm (Is_Called, Flag),
        Sm (Is_CUDA_Kernel, Flag),
        Sm (Is_DIC_Procedure, Flag),
        Sm (Is_Generic_Actual_Subprogram, Flag),
        Sm (Is_Initial_Condition_Procedure, Flag),
        Sm (Is_Inlined_Always, Flag),
        Sm (Is_Invariant_Procedure, Flag),
        Sm (Is_Partial_Invariant_Procedure, Flag),
        Sm (Is_Predicate_Function, Flag),
        Sm (Is_Predicate_Function_M, Flag),
        Sm (Is_Primitive_Wrapper, Flag),
        Sm (Is_Private_Primitive, Flag),
        Sm (Mechanism, Mechanism_Type),
        Sm (Next_Inlined_Subprogram, Node_Id),
        Sm (Original_Protected_Subprogram, Node_Id),
        Sm (Postconditions_Proc, Node_Id),
        Sm (Protected_Subprogram, Node_Id),
        Sm (Protection_Object, Node_Id),
        Sm (Related_Expression, Node_Id),
        Sm (Renaming_Map, Uint),
        Sm (Rewritten_For_C, Flag),
        Sm (Thunk_Entity, Node_Id,
            Pre => "Is_Thunk (N)"),
        Sm (Wrapped_Entity, Node_Id,
            Pre => "Is_Primitive_Wrapper (N)")));

   Cc (E_Operator, Subprogram_Kind,
       (Sm (Extra_Accessibility_Of_Result, Node_Id)));

   Cc (E_Procedure, Subprogram_Kind,
       (Sm (Anonymous_Masters, Elist_Id),
        Sm (Associated_Node_For_Itype, Node_Id),
        Sm (Corresponding_Function, Node_Id),
--  ????See comment in E_Function.
--        Sm (DT_Position, Uint,
--            Pre => "Present (DTC_Entity (N))"),
        Sm (DT_Position, Uint),
        Sm (DTC_Entity, Node_Id),
        Sm (Entry_Parameters_Type, Node_Id),
        Sm (Generic_Renamings, Elist_Id),
        Sm (Handler_Records, List_Id),
        Sm (Inner_Instances, Elist_Id),
        Sm (Is_Asynchronous, Flag),
        Sm (Is_Called, Flag),
        Sm (Is_CUDA_Kernel, Flag),
        Sm (Is_DIC_Procedure, Flag),
        Sm (Is_Generic_Actual_Subprogram, Flag),
        Sm (Is_Initial_Condition_Procedure, Flag),
        Sm (Is_Inlined_Always, Flag),
        Sm (Is_Invariant_Procedure, Flag),
        Sm (Is_Null_Init_Proc, Flag),
        Sm (Is_Partial_Invariant_Procedure, Flag),
        Sm (Is_Predicate_Function, Flag),
        Sm (Is_Predicate_Function_M, Flag),
        Sm (Is_Primitive_Wrapper, Flag),
        Sm (Is_Private_Primitive, Flag),
        Sm (Is_Valued_Procedure, Flag),
        Sm (Next_Inlined_Subprogram, Node_Id),
        Sm (Original_Protected_Subprogram, Node_Id),
        Sm (Postconditions_Proc, Node_Id),
        Sm (Protected_Subprogram, Node_Id),
        Sm (Protection_Object, Node_Id),
        Sm (Receiving_Entry, Node_Id),
        Sm (Renaming_Map, Uint),
        Sm (Static_Initialization, Node_Id,
            Pre => "not Is_Dispatching_Operation (N)"),
        Sm (Thunk_Entity, Node_Id,
            Pre => "Is_Thunk (N)"),
        Sm (Wrapped_Entity, Node_Id,
            Pre => "Is_Primitive_Wrapper (N)")));

   Cc (E_Abstract_State, Overloadable_Kind,
       (Sm (Body_References, Elist_Id),
        Sm (Encapsulating_State, Node_Id),
        Sm (First_Entity, Node_Id),
        Sm (Has_Partial_Visible_Refinement, Flag),
        Sm (Has_Visible_Refinement, Flag),
        Sm (Non_Limited_View, Node_Id),
        Sm (Part_Of_Constituents, Elist_Id),
        Sm (Refinement_Constituents, Elist_Id),
        Sm (SPARK_Pragma, Node_Id),
        Sm (SPARK_Pragma_Inherited, Flag)));

   Cc (E_Entry, Overloadable_Kind,
       (Sm (Accept_Address, Elist_Id),
        Sm (Barrier_Function, Node_Id),
        Sm (Contract, Node_Id),
        Sm (Contract_Wrapper, Node_Id),
        Sm (Elaboration_Entity, Node_Id),
        Sm (Elaboration_Entity_Required, Flag),
        Sm (Entry_Accepted, Flag),
        Sm (Entry_Parameters_Type, Node_Id),
        Sm (First_Entity, Node_Id),
        Sm (Has_Out_Or_In_Out_Parameter, Flag),
        Sm (Ignore_SPARK_Mode_Pragmas, Flag),
        Sm (Is_Elaboration_Checks_OK_Id, Flag),
        Sm (Is_Elaboration_Warnings_OK_Id, Flag),
        Sm (Last_Entity, Node_Id),
        Sm (Postconditions_Proc, Node_Id),
        Sm (Protected_Body_Subprogram, Node_Id),
        Sm (Protection_Object, Node_Id),
        Sm (Scope_Depth_Value, Uint),
        Sm (SPARK_Pragma, Node_Id),
        Sm (SPARK_Pragma_Inherited, Flag)));

   Cc (E_Entry_Family, Entity_Kind,
       (Sm (Accept_Address, Elist_Id),
        Sm (Barrier_Function, Node_Id),
        Sm (Contract, Node_Id),
        Sm (Contract_Wrapper, Node_Id),
        Sm (Elaboration_Entity, Node_Id),
        Sm (Elaboration_Entity_Required, Flag),
        Sm (Entry_Accepted, Flag),
        Sm (Entry_Parameters_Type, Node_Id),
        Sm (Extra_Formals, Node_Id),
        Sm (First_Entity, Node_Id),
        Sm (Has_Out_Or_In_Out_Parameter, Flag),
        Sm (Ignore_SPARK_Mode_Pragmas, Flag),
        Sm (Is_Elaboration_Checks_OK_Id, Flag),
        Sm (Is_Elaboration_Warnings_OK_Id, Flag),
        Sm (Last_Entity, Node_Id),
        Sm (Needs_No_Actuals, Flag),
        Sm (Postconditions_Proc, Node_Id),
        Sm (Protected_Body_Subprogram, Node_Id),
        Sm (Protection_Object, Node_Id),
        Sm (Renamed_Or_Alias, Node_Id),
        Sm (Scope_Depth_Value, Uint),
        Sm (SPARK_Pragma, Node_Id),
        Sm (SPARK_Pragma_Inherited, Flag)));

   Cc (E_Block, Entity_Kind,
       (Sm (Block_Node, Node_Id),
        Sm (Entry_Cancel_Parameter, Node_Id),
        Sm (First_Entity, Node_Id),
        Sm (Is_Exception_Handler, Flag),
        Sm (Last_Entity, Node_Id),
        Sm (Renamed_Or_Alias, Node_Id),
        Sm (Return_Applies_To, Node_Id),
        Sm (Scope_Depth_Value, Uint)));

   Cc (E_Entry_Index_Parameter, Entity_Kind,
       (Sm (Entry_Index_Constant, Node_Id)));

   Cc (E_Exception, Entity_Kind,
       (Sm (Alignment, Uint),
        Sm (Esize, Uint),
        Sm (Interface_Name, Node_Id),
        Sm (Is_Raised, Flag),
        Sm (Register_Exception_Call, Node_Id),
        Sm (Renamed_Or_Alias, Node_Id)));

   Ab (Generic_Unit_Kind, Entity_Kind,
       (Sm (Body_Needed_For_SAL, Flag),
        Sm (Contract, Node_Id),
        Sm (Elaboration_Entity, Node_Id),
        Sm (Elaboration_Entity_Required, Flag),
        Sm (First_Entity, Node_Id),
        Sm (Ignore_SPARK_Mode_Pragmas, Flag),
        Sm (Inner_Instances, Elist_Id),
        Sm (Interface_Name, Node_Id),
        Sm (Is_Elaboration_Checks_OK_Id, Flag),
        Sm (Is_Elaboration_Warnings_OK_Id, Flag),
        Sm (Last_Entity, Node_Id),
        Sm (Renamed_Or_Alias, Node_Id),
        Sm (Renaming_Map, Uint),
        Sm (Scope_Depth_Value, Uint),
        Sm (SPARK_Pragma, Node_Id),
        Sm (SPARK_Pragma_Inherited, Flag)));

   Ab (Generic_Subprogram_Kind, Generic_Unit_Kind,
       (Sm (Has_Out_Or_In_Out_Parameter, Flag),
        Sm (Is_Primitive, Flag),
        Sm (Next_Inlined_Subprogram, Node_Id),
        Sm (Overridden_Operation, Node_Id)));

   Cc (E_Generic_Function, Generic_Subprogram_Kind,
       (Sm (Has_Missing_Return, Flag)));

   Cc (E_Generic_Procedure, Generic_Subprogram_Kind);

   Cc (E_Generic_Package, Generic_Unit_Kind,
       (Sm (Abstract_States, Elist_Id),
        Sm (Body_Entity, Node_Id),
        Sm (First_Private_Entity, Node_Id),
        Sm (Generic_Homonym, Node_Id),
        Sm (Package_Instantiation, Node_Id),
        Sm (SPARK_Aux_Pragma, Node_Id),
        Sm (SPARK_Aux_Pragma_Inherited, Flag)));

   Cc (E_Label, Entity_Kind,
       (Sm (Enclosing_Scope, Node_Id),
        Sm (Renamed_Or_Alias, Node_Id)));

   Cc (E_Loop, Entity_Kind,
       (Sm (First_Entity, Node_Id),
        Sm (First_Exit_Statement, Node_Id),
        Sm (Has_Loop_Entry_Attributes, Flag),
        Sm (Last_Entity, Node_Id),
        Sm (Renamed_Or_Alias, Node_Id),
        Sm (Scope_Depth_Value, Uint)));

   Cc (E_Return_Statement, Entity_Kind,
       (Sm (First_Entity, Node_Id),
        Sm (Last_Entity, Node_Id),
        Sm (Return_Applies_To, Node_Id),
        Sm (Scope_Depth_Value, Uint)));

   Cc (E_Package, Entity_Kind,
       (Sm (Abstract_States, Elist_Id),
        Sm (Anonymous_Masters, Elist_Id),
        Sm (Associated_Formal_Package, Node_Id),
        Sm (Body_Entity, Node_Id),
        Sm (Body_Needed_For_Inlining, Flag),
        Sm (Body_Needed_For_SAL, Flag),
        Sm (Contract, Node_Id),
        Sm (Current_Use_Clause, Node_Id),
        Sm (Dependent_Instances, Elist_Id,
            Pre => "Is_Generic_Instance (N)"),
        Sm (Elaborate_Body_Desirable, Flag),
        Sm (Elaboration_Entity, Node_Id),
        Sm (Elaboration_Entity_Required, Flag),
        Sm (Finalizer, Node_Id),
        Sm (First_Entity, Node_Id),
        Sm (First_Private_Entity, Node_Id),
        Sm (Generic_Renamings, Elist_Id),
        Sm (Handler_Records, List_Id),
        Sm (Has_RACW, Flag),
        Sm (Hidden_In_Formal_Instance, Elist_Id),
        Sm (Ignore_SPARK_Mode_Pragmas, Flag),
        Sm (Incomplete_Actuals, Elist_Id),
        Sm (Inner_Instances, Elist_Id),
        Sm (Interface_Name, Node_Id),
        Sm (Is_Called, Flag),
        Sm (Is_Elaboration_Checks_OK_Id, Flag),
        Sm (Is_Elaboration_Warnings_OK_Id, Flag),
        Sm (Last_Entity, Node_Id),
        Sm (Limited_View, Node_Id),
        Sm (Package_Instantiation, Node_Id),
        Sm (Related_Instance, Node_Id),
        Sm (Renamed_In_Spec, Flag),
        Sm (Renamed_Or_Alias, Node_Id),
        Sm (Renaming_Map, Uint),
        Sm (Scope_Depth_Value, Uint),
        Sm (SPARK_Aux_Pragma, Node_Id),
        Sm (SPARK_Aux_Pragma_Inherited, Flag),
        Sm (SPARK_Pragma, Node_Id),
        Sm (SPARK_Pragma_Inherited, Flag),
        Sm (Static_Elaboration_Desired, Flag)));

   Cc (E_Package_Body, Entity_Kind,
       (Sm (Contract, Node_Id),
        Sm (Finalizer, Node_Id),
        Sm (First_Entity, Node_Id),
        Sm (Handler_Records, List_Id),
        Sm (Ignore_SPARK_Mode_Pragmas, Flag),
        Sm (Last_Entity, Node_Id),
        Sm (Related_Instance, Node_Id),
        Sm (Renamed_Or_Alias, Node_Id),
        Sm (Scope_Depth_Value, Uint),
        Sm (SPARK_Aux_Pragma, Node_Id),
        Sm (SPARK_Aux_Pragma_Inherited, Flag),
        Sm (SPARK_Pragma, Node_Id),
        Sm (SPARK_Pragma_Inherited, Flag),
        Sm (Spec_Entity, Node_Id)));

   Ab (Concurrent_Body_Kind, Entity_Kind,
       (Sm (Ignore_SPARK_Mode_Pragmas, Flag),
        Sm (SPARK_Pragma, Node_Id),
        Sm (SPARK_Pragma_Inherited, Flag)));

   Cc (E_Protected_Body, Concurrent_Body_Kind);

   Cc (E_Task_Body, Concurrent_Body_Kind,
       (Sm (Contract, Node_Id),
        Sm (First_Entity, Node_Id)));

   Cc (E_Subprogram_Body, Entity_Kind,
       (Sm (Anonymous_Masters, Elist_Id),
        Sm (Contract, Node_Id),
        Sm (Extra_Formals, Node_Id),
        Sm (First_Entity, Node_Id),
        Sm (Ignore_SPARK_Mode_Pragmas, Flag),
        Sm (Interface_Name, Node_Id),
        Sm (Last_Entity, Node_Id),
        Sm (Renamed_Or_Alias, Node_Id),
        Sm (Scope_Depth_Value, Uint),
        Sm (SPARK_Pragma, Node_Id),
        Sm (SPARK_Pragma_Inherited, Flag)));

   --  Union types. These don't fit into the normal parent/child hierarchy
   --  above.

   Union (Anonymous_Access_Kind,
          Children =>
            (E_Anonymous_Access_Protected_Subprogram_Type,
             E_Anonymous_Access_Subprogram_Type,
             E_Anonymous_Access_Type));

   Union (Assignable_Kind,
          Children =>
            (E_Variable,
             E_Out_Parameter,
             E_In_Out_Parameter));

   Union (Digits_Kind,
          Children =>
            (Decimal_Fixed_Point_Kind,
             Float_Kind));

   Union (Discrete_Or_Fixed_Point_Kind,
          Children =>
            (Discrete_Kind,
             Fixed_Point_Kind));

   Union (Entry_Kind,
          Children =>
            (E_Entry,
             E_Entry_Family));

   Union (Numeric_Kind,
          Children =>
            (Integer_Kind,
             Fixed_Point_Kind,
             Float_Kind));

   Union (Record_Kind,
          Children =>
            (E_Class_Wide_Type,
             E_Class_Wide_Subtype,
             E_Record_Type,
             E_Record_Subtype,
             E_Record_Type_With_Private,
             E_Record_Subtype_With_Private));

end Gen_IL.Gen.Gen_Entities;
