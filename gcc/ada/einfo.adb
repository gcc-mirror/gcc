------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                E I N F O                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2016, Free Software Foundation, Inc.         --
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

pragma Style_Checks (All_Checks);
--  Turn off subprogram ordering, not used for this unit

with Atree;   use Atree;
with Elists;  use Elists;
with Namet;   use Namet;
with Nlists;  use Nlists;
with Output;  use Output;
with Sinfo;   use Sinfo;
with Stand;   use Stand;

package body Einfo is

   use Atree.Unchecked_Access;
   --  This is one of the packages that is allowed direct untyped access to
   --  the fields in a node, since it provides the next level abstraction
   --  which incorporates appropriate checks.

   ----------------------------------------------
   -- Usage of Fields in Defining Entity Nodes --
   ----------------------------------------------

   --  Four of these fields are defined in Sinfo, since they in are the base
   --  part of the node. The access routines for these four fields and the
   --  corresponding set procedures are defined in Sinfo. These fields are
   --  present in all entities. Note that Homonym is also in the base part of
   --  the node, but has access routines that are more properly part of Einfo,
   --  which is why they are defined here.

   --    Chars                           Name1
   --    Next_Entity                     Node2
   --    Scope                           Node3
   --    Etype                           Node5

   --  Remaining fields are present only in extended nodes (i.e. entities).

   --  The following fields are present in all entities

   --    Homonym                         Node4
   --    First_Rep_Item                  Node6
   --    Freeze_Node                     Node7
   --    Associated_Entity               Node37

   --  The usage of other fields (and the entity kinds to which it applies)
   --  depends on the particular field (see Einfo spec for details).

   --    Associated_Node_For_Itype       Node8
   --    Dependent_Instances             Elist8
   --    Hiding_Loop_Variable            Node8
   --    Mechanism                       Uint8 (but returns Mechanism_Type)
   --    Normalized_First_Bit            Uint8
   --    Refinement_Constituents         Elist8
   --    Return_Applies_To               Node8
   --    First_Exit_Statement            Node8

   --    Class_Wide_Type                 Node9
   --    Current_Value                   Node9
   --    Renaming_Map                    Uint9

   --    Direct_Primitive_Operations     Elist10
   --    Discriminal_Link                Node10
   --    Float_Rep                       Uint10 (but returns Float_Rep_Kind)
   --    Handler_Records                 List10
   --    Normalized_Position_Max         Uint10
   --    Part_Of_Constituents            Elist10

   --    Block_Node                      Node11
   --    Component_Bit_Offset            Uint11
   --    Full_View                       Node11
   --    Entry_Component                 Node11
   --    Enumeration_Pos                 Uint11
   --    Generic_Homonym                 Node11
   --    Part_Of_References              Elist11
   --    Protected_Body_Subprogram       Node11

   --    Barrier_Function                Node12
   --    Enumeration_Rep                 Uint12
   --    Esize                           Uint12
   --    Next_Inlined_Subprogram         Node12

   --    Component_Clause                Node13
   --    Elaboration_Entity              Node13
   --    Extra_Accessibility             Node13
   --    RM_Size                         Uint13

   --    Alignment                       Uint14
   --    Normalized_Position             Uint14
   --    Postconditions_Proc             Node14
   --    Shadow_Entities                 List14

   --    Discriminant_Number             Uint15
   --    DT_Position                     Uint15
   --    DT_Entry_Count                  Uint15
   --    Entry_Parameters_Type           Node15
   --    Extra_Formal                    Node15
   --    Pending_Access_Types            Elist15
   --    Related_Instance                Node15
   --    Status_Flag_Or_Transient_Decl   Node15

   --    Access_Disp_Table               Elist16
   --    Body_References                 Elist16
   --    Cloned_Subtype                  Node16
   --    DTC_Entity                      Node16
   --    Entry_Formal                    Node16
   --    First_Private_Entity            Node16
   --    Lit_Strings                     Node16
   --    Scale_Value                     Uint16
   --    String_Literal_Length           Uint16
   --    Unset_Reference                 Node16

   --    Actual_Subtype                  Node17
   --    Digits_Value                    Uint17
   --    Discriminal                     Node17
   --    First_Entity                    Node17
   --    First_Index                     Node17
   --    First_Literal                   Node17
   --    Master_Id                       Node17
   --    Modulus                         Uint17
   --    Prival                          Node17

   --    Alias                           Node18
   --    Corresponding_Concurrent_Type   Node18
   --    Corresponding_Protected_Entry   Node18
   --    Corresponding_Record_Type       Node18
   --    Delta_Value                     Ureal18
   --    Enclosing_Scope                 Node18
   --    Equivalent_Type                 Node18
   --    Lit_Indexes                     Node18
   --    Private_Dependents              Elist18
   --    Renamed_Entity                  Node18
   --    Renamed_Object                  Node18
   --    String_Literal_Low_Bound        Node18

   --    Body_Entity                     Node19
   --    Corresponding_Discriminant      Node19
   --    Default_Aspect_Component_Value  Node19
   --    Default_Aspect_Value            Node19
   --    Entry_Bodies_Array              Node19
   --    Extra_Accessibility_Of_Result   Node19
   --    Non_Limited_View                Node19
   --    Parent_Subtype                  Node19
   --    Size_Check_Code                 Node19
   --    Spec_Entity                     Node19
   --    Underlying_Full_View            Node19

   --    Component_Type                  Node20
   --    Default_Value                   Node20
   --    Directly_Designated_Type        Node20
   --    Discriminant_Checking_Func      Node20
   --    Discriminant_Default_Value      Node20
   --    Last_Entity                     Node20
   --    Prival_Link                     Node20
   --    Register_Exception_Call         Node20
   --    Scalar_Range                    Node20

   --    Accept_Address                  Elist21
   --    Default_Expr_Function           Node21
   --    Discriminant_Constraint         Elist21
   --    Interface_Name                  Node21
   --    Original_Array_Type             Node21
   --    Small_Value                     Ureal21

   --    Associated_Storage_Pool         Node22
   --    Component_Size                  Uint22
   --    Corresponding_Remote_Type       Node22
   --    Enumeration_Rep_Expr            Node22
   --    Original_Record_Component       Node22
   --    Private_View                    Node22
   --    Protected_Formal                Node22
   --    Scope_Depth_Value               Uint22
   --    Shared_Var_Procs_Instance       Node22

   --    CR_Discriminant                 Node23
   --    Entry_Cancel_Parameter          Node23
   --    Enum_Pos_To_Rep                 Node23
   --    Extra_Constrained               Node23
   --    Finalization_Master             Node23
   --    Generic_Renamings               Elist23
   --    Inner_Instances                 Elist23
   --    Limited_View                    Node23
   --    Packed_Array_Impl_Type          Node23
   --    Protection_Object               Node23
   --    Stored_Constraint               Elist23

   --    Incomplete_Actuals              Elist24
   --    Related_Expression              Node24
   --    Subps_Index                     Uint24

   --    Contract_Wrapper                Node25
   --    Debug_Renaming_Link             Node25
   --    DT_Offset_To_Top_Func           Node25
   --    Interface_Alias                 Node25
   --    Interfaces                      Elist25
   --    Related_Array_Object            Node25
   --    Static_Discrete_Predicate       List25
   --    Static_Real_Or_String_Predicate Node25
   --    Task_Body_Procedure             Node25

   --    Dispatch_Table_Wrappers         Elist26
   --    Last_Assignment                 Node26
   --    Overridden_Operation            Node26
   --    Package_Instantiation           Node26
   --    Storage_Size_Variable           Node26

   --    Current_Use_Clause              Node27
   --    Related_Type                    Node27
   --    Wrapped_Entity                  Node27

   --    Extra_Formals                   Node28
   --    Finalizer                       Node28
   --    Initialization_Statements       Node28
   --    Original_Access_Type            Node28
   --    Relative_Deadline_Variable      Node28
   --    Underlying_Record_View          Node28

   --    BIP_Initialization_Call         Node29
   --    Subprograms_For_Type            Node29

   --    Anonymous_Object                Node30
   --    Corresponding_Equality          Node30
   --    Last_Aggregate_Assignment       Node30
   --    Static_Initialization           Node30

   --    Derived_Type_Link               Node31
   --    Thunk_Entity                    Node31
   --    Activation_Record_Component     Node31

   --    Encapsulating_State             Node32
   --    No_Tagged_Streams_Pragma        Node32

   --    Linker_Section_Pragma           Node33

   --    Contract                        Node34

   --    Import_Pragma                   Node35

   --    Anonymous_Master                Node36

   --    Class_Wide_Preconds             List38

   --    Class_Wide_Postconds            List39

   --    SPARK_Pragma                    Node40

   --    SPARK_Aux_Pragma                Node41

   ---------------------------------------------
   -- Usage of Flags in Defining Entity Nodes --
   ---------------------------------------------

   --  All flags are unique, there is no overlaying, so each flag is physically
   --  present in every entity. However, for many of the flags, it only makes
   --  sense for them to be set true for certain subsets of entity kinds. See
   --  the spec of Einfo for further details.

   --    Is_Inlined_Always               Flag1
   --    Is_Hidden_Non_Overridden_Subpgm Flag2
   --    Has_Default_Init_Cond           Flag3
   --    Is_Frozen                       Flag4
   --    Has_Discriminants               Flag5
   --    Is_Dispatching_Operation        Flag6
   --    Is_Immediately_Visible          Flag7
   --    In_Use                          Flag8
   --    Is_Potentially_Use_Visible      Flag9
   --    Is_Public                       Flag10

   --    Is_Inlined                      Flag11
   --    Is_Constrained                  Flag12
   --    Is_Generic_Type                 Flag13
   --    Depends_On_Private              Flag14
   --    Is_Aliased                      Flag15
   --    Is_Volatile                     Flag16
   --    Is_Internal                     Flag17
   --    Has_Delayed_Freeze              Flag18
   --    Is_Abstract_Subprogram          Flag19
   --    Is_Concurrent_Record_Type       Flag20

   --    Has_Master_Entity               Flag21
   --    Needs_No_Actuals                Flag22
   --    Has_Storage_Size_Clause         Flag23
   --    Is_Imported                     Flag24
   --    Is_Limited_Record               Flag25
   --    Has_Completion                  Flag26
   --    Has_Pragma_Controlled           Flag27
   --    Is_Statically_Allocated         Flag28
   --    Has_Size_Clause                 Flag29
   --    Has_Task                        Flag30

   --    Checks_May_Be_Suppressed        Flag31
   --    Kill_Elaboration_Checks         Flag32
   --    Kill_Range_Checks               Flag33
   --    Has_Independent_Components      Flag34
   --    Is_Class_Wide_Equivalent_Type   Flag35
   --    Referenced_As_LHS               Flag36
   --    Is_Known_Non_Null               Flag37
   --    Can_Never_Be_Null               Flag38
   --    Has_Default_Aspect              Flag39
   --    Body_Needed_For_SAL             Flag40

   --    Treat_As_Volatile               Flag41
   --    Is_Controlled                   Flag42
   --    Has_Controlled_Component        Flag43
   --    Is_Pure                         Flag44
   --    In_Private_Part                 Flag45
   --    Has_Alignment_Clause            Flag46
   --    Has_Exit                        Flag47
   --    In_Package_Body                 Flag48
   --    Reachable                       Flag49
   --    Delay_Subprogram_Descriptors    Flag50

   --    Is_Packed                       Flag51
   --    Is_Entry_Formal                 Flag52
   --    Is_Private_Descendant           Flag53
   --    Return_Present                  Flag54
   --    Is_Tagged_Type                  Flag55
   --    Has_Homonym                     Flag56
   --    Is_Hidden                       Flag57
   --    Non_Binary_Modulus              Flag58
   --    Is_Preelaborated                Flag59
   --    Is_Shared_Passive               Flag60

   --    Is_Remote_Types                 Flag61
   --    Is_Remote_Call_Interface        Flag62
   --    Is_Character_Type               Flag63
   --    Is_Intrinsic_Subprogram         Flag64
   --    Has_Record_Rep_Clause           Flag65
   --    Has_Enumeration_Rep_Clause      Flag66
   --    Has_Small_Clause                Flag67
   --    Has_Component_Size_Clause       Flag68
   --    Is_Access_Constant              Flag69
   --    Is_First_Subtype                Flag70

   --    Has_Completion_In_Body          Flag71
   --    Has_Unknown_Discriminants       Flag72
   --    Is_Child_Unit                   Flag73
   --    Is_CPP_Class                    Flag74
   --    Has_Non_Standard_Rep            Flag75
   --    Is_Constructor                  Flag76
   --    Static_Elaboration_Desired      Flag77
   --    Is_Tag                          Flag78
   --    Has_All_Calls_Remote            Flag79
   --    Is_Constr_Subt_For_U_Nominal    Flag80

   --    Is_Asynchronous                 Flag81
   --    Has_Gigi_Rep_Item               Flag82
   --    Has_Machine_Radix_Clause        Flag83
   --    Machine_Radix_10                Flag84
   --    Is_Atomic                       Flag85
   --    Has_Atomic_Components           Flag86
   --    Has_Volatile_Components         Flag87
   --    Discard_Names                   Flag88
   --    Is_Interrupt_Handler            Flag89
   --    Returns_By_Ref                  Flag90

   --    Is_Itype                        Flag91
   --    Size_Known_At_Compile_Time      Flag92
   --    Reverse_Storage_Order           Flag93
   --    Is_Generic_Actual_Type          Flag94
   --    Uses_Sec_Stack                  Flag95
   --    Warnings_Off                    Flag96
   --    Is_Controlling_Formal           Flag97
   --    Has_Controlling_Result          Flag98
   --    Is_Exported                     Flag99
   --    Has_Specified_Layout            Flag100

   --    Has_Nested_Block_With_Handler   Flag101
   --    Is_Called                       Flag102
   --    Is_Completely_Hidden            Flag103
   --    Address_Taken                   Flag104
   --    Suppress_Initialization         Flag105
   --    Is_Limited_Composite            Flag106
   --    Is_Private_Composite            Flag107
   --    Default_Expressions_Processed   Flag108
   --    Is_Non_Static_Subtype           Flag109
   --    Has_Out_Or_In_Out_Parameter     Flag110

   --    Is_Formal_Subprogram            Flag111
   --    Is_Renaming_Of_Object           Flag112
   --    No_Return                       Flag113
   --    Delay_Cleanups                  Flag114
   --    Never_Set_In_Source             Flag115
   --    Is_Visible_Lib_Unit             Flag116
   --    Is_Unchecked_Union              Flag117
   --    Is_For_Access_Subtype           Flag118
   --    Has_Convention_Pragma           Flag119
   --    Has_Primitive_Operations        Flag120

   --    Has_Pragma_Pack                 Flag121
   --    Is_Bit_Packed_Array             Flag122
   --    Has_Unchecked_Union             Flag123
   --    Is_Eliminated                   Flag124
   --    C_Pass_By_Copy                  Flag125
   --    Is_Instantiated                 Flag126
   --    Is_Valued_Procedure             Flag127
   --    (used for Component_Alignment)  Flag128
   --    (used for Component_Alignment)  Flag129
   --    Is_Generic_Instance             Flag130

   --    No_Pool_Assigned                Flag131
   --    Is_Default_Init_Cond_Procedure  Flag132
   --    Has_Inherited_Default_Init_Cond Flag133
   --    Returns_Limited_View            Flag134
   --    Has_Aliased_Components          Flag135
   --    No_Strict_Aliasing              Flag136
   --    Is_Machine_Code_Subprogram      Flag137
   --    Is_Packed_Array_Impl_Type       Flag138
   --    Has_Biased_Representation       Flag139
   --    Has_Complex_Representation      Flag140

   --    Is_Constr_Subt_For_UN_Aliased   Flag141
   --    Has_Missing_Return              Flag142
   --    Has_Recursive_Call              Flag143
   --    Is_Unsigned_Type                Flag144
   --    Strict_Alignment                Flag145
   --    Is_Abstract_Type                Flag146
   --    Needs_Debug_Info                Flag147
   --    Suppress_Elaboration_Warnings   Flag148
   --    Is_Compilation_Unit             Flag149
   --    Has_Pragma_Elaborate_Body       Flag150

   --    Has_Private_Ancestor            Flag151
   --    Entry_Accepted                  Flag152
   --    Is_Obsolescent                  Flag153
   --    Has_Per_Object_Constraint       Flag154
   --    Has_Private_Declaration         Flag155
   --    Referenced                      Flag156
   --    Has_Pragma_Inline               Flag157
   --    Finalize_Storage_Only           Flag158
   --    From_Limited_With               Flag159
   --    Is_Package_Body_Entity          Flag160

   --    Has_Qualified_Name              Flag161
   --    Nonzero_Is_True                 Flag162
   --    Is_True_Constant                Flag163
   --    Reverse_Bit_Order               Flag164
   --    Suppress_Style_Checks           Flag165
   --    Debug_Info_Off                  Flag166
   --    Sec_Stack_Needed_For_Return     Flag167
   --    Materialize_Entity              Flag168
   --    Has_Pragma_Thread_Local_Storage Flag169
   --    Is_Known_Valid                  Flag170

   --    Is_Hidden_Open_Scope            Flag171
   --    Has_Object_Size_Clause          Flag172
   --    Has_Fully_Qualified_Name        Flag173
   --    Elaboration_Entity_Required     Flag174
   --    Has_Forward_Instantiation       Flag175
   --    Is_Discrim_SO_Function          Flag176
   --    Size_Depends_On_Discriminant    Flag177
   --    Is_Null_Init_Proc               Flag178
   --    Has_Pragma_Pure_Function        Flag179
   --    Has_Pragma_Unreferenced         Flag180

   --    Has_Contiguous_Rep              Flag181
   --    Has_Xref_Entry                  Flag182
   --    Must_Be_On_Byte_Boundary        Flag183
   --    Has_Stream_Size_Clause          Flag184
   --    Is_Ada_2005_Only                Flag185
   --    Is_Interface                    Flag186
   --    Has_Constrained_Partial_View    Flag187
   --    Uses_Lock_Free                  Flag188
   --    Is_Pure_Unit_Access_Type        Flag189
   --    Has_Specified_Stream_Input      Flag190

   --    Has_Specified_Stream_Output     Flag191
   --    Has_Specified_Stream_Read       Flag192
   --    Has_Specified_Stream_Write      Flag193
   --    Is_Local_Anonymous_Access       Flag194
   --    Is_Primitive_Wrapper            Flag195
   --    Was_Hidden                      Flag196
   --    Is_Limited_Interface            Flag197
   --    Has_Pragma_Ordered              Flag198
   --    Is_Ada_2012_Only                Flag199

   --    Has_Delayed_Aspects             Flag200
   --    Has_Pragma_No_Inline            Flag201
   --    Itype_Printed                   Flag202
   --    Has_Pragma_Pure                 Flag203
   --    Is_Known_Null                   Flag204
   --    Low_Bound_Tested                Flag205
   --    Is_Visible_Formal               Flag206
   --    Known_To_Have_Preelab_Init      Flag207
   --    Must_Have_Preelab_Init          Flag208
   --    Is_Return_Object                Flag209
   --    Elaborate_Body_Desirable        Flag210

   --    Has_Static_Discriminants        Flag211
   --    Has_Pragma_Unreferenced_Objects Flag212
   --    Requires_Overriding             Flag213
   --    Has_RACW                        Flag214
   --    Is_Param_Block_Component_Type   Flag215
   --    Universal_Aliasing              Flag216
   --    Suppress_Value_Tracking_On_Call Flag217
   --    Is_Primitive                    Flag218
   --    Has_Initial_Value               Flag219
   --    Has_Dispatch_Table              Flag220

   --    Has_Pragma_Preelab_Init         Flag221
   --    Used_As_Generic_Actual          Flag222
   --    Is_Descendant_Of_Address        Flag223
   --    Is_Raised                       Flag224
   --    Is_Thunk                        Flag225
   --    Is_Only_Out_Parameter           Flag226
   --    Referenced_As_Out_Parameter     Flag227
   --    Has_Thunks                      Flag228
   --    Can_Use_Internal_Rep            Flag229
   --    Has_Pragma_Inline_Always        Flag230

   --    Renamed_In_Spec                 Flag231
   --    Has_Invariants                  Flag232
   --    Has_Pragma_Unmodified           Flag233
   --    Is_Dispatch_Table_Entity        Flag234
   --    Is_Trivial_Subprogram           Flag235
   --    Warnings_Off_Used               Flag236
   --    Warnings_Off_Used_Unmodified    Flag237
   --    Warnings_Off_Used_Unreferenced  Flag238
   --    OK_To_Reorder_Components        Flag239
   --    Has_Expanded_Contract           Flag240

   --    Optimize_Alignment_Space        Flag241
   --    Optimize_Alignment_Time         Flag242
   --    Overlays_Constant               Flag243
   --    Is_RACW_Stub_Type               Flag244
   --    Is_Private_Primitive            Flag245
   --    Is_Underlying_Record_View       Flag246
   --    OK_To_Rename                    Flag247
   --    Has_Inheritable_Invariants      Flag248
   --    Is_Safe_To_Reevaluate           Flag249
   --    Has_Predicates                  Flag250

   --    Has_Implicit_Dereference        Flag251
   --    Is_Processed_Transient          Flag252
   --    Disable_Controlled              Flag253
   --    Is_Implementation_Defined       Flag254
   --    Is_Predicate_Function           Flag255
   --    Is_Predicate_Function_M         Flag256
   --    Is_Invariant_Procedure          Flag257
   --    Has_Dynamic_Predicate_Aspect    Flag258
   --    Has_Static_Predicate_Aspect     Flag259
   --    Has_Loop_Entry_Attributes       Flag260

   --    Has_Delayed_Rep_Aspects         Flag261
   --    May_Inherit_Delayed_Rep_Aspects Flag262
   --    Has_Visible_Refinement          Flag263
   --    Is_Discriminant_Check_Function  Flag264
   --    SPARK_Pragma_Inherited          Flag265
   --    SPARK_Aux_Pragma_Inherited      Flag266
   --    Has_Shift_Operator              Flag267
   --    Is_Independent                  Flag268
   --    Has_Static_Predicate            Flag269
   --    Stores_Attribute_Old_Prefix     Flag270

   --    Has_Protected                   Flag271
   --    SSO_Set_Low_By_Default          Flag272
   --    SSO_Set_High_By_Default         Flag273
   --    Is_Generic_Actual_Subprogram    Flag274
   --    No_Predicate_On_Actual          Flag275
   --    No_Dynamic_Predicate_On_Actual  Flag276
   --    Is_Checked_Ghost_Entity         Flag277
   --    Is_Ignored_Ghost_Entity         Flag278
   --    Contains_Ignored_Ghost_Code     Flag279
   --    Partial_View_Has_Unknown_Discr  Flag280

   --    Is_Static_Type                  Flag281
   --    Has_Nested_Subprogram           Flag282
   --    Is_Uplevel_Referenced_Entity    Flag283
   --    Is_Unimplemented                Flag284
   --    Is_Volatile_Full_Access         Flag285
   --    Is_Exception_Handler            Flag286
   --    Rewritten_For_C                 Flag287

   --    (unused)                        Flag288
   --    (unused)                        Flag289
   --    (unused)                        Flag300

   --    (unused)                        Flag301
   --    (unused)                        Flag302
   --    (unused)                        Flag303
   --    (unused)                        Flag304
   --    (unused)                        Flag305
   --    (unused)                        Flag306
   --    (unused)                        Flag307
   --    (unused)                        Flag308
   --    (unused)                        Flag309

   --  Note: Flag310-317 are defined in atree.ads/adb, but not yet in atree.h

   -----------------------
   -- Local subprograms --
   -----------------------

   function Has_Option
     (State_Id   : Entity_Id;
      Option_Nam : Name_Id) return Boolean;
   --  Determine whether abstract state State_Id has particular option denoted
   --  by the name Option_Nam.

   ---------------
   -- Float_Rep --
   ---------------

   function Float_Rep (Id : E) return F is
      pragma Assert (Is_Floating_Point_Type (Id));
   begin
      return F'Val (UI_To_Int (Uint10 (Base_Type (Id))));
   end Float_Rep;

   ----------------
   -- Has_Option --
   ----------------

   function Has_Option
     (State_Id   : Entity_Id;
      Option_Nam : Name_Id) return Boolean
   is
      Decl    : constant Node_Id := Parent (State_Id);
      Opt     : Node_Id;
      Opt_Nam : Node_Id;

   begin
      pragma Assert (Ekind (State_Id) = E_Abstract_State);

      --  The declaration of abstract states with options appear as an
      --  extension aggregate. If this is not the case, the option is not
      --  available.

      if Nkind (Decl) /= N_Extension_Aggregate then
         return False;
      end if;

      --  Simple options

      Opt := First (Expressions (Decl));
      while Present (Opt) loop
         if Nkind (Opt) = N_Identifier and then Chars (Opt) = Option_Nam then
            return True;
         end if;

         Next (Opt);
      end loop;

      --  Complex options with various specifiers

      Opt := First (Component_Associations (Decl));
      while Present (Opt) loop
         Opt_Nam := First (Choices (Opt));

         if Nkind (Opt_Nam) = N_Identifier
           and then Chars (Opt_Nam) = Option_Nam
         then
            return True;
         end if;

         Next (Opt);
      end loop;

      return False;
   end Has_Option;

   --------------------------------
   -- Attribute Access Functions --
   --------------------------------

   function Abstract_States (Id : E) return L is
   begin
      pragma Assert (Ekind_In (Id, E_Generic_Package, E_Package));
      return Elist25 (Id);
   end Abstract_States;

   function Accept_Address (Id : E) return L is
   begin
      return Elist21 (Id);
   end Accept_Address;

   function Access_Disp_Table (Id : E) return L is
   begin
      pragma Assert (Ekind_In (Id, E_Record_Type,
                                   E_Record_Type_With_Private,
                                   E_Record_Subtype));
      return Elist16 (Implementation_Base_Type (Id));
   end Access_Disp_Table;

   function Activation_Record_Component (Id : E) return E is
   begin
      pragma Assert (Ekind_In (Id, E_Constant,
                                   E_In_Parameter,
                                   E_In_Out_Parameter,
                                   E_Loop_Parameter,
                                   E_Out_Parameter,
                                   E_Variable));
      return Node31 (Id);
   end Activation_Record_Component;

   function Actual_Subtype (Id : E) return E is
   begin
      pragma Assert
        (Ekind_In (Id, E_Constant, E_Variable, E_Generic_In_Out_Parameter)
           or else Is_Formal (Id));
      return Node17 (Id);
   end Actual_Subtype;

   function Address_Taken (Id : E) return B is
   begin
      return Flag104 (Id);
   end Address_Taken;

   function Alias (Id : E) return E is
   begin
      pragma Assert
        (Is_Overloadable (Id) or else Ekind (Id) = E_Subprogram_Type);
      return Node18 (Id);
   end Alias;

   function Alignment (Id : E) return U is
   begin
      pragma Assert (Is_Type (Id)
                       or else Is_Formal (Id)
                       or else Ekind_In (Id, E_Loop_Parameter,
                                             E_Constant,
                                             E_Exception,
                                             E_Variable));
      return Uint14 (Id);
   end Alignment;

   function Anonymous_Master (Id : E) return E is
   begin
      pragma Assert (Ekind_In (Id, E_Function,
                                   E_Package,
                                   E_Package_Body,
                                   E_Procedure,
                                   E_Subprogram_Body));
      return Node36 (Id);
   end Anonymous_Master;

   function Anonymous_Object (Id : E) return E is
   begin
      pragma Assert (Ekind_In (Id, E_Protected_Type, E_Task_Type));
      return Node30 (Id);
   end Anonymous_Object;

   function Associated_Entity (Id : E) return E is
   begin
      return Node37 (Id);
   end Associated_Entity;

   function Associated_Formal_Package (Id : E) return E is
   begin
      pragma Assert (Ekind (Id) = E_Package);
      return Node12 (Id);
   end Associated_Formal_Package;

   function Associated_Node_For_Itype (Id : E) return N is
   begin
      return Node8 (Id);
   end Associated_Node_For_Itype;

   function Associated_Storage_Pool (Id : E) return E is
   begin
      pragma Assert (Is_Access_Type (Id));
      return Node22 (Root_Type (Id));
   end Associated_Storage_Pool;

   function Barrier_Function (Id : E) return N is
   begin
      pragma Assert (Is_Entry (Id));
      return Node12 (Id);
   end Barrier_Function;

   function Block_Node (Id : E) return N is
   begin
      pragma Assert (Ekind (Id) = E_Block);
      return Node11 (Id);
   end Block_Node;

   function Body_Entity (Id : E) return E is
   begin
      pragma Assert (Ekind_In (Id, E_Package, E_Generic_Package));
      return Node19 (Id);
   end Body_Entity;

   function Body_Needed_For_SAL (Id : E) return B is
   begin
      pragma Assert
        (Ekind (Id) = E_Package
           or else Is_Subprogram (Id)
           or else Is_Generic_Unit (Id));
      return Flag40 (Id);
   end Body_Needed_For_SAL;

   function Body_References (Id : E) return L is
   begin
      pragma Assert (Ekind (Id) = E_Abstract_State);
      return Elist16 (Id);
   end Body_References;

   function BIP_Initialization_Call (Id : E) return N is
   begin
      pragma Assert (Ekind_In (Id, E_Constant, E_Variable));
      return Node29 (Id);
   end BIP_Initialization_Call;

   function C_Pass_By_Copy (Id : E) return B is
   begin
      pragma Assert (Is_Record_Type (Id));
      return Flag125 (Implementation_Base_Type (Id));
   end C_Pass_By_Copy;

   function Can_Never_Be_Null (Id : E) return B is
   begin
      return Flag38 (Id);
   end Can_Never_Be_Null;

   function Checks_May_Be_Suppressed (Id : E) return B is
   begin
      return Flag31 (Id);
   end Checks_May_Be_Suppressed;

   function Class_Wide_Postconds (Id : E) return S is
   begin
      pragma Assert (Is_Subprogram (Id));
      return List39 (Id);
   end Class_Wide_Postconds;

   function Class_Wide_Preconds (Id : E) return S is
   begin
      pragma Assert (Is_Subprogram (Id));
      return List38 (Id);
   end Class_Wide_Preconds;

   function Class_Wide_Type (Id : E) return E is
   begin
      pragma Assert (Is_Type (Id));
      return Node9 (Id);
   end Class_Wide_Type;

   function Cloned_Subtype (Id : E) return E is
   begin
      pragma Assert (Ekind_In (Id, E_Record_Subtype, E_Class_Wide_Subtype));
      return Node16 (Id);
   end Cloned_Subtype;

   function Component_Bit_Offset (Id : E) return U is
   begin
      pragma Assert (Ekind_In (Id, E_Component, E_Discriminant));
      return Uint11 (Id);
   end Component_Bit_Offset;

   function Component_Clause (Id : E) return N is
   begin
      pragma Assert (Ekind_In (Id, E_Component, E_Discriminant));
      return Node13 (Id);
   end Component_Clause;

   function Component_Size (Id : E) return U is
   begin
      pragma Assert (Is_Array_Type (Id));
      return Uint22 (Implementation_Base_Type (Id));
   end Component_Size;

   function Component_Type (Id : E) return E is
   begin
      pragma Assert (Is_Array_Type (Id));
      return Node20 (Implementation_Base_Type (Id));
   end Component_Type;

   function Corresponding_Concurrent_Type (Id : E) return E is
   begin
      pragma Assert (Ekind (Id) = E_Record_Type);
      return Node18 (Id);
   end Corresponding_Concurrent_Type;

   function Corresponding_Discriminant (Id : E) return E is
   begin
      pragma Assert (Ekind (Id) = E_Discriminant);
      return Node19 (Id);
   end Corresponding_Discriminant;

   function Corresponding_Equality (Id : E) return E is
   begin
      pragma Assert
        (Ekind (Id) = E_Function
          and then not Comes_From_Source (Id)
          and then Chars (Id) = Name_Op_Ne);
      return Node30 (Id);
   end Corresponding_Equality;

   function Corresponding_Protected_Entry (Id : E) return E is
   begin
      pragma Assert (Ekind (Id) = E_Subprogram_Body);
      return Node18 (Id);
   end Corresponding_Protected_Entry;

   function Corresponding_Record_Type (Id : E) return E is
   begin
      pragma Assert (Is_Concurrent_Type (Id));
      return Node18 (Id);
   end Corresponding_Record_Type;

   function Corresponding_Remote_Type (Id : E) return E is
   begin
      return Node22 (Id);
   end Corresponding_Remote_Type;

   function Current_Use_Clause (Id : E) return E is
   begin
      pragma Assert (Ekind (Id) = E_Package or else Is_Type (Id));
      return Node27 (Id);
   end Current_Use_Clause;

   function Current_Value (Id : E) return N is
   begin
      pragma Assert (Ekind (Id) in Object_Kind);
      return Node9 (Id);
   end Current_Value;

   function CR_Discriminant (Id : E) return E is
   begin
      return Node23 (Id);
   end CR_Discriminant;

   function Debug_Info_Off (Id : E) return B is
   begin
      return Flag166 (Id);
   end Debug_Info_Off;

   function Debug_Renaming_Link (Id : E) return E is
   begin
      return Node25 (Id);
   end Debug_Renaming_Link;

   function Default_Aspect_Component_Value (Id : E) return N is
   begin
      pragma Assert (Is_Array_Type (Id));
      return Node19 (Base_Type (Id));
   end Default_Aspect_Component_Value;

   function Default_Aspect_Value (Id : E) return N is
   begin
      pragma Assert (Is_Scalar_Type (Id));
      return Node19 (Base_Type (Id));
   end Default_Aspect_Value;

   function Default_Expr_Function (Id : E) return E is
   begin
      pragma Assert (Is_Formal (Id));
      return Node21 (Id);
   end Default_Expr_Function;

   function Default_Expressions_Processed (Id : E) return B is
   begin
      return Flag108 (Id);
   end Default_Expressions_Processed;

   function Default_Value (Id : E) return N is
   begin
      pragma Assert (Is_Formal (Id));
      return Node20 (Id);
   end Default_Value;

   function Delay_Cleanups (Id : E) return B is
   begin
      return Flag114 (Id);
   end Delay_Cleanups;

   function Delay_Subprogram_Descriptors (Id : E) return B is
   begin
      return Flag50 (Id);
   end Delay_Subprogram_Descriptors;

   function Delta_Value (Id : E) return R is
   begin
      pragma Assert (Is_Fixed_Point_Type (Id));
      return Ureal18 (Id);
   end Delta_Value;

   function Dependent_Instances (Id : E) return L is
   begin
      pragma Assert (Is_Generic_Instance (Id));
      return Elist8 (Id);
   end Dependent_Instances;

   function Depends_On_Private (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Flag14 (Id);
   end Depends_On_Private;

   function Derived_Type_Link (Id : E) return E is
   begin
      pragma Assert (Is_Type (Id));
      return Node31 (Base_Type (Id));
   end Derived_Type_Link;

   function Digits_Value (Id : E) return U is
   begin
      pragma Assert
        (Is_Floating_Point_Type (Id)
          or else Is_Decimal_Fixed_Point_Type (Id));
      return Uint17 (Id);
   end Digits_Value;

   function Direct_Primitive_Operations (Id : E) return L is
   begin
      pragma Assert (Is_Tagged_Type (Id));
      return Elist10 (Id);
   end Direct_Primitive_Operations;

   function Directly_Designated_Type (Id : E) return E is
   begin
      pragma Assert (Is_Access_Type (Id));
      return Node20 (Id);
   end Directly_Designated_Type;

   function Disable_Controlled (Id : E) return B is
   begin
      return Flag253 (Base_Type (Id));
   end Disable_Controlled;

   function Discard_Names (Id : E) return B is
   begin
      return Flag88 (Id);
   end Discard_Names;

   function Discriminal (Id : E) return E is
   begin
      pragma Assert (Ekind (Id) = E_Discriminant);
      return Node17 (Id);
   end Discriminal;

   function Discriminal_Link (Id : E) return N is
   begin
      return Node10 (Id);
   end Discriminal_Link;

   function Discriminant_Checking_Func (Id : E) return E is
   begin
      pragma Assert (Ekind (Id) = E_Component);
      return Node20 (Id);
   end Discriminant_Checking_Func;

   function Discriminant_Constraint (Id : E) return L is
   begin
      pragma Assert (Is_Composite_Type (Id) and then Has_Discriminants (Id));
      return Elist21 (Id);
   end Discriminant_Constraint;

   function Discriminant_Default_Value (Id : E) return N is
   begin
      pragma Assert (Ekind (Id) = E_Discriminant);
      return Node20 (Id);
   end Discriminant_Default_Value;

   function Discriminant_Number (Id : E) return U is
   begin
      pragma Assert (Ekind (Id) = E_Discriminant);
      return Uint15 (Id);
   end Discriminant_Number;

   function Dispatch_Table_Wrappers (Id : E) return L is
   begin
      pragma Assert (Ekind_In (Id, E_Record_Type,
                                   E_Record_Subtype));
      return Elist26 (Implementation_Base_Type (Id));
   end Dispatch_Table_Wrappers;

   function DT_Entry_Count (Id : E) return U is
   begin
      pragma Assert (Ekind (Id) = E_Component and then Is_Tag (Id));
      return Uint15 (Id);
   end DT_Entry_Count;

   function DT_Offset_To_Top_Func (Id : E) return E is
   begin
      pragma Assert (Ekind (Id) = E_Component and then Is_Tag (Id));
      return Node25 (Id);
   end DT_Offset_To_Top_Func;

   function DT_Position (Id : E) return U is
   begin
      pragma Assert (Ekind_In (Id, E_Function, E_Procedure)
                       and then Present (DTC_Entity (Id)));
      return Uint15 (Id);
   end DT_Position;

   function DTC_Entity (Id : E) return E is
   begin
      pragma Assert (Ekind_In (Id, E_Function, E_Procedure));
      return Node16 (Id);
   end DTC_Entity;

   function Elaborate_Body_Desirable (Id : E) return B is
   begin
      pragma Assert (Ekind (Id) = E_Package);
      return Flag210 (Id);
   end Elaborate_Body_Desirable;

   function Elaboration_Entity (Id : E) return E is
   begin
      pragma Assert
        (Is_Subprogram (Id)
           or else
         Ekind (Id) = E_Package
           or else
         Is_Generic_Unit (Id));
      return Node13 (Id);
   end Elaboration_Entity;

   function Elaboration_Entity_Required (Id : E) return B is
   begin
      pragma Assert
        (Is_Subprogram (Id)
           or else
         Ekind (Id) = E_Package
           or else
         Is_Generic_Unit (Id));
      return Flag174 (Id);
   end Elaboration_Entity_Required;

   function Encapsulating_State (Id : E) return N is
   begin
      pragma Assert (Ekind_In (Id, E_Abstract_State, E_Constant, E_Variable));
      return Node32 (Id);
   end Encapsulating_State;

   function Enclosing_Scope (Id : E) return E is
   begin
      return Node18 (Id);
   end Enclosing_Scope;

   function Entry_Accepted (Id : E) return B is
   begin
      pragma Assert (Is_Entry (Id));
      return Flag152 (Id);
   end Entry_Accepted;

   function Entry_Bodies_Array (Id : E) return E is
   begin
      return Node19 (Id);
   end Entry_Bodies_Array;

   function Entry_Cancel_Parameter (Id : E) return E is
   begin
      return Node23 (Id);
   end Entry_Cancel_Parameter;

   function Entry_Component (Id : E) return E is
   begin
      return Node11 (Id);
   end Entry_Component;

   function Entry_Formal (Id : E) return E is
   begin
      return Node16 (Id);
   end Entry_Formal;

   function Entry_Index_Constant (Id : E) return N is
   begin
      pragma Assert (Ekind (Id) = E_Entry_Index_Parameter);
      return Node18 (Id);
   end Entry_Index_Constant;

   function Contains_Ignored_Ghost_Code (Id : E) return B is
   begin
      pragma Assert
        (Ekind_In (Id, E_Block,
                       E_Function,
                       E_Generic_Function,
                       E_Generic_Package,
                       E_Generic_Procedure,
                       E_Package,
                       E_Package_Body,
                       E_Procedure,
                       E_Subprogram_Body));
      return Flag279 (Id);
   end Contains_Ignored_Ghost_Code;

   function Contract (Id : E) return N is
   begin
      pragma Assert
        (Ekind_In (Id, E_Protected_Type,   --  concurrent variants
                       E_Task_Body,
                       E_Task_Type)
           or else
         Ekind_In (Id, E_Constant,         --  object variants
                       E_Variable)
           or else
         Ekind_In (Id, E_Entry,            --  overloadable variants
                       E_Entry_Family,
                       E_Function,
                       E_Generic_Function,
                       E_Generic_Procedure,
                       E_Operator,
                       E_Procedure,
                       E_Subprogram_Body)
           or else
         Ekind_In (Id, E_Generic_Package,  --  package variants
                       E_Package,
                       E_Package_Body)
           or else
         Ekind (Id) = E_Void);             --  special purpose
      return Node34 (Id);
   end Contract;

   function Contract_Wrapper (Id : E) return E is
   begin
      pragma Assert (Ekind_In (Id, E_Entry, E_Entry_Family));
      return Node25 (Id);
   end Contract_Wrapper;

   function Entry_Parameters_Type (Id : E) return E is
   begin
      return Node15 (Id);
   end Entry_Parameters_Type;

   function Enum_Pos_To_Rep (Id : E) return E is
   begin
      pragma Assert (Ekind (Id) = E_Enumeration_Type);
      return Node23 (Id);
   end Enum_Pos_To_Rep;

   function Enumeration_Pos (Id : E) return Uint is
   begin
      pragma Assert (Ekind (Id) = E_Enumeration_Literal);
      return Uint11 (Id);
   end Enumeration_Pos;

   function Enumeration_Rep (Id : E) return U is
   begin
      pragma Assert (Ekind (Id) = E_Enumeration_Literal);
      return Uint12 (Id);
   end Enumeration_Rep;

   function Enumeration_Rep_Expr (Id : E) return N is
   begin
      pragma Assert (Ekind (Id) = E_Enumeration_Literal);
      return Node22 (Id);
   end Enumeration_Rep_Expr;

   function Equivalent_Type (Id : E) return E is
   begin
      pragma Assert
        (Ekind_In (Id, E_Class_Wide_Type,
                       E_Class_Wide_Subtype,
                       E_Access_Subprogram_Type,
                       E_Access_Protected_Subprogram_Type,
                       E_Anonymous_Access_Protected_Subprogram_Type,
                       E_Access_Subprogram_Type,
                       E_Exception_Type));
      return Node18 (Id);
   end Equivalent_Type;

   function Esize (Id : E) return Uint is
   begin
      return Uint12 (Id);
   end Esize;

   function Extra_Accessibility (Id : E) return E is
   begin
      pragma Assert
        (Is_Formal (Id) or else Ekind_In (Id, E_Variable, E_Constant));
      return Node13 (Id);
   end Extra_Accessibility;

   function Extra_Accessibility_Of_Result (Id : E) return E is
   begin
      pragma Assert (Ekind_In (Id, E_Function, E_Operator, E_Subprogram_Type));
      return Node19 (Id);
   end Extra_Accessibility_Of_Result;

   function Extra_Constrained (Id : E) return E is
   begin
      pragma Assert (Is_Formal (Id) or else Ekind (Id) = E_Variable);
      return Node23 (Id);
   end Extra_Constrained;

   function Extra_Formal (Id : E) return E is
   begin
      return Node15 (Id);
   end Extra_Formal;

   function Extra_Formals (Id : E) return E is
   begin
      pragma Assert
        (Is_Overloadable (Id)
           or else Ekind_In (Id, E_Entry_Family,
                                 E_Subprogram_Body,
                                 E_Subprogram_Type));
      return Node28 (Id);
   end Extra_Formals;

   function Can_Use_Internal_Rep (Id : E) return B is
   begin
      pragma Assert (Is_Access_Subprogram_Type (Base_Type (Id)));
      return Flag229 (Base_Type (Id));
   end Can_Use_Internal_Rep;

   function Finalization_Master (Id : E) return E is
   begin
      pragma Assert (Is_Access_Type (Id));
      return Node23 (Root_Type (Id));
   end Finalization_Master;

   function Finalize_Storage_Only (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag158 (Base_Type (Id));
   end Finalize_Storage_Only;

   function Finalizer (Id : E) return E is
   begin
      pragma Assert (Ekind_In (Id, E_Package, E_Package_Body));
      return Node28 (Id);
   end Finalizer;

   function First_Entity (Id : E) return E is
   begin
      return Node17 (Id);
   end First_Entity;

   function First_Exit_Statement (Id : E) return N is
   begin
      pragma Assert (Ekind (Id) = E_Loop);
      return Node8 (Id);
   end First_Exit_Statement;

   function First_Index (Id : E) return N is
   begin
      pragma Assert (Is_Array_Type (Id));
      return Node17 (Id);
   end First_Index;

   function First_Literal (Id : E) return E is
   begin
      pragma Assert (Is_Enumeration_Type (Id));
      return Node17 (Id);
   end First_Literal;

   function First_Private_Entity (Id : E) return E is
   begin
      pragma Assert (Ekind_In (Id, E_Package, E_Generic_Package)
                       or else Ekind (Id) in Concurrent_Kind);
      return Node16 (Id);
   end First_Private_Entity;

   function First_Rep_Item (Id : E) return E is
   begin
      return Node6 (Id);
   end First_Rep_Item;

   function Freeze_Node (Id : E) return N is
   begin
      return Node7 (Id);
   end Freeze_Node;

   function From_Limited_With (Id : E) return B is
   begin
      return Flag159 (Id);
   end From_Limited_With;

   function Full_View (Id : E) return E is
   begin
      pragma Assert (Is_Type (Id) or else Ekind (Id) = E_Constant);
      return Node11 (Id);
   end Full_View;

   function Generic_Homonym (Id : E) return E is
   begin
      pragma Assert (Ekind (Id) = E_Generic_Package);
      return Node11 (Id);
   end Generic_Homonym;

   function Generic_Renamings (Id : E) return L is
   begin
      return Elist23 (Id);
   end Generic_Renamings;

   function Handler_Records (Id : E) return S is
   begin
      return List10 (Id);
   end Handler_Records;

   function Has_Aliased_Components (Id : E) return B is
   begin
      return Flag135 (Implementation_Base_Type (Id));
   end Has_Aliased_Components;

   function Has_Alignment_Clause (Id : E) return B is
   begin
      return Flag46 (Id);
   end Has_Alignment_Clause;

   function Has_All_Calls_Remote (Id : E) return B is
   begin
      return Flag79 (Id);
   end Has_All_Calls_Remote;

   function Has_Atomic_Components (Id : E) return B is
   begin
      return Flag86 (Implementation_Base_Type (Id));
   end Has_Atomic_Components;

   function Has_Biased_Representation (Id : E) return B is
   begin
      return Flag139 (Id);
   end Has_Biased_Representation;

   function Has_Completion (Id : E) return B is
   begin
      return Flag26 (Id);
   end Has_Completion;

   function Has_Completion_In_Body (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag71 (Id);
   end Has_Completion_In_Body;

   function Has_Complex_Representation (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag140 (Implementation_Base_Type (Id));
   end Has_Complex_Representation;

   function Has_Component_Size_Clause (Id : E) return B is
   begin
      pragma Assert (Is_Array_Type (Id));
      return Flag68 (Implementation_Base_Type (Id));
   end Has_Component_Size_Clause;

   function Has_Constrained_Partial_View (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag187 (Id);
   end Has_Constrained_Partial_View;

   function Has_Controlled_Component (Id : E) return B is
   begin
      return Flag43 (Base_Type (Id));
   end Has_Controlled_Component;

   function Has_Contiguous_Rep (Id : E) return B is
   begin
      return Flag181 (Id);
   end Has_Contiguous_Rep;

   function Has_Controlling_Result (Id : E) return B is
   begin
      return Flag98 (Id);
   end Has_Controlling_Result;

   function Has_Convention_Pragma (Id : E) return B is
   begin
      return Flag119 (Id);
   end Has_Convention_Pragma;

   function Has_Default_Aspect (Id : E) return B is
   begin
      return Flag39 (Base_Type (Id));
   end Has_Default_Aspect;

   function Has_Default_Init_Cond (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag3 (Base_Type (Id));
   end Has_Default_Init_Cond;

   function Has_Delayed_Aspects (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Flag200 (Id);
   end Has_Delayed_Aspects;

   function Has_Delayed_Freeze (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Flag18 (Id);
   end Has_Delayed_Freeze;

   function Has_Delayed_Rep_Aspects (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Flag261 (Id);
   end Has_Delayed_Rep_Aspects;

   function Has_Discriminants (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Flag5 (Id);
   end Has_Discriminants;

   function Has_Dispatch_Table (Id : E) return B is
   begin
      pragma Assert (Is_Tagged_Type (Id));
      return Flag220 (Id);
   end Has_Dispatch_Table;

   function Has_Dynamic_Predicate_Aspect (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag258 (Id);
   end Has_Dynamic_Predicate_Aspect;

   function Has_Enumeration_Rep_Clause (Id : E) return B is
   begin
      pragma Assert (Is_Enumeration_Type (Id));
      return Flag66 (Id);
   end Has_Enumeration_Rep_Clause;

   function Has_Exit (Id : E) return B is
   begin
      return Flag47 (Id);
   end Has_Exit;

   function Has_Expanded_Contract (Id : E) return B is
   begin
      pragma Assert (Is_Subprogram (Id));
      return Flag240 (Id);
   end Has_Expanded_Contract;

   function Has_Forward_Instantiation (Id : E) return B is
   begin
      return Flag175 (Id);
   end Has_Forward_Instantiation;

   function Has_Fully_Qualified_Name (Id : E) return B is
   begin
      return Flag173 (Id);
   end Has_Fully_Qualified_Name;

   function Has_Gigi_Rep_Item (Id : E) return B is
   begin
      return Flag82 (Id);
   end Has_Gigi_Rep_Item;

   function Has_Homonym (Id : E) return B is
   begin
      return Flag56 (Id);
   end Has_Homonym;

   function Has_Implicit_Dereference (Id : E) return B is
   begin
      return Flag251 (Id);
   end Has_Implicit_Dereference;

   function Has_Independent_Components (Id : E) return B is
   begin
      return Flag34 (Implementation_Base_Type (Id));
   end Has_Independent_Components;

   function Has_Inheritable_Invariants (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag248 (Id);
   end Has_Inheritable_Invariants;

   function Has_Inherited_Default_Init_Cond (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag133 (Base_Type (Id));
   end Has_Inherited_Default_Init_Cond;

   function Has_Initial_Value (Id : E) return B is
   begin
      pragma Assert (Ekind (Id) = E_Variable or else Is_Formal (Id));
      return Flag219 (Id);
   end Has_Initial_Value;

   function Has_Invariants (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag232 (Id);
   end Has_Invariants;

   function Has_Loop_Entry_Attributes (Id : E) return B is
   begin
      pragma Assert (Ekind (Id) = E_Loop);
      return Flag260 (Id);
   end Has_Loop_Entry_Attributes;

   function Has_Machine_Radix_Clause (Id : E) return B is
   begin
      pragma Assert (Is_Decimal_Fixed_Point_Type (Id));
      return Flag83 (Id);
   end Has_Machine_Radix_Clause;

   function Has_Master_Entity (Id : E) return B is
   begin
      return Flag21 (Id);
   end Has_Master_Entity;

   function Has_Missing_Return (Id : E) return B is
   begin
      pragma Assert (Ekind_In (Id, E_Function, E_Generic_Function));
      return Flag142 (Id);
   end Has_Missing_Return;

   function Has_Nested_Block_With_Handler (Id : E) return B is
   begin
      return Flag101 (Id);
   end Has_Nested_Block_With_Handler;

   function Has_Nested_Subprogram (Id : E) return B is
   begin
      pragma Assert (Is_Subprogram (Id));
      return Flag282 (Id);
   end Has_Nested_Subprogram;

   function Has_Non_Standard_Rep (Id : E) return B is
   begin
      return Flag75 (Implementation_Base_Type (Id));
   end Has_Non_Standard_Rep;

   function Has_Object_Size_Clause (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag172 (Id);
   end Has_Object_Size_Clause;

   function Has_Out_Or_In_Out_Parameter (Id : E) return B is
   begin
      pragma Assert
        (Ekind_In (Id, E_Entry, E_Entry_Family)
          or else Is_Subprogram_Or_Generic_Subprogram (Id));
      return Flag110 (Id);
   end Has_Out_Or_In_Out_Parameter;

   function Has_Per_Object_Constraint (Id : E) return B is
   begin
      return Flag154 (Id);
   end Has_Per_Object_Constraint;

   function Has_Pragma_Controlled (Id : E) return B is
   begin
      pragma Assert (Is_Access_Type (Id));
      return Flag27 (Implementation_Base_Type (Id));
   end Has_Pragma_Controlled;

   function Has_Pragma_Elaborate_Body (Id : E) return B is
   begin
      return Flag150 (Id);
   end Has_Pragma_Elaborate_Body;

   function Has_Pragma_Inline (Id : E) return B is
   begin
      return Flag157 (Id);
   end Has_Pragma_Inline;

   function Has_Pragma_Inline_Always (Id : E) return B is
   begin
      return Flag230 (Id);
   end Has_Pragma_Inline_Always;

   function Has_Pragma_No_Inline (Id : E) return B is
   begin
      return Flag201 (Id);
   end Has_Pragma_No_Inline;

   function Has_Pragma_Ordered (Id : E) return B is
   begin
      pragma Assert (Is_Enumeration_Type (Id));
      return Flag198 (Implementation_Base_Type (Id));
   end Has_Pragma_Ordered;

   function Has_Pragma_Pack (Id : E) return B is
   begin
      pragma Assert (Is_Record_Type (Id) or else Is_Array_Type (Id));
      return Flag121 (Implementation_Base_Type (Id));
   end Has_Pragma_Pack;

   function Has_Pragma_Preelab_Init (Id : E) return B is
   begin
      return Flag221 (Id);
   end Has_Pragma_Preelab_Init;

   function Has_Pragma_Pure (Id : E) return B is
   begin
      return Flag203 (Id);
   end Has_Pragma_Pure;

   function Has_Pragma_Pure_Function (Id : E) return B is
   begin
      return Flag179 (Id);
   end Has_Pragma_Pure_Function;

   function Has_Pragma_Thread_Local_Storage (Id : E) return B is
   begin
      return Flag169 (Id);
   end Has_Pragma_Thread_Local_Storage;

   function Has_Pragma_Unmodified (Id : E) return B is
   begin
      return Flag233 (Id);
   end Has_Pragma_Unmodified;

   function Has_Pragma_Unreferenced (Id : E) return B is
   begin
      return Flag180 (Id);
   end Has_Pragma_Unreferenced;

   function Has_Pragma_Unreferenced_Objects (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag212 (Id);
   end Has_Pragma_Unreferenced_Objects;

   function Has_Predicates (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag250 (Id);
   end Has_Predicates;

   function Has_Primitive_Operations (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag120 (Base_Type (Id));
   end Has_Primitive_Operations;

   function Has_Private_Ancestor (Id : E) return B is
   begin
      return Flag151 (Id);
   end Has_Private_Ancestor;

   function Has_Private_Declaration (Id : E) return B is
   begin
      return Flag155 (Id);
   end Has_Private_Declaration;

   function Has_Protected (Id : E) return B is
   begin
      return Flag271 (Base_Type (Id));
   end Has_Protected;

   function Has_Qualified_Name (Id : E) return B is
   begin
      return Flag161 (Id);
   end Has_Qualified_Name;

   function Has_RACW (Id : E) return B is
   begin
      pragma Assert (Ekind (Id) = E_Package);
      return Flag214 (Id);
   end Has_RACW;

   function Has_Record_Rep_Clause (Id : E) return B is
   begin
      pragma Assert (Is_Record_Type (Id));
      return Flag65 (Implementation_Base_Type (Id));
   end Has_Record_Rep_Clause;

   function Has_Recursive_Call (Id : E) return B is
   begin
      pragma Assert (Is_Subprogram (Id));
      return Flag143 (Id);
   end Has_Recursive_Call;

   function Has_Shift_Operator (Id : E) return B is
   begin
      pragma Assert (Is_Integer_Type (Id));
      return Flag267 (Base_Type (Id));
   end Has_Shift_Operator;

   function Has_Size_Clause (Id : E) return B is
   begin
      return Flag29 (Id);
   end Has_Size_Clause;

   function Has_Small_Clause (Id : E) return B is
   begin
      pragma Assert (Is_Ordinary_Fixed_Point_Type (Id));
      return Flag67 (Id);
   end Has_Small_Clause;

   function Has_Specified_Layout (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag100 (Implementation_Base_Type (Id));
   end Has_Specified_Layout;

   function Has_Specified_Stream_Input (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag190 (Id);
   end Has_Specified_Stream_Input;

   function Has_Specified_Stream_Output (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag191 (Id);
   end Has_Specified_Stream_Output;

   function Has_Specified_Stream_Read (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag192 (Id);
   end Has_Specified_Stream_Read;

   function Has_Specified_Stream_Write (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag193 (Id);
   end Has_Specified_Stream_Write;

   function Has_Static_Discriminants (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag211 (Id);
   end Has_Static_Discriminants;

   function Has_Static_Predicate (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag269 (Id);
   end Has_Static_Predicate;

   function Has_Static_Predicate_Aspect (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag259 (Id);
   end Has_Static_Predicate_Aspect;

   function Has_Storage_Size_Clause (Id : E) return B is
   begin
      pragma Assert (Is_Access_Type (Id) or else Is_Task_Type (Id));
      return Flag23 (Implementation_Base_Type (Id));
   end Has_Storage_Size_Clause;

   function Has_Stream_Size_Clause (Id : E) return B is
   begin
      return Flag184 (Id);
   end Has_Stream_Size_Clause;

   function Has_Task (Id : E) return B is
   begin
      return Flag30 (Base_Type (Id));
   end Has_Task;

   function Has_Thunks (Id : E) return B is
   begin
      return Flag228 (Id);
   end Has_Thunks;

   function Has_Unchecked_Union (Id : E) return B is
   begin
      return Flag123 (Base_Type (Id));
   end Has_Unchecked_Union;

   function Has_Unknown_Discriminants (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag72 (Id);
   end Has_Unknown_Discriminants;

   function Has_Visible_Refinement (Id : E) return B is
   begin
      pragma Assert (Ekind (Id) = E_Abstract_State);
      return Flag263 (Id);
   end Has_Visible_Refinement;

   function Has_Volatile_Components (Id : E) return B is
   begin
      return Flag87 (Implementation_Base_Type (Id));
   end Has_Volatile_Components;

   function Has_Xref_Entry (Id : E) return B is
   begin
      return Flag182 (Id);
   end Has_Xref_Entry;

   function Hiding_Loop_Variable (Id : E) return E is
   begin
      pragma Assert (Ekind (Id) = E_Variable);
      return Node8 (Id);
   end Hiding_Loop_Variable;

   function Homonym (Id : E) return E is
   begin
      return Node4 (Id);
   end Homonym;

   function Import_Pragma (Id : E) return E is
   begin
      pragma Assert (Is_Subprogram (Id));
      return Node35 (Id);
   end Import_Pragma;

   function Incomplete_Actuals (Id : E) return L is
   begin
      pragma Assert (Ekind (Id) = E_Package);
      return Elist24 (Id);
   end Incomplete_Actuals;

   function Interface_Alias (Id : E) return E is
   begin
      pragma Assert (Is_Subprogram (Id));
      return Node25 (Id);
   end Interface_Alias;

   function Interfaces (Id : E) return L is
   begin
      pragma Assert (Is_Record_Type (Id));
      return Elist25 (Id);
   end Interfaces;

   function In_Package_Body (Id : E) return B is
   begin
      return Flag48 (Id);
   end In_Package_Body;

   function In_Private_Part (Id : E) return B is
   begin
      return Flag45 (Id);
   end In_Private_Part;

   function In_Use (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Flag8 (Id);
   end In_Use;

   function Initialization_Statements (Id : E) return N is
   begin
      pragma Assert (Ekind_In (Id, E_Constant, E_Variable));
      return Node28 (Id);
   end Initialization_Statements;

   function Inner_Instances (Id : E) return L is
   begin
      return Elist23 (Id);
   end Inner_Instances;

   function Interface_Name (Id : E) return N is
   begin
      return Node21 (Id);
   end Interface_Name;

   function Is_Abstract_Subprogram (Id : E) return B is
   begin
      pragma Assert (Is_Overloadable (Id));
      return Flag19 (Id);
   end Is_Abstract_Subprogram;

   function Is_Abstract_Type (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag146 (Id);
   end Is_Abstract_Type;

   function Is_Access_Constant (Id : E) return B is
   begin
      pragma Assert (Is_Access_Type (Id));
      return Flag69 (Id);
   end Is_Access_Constant;

   function Is_Ada_2005_Only (Id : E) return B is
   begin
      return Flag185 (Id);
   end Is_Ada_2005_Only;

   function Is_Ada_2012_Only (Id : E) return B is
   begin
      return Flag199 (Id);
   end Is_Ada_2012_Only;

   function Is_Aliased (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Flag15 (Id);
   end Is_Aliased;

   function Is_Asynchronous (Id : E) return B is
   begin
      pragma Assert (Ekind (Id) = E_Procedure or else Is_Type (Id));
      return Flag81 (Id);
   end Is_Asynchronous;

   function Is_Atomic (Id : E) return B is
   begin
      return Flag85 (Id);
   end Is_Atomic;

   function Is_Bit_Packed_Array (Id : E) return B is
   begin
      return Flag122 (Implementation_Base_Type (Id));
   end Is_Bit_Packed_Array;

   function Is_Called (Id : E) return B is
   begin
      pragma Assert (Ekind_In (Id, E_Procedure, E_Function));
      return Flag102 (Id);
   end Is_Called;

   function Is_Character_Type (Id : E) return B is
   begin
      return Flag63 (Id);
   end Is_Character_Type;

   function Is_Checked_Ghost_Entity (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Flag277 (Id);
   end Is_Checked_Ghost_Entity;

   function Is_Child_Unit (Id : E) return B is
   begin
      return Flag73 (Id);
   end Is_Child_Unit;

   function Is_Class_Wide_Equivalent_Type (Id : E) return B is
   begin
      return Flag35 (Id);
   end Is_Class_Wide_Equivalent_Type;

   function Is_Compilation_Unit (Id : E) return B is
   begin
      return Flag149 (Id);
   end Is_Compilation_Unit;

   function Is_Completely_Hidden (Id : E) return B is
   begin
      pragma Assert (Ekind (Id) = E_Discriminant);
      return Flag103 (Id);
   end Is_Completely_Hidden;

   function Is_Constr_Subt_For_U_Nominal (Id : E) return B is
   begin
      return Flag80 (Id);
   end Is_Constr_Subt_For_U_Nominal;

   function Is_Constr_Subt_For_UN_Aliased (Id : E) return B is
   begin
      return Flag141 (Id);
   end Is_Constr_Subt_For_UN_Aliased;

   function Is_Constrained (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Flag12 (Id);
   end Is_Constrained;

   function Is_Constructor (Id : E) return B is
   begin
      return Flag76 (Id);
   end Is_Constructor;

   function Is_Controlled (Id : E) return B is
   begin
      return Flag42 (Base_Type (Id));
   end Is_Controlled;

   function Is_Controlling_Formal (Id : E) return B is
   begin
      pragma Assert (Is_Formal (Id));
      return Flag97 (Id);
   end Is_Controlling_Formal;

   function Is_CPP_Class (Id : E) return B is
   begin
      return Flag74 (Id);
   end Is_CPP_Class;

   function Is_Default_Init_Cond_Procedure (Id : E) return B is
   begin
      pragma Assert (Ekind_In (Id, E_Function, E_Procedure));
      return Flag132 (Id);
   end Is_Default_Init_Cond_Procedure;

   function Is_Descendant_Of_Address (Id : E) return B is
   begin
      return Flag223 (Id);
   end Is_Descendant_Of_Address;

   function Is_Discrim_SO_Function (Id : E) return B is
   begin
      return Flag176 (Id);
   end Is_Discrim_SO_Function;

   function Is_Discriminant_Check_Function (Id : E) return B is
   begin
      return Flag264 (Id);
   end Is_Discriminant_Check_Function;

   function Is_Dispatch_Table_Entity (Id : E) return B is
   begin
      return Flag234 (Id);
   end Is_Dispatch_Table_Entity;

   function Is_Dispatching_Operation (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Flag6 (Id);
   end Is_Dispatching_Operation;

   function Is_Eliminated (Id : E) return B is
   begin
      return Flag124 (Id);
   end Is_Eliminated;

   function Is_Entry_Formal (Id : E) return B is
   begin
      return Flag52 (Id);
   end Is_Entry_Formal;

   function Is_Exception_Handler (Id : E) return B is
   begin
      pragma Assert (Ekind (Id) = E_Block);
      return Flag286 (Id);
   end Is_Exception_Handler;

   function Is_Exported (Id : E) return B is
   begin
      return Flag99 (Id);
   end Is_Exported;

   function Is_First_Subtype (Id : E) return B is
   begin
      return Flag70 (Id);
   end Is_First_Subtype;

   function Is_For_Access_Subtype (Id : E) return B is
   begin
      pragma Assert (Ekind_In (Id, E_Record_Subtype, E_Private_Subtype));
      return Flag118 (Id);
   end Is_For_Access_Subtype;

   function Is_Formal_Subprogram (Id : E) return B is
   begin
      return Flag111 (Id);
   end Is_Formal_Subprogram;

   function Is_Frozen (Id : E) return B is
   begin
      return Flag4 (Id);
   end Is_Frozen;

   function Is_Generic_Actual_Subprogram (Id : E) return B is
   begin
      pragma Assert (Ekind (Id) = E_Function or else Ekind (Id) = E_Procedure);
      return Flag274 (Id);
   end Is_Generic_Actual_Subprogram;

   function Is_Generic_Actual_Type (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag94 (Id);
   end Is_Generic_Actual_Type;

   function Is_Generic_Instance (Id : E) return B is
   begin
      return Flag130 (Id);
   end Is_Generic_Instance;

   function Is_Generic_Type (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Flag13 (Id);
   end Is_Generic_Type;

   function Is_Hidden (Id : E) return B is
   begin
      return Flag57 (Id);
   end Is_Hidden;

   function Is_Hidden_Non_Overridden_Subpgm (Id : E) return B is
   begin
      return Flag2 (Id);
   end Is_Hidden_Non_Overridden_Subpgm;

   function Is_Hidden_Open_Scope (Id : E) return B is
   begin
      return Flag171 (Id);
   end Is_Hidden_Open_Scope;

   function Is_Ignored_Ghost_Entity (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Flag278 (Id);
   end Is_Ignored_Ghost_Entity;

   function Is_Immediately_Visible (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Flag7 (Id);
   end Is_Immediately_Visible;

   function Is_Implementation_Defined (Id : E) return B is
   begin
      return Flag254 (Id);
   end Is_Implementation_Defined;

   function Is_Imported (Id : E) return B is
   begin
      return Flag24 (Id);
   end Is_Imported;

   function Is_Independent (Id : E) return B is
   begin
      return Flag268 (Id);
   end Is_Independent;

   function Is_Inlined (Id : E) return B is
   begin
      return Flag11 (Id);
   end Is_Inlined;

   function Is_Inlined_Always (Id : E) return B is
   begin
      pragma Assert (Ekind (Id) = E_Function or else Ekind (Id) = E_Procedure);
      return Flag1 (Id);
   end Is_Inlined_Always;

   function Is_Interface (Id : E) return B is
   begin
      return Flag186 (Id);
   end Is_Interface;

   function Is_Instantiated (Id : E) return B is
   begin
      return Flag126 (Id);
   end Is_Instantiated;

   function Is_Internal (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Flag17 (Id);
   end Is_Internal;

   function Is_Interrupt_Handler (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Flag89 (Id);
   end Is_Interrupt_Handler;

   function Is_Intrinsic_Subprogram (Id : E) return B is
   begin
      return Flag64 (Id);
   end Is_Intrinsic_Subprogram;

   function Is_Invariant_Procedure (Id : E) return B is
   begin
      pragma Assert (Ekind_In (Id, E_Function, E_Procedure));
      return Flag257 (Id);
   end Is_Invariant_Procedure;

   function Is_Itype (Id : E) return B is
   begin
      return Flag91 (Id);
   end Is_Itype;

   function Is_Known_Non_Null (Id : E) return B is
   begin
      return Flag37 (Id);
   end Is_Known_Non_Null;

   function Is_Known_Null (Id : E) return B is
   begin
      return Flag204 (Id);
   end Is_Known_Null;

   function Is_Known_Valid (Id : E) return B is
   begin
      return Flag170 (Id);
   end Is_Known_Valid;

   function Is_Limited_Composite (Id : E) return B is
   begin
      return Flag106 (Id);
   end Is_Limited_Composite;

   function Is_Limited_Interface (Id : E) return B is
   begin
      return Flag197 (Id);
   end Is_Limited_Interface;

   function Is_Limited_Record (Id : E) return B is
   begin
      return Flag25 (Id);
   end Is_Limited_Record;

   function Is_Local_Anonymous_Access (Id : E) return B is
   begin
      pragma Assert (Is_Access_Type (Id));
      return Flag194 (Id);
   end Is_Local_Anonymous_Access;

   function Is_Machine_Code_Subprogram (Id : E) return B is
   begin
      pragma Assert (Is_Subprogram (Id));
      return Flag137 (Id);
   end Is_Machine_Code_Subprogram;

   function Is_Non_Static_Subtype (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag109 (Id);
   end Is_Non_Static_Subtype;

   function Is_Null_Init_Proc (Id : E) return B is
   begin
      pragma Assert (Ekind (Id) = E_Procedure);
      return Flag178 (Id);
   end Is_Null_Init_Proc;

   function Is_Obsolescent (Id : E) return B is
   begin
      return Flag153 (Id);
   end Is_Obsolescent;

   function Is_Only_Out_Parameter (Id : E) return B is
   begin
      pragma Assert (Is_Formal (Id));
      return Flag226 (Id);
   end Is_Only_Out_Parameter;

   function Is_Package_Body_Entity (Id : E) return B is
   begin
      return Flag160 (Id);
   end Is_Package_Body_Entity;

   function Is_Packed (Id : E) return B is
   begin
      return Flag51 (Implementation_Base_Type (Id));
   end Is_Packed;

   function Is_Packed_Array_Impl_Type (Id : E) return B is
   begin
      return Flag138 (Id);
   end Is_Packed_Array_Impl_Type;

   function Is_Param_Block_Component_Type (Id : E) return B is
   begin
      pragma Assert (Is_Access_Type (Id));
      return Flag215 (Base_Type (Id));
   end Is_Param_Block_Component_Type;

   function Is_Potentially_Use_Visible (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Flag9 (Id);
   end Is_Potentially_Use_Visible;

   function Is_Predicate_Function (Id : E) return B is
   begin
      pragma Assert (Ekind_In (Id, E_Function, E_Procedure));
      return Flag255 (Id);
   end Is_Predicate_Function;

   function Is_Predicate_Function_M (Id : E) return B is
   begin
      pragma Assert (Ekind_In (Id, E_Function, E_Procedure));
      return Flag256 (Id);
   end Is_Predicate_Function_M;

   function Is_Preelaborated (Id : E) return B is
   begin
      return Flag59 (Id);
   end Is_Preelaborated;

   function Is_Primitive (Id : E) return B is
   begin
      pragma Assert
        (Is_Overloadable (Id)
           or else Ekind_In (Id, E_Generic_Function, E_Generic_Procedure));
      return Flag218 (Id);
   end Is_Primitive;

   function Is_Primitive_Wrapper (Id : E) return B is
   begin
      pragma Assert (Ekind_In (Id, E_Function, E_Procedure));
      return Flag195 (Id);
   end Is_Primitive_Wrapper;

   function Is_Private_Composite (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag107 (Id);
   end Is_Private_Composite;

   function Is_Private_Descendant (Id : E) return B is
   begin
      return Flag53 (Id);
   end Is_Private_Descendant;

   function Is_Private_Primitive (Id : E) return B is
   begin
      pragma Assert (Ekind_In (Id, E_Function, E_Procedure));
      return Flag245 (Id);
   end Is_Private_Primitive;

   function Is_Processed_Transient (Id : E) return B is
   begin
      pragma Assert (Ekind_In (Id, E_Constant, E_Loop_Parameter, E_Variable));
      return Flag252 (Id);
   end Is_Processed_Transient;

   function Is_Public (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Flag10 (Id);
   end Is_Public;

   function Is_Pure (Id : E) return B is
   begin
      return Flag44 (Id);
   end Is_Pure;

   function Is_Pure_Unit_Access_Type (Id : E) return B is
   begin
      pragma Assert (Is_Access_Type (Id));
      return Flag189 (Id);
   end Is_Pure_Unit_Access_Type;

   function Is_RACW_Stub_Type (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag244 (Id);
   end Is_RACW_Stub_Type;

   function Is_Raised (Id : E) return B is
   begin
      pragma Assert (Ekind (Id) = E_Exception);
      return Flag224 (Id);
   end Is_Raised;

   function Is_Remote_Call_Interface (Id : E) return B is
   begin
      return Flag62 (Id);
   end Is_Remote_Call_Interface;

   function Is_Remote_Types (Id : E) return B is
   begin
      return Flag61 (Id);
   end Is_Remote_Types;

   function Is_Renaming_Of_Object (Id : E) return B is
   begin
      return Flag112 (Id);
   end Is_Renaming_Of_Object;

   function Is_Return_Object (Id : E) return B is
   begin
      return Flag209 (Id);
   end Is_Return_Object;

   function Is_Safe_To_Reevaluate (Id : E) return B is
   begin
      return Flag249 (Id);
   end Is_Safe_To_Reevaluate;

   function Is_Shared_Passive (Id : E) return B is
   begin
      return Flag60 (Id);
   end Is_Shared_Passive;

   function Is_Static_Type (Id : E) return B is
   begin
      return Flag281 (Id);
   end Is_Static_Type;

   function Is_Statically_Allocated (Id : E) return B is
   begin
      return Flag28 (Id);
   end Is_Statically_Allocated;

   function Is_Tag (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Flag78 (Id);
   end Is_Tag;

   function Is_Tagged_Type (Id : E) return B is
   begin
      return Flag55 (Id);
   end Is_Tagged_Type;

   function Is_Thunk (Id : E) return B is
   begin
      return Flag225 (Id);
   end Is_Thunk;

   function Is_Trivial_Subprogram (Id : E) return B is
   begin
      return Flag235 (Id);
   end Is_Trivial_Subprogram;

   function Is_True_Constant (Id : E) return B is
   begin
      return Flag163 (Id);
   end Is_True_Constant;

   function Is_Unchecked_Union (Id : E) return B is
   begin
      return Flag117 (Implementation_Base_Type (Id));
   end Is_Unchecked_Union;

   function Is_Underlying_Record_View (Id : E) return B is
   begin
      return Flag246 (Id);
   end Is_Underlying_Record_View;

   function Is_Unimplemented (Id : E) return B is
   begin
      return Flag284 (Id);
   end Is_Unimplemented;

   function Is_Unsigned_Type (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag144 (Id);
   end Is_Unsigned_Type;

   function Is_Uplevel_Referenced_Entity (Id : E) return B is
   begin
      return Flag283 (Id);
   end Is_Uplevel_Referenced_Entity;

   function Is_Valued_Procedure (Id : E) return B is
   begin
      pragma Assert (Ekind (Id) = E_Procedure);
      return Flag127 (Id);
   end Is_Valued_Procedure;

   function Is_Visible_Formal (Id : E) return B is
   begin
      return Flag206 (Id);
   end Is_Visible_Formal;

   function Is_Visible_Lib_Unit (Id : E) return B is
   begin
      return Flag116 (Id);
   end Is_Visible_Lib_Unit;

   function Is_Volatile (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);

      if Is_Type (Id) then
         return Flag16 (Base_Type (Id));
      else
         return Flag16 (Id);
      end if;
   end Is_Volatile;

   function Is_Volatile_Full_Access (Id : E) return B is
   begin
      return Flag285 (Id);
   end Is_Volatile_Full_Access;

   function Itype_Printed (Id : E) return B is
   begin
      pragma Assert (Is_Itype (Id));
      return Flag202 (Id);
   end Itype_Printed;

   function Kill_Elaboration_Checks (Id : E) return B is
   begin
      return Flag32 (Id);
   end Kill_Elaboration_Checks;

   function Kill_Range_Checks (Id : E) return B is
   begin
      return Flag33 (Id);
   end Kill_Range_Checks;

   function Known_To_Have_Preelab_Init (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag207 (Id);
   end Known_To_Have_Preelab_Init;

   function Last_Aggregate_Assignment (Id : E) return N is
   begin
      pragma Assert (Ekind_In (Id, E_Constant, E_Variable));
      return Node30 (Id);
   end Last_Aggregate_Assignment;

   function Last_Assignment (Id : E) return N is
   begin
      pragma Assert (Is_Assignable (Id));
      return Node26 (Id);
   end Last_Assignment;

   function Last_Entity (Id : E) return E is
   begin
      return Node20 (Id);
   end Last_Entity;

   function Limited_View (Id : E) return E is
   begin
      pragma Assert (Ekind (Id) = E_Package);
      return Node23 (Id);
   end Limited_View;

   function Linker_Section_Pragma (Id : E) return N is
   begin
      pragma Assert
        (Is_Type (Id) or else Is_Object (Id) or else Is_Subprogram (Id));
      return Node33 (Id);
   end Linker_Section_Pragma;

   function Lit_Indexes (Id : E) return E is
   begin
      pragma Assert (Is_Enumeration_Type (Id));
      return Node18 (Id);
   end Lit_Indexes;

   function Lit_Strings (Id : E) return E is
   begin
      pragma Assert (Is_Enumeration_Type (Id));
      return Node16 (Id);
   end Lit_Strings;

   function Low_Bound_Tested (Id : E) return B is
   begin
      return Flag205 (Id);
   end Low_Bound_Tested;

   function Machine_Radix_10 (Id : E) return B is
   begin
      pragma Assert (Is_Decimal_Fixed_Point_Type (Id));
      return Flag84 (Id);
   end Machine_Radix_10;

   function Master_Id (Id : E) return E is
   begin
      pragma Assert (Is_Access_Type (Id));
      return Node17 (Id);
   end Master_Id;

   function Materialize_Entity (Id : E) return B is
   begin
      return Flag168 (Id);
   end Materialize_Entity;

   function May_Inherit_Delayed_Rep_Aspects (Id : E) return B is
   begin
      return Flag262 (Id);
   end May_Inherit_Delayed_Rep_Aspects;

   function Mechanism (Id : E) return M is
   begin
      pragma Assert (Ekind (Id) = E_Function or else Is_Formal (Id));
      return UI_To_Int (Uint8 (Id));
   end Mechanism;

   function Modulus (Id : E) return Uint is
   begin
      pragma Assert (Is_Modular_Integer_Type (Id));
      return Uint17 (Base_Type (Id));
   end Modulus;

   function Must_Be_On_Byte_Boundary (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag183 (Id);
   end Must_Be_On_Byte_Boundary;

   function Must_Have_Preelab_Init (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag208 (Id);
   end Must_Have_Preelab_Init;

   function Needs_Debug_Info (Id : E) return B is
   begin
      return Flag147 (Id);
   end Needs_Debug_Info;

   function Needs_No_Actuals (Id : E) return B is
   begin
      pragma Assert
        (Is_Overloadable (Id)
           or else Ekind_In (Id, E_Subprogram_Type, E_Entry_Family));
      return Flag22 (Id);
   end Needs_No_Actuals;

   function Never_Set_In_Source (Id : E) return B is
   begin
      return Flag115 (Id);
   end Never_Set_In_Source;

   function Next_Inlined_Subprogram (Id : E) return E is
   begin
      return Node12 (Id);
   end Next_Inlined_Subprogram;

   function No_Dynamic_Predicate_On_Actual (Id : E) return Boolean is
   begin
      pragma Assert (Is_Discrete_Type (Id));
      return Flag276 (Id);
   end No_Dynamic_Predicate_On_Actual;

   function No_Pool_Assigned (Id : E) return B is
   begin
      pragma Assert (Is_Access_Type (Id));
      return Flag131 (Root_Type (Id));
   end No_Pool_Assigned;

   function No_Predicate_On_Actual (Id : E) return Boolean is
   begin
      pragma Assert (Is_Discrete_Type (Id));
      return Flag275 (Id);
   end No_Predicate_On_Actual;

   function No_Return (Id : E) return B is
   begin
      return Flag113 (Id);
   end No_Return;

   function No_Strict_Aliasing (Id : E) return B is
   begin
      pragma Assert (Is_Access_Type (Id));
      return Flag136 (Base_Type (Id));
   end No_Strict_Aliasing;

   function No_Tagged_Streams_Pragma (Id : E) return N is
   begin
      pragma Assert (Is_Tagged_Type (Id));
      return Node32 (Id);
   end No_Tagged_Streams_Pragma;

   function Non_Binary_Modulus (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag58 (Base_Type (Id));
   end Non_Binary_Modulus;

   function Non_Limited_View (Id : E) return E is
   begin
      pragma Assert
        (Ekind (Id) in Incomplete_Kind
           or else
         Ekind (Id) in Class_Wide_Kind
           or else
         Ekind (Id) = E_Abstract_State);
      return Node19 (Id);
   end Non_Limited_View;

   function Nonzero_Is_True (Id : E) return B is
   begin
      pragma Assert (Root_Type (Id) = Standard_Boolean);
      return Flag162 (Base_Type (Id));
   end Nonzero_Is_True;

   function Normalized_First_Bit (Id : E) return U is
   begin
      pragma Assert (Ekind_In (Id, E_Component, E_Discriminant));
      return Uint8 (Id);
   end Normalized_First_Bit;

   function Normalized_Position (Id : E) return U is
   begin
      pragma Assert (Ekind_In (Id, E_Component, E_Discriminant));
      return Uint14 (Id);
   end Normalized_Position;

   function Normalized_Position_Max (Id : E) return U is
   begin
      pragma Assert (Ekind_In (Id, E_Component, E_Discriminant));
      return Uint10 (Id);
   end Normalized_Position_Max;

   function OK_To_Rename (Id : E) return B is
   begin
      pragma Assert (Ekind (Id) = E_Variable);
      return Flag247 (Id);
   end OK_To_Rename;

   function OK_To_Reorder_Components (Id : E) return B is
   begin
      pragma Assert (Is_Record_Type (Id));
      return Flag239 (Base_Type (Id));
   end OK_To_Reorder_Components;

   function Optimize_Alignment_Space (Id : E) return B is
   begin
      pragma Assert
        (Is_Type (Id) or else Ekind_In (Id, E_Constant, E_Variable));
      return Flag241 (Id);
   end Optimize_Alignment_Space;

   function Optimize_Alignment_Time (Id : E) return B is
   begin
      pragma Assert
        (Is_Type (Id) or else Ekind_In (Id, E_Constant, E_Variable));
      return Flag242 (Id);
   end Optimize_Alignment_Time;

   function Original_Access_Type (Id : E) return E is
   begin
      pragma Assert (Ekind (Id) = E_Access_Subprogram_Type);
      return Node28 (Id);
   end Original_Access_Type;

   function Original_Array_Type (Id : E) return E is
   begin
      pragma Assert (Is_Array_Type (Id) or else Is_Modular_Integer_Type (Id));
      return Node21 (Id);
   end Original_Array_Type;

   function Original_Record_Component (Id : E) return E is
   begin
      pragma Assert (Ekind_In (Id, E_Void, E_Component, E_Discriminant));
      return Node22 (Id);
   end Original_Record_Component;

   function Overlays_Constant (Id : E) return B is
   begin
      return Flag243 (Id);
   end Overlays_Constant;

   function Overridden_Operation (Id : E) return E is
   begin
      pragma Assert (Is_Subprogram (Id) or else Is_Generic_Subprogram (Id));
      return Node26 (Id);
   end Overridden_Operation;

   function Package_Instantiation (Id : E) return N is
   begin
      pragma Assert (Ekind_In (Id, E_Package, E_Generic_Package));
      return Node26 (Id);
   end Package_Instantiation;

   function Packed_Array_Impl_Type (Id : E) return E is
   begin
      pragma Assert (Is_Array_Type (Id));
      return Node23 (Id);
   end Packed_Array_Impl_Type;

   function Parent_Subtype (Id : E) return E is
   begin
      pragma Assert (Is_Record_Type (Id));
      return Node19 (Base_Type (Id));
   end Parent_Subtype;

   function Part_Of_Constituents (Id : E) return L is
   begin
      pragma Assert (Ekind_In (Id, E_Abstract_State, E_Variable));
      return Elist10 (Id);
   end Part_Of_Constituents;

   function Part_Of_References (Id : E) return L is
   begin
      pragma Assert (Ekind (Id) = E_Variable);
      return Elist11 (Id);
   end Part_Of_References;

   function Partial_View_Has_Unknown_Discr (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag280 (Id);
   end Partial_View_Has_Unknown_Discr;

   function Pending_Access_Types (Id : E) return L is
   begin
      pragma Assert (Is_Type (Id));
      return Elist15 (Id);
   end Pending_Access_Types;

   function Postconditions_Proc (Id : E) return E is
   begin
      pragma Assert (Ekind_In (Id, E_Entry,
                                   E_Entry_Family,
                                   E_Function,
                                   E_Procedure));
      return Node14 (Id);
   end Postconditions_Proc;

   function Prival (Id : E) return E is
   begin
      pragma Assert (Is_Protected_Component (Id));
      return Node17 (Id);
   end Prival;

   function Prival_Link (Id : E) return E is
   begin
      pragma Assert (Ekind_In (Id, E_Constant, E_Variable));
      return Node20 (Id);
   end Prival_Link;

   function Private_Dependents (Id : E) return L is
   begin
      pragma Assert (Is_Incomplete_Or_Private_Type (Id));
      return Elist18 (Id);
   end Private_Dependents;

   function Private_View (Id : E) return N is
   begin
      pragma Assert (Is_Private_Type (Id));
      return Node22 (Id);
   end Private_View;

   function Protected_Body_Subprogram (Id : E) return E is
   begin
      pragma Assert (Is_Subprogram (Id) or else Is_Entry (Id));
      return Node11 (Id);
   end Protected_Body_Subprogram;

   function Protected_Formal (Id : E) return E is
   begin
      pragma Assert (Is_Formal (Id));
      return Node22 (Id);
   end Protected_Formal;

   function Protection_Object (Id : E) return E is
   begin
      pragma Assert
        (Ekind_In (Id, E_Entry, E_Entry_Family, E_Function, E_Procedure));
      return Node23 (Id);
   end Protection_Object;

   function Reachable (Id : E) return B is
   begin
      return Flag49 (Id);
   end Reachable;

   function Referenced (Id : E) return B is
   begin
      return Flag156 (Id);
   end Referenced;

   function Referenced_As_LHS (Id : E) return B is
   begin
      return Flag36 (Id);
   end Referenced_As_LHS;

   function Referenced_As_Out_Parameter (Id : E) return B is
   begin
      return Flag227 (Id);
   end Referenced_As_Out_Parameter;

   function Refinement_Constituents (Id : E) return L is
   begin
      pragma Assert (Ekind (Id) = E_Abstract_State);
      return Elist8 (Id);
   end Refinement_Constituents;

   function Register_Exception_Call (Id : E) return N is
   begin
      pragma Assert (Ekind (Id) = E_Exception);
      return Node20 (Id);
   end Register_Exception_Call;

   function Related_Array_Object (Id : E) return E is
   begin
      pragma Assert (Is_Array_Type (Id));
      return Node25 (Id);
   end Related_Array_Object;

   function Related_Expression (Id : E) return N is
   begin
      pragma Assert (Ekind (Id) in Type_Kind
                       or else Ekind_In (Id, E_Constant, E_Variable));
      return Node24 (Id);
   end Related_Expression;

   function Related_Instance (Id : E) return E is
   begin
      pragma Assert (Ekind_In (Id, E_Package, E_Package_Body));
      return Node15 (Id);
   end Related_Instance;

   function Related_Type (Id : E) return E is
   begin
      pragma Assert (Ekind_In (Id, E_Component, E_Constant, E_Variable));
      return Node27 (Id);
   end Related_Type;

   function Relative_Deadline_Variable (Id : E) return E is
   begin
      pragma Assert (Is_Task_Type (Id));
      return Node28 (Implementation_Base_Type (Id));
   end Relative_Deadline_Variable;

   function Renamed_Entity (Id : E) return N is
   begin
      return Node18 (Id);
   end Renamed_Entity;

   function Renamed_In_Spec (Id : E) return B is
   begin
      pragma Assert (Ekind (Id) = E_Package);
      return Flag231 (Id);
   end Renamed_In_Spec;

   function Renamed_Object (Id : E) return N is
   begin
      return Node18 (Id);
   end Renamed_Object;

   function Renaming_Map (Id : E) return U is
   begin
      return Uint9 (Id);
   end Renaming_Map;

   function Requires_Overriding (Id : E) return B is
   begin
      pragma Assert (Is_Overloadable (Id));
      return Flag213 (Id);
   end Requires_Overriding;

   function Return_Present (Id : E) return B is
   begin
      return Flag54 (Id);
   end Return_Present;

   function Return_Applies_To (Id : E) return N is
   begin
      return Node8 (Id);
   end Return_Applies_To;

   function Returns_By_Ref (Id : E) return B is
   begin
      return Flag90 (Id);
   end Returns_By_Ref;

   function Returns_Limited_View (Id : E) return B is
   begin
      pragma Assert (Ekind (Id) = E_Function);
      return Flag134 (Id);
   end Returns_Limited_View;

   function Reverse_Bit_Order (Id : E) return B is
   begin
      pragma Assert (Is_Record_Type (Id));
      return Flag164 (Base_Type (Id));
   end Reverse_Bit_Order;

   function Reverse_Storage_Order (Id : E) return B is
   begin
      pragma Assert (Is_Record_Type (Id) or else Is_Array_Type (Id));
      return Flag93 (Base_Type (Id));
   end Reverse_Storage_Order;

   function Rewritten_For_C (Id : E) return B is
   begin
      pragma Assert (Ekind (Id) = E_Function);
      return Flag287 (Id);
   end Rewritten_For_C;

   function RM_Size (Id : E) return U is
   begin
      pragma Assert (Is_Type (Id));
      return Uint13 (Id);
   end RM_Size;

   function Scalar_Range (Id : E) return N is
   begin
      return Node20 (Id);
   end Scalar_Range;

   function Scale_Value (Id : E) return U is
   begin
      return Uint16 (Id);
   end Scale_Value;

   function Scope_Depth_Value (Id : E) return U is
   begin
      return Uint22 (Id);
   end Scope_Depth_Value;

   function Sec_Stack_Needed_For_Return (Id : E) return B is
   begin
      return Flag167 (Id);
   end Sec_Stack_Needed_For_Return;

   function Shadow_Entities (Id : E) return S is
   begin
      pragma Assert (Ekind_In (Id, E_Package, E_Generic_Package));
      return List14 (Id);
   end Shadow_Entities;

   function Shared_Var_Procs_Instance (Id : E) return E is
   begin
      pragma Assert (Ekind (Id) = E_Variable);
      return Node22 (Id);
   end Shared_Var_Procs_Instance;

   function Size_Check_Code (Id : E) return N is
   begin
      pragma Assert (Ekind_In (Id, E_Constant, E_Variable));
      return Node19 (Id);
   end Size_Check_Code;

   function Size_Depends_On_Discriminant (Id : E) return B is
   begin
      return Flag177 (Id);
   end Size_Depends_On_Discriminant;

   function Size_Known_At_Compile_Time (Id : E) return B is
   begin
      return Flag92 (Id);
   end Size_Known_At_Compile_Time;

   function Small_Value (Id : E) return R is
   begin
      pragma Assert (Is_Fixed_Point_Type (Id));
      return Ureal21 (Id);
   end Small_Value;

   function SPARK_Aux_Pragma (Id : E) return N is
   begin
      pragma Assert
        (Ekind_In (Id, E_Protected_Type,   --  concurrent variants
                       E_Task_Type)
           or else
         Ekind_In (Id, E_Generic_Package,  --  package variants
                       E_Package,
                       E_Package_Body));
      return Node41 (Id);
   end SPARK_Aux_Pragma;

   function SPARK_Aux_Pragma_Inherited (Id : E) return B is
   begin
      pragma Assert
        (Ekind_In (Id, E_Protected_Type,   --  concurrent variants
                       E_Task_Type)
           or else
         Ekind_In (Id, E_Generic_Package,  --  package variants
                       E_Package,
                       E_Package_Body));
      return Flag266 (Id);
   end SPARK_Aux_Pragma_Inherited;

   function SPARK_Pragma (Id : E) return N is
   begin
      pragma Assert
        (Ekind_In (Id, E_Protected_Body,   --  concurrent variants
                       E_Protected_Type,
                       E_Task_Body,
                       E_Task_Type)
          or else
         Ekind_In (Id, E_Entry,            --  overloadable variants
                       E_Entry_Family,
                       E_Function,
                       E_Generic_Function,
                       E_Generic_Procedure,
                       E_Operator,
                       E_Procedure,
                       E_Subprogram_Body)
           or else
         Ekind_In (Id, E_Generic_Package,  --  package variants
                       E_Package,
                       E_Package_Body)
           or else
         Ekind (Id) = E_Variable);         --  variable
      return Node40 (Id);
   end SPARK_Pragma;

   function SPARK_Pragma_Inherited (Id : E) return B is
   begin
      pragma Assert
        (Ekind_In (Id, E_Protected_Body,   --  concurrent variants
                       E_Protected_Type,
                       E_Task_Body,
                       E_Task_Type)
           or else
         Ekind_In (Id, E_Entry,            --  overloadable variants
                       E_Entry_Family,
                       E_Function,
                       E_Generic_Function,
                       E_Generic_Procedure,
                       E_Operator,
                       E_Procedure,
                       E_Subprogram_Body)
           or else
         Ekind_In (Id, E_Generic_Package,  --  package variants
                       E_Package,
                       E_Package_Body)
           or else
         Ekind (Id) = E_Variable);         --  variable
      return Flag265 (Id);
   end SPARK_Pragma_Inherited;

   function Spec_Entity (Id : E) return E is
   begin
      pragma Assert (Ekind (Id) = E_Package_Body or else Is_Formal (Id));
      return Node19 (Id);
   end Spec_Entity;

   function SSO_Set_High_By_Default (Id : E) return B is
   begin
      pragma Assert (Is_Record_Type (Id) or else Is_Array_Type (Id));
      return Flag273 (Base_Type (Id));
   end SSO_Set_High_By_Default;

   function SSO_Set_Low_By_Default (Id : E) return B is
   begin
      pragma Assert (Is_Record_Type (Id) or else Is_Array_Type (Id));
      return Flag272 (Base_Type (Id));
   end SSO_Set_Low_By_Default;

   function Static_Discrete_Predicate (Id : E) return S is
   begin
      pragma Assert (Is_Discrete_Type (Id));
      return List25 (Id);
   end Static_Discrete_Predicate;

   function Static_Real_Or_String_Predicate (Id : E) return N is
   begin
      pragma Assert (Is_Real_Type (Id) or else Is_String_Type (Id));
      return Node25 (Id);
   end Static_Real_Or_String_Predicate;

   function Status_Flag_Or_Transient_Decl (Id : E) return N is
   begin
      pragma Assert (Ekind_In (Id, E_Constant, E_Variable));
      return Node15 (Id);
   end Status_Flag_Or_Transient_Decl;

   function Storage_Size_Variable (Id : E) return E is
   begin
      pragma Assert (Is_Access_Type (Id) or else Is_Task_Type (Id));
      return Node26 (Implementation_Base_Type (Id));
   end Storage_Size_Variable;

   function Static_Elaboration_Desired (Id : E) return B is
   begin
      pragma Assert (Ekind (Id) = E_Package);
      return Flag77 (Id);
   end Static_Elaboration_Desired;

   function Static_Initialization (Id : E) return N is
   begin
      pragma Assert
        (Ekind (Id) = E_Procedure and then not Is_Dispatching_Operation (Id));
      return Node30 (Id);
   end Static_Initialization;

   function Stored_Constraint (Id : E) return L is
   begin
      pragma Assert
        (Is_Composite_Type (Id) and then not Is_Array_Type (Id));
      return Elist23 (Id);
   end Stored_Constraint;

   function Stores_Attribute_Old_Prefix (Id : E) return B is
   begin
      return Flag270 (Id);
   end Stores_Attribute_Old_Prefix;

   function Strict_Alignment (Id : E) return B is
   begin
      return Flag145 (Implementation_Base_Type (Id));
   end Strict_Alignment;

   function String_Literal_Length (Id : E) return U is
   begin
      return Uint16 (Id);
   end String_Literal_Length;

   function String_Literal_Low_Bound (Id : E) return N is
   begin
      return Node18 (Id);
   end String_Literal_Low_Bound;

   function Subprograms_For_Type (Id : E) return E is
   begin
      pragma Assert (Is_Type (Id) or else Is_Subprogram (Id));
      return Node29 (Id);
   end Subprograms_For_Type;

   function Subps_Index (Id : E) return U is
   begin
      pragma Assert (Is_Subprogram (Id));
      return Uint24 (Id);
   end Subps_Index;

   function Suppress_Elaboration_Warnings (Id : E) return B is
   begin
      return Flag148 (Id);
   end Suppress_Elaboration_Warnings;

   function Suppress_Initialization (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id) or else Ekind (Id) = E_Variable);
      return Flag105 (Id);
   end Suppress_Initialization;

   function Suppress_Style_Checks (Id : E) return B is
   begin
      return Flag165 (Id);
   end Suppress_Style_Checks;

   function Suppress_Value_Tracking_On_Call (Id : E) return B is
   begin
      return Flag217 (Id);
   end Suppress_Value_Tracking_On_Call;

   function Task_Body_Procedure (Id : E) return N is
   begin
      pragma Assert (Ekind (Id) in Task_Kind);
      return Node25 (Id);
   end Task_Body_Procedure;

   function Thunk_Entity (Id : E) return E is
   begin
      pragma Assert (Ekind_In (Id, E_Function, E_Procedure)
                      and then Is_Thunk (Id));
      return Node31 (Id);
   end Thunk_Entity;

   function Treat_As_Volatile (Id : E) return B is
   begin
      return Flag41 (Id);
   end Treat_As_Volatile;

   function Underlying_Full_View (Id : E) return E is
   begin
      pragma Assert (Ekind (Id) in Private_Kind);
      return Node19 (Id);
   end Underlying_Full_View;

   function Underlying_Record_View (Id : E) return E is
   begin
      return Node28 (Id);
   end Underlying_Record_View;

   function Universal_Aliasing (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag216 (Implementation_Base_Type (Id));
   end Universal_Aliasing;

   function Unset_Reference (Id : E) return N is
   begin
      return Node16 (Id);
   end Unset_Reference;

   function Used_As_Generic_Actual (Id : E) return B is
   begin
      return Flag222 (Id);
   end Used_As_Generic_Actual;

   function Uses_Lock_Free (Id : E) return B is
   begin
      pragma Assert (Is_Protected_Type (Id));
      return Flag188 (Id);
   end Uses_Lock_Free;

   function Uses_Sec_Stack (Id : E) return B is
   begin
      return Flag95 (Id);
   end Uses_Sec_Stack;

   function Warnings_Off (Id : E) return B is
   begin
      return Flag96 (Id);
   end Warnings_Off;

   function Warnings_Off_Used (Id : E) return B is
   begin
      return Flag236 (Id);
   end Warnings_Off_Used;

   function Warnings_Off_Used_Unmodified (Id : E) return B is
   begin
      return Flag237 (Id);
   end Warnings_Off_Used_Unmodified;

   function Warnings_Off_Used_Unreferenced (Id : E) return B is
   begin
      return Flag238 (Id);
   end Warnings_Off_Used_Unreferenced;

   function Wrapped_Entity (Id : E) return E is
   begin
      pragma Assert (Ekind_In (Id, E_Function, E_Procedure)
                       and then Is_Primitive_Wrapper (Id));
      return Node27 (Id);
   end Wrapped_Entity;

   function Was_Hidden (Id : E) return B is
   begin
      return Flag196 (Id);
   end Was_Hidden;

   ------------------------------
   -- Classification Functions --
   ------------------------------

   function Is_Access_Type                      (Id : E) return B is
   begin
      return Ekind (Id) in Access_Kind;
   end Is_Access_Type;

   function Is_Access_Protected_Subprogram_Type (Id : E) return B is
   begin
      return Ekind (Id) in Access_Protected_Kind;
   end Is_Access_Protected_Subprogram_Type;

   function Is_Access_Subprogram_Type           (Id : E) return B is
   begin
      return Ekind (Id) in Access_Subprogram_Kind;
   end Is_Access_Subprogram_Type;

   function Is_Aggregate_Type                   (Id : E) return B is
   begin
      return Ekind (Id) in Aggregate_Kind;
   end Is_Aggregate_Type;

   function Is_Array_Type                       (Id : E) return B is
   begin
      return Ekind (Id) in Array_Kind;
   end Is_Array_Type;

   function Is_Assignable                       (Id : E) return B is
   begin
      return Ekind (Id) in Assignable_Kind;
   end Is_Assignable;

   function Is_Class_Wide_Type                  (Id : E) return B is
   begin
      return Ekind (Id) in Class_Wide_Kind;
   end Is_Class_Wide_Type;

   function Is_Composite_Type                   (Id : E) return B is
   begin
      return Ekind (Id) in Composite_Kind;
   end Is_Composite_Type;

   function Is_Concurrent_Body                  (Id : E) return B is
   begin
      return Ekind (Id) in Concurrent_Body_Kind;
   end Is_Concurrent_Body;

   function Is_Concurrent_Record_Type           (Id : E) return B is
   begin
      return Flag20 (Id);
   end Is_Concurrent_Record_Type;

   function Is_Concurrent_Type                  (Id : E) return B is
   begin
      return Ekind (Id) in Concurrent_Kind;
   end Is_Concurrent_Type;

   function Is_Decimal_Fixed_Point_Type         (Id : E) return B is
   begin
      return Ekind (Id) in Decimal_Fixed_Point_Kind;
   end Is_Decimal_Fixed_Point_Type;

   function Is_Digits_Type                      (Id : E) return B is
   begin
      return Ekind (Id) in Digits_Kind;
   end Is_Digits_Type;

   function Is_Discrete_Or_Fixed_Point_Type     (Id : E) return B is
   begin
      return Ekind (Id) in Discrete_Or_Fixed_Point_Kind;
   end Is_Discrete_Or_Fixed_Point_Type;

   function Is_Discrete_Type                    (Id : E) return B is
   begin
      return Ekind (Id) in Discrete_Kind;
   end Is_Discrete_Type;

   function Is_Elementary_Type                  (Id : E) return B is
   begin
      return Ekind (Id) in Elementary_Kind;
   end Is_Elementary_Type;

   function Is_Entry                            (Id : E) return B is
   begin
      return Ekind (Id) in Entry_Kind;
   end Is_Entry;

   function Is_Enumeration_Type                 (Id : E) return B is
   begin
      return Ekind (Id) in Enumeration_Kind;
   end Is_Enumeration_Type;

   function Is_Fixed_Point_Type                 (Id : E) return B is
   begin
      return Ekind (Id) in Fixed_Point_Kind;
   end Is_Fixed_Point_Type;

   function Is_Floating_Point_Type              (Id : E) return B is
   begin
      return Ekind (Id) in Float_Kind;
   end Is_Floating_Point_Type;

   function Is_Formal                           (Id : E) return B is
   begin
      return Ekind (Id) in Formal_Kind;
   end Is_Formal;

   function Is_Formal_Object                    (Id : E) return B is
   begin
      return Ekind (Id) in Formal_Object_Kind;
   end Is_Formal_Object;

   function Is_Generic_Subprogram               (Id : E) return B is
   begin
      return Ekind (Id) in Generic_Subprogram_Kind;
   end Is_Generic_Subprogram;

   function Is_Generic_Unit                     (Id : E) return B is
   begin
      return Ekind (Id) in Generic_Unit_Kind;
   end Is_Generic_Unit;

   function Is_Ghost_Entity (Id : Entity_Id) return Boolean is
   begin
      return Is_Checked_Ghost_Entity (Id) or else Is_Ignored_Ghost_Entity (Id);
   end Is_Ghost_Entity;

   function Is_Incomplete_Or_Private_Type       (Id : E) return B is
   begin
      return Ekind (Id) in Incomplete_Or_Private_Kind;
   end Is_Incomplete_Or_Private_Type;

   function Is_Incomplete_Type                  (Id : E) return B is
   begin
      return Ekind (Id) in Incomplete_Kind;
   end Is_Incomplete_Type;

   function Is_Integer_Type                     (Id : E) return B is
   begin
      return Ekind (Id) in Integer_Kind;
   end Is_Integer_Type;

   function Is_Modular_Integer_Type             (Id : E) return B is
   begin
      return Ekind (Id) in Modular_Integer_Kind;
   end Is_Modular_Integer_Type;

   function Is_Named_Number                     (Id : E) return B is
   begin
      return Ekind (Id) in Named_Kind;
   end Is_Named_Number;

   function Is_Numeric_Type                     (Id : E) return B is
   begin
      return Ekind (Id) in Numeric_Kind;
   end Is_Numeric_Type;

   function Is_Object                           (Id : E) return B is
   begin
      return Ekind (Id) in Object_Kind;
   end Is_Object;

   function Is_Ordinary_Fixed_Point_Type        (Id : E) return B is
   begin
      return Ekind (Id) in Ordinary_Fixed_Point_Kind;
   end Is_Ordinary_Fixed_Point_Type;

   function Is_Overloadable                     (Id : E) return B is
   begin
      return Ekind (Id) in Overloadable_Kind;
   end Is_Overloadable;

   function Is_Private_Type                     (Id : E) return B is
   begin
      return Ekind (Id) in Private_Kind;
   end Is_Private_Type;

   function Is_Protected_Type                   (Id : E) return B is
   begin
      return Ekind (Id) in Protected_Kind;
   end Is_Protected_Type;

   function Is_Real_Type                        (Id : E) return B is
   begin
      return Ekind (Id) in Real_Kind;
   end Is_Real_Type;

   function Is_Record_Type                      (Id : E) return B is
   begin
      return Ekind (Id) in Record_Kind;
   end Is_Record_Type;

   function Is_Scalar_Type                      (Id : E) return B is
   begin
      return Ekind (Id) in Scalar_Kind;
   end Is_Scalar_Type;

   function Is_Signed_Integer_Type              (Id : E) return B is
   begin
      return Ekind (Id) in Signed_Integer_Kind;
   end Is_Signed_Integer_Type;

   function Is_Subprogram                       (Id : E) return B is
   begin
      return Ekind (Id) in Subprogram_Kind;
   end Is_Subprogram;

   function Is_Subprogram_Or_Generic_Subprogram (Id : E) return B is
   begin
      return Ekind (Id) in Subprogram_Kind
               or else
             Ekind (Id) in Generic_Subprogram_Kind;
   end Is_Subprogram_Or_Generic_Subprogram;

   function Is_Task_Type                        (Id : E) return B is
   begin
      return Ekind (Id) in Task_Kind;
   end Is_Task_Type;

   function Is_Type                             (Id : E) return B is
   begin
      return Ekind (Id) in Type_Kind;
   end Is_Type;

   ------------------------------
   -- Attribute Set Procedures --
   ------------------------------

   --  Note: in many of these set procedures an "obvious" assertion is missing.
   --  The reason for this is that in many cases, a field is set before the
   --  Ekind field is set, so that the field is set when Ekind = E_Void. It
   --  it is possible to add assertions that specifically include the E_Void
   --  possibility, but in some cases, we just omit the assertions.

   procedure Set_Abstract_States (Id : E; V : L) is
   begin
      pragma Assert (Ekind_In (Id, E_Generic_Package, E_Package));
      Set_Elist25 (Id, V);
   end Set_Abstract_States;

   procedure Set_Accept_Address (Id : E; V : L) is
   begin
      Set_Elist21 (Id, V);
   end Set_Accept_Address;

   procedure Set_Access_Disp_Table (Id : E; V : L) is
   begin
      pragma Assert (Ekind (Id) = E_Record_Type
        and then Id = Implementation_Base_Type (Id));
      pragma Assert (V = No_Elist or else Is_Tagged_Type (Id));
      Set_Elist16 (Id, V);
   end Set_Access_Disp_Table;

   procedure Set_Anonymous_Master (Id : E; V : E) is
   begin
      pragma Assert (Ekind_In (Id, E_Function,
                                   E_Package,
                                   E_Package_Body,
                                   E_Procedure,
                                   E_Subprogram_Body));
      Set_Node36 (Id, V);
   end Set_Anonymous_Master;

   procedure Set_Anonymous_Object (Id : E; V : E) is
   begin
      pragma Assert (Ekind_In (Id, E_Protected_Type, E_Task_Type));
      Set_Node30 (Id, V);
   end Set_Anonymous_Object;

   procedure Set_Associated_Entity (Id : E; V : E) is
   begin
      Set_Node37 (Id, V);
   end Set_Associated_Entity;

   procedure Set_Associated_Formal_Package (Id : E; V : E) is
   begin
      Set_Node12 (Id, V);
   end Set_Associated_Formal_Package;

   procedure Set_Associated_Node_For_Itype (Id : E; V : E) is
   begin
      Set_Node8 (Id, V);
   end Set_Associated_Node_For_Itype;

   procedure Set_Associated_Storage_Pool (Id : E; V : E) is
   begin
      pragma Assert (Is_Access_Type (Id) and then Is_Base_Type (Id));
      Set_Node22 (Id, V);
   end Set_Associated_Storage_Pool;

   procedure Set_Activation_Record_Component (Id : E; V : E) is
   begin
      pragma Assert (Ekind_In (Id, E_Constant,
                                   E_In_Parameter,
                                   E_In_Out_Parameter,
                                   E_Loop_Parameter,
                                   E_Out_Parameter,
                                   E_Variable));
      Set_Node31 (Id, V);
   end Set_Activation_Record_Component;

   procedure Set_Actual_Subtype (Id : E; V : E) is
   begin
      pragma Assert
        (Ekind_In (Id, E_Constant, E_Variable, E_Generic_In_Out_Parameter)
           or else Is_Formal (Id));
      Set_Node17 (Id, V);
   end Set_Actual_Subtype;

   procedure Set_Address_Taken (Id : E; V : B := True) is
   begin
      Set_Flag104 (Id, V);
   end Set_Address_Taken;

   procedure Set_Alias (Id : E; V : E) is
   begin
      pragma Assert
        (Is_Overloadable (Id) or else Ekind (Id) = E_Subprogram_Type);
      Set_Node18 (Id, V);
   end Set_Alias;

   procedure Set_Alignment (Id : E; V : U) is
   begin
      pragma Assert (Is_Type (Id)
                       or else Is_Formal (Id)
                       or else Ekind_In (Id, E_Loop_Parameter,
                                             E_Constant,
                                             E_Exception,
                                             E_Variable));
      Set_Uint14 (Id, V);
   end Set_Alignment;

   procedure Set_Barrier_Function (Id : E; V : N) is
   begin
      pragma Assert (Is_Entry (Id));
      Set_Node12 (Id, V);
   end Set_Barrier_Function;

   procedure Set_Block_Node (Id : E; V : N) is
   begin
      pragma Assert (Ekind (Id) = E_Block);
      Set_Node11 (Id, V);
   end Set_Block_Node;

   procedure Set_Body_Entity (Id : E; V : E) is
   begin
      pragma Assert (Ekind_In (Id, E_Package, E_Generic_Package));
      Set_Node19 (Id, V);
   end Set_Body_Entity;

   procedure Set_Body_Needed_For_SAL (Id : E; V : B := True) is
   begin
      pragma Assert
        (Ekind (Id) = E_Package
           or else Is_Subprogram (Id)
           or else Is_Generic_Unit (Id));
      Set_Flag40 (Id, V);
   end Set_Body_Needed_For_SAL;

   procedure Set_Body_References (Id : E; V : L) is
   begin
      pragma Assert (Ekind (Id) = E_Abstract_State);
      Set_Elist16 (Id, V);
   end Set_Body_References;

   procedure Set_BIP_Initialization_Call (Id : E; V : N) is
   begin
      pragma Assert (Ekind_In (Id, E_Constant, E_Variable));
      Set_Node29 (Id, V);
   end Set_BIP_Initialization_Call;

   procedure Set_C_Pass_By_Copy (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Record_Type (Id) and then Is_Base_Type (Id));
      Set_Flag125 (Id, V);
   end Set_C_Pass_By_Copy;

   procedure Set_Can_Never_Be_Null (Id : E; V : B := True) is
   begin
      Set_Flag38 (Id, V);
   end Set_Can_Never_Be_Null;

   procedure Set_Can_Use_Internal_Rep (Id : E; V : B := True) is
   begin
      pragma Assert
        (Is_Access_Subprogram_Type (Id) and then Is_Base_Type (Id));
      Set_Flag229 (Id, V);
   end Set_Can_Use_Internal_Rep;

   procedure Set_Checks_May_Be_Suppressed (Id : E; V : B := True) is
   begin
      Set_Flag31 (Id, V);
   end Set_Checks_May_Be_Suppressed;

   procedure Set_Class_Wide_Preconds (Id : E; V : S) is
   begin
      pragma Assert (Is_Subprogram (Id));
      Set_List38 (Id, V);
   end Set_Class_Wide_Preconds;

   procedure Set_Class_Wide_Postconds (Id : E; V : S) is
   begin
      pragma Assert (Is_Subprogram (Id));
      Set_List39 (Id, V);
   end Set_Class_Wide_Postconds;

   procedure Set_Class_Wide_Type (Id : E; V : E) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Node9 (Id, V);
   end Set_Class_Wide_Type;

   procedure Set_Cloned_Subtype (Id : E; V : E) is
   begin
      pragma Assert (Ekind_In (Id, E_Record_Subtype, E_Class_Wide_Subtype));
      Set_Node16 (Id, V);
   end Set_Cloned_Subtype;

   procedure Set_Component_Bit_Offset (Id : E; V : U) is
   begin
      pragma Assert (Ekind_In (Id, E_Component, E_Discriminant));
      Set_Uint11 (Id, V);
   end Set_Component_Bit_Offset;

   procedure Set_Component_Clause (Id : E; V : N) is
   begin
      pragma Assert (Ekind_In (Id, E_Component, E_Discriminant));
      Set_Node13 (Id, V);
   end Set_Component_Clause;

   procedure Set_Component_Size (Id : E; V : U) is
   begin
      pragma Assert (Is_Array_Type (Id) and then Is_Base_Type (Id));
      Set_Uint22 (Id, V);
   end Set_Component_Size;

   procedure Set_Component_Type (Id : E; V : E) is
   begin
      pragma Assert (Is_Array_Type (Id) and then Is_Base_Type (Id));
      Set_Node20 (Id, V);
   end Set_Component_Type;

   procedure Set_Contains_Ignored_Ghost_Code (Id : E; V : B := True) is
   begin
      pragma Assert
        (Ekind_In (Id, E_Block,
                       E_Function,
                       E_Generic_Function,
                       E_Generic_Package,
                       E_Generic_Procedure,
                       E_Package,
                       E_Package_Body,
                       E_Procedure,
                       E_Subprogram_Body));
      Set_Flag279 (Id, V);
   end Set_Contains_Ignored_Ghost_Code;

   procedure Set_Contract (Id : E; V : N) is
   begin
      pragma Assert
        (Ekind_In (Id, E_Protected_Type,   --  concurrent variants
                       E_Task_Body,
                       E_Task_Type)
           or else
         Ekind_In (Id, E_Constant,         --  object variants
                       E_Variable)
           or else
         Ekind_In (Id, E_Entry,            --  overloadable variants
                       E_Entry_Family,
                       E_Function,
                       E_Generic_Function,
                       E_Generic_Procedure,
                       E_Operator,
                       E_Procedure,
                       E_Subprogram_Body)
           or else
         Ekind_In (Id, E_Generic_Package,  --  package variants
                       E_Package,
                       E_Package_Body)
           or else
         Ekind (Id) = E_Void);             --  special purpose
      Set_Node34 (Id, V);
   end Set_Contract;

   procedure Set_Contract_Wrapper (Id : E; V : E) is
   begin
      pragma Assert (Ekind_In (Id, E_Entry, E_Entry_Family));
      Set_Node25 (Id, V);
   end Set_Contract_Wrapper;

   procedure Set_Corresponding_Concurrent_Type (Id : E; V : E) is
   begin
      pragma Assert
        (Ekind (Id) = E_Record_Type and then Is_Concurrent_Type (V));
      Set_Node18 (Id, V);
   end Set_Corresponding_Concurrent_Type;

   procedure Set_Corresponding_Discriminant (Id : E; V : E) is
   begin
      pragma Assert (Ekind (Id) = E_Discriminant);
      Set_Node19 (Id, V);
   end Set_Corresponding_Discriminant;

   procedure Set_Corresponding_Equality (Id : E; V : E) is
   begin
      pragma Assert
        (Ekind (Id) = E_Function
          and then not Comes_From_Source (Id)
          and then Chars (Id) = Name_Op_Ne);
      Set_Node30 (Id, V);
   end Set_Corresponding_Equality;

   procedure Set_Corresponding_Protected_Entry (Id : E; V : E) is
   begin
      pragma Assert (Ekind_In (Id, E_Void, E_Subprogram_Body));
      Set_Node18 (Id, V);
   end Set_Corresponding_Protected_Entry;

   procedure Set_Corresponding_Record_Type (Id : E; V : E) is
   begin
      pragma Assert (Is_Concurrent_Type (Id));
      Set_Node18 (Id, V);
   end Set_Corresponding_Record_Type;

   procedure Set_Corresponding_Remote_Type (Id : E; V : E) is
   begin
      Set_Node22 (Id, V);
   end Set_Corresponding_Remote_Type;

   procedure Set_Current_Use_Clause (Id : E; V : E) is
   begin
      pragma Assert (Ekind (Id) = E_Package or else Is_Type (Id));
      Set_Node27 (Id, V);
   end Set_Current_Use_Clause;

   procedure Set_Current_Value (Id : E; V : N) is
   begin
      pragma Assert (Ekind (Id) in Object_Kind or else Ekind (Id) = E_Void);
      Set_Node9 (Id, V);
   end Set_Current_Value;

   procedure Set_CR_Discriminant (Id : E; V : E) is
   begin
      Set_Node23 (Id, V);
   end Set_CR_Discriminant;

   procedure Set_Debug_Info_Off (Id : E; V : B := True) is
   begin
      Set_Flag166 (Id, V);
   end Set_Debug_Info_Off;

   procedure Set_Debug_Renaming_Link (Id : E; V : E) is
   begin
      Set_Node25 (Id, V);
   end Set_Debug_Renaming_Link;

   procedure Set_Default_Aspect_Component_Value (Id : E; V : E) is
   begin
      pragma Assert (Is_Array_Type (Id) and then Is_Base_Type (Id));
      Set_Node19 (Id, V);
   end Set_Default_Aspect_Component_Value;

   procedure Set_Default_Aspect_Value (Id : E; V : E) is
   begin
      pragma Assert (Is_Scalar_Type (Id) and then Is_Base_Type (Id));
      Set_Node19 (Id, V);
   end Set_Default_Aspect_Value;

   procedure Set_Default_Expr_Function (Id : E; V : E) is
   begin
      pragma Assert (Is_Formal (Id));
      Set_Node21 (Id, V);
   end Set_Default_Expr_Function;

   procedure Set_Default_Expressions_Processed (Id : E; V : B := True) is
   begin
      Set_Flag108 (Id, V);
   end Set_Default_Expressions_Processed;

   procedure Set_Default_Value (Id : E; V : N) is
   begin
      pragma Assert (Is_Formal (Id));
      Set_Node20 (Id, V);
   end Set_Default_Value;

   procedure Set_Delay_Cleanups (Id : E; V : B := True) is
   begin
      pragma Assert
        (Is_Subprogram (Id)
           or else Is_Task_Type (Id)
           or else Ekind (Id) = E_Block);
      Set_Flag114 (Id, V);
   end Set_Delay_Cleanups;

   procedure Set_Delay_Subprogram_Descriptors (Id : E; V : B := True) is
   begin
      pragma Assert
        (Is_Subprogram (Id) or else Ekind_In (Id, E_Package, E_Package_Body));

      Set_Flag50 (Id, V);
   end Set_Delay_Subprogram_Descriptors;

   procedure Set_Delta_Value (Id : E; V : R) is
   begin
      pragma Assert (Is_Fixed_Point_Type (Id));
      Set_Ureal18 (Id, V);
   end Set_Delta_Value;

   procedure Set_Dependent_Instances (Id : E; V : L) is
   begin
      pragma Assert (Is_Generic_Instance (Id));
      Set_Elist8 (Id, V);
   end Set_Dependent_Instances;

   procedure Set_Depends_On_Private (Id : E; V : B := True) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Flag14 (Id, V);
   end Set_Depends_On_Private;

   procedure Set_Derived_Type_Link (Id : E; V : E) is
   begin
      pragma Assert (Is_Type (Id) and then Is_Base_Type (Id));
      Set_Node31 (Id, V);
   end Set_Derived_Type_Link;

   procedure Set_Digits_Value (Id : E; V : U) is
   begin
      pragma Assert
        (Is_Floating_Point_Type (Id)
          or else Is_Decimal_Fixed_Point_Type (Id));
      Set_Uint17 (Id, V);
   end Set_Digits_Value;

   procedure Set_Directly_Designated_Type (Id : E; V : E) is
   begin
      Set_Node20 (Id, V);
   end Set_Directly_Designated_Type;

   procedure Set_Disable_Controlled (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id) and then Is_Base_Type (Id));
      Set_Flag253 (Id, V);
   end Set_Disable_Controlled;

   procedure Set_Discard_Names (Id : E; V : B := True) is
   begin
      Set_Flag88 (Id, V);
   end Set_Discard_Names;

   procedure Set_Discriminal (Id : E; V : E) is
   begin
      pragma Assert (Ekind (Id) = E_Discriminant);
      Set_Node17 (Id, V);
   end Set_Discriminal;

   procedure Set_Discriminal_Link (Id : E; V : E) is
   begin
      Set_Node10 (Id, V);
   end Set_Discriminal_Link;

   procedure Set_Discriminant_Checking_Func (Id  : E; V : E) is
   begin
      pragma Assert (Ekind (Id) = E_Component);
      Set_Node20 (Id, V);
   end Set_Discriminant_Checking_Func;

   procedure Set_Discriminant_Constraint (Id : E; V : L) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Elist21 (Id, V);
   end Set_Discriminant_Constraint;

   procedure Set_Discriminant_Default_Value (Id : E; V : N) is
   begin
      Set_Node20 (Id, V);
   end Set_Discriminant_Default_Value;

   procedure Set_Discriminant_Number (Id : E; V : U) is
   begin
      Set_Uint15 (Id, V);
   end Set_Discriminant_Number;

   procedure Set_Dispatch_Table_Wrappers (Id : E; V : L) is
   begin
      pragma Assert (Ekind (Id) = E_Record_Type
        and then Id = Implementation_Base_Type (Id));
      pragma Assert (V = No_Elist or else Is_Tagged_Type (Id));
      Set_Elist26 (Id, V);
   end Set_Dispatch_Table_Wrappers;

   procedure Set_DT_Entry_Count (Id : E; V : U) is
   begin
      pragma Assert (Ekind (Id) = E_Component);
      Set_Uint15 (Id, V);
   end Set_DT_Entry_Count;

   procedure Set_DT_Offset_To_Top_Func (Id : E; V : E) is
   begin
      pragma Assert (Ekind (Id) = E_Component and then Is_Tag (Id));
      Set_Node25 (Id, V);
   end Set_DT_Offset_To_Top_Func;

   procedure Set_DT_Position (Id : E; V : U) is
   begin
      pragma Assert (Ekind_In (Id, E_Function, E_Procedure));
      Set_Uint15 (Id, V);
   end Set_DT_Position;

   procedure Set_DTC_Entity (Id : E; V : E) is
   begin
      pragma Assert (Ekind_In (Id, E_Function, E_Procedure));
      Set_Node16 (Id, V);
   end Set_DTC_Entity;

   procedure Set_Elaborate_Body_Desirable (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind (Id) = E_Package);
      Set_Flag210 (Id, V);
   end Set_Elaborate_Body_Desirable;

   procedure Set_Elaboration_Entity (Id : E; V : E) is
   begin
      pragma Assert
        (Is_Subprogram (Id)
           or else
         Ekind (Id) = E_Package
           or else
         Is_Generic_Unit (Id));
      Set_Node13 (Id, V);
   end Set_Elaboration_Entity;

   procedure Set_Elaboration_Entity_Required (Id : E; V : B := True) is
   begin
      pragma Assert
        (Is_Subprogram (Id)
           or else
         Ekind (Id) = E_Package
           or else
         Is_Generic_Unit (Id));
      Set_Flag174 (Id, V);
   end Set_Elaboration_Entity_Required;

   procedure Set_Encapsulating_State (Id : E; V : E) is
   begin
      pragma Assert (Ekind_In (Id, E_Abstract_State, E_Constant, E_Variable));
      Set_Node32 (Id, V);
   end Set_Encapsulating_State;

   procedure Set_Enclosing_Scope (Id : E; V : E) is
   begin
      Set_Node18 (Id, V);
   end Set_Enclosing_Scope;

   procedure Set_Entry_Accepted (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Entry (Id));
      Set_Flag152 (Id, V);
   end Set_Entry_Accepted;

   procedure Set_Entry_Bodies_Array (Id : E; V : E) is
   begin
      Set_Node19 (Id, V);
   end Set_Entry_Bodies_Array;

   procedure Set_Entry_Cancel_Parameter (Id : E; V : E) is
   begin
      Set_Node23 (Id, V);
   end Set_Entry_Cancel_Parameter;

   procedure Set_Entry_Component (Id : E; V : E) is
   begin
      Set_Node11 (Id, V);
   end Set_Entry_Component;

   procedure Set_Entry_Formal (Id : E; V : E) is
   begin
      Set_Node16 (Id, V);
   end Set_Entry_Formal;

   procedure Set_Entry_Index_Constant (Id : E; V : E) is
   begin
      pragma Assert (Ekind (Id) = E_Entry_Index_Parameter);
      Set_Node18 (Id, V);
   end Set_Entry_Index_Constant;

   procedure Set_Entry_Parameters_Type (Id : E; V : E) is
   begin
      Set_Node15 (Id, V);
   end Set_Entry_Parameters_Type;

   procedure Set_Enum_Pos_To_Rep (Id : E; V : E) is
   begin
      pragma Assert (Ekind (Id) = E_Enumeration_Type);
      Set_Node23 (Id, V);
   end Set_Enum_Pos_To_Rep;

   procedure Set_Enumeration_Pos (Id : E; V : U) is
   begin
      pragma Assert (Ekind (Id) = E_Enumeration_Literal);
      Set_Uint11 (Id, V);
   end Set_Enumeration_Pos;

   procedure Set_Enumeration_Rep (Id : E; V : U) is
   begin
      pragma Assert (Ekind (Id) = E_Enumeration_Literal);
      Set_Uint12 (Id, V);
   end Set_Enumeration_Rep;

   procedure Set_Enumeration_Rep_Expr (Id : E; V : N) is
   begin
      pragma Assert (Ekind (Id) = E_Enumeration_Literal);
      Set_Node22 (Id, V);
   end Set_Enumeration_Rep_Expr;

   procedure Set_Equivalent_Type (Id : E; V : E) is
   begin
      pragma Assert
        (Ekind_In (Id, E_Class_Wide_Type,
                       E_Class_Wide_Subtype,
                       E_Access_Protected_Subprogram_Type,
                       E_Anonymous_Access_Protected_Subprogram_Type,
                       E_Access_Subprogram_Type,
                       E_Exception_Type));
      Set_Node18 (Id, V);
   end Set_Equivalent_Type;

   procedure Set_Esize (Id : E; V : U) is
   begin
      Set_Uint12 (Id, V);
   end Set_Esize;

   procedure Set_Extra_Accessibility (Id : E; V : E) is
   begin
      pragma Assert
        (Is_Formal (Id) or else Ekind_In (Id, E_Variable, E_Constant));
      Set_Node13 (Id, V);
   end Set_Extra_Accessibility;

   procedure Set_Extra_Accessibility_Of_Result (Id : E; V : E) is
   begin
      pragma Assert (Ekind_In (Id, E_Function, E_Operator, E_Subprogram_Type));
      Set_Node19 (Id, V);
   end Set_Extra_Accessibility_Of_Result;

   procedure Set_Extra_Constrained (Id : E; V : E) is
   begin
      pragma Assert (Is_Formal (Id) or else Ekind (Id) = E_Variable);
      Set_Node23 (Id, V);
   end Set_Extra_Constrained;

   procedure Set_Extra_Formal (Id : E; V : E) is
   begin
      Set_Node15 (Id, V);
   end Set_Extra_Formal;

   procedure Set_Extra_Formals (Id : E; V : E) is
   begin
      pragma Assert
        (Is_Overloadable (Id)
           or else Ekind_In (Id, E_Entry_Family,
                                 E_Subprogram_Body,
                                 E_Subprogram_Type));
      Set_Node28 (Id, V);
   end Set_Extra_Formals;

   procedure Set_Finalization_Master (Id : E; V : E) is
   begin
      pragma Assert (Is_Access_Type (Id) and then Is_Base_Type (Id));
      Set_Node23 (Id, V);
   end Set_Finalization_Master;

   procedure Set_Finalize_Storage_Only (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id) and then Is_Base_Type (Id));
      Set_Flag158 (Id, V);
   end Set_Finalize_Storage_Only;

   procedure Set_Finalizer (Id : E; V : E) is
   begin
      pragma Assert (Ekind_In (Id, E_Package, E_Package_Body));
      Set_Node28 (Id, V);
   end Set_Finalizer;

   procedure Set_First_Entity (Id : E; V : E) is
   begin
      Set_Node17 (Id, V);
   end Set_First_Entity;

   procedure Set_First_Exit_Statement (Id : E; V : N) is
   begin
      pragma Assert (Ekind (Id) = E_Loop);
      Set_Node8 (Id, V);
   end Set_First_Exit_Statement;

   procedure Set_First_Index (Id : E; V : N) is
   begin
      pragma Assert (Is_Array_Type (Id));
      Set_Node17 (Id, V);
   end Set_First_Index;

   procedure Set_First_Literal (Id : E; V : E) is
   begin
      pragma Assert (Is_Enumeration_Type (Id));
      Set_Node17 (Id, V);
   end Set_First_Literal;

   procedure Set_First_Private_Entity (Id : E; V : E) is
   begin
      pragma Assert (Ekind_In (Id, E_Package, E_Generic_Package)
                       or else Ekind (Id) in Concurrent_Kind);
      Set_Node16 (Id, V);
   end Set_First_Private_Entity;

   procedure Set_First_Rep_Item (Id : E; V : N) is
   begin
      Set_Node6 (Id, V);
   end Set_First_Rep_Item;

   procedure Set_Float_Rep (Id : E; V : F) is
      pragma Assert (Ekind (Id) = E_Floating_Point_Type);
   begin
      Set_Uint10 (Id, UI_From_Int (F'Pos (V)));
   end Set_Float_Rep;

   procedure Set_Freeze_Node (Id : E; V : N) is
   begin
      Set_Node7 (Id, V);
   end Set_Freeze_Node;

   procedure Set_From_Limited_With (Id : E; V : B := True) is
   begin
      pragma Assert
        (Is_Type (Id) or else Ekind_In (Id, E_Abstract_State, E_Package));
      Set_Flag159 (Id, V);
   end Set_From_Limited_With;

   procedure Set_Full_View (Id : E; V : E) is
   begin
      pragma Assert (Is_Type (Id) or else Ekind (Id) = E_Constant);
      Set_Node11 (Id, V);
   end Set_Full_View;

   procedure Set_Generic_Homonym (Id : E; V : E) is
   begin
      Set_Node11 (Id, V);
   end Set_Generic_Homonym;

   procedure Set_Generic_Renamings (Id : E; V : L) is
   begin
      Set_Elist23 (Id, V);
   end Set_Generic_Renamings;

   procedure Set_Handler_Records (Id : E; V : S) is
   begin
      Set_List10 (Id, V);
   end Set_Handler_Records;

   procedure Set_Has_Aliased_Components (Id : E; V : B := True) is
   begin
      pragma Assert (Id = Base_Type (Id));
      Set_Flag135 (Id, V);
   end Set_Has_Aliased_Components;

   procedure Set_Has_Alignment_Clause (Id : E; V : B := True) is
   begin
      Set_Flag46 (Id, V);
   end Set_Has_Alignment_Clause;

   procedure Set_Has_All_Calls_Remote (Id : E; V : B := True) is
   begin
      Set_Flag79 (Id, V);
   end Set_Has_All_Calls_Remote;

   procedure Set_Has_Atomic_Components (Id : E; V : B := True) is
   begin
      pragma Assert (not Is_Type (Id) or else Is_Base_Type (Id));
      Set_Flag86 (Id, V);
   end Set_Has_Atomic_Components;

   procedure Set_Has_Biased_Representation (Id : E; V : B := True) is
   begin
      pragma Assert
        ((V = False) or else (Is_Discrete_Type (Id) or else Is_Object (Id)));
      Set_Flag139 (Id, V);
   end Set_Has_Biased_Representation;

   procedure Set_Has_Completion (Id : E; V : B := True) is
   begin
      Set_Flag26 (Id, V);
   end Set_Has_Completion;

   procedure Set_Has_Completion_In_Body (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag71 (Id, V);
   end Set_Has_Completion_In_Body;

   procedure Set_Has_Complex_Representation (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind (Id) = E_Record_Type);
      Set_Flag140 (Id, V);
   end Set_Has_Complex_Representation;

   procedure Set_Has_Component_Size_Clause (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind (Id) = E_Array_Type);
      Set_Flag68 (Id, V);
   end Set_Has_Component_Size_Clause;

   procedure Set_Has_Constrained_Partial_View (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag187 (Id, V);
   end Set_Has_Constrained_Partial_View;

   procedure Set_Has_Contiguous_Rep (Id : E; V : B := True) is
   begin
      Set_Flag181 (Id, V);
   end Set_Has_Contiguous_Rep;

   procedure Set_Has_Controlled_Component (Id : E; V : B := True) is
   begin
      pragma Assert (Id = Base_Type (Id));
      Set_Flag43 (Id, V);
   end Set_Has_Controlled_Component;

   procedure Set_Has_Controlling_Result (Id : E; V : B := True) is
   begin
      Set_Flag98 (Id, V);
   end Set_Has_Controlling_Result;

   procedure Set_Has_Convention_Pragma (Id : E; V : B := True) is
   begin
      Set_Flag119 (Id, V);
   end Set_Has_Convention_Pragma;

   procedure Set_Has_Default_Aspect (Id : E; V : B := True) is
   begin
      pragma Assert
        ((Is_Scalar_Type (Id) or else Is_Array_Type (Id))
           and then Is_Base_Type (Id));
      Set_Flag39 (Id, V);
   end Set_Has_Default_Aspect;

   procedure Set_Has_Default_Init_Cond (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag3 (Base_Type (Id), V);
   end Set_Has_Default_Init_Cond;

   procedure Set_Has_Delayed_Aspects (Id : E; V : B := True) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Flag200 (Id, V);
   end Set_Has_Delayed_Aspects;

   procedure Set_Has_Delayed_Freeze (Id : E; V : B := True) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Flag18 (Id, V);
   end Set_Has_Delayed_Freeze;

   procedure Set_Has_Delayed_Rep_Aspects (Id : E; V : B := True) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Flag261 (Id, V);
   end Set_Has_Delayed_Rep_Aspects;

   procedure Set_Has_Discriminants (Id : E; V : B := True) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Flag5 (Id, V);
   end Set_Has_Discriminants;

   procedure Set_Has_Dispatch_Table (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind (Id) = E_Record_Type
        and then Is_Tagged_Type (Id));
      Set_Flag220 (Id, V);
   end Set_Has_Dispatch_Table;

   procedure Set_Has_Dynamic_Predicate_Aspect (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag258 (Id, V);
   end Set_Has_Dynamic_Predicate_Aspect;

   procedure Set_Has_Enumeration_Rep_Clause (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Enumeration_Type (Id));
      Set_Flag66 (Id, V);
   end Set_Has_Enumeration_Rep_Clause;

   procedure Set_Has_Exit (Id : E; V : B := True) is
   begin
      Set_Flag47 (Id, V);
   end Set_Has_Exit;

   procedure Set_Has_Expanded_Contract (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind_In (Id, E_Entry,
                                   E_Entry_Family,
                                   E_Function,
                                   E_Procedure));
      Set_Flag240 (Id, V);
   end Set_Has_Expanded_Contract;

   procedure Set_Has_Forward_Instantiation (Id : E; V : B := True) is
   begin
      Set_Flag175 (Id, V);
   end Set_Has_Forward_Instantiation;

   procedure Set_Has_Fully_Qualified_Name (Id : E; V : B := True) is
   begin
      Set_Flag173 (Id, V);
   end Set_Has_Fully_Qualified_Name;

   procedure Set_Has_Gigi_Rep_Item (Id : E; V : B := True) is
   begin
      Set_Flag82 (Id, V);
   end Set_Has_Gigi_Rep_Item;

   procedure Set_Has_Homonym (Id : E; V : B := True) is
   begin
      Set_Flag56 (Id, V);
   end Set_Has_Homonym;

   procedure Set_Has_Implicit_Dereference (Id : E; V : B := True) is
   begin
      Set_Flag251 (Id, V);
   end Set_Has_Implicit_Dereference;

   procedure Set_Has_Independent_Components (Id : E; V : B := True) is
   begin
      pragma Assert (not Is_Type (Id) or else Is_Base_Type (Id));
      Set_Flag34 (Id, V);
   end Set_Has_Independent_Components;

   procedure Set_Has_Inheritable_Invariants (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag248 (Id, V);
   end Set_Has_Inheritable_Invariants;

   procedure Set_Has_Inherited_Default_Init_Cond (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag133 (Base_Type (Id), V);
   end Set_Has_Inherited_Default_Init_Cond;

   procedure Set_Has_Initial_Value (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind_In (Id, E_Variable, E_Out_Parameter));
      Set_Flag219 (Id, V);
   end Set_Has_Initial_Value;

   procedure Set_Has_Invariants (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag232 (Id, V);
   end Set_Has_Invariants;

   procedure Set_Has_Loop_Entry_Attributes (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind (Id) = E_Loop);
      Set_Flag260 (Id, V);
   end Set_Has_Loop_Entry_Attributes;

   procedure Set_Has_Machine_Radix_Clause (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Decimal_Fixed_Point_Type (Id));
      Set_Flag83 (Id, V);
   end Set_Has_Machine_Radix_Clause;

   procedure Set_Has_Master_Entity (Id : E; V : B := True) is
   begin
      Set_Flag21 (Id, V);
   end Set_Has_Master_Entity;

   procedure Set_Has_Missing_Return (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind_In (Id, E_Function, E_Generic_Function));
      Set_Flag142 (Id, V);
   end Set_Has_Missing_Return;

   procedure Set_Has_Nested_Block_With_Handler (Id : E; V : B := True) is
   begin
      Set_Flag101 (Id, V);
   end Set_Has_Nested_Block_With_Handler;

   procedure Set_Has_Nested_Subprogram (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Subprogram (Id));
      Set_Flag282 (Id, V);
   end Set_Has_Nested_Subprogram;

   procedure Set_Has_Non_Standard_Rep (Id : E; V : B := True) is
   begin
      pragma Assert (Id = Base_Type (Id));
      Set_Flag75 (Id, V);
   end Set_Has_Non_Standard_Rep;

   procedure Set_Has_Object_Size_Clause (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag172 (Id, V);
   end Set_Has_Object_Size_Clause;

   procedure Set_Has_Out_Or_In_Out_Parameter (Id : E; V : B := True) is
   begin
      pragma Assert
        (Ekind_In (Id, E_Entry, E_Entry_Family)
          or else Is_Subprogram_Or_Generic_Subprogram (Id));
      Set_Flag110 (Id, V);
   end Set_Has_Out_Or_In_Out_Parameter;

   procedure Set_Has_Per_Object_Constraint (Id : E; V : B := True) is
   begin
      Set_Flag154 (Id, V);
   end Set_Has_Per_Object_Constraint;

   procedure Set_Has_Pragma_Controlled (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Access_Type (Id));
      Set_Flag27 (Base_Type (Id), V);
   end Set_Has_Pragma_Controlled;

   procedure Set_Has_Pragma_Elaborate_Body (Id : E; V : B := True) is
   begin
      Set_Flag150 (Id, V);
   end Set_Has_Pragma_Elaborate_Body;

   procedure Set_Has_Pragma_Inline (Id : E; V : B := True) is
   begin
      Set_Flag157 (Id, V);
   end Set_Has_Pragma_Inline;

   procedure Set_Has_Pragma_Inline_Always (Id : E; V : B := True) is
   begin
      Set_Flag230 (Id, V);
   end Set_Has_Pragma_Inline_Always;

   procedure Set_Has_Pragma_No_Inline (Id : E; V : B := True) is
   begin
      Set_Flag201 (Id, V);
   end Set_Has_Pragma_No_Inline;

   procedure Set_Has_Pragma_Ordered (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Enumeration_Type (Id));
      pragma Assert (Id = Base_Type (Id));
      Set_Flag198 (Id, V);
   end Set_Has_Pragma_Ordered;

   procedure Set_Has_Pragma_Pack (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Array_Type (Id) or else Is_Record_Type (Id));
      pragma Assert (Id = Base_Type (Id));
      Set_Flag121 (Id, V);
   end Set_Has_Pragma_Pack;

   procedure Set_Has_Pragma_Preelab_Init (Id : E; V : B := True) is
   begin
      Set_Flag221 (Id, V);
   end Set_Has_Pragma_Preelab_Init;

   procedure Set_Has_Pragma_Pure (Id : E; V : B := True) is
   begin
      Set_Flag203 (Id, V);
   end Set_Has_Pragma_Pure;

   procedure Set_Has_Pragma_Pure_Function (Id : E; V : B := True) is
   begin
      Set_Flag179 (Id, V);
   end Set_Has_Pragma_Pure_Function;

   procedure Set_Has_Pragma_Thread_Local_Storage (Id : E; V : B := True) is
   begin
      Set_Flag169 (Id, V);
   end Set_Has_Pragma_Thread_Local_Storage;

   procedure Set_Has_Pragma_Unmodified (Id : E; V : B := True) is
   begin
      Set_Flag233 (Id, V);
   end Set_Has_Pragma_Unmodified;

   procedure Set_Has_Pragma_Unreferenced (Id : E; V : B := True) is
   begin
      Set_Flag180 (Id, V);
   end Set_Has_Pragma_Unreferenced;

   procedure Set_Has_Pragma_Unreferenced_Objects (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag212 (Id, V);
   end Set_Has_Pragma_Unreferenced_Objects;

   procedure Set_Has_Predicates (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id) or else Ekind (Id) = E_Void);
      Set_Flag250 (Id, V);
   end Set_Has_Predicates;

   procedure Set_Has_Primitive_Operations (Id : E; V : B := True) is
   begin
      pragma Assert (Id = Base_Type (Id));
      Set_Flag120 (Id, V);
   end Set_Has_Primitive_Operations;

   procedure Set_Has_Private_Ancestor (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag151 (Id, V);
   end Set_Has_Private_Ancestor;

   procedure Set_Has_Private_Declaration (Id : E; V : B := True) is
   begin
      Set_Flag155 (Id, V);
   end Set_Has_Private_Declaration;

   procedure Set_Has_Protected (Id : E; V : B := True) is
   begin
      Set_Flag271 (Id, V);
   end Set_Has_Protected;

   procedure Set_Has_Qualified_Name (Id : E; V : B := True) is
   begin
      Set_Flag161 (Id, V);
   end Set_Has_Qualified_Name;

   procedure Set_Has_RACW (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind (Id) = E_Package);
      Set_Flag214 (Id, V);
   end Set_Has_RACW;

   procedure Set_Has_Record_Rep_Clause (Id : E; V : B := True) is
   begin
      pragma Assert (Id = Base_Type (Id));
      Set_Flag65 (Id, V);
   end Set_Has_Record_Rep_Clause;

   procedure Set_Has_Recursive_Call (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Subprogram (Id));
      Set_Flag143 (Id, V);
   end Set_Has_Recursive_Call;

   procedure Set_Has_Shift_Operator (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Integer_Type (Id) and then Is_Base_Type (Id));
      Set_Flag267 (Id, V);
   end Set_Has_Shift_Operator;

   procedure Set_Has_Size_Clause (Id : E; V : B := True) is
   begin
      Set_Flag29 (Id, V);
   end Set_Has_Size_Clause;

   procedure Set_Has_Small_Clause (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Ordinary_Fixed_Point_Type (Id));
      Set_Flag67 (Id, V);
   end Set_Has_Small_Clause;

   procedure Set_Has_Specified_Layout (Id : E; V : B := True) is
   begin
      pragma Assert (Id = Base_Type (Id));
      Set_Flag100 (Id, V);
   end Set_Has_Specified_Layout;

   procedure Set_Has_Specified_Stream_Input (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag190 (Id, V);
   end Set_Has_Specified_Stream_Input;

   procedure Set_Has_Specified_Stream_Output (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag191 (Id, V);
   end Set_Has_Specified_Stream_Output;

   procedure Set_Has_Specified_Stream_Read (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag192 (Id, V);
   end Set_Has_Specified_Stream_Read;

   procedure Set_Has_Specified_Stream_Write (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag193 (Id, V);
   end Set_Has_Specified_Stream_Write;

   procedure Set_Has_Static_Discriminants (Id : E; V : B := True) is
   begin
      Set_Flag211 (Id, V);
   end Set_Has_Static_Discriminants;

   procedure Set_Has_Static_Predicate (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag269 (Id, V);
   end Set_Has_Static_Predicate;

   procedure Set_Has_Static_Predicate_Aspect (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag259 (Id, V);
   end Set_Has_Static_Predicate_Aspect;

   procedure Set_Has_Storage_Size_Clause (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Access_Type (Id) or else Is_Task_Type (Id));
      pragma Assert (Id = Base_Type (Id));
      Set_Flag23 (Id, V);
   end Set_Has_Storage_Size_Clause;

   procedure Set_Has_Stream_Size_Clause (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Elementary_Type (Id));
      Set_Flag184 (Id, V);
   end Set_Has_Stream_Size_Clause;

   procedure Set_Has_Task (Id : E; V : B := True) is
   begin
      pragma Assert (Id = Base_Type (Id));
      Set_Flag30 (Id, V);
   end Set_Has_Task;

   procedure Set_Has_Thunks (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Tag (Id));
      Set_Flag228 (Id, V);
   end Set_Has_Thunks;

   procedure Set_Has_Unchecked_Union (Id : E; V : B := True) is
   begin
      pragma Assert (Id = Base_Type (Id));
      Set_Flag123 (Id, V);
   end Set_Has_Unchecked_Union;

   procedure Set_Has_Unknown_Discriminants (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag72 (Id, V);
   end Set_Has_Unknown_Discriminants;

   procedure Set_Has_Visible_Refinement (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind (Id) = E_Abstract_State);
      Set_Flag263 (Id, V);
   end Set_Has_Visible_Refinement;

   procedure Set_Has_Volatile_Components (Id : E; V : B := True) is
   begin
      pragma Assert (not Is_Type (Id) or else Is_Base_Type (Id));
      Set_Flag87 (Id, V);
   end Set_Has_Volatile_Components;

   procedure Set_Has_Xref_Entry (Id : E; V : B := True) is
   begin
      Set_Flag182 (Id, V);
   end Set_Has_Xref_Entry;

   procedure Set_Hiding_Loop_Variable (Id : E; V : E) is
   begin
      pragma Assert (Ekind (Id) = E_Variable);
      Set_Node8 (Id, V);
   end Set_Hiding_Loop_Variable;

   procedure Set_Homonym (Id : E; V : E) is
   begin
      pragma Assert (Id /= V);
      Set_Node4 (Id, V);
   end Set_Homonym;

   procedure Set_Incomplete_Actuals (Id : E; V : L) is
   begin
      pragma Assert (Ekind (Id) = E_Package);
      Set_Elist24 (Id, V);
   end Set_Incomplete_Actuals;

   procedure Set_Import_Pragma (Id : E; V : E) is
   begin
      pragma Assert (Is_Subprogram (Id));
      Set_Node35 (Id, V);
   end Set_Import_Pragma;

   procedure Set_Interface_Alias (Id : E; V : E) is
   begin
      pragma Assert
        (Is_Internal (Id)
           and then Is_Hidden (Id)
           and then (Ekind_In (Id, E_Procedure, E_Function)));
      Set_Node25 (Id, V);
   end Set_Interface_Alias;

   procedure Set_Interfaces (Id : E; V : L) is
   begin
      pragma Assert (Is_Record_Type (Id));
      Set_Elist25 (Id, V);
   end Set_Interfaces;

   procedure Set_In_Package_Body (Id : E; V : B := True) is
   begin
      Set_Flag48 (Id, V);
   end Set_In_Package_Body;

   procedure Set_In_Private_Part (Id : E; V : B := True) is
   begin
      Set_Flag45 (Id, V);
   end Set_In_Private_Part;

   procedure Set_In_Use (Id : E; V : B := True) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Flag8 (Id, V);
   end Set_In_Use;

   procedure Set_Initialization_Statements (Id : E; V : N) is
   begin
      --  Tolerate an E_Void entity since this can be called while resolving
      --  an aggregate used as the initialization expression for an object
      --  declaration, and this occurs before the Ekind for the object is set.

      pragma Assert (Ekind_In (Id, E_Void, E_Constant, E_Variable));
      Set_Node28 (Id, V);
   end Set_Initialization_Statements;

   procedure Set_Inner_Instances (Id : E; V : L) is
   begin
      Set_Elist23 (Id, V);
   end Set_Inner_Instances;

   procedure Set_Interface_Name (Id : E; V : N) is
   begin
      Set_Node21 (Id, V);
   end Set_Interface_Name;

   procedure Set_Is_Abstract_Subprogram (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Overloadable (Id));
      Set_Flag19 (Id, V);
   end Set_Is_Abstract_Subprogram;

   procedure Set_Is_Abstract_Type (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag146 (Id, V);
   end Set_Is_Abstract_Type;

   procedure Set_Is_Local_Anonymous_Access (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Access_Type (Id));
      Set_Flag194 (Id, V);
   end Set_Is_Local_Anonymous_Access;

   procedure Set_Is_Access_Constant (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Access_Type (Id));
      Set_Flag69 (Id, V);
   end Set_Is_Access_Constant;

   procedure Set_Is_Ada_2005_Only (Id : E; V : B := True) is
   begin
      Set_Flag185 (Id, V);
   end Set_Is_Ada_2005_Only;

   procedure Set_Is_Ada_2012_Only (Id : E; V : B := True) is
   begin
      Set_Flag199 (Id, V);
   end Set_Is_Ada_2012_Only;

   procedure Set_Is_Aliased (Id : E; V : B := True) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Flag15 (Id, V);
   end Set_Is_Aliased;

   procedure Set_Is_Asynchronous (Id : E; V : B := True) is
   begin
      pragma Assert
        (Ekind (Id) = E_Procedure or else Is_Type (Id));
      Set_Flag81 (Id, V);
   end Set_Is_Asynchronous;

   procedure Set_Is_Atomic (Id : E; V : B := True) is
   begin
      Set_Flag85 (Id, V);
   end Set_Is_Atomic;

   procedure Set_Is_Bit_Packed_Array (Id : E; V : B := True) is
   begin
      pragma Assert ((not V)
        or else (Is_Array_Type (Id) and then Is_Base_Type (Id)));
      Set_Flag122 (Id, V);
   end Set_Is_Bit_Packed_Array;

   procedure Set_Is_Called (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind_In (Id, E_Procedure, E_Function));
      Set_Flag102 (Id, V);
   end Set_Is_Called;

   procedure Set_Is_Character_Type (Id : E; V : B := True) is
   begin
      Set_Flag63 (Id, V);
   end Set_Is_Character_Type;

   procedure Set_Is_Checked_Ghost_Entity (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Formal (Id)
        or else Is_Object (Id)
        or else Is_Package_Or_Generic_Package (Id)
        or else Is_Subprogram_Or_Generic_Subprogram (Id)
        or else Is_Type (Id)
        or else Ekind (Id) = E_Abstract_State
        or else Ekind (Id) = E_Component
        or else Ekind (Id) = E_Discriminant
        or else Ekind (Id) = E_Exception
        or else Ekind (Id) = E_Package_Body
        or else Ekind (Id) = E_Subprogram_Body

        --  Allow this attribute to appear on non-analyzed entities

        or else Ekind (Id) = E_Void);
      Set_Flag277 (Id, V);
   end Set_Is_Checked_Ghost_Entity;

   procedure Set_Is_Child_Unit (Id : E; V : B := True) is
   begin
      Set_Flag73 (Id, V);
   end Set_Is_Child_Unit;

   procedure Set_Is_Class_Wide_Equivalent_Type (Id : E; V : B := True) is
   begin
      Set_Flag35 (Id, V);
   end Set_Is_Class_Wide_Equivalent_Type;

   procedure Set_Is_Compilation_Unit (Id : E; V : B := True) is
   begin
      Set_Flag149 (Id, V);
   end Set_Is_Compilation_Unit;

   procedure Set_Is_Completely_Hidden (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind (Id) = E_Discriminant);
      Set_Flag103 (Id, V);
   end Set_Is_Completely_Hidden;

   procedure Set_Is_Concurrent_Record_Type (Id : E; V : B := True) is
   begin
      Set_Flag20 (Id, V);
   end Set_Is_Concurrent_Record_Type;

   procedure Set_Is_Constr_Subt_For_U_Nominal (Id : E; V : B := True) is
   begin
      Set_Flag80 (Id, V);
   end Set_Is_Constr_Subt_For_U_Nominal;

   procedure Set_Is_Constr_Subt_For_UN_Aliased (Id : E; V : B := True) is
   begin
      Set_Flag141 (Id, V);
   end Set_Is_Constr_Subt_For_UN_Aliased;

   procedure Set_Is_Constrained (Id : E; V : B := True) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Flag12 (Id, V);
   end Set_Is_Constrained;

   procedure Set_Is_Constructor (Id : E; V : B := True) is
   begin
      Set_Flag76 (Id, V);
   end Set_Is_Constructor;

   procedure Set_Is_Controlled (Id : E; V : B := True) is
   begin
      pragma Assert (Id = Base_Type (Id));
      Set_Flag42 (Id, V);
   end Set_Is_Controlled;

   procedure Set_Is_Controlling_Formal (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Formal (Id));
      Set_Flag97 (Id, V);
   end Set_Is_Controlling_Formal;

   procedure Set_Is_CPP_Class (Id : E; V : B := True) is
   begin
      Set_Flag74 (Id, V);
   end Set_Is_CPP_Class;

   procedure Set_Is_Default_Init_Cond_Procedure (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind (Id) = E_Procedure);
      Set_Flag132 (Id, V);
   end Set_Is_Default_Init_Cond_Procedure;

   procedure Set_Is_Descendant_Of_Address (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag223 (Id, V);
   end Set_Is_Descendant_Of_Address;

   procedure Set_Is_Discrim_SO_Function (Id : E; V : B := True) is
   begin
      Set_Flag176 (Id, V);
   end Set_Is_Discrim_SO_Function;

   procedure Set_Is_Discriminant_Check_Function (Id : E; V : B := True) is
   begin
      Set_Flag264 (Id, V);
   end Set_Is_Discriminant_Check_Function;

   procedure Set_Is_Dispatch_Table_Entity (Id : E; V : B := True) is
   begin
      Set_Flag234 (Id, V);
   end Set_Is_Dispatch_Table_Entity;

   procedure Set_Is_Dispatching_Operation (Id : E; V : B := True) is
   begin
      pragma Assert
        (V = False
           or else
         Is_Overloadable (Id)
           or else
         Ekind (Id) = E_Subprogram_Type);

      Set_Flag6 (Id, V);
   end Set_Is_Dispatching_Operation;

   procedure Set_Is_Eliminated (Id : E; V : B := True) is
   begin
      Set_Flag124 (Id, V);
   end Set_Is_Eliminated;

   procedure Set_Is_Entry_Formal (Id : E; V : B := True) is
   begin
      Set_Flag52 (Id, V);
   end Set_Is_Entry_Formal;

   procedure Set_Is_Exception_Handler (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind (Id) = E_Block);
      Set_Flag286 (Id, V);
   end Set_Is_Exception_Handler;

   procedure Set_Is_Exported (Id : E; V : B := True) is
   begin
      Set_Flag99 (Id, V);
   end Set_Is_Exported;

   procedure Set_Is_First_Subtype (Id : E; V : B := True) is
   begin
      Set_Flag70 (Id, V);
   end Set_Is_First_Subtype;

   procedure Set_Is_For_Access_Subtype (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind_In (Id, E_Record_Subtype, E_Private_Subtype));
      Set_Flag118 (Id, V);
   end Set_Is_For_Access_Subtype;

   procedure Set_Is_Formal_Subprogram (Id : E; V : B := True) is
   begin
      Set_Flag111 (Id, V);
   end Set_Is_Formal_Subprogram;

   procedure Set_Is_Frozen (Id : E; V : B := True) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Flag4 (Id, V);
   end Set_Is_Frozen;

   procedure Set_Is_Generic_Actual_Subprogram (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind_In (Id, E_Function, E_Procedure));
      Set_Flag274 (Id, V);
   end Set_Is_Generic_Actual_Subprogram;

   procedure Set_Is_Generic_Actual_Type (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag94 (Id, V);
   end Set_Is_Generic_Actual_Type;

   procedure Set_Is_Generic_Instance (Id : E; V : B := True) is
   begin
      Set_Flag130 (Id, V);
   end Set_Is_Generic_Instance;

   procedure Set_Is_Generic_Type (Id : E; V : B := True) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Flag13 (Id, V);
   end Set_Is_Generic_Type;

   procedure Set_Is_Hidden (Id : E; V : B := True) is
   begin
      Set_Flag57 (Id, V);
   end Set_Is_Hidden;

   procedure Set_Is_Hidden_Non_Overridden_Subpgm (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind_In (Id, E_Function, E_Procedure));
      Set_Flag2 (Id, V);
   end Set_Is_Hidden_Non_Overridden_Subpgm;

   procedure Set_Is_Hidden_Open_Scope (Id : E; V : B := True) is
   begin
      Set_Flag171 (Id, V);
   end Set_Is_Hidden_Open_Scope;

   procedure Set_Is_Ignored_Ghost_Entity (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Formal (Id)
        or else Is_Object (Id)
        or else Is_Package_Or_Generic_Package (Id)
        or else Is_Subprogram_Or_Generic_Subprogram (Id)
        or else Is_Type (Id)
        or else Ekind (Id) = E_Abstract_State
        or else Ekind (Id) = E_Component
        or else Ekind (Id) = E_Discriminant
        or else Ekind (Id) = E_Exception
        or else Ekind (Id) = E_Package_Body
        or else Ekind (Id) = E_Subprogram_Body

        --  Allow this attribute to appear on non-analyzed entities

        or else Ekind (Id) = E_Void);
      Set_Flag278 (Id, V);
   end Set_Is_Ignored_Ghost_Entity;

   procedure Set_Is_Immediately_Visible (Id : E; V : B := True) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Flag7 (Id, V);
   end Set_Is_Immediately_Visible;

   procedure Set_Is_Implementation_Defined (Id : E; V : B := True) is
   begin
      Set_Flag254 (Id, V);
   end Set_Is_Implementation_Defined;

   procedure Set_Is_Imported (Id : E; V : B := True) is
   begin
      Set_Flag24 (Id, V);
   end Set_Is_Imported;

   procedure Set_Is_Independent (Id : E; V : B := True) is
   begin
      Set_Flag268 (Id, V);
   end Set_Is_Independent;

   procedure Set_Is_Inlined (Id : E; V : B := True) is
   begin
      Set_Flag11 (Id, V);
   end Set_Is_Inlined;

   procedure Set_Is_Inlined_Always (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind (Id) = E_Function or else Ekind (Id) = E_Procedure);
      Set_Flag1 (Id, V);
   end Set_Is_Inlined_Always;

   procedure Set_Is_Interface (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Record_Type (Id));
      Set_Flag186 (Id, V);
   end Set_Is_Interface;

   procedure Set_Is_Instantiated (Id : E; V : B := True) is
   begin
      Set_Flag126 (Id, V);
   end Set_Is_Instantiated;

   procedure Set_Is_Internal (Id : E; V : B := True) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Flag17 (Id, V);
   end Set_Is_Internal;

   procedure Set_Is_Interrupt_Handler (Id : E; V : B := True) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Flag89 (Id, V);
   end Set_Is_Interrupt_Handler;

   procedure Set_Is_Intrinsic_Subprogram (Id : E; V : B := True) is
   begin
      Set_Flag64 (Id, V);
   end Set_Is_Intrinsic_Subprogram;

   procedure Set_Is_Invariant_Procedure (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind (Id) = E_Procedure);
      Set_Flag257 (Id, V);
   end Set_Is_Invariant_Procedure;

   procedure Set_Is_Itype (Id : E; V : B := True) is
   begin
      Set_Flag91 (Id, V);
   end Set_Is_Itype;

   procedure Set_Is_Known_Non_Null (Id : E; V : B := True) is
   begin
      Set_Flag37 (Id, V);
   end Set_Is_Known_Non_Null;

   procedure Set_Is_Known_Null (Id : E; V : B := True) is
   begin
      Set_Flag204 (Id, V);
   end Set_Is_Known_Null;

   procedure Set_Is_Known_Valid (Id : E; V : B := True) is
   begin
      Set_Flag170 (Id, V);
   end Set_Is_Known_Valid;

   procedure Set_Is_Limited_Composite (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag106 (Id, V);
   end Set_Is_Limited_Composite;

   procedure Set_Is_Limited_Interface (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Interface (Id));
      Set_Flag197 (Id, V);
   end Set_Is_Limited_Interface;

   procedure Set_Is_Limited_Record (Id : E; V : B := True) is
   begin
      Set_Flag25 (Id, V);
   end Set_Is_Limited_Record;

   procedure Set_Is_Machine_Code_Subprogram (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Subprogram (Id));
      Set_Flag137 (Id, V);
   end Set_Is_Machine_Code_Subprogram;

   procedure Set_Is_Non_Static_Subtype (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag109 (Id, V);
   end Set_Is_Non_Static_Subtype;

   procedure Set_Is_Null_Init_Proc (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind (Id) = E_Procedure);
      Set_Flag178 (Id, V);
   end Set_Is_Null_Init_Proc;

   procedure Set_Is_Obsolescent (Id : E; V : B := True) is
   begin
      Set_Flag153 (Id, V);
   end Set_Is_Obsolescent;

   procedure Set_Is_Only_Out_Parameter (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind (Id) = E_Out_Parameter);
      Set_Flag226 (Id, V);
   end Set_Is_Only_Out_Parameter;

   procedure Set_Is_Package_Body_Entity (Id : E; V : B := True) is
   begin
      Set_Flag160 (Id, V);
   end Set_Is_Package_Body_Entity;

   procedure Set_Is_Packed (Id : E; V : B := True) is
   begin
      pragma Assert (Id = Base_Type (Id));
      Set_Flag51 (Id, V);
   end Set_Is_Packed;

   procedure Set_Is_Packed_Array_Impl_Type (Id : E; V : B := True) is
   begin
      Set_Flag138 (Id, V);
   end Set_Is_Packed_Array_Impl_Type;

   procedure Set_Is_Param_Block_Component_Type (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind_In (Id, E_Void, E_General_Access_Type));
      Set_Flag215 (Id, V);
   end Set_Is_Param_Block_Component_Type;

   procedure Set_Is_Potentially_Use_Visible (Id : E; V : B := True) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Flag9 (Id, V);
   end Set_Is_Potentially_Use_Visible;

   procedure Set_Is_Predicate_Function (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind_In (Id, E_Function, E_Procedure));
      Set_Flag255 (Id, V);
   end Set_Is_Predicate_Function;

   procedure Set_Is_Predicate_Function_M (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind_In (Id, E_Function, E_Procedure));
      Set_Flag256 (Id, V);
   end Set_Is_Predicate_Function_M;

   procedure Set_Is_Preelaborated (Id : E; V : B := True) is
   begin
      Set_Flag59 (Id, V);
   end Set_Is_Preelaborated;

   procedure Set_Is_Primitive (Id : E; V : B := True) is
   begin
      pragma Assert
        (Is_Overloadable (Id)
           or else Ekind_In (Id, E_Generic_Function, E_Generic_Procedure));
      Set_Flag218 (Id, V);
   end Set_Is_Primitive;

   procedure Set_Is_Primitive_Wrapper (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind_In (Id, E_Function, E_Procedure));
      Set_Flag195 (Id, V);
   end Set_Is_Primitive_Wrapper;

   procedure Set_Is_Private_Composite (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag107 (Id, V);
   end Set_Is_Private_Composite;

   procedure Set_Is_Private_Descendant (Id : E; V : B := True) is
   begin
      Set_Flag53 (Id, V);
   end Set_Is_Private_Descendant;

   procedure Set_Is_Private_Primitive (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind_In (Id, E_Function, E_Procedure));
      Set_Flag245 (Id, V);
   end Set_Is_Private_Primitive;

   procedure Set_Is_Processed_Transient (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind_In (Id, E_Constant, E_Loop_Parameter, E_Variable));
      Set_Flag252 (Id, V);
   end Set_Is_Processed_Transient;

   procedure Set_Is_Public (Id : E; V : B := True) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Flag10 (Id, V);
   end Set_Is_Public;

   procedure Set_Is_Pure (Id : E; V : B := True) is
   begin
      Set_Flag44 (Id, V);
   end Set_Is_Pure;

   procedure Set_Is_Pure_Unit_Access_Type (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Access_Type (Id));
      Set_Flag189 (Id, V);
   end Set_Is_Pure_Unit_Access_Type;

   procedure Set_Is_RACW_Stub_Type (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag244 (Id, V);
   end Set_Is_RACW_Stub_Type;

   procedure Set_Is_Raised (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind (Id) = E_Exception);
      Set_Flag224 (Id, V);
   end Set_Is_Raised;

   procedure Set_Is_Remote_Call_Interface (Id : E; V : B := True) is
   begin
      Set_Flag62 (Id, V);
   end Set_Is_Remote_Call_Interface;

   procedure Set_Is_Remote_Types (Id : E; V : B := True) is
   begin
      Set_Flag61 (Id, V);
   end Set_Is_Remote_Types;

   procedure Set_Is_Renaming_Of_Object (Id : E; V : B := True) is
   begin
      Set_Flag112 (Id, V);
   end Set_Is_Renaming_Of_Object;

   procedure Set_Is_Return_Object (Id : E; V : B := True) is
   begin
      Set_Flag209 (Id, V);
   end Set_Is_Return_Object;

   procedure Set_Is_Safe_To_Reevaluate (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind (Id) = E_Variable);
      Set_Flag249 (Id, V);
   end Set_Is_Safe_To_Reevaluate;

   procedure Set_Is_Shared_Passive (Id : E; V : B := True) is
   begin
      Set_Flag60 (Id, V);
   end Set_Is_Shared_Passive;

   procedure Set_Is_Static_Type (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag281 (Id, V);
   end Set_Is_Static_Type;

   procedure Set_Is_Statically_Allocated (Id : E; V : B := True) is
   begin
      pragma Assert
        (Is_Type (Id)
           or else Ekind_In (Id, E_Exception,
                                 E_Variable,
                                 E_Constant,
                                 E_Void));
      Set_Flag28 (Id, V);
   end Set_Is_Statically_Allocated;

   procedure Set_Is_Tag (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind_In (Id, E_Component, E_Constant, E_Variable));
      Set_Flag78 (Id, V);
   end Set_Is_Tag;

   procedure Set_Is_Tagged_Type (Id : E; V : B := True) is
   begin
      Set_Flag55 (Id, V);
   end Set_Is_Tagged_Type;

   procedure Set_Is_Thunk (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Subprogram (Id));
      Set_Flag225 (Id, V);
   end Set_Is_Thunk;

   procedure Set_Is_Trivial_Subprogram (Id : E; V : B := True) is
   begin
      Set_Flag235 (Id, V);
   end Set_Is_Trivial_Subprogram;

   procedure Set_Is_True_Constant (Id : E; V : B := True) is
   begin
      Set_Flag163 (Id, V);
   end Set_Is_True_Constant;

   procedure Set_Is_Unchecked_Union (Id : E; V : B := True) is
   begin
      pragma Assert (Id = Base_Type (Id));
      Set_Flag117 (Id, V);
   end Set_Is_Unchecked_Union;

   procedure Set_Is_Underlying_Record_View (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind (Id) = E_Record_Type);
      Set_Flag246 (Id, V);
   end Set_Is_Underlying_Record_View;

   procedure Set_Is_Unimplemented (Id : E; V : B := True) is
   begin
      Set_Flag284 (Id, V);
   end Set_Is_Unimplemented;

   procedure Set_Is_Unsigned_Type (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Discrete_Or_Fixed_Point_Type (Id));
      Set_Flag144 (Id, V);
   end Set_Is_Unsigned_Type;

   procedure Set_Is_Uplevel_Referenced_Entity (Id : E; V : B := True) is
   begin
      pragma Assert
        (Ekind_In (Id, E_Constant, E_Variable)
          or else Is_Formal (Id)
          or else Is_Type (Id));
      Set_Flag283 (Id, V);
   end Set_Is_Uplevel_Referenced_Entity;

   procedure Set_Is_Valued_Procedure (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind (Id) = E_Procedure);
      Set_Flag127 (Id, V);
   end Set_Is_Valued_Procedure;

   procedure Set_Is_Visible_Formal (Id : E; V : B := True) is
   begin
      Set_Flag206 (Id, V);
   end Set_Is_Visible_Formal;

   procedure Set_Is_Visible_Lib_Unit (Id : E; V : B := True) is
   begin
      Set_Flag116 (Id, V);
   end Set_Is_Visible_Lib_Unit;

   procedure Set_Is_Volatile (Id : E; V : B := True) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Flag16 (Id, V);
   end Set_Is_Volatile;

   procedure Set_Is_Volatile_Full_Access (Id : E; V : B := True) is
   begin
      Set_Flag285 (Id, V);
   end Set_Is_Volatile_Full_Access;

   procedure Set_Itype_Printed (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Itype (Id));
      Set_Flag202 (Id, V);
   end Set_Itype_Printed;

   procedure Set_Kill_Elaboration_Checks (Id : E; V : B := True) is
   begin
      Set_Flag32 (Id, V);
   end Set_Kill_Elaboration_Checks;

   procedure Set_Kill_Range_Checks (Id : E; V : B := True) is
   begin
      Set_Flag33 (Id, V);
   end Set_Kill_Range_Checks;

   procedure Set_Known_To_Have_Preelab_Init (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag207 (Id, V);
   end Set_Known_To_Have_Preelab_Init;

   procedure Set_Last_Aggregate_Assignment (Id : E; V : N) is
   begin
      pragma Assert (Ekind_In (Id, E_Constant, E_Variable));
      Set_Node30 (Id, V);
   end Set_Last_Aggregate_Assignment;

   procedure Set_Last_Assignment (Id : E; V : N) is
   begin
      pragma Assert (Is_Assignable (Id));
      Set_Node26 (Id, V);
   end Set_Last_Assignment;

   procedure Set_Last_Entity (Id : E; V : E) is
   begin
      Set_Node20 (Id, V);
   end Set_Last_Entity;

   procedure Set_Limited_View (Id : E; V : E) is
   begin
      pragma Assert (Ekind (Id) = E_Package);
      Set_Node23 (Id, V);
   end Set_Limited_View;

   procedure Set_Linker_Section_Pragma (Id : E; V : N) is
   begin
      pragma Assert (Is_Type (Id)
        or else Ekind_In (Id, E_Constant, E_Variable)
        or else Is_Subprogram (Id));
      Set_Node33 (Id, V);
   end Set_Linker_Section_Pragma;

   procedure Set_Lit_Indexes (Id : E; V : E) is
   begin
      pragma Assert (Is_Enumeration_Type (Id) and then Root_Type (Id) = Id);
      Set_Node18 (Id, V);
   end Set_Lit_Indexes;

   procedure Set_Lit_Strings (Id : E; V : E) is
   begin
      pragma Assert (Is_Enumeration_Type (Id) and then Root_Type (Id) = Id);
      Set_Node16 (Id, V);
   end Set_Lit_Strings;

   procedure Set_Low_Bound_Tested (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Formal (Id));
      Set_Flag205 (Id, V);
   end Set_Low_Bound_Tested;

   procedure Set_Machine_Radix_10 (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Decimal_Fixed_Point_Type (Id));
      Set_Flag84 (Id, V);
   end Set_Machine_Radix_10;

   procedure Set_Master_Id (Id : E; V : E) is
   begin
      pragma Assert (Is_Access_Type (Id));
      Set_Node17 (Id, V);
   end Set_Master_Id;

   procedure Set_Materialize_Entity (Id : E; V : B := True) is
   begin
      Set_Flag168 (Id, V);
   end Set_Materialize_Entity;

   procedure Set_May_Inherit_Delayed_Rep_Aspects (Id : E; V : B := True) is
   begin
      Set_Flag262 (Id, V);
   end Set_May_Inherit_Delayed_Rep_Aspects;

   procedure Set_Mechanism (Id : E; V : M) is
   begin
      pragma Assert (Ekind (Id) = E_Function or else Is_Formal (Id));
      Set_Uint8 (Id, UI_From_Int (V));
   end Set_Mechanism;

   procedure Set_Modulus (Id : E; V : U) is
   begin
      pragma Assert (Ekind (Id) = E_Modular_Integer_Type);
      Set_Uint17 (Id, V);
   end Set_Modulus;

   procedure Set_Must_Be_On_Byte_Boundary (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag183 (Id, V);
   end Set_Must_Be_On_Byte_Boundary;

   procedure Set_Must_Have_Preelab_Init (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag208 (Id, V);
   end Set_Must_Have_Preelab_Init;

   procedure Set_Needs_Debug_Info (Id : E; V : B := True) is
   begin
      Set_Flag147 (Id, V);
   end Set_Needs_Debug_Info;

   procedure Set_Needs_No_Actuals (Id : E; V : B := True) is
   begin
      pragma Assert
        (Is_Overloadable (Id)
           or else Ekind_In (Id, E_Subprogram_Type, E_Entry_Family));
      Set_Flag22 (Id, V);
   end Set_Needs_No_Actuals;

   procedure Set_Never_Set_In_Source (Id : E; V : B := True) is
   begin
      Set_Flag115 (Id, V);
   end Set_Never_Set_In_Source;

   procedure Set_Next_Inlined_Subprogram (Id : E; V : E) is
   begin
      Set_Node12 (Id, V);
   end Set_Next_Inlined_Subprogram;

   procedure Set_No_Dynamic_Predicate_On_Actual (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Discrete_Type (Id));
      Set_Flag276 (Id, V);
   end Set_No_Dynamic_Predicate_On_Actual;

   procedure Set_No_Pool_Assigned (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Access_Type (Id) and then Is_Base_Type (Id));
      Set_Flag131 (Id, V);
   end Set_No_Pool_Assigned;

   procedure Set_No_Predicate_On_Actual (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Discrete_Type (Id));
      Set_Flag275 (Id, V);
   end Set_No_Predicate_On_Actual;

   procedure Set_No_Return (Id : E; V : B := True) is
   begin
      pragma Assert
        (V = False or else Ekind_In (Id, E_Procedure, E_Generic_Procedure));
      Set_Flag113 (Id, V);
   end Set_No_Return;

   procedure Set_No_Strict_Aliasing (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Access_Type (Id) and then Is_Base_Type (Id));
      Set_Flag136 (Id, V);
   end Set_No_Strict_Aliasing;

   procedure Set_No_Tagged_Streams_Pragma (Id : E; V : E) is
   begin
      pragma Assert (Is_Tagged_Type (Id));
      Set_Node32 (Id, V);
   end Set_No_Tagged_Streams_Pragma;

   procedure Set_Non_Binary_Modulus (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id) and then Is_Base_Type (Id));
      Set_Flag58 (Id, V);
   end Set_Non_Binary_Modulus;

   procedure Set_Non_Limited_View (Id : E; V : E) is
   begin
      pragma Assert
        (Ekind (Id) in Incomplete_Kind
          or else Ekind_In (Id, E_Abstract_State, E_Class_Wide_Type));
      Set_Node19 (Id, V);
   end Set_Non_Limited_View;

   procedure Set_Nonzero_Is_True (Id : E; V : B := True) is
   begin
      pragma Assert
        (Root_Type (Id) = Standard_Boolean
          and then Ekind (Id) = E_Enumeration_Type);
      Set_Flag162 (Id, V);
   end Set_Nonzero_Is_True;

   procedure Set_Normalized_First_Bit (Id : E; V : U) is
   begin
      pragma Assert (Ekind_In (Id, E_Component, E_Discriminant));
      Set_Uint8 (Id, V);
   end Set_Normalized_First_Bit;

   procedure Set_Normalized_Position (Id : E; V : U) is
   begin
      pragma Assert (Ekind_In (Id, E_Component, E_Discriminant));
      Set_Uint14 (Id, V);
   end Set_Normalized_Position;

   procedure Set_Normalized_Position_Max (Id : E; V : U) is
   begin
      pragma Assert (Ekind_In (Id, E_Component, E_Discriminant));
      Set_Uint10 (Id, V);
   end Set_Normalized_Position_Max;

   procedure Set_OK_To_Rename (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind (Id) = E_Variable);
      Set_Flag247 (Id, V);
   end Set_OK_To_Rename;

   procedure Set_OK_To_Reorder_Components (Id : E; V : B := True) is
   begin
      pragma Assert
        (Is_Record_Type (Id) and then Is_Base_Type (Id));
      Set_Flag239 (Id, V);
   end Set_OK_To_Reorder_Components;

   procedure Set_Optimize_Alignment_Space (Id : E; V : B := True) is
   begin
      pragma Assert
        (Is_Type (Id) or else Ekind_In (Id, E_Constant, E_Variable));
      Set_Flag241 (Id, V);
   end Set_Optimize_Alignment_Space;

   procedure Set_Optimize_Alignment_Time (Id : E; V : B := True) is
   begin
      pragma Assert
        (Is_Type (Id) or else Ekind_In (Id, E_Constant, E_Variable));
      Set_Flag242 (Id, V);
   end Set_Optimize_Alignment_Time;

   procedure Set_Original_Access_Type (Id : E; V : E) is
   begin
      pragma Assert (Ekind (Id) = E_Access_Subprogram_Type);
      Set_Node28 (Id, V);
   end Set_Original_Access_Type;

   procedure Set_Original_Array_Type (Id : E; V : E) is
   begin
      pragma Assert (Is_Array_Type (Id) or else Is_Modular_Integer_Type (Id));
      Set_Node21 (Id, V);
   end Set_Original_Array_Type;

   procedure Set_Original_Record_Component (Id : E; V : E) is
   begin
      pragma Assert (Ekind_In (Id, E_Void, E_Component, E_Discriminant));
      Set_Node22 (Id, V);
   end Set_Original_Record_Component;

   procedure Set_Overlays_Constant (Id : E; V : B := True) is
   begin
      Set_Flag243 (Id, V);
   end Set_Overlays_Constant;

   procedure Set_Overridden_Operation (Id : E; V : E) is
   begin
      pragma Assert (Is_Subprogram (Id) or else Is_Generic_Subprogram (Id));
      Set_Node26 (Id, V);
   end Set_Overridden_Operation;

   procedure Set_Package_Instantiation (Id : E; V : N) is
   begin
      pragma Assert (Ekind_In (Id, E_Void, E_Generic_Package, E_Package));
      Set_Node26 (Id, V);
   end Set_Package_Instantiation;

   procedure Set_Packed_Array_Impl_Type (Id : E; V : E) is
   begin
      pragma Assert (Is_Array_Type (Id));
      Set_Node23 (Id, V);
   end Set_Packed_Array_Impl_Type;

   procedure Set_Parent_Subtype (Id : E; V : E) is
   begin
      pragma Assert (Ekind (Id) = E_Record_Type);
      Set_Node19 (Id, V);
   end Set_Parent_Subtype;

   procedure Set_Part_Of_Constituents (Id : E; V : L) is
   begin
      pragma Assert (Ekind_In (Id, E_Abstract_State, E_Variable));
      Set_Elist10 (Id, V);
   end Set_Part_Of_Constituents;

   procedure Set_Part_Of_References (Id : E; V : L) is
   begin
      pragma Assert (Ekind (Id) = E_Variable);
      Set_Elist11 (Id, V);
   end Set_Part_Of_References;

   procedure Set_Partial_View_Has_Unknown_Discr (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag280 (Id, V);
   end Set_Partial_View_Has_Unknown_Discr;

   procedure Set_Pending_Access_Types (Id : E; V : L) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Elist15 (Id, V);
   end Set_Pending_Access_Types;

   procedure Set_Postconditions_Proc (Id : E; V : E) is
   begin
      pragma Assert (Ekind_In (Id, E_Entry,
                                   E_Entry_Family,
                                   E_Function,
                                   E_Procedure));
      Set_Node14 (Id, V);
   end Set_Postconditions_Proc;

   procedure Set_Direct_Primitive_Operations (Id : E; V : L) is
   begin
      pragma Assert (Is_Tagged_Type (Id));
      Set_Elist10 (Id, V);
   end Set_Direct_Primitive_Operations;

   procedure Set_Prival (Id : E; V : E) is
   begin
      pragma Assert (Is_Protected_Component (Id));
      Set_Node17 (Id, V);
   end Set_Prival;

   procedure Set_Prival_Link (Id : E; V : E) is
   begin
      pragma Assert (Ekind_In (Id, E_Constant, E_Variable));
      Set_Node20 (Id, V);
   end Set_Prival_Link;

   procedure Set_Private_Dependents (Id : E; V : L) is
   begin
      pragma Assert (Is_Incomplete_Or_Private_Type (Id));
      Set_Elist18 (Id, V);
   end Set_Private_Dependents;

   procedure Set_Private_View (Id : E; V : N) is
   begin
      pragma Assert (Is_Private_Type (Id));
      Set_Node22 (Id, V);
   end Set_Private_View;

   procedure Set_Protected_Body_Subprogram (Id : E; V : E) is
   begin
      pragma Assert (Is_Subprogram (Id) or else Is_Entry (Id));
      Set_Node11 (Id, V);
   end Set_Protected_Body_Subprogram;

   procedure Set_Protected_Formal (Id : E; V : E) is
   begin
      pragma Assert (Is_Formal (Id));
      Set_Node22 (Id, V);
   end Set_Protected_Formal;

   procedure Set_Protection_Object (Id : E; V : E) is
   begin
      pragma Assert (Ekind_In (Id, E_Entry,
                                   E_Entry_Family,
                                   E_Function,
                                   E_Procedure));
      Set_Node23 (Id, V);
   end Set_Protection_Object;

   procedure Set_Reachable (Id : E; V : B := True) is
   begin
      Set_Flag49 (Id, V);
   end Set_Reachable;

   procedure Set_Referenced (Id : E; V : B := True) is
   begin
      Set_Flag156 (Id, V);
   end Set_Referenced;

   procedure Set_Referenced_As_LHS (Id : E; V : B := True) is
   begin
      Set_Flag36 (Id, V);
   end Set_Referenced_As_LHS;

   procedure Set_Referenced_As_Out_Parameter (Id : E; V : B := True) is
   begin
      Set_Flag227 (Id, V);
   end Set_Referenced_As_Out_Parameter;

   procedure Set_Refinement_Constituents (Id : E; V : L) is
   begin
      pragma Assert (Ekind (Id) = E_Abstract_State);
      Set_Elist8 (Id, V);
   end Set_Refinement_Constituents;

   procedure Set_Register_Exception_Call (Id : E; V : N) is
   begin
      pragma Assert (Ekind (Id) = E_Exception);
      Set_Node20 (Id, V);
   end Set_Register_Exception_Call;

   procedure Set_Related_Array_Object (Id : E; V : E) is
   begin
      pragma Assert (Is_Array_Type (Id));
      Set_Node25 (Id, V);
   end Set_Related_Array_Object;

   procedure Set_Related_Expression (Id : E; V : N) is
   begin
      pragma Assert (Ekind (Id) in Type_Kind
                       or else Ekind_In (Id, E_Constant, E_Variable, E_Void));
      Set_Node24 (Id, V);
   end Set_Related_Expression;

   procedure Set_Related_Instance (Id : E; V : E) is
   begin
      pragma Assert (Ekind_In (Id, E_Package, E_Package_Body));
      Set_Node15 (Id, V);
   end Set_Related_Instance;

   procedure Set_Related_Type (Id : E; V : E) is
   begin
      pragma Assert (Ekind_In (Id, E_Component, E_Constant, E_Variable));
      Set_Node27 (Id, V);
   end Set_Related_Type;

   procedure Set_Relative_Deadline_Variable (Id : E; V : E) is
   begin
      pragma Assert (Is_Task_Type (Id) and then Is_Base_Type (Id));
      Set_Node28 (Id, V);
   end Set_Relative_Deadline_Variable;

   procedure Set_Renamed_Entity (Id : E; V : N) is
   begin
      Set_Node18 (Id, V);
   end Set_Renamed_Entity;

   procedure Set_Renamed_In_Spec (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind (Id) = E_Package);
      Set_Flag231 (Id, V);
   end Set_Renamed_In_Spec;

   procedure Set_Renamed_Object (Id : E; V : N) is
   begin
      Set_Node18 (Id, V);
   end Set_Renamed_Object;

   procedure Set_Renaming_Map (Id : E; V : U) is
   begin
      Set_Uint9 (Id, V);
   end Set_Renaming_Map;

   procedure Set_Requires_Overriding (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Overloadable (Id));
      Set_Flag213 (Id, V);
   end Set_Requires_Overriding;

   procedure Set_Return_Present (Id : E; V : B := True) is
   begin
      Set_Flag54 (Id, V);
   end Set_Return_Present;

   procedure Set_Return_Applies_To (Id : E; V : N) is
   begin
      Set_Node8 (Id, V);
   end Set_Return_Applies_To;

   procedure Set_Returns_By_Ref (Id : E; V : B := True) is
   begin
      Set_Flag90 (Id, V);
   end Set_Returns_By_Ref;

   procedure Set_Returns_Limited_View (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind (Id) = E_Function);
      Set_Flag134 (Id, V);
   end Set_Returns_Limited_View;

   procedure Set_Reverse_Bit_Order (Id : E; V : B := True) is
   begin
      pragma Assert
        (Is_Record_Type (Id) and then Is_Base_Type (Id));
      Set_Flag164 (Id, V);
   end Set_Reverse_Bit_Order;

   procedure Set_Reverse_Storage_Order (Id : E; V : B := True) is
   begin
      pragma Assert
        (Is_Base_Type (Id)
           and then (Is_Record_Type (Id) or else Is_Array_Type (Id)));
      Set_Flag93 (Id, V);
   end Set_Reverse_Storage_Order;

   procedure Set_Rewritten_For_C (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind (Id) = E_Function);
      Set_Flag287 (Id, V);
   end Set_Rewritten_For_C;

   procedure Set_RM_Size (Id : E; V : U) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Uint13 (Id, V);
   end Set_RM_Size;

   procedure Set_Scalar_Range (Id : E; V : N) is
   begin
      Set_Node20 (Id, V);
   end Set_Scalar_Range;

   procedure Set_Scale_Value (Id : E; V : U) is
   begin
      Set_Uint16 (Id, V);
   end Set_Scale_Value;

   procedure Set_Scope_Depth_Value (Id : E; V : U) is
   begin
      pragma Assert (not Is_Record_Type (Id));
      Set_Uint22 (Id, V);
   end Set_Scope_Depth_Value;

   procedure Set_Sec_Stack_Needed_For_Return (Id : E; V : B := True) is
   begin
      Set_Flag167 (Id, V);
   end Set_Sec_Stack_Needed_For_Return;

   procedure Set_Shadow_Entities (Id : E; V : S) is
   begin
      pragma Assert (Ekind_In (Id, E_Package, E_Generic_Package));
      Set_List14 (Id, V);
   end Set_Shadow_Entities;

   procedure Set_Shared_Var_Procs_Instance (Id : E; V : E) is
   begin
      pragma Assert (Ekind (Id) = E_Variable);
      Set_Node22 (Id, V);
   end Set_Shared_Var_Procs_Instance;

   procedure Set_Size_Check_Code (Id : E; V : N) is
   begin
      pragma Assert (Ekind_In (Id, E_Constant, E_Variable));
      Set_Node19 (Id, V);
   end Set_Size_Check_Code;

   procedure Set_Size_Depends_On_Discriminant (Id : E; V : B := True) is
   begin
      Set_Flag177 (Id, V);
   end Set_Size_Depends_On_Discriminant;

   procedure Set_Size_Known_At_Compile_Time (Id : E; V : B := True) is
   begin
      Set_Flag92 (Id, V);
   end Set_Size_Known_At_Compile_Time;

   procedure Set_Small_Value (Id : E; V : R) is
   begin
      pragma Assert (Is_Fixed_Point_Type (Id));
      Set_Ureal21 (Id, V);
   end Set_Small_Value;

   procedure Set_SPARK_Aux_Pragma (Id : E; V : N) is
   begin
      pragma Assert
        (Ekind_In (Id, E_Protected_Type,   --  concurrent variants
                       E_Task_Type)
           or else
         Ekind_In (Id, E_Generic_Package,  --  package variants
                       E_Package,
                       E_Package_Body));
      Set_Node41 (Id, V);
   end Set_SPARK_Aux_Pragma;

   procedure Set_SPARK_Aux_Pragma_Inherited (Id : E; V : B := True) is
   begin
      pragma Assert
        (Ekind_In (Id, E_Protected_Type,   --  concurrent variants
                       E_Task_Type)
           or else
         Ekind_In (Id, E_Generic_Package,  --  package variants
                       E_Package,
                       E_Package_Body));
      Set_Flag266 (Id, V);
   end Set_SPARK_Aux_Pragma_Inherited;

   procedure Set_SPARK_Pragma (Id : E; V : N) is
   begin
      pragma Assert
        (Ekind_In (Id, E_Protected_Body,   --  concurrent variants
                       E_Protected_Type,
                       E_Task_Body,
                       E_Task_Type)
           or else
         Ekind_In (Id, E_Entry,            --  overloadable variants
                       E_Entry_Family,
                       E_Function,
                       E_Generic_Function,
                       E_Generic_Procedure,
                       E_Operator,
                       E_Procedure,
                       E_Subprogram_Body)
           or else
         Ekind_In (Id, E_Generic_Package,  --  package variants
                       E_Package,
                       E_Package_Body)
           or else
         Ekind (Id) = E_Variable);         --  variable
      Set_Node40 (Id, V);
   end Set_SPARK_Pragma;

   procedure Set_SPARK_Pragma_Inherited (Id : E; V : B := True) is
   begin
      pragma Assert
        (Ekind_In (Id, E_Protected_Body,   --  concurrent variants
                       E_Protected_Type,
                       E_Task_Body,
                       E_Task_Type)
           or else
         Ekind_In (Id, E_Entry,            --  overloadable variants
                       E_Entry_Family,
                       E_Function,
                       E_Generic_Function,
                       E_Generic_Procedure,
                       E_Operator,
                       E_Procedure,
                       E_Subprogram_Body)
           or else
         Ekind_In (Id, E_Generic_Package,  --  package variants
                       E_Package,
                       E_Package_Body)
           or else
         Ekind (Id) = E_Variable);         --  variable
      Set_Flag265 (Id, V);
   end Set_SPARK_Pragma_Inherited;

   procedure Set_Spec_Entity (Id : E; V : E) is
   begin
      pragma Assert (Ekind (Id) = E_Package_Body or else Is_Formal (Id));
      Set_Node19 (Id, V);
   end Set_Spec_Entity;

   procedure Set_SSO_Set_High_By_Default (Id : E; V : B := True) is
   begin
      pragma Assert
        (Is_Base_Type (Id)
         and then (Is_Record_Type (Id) or else Is_Array_Type (Id)));
      Set_Flag273 (Id, V);
   end Set_SSO_Set_High_By_Default;

   procedure Set_SSO_Set_Low_By_Default (Id : E; V : B := True) is
   begin
      pragma Assert
        (Is_Base_Type (Id)
         and then (Is_Record_Type (Id) or else Is_Array_Type (Id)));
      Set_Flag272 (Id, V);
   end Set_SSO_Set_Low_By_Default;

   procedure Set_Static_Discrete_Predicate (Id : E; V : S) is
   begin
      pragma Assert (Is_Discrete_Type (Id) and then Has_Predicates (Id));
      Set_List25 (Id, V);
   end Set_Static_Discrete_Predicate;

   procedure Set_Static_Real_Or_String_Predicate (Id : E; V : N) is
   begin
      pragma Assert ((Is_Real_Type (Id) or else Is_String_Type (Id))
                      and then Has_Predicates (Id));
      Set_Node25 (Id, V);
   end Set_Static_Real_Or_String_Predicate;

   procedure Set_Status_Flag_Or_Transient_Decl (Id : E; V : E) is
   begin
      pragma Assert (Ekind_In (Id, E_Constant, E_Variable));
      Set_Node15 (Id, V);
   end Set_Status_Flag_Or_Transient_Decl;

   procedure Set_Storage_Size_Variable (Id : E; V : E) is
   begin
      pragma Assert (Is_Access_Type (Id) or else Is_Task_Type (Id));
      pragma Assert (Id = Base_Type (Id));
      Set_Node26 (Id, V);
   end Set_Storage_Size_Variable;

   procedure Set_Static_Elaboration_Desired (Id : E; V : B) is
   begin
      pragma Assert (Ekind (Id) = E_Package);
      Set_Flag77 (Id, V);
   end Set_Static_Elaboration_Desired;

   procedure Set_Static_Initialization (Id : E; V : N) is
   begin
      pragma Assert
        (Ekind (Id) = E_Procedure and then not Is_Dispatching_Operation (Id));
      Set_Node30 (Id, V);
   end Set_Static_Initialization;

   procedure Set_Stored_Constraint (Id : E; V : L) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Elist23 (Id, V);
   end Set_Stored_Constraint;

   procedure Set_Stores_Attribute_Old_Prefix (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind (Id) = E_Constant);
      Set_Flag270 (Id, V);
   end Set_Stores_Attribute_Old_Prefix;

   procedure Set_Strict_Alignment (Id : E; V : B := True) is
   begin
      pragma Assert (Id = Base_Type (Id));
      Set_Flag145 (Id, V);
   end Set_Strict_Alignment;

   procedure Set_String_Literal_Length (Id : E; V : U) is
   begin
      pragma Assert (Ekind (Id) = E_String_Literal_Subtype);
      Set_Uint16 (Id, V);
   end Set_String_Literal_Length;

   procedure Set_String_Literal_Low_Bound (Id : E; V : N) is
   begin
      pragma Assert (Ekind (Id) = E_String_Literal_Subtype);
      Set_Node18 (Id, V);
   end Set_String_Literal_Low_Bound;

   procedure Set_Subprograms_For_Type (Id : E; V : E) is
   begin
      pragma Assert (Is_Type (Id) or else Is_Subprogram (Id));
      Set_Node29 (Id, V);
   end Set_Subprograms_For_Type;

   procedure Set_Subps_Index (Id : E; V : U) is
   begin
      pragma Assert (Is_Subprogram (Id));
      Set_Uint24 (Id, V);
   end Set_Subps_Index;

   procedure Set_Suppress_Elaboration_Warnings (Id : E; V : B := True) is
   begin
      Set_Flag148 (Id, V);
   end Set_Suppress_Elaboration_Warnings;

   procedure Set_Suppress_Initialization (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id) or else Ekind (Id) = E_Variable);
      Set_Flag105 (Id, V);
   end Set_Suppress_Initialization;

   procedure Set_Suppress_Style_Checks (Id : E; V : B := True) is
   begin
      Set_Flag165 (Id, V);
   end Set_Suppress_Style_Checks;

   procedure Set_Suppress_Value_Tracking_On_Call (Id : E; V : B := True) is
   begin
      Set_Flag217 (Id, V);
   end Set_Suppress_Value_Tracking_On_Call;

   procedure Set_Task_Body_Procedure (Id : E; V : N) is
   begin
      pragma Assert (Ekind (Id) in Task_Kind);
      Set_Node25 (Id, V);
   end Set_Task_Body_Procedure;

   procedure Set_Thunk_Entity (Id : E; V : E) is
   begin
      pragma Assert (Ekind_In (Id, E_Function, E_Procedure)
                       and then Is_Thunk (Id));
      Set_Node31 (Id, V);
   end Set_Thunk_Entity;

   procedure Set_Treat_As_Volatile (Id : E; V : B := True) is
   begin
      Set_Flag41 (Id, V);
   end Set_Treat_As_Volatile;

   procedure Set_Underlying_Full_View (Id : E; V : E) is
   begin
      pragma Assert (Ekind (Id) in Private_Kind);
      Set_Node19 (Id, V);
   end Set_Underlying_Full_View;

   procedure Set_Underlying_Record_View (Id : E; V : E) is
   begin
      pragma Assert (Ekind (Id) = E_Record_Type);
      Set_Node28 (Id, V);
   end Set_Underlying_Record_View;

   procedure Set_Universal_Aliasing (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id) and then Is_Base_Type (Id));
      Set_Flag216 (Id, V);
   end Set_Universal_Aliasing;

   procedure Set_Unset_Reference (Id : E; V : N) is
   begin
      Set_Node16 (Id, V);
   end Set_Unset_Reference;

   procedure Set_Used_As_Generic_Actual (Id : E; V : B := True) is
   begin
      Set_Flag222 (Id, V);
   end Set_Used_As_Generic_Actual;

   procedure Set_Uses_Lock_Free (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind (Id) = E_Protected_Type);
      Set_Flag188 (Id, V);
   end Set_Uses_Lock_Free;

   procedure Set_Uses_Sec_Stack (Id : E; V : B := True) is
   begin
      Set_Flag95 (Id, V);
   end Set_Uses_Sec_Stack;

   procedure Set_Warnings_Off (Id : E; V : B := True) is
   begin
      Set_Flag96 (Id, V);
   end Set_Warnings_Off;

   procedure Set_Warnings_Off_Used (Id : E; V : B := True) is
   begin
      Set_Flag236 (Id, V);
   end Set_Warnings_Off_Used;

   procedure Set_Warnings_Off_Used_Unmodified (Id : E; V : B := True) is
   begin
      Set_Flag237 (Id, V);
   end Set_Warnings_Off_Used_Unmodified;

   procedure Set_Warnings_Off_Used_Unreferenced (Id : E; V : B := True) is
   begin
      Set_Flag238 (Id, V);
   end Set_Warnings_Off_Used_Unreferenced;

   procedure Set_Was_Hidden (Id : E; V : B := True) is
   begin
      Set_Flag196 (Id, V);
   end Set_Was_Hidden;

   procedure Set_Wrapped_Entity (Id : E; V : E) is
   begin
      pragma Assert (Ekind_In (Id, E_Function, E_Procedure)
                       and then Is_Primitive_Wrapper (Id));
      Set_Node27 (Id, V);
   end Set_Wrapped_Entity;

   -----------------------------------
   -- Field Initialization Routines --
   -----------------------------------

   procedure Init_Alignment (Id : E) is
   begin
      Set_Uint14 (Id, Uint_0);
   end Init_Alignment;

   procedure Init_Alignment (Id : E; V : Int) is
   begin
      Set_Uint14 (Id, UI_From_Int (V));
   end Init_Alignment;

   procedure Init_Component_Bit_Offset (Id : E) is
   begin
      Set_Uint11 (Id, No_Uint);
   end Init_Component_Bit_Offset;

   procedure Init_Component_Bit_Offset (Id : E; V : Int) is
   begin
      Set_Uint11 (Id, UI_From_Int (V));
   end Init_Component_Bit_Offset;

   procedure Init_Component_Size (Id : E) is
   begin
      Set_Uint22 (Id, Uint_0);
   end Init_Component_Size;

   procedure Init_Component_Size (Id : E; V : Int) is
   begin
      Set_Uint22 (Id, UI_From_Int (V));
   end Init_Component_Size;

   procedure Init_Digits_Value (Id : E) is
   begin
      Set_Uint17 (Id, Uint_0);
   end Init_Digits_Value;

   procedure Init_Digits_Value (Id : E; V : Int) is
   begin
      Set_Uint17 (Id, UI_From_Int (V));
   end Init_Digits_Value;

   procedure Init_Esize (Id : E) is
   begin
      Set_Uint12 (Id, Uint_0);
   end Init_Esize;

   procedure Init_Esize (Id : E; V : Int) is
   begin
      Set_Uint12 (Id, UI_From_Int (V));
   end Init_Esize;

   procedure Init_Normalized_First_Bit (Id : E) is
   begin
      Set_Uint8 (Id, No_Uint);
   end Init_Normalized_First_Bit;

   procedure Init_Normalized_First_Bit (Id : E; V : Int) is
   begin
      Set_Uint8 (Id, UI_From_Int (V));
   end Init_Normalized_First_Bit;

   procedure Init_Normalized_Position (Id : E) is
   begin
      Set_Uint14 (Id, No_Uint);
   end Init_Normalized_Position;

   procedure Init_Normalized_Position (Id : E; V : Int) is
   begin
      Set_Uint14 (Id, UI_From_Int (V));
   end Init_Normalized_Position;

   procedure Init_Normalized_Position_Max (Id : E) is
   begin
      Set_Uint10 (Id, No_Uint);
   end Init_Normalized_Position_Max;

   procedure Init_Normalized_Position_Max (Id : E; V : Int) is
   begin
      Set_Uint10 (Id, UI_From_Int (V));
   end Init_Normalized_Position_Max;

   procedure Init_RM_Size (Id : E) is
   begin
      Set_Uint13 (Id, Uint_0);
   end Init_RM_Size;

   procedure Init_RM_Size (Id : E; V : Int) is
   begin
      Set_Uint13 (Id, UI_From_Int (V));
   end Init_RM_Size;

   -----------------------------
   -- Init_Component_Location --
   -----------------------------

   procedure Init_Component_Location (Id : E) is
   begin
      Set_Uint8  (Id, No_Uint);  -- Normalized_First_Bit
      Set_Uint10 (Id, No_Uint);  -- Normalized_Position_Max
      Set_Uint11 (Id, No_Uint);  -- Component_Bit_Offset
      Set_Uint12 (Id, Uint_0);   -- Esize
      Set_Uint14 (Id, No_Uint);  -- Normalized_Position
   end Init_Component_Location;

   ----------------------------
   -- Init_Object_Size_Align --
   ----------------------------

   procedure Init_Object_Size_Align (Id : E) is
   begin
      Set_Uint12 (Id, Uint_0);  -- Esize
      Set_Uint14 (Id, Uint_0);  -- Alignment
   end Init_Object_Size_Align;

   ---------------
   -- Init_Size --
   ---------------

   procedure Init_Size (Id : E; V : Int) is
   begin
      pragma Assert (not Is_Object (Id));
      Set_Uint12 (Id, UI_From_Int (V));  -- Esize
      Set_Uint13 (Id, UI_From_Int (V));  -- RM_Size
   end Init_Size;

   ---------------------
   -- Init_Size_Align --
   ---------------------

   procedure Init_Size_Align (Id : E) is
   begin
      pragma Assert (not Is_Object (Id));
      Set_Uint12 (Id, Uint_0);  -- Esize
      Set_Uint13 (Id, Uint_0);  -- RM_Size
      Set_Uint14 (Id, Uint_0);  -- Alignment
   end Init_Size_Align;

   ----------------------------------------------
   -- Type Representation Attribute Predicates --
   ----------------------------------------------

   function Known_Alignment                       (E : Entity_Id) return B is
   begin
      return Uint14 (E) /= Uint_0
        and then Uint14 (E) /= No_Uint;
   end Known_Alignment;

   function Known_Component_Bit_Offset            (E : Entity_Id) return B is
   begin
      return Uint11 (E) /= No_Uint;
   end Known_Component_Bit_Offset;

   function Known_Component_Size                  (E : Entity_Id) return B is
   begin
      return Uint22 (Base_Type (E)) /= Uint_0
        and then Uint22 (Base_Type (E)) /= No_Uint;
   end Known_Component_Size;

   function Known_Esize                           (E : Entity_Id) return B is
   begin
      return Uint12 (E) /= Uint_0
        and then Uint12 (E) /= No_Uint;
   end Known_Esize;

   function Known_Normalized_First_Bit            (E : Entity_Id) return B is
   begin
      return Uint8 (E) /= No_Uint;
   end Known_Normalized_First_Bit;

   function Known_Normalized_Position             (E : Entity_Id) return B is
   begin
      return Uint14 (E) /= No_Uint;
   end Known_Normalized_Position;

   function Known_Normalized_Position_Max         (E : Entity_Id) return B is
   begin
      return Uint10 (E) /= No_Uint;
   end Known_Normalized_Position_Max;

   function Known_RM_Size                         (E : Entity_Id) return B is
   begin
      return Uint13 (E) /= No_Uint
        and then (Uint13 (E) /= Uint_0
                    or else Is_Discrete_Type (E)
                    or else Is_Fixed_Point_Type (E));
   end Known_RM_Size;

   function Known_Static_Component_Bit_Offset     (E : Entity_Id) return B is
   begin
      return Uint11 (E) /= No_Uint
        and then Uint11 (E) >= Uint_0;
   end Known_Static_Component_Bit_Offset;

   function Known_Static_Component_Size           (E : Entity_Id) return B is
   begin
      return Uint22 (Base_Type (E)) > Uint_0;
   end Known_Static_Component_Size;

   function Known_Static_Esize                    (E : Entity_Id) return B is
   begin
      return Uint12 (E) > Uint_0
        and then not Is_Generic_Type (E);
   end Known_Static_Esize;

   function Known_Static_Normalized_First_Bit     (E : Entity_Id) return B is
   begin
      return Uint8 (E) /= No_Uint
        and then Uint8 (E) >= Uint_0;
   end Known_Static_Normalized_First_Bit;

   function Known_Static_Normalized_Position      (E : Entity_Id) return B is
   begin
      return Uint14 (E) /= No_Uint
        and then Uint14 (E) >= Uint_0;
   end Known_Static_Normalized_Position;

   function Known_Static_Normalized_Position_Max  (E : Entity_Id) return B is
   begin
      return Uint10 (E) /= No_Uint
        and then Uint10 (E) >= Uint_0;
   end Known_Static_Normalized_Position_Max;

   function Known_Static_RM_Size                  (E : Entity_Id) return B is
   begin
      return (Uint13 (E) > Uint_0
                or else Is_Discrete_Type (E)
                or else Is_Fixed_Point_Type (E))
        and then not Is_Generic_Type (E);
   end Known_Static_RM_Size;

   function Unknown_Alignment                     (E : Entity_Id) return B is
   begin
      return Uint14 (E) = Uint_0
        or else Uint14 (E) = No_Uint;
   end Unknown_Alignment;

   function Unknown_Component_Bit_Offset          (E : Entity_Id) return B is
   begin
      return Uint11 (E) = No_Uint;
   end Unknown_Component_Bit_Offset;

   function Unknown_Component_Size                (E : Entity_Id) return B is
   begin
      return Uint22 (Base_Type (E)) = Uint_0
               or else
             Uint22 (Base_Type (E)) = No_Uint;
   end Unknown_Component_Size;

   function Unknown_Esize                         (E : Entity_Id) return B is
   begin
      return Uint12 (E) = No_Uint
               or else
             Uint12 (E) = Uint_0;
   end Unknown_Esize;

   function Unknown_Normalized_First_Bit          (E : Entity_Id) return B is
   begin
      return Uint8 (E) = No_Uint;
   end Unknown_Normalized_First_Bit;

   function Unknown_Normalized_Position           (E : Entity_Id) return B is
   begin
      return Uint14 (E) = No_Uint;
   end Unknown_Normalized_Position;

   function Unknown_Normalized_Position_Max       (E : Entity_Id) return B is
   begin
      return Uint10 (E) = No_Uint;
   end Unknown_Normalized_Position_Max;

   function Unknown_RM_Size                       (E : Entity_Id) return B is
   begin
      return (Uint13 (E) = Uint_0
                and then not Is_Discrete_Type (E)
                and then not Is_Fixed_Point_Type (E))
        or else Uint13 (E) = No_Uint;
   end Unknown_RM_Size;

   --------------------
   -- Address_Clause --
   --------------------

   function Address_Clause (Id : E) return N is
   begin
      return Get_Attribute_Definition_Clause (Id, Attribute_Address);
   end Address_Clause;

   ---------------
   -- Aft_Value --
   ---------------

   function Aft_Value (Id : E) return U is
      Result    : Nat := 1;
      Delta_Val : Ureal := Delta_Value (Id);
   begin
      while Delta_Val < Ureal_Tenth loop
         Delta_Val := Delta_Val * Ureal_10;
         Result := Result + 1;
      end loop;

      return UI_From_Int (Result);
   end Aft_Value;

   ----------------------
   -- Alignment_Clause --
   ----------------------

   function Alignment_Clause (Id : E) return N is
   begin
      return Get_Attribute_Definition_Clause (Id, Attribute_Alignment);
   end Alignment_Clause;

   -------------------
   -- Append_Entity --
   -------------------

   procedure Append_Entity (Id : Entity_Id; V : Entity_Id) is
   begin
      if Last_Entity (V) = Empty then
         Set_First_Entity (Id => V, V => Id);
      else
         Set_Next_Entity (Last_Entity (V), Id);
      end if;

      Set_Next_Entity (Id, Empty);
      Set_Scope (Id, V);
      Set_Last_Entity (Id => V, V => Id);
   end Append_Entity;

   ---------------
   -- Base_Type --
   ---------------

   function Base_Type (Id : E) return E is
   begin
      if Is_Base_Type (Id) then
         return Id;
      else
         pragma Assert (Is_Type (Id));
         return Etype (Id);
      end if;
   end Base_Type;

   -------------------------
   -- Component_Alignment --
   -------------------------

   --  Component Alignment is encoded using two flags, Flag128/129 as
   --  follows. Note that both flags False = Align_Default, so that the
   --  default initialization of flags to False initializes component
   --  alignment to the default value as required.

   --     Flag128      Flag129      Value
   --     -------      -------      -----
   --      False        False       Calign_Default
   --      False        True        Calign_Component_Size
   --      True         False       Calign_Component_Size_4
   --      True         True        Calign_Storage_Unit

   function Component_Alignment (Id : E) return C is
      BT : constant Node_Id := Base_Type (Id);

   begin
      pragma Assert (Is_Array_Type (Id) or else Is_Record_Type (Id));

      if Flag128 (BT) then
         if Flag129 (BT) then
            return Calign_Storage_Unit;
         else
            return Calign_Component_Size_4;
         end if;

      else
         if Flag129 (BT) then
            return Calign_Component_Size;
         else
            return Calign_Default;
         end if;
      end if;
   end Component_Alignment;

   ----------------------
   -- Declaration_Node --
   ----------------------

   function Declaration_Node (Id : E) return N is
      P : Node_Id;

   begin
      if Ekind (Id) = E_Incomplete_Type
        and then Present (Full_View (Id))
      then
         P := Parent (Full_View (Id));
      else
         P := Parent (Id);
      end if;

      loop
         if Nkind (P) /= N_Selected_Component
           and then Nkind (P) /= N_Expanded_Name
           and then
             not (Nkind (P) = N_Defining_Program_Unit_Name
                   and then Is_Child_Unit (Id))
         then
            return P;
         else
            P := Parent (P);
         end if;
      end loop;
   end Declaration_Node;

   ---------------------------------
   -- Default_Init_Cond_Procedure --
   ---------------------------------

   function Default_Init_Cond_Procedure (Id : E) return E is
      Subp_Id : Entity_Id;

   begin
      pragma Assert
        (Is_Type (Id)
          and then (Has_Default_Init_Cond (Id)
                     or Has_Inherited_Default_Init_Cond (Id)));

      Subp_Id := Subprograms_For_Type (Base_Type (Id));
      while Present (Subp_Id) loop
         if Is_Default_Init_Cond_Procedure (Subp_Id) then
            return Subp_Id;
         end if;

         Subp_Id := Subprograms_For_Type (Subp_Id);
      end loop;

      return Empty;
   end Default_Init_Cond_Procedure;

   ---------------------
   -- Designated_Type --
   ---------------------

   function Designated_Type (Id : E) return E is
      Desig_Type : E;

   begin
      Desig_Type := Directly_Designated_Type (Id);

      if Ekind (Desig_Type) = E_Incomplete_Type
        and then Present (Full_View (Desig_Type))
      then
         return Full_View (Desig_Type);

      elsif Is_Class_Wide_Type (Desig_Type)
        and then Ekind (Etype (Desig_Type)) = E_Incomplete_Type
        and then Present (Full_View (Etype (Desig_Type)))
        and then Present (Class_Wide_Type (Full_View (Etype (Desig_Type))))
      then
         return Class_Wide_Type (Full_View (Etype (Desig_Type)));

      else
         return Desig_Type;
      end if;
   end Designated_Type;

   ----------------------
   -- Entry_Index_Type --
   ----------------------

   function Entry_Index_Type (Id : E) return N is
   begin
      pragma Assert (Ekind (Id) = E_Entry_Family);
      return Etype (Discrete_Subtype_Definition (Parent (Id)));
   end Entry_Index_Type;

   ---------------------
   -- First_Component --
   ---------------------

   function First_Component (Id : E) return E is
      Comp_Id : E;

   begin
      pragma Assert
        (Is_Concurrent_Type (Id)
          or else Is_Incomplete_Or_Private_Type (Id)
          or else Is_Record_Type (Id));

      Comp_Id := First_Entity (Id);
      while Present (Comp_Id) loop
         exit when Ekind (Comp_Id) = E_Component;
         Comp_Id := Next_Entity (Comp_Id);
      end loop;

      return Comp_Id;
   end First_Component;

   -------------------------------------
   -- First_Component_Or_Discriminant --
   -------------------------------------

   function First_Component_Or_Discriminant (Id : E) return E is
      Comp_Id : E;

   begin
      pragma Assert
        (Is_Concurrent_Type (Id)
          or else Is_Incomplete_Or_Private_Type (Id)
          or else Is_Record_Type (Id)
          or else Has_Discriminants (Id));

      Comp_Id := First_Entity (Id);
      while Present (Comp_Id) loop
         exit when Ekind_In (Comp_Id, E_Component, E_Discriminant);
         Comp_Id := Next_Entity (Comp_Id);
      end loop;

      return Comp_Id;
   end First_Component_Or_Discriminant;

   ------------------
   -- First_Formal --
   ------------------

   function First_Formal (Id : E) return E is
      Formal : E;

   begin
      pragma Assert
        (Is_Generic_Subprogram (Id)
           or else Is_Overloadable (Id)
           or else Ekind_In (Id, E_Entry_Family,
                                 E_Subprogram_Body,
                                 E_Subprogram_Type));

      if Ekind (Id) = E_Enumeration_Literal then
         return Empty;

      else
         Formal := First_Entity (Id);

         --  Deal with the common, non-generic case first

         if No (Formal) or else Is_Formal (Formal) then
            return Formal;
         end if;

         --  The first/next entity chain of a generic subprogram contains all
         --  generic formal parameters, followed by the formal parameters.

         if Is_Generic_Subprogram (Id) then
            while Present (Formal) and then not Is_Formal (Formal) loop
               Next_Entity (Formal);
            end loop;
            return Formal;
         else
            return Empty;
         end if;
      end if;
   end First_Formal;

   ------------------------------
   -- First_Formal_With_Extras --
   ------------------------------

   function First_Formal_With_Extras (Id : E) return E is
      Formal : E;

   begin
      pragma Assert
        (Is_Generic_Subprogram (Id)
           or else Is_Overloadable (Id)
           or else Ekind_In (Id, E_Entry_Family,
                                 E_Subprogram_Body,
                                 E_Subprogram_Type));

      if Ekind (Id) = E_Enumeration_Literal then
         return Empty;

      else
         Formal := First_Entity (Id);

         --  The first/next entity chain of a generic subprogram contains all
         --  generic formal parameters, followed by the formal parameters. Go
         --  directly to the parameters by skipping the formal part.

         if Is_Generic_Subprogram (Id) then
            while Present (Formal) and then not Is_Formal (Formal) loop
               Next_Entity (Formal);
            end loop;
         end if;

         if Present (Formal) and then Is_Formal (Formal) then
            return Formal;
         else
            return Extra_Formals (Id);  -- Empty if no extra formals
         end if;
      end if;
   end First_Formal_With_Extras;

   -------------------------------------
   -- Get_Attribute_Definition_Clause --
   -------------------------------------

   function Get_Attribute_Definition_Clause
     (E  : Entity_Id;
      Id : Attribute_Id) return Node_Id
   is
      N : Node_Id;

   begin
      N := First_Rep_Item (E);
      while Present (N) loop
         if Nkind (N) = N_Attribute_Definition_Clause
           and then Get_Attribute_Id (Chars (N)) = Id
         then
            return N;
         else
            Next_Rep_Item (N);
         end if;
      end loop;

      return Empty;
   end Get_Attribute_Definition_Clause;

   -------------------
   -- Get_Full_View --
   -------------------

   function Get_Full_View (T : Entity_Id) return Entity_Id is
   begin
      if Ekind (T) = E_Incomplete_Type and then Present (Full_View (T)) then
         return Full_View (T);

      elsif Is_Class_Wide_Type (T)
        and then Ekind (Root_Type (T)) = E_Incomplete_Type
        and then Present (Full_View (Root_Type (T)))
      then
         return Class_Wide_Type (Full_View (Root_Type (T)));

      else
         return T;
      end if;
   end Get_Full_View;

   ----------------
   -- Get_Pragma --
   ----------------

   function Get_Pragma (E : Entity_Id; Id : Pragma_Id) return Node_Id is

      --  Classification pragmas

      Is_CLS : constant Boolean :=
                 Id = Pragma_Abstract_State             or else
                 Id = Pragma_Attach_Handler             or else
                 Id = Pragma_Async_Readers              or else
                 Id = Pragma_Async_Writers              or else
                 Id = Pragma_Constant_After_Elaboration or else
                 Id = Pragma_Depends                    or else
                 Id = Pragma_Effective_Reads            or else
                 Id = Pragma_Effective_Writes           or else
                 Id = Pragma_Extensions_Visible         or else
                 Id = Pragma_Global                     or else
                 Id = Pragma_Initial_Condition          or else
                 Id = Pragma_Initializes                or else
                 Id = Pragma_Interrupt_Handler          or else
                 Id = Pragma_Part_Of                    or else
                 Id = Pragma_Refined_Depends            or else
                 Id = Pragma_Refined_Global             or else
                 Id = Pragma_Refined_State              or else
                 Id = Pragma_Volatile_Function;

      --  Contract / test case pragmas

      Is_CTC : constant Boolean :=
                  Id = Pragma_Contract_Cases            or else
                  Id = Pragma_Test_Case;

      --  Pre / postcondition pragmas

      Is_PPC : constant Boolean :=
                  Id = Pragma_Precondition              or else
                  Id = Pragma_Postcondition             or else
                  Id = Pragma_Refined_Post;

      In_Contract : constant Boolean := Is_CLS or Is_CTC or Is_PPC;

      Item  : Node_Id;
      Items : Node_Id;

   begin
      --  Handle pragmas that appear in N_Contract nodes. Those have to be
      --  extracted from their specialized list.

      if In_Contract then
         Items := Contract (E);

         if No (Items) then
            return Empty;

         elsif Is_CLS then
            Item := Classifications (Items);

         elsif Is_CTC then
            Item := Contract_Test_Cases (Items);

         else
            Item := Pre_Post_Conditions (Items);
         end if;

      --  Regular pragmas

      else
         Item := First_Rep_Item (E);
      end if;

      while Present (Item) loop
         if Nkind (Item) = N_Pragma
           and then Get_Pragma_Id (Pragma_Name (Item)) = Id
         then
            return Item;

         --  All nodes in N_Contract are chained using Next_Pragma

         elsif In_Contract then
            Item := Next_Pragma (Item);

         --  Regular pragmas

         else
            Next_Rep_Item (Item);
         end if;
      end loop;

      return Empty;
   end Get_Pragma;

   --------------------------------------
   -- Get_Record_Representation_Clause --
   --------------------------------------

   function Get_Record_Representation_Clause (E : Entity_Id) return Node_Id is
      N : Node_Id;

   begin
      N := First_Rep_Item (E);
      while Present (N) loop
         if Nkind (N) = N_Record_Representation_Clause then
            return N;
         end if;

         Next_Rep_Item (N);
      end loop;

      return Empty;
   end Get_Record_Representation_Clause;

   ------------------------
   -- Has_Attach_Handler --
   ------------------------

   function Has_Attach_Handler (Id : E) return B is
      Ritem : Node_Id;

   begin
      pragma Assert (Is_Protected_Type (Id));

      Ritem := First_Rep_Item (Id);
      while Present (Ritem) loop
         if Nkind (Ritem) = N_Pragma
           and then Pragma_Name (Ritem) = Name_Attach_Handler
         then
            return True;
         else
            Next_Rep_Item (Ritem);
         end if;
      end loop;

      return False;
   end Has_Attach_Handler;

   -----------------
   -- Has_Entries --
   -----------------

   function Has_Entries (Id : E) return B is
      Ent : Entity_Id;

   begin
      pragma Assert (Is_Concurrent_Type (Id));

      Ent := First_Entity (Id);
      while Present (Ent) loop
         if Is_Entry (Ent) then
            return True;
         end if;

         Ent := Next_Entity (Ent);
      end loop;

      return False;
   end Has_Entries;

   ----------------------------
   -- Has_Foreign_Convention --
   ----------------------------

   function Has_Foreign_Convention (Id : E) return B is
   begin
      --  While regular Intrinsics such as the Standard operators fit in the
      --  "Ada" convention, those with an Interface_Name materialize GCC
      --  builtin imports for which Ada special treatments shouldn't apply.

      return Convention (Id) in Foreign_Convention
        or else (Convention (Id) = Convention_Intrinsic
                   and then Present (Interface_Name (Id)));
   end Has_Foreign_Convention;

   ---------------------------
   -- Has_Interrupt_Handler --
   ---------------------------

   function Has_Interrupt_Handler (Id : E) return B is
      Ritem : Node_Id;

   begin
      pragma Assert (Is_Protected_Type (Id));

      Ritem := First_Rep_Item (Id);
      while Present (Ritem) loop
         if Nkind (Ritem) = N_Pragma
           and then Pragma_Name (Ritem) = Name_Interrupt_Handler
         then
            return True;
         else
            Next_Rep_Item (Ritem);
         end if;
      end loop;

      return False;
   end Has_Interrupt_Handler;

   --------------------------
   -- Has_Non_Limited_View --
   --------------------------

   function Has_Non_Limited_View (Id : E) return B is
   begin
      return (Ekind (Id) in Incomplete_Kind
               or else Ekind (Id) in Class_Wide_Kind
               or else Ekind (Id) = E_Abstract_State)
        and then Present (Non_Limited_View (Id));
   end Has_Non_Limited_View;

   ---------------------------------
   -- Has_Non_Null_Abstract_State --
   ---------------------------------

   function Has_Non_Null_Abstract_State (Id : E) return B is
   begin
      pragma Assert (Ekind_In (Id, E_Generic_Package, E_Package));

      return
        Present (Abstract_States (Id))
          and then
            not Is_Null_State (Node (First_Elmt (Abstract_States (Id))));
   end Has_Non_Null_Abstract_State;

   -------------------------------------
   -- Has_Non_Null_Visible_Refinement --
   -------------------------------------

   function Has_Non_Null_Visible_Refinement (Id : E) return B is
   begin
      --  "Refinement" is a concept applicable only to abstract states

      pragma Assert (Ekind (Id) = E_Abstract_State);

      if Has_Visible_Refinement (Id) then
         pragma Assert (Present (Refinement_Constituents (Id)));

         --  For a refinement to be non-null, the first constituent must be
         --  anything other than null.

         return
           Nkind (Node (First_Elmt (Refinement_Constituents (Id)))) /= N_Null;
      end if;

      return False;
   end Has_Non_Null_Visible_Refinement;

   -----------------------------
   -- Has_Null_Abstract_State --
   -----------------------------

   function Has_Null_Abstract_State (Id : E) return B is
   begin
      pragma Assert (Ekind_In (Id, E_Generic_Package, E_Package));

      return
        Present (Abstract_States (Id))
          and then Is_Null_State (Node (First_Elmt (Abstract_States (Id))));
   end Has_Null_Abstract_State;

   ---------------------------------
   -- Has_Null_Visible_Refinement --
   ---------------------------------

   function Has_Null_Visible_Refinement (Id : E) return B is
   begin
      --  "Refinement" is a concept applicable only to abstract states

      pragma Assert (Ekind (Id) = E_Abstract_State);

      if Has_Visible_Refinement (Id) then
         pragma Assert (Present (Refinement_Constituents (Id)));

         --  For a refinement to be null, the state's sole constituent must be
         --  a null.

         return
           Nkind (Node (First_Elmt (Refinement_Constituents (Id)))) = N_Null;
      end if;

      return False;
   end Has_Null_Visible_Refinement;

   --------------------
   -- Has_Unmodified --
   --------------------

   function Has_Unmodified (E : Entity_Id) return Boolean is
   begin
      if Has_Pragma_Unmodified (E) then
         return True;
      elsif Warnings_Off (E) then
         Set_Warnings_Off_Used_Unmodified (E);
         return True;
      else
         return False;
      end if;
   end Has_Unmodified;

   ---------------------
   -- Has_Unreferenced --
   ---------------------

   function Has_Unreferenced (E : Entity_Id) return Boolean is
   begin
      if Has_Pragma_Unreferenced (E) then
         return True;
      elsif Warnings_Off (E) then
         Set_Warnings_Off_Used_Unreferenced (E);
         return True;
      else
         return False;
      end if;
   end Has_Unreferenced;

   ----------------------
   -- Has_Warnings_Off --
   ----------------------

   function Has_Warnings_Off (E : Entity_Id) return Boolean is
   begin
      if Warnings_Off (E) then
         Set_Warnings_Off_Used (E);
         return True;
      else
         return False;
      end if;
   end Has_Warnings_Off;

   ------------------------------
   -- Implementation_Base_Type --
   ------------------------------

   function Implementation_Base_Type (Id : E) return E is
      Bastyp : Entity_Id;
      Imptyp : Entity_Id;

   begin
      Bastyp := Base_Type (Id);

      if Is_Incomplete_Or_Private_Type (Bastyp) then
         Imptyp := Underlying_Type (Bastyp);

         --  If we have an implementation type, then just return it,
         --  otherwise we return the Base_Type anyway. This can only
         --  happen in error situations and should avoid some error bombs.

         if Present (Imptyp) then
            return Base_Type (Imptyp);
         else
            return Bastyp;
         end if;

      else
         return Bastyp;
      end if;
   end Implementation_Base_Type;

   -------------------------
   -- Invariant_Procedure --
   -------------------------

   function Invariant_Procedure (Id : E) return E is
      S : Entity_Id;

   begin
      pragma Assert (Is_Type (Id) and then Has_Invariants (Id));

      if No (Subprograms_For_Type (Id)) then
         return Empty;

      else
         S := Subprograms_For_Type (Id);
         while Present (S) loop
            if Is_Invariant_Procedure (S) then
               return S;
            else
               S := Subprograms_For_Type (S);
            end if;
         end loop;

         return Empty;
      end if;
   end Invariant_Procedure;

   ----------------------
   -- Is_Atomic_Or_VFA --
   ----------------------

   function Is_Atomic_Or_VFA (Id : E) return B is
   begin
      return Is_Atomic (Id) or else Is_Volatile_Full_Access (Id);
   end Is_Atomic_Or_VFA;

   ------------------
   -- Is_Base_Type --
   ------------------

   --  Global flag table allowing rapid computation of this function

   Entity_Is_Base_Type : constant array (Entity_Kind) of Boolean :=
     (E_Enumeration_Subtype          |
      E_Incomplete_Type              |
      E_Signed_Integer_Subtype       |
      E_Modular_Integer_Subtype      |
      E_Floating_Point_Subtype       |
      E_Ordinary_Fixed_Point_Subtype |
      E_Decimal_Fixed_Point_Subtype  |
      E_Array_Subtype                |
      E_Record_Subtype               |
      E_Private_Subtype              |
      E_Record_Subtype_With_Private  |
      E_Limited_Private_Subtype      |
      E_Access_Subtype               |
      E_Protected_Subtype            |
      E_Task_Subtype                 |
      E_String_Literal_Subtype       |
      E_Class_Wide_Subtype           => False,
      others                         => True);

   function Is_Base_Type (Id : E) return Boolean is
   begin
      return Entity_Is_Base_Type (Ekind (Id));
   end Is_Base_Type;

   ---------------------
   -- Is_Boolean_Type --
   ---------------------

   function Is_Boolean_Type (Id : E) return B is
   begin
      return Root_Type (Id) = Standard_Boolean;
   end Is_Boolean_Type;

   ------------------------
   -- Is_Constant_Object --
   ------------------------

   function Is_Constant_Object (Id : E) return B is
      K : constant Entity_Kind := Ekind (Id);
   begin
      return
        K = E_Constant or else K = E_In_Parameter or else K = E_Loop_Parameter;
   end Is_Constant_Object;

   --------------------------
   -- Is_Controlled_Active --
   --------------------------

   function Is_Controlled_Active (Id : E) return B is
   begin
      return Is_Controlled (Id) and then not Disable_Controlled (Id);
   end Is_Controlled_Active;

   --------------------
   -- Is_Discriminal --
   --------------------

   function Is_Discriminal (Id : E) return B is
   begin
      return (Ekind_In (Id, E_Constant, E_In_Parameter)
                and then Present (Discriminal_Link (Id)));
   end Is_Discriminal;

   ----------------------
   -- Is_Dynamic_Scope --
   ----------------------

   function Is_Dynamic_Scope (Id : E) return B is
   begin
      return
        Ekind (Id) = E_Block
          or else
        Ekind (Id) = E_Function
          or else
        Ekind (Id) = E_Procedure
          or else
        Ekind (Id) = E_Subprogram_Body
          or else
        Ekind (Id) = E_Task_Type
          or else
       (Ekind (Id) = E_Limited_Private_Type
         and then Present (Full_View (Id))
         and then Ekind (Full_View (Id)) = E_Task_Type)
          or else
        Ekind (Id) = E_Entry
          or else
        Ekind (Id) = E_Entry_Family
          or else
        Ekind (Id) = E_Return_Statement;
   end Is_Dynamic_Scope;

   --------------------
   -- Is_Entity_Name --
   --------------------

   function Is_Entity_Name (N : Node_Id) return Boolean is
      Kind : constant Node_Kind := Nkind (N);

   begin
      --  Identifiers, operator symbols, expanded names are entity names

      return Kind = N_Identifier
        or else Kind = N_Operator_Symbol
        or else Kind = N_Expanded_Name

      --  Attribute references are entity names if they refer to an entity.
      --  Note that we don't do this by testing for the presence of the
      --  Entity field in the N_Attribute_Reference node, since it may not
      --  have been set yet.

        or else (Kind = N_Attribute_Reference
                  and then Is_Entity_Attribute_Name (Attribute_Name (N)));
   end Is_Entity_Name;

   -----------------------
   -- Is_External_State --
   -----------------------

   function Is_External_State (Id : E) return B is
   begin
      return
        Ekind (Id) = E_Abstract_State and then Has_Option (Id, Name_External);
   end Is_External_State;

   ------------------
   -- Is_Finalizer --
   ------------------

   function Is_Finalizer (Id : E) return B is
   begin
      return Ekind (Id) = E_Procedure and then Chars (Id) = Name_uFinalizer;
   end Is_Finalizer;

   -------------------
   -- Is_Null_State --
   -------------------

   function Is_Null_State (Id : E) return B is
   begin
      return
        Ekind (Id) = E_Abstract_State and then Nkind (Parent (Id)) = N_Null;
   end Is_Null_State;

   ---------------------
   -- Is_Packed_Array --
   ---------------------

   function Is_Packed_Array (Id : E) return B is
   begin
      return Is_Array_Type (Id) and then Is_Packed (Id);
   end Is_Packed_Array;

   -----------------------------------
   -- Is_Package_Or_Generic_Package --
   -----------------------------------

   function Is_Package_Or_Generic_Package (Id : E) return B is
   begin
      return Ekind_In (Id, E_Generic_Package, E_Package);
   end Is_Package_Or_Generic_Package;

   ---------------
   -- Is_Prival --
   ---------------

   function Is_Prival (Id : E) return B is
   begin
      return (Ekind_In (Id, E_Constant, E_Variable)
                and then Present (Prival_Link (Id)));
   end Is_Prival;

   ----------------------------
   -- Is_Protected_Component --
   ----------------------------

   function Is_Protected_Component (Id : E) return B is
   begin
      return Ekind (Id) = E_Component and then Is_Protected_Type (Scope (Id));
   end Is_Protected_Component;

   ----------------------------
   -- Is_Protected_Interface --
   ----------------------------

   function Is_Protected_Interface (Id : E) return B is
      Typ : constant Entity_Id := Base_Type (Id);
   begin
      if not Is_Interface (Typ) then
         return False;
      elsif Is_Class_Wide_Type (Typ) then
         return Is_Protected_Interface (Etype (Typ));
      else
         return Protected_Present (Type_Definition (Parent (Typ)));
      end if;
   end Is_Protected_Interface;

   ------------------------------
   -- Is_Protected_Record_Type --
   ------------------------------

   function Is_Protected_Record_Type (Id : E) return B is
   begin
      return
        Is_Concurrent_Record_Type (Id)
          and then Is_Protected_Type (Corresponding_Concurrent_Type (Id));
   end Is_Protected_Record_Type;

   --------------------------------
   -- Is_Standard_Character_Type --
   --------------------------------

   function Is_Standard_Character_Type (Id : E) return B is
   begin
      if Is_Type (Id) then
         declare
            R : constant Entity_Id := Root_Type (Id);
         begin
            return
              R = Standard_Character
                or else
              R = Standard_Wide_Character
                or else
              R = Standard_Wide_Wide_Character;
         end;

      else
         return False;
      end if;
   end Is_Standard_Character_Type;

   -----------------------------
   -- Is_Standard_String_Type --
   -----------------------------

   function Is_Standard_String_Type (Id : E) return B is
   begin
      if Is_Type (Id) then
         declare
            R : constant Entity_Id := Root_Type (Id);
         begin
            return
              R = Standard_String
                or else
              R = Standard_Wide_String
                or else
              R = Standard_Wide_Wide_String;
         end;

      else
         return False;
      end if;
   end Is_Standard_String_Type;

   --------------------
   -- Is_String_Type --
   --------------------

   function Is_String_Type (Id : E) return B is
   begin
      return Is_Array_Type (Id)
        and then Id /= Any_Composite
        and then Number_Dimensions (Id) = 1
        and then Is_Character_Type (Component_Type (Id));
   end Is_String_Type;

   -------------------------------
   -- Is_Synchronized_Interface --
   -------------------------------

   function Is_Synchronized_Interface (Id : E) return B is
      Typ : constant Entity_Id := Base_Type (Id);

   begin
      if not Is_Interface (Typ) then
         return False;

      elsif Is_Class_Wide_Type (Typ) then
         return Is_Synchronized_Interface (Etype (Typ));

      else
         return    Protected_Present    (Type_Definition (Parent (Typ)))
           or else Synchronized_Present (Type_Definition (Parent (Typ)))
           or else Task_Present         (Type_Definition (Parent (Typ)));
      end if;
   end Is_Synchronized_Interface;

   ---------------------------
   -- Is_Synchronized_State --
   ---------------------------

   function Is_Synchronized_State (Id : E) return B is
   begin
      return
        Ekind (Id) = E_Abstract_State
          and then Has_Option (Id, Name_Synchronous);
   end Is_Synchronized_State;

   -----------------------
   -- Is_Task_Interface --
   -----------------------

   function Is_Task_Interface (Id : E) return B is
      Typ : constant Entity_Id := Base_Type (Id);
   begin
      if not Is_Interface (Typ) then
         return False;
      elsif Is_Class_Wide_Type (Typ) then
         return Is_Task_Interface (Etype (Typ));
      else
         return Task_Present (Type_Definition (Parent (Typ)));
      end if;
   end Is_Task_Interface;

   -------------------------
   -- Is_Task_Record_Type --
   -------------------------

   function Is_Task_Record_Type (Id : E) return B is
   begin
      return
        Is_Concurrent_Record_Type (Id)
          and then Is_Task_Type (Corresponding_Concurrent_Type (Id));
   end Is_Task_Record_Type;

   ------------------------
   -- Is_Wrapper_Package --
   ------------------------

   function Is_Wrapper_Package (Id : E) return B is
   begin
      return (Ekind (Id) = E_Package and then Present (Related_Instance (Id)));
   end Is_Wrapper_Package;

   -----------------
   -- Last_Formal --
   -----------------

   function Last_Formal (Id : E) return E is
      Formal : E;

   begin
      pragma Assert
        (Is_Overloadable (Id)
          or else Ekind_In (Id, E_Entry_Family,
                                E_Subprogram_Body,
                                E_Subprogram_Type));

      if Ekind (Id) = E_Enumeration_Literal then
         return Empty;

      else
         Formal := First_Formal (Id);

         if Present (Formal) then
            while Present (Next_Formal (Formal)) loop
               Formal := Next_Formal (Formal);
            end loop;
         end if;

         return Formal;
      end if;
   end Last_Formal;

   function Model_Emin_Value (Id : E) return Uint is
   begin
      return Machine_Emin_Value (Id);
   end Model_Emin_Value;

   -------------------------
   -- Model_Epsilon_Value --
   -------------------------

   function Model_Epsilon_Value (Id : E) return Ureal is
      Radix : constant Ureal := UR_From_Uint (Machine_Radix_Value (Id));
   begin
      return Radix ** (1 - Model_Mantissa_Value (Id));
   end Model_Epsilon_Value;

   --------------------------
   -- Model_Mantissa_Value --
   --------------------------

   function Model_Mantissa_Value (Id : E) return Uint is
   begin
      return Machine_Mantissa_Value (Id);
   end Model_Mantissa_Value;

   -----------------------
   -- Model_Small_Value --
   -----------------------

   function Model_Small_Value (Id : E) return Ureal is
      Radix : constant Ureal := UR_From_Uint (Machine_Radix_Value (Id));
   begin
      return Radix ** (Model_Emin_Value (Id) - 1);
   end Model_Small_Value;

   ------------------------
   -- Machine_Emax_Value --
   ------------------------

   function Machine_Emax_Value (Id : E) return Uint is
      Digs : constant Pos := UI_To_Int (Digits_Value (Base_Type (Id)));

   begin
      case Float_Rep (Id) is
         when IEEE_Binary =>
            case Digs is
               when  1 ..  6 => return Uint_128;
               when  7 .. 15 => return 2**10;
               when 16 .. 33 => return 2**14;
               when others => return No_Uint;
            end case;

         when AAMP =>
            return Uint_2 ** Uint_7 - Uint_1;
      end case;
   end Machine_Emax_Value;

   ------------------------
   -- Machine_Emin_Value --
   ------------------------

   function Machine_Emin_Value (Id : E) return Uint is
   begin
      case Float_Rep (Id) is
         when IEEE_Binary => return Uint_3 - Machine_Emax_Value (Id);
         when AAMP        => return -Machine_Emax_Value (Id);
      end case;
   end Machine_Emin_Value;

   ----------------------------
   -- Machine_Mantissa_Value --
   ----------------------------

   function Machine_Mantissa_Value (Id : E) return Uint is
      Digs : constant Pos := UI_To_Int (Digits_Value (Base_Type (Id)));

   begin
      case Float_Rep (Id) is
         when IEEE_Binary =>
            case Digs is
               when  1 ..  6 => return Uint_24;
               when  7 .. 15 => return UI_From_Int (53);
               when 16 .. 18 => return Uint_64;
               when 19 .. 33 => return UI_From_Int (113);
               when others => return No_Uint;
            end case;

         when AAMP =>
            case Digs is
               when  1 ..  6 => return Uint_24;
               when  7 ..  9 => return UI_From_Int (40);
               when others => return No_Uint;
            end case;
      end case;
   end Machine_Mantissa_Value;

   -------------------------
   -- Machine_Radix_Value --
   -------------------------

   function Machine_Radix_Value (Id : E) return U is
   begin
      case Float_Rep (Id) is
         when IEEE_Binary | AAMP =>
            return Uint_2;
      end case;
   end Machine_Radix_Value;

   --------------------
   -- Next_Component --
   --------------------

   function Next_Component (Id : E) return E is
      Comp_Id : E;

   begin
      Comp_Id := Next_Entity (Id);
      while Present (Comp_Id) loop
         exit when Ekind (Comp_Id) = E_Component;
         Comp_Id := Next_Entity (Comp_Id);
      end loop;

      return Comp_Id;
   end Next_Component;

   ------------------------------------
   -- Next_Component_Or_Discriminant --
   ------------------------------------

   function Next_Component_Or_Discriminant (Id : E) return E is
      Comp_Id : E;

   begin
      Comp_Id := Next_Entity (Id);
      while Present (Comp_Id) loop
         exit when Ekind_In (Comp_Id, E_Component, E_Discriminant);
         Comp_Id := Next_Entity (Comp_Id);
      end loop;

      return Comp_Id;
   end Next_Component_Or_Discriminant;

   -----------------------
   -- Next_Discriminant --
   -----------------------

   --  This function actually implements both Next_Discriminant and
   --  Next_Stored_Discriminant by making sure that the Discriminant
   --  returned is of the same variety as Id.

   function Next_Discriminant (Id : E) return E is

      --  Derived Tagged types with private extensions look like this...

      --       E_Discriminant d1
      --       E_Discriminant d2
      --       E_Component    _tag
      --       E_Discriminant d1
      --       E_Discriminant d2
      --       ...

      --  so it is critical not to go past the leading discriminants

      D : E := Id;

   begin
      pragma Assert (Ekind (Id) = E_Discriminant);

      loop
         D := Next_Entity (D);
         if No (D)
           or else (Ekind (D) /= E_Discriminant
                      and then not Is_Itype (D))
         then
            return Empty;
         end if;

         exit when Ekind (D) = E_Discriminant
           and then (Is_Completely_Hidden (D) = Is_Completely_Hidden (Id));
      end loop;

      return D;
   end Next_Discriminant;

   -----------------
   -- Next_Formal --
   -----------------

   function Next_Formal (Id : E) return E is
      P : E;

   begin
      --  Follow the chain of declared entities as long as the kind of the
      --  entity corresponds to a formal parameter. Skip internal entities
      --  that may have been created for implicit subtypes, in the process
      --  of analyzing default expressions.

      P := Id;
      loop
         Next_Entity (P);

         if No (P) or else Is_Formal (P) then
            return P;
         elsif not Is_Internal (P) then
            return Empty;
         end if;
      end loop;
   end Next_Formal;

   -----------------------------
   -- Next_Formal_With_Extras --
   -----------------------------

   function Next_Formal_With_Extras (Id : E) return E is
   begin
      if Present (Extra_Formal (Id)) then
         return Extra_Formal (Id);
      else
         return Next_Formal (Id);
      end if;
   end Next_Formal_With_Extras;

   ----------------
   -- Next_Index --
   ----------------

   function Next_Index (Id : Node_Id) return Node_Id is
   begin
      return Next (Id);
   end Next_Index;

   ------------------
   -- Next_Literal --
   ------------------

   function Next_Literal (Id : E) return E is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Next (Id);
   end Next_Literal;

   ------------------------------
   -- Next_Stored_Discriminant --
   ------------------------------

   function Next_Stored_Discriminant (Id : E) return E is
   begin
      --  See comment in Next_Discriminant

      return Next_Discriminant (Id);
   end Next_Stored_Discriminant;

   -----------------------
   -- Number_Dimensions --
   -----------------------

   function Number_Dimensions (Id : E) return Pos is
      N : Int;
      T : Node_Id;

   begin
      if Ekind (Id) = E_String_Literal_Subtype then
         return 1;

      else
         N := 0;
         T := First_Index (Id);
         while Present (T) loop
            N := N + 1;
            Next_Index (T);
         end loop;

         return N;
      end if;
   end Number_Dimensions;

   --------------------
   -- Number_Entries --
   --------------------

   function Number_Entries (Id : E) return Nat is
      N   : Int;
      Ent : Entity_Id;

   begin
      pragma Assert (Is_Concurrent_Type (Id));

      N := 0;
      Ent := First_Entity (Id);
      while Present (Ent) loop
         if Is_Entry (Ent) then
            N := N + 1;
         end if;

         Ent := Next_Entity (Ent);
      end loop;

      return N;
   end Number_Entries;

   --------------------
   -- Number_Formals --
   --------------------

   function Number_Formals (Id : E) return Pos is
      N      : Int;
      Formal : Entity_Id;

   begin
      N := 0;
      Formal := First_Formal (Id);
      while Present (Formal) loop
         N := N + 1;
         Formal := Next_Formal (Formal);
      end loop;

      return N;
   end Number_Formals;

   --------------------
   -- Parameter_Mode --
   --------------------

   function Parameter_Mode (Id : E) return Formal_Kind is
   begin
      return Ekind (Id);
   end Parameter_Mode;

   ------------------------
   -- Predicate_Function --
   ------------------------

   function Predicate_Function (Id : E) return E is
      S : Entity_Id;
      T : Entity_Id;

   begin
      pragma Assert (Is_Type (Id));

      --  If type is private and has a completion, predicate may be defined
      --  on the full view.

      if Is_Private_Type (Id)
         and then
           (not Has_Predicates (Id) or else No (Subprograms_For_Type (Id)))
         and then Present (Full_View (Id))
      then
         T := Full_View (Id);

      else
         T := Id;
      end if;

      if No (Subprograms_For_Type (T)) then
         return Empty;

      else
         S := Subprograms_For_Type (T);
         while Present (S) loop
            if Is_Predicate_Function (S) then
               return S;
            else
               S := Subprograms_For_Type (S);
            end if;
         end loop;

         return Empty;
      end if;
   end Predicate_Function;

   --------------------------
   -- Predicate_Function_M --
   --------------------------

   function Predicate_Function_M (Id : E) return E is
      S : Entity_Id;
      T : Entity_Id;

   begin
      pragma Assert (Is_Type (Id));

      --  If type is private and has a completion, predicate may be defined
      --  on the full view.

      if Is_Private_Type (Id) and then Present (Full_View (Id)) then
         T := Full_View (Id);
      else
         T := Id;
      end if;

      if No (Subprograms_For_Type (T)) then
         return Empty;

      else
         S := Subprograms_For_Type (T);
         while Present (S) loop
            if Is_Predicate_Function_M (S) then
               return S;
            else
               S := Subprograms_For_Type (S);
            end if;
         end loop;

         return Empty;
      end if;
   end Predicate_Function_M;

   -------------------------
   -- Present_In_Rep_Item --
   -------------------------

   function Present_In_Rep_Item (E : Entity_Id; N : Node_Id) return Boolean is
      Ritem : Node_Id;

   begin
      Ritem := First_Rep_Item (E);

      while Present (Ritem) loop
         if Ritem = N then
            return True;
         end if;

         Next_Rep_Item (Ritem);
      end loop;

      return False;
   end Present_In_Rep_Item;

   --------------------------
   -- Primitive_Operations --
   --------------------------

   function Primitive_Operations (Id : E) return L is
   begin
      if Is_Concurrent_Type (Id) then
         if Present (Corresponding_Record_Type (Id)) then
            return Direct_Primitive_Operations
              (Corresponding_Record_Type (Id));

         --  If expansion is disabled the corresponding record type is absent,
         --  but if the type has ancestors it may have primitive operations.

         elsif Is_Tagged_Type (Id) then
            return Direct_Primitive_Operations (Id);

         else
            return No_Elist;
         end if;
      else
         return Direct_Primitive_Operations (Id);
      end if;
   end Primitive_Operations;

   ---------------------
   -- Record_Rep_Item --
   ---------------------

   procedure Record_Rep_Item (E : Entity_Id; N : Node_Id) is
   begin
      Set_Next_Rep_Item (N, First_Rep_Item (E));
      Set_First_Rep_Item (E, N);
   end Record_Rep_Item;

   ---------------
   -- Root_Type --
   ---------------

   function Root_Type (Id : E) return E is
      T, Etyp : E;

   begin
      pragma Assert (Nkind (Id) in N_Entity);

      T := Base_Type (Id);

      if Ekind (T) = E_Class_Wide_Type then
         return Etype (T);

      --  Other cases

      else
         loop
            Etyp := Etype (T);

            if T = Etyp then
               return T;

            --  Following test catches some error cases resulting from
            --  previous errors.

            elsif No (Etyp) then
               Check_Error_Detected;
               return T;

            elsif Is_Private_Type (T) and then Etyp = Full_View (T) then
               return T;

            elsif Is_Private_Type (Etyp) and then Full_View (Etyp) = T then
               return T;
            end if;

            T := Etyp;

            --  Return if there is a circularity in the inheritance chain. This
            --  happens in some error situations and we do not want to get
            --  stuck in this loop.

            if T = Base_Type (Id) then
               return T;
            end if;
         end loop;
      end if;
   end Root_Type;

   ---------------------
   -- Safe_Emax_Value --
   ---------------------

   function Safe_Emax_Value (Id : E) return Uint is
   begin
      return Machine_Emax_Value (Id);
   end Safe_Emax_Value;

   ----------------------
   -- Safe_First_Value --
   ----------------------

   function Safe_First_Value (Id : E) return Ureal is
   begin
      return -Safe_Last_Value (Id);
   end Safe_First_Value;

   ---------------------
   -- Safe_Last_Value --
   ---------------------

   function Safe_Last_Value (Id : E) return Ureal is
      Radix       : constant Uint := Machine_Radix_Value (Id);
      Mantissa    : constant Uint := Machine_Mantissa_Value (Id);
      Emax        : constant Uint := Safe_Emax_Value (Id);
      Significand : constant Uint := Radix ** Mantissa - 1;
      Exponent    : constant Uint := Emax - Mantissa;

   begin
      if Radix = 2 then
         return
           UR_From_Components
             (Num   => Significand * 2 ** (Exponent mod 4),
              Den   => -Exponent / 4,
              Rbase => 16);
      else
         return
           UR_From_Components
             (Num => Significand,
              Den => -Exponent,
              Rbase => 16);
      end if;
   end Safe_Last_Value;

   -----------------
   -- Scope_Depth --
   -----------------

   function Scope_Depth (Id : E) return Uint is
      Scop : Entity_Id;

   begin
      Scop := Id;
      while Is_Record_Type (Scop) loop
         Scop := Scope (Scop);
      end loop;

      return Scope_Depth_Value (Scop);
   end Scope_Depth;

   ---------------------
   -- Scope_Depth_Set --
   ---------------------

   function Scope_Depth_Set (Id : E) return B is
   begin
      return not Is_Record_Type (Id)
        and then Field22 (Id) /= Union_Id (Empty);
   end Scope_Depth_Set;

   -----------------------------
   -- Set_Component_Alignment --
   -----------------------------

   --  Component Alignment is encoded using two flags, Flag128/129 as
   --  follows. Note that both flags False = Align_Default, so that the
   --  default initialization of flags to False initializes component
   --  alignment to the default value as required.

   --     Flag128      Flag129      Value
   --     -------      -------      -----
   --      False        False       Calign_Default
   --      False        True        Calign_Component_Size
   --      True         False       Calign_Component_Size_4
   --      True         True        Calign_Storage_Unit

   procedure Set_Component_Alignment (Id : E; V : C) is
   begin
      pragma Assert ((Is_Array_Type (Id) or else Is_Record_Type (Id))
                       and then Is_Base_Type (Id));

      case V is
         when Calign_Default          =>
            Set_Flag128 (Id, False);
            Set_Flag129 (Id, False);

         when Calign_Component_Size   =>
            Set_Flag128 (Id, False);
            Set_Flag129 (Id, True);

         when Calign_Component_Size_4 =>
            Set_Flag128 (Id, True);
            Set_Flag129 (Id, False);

         when Calign_Storage_Unit     =>
            Set_Flag128 (Id, True);
            Set_Flag129 (Id, True);
      end case;
   end Set_Component_Alignment;

   -------------------------------------
   -- Set_Default_Init_Cond_Procedure --
   -------------------------------------

   procedure Set_Default_Init_Cond_Procedure (Id : E; V : E) is
      Base_Typ : Entity_Id;
      Subp_Id  : Entity_Id;

   begin
      pragma Assert
        (Is_Type (Id)
          and then (Has_Default_Init_Cond (Id)
                     or Has_Inherited_Default_Init_Cond (Id)));
      Base_Typ := Base_Type (Id);

      Subp_Id := Subprograms_For_Type (Base_Typ);
      Set_Subprograms_For_Type (Base_Typ, V);
      Set_Subprograms_For_Type (V, Subp_Id);

      --  Check for a duplicate procedure

      while Present (Subp_Id) loop
         if Is_Default_Init_Cond_Procedure (Subp_Id) then
            raise Program_Error;
         end if;

         Subp_Id := Subprograms_For_Type (Subp_Id);
      end loop;
   end Set_Default_Init_Cond_Procedure;

   -----------------------------
   -- Set_Invariant_Procedure --
   -----------------------------

   procedure Set_Invariant_Procedure (Id : E; V : E) is
      S : Entity_Id;

   begin
      pragma Assert (Is_Type (Id) and then Has_Invariants (Id));

      S := Subprograms_For_Type (Id);
      Set_Subprograms_For_Type (Id, V);
      Set_Subprograms_For_Type (V, S);

      --  Check for duplicate entry

      while Present (S) loop
         if Is_Invariant_Procedure (S) then
            raise Program_Error;
         else
            S := Subprograms_For_Type (S);
         end if;
      end loop;
   end Set_Invariant_Procedure;

   ----------------------------
   -- Set_Predicate_Function --
   ----------------------------

   procedure Set_Predicate_Function (Id : E; V : E) is
      S : Entity_Id;

   begin
      pragma Assert (Is_Type (Id) and then Has_Predicates (Id));

      S := Subprograms_For_Type (Id);
      Set_Subprograms_For_Type (Id, V);
      Set_Subprograms_For_Type (V, S);

      while Present (S) loop
         if Is_Predicate_Function (S) then
            raise Program_Error;
         else
            S := Subprograms_For_Type (S);
         end if;
      end loop;
   end Set_Predicate_Function;

   ------------------------------
   -- Set_Predicate_Function_M --
   ------------------------------

   procedure Set_Predicate_Function_M (Id : E; V : E) is
      S : Entity_Id;

   begin
      pragma Assert (Is_Type (Id) and then Has_Predicates (Id));

      S := Subprograms_For_Type (Id);
      Set_Subprograms_For_Type (Id, V);
      Set_Subprograms_For_Type (V, S);

      --  Check for duplicates

      while Present (S) loop
         if Is_Predicate_Function_M (S) then
            raise Program_Error;
         else
            S := Subprograms_For_Type (S);
         end if;
      end loop;
   end Set_Predicate_Function_M;

   -----------------
   -- Size_Clause --
   -----------------

   function Size_Clause (Id : E) return N is
   begin
      return Get_Attribute_Definition_Clause (Id, Attribute_Size);
   end Size_Clause;

   ------------------------
   -- Stream_Size_Clause --
   ------------------------

   function Stream_Size_Clause (Id : E) return N is
   begin
      return Get_Attribute_Definition_Clause (Id, Attribute_Stream_Size);
   end Stream_Size_Clause;

   ------------------
   -- Subtype_Kind --
   ------------------

   function Subtype_Kind (K : Entity_Kind) return Entity_Kind is
      Kind : Entity_Kind;

   begin
      case K is
         when Access_Kind                    =>
            Kind := E_Access_Subtype;

         when E_Array_Type                   |
              E_Array_Subtype                =>
            Kind := E_Array_Subtype;

         when E_Class_Wide_Type              |
              E_Class_Wide_Subtype           =>
            Kind := E_Class_Wide_Subtype;

         when E_Decimal_Fixed_Point_Type     |
              E_Decimal_Fixed_Point_Subtype  =>
            Kind := E_Decimal_Fixed_Point_Subtype;

         when E_Ordinary_Fixed_Point_Type    |
              E_Ordinary_Fixed_Point_Subtype =>
            Kind := E_Ordinary_Fixed_Point_Subtype;

         when E_Private_Type                 |
              E_Private_Subtype              =>
            Kind := E_Private_Subtype;

         when E_Limited_Private_Type         |
              E_Limited_Private_Subtype      =>
            Kind := E_Limited_Private_Subtype;

         when E_Record_Type_With_Private     |
              E_Record_Subtype_With_Private  =>
            Kind := E_Record_Subtype_With_Private;

         when E_Record_Type                  |
              E_Record_Subtype               =>
            Kind := E_Record_Subtype;

         when Enumeration_Kind               =>
            Kind := E_Enumeration_Subtype;

         when Float_Kind                     =>
            Kind := E_Floating_Point_Subtype;

         when Signed_Integer_Kind            =>
            Kind := E_Signed_Integer_Subtype;

         when Modular_Integer_Kind           =>
            Kind := E_Modular_Integer_Subtype;

         when Protected_Kind                 =>
            Kind := E_Protected_Subtype;

         when Task_Kind                      =>
            Kind := E_Task_Subtype;

         when others                         =>
            Kind := E_Void;
            raise Program_Error;
      end case;

      return Kind;
   end Subtype_Kind;

   ---------------------
   -- Type_High_Bound --
   ---------------------

   function Type_High_Bound (Id : E) return Node_Id is
      Rng : constant Node_Id := Scalar_Range (Id);
   begin
      if Nkind (Rng) = N_Subtype_Indication then
         return High_Bound (Range_Expression (Constraint (Rng)));
      else
         return High_Bound (Rng);
      end if;
   end Type_High_Bound;

   --------------------
   -- Type_Low_Bound --
   --------------------

   function Type_Low_Bound (Id : E) return Node_Id is
      Rng : constant Node_Id := Scalar_Range (Id);
   begin
      if Nkind (Rng) = N_Subtype_Indication then
         return Low_Bound (Range_Expression (Constraint (Rng)));
      else
         return Low_Bound (Rng);
      end if;
   end Type_Low_Bound;

   ---------------------
   -- Underlying_Type --
   ---------------------

   function Underlying_Type (Id : E) return E is
   begin
      --  For record_with_private the underlying type is always the direct
      --  full view. Never try to take the full view of the parent it
      --  doesn't make sense.

      if Ekind (Id) = E_Record_Type_With_Private then
         return Full_View (Id);

      elsif Ekind (Id) in Incomplete_Or_Private_Kind then

         --  If we have an incomplete or private type with a full view,
         --  then we return the Underlying_Type of this full view.

         if Present (Full_View (Id)) then
            if Id = Full_View (Id) then

               --  Previous error in declaration

               return Empty;

            else
               return Underlying_Type (Full_View (Id));
            end if;

         --  If we have a private type with an underlying full view, then we
         --  return the Underlying_Type of this underlying full view.

         elsif Ekind (Id) in Private_Kind
           and then Present (Underlying_Full_View (Id))
         then
            return Underlying_Type (Underlying_Full_View (Id));

         --  If we have an incomplete entity that comes from the limited
         --  view then we return the Underlying_Type of its non-limited
         --  view.

         elsif From_Limited_With (Id)
           and then Present (Non_Limited_View (Id))
         then
            return Underlying_Type (Non_Limited_View (Id));

         --  Otherwise check for the case where we have a derived type or
         --  subtype, and if so get the Underlying_Type of the parent type.

         elsif Etype (Id) /= Id then
            return Underlying_Type (Etype (Id));

         --  Otherwise we have an incomplete or private type that has
         --  no full view, which means that we have not encountered the
         --  completion, so return Empty to indicate the underlying type
         --  is not yet known.

         else
            return Empty;
         end if;

      --  For non-incomplete, non-private types, return the type itself Also
      --  for entities that are not types at all return the entity itself.

      else
         return Id;
      end if;
   end Underlying_Type;

   ------------------------
   -- Write_Entity_Flags --
   ------------------------

   procedure Write_Entity_Flags (Id : Entity_Id; Prefix : String) is

      procedure W (Flag_Name : String; Flag : Boolean);
      --  Write out given flag if it is set

      -------
      -- W --
      -------

      procedure W (Flag_Name : String; Flag : Boolean) is
      begin
         if Flag then
            Write_Str (Prefix);
            Write_Str (Flag_Name);
            Write_Str (" = True");
            Write_Eol;
         end if;
      end W;

   --  Start of processing for Write_Entity_Flags

   begin
      if (Is_Array_Type (Id) or else Is_Record_Type (Id))
        and then Is_Base_Type (Id)
      then
         Write_Str (Prefix);
         Write_Str ("Component_Alignment = ");

         case Component_Alignment (Id) is
            when Calign_Default =>
               Write_Str ("Calign_Default");

            when Calign_Component_Size =>
               Write_Str ("Calign_Component_Size");

            when Calign_Component_Size_4 =>
               Write_Str ("Calign_Component_Size_4");

            when Calign_Storage_Unit =>
               Write_Str ("Calign_Storage_Unit");
         end case;

         Write_Eol;
      end if;

      W ("Address_Taken",                   Flag104 (Id));
      W ("Body_Needed_For_SAL",             Flag40  (Id));
      W ("C_Pass_By_Copy",                  Flag125 (Id));
      W ("Can_Never_Be_Null",               Flag38  (Id));
      W ("Checks_May_Be_Suppressed",        Flag31  (Id));
      W ("Contains_Ignored_Ghost_Code",     Flag279 (Id));
      W ("Debug_Info_Off",                  Flag166 (Id));
      W ("Default_Expressions_Processed",   Flag108 (Id));
      W ("Delay_Cleanups",                  Flag114 (Id));
      W ("Delay_Subprogram_Descriptors",    Flag50  (Id));
      W ("Depends_On_Private",              Flag14  (Id));
      W ("Discard_Names",                   Flag88  (Id));
      W ("Elaboration_Entity_Required",     Flag174 (Id));
      W ("Elaborate_Body_Desirable",        Flag210 (Id));
      W ("Entry_Accepted",                  Flag152 (Id));
      W ("Can_Use_Internal_Rep",            Flag229 (Id));
      W ("Finalize_Storage_Only",           Flag158 (Id));
      W ("From_Limited_With",               Flag159 (Id));
      W ("Has_Aliased_Components",          Flag135 (Id));
      W ("Has_Alignment_Clause",            Flag46  (Id));
      W ("Has_All_Calls_Remote",            Flag79  (Id));
      W ("Has_Atomic_Components",           Flag86  (Id));
      W ("Has_Biased_Representation",       Flag139 (Id));
      W ("Has_Completion",                  Flag26  (Id));
      W ("Has_Completion_In_Body",          Flag71  (Id));
      W ("Has_Complex_Representation",      Flag140 (Id));
      W ("Has_Component_Size_Clause",       Flag68  (Id));
      W ("Has_Contiguous_Rep",              Flag181 (Id));
      W ("Has_Controlled_Component",        Flag43  (Id));
      W ("Has_Controlling_Result",          Flag98  (Id));
      W ("Has_Convention_Pragma",           Flag119 (Id));
      W ("Has_Default_Aspect",              Flag39  (Id));
      W ("Has_Default_Init_Cond",           Flag3   (Id));
      W ("Has_Delayed_Aspects",             Flag200 (Id));
      W ("Has_Delayed_Freeze",              Flag18  (Id));
      W ("Has_Delayed_Rep_Aspects",         Flag261 (Id));
      W ("Has_Discriminants",               Flag5   (Id));
      W ("Has_Dispatch_Table",              Flag220 (Id));
      W ("Has_Dynamic_Predicate_Aspect",    Flag258 (Id));
      W ("Has_Enumeration_Rep_Clause",      Flag66  (Id));
      W ("Has_Exit",                        Flag47  (Id));
      W ("Has_Expanded_Contract",           Flag240 (Id));
      W ("Has_Forward_Instantiation",       Flag175 (Id));
      W ("Has_Fully_Qualified_Name",        Flag173 (Id));
      W ("Has_Gigi_Rep_Item",               Flag82  (Id));
      W ("Has_Homonym",                     Flag56  (Id));
      W ("Has_Implicit_Dereference",        Flag251 (Id));
      W ("Has_Independent_Components",      Flag34  (Id));
      W ("Has_Inheritable_Invariants",      Flag248 (Id));
      W ("Has_Inherited_Default_Init_Cond", Flag133 (Id));
      W ("Has_Initial_Value",               Flag219 (Id));
      W ("Has_Invariants",                  Flag232 (Id));
      W ("Has_Loop_Entry_Attributes",       Flag260 (Id));
      W ("Has_Machine_Radix_Clause",        Flag83  (Id));
      W ("Has_Master_Entity",               Flag21  (Id));
      W ("Has_Missing_Return",              Flag142 (Id));
      W ("Has_Nested_Block_With_Handler",   Flag101 (Id));
      W ("Has_Nested_Subprogram",           Flag282 (Id));
      W ("Has_Non_Standard_Rep",            Flag75  (Id));
      W ("Has_Out_Or_In_Out_Parameter",     Flag110 (Id));
      W ("Has_Object_Size_Clause",          Flag172 (Id));
      W ("Has_Per_Object_Constraint",       Flag154 (Id));
      W ("Has_Pragma_Controlled",           Flag27  (Id));
      W ("Has_Pragma_Elaborate_Body",       Flag150 (Id));
      W ("Has_Pragma_Inline",               Flag157 (Id));
      W ("Has_Pragma_Inline_Always",        Flag230 (Id));
      W ("Has_Pragma_No_Inline",            Flag201 (Id));
      W ("Has_Pragma_Ordered",              Flag198 (Id));
      W ("Has_Pragma_Pack",                 Flag121 (Id));
      W ("Has_Pragma_Preelab_Init",         Flag221 (Id));
      W ("Has_Pragma_Pure",                 Flag203 (Id));
      W ("Has_Pragma_Pure_Function",        Flag179 (Id));
      W ("Has_Pragma_Thread_Local_Storage", Flag169 (Id));
      W ("Has_Pragma_Unmodified",           Flag233 (Id));
      W ("Has_Pragma_Unreferenced",         Flag180 (Id));
      W ("Has_Pragma_Unreferenced_Objects", Flag212 (Id));
      W ("Has_Predicates",                  Flag250 (Id));
      W ("Has_Primitive_Operations",        Flag120 (Id));
      W ("Has_Private_Ancestor",            Flag151 (Id));
      W ("Has_Private_Declaration",         Flag155 (Id));
      W ("Has_Protected",                   Flag271 (Id));
      W ("Has_Qualified_Name",              Flag161 (Id));
      W ("Has_RACW",                        Flag214 (Id));
      W ("Has_Record_Rep_Clause",           Flag65  (Id));
      W ("Has_Recursive_Call",              Flag143 (Id));
      W ("Has_Shift_Operator",              Flag267 (Id));
      W ("Has_Size_Clause",                 Flag29  (Id));
      W ("Has_Small_Clause",                Flag67  (Id));
      W ("Has_Specified_Layout",            Flag100 (Id));
      W ("Has_Specified_Stream_Input",      Flag190 (Id));
      W ("Has_Specified_Stream_Output",     Flag191 (Id));
      W ("Has_Specified_Stream_Read",       Flag192 (Id));
      W ("Has_Specified_Stream_Write",      Flag193 (Id));
      W ("Has_Static_Discriminants",        Flag211 (Id));
      W ("Has_Static_Predicate",            Flag269 (Id));
      W ("Has_Static_Predicate_Aspect",     Flag259 (Id));
      W ("Has_Storage_Size_Clause",         Flag23  (Id));
      W ("Has_Stream_Size_Clause",          Flag184 (Id));
      W ("Has_Task",                        Flag30  (Id));
      W ("Has_Thunks",                      Flag228 (Id));
      W ("Has_Unchecked_Union",             Flag123 (Id));
      W ("Has_Unknown_Discriminants",       Flag72  (Id));
      W ("Has_Visible_Refinement",          Flag263 (Id));
      W ("Has_Volatile_Components",         Flag87  (Id));
      W ("Has_Xref_Entry",                  Flag182 (Id));
      W ("In_Package_Body",                 Flag48  (Id));
      W ("In_Private_Part",                 Flag45  (Id));
      W ("In_Use",                          Flag8   (Id));
      W ("Is_Abstract_Subprogram",          Flag19  (Id));
      W ("Is_Abstract_Type",                Flag146 (Id));
      W ("Is_Access_Constant",              Flag69  (Id));
      W ("Is_Ada_2005_Only",                Flag185 (Id));
      W ("Is_Ada_2012_Only",                Flag199 (Id));
      W ("Is_Aliased",                      Flag15  (Id));
      W ("Is_Asynchronous",                 Flag81  (Id));
      W ("Is_Atomic",                       Flag85  (Id));
      W ("Is_Bit_Packed_Array",             Flag122 (Id));
      W ("Is_CPP_Class",                    Flag74  (Id));
      W ("Is_Called",                       Flag102 (Id));
      W ("Is_Character_Type",               Flag63  (Id));
      W ("Is_Checked_Ghost_Entity",         Flag277 (Id));
      W ("Is_Child_Unit",                   Flag73  (Id));
      W ("Is_Class_Wide_Equivalent_Type",   Flag35  (Id));
      W ("Is_Compilation_Unit",             Flag149 (Id));
      W ("Is_Completely_Hidden",            Flag103 (Id));
      W ("Is_Concurrent_Record_Type",       Flag20  (Id));
      W ("Is_Constr_Subt_For_UN_Aliased",   Flag141 (Id));
      W ("Is_Constr_Subt_For_U_Nominal",    Flag80  (Id));
      W ("Is_Constrained",                  Flag12  (Id));
      W ("Is_Constructor",                  Flag76  (Id));
      W ("Is_Controlled",                   Flag42  (Id));
      W ("Is_Controlling_Formal",           Flag97  (Id));
      W ("Is_Default_Init_Cond_Procedure",  Flag132 (Id));
      W ("Is_Descendant_Of_Address",        Flag223 (Id));
      W ("Is_Discrim_SO_Function",          Flag176 (Id));
      W ("Is_Discriminant_Check_Function",  Flag264 (Id));
      W ("Is_Dispatch_Table_Entity",        Flag234 (Id));
      W ("Is_Dispatching_Operation",        Flag6   (Id));
      W ("Is_Eliminated",                   Flag124 (Id));
      W ("Is_Entry_Formal",                 Flag52  (Id));
      W ("Is_Exception_Handler",            Flag286 (Id));
      W ("Is_Exported",                     Flag99  (Id));
      W ("Is_First_Subtype",                Flag70  (Id));
      W ("Is_For_Access_Subtype",           Flag118 (Id));
      W ("Is_Formal_Subprogram",            Flag111 (Id));
      W ("Is_Frozen",                       Flag4   (Id));
      W ("Is_Generic_Actual_Subprogram",    Flag274 (Id));
      W ("Is_Generic_Actual_Type",          Flag94  (Id));
      W ("Is_Generic_Instance",             Flag130 (Id));
      W ("Is_Generic_Type",                 Flag13  (Id));
      W ("Is_Hidden",                       Flag57  (Id));
      W ("Is_Hidden_Non_Overridden_Subpgm", Flag2   (Id));
      W ("Is_Hidden_Open_Scope",            Flag171 (Id));
      W ("Is_Ignored_Ghost_Entity",         Flag278 (Id));
      W ("Is_Immediately_Visible",          Flag7   (Id));
      W ("Is_Implementation_Defined",       Flag254 (Id));
      W ("Is_Imported",                     Flag24  (Id));
      W ("Is_Independent",                  Flag268 (Id));
      W ("Is_Inlined",                      Flag11  (Id));
      W ("Is_Inlined_Always",               Flag1   (Id));
      W ("Is_Instantiated",                 Flag126 (Id));
      W ("Is_Interface",                    Flag186 (Id));
      W ("Is_Internal",                     Flag17  (Id));
      W ("Is_Interrupt_Handler",            Flag89  (Id));
      W ("Is_Intrinsic_Subprogram",         Flag64  (Id));
      W ("Is_Invariant_Procedure",          Flag257 (Id));
      W ("Is_Itype",                        Flag91  (Id));
      W ("Is_Known_Non_Null",               Flag37  (Id));
      W ("Is_Known_Null",                   Flag204 (Id));
      W ("Is_Known_Valid",                  Flag170 (Id));
      W ("Is_Limited_Composite",            Flag106 (Id));
      W ("Is_Limited_Interface",            Flag197 (Id));
      W ("Is_Limited_Record",               Flag25  (Id));
      W ("Is_Local_Anonymous_Access",       Flag194 (Id));
      W ("Is_Machine_Code_Subprogram",      Flag137 (Id));
      W ("Is_Non_Static_Subtype",           Flag109 (Id));
      W ("Is_Null_Init_Proc",               Flag178 (Id));
      W ("Is_Obsolescent",                  Flag153 (Id));
      W ("Is_Only_Out_Parameter",           Flag226 (Id));
      W ("Is_Package_Body_Entity",          Flag160 (Id));
      W ("Is_Packed",                       Flag51  (Id));
      W ("Is_Packed_Array_Impl_Type",       Flag138 (Id));
      W ("Is_Param_Block_Component_Type",   Flag215 (Id));
      W ("Is_Potentially_Use_Visible",      Flag9   (Id));
      W ("Is_Predicate_Function",           Flag255 (Id));
      W ("Is_Predicate_Function_M",         Flag256 (Id));
      W ("Is_Preelaborated",                Flag59  (Id));
      W ("Is_Primitive",                    Flag218 (Id));
      W ("Is_Primitive_Wrapper",            Flag195 (Id));
      W ("Is_Private_Composite",            Flag107 (Id));
      W ("Is_Private_Descendant",           Flag53  (Id));
      W ("Is_Private_Primitive",            Flag245 (Id));
      W ("Is_Processed_Transient",          Flag252 (Id));
      W ("Is_Public",                       Flag10  (Id));
      W ("Is_Pure",                         Flag44  (Id));
      W ("Is_Pure_Unit_Access_Type",        Flag189 (Id));
      W ("Is_RACW_Stub_Type",               Flag244 (Id));
      W ("Is_Raised",                       Flag224 (Id));
      W ("Is_Remote_Call_Interface",        Flag62  (Id));
      W ("Is_Remote_Types",                 Flag61  (Id));
      W ("Is_Renaming_Of_Object",           Flag112 (Id));
      W ("Is_Return_Object",                Flag209 (Id));
      W ("Is_Safe_To_Reevaluate",           Flag249 (Id));
      W ("Is_Shared_Passive",               Flag60  (Id));
      W ("Is_Static_Type",                  Flag281 (Id));
      W ("Is_Statically_Allocated",         Flag28  (Id));
      W ("Is_Tag",                          Flag78  (Id));
      W ("Is_Tagged_Type",                  Flag55  (Id));
      W ("Is_Thunk",                        Flag225 (Id));
      W ("Is_Trivial_Subprogram",           Flag235 (Id));
      W ("Is_True_Constant",                Flag163 (Id));
      W ("Is_Unchecked_Union",              Flag117 (Id));
      W ("Is_Underlying_Record_View",       Flag246 (Id));
      W ("Is_Unimplemented",                Flag284 (Id));
      W ("Is_Unsigned_Type",                Flag144 (Id));
      W ("Is_Uplevel_Referenced_Entity",    Flag283 (Id));
      W ("Is_Valued_Procedure",             Flag127 (Id));
      W ("Is_Visible_Formal",               Flag206 (Id));
      W ("Is_Visible_Lib_Unit",             Flag116 (Id));
      W ("Is_Volatile",                     Flag16  (Id));
      W ("Is_Volatile_Full_Access",         Flag285 (Id));
      W ("Itype_Printed",                   Flag202 (Id));
      W ("Kill_Elaboration_Checks",         Flag32  (Id));
      W ("Kill_Range_Checks",               Flag33  (Id));
      W ("Known_To_Have_Preelab_Init",      Flag207 (Id));
      W ("Low_Bound_Tested",                Flag205 (Id));
      W ("Machine_Radix_10",                Flag84  (Id));
      W ("Materialize_Entity",              Flag168 (Id));
      W ("May_Inherit_Delayed_Rep_Aspects", Flag262 (Id));
      W ("Must_Be_On_Byte_Boundary",        Flag183 (Id));
      W ("Must_Have_Preelab_Init",          Flag208 (Id));
      W ("Needs_Debug_Info",                Flag147 (Id));
      W ("Needs_No_Actuals",                Flag22  (Id));
      W ("Never_Set_In_Source",             Flag115 (Id));
      W ("No_Dynamic_Predicate_On_actual",  Flag276 (Id));
      W ("No_Pool_Assigned",                Flag131 (Id));
      W ("No_Predicate_On_actual",          Flag275 (Id));
      W ("No_Return",                       Flag113 (Id));
      W ("No_Strict_Aliasing",              Flag136 (Id));
      W ("Non_Binary_Modulus",              Flag58  (Id));
      W ("Nonzero_Is_True",                 Flag162 (Id));
      W ("OK_To_Rename",                    Flag247 (Id));
      W ("OK_To_Reorder_Components",        Flag239 (Id));
      W ("Optimize_Alignment_Space",        Flag241 (Id));
      W ("Optimize_Alignment_Time",         Flag242 (Id));
      W ("Overlays_Constant",               Flag243 (Id));
      W ("Partial_View_Has_Unknown_Discr",  Flag280 (Id));
      W ("Reachable",                       Flag49  (Id));
      W ("Referenced",                      Flag156 (Id));
      W ("Referenced_As_LHS",               Flag36  (Id));
      W ("Referenced_As_Out_Parameter",     Flag227 (Id));
      W ("Renamed_In_Spec",                 Flag231 (Id));
      W ("Requires_Overriding",             Flag213 (Id));
      W ("Return_Present",                  Flag54  (Id));
      W ("Returns_By_Ref",                  Flag90  (Id));
      W ("Returns_Limited_View",            Flag134 (Id));
      W ("Reverse_Bit_Order",               Flag164 (Id));
      W ("Reverse_Storage_Order",           Flag93  (Id));
      W ("Rewritten_For_C",                 Flag287 (Id));
      W ("Sec_Stack_Needed_For_Return",     Flag167 (Id));
      W ("Size_Depends_On_Discriminant",    Flag177 (Id));
      W ("Size_Known_At_Compile_Time",      Flag92  (Id));
      W ("SPARK_Aux_Pragma_Inherited",      Flag266 (Id));
      W ("SPARK_Pragma_Inherited",          Flag265 (Id));
      W ("SSO_Set_High_By_Default",         Flag273 (Id));
      W ("SSO_Set_Low_By_Default",          Flag272 (Id));
      W ("Static_Elaboration_Desired",      Flag77  (Id));
      W ("Stores_Attribute_Old_Prefix",     Flag270 (Id));
      W ("Strict_Alignment",                Flag145 (Id));
      W ("Suppress_Elaboration_Warnings",   Flag148 (Id));
      W ("Suppress_Initialization",         Flag105 (Id));
      W ("Suppress_Style_Checks",           Flag165 (Id));
      W ("Suppress_Value_Tracking_On_Call", Flag217 (Id));
      W ("Treat_As_Volatile",               Flag41  (Id));
      W ("Universal_Aliasing",              Flag216 (Id));
      W ("Used_As_Generic_Actual",          Flag222 (Id));
      W ("Uses_Sec_Stack",                  Flag95  (Id));
      W ("Warnings_Off",                    Flag96  (Id));
      W ("Warnings_Off_Used",               Flag236 (Id));
      W ("Warnings_Off_Used_Unmodified",    Flag237 (Id));
      W ("Warnings_Off_Used_Unreferenced",  Flag238 (Id));
      W ("Was_Hidden",                      Flag196 (Id));
   end Write_Entity_Flags;

   -----------------------
   -- Write_Entity_Info --
   -----------------------

   procedure Write_Entity_Info (Id : Entity_Id; Prefix : String) is

      procedure Write_Attribute (Which : String; Nam : E);
      --  Write attribute value with given string name

      procedure Write_Kind (Id : Entity_Id);
      --  Write Ekind field of entity

      ---------------------
      -- Write_Attribute --
      ---------------------

      procedure Write_Attribute (Which : String; Nam : E) is
      begin
         Write_Str (Prefix);
         Write_Str (Which);
         Write_Int (Int (Nam));
         Write_Str (" ");
         Write_Name (Chars (Nam));
         Write_Str (" ");
      end Write_Attribute;

      ----------------
      -- Write_Kind --
      ----------------

      procedure Write_Kind (Id : Entity_Id) is
         K : constant String := Entity_Kind'Image (Ekind (Id));

      begin
         Write_Str (Prefix);
         Write_Str ("   Kind    ");

         if Is_Type (Id) and then Is_Tagged_Type (Id) then
            Write_Str ("TAGGED ");
         end if;

         Write_Str (K (3 .. K'Length));
         Write_Str (" ");

         if Is_Type (Id) and then Depends_On_Private (Id) then
            Write_Str ("Depends_On_Private ");
         end if;
      end Write_Kind;

   --  Start of processing for Write_Entity_Info

   begin
      Write_Eol;
      Write_Attribute ("Name ", Id);
      Write_Int (Int (Id));
      Write_Eol;
      Write_Kind (Id);
      Write_Eol;
      Write_Attribute ("   Type    ", Etype (Id));
      Write_Eol;
      Write_Attribute ("   Scope   ", Scope (Id));
      Write_Eol;

      case Ekind (Id) is

         when Discrete_Kind =>
            Write_Str ("Bounds: Id = ");

            if Present (Scalar_Range (Id)) then
               Write_Int (Int (Type_Low_Bound (Id)));
               Write_Str (" .. Id = ");
               Write_Int (Int (Type_High_Bound (Id)));
            else
               Write_Str ("Empty");
            end if;

            Write_Eol;

         when Array_Kind =>
            declare
               Index : E;

            begin
               Write_Attribute
                 ("   Component Type    ", Component_Type (Id));
               Write_Eol;
               Write_Str (Prefix);
               Write_Str ("   Indexes ");

               Index := First_Index (Id);
               while Present (Index) loop
                  Write_Attribute (" ", Etype (Index));
                  Index := Next_Index (Index);
               end loop;

               Write_Eol;
            end;

         when Access_Kind =>
               Write_Attribute
                 ("   Directly Designated Type ",
                  Directly_Designated_Type (Id));
               Write_Eol;

         when Overloadable_Kind =>
            if Present (Homonym (Id)) then
               Write_Str ("   Homonym   ");
               Write_Name (Chars (Homonym (Id)));
               Write_Str ("   ");
               Write_Int (Int (Homonym (Id)));
               Write_Eol;
            end if;

            Write_Eol;

         when E_Component =>
            if Ekind (Scope (Id)) in Record_Kind then
               Write_Attribute (
                  "   Original_Record_Component   ",
                  Original_Record_Component (Id));
               Write_Int (Int (Original_Record_Component (Id)));
               Write_Eol;
            end if;

         when others => null;
      end case;
   end Write_Entity_Info;

   -----------------------
   -- Write_Field6_Name --
   -----------------------

   procedure Write_Field6_Name (Id : Entity_Id) is
      pragma Unreferenced (Id);
   begin
      Write_Str ("First_Rep_Item");
   end Write_Field6_Name;

   -----------------------
   -- Write_Field7_Name --
   -----------------------

   procedure Write_Field7_Name (Id : Entity_Id) is
      pragma Unreferenced (Id);
   begin
      Write_Str ("Freeze_Node");
   end Write_Field7_Name;

   -----------------------
   -- Write_Field8_Name --
   -----------------------

   procedure Write_Field8_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when Type_Kind                                    =>
            Write_Str ("Associated_Node_For_Itype");

         when E_Package                                    =>
            Write_Str ("Dependent_Instances");

         when E_Loop                                       =>
            Write_Str ("First_Exit_Statement");

         when E_Variable                                   =>
            Write_Str ("Hiding_Loop_Variable");

         when Formal_Kind                                  |
              E_Function                                   |
              E_Subprogram_Body                            =>
            Write_Str ("Mechanism");

         when E_Component                                  |
              E_Discriminant                               =>
            Write_Str ("Normalized_First_Bit");

         when E_Abstract_State                             =>
            Write_Str ("Refinement_Constituents");

         when E_Return_Statement                           =>
            Write_Str ("Return_Applies_To");

         when others                                       =>
            Write_Str ("Field8??");
      end case;
   end Write_Field8_Name;

   -----------------------
   -- Write_Field9_Name --
   -----------------------

   procedure Write_Field9_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when Type_Kind                                    =>
            Write_Str ("Class_Wide_Type");

         when Object_Kind                                  =>
            Write_Str ("Current_Value");

         when E_Function                                   |
              E_Generic_Function                           |
              E_Generic_Package                            |
              E_Generic_Procedure                          |
              E_Package                                    |
              E_Procedure                                  =>
            Write_Str ("Renaming_Map");

         when others                                       =>
            Write_Str ("Field9??");
      end case;
   end Write_Field9_Name;

   ------------------------
   -- Write_Field10_Name --
   ------------------------

   procedure Write_Field10_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when Class_Wide_Kind                              |
              Incomplete_Kind                              |
              E_Record_Type                                |
              E_Record_Subtype                             |
              Private_Kind                                 |
              Concurrent_Kind                              =>
            Write_Str ("Direct_Primitive_Operations");

         when E_In_Parameter                               |
              E_Constant                                   =>
            Write_Str ("Discriminal_Link");

         when Float_Kind                                   =>
            Write_Str ("Float_Rep");

         when E_Function                                   |
              E_Package                                    |
              E_Package_Body                               |
              E_Procedure                                  =>
            Write_Str ("Handler_Records");

         when E_Component                                  |
              E_Discriminant                               =>
            Write_Str ("Normalized_Position_Max");

         when E_Abstract_State                             |
              E_Variable                                   =>
            Write_Str ("Part_Of_Constituents");

         when others                                       =>
            Write_Str ("Field10??");
      end case;
   end Write_Field10_Name;

   ------------------------
   -- Write_Field11_Name --
   ------------------------

   procedure Write_Field11_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when E_Block                                      =>
            Write_Str ("Block_Node");

         when E_Component                                  |
              E_Discriminant                               =>
            Write_Str ("Component_Bit_Offset");

         when Formal_Kind                                  =>
            Write_Str ("Entry_Component");

         when E_Enumeration_Literal                        =>
            Write_Str ("Enumeration_Pos");

         when Type_Kind                                    |
              E_Constant                                   =>
            Write_Str ("Full_View");

         when E_Generic_Package                            =>
            Write_Str ("Generic_Homonym");

         when E_Variable                                   =>
            Write_Str ("Part_Of_References");

         when E_Entry                                      |
              E_Entry_Family                               |
              E_Function                                   |
              E_Procedure                                  =>
            Write_Str ("Protected_Body_Subprogram");

         when others                                       =>
            Write_Str ("Field11??");
      end case;
   end Write_Field11_Name;

   ------------------------
   -- Write_Field12_Name --
   ------------------------

   procedure Write_Field12_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when E_Package                                    =>
            Write_Str ("Associated_Formal_Package");

         when Entry_Kind                                   =>
            Write_Str ("Barrier_Function");

         when E_Enumeration_Literal                        =>
            Write_Str ("Enumeration_Rep");

         when Type_Kind                                    |
              E_Component                                  |
              E_Constant                                   |
              E_Discriminant                               |
              E_Exception                                  |
              E_In_Parameter                               |
              E_In_Out_Parameter                           |
              E_Out_Parameter                              |
              E_Loop_Parameter                             |
              E_Variable                                   =>
            Write_Str ("Esize");

         when E_Function                                   |
              E_Procedure                                  =>
            Write_Str ("Next_Inlined_Subprogram");

         when others                                       =>
            Write_Str ("Field12??");
      end case;
   end Write_Field12_Name;

   ------------------------
   -- Write_Field13_Name --
   ------------------------

   procedure Write_Field13_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when E_Component                                  |
              E_Discriminant                               =>
            Write_Str ("Component_Clause");

         when E_Function                                   =>
            Write_Str ("Elaboration_Entity");

         when E_Procedure                                  |
              E_Package                                    |
              Generic_Unit_Kind                            =>
            Write_Str ("Elaboration_Entity");

         when Formal_Kind                                  |
              E_Variable                                   =>
            Write_Str ("Extra_Accessibility");

         when Type_Kind                                    =>
            Write_Str ("RM_Size");

         when others                                       =>
            Write_Str ("Field13??");
      end case;
   end Write_Field13_Name;

   -----------------------
   -- Write_Field14_Name --
   -----------------------

   procedure Write_Field14_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when Type_Kind                                    |
              Formal_Kind                                  |
              E_Constant                                   |
              E_Exception                                  |
              E_Loop_Parameter                             |
              E_Variable                                   =>
            Write_Str ("Alignment");

         when E_Component                                  |
              E_Discriminant                               =>
            Write_Str ("Normalized_Position");

         when E_Entry                                      |
              E_Entry_Family                               |
              E_Function                                   |
              E_Procedure                                  =>
            Write_Str ("Postconditions_Proc");

         when E_Generic_Package                            |
              E_Package                                    =>
            Write_Str ("Shadow_Entities");

         when others                                       =>
            Write_Str ("Field14??");
      end case;
   end Write_Field14_Name;

   ------------------------
   -- Write_Field15_Name --
   ------------------------

   procedure Write_Field15_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when E_Discriminant                               =>
            Write_Str ("Discriminant_Number");

         when E_Component                                  =>
            Write_Str ("DT_Entry_Count");

         when E_Function                                   |
              E_Procedure                                  =>
            Write_Str ("DT_Position");

         when Entry_Kind                                   =>
            Write_Str ("Entry_Parameters_Type");

         when Formal_Kind                                  =>
            Write_Str ("Extra_Formal");

         when Type_Kind                                    =>
            Write_Str ("Pending_Access_Types");

         when E_Package                                    |
              E_Package_Body                               =>
            Write_Str ("Related_Instance");

         when E_Constant                                   |
              E_Variable                                   =>
            Write_Str ("Status_Flag_Or_Transient_Decl");

         when others                                       =>
            Write_Str ("Field15??");
      end case;
   end Write_Field15_Name;

   ------------------------
   -- Write_Field16_Name --
   ------------------------

   procedure Write_Field16_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when E_Record_Type                                |
              E_Record_Type_With_Private                   =>
            Write_Str ("Access_Disp_Table");

         when E_Abstract_State                             =>
            Write_Str ("Body_References");

         when E_Record_Subtype                             |
              E_Class_Wide_Subtype                         =>
            Write_Str ("Cloned_Subtype");

         when E_Function                                   |
              E_Procedure                                  =>
            Write_Str ("DTC_Entity");

         when E_Component                                  =>
            Write_Str ("Entry_Formal");

         when E_Package                                    |
              E_Generic_Package                            |
              Concurrent_Kind                              =>
            Write_Str ("First_Private_Entity");

         when Enumeration_Kind                             =>
            Write_Str ("Lit_Strings");

         when Decimal_Fixed_Point_Kind                     =>
            Write_Str ("Scale_Value");

         when E_String_Literal_Subtype                     =>
            Write_Str ("String_Literal_Length");

         when E_Variable                                   |
              E_Out_Parameter                              =>
            Write_Str ("Unset_Reference");

         when others                                       =>
            Write_Str ("Field16??");
      end case;
   end Write_Field16_Name;

   ------------------------
   -- Write_Field17_Name --
   ------------------------

   procedure Write_Field17_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when Formal_Kind                                  |
              E_Constant                                   |
              E_Generic_In_Out_Parameter                   |
              E_Variable                                   =>
            Write_Str ("Actual_Subtype");

         when Digits_Kind                                  =>
            Write_Str ("Digits_Value");

         when E_Discriminant                               =>
            Write_Str ("Discriminal");

         when E_Block                                      |
              Class_Wide_Kind                              |
              Concurrent_Kind                              |
              Private_Kind                                 |
              E_Entry                                      |
              E_Entry_Family                               |
              E_Function                                   |
              E_Generic_Function                           |
              E_Generic_Package                            |
              E_Generic_Procedure                          |
              E_Loop                                       |
              E_Operator                                   |
              E_Package                                    |
              E_Package_Body                               |
              E_Procedure                                  |
              E_Record_Type                                |
              E_Record_Subtype                             |
              E_Return_Statement                           |
              E_Subprogram_Body                            |
              E_Subprogram_Type                            =>
            Write_Str ("First_Entity");

         when Array_Kind                                   =>
            Write_Str ("First_Index");

         when Enumeration_Kind                             =>
            Write_Str ("First_Literal");

         when Access_Kind                                  =>
            Write_Str ("Master_Id");

         when Modular_Integer_Kind                         =>
            Write_Str ("Modulus");

         when E_Component                                  =>
            Write_Str ("Prival");

         when others                                       =>
            Write_Str ("Field17??");
      end case;
   end Write_Field17_Name;

   ------------------------
   -- Write_Field18_Name --
   ------------------------

   procedure Write_Field18_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when E_Enumeration_Literal                        |
              E_Function                                   |
              E_Operator                                   |
              E_Procedure                                  =>
            Write_Str ("Alias");

         when E_Record_Type                                =>
            Write_Str ("Corresponding_Concurrent_Type");

         when E_Subprogram_Body                            =>
            Write_Str ("Corresponding_Protected_Entry");

         when Concurrent_Kind                              =>
            Write_Str ("Corresponding_Record_Type");

         when E_Label                                      |
              E_Loop                                       |
              E_Block                                      =>
            Write_Str ("Enclosing_Scope");

         when E_Entry_Index_Parameter                      =>
            Write_Str ("Entry_Index_Constant");

         when E_Class_Wide_Subtype                         |
              E_Access_Protected_Subprogram_Type           |
              E_Anonymous_Access_Protected_Subprogram_Type |
              E_Access_Subprogram_Type                     |
              E_Exception_Type                             =>
            Write_Str ("Equivalent_Type");

         when Fixed_Point_Kind                             =>
            Write_Str ("Delta_Value");

         when Enumeration_Kind                             =>
            Write_Str ("Lit_Indexes");

         when Incomplete_Or_Private_Kind                   |
              E_Record_Subtype                             =>
            Write_Str ("Private_Dependents");

         when Object_Kind                                  =>
            Write_Str ("Renamed_Object");

         when E_Exception                                  |
              E_Package                                    |
              E_Generic_Function                           |
              E_Generic_Procedure                          |
              E_Generic_Package                            =>
            Write_Str ("Renamed_Entity");

         when E_String_Literal_Subtype                     =>
            Write_Str ("String_Literal_Low_Bound");

         when others                                       =>
            Write_Str ("Field18??");
      end case;
   end Write_Field18_Name;

   -----------------------
   -- Write_Field19_Name --
   -----------------------

   procedure Write_Field19_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when E_Package                                    |
              E_Generic_Package                            =>
            Write_Str ("Body_Entity");

         when E_Discriminant                               =>
            Write_Str ("Corresponding_Discriminant");

         when Scalar_Kind                                  =>
            Write_Str ("Default_Aspect_Value");

         when E_Abstract_State                             |
              E_Class_Wide_Type                            |
              E_Incomplete_Type                            =>
            Write_Str ("Non_Limited_View");

         when E_Incomplete_Subtype                         =>
            if From_Limited_With (Id) then
               Write_Str ("Non_Limited_View");
            end if;

         when E_Array_Type                                 =>
            Write_Str ("Default_Component_Value");

         when E_Protected_Type                             =>
            Write_Str ("Entry_Bodies_Array");

         when E_Function                                   |
              E_Operator                                   |
              E_Subprogram_Type                            =>
            Write_Str ("Extra_Accessibility_Of_Result");

         when E_Record_Type                                =>
            Write_Str ("Parent_Subtype");

         when E_Constant                                   |
              E_Variable                                   =>
            Write_Str ("Size_Check_Code");

         when E_Package_Body                               |
              Formal_Kind                                  =>
            Write_Str ("Spec_Entity");

         when Private_Kind                                 =>
            Write_Str ("Underlying_Full_View");

         when others                                       =>
            Write_Str ("Field19??");
      end case;
   end Write_Field19_Name;

   -----------------------
   -- Write_Field20_Name --
   -----------------------

   procedure Write_Field20_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when Array_Kind                                   =>
            Write_Str ("Component_Type");

         when E_In_Parameter                               |
              E_Generic_In_Parameter                       =>
            Write_Str ("Default_Value");

         when Access_Kind                                  =>
            Write_Str ("Directly_Designated_Type");

         when E_Component                                  =>
            Write_Str ("Discriminant_Checking_Func");

         when E_Discriminant                               =>
            Write_Str ("Discriminant_Default_Value");

         when E_Block                                      |
              Class_Wide_Kind                              |
              Concurrent_Kind                              |
              Private_Kind                                 |
              E_Entry                                      |
              E_Entry_Family                               |
              E_Function                                   |
              E_Generic_Function                           |
              E_Generic_Package                            |
              E_Generic_Procedure                          |
              E_Loop                                       |
              E_Operator                                   |
              E_Package                                    |
              E_Package_Body                               |
              E_Procedure                                  |
              E_Record_Type                                |
              E_Record_Subtype                             |
              E_Return_Statement                           |
              E_Subprogram_Body                            |
              E_Subprogram_Type                            =>
            Write_Str ("Last_Entity");

         when E_Constant                                   |
              E_Variable                                   =>
            Write_Str ("Prival_Link");

         when Scalar_Kind                                  =>
            Write_Str ("Scalar_Range");

         when E_Exception                                  =>
            Write_Str ("Register_Exception_Call");

         when others                                       =>
            Write_Str ("Field20??");
      end case;
   end Write_Field20_Name;

   -----------------------
   -- Write_Field21_Name --
   -----------------------

   procedure Write_Field21_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when Entry_Kind                                   =>
            Write_Str ("Accept_Address");

         when E_In_Parameter                               =>
            Write_Str ("Default_Expr_Function");

         when Concurrent_Kind                              |
              Incomplete_Or_Private_Kind                   |
              Class_Wide_Kind                              |
              E_Record_Type                                |
              E_Record_Subtype                             =>
            Write_Str ("Discriminant_Constraint");

         when E_Constant                                   |
              E_Exception                                  |
              E_Function                                   |
              E_Generic_Function                           |
              E_Procedure                                  |
              E_Generic_Procedure                          |
              E_Variable                                   =>
            Write_Str ("Interface_Name");

         when Array_Kind                                   |
              Modular_Integer_Kind                         =>
            Write_Str ("Original_Array_Type");

         when Fixed_Point_Kind                             =>
            Write_Str ("Small_Value");

         when others                                       =>
            Write_Str ("Field21??");
      end case;
   end Write_Field21_Name;

   -----------------------
   -- Write_Field22_Name --
   -----------------------

   procedure Write_Field22_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when Access_Kind                                  =>
            Write_Str ("Associated_Storage_Pool");

         when Array_Kind                                   =>
            Write_Str ("Component_Size");

         when E_Record_Type                                =>
            Write_Str ("Corresponding_Remote_Type");

         when E_Component                                  |
              E_Discriminant                               =>
            Write_Str ("Original_Record_Component");

         when E_Enumeration_Literal                        =>
            Write_Str ("Enumeration_Rep_Expr");

         when E_Record_Type_With_Private                   |
              E_Record_Subtype_With_Private                |
              E_Private_Type                               |
              E_Private_Subtype                            |
              E_Limited_Private_Type                       |
              E_Limited_Private_Subtype                    =>
            Write_Str ("Private_View");

         when Formal_Kind                                  =>
            Write_Str ("Protected_Formal");

         when E_Block                                      |
              E_Entry                                      |
              E_Entry_Family                               |
              E_Function                                   |
              E_Loop                                       |
              E_Package                                    |
              E_Package_Body                               |
              E_Generic_Package                            |
              E_Generic_Function                           |
              E_Generic_Procedure                          |
              E_Procedure                                  |
              E_Protected_Type                             |
              E_Return_Statement                           |
              E_Subprogram_Body                            |
              E_Task_Type                                  =>
            Write_Str ("Scope_Depth_Value");

         when E_Variable                                   =>
            Write_Str ("Shared_Var_Procs_Instance");

         when others                                       =>
            Write_Str ("Field22??");
      end case;
   end Write_Field22_Name;

   ------------------------
   -- Write_Field23_Name --
   ------------------------

   procedure Write_Field23_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when E_Discriminant                               =>
            Write_Str ("CR_Discriminant");

         when E_Block                                      =>
            Write_Str ("Entry_Cancel_Parameter");

         when E_Enumeration_Type                           =>
            Write_Str ("Enum_Pos_To_Rep");

         when Formal_Kind                                  |
              E_Variable                                   =>
            Write_Str ("Extra_Constrained");

         when Access_Kind                                  =>
            Write_Str ("Finalization_Master");

         when E_Generic_Function                           |
              E_Generic_Package                            |
              E_Generic_Procedure                          =>
            Write_Str ("Inner_Instances");

         when Array_Kind                                   =>
            Write_Str ("Packed_Array_Impl_Type");

         when Entry_Kind                                   =>
            Write_Str ("Protection_Object");

         when Concurrent_Kind                              |
              Incomplete_Or_Private_Kind                   |
              Class_Wide_Kind                              |
              E_Record_Type                                |
              E_Record_Subtype                             =>
            Write_Str ("Stored_Constraint");

         when E_Function                                   |
              E_Procedure                                  =>
            if Present (Scope (Id))
              and then Is_Protected_Type (Scope (Id))
            then
               Write_Str ("Protection_Object");
            else
               Write_Str ("Generic_Renamings");
            end if;

         when E_Package                                    =>
            if Is_Generic_Instance (Id) then
               Write_Str ("Generic_Renamings");
            else
               Write_Str ("Limited_View");
            end if;

         when others                                       =>
            Write_Str ("Field23??");
      end case;
   end Write_Field23_Name;

   ------------------------
   -- Write_Field24_Name --
   ------------------------

   procedure Write_Field24_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when E_Constant                                   |
              E_Variable                                   |
              Type_Kind                                    =>
            Write_Str ("Related_Expression");

         when E_Function                                   |
              E_Operator                                   |
              E_Procedure                                  =>
            Write_Str ("Subps_Index");

         when E_Package                                    =>
            Write_Str ("Incomplete_Actuals");

         when others                                       =>
            Write_Str ("Field24???");
      end case;
   end Write_Field24_Name;

   ------------------------
   -- Write_Field25_Name --
   ------------------------

   procedure Write_Field25_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when E_Generic_Package                            |
              E_Package                                    =>
            Write_Str ("Abstract_States");

         when E_Entry                                      |
              E_Entry_Family                               =>
            Write_Str ("Contract_Wrapper");

         when E_Variable                                   =>
            Write_Str ("Debug_Renaming_Link");

         when E_Component                                  =>
            Write_Str ("DT_Offset_To_Top_Func");

         when E_Procedure                                  |
              E_Function                                   =>
            Write_Str ("Interface_Alias");

         when E_Record_Type                                |
              E_Record_Subtype                             |
              E_Record_Type_With_Private                   |
              E_Record_Subtype_With_Private                =>
            Write_Str ("Interfaces");

         when E_Array_Type                                 |
              E_Array_Subtype                              =>
            Write_Str ("Related_Array_Object");

         when Task_Kind                                    =>
            Write_Str ("Task_Body_Procedure");

         when Discrete_Kind                                =>
            Write_Str ("Static_Discrete_Predicate");

         when Real_Kind                                    =>
            Write_Str ("Static_Real_Or_String_Predicate");

         when others                                       =>
            Write_Str ("Field25??");
      end case;
   end Write_Field25_Name;

   ------------------------
   -- Write_Field26_Name --
   ------------------------

   procedure Write_Field26_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when E_Record_Type                                |
              E_Record_Type_With_Private                   =>
            Write_Str ("Dispatch_Table_Wrappers");

         when E_In_Out_Parameter                           |
              E_Out_Parameter                              |
              E_Variable                                   =>
            Write_Str ("Last_Assignment");

         when E_Procedure                                  |
              E_Function                                   =>
            Write_Str ("Overridden_Operation");

         when E_Generic_Package                            |
              E_Package                                    =>
            Write_Str ("Package_Instantiation");

         when E_Component                                  |
              E_Constant                                   =>
            Write_Str ("Related_Type");

         when Access_Kind                                  |
              Task_Kind                                    =>
            Write_Str ("Storage_Size_Variable");

         when others                                       =>
            Write_Str ("Field26??");
      end case;
   end Write_Field26_Name;

   ------------------------
   -- Write_Field27_Name --
   ------------------------

   procedure Write_Field27_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when E_Package                                    |
              Type_Kind                                    =>
            Write_Str ("Current_Use_Clause");

         when E_Component                                  |
              E_Constant                                   |
              E_Variable                                   =>
            Write_Str ("Related_Type");

         when E_Procedure                                  |
              E_Function                                   =>
            Write_Str ("Wrapped_Entity");

         when others                                       =>
            Write_Str ("Field27??");
      end case;
   end Write_Field27_Name;

   ------------------------
   -- Write_Field28_Name --
   ------------------------

   procedure Write_Field28_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when E_Entry                                      |
              E_Entry_Family                               |
              E_Function                                   |
              E_Procedure                                  |
              E_Subprogram_Body                            |
              E_Subprogram_Type                            =>
            Write_Str ("Extra_Formals");

         when E_Package                                    |
              E_Package_Body                               =>
            Write_Str ("Finalizer");

         when E_Constant                                   |
              E_Variable                                   =>
            Write_Str ("Initialization_Statements");

         when E_Access_Subprogram_Type                     =>
            Write_Str ("Original_Access_Type");

         when Task_Kind                                    =>
            Write_Str ("Relative_Deadline_Variable");

         when E_Record_Type =>
            Write_Str ("Underlying_Record_View");

         when others                                       =>
            Write_Str ("Field28??");
      end case;
   end Write_Field28_Name;

   ------------------------
   -- Write_Field29_Name --
   ------------------------

   procedure Write_Field29_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when E_Constant                                   |
              E_Variable                                   =>
            Write_Str ("BIP_Initialization_Call");

         when Type_Kind =>
            Write_Str ("Subprograms_For_Type");

         when others                                       =>
            Write_Str ("Field29??");
      end case;
   end Write_Field29_Name;

   ------------------------
   -- Write_Field30_Name --
   ------------------------

   procedure Write_Field30_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when E_Protected_Type                             |
              E_Task_Type                                  =>
            Write_Str ("Anonymous_Object");

         when E_Function                                   =>
            Write_Str ("Corresponding_Equality");

         when E_Constant                                   |
              E_Variable                                   =>
            Write_Str ("Last_Aggregate_Assignment");

         when E_Procedure                                  =>
            Write_Str ("Static_Initialization");

         when others                                       =>
            Write_Str ("Field30??");
      end case;
   end Write_Field30_Name;

   ------------------------
   -- Write_Field31_Name --
   ------------------------

   procedure Write_Field31_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when E_Procedure                                  |
              E_Function                                   =>
            Write_Str ("Thunk_Entity");

         when Type_Kind                                    =>
            Write_Str ("Derived_Type_Link");

         when E_Constant                                   |
              E_In_Parameter                               |
              E_In_Out_Parameter                           |
              E_Loop_Parameter                             |
              E_Out_Parameter                              |
              E_Variable                                   =>
            Write_Str ("Activation_Record_Component");

         when others                                       =>
            Write_Str ("Field31??");
      end case;
   end Write_Field31_Name;

   ------------------------
   -- Write_Field32_Name --
   ------------------------

   procedure Write_Field32_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when E_Abstract_State                             |
              E_Constant                                   |
              E_Variable                                   =>
            Write_Str ("Encapsulating_State");

         when Type_Kind                                    =>
            Write_Str ("No_Tagged_Streams_Pragma");

         when others                                       =>
            Write_Str ("Field32??");
      end case;
   end Write_Field32_Name;

   ------------------------
   -- Write_Field33_Name --
   ------------------------

   procedure Write_Field33_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when E_Constant                                   |
              E_Variable                                   |
              Subprogram_Kind                              |
              Type_Kind                                    =>
            Write_Str ("Linker_Section_Pragma");

         when others                                       =>
            Write_Str ("Field33??");
      end case;
   end Write_Field33_Name;

   ------------------------
   -- Write_Field34_Name --
   ------------------------

   procedure Write_Field34_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when E_Constant                                   |
              E_Entry                                      |
              E_Entry_Family                               |
              E_Function                                   |
              E_Generic_Function                           |
              E_Generic_Package                            |
              E_Generic_Procedure                          |
              E_Operator                                   |
              E_Package                                    |
              E_Package_Body                               |
              E_Procedure                                  |
              E_Protected_Type                             |
              E_Subprogram_Body                            |
              E_Task_Body                                  |
              E_Task_Type                                  |
              E_Variable                                   |
              E_Void                                       =>
            Write_Str ("Contract");

         when others                                       =>
            Write_Str ("Field34??");
      end case;
   end Write_Field34_Name;

   ------------------------
   -- Write_Field35_Name --
   ------------------------

   procedure Write_Field35_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when Subprogram_Kind                              =>
            Write_Str ("Import_Pragma");

         when others                                       =>
            Write_Str ("Field35??");
      end case;
   end Write_Field35_Name;

   ------------------------
   -- Write_Field36_Name --
   ------------------------

   procedure Write_Field36_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when E_Function                                   |
              E_Operator                                   |
              E_Package                                    |
              E_Package_Body                               |
              E_Procedure                                  |
              E_Subprogram_Body                            =>
            Write_Str ("Anonymous_Master");

         when others                                       =>
            Write_Str ("Field36??");
      end case;
   end Write_Field36_Name;

   ------------------------
   -- Write_Field37_Name --
   ------------------------

   procedure Write_Field37_Name (Id : Entity_Id) is
      pragma Unreferenced (Id);
   begin
      Write_Str ("Associated_Entity");
   end Write_Field37_Name;

   ------------------------
   -- Write_Field38_Name --
   ------------------------

   procedure Write_Field38_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when E_Function                                   |
              E_Procedure                                  =>
            Write_Str ("Class-wide preconditions");

         when others                                       =>
            Write_Str ("Field38??");
      end case;
   end Write_Field38_Name;

   ------------------------
   -- Write_Field39_Name --
   ------------------------

   procedure Write_Field39_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when E_Function                                   |
              E_Procedure                                  =>
            Write_Str ("Class-wide postcondition");

         when others                                       =>
            Write_Str ("Field39??");
      end case;
   end Write_Field39_Name;

   ------------------------
   -- Write_Field40_Name --
   ------------------------

   procedure Write_Field40_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when E_Entry                                      |
              E_Entry_Family                               |
              E_Function                                   |
              E_Generic_Function                           |
              E_Generic_Package                            |
              E_Generic_Procedure                          |
              E_Operator                                   |
              E_Package                                    |
              E_Package_Body                               |
              E_Procedure                                  |
              E_Protected_Body                             |
              E_Protected_Type                             |
              E_Subprogram_Body                            |
              E_Task_Body                                  |
              E_Task_Type                                  |
              E_Variable                                   =>
            Write_Str ("SPARK_Pragma");

         when others                                       =>
            Write_Str ("Field40??");
      end case;
   end Write_Field40_Name;

   ------------------------
   -- Write_Field41_Name --
   ------------------------

   procedure Write_Field41_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when E_Generic_Package                            |
              E_Package                                    |
              E_Package_Body                               |
              E_Protected_Type                             |
              E_Task_Type                                  =>
            Write_Str ("SPARK_Aux_Pragma");

         when others                                       =>
            Write_Str ("Field41??");
      end case;
   end Write_Field41_Name;

   -------------------------
   -- Iterator Procedures --
   -------------------------

   procedure Proc_Next_Component                 (N : in out Node_Id) is
   begin
      N := Next_Component (N);
   end Proc_Next_Component;

   procedure Proc_Next_Component_Or_Discriminant (N : in out Node_Id) is
   begin
      N := Next_Entity (N);
      while Present (N) loop
         exit when Ekind_In (N, E_Component, E_Discriminant);
         N := Next_Entity (N);
      end loop;
   end Proc_Next_Component_Or_Discriminant;

   procedure Proc_Next_Discriminant              (N : in out Node_Id) is
   begin
      N := Next_Discriminant (N);
   end Proc_Next_Discriminant;

   procedure Proc_Next_Formal                    (N : in out Node_Id) is
   begin
      N := Next_Formal (N);
   end Proc_Next_Formal;

   procedure Proc_Next_Formal_With_Extras        (N : in out Node_Id) is
   begin
      N := Next_Formal_With_Extras (N);
   end Proc_Next_Formal_With_Extras;

   procedure Proc_Next_Index                     (N : in out Node_Id) is
   begin
      N := Next_Index (N);
   end Proc_Next_Index;

   procedure Proc_Next_Inlined_Subprogram        (N : in out Node_Id) is
   begin
      N := Next_Inlined_Subprogram (N);
   end Proc_Next_Inlined_Subprogram;

   procedure Proc_Next_Literal                   (N : in out Node_Id) is
   begin
      N := Next_Literal (N);
   end Proc_Next_Literal;

   procedure Proc_Next_Stored_Discriminant       (N : in out Node_Id) is
   begin
      N := Next_Stored_Discriminant (N);
   end Proc_Next_Stored_Discriminant;

end Einfo;
