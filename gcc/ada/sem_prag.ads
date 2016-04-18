------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ P R A G                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2015, Free Software Foundation, Inc.         --
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

--  Pragma handling is isolated in a separate package
--  (logically this processing belongs in chapter 4)

with Namet;  use Namet;
with Opt;    use Opt;
with Snames; use Snames;
with Types;  use Types;

package Sem_Prag is

   --  The following table lists all pragmas that emulate an Ada 2012 aspect

   Aspect_Specifying_Pragma : constant array (Pragma_Id) of Boolean :=
     (Pragma_Abstract_State               => True,
      Pragma_All_Calls_Remote             => True,
      Pragma_Annotate                     => True,
      Pragma_Async_Readers                => True,
      Pragma_Async_Writers                => True,
      Pragma_Asynchronous                 => True,
      Pragma_Atomic                       => True,
      Pragma_Atomic_Components            => True,
      Pragma_Attach_Handler               => True,
      Pragma_Constant_After_Elaboration   => True,
      Pragma_Contract_Cases               => True,
      Pragma_Convention                   => True,
      Pragma_CPU                          => True,
      Pragma_Default_Initial_Condition    => True,
      Pragma_Default_Storage_Pool         => True,
      Pragma_Depends                      => True,
      Pragma_Discard_Names                => True,
      Pragma_Dispatching_Domain           => True,
      Pragma_Effective_Reads              => True,
      Pragma_Effective_Writes             => True,
      Pragma_Elaborate_Body               => True,
      Pragma_Export                       => True,
      Pragma_Extensions_Visible           => True,
      Pragma_Favor_Top_Level              => True,
      Pragma_Ghost                        => True,
      Pragma_Global                       => True,
      Pragma_Import                       => True,
      Pragma_Independent                  => True,
      Pragma_Independent_Components       => True,
      Pragma_Initial_Condition            => True,
      Pragma_Initializes                  => True,
      Pragma_Inline                       => True,
      Pragma_Inline_Always                => True,
      Pragma_Interrupt_Handler            => True,
      Pragma_Interrupt_Priority           => True,
      Pragma_Invariant                    => True,
      Pragma_Linker_Section               => True,
      Pragma_Lock_Free                    => True,
      Pragma_No_Elaboration_Code_All      => True,
      Pragma_No_Return                    => True,
      Pragma_Obsolescent                  => True,
      Pragma_Pack                         => True,
      Pragma_Part_Of                      => True,
      Pragma_Persistent_BSS               => True,
      Pragma_Post                         => True,
      Pragma_Post_Class                   => True,
      Pragma_Postcondition                => True,
      Pragma_Pre                          => True,
      Pragma_Pre_Class                    => True,
      Pragma_Precondition                 => True,
      Pragma_Predicate                    => True,
      Pragma_Preelaborable_Initialization => True,
      Pragma_Preelaborate                 => True,
      Pragma_Priority                     => True,
      Pragma_Pure                         => True,
      Pragma_Pure_Function                => True,
      Pragma_Refined_Depends              => True,
      Pragma_Refined_Global               => True,
      Pragma_Refined_Post                 => True,
      Pragma_Refined_State                => True,
      Pragma_Relative_Deadline            => True,
      Pragma_Remote_Access_Type           => True,
      Pragma_Remote_Call_Interface        => True,
      Pragma_Remote_Types                 => True,
      Pragma_Shared                       => True,
      Pragma_Shared_Passive               => True,
      Pragma_Simple_Storage_Pool_Type     => True,
      Pragma_SPARK_Mode                   => True,
      Pragma_Storage_Size                 => True,
      Pragma_Suppress                     => True,
      Pragma_Suppress_Debug_Info          => True,
      Pragma_Suppress_Initialization      => True,
      Pragma_Test_Case                    => True,
      Pragma_Thread_Local_Storage         => True,
      Pragma_Type_Invariant               => True,
      Pragma_Unchecked_Union              => True,
      Pragma_Universal_Aliasing           => True,
      Pragma_Universal_Data               => True,
      Pragma_Unmodified                   => True,
      Pragma_Unreferenced                 => True,
      Pragma_Unreferenced_Objects         => True,
      Pragma_Unsuppress                   => True,
      Pragma_Volatile                     => True,
      Pragma_Volatile_Components          => True,
      Pragma_Volatile_Full_Access         => True,
      Pragma_Warnings                     => True,
      others                              => False);

   --  The following table lists all pragmas that act as an assertion
   --  expression.

   Assertion_Expression_Pragma : constant array (Pragma_Id) of Boolean :=
     (Pragma_Assert                    => True,
      Pragma_Assert_And_Cut            => True,
      Pragma_Assume                    => True,
      Pragma_Check                     => True,
      Pragma_Contract_Cases            => True,
      Pragma_Default_Initial_Condition => True,
      Pragma_Initial_Condition         => True,
      Pragma_Invariant                 => True,
      Pragma_Loop_Invariant            => True,
      Pragma_Loop_Variant              => True,
      Pragma_Post                      => True,
      Pragma_Post_Class                => True,
      Pragma_Postcondition             => True,
      Pragma_Pre                       => True,
      Pragma_Pre_Class                 => True,
      Pragma_Precondition              => True,
      Pragma_Predicate                 => True,
      Pragma_Refined_Post              => True,
      Pragma_Test_Case                 => True,
      Pragma_Type_Invariant            => True,
      Pragma_Type_Invariant_Class      => True,
      others                           => False);

   --  The following table lists all the implementation-defined pragmas that
   --  should apply to the anonymous object produced by the analysis of a
   --  single protected or task type. The table should be synchronized with
   --  Aspect_On_Anonymous_Object_OK in unit Aspects.

   Pragma_On_Anonymous_Object_OK : constant array (Pragma_Id) of Boolean :=
     (Pragma_Depends => True,
      Pragma_Global  => True,
      Pragma_Part_Of => True,
      others         => False);

   --  The following table lists all the implementation-defined pragmas that
   --  may apply to a body stub (no language defined pragmas apply). The table
   --  should be synchronized with Aspect_On_Body_Or_Stub_OK in unit Aspects.

   Pragma_On_Body_Or_Stub_OK : constant array (Pragma_Id) of Boolean :=
     (Pragma_Refined_Depends => True,
      Pragma_Refined_Global  => True,
      Pragma_Refined_Post    => True,
      Pragma_SPARK_Mode      => True,
      Pragma_Warnings        => True,
      others                 => False);

   -----------------
   -- Subprograms --
   -----------------

   procedure Analyze_Pragma (N : Node_Id);
   --  Analyze procedure for pragma reference node N

   procedure Analyze_Contract_Cases_In_Decl_Part
     (N         : Node_Id;
      Freeze_Id : Entity_Id := Empty);
   --  Perform full analysis of delayed pragma Contract_Cases. Freeze_Id is the
   --  entity of [generic] package body or [generic] subprogram body which
   --  caused "freezing" of the related contract where the pragma resides.

   procedure Analyze_Depends_In_Decl_Part (N : Node_Id);
   --  Perform full analysis of delayed pragma Depends. This routine is also
   --  capable of performing basic analysis of pragma Refined_Depends.

   procedure Analyze_External_Property_In_Decl_Part
     (N        : Node_Id;
      Expr_Val : out Boolean);
   --  Perform full analysis of delayed pragmas Async_Readers, Async_Writers,
   --  Effective_Reads and Effective_Writes. Flag Expr_Val contains the Boolean
   --  argument of the pragma or a default True if no argument is present.

   procedure Analyze_Global_In_Decl_Part (N : Node_Id);
   --  Perform full analysis of delayed pragma Global. This routine is also
   --  capable of performing basic analysis of pragma Refind_Global.

   procedure Analyze_Initial_Condition_In_Decl_Part (N : Node_Id);
   --  Perform full analysis of delayed pragma Initial_Condition

   procedure Analyze_Initializes_In_Decl_Part (N : Node_Id);
   --  Perform full analysis of delayed pragma Initializes

   procedure Analyze_Part_Of_In_Decl_Part
     (N         : Node_Id;
      Freeze_Id : Entity_Id := Empty);
   --  Perform full analysis of delayed pragma Part_Of. Freeze_Id is the entity
   --  of [generic] package body or [generic] subprogram body which caused the
   --  "freezing" of the related contract where the pragma resides.

   procedure Analyze_Pre_Post_Condition_In_Decl_Part
     (N         : Node_Id;
      Freeze_Id : Entity_Id := Empty);
   --  Perform full analysis of pragmas Precondition and Postcondition.
   --  Freeze_Id denotes the entity of [generic] package body or [generic]
   --  subprogram body which caused "freezing" of the related contract where
   --  the pragma resides.

   procedure Analyze_Refined_Depends_In_Decl_Part (N : Node_Id);
   --  Preform full analysis of delayed pragma Refined_Depends. This routine
   --  uses Analyze_Depends_In_Decl_Part as a starting point, then performs
   --  various consistency checks between Depends and Refined_Depends.

   procedure Analyze_Refined_Global_In_Decl_Part (N : Node_Id);
   --  Perform full analysis of delayed pragma Refined_Global. This routine
   --  uses Analyze_Global_In_Decl_Part as a starting point, then performs
   --  various consistency checks between Global and Refined_Global.

   procedure Analyze_Refined_State_In_Decl_Part
     (N         : Node_Id;
      Freeze_Id : Entity_Id := Empty);
   --  Perform full analysis of delayed pragma Refined_State. Freeze_Id denotes
   --  the entity of [generic] package body or [generic] subprogram body which
   --  caused "freezing" of the related contract where the pragma resides.

   procedure Analyze_Test_Case_In_Decl_Part (N : Node_Id);
   --  Perform preanalysis of pragma Test_Case

   procedure Check_Applicable_Policy (N : Node_Id);
   --  N is either an N_Aspect or an N_Pragma node. There are two cases. If
   --  the name of the aspect or pragma is not one of those recognized as
   --  an assertion kind by an Assertion_Policy pragma, then the call has
   --  no effect. Note that in the case of a pragma derived from an aspect,
   --  the name we use for the purpose of this procedure is the aspect name,
   --  which may be different from the pragma name (e.g. Precondition for
   --  Pre aspect). In addition, 'Class aspects are recognized (and the
   --  corresponding special names used in the processing).
   --
   --  If the name is a valid assertion kind name, then the Check_Policy pragma
   --  chain is checked for a matching entry (or for an Assertion entry which
   --  matches all possibilities). If a matching entry is found then the policy
   --  is checked. If it is On or Check, then the Is_Checked flag is set in
   --  the aspect or pragma node. If it is Off, Ignore, or Disable, then the
   --  Is_Ignored flag is set in the aspect or pragma node. Additionally for
   --  policy Disable, the Is_Disabled flag is set.
   --
   --  If no matching Check_Policy pragma is found then the effect depends on
   --  whether -gnata was used, if so, then the call has no effect, otherwise
   --  Is_Ignored (but not Is_Disabled) is set True.

   procedure Check_External_Properties
     (Item : Node_Id;
      AR   : Boolean;
      AW   : Boolean;
      ER   : Boolean;
      EW   : Boolean);
   --  Flags AR, AW, ER and EW denote the static values of external properties
   --  Async_Readers, Async_Writers, Effective_Reads and Effective_Writes. Item
   --  is the related variable or state. Ensure legality of the combination and
   --  issue an error for an illegal combination.

   function Check_Kind (Nam : Name_Id) return Name_Id;
   --  This function is used in connection with pragmas Assert, Check,
   --  and assertion aspects and pragmas, to determine if Check pragmas
   --  (or corresponding assertion aspects or pragmas) are currently active
   --  as determined by the presence of -gnata on the command line (which
   --  sets the default), and the appearance of pragmas Check_Policy and
   --  Assertion_Policy as configuration pragmas either in a configuration
   --  pragma file, or at the start of the current unit, or locally given
   --  Check_Policy and Assertion_Policy pragmas that are currently active.
   --
   --  The value returned is one of the names Check, Ignore, Disable (On
   --  returns Check, and Off returns Ignore).
   --
   --  Note: for assertion kinds Pre'Class, Post'Class, Invariant'Class,
   --  and Type_Invariant'Class, the name passed is Name_uPre, Name_uPost,
   --  Name_uInvariant, or Name_uType_Invariant, which corresponds to _Pre,
   --  _Post, _Invariant, or _Type_Invariant, which are special names used
   --  in identifiers to represent these attribute references.

   procedure Check_Missing_Part_Of (Item_Id : Entity_Id);
   --  Determine whether the placement within the state space of an abstract
   --  state, variable or package instantiation denoted by Item_Id requires the
   --  use of indicator/option Part_Of. If this is the case, emit an error.

   procedure Collect_Subprogram_Inputs_Outputs
     (Subp_Id      : Entity_Id;
      Synthesize   : Boolean := False;
      Subp_Inputs  : in out Elist_Id;
      Subp_Outputs : in out Elist_Id;
      Global_Seen  : out Boolean);
   --  Subsidiary to the analysis of pragmas Depends, Global, Refined_Depends
   --  and Refined_Global. The routine is also used by GNATprove. Collect all
   --  inputs and outputs of subprogram Subp_Id in lists Subp_Inputs (inputs)
   --  and Subp_Outputs (outputs). The inputs and outputs are gathered from:
   --    1) The formal parameters of the subprogram
   --    2) The generic formal parameters of the generic subprogram
   --    3) The current instance of a concurrent type
   --    4) The items of pragma [Refined_]Global
   --         or
   --    5) The items of pragma [Refined_]Depends if there is no pragma
   --       [Refined_]Global present and flag Synthesize is set to True.
   --  If the subprogram has no inputs and/or outputs, then the returned list
   --  is No_Elist. Flag Global_Seen is set when the related subprogram has
   --  pragma [Refined_]Global.

   function Delay_Config_Pragma_Analyze (N : Node_Id) return Boolean;
   --  N is a pragma appearing in a configuration pragma file. Most such
   --  pragmas are analyzed when the file is read, before parsing and analyzing
   --  the main unit. However, the analysis of certain pragmas results in
   --  adding information to the compiled main unit, and this cannot be done
   --  till the main unit is processed. Such pragmas return True from this
   --  function and in Frontend pragmas where Delay_Config_Pragma_Analyze is
   --  True have their analysis delayed until after the main program is parsed
   --  and analyzed.

   function Find_Related_Package_Or_Body
     (Prag      : Node_Id;
      Do_Checks : Boolean := False) return Node_Id;
   --  Subsidiary to the analysis of pragmas Abstract_State, Initial_Condition,
   --  Initializes and Refined_State. Find the declaration of the related
   --  package [body] subject to pragma Prag. The return value is either
   --  N_Package_Declaration, N_Package_Body or Empty if the placement of
   --  the pragma is illegal. If flag Do_Checks is set, the routine reports
   --  duplicate pragmas.

   function Find_Related_Declaration_Or_Body
     (Prag      : Node_Id;
      Do_Checks : Boolean := False) return Node_Id;
   --  Subsidiary to the analysis of pragmas
   --    Contract_Cases
   --    Depends
   --    Extensions_Visible
   --    Global
   --    Post
   --    Post_Class
   --    Postcondition
   --    Pre
   --    Pre_Class
   --    Precondition
   --    Refined_Depends
   --    Refined_Global
   --    Refined_Post
   --    Test_Case
   --  as well as attributes 'Old and 'Result. Find the declaration of the
   --  related entry, subprogram or task type [body] subject to pragma Prag.
   --  If flag Do_Checks is set, the routine reports duplicate pragmas and
   --  detects improper use of refinement pragmas in stand alone expression
   --  functions.

   function Get_Argument
     (Prag       : Node_Id;
      Context_Id : Node_Id := Empty) return Node_Id;
   --  Obtain the argument of pragma Prag depending on context and the nature
   --  of the pragma. The argument is extracted in the following manner:
   --
   --    When the pragma is generated from an aspect, return the corresponding
   --    aspect for ASIS or when Context_Id denotes a generic unit.
   --
   --    Otherwise return the first argument of Prag
   --
   --  Context denotes the entity of the function, package or procedure where
   --  Prag resides.

   function Get_SPARK_Mode_From_Pragma (N : Node_Id) return SPARK_Mode_Type;
   --  Given a pragma SPARK_Mode node, return corresponding mode id

   function Get_SPARK_Mode_Type (N : Name_Id) return SPARK_Mode_Type;
   --  Subsidiary to the analysis of pragma SPARK_Mode as well as subprogram
   --  Get_SPARK_Mode_From_Pragma. Convert a name into a corresponding value
   --  of type SPARK_Mode_Type.

   procedure Initialize;
   --  Initializes data structures used for pragma processing. Must be called
   --  before analyzing each new main source program.

   function Is_Config_Static_String (Arg : Node_Id) return Boolean;
   --  This is called for a configuration pragma that requires either string
   --  literal or a concatenation of string literals. We cannot use normal
   --  static string processing because it is too early in the case of the
   --  pragma appearing in a configuration pragmas file. If Arg is of an
   --  appropriate form, then this call obtains the string (doing any necessary
   --  concatenations) and places it in Name_Buffer, setting Name_Len to its
   --  length, and then returns True. If it is not of the correct form, then an
   --  appropriate error message is posted, and False is returned.

   function Is_Elaboration_SPARK_Mode (N : Node_Id) return Boolean;
   --  Determine whether pragma SPARK_Mode appears in the statement part of a
   --  package body.

   function Is_Enabled_Pragma (Prag : Node_Id) return Boolean;
   --  Determine whether a Boolean-like SPARK pragma Prag is enabled. To be
   --  considered enabled, the pragma must either:
   --    * Appear without its Boolean expression
   --    * The Boolean expression evaluates to "True"
   --
   --  Boolean-like SPARK pragmas differ from pure Boolean Ada pragmas in that
   --  their optional Boolean expression must be static and cannot benefit from
   --  forward references. The following are Boolean-like SPARK pragmas:
   --    Async_Readers
   --    Async_Writers
   --    Constant_After_Elaboration
   --    Effective_Reads
   --    Effective_Writes
   --    Extensions_Visible
   --    Volatile_Function

   function Is_Non_Significant_Pragma_Reference (N : Node_Id) return Boolean;
   --  The node N is a node for an entity and the issue is whether the
   --  occurrence is a reference for the purposes of giving warnings about
   --  unreferenced variables. This function returns True if the reference is
   --  not a reference from this point of view (e.g. the occurrence in a pragma
   --  Pack) and False if it is a real reference (e.g. the occurrence in a
   --  pragma Export);

   function Is_Pragma_String_Literal (Par : Node_Id) return Boolean;
   --  Given an N_Pragma_Argument_Association node, Par, which has the form of
   --  an operator symbol, determines whether or not it should be treated as an
   --  string literal. This is called by Sem_Ch6.Analyze_Operator_Symbol. If
   --  True is returned, the argument is converted to a string literal. If
   --  False is returned, then the argument is treated as an entity reference
   --  to the operator.

   function Is_Private_SPARK_Mode (N : Node_Id) return Boolean;
   --  Determine whether pragma SPARK_Mode appears in the private part of a
   --  package.

   function Is_Valid_Assertion_Kind (Nam : Name_Id) return Boolean;
   --  Returns True if Nam is one of the names recognized as a valid assertion
   --  kind by the Assertion_Policy pragma. Note that the 'Class cases are
   --  represented by the corresponding special names Name_uPre, Name_uPost,
   --  Name_uInvariant, and Name_uType_Invariant (_Pre, _Post, _Invariant,
   --  and _Type_Invariant).

   procedure Process_Compilation_Unit_Pragmas (N : Node_Id);
   --  Called at the start of processing compilation unit N to deal with any
   --  special issues regarding pragmas. In particular, we have to deal with
   --  Suppress_All at this stage, since it can appear after the unit instead
   --  of before (actually we allow it to appear anywhere).

   procedure Relocate_Pragmas_To_Anonymous_Object
     (Typ_Decl : Node_Id;
      Obj_Decl : Node_Id);
   --  Relocate all pragmas that appear in the visible declarations of task or
   --  protected type declaration Typ_Decl after the declaration of anonymous
   --  object Obj_Decl. Table Pragmas_On_Anonymous_Object_OK contains the list
   --  of candidate pragmas.

   procedure Relocate_Pragmas_To_Body
     (Subp_Body   : Node_Id;
      Target_Body : Node_Id := Empty);
   --  Resocate all pragmas that follow and apply to subprogram body Subp_Body
   --  to its own declaration list. Candidate pragmas are classified in table
   --  Pragma_On_Body_Or_Stub_OK. If Target_Body is set, the pragma are moved
   --  to the declarations of Target_Body. This formal should be set when
   --  dealing with subprogram body stubs or expression functions.

   procedure Set_Encoded_Interface_Name (E : Entity_Id; S : Node_Id);
   --  This routine is used to set an encoded interface name. The node S is
   --  an N_String_Literal node for the external name to be set, and E is an
   --  entity whose Interface_Name field is to be set. In the normal case where
   --  S contains a name that is a valid C identifier, then S is simply set as
   --  the value of the Interface_Name. Otherwise it is encoded as needed by
   --  particular operating systems. See the body for details of the encoding.

   function Test_Case_Arg
     (Prag        : Node_Id;
      Arg_Nam     : Name_Id;
      From_Aspect : Boolean := False) return Node_Id;
   --  Obtain argument "Name", "Mode", "Ensures" or "Requires" from Test_Case
   --  pragma Prag as denoted by Arg_Nam. When From_Aspect is set, an attempt
   --  is made to retrieve the argument from the corresponding aspect if there
   --  is one. The returned argument has several formats:
   --
   --    N_Pragma_Argument_Association if retrieved directly from the pragma
   --
   --    N_Component_Association if retrieved from the corresponding aspect and
   --    the argument appears in a named association form.
   --
   --    An arbitrary expression if retrieved from the corresponding aspect and
   --    the argument appears in positional form.
   --
   --    Empty if there is no such argument

end Sem_Prag;
