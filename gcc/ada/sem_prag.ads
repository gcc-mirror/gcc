------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ P R A G                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2013, Free Software Foundation, Inc.         --
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
with Snames; use Snames;
with Types;  use Types;

package Sem_Prag is

   --  The following table lists all the implementation-defined pragmas that
   --  may apply to a body stub (no language defined pragmas apply). The table
   --  should be synchronized with Aspect_On_Body_Or_Stub_OK in unit Aspects if
   --  the pragmas below implement an aspect.

   Pragma_On_Body_Or_Stub_OK : constant array (Pragma_Id) of Boolean :=
     (Pragma_Refined_Depends => True,
      Pragma_Refined_Global  => True,
      Pragma_Refined_Post    => True,
      Pragma_Refined_Pre     => True,
      Pragma_SPARK_Mode      => True,
      Pragma_Warnings        => True,
      others                 => False);

   -----------------
   -- Subprograms --
   -----------------

   procedure Analyze_Pragma (N : Node_Id);
   --  Analyze procedure for pragma reference node N

   procedure Analyze_Contract_Cases_In_Decl_Part (N : Node_Id);
   --  Perform full analysis and expansion of delayed pragma Contract_Cases

   procedure Analyze_Depends_In_Decl_Part (N : Node_Id);
   --  Perform full analysis of delayed pragma Depends

   procedure Analyze_Global_In_Decl_Part (N : Node_Id);
   --  Perform full analysis of delayed pragma Global

   procedure Analyze_Initializes_In_Decl_Part (N : Node_Id);
   --  Perform full analysis of delayed pragma Initializes

   procedure Analyze_Pre_Post_Condition_In_Decl_Part
     (Prag    : Node_Id;
      Subp_Id : Entity_Id);
   --  Perform preanalysis of a [refined] precondition or postcondition that
   --  appears on a subprogram declaration or body [stub]. Prag denotes the
   --  pragma, Subp_Id is the entity of the related subprogram. The preanalysis
   --  of the expression is done as "spec expression" (see section "Handling
   --  of Default and Per-Object Expressions in Sem).

   procedure Analyze_Refined_Depends_In_Decl_Part (N : Node_Id);
   --  Preform full analysis of delayed pragma Refined_Depends

   procedure Analyze_Refined_Global_In_Decl_Part (N : Node_Id);
   --  Perform full analysis of delayed pragma Refined_Global

   procedure Analyze_Refined_State_In_Decl_Part (N : Node_Id);
   --  Perform full analysis of delayed pragma Refined_State

   procedure Analyze_Test_Case_In_Decl_Part (N : Node_Id; S : Entity_Id);
   --  Perform preanalysis of pragma Test_Case that applies to a subprogram
   --  declaration. Parameter N denotes the pragma, S is the entity of the
   --  related subprogram. The preanalysis of the expression is done as "spec
   --  expression" (see section "Handling of Default and Per-Object Expressions
   --  in Sem).

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

   function Delay_Config_Pragma_Analyze (N : Node_Id) return Boolean;
   --  N is a pragma appearing in a configuration pragma file. Most such
   --  pragmas are analyzed when the file is read, before parsing and analyzing
   --  the main unit. However, the analysis of certain pragmas results in
   --  adding information to the compiled main unit, and this cannot be done
   --  till the main unit is processed. Such pragmas return True from this
   --  function and in Frontend pragmas where Delay_Config_Pragma_Analyze is
   --  True have their analysis delayed until after the main program is parsed
   --  and analyzed.

   function Get_SPARK_Mode_Id (N : Node_Id) return SPARK_Mode_Id;
   --  Given a pragma SPARK_Mode node, return the corresponding mode id

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
   --  Name_uInviarnat, and Name_uType_Invariant (_Pre, _Post, _Invariant,
   --  and _Type_Invariant).

   procedure Make_Aspect_For_PPC_In_Gen_Sub_Decl (Decl : Node_Id);
   --  This routine makes aspects from precondition or postcondition pragmas
   --  that appear within a generic subprogram declaration. Decl is the generic
   --  subprogram declaration node. Note that the aspects are attached to the
   --  generic copy and also to the orginal tree.

   procedure Process_Compilation_Unit_Pragmas (N : Node_Id);
   --  Called at the start of processing compilation unit N to deal with any
   --  special issues regarding pragmas. In particular, we have to deal with
   --  Suppress_All at this stage, since it can appear after the unit instead
   --  of before (actually we allow it to appear anywhere).

   procedure Relocate_Pragmas_To_Body
     (Subp_Body   : Node_Id;
      Target_Body : Node_Id := Empty);
   --  Resocate all pragmas that follow and apply to subprogram body Subp_Body
   --  to its own declaration list. Candidate pragmas are classified in table
   --  Pragma_On_Body_Or_Stub_OK. If Target_Body is set, the pragma are moved
   --  to the declarations of Target_Body. This formal should be set when
   --  dealing with subprogram body stubs or expression functions.

   procedure Set_Encoded_Interface_Name (E : Entity_Id; S : Node_Id);
   --  This routine is used to set an encoded interface name. The node S is an
   --  N_String_Literal node for the external name to be set, and E is an
   --  entity whose Interface_Name field is to be set. In the normal case where
   --  S contains a name that is a valid C identifier, then S is simply set as
   --  the value of the Interface_Name. Otherwise it is encoded. See the body
   --  for details of the encoding. This encoding is only done on VMS systems,
   --  since it seems pretty silly, but is needed to pass some dubious tests in
   --  the test suite.

end Sem_Prag;
