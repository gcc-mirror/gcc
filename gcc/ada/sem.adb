------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  S E M                                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2012, Free Software Foundation, Inc.         --
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

with Atree;    use Atree;
with Debug;    use Debug;
with Debug_A;  use Debug_A;
with Elists;   use Elists;
with Errout;   use Errout;
with Expander; use Expander;
with Fname;    use Fname;
with Lib;      use Lib;
with Lib.Load; use Lib.Load;
with Nlists;   use Nlists;
with Output;   use Output;
with Restrict; use Restrict;
with Sem_Attr; use Sem_Attr;
with Sem_Aux;  use Sem_Aux;
with Sem_Ch2;  use Sem_Ch2;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch4;  use Sem_Ch4;
with Sem_Ch5;  use Sem_Ch5;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch7;  use Sem_Ch7;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch9;  use Sem_Ch9;
with Sem_Ch10; use Sem_Ch10;
with Sem_Ch11; use Sem_Ch11;
with Sem_Ch12; use Sem_Ch12;
with Sem_Ch13; use Sem_Ch13;
with Sem_Prag; use Sem_Prag;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Stand;    use Stand;
with Uintp;    use Uintp;
with Uname;    use Uname;

with Unchecked_Deallocation;

pragma Warnings (Off, Sem_Util);
--  Suppress warnings of unused with for Sem_Util (used only in asserts)

package body Sem is

   Debug_Unit_Walk : Boolean renames Debug_Flag_Dot_WW;
   --  Controls debugging printouts for Walk_Library_Items

   Outer_Generic_Scope : Entity_Id := Empty;
   --  Global reference to the outer scope that is generic. In a non-generic
   --  context, it is empty. At the moment, it is only used for avoiding
   --  freezing of external references in generics.

   Comp_Unit_List : Elist_Id := No_Elist;
   --  Used by Walk_Library_Items. This is a list of N_Compilation_Unit nodes
   --  processed by Semantics, in an appropriate order. Initialized to
   --  No_Elist, because it's too early to call New_Elmt_List; we will set it
   --  to New_Elmt_List on first use.

   generic
      with procedure Action (Withed_Unit : Node_Id);
   procedure Walk_Withs_Immediate (CU : Node_Id; Include_Limited : Boolean);
   --  Walk all the with clauses of CU, and call Action for the with'ed unit.
   --  Ignore limited withs, unless Include_Limited is True. CU must be an
   --  N_Compilation_Unit.

   generic
      with procedure Action (Withed_Unit : Node_Id);
   procedure Walk_Withs (CU : Node_Id; Include_Limited : Boolean);
   --  Same as Walk_Withs_Immediate, but also include with clauses on subunits
   --  of this unit, since they count as dependences on their parent library
   --  item. CU must be an N_Compilation_Unit whose Unit is not an N_Subunit.

   -------------
   -- Analyze --
   -------------

   procedure Analyze (N : Node_Id) is
   begin
      Debug_A_Entry ("analyzing  ", N);

      --  Immediate return if already analyzed

      if Analyzed (N) then
         Debug_A_Exit ("analyzing  ", N, "  (done, analyzed already)");
         return;
      end if;

      --  Otherwise processing depends on the node kind

      case Nkind (N) is

         when N_Abort_Statement =>
            Analyze_Abort_Statement (N);

         when N_Abstract_Subprogram_Declaration =>
            Analyze_Abstract_Subprogram_Declaration (N);

         when N_Accept_Alternative =>
            Analyze_Accept_Alternative (N);

         when N_Accept_Statement =>
            Analyze_Accept_Statement (N);

         when N_Aggregate =>
            Analyze_Aggregate (N);

         when N_Allocator =>
            Analyze_Allocator (N);

         when N_And_Then =>
            Analyze_Short_Circuit (N);

         when N_Assignment_Statement =>
            Analyze_Assignment (N);

         when N_Asynchronous_Select =>
            Analyze_Asynchronous_Select (N);

         when N_At_Clause =>
            Analyze_At_Clause (N);

         when N_Attribute_Reference =>
            Analyze_Attribute (N);

         when N_Attribute_Definition_Clause   =>
            Analyze_Attribute_Definition_Clause (N);

         when N_Block_Statement =>
            Analyze_Block_Statement (N);

         when N_Case_Expression =>
            Analyze_Case_Expression (N);

         when N_Case_Statement =>
            Analyze_Case_Statement (N);

         when N_Character_Literal =>
            Analyze_Character_Literal (N);

         when N_Code_Statement =>
            Analyze_Code_Statement (N);

         when N_Compilation_Unit =>
            Analyze_Compilation_Unit (N);

         when N_Component_Declaration =>
            Analyze_Component_Declaration (N);

         when N_Conditional_Expression =>
            Analyze_Conditional_Expression (N);

         when N_Conditional_Entry_Call =>
            Analyze_Conditional_Entry_Call (N);

         when N_Delay_Alternative =>
            Analyze_Delay_Alternative (N);

         when N_Delay_Relative_Statement =>
            Analyze_Delay_Relative (N);

         when N_Delay_Until_Statement =>
            Analyze_Delay_Until (N);

         when N_Entry_Body =>
            Analyze_Entry_Body (N);

         when N_Entry_Body_Formal_Part =>
            Analyze_Entry_Body_Formal_Part (N);

         when N_Entry_Call_Alternative =>
            Analyze_Entry_Call_Alternative (N);

         when N_Entry_Declaration =>
            Analyze_Entry_Declaration (N);

         when N_Entry_Index_Specification     =>
            Analyze_Entry_Index_Specification (N);

         when N_Enumeration_Representation_Clause =>
            Analyze_Enumeration_Representation_Clause (N);

         when N_Exception_Declaration =>
            Analyze_Exception_Declaration (N);

         when N_Exception_Renaming_Declaration =>
            Analyze_Exception_Renaming (N);

         when N_Exit_Statement =>
            Analyze_Exit_Statement (N);

         when N_Expanded_Name =>
            Analyze_Expanded_Name (N);

         when N_Explicit_Dereference =>
            Analyze_Explicit_Dereference (N);

         when N_Expression_Function =>
            Analyze_Expression_Function (N);

         when N_Expression_With_Actions =>
            Analyze_Expression_With_Actions (N);

         when N_Extended_Return_Statement =>
            Analyze_Extended_Return_Statement (N);

         when N_Extension_Aggregate =>
            Analyze_Aggregate (N);

         when N_Formal_Object_Declaration =>
            Analyze_Formal_Object_Declaration (N);

         when N_Formal_Package_Declaration =>
            Analyze_Formal_Package_Declaration (N);

         when N_Formal_Subprogram_Declaration =>
            Analyze_Formal_Subprogram_Declaration (N);

         when N_Formal_Type_Declaration =>
            Analyze_Formal_Type_Declaration (N);

         when N_Free_Statement =>
            Analyze_Free_Statement (N);

         when N_Freeze_Entity =>
            Analyze_Freeze_Entity (N);

         when N_Full_Type_Declaration =>
            Analyze_Full_Type_Declaration (N);

         when N_Function_Call =>
            Analyze_Function_Call (N);

         when N_Function_Instantiation =>
            Analyze_Function_Instantiation (N);

         when N_Generic_Function_Renaming_Declaration =>
            Analyze_Generic_Function_Renaming (N);

         when N_Generic_Package_Declaration =>
            Analyze_Generic_Package_Declaration (N);

         when N_Generic_Package_Renaming_Declaration =>
            Analyze_Generic_Package_Renaming (N);

         when N_Generic_Procedure_Renaming_Declaration =>
            Analyze_Generic_Procedure_Renaming (N);

         when N_Generic_Subprogram_Declaration =>
            Analyze_Generic_Subprogram_Declaration (N);

         when N_Goto_Statement =>
            Analyze_Goto_Statement (N);

         when N_Handled_Sequence_Of_Statements =>
            Analyze_Handled_Statements (N);

         when N_Identifier =>
            Analyze_Identifier (N);

         when N_If_Statement =>
            Analyze_If_Statement (N);

         when N_Implicit_Label_Declaration =>
            Analyze_Implicit_Label_Declaration (N);

         when N_In =>
            Analyze_Membership_Op (N);

         when N_Incomplete_Type_Declaration =>
            Analyze_Incomplete_Type_Decl (N);

         when N_Indexed_Component =>
            Analyze_Indexed_Component_Form (N);

         when N_Integer_Literal =>
            Analyze_Integer_Literal (N);

         when N_Iterator_Specification =>
            Analyze_Iterator_Specification (N);

         when N_Itype_Reference =>
            Analyze_Itype_Reference (N);

         when N_Label =>
            Analyze_Label (N);

         when N_Loop_Parameter_Specification =>
            Analyze_Loop_Parameter_Specification (N);

         when N_Loop_Statement =>
            Analyze_Loop_Statement (N);

         when N_Not_In =>
            Analyze_Membership_Op (N);

         when N_Null =>
            Analyze_Null (N);

         when N_Null_Statement =>
            Analyze_Null_Statement (N);

         when N_Number_Declaration =>
            Analyze_Number_Declaration (N);

         when N_Object_Declaration =>
            Analyze_Object_Declaration (N);

         when N_Object_Renaming_Declaration  =>
            Analyze_Object_Renaming (N);

         when N_Operator_Symbol =>
            Analyze_Operator_Symbol (N);

         when N_Op_Abs =>
            Analyze_Unary_Op (N);

         when N_Op_Add =>
            Analyze_Arithmetic_Op (N);

         when N_Op_And =>
            Analyze_Logical_Op (N);

         when N_Op_Concat =>
            Analyze_Concatenation (N);

         when N_Op_Divide =>
            Analyze_Arithmetic_Op (N);

         when N_Op_Eq =>
            Analyze_Equality_Op (N);

         when N_Op_Expon =>
            Analyze_Arithmetic_Op (N);

         when N_Op_Ge =>
            Analyze_Comparison_Op (N);

         when N_Op_Gt =>
            Analyze_Comparison_Op (N);

         when N_Op_Le =>
            Analyze_Comparison_Op (N);

         when N_Op_Lt =>
            Analyze_Comparison_Op (N);

         when N_Op_Minus =>
            Analyze_Unary_Op (N);

         when N_Op_Mod =>
            Analyze_Mod (N);

         when N_Op_Multiply =>
            Analyze_Arithmetic_Op (N);

         when N_Op_Ne =>
            Analyze_Equality_Op (N);

         when N_Op_Not =>
            Analyze_Negation (N);

         when N_Op_Or =>
            Analyze_Logical_Op (N);

         when N_Op_Plus =>
            Analyze_Unary_Op (N);

         when N_Op_Rem =>
            Analyze_Arithmetic_Op (N);

         when N_Op_Rotate_Left =>
            Analyze_Arithmetic_Op (N);

         when N_Op_Rotate_Right =>
            Analyze_Arithmetic_Op (N);

         when N_Op_Shift_Left =>
            Analyze_Arithmetic_Op (N);

         when N_Op_Shift_Right =>
            Analyze_Arithmetic_Op (N);

         when N_Op_Shift_Right_Arithmetic =>
            Analyze_Arithmetic_Op (N);

         when N_Op_Subtract =>
            Analyze_Arithmetic_Op (N);

         when N_Op_Xor =>
            Analyze_Logical_Op (N);

         when N_Or_Else =>
            Analyze_Short_Circuit (N);

         when N_Others_Choice =>
            Analyze_Others_Choice (N);

         when N_Package_Body =>
            Analyze_Package_Body (N);

         when N_Package_Body_Stub =>
            Analyze_Package_Body_Stub (N);

         when N_Package_Declaration =>
            Analyze_Package_Declaration (N);

         when N_Package_Instantiation =>
            Analyze_Package_Instantiation (N);

         when N_Package_Renaming_Declaration =>
            Analyze_Package_Renaming (N);

         when N_Package_Specification =>
            Analyze_Package_Specification (N);

         when N_Parameter_Association =>
            Analyze_Parameter_Association (N);

         when N_Pragma =>
            Analyze_Pragma (N);

         when N_Private_Extension_Declaration =>
            Analyze_Private_Extension_Declaration (N);

         when N_Private_Type_Declaration =>
            Analyze_Private_Type_Declaration (N);

         when N_Procedure_Call_Statement =>
            Analyze_Procedure_Call (N);

         when N_Procedure_Instantiation =>
            Analyze_Procedure_Instantiation (N);

         when N_Protected_Body =>
            Analyze_Protected_Body (N);

         when N_Protected_Body_Stub =>
            Analyze_Protected_Body_Stub (N);

         when N_Protected_Definition =>
            Analyze_Protected_Definition (N);

         when N_Protected_Type_Declaration =>
            Analyze_Protected_Type_Declaration (N);

         when N_Qualified_Expression =>
            Analyze_Qualified_Expression (N);

         when N_Quantified_Expression =>
            Analyze_Quantified_Expression (N);

         when N_Raise_Statement =>
            Analyze_Raise_Statement (N);

         when N_Raise_xxx_Error =>
            Analyze_Raise_xxx_Error (N);

         when N_Range =>
            Analyze_Range (N);

         when N_Range_Constraint =>
            Analyze_Range (Range_Expression (N));

         when N_Real_Literal =>
            Analyze_Real_Literal (N);

         when N_Record_Representation_Clause =>
            Analyze_Record_Representation_Clause (N);

         when N_Reference =>
            Analyze_Reference (N);

         when N_Requeue_Statement =>
            Analyze_Requeue (N);

         when N_Simple_Return_Statement =>
            Analyze_Simple_Return_Statement (N);

         when N_Selected_Component =>
            Find_Selected_Component (N);
            --  ??? why not Analyze_Selected_Component, needs comments

         when N_Selective_Accept =>
            Analyze_Selective_Accept (N);

         when N_Single_Protected_Declaration =>
            Analyze_Single_Protected_Declaration (N);

         when N_Single_Task_Declaration =>
            Analyze_Single_Task_Declaration (N);

         when N_Slice =>
            Analyze_Slice (N);

         when N_String_Literal =>
            Analyze_String_Literal (N);

         when N_Subprogram_Body =>
            Analyze_Subprogram_Body (N);

         when N_Subprogram_Body_Stub =>
            Analyze_Subprogram_Body_Stub (N);

         when N_Subprogram_Declaration =>
            Analyze_Subprogram_Declaration (N);

         when N_Subprogram_Info =>
            Analyze_Subprogram_Info (N);

         when N_Subprogram_Renaming_Declaration =>
            Analyze_Subprogram_Renaming (N);

         when N_Subtype_Declaration =>
            Analyze_Subtype_Declaration (N);

         when N_Subtype_Indication =>
            Analyze_Subtype_Indication (N);

         when N_Subunit =>
            Analyze_Subunit (N);

         when N_Task_Body =>
            Analyze_Task_Body (N);

         when N_Task_Body_Stub =>
            Analyze_Task_Body_Stub (N);

         when N_Task_Definition =>
            Analyze_Task_Definition (N);

         when N_Task_Type_Declaration =>
            Analyze_Task_Type_Declaration (N);

         when N_Terminate_Alternative =>
            Analyze_Terminate_Alternative (N);

         when N_Timed_Entry_Call =>
            Analyze_Timed_Entry_Call (N);

         when N_Triggering_Alternative =>
            Analyze_Triggering_Alternative (N);

         when N_Type_Conversion =>
            Analyze_Type_Conversion (N);

         when N_Unchecked_Expression =>
            Analyze_Unchecked_Expression (N);

         when N_Unchecked_Type_Conversion =>
            Analyze_Unchecked_Type_Conversion (N);

         when N_Use_Package_Clause =>
            Analyze_Use_Package (N);

         when N_Use_Type_Clause =>
            Analyze_Use_Type (N);

         when N_Validate_Unchecked_Conversion =>
            null;

         when N_Variant_Part =>
            Analyze_Variant_Part (N);

         when N_With_Clause =>
            Analyze_With_Clause (N);

         --  A call to analyze the Empty node is an error, but most likely it
         --  is an error caused by an attempt to analyze a malformed piece of
         --  tree caused by some other error, so if there have been any other
         --  errors, we just ignore it, otherwise it is a real internal error
         --  which we complain about.

         --  We must also consider the case of call to a runtime function that
         --  is not available in the configurable runtime.

         when N_Empty =>
            pragma Assert (Serious_Errors_Detected /= 0
              or else Configurable_Run_Time_Violations /= 0);
            null;

         --  A call to analyze the error node is simply ignored, to avoid
         --  causing cascaded errors (happens of course only in error cases)

         when N_Error =>
            null;

         --  Push/Pop nodes normally don't come through an analyze call. An
         --  exception is the dummy ones bracketing a subprogram body. In any
         --  case there is nothing to be done to analyze such nodes.

         when N_Push_Pop_xxx_Label =>
            null;

         --  SCIL nodes don't need analysis because they are decorated when
         --  they are built. They are added to the tree by Insert_Actions and
         --  the call to analyze them is generated when the full list is
         --  analyzed.

         when
           N_SCIL_Dispatch_Table_Tag_Init |
           N_SCIL_Dispatching_Call        |
           N_SCIL_Membership_Test         =>
            null;

         --  For the remaining node types, we generate compiler abort, because
         --  these nodes are always analyzed within the Sem_Chn routines and
         --  there should never be a case of making a call to the main Analyze
         --  routine for these node kinds. For example, an N_Access_Definition
         --  node appears only in the context of a type declaration, and is
         --  processed by the analyze routine for type declarations.

         when
           N_Abortable_Part                         |
           N_Access_Definition                      |
           N_Access_Function_Definition             |
           N_Access_Procedure_Definition            |
           N_Access_To_Object_Definition            |
           N_Aspect_Specification                   |
           N_Case_Expression_Alternative            |
           N_Case_Statement_Alternative             |
           N_Compilation_Unit_Aux                   |
           N_Component_Association                  |
           N_Component_Clause                       |
           N_Component_Definition                   |
           N_Component_List                         |
           N_Constrained_Array_Definition           |
           N_Contract                               |
           N_Decimal_Fixed_Point_Definition         |
           N_Defining_Character_Literal             |
           N_Defining_Identifier                    |
           N_Defining_Operator_Symbol               |
           N_Defining_Program_Unit_Name             |
           N_Delta_Constraint                       |
           N_Derived_Type_Definition                |
           N_Designator                             |
           N_Digits_Constraint                      |
           N_Discriminant_Association               |
           N_Discriminant_Specification             |
           N_Elsif_Part                             |
           N_Entry_Call_Statement                   |
           N_Enumeration_Type_Definition            |
           N_Exception_Handler                      |
           N_Floating_Point_Definition              |
           N_Formal_Decimal_Fixed_Point_Definition  |
           N_Formal_Derived_Type_Definition         |
           N_Formal_Discrete_Type_Definition        |
           N_Formal_Floating_Point_Definition       |
           N_Formal_Modular_Type_Definition         |
           N_Formal_Ordinary_Fixed_Point_Definition |
           N_Formal_Private_Type_Definition         |
           N_Formal_Incomplete_Type_Definition      |
           N_Formal_Signed_Integer_Type_Definition  |
           N_Function_Specification                 |
           N_Generic_Association                    |
           N_Index_Or_Discriminant_Constraint       |
           N_Iteration_Scheme                       |
           N_Mod_Clause                             |
           N_Modular_Type_Definition                |
           N_Ordinary_Fixed_Point_Definition        |
           N_Parameter_Specification                |
           N_Pragma_Argument_Association            |
           N_Procedure_Specification                |
           N_Real_Range_Specification               |
           N_Record_Definition                      |
           N_Signed_Integer_Type_Definition         |
           N_Unconstrained_Array_Definition         |
           N_Unused_At_Start                        |
           N_Unused_At_End                          |
           N_Variant                                =>

            raise Program_Error;
      end case;

      Debug_A_Exit ("analyzing  ", N, "  (done)");

      --  Now that we have analyzed the node, we call the expander to perform
      --  possible expansion. We skip this for subexpressions, because we don't
      --  have the type yet, and the expander will need to know the type before
      --  it can do its job. For subexpression nodes, the call to the expander
      --  happens in Sem_Res.Resolve. A special exception is Raise_xxx_Error,
      --  which can appear in a statement context, and needs expanding now in
      --  the case (distinguished by Etype, as documented in Sinfo).

      --  The Analyzed flag is also set at this point for non-subexpression
      --  nodes (in the case of subexpression nodes, we can't set the flag yet,
      --  since resolution and expansion have not yet been completed). Note
      --  that for N_Raise_xxx_Error we have to distinguish the expression
      --  case from the statement case.

      if Nkind (N) not in N_Subexpr
        or else (Nkind (N) in N_Raise_xxx_Error
                  and then Etype (N) = Standard_Void_Type)
      then
         Expand (N);
      end if;
   end Analyze;

   --  Version with check(s) suppressed

   procedure Analyze (N : Node_Id; Suppress : Check_Id) is
   begin
      if Suppress = All_Checks then
         declare
            Svg : constant Suppress_Array := Scope_Suppress;
         begin
            Scope_Suppress := (others => True);
            Analyze (N);
            Scope_Suppress := Svg;
         end;

      else
         declare
            Svg : constant Boolean := Scope_Suppress (Suppress);
         begin
            Scope_Suppress (Suppress) := True;
            Analyze (N);
            Scope_Suppress (Suppress) := Svg;
         end;
      end if;
   end Analyze;

   ------------------
   -- Analyze_List --
   ------------------

   procedure Analyze_List (L : List_Id) is
      Node : Node_Id;

   begin
      Node := First (L);
      while Present (Node) loop
         Analyze (Node);
         Next (Node);
      end loop;
   end Analyze_List;

   --  Version with check(s) suppressed

   procedure Analyze_List (L : List_Id; Suppress : Check_Id) is
   begin
      if Suppress = All_Checks then
         declare
            Svg : constant Suppress_Array := Scope_Suppress;
         begin
            Scope_Suppress := (others => True);
            Analyze_List (L);
            Scope_Suppress := Svg;
         end;

      else
         declare
            Svg : constant Boolean := Scope_Suppress (Suppress);
         begin
            Scope_Suppress (Suppress) := True;
            Analyze_List (L);
            Scope_Suppress (Suppress) := Svg;
         end;
      end if;
   end Analyze_List;

   --------------------------
   -- Copy_Suppress_Status --
   --------------------------

   procedure Copy_Suppress_Status
     (C    : Check_Id;
      From : Entity_Id;
      To   : Entity_Id)
   is
      Found : Boolean;
      pragma Warnings (Off, Found);

      procedure Search_Stack
        (Top   : Suppress_Stack_Entry_Ptr;
         Found : out Boolean);
      --  Search given suppress stack for matching entry for entity. If found
      --  then set Checks_May_Be_Suppressed on To, and push an appropriate
      --  entry for To onto the local suppress stack.

      ------------------
      -- Search_Stack --
      ------------------

      procedure Search_Stack
        (Top   : Suppress_Stack_Entry_Ptr;
         Found : out Boolean)
      is
         Ptr : Suppress_Stack_Entry_Ptr;

      begin
         Ptr := Top;
         while Ptr /= null loop
            if Ptr.Entity = From
              and then (Ptr.Check = All_Checks or else Ptr.Check = C)
            then
               if Ptr.Suppress then
                  Set_Checks_May_Be_Suppressed (To, True);
                  Push_Local_Suppress_Stack_Entry
                    (Entity   => To,
                     Check    => C,
                     Suppress => True);
                  Found := True;
                  return;
               end if;
            end if;

            Ptr := Ptr.Prev;
         end loop;

         Found := False;
         return;
      end Search_Stack;

   --  Start of processing for Copy_Suppress_Status

   begin
      if not Checks_May_Be_Suppressed (From) then
         return;
      end if;

      --  First search the global entity suppress table for a matching entry.
      --  We also search this in reverse order so that if there are multiple
      --  pragmas for the same entity, the last one applies.

      Search_Stack (Global_Suppress_Stack_Top, Found);

      if Found then
         return;
      end if;

      --  Now search the local entity suppress stack, we search this in
      --  reverse order so that we get the innermost entry that applies to
      --  this case if there are nested entries. Note that for the purpose
      --  of this procedure we are ONLY looking for entries corresponding
      --  to a two-argument Suppress, where the second argument matches From.

      Search_Stack (Local_Suppress_Stack_Top, Found);
   end Copy_Suppress_Status;

   -------------------------
   -- Enter_Generic_Scope --
   -------------------------

   procedure Enter_Generic_Scope (S : Entity_Id) is
   begin
      if No (Outer_Generic_Scope) then
         Outer_Generic_Scope := S;
      end if;
   end Enter_Generic_Scope;

   ------------------------
   -- Exit_Generic_Scope --
   ------------------------

   procedure Exit_Generic_Scope  (S : Entity_Id) is
   begin
      if S = Outer_Generic_Scope then
         Outer_Generic_Scope := Empty;
      end if;
   end Exit_Generic_Scope;

   -----------------------
   -- Explicit_Suppress --
   -----------------------

   function Explicit_Suppress (E : Entity_Id; C : Check_Id) return Boolean is
      Ptr : Suppress_Stack_Entry_Ptr;

   begin
      if not Checks_May_Be_Suppressed (E) then
         return False;

      else
         Ptr := Global_Suppress_Stack_Top;
         while Ptr /= null loop
            if Ptr.Entity = E
              and then (Ptr.Check = All_Checks or else Ptr.Check = C)
            then
               return Ptr.Suppress;
            end if;

            Ptr := Ptr.Prev;
         end loop;
      end if;

      return False;
   end Explicit_Suppress;

   -----------------------------
   -- External_Ref_In_Generic --
   -----------------------------

   function External_Ref_In_Generic (E : Entity_Id) return Boolean is
      Scop : Entity_Id;

   begin
      --  Entity is global if defined outside of current outer_generic_scope:
      --  Either the entity has a smaller depth that the outer generic, or it
      --  is in a different compilation unit, or it is defined within a unit
      --  in the same compilation, that is not within the outer_generic.

      if No (Outer_Generic_Scope) then
         return False;

      elsif Scope_Depth (Scope (E)) < Scope_Depth (Outer_Generic_Scope)
        or else not In_Same_Source_Unit (E, Outer_Generic_Scope)
      then
         return True;

      else
         Scop := Scope (E);
         while Present (Scop) loop
            if Scop = Outer_Generic_Scope then
               return False;
            elsif Scope_Depth (Scop) < Scope_Depth (Outer_Generic_Scope) then
               return True;
            else
               Scop := Scope (Scop);
            end if;
         end loop;

         return True;
      end if;
   end External_Ref_In_Generic;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Next : Suppress_Stack_Entry_Ptr;

      procedure Free is new Unchecked_Deallocation
        (Suppress_Stack_Entry, Suppress_Stack_Entry_Ptr);

   begin
      --  Free any global suppress stack entries from a previous invocation
      --  of the compiler (in the normal case this loop does nothing).

      while Suppress_Stack_Entries /= null loop
         Next := Suppress_Stack_Entries.Next;
         Free (Suppress_Stack_Entries);
         Suppress_Stack_Entries := Next;
      end loop;

      Local_Suppress_Stack_Top := null;
      Global_Suppress_Stack_Top := null;

      --  Clear scope stack, and reset global variables

      Scope_Stack.Init;
      Unloaded_Subunits := False;
   end Initialize;

   ------------------------------
   -- Insert_After_And_Analyze --
   ------------------------------

   procedure Insert_After_And_Analyze (N : Node_Id; M : Node_Id) is
      Node : Node_Id;

   begin
      if Present (M) then

         --  If we are not at the end of the list, then the easiest
         --  coding is simply to insert before our successor

         if Present (Next (N)) then
            Insert_Before_And_Analyze (Next (N), M);

         --  Case of inserting at the end of the list

         else
            --  Capture the Node_Id of the node to be inserted. This Node_Id
            --  will still be the same after the insert operation.

            Node := M;
            Insert_After (N, M);

            --  Now just analyze from the inserted node to the end of
            --  the new list (note that this properly handles the case
            --  where any of the analyze calls result in the insertion of
            --  nodes after the analyzed node, expecting analysis).

            while Present (Node) loop
               Analyze (Node);
               Mark_Rewrite_Insertion (Node);
               Next (Node);
            end loop;
         end if;
      end if;
   end Insert_After_And_Analyze;

   --  Version with check(s) suppressed

   procedure Insert_After_And_Analyze
     (N        : Node_Id;
      M        : Node_Id;
      Suppress : Check_Id)
   is
   begin
      if Suppress = All_Checks then
         declare
            Svg : constant Suppress_Array := Scope_Suppress;
         begin
            Scope_Suppress := (others => True);
            Insert_After_And_Analyze (N, M);
            Scope_Suppress := Svg;
         end;

      else
         declare
            Svg : constant Boolean := Scope_Suppress (Suppress);
         begin
            Scope_Suppress (Suppress) := True;
            Insert_After_And_Analyze (N, M);
            Scope_Suppress (Suppress) := Svg;
         end;
      end if;
   end Insert_After_And_Analyze;

   -------------------------------
   -- Insert_Before_And_Analyze --
   -------------------------------

   procedure Insert_Before_And_Analyze (N : Node_Id; M : Node_Id) is
      Node : Node_Id;

   begin
      if Present (M) then

         --  Capture the Node_Id of the first list node to be inserted.
         --  This will still be the first node after the insert operation,
         --  since Insert_List_After does not modify the Node_Id values.

         Node := M;
         Insert_Before (N, M);

         --  The insertion does not change the Id's of any of the nodes in
         --  the list, and they are still linked, so we can simply loop from
         --  the original first node until we meet the node before which the
         --  insertion is occurring. Note that this properly handles the case
         --  where any of the analyzed nodes insert nodes after themselves,
         --  expecting them to get analyzed.

         while Node /= N loop
            Analyze (Node);
            Mark_Rewrite_Insertion (Node);
            Next (Node);
         end loop;
      end if;
   end Insert_Before_And_Analyze;

   --  Version with check(s) suppressed

   procedure Insert_Before_And_Analyze
     (N        : Node_Id;
      M        : Node_Id;
      Suppress : Check_Id)
   is
   begin
      if Suppress = All_Checks then
         declare
            Svg : constant Suppress_Array := Scope_Suppress;
         begin
            Scope_Suppress := (others => True);
            Insert_Before_And_Analyze (N, M);
            Scope_Suppress := Svg;
         end;

      else
         declare
            Svg : constant Boolean := Scope_Suppress (Suppress);
         begin
            Scope_Suppress (Suppress) := True;
            Insert_Before_And_Analyze (N, M);
            Scope_Suppress (Suppress) := Svg;
         end;
      end if;
   end Insert_Before_And_Analyze;

   -----------------------------------
   -- Insert_List_After_And_Analyze --
   -----------------------------------

   procedure Insert_List_After_And_Analyze (N : Node_Id; L : List_Id) is
      After : constant Node_Id := Next (N);
      Node  : Node_Id;

   begin
      if Is_Non_Empty_List (L) then

         --  Capture the Node_Id of the first list node to be inserted.
         --  This will still be the first node after the insert operation,
         --  since Insert_List_After does not modify the Node_Id values.

         Node := First (L);
         Insert_List_After (N, L);

         --  Now just analyze from the original first node until we get to the
         --  successor of the original insertion point (which may be Empty if
         --  the insertion point was at the end of the list). Note that this
         --  properly handles the case where any of the analyze calls result in
         --  the insertion of nodes after the analyzed node (possibly calling
         --  this routine recursively).

         while Node /= After loop
            Analyze (Node);
            Mark_Rewrite_Insertion (Node);
            Next (Node);
         end loop;
      end if;
   end Insert_List_After_And_Analyze;

   --  Version with check(s) suppressed

   procedure Insert_List_After_And_Analyze
     (N : Node_Id; L : List_Id; Suppress : Check_Id)
   is
   begin
      if Suppress = All_Checks then
         declare
            Svg : constant Suppress_Array := Scope_Suppress;
         begin
            Scope_Suppress := (others => True);
            Insert_List_After_And_Analyze (N, L);
            Scope_Suppress := Svg;
         end;

      else
         declare
            Svg : constant Boolean := Scope_Suppress (Suppress);
         begin
            Scope_Suppress (Suppress) := True;
            Insert_List_After_And_Analyze (N, L);
            Scope_Suppress (Suppress) := Svg;
         end;
      end if;
   end Insert_List_After_And_Analyze;

   ------------------------------------
   -- Insert_List_Before_And_Analyze --
   ------------------------------------

   procedure Insert_List_Before_And_Analyze (N : Node_Id; L : List_Id) is
      Node : Node_Id;

   begin
      if Is_Non_Empty_List (L) then

         --  Capture the Node_Id of the first list node to be inserted. This
         --  will still be the first node after the insert operation, since
         --  Insert_List_After does not modify the Node_Id values.

         Node := First (L);
         Insert_List_Before (N, L);

         --  The insertion does not change the Id's of any of the nodes in
         --  the list, and they are still linked, so we can simply loop from
         --  the original first node until we meet the node before which the
         --  insertion is occurring. Note that this properly handles the case
         --  where any of the analyzed nodes insert nodes after themselves,
         --  expecting them to get analyzed.

         while Node /= N loop
            Analyze (Node);
            Mark_Rewrite_Insertion (Node);
            Next (Node);
         end loop;
      end if;
   end Insert_List_Before_And_Analyze;

   --  Version with check(s) suppressed

   procedure Insert_List_Before_And_Analyze
     (N : Node_Id; L : List_Id; Suppress : Check_Id)
   is
   begin
      if Suppress = All_Checks then
         declare
            Svg : constant Suppress_Array := Scope_Suppress;
         begin
            Scope_Suppress := (others => True);
            Insert_List_Before_And_Analyze (N, L);
            Scope_Suppress := Svg;
         end;

      else
         declare
            Svg : constant Boolean := Scope_Suppress (Suppress);
         begin
            Scope_Suppress (Suppress) := True;
            Insert_List_Before_And_Analyze (N, L);
            Scope_Suppress (Suppress) := Svg;
         end;
      end if;
   end Insert_List_Before_And_Analyze;

   -------------------------
   -- Is_Check_Suppressed --
   -------------------------

   function Is_Check_Suppressed (E : Entity_Id; C : Check_Id) return Boolean is

      Ptr : Suppress_Stack_Entry_Ptr;

   begin
      --  First search the local entity suppress stack. We search this from the
      --  top of the stack down so that we get the innermost entry that applies
      --  to this case if there are nested entries.

      Ptr := Local_Suppress_Stack_Top;
      while Ptr /= null loop
         if (Ptr.Entity = Empty or else Ptr.Entity = E)
           and then (Ptr.Check = All_Checks or else Ptr.Check = C)
         then
            return Ptr.Suppress;
         end if;

         Ptr := Ptr.Prev;
      end loop;

      --  Now search the global entity suppress table for a matching entry.
      --  We also search this from the top down so that if there are multiple
      --  pragmas for the same entity, the last one applies (not clear what
      --  or whether the RM specifies this handling, but it seems reasonable).

      Ptr := Global_Suppress_Stack_Top;
      while Ptr /= null loop
         if (Ptr.Entity = Empty or else Ptr.Entity = E)
           and then (Ptr.Check = All_Checks or else Ptr.Check = C)
         then
            return Ptr.Suppress;
         end if;

         Ptr := Ptr.Prev;
      end loop;

      --  If we did not find a matching entry, then use the normal scope
      --  suppress value after all (actually this will be the global setting
      --  since it clearly was not overridden at any point). For a predefined
      --  check, we test the specific flag. For a user defined check, we check
      --  the All_Checks flag.

      if C in Predefined_Check_Id then
         return Scope_Suppress (C);
      else
         return Scope_Suppress (All_Checks);
      end if;
   end Is_Check_Suppressed;

   ----------
   -- Lock --
   ----------

   procedure Lock is
   begin
      Scope_Stack.Locked := True;
      Scope_Stack.Release;
   end Lock;

   ----------------
   -- Preanalyze --
   ----------------

   procedure Preanalyze (N : Node_Id) is
      Save_Full_Analysis : constant Boolean := Full_Analysis;

   begin
      Full_Analysis := False;
      Expander_Mode_Save_And_Set (False);

      Analyze (N);

      Expander_Mode_Restore;
      Full_Analysis := Save_Full_Analysis;
   end Preanalyze;

   --------------------------------------
   -- Push_Global_Suppress_Stack_Entry --
   --------------------------------------

   procedure Push_Global_Suppress_Stack_Entry
     (Entity   : Entity_Id;
      Check    : Check_Id;
      Suppress : Boolean)
   is
   begin
      Global_Suppress_Stack_Top :=
        new Suppress_Stack_Entry'
          (Entity   => Entity,
           Check    => Check,
           Suppress => Suppress,
           Prev     => Global_Suppress_Stack_Top,
           Next     => Suppress_Stack_Entries);
      Suppress_Stack_Entries := Global_Suppress_Stack_Top;
      return;

   end Push_Global_Suppress_Stack_Entry;

   -------------------------------------
   -- Push_Local_Suppress_Stack_Entry --
   -------------------------------------

   procedure Push_Local_Suppress_Stack_Entry
     (Entity   : Entity_Id;
      Check    : Check_Id;
      Suppress : Boolean)
   is
   begin
      Local_Suppress_Stack_Top :=
        new Suppress_Stack_Entry'
          (Entity   => Entity,
           Check    => Check,
           Suppress => Suppress,
           Prev     => Local_Suppress_Stack_Top,
           Next     => Suppress_Stack_Entries);
      Suppress_Stack_Entries := Local_Suppress_Stack_Top;

      return;
   end Push_Local_Suppress_Stack_Entry;

   ---------------
   -- Semantics --
   ---------------

   procedure Semantics (Comp_Unit : Node_Id) is

      --  The following locations save the corresponding global flags and
      --  variables so that they can be restored on completion. This is needed
      --  so that calls to Rtsfind start with the proper default values for
      --  these variables, and also that such calls do not disturb the settings
      --  for units being analyzed at a higher level.

      S_Current_Sem_Unit : constant Unit_Number_Type := Current_Sem_Unit;
      S_Full_Analysis    : constant Boolean          := Full_Analysis;
      S_GNAT_Mode        : constant Boolean          := GNAT_Mode;
      S_Global_Dis_Names : constant Boolean          := Global_Discard_Names;
      S_In_Spec_Expr     : constant Boolean          := In_Spec_Expression;
      S_Inside_A_Generic : constant Boolean          := Inside_A_Generic;
      S_Outer_Gen_Scope  : constant Entity_Id        := Outer_Generic_Scope;

      Generic_Main : constant Boolean :=
                       Nkind (Unit (Cunit (Main_Unit)))
                         in N_Generic_Declaration;
      --  If the main unit is generic, every compiled unit, including its
      --  context, is compiled with expansion disabled.

      Save_Config_Switches : Config_Switches_Type;
      --  Variable used to save values of config switches while we analyze the
      --  new unit, to be restored on exit for proper recursive behavior.

      Save_Cunit_Restrictions : Save_Cunit_Boolean_Restrictions;
      --  Used to save non-partition wide restrictions before processing new
      --  unit. All with'ed units are analyzed with config restrictions reset
      --  and we need to restore these saved values at the end.

      procedure Do_Analyze;
      --  Procedure to analyze the compilation unit

      ----------------
      -- Do_Analyze --
      ----------------

      procedure Do_Analyze is
      begin
         Save_Scope_Stack;
         Push_Scope (Standard_Standard);
         Scope_Suppress := Suppress_Options;
         Scope_Stack.Table
           (Scope_Stack.Last).Component_Alignment_Default := Calign_Default;
         Scope_Stack.Table
           (Scope_Stack.Last).Is_Active_Stack_Base := True;
         Outer_Generic_Scope := Empty;

         --  Now analyze the top level compilation unit node

         Analyze (Comp_Unit);

         --  Check for scope mismatch on exit from compilation

         pragma Assert (Current_Scope = Standard_Standard
                          or else Comp_Unit = Cunit (Main_Unit));

         --  Then pop entry for Standard, and pop implicit types

         Pop_Scope;
         Restore_Scope_Stack;
      end Do_Analyze;

      Already_Analyzed : constant Boolean := Analyzed (Comp_Unit);

   --  Start of processing for Semantics

   begin
      if Debug_Unit_Walk then
         if Already_Analyzed then
            Write_Str ("(done)");
         end if;

         Write_Unit_Info
           (Get_Cunit_Unit_Number (Comp_Unit),
            Unit (Comp_Unit),
            Prefix => "--> ");
         Indent;
      end if;

      Compiler_State   := Analyzing;
      Current_Sem_Unit := Get_Cunit_Unit_Number (Comp_Unit);

      --  Compile predefined units with GNAT_Mode set to True, to properly
      --  process the categorization stuff. However, do not set GNAT_Mode
      --  to True for the renamings units (Text_IO, IO_Exceptions, Direct_IO,
      --  Sequential_IO) as this would prevent pragma Extend_System from being
      --  taken into account, for example when Text_IO is renaming DEC.Text_IO.

      --  Cleaner might be to do the kludge at the point of excluding the
      --  pragma (do not exclude for renamings ???)

      if Is_Predefined_File_Name
           (Unit_File_Name (Current_Sem_Unit), Renamings_Included => False)
      then
         GNAT_Mode := True;
      end if;

      if Generic_Main then
         Expander_Mode_Save_And_Set (False);
      else
         Expander_Mode_Save_And_Set
           (Operating_Mode = Generate_Code or Debug_Flag_X);
      end if;

      Full_Analysis      := True;
      Inside_A_Generic   := False;
      In_Spec_Expression := False;

      Set_Comes_From_Source_Default (False);

      --  Save current config switches and reset then appropriately

      Save_Opt_Config_Switches (Save_Config_Switches);
      Set_Opt_Config_Switches
        (Is_Internal_File_Name (Unit_File_Name (Current_Sem_Unit)),
         Current_Sem_Unit = Main_Unit);

      --  Save current non-partition-wide restrictions

      Save_Cunit_Restrictions := Cunit_Boolean_Restrictions_Save;

      --  For unit in main extended unit, we reset the configuration values
      --  for the non-partition-wide restrictions. For other units reset them.

      if In_Extended_Main_Source_Unit (Comp_Unit) then
         Restore_Config_Cunit_Boolean_Restrictions;
      else
         Reset_Cunit_Boolean_Restrictions;
      end if;

      --  Only do analysis of unit that has not already been analyzed

      if not Analyzed (Comp_Unit) then
         Initialize_Version (Current_Sem_Unit);

         --  Do analysis, and then append the compilation unit onto the
         --  Comp_Unit_List, if appropriate. This is done after analysis,
         --  so if this unit depends on some others, they have already been
         --  appended. We ignore bodies, except for the main unit itself, and
         --  for subprogram bodies that act as specs. We have also to guard
         --  against ill-formed subunits that have an improper context.

         Do_Analyze;

         if Present (Comp_Unit)
           and then Nkind (Unit (Comp_Unit)) in N_Proper_Body
           and then (Nkind (Unit (Comp_Unit)) /= N_Subprogram_Body
                       or else not Acts_As_Spec (Comp_Unit))
           and then not In_Extended_Main_Source_Unit (Comp_Unit)
         then
            null;

         else
            --  Initialize if first time

            if No (Comp_Unit_List) then
               Comp_Unit_List := New_Elmt_List;
            end if;

            Append_Elmt (Comp_Unit, Comp_Unit_List);

            if Debug_Unit_Walk then
               Write_Str ("Appending ");
               Write_Unit_Info
                 (Get_Cunit_Unit_Number (Comp_Unit), Unit (Comp_Unit));
            end if;
         end if;
      end if;

      --  Save indication of dynamic elaboration checks for ALI file

      Set_Dynamic_Elab (Current_Sem_Unit, Dynamic_Elaboration_Checks);

      --  Restore settings of saved switches to entry values

      Current_Sem_Unit     := S_Current_Sem_Unit;
      Full_Analysis        := S_Full_Analysis;
      Global_Discard_Names := S_Global_Dis_Names;
      GNAT_Mode            := S_GNAT_Mode;
      In_Spec_Expression   := S_In_Spec_Expr;
      Inside_A_Generic     := S_Inside_A_Generic;
      Outer_Generic_Scope  := S_Outer_Gen_Scope;

      Restore_Opt_Config_Switches (Save_Config_Switches);

      --  Deal with restore of restrictions

      Cunit_Boolean_Restrictions_Restore (Save_Cunit_Restrictions);

      Expander_Mode_Restore;

      if Debug_Unit_Walk then
         Outdent;

         if Already_Analyzed then
            Write_Str ("(done)");
         end if;

         Write_Unit_Info
           (Get_Cunit_Unit_Number (Comp_Unit),
            Unit (Comp_Unit),
            Prefix => "<-- ");
      end if;
   end Semantics;

   --------
   -- ss --
   --------

   function ss (Index : Int) return Scope_Stack_Entry is
   begin
      return Scope_Stack.Table (Index);
   end ss;

   ---------
   -- sst --
   ---------

   function sst return Scope_Stack_Entry is
   begin
      return ss (Scope_Stack.Last);
   end sst;

   ------------------------
   -- Walk_Library_Items --
   ------------------------

   procedure Walk_Library_Items is
      type Unit_Number_Set is array (Main_Unit .. Last_Unit) of Boolean;
      pragma Pack (Unit_Number_Set);

      Main_CU : constant Node_Id := Cunit (Main_Unit);

      Seen, Done : Unit_Number_Set := (others => False);
      --  Seen (X) is True after we have seen unit X in the walk. This is used
      --  to prevent processing the same unit more than once. Done (X) is True
      --  after we have fully processed X, and is used only for debugging
      --  printouts and assertions.

      Do_Main : Boolean := False;
      --  Flag to delay processing the main body until after all other units.
      --  This is needed because the spec of the main unit may appear in the
      --  context of some other unit. We do not want this to force processing
      --  of the main body before all other units have been processed.
      --
      --  Another circularity pattern occurs when the main unit is a child unit
      --  and the body of an ancestor has a with-clause of the main unit or on
      --  one of its children. In both cases the body in question has a with-
      --  clause on the main unit, and must be excluded from the traversal. In
      --  some convoluted cases this may lead to a CodePeer error because the
      --  spec of a subprogram declared in an instance within the parent will
      --  not be seen in the main unit.

      function Depends_On_Main (CU : Node_Id) return Boolean;
      --  The body of a unit that is withed by the spec of the main unit may in
      --  turn have a with_clause on that spec. In that case do not traverse
      --  the body, to prevent loops. It can also happen that the main body has
      --  a with_clause on a child, which of course has an implicit with on its
      --  parent. It's OK to traverse the child body if the main spec has been
      --  processed, otherwise we also have a circularity to avoid.

      procedure Do_Action (CU : Node_Id; Item : Node_Id);
      --  Calls Action, with some validity checks

      procedure Do_Unit_And_Dependents (CU : Node_Id; Item : Node_Id);
      --  Calls Do_Action, first on the units with'ed by this one, then on
      --  this unit. If it's an instance body, do the spec first. If it is
      --  an instance spec, do the body last.

      procedure Do_Withed_Unit (Withed_Unit : Node_Id);
      --  Apply Do_Unit_And_Dependents to a unit in a context clause

      procedure Process_Bodies_In_Context (Comp : Node_Id);
      --  The main unit and its spec may depend on bodies that contain generics
      --  that are instantiated in them. Iterate through the corresponding
      --  contexts before processing main (spec/body) itself, to process bodies
      --  that may be present, together with their  context. The spec of main
      --  is processed wherever it appears in the list of units, while the body
      --  is processed as the last unit in the list.

      ---------------------
      -- Depends_On_Main --
      ---------------------

      function Depends_On_Main (CU : Node_Id) return Boolean is
         CL  : Node_Id;
         MCU : constant Node_Id := Unit (Main_CU);

      begin
         CL := First (Context_Items (CU));

         --  Problem does not arise with main subprograms

         if
           not Nkind_In (MCU, N_Package_Body, N_Package_Declaration)
         then
            return False;
         end if;

         while Present (CL) loop
            if Nkind (CL) = N_With_Clause
              and then Library_Unit (CL) = Main_CU
              and then not Done (Get_Cunit_Unit_Number (Library_Unit (CL)))
            then
               return True;
            end if;

            Next (CL);
         end loop;

         return False;
      end Depends_On_Main;

      ---------------
      -- Do_Action --
      ---------------

      procedure Do_Action (CU : Node_Id; Item : Node_Id) is
      begin
         --  This calls Action at the end. All the preceding code is just
         --  assertions and debugging output.

         pragma Assert (No (CU) or else Nkind (CU) = N_Compilation_Unit);

         case Nkind (Item) is
            when N_Generic_Subprogram_Declaration        |
                 N_Generic_Package_Declaration           |
                 N_Package_Declaration                   |
                 N_Subprogram_Declaration                |
                 N_Subprogram_Renaming_Declaration       |
                 N_Package_Renaming_Declaration          |
                 N_Generic_Function_Renaming_Declaration |
                 N_Generic_Package_Renaming_Declaration  |
                 N_Generic_Procedure_Renaming_Declaration =>

               --  Specs are OK

               null;

            when N_Package_Body  =>

               --  Package bodies are processed separately if the main unit
               --  depends on them.

               null;

            when N_Subprogram_Body =>

               --  A subprogram body must be the main unit

               pragma Assert (Acts_As_Spec (CU)
                               or else CU = Cunit (Main_Unit));
               null;

            when N_Function_Instantiation  |
                 N_Procedure_Instantiation |
                 N_Package_Instantiation   =>

               --  Can only happen if some generic body (needed for gnat2scil
               --  traversal, but not by GNAT) is not available, ignore.

               null;

            --  All other cases cannot happen

            when N_Subunit =>
               pragma Assert (False, "subunit");
               null;

            when others =>
               pragma Assert (False);
               null;
         end case;

         if Present (CU) then
            pragma Assert (Item /= Stand.Standard_Package_Node);
            pragma Assert (Item = Unit (CU));

            declare
               Unit_Num : constant Unit_Number_Type :=
                            Get_Cunit_Unit_Number (CU);

               procedure Assert_Done (Withed_Unit : Node_Id);
               --  Assert Withed_Unit is already Done, unless it's a body. It
               --  might seem strange for a with_clause to refer to a body, but
               --  this happens in the case of a generic instantiation, which
               --  gets transformed into the instance body (and the instance
               --  spec is also created). With clauses pointing to the
               --  instantiation end up pointing to the instance body.

               -----------------
               -- Assert_Done --
               -----------------

               procedure Assert_Done (Withed_Unit : Node_Id) is
               begin
                  if not Done (Get_Cunit_Unit_Number (Withed_Unit)) then
                     if not Nkind_In
                              (Unit (Withed_Unit),
                                 N_Generic_Package_Declaration,
                                 N_Package_Body,
                                 N_Package_Renaming_Declaration,
                                 N_Subprogram_Body)
                     then
                        Write_Unit_Name
                          (Unit_Name (Get_Cunit_Unit_Number (Withed_Unit)));
                        Write_Str (" not yet walked!");

                        if Get_Cunit_Unit_Number (Withed_Unit) = Unit_Num then
                           Write_Str (" (self-ref)");
                        end if;

                        Write_Eol;

                        pragma Assert (False);
                     end if;
                  end if;
               end Assert_Done;

               procedure Assert_Withed_Units_Done is
                 new Walk_Withs (Assert_Done);

            begin
               if Debug_Unit_Walk then
                  Write_Unit_Info (Unit_Num, Item, Withs => True);
               end if;

               --  Main unit should come last, except in the case where we
               --  skipped System_Aux_Id, in which case we missed the things it
               --  depends on, and in the case of parent bodies if present.

               pragma Assert
                 (not Done (Main_Unit)
                  or else Present (System_Aux_Id)
                  or else Nkind (Item) = N_Package_Body);

               --  We shouldn't do the same thing twice

               pragma Assert (not Done (Unit_Num));

               --  Everything we depend upon should already be done

               pragma Debug
                 (Assert_Withed_Units_Done (CU, Include_Limited => False));
            end;

         else
            --  Must be Standard, which has no entry in the units table

            pragma Assert (Item = Stand.Standard_Package_Node);

            if Debug_Unit_Walk then
               Write_Line ("Standard");
            end if;
         end if;

         Action (Item);
      end Do_Action;

      --------------------
      -- Do_Withed_Unit --
      --------------------

      procedure Do_Withed_Unit (Withed_Unit : Node_Id) is
      begin
         Do_Unit_And_Dependents (Withed_Unit, Unit (Withed_Unit));

         --  If the unit in the with_clause is a generic instance, the clause
         --  now denotes the instance body. Traverse the corresponding spec
         --  because there may be no other dependence that will force the
         --  traversal of its own context.

         if Nkind (Unit (Withed_Unit)) = N_Package_Body
           and then Is_Generic_Instance
                      (Defining_Entity (Unit (Library_Unit (Withed_Unit))))
         then
            Do_Withed_Unit (Library_Unit (Withed_Unit));
         end if;
      end Do_Withed_Unit;

      ----------------------------
      -- Do_Unit_And_Dependents --
      ----------------------------

      procedure Do_Unit_And_Dependents (CU : Node_Id; Item : Node_Id) is
         Unit_Num  : constant Unit_Number_Type := Get_Cunit_Unit_Number (CU);
         Child     : Node_Id;
         Body_U    : Unit_Number_Type;
         Parent_CU : Node_Id;

         procedure Do_Withed_Units is new Walk_Withs (Do_Withed_Unit);

      begin
         if not Seen (Unit_Num) then

            --  Process the with clauses

            Do_Withed_Units (CU, Include_Limited => False);

            --  Process the unit if it is a spec or the main unit, if it
            --  has no previous spec or we have done all other units.

            if not Nkind_In (Item, N_Package_Body, N_Subprogram_Body)
              or else Acts_As_Spec (CU)
            then
               if CU = Cunit (Main_Unit)
                   and then not Do_Main
               then
                  Seen (Unit_Num) := False;

               else
                  Seen (Unit_Num) := True;

                  if CU = Library_Unit (Main_CU) then
                     Process_Bodies_In_Context (CU);

                     --  If main is a child unit, examine parent unit contexts
                     --  to see if they include instantiated units. Also, if
                     --  the parent itself is an instance, process its body
                     --  because it may contain subprograms that are called
                     --  in the main unit.

                     if Is_Child_Unit (Cunit_Entity (Main_Unit)) then
                        Child := Cunit_Entity (Main_Unit);
                        while Is_Child_Unit (Child) loop
                           Parent_CU :=
                             Cunit
                               (Get_Cunit_Entity_Unit_Number (Scope (Child)));
                           Process_Bodies_In_Context (Parent_CU);

                           if Nkind (Unit (Parent_CU)) = N_Package_Body
                             and then
                               Nkind (Original_Node (Unit (Parent_CU)))
                                 = N_Package_Instantiation
                             and then
                               not Seen (Get_Cunit_Unit_Number (Parent_CU))
                           then
                              Body_U := Get_Cunit_Unit_Number (Parent_CU);
                              Seen (Body_U) := True;
                              Do_Action (Parent_CU, Unit (Parent_CU));
                              Done (Body_U) := True;
                           end if;

                           Child := Scope (Child);
                        end loop;
                     end if;
                  end if;

                  Do_Action (CU, Item);
                  Done (Unit_Num) := True;
               end if;
            end if;
         end if;
      end Do_Unit_And_Dependents;

      -------------------------------
      -- Process_Bodies_In_Context --
      -------------------------------

      procedure Process_Bodies_In_Context (Comp : Node_Id) is
         Body_CU : Node_Id;
         Body_U  : Unit_Number_Type;
         Clause  : Node_Id;
         Spec    : Node_Id;

         procedure Do_Withed_Units is new Walk_Withs (Do_Withed_Unit);

      --  Start of processing for Process_Bodies_In_Context

      begin
         Clause := First (Context_Items (Comp));
         while Present (Clause) loop
            if Nkind (Clause) = N_With_Clause then
               Spec := Library_Unit (Clause);
               Body_CU := Library_Unit (Spec);

               --  If we are processing the spec of the main unit, load bodies
               --  only if the with_clause indicates that it forced the loading
               --  of the body for a generic instantiation. Note that bodies of
               --  parents that are instances have been loaded already.

               if Present (Body_CU)
                 and then Body_CU /= Cunit (Main_Unit)
                 and then Nkind (Unit (Body_CU)) /= N_Subprogram_Body
                 and then (Nkind (Unit (Comp)) /= N_Package_Declaration
                             or else Present (Withed_Body (Clause)))
               then
                  Body_U := Get_Cunit_Unit_Number (Body_CU);

                  if not Seen (Body_U)
                    and then not Depends_On_Main (Body_CU)
                  then
                     Seen (Body_U) := True;
                     Do_Withed_Units (Body_CU, Include_Limited => False);
                     Do_Action (Body_CU, Unit (Body_CU));
                     Done (Body_U) := True;
                  end if;
               end if;
            end if;

            Next (Clause);
         end loop;
      end Process_Bodies_In_Context;

      --  Local Declarations

      Cur : Elmt_Id;

   --  Start of processing for Walk_Library_Items

   begin
      if Debug_Unit_Walk then
         Write_Line ("Walk_Library_Items:");
         Indent;
      end if;

      --  Do Standard first, then walk the Comp_Unit_List

      Do_Action (Empty, Standard_Package_Node);

      --  First place the context of all instance bodies on the corresponding
      --  spec, because it may be needed to analyze the code at the place of
      --  the instantiation.

      Cur := First_Elmt (Comp_Unit_List);
      while Present (Cur) loop
         declare
            CU : constant Node_Id := Node (Cur);
            N  : constant Node_Id := Unit (CU);

         begin
            if Nkind (N) = N_Package_Body
              and then Is_Generic_Instance (Defining_Entity (N))
            then
               Append_List
                 (Context_Items (CU), Context_Items (Library_Unit (CU)));
            end if;

            Next_Elmt (Cur);
         end;
      end loop;

      --  Now traverse compilation units (specs) in order

      Cur := First_Elmt (Comp_Unit_List);
      while Present (Cur) loop
         declare
            CU  : constant Node_Id := Node (Cur);
            N   : constant Node_Id := Unit (CU);
            Par : Entity_Id;

         begin
            pragma Assert (Nkind (CU) = N_Compilation_Unit);

            case Nkind (N) is

               --  If it is a subprogram body, process it if it has no
               --  separate spec.

               --  If it's a package body, ignore it, unless it is a body
               --  created for an instance that is the main unit. In the case
               --  of subprograms, the body is the wrapper package. In case of
               --  a package, the original file carries the body, and the spec
               --  appears as a later entry in the units list.

               --  Otherwise bodies appear in the list only because of inlining
               --  or instantiations, and they are processed only if relevant.
               --  The flag Withed_Body on a context clause indicates that a
               --  unit contains an instantiation that may be needed later,
               --  and therefore the body that contains the generic body (and
               --  its context)  must be traversed immediately after the
               --  corresponding spec (see Do_Unit_And_Dependents).

               --  The main unit itself is processed separately after all other
               --  specs, and relevant bodies are examined in Process_Main.

               when N_Subprogram_Body =>
                  if Acts_As_Spec (N) then
                     Do_Unit_And_Dependents (CU, N);
                  end if;

               when N_Package_Body =>
                  if CU = Main_CU
                    and then Nkind (Original_Node (Unit (Main_CU))) in
                                                  N_Generic_Instantiation
                    and then Present (Library_Unit (Main_CU))
                  then
                     Do_Unit_And_Dependents
                       (Library_Unit (Main_CU),
                        Unit (Library_Unit (Main_CU)));
                  end if;

                  --  It's a spec, process it, and the units it depends on,
                  --  unless it is a descendent of the main unit.  This can
                  --  happen when the body of a parent depends on some other
                  --  descendent.

               when others =>
                  Par := Scope (Defining_Entity (Unit (CU)));

                  if Is_Child_Unit (Defining_Entity (Unit (CU))) then
                     while Present (Par)
                       and then Par /= Standard_Standard
                       and then Par /= Cunit_Entity (Main_Unit)
                     loop
                        Par := Scope (Par);
                     end loop;
                  end if;

                  if Par /= Cunit_Entity (Main_Unit) then
                     Do_Unit_And_Dependents (CU, N);
                  end if;
            end case;
         end;

         Next_Elmt (Cur);
      end loop;

      --  Now process package bodies on which main depends, followed by bodies
      --  of parents, if present, and finally main itself.

      if not Done (Main_Unit) then
         Do_Main := True;

         Process_Main : declare
            Parent_CU : Node_Id;
            Body_CU   : Node_Id;
            Body_U    : Unit_Number_Type;
            Child     : Entity_Id;

            function Is_Subunit_Of_Main (U : Node_Id) return Boolean;
            --  If the main unit has subunits, their context may include
            --  bodies that are needed in the body of main. We must examine
            --  the context of the subunits, which are otherwise not made
            --  explicit in the main unit.

            ------------------------
            -- Is_Subunit_Of_Main --
            ------------------------

            function Is_Subunit_Of_Main (U : Node_Id) return Boolean is
               Lib : Node_Id;
            begin
               if No (U) then
                  return False;
               else
                  Lib := Library_Unit (U);
                  return Nkind (Unit (U)) = N_Subunit
                    and then
                      (Lib = Cunit (Main_Unit)
                        or else Is_Subunit_Of_Main (Lib));
               end if;
            end Is_Subunit_Of_Main;

         --  Start of processing for Process_Main

         begin
            Process_Bodies_In_Context (Main_CU);

            for Unit_Num in Done'Range loop
               if Is_Subunit_Of_Main (Cunit (Unit_Num)) then
                  Process_Bodies_In_Context (Cunit (Unit_Num));
               end if;
            end loop;

            --  If the main unit is a child unit, parent bodies may be present
            --  because they export instances or inlined subprograms. Check for
            --  presence of these, which are not present in context clauses.
            --  Note that if the parents are instances, their bodies have been
            --  processed before the main spec, because they may be needed
            --  therein, so the following loop only affects non-instances.

            if Is_Child_Unit (Cunit_Entity (Main_Unit)) then
               Child := Cunit_Entity (Main_Unit);
               while Is_Child_Unit (Child) loop
                  Parent_CU :=
                    Cunit (Get_Cunit_Entity_Unit_Number (Scope (Child)));
                  Body_CU := Library_Unit (Parent_CU);

                  if Present (Body_CU)
                    and then not Seen (Get_Cunit_Unit_Number (Body_CU))
                    and then not Depends_On_Main (Body_CU)
                  then
                     Body_U := Get_Cunit_Unit_Number (Body_CU);
                     Seen (Body_U) := True;
                     Do_Action (Body_CU, Unit (Body_CU));
                     Done (Body_U) := True;
                  end if;

                  Child := Scope (Child);
               end loop;
            end if;

            Do_Action (Main_CU, Unit (Main_CU));
            Done (Main_Unit) := True;
         end Process_Main;
      end if;

      if Debug_Unit_Walk then
         if Done /= (Done'Range => True) then
            Write_Eol;
            Write_Line ("Ignored units:");

            Indent;

            for Unit_Num in Done'Range loop
               if not Done (Unit_Num) then
                  Write_Unit_Info
                    (Unit_Num, Unit (Cunit (Unit_Num)), Withs => True);
               end if;
            end loop;

            Outdent;
         end if;
      end if;

      pragma Assert (Done (Main_Unit));

      if Debug_Unit_Walk then
         Outdent;
         Write_Line ("end Walk_Library_Items.");
      end if;
   end Walk_Library_Items;

   ----------------
   -- Walk_Withs --
   ----------------

   procedure Walk_Withs (CU : Node_Id; Include_Limited : Boolean) is
      pragma Assert (Nkind (CU) = N_Compilation_Unit);
      pragma Assert (Nkind (Unit (CU)) /= N_Subunit);

      procedure Walk_Immediate is new Walk_Withs_Immediate (Action);

   begin
      --  First walk the withs immediately on the library item

      Walk_Immediate (CU, Include_Limited);

      --  For a body, we must also check for any subunits which belong to it
      --  and which have context clauses of their own, since these with'ed
      --  units are part of its own dependencies.

      if Nkind (Unit (CU)) in N_Unit_Body then
         for S in Main_Unit .. Last_Unit loop

            --  We are only interested in subunits. For preproc. data and def.
            --  files, Cunit is Empty, so we need to test that first.

            if Cunit (S) /= Empty
              and then Nkind (Unit (Cunit (S))) = N_Subunit
            then
               declare
                  Pnode : Node_Id;

               begin
                  Pnode := Library_Unit (Cunit (S));

                  --  In -gnatc mode, the errors in the subunits will not have
                  --  been recorded, but the analysis of the subunit may have
                  --  failed, so just quit.

                  if No (Pnode) then
                     exit;
                  end if;

                  --  Find ultimate parent of the subunit

                  while Nkind (Unit (Pnode)) = N_Subunit loop
                     Pnode := Library_Unit (Pnode);
                  end loop;

                  --  See if it belongs to current unit, and if so, include its
                  --  with_clauses. Do not process main unit prematurely.

                  if Pnode = CU and then CU /= Cunit (Main_Unit) then
                     Walk_Immediate (Cunit (S), Include_Limited);
                  end if;
               end;
            end if;
         end loop;
      end if;
   end Walk_Withs;

   --------------------------
   -- Walk_Withs_Immediate --
   --------------------------

   procedure Walk_Withs_Immediate (CU : Node_Id; Include_Limited : Boolean) is
      pragma Assert (Nkind (CU) = N_Compilation_Unit);

      Context_Item : Node_Id;
      Lib_Unit     : Node_Id;
      Body_CU      : Node_Id;

   begin
      Context_Item := First (Context_Items (CU));
      while Present (Context_Item) loop
         if Nkind (Context_Item) = N_With_Clause
           and then (Include_Limited
                     or else not Limited_Present (Context_Item))
         then
            Lib_Unit := Library_Unit (Context_Item);
            Action (Lib_Unit);

            --  If the context item indicates that a package body is needed
            --  because of an instantiation in CU, traverse the body now, even
            --  if CU is not related to the main unit. If the generic itself
            --  appears in a package body, the context item is this body, and
            --  it already appears in the traversal order, so we only need to
            --  examine the case of a context item being a package declaration.

            if Present (Withed_Body (Context_Item))
              and then Nkind (Unit (Lib_Unit)) = N_Package_Declaration
              and then Present (Corresponding_Body (Unit (Lib_Unit)))
            then
               Body_CU :=
                 Parent
                   (Unit_Declaration_Node
                     (Corresponding_Body (Unit (Lib_Unit))));

               --  A body may have an implicit with on its own spec, in which
               --  case we must ignore this context item to prevent looping.

               if Unit (CU) /= Unit (Body_CU) then
                  Action (Body_CU);
               end if;
            end if;
         end if;

         Context_Item := Next (Context_Item);
      end loop;
   end Walk_Withs_Immediate;

end Sem;
