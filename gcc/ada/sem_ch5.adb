------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 5                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-2002 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Atree;    use Atree;
with Checks;   use Checks;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Expander; use Expander;
with Exp_Util; use Exp_Util;
with Freeze;   use Freeze;
with Lib.Xref; use Lib.Xref;
with Nlists;   use Nlists;
with Opt;      use Opt;
with Sem;      use Sem;
with Sem_Case; use Sem_Case;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Disp; use Sem_Disp;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Type; use Sem_Type;
with Sem_Util; use Sem_Util;
with Sem_Warn; use Sem_Warn;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

package body Sem_Ch5 is

   Unblocked_Exit_Count : Nat := 0;
   --  This variable is used when processing if statements or case
   --  statements, it counts the number of branches of the conditional
   --  that are not blocked by unconditional transfer instructions. At
   --  the end of processing, if the count is zero, it means that control
   --  cannot fall through the conditional statement. This is used for
   --  the generation of warning messages. This variable is recursively
   --  saved on entry to processing an if or case, and restored on exit.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Analyze_Iteration_Scheme (N : Node_Id);

   ------------------------
   -- Analyze_Assignment --
   ------------------------

   procedure Analyze_Assignment (N : Node_Id) is
      Lhs    : constant Node_Id := Name (N);
      Rhs    : constant Node_Id := Expression (N);
      T1, T2 : Entity_Id;
      Decl   : Node_Id;

      procedure Diagnose_Non_Variable_Lhs (N : Node_Id);
      --  N is the node for the left hand side of an assignment, and it
      --  is not a variable. This routine issues an appropriate diagnostic.

      procedure Set_Assignment_Type
        (Opnd      : Node_Id;
         Opnd_Type : in out Entity_Id);
      --  Opnd is either the Lhs or Rhs of the assignment, and Opnd_Type
      --  is the nominal subtype. This procedure is used to deal with cases
      --  where the nominal subtype must be replaced by the actual subtype.

      -------------------------------
      -- Diagnose_Non_Variable_Lhs --
      -------------------------------

      procedure Diagnose_Non_Variable_Lhs (N : Node_Id) is
      begin
         --  Not worth posting another error if left hand side already
         --  flagged as being illegal in some respect

         if Error_Posted (N) then
            return;

         --  Some special bad cases of entity names

         elsif Is_Entity_Name (N) then

            if Ekind (Entity (N)) = E_In_Parameter then
               Error_Msg_N
                 ("assignment to IN mode parameter not allowed", N);
               return;

            --  Private declarations in a protected object are turned into
            --  constants when compiling a protected function.

            elsif Present (Scope (Entity (N)))
              and then Is_Protected_Type (Scope (Entity (N)))
              and then
                (Ekind (Current_Scope) = E_Function
                  or else
                 Ekind (Enclosing_Dynamic_Scope (Current_Scope)) = E_Function)
            then
               Error_Msg_N
                 ("protected function cannot modify protected object", N);
               return;

            elsif Ekind (Entity (N)) = E_Loop_Parameter then
               Error_Msg_N
                 ("assignment to loop parameter not allowed", N);
               return;

            end if;

         --  For indexed components, or selected components, test prefix

         elsif Nkind (N) = N_Indexed_Component
           or else Nkind (N) = N_Selected_Component
         then
            Diagnose_Non_Variable_Lhs (Prefix (N));
            return;
         end if;

         --  If we fall through, we have no special message to issue!

         Error_Msg_N ("left hand side of assignment must be a variable", N);

      end Diagnose_Non_Variable_Lhs;

      -------------------------
      -- Set_Assignment_Type --
      -------------------------

      procedure Set_Assignment_Type
        (Opnd      : Node_Id;
         Opnd_Type : in out Entity_Id)
      is
      begin
         --  If the assignment operand is an in-out or out parameter, then we
         --  get the actual subtype (needed for the unconstrained case).

         if Is_Entity_Name (Opnd)
           and then (Ekind (Entity (Opnd)) = E_Out_Parameter
                      or else Ekind (Entity (Opnd)) =
                           E_In_Out_Parameter
                      or else Ekind (Entity (Opnd)) =
                           E_Generic_In_Out_Parameter)
         then
            Opnd_Type := Get_Actual_Subtype (Opnd);

         --  If assignment operand is a component reference, then we get the
         --  actual subtype of the component for the unconstrained case.

         elsif Nkind (Opnd) = N_Selected_Component
           or else Nkind (Opnd) = N_Explicit_Dereference
         then
            Decl := Build_Actual_Subtype_Of_Component (Opnd_Type, Opnd);

            if Present (Decl) then
               Insert_Action (N, Decl);
               Mark_Rewrite_Insertion (Decl);
               Analyze (Decl);
               Opnd_Type := Defining_Identifier (Decl);
               Set_Etype (Opnd, Opnd_Type);
               Freeze_Itype (Opnd_Type, N);

            elsif Is_Constrained (Etype (Opnd)) then
               Opnd_Type := Etype (Opnd);
            end if;

         --  For slice, use the constrained subtype created for the slice

         elsif Nkind (Opnd) = N_Slice then
            Opnd_Type := Etype (Opnd);
         end if;
      end Set_Assignment_Type;

   --  Start of processing for Analyze_Assignment

   begin
      Analyze (Rhs);
      Analyze (Lhs);
      T1 := Etype (Lhs);

      --  In the most general case, both Lhs and Rhs can be overloaded, and we
      --  must compute the intersection of the possible types on each side.

      if Is_Overloaded (Lhs) then
         declare
            I  : Interp_Index;
            It : Interp;

         begin
            T1 := Any_Type;
            Get_First_Interp (Lhs, I, It);

            while Present (It.Typ) loop
               if Has_Compatible_Type (Rhs, It.Typ) then

                  if T1 /= Any_Type then

                     --  An explicit dereference is overloaded if the prefix
                     --  is. Try to remove the ambiguity on the prefix, the
                     --  error will be posted there if the ambiguity is real.

                     if Nkind (Lhs) = N_Explicit_Dereference then
                        declare
                           PI    : Interp_Index;
                           PI1   : Interp_Index := 0;
                           PIt   : Interp;
                           Found : Boolean;

                        begin
                           Found := False;
                           Get_First_Interp (Prefix (Lhs), PI, PIt);

                           while Present (PIt.Typ) loop
                              if Has_Compatible_Type (Rhs,
                                Designated_Type (PIt.Typ))
                              then
                                 if Found then
                                    PIt :=
                                      Disambiguate (Prefix (Lhs),
                                        PI1, PI, Any_Type);

                                    if PIt = No_Interp then
                                       return;
                                    else
                                       Resolve (Prefix (Lhs), PIt.Typ);
                                    end if;

                                    exit;
                                 else
                                    Found := True;
                                    PI1 := PI;
                                 end if;
                              end if;

                              Get_Next_Interp (PI, PIt);
                           end loop;
                        end;

                     else
                        Error_Msg_N
                          ("ambiguous left-hand side in assignment", Lhs);
                        exit;
                     end if;
                  else
                     T1 := It.Typ;
                  end if;
               end if;

               Get_Next_Interp (I, It);
            end loop;
         end;

         if T1 = Any_Type then
            Error_Msg_N
              ("no valid types for left-hand side for assignment", Lhs);
            return;
         end if;
      end if;

      Resolve (Lhs, T1);

      if not Is_Variable (Lhs) then
         Diagnose_Non_Variable_Lhs (Lhs);
         return;

      elsif Is_Limited_Type (T1)
        and then not Assignment_OK (Lhs)
        and then not Assignment_OK (Original_Node (Lhs))
      then
         Error_Msg_N
           ("left hand of assignment must not be limited type", Lhs);
         return;
      end if;

      --  Resolution may have updated the subtype, in case the left-hand
      --  side is a private protected component. Use the correct subtype
      --  to avoid scoping issues in the back-end.

      T1 := Etype (Lhs);
      Set_Assignment_Type (Lhs, T1);

      Resolve (Rhs, T1);

      --  Remaining steps are skipped if Rhs was synatactically in error

      if Rhs = Error then
         return;
      end if;

      T2 := Etype (Rhs);
      Check_Unset_Reference (Rhs);
      Note_Possible_Modification (Lhs);

      if Covers (T1, T2) then
         null;
      else
         Wrong_Type (Rhs, Etype (Lhs));
         return;
      end if;

      Set_Assignment_Type (Rhs, T2);

      if T1 = Any_Type or else T2 = Any_Type then
         return;
      end if;

      if (Is_Class_Wide_Type (T2) or else Is_Dynamically_Tagged (Rhs))
        and then not Is_Class_Wide_Type (T1)
      then
         Error_Msg_N ("dynamically tagged expression not allowed!", Rhs);

      elsif Is_Class_Wide_Type (T1)
        and then not Is_Class_Wide_Type (T2)
        and then not Is_Tag_Indeterminate (Rhs)
        and then not Is_Dynamically_Tagged (Rhs)
      then
         Error_Msg_N ("dynamically tagged expression required!", Rhs);
      end if;

      --  Tag propagation is done only in semantics mode only. If expansion
      --  is on, the rhs tag indeterminate function call has been expanded
      --  and tag propagation would have happened too late, so the
      --  propagation take place in expand_call instead.

      if not Expander_Active
        and then Is_Class_Wide_Type (T1)
        and then Is_Tag_Indeterminate (Rhs)
      then
         Propagate_Tag (Lhs, Rhs);
      end if;

      if Is_Scalar_Type (T1) then
         Apply_Scalar_Range_Check (Rhs, Etype (Lhs));

      elsif Is_Array_Type (T1) then

         --  Assignment verifies that the length of the Lsh and Rhs are equal,
         --  but of course the indices do not have to match.

         Apply_Length_Check (Rhs, Etype (Lhs));

      else
         --  Discriminant checks are applied in the course of expansion.
         null;
      end if;

      --  ??? a real accessibility check is needed when ???

      --  Post warning for useless assignment

      if Warn_On_Redundant_Constructs

         --  We only warn for source constructs

         and then Comes_From_Source (N)

         --  Where the entity is the same on both sides

         and then Is_Entity_Name (Lhs)
         and then Is_Entity_Name (Rhs)
         and then Entity (Lhs) = Entity (Rhs)

         --  But exclude the case where the right side was an operation
         --  that got rewritten (e.g. JUNK + K, where K was known to be
         --  zero). We don't want to warn in such a case, since it is
         --  reasonable to write such expressions especially when K is
         --  defined symbolically in some other package.

        and then Nkind (Original_Node (Rhs)) not in N_Op
      then
         Error_Msg_NE
           ("?useless assignment of & to itself", N, Entity (Lhs));
      end if;
   end Analyze_Assignment;

   -----------------------------
   -- Analyze_Block_Statement --
   -----------------------------

   procedure Analyze_Block_Statement (N : Node_Id) is
      Decls : constant List_Id := Declarations (N);
      Id    : constant Node_Id := Identifier (N);
      Ent   : Entity_Id;

   begin
      --  If a label is present analyze it and mark it as referenced

      if Present (Id) then
         Analyze (Id);
         Ent := Entity (Id);
         Set_Ekind (Ent, E_Block);
         Generate_Reference (Ent, N, ' ');
         Generate_Definition (Ent);

         if Nkind (Parent (Ent)) = N_Implicit_Label_Declaration then
            Set_Label_Construct (Parent (Ent), N);
         end if;

      --  Otherwise create a label entity

      else
         Ent := New_Internal_Entity (E_Block, Current_Scope, Sloc (N), 'B');
         Set_Identifier (N, New_Occurrence_Of (Ent, Sloc (N)));
      end if;

      Set_Etype (Ent, Standard_Void_Type);
      Set_Block_Node (Ent, Identifier (N));
      New_Scope (Ent);

      if Present (Decls) then
         Analyze_Declarations (Decls);
         Check_Completion;
      end if;

      Analyze (Handled_Statement_Sequence (N));
      Process_End_Label (Handled_Statement_Sequence (N), 'e', Ent);

      --  Analyze exception handlers if present. Note that the test for
      --  HSS being present is an error defence against previous errors.

      if Present (Handled_Statement_Sequence (N))
        and then Present (Exception_Handlers (Handled_Statement_Sequence (N)))
      then
         declare
            S : Entity_Id := Scope (Ent);

         begin
            --  Indicate that enclosing scopes contain a block with handlers.
            --  Only non-generic scopes need to be marked.

            loop
               Set_Has_Nested_Block_With_Handler (S);
               exit when Is_Overloadable (S)
                 or else Ekind (S) = E_Package
                 or else Ekind (S) = E_Generic_Function
                 or else Ekind (S) = E_Generic_Package
                 or else Ekind (S) = E_Generic_Procedure;
               S := Scope (S);
            end loop;
         end;
      end if;

      Check_References (Ent);
      End_Scope;
   end Analyze_Block_Statement;

   ----------------------------
   -- Analyze_Case_Statement --
   ----------------------------

   procedure Analyze_Case_Statement (N : Node_Id) is

      Statements_Analyzed : Boolean := False;
      --  Set True if at least some statement sequences get analyzed.
      --  If False on exit, means we had a serious error that prevented
      --  full analysis of the case statement, and as a result it is not
      --  a good idea to output warning messages about unreachable code.

      Save_Unblocked_Exit_Count : constant Nat := Unblocked_Exit_Count;
      --  Recursively save value of this global, will be restored on exit

      procedure Non_Static_Choice_Error (Choice : Node_Id);
      --  Error routine invoked by the generic instantiation below when
      --  the case statement has a non static choice.

      procedure Process_Statements (Alternative : Node_Id);
      --  Analyzes all the statements associated to a case alternative.
      --  Needed by the generic instantiation below.

      package Case_Choices_Processing is new
        Generic_Choices_Processing
          (Get_Alternatives          => Alternatives,
           Get_Choices               => Discrete_Choices,
           Process_Empty_Choice      => No_OP,
           Process_Non_Static_Choice => Non_Static_Choice_Error,
           Process_Associated_Node   => Process_Statements);
      use Case_Choices_Processing;
      --  Instantiation of the generic choice processing package.

      -----------------------------
      -- Non_Static_Choice_Error --
      -----------------------------

      procedure Non_Static_Choice_Error (Choice : Node_Id) is
      begin
         Error_Msg_N ("choice given in case statement is not static", Choice);
      end Non_Static_Choice_Error;

      ------------------------
      -- Process_Statements --
      ------------------------

      procedure Process_Statements (Alternative : Node_Id) is
      begin
         Unblocked_Exit_Count := Unblocked_Exit_Count + 1;
         Statements_Analyzed := True;
         Analyze_Statements (Statements (Alternative));
      end Process_Statements;

      --  Variables local to Analyze_Case_Statement.

      Exp       : Node_Id;
      Exp_Type  : Entity_Id;
      Exp_Btype : Entity_Id;

      Case_Table     : Choice_Table_Type (1 .. Number_Of_Choices (N));
      Last_Choice    : Nat;
      Dont_Care      : Boolean;
      Others_Present : Boolean;

   --  Start of processing for Analyze_Case_Statement

   begin
      Unblocked_Exit_Count := 0;
      Exp := Expression (N);
      Analyze_And_Resolve (Exp, Any_Discrete);
      Check_Unset_Reference (Exp);
      Exp_Type  := Etype (Exp);
      Exp_Btype := Base_Type (Exp_Type);

      --  The expression must be of a discrete type which must be determinable
      --  independently of the context in which the expression occurs, but
      --  using the fact that the expression must be of a discrete type.
      --  Moreover, the type this expression must not be a character literal
      --  (which is always ambiguous) or, for Ada-83, a generic formal type.

      --  If error already reported by Resolve, nothing more to do

      if Exp_Btype = Any_Discrete
        or else Exp_Btype = Any_Type
      then
         return;

      elsif Exp_Btype = Any_Character then
         Error_Msg_N
           ("character literal as case expression is ambiguous", Exp);
         return;

      elsif Ada_83
        and then (Is_Generic_Type (Exp_Btype)
                    or else Is_Generic_Type (Root_Type (Exp_Btype)))
      then
         Error_Msg_N
           ("(Ada 83) case expression cannot be of a generic type", Exp);
         return;
      end if;

      --  If the case expression is a formal object of mode in out,
      --  then treat it as having a nonstatic subtype by forcing
      --  use of the base type (which has to get passed to
      --  Check_Case_Choices below).  Also use base type when
      --  the case expression is parenthesized.

      if Paren_Count (Exp) > 0
        or else (Is_Entity_Name (Exp)
                  and then Ekind (Entity (Exp)) = E_Generic_In_Out_Parameter)
      then
         Exp_Type := Exp_Btype;
      end if;

      --  Call the instantiated Analyze_Choices which does the rest of the work

      Analyze_Choices
        (N, Exp_Type, Case_Table, Last_Choice, Dont_Care, Others_Present);

      if Exp_Type = Universal_Integer and then not Others_Present then
         Error_Msg_N ("case on universal integer requires OTHERS choice", Exp);
      end if;

      --  If all our exits were blocked by unconditional transfers of control,
      --  then the entire CASE statement acts as an unconditional transfer of
      --  control, so treat it like one, and check unreachable code. Skip this
      --  test if we had serious errors preventing any statement analysis.

      if Unblocked_Exit_Count = 0 and then Statements_Analyzed then
         Unblocked_Exit_Count := Save_Unblocked_Exit_Count;
         Check_Unreachable_Code (N);
      else
         Unblocked_Exit_Count := Save_Unblocked_Exit_Count;
      end if;
   end Analyze_Case_Statement;

   ----------------------------
   -- Analyze_Exit_Statement --
   ----------------------------

   --  If the exit includes a name, it must be the name of a currently open
   --  loop. Otherwise there must be an innermost open loop on the stack,
   --  to which the statement implicitly refers.

   procedure Analyze_Exit_Statement (N : Node_Id) is
      Target   : constant Node_Id := Name (N);
      Cond     : constant Node_Id := Condition (N);
      Scope_Id : Entity_Id;
      U_Name   : Entity_Id;
      Kind     : Entity_Kind;

   begin
      if No (Cond) then
         Check_Unreachable_Code (N);
      end if;

      if Present (Target) then
         Analyze (Target);
         U_Name := Entity (Target);

         if not In_Open_Scopes (U_Name) or else Ekind (U_Name) /= E_Loop then
            Error_Msg_N ("invalid loop name in exit statement", N);
            return;
         else
            Set_Has_Exit (U_Name);
         end if;

      else
         U_Name := Empty;
      end if;

      for J in reverse 0 .. Scope_Stack.Last loop
         Scope_Id := Scope_Stack.Table (J).Entity;
         Kind := Ekind (Scope_Id);

         if Kind = E_Loop
           and then (No (Target) or else Scope_Id = U_Name) then
            Set_Has_Exit (Scope_Id);
            exit;

         elsif Kind = E_Block or else Kind = E_Loop then
            null;

         else
            Error_Msg_N
              ("cannot exit from program unit or accept statement", N);
            exit;
         end if;
      end loop;

      --  Verify that if present the condition is a Boolean expression.

      if Present (Cond) then
         Analyze_And_Resolve (Cond, Any_Boolean);
         Check_Unset_Reference (Cond);
      end if;
   end Analyze_Exit_Statement;

   ----------------------------
   -- Analyze_Goto_Statement --
   ----------------------------

   procedure Analyze_Goto_Statement (N : Node_Id) is
      Label       : constant Node_Id := Name (N);
      Scope_Id    : Entity_Id;
      Label_Scope : Entity_Id;

   begin
      Check_Unreachable_Code (N);

      Analyze (Label);

      if Entity (Label) = Any_Id then
         return;

      elsif Ekind (Entity (Label)) /= E_Label then
         Error_Msg_N ("target of goto statement must be a label", Label);
         return;

      elsif not Reachable (Entity (Label)) then
         Error_Msg_N ("target of goto statement is not reachable", Label);
         return;
      end if;

      Label_Scope := Enclosing_Scope (Entity (Label));

      for J in reverse 0 .. Scope_Stack.Last loop
         Scope_Id := Scope_Stack.Table (J).Entity;

         if Label_Scope = Scope_Id
           or else (Ekind (Scope_Id) /= E_Block
                     and then Ekind (Scope_Id) /= E_Loop)
         then
            if Scope_Id /= Label_Scope then
               Error_Msg_N
                 ("cannot exit from program unit or accept statement", N);
            end if;

            return;
         end if;
      end loop;

      raise Program_Error;

   end Analyze_Goto_Statement;

   --------------------------
   -- Analyze_If_Statement --
   --------------------------

   --  A special complication arises in the analysis of if statements.
   --  The expander has circuitry to completely deleted code that it
   --  can tell will not be executed (as a result of compile time known
   --  conditions). In the analyzer, we ensure that code that will be
   --  deleted in this manner is analyzed but not expanded. This is
   --  obviously more efficient, but more significantly, difficulties
   --  arise if code is expanded and then eliminated (e.g. exception
   --  table entries disappear).

   procedure Analyze_If_Statement (N : Node_Id) is
      E : Node_Id;

      Save_Unblocked_Exit_Count : constant Nat := Unblocked_Exit_Count;
      --  Recursively save value of this global, will be restored on exit

      Del : Boolean := False;
      --  This flag gets set True if a True condition has been found,
      --  which means that remaining ELSE/ELSIF parts are deleted.

      procedure Analyze_Cond_Then (Cnode : Node_Id);
      --  This is applied to either the N_If_Statement node itself or
      --  to an N_Elsif_Part node. It deals with analyzing the condition
      --  and the THEN statements associated with it.

      procedure Analyze_Cond_Then (Cnode : Node_Id) is
         Cond : constant Node_Id := Condition (Cnode);
         Tstm : constant List_Id := Then_Statements (Cnode);

      begin
         Unblocked_Exit_Count := Unblocked_Exit_Count + 1;
         Analyze_And_Resolve (Cond, Any_Boolean);
         Check_Unset_Reference (Cond);

         --  If already deleting, then just analyze then statements

         if Del then
            Analyze_Statements (Tstm);

         --  Compile time known value, not deleting yet

         elsif Compile_Time_Known_Value (Cond) then

            --  If condition is True, then analyze the THEN statements
            --  and set no expansion for ELSE and ELSIF parts.

            if Is_True (Expr_Value (Cond)) then
               Analyze_Statements (Tstm);
               Del := True;
               Expander_Mode_Save_And_Set (False);

            --  If condition is False, analyze THEN with expansion off

            else -- Is_False (Expr_Value (Cond))
               Expander_Mode_Save_And_Set (False);
               Analyze_Statements (Tstm);
               Expander_Mode_Restore;
            end if;

         --  Not known at compile time, not deleting, normal analysis

         else
            Analyze_Statements (Tstm);
         end if;
      end Analyze_Cond_Then;

   --  Start of Analyze_If_Statement

   begin
      --  Initialize exit count for else statements. If there is no else
      --  part, this count will stay non-zero reflecting the fact that the
      --  uncovered else case is an unblocked exit.

      Unblocked_Exit_Count := 1;
      Analyze_Cond_Then (N);

      --  Now to analyze the elsif parts if any are present

      if Present (Elsif_Parts (N)) then
         E := First (Elsif_Parts (N));
         while Present (E) loop
            Analyze_Cond_Then (E);
            Next (E);
         end loop;
      end if;

      if Present (Else_Statements (N)) then
         Analyze_Statements (Else_Statements (N));
      end if;

      --  If all our exits were blocked by unconditional transfers of control,
      --  then the entire IF statement acts as an unconditional transfer of
      --  control, so treat it like one, and check unreachable code.

      if Unblocked_Exit_Count = 0 then
         Unblocked_Exit_Count := Save_Unblocked_Exit_Count;
         Check_Unreachable_Code (N);
      else
         Unblocked_Exit_Count := Save_Unblocked_Exit_Count;
      end if;

      if Del then
         Expander_Mode_Restore;
      end if;

   end Analyze_If_Statement;

   ----------------------------------------
   -- Analyze_Implicit_Label_Declaration --
   ----------------------------------------

   --  An implicit label declaration is generated in the innermost
   --  enclosing declarative part. This is done for labels as well as
   --  block and loop names.

   --  Note: any changes in this routine may need to be reflected in
   --  Analyze_Label_Entity.

   procedure Analyze_Implicit_Label_Declaration (N : Node_Id) is
      Id : Node_Id := Defining_Identifier (N);

   begin
      Enter_Name (Id);
      Set_Ekind           (Id, E_Label);
      Set_Etype           (Id, Standard_Void_Type);
      Set_Enclosing_Scope (Id, Current_Scope);
   end Analyze_Implicit_Label_Declaration;

   ------------------------------
   -- Analyze_Iteration_Scheme --
   ------------------------------

   procedure Analyze_Iteration_Scheme (N : Node_Id) is
   begin
      --  For an infinite loop, there is no iteration scheme

      if No (N) then
         return;

      else
         declare
            Cond : constant Node_Id := Condition (N);

         begin
            --  For WHILE loop, verify that the condition is a Boolean
            --  expression and resolve and check it.

            if Present (Cond) then
               Analyze_And_Resolve (Cond, Any_Boolean);
               Check_Unset_Reference (Cond);

            --  Else we have a FOR loop

            else
               declare
                  LP : constant Node_Id   := Loop_Parameter_Specification (N);
                  Id : constant Entity_Id := Defining_Identifier (LP);
                  DS : constant Node_Id   := Discrete_Subtype_Definition (LP);
                  F  : List_Id;

               begin
                  Enter_Name (Id);

                  --  We always consider the loop variable to be referenced,
                  --  since the loop may be used just for counting purposes.

                  Generate_Reference (Id, N, ' ');

                  --  Check for case of loop variable hiding a local
                  --  variable (used later on to give a nice warning
                  --  if the hidden variable is never assigned).

                  declare
                     H : constant Entity_Id := Homonym (Id);

                  begin
                     if Present (H)
                       and then Enclosing_Dynamic_Scope (H) =
                                Enclosing_Dynamic_Scope (Id)
                       and then Ekind (H) = E_Variable
                       and then Is_Discrete_Type (Etype (H))
                     then
                        Set_Hiding_Loop_Variable (H, Id);
                     end if;
                  end;

                  --  Now analyze the subtype definition

                  Analyze (DS);

                  if DS = Error then
                     return;
                  end if;

                  --  The subtype indication may denote the completion
                  --  of an incomplete type declaration.

                  if Is_Entity_Name (DS)
                    and then Present (Entity (DS))
                    and then Is_Type (Entity (DS))
                    and then Ekind (Entity (DS)) = E_Incomplete_Type
                  then
                     Set_Entity (DS, Get_Full_View (Entity (DS)));
                     Set_Etype  (DS, Entity (DS));
                  end if;

                  if not Is_Discrete_Type (Etype (DS)) then
                     Wrong_Type (DS, Any_Discrete);
                     Set_Etype (DS, Any_Type);
                  end if;

                  Make_Index (DS, LP);

                  Set_Ekind          (Id, E_Loop_Parameter);
                  Set_Etype          (Id, Etype (DS));
                  Set_Is_Known_Valid (Id, True);

                  --  The loop is not a declarative part, so the only entity
                  --  declared "within" must be frozen explicitly. Since the
                  --  type of this entity has already been frozen, this cannot
                  --  generate any freezing actions.

                  F := Freeze_Entity (Id, Sloc (LP));
                  pragma Assert (F = No_List);

                  --  Check for null or possibly null range and issue warning.
                  --  We suppress such messages in generic templates and
                  --  instances, because in practice they tend to be dubious
                  --  in these cases.

                  if Nkind (DS) = N_Range
                    and then Comes_From_Source (N)
                    and then not Inside_A_Generic
                    and then not In_Instance
                  then
                     declare
                        L : constant Node_Id := Low_Bound  (DS);
                        H : constant Node_Id := High_Bound (DS);

                        Llo : Uint;
                        Lhi : Uint;
                        LOK : Boolean;
                        Hlo : Uint;
                        Hhi : Uint;
                        HOK : Boolean;

                     begin
                        Determine_Range (L, LOK, Llo, Lhi);
                        Determine_Range (H, HOK, Hlo, Hhi);

                        --  If range of loop is null, issue warning

                        if (LOK and HOK) and then Llo > Hhi then
                           Error_Msg_N
                             ("?loop range is null, loop will not execute",
                              DS);

                        --  The other case for a warning is a reverse loop
                        --  where the upper bound is the integer literal
                        --  zero or one, and the lower bound can be positive.

                        elsif Reverse_Present (LP)
                          and then Nkind (H) = N_Integer_Literal
                          and then (Intval (H) = Uint_0
                                      or else
                                    Intval (H) = Uint_1)
                          and then Lhi > Hhi
                        then
                           Warn_On_Instance := True;
                           Error_Msg_N ("?loop range may be null", DS);
                           Warn_On_Instance := False;
                        end if;
                     end;
                  end if;
               end;
            end if;
         end;
      end if;
   end Analyze_Iteration_Scheme;

   -------------------
   -- Analyze_Label --
   -------------------

   --  Important note: normally this routine is called from Analyze_Statements
   --  which does a prescan, to make sure that the Reachable flags are set on
   --  all labels before encountering a possible goto to one of these labels.
   --  If expanded code analyzes labels via the normal Sem path, then it must
   --  ensure that Reachable is set early enough to avoid problems in the case
   --  of a forward goto.

   procedure Analyze_Label (N : Node_Id) is
      Lab : Entity_Id;

   begin
      Analyze (Identifier (N));
      Lab := Entity (Identifier (N));

      --  If we found a label mark it as reachable.

      if Ekind (Lab) = E_Label then
         Generate_Definition (Lab);
         Set_Reachable (Lab);

         if Nkind (Parent (Lab)) = N_Implicit_Label_Declaration then
            Set_Label_Construct (Parent (Lab), N);
         end if;

      --  If we failed to find a label, it means the implicit declaration
      --  of the label was hidden.  A for-loop parameter can do this to a
      --  label with the same name inside the loop, since the implicit label
      --  declaration is in the innermost enclosing body or block statement.

      else
         Error_Msg_Sloc := Sloc (Lab);
         Error_Msg_N
           ("implicit label declaration for & is hidden#",
            Identifier (N));
      end if;
   end Analyze_Label;

   --------------------------
   -- Analyze_Label_Entity --
   --------------------------

   procedure Analyze_Label_Entity (E : Entity_Id) is
   begin
      Set_Ekind           (E, E_Label);
      Set_Etype           (E, Standard_Void_Type);
      Set_Enclosing_Scope (E, Current_Scope);
      Set_Reachable       (E, True);
   end Analyze_Label_Entity;

   ----------------------------
   -- Analyze_Loop_Statement --
   ----------------------------

   procedure Analyze_Loop_Statement (N : Node_Id) is
      Id  : constant Node_Id := Identifier (N);
      Ent : Entity_Id;

   begin
      if Present (Id) then

         --  Make name visible, e.g. for use in exit statements. Loop
         --  labels are always considered to be referenced.

         Analyze (Id);
         Ent := Entity (Id);
         Generate_Reference  (Ent, N, ' ');
         Generate_Definition (Ent);

         --  If we found a label, mark its type. If not, ignore it, since it
         --  means we have a conflicting declaration, which would already have
         --  been diagnosed at declaration time. Set Label_Construct of the
         --  implicit label declaration, which is not created by the parser
         --  for generic units.

         if Ekind (Ent) = E_Label then
            Set_Ekind (Ent, E_Loop);

            if Nkind (Parent (Ent)) = N_Implicit_Label_Declaration then
               Set_Label_Construct (Parent (Ent), N);
            end if;
         end if;

      --  Case of no identifier present

      else
         Ent := New_Internal_Entity (E_Loop, Current_Scope, Sloc (N), 'L');
         Set_Etype (Ent,  Standard_Void_Type);
         Set_Parent (Ent, N);
      end if;

      New_Scope (Ent);
      Analyze_Iteration_Scheme (Iteration_Scheme (N));
      Analyze_Statements (Statements (N));
      Process_End_Label (N, 'e', Ent);
      End_Scope;
   end Analyze_Loop_Statement;

   ----------------------------
   -- Analyze_Null_Statement --
   ----------------------------

   --  Note: the semantics of the null statement is implemented by a single
   --  null statement, too bad everything isn't as simple as this!

   procedure Analyze_Null_Statement (N : Node_Id) is
      pragma Warnings (Off, N);

   begin
      null;
   end Analyze_Null_Statement;

   ------------------------
   -- Analyze_Statements --
   ------------------------

   procedure Analyze_Statements (L : List_Id) is
      S : Node_Id;

   begin
      --  The labels declared in the statement list are reachable from
      --  statements in the list. We do this as a prepass so that any
      --  goto statement will be properly flagged if its target is not
      --  reachable. This is not required, but is nice behavior!

      S := First (L);

      while Present (S) loop
         if Nkind (S) = N_Label then
            Analyze_Label (S);
         end if;

         Next (S);
      end loop;

      --  Perform semantic analysis on all statements

      S := First (L);

      while Present (S) loop

         if Nkind (S) /= N_Label then
            Analyze (S);
         end if;

         Next (S);
      end loop;

      --  Make labels unreachable. Visibility is not sufficient, because
      --  labels in one if-branch for example are not reachable from the
      --  other branch, even though their declarations are in the enclosing
      --  declarative part.

      S := First (L);

      while Present (S) loop
         if Nkind (S) = N_Label then
            Set_Reachable (Entity (Identifier (S)), False);
         end if;

         Next (S);
      end loop;
   end Analyze_Statements;

   ----------------------------
   -- Check_Unreachable_Code --
   ----------------------------

   procedure Check_Unreachable_Code (N : Node_Id) is
      Error_Loc : Source_Ptr;
      P         : Node_Id;

   begin
      if Is_List_Member (N)
        and then Comes_From_Source (N)
      then
         declare
            Nxt : Node_Id;

         begin
            Nxt := Original_Node (Next (N));

            if Present (Nxt)
              and then Comes_From_Source (Nxt)
              and then Is_Statement (Nxt)
            then
               --  Special very annoying exception. If we have a return that
               --  follows a raise, then we allow it without a warning, since
               --  the Ada RM annoyingly requires a useless return here!

               if Nkind (Original_Node (N)) /= N_Raise_Statement
                 or else Nkind (Nxt) /= N_Return_Statement
               then
                  --  The rather strange shenanigans with the warning message
                  --  here reflects the fact that Kill_Dead_Code is very good
                  --  at removing warnings in deleted code, and this is one
                  --  warning we would prefer NOT to have removed :-)

                  Error_Loc := Sloc (Nxt);

                  --  If we have unreachable code, analyze and remove the
                  --  unreachable code, since it is useless and we don't
                  --  want to generate junk warnings.

                  --  We skip this step if we are not in code generation mode.
                  --  This is the one case where we remove dead code in the
                  --  semantics as opposed to the expander, and we do not want
                  --  to remove code if we are not in code generation mode,
                  --  since this messes up the ASIS trees.

                  --  Note that one might react by moving the whole circuit to
                  --  exp_ch5, but then we lose the warning in -gnatc mode.

                  if Operating_Mode = Generate_Code then
                     loop
                        Nxt := Next (N);
                        exit when No (Nxt) or else not Is_Statement (Nxt);
                        Analyze (Nxt);
                        Remove (Nxt);
                        Kill_Dead_Code (Nxt);
                     end loop;
                  end if;

                  --  Now issue the warning

                  Error_Msg ("?unreachable code", Error_Loc);
               end if;

            --  If the unconditional transfer of control instruction is
            --  the last statement of a sequence, then see if our parent
            --  is an IF statement, and if so adjust the unblocked exit
            --  count of the if statement to reflect the fact that this
            --  branch of the if is indeed blocked by a transfer of control.

            else
               P := Parent (N);

               if Nkind (P) = N_If_Statement then
                  null;

               elsif Nkind (P) = N_Elsif_Part then
                  P := Parent (P);
                  pragma Assert (Nkind (P) = N_If_Statement);

               elsif Nkind (P) = N_Case_Statement_Alternative then
                  P := Parent (P);
                  pragma Assert (Nkind (P) = N_Case_Statement);

               else
                  return;
               end if;

               Unblocked_Exit_Count := Unblocked_Exit_Count - 1;
            end if;
         end;
      end if;
   end Check_Unreachable_Code;

end Sem_Ch5;
