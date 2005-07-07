------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 4                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Atree;    use Atree;
with Checks;   use Checks;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Exp_Util; use Exp_Util;
with Fname;    use Fname;
with Itypes;   use Itypes;
with Lib;      use Lib;
with Lib.Xref; use Lib.Xref;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Output;   use Output;
with Restrict; use Restrict;
with Rident;   use Rident;
with Sem;      use Sem;
with Sem_Cat;  use Sem_Cat;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Dist; use Sem_Dist;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sem_Type; use Sem_Type;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Tbuild;   use Tbuild;

with GNAT.Spelling_Checker; use GNAT.Spelling_Checker;

package body Sem_Ch4 is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Analyze_Expression (N : Node_Id);
   --  For expressions that are not names, this is just a call to analyze.
   --  If the expression is a name, it may be a call to a parameterless
   --  function, and if so must be converted into an explicit call node
   --  and analyzed as such. This deproceduring must be done during the first
   --  pass of overload resolution, because otherwise a procedure call with
   --  overloaded actuals may fail to resolve. See 4327-001 for an example.

   procedure Analyze_Operator_Call (N : Node_Id; Op_Id : Entity_Id);
   --  Analyze a call of the form "+"(x, y), etc. The prefix of the call
   --  is an operator name or an expanded name whose selector is an operator
   --  name, and one possible interpretation is as a predefined operator.

   procedure Analyze_Overloaded_Selected_Component (N : Node_Id);
   --  If the prefix of a selected_component is overloaded, the proper
   --  interpretation that yields a record type with the proper selector
   --  name must be selected.

   procedure Analyze_User_Defined_Binary_Op (N : Node_Id; Op_Id : Entity_Id);
   --  Procedure to analyze a user defined binary operator, which is resolved
   --  like a function, but instead of a list of actuals it is presented
   --  with the left and right operands of an operator node.

   procedure Analyze_User_Defined_Unary_Op (N : Node_Id; Op_Id : Entity_Id);
   --  Procedure to analyze a user defined unary operator, which is resolved
   --  like a function, but instead of a list of actuals, it is presented with
   --  the operand of the operator node.

   procedure Ambiguous_Operands (N : Node_Id);
   --  for equality, membership, and comparison operators with overloaded
   --  arguments, list possible interpretations.

   procedure Analyze_One_Call
      (N       : Node_Id;
       Nam     : Entity_Id;
       Report  : Boolean;
       Success : out Boolean);
   --  Check one interpretation of an overloaded subprogram name for
   --  compatibility with the types of the actuals in a call. If there is a
   --  single interpretation which does not match, post error if Report is
   --  set to True.
   --
   --  Nam is the entity that provides the formals against which the actuals
   --  are checked. Nam is either the name of a subprogram, or the internal
   --  subprogram type constructed for an access_to_subprogram. If the actuals
   --  are compatible with Nam, then Nam is added to the list of candidate
   --  interpretations for N, and Success is set to True.

   procedure Check_Misspelled_Selector
     (Prefix : Entity_Id;
      Sel    : Node_Id);
   --  Give possible misspelling diagnostic if Sel is likely to be
   --  a misspelling of one of the selectors of the Prefix.
   --  This is called by Analyze_Selected_Component after producing
   --  an invalid selector error message.

   function Defined_In_Scope (T : Entity_Id; S : Entity_Id) return Boolean;
   --  Verify that type T is declared in scope S. Used to find intepretations
   --  for operators given by expanded names. This is abstracted as a separate
   --  function to handle extensions to System, where S is System, but T is
   --  declared in the extension.

   procedure Find_Arithmetic_Types
     (L, R  : Node_Id;
      Op_Id : Entity_Id;
      N     : Node_Id);
   --  L and R are the operands of an arithmetic operator. Find
   --  consistent pairs of interpretations for L and R that have a
   --  numeric type consistent with the semantics of the operator.

   procedure Find_Comparison_Types
     (L, R  : Node_Id;
      Op_Id : Entity_Id;
      N     : Node_Id);
   --  L and R are operands of a comparison operator. Find consistent
   --  pairs of interpretations for L and R.

   procedure Find_Concatenation_Types
     (L, R  : Node_Id;
      Op_Id : Entity_Id;
      N     : Node_Id);
   --  For the four varieties of concatenation

   procedure Find_Equality_Types
     (L, R  : Node_Id;
      Op_Id : Entity_Id;
      N     : Node_Id);
   --  Ditto for equality operators

   procedure Find_Boolean_Types
     (L, R  : Node_Id;
      Op_Id : Entity_Id;
      N     : Node_Id);
   --  Ditto for binary logical operations

   procedure Find_Negation_Types
     (R     : Node_Id;
      Op_Id : Entity_Id;
      N     : Node_Id);
   --  Find consistent interpretation for operand of negation operator

   procedure Find_Non_Universal_Interpretations
     (N     : Node_Id;
      R     : Node_Id;
      Op_Id : Entity_Id;
      T1    : Entity_Id);
   --  For equality and comparison operators, the result is always boolean,
   --  and the legality of the operation is determined from the visibility
   --  of the operand types. If one of the operands has a universal interpre-
   --  tation,  the legality check uses some compatible non-universal
   --  interpretation of the other operand. N can be an operator node, or
   --  a function call whose name is an operator designator.

   procedure Find_Unary_Types
     (R     : Node_Id;
      Op_Id : Entity_Id;
      N     : Node_Id);
   --  Unary arithmetic types: plus, minus, abs

   procedure Check_Arithmetic_Pair
     (T1, T2 : Entity_Id;
      Op_Id  : Entity_Id;
      N      : Node_Id);
   --  Subsidiary procedure to Find_Arithmetic_Types. T1 and T2 are valid
   --  types for left and right operand. Determine whether they constitute
   --  a valid pair for the given operator, and record the corresponding
   --  interpretation of the operator node. The node N may be an operator
   --  node (the usual case) or a function call whose prefix is an operator
   --  designator. In  both cases Op_Id is the operator name itself.

   procedure Diagnose_Call (N : Node_Id; Nam : Node_Id);
   --  Give detailed information on overloaded call where none of the
   --  interpretations match. N is the call node, Nam the designator for
   --  the overloaded entity being called.

   function Junk_Operand (N : Node_Id) return Boolean;
   --  Test for an operand that is an inappropriate entity (e.g. a package
   --  name or a label). If so, issue an error message and return True. If
   --  the operand is not an inappropriate entity kind, return False.

   procedure Operator_Check (N : Node_Id);
   --  Verify that an operator has received some valid interpretation. If none
   --  was found, determine whether a use clause would make the operation
   --  legal. The variable Candidate_Type (defined in Sem_Type) is set for
   --  every type compatible with the operator, even if the operator for the
   --  type is not directly visible. The routine uses this type to emit a more
   --  informative message.

   procedure Process_Implicit_Dereference_Prefix
     (E : Entity_Id;
      P : Node_Id);
   --  Called when P is the prefix of an implicit dereference, denoting an
   --  object E. If in semantics only mode (-gnatc or generic), record that is
   --  a reference to E. Normally, such a reference is generated only when the
   --  implicit dereference is expanded into an explicit one. E may be empty,
   --  in which case this procedure does nothing.

   procedure Remove_Abstract_Operations (N : Node_Id);
   --  Ada 2005: implementation of AI-310. An abstract non-dispatching
   --  operation is not a candidate interpretation.

   function Try_Indexed_Call
     (N   : Node_Id;
      Nam : Entity_Id;
      Typ : Entity_Id) return Boolean;
   --  If a function has defaults for all its actuals, a call to it may
   --  in fact be an indexing on the result of the call. Try_Indexed_Call
   --  attempts the interpretation as an indexing, prior to analysis as
   --  a call. If both are possible,  the node is overloaded with both
   --  interpretations (same symbol but two different types).

   function Try_Indirect_Call
     (N   : Node_Id;
      Nam : Entity_Id;
      Typ : Entity_Id) return Boolean;
   --  Similarly, a function F that needs no actuals can return an access
   --  to a subprogram, and the call F (X)  interpreted as F.all (X). In
   --  this case the call may be overloaded with both interpretations.

   function Try_Object_Operation (N : Node_Id) return Boolean;
   --  Ada 2005 (AI-252): Give support to the object operation notation

   ------------------------
   -- Ambiguous_Operands --
   ------------------------

   procedure Ambiguous_Operands (N : Node_Id) is
      procedure List_Operand_Interps (Opnd : Node_Id);

      --------------------------
      -- List_Operand_Interps --
      --------------------------

      procedure List_Operand_Interps (Opnd : Node_Id) is
         Nam   : Node_Id;
         Err   : Node_Id := N;

      begin
         if Is_Overloaded (Opnd) then
            if Nkind (Opnd) in N_Op then
               Nam := Opnd;
            elsif Nkind (Opnd) = N_Function_Call then
               Nam := Name (Opnd);
            else
               return;
            end if;

         else
            return;
         end if;

         if Opnd = Left_Opnd (N) then
            Error_Msg_N
              ("\left operand has the following interpretations", N);
         else
            Error_Msg_N
              ("\right operand has the following interpretations", N);
            Err := Opnd;
         end if;

         List_Interps (Nam, Err);
      end List_Operand_Interps;

   --  Start of processing for Ambiguous_Operands

   begin
      if Nkind (N) = N_In
        or else Nkind (N) = N_Not_In
      then
         Error_Msg_N ("ambiguous operands for membership",  N);

      elsif Nkind (N) = N_Op_Eq
        or else Nkind (N) = N_Op_Ne
      then
         Error_Msg_N ("ambiguous operands for equality",  N);

      else
         Error_Msg_N ("ambiguous operands for comparison",  N);
      end if;

      if All_Errors_Mode then
         List_Operand_Interps (Left_Opnd  (N));
         List_Operand_Interps (Right_Opnd (N));
      else
         Error_Msg_N ("\use -gnatf switch for details", N);
      end if;
   end Ambiguous_Operands;

   -----------------------
   -- Analyze_Aggregate --
   -----------------------

   --  Most of the analysis of Aggregates requires that the type be known,
   --  and is therefore put off until resolution.

   procedure Analyze_Aggregate (N : Node_Id) is
   begin
      if No (Etype (N)) then
         Set_Etype (N, Any_Composite);
      end if;
   end Analyze_Aggregate;

   -----------------------
   -- Analyze_Allocator --
   -----------------------

   procedure Analyze_Allocator (N : Node_Id) is
      Loc      : constant Source_Ptr := Sloc (N);
      Sav_Errs : constant Nat        := Serious_Errors_Detected;
      E        : Node_Id            := Expression (N);
      Acc_Type : Entity_Id;
      Type_Id  : Entity_Id;

   begin
      Check_Restriction (No_Allocators, N);

      if Nkind (E) = N_Qualified_Expression then
         Acc_Type := Create_Itype (E_Allocator_Type, N);
         Set_Etype (Acc_Type, Acc_Type);
         Init_Size_Align (Acc_Type);
         Find_Type (Subtype_Mark (E));
         Type_Id := Entity (Subtype_Mark (E));
         Check_Fully_Declared (Type_Id, N);
         Set_Directly_Designated_Type (Acc_Type, Type_Id);

         if Is_Limited_Type (Type_Id)
           and then Comes_From_Source (N)
           and then not In_Instance_Body
         then
            --  Ada 2005 (AI-287): Do not post an error if the expression
            --  corresponds to a limited aggregate. Limited aggregates
            --  are checked in sem_aggr in a per-component manner
            --  (compare with handling of Get_Value subprogram).

            if Ada_Version >= Ada_05
              and then Nkind (Expression (E)) = N_Aggregate
            then
               null;
            else
               Error_Msg_N ("initialization not allowed for limited types", N);
               Explain_Limited_Type (Type_Id, N);
            end if;
         end if;

         Analyze_And_Resolve (Expression (E), Type_Id);

         --  A qualified expression requires an exact match of the type,
         --  class-wide matching is not allowed.

         if Is_Class_Wide_Type (Type_Id)
           and then Base_Type (Etype (Expression (E))) /= Base_Type (Type_Id)
         then
            Wrong_Type (Expression (E), Type_Id);
         end if;

         Check_Non_Static_Context (Expression (E));

         --  We don't analyze the qualified expression itself because it's
         --  part of the allocator

         Set_Etype  (E, Type_Id);

      --  Case where no qualified expression is present

      else
         declare
            Def_Id   : Entity_Id;
            Base_Typ : Entity_Id;

         begin
            --  If the allocator includes a N_Subtype_Indication then a
            --  constraint is present, otherwise the node is a subtype mark.
            --  Introduce an explicit subtype declaration into the tree
            --  defining some anonymous subtype and rewrite the allocator to
            --  use this subtype rather than the subtype indication.

            --  It is important to introduce the explicit subtype declaration
            --  so that the bounds of the subtype indication are attached to
            --  the tree in case the allocator is inside a generic unit.

            if Nkind (E) = N_Subtype_Indication then

               --  A constraint is only allowed for a composite type in Ada
               --  95. In Ada 83, a constraint is also allowed for an
               --  access-to-composite type, but the constraint is ignored.

               Find_Type (Subtype_Mark (E));
               Base_Typ := Entity (Subtype_Mark (E));

               if Is_Elementary_Type (Base_Typ) then
                  if not (Ada_Version = Ada_83
                           and then Is_Access_Type (Base_Typ))
                  then
                     Error_Msg_N ("constraint not allowed here", E);

                     if Nkind (Constraint (E))
                       = N_Index_Or_Discriminant_Constraint
                     then
                        Error_Msg_N
                          ("\if qualified expression was meant, " &
                              "use apostrophe", Constraint (E));
                     end if;
                  end if;

                  --  Get rid of the bogus constraint:

                  Rewrite (E, New_Copy_Tree (Subtype_Mark (E)));
                  Analyze_Allocator (N);
                  return;

               --  Ada 2005, AI-363: if the designated type has a constrained
               --  partial view, it cannot receive a discriminant constraint,
               --  and the allocated object is unconstrained.

               elsif Ada_Version >= Ada_05
                 and then Has_Constrained_Partial_View (Base_Typ)
               then
                  Error_Msg_N
                    ("constraint no allowed when type " &
                      "has a constrained partial view", Constraint (E));
               end if;

               if Expander_Active then
                  Def_Id :=
                    Make_Defining_Identifier (Loc, New_Internal_Name ('S'));

                  Insert_Action (E,
                    Make_Subtype_Declaration (Loc,
                      Defining_Identifier => Def_Id,
                      Subtype_Indication  => Relocate_Node (E)));

                  if Sav_Errs /= Serious_Errors_Detected
                    and then Nkind (Constraint (E))
                      = N_Index_Or_Discriminant_Constraint
                  then
                     Error_Msg_N
                       ("if qualified expression was meant, " &
                           "use apostrophe!", Constraint (E));
                  end if;

                  E := New_Occurrence_Of (Def_Id, Loc);
                  Rewrite (Expression (N), E);
               end if;
            end if;

            Type_Id := Process_Subtype (E, N);
            Acc_Type := Create_Itype (E_Allocator_Type, N);
            Set_Etype                    (Acc_Type, Acc_Type);
            Init_Size_Align              (Acc_Type);
            Set_Directly_Designated_Type (Acc_Type, Type_Id);
            Check_Fully_Declared (Type_Id, N);

            --  Ada 2005 (AI-231)

            if Can_Never_Be_Null (Type_Id) then
               Error_Msg_N ("(Ada 2005) qualified expression required",
                            Expression (N));
            end if;

            --  Check restriction against dynamically allocated protected
            --  objects. Note that when limited aggregates are supported,
            --  a similar test should be applied to an allocator with a
            --  qualified expression ???

            if Is_Protected_Type (Type_Id) then
               Check_Restriction (No_Protected_Type_Allocators, N);
            end if;

            --  Check for missing initialization. Skip this check if we already
            --  had errors on analyzing the allocator, since in that case these
            --  are probably cascaded errors

            if Is_Indefinite_Subtype (Type_Id)
              and then Serious_Errors_Detected = Sav_Errs
            then
               if Is_Class_Wide_Type (Type_Id) then
                  Error_Msg_N
                    ("initialization required in class-wide allocation", N);
               else
                  Error_Msg_N
                    ("initialization required in unconstrained allocation", N);
               end if;
            end if;
         end;
      end if;

      if Is_Abstract (Type_Id) then
         Error_Msg_N ("cannot allocate abstract object", E);
      end if;

      if Has_Task (Designated_Type (Acc_Type)) then
         Check_Restriction (No_Tasking, N);
         Check_Restriction (Max_Tasks, N);
         Check_Restriction (No_Task_Allocators, N);
      end if;

      --  If the No_Streams restriction is set, check that the type of the
      --  object is not, and does not contain, any subtype derived from
      --  Ada.Streams.Root_Stream_Type. Note that we guard the call to
      --  Has_Stream just for efficiency reasons. There is no point in
      --  spending time on a Has_Stream check if the restriction is not set.

      if Restrictions.Set (No_Streams) then
         if Has_Stream (Designated_Type (Acc_Type)) then
            Check_Restriction (No_Streams, N);
         end if;
      end if;

      Set_Etype (N, Acc_Type);

      if not Is_Library_Level_Entity (Acc_Type) then
         Check_Restriction (No_Local_Allocators, N);
      end if;

      --  Ada 2005 (AI-231): Static checks

      if Ada_Version >= Ada_05
        and then (Null_Exclusion_Present (N)
                    or else Can_Never_Be_Null (Etype (N)))
      then
         Null_Exclusion_Static_Checks (N);
      end if;

      if Serious_Errors_Detected > Sav_Errs then
         Set_Error_Posted (N);
         Set_Etype (N, Any_Type);
      end if;
   end Analyze_Allocator;

   ---------------------------
   -- Analyze_Arithmetic_Op --
   ---------------------------

   procedure Analyze_Arithmetic_Op (N : Node_Id) is
      L     : constant Node_Id := Left_Opnd (N);
      R     : constant Node_Id := Right_Opnd (N);
      Op_Id : Entity_Id;

   begin
      Candidate_Type := Empty;
      Analyze_Expression (L);
      Analyze_Expression (R);

      --  If the entity is already set, the node is the instantiation of
      --  a generic node with a non-local reference, or was manufactured
      --  by a call to Make_Op_xxx. In either case the entity is known to
      --  be valid, and we do not need to collect interpretations, instead
      --  we just get the single possible interpretation.

      Op_Id := Entity (N);

      if Present (Op_Id) then
         if Ekind (Op_Id) = E_Operator then

            if (Nkind (N) = N_Op_Divide   or else
                Nkind (N) = N_Op_Mod      or else
                Nkind (N) = N_Op_Multiply or else
                Nkind (N) = N_Op_Rem)
              and then Treat_Fixed_As_Integer (N)
            then
               null;
            else
               Set_Etype (N, Any_Type);
               Find_Arithmetic_Types (L, R, Op_Id, N);
            end if;

         else
            Set_Etype (N, Any_Type);
            Add_One_Interp (N, Op_Id, Etype (Op_Id));
         end if;

      --  Entity is not already set, so we do need to collect interpretations

      else
         Op_Id := Get_Name_Entity_Id (Chars (N));
         Set_Etype (N, Any_Type);

         while Present (Op_Id) loop
            if Ekind (Op_Id) = E_Operator
              and then Present (Next_Entity (First_Entity (Op_Id)))
            then
               Find_Arithmetic_Types (L, R, Op_Id, N);

            --  The following may seem superfluous, because an operator cannot
            --  be generic, but this ignores the cleverness of the author of
            --  ACVC bc1013a.

            elsif Is_Overloadable (Op_Id) then
               Analyze_User_Defined_Binary_Op (N, Op_Id);
            end if;

            Op_Id := Homonym (Op_Id);
         end loop;
      end if;

      Operator_Check (N);
   end Analyze_Arithmetic_Op;

   ------------------
   -- Analyze_Call --
   ------------------

   --  Function, procedure, and entry calls are checked here. The Name in
   --  the call may be overloaded. The actuals have been analyzed and may
   --  themselves be overloaded. On exit from this procedure, the node N
   --  may have zero, one or more interpretations. In the first case an
   --  error message is produced. In the last case, the node is flagged
   --  as overloaded and the interpretations are collected in All_Interp.

   --  If the name is an Access_To_Subprogram, it cannot be overloaded, but
   --  the type-checking is similar to that of other calls.

   procedure Analyze_Call (N : Node_Id) is
      Actuals : constant List_Id := Parameter_Associations (N);
      Nam     : Node_Id          := Name (N);
      X       : Interp_Index;
      It      : Interp;
      Nam_Ent : Entity_Id;
      Success : Boolean := False;

      function Name_Denotes_Function return Boolean;
      --  If the type of the name is an access to subprogram, this may be
      --  the type of a name, or the return type of the function being called.
      --  If the name is not an entity then it can denote a protected function.
      --  Until we distinguish Etype from Return_Type, we must use this
      --  routine to resolve the meaning of the name in the call.

      ---------------------------
      -- Name_Denotes_Function --
      ---------------------------

      function Name_Denotes_Function return Boolean is
      begin
         if Is_Entity_Name (Nam) then
            return Ekind (Entity (Nam)) = E_Function;

         elsif Nkind (Nam) = N_Selected_Component then
            return Ekind (Entity (Selector_Name (Nam))) = E_Function;

         else
            return False;
         end if;
      end Name_Denotes_Function;

   --  Start of processing for Analyze_Call

   begin
      --  Initialize the type of the result of the call to the error type,
      --  which will be reset if the type is successfully resolved.

      Set_Etype (N, Any_Type);

      if not Is_Overloaded (Nam) then

         --  Only one interpretation to check

         if Ekind (Etype (Nam)) = E_Subprogram_Type then
            Nam_Ent := Etype (Nam);

         --  If the prefix is an access_to_subprogram, this may be an indirect
         --  call. This is the case if the name in the call is not an entity
         --  name, or if it is a function name in the context of a procedure
         --  call. In this latter case, we have a call to a parameterless
         --  function that returns a pointer_to_procedure which is the entity
         --  being called.

         elsif Is_Access_Type (Etype (Nam))
           and then Ekind (Designated_Type (Etype (Nam))) = E_Subprogram_Type
           and then
             (not Name_Denotes_Function
                or else Nkind (N) = N_Procedure_Call_Statement)
         then
            Nam_Ent := Designated_Type (Etype (Nam));
            Insert_Explicit_Dereference (Nam);

         --  Selected component case. Simple entry or protected operation,
         --  where the entry name is given by the selector name.

         elsif Nkind (Nam) = N_Selected_Component then
            Nam_Ent := Entity (Selector_Name (Nam));

            if Ekind (Nam_Ent) /= E_Entry
              and then Ekind (Nam_Ent) /= E_Entry_Family
              and then Ekind (Nam_Ent) /= E_Function
              and then Ekind (Nam_Ent) /= E_Procedure
            then
               Error_Msg_N ("name in call is not a callable entity", Nam);
               Set_Etype (N, Any_Type);
               return;
            end if;

         --  If the name is an Indexed component, it can be a call to a member
         --  of an entry family. The prefix must be a selected component whose
         --  selector is the entry. Analyze_Procedure_Call normalizes several
         --  kinds of call into this form.

         elsif Nkind (Nam) = N_Indexed_Component then

            if Nkind (Prefix (Nam)) = N_Selected_Component then
               Nam_Ent := Entity (Selector_Name (Prefix (Nam)));
            else
               Error_Msg_N ("name in call is not a callable entity", Nam);
               Set_Etype (N, Any_Type);
               return;
            end if;

         elsif not Is_Entity_Name (Nam) then
            Error_Msg_N ("name in call is not a callable entity", Nam);
            Set_Etype (N, Any_Type);
            return;

         else
            Nam_Ent := Entity (Nam);

            --  If no interpretations, give error message

            if not Is_Overloadable (Nam_Ent) then
               declare
                  L : constant Boolean   := Is_List_Member (N);
                  K : constant Node_Kind := Nkind (Parent (N));

               begin
                  --  If the node is in a list whose parent is not an
                  --  expression then it must be an attempted procedure call.

                  if L and then K not in N_Subexpr then
                     if Ekind (Entity (Nam)) = E_Generic_Procedure then
                        Error_Msg_NE
                          ("must instantiate generic procedure& before call",
                           Nam, Entity (Nam));
                     else
                        Error_Msg_N
                          ("procedure or entry name expected", Nam);
                     end if;

                  --  Check for tasking cases where only an entry call will do

                  elsif not L
                    and then (K = N_Entry_Call_Alternative
                               or else K = N_Triggering_Alternative)
                  then
                     Error_Msg_N ("entry name expected", Nam);

                  --  Otherwise give general error message

                  else
                     Error_Msg_N ("invalid prefix in call", Nam);
                  end if;

                  return;
               end;
            end if;
         end if;

         Analyze_One_Call (N, Nam_Ent, True, Success);

      else
         --  An overloaded selected component must denote overloaded
         --  operations of a concurrent type. The interpretations are
         --  attached to the simple name of those operations.

         if Nkind (Nam) = N_Selected_Component then
            Nam := Selector_Name (Nam);
         end if;

         Get_First_Interp (Nam, X, It);

         while Present (It.Nam) loop
            Nam_Ent := It.Nam;

            --  Name may be call that returns an access to subprogram, or more
            --  generally an overloaded expression one of whose interpretations
            --  yields an access to subprogram. If the name is an entity, we
            --  do not dereference, because the node is a call that returns
            --  the access type: note difference between f(x), where the call
            --  may return an access subprogram type, and f(x)(y), where the
            --  type returned by the call to f is implicitly dereferenced to
            --  analyze the outer call.

            if Is_Access_Type (Nam_Ent) then
               Nam_Ent := Designated_Type (Nam_Ent);

            elsif Is_Access_Type (Etype (Nam_Ent))
              and then not Is_Entity_Name (Nam)
              and then Ekind (Designated_Type (Etype (Nam_Ent)))
                                                          = E_Subprogram_Type
            then
               Nam_Ent := Designated_Type (Etype (Nam_Ent));
            end if;

            Analyze_One_Call (N, Nam_Ent, False, Success);

            --  If the interpretation succeeds, mark the proper type of the
            --  prefix (any valid candidate will do). If not, remove the
            --  candidate interpretation. This only needs to be done for
            --  overloaded protected operations, for other entities disambi-
            --  guation is done directly in Resolve.

            if Success then
               Set_Etype (Nam, It.Typ);

            elsif Nkind (Name (N)) = N_Selected_Component
              or else Nkind (Name (N)) = N_Function_Call
            then
               Remove_Interp (X);
            end if;

            Get_Next_Interp (X, It);
         end loop;

         --  If the name is the result of a function call, it can only
         --  be a call to a function returning an access to subprogram.
         --  Insert explicit dereference.

         if Nkind (Nam) = N_Function_Call then
            Insert_Explicit_Dereference (Nam);
         end if;

         if Etype (N) = Any_Type then

            --  None of the interpretations is compatible with the actuals

            Diagnose_Call (N, Nam);

            --  Special checks for uninstantiated put routines

            if Nkind (N) = N_Procedure_Call_Statement
              and then Is_Entity_Name (Nam)
              and then Chars (Nam) = Name_Put
              and then List_Length (Actuals) = 1
            then
               declare
                  Arg : constant Node_Id := First (Actuals);
                  Typ : Entity_Id;

               begin
                  if Nkind (Arg) = N_Parameter_Association then
                     Typ := Etype (Explicit_Actual_Parameter (Arg));
                  else
                     Typ := Etype (Arg);
                  end if;

                  if Is_Signed_Integer_Type (Typ) then
                     Error_Msg_N
                       ("possible missing instantiation of " &
                          "'Text_'I'O.'Integer_'I'O!", Nam);

                  elsif Is_Modular_Integer_Type (Typ) then
                     Error_Msg_N
                       ("possible missing instantiation of " &
                          "'Text_'I'O.'Modular_'I'O!", Nam);

                  elsif Is_Floating_Point_Type (Typ) then
                     Error_Msg_N
                       ("possible missing instantiation of " &
                          "'Text_'I'O.'Float_'I'O!", Nam);

                  elsif Is_Ordinary_Fixed_Point_Type (Typ) then
                     Error_Msg_N
                       ("possible missing instantiation of " &
                          "'Text_'I'O.'Fixed_'I'O!", Nam);

                  elsif Is_Decimal_Fixed_Point_Type (Typ) then
                     Error_Msg_N
                       ("possible missing instantiation of " &
                          "'Text_'I'O.'Decimal_'I'O!", Nam);

                  elsif Is_Enumeration_Type (Typ) then
                     Error_Msg_N
                       ("possible missing instantiation of " &
                          "'Text_'I'O.'Enumeration_'I'O!", Nam);
                  end if;
               end;
            end if;

         elsif not Is_Overloaded (N)
           and then Is_Entity_Name (Nam)
         then
            --  Resolution yields a single interpretation. Verify that
            --  is has the proper capitalization.

            Set_Entity_With_Style_Check (Nam, Entity (Nam));
            Generate_Reference (Entity (Nam), Nam);

            Set_Etype (Nam, Etype (Entity (Nam)));
         else
            Remove_Abstract_Operations (N);
         end if;

         End_Interp_List;
      end if;
   end Analyze_Call;

   ---------------------------
   -- Analyze_Comparison_Op --
   ---------------------------

   procedure Analyze_Comparison_Op (N : Node_Id) is
      L     : constant Node_Id := Left_Opnd (N);
      R     : constant Node_Id := Right_Opnd (N);
      Op_Id : Entity_Id        := Entity (N);

   begin
      Set_Etype (N, Any_Type);
      Candidate_Type := Empty;

      Analyze_Expression (L);
      Analyze_Expression (R);

      if Present (Op_Id) then
         if Ekind (Op_Id) = E_Operator then
            Find_Comparison_Types (L, R, Op_Id, N);
         else
            Add_One_Interp (N, Op_Id, Etype (Op_Id));
         end if;

         if Is_Overloaded (L) then
            Set_Etype (L, Intersect_Types (L, R));
         end if;

      else
         Op_Id := Get_Name_Entity_Id (Chars (N));
         while Present (Op_Id) loop
            if Ekind (Op_Id) = E_Operator then
               Find_Comparison_Types (L, R, Op_Id, N);
            else
               Analyze_User_Defined_Binary_Op (N, Op_Id);
            end if;

            Op_Id := Homonym (Op_Id);
         end loop;
      end if;

      Operator_Check (N);
   end Analyze_Comparison_Op;

   ---------------------------
   -- Analyze_Concatenation --
   ---------------------------

   --  If the only one-dimensional array type in scope is String,
   --  this is the resulting type of the operation. Otherwise there
   --  will be a concatenation operation defined for each user-defined
   --  one-dimensional array.

   procedure Analyze_Concatenation (N : Node_Id) is
      L     : constant Node_Id := Left_Opnd (N);
      R     : constant Node_Id := Right_Opnd (N);
      Op_Id : Entity_Id        := Entity (N);
      LT    : Entity_Id;
      RT    : Entity_Id;

   begin
      Set_Etype (N, Any_Type);
      Candidate_Type := Empty;

      Analyze_Expression (L);
      Analyze_Expression (R);

      --  If the entity is present, the  node appears in an instance,
      --  and denotes a predefined concatenation operation. The resulting
      --  type is obtained from the arguments when possible. If the arguments
      --  are aggregates, the array type and the concatenation type must be
      --  visible.

      if Present (Op_Id) then
         if Ekind (Op_Id) = E_Operator then

            LT := Base_Type (Etype (L));
            RT := Base_Type (Etype (R));

            if Is_Array_Type (LT)
              and then (RT = LT or else RT = Base_Type (Component_Type (LT)))
            then
               Add_One_Interp (N, Op_Id, LT);

            elsif Is_Array_Type (RT)
              and then LT = Base_Type (Component_Type (RT))
            then
               Add_One_Interp (N, Op_Id, RT);

            --  If one operand is a string type or a user-defined array type,
            --  and the other is a literal, result is of the specific type.

            elsif
              (Root_Type (LT) = Standard_String
                 or else Scope (LT) /= Standard_Standard)
              and then Etype (R) = Any_String
            then
               Add_One_Interp (N, Op_Id, LT);

            elsif
              (Root_Type (RT) = Standard_String
                 or else Scope (RT) /= Standard_Standard)
              and then Etype (L) = Any_String
            then
               Add_One_Interp (N, Op_Id, RT);

            elsif not Is_Generic_Type (Etype (Op_Id)) then
               Add_One_Interp (N, Op_Id, Etype (Op_Id));

            else
               --  Type and its operations must be visible

               Set_Entity (N, Empty);
               Analyze_Concatenation (N);
            end if;

         else
            Add_One_Interp (N, Op_Id, Etype (Op_Id));
         end if;

      else
         Op_Id := Get_Name_Entity_Id (Name_Op_Concat);
         while Present (Op_Id) loop
            if Ekind (Op_Id) = E_Operator then

               --  Do not consider operators declared in dead code, they can
               --  not be part of the resolution.

               if Is_Eliminated (Op_Id) then
                  null;
               else
                  Find_Concatenation_Types (L, R, Op_Id, N);
               end if;

            else
               Analyze_User_Defined_Binary_Op (N, Op_Id);
            end if;

            Op_Id := Homonym (Op_Id);
         end loop;
      end if;

      Operator_Check (N);
   end Analyze_Concatenation;

   ------------------------------------
   -- Analyze_Conditional_Expression --
   ------------------------------------

   procedure Analyze_Conditional_Expression (N : Node_Id) is
      Condition : constant Node_Id := First (Expressions (N));
      Then_Expr : constant Node_Id := Next (Condition);
      Else_Expr : constant Node_Id := Next (Then_Expr);
   begin
      Analyze_Expression (Condition);
      Analyze_Expression (Then_Expr);
      Analyze_Expression (Else_Expr);
      Set_Etype (N, Etype (Then_Expr));
   end Analyze_Conditional_Expression;

   -------------------------
   -- Analyze_Equality_Op --
   -------------------------

   procedure Analyze_Equality_Op (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      L     : constant Node_Id := Left_Opnd (N);
      R     : constant Node_Id := Right_Opnd (N);
      Op_Id : Entity_Id;

   begin
      Set_Etype (N, Any_Type);
      Candidate_Type := Empty;

      Analyze_Expression (L);
      Analyze_Expression (R);

      --  If the entity is set, the node is a generic instance with a non-local
      --  reference to the predefined operator or to a user-defined function.
      --  It can also be an inequality that is expanded into the negation of a
      --  call to a user-defined equality operator.

      --  For the predefined case, the result is Boolean, regardless of the
      --  type of the  operands. The operands may even be limited, if they are
      --  generic actuals. If they are overloaded, label the left argument with
      --  the common type that must be present, or with the type of the formal
      --  of the user-defined function.

      if Present (Entity (N)) then
         Op_Id := Entity (N);

         if Ekind (Op_Id) = E_Operator then
            Add_One_Interp (N, Op_Id, Standard_Boolean);
         else
            Add_One_Interp (N, Op_Id, Etype (Op_Id));
         end if;

         if Is_Overloaded (L) then
            if Ekind (Op_Id) = E_Operator then
               Set_Etype (L, Intersect_Types (L, R));
            else
               Set_Etype (L, Etype (First_Formal (Op_Id)));
            end if;
         end if;

      else
         Op_Id := Get_Name_Entity_Id (Chars (N));
         while Present (Op_Id) loop
            if Ekind (Op_Id) = E_Operator then
               Find_Equality_Types (L, R, Op_Id, N);
            else
               Analyze_User_Defined_Binary_Op (N, Op_Id);
            end if;

            Op_Id := Homonym (Op_Id);
         end loop;
      end if;

      --  If there was no match, and the operator is inequality, this may
      --  be a case where inequality has not been made explicit, as for
      --  tagged types. Analyze the node as the negation of an equality
      --  operation. This cannot be done earlier, because before analysis
      --  we cannot rule out the presence of an explicit inequality.

      if Etype (N) = Any_Type
        and then Nkind (N) = N_Op_Ne
      then
         Op_Id := Get_Name_Entity_Id (Name_Op_Eq);

         while Present (Op_Id) loop

            if Ekind (Op_Id) = E_Operator then
               Find_Equality_Types (L, R, Op_Id, N);
            else
               Analyze_User_Defined_Binary_Op (N, Op_Id);
            end if;

            Op_Id := Homonym (Op_Id);
         end loop;

         if Etype (N) /= Any_Type then
            Op_Id := Entity (N);

            Rewrite (N,
              Make_Op_Not (Loc,
                Right_Opnd =>
                  Make_Op_Eq (Loc,
                    Left_Opnd =>  Relocate_Node (Left_Opnd (N)),
                    Right_Opnd => Relocate_Node (Right_Opnd (N)))));

            Set_Entity (Right_Opnd (N), Op_Id);
            Analyze (N);
         end if;
      end if;

      Operator_Check (N);
   end Analyze_Equality_Op;

   ----------------------------------
   -- Analyze_Explicit_Dereference --
   ----------------------------------

   procedure Analyze_Explicit_Dereference (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      P     : constant Node_Id := Prefix (N);
      T     : Entity_Id;
      I     : Interp_Index;
      It    : Interp;
      New_N : Node_Id;

      function Is_Function_Type return Boolean;
      --  Check whether node may be interpreted as an implicit function call

      ----------------------
      -- Is_Function_Type --
      ----------------------

      function Is_Function_Type return Boolean is
         I  : Interp_Index;
         It : Interp;

      begin
         if not Is_Overloaded (N) then
            return Ekind (Base_Type (Etype (N))) = E_Subprogram_Type
              and then Etype (Base_Type (Etype (N))) /= Standard_Void_Type;

         else
            Get_First_Interp (N, I, It);

            while Present (It.Nam) loop
               if Ekind (Base_Type (It.Typ)) /= E_Subprogram_Type
                 or else Etype (Base_Type (It.Typ)) = Standard_Void_Type
               then
                  return False;
               end if;

               Get_Next_Interp (I, It);
            end loop;

            return True;
         end if;
      end Is_Function_Type;

   --  Start of processing for Analyze_Explicit_Dereference

   begin
      Analyze (P);
      Set_Etype (N, Any_Type);

      --  Test for remote access to subprogram type, and if so return
      --  after rewriting the original tree.

      if Remote_AST_E_Dereference (P) then
         return;
      end if;

      --  Normal processing for other than remote access to subprogram type

      if not Is_Overloaded (P) then
         if Is_Access_Type (Etype (P)) then

            --  Set the Etype. We need to go thru Is_For_Access_Subtypes
            --  to avoid other problems caused by the Private_Subtype
            --  and it is safe to go to the Base_Type because this is the
            --  same as converting the access value to its Base_Type.

            declare
               DT : Entity_Id := Designated_Type (Etype (P));

            begin
               if Ekind (DT) = E_Private_Subtype
                 and then Is_For_Access_Subtype (DT)
               then
                  DT := Base_Type (DT);
               end if;

               Set_Etype (N, DT);
            end;

         elsif Etype (P) /= Any_Type then
            Error_Msg_N ("prefix of dereference must be an access type", N);
            return;
         end if;

      else
         Get_First_Interp (P, I, It);

         while Present (It.Nam) loop
            T := It.Typ;

            if Is_Access_Type (T) then
               Add_One_Interp (N, Designated_Type (T), Designated_Type (T));
            end if;

            Get_Next_Interp (I, It);
         end loop;

         --  Error if no interpretation of the prefix has an access type

         if Etype (N) = Any_Type then
            Error_Msg_N
              ("access type required in prefix of explicit dereference", P);
            Set_Etype (N, Any_Type);
            return;
         end if;
      end if;

      if Is_Function_Type
        and then Nkind (Parent (N)) /= N_Indexed_Component

        and then (Nkind (Parent (N)) /= N_Function_Call
                   or else N /= Name (Parent (N)))

        and then (Nkind (Parent (N)) /= N_Procedure_Call_Statement
                   or else N /= Name (Parent (N)))

        and then Nkind (Parent (N)) /= N_Subprogram_Renaming_Declaration
        and then (Nkind (Parent (N)) /= N_Attribute_Reference
                    or else
                      (Attribute_Name (Parent (N)) /= Name_Address
                        and then
                       Attribute_Name (Parent (N)) /= Name_Access))
      then
         --  Name is a function call with no actuals, in a context that
         --  requires deproceduring (including as an actual in an enclosing
         --  function or procedure call). There are some pathological cases
         --  where the prefix might include functions that return access to
         --  subprograms and others that return a regular type. Disambiguation
         --  of those has to take place in Resolve.
         --  See e.g. 7117-014 and E317-001.

         New_N :=
           Make_Function_Call (Loc,
           Name => Make_Explicit_Dereference (Loc, P),
           Parameter_Associations => New_List);

         --  If the prefix is overloaded, remove operations that have formals,
         --  we know that this is a parameterless call.

         if Is_Overloaded (P) then
            Get_First_Interp (P, I, It);
            while Present (It.Nam) loop
               T := It.Typ;

               if No (First_Formal (Base_Type (Designated_Type (T)))) then
                  Set_Etype (P, T);
               else
                  Remove_Interp (I);
               end if;

               Get_Next_Interp (I, It);
            end loop;
         end if;

         Rewrite (N, New_N);
         Analyze (N);

      elsif not Is_Function_Type
        and then Is_Overloaded (N)
      then
         --  The prefix may include access to subprograms and other access
         --  types. If the context selects the interpretation that is a call,
         --  we cannot rewrite the node yet, but we include the result of
         --  the call interpretation.

         Get_First_Interp (N, I, It);
         while Present (It.Nam) loop
            if Ekind (Base_Type (It.Typ)) = E_Subprogram_Type
               and then Etype (Base_Type (It.Typ)) /= Standard_Void_Type
            then
               Add_One_Interp (N, Etype (It.Typ), Etype (It.Typ));
            end if;

            Get_Next_Interp (I, It);
         end loop;
      end if;

      --  A value of remote access-to-class-wide must not be dereferenced
      --  (RM E.2.2(16)).

      Validate_Remote_Access_To_Class_Wide_Type (N);
   end Analyze_Explicit_Dereference;

   ------------------------
   -- Analyze_Expression --
   ------------------------

   procedure Analyze_Expression (N : Node_Id) is
   begin
      Analyze (N);
      Check_Parameterless_Call (N);
   end Analyze_Expression;

   ------------------------------------
   -- Analyze_Indexed_Component_Form --
   ------------------------------------

   procedure Analyze_Indexed_Component_Form (N : Node_Id) is
      P     : constant Node_Id := Prefix (N);
      Exprs : constant List_Id := Expressions (N);
      Exp   : Node_Id;
      P_T   : Entity_Id;
      E     : Node_Id;
      U_N   : Entity_Id;

      procedure Process_Function_Call;
      --  Prefix in indexed component form is an overloadable entity,
      --  so the node is a function call. Reformat it as such.

      procedure Process_Indexed_Component;
      --  Prefix in indexed component form is actually an indexed component.
      --  This routine processes it, knowing that the prefix is already
      --  resolved.

      procedure Process_Indexed_Component_Or_Slice;
      --  An indexed component with a single index may designate a slice if
      --  the index is a subtype mark. This routine disambiguates these two
      --  cases by resolving the prefix to see if it is a subtype mark.

      procedure Process_Overloaded_Indexed_Component;
      --  If the prefix of an indexed component is overloaded, the proper
      --  interpretation is selected by the index types and the context.

      ---------------------------
      -- Process_Function_Call --
      ---------------------------

      procedure Process_Function_Call is
         Actual : Node_Id;

      begin
         Change_Node (N, N_Function_Call);
         Set_Name (N, P);
         Set_Parameter_Associations (N, Exprs);

         Actual := First (Parameter_Associations (N));
         while Present (Actual) loop
            Analyze (Actual);
            Check_Parameterless_Call (Actual);
            Next_Actual (Actual);
         end loop;

         Analyze_Call (N);
      end Process_Function_Call;

      -------------------------------
      -- Process_Indexed_Component --
      -------------------------------

      procedure Process_Indexed_Component is
         Exp          : Node_Id;
         Array_Type   : Entity_Id;
         Index        : Node_Id;
         Pent         : Entity_Id := Empty;

      begin
         Exp := First (Exprs);

         if Is_Overloaded (P) then
            Process_Overloaded_Indexed_Component;

         else
            Array_Type := Etype (P);

            if Is_Entity_Name (P) then
               Pent := Entity (P);
            elsif Nkind (P) = N_Selected_Component
              and then Is_Entity_Name (Selector_Name (P))
            then
               Pent := Entity (Selector_Name (P));
            end if;

            --  Prefix must be appropriate for an array type, taking into
            --  account a possible implicit dereference.

            if Is_Access_Type (Array_Type) then
               Array_Type := Designated_Type (Array_Type);
               Error_Msg_NW (Warn_On_Dereference, "?implicit dereference", N);
               Process_Implicit_Dereference_Prefix (Pent, P);
            end if;

            if Is_Array_Type (Array_Type) then
               null;

            elsif Present (Pent) and then Ekind (Pent) = E_Entry_Family then
               Analyze (Exp);
               Set_Etype (N, Any_Type);

               if not Has_Compatible_Type
                 (Exp, Entry_Index_Type (Pent))
               then
                  Error_Msg_N ("invalid index type in entry name", N);

               elsif Present (Next (Exp)) then
                  Error_Msg_N ("too many subscripts in entry reference", N);

               else
                  Set_Etype (N,  Etype (P));
               end if;

               return;

            elsif Is_Record_Type (Array_Type)
              and then Remote_AST_I_Dereference (P)
            then
               return;

            elsif Array_Type = Any_Type then
               Set_Etype (N, Any_Type);
               return;

            --  Here we definitely have a bad indexing

            else
               if Nkind (Parent (N)) = N_Requeue_Statement
                 and then Present (Pent) and then Ekind (Pent) = E_Entry
               then
                  Error_Msg_N
                    ("REQUEUE does not permit parameters", First (Exprs));

               elsif Is_Entity_Name (P)
                 and then Etype (P) = Standard_Void_Type
               then
                  Error_Msg_NE ("incorrect use of&", P, Entity (P));

               else
                  Error_Msg_N ("array type required in indexed component", P);
               end if;

               Set_Etype (N, Any_Type);
               return;
            end if;

            Index := First_Index (Array_Type);

            while Present (Index) and then Present (Exp) loop
               if not Has_Compatible_Type (Exp, Etype (Index)) then
                  Wrong_Type (Exp, Etype (Index));
                  Set_Etype (N, Any_Type);
                  return;
               end if;

               Next_Index (Index);
               Next (Exp);
            end loop;

            Set_Etype (N, Component_Type (Array_Type));

            if Present (Index) then
               Error_Msg_N
                 ("too few subscripts in array reference", First (Exprs));

            elsif Present (Exp) then
               Error_Msg_N ("too many subscripts in array reference", Exp);
            end if;
         end if;
      end Process_Indexed_Component;

      ----------------------------------------
      -- Process_Indexed_Component_Or_Slice --
      ----------------------------------------

      procedure Process_Indexed_Component_Or_Slice is
      begin
         Exp := First (Exprs);
         while Present (Exp) loop
            Analyze_Expression (Exp);
            Next (Exp);
         end loop;

         Exp := First (Exprs);

         --  If one index is present, and it is a subtype name, then the
         --  node denotes a slice (note that the case of an explicit range
         --  for a slice was already built as an N_Slice node in the first
         --  place, so that case is not handled here).

         --  We use a replace rather than a rewrite here because this is one
         --  of the cases in which the tree built by the parser is plain wrong.

         if No (Next (Exp))
           and then Is_Entity_Name (Exp)
           and then Is_Type (Entity (Exp))
         then
            Replace (N,
               Make_Slice (Sloc (N),
                 Prefix => P,
                 Discrete_Range => New_Copy (Exp)));
            Analyze (N);

         --  Otherwise (more than one index present, or single index is not
         --  a subtype name), then we have the indexed component case.

         else
            Process_Indexed_Component;
         end if;
      end Process_Indexed_Component_Or_Slice;

      ------------------------------------------
      -- Process_Overloaded_Indexed_Component --
      ------------------------------------------

      procedure Process_Overloaded_Indexed_Component is
         Exp   : Node_Id;
         I     : Interp_Index;
         It    : Interp;
         Typ   : Entity_Id;
         Index : Node_Id;
         Found : Boolean;

      begin
         Set_Etype (N, Any_Type);

         Get_First_Interp (P, I, It);
         while Present (It.Nam) loop
            Typ := It.Typ;

            if Is_Access_Type (Typ) then
               Typ := Designated_Type (Typ);
               Error_Msg_NW (Warn_On_Dereference, "?implicit dereference", N);
            end if;

            if Is_Array_Type (Typ) then

               --  Got a candidate: verify that index types are compatible

               Index := First_Index (Typ);
               Found := True;
               Exp := First (Exprs);
               while Present (Index) and then Present (Exp) loop
                  if Has_Compatible_Type (Exp, Etype (Index)) then
                     null;
                  else
                     Found := False;
                     Remove_Interp (I);
                     exit;
                  end if;

                  Next_Index (Index);
                  Next (Exp);
               end loop;

               if Found and then No (Index) and then No (Exp) then
                  Add_One_Interp (N,
                     Etype (Component_Type (Typ)),
                     Etype (Component_Type (Typ)));
               end if;
            end if;

            Get_Next_Interp (I, It);
         end loop;

         if Etype (N) = Any_Type then
            Error_Msg_N ("no legal interpetation for indexed component", N);
            Set_Is_Overloaded (N, False);
         end if;

         End_Interp_List;
      end Process_Overloaded_Indexed_Component;

   --  Start of processing for Analyze_Indexed_Component_Form

   begin
      --  Get name of array, function or type

      Analyze (P);
      if Nkind (N) = N_Function_Call
        or else Nkind (N) = N_Procedure_Call_Statement
      then
         --  If P is an explicit dereference whose prefix is of a
         --  remote access-to-subprogram type, then N has already
         --  been rewritten as a subprogram call and analyzed.

         return;
      end if;

      pragma Assert (Nkind (N) = N_Indexed_Component);

      P_T := Base_Type (Etype (P));

      if Is_Entity_Name (P)
        or else Nkind (P) = N_Operator_Symbol
      then
         U_N := Entity (P);

         if Ekind (U_N) in Type_Kind then

            --  Reformat node as a type conversion

            E := Remove_Head (Exprs);

            if Present (First (Exprs)) then
               Error_Msg_N
                ("argument of type conversion must be single expression", N);
            end if;

            Change_Node (N, N_Type_Conversion);
            Set_Subtype_Mark (N, P);
            Set_Etype (N, U_N);
            Set_Expression (N, E);

            --  After changing the node, call for the specific Analysis
            --  routine directly, to avoid a double call to the expander.

            Analyze_Type_Conversion (N);
            return;
         end if;

         if Is_Overloadable (U_N) then
            Process_Function_Call;

         elsif Ekind (Etype (P)) = E_Subprogram_Type
           or else (Is_Access_Type (Etype (P))
                      and then
                    Ekind (Designated_Type (Etype (P))) = E_Subprogram_Type)
         then
            --  Call to access_to-subprogram with possible implicit dereference

            Process_Function_Call;

         elsif Is_Generic_Subprogram (U_N) then

            --  A common beginner's (or C++ templates fan) error

            Error_Msg_N ("generic subprogram cannot be called", N);
            Set_Etype (N, Any_Type);
            return;

         else
            Process_Indexed_Component_Or_Slice;
         end if;

      --  If not an entity name, prefix is an expression that may denote
      --  an array or an access-to-subprogram.

      else
         if Ekind (P_T) = E_Subprogram_Type
           or else (Is_Access_Type (P_T)
                     and then
                    Ekind (Designated_Type (P_T)) = E_Subprogram_Type)
         then
            Process_Function_Call;

         elsif Nkind (P) = N_Selected_Component
           and then Is_Overloadable (Entity (Selector_Name (P)))
         then
            Process_Function_Call;

         else
            --  Indexed component, slice, or a call to a member of a family
            --  entry, which will be converted to an entry call later.

            Process_Indexed_Component_Or_Slice;
         end if;
      end if;
   end Analyze_Indexed_Component_Form;

   ------------------------
   -- Analyze_Logical_Op --
   ------------------------

   procedure Analyze_Logical_Op (N : Node_Id) is
      L     : constant Node_Id := Left_Opnd (N);
      R     : constant Node_Id := Right_Opnd (N);
      Op_Id : Entity_Id := Entity (N);

   begin
      Set_Etype (N, Any_Type);
      Candidate_Type := Empty;

      Analyze_Expression (L);
      Analyze_Expression (R);

      if Present (Op_Id) then

         if Ekind (Op_Id) = E_Operator then
            Find_Boolean_Types (L, R, Op_Id, N);
         else
            Add_One_Interp (N, Op_Id, Etype (Op_Id));
         end if;

      else
         Op_Id := Get_Name_Entity_Id (Chars (N));

         while Present (Op_Id) loop
            if Ekind (Op_Id) = E_Operator then
               Find_Boolean_Types (L, R, Op_Id, N);
            else
               Analyze_User_Defined_Binary_Op (N, Op_Id);
            end if;

            Op_Id := Homonym (Op_Id);
         end loop;
      end if;

      Operator_Check (N);
   end Analyze_Logical_Op;

   ---------------------------
   -- Analyze_Membership_Op --
   ---------------------------

   procedure Analyze_Membership_Op (N : Node_Id) is
      L     : constant Node_Id := Left_Opnd (N);
      R     : constant Node_Id := Right_Opnd (N);

      Index : Interp_Index;
      It    : Interp;
      Found : Boolean := False;
      I_F   : Interp_Index;
      T_F   : Entity_Id;

      procedure Try_One_Interp (T1 : Entity_Id);
      --  Routine to try one proposed interpretation. Note that the context
      --  of the operation plays no role in resolving the arguments, so that
      --  if there is more than one interpretation of the operands that is
      --  compatible with a membership test, the operation is ambiguous.

      --------------------
      -- Try_One_Interp --
      --------------------

      procedure Try_One_Interp (T1 : Entity_Id) is
      begin
         if Has_Compatible_Type (R, T1) then
            if Found
              and then Base_Type (T1) /= Base_Type (T_F)
            then
               It := Disambiguate (L, I_F, Index, Any_Type);

               if It = No_Interp then
                  Ambiguous_Operands (N);
                  Set_Etype (L, Any_Type);
                  return;

               else
                  T_F := It.Typ;
               end if;

            else
               Found := True;
               T_F   := T1;
               I_F   := Index;
            end if;

            Set_Etype (L, T_F);
         end if;

      end Try_One_Interp;

   --  Start of processing for Analyze_Membership_Op

   begin
      Analyze_Expression (L);

      if Nkind (R) = N_Range
        or else (Nkind (R) = N_Attribute_Reference
                  and then Attribute_Name (R) = Name_Range)
      then
         Analyze (R);

         if not Is_Overloaded (L) then
            Try_One_Interp (Etype (L));

         else
            Get_First_Interp (L, Index, It);

            while Present (It.Typ) loop
               Try_One_Interp (It.Typ);
               Get_Next_Interp (Index, It);
            end loop;
         end if;

      --  If not a range, it can only be a subtype mark, or else there
      --  is a more basic error, to be diagnosed in Find_Type.

      else
         Find_Type (R);

         if Is_Entity_Name (R) then
            Check_Fully_Declared (Entity (R), R);
         end if;
      end if;

      --  Compatibility between expression and subtype mark or range is
      --  checked during resolution. The result of the operation is Boolean
      --  in any case.

      Set_Etype (N, Standard_Boolean);
   end Analyze_Membership_Op;

   ----------------------
   -- Analyze_Negation --
   ----------------------

   procedure Analyze_Negation (N : Node_Id) is
      R     : constant Node_Id := Right_Opnd (N);
      Op_Id : Entity_Id := Entity (N);

   begin
      Set_Etype (N, Any_Type);
      Candidate_Type := Empty;

      Analyze_Expression (R);

      if Present (Op_Id) then
         if Ekind (Op_Id) = E_Operator then
            Find_Negation_Types (R, Op_Id, N);
         else
            Add_One_Interp (N, Op_Id, Etype (Op_Id));
         end if;

      else
         Op_Id := Get_Name_Entity_Id (Chars (N));
         while Present (Op_Id) loop
            if Ekind (Op_Id) = E_Operator then
               Find_Negation_Types (R, Op_Id, N);
            else
               Analyze_User_Defined_Unary_Op (N, Op_Id);
            end if;

            Op_Id := Homonym (Op_Id);
         end loop;
      end if;

      Operator_Check (N);
   end Analyze_Negation;

   ------------------
   -- Analyze_Null --
   ------------------

   procedure Analyze_Null (N : Node_Id) is
   begin
      Set_Etype (N, Any_Access);
   end Analyze_Null;

   ----------------------
   -- Analyze_One_Call --
   ----------------------

   procedure Analyze_One_Call
      (N       : Node_Id;
       Nam     : Entity_Id;
       Report  : Boolean;
       Success : out Boolean)
   is
      Actuals    : constant List_Id   := Parameter_Associations (N);
      Prev_T     : constant Entity_Id := Etype (N);
      Formal     : Entity_Id;
      Actual     : Node_Id;
      Is_Indexed : Boolean := False;
      Subp_Type  : constant Entity_Id := Etype (Nam);
      Norm_OK    : Boolean;

      procedure Indicate_Name_And_Type;
      --  If candidate interpretation matches, indicate name and type of
      --  result on call node.

      ----------------------------
      -- Indicate_Name_And_Type --
      ----------------------------

      procedure Indicate_Name_And_Type is
      begin
         Add_One_Interp (N, Nam, Etype (Nam));
         Success := True;

         --  If the prefix of the call is a name, indicate the entity
         --  being called. If it is not a name,  it is an expression that
         --  denotes an access to subprogram or else an entry or family. In
         --  the latter case, the name is a selected component, and the entity
         --  being called is noted on the selector.

         if not Is_Type (Nam) then
            if Is_Entity_Name (Name (N))
              or else Nkind (Name (N)) = N_Operator_Symbol
            then
               Set_Entity (Name (N), Nam);

            elsif Nkind (Name (N)) = N_Selected_Component then
               Set_Entity (Selector_Name (Name (N)),  Nam);
            end if;
         end if;

         if Debug_Flag_E and not Report then
            Write_Str (" Overloaded call ");
            Write_Int (Int (N));
            Write_Str (" compatible with ");
            Write_Int (Int (Nam));
            Write_Eol;
         end if;
      end Indicate_Name_And_Type;

   --  Start of processing for Analyze_One_Call

   begin
      Success := False;

      --  If the subprogram has no formals, or if all the formals have
      --  defaults, and the return type is an array type, the node may
      --  denote an indexing of the result of a parameterless call.

      if Needs_No_Actuals (Nam)
        and then Present (Actuals)
      then
         if Is_Array_Type (Subp_Type) then
            Is_Indexed := Try_Indexed_Call (N, Nam, Subp_Type);

         elsif Is_Access_Type (Subp_Type)
           and then Is_Array_Type (Designated_Type (Subp_Type))
         then
            Is_Indexed :=
              Try_Indexed_Call (N, Nam, Designated_Type (Subp_Type));

         --  The prefix can also be a parameterless function that returns an
         --  access to subprogram. in which case this is an indirect call.

         elsif Is_Access_Type (Subp_Type)
           and then Ekind (Designated_Type (Subp_Type))  = E_Subprogram_Type
         then
            Is_Indexed := Try_Indirect_Call (N, Nam, Subp_Type);
         end if;

      end if;

      Normalize_Actuals (N, Nam, (Report and not Is_Indexed), Norm_OK);

      if not Norm_OK then

         --  Mismatch in number or names of parameters

         if Debug_Flag_E then
            Write_Str (" normalization fails in call ");
            Write_Int (Int (N));
            Write_Str (" with subprogram ");
            Write_Int (Int (Nam));
            Write_Eol;
         end if;

      --  If the context expects a function call, discard any interpretation
      --  that is a procedure. If the node is not overloaded, leave as is for
      --  better error reporting when type mismatch is found.

      elsif Nkind (N) = N_Function_Call
        and then Is_Overloaded (Name (N))
        and then Ekind (Nam) = E_Procedure
      then
         return;

      --  Ditto for function calls in a procedure context

      elsif Nkind (N) = N_Procedure_Call_Statement
         and then Is_Overloaded (Name (N))
         and then Etype (Nam) /= Standard_Void_Type
      then
         return;

      elsif not Present (Actuals) then

         --  If Normalize succeeds, then there are default parameters for
         --  all formals.

         Indicate_Name_And_Type;

      elsif Ekind (Nam) = E_Operator then
         if Nkind (N) = N_Procedure_Call_Statement then
            return;
         end if;

         --  This can occur when the prefix of the call is an operator
         --  name or an expanded name whose selector is an operator name.

         Analyze_Operator_Call (N, Nam);

         if Etype (N) /= Prev_T then

            --  There may be a user-defined operator that hides the
            --  current interpretation. We must check for this independently
            --  of the analysis of the call with the user-defined operation,
            --  because the parameter names may be wrong and yet the hiding
            --  takes place. Fixes b34014o.

            if Is_Overloaded (Name (N)) then
               declare
                  I  : Interp_Index;
                  It : Interp;

               begin
                  Get_First_Interp (Name (N), I, It);
                  while Present (It.Nam) loop
                     if Ekind (It.Nam) /= E_Operator
                        and then Hides_Op (It.Nam, Nam)
                        and then
                          Has_Compatible_Type
                            (First_Actual (N), Etype (First_Formal (It.Nam)))
                        and then (No (Next_Actual (First_Actual (N)))
                           or else Has_Compatible_Type
                            (Next_Actual (First_Actual (N)),
                             Etype (Next_Formal (First_Formal (It.Nam)))))
                     then
                        Set_Etype (N, Prev_T);
                        return;
                     end if;

                     Get_Next_Interp (I, It);
                  end loop;
               end;
            end if;

            --  If operator matches formals, record its name on the call.
            --  If the operator is overloaded, Resolve will select the
            --  correct one from the list of interpretations. The call
            --  node itself carries the first candidate.

            Set_Entity (Name (N), Nam);
            Success := True;

         elsif Report and then Etype (N) = Any_Type then
            Error_Msg_N ("incompatible arguments for operator", N);
         end if;

      else
         --  Normalize_Actuals has chained the named associations in the
         --  correct order of the formals.

         Actual := First_Actual (N);
         Formal := First_Formal (Nam);
         while Present (Actual) and then Present (Formal) loop
            if Nkind (Parent (Actual)) /= N_Parameter_Association
              or else Chars (Selector_Name (Parent (Actual))) = Chars (Formal)
            then
               if Has_Compatible_Type (Actual, Etype (Formal)) then
                  Next_Actual (Actual);
                  Next_Formal (Formal);

               else
                  if Debug_Flag_E then
                     Write_Str (" type checking fails in call ");
                     Write_Int (Int (N));
                     Write_Str (" with formal ");
                     Write_Int (Int (Formal));
                     Write_Str (" in subprogram ");
                     Write_Int (Int (Nam));
                     Write_Eol;
                  end if;

                  if Report and not Is_Indexed then

                     --  Ada 2005 (AI-251): Complete the error notification
                     --  to help new Ada 2005 users

                     if Is_Class_Wide_Type (Etype (Formal))
                       and then Is_Interface (Etype (Etype (Formal)))
                       and then not Interface_Present_In_Ancestor
                                      (Typ   => Etype (Actual),
                                       Iface => Etype (Etype (Formal)))
                     then
                        Error_Msg_Name_1 := Chars (Actual);
                        Error_Msg_Name_2 := Chars (Etype (Etype (Formal)));
                        Error_Msg_NE
                          ("(Ada 2005) % does not implement interface %",
                           Actual, Etype (Etype (Formal)));
                     end if;

                     Wrong_Type (Actual, Etype (Formal));

                     if Nkind (Actual) = N_Op_Eq
                       and then Nkind (Left_Opnd (Actual)) = N_Identifier
                     then
                        Formal := First_Formal (Nam);

                        while Present (Formal) loop

                           if Chars (Left_Opnd (Actual)) = Chars (Formal) then
                              Error_Msg_N
                                ("possible misspelling of `='>`!", Actual);
                              exit;
                           end if;

                           Next_Formal (Formal);
                        end loop;
                     end if;

                     if All_Errors_Mode then
                        Error_Msg_Sloc := Sloc (Nam);

                        if Is_Overloadable (Nam)
                          and then Present (Alias (Nam))
                          and then not Comes_From_Source (Nam)
                        then
                           Error_Msg_NE
                             ("  =='> in call to &#(inherited)!", Actual, Nam);

                        elsif Ekind (Nam) = E_Subprogram_Type then
                           declare
                              Access_To_Subprogram_Typ :
                                constant Entity_Id :=
                                  Defining_Identifier
                                    (Associated_Node_For_Itype (Nam));
                           begin
                              Error_Msg_NE (
                                "  =='> in call to dereference of &#!",
                                Actual, Access_To_Subprogram_Typ);
                           end;

                        else
                           Error_Msg_NE ("  =='> in call to &#!", Actual, Nam);

                        end if;
                     end if;
                  end if;

                  return;
               end if;

            else
               --  Normalize_Actuals has verified that a default value exists
               --  for this formal. Current actual names a subsequent formal.

               Next_Formal (Formal);
            end if;
         end loop;

         --  On exit, all actuals match

         Indicate_Name_And_Type;
      end if;
   end Analyze_One_Call;

   ---------------------------
   -- Analyze_Operator_Call --
   ---------------------------

   procedure Analyze_Operator_Call (N : Node_Id; Op_Id : Entity_Id) is
      Op_Name : constant Name_Id := Chars (Op_Id);
      Act1    : constant Node_Id := First_Actual (N);
      Act2    : constant Node_Id := Next_Actual (Act1);

   begin
      --  Binary operator case

      if Present (Act2) then

         --  If more than two operands, then not binary operator after all

         if Present (Next_Actual (Act2)) then
            return;

         elsif     Op_Name = Name_Op_Add
           or else Op_Name = Name_Op_Subtract
           or else Op_Name = Name_Op_Multiply
           or else Op_Name = Name_Op_Divide
           or else Op_Name = Name_Op_Mod
           or else Op_Name = Name_Op_Rem
           or else Op_Name = Name_Op_Expon
         then
            Find_Arithmetic_Types (Act1, Act2, Op_Id, N);

         elsif     Op_Name =  Name_Op_And
           or else Op_Name = Name_Op_Or
           or else Op_Name = Name_Op_Xor
         then
            Find_Boolean_Types (Act1, Act2, Op_Id, N);

         elsif     Op_Name = Name_Op_Lt
           or else Op_Name = Name_Op_Le
           or else Op_Name = Name_Op_Gt
           or else Op_Name = Name_Op_Ge
         then
            Find_Comparison_Types (Act1, Act2, Op_Id,  N);

         elsif     Op_Name = Name_Op_Eq
           or else Op_Name = Name_Op_Ne
         then
            Find_Equality_Types (Act1, Act2, Op_Id,  N);

         elsif     Op_Name = Name_Op_Concat then
            Find_Concatenation_Types (Act1, Act2, Op_Id, N);

         --  Is this else null correct, or should it be an abort???

         else
            null;
         end if;

      --  Unary operator case

      else
         if Op_Name = Name_Op_Subtract or else
            Op_Name = Name_Op_Add      or else
            Op_Name = Name_Op_Abs
         then
            Find_Unary_Types (Act1, Op_Id, N);

         elsif
            Op_Name = Name_Op_Not
         then
            Find_Negation_Types (Act1, Op_Id, N);

         --  Is this else null correct, or should it be an abort???

         else
            null;
         end if;
      end if;
   end Analyze_Operator_Call;

   -------------------------------------------
   -- Analyze_Overloaded_Selected_Component --
   -------------------------------------------

   procedure Analyze_Overloaded_Selected_Component (N : Node_Id) is
      Nam   : constant Node_Id := Prefix (N);
      Sel   : constant Node_Id := Selector_Name (N);
      Comp  : Entity_Id;
      I     : Interp_Index;
      It    : Interp;
      T     : Entity_Id;

   begin
      Set_Etype (Sel, Any_Type);

      Get_First_Interp (Nam, I, It);
      while Present (It.Typ) loop
         if Is_Access_Type (It.Typ) then
            T := Designated_Type (It.Typ);
            Error_Msg_NW (Warn_On_Dereference, "?implicit dereference", N);
         else
            T := It.Typ;
         end if;

         if Is_Record_Type (T) then
            Comp := First_Entity (T);
            while Present (Comp) loop
               if Chars (Comp) = Chars (Sel)
                 and then Is_Visible_Component (Comp)
               then
                  Set_Entity_With_Style_Check (Sel, Comp);
                  Generate_Reference (Comp, Sel);

                  Set_Etype (Sel, Etype (Comp));
                  Add_One_Interp (N, Etype (Comp), Etype (Comp));

                  --  This also specifies a candidate to resolve the name.
                  --  Further overloading will be resolved from context.

                  Set_Etype (Nam, It.Typ);
               end if;

               Next_Entity (Comp);
            end loop;

         elsif Is_Concurrent_Type (T) then
            Comp := First_Entity (T);
            while Present (Comp)
              and then Comp /= First_Private_Entity (T)
            loop
               if Chars (Comp) = Chars (Sel) then
                  if Is_Overloadable (Comp) then
                     Add_One_Interp (Sel, Comp, Etype (Comp));
                  else
                     Set_Entity_With_Style_Check (Sel, Comp);
                     Generate_Reference (Comp, Sel);
                  end if;

                  Set_Etype (Sel, Etype (Comp));
                  Set_Etype (N,   Etype (Comp));
                  Set_Etype (Nam, It.Typ);

                  --  For access type case, introduce explicit deference for
                  --  more uniform treatment of entry calls.

                  if Is_Access_Type (Etype (Nam)) then
                     Insert_Explicit_Dereference (Nam);
                     Error_Msg_NW
                       (Warn_On_Dereference, "?implicit dereference", N);
                  end if;
               end if;

               Next_Entity (Comp);
            end loop;

            Set_Is_Overloaded (N, Is_Overloaded (Sel));
         end if;

         Get_Next_Interp (I, It);
      end loop;

      if Etype (N) = Any_Type then
         Error_Msg_NE ("undefined selector& for overloaded prefix", N, Sel);
         Set_Entity (Sel, Any_Id);
         Set_Etype  (Sel, Any_Type);
      end if;
   end Analyze_Overloaded_Selected_Component;

   ----------------------------------
   -- Analyze_Qualified_Expression --
   ----------------------------------

   procedure Analyze_Qualified_Expression (N : Node_Id) is
      Mark : constant Entity_Id := Subtype_Mark (N);
      T    : Entity_Id;

   begin
      Set_Etype (N, Any_Type);
      Find_Type (Mark);
      T := Entity (Mark);

      if T = Any_Type then
         return;
      end if;

      Check_Fully_Declared (T, N);
      Analyze_Expression (Expression (N));
      Set_Etype  (N, T);
   end Analyze_Qualified_Expression;

   -------------------
   -- Analyze_Range --
   -------------------

   procedure Analyze_Range (N : Node_Id) is
      L        : constant Node_Id := Low_Bound (N);
      H        : constant Node_Id := High_Bound (N);
      I1, I2   : Interp_Index;
      It1, It2 : Interp;

      procedure Check_Common_Type (T1, T2 : Entity_Id);
      --  Verify the compatibility of two types,  and choose the
      --  non universal one if the other is universal.

      procedure Check_High_Bound (T : Entity_Id);
      --  Test one interpretation of the low bound against all those
      --  of the high bound.

      procedure Check_Universal_Expression (N : Node_Id);
      --  In Ada83, reject bounds of a universal range that are not
      --  literals or entity names.

      -----------------------
      -- Check_Common_Type --
      -----------------------

      procedure Check_Common_Type (T1, T2 : Entity_Id) is
      begin
         if Covers (T1, T2) or else Covers (T2, T1) then
            if T1 = Universal_Integer
              or else T1 = Universal_Real
              or else T1 = Any_Character
            then
               Add_One_Interp (N, Base_Type (T2), Base_Type (T2));

            elsif T1 = T2 then
               Add_One_Interp (N, T1, T1);

            else
               Add_One_Interp (N, Base_Type (T1), Base_Type (T1));
            end if;
         end if;
      end Check_Common_Type;

      ----------------------
      -- Check_High_Bound --
      ----------------------

      procedure Check_High_Bound (T : Entity_Id) is
      begin
         if not Is_Overloaded (H) then
            Check_Common_Type (T, Etype (H));
         else
            Get_First_Interp (H, I2, It2);
            while Present (It2.Typ) loop
               Check_Common_Type (T, It2.Typ);
               Get_Next_Interp (I2, It2);
            end loop;
         end if;
      end Check_High_Bound;

      -----------------------------
      -- Is_Universal_Expression --
      -----------------------------

      procedure Check_Universal_Expression (N : Node_Id) is
      begin
         if Etype (N) = Universal_Integer
           and then Nkind (N) /= N_Integer_Literal
           and then not Is_Entity_Name (N)
           and then Nkind (N) /= N_Attribute_Reference
         then
            Error_Msg_N ("illegal bound in discrete range", N);
         end if;
      end Check_Universal_Expression;

   --  Start of processing for Analyze_Range

   begin
      Set_Etype (N, Any_Type);
      Analyze_Expression (L);
      Analyze_Expression (H);

      if Etype (L) = Any_Type or else Etype (H) = Any_Type then
         return;

      else
         if not Is_Overloaded (L) then
            Check_High_Bound (Etype (L));
         else
            Get_First_Interp (L, I1, It1);
            while Present (It1.Typ) loop
               Check_High_Bound (It1.Typ);
               Get_Next_Interp (I1, It1);
            end loop;
         end if;

         --  If result is Any_Type, then we did not find a compatible pair

         if Etype (N) = Any_Type then
            Error_Msg_N ("incompatible types in range ", N);
         end if;
      end if;

      if Ada_Version = Ada_83
        and then
          (Nkind (Parent (N)) = N_Loop_Parameter_Specification
             or else Nkind (Parent (N)) = N_Constrained_Array_Definition)
      then
         Check_Universal_Expression (L);
         Check_Universal_Expression (H);
      end if;
   end Analyze_Range;

   -----------------------
   -- Analyze_Reference --
   -----------------------

   procedure Analyze_Reference (N : Node_Id) is
      P        : constant Node_Id := Prefix (N);
      Acc_Type : Entity_Id;
   begin
      Analyze (P);
      Acc_Type := Create_Itype (E_Allocator_Type, N);
      Set_Etype                    (Acc_Type,  Acc_Type);
      Init_Size_Align              (Acc_Type);
      Set_Directly_Designated_Type (Acc_Type, Etype (P));
      Set_Etype (N, Acc_Type);
   end Analyze_Reference;

   --------------------------------
   -- Analyze_Selected_Component --
   --------------------------------

   --  Prefix is a record type or a task or protected type. In the
   --  later case, the selector must denote a visible entry.

   procedure Analyze_Selected_Component (N : Node_Id) is
      Name        : constant Node_Id := Prefix (N);
      Sel         : constant Node_Id := Selector_Name (N);
      Comp        : Entity_Id;
      Entity_List : Entity_Id;
      Prefix_Type : Entity_Id;
      Pent        : Entity_Id := Empty;
      Act_Decl    : Node_Id;
      In_Scope    : Boolean;
      Parent_N    : Node_Id;

   --  Start of processing for Analyze_Selected_Component

   begin
      Set_Etype (N, Any_Type);

      if Is_Overloaded (Name) then
         Analyze_Overloaded_Selected_Component (N);
         return;

      elsif Etype (Name) = Any_Type then
         Set_Entity (Sel, Any_Id);
         Set_Etype (Sel, Any_Type);
         return;

      else
         --  Function calls that are prefixes of selected components must be
         --  fully resolved in case we need to build an actual subtype, or
         --  do some other operation requiring a fully resolved prefix.

         --  Note: Resolving all Nkinds of nodes here doesn't work.
         --  (Breaks 2129-008) ???.

         if Nkind (Name) = N_Function_Call then
            Resolve (Name);
         end if;

         Prefix_Type := Etype (Name);
      end if;

      if Is_Access_Type (Prefix_Type) then

         --  A RACW object can never be used as prefix of a selected
         --  component since that means it is dereferenced without
         --  being a controlling operand of a dispatching operation
         --  (RM E.2.2(15)).

         if Is_Remote_Access_To_Class_Wide_Type (Prefix_Type)
           and then Comes_From_Source (N)
         then
            Error_Msg_N
              ("invalid dereference of a remote access to class-wide value",
               N);

         --  Normal case of selected component applied to access type

         else
            Error_Msg_NW (Warn_On_Dereference, "?implicit dereference", N);

            if Is_Entity_Name (Name) then
               Pent := Entity (Name);
            elsif Nkind (Name) = N_Selected_Component
              and then Is_Entity_Name (Selector_Name (Name))
            then
               Pent := Entity (Selector_Name (Name));
            end if;

            Process_Implicit_Dereference_Prefix (Pent, Name);
         end if;

         Prefix_Type := Designated_Type (Prefix_Type);
      end if;

      if Ekind (Prefix_Type) = E_Private_Subtype then
         Prefix_Type := Base_Type (Prefix_Type);
      end if;

      Entity_List := Prefix_Type;

      --  For class-wide types, use the entity list of the root type. This
      --  indirection is specially important for private extensions because
      --  only the root type get switched (not the class-wide type).

      if Is_Class_Wide_Type (Prefix_Type) then
         Entity_List := Root_Type (Prefix_Type);
      end if;

      Comp := First_Entity (Entity_List);

      --  If the selector has an original discriminant, the node appears in
      --  an instance. Replace the discriminant with the corresponding one
      --  in the current discriminated type. For nested generics, this must
      --  be done transitively, so note the new original discriminant.

      if Nkind (Sel) = N_Identifier
        and then Present (Original_Discriminant (Sel))
      then
         Comp := Find_Corresponding_Discriminant (Sel, Prefix_Type);

         --  Mark entity before rewriting, for completeness and because
         --  subsequent semantic checks might examine the original node.

         Set_Entity (Sel, Comp);
         Rewrite (Selector_Name (N),
           New_Occurrence_Of (Comp, Sloc (N)));
         Set_Original_Discriminant (Selector_Name (N), Comp);
         Set_Etype (N, Etype (Comp));

         if Is_Access_Type (Etype (Name)) then
            Insert_Explicit_Dereference (Name);
            Error_Msg_NW (Warn_On_Dereference, "?implicit dereference", N);
         end if;

      elsif Is_Record_Type (Prefix_Type) then

         --  Find component with given name

         while Present (Comp) loop
            if Chars (Comp) = Chars (Sel)
              and then Is_Visible_Component (Comp)
            then
               Set_Entity_With_Style_Check (Sel, Comp);
               Generate_Reference (Comp, Sel);

               Set_Etype (Sel, Etype (Comp));

               if Ekind (Comp) = E_Discriminant then
                  if Is_Unchecked_Union (Base_Type (Prefix_Type)) then
                     Error_Msg_N
                       ("cannot reference discriminant of Unchecked_Union",
                        Sel);
                  end if;

                  if Is_Generic_Type (Prefix_Type)
                       or else
                     Is_Generic_Type (Root_Type (Prefix_Type))
                  then
                     Set_Original_Discriminant (Sel, Comp);
                  end if;
               end if;

               --  Resolve the prefix early otherwise it is not possible to
               --  build the actual subtype of the component: it may need
               --  to duplicate this prefix and duplication is only allowed
               --  on fully resolved expressions.

               Resolve (Name);

               --  Ada 2005 (AI-50217): Check wrong use of incomplete type.
               --  Example:

               --    limited with Pkg;
               --    package Pkg is
               --       type Acc_Inc is access Pkg.T;
               --       X : Acc_Inc;
               --       N : Natural := X.all.Comp; -- ERROR
               --    end Pkg;

               if Nkind (Name) = N_Explicit_Dereference
                 and then From_With_Type (Etype (Prefix (Name)))
                 and then not Is_Potentially_Use_Visible (Etype (Name))
               then
                  Error_Msg_NE
                    ("premature usage of incomplete}", Prefix (Name),
                     Etype (Prefix (Name)));
               end if;

               --  We never need an actual subtype for the case of a selection
               --  for a indexed component of a non-packed array, since in
               --  this case gigi generates all the checks and can find the
               --  necessary bounds information.

               --  We also do not need an actual subtype for the case of
               --  a first, last, length, or range attribute applied to a
               --  non-packed array, since gigi can again get the bounds in
               --  these cases (gigi cannot handle the packed case, since it
               --  has the bounds of the packed array type, not the original
               --  bounds of the type). However, if the prefix is itself a
               --  selected component, as in a.b.c (i), gigi may regard a.b.c
               --  as a dynamic-sized temporary, so we do generate an actual
               --  subtype for this case.

               Parent_N := Parent (N);

               if not Is_Packed (Etype (Comp))
                 and then
                   ((Nkind (Parent_N) = N_Indexed_Component
                      and then Nkind (Name) /= N_Selected_Component)
                     or else
                      (Nkind (Parent_N) = N_Attribute_Reference
                         and then (Attribute_Name (Parent_N) = Name_First
                                     or else
                                   Attribute_Name (Parent_N) = Name_Last
                                     or else
                                   Attribute_Name (Parent_N) = Name_Length
                                     or else
                                   Attribute_Name (Parent_N) = Name_Range)))
               then
                  Set_Etype (N, Etype (Comp));

               --  If full analysis is not enabled, we do not generate an
               --  actual subtype, because in the absence of expansion
               --  reference to a formal of a protected type, for example,
               --  will not be properly transformed, and will lead to
               --  out-of-scope references in gigi.

               --  In all other cases, we currently build an actual subtype.
               --  It seems likely that many of these cases can be avoided,
               --  but right now, the front end makes direct references to the
               --  bounds (e.g. in generating a length check), and if we do
               --  not make an actual subtype, we end up getting a direct
               --  reference to a discriminant, which will not do.

               elsif Full_Analysis then
                  Act_Decl :=
                    Build_Actual_Subtype_Of_Component (Etype (Comp), N);
                  Insert_Action (N, Act_Decl);

                  if No (Act_Decl) then
                     Set_Etype (N, Etype (Comp));

                  else
                     --  Component type depends on discriminants. Enter the
                     --  main attributes of the subtype.

                     declare
                        Subt : constant Entity_Id :=
                                 Defining_Identifier (Act_Decl);

                     begin
                        Set_Etype (Subt, Base_Type (Etype (Comp)));
                        Set_Ekind (Subt, Ekind (Etype (Comp)));
                        Set_Etype (N, Subt);
                     end;
                  end if;

               --  If Full_Analysis not enabled, just set the Etype

               else
                  Set_Etype (N, Etype (Comp));
               end if;

               return;
            end if;

            Next_Entity (Comp);
         end loop;

         --  Ada 2005 (AI-252)

         if Ada_Version >= Ada_05
           and then Is_Tagged_Type (Prefix_Type)
           and then Try_Object_Operation (N)
         then
            return;

            --  If the transformation fails, it will be necessary to redo the
            --  analysis with all errors enabled, to indicate candidate
            --  interpretations and reasons for each failure ???

         end if;

      elsif Is_Private_Type (Prefix_Type) then

         --  Allow access only to discriminants of the type. If the type has
         --  no full view, gigi uses the parent type for the components, so we
         --  do the same here.

         if No (Full_View (Prefix_Type)) then
            Entity_List := Root_Type (Base_Type (Prefix_Type));
            Comp := First_Entity (Entity_List);
         end if;

         while Present (Comp) loop
            if Chars (Comp) = Chars (Sel) then
               if Ekind (Comp) = E_Discriminant then
                  Set_Entity_With_Style_Check (Sel, Comp);
                  Generate_Reference (Comp, Sel);

                  Set_Etype (Sel, Etype (Comp));
                  Set_Etype (N,   Etype (Comp));

                  if Is_Generic_Type (Prefix_Type)
                    or else
                     Is_Generic_Type (Root_Type (Prefix_Type))
                  then
                     Set_Original_Discriminant (Sel, Comp);
                  end if;

               else
                  Error_Msg_NE
                    ("invisible selector for }",
                     N, First_Subtype (Prefix_Type));
                  Set_Entity (Sel, Any_Id);
                  Set_Etype (N, Any_Type);
               end if;

               return;
            end if;

            Next_Entity (Comp);
         end loop;

      elsif Is_Concurrent_Type (Prefix_Type) then

         --  Prefix is concurrent type. Find visible operation with given name
         --  For a task, this can only include entries or discriminants if the
         --  task type is not an enclosing scope. If it is an enclosing scope
         --  (e.g. in an inner task) then all entities are visible, but the
         --  prefix must denote the enclosing scope, i.e. can only be a direct
         --  name or an expanded name.

         Set_Etype (Sel,  Any_Type);
         In_Scope := In_Open_Scopes (Prefix_Type);

         while Present (Comp) loop
            if Chars (Comp) = Chars (Sel) then
               if Is_Overloadable (Comp) then
                  Add_One_Interp (Sel, Comp, Etype (Comp));

               elsif Ekind (Comp) = E_Discriminant
                 or else Ekind (Comp) = E_Entry_Family
                 or else (In_Scope
                   and then Is_Entity_Name (Name))
               then
                  Set_Entity_With_Style_Check (Sel, Comp);
                  Generate_Reference (Comp, Sel);

               else
                  goto Next_Comp;
               end if;

               Set_Etype (Sel, Etype (Comp));
               Set_Etype (N,   Etype (Comp));

               if Ekind (Comp) = E_Discriminant then
                  Set_Original_Discriminant (Sel, Comp);
               end if;

               --  For access type case, introduce explicit deference for more
               --  uniform treatment of entry calls.

               if Is_Access_Type (Etype (Name)) then
                  Insert_Explicit_Dereference (Name);
                  Error_Msg_NW
                    (Warn_On_Dereference, "?implicit dereference", N);
               end if;
            end if;

            <<Next_Comp>>
               Next_Entity (Comp);
               exit when not In_Scope
                 and then
                   Comp = First_Private_Entity (Base_Type (Prefix_Type));
         end loop;

         Set_Is_Overloaded (N, Is_Overloaded (Sel));

      else
         --  Invalid prefix

         Error_Msg_NE ("invalid prefix in selected component&", N, Sel);
      end if;

      --  If N still has no type, the component is not defined in the prefix

      if Etype (N) = Any_Type then

         --  If the prefix is a single concurrent object, use its name in the
         --  error message, rather than that of its anonymous type.

         if Is_Concurrent_Type (Prefix_Type)
           and then Is_Internal_Name (Chars (Prefix_Type))
           and then not Is_Derived_Type (Prefix_Type)
           and then Is_Entity_Name (Name)
         then

            Error_Msg_Node_2 := Entity (Name);
            Error_Msg_NE ("no selector& for&", N, Sel);

            Check_Misspelled_Selector (Entity_List, Sel);

         elsif Is_Generic_Type (Prefix_Type)
           and then Ekind (Prefix_Type) = E_Record_Type_With_Private
           and then Prefix_Type /= Etype (Prefix_Type)
           and then Is_Record_Type (Etype (Prefix_Type))
         then
            --  If this is a derived formal type, the parent may have
            --  different visibility at this point. Try for an inherited
            --  component before reporting an error.

            Set_Etype (Prefix (N), Etype (Prefix_Type));
            Analyze_Selected_Component (N);
            return;

         elsif Ekind (Prefix_Type) = E_Record_Subtype_With_Private
           and then Is_Generic_Actual_Type (Prefix_Type)
           and then Present (Full_View (Prefix_Type))
         then
            --  Similarly, if this the actual for a formal derived type, the
            --  component inherited from the generic parent may not be visible
            --  in the actual, but the selected component is legal.

            declare
               Comp : Entity_Id;

            begin
               Comp :=
                 First_Component (Generic_Parent_Type (Parent (Prefix_Type)));
               while Present (Comp) loop
                  if Chars (Comp) = Chars (Sel) then
                     Set_Entity_With_Style_Check (Sel, Comp);
                     Set_Etype (Sel, Etype (Comp));
                     Set_Etype (N,   Etype (Comp));
                     exit;
                  end if;

                  Next_Component (Comp);
               end loop;

               pragma Assert (Etype (N) /= Any_Type);
            end;

         else
            if Ekind (Prefix_Type) = E_Record_Subtype then

               --  Check whether this is a component of the base type
               --  which is absent from a statically constrained subtype.
               --  This will raise constraint error at run-time, but is
               --  not a compile-time error. When the selector is illegal
               --  for base type as well fall through and generate a
               --  compilation error anyway.

               Comp := First_Component (Base_Type (Prefix_Type));
               while Present (Comp) loop
                  if Chars (Comp) = Chars (Sel)
                    and then Is_Visible_Component (Comp)
                  then
                     Set_Entity_With_Style_Check (Sel, Comp);
                     Generate_Reference (Comp, Sel);
                     Set_Etype (Sel, Etype (Comp));
                     Set_Etype (N,   Etype (Comp));

                     --  Emit appropriate message. Gigi will replace the
                     --  node subsequently with the appropriate Raise.

                     Apply_Compile_Time_Constraint_Error
                       (N, "component not present in }?",
                        CE_Discriminant_Check_Failed,
                        Ent => Prefix_Type, Rep => False);
                     Set_Raises_Constraint_Error (N);
                     return;
                  end if;

                  Next_Component (Comp);
               end loop;

            end if;

            Error_Msg_Node_2 := First_Subtype (Prefix_Type);
            Error_Msg_NE ("no selector& for}", N, Sel);

            Check_Misspelled_Selector (Entity_List, Sel);

         end if;

         Set_Entity (Sel, Any_Id);
         Set_Etype (Sel, Any_Type);
      end if;
   end Analyze_Selected_Component;

   ---------------------------
   -- Analyze_Short_Circuit --
   ---------------------------

   procedure Analyze_Short_Circuit (N : Node_Id) is
      L   : constant Node_Id := Left_Opnd  (N);
      R   : constant Node_Id := Right_Opnd (N);
      Ind : Interp_Index;
      It  : Interp;

   begin
      Analyze_Expression (L);
      Analyze_Expression (R);
      Set_Etype (N, Any_Type);

      if not Is_Overloaded (L) then

         if Root_Type (Etype (L)) = Standard_Boolean
           and then Has_Compatible_Type (R, Etype (L))
         then
            Add_One_Interp (N, Etype (L), Etype (L));
         end if;

      else
         Get_First_Interp (L, Ind, It);

         while Present (It.Typ) loop
            if Root_Type (It.Typ) = Standard_Boolean
              and then Has_Compatible_Type (R, It.Typ)
            then
               Add_One_Interp (N, It.Typ, It.Typ);
            end if;

            Get_Next_Interp (Ind, It);
         end loop;
      end if;

      --  Here we have failed to find an interpretation. Clearly we
      --  know that it is not the case that both operands can have
      --  an interpretation of Boolean, but this is by far the most
      --  likely intended interpretation. So we simply resolve both
      --  operands as Booleans, and at least one of these resolutions
      --  will generate an error message, and we do not need to give
      --  a further error message on the short circuit operation itself.

      if Etype (N) = Any_Type then
         Resolve (L, Standard_Boolean);
         Resolve (R, Standard_Boolean);
         Set_Etype (N, Standard_Boolean);
      end if;
   end Analyze_Short_Circuit;

   -------------------
   -- Analyze_Slice --
   -------------------

   procedure Analyze_Slice (N : Node_Id) is
      P          : constant Node_Id := Prefix (N);
      D          : constant Node_Id := Discrete_Range (N);
      Array_Type : Entity_Id;

      procedure Analyze_Overloaded_Slice;
      --  If the prefix is overloaded, select those interpretations that
      --  yield a one-dimensional array type.

      ------------------------------
      -- Analyze_Overloaded_Slice --
      ------------------------------

      procedure Analyze_Overloaded_Slice is
         I   : Interp_Index;
         It  : Interp;
         Typ : Entity_Id;

      begin
         Set_Etype (N, Any_Type);

         Get_First_Interp (P, I, It);
         while Present (It.Nam) loop
            Typ := It.Typ;

            if Is_Access_Type (Typ) then
               Typ := Designated_Type (Typ);
               Error_Msg_NW (Warn_On_Dereference, "?implicit dereference", N);
            end if;

            if Is_Array_Type (Typ)
              and then Number_Dimensions (Typ) = 1
              and then Has_Compatible_Type (D, Etype (First_Index (Typ)))
            then
               Add_One_Interp (N, Typ, Typ);
            end if;

            Get_Next_Interp (I, It);
         end loop;

         if Etype (N) = Any_Type then
            Error_Msg_N ("expect array type in prefix of slice",  N);
         end if;
      end Analyze_Overloaded_Slice;

   --  Start of processing for Analyze_Slice

   begin
      Analyze (P);
      Analyze (D);

      if Is_Overloaded (P) then
         Analyze_Overloaded_Slice;

      else
         Array_Type := Etype (P);
         Set_Etype (N, Any_Type);

         if Is_Access_Type (Array_Type) then
            Array_Type := Designated_Type (Array_Type);
            Error_Msg_NW (Warn_On_Dereference, "?implicit dereference", N);
         end if;

         if not Is_Array_Type (Array_Type) then
            Wrong_Type (P, Any_Array);

         elsif Number_Dimensions (Array_Type) > 1 then
            Error_Msg_N
              ("type is not one-dimensional array in slice prefix", N);

         elsif not
           Has_Compatible_Type (D, Etype (First_Index (Array_Type)))
         then
            Wrong_Type (D, Etype (First_Index (Array_Type)));

         else
            Set_Etype (N, Array_Type);
         end if;
      end if;
   end Analyze_Slice;

   -----------------------------
   -- Analyze_Type_Conversion --
   -----------------------------

   procedure Analyze_Type_Conversion (N : Node_Id) is
      Expr : constant Node_Id := Expression (N);
      T    : Entity_Id;

   begin
      --  If Conversion_OK is set, then the Etype is already set, and the
      --  only processing required is to analyze the expression. This is
      --  used to construct certain "illegal" conversions which are not
      --  allowed by Ada semantics, but can be handled OK by Gigi, see
      --  Sinfo for further details.

      if Conversion_OK (N) then
         Analyze (Expr);
         return;
      end if;

      --  Otherwise full type analysis is required, as well as some semantic
      --  checks to make sure the argument of the conversion is appropriate.

      Find_Type (Subtype_Mark (N));
      T := Entity (Subtype_Mark (N));
      Set_Etype (N, T);
      Check_Fully_Declared (T, N);
      Analyze_Expression (Expr);
      Validate_Remote_Type_Type_Conversion (N);

      --  Only remaining step is validity checks on the argument. These
      --  are skipped if the conversion does not come from the source.

      if not Comes_From_Source (N) then
         return;

      elsif Nkind (Expr) = N_Null then
         Error_Msg_N ("argument of conversion cannot be null", N);
         Error_Msg_N ("\use qualified expression instead", N);
         Set_Etype (N, Any_Type);

      elsif Nkind (Expr) = N_Aggregate then
         Error_Msg_N ("argument of conversion cannot be aggregate", N);
         Error_Msg_N ("\use qualified expression instead", N);

      elsif Nkind (Expr) = N_Allocator then
         Error_Msg_N ("argument of conversion cannot be an allocator", N);
         Error_Msg_N ("\use qualified expression instead", N);

      elsif Nkind (Expr) = N_String_Literal then
         Error_Msg_N ("argument of conversion cannot be string literal", N);
         Error_Msg_N ("\use qualified expression instead", N);

      elsif Nkind (Expr) = N_Character_Literal then
         if Ada_Version = Ada_83 then
            Resolve (Expr, T);
         else
            Error_Msg_N ("argument of conversion cannot be character literal",
              N);
            Error_Msg_N ("\use qualified expression instead", N);
         end if;

      elsif Nkind (Expr) = N_Attribute_Reference
        and then
          (Attribute_Name (Expr) = Name_Access            or else
           Attribute_Name (Expr) = Name_Unchecked_Access  or else
           Attribute_Name (Expr) = Name_Unrestricted_Access)
      then
         Error_Msg_N ("argument of conversion cannot be access", N);
         Error_Msg_N ("\use qualified expression instead", N);
      end if;
   end Analyze_Type_Conversion;

   ----------------------
   -- Analyze_Unary_Op --
   ----------------------

   procedure Analyze_Unary_Op (N : Node_Id) is
      R     : constant Node_Id := Right_Opnd (N);
      Op_Id : Entity_Id := Entity (N);

   begin
      Set_Etype (N, Any_Type);
      Candidate_Type := Empty;

      Analyze_Expression (R);

      if Present (Op_Id) then
         if Ekind (Op_Id) = E_Operator then
            Find_Unary_Types (R, Op_Id,  N);
         else
            Add_One_Interp (N, Op_Id, Etype (Op_Id));
         end if;

      else
         Op_Id := Get_Name_Entity_Id (Chars (N));
         while Present (Op_Id) loop
            if Ekind (Op_Id) = E_Operator then
               if No (Next_Entity (First_Entity (Op_Id))) then
                  Find_Unary_Types (R, Op_Id,  N);
               end if;

            elsif Is_Overloadable (Op_Id) then
               Analyze_User_Defined_Unary_Op (N, Op_Id);
            end if;

            Op_Id := Homonym (Op_Id);
         end loop;
      end if;

      Operator_Check (N);
   end Analyze_Unary_Op;

   ----------------------------------
   -- Analyze_Unchecked_Expression --
   ----------------------------------

   procedure Analyze_Unchecked_Expression (N : Node_Id) is
   begin
      Analyze (Expression (N), Suppress => All_Checks);
      Set_Etype (N, Etype (Expression (N)));
      Save_Interps (Expression (N), N);
   end Analyze_Unchecked_Expression;

   ---------------------------------------
   -- Analyze_Unchecked_Type_Conversion --
   ---------------------------------------

   procedure Analyze_Unchecked_Type_Conversion (N : Node_Id) is
   begin
      Find_Type (Subtype_Mark (N));
      Analyze_Expression (Expression (N));
      Set_Etype (N, Entity (Subtype_Mark (N)));
   end Analyze_Unchecked_Type_Conversion;

   ------------------------------------
   -- Analyze_User_Defined_Binary_Op --
   ------------------------------------

   procedure Analyze_User_Defined_Binary_Op
     (N     : Node_Id;
      Op_Id : Entity_Id)
   is
   begin
      --  Only do analysis if the operator Comes_From_Source, since otherwise
      --  the operator was generated by the expander, and all such operators
      --  always refer to the operators in package Standard.

      if Comes_From_Source (N) then
         declare
            F1 : constant Entity_Id := First_Formal (Op_Id);
            F2 : constant Entity_Id := Next_Formal (F1);

         begin
            --  Verify that Op_Id is a visible binary function. Note that since
            --  we know Op_Id is overloaded, potentially use visible means use
            --  visible for sure (RM 9.4(11)).

            if Ekind (Op_Id) = E_Function
              and then Present (F2)
              and then (Is_Immediately_Visible (Op_Id)
                         or else Is_Potentially_Use_Visible (Op_Id))
              and then Has_Compatible_Type (Left_Opnd (N), Etype (F1))
              and then Has_Compatible_Type (Right_Opnd (N), Etype (F2))
            then
               Add_One_Interp (N, Op_Id, Etype (Op_Id));

               if Debug_Flag_E then
                  Write_Str ("user defined operator ");
                  Write_Name (Chars (Op_Id));
                  Write_Str (" on node ");
                  Write_Int (Int (N));
                  Write_Eol;
               end if;
            end if;
         end;
      end if;
   end Analyze_User_Defined_Binary_Op;

   -----------------------------------
   -- Analyze_User_Defined_Unary_Op --
   -----------------------------------

   procedure Analyze_User_Defined_Unary_Op
     (N     : Node_Id;
      Op_Id : Entity_Id)
   is
   begin
      --  Only do analysis if the operator Comes_From_Source, since otherwise
      --  the operator was generated by the expander, and all such operators
      --  always refer to the operators in package Standard.

      if Comes_From_Source (N) then
         declare
            F : constant Entity_Id := First_Formal (Op_Id);

         begin
            --  Verify that Op_Id is a visible unary function. Note that since
            --  we know Op_Id is overloaded, potentially use visible means use
            --  visible for sure (RM 9.4(11)).

            if Ekind (Op_Id) = E_Function
              and then No (Next_Formal (F))
              and then (Is_Immediately_Visible (Op_Id)
                         or else Is_Potentially_Use_Visible (Op_Id))
              and then Has_Compatible_Type (Right_Opnd (N), Etype (F))
            then
               Add_One_Interp (N, Op_Id, Etype (Op_Id));
            end if;
         end;
      end if;
   end Analyze_User_Defined_Unary_Op;

   ---------------------------
   -- Check_Arithmetic_Pair --
   ---------------------------

   procedure Check_Arithmetic_Pair
     (T1, T2 : Entity_Id;
      Op_Id  : Entity_Id;
      N      : Node_Id)
   is
      Op_Name : constant Name_Id   := Chars (Op_Id);

      function Has_Fixed_Op (Typ : Entity_Id; Op : Entity_Id) return Boolean;
      --  Check whether the fixed-point type Typ has a user-defined operator
      --  (multiplication or division) that should hide the corresponding
      --  predefined operator. Used to implement Ada 2005 AI-264, to make
      --  such operators more visible and therefore useful.

      function Specific_Type (T1, T2 : Entity_Id) return Entity_Id;
      --  Get specific type (i.e. non-universal type if there is one)

      ------------------
      -- Has_Fixed_Op --
      ------------------

      function Has_Fixed_Op (Typ : Entity_Id; Op : Entity_Id) return Boolean is
         Ent : Entity_Id;
         F1  : Entity_Id;
         F2  : Entity_Id;

      begin
         --  The operation is treated as primitive if it is declared in the
         --  same scope as the type, and therefore on the same entity chain.

         Ent := Next_Entity (Typ);
         while Present (Ent) loop
            if Chars (Ent) = Chars (Op) then
               F1 := First_Formal (Ent);
               F2 := Next_Formal (F1);

               --  The operation counts as primitive if either operand or
               --  result are of the given type, and both operands are fixed
               --  point types.

               if (Etype (F1) = Typ
                    and then Is_Fixed_Point_Type (Etype (F2)))

                 or else
                   (Etype (F2) = Typ
                     and then Is_Fixed_Point_Type (Etype (F1)))

                 or else
                   (Etype (Ent) = Typ
                     and then Is_Fixed_Point_Type (Etype (F1))
                     and then Is_Fixed_Point_Type (Etype (F2)))
               then
                  return True;
               end if;
            end if;

            Next_Entity (Ent);
         end loop;

         return False;
      end Has_Fixed_Op;

      -------------------
      -- Specific_Type --
      -------------------

      function Specific_Type (T1, T2 : Entity_Id) return Entity_Id is
      begin
         if T1 = Universal_Integer or else T1 = Universal_Real then
            return Base_Type (T2);
         else
            return Base_Type (T1);
         end if;
      end Specific_Type;

   --  Start of processing for Check_Arithmetic_Pair

   begin
      if Op_Name = Name_Op_Add or else Op_Name = Name_Op_Subtract then

         if Is_Numeric_Type (T1)
           and then Is_Numeric_Type (T2)
           and then (Covers (T1, T2) or else Covers (T2, T1))
         then
            Add_One_Interp (N, Op_Id, Specific_Type (T1, T2));
         end if;

      elsif Op_Name = Name_Op_Multiply or else Op_Name = Name_Op_Divide then

         if Is_Fixed_Point_Type (T1)
           and then (Is_Fixed_Point_Type (T2)
                       or else T2 = Universal_Real)
         then
            --  If Treat_Fixed_As_Integer is set then the Etype is already set
            --  and no further processing is required (this is the case of an
            --  operator constructed by Exp_Fixd for a fixed point operation)
            --  Otherwise add one interpretation with universal fixed result
            --  If the operator is given in  functional notation, it comes
            --  from source and Fixed_As_Integer cannot apply.

            if (Nkind (N) not in N_Op
                 or else not Treat_Fixed_As_Integer (N))
              and then
                (not (Ada_Version >= Ada_05 and then Has_Fixed_Op (T1, Op_Id))
                  or else Nkind (Parent (N)) = N_Type_Conversion)
            then
               Add_One_Interp (N, Op_Id, Universal_Fixed);
            end if;

         elsif Is_Fixed_Point_Type (T2)
           and then (Nkind (N) not in N_Op
                      or else not Treat_Fixed_As_Integer (N))
           and then T1 = Universal_Real
           and then
             (not (Ada_Version >= Ada_05 and then Has_Fixed_Op (T1, Op_Id))
               or else Nkind (Parent (N)) = N_Type_Conversion)
         then
            Add_One_Interp (N, Op_Id, Universal_Fixed);

         elsif Is_Numeric_Type (T1)
           and then Is_Numeric_Type (T2)
           and then (Covers (T1, T2) or else Covers (T2, T1))
         then
            Add_One_Interp (N, Op_Id, Specific_Type (T1, T2));

         elsif Is_Fixed_Point_Type (T1)
           and then (Base_Type (T2) = Base_Type (Standard_Integer)
                       or else T2 = Universal_Integer)
         then
            Add_One_Interp (N, Op_Id, T1);

         elsif T2 = Universal_Real
           and then Base_Type (T1) = Base_Type (Standard_Integer)
           and then Op_Name = Name_Op_Multiply
         then
            Add_One_Interp (N, Op_Id, Any_Fixed);

         elsif T1 = Universal_Real
           and then Base_Type (T2) = Base_Type (Standard_Integer)
         then
            Add_One_Interp (N, Op_Id, Any_Fixed);

         elsif Is_Fixed_Point_Type (T2)
           and then (Base_Type (T1) = Base_Type (Standard_Integer)
                       or else T1 = Universal_Integer)
           and then Op_Name = Name_Op_Multiply
         then
            Add_One_Interp (N, Op_Id, T2);

         elsif T1 = Universal_Real and then T2 = Universal_Integer then
            Add_One_Interp (N, Op_Id, T1);

         elsif T2 = Universal_Real
           and then T1 = Universal_Integer
           and then Op_Name = Name_Op_Multiply
         then
            Add_One_Interp (N, Op_Id, T2);
         end if;

      elsif Op_Name = Name_Op_Mod or else Op_Name = Name_Op_Rem then

         --  Note: The fixed-point operands case with Treat_Fixed_As_Integer
         --  set does not require any special processing, since the Etype is
         --  already set (case of operation constructed by Exp_Fixed).

         if Is_Integer_Type (T1)
           and then (Covers (T1, T2) or else Covers (T2, T1))
         then
            Add_One_Interp (N, Op_Id, Specific_Type (T1, T2));
         end if;

      elsif Op_Name = Name_Op_Expon then
         if Is_Numeric_Type (T1)
           and then not Is_Fixed_Point_Type (T1)
           and then (Base_Type (T2) = Base_Type (Standard_Integer)
                      or else T2 = Universal_Integer)
         then
            Add_One_Interp (N, Op_Id, Base_Type (T1));
         end if;

      else pragma Assert (Nkind (N) in N_Op_Shift);

         --  If not one of the predefined operators, the node may be one
         --  of the intrinsic functions. Its kind is always specific, and
         --  we can use it directly, rather than the name of the operation.

         if Is_Integer_Type (T1)
           and then (Base_Type (T2) = Base_Type (Standard_Integer)
                      or else T2 = Universal_Integer)
         then
            Add_One_Interp (N, Op_Id, Base_Type (T1));
         end if;
      end if;
   end Check_Arithmetic_Pair;

   -------------------------------
   -- Check_Misspelled_Selector --
   -------------------------------

   procedure Check_Misspelled_Selector
     (Prefix : Entity_Id;
      Sel    : Node_Id)
   is
      Max_Suggestions   : constant := 2;
      Nr_Of_Suggestions : Natural := 0;

      Suggestion_1 : Entity_Id := Empty;
      Suggestion_2 : Entity_Id := Empty;

      Comp : Entity_Id;

   begin
      --  All the components of the prefix of selector Sel are matched
      --  against  Sel and a count is maintained of possible misspellings.
      --  When at the end of the analysis there are one or two (not more!)
      --  possible misspellings, these misspellings will be suggested as
      --  possible correction.

      if not (Is_Private_Type (Prefix) or else Is_Record_Type (Prefix)) then

         --  Concurrent types should be handled as well ???

         return;
      end if;

      Get_Name_String (Chars (Sel));

      declare
         S  : constant String (1 .. Name_Len) := Name_Buffer (1 .. Name_Len);

      begin
         Comp  := First_Entity (Prefix);
         while Nr_Of_Suggestions <= Max_Suggestions
            and then Present (Comp)
         loop
            if Is_Visible_Component (Comp) then
               Get_Name_String (Chars (Comp));

               if Is_Bad_Spelling_Of (Name_Buffer (1 .. Name_Len), S) then
                  Nr_Of_Suggestions := Nr_Of_Suggestions + 1;

                  case Nr_Of_Suggestions is
                     when 1      => Suggestion_1 := Comp;
                     when 2      => Suggestion_2 := Comp;
                     when others => exit;
                  end case;
               end if;
            end if;

            Comp := Next_Entity (Comp);
         end loop;

         --  Report at most two suggestions

         if Nr_Of_Suggestions = 1 then
            Error_Msg_NE ("\possible misspelling of&", Sel, Suggestion_1);

         elsif Nr_Of_Suggestions = 2 then
            Error_Msg_Node_2 := Suggestion_2;
            Error_Msg_NE ("\possible misspelling of& or&",
              Sel, Suggestion_1);
         end if;
      end;
   end Check_Misspelled_Selector;

   ----------------------
   -- Defined_In_Scope --
   ----------------------

   function Defined_In_Scope (T : Entity_Id; S : Entity_Id) return Boolean
   is
      S1 : constant Entity_Id := Scope (Base_Type (T));
   begin
      return S1 = S
        or else (S1 = System_Aux_Id and then S = Scope (S1));
   end Defined_In_Scope;

   -------------------
   -- Diagnose_Call --
   -------------------

   procedure Diagnose_Call (N : Node_Id; Nam : Node_Id) is
      Actual           : Node_Id;
      X                : Interp_Index;
      It               : Interp;
      Success          : Boolean;
      Err_Mode         : Boolean;
      New_Nam          : Node_Id;
      Void_Interp_Seen : Boolean := False;

   begin
      if Ada_Version >= Ada_05 then
         Actual := First_Actual (N);
         while Present (Actual) loop

            --  Ada 2005 (AI-50217): Post an error in case of premature
            --  usage of an entity from the limited view.

            if not Analyzed (Etype (Actual))
             and then From_With_Type (Etype (Actual))
            then
               Error_Msg_Qual_Level := 1;
               Error_Msg_NE
                ("missing with_clause for scope of imported type&",
                  Actual, Etype (Actual));
               Error_Msg_Qual_Level := 0;
            end if;

            Next_Actual (Actual);
         end loop;
      end if;

      --   Analyze each candidate call again, with full error reporting
      --   for each.

      Error_Msg_N
        ("no candidate interpretations match the actuals:!", Nam);
      Err_Mode := All_Errors_Mode;
      All_Errors_Mode := True;

      --  If this is a call to an operation of a concurrent type,
      --  the failed interpretations have been removed from the
      --  name. Recover them to provide full diagnostics.

      if Nkind (Parent (Nam)) = N_Selected_Component then
         Set_Entity (Nam, Empty);
         New_Nam := New_Copy_Tree (Parent (Nam));
         Set_Is_Overloaded (New_Nam, False);
         Set_Is_Overloaded (Selector_Name (New_Nam), False);
         Set_Parent (New_Nam, Parent (Parent (Nam)));
         Analyze_Selected_Component (New_Nam);
         Get_First_Interp (Selector_Name (New_Nam), X, It);
      else
         Get_First_Interp (Nam, X, It);
      end if;

      while Present (It.Nam) loop
         if Etype (It.Nam) = Standard_Void_Type then
            Void_Interp_Seen := True;
         end if;

         Analyze_One_Call (N, It.Nam, True, Success);
         Get_Next_Interp (X, It);
      end loop;

      if Nkind (N) = N_Function_Call then
         Get_First_Interp (Nam, X, It);
         while Present (It.Nam) loop
            if Ekind (It.Nam) = E_Function
              or else Ekind (It.Nam) = E_Operator
            then
               return;
            else
               Get_Next_Interp (X, It);
            end if;
         end loop;

         --  If all interpretations are procedures, this deserves a
         --  more precise message. Ditto if this appears as the prefix
         --  of a selected component, which may be a lexical error.

         Error_Msg_N
           ("\context requires function call, found procedure name", Nam);

         if Nkind (Parent (N)) = N_Selected_Component
           and then N = Prefix (Parent (N))
         then
            Error_Msg_N (
              "\period should probably be semicolon", Parent (N));
         end if;

      elsif Nkind (N) = N_Procedure_Call_Statement
        and then not Void_Interp_Seen
      then
         Error_Msg_N (
         "\function name found in procedure call", Nam);
      end if;

      All_Errors_Mode := Err_Mode;
   end Diagnose_Call;

   ---------------------------
   -- Find_Arithmetic_Types --
   ---------------------------

   procedure Find_Arithmetic_Types
     (L, R  : Node_Id;
      Op_Id : Entity_Id;
      N     : Node_Id)
   is
      Index1 : Interp_Index;
      Index2 : Interp_Index;
      It1    : Interp;
      It2    : Interp;

      procedure Check_Right_Argument (T : Entity_Id);
      --  Check right operand of operator

      --------------------------
      -- Check_Right_Argument --
      --------------------------

      procedure Check_Right_Argument (T : Entity_Id) is
      begin
         if not Is_Overloaded (R) then
            Check_Arithmetic_Pair (T, Etype (R), Op_Id,  N);
         else
            Get_First_Interp (R, Index2, It2);
            while Present (It2.Typ) loop
               Check_Arithmetic_Pair (T, It2.Typ, Op_Id, N);
               Get_Next_Interp (Index2, It2);
            end loop;
         end if;
      end Check_Right_Argument;

   --  Start processing for Find_Arithmetic_Types

   begin
      if not Is_Overloaded (L) then
         Check_Right_Argument (Etype (L));

      else
         Get_First_Interp (L, Index1, It1);

         while Present (It1.Typ) loop
            Check_Right_Argument (It1.Typ);
            Get_Next_Interp (Index1, It1);
         end loop;
      end if;

   end Find_Arithmetic_Types;

   ------------------------
   -- Find_Boolean_Types --
   ------------------------

   procedure Find_Boolean_Types
     (L, R  : Node_Id;
      Op_Id : Entity_Id;
      N     : Node_Id)
   is
      Index : Interp_Index;
      It    : Interp;

      procedure Check_Numeric_Argument (T : Entity_Id);
      --  Special case for logical operations one of whose operands is an
      --  integer literal. If both are literal the result is any modular type.

      ----------------------------
      -- Check_Numeric_Argument --
      ----------------------------

      procedure Check_Numeric_Argument (T : Entity_Id) is
      begin
         if T = Universal_Integer then
            Add_One_Interp (N, Op_Id, Any_Modular);

         elsif Is_Modular_Integer_Type (T) then
            Add_One_Interp (N, Op_Id, T);
         end if;
      end Check_Numeric_Argument;

   --  Start of processing for Find_Boolean_Types

   begin
      if not Is_Overloaded (L) then
         if Etype (L) = Universal_Integer
           or else Etype (L) = Any_Modular
         then
            if not Is_Overloaded (R) then
               Check_Numeric_Argument (Etype (R));

            else
               Get_First_Interp (R, Index, It);
               while Present (It.Typ) loop
                  Check_Numeric_Argument (It.Typ);
                  Get_Next_Interp (Index, It);
               end loop;
            end if;

         elsif Valid_Boolean_Arg (Etype (L))
           and then Has_Compatible_Type (R, Etype (L))
         then
            Add_One_Interp (N, Op_Id, Etype (L));
         end if;

      else
         Get_First_Interp (L, Index, It);
         while Present (It.Typ) loop
            if Valid_Boolean_Arg (It.Typ)
              and then Has_Compatible_Type (R, It.Typ)
            then
               Add_One_Interp (N, Op_Id, It.Typ);
            end if;

            Get_Next_Interp (Index, It);
         end loop;
      end if;
   end Find_Boolean_Types;

   ---------------------------
   -- Find_Comparison_Types --
   ---------------------------

   procedure Find_Comparison_Types
     (L, R  : Node_Id;
      Op_Id : Entity_Id;
      N     : Node_Id)
   is
      Index : Interp_Index;
      It    : Interp;
      Found : Boolean := False;
      I_F   : Interp_Index;
      T_F   : Entity_Id;
      Scop  : Entity_Id := Empty;

      procedure Try_One_Interp (T1 : Entity_Id);
      --  Routine to try one proposed interpretation. Note that the context
      --  of the operator plays no role in resolving the arguments, so that
      --  if there is more than one interpretation of the operands that is
      --  compatible with comparison, the operation is ambiguous.

      --------------------
      -- Try_One_Interp --
      --------------------

      procedure Try_One_Interp (T1 : Entity_Id) is
      begin

         --  If the operator is an expanded name, then the type of the operand
         --  must be defined in the corresponding scope. If the type is
         --  universal, the context will impose the correct type.

         if Present (Scop)
            and then not Defined_In_Scope (T1, Scop)
            and then T1 /= Universal_Integer
            and then T1 /= Universal_Real
            and then T1 /= Any_String
            and then T1 /= Any_Composite
         then
            return;
         end if;

         if Valid_Comparison_Arg (T1)
           and then Has_Compatible_Type (R, T1)
         then
            if Found
              and then Base_Type (T1) /= Base_Type (T_F)
            then
               It := Disambiguate (L, I_F, Index, Any_Type);

               if It = No_Interp then
                  Ambiguous_Operands (N);
                  Set_Etype (L, Any_Type);
                  return;

               else
                  T_F := It.Typ;
               end if;

            else
               Found := True;
               T_F   := T1;
               I_F   := Index;
            end if;

            Set_Etype (L, T_F);
            Find_Non_Universal_Interpretations (N, R, Op_Id, T1);

         end if;
      end Try_One_Interp;

   --  Start processing for Find_Comparison_Types

   begin
      --  If left operand is aggregate, the right operand has to
      --  provide a usable type for it.

      if Nkind (L) = N_Aggregate
        and then Nkind (R) /= N_Aggregate
      then
         Find_Comparison_Types (R, L, Op_Id, N);
         return;
      end if;

      if Nkind (N) = N_Function_Call
         and then Nkind (Name (N)) = N_Expanded_Name
      then
         Scop := Entity (Prefix (Name (N)));

         --  The prefix may be a package renaming, and the subsequent test
         --  requires the original package.

         if Ekind (Scop) = E_Package
           and then Present (Renamed_Entity (Scop))
         then
            Scop := Renamed_Entity (Scop);
            Set_Entity (Prefix (Name (N)), Scop);
         end if;
      end if;

      if not Is_Overloaded (L) then
         Try_One_Interp (Etype (L));

      else
         Get_First_Interp (L, Index, It);
         while Present (It.Typ) loop
            Try_One_Interp (It.Typ);
            Get_Next_Interp (Index, It);
         end loop;
      end if;
   end Find_Comparison_Types;

   ----------------------------------------
   -- Find_Non_Universal_Interpretations --
   ----------------------------------------

   procedure Find_Non_Universal_Interpretations
     (N     : Node_Id;
      R     : Node_Id;
      Op_Id : Entity_Id;
      T1    : Entity_Id)
   is
      Index : Interp_Index;
      It    : Interp;

   begin
      if T1 = Universal_Integer
        or else T1 = Universal_Real
      then
         if not Is_Overloaded (R) then
            Add_One_Interp
              (N, Op_Id, Standard_Boolean, Base_Type (Etype (R)));
         else
            Get_First_Interp (R, Index, It);
            while Present (It.Typ) loop
               if Covers (It.Typ, T1) then
                  Add_One_Interp
                    (N, Op_Id, Standard_Boolean, Base_Type (It.Typ));
               end if;

               Get_Next_Interp (Index, It);
            end loop;
         end if;
      else
         Add_One_Interp (N, Op_Id, Standard_Boolean, Base_Type (T1));
      end if;
   end Find_Non_Universal_Interpretations;

   ------------------------------
   -- Find_Concatenation_Types --
   ------------------------------

   procedure Find_Concatenation_Types
     (L, R  : Node_Id;
      Op_Id : Entity_Id;
      N     : Node_Id)
   is
      Op_Type : constant Entity_Id := Etype (Op_Id);

   begin
      if Is_Array_Type (Op_Type)
        and then not Is_Limited_Type (Op_Type)

        and then (Has_Compatible_Type (L, Op_Type)
                    or else
                  Has_Compatible_Type (L, Component_Type (Op_Type)))

        and then (Has_Compatible_Type (R, Op_Type)
                    or else
                  Has_Compatible_Type (R, Component_Type (Op_Type)))
      then
         Add_One_Interp (N, Op_Id, Op_Type);
      end if;
   end Find_Concatenation_Types;

   -------------------------
   -- Find_Equality_Types --
   -------------------------

   procedure Find_Equality_Types
     (L, R  : Node_Id;
      Op_Id : Entity_Id;
      N     : Node_Id)
   is
      Index : Interp_Index;
      It    : Interp;
      Found : Boolean := False;
      I_F   : Interp_Index;
      T_F   : Entity_Id;
      Scop  : Entity_Id := Empty;

      procedure Try_One_Interp (T1 : Entity_Id);
      --  The context of the operator plays no role in resolving the
      --  arguments,  so that if there is more than one interpretation
      --  of the operands that is compatible with equality, the construct
      --  is ambiguous and an error can be emitted now, after trying to
      --  disambiguate, i.e. applying preference rules.

      --------------------
      -- Try_One_Interp --
      --------------------

      procedure Try_One_Interp (T1 : Entity_Id) is
      begin
         --  If the operator is an expanded name, then the type of the operand
         --  must be defined in the corresponding scope. If the type is
         --  universal, the context will impose the correct type. An anonymous
         --  type for a 'Access reference is also universal in this sense, as
         --  the actual type is obtained from context.

         if Present (Scop)
            and then not Defined_In_Scope (T1, Scop)
            and then T1 /= Universal_Integer
            and then T1 /= Universal_Real
            and then T1 /= Any_Access
            and then T1 /= Any_String
            and then T1 /= Any_Composite
            and then (Ekind (T1) /= E_Access_Subprogram_Type
                        or else Comes_From_Source (T1))
         then
            return;
         end if;

         --  Ada 2005 (AI-230): Keep restriction imposed by Ada 83 and 95:
         --  Do not allow anonymous access types in equality operators.

         if Ada_Version < Ada_05
           and then Ekind (T1) = E_Anonymous_Access_Type
         then
            return;
         end if;

         if T1 /= Standard_Void_Type
           and then not Is_Limited_Type (T1)
           and then not Is_Limited_Composite (T1)
           and then Has_Compatible_Type (R, T1)
         then
            if Found
              and then Base_Type (T1) /= Base_Type (T_F)
            then
               It := Disambiguate (L, I_F, Index, Any_Type);

               if It = No_Interp then
                  Ambiguous_Operands (N);
                  Set_Etype (L, Any_Type);
                  return;

               else
                  T_F := It.Typ;
               end if;

            else
               Found := True;
               T_F   := T1;
               I_F   := Index;
            end if;

            if not Analyzed (L) then
               Set_Etype (L, T_F);
            end if;

            Find_Non_Universal_Interpretations (N, R, Op_Id, T1);

            --  Case of operator was not visible, Etype still set to Any_Type

            if Etype (N) = Any_Type then
               Found := False;
            end if;
         end if;
      end Try_One_Interp;

   --  Start of processing for Find_Equality_Types

   begin
      --  If left operand is aggregate, the right operand has to
      --  provide a usable type for it.

      if Nkind (L) = N_Aggregate
        and then Nkind (R) /= N_Aggregate
      then
         Find_Equality_Types (R, L, Op_Id, N);
         return;
      end if;

      if Nkind (N) = N_Function_Call
         and then Nkind (Name (N)) = N_Expanded_Name
      then
         Scop := Entity (Prefix (Name (N)));

         --  The prefix may be a package renaming, and the subsequent test
         --  requires the original package.

         if Ekind (Scop) = E_Package
           and then Present (Renamed_Entity (Scop))
         then
            Scop := Renamed_Entity (Scop);
            Set_Entity (Prefix (Name (N)), Scop);
         end if;
      end if;

      if not Is_Overloaded (L) then
         Try_One_Interp (Etype (L));

      else
         Get_First_Interp (L, Index, It);
         while Present (It.Typ) loop
            Try_One_Interp (It.Typ);
            Get_Next_Interp (Index, It);
         end loop;
      end if;
   end Find_Equality_Types;

   -------------------------
   -- Find_Negation_Types --
   -------------------------

   procedure Find_Negation_Types
     (R     : Node_Id;
      Op_Id : Entity_Id;
      N     : Node_Id)
   is
      Index : Interp_Index;
      It    : Interp;

   begin
      if not Is_Overloaded (R) then
         if Etype (R) = Universal_Integer then
            Add_One_Interp (N, Op_Id, Any_Modular);
         elsif Valid_Boolean_Arg (Etype (R)) then
            Add_One_Interp (N, Op_Id, Etype (R));
         end if;

      else
         Get_First_Interp (R, Index, It);
         while Present (It.Typ) loop
            if Valid_Boolean_Arg (It.Typ) then
               Add_One_Interp (N, Op_Id, It.Typ);
            end if;

            Get_Next_Interp (Index, It);
         end loop;
      end if;
   end Find_Negation_Types;

   ----------------------
   -- Find_Unary_Types --
   ----------------------

   procedure Find_Unary_Types
     (R     : Node_Id;
      Op_Id : Entity_Id;
      N     : Node_Id)
   is
      Index : Interp_Index;
      It    : Interp;

   begin
      if not Is_Overloaded (R) then
         if Is_Numeric_Type (Etype (R)) then
            Add_One_Interp (N, Op_Id, Base_Type (Etype (R)));
         end if;

      else
         Get_First_Interp (R, Index, It);
         while Present (It.Typ) loop
            if Is_Numeric_Type (It.Typ) then
               Add_One_Interp (N, Op_Id, Base_Type (It.Typ));
            end if;

            Get_Next_Interp (Index, It);
         end loop;
      end if;
   end Find_Unary_Types;

   ------------------
   -- Junk_Operand --
   ------------------

   function Junk_Operand (N : Node_Id) return Boolean is
      Enode : Node_Id;

   begin
      if Error_Posted (N) then
         return False;
      end if;

      --  Get entity to be tested

      if Is_Entity_Name (N)
        and then Present (Entity (N))
      then
         Enode := N;

      --  An odd case, a procedure name gets converted to a very peculiar
      --  function call, and here is where we detect this happening.

      elsif Nkind (N) = N_Function_Call
        and then Is_Entity_Name (Name (N))
        and then Present (Entity (Name (N)))
      then
         Enode := Name (N);

      --  Another odd case, there are at least some cases of selected
      --  components where the selected component is not marked as having
      --  an entity, even though the selector does have an entity

      elsif Nkind (N) = N_Selected_Component
        and then Present (Entity (Selector_Name (N)))
      then
         Enode := Selector_Name (N);

      else
         return False;
      end if;

      --  Now test the entity we got to see if it a bad case

      case Ekind (Entity (Enode)) is

         when E_Package =>
            Error_Msg_N
              ("package name cannot be used as operand", Enode);

         when Generic_Unit_Kind =>
            Error_Msg_N
              ("generic unit name cannot be used as operand", Enode);

         when Type_Kind =>
            Error_Msg_N
              ("subtype name cannot be used as operand", Enode);

         when Entry_Kind =>
            Error_Msg_N
              ("entry name cannot be used as operand", Enode);

         when E_Procedure =>
            Error_Msg_N
              ("procedure name cannot be used as operand", Enode);

         when E_Exception =>
            Error_Msg_N
              ("exception name cannot be used as operand", Enode);

         when E_Block | E_Label | E_Loop =>
            Error_Msg_N
              ("label name cannot be used as operand", Enode);

         when others =>
            return False;

      end case;

      return True;
   end Junk_Operand;

   --------------------
   -- Operator_Check --
   --------------------

   procedure Operator_Check (N : Node_Id) is
   begin
      Remove_Abstract_Operations (N);

      --  Test for case of no interpretation found for operator

      if Etype (N) = Any_Type then
         declare
            L : Node_Id;
            R : Node_Id;

         begin
            R := Right_Opnd (N);

            if Nkind (N) in N_Binary_Op then
               L := Left_Opnd (N);
            else
               L := Empty;
            end if;

            --  If either operand has no type, then don't complain further,
            --  since this simply means that we have a propragated error.

            if R = Error
              or else Etype (R) = Any_Type
              or else (Nkind (N) in N_Binary_Op and then Etype (L) = Any_Type)
            then
               return;

            --  We explicitly check for the case of concatenation of component
            --  with component to avoid reporting spurious matching array types
            --  that might happen to be lurking in distant packages (such as
            --  run-time packages). This also prevents inconsistencies in the
            --  messages for certain ACVC B tests, which can vary depending on
            --  types declared in run-time interfaces. Another improvement when
            --  aggregates are present is to look for a well-typed operand.

            elsif Present (Candidate_Type)
              and then (Nkind (N) /= N_Op_Concat
                         or else Is_Array_Type (Etype (L))
                         or else Is_Array_Type (Etype (R)))
            then

               if Nkind (N) = N_Op_Concat then
                  if Etype (L) /= Any_Composite
                    and then Is_Array_Type (Etype (L))
                  then
                     Candidate_Type := Etype (L);

                  elsif Etype (R) /= Any_Composite
                    and then Is_Array_Type (Etype (R))
                  then
                     Candidate_Type := Etype (R);
                  end if;
               end if;

               Error_Msg_NE
                 ("operator for} is not directly visible!",
                  N, First_Subtype (Candidate_Type));
               Error_Msg_N ("use clause would make operation legal!",  N);
               return;

            --  If either operand is a junk operand (e.g. package name), then
            --  post appropriate error messages, but do not complain further.

            --  Note that the use of OR in this test instead of OR ELSE
            --  is quite deliberate, we may as well check both operands
            --  in the binary operator case.

            elsif Junk_Operand (R)
              or (Nkind (N) in N_Binary_Op and then Junk_Operand (L))
            then
               return;

            --  If we have a logical operator, one of whose operands is
            --  Boolean, then we know that the other operand cannot resolve
            --  to Boolean (since we got no interpretations), but in that
            --  case we pretty much know that the other operand should be
            --  Boolean, so resolve it that way (generating an error)

            elsif Nkind (N) = N_Op_And
                    or else
                  Nkind (N) = N_Op_Or
                    or else
                  Nkind (N) = N_Op_Xor
            then
               if Etype (L) = Standard_Boolean then
                  Resolve (R, Standard_Boolean);
                  return;
               elsif Etype (R) = Standard_Boolean then
                  Resolve (L, Standard_Boolean);
                  return;
               end if;

            --  For an arithmetic operator or comparison operator, if one
            --  of the operands is numeric, then we know the other operand
            --  is not the same numeric type. If it is a non-numeric type,
            --  then probably it is intended to match the other operand.

            elsif Nkind (N) = N_Op_Add      or else
                  Nkind (N) = N_Op_Divide   or else
                  Nkind (N) = N_Op_Ge       or else
                  Nkind (N) = N_Op_Gt       or else
                  Nkind (N) = N_Op_Le       or else
                  Nkind (N) = N_Op_Lt       or else
                  Nkind (N) = N_Op_Mod      or else
                  Nkind (N) = N_Op_Multiply or else
                  Nkind (N) = N_Op_Rem      or else
                  Nkind (N) = N_Op_Subtract
            then
               if Is_Numeric_Type (Etype (L))
                 and then not Is_Numeric_Type (Etype (R))
               then
                  Resolve (R, Etype (L));
                  return;

               elsif Is_Numeric_Type (Etype (R))
                 and then not Is_Numeric_Type (Etype (L))
               then
                  Resolve (L, Etype (R));
                  return;
               end if;

            --  Comparisons on A'Access are common enough to deserve a
            --  special message.

            elsif (Nkind (N) = N_Op_Eq  or else
                   Nkind (N) = N_Op_Ne)
               and then Ekind (Etype (L)) = E_Access_Attribute_Type
               and then Ekind (Etype (R)) = E_Access_Attribute_Type
            then
               Error_Msg_N
                 ("two access attributes cannot be compared directly", N);
               Error_Msg_N
                 ("\they must be converted to an explicit type for comparison",
                   N);
               return;

            --  Another one for C programmers

            elsif Nkind (N) = N_Op_Concat
              and then Valid_Boolean_Arg (Etype (L))
              and then Valid_Boolean_Arg (Etype (R))
            then
               Error_Msg_N ("invalid operands for concatenation", N);
               Error_Msg_N ("\maybe AND was meant", N);
               return;

            --  A special case for comparison of access parameter with null

            elsif Nkind (N) = N_Op_Eq
              and then Is_Entity_Name (L)
              and then Nkind (Parent (Entity (L))) = N_Parameter_Specification
              and then Nkind (Parameter_Type (Parent (Entity (L)))) =
                                                  N_Access_Definition
              and then Nkind (R) = N_Null
            then
               Error_Msg_N ("access parameter is not allowed to be null", L);
               Error_Msg_N ("\(call would raise Constraint_Error)", L);
               return;
            end if;

            --  If we fall through then just give general message. Note
            --  that in the following messages, if the operand is overloaded
            --  we choose an arbitrary type to complain about, but that is
            --  probably more useful than not giving a type at all.

            if Nkind (N) in N_Unary_Op then
               Error_Msg_Node_2 := Etype (R);
               Error_Msg_N ("operator& not defined for}", N);
               return;

            else
               if Nkind (N) in N_Binary_Op then
                  if not Is_Overloaded (L)
                    and then not Is_Overloaded (R)
                    and then Base_Type (Etype (L)) = Base_Type (Etype (R))
                  then
                     Error_Msg_Node_2 := First_Subtype (Etype (R));
                     Error_Msg_N ("there is no applicable operator& for}", N);

                  else
                     Error_Msg_N ("invalid operand types for operator&", N);

                     if Nkind (N) /= N_Op_Concat then
                        Error_Msg_NE ("\left operand has}!",  N, Etype (L));
                        Error_Msg_NE ("\right operand has}!", N, Etype (R));
                     end if;
                  end if;
               end if;
            end if;
         end;
      end if;
   end Operator_Check;

   -----------------------------------------
   -- Process_Implicit_Dereference_Prefix --
   -----------------------------------------

   procedure Process_Implicit_Dereference_Prefix
     (E : Entity_Id;
      P : Entity_Id)
   is
      Ref : Node_Id;

   begin
      if Present (E)
        and then (Operating_Mode = Check_Semantics or else not Expander_Active)
      then
         --  We create a dummy reference to E to ensure that the reference
         --  is not considered as part of an assignment (an implicit
         --  dereference can never assign to its prefix). The Comes_From_Source
         --  attribute needs to be propagated for accurate warnings.

         Ref := New_Reference_To (E, Sloc (P));
         Set_Comes_From_Source (Ref, Comes_From_Source (P));
         Generate_Reference (E, Ref);
      end if;
   end Process_Implicit_Dereference_Prefix;

   --------------------------------
   -- Remove_Abstract_Operations --
   --------------------------------

   procedure Remove_Abstract_Operations (N : Node_Id) is
      I            : Interp_Index;
      It           : Interp;
      Abstract_Op  : Entity_Id := Empty;

      --  AI-310: If overloaded, remove abstract non-dispatching
      --  operations. We activate this if either extensions are
      --  enabled, or if the abstract operation in question comes
      --  from a predefined file. This latter test allows us to
      --  use abstract to make operations invisible to users. In
      --  particular, if type Address is non-private and abstract
      --  subprograms are used to hide its operators, they will be
      --  truly hidden.

      type Operand_Position is (First_Op, Second_Op);
      Univ_Type : constant Entity_Id := Universal_Interpretation (N);

      procedure Remove_Address_Interpretations (Op : Operand_Position);
      --  Ambiguities may arise when the operands are literal and the
      --  address operations in s-auxdec are visible. In that case, remove
      --  the interpretation of a literal as Address, to retain the semantics
      --  of Address as a private type.

      ------------------------------------
      -- Remove_Address_Interpretations --
      ------------------------------------

      procedure Remove_Address_Interpretations (Op : Operand_Position) is
         Formal : Entity_Id;

      begin
         if Is_Overloaded (N) then
            Get_First_Interp (N, I, It);
            while Present (It.Nam) loop
               Formal := First_Entity (It.Nam);

               if Op = Second_Op then
                  Formal := Next_Entity (Formal);
               end if;

               if Is_Descendent_Of_Address (Etype (Formal)) then
                  Remove_Interp (I);
               end if;

               Get_Next_Interp (I, It);
            end loop;
         end if;
      end Remove_Address_Interpretations;

   --  Start of processing for Remove_Abstract_Operations

   begin
      if Is_Overloaded (N) then
         Get_First_Interp (N, I, It);

         while Present (It.Nam) loop
            if not Is_Type (It.Nam)
              and then Is_Abstract (It.Nam)
              and then not Is_Dispatching_Operation (It.Nam)
              and then
                (Ada_Version >= Ada_05
                   or else Is_Predefined_File_Name
                             (Unit_File_Name (Get_Source_Unit (It.Nam))))

            then
               Abstract_Op := It.Nam;
               Remove_Interp (I);
               exit;
            end if;

            Get_Next_Interp (I, It);
         end loop;

         if No (Abstract_Op) then
            return;

         elsif Nkind (N) in N_Op then

            --  Remove interpretations that treat literals as addresses.
            --  This is never appropriate.

            if Nkind (N) in N_Binary_Op then
               declare
                  U1 : constant Boolean :=
                     Present (Universal_Interpretation (Right_Opnd (N)));
                  U2 : constant Boolean :=
                     Present (Universal_Interpretation (Left_Opnd (N)));

               begin
                  if U1 and then not U2 then
                     Remove_Address_Interpretations (Second_Op);

                  elsif U2 and then not U1 then
                     Remove_Address_Interpretations (First_Op);
                  end if;

                  if not (U1 and U2) then

                     --  Remove corresponding predefined operator, which is
                     --  always added to the overload set.

                     Get_First_Interp (N, I, It);
                     while Present (It.Nam) loop
                        if Scope (It.Nam) = Standard_Standard
                          and then Base_Type (It.Typ) =
                                   Base_Type (Etype (Abstract_Op))
                        then
                           Remove_Interp (I);
                        end if;

                        Get_Next_Interp (I, It);
                     end loop;

                  elsif Is_Overloaded (N)
                    and then Present (Univ_Type)
                  then
                     --  If both operands have a universal interpretation,
                     --  select the predefined operator and discard others.

                     Get_First_Interp (N, I, It);

                     while Present (It.Nam) loop
                        if Scope (It.Nam) = Standard_Standard then
                           Set_Etype (N, Univ_Type);
                           Set_Entity (N, It.Nam);
                           Set_Is_Overloaded (N, False);
                           exit;
                        end if;

                        Get_Next_Interp (I, It);
                     end loop;
                  end if;
               end;
            end if;

         elsif Nkind (N) = N_Function_Call
           and then
             (Nkind (Name (N)) = N_Operator_Symbol
                or else
                  (Nkind (Name (N)) = N_Expanded_Name
                     and then
                       Nkind (Selector_Name (Name (N))) = N_Operator_Symbol))
         then

            declare
               Arg1 : constant Node_Id := First (Parameter_Associations (N));
               U1   : constant Boolean :=
                        Present (Universal_Interpretation (Arg1));
               U2   : constant Boolean :=
                        Present (Next (Arg1)) and then
                        Present (Universal_Interpretation (Next (Arg1)));

            begin
               if U1 and then not U2 then
                  Remove_Address_Interpretations (First_Op);

               elsif U2 and then not U1 then
                  Remove_Address_Interpretations (Second_Op);
               end if;

               if not (U1 and U2) then
                  Get_First_Interp (N, I, It);
                  while Present (It.Nam) loop
                     if Scope (It.Nam) = Standard_Standard
                       and then It.Typ = Base_Type (Etype (Abstract_Op))
                     then
                        Remove_Interp (I);
                     end if;

                     Get_Next_Interp (I, It);
                  end loop;
               end if;
            end;
         end if;

         --  If the removal has left no valid interpretations, emit
         --  error message now and label node as illegal.

         if Present (Abstract_Op) then
            Get_First_Interp (N, I, It);

            if No (It.Nam) then

               --  Removal of abstract operation left no viable candidate

               Set_Etype (N, Any_Type);
               Error_Msg_Sloc := Sloc (Abstract_Op);
               Error_Msg_NE
                 ("cannot call abstract operation& declared#", N, Abstract_Op);
            end if;
         end if;
      end if;
   end Remove_Abstract_Operations;

   -----------------------
   -- Try_Indirect_Call --
   -----------------------

   function Try_Indirect_Call
     (N   : Node_Id;
      Nam : Entity_Id;
      Typ : Entity_Id) return Boolean
   is
      Actual  : Node_Id;
      Formal  : Entity_Id;
      Call_OK : Boolean;

   begin
      Normalize_Actuals (N, Designated_Type (Typ), False, Call_OK);
      Actual := First_Actual (N);
      Formal := First_Formal (Designated_Type (Typ));

      while Present (Actual)
        and then Present (Formal)
      loop
         if not Has_Compatible_Type (Actual, Etype (Formal)) then
            return False;
         end if;

         Next (Actual);
         Next_Formal (Formal);
      end loop;

      if No (Actual) and then No (Formal) then
         Add_One_Interp (N, Nam, Etype (Designated_Type (Typ)));

         --  Nam is a candidate interpretation for the name in the call,
         --  if it is not an indirect call.

         if not Is_Type (Nam)
            and then Is_Entity_Name (Name (N))
         then
            Set_Entity (Name (N), Nam);
         end if;

         return True;
      else
         return False;
      end if;
   end Try_Indirect_Call;

   ----------------------
   -- Try_Indexed_Call --
   ----------------------

   function Try_Indexed_Call
     (N   : Node_Id;
      Nam : Entity_Id;
      Typ : Entity_Id) return Boolean
   is
      Actuals : constant List_Id   := Parameter_Associations (N);
      Actual : Node_Id;
      Index  : Entity_Id;

   begin
      Actual := First (Actuals);
      Index := First_Index (Typ);
      while Present (Actual)
        and then Present (Index)
      loop
         --  If the parameter list has a named association, the expression
         --  is definitely a call and not an indexed component.

         if Nkind (Actual) = N_Parameter_Association then
            return False;
         end if;

         if not Has_Compatible_Type (Actual, Etype (Index)) then
            return False;
         end if;

         Next (Actual);
         Next_Index (Index);
      end loop;

      if No (Actual) and then No (Index) then
         Add_One_Interp (N, Nam, Component_Type (Typ));

         --  Nam is a candidate interpretation for the name in the call,
         --  if it is not an indirect call.

         if not Is_Type (Nam)
            and then Is_Entity_Name (Name (N))
         then
            Set_Entity (Name (N), Nam);
         end if;

         return True;
      else
         return False;
      end if;
   end Try_Indexed_Call;

   --------------------------
   -- Try_Object_Operation --
   --------------------------

   function Try_Object_Operation (N : Node_Id) return Boolean is
      K               : constant Node_Kind  := Nkind (Parent (N));
      Loc             : constant Source_Ptr := Sloc (N);
      Is_Subprg_Call  : constant Boolean    := K = N_Procedure_Call_Statement
                                                or else K = N_Function_Call;
      Obj             : constant Node_Id    := Prefix (N);
      Subprog         : constant Node_Id    := Selector_Name (N);

      Actual          : Node_Id;
      Call_Node       : Node_Id;
      Call_Node_Case  : Node_Id := Empty;
      First_Actual    : Node_Id;
      Node_To_Replace : Node_Id;
      Obj_Type        : Entity_Id := Etype (Obj);

      procedure Complete_Object_Operation
        (Call_Node       : Node_Id;
         Node_To_Replace : Node_Id;
         Subprog         : Node_Id);
      --  Set Subprog as the name of Call_Node, replace Node_To_Replace with
      --  Call_Node and reanalyze Node_To_Replace.

      procedure Transform_Object_Operation
        (Call_Node       : out Node_Id;
         First_Actual    : Node_Id;
         Node_To_Replace : out Node_Id;
         Subprog         : Node_Id);
      --  Transform Object.Operation (...) to Operation (Object, ...)
      --  Call_Node is the resulting subprogram call node, First_Actual is
      --  either the object Obj or an explicit dereference of Obj in certain
      --  cases, Node_To_Replace is either N or the parent of N, and Subprog
      --  is the subprogram we are trying to match.

      function Try_Class_Wide_Operation
        (Call_Node       : Node_Id;
         Node_To_Replace : Node_Id) return Boolean;
      --  Traverse all the ancestor types looking for a class-wide subprogram
      --  that matches Subprog.

      function Try_Primitive_Operation
        (Call_Node       : Node_Id;
         Node_To_Replace : Node_Id) return Boolean;
      --  Traverse the list of primitive subprograms looking for a subprogram
      --  than matches Subprog.

      -------------------------------
      -- Complete_Object_Operation --
      -------------------------------

      procedure Complete_Object_Operation
        (Call_Node       : Node_Id;
         Node_To_Replace : Node_Id;
         Subprog         : Node_Id)
      is
      begin
         Set_Name (Call_Node, New_Copy_Tree (Subprog));
         Set_Analyzed (Call_Node, False);
         Rewrite (Node_To_Replace, Call_Node);
         Analyze (Node_To_Replace);
      end Complete_Object_Operation;

      --------------------------------
      -- Transform_Object_Operation --
      --------------------------------

      procedure Transform_Object_Operation
        (Call_Node       : out Node_Id;
         First_Actual    : Node_Id;
         Node_To_Replace : out Node_Id;
         Subprog         : Node_Id)
      is
         Actuals     : List_Id;
         Parent_Node : constant Node_Id := Parent (N);

      begin
         Actuals := New_List (New_Copy_Tree (First_Actual));

         if (Nkind (Parent_Node) = N_Function_Call
               or else
             Nkind (Parent_Node) = N_Procedure_Call_Statement)

            --  Avoid recursive calls

           and then N /= First (Parameter_Associations (Parent_Node))
         then
            Node_To_Replace := Parent_Node;

            --  Copy list of actuals in full before attempting to resolve call.
            --  This is necessary to ensure that the chaining of named actuals
            --  that happens during matching is done on a separate copy.

            declare
               Actual : Node_Id;
            begin
               Actual := First (Parameter_Associations (Parent_Node));
               while Present (Actual) loop
                  declare
                     New_Actual : constant Node_Id := New_Copy_Tree (Actual);

                  begin
                     Append (New_Actual, Actuals);

                     if Nkind (Actual) = N_Function_Call
                       and then Is_Overloaded (Name (Actual))
                     then
                        Save_Interps (Name (Actual), Name (New_Actual));
                     end if;
                  end;

                  Next (Actual);
               end loop;
            end;

            if Nkind (Parent_Node) = N_Procedure_Call_Statement then
               Call_Node :=
                 Make_Procedure_Call_Statement (Loc,
                   Name => New_Copy_Tree (Subprog),
                   Parameter_Associations => Actuals);

            else
               pragma Assert (Nkind (Parent_Node) = N_Function_Call);

               Call_Node :=
                 Make_Function_Call (Loc,
                   Name => New_Copy_Tree (Subprog),
                   Parameter_Associations => Actuals);

            end if;

         --  Before analysis, the function call appears as an
         --  indexed component.

         elsif Nkind (Parent_Node) =  N_Indexed_Component then
            Node_To_Replace := Parent_Node;

            declare
               Actual : Node_Id;
               New_Act : Node_Id;
            begin
               Actual := First (Expressions (Parent_Node));
               while Present (Actual) loop
                  New_Act := New_Copy_Tree (Actual);
                  Analyze (New_Act);
                  Append (New_Act, Actuals);
                  Next (Actual);
               end loop;
            end;

            Call_Node :=
               Make_Function_Call (Loc,
                 Name => New_Copy_Tree (Subprog),
                 Parameter_Associations => Actuals);

         --  Parameterless call

         else
            Node_To_Replace := N;

            Call_Node :=
               Make_Function_Call (Loc,
                 Name => New_Copy_Tree (Subprog),
                 Parameter_Associations => Actuals);
         end if;
      end Transform_Object_Operation;

      ------------------------------
      -- Try_Class_Wide_Operation --
      ------------------------------

      function Try_Class_Wide_Operation
        (Call_Node       : Node_Id;
         Node_To_Replace : Node_Id) return Boolean
      is
         Anc_Type : Entity_Id;
         Dummy    : Node_Id;
         Hom      : Entity_Id;
         Hom_Ref  : Node_Id;
         Success  : Boolean;

      begin
         --  Loop through ancestor types, traverse their homonym chains and
         --  gather all interpretations of the subprogram.

         Anc_Type := Obj_Type;
         loop
            Hom := Current_Entity (Subprog);
            while Present (Hom) loop
               if (Ekind (Hom) = E_Procedure
                     or else
                   Ekind (Hom) = E_Function)
                 and then Present (First_Formal (Hom))
                 and then Etype (First_Formal (Hom)) =
                            Class_Wide_Type (Anc_Type)
               then
                  Hom_Ref := New_Reference_To (Hom, Loc);

                  --  When both the type of the object and the type of the
                  --  first formal of the primitive operation are tagged
                  --  access types, we use a node with the object as first
                  --  actual.

                  if Is_Access_Type (Etype (Obj))
                    and then Ekind (Etype (First_Formal (Hom))) =
                               E_Anonymous_Access_Type
                  then
                     --  Allocate the node only once

                     if not Present (Call_Node_Case) then
                        Analyze_Expression (Obj);
                        Set_Analyzed       (Obj);

                        Transform_Object_Operation (
                          Call_Node       => Call_Node_Case,
                          First_Actual    => Obj,
                          Node_To_Replace => Dummy,
                          Subprog         => Subprog);

                        Set_Etype (Call_Node_Case, Any_Type);
                        Set_Parent (Call_Node_Case, Parent (Node_To_Replace));
                     end if;

                     Set_Name (Call_Node_Case, Hom_Ref);

                     Analyze_One_Call (
                       N       => Call_Node_Case,
                       Nam     => Hom,
                       Report  => False,
                       Success => Success);

                     if Success then
                        Complete_Object_Operation (
                          Call_Node       => Call_Node_Case,
                          Node_To_Replace => Node_To_Replace,
                          Subprog         => Hom_Ref);

                        return True;
                     end if;

                  --  ??? comment required

                  else
                     Set_Name (Call_Node, Hom_Ref);

                     Analyze_One_Call (
                       N       => Call_Node,
                       Nam     => Hom,
                       Report  => False,
                       Success => Success);

                     if Success then
                        Complete_Object_Operation (
                          Call_Node       => Call_Node,
                          Node_To_Replace => Node_To_Replace,
                          Subprog         => Hom_Ref);

                        return True;
                     end if;
                  end if;
               end if;

               Hom := Homonym (Hom);
            end loop;

            --  Climb to ancestor type if there is one

            exit when Etype (Anc_Type) = Anc_Type;
            Anc_Type := Etype (Anc_Type);
         end loop;

         return False;
      end Try_Class_Wide_Operation;

      -----------------------------
      -- Try_Primitive_Operation --
      -----------------------------

      function Try_Primitive_Operation
        (Call_Node       : Node_Id;
         Node_To_Replace : Node_Id) return Boolean
      is
         Dummy       : Node_Id;
         Elmt        : Elmt_Id;
         Prim_Op     : Entity_Id;
         Prim_Op_Ref : Node_Id;
         Success     : Boolean;

      begin
         --  Look for the subprogram in the list of primitive operations

         Elmt := First_Elmt (Primitive_Operations (Obj_Type));
         while Present (Elmt) loop
            Prim_Op := Node (Elmt);

            if Chars (Prim_Op) = Chars (Subprog)
              and then Present (First_Formal (Prim_Op))
            then
               Prim_Op_Ref := New_Reference_To (Prim_Op, Loc);

               --  When both the type of the object and the type of the first
               --  formal of the primitive operation are tagged access types,
               --  we use a node with the object as first actual.

               if Is_Access_Type (Etype (Obj))
                 and then Ekind (Etype (First_Formal (Prim_Op))) =
                            E_Anonymous_Access_Type
               then
                  --  Allocate the node only once

                  if not Present (Call_Node_Case) then
                     Analyze_Expression (Obj);
                     Set_Analyzed       (Obj);

                     Transform_Object_Operation (
                       Call_Node       => Call_Node_Case,
                       First_Actual    => Obj,
                       Node_To_Replace => Dummy,
                       Subprog         => Subprog);

                     Set_Etype (Call_Node_Case, Any_Type);
                     Set_Parent (Call_Node_Case, Parent (Node_To_Replace));
                  end if;

                  Set_Name (Call_Node_Case, Prim_Op_Ref);

                  Analyze_One_Call (
                    N       => Call_Node_Case,
                    Nam     => Prim_Op,
                    Report  => False,
                    Success => Success);

                  if Success then
                     Complete_Object_Operation (
                       Call_Node       => Call_Node_Case,
                       Node_To_Replace => Node_To_Replace,
                       Subprog         => Prim_Op_Ref);

                     return True;
                  end if;

               --  Comment required ???

               else
                  Set_Name (Call_Node, Prim_Op_Ref);

                  Analyze_One_Call (
                    N       => Call_Node,
                    Nam     => Prim_Op,
                    Report  => False,
                    Success => Success);

                  if Success then
                     Complete_Object_Operation (
                       Call_Node       => Call_Node,
                       Node_To_Replace => Node_To_Replace,
                       Subprog         => Prim_Op_Ref);

                     return True;
                  end if;
               end if;
            end if;

            Next_Elmt (Elmt);
         end loop;

         return False;
      end Try_Primitive_Operation;

   --  Start of processing for Try_Object_Operation

   begin
      if Is_Access_Type (Obj_Type) then
         Obj_Type := Designated_Type (Obj_Type);
      end if;

      if Ekind (Obj_Type) = E_Private_Subtype then
         Obj_Type := Base_Type (Obj_Type);
      end if;

      if Is_Class_Wide_Type (Obj_Type) then
         Obj_Type := Etype (Class_Wide_Type (Obj_Type));
      end if;

      --  Analyze the actuals in case of subprogram call

      if Is_Subprg_Call and then N = Name (Parent (N)) then
         Actual := First (Parameter_Associations (Parent (N)));
         while Present (Actual) loop
            Analyze_Expression (Actual);
            Next (Actual);
         end loop;
      end if;

      --  If the object is of an Access type, explicit dereference is
      --  required.

      if Is_Access_Type (Etype (Obj)) then
         First_Actual :=
           Make_Explicit_Dereference (Sloc (Obj), Obj);
         Set_Etype (First_Actual, Obj_Type);
      else
         First_Actual := Obj;
      end if;

      Analyze_Expression (First_Actual);
      Set_Analyzed       (First_Actual);

      --  Build a subprogram call node

      Transform_Object_Operation (
        Call_Node       => Call_Node,
        First_Actual    => First_Actual,
        Node_To_Replace => Node_To_Replace,
        Subprog         => Subprog);

      Set_Etype (Call_Node, Any_Type);
      Set_Parent (Call_Node, Parent (Node_To_Replace));

      return
         Try_Primitive_Operation
           (Call_Node       => Call_Node,
            Node_To_Replace => Node_To_Replace)
        or else
         Try_Class_Wide_Operation
           (Call_Node       => Call_Node,
            Node_To_Replace => Node_To_Replace);
   end Try_Object_Operation;

end Sem_Ch4;
