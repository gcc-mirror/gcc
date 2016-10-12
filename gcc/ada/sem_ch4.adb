------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 4                               --
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
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Aspects;  use Aspects;
with Atree;    use Atree;
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
with Namet.Sp; use Namet.Sp;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Output;   use Output;
with Restrict; use Restrict;
with Rident;   use Rident;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Case; use Sem_Case;
with Sem_Cat;  use Sem_Cat;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Dim;  use Sem_Dim;
with Sem_Disp; use Sem_Disp;
with Sem_Dist; use Sem_Dist;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Type; use Sem_Type;
with Sem_Util; use Sem_Util;
with Sem_Warn; use Sem_Warn;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

package body Sem_Ch4 is

   --  Tables which speed up the identification of dangerous calls to Ada 2012
   --  functions with writable actuals (AI05-0144).

   --  The following table enumerates the Ada constructs which may evaluate in
   --  arbitrary order. It does not cover all the language constructs which can
   --  be evaluated in arbitrary order but the subset needed for AI05-0144.

   Has_Arbitrary_Evaluation_Order : constant array (Node_Kind) of Boolean :=
     (N_Aggregate                      => True,
      N_Assignment_Statement           => True,
      N_Entry_Call_Statement           => True,
      N_Extension_Aggregate            => True,
      N_Full_Type_Declaration          => True,
      N_Indexed_Component              => True,
      N_Object_Declaration             => True,
      N_Pragma                         => True,
      N_Range                          => True,
      N_Slice                          => True,
      N_Array_Type_Definition          => True,
      N_Membership_Test                => True,
      N_Binary_Op                      => True,
      N_Subprogram_Call                => True,
      others                           => False);

   --  The following table enumerates the nodes on which we stop climbing when
   --  locating the outermost Ada construct that can be evaluated in arbitrary
   --  order.

   Stop_Subtree_Climbing : constant array (Node_Kind) of Boolean :=
     (N_Aggregate                    => True,
      N_Assignment_Statement         => True,
      N_Entry_Call_Statement         => True,
      N_Extended_Return_Statement    => True,
      N_Extension_Aggregate          => True,
      N_Full_Type_Declaration        => True,
      N_Object_Declaration           => True,
      N_Object_Renaming_Declaration  => True,
      N_Package_Specification        => True,
      N_Pragma                       => True,
      N_Procedure_Call_Statement     => True,
      N_Simple_Return_Statement      => True,
      N_Has_Condition                => True,
      others                         => False);

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Analyze_Concatenation_Rest (N : Node_Id);
   --  Does the "rest" of the work of Analyze_Concatenation, after the left
   --  operand has been analyzed. See Analyze_Concatenation for details.

   procedure Analyze_Expression (N : Node_Id);
   --  For expressions that are not names, this is just a call to analyze. If
   --  the expression is a name, it may be a call to a parameterless function,
   --  and if so must be converted into an explicit call node and analyzed as
   --  such. This deproceduring must be done during the first pass of overload
   --  resolution, because otherwise a procedure call with overloaded actuals
   --  may fail to resolve.

   procedure Analyze_Operator_Call (N : Node_Id; Op_Id : Entity_Id);
   --  Analyze a call of the form "+"(x, y), etc. The prefix of the call is an
   --  operator name or an expanded name whose selector is an operator name,
   --  and one possible interpretation is as a predefined operator.

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
   --  For equality, membership, and comparison operators with overloaded
   --  arguments, list possible interpretations.

   procedure Analyze_One_Call
      (N          : Node_Id;
       Nam        : Entity_Id;
       Report     : Boolean;
       Success    : out Boolean;
       Skip_First : Boolean := False);
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
   --
   --  The flag Skip_First is used when analyzing a call that was rewritten
   --  from object notation. In this case the first actual may have to receive
   --  an explicit dereference, depending on the first formal of the operation
   --  being called. The caller will have verified that the object is legal
   --  for the call. If the remaining parameters match, the first parameter
   --  will rewritten as a dereference if needed, prior to completing analysis.

   procedure Check_Misspelled_Selector
     (Prefix : Entity_Id;
      Sel    : Node_Id);
   --  Give possible misspelling message if Sel seems likely to be a mis-
   --  spelling of one of the selectors of the Prefix. This is called by
   --  Analyze_Selected_Component after producing an invalid selector error
   --  message.

   function Defined_In_Scope (T : Entity_Id; S : Entity_Id) return Boolean;
   --  Verify that type T is declared in scope S. Used to find interpretations
   --  for operators given by expanded names. This is abstracted as a separate
   --  function to handle extensions to System, where S is System, but T is
   --  declared in the extension.

   procedure Find_Arithmetic_Types
     (L, R  : Node_Id;
      Op_Id : Entity_Id;
      N     : Node_Id);
   --  L and R are the operands of an arithmetic operator. Find consistent
   --  pairs of interpretations for L and R that have a numeric type consistent
   --  with the semantics of the operator.

   procedure Find_Comparison_Types
     (L, R  : Node_Id;
      Op_Id : Entity_Id;
      N     : Node_Id);
   --  L and R are operands of a comparison operator. Find consistent pairs of
   --  interpretations for L and R.

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
   --  a function call whose name is an operator designator. Any_Access, which
   --  is the initial type of the literal NULL, is a universal type for the
   --  purpose of this routine.

   function Find_Primitive_Operation (N : Node_Id) return Boolean;
   --  Find candidate interpretations for the name Obj.Proc when it appears
   --  in a subprogram renaming declaration.

   procedure Find_Unary_Types
     (R     : Node_Id;
      Op_Id : Entity_Id;
      N     : Node_Id);
   --  Unary arithmetic types: plus, minus, abs

   procedure Check_Arithmetic_Pair
     (T1, T2 : Entity_Id;
      Op_Id  : Entity_Id;
      N      : Node_Id);
   --  Subsidiary procedure to Find_Arithmetic_Types. T1 and T2 are valid types
   --  for left and right operand. Determine whether they constitute a valid
   --  pair for the given operator, and record the corresponding interpretation
   --  of the operator node. The node N may be an operator node (the usual
   --  case) or a function call whose prefix is an operator designator. In
   --  both cases Op_Id is the operator name itself.

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

   function Process_Implicit_Dereference_Prefix
     (E : Entity_Id;
      P : Node_Id) return Entity_Id;
   --  Called when P is the prefix of an implicit dereference, denoting an
   --  object E. The function returns the designated type of the prefix, taking
   --  into account that the designated type of an anonymous access type may be
   --  a limited view, when the non-limited view is visible.
   --
   --  If in semantics only mode (-gnatc or generic), the function also records
   --  that the prefix is a reference to E, if any. Normally, such a reference
   --  is generated only when the implicit dereference is expanded into an
   --  explicit one, but for consistency we must generate the reference when
   --  expansion is disabled as well.

   procedure Remove_Abstract_Operations (N : Node_Id);
   --  Ada 2005: implementation of AI-310. An abstract non-dispatching
   --  operation is not a candidate interpretation.

   function Try_Container_Indexing
     (N      : Node_Id;
      Prefix : Node_Id;
      Exprs  : List_Id) return Boolean;
   --  AI05-0139: Generalized indexing to support iterators over containers

   function Try_Indexed_Call
     (N          : Node_Id;
      Nam        : Entity_Id;
      Typ        : Entity_Id;
      Skip_First : Boolean) return Boolean;
   --  If a function has defaults for all its actuals, a call to it may in fact
   --  be an indexing on the result of the call. Try_Indexed_Call attempts the
   --  interpretation as an indexing, prior to analysis as a call. If both are
   --  possible, the node is overloaded with both interpretations (same symbol
   --  but two different types). If the call is written in prefix form, the
   --  prefix becomes the first parameter in the call, and only the remaining
   --  actuals must be checked for the presence of defaults.

   function Try_Indirect_Call
     (N   : Node_Id;
      Nam : Entity_Id;
      Typ : Entity_Id) return Boolean;
   --  Similarly, a function F that needs no actuals can return an access to a
   --  subprogram, and the call F (X) interpreted as F.all (X). In this case
   --  the call may be overloaded with both interpretations.

   function Try_Object_Operation
     (N            : Node_Id;
      CW_Test_Only : Boolean := False) return Boolean;
   --  Ada 2005 (AI-252): Support the object.operation notation. If node N
   --  is a call in this notation, it is transformed into a normal subprogram
   --  call where the prefix is a parameter, and True is returned. If node
   --  N is not of this form, it is unchanged, and False is returned. If
   --  CW_Test_Only is true then N is an N_Selected_Component node which
   --  is part of a call to an entry or procedure of a tagged concurrent
   --  type and this routine is invoked to search for class-wide subprograms
   --  conflicting with the target entity.

   procedure wpo (T : Entity_Id);
   pragma Warnings (Off, wpo);
   --  Used for debugging: obtain list of primitive operations even if
   --  type is not frozen and dispatch table is not built yet.

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

            elsif Ada_Version >= Ada_2012 then
               declare
                  It : Interp;
                  I  : Interp_Index;

               begin
                  Get_First_Interp (Opnd, I, It);
                  while Present (It.Nam) loop
                     if Has_Implicit_Dereference (It.Typ) then
                        Error_Msg_N
                          ("can be interpreted as implicit dereference", Opnd);
                        return;
                     end if;

                     Get_Next_Interp (I, It);
                  end loop;
               end;

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
      if Nkind (N) in N_Membership_Test then
         Error_Msg_N ("ambiguous operands for membership",  N);

      elsif Nkind_In (N, N_Op_Eq, N_Op_Ne) then
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
      E        : Node_Id             := Expression (N);
      Acc_Type : Entity_Id;
      Type_Id  : Entity_Id;
      P        : Node_Id;
      C        : Node_Id;
      Onode    : Node_Id;

   begin
      Check_SPARK_05_Restriction ("allocator is not allowed", N);

      --  Deal with allocator restrictions

      --  In accordance with H.4(7), the No_Allocators restriction only applies
      --  to user-written allocators. The same consideration applies to the
      --  No_Standard_Allocators_Before_Elaboration restriction.

      if Comes_From_Source (N) then
         Check_Restriction (No_Allocators, N);

         --  Processing for No_Standard_Allocators_After_Elaboration, loop to
         --  look at enclosing context, checking task/main subprogram case.

         C := N;
         P := Parent (C);
         while Present (P) loop

            --  For the task case we need a handled sequence of statements,
            --  where the occurrence of the allocator is within the statements
            --  and the parent is a task body

            if Nkind (P) = N_Handled_Sequence_Of_Statements
              and then Is_List_Member (C)
              and then List_Containing (C) = Statements (P)
            then
               Onode := Original_Node (Parent (P));

               --  Check for allocator within task body, this is a definite
               --  violation of No_Allocators_After_Elaboration we can detect
               --  at compile time.

               if Nkind (Onode) = N_Task_Body then
                  Check_Restriction
                    (No_Standard_Allocators_After_Elaboration, N);
                  exit;
               end if;
            end if;

            --  The other case is appearance in a subprogram body. This is
            --  a violation if this is a library level subprogram with no
            --  parameters. Note that this is now a static error even if the
            --  subprogram is not the main program (this is a change, in an
            --  earlier version only the main program was affected, and the
            --  check had to be done in the binder.

            if Nkind (P) = N_Subprogram_Body
              and then Nkind (Parent (P)) = N_Compilation_Unit
              and then No (Parameter_Specifications (Specification (P)))
            then
               Check_Restriction
                 (No_Standard_Allocators_After_Elaboration, N);
            end if;

            C := P;
            P := Parent (C);
         end loop;
      end if;

      --  Ada 2012 (AI05-0111-3): Analyze the subpool_specification, if
      --  any. The expected type for the name is any type. A non-overloading
      --  rule then requires it to be of a type descended from
      --  System.Storage_Pools.Subpools.Subpool_Handle.

      --  This isn't exactly what the AI says, but it seems to be the right
      --  rule. The AI should be fixed.???

      declare
         Subpool : constant Node_Id := Subpool_Handle_Name (N);

      begin
         if Present (Subpool) then
            Analyze (Subpool);

            if Is_Overloaded (Subpool) then
               Error_Msg_N ("ambiguous subpool handle", Subpool);
            end if;

            --  Check that Etype (Subpool) is descended from Subpool_Handle

            Resolve (Subpool);
         end if;
      end;

      --  Analyze the qualified expression or subtype indication

      if Nkind (E) = N_Qualified_Expression then
         Acc_Type := Create_Itype (E_Allocator_Type, N);
         Set_Etype (Acc_Type, Acc_Type);
         Find_Type (Subtype_Mark (E));

         --  Analyze the qualified expression, and apply the name resolution
         --  rule given in  4.7(3).

         Analyze (E);
         Type_Id := Etype (E);
         Set_Directly_Designated_Type (Acc_Type, Type_Id);

         --  A qualified expression requires an exact match of the type,
         --  class-wide matching is not allowed.

         --  if Is_Class_Wide_Type (Type_Id)
         --    and then Base_Type
         --       (Etype (Expression (E))) /= Base_Type (Type_Id)
         --  then
         --     Wrong_Type (Expression (E), Type_Id);
         --  end if;

         --  We don't analyze the qualified expression itself because it's
         --  part of the allocator. It is fully analyzed and resolved when
         --  the allocator is resolved with the context type.

         Set_Etype  (E, Type_Id);

      --  Case where allocator has a subtype indication

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

            --  Finally, if there is no subtype indication and the type is
            --  a tagged unconstrained type with discriminants, the designated
            --  object is constrained by their default values, and it is
            --  simplest to introduce an explicit constraint now. In some cases
            --  this is done during expansion, but freeze actions are certain
            --  to be emitted in the proper order if constraint is explicit.

            if Is_Entity_Name (E) and then Expander_Active then
               Find_Type (E);
               Type_Id := Entity (E);

               if Is_Tagged_Type (Type_Id)
                 and then Has_Discriminants (Type_Id)
                 and then not Is_Constrained (Type_Id)
                 and then
                   Present
                     (Discriminant_Default_Value
                       (First_Discriminant (Type_Id)))
               then
                  declare
                     Constr : constant List_Id    := New_List;
                     Loc    : constant Source_Ptr := Sloc (E);
                     Discr  : Entity_Id := First_Discriminant (Type_Id);

                  begin
                     if Present (Discriminant_Default_Value (Discr)) then
                        while Present (Discr) loop
                           Append (Discriminant_Default_Value (Discr), Constr);
                           Next_Discriminant (Discr);
                        end loop;

                        Rewrite (E,
                          Make_Subtype_Indication (Loc,
                            Subtype_Mark => New_Occurrence_Of (Type_Id, Loc),
                            Constraint   =>
                              Make_Index_Or_Discriminant_Constraint (Loc,
                                Constraints => Constr)));
                     end if;
                  end;
               end if;
            end if;

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

                     if Nkind (Constraint (E)) =
                          N_Index_Or_Discriminant_Constraint
                     then
                        Error_Msg_N -- CODEFIX
                          ("\if qualified expression was meant, " &
                              "use apostrophe", Constraint (E));
                     end if;
                  end if;

                  --  Get rid of the bogus constraint:

                  Rewrite (E, New_Copy_Tree (Subtype_Mark (E)));
                  Analyze_Allocator (N);
                  return;
               end if;

               if Expander_Active then
                  Def_Id := Make_Temporary (Loc, 'S');

                  Insert_Action (E,
                    Make_Subtype_Declaration (Loc,
                      Defining_Identifier => Def_Id,
                      Subtype_Indication  => Relocate_Node (E)));

                  if Sav_Errs /= Serious_Errors_Detected
                    and then Nkind (Constraint (E)) =
                               N_Index_Or_Discriminant_Constraint
                  then
                     Error_Msg_N -- CODEFIX
                       ("if qualified expression was meant, "
                        & "use apostrophe!", Constraint (E));
                  end if;

                  E := New_Occurrence_Of (Def_Id, Loc);
                  Rewrite (Expression (N), E);
               end if;
            end if;

            Type_Id := Process_Subtype (E, N);
            Acc_Type := Create_Itype (E_Allocator_Type, N);
            Set_Etype (Acc_Type, Acc_Type);
            Set_Directly_Designated_Type (Acc_Type, Type_Id);
            Check_Fully_Declared (Type_Id, N);

            --  Ada 2005 (AI-231): If the designated type is itself an access
            --  type that excludes null, its default initialization will
            --  be a null object, and we can insert an unconditional raise
            --  before the allocator.

            --  Ada 2012 (AI-104): A not null indication here is altogether
            --  illegal.

            if Can_Never_Be_Null (Type_Id) then
               declare
                  Not_Null_Check : constant Node_Id :=
                                     Make_Raise_Constraint_Error (Sloc (E),
                                       Reason => CE_Null_Not_Allowed);

               begin
                  if Expander_Active then
                     Insert_Action (N, Not_Null_Check);
                     Analyze (Not_Null_Check);

                  elsif Warn_On_Ada_2012_Compatibility then
                     Error_Msg_N
                       ("null value not allowed here in Ada 2012?y?", E);
                  end if;
               end;
            end if;

            --  Check for missing initialization. Skip this check if we already
            --  had errors on analyzing the allocator, since in that case these
            --  are probably cascaded errors.

            if not Is_Definite_Subtype (Type_Id)
              and then Serious_Errors_Detected = Sav_Errs
            then
               --  The build-in-place machinery may produce an allocator when
               --  the designated type is indefinite but the underlying type is
               --  not. In this case the unknown discriminants are meaningless
               --  and should not trigger error messages. Check the parent node
               --  because the allocator is marked as coming from source.

               if Present (Underlying_Type (Type_Id))
                 and then Is_Definite_Subtype (Underlying_Type (Type_Id))
                 and then not Comes_From_Source (Parent (N))
               then
                  null;

               elsif Is_Class_Wide_Type (Type_Id) then
                  Error_Msg_N
                    ("initialization required in class-wide allocation", N);

               else
                  if Ada_Version < Ada_2005
                    and then Is_Limited_Type (Type_Id)
                  then
                     Error_Msg_N ("unconstrained allocation not allowed", N);

                     if Is_Array_Type (Type_Id) then
                        Error_Msg_N
                          ("\constraint with array bounds required", N);

                     elsif Has_Unknown_Discriminants (Type_Id) then
                        null;

                     else pragma Assert (Has_Discriminants (Type_Id));
                        Error_Msg_N
                          ("\constraint with discriminant values required", N);
                     end if;

                  --  Limited Ada 2005 and general non-limited case

                  else
                     Error_Msg_N
                       ("uninitialized unconstrained allocation not "
                        & "allowed", N);

                     if Is_Array_Type (Type_Id) then
                        Error_Msg_N
                          ("\qualified expression or constraint with "
                           & "array bounds required", N);

                     elsif Has_Unknown_Discriminants (Type_Id) then
                        Error_Msg_N ("\qualified expression required", N);

                     else pragma Assert (Has_Discriminants (Type_Id));
                        Error_Msg_N
                          ("\qualified expression or constraint with "
                           & "discriminant values required", N);
                     end if;
                  end if;
               end if;
            end if;
         end;
      end if;

      if Is_Abstract_Type (Type_Id) then
         Error_Msg_N ("cannot allocate abstract object", E);
      end if;

      if Has_Task (Designated_Type (Acc_Type)) then
         Check_Restriction (No_Tasking, N);
         Check_Restriction (Max_Tasks, N);
         Check_Restriction (No_Task_Allocators, N);
      end if;

      --  Check restriction against dynamically allocated protected objects

      if Has_Protected (Designated_Type (Acc_Type)) then
         Check_Restriction (No_Protected_Type_Allocators, N);
      end if;

      --  AI05-0013-1: No_Nested_Finalization forbids allocators if the access
      --  type is nested, and the designated type needs finalization. The rule
      --  is conservative in that class-wide types need finalization.

      if Needs_Finalization (Designated_Type (Acc_Type))
        and then not Is_Library_Level_Entity (Acc_Type)
      then
         Check_Restriction (No_Nested_Finalization, N);
      end if;

      --  Check that an allocator of a nested access type doesn't create a
      --  protected object when restriction No_Local_Protected_Objects applies.

      if Has_Protected (Designated_Type (Acc_Type))
        and then not Is_Library_Level_Entity (Acc_Type)
      then
         Check_Restriction (No_Local_Protected_Objects, N);
      end if;

      --  Likewise for No_Local_Timing_Events

      if Has_Timing_Event (Designated_Type (Acc_Type))
        and then not Is_Library_Level_Entity (Acc_Type)
      then
         Check_Restriction (No_Local_Timing_Events, N);
      end if;

      --  If the No_Streams restriction is set, check that the type of the
      --  object is not, and does not contain, any subtype derived from
      --  Ada.Streams.Root_Stream_Type. Note that we guard the call to
      --  Has_Stream just for efficiency reasons. There is no point in
      --  spending time on a Has_Stream check if the restriction is not set.

      if Restriction_Check_Required (No_Streams) then
         if Has_Stream (Designated_Type (Acc_Type)) then
            Check_Restriction (No_Streams, N);
         end if;
      end if;

      Set_Etype (N, Acc_Type);

      if not Is_Library_Level_Entity (Acc_Type) then
         Check_Restriction (No_Local_Allocators, N);
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

      --  If the entity is already set, the node is the instantiation of a
      --  generic node with a non-local reference, or was manufactured by a
      --  call to Make_Op_xxx. In either case the entity is known to be valid,
      --  and we do not need to collect interpretations, instead we just get
      --  the single possible interpretation.

      Op_Id := Entity (N);

      if Present (Op_Id) then
         if Ekind (Op_Id) = E_Operator then

            if Nkind_In (N, N_Op_Divide, N_Op_Mod, N_Op_Multiply, N_Op_Rem)
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
         Set_Etype (N, Any_Type);

         Op_Id := Get_Name_Entity_Id (Chars (N));
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
      Check_Function_Writable_Actuals (N);
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
      Nam     : Node_Id;
      X       : Interp_Index;
      It      : Interp;
      Nam_Ent : Entity_Id;
      Success : Boolean := False;

      Deref : Boolean := False;
      --  Flag indicates whether an interpretation of the prefix is a
      --  parameterless call that returns an access_to_subprogram.

      procedure Check_Mixed_Parameter_And_Named_Associations;
      --  Check that parameter and named associations are not mixed. This is
      --  a restriction in SPARK mode.

      procedure Check_Writable_Actuals (N : Node_Id);
      --  If the call has out or in-out parameters then mark its outermost
      --  enclosing construct as a node on which the writable actuals check
      --  must be performed.

      function Name_Denotes_Function return Boolean;
      --  If the type of the name is an access to subprogram, this may be the
      --  type of a name, or the return type of the function being called. If
      --  the name is not an entity then it can denote a protected function.
      --  Until we distinguish Etype from Return_Type, we must use this routine
      --  to resolve the meaning of the name in the call.

      procedure No_Interpretation;
      --  Output error message when no valid interpretation exists

      --------------------------------------------------
      -- Check_Mixed_Parameter_And_Named_Associations --
      --------------------------------------------------

      procedure Check_Mixed_Parameter_And_Named_Associations is
         Actual     : Node_Id;
         Named_Seen : Boolean;

      begin
         Named_Seen := False;

         Actual := First (Actuals);
         while Present (Actual) loop
            case Nkind (Actual) is
               when N_Parameter_Association =>
                  if Named_Seen then
                     Check_SPARK_05_Restriction
                       ("named association cannot follow positional one",
                        Actual);
                     exit;
                  end if;

               when others =>
                  Named_Seen := True;
            end case;

            Next (Actual);
         end loop;
      end Check_Mixed_Parameter_And_Named_Associations;

      ----------------------------
      -- Check_Writable_Actuals --
      ----------------------------

      --  The identification of conflicts in calls to functions with writable
      --  actuals is performed in the analysis phase of the front end to ensure
      --  that it reports exactly the same errors compiling with and without
      --  expansion enabled. It is performed in two stages:

      --    1) When a call to a function with out-mode parameters is found,
      --       we climb to the outermost enclosing construct that can be
      --       evaluated in arbitrary order and we mark it with the flag
      --       Check_Actuals.

      --    2) When the analysis of the marked node is complete, we traverse
      --       its decorated subtree searching for conflicts (see function
      --       Sem_Util.Check_Function_Writable_Actuals).

      --  The unique exception to this general rule is for aggregates, since
      --  their analysis is performed by the front end in the resolution
      --  phase. For aggregates we do not climb to their enclosing construct:
      --  we restrict the analysis to the subexpressions initializing the
      --  aggregate components.

      --  This implies that the analysis of expressions containing aggregates
      --  is not complete, since there may be conflicts on writable actuals
      --  involving subexpressions of the enclosing logical or arithmetic
      --  expressions. However, we cannot wait and perform the analysis when
      --  the whole subtree is resolved, since the subtrees may be transformed,
      --  thus adding extra complexity and computation cost to identify and
      --  report exactly the same errors compiling with and without expansion
      --  enabled.

      procedure Check_Writable_Actuals (N : Node_Id) is
      begin
         if Comes_From_Source (N)
           and then Present (Get_Subprogram_Entity (N))
           and then Has_Out_Or_In_Out_Parameter (Get_Subprogram_Entity (N))
         then
            --  For procedures and entries there is no need to climb since
            --  we only need to check if the actuals of this call invoke
            --  functions whose out-mode parameters overlap.

            if Nkind (N) /= N_Function_Call then
               Set_Check_Actuals (N);

            --  For calls to functions we climb to the outermost enclosing
            --  construct where the out-mode actuals of this function may
            --  introduce conflicts.

            else
               declare
                  Outermost : Node_Id;
                  P         : Node_Id := N;

               begin
                  while Present (P) loop

                     --  For object declarations we can climb to the node from
                     --  its object definition branch or from its initializing
                     --  expression. We prefer to mark the child node as the
                     --  outermost construct to avoid adding further complexity
                     --  to the routine that will later take care of
                     --  performing the writable actuals check.

                     if Has_Arbitrary_Evaluation_Order (Nkind (P))
                       and then not Nkind_In (P, N_Assignment_Statement,
                                                 N_Object_Declaration)
                     then
                        Outermost := P;
                     end if;

                     --  Avoid climbing more than needed!

                     exit when Stop_Subtree_Climbing (Nkind (P))
                       or else (Nkind (P) = N_Range
                                 and then not
                                   Nkind_In (Parent (P), N_In, N_Not_In));

                     P := Parent (P);
                  end loop;

                  Set_Check_Actuals (Outermost);
               end;
            end if;
         end if;
      end Check_Writable_Actuals;

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

      -----------------------
      -- No_Interpretation --
      -----------------------

      procedure No_Interpretation is
         L : constant Boolean   := Is_List_Member (N);
         K : constant Node_Kind := Nkind (Parent (N));

      begin
         --  If the node is in a list whose parent is not an expression then it
         --  must be an attempted procedure call.

         if L and then K not in N_Subexpr then
            if Ekind (Entity (Nam)) = E_Generic_Procedure then
               Error_Msg_NE
                 ("must instantiate generic procedure& before call",
                  Nam, Entity (Nam));
            else
               Error_Msg_N ("procedure or entry name expected", Nam);
            end if;

         --  Check for tasking cases where only an entry call will do

         elsif not L
           and then Nkind_In (K, N_Entry_Call_Alternative,
                                 N_Triggering_Alternative)
         then
            Error_Msg_N ("entry name expected", Nam);

         --  Otherwise give general error message

         else
            Error_Msg_N ("invalid prefix in call", Nam);
         end if;
      end No_Interpretation;

   --  Start of processing for Analyze_Call

   begin
      if Restriction_Check_Required (SPARK_05) then
         Check_Mixed_Parameter_And_Named_Associations;
      end if;

      --  Initialize the type of the result of the call to the error type,
      --  which will be reset if the type is successfully resolved.

      Set_Etype (N, Any_Type);

      Nam := Name (N);

      if not Is_Overloaded (Nam) then

         --  Only one interpretation to check

         if Ekind (Etype (Nam)) = E_Subprogram_Type then
            Nam_Ent := Etype (Nam);

         --  If the prefix is an access_to_subprogram, this may be an indirect
         --  call. This is the case if the name in the call is not an entity
         --  name, or if it is a function name in the context of a procedure
         --  call. In this latter case, we have a call to a parameterless
         --  function that returns a pointer_to_procedure which is the entity
         --  being called. Finally, F (X) may be a call to a parameterless
         --  function that returns a pointer to a function with parameters.
         --  Note that if F returns an access-to-subprogram whose designated
         --  type is an array, F (X) cannot be interpreted as an indirect call
         --  through the result of the call to F.

         elsif Is_Access_Type (Etype (Nam))
           and then Ekind (Designated_Type (Etype (Nam))) = E_Subprogram_Type
           and then
             (not Name_Denotes_Function
               or else Nkind (N) = N_Procedure_Call_Statement
               or else
                 (Nkind (Parent (N)) /= N_Explicit_Dereference
                   and then Is_Entity_Name (Nam)
                   and then No (First_Formal (Entity (Nam)))
                   and then not
                     Is_Array_Type (Etype (Designated_Type (Etype (Nam))))
                   and then Present (Actuals)))
         then
            Nam_Ent := Designated_Type (Etype (Nam));
            Insert_Explicit_Dereference (Nam);

         --  Selected component case. Simple entry or protected operation,
         --  where the entry name is given by the selector name.

         elsif Nkind (Nam) = N_Selected_Component then
            Nam_Ent := Entity (Selector_Name (Nam));

            if not Ekind_In (Nam_Ent, E_Entry,
                                      E_Entry_Family,
                                      E_Function,
                                      E_Procedure)
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

            --  If not overloadable, this may be a generalized indexing
            --  operation with named associations. Rewrite again as an
            --  indexed component and analyze as container indexing.

            if not Is_Overloadable (Nam_Ent) then
               if Present
                    (Find_Value_Of_Aspect
                       (Etype (Nam_Ent), Aspect_Constant_Indexing))
               then
                  Replace (N,
                    Make_Indexed_Component (Sloc (N),
                      Prefix      => Nam,
                      Expressions => Parameter_Associations (N)));

                  if Try_Container_Indexing (N, Nam, Expressions (N)) then
                     return;
                  else
                     No_Interpretation;
                  end if;

               else
                  No_Interpretation;
               end if;

               return;
            end if;
         end if;

         --  Operations generated for RACW stub types are called only through
         --  dispatching, and can never be the static interpretation of a call.

         if Is_RACW_Stub_Type_Operation (Nam_Ent) then
            No_Interpretation;
            return;
         end if;

         Analyze_One_Call (N, Nam_Ent, True, Success);

         --  If this is an indirect call, the return type of the access_to
         --  subprogram may be an incomplete type. At the point of the call,
         --  use the full type if available, and at the same time update the
         --  return type of the access_to_subprogram.

         if Success
           and then Nkind (Nam) = N_Explicit_Dereference
           and then Ekind (Etype (N)) = E_Incomplete_Type
           and then Present (Full_View (Etype (N)))
         then
            Set_Etype (N, Full_View (Etype (N)));
            Set_Etype (Nam_Ent, Etype (N));
         end if;

      --  Overloaded call

      else
         --  An overloaded selected component must denote overloaded operations
         --  of a concurrent type. The interpretations are attached to the
         --  simple name of those operations.

         if Nkind (Nam) = N_Selected_Component then
            Nam := Selector_Name (Nam);
         end if;

         Get_First_Interp (Nam, X, It);
         while Present (It.Nam) loop
            Nam_Ent := It.Nam;
            Deref   := False;

            --  Name may be call that returns an access to subprogram, or more
            --  generally an overloaded expression one of whose interpretations
            --  yields an access to subprogram. If the name is an entity, we do
            --  not dereference, because the node is a call that returns the
            --  access type: note difference between f(x), where the call may
            --  return an access subprogram type, and f(x)(y), where the type
            --  returned by the call to f is implicitly dereferenced to analyze
            --  the outer call.

            if Is_Access_Type (Nam_Ent) then
               Nam_Ent := Designated_Type (Nam_Ent);

            elsif Is_Access_Type (Etype (Nam_Ent))
              and then
                (not Is_Entity_Name (Nam)
                   or else Nkind (N) = N_Procedure_Call_Statement)
              and then Ekind (Designated_Type (Etype (Nam_Ent)))
                                                          = E_Subprogram_Type
            then
               Nam_Ent := Designated_Type (Etype (Nam_Ent));

               if Is_Entity_Name (Nam) then
                  Deref := True;
               end if;
            end if;

            --  If the call has been rewritten from a prefixed call, the first
            --  parameter has been analyzed, but may need a subsequent
            --  dereference, so skip its analysis now.

            if N /= Original_Node (N)
              and then Nkind (Original_Node (N)) = Nkind (N)
              and then Nkind (Name (N)) /= Nkind (Name (Original_Node (N)))
              and then Present (Parameter_Associations (N))
              and then Present (Etype (First (Parameter_Associations (N))))
            then
               Analyze_One_Call
                 (N, Nam_Ent, False, Success, Skip_First => True);
            else
               Analyze_One_Call (N, Nam_Ent, False, Success);
            end if;

            --  If the interpretation succeeds, mark the proper type of the
            --  prefix (any valid candidate will do). If not, remove the
            --  candidate interpretation. This only needs to be done for
            --  overloaded protected operations, for other entities disambi-
            --  guation is done directly in Resolve.

            if Success then
               if Deref
                 and then Nkind (Parent (N)) /= N_Explicit_Dereference
               then
                  Set_Entity (Nam, It.Nam);
                  Insert_Explicit_Dereference (Nam);
                  Set_Etype (Nam, Nam_Ent);

               else
                  Set_Etype (Nam, It.Typ);
               end if;

            elsif Nkind_In (Name (N), N_Selected_Component,
                                      N_Function_Call)
            then
               Remove_Interp (X);
            end if;

            Get_Next_Interp (X, It);
         end loop;

         --  If the name is the result of a function call, it can only be a
         --  call to a function returning an access to subprogram. Insert
         --  explicit dereference.

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
                       ("possible missing instantiation of "
                        & "'Text_'I'O.'Integer_'I'O!", Nam);

                  elsif Is_Modular_Integer_Type (Typ) then
                     Error_Msg_N
                       ("possible missing instantiation of "
                        & "'Text_'I'O.'Modular_'I'O!", Nam);

                  elsif Is_Floating_Point_Type (Typ) then
                     Error_Msg_N
                       ("possible missing instantiation of "
                        & "'Text_'I'O.'Float_'I'O!", Nam);

                  elsif Is_Ordinary_Fixed_Point_Type (Typ) then
                     Error_Msg_N
                       ("possible missing instantiation of "
                        & "'Text_'I'O.'Fixed_'I'O!", Nam);

                  elsif Is_Decimal_Fixed_Point_Type (Typ) then
                     Error_Msg_N
                       ("possible missing instantiation of "
                        & "'Text_'I'O.'Decimal_'I'O!", Nam);

                  elsif Is_Enumeration_Type (Typ) then
                     Error_Msg_N
                       ("possible missing instantiation of "
                        & "'Text_'I'O.'Enumeration_'I'O!", Nam);
                  end if;
               end;
            end if;

         elsif not Is_Overloaded (N)
           and then Is_Entity_Name (Nam)
         then
            --  Resolution yields a single interpretation. Verify that the
            --  reference has capitalization consistent with the declaration.

            Set_Entity_With_Checks (Nam, Entity (Nam));
            Generate_Reference (Entity (Nam), Nam);

            Set_Etype (Nam, Etype (Entity (Nam)));
         else
            Remove_Abstract_Operations (N);
         end if;

         End_Interp_List;
      end if;

      if Ada_Version >= Ada_2012 then

         --  Check if the call contains a function with writable actuals

         Check_Writable_Actuals (N);

         --  If found and the outermost construct that can be evaluated in
         --  an arbitrary order is precisely this call, then check all its
         --  actuals.

         Check_Function_Writable_Actuals (N);
      end if;
   end Analyze_Call;

   -----------------------------
   -- Analyze_Case_Expression --
   -----------------------------

   procedure Analyze_Case_Expression (N : Node_Id) is
      procedure Non_Static_Choice_Error (Choice : Node_Id);
      --  Error routine invoked by the generic instantiation below when
      --  the case expression has a non static choice.

      package Case_Choices_Analysis is new
        Generic_Analyze_Choices
          (Process_Associated_Node => No_OP);
      use Case_Choices_Analysis;

      package Case_Choices_Checking is new
        Generic_Check_Choices
          (Process_Empty_Choice      => No_OP,
           Process_Non_Static_Choice => Non_Static_Choice_Error,
           Process_Associated_Node   => No_OP);
      use Case_Choices_Checking;

      -----------------------------
      -- Non_Static_Choice_Error --
      -----------------------------

      procedure Non_Static_Choice_Error (Choice : Node_Id) is
      begin
         Flag_Non_Static_Expr
           ("choice given in case expression is not static!", Choice);
      end Non_Static_Choice_Error;

      --  Local variables

      Expr      : constant Node_Id := Expression (N);
      Alt       : Node_Id;
      Exp_Type  : Entity_Id;
      Exp_Btype : Entity_Id;

      FirstX : Node_Id := Empty;
      --  First expression in the case for which there is some type information
      --  available, i.e. it is not Any_Type, which can happen because of some
      --  error, or from the use of e.g. raise Constraint_Error.

      Others_Present : Boolean;
      --  Indicates if Others was present

      Wrong_Alt : Node_Id := Empty;
      --  For error reporting

   --  Start of processing for Analyze_Case_Expression

   begin
      if Comes_From_Source (N) then
         Check_Compiler_Unit ("case expression", N);
      end if;

      Analyze_And_Resolve (Expr, Any_Discrete);
      Check_Unset_Reference (Expr);
      Exp_Type := Etype (Expr);
      Exp_Btype := Base_Type (Exp_Type);

      Alt := First (Alternatives (N));
      while Present (Alt) loop
         Analyze (Expression (Alt));

         if No (FirstX) and then Etype (Expression (Alt)) /= Any_Type then
            FirstX := Expression (Alt);
         end if;

         Next (Alt);
      end loop;

      --  Get our initial type from the first expression for which we got some
      --  useful type information from the expression.

      if not Is_Overloaded (FirstX) then
         Set_Etype (N, Etype (FirstX));

      else
         declare
            I  : Interp_Index;
            It : Interp;

         begin
            Set_Etype (N, Any_Type);

            Get_First_Interp (FirstX, I, It);
            while Present (It.Nam) loop

               --  For each interpretation of the first expression, we only
               --  add the interpretation if every other expression in the
               --  case expression alternatives has a compatible type.

               Alt := Next (First (Alternatives (N)));
               while Present (Alt) loop
                  exit when not Has_Compatible_Type (Expression (Alt), It.Typ);
                  Next (Alt);
               end loop;

               if No (Alt) then
                  Add_One_Interp (N, It.Typ, It.Typ);
               else
                  Wrong_Alt := Alt;
               end if;

               Get_Next_Interp (I, It);
            end loop;
         end;
      end if;

      Exp_Btype := Base_Type (Exp_Type);

      --  The expression must be of a discrete type which must be determinable
      --  independently of the context in which the expression occurs, but
      --  using the fact that the expression must be of a discrete type.
      --  Moreover, the type this expression must not be a character literal
      --  (which is always ambiguous).

      --  If error already reported by Resolve, nothing more to do

      if Exp_Btype = Any_Discrete or else Exp_Btype = Any_Type then
         return;

      --  Special casee message for character literal

      elsif Exp_Btype = Any_Character then
         Error_Msg_N
           ("character literal as case expression is ambiguous", Expr);
         return;
      end if;

      if Etype (N) = Any_Type and then Present (Wrong_Alt) then
         Error_Msg_N
           ("type incompatible with that of previous alternatives",
            Expression (Wrong_Alt));
         return;
      end if;

      --  If the case expression is a formal object of mode in out, then
      --  treat it as having a nonstatic subtype by forcing use of the base
      --  type (which has to get passed to Check_Case_Choices below).  Also
      --  use base type when the case expression is parenthesized.

      if Paren_Count (Expr) > 0
        or else (Is_Entity_Name (Expr)
                  and then Ekind (Entity (Expr)) = E_Generic_In_Out_Parameter)
      then
         Exp_Type := Exp_Btype;
      end if;

      --  The case expression alternatives cover the range of a static subtype
      --  subject to aspect Static_Predicate. Do not check the choices when the
      --  case expression has not been fully analyzed yet because this may lead
      --  to bogus errors.

      if Is_OK_Static_Subtype (Exp_Type)
        and then Has_Static_Predicate_Aspect (Exp_Type)
        and then In_Spec_Expression
      then
         null;

      --  Call Analyze_Choices and Check_Choices to do the rest of the work

      else
         Analyze_Choices (Alternatives (N), Exp_Type);
         Check_Choices (N, Alternatives (N), Exp_Type, Others_Present);
      end if;

      if Exp_Type = Universal_Integer and then not Others_Present then
         Error_Msg_N
           ("case on universal integer requires OTHERS choice", Expr);
      end if;
   end Analyze_Case_Expression;

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
      Check_Function_Writable_Actuals (N);
   end Analyze_Comparison_Op;

   ---------------------------
   -- Analyze_Concatenation --
   ---------------------------

   procedure Analyze_Concatenation (N : Node_Id) is

      --  We wish to avoid deep recursion, because concatenations are often
      --  deeply nested, as in A&B&...&Z. Therefore, we walk down the left
      --  operands nonrecursively until we find something that is not a
      --  concatenation (A in this case), or has already been analyzed. We
      --  analyze that, and then walk back up the tree following Parent
      --  pointers, calling Analyze_Concatenation_Rest to do the rest of the
      --  work at each level. The Parent pointers allow us to avoid recursion,
      --  and thus avoid running out of memory.

      NN : Node_Id := N;
      L  : Node_Id;

   begin
      Candidate_Type := Empty;

      --  The following code is equivalent to:

      --    Set_Etype (N, Any_Type);
      --    Analyze_Expression (Left_Opnd (N));
      --    Analyze_Concatenation_Rest (N);

      --  where the Analyze_Expression call recurses back here if the left
      --  operand is a concatenation.

      --  Walk down left operands

      loop
         Set_Etype (NN, Any_Type);
         L := Left_Opnd (NN);
         exit when Nkind (L) /= N_Op_Concat or else Analyzed (L);
         NN := L;
      end loop;

      --  Now (given the above example) NN is A&B and L is A

      --  First analyze L ...

      Analyze_Expression (L);

      --  ... then walk NN back up until we reach N (where we started), calling
      --  Analyze_Concatenation_Rest along the way.

      loop
         Analyze_Concatenation_Rest (NN);
         exit when NN = N;
         NN := Parent (NN);
      end loop;
   end Analyze_Concatenation;

   --------------------------------
   -- Analyze_Concatenation_Rest --
   --------------------------------

   --  If the only one-dimensional array type in scope is String,
   --  this is the resulting type of the operation. Otherwise there
   --  will be a concatenation operation defined for each user-defined
   --  one-dimensional array.

   procedure Analyze_Concatenation_Rest (N : Node_Id) is
      L     : constant Node_Id := Left_Opnd (N);
      R     : constant Node_Id := Right_Opnd (N);
      Op_Id : Entity_Id        := Entity (N);
      LT    : Entity_Id;
      RT    : Entity_Id;

   begin
      Analyze_Expression (R);

      --  If the entity is present, the node appears in an instance, and
      --  denotes a predefined concatenation operation. The resulting type is
      --  obtained from the arguments when possible. If the arguments are
      --  aggregates, the array type and the concatenation type must be
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
   end Analyze_Concatenation_Rest;

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
      --  type of the operands. The operands may even be limited, if they are
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

      --  If there was no match, and the operator is inequality, this may be
      --  a case where inequality has not been made explicit, as for tagged
      --  types. Analyze the node as the negation of an equality operation.
      --  This cannot be done earlier, because before analysis we cannot rule
      --  out the presence of an explicit inequality.

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
                    Left_Opnd  => Left_Opnd (N),
                    Right_Opnd => Right_Opnd (N))));

            Set_Entity (Right_Opnd (N), Op_Id);
            Analyze (N);
         end if;
      end if;

      Operator_Check (N);
      Check_Function_Writable_Actuals (N);
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
      --  If source node, check SPARK restriction. We guard this with the
      --  source node check, because ???

      if Comes_From_Source (N) then
         Check_SPARK_05_Restriction ("explicit dereference is not allowed", N);
      end if;

      --  In formal verification mode, keep track of all reads and writes
      --  through explicit dereferences.

      if GNATprove_Mode then
         SPARK_Specific.Generate_Dereference (N);
      end if;

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

            --  Set the Etype. We need to go through Is_For_Access_Subtypes to
            --  avoid other problems caused by the Private_Subtype and it is
            --  safe to go to the Base_Type because this is the same as
            --  converting the access value to its Base_Type.

            declare
               DT : Entity_Id := Designated_Type (Etype (P));

            begin
               if Ekind (DT) = E_Private_Subtype
                 and then Is_For_Access_Subtype (DT)
               then
                  DT := Base_Type (DT);
               end if;

               --  An explicit dereference is a legal occurrence of an
               --  incomplete type imported through a limited_with clause, if
               --  the full view is visible, or if we are within an instance
               --  body, where the enclosing body has a regular with_clause
               --  on the unit.

               if From_Limited_With (DT)
                 and then not From_Limited_With (Scope (DT))
                 and then
                   (Is_Immediately_Visible (Scope (DT))
                     or else
                       (Is_Child_Unit (Scope (DT))
                         and then Is_Visible_Lib_Unit (Scope (DT)))
                     or else In_Instance_Body)
               then
                  Set_Etype (N, Available_View (DT));

               else
                  Set_Etype (N, DT);
               end if;
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
         --  types. If the context selects the interpretation that is a
         --  function call (not a procedure call) we cannot rewrite the node
         --  yet, but we include the result of the call interpretation.

         Get_First_Interp (N, I, It);
         while Present (It.Nam) loop
            if Ekind (Base_Type (It.Typ)) = E_Subprogram_Type
               and then Etype (Base_Type (It.Typ)) /= Standard_Void_Type
               and then Nkind (Parent (N)) /= N_Procedure_Call_Statement
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

      --  If the expression is an indexed component that will be rewritten
      --  as a container indexing, it has already been analyzed.

      if Nkind (N) = N_Indexed_Component
        and then Present (Generalized_Indexing (N))
      then
         null;

      else
         Analyze (N);
         Check_Parameterless_Call (N);
      end if;
   end Analyze_Expression;

   -------------------------------------
   -- Analyze_Expression_With_Actions --
   -------------------------------------

   procedure Analyze_Expression_With_Actions (N : Node_Id) is
      A : Node_Id;

   begin
      A := First (Actions (N));
      while Present (A) loop
         Analyze (A);
         Next (A);
      end loop;

      Analyze_Expression (Expression (N));
      Set_Etype (N, Etype (Expression (N)));
   end Analyze_Expression_With_Actions;

   ---------------------------
   -- Analyze_If_Expression --
   ---------------------------

   procedure Analyze_If_Expression (N : Node_Id) is
      Condition : constant Node_Id := First (Expressions (N));
      Then_Expr : constant Node_Id := Next (Condition);
      Else_Expr : Node_Id;

   begin
      --  Defend against error of missing expressions from previous error

      if No (Then_Expr) then
         Check_Error_Detected;
         return;
      end if;

      if Comes_From_Source (N) then
         Check_SPARK_05_Restriction ("if expression is not allowed", N);
      end if;

      Else_Expr := Next (Then_Expr);

      if Comes_From_Source (N) then
         Check_Compiler_Unit ("if expression", N);
      end if;

      --  Analyze and resolve the condition. We need to resolve this now so
      --  that it gets folded to True/False if possible, before we analyze
      --  the THEN/ELSE branches, because when analyzing these branches, we
      --  may call Is_Statically_Unevaluated, which expects the condition of
      --  an enclosing IF to have been analyze/resolved/evaluated.

      Analyze_Expression (Condition);
      Resolve (Condition, Any_Boolean);

      --  Analyze THEN expression and (if present) ELSE expression. For those
      --  we delay resolution in the normal manner, because of overloading etc.

      Analyze_Expression (Then_Expr);

      if Present (Else_Expr) then
         Analyze_Expression (Else_Expr);
      end if;

      --  If then expression not overloaded, then that decides the type

      if not Is_Overloaded (Then_Expr) then
         Set_Etype (N, Etype (Then_Expr));

      --  Case where then expression is overloaded

      else
         declare
            I  : Interp_Index;
            It : Interp;

         begin
            Set_Etype (N, Any_Type);

            --  Loop through interpretations of Then_Expr

            Get_First_Interp (Then_Expr, I, It);
            while Present (It.Nam) loop

               --  Add possible interpretation of Then_Expr if no Else_Expr, or
               --  Else_Expr is present and has a compatible type.

               if No (Else_Expr)
                 or else Has_Compatible_Type (Else_Expr, It.Typ)
               then
                  Add_One_Interp (N, It.Typ, It.Typ);
               end if;

               Get_Next_Interp (I, It);
            end loop;

            --  If no valid interpretation has been found, then the type of the
            --  ELSE expression does not match any interpretation of the THEN
            --  expression.

            if Etype (N) = Any_Type then
               Error_Msg_N
                 ("type incompatible with that of `THEN` expression",
                  Else_Expr);
               return;
            end if;
         end;
      end if;
   end Analyze_If_Expression;

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
      --  Prefix in indexed component form is an overloadable entity, so the
      --  node is a function call. Reformat it as such.

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
         Loc    : constant Source_Ptr := Sloc (N);
         Actual : Node_Id;

      begin
         Change_Node (N, N_Function_Call);
         Set_Name (N, P);
         Set_Parameter_Associations (N, Exprs);

         --  Analyze actuals prior to analyzing the call itself

         Actual := First (Parameter_Associations (N));
         while Present (Actual) loop
            Analyze (Actual);
            Check_Parameterless_Call (Actual);

            --  Move to next actual. Note that we use Next, not Next_Actual
            --  here. The reason for this is a bit subtle. If a function call
            --  includes named associations, the parser recognizes the node
            --  as a call, and it is analyzed as such. If all associations are
            --  positional, the parser builds an indexed_component node, and
            --  it is only after analysis of the prefix that the construct
            --  is recognized as a call, in which case Process_Function_Call
            --  rewrites the node and analyzes the actuals. If the list of
            --  actuals is malformed, the parser may leave the node as an
            --  indexed component (despite the presence of named associations).
            --  The iterator Next_Actual is equivalent to Next if the list is
            --  positional, but follows the normalized chain of actuals when
            --  named associations are present. In this case normalization has
            --  not taken place, and actuals remain unanalyzed, which leads to
            --  subsequent crashes or loops if there is an attempt to continue
            --  analysis of the program.

            --  IF there is a single actual and it is a type name, the node
            --  can only be interpreted as a slice of a parameterless call.
            --  Rebuild the node as such and analyze.

            if No (Next (Actual))
              and then Is_Entity_Name (Actual)
              and then Is_Type (Entity (Actual))
              and then Is_Discrete_Type (Entity (Actual))
            then
               Replace (N,
                 Make_Slice (Loc,
                   Prefix         => P,
                   Discrete_Range =>
                     New_Occurrence_Of (Entity (Actual), Loc)));
               Analyze (N);
               return;

            else
               Next (Actual);
            end if;
         end loop;

         Analyze_Call (N);
      end Process_Function_Call;

      -------------------------------
      -- Process_Indexed_Component --
      -------------------------------

      procedure Process_Indexed_Component is
         Exp        : Node_Id;
         Array_Type : Entity_Id;
         Index      : Node_Id;
         Pent       : Entity_Id := Empty;

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
               Error_Msg_NW
                 (Warn_On_Dereference, "?d?implicit dereference", N);
               Array_Type := Process_Implicit_Dereference_Prefix (Pent, P);
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

            elsif Try_Container_Indexing (N, P, Exprs) then
               return;

            elsif Array_Type = Any_Type then
               Set_Etype (N, Any_Type);

               --  In most cases the analysis of the prefix will have emitted
               --  an error already, but if the prefix may be interpreted as a
               --  call in prefixed notation, the report is left to the caller.
               --  To prevent cascaded errors, report only if no previous ones.

               if Serious_Errors_Detected = 0 then
                  Error_Msg_N ("invalid prefix in indexed component", P);

                  if Nkind (P) = N_Expanded_Name then
                     Error_Msg_NE ("\& is not visible", P, Selector_Name (P));
                  end if;
               end if;

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
                  Error_Msg_NE ("incorrect use of &", P, Entity (P));

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
            Check_Implicit_Dereference (N, Etype (N));

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

         --  If one index is present, and it is a subtype name, then the node
         --  denotes a slice (note that the case of an explicit range for a
         --  slice was already built as an N_Slice node in the first place,
         --  so that case is not handled here).

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
               Error_Msg_NW
                 (Warn_On_Dereference, "?d?implicit dereference", N);
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
                  declare
                     CT : constant Entity_Id :=
                            Base_Type (Component_Type (Typ));
                  begin
                     Add_One_Interp (N, CT, CT);
                     Check_Implicit_Dereference (N, CT);
                  end;
               end if;

            elsif Try_Container_Indexing (N, P, Exprs) then
               return;

            end if;

            Get_Next_Interp (I, It);
         end loop;

         if Etype (N) = Any_Type then
            Error_Msg_N ("no legal interpretation for indexed component", N);
            Set_Is_Overloaded (N, False);
         end if;

         End_Interp_List;
      end Process_Overloaded_Indexed_Component;

   --  Start of processing for Analyze_Indexed_Component_Form

   begin
      --  Get name of array, function or type

      Analyze (P);

      --  If P is an explicit dereference whose prefix is of a remote access-
      --  to-subprogram type, then N has already been rewritten as a subprogram
      --  call and analyzed.

      if Nkind (N) in N_Subprogram_Call then
         return;

      --  When the prefix is attribute 'Loop_Entry and the sole expression of
      --  the indexed component denotes a loop name, the indexed form is turned
      --  into an attribute reference.

      elsif Nkind (N) = N_Attribute_Reference
        and then Attribute_Name (N) = Name_Loop_Entry
      then
         return;
      end if;

      pragma Assert (Nkind (N) = N_Indexed_Component);

      P_T := Base_Type (Etype (P));

      if Is_Entity_Name (P) and then Present (Entity (P)) then
         U_N := Entity (P);

         if Is_Type (U_N) then

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
                        Ekind (Designated_Type (Etype (P))) =
                                                   E_Subprogram_Type)
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
           and then Present (Entity (Selector_Name (P)))
           and then Is_Overloadable (Entity (Selector_Name (P)))
         then
            Process_Function_Call;

         --  In ASIS mode within a generic, a prefixed call is analyzed and
         --  partially rewritten but the original indexed component has not
         --  yet been rewritten as a call. Perform the replacement now.

         elsif Nkind (P) = N_Selected_Component
           and then Nkind (Parent (P)) = N_Function_Call
           and then ASIS_Mode
         then
            Rewrite (N, Parent (P));
            Analyze (N);

         else
            --  Indexed component, slice, or a call to a member of a family
            --  entry, which will be converted to an entry call later.

            Process_Indexed_Component_Or_Slice;
         end if;
      end if;

      Analyze_Dimension (N);
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
      Check_Function_Writable_Actuals (N);
   end Analyze_Logical_Op;

   ---------------------------
   -- Analyze_Membership_Op --
   ---------------------------

   procedure Analyze_Membership_Op (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      L     : constant Node_Id    := Left_Opnd (N);
      R     : constant Node_Id    := Right_Opnd (N);

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

      procedure Analyze_Set_Membership;
      --  If a set of alternatives is present, analyze each and find the
      --  common type to which they must all resolve.

      ----------------------------
      -- Analyze_Set_Membership --
      ----------------------------

      procedure Analyze_Set_Membership is
         Alt               : Node_Id;
         Index             : Interp_Index;
         It                : Interp;
         Candidate_Interps : Node_Id;
         Common_Type       : Entity_Id := Empty;

      begin
         if Comes_From_Source (N) then
            Check_Compiler_Unit ("set membership", N);
         end if;

         Analyze (L);
         Candidate_Interps := L;

         if not Is_Overloaded (L) then
            Common_Type := Etype (L);

            Alt := First (Alternatives (N));
            while Present (Alt) loop
               Analyze (Alt);

               if not Has_Compatible_Type (Alt, Common_Type) then
                  Wrong_Type (Alt, Common_Type);
               end if;

               Next (Alt);
            end loop;

         else
            Alt := First (Alternatives (N));
            while Present (Alt) loop
               Analyze (Alt);
               if not Is_Overloaded (Alt) then
                  Common_Type := Etype (Alt);

               else
                  Get_First_Interp (Alt, Index, It);
                  while Present (It.Typ) loop
                     if not
                       Has_Compatible_Type (Candidate_Interps, It.Typ)
                     then
                        Remove_Interp (Index);
                     end if;

                     Get_Next_Interp (Index, It);
                  end loop;

                  Get_First_Interp (Alt, Index, It);

                  if No (It.Typ) then
                     Error_Msg_N ("alternative has no legal type", Alt);
                     return;
                  end if;

                  --  If alternative is not overloaded, we have a unique type
                  --  for all of them.

                  Set_Etype (Alt, It.Typ);
                  Get_Next_Interp (Index, It);

                  if No (It.Typ) then
                     Set_Is_Overloaded (Alt, False);
                     Common_Type := Etype (Alt);
                  end if;

                  Candidate_Interps := Alt;
               end if;

               Next (Alt);
            end loop;
         end if;

         Set_Etype (N, Standard_Boolean);

         if Present (Common_Type) then
            Set_Etype (L, Common_Type);

            --  The left operand may still be overloaded, to be resolved using
            --  the Common_Type.

         else
            Error_Msg_N ("cannot resolve membership operation", N);
         end if;
      end Analyze_Set_Membership;

   --  Start of processing for Analyze_Membership_Op

   begin
      Analyze_Expression (L);

      if No (R) and then Ada_Version >= Ada_2012 then
         Analyze_Set_Membership;
         Check_Function_Writable_Actuals (N);

         return;
      end if;

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

      --  If not a range, it can be a subtype mark, or else it is a degenerate
      --  membership test with a singleton value, i.e. a test for equality,
      --  if the types are compatible.

      else
         Analyze (R);

         if Is_Entity_Name (R)
           and then Is_Type (Entity (R))
         then
            Find_Type (R);
            Check_Fully_Declared (Entity (R), R);

         elsif Ada_Version >= Ada_2012
           and then Has_Compatible_Type (R, Etype (L))
         then
            if Nkind (N) = N_In then
               Rewrite (N,
                 Make_Op_Eq (Loc,
                   Left_Opnd  => L,
                   Right_Opnd => R));
            else
               Rewrite (N,
                 Make_Op_Ne (Loc,
                   Left_Opnd  => L,
                   Right_Opnd => R));
            end if;

            Analyze (N);
            return;

         else
            --  In all versions of the language, if we reach this point there
            --  is a previous error that will be diagnosed below.

            Find_Type (R);
         end if;
      end if;

      --  Compatibility between expression and subtype mark or range is
      --  checked during resolution. The result of the operation is Boolean
      --  in any case.

      Set_Etype (N, Standard_Boolean);

      if Comes_From_Source (N)
        and then Present (Right_Opnd (N))
        and then Is_CPP_Class (Etype (Etype (Right_Opnd (N))))
      then
         Error_Msg_N ("membership test not applicable to cpp-class types", N);
      end if;

      Check_Function_Writable_Actuals (N);
   end Analyze_Membership_Op;

   -----------------
   -- Analyze_Mod --
   -----------------

   procedure Analyze_Mod (N : Node_Id) is
   begin
      --  A special warning check, if we have an expression of the form:
      --    expr mod 2 * literal
      --  where literal is 64 or less, then probably what was meant was
      --    expr mod 2 ** literal
      --  so issue an appropriate warning.

      if Warn_On_Suspicious_Modulus_Value
        and then Nkind (Right_Opnd (N)) = N_Integer_Literal
        and then Intval (Right_Opnd (N)) = Uint_2
        and then Nkind (Parent (N)) = N_Op_Multiply
        and then Nkind (Right_Opnd (Parent (N))) = N_Integer_Literal
        and then Intval (Right_Opnd (Parent (N))) <= Uint_64
      then
         Error_Msg_N
           ("suspicious MOD value, was '*'* intended'??M?", Parent (N));
      end if;

      --  Remaining processing is same as for other arithmetic operators

      Analyze_Arithmetic_Op (N);
   end Analyze_Mod;

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
      Check_SPARK_05_Restriction ("null is not allowed", N);

      Set_Etype (N, Any_Access);
   end Analyze_Null;

   ----------------------
   -- Analyze_One_Call --
   ----------------------

   procedure Analyze_One_Call
      (N          : Node_Id;
       Nam        : Entity_Id;
       Report     : Boolean;
       Success    : out Boolean;
       Skip_First : Boolean := False)
   is
      Actuals : constant List_Id   := Parameter_Associations (N);
      Prev_T  : constant Entity_Id := Etype (N);

      Must_Skip  : constant Boolean := Skip_First
                     or else Nkind (Original_Node (N)) = N_Selected_Component
                     or else
                       (Nkind (Original_Node (N)) = N_Indexed_Component
                          and then Nkind (Prefix (Original_Node (N)))
                            = N_Selected_Component);
      --  The first formal must be omitted from the match when trying to find
      --  a primitive operation that is a possible interpretation, and also
      --  after the call has been rewritten, because the corresponding actual
      --  is already known to be compatible, and because this may be an
      --  indexing of a call with default parameters.

      Formal      : Entity_Id;
      Actual      : Node_Id;
      Is_Indexed  : Boolean := False;
      Is_Indirect : Boolean := False;
      Subp_Type   : constant Entity_Id := Etype (Nam);
      Norm_OK     : Boolean;

      function Compatible_Types_In_Predicate
        (T1 : Entity_Id;
         T2 : Entity_Id) return Boolean;
      --  For an Ada 2012 predicate or invariant, a call may mention an
      --  incomplete type, while resolution of the corresponding predicate
      --  function may see the full view, as a consequence of the delayed
      --  resolution of the corresponding expressions. This may occur in
      --  the body of a predicate function, or in a call to such. Anomalies
      --  involving private and full views can also happen. In each case,
      --  rewrite node or add conversions to remove spurious type errors.

      procedure Indicate_Name_And_Type;
      --  If candidate interpretation matches, indicate name and type of result
      --  on call node.

      function Operator_Hidden_By (Fun : Entity_Id) return Boolean;
      --  There may be a user-defined operator that hides the current
      --  interpretation. We must check for this independently of the
      --  analysis of the call with the user-defined operation, because
      --  the parameter names may be wrong and yet the hiding takes place.
      --  This fixes a problem with ACATS test B34014O.
      --
      --  When the type Address is a visible integer type, and the DEC
      --  system extension is visible, the predefined operator may be
      --  hidden as well, by one of the address operations in auxdec.
      --  Finally, The abstract operations on address do not hide the
      --  predefined operator (this is the purpose of making them abstract).

      -----------------------------------
      -- Compatible_Types_In_Predicate --
      -----------------------------------

      function Compatible_Types_In_Predicate
        (T1 : Entity_Id;
         T2 : Entity_Id) return Boolean
      is
         function Common_Type (T : Entity_Id) return Entity_Id;
         --  Find non-private full view if any, without going to ancestor type
         --  (as opposed to Underlying_Type).

         -----------------
         -- Common_Type --
         -----------------

         function Common_Type (T : Entity_Id) return Entity_Id is
         begin
            if Is_Private_Type (T) and then Present (Full_View (T)) then
               return Base_Type (Full_View (T));
            else
               return Base_Type (T);
            end if;
         end Common_Type;

      --  Start of processing for Compatible_Types_In_Predicate

      begin
         if (Ekind (Current_Scope) = E_Function
              and then Is_Predicate_Function (Current_Scope))
           or else
            (Ekind (Nam) = E_Function
              and then Is_Predicate_Function (Nam))
         then
            if Is_Incomplete_Type (T1)
              and then Present (Full_View (T1))
              and then Full_View (T1) = T2
            then
               Set_Etype (Formal, Etype (Actual));
               return True;

            elsif Common_Type (T1) = Common_Type (T2) then
               Rewrite (Actual, Unchecked_Convert_To (Etype (Formal), Actual));
               return True;

            else
               return False;
            end if;

         else
            return False;
         end if;
      end Compatible_Types_In_Predicate;

      ----------------------------
      -- Indicate_Name_And_Type --
      ----------------------------

      procedure Indicate_Name_And_Type is
      begin
         Add_One_Interp (N, Nam, Etype (Nam));
         Check_Implicit_Dereference (N, Etype (Nam));
         Success := True;

         --  If the prefix of the call is a name, indicate the entity
         --  being called. If it is not a name,  it is an expression that
         --  denotes an access to subprogram or else an entry or family. In
         --  the latter case, the name is a selected component, and the entity
         --  being called is noted on the selector.

         if not Is_Type (Nam) then
            if Is_Entity_Name (Name (N)) then
               Set_Entity (Name (N), Nam);
               Set_Etype  (Name (N), Etype (Nam));

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

      ------------------------
      -- Operator_Hidden_By --
      ------------------------

      function Operator_Hidden_By (Fun : Entity_Id) return Boolean is
         Act1  : constant Node_Id   := First_Actual (N);
         Act2  : constant Node_Id   := Next_Actual (Act1);
         Form1 : constant Entity_Id := First_Formal (Fun);
         Form2 : constant Entity_Id := Next_Formal (Form1);

      begin
         if Ekind (Fun) /= E_Function or else Is_Abstract_Subprogram (Fun) then
            return False;

         elsif not Has_Compatible_Type (Act1, Etype (Form1)) then
            return False;

         elsif Present (Form2) then
            if No (Act2)
              or else not Has_Compatible_Type (Act2, Etype (Form2))
            then
               return False;
            end if;

         elsif Present (Act2) then
            return False;
         end if;

         --  Now we know that the arity of the operator matches the function,
         --  and the function call is a valid interpretation. The function
         --  hides the operator if it has the right signature, or if one of
         --  its operands is a non-abstract operation on Address when this is
         --  a visible integer type.

         return Hides_Op (Fun, Nam)
           or else Is_Descendant_Of_Address (Etype (Form1))
           or else
             (Present (Form2)
               and then Is_Descendant_Of_Address (Etype (Form2)));
      end Operator_Hidden_By;

   --  Start of processing for Analyze_One_Call

   begin
      Success := False;

      --  If the subprogram has no formals or if all the formals have defaults,
      --  and the return type is an array type, the node may denote an indexing
      --  of the result of a parameterless call. In Ada 2005, the subprogram
      --  may have one non-defaulted formal, and the call may have been written
      --  in prefix notation, so that the rebuilt parameter list has more than
      --  one actual.

      if not Is_Overloadable (Nam)
        and then Ekind (Nam) /= E_Subprogram_Type
        and then Ekind (Nam) /= E_Entry_Family
      then
         return;
      end if;

      --  An indexing requires at least one actual. The name of the call cannot
      --  be an implicit indirect call, so it cannot be a generated explicit
      --  dereference.

      if not Is_Empty_List (Actuals)
        and then
          (Needs_No_Actuals (Nam)
            or else
              (Needs_One_Actual (Nam)
                and then Present (Next_Actual (First (Actuals)))))
      then
         if Is_Array_Type (Subp_Type)
           and then
            (Nkind (Name (N)) /= N_Explicit_Dereference
              or else Comes_From_Source (Name (N)))
         then
            Is_Indexed := Try_Indexed_Call (N, Nam, Subp_Type, Must_Skip);

         elsif Is_Access_Type (Subp_Type)
           and then Is_Array_Type (Designated_Type (Subp_Type))
         then
            Is_Indexed :=
              Try_Indexed_Call
                (N, Nam, Designated_Type (Subp_Type), Must_Skip);

         --  The prefix can also be a parameterless function that returns an
         --  access to subprogram, in which case this is an indirect call.
         --  If this succeeds, an explicit dereference is added later on,
         --  in Analyze_Call or Resolve_Call.

         elsif Is_Access_Type (Subp_Type)
           and then Ekind (Designated_Type (Subp_Type)) = E_Subprogram_Type
         then
            Is_Indirect := Try_Indirect_Call (N, Nam, Subp_Type);
         end if;

      end if;

      --  If the call has been transformed into a slice, it is of the form
      --  F (Subtype) where F is parameterless. The node has been rewritten in
      --  Try_Indexed_Call and there is nothing else to do.

      if Is_Indexed
        and then Nkind (N) = N_Slice
      then
         return;
      end if;

      Normalize_Actuals
        (N, Nam, (Report and not Is_Indexed and not Is_Indirect), Norm_OK);

      if not Norm_OK then

         --  If an indirect call is a possible interpretation, indicate
         --  success to the caller. This may be an indexing of an explicit
         --  dereference of a call that returns an access type (see above).

         if Is_Indirect
           or else (Is_Indexed
                     and then Nkind (Name (N)) = N_Explicit_Dereference
                     and then Comes_From_Source (Name (N)))
         then
            Success := True;
            return;

         --  Mismatch in number or names of parameters

         elsif Debug_Flag_E then
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

      elsif No (Actuals) then

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

            --  Check that operator is not hidden by a function interpretation

            if Is_Overloaded (Name (N)) then
               declare
                  I  : Interp_Index;
                  It : Interp;

               begin
                  Get_First_Interp (Name (N), I, It);
                  while Present (It.Nam) loop
                     if Operator_Hidden_By (It.Nam) then
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

         --  If we are analyzing a call rewritten from object notation, skip
         --  first actual, which may be rewritten later as an explicit
         --  dereference.

         if Must_Skip then
            Next_Actual (Actual);
            Next_Formal (Formal);
         end if;

         while Present (Actual) and then Present (Formal) loop
            if Nkind (Parent (Actual)) /= N_Parameter_Association
              or else Chars (Selector_Name (Parent (Actual))) = Chars (Formal)
            then
               --  The actual can be compatible with the formal, but we must
               --  also check that the context is not an address type that is
               --  visibly an integer type. In this case the use of literals is
               --  illegal, except in the body of descendants of system, where
               --  arithmetic operations on address are of course used.

               if Has_Compatible_Type (Actual, Etype (Formal))
                 and then
                  (Etype (Actual) /= Universal_Integer
                    or else not Is_Descendant_Of_Address (Etype (Formal))
                    or else
                      Is_Predefined_File_Name
                        (Unit_File_Name (Get_Source_Unit (N))))
               then
                  Next_Actual (Actual);
                  Next_Formal (Formal);

               --  In Allow_Integer_Address mode, we allow an actual integer to
               --  match a formal address type and vice versa. We only do this
               --  if we are certain that an error will otherwise be issued

               elsif Address_Integer_Convert_OK
                       (Etype (Actual), Etype (Formal))
                 and then (Report and not Is_Indexed and not Is_Indirect)
               then
                  --  Handle this case by introducing an unchecked conversion

                  Rewrite (Actual,
                           Unchecked_Convert_To (Etype (Formal),
                             Relocate_Node (Actual)));
                  Analyze_And_Resolve (Actual, Etype (Formal));
                  Next_Actual (Actual);
                  Next_Formal (Formal);

               --  Under relaxed RM semantics silently replace occurrences of
               --  null by System.Address_Null. We only do this if we know that
               --  an error will otherwise be issued.

               elsif Null_To_Null_Address_Convert_OK (Actual, Etype (Formal))
                 and then (Report and not Is_Indexed and not Is_Indirect)
               then
                  Replace_Null_By_Null_Address (Actual);
                  Analyze_And_Resolve (Actual, Etype (Formal));
                  Next_Actual (Actual);
                  Next_Formal (Formal);

               elsif Compatible_Types_In_Predicate
                       (Etype (Formal), Etype (Actual))
               then
                  Next_Actual (Actual);
                  Next_Formal (Formal);

               --  In a complex case where an enclosing generic and a nested
               --  generic package, both declared with partially parameterized
               --  formal subprograms with the same names, are instantiated
               --  with the same type, the types of the actual parameter and
               --  that of the formal may appear incompatible at first sight.

               --   generic
               --      type Outer_T is private;
               --      with function Func (Formal : Outer_T)
               --                         return ... is <>;

               --   package Outer_Gen is
               --      generic
               --         type Inner_T is private;
               --         with function Func (Formal : Inner_T)   --  (1)
               --           return ... is <>;

               --      package Inner_Gen is
               --         function Inner_Func (Formal : Inner_T)  --  (2)
               --           return ... is (Func (Formal));
               --      end Inner_Gen;
               --   end Outer_Generic;

               --   package Outer_Inst is new Outer_Gen (Actual_T);
               --   package Inner_Inst is new Outer_Inst.Inner_Gen (Actual_T);

               --  In the example above, the type of parameter
               --  Inner_Func.Formal at (2) is incompatible with the type of
               --  Func.Formal at (1) in the context of instantiations
               --  Outer_Inst and Inner_Inst. In reality both types are generic
               --  actual subtypes renaming base type Actual_T as part of the
               --  generic prologues for the instantiations.

               --  Recognize this case and add a type conversion to allow this
               --  kind of generic actual subtype conformance. Note that this
               --  is done only when the call is non-overloaded because the
               --  resolution mechanism already has the means to disambiguate
               --  similar cases.

               elsif not Is_Overloaded (Name (N))
                 and then Is_Type (Etype (Actual))
                 and then Is_Type (Etype (Formal))
                 and then Is_Generic_Actual_Type (Etype (Actual))
                 and then Is_Generic_Actual_Type (Etype (Formal))
                 and then Base_Type (Etype (Actual)) =
                          Base_Type (Etype (Formal))
               then
                  Rewrite (Actual,
                    Convert_To (Etype (Formal), Relocate_Node (Actual)));
                  Analyze_And_Resolve (Actual, Etype (Formal));
                  Next_Actual (Actual);
                  Next_Formal (Formal);

               --  Handle failed type check

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

                  --  Comment needed on the following test???

                  if Report and not Is_Indexed and not Is_Indirect then

                     --  Ada 2005 (AI-251): Complete the error notification
                     --  to help new Ada 2005 users.

                     if Is_Class_Wide_Type (Etype (Formal))
                       and then Is_Interface (Etype (Etype (Formal)))
                       and then not Interface_Present_In_Ancestor
                                      (Typ   => Etype (Actual),
                                       Iface => Etype (Etype (Formal)))
                     then
                        Error_Msg_NE
                          ("(Ada 2005) does not implement interface }",
                           Actual, Etype (Etype (Formal)));
                     end if;

                     Wrong_Type (Actual, Etype (Formal));

                     if Nkind (Actual) = N_Op_Eq
                       and then Nkind (Left_Opnd (Actual)) = N_Identifier
                     then
                        Formal := First_Formal (Nam);
                        while Present (Formal) loop
                           if Chars (Left_Opnd (Actual)) = Chars (Formal) then
                              Error_Msg_N -- CODEFIX
                                ("possible misspelling of `='>`!", Actual);
                              exit;
                           end if;

                           Next_Formal (Formal);
                        end loop;
                     end if;

                     if All_Errors_Mode then
                        Error_Msg_Sloc := Sloc (Nam);

                        if Etype (Formal) = Any_Type then
                           Error_Msg_N
                             ("there is no legal actual parameter", Actual);
                        end if;

                        if Is_Overloadable (Nam)
                          and then Present (Alias (Nam))
                          and then not Comes_From_Source (Nam)
                        then
                           Error_Msg_NE
                             ("\\  =='> in call to inherited operation & #!",
                              Actual, Nam);

                        elsif Ekind (Nam) = E_Subprogram_Type then
                           declare
                              Access_To_Subprogram_Typ :
                                constant Entity_Id :=
                                  Defining_Identifier
                                    (Associated_Node_For_Itype (Nam));
                           begin
                              Error_Msg_NE
                                ("\\  =='> in call to dereference of &#!",
                                 Actual, Access_To_Subprogram_Typ);
                           end;

                        else
                           Error_Msg_NE
                             ("\\  =='> in call to &#!", Actual, Nam);

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
         end if;

         --  Otherwise action depends on operator

         case Op_Name is
            when Name_Op_Add      |
                 Name_Op_Subtract |
                 Name_Op_Multiply |
                 Name_Op_Divide   |
                 Name_Op_Mod      |
                 Name_Op_Rem      |
                 Name_Op_Expon    =>
               Find_Arithmetic_Types (Act1, Act2, Op_Id, N);

            when Name_Op_And      |
                 Name_Op_Or       |
                 Name_Op_Xor      =>
               Find_Boolean_Types (Act1, Act2, Op_Id, N);

            when Name_Op_Lt       |
                 Name_Op_Le       |
                 Name_Op_Gt       |
                 Name_Op_Ge       =>
               Find_Comparison_Types (Act1, Act2, Op_Id,  N);

            when Name_Op_Eq       |
                 Name_Op_Ne       =>
               Find_Equality_Types (Act1, Act2, Op_Id,  N);

            when Name_Op_Concat   =>
               Find_Concatenation_Types (Act1, Act2, Op_Id, N);

            --  Is this when others, or should it be an abort???

            when others           =>
               null;
         end case;

      --  Unary operator case

      else
         case Op_Name is
            when Name_Op_Subtract |
                 Name_Op_Add      |
                 Name_Op_Abs      =>
               Find_Unary_Types (Act1, Op_Id, N);

            when Name_Op_Not      =>
               Find_Negation_Types (Act1, Op_Id, N);

            --  Is this when others correct, or should it be an abort???

            when others           =>
               null;
         end case;
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
            Error_Msg_NW (Warn_On_Dereference, "?d?implicit dereference", N);
         else
            T := It.Typ;
         end if;

         --  Locate the component. For a private prefix the selector can denote
         --  a discriminant.

         if Is_Record_Type (T) or else Is_Private_Type (T) then

            --  If the prefix is a class-wide type, the visible components are
            --  those of the base type.

            if Is_Class_Wide_Type (T) then
               T := Etype (T);
            end if;

            Comp := First_Entity (T);
            while Present (Comp) loop
               if Chars (Comp) = Chars (Sel)
                 and then Is_Visible_Component (Comp)
               then

                  --  AI05-105:  if the context is an object renaming with
                  --  an anonymous access type, the expected type of the
                  --  object must be anonymous. This is a name resolution rule.

                  if Nkind (Parent (N)) /= N_Object_Renaming_Declaration
                    or else No (Access_Definition (Parent (N)))
                    or else Ekind (Etype (Comp)) = E_Anonymous_Access_Type
                    or else
                      Ekind (Etype (Comp)) = E_Anonymous_Access_Subprogram_Type
                  then
                     Set_Entity (Sel, Comp);
                     Set_Etype (Sel, Etype (Comp));
                     Add_One_Interp (N, Etype (Comp), Etype (Comp));
                     Check_Implicit_Dereference (N, Etype (Comp));

                     --  This also specifies a candidate to resolve the name.
                     --  Further overloading will be resolved from context.
                     --  The selector name itself does not carry overloading
                     --  information.

                     Set_Etype (Nam, It.Typ);

                  else
                     --  Named access type in the context of a renaming
                     --  declaration with an access definition. Remove
                     --  inapplicable candidate.

                     Remove_Interp (I);
                  end if;
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
                     Set_Entity_With_Checks (Sel, Comp);
                     Generate_Reference (Comp, Sel);
                  end if;

                  Set_Etype (Sel, Etype (Comp));
                  Set_Etype (N,   Etype (Comp));
                  Set_Etype (Nam, It.Typ);

                  --  For access type case, introduce explicit dereference for
                  --  more uniform treatment of entry calls. Do this only once
                  --  if several interpretations yield an access type.

                  if Is_Access_Type (Etype (Nam))
                    and then Nkind (Nam) /= N_Explicit_Dereference
                  then
                     Insert_Explicit_Dereference (Nam);
                     Error_Msg_NW
                       (Warn_On_Dereference, "?d?implicit dereference", N);
                  end if;
               end if;

               Next_Entity (Comp);
            end loop;

            Set_Is_Overloaded (N, Is_Overloaded (Sel));
         end if;

         Get_Next_Interp (I, It);
      end loop;

      if Etype (N) = Any_Type
        and then not Try_Object_Operation (N)
      then
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
      Expr : constant Node_Id   := Expression (N);
      I    : Interp_Index;
      It   : Interp;
      T    : Entity_Id;

   begin
      Analyze_Expression (Expr);

      Set_Etype (N, Any_Type);
      Find_Type (Mark);
      T := Entity (Mark);
      Set_Etype (N, T);

      if T = Any_Type then
         return;
      end if;

      Check_Fully_Declared (T, N);

      --  If expected type is class-wide, check for exact match before
      --  expansion, because if the expression is a dispatching call it
      --  may be rewritten as explicit dereference with class-wide result.
      --  If expression is overloaded, retain only interpretations that
      --  will yield exact matches.

      if Is_Class_Wide_Type (T) then
         if not Is_Overloaded (Expr) then
            if Base_Type (Etype (Expr)) /= Base_Type (T) then
               if Nkind (Expr) = N_Aggregate then
                  Error_Msg_N ("type of aggregate cannot be class-wide", Expr);
               else
                  Wrong_Type (Expr, T);
               end if;
            end if;

         else
            Get_First_Interp (Expr, I, It);

            while Present (It.Nam) loop
               if Base_Type (It.Typ) /= Base_Type (T) then
                  Remove_Interp (I);
               end if;

               Get_Next_Interp (I, It);
            end loop;
         end if;
      end if;

      Set_Etype  (N, T);
   end Analyze_Qualified_Expression;

   -----------------------------------
   -- Analyze_Quantified_Expression --
   -----------------------------------

   procedure Analyze_Quantified_Expression (N : Node_Id) is
      function Is_Empty_Range (Typ : Entity_Id) return Boolean;
      --  If the iterator is part of a quantified expression, and the range is
      --  known to be statically empty, emit a warning and replace expression
      --  with its static value. Returns True if the replacement occurs.

      function No_Else_Or_Trivial_True (If_Expr : Node_Id) return Boolean;
      --  Determine whether if expression If_Expr lacks an else part or if it
      --  has one, it evaluates to True.

      --------------------
      -- Is_Empty_Range --
      --------------------

      function Is_Empty_Range (Typ : Entity_Id) return Boolean is
         Loc : constant Source_Ptr := Sloc (N);

      begin
         if Is_Array_Type (Typ)
           and then Compile_Time_Known_Bounds (Typ)
           and then
             (Expr_Value (Type_Low_Bound  (Etype (First_Index (Typ)))) >
              Expr_Value (Type_High_Bound (Etype (First_Index (Typ)))))
         then
            Preanalyze_And_Resolve (Condition (N), Standard_Boolean);

            if All_Present (N) then
               Error_Msg_N
                 ("??quantified expression with ALL "
                  & "over a null range has value True", N);
               Rewrite (N, New_Occurrence_Of (Standard_True, Loc));

            else
               Error_Msg_N
                 ("??quantified expression with SOME "
                  & "over a null range has value False", N);
               Rewrite (N, New_Occurrence_Of (Standard_False, Loc));
            end if;

            Analyze (N);
            return True;

         else
            return False;
         end if;
      end Is_Empty_Range;

      -----------------------------
      -- No_Else_Or_Trivial_True --
      -----------------------------

      function No_Else_Or_Trivial_True (If_Expr : Node_Id) return Boolean is
         Else_Expr : constant Node_Id :=
                       Next (Next (First (Expressions (If_Expr))));
      begin
         return
           No (Else_Expr)
             or else (Compile_Time_Known_Value (Else_Expr)
                       and then Is_True (Expr_Value (Else_Expr)));
      end No_Else_Or_Trivial_True;

      --  Local variables

      Cond    : constant Node_Id := Condition (N);
      Loop_Id : Entity_Id;
      QE_Scop : Entity_Id;

   --  Start of processing for Analyze_Quantified_Expression

   begin
      Check_SPARK_05_Restriction ("quantified expression is not allowed", N);

      --  Create a scope to emulate the loop-like behavior of the quantified
      --  expression. The scope is needed to provide proper visibility of the
      --  loop variable.

      QE_Scop := New_Internal_Entity (E_Loop, Current_Scope, Sloc (N), 'L');
      Set_Etype  (QE_Scop, Standard_Void_Type);
      Set_Scope  (QE_Scop, Current_Scope);
      Set_Parent (QE_Scop, N);

      Push_Scope (QE_Scop);

      --  All constituents are preanalyzed and resolved to avoid untimely
      --  generation of various temporaries and types. Full analysis and
      --  expansion is carried out when the quantified expression is
      --  transformed into an expression with actions.

      if Present (Iterator_Specification (N)) then
         Preanalyze (Iterator_Specification (N));

         --  Do not proceed with the analysis when the range of iteration is
         --  empty. The appropriate error is issued by Is_Empty_Range.

         if Is_Entity_Name (Name (Iterator_Specification (N)))
           and then Is_Empty_Range (Etype (Name (Iterator_Specification (N))))
         then
            return;
         end if;

      else pragma Assert (Present (Loop_Parameter_Specification (N)));
         declare
            Loop_Par : constant Node_Id := Loop_Parameter_Specification (N);

         begin
            Preanalyze (Loop_Par);

            if Nkind (Discrete_Subtype_Definition (Loop_Par)) = N_Function_Call
              and then Parent (Loop_Par) /= N
            then
               --  The parser cannot distinguish between a loop specification
               --  and an iterator specification. If after pre-analysis the
               --  proper form has been recognized, rewrite the expression to
               --  reflect the right kind. This is needed for proper ASIS
               --  navigation. If expansion is enabled, the transformation is
               --  performed when the expression is rewritten as a loop.

               Set_Iterator_Specification (N,
                 New_Copy_Tree (Iterator_Specification (Parent (Loop_Par))));

               Set_Defining_Identifier (Iterator_Specification (N),
                 Relocate_Node (Defining_Identifier (Loop_Par)));
               Set_Name (Iterator_Specification (N),
                 Relocate_Node (Discrete_Subtype_Definition (Loop_Par)));
               Set_Comes_From_Source (Iterator_Specification (N),
                 Comes_From_Source (Loop_Parameter_Specification (N)));
               Set_Loop_Parameter_Specification (N, Empty);
            end if;
         end;
      end if;

      Preanalyze_And_Resolve (Cond, Standard_Boolean);

      End_Scope;
      Set_Etype (N, Standard_Boolean);

      --  Verify that the loop variable is used within the condition of the
      --  quantified expression.

      if Present (Iterator_Specification (N)) then
         Loop_Id := Defining_Identifier (Iterator_Specification (N));
      else
         Loop_Id := Defining_Identifier (Loop_Parameter_Specification (N));
      end if;

      if Warn_On_Suspicious_Contract
        and then not Referenced (Loop_Id, Cond)
      then
         --  Generating C, this check causes spurious warnings on inlined
         --  postconditions; we can safely disable it because this check
         --  was previously performed when analyzing the internally built
         --  postconditions procedure.

         if Modify_Tree_For_C and then In_Inlined_Body then
            null;
         else
            Error_Msg_N ("?T?unused variable &", Loop_Id);
         end if;
      end if;

      --  Diagnose a possible misuse of the SOME existential quantifier. When
      --  we have a quantified expression of the form:

      --    for some X => (if P then Q [else True])

      --  any value for X that makes P False results in the if expression being
      --  trivially True, and so also results in the quantified expression
      --  being trivially True.

      if Warn_On_Suspicious_Contract
        and then not All_Present (N)
        and then Nkind (Cond) = N_If_Expression
        and then No_Else_Or_Trivial_True (Cond)
      then
         Error_Msg_N ("?T?suspicious expression", N);
         Error_Msg_N ("\\did you mean (for all X ='> (if P then Q))", N);
         Error_Msg_N ("\\or (for some X ='> P and then Q) instead'?", N);
      end if;
   end Analyze_Quantified_Expression;

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
      --  In Ada 83, reject bounds of a universal range that are not literals
      --  or entity names.

      -----------------------
      -- Check_Common_Type --
      -----------------------

      procedure Check_Common_Type (T1, T2 : Entity_Id) is
      begin
         if Covers (T1 => T1, T2 => T2)
              or else
            Covers (T1 => T2, T2 => T1)
         then
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

      Check_Function_Writable_Actuals (N);
   end Analyze_Range;

   -----------------------
   -- Analyze_Reference --
   -----------------------

   procedure Analyze_Reference (N : Node_Id) is
      P        : constant Node_Id := Prefix (N);
      E        : Entity_Id;
      T        : Entity_Id;
      Acc_Type : Entity_Id;

   begin
      Analyze (P);

      --  An interesting error check, if we take the 'Ref of an object for
      --  which a pragma Atomic or Volatile has been given, and the type of the
      --  object is not Atomic or Volatile, then we are in trouble. The problem
      --  is that no trace of the atomic/volatile status will remain for the
      --  backend to respect when it deals with the resulting pointer, since
      --  the pointer type will not be marked atomic (it is a pointer to the
      --  base type of the object).

      --  It is not clear if that can ever occur, but in case it does, we will
      --  generate an error message. Not clear if this message can ever be
      --  generated, and pretty clear that it represents a bug if it is, still
      --  seems worth checking, except in CodePeer mode where we do not really
      --  care and don't want to bother the user.

      T := Etype (P);

      if Is_Entity_Name (P)
        and then Is_Object_Reference (P)
        and then not CodePeer_Mode
      then
         E := Entity (P);
         T := Etype (P);

         if (Has_Atomic_Components   (E)
              and then not Has_Atomic_Components   (T))
           or else
            (Has_Volatile_Components (E)
              and then not Has_Volatile_Components (T))
           or else (Is_Atomic   (E) and then not Is_Atomic   (T))
           or else (Is_Volatile (E) and then not Is_Volatile (T))
         then
            Error_Msg_N ("cannot take reference to Atomic/Volatile object", N);
         end if;
      end if;

      --  Carry on with normal processing

      Acc_Type := Create_Itype (E_Allocator_Type, N);
      Set_Etype (Acc_Type,  Acc_Type);
      Set_Directly_Designated_Type (Acc_Type, Etype (P));
      Set_Etype (N, Acc_Type);
   end Analyze_Reference;

   --------------------------------
   -- Analyze_Selected_Component --
   --------------------------------

   --  Prefix is a record type or a task or protected type. In the latter case,
   --  the selector must denote a visible entry.

   procedure Analyze_Selected_Component (N : Node_Id) is
      Name          : constant Node_Id := Prefix (N);
      Sel           : constant Node_Id := Selector_Name (N);
      Act_Decl      : Node_Id;
      Comp          : Entity_Id;
      Has_Candidate : Boolean := False;
      In_Scope      : Boolean;
      Parent_N      : Node_Id;
      Pent          : Entity_Id := Empty;
      Prefix_Type   : Entity_Id;

      Type_To_Use : Entity_Id;
      --  In most cases this is the Prefix_Type, but if the Prefix_Type is
      --  a class-wide type, we use its root type, whose components are
      --  present in the class-wide type.

      Is_Single_Concurrent_Object : Boolean;
      --  Set True if the prefix is a single task or a single protected object

      procedure Find_Component_In_Instance (Rec : Entity_Id);
      --  In an instance, a component of a private extension may not be visible
      --  while it was visible in the generic. Search candidate scope for a
      --  component with the proper identifier. This is only done if all other
      --  searches have failed. If a match is found, the Etype of both N and
      --  Sel are set from this component, and the entity of Sel is set to
      --  reference this component. If no match is found, Entity (Sel) remains
      --  unset. For a derived type that is an actual of the instance, the
      --  desired component may be found in any ancestor.

      function Has_Mode_Conformant_Spec (Comp : Entity_Id) return Boolean;
      --  It is known that the parent of N denotes a subprogram call. Comp
      --  is an overloadable component of the concurrent type of the prefix.
      --  Determine whether all formals of the parent of N and Comp are mode
      --  conformant. If the parent node is not analyzed yet it may be an
      --  indexed component rather than a function call.

      function Has_Dereference (Nod : Node_Id) return Boolean;
      --  Check whether prefix includes a dereference at any level.

      --------------------------------
      -- Find_Component_In_Instance --
      --------------------------------

      procedure Find_Component_In_Instance (Rec : Entity_Id) is
         Comp : Entity_Id;
         Typ  : Entity_Id;

      begin
         Typ := Rec;
         while Present (Typ) loop
            Comp := First_Component (Typ);
            while Present (Comp) loop
               if Chars (Comp) = Chars (Sel) then
                  Set_Entity_With_Checks (Sel, Comp);
                  Set_Etype (Sel, Etype (Comp));
                  Set_Etype (N,   Etype (Comp));
                  return;
               end if;

               Next_Component (Comp);
            end loop;

            --  If not found, the component may be declared in the parent
            --  type or its full view, if any.

            if Is_Derived_Type (Typ) then
               Typ := Etype (Typ);

               if Is_Private_Type (Typ) then
                  Typ := Full_View (Typ);
               end if;

            else
               return;
            end if;
         end loop;

         --  If we fall through, no match, so no changes made

         return;
      end Find_Component_In_Instance;

      ------------------------------
      -- Has_Mode_Conformant_Spec --
      ------------------------------

      function Has_Mode_Conformant_Spec (Comp : Entity_Id) return Boolean is
         Comp_Param : Entity_Id;
         Param      : Node_Id;
         Param_Typ  : Entity_Id;

      begin
         Comp_Param := First_Formal (Comp);

         if Nkind (Parent (N)) = N_Indexed_Component then
            Param := First (Expressions (Parent (N)));
         else
            Param := First (Parameter_Associations (Parent (N)));
         end if;

         while Present (Comp_Param)
           and then Present (Param)
         loop
            Param_Typ := Find_Parameter_Type (Param);

            if Present (Param_Typ)
              and then
                not Conforming_Types
                     (Etype (Comp_Param), Param_Typ, Mode_Conformant)
            then
               return False;
            end if;

            Next_Formal (Comp_Param);
            Next (Param);
         end loop;

         --  One of the specs has additional formals; there is no match, unless
         --  this may be an indexing of a parameterless call.

         --  Note that when expansion is disabled, the corresponding record
         --  type of synchronized types is not constructed, so that there is
         --  no point is attempting an interpretation as a prefixed call, as
         --  this is bound to fail because the primitive operations will not
         --  be properly located.

         if Present (Comp_Param) or else Present (Param) then
            if Needs_No_Actuals (Comp)
              and then Is_Array_Type (Etype (Comp))
              and then not Expander_Active
            then
               return True;
            else
               return False;
            end if;
         end if;

         return True;
      end Has_Mode_Conformant_Spec;

      ---------------------
      -- Has_Dereference --
      ---------------------

      function Has_Dereference (Nod : Node_Id) return Boolean is
      begin
         if Nkind (Nod) = N_Explicit_Dereference then
            return True;

         --  When expansion is disabled an explicit dereference may not have
         --  been inserted, but if this is an access type the indirection makes
         --  the call safe.

         elsif Is_Access_Type (Etype (Nod)) then
            return True;

         elsif Nkind_In (Nod, N_Indexed_Component, N_Selected_Component) then
            return Has_Dereference (Prefix (Nod));

         else
            return False;
         end if;
      end Has_Dereference;

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
         Prefix_Type := Etype (Name);
      end if;

      if Is_Access_Type (Prefix_Type) then

         --  A RACW object can never be used as prefix of a selected component
         --  since that means it is dereferenced without being a controlling
         --  operand of a dispatching operation (RM E.2.2(16/1)). Before
         --  reporting an error, we must check whether this is actually a
         --  dispatching call in prefix form.

         if Is_Remote_Access_To_Class_Wide_Type (Prefix_Type)
           and then Comes_From_Source (N)
         then
            if Try_Object_Operation (N) then
               return;
            else
               Error_Msg_N
                 ("invalid dereference of a remote access-to-class-wide value",
                  N);
            end if;

         --  Normal case of selected component applied to access type

         else
            Error_Msg_NW (Warn_On_Dereference, "?d?implicit dereference", N);

            if Is_Entity_Name (Name) then
               Pent := Entity (Name);
            elsif Nkind (Name) = N_Selected_Component
              and then Is_Entity_Name (Selector_Name (Name))
            then
               Pent := Entity (Selector_Name (Name));
            end if;

            Prefix_Type := Process_Implicit_Dereference_Prefix (Pent, Name);
         end if;

      --  If we have an explicit dereference of a remote access-to-class-wide
      --  value, then issue an error (see RM-E.2.2(16/1)). However we first
      --  have to check for the case of a prefix that is a controlling operand
      --  of a prefixed dispatching call, as the dereference is legal in that
      --  case. Normally this condition is checked in Validate_Remote_Access_
      --  To_Class_Wide_Type, but we have to defer the checking for selected
      --  component prefixes because of the prefixed dispatching call case.
      --  Note that implicit dereferences are checked for this just above.

      elsif Nkind (Name) = N_Explicit_Dereference
        and then Is_Remote_Access_To_Class_Wide_Type (Etype (Prefix (Name)))
        and then Comes_From_Source (N)
      then
         if Try_Object_Operation (N) then
            return;
         else
            Error_Msg_N
              ("invalid dereference of a remote access-to-class-wide value",
               N);
         end if;
      end if;

      --  (Ada 2005): if the prefix is the limited view of a type, and
      --  the context already includes the full view, use the full view
      --  in what follows, either to retrieve a component of to find
      --  a primitive operation. If the prefix is an explicit dereference,
      --  set the type of the prefix to reflect this transformation.
      --  If the non-limited view is itself an incomplete type, get the
      --  full view if available.

      if From_Limited_With (Prefix_Type)
        and then Has_Non_Limited_View (Prefix_Type)
      then
         Prefix_Type := Get_Full_View (Non_Limited_View (Prefix_Type));

         if Nkind (N) = N_Explicit_Dereference then
            Set_Etype (Prefix (N), Prefix_Type);
         end if;
      end if;

      if Ekind (Prefix_Type) = E_Private_Subtype then
         Prefix_Type := Base_Type (Prefix_Type);
      end if;

      Type_To_Use := Prefix_Type;

      --  For class-wide types, use the entity list of the root type. This
      --  indirection is specially important for private extensions because
      --  only the root type get switched (not the class-wide type).

      if Is_Class_Wide_Type (Prefix_Type) then
         Type_To_Use := Root_Type (Prefix_Type);
      end if;

      --  If the prefix is a single concurrent object, use its name in error
      --  messages, rather than that of its anonymous type.

      Is_Single_Concurrent_Object :=
        Is_Concurrent_Type (Prefix_Type)
          and then Is_Internal_Name (Chars (Prefix_Type))
          and then not Is_Derived_Type (Prefix_Type)
          and then Is_Entity_Name (Name);

      Comp := First_Entity (Type_To_Use);

      --  If the selector has an original discriminant, the node appears in
      --  an instance. Replace the discriminant with the corresponding one
      --  in the current discriminated type. For nested generics, this must
      --  be done transitively, so note the new original discriminant.

      if Nkind (Sel) = N_Identifier
        and then In_Instance
        and then Present (Original_Discriminant (Sel))
      then
         Comp := Find_Corresponding_Discriminant (Sel, Prefix_Type);

         --  Mark entity before rewriting, for completeness and because
         --  subsequent semantic checks might examine the original node.

         Set_Entity (Sel, Comp);
         Rewrite (Selector_Name (N), New_Occurrence_Of (Comp, Sloc (N)));
         Set_Original_Discriminant (Selector_Name (N), Comp);
         Set_Etype (N, Etype (Comp));
         Check_Implicit_Dereference (N, Etype (Comp));

         if Is_Access_Type (Etype (Name)) then
            Insert_Explicit_Dereference (Name);
            Error_Msg_NW (Warn_On_Dereference, "?d?implicit dereference", N);
         end if;

      elsif Is_Record_Type (Prefix_Type) then

         --  Find component with given name. In an instance, if the node is
         --  known as a prefixed call, do not examine components whose
         --  visibility may be accidental.

         while Present (Comp) and then not Is_Prefixed_Call (N) loop
            if Chars (Comp) = Chars (Sel)
              and then Is_Visible_Component (Comp, N)
            then
               Set_Entity_With_Checks (Sel, Comp);
               Set_Etype (Sel, Etype (Comp));

               if Ekind (Comp) = E_Discriminant then
                  if Is_Unchecked_Union (Base_Type (Prefix_Type)) then
                     Error_Msg_N
                       ("cannot reference discriminant of unchecked union",
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

               --  Ada 2005 (AI-50217): Check wrong use of incomplete types or
               --  subtypes in a package specification.
               --  Example:

               --    limited with Pkg;
               --    package Pkg is
               --       type Acc_Inc is access Pkg.T;
               --       X : Acc_Inc;
               --       N : Natural := X.all.Comp;  --  ERROR, limited view
               --    end Pkg;                       --  Comp is not visible

               if Nkind (Name) = N_Explicit_Dereference
                 and then From_Limited_With (Etype (Prefix (Name)))
                 and then not Is_Potentially_Use_Visible (Etype (Name))
                 and then Nkind (Parent (Cunit_Entity (Current_Sem_Unit))) =
                            N_Package_Specification
               then
                  Error_Msg_NE
                    ("premature usage of incomplete}", Prefix (Name),
                     Etype (Prefix (Name)));
               end if;

               --  We never need an actual subtype for the case of a selection
               --  for a indexed component of a non-packed array, since in
               --  this case gigi generates all the checks and can find the
               --  necessary bounds information.

               --  We also do not need an actual subtype for the case of a
               --  first, last, length, or range attribute applied to a
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
                        and then
                          Nam_In (Attribute_Name (Parent_N), Name_First,
                                                             Name_Last,
                                                             Name_Length,
                                                             Name_Range)))
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

               Check_Implicit_Dereference (N, Etype (N));
               return;
            end if;

            --  If the prefix is a private extension, check only the visible
            --  components of the partial view. This must include the tag,
            --  which can appear in expanded code in a tag check.

            if Ekind (Type_To_Use) = E_Record_Type_With_Private
              and then Chars (Selector_Name (N)) /= Name_uTag
            then
               exit when Comp = Last_Entity (Type_To_Use);
            end if;

            Next_Entity (Comp);
         end loop;

         --  Ada 2005 (AI-252): The selected component can be interpreted as
         --  a prefixed view of a subprogram. Depending on the context, this is
         --  either a name that can appear in a renaming declaration, or part
         --  of an enclosing call given in prefix form.

         --  Ada 2005 (AI05-0030): In the case of dispatching requeue, the
         --  selected component should resolve to a name.

         if Ada_Version >= Ada_2005
           and then Is_Tagged_Type (Prefix_Type)
           and then not Is_Concurrent_Type (Prefix_Type)
         then
            if Nkind (Parent (N)) = N_Generic_Association
              or else Nkind (Parent (N)) = N_Requeue_Statement
              or else Nkind (Parent (N)) = N_Subprogram_Renaming_Declaration
            then
               if Find_Primitive_Operation (N) then
                  return;
               end if;

            elsif Try_Object_Operation (N) then
               return;
            end if;

            --  If the transformation fails, it will be necessary to redo the
            --  analysis with all errors enabled, to indicate candidate
            --  interpretations and reasons for each failure ???

         end if;

      elsif Is_Private_Type (Prefix_Type) then

         --  Allow access only to discriminants of the type. If the type has
         --  no full view, gigi uses the parent type for the components, so we
         --  do the same here.

         if No (Full_View (Prefix_Type)) then
            Type_To_Use := Root_Type (Base_Type (Prefix_Type));
            Comp := First_Entity (Type_To_Use);
         end if;

         while Present (Comp) loop
            if Chars (Comp) = Chars (Sel) then
               if Ekind (Comp) = E_Discriminant then
                  Set_Entity_With_Checks (Sel, Comp);
                  Generate_Reference (Comp, Sel);

                  Set_Etype (Sel, Etype (Comp));
                  Set_Etype (N,   Etype (Comp));
                  Check_Implicit_Dereference (N, Etype (N));

                  if Is_Generic_Type (Prefix_Type)
                    or else Is_Generic_Type (Root_Type (Prefix_Type))
                  then
                     Set_Original_Discriminant (Sel, Comp);
                  end if;

               --  Before declaring an error, check whether this is tagged
               --  private type and a call to a primitive operation.

               elsif Ada_Version >= Ada_2005
                 and then Is_Tagged_Type (Prefix_Type)
                 and then Try_Object_Operation (N)
               then
                  return;

               else
                  Error_Msg_Node_2 := First_Subtype (Prefix_Type);
                  Error_Msg_NE ("invisible selector& for }", N, Sel);
                  Set_Entity (Sel, Any_Id);
                  Set_Etype (N, Any_Type);
               end if;

               return;
            end if;

            Next_Entity (Comp);
         end loop;

      elsif Is_Concurrent_Type (Prefix_Type) then

         --  Find visible operation with given name. For a protected type,
         --  the possible candidates are discriminants, entries or protected
         --  procedures. For a task type, the set can only include entries or
         --  discriminants if the task type is not an enclosing scope. If it
         --  is an enclosing scope (e.g. in an inner task) then all entities
         --  are visible, but the prefix must denote the enclosing scope, i.e.
         --  can only be a direct name or an expanded name.

         Set_Etype (Sel, Any_Type);
         In_Scope := In_Open_Scopes (Prefix_Type);

         while Present (Comp) loop

            --  Do not examine private operations of the type if not within
            --  its scope.

            if Chars (Comp) = Chars (Sel) then
               if Is_Overloadable (Comp)
                 and then (In_Scope
                            or else Comp /= First_Private_Entity (Type_To_Use))
               then
                  Add_One_Interp (Sel, Comp, Etype (Comp));

                  --  If the prefix is tagged, the correct interpretation may
                  --  lie in the primitive or class-wide operations of the
                  --  type. Perform a simple conformance check to determine
                  --  whether Try_Object_Operation should be invoked even if
                  --  a visible entity is found.

                  if Is_Tagged_Type (Prefix_Type)
                    and then Nkind_In (Parent (N), N_Function_Call,
                                                   N_Indexed_Component,
                                                   N_Procedure_Call_Statement)
                    and then Has_Mode_Conformant_Spec (Comp)
                  then
                     Has_Candidate := True;
                  end if;

               --  Note: a selected component may not denote a component of a
               --  protected type (4.1.3(7)).

               elsif Ekind_In (Comp, E_Discriminant, E_Entry_Family)
                 or else (In_Scope
                            and then not Is_Protected_Type (Prefix_Type)
                            and then Is_Entity_Name (Name))
               then
                  Set_Entity_With_Checks (Sel, Comp);
                  Generate_Reference (Comp, Sel);

                  --  The selector is not overloadable, so we have a candidate
                  --  interpretation.

                  Has_Candidate := True;

               else
                  goto Next_Comp;
               end if;

               Set_Etype (Sel, Etype (Comp));
               Set_Etype (N,   Etype (Comp));

               if Ekind (Comp) = E_Discriminant then
                  Set_Original_Discriminant (Sel, Comp);
               end if;

               --  For access type case, introduce explicit dereference for
               --  more uniform treatment of entry calls.

               if Is_Access_Type (Etype (Name)) then
                  Insert_Explicit_Dereference (Name);
                  Error_Msg_NW
                    (Warn_On_Dereference, "?d?implicit dereference", N);
               end if;
            end if;

            <<Next_Comp>>
               Next_Entity (Comp);
               exit when not In_Scope
                 and then
                   Comp = First_Private_Entity (Base_Type (Prefix_Type));
         end loop;

         --  If the scope is a current instance, the prefix cannot be an
         --  expression of the same type, unless the selector designates a
         --  public operation (otherwise that would represent an attempt to
         --  reach an internal entity of another synchronized object).
         --  This is legal if prefix is an access to such type and there is
         --  a dereference, or is a component with a dereferenced prefix.
         --  It is also legal if the prefix is a component of a task type,
         --  and the selector is one of the task operations.

         if In_Scope
           and then not Is_Entity_Name (Name)
           and then not Has_Dereference (Name)
         then
            if Is_Task_Type (Prefix_Type)
              and then Present (Entity (Sel))
              and then Ekind_In (Entity (Sel), E_Entry, E_Entry_Family)
            then
               null;

            else
               Error_Msg_NE
                 ("invalid reference to internal operation of some object of "
                  & "type &", N, Type_To_Use);
               Set_Entity (Sel, Any_Id);
               Set_Etype  (Sel, Any_Type);
               return;
            end if;
         end if;

         --  If there is no visible entity with the given name or none of the
         --  visible entities are plausible interpretations, check whether
         --  there is some other primitive operation with that name.

         if Ada_Version >= Ada_2005 and then Is_Tagged_Type (Prefix_Type) then
            if (Etype (N) = Any_Type
                  or else not Has_Candidate)
              and then Try_Object_Operation (N)
            then
               return;

            --  If the context is not syntactically a procedure call, it
            --  may be a call to a primitive function declared outside of
            --  the synchronized type.

            --  If the context is a procedure call, there might still be
            --  an overloading between an entry and a primitive procedure
            --  declared outside of the synchronized type, called in prefix
            --  notation. This is harder to disambiguate because in one case
            --  the controlling formal is implicit ???

            elsif Nkind (Parent (N)) /= N_Procedure_Call_Statement
              and then Nkind (Parent (N)) /= N_Indexed_Component
              and then Try_Object_Operation (N)
            then
               return;
            end if;

            --  Ada 2012 (AI05-0090-1): If we found a candidate of a call to an
            --  entry or procedure of a tagged concurrent type we must check
            --  if there are class-wide subprograms covering the primitive. If
            --  true then Try_Object_Operation reports the error.

            if Has_Candidate
              and then Is_Concurrent_Type (Prefix_Type)
              and then Nkind (Parent (N)) = N_Procedure_Call_Statement
            then
               --  Duplicate the call. This is required to avoid problems with
               --  the tree transformations performed by Try_Object_Operation.
               --  Set properly the parent of the copied call, because it is
               --  about to be reanalyzed.

               declare
                  Par : constant Node_Id := New_Copy_Tree (Parent (N));

               begin
                  Set_Parent (Par, Parent (Parent (N)));

                  if Try_Object_Operation
                       (Sinfo.Name (Par), CW_Test_Only => True)
                  then
                     return;
                  end if;
               end;
            end if;
         end if;

         if Etype (N) = Any_Type and then Is_Protected_Type (Prefix_Type) then

            --  Case of a prefix of a protected type: selector might denote
            --  an invisible private component.

            Comp := First_Private_Entity (Base_Type (Prefix_Type));
            while Present (Comp) and then Chars (Comp) /= Chars (Sel) loop
               Next_Entity (Comp);
            end loop;

            if Present (Comp) then
               if Is_Single_Concurrent_Object then
                  Error_Msg_Node_2 := Entity (Name);
                  Error_Msg_NE ("invisible selector& for &", N, Sel);

               else
                  Error_Msg_Node_2 := First_Subtype (Prefix_Type);
                  Error_Msg_NE ("invisible selector& for }", N, Sel);
               end if;
               return;
            end if;
         end if;

         Set_Is_Overloaded (N, Is_Overloaded (Sel));

      else
         --  Invalid prefix

         Error_Msg_NE ("invalid prefix in selected component&", N, Sel);
      end if;

      --  If N still has no type, the component is not defined in the prefix

      if Etype (N) = Any_Type then

         if Is_Single_Concurrent_Object then
            Error_Msg_Node_2 := Entity (Name);
            Error_Msg_NE ("no selector& for&", N, Sel);

            Check_Misspelled_Selector (Type_To_Use, Sel);

         --  If this is a derived formal type, the parent may have different
         --  visibility at this point. Try for an inherited component before
         --  reporting an error.

         elsif Is_Generic_Type (Prefix_Type)
           and then Ekind (Prefix_Type) = E_Record_Type_With_Private
           and then Prefix_Type /= Etype (Prefix_Type)
           and then Is_Record_Type (Etype (Prefix_Type))
         then
            Set_Etype (Prefix (N), Etype (Prefix_Type));
            Analyze_Selected_Component (N);
            return;

         --  Similarly, if this is the actual for a formal derived type, or
         --  a derived type thereof, the component inherited from the generic
         --  parent may not be visible in the actual, but the selected
         --  component is legal. Climb up the derivation chain of the generic
         --  parent type until we find the proper ancestor type.

         elsif In_Instance and then Is_Tagged_Type (Prefix_Type) then
            declare
               Par : Entity_Id := Prefix_Type;
            begin
               --  Climb up derivation chain to generic actual subtype

               while not Is_Generic_Actual_Type (Par) loop
                  if Ekind (Par) = E_Record_Type then
                     Par := Parent_Subtype (Par);
                     exit when No (Par);
                  else
                     exit when Par = Etype (Par);
                     Par := Etype (Par);
                  end if;
               end loop;

               if Present (Par) and then Is_Generic_Actual_Type (Par) then

                  --  Now look for component in ancestor types

                  Par := Generic_Parent_Type (Declaration_Node (Par));
                  loop
                     Find_Component_In_Instance (Par);
                     exit when Present (Entity (Sel))
                       or else Par = Etype (Par);
                     Par := Etype (Par);
                  end loop;

               --  Another special case: the type is an extension of a private
               --  type T, is an actual in an instance, and we are in the body
               --  of the instance, so the generic body had a full view of the
               --  type declaration for T or of some ancestor that defines the
               --  component in question.

               elsif Is_Derived_Type (Type_To_Use)
                 and then Used_As_Generic_Actual (Type_To_Use)
                 and then In_Instance_Body
               then
                  Find_Component_In_Instance (Parent_Subtype (Type_To_Use));

               --  In ASIS mode the generic parent type may be absent. Examine
               --  the parent type directly for a component that may have been
               --  visible in a parent generic unit.

               elsif Is_Derived_Type (Prefix_Type) then
                  Par := Etype (Prefix_Type);
                  Find_Component_In_Instance (Par);
               end if;
            end;

            --  The search above must have eventually succeeded, since the
            --  selected component was legal in the generic.

            if No (Entity (Sel)) then
               raise Program_Error;
            end if;

            return;

         --  Component not found, specialize error message when appropriate

         else
            if Ekind (Prefix_Type) = E_Record_Subtype then

               --  Check whether this is a component of the base type which
               --  is absent from a statically constrained subtype. This will
               --  raise constraint error at run time, but is not a compile-
               --  time error. When the selector is illegal for base type as
               --  well fall through and generate a compilation error anyway.

               Comp := First_Component (Base_Type (Prefix_Type));
               while Present (Comp) loop
                  if Chars (Comp) = Chars (Sel)
                    and then Is_Visible_Component (Comp)
                  then
                     Set_Entity_With_Checks (Sel, Comp);
                     Generate_Reference (Comp, Sel);
                     Set_Etype (Sel, Etype (Comp));
                     Set_Etype (N,   Etype (Comp));

                     --  Emit appropriate message. The node will be replaced
                     --  by an appropriate raise statement.

                     --  Note that in SPARK mode, as with all calls to apply a
                     --  compile time constraint error, this will be made into
                     --  an error to simplify the processing of the formal
                     --  verification backend.

                     Apply_Compile_Time_Constraint_Error
                       (N, "component not present in }??",
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

            --  Add information in the case of an incomplete prefix

            if Is_Incomplete_Type (Type_To_Use) then
               declare
                  Inc : constant Entity_Id := First_Subtype (Type_To_Use);

               begin
                  if From_Limited_With (Scope (Type_To_Use)) then
                     Error_Msg_NE
                       ("\limited view of& has no components", N, Inc);

                  else
                     Error_Msg_NE
                       ("\premature usage of incomplete type&", N, Inc);

                     if Nkind (Parent (Inc)) =
                                          N_Incomplete_Type_Declaration
                     then
                        --  Record location of premature use in entity so that
                        --  a continuation message is generated when the
                        --  completion is seen.

                        Set_Premature_Use (Parent (Inc), N);
                     end if;
                  end if;
               end;
            end if;

            Check_Misspelled_Selector (Type_To_Use, Sel);
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

      --  Here we have failed to find an interpretation. Clearly we know that
      --  it is not the case that both operands can have an interpretation of
      --  Boolean, but this is by far the most likely intended interpretation.
      --  So we simply resolve both operands as Booleans, and at least one of
      --  these resolutions will generate an error message, and we do not need
      --  to give another error message on the short circuit operation itself.

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
      D          : constant Node_Id := Discrete_Range (N);
      P          : constant Node_Id := Prefix (N);
      Array_Type : Entity_Id;
      Index_Type : Entity_Id;

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
               Error_Msg_NW
                 (Warn_On_Dereference, "?d?implicit dereference", N);
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
      if Comes_From_Source (N) then
         Check_SPARK_05_Restriction ("slice is not allowed", N);
      end if;

      Analyze (P);
      Analyze (D);

      if Is_Overloaded (P) then
         Analyze_Overloaded_Slice;

      else
         Array_Type := Etype (P);
         Set_Etype (N, Any_Type);

         if Is_Access_Type (Array_Type) then
            Array_Type := Designated_Type (Array_Type);
            Error_Msg_NW (Warn_On_Dereference, "?d?implicit dereference", N);
         end if;

         if not Is_Array_Type (Array_Type) then
            Wrong_Type (P, Any_Array);

         elsif Number_Dimensions (Array_Type) > 1 then
            Error_Msg_N
              ("type is not one-dimensional array in slice prefix", N);

         else
            if Ekind (Array_Type) = E_String_Literal_Subtype then
               Index_Type := Etype (String_Literal_Low_Bound (Array_Type));
            else
               Index_Type := Etype (First_Index (Array_Type));
            end if;

            if not Has_Compatible_Type (D, Index_Type) then
               Wrong_Type (D, Index_Type);
            else
               Set_Etype (N, Array_Type);
            end if;
         end if;
      end if;
   end Analyze_Slice;

   -----------------------------
   -- Analyze_Type_Conversion --
   -----------------------------

   procedure Analyze_Type_Conversion (N : Node_Id) is
      Expr : constant Node_Id := Expression (N);
      Typ  : Entity_Id;

   begin
      --  If Conversion_OK is set, then the Etype is already set, and the only
      --  processing required is to analyze the expression. This is used to
      --  construct certain "illegal" conversions which are not allowed by Ada
      --  semantics, but can be handled by Gigi, see Sinfo for further details.

      if Conversion_OK (N) then
         Analyze (Expr);
         return;
      end if;

      --  Otherwise full type analysis is required, as well as some semantic
      --  checks to make sure the argument of the conversion is appropriate.

      Find_Type (Subtype_Mark (N));
      Typ := Entity (Subtype_Mark (N));
      Set_Etype (N, Typ);
      Check_Fully_Declared (Typ, N);
      Analyze_Expression (Expr);
      Validate_Remote_Type_Type_Conversion (N);

      --  Only remaining step is validity checks on the argument. These
      --  are skipped if the conversion does not come from the source.

      if not Comes_From_Source (N) then
         return;

      --  If there was an error in a generic unit, no need to replicate the
      --  error message. Conversely, constant-folding in the generic may
      --  transform the argument of a conversion into a string literal, which
      --  is legal. Therefore the following tests are not performed in an
      --  instance. The same applies to an inlined body.

      elsif In_Instance or In_Inlined_Body then
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
            Resolve (Expr, Typ);
         else
            Error_Msg_N ("argument of conversion cannot be character literal",
              N);
            Error_Msg_N ("\use qualified expression instead", N);
         end if;

      elsif Nkind (Expr) = N_Attribute_Reference
        and then Nam_In (Attribute_Name (Expr), Name_Access,
                                                Name_Unchecked_Access,
                                                Name_Unrestricted_Access)
      then
         Error_Msg_N ("argument of conversion cannot be access", N);
         Error_Msg_N ("\use qualified expression instead", N);
      end if;

      --  A formal parameter of a specific tagged type whose related subprogram
      --  is subject to pragma Extensions_Visible with value "False" cannot
      --  appear in a class-wide conversion (SPARK RM 6.1.7(3)). Do not check
      --  internally generated expressions.

      if Is_Class_Wide_Type (Typ)
        and then Comes_From_Source (Expr)
        and then Is_EVF_Expression (Expr)
      then
         Error_Msg_N
           ("formal parameter cannot be converted to class-wide type when "
            & "Extensions_Visible is False", Expr);
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

               --  If the left operand is overloaded, indicate that the current
               --  type is a viable candidate. This is redundant in most cases,
               --  but for equality and comparison operators where the context
               --  does not impose a type on the operands, setting the proper
               --  type is necessary to avoid subsequent ambiguities during
               --  resolution, when both user-defined and predefined operators
               --  may be candidates.

               if Is_Overloaded (Left_Opnd (N)) then
                  Set_Etype (Left_Opnd (N), Etype (F1));
               end if;

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
      Op_Name : constant Name_Id := Chars (Op_Id);

      function Has_Fixed_Op (Typ : Entity_Id; Op : Entity_Id) return Boolean;
      --  Check whether the fixed-point type Typ has a user-defined operator
      --  (multiplication or division) that should hide the corresponding
      --  predefined operator. Used to implement Ada 2005 AI-264, to make
      --  such operators more visible and therefore useful.
      --
      --  If the name of the operation is an expanded name with prefix
      --  Standard, the predefined universal fixed operator is available,
      --  as specified by AI-420 (RM 4.5.5 (19.1/2)).

      function Specific_Type (T1, T2 : Entity_Id) return Entity_Id;
      --  Get specific type (i.e. non-universal type if there is one)

      ------------------
      -- Has_Fixed_Op --
      ------------------

      function Has_Fixed_Op (Typ : Entity_Id; Op : Entity_Id) return Boolean is
         Bas : constant Entity_Id := Base_Type (Typ);
         Ent : Entity_Id;
         F1  : Entity_Id;
         F2  : Entity_Id;

      begin
         --  If the universal_fixed operation is given explicitly the rule
         --  concerning primitive operations of the type do not apply.

         if Nkind (N) = N_Function_Call
           and then Nkind (Name (N)) = N_Expanded_Name
           and then Entity (Prefix (Name (N))) = Standard_Standard
         then
            return False;
         end if;

         --  The operation is treated as primitive if it is declared in the
         --  same scope as the type, and therefore on the same entity chain.

         Ent := Next_Entity (Typ);
         while Present (Ent) loop
            if Chars (Ent) = Chars (Op) then
               F1 := First_Formal (Ent);
               F2 := Next_Formal (F1);

               --  The operation counts as primitive if either operand or
               --  result are of the given base type, and both operands are
               --  fixed point types.

               if (Base_Type (Etype (F1)) = Bas
                    and then Is_Fixed_Point_Type (Etype (F2)))

                 or else
                   (Base_Type (Etype (F2)) = Bas
                     and then Is_Fixed_Point_Type (Etype (F1)))

                 or else
                   (Base_Type (Etype (Ent)) = Bas
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
      if Nam_In (Op_Name, Name_Op_Add, Name_Op_Subtract) then
         if Is_Numeric_Type (T1)
           and then Is_Numeric_Type (T2)
           and then (Covers (T1 => T1, T2 => T2)
                       or else
                     Covers (T1 => T2, T2 => T1))
         then
            Add_One_Interp (N, Op_Id, Specific_Type (T1, T2));
         end if;

      elsif Nam_In (Op_Name, Name_Op_Multiply, Name_Op_Divide) then
         if Is_Fixed_Point_Type (T1)
           and then (Is_Fixed_Point_Type (T2) or else T2 = Universal_Real)
         then
            --  If Treat_Fixed_As_Integer is set then the Etype is already set
            --  and no further processing is required (this is the case of an
            --  operator constructed by Exp_Fixd for a fixed point operation)
            --  Otherwise add one interpretation with universal fixed result
            --  If the operator is given in functional notation, it comes
            --  from source and Fixed_As_Integer cannot apply.

            if (Nkind (N) not in N_Op
                 or else not Treat_Fixed_As_Integer (N))
              and then
                (not Has_Fixed_Op (T1, Op_Id)
                  or else Nkind (Parent (N)) = N_Type_Conversion)
            then
               Add_One_Interp (N, Op_Id, Universal_Fixed);
            end if;

         elsif Is_Fixed_Point_Type (T2)
           and then (Nkind (N) not in N_Op
                      or else not Treat_Fixed_As_Integer (N))
           and then T1 = Universal_Real
           and then
             (not Has_Fixed_Op (T1, Op_Id)
               or else Nkind (Parent (N)) = N_Type_Conversion)
         then
            Add_One_Interp (N, Op_Id, Universal_Fixed);

         elsif Is_Numeric_Type (T1)
           and then Is_Numeric_Type (T2)
           and then (Covers (T1 => T1, T2 => T2)
                       or else
                     Covers (T1 => T2, T2 => T1))
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
           and then (Covers (T1 => T1, T2 => T2)
                       or else
                     Covers (T1 => T2, T2 => T1))
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
      --  All the components of the prefix of selector Sel are matched against
      --  Sel and a count is maintained of possible misspellings. When at
      --  the end of the analysis there are one or two (not more) possible
      --  misspellings, these misspellings will be suggested as possible
      --  correction.

      if not (Is_Private_Type (Prefix) or else Is_Record_Type (Prefix)) then

         --  Concurrent types should be handled as well ???

         return;
      end if;

      Comp  := First_Entity (Prefix);
      while Nr_Of_Suggestions <= Max_Suggestions and then Present (Comp) loop
         if Is_Visible_Component (Comp) then
            if Is_Bad_Spelling_Of (Chars (Comp), Chars (Sel)) then
               Nr_Of_Suggestions := Nr_Of_Suggestions + 1;

               case Nr_Of_Suggestions is
                  when 1      => Suggestion_1 := Comp;
                  when 2      => Suggestion_2 := Comp;
                  when others => null;
               end case;
            end if;
         end if;

         Comp := Next_Entity (Comp);
      end loop;

      --  Report at most two suggestions

      if Nr_Of_Suggestions = 1 then
         Error_Msg_NE -- CODEFIX
           ("\possible misspelling of&", Sel, Suggestion_1);

      elsif Nr_Of_Suggestions = 2 then
         Error_Msg_Node_2 := Suggestion_2;
         Error_Msg_NE -- CODEFIX
           ("\possible misspelling of& or&", Sel, Suggestion_1);
      end if;
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
      Err_Mode         : Boolean;
      New_Nam          : Node_Id;
      Void_Interp_Seen : Boolean := False;

      Success : Boolean;
      pragma Warnings (Off, Boolean);

   begin
      if Ada_Version >= Ada_2005 then
         Actual := First_Actual (N);
         while Present (Actual) loop

            --  Ada 2005 (AI-50217): Post an error in case of premature
            --  usage of an entity from the limited view.

            if not Analyzed (Etype (Actual))
             and then From_Limited_With (Etype (Actual))
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
            if Ekind_In (It.Nam, E_Function, E_Operator) then
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
            Error_Msg_N -- CODEFIX
              ("\period should probably be semicolon", Parent (N));
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

   --  Start of processing for Find_Arithmetic_Types

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

         --  If operands are aggregates, we must assume that they may be
         --  boolean arrays, and leave disambiguation for the second pass.
         --  If only one is an aggregate, verify that the other one has an
         --  interpretation as a boolean array

         elsif Nkind (L) = N_Aggregate then
            if Nkind (R) = N_Aggregate then
               Add_One_Interp (N, Op_Id, Etype (L));

            elsif not Is_Overloaded (R) then
               if Valid_Boolean_Arg (Etype (R)) then
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

         if Valid_Comparison_Arg (T1) and then Has_Compatible_Type (R, T1) then
            if Found and then Base_Type (T1) /= Base_Type (T_F) then
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

   --  Start of processing for Find_Comparison_Types

   begin
      --  If left operand is aggregate, the right operand has to
      --  provide a usable type for it.

      if Nkind (L) = N_Aggregate and then Nkind (R) /= N_Aggregate then
         Find_Comparison_Types (L => R, R => L, Op_Id => Op_Id, N => N);
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
      if T1 = Universal_Integer or else T1 = Universal_Real

        --  If the left operand of an equality operator is null, the visibility
        --  of the operator must be determined from the interpretation of the
        --  right operand. This processing must be done for Any_Access, which
        --  is the internal representation of the type of the literal null.

        or else T1 = Any_Access
      then
         if not Is_Overloaded (R) then
            Add_One_Interp (N, Op_Id, Standard_Boolean, Base_Type (Etype (R)));
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
      --  The context of the equality operator plays no role in resolving the
      --  arguments, so that if there is more than one interpretation of the
      --  operands that is compatible with equality, the construct is ambiguous
      --  and an error can be emitted now, after trying to disambiguate, i.e.
      --  applying preference rules.

      --------------------
      -- Try_One_Interp --
      --------------------

      procedure Try_One_Interp (T1 : Entity_Id) is
         Bas : constant Entity_Id := Base_Type (T1);

      begin
         --  If the operator is an expanded name, then the type of the operand
         --  must be defined in the corresponding scope. If the type is
         --  universal, the context will impose the correct type. An anonymous
         --  type for a 'Access reference is also universal in this sense, as
         --  the actual type is obtained from context.

         --  In Ada 2005, the equality operator for anonymous access types
         --  is declared in Standard, and preference rules apply to it.

         if Present (Scop) then
            if Defined_In_Scope (T1, Scop)
              or else T1 = Universal_Integer
              or else T1 = Universal_Real
              or else T1 = Any_Access
              or else T1 = Any_String
              or else T1 = Any_Composite
              or else (Ekind (T1) = E_Access_Subprogram_Type
                        and then not Comes_From_Source (T1))
            then
               null;

            elsif Ekind (T1) = E_Anonymous_Access_Type
              and then Scop = Standard_Standard
            then
               null;

            else
               --  The scope does not contain an operator for the type

               return;
            end if;

         --  If we have infix notation, the operator must be usable. Within
         --  an instance, if the type is already established we know it is
         --  correct. If an operand is universal it is compatible with any
         --  numeric type.

         elsif In_Open_Scopes (Scope (Bas))
           or else Is_Potentially_Use_Visible (Bas)
           or else In_Use (Bas)
           or else (In_Use (Scope (Bas)) and then not Is_Hidden (Bas))

            --  In an instance, the type may have been immediately visible.
            --  Either the types are compatible, or one operand is universal
            --  (numeric or null).

           or else (In_Instance
                     and then
                       (First_Subtype (T1) = First_Subtype (Etype (R))
                         or else Nkind (R) = N_Null
                         or else
                           (Is_Numeric_Type (T1)
                             and then Is_Universal_Numeric_Type (Etype (R)))))

           --  In Ada 2005, the equality on anonymous access types is declared
           --  in Standard, and is always visible.

           or else Ekind (T1) = E_Anonymous_Access_Type
         then
            null;

         else
            --  Save candidate type for subsequent error message, if any

            if not Is_Limited_Type (T1) then
               Candidate_Type := T1;
            end if;

            return;
         end if;

         --  Ada 2005 (AI-230): Keep restriction imposed by Ada 83 and 95:
         --  Do not allow anonymous access types in equality operators.

         if Ada_Version < Ada_2005
           and then Ekind (T1) = E_Anonymous_Access_Type
         then
            return;
         end if;

         --  If the right operand has a type compatible with T1, check for an
         --  acceptable interpretation, unless T1 is limited (no predefined
         --  equality available), or this is use of a "/=" for a tagged type.
         --  In the latter case, possible interpretations of equality need
         --  to be considered, we don't want the default inequality declared
         --  in Standard to be chosen, and the "/=" will be rewritten as a
         --  negation of "=" (see the end of Analyze_Equality_Op). This ensures
         --  that rewriting happens during analysis rather than being
         --  delayed until expansion (this is needed for ASIS, which only sees
         --  the unexpanded tree). Note that if the node is N_Op_Ne, but Op_Id
         --  is Name_Op_Eq then we still proceed with the interpretation,
         --  because that indicates the potential rewriting case where the
         --  interpretation to consider is actually "=" and the node may be
         --  about to be rewritten by Analyze_Equality_Op.

         if T1 /= Standard_Void_Type
           and then Has_Compatible_Type (R, T1)

           and then
             ((not Is_Limited_Type (T1)
                and then not Is_Limited_Composite (T1))

               or else
                 (Is_Array_Type (T1)
                   and then not Is_Limited_Type (Component_Type (T1))
                   and then Available_Full_View_Of_Component (T1)))

           and then
             (Nkind (N) /= N_Op_Ne
               or else not Is_Tagged_Type (T1)
               or else Chars (Op_Id) = Name_Op_Eq)
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

         elsif Scop = Standard_Standard
           and then Ekind (T1) = E_Anonymous_Access_Type
         then
            Found := True;
         end if;
      end Try_One_Interp;

   --  Start of processing for Find_Equality_Types

   begin
      --  If left operand is aggregate, the right operand has to
      --  provide a usable type for it.

      if Nkind (L) = N_Aggregate
        and then Nkind (R) /= N_Aggregate
      then
         Find_Equality_Types (L => R, R => L, Op_Id => Op_Id, N => N);
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

   ------------------------------
   -- Find_Primitive_Operation --
   ------------------------------

   function Find_Primitive_Operation (N : Node_Id) return Boolean is
      Obj : constant Node_Id := Prefix (N);
      Op  : constant Node_Id := Selector_Name (N);

      Prim  : Elmt_Id;
      Prims : Elist_Id;
      Typ   : Entity_Id;

   begin
      Set_Etype (Op, Any_Type);

      if Is_Access_Type (Etype (Obj)) then
         Typ := Designated_Type (Etype (Obj));
      else
         Typ := Etype (Obj);
      end if;

      if Is_Class_Wide_Type (Typ) then
         Typ := Root_Type (Typ);
      end if;

      Prims := Primitive_Operations (Typ);

      Prim := First_Elmt (Prims);
      while Present (Prim) loop
         if Chars (Node (Prim)) = Chars (Op) then
            Add_One_Interp (Op, Node (Prim), Etype (Node (Prim)));
            Set_Etype (N, Etype (Node (Prim)));
         end if;

         Next_Elmt (Prim);
      end loop;

      --  Now look for class-wide operations of the type or any of its
      --  ancestors by iterating over the homonyms of the selector.

      declare
         Cls_Type : constant Entity_Id := Class_Wide_Type (Typ);
         Hom      : Entity_Id;

      begin
         Hom := Current_Entity (Op);
         while Present (Hom) loop
            if (Ekind (Hom) = E_Procedure
                  or else
                Ekind (Hom) = E_Function)
              and then Scope (Hom) = Scope (Typ)
              and then Present (First_Formal (Hom))
              and then
                (Base_Type (Etype (First_Formal (Hom))) = Cls_Type
                  or else
                    (Is_Access_Type (Etype (First_Formal (Hom)))
                      and then
                        Ekind (Etype (First_Formal (Hom))) =
                          E_Anonymous_Access_Type
                      and then
                        Base_Type
                          (Designated_Type (Etype (First_Formal (Hom)))) =
                                                                Cls_Type))
            then
               Add_One_Interp (Op, Hom, Etype (Hom));
               Set_Etype (N, Etype (Hom));
            end if;

            Hom := Homonym (Hom);
         end loop;
      end;

      return Etype (Op) /= Any_Type;
   end Find_Primitive_Operation;

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

            --  In an instance a generic actual may be a numeric type even if
            --  the formal in the generic unit was not. In that case, the
            --  predefined operator was not a possible interpretation in the
            --  generic, and cannot be one in the instance, unless the operator
            --  is an actual of an instance.

            if In_Instance
              and then
                not Is_Numeric_Type (Corresponding_Generic_Type (Etype (R)))
            then
               null;
            else
               Add_One_Interp (N, Op_Id, Base_Type (Etype (R)));
            end if;
         end if;

      else
         Get_First_Interp (R, Index, It);
         while Present (It.Typ) loop
            if Is_Numeric_Type (It.Typ) then
               if In_Instance
                 and then
                   not Is_Numeric_Type
                     (Corresponding_Generic_Type (Etype (It.Typ)))
               then
                  null;

               else
                  Add_One_Interp (N, Op_Id, Base_Type (It.Typ));
               end if;
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

      --  Now test the entity we got to see if it is a bad case

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
            L     : Node_Id;
            R     : Node_Id;
            Op_Id : Entity_Id := Empty;

         begin
            R := Right_Opnd (N);

            if Nkind (N) in N_Binary_Op then
               L := Left_Opnd (N);
            else
               L := Empty;
            end if;

            --  If either operand has no type, then don't complain further,
            --  since this simply means that we have a propagated error.

            if R = Error
              or else Etype (R) = Any_Type
              or else (Nkind (N) in N_Binary_Op and then Etype (L) = Any_Type)
            then
               --  For the rather unusual case where one of the operands is
               --  a Raise_Expression, whose initial type is Any_Type, use
               --  the type of the other operand.

               if Nkind (L) = N_Raise_Expression then
                  Set_Etype (L, Etype (R));
                  Set_Etype (N, Etype (R));

               elsif Nkind (R) = N_Raise_Expression then
                  Set_Etype (R, Etype (L));
                  Set_Etype (N, Etype (L));
               end if;

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

               Error_Msg_NE -- CODEFIX
                 ("operator for} is not directly visible!",
                  N, First_Subtype (Candidate_Type));

               declare
                  U : constant Node_Id :=
                        Cunit (Get_Source_Unit (Candidate_Type));
               begin
                  if Unit_Is_Visible (U) then
                     Error_Msg_N -- CODEFIX
                       ("use clause would make operation legal!",  N);
                  else
                     Error_Msg_NE  --  CODEFIX
                       ("add with_clause and use_clause for&!",
                        N, Defining_Entity (Unit (U)));
                  end if;
               end;
               return;

            --  If either operand is a junk operand (e.g. package name), then
            --  post appropriate error messages, but do not complain further.

            --  Note that the use of OR in this test instead of OR ELSE is
            --  quite deliberate, we may as well check both operands in the
            --  binary operator case.

            elsif Junk_Operand (R)
              or  -- really mean OR here and not OR ELSE, see above
                (Nkind (N) in N_Binary_Op and then Junk_Operand (L))
            then
               return;

            --  If we have a logical operator, one of whose operands is
            --  Boolean, then we know that the other operand cannot resolve to
            --  Boolean (since we got no interpretations), but in that case we
            --  pretty much know that the other operand should be Boolean, so
            --  resolve it that way (generating an error).

            elsif Nkind_In (N, N_Op_And, N_Op_Or, N_Op_Xor) then
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

            elsif Nkind_In (N, N_Op_Add,
                               N_Op_Divide,
                               N_Op_Ge,
                               N_Op_Gt,
                               N_Op_Le)
              or else
                  Nkind_In (N, N_Op_Lt,
                               N_Op_Mod,
                               N_Op_Multiply,
                               N_Op_Rem,
                               N_Op_Subtract)
            then
               --  If Allow_Integer_Address is active, check whether the
               --  operation becomes legal after converting an operand.

               if Is_Numeric_Type (Etype (L))
                 and then not Is_Numeric_Type (Etype (R))
               then
                  if Address_Integer_Convert_OK (Etype (R), Etype (L)) then
                     Rewrite (R,
                       Unchecked_Convert_To (Etype (L), Relocate_Node (R)));

                     if Nkind_In (N, N_Op_Ge, N_Op_Gt, N_Op_Le, N_Op_Lt) then
                        Analyze_Comparison_Op (N);
                     else
                        Analyze_Arithmetic_Op (N);
                     end if;
                  else
                     Resolve (R, Etype (L));
                  end if;

                  return;

               elsif Is_Numeric_Type (Etype (R))
                 and then not Is_Numeric_Type (Etype (L))
               then
                  if Address_Integer_Convert_OK (Etype (L), Etype (R)) then
                     Rewrite (L,
                       Unchecked_Convert_To (Etype (R), Relocate_Node (L)));

                     if Nkind_In (N, N_Op_Ge, N_Op_Gt, N_Op_Le, N_Op_Lt) then
                        Analyze_Comparison_Op (N);
                     else
                        Analyze_Arithmetic_Op (N);
                     end if;

                     return;

                  else
                     Resolve (L, Etype (R));
                  end if;

                  return;

               elsif Allow_Integer_Address
                 and then Is_Descendant_Of_Address (Etype (L))
                 and then Is_Descendant_Of_Address (Etype (R))
                 and then not Error_Posted (N)
               then
                  declare
                     Addr_Type : constant Entity_Id := Etype (L);

                  begin
                     Rewrite (L,
                       Unchecked_Convert_To (
                         Standard_Integer, Relocate_Node (L)));
                     Rewrite (R,
                       Unchecked_Convert_To (
                         Standard_Integer, Relocate_Node (R)));

                     if Nkind_In (N, N_Op_Ge, N_Op_Gt, N_Op_Le, N_Op_Lt) then
                        Analyze_Comparison_Op (N);
                     else
                        Analyze_Arithmetic_Op (N);
                     end if;

                     --  If this is an operand in an enclosing arithmetic
                     --  operation, Convert the result as an address so that
                     --  arithmetic folding of address can continue.

                     if Nkind (Parent (N)) in N_Op then
                        Rewrite (N,
                          Unchecked_Convert_To (Addr_Type, Relocate_Node (N)));
                     end if;

                     return;
                  end;

               --  Under relaxed RM semantics silently replace occurrences of
               --  null by System.Address_Null.

               elsif Null_To_Null_Address_Convert_OK (N) then
                  Replace_Null_By_Null_Address (N);

                  if Nkind_In (N, N_Op_Ge, N_Op_Gt, N_Op_Le, N_Op_Lt) then
                     Analyze_Comparison_Op (N);
                  else
                     Analyze_Arithmetic_Op (N);
                  end if;

                  return;
               end if;

            --  Comparisons on A'Access are common enough to deserve a
            --  special message.

            elsif Nkind_In (N, N_Op_Eq, N_Op_Ne)
               and then Ekind (Etype (L)) = E_Access_Attribute_Type
               and then Ekind (Etype (R)) = E_Access_Attribute_Type
            then
               Error_Msg_N
                 ("two access attributes cannot be compared directly", N);
               Error_Msg_N
                 ("\use qualified expression for one of the operands",
                   N);
               return;

            --  Another one for C programmers

            elsif Nkind (N) = N_Op_Concat
              and then Valid_Boolean_Arg (Etype (L))
              and then Valid_Boolean_Arg (Etype (R))
            then
               Error_Msg_N ("invalid operands for concatenation", N);
               Error_Msg_N -- CODEFIX
                 ("\maybe AND was meant", N);
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

            --  Another special case for exponentiation, where the right
            --  operand must be Natural, independently of the base.

            elsif Nkind (N) = N_Op_Expon
              and then Is_Numeric_Type (Etype (L))
              and then not Is_Overloaded (R)
              and then
                First_Subtype (Base_Type (Etype (R))) /= Standard_Integer
              and then Base_Type (Etype (R)) /= Universal_Integer
            then
               if Ada_Version >= Ada_2012
                 and then Has_Dimension_System (Etype (L))
               then
                  Error_Msg_NE
                    ("exponent for dimensioned type must be a rational" &
                     ", found}", R, Etype (R));
               else
                  Error_Msg_NE
                    ("exponent must be of type Natural, found}", R, Etype (R));
               end if;

               return;

            elsif Nkind_In (N, N_Op_Eq, N_Op_Ne) then
               if Address_Integer_Convert_OK (Etype (R), Etype (L)) then
                  Rewrite (R,
                    Unchecked_Convert_To (Etype (L), Relocate_Node (R)));
                  Analyze_Equality_Op (N);
                  return;

               --  Under relaxed RM semantics silently replace occurrences of
               --  null by System.Address_Null.

               elsif Null_To_Null_Address_Convert_OK (N) then
                  Replace_Null_By_Null_Address (N);
                  Analyze_Equality_Op (N);
                  return;
               end if;
            end if;

            --  If we fall through then just give general message. Note that in
            --  the following messages, if the operand is overloaded we choose
            --  an arbitrary type to complain about, but that is probably more
            --  useful than not giving a type at all.

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
                     --  Another attempt to find a fix: one of the candidate
                     --  interpretations may not be use-visible. This has
                     --  already been checked for predefined operators, so
                     --  we examine only user-defined functions.

                     Op_Id := Get_Name_Entity_Id (Chars (N));

                     while Present (Op_Id) loop
                        if Ekind (Op_Id) /= E_Operator
                          and then Is_Overloadable (Op_Id)
                        then
                           if not Is_Immediately_Visible (Op_Id)
                             and then not In_Use (Scope (Op_Id))
                             and then not Is_Abstract_Subprogram (Op_Id)
                             and then not Is_Hidden (Op_Id)
                             and then Ekind (Scope (Op_Id)) = E_Package
                             and then
                               Has_Compatible_Type
                                 (L, Etype (First_Formal (Op_Id)))
                             and then Present
                              (Next_Formal (First_Formal (Op_Id)))
                             and then
                               Has_Compatible_Type
                                 (R,
                                  Etype (Next_Formal (First_Formal (Op_Id))))
                           then
                              Error_Msg_N
                                ("No legal interpretation for operator&", N);
                              Error_Msg_NE
                                ("\use clause on& would make operation legal",
                                 N, Scope (Op_Id));
                              exit;
                           end if;
                        end if;

                        Op_Id := Homonym (Op_Id);
                     end loop;

                     if No (Op_Id) then
                        Error_Msg_N ("invalid operand types for operator&", N);

                        if Nkind (N) /= N_Op_Concat then
                           Error_Msg_NE ("\left operand has}!",  N, Etype (L));
                           Error_Msg_NE ("\right operand has}!", N, Etype (R));

                        --  For concatenation operators it is more difficult to
                        --  determine which is the wrong operand. It is worth
                        --  flagging explicitly an access type, for those who
                        --  might think that a dereference happens here.

                        elsif Is_Access_Type (Etype (L)) then
                           Error_Msg_N ("\left operand is access type", N);

                        elsif Is_Access_Type (Etype (R)) then
                           Error_Msg_N ("\right operand is access type", N);
                        end if;
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

   function Process_Implicit_Dereference_Prefix
     (E : Entity_Id;
      P : Entity_Id) return Entity_Id
   is
      Ref : Node_Id;
      Typ : constant Entity_Id := Designated_Type (Etype (P));

   begin
      if Present (E)
        and then (Operating_Mode = Check_Semantics or else not Expander_Active)
      then
         --  We create a dummy reference to E to ensure that the reference is
         --  not considered as part of an assignment (an implicit dereference
         --  can never assign to its prefix). The Comes_From_Source attribute
         --  needs to be propagated for accurate warnings.

         Ref := New_Occurrence_Of (E, Sloc (P));
         Set_Comes_From_Source (Ref, Comes_From_Source (P));
         Generate_Reference (E, Ref);
      end if;

      --  An implicit dereference is a legal occurrence of an incomplete type
      --  imported through a limited_with clause, if the full view is visible.

      if From_Limited_With (Typ)
        and then not From_Limited_With (Scope (Typ))
        and then
          (Is_Immediately_Visible (Scope (Typ))
            or else
              (Is_Child_Unit (Scope (Typ))
                and then Is_Visible_Lib_Unit (Scope (Typ))))
      then
         return Available_View (Typ);
      else
         return Typ;
      end if;
   end Process_Implicit_Dereference_Prefix;

   --------------------------------
   -- Remove_Abstract_Operations --
   --------------------------------

   procedure Remove_Abstract_Operations (N : Node_Id) is
      Abstract_Op        : Entity_Id := Empty;
      Address_Descendant : Boolean := False;
      I                  : Interp_Index;
      It                 : Interp;

      --  AI-310: If overloaded, remove abstract non-dispatching operations. We
      --  activate this if either extensions are enabled, or if the abstract
      --  operation in question comes from a predefined file. This latter test
      --  allows us to use abstract to make operations invisible to users. In
      --  particular, if type Address is non-private and abstract subprograms
      --  are used to hide its operators, they will be truly hidden.

      type Operand_Position is (First_Op, Second_Op);
      Univ_Type : constant Entity_Id := Universal_Interpretation (N);

      procedure Remove_Address_Interpretations (Op : Operand_Position);
      --  Ambiguities may arise when the operands are literal and the address
      --  operations in s-auxdec are visible. In that case, remove the
      --  interpretation of a literal as Address, to retain the semantics
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

               if Is_Descendant_Of_Address (Etype (Formal)) then
                  Address_Descendant := True;
                  Remove_Interp (I);
               end if;

               Get_Next_Interp (I, It);
            end loop;
         end if;
      end Remove_Address_Interpretations;

   --  Start of processing for Remove_Abstract_Operations

   begin
      if Is_Overloaded (N) then
         if Debug_Flag_V then
            Write_Str ("Remove_Abstract_Operations: ");
            Write_Overloads (N);
         end if;

         Get_First_Interp (N, I, It);

         while Present (It.Nam) loop
            if Is_Overloadable (It.Nam)
              and then Is_Abstract_Subprogram (It.Nam)
              and then not Is_Dispatching_Operation (It.Nam)
            then
               Abstract_Op := It.Nam;

               if Is_Descendant_Of_Address (It.Typ) then
                  Address_Descendant := True;
                  Remove_Interp (I);
                  exit;

               --  In Ada 2005, this operation does not participate in overload
               --  resolution. If the operation is defined in a predefined
               --  unit, it is one of the operations declared abstract in some
               --  variants of System, and it must be removed as well.

               elsif Ada_Version >= Ada_2005
                 or else Is_Predefined_File_Name
                           (Unit_File_Name (Get_Source_Unit (It.Nam)))
               then
                  Remove_Interp (I);
                  exit;
               end if;
            end if;

            Get_Next_Interp (I, It);
         end loop;

         if No (Abstract_Op) then

            --  If some interpretation yields an integer type, it is still
            --  possible that there are address interpretations. Remove them
            --  if one operand is a literal, to avoid spurious ambiguities
            --  on systems where Address is a visible integer type.

            if Is_Overloaded (N)
              and then Nkind (N) in N_Op
              and then Is_Integer_Type (Etype (N))
            then
               if Nkind (N) in N_Binary_Op then
                  if Nkind (Right_Opnd (N)) = N_Integer_Literal then
                     Remove_Address_Interpretations (Second_Op);

                  elsif Nkind (Right_Opnd (N)) = N_Integer_Literal then
                     Remove_Address_Interpretations (First_Op);
                  end if;
               end if;
            end if;

         elsif Nkind (N) in N_Op then

            --  Remove interpretations that treat literals as addresses. This
            --  is never appropriate, even when Address is defined as a visible
            --  Integer type. The reason is that we would really prefer Address
            --  to behave as a private type, even in this case. If Address is a
            --  visible integer type, we get lots of overload ambiguities.

            if Nkind (N) in N_Binary_Op then
               declare
                  U1 : constant Boolean :=
                         Present (Universal_Interpretation (Right_Opnd (N)));
                  U2 : constant Boolean :=
                         Present (Universal_Interpretation (Left_Opnd (N)));

               begin
                  if U1 then
                     Remove_Address_Interpretations (Second_Op);
                  end if;

                  if U2 then
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
                     --  it is still necessary to remove interpretations that
                     --  yield Address. Any remaining ambiguities will be
                     --  removed in Disambiguate.

                     Get_First_Interp (N, I, It);
                     while Present (It.Nam) loop
                        if Is_Descendant_Of_Address (It.Typ) then
                           Remove_Interp (I);

                        elsif not Is_Type (It.Nam) then
                           Set_Entity (N, It.Nam);
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
               if U1 then
                  Remove_Address_Interpretations (First_Op);
               end if;

               if U2 then
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

         --  If the removal has left no valid interpretations, emit an error
         --  message now and label node as illegal.

         if Present (Abstract_Op) then
            Get_First_Interp (N, I, It);

            if No (It.Nam) then

               --  Removal of abstract operation left no viable candidate

               Set_Etype (N, Any_Type);
               Error_Msg_Sloc := Sloc (Abstract_Op);
               Error_Msg_NE
                 ("cannot call abstract operation& declared#", N, Abstract_Op);

            --  In Ada 2005, an abstract operation may disable predefined
            --  operators. Since the context is not yet known, we mark the
            --  predefined operators as potentially hidden. Do not include
            --  predefined operators when addresses are involved since this
            --  case is handled separately.

            elsif Ada_Version >= Ada_2005 and then not Address_Descendant then
               while Present (It.Nam) loop
                  if Is_Numeric_Type (It.Typ)
                    and then Scope (It.Typ) = Standard_Standard
                  then
                     Set_Abstract_Op (I, Abstract_Op);
                  end if;

                  Get_Next_Interp (I, It);
               end loop;
            end if;
         end if;

         if Debug_Flag_V then
            Write_Str ("Remove_Abstract_Operations done: ");
            Write_Overloads (N);
         end if;
      end if;
   end Remove_Abstract_Operations;

   ----------------------------
   -- Try_Container_Indexing --
   ----------------------------

   function Try_Container_Indexing
     (N      : Node_Id;
      Prefix : Node_Id;
      Exprs  : List_Id) return Boolean
   is
      Pref_Typ : constant Entity_Id := Etype (Prefix);

      function Constant_Indexing_OK return Boolean;
      --  Constant_Indexing is legal if there is no Variable_Indexing defined
      --  for the type, or else node not a target of assignment, or an actual
      --  for an IN OUT or OUT formal (RM 4.1.6 (11)).

      function Find_Indexing_Operations
        (T           : Entity_Id;
         Nam         : Name_Id;
         Is_Constant : Boolean) return Node_Id;
      --  Return a reference to the primitive operation of type T denoted by
      --  name Nam. If the operation is overloaded, the reference carries all
      --  interpretations. Flag Is_Constant should be set when the context is
      --  constant indexing.

      --------------------------
      -- Constant_Indexing_OK --
      --------------------------

      function Constant_Indexing_OK return Boolean is
         Par : Node_Id;

      begin
         if No (Find_Value_Of_Aspect (Pref_Typ, Aspect_Variable_Indexing)) then
            return True;

         elsif not Is_Variable (Prefix) then
            return True;
         end if;

         Par := N;
         while Present (Par) loop
            if Nkind (Parent (Par)) = N_Assignment_Statement
              and then Par = Name (Parent (Par))
            then
               return False;

            --  The call may be overloaded, in which case we assume that its
            --  resolution does not depend on the type of the parameter that
            --  includes the indexing operation.

            elsif Nkind_In (Parent (Par), N_Function_Call,
                                          N_Procedure_Call_Statement)
              and then Is_Entity_Name (Name (Parent (Par)))
            then
               declare
                  Actual : Node_Id;
                  Formal : Entity_Id;
                  Proc   : Entity_Id;

               begin
                  --  We should look for an interpretation with the proper
                  --  number of formals, and determine whether it is an
                  --  In_Parameter, but for now we examine the formal that
                  --  corresponds to the indexing, and assume that variable
                  --  indexing is required if some interpretation has an
                  --  assignable formal at that position.  Still does not
                  --  cover the most complex cases ???

                  if Is_Overloaded (Name (Parent (Par))) then
                     declare
                        Proc : constant Node_Id := Name (Parent (Par));
                        A    : Node_Id;
                        F    : Entity_Id;
                        I    : Interp_Index;
                        It   : Interp;

                     begin
                        Get_First_Interp (Proc, I, It);
                        while Present (It.Nam) loop
                           F := First_Formal (It.Nam);
                           A := First (Parameter_Associations (Parent (Par)));

                           while Present (F) and then Present (A) loop
                              if A = Par then
                                 if Ekind (F) /= E_In_Parameter then
                                    return False;
                                 else
                                    exit;  --  interpretation is safe
                                 end if;
                              end if;

                              Next_Formal (F);
                              Next_Actual (A);
                           end loop;

                           Get_Next_Interp (I, It);
                        end loop;
                     end;

                     return True;

                  else
                     Proc := Entity (Name (Parent (Par)));

                     --  If this is an indirect call, get formals from
                     --  designated type.

                     if Is_Access_Subprogram_Type (Etype (Proc)) then
                        Proc := Designated_Type (Etype (Proc));
                     end if;
                  end if;

                  Formal := First_Formal (Proc);
                  Actual := First_Actual (Parent (Par));

                  --  Find corresponding actual

                  while Present (Actual) loop
                     exit when Actual = Par;
                     Next_Actual (Actual);

                     if Present (Formal) then
                        Next_Formal (Formal);

                     --  Otherwise this is a parameter mismatch, the error is
                     --  reported elsewhere.

                     else
                        return False;
                     end if;
                  end loop;

                  return Ekind (Formal) = E_In_Parameter;
               end;

            elsif Nkind (Parent (Par)) = N_Object_Renaming_Declaration then
               return False;

            --  If the indexed component is a prefix it may be the first actual
            --  of a prefixed call. Retrieve the called entity, if any, and
            --  check its first formal. Determine if the context is a procedure
            --  or function call.

            elsif Nkind (Parent (Par)) = N_Selected_Component then
               declare
                  Sel : constant Node_Id   := Selector_Name (Parent (Par));
                  Nam : constant Entity_Id := Current_Entity (Sel);

               begin
                  if Present (Nam) and then Is_Overloadable (Nam) then
                     if Nkind (Parent (Parent (Par))) =
                          N_Procedure_Call_Statement
                     then
                        return False;

                     elsif Ekind (Nam) = E_Function
                       and then Present (First_Formal (Nam))
                     then
                        return Ekind (First_Formal (Nam)) = E_In_Parameter;
                     end if;
                  end if;
               end;

            elsif Nkind (Par) in N_Op then
               return True;
            end if;

            Par := Parent (Par);
         end loop;

         --  In all other cases, constant indexing is legal

         return True;
      end Constant_Indexing_OK;

      ------------------------------
      -- Find_Indexing_Operations --
      ------------------------------

      function Find_Indexing_Operations
        (T           : Entity_Id;
         Nam         : Name_Id;
         Is_Constant : Boolean) return Node_Id
      is
         procedure Inspect_Declarations
           (Typ : Entity_Id;
            Ref : in out Node_Id);
         --  Traverse the declarative list where type Typ resides and collect
         --  all suitable interpretations in node Ref.

         procedure Inspect_Primitives
           (Typ : Entity_Id;
            Ref : in out Node_Id);
         --  Traverse the list of primitive operations of type Typ and collect
         --  all suitable interpretations in node Ref.

         function Is_OK_Candidate
           (Subp_Id : Entity_Id;
            Typ     : Entity_Id) return Boolean;
         --  Determine whether subprogram Subp_Id is a suitable indexing
         --  operation for type Typ. To qualify as such, the subprogram must
         --  be a function, have at least two parameters, and the type of the
         --  first parameter must be either Typ, or Typ'Class, or access [to
         --  constant] with designated type Typ or Typ'Class.

         procedure Record_Interp (Subp_Id : Entity_Id; Ref : in out Node_Id);
         --  Store subprogram Subp_Id as an interpretation in node Ref

         --------------------------
         -- Inspect_Declarations --
         --------------------------

         procedure Inspect_Declarations
           (Typ : Entity_Id;
            Ref : in out Node_Id)
         is
            Typ_Decl : constant Node_Id := Declaration_Node (Typ);
            Decl     : Node_Id;
            Subp_Id  : Entity_Id;

         begin
            --  Ensure that the routine is not called with itypes, which lack a
            --  declarative node.

            pragma Assert (Present (Typ_Decl));
            pragma Assert (Is_List_Member (Typ_Decl));

            Decl := First (List_Containing (Typ_Decl));
            while Present (Decl) loop
               if Nkind (Decl) = N_Subprogram_Declaration then
                  Subp_Id := Defining_Entity (Decl);

                  if Is_OK_Candidate (Subp_Id, Typ) then
                     Record_Interp (Subp_Id, Ref);
                  end if;
               end if;

               Next (Decl);
            end loop;
         end Inspect_Declarations;

         ------------------------
         -- Inspect_Primitives --
         ------------------------

         procedure Inspect_Primitives
           (Typ : Entity_Id;
            Ref : in out Node_Id)
         is
            Prim_Elmt : Elmt_Id;
            Prim_Id   : Entity_Id;

         begin
            Prim_Elmt := First_Elmt (Primitive_Operations (Typ));
            while Present (Prim_Elmt) loop
               Prim_Id := Node (Prim_Elmt);

               if Is_OK_Candidate (Prim_Id, Typ) then
                  Record_Interp (Prim_Id, Ref);
               end if;

               Next_Elmt (Prim_Elmt);
            end loop;
         end Inspect_Primitives;

         ---------------------
         -- Is_OK_Candidate --
         ---------------------

         function Is_OK_Candidate
           (Subp_Id : Entity_Id;
            Typ     : Entity_Id) return Boolean
         is
            Formal     : Entity_Id;
            Formal_Typ : Entity_Id;
            Param_Typ  : Node_Id;

         begin
            --  To classify as a suitable candidate, the subprogram must be a
            --  function whose name matches the argument of aspect Constant or
            --  Variable_Indexing.

            if Ekind (Subp_Id) = E_Function and then Chars (Subp_Id) = Nam then
               Formal := First_Formal (Subp_Id);

               --  The candidate requires at least two parameters

               if Present (Formal) and then Present (Next_Formal (Formal)) then
                  Formal_Typ := Empty;
                  Param_Typ  := Parameter_Type (Parent (Formal));

                  --  Use the designated type when the first parameter is of an
                  --  access type.

                  if Nkind (Param_Typ) = N_Access_Definition
                    and then Present (Subtype_Mark (Param_Typ))
                  then
                     --  When the context is a constant indexing, the access
                     --  definition must be access-to-constant. This does not
                     --  apply to variable indexing.

                     if not Is_Constant
                       or else Constant_Present (Param_Typ)
                     then
                        Formal_Typ := Etype (Subtype_Mark (Param_Typ));
                     end if;

                  --  Otherwise use the parameter type

                  else
                     Formal_Typ := Etype (Param_Typ);
                  end if;

                  if Present (Formal_Typ) then

                     --  Use the specific type when the parameter type is
                     --  class-wide.

                     if Is_Class_Wide_Type (Formal_Typ) then
                        Formal_Typ := Etype (Base_Type (Formal_Typ));
                     end if;

                     --  Use the full view when the parameter type is private
                     --  or incomplete.

                     if Is_Incomplete_Or_Private_Type (Formal_Typ)
                       and then Present (Full_View (Formal_Typ))
                     then
                        Formal_Typ := Full_View (Formal_Typ);
                     end if;

                     --  The type of the first parameter must denote the type
                     --  of the container or acts as its ancestor type.

                     return
                       Formal_Typ = Typ
                         or else Is_Ancestor (Formal_Typ, Typ);
                  end if;
               end if;
            end if;

            return False;
         end Is_OK_Candidate;

         -------------------
         -- Record_Interp --
         -------------------

         procedure Record_Interp (Subp_Id : Entity_Id; Ref : in out Node_Id) is
         begin
            if Present (Ref) then
               Add_One_Interp (Ref, Subp_Id, Etype (Subp_Id));

            --  Otherwise this is the first interpretation. Create a reference
            --  where all remaining interpretations will be collected.

            else
               Ref := New_Occurrence_Of (Subp_Id, Sloc (T));
            end if;
         end Record_Interp;

         --  Local variables

         Ref : Node_Id;
         Typ : Entity_Id;

      --  Start of processing for Find_Indexing_Operations

      begin
         Typ := T;

         --  Use the specific type when the parameter type is class-wide

         if Is_Class_Wide_Type (Typ) then
            Typ := Root_Type (Typ);
         end if;

         Ref := Empty;
         Typ := Underlying_Type (Base_Type (Typ));

         Inspect_Primitives   (Typ, Ref);
         Inspect_Declarations (Typ, Ref);

         return Ref;
      end Find_Indexing_Operations;

      --  Local variables

      Loc       : constant Source_Ptr := Sloc (N);
      Assoc     : List_Id;
      C_Type    : Entity_Id;
      Func      : Entity_Id;
      Func_Name : Node_Id;
      Indexing  : Node_Id;

      Is_Constant_Indexing : Boolean := False;
      --  This flag reflects the nature of the container indexing. Note that
      --  the context may be suited for constant indexing, but the type may
      --  lack a Constant_Indexing annotation.

   --  Start of processing for Try_Container_Indexing

   begin
      --  Node may have been analyzed already when testing for a prefixed
      --  call, in which case do not redo analysis.

      if Present (Generalized_Indexing (N)) then
         return True;
      end if;

      C_Type := Pref_Typ;

      --  If indexing a class-wide container, obtain indexing primitive from
      --  specific type.

      if Is_Class_Wide_Type (C_Type) then
         C_Type := Etype (Base_Type (C_Type));
      end if;

      --  Check whether the type has a specified indexing aspect

      Func_Name := Empty;

      --  The context is suitable for constant indexing, so obtain the name of
      --  the indexing function from aspect Constant_Indexing.

      if Constant_Indexing_OK then
         Func_Name :=
           Find_Value_Of_Aspect (Pref_Typ, Aspect_Constant_Indexing);
      end if;

      if Present (Func_Name) then
         Is_Constant_Indexing := True;

      --  Otherwise attempt variable indexing

      else
         Func_Name :=
           Find_Value_Of_Aspect (Pref_Typ, Aspect_Variable_Indexing);
      end if;

      --  The type is not subject to either form of indexing, therefore the
      --  indexed component does not denote container indexing. If this is a
      --  true error, it is diagnosed by the caller.

      if No (Func_Name) then

         --  The prefix itself may be an indexing of a container. Rewrite it
         --  as such and retry.

         if Has_Implicit_Dereference (Pref_Typ) then
            Build_Explicit_Dereference (Prefix, First_Discriminant (Pref_Typ));
            return Try_Container_Indexing (N, Prefix, Exprs);

         --  Otherwise this is definitely not container indexing

         else
            return False;
         end if;

      --  If the container type is derived from another container type, the
      --  value of the inherited aspect is the Reference operation declared
      --  for the parent type.

      --  However, Reference is also a primitive operation of the type, and the
      --  inherited operation has a different signature. We retrieve the right
      --  ones (the function may be overloaded) from the list of primitive
      --  operations of the derived type.

      --  Note that predefined containers are typically all derived from one of
      --  the Controlled types. The code below is motivated by containers that
      --  are derived from other types with a Reference aspect.

      elsif Is_Derived_Type (C_Type)
        and then Etype (First_Formal (Entity (Func_Name))) /= Pref_Typ
      then
         Func_Name :=
           Find_Indexing_Operations
             (T           => C_Type,
              Nam         => Chars (Func_Name),
              Is_Constant => Is_Constant_Indexing);
      end if;

      Assoc := New_List (Relocate_Node (Prefix));

      --  A generalized indexing may have nore than one index expression, so
      --  transfer all of them to the argument list to be used in the call.
      --  Note that there may be named associations, in which case the node
      --  was rewritten earlier as a call, and has been transformed back into
      --  an indexed expression to share the following processing.

      --  The generalized indexing node is the one on which analysis and
      --  resolution take place. Before expansion the original node is replaced
      --  with the generalized indexing node, which is a call, possibly with a
      --  dereference operation.

      if Comes_From_Source (N) then
         Check_Compiler_Unit ("generalized indexing", N);
      end if;

      --  Create argument list for function call that represents generalized
      --  indexing. Note that indices (i.e. actuals) may themselves be
      --  overloaded.

      declare
         Arg     : Node_Id;
         New_Arg : Node_Id;

      begin
         Arg := First (Exprs);
         while Present (Arg) loop
            New_Arg := Relocate_Node (Arg);

            --  The arguments can be parameter associations, in which case the
            --  explicit actual parameter carries the overloadings.

            if Nkind (New_Arg) /= N_Parameter_Association then
               Save_Interps (Arg, New_Arg);
            end if;

            Append (New_Arg, Assoc);
            Next (Arg);
         end loop;
      end;

      if not Is_Overloaded (Func_Name) then
         Func := Entity (Func_Name);
         Indexing :=
           Make_Function_Call (Loc,
             Name                   => New_Occurrence_Of (Func, Loc),
             Parameter_Associations => Assoc);
         Set_Parent (Indexing, Parent (N));
         Set_Generalized_Indexing (N, Indexing);
         Analyze (Indexing);
         Set_Etype (N, Etype (Indexing));

         --  If the return type of the indexing function is a reference type,
         --  add the dereference as a possible interpretation. Note that the
         --  indexing aspect may be a function that returns the element type
         --  with no intervening implicit dereference, and that the reference
         --  discriminant is not the first discriminant.

         if Has_Discriminants (Etype (Func)) then
            Check_Implicit_Dereference (N, Etype (Func));
         end if;

      else
         --  If there are multiple indexing functions, build a function call
         --  and analyze it for each of the possible interpretations.

         Indexing :=
           Make_Function_Call (Loc,
             Name                   =>
               Make_Identifier (Loc, Chars (Func_Name)),
             Parameter_Associations => Assoc);

         Set_Parent (Indexing, Parent (N));
         Set_Generalized_Indexing (N, Indexing);
         Set_Etype (N, Any_Type);
         Set_Etype (Name (Indexing), Any_Type);

         declare
            I       : Interp_Index;
            It      : Interp;
            Success : Boolean;

         begin
            Get_First_Interp (Func_Name, I, It);
            Set_Etype (Indexing, Any_Type);

            --  Analyze eacn candidae function with the given actuals

            while Present (It.Nam) loop
               Analyze_One_Call (Indexing, It.Nam, False, Success);
               Get_Next_Interp (I, It);
            end loop;

            --  If there are several successful candidates, resolution will
            --  be by result. Mark the interpretations of the function name
            --  itself.

            if Is_Overloaded (Indexing) then
               Get_First_Interp (Indexing, I, It);

               while Present (It.Nam) loop
                  Add_One_Interp (Name (Indexing), It.Nam, It.Typ);
                  Get_Next_Interp (I, It);
               end loop;

            else
               Set_Etype (Name (Indexing), Etype (Indexing));
            end if;

            --  Now add the candidate interpretations to the indexing node
            --  itself, to be replaced later by the function call.

            if Is_Overloaded (Name (Indexing)) then
               Get_First_Interp (Name (Indexing), I, It);

               while Present (It.Nam) loop
                  Add_One_Interp (N, It.Nam, It.Typ);

                  --  Add dereference interpretation if the result type has
                  --  implicit reference discriminants.

                  if Has_Discriminants (Etype (It.Nam)) then
                     Check_Implicit_Dereference (N, Etype (It.Nam));
                  end if;

                  Get_Next_Interp (I, It);
               end loop;

            else
               Set_Etype (N, Etype (Name (Indexing)));
               if Has_Discriminants (Etype (N)) then
                  Check_Implicit_Dereference (N, Etype (N));
               end if;
            end if;
         end;
      end if;

      if Etype (Indexing) = Any_Type then
         Error_Msg_NE
           ("container cannot be indexed with&", N, Etype (First (Exprs)));
         Rewrite (N, New_Occurrence_Of (Any_Id, Loc));
      end if;

      return True;
   end Try_Container_Indexing;

   -----------------------
   -- Try_Indirect_Call --
   -----------------------

   function Try_Indirect_Call
     (N   : Node_Id;
      Nam : Entity_Id;
      Typ : Entity_Id) return Boolean
   is
      Actual : Node_Id;
      Formal : Entity_Id;

      Call_OK : Boolean;
      pragma Warnings (Off, Call_OK);

   begin
      Normalize_Actuals (N, Designated_Type (Typ), False, Call_OK);

      Actual := First_Actual (N);
      Formal := First_Formal (Designated_Type (Typ));
      while Present (Actual) and then Present (Formal) loop
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
     (N          : Node_Id;
      Nam        : Entity_Id;
      Typ        : Entity_Id;
      Skip_First : Boolean) return Boolean
   is
      Loc     : constant Source_Ptr := Sloc (N);
      Actuals : constant List_Id    := Parameter_Associations (N);
      Actual  : Node_Id;
      Index   : Entity_Id;

   begin
      Actual := First (Actuals);

      --  If the call was originally written in prefix form, skip the first
      --  actual, which is obviously not defaulted.

      if Skip_First then
         Next (Actual);
      end if;

      Index := First_Index (Typ);
      while Present (Actual) and then Present (Index) loop

         --  If the parameter list has a named association, the expression
         --  is definitely a call and not an indexed component.

         if Nkind (Actual) = N_Parameter_Association then
            return False;
         end if;

         if Is_Entity_Name (Actual)
           and then Is_Type (Entity (Actual))
           and then No (Next (Actual))
         then
            --  A single actual that is a type name indicates a slice if the
            --  type is discrete, and an error otherwise.

            if Is_Discrete_Type (Entity (Actual)) then
               Rewrite (N,
                 Make_Slice (Loc,
                   Prefix =>
                     Make_Function_Call (Loc,
                       Name => Relocate_Node (Name (N))),
                   Discrete_Range =>
                     New_Occurrence_Of (Entity (Actual), Sloc (Actual))));

               Analyze (N);

            else
               Error_Msg_N ("invalid use of type in expression", Actual);
               Set_Etype (N, Any_Type);
            end if;

            return True;

         elsif not Has_Compatible_Type (Actual, Etype (Index)) then
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

   function Try_Object_Operation
     (N : Node_Id; CW_Test_Only : Boolean := False) return Boolean
   is
      K              : constant Node_Kind  := Nkind (Parent (N));
      Is_Subprg_Call : constant Boolean    := K in N_Subprogram_Call;
      Loc            : constant Source_Ptr := Sloc (N);
      Obj            : constant Node_Id    := Prefix (N);

      Subprog : constant Node_Id    :=
                  Make_Identifier (Sloc (Selector_Name (N)),
                    Chars => Chars (Selector_Name (N)));
      --  Identifier on which possible interpretations will be collected

      Report_Error : Boolean := False;
      --  If no candidate interpretation matches the context, redo analysis
      --  with Report_Error True to provide additional information.

      Actual          : Node_Id;
      Candidate       : Entity_Id := Empty;
      New_Call_Node   : Node_Id := Empty;
      Node_To_Replace : Node_Id;
      Obj_Type        : Entity_Id := Etype (Obj);
      Success         : Boolean := False;

      function Valid_Candidate
        (Success : Boolean;
         Call    : Node_Id;
         Subp    : Entity_Id) return Entity_Id;
      --  If the subprogram is a valid interpretation, record it, and add
      --  to the list of interpretations of Subprog. Otherwise return Empty.

      procedure Complete_Object_Operation
        (Call_Node       : Node_Id;
         Node_To_Replace : Node_Id);
      --  Make Subprog the name of Call_Node, replace Node_To_Replace with
      --  Call_Node, insert the object (or its dereference) as the first actual
      --  in the call, and complete the analysis of the call.

      procedure Report_Ambiguity (Op : Entity_Id);
      --  If a prefixed procedure call is ambiguous, indicate whether the
      --  call includes an implicit dereference or an implicit 'Access.

      procedure Transform_Object_Operation
        (Call_Node       : out Node_Id;
         Node_To_Replace : out Node_Id);
      --  Transform Obj.Operation (X, Y,,) into Operation (Obj, X, Y ..)
      --  Call_Node is the resulting subprogram call, Node_To_Replace is
      --  either N or the parent of N, and Subprog is a reference to the
      --  subprogram we are trying to match.

      function Try_Class_Wide_Operation
        (Call_Node       : Node_Id;
         Node_To_Replace : Node_Id) return Boolean;
      --  Traverse all ancestor types looking for a class-wide subprogram
      --  for which the current operation is a valid non-dispatching call.

      procedure Try_One_Prefix_Interpretation (T : Entity_Id);
      --  If prefix is overloaded, its interpretation may include different
      --  tagged types, and we must examine the primitive operations and
      --  the class-wide operations of each in order to find candidate
      --  interpretations for the call as a whole.

      function Try_Primitive_Operation
        (Call_Node       : Node_Id;
         Node_To_Replace : Node_Id) return Boolean;
      --  Traverse the list of primitive subprograms looking for a dispatching
      --  operation for which the current node is a valid call .

      ---------------------
      -- Valid_Candidate --
      ---------------------

      function Valid_Candidate
        (Success : Boolean;
         Call    : Node_Id;
         Subp    : Entity_Id) return Entity_Id
      is
         Arr_Type  : Entity_Id;
         Comp_Type : Entity_Id;

      begin
         --  If the subprogram is a valid interpretation, record it in global
         --  variable Subprog, to collect all possible overloadings.

         if Success then
            if Subp /= Entity (Subprog) then
               Add_One_Interp (Subprog, Subp, Etype (Subp));
            end if;
         end if;

         --  If the call may be an indexed call, retrieve component type of
         --  resulting expression, and add possible interpretation.

         Arr_Type  := Empty;
         Comp_Type := Empty;

         if Nkind (Call) = N_Function_Call
           and then Nkind (Parent (N)) = N_Indexed_Component
           and then Needs_One_Actual (Subp)
         then
            if Is_Array_Type (Etype (Subp)) then
               Arr_Type := Etype (Subp);

            elsif Is_Access_Type (Etype (Subp))
              and then Is_Array_Type (Designated_Type (Etype (Subp)))
            then
               Arr_Type := Designated_Type (Etype (Subp));
            end if;
         end if;

         if Present (Arr_Type) then

            --  Verify that the actuals (excluding the object) match the types
            --  of the indexes.

            declare
               Actual : Node_Id;
               Index  : Node_Id;

            begin
               Actual := Next (First_Actual (Call));
               Index  := First_Index (Arr_Type);
               while Present (Actual) and then Present (Index) loop
                  if not Has_Compatible_Type (Actual, Etype (Index)) then
                     Arr_Type := Empty;
                     exit;
                  end if;

                  Next_Actual (Actual);
                  Next_Index  (Index);
               end loop;

               if No (Actual)
                  and then No (Index)
                  and then Present (Arr_Type)
               then
                  Comp_Type := Component_Type (Arr_Type);
               end if;
            end;

            if Present (Comp_Type)
              and then Etype (Subprog) /= Comp_Type
            then
               Add_One_Interp (Subprog, Subp, Comp_Type);
            end if;
         end if;

         if Etype (Call) /= Any_Type then
            return Subp;
         else
            return Empty;
         end if;
      end Valid_Candidate;

      -------------------------------
      -- Complete_Object_Operation --
      -------------------------------

      procedure Complete_Object_Operation
        (Call_Node       : Node_Id;
         Node_To_Replace : Node_Id)
      is
         Control      : constant Entity_Id := First_Formal (Entity (Subprog));
         Formal_Type  : constant Entity_Id := Etype (Control);
         First_Actual : Node_Id;

      begin
         --  Place the name of the operation, with its interpretations,
         --  on the rewritten call.

         Set_Name (Call_Node, Subprog);

         First_Actual := First (Parameter_Associations (Call_Node));

         --  For cross-reference purposes, treat the new node as being in the
         --  source if the original one is. Set entity and type, even though
         --  they may be overwritten during resolution if overloaded.

         Set_Comes_From_Source (Subprog, Comes_From_Source (N));
         Set_Comes_From_Source (Call_Node, Comes_From_Source (N));

         if Nkind (N) = N_Selected_Component
           and then not Inside_A_Generic
         then
            Set_Entity (Selector_Name (N), Entity (Subprog));
            Set_Etype  (Selector_Name (N), Etype (Entity (Subprog)));
         end if;

         --  If need be, rewrite first actual as an explicit dereference. If
         --  the call is overloaded, the rewriting can only be done once the
         --  primitive operation is identified.

         if Is_Overloaded (Subprog) then

            --  The prefix itself may be overloaded, and its interpretations
            --  must be propagated to the new actual in the call.

            if Is_Overloaded (Obj) then
               Save_Interps (Obj, First_Actual);
            end if;

            Rewrite (First_Actual, Obj);

         elsif not Is_Access_Type (Formal_Type)
           and then Is_Access_Type (Etype (Obj))
         then
            Rewrite (First_Actual,
              Make_Explicit_Dereference (Sloc (Obj), Obj));
            Analyze (First_Actual);

            --  If we need to introduce an explicit dereference, verify that
            --  the resulting actual is compatible with the mode of the formal.

            if Ekind (First_Formal (Entity (Subprog))) /= E_In_Parameter
              and then Is_Access_Constant (Etype (Obj))
            then
               Error_Msg_NE
                 ("expect variable in call to&", Prefix (N), Entity (Subprog));
            end if;

         --  Conversely, if the formal is an access parameter and the object
         --  is not, replace the actual with a 'Access reference. Its analysis
         --  will check that the object is aliased.

         elsif Is_Access_Type (Formal_Type)
           and then not Is_Access_Type (Etype (Obj))
         then
            --  A special case: A.all'access is illegal if A is an access to a
            --  constant and the context requires an access to a variable.

            if not Is_Access_Constant (Formal_Type) then
               if (Nkind (Obj) = N_Explicit_Dereference
                    and then Is_Access_Constant (Etype (Prefix (Obj))))
                 or else not Is_Variable (Obj)
               then
                  Error_Msg_NE
                    ("actual for & must be a variable", Obj, Control);
               end if;
            end if;

            Rewrite (First_Actual,
              Make_Attribute_Reference (Loc,
                Attribute_Name => Name_Access,
                Prefix => Relocate_Node (Obj)));

            if not Is_Aliased_View (Obj) then
               Error_Msg_NE
                 ("object in prefixed call to & must be aliased "
                  & "(RM 4.1.3 (13 1/2))", Prefix (First_Actual), Subprog);
            end if;

            Analyze (First_Actual);

         else
            if Is_Overloaded (Obj) then
               Save_Interps (Obj, First_Actual);
            end if;

            Rewrite (First_Actual, Obj);
         end if;

         --  The operation is obtained from the dispatch table and not by
         --  visibility, and may be declared in a unit that is not explicitly
         --  referenced in the source, but is nevertheless required in the
         --  context of the current unit. Indicate that operation and its scope
         --  are referenced, to prevent spurious and misleading warnings. If
         --  the operation is overloaded, all primitives are in the same scope
         --  and we can use any of them.

         Set_Referenced (Entity (Subprog), True);
         Set_Referenced (Scope (Entity (Subprog)), True);

         Rewrite (Node_To_Replace, Call_Node);

         --  Propagate the interpretations collected in subprog to the new
         --  function call node, to be resolved from context.

         if Is_Overloaded (Subprog) then
            Save_Interps (Subprog, Node_To_Replace);

         else
            --  The type of the subprogram may be a limited view obtained
            --  transitively from another unit. If full view is available,
            --  use it to analyze call.

            declare
               T : constant Entity_Id := Etype (Subprog);
            begin
               if From_Limited_With (T) then
                  Set_Etype (Entity (Subprog), Available_View (T));
               end if;
            end;

            Analyze (Node_To_Replace);

            --  If the operation has been rewritten into a call, which may get
            --  subsequently an explicit dereference, preserve the type on the
            --  original node (selected component or indexed component) for
            --  subsequent legality tests, e.g. Is_Variable. which examines
            --  the original node.

            if Nkind (Node_To_Replace) = N_Function_Call then
               Set_Etype
                 (Original_Node (Node_To_Replace), Etype (Node_To_Replace));
            end if;
         end if;
      end Complete_Object_Operation;

      ----------------------
      -- Report_Ambiguity --
      ----------------------

      procedure Report_Ambiguity (Op : Entity_Id) is
         Access_Actual : constant Boolean :=
                           Is_Access_Type (Etype (Prefix (N)));
         Access_Formal : Boolean := False;

      begin
         Error_Msg_Sloc := Sloc (Op);

         if Present (First_Formal (Op)) then
            Access_Formal := Is_Access_Type (Etype (First_Formal (Op)));
         end if;

         if Access_Formal and then not Access_Actual then
            if Nkind (Parent (Op)) = N_Full_Type_Declaration then
               Error_Msg_N
                 ("\possible interpretation "
                  & "(inherited, with implicit 'Access) #", N);
            else
               Error_Msg_N
                 ("\possible interpretation (with implicit 'Access) #", N);
            end if;

         elsif not Access_Formal and then Access_Actual then
            if Nkind (Parent (Op)) = N_Full_Type_Declaration then
               Error_Msg_N
                 ("\possible interpretation "
                  & "(inherited, with implicit dereference) #", N);
            else
               Error_Msg_N
                 ("\possible interpretation (with implicit dereference) #", N);
            end if;

         else
            if Nkind (Parent (Op)) = N_Full_Type_Declaration then
               Error_Msg_N ("\possible interpretation (inherited)#", N);
            else
               Error_Msg_N -- CODEFIX
                 ("\possible interpretation#", N);
            end if;
         end if;
      end Report_Ambiguity;

      --------------------------------
      -- Transform_Object_Operation --
      --------------------------------

      procedure Transform_Object_Operation
        (Call_Node       : out Node_Id;
         Node_To_Replace : out Node_Id)
      is
         Dummy : constant Node_Id := New_Copy (Obj);
         --  Placeholder used as a first parameter in the call, replaced
         --  eventually by the proper object.

         Parent_Node : constant Node_Id := Parent (N);

         Actual  : Node_Id;
         Actuals : List_Id;

      begin
         --  Common case covering 1) Call to a procedure and 2) Call to a
         --  function that has some additional actuals.

         if Nkind (Parent_Node) in N_Subprogram_Call

            --  N is a selected component node containing the name of the
            --  subprogram. If N is not the name of the parent node we must
            --  not replace the parent node by the new construct. This case
            --  occurs when N is a parameterless call to a subprogram that
            --  is an actual parameter of a call to another subprogram. For
            --  example:
            --            Some_Subprogram (..., Obj.Operation, ...)

            and then Name (Parent_Node) = N
         then
            Node_To_Replace := Parent_Node;

            Actuals := Parameter_Associations (Parent_Node);

            if Present (Actuals) then
               Prepend (Dummy, Actuals);
            else
               Actuals := New_List (Dummy);
            end if;

            if Nkind (Parent_Node) = N_Procedure_Call_Statement then
               Call_Node :=
                 Make_Procedure_Call_Statement (Loc,
                   Name => New_Copy (Subprog),
                   Parameter_Associations => Actuals);

            else
               Call_Node :=
                 Make_Function_Call (Loc,
                   Name                   => New_Copy (Subprog),
                   Parameter_Associations => Actuals);
            end if;

         --  Before analysis, a function call appears as an indexed component
         --  if there are no named associations.

         elsif Nkind (Parent_Node) = N_Indexed_Component
           and then N = Prefix (Parent_Node)
         then
            Node_To_Replace := Parent_Node;
            Actuals := Expressions (Parent_Node);

            Actual := First (Actuals);
            while Present (Actual) loop
               Analyze (Actual);
               Next (Actual);
            end loop;

            Prepend (Dummy, Actuals);

            Call_Node :=
               Make_Function_Call (Loc,
                 Name                   => New_Copy (Subprog),
                 Parameter_Associations => Actuals);

         --  Parameterless call: Obj.F is rewritten as F (Obj)

         else
            Node_To_Replace := N;

            Call_Node :=
               Make_Function_Call (Loc,
                 Name                   => New_Copy (Subprog),
                 Parameter_Associations => New_List (Dummy));
         end if;
      end Transform_Object_Operation;

      ------------------------------
      -- Try_Class_Wide_Operation --
      ------------------------------

      function Try_Class_Wide_Operation
        (Call_Node       : Node_Id;
         Node_To_Replace : Node_Id) return Boolean
      is
         Anc_Type    : Entity_Id;
         Matching_Op : Entity_Id := Empty;
         Error       : Boolean;

         procedure Traverse_Homonyms
           (Anc_Type : Entity_Id;
            Error    : out Boolean);
         --  Traverse the homonym chain of the subprogram searching for those
         --  homonyms whose first formal has the Anc_Type's class-wide type,
         --  or an anonymous access type designating the class-wide type. If
         --  an ambiguity is detected, then Error is set to True.

         procedure Traverse_Interfaces
           (Anc_Type : Entity_Id;
            Error    : out Boolean);
         --  Traverse the list of interfaces, if any, associated with Anc_Type
         --  and search for acceptable class-wide homonyms associated with each
         --  interface. If an ambiguity is detected, then Error is set to True.

         -----------------------
         -- Traverse_Homonyms --
         -----------------------

         procedure Traverse_Homonyms
           (Anc_Type : Entity_Id;
            Error    : out Boolean)
         is
            Cls_Type    : Entity_Id;
            Hom         : Entity_Id;
            Hom_Ref     : Node_Id;
            Success     : Boolean;

         begin
            Error := False;

            Cls_Type := Class_Wide_Type (Anc_Type);

            Hom := Current_Entity (Subprog);

            --  Find a non-hidden operation whose first parameter is of the
            --  class-wide type, a subtype thereof, or an anonymous access
            --  to same. If in an instance, the operation can be considered
            --  even if hidden (it may be hidden because the instantiation
            --  is expanded after the containing package has been analyzed).

            while Present (Hom) loop
               if Ekind_In (Hom, E_Procedure, E_Function)
                 and then (not Is_Hidden (Hom) or else In_Instance)
                 and then Scope (Hom) = Scope (Anc_Type)
                 and then Present (First_Formal (Hom))
                 and then
                   (Base_Type (Etype (First_Formal (Hom))) = Cls_Type
                     or else
                       (Is_Access_Type (Etype (First_Formal (Hom)))
                         and then
                           Ekind (Etype (First_Formal (Hom))) =
                             E_Anonymous_Access_Type
                         and then
                           Base_Type
                             (Designated_Type (Etype (First_Formal (Hom)))) =
                                                                   Cls_Type))
               then
                  --  If the context is a procedure call, ignore functions
                  --  in the name of the call.

                  if Ekind (Hom) = E_Function
                    and then Nkind (Parent (N)) = N_Procedure_Call_Statement
                    and then N = Name (Parent (N))
                  then
                     goto Next_Hom;

                  --  If the context is a function call, ignore procedures
                  --  in the name of the call.

                  elsif Ekind (Hom) = E_Procedure
                    and then Nkind (Parent (N)) /= N_Procedure_Call_Statement
                  then
                     goto Next_Hom;
                  end if;

                  Set_Etype (Call_Node, Any_Type);
                  Set_Is_Overloaded (Call_Node, False);
                  Success := False;

                  if No (Matching_Op) then
                     Hom_Ref := New_Occurrence_Of (Hom, Sloc (Subprog));
                     Set_Etype (Call_Node, Any_Type);
                     Set_Parent (Call_Node, Parent (Node_To_Replace));

                     Set_Name (Call_Node, Hom_Ref);

                     Analyze_One_Call
                       (N          => Call_Node,
                        Nam        => Hom,
                        Report     => Report_Error,
                        Success    => Success,
                        Skip_First => True);

                     Matching_Op :=
                       Valid_Candidate (Success, Call_Node, Hom);

                  else
                     Analyze_One_Call
                       (N          => Call_Node,
                        Nam        => Hom,
                        Report     => Report_Error,
                        Success    => Success,
                        Skip_First => True);

                     if Present (Valid_Candidate (Success, Call_Node, Hom))
                       and then Nkind (Call_Node) /= N_Function_Call
                     then
                        Error_Msg_NE ("ambiguous call to&", N, Hom);
                        Report_Ambiguity (Matching_Op);
                        Report_Ambiguity (Hom);
                        Error := True;
                        return;
                     end if;
                  end if;
               end if;

               <<Next_Hom>>
                  Hom := Homonym (Hom);
            end loop;
         end Traverse_Homonyms;

         -------------------------
         -- Traverse_Interfaces --
         -------------------------

         procedure Traverse_Interfaces
           (Anc_Type : Entity_Id;
            Error    : out Boolean)
         is
            Intface_List : constant List_Id :=
                             Abstract_Interface_List (Anc_Type);
            Intface      : Node_Id;

         begin
            Error := False;

            if Is_Non_Empty_List (Intface_List) then
               Intface := First (Intface_List);
               while Present (Intface) loop

                  --  Look for acceptable class-wide homonyms associated with
                  --  the interface.

                  Traverse_Homonyms (Etype (Intface), Error);

                  if Error then
                     return;
                  end if;

                  --  Continue the search by looking at each of the interface's
                  --  associated interface ancestors.

                  Traverse_Interfaces (Etype (Intface), Error);

                  if Error then
                     return;
                  end if;

                  Next (Intface);
               end loop;
            end if;
         end Traverse_Interfaces;

      --  Start of processing for Try_Class_Wide_Operation

      begin
         --  If we are searching only for conflicting class-wide subprograms
         --  then initialize directly Matching_Op with the target entity.

         if CW_Test_Only then
            Matching_Op := Entity (Selector_Name (N));
         end if;

         --  Loop through ancestor types (including interfaces), traversing
         --  the homonym chain of the subprogram, trying out those homonyms
         --  whose first formal has the class-wide type of the ancestor, or
         --  an anonymous access type designating the class-wide type.

         Anc_Type := Obj_Type;
         loop
            --  Look for a match among homonyms associated with the ancestor

            Traverse_Homonyms (Anc_Type, Error);

            if Error then
               return True;
            end if;

            --  Continue the search for matches among homonyms associated with
            --  any interfaces implemented by the ancestor.

            Traverse_Interfaces (Anc_Type, Error);

            if Error then
               return True;
            end if;

            exit when Etype (Anc_Type) = Anc_Type;
            Anc_Type := Etype (Anc_Type);
         end loop;

         if Present (Matching_Op) then
            Set_Etype (Call_Node, Etype (Matching_Op));
         end if;

         return Present (Matching_Op);
      end Try_Class_Wide_Operation;

      -----------------------------------
      -- Try_One_Prefix_Interpretation --
      -----------------------------------

      procedure Try_One_Prefix_Interpretation (T : Entity_Id) is

         --  If the interpretation does not have a valid candidate type,
         --  preserve current value of Obj_Type for subsequent errors.

         Prev_Obj_Type : constant Entity_Id := Obj_Type;

      begin
         Obj_Type := T;

         if Is_Access_Type (Obj_Type) then
            Obj_Type := Designated_Type (Obj_Type);
         end if;

         if Ekind (Obj_Type) = E_Private_Subtype then
            Obj_Type := Base_Type (Obj_Type);
         end if;

         if Is_Class_Wide_Type (Obj_Type) then
            Obj_Type := Etype (Class_Wide_Type (Obj_Type));
         end if;

         --  The type may have be obtained through a limited_with clause,
         --  in which case the primitive operations are available on its
         --  non-limited view. If still incomplete, retrieve full view.

         if Ekind (Obj_Type) = E_Incomplete_Type
           and then From_Limited_With (Obj_Type)
           and then Has_Non_Limited_View (Obj_Type)
         then
            Obj_Type := Get_Full_View (Non_Limited_View (Obj_Type));
         end if;

         --  If the object is not tagged, or the type is still an incomplete
         --  type, this is not a prefixed call.

         if not Is_Tagged_Type (Obj_Type)
           or else Is_Incomplete_Type (Obj_Type)
         then

            --  Restore previous type if current one is not legal candidate

            Obj_Type := Prev_Obj_Type;
            return;
         end if;

         declare
            Dup_Call_Node : constant Node_Id := New_Copy (New_Call_Node);
            CW_Result     : Boolean;
            Prim_Result   : Boolean;
            pragma Unreferenced (CW_Result);

         begin
            if not CW_Test_Only then
               Prim_Result :=
                  Try_Primitive_Operation
                   (Call_Node       => New_Call_Node,
                    Node_To_Replace => Node_To_Replace);
            end if;

            --  Check if there is a class-wide subprogram covering the
            --  primitive. This check must be done even if a candidate
            --  was found in order to report ambiguous calls.

            if not (Prim_Result) then
               CW_Result :=
                 Try_Class_Wide_Operation
                   (Call_Node       => New_Call_Node,
                    Node_To_Replace => Node_To_Replace);

            --  If we found a primitive we search for class-wide subprograms
            --  using a duplicate of the call node (done to avoid missing its
            --  decoration if there is no ambiguity).

            else
               CW_Result :=
                 Try_Class_Wide_Operation
                   (Call_Node       => Dup_Call_Node,
                    Node_To_Replace => Node_To_Replace);
            end if;
         end;
      end Try_One_Prefix_Interpretation;

      -----------------------------
      -- Try_Primitive_Operation --
      -----------------------------

      function Try_Primitive_Operation
        (Call_Node       : Node_Id;
         Node_To_Replace : Node_Id) return Boolean
      is
         Elmt        : Elmt_Id;
         Prim_Op     : Entity_Id;
         Matching_Op : Entity_Id := Empty;
         Prim_Op_Ref : Node_Id   := Empty;

         Corr_Type : Entity_Id := Empty;
         --  If the prefix is a synchronized type, the controlling type of
         --  the primitive operation is the corresponding record type, else
         --  this is the object type itself.

         Success : Boolean   := False;

         function Collect_Generic_Type_Ops (T : Entity_Id) return Elist_Id;
         --  For tagged types the candidate interpretations are found in
         --  the list of primitive operations of the type and its ancestors.
         --  For formal tagged types we have to find the operations declared
         --  in the same scope as the type (including in the generic formal
         --  part) because the type itself carries no primitive operations,
         --  except for formal derived types that inherit the operations of
         --  the parent and progenitors.
         --
         --  If the context is a generic subprogram body, the generic formals
         --  are visible by name, but are not in the entity list of the
         --  subprogram because that list starts with the subprogram formals.
         --  We retrieve the candidate operations from the generic declaration.

         function Extended_Primitive_Ops (T : Entity_Id) return Elist_Id;
         --  Prefix notation can also be used on operations that are not
         --  primitives of the type, but are declared in the same immediate
         --  declarative part, which can only mean the corresponding package
         --  body (See RM 4.1.3 (9.2/3)). If we are in that body we extend the
         --  list of primitives with body operations with the same name that
         --  may be candidates, so that Try_Primitive_Operations can examine
         --  them if no real primitive is found.

         function Is_Private_Overriding (Op : Entity_Id) return Boolean;
         --  An operation that overrides an inherited operation in the private
         --  part of its package may be hidden, but if the inherited operation
         --  is visible a direct call to it will dispatch to the private one,
         --  which is therefore a valid candidate.

         function Names_Match
           (Obj_Type : Entity_Id;
            Prim_Op  : Entity_Id;
            Subprog  : Entity_Id) return Boolean;
         --  Return True if the names of Prim_Op and Subprog match. If Obj_Type
         --  is a protected type then compare also the original name of Prim_Op
         --  with the name of Subprog (since the expander may have added a
         --  prefix to its original name --see Exp_Ch9.Build_Selected_Name).

         function Valid_First_Argument_Of (Op : Entity_Id) return Boolean;
         --  Verify that the prefix, dereferenced if need be, is a valid
         --  controlling argument in a call to Op. The remaining actuals
         --  are checked in the subsequent call to Analyze_One_Call.

         ------------------------------
         -- Collect_Generic_Type_Ops --
         ------------------------------

         function Collect_Generic_Type_Ops (T : Entity_Id) return Elist_Id is
            Bas        : constant Entity_Id := Base_Type (T);
            Candidates : constant Elist_Id := New_Elmt_List;
            Subp       : Entity_Id;
            Formal     : Entity_Id;

            procedure Check_Candidate;
            --  The operation is a candidate if its first parameter is a
            --  controlling operand of the desired type.

            -----------------------
            --  Check_Candidate; --
            -----------------------

            procedure Check_Candidate is
            begin
               Formal := First_Formal (Subp);

               if Present (Formal)
                 and then Is_Controlling_Formal (Formal)
                 and then
                   (Base_Type (Etype (Formal)) = Bas
                     or else
                       (Is_Access_Type (Etype (Formal))
                         and then Designated_Type (Etype (Formal)) = Bas))
               then
                  Append_Elmt (Subp, Candidates);
               end if;
            end Check_Candidate;

         --  Start of processing for Collect_Generic_Type_Ops

         begin
            if Is_Derived_Type (T) then
               return Primitive_Operations (T);

            elsif Ekind_In (Scope (T), E_Procedure, E_Function) then

               --  Scan the list of generic formals to find subprograms
               --  that may have a first controlling formal of the type.

               if Nkind (Unit_Declaration_Node (Scope (T))) =
                                         N_Generic_Subprogram_Declaration
               then
                  declare
                     Decl : Node_Id;

                  begin
                     Decl :=
                       First (Generic_Formal_Declarations
                               (Unit_Declaration_Node (Scope (T))));
                     while Present (Decl) loop
                        if Nkind (Decl) in N_Formal_Subprogram_Declaration then
                           Subp := Defining_Entity (Decl);
                           Check_Candidate;
                        end if;

                        Next (Decl);
                     end loop;
                  end;
               end if;
               return Candidates;

            else
               --  Scan the list of entities declared in the same scope as
               --  the type. In general this will be an open scope, given that
               --  the call we are analyzing can only appear within a generic
               --  declaration or body (either the one that declares T, or a
               --  child unit).

               --  For a subtype representing a generic actual type, go to the
               --  base type.

               if Is_Generic_Actual_Type (T) then
                  Subp := First_Entity (Scope (Base_Type (T)));
               else
                  Subp := First_Entity (Scope (T));
               end if;

               while Present (Subp) loop
                  if Is_Overloadable (Subp) then
                     Check_Candidate;
                  end if;

                  Next_Entity (Subp);
               end loop;

               return Candidates;
            end if;
         end Collect_Generic_Type_Ops;

         ----------------------------
         -- Extended_Primitive_Ops --
         ----------------------------

         function Extended_Primitive_Ops (T : Entity_Id) return Elist_Id is
            Type_Scope : constant Entity_Id := Scope (T);

            Body_Decls : List_Id;
            Op_Found   : Boolean;
            Op         : Entity_Id;
            Op_List    : Elist_Id;

         begin
            Op_List := Primitive_Operations (T);

            if Ekind (Type_Scope) = E_Package
              and then In_Package_Body (Type_Scope)
              and then In_Open_Scopes (Type_Scope)
            then
               --  Retrieve list of declarations of package body.

               Body_Decls :=
                 Declarations
                   (Unit_Declaration_Node
                     (Corresponding_Body
                       (Unit_Declaration_Node (Type_Scope))));

               Op       := Current_Entity (Subprog);
               Op_Found := False;
               while Present (Op) loop
                  if Comes_From_Source (Op)
                    and then Is_Overloadable (Op)

                    --  Exclude overriding primitive operations of a type
                    --  extension declared in the package body, to prevent
                    --  duplicates in extended list.

                    and then not Is_Primitive (Op)
                    and then Is_List_Member (Unit_Declaration_Node (Op))
                    and then List_Containing (Unit_Declaration_Node (Op)) =
                                                                   Body_Decls
                  then
                     if not Op_Found then

                        --  Copy list of primitives so it is not affected for
                        --  other uses.

                        Op_List  := New_Copy_Elist (Op_List);
                        Op_Found := True;
                     end if;

                     Append_Elmt (Op, Op_List);
                  end if;

                  Op := Homonym (Op);
               end loop;
            end if;

            return Op_List;
         end Extended_Primitive_Ops;

         ---------------------------
         -- Is_Private_Overriding --
         ---------------------------

         function Is_Private_Overriding (Op : Entity_Id) return Boolean is
            Visible_Op : constant Entity_Id := Homonym (Op);

         begin
            return Present (Visible_Op)
              and then Scope (Op) = Scope (Visible_Op)
              and then not Comes_From_Source (Visible_Op)
              and then Alias (Visible_Op) = Op
              and then not Is_Hidden (Visible_Op);
         end Is_Private_Overriding;

         -----------------
         -- Names_Match --
         -----------------

         function Names_Match
           (Obj_Type : Entity_Id;
            Prim_Op  : Entity_Id;
            Subprog  : Entity_Id) return Boolean is
         begin
            --  Common case: exact match

            if Chars (Prim_Op) = Chars (Subprog) then
               return True;

            --  For protected type primitives the expander may have built the
            --  name of the dispatching primitive prepending the type name to
            --  avoid conflicts with the name of the protected subprogram (see
            --  Exp_Ch9.Build_Selected_Name).

            elsif Is_Protected_Type (Obj_Type) then
               return
                 Present (Original_Protected_Subprogram (Prim_Op))
                   and then Chars (Original_Protected_Subprogram (Prim_Op)) =
                              Chars (Subprog);
            end if;

            return False;
         end Names_Match;

         -----------------------------
         -- Valid_First_Argument_Of --
         -----------------------------

         function Valid_First_Argument_Of (Op : Entity_Id) return Boolean is
            Typ : Entity_Id := Etype (First_Formal (Op));

         begin
            if Is_Concurrent_Type (Typ)
              and then Present (Corresponding_Record_Type (Typ))
            then
               Typ := Corresponding_Record_Type (Typ);
            end if;

            --  Simple case. Object may be a subtype of the tagged type or
            --  may be the corresponding record of a synchronized type.

            return Obj_Type = Typ
              or else Base_Type (Obj_Type) = Typ
              or else Corr_Type = Typ

               --  Prefix can be dereferenced

              or else
                (Is_Access_Type (Corr_Type)
                  and then Designated_Type (Corr_Type) = Typ)

               --  Formal is an access parameter, for which the object
               --  can provide an access.

              or else
                (Ekind (Typ) = E_Anonymous_Access_Type
                  and then
                    Base_Type (Designated_Type (Typ)) = Base_Type (Corr_Type));
         end Valid_First_Argument_Of;

      --  Start of processing for Try_Primitive_Operation

      begin
         --  Look for subprograms in the list of primitive operations. The name
         --  must be identical, and the kind of call indicates the expected
         --  kind of operation (function or procedure). If the type is a
         --  (tagged) synchronized type, the primitive ops are attached to the
         --  corresponding record (base) type.

         if Is_Concurrent_Type (Obj_Type) then
            if Present (Corresponding_Record_Type (Obj_Type)) then
               Corr_Type := Base_Type (Corresponding_Record_Type (Obj_Type));
               Elmt := First_Elmt (Primitive_Operations (Corr_Type));
            else
               Corr_Type := Obj_Type;
               Elmt := First_Elmt (Collect_Generic_Type_Ops (Obj_Type));
            end if;

         elsif not Is_Generic_Type (Obj_Type) then
            Corr_Type := Obj_Type;
            Elmt := First_Elmt (Extended_Primitive_Ops (Obj_Type));

         else
            Corr_Type := Obj_Type;
            Elmt := First_Elmt (Collect_Generic_Type_Ops (Obj_Type));
         end if;

         while Present (Elmt) loop
            Prim_Op := Node (Elmt);

            if Names_Match (Obj_Type, Prim_Op, Subprog)
              and then Present (First_Formal (Prim_Op))
              and then Valid_First_Argument_Of (Prim_Op)
              and then
                (Nkind (Call_Node) = N_Function_Call)
                    =
                (Ekind (Prim_Op) = E_Function)
            then
               --  Ada 2005 (AI-251): If this primitive operation corresponds
               --  to an immediate ancestor interface there is no need to add
               --  it to the list of interpretations; the corresponding aliased
               --  primitive is also in this list of primitive operations and
               --  will be used instead.

               if (Present (Interface_Alias (Prim_Op))
                    and then Is_Ancestor (Find_Dispatching_Type
                                            (Alias (Prim_Op)), Corr_Type))

                 --  Do not consider hidden primitives unless the type is in an
                 --  open scope or we are within an instance, where visibility
                 --  is known to be correct, or else if this is an overriding
                 --  operation in the private part for an inherited operation.

                 or else (Is_Hidden (Prim_Op)
                           and then not Is_Immediately_Visible (Obj_Type)
                           and then not In_Instance
                           and then not Is_Private_Overriding (Prim_Op))
               then
                  goto Continue;
               end if;

               Set_Etype (Call_Node, Any_Type);
               Set_Is_Overloaded (Call_Node, False);

               if No (Matching_Op) then
                  Prim_Op_Ref := New_Occurrence_Of (Prim_Op, Sloc (Subprog));
                  Candidate := Prim_Op;

                  Set_Parent (Call_Node, Parent (Node_To_Replace));

                  Set_Name (Call_Node, Prim_Op_Ref);
                  Success := False;

                  Analyze_One_Call
                    (N          => Call_Node,
                     Nam        => Prim_Op,
                     Report     => Report_Error,
                     Success    => Success,
                     Skip_First => True);

                  Matching_Op := Valid_Candidate (Success, Call_Node, Prim_Op);

               --  More than one interpretation, collect for subsequent
               --  disambiguation. If this is a procedure call and there
               --  is another match, report ambiguity now.

               else
                  Analyze_One_Call
                    (N          => Call_Node,
                     Nam        => Prim_Op,
                     Report     => Report_Error,
                     Success    => Success,
                     Skip_First => True);

                  if Present (Valid_Candidate (Success, Call_Node, Prim_Op))
                    and then Nkind (Call_Node) /= N_Function_Call
                  then
                     Error_Msg_NE ("ambiguous call to&", N, Prim_Op);
                     Report_Ambiguity (Matching_Op);
                     Report_Ambiguity (Prim_Op);
                     return True;
                  end if;
               end if;
            end if;

            <<Continue>>
            Next_Elmt (Elmt);
         end loop;

         if Present (Matching_Op) then
            Set_Etype (Call_Node, Etype (Matching_Op));
         end if;

         return Present (Matching_Op);
      end Try_Primitive_Operation;

   --  Start of processing for Try_Object_Operation

   begin
      Analyze_Expression (Obj);

      --  Analyze the actuals if node is known to be a subprogram call

      if Is_Subprg_Call and then N = Name (Parent (N)) then
         Actual := First (Parameter_Associations (Parent (N)));
         while Present (Actual) loop
            Analyze_Expression (Actual);
            Next (Actual);
         end loop;
      end if;

      --  Build a subprogram call node, using a copy of Obj as its first
      --  actual. This is a placeholder, to be replaced by an explicit
      --  dereference when needed.

      Transform_Object_Operation
        (Call_Node       => New_Call_Node,
         Node_To_Replace => Node_To_Replace);

      Set_Etype (New_Call_Node, Any_Type);
      Set_Etype (Subprog, Any_Type);
      Set_Parent (New_Call_Node, Parent (Node_To_Replace));

      if not Is_Overloaded (Obj) then
         Try_One_Prefix_Interpretation (Obj_Type);

      else
         declare
            I  : Interp_Index;
            It : Interp;
         begin
            Get_First_Interp (Obj, I, It);
            while Present (It.Nam) loop
               Try_One_Prefix_Interpretation (It.Typ);
               Get_Next_Interp (I, It);
            end loop;
         end;
      end if;

      if Etype (New_Call_Node) /= Any_Type then

         --  No need to complete the tree transformations if we are only
         --  searching for conflicting class-wide subprograms

         if CW_Test_Only then
            return False;
         else
            Complete_Object_Operation
              (Call_Node       => New_Call_Node,
               Node_To_Replace => Node_To_Replace);
            return True;
         end if;

      elsif Present (Candidate) then

         --  The argument list is not type correct. Re-analyze with error
         --  reporting enabled, and use one of the possible candidates.
         --  In All_Errors_Mode, re-analyze all failed interpretations.

         if All_Errors_Mode then
            Report_Error := True;
            if Try_Primitive_Operation
                 (Call_Node       => New_Call_Node,
                  Node_To_Replace => Node_To_Replace)

              or else
                Try_Class_Wide_Operation
                  (Call_Node       => New_Call_Node,
                   Node_To_Replace => Node_To_Replace)
            then
               null;
            end if;

         else
            Analyze_One_Call
              (N          => New_Call_Node,
               Nam        => Candidate,
               Report     => True,
               Success    => Success,
               Skip_First => True);
         end if;

         --  No need for further errors

         return True;

      else
         --  There was no candidate operation, so report it as an error
         --  in the caller: Analyze_Selected_Component.

         return False;
      end if;
   end Try_Object_Operation;

   ---------
   -- wpo --
   ---------

   procedure wpo (T : Entity_Id) is
      Op : Entity_Id;
      E  : Elmt_Id;

   begin
      if not Is_Tagged_Type (T) then
         return;
      end if;

      E := First_Elmt (Primitive_Operations (Base_Type (T)));
      while Present (E) loop
         Op := Node (E);
         Write_Int (Int (Op));
         Write_Str (" === ");
         Write_Name (Chars (Op));
         Write_Str (" in ");
         Write_Name (Chars (Scope (Op)));
         Next_Elmt (E);
         Write_Eol;
      end loop;
   end wpo;

end Sem_Ch4;
