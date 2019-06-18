------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 5                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
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
with Checks;   use Checks;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Expander; use Expander;
with Exp_Ch6;  use Exp_Ch6;
with Exp_Util; use Exp_Util;
with Freeze;   use Freeze;
with Ghost;    use Ghost;
with Lib;      use Lib;
with Lib.Xref; use Lib.Xref;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rident;   use Rident;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Case; use Sem_Case;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Dim;  use Sem_Dim;
with Sem_Disp; use Sem_Disp;
with Sem_Elab; use Sem_Elab;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Type; use Sem_Type;
with Sem_Util; use Sem_Util;
with Sem_Warn; use Sem_Warn;
with Snames;   use Snames;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Targparm; use Targparm;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

package body Sem_Ch5 is

   Current_Assignment : Node_Id := Empty;
   --  This variable holds the node for an assignment that contains target
   --  names. The corresponding flag has been set by the parser, and when
   --  set the analysis of the RHS must be done with all expansion disabled,
   --  because the assignment is reanalyzed after expansion has replaced all
   --  occurrences of the target name appropriately.

   Unblocked_Exit_Count : Nat := 0;
   --  This variable is used when processing if statements, case statements,
   --  and block statements. It counts the number of exit points that are not
   --  blocked by unconditional transfer instructions: for IF and CASE, these
   --  are the branches of the conditional; for a block, they are the statement
   --  sequence of the block, and the statement sequences of any exception
   --  handlers that are part of the block. When processing is complete, if
   --  this count is zero, it means that control cannot fall through the IF,
   --  CASE or block statement. This is used for the generation of warning
   --  messages. This variable is recursively saved on entry to processing the
   --  construct, and restored on exit.

   function Has_Sec_Stack_Call (N : Node_Id) return Boolean;
   --  N is the node for an arbitrary construct. This function searches the
   --  construct N to see if any expressions within it contain function
   --  calls that use the secondary stack, returning True if any such call
   --  is found, and False otherwise.

   procedure Preanalyze_Range (R_Copy : Node_Id);
   --  Determine expected type of range or domain of iteration of Ada 2012
   --  loop by analyzing separate copy. Do the analysis and resolution of the
   --  copy of the bound(s) with expansion disabled, to prevent the generation
   --  of finalization actions. This prevents memory leaks when the bounds
   --  contain calls to functions returning controlled arrays or when the
   --  domain of iteration is a container.

   ------------------------
   -- Analyze_Assignment --
   ------------------------

   --  WARNING: This routine manages Ghost regions. Return statements must be
   --  replaced by gotos which jump to the end of the routine and restore the
   --  Ghost mode.

   procedure Analyze_Assignment (N : Node_Id) is
      Lhs : constant Node_Id := Name (N);
      Rhs : Node_Id          := Expression (N);

      procedure Diagnose_Non_Variable_Lhs (N : Node_Id);
      --  N is the node for the left hand side of an assignment, and it is not
      --  a variable. This routine issues an appropriate diagnostic.

      function Is_Protected_Part_Of_Constituent
        (Nod : Node_Id) return Boolean;
      --  Determine whether arbitrary node Nod denotes a Part_Of constituent of
      --  a single protected type.

      procedure Kill_Lhs;
      --  This is called to kill current value settings of a simple variable
      --  on the left hand side. We call it if we find any error in analyzing
      --  the assignment, and at the end of processing before setting any new
      --  current values in place.

      procedure Set_Assignment_Type
        (Opnd      : Node_Id;
         Opnd_Type : in out Entity_Id);
      --  Opnd is either the Lhs or Rhs of the assignment, and Opnd_Type is the
      --  nominal subtype. This procedure is used to deal with cases where the
      --  nominal subtype must be replaced by the actual subtype.

      procedure Transform_BIP_Assignment (Typ : Entity_Id);
      function Should_Transform_BIP_Assignment
        (Typ : Entity_Id) return Boolean;
      --  If the right-hand side of an assignment statement is a build-in-place
      --  call we cannot build in place, so we insert a temp initialized with
      --  the call, and transform the assignment statement to copy the temp.
      --  Transform_BIP_Assignment does the tranformation, and
      --  Should_Transform_BIP_Assignment determines whether we should.
      --  The same goes for qualified expressions and conversions whose
      --  operand is such a call.
      --
      --  This is only for nonlimited types; assignment statements are illegal
      --  for limited types, but are generated internally for aggregates and
      --  init procs. These limited-type are not really assignment statements
      --  -- conceptually, they are initializations, so should not be
      --  transformed.
      --
      --  Similarly, for nonlimited types, aggregates and init procs generate
      --  assignment statements that are really initializations. These are
      --  marked No_Ctrl_Actions.

      function Within_Function return Boolean;
      --  Determine whether the current scope is a function or appears within
      --  one.

      -------------------------------
      -- Diagnose_Non_Variable_Lhs --
      -------------------------------

      procedure Diagnose_Non_Variable_Lhs (N : Node_Id) is
      begin
         --  Not worth posting another error if left hand side already flagged
         --  as being illegal in some respect.

         if Error_Posted (N) then
            return;

         --  Some special bad cases of entity names

         elsif Is_Entity_Name (N) then
            declare
               Ent : constant Entity_Id := Entity (N);

            begin
               if Ekind (Ent) = E_Loop_Parameter
                 or else Is_Loop_Parameter (Ent)
               then
                  Error_Msg_N ("assignment to loop parameter not allowed", N);
                  return;

               elsif Ekind (Ent) = E_In_Parameter then
                  Error_Msg_N
                    ("assignment to IN mode parameter not allowed", N);
                  return;

               --  Renamings of protected private components are turned into
               --  constants when compiling a protected function. In the case
               --  of single protected types, the private component appears
               --  directly.

               elsif (Is_Prival (Ent) and then Within_Function)
                   or else
                     (Ekind (Ent) = E_Component
                       and then Is_Protected_Type (Scope (Ent)))
               then
                  Error_Msg_N
                    ("protected function cannot modify protected object", N);
                  return;
               end if;
            end;

         --  For indexed components, test prefix if it is in array. We do not
         --  want to recurse for cases where the prefix is a pointer, since we
         --  may get a message confusing the pointer and what it references.

         elsif Nkind (N) = N_Indexed_Component
           and then Is_Array_Type (Etype (Prefix (N)))
         then
            Diagnose_Non_Variable_Lhs (Prefix (N));
            return;

         --  Another special case for assignment to discriminant

         elsif Nkind (N) = N_Selected_Component then
            if Present (Entity (Selector_Name (N)))
              and then Ekind (Entity (Selector_Name (N))) = E_Discriminant
            then
               Error_Msg_N ("assignment to discriminant not allowed", N);
               return;

            --  For selection from record, diagnose prefix, but note that again
            --  we only do this for a record, not e.g. for a pointer.

            elsif Is_Record_Type (Etype (Prefix (N))) then
               Diagnose_Non_Variable_Lhs (Prefix (N));
               return;
            end if;
         end if;

         --  If we fall through, we have no special message to issue

         Error_Msg_N ("left hand side of assignment must be a variable", N);
      end Diagnose_Non_Variable_Lhs;

      --------------------------------------
      -- Is_Protected_Part_Of_Constituent --
      --------------------------------------

      function Is_Protected_Part_Of_Constituent
        (Nod : Node_Id) return Boolean
      is
         Encap_Id : Entity_Id;
         Var_Id   : Entity_Id;

      begin
         --  Abstract states and variables may act as Part_Of constituents of
         --  single protected types, however only variables can be modified by
         --  an assignment.

         if Is_Entity_Name (Nod) then
            Var_Id := Entity (Nod);

            if Present (Var_Id) and then Ekind (Var_Id) = E_Variable then
               Encap_Id := Encapsulating_State (Var_Id);

               --  To qualify, the node must denote a reference to a variable
               --  whose encapsulating state is a single protected object.

               return
                 Present (Encap_Id)
                   and then Is_Single_Protected_Object (Encap_Id);
            end if;
         end if;

         return False;
      end Is_Protected_Part_Of_Constituent;

      --------------
      -- Kill_Lhs --
      --------------

      procedure Kill_Lhs is
      begin
         if Is_Entity_Name (Lhs) then
            declare
               Ent : constant Entity_Id := Entity (Lhs);
            begin
               if Present (Ent) then
                  Kill_Current_Values (Ent);
               end if;
            end;
         end if;
      end Kill_Lhs;

      -------------------------
      -- Set_Assignment_Type --
      -------------------------

      procedure Set_Assignment_Type
        (Opnd      : Node_Id;
         Opnd_Type : in out Entity_Id)
      is
         Decl : Node_Id;

      begin
         Require_Entity (Opnd);

         --  If the assignment operand is an in-out or out parameter, then we
         --  get the actual subtype (needed for the unconstrained case). If the
         --  operand is the actual in an entry declaration, then within the
         --  accept statement it is replaced with a local renaming, which may
         --  also have an actual subtype.

         if Is_Entity_Name (Opnd)
           and then (Ekind (Entity (Opnd)) = E_Out_Parameter
                      or else Ekind_In (Entity (Opnd),
                                        E_In_Out_Parameter,
                                        E_Generic_In_Out_Parameter)
                      or else
                        (Ekind (Entity (Opnd)) = E_Variable
                          and then Nkind (Parent (Entity (Opnd))) =
                                     N_Object_Renaming_Declaration
                          and then Nkind (Parent (Parent (Entity (Opnd)))) =
                                     N_Accept_Statement))
         then
            Opnd_Type := Get_Actual_Subtype (Opnd);

         --  If assignment operand is a component reference, then we get the
         --  actual subtype of the component for the unconstrained case.

         elsif Nkind_In (Opnd, N_Selected_Component, N_Explicit_Dereference)
           and then not Is_Unchecked_Union (Opnd_Type)
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

      -------------------------------------
      -- Should_Transform_BIP_Assignment --
      -------------------------------------

      function Should_Transform_BIP_Assignment
        (Typ : Entity_Id) return Boolean
      is
         Result : Boolean;

      begin
         if Expander_Active
           and then not Is_Limited_View (Typ)
           and then Is_Build_In_Place_Result_Type (Typ)
           and then not No_Ctrl_Actions (N)
         then
            --  This function is called early, before name resolution is
            --  complete, so we have to deal with things that might turn into
            --  function calls later. N_Function_Call and N_Op nodes are the
            --  obvious case. An N_Identifier or N_Expanded_Name is a
            --  parameterless function call if it denotes a function.
            --  Finally, an attribute reference can be a function call.

            case Nkind (Unqual_Conv (Rhs)) is
               when N_Function_Call
                  | N_Op
               =>
                  Result := True;

               when N_Expanded_Name
                  | N_Identifier
               =>
                  case Ekind (Entity (Unqual_Conv (Rhs))) is
                     when E_Function
                        | E_Operator
                     =>
                        Result := True;

                     when others =>
                        Result := False;
                  end case;

               when N_Attribute_Reference =>
                  Result := Attribute_Name (Unqual_Conv (Rhs)) = Name_Input;
                  --  T'Input will turn into a call whose result type is T

               when others =>
                  Result := False;
            end case;
         else
            Result := False;
         end if;

         return Result;
      end Should_Transform_BIP_Assignment;

      ------------------------------
      -- Transform_BIP_Assignment --
      ------------------------------

      procedure Transform_BIP_Assignment (Typ : Entity_Id) is

         --  Tranform "X : [constant] T := F (...);" into:
         --
         --     Temp : constant T := F (...);
         --     X := Temp;

         Loc      : constant Source_Ptr := Sloc (N);
         Def_Id   : constant Entity_Id  := Make_Temporary (Loc, 'Y', Rhs);
         Obj_Decl : constant Node_Id    :=
                      Make_Object_Declaration (Loc,
                        Defining_Identifier => Def_Id,
                        Constant_Present    => True,
                        Object_Definition   => New_Occurrence_Of (Typ, Loc),
                        Expression          => Rhs,
                        Has_Init_Expression => True);

      begin
         Set_Etype (Def_Id, Typ);
         Set_Expression (N, New_Occurrence_Of (Def_Id, Loc));

         --  At this point, Rhs is no longer equal to Expression (N), so:

         Rhs := Expression (N);

         Insert_Action (N, Obj_Decl);
      end Transform_BIP_Assignment;

      ---------------------
      -- Within_Function --
      ---------------------

      function Within_Function return Boolean is
         Scop_Id : constant Entity_Id := Current_Scope;

      begin
         if Ekind (Scop_Id) = E_Function then
            return True;

         elsif Ekind (Enclosing_Dynamic_Scope (Scop_Id)) = E_Function then
            return True;
         end if;

         return False;
      end Within_Function;

      --  Local variables

      Saved_GM  : constant Ghost_Mode_Type := Ghost_Mode;
      Saved_IGR : constant Node_Id         := Ignored_Ghost_Region;
      --  Save the Ghost-related attributes to restore on exit

      T1 : Entity_Id;
      T2 : Entity_Id;

      Save_Full_Analysis : Boolean := False;
      --  Force initialization to facilitate static analysis

   --  Start of processing for Analyze_Assignment

   begin
      Mark_Coextensions (N, Rhs);

      --  Preserve relevant elaboration-related attributes of the context which
      --  are no longer available or very expensive to recompute once analysis,
      --  resolution, and expansion are over.

      Mark_Elaboration_Attributes
        (N_Id   => N,
         Checks => True,
         Modes  => True);

      --  An assignment statement is Ghost when the left hand side denotes a
      --  Ghost entity. Set the mode now to ensure that any nodes generated
      --  during analysis and expansion are properly marked as Ghost.

      Mark_And_Set_Ghost_Assignment (N);

      if Has_Target_Names (N) then
         Current_Assignment := N;
         Expander_Mode_Save_And_Set (False);
         Save_Full_Analysis := Full_Analysis;
         Full_Analysis      := False;
      else
         Current_Assignment := Empty;
      end if;

      Analyze (Lhs);
      Analyze (Rhs);

      --  Ensure that we never do an assignment on a variable marked as
      --  Is_Safe_To_Reevaluate.

      pragma Assert
        (not Is_Entity_Name (Lhs)
          or else Ekind (Entity (Lhs)) /= E_Variable
          or else not Is_Safe_To_Reevaluate (Entity (Lhs)));

      --  Start type analysis for assignment

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

               --  An indexed component with generalized indexing is always
               --  overloaded with the corresponding dereference. Discard the
               --  interpretation that yields a reference type, which is not
               --  assignable.

               if Nkind (Lhs) = N_Indexed_Component
                 and then Present (Generalized_Indexing (Lhs))
                 and then Has_Implicit_Dereference (It.Typ)
               then
                  null;

               --  This may be a call to a parameterless function through an
               --  implicit dereference, so discard interpretation as well.

               elsif Is_Entity_Name (Lhs)
                 and then Has_Implicit_Dereference (It.Typ)
               then
                  null;

               elsif Has_Compatible_Type (Rhs, It.Typ) then
                  if T1 = Any_Type then
                     T1 := It.Typ;
                  else
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
                              if Is_Access_Type (PIt.Typ)
                                and then Has_Compatible_Type
                                           (Rhs, Designated_Type (PIt.Typ))
                              then
                                 if Found then
                                    PIt :=
                                      Disambiguate (Prefix (Lhs),
                                        PI1, PI, Any_Type);

                                    if PIt = No_Interp then
                                       Error_Msg_N
                                         ("ambiguous left-hand side in "
                                          & "assignment", Lhs);
                                       exit;
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
                  end if;
               end if;

               Get_Next_Interp (I, It);
            end loop;
         end;

         if T1 = Any_Type then
            Error_Msg_N
              ("no valid types for left-hand side for assignment", Lhs);
            Kill_Lhs;
            goto Leave;
         end if;
      end if;

      --  Deal with build-in-place calls for nonlimited types. We don't do this
      --  later, because resolving the rhs tranforms it incorrectly for build-
      --  in-place.

      if Should_Transform_BIP_Assignment (Typ => T1) then

         --  In certain cases involving user-defined concatenation operators,
         --  we need to resolve the right-hand side before transforming the
         --  assignment.

         case Nkind (Unqual_Conv (Rhs)) is
            when N_Function_Call =>
               declare
                  Actual     : Node_Id :=
                    First (Parameter_Associations (Unqual_Conv (Rhs)));
                  Actual_Exp : Node_Id;

               begin
                  while Present (Actual) loop
                     if Nkind (Actual) = N_Parameter_Association then
                        Actual_Exp := Explicit_Actual_Parameter (Actual);
                     else
                        Actual_Exp := Actual;
                     end if;

                     if Nkind (Actual_Exp) = N_Op_Concat then
                        Resolve (Rhs, T1);
                        exit;
                     end if;

                     Next (Actual);
                  end loop;
               end;

            when N_Attribute_Reference
               | N_Expanded_Name
               | N_Identifier
               | N_Op
            =>
               null;

            when others =>
               raise Program_Error;
         end case;

         Transform_BIP_Assignment (Typ => T1);
      end if;

      pragma Assert (not Should_Transform_BIP_Assignment (Typ => T1));

      --  The resulting assignment type is T1, so now we will resolve the left
      --  hand side of the assignment using this determined type.

      Resolve (Lhs, T1);

      --  Cases where Lhs is not a variable. In an instance or an inlined body
      --  no need for further check because assignment was legal in template.

      if In_Inlined_Body then
         null;

      elsif not Is_Variable (Lhs) then

         --  Ada 2005 (AI-327): Check assignment to the attribute Priority of a
         --  protected object.

         declare
            Ent : Entity_Id;
            S   : Entity_Id;

         begin
            if Ada_Version >= Ada_2005 then

               --  Handle chains of renamings

               Ent := Lhs;
               while Nkind (Ent) in N_Has_Entity
                 and then Present (Entity (Ent))
                 and then Present (Renamed_Object (Entity (Ent)))
               loop
                  Ent := Renamed_Object (Entity (Ent));
               end loop;

               if (Nkind (Ent) = N_Attribute_Reference
                    and then Attribute_Name (Ent) = Name_Priority)

                  --  Renamings of the attribute Priority applied to protected
                  --  objects have been previously expanded into calls to the
                  --  Get_Ceiling run-time subprogram.

                 or else Is_Expanded_Priority_Attribute (Ent)
               then
                  --  The enclosing subprogram cannot be a protected function

                  S := Current_Scope;
                  while not (Is_Subprogram (S)
                              and then Convention (S) = Convention_Protected)
                     and then S /= Standard_Standard
                  loop
                     S := Scope (S);
                  end loop;

                  if Ekind (S) = E_Function
                    and then Convention (S) = Convention_Protected
                  then
                     Error_Msg_N
                       ("protected function cannot modify protected object",
                        Lhs);
                  end if;

                  --  Changes of the ceiling priority of the protected object
                  --  are only effective if the Ceiling_Locking policy is in
                  --  effect (AARM D.5.2 (5/2)).

                  if Locking_Policy /= 'C' then
                     Error_Msg_N
                       ("assignment to the attribute PRIORITY has no effect??",
                        Lhs);
                     Error_Msg_N
                       ("\since no Locking_Policy has been specified??", Lhs);
                  end if;

                  goto Leave;
               end if;
            end if;
         end;

         Diagnose_Non_Variable_Lhs (Lhs);
         goto Leave;

      --  Error of assigning to limited type. We do however allow this in
      --  certain cases where the front end generates the assignments.

      elsif Is_Limited_Type (T1)
        and then not Assignment_OK (Lhs)
        and then not Assignment_OK (Original_Node (Lhs))
      then
         --  CPP constructors can only be called in declarations

         if Is_CPP_Constructor_Call (Rhs) then
            Error_Msg_N ("invalid use of 'C'P'P constructor", Rhs);
         else
            Error_Msg_N
              ("left hand of assignment must not be limited type", Lhs);
            Explain_Limited_Type (T1, Lhs);
         end if;

         goto Leave;

      --  A class-wide type may be a limited view. This illegal case is not
      --  caught by previous checks.

      elsif Ekind (T1) = E_Class_Wide_Type and then From_Limited_With (T1) then
         Error_Msg_NE ("invalid use of limited view of&", Lhs, T1);
         goto Leave;

      --  Enforce RM 3.9.3 (8): the target of an assignment operation cannot be
      --  abstract. This is only checked when the assignment Comes_From_Source,
      --  because in some cases the expander generates such assignments (such
      --  in the _assign operation for an abstract type).

      elsif Is_Abstract_Type (T1) and then Comes_From_Source (N) then
         Error_Msg_N
           ("target of assignment operation must not be abstract", Lhs);
      end if;

      --  Variables which are Part_Of constituents of single protected types
      --  behave in similar fashion to protected components. Such variables
      --  cannot be modified by protected functions.

      if Is_Protected_Part_Of_Constituent (Lhs) and then Within_Function then
         Error_Msg_N
           ("protected function cannot modify protected object", Lhs);
      end if;

      --  Resolution may have updated the subtype, in case the left-hand side
      --  is a private protected component. Use the correct subtype to avoid
      --  scoping issues in the back-end.

      T1 := Etype (Lhs);

      --  Ada 2005 (AI-50217, AI-326): Check wrong dereference of incomplete
      --  type. For example:

      --    limited with P;
      --    package Pkg is
      --      type Acc is access P.T;
      --    end Pkg;

      --    with Pkg; use Acc;
      --    procedure Example is
      --       A, B : Acc;
      --    begin
      --       A.all := B.all;  -- ERROR
      --    end Example;

      if Nkind (Lhs) = N_Explicit_Dereference
        and then Ekind (T1) = E_Incomplete_Type
      then
         Error_Msg_N ("invalid use of incomplete type", Lhs);
         Kill_Lhs;
         goto Leave;
      end if;

      --  Now we can complete the resolution of the right hand side

      Set_Assignment_Type (Lhs, T1);

      --  If the target of the assignment is an entity of a mutable type and
      --  the expression is a conditional expression, its alternatives can be
      --  of different subtypes of the nominal type of the LHS, so they must be
      --  resolved with the base type, given that their subtype may differ from
      --  that of the target mutable object.

      if Is_Entity_Name (Lhs)
        and then Ekind_In (Entity (Lhs), E_In_Out_Parameter,
                                         E_Out_Parameter,
                                         E_Variable)
        and then Is_Composite_Type (T1)
        and then not Is_Constrained (Etype (Entity (Lhs)))
        and then Nkind_In (Rhs, N_If_Expression, N_Case_Expression)
      then
         Resolve (Rhs, Base_Type (T1));

      else
         Resolve (Rhs, T1);
      end if;

      --  This is the point at which we check for an unset reference

      Check_Unset_Reference (Rhs);
      Check_Unprotected_Access (Lhs, Rhs);

      --  Remaining steps are skipped if Rhs was syntactically in error

      if Rhs = Error then
         Kill_Lhs;
         goto Leave;
      end if;

      T2 := Etype (Rhs);

      if not Covers (T1, T2) then
         Wrong_Type (Rhs, Etype (Lhs));
         Kill_Lhs;
         goto Leave;
      end if;

      --  Ada 2005 (AI-326): In case of explicit dereference of incomplete
      --  types, use the non-limited view if available

      if Nkind (Rhs) = N_Explicit_Dereference
        and then Is_Tagged_Type (T2)
        and then Has_Non_Limited_View (T2)
      then
         T2 := Non_Limited_View (T2);
      end if;

      Set_Assignment_Type (Rhs, T2);

      if Total_Errors_Detected /= 0 then
         if No (T1) then
            T1 := Any_Type;
         end if;

         if No (T2) then
            T2 := Any_Type;
         end if;
      end if;

      if T1 = Any_Type or else T2 = Any_Type then
         Kill_Lhs;
         goto Leave;
      end if;

      --  If the rhs is class-wide or dynamically tagged, then require the lhs
      --  to be class-wide. The case where the rhs is a dynamically tagged call
      --  to a dispatching operation with a controlling access result is
      --  excluded from this check, since the target has an access type (and
      --  no tag propagation occurs in that case).

      if (Is_Class_Wide_Type (T2)
           or else (Is_Dynamically_Tagged (Rhs)
                     and then not Is_Access_Type (T1)))
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

      --  Propagate the tag from a class-wide target to the rhs when the rhs
      --  is a tag-indeterminate call.

      if Is_Tag_Indeterminate (Rhs) then
         if Is_Class_Wide_Type (T1) then
            Propagate_Tag (Lhs, Rhs);

         elsif Nkind (Rhs) = N_Function_Call
           and then Is_Entity_Name (Name (Rhs))
           and then Is_Abstract_Subprogram (Entity (Name (Rhs)))
         then
            Error_Msg_N
              ("call to abstract function must be dispatching", Name (Rhs));

         elsif Nkind (Rhs) = N_Qualified_Expression
           and then Nkind (Expression (Rhs)) = N_Function_Call
              and then Is_Entity_Name (Name (Expression (Rhs)))
              and then
                Is_Abstract_Subprogram (Entity (Name (Expression (Rhs))))
         then
            Error_Msg_N
              ("call to abstract function must be dispatching",
                Name (Expression (Rhs)));
         end if;
      end if;

      --  Ada 2005 (AI-385): When the lhs type is an anonymous access type,
      --  apply an implicit conversion of the rhs to that type to force
      --  appropriate static and run-time accessibility checks. This applies
      --  as well to anonymous access-to-subprogram types that are component
      --  subtypes or formal parameters.

      if Ada_Version >= Ada_2005 and then Is_Access_Type (T1) then
         if Is_Local_Anonymous_Access (T1)
           or else Ekind (T2) = E_Anonymous_Access_Subprogram_Type

           --  Handle assignment to an Ada 2012 stand-alone object
           --  of an anonymous access type.

           or else (Ekind (T1) = E_Anonymous_Access_Type
                     and then Nkind (Associated_Node_For_Itype (T1)) =
                                                       N_Object_Declaration)

         then
            Rewrite (Rhs, Convert_To (T1, Relocate_Node (Rhs)));
            Analyze_And_Resolve (Rhs, T1);
         end if;
      end if;

      --  Ada 2005 (AI-231): Assignment to not null variable

      if Ada_Version >= Ada_2005
        and then Can_Never_Be_Null (T1)
        and then not Assignment_OK (Lhs)
      then
         --  Case where we know the right hand side is null

         if Known_Null (Rhs) then
            Apply_Compile_Time_Constraint_Error
              (N      => Rhs,
               Msg    =>
                 "(Ada 2005) null not allowed in null-excluding objects??",
               Reason => CE_Null_Not_Allowed);

            --  We still mark this as a possible modification, that's necessary
            --  to reset Is_True_Constant, and desirable for xref purposes.

            Note_Possible_Modification (Lhs, Sure => True);
            goto Leave;

         --  If we know the right hand side is non-null, then we convert to the
         --  target type, since we don't need a run time check in that case.

         elsif not Can_Never_Be_Null (T2) then
            Rewrite (Rhs, Convert_To (T1, Relocate_Node (Rhs)));
            Analyze_And_Resolve (Rhs, T1);
         end if;
      end if;

      if Is_Scalar_Type (T1) then
         Apply_Scalar_Range_Check (Rhs, Etype (Lhs));

      --  For array types, verify that lengths match. If the right hand side
      --  is a function call that has been inlined, the assignment has been
      --  rewritten as a block, and the constraint check will be applied to the
      --  assignment within the block.

      elsif Is_Array_Type (T1)
        and then (Nkind (Rhs) /= N_Type_Conversion
                   or else Is_Constrained (Etype (Rhs)))
        and then (Nkind (Rhs) /= N_Function_Call
                   or else Nkind (N) /= N_Block_Statement)
      then
         --  Assignment verifies that the length of the Lsh and Rhs are equal,
         --  but of course the indexes do not have to match. If the right-hand
         --  side is a type conversion to an unconstrained type, a length check
         --  is performed on the expression itself during expansion. In rare
         --  cases, the redundant length check is computed on an index type
         --  with a different representation, triggering incorrect code in the
         --  back end.

         Apply_Length_Check (Rhs, Etype (Lhs));

      else
         --  Discriminant checks are applied in the course of expansion

         null;
      end if;

      --  Note: modifications of the Lhs may only be recorded after
      --  checks have been applied.

      Note_Possible_Modification (Lhs, Sure => True);

      --  ??? a real accessibility check is needed when ???

      --  Post warning for redundant assignment or variable to itself

      if Warn_On_Redundant_Constructs

         --  We only warn for source constructs

         and then Comes_From_Source (N)

         --  Where the object is the same on both sides

         and then Same_Object (Lhs, Original_Node (Rhs))

         --  But exclude the case where the right side was an operation that
         --  got rewritten (e.g. JUNK + K, where K was known to be zero). We
         --  don't want to warn in such a case, since it is reasonable to write
         --  such expressions especially when K is defined symbolically in some
         --  other package.

        and then Nkind (Original_Node (Rhs)) not in N_Op
      then
         if Nkind (Lhs) in N_Has_Entity then
            Error_Msg_NE -- CODEFIX
              ("?r?useless assignment of & to itself!", N, Entity (Lhs));
         else
            Error_Msg_N -- CODEFIX
              ("?r?useless assignment of object to itself!", N);
         end if;
      end if;

      --  Check for non-allowed composite assignment

      if not Support_Composite_Assign_On_Target
        and then (Is_Array_Type (T1) or else Is_Record_Type (T1))
        and then (not Has_Size_Clause (T1) or else Esize (T1) > 64)
      then
         Error_Msg_CRT ("composite assignment", N);
      end if;

      --  Check elaboration warning for left side if not in elab code

      if Legacy_Elaboration_Checks
        and not In_Subprogram_Or_Concurrent_Unit
      then
         Check_Elab_Assign (Lhs);
      end if;

      --  Save the scenario for later examination by the ABE Processing phase

      Record_Elaboration_Scenario (N);

      --  Set Referenced_As_LHS if appropriate. We only set this flag if the
      --  assignment is a source assignment in the extended main source unit.
      --  We are not interested in any reference information outside this
      --  context, or in compiler generated assignment statements.

      if Comes_From_Source (N)
        and then In_Extended_Main_Source_Unit (Lhs)
      then
         Set_Referenced_Modified (Lhs, Out_Param => False);
      end if;

      --  RM 7.3.2 (12/3): An assignment to a view conversion (from a type to
      --  one of its ancestors) requires an invariant check. Apply check only
      --  if expression comes from source, otherwise it will be applied when
      --  value is assigned to source entity. This is not done in GNATprove
      --  mode, as GNATprove handles invariant checks itself.

      if Nkind (Lhs) = N_Type_Conversion
        and then Has_Invariants (Etype (Expression (Lhs)))
        and then Comes_From_Source (Expression (Lhs))
        and then not GNATprove_Mode
      then
         Insert_After (N, Make_Invariant_Call (Expression (Lhs)));
      end if;

      --  Final step. If left side is an entity, then we may be able to reset
      --  the current tracked values to new safe values. We only have something
      --  to do if the left side is an entity name, and expansion has not
      --  modified the node into something other than an assignment, and of
      --  course we only capture values if it is safe to do so.

      if Is_Entity_Name (Lhs)
        and then Nkind (N) = N_Assignment_Statement
      then
         declare
            Ent : constant Entity_Id := Entity (Lhs);

         begin
            if Safe_To_Capture_Value (N, Ent) then

               --  If simple variable on left side, warn if this assignment
               --  blots out another one (rendering it useless). We only do
               --  this for source assignments, otherwise we can generate bogus
               --  warnings when an assignment is rewritten as another
               --  assignment, and gets tied up with itself.

               --  There may have been a previous reference to a component of
               --  the variable, which in general removes the Last_Assignment
               --  field of the variable to indicate a relevant use of the
               --  previous assignment. However, if the assignment is to a
               --  subcomponent the reference may not have registered, because
               --  it is not possible to determine whether the context is an
               --  assignment. In those cases we generate a Deferred_Reference,
               --  to be used at the end of compilation to generate the right
               --  kind of reference, and we suppress a potential warning for
               --  a useless assignment, which might be premature. This may
               --  lose a warning in rare cases, but seems preferable to a
               --  misleading warning.

               if Warn_On_Modified_Unread
                 and then Is_Assignable (Ent)
                 and then Comes_From_Source (N)
                 and then In_Extended_Main_Source_Unit (Ent)
                 and then not Has_Deferred_Reference (Ent)
               then
                  Warn_On_Useless_Assignment (Ent, N);
               end if;

               --  If we are assigning an access type and the left side is an
               --  entity, then make sure that the Is_Known_[Non_]Null flags
               --  properly reflect the state of the entity after assignment.

               if Is_Access_Type (T1) then
                  if Known_Non_Null (Rhs) then
                     Set_Is_Known_Non_Null (Ent, True);

                  elsif Known_Null (Rhs)
                    and then not Can_Never_Be_Null (Ent)
                  then
                     Set_Is_Known_Null (Ent, True);

                  else
                     Set_Is_Known_Null (Ent, False);

                     if not Can_Never_Be_Null (Ent) then
                        Set_Is_Known_Non_Null (Ent, False);
                     end if;
                  end if;

               --  For discrete types, we may be able to set the current value
               --  if the value is known at compile time.

               elsif Is_Discrete_Type (T1)
                 and then Compile_Time_Known_Value (Rhs)
               then
                  Set_Current_Value (Ent, Rhs);
               else
                  Set_Current_Value (Ent, Empty);
               end if;

            --  If not safe to capture values, kill them

            else
               Kill_Lhs;
            end if;
         end;
      end if;

      --  If assigning to an object in whole or in part, note location of
      --  assignment in case no one references value. We only do this for
      --  source assignments, otherwise we can generate bogus warnings when an
      --  assignment is rewritten as another assignment, and gets tied up with
      --  itself.

      declare
         Ent : constant Entity_Id := Get_Enclosing_Object (Lhs);
      begin
         if Present (Ent)
           and then Safe_To_Capture_Value (N, Ent)
           and then Nkind (N) = N_Assignment_Statement
           and then Warn_On_Modified_Unread
           and then Is_Assignable (Ent)
           and then Comes_From_Source (N)
           and then In_Extended_Main_Source_Unit (Ent)
         then
            Set_Last_Assignment (Ent, Lhs);
         end if;
      end;

      Analyze_Dimension (N);

   <<Leave>>
      Restore_Ghost_Region (Saved_GM, Saved_IGR);

      --  If the right-hand side contains target names, expansion has been
      --  disabled to prevent expansion that might move target names out of
      --  the context of the assignment statement. Restore the expander mode
      --  now so that assignment statement can be properly expanded.

      if Nkind (N) = N_Assignment_Statement then
         if Has_Target_Names (N) then
            Expander_Mode_Restore;
            Full_Analysis := Save_Full_Analysis;
         end if;

         pragma Assert (not Should_Transform_BIP_Assignment (Typ => T1));
      end if;
   end Analyze_Assignment;

   -----------------------------
   -- Analyze_Block_Statement --
   -----------------------------

   procedure Analyze_Block_Statement (N : Node_Id) is
      procedure Install_Return_Entities (Scop : Entity_Id);
      --  Install all entities of return statement scope Scop in the visibility
      --  chain except for the return object since its entity is reused in a
      --  renaming.

      -----------------------------
      -- Install_Return_Entities --
      -----------------------------

      procedure Install_Return_Entities (Scop : Entity_Id) is
         Id : Entity_Id;

      begin
         Id := First_Entity (Scop);
         while Present (Id) loop

            --  Do not install the return object

            if not Ekind_In (Id, E_Constant, E_Variable)
              or else not Is_Return_Object (Id)
            then
               Install_Entity (Id);
            end if;

            Next_Entity (Id);
         end loop;
      end Install_Return_Entities;

      --  Local constants and variables

      Decls : constant List_Id := Declarations (N);
      Id    : constant Node_Id := Identifier (N);
      HSS   : constant Node_Id := Handled_Statement_Sequence (N);

      Is_BIP_Return_Statement : Boolean;

   --  Start of processing for Analyze_Block_Statement

   begin
      --  In SPARK mode, we reject block statements. Note that the case of
      --  block statements generated by the expander is fine.

      if Nkind (Original_Node (N)) = N_Block_Statement then
         Check_SPARK_05_Restriction ("block statement is not allowed", N);
      end if;

      --  If no handled statement sequence is present, things are really messed
      --  up, and we just return immediately (defence against previous errors).

      if No (HSS) then
         Check_Error_Detected;
         return;
      end if;

      --  Detect whether the block is actually a rewritten return statement of
      --  a build-in-place function.

      Is_BIP_Return_Statement :=
        Present (Id)
          and then Present (Entity (Id))
          and then Ekind (Entity (Id)) = E_Return_Statement
          and then Is_Build_In_Place_Function
                     (Return_Applies_To (Entity (Id)));

      --  Normal processing with HSS present

      declare
         EH  : constant List_Id := Exception_Handlers (HSS);
         Ent : Entity_Id        := Empty;
         S   : Entity_Id;

         Save_Unblocked_Exit_Count : constant Nat := Unblocked_Exit_Count;
         --  Recursively save value of this global, will be restored on exit

      begin
         --  Initialize unblocked exit count for statements of begin block
         --  plus one for each exception handler that is present.

         Unblocked_Exit_Count := 1;

         if Present (EH) then
            Unblocked_Exit_Count := Unblocked_Exit_Count + List_Length (EH);
         end if;

         --  If a label is present analyze it and mark it as referenced

         if Present (Id) then
            Analyze (Id);
            Ent := Entity (Id);

            --  An error defense. If we have an identifier, but no entity, then
            --  something is wrong. If previous errors, then just remove the
            --  identifier and continue, otherwise raise an exception.

            if No (Ent) then
               Check_Error_Detected;
               Set_Identifier (N, Empty);

            else
               Set_Ekind (Ent, E_Block);
               Generate_Reference (Ent, N, ' ');
               Generate_Definition (Ent);

               if Nkind (Parent (Ent)) = N_Implicit_Label_Declaration then
                  Set_Label_Construct (Parent (Ent), N);
               end if;
            end if;
         end if;

         --  If no entity set, create a label entity

         if No (Ent) then
            Ent := New_Internal_Entity (E_Block, Current_Scope, Sloc (N), 'B');
            Set_Identifier (N, New_Occurrence_Of (Ent, Sloc (N)));
            Set_Parent (Ent, N);
         end if;

         Set_Etype (Ent, Standard_Void_Type);
         Set_Block_Node (Ent, Identifier (N));
         Push_Scope (Ent);

         --  The block served as an extended return statement. Ensure that any
         --  entities created during the analysis and expansion of the return
         --  object declaration are once again visible.

         if Is_BIP_Return_Statement then
            Install_Return_Entities (Ent);
         end if;

         if Present (Decls) then
            Analyze_Declarations (Decls);
            Check_Completion;
            Inspect_Deferred_Constant_Completion (Decls);
         end if;

         Analyze (HSS);
         Process_End_Label (HSS, 'e', Ent);

         --  If exception handlers are present, then we indicate that enclosing
         --  scopes contain a block with handlers. We only need to mark non-
         --  generic scopes.

         if Present (EH) then
            S := Scope (Ent);
            loop
               Set_Has_Nested_Block_With_Handler (S);
               exit when Is_Overloadable (S)
                 or else Ekind (S) = E_Package
                 or else Is_Generic_Unit (S);
               S := Scope (S);
            end loop;
         end if;

         Check_References (Ent);
         Update_Use_Clause_Chain;
         End_Scope;

         if Unblocked_Exit_Count = 0 then
            Unblocked_Exit_Count := Save_Unblocked_Exit_Count;
            Check_Unreachable_Code (N);
         else
            Unblocked_Exit_Count := Save_Unblocked_Exit_Count;
         end if;
      end;
   end Analyze_Block_Statement;

   --------------------------------
   -- Analyze_Compound_Statement --
   --------------------------------

   procedure Analyze_Compound_Statement (N : Node_Id) is
   begin
      Analyze_List (Actions (N));
   end Analyze_Compound_Statement;

   ----------------------------
   -- Analyze_Case_Statement --
   ----------------------------

   procedure Analyze_Case_Statement (N : Node_Id) is
      Exp            : Node_Id;
      Exp_Type       : Entity_Id;
      Exp_Btype      : Entity_Id;
      Last_Choice    : Nat;

      Others_Present : Boolean;
      --  Indicates if Others was present

      pragma Warnings (Off, Last_Choice);
      --  Don't care about assigned value

      Statements_Analyzed : Boolean := False;
      --  Set True if at least some statement sequences get analyzed. If False
      --  on exit, means we had a serious error that prevented full analysis of
      --  the case statement, and as a result it is not a good idea to output
      --  warning messages about unreachable code.

      Save_Unblocked_Exit_Count : constant Nat := Unblocked_Exit_Count;
      --  Recursively save value of this global, will be restored on exit

      procedure Non_Static_Choice_Error (Choice : Node_Id);
      --  Error routine invoked by the generic instantiation below when the
      --  case statement has a non static choice.

      procedure Process_Statements (Alternative : Node_Id);
      --  Analyzes the statements associated with a case alternative. Needed
      --  by instantiation below.

      package Analyze_Case_Choices is new
        Generic_Analyze_Choices
          (Process_Associated_Node   => Process_Statements);
      use Analyze_Case_Choices;
      --  Instantiation of the generic choice analysis package

      package Check_Case_Choices is new
        Generic_Check_Choices
          (Process_Empty_Choice      => No_OP,
           Process_Non_Static_Choice => Non_Static_Choice_Error,
           Process_Associated_Node   => No_OP);
      use Check_Case_Choices;
      --  Instantiation of the generic choice processing package

      -----------------------------
      -- Non_Static_Choice_Error --
      -----------------------------

      procedure Non_Static_Choice_Error (Choice : Node_Id) is
      begin
         Flag_Non_Static_Expr
           ("choice given in case statement is not static!", Choice);
      end Non_Static_Choice_Error;

      ------------------------
      -- Process_Statements --
      ------------------------

      procedure Process_Statements (Alternative : Node_Id) is
         Choices : constant List_Id := Discrete_Choices (Alternative);
         Ent     : Entity_Id;

      begin
         Unblocked_Exit_Count := Unblocked_Exit_Count + 1;
         Statements_Analyzed := True;

         --  An interesting optimization. If the case statement expression
         --  is a simple entity, then we can set the current value within an
         --  alternative if the alternative has one possible value.

         --    case N is
         --      when 1      => alpha
         --      when 2 | 3  => beta
         --      when others => gamma

         --  Here we know that N is initially 1 within alpha, but for beta and
         --  gamma, we do not know anything more about the initial value.

         if Is_Entity_Name (Exp) then
            Ent := Entity (Exp);

            if Ekind_In (Ent, E_Variable,
                              E_In_Out_Parameter,
                              E_Out_Parameter)
            then
               if List_Length (Choices) = 1
                 and then Nkind (First (Choices)) in N_Subexpr
                 and then Compile_Time_Known_Value (First (Choices))
               then
                  Set_Current_Value (Entity (Exp), First (Choices));
               end if;

               Analyze_Statements (Statements (Alternative));

               --  After analyzing the case, set the current value to empty
               --  since we won't know what it is for the next alternative
               --  (unless reset by this same circuit), or after the case.

               Set_Current_Value (Entity (Exp), Empty);
               return;
            end if;
         end if;

         --  Case where expression is not an entity name of a variable

         Analyze_Statements (Statements (Alternative));
      end Process_Statements;

   --  Start of processing for Analyze_Case_Statement

   begin
      Unblocked_Exit_Count := 0;
      Exp := Expression (N);
      Analyze (Exp);

      --  The expression must be of any discrete type. In rare cases, the
      --  expander constructs a case statement whose expression has a private
      --  type whose full view is discrete. This can happen when generating
      --  a stream operation for a variant type after the type is frozen,
      --  when the partial of view of the type of the discriminant is private.
      --  In that case, use the full view to analyze case alternatives.

      if not Is_Overloaded (Exp)
        and then not Comes_From_Source (N)
        and then Is_Private_Type (Etype (Exp))
        and then Present (Full_View (Etype (Exp)))
        and then Is_Discrete_Type (Full_View (Etype (Exp)))
      then
         Resolve (Exp, Etype (Exp));
         Exp_Type := Full_View (Etype (Exp));

      else
         Analyze_And_Resolve (Exp, Any_Discrete);
         Exp_Type := Etype (Exp);
      end if;

      Check_Unset_Reference (Exp);
      Exp_Btype := Base_Type (Exp_Type);

      --  The expression must be of a discrete type which must be determinable
      --  independently of the context in which the expression occurs, but
      --  using the fact that the expression must be of a discrete type.
      --  Moreover, the type this expression must not be a character literal
      --  (which is always ambiguous) or, for Ada-83, a generic formal type.

      --  If error already reported by Resolve, nothing more to do

      if Exp_Btype = Any_Discrete or else Exp_Btype = Any_Type then
         return;

      elsif Exp_Btype = Any_Character then
         Error_Msg_N
           ("character literal as case expression is ambiguous", Exp);
         return;

      elsif Ada_Version = Ada_83
        and then (Is_Generic_Type (Exp_Btype)
                   or else Is_Generic_Type (Root_Type (Exp_Btype)))
      then
         Error_Msg_N
           ("(Ada 83) case expression cannot be of a generic type", Exp);
         return;
      end if;

      --  If the case expression is a formal object of mode in out, then treat
      --  it as having a nonstatic subtype by forcing use of the base type
      --  (which has to get passed to Check_Case_Choices below). Also use base
      --  type when the case expression is parenthesized.

      if Paren_Count (Exp) > 0
        or else (Is_Entity_Name (Exp)
                  and then Ekind (Entity (Exp)) = E_Generic_In_Out_Parameter)
      then
         Exp_Type := Exp_Btype;
      end if;

      --  Call instantiated procedures to analyzwe and check discrete choices

      Analyze_Choices (Alternatives (N), Exp_Type);
      Check_Choices (N, Alternatives (N), Exp_Type, Others_Present);

      --  Case statement with single OTHERS alternative not allowed in SPARK

      if Others_Present and then List_Length (Alternatives (N)) = 1 then
         Check_SPARK_05_Restriction
           ("OTHERS as unique case alternative is not allowed", N);
      end if;

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

      --  If the expander is active it will detect the case of a statically
      --  determined single alternative and remove warnings for the case, but
      --  if we are not doing expansion, that circuit won't be active. Here we
      --  duplicate the effect of removing warnings in the same way, so that
      --  we will get the same set of warnings in -gnatc mode.

      if not Expander_Active
        and then Compile_Time_Known_Value (Expression (N))
        and then Serious_Errors_Detected = 0
      then
         declare
            Chosen : constant Node_Id := Find_Static_Alternative (N);
            Alt    : Node_Id;

         begin
            Alt := First (Alternatives (N));
            while Present (Alt) loop
               if Alt /= Chosen then
                  Remove_Warning_Messages (Statements (Alt));
               end if;

               Next (Alt);
            end loop;
         end;
      end if;
   end Analyze_Case_Statement;

   ----------------------------
   -- Analyze_Exit_Statement --
   ----------------------------

   --  If the exit includes a name, it must be the name of a currently open
   --  loop. Otherwise there must be an innermost open loop on the stack, to
   --  which the statement implicitly refers.

   --  Additionally, in SPARK mode:

   --    The exit can only name the closest enclosing loop;

   --    An exit with a when clause must be directly contained in a loop;

   --    An exit without a when clause must be directly contained in an
   --    if-statement with no elsif or else, which is itself directly contained
   --    in a loop. The exit must be the last statement in the if-statement.

   procedure Analyze_Exit_Statement (N : Node_Id) is
      Target   : constant Node_Id := Name (N);
      Cond     : constant Node_Id := Condition (N);
      Scope_Id : Entity_Id := Empty;  -- initialize to prevent warning
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
            if Has_Loop_In_Inner_Open_Scopes (U_Name) then
               Check_SPARK_05_Restriction
                 ("exit label must name the closest enclosing loop", N);
            end if;

            Set_Has_Exit (U_Name);
         end if;

      else
         U_Name := Empty;
      end if;

      for J in reverse 0 .. Scope_Stack.Last loop
         Scope_Id := Scope_Stack.Table (J).Entity;
         Kind := Ekind (Scope_Id);

         if Kind = E_Loop and then (No (Target) or else Scope_Id = U_Name) then
            Set_Has_Exit (Scope_Id);
            exit;

         elsif Kind = E_Block
           or else Kind = E_Loop
           or else Kind = E_Return_Statement
         then
            null;

         else
            Error_Msg_N
              ("cannot exit from program unit or accept statement", N);
            return;
         end if;
      end loop;

      --  Verify that if present the condition is a Boolean expression

      if Present (Cond) then
         Analyze_And_Resolve (Cond, Any_Boolean);
         Check_Unset_Reference (Cond);
      end if;

      --  In SPARK mode, verify that the exit statement respects the SPARK
      --  restrictions.

      if Present (Cond) then
         if Nkind (Parent (N)) /= N_Loop_Statement then
            Check_SPARK_05_Restriction
              ("exit with when clause must be directly in loop", N);
         end if;

      else
         if Nkind (Parent (N)) /= N_If_Statement then
            if Nkind (Parent (N)) = N_Elsif_Part then
               Check_SPARK_05_Restriction
                 ("exit must be in IF without ELSIF", N);
            else
               Check_SPARK_05_Restriction ("exit must be directly in IF", N);
            end if;

         elsif Nkind (Parent (Parent (N))) /= N_Loop_Statement then
            Check_SPARK_05_Restriction
              ("exit must be in IF directly in loop", N);

         --  First test the presence of ELSE, so that an exit in an ELSE leads
         --  to an error mentioning the ELSE.

         elsif Present (Else_Statements (Parent (N))) then
            Check_SPARK_05_Restriction ("exit must be in IF without ELSE", N);

         --  An exit in an ELSIF does not reach here, as it would have been
         --  detected in the case (Nkind (Parent (N)) /= N_If_Statement).

         elsif Present (Elsif_Parts (Parent (N))) then
            Check_SPARK_05_Restriction ("exit must be in IF without ELSIF", N);
         end if;
      end if;

      --  Chain exit statement to associated loop entity

      Set_Next_Exit_Statement  (N, First_Exit_Statement (Scope_Id));
      Set_First_Exit_Statement (Scope_Id, N);

      --  Since the exit may take us out of a loop, any previous assignment
      --  statement is not useless, so clear last assignment indications. It
      --  is OK to keep other current values, since if the exit statement
      --  does not exit, then the current values are still valid.

      Kill_Current_Values (Last_Assignment_Only => True);
   end Analyze_Exit_Statement;

   ----------------------------
   -- Analyze_Goto_Statement --
   ----------------------------

   procedure Analyze_Goto_Statement (N : Node_Id) is
      Label       : constant Node_Id := Name (N);
      Scope_Id    : Entity_Id;
      Label_Scope : Entity_Id;
      Label_Ent   : Entity_Id;

   begin
      Check_SPARK_05_Restriction ("goto statement is not allowed", N);

      --  Actual semantic checks

      Check_Unreachable_Code (N);
      Kill_Current_Values (Last_Assignment_Only => True);

      Analyze (Label);
      Label_Ent := Entity (Label);

      --  Ignore previous error

      if Label_Ent = Any_Id then
         Check_Error_Detected;
         return;

      --  We just have a label as the target of a goto

      elsif Ekind (Label_Ent) /= E_Label then
         Error_Msg_N ("target of goto statement must be a label", Label);
         return;

      --  Check that the target of the goto is reachable according to Ada
      --  scoping rules. Note: the special gotos we generate for optimizing
      --  local handling of exceptions would violate these rules, but we mark
      --  such gotos as analyzed when built, so this code is never entered.

      elsif not Reachable (Label_Ent) then
         Error_Msg_N ("target of goto statement is not reachable", Label);
         return;
      end if;

      --  Here if goto passes initial validity checks

      Label_Scope := Enclosing_Scope (Label_Ent);

      for J in reverse 0 .. Scope_Stack.Last loop
         Scope_Id := Scope_Stack.Table (J).Entity;

         if Label_Scope = Scope_Id
           or else not Ekind_In (Scope_Id, E_Block, E_Loop, E_Return_Statement)
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

   --  A special complication arises in the analysis of if statements

   --  The expander has circuitry to completely delete code that it can tell
   --  will not be executed (as a result of compile time known conditions). In
   --  the analyzer, we ensure that code that will be deleted in this manner
   --  is analyzed but not expanded. This is obviously more efficient, but
   --  more significantly, difficulties arise if code is expanded and then
   --  eliminated (e.g. exception table entries disappear). Similarly, itypes
   --  generated in deleted code must be frozen from start, because the nodes
   --  on which they depend will not be available at the freeze point.

   procedure Analyze_If_Statement (N : Node_Id) is
      E : Node_Id;

      Save_Unblocked_Exit_Count : constant Nat := Unblocked_Exit_Count;
      --  Recursively save value of this global, will be restored on exit

      Save_In_Deleted_Code : Boolean;

      Del : Boolean := False;
      --  This flag gets set True if a True condition has been found, which
      --  means that remaining ELSE/ELSIF parts are deleted.

      procedure Analyze_Cond_Then (Cnode : Node_Id);
      --  This is applied to either the N_If_Statement node itself or to an
      --  N_Elsif_Part node. It deals with analyzing the condition and the THEN
      --  statements associated with it.

      -----------------------
      -- Analyze_Cond_Then --
      -----------------------

      procedure Analyze_Cond_Then (Cnode : Node_Id) is
         Cond : constant Node_Id := Condition (Cnode);
         Tstm : constant List_Id := Then_Statements (Cnode);

      begin
         Unblocked_Exit_Count := Unblocked_Exit_Count + 1;
         Analyze_And_Resolve (Cond, Any_Boolean);
         Check_Unset_Reference (Cond);
         Set_Current_Value_Condition (Cnode);

         --  If already deleting, then just analyze then statements

         if Del then
            Analyze_Statements (Tstm);

         --  Compile time known value, not deleting yet

         elsif Compile_Time_Known_Value (Cond) then
            Save_In_Deleted_Code := In_Deleted_Code;

            --  If condition is True, then analyze the THEN statements and set
            --  no expansion for ELSE and ELSIF parts.

            if Is_True (Expr_Value (Cond)) then
               Analyze_Statements (Tstm);
               Del := True;
               Expander_Mode_Save_And_Set (False);
               In_Deleted_Code := True;

            --  If condition is False, analyze THEN with expansion off

            else -- Is_False (Expr_Value (Cond))
               Expander_Mode_Save_And_Set (False);
               In_Deleted_Code := True;
               Analyze_Statements (Tstm);
               Expander_Mode_Restore;
               In_Deleted_Code := Save_In_Deleted_Code;
            end if;

         --  Not known at compile time, not deleting, normal analysis

         else
            Analyze_Statements (Tstm);
         end if;
      end Analyze_Cond_Then;

   --  Start of processing for Analyze_If_Statement

   begin
      --  Initialize exit count for else statements. If there is no else part,
      --  this count will stay non-zero reflecting the fact that the uncovered
      --  else case is an unblocked exit.

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
         In_Deleted_Code := Save_In_Deleted_Code;
      end if;

      if not Expander_Active
        and then Compile_Time_Known_Value (Condition (N))
        and then Serious_Errors_Detected = 0
      then
         if Is_True (Expr_Value (Condition (N))) then
            Remove_Warning_Messages (Else_Statements (N));

            if Present (Elsif_Parts (N)) then
               E := First (Elsif_Parts (N));
               while Present (E) loop
                  Remove_Warning_Messages (Then_Statements (E));
                  Next (E);
               end loop;
            end if;

         else
            Remove_Warning_Messages (Then_Statements (N));
         end if;
      end if;

      --  Warn on redundant if statement that has no effect

      --  Note, we could also check empty ELSIF parts ???

      if Warn_On_Redundant_Constructs

        --  If statement must be from source

        and then Comes_From_Source (N)

        --  Condition must not have obvious side effect

        and then Has_No_Obvious_Side_Effects (Condition (N))

        --  No elsif parts of else part

        and then No (Elsif_Parts (N))
        and then No (Else_Statements (N))

        --  Then must be a single null statement

        and then List_Length (Then_Statements (N)) = 1
      then
         --  Go to original node, since we may have rewritten something as
         --  a null statement (e.g. a case we could figure the outcome of).

         declare
            T : constant Node_Id := First (Then_Statements (N));
            S : constant Node_Id := Original_Node (T);

         begin
            if Comes_From_Source (S) and then Nkind (S) = N_Null_Statement then
               Error_Msg_N ("if statement has no effect?r?", N);
            end if;
         end;
      end if;
   end Analyze_If_Statement;

   ----------------------------------------
   -- Analyze_Implicit_Label_Declaration --
   ----------------------------------------

   --  An implicit label declaration is generated in the innermost enclosing
   --  declarative part. This is done for labels, and block and loop names.

   --  Note: any changes in this routine may need to be reflected in
   --  Analyze_Label_Entity.

   procedure Analyze_Implicit_Label_Declaration (N : Node_Id) is
      Id : constant Node_Id := Defining_Identifier (N);
   begin
      Enter_Name          (Id);
      Set_Ekind           (Id, E_Label);
      Set_Etype           (Id, Standard_Void_Type);
      Set_Enclosing_Scope (Id, Current_Scope);
   end Analyze_Implicit_Label_Declaration;

   ------------------------------
   -- Analyze_Iteration_Scheme --
   ------------------------------

   procedure Analyze_Iteration_Scheme (N : Node_Id) is
      Cond      : Node_Id;
      Iter_Spec : Node_Id;
      Loop_Spec : Node_Id;

   begin
      --  For an infinite loop, there is no iteration scheme

      if No (N) then
         return;
      end if;

      Cond      := Condition (N);
      Iter_Spec := Iterator_Specification (N);
      Loop_Spec := Loop_Parameter_Specification (N);

      if Present (Cond) then
         Analyze_And_Resolve (Cond, Any_Boolean);
         Check_Unset_Reference (Cond);
         Set_Current_Value_Condition (N);

      elsif Present (Iter_Spec) then
         Analyze_Iterator_Specification (Iter_Spec);

      else
         Analyze_Loop_Parameter_Specification (Loop_Spec);
      end if;
   end Analyze_Iteration_Scheme;

   ------------------------------------
   -- Analyze_Iterator_Specification --
   ------------------------------------

   procedure Analyze_Iterator_Specification (N : Node_Id) is
      Def_Id    : constant Node_Id    := Defining_Identifier (N);
      Iter_Name : constant Node_Id    := Name (N);
      Loc       : constant Source_Ptr := Sloc (N);
      Subt      : constant Node_Id    := Subtype_Indication (N);

      Bas : Entity_Id := Empty;  -- initialize to prevent warning
      Typ : Entity_Id;

      procedure Check_Reverse_Iteration (Typ : Entity_Id);
      --  For an iteration over a container, if the loop carries the Reverse
      --  indicator, verify that the container type has an Iterate aspect that
      --  implements the reversible iterator interface.

      procedure Check_Subtype_Indication (Comp_Type : Entity_Id);
      --  If a subtype indication is present, verify that it is consistent
      --  with the component type of the array or container name.

      function Get_Cursor_Type (Typ : Entity_Id) return Entity_Id;
      --  For containers with Iterator and related aspects, the cursor is
      --  obtained by locating an entity with the proper name in the scope
      --  of the type.

      -----------------------------
      -- Check_Reverse_Iteration --
      -----------------------------

      procedure Check_Reverse_Iteration (Typ : Entity_Id) is
      begin
         if Reverse_Present (N) then
            if Is_Array_Type (Typ)
              or else Is_Reversible_Iterator (Typ)
              or else
                (Present (Find_Aspect (Typ, Aspect_Iterable))
                  and then
                    Present
                      (Get_Iterable_Type_Primitive (Typ, Name_Previous)))
            then
               null;
            else
               Error_Msg_NE
                 ("container type does not support reverse iteration", N, Typ);
            end if;
         end if;
      end Check_Reverse_Iteration;

      -------------------------------
      --  Check_Subtype_Indication --
      -------------------------------

      procedure Check_Subtype_Indication (Comp_Type : Entity_Id) is
      begin
         if Present (Subt)
           and then (not Covers (Base_Type ((Bas)), Comp_Type)
                      or else not Subtypes_Statically_Match (Bas, Comp_Type))
         then
            if Is_Array_Type (Typ) then
               Error_Msg_N
                 ("subtype indication does not match component type", Subt);
            else
               Error_Msg_N
                 ("subtype indication does not match element type", Subt);
            end if;
         end if;
      end Check_Subtype_Indication;

      ---------------------
      -- Get_Cursor_Type --
      ---------------------

      function Get_Cursor_Type (Typ : Entity_Id) return Entity_Id is
         Ent : Entity_Id;

      begin
         --  If iterator type is derived, the cursor is declared in the scope
         --  of the parent type.

         if Is_Derived_Type (Typ) then
            Ent := First_Entity (Scope (Etype (Typ)));
         else
            Ent := First_Entity (Scope (Typ));
         end if;

         while Present (Ent) loop
            exit when Chars (Ent) = Name_Cursor;
            Next_Entity (Ent);
         end loop;

         if No (Ent) then
            return Any_Type;
         end if;

         --  The cursor is the target of generated assignments in the
         --  loop, and cannot have a limited type.

         if Is_Limited_Type (Etype (Ent)) then
            Error_Msg_N ("cursor type cannot be limited", N);
         end if;

         return Etype (Ent);
      end Get_Cursor_Type;

   --   Start of processing for Analyze_Iterator_Specification

   begin
      Enter_Name (Def_Id);

      --  AI12-0151 specifies that when the subtype indication is present, it
      --  must statically match the type of the array or container element.
      --  To simplify this check, we introduce a subtype declaration with the
      --  given subtype indication when it carries a constraint, and rewrite
      --  the original as a reference to the created subtype entity.

      if Present (Subt) then
         if Nkind (Subt) = N_Subtype_Indication then
            declare
               S    : constant Entity_Id := Make_Temporary (Sloc (Subt), 'S');
               Decl : constant Node_Id :=
                        Make_Subtype_Declaration (Loc,
                          Defining_Identifier => S,
                          Subtype_Indication  => New_Copy_Tree (Subt));
            begin
               Insert_Before (Parent (Parent (N)), Decl);
               Analyze (Decl);
               Rewrite (Subt, New_Occurrence_Of (S, Sloc (Subt)));
            end;
         else
            Analyze (Subt);
         end if;

         --  Save entity of subtype indication for subsequent check

         Bas := Entity (Subt);
      end if;

      Preanalyze_Range (Iter_Name);

      --  If the domain of iteration is a function call, make sure the function
      --  itself is frozen. This is an issue if this is a local expression
      --  function.

      if Nkind (Iter_Name) = N_Function_Call
        and then Is_Entity_Name (Name (Iter_Name))
        and then Full_Analysis
        and then (In_Assertion_Expr = 0 or else Assertions_Enabled)
      then
         Freeze_Before (N, Entity (Name (Iter_Name)));
      end if;

      --  Set the kind of the loop variable, which is not visible within the
      --  iterator name.

      Set_Ekind (Def_Id, E_Variable);

      --  Provide a link between the iterator variable and the container, for
      --  subsequent use in cross-reference and modification information.

      if Of_Present (N) then
         Set_Related_Expression (Def_Id, Iter_Name);

         --  For a container, the iterator is specified through the aspect

         if not Is_Array_Type (Etype (Iter_Name)) then
            declare
               Iterator : constant Entity_Id :=
                            Find_Value_Of_Aspect
                              (Etype (Iter_Name), Aspect_Default_Iterator);

               I  : Interp_Index;
               It : Interp;

            begin
               if No (Iterator) then
                  null;  --  error reported below

               elsif not Is_Overloaded (Iterator) then
                  Check_Reverse_Iteration (Etype (Iterator));

               --  If Iterator is overloaded, use reversible iterator if one is
               --  available.

               elsif Is_Overloaded (Iterator) then
                  Get_First_Interp (Iterator, I, It);
                  while Present (It.Nam) loop
                     if Ekind (It.Nam) = E_Function
                       and then Is_Reversible_Iterator (Etype (It.Nam))
                     then
                        Set_Etype (Iterator, It.Typ);
                        Set_Entity (Iterator, It.Nam);
                        exit;
                     end if;

                     Get_Next_Interp (I, It);
                  end loop;

                  Check_Reverse_Iteration (Etype (Iterator));
               end if;
            end;
         end if;
      end if;

      --  If the domain of iteration is an expression, create a declaration for
      --  it, so that finalization actions are introduced outside of the loop.
      --  The declaration must be a renaming because the body of the loop may
      --  assign to elements.

      if not Is_Entity_Name (Iter_Name)

        --  When the context is a quantified expression, the renaming
        --  declaration is delayed until the expansion phase if we are
        --  doing expansion.

        and then (Nkind (Parent (N)) /= N_Quantified_Expression
                   or else Operating_Mode = Check_Semantics)

        --  Do not perform this expansion for ASIS and when expansion is
        --  disabled, where the temporary may hide the transformation of a
        --  selected component into a prefixed function call, and references
        --  need to see the original expression.

        and then Expander_Active
      then
         declare
            Id    : constant Entity_Id := Make_Temporary (Loc, 'R', Iter_Name);
            Decl  : Node_Id;
            Act_S : Node_Id;

         begin

            --  If the domain of iteration is an array component that depends
            --  on a discriminant, create actual subtype for it. preanalysis
            --  does not generate the actual subtype of a selected component.

            if Nkind (Iter_Name) = N_Selected_Component
              and then Is_Array_Type (Etype (Iter_Name))
            then
               Act_S :=
                 Build_Actual_Subtype_Of_Component
                   (Etype (Selector_Name (Iter_Name)), Iter_Name);
               Insert_Action (N, Act_S);

               if Present (Act_S) then
                  Typ := Defining_Identifier (Act_S);
               else
                  Typ := Etype (Iter_Name);
               end if;

            else
               Typ := Etype (Iter_Name);

               --  Verify that the expression produces an iterator

               if not Of_Present (N) and then not Is_Iterator (Typ)
                 and then not Is_Array_Type (Typ)
                 and then No (Find_Aspect (Typ, Aspect_Iterable))
               then
                  Error_Msg_N
                    ("expect object that implements iterator interface",
                     Iter_Name);
               end if;
            end if;

            --  Protect against malformed iterator

            if Typ = Any_Type then
               Error_Msg_N ("invalid expression in loop iterator", Iter_Name);
               return;
            end if;

            if not Of_Present (N) then
               Check_Reverse_Iteration (Typ);
            end if;

            --  The name in the renaming declaration may be a function call.
            --  Indicate that it does not come from source, to suppress
            --  spurious warnings on renamings of parameterless functions,
            --  a common enough idiom in user-defined iterators.

            Decl :=
              Make_Object_Renaming_Declaration (Loc,
                Defining_Identifier => Id,
                Subtype_Mark        => New_Occurrence_Of (Typ, Loc),
                Name                =>
                  New_Copy_Tree (Iter_Name, New_Sloc => Loc));

            Insert_Actions (Parent (Parent (N)), New_List (Decl));
            Rewrite (Name (N), New_Occurrence_Of (Id, Loc));
            Set_Etype (Id, Typ);
            Set_Etype (Name (N), Typ);
         end;

      --  Container is an entity or an array with uncontrolled components, or
      --  else it is a container iterator given by a function call, typically
      --  called Iterate in the case of predefined containers, even though
      --  Iterate is not a reserved name. What matters is that the return type
      --  of the function is an iterator type.

      elsif Is_Entity_Name (Iter_Name) then
         Analyze (Iter_Name);

         if Nkind (Iter_Name) = N_Function_Call then
            declare
               C  : constant Node_Id := Name (Iter_Name);
               I  : Interp_Index;
               It : Interp;

            begin
               if not Is_Overloaded (Iter_Name) then
                  Resolve (Iter_Name, Etype (C));

               else
                  Get_First_Interp (C, I, It);
                  while It.Typ /= Empty loop
                     if Reverse_Present (N) then
                        if Is_Reversible_Iterator (It.Typ) then
                           Resolve (Iter_Name, It.Typ);
                           exit;
                        end if;

                     elsif Is_Iterator (It.Typ) then
                        Resolve (Iter_Name, It.Typ);
                        exit;
                     end if;

                     Get_Next_Interp (I, It);
                  end loop;
               end if;
            end;

         --  Domain of iteration is not overloaded

         else
            Resolve (Iter_Name, Etype (Iter_Name));
         end if;

         if not Of_Present (N) then
            Check_Reverse_Iteration (Etype (Iter_Name));
         end if;
      end if;

      --  Get base type of container, for proper retrieval of Cursor type
      --  and primitive operations.

      Typ := Base_Type (Etype (Iter_Name));

      if Is_Array_Type (Typ) then
         if Of_Present (N) then
            Set_Etype (Def_Id, Component_Type (Typ));

            --  The loop variable is aliased if the array components are
            --  aliased.

            Set_Is_Aliased (Def_Id, Has_Aliased_Components (Typ));

            --  AI12-0047 stipulates that the domain (array or container)
            --  cannot be a component that depends on a discriminant if the
            --  enclosing object is mutable, to prevent a modification of the
            --  dowmain of iteration in the course of an iteration.

            --  If the object is an expression it has been captured in a
            --  temporary, so examine original node.

            if Nkind (Original_Node (Iter_Name)) = N_Selected_Component
              and then Is_Dependent_Component_Of_Mutable_Object
                         (Original_Node (Iter_Name))
            then
               Error_Msg_N
                 ("iterable name cannot be a discriminant-dependent "
                  & "component of a mutable object", N);
            end if;

            Check_Subtype_Indication (Component_Type (Typ));

         --  Here we have a missing Range attribute

         else
            Error_Msg_N
              ("missing Range attribute in iteration over an array", N);

            --  In Ada 2012 mode, this may be an attempt at an iterator

            if Ada_Version >= Ada_2012 then
               Error_Msg_NE
                 ("\if& is meant to designate an element of the array, use OF",
                  N, Def_Id);
            end if;

            --  Prevent cascaded errors

            Set_Ekind (Def_Id, E_Loop_Parameter);
            Set_Etype (Def_Id, Etype (First_Index (Typ)));
         end if;

         --  Check for type error in iterator

      elsif Typ = Any_Type then
         return;

      --  Iteration over a container

      else
         Set_Ekind (Def_Id, E_Loop_Parameter);
         Error_Msg_Ada_2012_Feature ("container iterator", Sloc (N));

         --  OF present

         if Of_Present (N) then
            if Has_Aspect (Typ, Aspect_Iterable) then
               declare
                  Elt : constant Entity_Id :=
                          Get_Iterable_Type_Primitive (Typ, Name_Element);
               begin
                  if No (Elt) then
                     Error_Msg_N
                       ("missing Element primitive for iteration", N);
                  else
                     Set_Etype (Def_Id, Etype (Elt));
                     Check_Reverse_Iteration (Typ);
                  end if;
               end;

               Check_Subtype_Indication (Etype (Def_Id));

            --  For a predefined container, The type of the loop variable is
            --  the Iterator_Element aspect of the container type.

            else
               declare
                  Element        : constant Entity_Id :=
                                     Find_Value_Of_Aspect
                                       (Typ, Aspect_Iterator_Element);
                  Iterator       : constant Entity_Id :=
                                     Find_Value_Of_Aspect
                                       (Typ, Aspect_Default_Iterator);
                  Orig_Iter_Name : constant Node_Id :=
                                     Original_Node (Iter_Name);
                  Cursor_Type    : Entity_Id;

               begin
                  if No (Element) then
                     Error_Msg_NE ("cannot iterate over&", N, Typ);
                     return;

                  else
                     Set_Etype (Def_Id, Entity (Element));
                     Cursor_Type := Get_Cursor_Type (Typ);
                     pragma Assert (Present (Cursor_Type));

                     Check_Subtype_Indication (Etype (Def_Id));

                     --  If the container has a variable indexing aspect, the
                     --  element is a variable and is modifiable in the loop.

                     if Has_Aspect (Typ, Aspect_Variable_Indexing) then
                        Set_Ekind (Def_Id, E_Variable);
                     end if;

                     --  If the container is a constant, iterating over it
                     --  requires a Constant_Indexing operation.

                     if not Is_Variable (Iter_Name)
                       and then not Has_Aspect (Typ, Aspect_Constant_Indexing)
                     then
                        Error_Msg_N
                          ("iteration over constant container require "
                           & "constant_indexing aspect", N);

                     --  The Iterate function may have an in_out parameter,
                     --  and a constant container is thus illegal.

                     elsif Present (Iterator)
                       and then Ekind (Entity (Iterator)) = E_Function
                       and then Ekind (First_Formal (Entity (Iterator))) /=
                                  E_In_Parameter
                       and then not Is_Variable (Iter_Name)
                     then
                        Error_Msg_N ("variable container expected", N);
                     end if;

                     --  Detect a case where the iterator denotes a component
                     --  of a mutable object which depends on a discriminant.
                     --  Note that the iterator may denote a function call in
                     --  qualified form, in which case this check should not
                     --  be performed.

                     if Nkind (Orig_Iter_Name) = N_Selected_Component
                       and then
                         Present (Entity (Selector_Name (Orig_Iter_Name)))
                       and then Ekind_In
                                  (Entity (Selector_Name (Orig_Iter_Name)),
                                   E_Component,
                                   E_Discriminant)
                       and then Is_Dependent_Component_Of_Mutable_Object
                                  (Orig_Iter_Name)
                     then
                        Error_Msg_N
                          ("container cannot be a discriminant-dependent "
                           & "component of a mutable object", N);
                     end if;
                  end if;
               end;
            end if;

         --  IN iterator, domain is a range, or a call to Iterate function

         else
            --  For an iteration of the form IN, the name must denote an
            --  iterator, typically the result of a call to Iterate. Give a
            --  useful error message when the name is a container by itself.

            --  The type may be a formal container type, which has to have
            --  an Iterable aspect detailing the required primitives.

            if Is_Entity_Name (Original_Node (Name (N)))
              and then not Is_Iterator (Typ)
            then
               if Has_Aspect (Typ, Aspect_Iterable) then
                  null;

               elsif not Has_Aspect (Typ, Aspect_Iterator_Element) then
                  Error_Msg_NE
                    ("cannot iterate over&", Name (N), Typ);
               else
                  Error_Msg_N
                    ("name must be an iterator, not a container", Name (N));
               end if;

               if Has_Aspect (Typ, Aspect_Iterable) then
                  null;
               else
                  Error_Msg_NE
                    ("\to iterate directly over the elements of a container, "
                     & "write `of &`", Name (N), Original_Node (Name (N)));

                  --  No point in continuing analysis of iterator spec

                  return;
               end if;
            end if;

            --  If the name is a call (typically prefixed) to some Iterate
            --  function, it has been rewritten as an object declaration.
            --  If that object is a selected component, verify that it is not
            --  a component of an unconstrained mutable object.

            if Nkind (Iter_Name) = N_Identifier
              or else (not Expander_Active and Comes_From_Source (Iter_Name))
            then
               declare
                  Orig_Node : constant Node_Id   := Original_Node (Iter_Name);
                  Iter_Kind : constant Node_Kind := Nkind (Orig_Node);
                  Obj       : Node_Id;

               begin
                  if Iter_Kind = N_Selected_Component then
                     Obj  := Prefix (Orig_Node);

                  elsif Iter_Kind = N_Function_Call then
                     Obj  := First_Actual (Orig_Node);

                  --  If neither, the name comes from source

                  else
                     Obj := Iter_Name;
                  end if;

                  if Nkind (Obj) = N_Selected_Component
                    and then Is_Dependent_Component_Of_Mutable_Object (Obj)
                  then
                     Error_Msg_N
                       ("container cannot be a discriminant-dependent "
                        & "component of a mutable object", N);
                  end if;
               end;
            end if;

            --  The result type of Iterate function is the classwide type of
            --  the interface parent. We need the specific Cursor type defined
            --  in the container package. We obtain it by name for a predefined
            --  container, or through the Iterable aspect for a formal one.

            if Has_Aspect (Typ, Aspect_Iterable) then
               Set_Etype (Def_Id,
                 Get_Cursor_Type
                   (Parent (Find_Value_Of_Aspect (Typ, Aspect_Iterable)),
                    Typ));

            else
               Set_Etype (Def_Id, Get_Cursor_Type (Typ));
               Check_Reverse_Iteration (Etype (Iter_Name));
            end if;

         end if;
      end if;
   end Analyze_Iterator_Specification;

   -------------------
   -- Analyze_Label --
   -------------------

   --  Note: the semantic work required for analyzing labels (setting them as
   --  reachable) was done in a prepass through the statements in the block,
   --  so that forward gotos would be properly handled. See Analyze_Statements
   --  for further details. The only processing required here is to deal with
   --  optimizations that depend on an assumption of sequential control flow,
   --  since of course the occurrence of a label breaks this assumption.

   procedure Analyze_Label (N : Node_Id) is
      pragma Warnings (Off, N);
   begin
      Kill_Current_Values;
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

   ------------------------------------------
   -- Analyze_Loop_Parameter_Specification --
   ------------------------------------------

   procedure Analyze_Loop_Parameter_Specification (N : Node_Id) is
      Loop_Nod : constant Node_Id := Parent (Parent (N));

      procedure Check_Controlled_Array_Attribute (DS : Node_Id);
      --  If the bounds are given by a 'Range reference on a function call
      --  that returns a controlled array, introduce an explicit declaration
      --  to capture the bounds, so that the function result can be finalized
      --  in timely fashion.

      procedure Check_Predicate_Use (T : Entity_Id);
      --  Diagnose Attempt to iterate through non-static predicate. Note that
      --  a type with inherited predicates may have both static and dynamic
      --  forms. In this case it is not sufficent to check the static predicate
      --  function only, look for a dynamic predicate aspect as well.

      procedure Process_Bounds (R : Node_Id);
      --  If the iteration is given by a range, create temporaries and
      --  assignment statements block to capture the bounds and perform
      --  required finalization actions in case a bound includes a function
      --  call that uses the temporary stack. We first preanalyze a copy of
      --  the range in order to determine the expected type, and analyze and
      --  resolve the original bounds.

      --------------------------------------
      -- Check_Controlled_Array_Attribute --
      --------------------------------------

      procedure Check_Controlled_Array_Attribute (DS : Node_Id) is
      begin
         if Nkind (DS) = N_Attribute_Reference
           and then Is_Entity_Name (Prefix (DS))
           and then Ekind (Entity (Prefix (DS))) = E_Function
           and then Is_Array_Type (Etype (Entity (Prefix (DS))))
           and then
             Is_Controlled (Component_Type (Etype (Entity (Prefix (DS)))))
           and then Expander_Active
         then
            declare
               Loc  : constant Source_Ptr := Sloc (N);
               Arr  : constant Entity_Id := Etype (Entity (Prefix (DS)));
               Indx : constant Entity_Id :=
                        Base_Type (Etype (First_Index (Arr)));
               Subt : constant Entity_Id := Make_Temporary (Loc, 'S');
               Decl : Node_Id;

            begin
               Decl :=
                 Make_Subtype_Declaration (Loc,
                   Defining_Identifier => Subt,
                   Subtype_Indication  =>
                      Make_Subtype_Indication (Loc,
                        Subtype_Mark => New_Occurrence_Of (Indx, Loc),
                        Constraint   =>
                          Make_Range_Constraint (Loc, Relocate_Node (DS))));
               Insert_Before (Loop_Nod, Decl);
               Analyze (Decl);

               Rewrite (DS,
                 Make_Attribute_Reference (Loc,
                   Prefix         => New_Occurrence_Of (Subt, Loc),
                   Attribute_Name => Attribute_Name (DS)));

               Analyze (DS);
            end;
         end if;
      end Check_Controlled_Array_Attribute;

      -------------------------
      -- Check_Predicate_Use --
      -------------------------

      procedure Check_Predicate_Use (T : Entity_Id) is
      begin
         --  A predicated subtype is illegal in loops and related constructs
         --  if the predicate is not static, or if it is a non-static subtype
         --  of a statically predicated subtype.

         if Is_Discrete_Type (T)
           and then Has_Predicates (T)
           and then (not Has_Static_Predicate (T)
                      or else not Is_Static_Subtype (T)
                      or else Has_Dynamic_Predicate_Aspect (T))
         then
            --  Seems a confusing message for the case of a static predicate
            --  with a non-static subtype???

            Bad_Predicated_Subtype_Use
              ("cannot use subtype& with non-static predicate for loop "
               & "iteration", Discrete_Subtype_Definition (N),
               T, Suggest_Static => True);

         elsif Inside_A_Generic
           and then Is_Generic_Formal (T)
           and then Is_Discrete_Type (T)
         then
            Set_No_Dynamic_Predicate_On_Actual (T);
         end if;
      end Check_Predicate_Use;

      --------------------
      -- Process_Bounds --
      --------------------

      procedure Process_Bounds (R : Node_Id) is
         Loc : constant Source_Ptr := Sloc (N);

         function One_Bound
           (Original_Bound : Node_Id;
            Analyzed_Bound : Node_Id;
            Typ            : Entity_Id) return Node_Id;
         --  Capture value of bound and return captured value

         ---------------
         -- One_Bound --
         ---------------

         function One_Bound
           (Original_Bound : Node_Id;
            Analyzed_Bound : Node_Id;
            Typ            : Entity_Id) return Node_Id
         is
            Assign : Node_Id;
            Decl   : Node_Id;
            Id     : Entity_Id;

         begin
            --  If the bound is a constant or an object, no need for a separate
            --  declaration. If the bound is the result of previous expansion
            --  it is already analyzed and should not be modified. Note that
            --  the Bound will be resolved later, if needed, as part of the
            --  call to Make_Index (literal bounds may need to be resolved to
            --  type Integer).

            if Analyzed (Original_Bound) then
               return Original_Bound;

            elsif Nkind_In (Analyzed_Bound, N_Integer_Literal,
                                            N_Character_Literal)
              or else Is_Entity_Name (Analyzed_Bound)
            then
               Analyze_And_Resolve (Original_Bound, Typ);
               return Original_Bound;
            end if;

            --  Normally, the best approach is simply to generate a constant
            --  declaration that captures the bound. However, there is a nasty
            --  case where this is wrong. If the bound is complex, and has a
            --  possible use of the secondary stack, we need to generate a
            --  separate assignment statement to ensure the creation of a block
            --  which will release the secondary stack.

            --  We prefer the constant declaration, since it leaves us with a
            --  proper trace of the value, useful in optimizations that get rid
            --  of junk range checks.

            if not Has_Sec_Stack_Call (Analyzed_Bound) then
               Analyze_And_Resolve (Original_Bound, Typ);

               --  Ensure that the bound is valid. This check should not be
               --  generated when the range belongs to a quantified expression
               --  as the construct is still not expanded into its final form.

               if Nkind (Parent (R)) /= N_Loop_Parameter_Specification
                 or else Nkind (Parent (Parent (R))) /= N_Quantified_Expression
               then
                  Ensure_Valid (Original_Bound);
               end if;

               Force_Evaluation (Original_Bound);
               return Original_Bound;
            end if;

            Id := Make_Temporary (Loc, 'R', Original_Bound);

            --  Here we make a declaration with a separate assignment
            --  statement, and insert before loop header.

            Decl :=
              Make_Object_Declaration (Loc,
                Defining_Identifier => Id,
                Object_Definition   => New_Occurrence_Of (Typ, Loc));

            Assign :=
              Make_Assignment_Statement (Loc,
                Name        => New_Occurrence_Of (Id, Loc),
                Expression  => Relocate_Node (Original_Bound));

            Insert_Actions (Loop_Nod, New_List (Decl, Assign));

            --  Now that this temporary variable is initialized we decorate it
            --  as safe-to-reevaluate to inform to the backend that no further
            --  asignment will be issued and hence it can be handled as side
            --  effect free. Note that this decoration must be done when the
            --  assignment has been analyzed because otherwise it will be
            --  rejected (see Analyze_Assignment).

            Set_Is_Safe_To_Reevaluate (Id);

            Rewrite (Original_Bound, New_Occurrence_Of (Id, Loc));

            if Nkind (Assign) = N_Assignment_Statement then
               return Expression (Assign);
            else
               return Original_Bound;
            end if;
         end One_Bound;

         Hi     : constant Node_Id := High_Bound (R);
         Lo     : constant Node_Id := Low_Bound  (R);
         R_Copy : constant Node_Id := New_Copy_Tree (R);
         New_Hi : Node_Id;
         New_Lo : Node_Id;
         Typ    : Entity_Id;

      --  Start of processing for Process_Bounds

      begin
         Set_Parent (R_Copy, Parent (R));
         Preanalyze_Range (R_Copy);
         Typ := Etype (R_Copy);

         --  If the type of the discrete range is Universal_Integer, then the
         --  bound's type must be resolved to Integer, and any object used to
         --  hold the bound must also have type Integer, unless the literal
         --  bounds are constant-folded expressions with a user-defined type.

         if Typ = Universal_Integer then
            if Nkind (Lo) = N_Integer_Literal
              and then Present (Etype (Lo))
              and then Scope (Etype (Lo)) /= Standard_Standard
            then
               Typ := Etype (Lo);

            elsif Nkind (Hi) = N_Integer_Literal
              and then Present (Etype (Hi))
              and then Scope (Etype (Hi)) /= Standard_Standard
            then
               Typ := Etype (Hi);

            else
               Typ := Standard_Integer;
            end if;
         end if;

         Set_Etype (R, Typ);

         New_Lo := One_Bound (Lo, Low_Bound  (R_Copy), Typ);
         New_Hi := One_Bound (Hi, High_Bound (R_Copy), Typ);

         --  Propagate staticness to loop range itself, in case the
         --  corresponding subtype is static.

         if New_Lo /= Lo and then Is_OK_Static_Expression (New_Lo) then
            Rewrite (Low_Bound (R), New_Copy (New_Lo));
         end if;

         if New_Hi /= Hi and then Is_OK_Static_Expression (New_Hi) then
            Rewrite (High_Bound (R), New_Copy (New_Hi));
         end if;
      end Process_Bounds;

      --  Local variables

      DS : constant Node_Id   := Discrete_Subtype_Definition (N);
      Id : constant Entity_Id := Defining_Identifier (N);

      DS_Copy : Node_Id;

   --  Start of processing for Analyze_Loop_Parameter_Specification

   begin
      Enter_Name (Id);

      --  We always consider the loop variable to be referenced, since the loop
      --  may be used just for counting purposes.

      Generate_Reference (Id, N, ' ');

      --  Check for the case of loop variable hiding a local variable (used
      --  later on to give a nice warning if the hidden variable is never
      --  assigned).

      declare
         H : constant Entity_Id := Homonym (Id);
      begin
         if Present (H)
           and then Ekind (H) = E_Variable
           and then Is_Discrete_Type (Etype (H))
           and then Enclosing_Dynamic_Scope (H) = Enclosing_Dynamic_Scope (Id)
         then
            Set_Hiding_Loop_Variable (H, Id);
         end if;
      end;

      --  Loop parameter specification must include subtype mark in SPARK

      if Nkind (DS) = N_Range then
         Check_SPARK_05_Restriction
           ("loop parameter specification must include subtype mark", N);
      end if;

      --  Analyze the subtype definition and create temporaries for the bounds.
      --  Do not evaluate the range when preanalyzing a quantified expression
      --  because bounds expressed as function calls with side effects will be
      --  incorrectly replicated.

      if Nkind (DS) = N_Range
        and then Expander_Active
        and then Nkind (Parent (N)) /= N_Quantified_Expression
      then
         Process_Bounds (DS);

      --  Either the expander not active or the range of iteration is a subtype
      --  indication, an entity, or a function call that yields an aggregate or
      --  a container.

      else
         DS_Copy := New_Copy_Tree (DS);
         Set_Parent (DS_Copy, Parent (DS));
         Preanalyze_Range (DS_Copy);

         --  Ada 2012: If the domain of iteration is:

         --  a)  a function call,
         --  b)  an identifier that is not a type,
         --  c)  an attribute reference 'Old (within a postcondition),
         --  d)  an unchecked conversion or a qualified expression with
         --      the proper iterator type.

         --  then it is an iteration over a container. It was classified as
         --  a loop specification by the parser, and must be rewritten now
         --  to activate container iteration. The last case will occur within
         --  an expanded inlined call, where the expansion wraps an actual in
         --  an unchecked conversion when needed. The expression of the
         --  conversion is always an object.

         if Nkind (DS_Copy) = N_Function_Call

           or else (Is_Entity_Name (DS_Copy)
                     and then not Is_Type (Entity (DS_Copy)))

           or else (Nkind (DS_Copy) = N_Attribute_Reference
                     and then Nam_In (Attribute_Name (DS_Copy),
                                      Name_Loop_Entry, Name_Old))

           or else Has_Aspect (Etype (DS_Copy), Aspect_Iterable)

           or else Nkind (DS_Copy) = N_Unchecked_Type_Conversion
           or else (Nkind (DS_Copy) = N_Qualified_Expression
                     and then Is_Iterator (Etype (DS_Copy)))
         then
            --  This is an iterator specification. Rewrite it as such and
            --  analyze it to capture function calls that may require
            --  finalization actions.

            declare
               I_Spec : constant Node_Id :=
                          Make_Iterator_Specification (Sloc (N),
                            Defining_Identifier => Relocate_Node (Id),
                            Name                => DS_Copy,
                            Subtype_Indication  => Empty,
                            Reverse_Present     => Reverse_Present (N));
               Scheme : constant Node_Id := Parent (N);

            begin
               Set_Iterator_Specification (Scheme, I_Spec);
               Set_Loop_Parameter_Specification (Scheme, Empty);
               Analyze_Iterator_Specification (I_Spec);

               --  In a generic context, analyze the original domain of
               --  iteration, for name capture.

               if not Expander_Active then
                  Analyze (DS);
               end if;

               --  Set kind of loop parameter, which may be used in the
               --  subsequent analysis of the condition in a quantified
               --  expression.

               Set_Ekind (Id, E_Loop_Parameter);
               return;
            end;

         --  Domain of iteration is not a function call, and is side-effect
         --  free.

         else
            --  A quantified expression that appears in a pre/post condition
            --  is preanalyzed several times.  If the range is given by an
            --  attribute reference it is rewritten as a range, and this is
            --  done even with expansion disabled. If the type is already set
            --  do not reanalyze, because a range with static bounds may be
            --  typed Integer by default.

            if Nkind (Parent (N)) = N_Quantified_Expression
              and then Present (Etype (DS))
            then
               null;
            else
               Analyze (DS);
            end if;
         end if;
      end if;

      if DS = Error then
         return;
      end if;

      --  Some additional checks if we are iterating through a type

      if Is_Entity_Name (DS)
        and then Present (Entity (DS))
        and then Is_Type (Entity (DS))
      then
         --  The subtype indication may denote the completion of an incomplete
         --  type declaration.

         if Ekind (Entity (DS)) = E_Incomplete_Type then
            Set_Entity (DS, Get_Full_View (Entity (DS)));
            Set_Etype  (DS, Entity (DS));
         end if;

         Check_Predicate_Use (Entity (DS));
      end if;

      --  Error if not discrete type

      if not Is_Discrete_Type (Etype (DS)) then
         Wrong_Type (DS, Any_Discrete);
         Set_Etype (DS, Any_Type);
      end if;

      Check_Controlled_Array_Attribute (DS);

      if Nkind (DS) = N_Subtype_Indication then
         Check_Predicate_Use (Entity (Subtype_Mark (DS)));
      end if;

      Make_Index (DS, N, In_Iter_Schm => True);
      Set_Ekind (Id, E_Loop_Parameter);

      --  A quantified expression which appears in a pre- or post-condition may
      --  be analyzed multiple times. The analysis of the range creates several
      --  itypes which reside in different scopes depending on whether the pre-
      --  or post-condition has been expanded. Update the type of the loop
      --  variable to reflect the proper itype at each stage of analysis.

      if No (Etype (Id))
        or else Etype (Id) = Any_Type
        or else
          (Present (Etype (Id))
            and then Is_Itype (Etype (Id))
            and then Nkind (Parent (Loop_Nod)) = N_Expression_With_Actions
            and then Nkind (Original_Node (Parent (Loop_Nod))) =
                                                   N_Quantified_Expression)
      then
         Set_Etype (Id, Etype (DS));
      end if;

      --  Treat a range as an implicit reference to the type, to inhibit
      --  spurious warnings.

      Generate_Reference (Base_Type (Etype (DS)), N, ' ');
      Set_Is_Known_Valid (Id, True);

      --  The loop is not a declarative part, so the loop variable must be
      --  frozen explicitly. Do not freeze while preanalyzing a quantified
      --  expression because the freeze node will not be inserted into the
      --  tree due to flag Is_Spec_Expression being set.

      if Nkind (Parent (N)) /= N_Quantified_Expression then
         declare
            Flist : constant List_Id := Freeze_Entity (Id, N);
         begin
            if Is_Non_Empty_List (Flist) then
               Insert_Actions (N, Flist);
            end if;
         end;
      end if;

      --  Case where we have a range or a subtype, get type bounds

      if Nkind_In (DS, N_Range, N_Subtype_Indication)
        and then not Error_Posted (DS)
        and then Etype (DS) /= Any_Type
        and then Is_Discrete_Type (Etype (DS))
      then
         declare
            L : Node_Id;
            H : Node_Id;

         begin
            if Nkind (DS) = N_Range then
               L := Low_Bound  (DS);
               H := High_Bound (DS);
            else
               L :=
                 Type_Low_Bound  (Underlying_Type (Etype (Subtype_Mark (DS))));
               H :=
                 Type_High_Bound (Underlying_Type (Etype (Subtype_Mark (DS))));
            end if;

            --  Check for null or possibly null range and issue warning. We
            --  suppress such messages in generic templates and instances,
            --  because in practice they tend to be dubious in these cases. The
            --  check applies as well to rewritten array element loops where a
            --  null range may be detected statically.

            if Compile_Time_Compare (L, H, Assume_Valid => True) = GT then

               --  Suppress the warning if inside a generic template or
               --  instance, since in practice they tend to be dubious in these
               --  cases since they can result from intended parameterization.

               if not Inside_A_Generic and then not In_Instance then

                  --  Specialize msg if invalid values could make the loop
                  --  non-null after all.

                  if Compile_Time_Compare
                       (L, H, Assume_Valid => False) = GT
                  then
                     --  Since we know the range of the loop is null, set the
                     --  appropriate flag to remove the loop entirely during
                     --  expansion.

                     Set_Is_Null_Loop (Loop_Nod);

                     if Comes_From_Source (N) then
                        Error_Msg_N
                          ("??loop range is null, loop will not execute", DS);
                     end if;

                     --  Here is where the loop could execute because of
                     --  invalid values, so issue appropriate message and in
                     --  this case we do not set the Is_Null_Loop flag since
                     --  the loop may execute.

                  elsif Comes_From_Source (N) then
                     Error_Msg_N
                       ("??loop range may be null, loop may not execute",
                        DS);
                     Error_Msg_N
                       ("??can only execute if invalid values are present",
                        DS);
                  end if;
               end if;

               --  In either case, suppress warnings in the body of the loop,
               --  since it is likely that these warnings will be inappropriate
               --  if the loop never actually executes, which is likely.

               Set_Suppress_Loop_Warnings (Loop_Nod);

               --  The other case for a warning is a reverse loop where the
               --  upper bound is the integer literal zero or one, and the
               --  lower bound may exceed this value.

               --  For example, we have

               --     for J in reverse N .. 1 loop

               --  In practice, this is very likely to be a case of reversing
               --  the bounds incorrectly in the range.

            elsif Reverse_Present (N)
              and then Nkind (Original_Node (H)) = N_Integer_Literal
              and then
                (Intval (Original_Node (H)) = Uint_0
                  or else
                 Intval (Original_Node (H)) = Uint_1)
            then
               --  Lower bound may in fact be known and known not to exceed
               --  upper bound (e.g. reverse 0 .. 1) and that's OK.

               if Compile_Time_Known_Value (L)
                 and then Expr_Value (L) <= Expr_Value (H)
               then
                  null;

               --  Otherwise warning is warranted

               else
                  Error_Msg_N ("??loop range may be null", DS);
                  Error_Msg_N ("\??bounds may be wrong way round", DS);
               end if;
            end if;

            --  Check if either bound is known to be outside the range of the
            --  loop parameter type, this is e.g. the case of a loop from
            --  20..X where the type is 1..19.

            --  Such a loop is dubious since either it raises CE or it executes
            --  zero times, and that cannot be useful!

            if Etype (DS) /= Any_Type
              and then not Error_Posted (DS)
              and then Nkind (DS) = N_Subtype_Indication
              and then Nkind (Constraint (DS)) = N_Range_Constraint
            then
               declare
                  LLo : constant Node_Id :=
                          Low_Bound  (Range_Expression (Constraint (DS)));
                  LHi : constant Node_Id :=
                          High_Bound (Range_Expression (Constraint (DS)));

                  Bad_Bound : Node_Id := Empty;
                  --  Suspicious loop bound

               begin
                  --  At this stage L, H are the bounds of the type, and LLo
                  --  Lhi are the low bound and high bound of the loop.

                  if Compile_Time_Compare (LLo, L, Assume_Valid => True) = LT
                       or else
                     Compile_Time_Compare (LLo, H, Assume_Valid => True) = GT
                  then
                     Bad_Bound := LLo;
                  end if;

                  if Compile_Time_Compare (LHi, L, Assume_Valid => True) = LT
                       or else
                     Compile_Time_Compare (LHi, H, Assume_Valid => True) = GT
                  then
                     Bad_Bound := LHi;
                  end if;

                  if Present (Bad_Bound) then
                     Error_Msg_N
                       ("suspicious loop bound out of range of "
                        & "loop subtype??", Bad_Bound);
                     Error_Msg_N
                       ("\loop executes zero times or raises "
                        & "Constraint_Error??", Bad_Bound);
                  end if;
               end;
            end if;

         --  This declare block is about warnings, if we get an exception while
         --  testing for warnings, we simply abandon the attempt silently. This
         --  most likely occurs as the result of a previous error, but might
         --  just be an obscure case we have missed. In either case, not giving
         --  the warning is perfectly acceptable.

         exception
            when others => null;
         end;
      end if;

      --  A loop parameter cannot be effectively volatile (SPARK RM 7.1.3(4)).
      --  This check is relevant only when SPARK_Mode is on as it is not a
      --  standard Ada legality check.

      if SPARK_Mode = On and then Is_Effectively_Volatile (Id) then
         Error_Msg_N ("loop parameter cannot be volatile", Id);
      end if;
   end Analyze_Loop_Parameter_Specification;

   ----------------------------
   -- Analyze_Loop_Statement --
   ----------------------------

   procedure Analyze_Loop_Statement (N : Node_Id) is

      --  The following exception is raised by routine Prepare_Loop_Statement
      --  to avoid further analysis of a transformed loop.

      function Disable_Constant (N : Node_Id) return Traverse_Result;
      --  If N represents an E_Variable entity, set Is_True_Constant To False

      procedure Disable_Constants is new Traverse_Proc (Disable_Constant);
      --  Helper for Analyze_Loop_Statement, to unset Is_True_Constant on
      --  variables referenced within an OpenACC construct.

      procedure Prepare_Loop_Statement
        (Iter            : Node_Id;
         Stop_Processing : out Boolean);
      --  Determine whether loop statement N with iteration scheme Iter must be
      --  transformed prior to analysis, and if so, perform it.
      --  If Stop_Processing is set to True, should stop further processing.

      ----------------------
      -- Disable_Constant --
      ----------------------

      function Disable_Constant (N : Node_Id) return Traverse_Result is
      begin
         if Is_Entity_Name (N)
            and then Present (Entity (N))
            and then Ekind (Entity (N)) = E_Variable
         then
            Set_Is_True_Constant (Entity (N), False);
         end if;

         return OK;
      end Disable_Constant;

      ----------------------------
      -- Prepare_Loop_Statement --
      ----------------------------

      procedure Prepare_Loop_Statement
        (Iter            : Node_Id;
         Stop_Processing : out Boolean)
      is
         function Has_Sec_Stack_Default_Iterator
           (Cont_Typ : Entity_Id) return Boolean;
         pragma Inline (Has_Sec_Stack_Default_Iterator);
         --  Determine whether container type Cont_Typ has a default iterator
         --  that requires secondary stack management.

         function Is_Sec_Stack_Iteration_Primitive
           (Cont_Typ      : Entity_Id;
            Iter_Prim_Nam : Name_Id) return Boolean;
         pragma Inline (Is_Sec_Stack_Iteration_Primitive);
         --  Determine whether container type Cont_Typ has an iteration routine
         --  described by its name Iter_Prim_Nam that requires secondary stack
         --  management.

         function Is_Wrapped_In_Block (Stmt : Node_Id) return Boolean;
         pragma Inline (Is_Wrapped_In_Block);
         --  Determine whether arbitrary statement Stmt is the sole statement
         --  wrapped within some block, excluding pragmas.

         procedure Prepare_Iterator_Loop
           (Iter_Spec       : Node_Id;
            Stop_Processing : out Boolean);
         pragma Inline (Prepare_Iterator_Loop);
         --  Prepare an iterator loop with iteration specification Iter_Spec
         --  for transformation if needed.
         --  If Stop_Processing is set to True, should stop further processing.

         procedure Prepare_Param_Spec_Loop
           (Param_Spec      : Node_Id;
            Stop_Processing : out Boolean);
         pragma Inline (Prepare_Param_Spec_Loop);
         --  Prepare a discrete loop with parameter specification Param_Spec
         --  for transformation if needed.
         --  If Stop_Processing is set to True, should stop further processing.

         procedure Wrap_Loop_Statement (Manage_Sec_Stack : Boolean);
         pragma Inline (Wrap_Loop_Statement);
         --  Wrap loop statement N within a block. Flag Manage_Sec_Stack must
         --  be set when the block must mark and release the secondary stack.
         --  Should stop further processing after calling this procedure.

         ------------------------------------
         -- Has_Sec_Stack_Default_Iterator --
         ------------------------------------

         function Has_Sec_Stack_Default_Iterator
           (Cont_Typ : Entity_Id) return Boolean
         is
            Def_Iter : constant Node_Id :=
                         Find_Value_Of_Aspect
                           (Cont_Typ, Aspect_Default_Iterator);
         begin
            return
              Present (Def_Iter)
                and then Requires_Transient_Scope (Etype (Def_Iter));
         end Has_Sec_Stack_Default_Iterator;

         --------------------------------------
         -- Is_Sec_Stack_Iteration_Primitive --
         --------------------------------------

         function Is_Sec_Stack_Iteration_Primitive
           (Cont_Typ      : Entity_Id;
            Iter_Prim_Nam : Name_Id) return Boolean
         is
            Iter_Prim : constant Entity_Id :=
                          Get_Iterable_Type_Primitive
                            (Cont_Typ, Iter_Prim_Nam);
         begin
            return
              Present (Iter_Prim)
                and then Requires_Transient_Scope (Etype (Iter_Prim));
         end Is_Sec_Stack_Iteration_Primitive;

         -------------------------
         -- Is_Wrapped_In_Block --
         -------------------------

         function Is_Wrapped_In_Block (Stmt : Node_Id) return Boolean is
            Blk_HSS  : Node_Id;
            Blk_Id   : Entity_Id;
            Blk_Stmt : Node_Id;

         begin
            Blk_Id := Current_Scope;

            --  The current context is a block. Inspect the statements of the
            --  block to determine whether it wraps Stmt.

            if Ekind (Blk_Id) = E_Block
              and then Present (Block_Node (Blk_Id))
            then
               Blk_HSS :=
                 Handled_Statement_Sequence (Parent (Block_Node (Blk_Id)));

               --  Skip leading pragmas introduced for invariant and predicate
               --  checks.

               Blk_Stmt := First (Statements (Blk_HSS));
               while Present (Blk_Stmt)
                 and then Nkind (Blk_Stmt) = N_Pragma
               loop
                  Next (Blk_Stmt);
               end loop;

               return Blk_Stmt = Stmt and then No (Next (Blk_Stmt));
            end if;

            return False;
         end Is_Wrapped_In_Block;

         ---------------------------
         -- Prepare_Iterator_Loop --
         ---------------------------

         procedure Prepare_Iterator_Loop
           (Iter_Spec       : Node_Id;
            Stop_Processing : out Boolean)
         is
            Cont_Typ : Entity_Id;
            Nam      : Node_Id;
            Nam_Copy : Node_Id;

         begin
            Stop_Processing := False;

            --  The iterator specification has syntactic errors. Transform the
            --  loop into an infinite loop in order to safely perform at least
            --  some minor analysis. This check must come first.

            if Error_Posted (Iter_Spec) then
               Set_Iteration_Scheme (N, Empty);
               Analyze (N);
               Stop_Processing := True;

            --  Nothing to do when the loop is already wrapped in a block

            elsif Is_Wrapped_In_Block (N) then
               null;

            --  Otherwise the iterator loop traverses an array or a container
            --  and appears in the form
            --
            --    for Def_Id in [reverse] Iterator_Name loop
            --    for Def_Id [: Subtyp_Indic] of [reverse] Iterable_Name loop

            else
               --  Prepare a copy of the iterated name for preanalysis. The
               --  copy is semi inserted into the tree by setting its Parent
               --  pointer.

               Nam      := Name (Iter_Spec);
               Nam_Copy := New_Copy_Tree (Nam);
               Set_Parent (Nam_Copy, Parent (Nam));

               --  Determine what the loop is iterating on

               Preanalyze_Range (Nam_Copy);
               Cont_Typ := Etype (Nam_Copy);

               --  The iterator loop is traversing an array. This case does not
               --  require any transformation.

               if Is_Array_Type (Cont_Typ) then
                  null;

               --  Otherwise unconditionally wrap the loop statement within
               --  a block. The expansion of iterator loops may relocate the
               --  iterator outside the loop, thus "leaking" its entity into
               --  the enclosing scope. Wrapping the loop statement allows
               --  for multiple iterator loops using the same iterator name
               --  to coexist within the same scope.
               --
               --  The block must manage the secondary stack when the iterator
               --  loop is traversing a container using either
               --
               --    * A default iterator obtained on the secondary stack
               --
               --    * Call to Iterate where the iterator is returned on the
               --      secondary stack.
               --
               --    * Combination of First, Next, and Has_Element where the
               --      first two return a cursor on the secondary stack.

               else
                  Wrap_Loop_Statement
                    (Manage_Sec_Stack =>
                       Has_Sec_Stack_Default_Iterator (Cont_Typ)
                         or else Has_Sec_Stack_Call (Nam_Copy)
                         or else Is_Sec_Stack_Iteration_Primitive
                                   (Cont_Typ, Name_First)
                         or else Is_Sec_Stack_Iteration_Primitive
                                   (Cont_Typ, Name_Next));
                  Stop_Processing := True;
               end if;
            end if;
         end Prepare_Iterator_Loop;

         -----------------------------
         -- Prepare_Param_Spec_Loop --
         -----------------------------

         procedure Prepare_Param_Spec_Loop
           (Param_Spec      : Node_Id;
            Stop_Processing : out Boolean)
         is
            High     : Node_Id;
            Low      : Node_Id;
            Rng      : Node_Id;
            Rng_Copy : Node_Id;
            Rng_Typ  : Entity_Id;

         begin
            Stop_Processing := False;
            Rng := Discrete_Subtype_Definition (Param_Spec);

            --  Nothing to do when the loop is already wrapped in a block

            if Is_Wrapped_In_Block (N) then
               null;

            --  The parameter specification appears in the form
            --
            --    for Def_Id in Subtype_Mark Constraint loop

            elsif Nkind (Rng) = N_Subtype_Indication
              and then Nkind (Range_Expression (Constraint (Rng))) = N_Range
            then
               Rng := Range_Expression (Constraint (Rng));

               --  Preanalyze the bounds of the range constraint

               Low  := New_Copy_Tree (Low_Bound  (Rng));
               High := New_Copy_Tree (High_Bound (Rng));

               Preanalyze (Low);
               Preanalyze (High);

               --  The bounds contain at least one function call that returns
               --  on the secondary stack. Note that the loop must be wrapped
               --  only when such a call exists.

               if Has_Sec_Stack_Call (Low) or else Has_Sec_Stack_Call (High)
               then
                  Wrap_Loop_Statement (Manage_Sec_Stack => True);
                  Stop_Processing := True;
               end if;

            --  Otherwise the parameter specification appears in the form
            --
            --    for Def_Id in Range loop

            else
               --  Prepare a copy of the discrete range for preanalysis. The
               --  copy is semi inserted into the tree by setting its Parent
               --  pointer.

               Rng_Copy := New_Copy_Tree (Rng);
               Set_Parent (Rng_Copy, Parent (Rng));

               --  Determine what the loop is iterating on

               Preanalyze_Range (Rng_Copy);
               Rng_Typ := Etype (Rng_Copy);

               --  Wrap the loop statement within a block in order to manage
               --  the secondary stack when the discrete range is
               --
               --    * Either a Forward_Iterator or a Reverse_Iterator
               --
               --    * Function call whose return type requires finalization
               --      actions.

               --  ??? it is unclear why using Has_Sec_Stack_Call directly on
               --  the discrete range causes the freeze node of an itype to be
               --  in the wrong scope in complex assertion expressions.

               if Is_Iterator (Rng_Typ)
                 or else (Nkind (Rng_Copy) = N_Function_Call
                           and then Needs_Finalization (Rng_Typ))
               then
                  Wrap_Loop_Statement (Manage_Sec_Stack => True);
                  Stop_Processing := True;
               end if;
            end if;
         end Prepare_Param_Spec_Loop;

         -------------------------
         -- Wrap_Loop_Statement --
         -------------------------

         procedure Wrap_Loop_Statement (Manage_Sec_Stack : Boolean) is
            Loc : constant Source_Ptr := Sloc (N);

            Blk    : Node_Id;
            Blk_Id : Entity_Id;

         begin
            Blk :=
              Make_Block_Statement (Loc,
                Declarations               => New_List,
                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc,
                    Statements => New_List (Relocate_Node (N))));

            Add_Block_Identifier (Blk, Blk_Id);
            Set_Uses_Sec_Stack (Blk_Id, Manage_Sec_Stack);

            Rewrite (N, Blk);
            Analyze (N);
         end Wrap_Loop_Statement;

         --  Local variables

         Iter_Spec  : constant Node_Id := Iterator_Specification (Iter);
         Param_Spec : constant Node_Id := Loop_Parameter_Specification (Iter);

      --  Start of processing for Prepare_Loop_Statement

      begin
         Stop_Processing := False;

         if Present (Iter_Spec) then
            Prepare_Iterator_Loop (Iter_Spec, Stop_Processing);

         elsif Present (Param_Spec) then
            Prepare_Param_Spec_Loop (Param_Spec, Stop_Processing);
         end if;
      end Prepare_Loop_Statement;

      --  Local declarations

      Id   : constant Node_Id := Identifier (N);
      Iter : constant Node_Id := Iteration_Scheme (N);
      Loc  : constant Source_Ptr := Sloc (N);
      Ent  : Entity_Id;
      Stmt : Node_Id;

   --  Start of processing for Analyze_Loop_Statement

   begin
      if Present (Id) then

         --  Make name visible, e.g. for use in exit statements. Loop labels
         --  are always considered to be referenced.

         Analyze (Id);
         Ent := Entity (Id);

         --  Guard against serious error (typically, a scope mismatch when
         --  semantic analysis is requested) by creating loop entity to
         --  continue analysis.

         if No (Ent) then
            if Total_Errors_Detected /= 0 then
               Ent := New_Internal_Entity (E_Loop, Current_Scope, Loc, 'L');
            else
               raise Program_Error;
            end if;

         --  Verify that the loop name is hot hidden by an unrelated
         --  declaration in an inner scope.

         elsif Ekind (Ent) /= E_Label and then Ekind (Ent) /= E_Loop then
            Error_Msg_Sloc := Sloc (Ent);
            Error_Msg_N ("implicit label declaration for & is hidden#", Id);

            if Present (Homonym (Ent))
              and then Ekind (Homonym (Ent)) = E_Label
            then
               Set_Entity (Id, Ent);
               Set_Ekind (Ent, E_Loop);
            end if;

         else
            Generate_Reference (Ent, N, ' ');
            Generate_Definition (Ent);

            --  If we found a label, mark its type. If not, ignore it, since it
            --  means we have a conflicting declaration, which would already
            --  have been diagnosed at declaration time. Set Label_Construct
            --  of the implicit label declaration, which is not created by the
            --  parser for generic units.

            if Ekind (Ent) = E_Label then
               Set_Ekind (Ent, E_Loop);

               if Nkind (Parent (Ent)) = N_Implicit_Label_Declaration then
                  Set_Label_Construct (Parent (Ent), N);
               end if;
            end if;
         end if;

      --  Case of no identifier present. Create one and attach it to the
      --  loop statement for use as a scope and as a reference for later
      --  expansions. Indicate that the label does not come from source,
      --  and attach it to the loop statement so it is part of the tree,
      --  even without a full declaration.

      else
         Ent := New_Internal_Entity (E_Loop, Current_Scope, Loc, 'L');
         Set_Etype  (Ent, Standard_Void_Type);
         Set_Identifier (N, New_Occurrence_Of (Ent, Loc));
         Set_Parent (Ent, N);
         Set_Has_Created_Identifier (N);
      end if;

      --  Determine whether the loop statement must be transformed prior to
      --  analysis, and if so, perform it. This early modification is needed
      --  when:
      --
      --    * The loop has an erroneous iteration scheme. In this case the
      --      loop is converted into an infinite loop in order to perform
      --      minor analysis.
      --
      --    * The loop is an Ada 2012 iterator loop. In this case the loop is
      --      wrapped within a block to provide a local scope for the iterator.
      --      If the iterator specification requires the secondary stack in any
      --      way, the block is marked in order to manage it.
      --
      --    * The loop is using a parameter specification where the discrete
      --      range requires the secondary stack. In this case the loop is
      --      wrapped within a block in order to manage the secondary stack.

      if Present (Iter) then
         declare
            Stop_Processing : Boolean;
         begin
            Prepare_Loop_Statement (Iter, Stop_Processing);

            if Stop_Processing then
               return;
            end if;
         end;
      end if;

      --  Kill current values on entry to loop, since statements in the body of
      --  the loop may have been executed before the loop is entered. Similarly
      --  we kill values after the loop, since we do not know that the body of
      --  the loop was executed.

      Kill_Current_Values;
      Push_Scope (Ent);
      Analyze_Iteration_Scheme (Iter);

      --  Check for following case which merits a warning if the type E of is
      --  a multi-dimensional array (and no explicit subscript ranges present).

      --      for J in E'Range
      --         for K in E'Range

      if Present (Iter)
        and then Present (Loop_Parameter_Specification (Iter))
      then
         declare
            LPS : constant Node_Id := Loop_Parameter_Specification (Iter);
            DSD : constant Node_Id :=
                    Original_Node (Discrete_Subtype_Definition (LPS));
         begin
            if Nkind (DSD) = N_Attribute_Reference
              and then Attribute_Name (DSD) = Name_Range
              and then No (Expressions (DSD))
            then
               declare
                  Typ : constant Entity_Id := Etype (Prefix (DSD));
               begin
                  if Is_Array_Type (Typ)
                    and then Number_Dimensions (Typ) > 1
                    and then Nkind (Parent (N)) = N_Loop_Statement
                    and then Present (Iteration_Scheme (Parent (N)))
                  then
                     declare
                        OIter : constant Node_Id :=
                          Iteration_Scheme (Parent (N));
                        OLPS  : constant Node_Id :=
                          Loop_Parameter_Specification (OIter);
                        ODSD  : constant Node_Id :=
                          Original_Node (Discrete_Subtype_Definition (OLPS));
                     begin
                        if Nkind (ODSD) = N_Attribute_Reference
                          and then Attribute_Name (ODSD) = Name_Range
                          and then No (Expressions (ODSD))
                          and then Etype (Prefix (ODSD)) = Typ
                        then
                           Error_Msg_Sloc := Sloc (ODSD);
                           Error_Msg_N
                             ("inner range same as outer range#??", DSD);
                        end if;
                     end;
                  end if;
               end;
            end if;
         end;
      end if;

      --  Analyze the statements of the body except in the case of an Ada 2012
      --  iterator with the expander active. In this case the expander will do
      --  a rewrite of the loop into a while loop. We will then analyze the
      --  loop body when we analyze this while loop.

      --  We need to do this delay because if the container is for indefinite
      --  types the actual subtype of the components will only be determined
      --  when the cursor declaration is analyzed.

      --  If the expander is not active then we want to analyze the loop body
      --  now even in the Ada 2012 iterator case, since the rewriting will not
      --  be done. Insert the loop variable in the current scope, if not done
      --  when analysing the iteration scheme.  Set its kind properly to detect
      --  improper uses in the loop body.

      --  In GNATprove mode, we do one of the above depending on the kind of
      --  loop. If it is an iterator over an array, then we do not analyze the
      --  loop now. We will analyze it after it has been rewritten by the
      --  special SPARK expansion which is activated in GNATprove mode. We need
      --  to do this so that other expansions that should occur in GNATprove
      --  mode take into account the specificities of the rewritten loop, in
      --  particular the introduction of a renaming (which needs to be
      --  expanded).

      --  In other cases in GNATprove mode then we want to analyze the loop
      --  body now, since no rewriting will occur. Within a generic the
      --  GNATprove mode is irrelevant, we must analyze the generic for
      --  non-local name capture.

      if Present (Iter)
        and then Present (Iterator_Specification (Iter))
      then
         if GNATprove_Mode
           and then Is_Iterator_Over_Array (Iterator_Specification (Iter))
           and then not Inside_A_Generic
         then
            null;

         elsif not Expander_Active then
            declare
               I_Spec : constant Node_Id   := Iterator_Specification (Iter);
               Id     : constant Entity_Id := Defining_Identifier (I_Spec);

            begin
               if Scope (Id) /= Current_Scope then
                  Enter_Name (Id);
               end if;

               --  In an element iterator, The loop parameter is a variable if
               --  the domain of iteration (container or array) is a variable.

               if not Of_Present (I_Spec)
                 or else not Is_Variable (Name (I_Spec))
               then
                  Set_Ekind (Id, E_Loop_Parameter);
               end if;
            end;

            Analyze_Statements (Statements (N));
         end if;

      else
         --  Pre-Ada2012 for-loops and while loops

         Analyze_Statements (Statements (N));
      end if;

      --  When the iteration scheme of a loop contains attribute 'Loop_Entry,
      --  the loop is transformed into a conditional block. Retrieve the loop.

      Stmt := N;

      if Subject_To_Loop_Entry_Attributes (Stmt) then
         Stmt := Find_Loop_In_Conditional_Block (Stmt);
      end if;

      --  Finish up processing for the loop. We kill all current values, since
      --  in general we don't know if the statements in the loop have been
      --  executed. We could do a bit better than this with a loop that we
      --  know will execute at least once, but it's not worth the trouble and
      --  the front end is not in the business of flow tracing.

      Process_End_Label (Stmt, 'e', Ent);
      End_Scope;
      Kill_Current_Values;

      --  Check for infinite loop. Skip check for generated code, since it
      --  justs waste time and makes debugging the routine called harder.

      --  Note that we have to wait till the body of the loop is fully analyzed
      --  before making this call, since Check_Infinite_Loop_Warning relies on
      --  being able to use semantic visibility information to find references.

      if Comes_From_Source (Stmt) then
         Check_Infinite_Loop_Warning (Stmt);
      end if;

      --  Code after loop is unreachable if the loop has no WHILE or FOR and
      --  contains no EXIT statements within the body of the loop.

      if No (Iter) and then not Has_Exit (Ent) then
         Check_Unreachable_Code (Stmt);
      end if;

      --  Variables referenced within a loop subject to possible OpenACC
      --  offloading may be implicitly written to as part of the OpenACC
      --  transaction.  Clear flags possibly conveying that they are constant,
      --  set for example when the code does not explicitly assign them.

      if Is_OpenAcc_Environment (Stmt) then
         Disable_Constants (Stmt);
      end if;
   end Analyze_Loop_Statement;

   ----------------------------
   -- Analyze_Null_Statement --
   ----------------------------

   --  Note: the semantics of the null statement is implemented by a single
   --  null statement, too bad everything isn't as simple as this.

   procedure Analyze_Null_Statement (N : Node_Id) is
      pragma Warnings (Off, N);
   begin
      null;
   end Analyze_Null_Statement;

   -------------------------
   -- Analyze_Target_Name --
   -------------------------

   procedure Analyze_Target_Name (N : Node_Id) is
   begin
      --  A target name has the type of the left-hand side of the enclosing
      --  assignment.

      Set_Etype (N, Etype (Name (Current_Assignment)));
   end Analyze_Target_Name;

   ------------------------
   -- Analyze_Statements --
   ------------------------

   procedure Analyze_Statements (L : List_Id) is
      Lab : Entity_Id;
      S   : Node_Id;

   begin
      --  The labels declared in the statement list are reachable from
      --  statements in the list. We do this as a prepass so that any goto
      --  statement will be properly flagged if its target is not reachable.
      --  This is not required, but is nice behavior.

      S := First (L);
      while Present (S) loop
         if Nkind (S) = N_Label then
            Analyze (Identifier (S));
            Lab := Entity (Identifier (S));

            --  If we found a label mark it as reachable

            if Ekind (Lab) = E_Label then
               Generate_Definition (Lab);
               Set_Reachable (Lab);

               if Nkind (Parent (Lab)) = N_Implicit_Label_Declaration then
                  Set_Label_Construct (Parent (Lab), S);
               end if;

            --  If we failed to find a label, it means the implicit declaration
            --  of the label was hidden.  A for-loop parameter can do this to
            --  a label with the same name inside the loop, since the implicit
            --  label declaration is in the innermost enclosing body or block
            --  statement.

            else
               Error_Msg_Sloc := Sloc (Lab);
               Error_Msg_N
                 ("implicit label declaration for & is hidden#",
                  Identifier (S));
            end if;
         end if;

         Next (S);
      end loop;

      --  Perform semantic analysis on all statements

      Conditional_Statements_Begin;

      S := First (L);
      while Present (S) loop
         Analyze (S);

         --  Remove dimension in all statements

         Remove_Dimension_In_Statement (S);
         Next (S);
      end loop;

      Conditional_Statements_End;

      --  Make labels unreachable. Visibility is not sufficient, because labels
      --  in one if-branch for example are not reachable from the other branch,
      --  even though their declarations are in the enclosing declarative part.

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
      Error_Node : Node_Id;
      P          : Node_Id;

   begin
      if Is_List_Member (N) and then Comes_From_Source (N) then
         declare
            Nxt : Node_Id;

         begin
            Nxt := Original_Node (Next (N));

            --  Skip past pragmas

            while Nkind (Nxt) = N_Pragma loop
               Nxt := Original_Node (Next (Nxt));
            end loop;

            --  If a label follows us, then we never have dead code, since
            --  someone could branch to the label, so we just ignore it, unless
            --  we are in formal mode where goto statements are not allowed.

            if Nkind (Nxt) = N_Label
              and then not Restriction_Check_Required (SPARK_05)
            then
               return;

            --  Otherwise see if we have a real statement following us

            elsif Present (Nxt)
              and then Comes_From_Source (Nxt)
              and then Is_Statement (Nxt)
            then
               --  Special very annoying exception. If we have a return that
               --  follows a raise, then we allow it without a warning, since
               --  the Ada RM annoyingly requires a useless return here.

               if Nkind (Original_Node (N)) /= N_Raise_Statement
                 or else Nkind (Nxt) /= N_Simple_Return_Statement
               then
                  --  The rather strange shenanigans with the warning message
                  --  here reflects the fact that Kill_Dead_Code is very good
                  --  at removing warnings in deleted code, and this is one
                  --  warning we would prefer NOT to have removed.

                  Error_Node := Nxt;

                  --  If we have unreachable code, analyze and remove the
                  --  unreachable code, since it is useless and we don't
                  --  want to generate junk warnings.

                  --  We skip this step if we are not in code generation mode
                  --  or CodePeer mode.

                  --  This is the one case where we remove dead code in the
                  --  semantics as opposed to the expander, and we do not want
                  --  to remove code if we are not in code generation mode,
                  --  since this messes up the ASIS trees or loses useful
                  --  information in the CodePeer tree.

                  --  Note that one might react by moving the whole circuit to
                  --  exp_ch5, but then we lose the warning in -gnatc mode.

                  if Operating_Mode = Generate_Code
                    and then not CodePeer_Mode
                  then
                     loop
                        Nxt := Next (N);

                        --  Quit deleting when we have nothing more to delete
                        --  or if we hit a label (since someone could transfer
                        --  control to a label, so we should not delete it).

                        exit when No (Nxt) or else Nkind (Nxt) = N_Label;

                        --  Statement/declaration is to be deleted

                        Analyze (Nxt);
                        Remove (Nxt);
                        Kill_Dead_Code (Nxt);
                     end loop;
                  end if;

                  --  Now issue the warning (or error in formal mode)

                  if Restriction_Check_Required (SPARK_05) then
                     Check_SPARK_05_Restriction
                       ("unreachable code is not allowed", Error_Node);
                  else
                     Error_Msg
                       ("??unreachable code!", Sloc (Error_Node), Error_Node);
                  end if;
               end if;

            --  If the unconditional transfer of control instruction is the
            --  last statement of a sequence, then see if our parent is one of
            --  the constructs for which we count unblocked exits, and if so,
            --  adjust the count.

            else
               P := Parent (N);

               --  Statements in THEN part or ELSE part of IF statement

               if Nkind (P) = N_If_Statement then
                  null;

               --  Statements in ELSIF part of an IF statement

               elsif Nkind (P) = N_Elsif_Part then
                  P := Parent (P);
                  pragma Assert (Nkind (P) = N_If_Statement);

               --  Statements in CASE statement alternative

               elsif Nkind (P) = N_Case_Statement_Alternative then
                  P := Parent (P);
                  pragma Assert (Nkind (P) = N_Case_Statement);

               --  Statements in body of block

               elsif Nkind (P) = N_Handled_Sequence_Of_Statements
                 and then Nkind (Parent (P)) = N_Block_Statement
               then
                  --  The original loop is now placed inside a block statement
                  --  due to the expansion of attribute 'Loop_Entry. Return as
                  --  this is not a "real" block for the purposes of exit
                  --  counting.

                  if Nkind (N) = N_Loop_Statement
                    and then Subject_To_Loop_Entry_Attributes (N)
                  then
                     return;
                  end if;

               --  Statements in exception handler in a block

               elsif Nkind (P) = N_Exception_Handler
                 and then Nkind (Parent (P)) = N_Handled_Sequence_Of_Statements
                 and then Nkind (Parent (Parent (P))) = N_Block_Statement
               then
                  null;

               --  None of these cases, so return

               else
                  return;
               end if;

               --  This was one of the cases we are looking for (i.e. the
               --  parent construct was IF, CASE or block) so decrement count.

               Unblocked_Exit_Count := Unblocked_Exit_Count - 1;
            end if;
         end;
      end if;
   end Check_Unreachable_Code;

   ------------------------
   -- Has_Sec_Stack_Call --
   ------------------------

   function Has_Sec_Stack_Call (N : Node_Id) return Boolean is
      function Check_Call (N : Node_Id) return Traverse_Result;
      --  Check if N is a function call which uses the secondary stack

      ----------------
      -- Check_Call --
      ----------------

      function Check_Call (N : Node_Id) return Traverse_Result is
         Nam  : Node_Id;
         Subp : Entity_Id;
         Typ  : Entity_Id;

      begin
         if Nkind (N) = N_Function_Call then
            Nam := Name (N);

            --  Obtain the subprogram being invoked

            loop
               if Nkind (Nam) = N_Explicit_Dereference then
                  Nam := Prefix (Nam);

               elsif Nkind (Nam) = N_Selected_Component then
                  Nam := Selector_Name (Nam);

               else
                  exit;
               end if;
            end loop;

            Subp := Entity (Nam);

            if Present (Subp) then
               Typ := Etype (Subp);

               if Requires_Transient_Scope (Typ) then
                  return Abandon;

               elsif Sec_Stack_Needed_For_Return (Subp) then
                  return Abandon;
               end if;
            end if;
         end if;

         --  Continue traversing the tree

         return OK;
      end Check_Call;

      function Check_Calls is new Traverse_Func (Check_Call);

   --  Start of processing for Has_Sec_Stack_Call

   begin
      return Check_Calls (N) = Abandon;
   end Has_Sec_Stack_Call;

   ----------------------
   -- Preanalyze_Range --
   ----------------------

   procedure Preanalyze_Range (R_Copy : Node_Id) is
      Save_Analysis : constant Boolean := Full_Analysis;
      Typ           : Entity_Id;

   begin
      Full_Analysis := False;
      Expander_Mode_Save_And_Set (False);

      --  In addition to the above we must explicitly suppress the generation
      --  of freeze nodes that might otherwise be generated during resolution
      --  of the range (e.g. if given by an attribute that will freeze its
      --  prefix).

      Set_Must_Not_Freeze (R_Copy);

      if Nkind (R_Copy) = N_Attribute_Reference then
         Set_Must_Not_Freeze (Prefix (R_Copy));
      end if;

      Analyze (R_Copy);

      if Nkind (R_Copy) in N_Subexpr and then Is_Overloaded (R_Copy) then

         --  Apply preference rules for range of predefined integer types, or
         --  check for array or iterable construct for "of" iterator, or
         --  diagnose true ambiguity.

         declare
            I     : Interp_Index;
            It    : Interp;
            Found : Entity_Id := Empty;

         begin
            Get_First_Interp (R_Copy, I, It);
            while Present (It.Typ) loop
               if Is_Discrete_Type (It.Typ) then
                  if No (Found) then
                     Found := It.Typ;
                  else
                     if Scope (Found) = Standard_Standard then
                        null;

                     elsif Scope (It.Typ) = Standard_Standard then
                        Found := It.Typ;

                     else
                        --  Both of them are user-defined

                        Error_Msg_N
                          ("ambiguous bounds in range of iteration", R_Copy);
                        Error_Msg_N ("\possible interpretations:", R_Copy);
                        Error_Msg_NE ("\\} ", R_Copy, Found);
                        Error_Msg_NE ("\\} ", R_Copy, It.Typ);
                        exit;
                     end if;
                  end if;

               elsif Nkind (Parent (R_Copy)) = N_Iterator_Specification
                 and then Of_Present (Parent (R_Copy))
               then
                  if Is_Array_Type (It.Typ)
                    or else Has_Aspect (It.Typ, Aspect_Iterator_Element)
                    or else Has_Aspect (It.Typ, Aspect_Constant_Indexing)
                    or else Has_Aspect (It.Typ, Aspect_Variable_Indexing)
                  then
                     if No (Found) then
                        Found := It.Typ;
                        Set_Etype (R_Copy, It.Typ);

                     else
                        Error_Msg_N ("ambiguous domain of iteration", R_Copy);
                     end if;
                  end if;
               end if;

               Get_Next_Interp (I, It);
            end loop;
         end;
      end if;

      --  Subtype mark in iteration scheme

      if Is_Entity_Name (R_Copy) and then Is_Type (Entity (R_Copy)) then
         null;

      --  Expression in range, or Ada 2012 iterator

      elsif Nkind (R_Copy) in N_Subexpr then
         Resolve (R_Copy);
         Typ := Etype (R_Copy);

         if Is_Discrete_Type (Typ) then
            null;

         --  Check that the resulting object is an iterable container

         elsif Has_Aspect (Typ, Aspect_Iterator_Element)
           or else Has_Aspect (Typ, Aspect_Constant_Indexing)
           or else Has_Aspect (Typ, Aspect_Variable_Indexing)
         then
            null;

         --  The expression may yield an implicit reference to an iterable
         --  container. Insert explicit dereference so that proper type is
         --  visible in the loop.

         elsif Has_Implicit_Dereference (Etype (R_Copy)) then
            declare
               Disc : Entity_Id;

            begin
               Disc := First_Discriminant (Typ);
               while Present (Disc) loop
                  if Has_Implicit_Dereference (Disc) then
                     Build_Explicit_Dereference (R_Copy, Disc);
                     exit;
                  end if;

                  Next_Discriminant (Disc);
               end loop;
            end;

         end if;
      end if;

      Expander_Mode_Restore;
      Full_Analysis := Save_Analysis;
   end Preanalyze_Range;

end Sem_Ch5;
