------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 6                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2006, Free Software Foundation, Inc.         --
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
with Expander; use Expander;
with Exp_Ch7;  use Exp_Ch7;
with Exp_Tss;  use Exp_Tss;
with Fname;    use Fname;
with Freeze;   use Freeze;
with Itypes;   use Itypes;
with Lib.Xref; use Lib.Xref;
with Namet;    use Namet;
with Lib;      use Lib;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Output;   use Output;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Cat;  use Sem_Cat;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch4;  use Sem_Ch4;
with Sem_Ch5;  use Sem_Ch5;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch10; use Sem_Ch10;
with Sem_Ch12; use Sem_Ch12;
with Sem_Disp; use Sem_Disp;
with Sem_Dist; use Sem_Dist;
with Sem_Elim; use Sem_Elim;
with Sem_Eval; use Sem_Eval;
with Sem_Mech; use Sem_Mech;
with Sem_Prag; use Sem_Prag;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sem_Type; use Sem_Type;
with Sem_Warn; use Sem_Warn;
with Sinput;   use Sinput;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Sinfo.CN; use Sinfo.CN;
with Snames;   use Snames;
with Stringt;  use Stringt;
with Style;
with Stylesw;  use Stylesw;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;
with Urealp;   use Urealp;
with Validsw;  use Validsw;

package body Sem_Ch6 is

   --  The following flag is used to indicate that two formals in two
   --  subprograms being checked for conformance differ only in that one is
   --  an access parameter while the other is of a general access type with
   --  the same designated type. In this case, if the rest of the signatures
   --  match, a call to either subprogram may be ambiguous, which is worth
   --  a warning. The flag is set in Compatible_Types, and the warning emitted
   --  in New_Overloaded_Entity.

   May_Hide_Profile : Boolean := False;

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Analyze_Return_Type (N : Node_Id);
   --  Subsidiary to Process_Formals: analyze subtype mark in function
   --  specification, in a context where the formals are visible and hide
   --  outer homographs.

   procedure Analyze_Generic_Subprogram_Body (N : Node_Id; Gen_Id : Entity_Id);
   --  Analyze a generic subprogram body. N is the body to be analyzed, and
   --  Gen_Id is the defining entity Id for the corresponding spec.

   procedure Build_Body_To_Inline (N : Node_Id; Subp : Entity_Id);
   --  If a subprogram has pragma Inline and inlining is active, use generic
   --  machinery to build an unexpanded body for the subprogram. This body is
   --  subsequenty used for inline expansions at call sites. If subprogram can
   --  be inlined (depending on size and nature of local declarations) this
   --  function returns true. Otherwise subprogram body is treated normally.
   --  If proper warnings are enabled and the subprogram contains a construct
   --  that cannot be inlined, the offending construct is flagged accordingly.

   type Conformance_Type is
     (Type_Conformant, Mode_Conformant, Subtype_Conformant, Fully_Conformant);
   --  Conformance type used for following call, meaning matches the
   --  RM definitions of the corresponding terms.

   procedure Check_Conformance
     (New_Id                   : Entity_Id;
      Old_Id                   : Entity_Id;
      Ctype                    : Conformance_Type;
      Errmsg                   : Boolean;
      Conforms                 : out Boolean;
      Err_Loc                  : Node_Id := Empty;
      Get_Inst                 : Boolean := False;
      Skip_Controlling_Formals : Boolean := False);
   --  Given two entities, this procedure checks that the profiles associated
   --  with these entities meet the conformance criterion given by the third
   --  parameter. If they conform, Conforms is set True and control returns
   --  to the caller. If they do not conform, Conforms is set to False, and
   --  in addition, if Errmsg is True on the call, proper messages are output
   --  to complain about the conformance failure. If Err_Loc is non_Empty
   --  the error messages are placed on Err_Loc, if Err_Loc is empty, then
   --  error messages are placed on the appropriate part of the construct
   --  denoted by New_Id. If Get_Inst is true, then this is a mode conformance
   --  against a formal access-to-subprogram type so Get_Instance_Of must
   --  be called.

   procedure Check_Overriding_Indicator
     (Subp          : Entity_Id;
      Does_Override : Boolean);
   --  Verify the consistency of an overriding_indicator given for subprogram
   --  declaration, body, renaming, or instantiation. The flag Does_Override
   --  is set if the scope into which we are introducing the subprogram
   --  contains a type-conformant subprogram that becomes hidden by the new
   --  subprogram.

   procedure Check_Subprogram_Order (N : Node_Id);
   --  N is the N_Subprogram_Body node for a subprogram. This routine applies
   --  the alpha ordering rule for N if this ordering requirement applicable.

   procedure Check_Returns
     (HSS  : Node_Id;
      Mode : Character;
      Err  : out Boolean;
      Proc : Entity_Id := Empty);
   --  Called to check for missing return statements in a function body, or for
   --  returns present in a procedure body which has No_Return set. L is the
   --  handled statement sequence for the subprogram body. This procedure
   --  checks all flow paths to make sure they either have return (Mode = 'F',
   --  used for functions) or do not have a return (Mode = 'P', used for
   --  No_Return procedures). The flag Err is set if there are any control
   --  paths not explicitly terminated by a return in the function case, and is
   --  True otherwise. Proc is the entity for the procedure case and is used
   --  in posting the warning message.

   function Conforming_Types
     (T1       : Entity_Id;
      T2       : Entity_Id;
      Ctype    : Conformance_Type;
      Get_Inst : Boolean := False) return Boolean;
   --  Check that two formal parameter types conform, checking both for
   --  equality of base types, and where required statically matching
   --  subtypes, depending on the setting of Ctype.

   procedure Enter_Overloaded_Entity (S : Entity_Id);
   --  This procedure makes S, a new overloaded entity, into the first visible
   --  entity with that name.

   procedure Install_Entity (E : Entity_Id);
   --  Make single entity visible. Used for generic formals as well

   procedure Install_Formals (Id : Entity_Id);
   --  On entry to a subprogram body, make the formals visible. Note that
   --  simply placing the subprogram on the scope stack is not sufficient:
   --  the formals must become the current entities for their names.

   function Is_Non_Overriding_Operation
     (Prev_E : Entity_Id;
      New_E  : Entity_Id) return Boolean;
   --  Enforce the rule given in 12.3(18): a private operation in an instance
   --  overrides an inherited operation only if the corresponding operation
   --  was overriding in the generic. This can happen for primitive operations
   --  of types derived (in the generic unit) from formal private or formal
   --  derived types.

   procedure Make_Inequality_Operator (S : Entity_Id);
   --  Create the declaration for an inequality operator that is implicitly
   --  created by a user-defined equality operator that yields a boolean.

   procedure May_Need_Actuals (Fun : Entity_Id);
   --  Flag functions that can be called without parameters, i.e. those that
   --  have no parameters, or those for which defaults exist for all parameters

   procedure Reference_Body_Formals (Spec : Entity_Id; Bod : Entity_Id);
   --  If there is a separate spec for a subprogram or generic subprogram, the
   --  formals of the body are treated as references to the corresponding
   --  formals of the spec. This reference does not count as an actual use of
   --  the formal, in order to diagnose formals that are unused in the body.

   procedure Set_Formal_Validity (Formal_Id : Entity_Id);
   --  Formal_Id is an formal parameter entity. This procedure deals with
   --  setting the proper validity status for this entity, which depends
   --  on the kind of parameter and the validity checking mode.

   ---------------------------------------------
   -- Analyze_Abstract_Subprogram_Declaration --
   ---------------------------------------------

   procedure Analyze_Abstract_Subprogram_Declaration (N : Node_Id) is
      Designator : constant Entity_Id :=
                     Analyze_Subprogram_Specification (Specification (N));
      Scop       : constant Entity_Id := Current_Scope;

   begin
      Generate_Definition (Designator);
      Set_Is_Abstract (Designator);
      New_Overloaded_Entity (Designator);
      Check_Delayed_Subprogram (Designator);

      Set_Categorization_From_Scope (Designator, Scop);

      if Ekind (Scope (Designator)) = E_Protected_Type then
         Error_Msg_N
           ("abstract subprogram not allowed in protected type", N);
      end if;

      Generate_Reference_To_Formals (Designator);
   end Analyze_Abstract_Subprogram_Declaration;

   ----------------------------
   -- Analyze_Function_Call  --
   ----------------------------

   procedure Analyze_Function_Call (N : Node_Id) is
      P      : constant Node_Id := Name (N);
      L      : constant List_Id := Parameter_Associations (N);
      Actual : Node_Id;

   begin
      Analyze (P);

      --  A call of the form A.B (X) may be an Ada05 call, which is rewritten
      --  as B (A, X). If the rewriting is successful, the call has been
      --  analyzed and we just return.

      if Nkind (P) = N_Selected_Component
        and then Name (N) /= P
        and then Is_Rewrite_Substitution (N)
        and then Present (Etype (N))
      then
         return;
      end if;

      --  If error analyzing name, then set Any_Type as result type and return

      if Etype (P) = Any_Type then
         Set_Etype (N, Any_Type);
         return;
      end if;

      --  Otherwise analyze the parameters

      if Present (L) then
         Actual := First (L);
         while Present (Actual) loop
            Analyze (Actual);
            Check_Parameterless_Call (Actual);
            Next (Actual);
         end loop;
      end if;

      Analyze_Call (N);
   end Analyze_Function_Call;

   -------------------------------------
   -- Analyze_Generic_Subprogram_Body --
   -------------------------------------

   procedure Analyze_Generic_Subprogram_Body
     (N      : Node_Id;
      Gen_Id : Entity_Id)
   is
      Gen_Decl : constant Node_Id     := Unit_Declaration_Node (Gen_Id);
      Kind     : constant Entity_Kind := Ekind (Gen_Id);
      Body_Id  : Entity_Id;
      New_N    : Node_Id;
      Spec     : Node_Id;

   begin
      --  Copy body and disable expansion while analyzing the generic For a
      --  stub, do not copy the stub (which would load the proper body), this
      --  will be done when the proper body is analyzed.

      if Nkind (N) /= N_Subprogram_Body_Stub then
         New_N := Copy_Generic_Node (N, Empty, Instantiating => False);
         Rewrite (N, New_N);
         Start_Generic;
      end if;

      Spec := Specification (N);

      --  Within the body of the generic, the subprogram is callable, and
      --  behaves like the corresponding non-generic unit.

      Body_Id := Defining_Entity (Spec);

      if Kind = E_Generic_Procedure
        and then Nkind (Spec) /= N_Procedure_Specification
      then
         Error_Msg_N ("invalid body for generic procedure ", Body_Id);
         return;

      elsif Kind = E_Generic_Function
        and then Nkind (Spec) /= N_Function_Specification
      then
         Error_Msg_N ("invalid body for generic function ", Body_Id);
         return;
      end if;

      Set_Corresponding_Body (Gen_Decl, Body_Id);

      if Has_Completion (Gen_Id)
        and then Nkind (Parent (N)) /= N_Subunit
      then
         Error_Msg_N ("duplicate generic body", N);
         return;
      else
         Set_Has_Completion (Gen_Id);
      end if;

      if Nkind (N) = N_Subprogram_Body_Stub then
         Set_Ekind (Defining_Entity (Specification (N)), Kind);
      else
         Set_Corresponding_Spec (N, Gen_Id);
      end if;

      if Nkind (Parent (N)) = N_Compilation_Unit then
         Set_Cunit_Entity (Current_Sem_Unit, Defining_Entity (N));
      end if;

      --  Make generic parameters immediately visible in the body. They are
      --  needed to process the formals declarations. Then make the formals
      --  visible in a separate step.

      New_Scope (Gen_Id);

      declare
         E         : Entity_Id;
         First_Ent : Entity_Id;

      begin
         First_Ent := First_Entity (Gen_Id);

         E := First_Ent;
         while Present (E) and then not Is_Formal (E) loop
            Install_Entity (E);
            Next_Entity (E);
         end loop;

         Set_Use (Generic_Formal_Declarations (Gen_Decl));

         --  Now generic formals are visible, and the specification can be
         --  analyzed, for subsequent conformance check.

         Body_Id := Analyze_Subprogram_Specification (Spec);

         --  Make formal parameters visible

         if Present (E) then

            --  E is the first formal parameter, we loop through the formals
            --  installing them so that they will be visible.

            Set_First_Entity (Gen_Id, E);
            while Present (E) loop
               Install_Entity (E);
               Next_Formal (E);
            end loop;
         end if;

         --  Visible generic entity is callable within its own body

         Set_Ekind (Gen_Id, Ekind (Body_Id));
         Set_Ekind (Body_Id, E_Subprogram_Body);
         Set_Convention (Body_Id, Convention (Gen_Id));
         Set_Scope (Body_Id, Scope (Gen_Id));
         Check_Fully_Conformant (Body_Id, Gen_Id, Body_Id);

         if Nkind (N) = N_Subprogram_Body_Stub then

            --  No body to analyze, so restore state of generic unit

            Set_Ekind (Gen_Id, Kind);
            Set_Ekind (Body_Id, Kind);

            if Present (First_Ent) then
               Set_First_Entity (Gen_Id, First_Ent);
            end if;

            End_Scope;
            return;
         end if;

         --  If this is a compilation unit, it must be made visible explicitly,
         --  because the compilation of the declaration, unlike other library
         --  unit declarations, does not. If it is not a unit, the following
         --  is redundant but harmless.

         Set_Is_Immediately_Visible (Gen_Id);
         Reference_Body_Formals (Gen_Id, Body_Id);

         Set_Actual_Subtypes (N, Current_Scope);
         Analyze_Declarations (Declarations (N));
         Check_Completion;
         Analyze (Handled_Statement_Sequence (N));

         Save_Global_References (Original_Node (N));

         --  Prior to exiting the scope, include generic formals again (if any
         --  are present) in the set of local entities.

         if Present (First_Ent) then
            Set_First_Entity (Gen_Id, First_Ent);
         end if;

         Check_References (Gen_Id);
      end;

      Process_End_Label (Handled_Statement_Sequence (N), 't', Current_Scope);
      End_Scope;
      Check_Subprogram_Order (N);

      --  Outside of its body, unit is generic again

      Set_Ekind (Gen_Id, Kind);
      Generate_Reference (Gen_Id, Body_Id, 'b', Set_Ref => False);
      Style.Check_Identifier (Body_Id, Gen_Id);
      End_Generic;
   end Analyze_Generic_Subprogram_Body;

   -----------------------------
   -- Analyze_Operator_Symbol --
   -----------------------------

   --  An operator symbol such as "+" or "and" may appear in context where the
   --  literal denotes an entity name, such as "+"(x, y) or in context when it
   --  is just a string, as in (conjunction = "or"). In these cases the parser
   --  generates this node, and the semantics does the disambiguation. Other
   --  such case are actuals in an instantiation, the generic unit in an
   --  instantiation, and pragma arguments.

   procedure Analyze_Operator_Symbol (N : Node_Id) is
      Par : constant Node_Id := Parent (N);

   begin
      if        (Nkind (Par) = N_Function_Call and then N = Name (Par))
        or else  Nkind (Par) = N_Function_Instantiation
        or else (Nkind (Par) = N_Indexed_Component and then N = Prefix (Par))
        or else (Nkind (Par) = N_Pragma_Argument_Association
                   and then not Is_Pragma_String_Literal (Par))
        or else  Nkind (Par) = N_Subprogram_Renaming_Declaration
        or else  (Nkind (Par) = N_Attribute_Reference
                   and then Attribute_Name (Par) /= Name_Value)
      then
         Find_Direct_Name (N);

      else
         Change_Operator_Symbol_To_String_Literal (N);
         Analyze (N);
      end if;
   end Analyze_Operator_Symbol;

   -----------------------------------
   -- Analyze_Parameter_Association --
   -----------------------------------

   procedure Analyze_Parameter_Association (N : Node_Id) is
   begin
      Analyze (Explicit_Actual_Parameter (N));
   end Analyze_Parameter_Association;

   ----------------------------
   -- Analyze_Procedure_Call --
   ----------------------------

   procedure Analyze_Procedure_Call (N : Node_Id) is
      Loc     : constant Source_Ptr := Sloc (N);
      P       : constant Node_Id    := Name (N);
      Actuals : constant List_Id    := Parameter_Associations (N);
      Actual  : Node_Id;
      New_N   : Node_Id;

      procedure Analyze_Call_And_Resolve;
      --  Do Analyze and Resolve calls for procedure call

      ------------------------------
      -- Analyze_Call_And_Resolve --
      ------------------------------

      procedure Analyze_Call_And_Resolve is
      begin
         if Nkind (N) = N_Procedure_Call_Statement then
            Analyze_Call (N);
            Resolve (N, Standard_Void_Type);
         else
            Analyze (N);
         end if;
      end Analyze_Call_And_Resolve;

   --  Start of processing for Analyze_Procedure_Call

   begin
      --  The syntactic construct: PREFIX ACTUAL_PARAMETER_PART can denote
      --  a procedure call or an entry call. The prefix may denote an access
      --  to subprogram type, in which case an implicit dereference applies.
      --  If the prefix is an indexed component (without implicit defererence)
      --  then the construct denotes a call to a member of an entire family.
      --  If the prefix is a simple name, it may still denote a call to a
      --  parameterless member of an entry family. Resolution of these various
      --  interpretations is delicate.

      Analyze (P);

      --  If this is a call of the form Obj.Op, the call may have been
      --  analyzed and possibly rewritten into a block, in which case
      --  we are done.

      if Analyzed (N) then
         return;
      end if;

      --  If error analyzing prefix, then set Any_Type as result and return

      if Etype (P) = Any_Type then
         Set_Etype (N, Any_Type);
         return;
      end if;

      --  Otherwise analyze the parameters

      if Present (Actuals) then
         Actual := First (Actuals);

         while Present (Actual) loop
            Analyze (Actual);
            Check_Parameterless_Call (Actual);
            Next (Actual);
         end loop;
      end if;

      --  Special processing for Elab_Spec and Elab_Body calls

      if Nkind (P) = N_Attribute_Reference
        and then (Attribute_Name (P) = Name_Elab_Spec
                   or else Attribute_Name (P) = Name_Elab_Body)
      then
         if Present (Actuals) then
            Error_Msg_N
              ("no parameters allowed for this call", First (Actuals));
            return;
         end if;

         Set_Etype (N, Standard_Void_Type);
         Set_Analyzed (N);

      elsif Is_Entity_Name (P)
        and then Is_Record_Type (Etype (Entity (P)))
        and then Remote_AST_I_Dereference (P)
      then
         return;

      elsif Is_Entity_Name (P)
        and then Ekind (Entity (P)) /= E_Entry_Family
      then
         if Is_Access_Type (Etype (P))
           and then Ekind (Designated_Type (Etype (P))) = E_Subprogram_Type
           and then No (Actuals)
           and then Comes_From_Source (N)
         then
            Error_Msg_N ("missing explicit dereference in call", N);
         end if;

         Analyze_Call_And_Resolve;

      --  If the prefix is the simple name of an entry family, this is
      --  a parameterless call from within the task body itself.

      elsif Is_Entity_Name (P)
        and then Nkind (P) = N_Identifier
        and then Ekind (Entity (P)) = E_Entry_Family
        and then Present (Actuals)
        and then No (Next (First (Actuals)))
      then
         --  Can be call to parameterless entry family. What appears to be the
         --  sole argument is in fact the entry index. Rewrite prefix of node
         --  accordingly. Source representation is unchanged by this
         --  transformation.

         New_N :=
           Make_Indexed_Component (Loc,
             Prefix =>
               Make_Selected_Component (Loc,
                 Prefix => New_Occurrence_Of (Scope (Entity (P)), Loc),
                 Selector_Name => New_Occurrence_Of (Entity (P), Loc)),
             Expressions => Actuals);
         Set_Name (N, New_N);
         Set_Etype (New_N, Standard_Void_Type);
         Set_Parameter_Associations (N, No_List);
         Analyze_Call_And_Resolve;

      elsif Nkind (P) = N_Explicit_Dereference then
         if Ekind (Etype (P)) = E_Subprogram_Type then
            Analyze_Call_And_Resolve;
         else
            Error_Msg_N ("expect access to procedure in call", P);
         end if;

      --  The name can be a selected component or an indexed component that
      --  yields an access to subprogram. Such a prefix is legal if the call
      --  has parameter associations.

      elsif Is_Access_Type (Etype (P))
        and then Ekind (Designated_Type (Etype (P))) = E_Subprogram_Type
      then
         if Present (Actuals) then
            Analyze_Call_And_Resolve;
         else
            Error_Msg_N ("missing explicit dereference in call ", N);
         end if;

      --  If not an access to subprogram, then the prefix must resolve to the
      --  name of an entry, entry family, or protected operation.

      --  For the case of a simple entry call, P is a selected component where
      --  the prefix is the task and the selector name is the entry. A call to
      --  a protected procedure will have the same syntax. If the protected
      --  object contains overloaded operations, the entity may appear as a
      --  function, the context will select the operation whose type is Void.

      elsif Nkind (P) = N_Selected_Component
        and then (Ekind (Entity (Selector_Name (P))) = E_Entry
                    or else
                  Ekind (Entity (Selector_Name (P))) = E_Procedure
                    or else
                  Ekind (Entity (Selector_Name (P))) = E_Function)
      then
         Analyze_Call_And_Resolve;

      elsif Nkind (P) = N_Selected_Component
        and then Ekind (Entity (Selector_Name (P))) = E_Entry_Family
        and then Present (Actuals)
        and then No (Next (First (Actuals)))
      then
         --  Can be call to parameterless entry family. What appears to be the
         --  sole argument is in fact the entry index. Rewrite prefix of node
         --  accordingly. Source representation is unchanged by this
         --  transformation.

         New_N :=
           Make_Indexed_Component (Loc,
             Prefix => New_Copy (P),
             Expressions => Actuals);
         Set_Name (N, New_N);
         Set_Etype (New_N, Standard_Void_Type);
         Set_Parameter_Associations (N, No_List);
         Analyze_Call_And_Resolve;

      --  For the case of a reference to an element of an entry family, P is
      --  an indexed component whose prefix is a selected component (task and
      --  entry family), and whose index is the entry family index.

      elsif Nkind (P) = N_Indexed_Component
        and then Nkind (Prefix (P)) = N_Selected_Component
        and then Ekind (Entity (Selector_Name (Prefix (P)))) = E_Entry_Family
      then
         Analyze_Call_And_Resolve;

      --  If the prefix is the name of an entry family, it is a call from
      --  within the task body itself.

      elsif Nkind (P) = N_Indexed_Component
        and then Nkind (Prefix (P)) = N_Identifier
        and then Ekind (Entity (Prefix (P))) = E_Entry_Family
      then
         New_N :=
           Make_Selected_Component (Loc,
             Prefix => New_Occurrence_Of (Scope (Entity (Prefix (P))), Loc),
             Selector_Name => New_Occurrence_Of (Entity (Prefix (P)), Loc));
         Rewrite (Prefix (P), New_N);
         Analyze (P);
         Analyze_Call_And_Resolve;

      --  Anything else is an error

      else
         Error_Msg_N ("invalid procedure or entry call", N);
      end if;
   end Analyze_Procedure_Call;

   ------------------------------
   -- Analyze_Return_Statement --
   ------------------------------

   procedure Analyze_Return_Statement (N : Node_Id) is
      Loc      : constant Source_Ptr := Sloc (N);
      Expr     : Node_Id;
      Scope_Id : Entity_Id;
      Kind     : Entity_Kind;
      R_Type   : Entity_Id;

   begin
      --  Find subprogram or accept statement enclosing the return statement

      Scope_Id := Empty;
      for J in reverse 0 .. Scope_Stack.Last loop
         Scope_Id := Scope_Stack.Table (J).Entity;
         exit when Ekind (Scope_Id) /= E_Block and then
                   Ekind (Scope_Id) /= E_Loop;
      end loop;

      pragma Assert (Present (Scope_Id));

      Kind := Ekind (Scope_Id);
      Expr := Expression (N);

      if Kind /= E_Function
        and then Kind /= E_Generic_Function
        and then Kind /= E_Procedure
        and then Kind /= E_Generic_Procedure
        and then Kind /= E_Entry
        and then Kind /= E_Entry_Family
      then
         Error_Msg_N ("illegal context for return statement", N);

      elsif Present (Expr) then
         if Kind = E_Function or else Kind = E_Generic_Function then
            Set_Return_Present (Scope_Id);
            R_Type := Etype (Scope_Id);
            Set_Return_Type (N, R_Type);
            Analyze_And_Resolve (Expr, R_Type);

            --  Ada 2005 (AI-318-02): When the result type is an anonymous
            --  access type, apply an implicit conversion of the expression
            --  to that type to force appropriate static and run-time
            --  accessibility checks.

            if Ada_Version >= Ada_05
              and then Ekind (R_Type) = E_Anonymous_Access_Type
            then
               Rewrite (Expr, Convert_To (R_Type, Relocate_Node (Expr)));
               Analyze_And_Resolve (Expr, R_Type);
            end if;

            if (Is_Class_Wide_Type (Etype (Expr))
                 or else Is_Dynamically_Tagged (Expr))
              and then not Is_Class_Wide_Type (R_Type)
            then
               Error_Msg_N
                 ("dynamically tagged expression not allowed!", Expr);
            end if;

            Apply_Constraint_Check (Expr, R_Type);

            --  Ada 2005 (AI-318-02): Return-by-reference types have been
            --  removed and replaced by anonymous access results. This is
            --  an incompatibility with Ada 95. Not clear whether this
            --  should be enforced yet or perhaps controllable with a
            --  special switch. ???

            --  if Ada_Version >= Ada_05
            --    and then Is_Limited_Type (R_Type)
            --    and then Nkind (Expr) /= N_Aggregate
            --    and then Nkind (Expr) /= N_Extension_Aggregate
            --    and then Nkind (Expr) /= N_Function_Call
            --  then
            --     Error_Msg_N
            --       ("(Ada 2005) illegal operand for limited return", N);
            --  end if;

            --  ??? A real run-time accessibility check is needed in cases
            --  involving dereferences of access parameters. For now we just
            --  check the static cases.

            if Is_Return_By_Reference_Type (Etype (Scope_Id))
              and then Object_Access_Level (Expr)
                > Subprogram_Access_Level (Scope_Id)
            then
               Rewrite (N,
                 Make_Raise_Program_Error (Loc,
                   Reason => PE_Accessibility_Check_Failed));
               Analyze (N);

               Error_Msg_N
                 ("cannot return a local value by reference?", N);
               Error_Msg_NE
                 ("\& will be raised at run time?",
                  N, Standard_Program_Error);
            end if;

         elsif Kind = E_Procedure or else Kind = E_Generic_Procedure then
            Error_Msg_N ("procedure cannot return value (use function)", N);

         else
            Error_Msg_N ("accept statement cannot return value", N);
         end if;

      --  No expression present

      else
         if Kind = E_Function or Kind = E_Generic_Function then
            Error_Msg_N ("missing expression in return from function", N);
         end if;

         if (Ekind (Scope_Id) = E_Procedure
              or else Ekind (Scope_Id) = E_Generic_Procedure)
           and then No_Return (Scope_Id)
         then
            Error_Msg_N
              ("RETURN statement not allowed (No_Return)", N);
         end if;
      end if;

      Check_Unreachable_Code (N);
   end Analyze_Return_Statement;

   -------------------------
   -- Analyze_Return_Type --
   -------------------------

   procedure Analyze_Return_Type (N : Node_Id) is
      Designator : constant Entity_Id := Defining_Entity (N);
      Typ        : Entity_Id := Empty;

   begin
      if Result_Definition (N) /= Error then
         if Nkind (Result_Definition (N)) = N_Access_Definition then
            Typ := Access_Definition (N, Result_Definition (N));
            Set_Parent (Typ, Result_Definition (N));
            Set_Is_Local_Anonymous_Access (Typ);
            Set_Etype (Designator, Typ);

            --  Ada 2005 (AI-231): Static checks

            --  Null_Exclusion_Static_Checks needs to be extended to handle
            --  null exclusion checks for function specifications. ???

            --  if Null_Exclusion_Present (N) then
            --     Null_Exclusion_Static_Checks (Param_Spec);
            --  end if;

         --  Subtype_Mark case

         else
            Find_Type (Result_Definition (N));
            Typ := Entity (Result_Definition (N));
            Set_Etype (Designator, Typ);

            if Ekind (Typ) = E_Incomplete_Type
              or else (Is_Class_Wide_Type (Typ)
                         and then
                           Ekind (Root_Type (Typ)) = E_Incomplete_Type)
            then
               Error_Msg_N
                 ("invalid use of incomplete type", Result_Definition (N));
            end if;
         end if;

      else
         Set_Etype (Designator, Any_Type);
      end if;
   end Analyze_Return_Type;

   -----------------------------
   -- Analyze_Subprogram_Body --
   -----------------------------

   --  This procedure is called for regular subprogram bodies, generic bodies,
   --  and for subprogram stubs of both kinds. In the case of stubs, only the
   --  specification matters, and is used to create a proper declaration for
   --  the subprogram, or to perform conformance checks.

   procedure Analyze_Subprogram_Body (N : Node_Id) is
      Loc          : constant Source_Ptr := Sloc (N);
      Body_Spec    : constant Node_Id    := Specification (N);
      Body_Id      : Entity_Id           := Defining_Entity (Body_Spec);
      Prev_Id      : constant Entity_Id  := Current_Entity_In_Scope (Body_Id);
      Body_Deleted : constant Boolean    := False;

      HSS          : Node_Id;
      Spec_Id      : Entity_Id;
      Spec_Decl    : Node_Id   := Empty;
      Last_Formal  : Entity_Id := Empty;
      Conformant   : Boolean;
      Missing_Ret  : Boolean;
      P_Ent        : Entity_Id;

      procedure Check_Inline_Pragma (Spec : in out Node_Id);
      --  Look ahead to recognize a pragma that may appear after the body.
      --  If there is a previous spec, check that it appears in the same
      --  declarative part. If the pragma is Inline_Always, perform inlining
      --  unconditionally, otherwise only if Front_End_Inlining is requested.
      --  If the body acts as a spec, and inlining is required, we create a
      --  subprogram declaration for it, in order to attach the body to inline.

      procedure Copy_Parameter_List (Plist : List_Id);
      --  Comment required ???

      procedure Verify_Overriding_Indicator;
      --  If there was a previous spec, the entity has been entered in the
      --  current scope previously. If the body itself carries an overriding
      --  indicator, check that it is consistent with the known status of the
      --  entity.

      -------------------------
      -- Check_Inline_Pragma --
      -------------------------

      procedure Check_Inline_Pragma (Spec : in out Node_Id) is
         Prag  : Node_Id;
         Plist : List_Id;

      begin
         if not Expander_Active then
            return;
         end if;

         if Is_List_Member (N)
           and then Present (Next (N))
           and then Nkind (Next (N)) = N_Pragma
         then
            Prag := Next (N);

            if Nkind (Prag) = N_Pragma
              and then
                 (Get_Pragma_Id (Chars (Prag)) = Pragma_Inline_Always
                  or else
                    (Front_End_Inlining
                     and then Get_Pragma_Id (Chars (Prag)) = Pragma_Inline))
              and then
                 Chars
                   (Expression (First (Pragma_Argument_Associations (Prag))))
                      = Chars (Body_Id)
            then
               Prag := Next (N);
            else
               Prag := Empty;
            end if;
         else
            Prag := Empty;
         end if;

         if Present (Prag) then
            if Present (Spec_Id) then
               if List_Containing (N) =
                 List_Containing (Unit_Declaration_Node (Spec_Id))
               then
                  Analyze (Prag);
               end if;

            else
               --  Create a subprogram declaration, to make treatment uniform

               declare
                  Subp : constant Entity_Id :=
                    Make_Defining_Identifier (Loc, Chars (Body_Id));
                  Decl : constant Node_Id :=
                    Make_Subprogram_Declaration (Loc,
                      Specification =>  New_Copy_Tree (Specification (N)));
               begin
                  Set_Defining_Unit_Name (Specification (Decl), Subp);

                  if Present (First_Formal (Body_Id)) then
                     Plist := New_List;
                     Copy_Parameter_List (Plist);
                     Set_Parameter_Specifications
                       (Specification (Decl), Plist);
                  end if;

                  Insert_Before (N, Decl);
                  Analyze (Decl);
                  Analyze (Prag);
                  Set_Has_Pragma_Inline (Subp);

                  if Get_Pragma_Id (Chars (Prag)) = Pragma_Inline_Always then
                     Set_Is_Inlined (Subp);
                     Set_Next_Rep_Item (Prag, First_Rep_Item (Subp));
                     Set_First_Rep_Item (Subp, Prag);
                  end if;

                  Spec := Subp;
               end;
            end if;
         end if;
      end Check_Inline_Pragma;

      -------------------------
      -- Copy_Parameter_List --
      -------------------------

      procedure Copy_Parameter_List (Plist : List_Id) is
         Formal : Entity_Id;

      begin
         Formal := First_Formal (Body_Id);

         while Present (Formal) loop
            Append
              (Make_Parameter_Specification (Loc,
                Defining_Identifier =>
                  Make_Defining_Identifier (Sloc (Formal),
                    Chars => Chars (Formal)),
                In_Present  => In_Present (Parent (Formal)),
                Out_Present => Out_Present (Parent (Formal)),
             Parameter_Type =>
                  New_Reference_To (Etype (Formal), Loc),
                Expression =>
                  New_Copy_Tree (Expression (Parent (Formal)))),
              Plist);

            Next_Formal (Formal);
         end loop;
      end Copy_Parameter_List;

      ---------------------------------
      -- Verify_Overriding_Indicator --
      ---------------------------------

      procedure Verify_Overriding_Indicator is
      begin
         if Must_Override (Body_Spec)
           and then not Is_Overriding_Operation (Spec_Id)
         then
            Error_Msg_NE
              ("subprogram& is not overriding", Body_Spec, Spec_Id);

         elsif Must_Not_Override (Body_Spec)
              and then Is_Overriding_Operation (Spec_Id)
         then
            Error_Msg_NE
              ("subprogram& overrides inherited operation",
                 Body_Spec, Spec_Id);
         end if;
      end Verify_Overriding_Indicator;

   --  Start of processing for Analyze_Subprogram_Body

   begin
      if Debug_Flag_C then
         Write_Str ("====  Compiling subprogram body ");
         Write_Name (Chars (Body_Id));
         Write_Str (" from ");
         Write_Location (Loc);
         Write_Eol;
      end if;

      Trace_Scope (N, Body_Id, " Analyze subprogram");

      --  Generic subprograms are handled separately. They always have a
      --  generic specification. Determine whether current scope has a
      --  previous declaration.

      --  If the subprogram body is defined within an instance of the same
      --  name, the instance appears as a package renaming, and will be hidden
      --  within the subprogram.

      if Present (Prev_Id)
        and then not Is_Overloadable (Prev_Id)
        and then (Nkind (Parent (Prev_Id)) /= N_Package_Renaming_Declaration
                   or else Comes_From_Source (Prev_Id))
      then
         if Is_Generic_Subprogram (Prev_Id) then
            Spec_Id := Prev_Id;
            Set_Is_Compilation_Unit (Body_Id, Is_Compilation_Unit (Spec_Id));
            Set_Is_Child_Unit       (Body_Id, Is_Child_Unit       (Spec_Id));

            Analyze_Generic_Subprogram_Body (N, Spec_Id);
            return;

         else
            --  Previous entity conflicts with subprogram name. Attempting to
            --  enter name will post error.

            Enter_Name (Body_Id);
            return;
         end if;

      --  Non-generic case, find the subprogram declaration, if one was seen,
      --  or enter new overloaded entity in the current scope. If the
      --  Current_Entity is the Body_Id itself, the unit is being analyzed as
      --  part of the context of one of its subunits. No need to redo the
      --  analysis.

      elsif Prev_Id = Body_Id
        and then Has_Completion (Body_Id)
      then
         return;

      else
         Body_Id := Analyze_Subprogram_Specification (Body_Spec);

         if Nkind (N) = N_Subprogram_Body_Stub
           or else No (Corresponding_Spec (N))
         then
            Spec_Id := Find_Corresponding_Spec (N);

            --  If this is a duplicate body, no point in analyzing it

            if Error_Posted (N) then
               return;
            end if;

            --  A subprogram body should cause freezing of its own declaration,
            --  but if there was no previous explicit declaration, then the
            --  subprogram will get frozen too late (there may be code within
            --  the body that depends on the subprogram having been frozen,
            --  such as uses of extra formals), so we force it to be frozen
            --  here. Same holds if the body and the spec are compilation
            --  units.

            if No (Spec_Id) then
               Freeze_Before (N, Body_Id);

            elsif Nkind (Parent (N)) = N_Compilation_Unit then
               Freeze_Before (N, Spec_Id);
            end if;
         else
            Spec_Id := Corresponding_Spec (N);
         end if;
      end if;

      --  Do not inline any subprogram that contains nested subprograms, since
      --  the backend inlining circuit seems to generate uninitialized
      --  references in this case. We know this happens in the case of front
      --  end ZCX support, but it also appears it can happen in other cases as
      --  well. The backend often rejects attempts to inline in the case of
      --  nested procedures anyway, so little if anything is lost by this.
      --  Note that this is test is for the benefit of the back-end. There is
      --  a separate test for front-end inlining that also rejects nested
      --  subprograms.

      --  Do not do this test if errors have been detected, because in some
      --  error cases, this code blows up, and we don't need it anyway if
      --  there have been errors, since we won't get to the linker anyway.

      if Comes_From_Source (Body_Id)
        and then Serious_Errors_Detected = 0
      then
         P_Ent := Body_Id;
         loop
            P_Ent := Scope (P_Ent);
            exit when No (P_Ent) or else P_Ent = Standard_Standard;

            if Is_Subprogram (P_Ent) then
               Set_Is_Inlined (P_Ent, False);

               if Comes_From_Source (P_Ent)
                 and then Has_Pragma_Inline (P_Ent)
               then
                  Cannot_Inline
                    ("cannot inline& (nested subprogram)?",
                     N, P_Ent);
               end if;
            end if;
         end loop;
      end if;

      Check_Inline_Pragma (Spec_Id);

      --  Case of fully private operation in the body of the protected type.
      --  We must create a declaration for the subprogram, in order to attach
      --  the protected subprogram that will be used in internal calls.

      if No (Spec_Id)
        and then Comes_From_Source (N)
        and then Is_Protected_Type (Current_Scope)
      then
         declare
            Decl     : Node_Id;
            Plist    : List_Id;
            Formal   : Entity_Id;
            New_Spec : Node_Id;

         begin
            Formal := First_Formal (Body_Id);

            --  The protected operation always has at least one formal, namely
            --  the object itself, but it is only placed in the parameter list
            --  if expansion is enabled.

            if Present (Formal)
              or else Expander_Active
            then
               Plist := New_List;

            else
               Plist := No_List;
            end if;

            Copy_Parameter_List (Plist);

            if Nkind (Body_Spec) = N_Procedure_Specification then
               New_Spec :=
                 Make_Procedure_Specification (Loc,
                    Defining_Unit_Name =>
                      Make_Defining_Identifier (Sloc (Body_Id),
                        Chars => Chars (Body_Id)),
                    Parameter_Specifications => Plist);
            else
               New_Spec :=
                 Make_Function_Specification (Loc,
                    Defining_Unit_Name =>
                      Make_Defining_Identifier (Sloc (Body_Id),
                        Chars => Chars (Body_Id)),
                    Parameter_Specifications => Plist,
                    Result_Definition =>
                      New_Occurrence_Of (Etype (Body_Id), Loc));
            end if;

            Decl :=
              Make_Subprogram_Declaration (Loc,
                Specification => New_Spec);
            Insert_Before (N, Decl);
            Spec_Id := Defining_Unit_Name (New_Spec);

            --  Indicate that the entity comes from source, to ensure that
            --  cross-reference information is properly generated. The body
            --  itself is rewritten during expansion, and the body entity will
            --  not appear in calls to the operation.

            Set_Comes_From_Source (Spec_Id, True);
            Analyze (Decl);
            Set_Has_Completion (Spec_Id);
            Set_Convention (Spec_Id, Convention_Protected);
         end;

      elsif Present (Spec_Id) then
         Spec_Decl := Unit_Declaration_Node (Spec_Id);
         Verify_Overriding_Indicator;
      end if;

      --  Place subprogram on scope stack, and make formals visible. If there
      --  is a spec, the visible entity remains that of the spec.

      if Present (Spec_Id) then
         Generate_Reference (Spec_Id, Body_Id, 'b', Set_Ref => False);

         if Is_Child_Unit (Spec_Id) then
            Generate_Reference (Spec_Id, Scope (Spec_Id), 'k', False);
         end if;

         if Style_Check then
            Style.Check_Identifier (Body_Id, Spec_Id);
         end if;

         Set_Is_Compilation_Unit (Body_Id, Is_Compilation_Unit (Spec_Id));
         Set_Is_Child_Unit       (Body_Id, Is_Child_Unit       (Spec_Id));

         if Is_Abstract (Spec_Id) then
            Error_Msg_N ("an abstract subprogram cannot have a body", N);
            return;
         else
            Set_Convention (Body_Id, Convention (Spec_Id));
            Set_Has_Completion (Spec_Id);

            if Is_Protected_Type (Scope (Spec_Id)) then
               Set_Privals_Chain (Spec_Id, New_Elmt_List);
            end if;

            --  If this is a body generated for a renaming, do not check for
            --  full conformance. The check is redundant, because the spec of
            --  the body is a copy of the spec in the renaming declaration,
            --  and the test can lead to spurious errors on nested defaults.

            if Present (Spec_Decl)
              and then not Comes_From_Source (N)
              and then
                (Nkind (Original_Node (Spec_Decl)) =
                                        N_Subprogram_Renaming_Declaration
                   or else (Present (Corresponding_Body (Spec_Decl))
                              and then
                                Nkind (Unit_Declaration_Node
                                        (Corresponding_Body (Spec_Decl))) =
                                           N_Subprogram_Renaming_Declaration))
            then
               Conformant := True;
            else
               Check_Conformance
                 (Body_Id, Spec_Id,
                   Fully_Conformant, True, Conformant, Body_Id);
            end if;

            --  If the body is not fully conformant, we have to decide if we
            --  should analyze it or not. If it has a really messed up profile
            --  then we probably should not analyze it, since we will get too
            --  many bogus messages.

            --  Our decision is to go ahead in the non-fully conformant case
            --  only if it is at least mode conformant with the spec. Note
            --  that the call to Check_Fully_Conformant has issued the proper
            --  error messages to complain about the lack of conformance.

            if not Conformant
              and then not Mode_Conformant (Body_Id, Spec_Id)
            then
               return;
            end if;
         end if;

         if Spec_Id /= Body_Id then
            Reference_Body_Formals (Spec_Id, Body_Id);
         end if;

         if Nkind (N) /= N_Subprogram_Body_Stub then
            Set_Corresponding_Spec (N, Spec_Id);

            --  Ada 2005 (AI-345): Restore the correct Etype: here we undo the
            --  work done by Analyze_Subprogram_Specification to allow the
            --  overriding of task, protected and interface primitives.

            if Comes_From_Source (Spec_Id)
              and then Present (First_Entity (Spec_Id))
              and then Ekind (Etype (First_Entity (Spec_Id))) = E_Record_Type
              and then Is_Tagged_Type (Etype (First_Entity (Spec_Id)))
              and then Present (Abstract_Interfaces
                                (Etype (First_Entity (Spec_Id))))
              and then Present (Corresponding_Concurrent_Type
                                (Etype (First_Entity (Spec_Id))))
            then
               Set_Etype (First_Entity (Spec_Id),
                 Corresponding_Concurrent_Type
                   (Etype (First_Entity (Spec_Id))));
            end if;

            --  Ada 2005: A formal that is an access parameter may have a
            --  designated type imported through a limited_with clause, while
            --  the body has a regular with clause. Update the types of the
            --  formals accordingly, so that the non-limited view of each type
            --  is available in the body. We have already verified that the
            --  declarations are type-conformant.

            if Ada_Version >= Ada_05 then
               declare
                  F_Spec : Entity_Id;
                  F_Body : Entity_Id;

               begin
                  F_Spec := First_Formal (Spec_Id);
                  F_Body := First_Formal (Body_Id);

                  while Present (F_Spec) loop
                     if Ekind (Etype (F_Spec)) = E_Anonymous_Access_Type
                       and then
                         From_With_Type (Designated_Type (Etype (F_Spec)))
                     then
                        Set_Etype (F_Spec, Etype (F_Body));
                     end if;

                     Next_Formal (F_Spec);
                     Next_Formal (F_Body);
                  end loop;
               end;
            end if;

            --  Now make the formals visible, and place subprogram
            --  on scope stack.

            Install_Formals (Spec_Id);
            Last_Formal := Last_Entity (Spec_Id);
            New_Scope (Spec_Id);

            --  Make sure that the subprogram is immediately visible. For
            --  child units that have no separate spec this is indispensable.
            --  Otherwise it is safe albeit redundant.

            Set_Is_Immediately_Visible (Spec_Id);
         end if;

         Set_Corresponding_Body (Unit_Declaration_Node (Spec_Id), Body_Id);
         Set_Ekind (Body_Id, E_Subprogram_Body);
         Set_Scope (Body_Id, Scope (Spec_Id));

      --  Case of subprogram body with no previous spec

      else
         if Style_Check
           and then Comes_From_Source (Body_Id)
           and then not Suppress_Style_Checks (Body_Id)
           and then not In_Instance
         then
            Style.Body_With_No_Spec (N);
         end if;

         New_Overloaded_Entity (Body_Id);

         if Nkind (N) /= N_Subprogram_Body_Stub then
            Set_Acts_As_Spec (N);
            Generate_Definition (Body_Id);
            Generate_Reference
              (Body_Id, Body_Id, 'b', Set_Ref => False, Force => True);
            Generate_Reference_To_Formals (Body_Id);
            Install_Formals (Body_Id);
            New_Scope (Body_Id);
         end if;
      end if;

      --  If this is the proper body of a stub, we must verify that the stub
      --  conforms to the body, and to the previous spec if one was present.
      --  we know already that the body conforms to that spec. This test is
      --  only required for subprograms that come from source.

      if Nkind (Parent (N)) = N_Subunit
        and then Comes_From_Source (N)
        and then not Error_Posted (Body_Id)
        and then Nkind (Corresponding_Stub (Parent (N))) =
                                                N_Subprogram_Body_Stub
      then
         declare
            Old_Id : constant Entity_Id :=
                       Defining_Entity
                         (Specification (Corresponding_Stub (Parent (N))));

            Conformant : Boolean := False;

         begin
            if No (Spec_Id) then
               Check_Fully_Conformant (Body_Id, Old_Id);

            else
               Check_Conformance
                 (Body_Id, Old_Id, Fully_Conformant, False, Conformant);

               if not Conformant then

                  --  The stub was taken to be a new declaration. Indicate
                  --  that it lacks a body.

                  Set_Has_Completion (Old_Id, False);
               end if;
            end if;
         end;
      end if;

      Set_Has_Completion (Body_Id);
      Check_Eliminated (Body_Id);

      if Nkind (N) = N_Subprogram_Body_Stub then
         return;

      elsif  Present (Spec_Id)
        and then Expander_Active
        and then
          (Is_Always_Inlined (Spec_Id)
             or else (Has_Pragma_Inline (Spec_Id) and Front_End_Inlining))
      then
         Build_Body_To_Inline (N, Spec_Id);
      end if;

      --  Ada 2005 (AI-262): In library subprogram bodies, after the analysis
      --  if its specification we have to install the private withed units.

      if Is_Compilation_Unit (Body_Id)
        and then Scope (Body_Id) = Standard_Standard
      then
         Install_Private_With_Clauses (Body_Id);
      end if;

      --  Now we can go on to analyze the body

      HSS := Handled_Statement_Sequence (N);
      Set_Actual_Subtypes (N, Current_Scope);
      Analyze_Declarations (Declarations (N));
      Check_Completion;
      Analyze (HSS);
      Process_End_Label (HSS, 't', Current_Scope);
      End_Scope;
      Check_Subprogram_Order (N);
      Set_Analyzed (Body_Id);

      --  If we have a separate spec, then the analysis of the declarations
      --  caused the entities in the body to be chained to the spec id, but
      --  we want them chained to the body id. Only the formal parameters
      --  end up chained to the spec id in this case.

      if Present (Spec_Id) then

         --  We must conform to the categorization of our spec

         Validate_Categorization_Dependency (N, Spec_Id);

         --  And if this is a child unit, the parent units must conform

         if Is_Child_Unit (Spec_Id) then
            Validate_Categorization_Dependency
              (Unit_Declaration_Node (Spec_Id), Spec_Id);
         end if;

         if Present (Last_Formal) then
            Set_Next_Entity
              (Last_Entity (Body_Id), Next_Entity (Last_Formal));
            Set_Next_Entity (Last_Formal, Empty);
            Set_Last_Entity (Body_Id, Last_Entity (Spec_Id));
            Set_Last_Entity (Spec_Id, Last_Formal);

         else
            Set_First_Entity (Body_Id, First_Entity (Spec_Id));
            Set_Last_Entity  (Body_Id, Last_Entity (Spec_Id));
            Set_First_Entity (Spec_Id, Empty);
            Set_Last_Entity  (Spec_Id, Empty);
         end if;
      end if;

      --  If function, check return statements

      if Nkind (Body_Spec) = N_Function_Specification then
         declare
            Id : Entity_Id;

         begin
            if Present (Spec_Id) then
               Id := Spec_Id;
            else
               Id := Body_Id;
            end if;

            if Return_Present (Id) then
               Check_Returns (HSS, 'F', Missing_Ret);

               if Missing_Ret then
                  Set_Has_Missing_Return (Id);
               end if;

            elsif not Is_Machine_Code_Subprogram (Id)
              and then not Body_Deleted
            then
               Error_Msg_N ("missing RETURN statement in function body", N);
            end if;
         end;

      --  If procedure with No_Return, check returns

      elsif Nkind (Body_Spec) = N_Procedure_Specification
        and then Present (Spec_Id)
        and then No_Return (Spec_Id)
      then
         Check_Returns (HSS, 'P', Missing_Ret, Spec_Id);
      end if;

      --  Now we are going to check for variables that are never modified in
      --  the body of the procedure. We omit these checks if the first
      --  statement of the procedure raises an exception. In particular this
      --  deals with the common idiom of a stubbed function, which might
      --  appear as something like

      --     function F (A : Integer) return Some_Type;
      --        X : Some_Type;
      --     begin
      --        raise Program_Error;
      --        return X;
      --     end F;

      --  Here the purpose of X is simply to satisfy the (annoying)
      --  requirement in Ada that there be at least one return, and we
      --  certainly do not want to go posting warnings on X that it is not
      --  initialized!

      declare
         Stm : Node_Id := First (Statements (HSS));

      begin
         --  Skip an initial label (for one thing this occurs when we are in
         --  front end ZCX mode, but in any case it is irrelevant).

         if Nkind (Stm) = N_Label then
            Next (Stm);
         end if;

         --  Do the test on the original statement before expansion

         declare
            Ostm : constant Node_Id := Original_Node (Stm);

         begin
            --  If explicit raise statement, return with no checks

            if Nkind (Ostm) = N_Raise_Statement then
               return;

            --  Check for explicit call cases which likely raise an exception

            elsif Nkind (Ostm) = N_Procedure_Call_Statement then
               if Is_Entity_Name (Name (Ostm)) then
                  declare
                     Ent : constant Entity_Id := Entity (Name (Ostm));

                  begin
                     --  If the procedure is marked No_Return, then likely it
                     --  raises an exception, but in any case it is not coming
                     --  back here, so no need to check beyond the call.

                     if Ekind (Ent) = E_Procedure
                       and then No_Return (Ent)
                     then
                        return;

                     --  If the procedure name is Raise_Exception, then also
                     --  assume that it raises an exception. The main target
                     --  here is Ada.Exceptions.Raise_Exception, but this name
                     --  is pretty evocative in any context! Note that the
                     --  procedure in Ada.Exceptions is not marked No_Return
                     --  because of the annoying case of the null exception Id.

                     elsif Chars (Ent) = Name_Raise_Exception then
                        return;
                     end if;
                  end;
               end if;
            end if;
         end;
      end;

      --  Check for variables that are never modified

      declare
         E1, E2 : Entity_Id;

      begin
         --  If there is a separate spec, then transfer Never_Set_In_Source
         --  flags from out parameters to the corresponding entities in the
         --  body. The reason we do that is we want to post error flags on
         --  the body entities, not the spec entities.

         if Present (Spec_Id) then
            E1 := First_Entity (Spec_Id);

            while Present (E1) loop
               if Ekind (E1) = E_Out_Parameter then
                  E2 := First_Entity (Body_Id);
                  while Present (E2) loop
                     exit when Chars (E1) = Chars (E2);
                     Next_Entity (E2);
                  end loop;

                  if Present (E2) then
                     Set_Never_Set_In_Source (E2, Never_Set_In_Source (E1));
                  end if;
               end if;

               Next_Entity (E1);
            end loop;
         end if;

         --  Check references in body unless it was deleted. Note that the
         --  check of Body_Deleted here is not just for efficiency, it is
         --  necessary to avoid junk warnings on formal parameters.

         if not Body_Deleted then
            Check_References (Body_Id);
         end if;
      end;
   end Analyze_Subprogram_Body;

   ------------------------------------
   -- Analyze_Subprogram_Declaration --
   ------------------------------------

   procedure Analyze_Subprogram_Declaration (N : Node_Id) is
      Designator : constant Entity_Id :=
                     Analyze_Subprogram_Specification (Specification (N));
      Scop       : constant Entity_Id := Current_Scope;

   --  Start of processing for Analyze_Subprogram_Declaration

   begin
      Generate_Definition (Designator);

      --  Check for RCI unit subprogram declarations against in-lined
      --  subprograms and subprograms having access parameter or limited
      --  parameter without Read and Write (RM E.2.3(12-13)).

      Validate_RCI_Subprogram_Declaration (N);

      Trace_Scope
        (N,
         Defining_Entity (N),
         " Analyze subprogram spec. ");

      if Debug_Flag_C then
         Write_Str ("====  Compiling subprogram spec ");
         Write_Name (Chars (Designator));
         Write_Str (" from ");
         Write_Location (Sloc (N));
         Write_Eol;
      end if;

      New_Overloaded_Entity (Designator);
      Check_Delayed_Subprogram (Designator);

      --  What is the following code for, it used to be

      --  ???   Set_Suppress_Elaboration_Checks
      --  ???     (Designator, Elaboration_Checks_Suppressed (Designator));

      --  The following seems equivalent, but a bit dubious

      if Elaboration_Checks_Suppressed (Designator) then
         Set_Kill_Elaboration_Checks (Designator);
      end if;

      if Scop /= Standard_Standard
        and then not Is_Child_Unit (Designator)
      then
         Set_Categorization_From_Scope (Designator, Scop);
      else
         --  For a compilation unit, check for library-unit pragmas

         New_Scope (Designator);
         Set_Categorization_From_Pragmas (N);
         Validate_Categorization_Dependency (N, Designator);
         Pop_Scope;
      end if;

      --  For a compilation unit, set body required. This flag will only be
      --  reset if a valid Import or Interface pragma is processed later on.

      if Nkind (Parent (N)) = N_Compilation_Unit then
         Set_Body_Required (Parent (N), True);

         if Ada_Version >= Ada_05
           and then Nkind (Specification (N)) = N_Procedure_Specification
           and then Null_Present (Specification (N))
         then
            Error_Msg_N
              ("null procedure cannot be declared at library level", N);
         end if;
      end if;

      Generate_Reference_To_Formals (Designator);
      Check_Eliminated (Designator);

      --  Ada 2005: if procedure is declared with "is null" qualifier,
      --  it requires no body.

      if Nkind (Specification (N)) = N_Procedure_Specification
        and then Null_Present (Specification (N))
      then
         Set_Has_Completion (Designator);
         Set_Is_Inlined (Designator);
      end if;
   end Analyze_Subprogram_Declaration;

   --------------------------------------
   -- Analyze_Subprogram_Specification --
   --------------------------------------

   --  Reminder: N here really is a subprogram specification (not a subprogram
   --  declaration). This procedure is called to analyze the specification in
   --  both subprogram bodies and subprogram declarations (specs).

   function Analyze_Subprogram_Specification (N : Node_Id) return Entity_Id is
      Designator : constant Entity_Id := Defining_Entity (N);
      Formals    : constant List_Id   := Parameter_Specifications (N);

      function Has_Interface_Formals (T : List_Id) return Boolean;
      --  Ada 2005 (AI-251): Returns true if some non class-wide interface
      --  formal is found.

      ---------------------------
      -- Has_Interface_Formals --
      ---------------------------

      function Has_Interface_Formals (T : List_Id) return Boolean is
         Param_Spec : Node_Id;
         Formal     : Entity_Id;

      begin
         Param_Spec := First (T);

         while Present (Param_Spec) loop
            Formal := Defining_Identifier (Param_Spec);

            if Is_Class_Wide_Type (Etype (Formal)) then
               null;

            elsif Is_Interface (Etype (Formal)) then
               return True;
            end if;

            Next (Param_Spec);
         end loop;

         return False;
      end Has_Interface_Formals;

   --  Start of processing for Analyze_Subprogram_Specification

   begin
      Generate_Definition (Designator);

      if Nkind (N) = N_Function_Specification then
         Set_Ekind (Designator, E_Function);
         Set_Mechanism (Designator, Default_Mechanism);

      else
         Set_Ekind (Designator, E_Procedure);
         Set_Etype (Designator, Standard_Void_Type);
      end if;

      --  Introduce new scope for analysis of the formals and of the
      --  return type.

      Set_Scope (Designator, Current_Scope);

      if Present (Formals) then
         New_Scope (Designator);
         Process_Formals (Formals, N);

         --  Ada 2005 (AI-345): Allow overriding primitives of protected
         --  interfaces by means of normal subprograms. For this purpose
         --  temporarily use the corresponding record type as the etype
         --  of the first formal.

         if Ada_Version >= Ada_05
           and then Comes_From_Source (Designator)
           and then Present (First_Entity (Designator))
           and then (Ekind (Etype (First_Entity (Designator)))
                             = E_Protected_Type
                       or else
                     Ekind (Etype (First_Entity (Designator)))
                             = E_Task_Type)
           and then Present (Corresponding_Record_Type
                             (Etype (First_Entity (Designator))))
           and then Present (Abstract_Interfaces
                             (Corresponding_Record_Type
                             (Etype (First_Entity (Designator)))))
         then
            Set_Etype (First_Entity (Designator),
              Corresponding_Record_Type (Etype (First_Entity (Designator))));
         end if;

         End_Scope;

      elsif Nkind (N) = N_Function_Specification then
         Analyze_Return_Type (N);
      end if;

      if Nkind (N) = N_Function_Specification then
         if Nkind (Designator) = N_Defining_Operator_Symbol then
            Valid_Operator_Definition (Designator);
         end if;

         May_Need_Actuals (Designator);

         if Is_Abstract (Etype (Designator))
           and then Nkind (Parent (N))
                      /= N_Abstract_Subprogram_Declaration
           and then (Nkind (Parent (N)))
                      /= N_Formal_Abstract_Subprogram_Declaration
           and then (Nkind (Parent (N)) /= N_Subprogram_Renaming_Declaration
                      or else not Is_Entity_Name (Name (Parent (N)))
                      or else not Is_Abstract (Entity (Name (Parent (N)))))
         then
            Error_Msg_N
              ("function that returns abstract type must be abstract", N);
         end if;
      end if;

      if Ada_Version >= Ada_05
        and then Comes_From_Source (N)
        and then Nkind (Parent (N)) /= N_Abstract_Subprogram_Declaration
        and then (Nkind (N) /= N_Procedure_Specification
                    or else
                  not Null_Present (N))
        and then Has_Interface_Formals (Formals)
      then
         Error_Msg_Name_1 := Chars (Defining_Unit_Name
                                    (Specification (Parent (N))));
         Error_Msg_N
           ("(Ada 2005) interface subprogram % must be abstract or null", N);
      end if;

      return Designator;
   end Analyze_Subprogram_Specification;

   --------------------------
   -- Build_Body_To_Inline --
   --------------------------

   procedure Build_Body_To_Inline (N : Node_Id; Subp : Entity_Id) is
      Decl : constant Node_Id := Unit_Declaration_Node (Subp);
      Original_Body   : Node_Id;
      Body_To_Analyze : Node_Id;
      Max_Size        : constant := 10;
      Stat_Count      : Integer := 0;

      function Has_Excluded_Declaration (Decls : List_Id) return Boolean;
      --  Check for declarations that make inlining not worthwhile

      function Has_Excluded_Statement   (Stats : List_Id) return Boolean;
      --  Check for statements that make inlining not worthwhile: any tasking
      --  statement, nested at any level. Keep track of total number of
      --  elementary statements, as a measure of acceptable size.

      function Has_Pending_Instantiation return Boolean;
      --  If some enclosing body contains instantiations that appear before
      --  the corresponding generic body, the enclosing body has a freeze node
      --  so that it can be elaborated after the generic itself. This might
      --  conflict with subsequent inlinings, so that it is unsafe to try to
      --  inline in such a case.

      function Has_Single_Return return Boolean;
      --  In general we cannot inline functions that return unconstrained
      --  type. However, we can handle such functions if all return statements
      --  return a local variable that is the only declaration in the body
      --  of the function. In that case the call can be replaced by that
      --  local variable as is done for other inlined calls.

      procedure Remove_Pragmas;
      --  A pragma Unreferenced that mentions a formal parameter has no
      --  meaning when the body is inlined and the formals are rewritten.
      --  Remove it from body to inline. The analysis of the non-inlined body
      --  will handle the pragma properly.

      function Uses_Secondary_Stack (Bod : Node_Id) return Boolean;
      --  If the body of the subprogram includes a call that returns an
      --  unconstrained type, the secondary stack is involved, and it
      --  is not worth inlining.

      ------------------------------
      -- Has_Excluded_Declaration --
      ------------------------------

      function Has_Excluded_Declaration (Decls : List_Id) return Boolean is
         D : Node_Id;

         function Is_Unchecked_Conversion (D : Node_Id) return Boolean;
         --  Nested subprograms make a given body ineligible for inlining, but
         --  we make an exception for instantiations of unchecked conversion.
         --  The body has not been analyzed yet, so check the name, and verify
         --  that the visible entity with that name is the predefined unit.

         -----------------------------
         -- Is_Unchecked_Conversion --
         -----------------------------

         function Is_Unchecked_Conversion (D : Node_Id) return Boolean is
            Id   : constant Node_Id := Name (D);
            Conv : Entity_Id;

         begin
            if Nkind (Id) = N_Identifier
              and then Chars (Id) = Name_Unchecked_Conversion
            then
               Conv := Current_Entity (Id);

            elsif (Nkind (Id) = N_Selected_Component
                    or else Nkind (Id) = N_Expanded_Name)
              and then Chars (Selector_Name (Id)) = Name_Unchecked_Conversion
            then
               Conv := Current_Entity (Selector_Name (Id));

            else
               return False;
            end if;

            return Present (Conv)
              and then Is_Predefined_File_Name
                         (Unit_File_Name (Get_Source_Unit (Conv)))
              and then Is_Intrinsic_Subprogram (Conv);
         end Is_Unchecked_Conversion;

      --  Start of processing for Has_Excluded_Declaration

      begin
         D := First (Decls);

         while Present (D) loop
            if       (Nkind (D) = N_Function_Instantiation
                        and then not Is_Unchecked_Conversion (D))
              or else Nkind (D) = N_Protected_Type_Declaration
              or else Nkind (D) = N_Package_Declaration
              or else Nkind (D) = N_Package_Instantiation
              or else Nkind (D) = N_Subprogram_Body
              or else Nkind (D) = N_Procedure_Instantiation
              or else Nkind (D) = N_Task_Type_Declaration
            then
               Cannot_Inline
                 ("cannot inline & (non-allowed declaration)?", D, Subp);
               return True;
            end if;

            Next (D);
         end loop;

         return False;
      end Has_Excluded_Declaration;

      ----------------------------
      -- Has_Excluded_Statement --
      ----------------------------

      function Has_Excluded_Statement (Stats : List_Id) return Boolean is
         S : Node_Id;
         E : Node_Id;

      begin
         S := First (Stats);

         while Present (S) loop
            Stat_Count := Stat_Count + 1;

            if Nkind (S) = N_Abort_Statement
              or else Nkind (S) = N_Asynchronous_Select
              or else Nkind (S) = N_Conditional_Entry_Call
              or else Nkind (S) = N_Delay_Relative_Statement
              or else Nkind (S) = N_Delay_Until_Statement
              or else Nkind (S) = N_Selective_Accept
              or else Nkind (S) = N_Timed_Entry_Call
            then
               Cannot_Inline
                 ("cannot inline & (non-allowed statement)?", S, Subp);
               return True;

            elsif Nkind (S) = N_Block_Statement then
               if Present (Declarations (S))
                 and then Has_Excluded_Declaration (Declarations (S))
               then
                  return True;

               elsif Present (Handled_Statement_Sequence (S))
                  and then
                    (Present
                      (Exception_Handlers (Handled_Statement_Sequence (S)))
                     or else
                       Has_Excluded_Statement
                         (Statements (Handled_Statement_Sequence (S))))
               then
                  return True;
               end if;

            elsif Nkind (S) = N_Case_Statement then
               E := First (Alternatives (S));
               while Present (E) loop
                  if Has_Excluded_Statement (Statements (E)) then
                     return True;
                  end if;

                  Next (E);
               end loop;

            elsif Nkind (S) = N_If_Statement then
               if Has_Excluded_Statement (Then_Statements (S)) then
                  return True;
               end if;

               if Present (Elsif_Parts (S)) then
                  E := First (Elsif_Parts (S));
                  while Present (E) loop
                     if Has_Excluded_Statement (Then_Statements (E)) then
                        return True;
                     end if;
                     Next (E);
                  end loop;
               end if;

               if Present (Else_Statements (S))
                 and then Has_Excluded_Statement (Else_Statements (S))
               then
                  return True;
               end if;

            elsif Nkind (S) = N_Loop_Statement
              and then Has_Excluded_Statement (Statements (S))
            then
               return True;
            end if;

            Next (S);
         end loop;

         return False;
      end Has_Excluded_Statement;

      -------------------------------
      -- Has_Pending_Instantiation --
      -------------------------------

      function Has_Pending_Instantiation return Boolean is
         S : Entity_Id := Current_Scope;

      begin
         while Present (S) loop
            if Is_Compilation_Unit (S)
              or else Is_Child_Unit (S)
            then
               return False;
            elsif Ekind (S) = E_Package
              and then Has_Forward_Instantiation (S)
            then
               return True;
            end if;

            S := Scope (S);
         end loop;

         return False;
      end Has_Pending_Instantiation;

      ------------------------
      --  Has_Single_Return --
      ------------------------

      function Has_Single_Return return Boolean is
         Return_Statement : Node_Id := Empty;

         function Check_Return (N : Node_Id) return Traverse_Result;

         ------------------
         -- Check_Return --
         ------------------

         function Check_Return (N : Node_Id) return Traverse_Result is
         begin
            if Nkind (N) = N_Return_Statement then
               if Present (Expression (N))
                 and then Is_Entity_Name (Expression (N))
               then
                  if No (Return_Statement) then
                     Return_Statement := N;
                     return OK;

                  elsif Chars (Expression (N)) =
                        Chars (Expression (Return_Statement))
                  then
                     return OK;

                  else
                     return Abandon;
                  end if;

               else
                  --  Expression has wrong form

                  return Abandon;
               end if;

            else
               return OK;
            end if;
         end Check_Return;

         function Check_All_Returns is new Traverse_Func (Check_Return);

      --  Start of processing for Has_Single_Return

      begin
         return Check_All_Returns (N) = OK
           and then Present (Declarations (N))
           and then Chars (Expression (Return_Statement)) =
                    Chars (Defining_Identifier (First (Declarations (N))));
      end Has_Single_Return;

      --------------------
      -- Remove_Pragmas --
      --------------------

      procedure Remove_Pragmas is
         Decl : Node_Id;
         Nxt  : Node_Id;

      begin
         Decl := First (Declarations (Body_To_Analyze));
         while Present (Decl) loop
            Nxt := Next (Decl);

            if Nkind (Decl) = N_Pragma
              and then Chars (Decl) = Name_Unreferenced
            then
               Remove (Decl);
            end if;

            Decl := Nxt;
         end loop;
      end Remove_Pragmas;

      --------------------------
      -- Uses_Secondary_Stack --
      --------------------------

      function Uses_Secondary_Stack (Bod : Node_Id) return Boolean is
         function Check_Call (N : Node_Id) return Traverse_Result;
         --  Look for function calls that return an unconstrained type

         ----------------
         -- Check_Call --
         ----------------

         function Check_Call (N : Node_Id) return Traverse_Result is
         begin
            if Nkind (N) = N_Function_Call
              and then Is_Entity_Name (Name (N))
              and then Is_Composite_Type (Etype (Entity (Name (N))))
              and then not Is_Constrained (Etype (Entity (Name (N))))
            then
               Cannot_Inline
                 ("cannot inline & (call returns unconstrained type)?",
                    N, Subp);
               return Abandon;
            else
               return OK;
            end if;
         end Check_Call;

         function Check_Calls is new Traverse_Func (Check_Call);

      begin
         return Check_Calls (Bod) = Abandon;
      end Uses_Secondary_Stack;

   --  Start of processing for Build_Body_To_Inline

   begin
      if Nkind (Decl) = N_Subprogram_Declaration
        and then Present (Body_To_Inline (Decl))
      then
         return;    --  Done already.

      --  Functions that return unconstrained composite types require
      --  secondary stack handling, and cannot currently be inlined, unless
      --  all return statements return a local variable that is the first
      --  local declaration in the body.

      elsif Ekind (Subp) = E_Function
        and then not Is_Scalar_Type (Etype (Subp))
        and then not Is_Access_Type (Etype (Subp))
        and then not Is_Constrained (Etype (Subp))
      then
         if not Has_Single_Return then
            Cannot_Inline
              ("cannot inline & (unconstrained return type)?", N, Subp);
            return;
         end if;

      --  Ditto for functions that return controlled types, where controlled
      --  actions interfere in complex ways with inlining.

      elsif Ekind (Subp) = E_Function
        and then Controlled_Type (Etype (Subp))
      then
         Cannot_Inline
           ("cannot inline & (controlled return type)?", N, Subp);
         return;
      end if;

      if Present (Declarations (N))
        and then Has_Excluded_Declaration (Declarations (N))
      then
         return;
      end if;

      if Present (Handled_Statement_Sequence (N)) then
         if Present (Exception_Handlers (Handled_Statement_Sequence (N))) then
            Cannot_Inline
              ("cannot inline& (exception handler)?",
               First (Exception_Handlers (Handled_Statement_Sequence (N))),
               Subp);
            return;
         elsif
           Has_Excluded_Statement
             (Statements (Handled_Statement_Sequence (N)))
         then
            return;
         end if;
      end if;

      --  We do not inline a subprogram  that is too large, unless it is
      --  marked Inline_Always. This pragma does not suppress the other
      --  checks on inlining (forbidden declarations, handlers, etc).

      if Stat_Count > Max_Size
        and then not Is_Always_Inlined (Subp)
      then
         Cannot_Inline ("cannot inline& (body too large)?", N, Subp);
         return;
      end if;

      if Has_Pending_Instantiation then
         Cannot_Inline
           ("cannot inline& (forward instance within enclosing body)?",
             N, Subp);
         return;
      end if;

      --  Within an instance, the body to inline must be treated as a nested
      --  generic, so that the proper global references are preserved.

      if In_Instance then
         Save_Env (Scope (Current_Scope), Scope (Current_Scope));
         Original_Body := Copy_Generic_Node (N, Empty, True);
      else
         Original_Body := Copy_Separate_Tree (N);
      end if;

      --  We need to capture references to the formals in order to substitute
      --  the actuals at the point of inlining, i.e. instantiation. To treat
      --  the formals as globals to the body to inline, we nest it within
      --  a dummy parameterless subprogram, declared within the real one.
      --  To avoid generating an internal name (which is never public, and
      --  which affects serial numbers of other generated names), we use
      --  an internal symbol that cannot conflict with user declarations.

      Set_Parameter_Specifications (Specification (Original_Body), No_List);
      Set_Defining_Unit_Name
        (Specification (Original_Body),
          Make_Defining_Identifier (Sloc (N), Name_uParent));
      Set_Corresponding_Spec (Original_Body, Empty);

      Body_To_Analyze := Copy_Generic_Node (Original_Body, Empty, False);

      --  Set return type of function, which is also global and does not need
      --  to be resolved.

      if Ekind (Subp) = E_Function then
         Set_Result_Definition (Specification (Body_To_Analyze),
           New_Occurrence_Of (Etype (Subp), Sloc (N)));
      end if;

      if No (Declarations (N)) then
         Set_Declarations (N, New_List (Body_To_Analyze));
      else
         Append (Body_To_Analyze, Declarations (N));
      end if;

      Expander_Mode_Save_And_Set (False);
      Remove_Pragmas;

      Analyze (Body_To_Analyze);
      New_Scope (Defining_Entity (Body_To_Analyze));
      Save_Global_References (Original_Body);
      End_Scope;
      Remove (Body_To_Analyze);

      Expander_Mode_Restore;

      if In_Instance then
         Restore_Env;
      end if;

      --  If secondary stk used there is no point in inlining. We have
      --  already issued the warning in this case, so nothing to do.

      if Uses_Secondary_Stack (Body_To_Analyze) then
         return;
      end if;

      Set_Body_To_Inline (Decl, Original_Body);
      Set_Ekind (Defining_Entity (Original_Body), Ekind (Subp));
      Set_Is_Inlined (Subp);
   end Build_Body_To_Inline;

   -------------------
   -- Cannot_Inline --
   -------------------

   procedure Cannot_Inline (Msg : String; N : Node_Id; Subp : Entity_Id) is
   begin
      --  Do not emit warning if this is a predefined unit which is not
      --  the main unit. With validity checks enabled, some predefined
      --  subprograms may contain nested subprograms and become ineligible
      --  for inlining.

      if Is_Predefined_File_Name (Unit_File_Name (Get_Source_Unit (Subp)))
        and then not In_Extended_Main_Source_Unit (Subp)
      then
         null;

      elsif Is_Always_Inlined (Subp) then

         --  Remove last character (question mark) to make this into an error,
         --  because the Inline_Always pragma cannot be obeyed.

         Error_Msg_NE (Msg (1 .. Msg'Length - 1), N, Subp);

      elsif Ineffective_Inline_Warnings then
         Error_Msg_NE (Msg, N, Subp);
      end if;
   end Cannot_Inline;

   -----------------------
   -- Check_Conformance --
   -----------------------

   procedure Check_Conformance
     (New_Id                   : Entity_Id;
      Old_Id                   : Entity_Id;
      Ctype                    : Conformance_Type;
      Errmsg                   : Boolean;
      Conforms                 : out Boolean;
      Err_Loc                  : Node_Id := Empty;
      Get_Inst                 : Boolean := False;
      Skip_Controlling_Formals : Boolean := False)
   is
      Old_Type   : constant Entity_Id := Etype (Old_Id);
      New_Type   : constant Entity_Id := Etype (New_Id);
      Old_Formal : Entity_Id;
      New_Formal : Entity_Id;

      procedure Conformance_Error (Msg : String; N : Node_Id := New_Id);
      --  Post error message for conformance error on given node. Two messages
      --  are output. The first points to the previous declaration with a
      --  general "no conformance" message. The second is the detailed reason,
      --  supplied as Msg. The parameter N provide information for a possible
      --  & insertion in the message, and also provides the location for
      --  posting the message in the absence of a specified Err_Loc location.

      -----------------------
      -- Conformance_Error --
      -----------------------

      procedure Conformance_Error (Msg : String; N : Node_Id := New_Id) is
         Enode : Node_Id;

      begin
         Conforms := False;

         if Errmsg then
            if No (Err_Loc) then
               Enode := N;
            else
               Enode := Err_Loc;
            end if;

            Error_Msg_Sloc := Sloc (Old_Id);

            case Ctype is
               when Type_Conformant =>
                  Error_Msg_N
                    ("not type conformant with declaration#!", Enode);

               when Mode_Conformant =>
                  Error_Msg_N
                    ("not mode conformant with declaration#!", Enode);

               when Subtype_Conformant =>
                  Error_Msg_N
                    ("not subtype conformant with declaration#!", Enode);

               when Fully_Conformant =>
                  Error_Msg_N
                    ("not fully conformant with declaration#!", Enode);
            end case;

            Error_Msg_NE (Msg, Enode, N);
         end if;
      end Conformance_Error;

   --  Start of processing for Check_Conformance

   begin
      Conforms := True;

      --  We need a special case for operators, since they don't appear
      --  explicitly.

      if Ctype = Type_Conformant then
         if Ekind (New_Id) = E_Operator
           and then Operator_Matches_Spec (New_Id, Old_Id)
         then
            return;
         end if;
      end if;

      --  If both are functions/operators, check return types conform

      if Old_Type /= Standard_Void_Type
        and then New_Type /= Standard_Void_Type
      then
         if not Conforming_Types (Old_Type, New_Type, Ctype, Get_Inst) then
            Conformance_Error ("return type does not match!", New_Id);
            return;
         end if;

         --  Ada 2005 (AI-231): In case of anonymous access types check the
         --  null-exclusion and access-to-constant attributes must match.

         if Ada_Version >= Ada_05
           and then Ekind (Etype (Old_Type)) = E_Anonymous_Access_Type
           and then
             (Can_Never_Be_Null (Old_Type)
                /= Can_Never_Be_Null (New_Type)
              or else Is_Access_Constant (Etype (Old_Type))
                        /= Is_Access_Constant (Etype (New_Type)))
         then
            Conformance_Error ("return type does not match!", New_Id);
            return;
         end if;

      --  If either is a function/operator and the other isn't, error

      elsif Old_Type /= Standard_Void_Type
        or else New_Type /= Standard_Void_Type
      then
         Conformance_Error ("functions can only match functions!", New_Id);
         return;
      end if;

      --  In subtype conformant case, conventions must match (RM 6.3.1(16))
      --  If this is a renaming as body, refine error message to indicate that
      --  the conflict is with the original declaration. If the entity is not
      --  frozen, the conventions don't have to match, the one of the renamed
      --  entity is inherited.

      if Ctype >= Subtype_Conformant then
         if Convention (Old_Id) /= Convention (New_Id) then

            if not Is_Frozen (New_Id) then
               null;

            elsif Present (Err_Loc)
              and then Nkind (Err_Loc) = N_Subprogram_Renaming_Declaration
              and then Present (Corresponding_Spec (Err_Loc))
            then
               Error_Msg_Name_1 := Chars (New_Id);
               Error_Msg_Name_2 :=
                 Name_Ada + Convention_Id'Pos (Convention (New_Id));

               Conformance_Error ("prior declaration for% has convention %!");

            else
               Conformance_Error ("calling conventions do not match!");
            end if;

            return;

         elsif Is_Formal_Subprogram (Old_Id)
           or else Is_Formal_Subprogram (New_Id)
         then
            Conformance_Error ("formal subprograms not allowed!");
            return;
         end if;
      end if;

      --  Deal with parameters

      --  Note: we use the entity information, rather than going directly
      --  to the specification in the tree. This is not only simpler, but
      --  absolutely necessary for some cases of conformance tests between
      --  operators, where the declaration tree simply does not exist!

      Old_Formal := First_Formal (Old_Id);
      New_Formal := First_Formal (New_Id);

      while Present (Old_Formal) and then Present (New_Formal) loop
         if Is_Controlling_Formal (Old_Formal)
           and then Is_Controlling_Formal (New_Formal)
           and then Skip_Controlling_Formals
         then
            goto Skip_Controlling_Formal;
         end if;

         if Ctype = Fully_Conformant then

            --  Names must match. Error message is more accurate if we do
            --  this before checking that the types of the formals match.

            if Chars (Old_Formal) /= Chars (New_Formal) then
               Conformance_Error ("name & does not match!", New_Formal);

               --  Set error posted flag on new formal as well to stop
               --  junk cascaded messages in some cases.

               Set_Error_Posted (New_Formal);
               return;
            end if;
         end if;

         --  Types must always match. In the visible part of an instance,
         --  usual overloading rules for dispatching operations apply, and
         --  we check base types (not the actual subtypes).

         if In_Instance_Visible_Part
           and then Is_Dispatching_Operation (New_Id)
         then
            if not Conforming_Types
              (Base_Type (Etype (Old_Formal)),
                 Base_Type (Etype (New_Formal)), Ctype, Get_Inst)
            then
               Conformance_Error ("type of & does not match!", New_Formal);
               return;
            end if;

         elsif not Conforming_Types
           (Etype (Old_Formal), Etype (New_Formal), Ctype, Get_Inst)
         then
            Conformance_Error ("type of & does not match!", New_Formal);
            return;
         end if;

         --  For mode conformance, mode must match

         if Ctype >= Mode_Conformant
           and then Parameter_Mode (Old_Formal) /= Parameter_Mode (New_Formal)
         then
            Conformance_Error ("mode of & does not match!", New_Formal);
            return;
         end if;

         --  Full conformance checks

         if Ctype = Fully_Conformant then

            --  We have checked already that names match

            if Parameter_Mode (Old_Formal) = E_In_Parameter then

               --  Ada 2005 (AI-231): In case of anonymous access types check
               --  the null-exclusion and access-to-constant attributes must
               --  match.

               if Ada_Version >= Ada_05
                 and then Ekind (Etype (Old_Formal)) = E_Anonymous_Access_Type
                 and then
                   (Can_Never_Be_Null (Old_Formal)
                      /= Can_Never_Be_Null (New_Formal)
                    or else Is_Access_Constant (Etype (Old_Formal))
                              /= Is_Access_Constant (Etype (New_Formal)))
               then
                  --  It is allowed to omit the null-exclusion in case of
                  --  stream attribute subprograms

                  declare
                     TSS_Name : TSS_Name_Type;

                  begin
                     Get_Name_String (Chars (New_Id));
                     TSS_Name :=
                       TSS_Name_Type
                         (Name_Buffer
                            (Name_Len - TSS_Name'Length + 1 .. Name_Len));

                     if TSS_Name /= TSS_Stream_Read
                       and then TSS_Name /= TSS_Stream_Write
                       and then TSS_Name /= TSS_Stream_Input
                       and then TSS_Name /= TSS_Stream_Output
                     then
                        Conformance_Error
                          ("type of & does not match!", New_Formal);
                        return;
                     end if;
                  end;
               end if;

               --  Check default expressions for in parameters

               declare
                  NewD : constant Boolean :=
                           Present (Default_Value (New_Formal));
                  OldD : constant Boolean :=
                           Present (Default_Value (Old_Formal));
               begin
                  if NewD or OldD then

                     --  The old default value has been analyzed because the
                     --  current full declaration will have frozen everything
                     --  before. The new default values have not been
                     --  analyzed, so analyze them now before we check for
                     --  conformance.

                     if NewD then
                        New_Scope (New_Id);
                        Analyze_Per_Use_Expression
                          (Default_Value (New_Formal), Etype (New_Formal));
                        End_Scope;
                     end if;

                     if not (NewD and OldD)
                       or else not Fully_Conformant_Expressions
                                    (Default_Value (Old_Formal),
                                     Default_Value (New_Formal))
                     then
                        Conformance_Error
                          ("default expression for & does not match!",
                           New_Formal);
                        return;
                     end if;
                  end if;
               end;
            end if;
         end if;

         --  A couple of special checks for Ada 83 mode. These checks are
         --  skipped if either entity is an operator in package Standard.
         --  or if either old or new instance is not from the source program.

         if Ada_Version = Ada_83
           and then Sloc (Old_Id) > Standard_Location
           and then Sloc (New_Id) > Standard_Location
           and then Comes_From_Source (Old_Id)
           and then Comes_From_Source (New_Id)
         then
            declare
               Old_Param : constant Node_Id := Declaration_Node (Old_Formal);
               New_Param : constant Node_Id := Declaration_Node (New_Formal);

            begin
               --  Explicit IN must be present or absent in both cases. This
               --  test is required only in the full conformance case.

               if In_Present (Old_Param) /= In_Present (New_Param)
                 and then Ctype = Fully_Conformant
               then
                  Conformance_Error
                    ("(Ada 83) IN must appear in both declarations",
                     New_Formal);
                  return;
               end if;

               --  Grouping (use of comma in param lists) must be the same
               --  This is where we catch a misconformance like:

               --    A,B : Integer
               --    A : Integer; B : Integer

               --  which are represented identically in the tree except
               --  for the setting of the flags More_Ids and Prev_Ids.

               if More_Ids (Old_Param) /= More_Ids (New_Param)
                 or else Prev_Ids (Old_Param) /= Prev_Ids (New_Param)
               then
                  Conformance_Error
                    ("grouping of & does not match!", New_Formal);
                  return;
               end if;
            end;
         end if;

         --  This label is required when skipping controlling formals

         <<Skip_Controlling_Formal>>

         Next_Formal (Old_Formal);
         Next_Formal (New_Formal);
      end loop;

      if Present (Old_Formal) then
         Conformance_Error ("too few parameters!");
         return;

      elsif Present (New_Formal) then
         Conformance_Error ("too many parameters!", New_Formal);
         return;
      end if;
   end Check_Conformance;

   ------------------------------
   -- Check_Delayed_Subprogram --
   ------------------------------

   procedure Check_Delayed_Subprogram (Designator : Entity_Id) is
      F : Entity_Id;

      procedure Possible_Freeze (T : Entity_Id);
      --  T is the type of either a formal parameter or of the return type.
      --  If T is not yet frozen and needs a delayed freeze, then the
      --  subprogram itself must be delayed.

      ---------------------
      -- Possible_Freeze --
      ---------------------

      procedure Possible_Freeze (T : Entity_Id) is
      begin
         if Has_Delayed_Freeze (T)
           and then not Is_Frozen (T)
         then
            Set_Has_Delayed_Freeze (Designator);

         elsif Is_Access_Type (T)
           and then Has_Delayed_Freeze (Designated_Type (T))
           and then not Is_Frozen (Designated_Type (T))
         then
            Set_Has_Delayed_Freeze (Designator);
         end if;
      end Possible_Freeze;

   --  Start of processing for Check_Delayed_Subprogram

   begin
      --  Never need to freeze abstract subprogram

      if Is_Abstract (Designator) then
         null;
      else
         --  Need delayed freeze if return type itself needs a delayed
         --  freeze and is not yet frozen.

         Possible_Freeze (Etype (Designator));
         Possible_Freeze (Base_Type (Etype (Designator))); -- needed ???

         --  Need delayed freeze if any of the formal types themselves need
         --  a delayed freeze and are not yet frozen.

         F := First_Formal (Designator);
         while Present (F) loop
            Possible_Freeze (Etype (F));
            Possible_Freeze (Base_Type (Etype (F))); -- needed ???
            Next_Formal (F);
         end loop;
      end if;

      --  Mark functions that return by reference. Note that it cannot be
      --  done for delayed_freeze subprograms because the underlying
      --  returned type may not be known yet (for private types)

      if not Has_Delayed_Freeze (Designator)
        and then Expander_Active
      then
         declare
            Typ  : constant Entity_Id := Etype (Designator);
            Utyp : constant Entity_Id := Underlying_Type (Typ);

         begin
            if Is_Return_By_Reference_Type (Typ) then
               Set_Returns_By_Ref (Designator);

            elsif Present (Utyp) and then Controlled_Type (Utyp) then
               Set_Returns_By_Ref (Designator);
            end if;
         end;
      end if;
   end Check_Delayed_Subprogram;

   ------------------------------------
   -- Check_Discriminant_Conformance --
   ------------------------------------

   procedure Check_Discriminant_Conformance
     (N        : Node_Id;
      Prev     : Entity_Id;
      Prev_Loc : Node_Id)
   is
      Old_Discr      : Entity_Id := First_Discriminant (Prev);
      New_Discr      : Node_Id   := First (Discriminant_Specifications (N));
      New_Discr_Id   : Entity_Id;
      New_Discr_Type : Entity_Id;

      procedure Conformance_Error (Msg : String; N : Node_Id);
      --  Post error message for conformance error on given node. Two messages
      --  are output. The first points to the previous declaration with a
      --  general "no conformance" message. The second is the detailed reason,
      --  supplied as Msg. The parameter N provide information for a possible
      --  & insertion in the message.

      -----------------------
      -- Conformance_Error --
      -----------------------

      procedure Conformance_Error (Msg : String; N : Node_Id) is
      begin
         Error_Msg_Sloc := Sloc (Prev_Loc);
         Error_Msg_N ("not fully conformant with declaration#!", N);
         Error_Msg_NE (Msg, N, N);
      end Conformance_Error;

   --  Start of processing for Check_Discriminant_Conformance

   begin
      while Present (Old_Discr) and then Present (New_Discr) loop

         New_Discr_Id := Defining_Identifier (New_Discr);

         --  The subtype mark of the discriminant on the full type has not
         --  been analyzed so we do it here. For an access discriminant a new
         --  type is created.

         if Nkind (Discriminant_Type (New_Discr)) = N_Access_Definition then
            New_Discr_Type :=
              Access_Definition (N, Discriminant_Type (New_Discr));

         else
            Analyze (Discriminant_Type (New_Discr));
            New_Discr_Type := Etype (Discriminant_Type (New_Discr));
         end if;

         if not Conforming_Types
                  (Etype (Old_Discr), New_Discr_Type, Fully_Conformant)
         then
            Conformance_Error ("type of & does not match!", New_Discr_Id);
            return;
         else
            --  Treat the new discriminant as an occurrence of the old one,
            --  for navigation purposes, and fill in some semantic
            --  information, for completeness.

            Generate_Reference (Old_Discr, New_Discr_Id, 'r');
            Set_Etype (New_Discr_Id, Etype (Old_Discr));
            Set_Scope (New_Discr_Id, Scope (Old_Discr));
         end if;

         --  Names must match

         if Chars (Old_Discr) /= Chars (Defining_Identifier (New_Discr)) then
            Conformance_Error ("name & does not match!", New_Discr_Id);
            return;
         end if;

         --  Default expressions must match

         declare
            NewD : constant Boolean :=
                     Present (Expression (New_Discr));
            OldD : constant Boolean :=
                     Present (Expression (Parent (Old_Discr)));

         begin
            if NewD or OldD then

               --  The old default value has been analyzed and expanded,
               --  because the current full declaration will have frozen
               --  everything before. The new default values have not been
               --  expanded, so expand now to check conformance.

               if NewD then
                  Analyze_Per_Use_Expression
                    (Expression (New_Discr), New_Discr_Type);
               end if;

               if not (NewD and OldD)
                 or else not Fully_Conformant_Expressions
                              (Expression (Parent (Old_Discr)),
                               Expression (New_Discr))

               then
                  Conformance_Error
                    ("default expression for & does not match!",
                     New_Discr_Id);
                  return;
               end if;
            end if;
         end;

         --  In Ada 83 case, grouping must match: (A,B : X) /= (A : X; B : X)

         if Ada_Version = Ada_83 then
            declare
               Old_Disc : constant Node_Id := Declaration_Node (Old_Discr);

            begin
               --  Grouping (use of comma in param lists) must be the same
               --  This is where we catch a misconformance like:

               --    A,B : Integer
               --    A : Integer; B : Integer

               --  which are represented identically in the tree except
               --  for the setting of the flags More_Ids and Prev_Ids.

               if More_Ids (Old_Disc) /= More_Ids (New_Discr)
                 or else Prev_Ids (Old_Disc) /= Prev_Ids (New_Discr)
               then
                  Conformance_Error
                    ("grouping of & does not match!", New_Discr_Id);
                  return;
               end if;
            end;
         end if;

         Next_Discriminant (Old_Discr);
         Next (New_Discr);
      end loop;

      if Present (Old_Discr) then
         Conformance_Error ("too few discriminants!", Defining_Identifier (N));
         return;

      elsif Present (New_Discr) then
         Conformance_Error
           ("too many discriminants!", Defining_Identifier (New_Discr));
         return;
      end if;
   end Check_Discriminant_Conformance;

   ----------------------------
   -- Check_Fully_Conformant --
   ----------------------------

   procedure Check_Fully_Conformant
     (New_Id  : Entity_Id;
      Old_Id  : Entity_Id;
      Err_Loc : Node_Id := Empty)
   is
      Result : Boolean;
   begin
      Check_Conformance
        (New_Id, Old_Id, Fully_Conformant, True, Result, Err_Loc);
   end Check_Fully_Conformant;

   ---------------------------
   -- Check_Mode_Conformant --
   ---------------------------

   procedure Check_Mode_Conformant
     (New_Id   : Entity_Id;
      Old_Id   : Entity_Id;
      Err_Loc  : Node_Id := Empty;
      Get_Inst : Boolean := False)
   is
      Result : Boolean;

   begin
      Check_Conformance
        (New_Id, Old_Id, Mode_Conformant, True, Result, Err_Loc, Get_Inst);
   end Check_Mode_Conformant;

   --------------------------------
   -- Check_Overriding_Indicator --
   --------------------------------

   procedure Check_Overriding_Indicator
     (Subp          : Entity_Id;
      Does_Override : Boolean)
   is
      Decl : Node_Id;
      Spec : Node_Id;

   begin
      if Ekind (Subp) = E_Enumeration_Literal then

         --  No overriding indicator for literals

         return;

      else
         Decl := Unit_Declaration_Node (Subp);
      end if;

      if Nkind (Decl) = N_Subprogram_Declaration
        or else Nkind (Decl) = N_Subprogram_Body
        or else Nkind (Decl) = N_Subprogram_Renaming_Declaration
        or else Nkind (Decl) = N_Subprogram_Body_Stub
      then
         Spec := Specification (Decl);
      else
         return;
      end if;

      if not Does_Override then
         if Must_Override (Spec) then
            Error_Msg_NE ("subprogram& is not overriding", Spec, Subp);
         end if;

      else
         if Must_Not_Override (Spec) then
            Error_Msg_NE
              ("subprogram& overrides inherited operation", Spec, Subp);
         end if;
      end if;
   end Check_Overriding_Indicator;

   -------------------
   -- Check_Returns --
   -------------------

   procedure Check_Returns
     (HSS  : Node_Id;
      Mode : Character;
      Err  : out Boolean;
      Proc : Entity_Id := Empty)
   is
      Handler : Node_Id;

      procedure Check_Statement_Sequence (L : List_Id);
      --  Internal recursive procedure to check a list of statements for proper
      --  termination by a return statement (or a transfer of control or a
      --  compound statement that is itself internally properly terminated).

      ------------------------------
      -- Check_Statement_Sequence --
      ------------------------------

      procedure Check_Statement_Sequence (L : List_Id) is
         Last_Stm : Node_Id;
         Kind     : Node_Kind;

         Raise_Exception_Call : Boolean;
         --  Set True if statement sequence terminated by Raise_Exception call
         --  or a Reraise_Occurrence call.

      begin
         Raise_Exception_Call := False;

         --  Get last real statement

         Last_Stm := Last (L);

         --  Don't count pragmas

         while Nkind (Last_Stm) = N_Pragma

         --  Don't count call to SS_Release (can happen after Raise_Exception)

           or else
             (Nkind (Last_Stm) = N_Procedure_Call_Statement
                and then
              Nkind (Name (Last_Stm)) = N_Identifier
                and then
              Is_RTE (Entity (Name (Last_Stm)), RE_SS_Release))

         --  Don't count exception junk

           or else
             ((Nkind (Last_Stm) = N_Goto_Statement
                 or else Nkind (Last_Stm) = N_Label
                 or else Nkind (Last_Stm) = N_Object_Declaration)
               and then Exception_Junk (Last_Stm))
         loop
            Prev (Last_Stm);
         end loop;

         --  Here we have the "real" last statement

         Kind := Nkind (Last_Stm);

         --  Transfer of control, OK. Note that in the No_Return procedure
         --  case, we already diagnosed any explicit return statements, so
         --  we can treat them as OK in this context.

         if Is_Transfer (Last_Stm) then
            return;

         --  Check cases of explicit non-indirect procedure calls

         elsif Kind = N_Procedure_Call_Statement
           and then Is_Entity_Name (Name (Last_Stm))
         then
            --  Check call to Raise_Exception procedure which is treated
            --  specially, as is a call to Reraise_Occurrence.

            --  We suppress the warning in these cases since it is likely that
            --  the programmer really does not expect to deal with the case
            --  of Null_Occurrence, and thus would find a warning about a
            --  missing return curious, and raising Program_Error does not
            --  seem such a bad behavior if this does occur.

            --  Note that in the Ada 2005 case for Raise_Exception, the actual
            --  behavior will be to raise Constraint_Error (see AI-329).

            if Is_RTE (Entity (Name (Last_Stm)), RE_Raise_Exception)
                 or else
               Is_RTE (Entity (Name (Last_Stm)), RE_Reraise_Occurrence)
            then
               Raise_Exception_Call := True;

               --  For Raise_Exception call, test first argument, if it is
               --  an attribute reference for a 'Identity call, then we know
               --  that the call cannot possibly return.

               declare
                  Arg : constant Node_Id :=
                          Original_Node (First_Actual (Last_Stm));

               begin
                  if Nkind (Arg) = N_Attribute_Reference
                    and then Attribute_Name (Arg) = Name_Identity
                  then
                     return;
                  end if;
               end;
            end if;

         --  If statement, need to look inside if there is an else and check
         --  each constituent statement sequence for proper termination.

         elsif Kind = N_If_Statement
           and then Present (Else_Statements (Last_Stm))
         then
            Check_Statement_Sequence (Then_Statements (Last_Stm));
            Check_Statement_Sequence (Else_Statements (Last_Stm));

            if Present (Elsif_Parts (Last_Stm)) then
               declare
                  Elsif_Part : Node_Id := First (Elsif_Parts (Last_Stm));

               begin
                  while Present (Elsif_Part) loop
                     Check_Statement_Sequence (Then_Statements (Elsif_Part));
                     Next (Elsif_Part);
                  end loop;
               end;
            end if;

            return;

         --  Case statement, check each case for proper termination

         elsif Kind = N_Case_Statement then
            declare
               Case_Alt : Node_Id;

            begin
               Case_Alt := First_Non_Pragma (Alternatives (Last_Stm));
               while Present (Case_Alt) loop
                  Check_Statement_Sequence (Statements (Case_Alt));
                  Next_Non_Pragma (Case_Alt);
               end loop;
            end;

            return;

         --  Block statement, check its handled sequence of statements

         elsif Kind = N_Block_Statement then
            declare
               Err1 : Boolean;

            begin
               Check_Returns
                 (Handled_Statement_Sequence (Last_Stm), Mode, Err1);

               if Err1 then
                  Err := True;
               end if;

               return;
            end;

         --  Loop statement. If there is an iteration scheme, we can definitely
         --  fall out of the loop. Similarly if there is an exit statement, we
         --  can fall out. In either case we need a following return.

         elsif Kind = N_Loop_Statement then
            if Present (Iteration_Scheme (Last_Stm))
              or else Has_Exit (Entity (Identifier (Last_Stm)))
            then
               null;

            --  A loop with no exit statement or iteration scheme if either
            --  an inifite loop, or it has some other exit (raise/return).
            --  In either case, no warning is required.

            else
               return;
            end if;

         --  Timed entry call, check entry call and delay alternatives

         --  Note: in expanded code, the timed entry call has been converted
         --  to a set of expanded statements on which the check will work
         --  correctly in any case.

         elsif Kind = N_Timed_Entry_Call then
            declare
               ECA : constant Node_Id := Entry_Call_Alternative (Last_Stm);
               DCA : constant Node_Id := Delay_Alternative      (Last_Stm);

            begin
               --  If statement sequence of entry call alternative is missing,
               --  then we can definitely fall through, and we post the error
               --  message on the entry call alternative itself.

               if No (Statements (ECA)) then
                  Last_Stm := ECA;

               --  If statement sequence of delay alternative is missing, then
               --  we can definitely fall through, and we post the error
               --  message on the delay alternative itself.

               --  Note: if both ECA and DCA are missing the return, then we
               --  post only one message, should be enough to fix the bugs.
               --  If not we will get a message next time on the DCA when the
               --  ECA is fixed!

               elsif No (Statements (DCA)) then
                  Last_Stm := DCA;

               --  Else check both statement sequences

               else
                  Check_Statement_Sequence (Statements (ECA));
                  Check_Statement_Sequence (Statements (DCA));
                  return;
               end if;
            end;

         --  Conditional entry call, check entry call and else part

         --  Note: in expanded code, the conditional entry call has been
         --  converted to a set of expanded statements on which the check
         --  will work correctly in any case.

         elsif Kind = N_Conditional_Entry_Call then
            declare
               ECA : constant Node_Id := Entry_Call_Alternative (Last_Stm);

            begin
               --  If statement sequence of entry call alternative is missing,
               --  then we can definitely fall through, and we post the error
               --  message on the entry call alternative itself.

               if No (Statements (ECA)) then
                  Last_Stm := ECA;

               --  Else check statement sequence and else part

               else
                  Check_Statement_Sequence (Statements (ECA));
                  Check_Statement_Sequence (Else_Statements (Last_Stm));
                  return;
               end if;
            end;
         end if;

         --  If we fall through, issue appropriate message

         if Mode = 'F' then
            if not Raise_Exception_Call then
               Error_Msg_N
                 ("?RETURN statement missing following this statement",
                  Last_Stm);
               Error_Msg_N
                 ("\?Program_Error may be raised at run time",
                  Last_Stm);
            end if;

            --  Note: we set Err even though we have not issued a warning
            --  because we still have a case of a missing return. This is
            --  an extremely marginal case, probably will never be noticed
            --  but we might as well get it right.

            Err := True;

         --  Otherwise we have the case of a procedure marked No_Return

         else
            Error_Msg_N
              ("?implied return after this statement will raise Program_Error",
               Last_Stm);
            Error_Msg_NE
              ("?procedure & is marked as No_Return",
               Last_Stm, Proc);

            declare
               RE : constant Node_Id :=
                      Make_Raise_Program_Error (Sloc (Last_Stm),
                        Reason => PE_Implicit_Return);
            begin
               Insert_After (Last_Stm, RE);
               Analyze (RE);
            end;
         end if;
      end Check_Statement_Sequence;

   --  Start of processing for Check_Returns

   begin
      Err := False;
      Check_Statement_Sequence (Statements (HSS));

      if Present (Exception_Handlers (HSS)) then
         Handler := First_Non_Pragma (Exception_Handlers (HSS));
         while Present (Handler) loop
            Check_Statement_Sequence (Statements (Handler));
            Next_Non_Pragma (Handler);
         end loop;
      end if;
   end Check_Returns;

   ----------------------------
   -- Check_Subprogram_Order --
   ----------------------------

   procedure Check_Subprogram_Order (N : Node_Id) is

      function Subprogram_Name_Greater (S1, S2 : String) return Boolean;
      --  This is used to check if S1 > S2 in the sense required by this
      --  test, for example nameab < namec, but name2 < name10.

      -----------------------------
      -- Subprogram_Name_Greater --
      -----------------------------

      function Subprogram_Name_Greater (S1, S2 : String) return Boolean is
         L1, L2 : Positive;
         N1, N2 : Natural;

      begin
         --  Remove trailing numeric parts

         L1 := S1'Last;
         while S1 (L1) in '0' .. '9' loop
            L1 := L1 - 1;
         end loop;

         L2 := S2'Last;
         while S2 (L2) in '0' .. '9' loop
            L2 := L2 - 1;
         end loop;

         --  If non-numeric parts non-equal, that's decisive

         if S1 (S1'First .. L1) < S2 (S2'First .. L2) then
            return False;

         elsif S1 (S1'First .. L1) > S2 (S2'First .. L2) then
            return True;

         --  If non-numeric parts equal, compare suffixed numeric parts. Note
         --  that a missing suffix is treated as numeric zero in this test.

         else
            N1 := 0;
            while L1 < S1'Last loop
               L1 := L1 + 1;
               N1 := N1 * 10 + Character'Pos (S1 (L1)) - Character'Pos ('0');
            end loop;

            N2 := 0;
            while L2 < S2'Last loop
               L2 := L2 + 1;
               N2 := N2 * 10 + Character'Pos (S2 (L2)) - Character'Pos ('0');
            end loop;

            return N1 > N2;
         end if;
      end Subprogram_Name_Greater;

   --  Start of processing for Check_Subprogram_Order

   begin
      --  Check body in alpha order if this is option

      if Style_Check
        and then Style_Check_Order_Subprograms
        and then Nkind (N) = N_Subprogram_Body
        and then Comes_From_Source (N)
        and then In_Extended_Main_Source_Unit (N)
      then
         declare
            LSN : String_Ptr
                    renames Scope_Stack.Table
                              (Scope_Stack.Last).Last_Subprogram_Name;

            Body_Id : constant Entity_Id :=
                        Defining_Entity (Specification (N));

         begin
            Get_Decoded_Name_String (Chars (Body_Id));

            if LSN /= null then
               if Subprogram_Name_Greater
                    (LSN.all, Name_Buffer (1 .. Name_Len))
               then
                  Style.Subprogram_Not_In_Alpha_Order (Body_Id);
               end if;

               Free (LSN);
            end if;

            LSN := new String'(Name_Buffer (1 .. Name_Len));
         end;
      end if;
   end Check_Subprogram_Order;

   ------------------------------
   -- Check_Subtype_Conformant --
   ------------------------------

   procedure Check_Subtype_Conformant
     (New_Id  : Entity_Id;
      Old_Id  : Entity_Id;
      Err_Loc : Node_Id := Empty)
   is
      Result : Boolean;
   begin
      Check_Conformance
        (New_Id, Old_Id, Subtype_Conformant, True, Result, Err_Loc);
   end Check_Subtype_Conformant;

   ---------------------------
   -- Check_Type_Conformant --
   ---------------------------

   procedure Check_Type_Conformant
     (New_Id  : Entity_Id;
      Old_Id  : Entity_Id;
      Err_Loc : Node_Id := Empty)
   is
      Result : Boolean;
   begin
      Check_Conformance
        (New_Id, Old_Id, Type_Conformant, True, Result, Err_Loc);
   end Check_Type_Conformant;

   ----------------------
   -- Conforming_Types --
   ----------------------

   function Conforming_Types
     (T1       : Entity_Id;
      T2       : Entity_Id;
      Ctype    : Conformance_Type;
      Get_Inst : Boolean := False) return Boolean
   is
      Type_1 : Entity_Id := T1;
      Type_2 : Entity_Id := T2;
      Are_Anonymous_Access_To_Subprogram_Types : Boolean := False;

      function Base_Types_Match (T1, T2 : Entity_Id) return Boolean;
      --  If neither T1 nor T2 are generic actual types, or if they are
      --  in different scopes (e.g. parent and child instances), then verify
      --  that the base types are equal. Otherwise T1 and T2 must be
      --  on the same subtype chain. The whole purpose of this procedure
      --  is to prevent spurious ambiguities in an instantiation that may
      --  arise if two distinct generic types are instantiated with the
      --  same actual.

      ----------------------
      -- Base_Types_Match --
      ----------------------

      function Base_Types_Match (T1, T2 : Entity_Id) return Boolean is
      begin
         if T1 = T2 then
            return True;

         elsif Base_Type (T1) = Base_Type (T2) then

            --  The following is too permissive. A more precise test must
            --  check that the generic actual is an ancestor subtype of the
            --  other ???.

            return not Is_Generic_Actual_Type (T1)
              or else not Is_Generic_Actual_Type (T2)
              or else Scope (T1) /= Scope (T2);

         --  In some cases a type imported through a limited_with clause,
         --  and its non-limited view are both visible, for example in an
         --  anonymous access_to_classwide type in a formal. Both entities
         --  designate the same type.

         elsif From_With_Type (T1)
           and then Ekind (T1) = E_Incomplete_Type
           and then T2 = Non_Limited_View (T1)
         then
            return True;

         elsif From_With_Type (T2)
           and then Ekind (T2) = E_Incomplete_Type
           and then T1 = Non_Limited_View (T2)
         then
            return True;

         else
            return False;
         end if;
      end Base_Types_Match;

      --  Start of processing for Conforming_Types

   begin
      --  The context is an instance association for a formal
      --  access-to-subprogram type; the formal parameter types require
      --  mapping because they may denote other formal parameters of the
      --  generic unit.

      if Get_Inst then
         Type_1 := Get_Instance_Of (T1);
         Type_2 := Get_Instance_Of (T2);
      end if;

      --  First see if base types match

      if Base_Types_Match (Type_1, Type_2) then
         return Ctype <= Mode_Conformant
           or else Subtypes_Statically_Match (Type_1, Type_2);

      elsif Is_Incomplete_Or_Private_Type (Type_1)
        and then Present (Full_View (Type_1))
        and then Base_Types_Match (Full_View (Type_1), Type_2)
      then
         return Ctype <= Mode_Conformant
           or else Subtypes_Statically_Match (Full_View (Type_1), Type_2);

      elsif Ekind (Type_2) = E_Incomplete_Type
        and then Present (Full_View (Type_2))
        and then Base_Types_Match (Type_1, Full_View (Type_2))
      then
         return Ctype <= Mode_Conformant
           or else Subtypes_Statically_Match (Type_1, Full_View (Type_2));

      elsif Is_Private_Type (Type_2)
        and then In_Instance
        and then Present (Full_View (Type_2))
        and then Base_Types_Match (Type_1, Full_View (Type_2))
      then
         return Ctype <= Mode_Conformant
           or else Subtypes_Statically_Match (Type_1, Full_View (Type_2));
      end if;

      --  Ada 2005 (AI-254): Anonymous access to subprogram types must be
      --  treated recursively because they carry a signature.

      Are_Anonymous_Access_To_Subprogram_Types :=

         --  Case 1: Anonymous access to subprogram types

        (Ekind (Type_1) = E_Anonymous_Access_Subprogram_Type
           and then Ekind (Type_2) = E_Anonymous_Access_Subprogram_Type)

         --  Case 2: Anonymous access to PROTECTED subprogram types. In this
         --  case the anonymous type_declaration has been replaced by an
         --  occurrence of an internal access to subprogram type declaration
         --  available through the Original_Access_Type attribute

        or else
          (Ekind (Type_1) = E_Access_Protected_Subprogram_Type
            and then Ekind (Type_2) = E_Access_Protected_Subprogram_Type
            and then not Comes_From_Source (Type_1)
            and then not Comes_From_Source (Type_2)
            and then Present (Original_Access_Type (Type_1))
            and then Present (Original_Access_Type (Type_2))
            and then Ekind (Original_Access_Type (Type_1)) =
                       E_Anonymous_Access_Protected_Subprogram_Type
            and then Ekind (Original_Access_Type (Type_2)) =
                       E_Anonymous_Access_Protected_Subprogram_Type);

      --  Test anonymous access type case. For this case, static subtype
      --  matching is required for mode conformance (RM 6.3.1(15))

      if (Ekind (Type_1) = E_Anonymous_Access_Type
            and then Ekind (Type_2) = E_Anonymous_Access_Type)
        or else Are_Anonymous_Access_To_Subprogram_Types -- Ada 2005 (AI-254)
      then
         declare
            Desig_1 : Entity_Id;
            Desig_2 : Entity_Id;

         begin
            Desig_1 := Directly_Designated_Type (Type_1);

            --  An access parameter can designate an incomplete type
            --  If the incomplete type is the limited view of a type
            --  from a limited_with_clause, check whether the non-limited
            --  view is available.

            if Ekind (Desig_1) = E_Incomplete_Type then
               if Present (Full_View (Desig_1)) then
                  Desig_1 := Full_View (Desig_1);

               elsif Present (Non_Limited_View (Desig_1)) then
                  Desig_1 := Non_Limited_View (Desig_1);
               end if;
            end if;

            Desig_2 := Directly_Designated_Type (Type_2);

            if Ekind (Desig_2) = E_Incomplete_Type then
               if Present (Full_View (Desig_2)) then
                  Desig_2 := Full_View (Desig_2);
               elsif Present (Non_Limited_View (Desig_2)) then
                  Desig_2 := Non_Limited_View (Desig_2);
               end if;
            end if;

            --  The context is an instance association for a formal
            --  access-to-subprogram type; formal access parameter designated
            --  types require mapping because they may denote other formal
            --  parameters of the generic unit.

            if Get_Inst then
               Desig_1 := Get_Instance_Of (Desig_1);
               Desig_2 := Get_Instance_Of (Desig_2);
            end if;

            --  It is possible for a Class_Wide_Type to be introduced for an
            --  incomplete type, in which case there is a separate class_ wide
            --  type for the full view. The types conform if their Etypes
            --  conform, i.e. one may be the full view of the other. This can
            --  only happen in the context of an access parameter, other uses
            --  of an incomplete Class_Wide_Type are illegal.

            if Is_Class_Wide_Type (Desig_1)
              and then Is_Class_Wide_Type (Desig_2)
            then
               return
                 Conforming_Types
                   (Etype (Base_Type (Desig_1)),
                    Etype (Base_Type (Desig_2)), Ctype);

            elsif Are_Anonymous_Access_To_Subprogram_Types then
               if Ada_Version < Ada_05 then
                  return Ctype = Type_Conformant
                    or else
                      Subtypes_Statically_Match (Desig_1, Desig_2);

               --  We must check the conformance of the signatures themselves

               else
                  declare
                     Conformant : Boolean;
                  begin
                     Check_Conformance
                       (Desig_1, Desig_2, Ctype, False, Conformant);
                     return Conformant;
                  end;
               end if;

            else
               return Base_Type (Desig_1) = Base_Type (Desig_2)
                and then (Ctype = Type_Conformant
                            or else
                          Subtypes_Statically_Match (Desig_1, Desig_2));
            end if;
         end;

      --  Otherwise definitely no match

      else
         if ((Ekind (Type_1) = E_Anonymous_Access_Type
               and then Is_Access_Type (Type_2))
            or else (Ekind (Type_2) = E_Anonymous_Access_Type
                       and then Is_Access_Type (Type_1)))
           and then
             Conforming_Types
               (Designated_Type (Type_1), Designated_Type (Type_2), Ctype)
         then
            May_Hide_Profile := True;
         end if;

         return False;
      end if;
   end Conforming_Types;

   --------------------------
   -- Create_Extra_Formals --
   --------------------------

   procedure Create_Extra_Formals (E : Entity_Id) is
      Formal      : Entity_Id;
      Last_Extra  : Entity_Id;
      Formal_Type : Entity_Id;
      P_Formal    : Entity_Id := Empty;

      function Add_Extra_Formal (Typ : Entity_Id) return Entity_Id;
      --  Add an extra formal, associated with the current Formal. The extra
      --  formal is added to the list of extra formals, and also returned as
      --  the result. These formals are always of mode IN.

      ----------------------
      -- Add_Extra_Formal --
      ----------------------

      function Add_Extra_Formal (Typ : Entity_Id) return Entity_Id is
         EF : constant Entity_Id :=
                Make_Defining_Identifier (Sloc (Formal),
                  Chars => New_External_Name (Chars (Formal), 'F'));

      begin
         --  We never generate extra formals if expansion is not active
         --  because we don't need them unless we are generating code.

         if not Expander_Active then
            return Empty;
         end if;

         --  A little optimization. Never generate an extra formal for the
         --  _init operand of an initialization procedure, since it could
         --  never be used.

         if Chars (Formal) = Name_uInit then
            return Empty;
         end if;

         Set_Ekind           (EF, E_In_Parameter);
         Set_Actual_Subtype  (EF, Typ);
         Set_Etype           (EF, Typ);
         Set_Scope           (EF, Scope (Formal));
         Set_Mechanism       (EF, Default_Mechanism);
         Set_Formal_Validity (EF);

         Set_Extra_Formal (Last_Extra, EF);
         Last_Extra := EF;
         return EF;
      end Add_Extra_Formal;

   --  Start of processing for Create_Extra_Formals

   begin
      --  If this is a derived subprogram then the subtypes of the parent
      --  subprogram's formal parameters will be used to to determine the need
      --  for extra formals.

      if Is_Overloadable (E) and then Present (Alias (E)) then
         P_Formal := First_Formal (Alias (E));
      end if;

      Last_Extra := Empty;
      Formal := First_Formal (E);
      while Present (Formal) loop
         Last_Extra := Formal;
         Next_Formal (Formal);
      end loop;

      --  If Extra_formals where already created, don't do it again. This
      --  situation may arise for subprogram types created as part of
      --  dispatching calls (see Expand_Dispatching_Call)

      if Present (Last_Extra) and then
        Present (Extra_Formal (Last_Extra))
      then
         return;
      end if;

      Formal := First_Formal (E);

      while Present (Formal) loop

         --  Create extra formal for supporting the attribute 'Constrained.
         --  The case of a private type view without discriminants also
         --  requires the extra formal if the underlying type has defaulted
         --  discriminants.

         if Ekind (Formal) /= E_In_Parameter then
            if Present (P_Formal) then
               Formal_Type := Etype (P_Formal);
            else
               Formal_Type := Etype (Formal);
            end if;

            --  Do not produce extra formals for Unchecked_Union parameters.
            --  Jump directly to the end of the loop.

            if Is_Unchecked_Union (Base_Type (Formal_Type)) then
               goto Skip_Extra_Formal_Generation;
            end if;

            if not Has_Discriminants (Formal_Type)
              and then Ekind (Formal_Type) in Private_Kind
              and then Present (Underlying_Type (Formal_Type))
            then
               Formal_Type := Underlying_Type (Formal_Type);
            end if;

            if Has_Discriminants (Formal_Type)
              and then
                ((not Is_Constrained (Formal_Type)
                    and then not Is_Indefinite_Subtype (Formal_Type))
                  or else Present (Extra_Formal (Formal)))
            then
               Set_Extra_Constrained
                 (Formal, Add_Extra_Formal (Standard_Boolean));
            end if;
         end if;

         --  Create extra formal for supporting accessibility checking

         --  This is suppressed if we specifically suppress accessibility
         --  checks at the pacage level for either the subprogram, or the
         --  package in which it resides. However, we do not suppress it
         --  simply if the scope has accessibility checks suppressed, since
         --  this could cause trouble when clients are compiled with a
         --  different suppression setting. The explicit checks at the
         --  package level are safe from this point of view.

         if Ekind (Etype (Formal)) = E_Anonymous_Access_Type
           and then not
             (Explicit_Suppress (E, Accessibility_Check)
               or else
              Explicit_Suppress (Scope (E), Accessibility_Check))
           and then
             (No (P_Formal)
               or else Present (Extra_Accessibility (P_Formal)))
         then
            --  Temporary kludge: for now we avoid creating the extra formal
            --  for access parameters of protected operations because of
            --  problem with the case of internal protected calls. ???

            if Nkind (Parent (Parent (Parent (E)))) /= N_Protected_Definition
              and then Nkind (Parent (Parent (Parent (E)))) /= N_Protected_Body
            then
               Set_Extra_Accessibility
                 (Formal, Add_Extra_Formal (Standard_Natural));
            end if;
         end if;

         if Present (P_Formal) then
            Next_Formal (P_Formal);
         end if;

         --  This label is required when skipping extra formal generation for
         --  Unchecked_Union parameters.

         <<Skip_Extra_Formal_Generation>>

         Next_Formal (Formal);
      end loop;
   end Create_Extra_Formals;

   -----------------------------
   -- Enter_Overloaded_Entity --
   -----------------------------

   procedure Enter_Overloaded_Entity (S : Entity_Id) is
      E   : Entity_Id := Current_Entity_In_Scope (S);
      C_E : Entity_Id := Current_Entity (S);

   begin
      if Present (E) then
         Set_Has_Homonym (E);
         Set_Has_Homonym (S);
      end if;

      Set_Is_Immediately_Visible (S);
      Set_Scope (S, Current_Scope);

      --  Chain new entity if front of homonym in current scope, so that
      --  homonyms are contiguous.

      if Present (E)
        and then E /= C_E
      then
         while Homonym (C_E) /= E loop
            C_E := Homonym (C_E);
         end loop;

         Set_Homonym (C_E, S);

      else
         E := C_E;
         Set_Current_Entity (S);
      end if;

      Set_Homonym (S, E);

      Append_Entity (S, Current_Scope);
      Set_Public_Status (S);

      if Debug_Flag_E then
         Write_Str ("New overloaded entity chain: ");
         Write_Name (Chars (S));

         E := S;
         while Present (E) loop
            Write_Str (" "); Write_Int (Int (E));
            E := Homonym (E);
         end loop;

         Write_Eol;
      end if;

      --  Generate warning for hiding

      if Warn_On_Hiding
        and then Comes_From_Source (S)
        and then In_Extended_Main_Source_Unit (S)
      then
         E := S;
         loop
            E := Homonym (E);
            exit when No (E);

            --  Warn unless genuine overloading

            if (not Is_Overloadable (E))
              or else Subtype_Conformant (E, S)
            then
               Error_Msg_Sloc := Sloc (E);
               Error_Msg_N ("declaration of & hides one#?", S);
            end if;
         end loop;
      end if;
   end Enter_Overloaded_Entity;

   -----------------------------
   -- Find_Corresponding_Spec --
   -----------------------------

   function Find_Corresponding_Spec (N : Node_Id) return Entity_Id is
      Spec       : constant Node_Id   := Specification (N);
      Designator : constant Entity_Id := Defining_Entity (Spec);

      E : Entity_Id;

   begin
      E := Current_Entity (Designator);

      while Present (E) loop

         --  We are looking for a matching spec. It must have the same scope,
         --  and the same name, and either be type conformant, or be the case
         --  of a library procedure spec and its body (which belong to one
         --  another regardless of whether they are type conformant or not).

         if Scope (E) = Current_Scope then
            if Current_Scope = Standard_Standard
              or else (Ekind (E) = Ekind (Designator)
                         and then Type_Conformant (E, Designator))
            then
               --  Within an instantiation, we know that spec and body are
               --  subtype conformant, because they were subtype conformant
               --  in the generic. We choose the subtype-conformant entity
               --  here as well, to resolve spurious ambiguities in the
               --  instance that were not present in the generic (i.e. when
               --  two different types are given the same actual). If we are
               --  looking for a spec to match a body, full conformance is
               --  expected.

               if In_Instance then
                  Set_Convention (Designator, Convention (E));

                  if Nkind (N) = N_Subprogram_Body
                    and then Present (Homonym (E))
                    and then not Fully_Conformant (E, Designator)
                  then
                     goto Next_Entity;

                  elsif not Subtype_Conformant (E, Designator) then
                     goto Next_Entity;
                  end if;
               end if;

               if not Has_Completion (E) then

                  if Nkind (N) /= N_Subprogram_Body_Stub then
                     Set_Corresponding_Spec (N, E);
                  end if;

                  Set_Has_Completion (E);
                  return E;

               elsif Nkind (Parent (N)) = N_Subunit then

                  --  If this is the proper body of a subunit, the completion
                  --  flag is set when analyzing the stub.

                  return E;

               --  If body already exists, this is an error unless the
               --  previous declaration is the implicit declaration of
               --  a derived subprogram, or this is a spurious overloading
               --  in an instance.

               elsif No (Alias (E))
                 and then not Is_Intrinsic_Subprogram (E)
                 and then not In_Instance
               then
                  Error_Msg_Sloc := Sloc (E);
                  if Is_Imported (E) then
                     Error_Msg_NE
                      ("body not allowed for imported subprogram & declared#",
                        N, E);
                  else
                     Error_Msg_NE ("duplicate body for & declared#", N, E);
                  end if;
               end if;

            elsif Is_Child_Unit (E)
              and then
                Nkind (Unit_Declaration_Node (Designator)) = N_Subprogram_Body
              and then
                Nkind (Parent (Unit_Declaration_Node (Designator)))
                  = N_Compilation_Unit
            then

               --  Child units cannot be overloaded, so a conformance mismatch
               --  between body and a previous spec is an error.

               Error_Msg_N
                 ("body of child unit does not match previous declaration", N);
            end if;
         end if;

         <<Next_Entity>>
            E := Homonym (E);
      end loop;

      --  On exit, we know that no previous declaration of subprogram exists

      return Empty;
   end Find_Corresponding_Spec;

   ----------------------
   -- Fully_Conformant --
   ----------------------

   function Fully_Conformant (New_Id, Old_Id : Entity_Id) return Boolean is
      Result : Boolean;
   begin
      Check_Conformance (New_Id, Old_Id, Fully_Conformant, False, Result);
      return Result;
   end Fully_Conformant;

   ----------------------------------
   -- Fully_Conformant_Expressions --
   ----------------------------------

   function Fully_Conformant_Expressions
     (Given_E1 : Node_Id;
      Given_E2 : Node_Id) return Boolean
   is
      E1 : constant Node_Id := Original_Node (Given_E1);
      E2 : constant Node_Id := Original_Node (Given_E2);
      --  We always test conformance on original nodes, since it is possible
      --  for analysis and/or expansion to make things look as though they
      --  conform when they do not, e.g. by converting 1+2 into 3.

      function FCE (Given_E1, Given_E2 : Node_Id) return Boolean
        renames Fully_Conformant_Expressions;

      function FCL (L1, L2 : List_Id) return Boolean;
      --  Compare elements of two lists for conformance. Elements have to
      --  be conformant, and actuals inserted as default parameters do not
      --  match explicit actuals with the same value.

      function FCO (Op_Node, Call_Node : Node_Id) return Boolean;
      --  Compare an operator node with a function call

      ---------
      -- FCL --
      ---------

      function FCL (L1, L2 : List_Id) return Boolean is
         N1, N2 : Node_Id;

      begin
         if L1 = No_List then
            N1 := Empty;
         else
            N1 := First (L1);
         end if;

         if L2 = No_List then
            N2 := Empty;
         else
            N2 := First (L2);
         end if;

         --  Compare two lists, skipping rewrite insertions (we want to
         --  compare the original trees, not the expanded versions!)

         loop
            if Is_Rewrite_Insertion (N1) then
               Next (N1);
            elsif Is_Rewrite_Insertion (N2) then
               Next (N2);
            elsif No (N1) then
               return No (N2);
            elsif No (N2) then
               return False;
            elsif not FCE (N1, N2) then
               return False;
            else
               Next (N1);
               Next (N2);
            end if;
         end loop;
      end FCL;

      ---------
      -- FCO --
      ---------

      function FCO (Op_Node, Call_Node : Node_Id) return Boolean is
         Actuals : constant List_Id := Parameter_Associations (Call_Node);
         Act     : Node_Id;

      begin
         if No (Actuals)
            or else Entity (Op_Node) /= Entity (Name (Call_Node))
         then
            return False;

         else
            Act := First (Actuals);

            if Nkind (Op_Node) in N_Binary_Op then

               if not FCE (Left_Opnd (Op_Node), Act) then
                  return False;
               end if;

               Next (Act);
            end if;

            return Present (Act)
              and then FCE (Right_Opnd (Op_Node), Act)
              and then No (Next (Act));
         end if;
      end FCO;

   --  Start of processing for Fully_Conformant_Expressions

   begin
      --  Non-conformant if paren count does not match. Note: if some idiot
      --  complains that we don't do this right for more than 3 levels of
      --  parentheses, they will be treated with the respect they deserve :-)

      if Paren_Count (E1) /= Paren_Count (E2) then
         return False;

      --  If same entities are referenced, then they are conformant even if
      --  they have different forms (RM 8.3.1(19-20)).

      elsif Is_Entity_Name (E1) and then Is_Entity_Name (E2) then
         if Present (Entity (E1)) then
            return Entity (E1) = Entity (E2)
              or else (Chars (Entity (E1)) = Chars (Entity (E2))
                        and then Ekind (Entity (E1)) = E_Discriminant
                        and then Ekind (Entity (E2)) = E_In_Parameter);

         elsif Nkind (E1) = N_Expanded_Name
           and then Nkind (E2) = N_Expanded_Name
           and then Nkind (Selector_Name (E1)) = N_Character_Literal
           and then Nkind (Selector_Name (E2)) = N_Character_Literal
         then
            return Chars (Selector_Name (E1)) = Chars (Selector_Name (E2));

         else
            --  Identifiers in component associations don't always have
            --  entities, but their names must conform.

            return Nkind  (E1) = N_Identifier
              and then Nkind (E2) = N_Identifier
              and then Chars (E1) = Chars (E2);
         end if;

      elsif Nkind (E1) = N_Character_Literal
        and then Nkind (E2) = N_Expanded_Name
      then
         return Nkind (Selector_Name (E2)) = N_Character_Literal
           and then Chars (E1) = Chars (Selector_Name (E2));

      elsif Nkind (E2) = N_Character_Literal
        and then Nkind (E1) = N_Expanded_Name
      then
         return Nkind (Selector_Name (E1)) = N_Character_Literal
           and then Chars (E2) = Chars (Selector_Name (E1));

      elsif Nkind (E1) in N_Op
        and then Nkind (E2) = N_Function_Call
      then
         return FCO (E1, E2);

      elsif Nkind (E2) in N_Op
        and then Nkind (E1) = N_Function_Call
      then
         return FCO (E2, E1);

      --  Otherwise we must have the same syntactic entity

      elsif Nkind (E1) /= Nkind (E2) then
         return False;

      --  At this point, we specialize by node type

      else
         case Nkind (E1) is

            when N_Aggregate =>
               return
                 FCL (Expressions (E1), Expressions (E2))
                   and then FCL (Component_Associations (E1),
                                 Component_Associations (E2));

            when N_Allocator =>
               if Nkind (Expression (E1)) = N_Qualified_Expression
                    or else
                  Nkind (Expression (E2)) = N_Qualified_Expression
               then
                  return FCE (Expression (E1), Expression (E2));

               --  Check that the subtype marks and any constraints
               --  are conformant

               else
                  declare
                     Indic1 : constant Node_Id := Expression (E1);
                     Indic2 : constant Node_Id := Expression (E2);
                     Elt1   : Node_Id;
                     Elt2   : Node_Id;

                  begin
                     if Nkind (Indic1) /= N_Subtype_Indication then
                        return
                          Nkind (Indic2) /= N_Subtype_Indication
                            and then Entity (Indic1) = Entity (Indic2);

                     elsif Nkind (Indic2) /= N_Subtype_Indication then
                        return
                          Nkind (Indic1) /= N_Subtype_Indication
                            and then Entity (Indic1) = Entity (Indic2);

                     else
                        if Entity (Subtype_Mark (Indic1)) /=
                          Entity (Subtype_Mark (Indic2))
                        then
                           return False;
                        end if;

                        Elt1 := First (Constraints (Constraint (Indic1)));
                        Elt2 := First (Constraints (Constraint (Indic2)));

                        while Present (Elt1) and then Present (Elt2) loop
                           if not FCE (Elt1, Elt2) then
                              return False;
                           end if;

                           Next (Elt1);
                           Next (Elt2);
                        end loop;

                        return True;
                     end if;
                  end;
               end if;

            when N_Attribute_Reference =>
               return
                 Attribute_Name (E1) = Attribute_Name (E2)
                   and then FCL (Expressions (E1), Expressions (E2));

            when N_Binary_Op =>
               return
                 Entity (E1) = Entity (E2)
                   and then FCE (Left_Opnd  (E1), Left_Opnd  (E2))
                   and then FCE (Right_Opnd (E1), Right_Opnd (E2));

            when N_And_Then | N_Or_Else | N_In | N_Not_In =>
               return
                 FCE (Left_Opnd  (E1), Left_Opnd  (E2))
                   and then
                 FCE (Right_Opnd (E1), Right_Opnd (E2));

            when N_Character_Literal =>
               return
                 Char_Literal_Value (E1) = Char_Literal_Value (E2);

            when N_Component_Association =>
               return
                 FCL (Choices (E1), Choices (E2))
                   and then FCE (Expression (E1), Expression (E2));

            when N_Conditional_Expression =>
               return
                 FCL (Expressions (E1), Expressions (E2));

            when N_Explicit_Dereference =>
               return
                 FCE (Prefix (E1), Prefix (E2));

            when N_Extension_Aggregate =>
               return
                 FCL (Expressions (E1), Expressions (E2))
                   and then Null_Record_Present (E1) =
                            Null_Record_Present (E2)
                   and then FCL (Component_Associations (E1),
                               Component_Associations (E2));

            when N_Function_Call =>
               return
                 FCE (Name (E1), Name (E2))
                   and then FCL (Parameter_Associations (E1),
                                 Parameter_Associations (E2));

            when N_Indexed_Component =>
               return
                 FCE (Prefix (E1), Prefix (E2))
                   and then FCL (Expressions (E1), Expressions (E2));

            when N_Integer_Literal =>
               return (Intval (E1) = Intval (E2));

            when N_Null =>
               return True;

            when N_Operator_Symbol =>
               return
                 Chars (E1) = Chars (E2);

            when N_Others_Choice =>
               return True;

            when N_Parameter_Association =>
               return
                 Chars (Selector_Name (E1))  = Chars (Selector_Name (E2))
                   and then FCE (Explicit_Actual_Parameter (E1),
                                 Explicit_Actual_Parameter (E2));

            when N_Qualified_Expression =>
               return
                 FCE (Subtype_Mark (E1), Subtype_Mark (E2))
                   and then FCE (Expression (E1), Expression (E2));

            when N_Range =>
               return
                 FCE (Low_Bound (E1), Low_Bound (E2))
                   and then FCE (High_Bound (E1), High_Bound (E2));

            when N_Real_Literal =>
               return (Realval (E1) = Realval (E2));

            when N_Selected_Component =>
               return
                 FCE (Prefix (E1), Prefix (E2))
                   and then FCE (Selector_Name (E1), Selector_Name (E2));

            when N_Slice =>
               return
                 FCE (Prefix (E1), Prefix (E2))
                   and then FCE (Discrete_Range (E1), Discrete_Range (E2));

            when N_String_Literal =>
               declare
                  S1 : constant String_Id := Strval (E1);
                  S2 : constant String_Id := Strval (E2);
                  L1 : constant Nat       := String_Length (S1);
                  L2 : constant Nat       := String_Length (S2);

               begin
                  if L1 /= L2 then
                     return False;

                  else
                     for J in 1 .. L1 loop
                        if Get_String_Char (S1, J) /=
                           Get_String_Char (S2, J)
                        then
                           return False;
                        end if;
                     end loop;

                     return True;
                  end if;
               end;

            when N_Type_Conversion =>
               return
                 FCE (Subtype_Mark (E1), Subtype_Mark (E2))
                   and then FCE (Expression (E1), Expression (E2));

            when N_Unary_Op =>
               return
                 Entity (E1) = Entity (E2)
                   and then FCE (Right_Opnd (E1), Right_Opnd (E2));

            when N_Unchecked_Type_Conversion =>
               return
                 FCE (Subtype_Mark (E1), Subtype_Mark (E2))
                   and then FCE (Expression (E1), Expression (E2));

            --  All other node types cannot appear in this context. Strictly
            --  we should raise a fatal internal error. Instead we just ignore
            --  the nodes. This means that if anyone makes a mistake in the
            --  expander and mucks an expression tree irretrievably, the
            --  result will be a failure to detect a (probably very obscure)
            --  case of non-conformance, which is better than bombing on some
            --  case where two expressions do in fact conform.

            when others =>
               return True;

         end case;
      end if;
   end Fully_Conformant_Expressions;

   ----------------------------------------
   -- Fully_Conformant_Discrete_Subtypes --
   ----------------------------------------

   function Fully_Conformant_Discrete_Subtypes
     (Given_S1 : Node_Id;
      Given_S2 : Node_Id) return Boolean
   is
      S1 : constant Node_Id := Original_Node (Given_S1);
      S2 : constant Node_Id := Original_Node (Given_S2);

      function Conforming_Bounds (B1, B2 : Node_Id) return Boolean;
      --  Special-case for a bound given by a discriminant, which in the body
      --  is replaced with the discriminal of the enclosing type.

      function Conforming_Ranges (R1, R2 : Node_Id) return Boolean;
      --  Check both bounds

      function Conforming_Bounds (B1, B2 : Node_Id) return Boolean is
      begin
         if Is_Entity_Name (B1)
           and then Is_Entity_Name (B2)
           and then Ekind (Entity (B1)) = E_Discriminant
         then
            return Chars (B1) = Chars (B2);

         else
            return Fully_Conformant_Expressions (B1, B2);
         end if;
      end Conforming_Bounds;

      function Conforming_Ranges (R1, R2 : Node_Id) return Boolean is
      begin
         return
           Conforming_Bounds (Low_Bound (R1), Low_Bound (R2))
             and then
           Conforming_Bounds (High_Bound (R1), High_Bound (R2));
      end Conforming_Ranges;

   --  Start of processing for Fully_Conformant_Discrete_Subtypes

   begin
      if Nkind (S1) /= Nkind (S2) then
         return False;

      elsif Is_Entity_Name (S1) then
         return Entity (S1) = Entity (S2);

      elsif Nkind (S1) = N_Range then
         return Conforming_Ranges (S1, S2);

      elsif Nkind (S1) = N_Subtype_Indication then
         return
            Entity (Subtype_Mark (S1)) = Entity (Subtype_Mark (S2))
              and then
            Conforming_Ranges
              (Range_Expression (Constraint (S1)),
               Range_Expression (Constraint (S2)));
      else
         return True;
      end if;
   end Fully_Conformant_Discrete_Subtypes;

   --------------------
   -- Install_Entity --
   --------------------

   procedure Install_Entity (E : Entity_Id) is
      Prev : constant Entity_Id := Current_Entity (E);
   begin
      Set_Is_Immediately_Visible (E);
      Set_Current_Entity (E);
      Set_Homonym (E, Prev);
   end Install_Entity;

   ---------------------
   -- Install_Formals --
   ---------------------

   procedure Install_Formals (Id : Entity_Id) is
      F : Entity_Id;
   begin
      F := First_Formal (Id);
      while Present (F) loop
         Install_Entity (F);
         Next_Formal (F);
      end loop;
   end Install_Formals;

   ---------------------------------
   -- Is_Non_Overriding_Operation --
   ---------------------------------

   function Is_Non_Overriding_Operation
     (Prev_E : Entity_Id;
      New_E  : Entity_Id) return Boolean
   is
      Formal : Entity_Id;
      F_Typ  : Entity_Id;
      G_Typ  : Entity_Id := Empty;

      function Get_Generic_Parent_Type (F_Typ : Entity_Id) return Entity_Id;
      --  If F_Type is a derived type associated with a generic actual
      --  subtype, then return its Generic_Parent_Type attribute, else return
      --  Empty.

      function Types_Correspond
        (P_Type : Entity_Id;
         N_Type : Entity_Id) return Boolean;
      --  Returns true if and only if the types (or designated types in the
      --  case of anonymous access types) are the same or N_Type is derived
      --  directly or indirectly from P_Type.

      -----------------------------
      -- Get_Generic_Parent_Type --
      -----------------------------

      function Get_Generic_Parent_Type (F_Typ : Entity_Id) return Entity_Id is
         G_Typ : Entity_Id;
         Indic : Node_Id;

      begin
         if Is_Derived_Type (F_Typ)
           and then Nkind (Parent (F_Typ)) = N_Full_Type_Declaration
         then
            --  The tree must be traversed to determine the parent subtype in
            --  the generic unit, which unfortunately isn't always available
            --  via semantic attributes. ??? (Note: The use of Original_Node
            --  is needed for cases where a full derived type has been
            --  rewritten.)

            Indic := Subtype_Indication
                       (Type_Definition (Original_Node (Parent (F_Typ))));

            if Nkind (Indic) = N_Subtype_Indication then
               G_Typ := Entity (Subtype_Mark (Indic));
            else
               G_Typ := Entity (Indic);
            end if;

            if Nkind (Parent (G_Typ)) = N_Subtype_Declaration
              and then Present (Generic_Parent_Type (Parent (G_Typ)))
            then
               return Generic_Parent_Type (Parent (G_Typ));
            end if;
         end if;

         return Empty;
      end Get_Generic_Parent_Type;

      ----------------------
      -- Types_Correspond --
      ----------------------

      function Types_Correspond
        (P_Type : Entity_Id;
         N_Type : Entity_Id) return Boolean
      is
         Prev_Type : Entity_Id := Base_Type (P_Type);
         New_Type  : Entity_Id := Base_Type (N_Type);

      begin
         if Ekind (Prev_Type) = E_Anonymous_Access_Type then
            Prev_Type := Designated_Type (Prev_Type);
         end if;

         if Ekind (New_Type) = E_Anonymous_Access_Type then
            New_Type := Designated_Type (New_Type);
         end if;

         if Prev_Type = New_Type then
            return True;

         elsif not Is_Class_Wide_Type (New_Type) then
            while Etype (New_Type) /= New_Type loop
               New_Type := Etype (New_Type);
               if New_Type = Prev_Type then
                  return True;
               end if;
            end loop;
         end if;
         return False;
      end Types_Correspond;

   --  Start of processing for Is_Non_Overriding_Operation

   begin
      --  In the case where both operations are implicit derived subprograms
      --  then neither overrides the other. This can only occur in certain
      --  obscure cases (e.g., derivation from homographs created in a generic
      --  instantiation).

      if Present (Alias (Prev_E)) and then Present (Alias (New_E)) then
         return True;

      elsif Ekind (Current_Scope) = E_Package
        and then Is_Generic_Instance (Current_Scope)
        and then In_Private_Part (Current_Scope)
        and then Comes_From_Source (New_E)
      then
         --  We examine the formals and result subtype of the inherited
         --  operation, to determine whether their type is derived from (the
         --  instance of) a generic type.

         Formal := First_Formal (Prev_E);

         while Present (Formal) loop
            F_Typ := Base_Type (Etype (Formal));

            if Ekind (F_Typ) = E_Anonymous_Access_Type then
               F_Typ := Designated_Type (F_Typ);
            end if;

            G_Typ := Get_Generic_Parent_Type (F_Typ);

            Next_Formal (Formal);
         end loop;

         if No (G_Typ) and then Ekind (Prev_E) = E_Function then
            G_Typ := Get_Generic_Parent_Type (Base_Type (Etype (Prev_E)));
         end if;

         if No (G_Typ) then
            return False;
         end if;

         --  If the generic type is a private type, then the original
         --  operation was not overriding in the generic, because there was
         --  no primitive operation to override.

         if Nkind (Parent (G_Typ)) = N_Formal_Type_Declaration
           and then Nkind (Formal_Type_Definition (Parent (G_Typ))) =
             N_Formal_Private_Type_Definition
         then
            return True;

         --  The generic parent type is the ancestor of a formal derived
         --  type declaration. We need to check whether it has a primitive
         --  operation that should be overridden by New_E in the generic.

         else
            declare
               P_Formal : Entity_Id;
               N_Formal : Entity_Id;
               P_Typ    : Entity_Id;
               N_Typ    : Entity_Id;
               P_Prim   : Entity_Id;
               Prim_Elt : Elmt_Id := First_Elmt (Primitive_Operations (G_Typ));

            begin
               while Present (Prim_Elt) loop
                  P_Prim := Node (Prim_Elt);

                  if Chars (P_Prim) = Chars (New_E)
                    and then Ekind (P_Prim) = Ekind (New_E)
                  then
                     P_Formal := First_Formal (P_Prim);
                     N_Formal := First_Formal (New_E);
                     while Present (P_Formal) and then Present (N_Formal) loop
                        P_Typ := Etype (P_Formal);
                        N_Typ := Etype (N_Formal);

                        if not Types_Correspond (P_Typ, N_Typ) then
                           exit;
                        end if;

                        Next_Entity (P_Formal);
                        Next_Entity (N_Formal);
                     end loop;

                     --  Found a matching primitive operation belonging to the
                     --  formal ancestor type, so the new subprogram is
                     --  overriding.

                     if No (P_Formal)
                       and then No (N_Formal)
                       and then (Ekind (New_E) /= E_Function
                                  or else
                                 Types_Correspond
                                   (Etype (P_Prim), Etype (New_E)))
                     then
                        return False;
                     end if;
                  end if;

                  Next_Elmt (Prim_Elt);
               end loop;

               --  If no match found, then the new subprogram does not
               --  override in the generic (nor in the instance).

               return True;
            end;
         end if;
      else
         return False;
      end if;
   end Is_Non_Overriding_Operation;

   ------------------------------
   -- Make_Inequality_Operator --
   ------------------------------

   --  S is the defining identifier of an equality operator. We build a
   --  subprogram declaration with the right signature. This operation is
   --  intrinsic, because it is always expanded as the negation of the
   --  call to the equality function.

   procedure Make_Inequality_Operator (S : Entity_Id) is
      Loc     : constant Source_Ptr := Sloc (S);
      Decl    : Node_Id;
      Formals : List_Id;
      Op_Name : Entity_Id;

      FF : constant Entity_Id := First_Formal (S);
      NF : constant Entity_Id := Next_Formal (FF);

   begin
      --  Check that equality was properly defined, ignore call if not

      if No (NF) then
         return;
      end if;

      declare
         A : constant Entity_Id :=
               Make_Defining_Identifier (Sloc (FF),
                 Chars => Chars (FF));

         B  : constant Entity_Id :=
                Make_Defining_Identifier (Sloc (NF),
                  Chars => Chars (NF));

      begin
         Op_Name := Make_Defining_Operator_Symbol (Loc, Name_Op_Ne);

         Formals := New_List (
           Make_Parameter_Specification (Loc,
             Defining_Identifier => A,
             Parameter_Type      =>
               New_Reference_To (Etype (First_Formal (S)),
                 Sloc (Etype (First_Formal (S))))),

           Make_Parameter_Specification (Loc,
             Defining_Identifier => B,
             Parameter_Type      =>
               New_Reference_To (Etype (Next_Formal (First_Formal (S))),
                 Sloc (Etype (Next_Formal (First_Formal (S)))))));

         Decl :=
           Make_Subprogram_Declaration (Loc,
             Specification =>
               Make_Function_Specification (Loc,
                 Defining_Unit_Name       => Op_Name,
                 Parameter_Specifications => Formals,
                 Result_Definition        =>
                   New_Reference_To (Standard_Boolean, Loc)));

         --  Insert inequality right after equality if it is explicit or after
         --  the derived type when implicit. These entities are created only
         --  for visibility purposes, and eventually replaced in the course of
         --  expansion, so they do not need to be attached to the tree and seen
         --  by the back-end. Keeping them internal also avoids spurious
         --  freezing problems. The declaration is inserted in the tree for
         --  analysis, and removed afterwards. If the equality operator comes
         --  from an explicit declaration, attach the inequality immediately
         --  after. Else the equality is inherited from a derived type
         --  declaration, so insert inequality after that declaration.

         if No (Alias (S)) then
            Insert_After (Unit_Declaration_Node (S), Decl);
         elsif Is_List_Member (Parent (S)) then
            Insert_After (Parent (S), Decl);
         else
            Insert_After (Parent (Etype (First_Formal (S))), Decl);
         end if;

         Mark_Rewrite_Insertion (Decl);
         Set_Is_Intrinsic_Subprogram (Op_Name);
         Analyze (Decl);
         Remove (Decl);
         Set_Has_Completion (Op_Name);
         Set_Corresponding_Equality (Op_Name, S);
         Set_Is_Abstract (Op_Name, Is_Abstract (S));
      end;
   end Make_Inequality_Operator;

   ----------------------
   -- May_Need_Actuals --
   ----------------------

   procedure May_Need_Actuals (Fun : Entity_Id) is
      F : Entity_Id;
      B : Boolean;

   begin
      F := First_Formal (Fun);
      B := True;

      while Present (F) loop
         if No (Default_Value (F)) then
            B := False;
            exit;
         end if;

         Next_Formal (F);
      end loop;

      Set_Needs_No_Actuals (Fun, B);
   end May_Need_Actuals;

   ---------------------
   -- Mode_Conformant --
   ---------------------

   function Mode_Conformant (New_Id, Old_Id : Entity_Id) return Boolean is
      Result : Boolean;
   begin
      Check_Conformance (New_Id, Old_Id, Mode_Conformant, False, Result);
      return Result;
   end Mode_Conformant;

   ---------------------------
   -- New_Overloaded_Entity --
   ---------------------------

   procedure New_Overloaded_Entity
     (S            : Entity_Id;
      Derived_Type : Entity_Id := Empty)
   is
      Does_Override : Boolean := False;
      --  Set if the current scope has an operation that is type-conformant
      --  with S, and becomes hidden by S.

      E : Entity_Id;
      --  Entity that S overrides

      Prev_Vis : Entity_Id := Empty;
      --  Needs comment ???

      Is_Alias_Interface : Boolean := False;

      function Is_Private_Declaration (E : Entity_Id) return Boolean;
      --  Check that E is declared in the private part of the current package,
      --  or in the package body, where it may hide a previous declaration.
      --  We can't use In_Private_Part by itself because this flag is also
      --  set when freezing entities, so we must examine the place of the
      --  declaration in the tree, and recognize wrapper packages as well.

      procedure Maybe_Primitive_Operation (Is_Overriding : Boolean := False);
      --  If the subprogram being analyzed is a primitive operation of
      --  the type of one of its formals, set the corresponding flag.

      ----------------------------
      -- Is_Private_Declaration --
      ----------------------------

      function Is_Private_Declaration (E : Entity_Id) return Boolean is
         Priv_Decls : List_Id;
         Decl       : constant Node_Id := Unit_Declaration_Node (E);

      begin
         if Is_Package_Or_Generic_Package (Current_Scope)
           and then In_Private_Part (Current_Scope)
         then
            Priv_Decls :=
              Private_Declarations (
                Specification (Unit_Declaration_Node (Current_Scope)));

            return In_Package_Body (Current_Scope)
              or else
                (Is_List_Member (Decl)
                   and then List_Containing (Decl) = Priv_Decls)
              or else (Nkind (Parent (Decl)) = N_Package_Specification
                         and then not Is_Compilation_Unit (
                           Defining_Entity (Parent (Decl)))
                         and then List_Containing (Parent (Parent (Decl)))
                           = Priv_Decls);
         else
            return False;
         end if;
      end Is_Private_Declaration;

      -------------------------------
      -- Maybe_Primitive_Operation --
      -------------------------------

      procedure Maybe_Primitive_Operation (Is_Overriding : Boolean := False) is
         Formal : Entity_Id;
         F_Typ  : Entity_Id;
         B_Typ  : Entity_Id;

         function Visible_Part_Type (T : Entity_Id) return Boolean;
         --  Returns true if T is declared in the visible part of
         --  the current package scope; otherwise returns false.
         --  Assumes that T is declared in a package.

         procedure Check_Private_Overriding (T : Entity_Id);
         --  Checks that if a primitive abstract subprogram of a visible
         --  abstract type is declared in a private part, then it must
         --  override an abstract subprogram declared in the visible part.
         --  Also checks that if a primitive function with a controlling
         --  result is declared in a private part, then it must override
         --  a function declared in the visible part.

         ------------------------------
         -- Check_Private_Overriding --
         ------------------------------

         procedure Check_Private_Overriding (T : Entity_Id) is
         begin
            if Ekind (Current_Scope) = E_Package
              and then In_Private_Part (Current_Scope)
              and then Visible_Part_Type (T)
              and then not In_Instance
            then
               if Is_Abstract (T)
                 and then Is_Abstract (S)
                 and then (not Is_Overriding or else not Is_Abstract (E))
               then
                  if not Is_Interface (T) then
                     Error_Msg_N ("abstract subprograms must be visible "
                                   & "('R'M 3.9.3(10))!", S);

                  --  Ada 2005 (AI-251)

                  else
                     Error_Msg_N ("primitive subprograms of interface types "
                       & "declared in a visible part, must be declared in "
                       & "the visible part ('R'M 3.9.4)!", S);
                  end if;

               elsif Ekind (S) = E_Function
                 and then Is_Tagged_Type (T)
                 and then T = Base_Type (Etype (S))
                 and then not Is_Overriding
               then
                  Error_Msg_N
                    ("private function with tagged result must"
                     & " override visible-part function", S);
                  Error_Msg_N
                    ("\move subprogram to the visible part"
                     & " ('R'M 3.9.3(10))", S);
               end if;
            end if;
         end Check_Private_Overriding;

         -----------------------
         -- Visible_Part_Type --
         -----------------------

         function Visible_Part_Type (T : Entity_Id) return Boolean is
            P : constant Node_Id := Unit_Declaration_Node (Scope (T));
            N : Node_Id;

         begin
            --  If the entity is a private type, then it must be
            --  declared in a visible part.

            if Ekind (T) in Private_Kind then
               return True;
            end if;

            --  Otherwise, we traverse the visible part looking for its
            --  corresponding declaration. We cannot use the declaration
            --  node directly because in the private part the entity of a
            --  private type is the one in the full view, which does not
            --  indicate that it is the completion of something visible.

            N := First (Visible_Declarations (Specification (P)));
            while Present (N) loop
               if Nkind (N) = N_Full_Type_Declaration
                 and then Present (Defining_Identifier (N))
                 and then T = Defining_Identifier (N)
               then
                  return True;

               elsif (Nkind (N) = N_Private_Type_Declaration
                       or else
                      Nkind (N) = N_Private_Extension_Declaration)
                 and then Present (Defining_Identifier (N))
                 and then T = Full_View (Defining_Identifier (N))
               then
                  return True;
               end if;

               Next (N);
            end loop;

            return False;
         end Visible_Part_Type;

      --  Start of processing for Maybe_Primitive_Operation

      begin
         if not Comes_From_Source (S) then
            null;

         --  If the subprogram is at library level, it is not primitive
         --  operation.

         elsif Current_Scope = Standard_Standard then
            null;

         elsif (Ekind (Current_Scope) = E_Package
                 and then not In_Package_Body (Current_Scope))
           or else Is_Overriding
         then
            --  For function, check return type

            if Ekind (S) = E_Function then
               B_Typ := Base_Type (Etype (S));

               if Scope (B_Typ) = Current_Scope then
                  Set_Has_Primitive_Operations (B_Typ);
                  Check_Private_Overriding (B_Typ);
               end if;
            end if;

            --  For all subprograms, check formals

            Formal := First_Formal (S);
            while Present (Formal) loop
               if Ekind (Etype (Formal)) = E_Anonymous_Access_Type then
                  F_Typ := Designated_Type (Etype (Formal));
               else
                  F_Typ := Etype (Formal);
               end if;

               B_Typ := Base_Type (F_Typ);

               if Scope (B_Typ) = Current_Scope then
                  Set_Has_Primitive_Operations (B_Typ);
                  Check_Private_Overriding (B_Typ);
               end if;

               Next_Formal (Formal);
            end loop;
         end if;
      end Maybe_Primitive_Operation;

   --  Start of processing for New_Overloaded_Entity

   begin
      --  We need to look for an entity that S may override. This must be a
      --  homonym in the current scope, so we look for the first homonym of
      --  S in the current scope as the starting point for the search.

      E := Current_Entity_In_Scope (S);

      --  If there is no homonym then this is definitely not overriding

      if No (E) then
         Enter_Overloaded_Entity (S);
         Check_Dispatching_Operation (S, Empty);
         Maybe_Primitive_Operation;

         --  Ada 2005 (AI-397): Subprograms in the context of protected
         --  types have their overriding indicators checked in Sem_Ch9.

         if Ekind (S) not in Subprogram_Kind
           or else Ekind (Scope (S)) /= E_Protected_Type
         then
            Check_Overriding_Indicator (S, False);
         end if;

      --  If there is a homonym that is not overloadable, then we have an
      --  error, except for the special cases checked explicitly below.

      elsif not Is_Overloadable (E) then

         --  Check for spurious conflict produced by a subprogram that has the
         --  same name as that of the enclosing generic package. The conflict
         --  occurs within an instance, between the subprogram and the renaming
         --  declaration for the package. After the subprogram, the package
         --  renaming declaration becomes hidden.

         if Ekind (E) = E_Package
           and then Present (Renamed_Object (E))
           and then Renamed_Object (E) = Current_Scope
           and then Nkind (Parent (Renamed_Object (E))) =
                                                     N_Package_Specification
           and then Present (Generic_Parent (Parent (Renamed_Object (E))))
         then
            Set_Is_Hidden (E);
            Set_Is_Immediately_Visible (E, False);
            Enter_Overloaded_Entity (S);
            Set_Homonym (S, Homonym (E));
            Check_Dispatching_Operation (S, Empty);
            Check_Overriding_Indicator (S, False);

         --  If the subprogram is implicit it is hidden by the previous
         --  declaration. However if it is dispatching, it must appear in the
         --  dispatch table anyway, because it can be dispatched to even if it
         --  cannot be called directly.

         elsif Present (Alias (S))
           and then not Comes_From_Source (S)
         then
            Set_Scope (S, Current_Scope);

            if Is_Dispatching_Operation (Alias (S)) then
               Check_Dispatching_Operation (S, Empty);
            end if;

            return;

         else
            Error_Msg_Sloc := Sloc (E);
            Error_Msg_N ("& conflicts with declaration#", S);

            --  Useful additional warning

            if Is_Generic_Unit (E) then
               Error_Msg_N ("\previous generic unit cannot be overloaded", S);
            end if;

            return;
         end if;

      --  E exists and is overloadable

      else
         Is_Alias_Interface :=
            Present (Alias (S))
            and then Is_Dispatching_Operation (Alias (S))
            and then Present (DTC_Entity (Alias (S)))
            and then Is_Interface (Scope (DTC_Entity (Alias (S))));

         --  Loop through E and its homonyms to determine if any of them is
         --  the candidate for overriding by S.

         while Present (E) loop

            --  Definitely not interesting if not in the current scope

            if Scope (E) /= Current_Scope then
               null;

            --  Check if we have type conformance

            --  Ada 2005 (AI-251): In case of overriding an interface
            --  subprogram it is not an error that the old and new entities
            --  have the same profile, and hence we skip this code.

            elsif not Is_Alias_Interface
              and then Type_Conformant (E, S)

               --  Ada 2005 (AI-251): Do not consider here entities that cover
               --  abstract interface primitives. They will be handled after
               --  the overriden entity is found (see comments bellow inside
               --  this subprogram).

              and then not (Is_Subprogram (E)
                              and then Present (Abstract_Interface_Alias (E)))
            then
               --  If the old and new entities have the same profile and one
               --  is not the body of the other, then this is an error, unless
               --  one of them is implicitly declared.

               --  There are some cases when both can be implicit, for example
               --  when both a literal and a function that overrides it are
               --  inherited in a derivation, or when an inhertited operation
               --  of a tagged full type overrides the ineherited operation of
               --  a private extension. Ada 83 had a special rule for the the
               --  literal case. In Ada95, the later implicit operation hides
               --  the former, and the literal is always the former. In the
               --  odd case where both are derived operations declared at the
               --  same point, both operations should be declared, and in that
               --  case we bypass the following test and proceed to the next
               --  part (this can only occur for certain obscure cases
               --  involving homographs in instances and can't occur for
               --  dispatching operations ???). Note that the following
               --  condition is less than clear. For example, it's not at all
               --  clear why there's a test for E_Entry here. ???

               if Present (Alias (S))
                 and then (No (Alias (E))
                            or else Comes_From_Source (E)
                            or else Is_Dispatching_Operation (E))
                 and then
                   (Ekind (E) = E_Entry
                     or else Ekind (E) /= E_Enumeration_Literal)
               then
                  --  When an derived operation is overloaded it may be due to
                  --  the fact that the full view of a private extension
                  --  re-inherits. It has to be dealt with.

                  if Is_Package_Or_Generic_Package (Current_Scope)
                    and then In_Private_Part (Current_Scope)
                  then
                     Check_Operation_From_Private_View (S, E);
                  end if;

                  --  In any case the implicit operation remains hidden by
                  --  the existing declaration, which is overriding.

                  Set_Is_Overriding_Operation (E);

                  if Comes_From_Source (E) then
                     Check_Overriding_Indicator (E, True);

                     --  Indicate that E overrides the operation from which
                     --  S is inherited.

                     if  Present (Alias (S)) then
                        Set_Overridden_Operation (E, Alias (S));
                     else
                        Set_Overridden_Operation (E, S);
                     end if;
                  end if;

                  return;

                  --  Within an instance, the renaming declarations for
                  --  actual subprograms may become ambiguous, but they do
                  --  not hide each other.

               elsif Ekind (E) /= E_Entry
                 and then not Comes_From_Source (E)
                 and then not Is_Generic_Instance (E)
                 and then (Present (Alias (E))
                            or else Is_Intrinsic_Subprogram (E))
                 and then (not In_Instance
                            or else No (Parent (E))
                            or else Nkind (Unit_Declaration_Node (E)) /=
                               N_Subprogram_Renaming_Declaration)
               then
                  --  A subprogram child unit is not allowed to override
                  --  an inherited subprogram (10.1.1(20)).

                  if Is_Child_Unit (S) then
                     Error_Msg_N
                       ("child unit overrides inherited subprogram in parent",
                        S);
                     return;
                  end if;

                  if Is_Non_Overriding_Operation (E, S) then
                     Enter_Overloaded_Entity (S);
                     if No (Derived_Type)
                       or else Is_Tagged_Type (Derived_Type)
                     then
                        Check_Dispatching_Operation (S, Empty);
                     end if;

                     return;
                  end if;

                  --  E is a derived operation or an internal operator which
                  --  is being overridden. Remove E from further visibility.
                  --  Furthermore, if E is a dispatching operation, it must be
                  --  replaced in the list of primitive operations of its type
                  --  (see Override_Dispatching_Operation).

                  Does_Override := True;

                  declare
                     Prev : Entity_Id;

                  begin
                     Prev := First_Entity (Current_Scope);

                     while Present (Prev)
                       and then Next_Entity (Prev) /= E
                     loop
                        Next_Entity (Prev);
                     end loop;

                     --  It is possible for E to be in the current scope and
                     --  yet not in the entity chain. This can only occur in a
                     --  generic context where E is an implicit concatenation
                     --  in the formal part, because in a generic body the
                     --  entity chain starts with the formals.

                     pragma Assert
                       (Present (Prev) or else Chars (E) = Name_Op_Concat);

                     --  E must be removed both from the entity_list of the
                     --  current scope, and from the visibility chain

                     if Debug_Flag_E then
                        Write_Str ("Override implicit operation ");
                        Write_Int (Int (E));
                        Write_Eol;
                     end if;

                     --  If E is a predefined concatenation, it stands for four
                     --  different operations. As a result, a single explicit
                     --  declaration does not hide it. In a possible ambiguous
                     --  situation, Disambiguate chooses the user-defined op,
                     --  so it is correct to retain the previous internal one.

                     if Chars (E) /= Name_Op_Concat
                       or else Ekind (E) /= E_Operator
                     then
                        --  For nondispatching derived operations that are
                        --  overridden by a subprogram declared in the private
                        --  part of a package, we retain the derived
                        --  subprogram but mark it as not immediately visible.
                        --  If the derived operation was declared in the
                        --  visible part then this ensures that it will still
                        --  be visible outside the package with the proper
                        --  signature (calls from outside must also be
                        --  directed to this version rather than the
                        --  overriding one, unlike the dispatching case).
                        --  Calls from inside the package will still resolve
                        --  to the overriding subprogram since the derived one
                        --  is marked as not visible within the package.

                        --  If the private operation is dispatching, we achieve
                        --  the overriding by keeping the implicit operation
                        --  but setting its alias to be the overriding one. In
                        --  this fashion the proper body is executed in all
                        --  cases, but the original signature is used outside
                        --  of the package.

                        --  If the overriding is not in the private part, we
                        --  remove the implicit operation altogether.

                        if Is_Private_Declaration (S) then

                           if not Is_Dispatching_Operation (E) then
                              Set_Is_Immediately_Visible (E, False);
                           else
                              --  Work done in Override_Dispatching_Operation,
                              --  so nothing else need to be done here.

                              null;
                           end if;

                        else
                           --  Find predecessor of E in Homonym chain

                           if E = Current_Entity (E) then
                              Prev_Vis := Empty;
                           else
                              Prev_Vis := Current_Entity (E);
                              while Homonym (Prev_Vis) /= E loop
                                 Prev_Vis := Homonym (Prev_Vis);
                              end loop;
                           end if;

                           if Prev_Vis /= Empty then

                              --  Skip E in the visibility chain

                              Set_Homonym (Prev_Vis, Homonym (E));

                           else
                              Set_Name_Entity_Id (Chars (E), Homonym (E));
                           end if;

                           Set_Next_Entity (Prev, Next_Entity (E));

                           if No (Next_Entity (Prev)) then
                              Set_Last_Entity (Current_Scope, Prev);
                           end if;

                        end if;
                     end if;

                     Enter_Overloaded_Entity (S);
                     Set_Is_Overriding_Operation (S);
                     Check_Overriding_Indicator (S, True);

                     --  Indicate that S overrides the operation from which
                     --  E is inherited.

                     if Comes_From_Source (S) then
                        if Present (Alias (E)) then
                           Set_Overridden_Operation (S, Alias (E));
                        else
                           Set_Overridden_Operation (S, E);
                        end if;
                     end if;

                     if Is_Dispatching_Operation (E) then

                        --  An overriding dispatching subprogram inherits the
                        --  convention of the overridden subprogram (by
                        --  AI-117).

                        Set_Convention (S, Convention (E));

                        --  AI-251: For an entity overriding an interface
                        --  primitive check if the entity also covers other
                        --  abstract subprograms in the same scope. This is
                        --  required to handle the general case, that is,
                        --  1) overriding other interface primitives, and
                        --  2) overriding abstract subprograms inherited from
                        --  some abstract ancestor type.

                        if Has_Homonym (E)
                          and then Present (Alias (E))
                          and then Ekind (Alias (E)) /= E_Operator
                          and then Present (DTC_Entity (Alias (E)))
                          and then Is_Interface (Scope (DTC_Entity
                                                        (Alias (E))))
                        then
                           declare
                              E1 : Entity_Id;

                           begin
                              E1 := Homonym (E);
                              while Present (E1) loop
                                 if (Is_Overloadable (E1)
                                       or else Ekind (E1) = E_Subprogram_Type)
                                   and then Present (Alias (E1))
                                   and then Ekind (Alias (E1)) /= E_Operator
                                   and then Present (DTC_Entity (Alias (E1)))
                                   and then Is_Abstract
                                              (Scope (DTC_Entity (Alias (E1))))
                                   and then Type_Conformant (E1, S)
                                 then
                                    Check_Dispatching_Operation (S, E1);
                                 end if;

                                 E1 := Homonym (E1);
                              end loop;
                           end;
                        end if;

                        Check_Dispatching_Operation (S, E);

                        --  AI-251: Handle the case in which the entity
                        --  overrides a primitive operation that covered
                        --  several abstract interface primitives.

                        declare
                           E1 : Entity_Id;
                        begin
                           E1 := Current_Entity_In_Scope (S);
                           while Present (E1) loop
                              if Is_Subprogram (E1)
                                and then Present
                                           (Abstract_Interface_Alias (E1))
                                and then Alias (E1) = E
                              then
                                 Set_Alias (E1, S);
                              end if;

                              E1 := Homonym (E1);
                           end loop;
                        end;

                     else
                        Check_Dispatching_Operation (S, Empty);
                     end if;

                     Maybe_Primitive_Operation (Is_Overriding => True);
                     goto Check_Inequality;
                  end;

               --  Apparent redeclarations in instances can occur when two
               --  formal types get the same actual type. The subprograms in
               --  in the instance are legal,  even if not callable from the
               --  outside. Calls from within are disambiguated elsewhere.
               --  For dispatching operations in the visible part, the usual
               --  rules apply, and operations with the same profile are not
               --  legal (B830001).

               elsif (In_Instance_Visible_Part
                       and then not Is_Dispatching_Operation (E))
                 or else In_Instance_Not_Visible
               then
                  null;

               --  Here we have a real error (identical profile)

               else
                  Error_Msg_Sloc := Sloc (E);

                  --  Avoid cascaded errors if the entity appears in
                  --  subsequent calls.

                  Set_Scope (S, Current_Scope);

                  Error_Msg_N ("& conflicts with declaration#", S);

                  if Is_Generic_Instance (S)
                    and then not Has_Completion (E)
                  then
                     Error_Msg_N
                       ("\instantiation cannot provide body for it", S);
                  end if;

                  return;
               end if;

            else
               --  If one subprogram has an access parameter and the other
               --  a parameter of an access type, calls to either might be
               --  ambiguous. Verify that parameters match except for the
               --  access parameter.

               if May_Hide_Profile then
                  declare
                     F1    : Entity_Id;
                     F2    : Entity_Id;
                  begin
                     F1 := First_Formal (S);
                     F2 := First_Formal (E);
                     while Present (F1) and then Present (F2) loop
                        if Is_Access_Type (Etype (F1)) then
                           if not Is_Access_Type (Etype (F2))
                              or else not Conforming_Types
                                (Designated_Type (Etype (F1)),
                                 Designated_Type (Etype (F2)),
                                 Type_Conformant)
                           then
                              May_Hide_Profile := False;
                           end if;

                        elsif
                          not Conforming_Types
                            (Etype (F1), Etype (F2), Type_Conformant)
                        then
                           May_Hide_Profile := False;
                        end if;

                        Next_Formal (F1);
                        Next_Formal (F2);
                     end loop;

                     if May_Hide_Profile
                       and then No (F1)
                       and then No (F2)
                     then
                        Error_Msg_NE ("calls to& may be ambiguous?", S, S);
                     end if;
                  end;
               end if;
            end if;

            Prev_Vis := E;
            E := Homonym (E);
         end loop;

         --  On exit, we know that S is a new entity

         Enter_Overloaded_Entity (S);
         Maybe_Primitive_Operation;
         Check_Overriding_Indicator (S, Does_Override);

         --  If S is a derived operation for an untagged type then by
         --  definition it's not a dispatching operation (even if the parent
         --  operation was dispatching), so we don't call
         --  Check_Dispatching_Operation in that case.

         if No (Derived_Type)
           or else Is_Tagged_Type (Derived_Type)
         then
            Check_Dispatching_Operation (S, Empty);
         end if;
      end if;

      --  If this is a user-defined equality operator that is not a derived
      --  subprogram, create the corresponding inequality. If the operation is
      --  dispatching, the expansion is done elsewhere, and we do not create
      --  an explicit inequality operation.

      <<Check_Inequality>>
         if Chars (S) = Name_Op_Eq
           and then Etype (S) = Standard_Boolean
           and then Present (Parent (S))
           and then not Is_Dispatching_Operation (S)
         then
            Make_Inequality_Operator (S);
         end if;
   end New_Overloaded_Entity;

   ---------------------
   -- Process_Formals --
   ---------------------

   procedure Process_Formals
     (T           : List_Id;
      Related_Nod : Node_Id)
   is
      Param_Spec  : Node_Id;
      Formal      : Entity_Id;
      Formal_Type : Entity_Id;
      Default     : Node_Id;
      Ptype       : Entity_Id;

      function Is_Class_Wide_Default (D : Node_Id) return Boolean;
      --  Check whether the default has a class-wide type. After analysis the
      --  default has the type of the formal, so we must also check explicitly
      --  for an access attribute.

      ---------------------------
      -- Is_Class_Wide_Default --
      ---------------------------

      function Is_Class_Wide_Default (D : Node_Id) return Boolean is
      begin
         return Is_Class_Wide_Type (Designated_Type (Etype (D)))
           or else (Nkind (D) =  N_Attribute_Reference
                      and then Attribute_Name (D) = Name_Access
                      and then Is_Class_Wide_Type (Etype (Prefix (D))));
      end Is_Class_Wide_Default;

   --  Start of processing for Process_Formals

   begin
      --  In order to prevent premature use of the formals in the same formal
      --  part, the Ekind is left undefined until all default expressions are
      --  analyzed. The Ekind is established in a separate loop at the end.

      Param_Spec := First (T);

      while Present (Param_Spec) loop

         Formal := Defining_Identifier (Param_Spec);
         Enter_Name (Formal);

         --  Case of ordinary parameters

         if Nkind (Parameter_Type (Param_Spec)) /= N_Access_Definition then
            Find_Type (Parameter_Type (Param_Spec));
            Ptype := Parameter_Type (Param_Spec);

            if Ptype = Error then
               goto Continue;
            end if;

            Formal_Type := Entity (Ptype);

            if Ekind (Formal_Type) = E_Incomplete_Type
              or else (Is_Class_Wide_Type (Formal_Type)
                        and then Ekind (Root_Type (Formal_Type)) =
                                                         E_Incomplete_Type)
            then
               --  Ada 2005 (AI-326): Tagged incomplete types allowed

               if Is_Tagged_Type (Formal_Type) then
                  null;

               elsif Nkind (Parent (T)) /= N_Access_Function_Definition
                 and then Nkind (Parent (T)) /= N_Access_Procedure_Definition
               then
                  Error_Msg_N ("invalid use of incomplete type", Param_Spec);
               end if;

            elsif Ekind (Formal_Type) = E_Void then
               Error_Msg_NE ("premature use of&",
                 Parameter_Type (Param_Spec), Formal_Type);
            end if;

            --  Ada 2005 (AI-231): Create and decorate an internal subtype
            --  declaration corresponding to the null-excluding type of the
            --  formal in the enclosing scope. Finally, replace the parameter
            --  type of the formal with the internal subtype.

            if Ada_Version >= Ada_05
              and then Is_Access_Type (Formal_Type)
              and then Null_Exclusion_Present (Param_Spec)
            then
               if Can_Never_Be_Null (Formal_Type)
                 and then Comes_From_Source (Related_Nod)
               then
                  Error_Msg_N
                    ("null exclusion must apply to a type that does not "
                       & "exclude null ('R'M 3.10 (14)", Related_Nod);
               end if;

               Formal_Type :=
                 Create_Null_Excluding_Itype
                   (T           => Formal_Type,
                    Related_Nod => Related_Nod,
                    Scope_Id    => Scope (Current_Scope));
            end if;

         --  An access formal type

         else
            Formal_Type :=
              Access_Definition (Related_Nod, Parameter_Type (Param_Spec));

            --  Ada 2005 (AI-254)

            declare
               AD : constant Node_Id :=
                      Access_To_Subprogram_Definition
                        (Parameter_Type (Param_Spec));
            begin
               if Present (AD) and then Protected_Present (AD) then
                  Formal_Type :=
                    Replace_Anonymous_Access_To_Protected_Subprogram
                      (Param_Spec, Formal_Type);
               end if;
            end;
         end if;

         Set_Etype (Formal, Formal_Type);
         Default := Expression (Param_Spec);

         if Present (Default) then
            if Out_Present (Param_Spec) then
               Error_Msg_N
                 ("default initialization only allowed for IN parameters",
                  Param_Spec);
            end if;

            --  Do the special preanalysis of the expression (see section on
            --  "Handling of Default Expressions" in the spec of package Sem).

            Analyze_Per_Use_Expression (Default, Formal_Type);

            --  Check that the designated type of an access parameter's default
            --  is not a class-wide type unless the parameter's designated type
            --  is also class-wide.

            if Ekind (Formal_Type) = E_Anonymous_Access_Type
              and then not From_With_Type (Formal_Type)
              and then Is_Class_Wide_Default (Default)
              and then not Is_Class_Wide_Type (Designated_Type (Formal_Type))
            then
               Error_Msg_N
                 ("access to class-wide expression not allowed here", Default);
            end if;
         end if;

         --  Ada 2005 (AI-231): Static checks

         if Ada_Version >= Ada_05
           and then Is_Access_Type (Etype (Formal))
           and then Can_Never_Be_Null (Etype (Formal))
         then
            Null_Exclusion_Static_Checks (Param_Spec);
         end if;

      <<Continue>>
         Next (Param_Spec);
      end loop;

      --  If this is the formal part of a function specification, analyze the
      --  subtype mark in the context where the formals are visible but not
      --  yet usable, and may hide outer homographs.

      if Nkind (Related_Nod) = N_Function_Specification then
         Analyze_Return_Type (Related_Nod);
      end if;

      --  Now set the kind (mode) of each formal

      Param_Spec := First (T);

      while Present (Param_Spec) loop
         Formal := Defining_Identifier (Param_Spec);
         Set_Formal_Mode (Formal);

         if Ekind (Formal) = E_In_Parameter then
            Set_Default_Value (Formal, Expression (Param_Spec));

            if Present (Expression (Param_Spec)) then
               Default :=  Expression (Param_Spec);

               if Is_Scalar_Type (Etype (Default)) then
                  if Nkind
                       (Parameter_Type (Param_Spec)) /= N_Access_Definition
                  then
                     Formal_Type := Entity (Parameter_Type (Param_Spec));

                  else
                     Formal_Type := Access_Definition
                       (Related_Nod, Parameter_Type (Param_Spec));
                  end if;

                  Apply_Scalar_Range_Check (Default, Formal_Type);
               end if;
            end if;
         end if;

         Next (Param_Spec);
      end loop;

   end Process_Formals;

   ----------------------------
   -- Reference_Body_Formals --
   ----------------------------

   procedure Reference_Body_Formals (Spec : Entity_Id; Bod : Entity_Id) is
      Fs : Entity_Id;
      Fb : Entity_Id;

   begin
      if Error_Posted (Spec) then
         return;
      end if;

      Fs := First_Formal (Spec);
      Fb := First_Formal (Bod);

      while Present (Fs) loop
         Generate_Reference (Fs, Fb, 'b');

         if Style_Check then
            Style.Check_Identifier (Fb, Fs);
         end if;

         Set_Spec_Entity (Fb, Fs);
         Set_Referenced (Fs, False);
         Next_Formal (Fs);
         Next_Formal (Fb);
      end loop;
   end Reference_Body_Formals;

   -------------------------
   -- Set_Actual_Subtypes --
   -------------------------

   procedure Set_Actual_Subtypes (N : Node_Id; Subp : Entity_Id) is
      Loc            : constant Source_Ptr := Sloc (N);
      Decl           : Node_Id;
      Formal         : Entity_Id;
      T              : Entity_Id;
      First_Stmt     : Node_Id := Empty;
      AS_Needed      : Boolean;

   begin
      --  If this is an emtpy initialization procedure, no need to create
      --  actual subtypes (small optimization).

      if Ekind (Subp) = E_Procedure
        and then Is_Null_Init_Proc (Subp)
      then
         return;
      end if;

      Formal := First_Formal (Subp);
      while Present (Formal) loop
         T := Etype (Formal);

         --  We never need an actual subtype for a constrained formal

         if Is_Constrained (T) then
            AS_Needed := False;

         --  If we have unknown discriminants, then we do not need an actual
         --  subtype, or more accurately we cannot figure it out! Note that
         --  all class-wide types have unknown discriminants.

         elsif Has_Unknown_Discriminants (T) then
            AS_Needed := False;

         --  At this stage we have an unconstrained type that may need an
         --  actual subtype. For sure the actual subtype is needed if we have
         --  an unconstrained array type.

         elsif Is_Array_Type (T) then
            AS_Needed := True;

         --  The only other case needing an actual subtype is an unconstrained
         --  record type which is an IN parameter (we cannot generate actual
         --  subtypes for the OUT or IN OUT case, since an assignment can
         --  change the discriminant values. However we exclude the case of
         --  initialization procedures, since discriminants are handled very
         --  specially in this context, see the section entitled "Handling of
         --  Discriminants" in Einfo.

         --  We also exclude the case of Discrim_SO_Functions (functions used
         --  in front end layout mode for size/offset values), since in such
         --  functions only discriminants are referenced, and not only are such
         --  subtypes not needed, but they cannot always be generated, because
         --  of order of elaboration issues.

         elsif Is_Record_Type (T)
           and then Ekind (Formal) = E_In_Parameter
           and then Chars (Formal) /= Name_uInit
           and then not Is_Unchecked_Union (T)
           and then not Is_Discrim_SO_Function (Subp)
         then
            AS_Needed := True;

         --  All other cases do not need an actual subtype

         else
            AS_Needed := False;
         end if;

         --  Generate actual subtypes for unconstrained arrays and
         --  unconstrained discriminated records.

         if AS_Needed then
            if Nkind (N) = N_Accept_Statement then

               --  If expansion is active, The formal is replaced by a local
               --  variable that renames the corresponding entry of the
               --  parameter block, and it is this local variable that may
               --  require an actual subtype.

               if Expander_Active then
                  Decl := Build_Actual_Subtype (T, Renamed_Object (Formal));
               else
                  Decl := Build_Actual_Subtype (T, Formal);
               end if;

               if Present (Handled_Statement_Sequence (N)) then
                  First_Stmt :=
                    First (Statements (Handled_Statement_Sequence (N)));
                  Prepend (Decl, Statements (Handled_Statement_Sequence (N)));
                  Mark_Rewrite_Insertion (Decl);
               else
                  --  If the accept statement has no body, there will be no
                  --  reference to the actuals, so no need to compute actual
                  --  subtypes.

                  return;
               end if;

            else
               Decl := Build_Actual_Subtype (T, Formal);
               Prepend (Decl, Declarations (N));
               Mark_Rewrite_Insertion (Decl);
            end if;

            --  The declaration uses the bounds of an existing object, and
            --  therefore needs no constraint checks.

            Analyze (Decl, Suppress => All_Checks);

            --  We need to freeze manually the generated type when it is
            --  inserted anywhere else than in a declarative part.

            if Present (First_Stmt) then
               Insert_List_Before_And_Analyze (First_Stmt,
                 Freeze_Entity (Defining_Identifier (Decl), Loc));
            end if;

            if Nkind (N) = N_Accept_Statement
              and then Expander_Active
            then
               Set_Actual_Subtype (Renamed_Object (Formal),
                 Defining_Identifier (Decl));
            else
               Set_Actual_Subtype (Formal, Defining_Identifier (Decl));
            end if;
         end if;

         Next_Formal (Formal);
      end loop;
   end Set_Actual_Subtypes;

   ---------------------
   -- Set_Formal_Mode --
   ---------------------

   procedure Set_Formal_Mode (Formal_Id : Entity_Id) is
      Spec : constant Node_Id := Parent (Formal_Id);

   begin
      --  Note: we set Is_Known_Valid for IN parameters and IN OUT parameters
      --  since we ensure that corresponding actuals are always valid at the
      --  point of the call.

      if Out_Present (Spec) then
         if Ekind (Scope (Formal_Id)) = E_Function
           or else Ekind (Scope (Formal_Id)) = E_Generic_Function
         then
            Error_Msg_N ("functions can only have IN parameters", Spec);
            Set_Ekind (Formal_Id, E_In_Parameter);

         elsif In_Present (Spec) then
            Set_Ekind (Formal_Id, E_In_Out_Parameter);

         else
            Set_Ekind               (Formal_Id, E_Out_Parameter);
            Set_Never_Set_In_Source (Formal_Id, True);
            Set_Is_True_Constant    (Formal_Id, False);
            Set_Current_Value       (Formal_Id, Empty);
         end if;

      else
         Set_Ekind (Formal_Id, E_In_Parameter);
      end if;

      --  Set Is_Known_Non_Null for access parameters since the language
      --  guarantees that access parameters are always non-null. We also set
      --  Can_Never_Be_Null, since there is no way to change the value.

      if Nkind (Parameter_Type (Spec)) = N_Access_Definition then

         --  Ada 2005 (AI-231): In Ada95, access parameters are always non-
         --  null; In Ada 2005, only if then null_exclusion is explicit.

         if Ada_Version < Ada_05
           or else Can_Never_Be_Null (Etype (Formal_Id))
         then
            Set_Is_Known_Non_Null (Formal_Id);
            Set_Can_Never_Be_Null (Formal_Id);
         end if;

      --  Ada 2005 (AI-231): Null-exclusion access subtype

      elsif Is_Access_Type (Etype (Formal_Id))
        and then Can_Never_Be_Null (Etype (Formal_Id))
      then
         Set_Is_Known_Non_Null (Formal_Id);
      end if;

      Set_Mechanism (Formal_Id, Default_Mechanism);
      Set_Formal_Validity (Formal_Id);
   end Set_Formal_Mode;

   -------------------------
   -- Set_Formal_Validity --
   -------------------------

   procedure Set_Formal_Validity (Formal_Id : Entity_Id) is
   begin
      --  If no validity checking, then we cannot assume anything about the
      --  validity of parameters, since we do not know there is any checking
      --  of the validity on the call side.

      if not Validity_Checks_On then
         return;

      --  If validity checking for parameters is enabled, this means we are
      --  not supposed to make any assumptions about argument values.

      elsif Validity_Check_Parameters then
         return;

      --  If we are checking in parameters, we will assume that the caller is
      --  also checking parameters, so we can assume the parameter is valid.

      elsif Ekind (Formal_Id) = E_In_Parameter
        and then Validity_Check_In_Params
      then
         Set_Is_Known_Valid (Formal_Id, True);

      --  Similar treatment for IN OUT parameters

      elsif Ekind (Formal_Id) = E_In_Out_Parameter
        and then Validity_Check_In_Out_Params
      then
         Set_Is_Known_Valid (Formal_Id, True);
      end if;
   end Set_Formal_Validity;

   ------------------------
   -- Subtype_Conformant --
   ------------------------

   function Subtype_Conformant (New_Id, Old_Id : Entity_Id) return Boolean is
      Result : Boolean;
   begin
      Check_Conformance (New_Id, Old_Id, Subtype_Conformant, False, Result);
      return Result;
   end Subtype_Conformant;

   ---------------------
   -- Type_Conformant --
   ---------------------

   function Type_Conformant
     (New_Id                   : Entity_Id;
      Old_Id                   : Entity_Id;
      Skip_Controlling_Formals : Boolean := False) return Boolean
   is
      Result : Boolean;
   begin
      May_Hide_Profile := False;

      Check_Conformance
        (New_Id, Old_Id, Type_Conformant, False, Result,
         Skip_Controlling_Formals => Skip_Controlling_Formals);
      return Result;
   end Type_Conformant;

   -------------------------------
   -- Valid_Operator_Definition --
   -------------------------------

   procedure Valid_Operator_Definition (Designator : Entity_Id) is
      N    : Integer := 0;
      F    : Entity_Id;
      Id   : constant Name_Id := Chars (Designator);
      N_OK : Boolean;

   begin
      F := First_Formal (Designator);
      while Present (F) loop
         N := N + 1;

         if Present (Default_Value (F)) then
            Error_Msg_N
              ("default values not allowed for operator parameters",
               Parent (F));
         end if;

         Next_Formal (F);
      end loop;

      --  Verify that user-defined operators have proper number of arguments
      --  First case of operators which can only be unary

      if Id = Name_Op_Not
        or else Id = Name_Op_Abs
      then
         N_OK := (N = 1);

      --  Case of operators which can be unary or binary

      elsif Id = Name_Op_Add
        or Id = Name_Op_Subtract
      then
         N_OK := (N in 1 .. 2);

      --  All other operators can only be binary

      else
         N_OK := (N = 2);
      end if;

      if not N_OK then
         Error_Msg_N
           ("incorrect number of arguments for operator", Designator);
      end if;

      if Id = Name_Op_Ne
        and then Base_Type (Etype (Designator)) = Standard_Boolean
        and then not Is_Intrinsic_Subprogram (Designator)
      then
         Error_Msg_N
            ("explicit definition of inequality not allowed", Designator);
      end if;
   end Valid_Operator_Definition;

end Sem_Ch6;
