------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 6                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-2002, Free Software Foundation, Inc.         --
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
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Expander; use Expander;
with Exp_Ch7;  use Exp_Ch7;
with Freeze;   use Freeze;
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

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Analyze_Generic_Subprogram_Body (N : Node_Id; Gen_Id : Entity_Id);
   --  Analyze a generic subprogram body

   function Build_Body_To_Inline
     (N         : Node_Id;
      Subp      : Entity_Id;
      Orig_Body : Node_Id)
      return      Boolean;
   --  If a subprogram has pragma Inline and inlining is active, use generic
   --  machinery to build an unexpanded body for the subprogram. This body is
   --  subsequenty used for inline expansions at call sites. If subprogram can
   --  be inlined (depending on size and nature of local declarations) this
   --  function returns true. Otherwise subprogram body is treated normally.

   type Conformance_Type is
     (Type_Conformant, Mode_Conformant, Subtype_Conformant, Fully_Conformant);
   --  Conformance type used for following call, meaning matches the
   --  RM definitions of the corresponding terms.

   procedure Check_Conformance
     (New_Id   : Entity_Id;
      Old_Id   : Entity_Id;
      Ctype    : Conformance_Type;
      Errmsg   : Boolean;
      Conforms : out Boolean;
      Err_Loc  : Node_Id := Empty;
      Get_Inst : Boolean := False);
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

   procedure Check_Subprogram_Order (N : Node_Id);
   --  N is the N_Subprogram_Body node for a subprogram. This routine applies
   --  the alpha ordering rule for N if this ordering requirement applicable.

   function Is_Non_Overriding_Operation
     (Prev_E : Entity_Id;
      New_E  : Entity_Id)
      return   Boolean;
   --  Enforce the rule given in 12.3(18): a private operation in an instance
   --  overrides an inherited operation only if the corresponding operation
   --  was overriding in the generic. This can happen for primitive operations
   --  of types derived (in the generic unit) from formal private or formal
   --  derived types.

   procedure Check_Returns
     (HSS  : Node_Id;
      Mode : Character;
      Err  : out Boolean);
   --  Called to check for missing return statements in a function body,
   --  or for returns present in a procedure body which has No_Return set.
   --  L is the handled statement sequence for the subprogram body. This
   --  procedure checks all flow paths to make sure they either have a
   --  return (Mode = 'F') or do not have a return (Mode = 'P'). The flag
   --  Err is set if there are any control paths not explicitly terminated
   --  by a return in the function case, and is True otherwise.

   function Conforming_Types
     (T1       : Entity_Id;
      T2       : Entity_Id;
      Ctype    : Conformance_Type;
      Get_Inst : Boolean := False)
      return     Boolean;
   --  Check that two formal parameter types conform, checking both
   --  for equality of base types, and where required statically
   --  matching subtypes, depending on the setting of Ctype.

   procedure Enter_Overloaded_Entity (S : Entity_Id);
   --  This procedure makes S, a new overloaded entity, into the first
   --  visible entity with that name.

   procedure Install_Entity (E : Entity_Id);
   --  Make single entity visible. Used for generic formals as well.

   procedure Install_Formals (Id : Entity_Id);
   --  On entry to a subprogram body, make the formals visible. Note
   --  that simply placing the subprogram on the scope stack is not
   --  sufficient: the formals must become the current entities for
   --  their names.

   procedure Make_Inequality_Operator (S : Entity_Id);
   --  Create the declaration for an inequality operator that is implicitly
   --  created by a user-defined equality operator that yields a boolean.

   procedure May_Need_Actuals (Fun : Entity_Id);
   --  Flag functions that can be called without parameters, i.e. those that
   --  have no parameters, or those for which defaults exist for all parameters

   procedure Set_Formal_Validity (Formal_Id : Entity_Id);
   --  Formal_Id is an formal parameter entity. This procedure deals with
   --  setting the proper validity status for this entity, which depends
   --  on the kind of parameter and the validity checking mode.

   ---------------------------------------------
   -- Analyze_Abstract_Subprogram_Declaration --
   ---------------------------------------------

   procedure Analyze_Abstract_Subprogram_Declaration (N : Node_Id) is
      Designator : constant Entity_Id := Analyze_Spec (Specification (N));
      Scop       : constant Entity_Id := Current_Scope;

   begin
      Generate_Definition (Designator);
      Set_Is_Abstract (Designator);
      New_Overloaded_Entity (Designator);
      Check_Delayed_Subprogram (Designator);

      Set_Is_Pure (Designator,
        Is_Pure (Scop) and then Is_Library_Level_Entity (Designator));
      Set_Is_Remote_Call_Interface (
        Designator, Is_Remote_Call_Interface (Scop));
      Set_Is_Remote_Types (Designator, Is_Remote_Types (Scop));

      if Ekind (Scope (Designator)) = E_Protected_Type then
         Error_Msg_N
           ("abstract subprogram not allowed in protected type", N);
      end if;
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
      Gen_Decl : constant Node_Id := Unit_Declaration_Node (Gen_Id);
      Spec     : Node_Id;
      Kind     : constant Entity_Kind := Ekind (Gen_Id);
      Nam      : Entity_Id;
      New_N    : Node_Id;

   begin
      --  Copy body and disable expansion while analyzing the generic
      --  For a stub, do not copy the stub (which would load the proper body),
      --  this will be done when the proper body is analyzed.

      if Nkind (N) /= N_Subprogram_Body_Stub then
         New_N := Copy_Generic_Node (N, Empty, Instantiating => False);
         Rewrite (N, New_N);
         Start_Generic;
      end if;

      Spec := Specification (N);

      --  Within the body of the generic, the subprogram is callable, and
      --  behaves like the corresponding non-generic unit.

      Nam := Defining_Entity (Spec);

      if Kind = E_Generic_Procedure
        and then Nkind (Spec) /= N_Procedure_Specification
      then
         Error_Msg_N ("invalid body for generic procedure ", Nam);
         return;

      elsif Kind = E_Generic_Function
        and then Nkind (Spec) /= N_Function_Specification
      then
         Error_Msg_N ("invalid body for generic function ", Nam);
         return;
      end if;

      Set_Corresponding_Body (Gen_Decl, Nam);

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

         Nam := Analyze_Spec (Spec);

         if Nkind (N) = N_Subprogram_Body_Stub then

            --  Nothing to do if no body to process

            Set_Ekind (Nam, Kind);
            End_Scope;
            return;
         end if;

         if Present (E) then

            --  E is the first formal parameter, which must be the first
            --  entity in the subprogram body.

            Set_First_Entity (Gen_Id, E);

            --  Now make formal parameters visible

            while Present (E) loop
               Install_Entity (E);
               Next_Formal (E);
            end loop;
         end if;

         --  Visible generic entity is callable within its own body.

         Set_Ekind (Gen_Id, Ekind (Nam));
         Set_Convention (Nam, Convention (Gen_Id));
         Set_Scope (Nam, Scope (Gen_Id));
         Check_Fully_Conformant (Nam, Gen_Id, Nam);

         --  If this is a compilation unit, it must be made visible
         --  explicitly, because the compilation of the declaration,
         --  unlike other library unit declarations, does not. If it
         --  is not a unit, the following is redundant but harmless.

         Set_Is_Immediately_Visible (Gen_Id);

         Set_Actual_Subtypes (N, Current_Scope);
         Analyze_Declarations (Declarations (N));
         Check_Completion;
         Analyze (Handled_Statement_Sequence (N));

         Save_Global_References (Original_Node (N));

         --  Prior to exiting the scope, include generic formals again
         --  (if any are present) in the set of local entities.

         if Present (First_Ent) then
            Set_First_Entity (Gen_Id, First_Ent);
         end if;

      end;

      End_Scope;
      Check_Subprogram_Order (N);

      --  Outside of its body, unit is generic again.

      Set_Ekind (Gen_Id, Kind);
      Set_Ekind (Nam, E_Subprogram_Body);
      Generate_Reference (Gen_Id, Nam, 'b');
      Style.Check_Identifier (Nam, Gen_Id);
      End_Generic;

   end Analyze_Generic_Subprogram_Body;

   -----------------------------
   -- Analyze_Operator_Symbol --
   -----------------------------

   --  An operator symbol such as "+" or "and" may appear in context where
   --  the literal denotes an entity name, such as  "+"(x, y) or in a
   --  context when it is just a string, as in  (conjunction = "or"). In
   --  these cases the parser generates this node, and the semantics does
   --  the disambiguation. Other such case are actuals in an instantiation,
   --  the generic unit in an instantiation, and pragma arguments.

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
         --  Can be call to parameterless entry family. What appears to be
         --  the sole argument is in fact the entry index. Rewrite prefix
         --  of node accordingly. Source representation is unchanged by this
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

      --  The name can be a selected component or an indexed component
      --  that yields an access to subprogram. Such a prefix is legal if
      --  the call has parameter associations.

      elsif Is_Access_Type (Etype (P))
        and then Ekind (Designated_Type (Etype (P))) = E_Subprogram_Type
      then
         if Present (Actuals) then
            Analyze_Call_And_Resolve;
         else
            Error_Msg_N ("missing explicit dereference in call ", N);
         end if;

      --  If not an access to subprogram, then the prefix must resolve to
      --  the name of an entry, entry family, or protected operation.

      --  For the case of a simple entry call, P is a selected component
      --  where the prefix is the task and the selector name is the entry.
      --  A call to a protected procedure will have the same syntax. If
      --  the protected object contains overloaded operations, the entity
      --  may appear as a function, the context will select the operation
      --  whose type is Void.

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
         --  Can be call to parameterless entry family. What appears to be
         --  the sole argument is in fact the entry index. Rewrite prefix
         --  of node accordingly. Source representation is unchanged by this
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

      --  Anything else is an error.

      else
         Error_Msg_N ("Invalid procedure or entry call", N);
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

            if (Is_Class_Wide_Type (Etype (Expr))
                 or else Is_Dynamically_Tagged (Expr))
              and then not Is_Class_Wide_Type (R_Type)
            then
               Error_Msg_N
                 ("dynamically tagged expression not allowed!", Expr);
            end if;

            Apply_Constraint_Check (Expr, R_Type);

            --  ??? A real run-time accessibility check is needed
            --  in cases involving dereferences of access parameters.
            --  For now we just check the static cases.

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
                 ("& will be raised at run time?!",
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
           and then  No_Return (Scope_Id)
         then
            Error_Msg_N
              ("RETURN statement not allowed (No_Return)", N);
         end if;
      end if;

      Check_Unreachable_Code (N);
   end Analyze_Return_Statement;

   ------------------
   -- Analyze_Spec --
   ------------------

   function Analyze_Spec (N : Node_Id) return Entity_Id is
      Designator : constant Entity_Id := Defining_Entity (N);
      Formals    : constant List_Id   := Parameter_Specifications (N);
      Typ        : Entity_Id;

   begin
      Generate_Definition (Designator);

      if Nkind (N) = N_Function_Specification then
         Set_Ekind (Designator, E_Function);
         Set_Mechanism (Designator, Default_Mechanism);

         if Subtype_Mark (N) /= Error then
            Find_Type (Subtype_Mark (N));
            Typ := Entity (Subtype_Mark (N));
            Set_Etype (Designator, Typ);

            if (Ekind (Typ) = E_Incomplete_Type
                 or else (Is_Class_Wide_Type (Typ)
                           and then
                             Ekind (Root_Type (Typ)) = E_Incomplete_Type))
            then
               Error_Msg_N
                 ("invalid use of incomplete type", Subtype_Mark (N));
            end if;

         else
            Set_Etype (Designator, Any_Type);
         end if;

      else
         Set_Ekind (Designator, E_Procedure);
         Set_Etype (Designator, Standard_Void_Type);
      end if;

      if Present (Formals) then
         Set_Scope (Designator, Current_Scope);
         New_Scope (Designator);
         Process_Formals (Formals, N);
         End_Scope;
      end if;

      if Nkind (N) = N_Function_Specification then
         if Nkind (Designator) = N_Defining_Operator_Symbol then
            Valid_Operator_Definition (Designator);
         end if;

         May_Need_Actuals (Designator);

         if Is_Abstract (Etype (Designator))
           and then Nkind (Parent (N)) /= N_Abstract_Subprogram_Declaration
         then
            Error_Msg_N
              ("function that returns abstract type must be abstract", N);
         end if;
      end if;

      return Designator;
   end Analyze_Spec;

   -----------------------------
   -- Analyze_Subprogram_Body --
   -----------------------------

   --  This procedure is called for regular subprogram bodies, generic bodies,
   --  and for subprogram stubs of both kinds. In the case of stubs, only the
   --  specification matters, and is used to create a proper declaration for
   --  the subprogram, or to perform conformance checks.

   procedure Analyze_Subprogram_Body (N : Node_Id) is
      Loc       : constant Source_Ptr := Sloc (N);
      Body_Spec : constant Node_Id    := Specification (N);
      Body_Id   : Entity_Id           := Defining_Entity (Body_Spec);
      Prev_Id   : constant Entity_Id  := Current_Entity_In_Scope (Body_Id);

      HSS          : Node_Id;
      Spec_Id      : Entity_Id;
      Spec_Decl    : Node_Id   := Empty;
      Last_Formal  : Entity_Id := Empty;
      Conformant   : Boolean;
      Missing_Ret  : Boolean;
      Body_Deleted : Boolean := False;
      P_Ent        : Entity_Id;

   begin
      if Debug_Flag_C then
         Write_Str ("====  Compiling subprogram body ");
         Write_Name (Chars (Body_Id));
         Write_Str (" from ");
         Write_Location (Loc);
         Write_Eol;
      end if;

      Trace_Scope (N, Body_Id, " Analyze subprogram");

      --  Generic subprograms are handled separately. They always have
      --  a generic specification. Determine whether current scope has
      --  a previous declaration.

      --  If the subprogram body is defined within an instance of the
      --  same name, the instance appears as a package renaming, and
      --  will be hidden within the subprogram.

      if Present (Prev_Id)
        and then not Is_Overloadable (Prev_Id)
        and then (Nkind (Parent (Prev_Id)) /= N_Package_Renaming_Declaration
                   or else Comes_From_Source (Prev_Id))
      then
         if Ekind (Prev_Id) = E_Generic_Procedure
           or else Ekind (Prev_Id) = E_Generic_Function
         then
            Spec_Id := Prev_Id;
            Set_Is_Compilation_Unit (Body_Id, Is_Compilation_Unit (Spec_Id));
            Set_Is_Child_Unit       (Body_Id, Is_Child_Unit       (Spec_Id));

            Analyze_Generic_Subprogram_Body (N, Spec_Id);
            return;

         else
            --  Previous entity conflicts with subprogram name.
            --  Attempting to enter name will post error.

            Enter_Name (Body_Id);
            return;
         end if;

      --  Non-generic case, find the subprogram declaration, if one was
      --  seen, or enter new overloaded entity in the current scope.
      --  If the current_entity is the body_id itself, the unit is being
      --  analyzed as part of the context of one of its subunits. No need
      --  to redo the analysis.

      elsif Prev_Id = Body_Id
        and then Has_Completion (Body_Id)
      then
         return;

      else
         Body_Id := Analyze_Spec (Body_Spec);

         if Nkind (N) = N_Subprogram_Body_Stub
           or else No (Corresponding_Spec (N))
         then
            Spec_Id := Find_Corresponding_Spec (N);

            --  If this is a duplicate body, no point in analyzing it

            if Error_Posted (N) then
               return;
            end if;

            --  A subprogram body should cause freezing of its own
            --  declaration, but if there was no previous explicit
            --  declaration, then the subprogram will get frozen too
            --  late (there may be code within the body that depends
            --  on the subprogram having been frozen, such as uses of
            --  extra formals), so we force it to be frozen here.
            --  Same holds if the body and the spec are compilation units.

            if No (Spec_Id) then
               Freeze_Before (N, Body_Id);

            elsif Nkind (Parent (N)) = N_Compilation_Unit then
               Freeze_Before (N, Spec_Id);
            end if;
         else
            Spec_Id := Corresponding_Spec (N);
         end if;
      end if;

      --  Do not inline any subprogram that contains nested subprograms,
      --  since the backend inlining circuit seems to generate uninitialized
      --  references in this case. We know this happens in the case of front
      --  end ZCX support, but it also appears it can happen in other cases
      --  as well. The backend often rejects attempts to inline in the case
      --  of nested procedures anyway, so little if anything is lost by this.

      --  Do not do this test if errors have been detected, because in some
      --  error cases, this code blows up, and we don't need it anyway if
      --  there have been errors, since we won't get to the linker anyway.

      if Serious_Errors_Detected = 0 then
         P_Ent := Body_Id;
         loop
            P_Ent := Scope (P_Ent);
            exit when No (P_Ent) or else P_Ent = Standard_Standard;

            if Is_Subprogram (P_Ent) and then Is_Inlined (P_Ent) then
               Set_Is_Inlined (P_Ent, False);

               if Comes_From_Source (P_Ent)
                 and then Ineffective_Inline_Warnings
                 and then Has_Pragma_Inline (P_Ent)
               then
                  Error_Msg_NE
                    ("?pragma Inline for & ignored (has nested subprogram)",
                     Get_Rep_Pragma (P_Ent, Name_Inline), P_Ent);
               end if;
            end if;
         end loop;
      end if;

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

            --  The protected operation always has at least one formal,
            --  namely the object itself, but it is only placed in the
            --  parameter list if expansion is enabled.

            if Present (Formal)
              or else Expander_Active
            then
               Plist := New_List;

            else
               Plist := No_List;
            end if;

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
                    Subtype_Mark => New_Occurrence_Of (Etype (Body_Id), Loc));
            end if;

            Decl :=
              Make_Subprogram_Declaration (Loc,
                Specification => New_Spec);
            Insert_Before (N, Decl);
            Analyze (Decl);
            Spec_Id := Defining_Unit_Name (New_Spec);
            Set_Has_Completion (Spec_Id);
            Set_Convention (Spec_Id, Convention_Protected);
         end;

      elsif Present (Spec_Id) then
         Spec_Decl := Unit_Declaration_Node (Spec_Id);
      end if;

      --  Place subprogram on scope stack, and make formals visible. If there
      --  is a spec, the visible entity remains that of the spec.

      if Present (Spec_Id) then
         Generate_Reference (Spec_Id, Body_Id, 'b', Set_Ref => False);
         Style.Check_Identifier (Body_Id, Spec_Id);

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
              and then Nkind (Original_Node (Spec_Decl)) =
                N_Subprogram_Renaming_Declaration
              and then not Comes_From_Source (N)
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

         --  Generate references from body formals to spec formals
         --  and also set the Spec_Entity fields for all formals. We
         --  do not set this reference count as a reference for the
         --  purposes of identifying unreferenced formals however.

         if Spec_Id /= Body_Id then
            declare
               Fs : Entity_Id;
               Fb : Entity_Id;

            begin
               Fs := First_Formal (Spec_Id);
               Fb := First_Formal (Body_Id);
               while Present (Fs) loop
                  Generate_Reference (Fs, Fb, 'b');
                  Style.Check_Identifier (Fb, Fs);
                  Set_Spec_Entity (Fb, Fs);
                  Set_Referenced (Fs, False);
                  Next_Formal (Fs);
                  Next_Formal (Fb);
               end loop;
            end;
         end if;

         if Nkind (N) /= N_Subprogram_Body_Stub then
            Set_Corresponding_Spec (N, Spec_Id);
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
      then
         declare
            Conformant : Boolean := False;
            Old_Id     : Entity_Id :=
                           Defining_Entity
                             (Specification (Corresponding_Stub (Parent (N))));

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
        and then (Is_Always_Inlined (Spec_Id)
                    or else (Has_Pragma_Inline (Spec_Id)
                              and then
                                (Front_End_Inlining or else No_Run_Time)))
      then
         if Build_Body_To_Inline (N, Spec_Id, Copy_Separate_Tree (N)) then
            null;
         end if;
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

      --  If we have a separate spec, then the analysis of the declarations
      --  caused the entities in the body to be chained to the spec id, but
      --  we want them chained to the body id. Only the formal parameters
      --  end up chained to the spec id in this case.

      if Present (Spec_Id) then

         --  If a parent unit is categorized, the context of a subunit
         --  must conform to the categorization. Conversely, if a child
         --  unit is categorized, the parents themselves must conform.

         if Nkind (Parent (N)) = N_Subunit then
            Validate_Categorization_Dependency (N, Spec_Id);

         elsif Is_Child_Unit (Spec_Id) then
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
         Check_Returns (HSS, 'P', Missing_Ret);
      end if;

      --  Don't worry about checking for variables that are never modified
      --  if the first statement of the body is a raise statement, since
      --  we assume this is some kind of stub. We ignore a label generated
      --  by the exception stuff for the purpose of this test.

      declare
         Stm : Node_Id := First (Statements (HSS));

      begin
         if Nkind (Stm) = N_Label then
            Next (Stm);
         end if;

         if Nkind (Original_Node (Stm)) = N_Raise_Statement then
            return;
         end if;
      end;

      --  Check for variables that are never modified

      declare
         E1, E2 : Entity_Id;

      begin
         --  If there is a separate spec, then transfer Not_Source_Assigned
         --  flags from out parameters to the corresponding entities in the
         --  body. The reason we do that is we want to post error flags on
         --  the body entities, not the spec entities.

         if Present (Spec_Id) then
            E1 := First_Entity (Spec_Id);

            while Present (E1) loop
               if Ekind (E1) = E_Out_Parameter then
                  E2 := First_Entity (Body_Id);

                  loop
                     --  If no matching body entity, then we already had
                     --  a detected error of some kind, so just forget
                     --  about worrying about these warnings.

                     if No (E2) then
                        return;
                     end if;

                     exit when Chars (E1) = Chars (E2);
                     Next_Entity (E2);
                  end loop;

                  Set_Not_Source_Assigned (E2, Not_Source_Assigned (E1));
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
      Designator : constant Entity_Id := Analyze_Spec (Specification (N));
      Scop        : constant Entity_Id := Current_Scope;

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
      Set_Suppress_Elaboration_Checks
        (Designator, Elaboration_Checks_Suppressed (Designator));

      if Scop /= Standard_Standard
        and then not Is_Child_Unit (Designator)
      then
         Set_Is_Pure (Designator,
           Is_Pure (Scop) and then Is_Library_Level_Entity (Designator));
         Set_Is_Remote_Call_Interface (
           Designator, Is_Remote_Call_Interface (Scop));
         Set_Is_Remote_Types (Designator, Is_Remote_Types (Scop));

      else
         --  For a compilation unit, check for library-unit pragmas.

         New_Scope (Designator);
         Set_Categorization_From_Pragmas (N);
         Validate_Categorization_Dependency (N, Designator);
         Pop_Scope;
      end if;

      --  For a compilation unit, set body required. This flag will only be
      --  reset if a valid Import or Interface pragma is processed later on.

      if Nkind (Parent (N)) = N_Compilation_Unit then
         Set_Body_Required (Parent (N), True);
      end if;

      Check_Eliminated (Designator);
   end Analyze_Subprogram_Declaration;

   --------------------------
   -- Build_Body_To_Inline --
   --------------------------

   function Build_Body_To_Inline
     (N         : Node_Id;
      Subp      : Entity_Id;
      Orig_Body : Node_Id) return Boolean
   is
      Decl : constant Node_Id := Unit_Declaration_Node (Subp);
      Original_Body   : Node_Id;
      Body_To_Analyze : Node_Id;
      Max_Size        : constant := 10;
      Stat_Count      : Integer := 0;

      function Has_Excluded_Declaration (Decls : List_Id) return Boolean;
      --  Check for declarations that make inlining not worthwhile.

      function Has_Excluded_Statement   (Stats : List_Id) return Boolean;
      --  Check for statements that make inlining not worthwhile: any
      --  tasking statement, nested at any level. Keep track of total
      --  number of elementary statements, as a measure of acceptable size.

      function Has_Pending_Instantiation return Boolean;
      --  If some enclosing body contains instantiations that appear before
      --  the corresponding generic body, the enclosing body has a freeze node
      --  so that it can be elaborated after the generic itself. This might
      --  conflict with subsequent inlinings, so that it is unsafe to try to
      --  inline in such a case.

      -------------------
      -- Cannot_Inline --
      -------------------

      procedure Cannot_Inline (Msg : String; N : Node_Id);
      --  If subprogram has pragma Inline_Always, it is an error if
      --  it cannot be inlined. Otherwise, emit a warning.

      procedure Cannot_Inline (Msg : String; N : Node_Id) is
      begin
         if Is_Always_Inlined (Subp) then
            Error_Msg_NE (Msg (1 .. Msg'Length - 1), N, Subp);

         elsif Ineffective_Inline_Warnings then
            Error_Msg_NE (Msg, N, Subp);
         end if;
      end Cannot_Inline;

      ------------------------------
      -- Has_Excluded_Declaration --
      ------------------------------

      function Has_Excluded_Declaration (Decls : List_Id) return Boolean is
         D : Node_Id;

      begin
         D := First (Decls);

         while Present (D) loop
            if        Nkind (D) = N_Function_Instantiation
              or else Nkind (D) = N_Protected_Type_Declaration
              or else Nkind (D) = N_Package_Declaration
              or else Nkind (D) = N_Package_Instantiation
              or else Nkind (D) = N_Subprogram_Body
              or else Nkind (D) = N_Procedure_Instantiation
              or else Nkind (D) = N_Task_Type_Declaration
            then
               Cannot_Inline
                 ("\declaration prevents front-end inlining of&?", D);
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
                 ("\statement prevents front-end inlining of&?", S);
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

   --  Start of processing for Build_Body_To_Inline

   begin
      if Nkind (Decl) = N_Subprogram_Declaration
        and then Present (Body_To_Inline (Decl))
      then
         return True;    --  Done already.

      --  Functions that return unconstrained composite types will require
      --  secondary stack handling, and cannot currently be inlined.

      elsif Ekind (Subp) = E_Function
        and then not Is_Scalar_Type (Etype (Subp))
        and then not Is_Access_Type (Etype (Subp))
        and then not Is_Constrained (Etype (Subp))
      then
         Cannot_Inline
           ("unconstrained return type prevents front-end inlining of&?", N);
         return False;
      end if;

      --  We need to capture references to the formals in order to substitute
      --  the actuals at the point of inlining, i.e. instantiation. To treat
      --  the formals as globals to the body to inline, we nest it within
      --  a dummy parameterless subprogram, declared within the real one.

      Original_Body := Orig_Body;

      --  Within an instance, the current tree is already the result of
      --  a generic copy, and not what we need for subsequent inlining.
      --  We create the required body by doing an instantiating copy, to
      --  obtain the proper partially analyzed tree.

      if In_Instance then
         if No (Generic_Parent (Specification (N))) then
            return False;

         elsif Is_Child_Unit (Scope (Current_Scope)) then
            return False;

         elsif Scope (Current_Scope) = Cunit_Entity (Main_Unit) then

            --  compiling an instantiation. There is no point in generating
            --  bodies to inline, because they will not be used.

            return False;

         else
            Body_To_Analyze :=
              Copy_Generic_Node
                (Generic_Parent (Specification (N)), Empty,
                   Instantiating => True);
         end if;
      else
         Body_To_Analyze :=
           Copy_Generic_Node (Original_Body, Empty,
             Instantiating => False);
      end if;

      Set_Parameter_Specifications (Specification (Original_Body), No_List);
      Set_Defining_Unit_Name (Specification (Original_Body),
        Make_Defining_Identifier (Sloc (N), New_Internal_Name ('S')));
      Set_Corresponding_Spec (Original_Body, Empty);

      if Ekind (Subp) = E_Function then
         Set_Subtype_Mark (Specification (Original_Body),
           New_Occurrence_Of (Etype (Subp), Sloc (N)));
      end if;

      if Present (Declarations (Orig_Body))
        and then Has_Excluded_Declaration (Declarations (Orig_Body))
      then
         return False;
      end if;

      if Present (Handled_Statement_Sequence (N)) then
         if
          (Present (Exception_Handlers (Handled_Statement_Sequence (N))))
         then
            Cannot_Inline ("handler prevents front-end inlining of&?",
               First (Exception_Handlers (Handled_Statement_Sequence (N))));
            return False;
         elsif
           Has_Excluded_Statement
             (Statements (Handled_Statement_Sequence (N)))
         then
            return False;
         end if;
      end if;

      --  We do not inline a subprogram  that is too large, unless it is
      --  marked Inline_Always. This pragma does not suppress the other
      --  checks on inlining (forbidden declarations, handlers, etc).

      if Stat_Count > Max_Size
        and then not Is_Always_Inlined (Subp)
      then
         Cannot_Inline ("body is too large for front-end inlining of&?", N);
         return False;
      end if;

      if Has_Pending_Instantiation then
         Cannot_Inline
           ("cannot inline& because of forward instance within enclosing body",
             N);
         return False;
      end if;

      Body_To_Analyze := Copy_Generic_Node (Original_Body, Empty, False);

      --  Set return type of function, which is also global and does not need
      --  to be resolved.

      if Ekind (Subp) = E_Function then
         Set_Subtype_Mark (Specification (Body_To_Analyze),
           New_Occurrence_Of (Etype (Subp), Sloc (N)));
      end if;

      if No (Declarations (N)) then
         Set_Declarations (N, New_List (Body_To_Analyze));
      else
         Append (Body_To_Analyze, Declarations (N));
      end if;

      Expander_Mode_Save_And_Set (False);

      Analyze (Body_To_Analyze);
      New_Scope (Defining_Entity (Body_To_Analyze));
      Save_Global_References (Original_Body);
      End_Scope;
      Remove (Body_To_Analyze);

      Expander_Mode_Restore;
      Set_Body_To_Inline (Decl, Original_Body);
      Set_Is_Inlined (Subp);
      return True;

   end Build_Body_To_Inline;

   -----------------------
   -- Check_Conformance --
   -----------------------

   procedure Check_Conformance
     (New_Id   : Entity_Id;
      Old_Id   : Entity_Id;
      Ctype    : Conformance_Type;
      Errmsg   : Boolean;
      Conforms : out Boolean;
      Err_Loc  : Node_Id := Empty;
      Get_Inst : Boolean := False)
   is
      Old_Type   : constant Entity_Id := Etype (Old_Id);
      New_Type   : constant Entity_Id := Etype (New_Id);
      Old_Formal : Entity_Id;
      New_Formal : Entity_Id;

      procedure Conformance_Error (Msg : String; N : Node_Id := New_Id);
      --  Post error message for conformance error on given node.
      --  Two messages are output. The first points to the previous
      --  declaration with a general "no conformance" message.
      --  The second is the detailed reason, supplied as Msg. The
      --  parameter N provide information for a possible & insertion
      --  in the message, and also provides the location for posting
      --  the message in the absence of a specified Err_Loc location.

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

      --  We need a special case for operators, since they don't
      --  appear explicitly.

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

            --  Names must match

            if Chars (Old_Formal) /= Chars (New_Formal) then
               Conformance_Error ("name & does not match!", New_Formal);
               return;

            --  And default expressions for in parameters

            elsif Parameter_Mode (Old_Formal) = E_In_Parameter then
               declare
                  NewD : constant Boolean :=
                           Present (Default_Value (New_Formal));
                  OldD : constant Boolean :=
                           Present (Default_Value (Old_Formal));
               begin
                  if NewD or OldD then

                     --  The old default value has been analyzed and expanded,
                     --  because the current full declaration will have frozen
                     --  everything before. The new default values have not
                     --  been expanded, so expand now to check conformance.

                     if NewD then
                        New_Scope (New_Id);
                        Analyze_Default_Expression
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

         if Ada_83
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
      --  Post error message for conformance error on given node.
      --  Two messages are output. The first points to the previous
      --  declaration with a general "no conformance" message.
      --  The second is the detailed reason, supplied as Msg. The
      --  parameter N provide information for a possible & insertion
      --  in the message.

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

         --  The subtype mark of the discriminant on the full type
         --  has not been analyzed so we do it here. For an access
         --  discriminant a new type is created.

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
               --  everything before. The new default values have not
               --  been expanded, so expand now to check conformance.

               if NewD then
                  Analyze_Default_Expression
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

         if Ada_83 then
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

   -------------------
   -- Check_Returns --
   -------------------

   procedure Check_Returns
     (HSS  : Node_Id;
      Mode : Character;
      Err  : out Boolean)
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
                 ("?RETURN statement missing following this statement!",
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

         else
            Error_Msg_N
              ("implied return after this statement not allowed (No_Return)",
               Last_Stm);
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

      if Style_Check_Subprogram_Order
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
      Get_Inst : Boolean := False)
      return     Boolean
   is
      Type_1 : Entity_Id := T1;
      Type_2 : Entity_Id := T2;

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

         else
            return False;
         end if;
      end Base_Types_Match;

   begin
      --  The context is an instance association for a formal
      --  access-to-subprogram type; the formal parameter types
      --  require mapping because they may denote other formal
      --  parameters of the generic unit.

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
      end if;

      --  Test anonymous access type case. For this case, static subtype
      --  matching is required for mode conformance (RM 6.3.1(15))

      if Ekind (Type_1) = E_Anonymous_Access_Type
        and then Ekind (Type_2) = E_Anonymous_Access_Type
      then
         declare
            Desig_1 : Entity_Id;
            Desig_2 : Entity_Id;

         begin
            Desig_1 := Directly_Designated_Type (Type_1);

            --  An access parameter can designate an incomplete type.

            if Ekind (Desig_1) = E_Incomplete_Type
              and then Present (Full_View (Desig_1))
            then
               Desig_1 := Full_View (Desig_1);
            end if;

            Desig_2 := Directly_Designated_Type (Type_2);

            if Ekind (Desig_2) = E_Incomplete_Type
              and then Present (Full_View (Desig_2))
            then
               Desig_2 := Full_View (Desig_2);
            end if;

            --  The context is an instance association for a formal
            --  access-to-subprogram type; formal access parameter
            --  designated types require mapping because they may
            --  denote other formal parameters of the generic unit.

            if Get_Inst then
               Desig_1 := Get_Instance_Of (Desig_1);
               Desig_2 := Get_Instance_Of (Desig_2);
            end if;

            --  It is possible for a Class_Wide_Type to be introduced for
            --  an incomplete type, in which case there is a separate class_
            --  wide type for the full view. The types conform if their
            --  Etypes conform, i.e. one may be the full view of the other.
            --  This can only happen in the context of an access parameter,
            --  other uses of an incomplete Class_Wide_Type are illegal.

            if Ekind (Desig_1) = E_Class_Wide_Type
              and then Ekind (Desig_2) = E_Class_Wide_Type
            then
               return
                 Conforming_Types (Etype (Desig_1), Etype (Desig_2), Ctype);
            else
               return Base_Type (Desig_1) = Base_Type (Desig_2)
                and then (Ctype = Type_Conformant
                          or else
                        Subtypes_Statically_Match (Desig_1, Desig_2));
            end if;
         end;

      --  Otherwise definitely no match

      else
         return False;
      end if;

   end Conforming_Types;

   --------------------------
   -- Create_Extra_Formals --
   --------------------------

   procedure Create_Extra_Formals (E : Entity_Id) is
      Formal      : Entity_Id;
      Last_Formal : Entity_Id;
      Last_Extra  : Entity_Id;
      Formal_Type : Entity_Id;
      P_Formal    : Entity_Id := Empty;

      function Add_Extra_Formal (Typ : Entity_Id) return Entity_Id;
      --  Add an extra formal, associated with the current Formal. The
      --  extra formal is added to the list of extra formals, and also
      --  returned as the result. These formals are always of mode IN.

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

         --  A little optimization. Never generate an extra formal for
         --  the _init operand of an initialization procedure, since it
         --  could never be used.

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
      --  If this is a derived subprogram then the subtypes of the
      --  parent subprogram's formal parameters will be used to
      --  to determine the need for extra formals.

      if Is_Overloadable (E) and then Present (Alias (E)) then
         P_Formal := First_Formal (Alias (E));
      end if;

      Last_Extra := Empty;
      Formal := First_Formal (E);
      while Present (Formal) loop
         Last_Extra := Formal;
         Next_Formal (Formal);
      end loop;

      --  If Extra_formals where already created, don't do it again
      --  This situation may arise for subprogram types created as part
      --  of dispatching calls (see Expand_Dispatch_Call)

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
         --  checks for either the subprogram, or the package in which it
         --  resides. However, we do not suppress it simply if the scope
         --  has accessibility checks suppressed, since this could cause
         --  trouble when clients are compiled with a different suppression
         --  setting. The explicit checks are safe from this point of view.

         if Ekind (Etype (Formal)) = E_Anonymous_Access_Type
           and then not
             (Suppress_Accessibility_Checks (E)
               or else
              Suppress_Accessibility_Checks (Scope (E)))
           and then
             (not Present (P_Formal)
               or else Present (Extra_Accessibility (P_Formal)))
         then
            --  Temporary kludge: for now we avoid creating the extra
            --  formal for access parameters of protected operations
            --  because of problem with the case of internal protected
            --  calls. ???

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

         Last_Formal := Formal;
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
            if (Current_Scope = Standard_Standard
                  or else (Ekind (E) = Ekind (Designator)
                and then
                  Type_Conformant (E, Designator)))
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
      Given_E2 : Node_Id)
      return     Boolean
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
      --  Compare an operator node with a function call.

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

      --  If same entities are referenced, then they are conformant
      --  even if they have different forms (RM 8.3.1(19-20)).

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
      New_E  : Entity_Id)
      return Boolean
   is
      Formal : Entity_Id;
      F_Typ  : Entity_Id;
      G_Typ  : Entity_Id := Empty;

      function Get_Generic_Parent_Type (F_Typ : Entity_Id) return Entity_Id;
      --  If F_Type is a derived type associated with a generic actual
      --  subtype, then return its Generic_Parent_Type attribute, else
      --  return Empty.

      function Types_Correspond
        (P_Type : Entity_Id;
         N_Type : Entity_Id)
         return   Boolean;
      --  Returns true if and only if the types (or designated types
      --  in the case of anonymous access types) are the same or N_Type
      --  is derived directly or indirectly from P_Type.

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
            --  The tree must be traversed to determine the parent
            --  subtype in the generic unit, which unfortunately isn't
            --  always available via semantic attributes. ???
            --  (Note: The use of Original_Node is needed for cases
            --  where a full derived type has been rewritten.)

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
         N_Type : Entity_Id)
         return   Boolean
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
      --  In the case where both operations are implicit derived
      --  subprograms then neither overrides the other. This can
      --  only occur in certain obscure cases (e.g., derivation
      --  from homographs created in a generic instantiation).

      if Present (Alias (Prev_E)) and then Present (Alias (New_E)) then
         return True;

      elsif Ekind (Current_Scope) = E_Package
        and then Is_Generic_Instance (Current_Scope)
        and then In_Private_Part (Current_Scope)
        and then Comes_From_Source (New_E)
      then
         --  We examine the formals and result subtype of the inherited
         --  operation, to determine whether their type is derived from
         --  (the instance of) a generic type.

         Formal := First_Formal (Prev_E);

         while Present (Formal) loop
            F_Typ := Base_Type (Etype (Formal));

            if Ekind (F_Typ) = E_Anonymous_Access_Type then
               F_Typ := Designated_Type (F_Typ);
            end if;

            G_Typ := Get_Generic_Parent_Type (F_Typ);

            Next_Formal (Formal);
         end loop;

         if not Present (G_Typ) and then Ekind (Prev_E) = E_Function then
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

                     --  Found a matching primitive operation belonging to
                     --  the formal ancestor type, so the new subprogram
                     --  is overriding.

                     if not Present (P_Formal)
                       and then not Present (N_Formal)
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

               --  If no match found, then the new subprogram does
               --  not override in the generic (nor in the instance).

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

      A : Entity_Id;
      B : Entity_Id;

   begin
      --  Check that equality was properly defined.

      if  No (Next_Formal (First_Formal (S))) then
         return;
      end if;

      A := Make_Defining_Identifier (Loc, Chars (First_Formal (S)));
      B := Make_Defining_Identifier (Loc,
             Chars (Next_Formal (First_Formal (S))));

      Op_Name := Make_Defining_Operator_Symbol (Loc, Name_Op_Ne);

      Formals := New_List (
        Make_Parameter_Specification (Loc,
          Defining_Identifier => A,
          Parameter_Type =>
            New_Reference_To (Etype (First_Formal (S)), Loc)),

        Make_Parameter_Specification (Loc,
          Defining_Identifier => B,
          Parameter_Type =>
            New_Reference_To (Etype (Next_Formal (First_Formal (S))), Loc)));

      Decl :=
        Make_Subprogram_Declaration (Loc,
          Specification =>
            Make_Function_Specification (Loc,
              Defining_Unit_Name => Op_Name,
              Parameter_Specifications => Formals,
              Subtype_Mark => New_Reference_To (Standard_Boolean, Loc)));

      --  Insert inequality right after equality if it is explicit or after
      --  the derived type when implicit. These entities are created only
      --  for visibility purposes, and eventually replaced in the course of
      --  expansion, so they do not need to be attached to the tree and seen
      --  by the back-end. Keeping them internal also avoids spurious freezing
      --  problems. The parent field is set simply to make analysis safe.

      if No (Alias (S)) then
         Set_Parent (Decl, Parent (Unit_Declaration_Node (S)));
      else
         Set_Parent (Decl, Parent (Parent (Etype (First_Formal (S)))));
      end if;

      Mark_Rewrite_Insertion (Decl);
      Set_Is_Intrinsic_Subprogram (Op_Name);
      Analyze (Decl);
      Set_Has_Completion (Op_Name);
      Set_Corresponding_Equality (Op_Name, S);
      Set_Is_Abstract (Op_Name, Is_Abstract (S));

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
      E        : Entity_Id := Current_Entity_In_Scope (S);
      Prev_Vis : Entity_Id := Empty;

      function Is_Private_Declaration (E : Entity_Id) return Boolean;
      --  Check that E is declared in the private part of the current package,
      --  or in the package body, where it may hide a previous declaration.
      --  We can' use In_Private_Part by itself because this flag is also
      --  set when freezing entities, so we must examine the place of the
      --  declaration in the tree, and recognize wrapper packages as well.

      procedure Maybe_Primitive_Operation (Overriding : Boolean := False);
      --  If the subprogram being analyzed is a primitive operation of
      --  the type of one of its formals, set the corresponding flag.

      ----------------------------
      -- Is_Private_Declaration --
      ----------------------------

      function Is_Private_Declaration (E : Entity_Id) return Boolean is
         Priv_Decls : List_Id;
         Decl       : constant Node_Id := Unit_Declaration_Node (E);

      begin
         if Is_Package (Current_Scope)
           and then In_Private_Part (Current_Scope)
         then
            Priv_Decls :=
              Private_Declarations (
                Specification (Unit_Declaration_Node (Current_Scope)));

            return In_Package_Body (Current_Scope)
              or else List_Containing (Decl) = Priv_Decls
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

      procedure Maybe_Primitive_Operation (Overriding : Boolean := False) is
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
                 and then (not Overriding or else not Is_Abstract (E))
               then
                  Error_Msg_N ("abstract subprograms must be visible "
                                & "('R'M 3.9.3(10))!", S);

               elsif Ekind (S) = E_Function
                 and then Is_Tagged_Type (T)
                 and then T = Base_Type (Etype (S))
                 and then not Overriding
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

         elsif (Ekind (Current_Scope) = E_Package
                 and then not In_Package_Body (Current_Scope))
           or else Overriding
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
      if No (E) then
         Enter_Overloaded_Entity (S);
         Check_Dispatching_Operation (S, Empty);
         Maybe_Primitive_Operation;

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

         --  If the subprogram is implicit it is hidden by the previous
         --  declaration. However if it is dispatching, it must appear in
         --  the dispatch table anyway, because it can be dispatched to
         --  even if it cannot be called directly.

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

            --  Useful additional warning.

            if Is_Generic_Unit (E) then
               Error_Msg_N ("\previous generic unit cannot be overloaded", S);
            end if;

            return;
         end if;

      else
         --  E exists and is overloadable. Determine whether S is the body
         --  of E, a new overloaded entity with a different signature, or
         --  an error altogether.

         while Present (E) loop
            if Scope (E) /= Current_Scope then
               null;

            elsif Type_Conformant (E, S) then

               --  If the old and new entities have the same profile and
               --  one is not the body of the other, then this is an error,
               --  unless one of them is implicitly declared.

               --  There are some cases when both can be implicit, for example
               --  when both a literal and a function that overrides it are
               --  inherited in a derivation, or when an inhertited operation
               --  of a tagged full type overrides the ineherited operation of
               --  a private extension. Ada 83 had a special rule for the
               --  the literal case. In Ada95, the later implicit operation
               --  hides the former, and the literal is always the former.
               --  In the odd case where both are derived operations declared
               --  at the same point, both operations should be declared,
               --  and in that case we bypass the following test and proceed
               --  to the next part (this can only occur for certain obscure
               --  cases involving homographs in instances and can't occur for
               --  dispatching operations ???). Note that the following
               --  condition is less than clear. For example, it's not at
               --  all clear why there's a test for E_Entry here. ???

               if Present (Alias (S))
                 and then (No (Alias (E))
                            or else Comes_From_Source (E)
                            or else Is_Dispatching_Operation (E))
                 and then
                   (Ekind (E) = E_Entry
                     or else Ekind (E) /= E_Enumeration_Literal)
               then
                  --  When an derived operation is overloaded it may be due
                  --  to the fact that the full view of a private extension
                  --  re-inherits. It has to be dealt with.

                  if Is_Package (Current_Scope)
                    and then In_Private_Part (Current_Scope)
                  then
                     Check_Operation_From_Private_View (S, E);
                  end if;

                  --  In any case the implicit operation remains hidden by
                  --  the existing declaration.

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
                     if not Present (Derived_Type)
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
                        --  part of a package, we retain the derived subprogram
                        --  but mark it as not immediately visible. If the
                        --  derived operation was declared in the visible part
                        --  then this ensures that it will still be visible
                        --  outside the package with the proper signature
                        --  (calls from outside must also be directed to this
                        --  version rather than the overriding one, unlike the
                        --  dispatching case). Calls from inside the package
                        --  will still resolve to the overriding subprogram
                        --  since the derived one is marked as not visible
                        --  within the package.

                        --  If the private operation is dispatching, we achieve
                        --  the overriding by keeping the implicit operation
                        --  but setting its alias to be the overring one. In
                        --  this fashion the proper body is executed in all
                        --  cases, but the original signature is used outside
                        --  of the package.

                        --  If the overriding is not in the private part, we
                        --  remove the implicit operation altogether.

                        if Is_Private_Declaration (S) then

                           if not Is_Dispatching_Operation (E) then
                              Set_Is_Immediately_Visible (E, False);
                           else

                              --  work done in Override_Dispatching_Operation.

                              null;
                           end if;
                        else

                           --  Find predecessor of E in Homonym chain.

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

                     if Is_Dispatching_Operation (E) then
                        --  An overriding dispatching subprogram inherits
                        --  the convention of the overridden subprogram
                        --  (by AI-117).

                        Set_Convention (S, Convention (E));

                        Check_Dispatching_Operation (S, E);
                     else
                        Check_Dispatching_Operation (S, Empty);
                     end if;

                     Maybe_Primitive_Operation (Overriding => True);
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
               null;
            end if;

            Prev_Vis := E;
            E := Homonym (E);
         end loop;

         --  On exit, we know that S is a new entity

         Enter_Overloaded_Entity (S);
         Maybe_Primitive_Operation;

         --  If S is a derived operation for an untagged type then
         --  by definition it's not a dispatching operation (even
         --  if the parent operation was dispatching), so we don't
         --  call Check_Dispatching_Operation in that case.

         if not Present (Derived_Type)
           or else Is_Tagged_Type (Derived_Type)
         then
            Check_Dispatching_Operation (S, Empty);
         end if;
      end if;

      --  If this is a  user-defined equality operator that is not
      --  a derived subprogram, create the corresponding inequality.
      --  If the operation is dispatching, the expansion is done
      --  elsewhere,  and we do not create an explicit inequality
      --  operation.

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
      --  Check whether the default has a class-wide type. After analysis
      --  the default has the type of the formal, so we must also check
      --  explicitly for an access attribute.

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
               if Nkind (Parent (T)) /= N_Access_Function_Definition
                 and then Nkind (Parent (T)) /= N_Access_Procedure_Definition
               then
                  Error_Msg_N ("invalid use of incomplete type", Param_Spec);
               end if;

            elsif Ekind (Formal_Type) = E_Void then
               Error_Msg_NE ("premature use of&",
                 Parameter_Type (Param_Spec), Formal_Type);
            end if;

         --  An access formal type

         else
            Formal_Type :=
              Access_Definition (Related_Nod, Parameter_Type (Param_Spec));
         end if;

         Set_Etype (Formal, Formal_Type);

         Default :=  Expression (Param_Spec);

         if Present (Default) then
            if Out_Present (Param_Spec) then
               Error_Msg_N
                 ("default initialization only allowed for IN parameters",
                  Param_Spec);
            end if;

            --  Do the special preanalysis of the expression (see section on
            --  "Handling of Default Expressions" in the spec of package Sem).

            Analyze_Default_Expression (Default, Formal_Type);

            --  Check that the designated type of an access parameter's
            --  default is not a class-wide type unless the parameter's
            --  designated type is also class-wide.

            if Ekind (Formal_Type) = E_Anonymous_Access_Type
              and then Is_Class_Wide_Default (Default)
              and then not Is_Class_Wide_Type (Designated_Type (Formal_Type))
            then
               Error_Msg_N
                 ("access to class-wide expression not allowed here", Default);
            end if;
         end if;

      <<Continue>>
         Next (Param_Spec);
      end loop;

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

   -------------------------
   -- Set_Actual_Subtypes --
   -------------------------

   procedure Set_Actual_Subtypes (N : Node_Id; Subp : Entity_Id) is
      Loc        : constant Source_Ptr := Sloc (N);
      Decl       : Node_Id;
      Formal     : Entity_Id;
      T          : Entity_Id;
      First_Stmt : Node_Id := Empty;
      AS_Needed  : Boolean;

   begin
      Formal := First_Formal (Subp);
      while Present (Formal) loop
         T := Etype (Formal);

         --  We never need an actual subtype for a constrained formal.

         if Is_Constrained (T) then
            AS_Needed := False;

         --  If we have unknown discriminants, then we do not need an
         --  actual subtype, or more accurately we cannot figure it out!
         --  Note that all class-wide types have unknown discriminants.

         elsif Has_Unknown_Discriminants (T) then
            AS_Needed := False;

         --  At this stage we have an unconstrained type that may need
         --  an actual subtype. For sure the actual subtype is needed
         --  if we have an unconstrained array type.

         elsif Is_Array_Type (T) then
            AS_Needed := True;

         --  The only other case which needs an actual subtype is an
         --  unconstrained record type which is an IN parameter (we
         --  cannot generate actual subtypes for the OUT or IN OUT case,
         --  since an assignment can change the discriminant values.
         --  However we exclude the case of initialization procedures,
         --  since discriminants are handled very specially in this context,
         --  see the section entitled "Handling of Discriminants" in Einfo.
         --  We also exclude the case of Discrim_SO_Functions (functions
         --  used in front end layout mode for size/offset values), since
         --  in such functions only discriminants are referenced, and not
         --  only are such subtypes not needed, but they cannot always
         --  be generated, because of order of elaboration issues.

         elsif Is_Record_Type (T)
           and then Ekind (Formal) = E_In_Parameter
           and then Chars (Formal) /= Name_uInit
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
            Decl := Build_Actual_Subtype (T, Formal);

            if Nkind (N) = N_Accept_Statement then
               if Present (Handled_Statement_Sequence (N)) then
                  First_Stmt :=
                    First (Statements (Handled_Statement_Sequence (N)));
                  Prepend (Decl, Statements (Handled_Statement_Sequence (N)));
                  Mark_Rewrite_Insertion (Decl);
               else
                  --  If the accept statement has no body, there will be
                  --  no reference to the actuals, so no need to compute
                  --  actual subtypes.

                  return;
               end if;

            else
               Prepend (Decl, Declarations (N));
               Mark_Rewrite_Insertion (Decl);
            end if;

            Analyze (Decl);

            --  We need to freeze manually the generated type when it is
            --  inserted anywhere else than in a declarative part.

            if Present (First_Stmt) then
               Insert_List_Before_And_Analyze (First_Stmt,
                 Freeze_Entity (Defining_Identifier (Decl), Loc));
            end if;

            Set_Actual_Subtype (Formal, Defining_Identifier (Decl));
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
            Set_Ekind (Formal_Id, E_Out_Parameter);
            Set_Not_Source_Assigned (Formal_Id);
         end if;

      else
         Set_Ekind (Formal_Id, E_In_Parameter);
      end if;

      Set_Mechanism (Formal_Id, Default_Mechanism);
      Set_Formal_Validity (Formal_Id);
   end Set_Formal_Mode;

   -------------------------
   -- Set_Formal_Validity --
   -------------------------

   procedure Set_Formal_Validity (Formal_Id : Entity_Id) is
   begin
      --  If in full validity checking mode, then we can assume that
      --  an IN or IN OUT parameter is valid (see Exp_Ch5.Expand_Call)

      if not Validity_Checks_On then
         return;

      elsif Ekind (Formal_Id) = E_In_Parameter
        and then Validity_Check_In_Params
      then
         Set_Is_Known_Valid (Formal_Id, True);

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

   function Type_Conformant (New_Id, Old_Id : Entity_Id) return Boolean is
      Result : Boolean;

   begin
      Check_Conformance (New_Id, Old_Id, Type_Conformant, False, Result);
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
