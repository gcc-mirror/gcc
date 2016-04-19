------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ E L A B                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1997-2015, Free Software Foundation, Inc.         --
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
with Checks;   use Checks;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Exp_Tss;  use Exp_Tss;
with Exp_Util; use Exp_Util;
with Expander; use Expander;
with Fname;    use Fname;
with Lib;      use Lib;
with Lib.Load; use Lib.Load;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Output;   use Output;
with Restrict; use Restrict;
with Rident;   use Rident;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Cat;  use Sem_Cat;
with Sem_Ch7;  use Sem_Ch7;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Stand;    use Stand;
with Table;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;
with Uname;    use Uname;

package body Sem_Elab is

   --  The following table records the recursive call chain for output in the
   --  Output routine. Each entry records the call node and the entity of the
   --  called routine. The number of entries in the table (i.e. the value of
   --  Elab_Call.Last) indicates the current depth of recursion and is used to
   --  identify the outer level.

   type Elab_Call_Entry is record
      Cloc : Source_Ptr;
      Ent  : Entity_Id;
   end record;

   package Elab_Call is new Table.Table (
     Table_Component_Type => Elab_Call_Entry,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 1,
     Table_Initial        => 50,
     Table_Increment      => 100,
     Table_Name           => "Elab_Call");

   --  This table is initialized at the start of each outer level call. It
   --  holds the entities for all subprograms that have been examined for this
   --  particular outer level call, and is used to prevent both infinite
   --  recursion, and useless reanalysis of bodies already seen

   package Elab_Visited is new Table.Table (
     Table_Component_Type => Entity_Id,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 1,
     Table_Initial        => 200,
     Table_Increment      => 100,
     Table_Name           => "Elab_Visited");

   --  This table stores calls to Check_Internal_Call that are delayed
   --  until all generics are instantiated, and in particular that all
   --  generic bodies have been inserted. We need to delay, because we
   --  need to be able to look through the inserted bodies.

   type Delay_Element is record
      N : Node_Id;
      --  The parameter N from the call to Check_Internal_Call. Note that
      --  this node may get rewritten over the delay period by expansion
      --  in the call case (but not in the instantiation case).

      E : Entity_Id;
      --  The parameter E from the call to Check_Internal_Call

      Orig_Ent : Entity_Id;
      --  The parameter Orig_Ent from the call to Check_Internal_Call

      Curscop : Entity_Id;
      --  The current scope of the call. This is restored when we complete
      --  the delayed call, so that we do this in the right scope.

      From_Elab_Code : Boolean;
      --  Save indication of whether this call is from elaboration code

      Outer_Scope : Entity_Id;
      --  Save scope of outer level call
   end record;

   package Delay_Check is new Table.Table (
     Table_Component_Type => Delay_Element,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 1,
     Table_Initial        => 1000,
     Table_Increment      => 100,
     Table_Name           => "Delay_Check");

   C_Scope : Entity_Id;
   --  Top level scope of current scope. Compute this only once at the outer
   --  level, i.e. for a call to Check_Elab_Call from outside this unit.

   Outer_Level_Sloc : Source_Ptr;
   --  Save Sloc value for outer level call node for comparisons of source
   --  locations. A body is too late if it appears after the *outer* level
   --  call, not the particular call that is being analyzed.

   From_Elab_Code : Boolean;
   --  This flag shows whether the outer level call currently being examined
   --  is or is not in elaboration code. We are only interested in calls to
   --  routines in other units if this flag is True.

   In_Task_Activation : Boolean := False;
   --  This flag indicates whether we are performing elaboration checks on
   --  task procedures, at the point of activation. If true, we do not trace
   --  internal calls in these procedures, because all local bodies are known
   --  to be elaborated.

   Delaying_Elab_Checks : Boolean := True;
   --  This is set True till the compilation is complete, including the
   --  insertion of all instance bodies. Then when Check_Elab_Calls is called,
   --  the delay table is used to make the delayed calls and this flag is reset
   --  to False, so that the calls are processed.

   -----------------------
   -- Local Subprograms --
   -----------------------

   --  Note: Outer_Scope in all following specs represents the scope of
   --  interest of the outer level call. If it is set to Standard_Standard,
   --  then it means the outer level call was at elaboration level, and that
   --  thus all calls are of interest. If it was set to some other scope,
   --  then the original call was an inner call, and we are not interested
   --  in calls that go outside this scope.

   procedure Activate_Elaborate_All_Desirable (N : Node_Id; U : Entity_Id);
   --  Analysis of construct N shows that we should set Elaborate_All_Desirable
   --  for the WITH clause for unit U (which will always be present). A special
   --  case is when N is a function or procedure instantiation, in which case
   --  it is sufficient to set Elaborate_Desirable, since in this case there is
   --  no possibility of transitive elaboration issues.

   procedure Check_A_Call
     (N                 : Node_Id;
      E                 : Entity_Id;
      Outer_Scope       : Entity_Id;
      Inter_Unit_Only   : Boolean;
      Generate_Warnings : Boolean := True;
      In_Init_Proc      : Boolean := False);
   --  This is the internal recursive routine that is called to check for
   --  possible elaboration error. The argument N is a subprogram call or
   --  generic instantiation, or 'Access attribute reference to be checked, and
   --  E is the entity of the called subprogram, or instantiated generic unit,
   --  or subprogram referenced by 'Access.
   --
   --  In SPARK mode, N can also be a variable reference, since in SPARK this
   --  also triggers a requirement for Elaborate_All, and in this case E is the
   --  entity being referenced.
   --
   --  Outer_Scope is the outer level scope for the original reference.
   --  Inter_Unit_Only is set if the call is only to be checked in the
   --  case where it is to another unit (and skipped if within a unit).
   --  Generate_Warnings is set to False to suppress warning messages about
   --  missing pragma Elaborate_All's. These messages are not wanted for
   --  inner calls in the dynamic model. Note that an instance of the Access
   --  attribute applied to a subprogram also generates a call to this
   --  procedure (since the referenced subprogram may be called later
   --  indirectly). Flag In_Init_Proc should be set whenever the current
   --  context is a type init proc.
   --
   --  Note: this might better be called Check_A_Reference to recognize the
   --  variable case for SPARK, but we prefer to retain the historical name
   --  since in practice this is mostly about checking calls for the possible
   --  occurrence of an access-before-elaboration exception.

   procedure Check_Bad_Instantiation (N : Node_Id);
   --  N is a node for an instantiation (if called with any other node kind,
   --  Check_Bad_Instantiation ignores the call). This subprogram checks for
   --  the special case of a generic instantiation of a generic spec in the
   --  same declarative part as the instantiation where a body is present and
   --  has not yet been seen. This is an obvious error, but needs to be checked
   --  specially at the time of the instantiation, since it is a case where we
   --  cannot insert the body anywhere. If this case is detected, warnings are
   --  generated, and a raise of Program_Error is inserted. In addition any
   --  subprograms in the generic spec are stubbed, and the Bad_Instantiation
   --  flag is set on the instantiation node. The caller in Sem_Ch12 uses this
   --  flag as an indication that no attempt should be made to insert an
   --  instance body.

   procedure Check_Internal_Call
     (N           : Node_Id;
      E           : Entity_Id;
      Outer_Scope : Entity_Id;
      Orig_Ent    : Entity_Id);
   --  N is a function call or procedure statement call node and E is the
   --  entity of the called function, which is within the current compilation
   --  unit (where subunits count as part of the parent). This call checks if
   --  this call, or any call within any accessed body could cause an ABE, and
   --  if so, outputs a warning. Orig_Ent differs from E only in the case of
   --  renamings, and points to the original name of the entity. This is used
   --  for error messages. Outer_Scope is the outer level scope for the
   --  original call.

   procedure Check_Internal_Call_Continue
     (N           : Node_Id;
      E           : Entity_Id;
      Outer_Scope : Entity_Id;
      Orig_Ent    : Entity_Id);
   --  The processing for Check_Internal_Call is divided up into two phases,
   --  and this represents the second phase. The second phase is delayed if
   --  Delaying_Elab_Calls is set to True. In this delayed case, the first
   --  phase makes an entry in the Delay_Check table, which is processed when
   --  Check_Elab_Calls is called. N, E and Orig_Ent are as for the call to
   --  Check_Internal_Call. Outer_Scope is the outer level scope for the
   --  original call.

   function Has_Generic_Body (N : Node_Id) return Boolean;
   --  N is a generic package instantiation node, and this routine determines
   --  if this package spec does in fact have a generic body. If so, then
   --  True is returned, otherwise False. Note that this is not at all the
   --  same as checking if the unit requires a body, since it deals with
   --  the case of optional bodies accurately (i.e. if a body is optional,
   --  then it looks to see if a body is actually present). Note: this
   --  function can only do a fully correct job if in generating code mode
   --  where all bodies have to be present. If we are operating in semantics
   --  check only mode, then in some cases of optional bodies, a result of
   --  False may incorrectly be given. In practice this simply means that
   --  some cases of warnings for incorrect order of elaboration will only
   --  be given when generating code, which is not a big problem (and is
   --  inevitable, given the optional body semantics of Ada).

   procedure Insert_Elab_Check (N : Node_Id; C : Node_Id := Empty);
   --  Given code for an elaboration check (or unconditional raise if the check
   --  is not needed), inserts the code in the appropriate place. N is the call
   --  or instantiation node for which the check code is required. C is the
   --  test whose failure triggers the raise.

   function Is_Call_Of_Generic_Formal (N : Node_Id) return Boolean;
   --  Returns True if node N is a call to a generic formal subprogram

   function Is_Finalization_Procedure (Id : Entity_Id) return Boolean;
   --  Determine whether entity Id denotes a [Deep_]Finalize procedure

   procedure Output_Calls
     (N               : Node_Id;
      Check_Elab_Flag : Boolean);
   --  Outputs chain of calls stored in the Elab_Call table. The caller has
   --  already generated the main warning message, so the warnings generated
   --  are all continuation messages. The argument is the call node at which
   --  the messages are to be placed. When Check_Elab_Flag is set, calls are
   --  enumerated only when flag Elab_Warning is set for the dynamic case or
   --  when flag Elab_Info_Messages is set for the static case.

   function Same_Elaboration_Scope (Scop1, Scop2 : Entity_Id) return Boolean;
   --  Given two scopes, determine whether they are the same scope from an
   --  elaboration point of view, i.e. packages and blocks are ignored.

   procedure Set_C_Scope;
   --  On entry C_Scope is set to some scope. On return, C_Scope is reset
   --  to be the enclosing compilation unit of this scope.

   function Get_Referenced_Ent (N : Node_Id) return Entity_Id;
   --  N is either a function or procedure call or an access attribute that
   --  references a subprogram. This call retrieves the relevant entity. If
   --  this is a call to a protected subprogram, the entity is a selected
   --  component. The callable entity may be absent, in which case Empty is
   --  returned. This happens with non-analyzed calls in nested generics.
   --
   --  If SPARK_Mode is On, then N can also be a reference to an E_Variable
   --  entity, in which case, the value returned is simply this entity.

   procedure Set_Elaboration_Constraint
    (Call : Node_Id;
     Subp : Entity_Id;
     Scop : Entity_Id);
   --  The current unit U may depend semantically on some unit P which is not
   --  in the current context. If there is an elaboration call that reaches P,
   --  we need to indicate that P requires an Elaborate_All, but this is not
   --  effective in U's ali file, if there is no with_clause for P. In this
   --  case we add the Elaborate_All on the unit Q that directly or indirectly
   --  makes P available. This can happen in two cases:
   --
   --    a) Q declares a subtype of a type declared in P, and the call is an
   --    initialization call for an object of that subtype.
   --
   --    b) Q declares an object of some tagged type whose root type is
   --    declared in P, and the initialization call uses object notation on
   --    that object to reach a primitive operation or a classwide operation
   --    declared in P.
   --
   --  If P appears in the context of U, the current processing is correct.
   --  Otherwise we must identify these two cases to retrieve Q and place the
   --  Elaborate_All_Desirable on it.

   function Spec_Entity (E : Entity_Id) return Entity_Id;
   --  Given a compilation unit entity, if it is a spec entity, it is returned
   --  unchanged. If it is a body entity, then the spec for the corresponding
   --  spec is returned

   procedure Supply_Bodies (N : Node_Id);
   --  Given a node, N, that is either a subprogram declaration or a package
   --  declaration, this procedure supplies dummy bodies for the subprogram
   --  or for all subprograms in the package. If the given node is not one of
   --  these two possibilities, then Supply_Bodies does nothing. The dummy body
   --  contains a single Raise statement.

   procedure Supply_Bodies (L : List_Id);
   --  Calls Supply_Bodies for all elements of the given list L

   function Within (E1, E2 : Entity_Id) return Boolean;
   --  Given two scopes E1 and E2, returns True if E1 is equal to E2, or is one
   --  of its contained scopes, False otherwise.

   function Within_Elaborate_All
     (Unit : Unit_Number_Type;
      E    : Entity_Id) return Boolean;
   --  Return True if we are within the scope of an Elaborate_All for E, or if
   --  we are within the scope of an Elaborate_All for some other unit U, and U
   --  with's E. This prevents spurious warnings when the called entity is
   --  renamed within U, or in case of generic instances.

   --------------------------------------
   -- Activate_Elaborate_All_Desirable --
   --------------------------------------

   procedure Activate_Elaborate_All_Desirable (N : Node_Id; U : Entity_Id) is
      UN  : constant Unit_Number_Type := Get_Code_Unit (N);
      CU  : constant Node_Id          := Cunit (UN);
      UE  : constant Entity_Id        := Cunit_Entity (UN);
      Unm : constant Unit_Name_Type   := Unit_Name (UN);
      CI  : constant List_Id          := Context_Items (CU);
      Itm : Node_Id;
      Ent : Entity_Id;

      procedure Add_To_Context_And_Mark (Itm : Node_Id);
      --  This procedure is called when the elaborate indication must be
      --  applied to a unit not in the context of the referencing unit. The
      --  unit gets added to the context as an implicit with.

      function In_Withs_Of (UEs : Entity_Id) return Boolean;
      --  UEs is the spec entity of a unit. If the unit to be marked is
      --  in the context item list of this unit spec, then the call returns
      --  True and Itm is left set to point to the relevant N_With_Clause node.

      procedure Set_Elab_Flag (Itm : Node_Id);
      --  Sets Elaborate_[All_]Desirable as appropriate on Itm

      -----------------------------
      -- Add_To_Context_And_Mark --
      -----------------------------

      procedure Add_To_Context_And_Mark (Itm : Node_Id) is
         CW : constant Node_Id :=
                Make_With_Clause (Sloc (Itm),
                  Name => Name (Itm));

      begin
         Set_Library_Unit  (CW, Library_Unit (Itm));
         Set_Implicit_With (CW, True);

         --  Set elaborate all desirable on copy and then append the copy to
         --  the list of body with's and we are done.

         Set_Elab_Flag (CW);
         Append_To (CI, CW);
      end Add_To_Context_And_Mark;

      -----------------
      -- In_Withs_Of --
      -----------------

      function In_Withs_Of (UEs : Entity_Id) return Boolean is
         UNs : constant Unit_Number_Type := Get_Source_Unit (UEs);
         CUs : constant Node_Id          := Cunit (UNs);
         CIs : constant List_Id          := Context_Items (CUs);

      begin
         Itm := First (CIs);
         while Present (Itm) loop
            if Nkind (Itm) = N_With_Clause then
               Ent :=
                 Cunit_Entity (Get_Cunit_Unit_Number (Library_Unit (Itm)));

               if U = Ent then
                  return True;
               end if;
            end if;

            Next (Itm);
         end loop;

         return False;
      end In_Withs_Of;

      -------------------
      -- Set_Elab_Flag --
      -------------------

      procedure Set_Elab_Flag (Itm : Node_Id) is
      begin
         if Nkind (N) in N_Subprogram_Instantiation then
            Set_Elaborate_Desirable (Itm);
         else
            Set_Elaborate_All_Desirable (Itm);
         end if;
      end Set_Elab_Flag;

   --  Start of processing for Activate_Elaborate_All_Desirable

   begin
      --  Do not set binder indication if expansion is disabled, as when
      --  compiling a generic unit.

      if not Expander_Active then
         return;
      end if;

      Itm := First (CI);
      while Present (Itm) loop
         if Nkind (Itm) = N_With_Clause then
            Ent := Cunit_Entity (Get_Cunit_Unit_Number (Library_Unit (Itm)));

            --  If we find it, then mark elaborate all desirable and return

            if U = Ent then
               Set_Elab_Flag (Itm);
               return;
            end if;
         end if;

         Next (Itm);
      end loop;

      --  If we fall through then the with clause is not present in the
      --  current unit. One legitimate possibility is that the with clause
      --  is present in the spec when we are a body.

      if Is_Body_Name (Unm)
        and then In_Withs_Of (Spec_Entity (UE))
      then
         Add_To_Context_And_Mark (Itm);
         return;
      end if;

      --  Similarly, we may be in the spec or body of a child unit, where
      --  the unit in question is with'ed by some ancestor of the child unit.

      if Is_Child_Name (Unm) then
         declare
            Pkg : Entity_Id;

         begin
            Pkg := UE;
            loop
               Pkg := Scope (Pkg);
               exit when Pkg = Standard_Standard;

               if In_Withs_Of (Pkg) then
                  Add_To_Context_And_Mark (Itm);
                  return;
               end if;
            end loop;
         end;
      end if;

      --  Here if we do not find with clause on spec or body. We just ignore
      --  this case, it means that the elaboration involves some other unit
      --  than the unit being compiled, and will be caught elsewhere.

      null;
   end Activate_Elaborate_All_Desirable;

   ------------------
   -- Check_A_Call --
   ------------------

   procedure Check_A_Call
     (N                 : Node_Id;
      E                 : Entity_Id;
      Outer_Scope       : Entity_Id;
      Inter_Unit_Only   : Boolean;
      Generate_Warnings : Boolean := True;
      In_Init_Proc      : Boolean := False)
   is
      Access_Case : constant Boolean := Nkind (N) = N_Attribute_Reference;
      --  Indicates if we have Access attribute case

      Variable_Case : constant Boolean :=
                        Nkind (N) in N_Has_Entity
                          and then Present (Entity (N))
                          and then Ekind (Entity (N)) = E_Variable;
      --  Indicates if we have variable reference case

      procedure Elab_Warning
        (Msg_D : String;
         Msg_S : String;
         Ent   : Node_Or_Entity_Id);
       --  Generate a call to Error_Msg_NE with parameters Msg_D or Msg_S (for
       --  dynamic or static elaboration model), N and Ent. Msg_D is a real
       --  warning (output if Msg_D is non-null and Elab_Warnings is set),
       --  Msg_S is an info message (output if Elab_Info_Messages is set.

      ------------------
      -- Elab_Warning --
      ------------------

      procedure Elab_Warning
        (Msg_D : String;
         Msg_S : String;
         Ent   : Node_Or_Entity_Id)
      is
      begin
         --  Dynamic elaboration checks, real warning

         if Dynamic_Elaboration_Checks then
            if not Access_Case then
               if Msg_D /= "" and then Elab_Warnings then
                  Error_Msg_NE (Msg_D, N, Ent);
               end if;

            --  In the access case emit first warning message as well,
            --  otherwise list of calls will appear as errors.

            elsif Elab_Warnings then
               Error_Msg_NE (Msg_S, N, Ent);
            end if;

         --  Static elaboration checks, info message

         else
            if Elab_Info_Messages then
               Error_Msg_NE (Msg_S, N, Ent);
            end if;
         end if;
      end Elab_Warning;

      --  Local variables

      Loc : constant Source_Ptr := Sloc (N);

      Inst_Case : constant Boolean := Nkind (N) in N_Generic_Instantiation;
      --  Indicates if we have instantiation case

      Ent                  : Entity_Id;
      Callee_Unit_Internal : Boolean;
      Caller_Unit_Internal : Boolean;
      Decl                 : Node_Id;
      Inst_Callee          : Source_Ptr;
      Inst_Caller          : Source_Ptr;
      Unit_Callee          : Unit_Number_Type;
      Unit_Caller          : Unit_Number_Type;

      Body_Acts_As_Spec : Boolean;
      --  Set to true if call is to body acting as spec (no separate spec)

      Cunit_SC : Boolean := False;
      --  Set to suppress dynamic elaboration checks where one of the
      --  enclosing scopes has Elaboration_Checks_Suppressed set, or else
      --  if a pragma Elaborate[_All] applies to that scope, in which case
      --  warnings on the scope are also suppressed. For the internal case,
      --  we ignore this flag.

      E_Scope : Entity_Id;
      --  Top level scope of entity for called subprogram. This value includes
      --  following renamings and derivations, so this scope can be in a
      --  non-visible unit. This is the scope that is to be investigated to
      --  see whether an elaboration check is required.

      Is_DIC_Proc : Boolean := False;
      --  Flag set when the call denotes the Default_Initial_Condition
      --  procedure of a private type that wraps a nontrivial assertion
      --  expression.

      Issue_In_SPARK : Boolean;
      --  Flag set when a source entity is called during elaboration in SPARK

      W_Scope : Entity_Id;
      --  Top level scope of directly called entity for subprogram. This
      --  differs from E_Scope in the case where renamings or derivations
      --  are involved, since it does not follow these links. W_Scope is
      --  generally in a visible unit, and it is this scope that may require
      --  an Elaborate_All. However, there are some cases (initialization
      --  calls and calls involving object notation) where W_Scope might not
      --  be in the context of the current unit, and there is an intermediate
      --  package that is, in which case the Elaborate_All has to be placed
      --  on this intermediate package. These special cases are handled in
      --  Set_Elaboration_Constraint.

   --  Start of processing for Check_A_Call

   begin
      --  If the call is known to be within a local Suppress Elaboration
      --  pragma, nothing to check. This can happen in task bodies. But
      --  we ignore this for a call to a generic formal.

      if Nkind (N) in N_Subprogram_Call
        and then No_Elaboration_Check (N)
        and then not Is_Call_Of_Generic_Formal (N)
      then
         return;
      end if;

      --  If this is a rewrite of a Valid_Scalars attribute, then nothing to
      --  check, we don't mind in this case if the call occurs before the body
      --  since this is all generated code.

      if Nkind (Original_Node (N)) = N_Attribute_Reference
        and then Attribute_Name (Original_Node (N)) = Name_Valid_Scalars
      then
         return;
      end if;

      --  Proceed with check

      Ent := E;

      --  For a variable reference, just set Body_Acts_As_Spec to False

      if Variable_Case then
         Body_Acts_As_Spec := False;

      --  Additional checks for all other cases

      else
         --  Go to parent for derived subprogram, or to original subprogram in
         --  the case of a renaming (Alias covers both these cases).

         loop
            if (Suppress_Elaboration_Warnings (Ent)
                or else Elaboration_Checks_Suppressed (Ent))
              and then (Inst_Case or else No (Alias (Ent)))
            then
               return;
            end if;

            --  Nothing to do for imported entities

            if Is_Imported (Ent) then
               return;
            end if;

            exit when Inst_Case or else No (Alias (Ent));
            Ent := Alias (Ent);
         end loop;

         Decl := Unit_Declaration_Node (Ent);

         if Nkind (Decl) = N_Subprogram_Body then
            Body_Acts_As_Spec := True;

         elsif Nkind_In (Decl, N_Subprogram_Declaration,
                               N_Subprogram_Body_Stub)
           or else Inst_Case
         then
            Body_Acts_As_Spec := False;

         --  If we have none of an instantiation, subprogram body or subprogram
         --  declaration, or in the SPARK case, a variable reference, then
         --  it is not a case that we want to check. (One case is a call to a
         --  generic formal subprogram, where we do not want the check in the
         --  template).

         else
            return;
         end if;
      end if;

      E_Scope := Ent;
      loop
         if Elaboration_Checks_Suppressed (E_Scope)
           or else Suppress_Elaboration_Warnings (E_Scope)
         then
            Cunit_SC := True;
         end if;

         --  Exit when we get to compilation unit, not counting subunits

         exit when Is_Compilation_Unit (E_Scope)
           and then (Is_Child_Unit (E_Scope)
                      or else Scope (E_Scope) = Standard_Standard);

         --  If we did not find a compilation unit, other than standard,
         --  then nothing to check (happens in some instantiation cases)

         if E_Scope = Standard_Standard then
            return;

         --  Otherwise move up a scope looking for compilation unit

         else
            E_Scope := Scope (E_Scope);
         end if;
      end loop;

      --  No checks needed for pure or preelaborated compilation units

      if Is_Pure (E_Scope) or else Is_Preelaborated (E_Scope) then
         return;
      end if;

      --  If the generic entity is within a deeper instance than we are, then
      --  either the instantiation to which we refer itself caused an ABE, in
      --  which case that will be handled separately, or else we know that the
      --  body we need appears as needed at the point of the instantiation.
      --  However, this assumption is only valid if we are in static mode.

      if not Dynamic_Elaboration_Checks
        and then
          Instantiation_Depth (Sloc (Ent)) > Instantiation_Depth (Sloc (N))
      then
         return;
      end if;

      --  Do not give a warning for a package with no body

      if Ekind (Ent) = E_Generic_Package and then not Has_Generic_Body (N) then
         return;
      end if;

      --  Find top level scope for called entity (not following renamings
      --  or derivations). This is where the Elaborate_All will go if it is
      --  needed. We start with the called entity, except in the case of an
      --  initialization procedure outside the current package, where the init
      --  proc is in the root package, and we start from the entity of the name
      --  in the call.

      declare
         Ent : constant Entity_Id := Get_Referenced_Ent (N);
      begin
         if Is_Init_Proc (Ent) and then not In_Same_Extended_Unit (N, Ent) then
            W_Scope := Scope (Ent);
         else
            W_Scope := E;
         end if;
      end;

      --  Now loop through scopes to get to the enclosing compilation unit

      while not Is_Compilation_Unit (W_Scope) loop
         W_Scope := Scope (W_Scope);
      end loop;

      --  Case of entity is in same unit as call or instantiation. In the
      --  instantiation case, W_Scope may be different from E_Scope; we want
      --  the unit in which the instantiation occurs, since we're analyzing
      --  based on the expansion.

      if W_Scope = C_Scope then
         if not Inter_Unit_Only then
            Check_Internal_Call (N, Ent, Outer_Scope, E);
         end if;

         return;
      end if;

      --  Case of entity is not in current unit (i.e. with'ed unit case)

      --  We are only interested in such calls if the outer call was from
      --  elaboration code, or if we are in Dynamic_Elaboration_Checks mode.

      if not From_Elab_Code and then not Dynamic_Elaboration_Checks then
         return;
      end if;

      --  Nothing to do if some scope said that no checks were required

      if Cunit_SC then
         return;
      end if;

      --  Nothing to do for a generic instance, because in this case the
      --  checking was at the point of instantiation of the generic However,
      --  this shortcut is only applicable in static mode.

      if Is_Generic_Instance (Ent) and not Dynamic_Elaboration_Checks then
         return;
      end if;

      --  Nothing to do if subprogram with no separate spec. However, a call
      --  to Deep_Initialize may result in a call to a user-defined Initialize
      --  procedure, which imposes a body dependency. This happens only if the
      --  type is controlled and the Initialize procedure is not inherited.

      if Body_Acts_As_Spec then
         if Is_TSS (Ent, TSS_Deep_Initialize) then
            declare
               Typ  : constant Entity_Id := Etype (First_Formal (Ent));
               Init : Entity_Id;

            begin
               if not Is_Controlled (Typ) then
                  return;
               else
                  Init := Find_Prim_Op (Typ, Name_Initialize);

                  if Comes_From_Source (Init) then
                     Ent := Init;
                  else
                     return;
                  end if;
               end if;
            end;

         else
            return;
         end if;
      end if;

      --  Check cases of internal units

      Callee_Unit_Internal :=
        Is_Internal_File_Name (Unit_File_Name (Get_Source_Unit (E_Scope)));

      --  Do not give a warning if the with'ed unit is internal and this is
      --  the generic instantiation case (this saves a lot of hassle dealing
      --  with the Text_IO special child units)

      if Callee_Unit_Internal and Inst_Case then
         return;
      end if;

      if C_Scope = Standard_Standard then
         Caller_Unit_Internal := False;
      else
         Caller_Unit_Internal :=
           Is_Internal_File_Name (Unit_File_Name (Get_Source_Unit (C_Scope)));
      end if;

      --  Do not give a warning if the with'ed unit is internal and the
      --  caller is not internal (since the binder always elaborates
      --  internal units first).

      if Callee_Unit_Internal and (not Caller_Unit_Internal) then
         return;
      end if;

      --  For now, if debug flag -gnatdE is not set, do no checking for
      --  one internal unit withing another. This fixes the problem with
      --  the sgi build and storage errors. To be resolved later ???

      if (Callee_Unit_Internal and Caller_Unit_Internal)
        and not Debug_Flag_EE
      then
         return;
      end if;

      if Is_TSS (E, TSS_Deep_Initialize) then
         Ent := E;
      end if;

      --  If the call is in an instance, and the called entity is not
      --  defined in the same instance, then the elaboration issue focuses
      --  around the unit containing the template, it is this unit which
      --  requires an Elaborate_All.

      --  However, if we are doing dynamic elaboration, we need to chase the
      --  call in the usual manner.

      --  We also need to chase the call in the usual manner if it is a call
      --  to a generic formal parameter, since that case was not handled as
      --  part of the processing of the template.

      Inst_Caller := Instantiation (Get_Source_File_Index (Sloc (N)));
      Inst_Callee := Instantiation (Get_Source_File_Index (Sloc (Ent)));

      if Inst_Caller = No_Location then
         Unit_Caller := No_Unit;
      else
         Unit_Caller := Get_Source_Unit (N);
      end if;

      if Inst_Callee = No_Location then
         Unit_Callee := No_Unit;
      else
         Unit_Callee := Get_Source_Unit (Ent);
      end if;

      if Unit_Caller /= No_Unit
        and then Unit_Callee /= Unit_Caller
        and then not Dynamic_Elaboration_Checks
        and then not Is_Call_Of_Generic_Formal (N)
      then
         E_Scope := Spec_Entity (Cunit_Entity (Unit_Caller));

         --  If we don't get a spec entity, just ignore call. Not quite
         --  clear why this check is necessary. ???

         if No (E_Scope) then
            return;
         end if;

         --  Otherwise step to enclosing compilation unit

         while not Is_Compilation_Unit (E_Scope) loop
            E_Scope := Scope (E_Scope);
         end loop;

      --  For the case where N is not an instance, and is not a call within
      --  instance to other than a generic formal, we recompute E_Scope
      --  for the error message, since we do NOT want to go to the unit
      --  which has the ultimate declaration in the case of renaming and
      --  derivation and we also want to go to the generic unit in the
      --  case of an instance, and no further.

      else
         --  Loop to carefully follow renamings and derivations one step
         --  outside the current unit, but not further.

         if not (Inst_Case or Variable_Case)
           and then Present (Alias (Ent))
         then
            E_Scope := Alias (Ent);
         else
            E_Scope := Ent;
         end if;

         loop
            while not Is_Compilation_Unit (E_Scope) loop
               E_Scope := Scope (E_Scope);
            end loop;

            --  If E_Scope is the same as C_Scope, it means that there
            --  definitely was a local renaming or derivation, and we
            --  are not yet out of the current unit.

            exit when E_Scope /= C_Scope;
            Ent := Alias (Ent);
            E_Scope := Ent;

            --  If no alias, there could be a previous error, but not if we've
            --  already reached the outermost level (Standard).

            if No (Ent) then
               return;
            end if;
         end loop;
      end if;

      if Within_Elaborate_All (Current_Sem_Unit, E_Scope) then
         return;
      end if;

      Is_DIC_Proc := Is_Nontrivial_Default_Init_Cond_Procedure (Ent);

      --  Elaboration issues in SPARK are reported only for source constructs
      --  and for nontrivial Default_Initial_Condition procedures. The latter
      --  must be checked because the default initialization of an object of a
      --  private type triggers the evaluation of the Default_Initial_Condition
      --  expression, which in turn may have side effects.

      Issue_In_SPARK :=
        SPARK_Mode = On and (Comes_From_Source (Ent) or Is_DIC_Proc);

      --  Now check if an Elaborate_All (or dynamic check) is needed

      if not Suppress_Elaboration_Warnings (Ent)
        and then not Elaboration_Checks_Suppressed (Ent)
        and then not Suppress_Elaboration_Warnings (E_Scope)
        and then not Elaboration_Checks_Suppressed (E_Scope)
        and then ((Elab_Warnings or Elab_Info_Messages)
                    or else SPARK_Mode = On)
        and then Generate_Warnings
      then
         --  Instantiation case

         if Inst_Case then
            if Issue_In_SPARK then
               Error_Msg_NE
                 ("instantiation of & during elaboration in SPARK", N, Ent);
            else
               Elab_Warning
                 ("instantiation of & may raise Program_Error?l?",
                  "info: instantiation of & during elaboration?$?", Ent);
            end if;

         --  Indirect call case, info message only in static elaboration
         --  case, because the attribute reference itself cannot raise an
         --  exception. Note that SPARK does not  permit indirect calls.

         elsif Access_Case then
            Elab_Warning ("", "info: access to & during elaboration?$?", Ent);

         --  Variable reference in SPARK mode

         elsif Variable_Case and Issue_In_SPARK then
            Error_Msg_NE
              ("reference to & during elaboration in SPARK", N, Ent);

         --  Subprogram call case

         else
            if Nkind (Name (N)) in N_Has_Entity
              and then Is_Init_Proc (Entity (Name (N)))
              and then Comes_From_Source (Ent)
            then
               Elab_Warning
                 ("implicit call to & may raise Program_Error?l?",
                  "info: implicit call to & during elaboration?$?",
                  Ent);

            elsif Issue_In_SPARK then

               --  Emit a specialized error message when the elaboration of an
               --  object of a private type evaluates the expression of pragma
               --  Default_Initial_Condition. This prevents the internal name
               --  of the procedure from appearing in the error message.

               if Is_DIC_Proc then
                  Error_Msg_N
                    ("call to Default_Initial_Condition during elaboration in "
                     & "SPARK", N);
               else
                  Error_Msg_NE
                    ("call to & during elaboration in SPARK", N, Ent);
               end if;

            else
               Elab_Warning
                 ("call to & may raise Program_Error?l?",
                  "info: call to & during elaboration?$?",
                  Ent);
            end if;
         end if;

         Error_Msg_Qual_Level := Nat'Last;

         --  Case of Elaborate_All not present and required, for SPARK this
         --  is an error, so give an error message.

         if Issue_In_SPARK then
            Error_Msg_NE ("\Elaborate_All pragma required for&", N, W_Scope);

         --  Otherwise we generate an implicit pragma. For a subprogram
         --  instantiation, Elaborate is good enough, since no transitive
         --  call is possible at elaboration time in this case.

         elsif Nkind (N) in N_Subprogram_Instantiation then
            Elab_Warning
              ("\missing pragma Elaborate for&?l?",
               "\implicit pragma Elaborate for& generated?$?",
               W_Scope);

         --  For all other cases, we need an implicit Elaborate_All

         else
            Elab_Warning
              ("\missing pragma Elaborate_All for&?l?",
               "\implicit pragma Elaborate_All for & generated?$?",
               W_Scope);
         end if;

         Error_Msg_Qual_Level := 0;

         --  Take into account the flags related to elaboration warning
         --  messages when enumerating the various calls involved. This
         --  ensures the proper pairing of the main warning and the
         --  clarification messages generated by Output_Calls.

         Output_Calls (N, Check_Elab_Flag => True);

         --  Set flag to prevent further warnings for same unit unless in
         --  All_Errors_Mode.

         if not All_Errors_Mode and not Dynamic_Elaboration_Checks then
            Set_Suppress_Elaboration_Warnings (W_Scope, True);
         end if;
      end if;

      --  Check for runtime elaboration check required

      if Dynamic_Elaboration_Checks then
         if not Elaboration_Checks_Suppressed (Ent)
           and then not Elaboration_Checks_Suppressed (W_Scope)
           and then not Elaboration_Checks_Suppressed (E_Scope)
           and then not Cunit_SC
         then
            --  Runtime elaboration check required. Generate check of the
            --  elaboration Boolean for the unit containing the entity.

            --  Note that for this case, we do check the real unit (the one
            --  from following renamings, since that is the issue).

            --  Could this possibly miss a useless but required PE???

            Insert_Elab_Check (N,
              Make_Attribute_Reference (Loc,
                Attribute_Name => Name_Elaborated,
                Prefix         =>
                  New_Occurrence_Of (Spec_Entity (E_Scope), Loc)));

            --  Prevent duplicate elaboration checks on the same call,
            --  which can happen if the body enclosing the call appears
            --  itself in a call whose elaboration check is delayed.

            if Nkind (N) in N_Subprogram_Call then
               Set_No_Elaboration_Check (N);
            end if;
         end if;

      --  Case of static elaboration model

      else
         --  Do not do anything if elaboration checks suppressed. Note that
         --  we check Ent here, not E, since we want the real entity for the
         --  body to see if checks are suppressed for it, not the dummy
         --  entry for renamings or derivations.

         if Elaboration_Checks_Suppressed (Ent)
           or else Elaboration_Checks_Suppressed (E_Scope)
           or else Elaboration_Checks_Suppressed (W_Scope)
         then
            null;

         --  Do not generate an Elaborate_All for finalization routines
         --  which perform partial clean up as part of initialization.

         elsif In_Init_Proc and then Is_Finalization_Procedure (Ent) then
            null;

         --  Here we need to generate an implicit elaborate all

         else
            --  Generate Elaborate_All warning unless suppressed

            if (Elab_Info_Messages and Generate_Warnings and not Inst_Case)
              and then not Suppress_Elaboration_Warnings (Ent)
              and then not Suppress_Elaboration_Warnings (E_Scope)
              and then not Suppress_Elaboration_Warnings (W_Scope)
            then
               Error_Msg_Node_2 := W_Scope;
               Error_Msg_NE
                 ("info: call to& in elaboration code " &
                  "requires pragma Elaborate_All on&?$?", N, E);
            end if;

            --  Set indication for binder to generate Elaborate_All

            Set_Elaboration_Constraint (N, E, W_Scope);
         end if;
      end if;
   end Check_A_Call;

   -----------------------------
   -- Check_Bad_Instantiation --
   -----------------------------

   procedure Check_Bad_Instantiation (N : Node_Id) is
      Ent : Entity_Id;

   begin
      --  Nothing to do if we do not have an instantiation (happens in some
      --  error cases, and also in the formal package declaration case)

      if Nkind (N) not in N_Generic_Instantiation then
         return;

      --  Nothing to do if serious errors detected (avoid cascaded errors)

      elsif Serious_Errors_Detected /= 0 then
         return;

      --  Nothing to do if not in full analysis mode

      elsif not Full_Analysis then
         return;

      --  Nothing to do if inside a generic template

      elsif Inside_A_Generic then
         return;

      --  Nothing to do if a library level instantiation

      elsif Nkind (Parent (N)) = N_Compilation_Unit then
         return;

      --  Nothing to do if we are compiling a proper body for semantic
      --  purposes only. The generic body may be in another proper body.

      elsif
        Nkind (Parent (Unit_Declaration_Node (Main_Unit_Entity))) = N_Subunit
      then
         return;
      end if;

      Ent := Get_Generic_Entity (N);

      --  The case we are interested in is when the generic spec is in the
      --  current declarative part

      if not Same_Elaboration_Scope (Current_Scope, Scope (Ent))
        or else not In_Same_Extended_Unit (N, Ent)
      then
         return;
      end if;

      --  If the generic entity is within a deeper instance than we are, then
      --  either the instantiation to which we refer itself caused an ABE, in
      --  which case that will be handled separately. Otherwise, we know that
      --  the body we need appears as needed at the point of the instantiation.
      --  If they are both at the same level but not within the same instance
      --  then the body of the generic will be in the earlier instance.

      declare
         D1 : constant Nat := Instantiation_Depth (Sloc (Ent));
         D2 : constant Nat := Instantiation_Depth (Sloc (N));

      begin
         if D1 > D2 then
            return;

         elsif D1 = D2
           and then Is_Generic_Instance (Scope (Ent))
           and then not In_Open_Scopes (Scope (Ent))
         then
            return;
         end if;
      end;

      --  Now we can proceed, if the entity being called has a completion,
      --  then we are definitely OK, since we have already seen the body.

      if Has_Completion (Ent) then
         return;
      end if;

      --  If there is no body, then nothing to do

      if not Has_Generic_Body (N) then
         return;
      end if;

      --  Here we definitely have a bad instantiation

      Error_Msg_Warn := SPARK_Mode /= On;
      Error_Msg_NE ("cannot instantiate& before body seen<<", N, Ent);

      if Present (Instance_Spec (N)) then
         Supply_Bodies (Instance_Spec (N));
      end if;

      Error_Msg_N ("\Program_Error [<<", N);
      Insert_Elab_Check (N);
      Set_ABE_Is_Certain (N);
   end Check_Bad_Instantiation;

   ---------------------
   -- Check_Elab_Call --
   ---------------------

   procedure Check_Elab_Call
     (N            : Node_Id;
      Outer_Scope  : Entity_Id := Empty;
      In_Init_Proc : Boolean   := False)
   is
      Ent : Entity_Id;
      P   : Node_Id;

   begin
      --  If the reference is not in the main unit, there is nothing to check.
      --  Elaboration call from units in the context of the main unit will lead
      --  to semantic dependencies when those units are compiled.

      if not In_Extended_Main_Code_Unit (N) then
         return;
      end if;

      --  For an entry call, check relevant restriction

      if Nkind (N) = N_Entry_Call_Statement
        and then not In_Subprogram_Or_Concurrent_Unit
      then
         Check_Restriction (No_Entry_Calls_In_Elaboration_Code, N);

      --  Nothing to do if this is not an expected type of reference (happens
      --  in some error conditions, and in some cases where rewriting occurs).

      elsif Nkind (N) not in N_Subprogram_Call
        and then Nkind (N) /= N_Attribute_Reference
        and then (SPARK_Mode /= On
                   or else Nkind (N) not in N_Has_Entity
                   or else No (Entity (N))
                   or else Ekind (Entity (N)) /= E_Variable)
      then
         return;

      --  Nothing to do if this is a call already rewritten for elab checking.
      --  Such calls appear as the targets of If_Expressions.

      --  This check MUST be wrong, it catches far too much

      elsif Nkind (Parent (N)) = N_If_Expression then
         return;

      --  Nothing to do if inside a generic template

      elsif Inside_A_Generic
        and then No (Enclosing_Generic_Body (N))
      then
         return;

      --  Nothing to do if call is being pre-analyzed, as when within a
      --  pre/postcondition, a predicate, or an invariant.

      elsif In_Spec_Expression then
         return;
      end if;

      --  Nothing to do if this is a call to a postcondition, which is always
      --  within a subprogram body, even though the current scope may be the
      --  enclosing scope of the subprogram.

      if Nkind (N) = N_Procedure_Call_Statement
        and then Is_Entity_Name (Name (N))
        and then Chars (Entity (Name (N))) = Name_uPostconditions
      then
         return;
      end if;

      --  Here we have a reference at elaboration time which must be checked

      if Debug_Flag_LL then
         Write_Str ("  Check_Elab_Ref: ");

         if Nkind (N) = N_Attribute_Reference then
            if not Is_Entity_Name (Prefix (N)) then
               Write_Str ("<<not entity name>>");
            else
               Write_Name (Chars (Entity (Prefix (N))));
            end if;

            Write_Str ("'Access");

         elsif No (Name (N)) or else not Is_Entity_Name (Name (N)) then
            Write_Str ("<<not entity name>> ");

         else
            Write_Name (Chars (Entity (Name (N))));
         end if;

         Write_Str ("  reference at ");
         Write_Location (Sloc (N));
         Write_Eol;
      end if;

      --  Climb up the tree to make sure we are not inside default expression
      --  of a parameter specification or a record component, since in both
      --  these cases, we will be doing the actual reference later, not now,
      --  and it is at the time of the actual reference (statically speaking)
      --  that we must do our static check, not at the time of its initial
      --  analysis).

      --  However, we have to check references within component definitions
      --  (e.g. a function call that determines an array component bound),
      --  so we terminate the loop in that case.

      P := Parent (N);
      while Present (P) loop
         if Nkind_In (P, N_Parameter_Specification,
                         N_Component_Declaration)
         then
            return;

         --  The reference occurs within the constraint of a component,
         --  so it must be checked.

         elsif Nkind (P) = N_Component_Definition then
            exit;

         else
            P := Parent (P);
         end if;
      end loop;

      --  Stuff that happens only at the outer level

      if No (Outer_Scope) then
         Elab_Visited.Set_Last (0);

         --  Nothing to do if current scope is Standard (this is a bit odd, but
         --  it happens in the case of generic instantiations).

         C_Scope := Current_Scope;

         if C_Scope = Standard_Standard then
            return;
         end if;

         --  First case, we are in elaboration code

         From_Elab_Code := not In_Subprogram_Or_Concurrent_Unit;

         if From_Elab_Code then

            --  Complain if ref that comes from source in preelaborated unit
            --  and we are not inside a subprogram (i.e. we are in elab code).

            if Comes_From_Source (N)
              and then In_Preelaborated_Unit
              and then not In_Inlined_Body
              and then Nkind (N) /= N_Attribute_Reference
            then
               --  This is a warning in GNAT mode allowing such calls to be
               --  used in the predefined library with appropriate care.

               Error_Msg_Warn := GNAT_Mode;
               Error_Msg_N
                 ("<<non-static call not allowed in preelaborated unit", N);
               return;
            end if;

         --  Second case, we are inside a subprogram or concurrent unit, which
         --  means we are not in elaboration code.

         else
            --  In this case, the issue is whether we are inside the
            --  declarative part of the unit in which we live, or inside its
            --  statements. In the latter case, there is no issue of ABE calls
            --  at this level (a call from outside to the unit in which we live
            --  might cause an ABE, but that will be detected when we analyze
            --  that outer level call, as it recurses into the called unit).

            --  Climb up the tree, doing this test, and also testing for being
            --  inside a default expression, which, as discussed above, is not
            --  checked at this stage.

            declare
               P : Node_Id;
               L : List_Id;

            begin
               P := N;
               loop
                  --  If we find a parentless subtree, it seems safe to assume
                  --  that we are not in a declarative part and that no
                  --  checking is required.

                  if No (P) then
                     return;
                  end if;

                  if Is_List_Member (P) then
                     L := List_Containing (P);
                     P := Parent (L);
                  else
                     L := No_List;
                     P := Parent (P);
                  end if;

                  exit when Nkind (P) = N_Subunit;

                  --  Filter out case of default expressions, where we do not
                  --  do the check at this stage.

                  if Nkind_In (P, N_Parameter_Specification,
                                  N_Component_Declaration)
                  then
                     return;
                  end if;

                  --  A protected body has no elaboration code and contains
                  --  only other bodies.

                  if Nkind (P) = N_Protected_Body then
                     return;

                  elsif Nkind_In (P, N_Subprogram_Body,
                                     N_Task_Body,
                                     N_Block_Statement,
                                     N_Entry_Body)
                  then
                     if L = Declarations (P) then
                        exit;

                     --  We are not in elaboration code, but we are doing
                     --  dynamic elaboration checks, in this case, we still
                     --  need to do the reference, since the subprogram we are
                     --  in could be called from another unit, also in dynamic
                     --  elaboration check mode, at elaboration time.

                     elsif Dynamic_Elaboration_Checks then

                        --  We provide a debug flag to disable this check. That
                        --  way we have an easy work around for regressions
                        --  that are caused by this new check. This debug flag
                        --  can be removed later.

                        if Debug_Flag_DD then
                           return;
                        end if;

                        --  Do the check in this case

                        exit;

                     elsif Nkind (P) = N_Task_Body then

                        --  The check is deferred until Check_Task_Activation
                        --  but we need to capture local suppress pragmas
                        --  that may inhibit checks on this call.

                        Ent := Get_Referenced_Ent (N);

                        if No (Ent) then
                           return;

                        elsif Elaboration_Checks_Suppressed (Current_Scope)
                          or else Elaboration_Checks_Suppressed (Ent)
                          or else Elaboration_Checks_Suppressed (Scope (Ent))
                        then
                           if Nkind (N) in N_Subprogram_Call then
                              Set_No_Elaboration_Check (N);
                           end if;
                        end if;

                        return;

                     --  Static model, call is not in elaboration code, we
                     --  never need to worry, because in the static model the
                     --  top level caller always takes care of things.

                     else
                        return;
                     end if;
                  end if;
               end loop;
            end;
         end if;
      end if;

      Ent := Get_Referenced_Ent (N);

      if No (Ent) then
         return;
      end if;

      --  Nothing to do if this is a recursive call (i.e. a call to
      --  an entity that is already in the Elab_Call stack)

      for J in 1 .. Elab_Visited.Last loop
         if Ent = Elab_Visited.Table (J) then
            return;
         end if;
      end loop;

      --  See if we need to analyze this reference. We analyze it if either of
      --  the following conditions is met:

      --    It is an inner level call (since in this case it was triggered
      --    by an outer level call from elaboration code), but only if the
      --    call is within the scope of the original outer level call.

      --    It is an outer level reference from elaboration code, or a call to
      --    an entity is in the same elaboration scope.

      --  And in these cases, we will check both inter-unit calls and
      --  intra-unit (within a single unit) calls.

      C_Scope := Current_Scope;

      --  If not outer level reference, then we follow it if it is within the
      --  original scope of the outer reference.

      if Present (Outer_Scope)
        and then Within (Scope (Ent), Outer_Scope)
      then
         Set_C_Scope;
         Check_A_Call
           (N               => N,
            E               => Ent,
            Outer_Scope     => Outer_Scope,
            Inter_Unit_Only => False,
            In_Init_Proc    => In_Init_Proc);

      --  Nothing to do if elaboration checks suppressed for this scope.
      --  However, an interesting exception, the fact that elaboration checks
      --  are suppressed within an instance (because we can trace the body when
      --  we process the template) does not extend to calls to generic formal
      --  subprograms.

      elsif Elaboration_Checks_Suppressed (Current_Scope)
        and then not Is_Call_Of_Generic_Formal (N)
      then
         null;

      elsif From_Elab_Code then
         Set_C_Scope;
         Check_A_Call (N, Ent, Standard_Standard, Inter_Unit_Only => False);

      elsif Same_Elaboration_Scope (C_Scope, Scope (Ent)) then
         Set_C_Scope;
         Check_A_Call (N, Ent, Scope (Ent), Inter_Unit_Only => False);

      --  If none of those cases holds, but Dynamic_Elaboration_Checks mode
      --  is set, then we will do the check, but only in the inter-unit case
      --  (this is to accommodate unguarded elaboration calls from other units
      --  in which this same mode is set). We don't want warnings in this case,
      --  it would generate warnings having nothing to do with elaboration.

      elsif Dynamic_Elaboration_Checks then
         Set_C_Scope;
         Check_A_Call
           (N,
            Ent,
            Standard_Standard,
            Inter_Unit_Only   => True,
            Generate_Warnings => False);

      --  Otherwise nothing to do

      else
         return;
      end if;

      --  A call to an Init_Proc in elaboration code may bring additional
      --  dependencies, if some of the record components thereof have
      --  initializations that are function calls that come from source. We
      --  treat the current node as a call to each of these functions, to check
      --  their elaboration impact.

      if Is_Init_Proc (Ent) and then From_Elab_Code then
         Process_Init_Proc : declare
            Unit_Decl : constant Node_Id := Unit_Declaration_Node (Ent);

            function Check_Init_Call (Nod : Node_Id) return Traverse_Result;
            --  Find subprogram calls within body of Init_Proc for Traverse
            --  instantiation below.

            procedure Traverse_Body is new Traverse_Proc (Check_Init_Call);
            --  Traversal procedure to find all calls with body of Init_Proc

            ---------------------
            -- Check_Init_Call --
            ---------------------

            function Check_Init_Call (Nod : Node_Id) return Traverse_Result is
               Func : Entity_Id;

            begin
               if Nkind (Nod) in N_Subprogram_Call
                 and then Is_Entity_Name (Name (Nod))
               then
                  Func := Entity (Name (Nod));

                  if Comes_From_Source (Func) then
                     Check_A_Call
                       (N, Func, Standard_Standard, Inter_Unit_Only => True);
                  end if;

                  return OK;

               else
                  return OK;
               end if;
            end Check_Init_Call;

         --  Start of processing for Process_Init_Proc

         begin
            if Nkind (Unit_Decl) = N_Subprogram_Body then
               Traverse_Body (Handled_Statement_Sequence (Unit_Decl));
            end if;
         end Process_Init_Proc;
      end if;
   end Check_Elab_Call;

   -----------------------
   -- Check_Elab_Assign --
   -----------------------

   procedure Check_Elab_Assign (N : Node_Id) is
      Ent  : Entity_Id;
      Scop : Entity_Id;

      Pkg_Spec : Entity_Id;
      Pkg_Body : Entity_Id;

   begin
      --  For record or array component, check prefix. If it is an access type,
      --  then there is nothing to do (we do not know what is being assigned),
      --  but otherwise this is an assignment to the prefix.

      if Nkind_In (N, N_Indexed_Component,
                      N_Selected_Component,
                      N_Slice)
      then
         if not Is_Access_Type (Etype (Prefix (N))) then
            Check_Elab_Assign (Prefix (N));
         end if;

         return;
      end if;

      --  For type conversion, check expression

      if Nkind (N) = N_Type_Conversion then
         Check_Elab_Assign (Expression (N));
         return;
      end if;

      --  Nothing to do if this is not an entity reference otherwise get entity

      if Is_Entity_Name (N) then
         Ent := Entity (N);
      else
         return;
      end if;

      --  What we are looking for is a reference in the body of a package that
      --  modifies a variable declared in the visible part of the package spec.

      if Present (Ent)
        and then Comes_From_Source (N)
        and then not Suppress_Elaboration_Warnings (Ent)
        and then Ekind (Ent) = E_Variable
        and then not In_Private_Part (Ent)
        and then Is_Library_Level_Entity (Ent)
      then
         Scop := Current_Scope;
         loop
            if No (Scop) or else Scop = Standard_Standard then
               return;
            elsif Ekind (Scop) = E_Package
              and then Is_Compilation_Unit (Scop)
            then
               exit;
            else
               Scop := Scope (Scop);
            end if;
         end loop;

         --  Here Scop points to the containing library package

         Pkg_Spec := Scop;
         Pkg_Body := Body_Entity (Pkg_Spec);

         --  All OK if the package has an Elaborate_Body pragma

         if Has_Pragma_Elaborate_Body (Scop) then
            return;
         end if;

         --  OK if entity being modified is not in containing package spec

         if not In_Same_Source_Unit (Scop, Ent) then
            return;
         end if;

         --  All OK if entity appears in generic package or generic instance.
         --  We just get too messed up trying to give proper warnings in the
         --  presence of generics. Better no message than a junk one.

         Scop := Scope (Ent);
         while Present (Scop) and then Scop /= Pkg_Spec loop
            if Ekind (Scop) = E_Generic_Package then
               return;
            elsif Ekind (Scop) = E_Package
              and then Is_Generic_Instance (Scop)
            then
               return;
            end if;

            Scop := Scope (Scop);
         end loop;

         --  All OK if in task, don't issue warnings there

         if In_Task_Activation then
            return;
         end if;

         --  OK if no package body

         if No (Pkg_Body) then
            return;
         end if;

         --  OK if reference is not in package body

         if not In_Same_Source_Unit (Pkg_Body, N) then
            return;
         end if;

         --  OK if package body has no handled statement sequence

         declare
            HSS : constant Node_Id :=
                    Handled_Statement_Sequence (Declaration_Node (Pkg_Body));
         begin
            if No (HSS) or else not Comes_From_Source (HSS) then
               return;
            end if;
         end;

         --  We definitely have a case of a modification of an entity in
         --  the package spec from the elaboration code of the package body.
         --  We may not give the warning (because there are some additional
         --  checks to avoid too many false positives), but it would be a good
         --  idea for the binder to try to keep the body elaboration close to
         --  the spec elaboration.

         Set_Elaborate_Body_Desirable (Pkg_Spec);

         --  All OK in gnat mode (we know what we are doing)

         if GNAT_Mode then
            return;
         end if;

         --  All OK if all warnings suppressed

         if Warning_Mode = Suppress then
            return;
         end if;

         --  All OK if elaboration checks suppressed for entity

         if Checks_May_Be_Suppressed (Ent)
           and then Is_Check_Suppressed (Ent, Elaboration_Check)
         then
            return;
         end if;

         --  OK if the entity is initialized. Note that the No_Initialization
         --  flag usually means that the initialization has been rewritten into
         --  assignments, but that still counts for us.

         declare
            Decl : constant Node_Id := Declaration_Node (Ent);
         begin
            if Nkind (Decl) = N_Object_Declaration
              and then (Present (Expression (Decl))
                         or else No_Initialization (Decl))
            then
               return;
            end if;
         end;

         --  Here is where we give the warning

         --  All OK if warnings suppressed on the entity

         if not Has_Warnings_Off (Ent) then
            Error_Msg_Sloc := Sloc (Ent);

            Error_Msg_NE
              ("??& can be accessed by clients before this initialization",
               N, Ent);
            Error_Msg_NE
              ("\??add Elaborate_Body to spec to ensure & is initialized",
               N, Ent);
         end if;

         if not All_Errors_Mode then
            Set_Suppress_Elaboration_Warnings (Ent);
         end if;
      end if;
   end Check_Elab_Assign;

   ----------------------
   -- Check_Elab_Calls --
   ----------------------

   procedure Check_Elab_Calls is
   begin
      --  If expansion is disabled, do not generate any checks. Also skip
      --  checks if any subunits are missing because in either case we lack the
      --  full information that we need, and no object file will be created in
      --  any case.

      if not Expander_Active
        or else Is_Generic_Unit (Cunit_Entity (Main_Unit))
        or else Subunits_Missing
      then
         return;
      end if;

      --  Skip delayed calls if we had any errors

      if Serious_Errors_Detected = 0 then
         Delaying_Elab_Checks := False;
         Expander_Mode_Save_And_Set (True);

         for J in Delay_Check.First .. Delay_Check.Last loop
            Push_Scope (Delay_Check.Table (J).Curscop);
            From_Elab_Code := Delay_Check.Table (J).From_Elab_Code;

            Check_Internal_Call_Continue (
              N           => Delay_Check.Table (J).N,
              E           => Delay_Check.Table (J).E,
              Outer_Scope => Delay_Check.Table (J).Outer_Scope,
              Orig_Ent    => Delay_Check.Table (J).Orig_Ent);

            Pop_Scope;
         end loop;

         --  Set Delaying_Elab_Checks back on for next main compilation

         Expander_Mode_Restore;
         Delaying_Elab_Checks := True;
      end if;
   end Check_Elab_Calls;

   ------------------------------
   -- Check_Elab_Instantiation --
   ------------------------------

   procedure Check_Elab_Instantiation
     (N           : Node_Id;
      Outer_Scope : Entity_Id := Empty)
   is
      Ent : Entity_Id;

   begin
      --  Check for and deal with bad instantiation case. There is some
      --  duplicated code here, but we will worry about this later ???

      Check_Bad_Instantiation (N);

      if ABE_Is_Certain (N) then
         return;
      end if;

      --  Nothing to do if we do not have an instantiation (happens in some
      --  error cases, and also in the formal package declaration case)

      if Nkind (N) not in N_Generic_Instantiation then
         return;
      end if;

      --  Nothing to do if inside a generic template

      if Inside_A_Generic then
         return;
      end if;

      --  Nothing to do if the instantiation is not in the main unit

      if not In_Extended_Main_Code_Unit (N) then
         return;
      end if;

      Ent := Get_Generic_Entity (N);
      From_Elab_Code := not In_Subprogram_Or_Concurrent_Unit;

      --  See if we need to analyze this instantiation. We analyze it if
      --  either of the following conditions is met:

      --    It is an inner level instantiation (since in this case it was
      --    triggered by an outer level call from elaboration code), but
      --    only if the instantiation is within the scope of the original
      --    outer level call.

      --    It is an outer level instantiation from elaboration code, or the
      --    instantiated entity is in the same elaboration scope.

      --  And in these cases, we will check both the inter-unit case and
      --  the intra-unit (within a single unit) case.

      C_Scope := Current_Scope;

      if Present (Outer_Scope) and then Within (Scope (Ent), Outer_Scope) then
         Set_C_Scope;
         Check_A_Call (N, Ent, Outer_Scope, Inter_Unit_Only => False);

      elsif From_Elab_Code then
         Set_C_Scope;
         Check_A_Call (N, Ent, Standard_Standard, Inter_Unit_Only => False);

      elsif Same_Elaboration_Scope (C_Scope, Scope (Ent)) then
         Set_C_Scope;
         Check_A_Call (N, Ent, Scope (Ent), Inter_Unit_Only => False);

      --  If none of those cases holds, but Dynamic_Elaboration_Checks mode is
      --  set, then we will do the check, but only in the inter-unit case (this
      --  is to accommodate unguarded elaboration calls from other units in
      --  which this same mode is set). We inhibit warnings in this case, since
      --  this instantiation is not occurring in elaboration code.

      elsif Dynamic_Elaboration_Checks then
         Set_C_Scope;
         Check_A_Call
           (N,
            Ent,
            Standard_Standard,
            Inter_Unit_Only => True,
            Generate_Warnings => False);

      else
         return;
      end if;
   end Check_Elab_Instantiation;

   -------------------------
   -- Check_Internal_Call --
   -------------------------

   procedure Check_Internal_Call
     (N           : Node_Id;
      E           : Entity_Id;
      Outer_Scope : Entity_Id;
      Orig_Ent    : Entity_Id)
   is
      Inst_Case : constant Boolean := Nkind (N) in N_Generic_Instantiation;

   begin
      --  For P'Access, we want to warn if the -gnatw.f switch is set, and the
      --  node comes from source.

      if Nkind (N) = N_Attribute_Reference and then
        (not Warn_On_Elab_Access or else not Comes_From_Source (N))
      then
         return;

      --  If not function or procedure call, instantiation, or 'Access, then
      --  ignore call (this happens in some error cases and rewriting cases).

      elsif not Nkind_In
               (N, N_Function_Call,
                   N_Procedure_Call_Statement,
                   N_Attribute_Reference)
        and then not Inst_Case
      then
         return;

      --  Nothing to do if this is a call or instantiation that has already
      --  been found to be a sure ABE.

      elsif Nkind (N) /= N_Attribute_Reference and then ABE_Is_Certain (N) then
         return;

      --  Nothing to do if errors already detected (avoid cascaded errors)

      elsif Serious_Errors_Detected /= 0 then
         return;

      --  Nothing to do if not in full analysis mode

      elsif not Full_Analysis then
         return;

      --  Nothing to do if analyzing in special spec-expression mode, since the
      --  call is not actually being made at this time.

      elsif In_Spec_Expression then
         return;

      --  Nothing to do for call to intrinsic subprogram

      elsif Is_Intrinsic_Subprogram (E) then
         return;

      --  No need to trace local calls if checking task activation, because
      --  other local bodies are elaborated already.

      elsif In_Task_Activation then
         return;

      --  Nothing to do if call is within a generic unit

      elsif Inside_A_Generic then
         return;
      end if;

      --  Delay this call if we are still delaying calls

      if Delaying_Elab_Checks then
         Delay_Check.Append (
           (N              => N,
            E              => E,
            Orig_Ent       => Orig_Ent,
            Curscop        => Current_Scope,
            Outer_Scope    => Outer_Scope,
            From_Elab_Code => From_Elab_Code));
         return;

      --  Otherwise, call phase 2 continuation right now

      else
         Check_Internal_Call_Continue (N, E, Outer_Scope, Orig_Ent);
      end if;
   end Check_Internal_Call;

   ----------------------------------
   -- Check_Internal_Call_Continue --
   ----------------------------------

   procedure Check_Internal_Call_Continue
     (N           : Node_Id;
      E           : Entity_Id;
      Outer_Scope : Entity_Id;
      Orig_Ent    : Entity_Id)
   is
      function Find_Elab_Reference (N : Node_Id) return Traverse_Result;
      --  Function applied to each node as we traverse the body. Checks for
      --  call or entity reference that needs checking, and if so checks it.
      --  Always returns OK, so entire tree is traversed, except that as
      --  described below subprogram bodies are skipped for now.

      procedure Traverse is new Atree.Traverse_Proc (Find_Elab_Reference);
      --  Traverse procedure using above Find_Elab_Reference function

      -------------------------
      -- Find_Elab_Reference --
      -------------------------

      function Find_Elab_Reference (N : Node_Id) return Traverse_Result is
         Actual : Node_Id;

      begin
         --  If user has specified that there are no entry calls in elaboration
         --  code, do not trace past an accept statement, because the rendez-
         --  vous will happen after elaboration.

         if Nkind_In (Original_Node (N), N_Accept_Statement,
                                         N_Selective_Accept)
           and then Restriction_Active (No_Entry_Calls_In_Elaboration_Code)
         then
            return Abandon;

         --  If we have a function call, check it

         elsif Nkind (N) = N_Function_Call then
            Check_Elab_Call (N, Outer_Scope);
            return OK;

         --  If we have a procedure call, check the call, and also check
         --  arguments that are assignments (OUT or IN OUT mode formals).

         elsif Nkind (N) = N_Procedure_Call_Statement then
            Check_Elab_Call (N, Outer_Scope, In_Init_Proc => Is_Init_Proc (E));

            Actual := First_Actual (N);
            while Present (Actual) loop
               if Known_To_Be_Assigned (Actual) then
                  Check_Elab_Assign (Actual);
               end if;

               Next_Actual (Actual);
            end loop;

            return OK;

         --  If we have an access attribute for a subprogram, check it.
         --  Suppress this behavior under debug flag.

         elsif not Debug_Flag_Dot_UU
           and then Nkind (N) = N_Attribute_Reference
           and then Nam_In (Attribute_Name (N), Name_Access,
                                                Name_Unrestricted_Access)
           and then Is_Entity_Name (Prefix (N))
           and then Is_Subprogram (Entity (Prefix (N)))
         then
            Check_Elab_Call (N, Outer_Scope);
            return OK;

         --  In SPARK mode, if we have an entity reference to a variable, then
         --  check it. For now we consider any reference.

         elsif SPARK_Mode = On
           and then Nkind (N) in N_Has_Entity
           and then Present (Entity (N))
           and then Ekind (Entity (N)) = E_Variable
         then
            Check_Elab_Call (N, Outer_Scope);
            return OK;

         --  If we have a generic instantiation, check it

         elsif Nkind (N) in N_Generic_Instantiation then
            Check_Elab_Instantiation (N, Outer_Scope);
            return OK;

         --  Skip subprogram bodies that come from source (wait for call to
         --  analyze these). The reason for the come from source test is to
         --  avoid catching task bodies.

         --  For task bodies, we should really avoid these too, waiting for the
         --  task activation, but that's too much trouble to catch for now, so
         --  we go in unconditionally. This is not so terrible, it means the
         --  error backtrace is not quite complete, and we are too eager to
         --  scan bodies of tasks that are unused, but this is hardly very
         --  significant.

         elsif Nkind (N) = N_Subprogram_Body
           and then Comes_From_Source (N)
         then
            return Skip;

         elsif Nkind (N) = N_Assignment_Statement
           and then Comes_From_Source (N)
         then
            Check_Elab_Assign (Name (N));
            return OK;

         else
            return OK;
         end if;
      end Find_Elab_Reference;

      Inst_Case : constant Boolean    := Is_Generic_Unit (E);
      Loc       : constant Source_Ptr := Sloc (N);

      Ebody : Entity_Id;
      Sbody : Node_Id;

   --  Start of processing for Check_Internal_Call_Continue

   begin
      --  Save outer level call if at outer level

      if Elab_Call.Last = 0 then
         Outer_Level_Sloc := Loc;
      end if;

      Elab_Visited.Append (E);

      --  If the call is to a function that renames a literal, no check needed

      if Ekind (E) = E_Enumeration_Literal then
         return;
      end if;

      Sbody := Unit_Declaration_Node (E);

      if not Nkind_In (Sbody, N_Subprogram_Body, N_Package_Body) then
         Ebody := Corresponding_Body (Sbody);

         if No (Ebody) then
            return;
         else
            Sbody := Unit_Declaration_Node (Ebody);
         end if;
      end if;

      --  If the body appears after the outer level call or instantiation then
      --  we have an error case handled below.

      if Earlier_In_Extended_Unit (Outer_Level_Sloc, Sloc (Sbody))
        and then not In_Task_Activation
      then
         null;

      --  If we have the instantiation case we are done, since we now
      --  know that the body of the generic appeared earlier.

      elsif Inst_Case then
         return;

      --  Otherwise we have a call, so we trace through the called body to see
      --  if it has any problems.

      else
         pragma Assert (Nkind (Sbody) = N_Subprogram_Body);

         Elab_Call.Append ((Cloc => Loc, Ent => E));

         if Debug_Flag_LL then
            Write_Str ("Elab_Call.Last = ");
            Write_Int (Int (Elab_Call.Last));
            Write_Str ("   Ent = ");
            Write_Name (Chars (E));
            Write_Str ("   at ");
            Write_Location (Sloc (N));
            Write_Eol;
         end if;

         --  Now traverse declarations and statements of subprogram body. Note
         --  that we cannot simply Traverse (Sbody), since traverse does not
         --  normally visit subprogram bodies.

         declare
            Decl : Node_Id;
         begin
            Decl := First (Declarations (Sbody));
            while Present (Decl) loop
               Traverse (Decl);
               Next (Decl);
            end loop;
         end;

         Traverse (Handled_Statement_Sequence (Sbody));

         Elab_Call.Decrement_Last;
         return;
      end if;

      --  Here is the case of calling a subprogram where the body has not yet
      --  been encountered. A warning message is needed, except if this is the
      --  case of appearing within an aspect specification that results in
      --  a check call, we do not really have such a situation, so no warning
      --  is needed (e.g. the case of a precondition, where the call appears
      --  textually before the body, but in actual fact is moved to the
      --  appropriate subprogram body and so does not need a check).

      declare
         P : Node_Id;
         O : Node_Id;

      begin
         P := Parent (N);
         loop
            --  Keep looking at parents if we are still in the subexpression

            if Nkind (P) in N_Subexpr then
               P := Parent (P);

            --  Here P is the parent of the expression, check for special case

            else
               O := Original_Node (P);

               --  Definitely not the special case if orig node is not a pragma

               exit when Nkind (O) /= N_Pragma;

               --  Check we have an If statement or a null statement (happens
               --  when the If has been expanded to be True).

               exit when not Nkind_In (P, N_If_Statement, N_Null_Statement);

               --  Our special case will be indicated either by the pragma
               --  coming from an aspect ...

               if Present (Corresponding_Aspect (O)) then
                  return;

               --  Or, in the case of an initial condition, specifically by a
               --  Check pragma specifying an Initial_Condition check.

               elsif Pragma_Name (O) = Name_Check
                 and then
                   Chars
                     (Expression (First (Pragma_Argument_Associations (O)))) =
                                                       Name_Initial_Condition
               then
                  return;

               --  For anything else, we have an error

               else
                  exit;
               end if;
            end if;
         end loop;
      end;

      --  Not that special case, warning and dynamic check is required

      --  If we have nothing in the call stack, then this is at the outer
      --  level, and the ABE is bound to occur, unless it's a 'Access, or
      --  it's a renaming.

      if Elab_Call.Last = 0 then
         Error_Msg_Warn := SPARK_Mode /= On;

         declare
            Insert_Check : Boolean := True;
            --  This flag is set to True if an elaboration check should be
            --  inserted.

         begin
            if Inst_Case then
               Error_Msg_NE
                 ("cannot instantiate& before body seen<<", N, Orig_Ent);

            elsif Nkind (N) = N_Attribute_Reference then
               Error_Msg_NE
                 ("Access attribute of & before body seen<<", N, Orig_Ent);
               Error_Msg_N ("\possible Program_Error on later references<", N);
               Insert_Check := False;

            elsif Nkind (Unit_Declaration_Node (Orig_Ent)) /=
                    N_Subprogram_Renaming_Declaration
            then
               Error_Msg_NE
                 ("cannot call& before body seen<<", N, Orig_Ent);

            elsif not Is_Generic_Actual_Subprogram (Orig_Ent) then
               Insert_Check := False;
            end if;

            if Insert_Check then
               Error_Msg_N ("\Program_Error [<<", N);
               Insert_Elab_Check (N);
            end if;
         end;

      --  Call is not at outer level

      else
         --  Deal with dynamic elaboration check

         if not Elaboration_Checks_Suppressed (E) then
            Set_Elaboration_Entity_Required (E);

            --  Case of no elaboration entity allocated yet

            if No (Elaboration_Entity (E)) then

               --  Create object declaration for elaboration entity, and put it
               --  just in front of the spec of the subprogram or generic unit,
               --  in the same scope as this unit. The subprogram may be over-
               --  loaded, so make the name of elaboration entity unique by
               --  means of a numeric suffix.

               declare
                  Loce : constant Source_Ptr := Sloc (E);
                  Ent  : constant Entity_Id  :=
                           Make_Defining_Identifier (Loc,
                             Chars => New_External_Name (Chars (E), 'E', -1));

               begin
                  Set_Elaboration_Entity (E, Ent);
                  Push_Scope (Scope (E));

                  Insert_Action (Declaration_Node (E),
                    Make_Object_Declaration (Loce,
                      Defining_Identifier => Ent,
                      Object_Definition   =>
                        New_Occurrence_Of (Standard_Short_Integer, Loce),
                      Expression          =>
                        Make_Integer_Literal (Loc, Uint_0)));

                  --  Set elaboration flag at the point of the body

                  Set_Elaboration_Flag (Sbody, E);

                  --  Kill current value indication. This is necessary because
                  --  the tests of this flag are inserted out of sequence and
                  --  must not pick up bogus indications of the wrong constant
                  --  value. Also, this is never a true constant, since one way
                  --  or another, it gets reset.

                  Set_Current_Value    (Ent, Empty);
                  Set_Last_Assignment  (Ent, Empty);
                  Set_Is_True_Constant (Ent, False);
                  Pop_Scope;
               end;
            end if;

            --  Generate check of the elaboration counter

            Insert_Elab_Check (N,
               Make_Attribute_Reference (Loc,
                 Attribute_Name => Name_Elaborated,
                 Prefix         => New_Occurrence_Of (E, Loc)));
         end if;

         --  Generate the warning

         if not Suppress_Elaboration_Warnings (E)
           and then not Elaboration_Checks_Suppressed (E)

           --  Suppress this warning if we have a function call that occurred
           --  within an assertion expression, since we can get false warnings
           --  in this case, due to the out of order handling in this case.

           and then
             (Nkind (Original_Node (N)) /= N_Function_Call
               or else not In_Assertion_Expression_Pragma (Original_Node (N)))
         then
            Error_Msg_Warn := SPARK_Mode /= On;

            if Inst_Case then
               Error_Msg_NE
                 ("instantiation of& may occur before body is seen<l<",
                  N, Orig_Ent);
            else
               --  A rather specific check. For Finalize/Adjust/Initialize,
               --  if the type has Warnings_Off set, suppress the warning.

               if Nam_In (Chars (E), Name_Adjust,
                                     Name_Finalize,
                                     Name_Initialize)
                 and then Present (First_Formal (E))
               then
                  declare
                     T : constant Entity_Id := Etype (First_Formal (E));
                  begin
                     if Is_Controlled (T) then
                        if Warnings_Off (T)
                          or else (Ekind (T) = E_Private_Type
                                    and then Warnings_Off (Full_View (T)))
                        then
                           goto Output;
                        end if;
                     end if;
                  end;
               end if;

               --  Go ahead and give warning if not this special case

               Error_Msg_NE
                 ("call to& may occur before body is seen<l<", N, Orig_Ent);
            end if;

            Error_Msg_N ("\Program_Error ]<l<", N);

            --  There is no need to query the elaboration warning message flags
            --  because the main message is an error, not a warning, therefore
            --  all the clarification messages produces by Output_Calls must be
            --  emitted unconditionally.

            <<Output>>

            Output_Calls (N, Check_Elab_Flag => False);
         end if;
      end if;

      --  Set flag to suppress further warnings on same subprogram
      --  unless in all errors mode

      if not All_Errors_Mode then
         Set_Suppress_Elaboration_Warnings (E);
      end if;
   end Check_Internal_Call_Continue;

   ---------------------------
   -- Check_Task_Activation --
   ---------------------------

   procedure Check_Task_Activation (N : Node_Id) is
      Loc         : constant Source_Ptr := Sloc (N);
      Inter_Procs : constant Elist_Id   := New_Elmt_List;
      Intra_Procs : constant Elist_Id   := New_Elmt_List;
      Ent         : Entity_Id;
      P           : Entity_Id;
      Task_Scope  : Entity_Id;
      Cunit_SC    : Boolean := False;
      Decl        : Node_Id;
      Elmt        : Elmt_Id;
      Enclosing   : Entity_Id;

      procedure Add_Task_Proc (Typ : Entity_Id);
      --  Add to Task_Procs the task body procedure(s) of task types in Typ.
      --  For record types, this procedure recurses over component types.

      procedure Collect_Tasks (Decls : List_Id);
      --  Collect the types of the tasks that are to be activated in the given
      --  list of declarations, in order to perform elaboration checks on the
      --  corresponding task procedures which are called implicitly here.

      function Outer_Unit (E : Entity_Id) return Entity_Id;
      --  find enclosing compilation unit of Entity, ignoring subunits, or
      --  else enclosing subprogram. If E is not a package, there is no need
      --  for inter-unit elaboration checks.

      -------------------
      -- Add_Task_Proc --
      -------------------

      procedure Add_Task_Proc (Typ : Entity_Id) is
         Comp : Entity_Id;
         Proc : Entity_Id := Empty;

      begin
         if Is_Task_Type (Typ) then
            Proc := Get_Task_Body_Procedure (Typ);

         elsif Is_Array_Type (Typ)
           and then Has_Task (Base_Type (Typ))
         then
            Add_Task_Proc (Component_Type (Typ));

         elsif Is_Record_Type (Typ)
           and then Has_Task (Base_Type (Typ))
         then
            Comp := First_Component (Typ);
            while Present (Comp) loop
               Add_Task_Proc (Etype (Comp));
               Comp := Next_Component (Comp);
            end loop;
         end if;

         --  If the task type is another unit, we will perform the usual
         --  elaboration check on its enclosing unit. If the type is in the
         --  same unit, we can trace the task body as for an internal call,
         --  but we only need to examine other external calls, because at
         --  the point the task is activated, internal subprogram bodies
         --  will have been elaborated already. We keep separate lists for
         --  each kind of task.

         --  Skip this test if errors have occurred, since in this case
         --  we can get false indications.

         if Serious_Errors_Detected /= 0 then
            return;
         end if;

         if Present (Proc) then
            if Outer_Unit (Scope (Proc)) = Enclosing then

               if No (Corresponding_Body (Unit_Declaration_Node (Proc)))
                 and then
                   (not Is_Generic_Instance (Scope (Proc))
                     or else Scope (Proc) = Scope (Defining_Identifier (Decl)))
               then
                  Error_Msg_Warn := SPARK_Mode /= On;
                  Error_Msg_N
                    ("task will be activated before elaboration of its body<<",
                      Decl);
                  Error_Msg_N ("\Program_Error [<<", Decl);

               elsif Present
                       (Corresponding_Body (Unit_Declaration_Node (Proc)))
               then
                  Append_Elmt (Proc, Intra_Procs);
               end if;

            else
               --  No need for multiple entries of the same type

               Elmt := First_Elmt (Inter_Procs);
               while Present (Elmt) loop
                  if Node (Elmt) = Proc then
                     return;
                  end if;

                  Next_Elmt (Elmt);
               end loop;

               Append_Elmt (Proc, Inter_Procs);
            end if;
         end if;
      end Add_Task_Proc;

      -------------------
      -- Collect_Tasks --
      -------------------

      procedure Collect_Tasks (Decls : List_Id) is
      begin
         if Present (Decls) then
            Decl := First (Decls);
            while Present (Decl) loop
               if Nkind (Decl) = N_Object_Declaration
                 and then Has_Task (Etype (Defining_Identifier (Decl)))
               then
                  Add_Task_Proc (Etype (Defining_Identifier (Decl)));
               end if;

               Next (Decl);
            end loop;
         end if;
      end Collect_Tasks;

      ----------------
      -- Outer_Unit --
      ----------------

      function Outer_Unit (E : Entity_Id) return Entity_Id is
         Outer : Entity_Id;

      begin
         Outer := E;
         while Present (Outer) loop
            if Elaboration_Checks_Suppressed (Outer) then
               Cunit_SC := True;
            end if;

            exit when Is_Child_Unit (Outer)
              or else Scope (Outer) = Standard_Standard
              or else Ekind (Outer) /= E_Package;
            Outer := Scope (Outer);
         end loop;

         return Outer;
      end Outer_Unit;

   --  Start of processing for Check_Task_Activation

   begin
      Enclosing := Outer_Unit (Current_Scope);

      --  Find all tasks declared in the current unit

      if Nkind (N) = N_Package_Body then
         P := Unit_Declaration_Node (Corresponding_Spec (N));

         Collect_Tasks (Declarations (N));
         Collect_Tasks (Visible_Declarations (Specification (P)));
         Collect_Tasks (Private_Declarations (Specification (P)));

      elsif Nkind (N) = N_Package_Declaration then
         Collect_Tasks (Visible_Declarations (Specification (N)));
         Collect_Tasks (Private_Declarations (Specification (N)));

      else
         Collect_Tasks (Declarations (N));
      end if;

      --  We only perform detailed checks in all tasks that are library level
      --  entities. If the master is a subprogram or task, activation will
      --  depend on the activation of the master itself.

      --  Should dynamic checks be added in the more general case???

      if Ekind (Enclosing) /= E_Package then
         return;
      end if;

      --  For task types defined in other units, we want the unit containing
      --  the task body to be elaborated before the current one.

      Elmt := First_Elmt (Inter_Procs);
      while Present (Elmt) loop
         Ent := Node (Elmt);
         Task_Scope := Outer_Unit (Scope (Ent));

         if not Is_Compilation_Unit (Task_Scope) then
            null;

         elsif Suppress_Elaboration_Warnings (Task_Scope)
           or else Elaboration_Checks_Suppressed (Task_Scope)
         then
            null;

         elsif Dynamic_Elaboration_Checks then
            if not Elaboration_Checks_Suppressed (Ent)
              and then not Cunit_SC
              and then
                not Restriction_Active (No_Entry_Calls_In_Elaboration_Code)
            then
               --  Runtime elaboration check required. Generate check of the
               --  elaboration counter for the unit containing the entity.

               Insert_Elab_Check (N,
                 Make_Attribute_Reference (Loc,
                   Attribute_Name => Name_Elaborated,
                   Prefix =>
                     New_Occurrence_Of (Spec_Entity (Task_Scope), Loc)));
            end if;

         else
            --  Force the binder to elaborate other unit first

            if not Suppress_Elaboration_Warnings (Ent)
              and then not Elaboration_Checks_Suppressed (Ent)
              and then Elab_Info_Messages
              and then not Suppress_Elaboration_Warnings (Task_Scope)
              and then not Elaboration_Checks_Suppressed (Task_Scope)
            then
               Error_Msg_Node_2 := Task_Scope;
               Error_Msg_NE
                 ("info: activation of an instance of task type&" &
                  " requires pragma Elaborate_All on &?$?", N, Ent);
            end if;

            Activate_Elaborate_All_Desirable (N, Task_Scope);
            Set_Suppress_Elaboration_Warnings (Task_Scope);
         end if;

         Next_Elmt (Elmt);
      end loop;

      --  For tasks declared in the current unit, trace other calls within
      --  the task procedure bodies, which are available.

      In_Task_Activation := True;

      Elmt := First_Elmt (Intra_Procs);
      while Present (Elmt) loop
         Ent := Node (Elmt);
         Check_Internal_Call_Continue (N, Ent, Enclosing, Ent);
         Next_Elmt (Elmt);
      end loop;

      In_Task_Activation := False;
   end Check_Task_Activation;

   -------------------------------
   -- Is_Call_Of_Generic_Formal --
   -------------------------------

   function Is_Call_Of_Generic_Formal (N : Node_Id) return Boolean is
   begin
      return Nkind_In (N, N_Function_Call, N_Procedure_Call_Statement)

        --  Always return False if debug flag -gnatd.G is set

        and then not Debug_Flag_Dot_GG

      --  For now, we detect this by looking for the strange identifier
      --  node, whose Chars reflect the name of the generic formal, but
      --  the Chars of the Entity references the generic actual.

        and then Nkind (Name (N)) = N_Identifier
        and then Chars (Name (N)) /= Chars (Entity (Name (N)));
   end Is_Call_Of_Generic_Formal;

   --------------------------------
   -- Set_Elaboration_Constraint --
   --------------------------------

   procedure Set_Elaboration_Constraint
    (Call : Node_Id;
     Subp : Entity_Id;
     Scop : Entity_Id)
   is
      Elab_Unit  : Entity_Id;

      --  Check whether this is a call to an Initialize subprogram for a
      --  controlled type. Note that Call can also be a 'Access attribute
      --  reference, which now generates an elaboration check.

      Init_Call  : constant Boolean :=
                     Nkind (Call) = N_Procedure_Call_Statement
                       and then Chars (Subp) = Name_Initialize
                       and then Comes_From_Source (Subp)
                       and then Present (Parameter_Associations (Call))
                       and then Is_Controlled (Etype (First_Actual (Call)));
   begin
      --  If the unit is mentioned in a with_clause of the current unit, it is
      --  visible, and we can set the elaboration flag.

      if Is_Immediately_Visible (Scop)
        or else (Is_Child_Unit (Scop) and then Is_Visible_Lib_Unit (Scop))
      then
         Activate_Elaborate_All_Desirable (Call, Scop);
         Set_Suppress_Elaboration_Warnings (Scop, True);
         return;
      end if;

      --  If this is not an initialization call or a call using object notation
      --  we know that the unit of the called entity is in the context, and
      --  we can set the flag as well. The unit need not be visible if the call
      --  occurs within an instantiation.

      if Is_Init_Proc (Subp)
        or else Init_Call
        or else Nkind (Original_Node (Call)) = N_Selected_Component
      then
         null;  --  detailed processing follows.

      else
         Activate_Elaborate_All_Desirable (Call, Scop);
         Set_Suppress_Elaboration_Warnings (Scop, True);
         return;
      end if;

      --  If the unit is not in the context, there must be an intermediate unit
      --  that is, on which we need to place to elaboration flag. This happens
      --  with init proc calls.

      if Is_Init_Proc (Subp) or else Init_Call then

         --  The initialization call is on an object whose type is not declared
         --  in the same scope as the subprogram. The type of the object must
         --  be a subtype of the type of operation. This object is the first
         --  actual in the call.

         declare
            Typ : constant Entity_Id :=
                    Etype (First (Parameter_Associations (Call)));
         begin
            Elab_Unit := Scope (Typ);
            while (Present (Elab_Unit))
              and then not Is_Compilation_Unit (Elab_Unit)
            loop
               Elab_Unit := Scope (Elab_Unit);
            end loop;
         end;

      --  If original node uses selected component notation, the prefix is
      --  visible and determines the scope that must be elaborated. After
      --  rewriting, the prefix is the first actual in the call.

      elsif Nkind (Original_Node (Call)) = N_Selected_Component then
         Elab_Unit := Scope (Etype (First (Parameter_Associations (Call))));

      --  Not one of special cases above

      else
         --  Using previously computed scope. If the elaboration check is
         --  done after analysis, the scope is not visible any longer, but
         --  must still be in the context.

         Elab_Unit := Scop;
      end if;

      Activate_Elaborate_All_Desirable (Call, Elab_Unit);
      Set_Suppress_Elaboration_Warnings (Elab_Unit, True);
   end Set_Elaboration_Constraint;

   ------------------------
   -- Get_Referenced_Ent --
   ------------------------

   function Get_Referenced_Ent (N : Node_Id) return Entity_Id is
      Nam : Node_Id;

   begin
      if Nkind (N) in N_Has_Entity
        and then Present (Entity (N))
        and then Ekind (Entity (N)) = E_Variable
      then
         return Entity (N);
      end if;

      if Nkind (N) = N_Attribute_Reference then
         Nam := Prefix (N);
      else
         Nam := Name (N);
      end if;

      if No (Nam) then
         return Empty;
      elsif Nkind (Nam) = N_Selected_Component then
         return Entity (Selector_Name (Nam));
      elsif not Is_Entity_Name (Nam) then
         return Empty;
      else
         return Entity (Nam);
      end if;
   end Get_Referenced_Ent;

   ----------------------
   -- Has_Generic_Body --
   ----------------------

   function Has_Generic_Body (N : Node_Id) return Boolean is
      Ent  : constant Entity_Id := Get_Generic_Entity (N);
      Decl : constant Node_Id   := Unit_Declaration_Node (Ent);
      Scop : Entity_Id;

      function Find_Body_In (E : Entity_Id; N : Node_Id) return Node_Id;
      --  Determine if the list of nodes headed by N and linked by Next
      --  contains a package body for the package spec entity E, and if so
      --  return the package body. If not, then returns Empty.

      function Load_Package_Body (Nam : Unit_Name_Type) return Node_Id;
      --  This procedure is called load the unit whose name is given by Nam.
      --  This unit is being loaded to see whether it contains an optional
      --  generic body. The returned value is the loaded unit, which is always
      --  a package body (only package bodies can contain other entities in the
      --  sense in which Has_Generic_Body is interested). We only attempt to
      --  load bodies if we are generating code. If we are in semantics check
      --  only mode, then it would be wrong to load bodies that are not
      --  required from a semantic point of view, so in this case we return
      --  Empty. The result is that the caller may incorrectly decide that a
      --  generic spec does not have a body when in fact it does, but the only
      --  harm in this is that some warnings on elaboration problems may be
      --  lost in semantic checks only mode, which is not big loss. We also
      --  return Empty if we go for a body and it is not there.

      function Locate_Corresponding_Body (PE : Entity_Id) return Node_Id;
      --  PE is the entity for a package spec. This function locates the
      --  corresponding package body, returning Empty if none is found. The
      --  package body returned is fully parsed but may not yet be analyzed,
      --  so only syntactic fields should be referenced.

      ------------------
      -- Find_Body_In --
      ------------------

      function Find_Body_In (E : Entity_Id; N : Node_Id) return Node_Id is
         Nod : Node_Id;

      begin
         Nod := N;
         while Present (Nod) loop

            --  If we found the package body we are looking for, return it

            if Nkind (Nod) = N_Package_Body
              and then Chars (Defining_Unit_Name (Nod)) = Chars (E)
            then
               return Nod;

            --  If we found the stub for the body, go after the subunit,
            --  loading it if necessary.

            elsif Nkind (Nod) = N_Package_Body_Stub
              and then Chars (Defining_Identifier (Nod)) = Chars (E)
            then
               if Present (Library_Unit (Nod)) then
                  return Unit (Library_Unit (Nod));

               else
                  return Load_Package_Body (Get_Unit_Name (Nod));
               end if;

            --  If neither package body nor stub, keep looking on chain

            else
               Next (Nod);
            end if;
         end loop;

         return Empty;
      end Find_Body_In;

      -----------------------
      -- Load_Package_Body --
      -----------------------

      function Load_Package_Body (Nam : Unit_Name_Type) return Node_Id is
         U : Unit_Number_Type;

      begin
         if Operating_Mode /= Generate_Code then
            return Empty;
         else
            U :=
              Load_Unit
                (Load_Name  => Nam,
                 Required   => False,
                 Subunit    => False,
                 Error_Node => N);

            if U = No_Unit then
               return Empty;
            else
               return Unit (Cunit (U));
            end if;
         end if;
      end Load_Package_Body;

      -------------------------------
      -- Locate_Corresponding_Body --
      -------------------------------

      function Locate_Corresponding_Body (PE : Entity_Id) return Node_Id is
         Spec  : constant Node_Id   := Declaration_Node (PE);
         Decl  : constant Node_Id   := Parent (Spec);
         Scop  : constant Entity_Id := Scope (PE);
         PBody : Node_Id;

      begin
         if Is_Library_Level_Entity (PE) then

            --  If package is a library unit that requires a body, we have no
            --  choice but to go after that body because it might contain an
            --  optional body for the original generic package.

            if Unit_Requires_Body (PE) then

               --  Load the body. Note that we are a little careful here to use
               --  Spec to get the unit number, rather than PE or Decl, since
               --  in the case where the package is itself a library level
               --  instantiation, Spec will properly reference the generic
               --  template, which is what we really want.

               return
                 Load_Package_Body
                   (Get_Body_Name (Unit_Name (Get_Source_Unit (Spec))));

            --  But if the package is a library unit that does NOT require
            --  a body, then no body is permitted, so we are sure that there
            --  is no body for the original generic package.

            else
               return Empty;
            end if;

         --  Otherwise look and see if we are embedded in a further package

         elsif Is_Package_Or_Generic_Package (Scop) then

            --  If so, get the body of the enclosing package, and look in
            --  its package body for the package body we are looking for.

            PBody := Locate_Corresponding_Body (Scop);

            if No (PBody) then
               return Empty;
            else
               return Find_Body_In (PE, First (Declarations (PBody)));
            end if;

         --  If we are not embedded in a further package, then the body
         --  must be in the same declarative part as we are.

         else
            return Find_Body_In (PE, Next (Decl));
         end if;
      end Locate_Corresponding_Body;

   --  Start of processing for Has_Generic_Body

   begin
      if Present (Corresponding_Body (Decl)) then
         return True;

      elsif Unit_Requires_Body (Ent) then
         return True;

      --  Compilation units cannot have optional bodies

      elsif Is_Compilation_Unit (Ent) then
         return False;

      --  Otherwise look at what scope we are in

      else
         Scop := Scope (Ent);

         --  Case of entity is in other than a package spec, in this case
         --  the body, if present, must be in the same declarative part.

         if not Is_Package_Or_Generic_Package (Scop) then
            declare
               P : Node_Id;

            begin
               --  Declaration node may get us a spec, so if so, go to
               --  the parent declaration.

               P := Declaration_Node (Ent);
               while not Is_List_Member (P) loop
                  P := Parent (P);
               end loop;

               return Present (Find_Body_In (Ent, Next (P)));
            end;

         --  If the entity is in a package spec, then we have to locate
         --  the corresponding package body, and look there.

         else
            declare
               PBody : constant Node_Id := Locate_Corresponding_Body (Scop);

            begin
               if No (PBody) then
                  return False;
               else
                  return
                    Present
                      (Find_Body_In (Ent, (First (Declarations (PBody)))));
               end if;
            end;
         end if;
      end if;
   end Has_Generic_Body;

   -----------------------
   -- Insert_Elab_Check --
   -----------------------

   procedure Insert_Elab_Check (N : Node_Id; C : Node_Id := Empty) is
      Nod : Node_Id;
      Loc : constant Source_Ptr := Sloc (N);

      Chk : Node_Id;
      --  The check (N_Raise_Program_Error) node to be inserted

   begin
      --  If expansion is disabled, do not generate any checks. Also
      --  skip checks if any subunits are missing because in either
      --  case we lack the full information that we need, and no object
      --  file will be created in any case.

      if not Expander_Active or else Subunits_Missing then
         return;
      end if;

      --  If we have a generic instantiation, where Instance_Spec is set,
      --  then this field points to a generic instance spec that has
      --  been inserted before the instantiation node itself, so that
      --  is where we want to insert a check.

      if Nkind (N) in N_Generic_Instantiation
        and then Present (Instance_Spec (N))
      then
         Nod := Instance_Spec (N);
      else
         Nod := N;
      end if;

      --  Build check node, possibly with condition

      Chk :=
        Make_Raise_Program_Error (Loc, Reason => PE_Access_Before_Elaboration);

      if Present (C) then
         Set_Condition (Chk, Make_Op_Not (Loc, Right_Opnd => C));
      end if;

      --  If we are inserting at the top level, insert in Aux_Decls

      if Nkind (Parent (Nod)) = N_Compilation_Unit then
         declare
            ADN : constant Node_Id := Aux_Decls_Node (Parent (Nod));

         begin
            if No (Declarations (ADN)) then
               Set_Declarations (ADN, New_List (Chk));
            else
               Append_To (Declarations (ADN), Chk);
            end if;

            Analyze (Chk);
         end;

      --  Otherwise just insert as an action on the node in question

      else
         Insert_Action (Nod, Chk);
      end if;
   end Insert_Elab_Check;

   -------------------------------
   -- Is_Finalization_Procedure --
   -------------------------------

   function Is_Finalization_Procedure (Id : Entity_Id) return Boolean is
   begin
      --  Check whether Id is a procedure with at least one parameter

      if Ekind (Id) = E_Procedure and then Present (First_Formal (Id)) then
         declare
            Typ      : constant Entity_Id := Etype (First_Formal (Id));
            Deep_Fin : Entity_Id := Empty;
            Fin      : Entity_Id := Empty;

         begin
            --  If the type of the first formal does not require finalization
            --  actions, then this is definitely not [Deep_]Finalize.

            if not Needs_Finalization (Typ) then
               return False;
            end if;

            --  At this point we have the following scenario:

            --    procedure Name (Param1 : [in] [out] Ctrl[; Param2 : ...]);

            --  Recover the two possible versions of [Deep_]Finalize using the
            --  type of the first parameter and compare with the input.

            Deep_Fin := TSS (Typ, TSS_Deep_Finalize);

            if Is_Controlled (Typ) then
               Fin := Find_Prim_Op (Typ, Name_Finalize);
            end if;

            return    (Present (Deep_Fin) and then Id = Deep_Fin)
              or else (Present (Fin)      and then Id = Fin);
         end;
      end if;

      return False;
   end Is_Finalization_Procedure;

   ------------------
   -- Output_Calls --
   ------------------

   procedure Output_Calls
     (N               : Node_Id;
      Check_Elab_Flag : Boolean)
   is
      function Emit (Flag : Boolean) return Boolean;
      --  Determine whether to emit an error message based on the combination
      --  of flags Check_Elab_Flag and Flag.

      function Is_Printable_Error_Name return Boolean;
      --  An internal function, used to determine if a name, stored in the
      --  Name_Buffer, is either a non-internal name, or is an internal name
      --  that is printable by the error message circuits (i.e. it has a single
      --  upper case letter at the end).

      ----------
      -- Emit --
      ----------

      function Emit (Flag : Boolean) return Boolean is
      begin
         if Check_Elab_Flag then
            return Flag;
         else
            return True;
         end if;
      end Emit;

      -----------------------------
      -- Is_Printable_Error_Name --
      -----------------------------

      function Is_Printable_Error_Name return Boolean is
      begin
         if not Is_Internal_Name then
            return True;

         elsif Name_Len = 1 then
            return False;

         else
            Name_Len := Name_Len - 1;
            return not Is_Internal_Name;
         end if;
      end Is_Printable_Error_Name;

      --  Local variables

      Ent : Entity_Id;

   --  Start of processing for Output_Calls

   begin
      for J in reverse 1 .. Elab_Call.Last loop
         Error_Msg_Sloc := Elab_Call.Table (J).Cloc;

         Ent := Elab_Call.Table (J).Ent;
         Get_Name_String (Chars (Ent));

         --  Dynamic elaboration model, warnings controlled by -gnatwl

         if Dynamic_Elaboration_Checks then
            if Emit (Elab_Warnings) then
               if Is_Generic_Unit (Ent) then
                  Error_Msg_NE ("\\?l?& instantiated #", N, Ent);
               elsif Is_Init_Proc (Ent) then
                  Error_Msg_N ("\\?l?initialization procedure called #", N);
               elsif Is_Printable_Error_Name then
                  Error_Msg_NE ("\\?l?& called #", N, Ent);
               else
                  Error_Msg_N ("\\?l?called #", N);
               end if;
            end if;

         --  Static elaboration model, info messages controlled by -gnatel

         else
            if Emit (Elab_Info_Messages) then
               if Is_Generic_Unit (Ent) then
                  Error_Msg_NE ("\\?$?& instantiated #", N, Ent);
               elsif Is_Init_Proc (Ent) then
                  Error_Msg_N ("\\?$?initialization procedure called #", N);
               elsif Is_Printable_Error_Name then
                  Error_Msg_NE ("\\?$?& called #", N, Ent);
               else
                  Error_Msg_N ("\\?$?called #", N);
               end if;
            end if;
         end if;
      end loop;
   end Output_Calls;

   ----------------------------
   -- Same_Elaboration_Scope --
   ----------------------------

   function Same_Elaboration_Scope (Scop1, Scop2 : Entity_Id) return Boolean is
      S1 : Entity_Id;
      S2 : Entity_Id;

   begin
      --  Find elaboration scope for Scop1
      --  This is either a subprogram or a compilation unit.

      S1 := Scop1;
      while S1 /= Standard_Standard
        and then not Is_Compilation_Unit (S1)
        and then Ekind_In (S1, E_Package, E_Protected_Type, E_Block)
      loop
         S1 := Scope (S1);
      end loop;

      --  Find elaboration scope for Scop2

      S2 := Scop2;
      while S2 /= Standard_Standard
        and then not Is_Compilation_Unit (S2)
        and then Ekind_In (S2, E_Package, E_Protected_Type, E_Block)
      loop
         S2 := Scope (S2);
      end loop;

      return S1 = S2;
   end Same_Elaboration_Scope;

   -----------------
   -- Set_C_Scope --
   -----------------

   procedure Set_C_Scope is
   begin
      while not Is_Compilation_Unit (C_Scope) loop
         C_Scope := Scope (C_Scope);
      end loop;
   end Set_C_Scope;

   -----------------
   -- Spec_Entity --
   -----------------

   function Spec_Entity (E : Entity_Id) return Entity_Id is
      Decl : Node_Id;

   begin
      --  Check for case of body entity
      --  Why is the check for E_Void needed???

      if Ekind_In (E, E_Void, E_Subprogram_Body, E_Package_Body) then
         Decl := E;

         loop
            Decl := Parent (Decl);
            exit when Nkind (Decl) in N_Proper_Body;
         end loop;

         return Corresponding_Spec (Decl);

      else
         return E;
      end if;
   end Spec_Entity;

   -------------------
   -- Supply_Bodies --
   -------------------

   procedure Supply_Bodies (N : Node_Id) is
   begin
      if Nkind (N) = N_Subprogram_Declaration then
         declare
            Ent : constant Entity_Id := Defining_Unit_Name (Specification (N));

         begin
            --  Internal subprograms will already have a generated body, so
            --  there is no need to provide a stub for them.

            if No (Corresponding_Body (N)) then
               declare
                  Loc     : constant Source_Ptr := Sloc (N);
                  B       : Node_Id;
                  Formals : constant List_Id := Copy_Parameter_List (Ent);
                  Nam     : constant Entity_Id :=
                              Make_Defining_Identifier (Loc, Chars (Ent));
                  Spec    : Node_Id;
                  Stats   : constant List_Id :=
                              New_List
                               (Make_Raise_Program_Error (Loc,
                                  Reason => PE_Access_Before_Elaboration));

               begin
                  if Ekind (Ent) = E_Function then
                     Spec :=
                        Make_Function_Specification (Loc,
                          Defining_Unit_Name => Nam,
                          Parameter_Specifications => Formals,
                          Result_Definition =>
                            New_Copy_Tree
                              (Result_Definition (Specification (N))));

                     --  We cannot reliably make a return statement for this
                     --  body, but none is needed because the call raises
                     --  program error.

                     Set_Return_Present (Ent);

                  else
                     Spec :=
                        Make_Procedure_Specification (Loc,
                          Defining_Unit_Name => Nam,
                          Parameter_Specifications => Formals);
                  end if;

                  B := Make_Subprogram_Body (Loc,
                          Specification => Spec,
                          Declarations => New_List,
                          Handled_Statement_Sequence =>
                            Make_Handled_Sequence_Of_Statements (Loc,  Stats));
                  Insert_After (N, B);
                  Analyze (B);
               end;
            end if;
         end;

      elsif Nkind (N) = N_Package_Declaration then
         declare
            Spec : constant Node_Id := Specification (N);
         begin
            Push_Scope (Defining_Unit_Name (Spec));
            Supply_Bodies (Visible_Declarations (Spec));
            Supply_Bodies (Private_Declarations (Spec));
            Pop_Scope;
         end;
      end if;
   end Supply_Bodies;

   procedure Supply_Bodies (L : List_Id) is
      Elmt : Node_Id;
   begin
      if Present (L) then
         Elmt := First (L);
         while Present (Elmt) loop
            Supply_Bodies (Elmt);
            Next (Elmt);
         end loop;
      end if;
   end Supply_Bodies;

   ------------
   -- Within --
   ------------

   function Within (E1, E2 : Entity_Id) return Boolean is
      Scop : Entity_Id;
   begin
      Scop := E1;
      loop
         if Scop = E2 then
            return True;
         elsif Scop = Standard_Standard then
            return False;
         else
            Scop := Scope (Scop);
         end if;
      end loop;
   end Within;

   --------------------------
   -- Within_Elaborate_All --
   --------------------------

   function Within_Elaborate_All
     (Unit : Unit_Number_Type;
      E    : Entity_Id) return Boolean
   is
      type Unit_Number_Set is array (Main_Unit .. Last_Unit) of Boolean;
      pragma Pack (Unit_Number_Set);

      Seen : Unit_Number_Set := (others => False);
      --  Seen (X) is True after we have seen unit X in the walk. This is used
      --  to prevent processing the same unit more than once.

      Result : Boolean := False;

      procedure Helper (Unit : Unit_Number_Type);
      --  This helper procedure does all the work for Within_Elaborate_All. It
      --  walks the dependency graph, and sets Result to True if it finds an
      --  appropriate Elaborate_All.

      ------------
      -- Helper --
      ------------

      procedure Helper (Unit : Unit_Number_Type) is
         CU : constant Node_Id := Cunit (Unit);

         Item    : Node_Id;
         Item2   : Node_Id;
         Elab_Id : Entity_Id;
         Par     : Node_Id;

      begin
         if Seen (Unit) then
            return;
         else
            Seen (Unit) := True;
         end if;

         --  First, check for Elaborate_Alls on this unit

         Item := First (Context_Items (CU));
         while Present (Item) loop
            if Nkind (Item) = N_Pragma
              and then Pragma_Name (Item) = Name_Elaborate_All
            then
               --  Return if some previous error on the pragma itself. The
               --  pragma may be unanalyzed, because of a previous error, or
               --  if it is the context of a subunit, inherited by its parent.

               if Error_Posted (Item) or else not Analyzed (Item) then
                  return;
               end if;

               Elab_Id :=
                 Entity
                   (Expression (First (Pragma_Argument_Associations (Item))));

               if E = Elab_Id then
                  Result := True;
                  return;
               end if;

               Par := Parent (Unit_Declaration_Node (Elab_Id));

               Item2 := First (Context_Items (Par));
               while Present (Item2) loop
                  if Nkind (Item2) = N_With_Clause
                    and then Entity (Name (Item2)) = E
                    and then not Limited_Present (Item2)
                  then
                     Result := True;
                     return;
                  end if;

                  Next (Item2);
               end loop;
            end if;

            Next (Item);
         end loop;

         --  Second, recurse on with's. We could do this as part of the above
         --  loop, but it's probably more efficient to have two loops, because
         --  the relevant Elaborate_All is likely to be on the initial unit. In
         --  other words, we're walking the with's breadth-first. This part is
         --  only necessary in the dynamic elaboration model.

         if Dynamic_Elaboration_Checks then
            Item := First (Context_Items (CU));
            while Present (Item) loop
               if Nkind (Item) = N_With_Clause
                 and then not Limited_Present (Item)
               then
                  --  Note: the following call to Get_Cunit_Unit_Number does a
                  --  linear search, which could be slow, but it's OK because
                  --  we're about to give a warning anyway. Also, there might
                  --  be hundreds of units, but not millions. If it turns out
                  --  to be a problem, we could store the Get_Cunit_Unit_Number
                  --  in each N_Compilation_Unit node, but that would involve
                  --  rearranging N_Compilation_Unit_Aux to make room.

                  Helper (Get_Cunit_Unit_Number (Library_Unit (Item)));

                  if Result then
                     return;
                  end if;
               end if;

               Next (Item);
            end loop;
         end if;
      end Helper;

   --  Start of processing for Within_Elaborate_All

   begin
      Helper (Unit);
      return Result;
   end Within_Elaborate_All;

end Sem_Elab;
