------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 6                               --
--                                                                          --
--                                 B o d y                                  --
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

with Aspects;  use Aspects;
with Atree;    use Atree;
with Checks;   use Checks;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Elists;   use Elists;
with Exp_Aggr; use Exp_Aggr;
with Exp_Atag; use Exp_Atag;
with Exp_Ch2;  use Exp_Ch2;
with Exp_Ch3;  use Exp_Ch3;
with Exp_Ch7;  use Exp_Ch7;
with Exp_Ch9;  use Exp_Ch9;
with Exp_Dbug; use Exp_Dbug;
with Exp_Disp; use Exp_Disp;
with Exp_Dist; use Exp_Dist;
with Exp_Intr; use Exp_Intr;
with Exp_Pakd; use Exp_Pakd;
with Exp_Tss;  use Exp_Tss;
with Exp_Util; use Exp_Util;
with Exp_VFpt; use Exp_VFpt;
with Fname;    use Fname;
with Freeze;   use Freeze;
with Inline;   use Inline;
with Lib;      use Lib;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Output;   use Output;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch12; use Sem_Ch12;
with Sem_Ch13; use Sem_Ch13;
with Sem_Dim;  use Sem_Dim;
with Sem_Disp; use Sem_Disp;
with Sem_Dist; use Sem_Dist;
with Sem_Eval; use Sem_Eval;
with Sem_Mech; use Sem_Mech;
with Sem_Res;  use Sem_Res;
with Sem_SCIL; use Sem_SCIL;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Targparm; use Targparm;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;
with Validsw;  use Validsw;

package body Exp_Ch6 is

   Inlined_Calls : Elist_Id := No_Elist;
   Backend_Calls : Elist_Id := No_Elist;
   --  List of frontend inlined calls and inline calls passed to the backend

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Add_Access_Actual_To_Build_In_Place_Call
     (Function_Call : Node_Id;
      Function_Id   : Entity_Id;
      Return_Object : Node_Id;
      Is_Access     : Boolean := False);
   --  Ada 2005 (AI-318-02): Apply the Unrestricted_Access attribute to the
   --  object name given by Return_Object and add the attribute to the end of
   --  the actual parameter list associated with the build-in-place function
   --  call denoted by Function_Call. However, if Is_Access is True, then
   --  Return_Object is already an access expression, in which case it's passed
   --  along directly to the build-in-place function. Finally, if Return_Object
   --  is empty, then pass a null literal as the actual.

   procedure Add_Unconstrained_Actuals_To_Build_In_Place_Call
     (Function_Call  : Node_Id;
      Function_Id    : Entity_Id;
      Alloc_Form     : BIP_Allocation_Form := Unspecified;
      Alloc_Form_Exp : Node_Id             := Empty;
      Pool_Actual    : Node_Id             := Make_Null (No_Location));
   --  Ada 2005 (AI-318-02): Add the actuals needed for a build-in-place
   --  function call that returns a caller-unknown-size result (BIP_Alloc_Form
   --  and BIP_Storage_Pool). If Alloc_Form_Exp is present, then use it,
   --  otherwise pass a literal corresponding to the Alloc_Form parameter
   --  (which must not be Unspecified in that case). Pool_Actual is the
   --  parameter to pass to BIP_Storage_Pool.

   procedure Add_Finalization_Master_Actual_To_Build_In_Place_Call
     (Func_Call  : Node_Id;
      Func_Id    : Entity_Id;
      Ptr_Typ    : Entity_Id := Empty;
      Master_Exp : Node_Id   := Empty);
   --  Ada 2005 (AI-318-02): If the result type of a build-in-place call needs
   --  finalization actions, add an actual parameter which is a pointer to the
   --  finalization master of the caller. If Master_Exp is not Empty, then that
   --  will be passed as the actual. Otherwise, if Ptr_Typ is left Empty, this
   --  will result in an automatic "null" value for the actual.

   procedure Add_Task_Actuals_To_Build_In_Place_Call
     (Function_Call : Node_Id;
      Function_Id   : Entity_Id;
      Master_Actual : Node_Id);
   --  Ada 2005 (AI-318-02): For a build-in-place call, if the result type
   --  contains tasks, add two actual parameters: the master, and a pointer to
   --  the caller's activation chain. Master_Actual is the actual parameter
   --  expression to pass for the master. In most cases, this is the current
   --  master (_master). The two exceptions are: If the function call is the
   --  initialization expression for an allocator, we pass the master of the
   --  access type. If the function call is the initialization expression for a
   --  return object, we pass along the master passed in by the caller. The
   --  activation chain to pass is always the local one. Note: Master_Actual
   --  can be Empty, but only if there are no tasks.

   procedure Check_Overriding_Operation (Subp : Entity_Id);
   --  Subp is a dispatching operation. Check whether it may override an
   --  inherited private operation, in which case its DT entry is that of
   --  the hidden operation, not the one it may have received earlier.
   --  This must be done before emitting the code to set the corresponding
   --  DT to the address of the subprogram. The actual placement of Subp in
   --  the proper place in the list of primitive operations is done in
   --  Declare_Inherited_Private_Subprograms, which also has to deal with
   --  implicit operations. This duplication is unavoidable for now???

   procedure Detect_Infinite_Recursion (N : Node_Id; Spec : Entity_Id);
   --  This procedure is called only if the subprogram body N, whose spec
   --  has the given entity Spec, contains a parameterless recursive call.
   --  It attempts to generate runtime code to detect if this a case of
   --  infinite recursion.
   --
   --  The body is scanned to determine dependencies. If the only external
   --  dependencies are on a small set of scalar variables, then the values
   --  of these variables are captured on entry to the subprogram, and if
   --  the values are not changed for the call, we know immediately that
   --  we have an infinite recursion.

   procedure Expand_Ctrl_Function_Call (N : Node_Id);
   --  N is a function call which returns a controlled object. Transform the
   --  call into a temporary which retrieves the returned object from the
   --  secondary stack using 'reference.

   procedure Expand_Inlined_Call
    (N         : Node_Id;
     Subp      : Entity_Id;
     Orig_Subp : Entity_Id);
   --  If called subprogram can be inlined by the front-end, retrieve the
   --  analyzed body, replace formals with actuals and expand call in place.
   --  Generate thunks for actuals that are expressions, and insert the
   --  corresponding constant declarations before the call. If the original
   --  call is to a derived operation, the return type is the one of the
   --  derived operation, but the body is that of the original, so return
   --  expressions in the body must be converted to the desired type (which
   --  is simply not noted in the tree without inline expansion).

   procedure Expand_Non_Function_Return (N : Node_Id);
   --  Called by Expand_N_Simple_Return_Statement in case we're returning from
   --  a procedure body, entry body, accept statement, or extended return
   --  statement. Note that all non-function returns are simple return
   --  statements.

   function Expand_Protected_Object_Reference
     (N    : Node_Id;
      Scop : Entity_Id) return Node_Id;

   procedure Expand_Protected_Subprogram_Call
     (N    : Node_Id;
      Subp : Entity_Id;
      Scop : Entity_Id);
   --  A call to a protected subprogram within the protected object may appear
   --  as a regular call. The list of actuals must be expanded to contain a
   --  reference to the object itself, and the call becomes a call to the
   --  corresponding protected subprogram.

   function Has_Unconstrained_Access_Discriminants
     (Subtyp : Entity_Id) return Boolean;
   --  Returns True if the given subtype is unconstrained and has one
   --  or more access discriminants.

   procedure Expand_Simple_Function_Return (N : Node_Id);
   --  Expand simple return from function. In the case where we are returning
   --  from a function body this is called by Expand_N_Simple_Return_Statement.

   ----------------------------------------------
   -- Add_Access_Actual_To_Build_In_Place_Call --
   ----------------------------------------------

   procedure Add_Access_Actual_To_Build_In_Place_Call
     (Function_Call : Node_Id;
      Function_Id   : Entity_Id;
      Return_Object : Node_Id;
      Is_Access     : Boolean := False)
   is
      Loc            : constant Source_Ptr := Sloc (Function_Call);
      Obj_Address    : Node_Id;
      Obj_Acc_Formal : Entity_Id;

   begin
      --  Locate the implicit access parameter in the called function

      Obj_Acc_Formal := Build_In_Place_Formal (Function_Id, BIP_Object_Access);

      --  If no return object is provided, then pass null

      if not Present (Return_Object) then
         Obj_Address := Make_Null (Loc);
         Set_Parent (Obj_Address, Function_Call);

      --  If Return_Object is already an expression of an access type, then use
      --  it directly, since it must be an access value denoting the return
      --  object, and couldn't possibly be the return object itself.

      elsif Is_Access then
         Obj_Address := Return_Object;
         Set_Parent (Obj_Address, Function_Call);

      --  Apply Unrestricted_Access to caller's return object

      else
         Obj_Address :=
            Make_Attribute_Reference (Loc,
              Prefix         => Return_Object,
              Attribute_Name => Name_Unrestricted_Access);

         Set_Parent (Return_Object, Obj_Address);
         Set_Parent (Obj_Address, Function_Call);
      end if;

      Analyze_And_Resolve (Obj_Address, Etype (Obj_Acc_Formal));

      --  Build the parameter association for the new actual and add it to the
      --  end of the function's actuals.

      Add_Extra_Actual_To_Call (Function_Call, Obj_Acc_Formal, Obj_Address);
   end Add_Access_Actual_To_Build_In_Place_Call;

   ------------------------------------------------------
   -- Add_Unconstrained_Actuals_To_Build_In_Place_Call --
   ------------------------------------------------------

   procedure Add_Unconstrained_Actuals_To_Build_In_Place_Call
     (Function_Call  : Node_Id;
      Function_Id    : Entity_Id;
      Alloc_Form     : BIP_Allocation_Form := Unspecified;
      Alloc_Form_Exp : Node_Id             := Empty;
      Pool_Actual    : Node_Id             := Make_Null (No_Location))
   is
      Loc               : constant Source_Ptr := Sloc (Function_Call);
      Alloc_Form_Actual : Node_Id;
      Alloc_Form_Formal : Node_Id;
      Pool_Formal       : Node_Id;

   begin
      --  The allocation form generally doesn't need to be passed in the case
      --  of a constrained result subtype, since normally the caller performs
      --  the allocation in that case. However this formal is still needed in
      --  the case where the function has a tagged result, because generally
      --  such functions can be called in a dispatching context and such calls
      --  must be handled like calls to class-wide functions.

      if Is_Constrained (Underlying_Type (Etype (Function_Id)))
        and then not Is_Tagged_Type (Underlying_Type (Etype (Function_Id)))
      then
         return;
      end if;

      --  Locate the implicit allocation form parameter in the called function.
      --  Maybe it would be better for each implicit formal of a build-in-place
      --  function to have a flag or a Uint attribute to identify it. ???

      Alloc_Form_Formal := Build_In_Place_Formal (Function_Id, BIP_Alloc_Form);

      if Present (Alloc_Form_Exp) then
         pragma Assert (Alloc_Form = Unspecified);

         Alloc_Form_Actual := Alloc_Form_Exp;

      else
         pragma Assert (Alloc_Form /= Unspecified);

         Alloc_Form_Actual :=
           Make_Integer_Literal (Loc,
             Intval => UI_From_Int (BIP_Allocation_Form'Pos (Alloc_Form)));
      end if;

      Analyze_And_Resolve (Alloc_Form_Actual, Etype (Alloc_Form_Formal));

      --  Build the parameter association for the new actual and add it to the
      --  end of the function's actuals.

      Add_Extra_Actual_To_Call
        (Function_Call, Alloc_Form_Formal, Alloc_Form_Actual);

      --  Pass the Storage_Pool parameter. This parameter is omitted on
      --  .NET/JVM/ZFP as those targets do not support pools.

      if VM_Target = No_VM
        and then RTE_Available (RE_Root_Storage_Pool_Ptr)
      then
         Pool_Formal := Build_In_Place_Formal (Function_Id, BIP_Storage_Pool);
         Analyze_And_Resolve (Pool_Actual, Etype (Pool_Formal));
         Add_Extra_Actual_To_Call
           (Function_Call, Pool_Formal, Pool_Actual);
      end if;
   end Add_Unconstrained_Actuals_To_Build_In_Place_Call;

   -----------------------------------------------------------
   -- Add_Finalization_Master_Actual_To_Build_In_Place_Call --
   -----------------------------------------------------------

   procedure Add_Finalization_Master_Actual_To_Build_In_Place_Call
     (Func_Call  : Node_Id;
      Func_Id    : Entity_Id;
      Ptr_Typ    : Entity_Id := Empty;
      Master_Exp : Node_Id   := Empty)
   is
   begin
      if not Needs_BIP_Finalization_Master (Func_Id) then
         return;
      end if;

      declare
         Formal : constant Entity_Id :=
                    Build_In_Place_Formal (Func_Id, BIP_Finalization_Master);
         Loc    : constant Source_Ptr := Sloc (Func_Call);

         Actual    : Node_Id;
         Desig_Typ : Entity_Id;

      begin
         --  If there is a finalization master actual, such as the implicit
         --  finalization master of an enclosing build-in-place function,
         --  then this must be added as an extra actual of the call.

         if Present (Master_Exp) then
            Actual := Master_Exp;

         --  Case where the context does not require an actual master

         elsif No (Ptr_Typ) then
            Actual := Make_Null (Loc);

         else
            Desig_Typ := Directly_Designated_Type (Ptr_Typ);

            --  Check for a library-level access type whose designated type has
            --  supressed finalization. Such an access types lack a master.
            --  Pass a null actual to the callee in order to signal a missing
            --  master.

            if Is_Library_Level_Entity (Ptr_Typ)
              and then Finalize_Storage_Only (Desig_Typ)
            then
               Actual := Make_Null (Loc);

            --  Types in need of finalization actions

            elsif Needs_Finalization (Desig_Typ) then

               --  The general mechanism of creating finalization masters for
               --  anonymous access types is disabled by default, otherwise
               --  finalization masters will pop all over the place. Such types
               --  use context-specific masters.

               if Ekind (Ptr_Typ) = E_Anonymous_Access_Type
                 and then No (Finalization_Master (Ptr_Typ))
               then
                  Build_Finalization_Master
                    (Typ        => Ptr_Typ,
                     Ins_Node   => Associated_Node_For_Itype (Ptr_Typ),
                     Encl_Scope => Scope (Ptr_Typ));
               end if;

               --  Access-to-controlled types should always have a master

               pragma Assert (Present (Finalization_Master (Ptr_Typ)));

               Actual :=
                 Make_Attribute_Reference (Loc,
                   Prefix =>
                     New_Reference_To (Finalization_Master (Ptr_Typ), Loc),
                   Attribute_Name => Name_Unrestricted_Access);

            --  Tagged types

            else
               Actual := Make_Null (Loc);
            end if;
         end if;

         Analyze_And_Resolve (Actual, Etype (Formal));

         --  Build the parameter association for the new actual and add it to
         --  the end of the function's actuals.

         Add_Extra_Actual_To_Call (Func_Call, Formal, Actual);
      end;
   end Add_Finalization_Master_Actual_To_Build_In_Place_Call;

   ------------------------------
   -- Add_Extra_Actual_To_Call --
   ------------------------------

   procedure Add_Extra_Actual_To_Call
     (Subprogram_Call : Node_Id;
      Extra_Formal    : Entity_Id;
      Extra_Actual    : Node_Id)
   is
      Loc         : constant Source_Ptr := Sloc (Subprogram_Call);
      Param_Assoc : Node_Id;

   begin
      Param_Assoc :=
        Make_Parameter_Association (Loc,
          Selector_Name             => New_Occurrence_Of (Extra_Formal, Loc),
          Explicit_Actual_Parameter => Extra_Actual);

      Set_Parent (Param_Assoc, Subprogram_Call);
      Set_Parent (Extra_Actual, Param_Assoc);

      if Present (Parameter_Associations (Subprogram_Call)) then
         if Nkind (Last (Parameter_Associations (Subprogram_Call))) =
              N_Parameter_Association
         then

            --  Find last named actual, and append

            declare
               L : Node_Id;
            begin
               L := First_Actual (Subprogram_Call);
               while Present (L) loop
                  if No (Next_Actual (L)) then
                     Set_Next_Named_Actual (Parent (L), Extra_Actual);
                     exit;
                  end if;
                  Next_Actual (L);
               end loop;
            end;

         else
            Set_First_Named_Actual (Subprogram_Call, Extra_Actual);
         end if;

         Append (Param_Assoc, To => Parameter_Associations (Subprogram_Call));

      else
         Set_Parameter_Associations (Subprogram_Call, New_List (Param_Assoc));
         Set_First_Named_Actual (Subprogram_Call, Extra_Actual);
      end if;
   end Add_Extra_Actual_To_Call;

   ---------------------------------------------
   -- Add_Task_Actuals_To_Build_In_Place_Call --
   ---------------------------------------------

   procedure Add_Task_Actuals_To_Build_In_Place_Call
     (Function_Call : Node_Id;
      Function_Id   : Entity_Id;
      Master_Actual : Node_Id)
   is
      Loc           : constant Source_Ptr := Sloc (Function_Call);
      Result_Subt   : constant Entity_Id :=
                        Available_View (Etype (Function_Id));
      Actual        : Node_Id;
      Chain_Actual  : Node_Id;
      Chain_Formal  : Node_Id;
      Master_Formal : Node_Id;

   begin
      --  No such extra parameters are needed if there are no tasks

      if not Has_Task (Result_Subt) then
         return;
      end if;

      Actual := Master_Actual;

      --  Use a dummy _master actual in case of No_Task_Hierarchy

      if Restriction_Active (No_Task_Hierarchy) then
         Actual := New_Occurrence_Of (RTE (RE_Library_Task_Level), Loc);

      --  In the case where we use the master associated with an access type,
      --  the actual is an entity and requires an explicit reference.

      elsif Nkind (Actual) = N_Defining_Identifier then
         Actual := New_Reference_To (Actual, Loc);
      end if;

      --  Locate the implicit master parameter in the called function

      Master_Formal := Build_In_Place_Formal (Function_Id, BIP_Task_Master);
      Analyze_And_Resolve (Actual, Etype (Master_Formal));

      --  Build the parameter association for the new actual and add it to the
      --  end of the function's actuals.

      Add_Extra_Actual_To_Call (Function_Call, Master_Formal, Actual);

      --  Locate the implicit activation chain parameter in the called function

      Chain_Formal :=
        Build_In_Place_Formal (Function_Id, BIP_Activation_Chain);

      --  Create the actual which is a pointer to the current activation chain

      Chain_Actual :=
        Make_Attribute_Reference (Loc,
          Prefix         => Make_Identifier (Loc, Name_uChain),
          Attribute_Name => Name_Unrestricted_Access);

      Analyze_And_Resolve (Chain_Actual, Etype (Chain_Formal));

      --  Build the parameter association for the new actual and add it to the
      --  end of the function's actuals.

      Add_Extra_Actual_To_Call (Function_Call, Chain_Formal, Chain_Actual);
   end Add_Task_Actuals_To_Build_In_Place_Call;

   -----------------------
   -- BIP_Formal_Suffix --
   -----------------------

   function BIP_Formal_Suffix (Kind : BIP_Formal_Kind) return String is
   begin
      case Kind is
         when BIP_Alloc_Form          =>
            return "BIPalloc";
         when BIP_Storage_Pool        =>
            return "BIPstoragepool";
         when BIP_Finalization_Master =>
            return "BIPfinalizationmaster";
         when BIP_Task_Master         =>
            return "BIPtaskmaster";
         when BIP_Activation_Chain    =>
            return "BIPactivationchain";
         when BIP_Object_Access       =>
            return "BIPaccess";
      end case;
   end BIP_Formal_Suffix;

   ---------------------------
   -- Build_In_Place_Formal --
   ---------------------------

   function Build_In_Place_Formal
     (Func : Entity_Id;
      Kind : BIP_Formal_Kind) return Entity_Id
   is
      Formal_Name  : constant Name_Id :=
                       New_External_Name
                         (Chars (Func), BIP_Formal_Suffix (Kind));
      Extra_Formal : Entity_Id := Extra_Formals (Func);

   begin
      --  Maybe it would be better for each implicit formal of a build-in-place
      --  function to have a flag or a Uint attribute to identify it. ???

      --  The return type in the function declaration may have been a limited
      --  view, and the extra formals for the function were not generated at
      --  that point. At the point of call the full view must be available and
      --  the extra formals can be created.

      if No (Extra_Formal) then
         Create_Extra_Formals (Func);
         Extra_Formal := Extra_Formals (Func);
      end if;

      loop
         pragma Assert (Present (Extra_Formal));
         exit when Chars (Extra_Formal) = Formal_Name;

         Next_Formal_With_Extras (Extra_Formal);
      end loop;

      return Extra_Formal;
   end Build_In_Place_Formal;

   --------------------------------
   -- Check_Overriding_Operation --
   --------------------------------

   procedure Check_Overriding_Operation (Subp : Entity_Id) is
      Typ     : constant Entity_Id := Find_Dispatching_Type (Subp);
      Op_List : constant Elist_Id  := Primitive_Operations (Typ);
      Op_Elmt : Elmt_Id;
      Prim_Op : Entity_Id;
      Par_Op  : Entity_Id;

   begin
      if Is_Derived_Type (Typ)
        and then not Is_Private_Type (Typ)
        and then In_Open_Scopes (Scope (Etype (Typ)))
        and then Is_Base_Type (Typ)
      then
         --  Subp overrides an inherited private operation if there is an
         --  inherited operation with a different name than Subp (see
         --  Derive_Subprogram) whose Alias is a hidden subprogram with the
         --  same name as Subp.

         Op_Elmt := First_Elmt (Op_List);
         while Present (Op_Elmt) loop
            Prim_Op := Node (Op_Elmt);
            Par_Op  := Alias (Prim_Op);

            if Present (Par_Op)
              and then not Comes_From_Source (Prim_Op)
              and then Chars (Prim_Op) /= Chars (Par_Op)
              and then Chars (Par_Op) = Chars (Subp)
              and then Is_Hidden (Par_Op)
              and then Type_Conformant (Prim_Op, Subp)
            then
               Set_DT_Position (Subp, DT_Position (Prim_Op));
            end if;

            Next_Elmt (Op_Elmt);
         end loop;
      end if;
   end Check_Overriding_Operation;

   -------------------------------
   -- Detect_Infinite_Recursion --
   -------------------------------

   procedure Detect_Infinite_Recursion (N : Node_Id; Spec : Entity_Id) is
      Loc : constant Source_Ptr := Sloc (N);

      Var_List : constant Elist_Id := New_Elmt_List;
      --  List of globals referenced by body of procedure

      Call_List : constant Elist_Id := New_Elmt_List;
      --  List of recursive calls in body of procedure

      Shad_List : constant Elist_Id := New_Elmt_List;
      --  List of entity id's for entities created to capture the value of
      --  referenced globals on entry to the procedure.

      Scop : constant Uint := Scope_Depth (Spec);
      --  This is used to record the scope depth of the current procedure, so
      --  that we can identify global references.

      Max_Vars : constant := 4;
      --  Do not test more than four global variables

      Count_Vars : Natural := 0;
      --  Count variables found so far

      Var  : Entity_Id;
      Elm  : Elmt_Id;
      Ent  : Entity_Id;
      Call : Elmt_Id;
      Decl : Node_Id;
      Test : Node_Id;
      Elm1 : Elmt_Id;
      Elm2 : Elmt_Id;
      Last : Node_Id;

      function Process (Nod : Node_Id) return Traverse_Result;
      --  Function to traverse the subprogram body (using Traverse_Func)

      -------------
      -- Process --
      -------------

      function Process (Nod : Node_Id) return Traverse_Result is
      begin
         --  Procedure call

         if Nkind (Nod) = N_Procedure_Call_Statement then

            --  Case of one of the detected recursive calls

            if Is_Entity_Name (Name (Nod))
              and then Has_Recursive_Call (Entity (Name (Nod)))
              and then Entity (Name (Nod)) = Spec
            then
               Append_Elmt (Nod, Call_List);
               return Skip;

            --  Any other procedure call may have side effects

            else
               return Abandon;
            end if;

         --  A call to a pure function can always be ignored

         elsif Nkind (Nod) = N_Function_Call
           and then Is_Entity_Name (Name (Nod))
           and then Is_Pure (Entity (Name (Nod)))
         then
            return Skip;

         --  Case of an identifier reference

         elsif Nkind (Nod) = N_Identifier then
            Ent := Entity (Nod);

            --  If no entity, then ignore the reference

            --  Not clear why this can happen. To investigate, remove this
            --  test and look at the crash that occurs here in 3401-004 ???

            if No (Ent) then
               return Skip;

            --  Ignore entities with no Scope, again not clear how this
            --  can happen, to investigate, look at 4108-008 ???

            elsif No (Scope (Ent)) then
               return Skip;

            --  Ignore the reference if not to a more global object

            elsif Scope_Depth (Scope (Ent)) >= Scop then
               return Skip;

            --  References to types, exceptions and constants are always OK

            elsif Is_Type (Ent)
              or else Ekind (Ent) = E_Exception
              or else Ekind (Ent) = E_Constant
            then
               return Skip;

            --  If other than a non-volatile scalar variable, we have some
            --  kind of global reference (e.g. to a function) that we cannot
            --  deal with so we forget the attempt.

            elsif Ekind (Ent) /= E_Variable
              or else not Is_Scalar_Type (Etype (Ent))
              or else Treat_As_Volatile (Ent)
            then
               return Abandon;

            --  Otherwise we have a reference to a global scalar

            else
               --  Loop through global entities already detected

               Elm := First_Elmt (Var_List);
               loop
                  --  If not detected before, record this new global reference

                  if No (Elm) then
                     Count_Vars := Count_Vars + 1;

                     if Count_Vars <= Max_Vars then
                        Append_Elmt (Entity (Nod), Var_List);
                     else
                        return Abandon;
                     end if;

                     exit;

                  --  If recorded before, ignore

                  elsif Node (Elm) = Entity (Nod) then
                     return Skip;

                  --  Otherwise keep looking

                  else
                     Next_Elmt (Elm);
                  end if;
               end loop;

               return Skip;
            end if;

         --  For all other node kinds, recursively visit syntactic children

         else
            return OK;
         end if;
      end Process;

      function Traverse_Body is new Traverse_Func (Process);

   --  Start of processing for Detect_Infinite_Recursion

   begin
      --  Do not attempt detection in No_Implicit_Conditional mode, since we
      --  won't be able to generate the code to handle the recursion in any
      --  case.

      if Restriction_Active (No_Implicit_Conditionals) then
         return;
      end if;

      --  Otherwise do traversal and quit if we get abandon signal

      if Traverse_Body (N) = Abandon then
         return;

      --  We must have a call, since Has_Recursive_Call was set. If not just
      --  ignore (this is only an error check, so if we have a funny situation,
      --  due to bugs or errors, we do not want to bomb).

      elsif Is_Empty_Elmt_List (Call_List) then
         return;
      end if;

      --  Here is the case where we detect recursion at compile time

      --  Push our current scope for analyzing the declarations and code that
      --  we will insert for the checking.

      Push_Scope (Spec);

      --  This loop builds temporary variables for each of the referenced
      --  globals, so that at the end of the loop the list Shad_List contains
      --  these temporaries in one-to-one correspondence with the elements in
      --  Var_List.

      Last := Empty;
      Elm := First_Elmt (Var_List);
      while Present (Elm) loop
         Var := Node (Elm);
         Ent := Make_Temporary (Loc, 'S');
         Append_Elmt (Ent, Shad_List);

         --  Insert a declaration for this temporary at the start of the
         --  declarations for the procedure. The temporaries are declared as
         --  constant objects initialized to the current values of the
         --  corresponding temporaries.

         Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Ent,
             Object_Definition   => New_Occurrence_Of (Etype (Var), Loc),
             Constant_Present    => True,
             Expression          => New_Occurrence_Of (Var, Loc));

         if No (Last) then
            Prepend (Decl, Declarations (N));
         else
            Insert_After (Last, Decl);
         end if;

         Last := Decl;
         Analyze (Decl);
         Next_Elmt (Elm);
      end loop;

      --  Loop through calls

      Call := First_Elmt (Call_List);
      while Present (Call) loop

         --  Build a predicate expression of the form

         --    True
         --      and then global1 = temp1
         --      and then global2 = temp2
         --      ...

         --  This predicate determines if any of the global values
         --  referenced by the procedure have changed since the
         --  current call, if not an infinite recursion is assured.

         Test := New_Occurrence_Of (Standard_True, Loc);

         Elm1 := First_Elmt (Var_List);
         Elm2 := First_Elmt (Shad_List);
         while Present (Elm1) loop
            Test :=
              Make_And_Then (Loc,
                Left_Opnd  => Test,
                Right_Opnd =>
                  Make_Op_Eq (Loc,
                    Left_Opnd  => New_Occurrence_Of (Node (Elm1), Loc),
                    Right_Opnd => New_Occurrence_Of (Node (Elm2), Loc)));

            Next_Elmt (Elm1);
            Next_Elmt (Elm2);
         end loop;

         --  Now we replace the call with the sequence

         --    if no-changes (see above) then
         --       raise Storage_Error;
         --    else
         --       original-call
         --    end if;

         Rewrite (Node (Call),
           Make_If_Statement (Loc,
             Condition       => Test,
             Then_Statements => New_List (
               Make_Raise_Storage_Error (Loc,
                 Reason => SE_Infinite_Recursion)),

             Else_Statements => New_List (
               Relocate_Node (Node (Call)))));

         Analyze (Node (Call));

         Next_Elmt (Call);
      end loop;

      --  Remove temporary scope stack entry used for analysis

      Pop_Scope;
   end Detect_Infinite_Recursion;

   --------------------
   -- Expand_Actuals --
   --------------------

   procedure Expand_Actuals (N : Node_Id; Subp : Entity_Id) is
      Loc       : constant Source_Ptr := Sloc (N);
      Actual    : Node_Id;
      Formal    : Entity_Id;
      N_Node    : Node_Id;
      Post_Call : List_Id;
      E_Actual  : Entity_Id;
      E_Formal  : Entity_Id;

      procedure Add_Call_By_Copy_Code;
      --  For cases where the parameter must be passed by copy, this routine
      --  generates a temporary variable into which the actual is copied and
      --  then passes this as the parameter. For an OUT or IN OUT parameter,
      --  an assignment is also generated to copy the result back. The call
      --  also takes care of any constraint checks required for the type
      --  conversion case (on both the way in and the way out).

      procedure Add_Simple_Call_By_Copy_Code;
      --  This is similar to the above, but is used in cases where we know
      --  that all that is needed is to simply create a temporary and copy
      --  the value in and out of the temporary.

      procedure Check_Fortran_Logical;
      --  A value of type Logical that is passed through a formal parameter
      --  must be normalized because .TRUE. usually does not have the same
      --  representation as True. We assume that .FALSE. = False = 0.
      --  What about functions that return a logical type ???

      function Is_Legal_Copy return Boolean;
      --  Check that an actual can be copied before generating the temporary
      --  to be used in the call. If the actual is of a by_reference type then
      --  the program is illegal (this can only happen in the presence of
      --  rep. clauses that force an incorrect alignment). If the formal is
      --  a by_reference parameter imposed by a DEC pragma, emit a warning to
      --  the effect that this might lead to unaligned arguments.

      function Make_Var (Actual : Node_Id) return Entity_Id;
      --  Returns an entity that refers to the given actual parameter,
      --  Actual (not including any type conversion). If Actual is an
      --  entity name, then this entity is returned unchanged, otherwise
      --  a renaming is created to provide an entity for the actual.

      procedure Reset_Packed_Prefix;
      --  The expansion of a packed array component reference is delayed in
      --  the context of a call. Now we need to complete the expansion, so we
      --  unmark the analyzed bits in all prefixes.

      ---------------------------
      -- Add_Call_By_Copy_Code --
      ---------------------------

      procedure Add_Call_By_Copy_Code is
         Expr  : Node_Id;
         Init  : Node_Id;
         Temp  : Entity_Id;
         Indic : Node_Id;
         Var   : Entity_Id;
         F_Typ : constant Entity_Id := Etype (Formal);
         V_Typ : Entity_Id;
         Crep  : Boolean;

      begin
         if not Is_Legal_Copy then
            return;
         end if;

         Temp := Make_Temporary (Loc, 'T', Actual);

         --  Use formal type for temp, unless formal type is an unconstrained
         --  array, in which case we don't have to worry about bounds checks,
         --  and we use the actual type, since that has appropriate bounds.

         if Is_Array_Type (F_Typ) and then not Is_Constrained (F_Typ) then
            Indic := New_Occurrence_Of (Etype (Actual), Loc);
         else
            Indic := New_Occurrence_Of (Etype (Formal), Loc);
         end if;

         if Nkind (Actual) = N_Type_Conversion then
            V_Typ := Etype (Expression (Actual));

            --  If the formal is an (in-)out parameter, capture the name
            --  of the variable in order to build the post-call assignment.

            Var := Make_Var (Expression (Actual));

            Crep := not Same_Representation
                          (F_Typ, Etype (Expression (Actual)));

         else
            V_Typ := Etype (Actual);
            Var   := Make_Var (Actual);
            Crep  := False;
         end if;

         --  Setup initialization for case of in out parameter, or an out
         --  parameter where the formal is an unconstrained array (in the
         --  latter case, we have to pass in an object with bounds).

         --  If this is an out parameter, the initial copy is wasteful, so as
         --  an optimization for the one-dimensional case we extract the
         --  bounds of the actual and build an uninitialized temporary of the
         --  right size.

         if Ekind (Formal) = E_In_Out_Parameter
           or else (Is_Array_Type (F_Typ) and then not Is_Constrained (F_Typ))
         then
            if Nkind (Actual) = N_Type_Conversion then
               if Conversion_OK (Actual) then
                  Init := OK_Convert_To (F_Typ, New_Occurrence_Of (Var, Loc));
               else
                  Init := Convert_To (F_Typ, New_Occurrence_Of (Var, Loc));
               end if;

            elsif Ekind (Formal) = E_Out_Parameter
              and then Is_Array_Type (F_Typ)
              and then Number_Dimensions (F_Typ) = 1
              and then not Has_Non_Null_Base_Init_Proc (F_Typ)
            then
               --  Actual is a one-dimensional array or slice, and the type
               --  requires no initialization. Create a temporary of the
               --  right size, but do not copy actual into it (optimization).

               Init := Empty;
               Indic :=
                 Make_Subtype_Indication (Loc,
                   Subtype_Mark =>
                     New_Occurrence_Of (F_Typ, Loc),
                   Constraint   =>
                     Make_Index_Or_Discriminant_Constraint (Loc,
                       Constraints => New_List (
                         Make_Range (Loc,
                           Low_Bound  =>
                             Make_Attribute_Reference (Loc,
                               Prefix => New_Occurrence_Of (Var, Loc),
                               Attribute_Name => Name_First),
                           High_Bound =>
                             Make_Attribute_Reference (Loc,
                               Prefix => New_Occurrence_Of (Var, Loc),
                               Attribute_Name => Name_Last)))));

            else
               Init := New_Occurrence_Of (Var, Loc);
            end if;

         --  An initialization is created for packed conversions as
         --  actuals for out parameters to enable Make_Object_Declaration
         --  to determine the proper subtype for N_Node. Note that this
         --  is wasteful because the extra copying on the call side is
         --  not required for such out parameters. ???

         elsif Ekind (Formal) = E_Out_Parameter
           and then Nkind (Actual) = N_Type_Conversion
           and then (Is_Bit_Packed_Array (F_Typ)
                       or else
                     Is_Bit_Packed_Array (Etype (Expression (Actual))))
         then
            if Conversion_OK (Actual) then
               Init := OK_Convert_To (F_Typ, New_Occurrence_Of (Var, Loc));
            else
               Init := Convert_To (F_Typ, New_Occurrence_Of (Var, Loc));
            end if;

         elsif Ekind (Formal) = E_In_Parameter then

            --  Handle the case in which the actual is a type conversion

            if Nkind (Actual) = N_Type_Conversion then
               if Conversion_OK (Actual) then
                  Init := OK_Convert_To (F_Typ, New_Occurrence_Of (Var, Loc));
               else
                  Init := Convert_To (F_Typ, New_Occurrence_Of (Var, Loc));
               end if;
            else
               Init := New_Occurrence_Of (Var, Loc);
            end if;

         else
            Init := Empty;
         end if;

         N_Node :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Temp,
             Object_Definition   => Indic,
             Expression          => Init);
         Set_Assignment_OK (N_Node);
         Insert_Action (N, N_Node);

         --  Now, normally the deal here is that we use the defining
         --  identifier created by that object declaration. There is
         --  one exception to this. In the change of representation case
         --  the above declaration will end up looking like:

         --    temp : type := identifier;

         --  And in this case we might as well use the identifier directly
         --  and eliminate the temporary. Note that the analysis of the
         --  declaration was not a waste of time in that case, since it is
         --  what generated the necessary change of representation code. If
         --  the change of representation introduced additional code, as in
         --  a fixed-integer conversion, the expression is not an identifier
         --  and must be kept.

         if Crep
           and then Present (Expression (N_Node))
           and then Is_Entity_Name (Expression (N_Node))
         then
            Temp := Entity (Expression (N_Node));
            Rewrite (N_Node, Make_Null_Statement (Loc));
         end if;

         --  For IN parameter, all we do is to replace the actual

         if Ekind (Formal) = E_In_Parameter then
            Rewrite (Actual, New_Reference_To (Temp, Loc));
            Analyze (Actual);

         --  Processing for OUT or IN OUT parameter

         else
            --  Kill current value indications for the temporary variable we
            --  created, since we just passed it as an OUT parameter.

            Kill_Current_Values (Temp);
            Set_Is_Known_Valid (Temp, False);

            --  If type conversion, use reverse conversion on exit

            if Nkind (Actual) = N_Type_Conversion then
               if Conversion_OK (Actual) then
                  Expr := OK_Convert_To (V_Typ, New_Occurrence_Of (Temp, Loc));
               else
                  Expr := Convert_To (V_Typ, New_Occurrence_Of (Temp, Loc));
               end if;
            else
               Expr := New_Occurrence_Of (Temp, Loc);
            end if;

            Rewrite (Actual, New_Reference_To (Temp, Loc));
            Analyze (Actual);

            --  If the actual is a conversion of a packed reference, it may
            --  already have been expanded by Remove_Side_Effects, and the
            --  resulting variable is a temporary which does not designate
            --  the proper out-parameter, which may not be addressable. In
            --  that case, generate an assignment to the original expression
            --  (before expansion of the packed reference) so that the proper
            --  expansion of assignment to a packed component can take place.

            declare
               Obj : Node_Id;
               Lhs : Node_Id;

            begin
               if Is_Renaming_Of_Object (Var)
                 and then Nkind (Renamed_Object (Var)) = N_Selected_Component
                 and then Is_Entity_Name (Prefix (Renamed_Object (Var)))
                 and then Nkind (Original_Node (Prefix (Renamed_Object (Var))))
                   = N_Indexed_Component
                 and then
                   Has_Non_Standard_Rep (Etype (Prefix (Renamed_Object (Var))))
               then
                  Obj := Renamed_Object (Var);
                  Lhs :=
                    Make_Selected_Component (Loc,
                      Prefix        =>
                        New_Copy_Tree (Original_Node (Prefix (Obj))),
                      Selector_Name => New_Copy (Selector_Name (Obj)));
                  Reset_Analyzed_Flags (Lhs);

               else
                  Lhs :=  New_Occurrence_Of (Var, Loc);
               end if;

               Set_Assignment_OK (Lhs);

               if Is_Access_Type (E_Formal)
                 and then Is_Entity_Name (Lhs)
                 and then
                   Present (Effective_Extra_Accessibility (Entity (Lhs)))
               then
                  --  Copyback target is an Ada 2012 stand-alone object of an
                  --  anonymous access type.

                  pragma Assert (Ada_Version >= Ada_2012);

                  if Type_Access_Level (E_Formal) >
                     Object_Access_Level (Lhs)
                  then
                     Append_To (Post_Call,
                       Make_Raise_Program_Error (Loc,
                         Reason => PE_Accessibility_Check_Failed));
                  end if;

                  Append_To (Post_Call,
                    Make_Assignment_Statement (Loc,
                      Name       => Lhs,
                      Expression => Expr));

                  --  We would like to somehow suppress generation of the
                  --  extra_accessibility assignment generated by the expansion
                  --  of the above assignment statement. It's not a correctness
                  --  issue because the following assignment renders it dead,
                  --  but generating back-to-back assignments to the same
                  --  target is undesirable. ???

                  Append_To (Post_Call,
                    Make_Assignment_Statement (Loc,
                      Name       => New_Occurrence_Of (
                        Effective_Extra_Accessibility (Entity (Lhs)), Loc),
                      Expression => Make_Integer_Literal (Loc,
                        Type_Access_Level (E_Formal))));

               else
                  Append_To (Post_Call,
                    Make_Assignment_Statement (Loc,
                      Name       => Lhs,
                      Expression => Expr));
               end if;
            end;
         end if;
      end Add_Call_By_Copy_Code;

      ----------------------------------
      -- Add_Simple_Call_By_Copy_Code --
      ----------------------------------

      procedure Add_Simple_Call_By_Copy_Code is
         Temp   : Entity_Id;
         Decl   : Node_Id;
         Incod  : Node_Id;
         Outcod : Node_Id;
         Lhs    : Node_Id;
         Rhs    : Node_Id;
         Indic  : Node_Id;
         F_Typ  : constant Entity_Id := Etype (Formal);

      begin
         if not Is_Legal_Copy then
            return;
         end if;

         --  Use formal type for temp, unless formal type is an unconstrained
         --  array, in which case we don't have to worry about bounds checks,
         --  and we use the actual type, since that has appropriate bounds.

         if Is_Array_Type (F_Typ) and then not Is_Constrained (F_Typ) then
            Indic := New_Occurrence_Of (Etype (Actual), Loc);
         else
            Indic := New_Occurrence_Of (Etype (Formal), Loc);
         end if;

         --  Prepare to generate code

         Reset_Packed_Prefix;

         Temp := Make_Temporary (Loc, 'T', Actual);
         Incod  := Relocate_Node (Actual);
         Outcod := New_Copy_Tree (Incod);

         --  Generate declaration of temporary variable, initializing it
         --  with the input parameter unless we have an OUT formal or
         --  this is an initialization call.

         --  If the formal is an out parameter with discriminants, the
         --  discriminants must be captured even if the rest of the object
         --  is in principle uninitialized, because the discriminants may
         --  be read by the called subprogram.

         if Ekind (Formal) = E_Out_Parameter then
            Incod := Empty;

            if Has_Discriminants (Etype (Formal)) then
               Indic := New_Occurrence_Of (Etype (Actual), Loc);
            end if;

         elsif Inside_Init_Proc then

            --  Could use a comment here to match comment below ???

            if Nkind (Actual) /= N_Selected_Component
              or else
                not Has_Discriminant_Dependent_Constraint
                  (Entity (Selector_Name (Actual)))
            then
               Incod := Empty;

            --  Otherwise, keep the component in order to generate the proper
            --  actual subtype, that depends on enclosing discriminants.

            else
               null;
            end if;
         end if;

         Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Temp,
             Object_Definition   => Indic,
             Expression          => Incod);

         if Inside_Init_Proc
           and then No (Incod)
         then
            --  If the call is to initialize a component of a composite type,
            --  and the component does not depend on discriminants, use the
            --  actual type of the component. This is required in case the
            --  component is constrained, because in general the formal of the
            --  initialization procedure will be unconstrained. Note that if
            --  the component being initialized is constrained by an enclosing
            --  discriminant, the presence of the initialization in the
            --  declaration will generate an expression for the actual subtype.

            Set_No_Initialization (Decl);
            Set_Object_Definition (Decl,
              New_Occurrence_Of (Etype (Actual), Loc));
         end if;

         Insert_Action (N, Decl);

         --  The actual is simply a reference to the temporary

         Rewrite (Actual, New_Occurrence_Of (Temp, Loc));

         --  Generate copy out if OUT or IN OUT parameter

         if Ekind (Formal) /= E_In_Parameter then
            Lhs := Outcod;
            Rhs := New_Occurrence_Of (Temp, Loc);

            --  Deal with conversion

            if Nkind (Lhs) = N_Type_Conversion then
               Lhs := Expression (Lhs);
               Rhs := Convert_To (Etype (Actual), Rhs);
            end if;

            Append_To (Post_Call,
              Make_Assignment_Statement (Loc,
                Name       => Lhs,
                Expression => Rhs));
            Set_Assignment_OK (Name (Last (Post_Call)));
         end if;
      end Add_Simple_Call_By_Copy_Code;

      ---------------------------
      -- Check_Fortran_Logical --
      ---------------------------

      procedure Check_Fortran_Logical is
         Logical : constant Entity_Id := Etype (Formal);
         Var     : Entity_Id;

      --  Note: this is very incomplete, e.g. it does not handle arrays
      --  of logical values. This is really not the right approach at all???)

      begin
         if Convention (Subp) = Convention_Fortran
           and then Root_Type (Etype (Formal)) = Standard_Boolean
           and then Ekind (Formal) /= E_In_Parameter
         then
            Var := Make_Var (Actual);
            Append_To (Post_Call,
              Make_Assignment_Statement (Loc,
                Name => New_Occurrence_Of (Var, Loc),
                Expression =>
                  Unchecked_Convert_To (
                    Logical,
                    Make_Op_Ne (Loc,
                      Left_Opnd  => New_Occurrence_Of (Var, Loc),
                      Right_Opnd =>
                        Unchecked_Convert_To (
                          Logical,
                          New_Occurrence_Of (Standard_False, Loc))))));
         end if;
      end Check_Fortran_Logical;

      -------------------
      -- Is_Legal_Copy --
      -------------------

      function Is_Legal_Copy return Boolean is
      begin
         --  An attempt to copy a value of such a type can only occur if
         --  representation clauses give the actual a misaligned address.

         if Is_By_Reference_Type (Etype (Formal)) then

            --  If the front-end does not perform full type layout, the actual
            --  may in fact be properly aligned but there is not enough front-
            --  end information to determine this. In that case gigi will emit
            --  an error if a copy is not legal, or generate the proper code.
            --  For other backends we report the error now.

            --  Seems wrong to be issuing an error in the expander, since it
            --  will be missed in -gnatc mode ???

            if Frontend_Layout_On_Target then
               Error_Msg_N
                 ("misaligned actual cannot be passed by reference", Actual);
            end if;

            return False;

         --  For users of Starlet, we assume that the specification of by-
         --  reference mechanism is mandatory. This may lead to unaligned
         --  objects but at least for DEC legacy code it is known to work.
         --  The warning will alert users of this code that a problem may
         --  be lurking.

         elsif Mechanism (Formal) = By_Reference
           and then Is_Valued_Procedure (Scope (Formal))
         then
            Error_Msg_N
              ("by_reference actual may be misaligned??", Actual);
            return False;

         else
            return True;
         end if;
      end Is_Legal_Copy;

      --------------
      -- Make_Var --
      --------------

      function Make_Var (Actual : Node_Id) return Entity_Id is
         Var : Entity_Id;

      begin
         if Is_Entity_Name (Actual) then
            return Entity (Actual);

         else
            Var := Make_Temporary (Loc, 'T', Actual);

            N_Node :=
              Make_Object_Renaming_Declaration (Loc,
                Defining_Identifier => Var,
                Subtype_Mark        =>
                  New_Occurrence_Of (Etype (Actual), Loc),
                Name                => Relocate_Node (Actual));

            Insert_Action (N, N_Node);
            return Var;
         end if;
      end Make_Var;

      -------------------------
      -- Reset_Packed_Prefix --
      -------------------------

      procedure Reset_Packed_Prefix is
         Pfx : Node_Id := Actual;
      begin
         loop
            Set_Analyzed (Pfx, False);
            exit when
              not Nkind_In (Pfx, N_Selected_Component, N_Indexed_Component);
            Pfx := Prefix (Pfx);
         end loop;
      end Reset_Packed_Prefix;

   --  Start of processing for Expand_Actuals

   begin
      Post_Call := New_List;

      Formal := First_Formal (Subp);
      Actual := First_Actual (N);
      while Present (Formal) loop
         E_Formal := Etype (Formal);
         E_Actual := Etype (Actual);

         if Is_Scalar_Type (E_Formal)
           or else Nkind (Actual) = N_Slice
         then
            Check_Fortran_Logical;

         --  RM 6.4.1 (11)

         elsif Ekind (Formal) /= E_Out_Parameter then

            --  The unusual case of the current instance of a protected type
            --  requires special handling. This can only occur in the context
            --  of a call within the body of a protected operation.

            if Is_Entity_Name (Actual)
              and then Ekind (Entity (Actual)) = E_Protected_Type
              and then In_Open_Scopes (Entity (Actual))
            then
               if Scope (Subp) /= Entity (Actual) then
                  Error_Msg_N
                    ("operation outside protected type may not "
                     & "call back its protected operations??", Actual);
               end if;

               Rewrite (Actual,
                 Expand_Protected_Object_Reference (N, Entity (Actual)));
            end if;

            --  Ada 2005 (AI-318-02): If the actual parameter is a call to a
            --  build-in-place function, then a temporary return object needs
            --  to be created and access to it must be passed to the function.
            --  Currently we limit such functions to those with inherently
            --  limited result subtypes, but eventually we plan to expand the
            --  functions that are treated as build-in-place to include other
            --  composite result types.

            if Is_Build_In_Place_Function_Call (Actual) then
               Make_Build_In_Place_Call_In_Anonymous_Context (Actual);
            end if;

            Apply_Constraint_Check (Actual, E_Formal);

         --  Out parameter case. No constraint checks on access type
         --  RM 6.4.1 (13)

         elsif Is_Access_Type (E_Formal) then
            null;

         --  RM 6.4.1 (14)

         elsif Has_Discriminants (Base_Type (E_Formal))
           or else Has_Non_Null_Base_Init_Proc (E_Formal)
         then
            Apply_Constraint_Check (Actual, E_Formal);

         --  RM 6.4.1 (15)

         else
            Apply_Constraint_Check (Actual, Base_Type (E_Formal));
         end if;

         --  Processing for IN-OUT and OUT parameters

         if Ekind (Formal) /= E_In_Parameter then

            --  For type conversions of arrays, apply length/range checks

            if Is_Array_Type (E_Formal)
              and then Nkind (Actual) = N_Type_Conversion
            then
               if Is_Constrained (E_Formal) then
                  Apply_Length_Check (Expression (Actual), E_Formal);
               else
                  Apply_Range_Check (Expression (Actual), E_Formal);
               end if;
            end if;

            --  If argument is a type conversion for a type that is passed
            --  by copy, then we must pass the parameter by copy.

            if Nkind (Actual) = N_Type_Conversion
              and then
                (Is_Numeric_Type (E_Formal)
                  or else Is_Access_Type (E_Formal)
                  or else Is_Enumeration_Type (E_Formal)
                  or else Is_Bit_Packed_Array (Etype (Formal))
                  or else Is_Bit_Packed_Array (Etype (Expression (Actual)))

                  --  Also pass by copy if change of representation

                  or else not Same_Representation
                               (Etype (Formal),
                                Etype (Expression (Actual))))
            then
               Add_Call_By_Copy_Code;

            --  References to components of bit packed arrays are expanded
            --  at this point, rather than at the point of analysis of the
            --  actuals, to handle the expansion of the assignment to
            --  [in] out parameters.

            elsif Is_Ref_To_Bit_Packed_Array (Actual) then
               Add_Simple_Call_By_Copy_Code;

            --  If a non-scalar actual is possibly bit-aligned, we need a copy
            --  because the back-end cannot cope with such objects. In other
            --  cases where alignment forces a copy, the back-end generates
            --  it properly. It should not be generated unconditionally in the
            --  front-end because it does not know precisely the alignment
            --  requirements of the target, and makes too conservative an
            --  estimate, leading to superfluous copies or spurious errors
            --  on by-reference parameters.

            elsif Nkind (Actual) = N_Selected_Component
              and then
                Component_May_Be_Bit_Aligned (Entity (Selector_Name (Actual)))
              and then not Represented_As_Scalar (Etype (Formal))
            then
               Add_Simple_Call_By_Copy_Code;

            --  References to slices of bit packed arrays are expanded

            elsif Is_Ref_To_Bit_Packed_Slice (Actual) then
               Add_Call_By_Copy_Code;

            --  References to possibly unaligned slices of arrays are expanded

            elsif Is_Possibly_Unaligned_Slice (Actual) then
               Add_Call_By_Copy_Code;

            --  Deal with access types where the actual subtype and the
            --  formal subtype are not the same, requiring a check.

            --  It is necessary to exclude tagged types because of "downward
            --  conversion" errors.

            elsif Is_Access_Type (E_Formal)
              and then not Same_Type (E_Formal, E_Actual)
              and then not Is_Tagged_Type (Designated_Type (E_Formal))
            then
               Add_Call_By_Copy_Code;

            --  If the actual is not a scalar and is marked for volatile
            --  treatment, whereas the formal is not volatile, then pass
            --  by copy unless it is a by-reference type.

            --  Note: we use Is_Volatile here rather than Treat_As_Volatile,
            --  because this is the enforcement of a language rule that applies
            --  only to "real" volatile variables, not e.g. to the address
            --  clause overlay case.

            elsif Is_Entity_Name (Actual)
              and then Is_Volatile (Entity (Actual))
              and then not Is_By_Reference_Type (E_Actual)
              and then not Is_Scalar_Type (Etype (Entity (Actual)))
              and then not Is_Volatile (E_Formal)
            then
               Add_Call_By_Copy_Code;

            elsif Nkind (Actual) = N_Indexed_Component
              and then Is_Entity_Name (Prefix (Actual))
              and then Has_Volatile_Components (Entity (Prefix (Actual)))
            then
               Add_Call_By_Copy_Code;

            --  Add call-by-copy code for the case of scalar out parameters
            --  when it is not known at compile time that the subtype of the
            --  formal is a subrange of the subtype of the actual (or vice
            --  versa for in out parameters), in order to get range checks
            --  on such actuals. (Maybe this case should be handled earlier
            --  in the if statement???)

            elsif Is_Scalar_Type (E_Formal)
              and then
                (not In_Subrange_Of (E_Formal, E_Actual)
                  or else
                    (Ekind (Formal) = E_In_Out_Parameter
                      and then not In_Subrange_Of (E_Actual, E_Formal)))
            then
               --  Perhaps the setting back to False should be done within
               --  Add_Call_By_Copy_Code, since it could get set on other
               --  cases occurring above???

               if Do_Range_Check (Actual) then
                  Set_Do_Range_Check (Actual, False);
               end if;

               Add_Call_By_Copy_Code;
            end if;

            --  RM 3.2.4 (23/3) : A predicate is checked on in-out and out
            --  by-reference parameters on exit from the call. If the actual
            --  is a derived type and the operation is inherited, the body
            --  of the operation will not contain a call to the predicate
            --  function, so it must be done explicitly after the call. Ditto
            --  if the actual is an entity of a predicated subtype.

            --  The rule refers to by-reference types, but a check is needed
            --  for by-copy types as well. That check is subsumed by the rule
            --  for subtype conversion on assignment, but we can generate the
            --  required check now.

            --  Note that this is needed only if the subtype of the actual has
            --  an explicit predicate aspect, not if it inherits them from a
            --  base type or ancestor. The check is also superfluous if the
            --  subtype is elaborated before the body of the subprogram, but
            --  this is harder to verify, and there may be a redundant check.

            --  Note also that Subp may be either a subprogram entity for
            --  direct calls, or a type entity for indirect calls, which must
            --  be handled separately because the name does not denote an
            --  overloadable entity.

            --  If the formal is class-wide the corresponding postcondition
            --  procedure does not include a predicate call, so it has to be
            --  generated explicitly.

            if not Is_Init_Proc (Subp)
              and then (Has_Aspect (E_Actual, Aspect_Predicate)
                          or else
                        Has_Aspect (E_Actual, Aspect_Dynamic_Predicate)
                          or else
                        Has_Aspect (E_Actual, Aspect_Static_Predicate))
              and then Present (Predicate_Function (E_Actual))
            then
               if Is_Entity_Name (Actual)
                 or else
                   (Is_Derived_Type (E_Actual)
                     and then Is_Overloadable (Subp)
                     and then Is_Inherited_Operation_For_Type (Subp, E_Actual))
               then
                  Append_To (Post_Call,
                    Make_Predicate_Check (E_Actual, Actual));

               elsif Is_Class_Wide_Type (E_Formal)
                 and then not Is_Class_Wide_Type (E_Actual)
               then
                  Append_To (Post_Call,
                    Make_Predicate_Check (E_Actual, Actual));
               end if;
            end if;

         --  Processing for IN parameters

         else
            --  For IN parameters is in the packed array case, we expand an
            --  indexed component (the circuit in Exp_Ch4 deliberately left
            --  indexed components appearing as actuals untouched, so that
            --  the special processing above for the OUT and IN OUT cases
            --  could be performed. We could make the test in Exp_Ch4 more
            --  complex and have it detect the parameter mode, but it is
            --  easier simply to handle all cases here.)

            if Nkind (Actual) = N_Indexed_Component
              and then Is_Packed (Etype (Prefix (Actual)))
            then
               Reset_Packed_Prefix;
               Expand_Packed_Element_Reference (Actual);

            --  If we have a reference to a bit packed array, we copy it, since
            --  the actual must be byte aligned.

            --  Is this really necessary in all cases???

            elsif Is_Ref_To_Bit_Packed_Array (Actual) then
               Add_Simple_Call_By_Copy_Code;

            --  If a non-scalar actual is possibly unaligned, we need a copy

            elsif Is_Possibly_Unaligned_Object (Actual)
              and then not Represented_As_Scalar (Etype (Formal))
            then
               Add_Simple_Call_By_Copy_Code;

            --  Similarly, we have to expand slices of packed arrays here
            --  because the result must be byte aligned.

            elsif Is_Ref_To_Bit_Packed_Slice (Actual) then
               Add_Call_By_Copy_Code;

            --  Only processing remaining is to pass by copy if this is a
            --  reference to a possibly unaligned slice, since the caller
            --  expects an appropriately aligned argument.

            elsif Is_Possibly_Unaligned_Slice (Actual) then
               Add_Call_By_Copy_Code;

            --  An unusual case: a current instance of an enclosing task can be
            --  an actual, and must be replaced by a reference to self.

            elsif Is_Entity_Name (Actual)
              and then Is_Task_Type (Entity (Actual))
            then
               if In_Open_Scopes (Entity (Actual)) then
                  Rewrite (Actual,
                    (Make_Function_Call (Loc,
                     Name => New_Reference_To (RTE (RE_Self), Loc))));
                  Analyze (Actual);

               --  A task type cannot otherwise appear as an actual

               else
                  raise Program_Error;
               end if;
            end if;
         end if;

         Next_Formal (Formal);
         Next_Actual (Actual);
      end loop;

      --  Find right place to put post call stuff if it is present

      if not Is_Empty_List (Post_Call) then

         --  Cases where the call is not a member of a statement list

         if not Is_List_Member (N) then
            declare
               P :  Node_Id := Parent (N);

            begin
               --  In Ada 2012 the call may be a function call in an expression
               --  (since OUT and IN OUT parameters are now allowed for such
               --  calls. The write-back of (in)-out parameters is handled
               --  by the back-end, but the constraint checks generated when
               --  subtypes of formal and actual don't match must be inserted
               --  in the form of assignments, at the nearest point after the
               --  declaration or statement that contains the call.

               if Ada_Version >= Ada_2012
                 and then Nkind (N) = N_Function_Call
               then
                  while Nkind (P) not in N_Declaration
                    and then
                      Nkind (P) not in N_Statement_Other_Than_Procedure_Call
                  loop
                     P := Parent (P);
                  end loop;

                  Insert_Actions_After (P, Post_Call);

               --  If not the special Ada 2012 case of a function call, then
               --  we must have the triggering statement of a triggering
               --  alternative or an entry call alternative, and we can add
               --  the post call stuff to the corresponding statement list.

               else
                  pragma Assert (Nkind_In (P, N_Triggering_Alternative,
                                              N_Entry_Call_Alternative));

                  if Is_Non_Empty_List (Statements (P)) then
                     Insert_List_Before_And_Analyze
                       (First (Statements (P)), Post_Call);
                  else
                     Set_Statements (P, Post_Call);
                  end if;
               end if;

            end;

         --  Otherwise, normal case where N is in a statement sequence,
         --  just put the post-call stuff after the call statement.

         else
            Insert_Actions_After (N, Post_Call);
         end if;
      end if;

      --  The call node itself is re-analyzed in Expand_Call

   end Expand_Actuals;

   -----------------
   -- Expand_Call --
   -----------------

   --  This procedure handles expansion of function calls and procedure call
   --  statements (i.e. it serves as the body for Expand_N_Function_Call and
   --  Expand_N_Procedure_Call_Statement). Processing for calls includes:

   --    Replace call to Raise_Exception by Raise_Exception_Always if possible
   --    Provide values of actuals for all formals in Extra_Formals list
   --    Replace "call" to enumeration literal function by literal itself
   --    Rewrite call to predefined operator as operator
   --    Replace actuals to in-out parameters that are numeric conversions,
   --     with explicit assignment to temporaries before and after the call.
   --    Remove optional actuals if First_Optional_Parameter specified.

   --   Note that the list of actuals has been filled with default expressions
   --   during semantic analysis of the call. Only the extra actuals required
   --   for the 'Constrained attribute and for accessibility checks are added
   --   at this point.

   procedure Expand_Call (N : Node_Id) is
      Loc           : constant Source_Ptr := Sloc (N);
      Call_Node     : Node_Id := N;
      Extra_Actuals : List_Id := No_List;
      Prev          : Node_Id := Empty;

      procedure Add_Actual_Parameter (Insert_Param : Node_Id);
      --  Adds one entry to the end of the actual parameter list. Used for
      --  default parameters and for extra actuals (for Extra_Formals). The
      --  argument is an N_Parameter_Association node.

      procedure Add_Extra_Actual (Expr : Node_Id; EF : Entity_Id);
      --  Adds an extra actual to the list of extra actuals. Expr is the
      --  expression for the value of the actual, EF is the entity for the
      --  extra formal.

      procedure Do_Inline (Subp : Entity_Id; Orig_Subp : Entity_Id);
      --  Check and inline the body of Subp. Invoked when compiling with
      --  optimizations enabled and Subp has pragma inline or inline always.
      --  If the subprogram is a renaming, or if it is inherited, then Subp
      --  references the renamed entity and Orig_Subp is the entity of the
      --  call node N.

      procedure Do_Inline_Always (Subp : Entity_Id; Orig_Subp : Entity_Id);
      --  Check and inline the body of Subp. Invoked when compiling without
      --  optimizations and Subp has pragma inline always. If the subprogram is
      --  a renaming, or if it is inherited, then Subp references the renamed
      --  entity and Orig_Subp is the entity of the call node N.

      function Inherited_From_Formal (S : Entity_Id) return Entity_Id;
      --  Within an instance, a type derived from a non-tagged formal derived
      --  type inherits from the original parent, not from the actual. The
      --  current derivation mechanism has the derived type inherit from the
      --  actual, which is only correct outside of the instance. If the
      --  subprogram is inherited, we test for this particular case through a
      --  convoluted tree traversal before setting the proper subprogram to be
      --  called.

      function In_Unfrozen_Instance (E : Entity_Id) return Boolean;
      --  Return true if E comes from an instance that is not yet frozen

      function Is_Direct_Deep_Call (Subp : Entity_Id) return Boolean;
      --  Determine if Subp denotes a non-dispatching call to a Deep routine

      function New_Value (From : Node_Id) return Node_Id;
      --  From is the original Expression. New_Value is equivalent to a call
      --  to Duplicate_Subexpr with an explicit dereference when From is an
      --  access parameter.

      --------------------------
      -- Add_Actual_Parameter --
      --------------------------

      procedure Add_Actual_Parameter (Insert_Param : Node_Id) is
         Actual_Expr : constant Node_Id :=
                         Explicit_Actual_Parameter (Insert_Param);

      begin
         --  Case of insertion is first named actual

         if No (Prev) or else
            Nkind (Parent (Prev)) /= N_Parameter_Association
         then
            Set_Next_Named_Actual
              (Insert_Param, First_Named_Actual (Call_Node));
            Set_First_Named_Actual (Call_Node, Actual_Expr);

            if No (Prev) then
               if No (Parameter_Associations (Call_Node)) then
                  Set_Parameter_Associations (Call_Node, New_List);
               end if;

               Append (Insert_Param, Parameter_Associations (Call_Node));

            else
               Insert_After (Prev, Insert_Param);
            end if;

         --  Case of insertion is not first named actual

         else
            Set_Next_Named_Actual
              (Insert_Param, Next_Named_Actual (Parent (Prev)));
            Set_Next_Named_Actual (Parent (Prev), Actual_Expr);
            Append (Insert_Param, Parameter_Associations (Call_Node));
         end if;

         Prev := Actual_Expr;
      end Add_Actual_Parameter;

      ----------------------
      -- Add_Extra_Actual --
      ----------------------

      procedure Add_Extra_Actual (Expr : Node_Id; EF : Entity_Id) is
         Loc : constant Source_Ptr := Sloc (Expr);

      begin
         if Extra_Actuals = No_List then
            Extra_Actuals := New_List;
            Set_Parent (Extra_Actuals, Call_Node);
         end if;

         Append_To (Extra_Actuals,
           Make_Parameter_Association (Loc,
             Selector_Name             => Make_Identifier (Loc, Chars (EF)),
             Explicit_Actual_Parameter => Expr));

         Analyze_And_Resolve (Expr, Etype (EF));

         if Nkind (Call_Node) = N_Function_Call then
            Set_Is_Accessibility_Actual (Parent (Expr));
         end if;
      end Add_Extra_Actual;

      ----------------
      --  Do_Inline --
      ----------------

      procedure Do_Inline (Subp : Entity_Id; Orig_Subp : Entity_Id) is
         Spec : constant Node_Id := Unit_Declaration_Node (Subp);

         procedure Do_Backend_Inline;
         --  Check that the call can be safely passed to the backend. If true
         --  then register the enclosing unit of Subp to Inlined_Bodies so that
         --  the body of Subp can be retrieved and analyzed by the backend.

         procedure Register_Backend_Call (N : Node_Id);
         --  Append N to the list Backend_Calls

         -----------------------
         -- Do_Backend_Inline --
         -----------------------

         procedure Do_Backend_Inline is
         begin
            --  No extra test needed for init subprograms since we know they
            --  are available to the backend.

            if Is_Init_Proc (Subp) then
               Add_Inlined_Body (Subp);
               Register_Backend_Call (Call_Node);

            --  Verify that if the body to inline is located in the current
            --  unit the inlining does not occur earlier. This avoids
            --  order-of-elaboration problems in the back end.

            elsif In_Same_Extended_Unit (Call_Node, Subp)
              and then Nkind (Spec) = N_Subprogram_Declaration
              and then Earlier_In_Extended_Unit
                         (Loc, Sloc (Body_To_Inline (Spec)))
            then
               Error_Msg_NE
                 ("cannot inline& (body not seen yet)??", Call_Node, Subp);

            else
               declare
                  Backend_Inline : Boolean := True;

               begin
                  --  If we are compiling a package body that is not the
                  --  main unit, it must be for inlining/instantiation
                  --  purposes, in which case we inline the call to insure
                  --  that the same temporaries are generated when compiling
                  --  the body by itself. Otherwise link errors can occur.

                  --  If the function being called is itself in the main
                  --  unit, we cannot inline, because there is a risk of
                  --  double elaboration and/or circularity: the inlining
                  --  can make visible a private entity in the body of the
                  --  main unit, that gigi will see before its sees its
                  --  proper definition.

                  if not (In_Extended_Main_Code_Unit (Call_Node))
                    and then In_Package_Body
                  then
                     Backend_Inline :=
                       not In_Extended_Main_Source_Unit (Subp);
                  end if;

                  if Backend_Inline then
                     Add_Inlined_Body (Subp);
                     Register_Backend_Call (Call_Node);
                  end if;
               end;
            end if;
         end Do_Backend_Inline;

         ---------------------------
         -- Register_Backend_Call --
         ---------------------------

         procedure Register_Backend_Call (N : Node_Id) is
         begin
            if Backend_Calls = No_Elist then
               Backend_Calls := New_Elmt_List;
            end if;

            Append_Elmt (N, To => Backend_Calls);
         end Register_Backend_Call;

      --  Start of processing for Do_Inline

      begin
         --  Verify that the body to inline has already been seen

         if No (Spec)
           or else Nkind (Spec) /= N_Subprogram_Declaration
           or else No (Body_To_Inline (Spec))
         then
            if Comes_From_Source (Subp)
              and then Must_Inline (Subp)
            then
               Cannot_Inline
                 ("cannot inline& (body not seen yet)?", Call_Node, Subp);

            --  Let the back end handle it

            else
               Do_Backend_Inline;
               return;
            end if;

         --  If this an inherited function that returns a private type, do not
         --  inline if the full view is an unconstrained array, because such
         --  calls cannot be inlined.

         elsif Present (Orig_Subp)
           and then Is_Array_Type (Etype (Orig_Subp))
           and then not Is_Constrained (Etype (Orig_Subp))
         then
            Cannot_Inline
              ("cannot inline& (unconstrained array)?", Call_Node, Subp);

         else
            Expand_Inlined_Call (Call_Node, Subp, Orig_Subp);
         end if;
      end Do_Inline;

      ----------------------
      -- Do_Inline_Always --
      ----------------------

      procedure Do_Inline_Always (Subp : Entity_Id; Orig_Subp : Entity_Id) is
         Spec    : constant Node_Id := Unit_Declaration_Node (Subp);
         Body_Id : Entity_Id;

      begin
         if No (Spec)
           or else Nkind (Spec) /= N_Subprogram_Declaration
           or else No (Body_To_Inline (Spec))
           or else Serious_Errors_Detected /= 0
         then
            return;
         end if;

         Body_Id := Corresponding_Body (Spec);

         --  Verify that the body to inline has already been seen

         if No (Body_Id)
           or else not Analyzed (Body_Id)
         then
            Set_Is_Inlined (Subp, False);

            if Comes_From_Source (Subp) then

               --  Report a warning only if the call is located in the unit of
               --  the called subprogram; otherwise it is an error.

               if not In_Same_Extended_Unit (Call_Node, Subp) then
                  Cannot_Inline
                    ("cannot inline& (body not seen yet)?", Call_Node, Subp,
                     Is_Serious => True);

               elsif In_Open_Scopes (Subp) then

                  --  For backward compatibility we generate the same error
                  --  or warning of the previous implementation. This will
                  --  be changed when we definitely incorporate the new
                  --  support ???

                  if Front_End_Inlining
                    and then Optimization_Level = 0
                  then
                     Error_Msg_N
                       ("call to recursive subprogram cannot be inlined?p?",
                        N);

                  --  Do not emit error compiling runtime packages

                  elsif Is_Predefined_File_Name
                    (Unit_File_Name (Get_Source_Unit (Subp)))
                  then
                     Error_Msg_N
                       ("call to recursive subprogram cannot be inlined??",
                        N);

                  else
                     Error_Msg_N
                       ("call to recursive subprogram cannot be inlined",
                        N);
                  end if;

               else
                  Cannot_Inline
                    ("cannot inline& (body not seen yet)?", Call_Node, Subp);
               end if;
            end if;

            return;

         --  If this an inherited function that returns a private type, do not
         --  inline if the full view is an unconstrained array, because such
         --  calls cannot be inlined.

         elsif Present (Orig_Subp)
           and then Is_Array_Type (Etype (Orig_Subp))
           and then not Is_Constrained (Etype (Orig_Subp))
         then
            Cannot_Inline
              ("cannot inline& (unconstrained array)?", Call_Node, Subp);

         --  If the called subprogram comes from an instance in the same
         --  unit, and the instance is not yet frozen, inlining might
         --  trigger order-of-elaboration problems.

         elsif In_Unfrozen_Instance (Scope (Subp)) then
            Cannot_Inline
              ("cannot inline& (unfrozen instance)?", Call_Node, Subp);

         else
            Expand_Inlined_Call (Call_Node, Subp, Orig_Subp);
         end if;
      end Do_Inline_Always;

      ---------------------------
      -- Inherited_From_Formal --
      ---------------------------

      function Inherited_From_Formal (S : Entity_Id) return Entity_Id is
         Par      : Entity_Id;
         Gen_Par  : Entity_Id;
         Gen_Prim : Elist_Id;
         Elmt     : Elmt_Id;
         Indic    : Node_Id;

      begin
         --  If the operation is inherited, it is attached to the corresponding
         --  type derivation. If the parent in the derivation is a generic
         --  actual, it is a subtype of the actual, and we have to recover the
         --  original derived type declaration to find the proper parent.

         if Nkind (Parent (S)) /= N_Full_Type_Declaration
           or else not Is_Derived_Type (Defining_Identifier (Parent (S)))
           or else Nkind (Type_Definition (Original_Node (Parent (S)))) /=
                                                   N_Derived_Type_Definition
           or else not In_Instance
         then
            return Empty;

         else
            Indic :=
              Subtype_Indication
                (Type_Definition (Original_Node (Parent (S))));

            if Nkind (Indic) = N_Subtype_Indication then
               Par := Entity (Subtype_Mark (Indic));
            else
               Par := Entity (Indic);
            end if;
         end if;

         if not Is_Generic_Actual_Type (Par)
           or else Is_Tagged_Type (Par)
           or else Nkind (Parent (Par)) /= N_Subtype_Declaration
           or else not In_Open_Scopes (Scope (Par))
         then
            return Empty;
         else
            Gen_Par := Generic_Parent_Type (Parent (Par));
         end if;

         --  If the actual has no generic parent type, the formal is not
         --  a formal derived type, so nothing to inherit.

         if No (Gen_Par) then
            return Empty;
         end if;

         --  If the generic parent type is still the generic type, this is a
         --  private formal, not a derived formal, and there are no operations
         --  inherited from the formal.

         if Nkind (Parent (Gen_Par)) = N_Formal_Type_Declaration then
            return Empty;
         end if;

         Gen_Prim := Collect_Primitive_Operations (Gen_Par);

         Elmt := First_Elmt (Gen_Prim);
         while Present (Elmt) loop
            if Chars (Node (Elmt)) = Chars (S) then
               declare
                  F1 : Entity_Id;
                  F2 : Entity_Id;

               begin
                  F1 := First_Formal (S);
                  F2 := First_Formal (Node (Elmt));
                  while Present (F1)
                    and then Present (F2)
                  loop
                     if Etype (F1) = Etype (F2)
                       or else Etype (F2) = Gen_Par
                     then
                        Next_Formal (F1);
                        Next_Formal (F2);
                     else
                        Next_Elmt (Elmt);
                        exit;   --  not the right subprogram
                     end if;

                     return Node (Elmt);
                  end loop;
               end;

            else
               Next_Elmt (Elmt);
            end if;
         end loop;

         raise Program_Error;
      end Inherited_From_Formal;

      --------------------------
      -- In_Unfrozen_Instance --
      --------------------------

      function In_Unfrozen_Instance (E : Entity_Id) return Boolean is
         S : Entity_Id;

      begin
         S := E;
         while Present (S) and then S /= Standard_Standard loop
            if Is_Generic_Instance (S)
              and then Present (Freeze_Node (S))
              and then not Analyzed (Freeze_Node (S))
            then
               return True;
            end if;

            S := Scope (S);
         end loop;

         return False;
      end In_Unfrozen_Instance;

      -------------------------
      -- Is_Direct_Deep_Call --
      -------------------------

      function Is_Direct_Deep_Call (Subp : Entity_Id) return Boolean is
      begin
         if Is_TSS (Subp, TSS_Deep_Adjust)
           or else Is_TSS (Subp, TSS_Deep_Finalize)
           or else Is_TSS (Subp, TSS_Deep_Initialize)
         then
            declare
               Actual : Node_Id;
               Formal : Node_Id;

            begin
               Actual := First (Parameter_Associations (N));
               Formal := First_Formal (Subp);
               while Present (Actual)
                 and then Present (Formal)
               loop
                  if Nkind (Actual) = N_Identifier
                    and then Is_Controlling_Actual (Actual)
                    and then Etype (Actual) = Etype (Formal)
                  then
                     return True;
                  end if;

                  Next (Actual);
                  Next_Formal (Formal);
               end loop;
            end;
         end if;

         return False;
      end Is_Direct_Deep_Call;

      ---------------
      -- New_Value --
      ---------------

      function New_Value (From : Node_Id) return Node_Id is
         Res : constant Node_Id := Duplicate_Subexpr (From);
      begin
         if Is_Access_Type (Etype (From)) then
            return Make_Explicit_Dereference (Sloc (From), Prefix => Res);
         else
            return Res;
         end if;
      end New_Value;

      --  Local variables

      Curr_S        : constant Entity_Id := Current_Scope;
      Remote        : constant Boolean   := Is_Remote_Call (Call_Node);
      Actual        : Node_Id;
      Formal        : Entity_Id;
      Orig_Subp     : Entity_Id := Empty;
      Param_Count   : Natural := 0;
      Parent_Formal : Entity_Id;
      Parent_Subp   : Entity_Id;
      Scop          : Entity_Id;
      Subp          : Entity_Id;

      Prev_Orig : Node_Id;
      --  Original node for an actual, which may have been rewritten. If the
      --  actual is a function call that has been transformed from a selected
      --  component, the original node is unanalyzed. Otherwise, it carries
      --  semantic information used to generate additional actuals.

      CW_Interface_Formals_Present : Boolean := False;

   --  Start of processing for Expand_Call

   begin
      --  Expand the procedure call if the first actual has a dimension and if
      --  the procedure is Put (Ada 2012).

      if Ada_Version >= Ada_2012
        and then Nkind (Call_Node) = N_Procedure_Call_Statement
        and then Present (Parameter_Associations (Call_Node))
      then
         Expand_Put_Call_With_Symbol (Call_Node);
      end if;

      --  Ignore if previous error

      if Nkind (Call_Node) in N_Has_Etype
        and then Etype (Call_Node) = Any_Type
      then
         return;
      end if;

      --  Call using access to subprogram with explicit dereference

      if Nkind (Name (Call_Node)) = N_Explicit_Dereference then
         Subp        := Etype (Name (Call_Node));
         Parent_Subp := Empty;

      --  Case of call to simple entry, where the Name is a selected component
      --  whose prefix is the task, and whose selector name is the entry name

      elsif Nkind (Name (Call_Node)) = N_Selected_Component then
         Subp        := Entity (Selector_Name (Name (Call_Node)));
         Parent_Subp := Empty;

      --  Case of call to member of entry family, where Name is an indexed
      --  component, with the prefix being a selected component giving the
      --  task and entry family name, and the index being the entry index.

      elsif Nkind (Name (Call_Node)) = N_Indexed_Component then
         Subp        := Entity (Selector_Name (Prefix (Name (Call_Node))));
         Parent_Subp := Empty;

      --  Normal case

      else
         Subp        := Entity (Name (Call_Node));
         Parent_Subp := Alias (Subp);

         --  Replace call to Raise_Exception by call to Raise_Exception_Always
         --  if we can tell that the first parameter cannot possibly be null.
         --  This improves efficiency by avoiding a run-time test.

         --  We do not do this if Raise_Exception_Always does not exist, which
         --  can happen in configurable run time profiles which provide only a
         --  Raise_Exception.

         if Is_RTE (Subp, RE_Raise_Exception)
           and then RTE_Available (RE_Raise_Exception_Always)
         then
            declare
               FA : constant Node_Id :=
                      Original_Node (First_Actual (Call_Node));

            begin
               --  The case we catch is where the first argument is obtained
               --  using the Identity attribute (which must always be
               --  non-null).

               if Nkind (FA) = N_Attribute_Reference
                 and then Attribute_Name (FA) = Name_Identity
               then
                  Subp := RTE (RE_Raise_Exception_Always);
                  Set_Name (Call_Node, New_Occurrence_Of (Subp, Loc));
               end if;
            end;
         end if;

         if Ekind (Subp) = E_Entry then
            Parent_Subp := Empty;
         end if;
      end if;

      --  Detect the following code in System.Finalization_Masters only on
      --  .NET/JVM targets:
      --
      --    procedure Finalize (Master : in out Finalization_Master) is
      --    begin
      --       . . .
      --       begin
      --          Finalize (Curr_Ptr.all);
      --
      --  Since .NET/JVM compilers lack address arithmetic and Deep_Finalize
      --  cannot be named in library or user code, the compiler has to install
      --  a kludge and transform the call to Finalize into Deep_Finalize.

      if VM_Target /= No_VM
        and then Chars (Subp) = Name_Finalize
        and then Ekind (Curr_S) = E_Block
        and then Ekind (Scope (Curr_S)) = E_Procedure
        and then Chars (Scope (Curr_S)) = Name_Finalize
        and then Etype (First_Formal (Scope (Curr_S))) =
                   RTE (RE_Finalization_Master)
      then
         declare
            Deep_Fin : constant Entity_Id :=
                         Find_Prim_Op (RTE (RE_Root_Controlled),
                                       TSS_Deep_Finalize);
         begin
            --  Since Root_Controlled is a tagged type, the compiler should
            --  always generate Deep_Finalize for it.

            pragma Assert (Present (Deep_Fin));

            --  Generate:
            --    Deep_Finalize (Curr_Ptr.all);

            Rewrite (N,
              Make_Procedure_Call_Statement (Loc,
                Name =>
                  New_Reference_To (Deep_Fin, Loc),
                Parameter_Associations =>
                  New_Copy_List_Tree (Parameter_Associations (N))));

            Analyze (N);
            return;
         end;
      end if;

      --  Ada 2005 (AI-345): We have a procedure call as a triggering
      --  alternative in an asynchronous select or as an entry call in
      --  a conditional or timed select. Check whether the procedure call
      --  is a renaming of an entry and rewrite it as an entry call.

      if Ada_Version >= Ada_2005
        and then Nkind (Call_Node) = N_Procedure_Call_Statement
        and then
           ((Nkind (Parent (Call_Node)) = N_Triggering_Alternative
              and then Triggering_Statement (Parent (Call_Node)) = Call_Node)
          or else
            (Nkind (Parent (Call_Node)) = N_Entry_Call_Alternative
              and then Entry_Call_Statement (Parent (Call_Node)) = Call_Node))
      then
         declare
            Ren_Decl : Node_Id;
            Ren_Root : Entity_Id := Subp;

         begin
            --  This may be a chain of renamings, find the root

            if Present (Alias (Ren_Root)) then
               Ren_Root := Alias (Ren_Root);
            end if;

            if Present (Original_Node (Parent (Parent (Ren_Root)))) then
               Ren_Decl := Original_Node (Parent (Parent (Ren_Root)));

               if Nkind (Ren_Decl) = N_Subprogram_Renaming_Declaration then
                  Rewrite (Call_Node,
                    Make_Entry_Call_Statement (Loc,
                      Name =>
                        New_Copy_Tree (Name (Ren_Decl)),
                      Parameter_Associations =>
                        New_Copy_List_Tree
                          (Parameter_Associations (Call_Node))));

                  return;
               end if;
            end if;
         end;
      end if;

      --  First step, compute extra actuals, corresponding to any Extra_Formals
      --  present. Note that we do not access Extra_Formals directly, instead
      --  we simply note the presence of the extra formals as we process the
      --  regular formals collecting corresponding actuals in Extra_Actuals.

      --  We also generate any required range checks for actuals for in formals
      --  as we go through the loop, since this is a convenient place to do it.
      --  (Though it seems that this would be better done in Expand_Actuals???)

      --  Special case: Thunks must not compute the extra actuals; they must
      --  just propagate to the target primitive their extra actuals.

      if Is_Thunk (Current_Scope)
        and then Thunk_Entity (Current_Scope) = Subp
        and then Present (Extra_Formals (Subp))
      then
         pragma Assert (Present (Extra_Formals (Current_Scope)));

         declare
            Target_Formal : Entity_Id;
            Thunk_Formal  : Entity_Id;

         begin
            Target_Formal := Extra_Formals (Subp);
            Thunk_Formal  := Extra_Formals (Current_Scope);
            while Present (Target_Formal) loop
               Add_Extra_Actual
                 (New_Occurrence_Of (Thunk_Formal, Loc), Thunk_Formal);

               Target_Formal := Extra_Formal (Target_Formal);
               Thunk_Formal  := Extra_Formal (Thunk_Formal);
            end loop;

            while Is_Non_Empty_List (Extra_Actuals) loop
               Add_Actual_Parameter (Remove_Head (Extra_Actuals));
            end loop;

            Expand_Actuals (Call_Node, Subp);
            return;
         end;
      end if;

      Formal := First_Formal (Subp);
      Actual := First_Actual (Call_Node);
      Param_Count := 1;
      while Present (Formal) loop

         --  Generate range check if required

         if Do_Range_Check (Actual)
           and then Ekind (Formal) = E_In_Parameter
         then
            Set_Do_Range_Check (Actual, False);
            Generate_Range_Check
              (Actual, Etype (Formal), CE_Range_Check_Failed);
         end if;

         --  Prepare to examine current entry

         Prev := Actual;
         Prev_Orig := Original_Node (Prev);

         --  Ada 2005 (AI-251): Check if any formal is a class-wide interface
         --  to expand it in a further round.

         CW_Interface_Formals_Present :=
           CW_Interface_Formals_Present
             or else
               (Ekind (Etype (Formal)) = E_Class_Wide_Type
                 and then Is_Interface (Etype (Etype (Formal))))
             or else
               (Ekind (Etype (Formal)) = E_Anonymous_Access_Type
                 and then Is_Interface (Directly_Designated_Type
                                         (Etype (Etype (Formal)))));

         --  Create possible extra actual for constrained case. Usually, the
         --  extra actual is of the form actual'constrained, but since this
         --  attribute is only available for unconstrained records, TRUE is
         --  expanded if the type of the formal happens to be constrained (for
         --  instance when this procedure is inherited from an unconstrained
         --  record to a constrained one) or if the actual has no discriminant
         --  (its type is constrained). An exception to this is the case of a
         --  private type without discriminants. In this case we pass FALSE
         --  because the object has underlying discriminants with defaults.

         if Present (Extra_Constrained (Formal)) then
            if Ekind (Etype (Prev)) in Private_Kind
              and then not Has_Discriminants (Base_Type (Etype (Prev)))
            then
               Add_Extra_Actual
                 (New_Occurrence_Of (Standard_False, Loc),
                  Extra_Constrained (Formal));

            elsif Is_Constrained (Etype (Formal))
              or else not Has_Discriminants (Etype (Prev))
            then
               Add_Extra_Actual
                 (New_Occurrence_Of (Standard_True, Loc),
                  Extra_Constrained (Formal));

            --  Do not produce extra actuals for Unchecked_Union parameters.
            --  Jump directly to the end of the loop.

            elsif Is_Unchecked_Union (Base_Type (Etype (Actual))) then
               goto Skip_Extra_Actual_Generation;

            else
               --  If the actual is a type conversion, then the constrained
               --  test applies to the actual, not the target type.

               declare
                  Act_Prev : Node_Id;

               begin
                  --  Test for unchecked conversions as well, which can occur
                  --  as out parameter actuals on calls to stream procedures.

                  Act_Prev := Prev;
                  while Nkind_In (Act_Prev, N_Type_Conversion,
                                            N_Unchecked_Type_Conversion)
                  loop
                     Act_Prev := Expression (Act_Prev);
                  end loop;

                  --  If the expression is a conversion of a dereference, this
                  --  is internally generated code that manipulates addresses,
                  --  e.g. when building interface tables. No check should
                  --  occur in this case, and the discriminated object is not
                  --  directly a hand.

                  if not Comes_From_Source (Actual)
                    and then Nkind (Actual) = N_Unchecked_Type_Conversion
                    and then Nkind (Act_Prev) = N_Explicit_Dereference
                  then
                     Add_Extra_Actual
                       (New_Occurrence_Of (Standard_False, Loc),
                        Extra_Constrained (Formal));

                  else
                     Add_Extra_Actual
                       (Make_Attribute_Reference (Sloc (Prev),
                        Prefix =>
                          Duplicate_Subexpr_No_Checks
                            (Act_Prev, Name_Req => True),
                        Attribute_Name => Name_Constrained),
                        Extra_Constrained (Formal));
                  end if;
               end;
            end if;
         end if;

         --  Create possible extra actual for accessibility level

         if Present (Extra_Accessibility (Formal)) then

            --  Ada 2005 (AI-252): If the actual was rewritten as an Access
            --  attribute, then the original actual may be an aliased object
            --  occurring as the prefix in a call using "Object.Operation"
            --  notation. In that case we must pass the level of the object,
            --  so Prev_Orig is reset to Prev and the attribute will be
            --  processed by the code for Access attributes further below.

            if Prev_Orig /= Prev
              and then Nkind (Prev) = N_Attribute_Reference
              and then
                Get_Attribute_Id (Attribute_Name (Prev)) = Attribute_Access
              and then Is_Aliased_View (Prev_Orig)
            then
               Prev_Orig := Prev;
            end if;

            --  Ada 2005 (AI-251): Thunks must propagate the extra actuals of
            --  accessibility levels.

            if Is_Thunk (Current_Scope) then
               declare
                  Parm_Ent : Entity_Id;

               begin
                  if Is_Controlling_Actual (Actual) then

                     --  Find the corresponding actual of the thunk

                     Parm_Ent := First_Entity (Current_Scope);
                     for J in 2 .. Param_Count loop
                        Next_Entity (Parm_Ent);
                     end loop;

                  --  Handle unchecked conversion of access types generated
                  --  in thunks (cf. Expand_Interface_Thunk).

                  elsif Is_Access_Type (Etype (Actual))
                    and then Nkind (Actual) = N_Unchecked_Type_Conversion
                  then
                     Parm_Ent := Entity (Expression (Actual));

                  else pragma Assert (Is_Entity_Name (Actual));
                     Parm_Ent := Entity (Actual);
                  end if;

                  Add_Extra_Actual
                    (New_Occurrence_Of (Extra_Accessibility (Parm_Ent), Loc),
                     Extra_Accessibility (Formal));
               end;

            elsif Is_Entity_Name (Prev_Orig) then

               --  When passing an access parameter, or a renaming of an access
               --  parameter, as the actual to another access parameter we need
               --  to pass along the actual's own access level parameter. This
               --  is done if we are within the scope of the formal access
               --  parameter (if this is an inlined body the extra formal is
               --  irrelevant).

               if (Is_Formal (Entity (Prev_Orig))
                    or else
                      (Present (Renamed_Object (Entity (Prev_Orig)))
                        and then
                          Is_Entity_Name (Renamed_Object (Entity (Prev_Orig)))
                        and then
                          Is_Formal
                            (Entity (Renamed_Object (Entity (Prev_Orig))))))
                 and then Ekind (Etype (Prev_Orig)) = E_Anonymous_Access_Type
                 and then In_Open_Scopes (Scope (Entity (Prev_Orig)))
               then
                  declare
                     Parm_Ent : constant Entity_Id := Param_Entity (Prev_Orig);

                  begin
                     pragma Assert (Present (Parm_Ent));

                     if Present (Extra_Accessibility (Parm_Ent)) then
                        Add_Extra_Actual
                          (New_Occurrence_Of
                             (Extra_Accessibility (Parm_Ent), Loc),
                           Extra_Accessibility (Formal));

                     --  If the actual access parameter does not have an
                     --  associated extra formal providing its scope level,
                     --  then treat the actual as having library-level
                     --  accessibility.

                     else
                        Add_Extra_Actual
                          (Make_Integer_Literal (Loc,
                             Intval => Scope_Depth (Standard_Standard)),
                           Extra_Accessibility (Formal));
                     end if;
                  end;

               --  The actual is a normal access value, so just pass the level
               --  of the actual's access type.

               else
                  Add_Extra_Actual
                    (Dynamic_Accessibility_Level (Prev_Orig),
                     Extra_Accessibility (Formal));
               end if;

            --  If the actual is an access discriminant, then pass the level
            --  of the enclosing object (RM05-3.10.2(12.4/2)).

            elsif Nkind (Prev_Orig) = N_Selected_Component
              and then Ekind (Entity (Selector_Name (Prev_Orig))) =
                                                       E_Discriminant
              and then Ekind (Etype (Entity (Selector_Name (Prev_Orig)))) =
                                                       E_Anonymous_Access_Type
            then
               Add_Extra_Actual
                 (Make_Integer_Literal (Loc,
                    Intval => Object_Access_Level (Prefix (Prev_Orig))),
                  Extra_Accessibility (Formal));

            --  All other cases

            else
               case Nkind (Prev_Orig) is

                  when N_Attribute_Reference =>
                     case Get_Attribute_Id (Attribute_Name (Prev_Orig)) is

                        --  For X'Access, pass on the level of the prefix X

                        when Attribute_Access =>

                           --  If this is an Access attribute applied to the
                           --  the current instance object passed to a type
                           --  initialization procedure, then use the level
                           --  of the type itself. This is not really correct,
                           --  as there should be an extra level parameter
                           --  passed in with _init formals (only in the case
                           --  where the type is immutably limited), but we
                           --  don't have an easy way currently to create such
                           --  an extra formal (init procs aren't ever frozen).
                           --  For now we just use the level of the type,
                           --  which may be too shallow, but that works better
                           --  than passing Object_Access_Level of the type,
                           --  which can be one level too deep in some cases.
                           --  ???

                           if Is_Entity_Name (Prefix (Prev_Orig))
                             and then Is_Type (Entity (Prefix (Prev_Orig)))
                           then
                              Add_Extra_Actual
                                (Make_Integer_Literal (Loc,
                                   Intval =>
                                     Type_Access_Level
                                       (Entity (Prefix (Prev_Orig)))),
                                 Extra_Accessibility (Formal));

                           else
                              Add_Extra_Actual
                                (Make_Integer_Literal (Loc,
                                   Intval =>
                                     Object_Access_Level
                                       (Prefix (Prev_Orig))),
                                 Extra_Accessibility (Formal));
                           end if;

                        --  Treat the unchecked attributes as library-level

                        when Attribute_Unchecked_Access |
                           Attribute_Unrestricted_Access =>
                           Add_Extra_Actual
                             (Make_Integer_Literal (Loc,
                                Intval => Scope_Depth (Standard_Standard)),
                              Extra_Accessibility (Formal));

                        --  No other cases of attributes returning access
                        --  values that can be passed to access parameters.

                        when others =>
                           raise Program_Error;

                     end case;

                  --  For allocators we pass the level of the execution of the
                  --  called subprogram, which is one greater than the current
                  --  scope level.

                  when N_Allocator =>
                     Add_Extra_Actual
                       (Make_Integer_Literal (Loc,
                          Intval => Scope_Depth (Current_Scope) + 1),
                        Extra_Accessibility (Formal));

                  --  For most other cases we simply pass the level of the
                  --  actual's access type. The type is retrieved from
                  --  Prev rather than Prev_Orig, because in some cases
                  --  Prev_Orig denotes an original expression that has
                  --  not been analyzed.

                  when others =>
                     Add_Extra_Actual
                       (Dynamic_Accessibility_Level (Prev),
                        Extra_Accessibility (Formal));
               end case;
            end if;
         end if;

         --  Perform the check of 4.6(49) that prevents a null value from being
         --  passed as an actual to an access parameter. Note that the check
         --  is elided in the common cases of passing an access attribute or
         --  access parameter as an actual. Also, we currently don't enforce
         --  this check for expander-generated actuals and when -gnatdj is set.

         if Ada_Version >= Ada_2005 then

            --  Ada 2005 (AI-231): Check null-excluding access types. Note that
            --  the intent of 6.4.1(13) is that null-exclusion checks should
            --  not be done for 'out' parameters, even though it refers only
            --  to constraint checks, and a null_exclusion is not a constraint.
            --  Note that AI05-0196-1 corrects this mistake in the RM.

            if Is_Access_Type (Etype (Formal))
              and then Can_Never_Be_Null (Etype (Formal))
              and then Ekind (Formal) /= E_Out_Parameter
              and then Nkind (Prev) /= N_Raise_Constraint_Error
              and then (Known_Null (Prev)
                         or else not Can_Never_Be_Null (Etype (Prev)))
            then
               Install_Null_Excluding_Check (Prev);
            end if;

         --  Ada_Version < Ada_2005

         else
            if Ekind (Etype (Formal)) /= E_Anonymous_Access_Type
              or else Access_Checks_Suppressed (Subp)
            then
               null;

            elsif Debug_Flag_J then
               null;

            elsif not Comes_From_Source (Prev) then
               null;

            elsif Is_Entity_Name (Prev)
              and then Ekind (Etype (Prev)) = E_Anonymous_Access_Type
            then
               null;

            elsif Nkind_In (Prev, N_Allocator, N_Attribute_Reference) then
               null;

            --  Suppress null checks when passing to access parameters of Java
            --  and CIL subprograms. (Should this be done for other foreign
            --  conventions as well ???)

            elsif Convention (Subp) = Convention_Java
              or else Convention (Subp) = Convention_CIL
            then
               null;

            else
               Install_Null_Excluding_Check (Prev);
            end if;
         end if;

         --  Perform appropriate validity checks on parameters that
         --  are entities.

         if Validity_Checks_On then
            if  (Ekind (Formal) = E_In_Parameter
                  and then Validity_Check_In_Params)
              or else
                (Ekind (Formal) = E_In_Out_Parameter
                  and then Validity_Check_In_Out_Params)
            then
               --  If the actual is an indexed component of a packed type (or
               --  is an indexed or selected component whose prefix recursively
               --  meets this condition), it has not been expanded yet. It will
               --  be copied in the validity code that follows, and has to be
               --  expanded appropriately, so reanalyze it.

               --  What we do is just to unset analyzed bits on prefixes till
               --  we reach something that does not have a prefix.

               declare
                  Nod : Node_Id;

               begin
                  Nod := Actual;
                  while Nkind_In (Nod, N_Indexed_Component,
                                       N_Selected_Component)
                  loop
                     Set_Analyzed (Nod, False);
                     Nod := Prefix (Nod);
                  end loop;
               end;

               Ensure_Valid (Actual);
            end if;
         end if;

         --  For Ada 2012, if a parameter is aliased, the actual must be a
         --  tagged type or an aliased view of an object.

         if Is_Aliased (Formal)
           and then not Is_Aliased_View (Actual)
           and then not Is_Tagged_Type (Etype (Formal))
         then
            Error_Msg_NE
              ("actual for aliased formal& must be aliased object",
               Actual, Formal);
         end if;

         --  For IN OUT and OUT parameters, ensure that subscripts are valid
         --  since this is a left side reference. We only do this for calls
         --  from the source program since we assume that compiler generated
         --  calls explicitly generate any required checks. We also need it
         --  only if we are doing standard validity checks, since clearly it is
         --  not needed if validity checks are off, and in subscript validity
         --  checking mode, all indexed components are checked with a call
         --  directly from Expand_N_Indexed_Component.

         if Comes_From_Source (Call_Node)
           and then Ekind (Formal) /= E_In_Parameter
           and then Validity_Checks_On
           and then Validity_Check_Default
           and then not Validity_Check_Subscripts
         then
            Check_Valid_Lvalue_Subscripts (Actual);
         end if;

         --  Mark any scalar OUT parameter that is a simple variable as no
         --  longer known to be valid (unless the type is always valid). This
         --  reflects the fact that if an OUT parameter is never set in a
         --  procedure, then it can become invalid on the procedure return.

         if Ekind (Formal) = E_Out_Parameter
           and then Is_Entity_Name (Actual)
           and then Ekind (Entity (Actual)) = E_Variable
           and then not Is_Known_Valid (Etype (Actual))
         then
            Set_Is_Known_Valid (Entity (Actual), False);
         end if;

         --  For an OUT or IN OUT parameter, if the actual is an entity, then
         --  clear current values, since they can be clobbered. We are probably
         --  doing this in more places than we need to, but better safe than
         --  sorry when it comes to retaining bad current values.

         if Ekind (Formal) /= E_In_Parameter
           and then Is_Entity_Name (Actual)
           and then Present (Entity (Actual))
         then
            declare
               Ent : constant Entity_Id := Entity (Actual);
               Sav : Node_Id;

            begin
               --  For an OUT or IN OUT parameter that is an assignable entity,
               --  we do not want to clobber the Last_Assignment field, since
               --  if it is set, it was precisely because it is indeed an OUT
               --  or IN OUT parameter. We do reset the Is_Known_Valid flag
               --  since the subprogram could have returned in invalid value.

               if Ekind_In (Formal, E_Out_Parameter, E_In_Out_Parameter)
                 and then Is_Assignable (Ent)
               then
                  Sav := Last_Assignment (Ent);
                  Kill_Current_Values (Ent);
                  Set_Last_Assignment (Ent, Sav);
                  Set_Is_Known_Valid (Ent, False);

               --  For all other cases, just kill the current values

               else
                  Kill_Current_Values (Ent);
               end if;
            end;
         end if;

         --  If the formal is class wide and the actual is an aggregate, force
         --  evaluation so that the back end who does not know about class-wide
         --  type, does not generate a temporary of the wrong size.

         if not Is_Class_Wide_Type (Etype (Formal)) then
            null;

         elsif Nkind (Actual) = N_Aggregate
           or else (Nkind (Actual) = N_Qualified_Expression
                     and then Nkind (Expression (Actual)) = N_Aggregate)
         then
            Force_Evaluation (Actual);
         end if;

         --  In a remote call, if the formal is of a class-wide type, check
         --  that the actual meets the requirements described in E.4(18).

         if Remote and then Is_Class_Wide_Type (Etype (Formal)) then
            Insert_Action (Actual,
              Make_Transportable_Check (Loc,
                Duplicate_Subexpr_Move_Checks (Actual)));
         end if;

         --  This label is required when skipping extra actual generation for
         --  Unchecked_Union parameters.

         <<Skip_Extra_Actual_Generation>>

         Param_Count := Param_Count + 1;
         Next_Actual (Actual);
         Next_Formal (Formal);
      end loop;

      --  If we are calling an Ada 2012 function which needs to have the
      --  "accessibility level determined by the point of call" (AI05-0234)
      --  passed in to it, then pass it in.

      if Ekind_In (Subp, E_Function, E_Operator, E_Subprogram_Type)
        and then
          Present (Extra_Accessibility_Of_Result (Ultimate_Alias (Subp)))
      then
         declare
            Ancestor : Node_Id := Parent (Call_Node);
            Level    : Node_Id := Empty;
            Defer    : Boolean := False;

         begin
            --  Unimplemented: if Subp returns an anonymous access type, then

            --    a) if the call is the operand of an explict conversion, then
            --       the target type of the conversion (a named access type)
            --       determines the accessibility level pass in;

            --    b) if the call defines an access discriminant of an object
            --       (e.g., the discriminant of an object being created by an
            --       allocator, or the discriminant of a function result),
            --       then the accessibility level to pass in is that of the
            --       discriminated object being initialized).

            --  ???

            while Nkind (Ancestor) = N_Qualified_Expression
            loop
               Ancestor := Parent (Ancestor);
            end loop;

            case Nkind (Ancestor) is
               when N_Allocator =>

                  --  At this point, we'd like to assign

                  --    Level := Dynamic_Accessibility_Level (Ancestor);

                  --  but Etype of Ancestor may not have been set yet,
                  --  so that doesn't work.

                  --  Handle this later in Expand_Allocator_Expression.

                  Defer := True;

               when N_Object_Declaration | N_Object_Renaming_Declaration =>
                  declare
                     Def_Id : constant Entity_Id :=
                                Defining_Identifier (Ancestor);

                  begin
                     if Is_Return_Object (Def_Id) then
                        if Present (Extra_Accessibility_Of_Result
                                     (Return_Applies_To (Scope (Def_Id))))
                        then
                           --  Pass along value that was passed in if the
                           --  routine we are returning from also has an
                           --  Accessibility_Of_Result formal.

                           Level :=
                             New_Occurrence_Of
                              (Extra_Accessibility_Of_Result
                                (Return_Applies_To (Scope (Def_Id))), Loc);
                        end if;
                     else
                        Level :=
                          Make_Integer_Literal (Loc,
                            Intval => Object_Access_Level (Def_Id));
                     end if;
                  end;

               when N_Simple_Return_Statement =>
                  if Present (Extra_Accessibility_Of_Result
                               (Return_Applies_To
                                 (Return_Statement_Entity (Ancestor))))
                  then
                     --  Pass along value that was passed in if the routine
                     --  we are returning from also has an
                     --  Accessibility_Of_Result formal.

                     Level :=
                       New_Occurrence_Of
                         (Extra_Accessibility_Of_Result
                            (Return_Applies_To
                               (Return_Statement_Entity (Ancestor))), Loc);
                  end if;

               when others =>
                  null;
            end case;

            if not Defer then
               if not Present (Level) then

                  --  The "innermost master that evaluates the function call".

                  --  ??? - Should we use Integer'Last here instead in order
                  --  to deal with (some of) the problems associated with
                  --  calls to subps whose enclosing scope is unknown (e.g.,
                  --  Anon_Access_To_Subp_Param.all)?

                  Level := Make_Integer_Literal (Loc,
                             Scope_Depth (Current_Scope) + 1);
               end if;

               Add_Extra_Actual
                 (Level,
                  Extra_Accessibility_Of_Result (Ultimate_Alias (Subp)));
            end if;
         end;
      end if;

      --  If we are expanding the RHS of an assignment we need to check if tag
      --  propagation is needed. You might expect this processing to be in
      --  Analyze_Assignment but has to be done earlier (bottom-up) because the
      --  assignment might be transformed to a declaration for an unconstrained
      --  value if the expression is classwide.

      if Nkind (Call_Node) = N_Function_Call
        and then Is_Tag_Indeterminate (Call_Node)
        and then Is_Entity_Name (Name (Call_Node))
      then
         declare
            Ass : Node_Id := Empty;

         begin
            if Nkind (Parent (Call_Node)) = N_Assignment_Statement then
               Ass := Parent (Call_Node);

            elsif Nkind (Parent (Call_Node)) = N_Qualified_Expression
              and then Nkind (Parent (Parent (Call_Node))) =
                                                  N_Assignment_Statement
            then
               Ass := Parent (Parent (Call_Node));

            elsif Nkind (Parent (Call_Node)) = N_Explicit_Dereference
              and then Nkind (Parent (Parent (Call_Node))) =
                                                  N_Assignment_Statement
            then
               Ass := Parent (Parent (Call_Node));
            end if;

            if Present (Ass)
              and then Is_Class_Wide_Type (Etype (Name (Ass)))
            then
               if Is_Access_Type (Etype (Call_Node)) then
                  if Designated_Type (Etype (Call_Node)) /=
                    Root_Type (Etype (Name (Ass)))
                  then
                     Error_Msg_NE
                       ("tag-indeterminate expression "
                         & " must have designated type& (RM 5.2 (6))",
                         Call_Node, Root_Type (Etype (Name (Ass))));
                  else
                     Propagate_Tag (Name (Ass), Call_Node);
                  end if;

               elsif Etype (Call_Node) /= Root_Type (Etype (Name (Ass))) then
                  Error_Msg_NE
                    ("tag-indeterminate expression must have type&"
                     & "(RM 5.2 (6))",
                     Call_Node, Root_Type (Etype (Name (Ass))));

               else
                  Propagate_Tag (Name (Ass), Call_Node);
               end if;

               --  The call will be rewritten as a dispatching call, and
               --  expanded as such.

               return;
            end if;
         end;
      end if;

      --  Ada 2005 (AI-251): If some formal is a class-wide interface, expand
      --  it to point to the correct secondary virtual table

      if Nkind (Call_Node) in N_Subprogram_Call
        and then CW_Interface_Formals_Present
      then
         Expand_Interface_Actuals (Call_Node);
      end if;

      --  Deals with Dispatch_Call if we still have a call, before expanding
      --  extra actuals since this will be done on the re-analysis of the
      --  dispatching call. Note that we do not try to shorten the actual list
      --  for a dispatching call, it would not make sense to do so. Expansion
      --  of dispatching calls is suppressed when VM_Target, because the VM
      --  back-ends directly handle the generation of dispatching calls and
      --  would have to undo any expansion to an indirect call.

      if Nkind (Call_Node) in N_Subprogram_Call
        and then Present (Controlling_Argument (Call_Node))
      then
         declare
            Call_Typ   : constant Entity_Id := Etype (Call_Node);
            Typ        : constant Entity_Id := Find_Dispatching_Type (Subp);
            Eq_Prim_Op : Entity_Id := Empty;
            New_Call   : Node_Id;
            Param      : Node_Id;
            Prev_Call  : Node_Id;

         begin
            if not Is_Limited_Type (Typ) then
               Eq_Prim_Op := Find_Prim_Op (Typ, Name_Op_Eq);
            end if;

            if Tagged_Type_Expansion then
               Expand_Dispatching_Call (Call_Node);

               --  The following return is worrisome. Is it really OK to skip
               --  all remaining processing in this procedure ???

               return;

            --  VM targets

            else
               Apply_Tag_Checks (Call_Node);

               --  If this is a dispatching "=", we must first compare the
               --  tags so we generate: x.tag = y.tag and then x = y

               if Subp = Eq_Prim_Op then

                  --  Mark the node as analyzed to avoid reanalizing this
                  --  dispatching call (which would cause a never-ending loop)

                  Prev_Call := Relocate_Node (Call_Node);
                  Set_Analyzed (Prev_Call);

                  Param := First_Actual (Call_Node);
                  New_Call :=
                    Make_And_Then (Loc,
                      Left_Opnd =>
                           Make_Op_Eq (Loc,
                             Left_Opnd =>
                               Make_Selected_Component (Loc,
                                 Prefix        => New_Value (Param),
                                 Selector_Name =>
                                   New_Reference_To (First_Tag_Component (Typ),
                                                     Loc)),

                             Right_Opnd =>
                               Make_Selected_Component (Loc,
                                 Prefix        =>
                                   Unchecked_Convert_To (Typ,
                                     New_Value (Next_Actual (Param))),
                                 Selector_Name =>
                                   New_Reference_To
                                     (First_Tag_Component (Typ), Loc))),
                      Right_Opnd => Prev_Call);

                  Rewrite (Call_Node, New_Call);

                  Analyze_And_Resolve
                    (Call_Node, Call_Typ, Suppress => All_Checks);
               end if;

               --  Expansion of a dispatching call results in an indirect call,
               --  which in turn causes current values to be killed (see
               --  Resolve_Call), so on VM targets we do the call here to
               --  ensure consistent warnings between VM and non-VM targets.

               Kill_Current_Values;
            end if;

            --  If this is a dispatching "=" then we must update the reference
            --  to the call node because we generated:
            --     x.tag = y.tag and then x = y

            if Subp = Eq_Prim_Op then
               Call_Node := Right_Opnd (Call_Node);
            end if;
         end;
      end if;

      --  Similarly, expand calls to RCI subprograms on which pragma
      --  All_Calls_Remote applies. The rewriting will be reanalyzed
      --  later. Do this only when the call comes from source since we
      --  do not want such a rewriting to occur in expanded code.

      if Is_All_Remote_Call (Call_Node) then
         Expand_All_Calls_Remote_Subprogram_Call (Call_Node);

      --  Similarly, do not add extra actuals for an entry call whose entity
      --  is a protected procedure, or for an internal protected subprogram
      --  call, because it will be rewritten as a protected subprogram call
      --  and reanalyzed (see Expand_Protected_Subprogram_Call).

      elsif Is_Protected_Type (Scope (Subp))
         and then (Ekind (Subp) = E_Procedure
                    or else Ekind (Subp) = E_Function)
      then
         null;

      --  During that loop we gathered the extra actuals (the ones that
      --  correspond to Extra_Formals), so now they can be appended.

      else
         while Is_Non_Empty_List (Extra_Actuals) loop
            Add_Actual_Parameter (Remove_Head (Extra_Actuals));
         end loop;
      end if;

      --  At this point we have all the actuals, so this is the point at which
      --  the various expansion activities for actuals is carried out.

      Expand_Actuals (Call_Node, Subp);

      --  Verify that the actuals do not share storage. This check must be done
      --  on the caller side rather that inside the subprogram to avoid issues
      --  of parameter passing.

      if Check_Aliasing_Of_Parameters then
         Apply_Parameter_Aliasing_Checks (Call_Node, Subp);
      end if;

      --  If the subprogram is a renaming, or if it is inherited, replace it in
      --  the call with the name of the actual subprogram being called. If this
      --  is a dispatching call, the run-time decides what to call. The Alias
      --  attribute does not apply to entries.

      if Nkind (Call_Node) /= N_Entry_Call_Statement
        and then No (Controlling_Argument (Call_Node))
        and then Present (Parent_Subp)
        and then not Is_Direct_Deep_Call (Subp)
      then
         if Present (Inherited_From_Formal (Subp)) then
            Parent_Subp := Inherited_From_Formal (Subp);
         else
            Parent_Subp := Ultimate_Alias (Parent_Subp);
         end if;

         --  The below setting of Entity is suspect, see F109-018 discussion???

         Set_Entity (Name (Call_Node), Parent_Subp);

         if Is_Abstract_Subprogram (Parent_Subp)
           and then not In_Instance
         then
            Error_Msg_NE
              ("cannot call abstract subprogram &!",
               Name (Call_Node), Parent_Subp);
         end if;

         --  Inspect all formals of derived subprogram Subp. Compare parameter
         --  types with the parent subprogram and check whether an actual may
         --  need a type conversion to the corresponding formal of the parent
         --  subprogram.

         --  Not clear whether intrinsic subprograms need such conversions. ???

         if not Is_Intrinsic_Subprogram (Parent_Subp)
           or else Is_Generic_Instance (Parent_Subp)
         then
            declare
               procedure Convert (Act : Node_Id; Typ : Entity_Id);
               --  Rewrite node Act as a type conversion of Act to Typ. Analyze
               --  and resolve the newly generated construct.

               -------------
               -- Convert --
               -------------

               procedure Convert (Act : Node_Id; Typ : Entity_Id) is
               begin
                  Rewrite (Act, OK_Convert_To (Typ, Relocate_Node (Act)));
                  Analyze (Act);
                  Resolve (Act, Typ);
               end Convert;

               --  Local variables

               Actual_Typ : Entity_Id;
               Formal_Typ : Entity_Id;
               Parent_Typ : Entity_Id;

            begin
               Actual := First_Actual (Call_Node);
               Formal := First_Formal (Subp);
               Parent_Formal := First_Formal (Parent_Subp);
               while Present (Formal) loop
                  Actual_Typ := Etype (Actual);
                  Formal_Typ := Etype (Formal);
                  Parent_Typ := Etype (Parent_Formal);

                  --  For an IN parameter of a scalar type, the parent formal
                  --  type and derived formal type differ or the parent formal
                  --  type and actual type do not match statically.

                  if Is_Scalar_Type (Formal_Typ)
                    and then Ekind (Formal) = E_In_Parameter
                    and then Formal_Typ /= Parent_Typ
                    and then
                      not Subtypes_Statically_Match (Parent_Typ, Actual_Typ)
                    and then not Raises_Constraint_Error (Actual)
                  then
                     Convert (Actual, Parent_Typ);
                     Enable_Range_Check (Actual);

                     --  If the actual has been marked as requiring a range
                     --  check, then generate it here.

                     if Do_Range_Check (Actual) then
                        Set_Do_Range_Check (Actual, False);
                        Generate_Range_Check
                          (Actual, Etype (Formal), CE_Range_Check_Failed);
                     end if;

                  --  For access types, the parent formal type and actual type
                  --  differ.

                  elsif Is_Access_Type (Formal_Typ)
                    and then Base_Type (Parent_Typ) /= Base_Type (Actual_Typ)
                  then
                     if Ekind (Formal) /= E_In_Parameter then
                        Convert (Actual, Parent_Typ);

                     elsif Ekind (Parent_Typ) = E_Anonymous_Access_Type
                       and then Designated_Type (Parent_Typ) /=
                                Designated_Type (Actual_Typ)
                       and then not Is_Controlling_Formal (Formal)
                     then
                        --  This unchecked conversion is not necessary unless
                        --  inlining is enabled, because in that case the type
                        --  mismatch may become visible in the body about to be
                        --  inlined.

                        Rewrite (Actual,
                          Unchecked_Convert_To (Parent_Typ,
                            Relocate_Node (Actual)));
                        Analyze (Actual);
                        Resolve (Actual, Parent_Typ);
                     end if;

                  --  For array and record types, the parent formal type and
                  --  derived formal type have different sizes or pragma Pack
                  --  status.

                  elsif ((Is_Array_Type (Formal_Typ)
                            and then Is_Array_Type (Parent_Typ))
                       or else
                         (Is_Record_Type (Formal_Typ)
                            and then Is_Record_Type (Parent_Typ)))
                    and then
                      (Esize (Formal_Typ) /= Esize (Parent_Typ)
                         or else Has_Pragma_Pack (Formal_Typ) /=
                                 Has_Pragma_Pack (Parent_Typ))
                  then
                     Convert (Actual, Parent_Typ);
                  end if;

                  Next_Actual (Actual);
                  Next_Formal (Formal);
                  Next_Formal (Parent_Formal);
               end loop;
            end;
         end if;

         Orig_Subp := Subp;
         Subp := Parent_Subp;
      end if;

      --  Check for violation of No_Abort_Statements

      if Restriction_Check_Required (No_Abort_Statements)
        and then Is_RTE (Subp, RE_Abort_Task)
      then
         Check_Restriction (No_Abort_Statements, Call_Node);

      --  Check for violation of No_Dynamic_Attachment

      elsif Restriction_Check_Required (No_Dynamic_Attachment)
        and then RTU_Loaded (Ada_Interrupts)
        and then (Is_RTE (Subp, RE_Is_Reserved)      or else
                  Is_RTE (Subp, RE_Is_Attached)      or else
                  Is_RTE (Subp, RE_Current_Handler)  or else
                  Is_RTE (Subp, RE_Attach_Handler)   or else
                  Is_RTE (Subp, RE_Exchange_Handler) or else
                  Is_RTE (Subp, RE_Detach_Handler)   or else
                  Is_RTE (Subp, RE_Reference))
      then
         Check_Restriction (No_Dynamic_Attachment, Call_Node);
      end if;

      --  Deal with case where call is an explicit dereference

      if Nkind (Name (Call_Node)) = N_Explicit_Dereference then

      --  Handle case of access to protected subprogram type

         if Is_Access_Protected_Subprogram_Type
              (Base_Type (Etype (Prefix (Name (Call_Node)))))
         then
            --  If this is a call through an access to protected operation, the
            --  prefix has the form (object'address, operation'access). Rewrite
            --  as a for other protected calls: the object is the 1st parameter
            --  of the list of actuals.

            declare
               Call : Node_Id;
               Parm : List_Id;
               Nam  : Node_Id;
               Obj  : Node_Id;
               Ptr  : constant Node_Id := Prefix (Name (Call_Node));

               T : constant Entity_Id :=
                     Equivalent_Type (Base_Type (Etype (Ptr)));

               D_T : constant Entity_Id :=
                       Designated_Type (Base_Type (Etype (Ptr)));

            begin
               Obj :=
                 Make_Selected_Component (Loc,
                   Prefix        => Unchecked_Convert_To (T, Ptr),
                   Selector_Name =>
                     New_Occurrence_Of (First_Entity (T), Loc));

               Nam :=
                 Make_Selected_Component (Loc,
                   Prefix        => Unchecked_Convert_To (T, Ptr),
                   Selector_Name =>
                     New_Occurrence_Of (Next_Entity (First_Entity (T)), Loc));

               Nam :=
                 Make_Explicit_Dereference (Loc,
                   Prefix => Nam);

               if Present (Parameter_Associations (Call_Node))  then
                  Parm := Parameter_Associations (Call_Node);
               else
                  Parm := New_List;
               end if;

               Prepend (Obj, Parm);

               if Etype (D_T) = Standard_Void_Type then
                  Call :=
                    Make_Procedure_Call_Statement (Loc,
                      Name                   => Nam,
                      Parameter_Associations => Parm);
               else
                  Call :=
                    Make_Function_Call (Loc,
                      Name                   => Nam,
                      Parameter_Associations => Parm);
               end if;

               Set_First_Named_Actual (Call, First_Named_Actual (Call_Node));
               Set_Etype (Call, Etype (D_T));

               --  We do not re-analyze the call to avoid infinite recursion.
               --  We analyze separately the prefix and the object, and set
               --  the checks on the prefix that would otherwise be emitted
               --  when resolving a call.

               Rewrite (Call_Node, Call);
               Analyze (Nam);
               Apply_Access_Check (Nam);
               Analyze (Obj);
               return;
            end;
         end if;
      end if;

      --  If this is a call to an intrinsic subprogram, then perform the
      --  appropriate expansion to the corresponding tree node and we
      --  are all done (since after that the call is gone).

      --  In the case where the intrinsic is to be processed by the back end,
      --  the call to Expand_Intrinsic_Call will do nothing, which is fine,
      --  since the idea in this case is to pass the call unchanged. If the
      --  intrinsic is an inherited unchecked conversion, and the derived type
      --  is the target type of the conversion, we must retain it as the return
      --  type of the expression. Otherwise the expansion below, which uses the
      --  parent operation, will yield the wrong type.

      if Is_Intrinsic_Subprogram (Subp) then
         Expand_Intrinsic_Call (Call_Node, Subp);

         if Nkind (Call_Node) = N_Unchecked_Type_Conversion
           and then Parent_Subp /= Orig_Subp
           and then Etype (Parent_Subp) /= Etype (Orig_Subp)
         then
            Set_Etype (Call_Node, Etype (Orig_Subp));
         end if;

         return;
      end if;

      if Ekind_In (Subp, E_Function, E_Procedure) then

         --  We perform two simple optimization on calls:

         --  a) replace calls to null procedures unconditionally;

         --  b) for To_Address, just do an unchecked conversion. Not only is
         --  this efficient, but it also avoids order of elaboration problems
         --  when address clauses are inlined (address expression elaborated
         --  at the wrong point).

         --  We perform these optimization regardless of whether we are in the
         --  main unit or in a unit in the context of the main unit, to ensure
         --  that tree generated is the same in both cases, for CodePeer use.

         if Is_RTE (Subp, RE_To_Address) then
            Rewrite (Call_Node,
              Unchecked_Convert_To
                (RTE (RE_Address), Relocate_Node (First_Actual (Call_Node))));
            return;

         elsif Is_Null_Procedure (Subp)  then
            Rewrite (Call_Node, Make_Null_Statement (Loc));
            return;
         end if;

         --  Handle inlining (old semantics)

         if Is_Inlined (Subp) and then not Debug_Flag_Dot_K then
            Inlined_Subprogram : declare
               Bod         : Node_Id;
               Must_Inline : Boolean := False;
               Spec        : constant Node_Id := Unit_Declaration_Node (Subp);

            begin
               --  Verify that the body to inline has already been seen, and
               --  that if the body is in the current unit the inlining does
               --  not occur earlier. This avoids order-of-elaboration problems
               --  in the back end.

               --  This should be documented in sinfo/einfo ???

               if No (Spec)
                 or else Nkind (Spec) /= N_Subprogram_Declaration
                 or else No (Body_To_Inline (Spec))
               then
                  Must_Inline := False;

               --  If this an inherited function that returns a private type,
               --  do not inline if the full view is an unconstrained array,
               --  because such calls cannot be inlined.

               elsif Present (Orig_Subp)
                 and then Is_Array_Type (Etype (Orig_Subp))
                 and then not Is_Constrained (Etype (Orig_Subp))
               then
                  Must_Inline := False;

               elsif In_Unfrozen_Instance (Scope (Subp)) then
                  Must_Inline := False;

               else
                  Bod := Body_To_Inline (Spec);

                  if (In_Extended_Main_Code_Unit (Call_Node)
                        or else In_Extended_Main_Code_Unit (Parent (Call_Node))
                        or else Has_Pragma_Inline_Always (Subp))
                    and then (not In_Same_Extended_Unit (Sloc (Bod), Loc)
                               or else
                                 Earlier_In_Extended_Unit (Sloc (Bod), Loc))
                  then
                     Must_Inline := True;

                  --  If we are compiling a package body that is not the main
                  --  unit, it must be for inlining/instantiation purposes,
                  --  in which case we inline the call to insure that the same
                  --  temporaries are generated when compiling the body by
                  --  itself. Otherwise link errors can occur.

                  --  If the function being called is itself in the main unit,
                  --  we cannot inline, because there is a risk of double
                  --  elaboration and/or circularity: the inlining can make
                  --  visible a private entity in the body of the main unit,
                  --  that gigi will see before its sees its proper definition.

                  elsif not (In_Extended_Main_Code_Unit (Call_Node))
                    and then In_Package_Body
                  then
                     Must_Inline := not In_Extended_Main_Source_Unit (Subp);
                  end if;
               end if;

               if Must_Inline then
                  Expand_Inlined_Call (Call_Node, Subp, Orig_Subp);

               else
                  --  Let the back end handle it

                  Add_Inlined_Body (Subp);

                  if Front_End_Inlining
                    and then Nkind (Spec) = N_Subprogram_Declaration
                    and then (In_Extended_Main_Code_Unit (Call_Node))
                    and then No (Body_To_Inline (Spec))
                    and then not Has_Completion (Subp)
                    and then In_Same_Extended_Unit (Sloc (Spec), Loc)
                  then
                     Cannot_Inline
                       ("cannot inline& (body not seen yet)?",
                        Call_Node, Subp);
                  end if;
               end if;
            end Inlined_Subprogram;

         --  Handle inlining (new semantics)

         elsif Is_Inlined (Subp) then
            declare
               Spec : constant Node_Id := Unit_Declaration_Node (Subp);

            begin
               if Must_Inline (Subp) then
                  if In_Extended_Main_Code_Unit (Call_Node)
                    and then In_Same_Extended_Unit (Sloc (Spec), Loc)
                    and then not Has_Completion (Subp)
                  then
                     Cannot_Inline
                       ("cannot inline& (body not seen yet)?",
                        Call_Node, Subp);

                  else
                     Do_Inline_Always (Subp, Orig_Subp);
                  end if;

               elsif Optimization_Level > 0 then
                  Do_Inline (Subp, Orig_Subp);
               end if;

               --  The call may have been inlined or may have been passed to
               --  the backend. No further action needed if it was inlined.

               if Nkind (N) /= N_Function_Call then
                  return;
               end if;
            end;
         end if;
      end if;

      --  Check for protected subprogram. This is either an intra-object call,
      --  or a protected function call. Protected procedure calls are rewritten
      --  as entry calls and handled accordingly.

      --  In Ada 2005, this may be an indirect call to an access parameter that
      --  is an access_to_subprogram. In that case the anonymous type has a
      --  scope that is a protected operation, but the call is a regular one.
      --  In either case do not expand call if subprogram is eliminated.

      Scop := Scope (Subp);

      if Nkind (Call_Node) /= N_Entry_Call_Statement
        and then Is_Protected_Type (Scop)
        and then Ekind (Subp) /= E_Subprogram_Type
        and then not Is_Eliminated (Subp)
      then
         --  If the call is an internal one, it is rewritten as a call to the
         --  corresponding unprotected subprogram.

         Expand_Protected_Subprogram_Call (Call_Node, Subp, Scop);
      end if;

      --  Functions returning controlled objects need special attention. If
      --  the return type is limited, then the context is initialization and
      --  different processing applies. If the call is to a protected function,
      --  the expansion above will call Expand_Call recursively. Otherwise the
      --  function call is transformed into a temporary which obtains the
      --  result from the secondary stack.

      if Needs_Finalization (Etype (Subp)) then
         if not Is_Limited_View (Etype (Subp))
           and then
             (No (First_Formal (Subp))
                or else
                  not Is_Concurrent_Record_Type (Etype (First_Formal (Subp))))
         then
            Expand_Ctrl_Function_Call (Call_Node);

         --  Build-in-place function calls which appear in anonymous contexts
         --  need a transient scope to ensure the proper finalization of the
         --  intermediate result after its use.

         elsif Is_Build_In_Place_Function_Call (Call_Node)
           and then
             Nkind_In (Parent (Call_Node), N_Attribute_Reference,
                                           N_Function_Call,
                                           N_Indexed_Component,
                                           N_Object_Renaming_Declaration,
                                           N_Procedure_Call_Statement,
                                           N_Selected_Component,
                                           N_Slice)
         then
            Establish_Transient_Scope (Call_Node, Sec_Stack => True);
         end if;
      end if;

      --  Test for First_Optional_Parameter, and if so, truncate parameter list
      --  if there are optional parameters at the trailing end.
      --  Note: we never delete procedures for call via a pointer.

      if (Ekind (Subp) = E_Procedure or else Ekind (Subp) = E_Function)
        and then Present (First_Optional_Parameter (Subp))
      then
         declare
            Last_Keep_Arg : Node_Id;

         begin
            --  Last_Keep_Arg will hold the last actual that should be kept.
            --  If it remains empty at the end, it means that all parameters
            --  are optional.

            Last_Keep_Arg := Empty;

            --  Find first optional parameter, must be present since we checked
            --  the validity of the parameter before setting it.

            Formal := First_Formal (Subp);
            Actual := First_Actual (Call_Node);
            while Formal /= First_Optional_Parameter (Subp) loop
               Last_Keep_Arg := Actual;
               Next_Formal (Formal);
               Next_Actual (Actual);
            end loop;

            --  We have Formal and Actual pointing to the first potentially
            --  droppable argument. We can drop all the trailing arguments
            --  whose actual matches the default. Note that we know that all
            --  remaining formals have defaults, because we checked that this
            --  requirement was met before setting First_Optional_Parameter.

            --  We use Fully_Conformant_Expressions to check for identity
            --  between formals and actuals, which may miss some cases, but
            --  on the other hand, this is only an optimization (if we fail
            --  to truncate a parameter it does not affect functionality).
            --  So if the default is 3 and the actual is 1+2, we consider
            --  them unequal, which hardly seems worrisome.

            while Present (Formal) loop
               if not Fully_Conformant_Expressions
                    (Actual, Default_Value (Formal))
               then
                  Last_Keep_Arg := Actual;
               end if;

               Next_Formal (Formal);
               Next_Actual (Actual);
            end loop;

            --  If no arguments, delete entire list, this is the easy case

            if No (Last_Keep_Arg) then
               Set_Parameter_Associations (Call_Node, No_List);
               Set_First_Named_Actual (Call_Node, Empty);

            --  Case where at the last retained argument is positional. This
            --  is also an easy case, since the retained arguments are already
            --  in the right form, and we don't need to worry about the order
            --  of arguments that get eliminated.

            elsif Is_List_Member (Last_Keep_Arg) then
               while Present (Next (Last_Keep_Arg)) loop
                  Discard_Node (Remove_Next (Last_Keep_Arg));
               end loop;

               Set_First_Named_Actual (Call_Node, Empty);

            --  This is the annoying case where the last retained argument
            --  is a named parameter. Since the original arguments are not
            --  in declaration order, we may have to delete some fairly
            --  random collection of arguments.

            else
               declare
                  Temp   : Node_Id;
                  Passoc : Node_Id;

               begin
                  --  First step, remove all the named parameters from the
                  --  list (they are still chained using First_Named_Actual
                  --  and Next_Named_Actual, so we have not lost them).

                  Temp := First (Parameter_Associations (Call_Node));

                  --  Case of all parameters named, remove them all

                  if Nkind (Temp) = N_Parameter_Association then
                     --  Suppress warnings to avoid warning on possible
                     --  infinite loop (because Call_Node is not modified).

                     pragma Warnings (Off);
                     while Is_Non_Empty_List
                             (Parameter_Associations (Call_Node))
                     loop
                        Temp :=
                          Remove_Head (Parameter_Associations (Call_Node));
                     end loop;
                     pragma Warnings (On);

                  --  Case of mixed positional/named, remove named parameters

                  else
                     while Nkind (Next (Temp)) /= N_Parameter_Association loop
                        Next (Temp);
                     end loop;

                     while Present (Next (Temp)) loop
                        Remove (Next (Temp));
                     end loop;
                  end if;

                  --  Now we loop through the named parameters, till we get
                  --  to the last one to be retained, adding them to the list.
                  --  Note that the Next_Named_Actual list does not need to be
                  --  touched since we are only reordering them on the actual
                  --  parameter association list.

                  Passoc := Parent (First_Named_Actual (Call_Node));
                  loop
                     Temp := Relocate_Node (Passoc);
                     Append_To
                       (Parameter_Associations (Call_Node), Temp);
                     exit when
                       Last_Keep_Arg = Explicit_Actual_Parameter (Passoc);
                     Passoc := Parent (Next_Named_Actual (Passoc));
                  end loop;

                  Set_Next_Named_Actual (Temp, Empty);

                  loop
                     Temp := Next_Named_Actual (Passoc);
                     exit when No (Temp);
                     Set_Next_Named_Actual
                       (Passoc, Next_Named_Actual (Parent (Temp)));
                  end loop;
               end;

            end if;
         end;
      end if;
   end Expand_Call;

   ---------------------------
   -- Expand_Contract_Cases --
   ---------------------------

   --  Pragma Contract_Cases is expanded in the following manner:

   --    subprogram S is
   --       Flag_1   : Boolean := False;
   --       . . .
   --       Flag_N   : Boolean := False;
   --       Flag_N+1 : Boolean := False;  --  when "others" present
   --       Count    : Natural := 0;

   --       <preconditions (if any)>

   --       if Case_Guard_1 then
   --          Flag_1 := True;
   --          Count  := Count + 1;
   --       end if;
   --       . . .
   --       if Case_Guard_N then
   --          Flag_N := True;
   --          Count  := Count + 1;
   --       end if;

   --       if Count = 0 then
   --          raise Assertion_Error with "xxx contract cases incomplete";
   --            <or>
   --          Flag_N+1 := True;  --  when "others" present

   --       elsif Count > 1 then
   --          declare
   --             Str0 : constant String :=
   --                      "contract cases overlap for subprogram ABC";
   --             Str1 : constant String :=
   --                      (if Flag_1 then
   --                         Str0 & "case guard at xxx evaluates to True"
   --                       else Str0);
   --             StrN : constant String :=
   --                      (if Flag_N then
   --                         StrN-1 & "case guard at xxx evaluates to True"
   --                       else StrN-1);
   --          begin
   --             raise Assertion_Error with StrN;
   --          end;
   --       end if;

   --       procedure _Postconditions is
   --       begin
   --          <postconditions (if any)>

   --          if Flag_1 and then not Consequence_1 then
   --             raise Assertion_Error with "failed contract case at xxx";
   --          end if;
   --          . . .
   --          if Flag_N[+1] and then not Consequence_N[+1] then
   --             raise Assertion_Error with "failed contract case at xxx";
   --          end if;
   --       end _Postconditions;
   --    begin
   --       . . .
   --    end S;

   procedure Expand_Contract_Cases
     (CCs     : Node_Id;
      Subp_Id : Entity_Id;
      Decls   : List_Id;
      Stmts   : in out List_Id)
   is
      Loc : constant Source_Ptr := Sloc (CCs);

      procedure Case_Guard_Error
        (Decls     : List_Id;
         Flag      : Entity_Id;
         Error_Loc : Source_Ptr;
         Msg       : in out Entity_Id);
      --  Given a declarative list Decls, status flag Flag, the location of the
      --  error and a string Msg, construct the following check:
      --    Msg : constant String :=
      --            (if Flag then
      --                Msg & "case guard at Error_Loc evaluates to True"
      --             else Msg);
      --  The resulting code is added to Decls

      procedure Consequence_Error
        (Checks : in out Node_Id;
         Flag   : Entity_Id;
         Conseq : Node_Id);
      --  Given an if statement Checks, status flag Flag and a consequence
      --  Conseq, construct the following check:
      --    [els]if Flag and then not Conseq then
      --       raise Assertion_Error
      --         with "failed contract case at Sloc (Conseq)";
      --    [end if;]
      --  The resulting code is added to Checks

      function Declaration_Of (Id : Entity_Id) return Node_Id;
      --  Given the entity Id of a boolean flag, generate:
      --    Id : Boolean := False;

      function Increment (Id : Entity_Id) return Node_Id;
      --  Given the entity Id of a numerical variable, generate:
      --    Id := Id + 1;

      function Set (Id : Entity_Id) return Node_Id;
      --  Given the entity Id of a boolean variable, generate:
      --    Id := True;

      ----------------------
      -- Case_Guard_Error --
      ----------------------

      procedure Case_Guard_Error
        (Decls     : List_Id;
         Flag      : Entity_Id;
         Error_Loc : Source_Ptr;
         Msg       : in out Entity_Id)
      is
         New_Line : constant Character := Character'Val (10);
         New_Msg  : constant Entity_Id := Make_Temporary (Loc, 'S');

      begin
         Start_String;
         Store_String_Char  (New_Line);
         Store_String_Chars ("  case guard at ");
         Store_String_Chars (Build_Location_String (Error_Loc));
         Store_String_Chars (" evaluates to True");

         --  Generate:
         --    New_Msg : constant String :=
         --      (if Flag then
         --          Msg & "case guard at Error_Loc evaluates to True"
         --       else Msg);

         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => New_Msg,
             Constant_Present    => True,
             Object_Definition   => New_Reference_To (Standard_String, Loc),
             Expression          =>
               Make_If_Expression (Loc,
                 Expressions => New_List (
                   New_Reference_To (Flag, Loc),

                   Make_Op_Concat (Loc,
                     Left_Opnd  => New_Reference_To (Msg, Loc),
                     Right_Opnd => Make_String_Literal (Loc, End_String)),

                   New_Reference_To (Msg, Loc)))));

         Msg := New_Msg;
      end Case_Guard_Error;

      -----------------------
      -- Consequence_Error --
      -----------------------

      procedure Consequence_Error
        (Checks : in out Node_Id;
         Flag   : Entity_Id;
         Conseq : Node_Id)
      is
         Cond  : Node_Id;
         Error : Node_Id;

      begin
         --  Generate:
         --    Flag and then not Conseq

         Cond :=
           Make_And_Then (Loc,
             Left_Opnd  => New_Reference_To (Flag, Loc),
             Right_Opnd =>
               Make_Op_Not (Loc,
                 Right_Opnd => Relocate_Node (Conseq)));

         --  Generate:
         --    raise Assertion_Error
         --      with "failed contract case at Sloc (Conseq)";

         Start_String;
         Store_String_Chars ("failed contract case at ");
         Store_String_Chars (Build_Location_String (Sloc (Conseq)));

         Error :=
           Make_Procedure_Call_Statement (Loc,
             Name                   =>
               New_Reference_To (RTE (RE_Raise_Assert_Failure), Loc),
             Parameter_Associations => New_List (
               Make_String_Literal (Loc, End_String)));

         if No (Checks) then
            Checks :=
              Make_Implicit_If_Statement (CCs,
                Condition       => Cond,
                Then_Statements => New_List (Error));

         else
            if No (Elsif_Parts (Checks)) then
               Set_Elsif_Parts (Checks, New_List);
            end if;

            Append_To (Elsif_Parts (Checks),
              Make_Elsif_Part (Loc,
                Condition       => Cond,
                Then_Statements => New_List (Error)));
         end if;
      end Consequence_Error;

      --------------------
      -- Declaration_Of --
      --------------------

      function Declaration_Of (Id : Entity_Id) return Node_Id is
      begin
         return
           Make_Object_Declaration (Loc,
             Defining_Identifier => Id,
             Object_Definition   => New_Reference_To (Standard_Boolean, Loc),
             Expression          => New_Reference_To (Standard_False, Loc));
      end Declaration_Of;

      ---------------
      -- Increment --
      ---------------

      function Increment (Id : Entity_Id) return Node_Id is
      begin
         return
           Make_Assignment_Statement (Loc,
             Name       => New_Reference_To (Id, Loc),
             Expression =>
               Make_Op_Add (Loc,
                 Left_Opnd  => New_Reference_To (Id, Loc),
                 Right_Opnd => Make_Integer_Literal (Loc, 1)));
      end Increment;

      ---------
      -- Set --
      ---------

      function Set (Id : Entity_Id) return Node_Id is
      begin
         return
           Make_Assignment_Statement (Loc,
             Name       => New_Reference_To (Id, Loc),
             Expression => New_Reference_To (Standard_True, Loc));
      end Set;

      --  Local variables

      Aggr          : constant Node_Id :=
                        Expression (First
                          (Pragma_Argument_Associations (CCs)));
      Case_Guard    : Node_Id;
      CG_Checks     : Node_Id;
      CG_Stmts      : List_Id;
      Conseq        : Node_Id;
      Conseq_Checks : Node_Id := Empty;
      Count         : Entity_Id;
      Error_Decls   : List_Id;
      Flag          : Entity_Id;
      Msg_Str       : Entity_Id;
      Multiple_PCs  : Boolean;
      Others_Flag   : Entity_Id := Empty;
      Post_Case     : Node_Id;

   --  Start of processing for Expand_Contract_Cases

   begin
      --  Do nothing if pragma is not enabled. If pragma is disabled, it has
      --  already been rewritten as a Null statement.

      if Is_Ignored (CCs) then
         return;

      --  Guard against malformed contract cases

      elsif Nkind (Aggr) /= N_Aggregate then
         return;
      end if;

      Multiple_PCs := List_Length (Component_Associations (Aggr)) > 1;

      --  Create the counter which tracks the number of case guards that
      --  evaluate to True.

      --    Count : Natural := 0;

      Count := Make_Temporary (Loc, 'C');

      Prepend_To (Decls,
        Make_Object_Declaration (Loc,
          Defining_Identifier => Count,
          Object_Definition   => New_Reference_To (Standard_Natural, Loc),
          Expression          => Make_Integer_Literal (Loc, 0)));

      --  Create the base error message for multiple overlapping case guards

      --    Msg_Str : constant String :=
      --                "contract cases overlap for subprogram Subp_Id";

      if Multiple_PCs then
         Msg_Str := Make_Temporary (Loc, 'S');

         Start_String;
         Store_String_Chars ("contract cases overlap for subprogram ");
         Store_String_Chars (Get_Name_String (Chars (Subp_Id)));

         Error_Decls := New_List (
           Make_Object_Declaration (Loc,
             Defining_Identifier => Msg_Str,
             Constant_Present    => True,
             Object_Definition   => New_Reference_To (Standard_String, Loc),
             Expression          => Make_String_Literal (Loc, End_String)));
      end if;

      --  Process individual post cases

      Post_Case := First (Component_Associations (Aggr));
      while Present (Post_Case) loop
         Case_Guard := First (Choices (Post_Case));
         Conseq     := Expression (Post_Case);

         --  The "others" choice requires special processing

         if Nkind (Case_Guard) = N_Others_Choice then
            Others_Flag := Make_Temporary (Loc, 'F');
            Prepend_To (Decls, Declaration_Of (Others_Flag));

            --  Check possible overlap between a case guard and "others"

            if Multiple_PCs and Exception_Extra_Info then
               Case_Guard_Error
                 (Decls     => Error_Decls,
                  Flag      => Others_Flag,
                  Error_Loc => Sloc (Case_Guard),
                  Msg       => Msg_Str);
            end if;

            --  Check the corresponding consequence of "others"

            Consequence_Error
              (Checks => Conseq_Checks,
               Flag   => Others_Flag,
               Conseq => Conseq);

         --  Regular post case

         else
            --  Create the flag which tracks the state of its associated case
            --  guard.

            Flag := Make_Temporary (Loc, 'F');
            Prepend_To (Decls, Declaration_Of (Flag));

            --  The flag is set when the case guard is evaluated to True
            --    if Case_Guard then
            --       Flag  := True;
            --       Count := Count + 1;
            --    end if;

            Append_To (Decls,
              Make_Implicit_If_Statement (CCs,
                Condition       => Relocate_Node (Case_Guard),
                Then_Statements => New_List (
                  Set (Flag),
                  Increment (Count))));

            --  Check whether this case guard overlaps with another one

            if Multiple_PCs and Exception_Extra_Info then
               Case_Guard_Error
                 (Decls     => Error_Decls,
                  Flag      => Flag,
                  Error_Loc => Sloc (Case_Guard),
                  Msg       => Msg_Str);
            end if;

            --  The corresponding consequence of the case guard which evaluated
            --  to True must hold on exit from the subprogram.

            Consequence_Error
              (Checks => Conseq_Checks,
               Flag   => Flag,
               Conseq => Conseq);
         end if;

         Next (Post_Case);
      end loop;

      --  Raise Assertion_Error when none of the case guards evaluate to True.
      --  The only exception is when we have "others", in which case there is
      --  no error because "others" acts as a default True.

      --  Generate:
      --    Flag := True;

      if Present (Others_Flag) then
         CG_Stmts := New_List (Set (Others_Flag));

      --  Generate:
      --    raise Assertion_Error with "xxx contract cases incomplete";

      else
         Start_String;
         Store_String_Chars (Build_Location_String (Loc));
         Store_String_Chars (" contract cases incomplete");

         CG_Stmts := New_List (
           Make_Procedure_Call_Statement (Loc,
             Name                   =>
               New_Reference_To (RTE (RE_Raise_Assert_Failure), Loc),
             Parameter_Associations => New_List (
               Make_String_Literal (Loc, End_String))));
      end if;

      CG_Checks :=
        Make_Implicit_If_Statement (CCs,
          Condition       =>
            Make_Op_Eq (Loc,
              Left_Opnd  => New_Reference_To (Count, Loc),
              Right_Opnd => Make_Integer_Literal (Loc, 0)),
          Then_Statements => CG_Stmts);

      --  Detect a possible failure due to several case guards evaluating to
      --  True.

      --  Generate:
      --    elsif Count > 0 then
      --       declare
      --          <Error_Decls>
      --       begin
      --          raise Assertion_Error with <Msg_Str>;
      --    end if;

      if Multiple_PCs then
         Set_Elsif_Parts (CG_Checks, New_List (
           Make_Elsif_Part (Loc,
             Condition       =>
               Make_Op_Gt (Loc,
                 Left_Opnd  => New_Reference_To (Count, Loc),
                 Right_Opnd => Make_Integer_Literal (Loc, 1)),

             Then_Statements => New_List (
               Make_Block_Statement (Loc,
                 Declarations               => Error_Decls,
                 Handled_Statement_Sequence =>
                   Make_Handled_Sequence_Of_Statements (Loc,
                     Statements => New_List (
                       Make_Procedure_Call_Statement (Loc,
                         Name                   =>
                           New_Reference_To
                             (RTE (RE_Raise_Assert_Failure), Loc),
                         Parameter_Associations => New_List (
                           New_Reference_To (Msg_Str, Loc))))))))));
      end if;

      Append_To (Decls, CG_Checks);

      --  Raise Assertion_Error when the corresponding consequence of a case
      --  guard that evaluated to True fails.

      if No (Stmts) then
         Stmts := New_List;
      end if;

      Append_To (Stmts, Conseq_Checks);
   end Expand_Contract_Cases;

   -------------------------------
   -- Expand_Ctrl_Function_Call --
   -------------------------------

   procedure Expand_Ctrl_Function_Call (N : Node_Id) is
   begin
      --  Optimization, if the returned value (which is on the sec-stack) is
      --  returned again, no need to copy/readjust/finalize, we can just pass
      --  the value thru (see Expand_N_Simple_Return_Statement), and thus no
      --  attachment is needed

      if Nkind (Parent (N)) = N_Simple_Return_Statement then
         return;
      end if;

      --  Resolution is now finished, make sure we don't start analysis again
      --  because of the duplication.

      Set_Analyzed (N);

      --  A function which returns a controlled object uses the secondary
      --  stack. Rewrite the call into a temporary which obtains the result of
      --  the function using 'reference.

      Remove_Side_Effects (N);

      --  When the temporary function result appears inside a case or an if
      --  expression, its lifetime must be extended to match that of the
      --  context. If not, the function result would be finalized prematurely
      --  and the evaluation of the expression could yield the wrong result.

      if Within_Case_Or_If_Expression (N)
        and then Nkind (N) = N_Explicit_Dereference
      then
         Set_Is_Processed_Transient (Entity (Prefix (N)));
      end if;
   end Expand_Ctrl_Function_Call;

   -------------------------
   -- Expand_Inlined_Call --
   -------------------------

   procedure Expand_Inlined_Call
    (N         : Node_Id;
     Subp      : Entity_Id;
     Orig_Subp : Entity_Id)
   is
      Loc       : constant Source_Ptr := Sloc (N);
      Is_Predef : constant Boolean :=
                   Is_Predefined_File_Name
                     (Unit_File_Name (Get_Source_Unit (Subp)));
      Orig_Bod  : constant Node_Id :=
                    Body_To_Inline (Unit_Declaration_Node (Subp));

      Blk      : Node_Id;
      Decl     : Node_Id;
      Decls    : constant List_Id := New_List;
      Exit_Lab : Entity_Id := Empty;
      F        : Entity_Id;
      A        : Node_Id;
      Lab_Decl : Node_Id;
      Lab_Id   : Node_Id;
      New_A    : Node_Id;
      Num_Ret  : Int := 0;
      Ret_Type : Entity_Id;

      Targ : Node_Id;
      --  The target of the call. If context is an assignment statement then
      --  this is the left-hand side of the assignment, else it is a temporary
      --  to which the return value is assigned prior to rewriting the call.

      Targ1 : Node_Id;
      --  A separate target used when the return type is unconstrained

      Temp     : Entity_Id;
      Temp_Typ : Entity_Id;

      Return_Object : Entity_Id := Empty;
      --  Entity in declaration in an extended_return_statement

      Is_Unc      : Boolean;
      Is_Unc_Decl : Boolean;
      --  If the type returned by the function is unconstrained and the call
      --  can be inlined, special processing is required.

      procedure Make_Exit_Label;
      --  Build declaration for exit label to be used in Return statements,
      --  sets Exit_Lab (the label node) and Lab_Decl (corresponding implicit
      --  declaration). Does nothing if Exit_Lab already set.

      function Process_Formals (N : Node_Id) return Traverse_Result;
      --  Replace occurrence of a formal with the corresponding actual, or the
      --  thunk generated for it. Replace a return statement with an assignment
      --  to the target of the call, with appropriate conversions if needed.

      function Process_Sloc (Nod : Node_Id) return Traverse_Result;
      --  If the call being expanded is that of an internal subprogram, set the
      --  sloc of the generated block to that of the call itself, so that the
      --  expansion is skipped by the "next" command in gdb.
      --  Same processing for a subprogram in a predefined file, e.g.
      --  Ada.Tags. If Debug_Generated_Code is true, suppress this change to
      --  simplify our own development.

      procedure Reset_Dispatching_Calls (N : Node_Id);
      --  In subtree N search for occurrences of dispatching calls that use the
      --  Ada 2005 Object.Operation notation and the object is a formal of the
      --  inlined subprogram. Reset the entity associated with Operation in all
      --  the found occurrences.

      procedure Rewrite_Function_Call (N : Node_Id; Blk : Node_Id);
      --  If the function body is a single expression, replace call with
      --  expression, else insert block appropriately.

      procedure Rewrite_Procedure_Call (N : Node_Id; Blk : Node_Id);
      --  If procedure body has no local variables, inline body without
      --  creating block, otherwise rewrite call with block.

      function Formal_Is_Used_Once (Formal : Entity_Id) return Boolean;
      --  Determine whether a formal parameter is used only once in Orig_Bod

      ---------------------
      -- Make_Exit_Label --
      ---------------------

      procedure Make_Exit_Label is
         Lab_Ent : Entity_Id;
      begin
         if No (Exit_Lab) then
            Lab_Ent := Make_Temporary (Loc, 'L');
            Lab_Id  := New_Reference_To (Lab_Ent, Loc);
            Exit_Lab := Make_Label (Loc, Lab_Id);
            Lab_Decl :=
              Make_Implicit_Label_Declaration (Loc,
                Defining_Identifier  => Lab_Ent,
                Label_Construct      => Exit_Lab);
         end if;
      end Make_Exit_Label;

      ---------------------
      -- Process_Formals --
      ---------------------

      function Process_Formals (N : Node_Id) return Traverse_Result is
         A   : Entity_Id;
         E   : Entity_Id;
         Ret : Node_Id;

      begin
         if Is_Entity_Name (N) and then Present (Entity (N)) then
            E := Entity (N);

            if Is_Formal (E) and then Scope (E) = Subp then
               A := Renamed_Object (E);

               --  Rewrite the occurrence of the formal into an occurrence of
               --  the actual. Also establish visibility on the proper view of
               --  the actual's subtype for the body's context (if the actual's
               --  subtype is private at the call point but its full view is
               --  visible to the body, then the inlined tree here must be
               --  analyzed with the full view).

               if Is_Entity_Name (A) then
                  Rewrite (N, New_Occurrence_Of (Entity (A), Loc));
                  Check_Private_View (N);

               elsif Nkind (A) = N_Defining_Identifier then
                  Rewrite (N, New_Occurrence_Of (A, Loc));
                  Check_Private_View (N);

               --  Numeric literal

               else
                  Rewrite (N, New_Copy (A));
               end if;
            end if;

            return Skip;

         elsif Is_Entity_Name (N)
           and then Present (Return_Object)
           and then Chars (N) = Chars (Return_Object)
         then
            --  Occurrence within an extended return statement. The return
            --  object is local to the body been inlined, and thus the generic
            --  copy is not analyzed yet, so we match by name, and replace it
            --  with target of call.

            if Nkind (Targ) = N_Defining_Identifier then
               Rewrite (N, New_Occurrence_Of (Targ, Loc));
            else
               Rewrite (N, New_Copy_Tree (Targ));
            end if;

            return Skip;

         elsif Nkind (N) = N_Simple_Return_Statement then
            if No (Expression (N)) then
               Make_Exit_Label;
               Rewrite (N,
                 Make_Goto_Statement (Loc, Name => New_Copy (Lab_Id)));

            else
               if Nkind (Parent (N)) = N_Handled_Sequence_Of_Statements
                 and then Nkind (Parent (Parent (N))) = N_Subprogram_Body
               then
                  --  Function body is a single expression. No need for
                  --  exit label.

                  null;

               else
                  Num_Ret := Num_Ret + 1;
                  Make_Exit_Label;
               end if;

               --  Because of the presence of private types, the views of the
               --  expression and the context may be different, so place an
               --  unchecked conversion to the context type to avoid spurious
               --  errors, e.g. when the expression is a numeric literal and
               --  the context is private. If the expression is an aggregate,
               --  use a qualified expression, because an aggregate is not a
               --  legal argument of a conversion. Ditto for numeric literals,
               --  which must be resolved to a specific type.

               if Nkind_In (Expression (N), N_Aggregate,
                                            N_Null,
                                            N_Real_Literal,
                                            N_Integer_Literal)
               then
                  Ret :=
                    Make_Qualified_Expression (Sloc (N),
                      Subtype_Mark => New_Occurrence_Of (Ret_Type, Sloc (N)),
                      Expression => Relocate_Node (Expression (N)));
               else
                  Ret :=
                    Unchecked_Convert_To
                      (Ret_Type, Relocate_Node (Expression (N)));
               end if;

               if Nkind (Targ) = N_Defining_Identifier then
                  Rewrite (N,
                    Make_Assignment_Statement (Loc,
                      Name       => New_Occurrence_Of (Targ, Loc),
                      Expression => Ret));
               else
                  Rewrite (N,
                    Make_Assignment_Statement (Loc,
                      Name       => New_Copy (Targ),
                      Expression => Ret));
               end if;

               Set_Assignment_OK (Name (N));

               if Present (Exit_Lab) then
                  Insert_After (N,
                    Make_Goto_Statement (Loc, Name => New_Copy (Lab_Id)));
               end if;
            end if;

            return OK;

         --  An extended return becomes a block whose first statement is the
         --  assignment of the initial expression of the return object to the
         --  target of the call itself.

         elsif Nkind (N) = N_Extended_Return_Statement then
            declare
               Return_Decl : constant Entity_Id :=
                               First (Return_Object_Declarations (N));
               Assign      : Node_Id;

            begin
               Return_Object := Defining_Identifier (Return_Decl);

               if Present (Expression (Return_Decl)) then
                  if Nkind (Targ) = N_Defining_Identifier then
                     Assign :=
                       Make_Assignment_Statement (Loc,
                         Name       => New_Occurrence_Of (Targ, Loc),
                         Expression => Expression (Return_Decl));
                  else
                     Assign :=
                       Make_Assignment_Statement (Loc,
                         Name       => New_Copy (Targ),
                         Expression => Expression (Return_Decl));
                  end if;

                  Set_Assignment_OK (Name (Assign));

                  if No (Handled_Statement_Sequence (N)) then
                     Set_Handled_Statement_Sequence (N,
                       Make_Handled_Sequence_Of_Statements (Loc,
                         Statements => New_List));
                  end if;

                  Prepend (Assign,
                    Statements (Handled_Statement_Sequence (N)));
               end if;

               Rewrite (N,
                 Make_Block_Statement (Loc,
                    Handled_Statement_Sequence =>
                      Handled_Statement_Sequence (N)));

               return OK;
            end;

         --  Remove pragma Unreferenced since it may refer to formals that
         --  are not visible in the inlined body, and in any case we will
         --  not be posting warnings on the inlined body so it is unneeded.

         elsif Nkind (N) = N_Pragma
           and then Pragma_Name (N) = Name_Unreferenced
         then
            Rewrite (N, Make_Null_Statement (Sloc (N)));
            return OK;

         else
            return OK;
         end if;
      end Process_Formals;

      procedure Replace_Formals is new Traverse_Proc (Process_Formals);

      ------------------
      -- Process_Sloc --
      ------------------

      function Process_Sloc (Nod : Node_Id) return Traverse_Result is
      begin
         if not Debug_Generated_Code then
            Set_Sloc (Nod, Sloc (N));
            Set_Comes_From_Source (Nod, False);
         end if;

         return OK;
      end Process_Sloc;

      procedure Reset_Slocs is new Traverse_Proc (Process_Sloc);

      ------------------------------
      --  Reset_Dispatching_Calls --
      ------------------------------

      procedure Reset_Dispatching_Calls (N : Node_Id) is

         function Do_Reset (N : Node_Id) return Traverse_Result;
         --  Comment required ???

         --------------
         -- Do_Reset --
         --------------

         function Do_Reset (N : Node_Id) return Traverse_Result is
         begin
            if Nkind (N) = N_Procedure_Call_Statement
              and then Nkind (Name (N)) = N_Selected_Component
              and then Nkind (Prefix (Name (N))) = N_Identifier
              and then Is_Formal (Entity (Prefix (Name (N))))
              and then Is_Dispatching_Operation
                         (Entity (Selector_Name (Name (N))))
            then
               Set_Entity (Selector_Name (Name (N)), Empty);
            end if;

            return OK;
         end Do_Reset;

         function Do_Reset_Calls is new Traverse_Func (Do_Reset);

         --  Local variables

         Dummy : constant Traverse_Result := Do_Reset_Calls (N);
         pragma Unreferenced (Dummy);

         --  Start of processing for Reset_Dispatching_Calls

      begin
         null;
      end Reset_Dispatching_Calls;

      ---------------------------
      -- Rewrite_Function_Call --
      ---------------------------

      procedure Rewrite_Function_Call (N : Node_Id; Blk : Node_Id) is
         HSS : constant Node_Id := Handled_Statement_Sequence (Blk);
         Fst : constant Node_Id := First (Statements (HSS));

      begin
         --  Optimize simple case: function body is a single return statement,
         --  which has been expanded into an assignment.

         if Is_Empty_List (Declarations (Blk))
           and then Nkind (Fst) = N_Assignment_Statement
           and then No (Next (Fst))
         then
            --  The function call may have been rewritten as the temporary
            --  that holds the result of the call, in which case remove the
            --  now useless declaration.

            if Nkind (N) = N_Identifier
              and then Nkind (Parent (Entity (N))) = N_Object_Declaration
            then
               Rewrite (Parent (Entity (N)), Make_Null_Statement (Loc));
            end if;

            Rewrite (N, Expression (Fst));

         elsif Nkind (N) = N_Identifier
           and then Nkind (Parent (Entity (N))) = N_Object_Declaration
         then
            --  The block assigns the result of the call to the temporary

            Insert_After (Parent (Entity (N)), Blk);

         --  If the context is an assignment, and the left-hand side is free of
         --  side-effects, the replacement is also safe.
         --  Can this be generalized further???

         elsif Nkind (Parent (N)) = N_Assignment_Statement
           and then
            (Is_Entity_Name (Name (Parent (N)))
              or else
                (Nkind (Name (Parent (N))) = N_Explicit_Dereference
                  and then Is_Entity_Name (Prefix (Name (Parent (N)))))

              or else
                (Nkind (Name (Parent (N))) = N_Selected_Component
                  and then Is_Entity_Name (Prefix (Name (Parent (N))))))
         then
            --  Replace assignment with the block

            declare
               Original_Assignment : constant Node_Id := Parent (N);

            begin
               --  Preserve the original assignment node to keep the complete
               --  assignment subtree consistent enough for Analyze_Assignment
               --  to proceed (specifically, the original Lhs node must still
               --  have an assignment statement as its parent).

               --  We cannot rely on Original_Node to go back from the block
               --  node to the assignment node, because the assignment might
               --  already be a rewrite substitution.

               Discard_Node (Relocate_Node (Original_Assignment));
               Rewrite (Original_Assignment, Blk);
            end;

         elsif Nkind (Parent (N)) = N_Object_Declaration then

            --  A call to a function which returns an unconstrained type
            --  found in the expression initializing an object-declaration is
            --  expanded into a procedure call which must be added after the
            --  object declaration.

            if Is_Unc_Decl and then Debug_Flag_Dot_K then
               Insert_Action_After (Parent (N), Blk);
            else
               Set_Expression (Parent (N), Empty);
               Insert_After (Parent (N), Blk);
            end if;

         elsif Is_Unc and then not Debug_Flag_Dot_K then
            Insert_Before (Parent (N), Blk);
         end if;
      end Rewrite_Function_Call;

      ----------------------------
      -- Rewrite_Procedure_Call --
      ----------------------------

      procedure Rewrite_Procedure_Call (N : Node_Id; Blk : Node_Id) is
         HSS  : constant Node_Id := Handled_Statement_Sequence (Blk);

      begin
         --  If there is a transient scope for N, this will be the scope of the
         --  actions for N, and the statements in Blk need to be within this
         --  scope. For example, they need to have visibility on the constant
         --  declarations created for the formals.

         --  If N needs no transient scope, and if there are no declarations in
         --  the inlined body, we can do a little optimization and insert the
         --  statements for the body directly after N, and rewrite N to a
         --  null statement, instead of rewriting N into a full-blown block
         --  statement.

         if not Scope_Is_Transient
           and then Is_Empty_List (Declarations (Blk))
         then
            Insert_List_After (N, Statements (HSS));
            Rewrite (N, Make_Null_Statement (Loc));
         else
            Rewrite (N, Blk);
         end if;
      end Rewrite_Procedure_Call;

      -------------------------
      -- Formal_Is_Used_Once --
      -------------------------

      function Formal_Is_Used_Once (Formal : Entity_Id) return Boolean is
         Use_Counter : Int := 0;

         function Count_Uses (N : Node_Id) return Traverse_Result;
         --  Traverse the tree and count the uses of the formal parameter.
         --  In this case, for optimization purposes, we do not need to
         --  continue the traversal once more than one use is encountered.

         ----------------
         -- Count_Uses --
         ----------------

         function Count_Uses (N : Node_Id) return Traverse_Result is
         begin
            --  The original node is an identifier

            if Nkind (N) = N_Identifier
              and then Present (Entity (N))

               --  Original node's entity points to the one in the copied body

              and then Nkind (Entity (N)) = N_Identifier
              and then Present (Entity (Entity (N)))

               --  The entity of the copied node is the formal parameter

              and then Entity (Entity (N)) = Formal
            then
               Use_Counter := Use_Counter + 1;

               if Use_Counter > 1 then

                  --  Denote more than one use and abandon the traversal

                  Use_Counter := 2;
                  return Abandon;

               end if;
            end if;

            return OK;
         end Count_Uses;

         procedure Count_Formal_Uses is new Traverse_Proc (Count_Uses);

      --  Start of processing for Formal_Is_Used_Once

      begin
         Count_Formal_Uses (Orig_Bod);
         return Use_Counter = 1;
      end Formal_Is_Used_Once;

   --  Start of processing for Expand_Inlined_Call

   begin
      --  Initializations for old/new semantics

      if not Debug_Flag_Dot_K then
         Is_Unc      := Is_Array_Type (Etype (Subp))
                          and then not Is_Constrained (Etype (Subp));
         Is_Unc_Decl := False;
      else
         Is_Unc      := Returns_Unconstrained_Type (Subp)
                          and then Optimization_Level > 0;
         Is_Unc_Decl := Nkind (Parent (N)) = N_Object_Declaration
                          and then Is_Unc;
      end if;

      --  Check for an illegal attempt to inline a recursive procedure. If the
      --  subprogram has parameters this is detected when trying to supply a
      --  binding for parameters that already have one. For parameterless
      --  subprograms this must be done explicitly.

      if In_Open_Scopes (Subp) then
         Error_Msg_N ("call to recursive subprogram cannot be inlined??", N);
         Set_Is_Inlined (Subp, False);
         return;

      --  Skip inlining if this is not a true inlining since the attribute
      --  Body_To_Inline is also set for renamings (see sinfo.ads)

      elsif Nkind (Orig_Bod) in N_Entity then
         return;

      --  Skip inlining if the function returns an unconstrained type using
      --  an extended return statement since this part of the new inlining
      --  model which is not yet supported by the current implementation. ???

      elsif Is_Unc
        and then
          Nkind (First (Statements (Handled_Statement_Sequence (Orig_Bod))))
            = N_Extended_Return_Statement
        and then not Debug_Flag_Dot_K
      then
         return;
      end if;

      if Nkind (Orig_Bod) = N_Defining_Identifier
        or else Nkind (Orig_Bod) = N_Defining_Operator_Symbol
      then
         --  Subprogram is renaming_as_body. Calls occurring after the renaming
         --  can be replaced with calls to the renamed entity directly, because
         --  the subprograms are subtype conformant. If the renamed subprogram
         --  is an inherited operation, we must redo the expansion because
         --  implicit conversions may be needed. Similarly, if the renamed
         --  entity is inlined, expand the call for further optimizations.

         Set_Name (N, New_Occurrence_Of (Orig_Bod, Loc));

         if Present (Alias (Orig_Bod)) or else Is_Inlined (Orig_Bod) then
            Expand_Call (N);
         end if;

         return;
      end if;

      --  Register the call in the list of inlined calls

      if Inlined_Calls = No_Elist then
         Inlined_Calls := New_Elmt_List;
      end if;

      Append_Elmt (N, To => Inlined_Calls);

      --  Use generic machinery to copy body of inlined subprogram, as if it
      --  were an instantiation, resetting source locations appropriately, so
      --  that nested inlined calls appear in the main unit.

      Save_Env (Subp, Empty);
      Set_Copied_Sloc_For_Inlined_Body (N, Defining_Entity (Orig_Bod));

      --  Old semantics

      if not Debug_Flag_Dot_K then
         declare
            Bod : Node_Id;

         begin
            Bod := Copy_Generic_Node (Orig_Bod, Empty, Instantiating => True);
            Blk :=
              Make_Block_Statement (Loc,
                Declarations => Declarations (Bod),
                Handled_Statement_Sequence =>
                  Handled_Statement_Sequence (Bod));

            if No (Declarations (Bod)) then
               Set_Declarations (Blk, New_List);
            end if;

            --  For the unconstrained case, capture the name of the local
            --  variable that holds the result. This must be the first
            --  declaration in the block, because its bounds cannot depend
            --  on local variables. Otherwise there is no way to declare the
            --  result outside of the block. Needless to say, in general the
            --  bounds will depend on the actuals in the call.

            --  If the context is an assignment statement, as is the case
            --  for the expansion of an extended return, the left-hand side
            --  provides bounds even if the return type is unconstrained.

            if Is_Unc then
               declare
                  First_Decl : Node_Id;

               begin
                  First_Decl := First (Declarations (Blk));

                  if Nkind (First_Decl) /= N_Object_Declaration then
                     return;
                  end if;

                  if Nkind (Parent (N)) /= N_Assignment_Statement then
                     Targ1 := Defining_Identifier (First_Decl);
                  else
                     Targ1 := Name (Parent (N));
                  end if;
               end;
            end if;
         end;

      --  New semantics

      else
         declare
            Bod : Node_Id;

         begin
            --  General case

            if not Is_Unc then
               Bod :=
                 Copy_Generic_Node (Orig_Bod, Empty, Instantiating => True);
               Blk :=
                 Make_Block_Statement (Loc,
                                       Declarations => Declarations (Bod),
                                       Handled_Statement_Sequence =>
                                         Handled_Statement_Sequence (Bod));

            --  Inline a call to a function that returns an unconstrained type.
            --  The semantic analyzer checked that frontend-inlined functions
            --  returning unconstrained types have no declarations and have
            --  a single extended return statement. As part of its processing
            --  the function was split in two subprograms: a procedure P and
            --  a function F that has a block with a call to procedure P (see
            --  Split_Unconstrained_Function).

            else
               pragma Assert
                 (Nkind
                   (First
                     (Statements (Handled_Statement_Sequence (Orig_Bod))))
                  = N_Block_Statement);

               declare
                  Blk_Stmt    : constant Node_Id :=
                    First
                      (Statements
                        (Handled_Statement_Sequence (Orig_Bod)));
                  First_Stmt  : constant Node_Id :=
                    First
                      (Statements
                        (Handled_Statement_Sequence (Blk_Stmt)));
                  Second_Stmt : constant Node_Id := Next (First_Stmt);

               begin
                  pragma Assert
                    (Nkind (First_Stmt) = N_Procedure_Call_Statement
                      and then Nkind (Second_Stmt) = N_Simple_Return_Statement
                      and then No (Next (Second_Stmt)));

                  Bod :=
                    Copy_Generic_Node
                      (First
                        (Statements (Handled_Statement_Sequence (Orig_Bod))),
                       Empty, Instantiating => True);
                  Blk := Bod;

                  --  Capture the name of the local variable that holds the
                  --  result. This must be the first declaration in the block,
                  --  because its bounds cannot depend on local variables.
                  --  Otherwise there is no way to declare the result outside
                  --  of the block. Needless to say, in general the bounds will
                  --  depend on the actuals in the call.

                  if Nkind (Parent (N)) /= N_Assignment_Statement then
                     Targ1 := Defining_Identifier (First (Declarations (Blk)));

                  --  If the context is an assignment statement, as is the case
                  --  for the expansion of an extended return, the left-hand
                  --  side provides bounds even if the return type is
                  --  unconstrained.

                  else
                     Targ1 := Name (Parent (N));
                  end if;
               end;
            end if;

            if No (Declarations (Bod)) then
               Set_Declarations (Blk, New_List);
            end if;
         end;
      end if;

      --  If this is a derived function, establish the proper return type

      if Present (Orig_Subp) and then Orig_Subp /= Subp then
         Ret_Type := Etype (Orig_Subp);
      else
         Ret_Type := Etype (Subp);
      end if;

      --  Create temporaries for the actuals that are expressions, or that are
      --  scalars and require copying to preserve semantics.

      F := First_Formal (Subp);
      A := First_Actual (N);
      while Present (F) loop
         if Present (Renamed_Object (F)) then
            Error_Msg_N ("cannot inline call to recursive subprogram", N);
            return;
         end if;

         --  Reset Last_Assignment for any parameters of mode out or in out, to
         --  prevent spurious warnings about overwriting for assignments to the
         --  formal in the inlined code.

         if Is_Entity_Name (A) and then Ekind (F) /= E_In_Parameter then
            Set_Last_Assignment (Entity (A), Empty);
         end if;

         --  If the argument may be a controlling argument in a call within
         --  the inlined body, we must preserve its classwide nature to insure
         --  that dynamic dispatching take place subsequently. If the formal
         --  has a constraint it must be preserved to retain the semantics of
         --  the body.

         if Is_Class_Wide_Type (Etype (F))
           or else (Is_Access_Type (Etype (F))
                     and then Is_Class_Wide_Type (Designated_Type (Etype (F))))
         then
            Temp_Typ := Etype (F);

         elsif Base_Type (Etype (F)) = Base_Type (Etype (A))
           and then Etype (F) /= Base_Type (Etype (F))
         then
            Temp_Typ := Etype (F);
         else
            Temp_Typ := Etype (A);
         end if;

         --  If the actual is a simple name or a literal, no need to
         --  create a temporary, object can be used directly.

         --  If the actual is a literal and the formal has its address taken,
         --  we cannot pass the literal itself as an argument, so its value
         --  must be captured in a temporary.

         if (Is_Entity_Name (A)
              and then
               (not Is_Scalar_Type (Etype (A))
                 or else Ekind (Entity (A)) = E_Enumeration_Literal))

         --  When the actual is an identifier and the corresponding formal is
         --  used only once in the original body, the formal can be substituted
         --  directly with the actual parameter.

           or else (Nkind (A) = N_Identifier
             and then Formal_Is_Used_Once (F))

           or else
             (Nkind_In (A, N_Real_Literal,
                           N_Integer_Literal,
                           N_Character_Literal)
               and then not Address_Taken (F))
         then
            if Etype (F) /= Etype (A) then
               Set_Renamed_Object
                 (F, Unchecked_Convert_To (Etype (F), Relocate_Node (A)));
            else
               Set_Renamed_Object (F, A);
            end if;

         else
            Temp := Make_Temporary (Loc, 'C');

            --  If the actual for an in/in-out parameter is a view conversion,
            --  make it into an unchecked conversion, given that an untagged
            --  type conversion is not a proper object for a renaming.

            --  In-out conversions that involve real conversions have already
            --  been transformed in Expand_Actuals.

            if Nkind (A) = N_Type_Conversion
              and then Ekind (F) /= E_In_Parameter
            then
               New_A :=
                 Make_Unchecked_Type_Conversion (Loc,
                   Subtype_Mark => New_Occurrence_Of (Etype (F), Loc),
                   Expression   => Relocate_Node (Expression (A)));

            elsif Etype (F) /= Etype (A) then
               New_A := Unchecked_Convert_To (Etype (F), Relocate_Node (A));
               Temp_Typ := Etype (F);

            else
               New_A := Relocate_Node (A);
            end if;

            Set_Sloc (New_A, Sloc (N));

            --  If the actual has a by-reference type, it cannot be copied,
            --  so its value is captured in a renaming declaration. Otherwise
            --  declare a local constant initialized with the actual.

            --  We also use a renaming declaration for expressions of an array
            --  type that is not bit-packed, both for efficiency reasons and to
            --  respect the semantics of the call: in most cases the original
            --  call will pass the parameter by reference, and thus the inlined
            --  code will have the same semantics.

            if Ekind (F) = E_In_Parameter
              and then not Is_By_Reference_Type (Etype (A))
              and then
                (not Is_Array_Type (Etype (A))
                  or else not Is_Object_Reference (A)
                  or else Is_Bit_Packed_Array (Etype (A)))
            then
               Decl :=
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Temp,
                   Constant_Present    => True,
                   Object_Definition   => New_Occurrence_Of (Temp_Typ, Loc),
                   Expression          => New_A);
            else
               Decl :=
                 Make_Object_Renaming_Declaration (Loc,
                   Defining_Identifier => Temp,
                   Subtype_Mark        => New_Occurrence_Of (Temp_Typ, Loc),
                   Name                => New_A);
            end if;

            Append (Decl, Decls);
            Set_Renamed_Object (F, Temp);
         end if;

         Next_Formal (F);
         Next_Actual (A);
      end loop;

      --  Establish target of function call. If context is not assignment or
      --  declaration, create a temporary as a target. The declaration for the
      --  temporary may be subsequently optimized away if the body is a single
      --  expression, or if the left-hand side of the assignment is simple
      --  enough, i.e. an entity or an explicit dereference of one.

      if Ekind (Subp) = E_Function then
         if Nkind (Parent (N)) = N_Assignment_Statement
           and then Is_Entity_Name (Name (Parent (N)))
         then
            Targ := Name (Parent (N));

         elsif Nkind (Parent (N)) = N_Assignment_Statement
           and then Nkind (Name (Parent (N))) = N_Explicit_Dereference
           and then Is_Entity_Name (Prefix (Name (Parent (N))))
         then
            Targ := Name (Parent (N));

         elsif Nkind (Parent (N)) = N_Assignment_Statement
           and then Nkind (Name (Parent (N))) = N_Selected_Component
           and then Is_Entity_Name (Prefix (Name (Parent (N))))
         then
            Targ := New_Copy_Tree (Name (Parent (N)));

         elsif Nkind (Parent (N)) = N_Object_Declaration
           and then Is_Limited_Type (Etype (Subp))
         then
            Targ := Defining_Identifier (Parent (N));

         --  New semantics: In an object declaration avoid an extra copy
         --  of the result of a call to an inlined function that returns
         --  an unconstrained type

         elsif Debug_Flag_Dot_K
           and then Nkind (Parent (N)) = N_Object_Declaration
           and then Is_Unc
         then
            Targ := Defining_Identifier (Parent (N));

         else
            --  Replace call with temporary and create its declaration

            Temp := Make_Temporary (Loc, 'C');
            Set_Is_Internal (Temp);

            --  For the unconstrained case, the generated temporary has the
            --  same constrained declaration as the result variable. It may
            --  eventually be possible to remove that temporary and use the
            --  result variable directly.

            if Is_Unc
              and then Nkind (Parent (N)) /= N_Assignment_Statement
            then
               Decl :=
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Temp,
                   Object_Definition   =>
                     New_Copy_Tree (Object_Definition (Parent (Targ1))));

               Replace_Formals (Decl);

            else
               Decl :=
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Temp,
                   Object_Definition   => New_Occurrence_Of (Ret_Type, Loc));

               Set_Etype (Temp, Ret_Type);
            end if;

            Set_No_Initialization (Decl);
            Append (Decl, Decls);
            Rewrite (N, New_Occurrence_Of (Temp, Loc));
            Targ := Temp;
         end if;
      end if;

      Insert_Actions (N, Decls);

      if Is_Unc_Decl then

         --  Special management for inlining a call to a function that returns
         --  an unconstrained type and initializes an object declaration: we
         --  avoid generating undesired extra calls and goto statements.

         --     Given:
         --                 function Func (...) return ...
         --                 begin
         --                    declare
         --                       Result : String (1 .. 4);
         --                    begin
         --                       Proc (Result, ...);
         --                       return Result;
         --                    end;
         --                 end F;

         --                 Result : String := Func (...);

         --     Replace this object declaration by:

         --                 Result : String (1 .. 4);
         --                 Proc (Result, ...);

         Remove_Homonym (Targ);

         Decl :=
           Make_Object_Declaration
             (Loc,
              Defining_Identifier => Targ,
              Object_Definition   =>
                New_Copy_Tree (Object_Definition (Parent (Targ1))));
         Replace_Formals (Decl);
         Rewrite (Parent (N), Decl);
         Analyze (Parent (N));

         --  Avoid spurious warnings since we know that this declaration is
         --  referenced by the procedure call.

         Set_Never_Set_In_Source (Targ, False);

         --  Remove the local declaration of the extended return stmt from the
         --  inlined code

         Remove (Parent (Targ1));

         --  Update the reference to the result (since we have rewriten the
         --  object declaration)

         declare
            Blk_Call_Stmt : Node_Id;

         begin
            --  Capture the call to the procedure

            Blk_Call_Stmt :=
              First (Statements (Handled_Statement_Sequence (Blk)));
            pragma Assert
              (Nkind (Blk_Call_Stmt) = N_Procedure_Call_Statement);

            Remove (First (Parameter_Associations (Blk_Call_Stmt)));
            Prepend_To (Parameter_Associations (Blk_Call_Stmt),
              New_Reference_To (Targ, Loc));
         end;

         --  Remove the return statement

         pragma Assert
           (Nkind (Last (Statements (Handled_Statement_Sequence (Blk)))) =
                                                   N_Simple_Return_Statement);

         Remove (Last (Statements (Handled_Statement_Sequence (Blk))));
      end if;

      --  Traverse the tree and replace formals with actuals or their thunks.
      --  Attach block to tree before analysis and rewriting.

      Replace_Formals (Blk);
      Set_Parent (Blk, N);

      if not Comes_From_Source (Subp) or else Is_Predef then
         Reset_Slocs (Blk);
      end if;

      if Is_Unc_Decl then

         --  No action needed since return statement has been already removed

         null;

      elsif Present (Exit_Lab) then

         --  If the body was a single expression, the single return statement
         --  and the corresponding label are useless.

         if Num_Ret = 1
           and then
             Nkind (Last (Statements (Handled_Statement_Sequence (Blk)))) =
                                                            N_Goto_Statement
         then
            Remove (Last (Statements (Handled_Statement_Sequence (Blk))));
         else
            Append (Lab_Decl, (Declarations (Blk)));
            Append (Exit_Lab, Statements (Handled_Statement_Sequence (Blk)));
         end if;
      end if;

      --  Analyze Blk with In_Inlined_Body set, to avoid spurious errors
      --  on conflicting private views that Gigi would ignore. If this is a
      --  predefined unit, analyze with checks off, as is done in the non-
      --  inlined run-time units.

      declare
         I_Flag : constant Boolean := In_Inlined_Body;

      begin
         In_Inlined_Body := True;

         if Is_Predef then
            declare
               Style : constant Boolean := Style_Check;

            begin
               Style_Check := False;

               --  Search for dispatching calls that use the Object.Operation
               --  notation using an Object that is a parameter of the inlined
               --  function. We reset the decoration of Operation to force
               --  the reanalysis of the inlined dispatching call because
               --  the actual object has been inlined.

               Reset_Dispatching_Calls (Blk);

               Analyze (Blk, Suppress => All_Checks);
               Style_Check := Style;
            end;

         else
            Analyze (Blk);
         end if;

         In_Inlined_Body := I_Flag;
      end;

      if Ekind (Subp) = E_Procedure then
         Rewrite_Procedure_Call (N, Blk);

      else
         Rewrite_Function_Call (N, Blk);

         if Is_Unc_Decl then
            null;

         --  For the unconstrained case, the replacement of the call has been
         --  made prior to the complete analysis of the generated declarations.
         --  Propagate the proper type now.

         elsif Is_Unc then
            if Nkind (N) = N_Identifier then
               Set_Etype (N, Etype (Entity (N)));
            else
               Set_Etype (N, Etype (Targ1));
            end if;
         end if;
      end if;

      Restore_Env;

      --  Cleanup mapping between formals and actuals for other expansions

      F := First_Formal (Subp);
      while Present (F) loop
         Set_Renamed_Object (F, Empty);
         Next_Formal (F);
      end loop;
   end Expand_Inlined_Call;

   ----------------------------------------
   -- Expand_N_Extended_Return_Statement --
   ----------------------------------------

   --  If there is a Handled_Statement_Sequence, we rewrite this:

   --     return Result : T := <expression> do
   --        <handled_seq_of_stms>
   --     end return;

   --  to be:

   --     declare
   --        Result : T := <expression>;
   --     begin
   --        <handled_seq_of_stms>
   --        return Result;
   --     end;

   --  Otherwise (no Handled_Statement_Sequence), we rewrite this:

   --     return Result : T := <expression>;

   --  to be:

   --     return <expression>;

   --  unless it's build-in-place or there's no <expression>, in which case
   --  we generate:

   --     declare
   --        Result : T := <expression>;
   --     begin
   --        return Result;
   --     end;

   --  Note that this case could have been written by the user as an extended
   --  return statement, or could have been transformed to this from a simple
   --  return statement.

   --  That is, we need to have a reified return object if there are statements
   --  (which might refer to it) or if we're doing build-in-place (so we can
   --  set its address to the final resting place or if there is no expression
   --  (in which case default initial values might need to be set).

   procedure Expand_N_Extended_Return_Statement (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

      Par_Func     : constant Entity_Id :=
                       Return_Applies_To (Return_Statement_Entity (N));
      Result_Subt  : constant Entity_Id := Etype (Par_Func);
      Ret_Obj_Id   : constant Entity_Id :=
                       First_Entity (Return_Statement_Entity (N));
      Ret_Obj_Decl : constant Node_Id := Parent (Ret_Obj_Id);

      Is_Build_In_Place : constant Boolean :=
                            Is_Build_In_Place_Function (Par_Func);

      Exp         : Node_Id;
      HSS         : Node_Id;
      Result      : Node_Id;
      Return_Stmt : Node_Id;
      Stmts       : List_Id;

      function Build_Heap_Allocator
        (Temp_Id    : Entity_Id;
         Temp_Typ   : Entity_Id;
         Func_Id    : Entity_Id;
         Ret_Typ    : Entity_Id;
         Alloc_Expr : Node_Id) return Node_Id;
      --  Create the statements necessary to allocate a return object on the
      --  caller's master. The master is available through implicit parameter
      --  BIPfinalizationmaster.
      --
      --    if BIPfinalizationmaster /= null then
      --       declare
      --          type Ptr_Typ is access Ret_Typ;
      --          for Ptr_Typ'Storage_Pool use
      --                Base_Pool (BIPfinalizationmaster.all).all;
      --          Local : Ptr_Typ;
      --
      --       begin
      --          procedure Allocate (...) is
      --          begin
      --             System.Storage_Pools.Subpools.Allocate_Any (...);
      --          end Allocate;
      --
      --          Local := <Alloc_Expr>;
      --          Temp_Id := Temp_Typ (Local);
      --       end;
      --    end if;
      --
      --  Temp_Id is the temporary which is used to reference the internally
      --  created object in all allocation forms. Temp_Typ is the type of the
      --  temporary. Func_Id is the enclosing function. Ret_Typ is the return
      --  type of Func_Id. Alloc_Expr is the actual allocator.

      function Move_Activation_Chain return Node_Id;
      --  Construct a call to System.Tasking.Stages.Move_Activation_Chain
      --  with parameters:
      --    From         current activation chain
      --    To           activation chain passed in by the caller
      --    New_Master   master passed in by the caller

      --------------------------
      -- Build_Heap_Allocator --
      --------------------------

      function Build_Heap_Allocator
        (Temp_Id    : Entity_Id;
         Temp_Typ   : Entity_Id;
         Func_Id    : Entity_Id;
         Ret_Typ    : Entity_Id;
         Alloc_Expr : Node_Id) return Node_Id
      is
      begin
         pragma Assert (Is_Build_In_Place_Function (Func_Id));

         --  Processing for build-in-place object allocation. This is disabled
         --  on .NET/JVM because the targets do not support pools.

         if VM_Target = No_VM
           and then Needs_Finalization (Ret_Typ)
         then
            declare
               Decls      : constant List_Id := New_List;
               Fin_Mas_Id : constant Entity_Id :=
                              Build_In_Place_Formal
                                (Func_Id, BIP_Finalization_Master);
               Stmts      : constant List_Id := New_List;
               Desig_Typ  : Entity_Id;
               Local_Id   : Entity_Id;
               Pool_Id    : Entity_Id;
               Ptr_Typ    : Entity_Id;

            begin
               --  Generate:
               --    Pool_Id renames Base_Pool (BIPfinalizationmaster.all).all;

               Pool_Id := Make_Temporary (Loc, 'P');

               Append_To (Decls,
                 Make_Object_Renaming_Declaration (Loc,
                   Defining_Identifier => Pool_Id,
                   Subtype_Mark        =>
                     New_Reference_To (RTE (RE_Root_Storage_Pool), Loc),
                   Name                =>
                     Make_Explicit_Dereference (Loc,
                       Prefix =>
                         Make_Function_Call (Loc,
                           Name                   =>
                             New_Reference_To (RTE (RE_Base_Pool), Loc),
                           Parameter_Associations => New_List (
                             Make_Explicit_Dereference (Loc,
                               Prefix =>
                                 New_Reference_To (Fin_Mas_Id, Loc)))))));

               --  Create an access type which uses the storage pool of the
               --  caller's master. This additional type is necessary because
               --  the finalization master cannot be associated with the type
               --  of the temporary. Otherwise the secondary stack allocation
               --  will fail.

               Desig_Typ := Ret_Typ;

               --  Ensure that the build-in-place machinery uses a fat pointer
               --  when allocating an unconstrained array on the heap. In this
               --  case the result object type is a constrained array type even
               --  though the function type is unconstrained.

               if Ekind (Desig_Typ) = E_Array_Subtype then
                  Desig_Typ := Base_Type (Desig_Typ);
               end if;

               --  Generate:
               --    type Ptr_Typ is access Desig_Typ;

               Ptr_Typ := Make_Temporary (Loc, 'P');

               Append_To (Decls,
                 Make_Full_Type_Declaration (Loc,
                   Defining_Identifier => Ptr_Typ,
                   Type_Definition     =>
                     Make_Access_To_Object_Definition (Loc,
                       Subtype_Indication =>
                         New_Reference_To (Desig_Typ, Loc))));

               --  Perform minor decoration in order to set the master and the
               --  storage pool attributes.

               Set_Ekind (Ptr_Typ, E_Access_Type);
               Set_Finalization_Master     (Ptr_Typ, Fin_Mas_Id);
               Set_Associated_Storage_Pool (Ptr_Typ, Pool_Id);

               --  Create the temporary, generate:
               --    Local_Id : Ptr_Typ;

               Local_Id := Make_Temporary (Loc, 'T');

               Append_To (Decls,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Local_Id,
                   Object_Definition   =>
                     New_Reference_To (Ptr_Typ, Loc)));

               --  Allocate the object, generate:
               --    Local_Id := <Alloc_Expr>;

               Append_To (Stmts,
                 Make_Assignment_Statement (Loc,
                   Name       => New_Reference_To (Local_Id, Loc),
                   Expression => Alloc_Expr));

               --  Generate:
               --    Temp_Id := Temp_Typ (Local_Id);

               Append_To (Stmts,
                 Make_Assignment_Statement (Loc,
                   Name       => New_Reference_To (Temp_Id, Loc),
                   Expression =>
                     Unchecked_Convert_To (Temp_Typ,
                       New_Reference_To (Local_Id, Loc))));

               --  Wrap the allocation in a block. This is further conditioned
               --  by checking the caller finalization master at runtime. A
               --  null value indicates a non-existent master, most likely due
               --  to a Finalize_Storage_Only allocation.

               --  Generate:
               --    if BIPfinalizationmaster /= null then
               --       declare
               --          <Decls>
               --       begin
               --          <Stmts>
               --       end;
               --    end if;

               return
                 Make_If_Statement (Loc,
                   Condition       =>
                     Make_Op_Ne (Loc,
                       Left_Opnd  => New_Reference_To (Fin_Mas_Id, Loc),
                       Right_Opnd => Make_Null (Loc)),

                   Then_Statements => New_List (
                     Make_Block_Statement (Loc,
                       Declarations               => Decls,
                       Handled_Statement_Sequence =>
                         Make_Handled_Sequence_Of_Statements (Loc,
                           Statements => Stmts))));
            end;

         --  For all other cases, generate:
         --    Temp_Id := <Alloc_Expr>;

         else
            return
              Make_Assignment_Statement (Loc,
                Name       => New_Reference_To (Temp_Id, Loc),
                Expression => Alloc_Expr);
         end if;
      end Build_Heap_Allocator;

      ---------------------------
      -- Move_Activation_Chain --
      ---------------------------

      function Move_Activation_Chain return Node_Id is
      begin
         return
           Make_Procedure_Call_Statement (Loc,
             Name                   =>
               New_Reference_To (RTE (RE_Move_Activation_Chain), Loc),

             Parameter_Associations => New_List (

               --  Source chain

               Make_Attribute_Reference (Loc,
                 Prefix         => Make_Identifier (Loc, Name_uChain),
                 Attribute_Name => Name_Unrestricted_Access),

               --  Destination chain

               New_Reference_To
                 (Build_In_Place_Formal (Par_Func, BIP_Activation_Chain), Loc),

               --  New master

               New_Reference_To
                 (Build_In_Place_Formal (Par_Func, BIP_Task_Master), Loc)));
      end Move_Activation_Chain;

   --  Start of processing for Expand_N_Extended_Return_Statement

   begin
      --  Given that functionality of interface thunks is simple (just displace
      --  the pointer to the object) they are always handled by means of
      --  simple return statements.

      pragma Assert (not Is_Thunk (Current_Scope));

      if Nkind (Ret_Obj_Decl) = N_Object_Declaration then
         Exp := Expression (Ret_Obj_Decl);
      else
         Exp := Empty;
      end if;

      HSS := Handled_Statement_Sequence (N);

      --  If the returned object needs finalization actions, the function must
      --  perform the appropriate cleanup should it fail to return. The state
      --  of the function itself is tracked through a flag which is coupled
      --  with the scope finalizer. There is one flag per each return object
      --  in case of multiple returns.

      if Is_Build_In_Place
        and then Needs_Finalization (Etype (Ret_Obj_Id))
      then
         declare
            Flag_Decl : Node_Id;
            Flag_Id   : Entity_Id;
            Func_Bod  : Node_Id;

         begin
            --  Recover the function body

            Func_Bod := Unit_Declaration_Node (Par_Func);

            if Nkind (Func_Bod) = N_Subprogram_Declaration then
               Func_Bod := Parent (Parent (Corresponding_Body (Func_Bod)));
            end if;

            --  Create a flag to track the function state

            Flag_Id := Make_Temporary (Loc, 'F');
            Set_Status_Flag_Or_Transient_Decl (Ret_Obj_Id, Flag_Id);

            --  Insert the flag at the beginning of the function declarations,
            --  generate:
            --    Fnn : Boolean := False;

            Flag_Decl :=
              Make_Object_Declaration (Loc,
                Defining_Identifier => Flag_Id,
                  Object_Definition =>
                    New_Reference_To (Standard_Boolean, Loc),
                  Expression        => New_Reference_To (Standard_False, Loc));

            Prepend_To (Declarations (Func_Bod), Flag_Decl);
            Analyze (Flag_Decl);
         end;
      end if;

      --  Build a simple_return_statement that returns the return object when
      --  there is a statement sequence, or no expression, or the result will
      --  be built in place. Note however that we currently do this for all
      --  composite cases, even though nonlimited composite results are not yet
      --  built in place (though we plan to do so eventually).

      if Present (HSS)
        or else Is_Composite_Type (Result_Subt)
        or else No (Exp)
      then
         if No (HSS) then
            Stmts := New_List;

         --  If the extended return has a handled statement sequence, then wrap
         --  it in a block and use the block as the first statement.

         else
            Stmts := New_List (
              Make_Block_Statement (Loc,
                Declarations               => New_List,
                Handled_Statement_Sequence => HSS));
         end if;

         --  If the result type contains tasks, we call Move_Activation_Chain.
         --  Later, the cleanup code will call Complete_Master, which will
         --  terminate any unactivated tasks belonging to the return statement
         --  master. But Move_Activation_Chain updates their master to be that
         --  of the caller, so they will not be terminated unless the return
         --  statement completes unsuccessfully due to exception, abort, goto,
         --  or exit. As a formality, we test whether the function requires the
         --  result to be built in place, though that's necessarily true for
         --  the case of result types with task parts.

         if Is_Build_In_Place
           and then Has_Task (Result_Subt)
         then
            --  The return expression is an aggregate for a complex type which
            --  contains tasks. This particular case is left unexpanded since
            --  the regular expansion would insert all temporaries and
            --  initialization code in the wrong block.

            if Nkind (Exp) = N_Aggregate then
               Expand_N_Aggregate (Exp);
            end if;

            --  Do not move the activation chain if the return object does not
            --  contain tasks.

            if Has_Task (Etype (Ret_Obj_Id)) then
               Append_To (Stmts, Move_Activation_Chain);
            end if;
         end if;

         --  Update the state of the function right before the object is
         --  returned.

         if Is_Build_In_Place
           and then Needs_Finalization (Etype (Ret_Obj_Id))
         then
            declare
               Flag_Id : constant Entity_Id :=
                           Status_Flag_Or_Transient_Decl (Ret_Obj_Id);

            begin
               --  Generate:
               --    Fnn := True;

               Append_To (Stmts,
                 Make_Assignment_Statement (Loc,
                   Name       => New_Reference_To (Flag_Id, Loc),
                   Expression => New_Reference_To (Standard_True, Loc)));
            end;
         end if;

         --  Build a simple_return_statement that returns the return object

         Return_Stmt :=
           Make_Simple_Return_Statement (Loc,
             Expression => New_Occurrence_Of (Ret_Obj_Id, Loc));
         Append_To (Stmts, Return_Stmt);

         HSS := Make_Handled_Sequence_Of_Statements (Loc, Stmts);
      end if;

      --  Case where we build a return statement block

      if Present (HSS) then
         Result :=
           Make_Block_Statement (Loc,
             Declarations               => Return_Object_Declarations (N),
             Handled_Statement_Sequence => HSS);

         --  We set the entity of the new block statement to be that of the
         --  return statement. This is necessary so that various fields, such
         --  as Finalization_Chain_Entity carry over from the return statement
         --  to the block. Note that this block is unusual, in that its entity
         --  is an E_Return_Statement rather than an E_Block.

         Set_Identifier
           (Result, New_Occurrence_Of (Return_Statement_Entity (N), Loc));

         --  If the object decl was already rewritten as a renaming, then we
         --  don't want to do the object allocation and transformation of of
         --  the return object declaration to a renaming. This case occurs
         --  when the return object is initialized by a call to another
         --  build-in-place function, and that function is responsible for
         --  the allocation of the return object.

         if Is_Build_In_Place
           and then Nkind (Ret_Obj_Decl) = N_Object_Renaming_Declaration
         then
            pragma Assert
              (Nkind (Original_Node (Ret_Obj_Decl)) = N_Object_Declaration
                and then Is_Build_In_Place_Function_Call
                           (Expression (Original_Node (Ret_Obj_Decl))));

            --  Return the build-in-place result by reference

            Set_By_Ref (Return_Stmt);

         elsif Is_Build_In_Place then

            --  Locate the implicit access parameter associated with the
            --  caller-supplied return object and convert the return
            --  statement's return object declaration to a renaming of a
            --  dereference of the access parameter. If the return object's
            --  declaration includes an expression that has not already been
            --  expanded as separate assignments, then add an assignment
            --  statement to ensure the return object gets initialized.

            --    declare
            --       Result : T [:= <expression>];
            --    begin
            --       ...

            --  is converted to

            --    declare
            --       Result : T renames FuncRA.all;
            --       [Result := <expression;]
            --    begin
            --       ...

            declare
               Return_Obj_Id    : constant Entity_Id :=
                                    Defining_Identifier (Ret_Obj_Decl);
               Return_Obj_Typ   : constant Entity_Id := Etype (Return_Obj_Id);
               Return_Obj_Expr  : constant Node_Id :=
                                    Expression (Ret_Obj_Decl);
               Constr_Result    : constant Boolean :=
                                    Is_Constrained (Result_Subt);
               Obj_Alloc_Formal : Entity_Id;
               Object_Access    : Entity_Id;
               Obj_Acc_Deref    : Node_Id;
               Init_Assignment  : Node_Id := Empty;

            begin
               --  Build-in-place results must be returned by reference

               Set_By_Ref (Return_Stmt);

               --  Retrieve the implicit access parameter passed by the caller

               Object_Access :=
                 Build_In_Place_Formal (Par_Func, BIP_Object_Access);

               --  If the return object's declaration includes an expression
               --  and the declaration isn't marked as No_Initialization, then
               --  we need to generate an assignment to the object and insert
               --  it after the declaration before rewriting it as a renaming
               --  (otherwise we'll lose the initialization). The case where
               --  the result type is an interface (or class-wide interface)
               --  is also excluded because the context of the function call
               --  must be unconstrained, so the initialization will always
               --  be done as part of an allocator evaluation (storage pool
               --  or secondary stack), never to a constrained target object
               --  passed in by the caller. Besides the assignment being
               --  unneeded in this case, it avoids problems with trying to
               --  generate a dispatching assignment when the return expression
               --  is a nonlimited descendant of a limited interface (the
               --  interface has no assignment operation).

               if Present (Return_Obj_Expr)
                 and then not No_Initialization (Ret_Obj_Decl)
                 and then not Is_Interface (Return_Obj_Typ)
               then
                  Init_Assignment :=
                    Make_Assignment_Statement (Loc,
                      Name       => New_Reference_To (Return_Obj_Id, Loc),
                      Expression => Relocate_Node (Return_Obj_Expr));

                  Set_Etype (Name (Init_Assignment), Etype (Return_Obj_Id));
                  Set_Assignment_OK (Name (Init_Assignment));
                  Set_No_Ctrl_Actions (Init_Assignment);

                  Set_Parent (Name (Init_Assignment), Init_Assignment);
                  Set_Parent (Expression (Init_Assignment), Init_Assignment);

                  Set_Expression (Ret_Obj_Decl, Empty);

                  if Is_Class_Wide_Type (Etype (Return_Obj_Id))
                    and then not Is_Class_Wide_Type
                                   (Etype (Expression (Init_Assignment)))
                  then
                     Rewrite (Expression (Init_Assignment),
                       Make_Type_Conversion (Loc,
                         Subtype_Mark =>
                           New_Occurrence_Of (Etype (Return_Obj_Id), Loc),
                         Expression   =>
                           Relocate_Node (Expression (Init_Assignment))));
                  end if;

                  --  In the case of functions where the calling context can
                  --  determine the form of allocation needed, initialization
                  --  is done with each part of the if statement that handles
                  --  the different forms of allocation (this is true for
                  --  unconstrained and tagged result subtypes).

                  if Constr_Result
                    and then not Is_Tagged_Type (Underlying_Type (Result_Subt))
                  then
                     Insert_After (Ret_Obj_Decl, Init_Assignment);
                  end if;
               end if;

               --  When the function's subtype is unconstrained, a run-time
               --  test is needed to determine the form of allocation to use
               --  for the return object. The function has an implicit formal
               --  parameter indicating this. If the BIP_Alloc_Form formal has
               --  the value one, then the caller has passed access to an
               --  existing object for use as the return object. If the value
               --  is two, then the return object must be allocated on the
               --  secondary stack. Otherwise, the object must be allocated in
               --  a storage pool (currently only supported for the global
               --  heap, user-defined storage pools TBD ???). We generate an
               --  if statement to test the implicit allocation formal and
               --  initialize a local access value appropriately, creating
               --  allocators in the secondary stack and global heap cases.
               --  The special formal also exists and must be tested when the
               --  function has a tagged result, even when the result subtype
               --  is constrained, because in general such functions can be
               --  called in dispatching contexts and must be handled similarly
               --  to functions with a class-wide result.

               if not Constr_Result
                 or else Is_Tagged_Type (Underlying_Type (Result_Subt))
               then
                  Obj_Alloc_Formal :=
                    Build_In_Place_Formal (Par_Func, BIP_Alloc_Form);

                  declare
                     Pool_Id        : constant Entity_Id :=
                                        Make_Temporary (Loc, 'P');
                     Alloc_Obj_Id   : Entity_Id;
                     Alloc_Obj_Decl : Node_Id;
                     Alloc_If_Stmt  : Node_Id;
                     Heap_Allocator : Node_Id;
                     Pool_Decl      : Node_Id;
                     Pool_Allocator : Node_Id;
                     Ptr_Type_Decl  : Node_Id;
                     Ref_Type       : Entity_Id;
                     SS_Allocator   : Node_Id;

                  begin
                     --  Reuse the itype created for the function's implicit
                     --  access formal. This avoids the need to create a new
                     --  access type here, plus it allows assigning the access
                     --  formal directly without applying a conversion.

                     --    Ref_Type := Etype (Object_Access);

                     --  Create an access type designating the function's
                     --  result subtype.

                     Ref_Type := Make_Temporary (Loc, 'A');

                     Ptr_Type_Decl :=
                       Make_Full_Type_Declaration (Loc,
                         Defining_Identifier => Ref_Type,
                         Type_Definition     =>
                           Make_Access_To_Object_Definition (Loc,
                             All_Present        => True,
                             Subtype_Indication =>
                               New_Reference_To (Return_Obj_Typ, Loc)));

                     Insert_Before (Ret_Obj_Decl, Ptr_Type_Decl);

                     --  Create an access object that will be initialized to an
                     --  access value denoting the return object, either coming
                     --  from an implicit access value passed in by the caller
                     --  or from the result of an allocator.

                     Alloc_Obj_Id := Make_Temporary (Loc, 'R');
                     Set_Etype (Alloc_Obj_Id, Ref_Type);

                     Alloc_Obj_Decl :=
                       Make_Object_Declaration (Loc,
                         Defining_Identifier => Alloc_Obj_Id,
                         Object_Definition   =>
                           New_Reference_To (Ref_Type, Loc));

                     Insert_Before (Ret_Obj_Decl, Alloc_Obj_Decl);

                     --  Create allocators for both the secondary stack and
                     --  global heap. If there's an initialization expression,
                     --  then create these as initialized allocators.

                     if Present (Return_Obj_Expr)
                       and then not No_Initialization (Ret_Obj_Decl)
                     then
                        --  Always use the type of the expression for the
                        --  qualified expression, rather than the result type.
                        --  In general we cannot always use the result type
                        --  for the allocator, because the expression might be
                        --  of a specific type, such as in the case of an
                        --  aggregate or even a nonlimited object when the
                        --  result type is a limited class-wide interface type.

                        Heap_Allocator :=
                          Make_Allocator (Loc,
                            Expression =>
                              Make_Qualified_Expression (Loc,
                                Subtype_Mark =>
                                  New_Reference_To
                                    (Etype (Return_Obj_Expr), Loc),
                                Expression   =>
                                  New_Copy_Tree (Return_Obj_Expr)));

                     else
                        --  If the function returns a class-wide type we cannot
                        --  use the return type for the allocator. Instead we
                        --  use the type of the expression, which must be an
                        --  aggregate of a definite type.

                        if Is_Class_Wide_Type (Return_Obj_Typ) then
                           Heap_Allocator :=
                             Make_Allocator (Loc,
                               Expression =>
                                 New_Reference_To
                                   (Etype (Return_Obj_Expr), Loc));
                        else
                           Heap_Allocator :=
                             Make_Allocator (Loc,
                               Expression =>
                                 New_Reference_To (Return_Obj_Typ, Loc));
                        end if;

                        --  If the object requires default initialization then
                        --  that will happen later following the elaboration of
                        --  the object renaming. If we don't turn it off here
                        --  then the object will be default initialized twice.

                        Set_No_Initialization (Heap_Allocator);
                     end if;

                     --  The Pool_Allocator is just like the Heap_Allocator,
                     --  except we set Storage_Pool and Procedure_To_Call so
                     --  it will use the user-defined storage pool.

                     Pool_Allocator := New_Copy_Tree (Heap_Allocator);

                     --  Do not generate the renaming of the build-in-place
                     --  pool parameter on .NET/JVM/ZFP because the parameter
                     --  is not created in the first place.

                     if VM_Target = No_VM
                       and then RTE_Available (RE_Root_Storage_Pool_Ptr)
                     then
                        Pool_Decl :=
                          Make_Object_Renaming_Declaration (Loc,
                            Defining_Identifier => Pool_Id,
                            Subtype_Mark        =>
                              New_Reference_To
                                (RTE (RE_Root_Storage_Pool), Loc),
                            Name                =>
                              Make_Explicit_Dereference (Loc,
                                New_Reference_To
                                  (Build_In_Place_Formal
                                     (Par_Func, BIP_Storage_Pool), Loc)));
                        Set_Storage_Pool (Pool_Allocator, Pool_Id);
                        Set_Procedure_To_Call
                          (Pool_Allocator, RTE (RE_Allocate_Any));
                     else
                        Pool_Decl := Make_Null_Statement (Loc);
                     end if;

                     --  If the No_Allocators restriction is active, then only
                     --  an allocator for secondary stack allocation is needed.
                     --  It's OK for such allocators to have Comes_From_Source
                     --  set to False, because gigi knows not to flag them as
                     --  being a violation of No_Implicit_Heap_Allocations.

                     if Restriction_Active (No_Allocators) then
                        SS_Allocator   := Heap_Allocator;
                        Heap_Allocator := Make_Null (Loc);
                        Pool_Allocator := Make_Null (Loc);

                     --  Otherwise the heap and pool allocators may be needed,
                     --  so we make another allocator for secondary stack
                     --  allocation.

                     else
                        SS_Allocator := New_Copy_Tree (Heap_Allocator);

                        --  The heap and pool allocators are marked as
                        --  Comes_From_Source since they correspond to an
                        --  explicit user-written allocator (that is, it will
                        --  only be executed on behalf of callers that call the
                        --  function as initialization for such an allocator).
                        --  Prevents errors when No_Implicit_Heap_Allocations
                        --  is in force.

                        Set_Comes_From_Source (Heap_Allocator, True);
                        Set_Comes_From_Source (Pool_Allocator, True);
                     end if;

                     --  The allocator is returned on the secondary stack. We
                     --  don't do this on VM targets, since the SS is not used.

                     if VM_Target = No_VM then
                        Set_Storage_Pool (SS_Allocator, RTE (RE_SS_Pool));
                        Set_Procedure_To_Call
                          (SS_Allocator, RTE (RE_SS_Allocate));

                        --  The allocator is returned on the secondary stack,
                        --  so indicate that the function return, as well as
                        --  the block that encloses the allocator, must not
                        --  release it. The flags must be set now because
                        --  the decision to use the secondary stack is done
                        --  very late in the course of expanding the return
                        --  statement, past the point where these flags are
                        --  normally set.

                        Set_Sec_Stack_Needed_For_Return (Par_Func);
                        Set_Sec_Stack_Needed_For_Return
                          (Return_Statement_Entity (N));
                        Set_Uses_Sec_Stack (Par_Func);
                        Set_Uses_Sec_Stack (Return_Statement_Entity (N));
                     end if;

                     --  Create an if statement to test the BIP_Alloc_Form
                     --  formal and initialize the access object to either the
                     --  BIP_Object_Access formal (BIP_Alloc_Form =
                     --  Caller_Allocation), the result of allocating the
                     --  object in the secondary stack (BIP_Alloc_Form =
                     --  Secondary_Stack), or else an allocator to create the
                     --  return object in the heap or user-defined pool
                     --  (BIP_Alloc_Form = Global_Heap or User_Storage_Pool).

                     --  ??? An unchecked type conversion must be made in the
                     --  case of assigning the access object formal to the
                     --  local access object, because a normal conversion would
                     --  be illegal in some cases (such as converting access-
                     --  to-unconstrained to access-to-constrained), but the
                     --  the unchecked conversion will presumably fail to work
                     --  right in just such cases. It's not clear at all how to
                     --  handle this. ???

                     Alloc_If_Stmt :=
                       Make_If_Statement (Loc,
                         Condition =>
                           Make_Op_Eq (Loc,
                             Left_Opnd  =>
                               New_Reference_To (Obj_Alloc_Formal, Loc),
                             Right_Opnd =>
                               Make_Integer_Literal (Loc,
                                 UI_From_Int (BIP_Allocation_Form'Pos
                                                (Caller_Allocation)))),

                         Then_Statements => New_List (
                           Make_Assignment_Statement (Loc,
                             Name       =>
                               New_Reference_To (Alloc_Obj_Id, Loc),
                             Expression =>
                               Make_Unchecked_Type_Conversion (Loc,
                                 Subtype_Mark =>
                                   New_Reference_To (Ref_Type, Loc),
                                 Expression   =>
                                   New_Reference_To (Object_Access, Loc)))),

                         Elsif_Parts => New_List (
                           Make_Elsif_Part (Loc,
                             Condition =>
                               Make_Op_Eq (Loc,
                                 Left_Opnd  =>
                                   New_Reference_To (Obj_Alloc_Formal, Loc),
                                 Right_Opnd =>
                                   Make_Integer_Literal (Loc,
                                     UI_From_Int (BIP_Allocation_Form'Pos
                                                    (Secondary_Stack)))),

                             Then_Statements => New_List (
                               Make_Assignment_Statement (Loc,
                                 Name       =>
                                   New_Reference_To (Alloc_Obj_Id, Loc),
                                 Expression => SS_Allocator))),

                           Make_Elsif_Part (Loc,
                             Condition =>
                               Make_Op_Eq (Loc,
                                 Left_Opnd  =>
                                   New_Reference_To (Obj_Alloc_Formal, Loc),
                                 Right_Opnd =>
                                   Make_Integer_Literal (Loc,
                                     UI_From_Int (BIP_Allocation_Form'Pos
                                                    (Global_Heap)))),

                             Then_Statements => New_List (
                               Build_Heap_Allocator
                                 (Temp_Id    => Alloc_Obj_Id,
                                  Temp_Typ   => Ref_Type,
                                  Func_Id    => Par_Func,
                                  Ret_Typ    => Return_Obj_Typ,
                                  Alloc_Expr => Heap_Allocator)))),

                         Else_Statements => New_List (
                           Pool_Decl,
                           Build_Heap_Allocator
                             (Temp_Id    => Alloc_Obj_Id,
                              Temp_Typ   => Ref_Type,
                              Func_Id    => Par_Func,
                              Ret_Typ    => Return_Obj_Typ,
                              Alloc_Expr => Pool_Allocator)));

                     --  If a separate initialization assignment was created
                     --  earlier, append that following the assignment of the
                     --  implicit access formal to the access object, to ensure
                     --  that the return object is initialized in that case. In
                     --  this situation, the target of the assignment must be
                     --  rewritten to denote a dereference of the access to the
                     --  return object passed in by the caller.

                     if Present (Init_Assignment) then
                        Rewrite (Name (Init_Assignment),
                          Make_Explicit_Dereference (Loc,
                            Prefix => New_Reference_To (Alloc_Obj_Id, Loc)));

                        Set_Etype
                          (Name (Init_Assignment), Etype (Return_Obj_Id));

                        Append_To
                          (Then_Statements (Alloc_If_Stmt), Init_Assignment);
                     end if;

                     Insert_Before (Ret_Obj_Decl, Alloc_If_Stmt);

                     --  Remember the local access object for use in the
                     --  dereference of the renaming created below.

                     Object_Access := Alloc_Obj_Id;
                  end;
               end if;

               --  Replace the return object declaration with a renaming of a
               --  dereference of the access value designating the return
               --  object.

               Obj_Acc_Deref :=
                 Make_Explicit_Dereference (Loc,
                   Prefix => New_Reference_To (Object_Access, Loc));

               Rewrite (Ret_Obj_Decl,
                 Make_Object_Renaming_Declaration (Loc,
                   Defining_Identifier => Return_Obj_Id,
                   Access_Definition   => Empty,
                   Subtype_Mark        =>
                     New_Occurrence_Of (Return_Obj_Typ, Loc),
                   Name                => Obj_Acc_Deref));

               Set_Renamed_Object (Return_Obj_Id, Obj_Acc_Deref);
            end;
         end if;

      --  Case where we do not build a block

      else
         --  We're about to drop Return_Object_Declarations on the floor, so
         --  we need to insert it, in case it got expanded into useful code.
         --  Remove side effects from expression, which may be duplicated in
         --  subsequent checks (see Expand_Simple_Function_Return).

         Insert_List_Before (N, Return_Object_Declarations (N));
         Remove_Side_Effects (Exp);

         --  Build simple_return_statement that returns the expression directly

         Return_Stmt := Make_Simple_Return_Statement (Loc, Expression => Exp);
         Result := Return_Stmt;
      end if;

      --  Set the flag to prevent infinite recursion

      Set_Comes_From_Extended_Return_Statement (Return_Stmt);

      Rewrite (N, Result);
      Analyze (N);
   end Expand_N_Extended_Return_Statement;

   ----------------------------
   -- Expand_N_Function_Call --
   ----------------------------

   procedure Expand_N_Function_Call (N : Node_Id) is
   begin
      Expand_Call (N);

      --  If the return value of a foreign compiled function is VAX Float, then
      --  expand the return (adjusts the location of the return value on
      --  Alpha/VMS, no-op everywhere else).
      --  Comes_From_Source intercepts recursive expansion.

      if Nkind (N) = N_Function_Call
        and then Vax_Float (Etype (N))
        and then Present (Name (N))
        and then Present (Entity (Name (N)))
        and then Has_Foreign_Convention (Entity (Name (N)))
        and then Comes_From_Source (Parent (N))
      then
         Expand_Vax_Foreign_Return (N);
      end if;
   end Expand_N_Function_Call;

   ---------------------------------------
   -- Expand_N_Procedure_Call_Statement --
   ---------------------------------------

   procedure Expand_N_Procedure_Call_Statement (N : Node_Id) is
   begin
      Expand_Call (N);
   end Expand_N_Procedure_Call_Statement;

   --------------------------------------
   -- Expand_N_Simple_Return_Statement --
   --------------------------------------

   procedure Expand_N_Simple_Return_Statement (N : Node_Id) is
   begin
      --  Defend against previous errors (i.e. the return statement calls a
      --  function that is not available in configurable runtime).

      if Present (Expression (N))
        and then Nkind (Expression (N)) = N_Empty
      then
         Check_Error_Detected;
         return;
      end if;

      --  Distinguish the function and non-function cases:

      case Ekind (Return_Applies_To (Return_Statement_Entity (N))) is

         when E_Function          |
              E_Generic_Function  =>
            Expand_Simple_Function_Return (N);

         when E_Procedure         |
              E_Generic_Procedure |
              E_Entry             |
              E_Entry_Family      |
              E_Return_Statement =>
            Expand_Non_Function_Return (N);

         when others =>
            raise Program_Error;
      end case;

   exception
      when RE_Not_Available =>
         return;
   end Expand_N_Simple_Return_Statement;

   ------------------------------
   -- Expand_N_Subprogram_Body --
   ------------------------------

   --  Add poll call if ATC polling is enabled, unless the body will be inlined
   --  by the back-end.

   --  Add dummy push/pop label nodes at start and end to clear any local
   --  exception indications if local-exception-to-goto optimization is active.

   --  Add return statement if last statement in body is not a return statement
   --  (this makes things easier on Gigi which does not want to have to handle
   --  a missing return).

   --  Add call to Activate_Tasks if body is a task activator

   --  Deal with possible detection of infinite recursion

   --  Eliminate body completely if convention stubbed

   --  Encode entity names within body, since we will not need to reference
   --  these entities any longer in the front end.

   --  Initialize scalar out parameters if Initialize/Normalize_Scalars

   --  Reset Pure indication if any parameter has root type System.Address
   --  or has any parameters of limited types, where limited means that the
   --  run-time view is limited (i.e. the full type is limited).

   --  Wrap thread body

   procedure Expand_N_Subprogram_Body (N : Node_Id) is
      Loc      : constant Source_Ptr := Sloc (N);
      H        : constant Node_Id    := Handled_Statement_Sequence (N);
      Body_Id  : Entity_Id;
      Except_H : Node_Id;
      L        : List_Id;
      Spec_Id  : Entity_Id;

      procedure Add_Return (S : List_Id);
      --  Append a return statement to the statement sequence S if the last
      --  statement is not already a return or a goto statement. Note that
      --  the latter test is not critical, it does not matter if we add a few
      --  extra returns, since they get eliminated anyway later on.

      ----------------
      -- Add_Return --
      ----------------

      procedure Add_Return (S : List_Id) is
         Last_Stm : Node_Id;
         Loc      : Source_Ptr;

      begin
         --  Get last statement, ignoring any Pop_xxx_Label nodes, which are
         --  not relevant in this context since they are not executable.

         Last_Stm := Last (S);
         while Nkind (Last_Stm) in N_Pop_xxx_Label loop
            Prev (Last_Stm);
         end loop;

         --  Now insert return unless last statement is a transfer

         if not Is_Transfer (Last_Stm) then

            --  The source location for the return is the end label of the
            --  procedure if present. Otherwise use the sloc of the last
            --  statement in the list. If the list comes from a generated
            --  exception handler and we are not debugging generated code,
            --  all the statements within the handler are made invisible
            --  to the debugger.

            if Nkind (Parent (S)) = N_Exception_Handler
              and then not Comes_From_Source (Parent (S))
            then
               Loc := Sloc (Last_Stm);
            elsif Present (End_Label (H)) then
               Loc := Sloc (End_Label (H));
            else
               Loc := Sloc (Last_Stm);
            end if;

            declare
               Rtn : constant Node_Id := Make_Simple_Return_Statement (Loc);

            begin
               --  Append return statement, and set analyzed manually. We can't
               --  call Analyze on this return since the scope is wrong.

               --  Note: it almost works to push the scope and then do the
               --  Analyze call, but something goes wrong in some weird cases
               --  and it is not worth worrying about ???

               Append_To (S, Rtn);
               Set_Analyzed (Rtn);

               --  Call _Postconditions procedure if appropriate. We need to
               --  do this explicitly because we did not analyze the generated
               --  return statement above, so the call did not get inserted.

               if Ekind (Spec_Id) = E_Procedure
                 and then Has_Postconditions (Spec_Id)
               then
                  pragma Assert (Present (Postcondition_Proc (Spec_Id)));
                  Insert_Action (Rtn,
                    Make_Procedure_Call_Statement (Loc,
                      Name =>
                        New_Reference_To (Postcondition_Proc (Spec_Id), Loc)));
               end if;
            end;
         end if;
      end Add_Return;

   --  Start of processing for Expand_N_Subprogram_Body

   begin
      --  Set L to either the list of declarations if present, or to the list
      --  of statements if no declarations are present. This is used to insert
      --  new stuff at the start.

      if Is_Non_Empty_List (Declarations (N)) then
         L := Declarations (N);
      else
         L := Statements (H);
      end if;

      --  If local-exception-to-goto optimization active, insert dummy push
      --  statements at start, and dummy pop statements at end, but inhibit
      --  this if we have No_Exception_Handlers, since they are useless and
      --  intefere with analysis, e.g. by codepeer.

      if (Debug_Flag_Dot_G
           or else Restriction_Active (No_Exception_Propagation))
        and then not Restriction_Active (No_Exception_Handlers)
        and then not CodePeer_Mode
        and then Is_Non_Empty_List (L)
      then
         declare
            FS  : constant Node_Id    := First (L);
            FL  : constant Source_Ptr := Sloc (FS);
            LS  : Node_Id;
            LL  : Source_Ptr;

         begin
            --  LS points to either last statement, if statements are present
            --  or to the last declaration if there are no statements present.
            --  It is the node after which the pop's are generated.

            if Is_Non_Empty_List (Statements (H)) then
               LS := Last (Statements (H));
            else
               LS := Last (L);
            end if;

            LL := Sloc (LS);

            Insert_List_Before_And_Analyze (FS, New_List (
              Make_Push_Constraint_Error_Label (FL),
              Make_Push_Program_Error_Label    (FL),
              Make_Push_Storage_Error_Label    (FL)));

            Insert_List_After_And_Analyze (LS, New_List (
              Make_Pop_Constraint_Error_Label  (LL),
              Make_Pop_Program_Error_Label     (LL),
              Make_Pop_Storage_Error_Label     (LL)));
         end;
      end if;

      --  Find entity for subprogram

      Body_Id := Defining_Entity (N);

      if Present (Corresponding_Spec (N)) then
         Spec_Id := Corresponding_Spec (N);
      else
         Spec_Id := Body_Id;
      end if;

      --  Need poll on entry to subprogram if polling enabled. We only do this
      --  for non-empty subprograms, since it does not seem necessary to poll
      --  for a dummy null subprogram.

      if Is_Non_Empty_List (L) then

         --  Do not add a polling call if the subprogram is to be inlined by
         --  the back-end, to avoid repeated calls with multiple inlinings.

         if Is_Inlined (Spec_Id)
           and then Front_End_Inlining
           and then Optimization_Level > 1
         then
            null;
         else
            Generate_Poll_Call (First (L));
         end if;
      end if;

      --  If this is a Pure function which has any parameters whose root type
      --  is System.Address, reset the Pure indication, since it will likely
      --  cause incorrect code to be generated as the parameter is probably
      --  a pointer, and the fact that the same pointer is passed does not mean
      --  that the same value is being referenced.

      --  Note that if the programmer gave an explicit Pure_Function pragma,
      --  then we believe the programmer, and leave the subprogram Pure.

      --  This code should probably be at the freeze point, so that it happens
      --  even on a -gnatc (or more importantly -gnatt) compile, so that the
      --  semantic tree has Is_Pure set properly ???

      if Is_Pure (Spec_Id)
        and then Is_Subprogram (Spec_Id)
        and then not Has_Pragma_Pure_Function (Spec_Id)
      then
         declare
            F : Entity_Id;

         begin
            F := First_Formal (Spec_Id);
            while Present (F) loop
               if Is_Descendent_Of_Address (Etype (F))

                 --  Note that this test is being made in the body of the
                 --  subprogram, not the spec, so we are testing the full
                 --  type for being limited here, as required.

                 or else Is_Limited_Type (Etype (F))
               then
                  Set_Is_Pure (Spec_Id, False);

                  if Spec_Id /= Body_Id then
                     Set_Is_Pure (Body_Id, False);
                  end if;

                  exit;
               end if;

               Next_Formal (F);
            end loop;
         end;
      end if;

      --  Initialize any scalar OUT args if Initialize/Normalize_Scalars

      if Init_Or_Norm_Scalars and then Is_Subprogram (Spec_Id) then
         declare
            F : Entity_Id;
            A : Node_Id;

         begin
            --  Loop through formals

            F := First_Formal (Spec_Id);
            while Present (F) loop
               if Is_Scalar_Type (Etype (F))
                 and then Ekind (F) = E_Out_Parameter
               then
                  Check_Restriction (No_Default_Initialization, F);

                  --  Insert the initialization. We turn off validity checks
                  --  for this assignment, since we do not want any check on
                  --  the initial value itself (which may well be invalid).
                  --  Predicate checks are disabled as well (RM 6.4.1 (13/3))

                  A :=  Make_Assignment_Statement (Loc,
                      Name       => New_Occurrence_Of (F, Loc),
                      Expression => Get_Simple_Init_Val (Etype (F), N));
                  Set_Suppress_Assignment_Checks (A);

                  Insert_Before_And_Analyze (First (L),
                    A, Suppress => Validity_Check);
               end if;

               Next_Formal (F);
            end loop;
         end;
      end if;

      --  Clear out statement list for stubbed procedure

      if Present (Corresponding_Spec (N)) then
         Set_Elaboration_Flag (N, Spec_Id);

         if Convention (Spec_Id) = Convention_Stubbed
           or else Is_Eliminated (Spec_Id)
         then
            Set_Declarations (N, Empty_List);
            Set_Handled_Statement_Sequence (N,
              Make_Handled_Sequence_Of_Statements (Loc,
                Statements => New_List (Make_Null_Statement (Loc))));
            return;
         end if;
      end if;

      --  Create a set of discriminals for the next protected subprogram body

      if Is_List_Member (N)
        and then Present (Parent (List_Containing (N)))
        and then Nkind (Parent (List_Containing (N))) = N_Protected_Body
        and then Present (Next_Protected_Operation (N))
      then
         Set_Discriminals (Parent (Base_Type (Scope (Spec_Id))));
      end if;

      --  Returns_By_Ref flag is normally set when the subprogram is frozen but
      --  subprograms with no specs are not frozen.

      declare
         Typ  : constant Entity_Id := Etype (Spec_Id);
         Utyp : constant Entity_Id := Underlying_Type (Typ);

      begin
         if not Acts_As_Spec (N)
           and then Nkind (Parent (Parent (Spec_Id))) /=
             N_Subprogram_Body_Stub
         then
            null;

         elsif Is_Limited_View (Typ) then
            Set_Returns_By_Ref (Spec_Id);

         elsif Present (Utyp) and then CW_Or_Has_Controlled_Part (Utyp) then
            Set_Returns_By_Ref (Spec_Id);
         end if;
      end;

      --  For a procedure, we add a return for all possible syntactic ends of
      --  the subprogram.

      if Ekind_In (Spec_Id, E_Procedure, E_Generic_Procedure) then
         Add_Return (Statements (H));

         if Present (Exception_Handlers (H)) then
            Except_H := First_Non_Pragma (Exception_Handlers (H));
            while Present (Except_H) loop
               Add_Return (Statements (Except_H));
               Next_Non_Pragma (Except_H);
            end loop;
         end if;

      --  For a function, we must deal with the case where there is at least
      --  one missing return. What we do is to wrap the entire body of the
      --  function in a block:

      --    begin
      --      ...
      --    end;

      --  becomes

      --    begin
      --       begin
      --          ...
      --       end;

      --       raise Program_Error;
      --    end;

      --  This approach is necessary because the raise must be signalled to the
      --  caller, not handled by any local handler (RM 6.4(11)).

      --  Note: we do not need to analyze the constructed sequence here, since
      --  it has no handler, and an attempt to analyze the handled statement
      --  sequence twice is risky in various ways (e.g. the issue of expanding
      --  cleanup actions twice).

      elsif Has_Missing_Return (Spec_Id) then
         declare
            Hloc : constant Source_Ptr := Sloc (H);
            Blok : constant Node_Id    :=
                     Make_Block_Statement (Hloc,
                       Handled_Statement_Sequence => H);
            Rais : constant Node_Id    :=
                     Make_Raise_Program_Error (Hloc,
                       Reason => PE_Missing_Return);

         begin
            Set_Handled_Statement_Sequence (N,
              Make_Handled_Sequence_Of_Statements (Hloc,
                Statements => New_List (Blok, Rais)));

            Push_Scope (Spec_Id);
            Analyze (Blok);
            Analyze (Rais);
            Pop_Scope;
         end;
      end if;

      --  If subprogram contains a parameterless recursive call, then we may
      --  have an infinite recursion, so see if we can generate code to check
      --  for this possibility if storage checks are not suppressed.

      if Ekind (Spec_Id) = E_Procedure
        and then Has_Recursive_Call (Spec_Id)
        and then not Storage_Checks_Suppressed (Spec_Id)
      then
         Detect_Infinite_Recursion (N, Spec_Id);
      end if;

      --  Set to encode entity names in package body before gigi is called

      Qualify_Entity_Names (N);
   end Expand_N_Subprogram_Body;

   -----------------------------------
   -- Expand_N_Subprogram_Body_Stub --
   -----------------------------------

   procedure Expand_N_Subprogram_Body_Stub (N : Node_Id) is
   begin
      if Present (Corresponding_Body (N)) then
         Expand_N_Subprogram_Body (
           Unit_Declaration_Node (Corresponding_Body (N)));
      end if;
   end Expand_N_Subprogram_Body_Stub;

   -------------------------------------
   -- Expand_N_Subprogram_Declaration --
   -------------------------------------

   --  If the declaration appears within a protected body, it is a private
   --  operation of the protected type. We must create the corresponding
   --  protected subprogram an associated formals. For a normal protected
   --  operation, this is done when expanding the protected type declaration.

   --  If the declaration is for a null procedure, emit null body

   procedure Expand_N_Subprogram_Declaration (N : Node_Id) is
      Loc       : constant Source_Ptr := Sloc (N);
      Subp      : constant Entity_Id  := Defining_Entity (N);
      Scop      : constant Entity_Id  := Scope (Subp);
      Prot_Decl : Node_Id;
      Prot_Bod  : Node_Id;
      Prot_Id   : Entity_Id;

   begin
      --  In SPARK, subprogram declarations are only allowed in package
      --  specifications.

      if Nkind (Parent (N)) /= N_Package_Specification then
         if Nkind (Parent (N)) = N_Compilation_Unit then
            Check_SPARK_Restriction
              ("subprogram declaration is not a library item", N);

         elsif Present (Next (N))
           and then Nkind (Next (N)) = N_Pragma
           and then Get_Pragma_Id (Pragma_Name (Next (N))) = Pragma_Import
         then
            --  In SPARK, subprogram declarations are also permitted in
            --  declarative parts when immediately followed by a corresponding
            --  pragma Import. We only check here that there is some pragma
            --  Import.

            null;
         else
            Check_SPARK_Restriction
              ("subprogram declaration is not allowed here", N);
         end if;
      end if;

      --  Deal with case of protected subprogram. Do not generate protected
      --  operation if operation is flagged as eliminated.

      if Is_List_Member (N)
        and then Present (Parent (List_Containing (N)))
        and then Nkind (Parent (List_Containing (N))) = N_Protected_Body
        and then Is_Protected_Type (Scop)
      then
         if No (Protected_Body_Subprogram (Subp))
           and then not Is_Eliminated (Subp)
         then
            Prot_Decl :=
              Make_Subprogram_Declaration (Loc,
                Specification =>
                  Build_Protected_Sub_Specification
                    (N, Scop, Unprotected_Mode));

            --  The protected subprogram is declared outside of the protected
            --  body. Given that the body has frozen all entities so far, we
            --  analyze the subprogram and perform freezing actions explicitly.
            --  including the generation of an explicit freeze node, to ensure
            --  that gigi has the proper order of elaboration.
            --  If the body is a subunit, the insertion point is before the
            --  stub in the parent.

            Prot_Bod := Parent (List_Containing (N));

            if Nkind (Parent (Prot_Bod)) = N_Subunit then
               Prot_Bod := Corresponding_Stub (Parent (Prot_Bod));
            end if;

            Insert_Before (Prot_Bod, Prot_Decl);
            Prot_Id := Defining_Unit_Name (Specification (Prot_Decl));
            Set_Has_Delayed_Freeze (Prot_Id);

            Push_Scope (Scope (Scop));
            Analyze (Prot_Decl);
            Freeze_Before (N, Prot_Id);
            Set_Protected_Body_Subprogram (Subp, Prot_Id);

            --  Create protected operation as well. Even though the operation
            --  is only accessible within the body, it is possible to make it
            --  available outside of the protected object by using 'Access to
            --  provide a callback, so build protected version in all cases.

            Prot_Decl :=
              Make_Subprogram_Declaration (Loc,
                Specification =>
                  Build_Protected_Sub_Specification (N, Scop, Protected_Mode));
            Insert_Before (Prot_Bod, Prot_Decl);
            Analyze (Prot_Decl);

            Pop_Scope;
         end if;

      --  Ada 2005 (AI-348): Generate body for a null procedure. In most
      --  cases this is superfluous because calls to it will be automatically
      --  inlined, but we definitely need the body if preconditions for the
      --  procedure are present.

      elsif Nkind (Specification (N)) = N_Procedure_Specification
        and then Null_Present (Specification (N))
      then
         declare
            Bod : constant Node_Id := Body_To_Inline (N);

         begin
            Set_Has_Completion (Subp, False);
            Append_Freeze_Action (Subp, Bod);

            --  The body now contains raise statements, so calls to it will
            --  not be inlined.

            Set_Is_Inlined (Subp, False);
         end;
      end if;
   end Expand_N_Subprogram_Declaration;

   --------------------------------
   -- Expand_Non_Function_Return --
   --------------------------------

   procedure Expand_Non_Function_Return (N : Node_Id) is
      pragma Assert (No (Expression (N)));

      Loc         : constant Source_Ptr := Sloc (N);
      Scope_Id    : Entity_Id :=
                      Return_Applies_To (Return_Statement_Entity (N));
      Kind        : constant Entity_Kind := Ekind (Scope_Id);
      Call        : Node_Id;
      Acc_Stat    : Node_Id;
      Goto_Stat   : Node_Id;
      Lab_Node    : Node_Id;

   begin
      --  Call _Postconditions procedure if procedure with active
      --  postconditions. Here, we use the Postcondition_Proc attribute,
      --  which is needed for implicitly-generated returns. Functions
      --  never have implicitly-generated returns, and there's no
      --  room for Postcondition_Proc in E_Function, so we look up the
      --  identifier Name_uPostconditions for function returns (see
      --  Expand_Simple_Function_Return).

      if Ekind (Scope_Id) = E_Procedure
        and then Has_Postconditions (Scope_Id)
      then
         pragma Assert (Present (Postcondition_Proc (Scope_Id)));
         Insert_Action (N,
           Make_Procedure_Call_Statement (Loc,
             Name => New_Reference_To (Postcondition_Proc (Scope_Id), Loc)));
      end if;

      --  If it is a return from a procedure do no extra steps

      if Kind = E_Procedure or else Kind = E_Generic_Procedure then
         return;

      --  If it is a nested return within an extended one, replace it with a
      --  return of the previously declared return object.

      elsif Kind = E_Return_Statement then
         Rewrite (N,
           Make_Simple_Return_Statement (Loc,
             Expression =>
               New_Occurrence_Of (First_Entity (Scope_Id), Loc)));
         Set_Comes_From_Extended_Return_Statement (N);
         Set_Return_Statement_Entity (N, Scope_Id);
         Expand_Simple_Function_Return (N);
         return;
      end if;

      pragma Assert (Is_Entry (Scope_Id));

      --  Look at the enclosing block to see whether the return is from an
      --  accept statement or an entry body.

      for J in reverse 0 .. Scope_Stack.Last loop
         Scope_Id := Scope_Stack.Table (J).Entity;
         exit when Is_Concurrent_Type (Scope_Id);
      end loop;

      --  If it is a return from accept statement it is expanded as call to
      --  RTS Complete_Rendezvous and a goto to the end of the accept body.

      --  (cf : Expand_N_Accept_Statement, Expand_N_Selective_Accept,
      --  Expand_N_Accept_Alternative in exp_ch9.adb)

      if Is_Task_Type (Scope_Id) then

         Call :=
           Make_Procedure_Call_Statement (Loc,
             Name => New_Reference_To (RTE (RE_Complete_Rendezvous), Loc));
         Insert_Before (N, Call);
         --  why not insert actions here???
         Analyze (Call);

         Acc_Stat := Parent (N);
         while Nkind (Acc_Stat) /= N_Accept_Statement loop
            Acc_Stat := Parent (Acc_Stat);
         end loop;

         Lab_Node := Last (Statements
           (Handled_Statement_Sequence (Acc_Stat)));

         Goto_Stat := Make_Goto_Statement (Loc,
           Name => New_Occurrence_Of
             (Entity (Identifier (Lab_Node)), Loc));

         Set_Analyzed (Goto_Stat);

         Rewrite (N, Goto_Stat);
         Analyze (N);

      --  If it is a return from an entry body, put a Complete_Entry_Body call
      --  in front of the return.

      elsif Is_Protected_Type (Scope_Id) then
         Call :=
           Make_Procedure_Call_Statement (Loc,
             Name =>
               New_Reference_To (RTE (RE_Complete_Entry_Body), Loc),
             Parameter_Associations => New_List (
               Make_Attribute_Reference (Loc,
                 Prefix         =>
                   New_Reference_To
                     (Find_Protection_Object (Current_Scope), Loc),
                 Attribute_Name => Name_Unchecked_Access)));

         Insert_Before (N, Call);
         Analyze (Call);
      end if;
   end Expand_Non_Function_Return;

   ---------------------------------------
   -- Expand_Protected_Object_Reference --
   ---------------------------------------

   function Expand_Protected_Object_Reference
     (N    : Node_Id;
      Scop : Entity_Id) return Node_Id
   is
      Loc   : constant Source_Ptr := Sloc (N);
      Corr  : Entity_Id;
      Rec   : Node_Id;
      Param : Entity_Id;
      Proc  : Entity_Id;

   begin
      Rec := Make_Identifier (Loc, Name_uObject);
      Set_Etype (Rec, Corresponding_Record_Type (Scop));

      --  Find enclosing protected operation, and retrieve its first parameter,
      --  which denotes the enclosing protected object. If the enclosing
      --  operation is an entry, we are immediately within the protected body,
      --  and we can retrieve the object from the service entries procedure. A
      --  barrier function has the same signature as an entry. A barrier
      --  function is compiled within the protected object, but unlike
      --  protected operations its never needs locks, so that its protected
      --  body subprogram points to itself.

      Proc := Current_Scope;
      while Present (Proc)
        and then Scope (Proc) /= Scop
      loop
         Proc := Scope (Proc);
      end loop;

      Corr := Protected_Body_Subprogram (Proc);

      if No (Corr) then

         --  Previous error left expansion incomplete.
         --  Nothing to do on this call.

         return Empty;
      end if;

      Param :=
        Defining_Identifier
          (First (Parameter_Specifications (Parent (Corr))));

      if Is_Subprogram (Proc)
        and then Proc /= Corr
      then
         --  Protected function or procedure

         Set_Entity (Rec, Param);

         --  Rec is a reference to an entity which will not be in scope when
         --  the call is reanalyzed, and needs no further analysis.

         Set_Analyzed (Rec);

      else
         --  Entry or barrier function for entry body. The first parameter of
         --  the entry body procedure is pointer to the object. We create a
         --  local variable of the proper type, duplicating what is done to
         --  define _object later on.

         declare
            Decls   : List_Id;
            Obj_Ptr : constant Entity_Id :=  Make_Temporary (Loc, 'T');

         begin
            Decls := New_List (
              Make_Full_Type_Declaration (Loc,
                Defining_Identifier => Obj_Ptr,
                  Type_Definition   =>
                     Make_Access_To_Object_Definition (Loc,
                       Subtype_Indication =>
                         New_Reference_To
                           (Corresponding_Record_Type (Scop), Loc))));

            Insert_Actions (N, Decls);
            Freeze_Before (N, Obj_Ptr);

            Rec :=
              Make_Explicit_Dereference (Loc,
                Prefix =>
                  Unchecked_Convert_To (Obj_Ptr,
                    New_Occurrence_Of (Param, Loc)));

            --  Analyze new actual. Other actuals in calls are already analyzed
            --  and the list of actuals is not reanalyzed after rewriting.

            Set_Parent (Rec, N);
            Analyze (Rec);
         end;
      end if;

      return Rec;
   end Expand_Protected_Object_Reference;

   --------------------------------------
   -- Expand_Protected_Subprogram_Call --
   --------------------------------------

   procedure Expand_Protected_Subprogram_Call
     (N    : Node_Id;
      Subp : Entity_Id;
      Scop : Entity_Id)
   is
      Rec   : Node_Id;

   begin
      --  If the protected object is not an enclosing scope, this is an inter-
      --  object function call. Inter-object procedure calls are expanded by
      --  Exp_Ch9.Build_Simple_Entry_Call. The call is intra-object only if the
      --  subprogram being called is in the protected body being compiled, and
      --  if the protected object in the call is statically the enclosing type.
      --  The object may be an component of some other data structure, in which
      --  case this must be handled as an inter-object call.

      if not In_Open_Scopes (Scop)
        or else not Is_Entity_Name (Name (N))
      then
         if Nkind (Name (N)) = N_Selected_Component then
            Rec := Prefix (Name (N));

         else
            pragma Assert (Nkind (Name (N)) = N_Indexed_Component);
            Rec := Prefix (Prefix (Name (N)));
         end if;

         Build_Protected_Subprogram_Call (N,
           Name     => New_Occurrence_Of (Subp, Sloc (N)),
           Rec      => Convert_Concurrent (Rec, Etype (Rec)),
           External => True);

      else
         Rec := Expand_Protected_Object_Reference (N, Scop);

         if No (Rec) then
            return;
         end if;

         Build_Protected_Subprogram_Call (N,
           Name     => Name (N),
           Rec      => Rec,
           External => False);

      end if;

      --  If it is a function call it can appear in elaboration code and
      --  the called entity must be frozen here.

      if Ekind (Subp) = E_Function then
         Freeze_Expression (Name (N));
      end if;

      --  Analyze and resolve the new call. The actuals have already been
      --  resolved, but expansion of a function call will add extra actuals
      --  if needed. Analysis of a procedure call already includes resolution.

      Analyze (N);

      if Ekind (Subp) = E_Function then
         Resolve (N, Etype (Subp));
      end if;
   end Expand_Protected_Subprogram_Call;

   --------------------------------------------
   -- Has_Unconstrained_Access_Discriminants --
   --------------------------------------------

   function Has_Unconstrained_Access_Discriminants
     (Subtyp : Entity_Id) return Boolean
   is
      Discr : Entity_Id;

   begin
      if Has_Discriminants (Subtyp)
        and then not Is_Constrained (Subtyp)
      then
         Discr := First_Discriminant (Subtyp);
         while Present (Discr) loop
            if Ekind (Etype (Discr)) = E_Anonymous_Access_Type then
               return True;
            end if;

            Next_Discriminant (Discr);
         end loop;
      end if;

      return False;
   end Has_Unconstrained_Access_Discriminants;

   -----------------------------------
   -- Expand_Simple_Function_Return --
   -----------------------------------

   --  The "simple" comes from the syntax rule simple_return_statement. The
   --  semantics are not at all simple.

   procedure Expand_Simple_Function_Return (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

      Scope_Id : constant Entity_Id :=
                   Return_Applies_To (Return_Statement_Entity (N));
      --  The function we are returning from

      R_Type : constant Entity_Id := Etype (Scope_Id);
      --  The result type of the function

      Utyp : constant Entity_Id := Underlying_Type (R_Type);

      Exp : constant Node_Id := Expression (N);
      pragma Assert (Present (Exp));

      Exptyp : constant Entity_Id := Etype (Exp);
      --  The type of the expression (not necessarily the same as R_Type)

      Subtype_Ind : Node_Id;
      --  If the result type of the function is class-wide and the expression
      --  has a specific type, then we use the expression's type as the type of
      --  the return object. In cases where the expression is an aggregate that
      --  is built in place, this avoids the need for an expensive conversion
      --  of the return object to the specific type on assignments to the
      --  individual components.

   begin
      if Is_Class_Wide_Type (R_Type)
        and then not Is_Class_Wide_Type (Etype (Exp))
      then
         Subtype_Ind := New_Occurrence_Of (Etype (Exp), Loc);
      else
         Subtype_Ind := New_Occurrence_Of (R_Type, Loc);
      end if;

      --  For the case of a simple return that does not come from an extended
      --  return, in the case of Ada 2005 where we are returning a limited
      --  type, we rewrite "return <expression>;" to be:

      --    return _anon_ : <return_subtype> := <expression>

      --  The expansion produced by Expand_N_Extended_Return_Statement will
      --  contain simple return statements (for example, a block containing
      --  simple return of the return object), which brings us back here with
      --  Comes_From_Extended_Return_Statement set. The reason for the barrier
      --  checking for a simple return that does not come from an extended
      --  return is to avoid this infinite recursion.

      --  The reason for this design is that for Ada 2005 limited returns, we
      --  need to reify the return object, so we can build it "in place", and
      --  we need a block statement to hang finalization and tasking stuff.

      --  ??? In order to avoid disruption, we avoid translating to extended
      --  return except in the cases where we really need to (Ada 2005 for
      --  inherently limited). We might prefer to do this translation in all
      --  cases (except perhaps for the case of Ada 95 inherently limited),
      --  in order to fully exercise the Expand_N_Extended_Return_Statement
      --  code. This would also allow us to do the build-in-place optimization
      --  for efficiency even in cases where it is semantically not required.

      --  As before, we check the type of the return expression rather than the
      --  return type of the function, because the latter may be a limited
      --  class-wide interface type, which is not a limited type, even though
      --  the type of the expression may be.

      if not Comes_From_Extended_Return_Statement (N)
        and then Is_Limited_View (Etype (Expression (N)))
        and then Ada_Version >= Ada_2005
        and then not Debug_Flag_Dot_L

         --  The functionality of interface thunks is simple and it is always
         --  handled by means of simple return statements. This leaves their
         --  expansion simple and clean.

        and then not Is_Thunk (Current_Scope)
      then
         declare
            Return_Object_Entity : constant Entity_Id :=
                                     Make_Temporary (Loc, 'R', Exp);

            Obj_Decl : constant Node_Id :=
                         Make_Object_Declaration (Loc,
                           Defining_Identifier => Return_Object_Entity,
                           Object_Definition   => Subtype_Ind,
                           Expression          => Exp);

            Ext : constant Node_Id :=
                    Make_Extended_Return_Statement (Loc,
                      Return_Object_Declarations => New_List (Obj_Decl));
            --  Do not perform this high-level optimization if the result type
            --  is an interface because the "this" pointer must be displaced.

         begin
            Rewrite (N, Ext);
            Analyze (N);
            return;
         end;
      end if;

      --  Here we have a simple return statement that is part of the expansion
      --  of an extended return statement (either written by the user, or
      --  generated by the above code).

      --  Always normalize C/Fortran boolean result. This is not always needed,
      --  but it seems a good idea to minimize the passing around of non-
      --  normalized values, and in any case this handles the processing of
      --  barrier functions for protected types, which turn the condition into
      --  a return statement.

      if Is_Boolean_Type (Exptyp)
        and then Nonzero_Is_True (Exptyp)
      then
         Adjust_Condition (Exp);
         Adjust_Result_Type (Exp, Exptyp);
      end if;

      --  Do validity check if enabled for returns

      if Validity_Checks_On
        and then Validity_Check_Returns
      then
         Ensure_Valid (Exp);
      end if;

      --  Check the result expression of a scalar function against the subtype
      --  of the function by inserting a conversion. This conversion must
      --  eventually be performed for other classes of types, but for now it's
      --  only done for scalars.
      --  ???

      if Is_Scalar_Type (Exptyp) then
         Rewrite (Exp, Convert_To (R_Type, Exp));

         --  The expression is resolved to ensure that the conversion gets
         --  expanded to generate a possible constraint check.

         Analyze_And_Resolve (Exp, R_Type);
      end if;

      --  Deal with returning variable length objects and controlled types

      --  Nothing to do if we are returning by reference, or this is not a
      --  type that requires special processing (indicated by the fact that
      --  it requires a cleanup scope for the secondary stack case).

      if Is_Limited_View (Exptyp)
        or else Is_Limited_Interface (Exptyp)
      then
         null;

      --  No copy needed for thunks returning interface type objects since
      --  the object is returned by reference and the maximum functionality
      --  required is just to displace the pointer.

      elsif Is_Thunk (Current_Scope) and then Is_Interface (Exptyp) then
         null;

      elsif not Requires_Transient_Scope (R_Type) then

         --  Mutable records with no variable length components are not
         --  returned on the sec-stack, so we need to make sure that the
         --  backend will only copy back the size of the actual value, and not
         --  the maximum size. We create an actual subtype for this purpose.

         declare
            Ubt  : constant Entity_Id := Underlying_Type (Base_Type (Exptyp));
            Decl : Node_Id;
            Ent  : Entity_Id;
         begin
            if Has_Discriminants (Ubt)
              and then not Is_Constrained (Ubt)
              and then not Has_Unchecked_Union (Ubt)
            then
               Decl := Build_Actual_Subtype (Ubt, Exp);
               Ent := Defining_Identifier (Decl);
               Insert_Action (Exp, Decl);
               Rewrite (Exp, Unchecked_Convert_To (Ent, Exp));
               Analyze_And_Resolve (Exp);
            end if;
         end;

      --  Here if secondary stack is used

      else
         --  Make sure that no surrounding block will reclaim the secondary
         --  stack on which we are going to put the result. Not only may this
         --  introduce secondary stack leaks but worse, if the reclamation is
         --  done too early, then the result we are returning may get
         --  clobbered.

         declare
            S : Entity_Id;
         begin
            S := Current_Scope;
            while Ekind (S) = E_Block or else Ekind (S) = E_Loop loop
               Set_Sec_Stack_Needed_For_Return (S, True);
               S := Enclosing_Dynamic_Scope (S);
            end loop;
         end;

         --  Optimize the case where the result is a function call. In this
         --  case either the result is already on the secondary stack, or is
         --  already being returned with the stack pointer depressed and no
         --  further processing is required except to set the By_Ref flag
         --  to ensure that gigi does not attempt an extra unnecessary copy.
         --  (actually not just unnecessary but harmfully wrong in the case
         --  of a controlled type, where gigi does not know how to do a copy).
         --  To make up for a gcc 2.8.1 deficiency (???), we perform the copy
         --  for array types if the constrained status of the target type is
         --  different from that of the expression.

         if Requires_Transient_Scope (Exptyp)
           and then
              (not Is_Array_Type (Exptyp)
                or else Is_Constrained (Exptyp) = Is_Constrained (R_Type)
                or else CW_Or_Has_Controlled_Part (Utyp))
           and then Nkind (Exp) = N_Function_Call
         then
            Set_By_Ref (N);

            --  Remove side effects from the expression now so that other parts
            --  of the expander do not have to reanalyze this node without this
            --  optimization

            Rewrite (Exp, Duplicate_Subexpr_No_Checks (Exp));

         --  For controlled types, do the allocation on the secondary stack
         --  manually in order to call adjust at the right time:

         --    type Anon1 is access R_Type;
         --    for Anon1'Storage_pool use ss_pool;
         --    Anon2 : anon1 := new R_Type'(expr);
         --    return Anon2.all;

         --  We do the same for classwide types that are not potentially
         --  controlled (by the virtue of restriction No_Finalization) because
         --  gigi is not able to properly allocate class-wide types.

         elsif CW_Or_Has_Controlled_Part (Utyp) then
            declare
               Loc        : constant Source_Ptr := Sloc (N);
               Acc_Typ    : constant Entity_Id := Make_Temporary (Loc, 'A');
               Alloc_Node : Node_Id;
               Temp       : Entity_Id;

            begin
               Set_Ekind (Acc_Typ, E_Access_Type);

               Set_Associated_Storage_Pool (Acc_Typ, RTE (RE_SS_Pool));

               --  This is an allocator for the secondary stack, and it's fine
               --  to have Comes_From_Source set False on it, as gigi knows not
               --  to flag it as a violation of No_Implicit_Heap_Allocations.

               Alloc_Node :=
                 Make_Allocator (Loc,
                   Expression =>
                     Make_Qualified_Expression (Loc,
                       Subtype_Mark => New_Reference_To (Etype (Exp), Loc),
                       Expression   => Relocate_Node (Exp)));

               --  We do not want discriminant checks on the declaration,
               --  given that it gets its value from the allocator.

               Set_No_Initialization (Alloc_Node);

               Temp := Make_Temporary (Loc, 'R', Alloc_Node);

               Insert_List_Before_And_Analyze (N, New_List (
                 Make_Full_Type_Declaration (Loc,
                   Defining_Identifier => Acc_Typ,
                   Type_Definition     =>
                     Make_Access_To_Object_Definition (Loc,
                       Subtype_Indication => Subtype_Ind)),

                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Temp,
                   Object_Definition   => New_Reference_To (Acc_Typ, Loc),
                   Expression          => Alloc_Node)));

               Rewrite (Exp,
                 Make_Explicit_Dereference (Loc,
                 Prefix => New_Reference_To (Temp, Loc)));

               --  Ada 2005 (AI-251): If the type of the returned object is
               --  an interface then add an implicit type conversion to force
               --  displacement of the "this" pointer.

               if Is_Interface (R_Type) then
                  Rewrite (Exp, Convert_To (R_Type, Relocate_Node (Exp)));
               end if;

               Analyze_And_Resolve (Exp, R_Type);
            end;

         --  Otherwise use the gigi mechanism to allocate result on the
         --  secondary stack.

         else
            Check_Restriction (No_Secondary_Stack, N);
            Set_Storage_Pool (N, RTE (RE_SS_Pool));

            --  If we are generating code for the VM do not use
            --  SS_Allocate since everything is heap-allocated anyway.

            if VM_Target = No_VM then
               Set_Procedure_To_Call (N, RTE (RE_SS_Allocate));
            end if;
         end if;
      end if;

      --  Implement the rules of 6.5(8-10), which require a tag check in
      --  the case of a limited tagged return type, and tag reassignment for
      --  nonlimited tagged results. These actions are needed when the return
      --  type is a specific tagged type and the result expression is a
      --  conversion or a formal parameter, because in that case the tag of
      --  the expression might differ from the tag of the specific result type.

      if Is_Tagged_Type (Utyp)
        and then not Is_Class_Wide_Type (Utyp)
        and then (Nkind_In (Exp, N_Type_Conversion,
                                 N_Unchecked_Type_Conversion)
                    or else (Is_Entity_Name (Exp)
                               and then Ekind (Entity (Exp)) in Formal_Kind))
      then
         --  When the return type is limited, perform a check that the tag of
         --  the result is the same as the tag of the return type.

         if Is_Limited_Type (R_Type) then
            Insert_Action (Exp,
              Make_Raise_Constraint_Error (Loc,
                Condition =>
                  Make_Op_Ne (Loc,
                    Left_Opnd  =>
                      Make_Selected_Component (Loc,
                        Prefix        => Duplicate_Subexpr (Exp),
                        Selector_Name => Make_Identifier (Loc, Name_uTag)),
                    Right_Opnd =>
                      Make_Attribute_Reference (Loc,
                        Prefix         =>
                          New_Occurrence_Of (Base_Type (Utyp), Loc),
                        Attribute_Name => Name_Tag)),
                Reason    => CE_Tag_Check_Failed));

         --  If the result type is a specific nonlimited tagged type, then we
         --  have to ensure that the tag of the result is that of the result
         --  type. This is handled by making a copy of the expression in
         --  the case where it might have a different tag, namely when the
         --  expression is a conversion or a formal parameter. We create a new
         --  object of the result type and initialize it from the expression,
         --  which will implicitly force the tag to be set appropriately.

         else
            declare
               ExpR       : constant Node_Id   := Relocate_Node (Exp);
               Result_Id  : constant Entity_Id :=
                              Make_Temporary (Loc, 'R', ExpR);
               Result_Exp : constant Node_Id   :=
                              New_Reference_To (Result_Id, Loc);
               Result_Obj : constant Node_Id   :=
                              Make_Object_Declaration (Loc,
                                Defining_Identifier => Result_Id,
                                Object_Definition   =>
                                  New_Reference_To (R_Type, Loc),
                                Constant_Present    => True,
                                Expression          => ExpR);

            begin
               Set_Assignment_OK (Result_Obj);
               Insert_Action (Exp, Result_Obj);

               Rewrite (Exp, Result_Exp);
               Analyze_And_Resolve (Exp, R_Type);
            end;
         end if;

      --  Ada 2005 (AI-344): If the result type is class-wide, then insert
      --  a check that the level of the return expression's underlying type
      --  is not deeper than the level of the master enclosing the function.
      --  Always generate the check when the type of the return expression
      --  is class-wide, when it's a type conversion, or when it's a formal
      --  parameter. Otherwise, suppress the check in the case where the
      --  return expression has a specific type whose level is known not to
      --  be statically deeper than the function's result type.

      --  No runtime check needed in interface thunks since it is performed
      --  by the target primitive associated with the thunk.

      --  Note: accessibility check is skipped in the VM case, since there
      --  does not seem to be any practical way to implement this check.

      elsif Ada_Version >= Ada_2005
        and then Tagged_Type_Expansion
        and then Is_Class_Wide_Type (R_Type)
        and then not Is_Thunk (Current_Scope)
        and then not Scope_Suppress.Suppress (Accessibility_Check)
        and then
          (Is_Class_Wide_Type (Etype (Exp))
            or else Nkind_In (Exp, N_Type_Conversion,
                                   N_Unchecked_Type_Conversion)
            or else (Is_Entity_Name (Exp)
                      and then Ekind (Entity (Exp)) in Formal_Kind)
            or else Scope_Depth (Enclosing_Dynamic_Scope (Etype (Exp))) >
                      Scope_Depth (Enclosing_Dynamic_Scope (Scope_Id)))
      then
         declare
            Tag_Node : Node_Id;

         begin
            --  Ada 2005 (AI-251): In class-wide interface objects we displace
            --  "this" to reference the base of the object. This is required to
            --  get access to the TSD of the object.

            if Is_Class_Wide_Type (Etype (Exp))
              and then Is_Interface (Etype (Exp))
              and then Nkind (Exp) = N_Explicit_Dereference
            then
               Tag_Node :=
                 Make_Explicit_Dereference (Loc,
                   Prefix =>
                     Unchecked_Convert_To (RTE (RE_Tag_Ptr),
                       Make_Function_Call (Loc,
                         Name                   =>
                           New_Reference_To (RTE (RE_Base_Address), Loc),
                         Parameter_Associations => New_List (
                           Unchecked_Convert_To (RTE (RE_Address),
                             Duplicate_Subexpr (Prefix (Exp)))))));
            else
               Tag_Node :=
                 Make_Attribute_Reference (Loc,
                   Prefix         => Duplicate_Subexpr (Exp),
                   Attribute_Name => Name_Tag);
            end if;

            Insert_Action (Exp,
              Make_Raise_Program_Error (Loc,
                Condition =>
                  Make_Op_Gt (Loc,
                    Left_Opnd  => Build_Get_Access_Level (Loc, Tag_Node),
                    Right_Opnd =>
                      Make_Integer_Literal (Loc,
                        Scope_Depth (Enclosing_Dynamic_Scope (Scope_Id)))),
                Reason => PE_Accessibility_Check_Failed));
         end;

      --  AI05-0073: If function has a controlling access result, check that
      --  the tag of the return value, if it is not null, matches designated
      --  type of return type.

      --  The return expression is referenced twice in the code below, so it
      --  must be made free of side effects. Given that different compilers
      --  may evaluate these parameters in different order, both occurrences
      --  perform a copy.

      elsif Ekind (R_Type) = E_Anonymous_Access_Type
        and then Has_Controlling_Result (Scope_Id)
      then
         Insert_Action (N,
           Make_Raise_Constraint_Error (Loc,
             Condition =>
               Make_And_Then (Loc,
                 Left_Opnd  =>
                   Make_Op_Ne (Loc,
                     Left_Opnd  => Duplicate_Subexpr (Exp),
                     Right_Opnd => Make_Null (Loc)),

                 Right_Opnd => Make_Op_Ne (Loc,
                   Left_Opnd  =>
                     Make_Selected_Component (Loc,
                       Prefix        => Duplicate_Subexpr (Exp),
                       Selector_Name => Make_Identifier (Loc, Name_uTag)),

                   Right_Opnd =>
                     Make_Attribute_Reference (Loc,
                       Prefix         =>
                         New_Occurrence_Of (Designated_Type (R_Type), Loc),
                       Attribute_Name => Name_Tag))),

             Reason    => CE_Tag_Check_Failed),
             Suppress  => All_Checks);
      end if;

      --  AI05-0234: RM 6.5(21/3). Check access discriminants to
      --  ensure that the function result does not outlive an
      --  object designated by one of it discriminants.

      if Present (Extra_Accessibility_Of_Result (Scope_Id))
        and then Has_Unconstrained_Access_Discriminants (R_Type)
      then
         declare
            Discrim_Source : Node_Id;

            procedure Check_Against_Result_Level (Level : Node_Id);
            --  Check the given accessibility level against the level
            --  determined by the point of call. (AI05-0234).

            --------------------------------
            -- Check_Against_Result_Level --
            --------------------------------

            procedure Check_Against_Result_Level (Level : Node_Id) is
            begin
               Insert_Action (N,
                 Make_Raise_Program_Error (Loc,
                   Condition =>
                     Make_Op_Gt (Loc,
                       Left_Opnd  => Level,
                       Right_Opnd =>
                         New_Occurrence_Of
                           (Extra_Accessibility_Of_Result (Scope_Id), Loc)),
                       Reason => PE_Accessibility_Check_Failed));
            end Check_Against_Result_Level;

         begin
            Discrim_Source := Exp;
            while Nkind (Discrim_Source) = N_Qualified_Expression loop
               Discrim_Source := Expression (Discrim_Source);
            end loop;

            if Nkind (Discrim_Source) = N_Identifier
              and then Is_Return_Object (Entity (Discrim_Source))
            then
               Discrim_Source := Entity (Discrim_Source);

               if Is_Constrained (Etype (Discrim_Source)) then
                  Discrim_Source := Etype (Discrim_Source);
               else
                  Discrim_Source := Expression (Parent (Discrim_Source));
               end if;

            elsif Nkind (Discrim_Source) = N_Identifier
              and then Nkind_In (Original_Node (Discrim_Source),
                                 N_Aggregate, N_Extension_Aggregate)
            then
               Discrim_Source := Original_Node (Discrim_Source);

            elsif Nkind (Discrim_Source) = N_Explicit_Dereference and then
              Nkind (Original_Node (Discrim_Source)) = N_Function_Call
            then
               Discrim_Source := Original_Node (Discrim_Source);
            end if;

            while Nkind_In (Discrim_Source, N_Qualified_Expression,
                                            N_Type_Conversion,
                                            N_Unchecked_Type_Conversion)
            loop
               Discrim_Source := Expression (Discrim_Source);
            end loop;

            case Nkind (Discrim_Source) is
               when N_Defining_Identifier =>

                  pragma Assert (Is_Composite_Type (Discrim_Source)
                                  and then Has_Discriminants (Discrim_Source)
                                  and then Is_Constrained (Discrim_Source));

                  declare
                     Discrim   : Entity_Id :=
                                   First_Discriminant (Base_Type (R_Type));
                     Disc_Elmt : Elmt_Id   :=
                                   First_Elmt (Discriminant_Constraint
                                                 (Discrim_Source));
                  begin
                     loop
                        if Ekind (Etype (Discrim)) =
                             E_Anonymous_Access_Type
                        then
                           Check_Against_Result_Level
                             (Dynamic_Accessibility_Level (Node (Disc_Elmt)));
                        end if;

                        Next_Elmt (Disc_Elmt);
                        Next_Discriminant (Discrim);
                        exit when not Present (Discrim);
                     end loop;
                  end;

               when N_Aggregate | N_Extension_Aggregate =>

                  --  Unimplemented: extension aggregate case where discrims
                  --  come from ancestor part, not extension part.

                  declare
                     Discrim  : Entity_Id :=
                                  First_Discriminant (Base_Type (R_Type));

                     Disc_Exp : Node_Id   := Empty;

                     Positionals_Exhausted
                              : Boolean   := not Present (Expressions
                                                            (Discrim_Source));

                     function Associated_Expr
                       (Comp_Id : Entity_Id;
                        Associations : List_Id) return Node_Id;

                     --  Given a component and a component associations list,
                     --  locate the expression for that component; returns
                     --  Empty if no such expression is found.

                     ---------------------
                     -- Associated_Expr --
                     ---------------------

                     function Associated_Expr
                       (Comp_Id : Entity_Id;
                        Associations : List_Id) return Node_Id
                     is
                        Assoc  : Node_Id;
                        Choice : Node_Id;

                     begin
                        --  Simple linear search seems ok here

                        Assoc := First (Associations);
                        while Present (Assoc) loop
                           Choice := First (Choices (Assoc));
                           while Present (Choice) loop
                              if (Nkind (Choice) = N_Identifier
                                   and then Chars (Choice) = Chars (Comp_Id))
                                or else (Nkind (Choice) = N_Others_Choice)
                              then
                                 return Expression (Assoc);
                              end if;

                              Next (Choice);
                           end loop;

                           Next (Assoc);
                        end loop;

                        return Empty;
                     end Associated_Expr;

                  --  Start of processing for Expand_Simple_Function_Return

                  begin
                     if not Positionals_Exhausted then
                        Disc_Exp := First (Expressions (Discrim_Source));
                     end if;

                     loop
                        if Positionals_Exhausted then
                           Disc_Exp :=
                             Associated_Expr
                               (Discrim,
                                Component_Associations (Discrim_Source));
                        end if;

                        if Ekind (Etype (Discrim)) =
                             E_Anonymous_Access_Type
                        then
                           Check_Against_Result_Level
                             (Dynamic_Accessibility_Level (Disc_Exp));
                        end if;

                        Next_Discriminant (Discrim);
                        exit when not Present (Discrim);

                        if not Positionals_Exhausted then
                           Next (Disc_Exp);
                           Positionals_Exhausted := not Present (Disc_Exp);
                        end if;
                     end loop;
                  end;

               when N_Function_Call =>

                  --  No check needed (check performed by callee)

                  null;

               when others =>

                  declare
                     Level : constant Node_Id :=
                               Make_Integer_Literal (Loc,
                                 Object_Access_Level (Discrim_Source));

                  begin
                     --  Unimplemented: check for name prefix that includes
                     --  a dereference of an access value with a dynamic
                     --  accessibility level (e.g., an access param or a
                     --  saooaaat) and use dynamic level in that case. For
                     --  example:
                     --    return Access_Param.all(Some_Index).Some_Component;
                     --  ???

                     Set_Etype (Level, Standard_Natural);
                     Check_Against_Result_Level (Level);
                  end;

            end case;
         end;
      end if;

      --  If we are returning an object that may not be bit-aligned, then copy
      --  the value into a temporary first. This copy may need to expand to a
      --  loop of component operations.

      if Is_Possibly_Unaligned_Slice (Exp)
        or else Is_Possibly_Unaligned_Object (Exp)
      then
         declare
            ExpR : constant Node_Id   := Relocate_Node (Exp);
            Tnn  : constant Entity_Id := Make_Temporary (Loc, 'T', ExpR);
         begin
            Insert_Action (Exp,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Tnn,
                Constant_Present    => True,
                Object_Definition   => New_Occurrence_Of (R_Type, Loc),
                Expression          => ExpR),
              Suppress => All_Checks);
            Rewrite (Exp, New_Occurrence_Of (Tnn, Loc));
         end;
      end if;

      --  Generate call to postcondition checks if they are present

      if Ekind (Scope_Id) = E_Function
        and then Has_Postconditions (Scope_Id)
      then
         --  We are going to reference the returned value twice in this case,
         --  once in the call to _Postconditions, and once in the actual return
         --  statement, but we can't have side effects happening twice, and in
         --  any case for efficiency we don't want to do the computation twice.

         --  If the returned expression is an entity name, we don't need to
         --  worry since it is efficient and safe to reference it twice, that's
         --  also true for literals other than string literals, and for the
         --  case of X.all where X is an entity name.

         if Is_Entity_Name (Exp)
           or else Nkind_In (Exp, N_Character_Literal,
                                  N_Integer_Literal,
                                  N_Real_Literal)
           or else (Nkind (Exp) = N_Explicit_Dereference
                     and then Is_Entity_Name (Prefix (Exp)))
         then
            null;

         --  Otherwise we are going to need a temporary to capture the value

         else
            declare
               ExpR : Node_Id            := Relocate_Node (Exp);
               Tnn  : constant Entity_Id := Make_Temporary (Loc, 'T', ExpR);

            begin
               --  In the case of discriminated objects, we have created a
               --  constrained subtype above, and used the underlying type.
               --  This transformation is post-analysis and harmless, except
               --  that now the call to the post-condition will be analyzed and
               --  type kinds have to match.

               if Nkind (ExpR) = N_Unchecked_Type_Conversion
                 and then
                   Is_Private_Type (R_Type) /= Is_Private_Type (Etype (ExpR))
               then
                  ExpR := Expression (ExpR);
               end if;

               --  For a complex expression of an elementary type, capture
               --  value in the temporary and use it as the reference.

               if Is_Elementary_Type (R_Type) then
                  Insert_Action (Exp,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier => Tnn,
                      Constant_Present    => True,
                      Object_Definition   => New_Occurrence_Of (R_Type, Loc),
                      Expression          => ExpR),
                    Suppress => All_Checks);

                  Rewrite (Exp, New_Occurrence_Of (Tnn, Loc));

               --  If we have something we can rename, generate a renaming of
               --  the object and replace the expression with a reference

               elsif Is_Object_Reference (Exp) then
                  Insert_Action (Exp,
                    Make_Object_Renaming_Declaration (Loc,
                      Defining_Identifier => Tnn,
                      Subtype_Mark        => New_Occurrence_Of (R_Type, Loc),
                      Name                => ExpR),
                    Suppress => All_Checks);

                  Rewrite (Exp, New_Occurrence_Of (Tnn, Loc));

               --  Otherwise we have something like a string literal or an
               --  aggregate. We could copy the value, but that would be
               --  inefficient. Instead we make a reference to the value and
               --  capture this reference with a renaming, the expression is
               --  then replaced by a dereference of this renaming.

               else
                  --  For now, copy the value, since the code below does not
                  --  seem to work correctly ???

                  Insert_Action (Exp,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier => Tnn,
                      Constant_Present    => True,
                      Object_Definition   => New_Occurrence_Of (R_Type, Loc),
                      Expression          => Relocate_Node (Exp)),
                    Suppress => All_Checks);

                  Rewrite (Exp, New_Occurrence_Of (Tnn, Loc));

                  --  Insert_Action (Exp,
                  --    Make_Object_Renaming_Declaration (Loc,
                  --      Defining_Identifier => Tnn,
                  --      Access_Definition =>
                  --        Make_Access_Definition (Loc,
                  --          All_Present  => True,
                  --          Subtype_Mark => New_Occurrence_Of (R_Type, Loc)),
                  --      Name =>
                  --        Make_Reference (Loc,
                  --          Prefix => Relocate_Node (Exp))),
                  --    Suppress => All_Checks);

                  --  Rewrite (Exp,
                  --    Make_Explicit_Dereference (Loc,
                  --      Prefix => New_Occurrence_Of (Tnn, Loc)));
               end if;
            end;
         end if;

         --  Generate call to _postconditions

         Insert_Action (Exp,
           Make_Procedure_Call_Statement (Loc,
             Name => Make_Identifier (Loc, Name_uPostconditions),
             Parameter_Associations => New_List (Duplicate_Subexpr (Exp))));
      end if;

      --  Ada 2005 (AI-251): If this return statement corresponds with an
      --  simple return statement associated with an extended return statement
      --  and the type of the returned object is an interface then generate an
      --  implicit conversion to force displacement of the "this" pointer.

      if Ada_Version >= Ada_2005
        and then Comes_From_Extended_Return_Statement (N)
        and then Nkind (Expression (N)) = N_Identifier
        and then Is_Interface (Utyp)
        and then Utyp /= Underlying_Type (Exptyp)
      then
         Rewrite (Exp, Convert_To (Utyp, Relocate_Node (Exp)));
         Analyze_And_Resolve (Exp);
      end if;
   end Expand_Simple_Function_Return;

   --------------------------------
   -- Expand_Subprogram_Contract --
   --------------------------------

   procedure Expand_Subprogram_Contract
     (N       : Node_Id;
      Spec_Id : Entity_Id;
      Body_Id : Entity_Id)
   is
      procedure Add_Invariant_And_Predicate_Checks
        (Subp_Id : Entity_Id;
         Stmts   : in out List_Id;
         Result  : out Node_Id);
      --  Process the result of function Subp_Id (if applicable) and all its
      --  formals. Add invariant and predicate checks where applicable. The
      --  routine appends all the checks to list Stmts. If Subp_Id denotes a
      --  function, Result contains the entity of parameter _Result, to be
      --  used in the creation of procedure _Postconditions.

      procedure Append_Enabled_Item (Item : Node_Id; List : in out List_Id);
      --  Append a node to a list. If there is no list, create a new one. When
      --  the item denotes a pragma, it is added to the list only when it is
      --  enabled.

      procedure Build_Postconditions_Procedure
        (Subp_Id : Entity_Id;
         Stmts   : List_Id;
         Result  : Entity_Id);
      --  Create the body of procedure _Postconditions which handles various
      --  assertion actions on exit from subprogram Subp_Id. Stmts is the list
      --  of statements to be checked on exit. Parameter Result is the entity
      --  of parameter _Result when Subp_Id denotes a function.

      function Build_Pragma_Check_Equivalent
        (Prag     : Node_Id;
         Subp_Id  : Entity_Id := Empty;
         Inher_Id : Entity_Id := Empty) return Node_Id;
      --  Transform a [refined] pre- or postcondition denoted by Prag into an
      --  equivalent pragma Check. When the pre- or postcondition is inherited,
      --  the routine corrects the references of all formals of Inher_Id to
      --  point to the formals of Subp_Id.

      procedure Collect_Body_Postconditions (Stmts : in out List_Id);
      --  Process all postconditions found in the declarations of the body. The
      --  routine appends the pragma Check equivalents to list Stmts.

      procedure Collect_Spec_Postconditions
        (Subp_Id : Entity_Id;
         Stmts   : in out List_Id);
      --  Process all [inherited] postconditions of subprogram spec Subp_Id.
      --  The routine appends the pragma Check equivalents to list Stmts.

      procedure Collect_Spec_Preconditions (Subp_Id : Entity_Id);
      --  Process all [inherited] preconditions of subprogram spec Subp_Id. The
      --  routine prepends the pragma Check equivalents to the declarations of
      --  the body.

      procedure Prepend_To_Declarations (Item : Node_Id);
      --  Prepend a single item to the declarations of the subprogram body

      procedure Process_Contract_Cases
        (Subp_Id : Entity_Id;
         Stmts   : in out List_Id);
      --  Process pragma Contract_Cases of subprogram spec Subp_Id. The routine
      --  appends the expanded code to list Stmts.

      ----------------------------------------
      -- Add_Invariant_And_Predicate_Checks --
      ----------------------------------------

      procedure Add_Invariant_And_Predicate_Checks
        (Subp_Id : Entity_Id;
         Stmts   : in out List_Id;
         Result  : out Node_Id)
      is
         procedure Add_Invariant_Access_Checks (Id : Entity_Id);
         --  Id denotes the return value of a function or a formal parameter.
         --  Add an invariant check if the type of Id is access to a type with
         --  invariants. The routine appends the generated code to Stmts.

         function Invariant_Checks_OK (Typ : Entity_Id) return Boolean;
         --  Determine whether type Typ can benefit from invariant checks. To
         --  qualify, the type must have a non-null invariant procedure and
         --  subprogram Subp_Id must appear visible from the point of view of
         --  the type.

         function Predicate_Checks_OK (Typ : Entity_Id) return Boolean;
         --  Determine whether type Typ can benefit from predicate checks. To
         --  qualify, the type must have at least one checked predicate.

         ---------------------------------
         -- Add_Invariant_Access_Checks --
         ---------------------------------

         procedure Add_Invariant_Access_Checks (Id : Entity_Id) is
            Loc : constant Source_Ptr := Sloc (N);
            Ref : Node_Id;
            Typ : Entity_Id;

         begin
            Typ := Etype (Id);

            if Is_Access_Type (Typ) and then not Is_Access_Constant (Typ) then
               Typ := Designated_Type (Typ);

               if Invariant_Checks_OK (Typ) then
                  Ref :=
                    Make_Explicit_Dereference (Loc,
                      Prefix => New_Occurrence_Of (Id, Loc));
                  Set_Etype (Ref, Typ);

                  --  Generate:
                  --    if <Id> /= null then
                  --       <invariant_call (<Ref>)>
                  --    end if;

                  Append_Enabled_Item
                    (Item =>
                       Make_If_Statement (Loc,
                         Condition =>
                           Make_Op_Ne (Loc,
                             Left_Opnd  => New_Occurrence_Of (Id, Loc),
                             Right_Opnd => Make_Null (Loc)),
                         Then_Statements => New_List (
                           Make_Invariant_Call (Ref))),
                     List => Stmts);
               end if;
            end if;
         end Add_Invariant_Access_Checks;

         -------------------------
         -- Invariant_Checks_OK --
         -------------------------

         function Invariant_Checks_OK (Typ : Entity_Id) return Boolean is
            function Has_Null_Body (Proc_Id : Entity_Id) return Boolean;
            --  Determine whether the body of procedure Proc_Id contains a sole
            --  null statement, possibly followed by an optional return.

            function Has_Public_Visibility_Of_Subprogram return Boolean;
            --  Determine whether type Typ has public visibility of subprogram
            --  Subp_Id.

            -------------------
            -- Has_Null_Body --
            -------------------

            function Has_Null_Body (Proc_Id : Entity_Id) return Boolean is
               Body_Id : Entity_Id;
               Decl    : Node_Id;
               Spec    : Node_Id;
               Stmt1   : Node_Id;
               Stmt2   : Node_Id;

            begin
               Spec := Parent (Proc_Id);
               Decl := Parent (Spec);

               --  Retrieve the entity of the invariant procedure body

               if Nkind (Spec) = N_Procedure_Specification
                 and then Nkind (Decl) = N_Subprogram_Declaration
               then
                  Body_Id := Corresponding_Body (Decl);

               --  The body acts as a spec

               else
                  Body_Id := Proc_Id;
               end if;

               --  The body will be generated later

               if No (Body_Id) then
                  return False;
               end if;

               Spec := Parent (Body_Id);
               Decl := Parent (Spec);

               pragma Assert
                 (Nkind (Spec) = N_Procedure_Specification
                   and then Nkind (Decl) = N_Subprogram_Body);

               Stmt1 := First (Statements (Handled_Statement_Sequence (Decl)));

               --  Look for a null statement followed by an optional return
               --  statement.

               if Nkind (Stmt1) = N_Null_Statement then
                  Stmt2 := Next (Stmt1);

                  if Present (Stmt2) then
                     return Nkind (Stmt2) = N_Simple_Return_Statement;
                  else
                     return True;
                  end if;
               end if;

               return False;
            end Has_Null_Body;

            -----------------------------------------
            -- Has_Public_Visibility_Of_Subprogram --
            -----------------------------------------

            function Has_Public_Visibility_Of_Subprogram return Boolean is
               Subp_Decl : constant Node_Id := Unit_Declaration_Node (Subp_Id);

            begin
               --  An Initialization procedure must be considered visible even
               --  though it is internally generated.

               if Is_Init_Proc (Defining_Entity (Subp_Decl)) then
                  return True;

               elsif Ekind (Scope (Typ)) /= E_Package then
                  return False;

               --  Internally generated code is never publicly visible except
               --  for a subprogram that is the implementation of an expression
               --  function. In that case the visibility is determined by the
               --  last check.

               elsif not Comes_From_Source (Subp_Decl)
                 and then
                   (Nkind (Original_Node (Subp_Decl)) /= N_Expression_Function
                      or else not
                        Comes_From_Source (Defining_Entity (Subp_Decl)))
               then
                  return False;

               --  Determine whether the subprogram is declared in the visible
               --  declarations of the package containing the type.

               else
                  return List_Containing (Subp_Decl) =
                    Visible_Declarations
                      (Specification (Unit_Declaration_Node (Scope (Typ))));
               end if;
            end Has_Public_Visibility_Of_Subprogram;

         --  Start of processing for Invariant_Checks_OK

         begin
            return
              Has_Invariants (Typ)
                and then Present (Invariant_Procedure (Typ))
                and then not Has_Null_Body (Invariant_Procedure (Typ))
                and then Has_Public_Visibility_Of_Subprogram;
         end Invariant_Checks_OK;

         -------------------------
         -- Predicate_Checks_OK --
         -------------------------

         function Predicate_Checks_OK (Typ : Entity_Id) return Boolean is
            function Has_Checked_Predicate return Boolean;
            --  Determine whether type Typ has or inherits at least one
            --  predicate aspect or pragma, for which the applicable policy is
            --  Checked.

            ---------------------------
            -- Has_Checked_Predicate --
            ---------------------------

            function Has_Checked_Predicate return Boolean is
               Anc  : Entity_Id;
               Pred : Node_Id;

            begin
               --  Climb the ancestor type chain staring from the input. This
               --  is done because the input type may lack aspect/pragma
               --  predicate and simply inherit those from its ancestor.

               --  Note that predicate pragmas correspond to all three cases
               --  of predicate aspects (Predicate, Dynamic_Predicate, and
               --  Static_Predicate), so this routine checks for all three
               --  cases.

               Anc := Typ;
               while Present (Anc) loop
                  Pred := Get_Pragma (Anc, Pragma_Predicate);

                  if Present (Pred) and then not Is_Ignored (Pred) then
                     return True;
                  end if;

                  Anc := Nearest_Ancestor (Anc);
               end loop;

               return False;
            end Has_Checked_Predicate;

         --  Start of processing for Predicate_Checks_OK

         begin
            return
              Has_Predicates (Typ)
                and then Present (Predicate_Function (Typ))
                and then Has_Checked_Predicate;
         end Predicate_Checks_OK;

         --  Local variables

         Loc    : constant Source_Ptr := Sloc (N);
         Formal : Entity_Id;
         Typ    : Entity_Id;

      --  Start of processing for Add_Invariant_And_Predicate_Checks

      begin
         Result := Empty;

         --  Do not generate any checks if no code is being generated

         if not Expander_Active then
            return;
         end if;

         --  Process the result of a function

         if Ekind_In (Subp_Id, E_Function, E_Generic_Function) then
            Typ := Etype (Subp_Id);

            --  Generate _Result which is used in procedure _Postconditions to
            --  verify the return value.

            Result := Make_Defining_Identifier (Loc, Name_uResult);
            Set_Etype (Result, Typ);

            --  Add an invariant check when the return type has invariants and
            --  the related function is visible to the outside.

            if Invariant_Checks_OK (Typ) then
               Append_Enabled_Item
                 (Item =>
                    Make_Invariant_Call (New_Occurrence_Of (Result, Loc)),
                  List => Stmts);
            end if;

            --  Add an invariant check when the return type is an access to a
            --  type with invariants.

            Add_Invariant_Access_Checks (Result);
         end if;

         --  Add invariant and predicates for all formals that qualify

         Formal := First_Formal (Subp_Id);
         while Present (Formal) loop
            Typ := Etype (Formal);

            if Ekind (Formal) /= E_In_Parameter
              or else Is_Access_Type (Typ)
            then
               if Invariant_Checks_OK (Typ) then
                  Append_Enabled_Item
                    (Item =>
                       Make_Invariant_Call (New_Occurrence_Of (Formal, Loc)),
                     List => Stmts);
               end if;

               Add_Invariant_Access_Checks (Formal);

               if Predicate_Checks_OK (Typ) then
                  Append_Enabled_Item
                    (Item =>
                       Make_Predicate_Check
                         (Typ, New_Reference_To (Formal, Loc)),
                     List => Stmts);
               end if;
            end if;

            Next_Formal (Formal);
         end loop;
      end Add_Invariant_And_Predicate_Checks;

      -------------------------
      -- Append_Enabled_Item --
      -------------------------

      procedure Append_Enabled_Item (Item : Node_Id; List : in out List_Id) is
      begin
         --  Do not chain ignored or disabled pragmas

         if Nkind (Item) = N_Pragma
           and then (Is_Ignored (Item) or else Is_Disabled (Item))
         then
            null;

         --  Otherwise, add the item

         else
            if No (List) then
               List := New_List;
            end if;

            --  If the pragma is a conjunct in a composite postcondition, it
            --  has been processed in reverse order. In the postcondition body
            --  if must appear before the others.

            if Nkind (Item) = N_Pragma
              and then From_Aspect_Specification (Item)
              and then Split_PPC (Item)
            then
               Prepend (Item, List);
            else
               Append (Item, List);
            end if;
         end if;
      end Append_Enabled_Item;

      ------------------------------------
      -- Build_Postconditions_Procedure --
      ------------------------------------

      procedure Build_Postconditions_Procedure
        (Subp_Id : Entity_Id;
         Stmts   : List_Id;
         Result  : Entity_Id)
      is
         procedure Insert_After_Last_Declaration (Stmt : Node_Id);
         --  Insert node Stmt after the last declaration of the subprogram body

         -----------------------------------
         -- Insert_After_Last_Declaration --
         -----------------------------------

         procedure Insert_After_Last_Declaration (Stmt : Node_Id) is
            Decls : List_Id := Declarations (N);

         begin
            --  Ensure that the body has a declaration list

            if No (Decls) then
               Decls := New_List;
               Set_Declarations (N, Decls);
            end if;

            Append_To (Decls, Stmt);
         end Insert_After_Last_Declaration;

         --  Local variables

         Loc     : constant Source_Ptr := Sloc (N);
         Params  : List_Id := No_List;
         Proc_Id : Entity_Id;

      --  Start of processing for Build_Postconditions_Procedure

      begin
         --  Do not create the routine if no code is being generated

         if not Expander_Active then
            return;

         --  Nothing to do if there are no actions to check on exit

         elsif No (Stmts) then
            return;
         end if;

         Proc_Id := Make_Defining_Identifier (Loc, Name_uPostconditions);

         --  The related subprogram is a function, create the specification of
         --  parameter _Result.

         if Present (Result) then
            Params := New_List (
              Make_Parameter_Specification (Loc,
                Defining_Identifier => Result,
                Parameter_Type      =>
                  New_Reference_To (Etype (Result), Loc)));
         end if;

         --  Insert _Postconditions after the last declaration of the body.
         --  This ensures that the body will not cause any premature freezing
         --  as it may mention types:

         --    procedure Proc (Obj : Array_Typ) is
         --       procedure _postconditions is
         --       begin
         --          ... Obj ...
         --       end _postconditions;

         --       subtype T is Array_Typ (Obj'First (1) .. Obj'Last (1));
         --    begin

         --  In the example above, Obj is of type T but the incorrect placement
         --  of _Postconditions will cause a crash in gigi due to an out of
         --  order reference. The body of _Postconditions must be placed after
         --  the declaration of Temp to preserve correct visibility.

         Insert_After_Last_Declaration (
           Make_Subprogram_Body (Loc,
             Specification              =>
               Make_Procedure_Specification (Loc,
                 Defining_Unit_Name       => Proc_Id,
                 Parameter_Specifications => Params),

             Declarations               => Empty_List,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc, Stmts)));

         --  Set the attributes of the related subprogram to capture the
         --  generated procedure.

         if Ekind_In (Subp_Id, E_Generic_Procedure, E_Procedure) then
            Set_Postcondition_Proc (Subp_Id, Proc_Id);
         end if;

         Set_Has_Postconditions (Subp_Id);
      end Build_Postconditions_Procedure;

      -----------------------------------
      -- Build_Pragma_Check_Equivalent --
      -----------------------------------

      function Build_Pragma_Check_Equivalent
        (Prag     : Node_Id;
         Subp_Id  : Entity_Id := Empty;
         Inher_Id : Entity_Id := Empty) return Node_Id
      is
         Loc          : constant Source_Ptr := Sloc (Prag);
         Prag_Nam     : constant Name_Id    := Pragma_Name (Prag);
         Check_Prag   : Node_Id;
         Formals_Map  : Elist_Id;
         Inher_Formal : Entity_Id;
         Msg_Arg      : Node_Id;
         Nam          : Name_Id;
         Subp_Formal  : Entity_Id;

      begin
         Formals_Map := No_Elist;

         --  When the pre- or postcondition is inherited, map the formals of
         --  the inherited subprogram to those of the current subprogram.

         if Present (Inher_Id) then
            pragma Assert (Present (Subp_Id));

            Formals_Map := New_Elmt_List;

            --  Create a relation <inherited formal> => <subprogram formal>

            Inher_Formal := First_Formal (Inher_Id);
            Subp_Formal  := First_Formal (Subp_Id);
            while Present (Inher_Formal) and then Present (Subp_Formal) loop
               Append_Elmt (Inher_Formal, Formals_Map);
               Append_Elmt (Subp_Formal, Formals_Map);

               Next_Formal (Inher_Formal);
               Next_Formal (Subp_Formal);
            end loop;
         end if;

         --  Copy the original pragma while performing substitutions (if
         --  applicable).

         Check_Prag :=
           New_Copy_Tree
             (Source    => Prag,
              Map       => Formals_Map,
              New_Scope => Current_Scope);

         --  Mark the pragma as being internally generated and reset the
         --  Analyzed flag.

         Set_Comes_From_Source (Check_Prag, False);
         Set_Analyzed          (Check_Prag, False);

         --  For a postcondition pragma within a generic, preserve the pragma
         --  for later expansion. This is also used when an error was detected,
         --  thus setting Expander_Active to False.

         if Prag_Nam = Name_Postcondition and then not Expander_Active then
            return Check_Prag;
         end if;

         if Present (Corresponding_Aspect (Prag)) then
            Nam := Chars (Identifier (Corresponding_Aspect (Prag)));
         else
            Nam := Prag_Nam;
         end if;

         --  Convert the copy into pragma Check by correcting the name and
         --  adding a check_kind argument.

         Set_Pragma_Identifier
           (Check_Prag, Make_Identifier (Loc, Name_Check));

         Prepend_To (Pragma_Argument_Associations (Check_Prag),
           Make_Pragma_Argument_Association (Loc,
             Expression => Make_Identifier (Loc, Nam)));

         --  Update the error message when the pragma is inherited

         if Present (Inher_Id) then
            Msg_Arg := Last (Pragma_Argument_Associations (Check_Prag));

            if Chars (Msg_Arg) = Name_Message then
               String_To_Name_Buffer (Strval (Expression (Msg_Arg)));

               --  Insert "inherited" to improve the error message

               if Name_Buffer (1 .. 8) = "failed p" then
                  Insert_Str_In_Name_Buffer ("inherited ", 8);
                  Set_Strval (Expression (Msg_Arg), String_From_Name_Buffer);
               end if;
            end if;
         end if;

         return Check_Prag;
      end Build_Pragma_Check_Equivalent;

      ---------------------------------
      -- Collect_Body_Postconditions --
      ---------------------------------

      procedure Collect_Body_Postconditions (Stmts : in out List_Id) is
         procedure Collect_Body_Postconditions_Of_Kind (Post_Nam : Name_Id);
         --  Process postconditions of a particular kind denoted by Post_Nam

         -----------------------------------------
         -- Collect_Body_Postconditions_Of_Kind --
         -----------------------------------------

         procedure Collect_Body_Postconditions_Of_Kind (Post_Nam : Name_Id) is
            Check_Prag : Node_Id;
            Decl       : Node_Id;

         begin
            pragma Assert (Nam_In (Post_Nam, Name_Postcondition,
                                             Name_Refined_Post));

            --  Inspect the declarations of the subprogram body looking for a
            --  pragma that matches the desired name.

            Decl := First (Declarations (N));
            while Present (Decl) loop
               if Nkind (Decl) = N_Pragma then
                  if Pragma_Name (Decl) = Post_Nam then
                     Analyze (Decl);
                     Check_Prag := Build_Pragma_Check_Equivalent (Decl);

                     if Expander_Active then
                        Append_Enabled_Item
                          (Item => Check_Prag,
                           List => Stmts);

                     --  When analyzing a generic unit, save the pragma for
                     --  later.

                     else
                        Prepend_To_Declarations (Check_Prag);
                     end if;
                  end if;

               --  Skip internally generated code

               elsif not Comes_From_Source (Decl) then
                  null;

               --  Postconditions in bodies are usually grouped at the top of
               --  the declarations. There is no point in inspecting the whole
               --  source list.

               else
                  exit;
               end if;

               Next (Decl);
            end loop;
         end Collect_Body_Postconditions_Of_Kind;

      --  Start of processing for Collect_Body_Postconditions

      begin
         Collect_Body_Postconditions_Of_Kind (Name_Refined_Post);
         Collect_Body_Postconditions_Of_Kind (Name_Postcondition);
      end Collect_Body_Postconditions;

      ---------------------------------
      -- Collect_Spec_Postconditions --
      ---------------------------------

      procedure Collect_Spec_Postconditions
        (Subp_Id : Entity_Id;
         Stmts   : in out List_Id)
      is
         Inher_Subps   : constant Subprogram_List :=
                           Inherited_Subprograms (Subp_Id);
         Check_Prag    : Node_Id;
         Prag          : Node_Id;
         Inher_Subp_Id : Entity_Id;

      begin
         --  Process the contract of the spec

         Prag := Pre_Post_Conditions (Contract (Subp_Id));
         while Present (Prag) loop
            if Pragma_Name (Prag) = Name_Postcondition then
               Check_Prag := Build_Pragma_Check_Equivalent (Prag);

               if Expander_Active then
                  Append_Enabled_Item
                    (Item => Check_Prag,
                     List => Stmts);

               --  When analyzing a generic unit, save the pragma for later

               else
                  Prepend_To_Declarations (Check_Prag);
               end if;
            end if;

            Prag := Next_Pragma (Prag);
         end loop;

         --  Process the contracts of all inherited subprograms, looking for
         --  class-wide postconditions.

         for Index in Inher_Subps'Range loop
            Inher_Subp_Id := Inher_Subps (Index);

            Prag := Pre_Post_Conditions (Contract (Inher_Subp_Id));
            while Present (Prag) loop
               if Pragma_Name (Prag) = Name_Postcondition
                 and then Class_Present (Prag)
               then
                  Check_Prag :=
                    Build_Pragma_Check_Equivalent
                      (Prag     => Prag,
                       Subp_Id  => Subp_Id,
                       Inher_Id => Inher_Subp_Id);

                  if Expander_Active then
                     Append_Enabled_Item
                       (Item => Check_Prag,
                        List => Stmts);

                  --  When analyzing a generic unit, save the pragma for later

                  else
                     Prepend_To_Declarations (Check_Prag);
                  end if;
               end if;

               Prag := Next_Pragma (Prag);
            end loop;
         end loop;
      end Collect_Spec_Postconditions;

      --------------------------------
      -- Collect_Spec_Preconditions --
      --------------------------------

      procedure Collect_Spec_Preconditions (Subp_Id : Entity_Id) is
         procedure Merge_Preconditions (From : Node_Id; Into : Node_Id);
         --  Merge two class-wide preconditions by "or else"-ing them. The
         --  changes are accumulated in parameter Into. Update the error
         --  message of Into.

         -------------------------
         -- Merge_Preconditions --
         -------------------------

         procedure Merge_Preconditions (From : Node_Id; Into : Node_Id) is
            function Expression_Arg (Prag : Node_Id) return Node_Id;
            --  Return the boolean expression argument of a precondition while
            --  updating its parenteses count for the subsequent merge.

            function Message_Arg (Prag : Node_Id) return Node_Id;
            --  Return the message argument of a precondition

            --------------------
            -- Expression_Arg --
            --------------------

            function Expression_Arg (Prag : Node_Id) return Node_Id is
               Args : constant List_Id := Pragma_Argument_Associations (Prag);
               Arg  : constant Node_Id := Get_Pragma_Arg (Next (First (Args)));

            begin
               if Paren_Count (Arg) = 0 then
                  Set_Paren_Count (Arg, 1);
               end if;

               return Arg;
            end Expression_Arg;

            -----------------
            -- Message_Arg --
            -----------------

            function Message_Arg (Prag : Node_Id) return Node_Id is
               Args : constant List_Id := Pragma_Argument_Associations (Prag);
            begin
               return Get_Pragma_Arg (Last (Args));
            end Message_Arg;

            --  Local variables

            From_Expr : constant Node_Id := Expression_Arg (From);
            From_Msg  : constant Node_Id := Message_Arg    (From);
            Into_Expr : constant Node_Id := Expression_Arg (Into);
            Into_Msg  : constant Node_Id := Message_Arg    (Into);
            Loc       : constant Source_Ptr := Sloc (Into);

         --  Start of processing for Merge_Preconditions

         begin
            --  Merge the two preconditions by "or else"-ing them

            Rewrite (Into_Expr,
              Make_Or_Else (Loc,
                Right_Opnd => Relocate_Node (Into_Expr),
                Left_Opnd  => From_Expr));

            --  Merge the two error messages to produce a single message of the
            --  form:

            --    failed precondition from ...
            --      also failed inherited precondition from ...

            if not Exception_Locations_Suppressed then
               Start_String (Strval (Into_Msg));
               Store_String_Char (ASCII.LF);
               Store_String_Chars ("  also ");
               Store_String_Chars (Strval (From_Msg));

               Set_Strval (Into_Msg, End_String);
            end if;
         end Merge_Preconditions;

         --  Local variables

         Inher_Subps   : constant Subprogram_List :=
                           Inherited_Subprograms (Subp_Id);
         Check_Prag    : Node_Id;
         Class_Pre     : Node_Id := Empty;
         Inher_Subp_Id : Entity_Id;
         Prag          : Node_Id;

      --  Start of processing for Collect_Spec_Preconditions

      begin
         --  Process the contract of the spec

         Prag := Pre_Post_Conditions (Contract (Subp_Id));
         while Present (Prag) loop
            if Pragma_Name (Prag) = Name_Precondition then
               Check_Prag := Build_Pragma_Check_Equivalent (Prag);

               --  Save the sole class-wide precondition (if any) for the next
               --  step where it will be merged with inherited preconditions.

               if Class_Present (Prag) then
                  Class_Pre := Check_Prag;

               --  Accumulate the corresponding Check pragmas to the top of the
               --  declarations. Prepending the items ensures that they will
               --  be evaluated in their original order.

               else
                  Prepend_To_Declarations (Check_Prag);
               end if;
            end if;

            Prag := Next_Pragma (Prag);
         end loop;

         --  Process the contracts of all inherited subprograms, looking for
         --  class-wide preconditions.

         for Index in Inher_Subps'Range loop
            Inher_Subp_Id := Inher_Subps (Index);

            Prag := Pre_Post_Conditions (Contract (Inher_Subp_Id));
            while Present (Prag) loop
               if Pragma_Name (Prag) = Name_Precondition
                 and then Class_Present (Prag)
               then
                  Check_Prag :=
                    Build_Pragma_Check_Equivalent
                      (Prag     => Prag,
                       Subp_Id  => Subp_Id,
                       Inher_Id => Inher_Subp_Id);

                  --  The spec or an inherited subprogram already yielded a
                  --  class-wide precondition. Merge the existing precondition
                  --  with the current one using "or else".

                  if Present (Class_Pre) then
                     Merge_Preconditions (Check_Prag, Class_Pre);
                  else
                     Class_Pre := Check_Prag;
                  end if;
               end if;

               Prag := Next_Pragma (Prag);
            end loop;
         end loop;

         --  Add the merged class-wide preconditions (if any)

         if Present (Class_Pre) then
            Prepend_To_Declarations (Class_Pre);
         end if;
      end Collect_Spec_Preconditions;

      -----------------------------
      -- Prepend_To_Declarations --
      -----------------------------

      procedure Prepend_To_Declarations (Item : Node_Id) is
         Decls : List_Id := Declarations (N);

      begin
         --  Ensure that the body has a declarative list

         if No (Decls) then
            Decls := New_List;
            Set_Declarations (N, Decls);
         end if;

         Prepend_To (Decls, Item);
      end Prepend_To_Declarations;

      ----------------------------
      -- Process_Contract_Cases --
      ----------------------------

      procedure Process_Contract_Cases
        (Subp_Id : Entity_Id;
         Stmts   : in out List_Id)
      is
         Prag : Node_Id;

      begin
         --  Do not build the Contract_Cases circuitry if no code is being
         --  generated.

         if not Expander_Active then
            return;
         end if;

         Prag := Contract_Test_Cases (Contract (Subp_Id));
         while Present (Prag) loop
            if Pragma_Name (Prag) = Name_Contract_Cases then
               Expand_Contract_Cases
                 (CCs     => Prag,
                  Subp_Id => Subp_Id,
                  Decls   => Declarations (N),
                  Stmts   => Stmts);
            end if;

            Prag := Next_Pragma (Prag);
         end loop;
      end Process_Contract_Cases;

      --  Local variables

      Post_Stmts : List_Id := No_List;
      Result     : Entity_Id;
      Subp_Id    : Entity_Id;

   --  Start of processing for Expand_Subprogram_Contract

   begin
      if Present (Spec_Id) then
         Subp_Id := Spec_Id;
      else
         Subp_Id := Body_Id;
      end if;

      --  Do not process a predicate function as its body will end up with a
      --  recursive call to itself and blow up the stack.

      if Ekind (Subp_Id) = E_Function
        and then Is_Predicate_Function (Subp_Id)
      then
         return;

      --  Do not process TSS subprograms

      elsif Get_TSS_Name (Subp_Id) /= TSS_Null then
         return;
      end if;

      --  The expansion of a subprogram contract involves the relocation of
      --  various contract assertions to the declarations of the body in a
      --  particular order. The order is as follows:

      --    function Example (...) return ... is
      --       procedure _Postconditions (...) is
      --       begin
      --          <refined postconditions from body>
      --          <postconditions from body>
      --          <postconditions from spec>
      --          <inherited postconditions>
      --          <contract case consequences>
      --          <invariant check of function result (if applicable)>
      --          <invariant and predicate checks of parameters>
      --       end _Postconditions;

      --       <inherited preconditions>
      --       <preconditions from spec>
      --       <preconditions from body>
      --       <refined preconditions from body>
      --       <contract case conditions>

      --       <source declarations>
      --    begin
      --       <source statements>

      --       _Preconditions (Result);
      --       return Result;
      --    end Example;

      --  Routine _Postconditions holds all contract assertions that must be
      --  verified on exit from the related routine.

      --  Collect all [inherited] preconditions from the spec, transform them
      --  into Check pragmas and add them to the declarations of the body in
      --  the order outlined above.

      if Present (Spec_Id) then
         Collect_Spec_Preconditions (Spec_Id);
      end if;

      --  Transform all [refined] postconditions of the body into Check
      --  pragmas. The resulting pragmas are accumulated in list Post_Stmts.

      Collect_Body_Postconditions (Post_Stmts);

      --  Transform all [inherited] postconditions from the spec into Check
      --  pragmas. The resulting pragmas are accumulated in list Post_Stmts.

      if Present (Spec_Id) then
         Collect_Spec_Postconditions (Spec_Id, Post_Stmts);

         --  Transform pragma Contract_Cases from the spec into its circuitry

         Process_Contract_Cases (Spec_Id, Post_Stmts);
      end if;

      --  Apply invariant and predicate checks on the result of a function (if
      --  applicable) and all formals. The resulting checks are accumulated in
      --  list Post_Stmts.

      Add_Invariant_And_Predicate_Checks (Subp_Id, Post_Stmts, Result);

      --  Construct procedure _Postconditions

      Build_Postconditions_Procedure (Subp_Id, Post_Stmts, Result);
   end Expand_Subprogram_Contract;

   --------------------------------
   -- Is_Build_In_Place_Function --
   --------------------------------

   function Is_Build_In_Place_Function (E : Entity_Id) return Boolean is
   begin
      --  This function is called from Expand_Subtype_From_Expr during
      --  semantic analysis, even when expansion is off. In those cases
      --  the build_in_place expansion will not take place.

      if not Expander_Active then
         return False;
      end if;

      --  For now we test whether E denotes a function or access-to-function
      --  type whose result subtype is inherently limited. Later this test
      --  may be revised to allow composite nonlimited types. Functions with
      --  a foreign convention or whose result type has a foreign convention
      --  never qualify.

      if Ekind_In (E, E_Function, E_Generic_Function)
        or else (Ekind (E) = E_Subprogram_Type
                  and then Etype (E) /= Standard_Void_Type)
      then
         --  Note: If you have Convention (C) on an inherently limited type,
         --  you're on your own. That is, the C code will have to be carefully
         --  written to know about the Ada conventions.

         if Has_Foreign_Convention (E)
           or else Has_Foreign_Convention (Etype (E))
         then
            return False;

         --  In Ada 2005 all functions with an inherently limited return type
         --  must be handled using a build-in-place profile, including the case
         --  of a function with a limited interface result, where the function
         --  may return objects of nonlimited descendants.

         else
            return Is_Limited_View (Etype (E))
              and then Ada_Version >= Ada_2005
              and then not Debug_Flag_Dot_L;
         end if;

      else
         return False;
      end if;
   end Is_Build_In_Place_Function;

   -------------------------------------
   -- Is_Build_In_Place_Function_Call --
   -------------------------------------

   function Is_Build_In_Place_Function_Call (N : Node_Id) return Boolean is
      Exp_Node    : Node_Id := N;
      Function_Id : Entity_Id;

   begin
      --  Return False if the expander is currently inactive, since awareness
      --  of build-in-place treatment is only relevant during expansion. Note
      --  that Is_Build_In_Place_Function, which is called as part of this
      --  function, is also conditioned this way, but we need to check here as
      --  well to avoid blowing up on processing protected calls when expansion
      --  is disabled (such as with -gnatc) since those would trip over the
      --  raise of Program_Error below.

      --  In SPARK mode, build-in-place calls are not expanded, so that we
      --  may end up with a call that is neither resolved to an entity, nor
      --  an indirect call.

      if not Expander_Active then
         return False;
      end if;

      --  Step past qualification or unchecked conversion (the latter can occur
      --  in cases of calls to 'Input).

      if Nkind_In (Exp_Node, N_Qualified_Expression,
                             N_Unchecked_Type_Conversion)
      then
         Exp_Node := Expression (N);
      end if;

      if Nkind (Exp_Node) /= N_Function_Call then
         return False;

      else
         if Is_Entity_Name (Name (Exp_Node)) then
            Function_Id := Entity (Name (Exp_Node));

         --  In the case of an explicitly dereferenced call, use the subprogram
         --  type generated for the dereference.

         elsif Nkind (Name (Exp_Node)) = N_Explicit_Dereference then
            Function_Id := Etype (Name (Exp_Node));

         --  This may be a call to a protected function.

         elsif Nkind (Name (Exp_Node)) = N_Selected_Component then
            Function_Id := Etype (Entity (Selector_Name (Name (Exp_Node))));

         else
            raise Program_Error;
         end if;

         return Is_Build_In_Place_Function (Function_Id);
      end if;
   end Is_Build_In_Place_Function_Call;

   -----------------------
   -- Freeze_Subprogram --
   -----------------------

   procedure Freeze_Subprogram (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

      procedure Register_Predefined_DT_Entry (Prim : Entity_Id);
      --  (Ada 2005): Register a predefined primitive in all the secondary
      --  dispatch tables of its primitive type.

      ----------------------------------
      -- Register_Predefined_DT_Entry --
      ----------------------------------

      procedure Register_Predefined_DT_Entry (Prim : Entity_Id) is
         Iface_DT_Ptr : Elmt_Id;
         Tagged_Typ   : Entity_Id;
         Thunk_Id     : Entity_Id;
         Thunk_Code   : Node_Id;

      begin
         Tagged_Typ := Find_Dispatching_Type (Prim);

         if No (Access_Disp_Table (Tagged_Typ))
           or else not Has_Interfaces (Tagged_Typ)
           or else not RTE_Available (RE_Interface_Tag)
           or else Restriction_Active (No_Dispatching_Calls)
         then
            return;
         end if;

         --  Skip the first two access-to-dispatch-table pointers since they
         --  leads to the primary dispatch table (predefined DT and user
         --  defined DT). We are only concerned with the secondary dispatch
         --  table pointers. Note that the access-to- dispatch-table pointer
         --  corresponds to the first implemented interface retrieved below.

         Iface_DT_Ptr :=
           Next_Elmt (Next_Elmt (First_Elmt (Access_Disp_Table (Tagged_Typ))));

         while Present (Iface_DT_Ptr)
           and then Ekind (Node (Iface_DT_Ptr)) = E_Constant
         loop
            pragma Assert (Has_Thunks (Node (Iface_DT_Ptr)));
            Expand_Interface_Thunk (Prim, Thunk_Id, Thunk_Code);

            if Present (Thunk_Code) then
               Insert_Actions_After (N, New_List (
                 Thunk_Code,

                 Build_Set_Predefined_Prim_Op_Address (Loc,
                   Tag_Node     =>
                     New_Reference_To (Node (Next_Elmt (Iface_DT_Ptr)), Loc),
                   Position     => DT_Position (Prim),
                   Address_Node =>
                     Unchecked_Convert_To (RTE (RE_Prim_Ptr),
                       Make_Attribute_Reference (Loc,
                         Prefix         => New_Reference_To (Thunk_Id, Loc),
                         Attribute_Name => Name_Unrestricted_Access))),

                 Build_Set_Predefined_Prim_Op_Address (Loc,
                   Tag_Node     =>
                     New_Reference_To
                      (Node (Next_Elmt (Next_Elmt (Next_Elmt (Iface_DT_Ptr)))),
                       Loc),
                   Position     => DT_Position (Prim),
                   Address_Node =>
                     Unchecked_Convert_To (RTE (RE_Prim_Ptr),
                       Make_Attribute_Reference (Loc,
                         Prefix         => New_Reference_To (Prim, Loc),
                         Attribute_Name => Name_Unrestricted_Access)))));
            end if;

            --  Skip the tag of the predefined primitives dispatch table

            Next_Elmt (Iface_DT_Ptr);
            pragma Assert (Has_Thunks (Node (Iface_DT_Ptr)));

            --  Skip tag of the no-thunks dispatch table

            Next_Elmt (Iface_DT_Ptr);
            pragma Assert (not Has_Thunks (Node (Iface_DT_Ptr)));

            --  Skip tag of predefined primitives no-thunks dispatch table

            Next_Elmt (Iface_DT_Ptr);
            pragma Assert (not Has_Thunks (Node (Iface_DT_Ptr)));

            Next_Elmt (Iface_DT_Ptr);
         end loop;
      end Register_Predefined_DT_Entry;

      --  Local variables

      Subp : constant Entity_Id  := Entity (N);

   --  Start of processing for Freeze_Subprogram

   begin
      --  We suppress the initialization of the dispatch table entry when
      --  VM_Target because the dispatching mechanism is handled internally
      --  by the VM.

      if Is_Dispatching_Operation (Subp)
        and then not Is_Abstract_Subprogram (Subp)
        and then Present (DTC_Entity (Subp))
        and then Present (Scope (DTC_Entity (Subp)))
        and then Tagged_Type_Expansion
        and then not Restriction_Active (No_Dispatching_Calls)
        and then RTE_Available (RE_Tag)
      then
         declare
            Typ : constant Entity_Id := Scope (DTC_Entity (Subp));

         begin
            --  Handle private overridden primitives

            if not Is_CPP_Class (Typ) then
               Check_Overriding_Operation (Subp);
            end if;

            --  We assume that imported CPP primitives correspond with objects
            --  whose constructor is in the CPP side; therefore we don't need
            --  to generate code to register them in the dispatch table.

            if Is_CPP_Class (Typ) then
               null;

            --  Handle CPP primitives found in derivations of CPP_Class types.
            --  These primitives must have been inherited from some parent, and
            --  there is no need to register them in the dispatch table because
            --  Build_Inherit_Prims takes care of initializing these slots.

            elsif Is_Imported (Subp)
               and then (Convention (Subp) = Convention_CPP
                           or else Convention (Subp) = Convention_C)
            then
               null;

            --  Generate code to register the primitive in non statically
            --  allocated dispatch tables

            elsif not Building_Static_DT (Scope (DTC_Entity (Subp))) then

               --  When a primitive is frozen, enter its name in its dispatch
               --  table slot.

               if not Is_Interface (Typ)
                 or else Present (Interface_Alias (Subp))
               then
                  if Is_Predefined_Dispatching_Operation (Subp) then
                     Register_Predefined_DT_Entry (Subp);
                  end if;

                  Insert_Actions_After (N,
                    Register_Primitive (Loc, Prim => Subp));
               end if;
            end if;
         end;
      end if;

      --  Mark functions that return by reference. Note that it cannot be part
      --  of the normal semantic analysis of the spec since the underlying
      --  returned type may not be known yet (for private types).

      declare
         Typ  : constant Entity_Id := Etype (Subp);
         Utyp : constant Entity_Id := Underlying_Type (Typ);
      begin
         if Is_Limited_View (Typ) then
            Set_Returns_By_Ref (Subp);
         elsif Present (Utyp) and then CW_Or_Has_Controlled_Part (Utyp) then
            Set_Returns_By_Ref (Subp);
         end if;
      end;

      --  Wnen freezing a null procedure, analyze its delayed aspects now
      --  because we may not have reached the end of the declarative list when
      --  delayed aspects are normally analyzed. This ensures that dispatching
      --  calls are properly rewritten when the generated _Postcondition
      --  procedure is analyzed in the null procedure body.

      if Nkind (Parent (Subp)) = N_Procedure_Specification
        and then Null_Present (Parent (Subp))
      then
         Analyze_Subprogram_Contract (Subp);
      end if;
   end Freeze_Subprogram;

   -----------------------
   -- Is_Null_Procedure --
   -----------------------

   function Is_Null_Procedure (Subp : Entity_Id) return Boolean is
      Decl : constant Node_Id := Unit_Declaration_Node (Subp);

   begin
      if Ekind (Subp) /= E_Procedure then
         return False;

      --  Check if this is a declared null procedure

      elsif Nkind (Decl) = N_Subprogram_Declaration then
         if not Null_Present (Specification (Decl)) then
            return False;

         elsif No (Body_To_Inline (Decl)) then
            return False;

         --  Check if the body contains only a null statement, followed by
         --  the return statement added during expansion.

         else
            declare
               Orig_Bod : constant Node_Id := Body_To_Inline (Decl);

               Stat  : Node_Id;
               Stat2 : Node_Id;

            begin
               if Nkind (Orig_Bod) /= N_Subprogram_Body then
                  return False;
               else
                  --  We must skip SCIL nodes because they are currently
                  --  implemented as special N_Null_Statement nodes.

                  Stat :=
                     First_Non_SCIL_Node
                       (Statements (Handled_Statement_Sequence (Orig_Bod)));
                  Stat2 := Next_Non_SCIL_Node (Stat);

                  return
                     Is_Empty_List (Declarations (Orig_Bod))
                       and then Nkind (Stat) = N_Null_Statement
                       and then
                        (No (Stat2)
                          or else
                            (Nkind (Stat2) = N_Simple_Return_Statement
                              and then No (Next (Stat2))));
               end if;
            end;
         end if;

      else
         return False;
      end if;
   end Is_Null_Procedure;

   -------------------------------------------
   -- Make_Build_In_Place_Call_In_Allocator --
   -------------------------------------------

   procedure Make_Build_In_Place_Call_In_Allocator
     (Allocator     : Node_Id;
      Function_Call : Node_Id)
   is
      Acc_Type          : constant Entity_Id := Etype (Allocator);
      Loc               : Source_Ptr;
      Func_Call         : Node_Id := Function_Call;
      Function_Id       : Entity_Id;
      Result_Subt       : Entity_Id;
      New_Allocator     : Node_Id;
      Return_Obj_Access : Entity_Id;

   begin
      --  Step past qualification or unchecked conversion (the latter can occur
      --  in cases of calls to 'Input).

      if Nkind_In (Func_Call,
                   N_Qualified_Expression,
                   N_Unchecked_Type_Conversion)
      then
         Func_Call := Expression (Func_Call);
      end if;

      --  If the call has already been processed to add build-in-place actuals
      --  then return. This should not normally occur in an allocator context,
      --  but we add the protection as a defensive measure.

      if Is_Expanded_Build_In_Place_Call (Func_Call) then
         return;
      end if;

      --  Mark the call as processed as a build-in-place call

      Set_Is_Expanded_Build_In_Place_Call (Func_Call);

      Loc := Sloc (Function_Call);

      if Is_Entity_Name (Name (Func_Call)) then
         Function_Id := Entity (Name (Func_Call));

      elsif Nkind (Name (Func_Call)) = N_Explicit_Dereference then
         Function_Id := Etype (Name (Func_Call));

      else
         raise Program_Error;
      end if;

      Result_Subt := Available_View (Etype (Function_Id));

      --  Check whether return type includes tasks. This may not have been done
      --  previously, if the type was a limited view.

      if Has_Task (Result_Subt) then
         Build_Activation_Chain_Entity (Allocator);
      end if;

      --  When the result subtype is constrained, the return object must be
      --  allocated on the caller side, and access to it is passed to the
      --  function.

      --  Here and in related routines, we must examine the full view of the
      --  type, because the view at the point of call may differ from that
      --  that in the function body, and the expansion mechanism depends on
      --  the characteristics of the full view.

      if Is_Constrained (Underlying_Type (Result_Subt)) then

         --  Replace the initialized allocator of form "new T'(Func (...))"
         --  with an uninitialized allocator of form "new T", where T is the
         --  result subtype of the called function. The call to the function
         --  is handled separately further below.

         New_Allocator :=
           Make_Allocator (Loc,
             Expression => New_Reference_To (Result_Subt, Loc));
         Set_No_Initialization (New_Allocator);

         --  Copy attributes to new allocator. Note that the new allocator
         --  logically comes from source if the original one did, so copy the
         --  relevant flag. This ensures proper treatment of the restriction
         --  No_Implicit_Heap_Allocations in this case.

         Set_Storage_Pool      (New_Allocator, Storage_Pool      (Allocator));
         Set_Procedure_To_Call (New_Allocator, Procedure_To_Call (Allocator));
         Set_Comes_From_Source (New_Allocator, Comes_From_Source (Allocator));

         Rewrite (Allocator, New_Allocator);

         --  Create a new access object and initialize it to the result of the
         --  new uninitialized allocator. Note: we do not use Allocator as the
         --  Related_Node of Return_Obj_Access in call to Make_Temporary below
         --  as this would create a sort of infinite "recursion".

         Return_Obj_Access := Make_Temporary (Loc, 'R');
         Set_Etype (Return_Obj_Access, Acc_Type);

         Insert_Action (Allocator,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Return_Obj_Access,
             Object_Definition   => New_Reference_To (Acc_Type, Loc),
             Expression          => Relocate_Node (Allocator)));

         --  When the function has a controlling result, an allocation-form
         --  parameter must be passed indicating that the caller is allocating
         --  the result object. This is needed because such a function can be
         --  called as a dispatching operation and must be treated similarly
         --  to functions with unconstrained result subtypes.

         Add_Unconstrained_Actuals_To_Build_In_Place_Call
           (Func_Call, Function_Id, Alloc_Form => Caller_Allocation);

         Add_Finalization_Master_Actual_To_Build_In_Place_Call
           (Func_Call, Function_Id, Acc_Type);

         Add_Task_Actuals_To_Build_In_Place_Call
           (Func_Call, Function_Id, Master_Actual => Master_Id (Acc_Type));

         --  Add an implicit actual to the function call that provides access
         --  to the allocated object. An unchecked conversion to the (specific)
         --  result subtype of the function is inserted to handle cases where
         --  the access type of the allocator has a class-wide designated type.

         Add_Access_Actual_To_Build_In_Place_Call
           (Func_Call,
            Function_Id,
            Make_Unchecked_Type_Conversion (Loc,
              Subtype_Mark => New_Reference_To (Result_Subt, Loc),
              Expression   =>
                Make_Explicit_Dereference (Loc,
                  Prefix => New_Reference_To (Return_Obj_Access, Loc))));

      --  When the result subtype is unconstrained, the function itself must
      --  perform the allocation of the return object, so we pass parameters
      --  indicating that. We don't yet handle the case where the allocation
      --  must be done in a user-defined storage pool, which will require
      --  passing another actual or two to provide allocation/deallocation
      --  operations. ???

      else
         --  Case of a user-defined storage pool. Pass an allocation parameter
         --  indicating that the function should allocate its result in the
         --  pool, and pass the pool. Use 'Unrestricted_Access because the
         --  pool may not be aliased.

         if VM_Target = No_VM
           and then Present (Associated_Storage_Pool (Acc_Type))
         then
            Add_Unconstrained_Actuals_To_Build_In_Place_Call
              (Func_Call, Function_Id, Alloc_Form => User_Storage_Pool,
               Pool_Actual =>
                 Make_Attribute_Reference (Loc,
                   Prefix         =>
                     New_Reference_To
                       (Associated_Storage_Pool (Acc_Type), Loc),
                   Attribute_Name => Name_Unrestricted_Access));

         --  No user-defined pool; pass an allocation parameter indicating that
         --  the function should allocate its result on the heap.

         else
            Add_Unconstrained_Actuals_To_Build_In_Place_Call
              (Func_Call, Function_Id, Alloc_Form => Global_Heap);
         end if;

         Add_Finalization_Master_Actual_To_Build_In_Place_Call
           (Func_Call, Function_Id, Acc_Type);

         Add_Task_Actuals_To_Build_In_Place_Call
           (Func_Call, Function_Id, Master_Actual => Master_Id (Acc_Type));

         --  The caller does not provide the return object in this case, so we
         --  have to pass null for the object access actual.

         Add_Access_Actual_To_Build_In_Place_Call
           (Func_Call, Function_Id, Return_Object => Empty);
      end if;

      --  If the build-in-place function call returns a controlled object,
      --  the finalization master will require a reference to routine
      --  Finalize_Address of the designated type. Setting this attribute
      --  is done in the same manner to expansion of allocators.

      if Needs_Finalization (Result_Subt) then

         --  Controlled types with supressed finalization do not need to
         --  associate the address of their Finalize_Address primitives with
         --  a master since they do not need a master to begin with.

         if Is_Library_Level_Entity (Acc_Type)
           and then Finalize_Storage_Only (Result_Subt)
         then
            null;

         --  Do not generate the call to Set_Finalize_Address in CodePeer mode
         --  because Finalize_Address is never built.

         elsif not CodePeer_Mode then
            Insert_Action (Allocator,
              Make_Set_Finalize_Address_Call (Loc,
                Typ     => Etype (Function_Id),
                Ptr_Typ => Acc_Type));
         end if;
      end if;

      --  Finally, replace the allocator node with a reference to the result
      --  of the function call itself (which will effectively be an access
      --  to the object created by the allocator).

      Rewrite (Allocator, Make_Reference (Loc, Relocate_Node (Function_Call)));

      --  Ada 2005 (AI-251): If the type of the allocator is an interface then
      --  generate an implicit conversion to force displacement of the "this"
      --  pointer.

      if Is_Interface (Designated_Type (Acc_Type)) then
         Rewrite (Allocator, Convert_To (Acc_Type, Relocate_Node (Allocator)));
      end if;

      Analyze_And_Resolve (Allocator, Acc_Type);
   end Make_Build_In_Place_Call_In_Allocator;

   ---------------------------------------------------
   -- Make_Build_In_Place_Call_In_Anonymous_Context --
   ---------------------------------------------------

   procedure Make_Build_In_Place_Call_In_Anonymous_Context
     (Function_Call : Node_Id)
   is
      Loc             : Source_Ptr;
      Func_Call       : Node_Id := Function_Call;
      Function_Id     : Entity_Id;
      Result_Subt     : Entity_Id;
      Return_Obj_Id   : Entity_Id;
      Return_Obj_Decl : Entity_Id;

   begin
      --  Step past qualification or unchecked conversion (the latter can occur
      --  in cases of calls to 'Input).

      if Nkind_In (Func_Call, N_Qualified_Expression,
                              N_Unchecked_Type_Conversion)
      then
         Func_Call := Expression (Func_Call);
      end if;

      --  If the call has already been processed to add build-in-place actuals
      --  then return. One place this can occur is for calls to build-in-place
      --  functions that occur within a call to a protected operation, where
      --  due to rewriting and expansion of the protected call there can be
      --  more than one call to Expand_Actuals for the same set of actuals.

      if Is_Expanded_Build_In_Place_Call (Func_Call) then
         return;
      end if;

      --  Mark the call as processed as a build-in-place call

      Set_Is_Expanded_Build_In_Place_Call (Func_Call);

      Loc := Sloc (Function_Call);

      if Is_Entity_Name (Name (Func_Call)) then
         Function_Id := Entity (Name (Func_Call));

      elsif Nkind (Name (Func_Call)) = N_Explicit_Dereference then
         Function_Id := Etype (Name (Func_Call));

      else
         raise Program_Error;
      end if;

      Result_Subt := Etype (Function_Id);

      --  If the build-in-place function returns a controlled object, then the
      --  object needs to be finalized immediately after the context. Since
      --  this case produces a transient scope, the servicing finalizer needs
      --  to name the returned object. Create a temporary which is initialized
      --  with the function call:
      --
      --    Temp_Id : Func_Type := BIP_Func_Call;
      --
      --  The initialization expression of the temporary will be rewritten by
      --  the expander using the appropriate mechanism in Make_Build_In_Place_
      --  Call_In_Object_Declaration.

      if Needs_Finalization (Result_Subt) then
         declare
            Temp_Id   : constant Entity_Id := Make_Temporary (Loc, 'R');
            Temp_Decl : Node_Id;

         begin
            --  Reset the guard on the function call since the following does
            --  not perform actual call expansion.

            Set_Is_Expanded_Build_In_Place_Call (Func_Call, False);

            Temp_Decl :=
              Make_Object_Declaration (Loc,
                Defining_Identifier => Temp_Id,
                Object_Definition =>
                  New_Reference_To (Result_Subt, Loc),
                Expression =>
                  New_Copy_Tree (Function_Call));

            Insert_Action (Function_Call, Temp_Decl);

            Rewrite (Function_Call, New_Reference_To (Temp_Id, Loc));
            Analyze (Function_Call);
         end;

      --  When the result subtype is constrained, an object of the subtype is
      --  declared and an access value designating it is passed as an actual.

      elsif Is_Constrained (Underlying_Type (Result_Subt)) then

         --  Create a temporary object to hold the function result

         Return_Obj_Id := Make_Temporary (Loc, 'R');
         Set_Etype (Return_Obj_Id, Result_Subt);

         Return_Obj_Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Return_Obj_Id,
             Aliased_Present     => True,
             Object_Definition   => New_Reference_To (Result_Subt, Loc));

         Set_No_Initialization (Return_Obj_Decl);

         Insert_Action (Func_Call, Return_Obj_Decl);

         --  When the function has a controlling result, an allocation-form
         --  parameter must be passed indicating that the caller is allocating
         --  the result object. This is needed because such a function can be
         --  called as a dispatching operation and must be treated similarly
         --  to functions with unconstrained result subtypes.

         Add_Unconstrained_Actuals_To_Build_In_Place_Call
           (Func_Call, Function_Id, Alloc_Form => Caller_Allocation);

         Add_Finalization_Master_Actual_To_Build_In_Place_Call
           (Func_Call, Function_Id);

         Add_Task_Actuals_To_Build_In_Place_Call
           (Func_Call, Function_Id, Make_Identifier (Loc, Name_uMaster));

         --  Add an implicit actual to the function call that provides access
         --  to the caller's return object.

         Add_Access_Actual_To_Build_In_Place_Call
           (Func_Call, Function_Id, New_Reference_To (Return_Obj_Id, Loc));

      --  When the result subtype is unconstrained, the function must allocate
      --  the return object in the secondary stack, so appropriate implicit
      --  parameters are added to the call to indicate that. A transient
      --  scope is established to ensure eventual cleanup of the result.

      else
         --  Pass an allocation parameter indicating that the function should
         --  allocate its result on the secondary stack.

         Add_Unconstrained_Actuals_To_Build_In_Place_Call
           (Func_Call, Function_Id, Alloc_Form => Secondary_Stack);

         Add_Finalization_Master_Actual_To_Build_In_Place_Call
           (Func_Call, Function_Id);

         Add_Task_Actuals_To_Build_In_Place_Call
           (Func_Call, Function_Id, Make_Identifier (Loc, Name_uMaster));

         --  Pass a null value to the function since no return object is
         --  available on the caller side.

         Add_Access_Actual_To_Build_In_Place_Call
           (Func_Call, Function_Id, Empty);
      end if;
   end Make_Build_In_Place_Call_In_Anonymous_Context;

   --------------------------------------------
   -- Make_Build_In_Place_Call_In_Assignment --
   --------------------------------------------

   procedure Make_Build_In_Place_Call_In_Assignment
     (Assign        : Node_Id;
      Function_Call : Node_Id)
   is
      Lhs          : constant Node_Id := Name (Assign);
      Func_Call    : Node_Id := Function_Call;
      Func_Id      : Entity_Id;
      Loc          : Source_Ptr;
      Obj_Decl     : Node_Id;
      Obj_Id       : Entity_Id;
      Ptr_Typ      : Entity_Id;
      Ptr_Typ_Decl : Node_Id;
      New_Expr     : Node_Id;
      Result_Subt  : Entity_Id;
      Target       : Node_Id;

   begin
      --  Step past qualification or unchecked conversion (the latter can occur
      --  in cases of calls to 'Input).

      if Nkind_In (Func_Call, N_Qualified_Expression,
                              N_Unchecked_Type_Conversion)
      then
         Func_Call := Expression (Func_Call);
      end if;

      --  If the call has already been processed to add build-in-place actuals
      --  then return. This should not normally occur in an assignment context,
      --  but we add the protection as a defensive measure.

      if Is_Expanded_Build_In_Place_Call (Func_Call) then
         return;
      end if;

      --  Mark the call as processed as a build-in-place call

      Set_Is_Expanded_Build_In_Place_Call (Func_Call);

      Loc := Sloc (Function_Call);

      if Is_Entity_Name (Name (Func_Call)) then
         Func_Id := Entity (Name (Func_Call));

      elsif Nkind (Name (Func_Call)) = N_Explicit_Dereference then
         Func_Id := Etype (Name (Func_Call));

      else
         raise Program_Error;
      end if;

      Result_Subt := Etype (Func_Id);

      --  When the result subtype is unconstrained, an additional actual must
      --  be passed to indicate that the caller is providing the return object.
      --  This parameter must also be passed when the called function has a
      --  controlling result, because dispatching calls to the function needs
      --  to be treated effectively the same as calls to class-wide functions.

      Add_Unconstrained_Actuals_To_Build_In_Place_Call
        (Func_Call, Func_Id, Alloc_Form => Caller_Allocation);

      Add_Finalization_Master_Actual_To_Build_In_Place_Call
        (Func_Call, Func_Id);

      Add_Task_Actuals_To_Build_In_Place_Call
        (Func_Call, Func_Id, Make_Identifier (Loc, Name_uMaster));

      --  Add an implicit actual to the function call that provides access to
      --  the caller's return object.

      Add_Access_Actual_To_Build_In_Place_Call
        (Func_Call,
         Func_Id,
         Make_Unchecked_Type_Conversion (Loc,
           Subtype_Mark => New_Reference_To (Result_Subt, Loc),
           Expression   => Relocate_Node (Lhs)));

      --  Create an access type designating the function's result subtype

      Ptr_Typ := Make_Temporary (Loc, 'A');

      Ptr_Typ_Decl :=
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => Ptr_Typ,
          Type_Definition     =>
            Make_Access_To_Object_Definition (Loc,
              All_Present        => True,
              Subtype_Indication =>
                New_Reference_To (Result_Subt, Loc)));
      Insert_After_And_Analyze (Assign, Ptr_Typ_Decl);

      --  Finally, create an access object initialized to a reference to the
      --  function call. We know this access value is non-null, so mark the
      --  entity accordingly to suppress junk access checks.

      New_Expr := Make_Reference (Loc, Relocate_Node (Func_Call));

      Obj_Id := Make_Temporary (Loc, 'R', New_Expr);
      Set_Etype (Obj_Id, Ptr_Typ);
      Set_Is_Known_Non_Null (Obj_Id);

      Obj_Decl :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => Obj_Id,
          Object_Definition   => New_Reference_To (Ptr_Typ, Loc),
          Expression          => New_Expr);
      Insert_After_And_Analyze (Ptr_Typ_Decl, Obj_Decl);

      Rewrite (Assign, Make_Null_Statement (Loc));

      --  Retrieve the target of the assignment

      if Nkind (Lhs) = N_Selected_Component then
         Target := Selector_Name (Lhs);
      elsif Nkind (Lhs) = N_Type_Conversion then
         Target := Expression (Lhs);
      else
         Target := Lhs;
      end if;

      --  If we are assigning to a return object or this is an expression of
      --  an extension aggregate, the target should either be an identifier
      --  or a simple expression. All other cases imply a different scenario.

      if Nkind (Target) in N_Has_Entity then
         Target := Entity (Target);
      else
         return;
      end if;
   end Make_Build_In_Place_Call_In_Assignment;

   ----------------------------------------------------
   -- Make_Build_In_Place_Call_In_Object_Declaration --
   ----------------------------------------------------

   procedure Make_Build_In_Place_Call_In_Object_Declaration
     (Object_Decl   : Node_Id;
      Function_Call : Node_Id)
   is
      Loc             : Source_Ptr;
      Obj_Def_Id      : constant Entity_Id :=
                          Defining_Identifier (Object_Decl);
      Enclosing_Func  : constant Entity_Id :=
                          Enclosing_Subprogram (Obj_Def_Id);
      Call_Deref      : Node_Id;
      Caller_Object   : Node_Id;
      Def_Id          : Entity_Id;
      Fmaster_Actual  : Node_Id := Empty;
      Func_Call       : Node_Id := Function_Call;
      Function_Id     : Entity_Id;
      Pool_Actual     : Node_Id;
      Ptr_Typ_Decl    : Node_Id;
      Pass_Caller_Acc : Boolean := False;
      New_Expr        : Node_Id;
      Ref_Type        : Entity_Id;
      Result_Subt     : Entity_Id;

   begin
      --  Step past qualification or unchecked conversion (the latter can occur
      --  in cases of calls to 'Input).

      if Nkind_In (Func_Call, N_Qualified_Expression,
                              N_Unchecked_Type_Conversion)
      then
         Func_Call := Expression (Func_Call);
      end if;

      --  If the call has already been processed to add build-in-place actuals
      --  then return. This should not normally occur in an object declaration,
      --  but we add the protection as a defensive measure.

      if Is_Expanded_Build_In_Place_Call (Func_Call) then
         return;
      end if;

      --  Mark the call as processed as a build-in-place call

      Set_Is_Expanded_Build_In_Place_Call (Func_Call);

      Loc := Sloc (Function_Call);

      if Is_Entity_Name (Name (Func_Call)) then
         Function_Id := Entity (Name (Func_Call));

      elsif Nkind (Name (Func_Call)) = N_Explicit_Dereference then
         Function_Id := Etype (Name (Func_Call));

      else
         raise Program_Error;
      end if;

      Result_Subt := Etype (Function_Id);

      --  If the the object is a return object of an enclosing build-in-place
      --  function, then the implicit build-in-place parameters of the
      --  enclosing function are simply passed along to the called function.
      --  (Unfortunately, this won't cover the case of extension aggregates
      --  where the ancestor part is a build-in-place unconstrained function
      --  call that should be passed along the caller's parameters. Currently
      --  those get mishandled by reassigning the result of the call to the
      --  aggregate return object, when the call result should really be
      --  directly built in place in the aggregate and not in a temporary. ???)

      if Is_Return_Object (Defining_Identifier (Object_Decl)) then
         Pass_Caller_Acc := True;

         --  When the enclosing function has a BIP_Alloc_Form formal then we
         --  pass it along to the callee (such as when the enclosing function
         --  has an unconstrained or tagged result type).

         if Needs_BIP_Alloc_Form (Enclosing_Func) then
            if VM_Target = No_VM and then
              RTE_Available (RE_Root_Storage_Pool_Ptr)
            then
               Pool_Actual :=
                 New_Reference_To (Build_In_Place_Formal
                   (Enclosing_Func, BIP_Storage_Pool), Loc);

            --  The build-in-place pool formal is not built on .NET/JVM

            else
               Pool_Actual := Empty;
            end if;

            Add_Unconstrained_Actuals_To_Build_In_Place_Call
              (Func_Call,
               Function_Id,
               Alloc_Form_Exp =>
                 New_Reference_To
                   (Build_In_Place_Formal (Enclosing_Func, BIP_Alloc_Form),
                    Loc),
               Pool_Actual => Pool_Actual);

         --  Otherwise, if enclosing function has a constrained result subtype,
         --  then caller allocation will be used.

         else
            Add_Unconstrained_Actuals_To_Build_In_Place_Call
              (Func_Call, Function_Id, Alloc_Form => Caller_Allocation);
         end if;

         if Needs_BIP_Finalization_Master (Enclosing_Func) then
            Fmaster_Actual :=
              New_Reference_To
                (Build_In_Place_Formal
                   (Enclosing_Func, BIP_Finalization_Master), Loc);
         end if;

         --  Retrieve the BIPacc formal from the enclosing function and convert
         --  it to the access type of the callee's BIP_Object_Access formal.

         Caller_Object :=
            Make_Unchecked_Type_Conversion (Loc,
              Subtype_Mark =>
                New_Reference_To
                  (Etype
                     (Build_In_Place_Formal (Function_Id, BIP_Object_Access)),
                   Loc),
              Expression   =>
                New_Reference_To
                  (Build_In_Place_Formal (Enclosing_Func, BIP_Object_Access),
                   Loc));

      --  In the constrained case, add an implicit actual to the function call
      --  that provides access to the declared object. An unchecked conversion
      --  to the (specific) result type of the function is inserted to handle
      --  the case where the object is declared with a class-wide type.

      elsif Is_Constrained (Underlying_Type (Result_Subt)) then
         Caller_Object :=
            Make_Unchecked_Type_Conversion (Loc,
              Subtype_Mark => New_Reference_To (Result_Subt, Loc),
              Expression   => New_Reference_To (Obj_Def_Id, Loc));

         --  When the function has a controlling result, an allocation-form
         --  parameter must be passed indicating that the caller is allocating
         --  the result object. This is needed because such a function can be
         --  called as a dispatching operation and must be treated similarly
         --  to functions with unconstrained result subtypes.

         Add_Unconstrained_Actuals_To_Build_In_Place_Call
           (Func_Call, Function_Id, Alloc_Form => Caller_Allocation);

      --  In other unconstrained cases, pass an indication to do the allocation
      --  on the secondary stack and set Caller_Object to Empty so that a null
      --  value will be passed for the caller's object address. A transient
      --  scope is established to ensure eventual cleanup of the result.

      else
         Add_Unconstrained_Actuals_To_Build_In_Place_Call
           (Func_Call, Function_Id, Alloc_Form => Secondary_Stack);
         Caller_Object := Empty;

         Establish_Transient_Scope (Object_Decl, Sec_Stack => True);
      end if;

      --  Pass along any finalization master actual, which is needed in the
      --  case where the called function initializes a return object of an
      --  enclosing build-in-place function.

      Add_Finalization_Master_Actual_To_Build_In_Place_Call
        (Func_Call  => Func_Call,
         Func_Id    => Function_Id,
         Master_Exp => Fmaster_Actual);

      if Nkind (Parent (Object_Decl)) = N_Extended_Return_Statement
        and then Has_Task (Result_Subt)
      then
         --  Here we're passing along the master that was passed in to this
         --  function.

         Add_Task_Actuals_To_Build_In_Place_Call
           (Func_Call, Function_Id,
            Master_Actual =>
              New_Reference_To (Build_In_Place_Formal
                (Enclosing_Func, BIP_Task_Master), Loc));

      else
         Add_Task_Actuals_To_Build_In_Place_Call
           (Func_Call, Function_Id, Make_Identifier (Loc, Name_uMaster));
      end if;

      Add_Access_Actual_To_Build_In_Place_Call
        (Func_Call, Function_Id, Caller_Object, Is_Access => Pass_Caller_Acc);

      --  Create an access type designating the function's result subtype. We
      --  use the type of the original expression because it may be a call to
      --  an inherited operation, which the expansion has replaced with the
      --  parent operation that yields the parent type.

      Ref_Type := Make_Temporary (Loc, 'A');

      Ptr_Typ_Decl :=
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => Ref_Type,
          Type_Definition     =>
            Make_Access_To_Object_Definition (Loc,
              All_Present        => True,
              Subtype_Indication =>
                New_Reference_To (Etype (Function_Call), Loc)));

      --  The access type and its accompanying object must be inserted after
      --  the object declaration in the constrained case, so that the function
      --  call can be passed access to the object. In the unconstrained case,
      --  or if the object declaration is for a return object, the access type
      --  and object must be inserted before the object, since the object
      --  declaration is rewritten to be a renaming of a dereference of the
      --  access object.

      if Is_Constrained (Underlying_Type (Result_Subt))
        and then not Is_Return_Object (Defining_Identifier (Object_Decl))
      then
         Insert_After_And_Analyze (Object_Decl, Ptr_Typ_Decl);
      else
         Insert_Action (Object_Decl, Ptr_Typ_Decl);
      end if;

      --  Finally, create an access object initialized to a reference to the
      --  function call. We know this access value cannot be null, so mark the
      --  entity accordingly to suppress the access check.

      New_Expr := Make_Reference (Loc, Relocate_Node (Func_Call));

      Def_Id := Make_Temporary (Loc, 'R', New_Expr);
      Set_Etype (Def_Id, Ref_Type);
      Set_Is_Known_Non_Null (Def_Id);

      Insert_After_And_Analyze (Ptr_Typ_Decl,
        Make_Object_Declaration (Loc,
          Defining_Identifier => Def_Id,
          Object_Definition   => New_Reference_To (Ref_Type, Loc),
          Expression          => New_Expr));

      --  If the result subtype of the called function is constrained and
      --  is not itself the return expression of an enclosing BIP function,
      --  then mark the object as having no initialization.

      if Is_Constrained (Underlying_Type (Result_Subt))
        and then not Is_Return_Object (Defining_Identifier (Object_Decl))
      then
         Set_Expression (Object_Decl, Empty);
         Set_No_Initialization (Object_Decl);

      --  In case of an unconstrained result subtype, or if the call is the
      --  return expression of an enclosing BIP function, rewrite the object
      --  declaration as an object renaming where the renamed object is a
      --  dereference of <function_Call>'reference:
      --
      --      Obj : Subt renames <function_call>'Ref.all;

      else
         Call_Deref :=
           Make_Explicit_Dereference (Loc,
             Prefix => New_Reference_To (Def_Id, Loc));

         Loc := Sloc (Object_Decl);
         Rewrite (Object_Decl,
           Make_Object_Renaming_Declaration (Loc,
             Defining_Identifier => Make_Temporary (Loc, 'D'),
             Access_Definition   => Empty,
             Subtype_Mark        => New_Occurrence_Of (Result_Subt, Loc),
             Name                => Call_Deref));

         Set_Renamed_Object (Defining_Identifier (Object_Decl), Call_Deref);

         Analyze (Object_Decl);

         --  Replace the internal identifier of the renaming declaration's
         --  entity with identifier of the original object entity. We also have
         --  to exchange the entities containing their defining identifiers to
         --  ensure the correct replacement of the object declaration by the
         --  object renaming declaration to avoid homograph conflicts (since
         --  the object declaration's defining identifier was already entered
         --  in current scope). The Next_Entity links of the two entities also
         --  have to be swapped since the entities are part of the return
         --  scope's entity list and the list structure would otherwise be
         --  corrupted. Finally, the homonym chain must be preserved as well.

         declare
            Renaming_Def_Id  : constant Entity_Id :=
                                 Defining_Identifier (Object_Decl);
            Next_Entity_Temp : constant Entity_Id :=
                                 Next_Entity (Renaming_Def_Id);
         begin
            Set_Chars (Renaming_Def_Id, Chars (Obj_Def_Id));

            --  Swap next entity links in preparation for exchanging entities

            Set_Next_Entity (Renaming_Def_Id, Next_Entity (Obj_Def_Id));
            Set_Next_Entity (Obj_Def_Id, Next_Entity_Temp);
            Set_Homonym     (Renaming_Def_Id, Homonym (Obj_Def_Id));

            Exchange_Entities (Renaming_Def_Id, Obj_Def_Id);

            --  Preserve source indication of original declaration, so that
            --  xref information is properly generated for the right entity.

            Preserve_Comes_From_Source
              (Object_Decl, Original_Node (Object_Decl));

            Preserve_Comes_From_Source
              (Obj_Def_Id, Original_Node (Object_Decl));

            Set_Comes_From_Source (Renaming_Def_Id, False);
         end;
      end if;

      --  If the object entity has a class-wide Etype, then we need to change
      --  it to the result subtype of the function call, because otherwise the
      --  object will be class-wide without an explicit initialization and
      --  won't be allocated properly by the back end. It seems unclean to make
      --  such a revision to the type at this point, and we should try to
      --  improve this treatment when build-in-place functions with class-wide
      --  results are implemented. ???

      if Is_Class_Wide_Type (Etype (Defining_Identifier (Object_Decl))) then
         Set_Etype (Defining_Identifier (Object_Decl), Result_Subt);
      end if;
   end Make_Build_In_Place_Call_In_Object_Declaration;

   --------------------------------------------
   -- Make_CPP_Constructor_Call_In_Allocator --
   --------------------------------------------

   procedure Make_CPP_Constructor_Call_In_Allocator
     (Allocator     : Node_Id;
      Function_Call : Node_Id)
   is
      Loc         : constant Source_Ptr := Sloc (Function_Call);
      Acc_Type    : constant Entity_Id := Etype (Allocator);
      Function_Id : constant Entity_Id := Entity (Name (Function_Call));
      Result_Subt : constant Entity_Id := Available_View (Etype (Function_Id));

      New_Allocator     : Node_Id;
      Return_Obj_Access : Entity_Id;
      Tmp_Obj           : Node_Id;

   begin
      pragma Assert (Nkind (Allocator) = N_Allocator
                       and then Nkind (Function_Call) = N_Function_Call);
      pragma Assert (Convention (Function_Id) = Convention_CPP
                       and then Is_Constructor (Function_Id));
      pragma Assert (Is_Constrained (Underlying_Type (Result_Subt)));

      --  Replace the initialized allocator of form "new T'(Func (...))" with
      --  an uninitialized allocator of form "new T", where T is the result
      --  subtype of the called function. The call to the function is handled
      --  separately further below.

      New_Allocator :=
        Make_Allocator (Loc,
          Expression => New_Reference_To (Result_Subt, Loc));
      Set_No_Initialization (New_Allocator);

      --  Copy attributes to new allocator. Note that the new allocator
      --  logically comes from source if the original one did, so copy the
      --  relevant flag. This ensures proper treatment of the restriction
      --  No_Implicit_Heap_Allocations in this case.

      Set_Storage_Pool      (New_Allocator, Storage_Pool      (Allocator));
      Set_Procedure_To_Call (New_Allocator, Procedure_To_Call (Allocator));
      Set_Comes_From_Source (New_Allocator, Comes_From_Source (Allocator));

      Rewrite (Allocator, New_Allocator);

      --  Create a new access object and initialize it to the result of the
      --  new uninitialized allocator. Note: we do not use Allocator as the
      --  Related_Node of Return_Obj_Access in call to Make_Temporary below
      --  as this would create a sort of infinite "recursion".

      Return_Obj_Access := Make_Temporary (Loc, 'R');
      Set_Etype (Return_Obj_Access, Acc_Type);

      --  Generate:
      --    Rnnn : constant ptr_T := new (T);
      --    Init (Rnn.all,...);

      Tmp_Obj :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => Return_Obj_Access,
          Constant_Present    => True,
          Object_Definition   => New_Reference_To (Acc_Type, Loc),
          Expression          => Relocate_Node (Allocator));
      Insert_Action (Allocator, Tmp_Obj);

      Insert_List_After_And_Analyze (Tmp_Obj,
        Build_Initialization_Call (Loc,
          Id_Ref =>
            Make_Explicit_Dereference (Loc,
              Prefix => New_Reference_To (Return_Obj_Access, Loc)),
          Typ => Etype (Function_Id),
          Constructor_Ref => Function_Call));

      --  Finally, replace the allocator node with a reference to the result of
      --  the function call itself (which will effectively be an access to the
      --  object created by the allocator).

      Rewrite (Allocator, New_Reference_To (Return_Obj_Access, Loc));

      --  Ada 2005 (AI-251): If the type of the allocator is an interface then
      --  generate an implicit conversion to force displacement of the "this"
      --  pointer.

      if Is_Interface (Designated_Type (Acc_Type)) then
         Rewrite (Allocator, Convert_To (Acc_Type, Relocate_Node (Allocator)));
      end if;

      Analyze_And_Resolve (Allocator, Acc_Type);
   end Make_CPP_Constructor_Call_In_Allocator;

   -----------------------------------
   -- Needs_BIP_Finalization_Master --
   -----------------------------------

   function Needs_BIP_Finalization_Master
     (Func_Id : Entity_Id) return Boolean
   is
      pragma Assert (Is_Build_In_Place_Function (Func_Id));
      Func_Typ : constant Entity_Id := Underlying_Type (Etype (Func_Id));
   begin
      return
        not Restriction_Active (No_Finalization)
          and then Needs_Finalization (Func_Typ);
   end Needs_BIP_Finalization_Master;

   --------------------------
   -- Needs_BIP_Alloc_Form --
   --------------------------

   function Needs_BIP_Alloc_Form (Func_Id : Entity_Id) return Boolean is
      pragma Assert (Is_Build_In_Place_Function (Func_Id));
      Func_Typ : constant Entity_Id := Underlying_Type (Etype (Func_Id));
   begin
      return not Is_Constrained (Func_Typ) or else Is_Tagged_Type (Func_Typ);
   end Needs_BIP_Alloc_Form;

   --------------------------------------
   -- Needs_Result_Accessibility_Level --
   --------------------------------------

   function Needs_Result_Accessibility_Level
     (Func_Id : Entity_Id) return Boolean
   is
      Func_Typ : constant Entity_Id := Underlying_Type (Etype (Func_Id));

      function Has_Unconstrained_Access_Discriminant_Component
        (Comp_Typ : Entity_Id) return Boolean;
      --  Returns True if any component of the type has an unconstrained access
      --  discriminant.

      -----------------------------------------------------
      -- Has_Unconstrained_Access_Discriminant_Component --
      -----------------------------------------------------

      function Has_Unconstrained_Access_Discriminant_Component
        (Comp_Typ :  Entity_Id) return Boolean
      is
      begin
         if not Is_Limited_Type (Comp_Typ) then
            return False;

            --  Only limited types can have access discriminants with
            --  defaults.

         elsif Has_Unconstrained_Access_Discriminants (Comp_Typ) then
            return True;

         elsif Is_Array_Type (Comp_Typ) then
            return Has_Unconstrained_Access_Discriminant_Component
                     (Underlying_Type (Component_Type (Comp_Typ)));

         elsif Is_Record_Type (Comp_Typ) then
            declare
               Comp : Entity_Id;

            begin
               Comp := First_Component (Comp_Typ);
               while Present (Comp) loop
                  if Has_Unconstrained_Access_Discriminant_Component
                       (Underlying_Type (Etype (Comp)))
                  then
                     return True;
                  end if;

                  Next_Component (Comp);
               end loop;
            end;
         end if;

         return False;
      end Has_Unconstrained_Access_Discriminant_Component;

      Feature_Disabled : constant Boolean := True;
      --  Temporary

   --  Start of processing for Needs_Result_Accessibility_Level

   begin
      --  False if completion unavailable (how does this happen???)

      if not Present (Func_Typ) then
         return False;

      elsif Feature_Disabled then
         return False;

      --  False if not a function, also handle enum-lit renames case

      elsif Func_Typ = Standard_Void_Type
        or else Is_Scalar_Type (Func_Typ)
      then
         return False;

      --  Handle a corner case, a cross-dialect subp renaming. For example,
      --  an Ada 2012 renaming of an Ada 2005 subprogram. This can occur when
      --  an Ada 2005 (or earlier) unit references predefined run-time units.

      elsif Present (Alias (Func_Id)) then

         --  Unimplemented: a cross-dialect subp renaming which does not set
         --  the Alias attribute (e.g., a rename of a dereference of an access
         --  to subprogram value). ???

         return Present (Extra_Accessibility_Of_Result (Alias (Func_Id)));

      --  Remaining cases require Ada 2012 mode

      elsif Ada_Version < Ada_2012 then
         return False;

      elsif Ekind (Func_Typ) = E_Anonymous_Access_Type
        or else Is_Tagged_Type (Func_Typ)
      then
         --  In the case of, say, a null tagged record result type, the need
         --  for this extra parameter might not be obvious. This function
         --  returns True for all tagged types for compatibility reasons.
         --  A function with, say, a tagged null controlling result type might
         --  be overridden by a primitive of an extension having an access
         --  discriminant and the overrider and overridden must have compatible
         --  calling conventions (including implicitly declared parameters).
         --  Similarly, values of one access-to-subprogram type might designate
         --  both a primitive subprogram of a given type and a function
         --  which is, for example, not a primitive subprogram of any type.
         --  Again, this requires calling convention compatibility.
         --  It might be possible to solve these issues by introducing
         --  wrappers, but that is not the approach that was chosen.

         return True;

      elsif Has_Unconstrained_Access_Discriminants (Func_Typ) then
         return True;

      elsif Has_Unconstrained_Access_Discriminant_Component (Func_Typ) then
         return True;

      --  False for all other cases

      else
         return False;
      end if;
   end Needs_Result_Accessibility_Level;

   ------------------------
   -- List_Inlining_Info --
   ------------------------

   procedure List_Inlining_Info is
      Elmt  : Elmt_Id;
      Nod   : Node_Id;
      Count : Nat;

   begin
      if not Debug_Flag_Dot_J then
         return;
      end if;

      --  Generate listing of calls inlined by the frontend

      if Present (Inlined_Calls) then
         Count := 0;
         Elmt  := First_Elmt (Inlined_Calls);
         while Present (Elmt) loop
            Nod := Node (Elmt);

            if In_Extended_Main_Code_Unit (Nod) then
               Count := Count + 1;

               if Count = 1 then
                  Write_Str ("Listing of frontend inlined calls");
                  Write_Eol;
               end if;

               Write_Str ("  ");
               Write_Int (Count);
               Write_Str (":");
               Write_Location (Sloc (Nod));
               Write_Str (":");
               Output.Write_Eol;
            end if;

            Next_Elmt (Elmt);
         end loop;
      end if;

      --  Generate listing of calls passed to the backend

      if Present (Backend_Calls) then
         Count := 0;

         Elmt := First_Elmt (Backend_Calls);
         while Present (Elmt) loop
            Nod := Node (Elmt);

            if In_Extended_Main_Code_Unit (Nod) then
               Count := Count + 1;

               if Count = 1 then
                  Write_Str ("Listing of inlined calls passed to the backend");
                  Write_Eol;
               end if;

               Write_Str ("  ");
               Write_Int (Count);
               Write_Str (":");
               Write_Location (Sloc (Nod));
               Output.Write_Eol;
            end if;

            Next_Elmt (Elmt);
         end loop;
      end if;
   end List_Inlining_Info;

end Exp_Ch6;
