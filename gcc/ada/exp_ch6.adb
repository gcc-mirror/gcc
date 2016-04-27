------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 6                               --
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

with Atree;     use Atree;
with Checks;    use Checks;
with Contracts; use Contracts;
with Debug;     use Debug;
with Einfo;     use Einfo;
with Errout;    use Errout;
with Elists;    use Elists;
with Exp_Aggr;  use Exp_Aggr;
with Exp_Atag;  use Exp_Atag;
with Exp_Ch2;   use Exp_Ch2;
with Exp_Ch3;   use Exp_Ch3;
with Exp_Ch7;   use Exp_Ch7;
with Exp_Ch9;   use Exp_Ch9;
with Exp_Dbug;  use Exp_Dbug;
with Exp_Disp;  use Exp_Disp;
with Exp_Dist;  use Exp_Dist;
with Exp_Intr;  use Exp_Intr;
with Exp_Pakd;  use Exp_Pakd;
with Exp_Tss;   use Exp_Tss;
with Exp_Util;  use Exp_Util;
with Freeze;    use Freeze;
with Ghost;     use Ghost;
with Inline;    use Inline;
with Lib;       use Lib;
with Namet;     use Namet;
with Nlists;    use Nlists;
with Nmake;     use Nmake;
with Opt;       use Opt;
with Restrict;  use Restrict;
with Rident;    use Rident;
with Rtsfind;   use Rtsfind;
with Sem;       use Sem;
with Sem_Aux;   use Sem_Aux;
with Sem_Ch6;   use Sem_Ch6;
with Sem_Ch8;   use Sem_Ch8;
with Sem_Ch12;  use Sem_Ch12;
with Sem_Ch13;  use Sem_Ch13;
with Sem_Dim;   use Sem_Dim;
with Sem_Disp;  use Sem_Disp;
with Sem_Dist;  use Sem_Dist;
with Sem_Eval;  use Sem_Eval;
with Sem_Mech;  use Sem_Mech;
with Sem_Res;   use Sem_Res;
with Sem_SCIL;  use Sem_SCIL;
with Sem_Util;  use Sem_Util;
with Sinfo;     use Sinfo;
with Snames;    use Snames;
with Stand;     use Stand;
with Targparm;  use Targparm;
with Tbuild;    use Tbuild;
with Uintp;     use Uintp;
with Validsw;   use Validsw;

package body Exp_Ch6 is

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
      Master_Actual : Node_Id;
      Chain         : Node_Id := Empty);
   --  Ada 2005 (AI-318-02): For a build-in-place call, if the result type
   --  contains tasks, add two actual parameters: the master, and a pointer to
   --  the caller's activation chain. Master_Actual is the actual parameter
   --  expression to pass for the master. In most cases, this is the current
   --  master (_master). The two exceptions are: If the function call is the
   --  initialization expression for an allocator, we pass the master of the
   --  access type. If the function call is the initialization expression for a
   --  return object, we pass along the master passed in by the caller. In most
   --  contexts, the activation chain to pass is the local one, which is
   --  indicated by No (Chain). However, in an allocator, the caller passes in
   --  the activation Chain. Note: Master_Actual can be Empty, but only if
   --  there are no tasks.

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

   procedure Expand_Actuals (N : in out Node_Id; Subp : Entity_Id);
   --  For each actual of an in-out or out parameter which is a numeric
   --  (view) conversion of the form T (A), where A denotes a variable,
   --  we insert the declaration:
   --
   --    Temp : T[ := T (A)];
   --
   --  prior to the call. Then we replace the actual with a reference to Temp,
   --  and append the assignment:
   --
   --    A := TypeA (Temp);
   --
   --  after the call. Here TypeA is the actual type of variable A. For out
   --  parameters, the initial declaration has no expression. If A is not an
   --  entity name, we generate instead:
   --
   --    Var  : TypeA renames A;
   --    Temp : T := Var;       --  omitting expression for out parameter.
   --    ...
   --    Var := TypeA (Temp);
   --
   --  For other in-out parameters, we emit the required constraint checks
   --  before and/or after the call.
   --
   --  For all parameter modes, actuals that denote components and slices of
   --  packed arrays are expanded into suitable temporaries.
   --
   --  For non-scalar objects that are possibly unaligned, add call by copy
   --  code (copy in for IN and IN OUT, copy out for OUT and IN OUT).
   --
   --  For OUT and IN OUT parameters, add predicate checks after the call
   --  based on the predicates of the actual type.
   --
   --  The parameter N is IN OUT because in some cases, the expansion code
   --  rewrites the call as an expression actions with the call inside. In
   --  this case N is reset to point to the inside call so that the caller
   --  can continue processing of this call.

   procedure Expand_Ctrl_Function_Call (N : Node_Id);
   --  N is a function call which returns a controlled object. Transform the
   --  call into a temporary which retrieves the returned object from the
   --  secondary stack using 'reference.

   procedure Expand_Non_Function_Return (N : Node_Id);
   --  Expand a simple return statement found in a procedure body, entry body,
   --  accept statement, or an extended return statement. Note that all non-
   --  function returns are simple return statements.

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

   procedure Rewrite_Function_Call_For_C (N : Node_Id);
   --  When generating C code, replace a call to a function that returns an
   --  array into the generated procedure with an additional out parameter.

   procedure Set_Enclosing_Sec_Stack_Return (N : Node_Id);
   --  N is a return statement for a function that returns its result on the
   --  secondary stack. This sets the Sec_Stack_Needed_For_Return flag on the
   --  function and all blocks and loops that the return statement is jumping
   --  out of. This ensures that the secondary stack is not released; otherwise
   --  the function result would be reclaimed before returning to the caller.

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
      --  ZFP as those targets do not support pools.

      if RTE_Available (RE_Root_Storage_Pool_Ptr) then
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
                    (Typ            => Ptr_Typ,
                     For_Anonymous  => True,
                     Context_Scope  => Scope (Ptr_Typ),
                     Insertion_Node => Associated_Node_For_Itype (Ptr_Typ));
               end if;

               --  Access-to-controlled types should always have a master

               pragma Assert (Present (Finalization_Master (Ptr_Typ)));

               Actual :=
                 Make_Attribute_Reference (Loc,
                   Prefix =>
                     New_Occurrence_Of (Finalization_Master (Ptr_Typ), Loc),
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
      Master_Actual : Node_Id;
      Chain         : Node_Id := Empty)
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
         Actual := New_Occurrence_Of (Actual, Loc);
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

      if No (Chain) then
         Chain_Actual :=
           Make_Attribute_Reference (Loc,
             Prefix         => Make_Identifier (Loc, Name_uChain),
             Attribute_Name => Name_Unrestricted_Access);

      --  Allocator case; make a reference to the Chain passed in by the caller

      else
         Chain_Actual :=
           Make_Attribute_Reference (Loc,
             Prefix         => New_Occurrence_Of (Chain, Loc),
             Attribute_Name => Name_Unrestricted_Access);
      end if;

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

   -------------------------------
   -- Build_Procedure_Body_Form --
   -------------------------------

   function Build_Procedure_Body_Form
     (Func_Id   : Entity_Id;
      Func_Body : Node_Id) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (Func_Body);

      Proc_Decl : constant Node_Id   :=
                    Next (Unit_Declaration_Node (Func_Id));
      --  It is assumed that the next node following the declaration of the
      --  corresponding subprogram spec is the declaration of the procedure
      --  form.

      Proc_Id : constant Entity_Id := Defining_Entity (Proc_Decl);

      procedure Replace_Returns (Param_Id : Entity_Id; Stmts : List_Id);
      --  Replace each return statement found in the list Stmts with an
      --  assignment of the return expression to parameter Param_Id.

      ---------------------
      -- Replace_Returns --
      ---------------------

      procedure Replace_Returns (Param_Id : Entity_Id; Stmts : List_Id) is
         Stmt : Node_Id;

      begin
         Stmt := First (Stmts);
         while Present (Stmt) loop
            if Nkind (Stmt) = N_Block_Statement then
               Replace_Returns (Param_Id, Statements (Stmt));

            elsif Nkind (Stmt) = N_Case_Statement then
               declare
                  Alt : Node_Id;
               begin
                  Alt := First (Alternatives (Stmt));
                  while Present (Alt) loop
                     Replace_Returns (Param_Id, Statements (Alt));
                     Next (Alt);
                  end loop;
               end;

            elsif Nkind (Stmt) = N_Extended_Return_Statement then
               declare
                  Ret_Obj : constant Entity_Id :=
                              Defining_Entity
                                (First (Return_Object_Declarations (Stmt)));
                  Assign  : constant Node_Id :=
                              Make_Assignment_Statement (Sloc (Stmt),
                                Name       =>
                                  New_Occurrence_Of (Param_Id, Loc),
                                Expression =>
                                  New_Occurrence_Of (Ret_Obj, Sloc (Stmt)));
                  Stmts   : List_Id;

               begin
                  --  The extended return may just contain the declaration

                  if Present (Handled_Statement_Sequence (Stmt)) then
                     Stmts := Statements (Handled_Statement_Sequence (Stmt));
                  else
                     Stmts := New_List;
                  end if;

                  Set_Assignment_OK (Name (Assign));

                  Rewrite (Stmt,
                    Make_Block_Statement (Sloc (Stmt),
                      Declarations               =>
                        Return_Object_Declarations (Stmt),
                      Handled_Statement_Sequence =>
                        Make_Handled_Sequence_Of_Statements (Loc,
                          Statements => Stmts)));

                  Replace_Returns (Param_Id, Stmts);

                  Append_To (Stmts, Assign);
                  Append_To (Stmts, Make_Simple_Return_Statement (Loc));
               end;

            elsif Nkind (Stmt) = N_If_Statement then
               Replace_Returns (Param_Id, Then_Statements (Stmt));
               Replace_Returns (Param_Id, Else_Statements (Stmt));

               declare
                  Part : Node_Id;
               begin
                  Part := First (Elsif_Parts (Stmt));
                  while Present (Part) loop
                     Replace_Returns (Param_Id, Then_Statements (Part));
                     Next (Part);
                  end loop;
               end;

            elsif Nkind (Stmt) = N_Loop_Statement then
               Replace_Returns (Param_Id, Statements (Stmt));

            elsif Nkind (Stmt) = N_Simple_Return_Statement then

               --  Generate:
               --    Param := Expr;
               --    return;

               Rewrite (Stmt,
                 Make_Assignment_Statement (Sloc (Stmt),
                   Name       => New_Occurrence_Of (Param_Id, Loc),
                   Expression => Relocate_Node (Expression (Stmt))));

               Insert_After (Stmt, Make_Simple_Return_Statement (Loc));

               --  Skip the added return

               Next (Stmt);
            end if;

            Next (Stmt);
         end loop;
      end Replace_Returns;

      --  Local variables

      Stmts    : List_Id;
      New_Body : Node_Id;

   --  Start of processing for Build_Procedure_Body_Form

   begin
      --  This routine replaces the original function body:

      --    function F (...) return Array_Typ is
      --    begin
      --       ...
      --       return Something;
      --    end F;

      --    with the following:

      --    procedure P (..., Result : out Array_Typ) is
      --    begin
      --       ...
      --       Result := Something;
      --    end P;

      Stmts :=
        Statements (Handled_Statement_Sequence (Func_Body));
      Replace_Returns (Last_Entity (Proc_Id), Stmts);

      New_Body :=
        Make_Subprogram_Body (Loc,
          Specification              =>
            Copy_Subprogram_Spec (Specification (Proc_Decl)),
          Declarations               => Declarations (Func_Body),
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Stmts));

      --  If the function is a generic instance, so is the new procedure.
      --  Set flag accordingly so that the proper renaming declarations are
      --  generated.

      Set_Is_Generic_Instance (Proc_Id, Is_Generic_Instance (Func_Id));
      return New_Body;
   end Build_Procedure_Body_Form;

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
               Set_DT_Position_Value (Subp, DT_Position (Prim_Op));
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

   procedure Expand_Actuals (N : in out Node_Id; Subp : Entity_Id) is
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
      --  Returns an entity that refers to the given actual parameter, Actual
      --  (not including any type conversion). If Actual is an entity name,
      --  then this entity is returned unchanged, otherwise a renaming is
      --  created to provide an entity for the actual.

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
                   Subtype_Mark => New_Occurrence_Of (F_Typ, Loc),
                   Constraint   =>
                     Make_Index_Or_Discriminant_Constraint (Loc,
                       Constraints => New_List (
                         Make_Range (Loc,
                           Low_Bound  =>
                             Make_Attribute_Reference (Loc,
                               Prefix         => New_Occurrence_Of (Var, Loc),
                               Attribute_Name => Name_First),
                           High_Bound =>
                             Make_Attribute_Reference (Loc,
                               Prefix         => New_Occurrence_Of (Var, Loc),
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
            Rewrite (Actual, New_Occurrence_Of (Temp, Loc));
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

            Rewrite (Actual, New_Occurrence_Of (Temp, Loc));
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
                  Lhs := New_Occurrence_Of (Var, Loc);
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

            --  RM 3.2.4 (23/3): A predicate is checked on in-out and out
            --  by-reference parameters on exit from the call. If the actual
            --  is a derived type and the operation is inherited, the body
            --  of the operation will not contain a call to the predicate
            --  function, so it must be done explicitly after the call. Ditto
            --  if the actual is an entity of a predicated subtype.

            --  The rule refers to by-reference types, but a check is needed
            --  for by-copy types as well. That check is subsumed by the rule
            --  for subtype conversion on assignment, but we can generate the
            --  required check now.

            --  Note also that Subp may be either a subprogram entity for
            --  direct calls, or a type entity for indirect calls, which must
            --  be handled separately because the name does not denote an
            --  overloadable entity.

            By_Ref_Predicate_Check : declare
               Aund : constant Entity_Id := Underlying_Type (E_Actual);
               Atyp : Entity_Id;

               function Is_Public_Subp return Boolean;
               --  Check whether the subprogram being called is a visible
               --  operation of the type of the actual. Used to determine
               --  whether an invariant check must be generated on the
               --  caller side.

               ---------------------
               --  Is_Public_Subp --
               ---------------------

               function Is_Public_Subp return Boolean is
                  Pack      : constant Entity_Id := Scope (Subp);
                  Subp_Decl : Node_Id;

               begin
                  if not Is_Subprogram (Subp) then
                     return False;

                  --  The operation may be inherited, or a primitive of the
                  --  root type.

                  elsif
                    Nkind_In (Parent (Subp), N_Private_Extension_Declaration,
                                             N_Full_Type_Declaration)
                  then
                     Subp_Decl := Parent (Subp);

                  else
                     Subp_Decl := Unit_Declaration_Node (Subp);
                  end if;

                  return Ekind (Pack) = E_Package
                    and then
                      List_Containing (Subp_Decl) =
                        Visible_Declarations
                          (Specification (Unit_Declaration_Node (Pack)));
               end Is_Public_Subp;

            --  Start of processing for By_Ref_Predicate_Check

            begin
               if No (Aund) then
                  Atyp := E_Actual;
               else
                  Atyp := Aund;
               end if;

               if Has_Predicates (Atyp)
                 and then Present (Predicate_Function (Atyp))

                 --  Skip predicate checks for special cases

                 and then Predicate_Tests_On_Arguments (Subp)
               then
                  Append_To (Post_Call,
                    Make_Predicate_Check (Atyp, Actual));
               end if;

               --  We generated caller-side invariant checks in two cases:

               --  a) when calling an inherited operation, where there is an
               --  implicit view conversion of the actual to the parent type.

               --  b) When the conversion is explicit

               --  We treat these cases separately because the required
               --  conversion for a) is added later when expanding the call.

               if Has_Invariants (Etype (Actual))
                  and then
                    Nkind (Parent (Subp)) = N_Private_Extension_Declaration
               then
                  if Comes_From_Source (N) and then Is_Public_Subp then
                     Append_To (Post_Call, Make_Invariant_Call (Actual));
                  end if;

               elsif Nkind (Actual) = N_Type_Conversion
                 and then Has_Invariants (Etype (Expression (Actual)))
               then
                  if Comes_From_Source (N) and then Is_Public_Subp then
                     Append_To (Post_Call,
                       Make_Invariant_Call (Expression (Actual)));
                  end if;
               end if;
            end By_Ref_Predicate_Check;

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
                       Name => New_Occurrence_Of (RTE (RE_Self), Loc))));
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

         --  Cases where the call is not a member of a statement list.
         --  This includes the case where the call is an actual in another
         --  function call or indexing, i.e. an expression context as well.

         if not Is_List_Member (N)
           or else Nkind_In (Parent (N), N_Function_Call, N_Indexed_Component)
         then
            --  In Ada 2012 the call may be a function call in an expression
            --  (since OUT and IN OUT parameters are now allowed for such
            --  calls). The write-back of (in)-out parameters is handled
            --  by the back-end, but the constraint checks generated when
            --  subtypes of formal and actual don't match must be inserted
            --  in the form of assignments.

            if Ada_Version >= Ada_2012
              and then Nkind (N) = N_Function_Call
            then
               --  We used to just do handle this by climbing up parents to
               --  a non-statement/declaration and then simply making a call
               --  to Insert_Actions_After (P, Post_Call), but that doesn't
               --  work. If we are in the middle of an expression, e.g. the
               --  condition of an IF, this call would insert after the IF
               --  statement, which is much too late to be doing the write
               --  back. For example:

               --     if Clobber (X) then
               --        Put_Line (X'Img);
               --     else
               --        goto Junk
               --     end if;

               --  Now assume Clobber changes X, if we put the write back
               --  after the IF, the Put_Line gets the wrong value and the
               --  goto causes the write back to be skipped completely.

               --  To deal with this, we replace the call by

               --    do
               --       Tnnn : constant function-result-type := function-call;
               --       Post_Call actions
               --    in
               --       Tnnn;
               --    end;

               declare
                  Tnnn  : constant Entity_Id := Make_Temporary (Loc, 'T');
                  FRTyp : constant Entity_Id := Etype (N);
                  Name  : constant Node_Id   := Relocate_Node (N);

               begin
                  Prepend_To (Post_Call,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier => Tnnn,
                      Object_Definition   => New_Occurrence_Of (FRTyp, Loc),
                      Constant_Present    => True,
                      Expression          => Name));

                  Rewrite (N,
                    Make_Expression_With_Actions (Loc,
                      Actions    => Post_Call,
                      Expression => New_Occurrence_Of (Tnnn, Loc)));

                  --  We don't want to just blindly call Analyze_And_Resolve
                  --  because that would cause unwanted recursion on the call.
                  --  So for a moment set the call as analyzed to prevent that
                  --  recursion, and get the rest analyzed properly, then reset
                  --  the analyzed flag, so our caller can continue.

                  Set_Analyzed (Name, True);
                  Analyze_And_Resolve (N, FRTyp);
                  Set_Analyzed (Name, False);

                  --  Reset calling argument to point to function call inside
                  --  the expression with actions so the caller can continue
                  --  to process the call. In spite of the fact that it is
                  --  marked Analyzed above, it may be rewritten by Remove_
                  --  Side_Effects if validity checks are present, so go back
                  --  to original call.

                  N := Original_Node (Name);
               end;

            --  If not the special Ada 2012 case of a function call, then
            --  we must have the triggering statement of a triggering
            --  alternative or an entry call alternative, and we can add
            --  the post call stuff to the corresponding statement list.

            else
               declare
                  P : Node_Id;

               begin
                  P := Parent (N);
                  pragma Assert (Nkind_In (P, N_Triggering_Alternative,
                                              N_Entry_Call_Alternative));

                  if Is_Non_Empty_List (Statements (P)) then
                     Insert_List_Before_And_Analyze
                       (First (Statements (P)), Post_Call);
                  else
                     Set_Statements (P, Post_Call);
                  end if;

                  return;
               end;
            end if;

         --  Otherwise, normal case where N is in a statement sequence,
         --  just put the post-call stuff after the call statement.

         else
            Insert_Actions_After (N, Post_Call);
            return;
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

      function Inherited_From_Formal (S : Entity_Id) return Entity_Id;
      --  Within an instance, a type derived from an untagged formal derived
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
             Selector_Name             => New_Occurrence_Of (EF, Loc),
             Explicit_Actual_Parameter => Expr));

         Analyze_And_Resolve (Expr, Etype (EF));

         if Nkind (Call_Node) = N_Function_Call then
            Set_Is_Accessibility_Actual (Parent (Expr));
         end if;
      end Add_Extra_Actual;

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

      function Rewritten_For_C_Func_Id (Proc_Id : Entity_Id) return Entity_Id;
      --  Given the Id of the procedure with an extra out parameter internally
      --  built to handle functions that return a constrained array type return
      --  the Id of the corresponding function.

      -----------------------------
      -- Rewritten_For_C_Func_Id --
      -----------------------------

      function Rewritten_For_C_Func_Id (Proc_Id : Entity_Id) return Entity_Id
      is
         Decl      : constant Node_Id := Unit_Declaration_Node (Proc_Id);
         Func_Decl : Node_Id;
         Func_Id   : Entity_Id;

      begin
         pragma Assert (Rewritten_For_C (Proc_Id));
         pragma Assert (Nkind (Decl) = N_Subprogram_Body);

         Func_Decl := Nlists.Prev (Decl);

         while Present (Func_Decl)
           and then
             (Nkind (Func_Decl) = N_Freeze_Entity
                or else
              Nkind (Func_Decl) /= N_Subprogram_Declaration
                or else
              Nkind (Specification (Func_Decl)) /= N_Function_Specification)
         loop
            Func_Decl := Nlists.Prev (Func_Decl);
         end loop;

         pragma Assert (Present (Func_Decl));
         Func_Id := Defining_Entity (Specification (Func_Decl));
         pragma Assert (Chars (Proc_Id) = Chars (Func_Id));
         return Func_Id;
      end Rewritten_For_C_Func_Id;

      --  Local variables

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
      --  Expand the function or procedure call if the first actual has a
      --  declared dimension aspect, and the subprogram is declared in one
      --  of the dimension I/O packages.

      if Ada_Version >= Ada_2012
        and then
           Nkind_In (Call_Node, N_Procedure_Call_Statement, N_Function_Call)
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

      --  When generating C code, transform a function call that returns a
      --  constrained array type into procedure form.

      if Modify_Tree_For_C
        and then Nkind (Call_Node) = N_Function_Call
        and then Is_Entity_Name (Name (Call_Node))
        and then Rewritten_For_C (Ultimate_Alias (Entity (Name (Call_Node))))
      then
         --  For internally generated calls ensure that they reference the
         --  entity of the spec of the called function (needed since the
         --  expander may generate calls using the entity of their body).
         --  See for example Expand_Boolean_Operator().

         if not (Comes_From_Source (Call_Node))
           and then Nkind (Unit_Declaration_Node
                            (Ultimate_Alias (Entity (Name (Call_Node))))) =
                              N_Subprogram_Body
         then
            Set_Entity (Name (Call_Node),
              Rewritten_For_C_Func_Id
                (Ultimate_Alias (Entity (Name (Call_Node)))));
         end if;

         Rewrite_Function_Call_For_C (Call_Node);
         return;
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
                     --  Pass along value that was passed in if the returned
                     --  routine also has an Accessibility_Of_Result formal.

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
      --  of dispatching calls is suppressed for VM targets, because the VM
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

                  --  Mark the node as analyzed to avoid reanalyzing this
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
                                   New_Occurrence_Of
                                     (First_Tag_Component (Typ), Loc)),

                             Right_Opnd =>
                               Make_Selected_Component (Loc,
                                 Prefix        =>
                                   Unchecked_Convert_To (Typ,
                                     New_Value (Next_Actual (Param))),
                                 Selector_Name =>
                                   New_Occurrence_Of
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

                  --  If there is a change of representation, then generate a
                  --  warning, and do the change of representation.

                  elsif not Same_Representation (Formal_Typ, Parent_Typ) then
                     Error_Msg_N
                       ("??change of representation required", Actual);
                     Convert (Actual, Parent_Typ);

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

               if Present (Parameter_Associations (Call_Node)) then
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

         elsif Is_Null_Procedure (Subp) then
            Rewrite (Call_Node, Make_Null_Statement (Loc));
            return;
         end if;

         --  Handle inlining. No action needed if the subprogram is not inlined

         if not Is_Inlined (Subp) then
            null;

         --  Handle frontend inlining

         elsif not Back_End_Inlining then
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

                  --  Inline calls to _postconditions when generating C code

                  elsif Generate_C_Code
                    and then In_Same_Extended_Unit (Sloc (Bod), Loc)
                    and then Chars (Name (N)) = Name_uPostconditions
                  then
                     Must_Inline := True;
                  end if;
               end if;

               if Must_Inline then
                  Expand_Inlined_Call (Call_Node, Subp, Orig_Subp);

               else
                  --  Let the back end handle it

                  Add_Inlined_Body (Subp, Call_Node);

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

         --  Back end inlining: let the back end handle it

         elsif No (Unit_Declaration_Node (Subp))
           or else Nkind (Unit_Declaration_Node (Subp)) /=
                                                 N_Subprogram_Declaration
           or else No (Body_To_Inline (Unit_Declaration_Node (Subp)))
           or else Nkind (Body_To_Inline (Unit_Declaration_Node (Subp))) in
                                                                      N_Entity
         then
            Add_Inlined_Body (Subp, Call_Node);

            --  If the inlined call appears within an instantiation and some
            --  level of optimization is required, ensure that the enclosing
            --  instance body is available so that the back-end can actually
            --  perform the inlining.

            if In_Instance
              and then Comes_From_Source (Subp)
              and then Optimization_Level > 0
            then
               declare
                  Inst : Entity_Id;
                  Decl : Node_Id;

               begin
                  Inst := Scope (Subp);

                  --  Find enclosing instance.

                  while Present (Inst) and then Inst /= Standard_Standard loop
                     exit when Is_Generic_Instance (Inst);
                     Inst := Scope (Inst);
                  end loop;

                  if Present (Inst)
                    and then Is_Generic_Instance (Inst)
                    and then not Is_Inlined (Inst)
                  then
                     Set_Is_Inlined (Inst);
                     Decl := Unit_Declaration_Node (Inst);

                     --  Do not add a pending instantiation if the body exits
                     --  already, or if the instance is a compilation unit, or
                     --  the instance node is missing.

                     if Present (Corresponding_Body (Decl))
                       or else Nkind (Parent (Decl)) = N_Compilation_Unit
                       or else No (Next (Decl))
                     then
                        null;

                     else
                        Add_Pending_Instantiation (Next (Decl), Decl);
                     end if;
                  end if;
               end;
            end if;

         --  Front end expansion of simple functions returning unconstrained
         --  types (see Check_And_Split_Unconstrained_Function). Note that the
         --  case of a simple renaming (Body_To_Inline in N_Entity above, see
         --  also Build_Renamed_Body) cannot be expanded here because this may
         --  give rise to order-of-elaboration issues for the types of the
         --  parameters of the subprogram, if any.

         else
            Expand_Inlined_Call (Call_Node, Subp, Orig_Subp);
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
   end Expand_Call;

   -------------------------------
   -- Expand_Ctrl_Function_Call --
   -------------------------------

   procedure Expand_Ctrl_Function_Call (N : Node_Id) is
      function Is_Element_Reference (N : Node_Id) return Boolean;
      --  Determine whether node N denotes a reference to an Ada 2012 container
      --  element.

      --------------------------
      -- Is_Element_Reference --
      --------------------------

      function Is_Element_Reference (N : Node_Id) return Boolean is
         Ref : constant Node_Id := Original_Node (N);

      begin
         --  Analysis marks an element reference by setting the generalized
         --  indexing attribute of an indexed component before the component
         --  is rewritten into a function call.

         return
           Nkind (Ref) = N_Indexed_Component
             and then Present (Generalized_Indexing (Ref));
      end Is_Element_Reference;

      --  Local variables

      Is_Elem_Ref : constant Boolean := Is_Element_Reference (N);

   --  Start of processing for Expand_Ctrl_Function_Call

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

      --  When the temporary function result appears inside a case expression
      --  or an if expression, its lifetime must be extended to match that of
      --  the context. If not, the function result will be finalized too early
      --  and the evaluation of the expression could yield incorrect result. An
      --  exception to this rule are references to Ada 2012 container elements.
      --  Such references must be finalized at the end of each iteration of the
      --  related quantified expression, otherwise the container will remain
      --  busy.

      if not Is_Elem_Ref
        and then Within_Case_Or_If_Expression (N)
        and then Nkind (N) = N_Explicit_Dereference
      then
         Set_Is_Processed_Transient (Entity (Prefix (N)));
      end if;
   end Expand_Ctrl_Function_Call;

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

      function Move_Activation_Chain (Func_Id : Entity_Id) return Node_Id;
      --  Construct a call to System.Tasking.Stages.Move_Activation_Chain
      --  with parameters:
      --    From         current activation chain
      --    To           activation chain passed in by the caller
      --    New_Master   master passed in by the caller
      --
      --  Func_Id is the entity of the function where the extended return
      --  statement appears.

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

         --  Processing for build-in-place object allocation.

         if Needs_Finalization (Ret_Typ) then
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
                     New_Occurrence_Of (RTE (RE_Root_Storage_Pool), Loc),
                   Name                =>
                     Make_Explicit_Dereference (Loc,
                       Prefix =>
                         Make_Function_Call (Loc,
                           Name                   =>
                             New_Occurrence_Of (RTE (RE_Base_Pool), Loc),
                           Parameter_Associations => New_List (
                             Make_Explicit_Dereference (Loc,
                               Prefix =>
                                 New_Occurrence_Of (Fin_Mas_Id, Loc)))))));

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
                         New_Occurrence_Of (Desig_Typ, Loc))));

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
                     New_Occurrence_Of (Ptr_Typ, Loc)));

               --  Allocate the object, generate:
               --    Local_Id := <Alloc_Expr>;

               Append_To (Stmts,
                 Make_Assignment_Statement (Loc,
                   Name       => New_Occurrence_Of (Local_Id, Loc),
                   Expression => Alloc_Expr));

               --  Generate:
               --    Temp_Id := Temp_Typ (Local_Id);

               Append_To (Stmts,
                 Make_Assignment_Statement (Loc,
                   Name       => New_Occurrence_Of (Temp_Id, Loc),
                   Expression =>
                     Unchecked_Convert_To (Temp_Typ,
                       New_Occurrence_Of (Local_Id, Loc))));

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
                       Left_Opnd  => New_Occurrence_Of (Fin_Mas_Id, Loc),
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
                Name       => New_Occurrence_Of (Temp_Id, Loc),
                Expression => Alloc_Expr);
         end if;
      end Build_Heap_Allocator;

      ---------------------------
      -- Move_Activation_Chain --
      ---------------------------

      function Move_Activation_Chain (Func_Id : Entity_Id) return Node_Id is
      begin
         return
           Make_Procedure_Call_Statement (Loc,
             Name                   =>
               New_Occurrence_Of (RTE (RE_Move_Activation_Chain), Loc),

             Parameter_Associations => New_List (

               --  Source chain

               Make_Attribute_Reference (Loc,
                 Prefix         => Make_Identifier (Loc, Name_uChain),
                 Attribute_Name => Name_Unrestricted_Access),

               --  Destination chain

               New_Occurrence_Of
                 (Build_In_Place_Formal (Func_Id, BIP_Activation_Chain), Loc),

               --  New master

               New_Occurrence_Of
                 (Build_In_Place_Formal (Func_Id, BIP_Task_Master), Loc)));
      end Move_Activation_Chain;

      --  Local variables

      Func_Id      : constant Entity_Id :=
                       Return_Applies_To (Return_Statement_Entity (N));
      Is_BIP_Func  : constant Boolean   :=
                       Is_Build_In_Place_Function (Func_Id);
      Ret_Obj_Id   : constant Entity_Id :=
                       First_Entity (Return_Statement_Entity (N));
      Ret_Obj_Decl : constant Node_Id   := Parent (Ret_Obj_Id);
      Ret_Typ      : constant Entity_Id := Etype (Func_Id);

      Exp         : Node_Id;
      HSS         : Node_Id;
      Result      : Node_Id;
      Return_Stmt : Node_Id;
      Stmts       : List_Id;

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

      if Is_BIP_Func and then Needs_Finalization (Etype (Ret_Obj_Id)) then
         declare
            Flag_Decl : Node_Id;
            Flag_Id   : Entity_Id;
            Func_Bod  : Node_Id;

         begin
            --  Recover the function body

            Func_Bod := Unit_Declaration_Node (Func_Id);

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
                    New_Occurrence_Of (Standard_Boolean, Loc),
                  Expression        =>
                    New_Occurrence_Of (Standard_False, Loc));

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
        or else Is_Composite_Type (Ret_Typ)
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

         if Is_BIP_Func and then Has_Task (Ret_Typ) then

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
               Append_To (Stmts, Move_Activation_Chain (Func_Id));
            end if;
         end if;

         --  Update the state of the function right before the object is
         --  returned.

         if Is_BIP_Func and then Needs_Finalization (Etype (Ret_Obj_Id)) then
            declare
               Flag_Id : constant Entity_Id :=
                           Status_Flag_Or_Transient_Decl (Ret_Obj_Id);

            begin
               --  Generate:
               --    Fnn := True;

               Append_To (Stmts,
                 Make_Assignment_Statement (Loc,
                   Name       => New_Occurrence_Of (Flag_Id, Loc),
                   Expression => New_Occurrence_Of (Standard_True, Loc)));
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
         --  don't want to do the object allocation and transformation of
         --  the return object declaration to a renaming. This case occurs
         --  when the return object is initialized by a call to another
         --  build-in-place function, and that function is responsible for
         --  the allocation of the return object.

         if Is_BIP_Func
           and then Nkind (Ret_Obj_Decl) = N_Object_Renaming_Declaration
         then
            pragma Assert
              (Nkind (Original_Node (Ret_Obj_Decl)) = N_Object_Declaration
                and then Is_Build_In_Place_Function_Call
                           (Expression (Original_Node (Ret_Obj_Decl))));

            --  Return the build-in-place result by reference

            Set_By_Ref (Return_Stmt);

         elsif Is_BIP_Func then

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
               Ret_Obj_Expr : constant Node_Id   := Expression (Ret_Obj_Decl);
               Ret_Obj_Typ  : constant Entity_Id := Etype (Ret_Obj_Id);

               Init_Assignment  : Node_Id := Empty;
               Obj_Acc_Formal   : Entity_Id;
               Obj_Acc_Deref    : Node_Id;
               Obj_Alloc_Formal : Entity_Id;

            begin
               --  Build-in-place results must be returned by reference

               Set_By_Ref (Return_Stmt);

               --  Retrieve the implicit access parameter passed by the caller

               Obj_Acc_Formal :=
                 Build_In_Place_Formal (Func_Id, BIP_Object_Access);

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

               if Present (Ret_Obj_Expr)
                 and then not No_Initialization (Ret_Obj_Decl)
                 and then not Is_Interface (Ret_Obj_Typ)
               then
                  Init_Assignment :=
                    Make_Assignment_Statement (Loc,
                      Name       => New_Occurrence_Of (Ret_Obj_Id, Loc),
                      Expression => Relocate_Node (Ret_Obj_Expr));

                  Set_Etype (Name (Init_Assignment), Etype (Ret_Obj_Id));
                  Set_Assignment_OK (Name (Init_Assignment));
                  Set_No_Ctrl_Actions (Init_Assignment);

                  Set_Parent (Name (Init_Assignment), Init_Assignment);
                  Set_Parent (Expression (Init_Assignment), Init_Assignment);

                  Set_Expression (Ret_Obj_Decl, Empty);

                  if Is_Class_Wide_Type (Etype (Ret_Obj_Id))
                    and then not Is_Class_Wide_Type
                                   (Etype (Expression (Init_Assignment)))
                  then
                     Rewrite (Expression (Init_Assignment),
                       Make_Type_Conversion (Loc,
                         Subtype_Mark =>
                           New_Occurrence_Of (Etype (Ret_Obj_Id), Loc),
                         Expression   =>
                           Relocate_Node (Expression (Init_Assignment))));
                  end if;

                  --  In the case of functions where the calling context can
                  --  determine the form of allocation needed, initialization
                  --  is done with each part of the if statement that handles
                  --  the different forms of allocation (this is true for
                  --  unconstrained and tagged result subtypes).

                  if Is_Constrained (Ret_Typ)
                    and then not Is_Tagged_Type (Underlying_Type (Ret_Typ))
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

               if not Is_Constrained (Ret_Typ)
                 or else Is_Tagged_Type (Underlying_Type (Ret_Typ))
               then
                  Obj_Alloc_Formal :=
                    Build_In_Place_Formal (Func_Id, BIP_Alloc_Form);

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
                               New_Occurrence_Of (Ret_Obj_Typ, Loc)));

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
                           New_Occurrence_Of (Ref_Type, Loc));

                     Insert_Before (Ret_Obj_Decl, Alloc_Obj_Decl);

                     --  Create allocators for both the secondary stack and
                     --  global heap. If there's an initialization expression,
                     --  then create these as initialized allocators.

                     if Present (Ret_Obj_Expr)
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
                                  New_Occurrence_Of
                                    (Etype (Ret_Obj_Expr), Loc),
                                Expression   => New_Copy_Tree (Ret_Obj_Expr)));

                     else
                        --  If the function returns a class-wide type we cannot
                        --  use the return type for the allocator. Instead we
                        --  use the type of the expression, which must be an
                        --  aggregate of a definite type.

                        if Is_Class_Wide_Type (Ret_Obj_Typ) then
                           Heap_Allocator :=
                             Make_Allocator (Loc,
                               Expression =>
                                 New_Occurrence_Of
                                   (Etype (Ret_Obj_Expr), Loc));
                        else
                           Heap_Allocator :=
                             Make_Allocator (Loc,
                               Expression =>
                                 New_Occurrence_Of (Ret_Obj_Typ, Loc));
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
                     --  pool parameter on ZFP because the parameter is not
                     --  created in the first place.

                     if RTE_Available (RE_Root_Storage_Pool_Ptr) then
                        Pool_Decl :=
                          Make_Object_Renaming_Declaration (Loc,
                            Defining_Identifier => Pool_Id,
                            Subtype_Mark        =>
                              New_Occurrence_Of
                                (RTE (RE_Root_Storage_Pool), Loc),
                            Name                =>
                              Make_Explicit_Dereference (Loc,
                                New_Occurrence_Of
                                  (Build_In_Place_Formal
                                     (Func_Id, BIP_Storage_Pool), Loc)));
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

                     --  The allocator is returned on the secondary stack.

                     Set_Storage_Pool (SS_Allocator, RTE (RE_SS_Pool));
                     Set_Procedure_To_Call
                       (SS_Allocator, RTE (RE_SS_Allocate));

                     --  The allocator is returned on the secondary stack,
                     --  so indicate that the function return, as well as
                     --  all blocks that encloses the allocator, must not
                     --  release it. The flags must be set now because
                     --  the decision to use the secondary stack is done
                     --  very late in the course of expanding the return
                     --  statement, past the point where these flags are
                     --  normally set.

                     Set_Uses_Sec_Stack (Func_Id);
                     Set_Uses_Sec_Stack (Return_Statement_Entity (N));
                     Set_Sec_Stack_Needed_For_Return
                       (Return_Statement_Entity (N));
                     Set_Enclosing_Sec_Stack_Return (N);

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
                               New_Occurrence_Of (Obj_Alloc_Formal, Loc),
                             Right_Opnd =>
                               Make_Integer_Literal (Loc,
                                 UI_From_Int (BIP_Allocation_Form'Pos
                                                (Caller_Allocation)))),

                         Then_Statements => New_List (
                           Make_Assignment_Statement (Loc,
                             Name       =>
                               New_Occurrence_Of (Alloc_Obj_Id, Loc),
                             Expression =>
                               Make_Unchecked_Type_Conversion (Loc,
                                 Subtype_Mark =>
                                   New_Occurrence_Of (Ref_Type, Loc),
                                 Expression   =>
                                   New_Occurrence_Of (Obj_Acc_Formal, Loc)))),

                         Elsif_Parts => New_List (
                           Make_Elsif_Part (Loc,
                             Condition =>
                               Make_Op_Eq (Loc,
                                 Left_Opnd  =>
                                   New_Occurrence_Of (Obj_Alloc_Formal, Loc),
                                 Right_Opnd =>
                                   Make_Integer_Literal (Loc,
                                     UI_From_Int (BIP_Allocation_Form'Pos
                                                    (Secondary_Stack)))),

                             Then_Statements => New_List (
                               Make_Assignment_Statement (Loc,
                                 Name       =>
                                   New_Occurrence_Of (Alloc_Obj_Id, Loc),
                                 Expression => SS_Allocator))),

                           Make_Elsif_Part (Loc,
                             Condition =>
                               Make_Op_Eq (Loc,
                                 Left_Opnd  =>
                                   New_Occurrence_Of (Obj_Alloc_Formal, Loc),
                                 Right_Opnd =>
                                   Make_Integer_Literal (Loc,
                                     UI_From_Int (BIP_Allocation_Form'Pos
                                                    (Global_Heap)))),

                             Then_Statements => New_List (
                               Build_Heap_Allocator
                                 (Temp_Id    => Alloc_Obj_Id,
                                  Temp_Typ   => Ref_Type,
                                  Func_Id    => Func_Id,
                                  Ret_Typ    => Ret_Obj_Typ,
                                  Alloc_Expr => Heap_Allocator)))),

                         Else_Statements => New_List (
                           Pool_Decl,
                           Build_Heap_Allocator
                             (Temp_Id    => Alloc_Obj_Id,
                              Temp_Typ   => Ref_Type,
                              Func_Id    => Func_Id,
                              Ret_Typ    => Ret_Obj_Typ,
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
                            Prefix => New_Occurrence_Of (Alloc_Obj_Id, Loc)));

                        Set_Etype (Name (Init_Assignment), Etype (Ret_Obj_Id));

                        Append_To
                          (Then_Statements (Alloc_If_Stmt), Init_Assignment);
                     end if;

                     Insert_Before (Ret_Obj_Decl, Alloc_If_Stmt);

                     --  Remember the local access object for use in the
                     --  dereference of the renaming created below.

                     Obj_Acc_Formal := Alloc_Obj_Id;
                  end;
               end if;

               --  Replace the return object declaration with a renaming of a
               --  dereference of the access value designating the return
               --  object.

               Obj_Acc_Deref :=
                 Make_Explicit_Dereference (Loc,
                   Prefix => New_Occurrence_Of (Obj_Acc_Formal, Loc));

               Rewrite (Ret_Obj_Decl,
                 Make_Object_Renaming_Declaration (Loc,
                   Defining_Identifier => Ret_Obj_Id,
                   Access_Definition   => Empty,
                   Subtype_Mark        => New_Occurrence_Of (Ret_Obj_Typ, Loc),
                   Name                => Obj_Acc_Deref));

               Set_Renamed_Object (Ret_Obj_Id, Obj_Acc_Deref);
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
   end Expand_N_Function_Call;

   ---------------------------------------
   -- Expand_N_Procedure_Call_Statement --
   ---------------------------------------

   procedure Expand_N_Procedure_Call_Statement (N : Node_Id) is
      Save_Ghost_Mode : constant Ghost_Mode_Type := Ghost_Mode;

   begin
      --  The procedure call is Ghost when the name is Ghost. Set the mode now
      --  to ensure that any nodes generated during expansion are properly set
      --  as Ghost.

      Set_Ghost_Mode (N);

      Expand_Call (N);
      Ghost_Mode := Save_Ghost_Mode;
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
      Body_Id  : constant Entity_Id  := Defining_Entity (N);
      HSS      : constant Node_Id    := Handled_Statement_Sequence (N);
      Loc      : constant Source_Ptr := Sloc (N);

      procedure Add_Return (Spec_Id : Entity_Id; Stmts : List_Id);
      --  Append a return statement to the statement sequence Stmts if the last
      --  statement is not already a return or a goto statement. Note that the
      --  latter test is not critical, it does not matter if we add a few extra
      --  returns, since they get eliminated anyway later on. Spec_Id denotes
      --  the corresponding spec of the subprogram body.

      ----------------
      -- Add_Return --
      ----------------

      procedure Add_Return (Spec_Id : Entity_Id; Stmts : List_Id) is
         Last_Stmt : Node_Id;
         Loc       : Source_Ptr;
         Stmt      : Node_Id;

      begin
         --  Get last statement, ignoring any Pop_xxx_Label nodes, which are
         --  not relevant in this context since they are not executable.

         Last_Stmt := Last (Stmts);
         while Nkind (Last_Stmt) in N_Pop_xxx_Label loop
            Prev (Last_Stmt);
         end loop;

         --  Now insert return unless last statement is a transfer

         if not Is_Transfer (Last_Stmt) then

            --  The source location for the return is the end label of the
            --  procedure if present. Otherwise use the sloc of the last
            --  statement in the list. If the list comes from a generated
            --  exception handler and we are not debugging generated code,
            --  all the statements within the handler are made invisible
            --  to the debugger.

            if Nkind (Parent (Stmts)) = N_Exception_Handler
              and then not Comes_From_Source (Parent (Stmts))
            then
               Loc := Sloc (Last_Stmt);
            elsif Present (End_Label (HSS)) then
               Loc := Sloc (End_Label (HSS));
            else
               Loc := Sloc (Last_Stmt);
            end if;

            --  Append return statement, and set analyzed manually. We can't
            --  call Analyze on this return since the scope is wrong.

            --  Note: it almost works to push the scope and then do the Analyze
            --  call, but something goes wrong in some weird cases and it is
            --  not worth worrying about ???

            Stmt := Make_Simple_Return_Statement (Loc);

            --  The return statement is handled properly, and the call to the
            --  postcondition, inserted below, does not require information
            --  from the body either. However, that call is analyzed in the
            --  enclosing scope, and an elaboration check might improperly be
            --  added to it. A guard in Sem_Elab is needed to prevent that
            --  spurious check, see Check_Elab_Call.

            Append_To (Stmts, Stmt);
            Set_Analyzed (Stmt);

            --  Call the _Postconditions procedure if the related subprogram
            --  has contract assertions that need to be verified on exit.

            if Ekind (Spec_Id) = E_Procedure
              and then Present (Postconditions_Proc (Spec_Id))
            then
               Insert_Action (Stmt,
                 Make_Procedure_Call_Statement (Loc,
                   Name =>
                     New_Occurrence_Of (Postconditions_Proc (Spec_Id), Loc)));
            end if;
         end if;
      end Add_Return;

      --  Local variables

      Save_Ghost_Mode : constant Ghost_Mode_Type := Ghost_Mode;

      Except_H : Node_Id;
      L        : List_Id;
      Spec_Id  : Entity_Id;

   --  Start of processing for Expand_N_Subprogram_Body

   begin
      if Present (Corresponding_Spec (N)) then
         Spec_Id := Corresponding_Spec (N);
      else
         Spec_Id := Body_Id;
      end if;

      --  If this is a Pure function which has any parameters whose root type
      --  is System.Address, reset the Pure indication.
      --  This check is also performed when the subprogram is frozen, but we
      --  repeat it on the body so that the indication is consistent, and so
      --  it applies as well to bodies without separate specifications.

      if Is_Pure (Spec_Id)
        and then Is_Subprogram (Spec_Id)
        and then not Has_Pragma_Pure_Function (Spec_Id)
      then
         Check_Function_With_Address_Parameter (Spec_Id);

         if Spec_Id /= Body_Id then
            Set_Is_Pure (Body_Id, Is_Pure (Spec_Id));
         end if;
      end if;

      --  The subprogram body is Ghost when it is stand alone and subject to
      --  pragma Ghost or the corresponding spec is Ghost. To accomodate both
      --  cases, set the mode now to ensure that any nodes generated during
      --  expansion are marked as Ghost.

      Set_Ghost_Mode (N, Spec_Id);

      --  Set L to either the list of declarations if present, or to the list
      --  of statements if no declarations are present. This is used to insert
      --  new stuff at the start.

      if Is_Non_Empty_List (Declarations (N)) then
         L := Declarations (N);
      else
         L := Statements (HSS);
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

            if Is_Non_Empty_List (Statements (HSS)) then
               LS := Last (Statements (HSS));
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

                  A :=
                    Make_Assignment_Statement (Loc,
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

            Ghost_Mode := Save_Ghost_Mode;
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
         Add_Return (Spec_Id, Statements (HSS));

         if Present (Exception_Handlers (HSS)) then
            Except_H := First_Non_Pragma (Exception_Handlers (HSS));
            while Present (Except_H) loop
               Add_Return (Spec_Id, Statements (Except_H));
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
            Hloc : constant Source_Ptr := Sloc (HSS);
            Blok : constant Node_Id    :=
                     Make_Block_Statement (Hloc,
                       Handled_Statement_Sequence => HSS);
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

      Ghost_Mode := Save_Ghost_Mode;
   end Expand_N_Subprogram_Body;

   -----------------------------------
   -- Expand_N_Subprogram_Body_Stub --
   -----------------------------------

   procedure Expand_N_Subprogram_Body_Stub (N : Node_Id) is
      Bod : Node_Id;

   begin
      if Present (Corresponding_Body (N)) then
         Bod := Unit_Declaration_Node (Corresponding_Body (N));

         --  The body may have been expanded already when it is analyzed
         --  through the subunit node. Do no expand again: it interferes
         --  with the construction of unnesting tables when generating C.

         if not Analyzed (Bod) then
            Expand_N_Subprogram_Body (Bod);
         end if;

         --  Add full qualification to entities that may be created late
         --  during unnesting.

         Qualify_Entity_Names (N);
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
      Loc  : constant Source_Ptr := Sloc (N);
      Subp : constant Entity_Id  := Defining_Entity (N);

      --  Local variables

      Scop      : constant Entity_Id  := Scope (Subp);
      Prot_Bod  : Node_Id;
      Prot_Decl : Node_Id;
      Prot_Id   : Entity_Id;

   --  Start of processing for Expand_N_Subprogram_Declaration

   begin
      --  In SPARK, subprogram declarations are only allowed in package
      --  specifications.

      if Nkind (Parent (N)) /= N_Package_Specification then
         if Nkind (Parent (N)) = N_Compilation_Unit then
            Check_SPARK_05_Restriction
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
            Check_SPARK_05_Restriction
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

      --  When generating C code, transform a function that returns a
      --  constrained array type into a procedure with an out parameter
      --  that carries the return value.

      --  We skip this transformation for unchecked conversions, since they
      --  are not needed by the C generator (and this also produces cleaner
      --  output).

      if Modify_Tree_For_C
        and then Nkind (Specification (N)) = N_Function_Specification
        and then Is_Array_Type (Etype (Subp))
        and then Is_Constrained (Etype (Subp))
        and then not Is_Unchecked_Conversion_Instance (Subp)
      then
         Build_Procedure_Form (N);
      end if;
   end Expand_N_Subprogram_Declaration;

   --------------------------------
   -- Expand_Non_Function_Return --
   --------------------------------

   procedure Expand_Non_Function_Return (N : Node_Id) is
      pragma Assert (No (Expression (N)));

      Loc       : constant Source_Ptr := Sloc (N);
      Scope_Id  : Entity_Id := Return_Applies_To (Return_Statement_Entity (N));
      Kind      : constant Entity_Kind := Ekind (Scope_Id);
      Call      : Node_Id;
      Acc_Stat  : Node_Id;
      Goto_Stat : Node_Id;
      Lab_Node  : Node_Id;

   begin
      --  Call the _Postconditions procedure if the related subprogram has
      --  contract assertions that need to be verified on exit.

      if Ekind_In (Scope_Id, E_Entry, E_Entry_Family, E_Procedure)
        and then Present (Postconditions_Proc (Scope_Id))
      then
         Insert_Action (N,
           Make_Procedure_Call_Statement (Loc,
             Name => New_Occurrence_Of (Postconditions_Proc (Scope_Id), Loc)));
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
             Name => New_Occurrence_Of (RTE (RE_Complete_Rendezvous), Loc));
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
               New_Occurrence_Of (RTE (RE_Complete_Entry_Body), Loc),
             Parameter_Associations => New_List (
               Make_Attribute_Reference (Loc,
                 Prefix         =>
                   New_Occurrence_Of
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

      if Is_Subprogram (Proc) and then Proc /= Corr then

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
            Obj_Ptr : constant Entity_Id := Make_Temporary (Loc, 'T');

         begin
            Decls := New_List (
              Make_Full_Type_Declaration (Loc,
                Defining_Identifier => Obj_Ptr,
                  Type_Definition   =>
                     Make_Access_To_Object_Definition (Loc,
                       Subtype_Indication =>
                         New_Occurrence_Of
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

      procedure Freeze_Called_Function;
      --  If it is a function call it can appear in elaboration code and
      --  the called entity must be frozen before the call. This must be
      --  done before the call is expanded, as the expansion may rewrite it
      --  to something other than a call (e.g. a temporary initialized in a
      --  transient block).

      ----------------------------
      -- Freeze_Called_Function --
      ----------------------------

      procedure Freeze_Called_Function is
      begin
         if Ekind (Subp) = E_Function then
            Freeze_Expression (Name (N));
         end if;
      end Freeze_Called_Function;

   --  Start of processing for Expand_Protected_Subprogram_Call

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

         Freeze_Called_Function;
         Build_Protected_Subprogram_Call (N,
           Name     => New_Occurrence_Of (Subp, Sloc (N)),
           Rec      => Convert_Concurrent (Rec, Etype (Rec)),
           External => True);

      else
         Rec := Expand_Protected_Object_Reference (N, Scop);

         if No (Rec) then
            return;
         end if;

         Freeze_Called_Function;
         Build_Protected_Subprogram_Call (N,
           Name     => Name (N),
           Rec      => Rec,
           External => False);

      end if;

      --  Analyze and resolve the new call. The actuals have already been
      --  resolved, but expansion of a function call will add extra actuals
      --  if needed. Analysis of a procedure call already includes resolution.

      Analyze (N);

      if Ekind (Subp) = E_Function then
         Resolve (N, Etype (Subp));
      end if;
   end Expand_Protected_Subprogram_Call;

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

      Exp : Node_Id := Expression (N);
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
        and then not Is_Class_Wide_Type (Exptyp)
        and then Nkind (Exp) /= N_Type_Conversion
      then
         Subtype_Ind := New_Occurrence_Of (Exptyp, Loc);
      else
         Subtype_Ind := New_Occurrence_Of (R_Type, Loc);

         --  If the result type is class-wide and the expression is a view
         --  conversion, the conversion plays no role in the expansion because
         --  it does not modify the tag of the object. Remove the conversion
         --  altogether to prevent tag overwriting.

         if Is_Class_Wide_Type (R_Type)
           and then not Is_Class_Wide_Type (Exptyp)
           and then Nkind (Exp) = N_Type_Conversion
         then
            Exp := Expression (Exp);
         end if;
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

      --  If the call is within a thunk and the type is a limited view, the
      --  backend will eventually see the non-limited view of the type.

      elsif Is_Thunk (Current_Scope) and then Is_Incomplete_Type (Exptyp) then
         return;

      elsif not Requires_Transient_Scope (R_Type) then

         --  Mutable records with variable-length components are not returned
         --  on the sec-stack, so we need to make sure that the back end will
         --  only copy back the size of the actual value, and not the maximum
         --  size. We create an actual subtype for this purpose. However we
         --  need not do it if the expression is a function call since this
         --  will be done in the called function and doing it here too would
         --  cause a temporary with maximum size to be created.

         declare
            Ubt  : constant Entity_Id := Underlying_Type (Base_Type (Exptyp));
            Decl : Node_Id;
            Ent  : Entity_Id;
         begin
            if Nkind (Exp) /= N_Function_Call
              and then Has_Discriminants (Ubt)
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
         --  Prevent the reclamation of the secondary stack by all enclosing
         --  blocks and loops as well as the related function; otherwise the
         --  result would be reclaimed too early.

         Set_Enclosing_Sec_Stack_Return (N);

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
                       Subtype_Mark => New_Occurrence_Of (Etype (Exp), Loc),
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
                   Object_Definition   => New_Occurrence_Of (Acc_Typ, Loc),
                   Expression          => Alloc_Node)));

               Rewrite (Exp,
                 Make_Explicit_Dereference (Loc,
                 Prefix => New_Occurrence_Of (Temp, Loc)));

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
            Set_Procedure_To_Call (N, RTE (RE_SS_Allocate));
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
                              New_Occurrence_Of (Result_Id, Loc);
               Result_Obj : constant Node_Id   :=
                              Make_Object_Declaration (Loc,
                                Defining_Identifier => Result_Id,
                                Object_Definition   =>
                                  New_Occurrence_Of (R_Type, Loc),
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
            then
               --  If the expression is an explicit dereference then we can
               --  directly displace the pointer to reference the base of
               --  the object.

               if Nkind (Exp) = N_Explicit_Dereference then
                  Tag_Node :=
                    Make_Explicit_Dereference (Loc,
                      Prefix =>
                        Unchecked_Convert_To (RTE (RE_Tag_Ptr),
                          Make_Function_Call (Loc,
                            Name                   =>
                              New_Occurrence_Of (RTE (RE_Base_Address), Loc),
                            Parameter_Associations => New_List (
                              Unchecked_Convert_To (RTE (RE_Address),
                                Duplicate_Subexpr (Prefix (Exp)))))));

               --  Similar case to the previous one but the expression is a
               --  renaming of an explicit dereference.

               elsif Nkind (Exp) = N_Identifier
                 and then Present (Renamed_Object (Entity (Exp)))
                 and then Nkind (Renamed_Object (Entity (Exp)))
                            = N_Explicit_Dereference
               then
                  Tag_Node :=
                    Make_Explicit_Dereference (Loc,
                      Prefix =>
                        Unchecked_Convert_To (RTE (RE_Tag_Ptr),
                          Make_Function_Call (Loc,
                            Name                   =>
                              New_Occurrence_Of (RTE (RE_Base_Address), Loc),
                            Parameter_Associations => New_List (
                              Unchecked_Convert_To (RTE (RE_Address),
                                Duplicate_Subexpr
                                  (Prefix
                                    (Renamed_Object (Entity (Exp)))))))));

               --  Common case: obtain the address of the actual object and
               --  displace the pointer to reference the base of the object.

               else
                  Tag_Node :=
                    Make_Explicit_Dereference (Loc,
                      Prefix =>
                        Unchecked_Convert_To (RTE (RE_Tag_Ptr),
                          Make_Function_Call (Loc,
                            Name               =>
                              New_Occurrence_Of (RTE (RE_Base_Address), Loc),
                            Parameter_Associations => New_List (
                              Make_Attribute_Reference (Loc,
                                Prefix         => Duplicate_Subexpr (Exp),
                                Attribute_Name => Name_Address)))));
               end if;
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

      --  Call the _Postconditions procedure if the related function has
      --  contract assertions that need to be verified on exit.

      if Ekind (Scope_Id) = E_Function
        and then Present (Postconditions_Proc (Scope_Id))
      then
         --  In the case of discriminated objects, we have created a
         --  constrained subtype above, and used the underlying type. This
         --  transformation is post-analysis and harmless, except that now the
         --  call to the post-condition will be analyzed and the type kinds
         --  have to match.

         if Nkind (Exp) = N_Unchecked_Type_Conversion
           and then Is_Private_Type (R_Type) /= Is_Private_Type (Etype (Exp))
         then
            Rewrite (Exp, Expression (Relocate_Node (Exp)));
         end if;

         --  We are going to reference the returned value twice in this case,
         --  once in the call to _Postconditions, and once in the actual return
         --  statement, but we can't have side effects happening twice.

         Remove_Side_Effects (Exp);

         --  Generate call to _Postconditions

         Insert_Action (Exp,
           Make_Procedure_Call_Statement (Loc,
             Name                   =>
               New_Occurrence_Of (Postconditions_Proc (Scope_Id), Loc),
             Parameter_Associations => New_List (New_Copy_Tree (Exp))));
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
         --  Note: If the function has a foreign convention, it cannot build
         --  its result in place, so you're on your own. On the other hand,
         --  if only the return type has a foreign convention, its layout is
         --  intended to be compatible with the other language, but the build-
         --  in place machinery can ensure that the object is not copied.

         if Has_Foreign_Convention (E) then
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

      --  Step past qualification, type conversion (which can occur in actual
      --  parameter contexts), and unchecked conversion (which can occur in
      --  cases of calls to 'Input).

      if Nkind_In (Exp_Node, N_Qualified_Expression,
                             N_Type_Conversion,
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
                     New_Occurrence_Of (Node (Next_Elmt (Iface_DT_Ptr)), Loc),
                   Position     => DT_Position (Prim),
                   Address_Node =>
                     Unchecked_Convert_To (RTE (RE_Prim_Ptr),
                       Make_Attribute_Reference (Loc,
                         Prefix         => New_Occurrence_Of (Thunk_Id, Loc),
                         Attribute_Name => Name_Unrestricted_Access))),

                 Build_Set_Predefined_Prim_Op_Address (Loc,
                   Tag_Node     =>
                     New_Occurrence_Of
                      (Node (Next_Elmt (Next_Elmt (Next_Elmt (Iface_DT_Ptr)))),
                       Loc),
                   Position     => DT_Position (Prim),
                   Address_Node =>
                     Unchecked_Convert_To (RTE (RE_Prim_Ptr),
                       Make_Attribute_Reference (Loc,
                         Prefix         => New_Occurrence_Of (Prim, Loc),
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
      --  not Tagged_Type_Expansion because the dispatching mechanism is
      --  handled internally by the target.

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
         Analyze_Entry_Or_Subprogram_Contract (Subp);
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
      Ref_Func_Call     : Node_Id;
      Function_Id       : Entity_Id;
      Result_Subt       : Entity_Id;
      New_Allocator     : Node_Id;
      Return_Obj_Access : Entity_Id; -- temp for function result
      Temp_Init         : Node_Id; -- initial value of Return_Obj_Access
      Alloc_Form        : BIP_Allocation_Form;
      Pool              : Node_Id; -- nonnull if Alloc_Form = User_Storage_Pool
      Return_Obj_Actual : Node_Id; -- the temp.all, in caller-allocates case
      Chain             : Entity_Id; -- activation chain, in case of tasks

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

      --  Create a temp for the function result. In the caller-allocates case,
      --  this will be initialized to the result of a new uninitialized
      --  allocator. Note: we do not use Allocator as the Related_Node of
      --  Return_Obj_Access in call to Make_Temporary below as this would
      --  create a sort of infinite "recursion".

      Return_Obj_Access := Make_Temporary (Loc, 'R');
      Set_Etype (Return_Obj_Access, Acc_Type);

      --  When the result subtype is constrained, the return object is
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
             Expression => New_Occurrence_Of (Result_Subt, Loc));
         Set_No_Initialization (New_Allocator);

         --  Copy attributes to new allocator. Note that the new allocator
         --  logically comes from source if the original one did, so copy the
         --  relevant flag. This ensures proper treatment of the restriction
         --  No_Implicit_Heap_Allocations in this case.

         Set_Storage_Pool      (New_Allocator, Storage_Pool      (Allocator));
         Set_Procedure_To_Call (New_Allocator, Procedure_To_Call (Allocator));
         Set_Comes_From_Source (New_Allocator, Comes_From_Source (Allocator));

         Rewrite (Allocator, New_Allocator);

         --  Initial value of the temp is the result of the uninitialized
         --  allocator

         Temp_Init := Relocate_Node (Allocator);

         --  Indicate that caller allocates, and pass in the return object

         Alloc_Form := Caller_Allocation;
         Pool := Make_Null (No_Location);
         Return_Obj_Actual :=
           Make_Unchecked_Type_Conversion (Loc,
             Subtype_Mark => New_Occurrence_Of (Result_Subt, Loc),
             Expression   =>
               Make_Explicit_Dereference (Loc,
                 Prefix => New_Occurrence_Of (Return_Obj_Access, Loc)));

      --  When the result subtype is unconstrained, the function itself must
      --  perform the allocation of the return object, so we pass parameters
      --  indicating that.

      else
         Temp_Init := Empty;

         --  Case of a user-defined storage pool. Pass an allocation parameter
         --  indicating that the function should allocate its result in the
         --  pool, and pass the pool. Use 'Unrestricted_Access because the
         --  pool may not be aliased.

         if Present (Associated_Storage_Pool (Acc_Type)) then
            Alloc_Form := User_Storage_Pool;
            Pool :=
              Make_Attribute_Reference (Loc,
                Prefix         =>
                  New_Occurrence_Of
                    (Associated_Storage_Pool (Acc_Type), Loc),
                Attribute_Name => Name_Unrestricted_Access);

         --  No user-defined pool; pass an allocation parameter indicating that
         --  the function should allocate its result on the heap.

         else
            Alloc_Form := Global_Heap;
            Pool := Make_Null (No_Location);
         end if;

         --  The caller does not provide the return object in this case, so we
         --  have to pass null for the object access actual.

         Return_Obj_Actual := Empty;
      end if;

      --  Declare the temp object

      Insert_Action (Allocator,
        Make_Object_Declaration (Loc,
          Defining_Identifier => Return_Obj_Access,
          Object_Definition   => New_Occurrence_Of (Acc_Type, Loc),
          Expression          => Temp_Init));

      Ref_Func_Call := Make_Reference (Loc, Func_Call);

      --  Ada 2005 (AI-251): If the type of the allocator is an interface
      --  then generate an implicit conversion to force displacement of the
      --  "this" pointer.

      if Is_Interface (Designated_Type (Acc_Type)) then
         Rewrite
           (Ref_Func_Call,
            OK_Convert_To (Acc_Type, Ref_Func_Call));
      end if;

      declare
         Assign : constant Node_Id :=
           Make_Assignment_Statement (Loc,
             Name       => New_Occurrence_Of (Return_Obj_Access, Loc),
             Expression => Ref_Func_Call);
         --  Assign the result of the function call into the temp. In the
         --  caller-allocates case, this is overwriting the temp with its
         --  initial value, which has no effect. In the callee-allocates case,
         --  this is setting the temp to point to the object allocated by the
         --  callee.

         Actions : List_Id;
         --  Actions to be inserted. If there are no tasks, this is just the
         --  assignment statement. If the allocated object has tasks, we need
         --  to wrap the assignment in a block that activates them. The
         --  activation chain of that block must be passed to the function,
         --  rather than some outer chain.
      begin
         if Has_Task (Result_Subt) then
            Actions := New_List;
            Build_Task_Allocate_Block_With_Init_Stmts
              (Actions, Allocator, Init_Stmts => New_List (Assign));
            Chain := Activation_Chain_Entity (Last (Actions));
         else
            Actions := New_List (Assign);
            Chain   := Empty;
         end if;

         Insert_Actions (Allocator, Actions);
      end;

      --  When the function has a controlling result, an allocation-form
      --  parameter must be passed indicating that the caller is allocating
      --  the result object. This is needed because such a function can be
      --  called as a dispatching operation and must be treated similarly
      --  to functions with unconstrained result subtypes.

      Add_Unconstrained_Actuals_To_Build_In_Place_Call
        (Func_Call, Function_Id, Alloc_Form, Pool_Actual => Pool);

      Add_Finalization_Master_Actual_To_Build_In_Place_Call
        (Func_Call, Function_Id, Acc_Type);

      Add_Task_Actuals_To_Build_In_Place_Call
        (Func_Call, Function_Id, Master_Actual => Master_Id (Acc_Type),
         Chain => Chain);

      --  Add an implicit actual to the function call that provides access
      --  to the allocated object. An unchecked conversion to the (specific)
      --  result subtype of the function is inserted to handle cases where
      --  the access type of the allocator has a class-wide designated type.

      Add_Access_Actual_To_Build_In_Place_Call
        (Func_Call, Function_Id, Return_Obj_Actual);

      --  Finally, replace the allocator node with a reference to the temp

      Rewrite (Allocator, New_Occurrence_Of (Return_Obj_Access, Loc));

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
      --  Step past qualification, type conversion (which can occur in actual
      --  parameter contexts), and unchecked conversion (which can occur in
      --  cases of calls to 'Input).

      if Nkind_In (Func_Call, N_Qualified_Expression,
                              N_Type_Conversion,
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
                  New_Occurrence_Of (Result_Subt, Loc),
                Expression =>
                  New_Copy_Tree (Function_Call));

            Insert_Action (Function_Call, Temp_Decl);

            Rewrite (Function_Call, New_Occurrence_Of (Temp_Id, Loc));
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
             Object_Definition   => New_Occurrence_Of (Result_Subt, Loc));

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
           (Func_Call, Function_Id, New_Occurrence_Of (Return_Obj_Id, Loc));

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
           Subtype_Mark => New_Occurrence_Of (Result_Subt, Loc),
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
                New_Occurrence_Of (Result_Subt, Loc)));
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
          Object_Definition   => New_Occurrence_Of (Ptr_Typ, Loc),
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
     (Obj_Decl      : Node_Id;
      Function_Call : Node_Id)
   is
      Obj_Def_Id : constant Entity_Id  := Defining_Identifier (Obj_Decl);
      Encl_Func  : constant Entity_Id  := Enclosing_Subprogram (Obj_Def_Id);
      Loc        : constant Source_Ptr := Sloc (Function_Call);
      Obj_Loc    : constant Source_Ptr := Sloc (Obj_Decl);

      Call_Deref      : Node_Id;
      Caller_Object   : Node_Id;
      Def_Id          : Entity_Id;
      Fmaster_Actual  : Node_Id := Empty;
      Func_Call       : Node_Id := Function_Call;
      Function_Id     : Entity_Id;
      Pool_Actual     : Node_Id;
      Ptr_Typ         : Entity_Id;
      Ptr_Typ_Decl    : Node_Id;
      Pass_Caller_Acc : Boolean := False;
      Res_Decl        : Node_Id;
      Result_Subt     : Entity_Id;

      Definite : Boolean;
      --  True for definite function result subtype

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

      if Is_Entity_Name (Name (Func_Call)) then
         Function_Id := Entity (Name (Func_Call));

      elsif Nkind (Name (Func_Call)) = N_Explicit_Dereference then
         Function_Id := Etype (Name (Func_Call));

      else
         raise Program_Error;
      end if;

      Result_Subt := Etype (Function_Id);
      Definite    := Is_Definite_Subtype (Underlying_Type (Result_Subt));

      --  Create an access type designating the function's result subtype. We
      --  use the type of the original call because it may be a call to an
      --  inherited operation, which the expansion has replaced with the parent
      --  operation that yields the parent type. Note that this access type
      --  must be declared before we establish a transient scope, so that it
      --  receives the proper accessibility level.

      Ptr_Typ := Make_Temporary (Loc, 'A');
      Ptr_Typ_Decl :=
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => Ptr_Typ,
          Type_Definition     =>
            Make_Access_To_Object_Definition (Loc,
              All_Present        => True,
              Subtype_Indication =>
                New_Occurrence_Of (Etype (Function_Call), Loc)));

      --  The access type and its accompanying object must be inserted after
      --  the object declaration in the constrained case, so that the function
      --  call can be passed access to the object. In the indefinite case,
      --  or if the object declaration is for a return object, the access type
      --  and object must be inserted before the object, since the object
      --  declaration is rewritten to be a renaming of a dereference of the
      --  access object. Note: we need to freeze Ptr_Typ explicitly, because
      --  the result object is in a different (transient) scope, so won't
      --  cause freezing.

      if Definite
        and then not Is_Return_Object (Defining_Identifier (Obj_Decl))
      then
         Insert_After_And_Analyze (Obj_Decl, Ptr_Typ_Decl);
      else
         Insert_Action (Obj_Decl, Ptr_Typ_Decl);
      end if;

      --  Force immediate freezing of Ptr_Typ because Res_Decl will be
      --  elaborated in an inner (transient) scope and thus won't cause
      --  freezing by itself.

      declare
         Ptr_Typ_Freeze_Ref : constant Node_Id :=
                                New_Occurrence_Of (Ptr_Typ, Loc);
      begin
         Set_Parent (Ptr_Typ_Freeze_Ref, Ptr_Typ_Decl);
         Freeze_Expression (Ptr_Typ_Freeze_Ref);
      end;

      --  If the object is a return object of an enclosing build-in-place
      --  function, then the implicit build-in-place parameters of the
      --  enclosing function are simply passed along to the called function.
      --  (Unfortunately, this won't cover the case of extension aggregates
      --  where the ancestor part is a build-in-place indefinite function
      --  call that should be passed along the caller's parameters. Currently
      --  those get mishandled by reassigning the result of the call to the
      --  aggregate return object, when the call result should really be
      --  directly built in place in the aggregate and not in a temporary. ???)

      if Is_Return_Object (Defining_Identifier (Obj_Decl)) then
         Pass_Caller_Acc := True;

         --  When the enclosing function has a BIP_Alloc_Form formal then we
         --  pass it along to the callee (such as when the enclosing function
         --  has an unconstrained or tagged result type).

         if Needs_BIP_Alloc_Form (Encl_Func) then
            if RTE_Available (RE_Root_Storage_Pool_Ptr) then
               Pool_Actual :=
                 New_Occurrence_Of
                   (Build_In_Place_Formal (Encl_Func, BIP_Storage_Pool), Loc);

            --  The build-in-place pool formal is not built on e.g. ZFP

            else
               Pool_Actual := Empty;
            end if;

            Add_Unconstrained_Actuals_To_Build_In_Place_Call
              (Function_Call  => Func_Call,
               Function_Id    => Function_Id,
               Alloc_Form_Exp =>
                 New_Occurrence_Of
                   (Build_In_Place_Formal (Encl_Func, BIP_Alloc_Form), Loc),
               Pool_Actual    => Pool_Actual);

         --  Otherwise, if enclosing function has a definite result subtype,
         --  then caller allocation will be used.

         else
            Add_Unconstrained_Actuals_To_Build_In_Place_Call
              (Func_Call, Function_Id, Alloc_Form => Caller_Allocation);
         end if;

         if Needs_BIP_Finalization_Master (Encl_Func) then
            Fmaster_Actual :=
              New_Occurrence_Of
                (Build_In_Place_Formal
                   (Encl_Func, BIP_Finalization_Master), Loc);
         end if;

         --  Retrieve the BIPacc formal from the enclosing function and convert
         --  it to the access type of the callee's BIP_Object_Access formal.

         Caller_Object :=
           Make_Unchecked_Type_Conversion (Loc,
             Subtype_Mark =>
               New_Occurrence_Of
                 (Etype
                    (Build_In_Place_Formal (Function_Id, BIP_Object_Access)),
                  Loc),
             Expression   =>
               New_Occurrence_Of
                 (Build_In_Place_Formal (Encl_Func, BIP_Object_Access),
                  Loc));

      --  In the definite case, add an implicit actual to the function call
      --  that provides access to the declared object. An unchecked conversion
      --  to the (specific) result type of the function is inserted to handle
      --  the case where the object is declared with a class-wide type.

      elsif Definite then
         Caller_Object :=
            Make_Unchecked_Type_Conversion (Loc,
              Subtype_Mark => New_Occurrence_Of (Result_Subt, Loc),
              Expression   => New_Occurrence_Of (Obj_Def_Id, Loc));

         --  When the function has a controlling result, an allocation-form
         --  parameter must be passed indicating that the caller is allocating
         --  the result object. This is needed because such a function can be
         --  called as a dispatching operation and must be treated similarly
         --  to functions with indefinite result subtypes.

         Add_Unconstrained_Actuals_To_Build_In_Place_Call
           (Func_Call, Function_Id, Alloc_Form => Caller_Allocation);

      --  The allocation for indefinite library-level objects occurs on the
      --  heap as opposed to the secondary stack. This accommodates DLLs where
      --  the secondary stack is destroyed after each library unload. This is
      --  a hybrid mechanism where a stack-allocated object lives on the heap.

      elsif Is_Library_Level_Entity (Defining_Identifier (Obj_Decl))
        and then not Restriction_Active (No_Implicit_Heap_Allocations)
      then
         Add_Unconstrained_Actuals_To_Build_In_Place_Call
           (Func_Call, Function_Id, Alloc_Form => Global_Heap);
         Caller_Object := Empty;

         --  Create a finalization master for the access result type to ensure
         --  that the heap allocation can properly chain the object and later
         --  finalize it when the library unit goes out of scope.

         if Needs_Finalization (Etype (Func_Call)) then
            Build_Finalization_Master
              (Typ            => Ptr_Typ,
               For_Lib_Level  => True,
               Insertion_Node => Ptr_Typ_Decl);

            Fmaster_Actual :=
              Make_Attribute_Reference (Loc,
                Prefix         =>
                  New_Occurrence_Of (Finalization_Master (Ptr_Typ), Loc),
                Attribute_Name => Name_Unrestricted_Access);
         end if;

      --  In other indefinite cases, pass an indication to do the allocation
      --  on the secondary stack and set Caller_Object to Empty so that a null
      --  value will be passed for the caller's object address. A transient
      --  scope is established to ensure eventual cleanup of the result.

      else
         Add_Unconstrained_Actuals_To_Build_In_Place_Call
           (Func_Call, Function_Id, Alloc_Form => Secondary_Stack);
         Caller_Object := Empty;

         Establish_Transient_Scope (Obj_Decl, Sec_Stack => True);
      end if;

      --  Pass along any finalization master actual, which is needed in the
      --  case where the called function initializes a return object of an
      --  enclosing build-in-place function.

      Add_Finalization_Master_Actual_To_Build_In_Place_Call
        (Func_Call  => Func_Call,
         Func_Id    => Function_Id,
         Master_Exp => Fmaster_Actual);

      if Nkind (Parent (Obj_Decl)) = N_Extended_Return_Statement
        and then Has_Task (Result_Subt)
      then
         --  Here we're passing along the master that was passed in to this
         --  function.

         Add_Task_Actuals_To_Build_In_Place_Call
           (Func_Call, Function_Id,
            Master_Actual =>
              New_Occurrence_Of
                (Build_In_Place_Formal (Encl_Func, BIP_Task_Master), Loc));

      else
         Add_Task_Actuals_To_Build_In_Place_Call
           (Func_Call, Function_Id, Make_Identifier (Loc, Name_uMaster));
      end if;

      Add_Access_Actual_To_Build_In_Place_Call
        (Func_Call, Function_Id, Caller_Object, Is_Access => Pass_Caller_Acc);

      --  Finally, create an access object initialized to a reference to the
      --  function call. We know this access value cannot be null, so mark the
      --  entity accordingly to suppress the access check.

      Def_Id := Make_Temporary (Loc, 'R', Func_Call);
      Set_Etype (Def_Id, Ptr_Typ);
      Set_Is_Known_Non_Null (Def_Id);

      Res_Decl :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => Def_Id,
          Constant_Present    => True,
          Object_Definition   => New_Occurrence_Of (Ptr_Typ, Loc),
          Expression          =>
            Make_Reference (Loc, Relocate_Node (Func_Call)));

      Insert_After_And_Analyze (Ptr_Typ_Decl, Res_Decl);

      --  If the result subtype of the called function is definite and is not
      --  itself the return expression of an enclosing BIP function, then mark
      --  the object as having no initialization.

      if Definite
        and then not Is_Return_Object (Defining_Identifier (Obj_Decl))
      then
         --  The related object declaration is encased in a transient block
         --  because the build-in-place function call contains at least one
         --  nested function call that produces a controlled transient
         --  temporary:

         --    Obj : ... := BIP_Func_Call (Ctrl_Func_Call);

         --  Since the build-in-place expansion decouples the call from the
         --  object declaration, the finalization machinery lacks the context
         --  which prompted the generation of the transient block. To resolve
         --  this scenario, store the build-in-place call.

         if Scope_Is_Transient and then Node_To_Be_Wrapped = Obj_Decl then
            Set_BIP_Initialization_Call (Obj_Def_Id, Res_Decl);
         end if;

         Set_Expression (Obj_Decl, Empty);
         Set_No_Initialization (Obj_Decl);

      --  In case of an indefinite result subtype, or if the call is the
      --  return expression of an enclosing BIP function, rewrite the object
      --  declaration as an object renaming where the renamed object is a
      --  dereference of <function_Call>'reference:
      --
      --      Obj : Subt renames <function_call>'Ref.all;

      else
         Call_Deref :=
           Make_Explicit_Dereference (Obj_Loc,
             Prefix => New_Occurrence_Of (Def_Id, Obj_Loc));

         Rewrite (Obj_Decl,
           Make_Object_Renaming_Declaration (Obj_Loc,
             Defining_Identifier => Make_Temporary (Obj_Loc, 'D'),
             Subtype_Mark        => New_Occurrence_Of (Result_Subt, Obj_Loc),
             Name                => Call_Deref));

         Set_Renamed_Object (Defining_Identifier (Obj_Decl), Call_Deref);

         --  If the original entity comes from source, then mark the new
         --  entity as needing debug information, even though it's defined
         --  by a generated renaming that does not come from source, so that
         --  the Materialize_Entity flag will be set on the entity when
         --  Debug_Renaming_Declaration is called during analysis.

         if Comes_From_Source (Obj_Def_Id) then
            Set_Debug_Info_Needed (Defining_Identifier (Obj_Decl));
         end if;

         Analyze (Obj_Decl);

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
            Ren_Id  : constant Entity_Id := Defining_Entity (Obj_Decl);
            Next_Id : constant Entity_Id := Next_Entity (Ren_Id);

         begin
            Set_Chars (Ren_Id, Chars (Obj_Def_Id));

            --  Swap next entity links in preparation for exchanging entities

            Set_Next_Entity (Ren_Id, Next_Entity (Obj_Def_Id));
            Set_Next_Entity (Obj_Def_Id, Next_Id);
            Set_Homonym     (Ren_Id, Homonym (Obj_Def_Id));

            Exchange_Entities (Ren_Id, Obj_Def_Id);

            --  Preserve source indication of original declaration, so that
            --  xref information is properly generated for the right entity.

            Preserve_Comes_From_Source (Obj_Decl, Original_Node (Obj_Decl));
            Preserve_Comes_From_Source (Obj_Def_Id, Original_Node (Obj_Decl));

            Set_Comes_From_Source (Ren_Id, False);
         end;
      end if;

      --  If the object entity has a class-wide Etype, then we need to change
      --  it to the result subtype of the function call, because otherwise the
      --  object will be class-wide without an explicit initialization and
      --  won't be allocated properly by the back end. It seems unclean to make
      --  such a revision to the type at this point, and we should try to
      --  improve this treatment when build-in-place functions with class-wide
      --  results are implemented. ???

      if Is_Class_Wide_Type (Etype (Defining_Identifier (Obj_Decl))) then
         Set_Etype (Defining_Identifier (Obj_Decl), Result_Subt);
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
          Expression => New_Occurrence_Of (Result_Subt, Loc));
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
          Object_Definition   => New_Occurrence_Of (Acc_Type, Loc),
          Expression          => Relocate_Node (Allocator));
      Insert_Action (Allocator, Tmp_Obj);

      Insert_List_After_And_Analyze (Tmp_Obj,
        Build_Initialization_Call (Loc,
          Id_Ref =>
            Make_Explicit_Dereference (Loc,
              Prefix => New_Occurrence_Of (Return_Obj_Access, Loc)),
          Typ => Etype (Function_Id),
          Constructor_Ref => Function_Call));

      --  Finally, replace the allocator node with a reference to the result of
      --  the function call itself (which will effectively be an access to the
      --  object created by the allocator).

      Rewrite (Allocator, New_Occurrence_Of (Return_Obj_Access, Loc));

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

   ---------------------------------
   -- Rewrite_Function_Call_For_C --
   ---------------------------------

   procedure Rewrite_Function_Call_For_C (N : Node_Id) is
      function Rewritten_For_C_Proc_Id (Func_Id : Entity_Id) return Entity_Id;
      --  Given the Id of the function that returns a constrained array type
      --  return the Id of its internally built procedure with an extra out
      --  parameter.

      -----------------------------
      -- Rewritten_For_C_Proc_Id --
      -----------------------------

      function Rewritten_For_C_Proc_Id (Func_Id : Entity_Id) return Entity_Id
      is
         Func_Decl : constant Node_Id := Unit_Declaration_Node (Func_Id);
         Proc_Decl : Node_Id;
         Proc_Id   : Entity_Id;

      begin
         Proc_Decl := Next (Func_Decl);

         while Present (Proc_Decl)
           and then
             (Nkind (Proc_Decl) = N_Freeze_Entity
                or else
              Nkind (Proc_Decl) /= N_Subprogram_Declaration)
         loop
            Proc_Decl := Next (Proc_Decl);
         end loop;

         pragma Assert (Present (Proc_Decl));
         Proc_Id := Defining_Entity (Proc_Decl);
         pragma Assert (Chars (Proc_Id) = Chars (Func_Id));
         return Proc_Id;
      end Rewritten_For_C_Proc_Id;

      --  Local variables

      Orig_Func   : constant Entity_Id  := Entity (Name (N));
      Func_Id     : constant Entity_Id  := Ultimate_Alias (Orig_Func);
      Par         : constant Node_Id    := Parent (N);
      Proc_Id     : constant Entity_Id  := Rewritten_For_C_Proc_Id (Func_Id);
      Loc         : constant Source_Ptr := Sloc (Par);
      Actuals     : List_Id;
      Last_Actual : Node_Id;
      Last_Formal : Entity_Id;

   --  Start of processing for Rewrite_Function_Call_For_C

   begin
      --  The actuals may be given by named associations, so the added actual
      --  that is the target of the return value of the call must be a named
      --  association as well, so we retrieve the name of the generated
      --  out_formal.

      Last_Formal := First_Formal (Proc_Id);
      while Present (Next_Formal (Last_Formal)) loop
         Last_Formal := Next_Formal (Last_Formal);
      end loop;

      Actuals := Parameter_Associations (N);

      --  The original function may lack parameters

      if No (Actuals) then
         Actuals := New_List;
      end if;

      --  If the function call is the expression of an assignment statement,
      --  transform the assignment into a procedure call. Generate:

      --    LHS := Func_Call (...);

      --    Proc_Call (..., LHS);

      --  If function is inherited, a conversion may be necessary.

      if Nkind (Par) = N_Assignment_Statement then
         Last_Actual :=  Name (Par);

         if not Comes_From_Source (Orig_Func)
           and then Etype (Orig_Func) /= Etype (Func_Id)
         then
            Last_Actual :=
              Make_Type_Conversion (Loc,
                New_Occurrence_Of (Etype (Func_Id), Loc),
                Last_Actual);
         end if;

         Append_To (Actuals,
           Make_Parameter_Association (Loc,
             Selector_Name             =>
               Make_Identifier (Loc, Chars (Last_Formal)),
             Explicit_Actual_Parameter => Last_Actual));

         Rewrite (Par,
           Make_Procedure_Call_Statement (Loc,
             Name                   => New_Occurrence_Of (Proc_Id, Loc),
             Parameter_Associations => Actuals));
         Analyze (Par);

      --  Otherwise the context is an expression. Generate a temporary and a
      --  procedure call to obtain the function result. Generate:

      --    ... Func_Call (...) ...

      --    Temp : ...;
      --    Proc_Call (..., Temp);
      --    ... Temp ...

      else
         declare
            Temp_Id : constant Entity_Id := Make_Temporary (Loc, 'T');
            Call    : Node_Id;
            Decl    : Node_Id;

         begin
            --  Generate:
            --    Temp : ...;

            Decl :=
              Make_Object_Declaration (Loc,
                Defining_Identifier => Temp_Id,
                Object_Definition   =>
                  New_Occurrence_Of (Etype (Func_Id), Loc));

            --  Generate:
            --    Proc_Call (..., Temp);

            Append_To (Actuals,
              Make_Parameter_Association (Loc,
                Selector_Name             =>
                  Make_Identifier (Loc, Chars (Last_Formal)),
                Explicit_Actual_Parameter =>
                  New_Occurrence_Of (Temp_Id, Loc)));

            Call :=
              Make_Procedure_Call_Statement (Loc,
                Name                   => New_Occurrence_Of (Proc_Id, Loc),
                Parameter_Associations => Actuals);

            Insert_Actions (Par, New_List (Decl, Call));
            Rewrite (N, New_Occurrence_Of (Temp_Id, Loc));
         end;
      end if;
   end Rewrite_Function_Call_For_C;

   ------------------------------------
   -- Set_Enclosing_Sec_Stack_Return --
   ------------------------------------

   procedure Set_Enclosing_Sec_Stack_Return (N : Node_Id) is
      P : Node_Id := N;

   begin
      --  Due to a possible mix of internally generated blocks, source blocks
      --  and loops, the scope stack may not be contiguous as all labels are
      --  inserted at the top level within the related function. Instead,
      --  perform a parent-based traversal and mark all appropriate constructs.

      while Present (P) loop

         --  Mark the label of a source or internally generated block or
         --  loop.

         if Nkind_In (P, N_Block_Statement, N_Loop_Statement) then
            Set_Sec_Stack_Needed_For_Return (Entity (Identifier (P)));

         --  Mark the enclosing function

         elsif Nkind (P) = N_Subprogram_Body then
            if Present (Corresponding_Spec (P)) then
               Set_Sec_Stack_Needed_For_Return (Corresponding_Spec (P));
            else
               Set_Sec_Stack_Needed_For_Return (Defining_Entity (P));
            end if;

            --  Do not go beyond the enclosing function

            exit;
         end if;

         P := Parent (P);
      end loop;
   end Set_Enclosing_Sec_Stack_Return;

end Exp_Ch6;
